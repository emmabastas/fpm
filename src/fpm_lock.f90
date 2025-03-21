!> Lock package directories before working on them.
!>
!># Synopsis
!>
!> Use the functions [[fpm_lock_acquire]] and [[fpm_lock_release]] to "lock" a
!> `fpm` package directory to prevent issues when multiple `fpm` process want
!> to work on the same package at the same time. Here's an example of how this
!> module is used in the rest of the codebase:
!>
!>```fortran
!> !> Entry point for the update subcommand
!>subroutine cmd_update(settings)
!> type(error_t), allcatable :: error
!> fpm_lock_acquire(error)
!> ! Do things here
!> fpm_lock_release(error)
!>end subroutine cmd_update
!>```
!>
!># Background
!>
!> This module exists to fix a buggy behavior that exists in many package
!> managers (however, most users never experience issues with it).
!>
!> The buggy behaviors is that when many `fpm` processes try to work on the same
!> package at the same time the different processes sort of step on one another
!> an it leads to problems, for instance two processes might try to compile the
!> same file  at the same time.
!>
!> Also see this issue:
!> [https://github.com/fortran-lang/fpm/issues/957](https://github.com/fortran-lang/fpm/issues/957)
!> for some
!> more details.
!>
!> What we need is for an `fpm` process \(A\) to see if another `fpm` process
!> \(B\) is already working on a package, and if so, wait for \(B\) to finish
!> that work before \(A\) steps in. The way we do this is with so-called
!> *lock-files*. Basically \(B\) creates a special file named
!> `.fpm-package-lock` in the package directory so that \(A\) will see that this
!> file exists and wait for it to be deleted by \(B\), when that is done it
!> means that the package directory is free, and so \(A\) now creates
!> `.fpm-package-lock` itself and does it's thing, after \(A\) is done it
!> deletes the lock-file again.
!>
!> That's pretty much the gist of it. It's complicated somewhat by the fact that
!> we need to consider certain rare cases (what if the program crashes and
!> leaves the lock-file behind for instance). Also, the lock-file operations
!> have to be what's called "atomic". For instance, consider this non-atomic way
!> of creating a lock-file: (in pseudocode)
!>```
!>1)   if file_exists('.fpm-package-lock') then
!>         wait_for_file_to_be_deleted('.fpm-package-lock')
!>2)   create_file('.fpm-package-lock')
!>3)   do_something()
!>4)   delete_file('.fpm-package-lock')
!>```
!> The problem with this code is that `.fpm-packge-lock` may be created by some
!> other process after the check on line (1), but before line (2) has executed,
!> and then it's not very clear what will happen, both processes might think
!> that they are have acquired a lock on the package directory. A better piece
!> of code could be:
!>```
!>error = create_file('.fpm-package-lock')
!>if error == ALREADY_EXISTS then
!>    create_this_file_again_after_deletion('.fpm-package-lock')
!>do_something()
!>delete_file('.fpm-package-lock')
!>```

module fpm_lock

use :: fpm_error, only : error_t, fatal_error
use :: iso_c_binding, only : c_int, c_bool

implicit none
private
public :: fpm_lock_acquire, fpm_lock_acquire_noblock, fpm_lock_release

logical :: has_lock = .false.

! These C functions are defined in `fpm_lock.c`
interface
    subroutine c_process_alive(pid, alive) bind(c, name="c_process_alive")
        import c_int, c_bool
        integer(kind=c_int),  intent(in)  :: pid
        logical(kind=c_bool), intent(out) :: alive
    end subroutine c_process_alive
end interface

contains

function process_alive(pid) result(alive)
    integer(kind=c_int), intent(in) :: pid
    logical(kind=c_bool) :: alive
    call c_process_alive(pid, alive)
end function process_alive

!> Like [[fpm_lock_acquire]] but it some other process already has a lock it
!> returns immediately instead of waiting indefinitely.
subroutine fpm_lock_acquire_noblock(error, success, pid)
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    !> `.true.` if a package lock was acquired, `.false.` otherwise.
    logical, optional, intent(out) :: success

    !> Use this PID instead of the current process PID. This is used in the
    !> test suite and probably there is no other good use for it.
    integer, optional, intent(in) :: pid

    integer :: pid_local

    ! unit for open lock-file.
    integer :: lock_unit

    ! Error status and message.
    integer :: iostat
    character(len=256) :: iomsg

    ! Did the lock-file exist already or not.
    logical :: exists

    ! If the file contains a PID we put it here.
    integer :: lock_pid

    if (present(pid)) then
        pid_local = pid
    else
        pid_local = getpid()
    endif

    if (has_lock) then
        call fatal_error (error, &
            "Tried locking package directory when it's already locked")
        if (present(success)) success = .false.
        return
    end if

    open( &
        file='.fpm-package-lock', &
        action='readwrite', &
        newunit=lock_unit, &
        iostat=iostat, &
        iomsg=iomsg)
    inquire(unit=lock_unit, exist=exists)

    ! An error occurred when opening the file. It could happen because some
    ! other process has the file open already, or something went wrong. In any
    ! case we didn't acquire a lock.
    if (iostat > 0) then
        close(unit=lock_unit)
        if (present(success)) success = .false.
        return
    end if

    ! The lock-file already exists and we managed to open it; This probably
    ! means that another fpm process has a lock already, but there are some edge
    ! cases we need to check before we can be sure.
    if (exists) then
        read(unit=lock_unit, fmt='(1I256)', iostat=iostat, iomsg=iomsg) lock_pid

        ! If iostat is zero then we managed to parse an integer in the lock-file
        ! If the parsed integer corresponds to the PID of a running process that
        ! isn't this current process then that process has the lock.
        if (iostat == 0 .and. lock_pid /= pid_local) then
            ! Fortran doesn't short-circuit boolean expressions, hence the
            ! nesting; We only want to check `process_alive` if `lock_pid` is
            ! valid!
            if (process_alive(lock_pid)) then
                close(unit=lock_unit)
                if (present(success)) success = .false.
                return
            end if
        end if

        ! At this point we conclude that although the lock-file already existed
        ! it wasn't valid, so we should go ahead an acquire the lock.

        ! BUG(@emmabastas) If the read statement fails it's probably because
        ! the lock-file didn't contain valid data, but it can fail more other
        ! reasons too with no way of distinguishing between them in a portable
        ! way. Ideally we would want to call fatal_error if the read statement
        ! failed for some unexpected reason.
    end if

    rewind(unit=lock_unit, iostat=iostat, iomsg=iomsg)
    if (iostat > 0) then
        call fatal_error(error, "Error rewinding lock-file " // iomsg)
        if (present(success)) success = .false.
        return
    end if

    write(unit=lock_unit, fmt='(1I256)', iostat=iostat, iomsg=iomsg) pid_local
    if (iostat > 0) then
        call fatal_error(error, "Error writing to lock-file " // iomsg)
        if (present(success)) success = .false.
        return
    end if

    close(unit=lock_unit, iostat=iostat, iomsg=iomsg)
    if (iostat > 0) then
        call fatal_error(error, "Error closing lock-file " // iomsg)
        if (present(success)) success = .false.
        return
    end if

    has_lock = .true.
    if (present(success)) success = .true.
end subroutine fpm_lock_acquire_noblock

!> Try to acquire a lock on the current package directory. If some other process
!> already has a lock this function blocks until it can get the lock.
!> @note
!> You cannot use this function multiple times without calling
!> [[fpm_lock_release]] first.
!> @endnote
subroutine fpm_lock_acquire(error)
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    logical :: got_lock

    call fpm_lock_acquire_noblock(error, success=got_lock)
    if (allocated(error)) return

    do while (.not. got_lock)
        call sleep(1) ! not very sophisticated but it works :-)
        call fpm_lock_acquire_noblock(error, success=got_lock)
        if (allocated(error)) return
    end do
end subroutine fpm_lock_acquire

!> Release a lock on the current package directory
!> @note
!> You can only release a lock if you acquired it with [[fpm_lock_acquire]]
!> first.
!> @endnote
subroutine fpm_lock_release(error)
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: lock_unit

    integer :: iostat
    character(len=256) :: iomsg

    if (.not. has_lock) then
        call fatal_error(error, &
            "Tried unlocking package directory when it wasn't locked")
        has_lock = .false.
        return
    end if

    open( &
        file='.fpm-package-lock', &
        newunit=lock_unit, &
        status='old', &
        iostat=iostat, &
        iomsg=iomsg)
    if (iostat /= 0) then
        call fatal_error(error, &
            "Error opening lock-file for deletion '"//iomsg//"'")
        has_lock = .false.
        return
    end if

    close(unit=lock_unit, status='delete', iostat=iostat, iomsg=iomsg)

    if (iostat /= 0) then
        call fatal_error(error, "Error deleting lock-file '"//iomsg//"'")
        has_lock = .false.
        return
    end if

    has_lock=.false.
end subroutine fpm_lock_release

end module fpm_lock
