!> This module exists to fix a buggy behavior that exists in many package
!> managers (however, most users never experience issues with it).
!>
!> The buggy behaviors is that when many `fpm` processes try to work on the same
!> package at the same time the different processes sort of step on one another an
!> it leads to problems, for instance two processes might try to compile the same file
!> at the same time.
!>
!> Also see this issue: https://github.com/fortran-lang/fpm/issues/957 for some more
!> details.
!>
!> What we need is for an `fpm` process \(A\) to see if another `fpm` process \(B\)
!> is already working on a package, and if so, wait for \(B\) to finish that work before
!> \(A\) steps in. The way we do this is with so-called *lock-files*. Basically \(B\)
!> creates a special file named `.fpm-package-lock` in the package directory
!> so \(A\) will see that this file exists and wait for it to be deleted by \(B\),
!> when that is done it means that the package directory is free, and so \(A\) now
!> creates `.fpm-package-lock` itself and does it's thing, after \(A\) is done it
!> deletes the lockfile again.
!>
!> That's pretty much the gist of it. It's complicated somewhat by the fact that
!> we need to consider certain rare cases (what if the program crashes and leaves
!> the lockfile behind). Also, the lockfile operations have to be what's called
!> "atomic". For instance,
!> consider this non-atomic way of creating a lockfile: (in pseudocode)
!>```
!>1)   if file_exists('.fpm-package-lock') then
!>         wait_for_file_to_be_deleted('.fpm-package-lock')
!>2)   create_file('.fpm-package-lock')
!>3)   do_something()
!>4)   delete_file('.fpm-package-lock')
!>```
!> The problem with this code is that `.fpm-packge-lock` may be created by some
!> other process after the check on line (1), but before line (2) has executed,
!> and then it's not very clear what will happen, both processes might think that
!> they are have acquired a lock on the package directory. A better piece of code
!> could be:
!>```
!>error = create_file('.fpm-package-lock')
!>if error == ALREADY_EXISTS then
!>    create_this_file_again_after_deletion('.fpm-package-lock')
!>do_something()
!>delete_file('.fpm-package-lock')
!>```

! IDEA(emma): As things are right now it's up to authors doing IO in package
!             directories to call `fpm_lock_package` and `fpm_unlock_package`
!             as appropriate, which is easy to forget. It would be cool if we
!             had an interface in this modules for opening files that
!             necessitated having acquired a lock first, and so you wouldn't
!             forget as easily. However, that requires refactoring quite a bit
!             and I have no idea of what an appropriate way of doing this in
!             Fortran would be.


module fpm_lock

use :: fpm_error, only : error_t, fatal_error
use :: iso_c_binding, only : c_int, c_bool

implicit none
private
public :: fpm_lock_acquire, fpm_lock_acquire_noblock, fpm_lock_release

logical :: has_lock = .false.

! These are defined in `fpm_lock.c`
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

subroutine fpm_lock_acquire_noblock(error, success, pid)
    type(error_t), allocatable, intent(out) :: error
    logical, optional, intent(out) :: success
    integer, optional, intent(in) :: pid

    integer :: pid_local

    !> unit for open lock-file
    integer :: lock_unit

    !> Error status and message
    integer :: iostat
    character(len=256) :: iomsg

    logical :: exists
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

    ! TODO(@emmabastas): It's imperative that we create the lockfile in an
    !     atomic manner, I don't know if this is atomic.
    open( &
        file='.fpm-package-lock', &
        !status='new', &
        action='readwrite', &
        newunit=lock_unit &
        )
    inquire(unit=lock_unit, iostat=iostat, exist=exists, iomsg=iomsg)

    ! An error occured when opening the file. It could happen because some other
    ! process has the file open already, or something went wrong. In any case
    ! we didn't acquire a lock.
    if (iostat > 0) then
        close(unit=lock_unit)
        if (present(success)) success = .false.
        return
    end if

    ! The lock-file already exists and we managed to open it; This probably means
    ! that another fpm process has a lock already, but there are some edge cases
    ! we need to check before we can be sure.
    if (exists) then
        read(unit=lock_unit, fmt='(1I256)', iostat=iostat, iomsg=iomsg) lock_pid

        ! If iostat is positive some error occured.
        if (iostat > 0) then
            call fatal_error(error, "Error reading lock-file '"//iomsg//"'")
            close(unit=lock_unit)
            if (present(success)) success = .false.
            return
        end if

        ! If iostat is zero then we managed to parse an integer in the lock-file.
        ! If the parsed integer corresponds to the PID of a running process that
        ! isn't this current process then that process has the lock
        if (iostat == 0 .and. lock_pid /= pid_local) then
            ! Fortran doesn't short-circut boolean expressions, hence the
            ! nesting; We only want to check `process_alive` if `lock_pid` is
            ! valid!
            if (process_alive(lock_pid)) then
                close(unit=lock_unit)
                if (present(success)) success = .false.
                return
            end if
        end if

        ! At this point we conclude that altough the lock-file already existed
        ! it wans't valid, so we should go ahead an acquire the lock.
    end if

    rewind(unit=lock_unit, iostat=iostat) ! TODO(@emmabastas) handle errors
    if (iostat > 0) then
        ! TODO(@emmabastas) error handling
        error stop "TODO 3"
    end if
    write(unit=lock_unit, fmt='(1I256)') pid_local

    inquire(unit=lock_unit, iostat=iostat)
    if (iostat > 0) then
        ! TODO(@emmabastas) error handling
        error stop "TODO 2"
    end if

    ! TODO(@emmabastas) error handling
    close(unit=lock_unit)
    if (iostat > 0) then
        ! TODO(@emmabastas) error handling
        error stop "TODO 4"
    end if

    has_lock = .true.
    if (present(success)) success = .true.
end subroutine fpm_lock_acquire_noblock

!> This subroutine blocks until it has acquired a lock on the package directory.
!> You're not allowed to call this subroutine multiple times!
subroutine fpm_lock_acquire(error)
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    logical :: got_lock

    call fpm_lock_acquire_noblock(error, success=got_lock)
    if (allocated(error)) return

    ! TODO(emma): Can we do something better than busy waiting? For instance,
    !             something similar to Linux's `inotify`, but cross-platform.
    do while (.not. got_lock)

        call sleep(1)

        call fpm_lock_acquire_noblock(error, success=got_lock)
        if (allocated(error)) return
    end do
end subroutine fpm_lock_acquire

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

    ! TODO(emma): Is open + close atomic?

    open( &
        file='.fpm-package-lock', &
        newunit=lock_unit, &
        status='old', &
        iostat=iostat, &
        iomsg=iomsg &
        )
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
