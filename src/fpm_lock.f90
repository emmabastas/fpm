! This module exists to fix a buggy behavior that exists in many package
! managers (however, most users never experience issues with it).
!
! The buggy behaviors is that when many `fpm` processes try to work on the same
! package at the same time the different processes sort of step on one another an
! it leads to problems, for instance two processes might try to compile the same file
! at the same time.
!
! Also see this issue: https://github.com/fortran-lang/fpm/issues/957 for some more
! details.
!
! What we need is for an `fpm` process (A) to see if another `fpm` process (B)
! is already working on a package, and if so, wait for (B) to finish that work before
! (A) steps in. The way we do this is with so-called lock-files. Basically (B)
! creates a special file named `.fpm-package-lock` in the package directory
! so (A) will see that this file exists and wait for it to be deleted by (B),
! when that is done it means that the package directory is free, and so (A) now
! creates `.fpm-package-lock` itself and does it's thing, after (A) is done it
! deletes the lockfile again.
!
! That's pretty much the gist of it. It's complicated somewhat by the fact that
! we need to consider certain rare cases (what if the program crashes and leaves
 ! the lockfile behind). Also, the lockfile operations have to be what's called
! "atomic". For instance,
! consider this non-atomic way of creating a lockfile: (in pseudocode)
!
!   1)   if file_exists('.fpm-package-lock') then
!            wait_for_file_to_be_deleted('.fpm-package-lock')
!   2)   create_file('.fpm-package-lock')
!   3)   do_something()
!   4)   delete_file('.fpm-package-lock')
!
! The problem with this code is that `.fpm-packge-lock` may be created by some
! other process after the check on line (1), but before line (2) has executed,
! and then it's not very clear what will happen, both processes might think that
! they are have acquired a lock on the package directory. A better piece of code
! could be:
!
!   1)   error = create_file('.fpm-package-lock')
!   2)   if error == ALREADY_EXISTS then
!   3)       create_this_file_again_after_deletion('.fpm-package-lock')
!   4)   do_something()
!   5)   delete_file('.fpm-package-lock')

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
use, intrinsic :: iso_fortran_env, only : stdin => input_unit, &
                                        & stdout => output_unit, &
                                        & stderr => error_unit

implicit none
private
public :: fpm_lock_acquire, fpm_lock_release

logical :: has_lock = .false.

contains

subroutine acquire_lock(error)
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    !> unit for open lock-file
    integer :: lock_unit

    !> Error status and message
    integer :: iostat
    integer :: iostat2
    character(len=256) :: iomsg

    logical :: exists
    integer :: lock_pid

    if (has_lock) then
        call fatal_error (error, &
            "Tried locking package directory when it's already locked")
        has_lock = .false.
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
        lock_pid = 0
        return
    end if

    ! The lock-file already exists and we managed to open it; This probably means
    ! that another fpm process has a lock already, but there are some edge cases
    ! we need to check before we can be sure.
    if (iostat == 0 .and. exists) then
        read(unit=lock_unit, fmt='(1I256)', iostat=iostat, iomsg=iomsg) lock_pid

        ! If iostat is negative we reached EOF, and the lock-file contents is
        ! not valid, so we assume there is no lock.
        if (iostat < 0) then
            rewind(unit=lock_unit, iostat=iostat2, iomsg=iomsg)
            if (iostat2 > 0) then
                call fatal_error(error, &
                    "Error rewinding lock-file '"//iomsg//"'")
                has_lock = .false.
                return
            end if

        ! If iostat is positive some error occured.
        else if (iostat > 0) then
            call fatal_error(error, "Error reading lock-file '"//iomsg//"'")
            has_lock = .false.
            return

        ! Handle the very rare sitation where fpm process A acquires a lock, but
        ! dies without removing the lock-file, and a new fpm process B is started
        ! and is assigned the same PID that A had.
        else if (lock_pid == getpid()) then
            ! TODO(@emmabastas) handle error
            rewind(unit=lock_unit)

        ! Check if the process that acquired the lock died without removing the
        ! lockfile.
        else if (.not. process_alive(lock_pid)) then
            ! TODO(@emmabastas)
            error stop ""

            ! TODO(@emmabastas): In the case the process is alive it could be wise
            !     to see what name it has, in case fpm process A acquired the lock,
            !     then died without removing the lockfile. Then a new non-fpm
            !     process is created with the same PID that A had.

        ! At this point we know that that another process has a lock on the
        ! package.
        else
            close(unit=lock_pid)
            lock_pid = 0
            return
        end if
    end if

    ! If we get all the way here it means either that the lock-file didn't exist
    ! and we created it, or the lockfile existed but had been created by a
    ! now dead fpm process. In any case we have the lock at this point.

    write(unit=lock_unit, fmt='(1I256)') getpid()
    inquire(unit=lock_unit, iostat=iostat)
    if (iostat > 0) then
        error stop ""
    end if

    ! TODO(@emmabastas) error handling
    close(unit=lock_unit)

    has_lock = .true.
end subroutine acquire_lock

!> This subroutine blocks until it has acquired a lock on the package directory.
!> You're not allowed to call this subroutine multiple times!
subroutine fpm_lock_acquire(error)
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    call acquire_lock(error)
    if (allocated(error)) then
        return
    end if

    ! TODO(emma): Can we do something better than busy waiting? For instance,
    !             something similar to Linux's `inotify`, but cross-platform.
    do while (.not. has_lock)

        write(stderr, *) "fpm_lock: Waiting to acquire lock..."
        call sleep(1)

        call acquire_lock(error)
    end do

    write(stderr, *) "Acquired lock!"
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

    write(stderr, *) "fpm_lock: Released lock."
    has_lock=.false.
end subroutine fpm_lock_release

! This has a race condition: It's possible that a new process with `pid` is
! created just after `process_alive` returned false
function process_alive(pid) result(alive)
    use iso_c_binding, only : c_int
    integer, intent(in)  :: pid
    logical              :: alive

    integer :: status

    interface
        function c_kill(pid, sig) result(status) bind(c, name="kill")
            ! TODO(emma): The of `pid` should really be `pid_t`, but on modern
            !             platforms this is always and `int`, but still..
            import c_int
            integer(kind=c_int), intent(in), value :: pid
            integer(kind=c_int), intent(in), value :: sig
            integer(kind=c_int) :: status
        end function c_kill
    end interface

    ! This is a common trick to check if a process is alive with POSIX C.
    status = c_kill(pid, 0)
    alive = status == 0
end function process_alive

end module fpm_lock
