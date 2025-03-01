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

use, intrinsic :: iso_fortran_env, only : stdin => input_unit, &
                                        & stdout => output_unit, &
                                        & stderr => error_unit

implicit none
private
public :: fpm_lock_package, fpm_unlock_package

integer :: lock_unit = 0  !> value of 0 indicates that we don't have a lock.

contains

!> This subroutine blocks until it has acquired a lock on the package directory.
!> You're not allowed to call this subroutine multiple times!
subroutine fpm_lock_package()
  logical :: acquired

  if (lock_unit /= 0) then
     error stop "fpm_lock: double-lock"
  end if

  call acquire_lock(acquired)

  ! TODO(emma): Can we do something better than busy waiting? For instance,
  !             something similar to Linux's `inotify`, but cross-platform.
  do while (.not. acquired)

     write(stderr, *) "fpm_lock: Waiting to acuire lock..."
     call sleep(1)

     call acquire_lock(acquired)

     if (acquired) then
        write(stderr, *) "Acquired lock!"
     end if
  end do
end subroutine fpm_lock_package

subroutine acquire_lock(acquired)
  logical, intent(out) :: acquired

  integer :: iostat

  ! TODO(emma): Write PID to lockfile.
  ! TODO(emma): Check if a previous fpm process died without removing lockfile.

  ! TODO(emma): It's imperative that we create the lockfile in an atomic manner,
  !             I don't know if this is atomic.
  open(file='.fpm-package-lock', status='new', action='readwrite', iostat=iostat, newunit=lock_unit)

  acquired = iostat == 0
end subroutine acquire_lock

subroutine fpm_unlock_package()
  integer :: iostat

  if (lock_unit == 0) then
     error stop "fpm_lock: I tried unlocking the package, but I hadn't locked it in the first place!"
  end if

  ! TODO(emma): It's imperative that we delete the lockfile in an atomic manner,
  !             I don't know if this is atomic.
  close(lock_unit, status='delete', iostat=iostat)

  if (iostat /= 0) then
     error stop "fpm_lock: I tried to unlock the package by deleting the .package-lock.lcok file, but it failed!"
  end if

  lock_unit = 0
end subroutine fpm_unlock_package

end module fpm_lock
