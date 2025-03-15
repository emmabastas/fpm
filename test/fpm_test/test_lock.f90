module test_lock

    use testsuite, only : new_unittest, unittest_t, error_t, test_failed
    use fpm_lock, only : fpm_lock_acquire, fpm_lock_release

    implicit none
    private
    public :: collect_lock

contains

    !> Collect unit tests.
    subroutine collect_lock(tests)

        !> Unit tests to collet.
        type(unittest_t), allocatable, intent(out) :: tests(:)

        tests = [ &
        & new_unittest('simple-acquire-release', simple_acquire_release), &
        & new_unittest('acquire-release-acquire-release', acquire_release_acquire_release), &
        & new_unittest('double-acquire', double_acquire, should_fail=.true.), &
        & new_unittest('release', release, should_fail=.true.), &
        & new_unittest('acquire-release-release', acquire_release_release, should_fail=.true.), &
        & new_unittest('acquire-existing-lockfile-valid', acquire_existing_lockfile_valid), &
        & new_unittest('acquire-existing-lockfile-empty', acquire_existing_lockfile_empty), &
        & new_unittest('acquire-existing-lockfile-garbled', acquire_existing_lockfile_garbled), &
        & new_unittest('acquire-existing-lockfile-same-pid', acquire_existing_lockfile_same_pid), &
        & new_unittest('acquire-existing-lockfile-dead-pid', acquire_existing_lockfile_dead_pid) &
        ]

    end subroutine collect_lock

    subroutine delete_lock_file
        integer :: lock_unit
        integer :: iostat

        open( &
            file='.fpm-package-lock', &
            status='old', &
            newunit=lock_unit, &
            iostat=iostat &
            )

        if (iostat == 0) close(unit=lock_unit, status="delete")
    end subroutine delete_lock_file

    subroutine simple_acquire_release(error)
        type(error_t), allocatable, intent(out) :: error

        call delete_lock_file()
        call fpm_lock_acquire(error)
        call fpm_lock_release(error)
    end subroutine simple_acquire_release

    subroutine acquire_release_acquire_release (error)
        type(error_t), allocatable, intent(out) :: error

        call delete_lock_file()
        call fpm_lock_acquire(error)
        call fpm_lock_release(error)
        call fpm_lock_acquire(error)
        call fpm_lock_release(error)
    end subroutine acquire_release_acquire_release

    subroutine double_acquire(error)
        type(error_t), allocatable, intent(out) :: error

        call delete_lock_file()
        call fpm_lock_acquire(error)
        call fpm_lock_acquire(error)
    end subroutine double_acquire

    subroutine release(error)
        type(error_t), allocatable, intent(out) :: error

        call delete_lock_file()
        call fpm_lock_release(error)
    end subroutine release

    subroutine acquire_release_release(error)
        type(error_t), allocatable, intent(out) :: error

        call delete_lock_file()
        call fpm_lock_acquire(error)
        call fpm_lock_release(error)
        call fpm_lock_release(error)
    end subroutine acquire_release_release

    subroutine acquire_existing_lockfile_valid(error)
        type(error_t), allocatable, intent(out) :: error
    end subroutine acquire_existing_lockfile_valid

    subroutine acquire_existing_lockfile_empty(error)
        type(error_t), allocatable, intent(out) :: error
    end subroutine acquire_existing_lockfile_empty

    subroutine acquire_existing_lockfile_garbled(error)
        type(error_t), allocatable, intent(out) :: error
    end subroutine acquire_existing_lockfile_garbled

    subroutine acquire_existing_lockfile_same_pid(error)
        type(error_t), allocatable, intent(out) :: error
    end subroutine acquire_existing_lockfile_same_pid

    subroutine acquire_existing_lockfile_dead_pid(error)
        type(error_t), allocatable, intent(out) :: error
    end subroutine acquire_existing_lockfile_dead_pid

end module test_lock
