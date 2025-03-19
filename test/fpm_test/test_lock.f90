module test_lock

    use testsuite, only : new_unittest, unittest_t, test_failed
    use fpm_error, only : error_t, fatal_error
    use fpm_filesystem, only : run
    use fpm_lock, only : fpm_lock_acquire, fpm_lock_acquire_noblock, &
                         fpm_lock_release
    use iso_c_binding, only : c_int

    implicit none
    private
    public :: collect_lock

interface
    subroutine c_dummy_process_start(pid) bind(c, name = "c_dummy_process_start")
        import c_int
        integer(kind=c_int), intent(out) :: pid
    end subroutine c_dummy_process_start

    subroutine c_kill_process(pid, stat) bind(c, name = "c_kill_process")
        import c_int
        integer(kind=c_int), intent(in)  :: pid
        integer(kind=c_int), intent(out) :: stat
    end subroutine c_kill_process
end interface

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
        & new_unittest('acquire-existing-lockfile-empty', acquire_existing_lockfile_empty), &
        & new_unittest('acquire-existing-lockfile-garbled', acquire_existing_lockfile_garbled), &
        & new_unittest('acquire-existing-lockfile-valid', acquire_existing_lockfile_valid), &
        & new_unittest('acquire-existing-lockfile-same-pid', acquire_existing_lockfile_same_pid), &
        & new_unittest('acquire-existing-lockfile-dead-pid', acquire_existing_lockfile_dead_pid), &
        & new_unittest('acquire-already-opened-lockfile', acquire_already_opened_lockfile) &
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

    subroutine dummy_process_start(pid, error)
        integer, intent(out) :: pid
        type(error_t), allocatable, intent(out) :: error

        call c_dummy_process_start(pid)
        if (pid == -1) then
            call fatal_error(error, "c_dummy_process_start failed")
        end if
    end subroutine dummy_process_start

    subroutine kill_process(pid, error)
        integer, intent(in) :: pid
        type(error_t), allocatable, intent(out) :: error
        integer :: ret

        call c_kill_process(pid, ret)
        if (ret == -1) then
            call fatal_error(error, "c_kill_process failed")
        end if
    end subroutine kill_process

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

    subroutine acquire_existing_lockfile_empty(error)
        type(error_t), allocatable, intent(out) :: error
        integer :: pid
        logical :: success

        call delete_lock_file()
        call run('touch .fpm-package-lock')
        call fpm_lock_acquire_noblock(error, success=success)

        if (.not. success) then
            call test_failed(error, "Failed getting a package lock")
        end if
    end subroutine acquire_existing_lockfile_empty

    subroutine acquire_existing_lockfile_garbled(error)
        type(error_t), allocatable, intent(out) :: error
        integer :: pid
        logical :: success

        call delete_lock_file()
        call run('echo "Lorem ipsum dolor sit amet" > .fpm-package-lock')
        call fpm_lock_acquire_noblock(error, success=success)

        if (.not. success) then
            call test_failed(error, "Failed getting a package lock")
        end if

        call fpm_lock_release(error)
    end subroutine acquire_existing_lockfile_garbled

    subroutine acquire_existing_lockfile_valid(error)
        type(error_t), allocatable, intent(out) :: error
        integer :: pid
        logical :: success

        ! Clean up if needed.
        call run('rm -f .fpm-package-lock')

        ! Simulate some other process.
        call dummy_process_start(pid, error)
        if (allocated(error)) return

        ! Pretend that the other process acquired a package lock.
        call fpm_lock_acquire_noblock(error, pid=pid, success=success)
        if (allocated(error)) return
        if (.not. success) then
            call test_failed(error, "Failed acquiring initial lock")
            return
        end if

        ! Since locking multiple times isn't allowed we need to perform this
        ! little maneuver.
        call run('cp .fpm-package-lock .fpm-package-lock.dup')
        call fpm_lock_release(error)
        if (allocated(error)) return
        call run('mv .fpm-package-lock.dup .fpm-package-lock')

        ! We expect this to not succeed, (but no errors should be raised).
        call fpm_lock_acquire_noblock(error, success=success)
        if (allocated(error)) return

        if (success) then
            call test_failed(error, &
                "Expected package lock to fail when another process had a "// &
                "lock, but it succeeded")
            call fpm_lock_release(error)
        end if

        ! Clean up.
        call kill_process(pid, error)
    end subroutine acquire_existing_lockfile_valid

    subroutine acquire_existing_lockfile_same_pid(error)
        type(error_t), allocatable, intent(out) :: error
        logical :: success

        ! Clean up if needed.
        call run('rm -f .fpm-package-lock')

        ! We pretend that some earlier process with the same pid that we happen
        ! to have now acquired the lock.
        call fpm_lock_acquire_noblock(error, pid=getpid(), success=success)
        if (allocated(error)) return
        if (.not. success) then
            call test_failed(error, "Failed acquiring initial lock")
            return
        end if

        ! Since locking multiple times isn't allowed we need to perform this
        ! little maneuver.
        call run('cp .fpm-package-lock .fpm-package-lock.dup')
        call fpm_lock_release(error)
        if (allocated(error)) return
        call run('mv .fpm-package-lock.dup .fpm-package-lock')

        ! We expect this to succeed.
        call fpm_lock_acquire_noblock(error, success=success)
        if (allocated(error)) return

        if (.not. success) then
            call test_failed(error, &
                "Expected package to lock succeed")
        end if

        ! Clean up.
        call fpm_lock_release(error)
    end subroutine acquire_existing_lockfile_same_pid

    subroutine acquire_existing_lockfile_dead_pid(error)
        type(error_t), allocatable, intent(out) :: error
        integer :: pid
        logical :: success

        ! Clean up if needed.
        call run('rm -f .fpm-package-lock')

        ! Simulate some other process.
        call dummy_process_start(pid, error)
        if (allocated(error)) return

        ! Pretend that the other process acquires a packag lock.
        call fpm_lock_acquire_noblock(error, pid=pid, success=success)
        if (allocated(error)) return
        if (.not. success) then
            call test_failed(error, "Failed acquiring initial lock")
            return
        end if

        ! Since locking multiple times isn't allowed we need to perform this
        ! little maneuver.
        call run('cp .fpm-package-lock .fpm-package-lock.dup')
        call fpm_lock_release(error)
        if (allocated(error)) return
        call run('mv .fpm-package-lock.dup .fpm-package-lock')

        ! Ooops the process died.
        call kill_process(pid, error)
        if (allocated(error)) return

        ! Since the other process is dead we expect this to succeed (but it
        ! souldn't raise an error).
        call fpm_lock_acquire_noblock(error, success=success)
        if (allocated(error)) return

        if (.not. success) then
            call test_failed(error, &
                "Expected package lock to succeed")
            return
        end if

        ! Clean up.
        call fpm_lock_release(error)
    end subroutine acquire_existing_lockfile_dead_pid

    subroutine acquire_already_opened_lockfile(error)
        type(error_t), allocatable, intent(out) :: error
        logical :: success

        ! Clean up if needed.
        call run('rm -f .fpm-package-lock')

        ! Create a lock-file and keep it open indeffinetly
        call run('touch .fpm-package-lock && tail -f .fpm-package-lock &')

        ! Even though the lock file is empty we expect that no lock is
        ! acquired since another process actively has the lock-file open.
        call fpm_lock_acquire_noblock(error, success=success)
        if (allocated(error)) return

        if (success) then
            call test_failed(error, &
                "Expected package lock to fail but it succeeded")
            call fpm_lock_release(error)
        end if

        ! Clean up.
        call run('rm -f .fpm-package-lock')
    end subroutine acquire_already_opened_lockfile
end module test_lock
