#define _POSIX_SOURCE

#include <signal.h>
#include <stdbool.h>
#include <fcntl.h>

#ifndef _WIN32

#include <sys/wait.h>
#include <errno.h>

#else

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#define pid_t DWORD

#endif

void c_dummy_process_start(pid_t *pid_ret) {
#ifndef _WIN32  // POSIX implementation.
    *pid_ret = fork();
    if (*pid_ret == -1) {  // Something went wrong.
        perror("c_dummy_process_start: Couldn't fork");
        exit(-1);
    }

    if (*pid_ret == 0) {
        while(1) { sleep(1); }
    }

    return;
#else  // Windows implementation.
    STARTUPINFO sinfo;
    PROCESS_INFORMATION pinfo;

    ZeroMemory(&sinfo, sizeof(sinfo));
    sinfo.cb = sizeof(sinfo);

    // We just want some random windows .exe that does nothing.
    BOOL success = CreateProcessA("C:\\Windows\\System32\\timeout.exe",
                                  "/t 9999",
                                  NULL,
                                  NULL,
                                  false,
                                  NORMAL_PRIORITY_CLASS,
                                  NULL,
                                  NULL,
                                  &sinfo,
                                  &pinfo
    );

    if (success == 0) {  // Something went wrong.
        perror("c_dummy_process_start: Couldn't start dummy process\n");
        exit(-1);
    } else {
        *pid_ret = pinfo.dwProcessId;
    }

    success &= CloseHandle(pinfo.hProcess);
    success &= CloseHandle(pinfo.hThread);
    if (success == 0) {  // Something went wrong.
        perror("c_dummy_process_start: Couldn't close handles\n");
        exit(-1);
    }
#endif
}

void c_kill_process(pid_t *pid) {
#ifndef _WIN32
    // Kill the process.
    int stat = kill(*pid, SIGKILL);

    if (stat == -1) {
        perror("c_kill_process: Couldn't kill process");
        exit(-1);
    }

    // Wait until process has actually died.
    while(true) {
        int status;
        int ret = waitpid(*pid, &status, 0);
        if (ret == *pid) {  // The child process was killed.
            return;
        }

        if (ret == -1 && errno == ECHILD) {  // The child process was killed
                                             // before we entered the loop.
            return;
        }

        // Something went wrong
        perror("c_kill_process: Couldn't get process status");
        exit(-1);
    }
#else
    HANDLE handle = OpenProcess(PROCESS_TERMINATE, false, *pid);
    if (handle == NULL) {  // Something went wrong.
        perror("c_kill_process: Couldn't obtain handle from PID");
        exit(-1);
    }

    BOOL success = TerminateProcess(handle, 9);

    if(CloseHandle(handle) == 0) {  // Something went wrong.
        perror("c_kill_process: Couldn't close handle.");
        exit(-1);
    }

    if (success == 0) {
        perror("c_kill_process: Couldn't terminate process.");
        exit(-1);
    }
#endif
}

int c_lock_file(char *path) {
    int ret = open(path, O_RDWR);
    if (ret == -1) {
        perror("c_lock_file: Couldn't open file");
    }
    return ret;
}

void c_unlock_file(int fd) {
    int ret = close(fd);
    if (ret == -1) {
        perror("c_close_file: Couldnät close file");
    }
}

