#define _POSIX_SOURCE

#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <signal.h>
#include <stdlib.h>
#include <stdbool.h>
#include <errno.h>

#ifndef _WIN32

#include <sys/wait.h>

#else

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#define pid_t DWORD

#endif

// Start a process that does nothing, returns the PID of the new process in
// `pid_ret`.
void c_dummy_process_start(pid_t *pid_ret) {
#ifndef _WIN32  // POSIX implementation.
    *pid_ret = fork();
    if (*pid_ret == -1) {  // Something went wrong.
        exit(-1);
    }

    if (*pid_ret == 0) {
        while(1) { sleep(1); }
    }
#else  // Windows implementation.
    STARTUPINFO sinfo;
    PROCESS_INFORMATION pinfo;

    ZeroMemory(&sinfo, sizeof(sinfo));
    sinfo.cb = sizeof(sinfo);

    // We just want some random windows .exe that does nothing.
    BOOL success = CreateProcessA("C:\\Windows\\System32\\timeout.exe",
                                  "/t 99999",
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
