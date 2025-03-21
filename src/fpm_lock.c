#include <stdbool.h>

#ifndef _WIN32

#include <signal.h>
#include <errno.h>

#else

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#define pid_t DWORD

#endif

void c_process_alive(pid_t *pid, bool *alive)  {
    #ifndef _WIN32

    // On POSIX we check if the PID is associated with a living process using
    // the kill-null-signal "trick" https://stackoverflow.com/a/9153003
    int status = kill(*pid, 0);

    if (status == 0) {  // Process alive.
        *alive = true;
        return;
    }

    if (status == -1 && errno == ESRCH) {  // Process dead.
        *alive = false;
        return;
    }

    // Something went wrong
    exit(-1);

    #else

    HANDLE handle = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, false, *pid);
    if (handle == NULL) {  // Something went wrong
        perror("c_process_alive: Couldn't obtain handle from PID\n");
        exit(-1);
    }

    DWORD exit_code;
    BOOL success = GetExitCodeProcess(handle, &exit_code);
    if (success == 0) {  // Something went wrong
        perror("c_process_alive: Couldn't obtain exit code from handle");
        exit(-1);
    }

    if(CloseHandle(handle) == 0) {
        perror("c_process_alive: Couldn't close handle.");
        exit(-1);
    }

    // It is possible that a process exists with exit code STILL_ACTIVE (!!) in
    // which case we'll mistakenly belive that the process is still alive..
    if (exit_code == STILL_ACTIVE) {
        *alive = true;
    } else {
        *alive = false;
    }

    #endif
}
