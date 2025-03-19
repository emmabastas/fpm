#define _POSIX_SOURCE

#include <stdio.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <signal.h>
#include <stdlib.h>
#include <stdbool.h>
#include <errno.h>

void c_dummy_process_start(pid_t *pid) {
    *pid = fork();
    if (*pid == -1) {  // Something went wrong.
        exit(-1);
    }

    if (*pid == 0) {
        while(1) { sleep(1); }
    }
}

void c_kill_process(pid_t *pid, int *stat) {
    // Kill the process.
    *stat = kill(*pid, SIGKILL);

    if (*stat == -1) {
        return;
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
        exit(-1);
    }
}
