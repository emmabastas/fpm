#define _POSIX_SOURCE

#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <signal.h>

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
    *stat = kill(*pid, SIGTERM);
}
