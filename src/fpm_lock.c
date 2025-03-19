#include <stdbool.h>
#include <signal.h>
#include <errno.h>

void c_process_alive(pid_t *pid, bool *alive)  {
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
}
