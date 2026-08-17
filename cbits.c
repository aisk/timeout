/* Snapshot inherited signal dispositions before the GHC RTS and the
   top-level interrupt handler replace them (SIGINT in particular is
   overridden before main runs), so timeout can honor SIG_IGN set by
   the shell for background jobs like GNU timeout does. */
#include <signal.h>
#include <stddef.h>

#define CAPTURED_SIGNALS NSIG

static char inherited_ignored[CAPTURED_SIGNALS];

__attribute__((constructor)) static void capture_signal_dispositions(void)
{
    for (int sig = 1; sig < CAPTURED_SIGNALS; sig++) {
        struct sigaction sa;
        if (sigaction(sig, NULL, &sa) == 0 && sa.sa_handler == SIG_IGN)
            inherited_ignored[sig] = 1;
    }
}

int timeout_signal_was_ignored(int sig)
{
    return 0 < sig && sig < CAPTURED_SIGNALS && inherited_ignored[sig];
}
