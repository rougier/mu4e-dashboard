;;; regression-tests.el --- basic regression tests for mu4e-dashboard.el

;;; Commentary:

;; These tests will load the two sample dashboards, activate them, and
;; assure basic functionality.

;;; Code:

(require 'ert)
(require 'mu4e-dashboard)

(setq-default
 mu4e-dashboard-mu-program
 (format "%s/test/mock-mu-cli" (getenv "srcdir")))

(ert-deftest smoke-tests ()
  "Simple mu4e-dashboard test."
  (find-file (format "%s/test/trivial-dash.org" (getenv "srcdir")))
  (mu4e-dashboard-mode)

  (while (< (buffer-chars-modified-tick) 8)
    (sit-for 1)
    (while (accept-process-output)))

  (should
   (string-equal
    (buffer-string)
    "
* Mailboxes                 *[[mu:flag:unread|%2d][17]]*

[[mu:flag:unread][Unread]] /[[mu:flag:unread|(%3d)][( 17)]]/ .... [u]  [[mu:date:today..now][Today]] /[[mu:date:today..now|(%3d)][(113)]]/ .......... [t]  *Compose* ...... [C]

#+KEYMAP: u | mu4e-headers-search \"flag:unread\"
#+KEYMAP: t | mu4e-headers-search \"date:today..now\"
#+KEYMAP: C | mu4e-compose-new
"))

  (should
   (string-match-p
    (regexp-quote "mu4e-compose-new")
    (prin1-to-string (lookup-key (current-local-map) "C")))))
