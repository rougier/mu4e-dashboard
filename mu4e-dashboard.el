;;; mu4e-dashboard.el --- Mu4e dashboard -*- lexical-binding: t -*-

;; Copyright (C) 2020 Nicolas P. Rougier

;; Author: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; Homepage: https://github.com/rougier/mu4e-dashboard
;; Keywords: convenience
;; Version: 0.1

;; Package-Requires: ((emacs "26.1"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; mu4e-dashboard provides enhanced org-mode links that allow you to
;; define custom dashboards that link back to the mu4e email client.
;;


(require 'subr-x)
(require 'ob-shell)


;; Timer handler
;; (defvar mu4e-dashboard--timer nil)

;; Dashboard buffer
(defvar mu4e-dashboard--buffer nil)

;; Install the mu4e link type
(defgroup mu4e-dashboard nil
  "Provides a new Org mode link type for mu4e queries."
  :group 'comm)

(defcustom mu4e-dashboard-link-name "mu"
  "Default link name."
  :type 'string)

(defcustom mu4e-dashboard-file nil
  "Org file containing the dashboard template."
  :type 'string)

(org-link-set-parameters
 mu4e-dashboard-link-name
 :follow #'mu4e-dashboard-follow-mu4e-link)

;; Minor mode to simulate buffer local keybindings.
;;;###autoload
(define-minor-mode mu4e-dashboard-mode
  "Minor mode to simulate buffer local keybindings."
  :keymap (make-sparse-keymap)
  :init-value nil)


(defun mu4e-dashboard-follow-mu4e-link (path)
  "Process a mu4e link of the form [[mu4e:query|fmt|limit][(---------)]].

If FMT is not specified or is nil, clicking on the link calls
mu4e with the specified QUERY (with or without the given
LIMIT). If FMT is specified, the description of the link is
updated with the QUERY count formatted using the provided
format (for example \"%4d\")."
  
  (let* ((link    (org-element-context))
         (query   (string-trim (nth 0 (split-string path "|"))))
         (fmt     (nth 1 (split-string path "|")))
         (count   (nth 2 (split-string path "|"))))
    (cond
     ;; Regular query without limit
     ((and (not fmt) (not count))
      (mu4e-headers-search query))

     ;; Regular query with limit
     ((and count (> (length count) 0))
      (let ((mu4e-headers-results-limit (string-to-number count)))
        (mu4e-headers-search query)))

     ;; Query count and link description update
     ((and fmt (> (length fmt) 0))
       (mu4e-dashboard-update-link link)))))


(defun mu4e-dashboard-update-link (link)
  "Update content of a formatted mu4e LINK.

A formatted link is a link of the form
[[mu4e:query|limit|fmt][(---------)]] where fmt is a non nil
string describing the format. When a link is cleared, the
description is replaced by a string for the form \"(---)\" and
have the same size as the current description. If the given
format is too big for the current description, description is
replaced with + signs."
  
  (let* ((path  (org-element-property :path link))
         (query (string-trim (nth 0 (split-string path "|"))))
         (fmt   (nth 1 (split-string path "|")))
         (count (nth 2 (split-string path "|")))
         (beg   (org-element-property :contents-begin link))
         (end   (org-element-property :contents-end link))
         (size  (- end beg))
         )
    (if (and fmt (> (length fmt) 0))
        (let* ((command (format "mu find %s 2> /dev/null | wc -l" query))
               (output (string-to-number (shell-command-to-string command)))
               (output  (format fmt output)))
          (with-current-buffer mu4e-dashboard--buffer
            (let ((modified (buffer-modified-p))
                  (inhibit-read-only t))
              (save-excursion
                (delete-region beg end)
                (goto-char beg)
                (insert (if (<= (length output) size) output
                          (make-string size ?+))))
              (set-buffer-modified-p modified)))))))


(defun async-shell-command-to-string (command callback)
  " Run a shell command in an asynchronous way. Once the call
terminates, callback is called with the result."
  
  (let* ((display-buffer-alist (list (cons "\\*Async Shell Command\\*.*"
                                       (cons #'display-buffer-no-window nil))))
         (output-buffer (generate-new-buffer "*Async Shell Command*"))
         (proc (progn
                 (async-shell-command command output-buffer)
                 (get-buffer-process output-buffer))))
    (if (process-live-p proc)
        (set-process-sentinel proc
                              (lambda (process signal)
                                (when (memq (process-status process) '(exit signal))
                                  (with-current-buffer output-buffer
                                    (funcall callback (buffer-string)))
                                  (kill-buffer output-buffer))))
      (message "No process running."))))


(defun mu4e-dashboard-update-all-async ()
  "Update content of all formatted mu4e links in an asynchronous way.

A formatted link is a link of the form
[[mu4e:query|limit|fmt][(---------)]] where fmt is a non nil
string describing the format.  When a link is cleared, the
description is replaced by a string for the form \"(---)\" and
have the same size as the current description."
  
  (let ((buffer (current-buffer)))
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (when (string= (org-element-property :type link) mu4e-dashboard-link-name)
          (let* ((path  (org-element-property :path link))
                 (query (string-trim (nth 0 (split-string path "|"))))
                 (fmt   (nth 1 (split-string path "|")))
                 (beg   (org-element-property :contents-begin link))
                 (end   (org-element-property :contents-end link))
                 (size  (if (and beg end) (- end beg) 0)))
            (when (and fmt (> (length fmt) 0))
                ;; The rest of this function will execute successfully with a
                ;; `size' of zero, but since there would be no reason to
                ;; proceed with no output, we signal an error.
                (if (eq size 0)
                    (error "The link ``%s'' has a format clause, but no output width" path))
                (let ((command (format "mu find %s 2> /dev/null | wc -l" query)))
                  (async-shell-command-to-string command
                      (lambda (output)
                        (with-current-buffer buffer
                          (let ((modified (buffer-modified-p))
                                (inhibit-read-only t)
                                (output (format fmt (string-to-number output))))
                            (save-excursion
                              (delete-region beg end)
                              (goto-char beg)
                              (insert (if (<= (length output) size) output
                                        (make-string size ?+))))
                            (set-buffer-modified-p modified)))))))))))))
                            

(defun mu4e-dashboard-upate-all-sync ()
  "Update content of all mu4e formatted links in a synchronous way.

A formatted link is a link of the form
[[mu4e:query|limit|fmt][(---------)]] where fmt is a non nil
string describing the format. When a link is cleared, the
description is replaced by a string for the form \"(---)\" and
have the same size as the current description."

  (mu4e-dashboard-clear-all)
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (string= (org-element-property :type link) mu4e-dashboard-link-name)
        (mu4e-dashboard-update-link link)
        (redisplay t)))))



(defun mu4e-dashboard-clear-link (link)
  "Clear a formatted mu4e link.

A formatted link is a link of the form
[[mu4e:query|limit|fmt][(---------)]] where fmt is a non nil
string describing the format. When a link is cleared, the
description is replaced by a string for the form \"(---)\" and
have the same size as the current description."
  
  (let* ((path (org-element-property :path link))
         (fmt  (nth 1 (split-string path "|")))
         (beg  (org-element-property :contents-begin link))
         (end  (org-element-property :contents-end link))
         (size (- end beg)))
    (if (and fmt (> (length fmt) 0))
        (with-current-buffer mu4e-dashboard--buffer
          (let ((modified (buffer-modified-p))
                (inhibit-read-only t))
            (save-excursion
              (delete-region beg end)
              (goto-char beg)
              (insert (format "(%s)" (make-string (- size 2) ?-))))
            (set-buffer-modified-p modified))))))

(defun mu4e-dashboard-clear-all ()
  "Clear all formatted mu4e links.

A formatted link is a link of the form
[[mu4e:query|limit|fmt][(---------)]] where fmt is a non nil
string describing the format. When a link is cleared, the
description is replaced by a string for the form \"(---)\" and
have the same size as the current description."
  
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (string= (org-element-property :type link) mu4e-dashboard-link-name)
        (mu4e-dashboard-clear-link link))))
  (redisplay t))


;;;###autoload
(defun mu4e-dashboard-activate ()
  "Activate the dashboard by installing keybindings and starting
the automatic update"
  (interactive)
  (setq mu4e-dashboard--buffer (current-buffer))
  (setq buffer-read-only t)
;;  (if mu4e-dashboard--timer
;;      (cancel-timer mu4e-dashboard--timer))
  (setq mu4e-dashboard--timer nil)
  (mu4e-dashboard-mode t)
  (mu4e-dashboard-parse-keymap)
  (add-hook 'mu4e-index-updated-hook
            'mu4e-dashboard-update)
  (mu4e-dashboard-update)
  (message (concat "["
                   (propertize "mu4e dashboard" 'face 'bold)
                   "] Activated"))
;;  (setq mu4e-dashboard--timer
;;        ;;  (run-at-time nil mu4e-update-interval 'mu4e-dashboard-update)))
;;        (run-with-idle-timer mu4e-update-interval t 'mu4e-dashboard-update))
  )

(defun mu4e-dashboard-quit ()
  "Quit the dashboard"
  (interactive)
  (with-current-buffer mu4e-dashboard--buffer
;;    (if mu4e-dashboard--timer
;;        (cancel-timer mu4e-dashboard--timer))
;;    (setq mu4e-dashboard--timer nil)
    (kill-current-buffer)))

(defun mu4e-dashboard-update ()
  "Update mu4e index and dashboard"
  (interactive)
  (with-current-buffer mu4e-dashboard--buffer
    (message (concat
              "["
              (propertize "mu4e dashboard" 'face 'bold)
              "] "
              (format-time-string "Update (%H:%M)")))
    ;; (mu4e-update-mail-and-index t)
;;    (with-current-buffer mu4e-dashboard--buffer
;;      (let ((modified (buffer-modified-p))
;;            (inhibit-read-only t))
;;        (save-excursion (org-babel-execute-buffer))
;;        (set-buffer-modified-p modified)))
    (mu4e-dashboard-update-all-async)))

(defun mu4e-dashboard-deactivate ()
  "Deactivate the dashboard by uninstalling keybindings and
stopping the automatic update"
  (interactive)
  (setq buffer-read-only nil)
;;  (if mu4e-dashboard--timer
;;      (cancel-timer mu4e-dashboard--timer))
;;  (setq mu4e-dashboard--timer nil)
  (mu4e-dashboard-mode 0)
  (remove-hook 'mu4e-index-updated-hook
            'mu4e-dashboard-update)
  (message (concat
            "["
            (propertize "mu4e dashboard" 'face 'bold)
            "] Deactivated")))

;;;###autoload
(defun mu4e-dashboard-toggle ()
  "Toggle mu4e-dashboard mode on and off."
  (interactive)
  (when (and (eq major-mode 'org-mode)
             (boundp mu4e-dashboard-mode))
    (if mu4e-dashboard-mode
        (mu4e-dashboard-deactivate)
      (mu4e-dashboard-activate))))

(defun mu4e-dashboard-parse-keymap ()
  "Parse an org file for keywords of type KEYMAP:VALUE and
install the corresponding key bindings in the mu4e-dashboard
minor mode keymap. Previous keymap (if any) is erased.

VALUE is composed of \"keybinding | function-call\" with keybidning
begin a string describing a key sequence and a call to an existing
function. For example, to have 'q' to kill the current buffer, the
 syntax would be:

#+KEYMAP: q | kill-current-buffer

This can be placed anywhere in the org file even though I advised
to group keymaps at the same place."

  (define-key mu4e-dashboard-mode-map (kbd "return") 'org-open-at-point)
  
  (org-element-map (org-element-parse-buffer) 'keyword
    (lambda (keyword)
      (when (string= (org-element-property :key keyword) "KEYMAP")
        (let* ((value (org-element-property :value keyword))
               (key   (string-trim (nth 0 (split-string value "|"))))
               (call  (string-trim (nth 1 (split-string value "|")))))
          (define-key mu4e-dashboard-mode-map (kbd key)
            (eval (car (read-from-string
                        (format "(lambda () (interactive) (%s))" call)))))
;;          (message (format "mu4e-dashboard: binding %s to %s"
;;                          key (format "(lambda () (interactive) (%s))" call)))
          )))))


(defconst mu4e-dashboard-buffer-name " *mu4e-dashboard*"
  "Name of the buffer where mu4e-dashboard shows directory contents.")


(defcustom mu4e-dashboard-window-position 'left
  "*The position of mu4e-dashboard window."
  :group 'mu4e-dashboard
  :type '(choice (const left)
                 (const right)))

(defcustom mu4e-dashboard-display-action '(mu4e-dashboard-default-display-fn)
  "*Action to use for displaying mu4e-dashboard window.
If you change the action so it doesn't use
`mu4e-dashboard-default-display-fn', then other variables such as
`mu4e-dashboard-window-position' won't be respected when opening mu4e-dashboard
window."
  :type 'sexp
  :group 'mu4e-dashboard)


(defcustom mu4e-dashboard-toggle-window-keep-p nil
  "If not nil, not switch to *mu4e-dashboard* buffer when executing `mu4e-dashboard-toggle-window'."
  :type 'boolean
  :group 'mu4e-dashboard)

(defcustom mu4e-dashboard-window-width 30
  "*Specifies the width of the mu4e-dashboard window."
  :type 'integer
  :group 'mu4e-dashboard)

(defcustom mu4e-dashboard-window-fixed-size t
  "*If the mu4e-dashboard windows is fixed, it won't be resize when rebalance windows."
  :type 'boolean
  :group 'mu4e-dashboard)

(defvar mu4e-dashboard-global--buffer nil)

(defvar mu4e-dashboard-global--window nil)


(defmacro mu4e-dashboard-global--with-buffer (&rest body)
  "Execute the forms in BODY with global mu4e-dashboard buffer."
  (declare (indent 0) (debug t))
  `(let ((mu4e-dashboard-buffer (mu4e-dashboard-global--get-buffer)))
     (unless (null mu4e-dashboard-buffer)
       (with-current-buffer mu4e-dashboard-buffer
         ,@body))))


(defmacro mu4e-dashboard-buffer--with-resizable-window (&rest body)
  "Execute BODY in mu4e-dashboard window without `window-size-fixed' restriction."
  `(let (rlt)
     (mu4e-dashboard-global--with-buffer
       (mu4e-dashboard-buffer--unlock-width))
     (setq rlt (progn ,@body))
     (mu4e-dashboard-global--with-buffer
       (mu4e-dashboard-buffer--lock-width))
     rlt))

(defmacro mu4e-dashboard-global--switch-to-buffer ()
  "Switch to mu4e-dashboard buffer."
  `(let ((mu4e-dashboard-buffer (mu4e-dashboard-global--get-buffer)))
     (unless (null mu4e-dashboard-buffer)
       (switch-to-buffer mu4e-dashboard-buffer))))

(defmacro mu4e-dashboard-global--with-window (&rest body)
  "Execute the forms in BODY with global mu4e-dashboard window."
  (declare (indent 0) (debug t))
  `(save-selected-window
     (mu4e-dashboard-global--select-window)
     ,@body))


(defun mu4e-dashboard-global--set-window-width (width)
  "Set mu4e-dashboard window width to WIDTH."
  (mu4e-dashboard-global--with-window
    (mu4e-dashboard-buffer--with-resizable-window
     (mu4e-dashboard-util--set-window-width (selected-window) width))))

(defun mu4e-dashboard-util--set-window-width (window n)
  "Make WINDOW N columns width."
  (let ((w (max n window-min-width)))
    (unless (null window)
      (if (> (window-width) w)
          (shrink-window-horizontally (- (window-width) w))
        (if (< (window-width) w)
            (enlarge-window-horizontally (- w (window-width))))))))


(defun mu4e-dashboard-buffer--create ()
  "Create and switch to mu4e-dashboard buffer."
  (find-file mu4e-dashboard-file)
  (rename-buffer mu4e-dashboard-buffer-name)
  (mu4e-dashboard-activate)
  ;; disable linum-mode
  (when (and (boundp 'linum-mode)
             (not (null linum-mode)))
    (linum-mode -1))
  (current-buffer))

(defun mu4e-dashboard-global--attach ()
  "Attach the global mu4e-dashboard buffer"
  (setq mu4e-dashboard-global--buffer (get-buffer mu4e-dashboard-buffer-name))
  (setq mu4e-dashboard-global--window (get-buffer-window
                            mu4e-dashboard-global--buffer))
  (mu4e-dashboard-global--with-buffer
    (mu4e-dashboard-buffer--lock-width))
  (run-hook-with-args 'mu4e-dashboard-after-create-hook '(window)))

(defun mu4e-dashboard-buffer--lock-width ()
  "Lock the width size for mu4e-dashboard window."
  (if mu4e-dashboard-window-fixed-size
      (setq window-size-fixed 'width)))

(defun mu4e-dashboard-buffer--unlock-width ()
  "Unlock the width size for mu4e-dashboard window."
  (setq window-size-fixed nil))

(defun mu4e-dashboard-global--reset-width ()
  "Set mu4e-dashboard window width to `mu4e-dashboard-window-width'."
  (mu4e-dashboard-global--set-window-width mu4e-dashboard-window-width))

;;;###autoload
(defun mu4e-dashboard-toggle-window ()
  "Toggle show the mu4e-dashboard window."
  (interactive)
  (if (mu4e-dashboard-global--window-exists-p)
      (mu4e-dashboard-hide)
    (mu4e-dashboard-show)))

;;;###autoload
(defun mu4e-dashboard-show ()
  "Show the mu4e-dashboard window."
  (interactive)
  (let ((cw (selected-window))
        (path (buffer-file-name)))  ;; save current window and buffer
    (mu4e-dashboard-global--select-window)
    (mu4e-dashboard-global--open)
    (when mu4e-dashboard-toggle-window-keep-p
      (select-window cw))))

;;;###autoload
(defun mu4e-dashboard-hide ()
  "Close the mu4e-dashboard window."
  (interactive)
  (if (mu4e-dashboard-global--window-exists-p)
      (delete-window mu4e-dashboard-global--window)))

(defun mu4e-dashboard-window--init (window buffer)
  "Make WINDOW a mu4e-dashboard window.
mu4e-dashboard buffer is BUFFER."
  (mu4e-dashboard-buffer--with-resizable-window
   (switch-to-buffer buffer)
   (set-window-parameter window 'no-delete-other-windows t)
   (set-window-dedicated-p window t))
  window)

(defun mu4e-dashboard-global--window-exists-p ()
  "Return non-nil if mu4e-dashboard window exists."
  (and (not (null (window-buffer mu4e-dashboard-global--window)))
       (eql (window-buffer mu4e-dashboard-global--window) (mu4e-dashboard-global--get-buffer))))

(defun mu4e-dashboard-global--select-window ()
  "Select the mu4e-dashboard window."
  (interactive)
  (let ((window (mu4e-dashboard-global--get-window t)))
    (select-window window)))

(defun mu4e-dashboard-global--get-window (&optional auto-create-p)
  "Return the mu4e-dashboard window if it exists, else return nil.
But when the mu4e-dashboard window does not exist and AUTO-CREATE-P is non-nil,
it will create the mu4e-dashboard window and return it."
  (unless (mu4e-dashboard-global--window-exists-p)
    (setf mu4e-dashboard-global--window nil))
  (when (and (null mu4e-dashboard-global--window)
             auto-create-p)
    (setq mu4e-dashboard-global--window
          (mu4e-dashboard-global--create-window)))
  mu4e-dashboard-global--window)

(defun mu4e-dashboard-default-display-fn (buffer _alist)
  "Display BUFFER to the left or right of the root window.
The side is decided according to `mu4e-dashboard-window-position'.
The root window is the root window of the selected frame.
_ALIST is ignored."
  (let ((window-pos (if (eq mu4e-dashboard-window-position 'left) 'left 'right)))
    (display-buffer-in-side-window buffer `((side . ,window-pos)))))

(defun mu4e-dashboard-global--create-window ()
  "Create global mu4e-dashboard window."
  (let ((window nil)
        (buffer (mu4e-dashboard-global--get-buffer t)))
    (setq window
          (select-window
           (display-buffer buffer mu4e-dashboard-display-action)))
    (mu4e-dashboard-window--init window buffer)
    (mu4e-dashboard-global--attach)
    (mu4e-dashboard-global--reset-width)
    window))

(defun mu4e-dashboard-global--get-buffer (&optional init-p)
  "Return the global mu4e-dashboard buffer if it exists.
If INIT-P is non-nil and global mu4e-dashboard buffer not exists, then create it."
  (unless (equal (buffer-name mu4e-dashboard-global--buffer)
                 mu4e-dashboard-buffer-name)
    (setf mu4e-dashboard-global--buffer nil))
  (when (and init-p
             (null mu4e-dashboard-global--buffer))
    (save-window-excursion
      (setq mu4e-dashboard-global--buffer
            (mu4e-dashboard-buffer--create))))
  mu4e-dashboard-global--buffer)

(defun mu4e-dashboard-global--alone-p ()
  "Check whether the global mu4e-dashboard window is alone with some other window."
  (let ((windows (window-list)))
    (and (= (length windows)
            2)
         (member mu4e-dashboard-global--window windows))))

(defun mu4e-dashboard-global--open ()
  "Show the mu4e-dashboard window."
  (mu4e-dashboard-global--get-window t))

(provide 'mu4e-dashboard)

;;; mu4e-dashboard.el ends here
