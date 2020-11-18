;;; mu4e-dashboard.el --- Mu4e dashboard using mu4e org links -*- lexical-binding: t -*-

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

(require 'subr-x)
(require 'aio)

;; Install the mu4e link type
(org-add-link-type "mu4e" 'mu4e-dashboard-follow-mu4e-link)
 
  
(defun mu4e-dashboard-follow-mu4e-link (path)
  "Process a mu4e link"
  (let* ((link    (org-element-context))
         (query   (string-trim (nth 0 (split-string path "|"))))
         (fmt     (nth 1 (split-string path "|")))
         (count   (nth 2 (split-string path "|"))))
    (cond
     ;; Regular query without limit
     ((and (not fmt) (not count))
      (mu4e-headers-search query))

     ;; Regular query with limit
     ((and count (> count 0))
      (let ((mu4e-headers-results-limit (string-to-number count)))
        (mu4e-headers-search query)))

     ;; Query count and link description update
     ((and fmt (> (length fmt) 0))
       (mu4e-dashboard-update-link link)))))

(defun mu4e-dashboard-update-link (link)
  "Update content of a formatted mu4e links"
  
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
          (save-excursion
            (delete-region beg end)
            (goto-char beg)
            (insert (if (<= (length output) size) output
                      (make-string size ?+))))))))


(defun async-shell-command-to-string (command callback)
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


(defun mu4e-dashboard-upate-all-async ()
  "Update content of all formatted mu4e links in an asynchronous way"
  (interactive)
  (mu4e-dashboard-clear-all-links)
  (let ((buffer (current-buffer)))
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (when (string= (org-element-property :type link) "mu4e")
          (let* ((path  (org-element-property :path link))
                 (query (string-trim (nth 0 (split-string path "|"))))
                 (fmt   (nth 1 (split-string path "|")))
                 (beg   (org-element-property :contents-begin link))
                 (end   (org-element-property :contents-end link))
                 (size  (- end beg)))
            (if (and fmt (> (length fmt) 0))
                (let ((command (format "mu find %s 2> /dev/null | wc -l" query)))
                  (async-shell-command-to-string command
                      (lambda (output)
                        (with-current-buffer buffer
                          (save-excursion
                            (delete-region beg end)
                            (goto-char beg)
                            (insert (format fmt (string-to-number output)))))))))))))))

(defun mu4e-dashboard-upate-all-sync ()
  "Update content of all formatted mu4e links in a synchronous way"
  (interactive)
  (mu4e-dashboard-clear-all-links)
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (string= (org-element-property :type link) "mu4e")
        (mu4e-dashboard-update-link link)
        (redisplay t)))))



(defun mu4e-dashboard-clear-link (link)
  "Remove content of a formatted mu4e links"
  
  (let* ((path (org-element-property :path link))
         (fmt  (nth 1 (split-string path "|")))
         (beg  (org-element-property :contents-begin link))
         (end  (org-element-property :contents-end link))
         (size (- end beg)))
    (if (> (length fmt) 0)
        (save-excursion
          (delete-region beg end)
          (goto-char beg)
          (insert (format "(%s)" (make-string (- size 2) ?-)))))))

(defun mu4e-dashboard-clear-all ()
  "Remove content of all formatted mu4e links"
  
  (interactive)
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (string= (org-element-property :type link) "mu4e")
        (mu4e-dashboard-clear-link link)))))
        

