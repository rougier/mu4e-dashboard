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
     (count
      (let ((mu4e-headers-results-limit (string-to-number count)))
        (mu4e-headers-search query)))

     ;; Query count and link description update
     (fmt (mu4e-dashboard-update-link link)))))
      

(defun mu4e-dashboard-update-link (link)
  "Update content of a formatted mu4e links"
  
  (let* ((path  (org-element-property :path link))
         (query (string-trim (nth 0 (split-string path "|"))))
         (fmt   (nth 1 (split-string path "|")))
         (count (nth 2 (split-string path "|")))
         (beg   (org-element-property :contents-begin link))
         (end   (org-element-property :contents-end link))
         (size  (- end beg)))
    (if fmt
        (let* ((command (format "mu find %s 2> /dev/null | wc -l" query))
               (output (string-to-number (shell-command-to-string command)))
               (output  (format fmt output)))
          (save-excursion
            (delete-region beg end)
            (goto-char beg)
            (insert (if (<= (length output) size) output
                      (make-string size ?+))))))))

(defun mu4e-dashboard-upate-all-links ()
  "Update content of all formatted mu4e links"
  
  (interactive)
  (mu4e-dashboard-clear-all-links)
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (string= (org-element-property :type link) "mu4e")
        (mu4e-dashboard-update-link link)))))


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

(defun mu4e-dashboard-clear-all-links ()
  "Remove content of all formatted mu4e links"
  
  (interactive)
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (string= (org-element-property :type link) "mu4e")
        (mu4e-dashboard-clear-link link)))))
        

