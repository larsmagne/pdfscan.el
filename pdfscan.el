;;; pdfscan.el --- Scanning and cropping
;; Copyright (C) 2013 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: scan

;; This file is not part of GNU Emacs.

;; Pdfscan is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Commentary:

;;; Code:

(require 'cl)

(defvar pdfscan-directory "~/pdfscan/"
  "The directory where all the scans will be put.")

(defun pdfscan (name)
  "Scan several pages into NAME."
  (interactive "sName: ")
  (unless (file-exists-p pdfscan-directory)
    (make-directory pdfscan-directory t))
  (let ((default-directory pdfscan-directory)
	(format "%s.%02d.tiff"))
    (when (file-exists-p (format format name 1))
      (error "%s already exists" name))
    (let ((ended nil)
	  (part 1))
      (while (not ended)
	(pdfscan-image (format format name part))
	(incf part)
	(setq ended (y-or-n-p "Finished document? ")))
      (pdfscan-make-pdf name))))

(defun pdfscan-image (file-name)
  (message "Scanning %s..." file-name)
  (call-process "scanimage" nil
		`((:file ,file-name) nil)
		nil
		"--mode=color"
		"-d" "epson"
		"--format=tiff"
		"--resolution" "300dpi")
  (message "Scanning %s...done" file-name)
  (start-process "*gimp*" nil "gimp"
		 (expand-file-name file-name pdfscan-directory)))

(defun pdfscan-make-pdf (name)
  (let ((images (directory-files
		 "." nil
		 (format "%s.[0-9]+.tiff" (regexp-quote name))))
	jpegs)
    (dolist (image images)
      (let ((jpeg (replace-regexp-in-string ".tiff$" ".jpg")))
	(call-process "convert" nil nil nil image jpeg)
	(push jpeg jpegs)))
    (setq jpegs (nreverse jpegs))
    (apply 'call-process "convert" nil (get-buffer-create "*pdf*") nil
	   (append jpegs
		   (list (format "%s.pdf" name))))))

(provide 'pdfscan)

;;; pdfscan.el ends here
