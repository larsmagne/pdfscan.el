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
  (let ((default-directory pdfscan-directory))
    (let ((ended nil)
	  (part 1))
      (while (not ended)
	(pdfscan-image (format "%s.%d.tiff" name part))
	(incf part)
	(setq ended (y-or-n-p "Finished document? ")))
      (pdfscan-make-pdf name))))

(defun pdfscan-image (file-name)
  (call-process "scanimage" nil
		`((:file ,file-name) nil)
		nil
		"--mode=color"
		"-d" "epson"
		"--format=tiff"
		"--resolution" "300dpi")
  (start-process "*gimp*" nil "gimp" file-name))

(defun pdfscan-make-pdf (name)
  (let ((images (directory-files
		 "." nil
		 (format "%s.[0-9]+.tiff" (regexp-quote name)))))
    (call-process "convert" nil nil nil
		  (append images
			  (list (format "%s.pdf" name))))))

(provide 'pdfscan)

;;; pdfscan.el ends here
