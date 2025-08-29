;;; ready-player-dired.el --- Show media info in dired -*- lexical-binding: t -*-

;; Copyright (C) 2025 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/ready-player

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Support this work https://github.com/sponsors/xenodium
;;
;; `ready-player-dired-mode' adds media metadata to `dired buffers'.
;;
;; Setup:
;;
;;   (require 'ready-player-dired)
;;   (ready-player-dired-mode +1)
;;

;;; Code:

(require 'dired)
(require 'ready-player)

(defvar-local ready-player-dired--overlays nil
  "Overlays storing media metadata info.")

;;;###autoload
(define-minor-mode ready-player-dired-mode
  "Toggle display of media info as overlays in `dired.'."
  :lighter " dmi"
  (unless (derived-mode-p 'dired-mode)
    (user-error "Not in a `dired' buffer (%s)" major-mode))
  (unless (executable-find "ffprobe")
    (user-error "Ffmpeg not available (please install)"))
  (if ready-player-dired-mode
      (progn
        (ready-player-dired--add-overlays)
        (add-hook 'dired-after-readin-hook #'ready-player-dired--add-overlays nil t))
    (remove-hook 'dired-after-readin-hook #'ready-player-dired--add-overlays t)
    (ready-player-dired--remove-overlays)))

(defun ready-player-dired--remove-overlays ()
  "Delete all ready-player-dired overlays."
  (unless (derived-mode-p 'dired-mode)
    (user-error "Not in a `dired' buffer (%s)" major-mode))
  (mapc #'delete-overlay ready-player-dired--overlays)
  (setq ready-player-dired--overlays nil))

(defun ready-player-dired--add-overlays ()
  "Asynchronously overlay aligned metadata with track title first, hiding file names."
  (unless (derived-mode-p 'dired-mode)
    (user-error "Not in a `dired' buffer (%s)" major-mode))
  (ready-player-dired--remove-overlays)
  (when-let ((files (ready-player-dired--media-files))
             (dired-buffer (current-buffer)))
    (ready-player-dired--load-metadata
     (nreverse files)
     (lambda (entries)
       (with-current-buffer dired-buffer
         (let* ((inhibit-read-only t)
                (title-width  (apply #'max (mapcar (lambda (e)
                                                     (length (or (map-elt e :title) "")))
                                                   entries)))
                (artist-width (apply #'max (mapcar (lambda (e)
                                                     (length (or (map-elt e :artist) "")))
                                                   entries)))
                (album-width  (apply #'max (mapcar (lambda (e)
                                                     (length (or (map-elt e :album) "")))
                                                   entries))))
           (save-excursion
             (dolist (entry entries)
               (when (ignore-errors (ready-player-dired--dired-goto-file (map-elt entry :file)))
                 (let* ((bol (line-beginning-position))
                        (eol (line-end-position))
                        (name-start (progn
                                      (goto-char bol)
                                      (re-search-forward dired-move-to-filename-regexp eol t)
                                      (point)))
                        (overlay (make-overlay name-start eol))
                        (padded-title  (propertize
                                        (format (format "%%-%ds" title-width)
                                                (or (and (not (string-empty-p (map-elt entry :title)))
                                                         (map-elt entry :title))
                                                    (file-name-sans-extension
                                                     (file-name-nondirectory (map-elt entry :file)))
                                                    ""))
                                        'face 'default))
                        (padded-artist (propertize
                                        (format (format "%%-%ds" artist-width)
                                                (or (map-elt entry :artist) ""))
                                        'face 'font-lock-string-face))
                        (padded-album  (propertize
                                        (format (format "%%-%ds" album-width)
                                                (or (map-elt entry :album) ""))
                                        'face 'font-lock-variable-name-face))
                        (msg (concat padded-title "    "
                                     padded-artist "    "
                                     padded-album)))
                   (overlay-put overlay 'invisible t)
                   (overlay-put overlay 'after-string (concat "  " msg))
                   (overlay-put overlay 'priority -60)
                   (push overlay ready-player-dired--overlays)))))))))))

(defun ready-player-dired--load-metadata (files on-finished)
  "Extract media metadata from FILES async and call ON-FINISHED."
  (unless (derived-mode-p 'dired-mode)
    (user-error "Not in a `dired' buffer (%s)" major-mode))
  (let* ((progress (make-progress-reporter "Loading media info"))
         (delimiter "[end-of-metadata]")
         (script (mapconcat
                  (lambda (f)
                    (format "ffprobe -v quiet -print_format json -show_entries format_tags=artist,title,album %s 2>/dev/null; echo '%s'\n"
                            (shell-quote-argument f) delimiter))
                  files))
         (output-buffer (generate-new-buffer "*dmi-ffprobe-output*")))
    (make-process
     :name "dmi-ffprobe"
     :buffer output-buffer
     :command `(,shell-file-name "-c" ,script)
     :noquery t
     :filter
     (lambda (_proc output)
       (with-current-buffer output-buffer
         (goto-char (point-max))
         (insert output))
       (progress-reporter-update progress))
     :sentinel
     (lambda (_proc _event)
       (with-current-buffer output-buffer
         (when-let* ((alive (buffer-live-p output-buffer))
                     (output (buffer-string))
                     (chunks (split-string output (regexp-quote delimiter) t "[ \t\n\r]+"))
                     (metadata
                      (mapcar
                       (lambda (i)
                         (let* ((json (nth i chunks))
                                (file (nth i files))
                                (data (ignore-errors (json-parse-string json :object-type 'alist)))
                                (format (map-elt data 'format))
                                (tags (map-elt format 'tags)))
                           `((:file . ,file)
                             (:artist . ,(map-elt tags 'artist))
                             (:title .  ,(map-elt tags 'title))
                             (:album .  ,(map-elt tags 'album)))))
                       (number-sequence 0 (1- (length chunks))))))
           (kill-buffer output-buffer)
           (progress-reporter-done progress)
           (funcall on-finished metadata)))))))

(defun ready-player-dired--media-files ()
  "Get media files in `dired' buffer."
  (unless (derived-mode-p 'dired-mode)
    (user-error "Not in a `dired' buffer (%s)" major-mode))
  (let ((files))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((file (dired-get-filename nil t)))
          (when (and file (file-regular-p file)
                     (ready-player-is-audio-p file))
            (push file files)))
        (forward-line 1)))
    files))

(defun ready-player-dired--dired-goto-file (file)
  "Like `dired-goto-file' but also works with `dired-subtree'.

Jump to FILE with absolute path in `dired' buffer."
  (unless (derived-mode-p 'dired-mode)
    (user-error "Not in a `dired' buffer (%s)" major-mode))
  (let ((found nil))
    (save-excursion
      (goto-char (point-min))
      (while (and (not found)
                  (not (eobp)))
        (let ((f (dired-get-filename nil t)))
          (when (and f (string= (expand-file-name f) (expand-file-name file)))
            (setq found (point))))
        (forward-line 1)))
    (when found (goto-char found))))

(provide 'ready-player-dired)

;;; ready-player-dired.el ends here
