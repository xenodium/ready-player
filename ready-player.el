;;; ready-player.el --- Open media files in ready-player-mode major mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/ready-player
;; Version: 0.0.1

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
;; `ready-player-mode' is a lightweight media (audio/video) major mode for Emacs.
;;
;; Setup:
;;
;;   (require 'ready-player)
;;   (ready-player-add-to-auto-mode-alist)
;;
;; To customize supported media files, set `ready-player-supported-media'
;; before invoking `ready-player-add-to-auto-mode-alist'.
;;
;; `ready-player-mode' relies on command line utilities to play media.
;;  Customize `ready-player-open-playback-commands' to your preference.
;;
;; Note: This is a freshly made package.  Please report issues or send
;; patches to https://github.com/xenodium/ready-player

(require 'seq)

;;; Code:

(defvar ready-player-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "SPC") #'ready-player-toggle-play-stop)
    (define-key map (kbd "TAB") #'ready-player-next-button)
    (define-key map (kbd "<backtab>") #'ready-player-previous-button)
    (define-key map (kbd "n") #'ready-player-next-file)
    (define-key map (kbd "p") #'ready-player-previous-file)
    (define-key map (kbd "e") #'ready-player-open-externally)
    (define-key map (kbd "o") #'ready-player-open-externally)
    (define-key map (kbd "q") #'ready-player-quit)
    map)
  "Keymap for `ready-player'.")

(defcustom ready-player-show-thumbnail t
  "Whether or not to attempt to display a thumbnail."
  :type 'boolean
  :group 'ready-player)

(defcustom ready-player-play-icon
  (if (string-equal system-type "darwin")
      "􀊄"
    "⏵")
  "Play icon string, for example: \"⏵\"."
  :type 'string
  :group 'ready-player)

(defcustom ready-player-open-externally-icon
  (if (string-equal system-type "darwin")
      "􀉐"
    "➦")
  "Open externally icon string, for example: \"➦\"."
  :type 'string
  :group 'ready-player)

(defcustom ready-player-open-playback-commands
  '(("mpv" "--audio-display=no")
    ("vlc")
    ("ffplay")
    ("mplayer"))
  "Command line utilities to try for playback.

Note each entry is a list, in case additional flags are needed.

Omit the file path, as it will be automatically appended."
  :type '(repeat (list string))
  :group 'ready-player)

(defcustom ready-player-supported-media
  '("3g2" "3gp" "aac" "ac3" "aiff" "amr" "ape" "asf" "asx" "avi"
    "cue" "divx" "drc" "dts" "dvb" "evo" "f4a" "f4b" "f4p" "f4v"
    "flac" "flv" "gif" "gsm" "h264" "h265" "hevc" "isma" "ismv"
    "jspf" "m2ts" "m2v" "m3u" "m3u8" "m4a" "midi" "mjpeg" "mlp"
    "mka" "mkv" "mlp" "mov" "mp2" "mp3" "mp4" "mpg" "mpeg" "mts"
    "mxf" "oga" "ogg" "ogm" "ogv" "opus" "pls" "pva" "qt" "ra"
    "ram" "raw" "rf64" "rm" "rmvb" "sami" "spx" "tta" "vob" "wav"
    "wavpack" "webm" "wma" "wmv" "wpl" "wv" "xspf")
  "Supported media types."
  :group 'play-mode
  :type '(repeat string))

(defcustom ready-player-stop-icon
  (if (string-equal system-type "darwin")
      "􀛷"
    "■")
  "Stop icon string, for example: \"■\"."
  :type 'string
  :group 'ready-player)

(defvar-local ready-player--process nil "Media-playing process.")

(defvar-local ready-player--metadata nil "File metadata as per ffprobe.")

(defvar-local ready-player--file-thumbnail nil "Thumbnail as per ffmpeg.")

(defun ready-player-add-to-auto-mode-alist ()
  "Add popular media supported by mpv."
  (dolist (ext ready-player-supported-media)
    (add-to-list 'auto-mode-alist (cons (concat "\\." ext "\\'") 'ready-player-mode))))

(define-derived-mode ready-player-mode special-mode "Ready Player"
  "Major mode to preview and play media files."
  :keymap ready-player-mode-map
  :after-hook (progn
                (goto-char (point-min))
                (search-forward ready-player-play-icon)
                (backward-char))
  (setq buffer-read-only t)
  (setq buffer-undo-list t)
  (let* ((fpath (buffer-file-name))
         (fname (file-name-nondirectory fpath))
         (attributes (file-attributes fpath))
         (buffer-read-only nil)
         (metadata-rows nil))
    (erase-buffer)
    (setq ready-player--metadata (ready-player--file-metadata fpath))
    (when ready-player-show-thumbnail
      (setq ready-player--file-thumbnail (ready-player--file-thumbnail fpath))
      (when ready-player--file-thumbnail
        (insert "\n ")
        (insert-image (create-image ready-player--file-thumbnail nil nil :max-width 400))
        (insert "\n")))
    (insert "\n")
    (insert (ready-player--make-file-button-line fname nil))
    (insert "\n")
    (insert "\n")
    (let-alist ready-player--metadata
      (when .format.tags.title
        (setq metadata-rows
              (append metadata-rows
                      (list
                       (list (cons 'label "Title:")
                             (cons 'value .format.tags.title))))))
      (when .format.tags.artist
        (setq metadata-rows
              (append metadata-rows
                      (list
                       (list (cons 'label "Artist:")
                             (cons 'value .format.tags.artist))))))
      (when .format.tags.album
        (setq metadata-rows
              (append metadata-rows
                      (list
                       (list (cons 'label "Album:")
                             (cons 'value .format.tags.album))))))
      (when .format.format_long_name
        (setq metadata-rows
              (append metadata-rows
                      (list
                       (list (cons 'label "Format:")
                             (cons 'value .format.format_long_name))))))
      (when .format.duration
        (setq metadata-rows
              (append metadata-rows
                      (list
                       (list (cons 'label "Duration:")
                             (cons 'value (ready-player--format-duration .format.duration))))))))
    (setq metadata-rows
          (append metadata-rows
                  (list
                   (list (cons 'label "File size:")
                         (cons 'value (ready-player--readable-size (nth 7 attributes))))) ))
    (insert (ready-player--format-metadata-rows metadata-rows))
    (add-hook 'kill-buffer-hook #'ready-player--clean-up nil t)
    (set-buffer-modified-p nil)))

(defun ready-player-next-button ()
  "Navigate to next button."
  (interactive)
  (or (progn
        (when (equal (symbol-name (symbol-at-point)) ready-player-play-icon)
          (forward-char))
        (when (search-forward ready-player-play-icon nil t)
          (forward-char -1)
          t))
      (progn
        (when (equal (symbol-name (symbol-at-point))
                     ready-player-open-externally-icon)
          (forward-char))
        (when (search-forward ready-player-open-externally-icon nil t)
          (forward-char -1)
          t))
      (progn
        (goto-char (point-min))
        (ready-player-next-button))))

(defun ready-player-previous-button ()
  "Navigate to previous button."
  (interactive)
  (or (progn
        (when (equal (symbol-name (symbol-at-point))
                     ready-player-open-externally-icon)
          (forward-char -1))
        (search-backward ready-player-open-externally-icon nil t))
      (progn
        (when (equal (symbol-name (symbol-at-point)) ready-player-play-icon)
          (forward-char -1))
        (search-backward ready-player-play-icon nil t))
      (progn
        (goto-char (point-max))
        (ready-player-previous-button))))

(defun ready-player-quit ()
  "Quit `ready-player-mode' window and kill buffer."
  (interactive)
  (quit-window t))

;; Based on `crux-open-with'.
(defun ready-player-open-externally (arg)
  "Open visited file in default external program.
When in Dired mode, open file under the cursor.

With a prefix ARG always prompt for command to use."
  (interactive "P")
  (ready-player-toggle-play-stop)
  (let* ((current-file-name
          (if (derived-mode-p 'dired-mode)
              (dired-get-file-for-visit)
            buffer-file-name))
         (open (pcase system-type
                 (`darwin "open")
                 ((or `gnu `gnu/linux `gnu/kfreebsd) "xdg-open")))
         (program (if (or arg (not open))
                      (read-shell-command "Open current file with: ")
                    open)))
    (call-process program nil 0 nil current-file-name)))

;; Piggybacks off `image-next-file'.
(defun ready-player-next-file (&optional n)
  "Visit the next media file in the same directory as current file.
With optional argument N, visit the Nth image file after the
current one, in cyclic alphabetical order.

This command visits the specified file via `find-alternate-file',
replacing the current Image mode buffer."
  (interactive "p" ready-player)
  (let ((major-mode 'image-mode) ;; pretend to be image-mode.
        (image-file-name-extensions ready-player-supported-media))
    (when (> n 0)
      (message "Next")
      (run-with-timer 0.8 nil
                      (lambda ()
                        (message ""))))
    (image-next-file n)))

(defun ready-player-previous-file (&optional n)
  "Visit the preceding image in the same directory as the current file.
With optional argument N, visit the Nth image file preceding the
current one, in cyclic alphabetical order.

This command visits the specified file via `find-alternate-file',
replacing the current Image mode buffer."
  (interactive "p" ready-player)
  (message "Previous")
  (run-with-timer 0.8 nil
                  (lambda ()
                    (message "")))
  (ready-player-next-file (- n)))

(defun ready-player-stop ()
  "Stop media playback."
  (interactive)
  (when-let ((fpath (buffer-file-name))
             (process ready-player--process))
    (delete-process process)
    (setq ready-player--process nil)
    (ready-player--refresh-status (file-name-nondirectory fpath) nil)
    (kill-buffer (ready-player--playback-buffer))))

(defun ready-player-play ()
  "Start media playback."
  (interactive)
  (ready-player-stop)
  (when-let ((fpath (buffer-file-name)))
    (setq ready-player--process (apply 'start-process
                                       (append
                                        (list "*play mode*" (ready-player--playback-buffer))
                                        (ready-player--playback-command) (list fpath))))
    (ready-player--refresh-status (file-name-nondirectory fpath) t)
    (set-process-sentinel
     ready-player--process
     (lambda (process _)
       (when (memq (process-status process) '(exit signal))
         (setq ready-player--process nil)
         (ready-player--refresh-status (file-name-nondirectory fpath) nil))))
    (set-process-filter ready-player--process #'comint-output-filter)))

(defun ready-player-toggle-play-stop ()
  "Toggle play/stop of media."
  (interactive)
  (if-let ((fpath (buffer-file-name)))
      (if ready-player--process
          (ready-player-stop)
        (ready-player-play))
    (error "No file to play/stop")))

(defun ready-player--playback-command ()
  "Craft a playback command from the first utility found on system."
  (if-let ((command (seq-find (lambda (command)
                                (when (seq-first command)
                                  (executable-find (seq-first command))))
                              ready-player-open-playback-commands)))
      command
    (user-error "No player found: %s"
                (mapconcat
                 'identity (seq-map #'seq-first ready-player-open-playback-commands) " "))))

(defun ready-player--make-file-button-line (fname busy)
  "Create button line with FNAME and BUSY."
  (format " %s %s"
          (propertize
           (format " %s %s "
                   (if busy
                       ready-player-stop-icon
                     ready-player-play-icon)
                   fname)
           'face '(:box t)
           'pointer 'hand
           'keymap (let ((map (make-sparse-keymap)))
                     (define-key map [mouse-1] #'ready-player-toggle-play-stop)
                     (define-key map (kbd "RET") #'ready-player-toggle-play-stop)
                     (define-key map [remap self-insert-command] 'ignore)
                     map))
          (propertize (format " %s " ready-player-open-externally-icon)
                      'face '(:box t)
                      'pointer 'hand
                      'keymap (let ((map (make-sparse-keymap)))
                                (define-key map [mouse-1] #'ready-player-open-externally)
                                (define-key map (kbd "RET") #'ready-player-open-externally)
                                (define-key map [remap self-insert-command] 'ignore)
                                map))))

(defun ready-player--refresh-status (fname busy)
  "Refresh and render status in buffer with FNAME and BUSY."
  (let ((inhibit-read-only t)
        (start (line-beginning-position))
        (end (line-end-position))
        (saved-point (point)))
    (save-excursion
      (goto-char (point-min))
      (when (search-forward (if busy
                                ready-player-play-icon
                              ready-player-stop-icon) nil t)
        (delete-region start end)
        (insert (ready-player--make-file-button-line fname busy))))
    (goto-char saved-point)
    (set-buffer-modified-p nil)
    (if busy
        (progn
          (message "Playing...")
          (run-with-timer 0.8 nil
                          (lambda ()
                            (message ""))))
      (message ""))))

(defun ready-player--file-metadata (fpath)
  "Get media metadata at FPATH."
  (if (executable-find "ffprobe")
      (with-temp-buffer
        (let* ((command (list "ffprobe" nil t nil "-v" "quiet" "-print_format" "json" "-show_format" "-show_streams" fpath))
               (exit-code (apply 'call-process command)))
          (if (zerop exit-code)
              (progn
                (goto-char (point-min))
                (json-parse-buffer :object-type 'alist))
            (message "ffprobe couldn't fetch metadata")
            nil)))
    (message "Metadata not available (ffprob not found)")
    nil))

(defun ready-player--file-thumbnail (media-fpath)
  "Get media thumbnail at MEDIA-FPATH."
  (when (executable-find "ffmpeg")
    (or (with-temp-buffer ;; mp3
          (let* ((thumbnail-fpath (concat (make-temp-file "ready-player-") ".png"))
                 (command (list "ffmpeg" nil t nil "-i" media-fpath "-an" "-vcodec" "copy" thumbnail-fpath))
                 (exit-code (apply 'call-process command)))
            (when (zerop exit-code)
              thumbnail-fpath)))
        (with-temp-buffer ;; video
          (let* ((thumbnail-fpath (concat (make-temp-file "ready-player-") ".png"))
                 (command (list "ffmpeg" nil t nil "-i" media-fpath "-ss" "00:00:01.000" "-vframes" "1" thumbnail-fpath))
                 (exit-code (apply 'call-process command)))
            (when (zerop exit-code)
              thumbnail-fpath))))))

(defun ready-player--playback-buffer ()
  "Get the process playback buffer."
  (let ((buffer (get-buffer-create "*play*")))
    (with-current-buffer buffer
      (erase-buffer)
      (let ((inhibit-message t))
        ;; Silence noise of entering shell-mode.
        (shell-mode)))
    buffer))

(defun ready-player--format-metadata-rows (rows)
  "Format metadata ROWS for rendering."
  (let ((max-label-length (+ 1 (apply #'max (mapcar (lambda (row) (length (cdr (assoc 'label row)))) rows)))))
    (mapconcat (lambda (row)
                 (let ((label (cdr (assoc 'label row)))
                       (value (cdr (assoc 'value row))))
                   (format " %s%s %s\n\n"
                           (propertize label 'face 'font-lock-comment-face)
                           (make-string (- max-label-length (length label)) ?\s)
                           value)))
               rows)))

(defun ready-player--format-duration (duration)
  "Format DURATION in a human-readable format."
  (setq duration (string-to-number duration))
  (let* ((hours   (/ duration 3600))
         (minutes (/ (mod duration 3600) 60))
         (seconds (mod duration 60)))
    (format "%d:%02d:%02d" hours minutes seconds)))

(defun ready-player--readable-size (size)
  "Format SIZE in a human-readable format."
  (cond
   ((> size (* 1024 1024 1024))
    (format "%.2f GB" (/ (float size) (* 1024 1024 1024))))
   ((> size (* 1024 1024))
    (format "%.2f MB" (/ (float size) (* 1024 1024))))
   ((> size 1024)
    (format "%.2f KB" (/ (float size) 1024)))
   (t
    (format "%d bytes" size))))

(defun ready-player--clean-up ()
  "Kill playback process."
  (when ready-player--file-thumbnail
    (condition-case nil
        (delete-file ready-player--file-thumbnail)
      (file-error nil)))
  (when ready-player--process
    (delete-process ready-player--process)
    (setq ready-player--process nil)
    (ready-player--refresh-status (file-name-nondirectory (buffer-file-name)) nil)))

(provide 'ready-player)

;;; ready-player.el ends here
