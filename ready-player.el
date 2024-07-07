;;; ready-player.el --- Open media files in ready-player-mode major mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/ready-player
;; Version: 0.0.32

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

(require 'dired)
(require 'image-mode)
(require 'seq)
(require 'shell)

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
    (define-key map (kbd "g") #'ready-player-toggle-reload-buffer)
    map)
  "Keymap for `ready-player'.")

(defcustom ready-player-show-thumbnail t
  "Whether or not to attempt to display a thumbnail."
  :type 'boolean
  :group 'ready-player)

(defcustom ready-player-repeat t
  "Continue playing if there's more media in current directory."
  :type 'boolean
  :group 'ready-player)

(defcustom ready-player-cache-thumbnails t
  "Whether or not to cache thumbnails."
  :type 'boolean
  :group 'ready-player)

;; TODO: Find a better way of checking for SF rendeing.
(defun ready-player-displays-as-sf-symbol-p (text)
  "Return t if TEXT can be displayed as macoOS SF symbols.  nil otherwise."
  (let ((result t)
        (displayable)
        (char))
    (dotimes (i (length text))
      (setq char (aref text i))
      (setq displayable (char-displayable-p char))
      (when (or (eq displayable t)
                (not (and (fontp displayable)
                          (string-match-p
                           "SF"
                           (font-get (char-displayable-p char) :name)))))
        (setq result nil)))
    result))

(defcustom ready-player--default-button 'play-stop
  "Button to focus on launch."
  :type 'symbol
  :group 'ready-player)

(defcustom ready-player-previous-icon
  (if (ready-player-displays-as-sf-symbol-p "􀊉")
      "􀊉"
    "<<")
  "Previous button icon string, for example: \"<<\"."
  :type 'string
  :group 'ready-player)

(defcustom ready-player-play-icon
  (if (ready-player-displays-as-sf-symbol-p "􀊄")
      "􀊄"
    "⏵")
  "Play button icon string, for example: \"⏵\"."
  :type 'string
  :group 'ready-player)

(defcustom ready-player-next-icon
  (if (ready-player-displays-as-sf-symbol-p "􀊋")
      "􀊋"
    ">>")
  "Next button icon string, for example: \">>\"."
  :type 'string
  :group 'ready-player)

(defcustom ready-player-open-externally-icon
  (if (ready-player-displays-as-sf-symbol-p "􀉐")
      "􀉐"
    "➦")
  "Open externally button icon string, for example: \"➦\"."
  :type 'string
  :group 'ready-player)

(defcustom ready-player-stop-icon
  (if (ready-player-displays-as-sf-symbol-p "􀛷")
      "􀛷"
    "■")
  "Stop icon string, for example: \"■\"."
  :type 'string
  :group 'ready-player)

(defcustom ready-player-repeat-icon
  (if (ready-player-displays-as-sf-symbol-p "􀊞")
      "􀊞"
    "⇆")
  "Repeat icon string, for example: \"⇆\"."
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

(defvar ready-player--process nil "Media-playing process.")

(defvar-local ready-player--metadata nil "Metadata as per ffprobe.")

(defvar-local ready-player--thumbnail nil "Thumbnail as per ffmpeg.")

;;;###autoload
(defun ready-player-add-to-auto-mode-alist ()
  "Add media recognized by `ready-player-mode'."
  (interactive)
  (add-to-list 'auto-mode-alist
               (cons (concat "\\." (regexp-opt ready-player-supported-media t) "\\'")
                     'ready-player-mode))
  ;; Suppress unnecessary buffer loading via file-name-handler-alist.
  (add-to-list
   'file-name-handler-alist
   (cons
    (concat "\\." (regexp-opt (append ready-player-supported-media
                                     ;; Also include uppercase extensions.
                                     ;; APFS (Apple File System) is case-insensitive.
                                     (mapcar 'upcase ready-player-supported-media))
                             t) "\\'")
    'ready-player-file-name-handler)))

(defun ready-player-remove-from-auto-mode-alist ()
  "Remove media recognized by `ready-player-mode'."
  (interactive)
  (setq auto-mode-alist
        (seq-remove (lambda (entry)
                      (and (symbolp (cdr entry))
                           (string-match "ready-player-mode" (symbol-name (cdr entry)))))
                    auto-mode-alist))
  (setq file-name-handler-alist
        (seq-remove (lambda (entry)
                      (equal 'ready-player-file-name-handler (cdr entry)))
                    file-name-handler-alist)))

(defun ready-player-file-name-handler (operation &rest args)
  "Suppress `insert-file-contents' OPERATION with ARGS.

`ready-player-mode' doesn't need to load files into the buffer.

Note: This function needs to be added to `file-name-handler-alist'."
  (pcase operation
    ('insert-file-contents
     (cl-destructuring-bind (filename visit _beg _end _replace) args
       (when visit
         (setq buffer-file-name filename))
       (list buffer-file-name (point-max))))
    ('file-attributes
     (let* ((file-name-handler-alist nil)
	    (attributes (apply 'file-name-non-special
                               (append (list operation) args))))
       ;; 7 is file size location
       ;; as per `file-attributes'.
       (setf (nth 7 attributes) 0)
       attributes))
    (_ (let ((inhibit-file-name-handlers
              (cons 'ready-player-file-name-handler
                    (and (eq inhibit-file-name-operation operation)
                         inhibit-file-name-handlers)))
             (inhibit-file-name-operation operation))
         (apply operation args)))))

(define-derived-mode ready-player-mode special-mode "Ready Player"
  "Major mode to preview and play media files."
  :keymap ready-player-mode-map
  (set-buffer-multibyte t)
  (setq buffer-read-only t)
  (setq buffer-undo-list t)

  ;; Never play more than one process. Stop existing.
  (when ready-player--process
    (delete-process ready-player--process)
    (setq ready-player--process nil))

  (let* ((buffer (current-buffer))
         (fpath (buffer-file-name))
         (cached-thumbnail (ready-player--cached-thumbnail fpath))
         (thumbnailer (if (executable-find "ffmpegthumbnailer")
                          #'ready-player--load-file-thumbnail-via-ffmpegthumbnailer
                        #'ready-player--load-file-thumbnail-via-ffmpeg)))
    (if cached-thumbnail
        (progn
          (setq ready-player--thumbnail cached-thumbnail)
          (ready-player--update-buffer
           buffer fpath
           ready-player--process
           ready-player-repeat
           cached-thumbnail ready-player--metadata))
      (funcall thumbnailer
               fpath (lambda (thumbnail)
                       (when (buffer-live-p buffer)
                         (with-current-buffer buffer
                           (when thumbnail
                             (setq ready-player--thumbnail thumbnail)
                             (ready-player--update-buffer
                              buffer fpath
                              ready-player--process
                              ready-player-repeat
                              thumbnail ready-player--metadata)
                             ;; Point won't move to button
                             ;; unless delayed ¯\_(ツ)_/¯.
                             (run-with-timer 0.1 nil
                                             (lambda ()
                                               (with-current-buffer buffer
                                                 (ready-player--goto-button
                                                  ready-player--default-button))))))))))
    (ready-player--load-file-metadata
     fpath (lambda (metadata)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (when metadata
                   (setq ready-player--metadata metadata)
                   (ready-player--update-buffer
                    buffer fpath
                    ready-player--process
                    ready-player-repeat
                    ready-player--thumbnail metadata)
                   (ready-player--goto-button
                    ready-player--default-button))))))
    (ready-player--update-buffer buffer fpath
                                 ready-player--process
                                 ready-player-repeat)
    (ready-player--goto-button
     ready-player--default-button))
  (add-hook 'kill-buffer-hook #'ready-player--clean-up nil t))

(defun ready-player--update-buffer (buffer fpath busy repeat &optional thumbnail metadata)
  "Update entire BUFFER content with FPATH BUSY REPEAT THUMBNAIL and METADATA."
  (let ((fname (file-name-nondirectory fpath))
        (buffer-read-only nil))
    (with-current-buffer buffer
      (erase-buffer)
      (goto-char (point-min))
      (when (and ready-player-show-thumbnail thumbnail)
        (let ((inhibit-read-only t))
          (when thumbnail
            (insert "\n ")
            (insert-image (create-image thumbnail nil nil :max-width 400))
            (insert "\n"))
          (set-buffer-modified-p nil)))
      (insert "\n")
      (insert (format " %s" (propertize fname 'face 'info-title-2)))
      (insert " ")
      (insert (propertize "(playing)"
                          'face `(:foreground ,(face-foreground 'font-lock-comment-face) :inherit info-title-2)
                          'invisible (not busy)
                          'playing-status t))
      (insert "\n")
      (insert "\n")
      (insert (ready-player--make-file-button-line busy repeat))
      (insert "\n")
      (insert "\n")
      (when metadata
        (insert (ready-player--format-metadata-rows
                 (ready-player--make-metadata-rows metadata))))
      (set-buffer-modified-p nil))))

(defun ready-player--make-metadata-rows (metadata)
  "Make METADATA row data."
  (let ((metadata-rows))
    (let-alist metadata
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
                             (cons 'value (ready-player--format-duration .format.duration)))))))
      (when .format.size
        (setq metadata-rows
              (append metadata-rows
                      (list
                       (list (cons 'label "Size:")
                             (cons 'value (ready-player--readable-size .format.size))))))))
    metadata-rows))

(defun ready-player--goto-button (button)
  "Goto BUTTON."
  (goto-char (point-min))
  (ready-player-search-forward 'button button))

;; TODO: Replace with text-property-search-forward.
(defun ready-player-search-forward (prop val)
  "Search forward for text with property PROP set to VAL."
  (interactive)
  (let ((pos (text-property-any (point) (point-max) prop val)))
    (when pos
      (goto-char pos))))

;; TODO: Simplify like ready-player-previous-button.
(defun ready-player-next-button ()
  "Navigate to next button."
  (interactive)
  (unless (eq major-mode 'ready-player-mode)
    (user-error "Not in a ready-player-mode buffer"))
  (or (progn
        (when (eq (get-text-property (point) 'button) 'previous)
          (ready-player-search-forward 'button nil))
        (when (ready-player-search-forward 'button 'previous)
          t))
      (progn
        (when (eq (get-text-property (point) 'button) 'play-stop)
          (ready-player-search-forward 'button nil))
        (when (ready-player-search-forward 'button 'play-stop)
          t))
      (progn
        (when (eq (get-text-property (point) 'button) 'next)
          (ready-player-search-forward 'button nil))
        (when (ready-player-search-forward 'button 'next)
          t))
      (progn
        (when (eq (get-text-property (point) 'button) 'open-externally)
          (ready-player-search-forward 'button nil))
        (when (ready-player-search-forward 'button 'open-externally)
          t))
      (progn
        (goto-char (point-min))
        (ready-player-next-button))))

(defun ready-player-previous-button ()
  "Navigate to previous button."
  (interactive)
  (unless (eq major-mode 'ready-player-mode)
    (user-error "Not in a ready-player-mode buffer"))
  (or
   (text-property-search-backward 'button)
   (progn
     (goto-char (point-max))
     (ready-player-previous-button))))

(defun ready-player-quit ()
  "Quit `ready-player-mode' window and kill buffer."
  (interactive)
  (unless (eq major-mode 'ready-player-mode)
    (user-error "Not in a ready-player-mode buffer"))
  (quit-window t))

;; Based on `crux-open-with'.
(defun ready-player-open-externally (arg)
  "Open visited file in default external program.
When in Dired mode, open file under the cursor.

With a prefix ARG always prompt for command to use."
  (interactive "P")
  (unless (eq major-mode 'ready-player-mode)
    (user-error "Not in a ready-player-mode buffer"))
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
  (unless (eq major-mode 'ready-player-mode)
    (user-error "Not in a ready-player-mode buffer"))
  (let ((major-mode 'image-mode) ;; pretend to be image-mode.
        (image-file-name-extensions ready-player-supported-media)
        (playing ready-player--process))
    (when (> n 0)
      (message "Next")
      (ready-player--goto-button 'next)
      (run-with-timer 0.8 nil
                      (lambda ()
                        (message ""))))
    (when (get-text-property (point) 'button)
      (setq ready-player--default-button
            (get-text-property (point) 'button)))
    (image-next-file n)
    (when playing
      (ready-player-play))))

(defun ready-player-previous-file (&optional n)
  "Visit the preceding image in the same directory as the current file.
With optional argument N, visit the Nth image file preceding the
current one, in cyclic alphabetical order.

This command visits the specified file via `find-alternate-file',
replacing the current Image mode buffer."
  (interactive "p" ready-player)
  (unless (eq major-mode 'ready-player-mode)
    (user-error "Not in a ready-player-mode buffer"))
  (message "Previous")
  (ready-player--goto-button 'previous)
  (run-with-timer 0.8 nil
                  (lambda ()
                    (message "")))
  (ready-player-next-file (- n)))

(defun ready-player-stop ()
  "Stop media playback."
  (interactive)
  (unless (eq major-mode 'ready-player-mode)
    (user-error "Not in a ready-player-mode buffer"))
  (when-let ((fpath (buffer-file-name))
             (process ready-player--process)
             (buffer (current-buffer)))
    (delete-process process)
    (setq ready-player--process nil)
    (ready-player--refresh-buffer-status
     buffer (file-name-nondirectory fpath)
     ready-player--process
     ready-player-repeat)
    (kill-buffer (ready-player--playback-buffer))))

(defun ready-player-play ()
  "Start media playback."
  (interactive)
  (unless (eq major-mode 'ready-player-mode)
    (user-error "Not in a ready-player-mode buffer"))
  (ready-player-stop)
  (when-let ((fpath (buffer-file-name))
             (buffer (current-buffer)))
    (setq ready-player--process (apply 'start-process
                                       (append
                                        (list "*play mode*" (ready-player--playback-buffer))
                                        (ready-player--playback-command) (list fpath))))
    (set-process-query-on-exit-flag ready-player--process nil)
    (ready-player--refresh-buffer-status
     buffer (file-name-nondirectory fpath)
     ready-player--process
     ready-player-repeat)
    (set-process-sentinel
     ready-player--process
     (lambda (process _)
       (when (memq (process-status process) '(exit signal))
         (with-current-buffer buffer
           (if (and ready-player-repeat
                    (buffer-live-p buffer)
                    (eq (process-exit-status process) 0))
               (progn
                 (call-interactively #'ready-player-next-file)
                 (ready-player-play))
             (setq ready-player--process nil)
             (ready-player--refresh-buffer-status
              buffer (file-name-nondirectory fpath)
              ready-player--process
              ready-player-repeat))))))
    (set-process-filter ready-player--process #'comint-output-filter)))

(defun ready-player-toggle-play-stop ()
  "Toggle play/stop of media."
  (interactive)
  (unless (eq major-mode 'ready-player-mode)
    (user-error "Not in a ready-player-mode buffer"))
  (ready-player--goto-button 'play-stop)
  (if-let ((fpath (buffer-file-name)))
      (if ready-player--process
          (ready-player-stop)
        (ready-player-play))
    (error "No file to play/stop")))

(defun ready-player-toggle-repeat ()
  "Toggle repeat setting."
  (interactive)
  (unless (eq major-mode 'ready-player-mode)
    (user-error "Not in a ready-player-mode buffer"))
  (setq ready-player-repeat (not ready-player-repeat))
  (ready-player--refresh-buffer-status
   (current-buffer)
   (file-name-nondirectory (buffer-file-name))
   ready-player--process
   ready-player-repeat)
  (message "Repeat: %s" (if ready-player-repeat
                            "ON"
                          "OFF"))
  (run-with-timer 1 nil
                  (lambda ()
                    (message ""))))

(defun ready-player-toggle-reload-buffer ()
  "Reload media from file."
  (interactive)
  (unless (eq major-mode 'ready-player-mode)
    (user-error "Not in a ready-player-mode buffer"))
  (let ((playing ready-player--process))
    (ready-player-stop)
    (revert-buffer nil t)
    (when playing
      (ready-player-play)))
  (message "Reloaded")
  (run-with-timer 1 nil
                  (lambda ()
                    (message ""))))

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

(defun ready-player--make-file-button-line (busy repeat)
  "Create button line with BUSY and REPEAT."
  (format " %s %s %s %s %s"
          (ready-player--make-button ready-player-previous-icon
                                     'previous
                                     #'ready-player-previous-file)
          (ready-player--make-button (if busy
                                         ready-player-stop-icon
                                       ready-player-play-icon)
                                     'play-stop
                                     #'ready-player-toggle-play-stop)
          (ready-player--make-button ready-player-next-icon
                                     'next
                                     #'ready-player-next-file)
          (ready-player--make-button ready-player-open-externally-icon
                                     'open-externally
                                     #'ready-player-open-externally)
          (ready-player--make-checkbox-button ready-player-repeat-icon repeat ;; FIXME
                                              'repeat
                                              #'ready-player-toggle-repeat)))

(defun ready-player--make-checkbox-button (text checked kind action)
  "Make a checkbox button with TEXT, CHECKED state, KIND, and ACTION."
  (propertize
   (format "%s%s"
           text
           (if checked
               "*"
             " "))
   'pointer 'hand
   'keymap (let ((map (make-sparse-keymap)))
             (define-key map [mouse-1] action)
             (define-key map (kbd "RET") action)
             (define-key map [remap self-insert-command] 'ignore)
             map)
   'button kind))

(defun ready-player--make-button (text kind action)
  "Make button with TEXT, KIND, and ACTION."
  (propertize
   (format " %s " text)
   'face '(:box t)
   'pointer 'hand
   'keymap (let ((map (make-sparse-keymap)))
             (define-key map [mouse-1] action)
             (define-key map (kbd "RET") action)
             (define-key map [remap self-insert-command] 'ignore)
             map)
   'button kind))

(defun ready-player--refresh-buffer-status (buffer fname busy repeat)
  "Refresh and render status in buffer with BUFFER, FNAME, BUSY and REPEAT."
  (when-let ((inhibit-read-only t)
             (saved-point (point))
             (live-buffer (buffer-live-p buffer)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))

        ;; Toggle (playing) next to file name.
        (when-let* ((match (text-property-search-forward 'playing-status))
                    (start (prop-match-beginning match))
                    (end (prop-match-end match)))
          (if busy
              (remove-text-properties start end '(invisible t))
            (add-text-properties start end '(invisible t))))

        (goto-char (point-min))

        (when (text-property-search-forward 'button)
          (delete-region (line-beginning-position) (line-end-position))
          (insert (ready-player--make-file-button-line busy repeat))))
      (goto-char saved-point)

      ;; Toggle (playing) in buffer name.
      (let ((base-name (replace-regexp-in-string " (playing)$" "" fname)))
        (rename-buffer (if busy
                           (concat base-name " (playing)")
                         base-name)))

      (set-buffer-modified-p nil)
      (if busy
          (progn
            (message "Playing...")
            (run-with-timer 0.8 nil
                            (lambda ()
                              (message ""))))
        (message "")))))

(defun ready-player--make-shasum (fpath)
  "Make shasum for FPATH."
  (with-temp-buffer
    (call-process "shasum" nil t nil "-a" "256" fpath)
    (goto-char (point-min))
    (car (split-string (buffer-string)))))

(defun ready-player--thumbnail-path (fpath)
  "Generate thumbnail path for media at FPATH."
  (let* ((temp-dir (concat (file-name-as-directory temporary-file-directory) "ready-player"))
         (temp-fpath (concat (file-name-as-directory temp-dir)
                             (md5 fpath) ".png")))
    (make-directory temp-dir t)
    temp-fpath))

(defun ready-player--load-file-thumbnail-via-ffmpegthumbnailer (media-fpath on-loaded)
  "Load media thumbnail at MEDIA-FPATH and invoke ON-LOADED.

Note: This needs the ffmpegthumbnailer command line utility."
  (if (executable-find "ffmpegthumbnailer")
      (let* ((thumbnail-fpath (ready-player--thumbnail-path media-fpath)))
        (make-process
         :name "ffmpegthumbnailer-process"
         :buffer (get-buffer-create "*ffmpegthumbnailer-output*")
         :command (list "ffmpegthumbnailer" "-i" media-fpath "-s" "0" "-m" "-o" thumbnail-fpath)
         :sentinel
         (lambda (process _)
           (if (eq (process-exit-status process) 0)
               (funcall on-loaded thumbnail-fpath)
             (condition-case nil
                 (delete-file thumbnail-fpath)
               (file-error nil))))))
    (message "Metadata not available (ffmpegthumbnailer not found)")))

(defun ready-player--cached-thumbnail (fpath)
  "Get cached thumbnail for media at FPATH."
  (let ((cache-fpath (ready-player--thumbnail-path fpath)))
    (when (and (file-exists-p cache-fpath)
               (> (file-attribute-size (file-attributes cache-fpath)) 0))
      cache-fpath)))

(defun ready-player--load-file-thumbnail-via-ffmpeg (media-fpath on-loaded)
  "Load media thumbnail at MEDIA-FPATH and invoke ON-LOADED.

Note: This needs the ffmpeg command line utility."
  (if (executable-find "ffmpeg")
      (let* ((thumbnail-fpath (ready-player--thumbnail-path media-fpath)))
        (make-process
         :name "ffmpeg-process"
         :buffer (get-buffer-create "*ffmpeg-output*")
         :command (list "ffmpeg" "-i" media-fpath "-vf" "thumbnail" "-frames:v" "1" thumbnail-fpath)
         :sentinel
         (lambda (process _)
           (if (eq (process-exit-status process) 0)
               (funcall on-loaded thumbnail-fpath)
             (condition-case nil
                 (delete-file thumbnail-fpath)
               (file-error nil))))))
    (message "Metadata not available (ffmpeg not found)")))

(defun ready-player--load-file-metadata (fpath on-loaded)
  "Load media metadata at FPATH and invoke ON-LOADED."
  (if (executable-find "ffprobe")
      (let ((buffer (generate-new-buffer "*ffprobe-output*")))
        (with-current-buffer buffer
          (erase-buffer))
        (make-process
         :name "ffprobe-process"
         :buffer buffer
         :command (list "ffprobe" "-v" "quiet" "-print_format" "json" "-show_format" "-show_streams" fpath)
         :sentinel
         (lambda (process _)
           (when (eq (process-exit-status process) 0)
             (with-current-buffer (process-buffer process)
               (goto-char (point-min))
               (funcall on-loaded (json-parse-buffer :object-type 'alist))))
           (kill-buffer (process-buffer process)))))
    (message "Metadata not available (ffprobe not found)")))

(defun ready-player--playback-buffer ()
  "Get the process playback buffer."
  (let ((buffer (get-buffer-create "*play*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer))
      (let ((inhibit-message t))
        ;; Silence noise of entering shell-mode.
        (shell-mode)))
    buffer))

(defun ready-player--format-metadata-rows (rows)
  "Format metadata ROWS for rendering."
  (if rows
      (let ((max-label-length (+ 1 (apply #'max (mapcar (lambda (row) (length (cdr (assoc 'label row)))) rows)))))
        (mapconcat (lambda (row)
                     (let ((label (cdr (assoc 'label row)))
                           (value (cdr (assoc 'value row))))
                       (format " %s%s %s\n\n"
                               (propertize label 'face 'font-lock-comment-face)
                               (make-string (- max-label-length (length label)) ?\s)
                               value)))
                   rows))
    ""))

(defun ready-player--format-duration (duration)
  "Format DURATION in a human-readable format."
  (setq duration (string-to-number duration))
  (let* ((hours   (/ duration 3600))
         (minutes (/ (mod duration 3600) 60))
         (seconds (mod duration 60)))
    (format "%d:%02d:%02d" hours minutes seconds)))

(defun ready-player--readable-size (size)
  "Format SIZE in a human-readable format."
  (setq size (string-to-number size))
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
  (when (and (not ready-player-cache-thumbnails)
             ready-player--thumbnail)
    (condition-case nil
        (delete-file ready-player--thumbnail)
      (file-error nil)))
  (when ready-player--process
    (delete-process ready-player--process)
    (setq ready-player--process nil)))

(provide 'ready-player)

;;; ready-player.el ends here
