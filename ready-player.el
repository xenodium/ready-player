;;; ready-player.el --- Open media files in ready-player major mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; Package-Requires: ((emacs "28.1"))
;; URL: https://github.com/xenodium/ready-player
;; Version: 0.16.1
(defconst ready-player--version "0.16.1")

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
;;   (ready-player-mode)
;;
;; To customize supported media files, set `ready-player-supported-media'
;; before invoking `ready-player-add-to-auto-mode-alist'.
;;
;; `ready-player-mode' relies on command line utilities to play media.
;;  Customize `ready-player-open-playback-commands' to your preference.
;;
;; Note: This is a new package.  Please report issues or send
;; patches to https://github.com/xenodium/ready-player

(require 'cl-lib)
(require 'comint)
(require 'dired)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'text-property-search)

;;; Code:

(defvar ready-player-major-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "SPC") #'ready-player-toggle-play-stop)
    (define-key map (kbd "TAB") #'ready-player-next-button)
    (define-key map (kbd "<backtab>") #'ready-player-previous-button)
    (define-key map (kbd "a") #'ready-player-toggle-autoplay)
    (define-key map (kbd "s") #'ready-player-toggle-shuffle)
    (define-key map (kbd "r") #'ready-player-toggle-repeat)
    (define-key map (kbd "i") #'ready-player-show-info)
    (define-key map (kbd "n") #'ready-player-next)
    (define-key map (kbd "p") #'ready-player-previous)
    (define-key map (kbd "f") #'ready-player-seek-forward)
    (define-key map (kbd "b") #'ready-player-seek-backward)
    (define-key map (kbd "e") #'ready-player-open-externally)
    (define-key map (kbd "o") #'ready-player-open-externally)
    (define-key map (kbd "q") #'ready-player-quit)
    (define-key map (kbd "g") #'ready-player-reload-buffer)
    (define-key map (kbd "m") #'ready-player-mark-dired-file)
    (define-key map (kbd "/") #'ready-player-search-dired-buffer-index)
    (define-key map (kbd "u") #'ready-player-unmark-dired-file)
    (define-key map (kbd "d") #'ready-player-view-dired-playback-buffer)
    map)
  "Keymap for `ready-player'.")

(defgroup ready-player nil
  "Settings for Ready Player mode."
  :group 'media)

(defcustom ready-player-set-global-bindings t
  "When non-nil, bind global bindings under C-c m prefix.

Value must be set before invoking `ready-player-mode'."
  :type 'boolean
  :group 'ready-player)

(defcustom ready-player-multi-buffer nil
  "When non-nil, enable opening multiple buffers with parallel playback."
  :type 'boolean
  :group 'ready-player)

(defcustom ready-player-show-thumbnail t
  "When non-nil, display file's thumbnail if available."
  :type 'boolean
  :group 'ready-player)

(defcustom ready-player-always-load-directory-recursively t
  "When non-nil, load directory recursively without prompt."
  :type 'boolean
  :group 'ready-player)

(defcustom ready-player-repeat t
  "Continue playing if there's more media in directory or playlist.

Playlist is automatically generated from the current directory.

t or `playlist'

  repeats and starts over from the beginning of the playlist/directory.

file

  repeats current file.

nil

  no repeat."
  :type '(choice (const :tag "Repeat" t)
                 (const :tag "No Repeat" nil)
                 (const :tag "File" file)
                 (const :tag "Playlist" playlist))
  :group 'ready-player)

(defcustom ready-player-autoplay t
  "When non-nil, automatically start playing when media file opens."
  :type 'boolean
  :group 'ready-player)

(defcustom ready-player-shuffle nil
  "Next media item is selected at random within current directory.

Repeats and starts over from the beginning of the directory."
  :type 'boolean
  :group 'ready-player)

(defcustom ready-player-cache-thumbnails t
  "When non-nil, cache thumbnail."
  :type 'boolean
  :group 'ready-player)

(defcustom ready-player-cache-metadata t
  "When non-nil, cache metadata."
  :type 'boolean
  :group 'ready-player)

(defcustom ready-player-hide-modeline t
  "If non-nil, hides mode line in buffer.

File information is already displayed in the buffer,
so users can opt to hide the mode line."
  :type 'boolean
  :group 'ready-player)

(defcustom ready-player-previous-icon "◁◁"
  "Previous button icon string, for example: \"◁◁\"."
  :type 'string
  :group 'ready-player)

(defcustom ready-player-play-icon "▶"
  "Play button icon string, for example: \"▶\"."
  :type 'string
  :group 'ready-player)

(defcustom ready-player-next-icon "▷▷"
  "Next button icon string, for example: \"▷▷\"."
  :type 'string
  :group 'ready-player)

(defcustom ready-player-open-externally-icon "➦"
  "Open externally button icon string, for example: \"➦\"."
  :type 'string
  :group 'ready-player)

(defcustom ready-player-stop-icon "■"
  "Stop icon string, for example: \"■\"."
  :type 'string
  :group 'ready-player)

(defcustom ready-player-repeat-icon "⇆"
  "Repeat icon string, for example: \"⇆\"."
  :type 'string
  :group 'ready-player)

(defcustom ready-player-shuffle-icon "⤮"
  "Shuffle icon string, for example: \"⤮\"."
  :type 'string
  :group 'ready-player)

(defcustom ready-player-autoplay-icon "⏻"
  "Autoplay icon string, for example: \"⏻\"."
  :type 'string
  :group 'ready-player)

(defcustom ready-player-search-icon "⌕"
  "Search icon string, for example: \"⌕\"."
  :type 'string
  :group 'ready-player)

(defcustom ready-player-thumbnail-max-pixel-height
  350
  "Maximum thumbnail pixel height."
  :type 'integer
  :group 'ready-player)

(defcustom ready-player-open-playback-commands
  '(("mpv" "--audio-display=no" "--input-ipc-server=<<socket>>")
    ("vlc")
    ("ffplay")
    ("mplayer"))
  "Command line utilities to try for playback.

Each command entry is a list to cater for additional command flags.

Omit file path parameter, as it will be automatically appended.

Prepend each command with a function to apply additional logic.

For example, to use different utilities for video and audio:

  ((ready-player-is-audio-p \"ffplay\" \"--audio-display=no\")
   (ready-player-is-video-p \"mpv\"))

You can further extend with additional logic like:

  ((ready-player-is-ogg123-p \"ogg123\")
   (ready-player-is-audio-p \"ffplay\" \"--audio-display=no\")
   (ready-player-is-video-p \"mpv\"))

To cater for different extensions, use a list of extensions as
first item in command list:

  (((\"mp3\" \"ogg\") \"audacious\")
   (\"mpv\" \"--audio-display=no\")
   (\"vlc\")
   (\"ffplay\")
   (\"mplayer\"))

To inject a socket file, use <<socket>>:

   ((\"mpv\" \"--audio-display=no\" \"--input-ipc-server=<<socket>>\")
    (\"vlc\")
    (\"ffplay\")
    (\"mplayer\"))

Note: Socket can only be used by mpv player. Get in touch if keen to
add for other players."
  :type '(repeat (list string))
  :group 'ready-player)

(defcustom ready-player-display-dired-playback-buffer-display-action
  `((display-buffer-reuse-mode-window
     (lambda (buffer alist) ;; Use right side window if one available.
       (when (window-combination-p (frame-root-window (selected-frame)) t)
         (window--display-buffer buffer
                                 (car (window-at-side-list nil 'right))
                                 'reuse alist)))
     display-buffer-in-direction)
    (window-width . 0.60)
    (direction . right))
  "Choose how to display the associated playback `dired' buffer.

Same format as a the action in a `display-buffer-alist' entry."
  :type (plist-get (cdr (get 'display-buffer-alist 'custom-type)) :value-type)
  :group 'ready-player)

(defcustom ready-player-supported-media
  #'ready-player-supported-audio-and-video
  "Supported media types."
  :group 'play-mode
  :type '(choice (function :tag "Function")
                 (repeat (string :tag "String"))))

(defcustom ready-player-supported-video
  '("3g2" "3gp" "asf" "asx" "avi" "divx" "drc" "dvb" "evo" "f4p"
    "f4v" "flv" "h264" "h265" "hevc" "m2ts" "m2v" "mkv" "mov" "mp4"
    "mpg" "mpeg" "mts" "mxf" "ogm" "ogv" "qt" "rm" "rmvb" "vob"
    "webm" "wmv")
  "Supported video media."
  :group 'play-mode
  :type '(repeat string))

(defcustom ready-player-supported-audio
  '("aac" "ac3" "aiff" "amr" "ape" "dts" "f4a" "f4b" "flac" "gsm"
    "m4a" "midi" "mlp" "mka" "mp2" "mp3" "oga" "ogg" "opus" "pva"
    "ra" "ram" "raw" "rf64" "spx" "tta" "wav" "wavpack" "wma" "wv")
  "Supported audiomedia."
  :group 'play-mode
  :type '(repeat string))

(defvar-local ready-player--process nil "Media-playing process.")

(defvar ready-player--active-buffer nil "Buffer to interact with.")

(defvar-local ready-player--metadata nil "Metadata as per ffprobe.")

(defvar-local ready-player--thumbnail nil "Thumbnail as per ffmpeg.")

(defvar ready-player--last-button-focus 'play-stop
  "Last button focused.

Could be one of:

=next= =previous= =play-stop= =open-externally= =repeat= =shuffle= or
=autoplay.=

Used to remember button position across files in continuous playback.")

(defvar ready-player--highest-priority-dired-buffer nil
  "Non-nil to make this this highest priority `dired' for playback.

Note: This must not be permanently set.

See `ready-player-load-dired-buffer' for temporary override.")

(defvar-local ready-player--dired-playback-buffer nil
  "`dired' buffer used when determining next/previous file.")

;;;###autoload
(define-minor-mode ready-player-mode
  "Toggle Ready Player mode media file recognition.

See variable `ready-player-supported-media' for recognized types."
  :global t
  (let ((called-interactively (called-interactively-p #'interactive)))
    (if ready-player-mode
        (progn
          (ready-player-add-to-auto-mode-alist)
          (when ready-player-set-global-bindings
            (global-set-key (kbd "C-c m m") 'ready-player-view-player)
            (global-set-key (kbd "C-c m /") 'ready-player-search-dired-buffer-index)
            (global-set-key (kbd "C-c m n") 'ready-player-next)
            (global-set-key (kbd "C-c m p") 'ready-player-previous)
            (global-set-key (kbd "C-c m i") 'ready-player-show-info)
            (global-set-key (kbd "C-c m SPC") 'ready-player-toggle-play-stop)
            (global-set-key (kbd "C-c m r") 'ready-player-toggle-repeat)
            (global-set-key (kbd "C-c m s") 'ready-player-toggle-shuffle)
            (global-set-key (kbd "C-c m a") 'ready-player-toggle-autoplay))
          (when (and called-interactively
                     (string-match-p "no-conversion"
                                     (symbol-name buffer-file-coding-system)))
            (revert-buffer nil t)))
      (ready-player-remove-from-auto-mode-alist)
      (when ready-player-set-global-bindings
        (global-unset-key (kbd "C-c m m"))
        (global-unset-key (kbd "C-c m /"))
        (global-unset-key (kbd "C-c m n"))
        (global-unset-key (kbd "C-c m p"))
        (global-unset-key (kbd "C-c m i"))
        (global-unset-key (kbd "C-c m SPC"))
        (global-unset-key (kbd "C-c m r"))
        (global-unset-key (kbd "C-c m s"))
        (global-unset-key (kbd "C-c m a")))
      (when (and called-interactively
                 (derived-mode-p 'ready-player-major-mode))
        (revert-buffer nil t)))))

;;;###autoload
(defun ready-player-add-to-auto-mode-alist ()
  "Add media recognized by `ready-player-mode'."
  (add-to-list 'auto-mode-alist
               (cons (concat "\\." (regexp-opt (ready-player-supported-media) t) "\\'")
                     #'ready-player-major-mode))
  ;; Suppress unnecessary buffer loading via file-name-handler-alist.
  (add-to-list
   'file-name-handler-alist
   (cons
    (concat "\\." (regexp-opt (ready-player--supported-media-with-uppercase) t) "\\'")
    #'ready-player-file-name-handler)))

(defun ready-player-macos-use-sf-symbols ()
  "Set all button icons to use macOS SF symbols."
  (setq ready-player-previous-icon "􀊉")
  (setq ready-player-play-icon "􀊄")
  (setq ready-player-next-icon "􀊋")
  (setq ready-player-open-externally-icon "􀉐")
  (setq ready-player-stop-icon "􀛷")
  (setq ready-player-repeat-icon "􀊞")
  (setq ready-player-shuffle-icon "􀊝")
  (setq ready-player-autoplay-icon "􀋦")
  (setq ready-player-search-icon "􀊫"))

(defun ready-player--supported-media-with-uppercase ()
  "Duplicate variable `ready-player-supported-media' with uppercase equivalents."
  (append (ready-player-supported-media)
          ;; Also include uppercase extensions.
          ;; APFS (Apple File System) is case-insensitive.
          (mapcar #'upcase (ready-player-supported-media))))

(defun ready-player-remove-from-auto-mode-alist ()
  "Remove media recognized by `ready-player-mode'."
  (setq auto-mode-alist
        (seq-remove (lambda (entry)
                      (and (symbolp (cdr entry))
                           (string-match "ready-player-major-mode" (symbol-name (cdr entry)))))
                    auto-mode-alist))
  (setq file-name-handler-alist
        (seq-remove (lambda (entry)
                      (equal #'ready-player-file-name-handler (cdr entry)))
                    file-name-handler-alist)))

(defun ready-player-supported-media ()
  "Get a list of all supported media.

See variable `ready-player-supported-media' for configuration."
  (if (listp ready-player-supported-media)
      ready-player-supported-media
    (funcall ready-player-supported-media)))

(defun ready-player-supported-audio-and-video ()
  "Get a list of supported audio and video.

See `ready-player-supported-audio' and `ready-player-supported-video'
for configuration."
  (append ready-player-supported-audio
          ready-player-supported-video))

(defun ready-player-is-audio-p (file)
  "Return non-nil if FILE extension is found in `ready-player-supported-audio'."
  (seq-contains-p ready-player-supported-audio
                  (file-name-extension file)
                  (lambda (a b)
                    (string-equal (downcase a) (downcase b)))))

(defun ready-player-is-ogg123-p (file)
  "Return non-nil if FILE can be handled by ogg123 utility."
  (equal (downcase (file-name-extension file)) "ogg"))

(defun ready-player-is-video-p (file)
  "Return non-nil if FILE extension is found in `ready-player-supported-video'."
  (seq-contains-p ready-player-supported-video
                  (file-name-extension file)
                  (lambda (a b)
                    (string-equal (downcase a) (downcase b)))))

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
	    (attributes (apply #'file-name-non-special
                               (append (list operation) args))))
       ;; 7 is file size location
       ;; as per `file-attributes'.
       (setf (nth 7 attributes) 0)
       attributes))
    (_ (let ((inhibit-file-name-handlers
              (cons #'ready-player-file-name-handler
                    (and (eq inhibit-file-name-operation operation)
                         inhibit-file-name-handlers)))
             (inhibit-file-name-operation operation))
         (apply operation args)))))

(define-derived-mode ready-player-major-mode special-mode "Ready Player"
  "Major mode to preview and play media files."
  :after-hook (progn
                (unless ready-player-multi-buffer
                  (let ((buffer (current-buffer)))
                    ;; Execute after current run loop to allow `find-file' completion.
                    (run-at-time 0.1 nil
                                 (lambda ()
                                   (ready-player--keep-only-this-buffer buffer)))))
               (when ready-player-autoplay
                  (ready-player--start-playback-process))
                (ready-player--goto-button ready-player--last-button-focus))
  :keymap ready-player-major-mode-map
  (set-buffer-multibyte t)
  (setq buffer-read-only t)
  (setq buffer-undo-list t)
  (when ready-player-hide-modeline
    (setq mode-line-format nil))
  (ready-player--save-state)

  (let* ((buffer (current-buffer))
         (fpath (buffer-file-name))
         (cached-metadata (ready-player--cached-metadata fpath))
         (local-thumbnail (ready-player--local-thumbnail-in-directory default-directory))
         (cached-thumbnail (or (ready-player--cached-thumbnail fpath)
                               local-thumbnail))
         (cached-dired-buffer (ready-player--resolve-file-dired-buffer
                               fpath ready-player--highest-priority-dired-buffer))
         (thumbnailer (if (executable-find "ffmpegthumbnailer")
                          #'ready-player--load-file-thumbnail-via-ffmpegthumbnailer
                        #'ready-player--load-file-thumbnail-via-ffmpeg)))
    (ready-player--update-buffer-name buffer nil)
    ;; Sets default related dired buffer.
    (if cached-dired-buffer
        (setq ready-player--dired-playback-buffer
              cached-dired-buffer)
      (setq ready-player--dired-playback-buffer
            (ready-player--find-file-noselect default-directory)))
    (ready-player--index-dired-buffer
     ready-player--dired-playback-buffer)
    (setq ready-player--active-buffer buffer)
    (ready-player--update-buffer buffer fpath
                                 ready-player--process
                                 ready-player-repeat
                                 ready-player-shuffle
                                 ready-player-autoplay
                                 nil nil (ready-player--dired-playback-buffer))
    (if cached-thumbnail
        (progn
          (setq ready-player--thumbnail cached-thumbnail)
          (ready-player--update-buffer
           buffer fpath
           ready-player--process
           ready-player-repeat
           ready-player-shuffle
           ready-player-autoplay
           cached-thumbnail ready-player--metadata
           (ready-player--dired-playback-buffer)))
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
                              ready-player-shuffle
                              ready-player-autoplay
                              thumbnail ready-player--metadata
                              (ready-player--dired-playback-buffer))
                             (ready-player--goto-button
                              ready-player--last-button-focus)))))))

    ;; Also attempt to extract embedded thumbnail to give it preference if found.
    (when local-thumbnail
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
                              ready-player-shuffle
                              ready-player-autoplay
                              thumbnail ready-player--metadata
                              (ready-player--dired-playback-buffer))
                             (ready-player--goto-button
                              ready-player--last-button-focus)))))))

    (if cached-metadata
        (progn
          (setq ready-player--metadata cached-metadata)
          (ready-player--update-buffer
           buffer fpath
           ready-player--process
           ready-player-repeat
           ready-player-shuffle
           ready-player-autoplay
           cached-thumbnail ready-player--metadata
           (ready-player--dired-playback-buffer)))
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
                      ready-player-shuffle
                      ready-player-autoplay
                      ready-player--thumbnail metadata
                      (ready-player--dired-playback-buffer))
                     (ready-player--goto-button
                      ready-player--last-button-focus))))))))
  (add-hook 'kill-buffer-hook #'ready-player--clean-up nil t))

(defun ready-player-version ()
  "Show Ready Player Mode version."
  (interactive)
  (message "Ready Player Mode v%s" ready-player--version))

(defun ready-player--make-thumbnail-placeholder (width height)
  "Make a thumbnail placeholder with WIDTH and HEIGHT dimensions."
  (let* ((icon-size 96)
         ;; TODO: Choose colors.
         (background-color (face-attribute 'default :background))
         (foreground-color (face-attribute 'default :foreground))
         (svg (svg-create width height)))
    (svg-rectangle svg 0 0 width height :fill background-color)
    (svg-text svg "♫"
              :x (/ width 2) :y (+ (/ height 2) (/ icon-size 3))
              :fill foreground-color :font-size icon-size
              :text-anchor "middle" :dominant-baseline "central")
    (svg-image svg)))

(defun ready-player--save-state ()
  "Save state across Emacs sessions."
  (ready-player--ensure-mode)
  (let ((media-file (buffer-file-name)))
    (with-temp-file (file-name-concat
                     (ready-player--temp-dir)
                     ".ready-player-state.el")
      (prin1 (list
              (cons 'buffer-file-name media-file))
             (current-buffer)))))

(defun ready-player--read-state ()
  "Read persisted state across Emacs sessions."
  (with-temp-buffer
    (condition-case nil
        (insert-file-contents (file-name-concat
                               (ready-player--temp-dir)
                               ".ready-player-state.el"))
      (error nil))
    (goto-char (point-min))
    (condition-case nil
        (read (current-buffer))
      (error nil))))

(defun ready-player--update-buffer (buffer fpath busy repeat shuffle autoplay &optional thumbnail metadata dired-buffer)
  "Update entire BUFFER content.

Render state from FPATH BUSY REPEAT SHUFFLE AUTOPLAY THUMBNAIL METADATA
and DIRED-BUFFER."
  (save-excursion
    (let ((fname (file-name-nondirectory fpath))
          (buffer-read-only nil))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (erase-buffer)
          (goto-char (point-min))
          (when ready-player-show-thumbnail
            (let ((inhibit-read-only t))
              (insert "\n ")
              (insert-image
               (if thumbnail
                   (create-image
                    thumbnail nil nil
                    :max-height ready-player-thumbnail-max-pixel-height)
                 (ready-player--make-thumbnail-placeholder
                  ready-player-thumbnail-max-pixel-height
                  ready-player-thumbnail-max-pixel-height)))
              (insert "\n")
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
          (insert (ready-player--make-file-button-line busy repeat
                                                       shuffle autoplay))
          (insert "\n")
          (insert "\n")
          (when metadata
            (insert (ready-player--format-metadata-rows
                     (ready-player--make-metadata-rows metadata dired-buffer))))
          (set-buffer-modified-p nil))))))

(defun ready-player-view-player ()
  "Switch to player buffer.

If on player buffer already, switch to previous buffer."
  (interactive)
  (if (eq major-mode 'ready-player-major-mode)
      (let ((buffers (buffer-list)))
        (when (>= (length buffers) 2)
          (switch-to-buffer (nth 1 buffers))))
    (if-let ((active-buffer (ready-player--active-buffer t)))
        (switch-to-buffer active-buffer)
      (if-let ((last-played (map-elt (ready-player--read-state)
                                     'buffer-file-name)))
          (find-file last-played)
        ;; Errors if there's no active buffer available.
        (ready-player--active-buffer)))))

(defun ready-player-show-info ()
  "Show playback info in the echo area."
  (interactive)
  (with-current-buffer (ready-player--active-buffer)
    (let ((fname (file-name-nondirectory (buffer-file-name))))
      (if ready-player--metadata
          (ready-player--message
           (ready-player--make-metadata-echo-text ready-player--metadata fname)
           5)
        (ready-player--load-file-metadata
         (buffer-file-name)
         (lambda (metadata)
           (ready-player--message
            (ready-player--make-metadata-echo-text metadata fname)
            5)))))))

(defun ready-player--make-metadata-echo-text (metadata fallback)
  "Make METADATA echo text for display.

If no useful metadata found, use FALLBACK."
  (let ((title (ready-player--row-value
                (ready-player--make-metadata-rows metadata)
                "Title:"))
        (artist (ready-player--row-value
                 (ready-player--make-metadata-rows metadata)
                 "Artist:"))
        (album (ready-player--row-value
                (ready-player--make-metadata-rows metadata)
                "Album:"))
        (text ""))
    (when title
      (setq text (concat text
                         (if (string-empty-p text) "" "    ")
                         title)))
    (when artist
      (setq text (concat text
                         (if (string-empty-p text) "" "    ")
                         (propertize
                          artist
                          'face 'font-lock-string-face))))
    (when album
      (setq text (concat text
                         (if (string-empty-p text) "" "    ")
                         (propertize
                          album
                          'face 'font-lock-variable-name-face))))
    (if (string-empty-p text)
        fallback
      text)))

(defun ready-player--make-metadata-mp3-rows (metadata)
  "Make METADATA row data from an mp3 file."
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
                             (cons 'value .format.tags.album)))))))
    metadata-rows))

(defun ready-player--make-metadata-core-rows (metadata)
  "Make core METADATA row data."
  (let ((metadata-rows))
    (let-alist metadata
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
                             (cons 'value (ready-player--readable-size .format.size))))))))))

(defun ready-player--make-metadata-ogg-rows (metadata)
  "Make METADATA row data from an ogg file."
  (let ((metadata-rows)
        (stream))
    (let-alist metadata
      (setq stream (seq-first .streams))
      (let-alist stream
        (when (or .tags.title .tags.TITLE)
          (setq metadata-rows
                (append metadata-rows
                        (list
                         (list (cons 'label "Title:")
                               (cons 'value (or .tags.title .tags.TITLE)))))))
        (when (or .tags.artist .tags.ARTIST)
          (setq metadata-rows
                (append metadata-rows
                        (list
                         (list (cons 'label "Artist:")
                               (cons 'value (or .tags.artist .tags.ARTIST)))))))
        (when (or .tags.album .tags.ALBUM)
          (setq metadata-rows
                (append metadata-rows
                        (list
                         (list (cons 'label "Album:")
                               (cons 'value (or .tags.album .tags.ALBUM)))))))))))

(defun ready-player--make-metadata-rows (metadata &optional dired-buffer)
  "Make METADATA row data with DIRED-BUFFER."
  (let ((metadata-rows)
        (new-rows))
    (setq new-rows (ready-player--make-metadata-mp3-rows metadata))
    (setq metadata-rows (append metadata-rows new-rows))
    (when new-rows
      (setq metadata-rows (append metadata-rows
                                  (ready-player--make-dired-playlist-row dired-buffer))))
    (setq new-rows (ready-player--make-metadata-ogg-rows metadata))
    (setq metadata-rows (append metadata-rows new-rows))
    (when new-rows
      (setq metadata-rows (append metadata-rows
                                  (ready-player--make-dired-playlist-row dired-buffer))))
    (setq new-rows (ready-player--make-metadata-core-rows metadata))
    (setq metadata-rows (append metadata-rows new-rows))
    metadata-rows))

(defun ready-player--make-dired-playlist-row (dired-buffer)
  "Make DIRED-BUFFER playlist row."
  (when dired-buffer
    (list
     (list (cons 'label "Playlist:")
           (cons 'value
                 (ready-player--make-button
                  (buffer-name dired-buffer)
                  'dired
                  #'ready-player-view-dired-playback-buffer
                  t))))))

(defun ready-player--goto-button (button)
  "Goto BUTTON (see \=`ready-player--last-button-focus'\= for values)."
  (ready-player--ensure-mode)
  (when-let* ((match (save-excursion
                       (goto-char (point-min))
                       (text-property-search-forward 'button button)))
              (button-pos (prop-match-end match)))
    (if-let ((window (get-buffer-window (current-buffer))))
        ;; Attempt to focus unfocused window so point actually moves.
        (with-selected-window window
          (goto-char button-pos))
      (goto-char button-pos))))

(defun ready-player-next-button ()
  "Navigate to next button."
  (interactive)
  (ready-player--ensure-mode)
  (if-let ((result (text-property-search-forward 'button nil nil t)))
      (progn
        (goto-char (prop-match-beginning result))
        (setq ready-player--last-button-focus
              (or (get-text-property (point) 'button)
                  ready-player--last-button-focus)))
    (goto-char (point-min))
    (ready-player-next-button)))

(defmacro ready-player--with-buffer-focused (buffer &rest body)
  "Like `with-current-buffer' executing BODY with BUFFER and WINDOW focused."
  `(with-current-buffer ,buffer
     (if-let ((win (get-buffer-window ,buffer)))
         (with-selected-window win
           ,@body)
       ,@body)))

(defun ready-player-previous-button ()
  "Navigate to previous button."
  (interactive)
  (ready-player--ensure-mode)
  (if-let ((result (text-property-search-backward 'button)))
      (setq ready-player--last-button-focus
            (or (get-text-property (point) 'button)
                ready-player--last-button-focus))
    (goto-char (point-max))
    (ready-player-previous-button)))

(defun ready-player-quit ()
  "Quit `ready-player-major-mode' window and kill buffer."
  (interactive)
  (ready-player--ensure-mode)
  (quit-window t))

;; Based on `crux-open-with'.
(defun ready-player-open-externally (arg)
  "Open visited file in default external program.
When in `dired' mode, open file under the cursor.

With a prefix ARG always prompt for command to use."
  (interactive "P")
  (ready-player--ensure-mode)
  (ready-player-stop)
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

(defun ready-player-previous (&optional n)
  "Open the previous media file in the same directory.

With optional argument N, visit the Nth file before the current one."
  (interactive "p" ready-player)
  (let ((in-player (eq major-mode 'ready-player-major-mode)))
    (with-current-buffer (ready-player--active-buffer)
      (ready-player--open-file-at-offset (- n) t)
      (unless in-player
        (ready-player-show-info)))))

(defun ready-player-next (&optional n)
  "Open the next media file in the same directory.

With optional argument N, visit the Nth file after the current one."
  (interactive "p" ready-player)
  (let ((in-player (eq major-mode 'ready-player-major-mode)))
    (with-current-buffer (ready-player--active-buffer)
      (ready-player--open-file-at-offset n t)
      (unless in-player
        (ready-player-show-info)))))

(defun ready-player--find-file-noselect (file)
  "Like `find-file-noselect' but be silent and don't record FILE in `recentf'."
  (let ((inhibit-message t)
        (recentf-exclude (list (concat (regexp-quote (file-name-nondirectory file)) "\\'"))))
    (ignore recentf-exclude)
    (find-file-noselect file)))

(defun ready-player--open-file (fpath buffer start-playing)
  "Open file at FPATH in BUFFER.

If START-PLAYING is non-nil, start playing the media file."
  (let ((old-buffer buffer)
        ;; Auto-played files should not be added to recentf.
        ;; Temporarily override `recentf-exclude'.
        (new-buffer (ready-player--find-file-noselect fpath)))
    (with-current-buffer new-buffer
      (when (get-buffer-window-list old-buffer nil t)
        (set-window-buffer (car (get-buffer-window-list old-buffer nil t)) new-buffer))
      (unless (eq new-buffer old-buffer)
        (kill-buffer old-buffer))
      (when start-playing
        (ready-player--start-playback-process)))
    new-buffer))

(defun ready-player--open-file-at-offset (n feedback)
  "Open the next media file in the same directory.

With optional argument N offset, visit the Nth file after the current
one.  Negative values move backwards.

With FEEDBACK, provide user feedback of the interaction."
  (ready-player--ensure-mode)
  (when feedback
    (ready-player--goto-button (if (> n 0) 'next 'previous))
    (setq ready-player--last-button-focus (if (> n 0) 'next 'previous)))

  (let* ((sticky-dired-buffer (ready-player--dired-playback-buffer))
         (playing ready-player--process)
         (new-file (or (ready-player--next-dired-file-from
                        buffer-file-name n nil ready-player-shuffle sticky-dired-buffer)
                       (when ready-player-repeat
                         (ready-player--next-dired-file-from
                          buffer-file-name n t ready-player-shuffle sticky-dired-buffer))))
         (new-buffer))
    (if new-file
        (progn
          (setq new-buffer (ready-player--open-file new-file (current-buffer) playing))
          (with-current-buffer new-buffer
            (setq ready-player--dired-playback-buffer sticky-dired-buffer)))
      (if playing
          (progn
            (message "No more media to play"))
        (message "No more media")))
    new-file))


(defun ready-player--dired-playback-buffer ()
  "Resolve the associated `dired' buffer, creating it if needed."
  (ready-player--ensure-mode)
  (cond ((and ready-player--dired-playback-buffer
              (buffer-live-p ready-player--dired-playback-buffer))
         ;; Dired set in buffer?
         ready-player--dired-playback-buffer)
        ((and ready-player--active-buffer
              (buffer-live-p ready-player--active-buffer)
              (buffer-local-value 'ready-player--dired-playback-buffer
                                  ready-player--active-buffer)
              (buffer-live-p (buffer-local-value 'ready-player--dired-playback-buffer
                                                 ready-player--active-buffer)))
         ;; Dired set in active buffer?
         (setq ready-player--dired-playback-buffer
               (buffer-local-value 'ready-player--dired-playback-buffer
                                   ready-player--active-buffer))
         ready-player--dired-playback-buffer)
        (t
         ;; Fall back to dired in current directory.
         (setq ready-player--dired-playback-buffer
               (find-file-noselect (file-name-directory (buffer-file-name))))
         ready-player--dired-playback-buffer)))

;; Based on `image-next-file'.
(defun ready-player--next-dired-file-from (file offset &optional from-top random dired-buffer)
  "Get the next available file from a `dired' buffer.

`dired' buffers are either derived from `file' or function
`ready-player--dired-playback-buffer'.

Start at the FILE's location in buffer and move to OFFSET.
If FROM-TOP is non-nil, offset is from top of the buffer.

With RANDOM set, choose next file at random.

Override DIRED-BUFFER, otherwise resolve internally."
  (let* ((regexp (concat "\\b" (regexp-opt (ready-player--supported-media-with-uppercase) t) "\\b"))
         (dired-buffers  (if (or dired-buffer (ready-player--dired-playback-buffer))
                             (list (or dired-buffer (ready-player--dired-playback-buffer)))
                           (when-let ((non-nil file)
                                      ;; Auto-played files should not be added to recentf.
                                      ;; Temporarily override `recentf-exclude'.
                                      (recentf-exclude (list (concat (regexp-quote (file-name-nondirectory file)) "\\'"))))
                             (ignore recentf-exclude)
                             (find-file-noselect (file-name-directory file))
                             (dired-buffers-for-dir (file-name-directory file)))))
         (next))
    ;; Move point in all relevant dired buffers.
    (dolist (buffer dired-buffers)
      (ready-player--with-buffer-focused
       buffer
       (if random
           (progn
             (goto-char (point-min))
             ;; Goto random line.
             (forward-line (+ (point-min)
                              (random (count-lines (point-min)
                                                   (point-max))))))
         (if (or from-top (not file))
             (goto-char (point-min))
           (dired-goto-file file)))
       (let (found)
         (while (and (not found)
                     (if (> offset 0)
                         (not (eobp))
                       (not (bobp))))
           (dired-next-line offset)
           ;; Ensure (eobp) or (bobp) are reached.
           (if (> offset 0)
               (end-of-line)
             (beginning-of-line))
           (when-let* ((candidate (dired-get-filename nil t))
                       (extension (file-name-extension candidate))
                       (match-p (string-match-p regexp extension)))
             (setq found candidate)))
         (if found
             (progn
               (setq next found)
               (forward-line 0))
           ;; No next match. Restore point.
           (when file
             (dired-goto-file file))))))
    next))

;; Based on `image-mode-mark-file'.
(defun ready-player-mark-dired-file ()
  "Mark the current file in the appropriate `dired' buffer(s)."
  (interactive nil ready-player-major-mode)
  (unless buffer-file-name
    (user-error "No media file in this buffer"))
  (if-let* ((file buffer-file-name)
            (marked-buffer
             (ready-player--apply-dired-function
              #'dired-mark file)))
      (progn
        (switch-to-buffer-other-window marked-buffer)
        (dired-goto-file file))
    (message "Couldn't find file to mark")))

;; Based on `image-mode-unmark-file'.
(defun ready-player-unmark-dired-file ()
  "Unmark the current file in the appropriate `dired' buffer(s)."
  (interactive nil ready-player-major-mode)
  (unless buffer-file-name
    (user-error "No media file in this buffer"))
  (if-let* ((file buffer-file-name)
            (marked-buffer
             (ready-player--apply-dired-function
              #'dired-unmark file)))
      (progn
        (switch-to-buffer-other-window marked-buffer)
        (dired-goto-file file))
    (message "Couldn't find file to unmark")))

(defun ready-player--apply-dired-function (function file)
  "Apply `dired' FUNCTION to FILE."
  (let* ((dir (file-name-directory file))
         (found)
	 (buffers (append
                   (seq-filter (lambda (buffer)
                                 (with-current-buffer buffer
                                   (and (derived-mode-p 'dired-mode)
                                        (equal (file-truename dir)
                                               (file-truename default-directory)))))
                               (dired-buffers-for-dir dir))
                   (list (ready-player--dired-playback-buffer)))))
    (unless buffers
      (save-excursion
        (setq buffers (list (find-file-noselect dir)))))
    (mapc
     (lambda (buffer)
       (with-current-buffer buffer
	 (when (dired-goto-file file)
	   (funcall function 1)
           (setq found buffer)))) buffers)
    found))

(defun ready-player-stop ()
  "Stop media playback."
  (interactive)
  (with-current-buffer (ready-player--active-buffer)
    (setq ready-player--last-button-focus 'play-stop)
    (ready-player--stop-playback-process))
  (ready-player--message "Stopped" 2))

(defun ready-player-play (&optional fallback-to-last)
  "Start media playback.

If FALLBACK-TO-LAST, attempt open last known file if needed."
  (interactive)
  (when (called-interactively-p #'interactive)
    (setq fallback-to-last t))
  (if (ready-player--active-buffer t)
      (with-current-buffer (ready-player--active-buffer)
        (setq ready-player--last-button-focus 'play-stop)
        (ready-player--start-playback-process))
    (if-let ((attempt-fallback fallback-to-last)
             (last-played (map-elt (ready-player--read-state)
                                   'buffer-file-name))
             ;; Avoid autoplaying here and redundantly
             ;; invoking (ready-player-play) right after.
             (buffer (let ((ready-player-autoplay nil))
                       (find-file-noselect last-played))))
        (with-current-buffer buffer
          (ready-player-play)
          (ready-player-show-info))
      (error "No file to play/stop"))))

(defun ready-player--ensure-mode ()
  "Ensure current buffer is running in `ready-player-major-mode'."
  (unless (derived-mode-p 'ready-player-major-mode)
    (user-error "Not in a ready-player-major-mode buffer (%s)" major-mode)))

(defun ready-player--stop-playback-process ()
  "Stop playback process."
  ;; Only kill the process.
  ;; The process sentinel updates the buffer status.
  (when ready-player--process
    (let ((buffer (process-buffer ready-player--process)))
      (delete-process ready-player--process)
      (kill-buffer buffer))
    (setq ready-player--process nil)))

(defun ready-player--start-playback-process ()
  "Start playback process."
  (ready-player--ensure-mode)
  (ready-player--stop-playback-process)
  (when-let* ((fpath (file-name-unquote (buffer-file-name)))
              (command (append
                        (list (format "*ready player mode '%s'*" (file-name-nondirectory fpath))
                              (ready-player--playback-process-buffer fpath))
                        (ready-player--playback-command fpath) (list fpath)))
              (buffer (current-buffer)))
    (setq ready-player--process (apply #'start-process
                                       command))
    (set-process-query-on-exit-flag ready-player--process nil)
    (ready-player--refresh-buffer-status
     buffer
     ready-player--process
     ready-player-repeat
     ready-player-shuffle
     ready-player-autoplay)
    (set-process-sentinel
     ready-player--process
     (lambda (process _)
       (when (and (memq (process-status process) '(exit signal))
                  (buffer-live-p buffer))
         (with-current-buffer buffer
           (if (and ready-player-repeat
                    (eq (process-exit-status process) 0)
                    ;; Disabling auto advance for non-audio files as
                    ;; the experience of switching between video
                    ;; window and Emacs window is currently
                    ;; uncomfortable.
                    (ready-player-is-audio-p fpath))
               (unless (ready-player--open-file-at-offset
                        (if (eq ready-player-repeat 'file)
                             0
                          1)
                        nil)
                 (ready-player--refresh-buffer-status
                  buffer
                  ready-player--process
                  ready-player-repeat
                  ready-player-shuffle
                  ready-player-autoplay))
             (setq ready-player--process nil)
             (ready-player--refresh-buffer-status
              buffer
              ready-player--process
              ready-player-repeat
              ready-player-shuffle
              ready-player-autoplay))))))
    (set-process-filter ready-player--process #'comint-output-filter)))

(defun ready-player-toggle-play-stop ()
  "Toggle play/stop of media."
  (interactive)
  (if (ready-player--active-buffer t)
      (let ((in-player (eq major-mode 'ready-player-major-mode)))
        (with-current-buffer (ready-player--active-buffer)
          (ready-player--goto-button 'play-stop)
          (if-let ((fpath (buffer-file-name)))
              (if ready-player--process
                  (if (ready-player--pausable-p)
                      (let ((paused (ready-player--toggle-pause)))
                        (when (not in-player)
                          (if paused
                              (ready-player--message "Paused" 2)
                            (ready-player-show-info))))
                    (ready-player-stop)
                    (ready-player--message "Stopped" 2))
                (ready-player-play)
                (unless in-player
                  (ready-player-show-info)))
            (error "No file to play/stop"))))
    (ready-player-play t)))

(defun ready-player--has-mpv-socket-p ()
  "Return non-nil if mpv enabled socket communication."
  (let ((process-command (ready-player--playback-command (buffer-file-name))))
    (and (equal "mpv" (seq-first process-command))
         (seq-find (lambda (param)
                     (string-match-p "--input-ipc-server" param))
                   process-command))))

(defun ready-player--pausable-p ()
  "Return non-nil if player can be paused.

Note: mpv player only at this time.

Get in touch if keen to add for other players."
  (ready-player--has-mpv-socket-p))

(defun ready-player-seek-forward (seconds)
  "Seek forward.

With prefix, seek in multiples of 60 seconds.

With numeric prefix, seek value in SECONDS.

Note: mpv player only at this time.

Get in touch if keen to add for other players."
  (interactive "P")
  (with-current-buffer (ready-player--active-buffer)
    (when ready-player--process
      (unless (ready-player--has-mpv-socket-p)
        (error "Cannot seek without an mpv socket"))
      (unless seconds
        (setq seconds 5))
      (unless (numberp seconds)
        (setq seconds (* 60 (/ (prefix-numeric-value seconds) 4))))
      (ready-player--send-command-to-socket
       (format "seek %d" seconds)
       (ready-player--socket-file))
      (let ((message-log-max nil))
      (when-let ((position (ready-player--position))
                 (duration (ready-player--duration)))
        (ready-player--message (ready-player--make-time-progress-bar position duration) 2))))))

(defun ready-player--message (text seconds)
  "Display TEXT inthe echo area for SECONDS seconds, then clear if still displayed."
  (message "%s" text)
  (run-at-time seconds nil
               (lambda (text)
                 (when (string= (current-message) text)
                   (message "")))
               text))

(defun ready-player-seek-backward (seconds)
  "Seek backward.

With prefix, seek in multiples of 60 seconds.

With numeric prefix, seek value in SECONDS.

Note: mpv player only at this time.

Get in touch if keen to add for other players."
  (interactive "P")
  (unless (ready-player--has-mpv-socket-p)
    (error "Cannot seek without an mpv socket"))
  (unless seconds
    (setq seconds 5))
  (unless (numberp seconds)
    (setq seconds (* 60 (/ (prefix-numeric-value seconds) 4))))
  (ready-player-seek-forward (- seconds)))

(defun ready-player--toggle-pause ()
  "Toggle pause.

Note: mpv player only at this time.

Get in touch if keen to add for other players."
  (unless (ready-player--has-mpv-socket-p)
    (error "Cannot pause without an mpv socket"))
  (ready-player--send-command-to-socket
   (json-encode '((command . ["cycle" "pause"])))
   (ready-player--socket-file))
  (let ((paused (ready-player--paused-p)))
    (ready-player--update-buffer
     (current-buffer) (buffer-file-name)
     (not paused)
     ready-player-repeat
     ready-player-shuffle
     ready-player-autoplay
     ready-player--thumbnail
     ready-player--metadata
     (ready-player--dired-playback-buffer))
    paused))

(defun ready-player--paused-p ()
  "Return non-nil if playback is paused.

Note: mpv player only at this time.

Get in touch if keen to add for other players."
  (with-current-buffer (ready-player--active-buffer)
    (when-let* ((has-mpv-socket (ready-player--has-mpv-socket-p))
                (response (condition-case nil
                              (json-read-from-string
                               (ready-player--send-command-to-socket
                                (json-encode '((command . ["get_property" "pause"])))
                                (ready-player--socket-file)))
                            (json-error nil))))
      ;; Response looks like:
      ;; json: {"data":false,"request_id":0,"error":"success"}
      ;; parsed: ((data . :json-false) (request_id . 0) (error . "success"))
      (and (equal (map-elt response 'data) t)
           (equal (map-elt response 'error) "success")))))

(defun ready-player--position ()
  "Return playback position.

Note: mpv player only at this time.

Get in touch if keen to add for other players."
  (with-current-buffer (ready-player--active-buffer)
    (when-let* ((has-mpv-socket (ready-player--has-mpv-socket-p))
                (response (condition-case nil
                              (json-read-from-string
                               (ready-player--send-command-to-socket
                                (json-encode '((command . ["get_property" "time-pos"])))
                                (ready-player--socket-file)))
                            (json-error nil))))
      ;; Response looks like:
      ;; json: {"data":266.182859,"request_id":0,"error":"success"}
      ;; parsed: ((data . 266.182859) (request_id . 0) (error . "success"))
      (when (map-elt response 'error)
        (map-elt response 'data)))))

(defun ready-player--duration ()
  "Return track duration.

Note: mpv player only at this time.

Get in touch if keen to add for other players."
  (with-current-buffer (ready-player--active-buffer)
    (when-let* ((has-mpv-socket (ready-player--has-mpv-socket-p))
                (response (condition-case nil
                              (json-read-from-string
                               (ready-player--send-command-to-socket
                                (json-encode '((command . ["get_property" "duration"])))
                                (ready-player--socket-file)))
                            (json-error nil))))
      ;; Response looks like:
      ;; json: {"data":266.182859,"request_id":0,"error":"success"}
      ;; parsed: ((data . 266.182859) (request_id . 0) (error . "success"))
      (when (map-elt response 'error)
        (map-elt response 'data)))))

(defun ready-player--format-time (seconds)
  "Convert SECONDS to a human readable string in the format MM:SS."
  (let* ((minutes (/ seconds 60))
         (remaining-seconds (% seconds 60)))
    (format "%02d:%02d" minutes remaining-seconds)))

(defun ready-player--make-time-progress-bar (progress total)
  "Make a bar with PROGRESS out of TOTAL."
  (setq progress (round progress))
  (setq total (round total))
  (let* ((bar-width (frame-width))
         (percentage (/ (* progress 1.0) total))
         (total-label (ready-player--format-time total))
         (label (ready-player--format-time (round progress)))
         (num-bars (round (* percentage bar-width)))
         ;; <---->  +  <-----> = 13
         ;; 00:00 ----- 00:30 ----- 01:00
         ;;                        <----> = 6
         (left-bar-width (- num-bars 13))
         (right-bar-width (- bar-width num-bars 6))
         ;; <---->     +    <----> = 12
         ;; 00:00 ---------- 01:00
         (full-bar-width (- bar-width 12)))
    (message "num-bars: %d" num-bars)
    (if (and (> left-bar-width 0)
             (> right-bar-width 0))
        (concat "00:00 "
                (propertize
                 (make-string left-bar-width ?┄)
                 'face 'font-lock-comment-face)
                " "
                label
                " "
                (propertize
                 (make-string right-bar-width ?┄)
                 'face 'font-lock-comment-face)
                " "
                total-label)
      (concat "00:00 "
              (propertize
               (make-string full-bar-width ?┄)
               'face 'font-lock-comment-face)
              " "
              total-label))))

(defun ready-player--send-command-to-socket (command socket-path)
  "Send COMMAND string to SOCKET-PATH.

Returns response string."
  (unless (string-suffix-p "\n" command)
    (setq command (concat command "\n")))
  (condition-case nil
      (with-temp-buffer
        (let ((process
               (make-network-process
                :name "ready-player send to socket"
                :buffer (current-buffer)
                :family 'local
                :service socket-path
                :coding 'utf-8
                :noquery t)))
          (process-send-string process command)
          (accept-process-output process)
          (goto-char (point-min))
          (buffer-string)))
    (error "")))

(defun ready-player-toggle-modeline ()
  "Toggle displaying the mode line."
  (interactive)
  (ready-player--ensure-mode)
  (if mode-line-format
      (progn
        (setq mode-line-format nil)
        (setq ready-player-hide-modeline t))
    (setq mode-line-format (default-value 'mode-line-format))
    (setq ready-player-hide-modeline nil)))

(defun ready-player-toggle-repeat ()
  "Cycle through repeat settings: file, directory, off."
  (interactive)
  (setq ready-player-repeat
        (cond
         ((eq ready-player-repeat 'file) 'playlist)
         ((eq ready-player-repeat 'playlist) nil)
         (t 'file)))
  (when-let ((buffer (ready-player--active-buffer)))
    (ready-player--refresh-buffer-status
     buffer
     ready-player--process
     ready-player-repeat
     ready-player-shuffle
     ready-player-autoplay))
  (ready-player--message
   (format "Repeat: %s"
           (cond
            ((eq ready-player-repeat 'file) "current file")
            ((eq ready-player-repeat 'playlist) "playlist")
            (t "off"))) 2))

(defun ready-player-toggle-shuffle ()
  "Toggle shuffle setting."
  (interactive)
  (setq ready-player-shuffle (not ready-player-shuffle))
  (when-let ((buffer (ready-player--active-buffer)))
    (ready-player--refresh-buffer-status
     buffer
     ready-player--process
     ready-player-repeat
     ready-player-shuffle
     ready-player-autoplay))
  (ready-player--message
   (format "Shuffle: %s" (if ready-player-shuffle
                             "ON"
                           "OFF")) 2))

(defun ready-player-toggle-autoplay ()
  "Toggle autoplay setting."
  (interactive)
  (setq ready-player-autoplay (not ready-player-autoplay))
  (when-let ((buffer (ready-player--active-buffer)))
    (ready-player--refresh-buffer-status
     buffer
     ready-player--process
     ready-player-repeat
     ready-player-shuffle
     ready-player-autoplay))
  (ready-player--message
   (format "Autoplay: %s" (if ready-player-autoplay
                              "ON"
                            "OFF")) 2))

(defun ready-player-reload-buffer ()
  "Reload media from file."
  (interactive)
  (ready-player--ensure-mode)
  (when (equal ready-player--thumbnail
               (ready-player--cached-thumbnail-path buffer-file-name))
    (condition-case nil
        (progn
          (delete-file ready-player--thumbnail)
          (image-flush (create-image
                        ready-player--thumbnail nil nil
                        :max-height ready-player-thumbnail-max-pixel-height)))
      (file-error nil)))
  (when (equal ready-player--metadata
               (ready-player--cached-metadata-path buffer-file-name))
    (condition-case nil
        (delete-file ready-player--metadata)
      (file-error nil)))
  (let ((playing ready-player--process)
        (dired-buffer (ready-player--dired-playback-buffer)))
    (ready-player--stop-playback-process)
    (revert-buffer nil t)
    ;; Override buffer-local dired buffer's to use existing one.
    (setq ready-player--dired-playback-buffer dired-buffer)
    ;; Refresh to ensure new dired buffer is displayed.
    (ready-player--update-buffer
     (current-buffer) (buffer-file-name)
     ready-player--process
     ready-player-repeat
     ready-player-shuffle
     ready-player-autoplay
     ready-player--thumbnail
     ready-player--metadata
     (ready-player--dired-playback-buffer))
    (when playing
      (ready-player-play)))
  (ready-player--message "Reloaded" 2))

(defun ready-player--playback-command (media-file)
  "Craft a playback command for MEDIA-FILE with first appropriate utility.

See `ready-player-open-playback-commands' for available commands.

Note: <<socket>> is expanded to socket path."
  (if-let ((command (seq-find (lambda (command)
                                (cond
                                 ((and (functionp (seq-first command))
                                       (funcall (seq-first command) media-file)
                                       (seq-elt command 1)
                                       (executable-find (seq-elt command 1)))
                                  (executable-find (seq-elt command 1)))
                                 ((and (listp (seq-first command))
                                       (seq-contains-p
                                        (seq-map #'downcase (seq-first command))
                                        (downcase (file-name-extension media-file)))
                                       (seq-elt command 1)
                                       (executable-find (seq-elt command 1)))
                                  (executable-find (seq-elt command 1)))
                                 ((stringp (seq-first command))
                                  (executable-find (seq-first command)))))
                              ready-player-open-playback-commands)))
      (mapcar (lambda (param)
                (replace-regexp-in-string "<<socket>>"
                                          (ready-player--socket-file) param))
              (if (or (functionp (car command)) (listp (car command)))
                  (cdr command)
                command))
    (user-error "No player found: %s"
                (mapconcat
                 #'identity (seq-map #'seq-first ready-player-open-playback-commands) " "))))

(defun ready-player--make-file-button-line (busy repeat shuffle autoplay)
  "Create button line with BUSY, REPEAT, AUTOPLAY, and SHUFFLE."
  (format " %s %s %s %s %s %s %s %s"
          (ready-player--make-button ready-player-previous-icon
                                     'previous
                                     #'ready-player-previous)
          (ready-player--make-button (if busy
                                         ready-player-stop-icon
                                       ready-player-play-icon)
                                     'play-stop
                                     #'ready-player-toggle-play-stop)
          (ready-player--make-button ready-player-next-icon
                                     'next
                                     #'ready-player-next)
          (ready-player--make-button ready-player-open-externally-icon
                                     'open-externally
                                     #'ready-player-open-externally)
          (ready-player--make-checkbox-button ready-player-repeat-icon repeat
                                              'repeat
                                              #'ready-player-toggle-repeat)
          (ready-player--make-checkbox-button ready-player-shuffle-icon shuffle
                                              'shuffle
                                              #'ready-player-toggle-shuffle)
          (ready-player--make-checkbox-button ready-player-autoplay-icon autoplay
                                              'autoplay
                                              #'ready-player-toggle-autoplay)
          (ready-player--make-button ready-player-search-icon
                                     'search
                                     #'ready-player-search-dired-buffer-index t)))

(defun ready-player--make-checkbox-button (text checked kind action)
  "Make a checkbox button with TEXT, CHECKED state, KIND, and ACTION."
  (propertize
   (format "%s%s"
           text
           (if checked
               "*"
             ""))
   'pointer 'hand
   'keymap (let ((map (make-sparse-keymap)))
             (define-key map [mouse-1] action)
             (define-key map (kbd "RET") action)
             (define-key map [remap self-insert-command] 'ignore)
             map)
   'button kind))

(defun ready-player--make-button (text kind action &optional no-box)
  "Make button with TEXT, KIND, ACTION and NO-BOX."
  (propertize
   (if no-box
       (format "%s" text)
     (format " %s " text))
   ;; TODO: Investigate why 'face is not enough.
   'font-lock-face (if no-box '() '(:box t))
   'pointer 'hand
   'keymap (let ((map (make-sparse-keymap)))
             (define-key map [mouse-1] action)
             (define-key map (kbd "RET") action)
             (define-key map [remap self-insert-command] 'ignore)
             map)
   'button kind))

(defun ready-player--update-buffer-name (buffer busy)
  "Rename BUFFER reflecting if BUSY playing."
  (with-current-buffer buffer
    (let ((base-name (string-remove-prefix "ready-player: "
                                           (string-remove-suffix " (playing)" (string-trim (buffer-name))))))
      (rename-buffer (if busy
                         (concat "ready-player: " base-name " (playing)")
                       (concat "ready-player: " base-name))))))

(defun ready-player--refresh-buffer-status (buffer busy repeat shuffle autoplay)
  "Refresh and render status in buffer in BUFFER.

Render FNAME, BUSY, REPEAT, SHUFFLE, and AUTOPLAY."
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
          (insert (ready-player--make-file-button-line busy repeat shuffle autoplay))))
      (goto-char saved-point)

      (ready-player--update-buffer-name buffer busy)

      (set-buffer-modified-p nil))))

(defun ready-player--cached-thumbnail-path (fpath)
  "Generate thumbnail path for media at FPATH."
  (ready-player--cached-item-path-for fpath ".png"))

(defun ready-player--cached-metadata-path (fpath)
  "Generate thumbnail path for media at FPATH."
  (ready-player--cached-item-path-for fpath ".json"))

(defun ready-player--cached-item-path-for (fpath suffix)
  "Generate cached item path for media at FPATH, appending SUFFIX."
  (let* ((temp-dir (concat (file-name-as-directory temporary-file-directory) "ready-player"))
         (temp-fpath (concat (file-name-as-directory temp-dir)
                             (md5 fpath) suffix)))
    (make-directory temp-dir t)
    temp-fpath))

(defun ready-player--socket-file ()
  "Socket file name in temp directory."
  (ready-player--temp-file "socket"))

(defun ready-player--temp-file (name)
  "Make temp file with NAME (not uniquified)."
  (file-name-concat (ready-player--temp-dir) name))

(defun ready-player--temp-dir ()
  "Get ready player's temp directory."
  (let* ((temp-dir (file-name-concat temporary-file-directory "ready-player")))
    (make-directory temp-dir t)
    temp-dir))

(defun ready-player--load-file-thumbnail-via-ffmpegthumbnailer (media-fpath on-loaded)
  "Load media thumbnail at MEDIA-FPATH and invoke ON-LOADED.

Note: This needs the ffmpegthumbnailer command line utility."
  (if (executable-find "ffmpegthumbnailer")
      (let* ((thumbnail-fpath (ready-player--cached-thumbnail-path media-fpath)))
        (make-process
         :name "ffmpegthumbnailer-process"
         :buffer (get-buffer-create "*ffmpegthumbnailer-output*")
         :command (list "ffmpegthumbnailer" "-i" (file-name-unquote media-fpath) "-s" "0" "-m" "-o" thumbnail-fpath)
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
  (let ((cache-fpath (ready-player--cached-thumbnail-path fpath)))
    (when (and (file-exists-p cache-fpath)
               (> (file-attribute-size (file-attributes cache-fpath)) 0))
      cache-fpath)))

(defun ready-player--local-thumbnail-in-directory (dir)
  "Return local thumbnail if found in DIR."
  (let ((candidates '("cover.jpg" "cover.png"
                      "front.jpg" "front.png"
                      "folder.jpg" "folder.png"
                      "album.jpg" "album.png"
                      "artwork.jpg" "artwork.png"))
        thumbnail)
    (catch 'found
      (dolist (candidate candidates)
        (setq candidate (expand-file-name candidate dir))
        (when (and (file-exists-p candidate)
                   (> (file-attribute-size (file-attributes candidate)) 0))
          (setq thumbnail candidate)
          (throw 'found thumbnail))))
    thumbnail))

(defun ready-player--cached-metadata (fpath)
  "Get cached thumbnail for media at FPATH."
  (setq fpath (file-name-unquote fpath))
  (let ((cache-fpath (ready-player--cached-metadata-path fpath)))
    (when (and (file-exists-p cache-fpath)
               (> (file-attribute-size (file-attributes cache-fpath)) 0))
      (with-temp-buffer
        (insert-file-contents cache-fpath)
        (goto-char (point-min))
        (json-parse-buffer :object-type 'alist)))))

(defun ready-player--load-file-thumbnail-via-ffmpeg (media-fpath on-loaded)
  "Load media thumbnail at MEDIA-FPATH and invoke ON-LOADED.

Note: This needs the ffmpeg command line utility."
  (setq media-fpath (file-name-unquote media-fpath))
  (if (executable-find "ffmpeg")
      (let* ((thumbnail-fpath (ready-player--cached-thumbnail-path media-fpath)))
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

(defun ready-player--load-file-metadata (fpath &optional on-loaded)
  "Load media metadata at FPATH synchronously.

If ON-LOADED is non-nil, load and invoke asynchronously."
  (setq fpath (file-name-unquote fpath))
  (if (executable-find "ffprobe")
      (if on-loaded
          (when-let* ((buffer (generate-new-buffer "*ffprobe-output*"))
                      (buffer-live (buffer-live-p buffer))
                      (metadata-fpath (ready-player--cached-metadata-path fpath)))
            (with-current-buffer buffer
              (erase-buffer))
            (make-process
             :name "ffprobe-process"
             :buffer buffer
             :command (list "ffprobe" "-v" "quiet" "-print_format" "json" "-show_format" "-show_streams" fpath)
             :sentinel
             (lambda (process _)
               (condition-case _
                   (when (and (eq (process-exit-status process) 0)
                              (buffer-live-p (process-buffer process)))
                     (with-current-buffer (process-buffer process)
                       ;; Using write-region to avoid "Wrote" echo message.
                       (write-region (point-min) (point-max) metadata-fpath nil 'noprint)
                       (goto-char (point-min))
                       (funcall on-loaded (json-parse-buffer :object-type 'alist))))
                 (error nil))
               (kill-buffer (process-buffer process)))))
        (with-temp-buffer
            (let ((inhibit-read-only t)
                  (metadata))
              (erase-buffer)
              (if (eq 0 (call-process "ffprobe" nil (current-buffer) nil "-v" "quiet" "-print_format" "json" "-show_format" "-show_streams" fpath))
                  (progn
                    (goto-char (point-min))
                    (setq metadata (json-parse-buffer :object-type 'alist)))
                (message "%s" (buffer-string)))
              metadata)))
    (message "Metadata not available (ffprobe not found)")))

(defun ready-player--playback-process-buffer (fpath)
  "Get the process playback buffer for FPATH."
  (when-let* ((buffer (get-buffer-create
                       (format "*%s %s* (ready-player)"
                               (nth 0 (ready-player--playback-command fpath))
                               (file-name-nondirectory fpath))))
              (buffer-live (buffer-live-p buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer))
      (comint-mode))
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

(defun ready-player-view-dired-playback-buffer ()
  "View associated `dired' playback buffer."
  (interactive)
  (ready-player--ensure-mode)
  (let ((media-file (buffer-file-name)))
    (display-buffer (ready-player--dired-playback-buffer)
                    ready-player-display-dired-playback-buffer-display-action)
    (switch-to-buffer-other-window (ready-player--dired-playback-buffer))
    (dired-goto-file media-file)))

(defun ready-player--row-value (rows label)
  "Resolve LABEL in ROWS to row value."
  (map-elt (seq-find (lambda (item)
                       (string= (cdr (assoc 'label item)) label))
                     rows)
           'value))

(defun ready-player-lookup-song ()
  "Look up current song on Discogs."
  (interactive)
  (with-current-buffer (ready-player--active-buffer)
    (let ((url "https://www.discogs.com/search/?type=all")
          (title (ready-player--row-value
                  (ready-player--make-metadata-rows ready-player--metadata)
                  "Title:"))
          (artist (ready-player--row-value
                   (ready-player--make-metadata-rows ready-player--metadata)
                   "Artist:")))
      (unless (or title artist)
        (error "No metadata available"))
      (when title
        (setq url (concat url "&title=" title)))
      (when artist
        (setq url (concat url "&artist=" artist)))
      (browse-url url))))

(defun ready-player--download-musicbrainz-album-artwork (artist album &optional to)
  "Download album artwork for ARTIST ALBUM from Internet Archive/MusicBrainz.

If TO is non-nil, save to that location.  Otherwise generate location."
  (when-let* ((musicbrainz-id-url
               (concat "https://musicbrainz.org/ws/2/release-group/?query=artist:"
                       (url-hexify-string artist)
                       "%20release:"
                       (url-hexify-string album)
                       "&fmt=json"))
              (json (with-current-buffer (url-retrieve-synchronously musicbrainz-id-url)
                      (goto-char (point-min))
                      (re-search-forward "^$")
                      (json-read)))
              (release-groups (cdr (assq 'release-groups json)))
              (found (> (length release-groups) 0))
              (musicbrainz-id (cdr (assq 'id (aref release-groups 0))))
              (cover-url (concat "https://coverartarchive.org/release-group/"
                                 (url-hexify-string musicbrainz-id) "/front"))
              (destination (or to (make-temp-file (replace-regexp-in-string
                                                   "/" "_" (concat artist "-" album)) nil ".jpg")))
              (downloaded (progn
                            (when (and to (file-exists-p to))
                              (unless (y-or-n-p (format "Override \"%s\"? " to))
                                (keyboard-quit)))
                            (ready-player--url-copy-file cover-url destination))))
    destination))

(defun ready-player--download-itunes-album-artwork (artist album &optional to)
  "Download album artwork for ARTIST ALBUM from iTunes.

If TO is non-nil, save to that location.  Otherwise generate location."
  (when-let* ((search-url
               (concat "https://itunes.apple.com/search?term="
                       (url-hexify-string (concat artist " " album))
                       "&entity=album&limit=1"))
              (json (with-current-buffer (url-retrieve-synchronously search-url)
                      (goto-char (point-min))
                      (re-search-forward "^$")
                      (let ((json-object-type 'alist))
                        (json-read))))
              (results (cdr (assq 'results json)))
              (cover-url (progn
                           (when (seq-empty-p results)
                             (error "No album art found"))
                           (replace-regexp-in-string
                            "100x100bb" "600x600bb"
                            (cdr (assq 'artworkUrl100 (aref results 0))))))
              (destination (or to (make-temp-file (replace-regexp-in-string
                                                   "/" "_" (concat artist "-" album)) nil ".jpg")))
              (downloaded (progn
                            (when (and to (file-exists-p to))
                              (unless (y-or-n-p (format "Override \"%s\"? " to))
                                (keyboard-quit)))
                            (ready-player--url-copy-file cover-url destination))))
    destination))

(defun ready-player--set-media-file-artwork (media-file artwork-file)
  "Set MEDIA-FILE's artwork to ARTWORK-FILE."
  (setq artwork-file (file-name-unquote artwork-file))
  (setq media-file (file-name-unquote media-file))
  (unless (executable-find "ffmpeg")
    (user-error "Ffmpeg not available (please install)"))
  (let* ((buffer (get-buffer-create "*ffmpeg output*"))
         (temp-extension (concat ".tmp." ;; eg. .tmp.mp3
                                 (file-name-extension media-file)))
         (temp-file (ready-player--unique-new-file-path
                     (concat media-file temp-extension)))
         (backup-file (ready-player--unique-new-file-path (concat media-file ".bak")))
         (status))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq status
              (call-process "ffmpeg" nil buffer nil
                            "-i" media-file "-i" artwork-file
                            "-map_metadata" "0" "-map" "0:a" "-map" "1" "-c" "copy"
                            "-disposition:v:0" "attached_pic" temp-file))
        (read-only-mode +1)))
    (if (eq status 0)
        (progn
          (rename-file media-file backup-file)
          (rename-file temp-file media-file))
      (display-buffer buffer)
      (switch-to-buffer-other-window buffer))))

(defun ready-player--unique-new-file-path (file-path)
  "Return a unique FILE-PATH alternative if it already exists.
\"/tmp/blah.txt\" -> \"/tmp/blah(1).txt\"
\"/tmp/blah\" -> \"/tmp/blah(1)\""
  (let ((counter 1)
        (name (file-name-sans-extension file-path))
        (extension (file-name-extension file-path)))
    (while (file-exists-p file-path)
      (if extension
          (setq file-path (format "%s(%d).%s" name counter extension))
        (setq file-path (format "%s(%d)" name counter)))
      (setq counter (1+ counter)))
    (expand-file-name file-path)))

(defun ready-player-set-album-artwork ()
  "Select image and set as album artwork."
  (interactive)
  (let ((artwork-path (read-file-name "Select artwork image: "
                                      nil nil t)))
    (unless (file-regular-p artwork-path)
      (user-error "Not a file"))
    (cond ((eq major-mode 'ready-player-major-mode)
           (ready-player--set-media-file-artwork buffer-file-name artwork-path)
           (ready-player-reload-buffer))
          ((eq major-mode 'dired-mode)
           (mapc (lambda (file)
                   (ready-player--set-media-file-artwork file artwork-path))
                 (dired-get-marked-files))
           (revert-buffer)))))

(defun ready-player--url-copy-file (url newname)
  "Like `url-copy-file', using URL and NEWNAME but with status check."
  (defvar url-http-end-of-headers) ;; Silence warning.
  (let* ((buffer (url-retrieve-synchronously url))
         (written))
    (with-current-buffer buffer
      (goto-char url-http-end-of-headers)
      (when (equal (url-http-parse-response) 200)
        (goto-char (1+ url-http-end-of-headers))
        (write-region (point) (point-max) newname)
        (setq written newname)))
    (kill-buffer buffer)
    written))

(defun ready-player-download-album-artwork-and-set-metadata ()
  "Download album artwork set media metadata.

Note: Can be invoked from either `dired' or `ready-player-major-mode' buffers."
  (interactive)
  (ready-player--download-album-artwork t))

(defun ready-player-download-album-artwork (prefix)
  "Download album artwork to media directory.

With PREFIX, ask user to provide artist and album.

Note: Can be invoked from either `dired' or `ready-player-major-mode' buffers."
  (interactive "P")
  (ready-player--download-album-artwork nil prefix))

(defun ready-player--download-album-artwork (&optional set-media-file ask-user)
  "Download album artwork to media directory.

With non-nil SET-MEDIA-FILE, modify media file instead of downloading

With non-nil ASK-USER, ask user to supply artist and album name.
to directory."
  (let* ((media-file (cond ((eq major-mode 'ready-player-major-mode)
                            buffer-file-name)
                           ((eq major-mode 'dired-mode)
                            (if-let ((files (dired-get-marked-files))
                                     (is-single (eq 1 (seq-length files))))
                                (seq-first files)
                              (error "Select a single file only")))
                           (t
                            (error "This buffer is not supported"))))
         (metadata (ready-player--load-file-metadata media-file))
         (fetcher (if (equal "Apple iTunes"
                             (completing-read "Download from: "
                                              '("Apple iTunes"
                                                "Internet Archive / MusicBrainz") nil t))
                      #'ready-player--download-itunes-album-artwork
                    #'ready-player--download-musicbrainz-album-artwork))
         (artist (or (when ask-user
                       (read-string "Artist: "))
                     (ready-player--row-value
                      (ready-player--make-metadata-rows metadata)
                      "Artist:")
                     (error "No artist available")))
         (album (or (when ask-user
                      (read-string "Album: "))
                    (ready-player--row-value
                     (ready-player--make-metadata-rows metadata)
                     "Album:")
                    (error "No album available")))
         (temp-file (progn
                      (redisplay) ;; Hides completing-read before blocking call.
                      (funcall fetcher artist album)))
         (counterpart-artwork (ready-player--unique-new-file-path
                               (concat (file-name-sans-extension media-file) ".jpg")))
         (directory-artwork (ready-player--unique-new-file-path
                             (file-name-concat default-directory "artwork.jpg")))
         (buffer (if temp-file
                     (display-image-in-temp-buffer temp-file)
                   (error "No artwork found")))
         (override)
         (reveal-file))
    (unless buffer
      (error "Couldn't display image"))
    (if set-media-file
        (progn
          (when (y-or-n-p (format "Override \"%s\" artwork? " (file-name-nondirectory media-file)))
            (ready-player--set-media-file-artwork media-file temp-file)
            (setq override t))
          (when (or override (y-or-n-p (format "Keep \"%s\"? " (file-name-nondirectory counterpart-artwork))))
            (rename-file temp-file counterpart-artwork)
            (setq reveal-file counterpart-artwork)))
      (when (y-or-n-p (format "Save \"%s\"? " (file-name-nondirectory directory-artwork)))
        (rename-file temp-file directory-artwork t)
        (setq reveal-file directory-artwork)))
    ;; Image buffer is already focused
    (quit-window t)
    (cond ((and reveal-file (eq major-mode 'dired-mode))
           (dired-jump nil reveal-file))
          ((and override (eq major-mode 'ready-player-major-mode))
           (ready-player-reload-buffer)))))

(defun ready-player-load-m3u-playlist ()
  "Load an .m3u playlist."
  (interactive)
  (let* ((m3u-path (read-file-name "find m3u: " nil nil t nil
                                   (lambda (name)
                                     (or (string-match "\\.m3u\\'" name)
                                         (file-directory-p name)))))
         (media-files (if (string-match "\\.m3u\\'" m3u-path)
                          (ready-player--media-at-m3u-file m3u-path)
                        (error "Not a .m3u file")))
         ;; Find a common parent via completion.
         (default-directory (file-name-directory
                             (try-completion "" media-files)))
         (m3u-fname (file-name-nondirectory m3u-path))
         (dired-buffer-name (format "*%s*" m3u-fname))
         (dired-buffer (dired (append (list dired-buffer-name)
                                      (mapcar (lambda (path)
                                                (file-relative-name path default-directory))
                                              media-files)))))
    (ready-player-load-dired-buffer dired-buffer)))

(defun ready-player--media-at-m3u-file (m3u-path)
  "Read m3u playlist at M3U-PATH and return files."
  (with-temp-buffer
    (insert-file-contents m3u-path)
    (let ((files))
      (while (re-search-forward
              (rx bol (not (any "#" space))
                  (zero-or-more (not (any "\n")))
                  eol) nil t)
        (when (file-exists-p (match-string 0))
          (push (match-string 0) files)))
      (nreverse files))))

(defun display-image-in-temp-buffer (image-path)
  "Display the image at IMAGE-PATH in a temporary, read-only buffer."
  (let ((buffer (get-buffer-create "*album cover*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (img (create-image
                  image-path nil nil
                  :max-height ready-player-thumbnail-max-pixel-height)))
        (erase-buffer)
        (insert-image img)
        (goto-char (point-min))
        (read-only-mode 1)))
    (display-buffer buffer ready-player-display-dired-playback-buffer-display-action)
    (switch-to-buffer-other-window buffer)
    buffer))

(defun ready-player-load-dired-buffer (&optional dired-buffer)
  "Load a `dired' buffer.

If DIRED-BUFFER is nil, offer to pick one.

`dired' buffers typically show a directory's content, but they can
also show the output of a shell command.  For example, `find-dired'.

`ready-player-load-dired-buffer' can open any `dired' buffer for
playback."
  (interactive)
  (let* ((dired-buffer (or dired-buffer
                           (completing-read "Open 'dired' buffer: "
                                            (or (seq-map
                                                 (lambda (buffer)
                                                   (buffer-name buffer))
                                                 (seq-filter (lambda (buffer)
                                                               (with-current-buffer buffer
                                                                 (derived-mode-p 'dired-mode)))
                                                             (buffer-list)))
                                                ;; TODO: Open find-file to open a dired buffer
                                                (error "No `dired' buffers available")) nil t)))
         (media-file (if (buffer-live-p (get-buffer dired-buffer))
                         (ready-player--next-dired-file-from
                          nil 1 t ready-player-shuffle (get-buffer dired-buffer))
                       (error "dired buffer not found")))
         (media-buffer (if media-file
                           (progn
                             (when-let ((existing-player (ready-player--active-buffer t)))
                               (kill-buffer existing-player))
                             ;; Momentarily override highest priority buffer
                             (let ((ready-player--highest-priority-dired-buffer
                                    (get-buffer dired-buffer)))
                               (find-file-noselect media-file)))
                         (error "No media found"))))
    (unless media-buffer
      (error "No media found"))
    (unless (eq (current-buffer) media-buffer)
      (switch-to-buffer media-buffer))))

(defun ready-player-load-directory ()
  "Load all media from directory (experimental).

If point is on a directory in a `dired' mode, offer to load it.
Otherwise browse to select a different directory to load."
  (interactive)
  (let* ((current-buffer (current-buffer))
         (directory (if-let* ((in-dired-mode (eq major-mode 'dired-mode))
                              (files (dired-get-marked-files))
                              (selection (progn
                                           (unless (eq 1 (seq-length files))
                                             (error "Select a single directory"))
                                           (seq-first files)))
                              (is-on-dir (file-directory-p selection))
                              (should-load (y-or-n-p (format "Load \"%s\"? "
                                                             (file-name-nondirectory
                                                              (string-remove-suffix "/" selection))))))
                        selection
                      (string-remove-suffix "/" (read-directory-name "Load directory: "))))
         (new-name (concat "*" (file-name-base directory) "*"))
         (progress-reporter (make-progress-reporter "Loading"))
         (dired-buffer)
         (hook))
    (setq hook (lambda (_beg _end _len)
                 (with-current-buffer dired-buffer)
                 (save-excursion
                   (progress-reporter-update progress-reporter)
                   (goto-char (point-max))
                   (when (string-prefix-p
                          "find finished"
                          (string-trim
                           (buffer-substring
                            (line-beginning-position)
                            (line-end-position))))
                     (remove-hook 'after-change-functions hook t)
                     (progress-reporter-done progress-reporter)
                     (ready-player-load-dired-buffer dired-buffer)))))
    (if (or ready-player-always-load-directory-recursively
            (y-or-n-p "Load recursively? "))
        (progn
          (when (buffer-live-p (get-buffer new-name))
            (kill-buffer new-name))
          (find-dired directory
                      (string-join (mapcar (lambda (ext)
                                             (format "-iname \\*.%s" ext))
                                           (ready-player-supported-media)) " -o "))
          ;; find-dired pops to dired buffer same window. Save it.
          (setq dired-buffer (current-buffer))
          (rename-buffer new-name)
          ;; Now switch back to existing buffer while busy finding.
          (when (buffer-live-p (get-buffer current-buffer))
            (switch-to-buffer current-buffer))
          (with-current-buffer dired-buffer
            (add-hook 'after-change-functions hook nil t)))
      (ready-player-load-dired-buffer
       (ready-player--find-file-noselect directory)))))

(defun ready-player--clean-up ()
  "Kill playback process."
  (ready-player--ensure-mode)
  (when-let ((delete-cached-file (not ready-player-cache-thumbnails)))
    (condition-case nil
        (delete-file ready-player--thumbnail)
      (file-error nil)))
  (when-let* ((delete-cached-file (not ready-player-cache-metadata))
              (metadata-path (ready-player--cached-metadata-path (buffer-file-name))))
    (condition-case nil
        (delete-file metadata-path)
      (file-error nil)))
  (kill-buffer (ready-player--playback-process-buffer (buffer-file-name)))
  (when ready-player--process
    (delete-process ready-player--process)
    (setq ready-player--process nil)))

(defun ready-player--buffers ()
  "Get all `ready-player-major-mode' buffers."
  (seq-filter (lambda (buffer)
                (eq (buffer-local-value 'major-mode buffer)
                    'ready-player-major-mode))
              (buffer-list)))

(defun ready-player--dired-buffers (&optional prioritized-dired-buffer)
  "Get all `ready-player-major-mode' `dired' buffers.

When PRIORITIZED-DIRED-BUFFER is non-nil return as the first buffer in list."
  (let ((dired-buffers '())
        (known-dired-buffers '())
        (result))
    (dolist (buffer (buffer-list) dired-buffers)
      (cond ((and (eq (buffer-local-value 'major-mode buffer)
                      'ready-player-major-mode)
                  (with-current-buffer buffer
                    (buffer-live-p ready-player--dired-playback-buffer)))
             (setq dired-buffers (append dired-buffers
                                         (list (with-current-buffer buffer
                                                 ready-player--dired-playback-buffer))))
             (setq known-dired-buffers (append known-dired-buffers
                                               (list (with-current-buffer buffer
                                                       ready-player--dired-playback-buffer)))))
            ((eq (buffer-local-value 'major-mode buffer)
                 'dired-mode)
             (setq dired-buffers (append dired-buffers
                                         (list buffer))))))
    (setq result (append
                  (when prioritized-dired-buffer
                    (list prioritized-dired-buffer))
                  known-dired-buffers
                  dired-buffers))
    ;; For debugging
    ;; (message "===============")
    ;; (message "Buffers:")
    ;; (mapc (lambda (buffer)
    ;; (message "%s" buffer)) result)
    ;; (message "===============")
    result))

(defun ready-player--resolve-file-dired-buffer (file &optional prioritized-dired-buffer)
  "Return a known `dired' buffer containing FILE or nil otherwise.

When PRIORITIZED-DIRED-BUFFER is non-nil give highest resolutin priority
to this `dired' buffers."
  (seq-find
   (lambda (buffer)
     (with-current-buffer buffer
       (dired-goto-file file)))
   (ready-player--dired-buffers prioritized-dired-buffer)))

(defun ready-player--keep-only-this-buffer (buffer)
  "Keep this BUFFER and kill all other `ready-player-mode' buffers."
  (when (buffer-live-p buffer)
    (mapc (lambda (other-buffer)
            (when (not (eq buffer other-buffer))
              (when (get-buffer-window-list other-buffer nil t)
                (set-window-buffer
                 (car (get-buffer-window-list other-buffer nil t))
                 buffer))
              (kill-buffer other-buffer)))
          (ready-player--buffers))))

(defun ready-player--active-buffer (&optional no-error)
  "Get the active buffer.

Fails if none available unless NO-ERROR is non-nil."
  (cond ((eq major-mode #'ready-player-major-mode)
         (setq ready-player--active-buffer (current-buffer))
         ready-player--active-buffer)
        ((and ready-player--active-buffer
              (buffer-live-p ready-player--active-buffer))
         ready-player--active-buffer)
        (no-error
         nil)
        (t
         (error "No ready-player buffer available"))))

(defun ready-player--index-dired-buffer (dired-buffer)
  "Index DIRED-BUFFER (experimental)."
  (let* ((new-dired-hash (md5 (with-current-buffer dired-buffer
                                (buffer-string))))
         (hash-path (ready-player--index-hash-path))
         (old-dired-hash (when (file-exists-p hash-path)
                           (with-temp-buffer
                             (insert-file-contents hash-path)
                             (string-trim (buffer-string)))))
         (destination-path (ready-player--index-path))
         ;; Source contains the list of all files.
         (source-path (ready-player--source-index-path))
         (source-path-temp (concat source-path ".tmp"))
         (now (current-time))
         (buffer (get-buffer-create
                  (format "Media indexing (%s)" (buffer-name dired-buffer)))))
    (unless (equal new-dired-hash old-dired-hash)
      (when-let* ((existing-process (get-buffer-process buffer)))
        (delete-process existing-process))
      (delete-file source-path-temp)
      (with-current-buffer buffer
        (erase-buffer))
      (unless (ready-player--dump-media-files-from-dired-buffer
               dired-buffer source-path-temp)
        (error "Nothing to index"))
      (if (executable-find "ffprobe")
          (set-process-sentinel
           (start-process
            "media-indexing"
            buffer
            (file-truename (expand-file-name invocation-name
                                             invocation-directory))
            "--quick" "--batch" "--eval"
            (prin1-to-string
             `(progn
                (interactive)
                (require 'cl-lib)
                (require 'seq)
                (require 'map)
                (defun parse-tags (path)
                  (setq path (file-name-unquote path))
                  (with-temp-buffer
                    (if (eq 0 (call-process "ffprobe" nil t nil "-v" "quiet"
                                            "-print_format" "json" "-show_format" path))
                        (map-elt (json-parse-string (buffer-string)
                                                    :object-type 'alist)
                                 'format)
                      (message "Warning: Couldn't read track metadata for %s" path)
                      (message "Only found:\n%s" (buffer-string))
                      (list (cons 'filename path)))))
                (let* ((paths (with-temp-buffer
                                (insert-file-contents ,source-path-temp)
                                (split-string (buffer-string) "\n" t)))
                       (total (length paths))
                       (n 0)
                       (records (seq-map (lambda (path)
                                           (let ((tags (parse-tags path)))
                                             (message "%d/%d %s" (setq n (1+ n))
                                                      total (or (map-elt (map-elt tags 'tags) 'title) "No title"))
                                             tags))
                                         paths)))
                  (rename-file ,source-path-temp ,source-path t)
                  (delete-file ,destination-path)
                  (with-temp-buffer
                    (prin1 (list
                            (cons 'index records)
                            (cons 'dired-hash ,new-dired-hash)) (current-buffer))
                    (write-file ,destination-path nil))
                  (with-temp-file ,hash-path
                    (insert ,new-dired-hash))))))
           (lambda (process _)
             ;; Ignore mentioning issues if process was killed.
             (unless (equal (process-status process) 'signal)
               (if (= (process-exit-status process) 0)
                   (when-let ((time (float-time (time-subtract (current-time) now))))
                     (with-current-buffer buffer
                       (goto-char (point-max))
                       (insert (format "Finished in %.3f seconds" time))))
                 (message "Failed media indexing, see %s" buffer)))))
        (message "Indexing not available (ffprobe not found)")))))

(defun ready-player--dump-media-files-from-dired-buffer (dired-buffer destination)
  "Dump all files in DIRED-BUFFER to a text file at DESTINATION."
  (with-current-buffer dired-buffer
    (let ((regexp (regexp-opt (ready-player--supported-media-with-uppercase) t))
          (temp-buffer (get-buffer-create "*dired-media-files*")))
      (with-current-buffer temp-buffer
        (erase-buffer))
      (delete-file destination)
      (save-excursion
        (goto-char (point-min))
        (while (= 0 (forward-line))
          (when-let* ((file (dired-get-filename nil t))
                      (extension (file-name-extension file))
                      (match-p (string-match-p regexp extension)))
            (with-current-buffer temp-buffer
              (insert file "\n")))))
      (with-current-buffer temp-buffer
        ;; Using write-region to avoid "Wrote" echo message.
        (write-region (point-min) (point-max) destination nil 'noprint)
        (kill-buffer temp-buffer)))
    (file-exists-p destination)))

(defun ready-player--index-hash-path ()
  "Generate an index for DIRED-BUFFER."
  (ready-player--cached-item-path-for
   (ready-player--dired-buffer-index-identifier) "_index.diredhash"))

(defun ready-player--index-path ()
  "Generate an index for DIRED-BUFFER."
  (ready-player--cached-item-path-for
   (ready-player--dired-buffer-index-identifier) "_index.el"))

(defun ready-player--dired-buffer-index-identifier ()
  "Return an identifier for assciated `dired' buffer."
  (with-current-buffer (ready-player--active-buffer)
    (with-current-buffer (ready-player--dired-playback-buffer)
      (concat (dired-current-directory) (buffer-name)))))

(defun ready-player--source-index-path ()
  "Generate a source index for DIRED-BUFFER."
  (ready-player--cached-item-path-for
   (ready-player--dired-buffer-index-identifier) "_source_index.txt"))

(defun ready-player-search-dired-buffer-index ()
  "Search the `dired' playlist for playback (experimental)."
  (interactive)
  (let ((in-player (eq major-mode 'ready-player-major-mode)))
    (let* ((title-width (max 25 (round (* (- (window-width) 9) 0.3))))
           (artist-width (round (* (- (window-width) 9) 0.25)))
           (album-width (- (window-width) 9 title-width artist-width))
           (index (map-elt (ready-player--dired-buffer-index) 'index))
           (tracks (progn
                     (when (seq-empty-p index)
                       (error "No index available"))
                     (mapcar
                      (lambda (track)
                        (let-alist track
                          ;; Multi-line
                          ;; (format "%s\n%s %s%s"
                          ;;         (or .tags.title (file-name-base .filename) "No title")
                          ;;         (propertize (or .tags.artist "") 'face 'font-lock-string-face)
                          ;;         (propertize (or .tags.album "") 'face 'font-lock-variable-name-face)
                          ;;         (propertize (format "file:%s" .filename) 'invisible t))
                          (format "%s   %s   %s%s"
                                  (truncate-string-to-width
                                   (or .tags.title
                                       (file-name-base .filename)
                                       "No title") title-width nil ?\s "…")
                                  (truncate-string-to-width (propertize (or .tags.artist "")
                                                                        'face 'font-lock-string-face) artist-width nil ?\s "…")
                                  (truncate-string-to-width
                                   (propertize (or .tags.album "")
                                               'face 'font-lock-variable-name-face) album-width nil ?\s "…")
                                  (propertize (format "file:%s" .filename) 'invisible t))))
                      index)))
           (selection (completing-read "Play: " tracks nil t))
           (file (with-temp-buffer
                   (insert (nth 1 (split-string selection "file:")))
                   (buffer-substring-no-properties (point-min) (point-max)))))
      (if (ready-player--active-buffer t)
          (ready-player--open-file file (ready-player--active-buffer) t)
        (ready-player--open-file file (find-file file) t))
      (unless in-player
        (ready-player-show-info)))))

(defun ready-player--dired-buffer-index ()
  "Load and return the associated index if one is found."
  ;; Attempt access to full index.
  (if (file-exists-p (ready-player--index-path))
      (with-temp-buffer
        (insert-file-contents (ready-player--index-path))
        (read (current-buffer)))
    (with-temp-buffer
      (cond ((file-exists-p (ready-player--source-index-path))
             ;; Attempt access to full index sources.
             (insert-file-contents (ready-player--source-index-path)))
            ((file-exists-p (concat (ready-player--source-index-path) ".tmp"))
             ;; Attempt access to partial index sources.
             (insert-file-contents (concat (ready-player--source-index-path) ".tmp"))))
      (let (result)
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
            (push (list (cons 'filename line)) result))
          (forward-line 1))
        (list
         (cons 'index (nreverse result)))))))

(provide 'ready-player)

;;; ready-player.el ends here
