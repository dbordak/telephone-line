;;; telephone-line-segments.el --- Segments for Telephone Line -*- lexical-binding: t -*-

;; Copyright (C) 2015-2022 Daniel Bordak

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Segments for Telephone Line.
;; To create your own, look at the functions defined in telephone-line-utils.el

;;; Code:

(require 'telephone-line-utils)

(telephone-line-defsegment* telephone-line-vc-segment ()
  (telephone-line-raw vc-mode t))

(telephone-line-defsegment telephone-line-process-segment ()
  mode-line-process)

(telephone-line-defsegment* telephone-line-position-segment ()
  (telephone-line-raw
   (if (eq major-mode 'paradox-menu-mode)
       ;;Paradox fills this with position info.
       mode-line-front-space
     mode-line-position) t))

(telephone-line-defsegment* telephone-line-airline-position-segment (&optional lines columns)
  "Position segment imitating vim-airline's appearance. Optional args set padding on lines/columns."
  (let* ((l (number-to-string (if lines lines 4)))
         (c (number-to-string (if columns columns 3))))
    (if (eq major-mode 'paradox-menu-mode)
        (telephone-line-raw mode-line-front-space t)
      `((-3 "%p") ,(concat " %" l "l"
                           ":%" c (if (bound-and-true-p column-number-indicator-zero-based) "c" "C"))))))

(telephone-line-defsegment* telephone-line-misc-info-segment ()
  (telephone-line-raw mode-line-misc-info t))

(telephone-line-defsegment* telephone-line-buffer-segment ()
  `(""
    mode-line-mule-info
    mode-line-modified
    mode-line-client
    mode-line-remote
    mode-line-frame-identification
    ,(telephone-line-raw mode-line-buffer-identification t)))

(telephone-line-defsegment* telephone-line-input-info-segment ()
  '("" mode-line-mule-info mode-line-modified mode-line-client mode-line-remote))

(telephone-line-defsegment* telephone-line-atom-eol-segment (&optional hide-lf)
  "Displays the eol style of the buffer the same way Atom does.
Set HIDE-LF to display nothing for unix endings, as it can be an assumed default.
Adapted from doom-modeline."
  (pcase (coding-system-eol-type buffer-file-coding-system)
    (0 (if hide-lf nil
         "LF"))
    (1 "CRLF")
    (2 "CR")))

(telephone-line-defsegment* telephone-line-atom-encoding-segment (&optional hide-utf8)
  "Displays the encoding of the buffer the same way Atom does.
Set HIDE-UTF8 to display nothing for UTF-8, as it can be an assumed default.
Adapted from doom-modeline."
  (let ((sys (coding-system-plist buffer-file-coding-system)))
    (cond ((memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
           (if hide-utf8 nil
             "UTF-8"))
          (t (upcase (symbol-name (plist-get sys :name)))))))

(telephone-line-defsegment* telephone-line-filesize-segment ()
  "%I")

(telephone-line-defsegment* telephone-line-simple-major-mode-segment ()
  "%[%m%]")

(telephone-line-defsegment* telephone-line-simple-minor-mode-segment ()
  (telephone-line-raw minor-mode-alist t))

(telephone-line-defsegment* telephone-line-minions-mode-segment ()
  (telephone-line-raw minions-mode-line-modes t))

;; For a file like /a/b/c/file.txt, this should display
;; file.txt
(telephone-line-defsegment telephone-line-buffer-name-segment ()
  mode-line-buffer-identification)

;; For a file like /a/b/c/file.txt, this should display
;; /a/b/c/file.txt
(telephone-line-defsegment telephone-line-file-name-absolute-path-segment ()
  buffer-file-name)

(telephone-line-defsegment* telephone-line-buffer-modified-segment ()
    (if (buffer-modified-p)
        (telephone-line-raw "!")
      (telephone-line-raw "-")))

(telephone-line-defsegment telephone-line-narrow-segment ()
  "%n")

(telephone-line-defsegment* telephone-line-major-mode-segment ()
  (let ((recursive-edit-help-echo "Recursive edit, type C-M-c to get out"))
    `((:propertize "%[" help-echo ,recursive-edit-help-echo face ,face)
      (:propertize ("" mode-name)
                   help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
                   mouse-face mode-line-highlight
                   local-map ,mode-line-major-mode-keymap
                   face ,face)
      (:propertize "%]" help-echo ,recursive-edit-help-echo face ,face))))

(telephone-line-defsegment telephone-line-minor-mode-segment ()
  `((:propertize ("" minor-mode-alist)
                 mouse-face mode-line-highlight
                 help-echo "Minor mode\n\
mouse-1: Display minor mode menu\n\
mouse-2: Show help for minor mode\n\
mouse-3: Toggle minor modes"
                 local-map ,mode-line-minor-mode-keymap
                 face ,face)
    (:propertize "%n"
                 mouse-face mode-line-highlight
                 help-echo "mouse-2: Remove narrowing from buffer"
                 local-map ,(make-mode-line-mouse-map
                             'mouse-2 #'mode-line-widen)
                 face ,face)))

(defun telephone-line--hud-axis-func (y)
  "Generate a HUD axis value given the current position Y."
  (let* ((height (or telephone-line-height (frame-char-height)))
         (start (floor (* height (float (window-start))) (point-max)))
         (end (ceiling (* height (float (window-end))) (point-max))))
    (if (<= start y end) 1 0)))

(defclass telephone-line--hud (telephone-line-separator)
  ((axis-func :initarg :axis-func :initform #'telephone-line--hud-axis-func)
   (axis-init :initarg :axis-init
              :initform (lambda (height) (number-sequence 0 (- height 1))))
   (pattern-func :initarg :pattern-func :initform #'telephone-line-row-pattern-binary)
   (image-cache :initform (make-hash-table :test 'equal :size 100))))

(cl-defmethod telephone-line-separator-render-image ((obj telephone-line--hud) foreground background)
  "Find cached pbm of OBJ in FOREGROUND and BACKGROUND.
If it doesn't exist, create and cache it."
  (let* ((height (or telephone-line-height (frame-char-height)))
         (start (floor (* height (float (window-start))) (point-max)))
         (end (ceiling (* height (float (window-end))) (point-max)))
         (hash-key (format "%s_%s_%d_%d" background foreground start end)))
    ;; Return cached image if we have it.
    (or (gethash hash-key (oref obj image-cache))
        (puthash hash-key
                 (telephone-line-propertize-image
                  (telephone-line--create-pbm-image
                   (telephone-line-separator-create-body obj)
                   background foreground))
                 (oref obj image-cache)))))

(defvar telephone-line-hud (make-instance 'telephone-line--hud))

(telephone-line-defsegment telephone-line-hud-segment ()
  "Miniature 'scroll bar' segment, as seen in the original emacs powerline."
  (let ((fg (face-attribute face :foreground)))
    (telephone-line-separator-render telephone-line-hud
                                     (if (eq fg 'unspecified)
                                         (face-attribute 'default :foreground)
                                       fg)
                                     face)))

(telephone-line-defsegment telephone-line-erc-modified-channels-segment ()
  (when (boundp 'erc-modified-channels-object)
    (string-trim erc-modified-channels-object)))

(telephone-line-defsegment telephone-line-tracking-segment ()
  (when (boundp 'tracking-mode-line-buffers)
    tracking-mode-line-buffers))

(telephone-line-defsegment telephone-line-window-number-segment (&optional in-unicode)
  (when (bound-and-true-p winum-mode)
    (if in-unicode
        (propertize (format "%c" (+ 9311 (winum-get-number))) 'face `winum-face)
      (winum-get-number-string))))

(telephone-line-defsegment telephone-line-projectile-segment ()
  "Displays the current project name, according to projectile."
  (if (fboundp 'projectile-project-name)
      (propertize (projectile-project-name)
                  'face 'telephone-line-projectile
                  'display '(raise 0.0)
                  'help-echo "Switch project"
                  'mouse-face '(:box 1)
                  'local-map (make-mode-line-mouse-map
                              'mouse-1 (lambda ()
                                         (interactive)
                                         (projectile-switch-project))))))

(defcustom telephone-line-project-custom-name nil
  "A custom directory-local name for a project.el project."
  :type 'string
  :group 'telephone-line)

(telephone-line-defsegment telephone-line-project-segment ()
  "Displays the project name, according to project.el"
  (if (project-current)
      (propertize (if (stringp telephone-line-project-custom-name)
          telephone-line-project-custom-name
        (file-name-nondirectory
         (directory-file-name
          (project-root (project-current)))))
                  'face 'telephone-line-projectile
                  'display '(raise 0.0)
                  'help-echo "Switch project"
                  'mouse-face '(:box 1)
                  'local-map (make-mode-line-mouse-map
                              'mouse-1 #'project-switch-project))))

(defun telephone-line--truncate-dir (dir)
  "Truncate DIR, respecting word boundaries."
  (if (string= dir "~")
      dir
    (string-join
     (mapcar (lambda (x) (seq-take x 1))
             (split-string dir "[^[:word:]]" t)))))

(defun telephone-line--truncate-path (path truncate-until)
  "Truncate PATH.  TRUNCATE-UNTIL indicates how far to truncate; -1 means leave the last element, 0 means truncate all, etc."
  (let* ((dirs (split-string path "/"))
         (take (+ truncate-until (length dirs)))
         (trunc (seq-take dirs take))
         (leave (seq-drop dirs take)))
    (string-join (append (mapcar #'telephone-line--truncate-dir trunc) leave) "/")))

(telephone-line-defsegment* telephone-line-projectile-buffer-segment (&optional truncate-until show-project-path)
  "Combined projectile project segment and filename segment with abbreviated filepath.
TRUNCATE-UNTIL sets when to stop truncating; -1 for all but one (i.e. filename), 0 for everything, etc.
If SHOW-PROJECT-PATH is non-nil, shows the abbreviated path leading up to the project dir. Value works the same as TRUNCATE-UNTIL
Inspired by doom-modeline."
  (if (and (buffer-file-name)
           (fboundp 'projectile-project-name)
           (fboundp 'projectile-project-p)
           (projectile-project-p))
      (list ""
            (if show-project-path
                (propertize
                 (telephone-line--truncate-path
                  (abbreviate-file-name (file-name-directory (directory-file-name (projectile-project-root)))) show-project-path)
                 'face 'telephone-line-unimportant
                 'help-echo (buffer-file-name)))
            (funcall (telephone-line-projectile-segment) face)
            (propertize
             (concat "/"
                     (if-let ((rel-path (file-relative-name (file-truename (buffer-file-name))
                                                            (projectile-project-root))))
                         (telephone-line--truncate-path rel-path (or truncate-until -1))))
             'help-echo (buffer-file-name)))
    (telephone-line-raw mode-line-buffer-identification t)))

(telephone-line-defsegment* telephone-line-project-buffer-segment (&optional truncate-until show-project-path)
  "Combined project.el project and filename with abbreviated filepath.

TRUNCATE-UNTIL sets when to stop truncating; -1 for all but
one (i.e. filename), 0 for everything, etc.  If SHOW-PROJECT-PATH
is non-nil, shows the abbreviated path leading up to the project
dir. Value works the same as TRUNCATE-UNTIL Inspired by
doom-modeline."
  (if-let ((buffer-file (buffer-file-name))
           (project (project-current))
           (project-directory (project-root project)))
      (list ""
            (if show-project-path
                (propertize
                 (telephone-line--truncate-path
                  (abbreviate-file-name
                   (file-name-directory (directory-file-name project-directory)))
                  show-project-path)
                 'face 'telephone-line-unimportant
                 'help-echo buffer-file))
            (funcall (telephone-line-project-segment) face)
            (propertize
             (concat "/"
                     (if-let ((rel-path (file-relative-name (file-truename buffer-file)
                                                            project-directory)))
                         (telephone-line--truncate-path rel-path (or truncate-until -1))))
             'help-echo (buffer-file-name)))
    (telephone-line-raw mode-line-buffer-identification t)))

(telephone-line-defsegment* telephone-line-evil-tag-segment ()
  "Displays current evil mode.
Configure the face group telephone-line-evil to change the colors per-mode."
  (when (bound-and-true-p evil-mode)
    (let ((tag (cond
                ((not (evil-visual-state-p)) (upcase (symbol-name evil-state)))
                ((eq evil-visual-selection 'block)
                 (if telephone-line-evil-use-short-tag "VB" "V-BLOCK"))
                ((eq evil-visual-selection 'line)
                 (if telephone-line-evil-use-short-tag "VL" "V-LINE"))
                (t "VISUAL"))))
      (if telephone-line-evil-use-short-tag
          (seq-take tag 2)
        tag))))

(telephone-line-defsegment telephone-line-flymake-segment ()
  "Wraps `flymake-mode' mode-line information in a telephone-line segment."
  (when (bound-and-true-p flymake-mode)
    (telephone-line-raw
     (if (boundp 'flymake--mode-line-format) flymake--mode-line-format
       flymake-mode-line-format) t)))

(telephone-line-defsegment telephone-line-flycheck-segment ()
  "Displays current checker state."
  (when (bound-and-true-p flycheck-mode)
    (let* ((text (pcase flycheck-last-status-change
                   ('finished (if flycheck-current-errors
                                  (let-alist (flycheck-count-errors flycheck-current-errors)
                                    (if (or .error .warning)
                                        (propertize (format "Problems: %s/%s"
                                                            (or .error 0) (or .warning 0))
                                                    'face 'telephone-line-warning)
                                      ""))
                                (propertize ":)" 'face 'telephone-line-unimportant)))
                   ('running     "*")
                   ('no-checker  (propertize "-" 'face 'telephone-line-unimportant))
                   ('not-checked "=")
                   ('errored     (propertize "!" 'face 'telephone-line-error))
                   ('interrupted (propertize "." 'face 'telephone-line-error))
                   ('suspicious  "?"))))
      (propertize text
                  'help-echo (pcase flycheck-last-status-change
                               ('finished "Display errors found by Flycheck")
                               ('running "Running...")
                               ('no-checker "No Checker")
                               ('not-checked "Not Checked")
                               ('errored "Error!")
                               ('interrupted "Interrupted")
                               ('suspicious "Suspicious?"))
                  'display '(raise 0.0)
                  'mouse-face '(:box 1)
                  'local-map (make-mode-line-mouse-map
                              'mouse-1 #'flycheck-list-errors)))))

(telephone-line-defsegment* telephone-line-xah-fly-keys-segment ()
  "Display the current mode for Xah Fly Keys."
  (when (boundp xah-fly-insert-state-p)
    (let ((tag (if xah-fly-insert-state-p
                   "INSERT" "COMMAND")))
      (if telephone-line-evil-use-short-tag
          (seq-take tag 1)
        tag))))

(telephone-line-defsegment* telephone-line-ryo-modal-segment ()
  (let ((tag (if (bound-and-true-p ryo-modal-mode)
                 "RYO" "EMACS")))
    (if telephone-line-evil-use-short-tag
        (seq-take tag 1)
      tag)))

(telephone-line-defsegment* telephone-line-meow-tag-segment ()
  (when (bound-and-true-p meow-mode)
    (let ((tag (meow--get-state-name (meow--current-state))))
      (if telephone-line-evil-use-short-tag
          (seq-take tag 1)
        tag))))

(telephone-line-defsegment* telephone-line-workgroups2-segment ()
  (when (bound-and-true-p workgroups-mode)
    (telephone-line-raw (wg-mode-line-string) t)))

(telephone-line-defsegment* telephone-line-perspective-segment ()
  (when (bound-and-true-p persp-mode)
    (telephone-line-raw (persp-mode-line) t)))

(telephone-line-defsegment* telephone-line-nyan-segment ()
  (when (bound-and-true-p nyan-mode)
    (nyan-create)))

(provide 'telephone-line-segments)
;;; telephone-line-segments.el ends here
