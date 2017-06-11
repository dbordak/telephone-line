;;; telephone-line-segments.el --- Segments for Telephone Line -*- lexical-binding: t -*-

;; Copyright (C) 2015-2017 Daniel Bordak

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
  (let* ((l (number-to-string (if lines lines 4)))
         (c (number-to-string (if columns columns 3))))
    (if (eq major-mode 'paradox-menu-mode)
        (telephone-line-raw mode-line-front-space t)
      `((-3 "%p") ,(concat " %" l "l:%" c "c")))))

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

(telephone-line-defsegment* telephone-line-simple-major-mode-segment ()
  "%[%m%]")

(telephone-line-defsegment* telephone-line-simple-minor-mode-segment ()
  (telephone-line-raw minor-mode-alist t))

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

(telephone-line-defsegment telephone-line-erc-modified-channels-segment ()
  (when (boundp 'erc-modified-channels-object)
    (string-trim erc-modified-channels-object)))

(eval-after-load 'evil
  '(telephone-line-defsegment* telephone-line-evil-tag-segment ()
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

(eval-after-load 'xah-fly-keys
  '(telephone-line-defsegment* telephone-line-xah-fly-keys-segment ()
     (let ((tag (if xah-fly-insert-state-q
                    "INSERT" "COMMAND")))
       (if telephone-line-evil-use-short-tag
           (seq-take tag 1)
         tag))))

(eval-after-load 'ryo-modal
  '(telephone-line-defsegment* telephone-line-ryo-modal-segment ()
     (let ((tag (if ryo-modal-mode
                    "RYO" "EMACS")))
       (if telephone-line-evil-use-short-tag
           (seq-take tag 1)
         tag))))

(eval-after-load 'workgroups2
  '(telephone-line-defsegment* telephone-line-workgroups2-segment ()
     (telephone-line-raw (wg-mode-line-string) t)))

(eval-after-load 'nyan-mode
  '(telephone-line-defsegment* telephone-line-nyan-segment ()
     (nyan-create)))

(provide 'telephone-line-segments)
;;; telephone-line-segments.el ends here
