;;; telephone-line.el --- Rewrite of Powerline -*- lexical-binding: t -*-

;; Copyright (C) 2015-2017 Daniel Bordak

;; Author: Daniel Bordak <dbordak@fastmail.fm>
;; URL: https://github.com/dbordak/telephone-line
;; Version: 0.5
;; Keywords: mode-line
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5") (cl-generic "0.2") (seq "1.8"))

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
;; Telephone Line is a library for customizing the mode-line that is
;; based on the Vim Powerline.  Themes can be created by customizing
;; the telephone-line-lhs and telephone-line-rhs variables.
;;

;;; Code:

(require 'telephone-line-separators)
(require 'telephone-line-segments)

(require 'seq)
(require 'cl-lib)

(defgroup telephone-line nil
  "Fancy separated mode-line."
  :group 'mode-line)

(defface telephone-line-accent-active
  '((t (:foreground "white" :background "grey22" :inherit mode-line)))
  "Accent face for mode-line."
  :group 'telephone-line)

(defface telephone-line-accent-inactive
  '((t (:foreground "white" :background "grey11" :inherit mode-line-inactive)))
  "Accent face for inactive mode-line."
  :group 'telephone-line)

(defface telephone-line-projectile
  '((t (:foreground "light green" :bold t :inherit mode-line)))
  "Hightlight face for the projectile segment"
  :group 'telephone-line)

(defface telephone-line-unimportant
  '((t (:foreground "dim grey" :inherit mode-line)))
  "Hightlight face for the projectile segment"
  :group 'telephone-line)

(defface telephone-line-error
  '((t (:inherit error :underline nil :strike-through nil)))
  "Face to higlight errors in telephone-line. "
  :group 'telephone-line)

(defface telephone-line-warning
  '((t (:inherit warning :underline nil :strike-through nil)))
  "Face to higlight warnings in telephone-line."
  :group 'telephone-line)

(defface telephone-line-evil
  '((t (:foreground "white" :weight bold :inherit mode-line)))
  "Meta-face used for property inheritance on all telephone-line-evil faces."
  :group 'telephone-line-evil)

(defface telephone-line-evil-insert
  '((t (:background "forest green" :inherit telephone-line-evil)))
  "Face used in evil color-coded segments when in Insert state."
  :group 'telephone-line-evil)

(defface telephone-line-evil-normal
  '((t (:background "red3" :inherit telephone-line-evil)))
  "Face used in evil color-coded segments when in Normal state."
  :group 'telephone-line-evil)

(defface telephone-line-evil-visual
  '((t (:background "dark orange" :inherit telephone-line-evil)))
  "Face used in evil color-coded segments when in Visual{,-Block,-Line} state."
  :group 'telephone-line-evil)

(defface telephone-line-evil-replace
  '((t (:background "black" :inherit telephone-line-evil)))
  "Face used in evil color-coded segments when in Replace state."
  :group 'telephone-line-evil)

(defface telephone-line-evil-motion
  '((t (:background "dark blue" :inherit telephone-line-evil)))
  "Face used in evil color-coded segments when in Motion state."
  :group 'telephone-line-evil)

(defface telephone-line-evil-operator
  '((t (:background "violet" :inherit telephone-line-evil)))
  "Face used in evil color-coded segments when in Operator state."
  :group 'telephone-line-evil)

(defface telephone-line-evil-emacs
  '((t (:background "dark violet" :inherit telephone-line-evil)))
  "Face used in evil color-coded segments when in Emacs state."
  :group 'telephone-line-evil)

(defface telephone-line-evil-god
  '((t (:background "sky blue" :inherit telephone-line-evil)))
  "Face used in evil color-coded segments when in God state.

God state is provided by the package evil-god-state."
  :group 'telephone-line-evil)

(defcustom telephone-line-faces
  '((evil . telephone-line-modal-face)
    (modal . telephone-line-modal-face)
    (ryo . telephone-line-ryo-modal-face)
    (accent . (telephone-line-accent-active . telephone-line-accent-inactive))
    (nil . (mode-line . mode-line-inactive)))
  "Alist providing all the available face symbols.

Symbols can either map to a pair of faces (ACTIVE . INACTIVE) or
to a function which takes ACTIVE as a parameter."
  :group 'telephone-line
  :type '(alist :key-type color-symbol :value-type pair-or-function))

(defcustom telephone-line-subseparator-faces
  '((evil . nil)
    (accent . nil)
    (nil . accent))
  "Alist pairing segment color-syms to subseparator color-syms.

If any sym is paired to itself, the subseparator will use the
foreground color for that segment. Otherwise, it will use the
background color from the paired sym."
  :group 'telephone-line
  :type '(alist :key-type color-symbol :value-type color-symbol))

(defcustom telephone-line-primary-left-separator 'telephone-line-abs-left
  "The primary separator to use on the left-hand side."
  :group 'telephone-line
  :type 'symbol)

(defcustom telephone-line-primary-right-separator 'telephone-line-abs-right
  "The primary separator to use on the right-hand side."
  :group 'telephone-line
  :type 'symbol)

(defcustom telephone-line-secondary-left-separator 'telephone-line-abs-hollow-left
  "The secondary separator to use on the left-hand side.

Secondary separators do not incur a background color change."
  :group 'telephone-line
  :type 'symbol)

(defcustom telephone-line-secondary-right-separator 'telephone-line-abs-hollow-right
  "The secondary separator to use on the right-hand side.

Secondary separators do not incur a background color change."
  :group 'telephone-line
  :type 'symbol)

(defun telephone-line-fill (reserve &optional face)
  "Return RESERVE empty space on the right, optionally with a FACE."
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin)
                                             ,reserve)))
              'face face))

;; Active Window

(defvar telephone-line-selected-window (frame-selected-window))

(defun telephone-line--set-selected-window (_)
  (unless (minibuffer-window-active-p (frame-selected-window))
    (setq telephone-line-selected-window (frame-selected-window))))

(add-hook 'pre-redisplay-functions #'telephone-line--set-selected-window)

(defun telephone-line-selected-window-active ()
  "Return whether the current window is active."
  (eq telephone-line-selected-window (selected-window)))

;; Face Functions

(defun telephone-line-face-map (sym)
  "Return the face corresponding to SYM for the selected window's active state."
  (telephone-line--face-map sym (telephone-line-selected-window-active)))

(defun telephone-line--face-map (sym active)
  "Return the face corresponding to SYM for the given ACTIVE state."
  (let ((pair-or-func (telephone-line-alist-get sym telephone-line-faces)))
    (cond ((functionp pair-or-func) (funcall pair-or-func active))
          (active (car pair-or-func))
          (t (cdr pair-or-func)))))

(defun telephone-line-subseparator-foreground (sym)
  "Get the foreground color for a subseparator on a given SYM."
  (let ((subseparator-sym (telephone-line-alist-get sym telephone-line-subseparator-faces)))
    (if (equal sym subseparator-sym)
        (face-attribute (telephone-line-face-map sym) :foreground)
      (face-attribute (telephone-line-face-map subseparator-sym) :background))))

(defun telephone-line-ryo-modal-face (active)
  "Return an appropriate face depending whether ryo-modal is activated, given whether frame is ACTIVE."
  (cond ((not active) 'mode-line-inactive)
        ((not (boundp 'ryo-modal-mode)) 'mode-line)
        ((not ryo-modal-mode) 'telephone-line-evil-insert)
        (t 'telephone-line-evil-normal)))

(defun telephone-line-modal-face (active)
  "Return an appropriate face for the current mode, given whether the frame is ACTIVE."
  (cond ((not active) 'mode-line-inactive)
        ((bound-and-true-p xah-fly-keys)
         (if xah-fly-insert-state-q
             'telephone-line-evil-insert
           'telephone-line-evil-normal))
        ((not (bound-and-true-p evil-mode)) 'mode-line)
        (t (intern (concat "telephone-line-evil-" (symbol-name evil-state))))))

;; Modeline generation

;;TODO: Clean this up
(defun telephone-line--separator-generator (primary-sep)
  (lambda (acc e)
    (let ((cur-color-sym (car e))
          (prev-color-sym (cdr acc))
          (cur-subsegments (cdr e))
          (accumulated-segments (car acc)))

      (cons
       (if accumulated-segments
           (cl-list*
            cur-subsegments ;New segment
            ;; Separator
            `(:eval (telephone-line-separator-render ,primary-sep
                                       (telephone-line-face-map ',prev-color-sym)
                                       (telephone-line-face-map ',cur-color-sym)))
            accumulated-segments) ;Old segments
         (list cur-subsegments))
       cur-color-sym))))

(defun telephone-line-propertize-segment (pred face segment)
  (unless (seq-empty-p (string-trim (format-mode-line segment)))
    (if (or pred (not (telephone-line-selected-window-active)))
        `(:propertize (" " ,segment " ") face ,face)
      `(" " ,segment " "))))

;;TODO: Clean this up
(defun telephone-line-add-subseparators (subsegments sep-func color-sym)
  (let* ((cur-face (telephone-line-face-map color-sym))
         (subseparator-foreground (telephone-line-subseparator-foreground color-sym))
         (subseparator (telephone-line-separator-render sep-func cur-face subseparator-foreground)))
    (telephone-line-propertize-segment
     color-sym cur-face
     (cdr (seq-mapcat
           (lambda (subseg)
             (when subseg
               (list subseparator subseg)))
           (mapcar (lambda (f) (funcall f cur-face))
                   subsegments))))))

(defun telephone-line-preprocess-subsegments (subsegments)
  "Normalize SUBSEGMENTS to create a strict list of functions."
  (mapcar (lambda (subsegment)
            (if (functionp subsegment)
                (funcall subsegment)
              (seq-let (segment-func &rest modifiers) subsegment
                (if (plist-get modifiers ':args)
                    (setq segment-func
                          (apply segment-func (plist-get modifiers ':args)))
                  (setq segment-func
                        (funcall segment-func)))
                (if (plist-get modifiers ':active)
                    (setq segment-func
                          `(lambda (face)
                             (if (telephone-line-selected-window-active)
                                 (,segment-func face)
                               nil))))
                (if (plist-get modifiers ':inactive)
                    (setq segment-func
                          `(lambda (face)
                            (if (not (telephone-line-selected-window-active))
                                (,segment-func face)
                              nil))))
                (if (and (plist-get modifiers ':truncate)
                         (< 0 (plist-get modifiers ':truncate)))
                    (setq segment-func
                          `(lambda (face)
                             (seq-take (format-mode-line (,segment-func face)) ,(plist-get modifiers ':truncate)))))
                segment-func)))
          subsegments))

;;TODO: Clean this up
(defun telephone-line-add-separators (segments primary-sep secondary-sep)
  "Interpolates SEGMENTS with PRIMARY-SEP and SECONDARY-SEP.

Primary separators are added at initialization.  Secondary
separators, as they are conditional, are evaluated on-the-fly."
  (when segments
    (car (seq-reduce
          (telephone-line--separator-generator primary-sep)
          (mapcar (lambda (segment-pair)
                    (seq-let (color-sym &rest subsegments) segment-pair
                      (cons color-sym
                            `(:eval
                              (telephone-line-add-subseparators
                               ',(telephone-line-preprocess-subsegments subsegments)
                               ,secondary-sep
                               ',color-sym)))))
                  (seq-reverse segments))
          '(nil . nil)))))

(defun telephone-line-width (values num-separators separator)
  "Get the column-length of VALUES, with NUM-SEPARATORS SEPARATORs interposed."
  (let ((base-width (string-width (format-mode-line values)))
        (separator-width (/ (telephone-line-separator-width separator)
                            (float (frame-char-width)))))
    (if window-system
        (+ base-width
           ;; Separators are (ceiling separator-width)-space strings,
           ;; but their actual width is separator-width. base-width
           ;; already includes the string width of those spaces, so we
           ;; need the difference.
           (* num-separators (- separator-width (ceiling separator-width))))
      base-width)))

(defcustom telephone-line-lhs
  '((evil   . (telephone-line-evil-tag-segment))
    (accent . (telephone-line-vc-segment
               telephone-line-erc-modified-channels-segment
               telephone-line-process-segment))
    (nil    . (telephone-line-projectile-segment
               telephone-line-buffer-segment)))
  "Left hand side segment alist."
  :type '(alist :key-type segment-color :value-type subsegment-list)
  :group 'telephone-line)

(defcustom telephone-line-center-lhs
  nil
  "Center-left segment alist."
  :type '(alist :key-type segment-color :value-type subsegment-list)
  :group 'telephone-line)

(defcustom telephone-line-center-rhs
  nil
  "Center-right segment alist."
  :type '(alist :key-type segment-color :value-type subsegment-list)
  :group 'telephone-line)

(defcustom telephone-line-rhs
  '((nil    . (telephone-line-flycheck-segment
               telephone-line-misc-info-segment))
    (accent . (telephone-line-major-mode-segment))
    (evil   . (telephone-line-airline-position-segment)))
  "Right hand side segment alist."
  :type '(alist :key-type segment-color :value-type subsegment-list)
  :group 'telephone-line)

(defun telephone-line--generate-mode-line-lhs ()
  (telephone-line-add-separators telephone-line-lhs
                   telephone-line-primary-left-separator
                   telephone-line-secondary-left-separator))

(defun telephone-line--generate-mode-line-center ()
  (append (telephone-line-add-separators telephone-line-center-lhs
                           telephone-line-primary-right-separator
                           telephone-line-secondary-right-separator)
          (telephone-line-add-separators telephone-line-center-rhs
                           telephone-line-primary-left-separator
                           telephone-line-secondary-left-separator)))

(defun telephone-line--generate-mode-line-rhs ()
  (telephone-line-add-separators telephone-line-rhs
                   telephone-line-primary-right-separator
                   telephone-line-secondary-right-separator))

(defun telephone-line--generate-mode-line ()
  `(,@(telephone-line--generate-mode-line-lhs)
    (:eval (when (or telephone-line-center-lhs telephone-line-center-rhs)
             (telephone-line-fill
              (/ (+ (window-width)
                    (telephone-line-width
                     ',(telephone-line--generate-mode-line-center)
                     ,(- (length telephone-line-rhs) 1)
                     ,telephone-line-primary-right-separator))
                 2)
              (telephone-line-face-map (caar telephone-line-center-lhs)))))
    ,@(telephone-line--generate-mode-line-center)
    (:eval (telephone-line-fill
            (telephone-line-width
             ',(telephone-line--generate-mode-line-rhs)
             ,(- (length telephone-line-rhs) 1)
             ,telephone-line-primary-right-separator)
            (telephone-line-face-map (caar telephone-line-rhs))))
    ,@(telephone-line--generate-mode-line-rhs)))

(defvar telephone-line--default-mode-line mode-line-format)

;;;###autoload
(define-minor-mode telephone-line-mode
  "Toggle telephone-line on or off."
  :group 'telephone-line
  :global t
  :lighter nil
  (setq-default mode-line-format
                (if telephone-line-mode
                    `("%e" ,@(telephone-line--generate-mode-line))
                  telephone-line--default-mode-line)))

(provide 'telephone-line)
;;; telephone-line.el ends here
