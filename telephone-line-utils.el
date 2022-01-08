;;; telephone-line-utils.el --- Functions for defining segparators and segments -*- lexical-binding: t -*-

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

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'color)
(require 'eieio)
(require 'seq)
(require 'subr-x)

(defcustom telephone-line-height nil
  "Override the mode-line height."
  :group 'telephone-line
  :type '(choice integer (const nil)))

(defcustom telephone-line-separator-extra-padding 1
  "Extra spacing around separators."
  :group 'telephone-line
  :type '(choice integer))

(defcustom telephone-line-evil-use-short-tag nil
  "If non-nil, use an abbreviated name for the evil mode tag."
  :type 'boolean
  :group 'telephone-line-evil)

(if (fboundp #'alist-get)
    (defalias 'telephone-line-alist-get #'alist-get)
  (defun telephone-line-alist-get (key alist &optional default remove)
    "Backport of alist-get for Emacs<25"
    (ignore remove)
    (let ((x (assq key alist)))
      (if x (cdr x) default))))

(defun telephone-line-create-axis (length)
  "Create an axis of length LENGTH from -((LENGTH-1)/2) to +((LENGTH-1)/2).
For instance a LENGTH of 9 produces:

  -4 -3 -2 -1 0 +1 +2 +3 +4

...And a LENGTH of 10 produces:

  -4.5 -3.5 -2.5 -1.5 -0.5 +0.5 +1.5 +2.5 +3.5 +4.5"
  (let ((bound (/ (1- length) 2.0)))
    (number-sequence (- bound) bound)))

(defun telephone-line-create-trig-axis (length)
  "Create a trig axis with LENGTH steps, ranging from -pi to +pi.
As with create-axis, on even LENGTHs, the 0 is repeated to
preserve symmetry."
  (let* ((integer-axis (telephone-line-create-axis length))
         (integer-max (seq-max integer-axis)))
    (mapcar (lambda (x)
              (/ (* float-pi x) integer-max))
            integer-axis)))

(defun telephone-line--normalize-axis (seq)
  "Apply an offset to all values of SEQ such that its range begins at 0."
  (let ((minimum (seq-min seq)))
    (if (not (eq minimum 0))
        (mapcar (lambda (i) (- i minimum)) seq)
      seq)))

(defun telephone-line-interpolate-rgb (color1 color2 &optional ratio)
  "Interpolate between COLOR1 and COLOR2, with color1/color2 RATIO.
When no RATIO is provided, produces the color halfway between
color1 and color2."
  (unless ratio (setq ratio .5))
  (apply #'color-rgb-to-hex
         (mapcar (lambda (n)
                   (+ (* ratio (nth n (color-name-to-rgb color1)))
                      (* (- 1 ratio) (nth n (color-name-to-rgb color2)))))
                 '(0 1 2))))

(defun telephone-line-color-to-bytestring (color)
  "Return an RGB bytestring for a given COLOR."
  (seq-mapcat (lambda (subc)
                (byte-to-string (floor (* 255 subc))))
              (if (listp color)
                  color
                (color-name-to-rgb color))
              'string))

;; TODO: error on non-rectangular input?
(defun telephone-line--create-pbm-image (body fg-color bg-color)
  "Create a pbm image from a byte list BODY and colors FG-COLOR and BG-COLOR."
  (create-image
   (concat
    (format "P6 %d %d 255 " (length (car body)) (length body))
    (seq-mapcat (lambda (pixel)
                  (telephone-line-color-to-bytestring
                   (telephone-line-interpolate-rgb bg-color fg-color pixel)))
                (seq-mapcat #'identity body)
                'string))
   'pbm t
   :scale 1.0
   :ascent 'center))

(defun telephone-line-propertize-image (image)
  "Return a propertized string of IMAGE."
  (propertize (make-string (ceiling (car (image-size image))) ?|)
              'display image))

(defun telephone-line-row-pattern (fill total)
  "Make a list of percentages (0 to 1), with FILL 0s out of TOTAL 1s, with a non-integer in between."
  (seq-let (intfill rem) (cl-floor fill)
    (nconc
     (make-list intfill 0) ;Left fill
     (when (< intfill total)
       (cons (- 1 rem) ;AA pixel
             (make-list (- total intfill 1) 1)))))) ;Right gap

(defun telephone-line-row-pattern-hollow (padding total)
  "Make a list of percentages (0 to 1), with a non-integer positioned PADDING places in out of TOTAL places."
  (seq-let (intpadding rem) (cl-floor padding)
    (nconc
     (make-list intpadding 1) ;Left gap
     (when (< intpadding total)
       (list rem)) ;Left AA pixel
     (when (< (1+ intpadding) total)
       (cons (- 1 rem)  ;Right AA pixel
             (make-list (- total intpadding 2) 1)))))) ;Right gap

(defun telephone-line-row-pattern-binary (fill total)
  (if (= fill 0)
      (make-list total 0)
    (make-list total 1)))

(defmacro telephone-line-complement (func)
  "Return a function which is the complement of FUNC."
  `(lambda (x)
     (- (,func x))))

(defclass telephone-line-unicode-separator ()
  ((char :initarg :char)
   (inverse-video :initarg :inverse-video :initform t)))

(defclass telephone-line-separator ()
  ((axis-func :initarg :axis-func)
   (axis-init :initarg :axis-init :initform #'telephone-line-create-trig-axis)
   (pattern-func :initarg :pattern-func :initform #'telephone-line-row-pattern)
   (forced-width :initarg :forced-width :initform nil)
   (alt-separator :initarg :alt-separator)
   (image-cache :initform (make-hash-table :test 'equal :size 10))))

(cl-defmethod telephone-line-separator-height ((obj telephone-line-separator))
  (or telephone-line-height (frame-char-height)))

(cl-defmethod telephone-line-separator-height ((obj telephone-line-unicode-separator))
  (frame-char-height))

(cl-defmethod telephone-line-separator-width ((obj telephone-line-separator))
  (or (oref obj forced-width)
      (ceiling (telephone-line-separator-height obj) 2)))

(cl-defmethod telephone-line-separator-width ((obj telephone-line-unicode-separator))
  (frame-char-width))

(defclass telephone-line-subseparator (telephone-line-separator)
  ((pattern-func :initarg :pattern-func
                 :initform #'telephone-line-row-pattern-hollow)))

(defclass telephone-line-nil-separator (telephone-line-separator) ())

(cl-defmethod telephone-line-separator-create-body ((obj telephone-line-separator))
  "Create a bytestring of a PBM image body of dimensions WIDTH and HEIGHT, and shape created from AXIS-FUNC and PATTERN-FUNC."
  (let* ((height (telephone-line-separator-height obj))
         (width (telephone-line-separator-width obj))
         (normalized-axis (telephone-line--normalize-axis
                           (mapcar (oref obj axis-func)
                                   (funcall (oref obj axis-init) height))))
         (range (seq-max normalized-axis))
         (scaling-factor (/ (1- width)(float range))))
    (mapcar (lambda (x)
              (funcall (oref obj pattern-func)
                       (* x scaling-factor) width))
            normalized-axis)))

(defun telephone-line--pad-body (body char-width)
  "Pad 2d byte-list BODY to a width of CHAR-WIDTH, given as a number of characters."
  (let* ((body-width (length (car body)))
         (padding-width (- (* char-width (frame-char-width)) body-width))
         (left-padding (make-list (floor padding-width 2) 1))
         (right-padding (make-list (ceiling padding-width 2) 1)))
    (mapcar (lambda (row)
              (append left-padding row right-padding))
            body)))

(cl-defmethod telephone-line-separator-create-body ((obj telephone-line-subseparator))
  "Create a bytestring of a PBM image body of dimensions WIDTH and HEIGHT, and shape created from AXIS-FUNC and PATTERN-FUNC.

Includes padding."
  (telephone-line--pad-body (cl-call-next-method)
                            (+ (ceiling (telephone-line-separator-width obj)
                                        (frame-char-width))
                               telephone-line-separator-extra-padding)))

(cl-defmethod telephone-line-separator--arg-handler (arg)
  "Translate ARG into an appropriate color for a separator."
  (if (facep arg)
      (face-attribute arg :background)
    arg))

(cl-defmethod telephone-line-separator-render-image ((obj telephone-line-separator) foreground background)
  "Find cached pbm of OBJ in FOREGROUND and BACKGROUND.
If it doesn't exist, create and cache it."
  (let* ((height (telephone-line-separator-height obj))
         (hash-key (format "%s_%s_%s" background foreground height)))
    ;; Return cached image if we have it.
    (or (gethash hash-key (oref obj image-cache))
        (puthash hash-key
                 (telephone-line-propertize-image
                  (telephone-line--create-pbm-image
                   (telephone-line-separator-create-body obj)
                   background foreground))
                 (oref obj image-cache)))))

(cl-defmethod telephone-line-separator-render ((obj telephone-line-unicode-separator) foreground background)
  (let ((fg-color (telephone-line-separator--arg-handler foreground))
        (bg-color (telephone-line-separator--arg-handler background)))
    (list :propertize (char-to-string (oref obj char))
        'face (list :foreground fg-color
                    :background bg-color
                    :inverse-video (oref obj inverse-video)))))

(cl-defmethod telephone-line-separator-render ((obj telephone-line-separator) foreground background)
  (let ((fg-color (telephone-line-separator--arg-handler foreground))
        (bg-color (telephone-line-separator--arg-handler background)))
    (if window-system
        (telephone-line-separator-render-image obj fg-color bg-color)
      (telephone-line-separator-render (oref obj alt-separator) fg-color bg-color))))

(cl-defmethod telephone-line-separator-render ((obj telephone-line-nil-separator) foreground background)
  nil)

(cl-defmethod telephone-line-separator-clear-cache ((obj telephone-line-separator))
  (clrhash (oref obj image-cache)))

;;;###autoload
(defmacro telephone-line-defsegment* (name &rest body)
  "Define NAME as a segment function.

Does not check if segment is empty; will always display on non-nil result."
  (declare (doc-string 3) (indent defun))
  `(defun ,name
     ,@(butlast body)
     (lambda (face)
       ,(car (last body)))))

;;;###autoload
(defmacro telephone-line-defsegment (name &rest body)
  "Define NAME as a segment function.

Empty strings will not render."
  (declare (doc-string 3) (indent defun))
  `(telephone-line-defsegment* ,name
     ,@(butlast body)
     (telephone-line-raw ,(car (last body)))))

;;;###autoload
(defun telephone-line-raw (str &optional preformatted)
  "Conditionally render STR as mode-line data.
If optional argument PREFORMATTED is non-nil, verify the output
instead.
Return nil for blank/empty strings."
  (let ((trimmed-str (string-trim (format-mode-line str))))
    (unless (seq-empty-p trimmed-str)
      (if preformatted
          ; format-mode-line will condense all escaped %s, so we need
          ; to re-escape them.
          (replace-regexp-in-string "%" "%%" trimmed-str)
        str))))

;;Stole this bit from seq.el
(defun telephone-line--activate-font-lock-keywords ()
  "Activate font-lock keywords for some symbols defined in telephone-line."
  (font-lock-add-keywords 'emacs-lisp-mode
                  '("\\<telephone-line-defsegment*\\>"
                    "\\<telephone-line-defsegment\\>")))

(unless (fboundp 'elisp--font-lock-flush-elisp-buffers)
  ;; In Emacs≥25, (via elisp--font-lock-flush-elisp-buffers and a few others)
  ;; we automatically highlight macros.
  (add-hook 'emacs-lisp-mode-hook #'telephone-line--activate-font-lock-keywords))

(provide 'telephone-line-utils)
;;; telephone-line-utils.el ends here
