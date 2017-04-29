;;; telephone-line-separators.el --- Separators for Telephone Line

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
;; Separators for Telephone Line.
;; To create your own, look at the functions defined in telephone-line-utils.el

;;; Code:

(require 'color)
(require 'telephone-line-utils)

(defun telephone-line-row-pattern-fixed-gradient (_ width)
  "Create a gradient bytestring of WIDTH.  _ is disregarded."
  (mapcar (lambda (num)
            (/ num (float width)))
          (number-sequence 1 width)))

(defvar telephone-line-gradient
  (make-instance 'telephone-line-separator
                 :axis-func #'identity
                 :pattern-func #'telephone-line-row-pattern-fixed-gradient))

(defvar telephone-line-abs-right
  (make-instance 'telephone-line-separator
                 :axis-func #'abs
                 :alt-char #xe0b2))
(defvar telephone-line-abs-left
  (make-instance 'telephone-line-separator
                 :axis-func (telephone-line-complement abs)
                 :alt-char #xe0b0))
(defvar telephone-line-abs-hollow-right
  (make-instance 'telephone-line-subseparator
                 :axis-func #'abs
                 :alt-char #xe0b3))
(defvar telephone-line-abs-hollow-left
  (make-instance 'telephone-line-subseparator
                 :axis-func (telephone-line-complement abs)
                 :alt-char #xe0b1))

(defvar telephone-line-cubed-right
  (make-instance 'telephone-line-separator
                 :axis-func (lambda (x) (expt x 3))))
(defvar telephone-line-cubed-left
  (make-instance 'telephone-line-separator
                 :axis-func (lambda (x) (- (expt x 3)))))
(defvar telephone-line-cubed-hollow-right
  (make-instance 'telephone-line-subseparator
                 :axis-func (lambda (x) (expt x 3))))
(defvar telephone-line-cubed-hollow-left
  (make-instance 'telephone-line-subseparator
                 :axis-func (lambda (x) (- (expt x 3)))))

(defvar telephone-line-identity-right
  (make-instance 'telephone-line-separator :axis-func #'identity))
(defvar telephone-line-identity-left
  (make-instance 'telephone-line-separator :axis-func #'-))
(defvar telephone-line-identity-hollow-right
  (make-instance 'telephone-line-subseparator :axis-func #'identity))
(defvar telephone-line-identity-hollow-left
  (make-instance 'telephone-line-subseparator :axis-func #'-))

(defvar telephone-line-nil
  (make-instance 'telephone-line-separator :axis-func (lambda (c) 0)))

(provide 'telephone-line-separators)
;;; telephone-line-separators.el ends here
