;;; telephone-line-separators.el --- Separators for Telephone Line

;; Copyright (C) 2015 Daniel Bordak

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
  (telephone-line-separator "gradient"
              :axis-func #'identity
              :pattern-func #'telephone-line-row-pattern-fixed-gradient))

(defvar telephone-line-abs-right
  (telephone-line-separator "abs-right"
              :axis-func #'abs
              :alt-char #xe0b2))
(defvar telephone-line-abs-left
  (telephone-line-separator "abs-left"
              :axis-func (telephone-line-complement abs)
              :alt-char #xe0b0))
(defvar telephone-line-abs-hollow-right
  (telephone-line-subseparator "abs-hollow-right"
                 :axis-func #'abs
                 :alt-char #xe0b3))
(defvar telephone-line-abs-hollow-left
  (telephone-line-subseparator "abs-hollow-left"
                 :axis-func (telephone-line-complement abs)
                 :alt-char #xe0b1))

(defvar telephone-line-cubed-right
  (telephone-line-separator "cubed-right"
              :axis-func (lambda (x) (expt x 3))))
(defvar telephone-line-cubed-left
  (telephone-line-separator "cubed-left"
              :axis-func (lambda (x) (- (expt x 3)))))
(defvar telephone-line-cubed-hollow-right
  (telephone-line-subseparator "cubed-hollow-right"
                 :axis-func (lambda (x) (expt x 3))))
(defvar telephone-line-cubed-hollow-left
  (telephone-line-subseparator "cubed-hollow-left"
                 :axis-func (lambda (x) (- (expt x 3)))))

(defvar telephone-line-identity-right
  (telephone-line-separator "identity-right" :axis-func #'identity))
(defvar telephone-line-identity-left
  (telephone-line-separator "identity-left" :axis-func #'-))
(defvar telephone-line-identity-hollow-right
  (telephone-line-subseparator "identity-hollow-right" :axis-func #'identity))
(defvar telephone-line-identity-hollow-left
  (telephone-line-subseparator "identity-hollow-left" :axis-func #'-))

(defvar telephone-line-nil
  (telephone-line-separator "nil" :axis-func (lambda (c1 c2) nil)))

(provide 'telephone-line-separators)
;;; telephone-line-separators.el ends here
