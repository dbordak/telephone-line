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

(telephone-line-defseparator telephone-line-abs-right
  #'abs #'telephone-line-row-pattern
  #xe0b2)
(telephone-line-defseparator telephone-line-abs-left
  (telephone-line-complement abs) #'telephone-line-row-pattern
  #xe0b0)
(telephone-line-defsubseparator telephone-line-abs-hollow-right
  #'abs #'telephone-line-row-pattern-hollow
  #xe0b3)
(telephone-line-defsubseparator telephone-line-abs-hollow-left
  (telephone-line-complement abs) #'telephone-line-row-pattern-hollow
  #xe0b1)

(telephone-line-defseparator telephone-line-cubed-right
  (lambda (x) (expt x 3)) #'telephone-line-row-pattern)
(telephone-line-defseparator telephone-line-cubed-left
  (lambda (x) (- (expt x 3))) #'telephone-line-row-pattern)
(telephone-line-defsubseparator telephone-line-cubed-hollow-right
  (lambda (x) (expt x 3)) #'telephone-line-row-pattern-hollow)
(telephone-line-defsubseparator telephone-line-cubed-hollow-left
  (lambda (x) (- (expt x 3))) #'telephone-line-row-pattern-hollow)

(telephone-line-defseparator telephone-line-identity-right
  #'identity #'telephone-line-row-pattern)
(telephone-line-defseparator telephone-line-identity-left
  #'- #'telephone-line-row-pattern)
(telephone-line-defsubseparator telephone-line-identity-hollow-right
  #'identity #'telephone-line-row-pattern-hollow)
(telephone-line-defsubseparator telephone-line-identity-hollow-left
  #'- #'telephone-line-row-pattern-hollow)

(telephone-line-defseparator telephone-line-gradient
  #'identity #'telephone-line-row-pattern-fixed-gradient)

(defun telephone-line-nil (color1 color2)
  "The nil separator.  COLOR1 and COLOR2 are disregarded."
  nil)

(provide 'telephone-line-separators)
;;; telephone-line-separators.el ends here
