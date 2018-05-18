;;; persona-tests.el ---
;;
;; Filename: persona-tests.el
;; Description:
;; Author: Lord Yuuma
;; Maintainer: Lord Yuuma
;; Version:
;; Package-Requires: ()
;; URL:
;; Doc URL:
;; Keywords:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(ert-deftest persona-unmask-font-lock-test ()
  "Check if `persona-unmask' handles known font-lock faces correctly."
  (skip-unless (featurep 'font-lock))
  (should (equal (persona-unmask '(font-lock builtin) nil #'ignore)
                 "font-lock-builtin-face"))
  (should (equal (persona-unmask '(font-lock comment-delimiter) nil #'ignore)
                 "font-lock-comment-delimiter-face"))
  (should (equal (persona-unmask '(font-lock comment) nil #'ignore)
                 "font-lock-comment-face"))
  (should (equal (persona-unmask '(font-lock constant) nil #'ignore)
                 "font-lock-constant-face"))
  (should (equal (persona-unmask '(font-lock doc) nil #'ignore)
                 "font-lock-doc-face"))
  (should (equal (persona-unmask '(font-lock function-name) nil #'ignore)
                 "font-lock-function-name-face"))
  (should (equal (persona-unmask '(font-lock keyword) nil #'ignore)
                 "font-lock-keyword-face"))
  (should (equal (persona-unmask '(font-lock negation-char) nil #'ignore)
                 "font-lock-negation-char-face"))
  (should (equal (persona-unmask '(font-lock preprocessor) nil #'ignore)
                 "font-lock-preprocessor-face"))
  (should (equal (persona-unmask '(font-lock string) nil #'ignore)
                 "font-lock-string-face"))
  (should (equal (persona-unmask '(font-lock type) nil #'ignore)
                 "font-lock-type-face"))
  (should (equal (persona-unmask '(font-lock variable-name) nil #'ignore)
                 "font-lock-variable-name-face"))
  (should (equal (persona-unmask '(font-lock warning) nil #'ignore)
                 "font-lock-warning-face")))

(ert-deftest persona-unmask-unknown-face-test ()
  "Check if `persona-unmask' handles known font-lock faces correctly."
  (skip-unless (not (facep 'undefined-face)))
  (should (equal (persona-unmask '(undefined) nil #'ignore)
                 nil)))

(ert-deftest persona-attributes-to-face-test ()
  (should (equal (persona-attributes-to-face
                  '(:foreground "blue" :inherit ((font-lock type))))
                 '(:foreground "blue" :inherit (font-lock-type-face)))))

(ert-deftest persona-declare-face-test ()
  (skip-unless noninteractive) ;; this test has side effects
  (let ((face (persona-declare-face 'persona-test-face
                                    '((t :inherit ((font-lock type))))
                                    "Undocumented")))
    (should (equal face 'persona-test-face))
    (should (equal (face-attribute face :inherit)
                   '(font-lock-type-face))))

  (let ((face (persona-declare-face 'persona-test-face
                                    '((t :foreground "blue"
                                         :inherit ((font-lock type))))
                                    "Undocumented")))
    (should (equal face 'persona-test-face))
    (should (equal (face-attribute face :inherit)
                   '(font-lock-type-face)))))

(ert-deftest personae-unmask-and-declare-test ()
  (skip-unless noninteractive) ;; this test has side effects
  (let ((personae
         '((personae
            ((simple test)
             (t :inherit (default)))
            ((inherit test)
             (t :inherit ((font-lock type))))))))
    (personae-unmask-and-declare personae)
    (should (facep 'personae-simple-test-face))
    (should (equal (face-attribute 'personae-simple-test-face :inherit)
                   '(default)))

    (should (facep 'personae-inherit-test-face))
    (should (equal (face-attribute 'personae-inherit-test-face :inherit)
                   '(font-lock-type-face)))))

;;; persona-tests.el ends here
