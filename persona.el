;;; persona.el --- Faces as variables, oh my...
;;
;; Filename: persona.el
;; Description:
;; Author: Lord Yuuma
;; Maintainer: Lord Yuuma
;; Version: 0.1
;; Package-Requires: ((emacs "25.3") (cl-lib "1.0"))
;; URL:
;; Doc URL:
;; Keywords: faces
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Persona offers the `persona' widget, which looks similar enough to
;; a face, but is not quite a face, like a mask of some sort.
;; This allows its usage as a customization type for variables, both
;; through `defcustom' and `eieio'.
;; Additionally, the `personae' widget is provided for bulk definitions.
;;
;; Notable functions are:
;;  * `persona-unmask': Reduce a list of symbols to a face name.
;;  * `persona-declare-face': Like `custom-declare-face', but
;;                            takes a persona as specification.
;;  * `personae-unmask-and-declare': Recursively declares all
;;                                   personae in its input.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 0.2: added editing functionality
;; 0.1: initial version
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

(require 'cus-edit)
(require 'cl-lib)
(require 'eieio) ;; list-of
(require 'widget)

;; TODO: Compilation currently seems to break the persona deftype
;; This snippet therefore doesn't amount to much, but it's taken from
;; the widget programming example, so it must have some meaning, right?
(eval-when-compile
  (require 'wid-edit))

(defconst persona-attribute-overrides
  '((:inherit
     (repeat :tag "Inherit"
             :help-echo "List of faces or personas to inherit from"
             (choice (face :tag "Face" default)
                     (repeat :tag "Persona" symbol)))))
  "Custom attribute overrides for `persona-attributes' widget.")

(define-widget 'persona-attributes 'set
  "Wrapper widget for face attributes."
  :tag "Attributes"
  :args
  (mapcar
   (lambda (attr)
     (list 'group :inline 't
           ;; hide plist tag form user
           (list 'const :format "" (car attr))
           ;; actual customization type
           (cadr
            (or (assoc (car attr)
                       persona-attribute-overrides)
                attr))))
   custom-face-attributes))

(define-widget 'persona 'editable-list
  "An editable list of display specifications and persona attributes"
  :tag "Persona"
  :entry-format "%i %d %v"
  :insert-button-args '(:help-echo "Insert new display specification here.")
  :append-button-args '(:help-echo "Append new display specification here.")
  :delete-button-args '(:help-echo "Delete this display specification.")
  :args '((group :format "%v"
                 custom-display
                 (persona-attributes :inline t))))

(cl-deftype persona ()
  `(satisfies ,(apply-partially #'widget-apply
                                (widget-convert 'persona)
                                :match)))

(define-widget 'personae 'lazy
  "Widget grouping persona definitions."
  :tag "Personae"
  :type '(alist :key-type (choice symbol (repeat symbol))
                :value-type (choice persona personae)))

(cl-deftype personae ()
  `(satisfies ,(apply-partially #'widget-apply
                                (widget-convert 'personae)
                                :match)))

(defun persona-face-or-unmask (face-or-persona)
  "Unmask FACE-OR-PERSONA if it is not already a face.
Use `intern' to create a canonical symbol."
  (cl-typecase face-or-persona
    ((or symbol face) face-or-persona)
    ((list-of symbol) (persona-unmask face-or-persona #'intern))))

(defun persona-unmask (persona &optional whenface unlessface)
  "Unmask PERSONA, returning an actual face name.
Optional parameters WHENFACE and UNLESSFACE are callbacks, which are
called with one string-type parameter, defaulting to `identity'.
WHENFACE is called when PERSONA resolves to an actual face, i.e. `facep'
succeeds.
UNLESSFACE is called, when PERSONA does not resolve to an actual face,
i.e. `facep' fails. If only WHENFACE is set, UNLESSFACE is also set
to WHENFACE to keep the symmetry.
The return value is filtered through the corresponding function.
"
  (cl-check-type persona (list-of symbol))
  (let ((whenface (or whenface #'identity))
        (unlessface (or unlessface whenface #'identity))
        (candidate (concat (mapconcat #'symbol-name persona "-")
                           "-face")))
    (if (facep candidate)
        (funcall whenface candidate)
      (funcall unlessface candidate))))

(defun persona-attributes-to-face (attrs)
  "Make ATTRS usable for face definitions."
  (let ((face-attrs (copy-sequence attrs))
        (inherit (plist-get attrs :inherit)))
    (when inherit
      (plist-put face-attrs
                 :inherit
                 (mapcar #'persona-face-or-unmask inherit)))
    face-attrs))

(defun persona-declare-face (symbol persona doc &rest args)
  "Declare SYMBOL as FACE using PERSONA.
DOC and ARGS are passed to `custom-declare-face'"
  (cl-check-type persona persona)

  (let ((spec (mapcar
               (lambda (spec)
                 (list (car spec)
                       (persona-attributes-to-face (cdr spec))))
               persona)))
    (apply #'custom-declare-face symbol spec doc args)))

(defvar personae-infer-documentation-function
  (lambda (persona)
    (format "Automatically generated face for %s" persona))
  "Function to infer the documentation of a persona.
This function is called with a single argument, which is the
list of symbols that are used to name the persona.
See `persona-unmask'.")

(defvar personae-infer-group-function
  #'identity
  "Function to infer the correct group associated with a symbol.
This function receives one argument of symbol type, which is the
currently guessed group. `identity' should be valid in almost
all cases.")

(defun personae-unmask-and-declare (personae &optional group)
  "Define all PERSONAE as faces.
Optional parameter GROUP is a group to which PERSONAE belong."
  (cl-check-type personae personae)
  (dolist (item personae)
    (let* ((group (if (listp group) group (list group)))
           (persona
            (append group
                   (if (listp (car item))
                       (car item)
                     (list (car item))))))
      (cl-typecase (cdr item)
        (persona
         (persona-declare-face
          (persona-unmask persona #'intern)
          (cdr item)
          (funcall personae-infer-documentation-function
                   persona)
          :group
          (funcall personae-infer-group-function
                   (intern (mapconcat #'symbol-name group "-")))))
        (personae
         (personae-unmask-and-declare
          (cdr item)
          persona))))))

(defun personae-new-at-point ()
  "Create a new buffer to insert new personae at point."
  (interactive)
  (let ((buffer (current-buffer)))
    (switch-to-buffer-other-window
     (generate-new-buffer "*personae*"))
    (kill-all-local-variables)
    (remove-overlays)
    ;; save the original buffer
    (setq-local orig-buffer buffer)
    ;; save the personae widget locally
    (setq-local widget (widget-create 'personae))
    (widget-insert "\n\n")
    (widget-create 'push-button
                   :notify
                   (lambda (&rest ignore)
                     (kill-buffer))
                   "Cancel")
    (widget-create 'push-button
                   :notify
                   (lambda (&rest ignore)
                     (let ((value (widget-value widget)))
                       (with-current-buffer orig-buffer
                         (pp
                          value
                          (current-buffer)))
                       (kill-buffer)))
                   "Apply")
    (use-local-map widget-keymap)
    (widget-setup)))

(defun personae-edit-at-point ()
  "Create a new buffer to edit the personae at point."
  (interactive)
  (save-excursion
    (let ((buffer (current-buffer))
          (p1 (point))
          (value (read (current-buffer)))
          (p2 (point)))
      (cl-check-type value personae)
      (switch-to-buffer-other-window
       (generate-new-buffer "*personae*"))
      (kill-all-local-variables)
      (remove-overlays)
      ;; save the original buffer and region locally to refer to it
      ;; within the widgets
      (setq-local orig-buffer buffer)
      (setq-local orig-buffer-region (list p1 p2))
      ;; save the personae-widget locally
      (setq-local widget (widget-create 'personae value))
      (widget-insert "\n\n")
      (widget-create 'push-button
                     :notify
                     (lambda (&rest ignore)
                       (kill-buffer))
                     "Cancel")
      (widget-create 'push-button
                     :notify
                     (lambda (&rest ignore)
                       (let ((region orig-buffer-region)
                             (value (widget-value widget)))
                        (with-current-buffer orig-buffer
                          (apply #'delete-region region)
                          (pp
                           value
                           (current-buffer)))
                        (kill-buffer)))
                     "Apply")
      (use-local-map widget-keymap)
      (widget-setup))))

(provide 'persona)

;;; persona.el ends here
