;;; elispfl.el --- Extra font locks made your Elisp mode fancy  -*- lexical-binding: t -*-

;; Copyright (C) 2019 Zhu Zihao

;; Author: Zhu Zihao <all_but_last@163.com>
;; URL: https://github.com/cireu/elispfl
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: lisp

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Run `elispfl-mode', made your emacs-lisp mode much more fancy!

;;; Code:

(require 'advice)
(require 'font-lock)

(defgroup elispfl nil
  "Enhanced font lock for `emacs-lisp-mode'."
  :prefix "elispfl-"
  :group 'faces)

(defface elispfl-function-name-face
  '((t :inherit font-lock-function-name-face))
  "Face used to highlight function names."
  :group 'elispfl)

(defface elispfl-builtin-function-name-face
  '((t :inherit font-lock-constant-face))
  "Face used to highlight built-in function names."
  :group 'elispfl)

(defface elispfl-special-variable-name-face
  '((t :inherit font-lock-variable-name-face))
  "Face used to highlight special variable names."
  :group 'elispfl)

(defface elispfl-face-name-face
  '((t :inherit default))
  "Face used to highlight face names."
  :group 'elispfl)

(defcustom elispfl-face-use-itself
  nil
  "Non-nil means highlight face name by the face itself instead of `elispfl-face-name-face'"
  :group 'elispfl
  :type 'boolean)

(defvar elispfl-face nil
  "A variable to hold current face used to render.")

(defun elispfl--real-function (sym)
  "Unwinding function chain of SYM and return real function definition.

Sign: (-> Sym Fn)

All aliases and advices will be removed."
  (let ((fn (indirect-function sym)))
    (while (let* ((unadvised (ad-get-orig-definition fn))
                  (unaliased (indirect-function unadvised)))
             (setq fn unaliased)
             (not (eq unaliased unadvised))))
    fn))

(defun elispfl--get-face (sym &optional subr-call?)
  "Get appropriate face for SYM.

Sign: (->* (Sym) (Bool) (Option (U 'elispfl-builtin-function-name-face
                                   'elispfl-special-variable-name-face
                                   'elispfl-function-name-face)))

If SUBR-CALL?, means SYM is appeared in a subroutine call form."
  (cond ((booleanp sym) nil)
        (subr-call?
         (when (fboundp sym)
           (let ((real-fn (elispfl--real-function sym)))
             ;; Macro and special-form already had font lock.
             (unless (or (macrop real-fn)
                         (special-form-p real-fn))
               (if (subrp real-fn)
                   'elispfl-builtin-function-name-face
                 'elispfl-function-name-face)))))
        ((facep sym)
         (if elispfl-face-use-itself
             sym
             'elispfl-face-name-face))
        ((special-variable-p sym)
         'elispfl-special-variable-name-face)))

(defsubst elispfl-inside-code? ()
  "Return t if current point not in comment or string.

Sign: (-> Bool)"
  (not (save-excursion
         (let ((ppss (syntax-ppss)))
           (or (nth 3 ppss) (nth 4 ppss))))))

(defun elispfl-extra-fontlock-matcher! (end)
  "Match defined variables and functions in current buffer with limited to END.

Sign: (-> Long Bool)

Functions are differentiated into special forms, built-in functions and
library/userland functions."
  (catch 'stop
    (while (re-search-forward "\\_<.+?\\_>" end t)
      (when (elispfl-inside-code?)
        (let* ((sym (intern-soft (match-string-no-properties 0)))
               ;; NOTE: We treat symbol after left round bracket as subroutine.
               ;; May trigger false positive in list literal e.g. '(foo bar),
               ;; but it's suitable for most cases.
               (subr-call? (eq (char-before (match-beginning 0)) ?\())
               (face (elispfl--get-face sym subr-call?)))
          (when face
            (setq elispfl-face face)
            (throw 'stop t)))))
    nil))

;;;###autoload
(define-minor-mode elispfl-mode
  "Enhanced font lock for `emacs-lisp-mode'."
  :global t
  (let ((keywords-alist
         '((elispfl-extra-fontlock-matcher! . elispfl-face)))
        (executor (if elispfl-mode
                      #'font-lock-add-keywords
                    #'font-lock-remove-keywords)))
    (funcall executor 'emacs-lisp-mode keywords-alist)
    (funcall executor 'lisp-interaction-mode keywords-alist)
    (font-lock-flush)))

(provide 'elispfl)
;;; elispfl.el ends here
