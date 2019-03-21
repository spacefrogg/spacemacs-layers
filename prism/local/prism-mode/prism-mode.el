;;; prism-mode.el -- Major mode for editing PRISM files

;; Author: Michael Raitza
;; Created: 25 Sep 2015
;; Keywords: PRISM model checker major-mode

;; Copyright (C) 2015, Michael Raitza <spacefrogg@spacefrogg.net>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:
;;
;; This mode is for editing source files for the PRISM probabilistic
;; model checker.

;;; Code:
(defvar prism-mode-hook nil)

(defvar prism-mode-map
  (let ((map (make-keymap)))
    map)
  "Keymap for PRISM major mode.")

(defconst prism-block-openers-regexp
  (rx symbol-start (or "module" "invariant" "system" "rewards" "init") symbol-end)
  "Keywords that open a new block in the PRISM language.")

(defconst prism-block-closers-regexp
  (rx symbol-start (or "endmodule" "endinvariant" "endsystem" "endrewards" "endinit") symbol-end)
  "Keywords that close a block in the PRISM language.")

(defconst profeat-block-openers-regexp
  (rx symbol-start (or "module" "invariant" "system" "rewards" "init" "feature"))
  "Keywords that close a block in the PROFEAT language.")

(defconst profeat-block-closers-regexp
  (rx symbol-start (or "module" "invariant" "system" "rewards" "init" "feature"))
  "Keywords that close a block in the PROFEAT language.")

(defvar prism-mode-block-openers-regexp nil
  "Buffer local variable that defines the openers depending on PRISM/PROFEAT mode.")

(defvar prism-mode-block-closers-regexp nil
  "Buffer local variable that defines the closers depending on PRISM/PROFEAT mode.")

(defvar prism-font-lock-keywords
  `((,(rx symbol-start
	  (or "const" "ctmc" "dtmc" "endinit" "endinvariant" "endmodule"
	      "endrewards" "endsystem" "formula" "filter" "global"
	      "init" "invariant" "label" "mdp" "module"
	      "nondeterministic" "probabilistic" "pta" "rewards"
	      "stochastic" "system")
	  symbol-end)
      0 font-lock-keyword-face)
    (,(rx symbol-start
	  (or "bool" "clock" "double" "int")
	  symbol-end)
      0 font-lock-type-face)
    (,(rx symbol-start
	  (or "min" "max" "floor" "ceil" "pow" "mod" "log" "func" "P" "Pmin"
	      "Pmax" "R" "Rmin" "Rmax")
	  symbol-end)
      0 font-lock-function-name-face)
    (,(rx symbol-start
	  (or "A" "C" "E" "F" "false" "G" "I" "X" "S" "true" "U" "W" "X")
	  symbol-end)
      0 font-lock-constant-face)
    (,(rx (seq symbol-start
	       (or "module" "formula")
	       symbol-end)
	  (one-or-more space)
	  (group symbol-start
		 (any "a-z" "A-Z" "_")
		 (zero-or-more
		  (any "a-z" "A-Z" "_" "0-9"))
		 symbol-end))
      1 font-lock-function-name-face))
  "Highlighting expressions for PRISM mode.")

(defvar profeat-font-lock-keywords
  `((,(rx symbol-start
	  (or "const" "ctmc" "dtmc" "endinit" "endinvariant" "endmodule"
	      "endrewards" "endsystem" "formula" "filter" "global"
	      "init" "invariant" "label" "mdp" "module"
	      "nondeterministic" "probabilistic" "pta" "rewards"
	      "stochastic" "system" "feature" "endfeature" "modules")
	  symbol-end)
      0 font-lock-keyword-face)
    (,(rx symbol-start
	  (or "bool" "clock" "double" "int")
	  symbol-end)
      0 font-lock-type-face)
    (,(rx symbol-start
	  (or "min" "max" "floor" "ceil" "pow" "mod" "log" "func" "P" "Pmin"
	      "Pmax" "R" "Rmin" "Rmax")
	  symbol-end)
      0 font-lock-function-name-face)
    (,(rx symbol-start
	  (or "A" "C" "E" "F" "false" "G" "I" "X" "S" "true" "U" "W" "X" "id")
	  symbol-end)
      0 font-lock-constant-face)
    (,(rx (seq symbol-start
	       (or "module" "formula" "feature")
	       symbol-end)
	  (one-or-more space)
	  (group symbol-start
		 (any "a-z" "A-Z" "_")
		 (zero-or-more
		  (any "a-z" "A-Z" "_" "0-9"))
		 symbol-end))
      1 font-lock-function-name-face))
  "Highlighting expressions for PROFEAT mode.")

(defun prism-indent-line ()
  "Indent current line of PRISM code."
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
        (indent (condition-case nil (max (prism-calculate-indentation) 0)
                  (error 0))))
    (if savep
        (save-excursion (indent-line-to indent))
        (indent-line-to indent))))

(defun prism-calculate-indentation ()
  "Calculate the indentation of the current line. Returned value
might be negative, in which case it is expected to be set to 0."
  (interactive)
  (let ((closers (concat "^[ \t]*" prism-block-closers-regexp))
        (openers (concat "^[ \t]*" prism-block-openers-regexp)))
    (save-excursion
      (beginning-of-line)
      (cond ((bobp) 0)
            ((looking-at closers)
             (forward-line -1)
             (- (current-indentation) tab-width))
            (t (cl-labels ((indenter (cur-indent)
                                     (forward-line -1)
                                     (cond ((looking-at closers)
                                            (current-indentation))
                                           ((looking-at openers)
                                            (+ (current-indentation) tab-width))
                                           ((bobp) t cur-indent)
                                           (t (indenter cur-indent)))))
                 (indenter 0)))))))

(defvar prism-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry '(?< . ?>) "." st)
    (modify-syntax-entry ?| "." st)
    (modify-syntax-entry ?+ "." st)
    (modify-syntax-entry ?* "." st)
    (modify-syntax-entry ?& "." st)
    (modify-syntax-entry ?/ ". 12b" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for prism-mode.")

;;;###autoload
(defun prism-mode ()
  "Major mode for editing PRISM Probabilistic Model Checker files."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table prism-mode-syntax-table)
  (use-local-map prism-mode-map)
  (set (make-local-variable 'font-lock-defaults) '((prism-font-lock-keywords) nil nil))
  (set (make-local-variable 'indent-line-function) 'prism-indent-line)
  (set (make-local-variable 'prism-mode-openers-regexp) 'prism-block-openers-regexp)
  (set (make-local-variable 'prism-mode-closers-regexp) 'prism-block-closers-regexp)
  (setq major-mode 'prism-mode
        mode-name "Prism")
  (run-hooks 'prism-mode-hook))

;;;###autoload
(define-derived-mode profeat-mode prism-mode
  "Profeat"
  "Major mode for editing PROFEAT files, an extension to the
PRISM Probabilistic Model Checker Language."
  (set (make-local-variable 'prism-mode-openers-regexp) 'profeat-block-openers-regexp)
  (set (make-local-variable 'prism-mode-closers-regexp) 'profeat-block-closers-regexp)
  (set (make-local-variable 'font-lock-defaults) '((profeat-font-lock-keywords) nil nil)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.prism\\'" . prism-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.profeat\\'" . profeat-mode))

(provide 'prism-mode)
(provide 'profeat-mode)


;;; prism-mode.el ends here
