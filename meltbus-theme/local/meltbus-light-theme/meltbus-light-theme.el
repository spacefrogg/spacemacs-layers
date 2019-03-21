;;; meltbus-light-theme.el --- Adaptation to eltbus-theme, an experimental theme that puts subtle back into emacs

;; Copyright (C) 2017-2018 the eltbus-theme authors and Michael Raitza

;; Author: Michael Raitza <spacefrogg-meltbus@meterriblecrew.net>
;;
;; Version: 0.1
;; Keywords: color, theme
;; Package-Requires: ()

;; This theme is based on eltbus theme.

;; This file is not part of Emacs.

;;; Commentary;

;;; Code:

(deftheme meltbus-light "Light variant of meltbus")

;; (custom-make-theme-feature 'meltbus)

(let* ((class '((class color) (min-colors 89)))
       (black  "#000000")
       (white "#ffffff")
       (text-white "#dddddd")
       (brighter-text "#cccccc")
       (dull-white "#888888")
       ;; (almost-invisible "#444444")
       (almost-invisible "#555555")
       (bright-black "#111111")
       (reddish "red";; "#f8c0c0"
                )
       (bluish "royal blue";; "#87afff"
               )
       (greenish "#c0f860")
       (yellowish "#cdad00")
       (magentaish "#db7093")
       (cyanish "#48d1cc")
       (dull-greenish "#448844")
       (dull-reddish "#aa4444")
       (dull-bluish "#7070aa")
       (default-bgc white)
       (font-base "Dejavu Sans Mono")
       (default-color `(:foreground ,black))
       (default-font `(:font ,font-base :height 80))
       (default-background `(:background ,default-bgc))
       (brighter `(:foreground ,brighter-text))
       (lessbright `(:foreground ,almost-invisible))
       (standout-dull `(:foreground ,bright-black :background ,text-white))
       (standout-bright `(:foreground ,bright-black :background ,white))
       (less-distraction `(:foreground ,almost-invisible))
       (link-like `(:background ,white :foreground ,bluish))
       (success-like `(:background ,white :foreground ,dull-greenish))
       (err-like `(:background ,white :foreground ,reddish))
       (fmt-none  `(:weight normal :slant normal :underline nil :inverse-video nil))
       (fmt-bld `(:weight bold :slant normal))
       (fmt-bldi `(:weight bold :slant italic))
       (fmt-und `(:weight normal : slant normal :underline t :inverse-video nil))
       (fmt-rev `(:weight normal : slant normal :underline nil :inverse-video t))
       (fmt-revb `(:weight bold : slant normal :underline nil :inverse-video t)))
  (custom-theme-set-faces
   'meltbus-light
   ;; Comments are things that are missing

   `(cursor ((,class (:background ,brighter-text))))
   `(default ((t (:foreground "black" :background "white" ,@default-font :inherit nil))
              (,class (:foreground "black" :background "white" ,@default-font :inherit nil))))

   `(error ((,class (,@err-like ,@fmt-none))))
   `(escape-glyph-face ((,class (,@err-like ,@fmt-none))))
   `(fringe ((,class (,@standout-dull ,@fmt-none))))
   `(linum ((,class (,@less-distraction ,@fmt-none))))

   `(hl-line ((,class (:underline t))))
   `(highlight ((,class (,@fmt-bld))))
   `(region ((,class (:inverse-video t))))
   `(isearch ((,class (:background ,greenish :foreground ,default-bgc))))
   `(isearch-fail ((,class (:background ,reddish :foreground ,default-bgc))))
   `(isearch-lazy-highlight-face ((,class (:background ,bluish :foreground ,default-bgc))))
   `(lazy-highlight ((,class (:background ,bluish :foreground ,white))))
   `(link ((,class (,@link-like ,@fmt-und))))
   `(link-visited ((,class (,@link-like ,@fmt-und))))

   ;; menu
   `(minibuffer-prompt ((,class (,@standout-dull ,@fmt-bld))))
   `(mode-line ((,class (,@standout-dull ,@fmt-rev))))
   `(mode-line-inactive ((,class (,@standout-dull ,@fmt-rev))))

   `(secondary-selection ((,class (:background ,dull-greenish))))
   `(shadow ((,class (,@less-distraction))))
   `(trailing-whitespace ((,class (,@err-like ,@fmt-rev))))

   ;; comint-highlight-prompt
   `(compilation-info ((,class (,@success-like))))
   `(compilation-warning ((,class (,@err-like ,@fmt-bld))))

   `(custom-button ((,class (,@success-like ,@fmt-none))))
   `(custom-button-mouse ((,class (,@success-like ,@fmt-none))))
   `(custom-button-pressed ((,class (,@success-like ,@fmt-none))))
   ;; custom-changed
   ;; custom-comment-tag
   ;; custom-documentation
   `(custom-group-tag ((,class (,@fmt-bld))))
   ;; custom-group-tag-1
   ;; custom-invalid
   ;; custom-link
   `(custom-state ((,class (,@fmt-bld))))
   `(custom-variable-tag ((,class (:inherit custom-group-tag))))

   `(diff-added ((,class (:foreground ,dull-greenish))))
   ;; diff-header
   `(diff-hunk-header ((,class (:foreground ,brighter-text :background ,almost-invisible))))
   ;; diff-file-header
   `(diff-refine-removed ((,class (:foreground ,default-bgc :background ,dull-reddish))))
   `(diff-refine-added ((,class (:foreground ,default-bgc :background ,dull-greenish))))
   `(diff-removed ((,class (:foreground ,dull-reddish))))

   `(ido-only-match ((,class (,@success-like ,@fmt-none))))
   `(ido-subdir ((,class (,@default-color ,@fmt-bld))))
   `(ido-first-match ((,class (,@link-like ,@fmt-und))))

   `(ivy-current-match ((,class (:background ,default-bgc :foreground ,bluish ,@fmt-bld :underline t))))
   `(ivy-minibuffer-match-face-1 ((,class (:inherit lazy-highlight))))
   `(ivy-minibuffer-match-face-2 ((,class (:inherit lazy-highlight))))
   `(ivy-prompt-match ((,class (,@link-like ,@fmt-none))))
   `(ivy-remote ((,class (,@success-like ,@fmt-none))))
   `(ivy-virtual ((,class (:background ,default-bgc :foreground ,dull-white ,@fmt-bld))))


   ;; emacs-wiki-bad-link-face
   ;; emacs-wiki-link-face
   ;; emacs-wiki-verbatim-face

   ;; eshell-ls-archive
   ;; eshell-ls-backup
   ;; eshell-ls-clutter
   ;; eshell-ls-directory
   ;; eshell-ls-executable
   ;; eshell-ls-missing
   ;; eshell-ls-product
   ;; eshell-ls-readonly
   ;; eshell-ls-special
   ;; eshell-ls-symlink
   ;; eshell-ls-unreadable
   ;; eshell-prompt

   `(font-latex-warning-face ((,class (,@err-like ,@fmt-none))))
   ;; font-latex-sectioning-5-face

   `(font-lock-builtin-face ((,class (,@lessbright ,@fmt-bld))))
   ;; font-lock color-constant-face
   `(font-lock-comment-delimiter-face ((,class (,@lessbright ,@fmt-none))))
   `(font-lock-comment-face ((,class (,@less-distraction ,@fmt-none))))
   `(font-lock-constant-face ((,class (,@lessbright ,@fmt-none))))
   `(font-lock-doc-face ((,class (,@less-distraction ,@fmt-bld))))
   ;; font-lock-doc-string-face
   ;; font-lock-exit-face
   `(font-lock-function-name-face ((,class (,@lessbright ,@fmt-bld))))
   `(font-lock-keyword-face ((,class (,@lessbright ,@fmt-none))))
   ;; font-lock-negation-char-face
   ;; font-lock-other-type-face
   `(font-lock-preprocessor-face ((,class (,@standout-dull ,@fmt-none))))
   `(font-lock-other-emphasized-face ((,class (,@default-color ,@fmt-bld))))
   ;; font-lock-regexp-grouping-backslash
   ;; font-lock-reference-face
   `(font-lock-string-face ((,class (,@standout-dull ,@fmt-none))))
   `(font-lock-type-face ((,class (,@lessbright ,@fmt-none))))
   `(font-lock-variable-name-face ((,class (,@default-color ,@fmt-none))))
   `(font-lock-warning-face ((,class (,@err-like ,@fmt-none))))
   `(font-lock-special-keyword-face ((,class (,@brighter ,@fmt-bldi))))

   ;; info-xref
   ;; info-xref-visited

   `(magit-item-highlight ((,class (,@fmt-bld))))
   `(magit-log-sha1 ((,class (,@link-like))))
   `(magit-diffstat-added ((,class (:inherit diff-added))))
   `(magit-diffstat-removed ((,class (:inherit diff-removed))))
   `(magit-diff-added ((,class (:inherit diff-added))))
   `(magit-diff-removed ((,class (:inherit diff-removed))))
   `(magit-diff-context-highlight ((,class (:foreground ,dull-white :background ,bright-black))))
   `(magit-diff-added-highlight ((,class (:inherit magit-diff-context-highlight :foreground ,dull-greenish))))
   `(magit-diff-removed-highlight ((,class (:inherit magit-diff-context-highlight :foreground ,dull-reddish))))
   `(magit-diff-hunk-heading ((,class (:inherit diff-hunk-header))))
   `(magit-diff-whitespace-warning ((,class (:foreground ,default-bgc :background ,reddish))))
   `(magit-process-ng ((,class (:inherit magit-section-heading :foreground ,reddish))))
   `(magit-process-ok ((,class (:inherit magit-section-heading :foreground ,greenish))))
   `(magit-section-heading ((,class (,@fmt-bld))))
   `(magit-section-highlight ((,class (:background ,text-white))))
   `(magit-branch-remote ((,class (,@standout-bright ,@fmt-bld))))
   `(magit-branch-local ((,class (:inherit magit-branch-remote :inverse-video t))))
   `(magit-tag ((,class (:foreground ,dull-greenish))))
   `(magit-log-author ((,class (,@fmt-none :foreground ,dull-white))))

   `(Man-overstrike ((,class (,@fmt-bld))))

   `(match ((,class (:inherit lazy-highlight))))

   `(notmuch-tag-face ((,class (:foreground ,bluish))))
   `(notmuch-tree-match-tag-face ((,class (:inherit notmuch-tag-face))))
   `(notmuch-tree-match-author-face ((,class (:inherit notmuch-tree-match-tag-face))))
   `(notmuch-crypto-signature-bad ((,class (:foreground ,reddish))))
   `(notmuch-crypto-signature-good ((,class (:foreground ,almost-invisible))))
   `(notmuch-crypto-signature-unknown ((,class (:foreground ,reddish))))
   `(notmuch-crypto-signature-good-key ((,class (:foreground ,reddish))))
   `(notmuch-crypto-decryption ((,class (:foreground ,reddish))))
   `(notmuch-message-summary-face ((,class (:background ,bright-black))))

   `(org-document-title ((,class (:height 1.7 ,@link-like :weight bold :slant normal))))
   `(org-date ((,class (,@link-like))))
   `(org-date-selected ((,class (:inherit lazy-highlight))))
   `(org-done ((,class (,@success-like ,@fmt-bld))))
   `(org-done-keyword-face ((,class (,@success-like ,@fmt-none))))
   `(org-block ((,class (,@fmt-none))))
   `(org-block-begin-line ((,class (,@lessbright ,@fmt-bld))))
   `(org-block-end-line ((,class (,@lessbright ,@fmt-bld))))
   `(org-level-1 ((,class (:height 1.5 :background ,text-white :foreground ,black ,@fmt-bld))))
   `(org-level-2 ((,class (:height 1.4 :background ,text-white ,@fmt-bld))))
   `(org-level-3 ((,class (:height 1.3 :background ,text-white ,@fmt-bld))))
   `(org-level-4 ((,class (:height 1.2 :background ,text-white ,@fmt-bld))))
   `(org-level-5 ((,class (:height 1.1 :background ,text-white ,@fmt-bld))))
   `(org-level-6 ((,class (:height 1.0 :background ,text-white ,@fmt-bld))))
   `(org-level-7 ((,class (:height 1.0 :background ,text-white ,@fmt-none))))
   `(org-level-8 ((,class (:height 1.0 :background ,text-white :foreground ,bright-black ,@fmt-none))))
   `(org-todo ((,class (,@err-like ,@fmt-bld))))
   `(org-todo-keyword-face ((,class (,@err-like ,@fmt-none))))

   ;; outline-1
   ;; outline-2
   ;; outline-3
   ;; outline-4
   ;; outline-5
   ;; outline-6
   ;; outline-7
   ;; outline-8
;;; powerline
   `(powerline-active1 ((,class (,@standout-bright))))
   `(powerline-active2 ((,class (,@standout-bright))))
   `(powerline-inactive1 ((,class (:background ,default-bgc ,@less-distraction))))
   `(powerline-inactive2 ((,class (:background ,default-bgc ,@less-distraction))))

   ;; speedbar-button-face
   ;; speedbar-directory-face
   ;; speedbar-file-face
   ;; speedbar-highlight-face
   ;; speedbar-selected-face
   ;; speedbar-separator-face
   ;; speedbar-tag-face
   `(sh-heredoc ((,class (,@standout-dull ,@fmt-none))))
   `(sh-quoted-exec ((,class (,@default-color ,@fmt-bld))))
   `(show-paren-match ((,class (:background ,dull-bluish :foreground ,default-bgc))))
   `(show-paren-mismatch ((,class (:background ,dull-reddish :foreground ,default-bgc))))

   `(smerge-markers ((,class (:inherit magit-diff-conflict-heading))))
   `(smerge-mine ((,class (:inherit diff-removed))))
   `(smerge-other ((,class (:inherit diff-added))))
   `(smerge-refined-removed ((,class (:inherit diff-refine-removed))))
   `(smerge-refined-added ((,class (:inherit diff-refine-added))))

   `(spacemacs-transient-state-title-face ((,class (:inherit mode-line))))

   `(table-cell ((,class (,@default-color ,@fmt-none))))

   ;;vertical-border

   `(widget-field ((,class (,@standout-bright))))
   ;; widget-single-line

   `(flymake-errline ((,class (,@err-like ,@fmt-bld))))
   `(flymake-warnline ((,class (,@err-like ,@fmt-none))))

   ;; column-marker-1
   ;; column-marker-2
   ;; column-marker-3

   ;; jabber-stuff
   ;; gnus-stuff
   `(message-cited-text ((,class (,@standout-dull))))
   `(message-header-cc ((,class (,@fmt-bld))))
   `(message-header-name ((,class (,@fmt-none :foreground ,dull-white))))
   `(message-header-newsgroup ((,class (,@standout-dull))))
   `(message-header-other ((,class (,@less-distraction))))
   `(message-header-subject ((,class (,@standout-dull))))
   ;;`(message-header-subject ((,class (,@standout-bright))))
   `(message-header-to ((,class (,@fmt-bld))))
   `(message-header-xheader ((,class (,@less-distraction))))
   `(message-header-mml ((,class (,@less-distraction))))
   `(message-header-separator ((,class (,@fmt-none :foreground ,bluish))))

   `(paren-face ((,class (,@default-color))))
   `(persp-selected-face ((,class (,@fmt-bld))))
   ;; rainbow-delimiters-stuff
   ;; slime-stuff
   `(slime-repl-input-face ((,class (,@fmt-none))))

   `(whitespace-empty ((,class ,@err-like)))
   `(whitespace-line ((,class ,@err-like)))
   ;; rcirc-stuff
   ;; erc-stuff

   )
  (setq frame-background-mode 'light)
  (mapc 'frame-set-background-mode (frame-list)))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'meltbus-light)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; meltbus-light-theme.el ends here
