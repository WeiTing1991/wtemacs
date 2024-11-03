;;; wt-ui.el ---   -*- lexical-binding: t -*-

;; code
;; Icon
(use-package all-the-icons
  :straight (all-the-icons :type git :host github :repo "domtronn/all-the-icons.el" :branch "svg" :files (:defaults "svg"))
  :demand t
  :if (display-graphic-p)
)

;; Indent mode line and highlight
;; check https://github.com/DarthFennec/highlight-indent-guides
(use-package highlight-indent-guides
  :straight t
  :defer t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?▏)
  (setq highlight-indent-guides-responsive 'top)
  (setq highlight-indent-guides--bitmap-line t)

  (setq highlight-indent-guides-auto-enabled nil)
  (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  (set-face-background 'highlight-indent-guides-even-face "darkgray")
  (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  (setq highlight-indent-guides-delay 0)
  )


;; Modeline
;; https://github.com/domtronn/all-the-icons.el/tree/svg
;; Hide the mode line globally
;; https://www.reddit.com/r/emacs/comments/6ftm3x/share_your_modeline_customization/


(set-face-attribute 'mode-line nil
                    :height 1.05
                    :background bella-color-black-blue
                    :box nil)

(set-face-attribute 'mode-line-inactive nil
                    :height 1.05
                    :background bella-color-black
                    :box nil)

(setq evil-normal-state-tag   (propertize "  NORMAL  " 'face `((:background ,bella-color-high :foreground ,bella-color-white )))
      evil-insert-state-tag   (propertize "  INSERT  " 'face `((:background ,bella-color-gold :foreground ,bella-color-black )))
      evil-visual-state-tag   (propertize "  VISUAL  " 'face `((:background ,bella-color-grey :foreground ,bella-color-white )))
      evil-emacs-state-tag    (propertize "  EMACS  " 'face `((:background ,bella-color-grey :foreground ,bella-color-white )))
      evil-motion-state-tag   (propertize "  MOTION  " )
      evil-operator-state-tag (propertize "  OPERATOR  ")
)

(setq-default mode-line-format
              '(
                (:eval evil-mode-line-tag)
                " "
                (vc-mode vc-mode)
                " "
                (:eval (all-the-icons-icon-for-buffer))
                " " ;; Separator
                mode-line-buffer-identification

                "   " ;; Separator
                "   " ;; Separator
                "   " ;; Separator
                "   " ;; Separator
                ;; "%="

                ;; mode-line-format-right-align
                " %l:%c:%p "
                (:eval (format " tab:%d " tab-width))
                ;; mode-line-frame-identification
                " "
                " "
                mode-line-modes
                ;; mode-line-misc-info
              )
              )

;;                           "[%*]"
;;                           (:propertize
;;                            ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote))
;; ;; Example: disable mode line in text mode
;; (add-hook 'text-mode-hook 'disable-mode-line)

(use-package minions
  :straight t
  :demand t
  :config
  (minions-mode 1)
  )

;; cursor
(custom-set-faces
 '(cursor ((t (:background "#eb6f92" :foreground "white")))))

(use-package rainbow-mode
  :straight t
  :defer t
  )

(use-package hl-todo
  :straight t
  :defer t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(
          ("TODO"    . (:foreground "#232136" :background ,bella-color-gold :weight 'bold))
          ("NOTE"    . (:foreground "#232136" :background ,bella-color-grey :weight 'bold))
          ("BUG"     . (:foreground "#232136" :background ,bella-color-love :weight 'bold))
          ("DEPRECATED " font-lock-doc-face bold)))
  )


;; line-column
(setq-default display-fill-column-indicator-column 120)
(setq-default display-fill-column-indicator-character ?┆)
;; (set-face-foreground 'fill-column-indicator "grey")

(dolist (mode '(prog-mode-hook))
  (add-hook mode (lambda ()
                   (display-line-numbers-mode 1)
                   (display-fill-column-indicator-mode 1)
                   ))
  )

(provide 'wt-ui)
;;; wt-ui.el ends here
