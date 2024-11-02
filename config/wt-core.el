;;; wt-core.el ---   -*- lexical-binding: t -*-

;; code

;; split the windows and focus on
(defun wt/split-and-follow-vertically ()
  "Split window vertically (below)."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun wt/split-and-follow-horizontally ()
  "Split window horizontally (right)."
  (interactive)
  (split-window-right)
  (other-window 1))

;; Gernal keybinding
;; Zoom in and out
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Bindings for copy and paste
(global-set-key (kbd "M-v") 'clipboard-yank)  ;; Paste
(global-set-key (kbd "M-c") 'kill-ring-save)   ;; Copy

(define-key global-map (kbd "C-k") nil)
(define-key global-map (kbd "C-j") nil)


;;(defun open-all-recent-files ()
;;  "Open all recent files."
;;  (interactive)
;;  (dolist (file  recentf-list) (find-file file)))
;;
;;(define-key global-map (kbd "C-w") 'open-all-recent-files)


;; Toggle between split windows and a single window
(defun wt-toggle-windows-split()
  (interactive)
  (if (not(window-minibuffer-p (selected-window)))
      (progn
        (if (< 1 (count-windows))
            (progn
              (window-configuration-to-register ?u)
              (delete-other-windows))
          (jump-to-register ?u))))
  (my-iswitchb-close))


;; Advise `kill-emacs`
(defun wt/advice-kill-emacs (orig-fun &rest args)
  "Advise `kill-emacs` to confirm before exit."
  (if (yes-or-no-p "Are you sure you want to exit Emacs? ")
      (apply orig-fun args)
    (message "Cancelled exit.")))


;; (defun wt/open-note-in-dired ()
;;   "Set the note dir by system"
;;   (interactive)
;;   (cond
;;    ((eq system-type 'darwin)
;;     (setq  wt/note-dir "/Users/weitingchen/Library/Mobile Documents/iCloud~md~obsidian/Documents/weitingchen")
;;     )
;;    ((eq system-type 'windows-nt)
;;     (setq  wt/note-dir "~/iCloudDrive/iCloud~md~obsidian/weitingchen/"))
;;    )
;;   (dired wt/note-dir)
;;   )

;; (defun wt/find-file-preview ()
;;   (interactive)
;;   (let ((consult-ripgrep-command "rg --multiline --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
;;     (consult-ripgrep)))


;; TODO
;; https://github.com/doomemacs/doomemacs/blob/master/modules/config/default/+evil-bindings.el
;; https://github.com/daviwil/dotfiles/blob/master/.emacs.d/modules/dw-keys-evil.e
(use-package drag-stuff
  :straight t
  :after evil
  :init
  (drag-stuff-mode t)
  )

(use-package evil
  :straight t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-undo-system 'undo-redo)

  (setq select-enable-clipboard t)
  (defalias 'forward-evil-word 'forward-evil-symbol)

  (setq evil-shift-width 2)

  ;; (setq evil-want-fine-undo t)
  ;; (setq evil-ex-visual-char-range t)

  :config
  (evil-mode 1)
  (setq x-select-enable-clipboard t)
  ;; https://evil.readthedocs.io/en/latest/faq.html#underscore-is-not-a-word-character
  (define-key evil-outer-text-objects-map "w" 'evil-a-symbol)
  (define-key evil-inner-text-objects-map "w" 'evil-inner-symbol)
  (define-key evil-outer-text-objects-map "o" 'evil-a-word)
  (define-key evil-inner-text-objects-map "o" 'evil-inner-word)

  (define-key evil-insert-state-map (kbd "C-c") 'evil-normal-state)

  (define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)

  (define-key evil-visual-state-map (kbd "-") 'comment-dwim)
  (define-key evil-normal-state-map (kbd "-") 'comment-line)

  (define-key evil-visual-state-map (kbd "K") 'drag-stuff-up)
  (define-key evil-visual-state-map (kbd "J") 'drag-stuff-down)

  ;; (define-key evil-visual-state-map (kbd ">") 'drag-stuff-right)
  ;; (define-key evil-visual-state-map (kbd "<") 'drag-stuff-left)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  ;; (evil-set-initial-state 'dashboard-mode 'normal)

  ;; folde mode
  (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
  (add-hook 'prog-mode-hook 'hs-minor-mode)
  (define-key evil-normal-state-map (kbd "TAB") 'evil-toggle-fold)
  (define-key evil-normal-state-map (kbd "C-'") #'wt-toggle-windows-split)

  )

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

;; TODO how to use it
(use-package evil-mc
  :straight t
  :after evil
  :config
  (global-evil-mc-mode 1))

;; (use-package evil-surround
;;   :straight t
;;   :after evil
;;   :config
;;   (global-evil-surround-mode t))

;; Main keybinding
(use-package general
  :straight t
  :config
  (general-evil-setup)

  ;; Keybind
  ;; set up 'Ctrl b' as the global systemh key
  (general-create-definer wt/system-key
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "C-b" ;; set leader
    :global-prefix "M-C-b") ;; access leader in insert mode

  (wt/system-key
    "v" #'(wt/split-and-follow-vertically :wk "split and follow vertically")
    "h" #'(wt/split-and-follow-horizontally :wk "split and follow horizontally")
    "r" '(eval-buffer :wk "eval-buffer")
    "s" '(tabspaces-switch-or-create-workspace :wk "switch")
    "n" '(tab-next :wk "buffer next")
    "p" '(tab-previous :wk "buffer pervious")
    "k" '(tabspaces-clear-buffers :wk "kill alll buffers")
    "K" '(tabspaces-kill-buffers-close-workspace :wk "kill others")
    )

  (wt/system-key
    "o" '(:ignore :wk "Notes")
    "ob" #'(wt/open-note-in-dired :wk "cd obsiden folder")
    )

  ;; set up 'SPC' as the global leader key
  (general-create-definer wt/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  ;; main kyes
  (wt/leader-keys
    "/" '(execute-extended-command :wk "consult-M-x")
    "'" '(project-eshell :wk "run eshell")
    "C-'" '(term :wk "run term")
    "c f" (lambda () (interactive) (dired "~/.dotfiles/"))
    ;; "p" #'(paste-from-clipboard :wk "leader p")
    )

  ;; find file
  (wt/leader-keys
    "f"  '(:ignore t :wk "Files")
    "ff" '(consult-fd :wk "fd Find Files")
    "fd" '(find-file :wk "Find Files in current DIR")
    "fo" '(find-file-other-window :wk "Find Files in current DIR with other frame")

    "fg" '(consult-grep :wk "Search for string in files in DIR")
    "fl" '(consult-ripgrep :wk "Search for string current file")

    ;; "fp" '(wt/find-file-preview :wk "Search for string current file")
    ;; "fn" '(counsel-recentf :wk "Find recent files")
    )

  ;; buffer move
  ;; BUG when change buffer it will move to next workspace
  (wt/leader-keys
    "b" '(:ignore t :wk "Bookmarks/Buffers")
    "bb" '(consult-buffer-other-window :wk "Switch buffer")
    "bl" '(ibuffer ibu :wk "List buffers")
    "SPC" '(consult-buffer :wk "Switch buffer")
    "q" '(kill-buffer-and-window :wk "Kill this buffer")
    "o" '(switch-to-next-buffer  :wk "Next buffer")
    "i" '(switch-to-prev-buffer :wk "Previous buffer")
    "br" '(revert-buffer :wk "Reload buffer")
    "bq" '(kill-buffer :wk "Kill buffer")
    )

  ;; window
  (wt/leader-keys
    "w" '(:ignore t :wk "Windows")
    ;; Window splits
    "wq" '(evil-window-delete :wk "Close window")

    ;; Window motions
    "h" '(evil-window-left :wk "Window left")
    "j" '(evil-window-down :wk "Window down")
    "k" '(evil-window-up :wk "Window up")
    "l" '(evil-window-right :wk "Window right")
    "w w" '(evil-window-next :wk "Goto next window")

    ;; Move Windows
    ;; "H" '(buf-move-left :wk "Buffer move left")
    ;; "J" '(buf-move-down :wk "Buffer move down")
    ;; "w K" '(buf-move-up :wk "Buffer move up")
    ;; "L" '(buf-move-right :wk "Buffer move right")
    )

  ;; dir
  (wt/leader-keys
    "d" '(:ignore t :wk "Dir")
    "dd" '(dired :wk "Open dired")
    "dj" '(dired-jump :wk "Open dired jump current")
    )

  ;; toggle
  (wt/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "tt" '(lsp-treemacs-error-list :wk "Error list")
    "to" '(org-mode :wk "Toggle org mode")
    "tr" '(rainbow-mode :wk "Toggle rainbow mode")
    "tf" '(flycheck-mode :wk "Toggle check mode")
    )

  ;; reload
  ;; (wt/leader-keys
  ;;   "h" '(:ignore t :wk "Help")
  ;;   "hr" '((lambda () (interactive)
  ;;               (load-file "~/.emacs.d/init.el")
  ;;               (ignore (elpaca-process-queues)))
  ;;             :wk "Reload emacs config")
  ;;   )

  )

;; hightlight yank
(use-package evil-goggles
  :straight t
  :config
  (evil-goggles-mode)
  ;; (evil-goggles-use-diff-faces)
  :custom
  (setq evil-goggles-duration 0.3)
  )
(set-face-attribute 'evil-goggles-yank-face nil
                    :foreground "white"
                    :background "yellow"
                    :weight 'bold)
;; clean white space
(add-hook 'before-save-hook 'whitespace-cleanup)


(use-package diminish
  :straight t
  )

;; which-key
(use-package which-key
  :straight t
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.1)
  )

;; maybe check this https://github.com/casouri/vundo
;; (use-package undo-tree
;;   :straight t
;;   :config
;;   (global-undo-tree-mode)
;;   :custom
;;   ;; on windows is really slow
;;   (setq undo-tree-history-directory "~/.emacs.d/undo")
;;   (setq undo-tree-auto-save-history t)
;; )
;;
;; (with-eval-after-load 'evil
;;   (when (bound-and-true-p global-undo-tree-mode)
;;     (define-key evil-normal-state-map (kbd "u") 'undo-tree-undo)
;;     (define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo))
;;   ;; (global-set-key (kbd "C-x u") 'undo-tree-visualize)
;; )


;;; UI
;; (use-package all-the-icons
;;   :straight (all-the-icons :type git :host github :repo "domtronn/all-the-icons.el" :branch "svg" :files (:defaults "svg"))
;;   :if (display-graphic-p))

;; indent mode and highlight
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

(use-package minions
  :straight t
  :demand t
  :config
  (minions-mode 1)
  )

(set-face-attribute 'mode-line nil
                    ;; :height 1.2
                    ;; :background "#0D0907"
                    :box nil)

(set-face-attribute 'mode-line-inactive nil
                    ;; :background "#0D0907"
                    ;; :height 1.2
                    :box nil)

(defun wt-modeline-format (left right)
  "Return a string of `window-width' length containing LEFT and RIGHT, aligned respectively."
  (let* ((left (format-mode-line left))
         (right (format-mode-line right))
         (reserve (+ 2 (length right))))
    (when (and (display-graphic-p) (eq 'right (get-scroll-bar-mode)))
      (setq reserve (- reserve 3)))
    (concat
     left
     " "
     (propertize  " "
                  'display `((space :align-to (- (+ right right-fringe right-margin)
                                                 ,reserve)
                                    :ascent 50)))
     right)))

;; (defvar wt-mode-line-format
;;   '((:eval
;;      (jp-modeline-format
;;       ;; left
;;       '((:eval ,(format-mode-line evil-mode-line-tag))
;;         (vc-mode vc-mode)
;;         " "
;;         (:eval (all-the-icons-icon-for-buffer))
;; )
;;       ;; right
;;       `((:eval ,(format-mode-line mode-line-modes))
;;         (:eval ,(format-mode-line vc-mode))
;;         (:eval ,(format-mode-line mode-line-misc-info))))))
;;   "Custom mode line format.")

;; (setq-default mode-line-format wt-mode-line-format)

;; (setq-default mode-line-format
;;               '((:eval
;;                  (simple-mode-line-render
;;                   ;; Left.
;;                   (quote ("%e "
;;                           evil-mode-line-tag
;;                           "[%*]"
;;                           " "
;;                           (:propertize
;;                            ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote))
;;                           " "
;;                           " %l : %c"
;;                           " "
;;                           (vc-mode vc-mode)
;;                           " "
;;                           (:eval (all-the-icons-icon-for-buffer))
;;                           " "
;;                           mode-line-buffer-identification
;;                           )
;;                          )
;;                   ;; Right.
;;                   (quote ("%p "
;;                           ;; mode-line-format-right-align
;;                           (:eval (format " Tab (%d) " tab-width))
;;                           mode-line-frame-identification
;;                           (:eval (all-the-icons-icon-for-mode))
;;                           mode-line-modes
;;                           mode-line-misc-info)))
;;                  )
;;                 )
;;               )

;; ;; Example: disable mode line in text mode
;; (add-hook 'text-mode-hook 'disable-mode-line)

;; cursor
(custom-set-faces
 '(cursor ((t (:background "#eb6f92" :foreground "white")))))

(use-package rainbow-mode
  :straight t
  :defer t
  )

;;TODO
(use-package hl-todo
  :straight t
  :defer t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(
          ("TODO "    . (:foreground "#232136" :background "#f6c177" :weight 'bold))
          ("NOTE "    . (:foreground "#232136" :background "#6e6a86" :weight 'bold))
          ("BUG "     . (:foreground "#232136" :background "#eb6f92" :weight 'bold))
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

;; (text       "#e0def4")  ;; foreground
;; (base       "#232136")  ;; background
;; (high       "#393552")  ;; highlight
;; (gold       "#f6c177")  ;; critical
;; (iris       "#c4a7e7")  ;; salient
;; (surface    "#6e6a86")  ;; strong
;; (love       "#eb6f92")  ;; popout
;; (subtle     "#2a273f")  ;; subtle
;; (faded      "#6e6a86")  ;; faded
;; (cursor     "#c4a7e7")) ;; cursor

;; ("HACK"       font-lock-constant-face bold)
;; ("REVIEW"     font-lock-keyword-face bold)
;; (("HOLD" . "#d0bf8f")
;;  ("TODO" . "#cc9393")
;;  ("NEXT" . "#dca3a3")
;;  ("THEM" . "#dc8cc3")
;;  ("PROG" . "#7cb8bb")
;;  ("OKAY" . "#7cb8bb")
;;  ("DONT" . "#5f7f5f")
;;  ("FAIL" . "#8c5353")
;;  ("DONE" . "#afd8af")
;;  ("NOTE" . "#d0bf8f")
;;  ("MAYBE" . "#d0bf8f")
;;  ("KLUDGE" . "#d0bf8f")
;;  ("HACK" . "#d0bf8f")
;;  ("TEMP" . "#d0bf8f")
;;  ("FIXME" . "#cc9393")
;;  ("XXXX*" . "#cc9393"))


;; TODO help fuction
;; https://github.com/Wilfred/helpful
;; (use-package helpful
;;   :defer t
;;   :commands (helpful-callable helpful-variable helpful-command helpful-key helpful-at-point)
;;   :bind
;;   ;; Remap standard help commands to helpful versions
;;   ([remap describe-function] . helpful-function)    ;; Remaps C-h f
;;   ([remap describe-command] . helpful-command)      ;; Remaps C-h x
;;   ([remap describe-variable] . helpful-variable)    ;; Remaps C-h v
;;   ([remap describe-key] . helpful-key)              ;; Remaps C-h k
;;   ;; Bind helpful-at-point to a custom key (optional)
;;   ("C-h p" . helpful-at-point)
;;   )                   ;; Quickly show info at point

(provide 'wt-core)
;;; wt-core.el ends here
