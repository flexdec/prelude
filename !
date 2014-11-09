(key-chord-define-global "jj" nil)
(key-chord-define-global "xx" nil)
(key-chord-define-global "uu" nil)
(key-chord-define-global "JJ" nil)
(key-chord-define-global "jl" nil)
(key-chord-define-global "jk" nil)
(key-chord-define-global "yy" nil)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-normal-state-map "jk"  'keyboard-quit)
(key-chord-define evil-visual-state-map "jk" 'keyboard-quit)
(key-chord-define minibuffer-local-map "jk" 'minibuffer-keyboard-quit)
(key-chord-define minibuffer-local-ns-map "jk" 'minibuffer-keyboard-quit)
(key-chord-define minibuffer-local-completion-map "jk" 'minibuffer-keyboard-quit)
(key-chord-define minibuffer-local-must-match-map "jk" 'minibuffer-keyboard-quit)
(key-chord-define minibuffer-local-isearch-map "jk" 'minibuffer-keyboard-quit)
;;Change the modeline color when the evil mode changes
(defun my-evil-modeline-change (default-color)
    "changes the modeline color when the evil mode changes"
    (let ((color (cond ((evil-insert-state-p) '("#002233" . "#ffffff"))
                       ((evil-visual-state-p) '("#330022" . "#ffffff"))
                       ((evil-normal-state-p) default-color)
                       (t '("#440000" . "#ffffff")))))
      (set-face-background 'mode-line (car color))
      (set-face-foreground 'mode-line (cdr color))))

  (lexical-let ((default-color (cons (face-background 'mode-line)
                                     (face-foreground 'mode-line))))
    (add-hook 'post-command-hook (lambda () (my-evil-modeline-change default-color))))

(evilnc-default-hotkeys)
(define-key evil-normal-state-map ",," 'evilnc-comment-or-uncomment-lines)

(windmove-default-keybindings 'meta)

(setq prelude-whitespace nil)

;; show column number and line number
(dolist (mode '(column-number-mode line-number-mode))
  (when (fboundp mode) (funcall mode t)))

(dolist (mode-hook '(text-mode-hook prog-mode-hook))
  (add-hook mode-hook
            (lambda ()
              (linum-mode 1))))

;; make the fringe thinner (default is 8 in pixels)
(fringe-mode 8)

;; show parenthesis match
(show-paren-mode)
(setq show-paren-style 'expression)
(load-theme 'monokai t)
(scroll-bar-mode -1)
