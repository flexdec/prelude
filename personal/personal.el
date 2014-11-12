;;; Personal.el -- Personal emacs settings
;;; Commentary:
;;; Based on prelude! https://github.com/bbatsov/prelude
;;; Code:

(prelude-require-package 'monokai-theme)
(prelude-require-package 'evil-nerd-commenter)
(prelude-require-package 'yasnippet)
(add-to-list 'load-path "~/.emacs.d/personal/elisp/")


;;Requirements
(require 'web-mode)

;;enable yasnippets everywhere
(yas-global-mode)

;;disable prelude keychords actions
(key-chord-define-global "jj" nil)
(key-chord-define-global "xx" nil)
(key-chord-define-global "uu" nil)
(key-chord-define-global "JJ" nil)
(key-chord-define-global "jl" nil)
(key-chord-define-global "jk" nil)
(key-chord-define-global "yy" nil)

;;Map jk to escape and quit stuff
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

;;set evil nerd commenter
(evilnc-default-hotkeys)
(define-key evil-normal-state-map ",," 'evilnc-comment-or-uncomment-lines)

;;windmove setup
(if (eq system-type 'darwin)
    (windmove-default-keybindings 'super)
  (windmove-default-keybindings 'meta))

;;cause its fucking annoying
(setq prelude-whitespace nil)

;; show column number and line number
(dolist (mode '(column-number-mode line-number-mode))
  (when (fboundp mode) (funcall mode t)))

(dolist (mode-hook '(text-mode-hook prog-mode-hook))
  (add-hook mode-hook
            (lambda ()
              (linum-mode 1))))

;; make the fringe thinner 
(fringe-mode 8)

;;Various gui eye candy
(show-paren-mode)
(setq show-paren-style 'expression)
(load-theme 'monokai t)
(scroll-bar-mode -1)

;;WEB-MODE
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

(defun ome-web-mode-hook ()
  ;;Add html snippets
  (yas-activate-extra-mode 'html-mode)
  ;; indentation
  ;; HTML offset indentation
  (setq web-mode-markup-indent-offset 2)
  ;; CSS offset indentation
  (setq web-mode-code-indent-offset 2)
  ;; Script offset indentation (for JavaScript, Java, PHP, etc.)
  (setq web-mode-css-indent-offset 2)
  ;; HTML content indentation
  (setq web-mode-indent-style 2)

  ;; padding
  ;; For <style> parts
  (setq web-mode-style-padding 1)
  ;; For <script> parts
  (setq web-mode-script-padding 1)
  ;; For multi-line blocks
  (setq web-mode-block-padding 0))

(add-hook 'web-mode-hook 'ome-web-mode-hook)

;;add auto complete sources
(setq web-mode-ac-sources-alist
      '(("css" . (ac-source-css-property))
        ("html" . (ac-source-words-in-buffer ac-source-abbrev))
        ("php" . (ac-source-yasnippet ac-source-php-auto-yasnippets))
        ("html" . (ac-source-emmet-html-aliases ac-source-emmet-html-snippets))
        ("css" . (ac-source-css-property ac-source-emmet-css-snippets))))
;;; personal.el ends here
