(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; packages to be installed
(ensure-package-installed
 'helm
 'magit
 'zenburn-theme
 'web-mode
 'projectile
 'helm-projectile
 'use-package
 's
 'dumb-jump
 'google-this
 'smart-mode-line
 'highlight-symbol
 'multiple-cursors
 'phi-search
 'company
 'whitespace-cleanup-mode
 )

(setq shell-file-name "/bin/bash")

(setq confirm-kill-emacs 'y-or-n-p)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq-default indent-tabs-mode nil)

;; web mode
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.djhtml?\\'" . web-mode)
         ("\\.tpl" . web-mode)
         ("\\.jsx" . web-mode))
  :config
  (defun m/current-buffer-django-p ()
    (save-excursion
      (search-forward-regexp "{% base\\|{% if\\|{% for\\|{% include\\|{% block\\|{% csrf_token %}\\|{% url\\|{{ "
                             nil
                             t)))
  (setq web-mode-engines-alist
        '(("django". "\\.djhtml")
          ("django" . m/current-buffer-django-p)
          ("php" . "\\.php")))
  (setq web-mode-content-types-alist
        '(("jsx"  . "\\.jsx")))
  (define-key web-mode-map (kbd "C-;") nil)
  (setq-default web-mode-markup-indent-offset 2)
  (add-hook 'web-mode-hook (lambda () (electric-pair-local-mode 0))))

;; temp file save directory
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)

;; (add-to-list 'load-path "/Users/conradframe/Documents/gdscript-mode")
;; (require 'gdscript-mode)

;; turn off bell
(setq ring-bell-function 'ignore)

;; magit bindings
(global-set-key (kbd "C-x g") 'magit-status)

;; helm bindings
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
; (global-set-key (kbd "s-g") 'helm-grep-do-git-grep)
; custom function to grep entire repo
(defun my-helm-grep-do-git-grep (not-all)
  (interactive "P")
  (helm-grep-git-1 default-directory (null not-all)))

(global-set-key (kbd "s-g") 'my-helm-grep-do-git-grep)

;; projectile
(setq projectile-enable-caching t)
(projectile-global-mode)
(global-set-key (kbd "C-x C-p") 'projectile-find-file)
;;(define-key projectile-mode-map [?\s-g] 'projectile-grep)
(define-key projectile-mode-map [?\s-f] 'projectile-find-file)

;; helm-projectile
(helm-projectile-on)

;; dumb-jump
(dumb-jump-mode)

;; theme
(load-theme 'zenburn t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (multiple-cursors zenburn-theme web-mode use-package undo-tree smart-mode-line nyan-mode magit highlight-symbol helm-projectile google-this flycheck dumb-jump)))
 '(safe-local-variable-values
   (quote
    ((flycheck-python-pylint-executable . "~/sites/str-prod/env/bin/pylint")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  ;; use local eslint from node_modules before global
  ;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))

  (defun my/configure-web-mode-flycheck-checkers ()
    ;; in order to have flycheck enabled in web-mode, add an entry to this
    ;; cond that matches the web-mode engine/content-type/etc and returns the
    ;; appropriate checker.
    (-when-let (checker (cond
                         ((string= web-mode-content-type "jsx")
                          'javascript-eslint)))
      (flycheck-mode)
      (flycheck-select-checker checker)))
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  (add-hook 'web-mode-hook #'my/configure-web-mode-flycheck-checkers))

(use-package s)

(use-package nyan-mode
  :config
  (nyan-mode))

(google-this-mode 1)

(sml/setup)

(use-package git-commit
	     :bind
	     ("C-c C-e" . m/suggest-commit-message-prefix))

(defun m/suggest-commit-message-prefix ()
  "Looks at recent commits for the currently staged files and
suggests some commit message prefixes."
  (interactive)
  (magit-with-toplevel
    (let* ((all-prefixes (mapcar (lambda (el) (car (s-match ".*: "
                                                            (substring el 1))))
                                 (magit-git-lines "log" "--no-merges" "--pretty=\"%s\"" "-100" "--"
                                                  (magit-git-lines "diff" "--cached" "--name-only"))))
           (uniq-prefixes (-uniq (-filter 'identity all-prefixes)))
           (counted-prefixes (mapcar (lambda (el) (cons el
                                                        (-count (lambda (el2) (string= el2 el))
                                                                all-prefixes)))
                                     uniq-prefixes))
           (sorted-choices (-sort (lambda (c1 c2) (> (cdr c1) (cdr c2)))
                                  counted-prefixes))
           (formatted-choices (mapcar (lambda (el) (format "%s (used %d time%s recently)"
                                                           (car el)
                                                           (cdr el)
                                                           (if (= (cdr el) 1)
                                                               ""
                                                             "s")))
                                      sorted-choices)))
      (when (> (length formatted-choices) 0)
        (insert (car (split-string (ido-completing-read "Commit message prefix: "
                                                          formatted-choices)
                                     " (used .* time.* recently)"))))
      formatted-choices)))

(use-package which-func
  :config
  (which-function-mode)
  (setq which-func-unknown "-")
  (setq mode-line-format (delete (assoc 'which-func-mode
                                        mode-line-format) mode-line-format)
        which-func-header-line-format '(which-func-mode ("" which-func-format)))
  (defadvice which-func-ff-hook (after header-line activate)
    (when which-func-mode
      (setq mode-line-format (delete (assoc 'which-func-mode
                                            mode-line-format) mode-line-format)
            header-line-format which-func-header-line-format)))
  (set-face-attribute 'which-func nil
                      :foreground "deep sky blue")
  (setq mode-line-misc-info
        ;; We remove Which Function Mode from the mode line, because it's mostly
        ;; invisible here anyway.
        (assq-delete-all 'which-func-mode mode-line-misc-info))
  (setq which-func-non-auto-modes '(gnus-group-mode
                                    gnus-summary-mode
                                    gnus-article-mode
                                    text-mode
                                    fundamental-mode
                                    help-mode
                                    git-commit-mode
                                    magit-mode)))

(use-package highlight-symbol
  :bind
  (("C-c M-s h ." . highlight-symbol-at-point)
   ("C-c M-s h n" . highlight-symbol-next)
   ("C-c M-s h p" . highlight-symbol-prev)
   ("C-c M-s h a" . highlight-symbol-remove-all)))

(use-package multiple-cursors
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C-c m C" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-c m >" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c m <" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)
   ("C-c m m <" . mc/mark-all-like-this))
  :init
  (use-package phi-search
    :init
    ;; credit to @jonebird for the following 
    ;; Allow isearch functionality with multipl-cursors
    (add-hook 'multiple-cursors-mode-enabled-hook
              (lambda ()
                (interactive)
                (global-set-key (kbd "C-s") 'phi-search)
                (global-set-key (kbd "C-r") 'phi-search-backward)))

    (add-hook 'multiple-cursors-mode-disabled-hook
              (lambda ()
                (interactive)
                (global-set-key (kbd "C-s") 'isearch-forward)
                (global-set-key (kbd "C-r") 'isearch-backward)))))

(add-hook 'after-init-hook 'global-company-mode)

(add-hook 'after-init-hook 'global-whitespace-cleanup-mode)

(defun m/shell-here ()
  "Create a new tmux window in the current directory and switch
to it."
  (interactive)
  (let ((tmux-cmd (format "tmux new-window -c \"%s\""
                          (expand-file-name default-directory)))
        (wmctrl-cmd (format "wmctrl -a 'zsh - \"%s@%s: '"
                            (user-login-name)
                            system-name)))
    (message "Trying %s" tmux-cmd)
    (shell-command tmux-cmd)
    (sit-for 1.5)
    (message "Trying %s" wmctrl-cmd)
    (shell-command wmctrl-cmd)))

(global-set-key (kbd "C-c m m x") 'm/shell-here)
