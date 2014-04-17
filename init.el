;; init.el -- Starting Emacs configuration file
;; Intended for use with Emacs 24 only 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; APPEARANCE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remove scroll, tool and menu bars
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Theme
(load-theme 'tango-dark t)

;; Font
(set-default-font "Terminus-9")

;; Window size
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 85))

;; Highlight the corresponding parentheses
(show-paren-mode t)

;; Don't show the splash screen
(setq inhibit-startup-screen t)

;; Don't show dialog boxes
(setq use-dialog-box "n")

;; Show column number in mode line
(column-number-mode 1)

;; Word wrap
(global-visual-line-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Work in UTF-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Backup settings and version control
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.d/saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Save custom settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Indentation
(setq-default c-basic-offset 4)
(electric-indent-mode t)

;; Set margins to 80 characters
(setq-default fill-column 79)

;; Scrolling
(setq scroll-step 1)

;; Show keystrokes immediately
(setq echo-keystrokes 0.1)

;; Auto-refresh buffers
(global-auto-revert-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL KEYBINDINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; M-j -- join the following line onto this one
(global-set-key (kbd "M-j")
		(lambda ()
		  (interactive)
		  (join-line -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load repositories
(defvar gnu '("gnu" . "http://elpa.gnu.org/packages/"))
(defvar marmalade '("marmalade" . "http://marmalade-repo.org/packages/"))
(defvar melpa '("melpa" . "http://melpa.milkbox.net/packages/"))

(setq package-archives (list gnu marmalade melpa))

(package-initialize)

(unless (and (file-exists-p "~/.emacs.d/elpa/archives/marmalade")
             (file-exists-p "~/.emacs.d/elpa/archives/gnu")
             (file-exists-p "~/.emacs.d/elpa/archives/melpa"))
  (package-refresh-contents))

(defun packages-install (&rest packages)
  (mapc (lambda (package)
          (let ((name (car package))
                (repo (cdr package)))
            (when (not (package-installed-p name))
              (let ((package-archives (list repo)))
                (package-initialize)
                (package-install name)))))
        packages)
  (package-initialize)
  (delete-other-windows))

(defun get-packages ()
  (packages-install
   (cons 'auctex gnu)
   (cons 'auto-complete gnu)
   (cons 'magit melpa)
   (cons 'gist melpa)))

(condition-case nil
    (get-packages)
  (error
   (package-refresh-contents)
   (get-packages)))

(require 'cl)
(require 'compile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE-SPECIFIC SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; AUCTeX
(setq TeX-PDF-mode t)
(defun pdfokular ()
   (add-to-list 'TeX-output-view-style 
                    (quote ("^pdf$" "." "okular %o %(outpage)")))
)
(add-hook 'LaTeX-mode-hook 'pdfokular t)
(add-hook 'LaTeX-mode-hook 'outline-minor-mode t)
(add-hook 'LaTeX-mode-hook 'TeX-fold-mode t)
(add-hook 'LaTeX-mode-hook (lambda ()
  (push 
    '("XeLaTeX" "xelatex %s" TeX-run-TeX nil t
      :help "Run XeLaTeX on file")
    TeX-command-list)))
(add-hook 'TeX-parse-self t)

;; Auto-complete
(require 'auto-complete-config)
(ac-config-default)

;; Haskell
(custom-set-variables
 '(haskell-mode-hook '(turn-on-haskell-indentation)))

;; IDO
(require 'ido)
(ido-mode 1)
(global-set-key
     "\M-x"
     (lambda ()
       (interactive)
       (call-interactively
        (intern
         (ido-completing-read
          "M-x "
          (all-completions "" obarray 'commandp))))))

;; Linum
;(setq linum-format "%d")
;(global-linum-mode 1)

;; Magit
(global-set-key (kbd "C-c g") 'magit-status)

;; Mail
(setq auto-mode-alist (append '(("/tmp/mutt.*" . mail-mode)) auto-mode-alist))

;; Org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(add-hook 'org-mode-hook 'turn-on-font-lock)
(setq org-startup-indented t)
(setq org-enforce-todo-dependencies t)
(setq org-mobile-directory "~/Dropbox/mobileorg")
(setq org-directory "~/org")

;; Tramp
(require 'tramp)
(put 'upcase-region 'disabled nil)
