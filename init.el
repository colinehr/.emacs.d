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
(set-default-font "DejaVu Sans Mono-8")

;; Window size
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 100))

;; Highlight the corresponding parentheses
(show-paren-mode t)

;; Don't show the splash screen
(setq inhibit-startup-screen t)

;; Don't show dialog boxes
(setq use-dialog-box "n")

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

;; Scrolling
(setq scroll-step 1)

;; Show keystrokes immediately
(setq echo-keystrokes 0.1)

;; Auto-refresh buffers
(global-auto-revert-mode 1)

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
   (cons 'magit melpa)
   (cons 'gist melpa)))

(condition-case nil
    (get-packages)
  (error
   (package-refresh-contents)
    (get-packages)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE-SPECIFIC SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; AUCTeX
(setq TeX-PDF-mode t)
(defun pdfevince ()
   (add-to-list 'TeX-output-view-style 
                    (quote ("^pdf$" "." "evince %o %(outpage)")))
)
(add-hook 'LaTeX-mode-hook 'pdfevince t)

;; Eclim
(require 'cl)
(require 'eclim)
(require 'eclimd)
(setq eclim-executable "/opt/eclipse/eclim")
(setq eclim-auto-save t)
(global-eclim-mode)

;; Linum
(setq linum-format "%d")
(global-linum-mode 1)

;; Magit
(global-set-key (kbd "C-c g") 'magit-status)

;; Org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(add-hook 'org-mode-hook 'turn-on-font-lock)
