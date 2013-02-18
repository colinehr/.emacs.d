;; setup-org.el -- Configure org-mode

(setq org-directory "~/planner/")
(setq default-notes-file (concat org-directory "notes.org"))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(define-key global-map "\C-cc" 'org-capture)
(add-hook 'org-mode-hook 'turn-on-font-lock)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (concat org-directory "main.org") "Tasks")
	 "* TODO %?\n  %i\n  %a")))

(setq org-agenda-files '((concat org-directory "main.org")))

(provide 'setup-org)
