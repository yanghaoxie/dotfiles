;;; packages.el --- my-config layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author:  <yhxie@westeros>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `my-config-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-config/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-config/pre-init-PACKAGE' and/or
;;   `my-config/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-config-packages
  '(helm-bibtex org-ref beacon golden-ratio-scroll-screen general posframe cdlatex)
  "The list of Lisp packages required by the my-config layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
e       `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

;; helm-bibtex
(defun my-config/init-helm-bibtex ()
  (use-package helm-bibtex
    :defer t
    :commands helm-bibtex
    :config
    (setq bibtex-completion-pdf-field "file")
    ;; (setq bibtex-completion-pdf-extenaion '(".pdf" ".html"))
    (setq bibtex-completion-find-additional-pdfs t)
    (setq bibtex-completion-bibliography "~/Dropbox/software/Zotero/bibtex/main.bib")
    (setq bibtex-completion-notes-path "~/Dropbox/document/org/references/ref_notes.org")
    (setq bibtex-completion-notes-template-one-file
          "\n* ${title} cite:${=key=}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n :INTERLEAVE_PDF: ${file}\n :END:\n\n")
    ))

;; pdf-tools
(spacemacs|use-package-add-hook pdf-tools
  :post-config
  ;; diable linum-mode in pdf-view-mode
  (add-hook 'pdf-view-mode-hook (lambda() (display-line-numbers-mode -1)))
  ;;turn off cua so copy works
  (add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0)))
  ;; more fine-grained zooming
  (setq pdf-view-resize-factor 1.1)
  (setq pdf-view-continuous 1)
  ;; enbale auto-revert-mode to auto revert pdf files
  (add-hook 'pdf-view-mode-hook 'auto-revert-mode)
  (setq auto-revert-interval 0.5)
  ;; key bindings
  (evilified-state-evilify pdf-view-mode pdf-view-mode-map
    "j" '(lambda() (interactive) (pdf-view-next-line-or-next-page 35))
    "J" '(lambda() (interactive) (pdf-view-next-line-or-next-page 10))
    "k" '(lambda() (interactive) (pdf-view-previous-line-or-previous-page 35))
    "K" '(lambda() (interactive) (pdf-view-previous-line-or-previous-page 10))
    "gc" 'eyebrowse-close-window-config
    "gr" 'eyebrowse-create-window-config
    "gl" 'eyebrowse-last-window-config
    "g0" 'eyebrowse-switch-to-window-config-0
    "g1" 'eyebrowse-switch-to-window-config-1
    "g2" 'eyebrowse-switch-to-window-config-2
    "g3" 'eyebrowse-switch-to-window-config-3
    "g4" 'eyebrowse-switch-to-window-config-4
    "g5" 'eyebrowse-switch-to-window-config-5
    "g6" 'eyebrowse-switch-to-window-config-6
    "g7" 'eyebrowse-switch-to-window-config-7
    "g8" 'eyebrowse-switch-to-window-config-8
    "g9" 'eyebrowse-switch-to-window-config-9
    )
  (add-hook 'pdf-view-mode-hook (lambda () (pdf-view-midnight-minor-mode t)))
  )

;; org-mode
(spacemacs|use-package-add-hook org
  :post-config
  (progn
    (add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))
    ;; (add-hook 'org-mode-hook 'turn-on-cdlatex)
    (require 'ox-bibtex)
    (setq org-directory "~/Dropbox/document/org")
    (setq org-latex-pdf-process
          '(
            "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "bibtex %b"
            "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            ))
    (setq org-log-done t)
    (with-eval-after-load 'ox-latex
      (add-to-list 'org-latex-classes
                   '("IEEEtran" "\\documentclass{IEEEtran}"
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
    ;; latex highlight in org mode
    (setq org-highlight-latex-and-related '(latex script entities))

    ;;setting up capture
    (setq org-default-notes-file (concat org-directory "/capture/capture.org"))
                                        ; Targets include this file and any file contributing to the agenda - up to 9 levels deep
    (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                     (org-agenda-files :maxlevel . 9))))
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "h" 'helm-org-in-buffer-headings)
    )
  )

;; org-ref
(defun my-config/init-org-ref ()
  (use-package org-ref
    :ensure t
    :defer 5
    ;; :mode (("\\.org\\'" . org-mode))
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "oa" 'my/org-ref-open-pdf-at-point))
    :config
    (progn
      (require 'org-ref)
      (setq org-ref-default-bibliography '("~/Dropbox/software/Zotero/bibtex/main.bib"))
      (setq org-ref-bibliography-notes "~/Dropbox/document/org/references/ref_notes.org")
      (defun my/org-ref-open-pdf-at-point ( )
        "Open the pdf for bibtex key under point if it exists."
        (interactive)
        (let* ((results (org-ref-get-bibtex-key-and-file))
               (key (car results))
               (pdf-file (car (bibtex-completion-find-pdf key))))
          (if (file-exists-p pdf-file)
              (org-open-file pdf-file)
            (message "No PDF found for %s" key))))
      ;; key binding
      )))

;; beacon
(defun my-config/init-beacon ()
  (use-package beacon
    :ensure t
    :config
    (setq beacon-blink-when-window-scrolls 0)
    (setq beacon-dont-blink-major-modes '(t pdf-view-mode))
    (beacon-mode 1)
    ))

;; golden-ratio-scroll-screen
(defun my-config/init-golden-ratio-scroll-screen ()
  (use-package golden-ratio-scroll-screen
    :ensure t
    :config
    (global-set-key [remap evil-scroll-page-up] 'golden-ratio-scroll-screen-down)
    (global-set-key [remap evil-scroll-page-down] 'golden-ratio-scroll-screen-up)
    ))

;; general
(defun my-config/init-general ()
  (use-package general
    :ensure t
    ))

;; helm
(spacemacs|use-package-add-hook helm
  :post-config
  (progn
    (defun ora-company-number ()
      "Forward to `company-complete-number'.

Unless the number is potentially part of the candidate.
In that case, insert the number."
      (interactive)
      (let* ((k (this-command-keys))
             (re (concat "^" company-prefix k)))
        (if (cl-find-if (lambda (s) (string-match re s))
                        company-candidates)
            (self-insert-command 1)
          (company-complete-number (string-to-number k)))))
    (let ((map company-active-map))
      (mapc
       (lambda (x)
         (define-key map (format "%d" x) 'ora-company-number
           ))
       (number-sequence 0 9))
      (define-key map " " (lambda ()
                            (interactive)
                            (company-abort)
                            (self-insert-command 1)))
      (define-key map (kbd "<return>") nil))
    (setq company-show-numbers t)
    ))

;; evil-snipe
(spacemacs|use-package-add-hook evil-snipe
  :post-config
  (progn
    (evil-snipe-override-mode t)
    (setq evil-snipe-scope 'line)
    ))

;; spaceline
(spacemacs|use-package-add-hook spaceline-config
  :post-config
  (setq
   powerline-height 18
   powerline-default-separator 'wave
   spaceline-minor-modes-p nil
   spaceline-buffer-encoding-abbrev-p nil
   spaceline-purpose-hide-if-not-dedicated t
   )
  (spaceline-compile)
  )

;; evil-escape
(spacemacs|use-package-add-hook evil-escape
  :post-config
  (setq evil-escape-key-sequence "jk")
  (setq evil-escape-unordered-key-sequence t)
  )

;; cdlatex
(defun my-config/init-cdlatex ()
  (use-package cdlatex
    :ensure t
    :config
    (setq cdlatex-simplify-sub-super-scripts nil)
    ))

;; tex
(spacemacs|use-package-add-hook tex
  :post-config
  (setq TeX-save-query nil)
  (setq TeX-view-program-selection
        (quote
         (((output-dvi has-no-display-manager)
           "dvi2tty")
          ((output-dvi style-pstricks)
           "dvips and gv")
          (output-dvi "xdvi")
          (output-pdf "PDF Tools")
          (output-html "xdg-open"))))
  )

;; helm
(spacemacs|use-package-add-hook helm
  :post-config
  (setq helm-display-function 'helm-default-display-buffer)
  )

;; shell
(setq
 shell-default-shell 'eshell
 shell-protect-eshell-prompt t
 )

;; which-key
;; (setq dotspacemacs-which-key-delay 0.8)

;; start server
(server-start)

;; display-time-mode
(setq
 display-time-24hr-format t
 display-time-default-load-average nil
 )
(display-time-mode 1)

;; purpose
(spacemacs|use-package-add-hook window-purpose
  :post-config
  (progn
    (setq
     purpose-user-mode-purposes
     '(
       (latex-mode . LaTeX)
       (pdf-view-mode . PDF)
       (emacs-lisp-mode . ELisp)
       )
     )
    (purpose-compile-user-configuration)
    ))

;; pyim
(defun my-config/init-posframe ()
  (use-package posframe
    :ensure t))
(spacemacs|use-package-add-hook pyim
  :pre-init
  (defvar pyim-title "Pyim") ;; modify pyim mode-line name
  :post-init
  (progn
    (pyim-basedict-enable)
    (setq-default pyim-english-input-switch-functions
                  '(
                    pyim-probe-dynamic-english
                    pyim-probe-isearch-mode
                    pyim-probe-program-mode
                    pyim-probe-org-structure-template
                    evil-normal-state-minor-mode ;; diable pyim in evil-normal-minor-mode
                    ))

    (setq-default pyim-punctuation-half-width-functions
                  '(pyim-probe-punctuation-line-beginning
                    pyim-probe-punctuation-after-punctuation))
    (setq pyim-page-tooltip 'posframe)
    )
  )
;;; packages.el ends here
