(spacemacs|use-package-add-hook pdf-tools
  :post-config
  (evilified-state-evilify pdf-view-mode pdf-view-mode-map
    "j" '(lambda() (interactive) (pdf-view-next-line-or-next-page 35))))

(provide 'init-pdf-tools)
