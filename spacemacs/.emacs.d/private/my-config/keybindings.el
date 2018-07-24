(spacemacs/set-leader-keys
  "hb" 'helm-bibtex
  )

;; winum-mode
(general-define-key
 :states '(normal insert visual)
 :keymaps 'override
 "M-1" 'winum-select-window-1
 "M-2" 'winum-select-window-2
 "M-3" 'winum-select-window-3
 "M-4" 'winum-select-window-4
 "M-5" 'winum-select-window-5
 )

;; eyebrowse-mode
(general-define-key
 :keymaps 'override
 :states '(normal visual)
 ;; eyebrowse
 "gt" 'eyebrowse-next-window-config
 "gT" 'eyebrowse-prev-window-config
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

