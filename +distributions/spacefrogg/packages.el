(setq spacefrogg-packages
      '((counsel-projectile :requires projectile)))



(defun spacefrogg/init-counsel-projectile ()
  (use-package counsel-projectile
    :defer t))
