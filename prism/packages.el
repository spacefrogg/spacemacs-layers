(setq prism-packages
      '((prism-mode :location local)))

(defun prism/init-prism-mode ()
  (use-package prism-mode
    :commands prism-mode profeat-mode
    :mode ("prism\\'" . prism-mode)))
