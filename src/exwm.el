(use-package exwm
  :ensure t
  :config
  (require 'exwm)
  (require 'exwm-config)
  (require 'exwm-randr)

  (exwm-config-default)
  (exwm-randr-enable)
  (setq exwm-randr-workspace-output-plist '(1 "Virtual-1"))
  (add-hook 'exwm-randr-screen-change-hook
	    (lambda () (start-process-shell-command
			"xrandr"
			nil
			"xrandr --output Virtual-1 --mode 1920x1080"))))
