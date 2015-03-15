(require 'ido)
(require 'device-control)
(require 'device-control-barebox-net)
(defvar-local openocd-net-hostname nil)
(defvar-local openocd-net-hostport nil)
(defvar-local pxa-last-dtb "")

(defun pxa-barebox-command (command)
  (dctrl-barebox-net-action-command command))
(defun pxa-barebox-async-command (command)
  (dctrl-barebox-net-action-async-command command))

(defun pxa-barebox-upload-file (file)
  (dctrl-barebox-net-action-upload-file file))

(defun dctrl-pxa-action-upload-launch-kernel ()
  (interactive)
  (let ((device-name (dctrl-complete-device nil "pxa")))
    (with-current-buffer (dctrl-get-buffer device-name)
      (append
       (pxa-barebox-upload-file (concat pxa-kpath "/arch/arm/boot/zImage"))
       (pxa-barebox-command (pxa-setup-bootargs pxa-extra-bootargs))
       (pxa-barebox-async-command "bootm zImage")))))

(defun dctrl-pxa-action-upload-launch-kernel-dt ()
  (interactive)
  (let ((device-name (dctrl-complete-device nil "pxa"))
	(dt-filename (ido-read-file-name "Device-tree dtb: "
					 (concat pxa-kpath "/arch/arm/boot/dts")
					 nil t pxa-last-dtb)))
    (setq pxa-last-dtb (file-name-nondirectory dt-filename))
    (with-current-buffer (dctrl-get-buffer device-name)
      (append
       (pxa-barebox-upload-file (concat pxa-kpath "/arch/arm/boot/zImage"))
       (pxa-barebox-upload-file dt-filename)
       (pxa-barebox-command (pxa-setup-bootargs pxa-extra-bootargs))
       (pxa-barebox-async-command
	(format "bootm -o %s zImage" (file-name-nondirectory dt-filename)))))))

(defun dctrl-pxa-action-command ()
  (dctrl-barebox-net-action-command))

(defun dctrl-pxa-get-actions ()
  (dctrl-agregate-fun-list (dctrl-build-fun-list "dctrl-pxa-action-")))

(defun dctrl-pxa-action-wait ()
  (dctrl-barebox-net-waitprompt))

(defun dctrl-pxa-action-reset ()
  (dctrl-run-process
   (nconc (list "sh")
	  (list "-c"
		(format "echo hard_reset_%s | socat STDIO TCP4:%s:%s"
			dctrl-device-name
			openocd-net-hostname openocd-net-hostport
			)))))

(defun dctrl-pxa-create ()
  (let ((o-host (read-string "Openocd hostname or ip: "))
	(o-port (read-string "Openocd port: ")))
    (dctrl-barebox-net-create)
    (setq openocd-net-hostname o-host
	  openocd-net-hostport o-port)))

(dctrl-register-backend
 (make-dctrl-backend :name "pxa"
		     :create 'dctrl-pxa-create
		     :get-actions 'dctrl-pxa-get-actions))

(provide 'device-control-pxa)
