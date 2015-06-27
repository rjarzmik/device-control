(require 'device-control)
(require 'ido)

(defvar barebox-tftp-host-directory "/srv/tftp")
(defvar-local barebox-tftp-host nil)
(defvar-local barebox-net-hostname nil)
(defvar-local barebox-net-hostport nil)

(defun dctrl-barebox-net-async-run (&rest args)
   (dctrl-run-process
    (nconc (list "sh")
	   (list "-c"
		 (format "echo %s | socat STDIO UDP4-DATAGRAM:%s:%s"
			 (mapconcat 'identity args " ")
			 barebox-net-hostname barebox-net-hostport)))))

(defun dctrl-barebox-net-run (&rest args)
  (append
   (dctrl-barebox-net-waitprompt)
   (dctrl-run-process
    (nconc (list "sh")
	   (list "-c"
		 (format "echo %s | socat STDIO UDP4-DATAGRAM:%s:%s"
			 (mapconcat 'identity args " ")
			 barebox-net-hostname barebox-net-hostport))))))

(defun dctrl-barebox-net-action-command (&optional command)
  (let ((command (or command
		     (read-string "Barebox command: "))))
    (dctrl-barebox-net-run command)))

(defun dctrl-barebox-net-action-async-command (&optional command)
  (let ((command (or command
		     (read-string "Barebox command: "))))
    (dctrl-barebox-net-async-run command)))

(defun dctrl-barebox-net-action-upload-file (&optional filename)
  (let* ((src (or filename (ido-read-file-name "Source: "))))
    (append
     (dctrl-run-process
      (nconc (list "scp")
	     (list src (format "%s:%s/" barebox-tftp-host
			       barebox-tftp-host-directory))))
     (dctrl-barebox-net-action-command
      (format "cp /mnt/tftp/%s /" (file-name-nondirectory src))))))

(defun dctrl-barebox-net-process-sentinel (p e)
  (with-current-buffer (process-buffer p)
    (when (string-match-p "finished\\|exited" e)
      (if (= 0 (process-exit-status p))
	  (when (eq dctrl-state 'running)
	    (dctrl-continue))
	(progn
	  (setcdr dctrl-actions
		  (cons (car (dctrl-barebox-net-waitprompt)) (cdr dctrl-actions)))
	  (setcdr dctrl-actions
		  (cons (car (dctrl-action-wait 3)) (cdr dctrl-actions)))
	  (dctrl-continue))))))

(defun dctrl-barebox-net-action-waitprompt ()
  ; Any command will trigger a waitprompt, let's go for echo
  (dctrl-barebox-net-run "echo"))

(defun dctrl-barebox-net-get-actions ()
  (dctrl-build-fun-list "dctrl-barebox-net-action-"))

(defmacro dctrl-barebox-net-waitprompt ()
  `(lexical-let ((args nil))
     (list (lambda ()
	     (dctrl-msg "Waiting for barebox prompt")
	     (set-process-sentinel
	      (apply 'start-file-process "ctrl" (current-buffer) "sh"
		     (list "-c"
			   (format "echo | socat STDIO 'UDP4-RECV:%s!!UDP4-DATAGRAM:%s:%s' | grep -q barebox.*:" barebox-net-hostport barebox-net-hostname barebox-net-hostport)))
	      'dctrl-barebox-net-process-sentinel)
	     (set-process-filter (get-buffer-process (current-buffer))
				 'dctrl-process-filter)
	     'sleep))))

(defun dctrl-barebox-net-start (&optional delay)
  (when delay
    (setcdr dctrl-actions
	    (cons (car (dctrl-action-wait delay)) (cdr dctrl-actions)))))

(defun dctrl-barebox-net-create (&optional atftphost anetconhost anetconport)
  (let ((tftp-host (or atftphost (read-string "Tftp host ip: ")))
	(host (or anetconhost (read-string "Device netconsole hostname or ip: ")))
	(port (or anetconport (read-string "Device netconsole port: "))))
    (setq barebox-tftp-host tftp-host
	  barebox-net-hostname host
	  barebox-net-hostport port)
    (rename-buffer (format "*%s*" dctrl-device-name))))

(dctrl-register-backend
 (make-dctrl-backend :name "barebox-net"
		     :create 'dctrl-barebox-net-create
		     :start 'dctrl-barebox-net-start
		     :get-actions 'dctrl-barebox-net-get-actions))

(provide 'device-control-barebox-net)
