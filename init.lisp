;;; general utilites
(defun empty-string-p (str)
  (equal str ""))
(set-prefix-key (kbd "s-d"))

;;; Init stuff
(run-shell-command "xsetroot -cursor_name left_ptr")

(defun filter-batt-status (battery-info kind)
  (multiple-value-bind (_ info)
      (cl-ppcre:scan-to-strings " ([A-Za-z]+), ([0-9]+%)" battery-info)
    (ecase kind
      (status (aref info 0))
      (power (aref info 1)))))

(defcommand battery () ()
  "Show battery status as seen through acpi"
  (message (uiop:run-program "acpi" :output :string)))

(defun show-batt ()
  (filter-batt-status (uiop:run-program "acpi" :output :string)
		      'power))

(defcommand battery-status (kind)
  ((:string "status/power ==> "))
  "Show battery status by acpi, but filtered for the most relevant info"
  (message (filter-batt-status (uiop:run-program "acpi" :output :string)
			       (read-from-string kind))))



(defcommand default-set-up ()  ()
  (grename "Browse")
  (alacritty)
  (emacs)
  (gnew "Edit"))
;(setf *mouse-focus-policy* :click)

;(defcommand setup-env () ()
;  "Setup my default environment"
;  )


(defcommand firefox () ()
  (run-shell-command "firefox https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book.html"))
(defcommand my-emacs () ()
  (run-shell-command "emacs"))
(defcommand alacritty () ()
  (run-shell-command "alacritty"))

(defcommand nyxt () ()
  (run-shell-command "nyxt/nyxt"))

(defun define-multiple-keys (map pair-lst)
  (mapcar #'(lambda (kbd-pair)
	      (define-key map (kbd (car kbd-pair)) (cdr kbd-pair)))
	  pair-lst))

;;; i3w emulation
#|
(defparameter *split-variants* '(vsplit hsplit))
(defparameter *how-to-split* 0)

(defcommand start-in-new-frame (application)
    ((:string "Enter application to start"))
  (progn
    (funcall (nth *how-to-split* *split-variants*))
    (run-shell-command application)))
(defcommand split-dependent-on-settings () ()
  "Splits i3-like depending on settings vertically or horizontally"
  (funcall (elt *split-variants* *how-to-split*))
  (fnext)
  (run-shell-command "alacritty"))

(defcommand choose-vertical-split () ()
  "Enable vertical split for all further operations of split-dependent-on-settings"
  (setf *how-to-split* 0))
(defcommand choose-horizontal-split () ()
  "Enable vertical split for allefurther operations of split-dependent-on-settings"
  (setf *how-to-split* 1))

(defcommand kill-application-and-refocus () ()
  "Kill application in current frame and restore frame split"
  (delete-window)
  (remove-split))

(define-multiple-keys *top-map*
  '(("s-h" . "move-focus left")
    ("s-j" . "move-focus down")
    ("s-k" . "move-focus up")
    ("s-l" . "move-focus right")
    ("s-RET" . "split-dependent-on-settings")
    ("s-V" . "choose-vertical-split")
    ("s-H" . "choose-horizontal-split")
    ("s-Q" . "kill-application-and-refocus")))
|#


;; For now, I will try stumps approch
;;; But a few simplifications are in order

(define-multiple-keys *top-map*
    '(("s-TAB" . "fnext")
    ;;; This is supposed to mean Super-Shift-Tab
      ))


(defcommand confirm-poweroff (y-no) ((:string "Really want to poweroff"))
  (if (or (equal y-no "y")
	  (equal y-no "yes"))
      (run-shell-command "poweroff")))

(define-multiple-keys *root-map*
    '(("o" . "only")
      ("s-a" . "start-application")
      ("s-c" . "alacritty")
      ("s-e" . "emacs")
      ("s-f" . "firefox")
      ("s-n" . "nyxt")
      ("s-p" . "confirm-poweroff")  
      ("s-r" . "loadrc")
      ("s-s" . "exec systemctl suspend")))

(defcommand start-application () ()
  (select-window "emacs")
  (run-shell-command "emacsclient -ne '(call-interactively (quote cr/launcher))'"))

(defcommand do-start-ff () ()
  (run-shell-command "firefox d")
  (other-window)
  (title "firefox"))

(defcommand change-brightness (amount) ((:number "CB:"))
  (if amount
      (if (or (< amount 6)
	      (> amount 100))
	  (error (format nil "Brightness level invalid ==> ~D" amount))
	  (run-shell-command (format nil "xbacklight -set ~D" amount)))
      (run-shell-command "xbacklight -set 10")))


(setf *window-format* "%m%n%s%c")
;; Do NOT use any number above 7, since it will crash stumpwm
(setf *screen-mode-line-format* (list
				 "^01^B[^B%n^b] %W^>^01^B"
				 '(:eval (show-batt))
				 " %d"))
(setf *time-modeline-string* "%a %b %e %k:%M")
(setf *mode-line-timeout* 2)
(enable-mode-line (current-screen) (current-head) t)




