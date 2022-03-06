(defvar *odysseus* (make-keymap "my-map"))

(define-key *odysseus*
  "C-f" 'nyxt/web-mode:history-forwards
  "C-b" 'nyxt/web-mode:history-backwards
  "C-s a" 'nyxt/web-mode:search-buffers
  )

(define-bookmarklet-command change-yt-speed
  "Change playback speed of yt videos"
  "(function (){
const rate = prompt('Enter playback Rate');
if (rate!=null){
const video = document.getElementsByTagName('video')[0];
video.playbackRate = parseFloat(rate);
}})()")

(define-bookmarklet-command zapp-fcking-consent
  "Zapp away googles **** consent poput."
"(function (){
    let buttons = document.getElementsByTagName('ytd-button-renderer');
    let len = buttons.length;
    let zappIt = buttons[len-1];
    zappIt.click();
})();
")

(defun use-theme (theme)
  (funcall theme))
(defun make-define (pair)
  (let ((place (car pair))
	(css (cadr pair)))
    `(define-configuration ,place
	 ((style (str:concat
		  %slot-default%
		  (cl-css:css
		   ',css)))))))

(defmacro define-theme (name place-css-pairs)
  `(defvar ,name (lambda ()
		   ,@(mapcar #'(lambda (pair)
				(make-define pair))
			    place-css-pairs))))


(define-theme my-theme
    ((prompt-buffer  ((body
		       :background-color "black"
		       :color "#808080")
	      ("#prompt-area"
		       :background-color "black")
		      ;; The area you input text in.
		      ("#input"
		       :background-color "white")
		      (".source-name"
		       :color "black"
		       :background-color "gray")
		      (".source-content"
		       :background-color "black")
		      (".source-content th"
		       :border "1px solid lightgray"
		       :background-color "black")
		     ;; The currently highlighted option.
		      ("#selection"
		       :background-color "#37a8e4"
		       :color "white")
		      (.marked :background-color "darkgray"
			       :font-weight "bold"
			       :color "white")
		      (.selected :background-color "black"
				 :color "white")))

     ((panel-buffer internal-buffer) 
      ((body :background-color "black"
	     :color "lightgray")
	     (hr
	      :color "darkgray")
	     (a
	      :color "lightgray")
	     (.button
	      :color "lightgray"
	      :background-color "gray")))))

(use-theme my-theme)


;(defun eval-in-emacs (&rest s-exps)
;  "Evaluate S-EXPS with emacsclient."
;  (let ((s-exps-string (cl-strings:replace-all
;			(write-to-string
;			 `(progn ,@s-exps) :case :downcase)
;			;; Discard the package prefix.
;			"nyxt::" "")))
;    (format *error-output* "Sending to Emacs:~%~a~%" s-exps-string)
;    (uiop:run-program
;     (list "emacsclient" "--eval" s-exps-string)))))

(define-configuration password:keepassxc-interface
  ((password:password-file "/home/whysoserious/Passwords.kdbx")))

(define-configuration buffer
  ((password-interface (make-instance 'password:user-keepassxc-interface))))

(DEFINE-CONFIGURATION BROWSER
  ((EXTERNAL-EDITOR-PROGRAM "/usr/bin/emacsclient")))


(defvar *redirect-list*
  '(("www.twitter.com" . "www.nitter.net")))

(defun find-redirect (query-url)
  (assoc (quri:uri-host query-url) *redirect-list* :test #'equal))
(defun redirection (redirect-pair)
  (cdr redirect-pair))

(defun redirector (request)
  (let ((query-url (url request)))
	    (echo query-url)
    (setf (url request)
	  (let ((matching-redirect (find-redirect query-url)))
	    (log:info "Host is ~s" (quri:uri-host query-url))
	    (when matching-redirect
	      (log:info "Switching to ~s" (redirection matching-redirect))
	      (setf (quri:uri-host query-url) (redirection matching-redirect)))
	    query-url))
    request))




(define-command youtube-dl-current-page ()
  "Download yt video in currently open buffer"
  (let ((url (buffer-get-url)))
    (uiop:run-program (list "youtube-dl" url))))

(define-configuration web-buffer
    ((request-resource-hook
      (hooks:add-hook %slot-default% (make-handler-resource #'redirector)))))


(define-mode penellope ()
  ((keymap-scheme (keymap:make-scheme
		   scheme:cua *odysseus*
		   scheme:vi-normal *odysseus*
		   scheme:emacs *odysseus*))))

(define-configuration (buffer web-buffer)
  ((default-modes (append '(penellope emacs-mode) %slot-default%))))

		   
