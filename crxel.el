(provide 'crxel)

(require 'websocket)
(require 'json)
(eval-when-compile (require 'cl))
;; (require 'deferred)

(defvar crxel/default-timeout 10.0)

(defvar crxel/websocket nil)
(defvar crxel/connection nil)
(defvar crxel/callback-table nil)
(defvar crxel/next-id 0)

(defun crxel/start (port)
  (interactive)
  (setq crxel/connection
        (websocket-server
         port
         :on-open 'crxel/on-open
         :on-message 'crxel/on-message
         :on-close 'crxel/on-close
         :on-error 'crxel/on-error)))

(defun crxel/stop ()
  (interactive)
  (let ((conn crxel/connection))
    (when crxel/websocket (websocket-close crxel/websocket))
    (setq crxel/callback-table nil
          crxel/connection nil
          crxel/websocket nil)
    (websocket-server-close conn)))

(defun crxel/on-open (ws)
  (when crxel/websocket (websocket-close crxel/websocket))
  (setq crxel/websocket ws))

(defun crxel/on-message (ws frame)
  (when (websocket-frame-completep frame)
    (let* ((data (json-read-from-string (websocket-frame-payload frame)))
           (id-string (cdr-safe (assq 'id data))))
      (when id-string
        (crxel/call-success (intern id-string) data)))))

(defun crxel/on-close (ws)
  (when (eq crxel/websocket ws)
    (setq crxel/websocket nil)))

(defun crxel/on-error (&rest args)
  )

(defun crxel/eval (code &rest plist)
  (lexical-let ((success (plist-get plist :success))
                (fail (plist-get plist :fail)))
    (plist-put plist :success
               (lambda (data)
                 (let ((err (cdr-safe (assq 'error data)))
                       (values (cdr-safe (assq 'values data)))
                       (values (cdr-safe (assq 'values data))))
                   (if err
                       (when fail (funcall fail err))
                     (when success (funcall success (elt values 0)))))))
    (apply 'crxel/request `(:code ,code :op "eval") plist)))

(defun crxel/request (data &rest plist)
  (interactive)
  (setq crxel/next-id (% (+ crxel/next-id 1) 32767))
  (lexical-let ((id (intern (int-to-string crxel/next-id)))
                (always (plist-get plist :always))
                timer-id)
    (plist-put plist :always (lambda (arg)
                               (ignore-errors (cancel-timer timer-id))
                               (when always (funcall always arg))))
    (crxel/register-callback id plist)
    (condition-case err
        (crxel/send-text
         (json-encode `(:id ,id ,@data)))
      (crxel/call-errorback id err))
    (setq timer-id
          (run-at-time
           (or (plist-get plist :timeout) crxel/default-timeout)
           nil (lambda () (crxel/call-fail id "timeout"))))
    timer-id))

(defun crxel/call-fail (id err)
  (crxel/call-callback id :fail err))

(defun crxel/call-success (id data)
  (crxel/call-callback id :success data))

(defun crxel/call-callback (id type arg)
  (let ((callbacks (cdr-safe (assq id crxel/callback-table))))
    (setq crxel/callback-table (assq-delete-all id crxel/callback-table))
    (when callbacks
      (let ((callback (plist-get callbacks type))
            (always (plist-get callbacks :always)))
        (unwind-protect
            (when callback
              (funcall callback arg))
          (when always
            (funcall always arg)))))))

(defun crxel/register-callback (id plist)
  (setq crxel/callback-table
        (acons id plist crxel/callback-table)))

(defun crxel/send-text (text)
  (let ((websocket-mask-frames nil))
    (if (websocket-openp crxel/websocket)
        (websocket-send-text crxel/websocket text)
      (error "no websocket"))))

;; (crxel/start 9649)
;; (crxel/eval "window.crxel.callback(1+1)" :success 'print :fail 'error)
;; (crxel/eval "var callback = window.crxel.callback;
;;              chrome.tabs.query({}, function(tabs) {
;;                  callback(JSON.stringify(tabs));
;;              });"
;;             :success (lambda (data)
;;                        (print (json-read-from-string data)))
;;             :fail 'error)
;; (crxel/stop)
