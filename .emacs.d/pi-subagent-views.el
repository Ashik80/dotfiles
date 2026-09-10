;;; pi-subagent-views.el --- Live views of pi subagent transcripts  -*- lexical-binding: t; -*-

;; pi subagent live views: the pi subagent extension mirrors each
;; subagent's activity to transcript files under
;; ~/.cache/pi/subagent-views/session-<pid>/<agent>-<n>.log (plus a
;; registry.json per session).  Open them manually — nothing is
;; auto-opened.  Transcripts are deleted when the pi session ends.

;;; Code:

(defconst my/pi-subagent-views-root
  (expand-file-name "pi/subagent-views"
                    (or (getenv "XDG_CACHE_HOME") "~/.cache"))
  "Root directory of pi subagent view transcripts.")

(defun my/pi-subagent-transcripts ()
  "Return pi subagent transcript files, newest first."
  (when (file-directory-p my/pi-subagent-views-root)
    (sort (directory-files-recursively my/pi-subagent-views-root "\\.log\\'")
          (lambda (a b)
            (time-less-p
             (file-attribute-modification-time (file-attributes b))
             (file-attribute-modification-time (file-attributes a)))))))

(defun my/pi-subagent-status (file)
  "Return the registry status for subagent transcript FILE."
  (condition-case nil
      (let* ((registry-file (expand-file-name "registry.json"
                                              (file-name-directory file)))
             (key (file-name-base file))
             (registry (with-temp-buffer
                         (insert-file-contents registry-file)
                         (json-parse-buffer)))
             (entry (gethash key registry)))
        (or (and entry (gethash "status" entry)) "unknown"))
    (error "unknown")))

(defun my/pi-subagent-status-label (status)
  "Return a visible status indicator for STATUS."
  (pcase status
    ("running" "● RUNNING")
    ("done" "✓ DONE")
    ("error" "✗ ERROR")
    ("aborted" "■ ABORTED")
    (_ "? UNKNOWN")))

(defun my/pi-subagent-view-header ()
  "Render the current ghostel subagent view's status header."
  (let* ((file my/pi-subagent-transcript-file)
         (status (and file (my/pi-subagent-status file)))
         (face (pcase status
                 ("running" 'warning)
                 ("done" 'success)
                 ("error" 'error)
                 (_ 'shadow))))
    (concat " "
            (propertize (my/pi-subagent-status-label status) 'face face)
            "  "
            (and file (file-name-base file)))))

(defun my/pi-subagent-views ()
  "Choose a mirrored pi subagent and open its live transcript."
  (interactive)
  (let* ((files (my/pi-subagent-transcripts))
         (entries
          (mapcar
           (lambda (file)
             (cons (format "%s  %s"
                           (my/pi-subagent-status-label
                            (my/pi-subagent-status file))
                           (file-name-base file))
                   file))
           files))
         (labels (mapcar #'car entries))
         (choice (car labels)))
    (unless choice
      (user-error "No pi subagent transcripts under %s"
                  my/pi-subagent-views-root))
    (setq choice (completing-read "Subagent view: " labels nil t nil nil choice))
    (my/pi-open-subagent-view (cdr (assoc choice entries)))))

(defvar-local my/pi-subagent-transcript-file nil
  "Transcript file followed by the current ghostel subagent view.")

(defun my/pi-open-subagent-view (file)
  "Open FILE as a live tail in a right-side ghostel buffer.
The buffer is named \"*pi-subagent:<basename>*\".  Use
`ghostel-send-C-c' (normally C-c C-c) to stop tail; ghostel then
kills the buffer automatically."
  (interactive "fSubagent transcript: ")
  (let* ((name (format "*pi-subagent:%s*"
                       (file-name-sans-extension
                        (file-name-nondirectory file))))
         (buf (get-buffer name)))
    (unless (and buf (process-live-p (get-buffer-process buf)))
      (when buf (kill-buffer buf))
      (setq buf (get-buffer-create name))
      (ghostel-exec buf "tail" (list "-f" "-n" "+1" (expand-file-name file))))
    (with-current-buffer buf
      (setq-local my/pi-subagent-transcript-file (expand-file-name file))
      (setq-local header-line-format '(:eval (my/pi-subagent-view-header))))
    (pop-to-buffer
     buf
     '((display-buffer-in-side-window)
       (side . right)
       (window-width . 0.45)))
    (message "Opened %s — C-c C-c closes the view" name)))

(provide 'pi-subagent-views)
;;; pi-subagent-views.el ends here
