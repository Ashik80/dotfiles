;;; pi-subagent-views-test.el --- Viewer placement check -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(load (expand-file-name "pi-subagent-views.el"
                        (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest my/pi-subagent-view-right-and-bottom ()
  (let* ((my/pi-subagent-views-root (make-temp-file "pi-view-test-" t))
         (file (expand-file-name "placement-test.log" my/pi-subagent-views-root))
         (buf (get-buffer-create "*pi-subagent:placement-test*"))
         (process (make-pipe-process :name "pi-view-test" :buffer buf :noquery t)))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (write-region "test transcript\n" nil file nil 'silent)
          ;; Supply the picker answer without requiring minibuffer input in batch.
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt choices &rest _) (car choices))))
            (call-interactively #'my/pi-subagent-views)
            (should (eq (window-parameter (selected-window) 'window-side) 'right))
            (should (eq (window-buffer) buf))
            (call-interactively #'my/pi-subagent-views-bottom)
            (should (eq (window-parameter (selected-window) 'window-side) 'bottom))
            (should (eq (window-buffer) buf))
            (should (eq (get-buffer-process buf) process))
            ;; Expand the current viewer without asking for another transcript.
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) (ert-fail "Unexpected transcript picker"))))
              (call-interactively #'my/pi-subagent-views-fullscreen))
            (should (= (length (window-list)) 1))
            (should-not (window-parameter (selected-window) 'window-side))
            (should (eq (window-buffer) buf))
            (should (eq (get-buffer-process buf) process))
            ;; Outside a viewer, fullscreen still offers the transcript picker.
            (switch-to-buffer (get-buffer-create " *pi-view-test-main*"))
            (call-interactively #'my/pi-subagent-views-fullscreen)
            (should (= (length (window-list)) 1))
            (should (eq (window-buffer) buf))
            ;; The original command must still choose the right side afterward.
            (call-interactively #'my/pi-subagent-views)
            (should (eq (window-parameter (selected-window) 'window-side) 'right))))
      (delete-process process)
      (kill-buffer buf)
      (when (get-buffer " *pi-view-test-main*")
        (kill-buffer " *pi-view-test-main*"))
      (delete-directory my/pi-subagent-views-root t))))

;;; pi-subagent-views-test.el ends here
