;; -*- lexical-binding: t; -*-

(defun ng-home (&optional arg)
  "Move back to indentation. If already there, move to beginning
of line. If a region is selected, create multiple cursors in the
selected lines before moving."
  (interactive "p")
  (setq arg (or arg 1))
  (if (/= arg 1)
      (move-beginning-of-line arg)
    (progn
      (if (and (use-region-p) (fboundp 'mc/edit-lines))
          (mc/edit-lines))
      (if (ng-at-indentation-p)
          (move-beginning-of-line 1)
        (back-to-indentation)))))

(defun ng-end (&optional arg)
  "Move back to indentation. If already there, move to beginning
of line. If a region is selected, create multiple cursors in the
selected lines before moving."
  (interactive "p")
  (setq arg (or arg 1))
  (if (/= arg 1)
      (move-end-of-line arg)
    (if (and (use-region-p) (fboundp 'mc/edit-lines))
        (mc/edit-lines))
    (move-end-of-line 1)))

(defun ng-at-indentation-p ()
  "Check if the cursor is already back to indentation."
  (interactive)
  (let ((old-column (current-column)))    
    (save-excursion
      (back-to-indentation)
      (= old-column (current-column)))))

(defun ng-rectangle-mark-down (&optional arg)
  (interactive "p")          
  (unless cua--rectangle (cua-set-rectangle-mark))
  (cua-resize-rectangle-down arg))
                             
(defun ng-rectangle-mark-up (&optional arg)
  (interactive "p")          
  (unless cua--rectangle (cua-set-rectangle-mark))
  (cua-resize-rectangle-up arg))

(defun ng-rectangle-mark-left (&optional arg)
  (interactive "p")
  (unless cua--rectangle (cua-set-rectangle-mark))
  (cua-resize-rectangle-left arg))

(defun ng-rectangle-mark-right (&optional arg)
  (interactive "p")
  (unless cua--rectangle (cua-set-rectangle-mark))
  (cua-resize-rectangle-right arg))

(defun ng-C-w (&optional arg)
  "If there is an active region, kill it. Otherwise, kill the
preceding word. Allows C-w to backward-kill-word like it does on
the terminal, but still kill-region when something is selected."
  (interactive "p")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))

(defun ng-C-k (&optional arg)
  "If there is an active region, kill it. Otherwise, kill the
following line. Adds symmetry to the mnemonic C-k=kill:C-y==yank."
  (interactive "P")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (kill-line arg)))

(defun ng-kill-this-buffer-and-window ()
  "Kill the current buffer and delete its window."
  (interactive)
  (let ((window (selected-window))
                (buffer (current-buffer)))
    (if (window-parent window)
        (delete-window window))
    (kill-buffer buffer)))

(defun ng-kill-other-buffer-and-window ()
  "Kill the buffer in the other window and delete its window."
  (interactive)
  (let ((old-window (selected-window))
        (old-buffer (current-buffer)))
    (other-window 1)
    (let ((new-window (selected-window))
          (new-buffer (current-buffer)))
      (unless (eq new-buffer old-buffer)
        (kill-buffer new-buffer))
      (unless (eq new-window old-window)
        (delete-window new-window)))))

(defun ng-done ()
  "Save the current buffer, kill it, and delete its
window. Notify client that we're done."
  (interactive)
  (save-buffer)
  (cond ((and (boundp 'with-editor-mode) with-editor-mode)
         (with-editor-finish nil))
        (server-buffer-clients
          (server-done))
        (t
         (ng-kill-this-buffer-and-window))))

(provide 'ng-lib)
