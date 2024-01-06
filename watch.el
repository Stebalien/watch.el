;;; watch.el --- Watch a command and display the output in a buffer -*- lexical-binding: t -*-

;; Copyright 2022 Steven Allen <steven@stebalien.com>

;; Author: Steven Allen <steven@stebalien.com>
;; URL: https://github.com/Stebalien/watch.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Repeatedly runs a command (like the Unix `watch' command), displaying the output in a buffer.
;; Unlike the Unix command, this tool supports:
;;
;; 1. Pausing, resuming, immediately refreshing.
;; 2. Temporarily pausing while the mark/region is active.
;; 3. Adjusting the interval without restarting.
;; 4. ANSI escape sequences.

;;; Code:

(defgroup watch nil
  "Watch customization group."
  :version "0.0.1"
  :group 'unix)

(defcustom watch-color-filter
  (cdr-safe (seq-find (lambda (pair) (require (car pair) nil 'noerror))
                      '((xterm-color . xterm-color-filter)
                        (ansi-color . ansi-color-filter-apply))))
  "Color filter for command output."
  :group 'watch
  :type '(choice
          (function :tag "Filter Function")
          (const :tag "None" nil)))

(defcustom watch-paused-indicator "<P>"
  "Status string to display when the watch buffer is paused."
  :group 'watch
  :type 'string)

(defcustom watch-running-indicator "<R>"
  "Status string to display when the watched command is actively running."
  :group 'watch
  :type 'string)

(defcustom watch-sleeping-indicator "<S>"
  "Status string to display when the watch buffer is sleeping between runs."
  :group 'watch
  :type 'string)

(defcustom watch-failed-indicator "<!>"
  "Status string to display when the watched command exists with a non-zero status."
  :group 'watch
  :type 'string)

(defvar-local watch-interval 2
  "Watch interval for the current buffer.")

(defvar-local watch-command nil
  "Watch command for the current buffer.")

(defvar-local watch--last-update "never"
  "Watch last update time.")

(defvar-local watch--timer nil
  "The timer running the watch command.")

(defvar-local watch--inhibited nil
  "Non-nil when the watch buffer is inhibited by a selection.")

(defvar-local watch--paused t
  "Non-nil when the watch buffer has been paused by the user.")

(defvar-local watch--failed nil
  "Non-nil if the last watch command failed.")

(defvar-local watch--pending-output nil
  "Pending command output.")

(defun watch--assert-mode ()
  "Asserts that the current buffer is a `watch-mode' buffer."
  (unless (eq major-mode 'watch-mode)
    (user-error "Command may only be run in a `watch' buffer")))

(defun watch--process-filter (proc text)
  "Process filter for the watched PROC, recording TEXT."
  (with-current-buffer (process-buffer proc)
    (when watch-color-filter
      (setq text (funcall watch-color-filter text)))
    (push text watch--pending-output)))

(defmacro watch--save-position (&rest body)
  "Save line, column, start; execute BODY; restore those things."
  `(let ((positions
          (mapcar
           (lambda (win)
             (with-selected-window win
               (list win
                     (current-column)
                     (count-lines (point-min) (window-start))
                     (count-lines (window-start) (line-beginning-position)))))
           (save-window-excursion ; otherwise, the cursor flashes
             (get-buffer-window-list nil nil t)))))
     (prog1 (progn ,@body)
       (pcase-dolist (`(,win ,col ,start ,line) positions)
         (with-selected-window win
           (goto-char (point-min))
           (forward-line start)
           (set-window-start nil (point))
           (forward-line line)
           (move-to-column col))))))

(defun watch--sentinel (proc _)
  "Process sentinel for the watched PROC."
  (unless (process-live-p proc)
    (let ((buf (process-buffer proc))
          (inhibit-read-only t))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (watch--save-position
           (erase-buffer)
           (apply #'insert (nreverse watch--pending-output)))
          (setq watch--pending-output nil
                watch--last-update (current-time-string)
                watch--failed (/= (process-exit-status proc) 0))
          (unless (or watch--paused watch--inhibited)
            (when watch--timer (cancel-timer watch--timer))
            (setq watch--timer (run-with-timer
                                watch-interval nil
                                #'watch--start-process
                                buf)))
          (watch--update-title))))))

(defun watch--stop ()
  "Stop the currently watched command and cancel any timers."
  ;; Cancel the timer, if any.
  (when watch--timer
    (cancel-timer watch--timer)
    (setq watch--timer nil))
  ;; Clear any progress so far.
  (setq watch--pending-output nil)
  ;; Kill the process
  (ignore-errors (delete-process))
  (setq watch--failed nil)
  ;; And update the title.
  (watch--update-title))

(defun watch-pause ()
  "Pause watching."
  (interactive nil watch-mode)
  (watch--assert-mode)
  (unless watch--paused
    (setq watch--paused t)
    (watch--stop)))

(defun watch-resume ()
  "Resume watching."
  (interactive nil watch-mode)
  (watch--assert-mode)
  (when watch--paused
    (setq watch--paused nil)
    (watch-refresh)))

(defun watch-toggle ()
  "Toggle watching."
  (interactive nil watch-mode)
  (watch--assert-mode)
  (if watch--paused (watch-resume) (watch-pause)))

(defun watch-refresh ()
  "Immediately refresh the watch buffer."
  (interactive nil watch-mode)
  (watch--assert-mode)
  (unless (and watch--inhibited (process-live-p (get-buffer-process (current-buffer))))
    (watch--start-process)))

(defun watch--inhibit ()
  "Inhibit updates to the watch buffer."
  (unless watch--inhibited
    (setq watch--inhibited t)
    (watch--stop)))

(defun watch--uninhibit ()
  "Uninhibit updates to the watch buffer."
  (when watch--inhibited
    (setq watch--inhibited nil)
    (unless watch--paused
      (watch-refresh))))

(defun watch--start-process (&optional buf)
  "Start the watched process in BUF (defaults to the current buffer)."
  (with-current-buffer (or buf (current-buffer))
    (when watch--timer
      (cancel-timer watch--timer)
      (setq watch--timer nil))
    (make-process
     :name (watch--command-name)
     :noquery t
     :buffer (current-buffer)
     :command (cond
               ((listp watch-command) watch-command)
               ((stringp watch-command) (list "bash" "-c" watch-command))
               (t (user-error "Unexpected command type")))
     :filter #'watch--process-filter
     :sentinel #'watch--sentinel)
    (watch--update-title)))

(defun watch--format-command-name (command)
  "Format COMMAND as a command name for display."
  (if (listp command) (combine-and-quote-strings command) command))

(defun watch--command-name ()
  "Return the current command name."
  (watch--format-command-name watch-command))

(defun watch--status-icon ()
  "Render the current status icon for the watched buffer."
  (cond
   ((or watch--inhibited watch--paused) watch-paused-indicator)
   ((get-buffer-process (current-buffer)) watch-running-indicator)
   (watch--failed watch-failed-indicator)
   (t watch-sleeping-indicator)))

(defun watch--update-title ()
  "Update the watch buffer title."
  (let* ((right watch--last-update)
         (left (concat
                (watch--status-icon)
                " Watch every "
                (format-seconds "%x%hhr %mmin %ssec%z" watch-interval)
                ": "
                (watch--command-name)))
         (right-width (string-pixel-width (propertize right 'face 'header-line)))
         (sep (propertize " " 'display `(space :align-to (- right (,right-width))))))
    (setq header-line-format (concat left sep right))))

(defun watch-quit ()
  "Quit the watch buffer."
  (interactive nil watch-mode)
  (quit-window 'kill))

(defun watch-set-interval (interval)
  "Change the watch interval to INTERVAL."
  (interactive (list (watch--read-interval)) watch-mode)
  (setq watch-interval interval)
  (if watch--paused
      (watch--update-title)
    (watch-refresh)))

(defvar-keymap watch-mode-map
  :doc "Keymap for watch-mode."
  :suppress t
  :parent special-mode-map
  "q" #'watch-quit
  "g" #'watch-refresh
  "i" #'watch-set-interval
  "p" #'watch-toggle)

(define-derived-mode watch-mode special-mode "Watch"
  "A major mode for watch buffers.
\\{watch-mode-map}"
  :interactive nil
  (add-hook 'activate-mark-hook 'watch--inhibit 0 'local)
  (add-hook 'deactivate-mark-hook 'watch--uninhibit 0 'local)
  (add-hook 'kill-buffer-hook #'watch--stop 0 'local))

(defun watch--read-interval ()
  "Read a watch interval from the user."
  (let* ((interval-s (read-string "Interval [default: 2 seconds]: "
                                  nil nil "2 seconds"))
         (interval (timer-duration interval-s)))
    (unless interval (user-error "Invalid interval %s" interval-s))
    interval))

;;;###autoload
(defun watch (command &optional interval)
  "Repeatedly run COMMAND at the specified INTERVAL."
  (interactive
   (list (read-shell-command "Watch: ")
         (watch--read-interval)))
  (let* ((buf (generate-new-buffer
               (concat "*watch: " (watch--format-command-name command)))))
    (with-current-buffer buf
      (watch-mode)
      (setq watch-interval (or interval 2)
            watch-command command)
      (watch-resume))
    (display-buffer buf)))
;;; watch.el ends here
