;;; simple-timer.el --- Simple timer

;; Author: Syohei Yoshida(syohex@gmail.com)
;; Version: 0.01

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup simple-timer nil
  "Simple timer"
  :prefix "simple-timer-"
  :group 'timer)

(defcustom simple-timer-mode-line-sign "●"
  "Sign of timer"
  :type 'string
  :group 'simple-timer)

(defface simple-timer-sign
  '((t (:foreground "blue")))
  "mode-line-face"
  :group 'simple-timer)

(defface simple-timer-timer
  '((t (:weight bold)))
  "mode-line-face"
  :group 'simple-timer)

(defvar simple-timer--timer nil)
(defvar simple-timer--remainder-seconds 0)
(defvar simple-timer--mode-line "")

(defmacro simple-timer--reset-remainder-time (time)
  `(setq simpler-timer--remainder-seconds (* ,time 60)))

(defsubst simple-timer--time-to-string (seconds)
  (format "%02d:%02d" (/ seconds 60) (mod seconds 60)))

(defun simple-timer--propertize-mode-line ()
  (unless (string= simple-timer--mode-line "")
    (concat (propertize simple-timer-mode-line-sign 'face 'simple-timer-sign)
            (propertize simple-timer--mode-line 'face 'simple-timer-timer))))

(defun simple-timer--set-mode-line ()
  (setq simple-timer--mode-line
        (simple-timer--time-to-string simple-timer--remainder-seconds)))

(defun simple-timer--tick ()
  (let ((remainder-seconds (1- simple-timer--remainder-seconds)))
    (if (< remainder-seconds 0)
        (simple-timer-stop)
      (decf simple-timer--remainder-seconds)
      (simple-timer--set-mode-line)
      (simple-timer--propertize-mode-line)
      (force-mode-line-update))))

(defsubst simple-timer--set-remainder-second (minutes)
  (setq simple-timer--remainder-seconds (* 60 minutes)))

;;;###autoload
(defun simple-timer-start (minutes)
  (interactive
   (list (read-number "How long minutes >> " 25)))
  (when simple-timer--timer
    (error "Already start timer!!"))
  (simple-timer--set-remainder-second minutes)
  (setq simple-timer--timer (run-with-timer 0 1 'simple-timer--tick)))

(defun simple-timer-stop (&optional do-reset)
  (interactive)
  (cancel-timer simple-timer--timer)
  (setq simple-timer--timer 'nil)
  (setq simple-timer--mode-line "")
  (force-mode-line-update))

;;;###autoload
(defun simple-timer-setup ()
  (interactive))

;;;###autoload
(defvar simple-timer--mode-line-initialized-p nil)

;;;###autoload
(unless simple-timer--mode-line-initialized-p
  (setq-default mode-line-format
                (cons '(:eval (simple-timer--propertize-mode-line))
                      mode-line-format))
  (setq simple-timer--mode-line-initialized-p t))

(provide 'simple-timer)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; simple-timer.el ends here
