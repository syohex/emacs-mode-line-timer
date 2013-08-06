;;; mode-line-timer.el --- Timer in mode-line

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

(defgroup mode-line-timer nil
  "Simple timer"
  :prefix "mode-line-timer-"
  :group 'timer)

(defcustom mode-line-timer-mode-line-sign "●"
  "Sign of timer"
  :type 'string
  :group 'mode-line-timer)

(defface mode-line-timer-sign
  '((t (:foreground "blue")))
  "mode-line-face"
  :group 'mode-line-timer)

(defface mode-line-timer-timer
  '((t (:weight bold)))
  "mode-line-face"
  :group 'mode-line-timer)

(defvar mode-line-timer--timer nil)
(defvar mode-line-timer--remainder-seconds 0)
(defvar mode-line-timer--mode-line "")

(defmacro mode-line-timer--reset-remainder-time (time)
  `(setq simpler-timer--remainder-seconds (* ,time 60)))

(defsubst mode-line-timer--time-to-string (seconds)
  (format "%02d:%02d" (/ seconds 60) (mod seconds 60)))

(defun mode-line-timer--propertize-mode-line ()
  (unless (string= mode-line-timer--mode-line "")
    (concat (propertize mode-line-timer-mode-line-sign 'face 'mode-line-timer-sign)
            (propertize mode-line-timer--mode-line 'face 'mode-line-timer-timer))))

(defun mode-line-timer--set-mode-line ()
  (setq mode-line-timer--mode-line
        (mode-line-timer--time-to-string mode-line-timer--remainder-seconds)))

(defun mode-line-timer--tick ()
  (let ((remainder-seconds (1- mode-line-timer--remainder-seconds)))
    (if (< remainder-seconds 0)
        (mode-line-timer-stop)
      (decf mode-line-timer--remainder-seconds)
      (mode-line-timer--set-mode-line)
      (mode-line-timer--propertize-mode-line)
      (force-mode-line-update))))

(defsubst mode-line-timer--set-remainder-second (minutes)
  (setq mode-line-timer--remainder-seconds (* 60 minutes)))

;;;###autoload
(defun mode-line-timer-start (minutes)
  (interactive
   (list (read-number "How long minutes >> " 25)))
  (when mode-line-timer--timer
    (error "Already start timer!!"))
  (mode-line-timer--set-remainder-second minutes)
  (setq mode-line-timer--timer (run-with-timer 0 1 'mode-line-timer--tick)))

(defun mode-line-timer-stop (&optional do-reset)
  (interactive)
  (cancel-timer mode-line-timer--timer)
  (setq mode-line-timer--timer 'nil)
  (setq mode-line-timer--mode-line "")
  (force-mode-line-update))

;;;###autoload
(defun mode-line-timer-setup ()
  (interactive))

;;;###autoload
(defvar mode-line-timer--mode-line-initialized-p nil)

;;;###autoload
(unless mode-line-timer--mode-line-initialized-p
  (setq-default mode-line-format
                (cons '(:eval (mode-line-timer--propertize-mode-line))
                      mode-line-format))
  (setq mode-line-timer--mode-line-initialized-p t))

(provide 'mode-line-timer)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mode-line-timer.el ends here
