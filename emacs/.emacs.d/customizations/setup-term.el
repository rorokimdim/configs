;; Kill buffer upon term exit
(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  (kill-buffer))
