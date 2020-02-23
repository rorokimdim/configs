(if (not (buffer-has-name? (current-buffer)))
    (begin
      (init-style "article")
      (buffer-pretend-saved (current-buffer))))