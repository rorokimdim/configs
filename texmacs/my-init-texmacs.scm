(kbd-map
  (:mode in-text?)
  ("D e f ." (make 'definition))
  ("L e m ." (make 'lemma))
  ("P r o p ." (make 'proposition))
  ("T h ." (make 'theorem))
  ("C r ." (make 'corollary))
  ("P f ." (make 'proof))
  ;; sessions
  ("M a x i m a ." (make-session "maxima" "default"))
  ("S a g e ." (make-session "sage" "default"))
  ("S c m ." (make-session "scheme" "default"))
  ("P y ." (make-session "python" "default"))
  )
