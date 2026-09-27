(let ((vocals "aeiouAEIOU")
      (consonants "bcdfghjklmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ"))
  (let ((vocalp (lambda (e) (seq-contains vocals e (lambda (c1 c2) (eq c1 c2)))))
	(consonantp (lambda (e) (seq-contains consonants e (lambda (c1 c2) (eq c1 c2))))))
    (let ((text (concat "La idea es seguir adelante, avanzar. "
			"Are you sure this is what you want to do? "
			"No veo alternativa. ")))
      (list :text (mapconcat 'char-to-string (seq-filter (lambda (c) (not (funcall vocalp c))) text))
	    :all (length text)
	    :non-chars (seq-reduce (lambda (c1 c2) (if (not (or (funcall vocalp c2) (funcall consonantp c2))) (+ c1 1) c1)) text 0)
	    :vocals (seq-reduce (lambda (c1 c2) (if (funcall vocalp c2) (+ c1 1) c1)) text 0)
	    :consonants (seq-reduce (lambda (c1 c2) (if (funcall consonantp c2) (+ c1 1) c1)) text 0)))))
