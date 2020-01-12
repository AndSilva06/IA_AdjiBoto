(defun bfs (no &optional (abertos (list no)) (fechados nil))
    (cond 
      ((no-solucaop no) no)
      ((null abertos) nil)
      (t (bfs (car abertos) (abertos-bfs (cdr abertos)
                      (apply #'append 
                          (mapcar  #'(lambda (suc) (if (or (null abertos) (existep suc fechados)) '() (list suc))) 
                            (gerar-sucessores (car abertos))  
                          )
                      ))  (cons  (car abertos) fechados)
          )
      )
    )
)

(defun dfs (no prof-max &optional (abertos (list no)) (fechados nil))
    (cond 
      ((= (no-prof no) prof-max) no)
      ((no-solucaop no) no)
      ((null abertos) nil)
      (t (dfs (car abertos) prof-max (abertos-dfs (cdr abertos)
                      (apply #'append 
                          (mapcar  #'(lambda (suc) (if (or (null abertos) (existep suc fechados)) '() (list suc))) 
                            (gerar-sucessores (car abertos))  
                          )
                      ))  (cons  (car abertos) fechados)
          )
      )
    )
)

(defun a-star (no &optional (abertos (list no)) (fechados nil))
  (cond 
      ((no-solucaop no) no)
      ((null abertos) nil)
      (t (a-star (car abertos) (abertos-a-star (cdr abertos) 
                        (apply #'append 
                            (mapcar  #'(lambda (suc) (if (or (null abertos) (existep suc fechados)) '() (list suc))) 
                              (gerar-sucessores (car abertos))  
                            )
                        )) (cons  (car abertos) fechados)
      ))
  )
)



(defun abertos-a-star (abertos sucessores)
"Devolver aqui a lista de abertos ordenada"
  (quicksort (append sucessores abertos))
)

(defun abertos-dfs (abertos sucessores) 
  (append sucessores abertos)
)

(defun abertos-bfs (abertos sucessores) 
  (append abertos sucessores)
)

(defun existep (no fechados) 
   (if (null fechados) nil
   (eval 
      (cons 
        'or (mapcar #'(lambda (no-fechado) (equal (no-tab no) (no-tab no-fechado))) fechados)
      )
   )) 
)


(defun numero-pecas-tabuleiro (tab) 
   (cond 
    ((null tab) nil)
    (t 
        (+ (apply #'+ (linha 0 tab)) (apply #'+ (linha 1 tab)))  )
   )
)

(defun heuristica (no)
  (cond 
  ((null no) 0)
    (t (- (numero-pecas-tabuleiro (no-tab no)) (cadr no)))
  )
)

(defun quicksort (no)
  (cond 
    ((null no) nil)
    (t 
      (let* ((x (car no))
        (r (cdr no))
        (fn (lambda (a) (< (no-heuristica a) (no-heuristica x)))))
        (append (quicksort (remove-if-not fn r))
          (list x)
          (quicksort (remove-if fn r)))
      )
    ) 
  )
)
