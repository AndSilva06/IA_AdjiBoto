;;;; Projeto Adji-boto
;;;; Disciplina de IA - 2018 / 2019
;;;; Autor: André Silva; Artur Ferreira

#|
---------------------------------------------------------- TABULEIROS ------------------------------------------------------------------
|# 
(defun tabuleiro-vazio (&optional (linhas 2) (colunas 6))
  "Retorna um tabuleiro 2x6 (default) com as casas vazias"
  (make-list linhas :initial-element (make-list colunas :initial-element '0))
)

(defun tabuleiro-teste ()
  "Retorna um tabuleiro de teste 2x6 que corresponde ao tabuleiro d) do enunciado do projeto"
  '((1 2 3 4 5 6)(6 5 4 3 2 1))
)

(defun tabuleiro()
 '((1 1 3 1 1 1)(2 1 1 1 1 1))
)
(defun tabuleiro-finalizado ()
  "Retorna um tabuleiro de teste 2x6 que corresponde ao tabuleiro d) do enunciado do projeto"
  '((2 0 2 0 0 0)(0 0 0 0 0 0))
)
(defun no-teste() 
  (cria-no (tabuleiro-finalizado))
)

#|
---------------------------------------------------------- SELETORES  ------------------------------------------------------------------
|# 
(defun linha-cima (tabuleiro)
  (car tabuleiro)
)

(defun linha-baixo (tabuleiro)
  (car (cdr tabuleiro))
)


(defun linha (indexLinha tabuleiro)
"recebe o indice da linha a devolver e o tabuleiro"
  (COND 
      ((NOT (listp tabuleiro)) nil)
      ((OR (< indexLinha 0)  (>= indexLinha (length tabuleiro))) nil)
      ((= indexLinha 0) (linha-cima tabuleiro))
      ((= indexLinha 1) (linha-baixo tabuleiro))
  )
)

(defun celula (indexLinha indexCelula tabuleiro)
"recebe o indice da linha o indice do atomo a devolver e o tabuleiro"
  (COND 
      ((NOT (listp tabuleiro)) nil)
      ((OR (< indexLinha 0)  (>= indexLinha (length tabuleiro))) nil)
      ((OR (< indexCelula 0)  (>= indexCelula (length (linha-cima tabuleiro)))) nil)
      ((= indexLinha 0)  (nth indexCelula (linha 0 tabuleiro)))
      ((= indexLinha 1)  (nth indexCelula (linha 1 tabuleiro)))
  )
)




#|
---------------------------------------------------------- FUNCOES AUX  ------------------------------------------------------------------
|# 

(defun tabuleiro-vaziop (tabuleiro)
"recebe um tabuleiro. Devolve T se todos os elementos estiverem a 0, caso contrario devolve nil."
  (cond 
    ((null tabuleiro) nil)
    ((AND 
      (= (apply #'+ (linha 0 tabuleiro))  0)
      (= (apply #'+ (linha 1 tabuleiro))  0)
    ))
  )
)

(defun substituir (linha celula tabuleiro &optional (peca 0) )
  (cond 
    ((eq linha 0) (list (substituir-posicao celula (car tabuleiro) peca) (cadr tabuleiro)) )
    ((eq linha 1) (list (car tabuleiro) (substituir-posicao celula (cadr tabuleiro) peca)) )
    (t tab)
  )
)

(defun substituir-aux (linha celula tabuleiro &optional (peca 0))
  (cria-no-aux (substituir linha celula (no-aux-tab tabuleiro) peca) (no-aux-pecas tabuleiro))
)


 (defun substituir-posicao (pos linha peca ) 
  (cond 
    ((null linha) linha)
    ((= pos 0) (cons peca (cdr linha)))
    (t (cons (car linha) (substituir-posicao (1- pos) (cdr linha) peca)))
  ) 
)

(defun substituir-celula (i j tab) 
  (cond 
    ((= i 0) (list (incrementar-posicao j (linha-cima tab)) (linha-baixo tab) ))
    ((= i 1) (list (linha-cima tab) (incrementar-posicao j (linha-baixo tab))))
    (t nil)
  )
)

(defun incrementar-posicao(index tab)
  (cond 
    ((null tab)nil)
    ((= index 0) (cons (1+ (car tab)) (cdr tab) ))
    (t (cons (car tab ) (incrementar-posicao (1- index) (cdr tab))))
  )
)



;; laboratorio



(defun expandir-no (no operadores)

)







#|
---------------------------------------------------------- OPERADORES  ------------------------------------------------------------------
|# 


(defun gerar-sucessores (no &aux (tab (no-tab no)))
  (mapcar #'(lambda (ntab) (if (null ntab) nil 
                                (cria-no (first ntab) 
                                          no 
                                          (1+ (no-prof no)) 
                                          (+ (no-pecas-retirads no) (second ntab))
                                          (heuristica (list (first ntab) (+ (no-pecas-retirads no) (second ntab))))
                                           )))
			(mapcar #' (lambda (pos) 
                        (substituir-aux (car pos) (cadr pos) (distribuir-pecas (car pos) (cadr pos) tab))) (celulas-com-pecas tab)
                  
      )	
  )
)

;; Para se jogar tem que ser P.e. : 
;; (substituir 0 2 (distribuir-pecas 0 2 (tabuleiro-teste)) 0)
(defun distribuir-pecas (i j tab 
                          &optional (n (1+ (celula i j tab))) 
                          (op #'1+) 
                          (size (length (car tab))) )
" nPecas - numero de pecas a distribuir, indexLinha - Linha onde distribuir as pecas, tabuleiro - por defeito é o tabuleiro vazio"
  (COND
    ((= n 0)  
          ;; Se o I for zero adicionamos um ao j, caso contrario retiramos 1 ao J
          ;; Para que a peça a ser validada seja a corecta
          (if (eq i 0) (valida-regra-aux i (1+ j) tab) (valida-regra-aux i (1- j) tab) )
    )
    ((= n 0) tab)
    ((AND (= i 1) (>= j size)) (distribuir-pecas 0 (1- size) tab n #'1-))
    ((AND (= i 0) (< j 0)) (distribuir-pecas 1 0 tab n #'1+))
    ((AND (= i 0) (eq op #'1+)) (distribuir-pecas i j tab n #'1-))
    (T (distribuir-pecas i (funcall op j) (substituir-celula  i j tab) (1- n)))
  )
)

(defun valida-regra-aux (i j tab)
  (cond 
    ((valida-regra (celula i j tab))  (cria-no-aux (substituir i  j tab 0) (celula i j tab)))
    (t (cria-no-aux tab 0))
  )
)

(defun valida-regra (x) 
  (cond 
    ((null x) nil)
    ((OR (= x 1) (= x 3) (= x 5)) T)
    (t nil)
  )
)

;; retorna lista com ex: ((linha index) (linha index)) 
;; ((0 0) (0 1) (0 2) (0 3) (0 4) (0 5) (1 0) (1 1) (1 2) (1 3) (1 4) (1 5))

(defun celulas-com-pecas (tab &optional (i 0))
  (COND
    ((null tab) nil)
    (T (append 
          (mapcar #'(lambda (j) (list i j)) 
            (coluna-com-pecas (car tab))) 
            (celulas-com-pecas (cdr tab) (1+ i)))
    )
  )
)


  (defun coluna-com-pecas (linha &optional (j 0))
    (COND
      ((NULL linha) nil)
      ((= (car linha) 0) (coluna-com-pecas (cdr linha) (1+ j)))
      (T (cons j (coluna-com-pecas (cdr linha) (1+ j))))
    )
  )



;; (1 2)
(defun jogar-teste(operador tab &optional (i (car operador)) (j (cadr operador)))
  (substituir i j (distribuir-pecas i j tab) 0)
)

#|
---------------------------------------------------------- NOS E CENAS :D  ------------------------------------------------------------------
|# 

(defun cria-no-aux (tab pecas &optional (heuristica 0))
  (list tab pecas heuristica)
)

(defun no-aux-tab (no)
  (first no)
)

(defun no-aux-pecas (no) 
  (second no)
)
(defun cria-no (tabuleiro &optional (no-pai nil) (profundidade 0) (pecas-capturados 0) (heuristica 0))
"cada no é representado por tabuleiro, no-pai e profundidade. Representa um estado do problema.
por defeito o no-pai é nil e a profundidade é 0"
  (list tabuleiro profundidade no-pai pecas-capturados heuristica)
)


(defun no-pai (no)
  (nth 2 no)
)

(defun no-prof (no)
  (nth 1 no)
)

(defun no-tab (no)
  (nth 0 no)
)

(defun no-pecas-retirads (no) 
  (nth 3 no)
)
(defun no-heuristica (no) 
  (nth 4 no)
)

(defun mostrar-caminho (no) 
  (cond 
    ((null no) (format t "Caminho percorrido"))
    (t
     (print (list (no-tab no) (no-prof no) (no-pecas-retirads no) (no-heuristica no)))
     (mostrar-caminho (no-pai no))
    )
  )
)
(defun no-solucaop (no)
"verifica se o tabuleiro está vazio"
  (tabuleiro-vaziop (no-tab no))
)



