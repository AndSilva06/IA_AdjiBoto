;;;; Projeto Adji-boto
;;;; Disciplina de IA - 2018 / 2019
;;;; 
;;;; Autor: André Silva, Artur Ferreira
;;;; Data: 04/11/2018


(defun puzzle ()
  "Permite iniciar o programa"
  (load (concatenate 'string (diretoria-atual) "problemas.dat"))
  (load (concatenate 'string (diretoria-atual) "procura.lisp"))
  (load (concatenate 'string (diretoria-atual) "puzzle.lisp"))
  (menu-principal)
  )

(defun diretoria-atual ()
  "Define o caminho para os ficheiros do projeto a partir da raiz"
  (let ((path "C:\\Users\\ferreiraaj\\Desktop\\IA 2018\\PROJETO\\Adji-Boto\\"))
    path
  )
)


;;; MENU PRINCIPAL
(defun menu-principal ()
  "Apresenta o menu principal com as opcões do programa"
  (loop
    (progn
      (format t "~% ------------------------------------------------------")
      (format t "~%|                   ADJI BOTO (Oware)                  |")
      (format t "~%|                                                      |")
      (format t "~%|            1-Resolver um tabuleiro                   |")
      (format t "~%|            2-Regras do jogo                          |")
      (format t "~%|            3-Mostrar um tabuleiro                    |")
      (format t "~%|            4-Sair                                    |")
      (format t "~%|                                                      |")
      (format t "~% ------------------------------------------------------")
      (format t "~%~%Escolha:")
      )
    (cond ((not (let ((escolha (read)))
               (cond 
                ((and (< escolha 5) (> escolha 0)) (case escolha
                                                    (1 (progn (menu-jogar) t))
                                                    (2 (progn (regras) t))
                                                    (3 (progn (imprime-tabuleiro) t))
                                                    (4 (progn (format t "PROGRAMA TERMINADO") nil)))
                )
                ( T (progn  (format t "~%ESCOLHA INVALIDA~%~%Escolha: ")
                            (setf escolha (read))
                            )))
               
               )) (return)))
    )
  )


(defun menu-jogar()
"Sub menu escolhe algoritmo"
 (loop
    (progn
      (format t "~% ------------------------------------------------------")
      (format t "~%|                 LISTA ALGORITMOS                     |")
      (format t "~%|                                                      |")
      (format t "~%|            1-Algorismo BFS                           |")
      (format t "~%|            2-Algoritmo DFS                           |")
      (format t "~%|            3-Algoritmo A*                            |")
      (format t "~%|            4-Algoritmo IDA*                          |")
      (format t "~%|            5-Menu inicial                            |")
      (format t "~%|                                                      |")
      (format t "~% ------------------------------------------------------")
      (format t "~%~%Escolha:")
      )
    (cond ((not (let ((escolha (read)))
               (cond 
                ((and (< escolha 6) (> escolha 0)) (case escolha
                                                    (1 (format t "a1") t)
                                                    (2 (format t "a2") t)
                                                    (3 (format t "a3") t)
                                                    (4 (format t "a4") t)
                                                    (5 (progn nil)))
                )
                ( T (progn  (format t "~%ESCOLHA INVALIDA~%~%Escolha: ")
                            (setf escolha (read))
                            )))
               
               )) (return)))
    )
  )


;;;Regras do puzzle
(defun regras () 
    (format t "
   -------------------------- Regras do jogo ADJI-BOTO (Oware) --------------------------
  |                                                                                      |
  |                                                                                      |
  |                                                                                      |
  |                                                                                      |
  |                                                                                      |
  |                                               flgdfjg                                |
  |                                                                                      |
  |                                                                                      |
  ----------------------------------------------------------------------------------------
  "
  )
)

(defun imprime-tabuleiro ()
"Sub-menu imprime problemas"
(loop
    (progn
      (format t "~% ------------------------------------------------------")
      (format t "~%|                   LISTA TABULEIROS                   |")
      (format t "~%|                                                      |")
      (format t "~%|            1-Problema A                              |")
      (format t "~%|            2-Problema B                              |")
      (format t "~%|            3-Problema C                              |")
      (format t "~%|            4-Problema D                              |")
      (format t "~%|            5-Problema E                              |")
      (format t "~%|            6-Problema F                              |")
      (format t "~%|            7-Problema G                              |")
      (format t "~%|            8-Menu Inicial                            |")
      (format t "~%|                                                      |")
      (format t "~% ------------------------------------------------------")
      (format t "~%~%Escolha:")
      )
    (cond ((not (let ((escolha (read)))
               (cond 
                ((and (< escolha 9) (> escolha 0)) (case escolha
                                                    (1 (print (problema-a)))
                                                    (2 (print (problema-b)))
                                                    (3 (print (problema-c)))
                                                    (4 (print (problema-d)))
                                                    (5 (print (problema-e)))
                                                    (6 (print (problema-f)))
                                                    (7 (print (problema-g)))
                                                    (8 (progn nil)))
                )
                ( T (progn  (format t "~%ESCOLHA INVALIDA~%~%Escolha: ")
                            (setf escolha (read))
                            )))
               
               )) (return)))
    )
  )






;;Escrever em ficheiros TODO