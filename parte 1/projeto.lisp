;;;; io.lisp
;;;; Fun��es para entrada e sa�de de dados - ecr� / ficheiro
;;;; Disciplina de IA - 2018 / 2019
;;;; Autor: Tiago Alves & Tiago Ribeiro

;;;;;;;;;; INITIALIZATION ;;;;;;;;;; 
(defun start-game() 
  (load-depedencies)
)

(defun get-curr-dir () 
  "Devolve o caminho absoluto at� a diretoria atual"
  (string "C:/Users/Tiago/Documents/ips/IA/projeto/parte 1")
)

(defun load-depedencies ()
  "Funcao que as depend�ncias do projeto" 
  (let ((path (get-curr-dir)))
    (progn
      (compile-file (concatenate 'string path "/puzzle.lisp"))
      ;(compile-file (concatenate 'string path "/procura.lisp"))
      (start-menu (read-problemas))
    )
  )
)


;;;;;;;;;; USER INTERACTION ;;;;;;;;;; 
(defun start-menu (problemas)
  "Fun��o que mostra o menu inicial do jogo"
  (format t "~% ~% ~%Bem vindo ao melhor jogo de sempre meu caro! ~%
             1 - Jogar
             2 - Sair ~%")
  (let ((answer (read)))
    (cond ((= answer 1) (select-problema problemas))
          ((= answer 2) (format t "Oh :("))
          (t (progn 
                (format t "~% >> Respota inv�lida, vamos tentar outra vez  << ~%")
                (start-menu problemas)
          ))
    )
  )
)

(defun select-problema (problemas)
  "Fun��o que constr�i o menu de problemas (com base na fun��o build-problemas-menu-options) 
   e permite o utilizador escohler um dos problemas"  
  (progn
    (build-problemas-menu-options problemas)

    (let ((answer (read))
        (maxAnswer (list-length problemas)))

      (cond ((OR (not (numberp answer)) (< answer 0) (> answer maxAnswer)) 
               (format t "~% >> Respota inv�lida, vamos tentar outra vez  << ~%")
               (select-problema problemas))
            (t (select-algo (nth answer problemas)))))
  )
)

(defun build-problemas-menu-options(problemas &optional (index 0))
  "Fun��o que constr�i as op��es do menu de problemas"
  (cond ((null problemas) (format t "~% ~%"))
        ((= index 0) 
           (progn
             (format t " > Escohla um problema")
             (build-problemas-menu-options problemas (1+ index))
           ))
        (t (progn 
             (format t "~% ~D - ~A" index (first problemas))
             (build-problemas-menu-options (rest problemas) (1+ index)))))
)

(defun select-algo (board)
  "Fun��o que permite o utilizador escolher um algoritmo de procura para aplicar
   no problema escolhido anteriormente"
  (progn 
    (build-algo-options board)

    (let ((answer (read)))

      (cond ((OR (not (numberp answer)) (< answer 1) (> answer 3)) 
               (format t "~% >> Respota inv�lida, vamos tentar outra vez  << ~%")
               (select-aglo board))
            (t (eval-algo board (get-algo-name answer)))))
  )
)

(defun build-algo-options(board)
  "Fun��o que constr�i o menu de escolha do algoritmo de procura"
  (format t " > Escolha um algoritmo para aplicar na resolu��o do problema: ~%   ~A ~% 1 - BFS ~% 2 - DFS ~% 3 - A* ~% ~%" board)
)

(defun get-algo-name (index)
  "Fun��o que retorna o nome do algoritmo com base no index"
  (cond ((= index 1) 'bfs)
        ((= index 2) 'dfs)
        (t 'a*))
)

(defun eval-algo (board algo)
  "Avalia o algortimo escolhido. Se o escolhido foi o DFS, ent�o pedimos ao utilizador
   a profundidade m�ximo do algoritmo e depois iniciamos o algoritmo, sen�o inicia-se logo"
  (cond ((equal algo 'dfs) (init-algo board 'dfs (get-dfs-depth)))
        (t (init-algo board algo)))
)

(defun get-dfs-depth() 
  "Fun��o pede a profundidade m�xima do dfs"
  (progn 
    (format t " > Introduza a profundidade m�ximo do algortmo ~%")

    (let ((answer (read)))

      (cond ((OR (not (numberp answer)) (< answer 1)) 
               (format t "~% >> Tem que ser n�mero positivo, vamos tentar outra vez << ~%")
               (get-dfs-depth))
            (t answer)))
  )
)


(defun init-algo (board algo &optional (depth 0))
  "Fun��o que inicia o algoritmo no problema escolhido"
  (format t "board: ~A ~%algo: ~s~%depth: ~D " board algo depth)
)

;;;;;;;;;; PROBLEMAS INPUT ;;;;;;;;;; 
(defun read-problemas ()
  "Abre o ficheiro problemas.dat existente na (get-curr-dir) e chama as fun��es 
   read-problemas-aux e build-boards para ler of ficheiro e construir os respetivos ficheiros.
   Lan�a error se n�o encontrar o ficheiro"
  (with-open-file (file (concatenate 'string (get-curr-dir) "/problemas.dat")
                   :direction :input
                   :if-does-not-exist :error)
      (build-boards (read-problemas-aux file)))
)

(defun read-problemas-aux(input &optional (output))
  "Percorre o ficheiro recebido (input) linha a linha recusrivamente e adiciona-as ao output.
   No final, retorna o output, que � uma lista cujos elementos s�o as v�rias linhas do ficheiro"   
  (let ((line (read-line input nil)))
    (cond ((not (null line))
              (read-problemas-aux input (append output (list line))))
          (t output)))
)

(defun build-boards(stringBoards &optional (boards))
  "Recebe a lista retornada em read-problemas-aux e percorre-a recursivamente, 
   criando listas a partir de cada elemento de {stringBoards}, que s�o strings,
   utilizando a fun��o build-board-aux. No final retorna uma lista de lsitas com os v�rios
   boards lidos do ficheiro."
  (cond ((null stringBoards) boards)
        (t (build-boards (rest stringBoards) (append boards (list (build-board-aux (first stringBoards)))))))
)

(defun build-board-aux (stringBoard)
  "Separar a string, que representa o tabuleiro recebida (stringBoard), em duas,
   sendo cada string uma linha do tabuleiro, e constr�i uma nova lista com essas duas 
   strings e retorna-a."
  (let ((board (split-sequence "," stringBoard)))
    (list (read-from-string (first board)) (read-from-string (second board))))
)