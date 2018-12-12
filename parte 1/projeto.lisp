;;;; io.lisp
;;;; Fun��es para entrada e sa�de de dados - ecr� / ficheiro
;;;; Disciplina de IA - 2018 / 2019
;;;; Autor: Tiago Alves & Tiago Ribeiro

;;;; funcoes que criam starter nodes para testar os algos ;;;;
(defun teste ()
  (create-node '((8 8 8 8 8 8) (8 8 8 8 8 8)) 'calc-heuristica 0)
)

(defun teste2 ()
  (create-node '((5 0 0 0 0 0) (0 0 0 0 0 5)) 'calc-heuristica 0)
)

(defun teste3 ()
  (create-node '((8 0 0 0 0 2) (0 0 0 0 4 0)) 'calc-heuristica 0)
)

(defun teste4 ()
  (create-node '((2 2 2 2 2 2) (2 2 2 2 2 2)) 'calc-heuristica 0)
)

(defun teste5 ()
  (create-node '((0 3 0 3 0 3) (3 0 3 0 3 0)) 'calc-heuristica 0)
)

(defun teste6 ()
  (create-node '((48 0 0 0 0 0) (0 0 0 0 0 48)) 'calc-heuristica 0)
)

(defun teste7 ()
  (create-node '((1 2 3 4 5 6) (6 5 4 3 2 1)) 'calc-heuristica 0)
)

(defun teste8 ()
  (create-node '((2 4 6 8 10 12) (12 10 8 6 4 2)) 'calc-heuristica 0)
)

(defun teste9 ()
  (create-node '((2 2 2 2 2 2) (2 2 2 2 2 2)) 'calc-heuristica 0)
)

;;;;;;;;;; INITIALIZATION ;;;;;;;;;; 
(defun start-game() 
  "Funcao de nome bonito para come�ar o jogo. Carrega os ficheiros e chama o menu pricipal"
  (progn 
    (load-depedencies)
    (start-menu (read-problemas))
  )
)

(defun get-curr-dir ()
  "Funcao que obter um caminho que � usado para carregar os .lisp e .dat"
  (string "C:/Users/Tiago/Documents/ips/IA/projeto/parte 1")
)

(defun load-depedencies ()
  "Funcao que carrega as dependencias do projeto" 
  (let ((path (get-curr-dir)))
    (progn
      (compile-file (concatenate 'string path "/puzzle.lisp"))
      (compile-file (concatenate 'string path "/procura.lisp"))
    )
  )
)


;;;;;;;;;; USER INTERACTION ;;;;;;;;;; 
(defun start-menu (problemas)
  "Funcao que mostra o menu inicial do jogo e obtem respota do utilizador (para come�ar o sair)"
  (progn
    (format t "~% ~% ~%Bem vindo ao melhor jogo de sempre meu caro! ~%
             1 - Jogar
             2 - Sair ~%")
    (let ((answer (read)))
      (cond ((= answer 1) (select-problema problemas))
            ((= answer 2) (format t "Oh :( ~% ~%"))
            (t (progn 
                 (format t "~% >> Respota inv�lida, vamos tentar outra vez  << ~%")
                 (start-menu problemas)
            ))
      )
    )
  )
)

(defun select-problema (problemas)
  "Funcao que constroi o menu de problemas (com base na funcao build-problemas-menu-options) e permite o utilizador escohler um dos problemas"
  (progn
    (build-problemas-menu-options problemas)

    (let ((answer (read))
        (maxAnswer (list-length problemas)))

      (cond ((OR (not (numberp answer)) (< answer 0) (> answer maxAnswer)) 
               (format t "~% >> Respota inv�lida, vamos tentar outra vez  << ~%")
               (select-problema problemas))
            (t (select-algo (nth (- answer 1) problemas)))))
  )
)

(defun build-problemas-menu-options(problemas &optional (index 0))
  "Funcao que constroi as opcoes do menu de problemas"
  (cond ((null problemas) (format t "~% ~%"))
        ((= index 0) 
           (progn
             (format t " > Escolha um problema")
             (build-problemas-menu-options problemas (1+ index))
           ))
        (t (progn 
             (print-board index (first problemas))
             (build-problemas-menu-options (rest problemas) (1+ index)))))
)

(defun select-algo (board)
  "Funcao que permite o utilizador escolher um algoritmo de procura para aplicar no problema escolhido anteriormente"
  (progn 
    (build-algo-options board)

    (let ((answer (read)))

      (cond ((OR (not (numberp answer)) (< answer 1) (> answer 3)) 
               (progn
                 (format t "~% >> Respota inv�lida, vamos tentar outra vez  << ~%")
                 (select-algo board)
               ))
            (t (eval-algo board (get-algo-name answer)))))
  )
)

(defun build-algo-options(board)
  "Fun��o que constr�i o menu de escolha do algoritmo de procura"
  (format t " > Escolha um algoritmo para aplicar na resolu��o do problema: ~%   ~A ~% 1 - BFS ~% 2 - DFS ~% 3 - A* ~% ~%" board)
)

(defun get-dfs-depth() 
  "Funcao pede a profundidade maxima do dfs ao utilizador"
  (progn 
    (format t " > Introduza a profundidade m�ximo do algortmo ~%")

    (let ((answer (read)))

      (cond ((OR (not (numberp answer)) (< answer 1))
               (progn 
                 (format t "~% >> Tem que ser n�mero positivo, vamos tentar outra vez << ~%")
                 (get-dfs-depth)
               ))
            (t answer)))
  )
)

;;;;;;;;;; ALGORYTHM ;;;;;;;;;; 

(defun init-algo (board algo &optional (heuristica 'calc-heuristica) (depth 0))
  "Funcao que aplica o algoritmo escolhido no problema escolhido e depois cria um ficheiro com os resultados (com recurso a funcao \"write-results-to-file\") e mostra na consola (com recurso a funcao \"format-results\")"
  (let* ((start-time (get-universal-time))
         (results (funcall algo (create-node board heuristica 0)))
         (runtime (- (get-universal-time) start-time))
         (path (concatenate 'string (get-curr-dir) "/estatisticas.dat")))

    (progn 
      (format t "             >>> Algoritmo finalizado <<< ~%~%")
      (format t "Os seguntes registos foram guardados em: ~s ~% ~%" path) 

      (format-results results 't algo depth board heuristica runtime)

      (write-results-to-file results algo depth board heuristica runtime path)
    )
  )
)

(defun get-algo-name (index)
  "Funcao que retorna o nome do algoritmo com base no index inserido pelo user"
  (cond ((= index 1) 'bfs)
        ((= index 2) 'dfs)
        (t 'a*))
)

(defun eval-algo (board algo)
  "Avalia o algortimo escolhido. Se o escolhido foi o DFS, ent�o pedimos ao utilizador a profundidade maximo do algoritmo e depois iniciamos o algoritmo, sen�o inicia-se logo"
  (cond ((equal algo 'dfs) (init-algo board 'dfs (get-dfs-depth)))
        (t (init-algo board algo)))
)

;;;;;;;;;; FINAL OUTPUT ;;;;;;;;;; 

(defun write-results-to-file (results algo depth board heuristica runtime path)
  "Funcao que escreve os resultados num ficheiro \"estatisticas.dat\""
  (with-open-file (output-file 
                   path
                   :direction :output
                   :if-exists :append
                   :if-does-not-exist :create)
    (format-results results output-file algo depth board heuristica runtime))
)

(defun format-results (results output algo depth board heuristica runtime)
  "Funcao que formata os resultados (results) do algoritmo para o {output} especificado (t > consola, filestream > ficheiro)"
  (progn 
    ;; por quest�es de legibilidade humana, as linhas t�o em formats diferentes
    ;; caracteristicas
    (format output "> Caracter�sticas: ~% - Algoritmo: ~s ~% - Heuristica: ~a ~% - Profundidade: ~s ~% - Problema: ~s ~% ~%"
             algo heuristica depth board)
    ;; resultados
    (format output "> Resultados: ~% - N�s gerados: ~d ~% - N�s expandidos: ~d ~% - Penetr�ncia: ~d ~% - Fator de ramifica��o: ~d ~% - Tempo de execu��o: ~d ~%"
            (first results) (second results) (third results) (fourth results) runtime)
    (get-caminho-solucao (fifth results) output)
  )
)

(defun get-caminho-solucao (node output)
  ""
  (progn 
    (format output " - Solu��o: ")
    (get-caminho-solucao-aux node output)
    (format output "~% ~% --------------------------------------------- ~% ~%")
  )
)

(defun get-caminho-solucao-aux (node output)
  (let ((parent (get-node-parent node)))
    (progn 
      (format output "~%    > ~a" (get-node-state node))
      (cond ((not (null parent)) (get-caminho-solucao-aux parent output)))
    )
  )
)

;;;;;;;;;; PROBLEMAS INPUT ;;;;;;;;;; 
(defun read-problemas ()
  "Abre o ficheiro problemas.dat existente na (get-curr-dir) e chama as fun��es 
   read-problemas-aux e build-boards  para ler of ficheiro e construir os respetivos ficheiros.
   Lan�a error se n�o encontrar o ficheiro"
  (with-open-file (file 
                   (concatenate 'string (get-curr-dir) "/problemas.dat")
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