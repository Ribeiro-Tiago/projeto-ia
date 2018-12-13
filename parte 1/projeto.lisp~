;;;; io.lisp
;;;; Funções para entrada e saíde de dados - ecrã / ficheiro
;;;; Disciplina de IA - 2018 / 2019
;;;; Autor: Tiago Alves & Tiago Ribeiro

;;;; funcoes que criam starter nodes para testar os algos ;;;;
(defun teste ()
  "Funcao que cria no inicial reference ao problema g para testar"
  (create-node '((8 8 8 8 8 8) (8 8 8 8 8 8)) 'calc-heuristica 0)
)

(defun teste2 ()
  "Funcao que cria no inicial dum tabuleiro aleatorio para testar"
  (create-node '((5 0 0 0 0 0) (0 0 0 0 0 5)) 'calc-heuristica 0)
)

(defun teste3 ()
  "Funcao que cria no inicial reference ao problema a para testar"
  (create-node '((0 0 0 0 0 2) (0 0 0 0 4 0)) 'calc-heuristica 0)
)

(defun teste4 ()
  "Funcao que cria no inicial reference ao problema b para testar"
  (create-node '((2 2 2 2 2 2) (2 2 2 2 2 2)) 'calc-heuristica 0)
)

(defun teste5 ()
  "Funcao que cria no inicial reference ao problema c para testar"
  (create-node '((0 3 0 3 0 3) (3 0 3 0 3 0)) 'calc-heuristica 0)
)

(defun teste6 ()
  "Funcao que cria no inicial reference ao problema f para testar"
  (create-node '((48 0 0 0 0 0) (0 0 0 0 0 48)) 'calc-heuristica 0)
)

(defun teste7 ()
  "Funcao que cria no inicial reference ao problema d para testar"
  (create-node '((1 2 3 4 5 6) (6 5 4 3 2 1)) 'calc-heuristica 0)
)

(defun teste8 ()
  "Funcao que cria no inicial reference ao problema e para testar"
  (create-node '((2 4 6 8 10 12) (12 10 8 6 4 2)) 'calc-heuristica 0)
)

(defun teste9 ()
  "Funcao que cria no inicial reference ao problema a para testar"
  (create-node '((2 2 2 2 2 2) (2 2 2 2 2 2)) 'calc-heuristica 0)
)

;;;;;;;;;; INITIALIZATION ;;;;;;;;;; 
(defun start-game() 
  "Funcao de nome bonito para começar o jogo. Carrega os ficheiros e chama o menu pricipal"
  (progn 
    (load-depedencies)
    (start-menu (read-problemas))
  )
)

(defun get-curr-dir ()
  "Funcao que obter um caminho que é usado para carregar os .lisp e .dat"
  (string "C:/Users/Tiago/Documents/ips/IA/projeto/parte 1")
)

(defun load-depedencies ()
  "Funcao que carrega as dependencias do projeto" 
  (let ((path (get-curr-dir)))
    (progn
      (compile-file (concatenate 'string path "/puzzle.lisp"))
      (compile-file (concatenate 'string path "/procura.lisp"))

      (load (concatenate 'string path "/puzzle.lisp"))
      (load (concatenate 'string path "/procura.lisp"))
    )
  )
)


;;;;;;;;;; USER INTERACTION ;;;;;;;;;; 
(defun start-menu (problemas)
  "Funcao que mostra o menu inicial do jogo e obtem respota do utilizador (para começar o sair)"
  (progn
    (format t "~% ~% ~%Bem vindo ao melhor jogo de sempre meu caro! ~%
             1 - Jogar
             2 - Sair ~%")
    (let ((answer (read)))
      (cond ((= answer 1) (select-problema problemas))
            ((= answer 2) (format t "Oh :( ~% ~%"))
            (t (progn 
                 (format t "~% >> Respota inválida, vamos tentar outra vez  << ~%")
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
               (format t "~% >> Respota inválida, vamos tentar outra vez  << ~%")
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
                 (format t "~% >> Respota inválida, vamos tentar outra vez  << ~%")
                 (select-algo board)
               ))
            (t (eval-algo board (get-algo-name answer)))))
  )
)

(defun build-algo-options(board)
  "Função que constrói o menu de escolha do algoritmo de procura"
  (format t " > Escolha um algoritmo para aplicar na resolução do problema: ~%   ~A ~% 1 - BFS ~% 2 - DFS ~% 3 - A* ~% ~%" board)
)

(defun get-dfs-depth() 
  "Funcao pede a profundidade maxima do dfs ao utilizador"
  (progn 
    (format t " > Introduza a profundidade máximo do algortmo ~%")

    (let ((answer (read)))

      (cond ((OR (not (numberp answer)) (< answer 1))
               (progn 
                 (format t "~% >> Tem que ser número positivo, vamos tentar outra vez << ~%")
                 (get-dfs-depth)
               ))
            (t answer)))
  )
)

;;;;;;;;;; ALGORYTHM ;;;;;;;;;; 

(defun init-algo (board algo &optional (heuristica 'heuristica-default) (depth 0))
  "Funcao que aplica o algoritmo escolhido no problema escolhido e depois cria um ficheiro com os resultados (com recurso a funcao \"write-results-to-file\") e mostra na consola (com recurso a funcao \"format-results\")"
  (let* ((start-time (get-universal-time))
         (results (funcall algo (create-node board heuristica 0)))
         (runtime (- (get-universal-time) start-time))
         (path (concatenate 'string (get-curr-dir) "/resultados.dat")))

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
  "Avalia o algortimo escolhido. Se o escolhido foi o DFS, então pedimos ao utilizador a profundidade maximo do algoritmo e depois iniciamos o algoritmo, senão inicia-se logo"
  (cond ((equal algo 'dfs) (init-algo board 'dfs (get-dfs-depth)))
        (t (init-algo board algo)))
)

;;;;;;;;;; FINAL OUTPUT ;;;;;;;;;; 

(defun write-results-to-file (results algo depth board heuristica runtime path)
  "Funcao que escreve os resultados num ficheiro \"resultados.dat\""
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
    ;; por questões de legibilidade humana, as linhas tão em formats diferentes
    ;; caracteristicas
    (format output "> Características: ~% - Algoritmo: ~s ~% - Heuristica: ~a ~% - Profundidade: ~s ~% - Problema: ~s ~% ~%"
             algo heuristica depth board)
    ;; resultados
    (format output "> Resultados: ~% - Nós gerados: ~d ~% - Nós expandidos: ~d ~% - Penetrância: ~d ~% - Fator de ramificação: ~d ~% - Tempo de execução: ~d ~%"
            (first results) (second results) (third results) (fourth results) runtime)
    (get-solucao (fifth results) output)
  )
)

(defun get-solucao (node output)
  "Funcao que faz o output da solucao. Usa \"get-caminho-solucao\" para mostrar o caminho solucao"
  (progn 
    (format output " - Solução: ")
    (get-caminho-solucao node output)
    (format output "~% ~% --------------------------------------------- ~% ~%")
  )
)

(defun get-caminho-solucao (node output)
  "Percorre o caminho solucao desde o no solucao ate ao no pai, fazendo output de cada no"
  (let ((parent (get-node-parent node)))
    (progn 
      (format output "~%    > ~a" (get-node-state node))
      (cond ((not (null parent)) (get-caminho-solucao parent output)))
    )
  )
)

;;;;;;;;;; PROBLEMAS INPUT ;;;;;;;;;; 
(defun read-problemas ()
  "Abre o ficheiro problemas.dat existente na (get-curr-dir) e chama as funções read-problemas-aux e build-boards  para ler of ficheiro e construir os respetivos ficheiros. Lança error se não encontrar o ficheiro"
  (with-open-file (file 
                   (concatenate 'string (get-curr-dir) "/problemas.dat")
                   :direction :input
                   :if-does-not-exist :error)
      (build-boards (read-problemas-aux file)))
)

(defun read-problemas-aux(input &optional (output))
  "Percorre o ficheiro recebido (input) linha a linha recusrivamente e adiciona-as ao output. No final, retorna o output, que é uma lista cujos elementos são as várias linhas do ficheiro"
  (let ((line (read-line input nil)))
    (cond ((not (null line))
              (read-problemas-aux input (append output (list line))))
          (t output)))
)

(defun build-boards(stringBoards &optional (boards))
  "Recebe a lista retornada em read-problemas-aux e percorre-a recursivamente, criando listas a partir de cada elemento de {stringBoards}, que são strings, utilizando a função build-board-aux. No final retorna uma lista de lsitas com os vários boards lidos do ficheiro."
  (cond ((null stringBoards) boards)
        (t (build-boards (rest stringBoards) (append boards (list (build-board-aux (first stringBoards)))))))
)

(defun build-board-aux (stringBoard)
  "Separar a string, que representa o tabuleiro recebida (stringBoard), em duas, sendo cada string uma linha do tabuleiro, e constrói uma nova lista com essas duas strings e retorna-a."
  (let ((board (split-sequence "," stringBoard)))
    (list (read-from-string (first board)) (read-from-string (second board))))
)