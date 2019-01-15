;;;; projeto.lisp
;;;; Funcoeses de interaco com o utilizador, entrada e saida de dados e misc
;;;; Disciplina de IA - 2018 / 2019
;;;; Autor: Tiago Alves & Tiago Ribeiro

;;;; funcoes que criam starter nodes para testar os algos ;;;;
(defun teste ()
  "Funcao que cria no inicial reference ao problema g para testar"
  (create-node '((8 8 8 8 8 8) (8 8 8 8 8 8)) 'heuristica-default 0)
)

(defun teste2 ()
  "Funcao que cria no inicial dum tabuleiro aleatorio para testar"
  (create-node '((5 0 0 0 0 0) (0 0 0 0 0 5)) 'heuristica-default 0)
)

(defun teste3 ()
  "Funcao que cria no inicial reference ao problema a para testar"
  (create-node '((0 0 0 0 0 2) (0 0 0 0 4 0)) 'heuristica-extra 0)
) 

(defun teste4 ()
  "Funcao que cria no inicial reference ao problema b para testar"
  (create-node '((2 2 2 2 2 2) (2 2 2 2 2 2)) 'heuristica-default 0)
)

(defun teste5 ()
  "Funcao que cria no inicial reference ao problema c para testar"
  (create-node '((0 3 0 3 0 3) (3 0 3 0 3 0)) 'heuristica-default 0)
)

(defun teste6 ()
  "Funcao que cria no inicial reference ao problema f para testar"
  (create-node '((48 0 0 0 0 0) (0 0 0 0 0 48)) 'heuristica-default 0)
)

(defun teste7 ()
  "Funcao que cria no inicial reference ao problema d para testar"
  (create-node '((1 2 3 4 5 6) (6 5 4 3 2 1)) 'heuristica-default 0)
)

(defun teste8 ()
  "Funcao que cria no inicial reference ao problema e para testar"
  (create-node '((2 4 6 8 10 12) (12 10 8 6 4 2)) 'heuristica-default 0)
)

(defun teste9 ()
  "Funcao que cria no inicial reference ao problema a para testar"
  (create-node '((2 2 2 2 2 2) (2 2 2 2 2 2)) 'heuristica-default 0)
)

(defun no-teste ()
  "Define um no teste do problema da vasilhas em que A=2, B=2, profundidade=0 e pai=NIL"
  (create-node '((5 0 0 0 0 0) (0 0 0 0 0 5)) nil 0 0)
)

;;;;;;;;;; INITIALIZATION ;;;;;;;;;; 
(defun start-game() 
  "Funcao de nome bonito para comecar o jogo. Carrega os ficheiros e chama o menu pricipal"
  (progn 
    (load-depedencies)
    (start-menu)
  )
)

(defun get-curr-dir ()
  "Funcao que obter um caminho que e usado para carregar os .lisp e .dat"
  ; (string "/Users/tiago/Documents/projetoIA/projeto-ia/parte 2"); - Alves mac
  (string "C:/Users/Tiago/Documents/ips/IA/projeto/parte 1"); - Ribeiro windows
)

(defun load-depedencies ()
  "Funcao que carrega as dependencias do projeto" 
  (let ((path (get-curr-dir)))
    (progn
      (compile-file (concatenate 'string path "/jogo.lisp"))
      (compile-file (concatenate 'string path "/algoritmo.lisp"))

      (load (concatenate 'string path "/jogo.lisp"))
      (load (concatenate 'string path "/algoritmo.lisp"))
    )
  )
)


;;;;;;;;;; USER INTERACTION ;;;;;;;;;; 
(defun start-menu ()
  "Funcao que mostra o menu inicial do jogo e obtem respota do utilizador (para comecar ou sair)"
  (progn
    (format t "~% ~% ~%Bem vindo a versao dois do melhor jogo de sempre meu caro! ~%
             1 - Jogar
             2 - Sair ~%")
    (let ((answer (read)))
      (cond ((= answer 1) (select-game-mode))
            ((= answer 2) (format t "Oh :( ~% ~%"))
            (t (progn 
                 (format t "~% ~% >> Respota invalida, vamos tentar outra vez  << ~% ~%")
                 (start-menu)
            ))
      )
    )
  )
)

(defun select-game-mode ()
  "Funcao que constroi o menu de escolha de modo de jogo"
  (progn
    (format t "~%> Escolha modo do jogo ~%1 - Humano VS M�quina ~%2 - M�quina VS M�quina ~%")

    (let ((answer (read)))

      (cond ((OR (not (numberp answer)) (< answer 1) (> answer 2)) 
               (format t "~% ~% >> Respota invalida, vamos tentar outra vez  << ~% ~%")
               (select-game-mode))
            ((= answer 1) (get-first-player))
            (t (get-max-timer))))
  )
)

(defun get-first-player ()
  "Funcao que permite o utilizador definir quem � o primeiro jogador"
  (progn
    (format t "~%> Quem come�a o jogo? ~%1 - Humano ~%2 - M�quina ~%")
    
    (let ((answer (read)))
      (cond ((OR (not (numberp answer)) (< answer 1) (> answer 2)) 
               (format t "~% ~% >> Respota invalida, vamos tentar outra vez  << ~% ~%")
               (get-first-player))
            (t (get-max-timer answer))))
  )
)

(defun get-max-timer (&optional (firstPlayer 1))
  "Funcao que permite o utilizador definir o tempo m�ximo de execu��o de cada jogada da m�quina"
  (progn
    (format t "~%> Tempo m�ximo (em milisegundos) de cada jogada da m�quina (entre 1000 e 5000)~%")
    
    (let ((answer (read)))
      (cond ((OR (not (numberp answer)) (< answer 1000) (> answer 5000)) 
               (format t "~% ~% >> Respota invalida, vamos tentar outra vez  << ~% ~%")
               (get-max-timer firstPlayer))

            (t (start firstPlayer answer))))
  )
)

(defun start (firstPlayer maxTimer)
  (list firstPlayer maxTimer)
)

;;;;;;;;;; ALGORYTHM ;;;;;;;;;; 

(defun init-algo (board algo &optional (heuristica nil) (depth 0))
  "Funcao que aplica o algoritmo escolhido no problema escolhido e depois cria um ficheiro com os resultados (com recurso a funcao \"write-results-to-file\") e mostra na consola (com recurso a funcao \"format-results\")"
  (let* ((start-time (get-internal-real-time))
         (results (funcall algo (create-node board heuristica 0) depth))
         (runtime (float (/ (- (get-internal-real-time) start-time) internal-time-units-per-second)))
         (path (concatenate 'string (get-curr-dir) "/resultados.dat")))

    (progn 
      (format t "             >>> Algoritmo finalizado <<< ~%~%")
      (format t "Os seguntes registos foram guardados em: ~s ~% ~%" path) 

      (format-results results 't algo depth board heuristica runtime)

      (write-results-to-file results algo depth board heuristica runtime path)
    )
  )
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
    ;; por questoes de legibilidade humana, as linhas estao em formats diferentes
    ;; caracteristicas
    (format output "> Caracteristicas: ~% - Algoritmo: ~s ~% - Heuristica: ~a ~% - Profundidade: ~s ~% - Problema: ~s ~% ~%"
             algo heuristica depth board)
    ;; resultados
    (format output "> Resultados: ~% - Nos gerados: ~d ~% - Nos expandidos: ~d ~% - Penetrancia: ~d ~% - Fator de ramificacao: ~d ~% - Tempo de execucao: ~d segundo(s) ~% - Profundidade da solucao: ~d ~%"
            (first results) (second results) (third results) (fourth results) runtime (get-node-depth (fifth results)))
    (get-solucao (fifth results) output)
  )
)

(defun get-solucao (node output)
  "Funcao que faz o output da solucao. Usa \"get-caminho-solucao\" para mostrar o caminho solucao"
  (progn 
    (format output " - Solucao: ")
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
  "Abre o ficheiro problemas.dat existente na (get-curr-dir) e chama as funcoes read-problemas-aux e build-boards  para ler of ficheiro e construir os respetivos ficheiros. Lanaa error se nao encontrar o ficheiro"
  (with-open-file (file 
                   (concatenate 'string (get-curr-dir) "/problemas.dat")
                   :direction :input
                   :if-does-not-exist :error)
      (build-boards (read-problemas-aux file)))
)

(defun read-problemas-aux(input &optional (output))
  "Percorre o ficheiro recebido (input) linha a linha recusrivamente e adiciona-as ao output. No final, retorna o output, que e uma lista cujos elementos sso as varias linhas do ficheiro"
  (let ((line (read-line input nil)))
    (cond ((not (null line))
              (read-problemas-aux input (append output (list line))))
          (t output)))
)

(defun build-boards(stringBoards &optional (boards))
  "Recebe a lista retornada em read-problemas-aux e percorre-a recursivamente, criando listas a partir de cada elemento de {stringBoards}, que sao strings, utilizando a funcao build-board-aux. No final retorna uma lista de lsitas com os varios boards lidos do ficheiro."
  (cond ((null stringBoards) boards)
        (t (build-boards (rest stringBoards) (append boards (list (build-board-aux (first stringBoards)))))))
)

(defun build-board-aux (stringBoard)
  "Separar a string, que representa o tabuleiro recebida (stringBoard), em duas, sendo cada string uma linha do tabuleiro, e constroi uma nova lista com essas duas strings e retorna-a."
  (let ((board (split-sequence "," stringBoard)))
    (list (read-from-string (first board)) (read-from-string (second board))))
)