;;;; interact.lisp
;;;; Carrega os outros ficheiros de código, escreve e lê de ficheiros e trata da interação com o utilizador
;;;; Disciplina de IA - 2018 / 2019
;;;; Autor: Tiago Alves & Tiago Ribeiro

;;;; funcoes que criam starter nodes para testar os algos ;;;;
(defun start-board () 
  '((8 8 8 8 8 8) (8 8 8 8 8 8))
)
 
(defun start-board2 () 
  '((1 0 0 0 0 0) (0 0 0 0 0 1))
)

(defun start-board3 () 
  '((3 0 0 0 0 0) (0 0 0 0 0 3))
)

(defun teste ()
  "Funcao que cria no inicial reference ao problema g para testar"
  (create-node '((8 8 8 8 8 8) (8 8 8 8 8 8)) 0)
)

(defun teste2 ()
  "Funcao que cria no inicial dum tabuleiro aleatorio para testar"
  (create-node '((5 0 0 0 0 0) (0 0 0 0 0 5)) 0)
)

(defun teste3 ()
  "Funcao que cria no inicial reference ao problema a para testar"
  (create-node '((0 0 0 0 0 2) (0 0 0 0 4 0)) 'heuristica-extra 0)
) 

(defun teste4 ()
  "Funcao que cria no inicial reference ao problema b para testar"
  (create-node '((2 2 2 2 2 2) (2 2 2 2 2 2)) 0)
)

(defun teste5 ()
  "Funcao que cria no inicial reference ao problema c para testar"
  (create-node '((0 3 0 3 0 3) (3 0 3 0 3 0)) 0)
)

(defun teste6 ()
  "Funcao que cria no inicial reference ao problema f para testar"
  (create-node '((48 0 0 0 0 0) (0 0 0 0 0 48)) 0)
)

(defun teste7 ()
  "Funcao que cria no inicial reference ao problema d para testar"
  (create-node '((1 2 3 4 5 6) (6 5 4 3 2 1)) 0)
)

(defun teste8 ()
  "Funcao que cria no inicial reference ao problema e para testar"
  (create-node '((2 4 6 8 10 12) (12 10 8 6 4 2)) 0)
)

(defun teste9 ()
  "Funcao que cria no inicial reference ao problema a para testar"
  (create-node '((2 2 2 2 2 2) (2 2 2 2 2 2)) 0)
)

(defun no-teste ()
  "Define um no teste do problema da vasilhas em que A=2, B=2, profundidade=0 e pai=NIL"
  (create-node '((5 0 0 0 0 0) (0 0 0 0 0 5)) 0 0)
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
  (string "C:/Users/Tiago/Documents/ips/IA/projeto/parte 2"); - Ribeiro windows
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
    (format t "~%> Escolha modo do jogo ~%1 - Humano VS Máquina ~%2 - Máquina VS Máquina ~%")

    (let ((answer (read)))

      (cond ((OR (not (numberp answer)) (< answer 1) (> answer 2)) 
               (format t "~% ~% >> Respota invalida, vamos tentar outra vez  << ~% ~%")
               (select-game-mode))
            ((= answer 1) (get-first-player))
            (t (get-max-timer))))
  )
)


(defun get-first-player ()
  "Funcao que permite o utilizador definir quem é o primeiro jogador"
  (progn
    (format t "~%> Quem começa o jogo? ~%1 - Humano ~%2 - Máquina ~%")
    
    (let ((answer (read)))
      (cond ((OR (not (numberp answer)) (< answer 1) (> answer 2)) 
               (format t "~% ~% >> Respota invalida, vamos tentar outra vez  << ~% ~%")
               (get-first-player))

            (t (get-max-timer (1- answer) t))))
  )
)


(defun get-max-timer (&optional (firstPlayer 0) (hasHuman nil) (board (start-board)) )
  "Funcao que permite o utilizador definir o tempo máximo de execução de cada jogada da máquina"
  (progn
    (format t "~%> Tempo máximo (em milisegundos) de cada jogada da máquina (entre 1000 e 5000)~%")
    
    (let ((answer (read)))
      (cond ((OR (not (numberp answer)) (< answer 1000) (> answer 5000)) 
               (format t "~% ~% >> Respota invalida, vamos tentar outra vez  << ~% ~%")
               (get-max-timer firstPlayer hasHuman board))

            (t (start firstPlayer answer hasHuman board))))
  )
)

(defun start (firstPlayer maxTimer hasHuman board)
  "Comeca o jogo. Mosta mensagem de inicio e pede a primeira jogada"
  (progn 
    (format t "~% ~% »» Muito bem, vamos começar o jogo! «« ~% ~% > Tabuleiro Inicial: ~%~%")
    (make-play firstPlayer maxTimer hasHuman board))
)

(defun make-play (player maxTimer hasHuman board)
  "Se o jogador poder jogar (i.e.: o lado dele tiver peças, pede uma jogada, executa-a e volta a chamar a funcao. Senao chama a funcao que permite o utilizador passar a vez"
  (let ((newPlayer (switch-player player)))
    
    ;; não podemos fazer jogada, passamos a vez
    (cond ((row-emptyp player board)
                   (progn 
                     (pass-play board)
                     (make-play newPlayer maxTimer hasHuman board)))

          ;; podes fazer jogada, é isso que fazemos
          (t (let* ((getPlayFunc (cond ((AND hasHuman (= player 0)) 'get-play)
                                       (t 'get-ai-play)))

                    (newBoard (allocate-pieces player (funcall getPlayFunc board player) board)))


                 (cond ((board-emptyp newBoard) (game-over))

                       (t (make-play newPlayer maxTimer hasHuman newBoard)))))))
)

(defun game-over ()
  (format t "yay, finitooooo") 
)

(defun switch-player (currPlayer)
  "Altera o jogador atual"
  (- 1 currPlayer)
)

(defun get-ai-play (board player)
  (format t "ai play")
  1
)

(defun get-play (board player)
  "Mostra o tabuleiro atual ao jogador e pede uma jogada. Se valida (entre 1 e 6 com valor da casa > 0) devolve a posicao dessa jogada. Senao pede uma jogada valida"
  (progn 
    (format t "~% ~% > Escolha a sua jogada, jogador ~d (1 - 6) ~%~%" player)
    (playing-board board)

    (let ((answer (read)))
      (cond ((OR (not (numberp answer)) (< answer 1) (> answer 6)) (jogada-invalida board player))

            ((not (move-validp player (1- answer) board)) (jogada-invalida board player))
            
            (t (1- answer))))
  )
)

(defun pass-play (board)
  "Informa o utilizador que nao tem jogadas possiveis e espera que ele carregue no enter para passar a frente"
  (progn 
    (format t "~% ~% > Não tem jogadas possiveis. Pressione qualquer tecla para passar a vez~%~%")
    (playing-board board)
    (read-char))
)

(defun jogada-invalida (board player)
  "Funcao auxiliar ao get-play para DRY. Esta e chamada quando o jogador escolher uma jogada invalida e  mostra mensagem de erro e chama o get-play novamente"
  (progn 
    (format t "~% ~% >> Jogada invalida, vamos tentar outra vez  << ~% ~%")
    (get-play board player))
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
