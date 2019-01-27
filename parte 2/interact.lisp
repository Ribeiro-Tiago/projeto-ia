;;;; interact.lisp
;;;; Carrega os outros ficheiros de c�digo, escreve e l� de ficheiros e trata da intera��o com o utilizador
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
  (create-node '((8 8 8 8 8 8) (8 8 8 8 8 8)))
)

(defun teste10 ()
  "Funcao que cria no inicial dum tabuleiro aleatorio para testar"
  (create-node '((5 0 0 0 0 0) (0 0 0 0 0 5)))
)

(defun teste2 ()
  "Funcao que cria no inicial dum tabuleiro aleatorio para testar"
  (create-node '((2 0 0 0 0 0) (0 0 0 2 0 0)))
)


(defun teste3 ()
  "Funcao que cria no inicial reference ao problema a para testar"
  (create-node '((0 0 0 0 0 2) (0 0 0 0 4 0)))
) 

(defun teste4 ()
  "Funcao que cria no inicial reference ao problema b para testar"
  (create-node '((2 2 2 2 2 2) (2 2 2 2 2 2)))
)

(defun teste5 ()
  "Funcao que cria no inicial reference ao problema c para testar"
  (create-node '((0 3 0 3 0 3) (3 0 3 0 3 0)))
)

(defun teste6 ()
  "Funcao que cria no inicial reference ao problema f para testar"
  (create-node '((48 0 0 0 0 0) (0 0 0 0 0 48)))
)

(defun teste7 ()
  "Funcao que cria no inicial reference ao problema d para testar"
  (create-node '((1 2 3 4 5 6) (6 5 4 3 2 1)))
)

(defun teste8 ()
  "Funcao que cria no inicial reference ao problema e para testar"
  (create-node '((2 4 6 8 10 12) (12 10 8 6 4 2)))
)

(defun teste9 ()
  "Funcao que cria no inicial reference ao problema a para testar"
  (create-node '((2 2 2 2 2 2) (2 2 2 2 2 2)))
)

(defun no-teste ()
  "Define um no teste do problema da vasilhas em que A=2, B=2, profundidade=0 e pai=NIL"
  (create-node '((5 0 0 0 0 0) (0 0 0 0 0 5)) 0)
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

            (t (get-max-timer (1- answer) 0)))) 
  )
)


(defun get-max-timer (&optional (firstPlayer 0) (gameMode 1) &aux (board (start-board3)))
  "Funcao que permite o utilizador definir o tempo m�ximo de execu��o de cada jogada da m�quina"
  (progn
    (format t "~%> Tempo m�ximo (em segundos) de cada jogada da m�quina (entre 1 e 5)~%")
    
    (let ((answer (read)))
      (cond ((OR (not (numberp answer)) (< answer 1) (> answer 5)) 
               (format t "~% ~% >> Respota invalida, vamos tentar outra vez  << ~% ~%")
               (get-max-timer firstPlayer gameMode))

            (t (progn 
                  (format t "~% ~% �� Muito bem, vamos come�ar o jogo! �� ~%~%")
                   (cond ((= gameMode 0) (make-play firstPlayer answer board))
                         (t (make-play-ai 0 answer board)))))))
  )
)

(defun make-play-ai (player maxTimer board &optional (score '(0 0)))
  "Faz a jogada do jogador aquina e depois troca de jogador"
  (let ((newPlayer (switch-player player)))

    (cond ((row-emptyp player board)
             (progn 
               (format t "~% > M�quina ~d n�o tem jogadas possiveis. A vez foi passada ~% " player)
               (make-play-ai newPlayer maxTimer board score)))

          (t (progn 
               (format t "~% > M�quina ~d a fazer a sua jogada... ~%" player)
               (alfabeta (create-node board) player maxTimer)

               (let* ((jogada (get-node-next-play *no-objetivo*))
                      (newBoard (get-node-state jogada)))

                 (format t "~% > M�quina ~d jogou na casa ~d." player (get-node-play-position jogada))
                 (print-board newBoard)
                 (check-for-gameover 
                          newPlayer
                          maxTimer
                          newBoard
                          'make-play-ai
                          (update-score board newBoard player score)))))))
)

(defun get-pecas-comidas (oldBoard newBoard)
  "Ve a diferenca nas pecas entre o novo tabuleiro e o anterior para ver quantas foram comidas"
  (- (board-value oldBoard) (board-value newBoard))
)

(defun update-score (oldBoard newBoard player score)
  "Calcula as pecas que foram comidas e atualiza o respetivo score na lista das pontuacoes"
  (let* ((pecasComidas (get-pecas-comidas oldBoard newBoard))
        (newValue (+ (nth player score) pecasComidas)))
    
    (cond ((= player 0) (list newValue (second score)))
          (t (list (first score) newValue))))
)

(defun make-play (player maxTimer board &optional (score '(0 0)))
  "Se o jogador poder jogar, i.e.: o lado dele tiver pe�as, pede uma jogada, executa-a e volta a chamar a funcao. Senao chama a funcao que permite o utilizador passar a vez"
  (let ((newPlayer (switch-player player)))
    
    ;; n�o podemos fazer jogada, passamos a vez
    (cond ((row-emptyp player board)
              (progn 
                (cond ((= player 0) (pass-play board))
                      (t (format t "~% ~% > A m�quina n�o tem jogadas possiveis. A vez foi passada")))
                (make-play newPlayer maxTimer board score)))

          ;; podemos fazer jogada, vemos se � jogada do user 
          ((= player 0) 
             (let ((newBoard (allocate-pieces player (get-play board player) board)))
               (check-for-gameover 
                       newPlayer 
                       maxTimer 
                       newBoard
                       'make-play
                       (update-score board newBoard player score))))

          ;; podemos fazer jogada e � do pc 
          (t (progn 
               (format t "~% > M�quina a fazer a sua jogada... ~%")
               (alfabeta (create-node board) player maxTimer)
               
               (let* ((jogada (get-node-next-play *no-objetivo*))
                      (newBoard (get-node-state jogada)))

                 (format t "~% > M�quina jogou na casa ~d." (get-node-play-position jogada))
                 (check-for-gameover 
                           newPlayer
                           maxTimer 
                           newBoard
                           'make-play 
                           (update-score board newBoard player score)))))))
)

(defun check-for-gameover (player maxTimer board nextPlay score)
  "Verifica se o jogo ja acabou. Se acabou passa para a funcao (game-over), senao passa a proxima jogada"
  (cond ((board-emptyp board) (game-over board score))
        (t (funcall nextPlay player maxTimer board score)))
)

(defun game-over (board score)
  "Mostra mensagem de final, o tabuleiro no seu estado final e estatisticas do jogo"
  (format t "~%yay, finitooooo ~%")
  (format t "O vencedor � ~s!~% " (get-vencedor score))
  (print-board board)
)

(defun get-vencedor (score)
  (let ((player1 (first score))
        (player2 (second score)))

    (cond ((> player1 player2) 'Jogador1)
          ((< player1 player2) 'Jogador2)
          (t 'empate)))
)


(defun get-play (board player)
  "Mostra o tabuleiro atual ao jogador e pede uma jogada. Se valida (entre 1 e 6 com valor da casa > 0) devolve a posicao dessa jogada. Senao pede uma jogada valida"
  (progn 
    (format t "~% ~% > Escolha a sua jogada (1 - 6) ~%~%")
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
    (format t "~% ~% > N�o tem jogadas possiveis. Pressione qualquer tecla para passar a vez~%~%")
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
