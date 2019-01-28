;;;; interact.lisp
;;;; Carrega os outros ficheiros de codigo, escreve e le de ficheiros e trata da interacao com o utilizador
;;;; Disciplina de IA - 2018 / 2019
;;;; Autor: Tiago Alves & Tiago Ribeiro

;;;; funcoes que criam starter nodes para testar os algos ;;;;
(defun start-board () 
  "Retorna um possivel tabuleiro de jogo"
  '((8 8 8 8 8 8) (8 8 8 8 8 8))
)
 
(defun start-board2 () 
  "Retorna um possivel tabuleiro de jogo"
  '((1 0 0 0 0 0) (0 0 0 0 0 1))
)

(defun start-board3 () 
  "Retorna um possivel tabuleiro de jogo"
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
    (format t "~%> Escolha modo do jogo ~%1 - Humano VS Maquina ~%2 - Maquina VS Maquina ~%")

    (let ((answer (read)))

      (cond ((OR (not (numberp answer)) (< answer 1) (> answer 2)) 
               (format t "~% ~% >> Respota invalida, vamos tentar outra vez  << ~% ~%")
               (select-game-mode))
            ((= answer 1) (get-first-player))
            (t (get-max-timer))))
  )
)


(defun get-first-player ()
  "Funcao que permite o utilizador definir quem e o primeiro jogador"
  (progn
    (format t "~%> Quem comeca o jogo? ~%1 - Humano ~%2 - Maquina ~%")
    
    (let ((answer (read)))
      (cond ((OR (not (numberp answer)) (< answer 1) (> answer 2)) 
               (format t "~% ~% >> Respota invalida, vamos tentar outra vez  << ~% ~%")
               (get-first-player))

            (t (get-max-timer (1- answer) 0)))) 
  )
)


(defun get-max-timer (&optional (firstPlayer 0) (gameMode 1) &aux (board (start-board)))
  "Funcao que permite o utilizador definir o tempo maximo de execucao de cada jogada da maquina"
  (progn
    (format t "~%> Tempo maximo (em segundos) de cada jogada da maquina (entre 1 e 5)~%")
    
    (let ((answer (read)))
      (cond ((OR (not (numberp answer)) (< answer 1) (> answer 5)) 
               (format t "~% ~% >> Respota invalida, vamos tentar outra vez  << ~% ~%")
               (get-max-timer firstPlayer gameMode))

            (t (progn 
                  (write-results "~% ~%----------------------------------------- ~% ~% »» Muito bem, vamos comecar o jogo! «« ~%~%")
                  (write-to-file "Tabuleiro de jogo: ~a ~%" board) 
                  (cond ((= gameMode 0) (make-play firstPlayer answer board))
                        (t (make-play-ai 0 answer board)))))))
  )
)

(defun make-play-ai (player maxTimer board &optional (score '(0 0)))
  "Faz a jogada do jogador aquina e depois troca de jogador"
  (let ((newPlayer (switch-player player)))

    (cond ((row-emptyp player board)
             (progn 
               (write-results "~% > Maquina ~d nao tem jogadas possiveis. A vez foi passada ~% " player)
               (make-play-ai newPlayer maxTimer board score)))

          (t (let ((startTime (get-internal-real-time)))
               (progn 
                 (format t "~% > Maquina ~d a fazer a sua jogada... ~%" player)
                 (reset-play-stats)
                 (memoizacao (create-node board) player maxTimer)

                 (let* ((jogada (get-node-next-play *no-objetivo*))
                        (newBoard (get-node-state jogada)))

                   (write-results "~% > Maquina ~d jogou " player)
                   (print-play-stats (get-node-play-position jogada) (calc-elapsed-time startTime))
                   (print-board newBoard)
                   (check-for-gameover 
                           newPlayer
                           maxTimer
                           newBoard
                           'make-play-ai
                           (update-score board newBoard player score))))))))
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
  "Se o jogador poder jogar, i.e.: o lado dele tiver pecas, pede uma jogada, executa-a e volta a chamar a funcao. Senao chama a funcao que permite o utilizador passar a vez"
  (let ((newPlayer (switch-player player)))
    
    ;; nao podemos fazer jogada, passamos a vez
    (cond ((row-emptyp player board)
              (progn 
                (cond ((= player 0) (pass-play board))
                      (t (write-results "~% ~% > A maquina nao tem jogadas possiveis. A vez foi passada")))
                (make-play newPlayer maxTimer board score)))

          ;; podemos fazer jogada, vemos se e jogada do user 
          ((= player 0) 
             (let ((newBoard (allocate-pieces player (get-play board player) board)))
               (check-for-gameover 
                       newPlayer 
                       maxTimer 
                       newBoard
                       'make-play
                       (update-score board newBoard player score))))

          ;; podemos fazer jogada e e o pc 
          (t (let ((startTime (get-internal-real-time)))
               (progn 
                 (format t "~% > Maquina a fazer a sua jogada... ~%")
                 (reset-play-stats)
                 (memoizacao (create-node board) player maxTimer)
               
                 (let* ((jogada (get-node-next-play *no-objetivo*))
                        (newBoard (get-node-state jogada)))

                   (write-results "~% > Maquina jogou")
                   (print-play-stats (get-node-play-position jogada) (calc-elapsed-time startTime))
                   (check-for-gameover 
                             newPlayer
                             maxTimer 
                             newBoard
                             'make-play 
                             (update-score board newBoard player score))))))))
)

(defun print-play-stats (casaJogada timeLapsed)
  "Mostra resultados estatisticos da jogada do computador"
  (write-results "~%   - na casa ~d" casaJogada)
  (write-results "~%   - cortou ~d no(s) alfa" *number-cuts-alfa*)
  (write-results "~%   - cortou ~d no(s) beta" *number-cuts-beta*)
  (write-results "~%   - avaliou ~d no(s)" *node-parsed*)
  (write-results "~%   - demorou ~d milisegundos(s)" timeLapsed)
)

(defun check-for-gameover (player maxTimer board nextPlay score)
  "Verifica se o jogo ja acabou. Se acabou passa para a funcao (game-over), senao passa a proxima jogada"
  (cond ((board-emptyp board) (game-over score))
        (t (funcall nextPlay player maxTimer board score)))
)

(defun game-over (score)
  "Mostra mensagem de final, o tabuleiro no seu estado final e estatisticas do jogo"
  (let ((vencedor (get-vencedor score)))
    (format t "~% ~% ~% »» Chegamos ao fim do jogo ««")
    
    (cond ((equal vencedor 'empate) 
                 (write-results "~%~%O jogo acabou em empate com pontuacao final de ~d pontos para ambos os jogadores" (first score)))

          ((equal vencedor 'jogador1) 
                 (progn 
                   (write-results "~%~%O jogo termiou e jogador 1 vence com ~d pontos " (first score))
                   (write-results "sobre o seu adversario que obteve ~d pontos" (second score))))

          (t (progn
               (write-results "~%~%O jogo termiou e jogador2 vence vencedor com ~d pontos " (second score))
               (write-results "sobre o seu adversario que obteve ~d pontos" (first score))))))
)

(defun get-vencedor (score)
  "Determina o vencedor com base na pontuacao final"
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
            
            (t (progn 
                 (write-results "~% > Humano jogou na casa ~d" answer)
                 (1- answer)))))
  )
)

(defun pass-play (board)
  "Informa o utilizador que nao tem jogadas possiveis e espera que ele carregue no enter para passar a frente"
  (progn 
    (write-to-file "~% > Humano nao tem jogadas possiveis")
    (format t "~% ~% > Nao tem jogadas possiveis. Pressione qualquer tecla para passar a vez~%~%")
    (playing-board board)
    (read-char))
)

(defun jogada-invalida (board player)
  "Funcao auxiliar ao get-play para DRY. Esta e chamada quando o jogador escolher uma jogada invalida e  mostra mensagem de erro e chama o get-play novamente"
  (progn 
    (format t "~% ~% >> Jogada invalida, vamos tentar outra vez  << ~% ~%")
    (get-play board player))
)

(defun calc-elapsed-time (startTime)
  (float (/ (- (get-internal-real-time) startTime) internal-time-units-per-second))
)

;;;;;;;;;; OUTPUT ;;;;;;;;;; 
(defun write-results (text &optional (arg nil))
  "Escreve o texto recebido por parametro em ficheiro e no ecra"
  (progn 
    (write-to-file text arg)
    (format t text arg)
  )
)

(defun write-to-file (text &optional (arg nil))
  "Escreve o texto recebido por parametro em ficheiro"
  (with-open-file (output-file 
                   (concatenate 'string (get-curr-dir) "/log.dat")
                   :direction :output
                   :if-exists :append
                   :if-does-not-exist :create)
    (format output-file text arg))
)