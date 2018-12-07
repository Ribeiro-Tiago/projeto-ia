;;;; io.lisp
;;;; Funções para entrada e saíde de dados - ecrã / ficheiro
;;;; Disciplina de IA - 2018 / 2019
;;;; Autor: Tiago Alves & Tiago Ribeiro

;;;;;;;;;; INITIALIZATION ;;;;;;;;;; 
(defun start() 
  (load-depedencies)
)

(defun get-curr-dir () 
  "Devolve o caminho absoluto até a diretoria atual"
  (string "C:/Users/Tiago/Documents/ips/IA/projeto/parte 1")
)

(defun load-depedencies ()
  "Funcao que as dependências do projeto" 
  (let ((path (get-curr-dir)))
    (progn
      (compile-file (concatenate 'string path "/puzzle.lisp"))
      ;(compile-file (concatenate 'string path "/procura.lisp"))
      (start-menu (problemas))
    )
  )
)

(defun start-menu (problemas)
  "Função que mostra o menu inicial do jogo"
  (format t "~% ~% ~%Bem vindo ao melhor jogo de sempre meu caro! ~%
             1 - Jogar
             2 - Sair ~%")
  (let ((answer (read)))
    (cond ((= answer 1) (board-pick))
          ((= answer 2) (format t "Oh :("))
          (t 
            (format t "Respota inválida, vamos tentar outra vez")
            (start-menu problemas)
          )))
)

(defun buildProblemaMenu(problemas) 
  
)

(defun selectProblema (problemas)
  "Função que mostra o menu inicial do jogo"
  (format t "~% ~% ~%Escolha um tabuleiro! ~%
             ~A 
             ~D - Sair ~%" (buildProblemasMenu problemas) (list-length problemas))  

  (let ((answer (read))
        (maxAnswer (list-length problemas)))

    (cond ((OR (not (numberp answer)) (< answer 0) (> answer maxAnswer)) 
             (format t "Respota inválida, vamos tentar outra vez")
             (selectProblema problemas))
          (t (comecarAlgo (nth answer problemas)))))
)

(defun comecarAlgo(board)
  (forma t "board escolhido ~A" board)
)


;;;;;;;;;; USER INPUT ;;;;;;;;;; 
(defun read-recursive(stream-in &optional (stream-out))
  (let ((line (read-line stream-in nil)))
    (cond ((not (null line))
              (rr stream-in (append stream-out (list line))))
          (t stream-out)))) 

(defun read-file () 
  (with-open-file (file-stream (concatenate 'string (get-curr-dir) "/problemas.dat"))
      (buildBoard (first (read-recursive file-stream))))
)

(defun buildBoard (stringBoard)
  (let ((board (split-sequence "," stringBoard)))
    (list (read-from-string (first board)) (read-from-string (second board))))
)