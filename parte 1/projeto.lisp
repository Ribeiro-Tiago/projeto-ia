;;;; io.lisp
;;;; Fun��es para entrada e sa�de de dados - ecr� / ficheiro
;;;; Disciplina de IA - 2018 / 2019
;;;; Autor: Tiago Alves & Tiago Ribeiro

;;;;;;;;;; INITIALIZATION ;;;;;;;;;; 
(defun start() 
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
      (start-menu (problemas)
    )
  )
)

(defun start-menu ()
  "Fun��o que mostra o menu inicial do jogo"
  (format t "~% ~% ~%Bem vindo ao melhor jogo de sempre meu caro! ~%
             1 - Jogar
             2 - Sair ~%")
  (let ((answer (read)))
    (cond ((= answer 1) (board-pick))
          ((= answer 2) (format t "Oh :("))
          (t 
            (format t "Respota inv�lida, vamos tentar outra vez")
            (start-menu)
          )))
)

(defun buildProblemaMenu(problemas) 
  
)

(defun selectProblema (problemas)
  "Fun��o que mostra o menu inicial do jogo"
  (format t "~% ~% ~%Escolha um tabuleiro! ~%
             ~A 
             ~D - Sair ~%" (buildProblemasMenu problemas) (list-length problemas))  

  (let ((answer (read))
        (maxAnswer (list-length problemas)))

    (cond ((OR (not (numberp answer)) (< answer 0) (> answer maxAnswer)) (
                                                                          (format t "Respota inv�lida, vamos tentar outra vez")
                                                                          (selectProblema)))
          (t (comecarAlgo (nth answer problemas)))))
)

(defun comecarAlgo(board)
  (forma t "board escolhido ~A" board)
)


;;;;;;;;;; USER INPUT ;;;;;;;;;; 
(defun read-file ()
  "Fun��o que permite ler um ficheiro"
  (with-open-file (ficheiro (concatenate 'string (get-curr-dir) "/problemas.dat")
                   :direction :input
                   :if-does-not-exist :error)
    (read-line ficheiro))
)