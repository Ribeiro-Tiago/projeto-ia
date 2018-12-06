;;;; io.lisp
;;;; Funções para entrada e saíde de dados - ecrã / ficheiro
;;;; Disciplina de IA - 2018 / 2019
;;;; Autor: Tiago Alves & Tiago Ribeiro

(defun get-curr-dir (filename) 
  "Devolve o caminho absoluto até a diretoria atual"
  'C:/Users/Tiago/Documents/ips/IA/
)

(defun read-file (filename)
  "Função que permite ler um ficheiro"
  (with-open-file (ficheiro ((get-curr-dir) filename)
                            :direction :input
                            :if-does-not-exist :error)
	(read ficheiro)
   )
)

(defun write-file (filename content)
  "Função que escreve o {content} recebido num ficheiro com o {filename}"
  (cond ((or (null filename) (null content) NIL))
        ())
)

(defun get-input-type ()
  "Função que pede ao utilizador qual o algoritmo a usar para testar e arranca com o processo todo"
  (format t "Escolha o algoritmo a usar: ~%
             1) BFS
             2) DFS
             3) A*
             4) SMA*
             5) IDA*
             6) RBFS
             7) Sair da aplicação")
  (let ((answer (read)))               
      (cond ((= answer 1) (bfs))
            ((= answer 2) (dfs))
            ((= answer 3) (astar))
            ((= answer 4) (sma))
            ((= answer 5) (ida))
            ((= answer 6) (rbfs))
            ((= answer 7) (format t "Adeus".))
            (t 
               (format t "Resposta inválida, escolha um número entre 1 e 7.~%")
               (get-input-type)
            )
      ))
)

