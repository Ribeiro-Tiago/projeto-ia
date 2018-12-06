;;;; board-builder.lisp
;;;; Fun��es de constru��o dos v�rios tabuleiros
;;;; Disciplina de IA - 2018 / 2019
;;;; Autor: Tiago Alves & Tiago Ribeiro

(defun empty-board (&optional (linhas 2) (colunas 6))
  "Retorna um tabuleiro vazio: 0 em todas as c�lulas"
  (make-list linhas :initial-element (make-list colunas :initial-element '0))
)

(defun start-board ()
  (board-g)
)

(defun board-a ()
  "Retorna o tabuleiro do problema a do enunciado"
  '((0 0 0 0 0 2)
    (0 0 0 0 4 0))
)

(defun board-b ()
  "Retorna o tabuleiro do problema b do enunciado"
  (make-list 2 :initial-element (make-list 6 :initial-element '2))
)

(defun board-c ()
  "Retorna o tabuleiro do problema c do enunciado"
  '((0 3 0 3 0 3)
    (3 0 3 0 3 0))
)

(defun board-d ()
  "Retorna o tabuleiro do problema d do enunciado"
  '((1 2 3 4 5 6)
    (6 5 4 3 2 1))
)

(defun board-e ()
  "Retorna o tabuleiro do problema e do enunciado"
  '((2 4 6 8 10 12)
    (12 10 8 6 4 2))
)

(defun board-f ()
  "Retorna o tabuleiro do problema f do enunciado"
  '((48 0 0 0 0 0)
    (0 0 0 0 0 48))
)

(defun board-g ()
  "Retorna o tabuleiro do problema g do enunciado"
  (make-list 2 :initial-element (make-list 6 :initial-element '8))
)

(defun print-board (board) 
  "Prints a pretty version of the board"
  (format t "(~A ~% ~A)" (first board) (second board))
)