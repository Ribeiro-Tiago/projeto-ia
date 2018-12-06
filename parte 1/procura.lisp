;;;; board-handler.lisp
;;;; Implementacao dos algoritmos de procura em espa�o de estados
;;;; Disciplina de IA - 2018 / 2019
;;;; Autor: Tiago Alves & Tiago Ribeiro

;;; Construtor
(defun create-node (board &optional (depth 0) (heuristica 0) (parent nil))
  (list board depth heuristica parent)
)


;;; getters 

;; Test: (node-state (create-node (empty-board)))
;; Result: ((0 0 0 0 0 0) (0 0 0 0 0 0))
(defun node-state (node)
  "Devolve o estado do problema neste n�"
  (first node)
)

;; Teste: (node-depth (create-node (empty-board)))
;; Result: 0
(defun node-depth (node)
  "Retorna a profundidade do {node} recebido"
  (second node)
)

;; Teste: (node-heuristic (create-node (empty-board)))
;; Result: 0
(defun node-heuristic (no)
  "Retorna a heur�stica associado ao n�"
  (third no)
)

;; Teste: (node-parent (create-node (empty-board)))
;; Result: NIL
(defun node-parent (node)
  "Retorna o n� pai de {node}"
  (fourth node)
)

;; Teste: (node-cost (create-node (empty-board)))
;; Result: 0
(defun node-cost (node)
  "Retorna o custo deste n� com base na heuristica"
  (+ (node-depth node) (node-heuristica node))
)

;; Test: (node-solutionop (create-node (empty-board)))
;; Result: T
(defun node-solutionop (node) 
  "Verifica se o {node} � um n� solu��o"     
  (cond ((board-emptyp (node-state node))))
)


;;; Fun��es auxiliares aos algos

