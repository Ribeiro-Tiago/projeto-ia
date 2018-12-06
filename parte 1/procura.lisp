;;;; board-handler.lisp
;;;; Implementacao dos algoritmos de procura em espaço de estados
;;;; Disciplina de IA - 2018 / 2019
;;;; Autor: Tiago Alves & Tiago Ribeiro

;;;;; Construtor ;;;;;
(defun create-node (board &optional (depth 0) (heuristica 0) (parent nil))
  (list board depth heuristica parent)
)


;;;;; getters ;;;;;

;; Test: (node-state (create-node (empty-board)))
;; Result: ((0 0 0 0 0 0) (0 0 0 0 0 0))
(defun node-state (node)
  "Devolve o estado do problema neste nó"
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
  "Retorna a heurística associado ao nó"
  (third no)
)

;; Teste: (node-parent (create-node (empty-board)))
;; Result: NIL
(defun node-parent (node)
  "Retorna o nó pai de {node}"
  (fourth node)
)

;; Teste: (node-cost (create-node (empty-board)))
;; Result: 0
(defun node-cost (node)
  "Retorna o custo deste nó com base na heuristica"
  (+ (node-depth node) (node-heuristica node))
)

;; Test: (node-solutionop (create-node (empty-board)))
;; Result: T
(defun node-solutionop (node) 
  "Verifica se o {node} é um nó solução"     
  (cond ((board-emptyp (node-state node))))
)


;;;;; Funções auxiliares aos algos ;;;;;
(defun operadores ()
  "Funcao que lista todos os operadores existentes no dominio do problema"
  '(allocate-pieces)
)



;;;;; Algos ;;;;;
(defun a* (
           node-inicial
           get-sucessores
           &optional (abertos (list node-inicial)) (fechados nil) (nodes-expandidos 0) (nodes-gerados 0))
  "Algoritmo de procura em espaço de estados A*"
  (cond
   ((null abertos) nil)
   (t '(
         (let* (
                (currNode (first abertos)) 
                (newAbertos (rest fechados))
                (newFechados (cons currNode fechados))
               )
           
           (sucessores) 
      )
   ))
  )
)





;;;;; Avaliação de eficiência ;;;;;


;; penetrancia
(defun penetrancia (depth generated-nodes)
  "Funcao que permite calcular a penetrancia de um algoritmo"
  (cond
     ((not (zerop depth)) (float (/ depth generated-nodes)))
     (t 0)
  )
)