 ;;;; board-handler.lisp
;;;; Implementacao dos algoritmos de procura em espa�o de estados
;;;; Disciplina de IA - 2018 / 2019
;;;; Autor: Tiago Alves & Tiago Ribeiro

;;;;; Construtor ;;;;;
;; custo = f | heuristica = g
(defun create-node (board heuristica custo &optional (depth 0) (parent nil))
  (list board heuristica custo depth parent)
)


;;;;; getters ;;;;;

;; Test: (node-state (create-node (empty-board)))
;; Result: ((0 0 0 0 0 0) (0 0 0 0 0 0))
(defun get-node-state (node)
  "Devolve o estado do problema neste n�"
  (first node)
)

;; Teste: (node-heuristic (create-node (empty-board)))
;; Result: 0
; num_pe�as_por_capturar - num_pe�as_capturadas
(defun get-node-heuristic (no)
  "Retorna a heur�stica associado ao n�"
  (second no)
)

;; Teste: (node-heuristic (create-node (empty-board)))
;; Result: 0
(defun get-node-cost (no)
  "Retorna a heur�stica associado ao n�"
  (third no)
)

;; Teste: (node-depth (create-node (empty-board)))
;; Result: 0
(defun get-node-depth (node)
  "Retorna a profundidade do {node} recebido"
  (fourth node)
)

;; Teste: (node-parent (create-node (empty-board)))
;; Result: NIL
(defun get-node-parent (node)
  "Retorna o n� pai de {node}"
  (fifth node)
)

;; Teste: (node-cost (create-node (empty-board)))
;; Result: 0
(defun calc-node-cost (node)
  "Retorna o custo deste n� com base na heuristica"
  (+ (node-depth node) (node-heuristica node))
)

;; Test: (node-solutionop (create-node (empty-board)))
;; Result: T
(defun node-solutionop (node) 
  "Verifica se o {node} � um n� solu��o"     
  (cond ((board-emptyp (node-state node))))
)


;;;;; Fun��es auxiliares aos algos ;;;;;
(defun shortest-cost-sort-compare (a b)
  "Fun��o de compara��o de custo mais baixo para o sort"
  (< (nth 2 a) (nth 2 b))

(defun in-fechadosp (board fechados)
  "Verifica se {node} encontra-se na lista dos {fechados}"
  (cond ((null fechados) nil)
         ((equal board (get-node-state (first fechados))) T)
         (t (in-fechadosp board (rest fechados))))
)

(defun sucessores (node fechados)
  "Percorre as posi��es todas do estado do {node} e gera os seus n�s sucessores"
  (loop for rowIndex from 0 to 1
     append (loop for cellIndex from 0 to 5
                  collect (sucessores-aux rowIndex cellIndex node fechados)))
)


(defun sucessores-aux (rowIndex cellIndex node fechados)
  (let ((board (get-node-state node)))

    (cond ((is-move-validp rowIndex cellIndex board)

        (let ((newBoard (allocate-pieces rowIndex cellIndex board)))

           (cond ((not (in-fechadosp newBoard fechados))
                    (create-node 
                        newBoard
                        'calc-heuristica
                        (+ (calc-heuristica newBoard node) (get-node-cost node))
                        (1+ (get-node-depth node))
                        node)
                 )
           )
        )
    ))
  )
)

(defun board-value (board) 
  "Calcula o valor total (soma do valor de cada posi��o) do tabuleiro recebido"
  (+ (apply '+ (first board)) (apply '+ (second board)))
)

(defun calc-heuristica (board node)
  "Calcula a heuristica predefinida"
  (let ((newBoardValue (board-value board)))
    (- newBoardValue (- (board-value (get-node-state node)) newBoardValue)))
)


;;;;; Algos ;;;;;
(defun a* (node-inicial heuristica &optional (abertos (list node-inicial)) (fechados nil) (nodes-expandidos 0) (nodes-gerados 0))
  "Algoritmo de procura em espa�o de estados A*"
  (cond
   ((null abertos) nil)
   (t '(
         (let* ((currNode (first abertos)) 
                (newFechados (cons currNode fechados)))

            (sucessores 
                currNode
                (funcall heuristica currNode)
      )
   ))
  )
)





;;;;; Avalia��o de efici�ncia ;;;;;


;; penetrancia
(defun penetrancia (depth generated-nodes)
  "Funcao que permite calcular a penetrancia de um algoritmo"
  (cond
     ((not (zerop depth)) (float (/ depth generated-nodes)))
     (t 0)
  )
)