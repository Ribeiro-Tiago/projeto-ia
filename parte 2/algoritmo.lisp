;;;; algoritmo.lisp
;;;; Implementacao do alfabeta
;;;; Disciplina de IA - 2018 / 2019
;;;; Autor: Tiago Alves & Tiago Ribeiro

;;;;;;;;;;;;;;; variaveis globais ;;;;;;;;;;;;;;;
(defvar *hash-table* (make-hash-table))
(defvar *no-objetivo* nil)

;;;;;;;;;;;;;;; Construtor ;;;;;;;;;;;;;;;
;; custo = f | heuristica = g
(defun create-node (board &optional (depth 0) (position -1) (parent nil))
  "Construtor do no das arvores para os algoritmos"
  (list board depth position parent)
)


;;;;;;;;;;;;;;; getters ;;;;;;;;;;;;;;;

;; Test: (get-node-state (teste))
;; Result: ((8 8 8 8 8 8) (8 8 8 8 8 8))
(defun get-node-state (node)
  "Devolve o estado (o tabuleiro) do problema neste no"
  (first node)
)


;; Teste: (get-node-depth (teste))
;; Result: 0
(defun get-node-depth (node)
  "Retorna a profundidade do {node} recebido"
  (second node)
)

(defun get-node-play-position (node)
  "Retorna o número da casa de onde a jogada foi feita, de 1 a 6"
  (1+ (third node))
)

;; Teste: (get-node-parent (teste))
;; Result: NIL
(defun get-node-parent (node)
  "Retorna o no pai de {node}"
  (fourth node)
)

(defun get-node-next-play (node &optional (currNode (get-node-state node)))
  (let ((parent (get-node-parent node)))
    (cond ((null parent) currNode)
          (t (get-node-next-play parent node))))
)

;; Test: (node-solutionp (teste))
;; Result: nil
(defun node-solutionp (node) 
  "Verifica se o {node} e um no solucao"
  (cond ((board-emptyp (get-node-state node))))
)


;;;;;;;;;;;;;;; ALFABETA ;;;;;;;;;;;;;;;
(defun alfabeta (no jogador timeLimit &optional (mDepth 5) (alfa MOST-NEGATIVE-FIXNUM) (beta MOST-POSITIVE-FIXNUM) (startTime (get-universal-time)))
  (cond ((= mDepth 0) (funcao-avaliacao no))
        (t (alfabeta-aux (sucessores-min-max no jogador) mDepth jogador timeLimit alfa beta startTime)) 
  )
)

(defun alfabeta-aux (sucessores mDepth jogador timeLimit alfa beta startTime &optional (trueValue -1))
  (cond ((null sucessores) trueValue)
        (t (let* ((currNode (first sucessores))
                  (valor (alfabeta currNode (switch-player jogador) timeLimit (- mDepth 1) alfa beta startTime)))

              (cond ((= jogador 0)
                      (let ((novoB (min beta valor)))
                         (cond ((<= novoB alfa) beta)
                               (t (progn
                                    (setf *no-objetivo* currNode)
                                    (alfabeta-aux (rest sucessores) mDepth 1 timeLimit alfa novoB startTime valor))))))

                    (t (let ((novoA (max alfa valor)))
                         (cond ((<= beta novoA) alfa)
                               (t (progn 
                                    (setf *no-objetivo* currNode)
                                    (alfabeta-aux (rest sucessores) mDepth 0 timeLimit novoA beta startTime valor))))))))))
)


(defun sucessores-min-max(no jogador)
  (mapcar (lambda (sucessor) (create-node (second sucessor) (1+ (get-node-depth no)) (first sucessor) no))
    
             (sucessores-aux jogador (get-starter-position jogador) (get-node-state no)))
)

(defun sucessores-aux(rowIndex cellIndex board &aux (nextCell (get-next-cellV1 rowIndex cellIndex)))
  (cond ((null cellIndex) nil)

        ((not (move-validp rowIndex cellIndex board)) (sucessores-aux rowIndex nextCell board))

        ((columnsValid rowIndex cellIndex) 
             (append (sucessores-aux rowIndex nextCell board) 
                     (list (list cellIndex (allocate-pieces rowIndex cellIndex board))))))
)

;;;;;;;;;;;;;;; FUNCOES AUX ;;;;;;;;;;;;;;;

(defun switch-player (currPlayer)
  "Altera o jogador atual"
  (- 1 currPlayer)
)

(defun columnsValid(row column)
  (cond ((and (= row 0) (>= column 0) (<= column 5)) t )
        ((and (= row 1) (<= column 5) (>= column 0)) t )
        (t nil))
)

(defun get-next-cellV1 (rowIndex cellIndex)
  "Calcula e retorna o index da proxima coluna com base no index da coluna e linha atual"
  (cond ((eq nil cellIndex) nil)
        ((AND (> cellIndex 0) (= rowIndex 0) ) (- cellIndex 1))
        ((AND (< cellIndex 5) (= rowIndex 1) ) (1+ cellIndex))
        (t nil))
)


(defun get-starter-position (player)
  (cond ((= player 0) 5)
        (t 0))
)

(defun funcao-avaliacao (no)
  (let ((valorA (board-value (get-node-state no)))
        (valorB (board-value (get-node-state (get-node-parent no)))))

    (- valorB valorA))
)



;;;;;;;;;;;;;;; MEMOIZACAO ;;;;;;;;;;;;;;;
(defun memoizacao (node)
  "Funcao que verifica se ja existe um resultado alfabeta para o no passado na hash table, caso exista devolve-o, caso nao exista calcula o seu valor, retorna-o e insere-o na hash table"
  (let ((nodeTable (gethash (hash-node node) *hash-table*)))
    (cond ((null nodeTable) ;; calcula  
           (let ((newNode (alfabeta))) (progn
                                         (setf (gethash (hash-node node) *hash-table*) newNode)
                                         newNode)))
          (t nodeTable)))
)

(defun hash-node (no)
  "Funcao que converte o estado e as pecas dos jogadores de um no numa string para que possa ser usada como key na hash table de memoizacao"
  (concatenate 'string (to-string (get-node-state no)) (to-string (get-node-depth no)))
)

(defun to-string (item)
  (format nil "~S" item)
)
