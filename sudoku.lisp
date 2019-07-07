(defvar board #2a(
    (0 0 0 0)
    (0 0 0 0)
    (0 0 0 0)
    (0 0 0 0)
))

(defvar regrasCima #2a(
    (0 0 0 0)
    (1 2 1 2)
    (0 0 0 0)
    (2 1 2 1)
))

(defvar regrasBaixo #2a(
    (2 1 2 1)
    (0 0 0 0)
    (1 2 1 2)
    (0 0 0 0)
))

(defvar regrasEsquerda #2a(
    (0 2 0 1)
    (0 2 0 2)
    (0 1 0 2)
    (0 1 0 1)
))

(defvar regrasDireita #2a(
    (2 0 2 0)
    (2 0 1 0)
    (1 0 1 0)
    (1 0 2 0)
))

;Funcoes que verificam se as regras de adjacencia sao satisfeitas:
(defun verificaSuperior (i j)
    (if (= i 4)
        T
        (if (or (= i 0) (= i 2))
            (verificaSuperior (+ i 1) j)
            (if (or (= i 1) (= i 3))
                (if (and (= (aref regrasCima i j) 1) (< (aref board i j) (aref board (- i 1) j)))
                    (if (< j 3)
                        (verificaSuperior i (+ j 1))
                        (verificaSuperior (+ i 1) 0)
                    )
                    (if (and (= (aref regrasCima i j) 2) (> (aref board i j) (aref board (- i 1) j)))
                        (if (< j 3)
                            (verificaSuperior i (+ j 1))
                            (verificaSuperior (+ 1 i) 0)
                        )
                        NIL
                    )
                )
            )
        )
    )
)

(defun verificaInferior (i j)
    (if (= i 4)
        T
        (if (or (= i 1) (= i 3))
            (verificaInferior (+ i 1) j)
            (if (or (= i 1) (= i 3))
                (if (and (= (aref regrasBaixo i j) 1) (< (aref board i j) (aref board (+ i 1) j)))
                    (if (< j 3)
                        (verificaInferior i (+ j 1))
                        (verificaInferior (+ i 1) 0)
                    )
                    (if (and (= (aref regrasBaixo i j) 2) (> (aref board i j) (aref board (+ i 1) j)))
                        (if (< j 3)
                            (verificaInferior i (+ j 1))
                            (verificaInferior (+ 1 i) 0)
                        )
                        NIL
                    )
                )
            )
        )
    )
)

(defun verificaEsquerda (i j)
    (if (= i 4)
        T
        (if (or (= j 0) (= j 2))
            (verificaEsquerda i (+ j 1))
            (if (or (= j 1) (= j 3))
                (if (and (= (aref regrasEsquerda i j) 1) (< (aref board i j) (aref board i (- j 1))))
                    (if (< j 3)
                        (verificaEsquerda i (+ j 1))
                        (verificaEsquerda (+ i 1) 0)
                    )
                    (if (and (= (aref regrasEsquerda i j) 2) (> (aref board i j) (aref board i (- j 1))))
                        (if (< j 3)
                            (verificaEsquerda i (+ j 1))
                            (verificaEsquerda (+ 1 i) 0)
                        )
                        NIL
                    )
                )
            )
        )
    )
)

(defun verificaDireita (i j)
    (if (or (= i 4) (= j 4))
        T
        (if (or (= j 1) (= j 3))
            (verificaDireita i (+ j 1))
            (if (or (= j 0) (= j 2))
                (if (and (= (aref regrasDireita i j) 1) (< (aref board i j) (aref board i (+ j 1))))
                    (if (< j 3)
                        (verificaDireita i (+ j 1))
                        (verificaDireita (+ i 1) 0)
                    )
                    (if (and (= (aref regrasDireita i j) 2) (> (aref board i j) (aref board i (+ j 1))))
                        (if (< j 3)
                            (verificaDireita i (+ j 1))
                            (verificaDireita (+ 1 i) 0)
                        )
                        NIL
                    )
                )
            )
        )
    )
)

;Verificacao de linhas, compara cada posicao com outra
(defun verificaLinhas (i)
    (if (= i 4)
        T
        (if (=/ (aref board i 0) (aref board i 1))
            (if (=/ (aref board i 0) (aref board i 2))
                (if (=/ (aref board i 0) (aref board i 3))
                    (if (=/ (aref board i 1) (aref board i 2))
                        (if (=/ (aref board i 1) (aref board i 3))
                            (if (=/ (aref board i 2) (aref board i 3))
                                (verificaLinhas (+ i 1))
                                NIL
                            )
                            NIL
                        )
                        NIL
                    )
                    NIL
                )
                NIL
            )
            NIL
        )
    )
)

;Verificacao de colunas, compara cada posicao com outra
(defun verificaColunas (j)
    (if (= j 4)
        T
        (if (=/ (aref board 0 j) (aref board 1 j))
            (if (=/ (aref board 0 j) (aref board 2 j))
                (if (=/ (aref board 0 j) (aref board 3 j))
                    (if (=/ (aref board 1 j) (aref board 2 j))
                        (if (=/ (aref board 1 j) (aref board 3 j))
                            (if (=/ (aref board 2 j) (aref board 3 j))
                                (verificaColunas (+ j 1))
                                NIL
                            )
                            NIL
                        )
                        NIL
                    )
                    NIL
                )
                NIL
            )
            NIL
        )
    )
)

;Funcao onde eh implementado o backtracking principal do programa
(defun backtracking (possibilidades i j)
    (setf (aref board i j) (car (car possibilidades)))
    (setf (car possibilidades) (cdr (car possibilidades)))
    (write-line (write-to-string board))
    (if (<= i 3)
        (if (< j 3)
            ;trata a proxima posicao, onde nao eh preciso pular uma linha
            (if (not (backtracking (cdr possibilidades) i (+ j 1)))
                (if (not (verificacoes)) 
                    ;se as possibilidades esgotaram, preenche novamente a lista de possibilidades e retorna null   
                    (if (null (car possibilidades))
                        (progn
                            (setf (car possibilidades) '(1 2 3 4))
                            NIL)
                        ;senao, tenta novamente com a proxima possibilidade da lista
                        (backtracking possibilidades i j)
                    )
                    T
                )
                T
            )
            ;mesma ideia acima, mas agora pulando uma linha (pois j = 3)
            (if (< i 3)
                (if (not (backtracking (cdr possibilidades) (+ i 1) 0))
                    (if (not (verificacoes))    
                        (if (null (car possibilidades))
                            (progn
                                (setf (car possibilidades) '(1 2 3 4))
                                NIL
                            )
                            (backtracking possibilidades i j)
                        )
                        T
                    )
                    T
                )
                ;clausula if para tratar da ultima posicao do tabuleiro
                (if (verificacoes)
                    T
                    (if (null (car possibilidades))
                        (progn
                            (setf (car possibilidades) '(1 2 3 4))
                            NIL
                        )
                        (backtracking possibilidades i j)
                    )
                )
            )
        )
        (if (verificacoes)
            T
            NIL
        )
    )
)

;Faz todas as verificacoes para saber se a solucao sera aceita
(defun verificacoes ()
    (if (verificaSuperior 0 0)
        (if (verificaDireita 0 0)
            (if (verificaEsquerda 0 0)
                (if (verificaInferior 0 0)
                    (if (verificaLinhas 0)
                        (if (verificaColunas 0)
                            T
                            NIL
                        )
                        NIL
                    )
                    NIL
                )
                NIL
            )
            NIL
        )
        NIL
    )
)

(defun main ()
    ;Lista de possiveis numeros a preencher o tabuleiro para cada posicao
    (setf possibilidades (list 
    '(1 2 3 4) '(1 2 3 4) '(1 2 3 4) '(1 2 3 4)
    '(1 2 3 4) '(1 2 3 4) '(1 2 3 4) '(1 2 3 4)
    '(1 2 3 4) '(1 2 3 4) '(1 2 3 4) '(1 2 3 4)
    '(1 2 3 4) '(1 2 3 4) '(1 2 3 4) '(1 2 3 4)))
    (backtracking possibilidades 0 0)
    (write-line (write-to-string board))
)

(main)