tabuleiro([
            [_, _, _, _, _, _],
            [_, _, _, _, _, _],
            [_, _, _, _, _, _],
            [_, _, _, _, _, _],
            [_, _, _, _, _, _],
            [_, _, _, _, _, _]
          ]).

n(1).
n(2).
n(3).
n(4).
n(5).
n(6).

%Funcoes para satisfazer as regras de adjacência
completaMenor(X,Y) :- n(X), n(Y), X < Y.
completaMaior(X,Y) :- n(X), n(Y), X > Y.

todosDiferentes([]).
todosDiferentes([H|T]) :- not(member(H,T)), todosDiferentes(T).

%Gera numeros para a lista tal que todos sejam diferentes entre si.
completa([X1, X2, X3, X4, X5, X6]) :-
    n(X1), n(X2), n(X3), n(X4), n(X5), n(X6),
    todosDiferentes([X1, X2, X3, X4, X5, X6]).

solucao(TabuleiroSolucao) :-
    TabuleiroSolucao = tabuleiro([
            [X11, X12, X13, X14, X15, X16],
            [X21, X22, X23, X24, X25, X26],
            [X31, X32, X33, X34, X35, X36],
            [X41, X42, X43, X44, X45, X46],
            [X51, X52, X53, X54, X55, X56],
            [X61, X62, X63, X64, X65, X66]
          ]),

    %Regras de adjacência
    %Primeiro e quarto quadrantes
    completaMenor(X11,X12),
    completaMenor(X11,X21),
    completaMaior(X12,X22),
    completaMaior(X21,X22),
    completaMaior(X22,X32),
    completaMenor(X31,X21),
    completaMenor(X31,X32),
    completa([X11, X12, X21, X22, X31, X32]),

    completaMenor(X41,X42),
    completaMenor(X41,X51),
    completaMenor(X42,X52),
    completaMaior(X51,X52),
    completaMaior(X52,X62),
    completaMenor(X61,X51),
    completaMenor(X62,X61),
    completa([X41, X42, X51, X52, X61, X62]),

    %Colunas 1 e 2
    completa([X11, X21, X31, X41, X51, X61]),
    completa([X12, X22, X32, X42, X52, X62]),

    %Segundo e quinto quadrantes
    completaMaior(X13,X14),
    completaMenor(X13,X23),
    completaMenor(X14,X24),
    completaMaior(X23,X24),
    completaMenor(X24,X34),
    completaMenor(X33,X23),
    completaMaior(X34,X33),
    completa([X13, X14, X23, X24, X33, X34]),

    completaMenor(X43,X44),
    completaMaior(X43,X53),
    completaMaior(X44,X54),
    completaMenor(X53,X54),
    completaMenor(X54,X64),
    completaMaior(X63,X53),
    completaMaior(X64,X63),
    completa([X43, X44, X53, X54, X63, X64]),

    %Colunas 3 e 4
    completa([X13, X23, X33, X43, X53, X63]),
    completa([X14, X24, X34, X44, X54, X64]),

    %Terceiro e sexto quadrantes
    completaMaior(X15,X16),
    completaMenor(X15,X25),
    completaMaior(X16,X26),
    completaMaior(X25,X26),
    completaMenor(X26,X36),
    completaMaior(X35,X25),
    completaMaior(X36,X35),
    completa([X15, X16, X25, X26, X35, X36]),

    completaMenor(X45,X46),
    completaMenor(X45,X55),
    completaMaior(X46,X56),
    completaMenor(X55,X56),
    completaMaior(X56,X66),
    completaMaior(X65,X55),
    completaMenor(X66,X65),
    completa([X45, X46, X55, X56, X65, X66]),

    %Colunas 5 e 6
    completa([X15, X25, X35, X45, X55, X65]),
    completa([X16, X26, X36, X46, X56, X66]),

    %linhas
    completa([X11, X12, X13, X14, X15, X16]),
    completa([X21, X22, X23, X24, X25, X26]),
    completa([X31, X32, X33, X34, X35, X36]),
    completa([X41, X42, X43, X44, X45, X46]),
    completa([X51, X52, X53, X54, X55, X56]),
    completa([X61, X62, X63, X64, X65, X66]).
