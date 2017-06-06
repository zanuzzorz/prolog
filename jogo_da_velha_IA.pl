%% Ricardo Zanuzzo
%% Para iniciar, execute o predicado jv.
%% ?- jv.

jv :- nl,
      write('Jogo da Velha!'), nl,
      write("Voce eh o Jogador 'X', Eu sou 'O'"), nl,
      quem_joga(X), nl,
			imprime_tabuleiro_fixo([1,2,3,4,5,6,7,8,9]), nl,
      write("Para jogar escolha uma posicao do tabuleiro e digite o numero correspondente."), nl,
      joga(X,1,[v,v,v,v,v,v,v,v,v]), limpa_jogada, !.

joga(J1, N, T) :- nl, imprime_tabuleiro(T),
									(J1 == 1, le_jog(N,J1,T,P1) | joga_c(N,T, P1)),     % Verifica se o proximo a jogar eh usuario ou computador
                  salva_jogada(P1),                                   % Salva a jogada atual.
                  executa(J1,P1,T,T1),
                  (fim(N, J1, T1) | proximo(J1,J2), N1 is N + 1,
                  joga(J2,N1,T1)), !.


le_jog(N, J, T, P) :- repeat, write_lista(['Jogada ', N, ' - Jogador ', J,	': ']),
                      le_pos(P), membro_nro(1,P,T,v), !.


le_pos(P) :- repeat, get_single_char(C), put(C), nl,
             catch(number_codes(P,[C]),error(syntax_error(_),_), fail),
             P @>= 1, P @=< 9, !.

proximo(1,2).
proximo(2,1).

executa(J,P,T1,T2) :- substitui(1,P,J,T1,T2), !.

substitui(N,N,J,[_|R],[J|R]):- !.
substitui(N,L,J,[X|R],[X|R1]):- N1 is N + 1, substitui(N1,L,J,R,R1), !.

membro_nro(N,N,[X|R],X) :- !.
membro_nro(N,L,[Y|R],X) :- N1 is N + 1, membro_nro(N1,L,R,X), !.

write_lista([]) :- !.
write_lista([X|R]) :- write(X), write_lista(R), !.

imprime_tabuleiro([X,Y,Z]) :- imp(X), imp(s), imp(Y), imp(s), imp(Z), nl, nl,!.
imprime_tabuleiro([X,Y,Z|R]) :- imp(X), imp(s), imp(Y), 
                                imp(s), imp(Z), nl, imp(l),
                                imprime_tabuleiro(R), !.

imprime_tabuleiro_fixo([X,Y,Z]) :- write(X), imp(s), write(Y), imp(s), write(Z), nl, nl,!.
imprime_tabuleiro_fixo([X,Y,Z|R]) :- write(X), imp(s), write(Y), 
                                     imp(s), write(Z), nl, imp(l), 
                                     imprime_tabuleiro_fixo(R), !.

imp(v):- write(' '), !.
imp(1):- write('X'), !.
imp(2):- write('O'), !.
imp(s):- write(' | '), !.
imp(l):- write('---------'),nl, !.


vitoria(J,[J,J,J,_,_,_,_,_,_]).
vitoria(J,[_,_,_,J,J,J,_,_,_]).
vitoria(J,[_,_,_,_,_,_,J,J,J]).
vitoria(J,[J,_,_,J,_,_,J,_,_]).
vitoria(J,[_,J,_,_,J,_,_,J,_]).
vitoria(J,[_,_,J,_,_,J,_,_,J]).
vitoria(J,[J,_,_,_,J,_,_,_,J]).
vitoria(J,[_,_,J,_,J,_,J,_,_]).


fim(N,J,T):- vitoria(J,T), nl,
						 (J == 1, write_lista(['Voce Venceu! Parabens Jogador', J, '! :(']) |
              write("Voce Perdeu! Eu levei a melhor! :) ")), nl,
	           imprime_tabuleiro(T), findall(F, recorded(jogada, F) ,Z), 
             (J == 1, salva_base(Z, p) | salva_base(Z, v)),                   % Salva o resultado da partida base de dados
             limpa_jogada,limpa_prox, limpa_fitness,  continuar, !.           % Limpa base de jogadas

fim(9,_,_) :- vitoria(J,T), nl, write('Empate! Deu Velha :| '), 
              findall(F, recorded(jogada, F) ,Z), salva_base(Z, e), 
              limpa_jogada, limpa_prox, limpa_fitness, continuar, !.


% -----------------------------------------------------------------------
% Jogadas que serao feitas pelo computador
% -----------------------------------------------------------------------
joga_c(N, T, P) :- write_lista(['Jogada ', N, ' - Computador : ']),
                   findall(F, recorded(base, F), Z), length(Z, X),
                   (X > 0, escolhe_jog(T, N, P) | joga_rand(T, P)), write(P), nl, !.

% Se nao possui uma base salva, o computador faz jogadas aleatorias
joga_rand(T, P) :- repeat, random(1, 10, P), membro_nro(1,P,T,v), !.

% Se possui base, busca jogadas salvas na base
escolhe_jog(T, N, P) :- pontua_tabuleiro(T, X, T1), consulta_base(T, T1, N, P), 
                        membro_nro(1,P,T,v), true | joga_rand(T, P), !.

% Pontua -1 para jogadas jah realizadas na partida.
% E 0.5 para as jogadas nao realizadas ainda.
pontua_tabuleiro([], T, T).
pontua_tabuleiro([X|R], T, NT) :- (X==v, append(T, [0.5], T1) | append(T, [-1], T1)), pontua_tabuleiro(R, T1, NT), !.


% -----------------------------------------------------------------------
% Consulta Base e escolhe melhor jogada
% -----------------------------------------------------------------------
consulta_base(T, TP, N, P) :- converte_tabuleiro(1, T, X, T1),      % Converte tabuleiro atual (pnts em coord)
                              subtract(T1, [v], T2),                % Remove as posicoes que n foram jogadas ainda
                              findall(F, recorded(base, F), Z),     % Pega as jogadas salvas na base
                              (pertence_base(T2, Z),                % Testa se a jogada atual esta salva na base
                               melhor_jogada(TP, N, P) |            % Se sim, calcula melhor jogada
                               joga_rand(T, P)), !.                 % Senao, faz uma jogada aleatoria


% Cada Jogada eh uma lista, e a Base eh uma lista de jogadas (lista de listas)
% -----------------------------------------------------------------------
% Verifica se a Jogada atual esta salva na base
% -----------------------------------------------------------------------
pertence_base(_, []):- fail.  % Se nao encontrar nenhuma jogada igual na base deve falhar
pertence_base(_, []):- !. 
pertence_base(T, [(X, V) | R]) :- (pertence(T, X),                  % Se pertence a base
                                      (salva_prox((X, V)),          % Add a Jogada e o Resultado (v,p,e)
                                       pertence_base(T, R)) |       % Tenta encontrar na base outras jogadas
                                      pertence_base(T, R)), !.      % Senao, soh tenta encontrar outras jogadas

pertence([], _).
pertence([X|R], L) :-  member(X, L), pertence(R, L), !. % Verifca se a Jogada pertence a lista de Jogadas


% -----------------------------------------------------------------------
% Encontra melhor jogada apartir da base
% -----------------------------------------------------------------------
melhor_jogada(T, N, P) :- findall(F, recorded(prox, F), J), flatten(J, JJ),
                          calc_fitness, findall(W, recorded(fitness, W), F), flatten(F, FF),
                          tabuleiro_fitness(N, JJ, FF, T, K, T1),
                          maior(T1, X), nth1(PP, T1, X), P = PP, !.


% Encontra jogada com maior pontuacao do tabuleiro
maior([X], X).
maior([X | R], Z) :- maior(R, I), maior(X, I, Z), !.
maior(X, Y, X) :- X >= Y.
maior(X, Y, Y) :- X < Y.


% -----------------------------------------------------------------------
% Calcula a funcao de expectativa (fitness), para as proximas jogadas
% -----------------------------------------------------------------------
calc_fitness :- findall(F, recorded(prox, F), J1), flatten(J1, J2),             % Lista todas as Proximas jog. possiveis
                busca_jogadas(J2,K,J), conta_jogadas(J, R), calc_fitness(R), !. % Calcula fitness com base nas prox. jog. da base

% Retorna somente as jogadas da base, sem os resultados (v,p,e)
busca_jogadas([], T, T).
busca_jogadas([(X,Y)|R], T, NT) :- append(T, X, T1), busca_jogadas(R, T1, NT), !.


% Calcula o valor da fitness para escolher proxima jogada
calc_fitness(L) :- findall(F, recorded(prox, F), J1), flatten(J1, J2),
                   forall(nth0(I, L, N, R), 
                     (pontua_jogada(N, J2, 0, P, U, Q),        % Soma a pontuacao das jogadas e divide
                      T is P/Q, salva_fitness((N, T)) )),!.    % pela quantidade de vezes que ocorreu


% Pontua as jogadas jah realizadas conforme o resultado
pontua_jogada(_, [], P, P, Q, Q):- !.
pontua_jogada((X,Y), [(X1,Y1) | R1], P, T, Q, QN):- member(X, X1), 
                                                    (pontua(Y1, P1), P2 is P + P1, pontua_jogada((X,Y), R1, P2, T, Y, QN)) |
                                                     pontua_jogada((X,Y), R1, P, T, Q, QN), !.

pontua(v, 2). % Se a jogada resultou em vitoria (v) Pontua 2.
pontua(e, 1). % Se a jogada resultou em empate (e) Pontua 1.
pontua(p, 0). % Se a jogada resultou em derota (p) Pontua 0.


% -----------------------------------------------------------------------
% Conta o numero de proximas jogadas iguais para o calc da fitness
% -----------------------------------------------------------------------
conta_jogadas([],[]).
conta_jogadas([H|T],[(H,N)|L2]) :- contaDel(H,[H|T],L,N), conta_jogadas(L,L2), !.

contaDel(X,T,R,N) :- conta(X,T,N), limpa_lista(X,T,R).

conta(_,[],0).
conta(X,[X|R],N) :- conta(X,R,NN), N is NN + 1.
conta(X,[Y|R],N) :- conta(X,R,N), dif(X,Y).

limpa_lista(_,[],[]).
limpa_lista(X,[X|R],Z) :- limpa_lista(X,R,Z).
limpa_lista(X,[Y|T],[Y|R]) :- dif(X,Y), limpa_lista(X,T,R).

% Conta Jogadas: [((X,Y),Q)]
% EX: [((1,1),2),((2,1),1),((2,3),1),((2,2),2),((1,3),1),((1,2),1),((3,3),2)]

% -----------------------------------------------------------------------
% Pontua o tabuleiro com o rultado da fitness
% -----------------------------------------------------------------------
tabuleiro_fitness(_, [], _, _, T, T).
tabuleiro_fitness(P, [(L,V)|R1], K, T, TT, NT) :- nth1(P, L, E), 
                                                  (encontra_fitness(E, K, Z), 
                                                      (converte(E, W), substitui(1,W,Z,T,T1), 
                                                         tabuleiro_fitness(P, R1, K, T1,  T1, NT)) |
                                                       tabuleiro_fitness(P, R1, K, T, TT, NT)), !.

encontra_fitness(_, [], _):- fail, !.
encontra_fitness(E, [((X,Y),Z)|R], U):- (E == X,  U = Z | encontra_fitness(E,R,U)), !.

% Recebe uma lista de possiveis jogadas e uma lista com a pontuacao para cada jogada
% Pontua o tabuleiro atual com os valores da fitness para as jogadas possiveis a serem realizadas
% PROX [([(1,1),(2,1),(2,2),(2,3),(3,3)],p),([(1,1),(1,3),(2,2),(1,2),(3,3)],p)]
% FITNESS [(((1,1),2),0),(((2,1),1),0),(((2,2),2),0),(((2,3),1),0),(((3,3),2),0),(((1,3),1),0),(((1,2),1),0)]

% -----------------------------------------------------------------------
% Converte tabuleiro de pontos em coordenadas
% -----------------------------------------------------------------------
converte_tabuleiro(_, [], T, T).
converte_tabuleiro(P, [X|R], T, NT) :- (X==v, append(T, [v], T1) | (converte(P, C), append(T, [C], T1))), 
                                        N is P + 1, converte_tabuleiro(N, R, T1, NT), !.


% Pontos em Coordenadas
converte(1, (1,1)).
converte(2, (1,2)).
converte(3, (1,3)).
converte(4, (2,1)).
converte(5, (2,2)).
converte(6, (2,3)).
converte(7, (3,1)).
converte(8, (3,2)).
converte(9, (3,3)).

% Coordenadas em Pontos
converte((1,1), 1).
converte((1,2), 2).
converte((1,3), 3).
converte((2,1), 4).
converte((2,2), 5).
converte((2,3), 6).
converte((3,1), 7).
converte((3,2), 8).
converte((3,3), 9).


% -----------------------------------------------------------------------
% Predicados de interacao com o usuario
% -----------------------------------------------------------------------
quem_joga(X) :- nl, write('Quer iniciar jogando? (sim/nao)'), nl,
                read(Resp),Resp == sim, X=1 | X=2.


continuar :- nl, write('Quer continuar jogando? (sim/nao)'), nl,
             read(Resp),
             (Resp == sim, joga(1,1,[v,v,v,v,v,v,v,v,v]) | finalizar).

finalizar :- grava_base, nl, nl,
             write('Base de Jogadas Salva!'),nl,nl,
             write('Programa finalizado...'), nl, !.

% Salva em um arquivo txt a base com as jogadas e resultados
grava_base :- open('base.txt',write, Stream),
              forall(recorded(base, X), (writeln(Stream,X))),
              close(Stream).


% -----------------------------------------------------------------------
% Predicados para manipular base de dados
% -----------------------------------------------------------------------
salva_base(T,R)  :- recordz(base, (T, R)), !.                % Salva base com todas as jogadas e resultados da partida.
salva_jogada(P)  :- converte(P, C), recordz(jogada, (C)), !. % Salva jogada atual.

% Salva as proximas jogadas possiveis a cada jogada realizada, evitando duplicacoes.
salva_prox(P)    :- recordz(prox, P),
                    findall(F, recorded(prox, F), Z), 
                    flatten(Z, R), list_to_set(R, X),
                    limpa_prox, recordz(prox, X), !.

% Salva o resultado da fitness a cada jogada realizada, evitando duplicacoes.
salva_fitness(P) :- recordz(fitness, P),                        
                    findall(F, recorded(fitness, F), Z), 
                    flatten(Z, R), list_to_set(R, X),
                    limpa_fitness, recordz(fitness, X), !.


limpa_base :- forall(recorded(base,_,R), erase(R)), !.
limpa_jogada :- forall(recorded(jogada,_,R), erase(R)), !.
limpa_prox :- forall(recorded(prox,_,R), erase(R)), !.
limpa_fitness :- forall(recorded(fitness,_,R), erase(R)), !.

imprime_base :- findall(F, recorded(base, F), Z), write_lista(Z).
imprime_jogada :- findall(F, recorded(jogada, F), Z), write_jogada(Z), nl.
imprime_prox :- findall(F, recorded(prox, F), Z), write_lista(Z).
imprime_fitness :- findall(F, recorded(fitness, F), Z), write_lista(Z).

write_jogada([]) :- !.
write_jogada([X|R]) :- write("("), write(X), write(") "), write_jogada(R), !.
