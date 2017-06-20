%% Ricardo Zanuzzo
%% Programa realiza uma busca pelo custo minimo no problema do caixeiro viajante (TSP).
%% Para iniciar, execute o predicado tsp.
%% tsp(Cidade) ou tsp(Cidade, Caminho, Custo).
%% Ex:
%% ?- tsp('POA').
%% ?- tsp('Cha', X, Y).

%% Base de dados com as cidades e a distancia entre elas
distancia('POA', 'FLO', 457).
distancia('POA', 'Ctba', 741).
distancia('POA', 'SP', 1143).
distancia('POA', 'RJ', 1568).
distancia('POA', 'BH', 1721).
distancia('POA', 'Bra', 2166).
distancia('POA', 'Ass', 1106).
distancia('POA', 'Cha', 452).

distancia('FLO', 'Ctba', 301).
distancia('FLO', 'SP', 704).
distancia('FLO', 'RJ', 1129).
distancia('FLO', 'BH', 1277).
distancia('FLO', 'Bra', 1676).
distancia('FLO', 'Ass', 1264).
distancia('FLO', 'Cha', 552).

distancia('Ctba', 'SP', 416).
distancia('Ctba', 'RJ', 842).
distancia('Ctba', 'BH', 990).
distancia('Ctba', 'Bra', 964).
distancia('Ctba', 'Ass', 1264).
distancia('Ctba', 'Cha', 480).

distancia('SP', 'RJ', 434).
distancia('SP', 'BH', 584).
distancia('SP', 'Bra', 1008).
distancia('SP', 'Ass', 1348).
distancia('SP', 'Cha', 885).

distancia('RJ', 'BH', 439).
distancia('RJ', 'Bra', 1167).
distancia('RJ', 'Ass', 1773).
distancia('RJ', 'Cha', 1319).

distancia('BH', 'Bra', 737).
distancia('BH', 'Ass', 1763).
distancia('BH', 'Cha', 1470).

distancia('Bra', 'Ass', 1825).
distancia('Bra', 'Cha', 1755).

distancia('Ass', 'Cha', 771).


%% TSP eh um ciclo Hamiltoniano, retorna o caminho mais curto.
tsp(I) :- lista_cidades(I, L), busca(I,I,L,C,D), imprime(I, C, D), !.
tsp(I, C , D) :- lista_cidades(I, L), busca(I,I,L,C,D), !.


%% lista todas as cidades conectadas a uma cidade especifica
lista_cidades(I, L) :- findall(X, distancia(I,X,_), L1), findall(X, distancia(X,I,_), L2), 
   append(L1,L2,L3), list_to_set(L3, L), !.


%% Predicado usado para verificar se a cidade X está na lista de cidades visitadas 
cidade_pertence(X,[X|_]).
cidade_pertence(X,[_|L]):- cidade_pertence(X,L).


%% Verifica se o Caminho eh constituido por todas as cidades da base de cidades 
percorreu_cidades(_,[]).
percorreu_cidades(Caminho, [H|T]):- cidade_pertence(H,Caminho), percorreu_cidades(Caminho,T).


%% Verifica se existe um caminho da Cidade1 para Cidade2, retorna distancia entre elas 
caminho(C1,C2,Distancia):- distancia(C1,C2,D), Distancia = D.
caminho(C1,C2,Distancia):- distancia(C2,C1,D), Distancia = D.


%% Verifica se eh possivel construir um caminho da Cidade1 para Cidade2 
novo_caminho(C1,C2,RC,RD):- construir_caminho(C1,[C2], RC, C1C2Dist), 
   caminho(C2,C1,LD), RD is C1C2Dist + LD. % Add custo para voltar cidade origem


%% A partir de uma Cidade X, percorre um caminho ate passar por todas as cidades, somando a distacia entre as cidades. 
construir_caminho(C1,[C1|R1],[C1|R1],RD):- RD = 0.
construir_caminho(C1,[C2|Visitados],RC,RD):- caminho(C2,CidadeX,D1),
   not(cidade_pertence(CidadeX,Visitados)),
   construir_caminho(C1,[CidadeX,C2|Visitados],RC,D2),
   RD is D2 + D1.

%% Verifica se existe uma distancia menor que o caminho atual
existe_menor_caminho(C1,C2,Cidades,Distancia):- testa_caminho(C1,CX,Cidades,_,ND), ND < Distancia. 

%% Pega qualquer cidade conectada a cidade especificada e chama outra busca para essa cidade
busca(C1,C1,Cidades,MenorCam,MenorDist):-  nth0(I, Cidades, CX, R), caminho(C1,CX,_),
   busca(C1,CX,Cidades,MenorCam,MenorDist).


%% Realiza uma busca pelo grafo das distancia, e retorna o caminho com menor custo.
busca(C1,C2,Cidades,MenorCam,MenorDist):- testa_caminho(C1,C2,Cidades,Caminho,Distancia),
   not(existe_menor_caminho(C1,C2,Cidades,Distancia)),
   MenorCam = Caminho,
   MenorDist = Distancia.

%% Testa um novo caminho, verificando se ele passou por todas as cidades
testa_caminho(C1,C2,Cidades,Caminho,Distancia):- novo_caminho(C1,C2,Caminho,Distancia),
   percorreu_cidades(Caminho,Cidades).
   %imprime_caminho(Caminho),
   %write(" Distancia = "), write(Distancia), write("KM"), nl.


%% Impressoes especiais para os caminhos e a distancias entre as cidades
imprime(N, C, D) :- nl, write("Caminho com Menor Custo: "), nl,
   append(C, [N], X), imprime_caminho2(X), 
   write(" = "), write(D), write("KM"), nl.

imprime_caminho([]).
imprime_caminho([H|T]):- write(H), write(' '), imprime_caminho(T).

imprime_caminho2([]).
imprime_caminho2([H|[]]):- write(H).
imprime_caminho2([H|T]):- write(H), write(' - '), imprime_caminho2(T).


% Predicado que testa todas as cidades da base de dados.
teste :- tsp('Cha'), tsp('Ass'), tsp('Bra'), tsp('POA'), tsp('SP'), tsp('Ctba'), tsp('RJ'), tsp('BH'), tsp('FLO').


