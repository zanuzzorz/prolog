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

tsp(I,D,C) :- todas_cidades(I,L), busca(I,I,0,[],L,D,C), !.

todas_cidades(I,L) :- findall(X, distancia(I,X,_), L1),
         findall(X,distancia(X,I,_), L2), append(L1,L2,L), !.

busca(I,C1, D1, LV, [], D2, LV) :- dist(C1,I,D3), D2 is D1 + D3,!.
busca(I,C1, D1, LV, LN, D2, C) :- menor_dist(C1, LN, C3, D3),
         D4 is D1 + D3, poe_fim(LV,C3,LV1),
         subtract(LN,[C3],LN1), busca(I,C3,D4,LV1,LN1,D2,C), !.

menor_dist(I, [X], X, D) :- dist(I,X,D), !.
menor_dist(I, [X|R], C, D) :- menor_dist(I, R,  C1, D1),			   dist(I,X,D2), (D2 < D1, D = D2, C = X | D = D1,
         C = C1), !.

dist(X,Y,D) :- distancia(X,Y,D), !.
dist(X,Y,D) :- distancia(Y,X,D), !.

poe_fim([],C,[C]) :- !.
poe_fim([X|R1],C,[X|R2]) :- poe_fim(R1,C,R2), !.
