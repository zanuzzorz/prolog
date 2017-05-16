%% Ricardo Zanuzzo

iniciar :- nl, 
           write('=> Programa de adivinhaÃ§Ã£o... '), nl,
           write('=> Responda as perguntas do programa com (sim ou nao)... '), nl,
           write('=> Pense em um animal...'), nl, nl,
           encontrar(Animal), nl, nl,
           write('****************************************'), nl,
           write('O animal que vc pensou eh: '), write(Animal), nl,
           write('****************************************'), nl, nl,
           write('Acertei ? (sim/nao) '), nl,
           read(Resposta), nl,
           ((Resposta == sim) -> continuar ;
                write('Hum, errei dessa vez...'), nl,
                write('Qual foi o animal que vc pensou ? '), nl,
                read(Nome), nl,
                write('Escreva uma caracteristica para diferenciar '),
                write(Nome), write(' de '), write(Animal), writeln(': '),
                write("(Ex: tem_pena ou 'come carne')"), nl,
                read(Caracteristica), nl, nl,
                asserta((encontrar(Nome) :- encontrar(Animal), verificar(Caracteristica))),
                continuar).

%% Hipotese inicial (cachorro)
encontrar(X) :- X = cachorro, !.

verificar(Caracteristica) :- nl,
                             write('O animal que vc pensou possui a seguinte caracteristica: '),
                             write(Caracteristica), write('? (sim/nao) '), nl,
                             read(Resp), nl,
                             ((Resp == sim) -> true ; fail).

continuar :- write('Quer continuar jogando? (sim/nao)'), nl,
             read(Resp),
             ((Resp == sim) -> iniciar ; finalizar).

finalizar :- nl, nl, write('Programa finalizado...'), nl, !.
