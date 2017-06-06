%% Ricardo Zanuzzo

iniciar :- nl, 
           write('=> Programa de adivinhação... '), nl,
           write('=> Responda as perguntas do programa com (sim ou nao)... '), nl,
           write('=> Pense em um animal...'), nl, nl,
           encontrar(Animal), nl, nl,
           write('****************************************'), nl,
           write('O animal que vc pensou eh: '), write(Animal), nl,
           write('****************************************'), nl, nl,
           write('Acertei ? (sim/nao) '), nl,
           read(Resposta), nl,
           ((Resposta == sim ; Resposta == s) -> 
                limpar_base, continuar ;
                write('Hum, errei dessa vez...'), nl,
                write('Qual foi o animal que vc pensou? '), nl,
                read(Nome), nl,
                write('Escreva uma caracteristica para diferenciar '),
                write(Nome), write(' de '), write(Animal), write(': '), nl,
                read(Caracteristica), nl, nl,
                asserta((encontrar(Nome) :- encontrar(Animal), verificar(Caracteristica))),
                limpar_base, continuar).


encontrar(X) :- X = tigre, !.

verificar(Caracteristica) :- (resp_sim(Caracteristica) -> true ; 
                             (resp_nao(Caracteristica) -> fail ; 
                               preguntar(Caracteristica))).

preguntar(Caracteristica) :- nl,
                             write('O animal que vc pensou possui a seguinte caracteristica: '),
                             write(Caracteristica), write('? (sim/nao) '), nl,
                             read(Resp), nl,
                             ((Resp == sim ; Resp == s) -> 
                                assert(resp_sim(Caracteristica)) ; 
                                assert(resp_nao(Caracteristica)), fail).

continuar :- write('Quer continuar jogando? (sim/nao)'), nl,
             read(Resp),
             ((Resp == sim ; Resp == s) -> iniciar ; finalizar).

finalizar :- nl, nl, write('Programa finalizado...'), nl, !.


/*funÃ§Ãµes dinÃ¢micas para ser adicionado as respostas*/
:- dynamic resp_sim/1, resp_nao/1.

limpar_base :- retract(resp_sim(_)),fail.
limpar_base :- retract(resp_nao(_)),fail.
limpar_base.
