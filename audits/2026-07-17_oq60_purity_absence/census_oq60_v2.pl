% OQ-60 census probe v2 — for POST-C-LATENT engines (2026-07-23).
%
% v1 (census_oq60.pl) tags m1 via scope_invariance_test(C, variant([])) and
% cross-checks m2 via "real coupling score == 0.0" — both tokens were RETIRED
% by the C-LATENT producer commit (variant([]) → no_data; grid<2 coupling →
% FAIL). Running v1 on a post-C-LATENT engine reports m1=0 VACUOUSLY
% (absence-satisfies-the-gate in the instrument). This v2 reads the post-fix
% tokens; its positive control still must fire all five branches in-process.
%
% Run per leg (serialized, fresh process each), from prolog/:
%   swipl -g "[stack], consult('<audit>/census_oq60_v2.pl'), run_leg(testsets, '<audit>/census_<leg>_<date>.tsv'), halt" -t "halt(1)"

:- use_module(library(lists)).

leg_setup(Leg) :-
    ( Leg == testsets
    ->  true
    ;   retractall(config:param(corpus_path, _)),
        asserta(config:param(corpus_path, Leg))
    ),
    corpus_loader:load_all_testsets,
    cache_registry:clear_all_caches.

grid_size(C, G) :-
    boltzmann_compliance:coupling_test_powers(Powers),
    boltzmann_compliance:coupling_test_scopes(Scopes),
    findall(t,
        (   member(P, Powers), member(S, Scopes),
            boltzmann_compliance:coupling_test_context(P, S, Ctx),
            boltzmann_compliance:classify_at_context(C, Ctx, _)
        ),
        Ts),
    length(Ts, G).

floor_clause(C, override) :- narrative_ontology:boltzmann_floor_override(C, _), !.
floor_clause(C, type)     :- narrative_ontology:coordination_type(C, _), !.
floor_clause(_, default).

census_line(S, C) :-
    ( boltzmann_compliance:epistemic_access_check(C, true) -> Gate = pass ; Gate = fail ),
    ( catch(purity_scoring:factorization_subscore(C, F), _, F = err) -> true ; F = fail ),
    ( catch(purity_scoring:scope_invariance_subscore(C, SIsub), _, SIsub = err) -> true ; SIsub = fail ),
    ( catch(purity_scoring:coupling_cleanliness_subscore(C, CC), _, CC = err) -> true ; CC = fail ),
    ( catch(purity_scoring:excess_extraction_subscore(C, EX), _, EX = err) -> true ; EX = fail ),
    ( catch(purity_scoring:purity_score(C, P), _, P = err) -> true ; P = fail ),
    % mech 1 (post-fix token): scope_invariance_test = no_data
    ( catch(boltzmann_compliance:scope_invariance_test(C, SR), _, SR = err) -> true ; SR = fail ),
    ( SR = invariant -> (N = 1, RawSI = 1.0)
    ; SR = variant(Ts), is_list(Ts) -> (length(Ts, N), RawSI is 1.0 - (N - 1) * 0.25)
    ; (N = na, RawSI = na)
    ),
    ( SR == no_data -> M1 = 1 ; M1 = 0 ),
    % mechs 2 & 3: shared grid
    ( catch(grid_size(C, G), _, G = err) -> true ; G = err ),
    ( integer(G), G < 2 -> M2 = 1 ; M2 = 0 ),
    ( integer(G), G =:= 0 -> M3 = 1 ; M3 = 0 ),
    % anti-reconstruction cross-check for M2 (post-fix): G<2 ==> real
    % cross_index_coupling FAILS (no fabricated 0.0)
    ( M2 =:= 1
    ->  ( catch(boltzmann_compliance:cross_index_coupling(C, _XS), _, fail)
        -> X2 = 'MISMATCH-coupling-succeeded' ; X2 = ok )
    ;   X2 = na ),
    % mech 4: excess_extraction/2 fails
    ( catch(boltzmann_compliance:excess_extraction(C, _), _, fail) -> M4 = 0 ; M4 = 1 ),
    % mech 5: floor default clause would be taken (pre-C-FLOOR HEAD) /
    % floor FAILS on absent coordination_type (post-C-FLOOR HEAD)
    floor_clause(C, FClause),
    ( FClause == default -> M5 = 1 ; M5 = 0 ),
    ( M5 =:= 1
    ->  ( catch(boltzmann_compliance:boltzmann_floor_for(C, FV), _, FV = failed)
        ->  ( config:param(boltzmann_floor_default, DF), number(FV), FV =:= DF
            -> X5 = ok_prefloor          % pre-C-FLOOR: default still fabricated
            ;  X5 = 'MISMATCH'-FV )
        ;   X5 = ok_postfloor )          % post-C-FLOOR: floor fails as specified
    ;   X5 = na ),
    ( Gate == fail -> Disp = sentinel
    ; ( M1 =:= 1 ; M2 =:= 1 ; M3 =:= 1 ; M4 =:= 1 ; M5 =:= 1 ) -> Disp = unknown
    ; Disp = scored ),
    ( ( M1 =:= 1 ; M2 =:= 1 ; M3 =:= 1 ; M4 =:= 1 ; M5 =:= 1 ) -> AnyMech = 1 ; AnyMech = 0 ),
    format(S, '~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w~n',
        [C, Gate, P, F, SIsub, CC, EX, SR, N, RawSI, G, M1, M2, M3, M4, M5, AnyMech, Disp, x2(X2)-x5(X5)]).

corpus_ids(IDs) :-
    findall(C, corpus_loader:corpus_constraint(C), IDs0),
    sort(IDs0, IDs).

run_leg(Leg, OutFile) :-
    leg_setup(Leg),
    corpus_ids(IDs),
    length(IDs, NPop),
    format(user_error, '~n[census-v2] leg=~w population=~w~n', [Leg, NPop]),
    open(OutFile, write, S),
    format(S, 'constraint\tgate\tpurity\tF\tSIsub\tCC\tEX\tscope_result\tN\trawSI\tgridG\tm1_si_no_data\tm2_grid_lt2\tm3_grid_empty\tm4_ex_fail\tm5_floor_default\tany_mech\tdisposition\tcrosschecks~n', []),
    forall(member(C, IDs), census_line(S, C)),
    close(S),
    summarize(Leg, IDs, OutFile).

summarize(Leg, IDs, OutFile) :-
    count_where(IDs, [C]>>(boltzmann_compliance:epistemic_access_check(C, true)), NGatePass),
    count_where(IDs, [C]>>tag(C, m1), M1n),
    count_where(IDs, [C]>>tag(C, m2), M2n),
    count_where(IDs, [C]>>tag(C, m3), M3n),
    count_where(IDs, [C]>>tag(C, m4), M4n),
    count_where(IDs, [C]>>tag(C, m5), M5n),
    count_where(IDs, [C]>>tag(C, any), AnyN),
    count_where(IDs, [C]>>( boltzmann_compliance:epistemic_access_check(C, true), tag(C, m1) ), GPM1),
    count_where(IDs, [C]>>( boltzmann_compliance:epistemic_access_check(C, true), tag(C, m2) ), GPM2),
    count_where(IDs, [C]>>( boltzmann_compliance:epistemic_access_check(C, true), tag(C, m3) ), GPM3),
    count_where(IDs, [C]>>( boltzmann_compliance:epistemic_access_check(C, true), tag(C, m4) ), GPM4),
    count_where(IDs, [C]>>( boltzmann_compliance:epistemic_access_check(C, true), tag(C, m5) ), GPM5),
    length(IDs, Pop),
    format(user_error, '[census-v2:~w] pop=~w gate_pass=~w | raw m1=~w m2=~w m3=~w m4=~w m5=~w any=~w~n',
        [Leg, Pop, NGatePass, M1n, M2n, M3n, M4n, M5n, AnyN]),
    format(user_error, '[census-v2:~w] GATE-PASS victims m1=~w m2=~w m3=~w m4=~w m5=~w~n',
        [Leg, GPM1, GPM2, GPM3, GPM4, GPM5]),
    positive_control(Leg),
    format(user_error, '[census-v2:~w] wrote ~w~n', [Leg, OutFile]).

count_where(IDs, Pred, N) :-
    include(Pred, IDs, Sub), length(Sub, N).

tag(C, m1) :- boltzmann_compliance:scope_invariance_test(C, no_data).
tag(C, m2) :- grid_size(C, G), G < 2.
tag(C, m3) :- grid_size(C, G), G =:= 0.
tag(C, m4) :- \+ catch(boltzmann_compliance:excess_extraction(C, _), _, fail).
tag(C, m5) :- floor_clause(C, Cl), Cl == default.
tag(C, any) :- ( tag(C,m1);tag(C,m2);tag(C,m3);tag(C,m4);tag(C,m5) ), !.

% Positive control: bare gate-passing constraint must fire ALL FIVE branches
% in the reporting process. Post-C-LATENT expectation: purity = unknown
% (pre-C-LATENT engines would show 1.0 — do not run v2 there; use v1).
positive_control(Leg) :-
    C = oq60_control_bare,
    retractall(constraint_indexing:constraint_classification(C, _, _)),
    Ctx1 = context(agent_power(powerless), time_horizon(generational), exit_options(trapped), spatial_scope(national)),
    Ctx2 = context(agent_power(moderate), time_horizon(generational), exit_options(mobile), spatial_scope(national)),
    Ctx3 = context(agent_power(analytical), time_horizon(civilizational), exit_options(analytical), spatial_scope(global)),
    assertz(constraint_indexing:constraint_classification(C, snare, Ctx1)),
    assertz(constraint_indexing:constraint_classification(C, snare, Ctx2)),
    assertz(constraint_indexing:constraint_classification(C, snare, Ctx3)),
    cache_registry:clear_all_caches,
    ( boltzmann_compliance:epistemic_access_check(C, true) -> Gate = pass ; Gate = gate_fail ),
    ( tag(C,m1)->B1=1;B1=0 ), ( tag(C,m2)->B2=1;B2=0 ), ( tag(C,m3)->B3=1;B3=0 ),
    ( tag(C,m4)->B4=1;B4=0 ), ( tag(C,m5)->B5=1;B5=0 ),
    purity_scoring:purity_score(C, P),
    format(user_error, '[control-v2:~w] BARE gate=~w purity=~w  m1=~w m2=~w m3=~w m4=~w m5=~w (expect all 1, purity=unknown post-C-LATENT)~n',
        [Leg, Gate, P, B1, B2, B3, B4, B5]),
    ( (B1+B2+B3+B4+B5) =:= 5, P == unknown
    -> format(user_error, '[control-v2:~w] POSITIVE CONTROL OK — all 5 branches fire, purity=unknown~n', [Leg])
    ;  format(user_error, '[control-v2:~w] *** CONTROL INCOMPLETE — branches ~w/~w/~w/~w/~w purity=~w~n', [Leg, B1,B2,B3,B4,B5, P]) ),
    retractall(constraint_indexing:constraint_classification(C, _, _)).
