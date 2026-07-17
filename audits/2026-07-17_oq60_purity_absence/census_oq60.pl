% OQ-60 Phase-0 census — instrument the REAL producer predicates (0a/0b/0c-data/0d).
%
% Per constraint, isolate the five absence branches by driving the real
% predicates' own building blocks (coupling_test_powers/scopes/context,
% classify_at_context, scope_invariance_test, excess_extraction,
% boltzmann_floor_for) — NOT a reconstructed model — and CROSS-VALIDATE each
% derived tag against the real predicate's observable output (anti-reconstruction
% control). Cache cleared at leg start; caches are per-constraint keyed (0e).
%
% Run per leg (serialized, fresh process each):
%   cd prolog && swipl -g "consult('<audit>/census_oq60.pl'), run_leg(testsets, '<audit>/census_testsets.tsv'), halt" -t "halt(1)"
% Archived leg: run_leg('archives/datasets/kernel_v1', '...tsv') overlays corpus_path.

:- use_module(library(lists)).

leg_setup(Leg) :-
    ( Leg == testsets
    ->  true
    ;   retractall(config:param(corpus_path, _)),
        asserta(config:param(corpus_path, Leg))
    ),
    corpus_loader:load_all_testsets,
    cache_registry:clear_all_caches.

% --- coupling/CC grid built from the REAL building blocks (0a) ---
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

% --- floor clause actually taken by boltzmann_floor_for/2 (mechanism 5) ---
floor_clause(C, override) :- narrative_ontology:boltzmann_floor_override(C, _), !.
floor_clause(C, type)     :- narrative_ontology:coordination_type(C, _), !.
floor_clause(_, default).

subscore(Goal, V) :- ( catch(Goal, _, fail) -> true ; V = fail_err ), ( var(V) -> V = fail_err ; true ).

census_line(S, C) :-
    % gate
    ( boltzmann_compliance:epistemic_access_check(C, true) -> Gate = pass ; Gate = fail ),
    % real subscores
    ( catch(purity_scoring:factorization_subscore(C, F), _, F = err) -> true ; F = fail ),
    ( catch(purity_scoring:scope_invariance_subscore(C, SIsub), _, SIsub = err) -> true ; SIsub = fail ),
    ( catch(purity_scoring:coupling_cleanliness_subscore(C, CC), _, CC = err) -> true ; CC = fail ),
    ( catch(purity_scoring:excess_extraction_subscore(C, EX), _, EX = err) -> true ; EX = fail ),
    ( catch(purity_scoring:purity_score(C, P), _, P = err) -> true ; P = fail ),
    % --- mechanism 1: scope_invariance_test variant([]) + raw SI + N ---
    ( catch(boltzmann_compliance:scope_invariance_test(C, SR), _, SR = err) -> true ; SR = fail ),
    ( SR = invariant -> (N = 1, RawSI = 1.0)
    ; SR = variant(Ts), is_list(Ts) -> (length(Ts, N), RawSI is 1.0 - (N - 1) * 0.25)
    ; (N = na, RawSI = na)
    ),
    ( SR = variant([]) -> M1 = 1 ; M1 = 0 ),
    % --- mechanisms 2 & 3: shared coupling/CC grid size ---
    ( catch(grid_size(C, G), _, G = err) -> true ; G = err ),
    ( integer(G), G < 2 -> M2 = 1 ; M2 = 0 ),          % coupling no-data
    ( integer(G), G =:= 0 -> M3 = 1 ; M3 = 0 ),        % CC no-data (empty grid)
    % anti-reconstruction cross-check for M2: G<2 ==> real coupling score 0.0
    ( M2 =:= 1
    ->  ( catch(boltzmann_compliance:cross_index_coupling(C, XS), _, XS = err),
          ( XS == 0.0 -> X2 = ok ; X2 = MISMATCH-XS ) )
    ;   X2 = na ),
    % --- mechanism 4: excess_extraction/2 fails (no eps data) ---
    ( catch(boltzmann_compliance:excess_extraction(C, _), _, fail) -> M4 = 0 ; M4 = 1 ),
    % --- mechanism 5: boltzmann_floor_for clause 3 (default) taken ---
    floor_clause(C, FClause),
    ( FClause == default -> M5 = 1 ; M5 = 0 ),
    ( M5 =:= 1
    ->  ( catch(boltzmann_compliance:boltzmann_floor_for(C, FV), _, FV = err),
          config:param(boltzmann_floor_default, DF),
          ( FV =:= DF -> X5 = ok ; X5 = MISMATCH-FV ) )
    ;   X5 = na ),
    % post-fix disposition (0d): gate-fail -> sentinel; else any mech -> unknown; else scored
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
    format(user_error, '~n[census] leg=~w population=~w~n', [Leg, NPop]),
    open(OutFile, write, S),
    format(S, 'constraint\tgate\tpurity\tF\tSIsub\tCC\tEX\tscope_result\tN\trawSI\tgridG\tm1_si_empty\tm2_grid_lt2\tm3_grid_empty\tm4_ex_fail\tm5_floor_default\tany_mech\tdisposition\tcrosschecks~n', []),
    forall(member(C, IDs), census_line(S, C)),
    close(S),
    summarize(Leg, IDs, OutFile).

% ---- summary + positive control, SAME process (per-process control) ----
summarize(Leg, IDs, OutFile) :-
    count_where(IDs, [C]>>(boltzmann_compliance:epistemic_access_check(C, true)), NGatePass),
    count_where(IDs, [C]>>tag(C, m1), M1n),
    count_where(IDs, [C]>>tag(C, m2), M2n),
    count_where(IDs, [C]>>tag(C, m3), M3n),
    count_where(IDs, [C]>>tag(C, m4), M4n),
    count_where(IDs, [C]>>tag(C, m5), M5n),
    count_where(IDs, [C]>>tag(C, any), AnyN),
    count_where(IDs, [C]>>tag(C, si_gt1_n_ge1), CeilN),
    count_where(IDs, [C]>>tag(C, si_lt0), FloorN),
    length(IDs, Pop),
    format(user_error, '[census:~w] pop=~w gate_pass=~w | m1=~w m2=~w m3=~w m4=~w m5=~w any=~w~n',
        [Leg, Pop, NGatePass, M1n, M2n, M3n, M4n, M5n, AnyN]),
    format(user_error, '[census:~w] 0b ceiling-violation(SI>1 & N>=1)=~w  floor-violation(rawSI<0,N>=6)=~w~n',
        [Leg, CeilN, FloorN]),
    positive_control(Leg),
    format(user_error, '[census:~w] wrote ~w~n', [Leg, OutFile]).

count_where(IDs, Pred, N) :-
    include(Pred, IDs, Sub), length(Sub, N).

tag(C, m1) :- boltzmann_compliance:scope_invariance_test(C, variant([])).
tag(C, m2) :- grid_size(C, G), G < 2.
tag(C, m3) :- grid_size(C, G), G =:= 0.
tag(C, m4) :- \+ catch(boltzmann_compliance:excess_extraction(C, _), _, fail).
tag(C, m5) :- floor_clause(C, Cl), Cl == default.   % NB unbound then ==; floor_clause(C,default) bound-arg bypasses the cut-guards
tag(C, any) :- ( tag(C,m1);tag(C,m2);tag(C,m3);tag(C,m4);tag(C,m5) ), !.
tag(C, si_gt1_n_ge1) :-
    boltzmann_compliance:scope_invariance_test(C, variant(Ts)), is_list(Ts),
    length(Ts, N), N >= 1, RawSI is 1.0 - (N-1)*0.25, RawSI > 1.0.
tag(C, si_lt0) :-
    boltzmann_compliance:scope_invariance_test(C, variant(Ts)), is_list(Ts),
    length(Ts, N), RawSI is 1.0 - (N-1)*0.25, RawSI < 0.0.

% Positive control: a BARE constraint that passes the epistemic gate (3 authored
% classifications) but holds NO grid/coupling/extraction/coordination data.
% MUST fire all five branches in the SAME process that reports the corpus zeros.
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
    format(user_error, '[control:~w] BARE gate=~w purity=~w  m1=~w m2=~w m3=~w m4=~w m5=~w (expect all 1, purity=1.0 pre-fix)~n',
        [Leg, Gate, P, B1, B2, B3, B4, B5]),
    ( (B1+B2+B3+B4+B5) =:= 5 -> format(user_error, '[control:~w] POSITIVE CONTROL OK — all 5 branches fire~n', [Leg])
    ; format(user_error, '[control:~w] *** CONTROL INCOMPLETE — not all branches fired~n', [Leg]) ),
    retractall(constraint_indexing:constraint_classification(C, _, _)).
