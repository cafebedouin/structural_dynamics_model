% Pass B(i)+(ii): synthetic no-data constraint — which branch does each subscore take,
% what fixed point survives, and does the census flag it (positive control).
% Also EX direction probe (B-iii partial) using the same constraint.

:- consult('/tmp/purity_audit/census.pl').

synth_id(purity_audit_synth_nodata).

setup_synth :-
    synth_id(C),
    Ctx1 = context(agent_power(powerless), time_horizon(generational), exit_options(trapped), spatial_scope(national)),
    Ctx2 = context(agent_power(moderate), time_horizon(generational), exit_options(mobile), spatial_scope(national)),
    Ctx3 = context(agent_power(analytical), time_horizon(civilizational), exit_options(analytical), spatial_scope(global)),
    assertz(constraint_indexing:constraint_classification(C, snare, Ctx1)),
    assertz(constraint_indexing:constraint_classification(C, snare, Ctx2)),
    assertz(constraint_indexing:constraint_classification(C, snare, Ctx3)).

probe_branches :-
    synth_id(C),
    boltzmann_compliance:epistemic_access_check(C, Acc),
    format(user_error, 'synth access=~w~n', [Acc]),
    ( catch(purity_scoring:factorization_subscore(C, F), E1, (F = error(E1))) -> true ; F = 'FAIL' ),
    ( catch(purity_scoring:scope_invariance_subscore(C, SI), E2, (SI = error(E2))) -> true ; SI = 'FAIL' ),
    ( catch(purity_scoring:coupling_cleanliness_subscore(C, CC), E3, (CC = error(E3))) -> true ; CC = 'FAIL' ),
    ( catch(purity_scoring:excess_extraction_subscore(C, EX), E4, (EX = error(E4))) -> true ; EX = 'FAIL' ),
    format(user_error, 'synth subscores F=~w SI=~w CC=~w EX=~w~n', [F, SI, CC, EX]),
    ( catch(purity_scoring:purity_score(C, P), E5, (P = error(E5))) -> true ; P = 'FAIL' ),
    format(user_error, 'synth purity_score=~w~n', [P]),
    ( number(P), P >= 0.0 -> logical_fingerprint:purity_zone(P, Z) ; Z = na ),
    format(user_error, 'synth zone=~w~n', [Z]),
    ( catch(signature_detection:structural_purity(C, Cls), E6, (Cls = error(E6))) -> true ; Cls = 'FAIL' ),
    format(user_error, 'synth structural_purity=~w~n', [Cls]).

census_control :-
    synth_id(C),
    open('/tmp/purity_audit/census_control.tsv', write, S),
    census_line(S, C),
    close(S),
    format(user_error, 'census_control written~n', []).

% EX direction probe: add extractiveness metric (no coordination_type -> default floor path)
ex_direction :-
    synth_id(C),
    config:param(boltzmann_floor_default, DF),
    format(user_error, 'boltzmann_floor_default=~w~n', [DF]),
    assertz(narrative_ontology:constraint_metric(C, extractiveness, 0.9)),
    purity_scoring:excess_extraction_subscore(C, EXhi),
    format(user_error, 'EX at eps=0.9 (default floor): ~w~n', [EXhi]),
    retract(narrative_ontology:constraint_metric(C, extractiveness, 0.9)),
    assertz(narrative_ontology:constraint_metric(C, extractiveness, DF)),
    purity_scoring:excess_extraction_subscore(C, EXlo),
    format(user_error, 'EX at eps=floor: ~w~n', [EXlo]),
    retract(narrative_ontology:constraint_metric(C, extractiveness, DF)),
    ( EXhi < EXlo -> format(user_error, 'EX direction OK (more excess -> lower subscore)~n', [])
    ; format(user_error, 'EX DIRECTION VIOLATION~n', []) ).

run_probe :- setup_synth, probe_branches, census_control, ex_direction.
