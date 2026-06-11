% OQ-90 Phase 2 — wired-path control battery (read-only; in-session overlay of the
% Phase-3 clause, BEFORE the output-changing commit). Host: regulatory_measurement_gap,
% the witnessed FCR-reaching diffuse+prohibitive constraint. Four shapes through the
% PRODUCTION path drl_core:dr_type/3:
%   1. as-is (diffuse + prohibitive)        -> expect piton (exact)
%   2. captured-twin (gain_flow -> a seat)  -> expect NOT piton (observed: rope)
%   3. transient-neglect twin (cost->cheap) -> expect NOT piton (observed: tangled_rope)
%   4. absent twin (gain_flow+cost retracted)-> expect NOT piton (fail-closed; observed: tangled_rope)
% HALT if shapes 2-4 promote TO PITON (over-fire) or shape 1 doesn't (under-fire/wiring bug).
% The exact non-piton fall-through value is downstream of how capture/cost reshape
% classification (shape2 lands rope because constraint_captured=true changes the generic-FCR
% fall-through) and is reported, not gated. Killswitch (param=0) -> tangled_rope.
%
% Run from prolog/:
%   swipl -q -g run_battery -t "halt(1)" ../audits/2026-06-11_oq90_piton_refinement/phase2_control_battery.pl

host(regulatory_measurement_gap).

ctx(context(agent_power(analytical), time_horizon(civilizational),
            exit_options(analytical), spatial_scope(universal))).

dt(C, T) :-
    cache_registry:clear_all_caches,
    ctx(Ctx),
    ( drl_core:dr_type(C, Ctx, T0) -> T = T0 ; T = '(no dr_type)' ).

% Exact-value check (shape 1, killswitch).
check(Label, Got, Want) :-
    ( Got == Want
    -> format('  PASS  ~w: dr_type = ~w (expected ~w)~n', [Label, Got, Want])
    ;  format('  *** HALT ~w: dr_type = ~w (expected ~w) ***~n', [Label, Got, Want]),
       nb_setval(oq90_halt, true) ).

% The genuine HALT criterion for shapes 2-4 (plan): "promote to piton" = over-fire.
% The exact non-piton value is downstream of how capture/cost reshape classification
% and is reported, not gated (shape2 lands rope, not tangled_rope — see witness notes).
check_not_piton(Label, Got) :-
    ( Got \== piton
    -> format('  PASS  ~w: dr_type = ~w (NOT piton — no over-fire)~n', [Label, Got])
    ;  format('  *** HALT ~w: dr_type = piton (OVER-FIRE) ***~n', [Label]),
       nb_setval(oq90_halt, true) ).

tf(G, true)  :- \+ \+ G, !.
tf(_, false).

run_battery :-
    use_module(stack),
    use_module(probe_harness),
    corpus_loader:ensure_corpus_loaded,
    nb_setval(oq90_halt, false),
    host(C),

    % Phase-3 clause + param are now compiled in (config.pl default piton_refinement_enabled=1);
    % the battery runs through the genuine production path, not an in-session overlay.
    ( config:param(piton_refinement_enabled, V) -> true ; V = '(unset)' ),
    format('~n=== OQ-90 Phase 2 control battery (host ~w, piton_refinement_enabled=~w) ===~n', [C, V]),
    tf(narrative_ontology:piton_candidate(C), PC),
    tf(narrative_ontology:constraint_captured(C), CAP),
    ( signature_detection:constraint_signature(C,S) -> Sig=S ; Sig=none ),
    format('Pre-checks: piton_candidate=~w  captured=~w  signature=~w~n', [PC, CAP, Sig]),

    % Shape 1 — as-is
    dt(C, T1), check('shape1 as-is', T1, piton),

    % Shape 2 — captured twin (overlay gain_flow to an EXISTING seat)
    probe_harness:with_overlay(
        [narrative_ontology:stakeholder_gain_flow(C, _)],
        [narrative_ontology:stakeholder_gain_flow(C, fda_regulatory_authority)],
        ( dt(C, T2v),
          tf(narrative_ontology:piton_candidate(C), P2), tf(narrative_ontology:constraint_captured(C), C2),
          format('  [shape2] piton_candidate now=~w captured now=~w~n', [P2, C2]) )),
    check_not_piton('shape2 captured-twin', T2v),

    % Shape 3 — transient-neglect twin (overlay fixing_cost prohibitive -> cheap)
    probe_harness:with_overlay(
        [narrative_ontology:fixing_cost_class(C, _)],
        [narrative_ontology:fixing_cost_class(C, cheap)],
        ( dt(C, T3v),
          tf(narrative_ontology:piton_candidate(C), P3), tf(narrative_ontology:transient_neglect(C), TN3),
          format('  [shape3] piton_candidate now=~w transient_neglect now=~w~n', [P3, TN3]) )),
    check_not_piton('shape3 transient-neglect twin', T3v),

    % Shape 4 — absent twin (retract gain_flow + fixing_cost)
    probe_harness:with_retracted(
        [narrative_ontology:stakeholder_gain_flow(C, _),
         narrative_ontology:fixing_cost_class(C, _)],
        ( dt(C, T4v),
          tf(narrative_ontology:piton_candidate(C), P4), tf(narrative_ontology:uncaptured(C), U4),
          format('  [shape4] piton_candidate now=~w uncaptured now=~w~n', [P4, U4]) )),
    check_not_piton('shape4 absent-twin', T4v),

    % Kill-switch control: flip the param to 0, host must fall back to tangled_rope.
    retract(config:param(piton_refinement_enabled, 1)),
    assertz(config:param(piton_refinement_enabled, 0)),
    dt(C, TOff),
    check('killswitch param=0', TOff, tangled_rope),
    retract(config:param(piton_refinement_enabled, 0)),
    assertz(config:param(piton_refinement_enabled, 1)),

    ( nb_getval(oq90_halt, true)
    -> format('~n*** BATTERY HALT — at least one shape violated expectation ***~n', []), halt(1)
    ;  format('~n=== BATTERY PASS — shape1 fires piton; shapes 2-4 fall through ===~n', []), halt(0) ).
