% ============================================================================
% cirope_reachability.pl — Q2 row-2 control: can a captured (beneficiary-bearing)
% low-ε NON-emergent constraint reach CI_Rope certification (signature_detection:1019)?
% Run from prolog/:
%   swipl -g run_cirope_reachability -t halt ../audits/2026-06-10_oq94_row2_cirope_reachability/cirope_reachability.pl
% Pre-registration: PREREGISTRATION.md (same dir; committed before the run).
% ============================================================================

:- [stack].
:- use_module(library(lists)).

assert_full_profile(C) :-
    assertz(narrative_ontology:constraint_metric(C, extractiveness, 0.10)),
    assertz(narrative_ontology:constraint_metric(C, suppression_requirement, 0.05)),
    assertz(narrative_ontology:constraint_metric(C, theater_ratio, 0.10)),
    assertz(narrative_ontology:constraint_metric(C, accessibility_collapse, 0.20)),
    assertz(narrative_ontology:constraint_metric(C, resistance, 0.10)).
    % NO emerges_naturally; NO constraint_claim facts.

build_cases :-
    assert_full_profile(cir_cap),
    assertz(narrative_ontology:constraint_beneficiary(cir_cap, capturer_ci)),
    assert_full_profile(cir_ctl).

% --- per-gate component report (so a non-reach names its blocking gate) ---
report_components(C) :-
    ( catch(signature_detection:boltzmann_compliant(C, Comp), E1, (Comp = error(E1)))
      -> true ; Comp = 'FAILED' ),
    format("    boltzmann_compliant = ~w~n", [Comp]),
    ( catch(signature_detection:scope_invariance_test(C, Scope), E2, (Scope = error(E2)))
      -> true ; Scope = 'FAILED' ),
    format("    scope_invariance    = ~w~n", [Scope]),
    ( narrative_ontology:has_coordination_function(C) -> Coord = true ; Coord = false ),
    format("    coordination_fn     = ~w~n", [Coord]),
    ( catch(signature_detection:excess_extraction(C, Ex), E3, (Ex = error(E3)))
      -> true ; Ex = 'FAILED' ),
    format("    excess_extraction   = ~w~n", [Ex]).

report_case(C) :-
    format("CASE ~w~n", [C]),
    report_components(C),
    ( catch(signature_detection:constraint_signature(C, Sig), E, (Sig = error(E)))
      -> true ; Sig = 'FAILED' ),
    format("  RESULT ~w | constraint_signature = ~w~n~n", [C, Sig]).

run_cirope_reachability :-
    build_cases,
    cache_registry:clear_all_caches,
    nl,
    format("==== Q2 row-2 CI_Rope reachability control ====~n~n", []),
    forall(member(C, [cir_cap, cir_ctl]), report_case(C)),
    format("==== verdict mapping: PREREGISTRATION.md (cir_cap=CI_Rope & cir_ctl/=CI_Rope => Outcome 1 REACHABLE) ====~n", []).
