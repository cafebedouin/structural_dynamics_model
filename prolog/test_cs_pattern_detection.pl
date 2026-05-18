% ============================================================================
% TEST: cs_pattern_detection.pl
% ============================================================================
% Run from prolog/ directory:
%   swipl -l stack.pl -l test_cs_pattern_detection.pl -g "run_cs_tests, halt." -t "halt(1)"
% ============================================================================

:- use_module(cs_pattern_detection).
:- use_module(narrative_ontology).
:- use_module(domain_priors).

/* ================================================================
   TEST HARNESS
   ================================================================ */

:- dynamic cs_test_result/3.  % cs_test_result(Name, pass|fail, Reason)

run_cs_tests :-
    format("~n=== CS PATTERN DETECTION TESTS ===~n", []),
    forall(cs_test_case(Name, Goal, Expected),
           run_cs_test(Name, Goal, Expected)),
    aggregate_all(count, cs_test_result(_, pass, _), Passed),
    aggregate_all(count, cs_test_result(_, fail, _), Failed),
    Total is Passed + Failed,
    format("~n--- Results: ~w/~w passed ---~n", [Passed, Total]),
    (   Failed > 0
    ->  format("FAILED TESTS:~n"),
        forall(cs_test_result(Name, fail, Reason),
               format("  [FAIL] ~w: ~w~n", [Name, Reason]))
    ;   true
    ).

run_cs_test(Name, Goal, Expected) :-
    (   catch(call(Goal), Err, (format(atom(ErrAtom), "~w", [Err]), fail))
    ->  Got = success
    ;   Got = fail
    ),
    (   Got == Expected
    ->  assert(cs_test_result(Name, pass, "")),
        format("  [PASS] ~w~n", [Name])
    ;   (var(Err) -> Reason = "unexpected_result" ; Reason = ErrAtom),
        assert(cs_test_result(Name, fail, Reason)),
        format("  [FAIL] ~w (expected ~w, got ~w)~n", [Name, Expected, Got])
    ).

/* ================================================================
   SYNTHETIC TEST FACTS
   ================================================================ */

% Test constraint: marked_revision (formalized + expertise)
:- multifile narrative_ontology:cs_kernel_codification/2.
:- multifile narrative_ontology:cs_authority_grounding/2.
:- multifile narrative_ontology:cs_interpretation_layer_present/1.
:- multifile narrative_ontology:constraint_metric/3.
:- multifile narrative_ontology:coordination_type/2.
:- multifile narrative_ontology:has_sunset_clause/1.
:- multifile domain_priors:requires_active_enforcement/1.
:- multifile domain_priors:emerges_naturally/1.

narrative_ontology:cs_kernel_codification(test_marked_revision, formalized).
narrative_ontology:cs_authority_grounding(test_marked_revision, expertise).
narrative_ontology:constraint_metric(test_marked_revision, suppression_requirement, 0.20).
narrative_ontology:constraint_metric(test_marked_revision, theater_ratio, 0.25).
narrative_ontology:has_sunset_clause(test_marked_revision).

% Test constraint: marked_revision with verdict (high suppression)
narrative_ontology:cs_kernel_codification(test_false_marked_revision, formalized).
narrative_ontology:cs_authority_grounding(test_false_marked_revision, expertise).
narrative_ontology:constraint_metric(test_false_marked_revision, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(test_false_marked_revision, theater_ratio, 0.30).
domain_priors:requires_active_enforcement(test_false_marked_revision).

% Test constraint: interpretive_accretion (fixed_text + lineage)
narrative_ontology:cs_kernel_codification(test_interp_accretion, fixed_text).
narrative_ontology:cs_authority_grounding(test_interp_accretion, lineage).
narrative_ontology:constraint_metric(test_interp_accretion, theater_ratio, 0.65).
narrative_ontology:constraint_metric(test_interp_accretion, suppression_requirement, 0.55).
narrative_ontology:coordination_type(test_interp_accretion, identity_coordination).

% Test constraint: interpretive_accretion with verdict (enforcement_mechanism)
narrative_ontology:cs_kernel_codification(test_false_interp_accretion, fixed_text).
narrative_ontology:cs_authority_grounding(test_false_interp_accretion, lineage).
narrative_ontology:coordination_type(test_false_interp_accretion, enforcement_mechanism).
narrative_ontology:constraint_metric(test_false_interp_accretion, theater_ratio, 0.65).
narrative_ontology:constraint_metric(test_false_interp_accretion, suppression_requirement, 0.55).

% Test constraint: anchored_fixity_with_accretion (formalized + extraction + interp layer)
narrative_ontology:cs_kernel_codification(test_af_accretion, formalized).
narrative_ontology:cs_authority_grounding(test_af_accretion, extraction).
narrative_ontology:cs_interpretation_layer_present(test_af_accretion).
narrative_ontology:constraint_metric(test_af_accretion, suppression_requirement, 0.48).
narrative_ontology:coordination_type(test_af_accretion, identity_coordination).

% Test constraint: anchored_fixity_brittle (formalized + extraction, no interp layer)
narrative_ontology:cs_kernel_codification(test_af_brittle, formalized).
narrative_ontology:cs_authority_grounding(test_af_brittle, extraction).
narrative_ontology:constraint_metric(test_af_brittle, suppression_requirement, 0.75).
narrative_ontology:coordination_type(test_af_brittle, enforcement_mechanism).
domain_priors:requires_active_enforcement(test_af_brittle).

% Test constraint: false_anchored_fixity_brittle (identity_coordination + low suppression)
narrative_ontology:cs_kernel_codification(test_false_af_brittle, formalized).
narrative_ontology:cs_authority_grounding(test_false_af_brittle, extraction).
narrative_ontology:constraint_metric(test_false_af_brittle, suppression_requirement, 0.35).
narrative_ontology:coordination_type(test_false_af_brittle, identity_coordination).

% Test constraint: diffuse_reconstruction (distributed + distributed)
narrative_ontology:cs_kernel_codification(test_diffuse, distributed).
narrative_ontology:cs_authority_grounding(test_diffuse, distributed).
narrative_ontology:constraint_metric(test_diffuse, suppression_requirement, 0.30).

% Test constraint: implicit_practice (implicit + practice)
narrative_ontology:cs_kernel_codification(test_implicit, implicit).
narrative_ontology:cs_authority_grounding(test_implicit, practice).
narrative_ontology:constraint_metric(test_implicit, theater_ratio, 0.20).
narrative_ontology:constraint_metric(test_implicit, suppression_requirement, 0.15).

% Test constraint: implicit_practice with verdict (emerges_naturally)
narrative_ontology:cs_kernel_codification(test_false_implicit, implicit).
narrative_ontology:cs_authority_grounding(test_false_implicit, practice).
domain_priors:emerges_naturally(test_false_implicit).

% Test constraint: anomalous combination (formalized + lineage)
narrative_ontology:cs_kernel_codification(test_anomalous, formalized).
narrative_ontology:cs_authority_grounding(test_anomalous, lineage).

% test_no_fields: no CS facts (should → cs_fields_absent)

/* ================================================================
   TEST CASES
   ================================================================ */

cs_test_case("no_fields_absent",
    (cs_pattern(test_no_fields, no_pattern_match, [cs_fields_absent])),
    success).

cs_test_case("has_fields_true",
    cs_has_fields(test_marked_revision),
    success).

cs_test_case("has_fields_false",
    cs_has_fields(test_no_fields),
    fail).

cs_test_case("marked_revision_pattern",
    (cs_pattern(test_marked_revision, marked_revision, _)),
    success).

cs_test_case("marked_revision_no_verdict",
    (cs_verdict(test_marked_revision, _)),
    fail).

cs_test_case("false_marked_revision_verdict",
    (cs_verdict(test_false_marked_revision, false_marked_revision)),
    success).

cs_test_case("interpretive_accretion_pattern",
    (cs_pattern(test_interp_accretion, interpretive_accretion, _)),
    success).

cs_test_case("interpretive_accretion_no_verdict",
    (cs_verdict(test_interp_accretion, _)),
    fail).

cs_test_case("false_interpretive_accretion_verdict",
    (cs_verdict(test_false_interp_accretion, false_interpretive_accretion)),
    success).

cs_test_case("anchored_fixity_with_accretion_pattern",
    (cs_pattern(test_af_accretion, anchored_fixity_with_accretion, _)),
    success).

cs_test_case("anchored_fixity_with_accretion_no_verdict",
    (cs_verdict(test_af_accretion, _)),
    fail).

cs_test_case("anchored_fixity_brittle_pattern",
    (cs_pattern(test_af_brittle, anchored_fixity_brittle, _)),
    success).

cs_test_case("anchored_fixity_brittle_no_verdict",
    (cs_verdict(test_af_brittle, _)),
    fail).

cs_test_case("false_anchored_fixity_brittle_verdict",
    (cs_verdict(test_false_af_brittle, false_anchored_fixity_brittle)),
    success).

cs_test_case("diffuse_reconstruction_pattern",
    (cs_pattern(test_diffuse, diffuse_reconstruction, _)),
    success).

cs_test_case("implicit_practice_pattern",
    (cs_pattern(test_implicit, implicit_practice, _)),
    success).

cs_test_case("implicit_practice_no_verdict",
    (cs_verdict(test_implicit, _)),
    fail).

cs_test_case("false_implicit_practice_verdict",
    (cs_verdict(test_false_implicit, false_implicit_practice)),
    success).

cs_test_case("anomalous_no_pattern_match",
    (cs_pattern(test_anomalous, no_pattern_match, _)),
    success).

cs_test_case("signals_contain_kernel_formalized",
    (cs_pattern(test_marked_revision, _, Sigs), member(kernel_formalized, Sigs)),
    success).

cs_test_case("signals_contain_interp_layer_present",
    (cs_pattern(test_af_accretion, _, Sigs), member(interp_layer_present, Sigs)),
    success).

cs_test_case("signals_contain_interp_layer_absent",
    (cs_pattern(test_af_brittle, _, Sigs), member(interp_layer_absent, Sigs)),
    success).
