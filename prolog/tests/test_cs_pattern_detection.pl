% ============================================================================
% TEST: cs_pattern_detection.pl
% ============================================================================
% Run from prolog/ directory:
%   swipl -l stack.pl -l test_cs_pattern_detection.pl -g "run_cs_tests, halt." -t "halt(1)"
% ============================================================================

:- use_module(cs_pattern_detection).
:- use_module(narrative_ontology).
:- use_module(domain_priors).

:- discontiguous cs_test_case/3.

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
:- multifile narrative_ontology:constraint_beneficiary/2.
:- multifile narrative_ontology:constraint_victim/2.
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

% test_natural_law (self_enforcing authority — should emit natural_law_constraint)
narrative_ontology:cs_kernel_codification(test_natural_law, distributed).
narrative_ontology:cs_authority_grounding(test_natural_law, self_enforcing).

% test_epistemic_consensus (diffuse_epistemic authority — should emit epistemic_consensus)
narrative_ontology:cs_kernel_codification(test_epistemic_consensus, distributed).
narrative_ontology:cs_authority_grounding(test_epistemic_consensus, diffuse_epistemic).

% test_naturalized_mountain (self_enforcing authority, low ε, both victims and beneficiaries —
% should emit false_natural_law_constraint verdict)
narrative_ontology:cs_kernel_codification(test_naturalized_mountain, distributed).
narrative_ontology:cs_authority_grounding(test_naturalized_mountain, self_enforcing).
narrative_ontology:constraint_metric(test_naturalized_mountain, extractiveness, 0.08).
narrative_ontology:constraint_beneficiary(test_naturalized_mountain, admin_structures).
narrative_ontology:constraint_victim(test_naturalized_mountain, workforce).

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

cs_test_case("natural_law_pattern",
    (cs_pattern(test_natural_law, natural_law_constraint, _)),
    success).

cs_test_case("epistemic_consensus_pattern",
    (cs_pattern(test_epistemic_consensus, epistemic_consensus, _)),
    success).

cs_test_case("false_natural_law_verdict",
    (cs_verdict(test_naturalized_mountain, false_natural_law_constraint)),
    success).

/* ================================================================
   SYNTHETIC FACTS: structural diagnostic tests (Phase 1)

   test_masking_case:     expertise authority + high-extraction metrics
                          → cs_authority_masking should fire
   test_cover_story_case: extraction authority + interp layer + high suppression
                          → cs_cover_story_active should fire
   test_no_masking_case:  extraction authority + high-extraction metrics
                          → cs_authority_masking should NOT fire (agreement)
   test_displaced_path:   lineage authority + affects_constraint to extraction sibling
                          → cs_displaced_beneficiary should fire
   test_displaced_extraction: extraction authority (sibling for above)
   ================================================================ */

% test_masking_case: claims expertise but signature will be constructed_high_extraction
narrative_ontology:cs_kernel_codification(test_masking_case, formalized).
narrative_ontology:cs_authority_grounding(test_masking_case, expertise).
narrative_ontology:constraint_metric(test_masking_case, extractiveness, 0.60).
narrative_ontology:constraint_metric(test_masking_case, suppression_requirement, 0.55).

% test_cover_story_case: extraction authority + interp layer
%   → pattern = anchored_fixity_with_accretion
%   → very high suppression fires false_anchored_fixity_accretion
%   → signature = constructed_high_extraction
narrative_ontology:cs_kernel_codification(test_cover_story_case, formalized).
narrative_ontology:cs_authority_grounding(test_cover_story_case, extraction).
narrative_ontology:cs_interpretation_layer_present(test_cover_story_case).
narrative_ontology:constraint_metric(test_cover_story_case, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(test_cover_story_case, extractiveness, 0.60).

% test_no_masking_case: extraction authority + extraction signature → no masking (agreement)
narrative_ontology:cs_kernel_codification(test_no_masking_case, formalized).
narrative_ontology:cs_authority_grounding(test_no_masking_case, extraction).
narrative_ontology:constraint_metric(test_no_masking_case, extractiveness, 0.60).
narrative_ontology:constraint_metric(test_no_masking_case, suppression_requirement, 0.55).

% test_displaced_path: lineage authority → forecloses edge → extraction sibling
% cs_reading_relation/3 typed edge is required; bare affects_constraint alone does not fire.
narrative_ontology:cs_kernel_codification(test_displaced_path, fixed_text).
narrative_ontology:cs_authority_grounding(test_displaced_path, lineage).
narrative_ontology:constraint_metric(test_displaced_path, extractiveness, 0.50).
narrative_ontology:constraint_metric(test_displaced_path, suppression_requirement, 0.45).
narrative_ontology:affects_constraint(test_displaced_path, test_displaced_extraction).
narrative_ontology:cs_reading_relation(test_displaced_path, test_displaced_extraction, forecloses).

% test_displaced_extraction: extraction authority (sibling with CS fields)
narrative_ontology:cs_kernel_codification(test_displaced_extraction, formalized).
narrative_ontology:cs_authority_grounding(test_displaced_extraction, extraction).

/* ================================================================
   TEST CASES: structural diagnostics
   ================================================================ */

cs_test_case("masking_fires_on_mismatch",
    cs_authority_masking(test_masking_case, _, expertise),
    success).

cs_test_case("masking_sig_is_extraction_indicating",
    (cs_authority_masking(test_masking_case, Sig, _),
     member(Sig, [false_natural_law, false_ci_rope, false_summit_mountain,
                  constructed_high_extraction])),
    success).

cs_test_case("masking_silent_on_agreement",
    cs_authority_masking(test_no_masking_case, _, _),
    fail).

cs_test_case("cover_story_fires_triple_corroboration",
    cs_cover_story_active(test_cover_story_case, false_anchored_fixity_accretion),
    success).

cs_test_case("cover_story_silent_on_non_extraction_authority",
    cs_cover_story_active(test_masking_case, _),
    fail).

cs_test_case("displaced_beneficiary_fires_on_extraction_sibling",
    cs_displaced_beneficiary(test_displaced_path),
    success).

cs_test_case("displaced_beneficiary_silent_on_extraction_authority",
    cs_displaced_beneficiary(test_displaced_extraction),
    fail).

/* ================================================================
   SYNTHETIC FACTS: cs_grounding_mismatch/3 tests

   test_gm_reverse_natural:
     extraction AG + natural_law signature (new reverse direction).
     emerges_naturally asserted; no beneficiaries; NL profile metrics.
     Signature path: emerges_naturally + NL profile → natural_law.

   test_gm_reverse_coord:
     extraction AG + coordination_scaffold signature (user-required new direction).
     No emerges_naturally (blocks natural_law path). HasAlternatives=true via
     synthetic intervention fact + intent_viable_alternative.
     Signature path: profile-based → coordination_scaffold.

   test_gm_self_coord:
     self_enforcing AG + coordination_scaffold signature (new vs shipped masking).
     Same coordination_scaffold setup as test_gm_reverse_coord.

   test_gm_consistent_extraction:
     extraction AG + constructed_high_extraction → cs_grounding_contradiction
     has no clause for this pair → silent.

   test_gm_consistent_lineage:
     lineage AG + constructed_constraint (extractiveness in 0.35-0.46 band) →
     constructed_constraint is not in cs_extraction_signature → silent.
   ================================================================ */

:- multifile narrative_ontology:affects_constraint/2.
:- multifile narrative_ontology:cs_reading_relation/3.
:- multifile narrative_ontology:intent_viable_alternative/3.

% test_gm_reverse_natural: extraction AG + natural_law signature
narrative_ontology:cs_kernel_codification(test_gm_reverse_natural, formalized).
narrative_ontology:cs_authority_grounding(test_gm_reverse_natural, extraction).
narrative_ontology:constraint_metric(test_gm_reverse_natural, extractiveness, 0.05).
narrative_ontology:constraint_metric(test_gm_reverse_natural, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(test_gm_reverse_natural, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(test_gm_reverse_natural, resistance, 0.03).
domain_priors:emerges_naturally(test_gm_reverse_natural).
% No beneficiaries → false_summit_mountain fails → natural_law fires

% test_gm_reverse_coord: extraction AG + coordination_scaffold signature
narrative_ontology:cs_kernel_codification(test_gm_reverse_coord, formalized).
narrative_ontology:cs_authority_grounding(test_gm_reverse_coord, extraction).
narrative_ontology:constraint_metric(test_gm_reverse_coord, extractiveness, 0.05).
narrative_ontology:constraint_metric(test_gm_reverse_coord, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(test_gm_reverse_coord, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(test_gm_reverse_coord, resistance, 0.03).
% HasAlternatives=true via synthetic intervention:
narrative_ontology:affects_constraint(test_gm_coord_intervention, test_gm_reverse_coord).
narrative_ontology:intent_viable_alternative(test_gm_coord_intervention, coord_alt, 0.8).
% No emerges_naturally → natural_law path blocked → profile-based → coordination_scaffold

% test_gm_self_coord: self_enforcing AG + coordination_scaffold signature
narrative_ontology:cs_kernel_codification(test_gm_self_coord, distributed).
narrative_ontology:cs_authority_grounding(test_gm_self_coord, self_enforcing).
narrative_ontology:constraint_metric(test_gm_self_coord, extractiveness, 0.05).
narrative_ontology:constraint_metric(test_gm_self_coord, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(test_gm_self_coord, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(test_gm_self_coord, resistance, 0.03).
narrative_ontology:affects_constraint(test_gm_self_intervention, test_gm_self_coord).
narrative_ontology:intent_viable_alternative(test_gm_self_intervention, self_alt, 0.8).

% test_gm_consistent_extraction: extraction + constructed_high_extraction → silent
narrative_ontology:cs_kernel_codification(test_gm_consistent_extraction, formalized).
narrative_ontology:cs_authority_grounding(test_gm_consistent_extraction, extraction).
narrative_ontology:constraint_metric(test_gm_consistent_extraction, extractiveness, 0.60).
narrative_ontology:constraint_metric(test_gm_consistent_extraction, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(test_gm_consistent_extraction, accessibility_collapse, 0.50).
narrative_ontology:constraint_metric(test_gm_consistent_extraction, resistance, 0.50).

% test_gm_consistent_lineage: lineage AG + coordination_scaffold signature → silent.
% Coordination_scaffold is NOT in cs_extraction_signature, so the masking-direction
% clause cs_grounding_contradiction(lineage, Sig) :- cs_extraction_signature(Sig) fails.
% Profile: high AccessCollapse + HasAlternatives=true → coordination_scaffold via
% profile-based path. Extractiveness=0.05 keeps excess_extraction ≤ fcr_excess_floor
% (max(0, 0.05 - 0.05) = 0) so false_ci_rope is not triggered.
narrative_ontology:cs_kernel_codification(test_gm_consistent_lineage, fixed_text).
narrative_ontology:cs_authority_grounding(test_gm_consistent_lineage, lineage).
narrative_ontology:constraint_metric(test_gm_consistent_lineage, extractiveness, 0.05).
narrative_ontology:constraint_metric(test_gm_consistent_lineage, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(test_gm_consistent_lineage, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(test_gm_consistent_lineage, resistance, 0.03).
% HasAlternatives=true via synthetic intervention:
narrative_ontology:affects_constraint(test_gm_lin_intervention, test_gm_consistent_lineage).
narrative_ontology:intent_viable_alternative(test_gm_lin_intervention, lineage_alt, 0.8).

/* ================================================================
   TEST CASES: cs_grounding_mismatch/3
   ================================================================ */

cs_test_case("gm_reverse_natural_fires",
    cs_grounding_mismatch(test_gm_reverse_natural, extraction, natural_law),
    success).

cs_test_case("gm_reverse_coord_fires",
    cs_grounding_mismatch(test_gm_reverse_coord, extraction, coordination_scaffold),
    success).

cs_test_case("gm_self_coord_fires",
    cs_grounding_mismatch(test_gm_self_coord, self_enforcing, coordination_scaffold),
    success).

cs_test_case("gm_consistent_extraction_silent",
    cs_grounding_mismatch(test_gm_consistent_extraction, _, _),
    fail).

cs_test_case("gm_consistent_lineage_silent",
    cs_grounding_mismatch(test_gm_consistent_lineage, _, _),
    fail).
