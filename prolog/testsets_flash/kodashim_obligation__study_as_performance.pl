% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_performance, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kodashim_obligation__study_as_performance
 *   human_readable: Kodashim Obligation: Study as Sacrificial Performance
 *   domain: religious_studies/jewish_law/textual_preservation
 *
 * SUMMARY:
 *   This constraint represents the reading of Kodashim (the order of
 *   sacrificial law in the Mishnah and Talmud) that posits the study of these
 *   laws as a direct, spiritually efficacious substitute for the physical
 *   performance of sacrifices. In this reading, the Temple's physical absence
 *   is irrelevant to the law's spiritual efficacy, as study itself
 *   constitutes the cosmic function of sacrifice. This is a Mountain from
 *   every seat, as it is understood as a divinely ordained, unchangeable
 *   truth within its theological framework, with no extraction from
 *   participants.
 *
 * KEY AGENTS:
 *   - israelite_community: Primary beneficiary (organized/identity_locked) — receives spiritual merit
 *   - talmudic_sages: Agenda setter (institutional/identity_locked) — established and perpetuated the doctrine
 *   - cosmic_order: Ultimate beneficiary (universal/analytical) — receives spiritual efficacy
 *   - secular_historians: Analytical observer (analytical/analytical) — study its cultural impact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_performance, 0.0).
domain_priors:suppression_score(kodashim_obligation__study_as_performance, 0.0).
domain_priors:theater_ratio(kodashim_obligation__study_as_performance, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, extractiveness, 0.0).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_performance, mountain).
narrative_ontology:human_readable(kodashim_obligation__study_as_performance, "Kodashim Obligation: Study as Sacrificial Performance").
narrative_ontology:topic_domain(kodashim_obligation__study_as_performance, "religious_studies/jewish_law/textual_preservation").

domain_priors:emerges_naturally(kodashim_obligation__study_as_performance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_performance, '2b961f44-8582-4344-9ffb-94fa4596c146').
narrative_ontology:cs_kernel_codification('2b961f44-8582-4344-9ffb-94fa4596c146', fixed_text).
narrative_ontology:cs_authority_grounding('2b961f44-8582-4344-9ffb-94fa4596c146', lineage).
narrative_ontology:cs_interpretation_layer_present('2b961f44-8582-4344-9ffb-94fa4596c146').
narrative_ontology:cs_reading_relation('2b961f44-8582-4344-9ffb-94fa4596c146', kodashim_obligation__study_as_archive, forecloses).
narrative_ontology:cs_reading_relation('2b961f44-8582-4344-9ffb-94fa4596c146', kodashim_obligation__study_as_preparation, coexists_with).
narrative_ontology:cs_axiom('2b961f44-8582-4344-9ffb-94fa4596c146', foundational, study_is_performance).
narrative_ontology:cs_axiom_status(study_is_performance, holdable).
narrative_ontology:cs_axiom_grounding('2b961f44-8582-4344-9ffb-94fa4596c146', study_is_performance, deontological).
narrative_ontology:cs_axiom('2b961f44-8582-4344-9ffb-94fa4596c146', foundational, temple_absence_irrelevant_to_spiritual_efficacy).
narrative_ontology:cs_axiom_status(temple_absence_irrelevant_to_spiritual_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('2b961f44-8582-4344-9ffb-94fa4596c146', temple_absence_irrelevant_to_spiritual_efficacy, theological).
narrative_ontology:cs_reference_frame('2b961f44-8582-4344-9ffb-94fa4596c146', rabbinic_post_temple_halakha).
narrative_ontology:cs_drift_state('2b961f44-8582-4344-9ffb-94fa4596c146', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2b961f44-8582-4344-9ffb-94fa4596c146', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_performance, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_performance, cosmic_order).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_performance, israelite_community).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_performance, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(kodashim_obligation__study_as_performance, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_performance_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, ExtMetricName, E),
    domain_priors:suppression_score(kodashim_obligation__study_as_performance, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(kodashim_obligation__study_as_performance),
    narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(kodashim_obligation__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Mountain because, within this reading, the spiritual efficacy of study is considered an inherent, unchangeable truth of the divine covenant, not a human construct. Extractiveness is zero because study is understood as a pure spiritual gain, not a cost or transfer. Suppression is zero as participation is voluntary and spiritually rewarding. Theater ratio is zero as the practice is considered fully functional. Accessibility collapse is high (0.95) because, within this framework, there are no 'alternatives' to fulfilling this divine obligation other than study itself; resistance is low (0.05) because the doctrine is widely accepted within the tradition.
 *
 * PERSPECTIVAL GAP:
 *   There is no significant perspectival gap in this reading, as all participants who accept its premise experience it as a beneficial, non-extractive spiritual practice. The 'agenda setters' (Talmudic Sages) are seen as transmitters of divine truth, not as extracting from the community. The 'cosmic order' is a non-agent beneficiary.
 *
 * DIRECTIONALITY LOGIC:
 *   The Israelite community and the cosmic order are full beneficiaries (d=0.0) as they receive spiritual merit and cosmic balance, respectively. The Talmudic Sages, as agenda setters, are also beneficiaries in that their authority is vindicated by the efficacy of the practice, but they do not extract from it. There are no victims in this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a deeply held spiritual practice as an extractive constraint. The 'mandate' (to maintain covenantal relationship) is resolved by the 'trophy' (study as performance), meaning the constraint's function is fully met by its current operation, despite the physical absence of the Temple. There is no mandatrophy in this reading because the spiritual efficacy is fully realized through study.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_theological_construct,
    'Is the spiritual efficacy of study a genuine natural law (a structural feature of reality), or a theological construct developed to cope with the Temple''s destruction?',
    'This is a conceptual omega, resolvable only through theological or philosophical commitment, not empirical data. Its resolution depends on the epistemic framework adopted.',
    'If a theological construct, the constraint would be reclassified from Mountain to a Rope or Tangled Rope, depending on whether any subtle extraction or coordination costs are identified. If a natural law, the Mountain classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_theological_construct, conceptual, 'Ambiguity between inherent spiritual law and post-destruction theological adaptation.').

omega_variable(
    beneficiary_of_cosmic_order,
    'Is ''cosmic_order'' a legitimate beneficiary, or is its inclusion a rhetorical device to naturalize a human-made spiritual solution?',
    'Resolution depends on the acceptance of the theological framework that posits such a beneficiary. From a secular analytical perspective, it would not be considered a beneficiary.',
    'If ''cosmic_order'' is not considered a legitimate beneficiary, the constraint''s beneficiary structure would be narrower, focusing solely on the Israelite community. This would not change the Mountain classification but would alter the interpretation of its scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_of_cosmic_order, conceptual, 'Legitimacy of ''cosmic_order'' as a beneficiary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_performance, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_obligation__study_as_performance, theater_ratio, 0, 0.0).
narrative_ontology:measurement(koda_tr_t500, kodashim_obligation__study_as_performance, theater_ratio, 500, 0.0).
narrative_ontology:measurement(koda_tr_t1000, kodashim_obligation__study_as_performance, theater_ratio, 1000, 0.0).
narrative_ontology:measurement(koda_tr_t1500, kodashim_obligation__study_as_performance, theater_ratio, 1500, 0.0).
narrative_ontology:measurement(koda_tr_t2000, kodashim_obligation__study_as_performance, theater_ratio, 2000, 0.0).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_obligation__study_as_performance, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(koda_be_t500, kodashim_obligation__study_as_performance, base_extractiveness, 500, 0.0).
narrative_ontology:measurement(koda_be_t1000, kodashim_obligation__study_as_performance, base_extractiveness, 1000, 0.0).
narrative_ontology:measurement(koda_be_t1500, kodashim_obligation__study_as_performance, base_extractiveness, 1500, 0.0).
narrative_ontology:measurement(koda_be_t2000, kodashim_obligation__study_as_performance, base_extractiveness, 2000, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_obligation__study_as_performance, suppression_requirement, 0, 0.0).
narrative_ontology:measurement(koda_su_t500, kodashim_obligation__study_as_performance, suppression_requirement, 500, 0.0).
narrative_ontology:measurement(koda_su_t1000, kodashim_obligation__study_as_performance, suppression_requirement, 1000, 0.0).
narrative_ontology:measurement(koda_su_t1500, kodashim_obligation__study_as_performance, suppression_requirement, 1500, 0.0).
narrative_ontology:measurement(koda_su_t2000, kodashim_obligation__study_as_performance, suppression_requirement, 2000, 0.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
