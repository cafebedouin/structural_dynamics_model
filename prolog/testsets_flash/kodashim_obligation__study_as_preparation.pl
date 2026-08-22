% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_preparation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_preparation, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kodashim_obligation__study_as_preparation
 *   human_readable: Kodashim Obligation: Study as Preparation for Messianic Restoration
 *   domain: religious_studies/jewish_law/textual_preservation
 *
 * SUMMARY:
 *   This constraint describes the obligation within traditional Jewish law to
 *   study the Kodashim (sacrificial) order, despite the destruction of the
 *   Temple rendering its performance impossible. This reading frames study
 *   not as a substitute for performance, but as an instrumental act of
 *   preparation, preserving the technical knowledge necessary for the
 *   messianic restoration of the Temple and its sacrificial service. It is a
 *   reading of the 'kodashim_obligation' kernel, distinct from
 *   'study_as_performance' or 'study_as_archive'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_preparation, 0.15).
domain_priors:suppression_score(kodashim_obligation__study_as_preparation, 0.05).
domain_priors:theater_ratio(kodashim_obligation__study_as_preparation, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, extractiveness, 0.15).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_preparation, mountain).
narrative_ontology:human_readable(kodashim_obligation__study_as_preparation, "Kodashim Obligation: Study as Preparation for Messianic Restoration").
narrative_ontology:topic_domain(kodashim_obligation__study_as_preparation, "religious_studies/jewish_law/textual_preservation").

domain_priors:emerges_naturally(kodashim_obligation__study_as_preparation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_preparation, '3227faf6-e2d6-4483-bec2-5a7b5e054256').
narrative_ontology:cs_kernel_codification('3227faf6-e2d6-4483-bec2-5a7b5e054256', fixed_text).
narrative_ontology:cs_authority_grounding('3227faf6-e2d6-4483-bec2-5a7b5e054256', lineage).
narrative_ontology:cs_interpretation_layer_present('3227faf6-e2d6-4483-bec2-5a7b5e054256').
narrative_ontology:cs_reading_relation('3227faf6-e2d6-4483-bec2-5a7b5e054256', kodashim_obligation__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('3227faf6-e2d6-4483-bec2-5a7b5e054256', kodashim_obligation__study_as_archive, coexists_with).
narrative_ontology:cs_axiom('3227faf6-e2d6-4483-bec2-5a7b5e054256', foundational, sacrificial_law_eternally_binding).
narrative_ontology:cs_axiom_status(sacrificial_law_eternally_binding, holdable).
narrative_ontology:cs_axiom_grounding('3227faf6-e2d6-4483-bec2-5a7b5e054256', sacrificial_law_eternally_binding, deontological).
narrative_ontology:cs_axiom('3227faf6-e2d6-4483-bec2-5a7b5e054256', foundational, study_is_instrumental_preparation).
narrative_ontology:cs_axiom_status(study_is_instrumental_preparation, holdable).
narrative_ontology:cs_axiom_grounding('3227faf6-e2d6-4483-bec2-5a7b5e054256', study_is_instrumental_preparation, conventional).
narrative_ontology:cs_reference_frame('3227faf6-e2d6-4483-bec2-5a7b5e054256', post_temple_destruction_rabbinic_framework).
narrative_ontology:cs_drift_state('3227faf6-e2d6-4483-bec2-5a7b5e054256', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3227faf6-e2d6-4483-bec2-5a7b5e054256', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_preparation, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, messianic_future_generation).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_preparation, current_generation_of_jews).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bears the deferred cosmic repair, as the sacrificial laws cannot be performed. Their study is an act of faith and preservation, a cost in time and intellectual effort, with the benefit deferred to a future generation. Identity is deeply tied to the continuity of Jewish law.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, current_generation_of_jews, payer,
    powerless, biographical, identity_locked, global).

% Will benefit from the preserved technical knowledge of sacrificial law, enabling its correct performance upon the rebuilding of the Temple. This generation is a conceptual beneficiary, representing the ultimate fulfillment of the religious obligation.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, messianic_future_generation, beneficiary,
    analytical, civilizational, analytical, universal).

% Are the primary interpreters and transmitters of Kodashim, emphasizing its binding nature and the preparatory role of study. They set the agenda for how this unperformable law is engaged with, ensuring its continuity and the technical knowledge required for its future restoration.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, rabbis_and_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% The overarching theological framework that mandates the sacrificial laws and their eventual restoration. It benefits from the continuity of the obligation and the preparatory study, as it affirms its eternal validity.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, divine_covenant, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(kodashim_obligation__study_as_preparation, divine_covenant).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the preservation of complex, unperformable religious law across generations, ensuring the technical knowledge for sacrificial rites is maintained for a future messianic era.
% TRANSFER_FUNCTION: Transfers the obligation of study and the burden of deferred cosmic repair from past generations to the current one, and the technical knowledge of sacrificial law from the current generation to a future messianic generation.
% ABSENT_VOICES: Those who might argue for a purely historical or symbolic reading of Kodashim, detaching it from future performance, are implicitly excluded by the emphasis on preparation and binding obligation. Their voices are present in other readings of the kernel.
% DISAPPEARANCE_RATIONALE: If the obligation to study Kodashim as preparation vanished, a core pillar of Jewish messianic hope and the continuity of religious law would collapse. The technical knowledge for future Temple service would be lost, fundamentally altering the religious landscape and the relationship to divine covenant.
% FOUNDING_PROBLEM: The destruction of the Second Temple rendered the sacrificial laws unperformable, creating a crisis of religious obligation and continuity for a central aspect of Jewish worship.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic tradition and centuries of Jewish legal scholarship attest to the ongoing nature of this problem, with the unperformability of sacrifices remaining a central theological challenge. The problem is acknowledged by all major streams of traditional Judaism, even those with differing views on its resolution.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_preparation, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_preparation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_preparation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(kodashim_obligation__study_as_preparation, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_preparation, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_preparation_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, ExtMetricName, E),
    domain_priors:suppression_score(kodashim_obligation__study_as_preparation, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(kodashim_obligation__study_as_preparation),
    narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(kodashim_obligation__study_as_preparation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as a Mountain due to its perceived divine origin and immutable nature within this reading. Extractiveness is low (0.15) because the 'cost' of study is primarily intellectual and spiritual, freely undertaken as a religious obligation, with the benefit deferred to a future, ideal state. Suppression is negligible (0.05) as participation is voluntary, driven by internal commitment rather than external coercion. Theater ratio is 0.0 as the study is genuinely functional for its stated purpose of preservation and preparation. Accessibility collapse is high (0.9) because for those who accept the premise, there is no alternative to this form of engagement with the unperformable law. Resistance is low (0.02) as the obligation is widely accepted within the tradition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the 'current_generation_of_jews', the constraint is a binding, divinely ordained obligation that requires significant personal investment for a future, uncertain benefit. From the 'messianic_future_generation' (an analytical seat), it is a pure benefit, the successful transmission of vital knowledge. The 'rabbis_and_scholars' see it as a sacred duty of preservation and transmission.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'current_generation_of_jews' are the primary payers, bearing the cost of study and the deferred cosmic repair. The 'messianic_future_generation' is the beneficiary, receiving the preserved knowledge. Rabbis and scholars act as agenda-setters, guiding the interpretation and transmission of this obligation. The 'divine_covenant' is a non-agent beneficiary, representing the vindication of the eternal law.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_vs_human_interpretation,
    'Is the binding nature of Kodashim and the preparatory role of study a direct divine mandate, or a rabbinic interpretation developed in response to historical circumstances?',
    'Theological and historical analysis of early rabbinic texts, examining the evolution of the ''study as preparation'' concept post-Temple destruction.',
    'If primarily a rabbinic interpretation, the ''emerges_naturally'' claim for this constraint would be weakened, potentially reclassifying it from Mountain to a more constructed type (e.g., Rope or Tangled Rope), reflecting human agency in its establishment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_mandate_vs_human_interpretation, conceptual, 'Ambiguity regarding the origin of the ''study as preparation'' obligation.').

omega_variable(
    messianic_restoration_certainty,
    'What is the certainty and imminence of the messianic restoration of the Temple and sacrificial service, and how does this affect the perceived value of preparatory study?',
    'Empirical observation of historical events and theological developments; however, this is ultimately a matter of faith and cannot be empirically resolved.',
    'If the messianic restoration is perceived as highly uncertain or indefinitely deferred, the ''beneficiary'' status of the ''messianic_future_generation'' might be weakened, and the ''extractiveness'' on the ''current_generation_of_jews'' could be perceived as higher, as the benefit becomes more abstract.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(messianic_restoration_certainty, preference, 'Uncertainty regarding the future fulfillment of the constraint''s ultimate purpose.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_preparation, 70, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t70, kodashim_obligation__study_as_preparation, theater_ratio, 70, 0.0).
narrative_ontology:measurement(koda_tr_t500, kodashim_obligation__study_as_preparation, theater_ratio, 500, 0.0).
narrative_ontology:measurement(koda_tr_t1000, kodashim_obligation__study_as_preparation, theater_ratio, 1000, 0.0).
narrative_ontology:measurement(koda_tr_t1500, kodashim_obligation__study_as_preparation, theater_ratio, 1500, 0.0).
narrative_ontology:measurement(koda_tr_t2024, kodashim_obligation__study_as_preparation, theater_ratio, 2024, 0.0).

% Extraction over time
narrative_ontology:measurement(koda_be_t70, kodashim_obligation__study_as_preparation, base_extractiveness, 70, 0.1).
narrative_ontology:measurement(koda_be_t500, kodashim_obligation__study_as_preparation, base_extractiveness, 500, 0.12).
narrative_ontology:measurement(koda_be_t1000, kodashim_obligation__study_as_preparation, base_extractiveness, 1000, 0.13).
narrative_ontology:measurement(koda_be_t1500, kodashim_obligation__study_as_preparation, base_extractiveness, 1500, 0.14).
narrative_ontology:measurement(koda_be_t2024, kodashim_obligation__study_as_preparation, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t70, kodashim_obligation__study_as_preparation, suppression_requirement, 70, 0.05).
narrative_ontology:measurement(koda_su_t500, kodashim_obligation__study_as_preparation, suppression_requirement, 500, 0.05).
narrative_ontology:measurement(koda_su_t1000, kodashim_obligation__study_as_preparation, suppression_requirement, 1000, 0.05).
narrative_ontology:measurement(koda_su_t1500, kodashim_obligation__study_as_preparation, suppression_requirement, 1500, 0.05).
narrative_ontology:measurement(koda_su_t2024, kodashim_obligation__study_as_preparation, suppression_requirement, 2024, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_preparation, identity_coordination).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_preparation, kodashim_obligation__study_as_performance).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_preparation, kodashim_obligation__study_as_archive).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'kodashim_obligation' kernel, each representing a distinct structural claim about the nature of studying sacrificial law after the Temple's destruction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
