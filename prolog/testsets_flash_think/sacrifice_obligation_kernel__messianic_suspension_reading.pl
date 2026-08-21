% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__messianic_suspension_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__messianic_suspension_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: sacrifice_obligation_kernel__messianic_suspension_reading
 *   human_readable: Sacrifice Obligation: Messianic Suspension Reading
 *   domain: religious_law/halakhic_authority/commitment_system_dynamics
 *
 * SUMMARY:
 *   This constraint represents one reading of the
 *   'sacrifice_obligation_kernel' within Jewish religious law. It posits that
 *   the divine obligation for sacrificial worship is currently suspended, not
 *   abrogated or transformed, until the messianic era. During this
 *   suspension, the study of sacrificial laws is understood as a vital
 *   activity that maintains 'operational readiness' for the eventual
 *   restoration of the Temple and its rituals. This reading emphasizes
 *   continuity and future-oriented preparation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__messianic_suspension_reading, 0.15).
domain_priors:suppression_score(sacrifice_obligation_kernel__messianic_suspension_reading, 0.1).
domain_priors:theater_ratio(sacrifice_obligation_kernel__messianic_suspension_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__messianic_suspension_reading, rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel__messianic_suspension_reading, "Sacrifice Obligation: Messianic Suspension Reading").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__messianic_suspension_reading, "religious_law/halakhic_authority/commitment_system_dynamics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__messianic_suspension_reading, '1c7b5413-3b5f-41fc-9641-bdcfcee3f9d1').
narrative_ontology:cs_kernel_codification('1c7b5413-3b5f-41fc-9641-bdcfcee3f9d1', fixed_text).
narrative_ontology:cs_authority_grounding('1c7b5413-3b5f-41fc-9641-bdcfcee3f9d1', lineage).
narrative_ontology:cs_interpretation_layer_present('1c7b5413-3b5f-41fc-9641-bdcfcee3f9d1').
narrative_ontology:cs_reading_relation('1c7b5413-3b5f-41fc-9641-bdcfcee3f9d1', sacrifice_obligation_kernel__performance_only_reading, forecloses).
narrative_ontology:cs_reading_relation('1c7b5413-3b5f-41fc-9641-bdcfcee3f9d1', sacrifice_obligation_kernel__study_as_exercise_reading, coexists_with).
narrative_ontology:cs_reading_relation('1c7b5413-3b5f-41fc-9641-bdcfcee3f9d1', sacrifice_obligation_kernel__symbolic_archive_reading, forecloses).
narrative_ontology:cs_axiom('1c7b5413-3b5f-41fc-9641-bdcfcee3f9d1', foundational, divine_suspension_of_mitzvah).
narrative_ontology:cs_axiom_status(divine_suspension_of_mitzvah, holdable).
narrative_ontology:cs_axiom_grounding('1c7b5413-3b5f-41fc-9641-bdcfcee3f9d1', divine_suspension_of_mitzvah, theological).
narrative_ontology:cs_axiom('1c7b5413-3b5f-41fc-9641-bdcfcee3f9d1', foundational, study_as_operational_readiness).
narrative_ontology:cs_axiom_status(study_as_operational_readiness, holdable).
narrative_ontology:cs_axiom_grounding('1c7b5413-3b5f-41fc-9641-bdcfcee3f9d1', study_as_operational_readiness, conventional).
narrative_ontology:cs_reference_frame('1c7b5413-3b5f-41fc-9641-bdcfcee3f9d1', post_temple_destruction_halakha).
narrative_ontology:cs_drift_state('1c7b5413-3b5f-41fc-9641-bdcfcee3f9d1', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1c7b5413-3b5f-41fc-9641-bdcfcee3f9d1', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, future_generations).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, halakhic_scholars).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, observant_jews).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and transmit the divine law, articulating the doctrine of messianic suspension and the role of study in maintaining operational readiness. They benefit from the continued relevance and intellectual engagement with the tradition.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, halakhic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Adhere to the interpretation that sacrifice is suspended, not abrogated, and engage in study as a means of fulfilling their religious obligation and maintaining readiness. They benefit from a coherent framework for religious practice in the absence of the Temple.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, observant_jews, beneficiary,
    moderate, biographical, constrained, global).

% Are the ultimate beneficiaries of the preserved knowledge and operational readiness, ensuring that the sacrificial system can be reinstituted immediately upon messianic restoration without loss of tradition or practical understanding.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, future_generations, beneficiary,
    powerless, generational, analytical, universal).

% Represents the future state for which the current community maintains operational readiness. This community would directly implement the preserved knowledge.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, messianic_era_community, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_kernel__messianic_suspension_reading, future_generations).
narrative_ontology:fixing_cost_class(sacrifice_obligation_kernel__messianic_suspension_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the religious community's intellectual and spiritual efforts during a period when physical ritual performance is impossible, ensuring the continuity of tradition and readiness for future messianic restoration.
% TRANSFER_FUNCTION: Transfers intellectual and devotional focus from physical ritual performance to the study and preservation of sacrificial law, from observant Jews to future generations, ensuring the continuity of halakhic knowledge.
% ABSENT_VOICES: Those who might argue for immediate, symbolic, or alternative forms of sacrifice, or those who reject the concept of messianic restoration entirely, are largely outside the halakhic discourse that upholds this interpretation.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, the entire framework of post-Temple Jewish religious observance and messianic hope would collapse, requiring a fundamental re-evaluation of core theological tenets and communal identity regarding the purpose and future of sacrifice.
% FOUNDING_PROBLEM: The destruction of the Second Temple rendered physical sacrificial worship impossible, creating a profound crisis of religious practice and continuity for the Jewish people.
% FOUNDING_PROBLEM_CORROBORATION: This interpretation is widely attested by centuries of rabbinic literature, historical accounts of the Temple's destruction, and ongoing theological discourse across diverse Jewish communities, confirming the enduring nature of the problem and the solution.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__messianic_suspension_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__messianic_suspension_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__messianic_suspension_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(sacrifice_obligation_kernel__messianic_suspension_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__messianic_suspension_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__messianic_suspension_reading_tests).
:- end_tests(sacrifice_obligation_kernel__messianic_suspension_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the obligation is in abeyance; no party is actively extracted from for non-performance, as performance is impossible. Suppression is low (0.1) as the constraint is a theological interpretation, not enforced by human coercion. Accessibility collapse is high (0.9) because the physical performance of sacrifice is divinely impossible without the Temple. Resistance is negligible (0.05) as this is a widely accepted theological position. Theater ratio is low (0.1) because study is considered a genuine, instrumental activity for maintaining future capacity, not a mere performance.
 *
 * PERSPECTIVAL GAP:
 *   Other readings of the sacrifice obligation kernel would yield different classifications. For instance, a 'performance_only_reading' might see high extraction from those unable to perform, while a 'symbolic_archive_reading' might view the entire concept as a cultural artifact with no active halakhic claim. This reading, however, focuses on the active, albeit suspended, nature of the obligation and the instrumental role of study.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic scholars are agenda-setters, interpreting and transmitting this understanding. Observant Jews are beneficiaries, gaining a coherent framework for practice and maintaining their connection to tradition. Future generations are the primary beneficiaries of the preserved knowledge and readiness. There are no victims, as the obligation is suspended, not violated or extracted from.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not mandatrophic because its mandate—maintaining operational readiness for messianic restoration—is still live. The 'founding problem' (the destruction of the Temple) persists, and the 'solution' (study for readiness) remains relevant until the messianic era. The constraint's function is forward-looking and therefore not atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_vs_transformation,
    'Is the sacrifice obligation truly suspended, or has it been fundamentally transformed into a different form of worship (e.g., prayer, study) that now fulfills the original mitzvah?',
    'Further theological and halakhic discourse, potentially informed by new revelatory insights or a consensus shift among leading rabbinic authorities.',
    'If transformed, the constraint''s ''suspended'' nature would be reclassified, potentially leading to a different understanding of current obligations and a lower ''readiness'' component. If truly suspended, the current reading holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suspension_vs_transformation, conceptual, 'Ambiguity regarding the nature of the obligation''s current status.').

omega_variable(
    efficacy_of_study_for_readiness,
    'Does the study of sacrificial laws genuinely maintain ''operational readiness'' for future physical performance, or is it primarily a symbolic act of continuity?',
    'Empirical observation during a hypothetical messianic restoration: would the community, based on study alone, be able to immediately and correctly reinstitute the rituals without significant practical gaps?',
    'If study is found to be insufficient for practical readiness, the ''instrumental'' aspect of this reading would weaken, potentially shifting it towards a more ''symbolic'' or ''performance_only'' interpretation. If effective, the current reading is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_of_study_for_readiness, empirical, 'Whether intellectual study translates effectively to practical ritual capacity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__messianic_suspension_reading, 70, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t70, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 70, 0.1).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 500, 0.09).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1500, 0.11).
narrative_ontology:measurement(sacr_tr_t2024, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t70, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 70, 0.15).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 500, 0.14).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1000, 0.15).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1500, 0.16).
narrative_ontology:measurement(sacr_be_t2024, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t70, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 70, 0.1).
narrative_ontology:measurement(sacr_su_t500, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 500, 0.09).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 1000, 0.1).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 1500, 0.11).
narrative_ontology:measurement(sacr_su_t2024, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__messianic_suspension_reading, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel__study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel__symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'sacrifice_obligation_kernel', each representing a distinct halakhic or theological interpretation of the obligation's status post-Temple destruction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
