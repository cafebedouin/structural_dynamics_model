% ============================================================================
% CONSTRAINT STORY: maat_order_principle__distributed_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__distributed_maintenance_reading, []).

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
 *   constraint_id: maat_order_principle__distributed_maintenance_reading
 *   human_readable: Ma'at Order Principle (Distributed Maintenance Reading)
 *   domain: ancient_history/political_philosophy/religious_studies
 *
 * SUMMARY:
 *   The Ma'at order principle, as understood through the distributed
 *   maintenance reading, posits that cosmic order and justice are sustained
 *   not solely by the Pharaoh's divine mandate, but by the proper conduct and
 *   adherence to Ma'at (truth, justice, cosmic balance) by all members of
 *   society, from the ruler to the commoner. This reading emphasizes a
 *   collective responsibility and a more horizontal accountability structure,
 *   where each actor's actions contribute to or detract from the overall
 *   cosmic balance. Failure to uphold Ma'at by any individual could, in
 *   theory, disrupt the order, making its maintenance a shared, active
 *   endeavor.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__distributed_maintenance_reading, 0.25).
domain_priors:suppression_score(maat_order_principle__distributed_maintenance_reading, 0.4).
domain_priors:theater_ratio(maat_order_principle__distributed_maintenance_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__distributed_maintenance_reading, rope).
narrative_ontology:human_readable(maat_order_principle__distributed_maintenance_reading, "Ma'at Order Principle (Distributed Maintenance Reading)").
narrative_ontology:topic_domain(maat_order_principle__distributed_maintenance_reading, "ancient_history/political_philosophy/religious_studies").

domain_priors:requires_active_enforcement(maat_order_principle__distributed_maintenance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__distributed_maintenance_reading, '802c8388-5c56-481f-aa45-a459e83b5527').
narrative_ontology:cs_kernel_codification('802c8388-5c56-481f-aa45-a459e83b5527', implicit).
narrative_ontology:cs_authority_grounding('802c8388-5c56-481f-aa45-a459e83b5527', practice).
narrative_ontology:cs_interpretation_layer_present('802c8388-5c56-481f-aa45-a459e83b5527').
narrative_ontology:cs_reading_relation('802c8388-5c56-481f-aa45-a459e83b5527', maat_order_principle__divine_mandate_reading, coexists_with).
narrative_ontology:cs_reading_relation('802c8388-5c56-481f-aa45-a459e83b5527', maat_order_principle__reciprocity_reading, coexists_with).
narrative_ontology:cs_axiom('802c8388-5c56-481f-aa45-a459e83b5527', foundational, cosmic_order_is_collective_responsibility).
narrative_ontology:cs_axiom_status(cosmic_order_is_collective_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('802c8388-5c56-481f-aa45-a459e83b5527', cosmic_order_is_collective_responsibility, deontological).
narrative_ontology:cs_axiom('802c8388-5c56-481f-aa45-a459e83b5527', foundational, legitimacy_from_demonstrated_conduct).
narrative_ontology:cs_axiom_status(legitimacy_from_demonstrated_conduct, holdable).
narrative_ontology:cs_axiom_grounding('802c8388-5c56-481f-aa45-a459e83b5527', legitimacy_from_demonstrated_conduct, empirically_contingent).
narrative_ontology:cs_reference_frame('802c8388-5c56-481f-aa45-a459e83b5527', collective_moral_economy).
narrative_ontology:cs_drift_state('802c8388-5c56-481f-aa45-a459e83b5527', late_period_complexity, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('802c8388-5c56-481f-aa45-a459e83b5527', '').
narrative_ontology:cs_kernel_id(maat_order_principle__distributed_maintenance_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, all_of_egypt).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, pharaoh).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, officials).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, commoners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the earthly embodiment of Ma'at, the Pharaoh is responsible for setting the example of proper conduct and ensuring the maintenance of cosmic order. Their legitimacy and the stability of their rule depend on upholding Ma'at, making them a primary beneficiary of its successful operation, but also bearing the highest responsibility.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, pharaoh, agenda_setter,
    institutional, generational, identity_locked, national).

% Administer justice and maintain order in accordance with Ma'at. Their careers and social standing are tied to their perceived adherence to its principles. They benefit from the stable society Ma'at creates but must actively enforce and embody its norms.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, officials, agenda_setter,
    organized, biographical, constrained, regional).

% Expected to live in accordance with Ma'at in their daily lives, contributing to the overall cosmic balance. They benefit from the social harmony, justice, and divine favor that a well-maintained Ma'at brings, and their proper conduct is seen as essential to its distributed maintenance.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, commoners, beneficiary,
    moderate, biographical, identity_locked, local).

% The entire society and cosmos are beneficiaries of Ma'at's successful maintenance, experiencing stability, prosperity, and divine blessing. The distributed responsibility ensures that the burden of upholding Ma'at is shared, making the collective a net beneficiary of the resulting order.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, all_of_egypt, beneficiary,
    institutional, civilizational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the behavior of all individuals in society towards a shared goal of cosmic and social order, ensuring stability, justice, and divine favor through collective adherence to Ma'at's principles.
% TRANSFER_FUNCTION: Transfers the responsibility for maintaining cosmic order from a singular divine source or ruler to a distributed network of actors, with the 'gain' being a stable and just society for all.
% ABSENT_VOICES: Those who might advocate for purely individualistic or anarchic principles would be absent, as the entire social and religious framework is built upon the necessity of collective adherence to Ma'at. Their voices are suppressed by the pervasive cultural and religious consensus.
% DISAPPEARANCE_RATIONALE: If the Ma'at order principle vanished, the entire social, political, and religious fabric of ancient Egypt would collapse. The Pharaoh's legitimacy, the legal system, and the very understanding of cosmic balance would be lost, leading to chaos and a complete reorganization of society.
% FOUNDING_PROBLEM: The problem of maintaining cosmic and social order, preventing chaos (Isfet), and ensuring divine favor in a complex society with diverse individual actions.
% FOUNDING_PROBLEM_CORROBORATION: The problem of maintaining order and preventing chaos was a constant concern throughout ancient Egyptian history, attested by numerous religious texts, wisdom literature, and historical records from various scribes and officials, not just the ruling elite. The ongoing need for Ma'at was a widely accepted truth across all social strata.
narrative_ontology:disappearance_verdict(maat_order_principle__distributed_maintenance_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__distributed_maintenance_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__distributed_maintenance_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(maat_order_principle__distributed_maintenance_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__distributed_maintenance_reading_tests).
:- end_tests(maat_order_principle__distributed_maintenance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the primary function is coordination towards a shared good (cosmic order), with costs distributed across all participants through behavioral norms rather than concentrated extraction. Suppression is moderate (0.4) as social norms and religious beliefs enforce adherence, but direct coercion is less central than in other readings. Theater ratio is low (0.1) because the performance of Ma'at is genuinely believed to contribute to cosmic order, making the actions functional rather than merely performative. Accessibility collapse is moderate (0.7) as the pervasive nature of Ma'at makes it difficult to opt out of its demands, but not impossible to deviate. Resistance is low (0.15) due to widespread belief in the system and the perceived benefits of cosmic order.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of commoners, the constraint is a set of behavioral norms that ensure societal stability and divine favor, making it a beneficial coordination mechanism. From the Pharaoh's perspective, it is a foundational principle that legitimizes their rule and requires constant vigilance and exemplary conduct, also largely beneficial but with significant responsibility. The distributed nature of maintenance reduces the potential for a stark perspectival gap seen in more extractive readings.
 *
 * DIRECTIONALITY LOGIC:
 *   All of Egypt, including the Pharaoh, officials, and commoners, are beneficiaries of the stable cosmic order that Ma'at provides. The distributed nature of responsibility means that while all contribute, no single group is a primary victim. The Pharaoh and officials, while having greater responsibility, also benefit from the legitimacy and stability that a well-maintained Ma'at provides. This distributed accountability leads to a more symmetric directionality for most actors, with costs and benefits broadly aligned.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    maat_interpretation_ambiguity,
    'Is Ma''at primarily maintained through distributed responsibility, divine mandate, or reciprocal obligations?',
    'Analysis of historical texts, legal codes, and religious practices to determine which interpretation held dominant sway in different periods and contexts.',
    'If divine mandate is primary, the constraint shifts towards a Snare for commoners and a Mountain for Pharaoh (as an embodiment of Ma''at). If reciprocity is primary, it becomes a Tangled Rope with mutual obligations and potential for extraction if one party fails.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(maat_interpretation_ambiguity, conceptual, 'This constraint is one reading of the Ma''at order principle kernel. This reading emphasizes distributed maintenance, contrasting with the divine mandate and reciprocity readings.').

omega_variable(
    authority_grounding_ambiguity,
    'Is the authority for Ma''at maintenance grounded in demonstrated proper conduct, or in inherent divine status?',
    'Examination of instances where Pharaohs or officials were criticized or deposed for failing to uphold Ma''at, indicating a performance-based grounding.',
    'If authority is performance-based, the constraint remains a Rope with accountability. If it''s status-based, it shifts towards a Mountain for the ruler, making extraction less visible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_ambiguity, empirical, 'Ambiguity regarding the source of legitimate authority for upholding Ma''at.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__distributed_maintenance_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__distributed_maintenance_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(maat_tr_t10, maat_order_principle__distributed_maintenance_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(maat_tr_t20, maat_order_principle__distributed_maintenance_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(maat_tr_t30, maat_order_principle__distributed_maintenance_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(maat_be_t10, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(maat_be_t20, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 20, 0.23).
narrative_ontology:measurement(maat_be_t30, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 30, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(maat_su_t10, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 10, 0.37).
narrative_ontology:measurement(maat_su_t20, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(maat_su_t30, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 30, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__distributed_maintenance_reading, identity_coordination).
narrative_ontology:affects_constraint(maat_order_principle__distributed_maintenance_reading, maat_order_principle__divine_mandate_reading).
narrative_ontology:affects_constraint(maat_order_principle__distributed_maintenance_reading, maat_order_principle__reciprocity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Ma'at order principle kernel. Each reading represents a distinct structural claim about how Ma'at is maintained and its implications for Egyptian society.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
