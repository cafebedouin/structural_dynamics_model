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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: maat_order_principle__distributed_maintenance_reading
 *   human_readable: Ma'at Order Principle: Distributed Maintenance Reading
 *   domain: ancient_history/political_philosophy/religious_studies
 *
 * SUMMARY:
 *   This constraint describes the 'Ma'at order principle' as a system of
 *   distributed responsibility, where all actors from the Pharaoh to the
 *   commoner are expected to sustain cosmic order through proper conduct in
 *   their station. This reading emphasizes collective adherence and
 *   internalized norms rather than top-down enforcement. The constraint is
 *   claimed as a Rope, reflecting its function as a pervasive coordination
 *   mechanism that benefits all participants by maintaining a stable and
 *   prosperous society.
 *
 * KEY AGENTS:
 *   - Pharaoh: Agenda-setter (institutional/constrained) — primary upholder and symbol of Ma'at.
 *   - Priestly Class: Beneficiary (organized/constrained) — interpreters and ritual specialists.
 *   - Commoners: Beneficiary (moderate/constrained) — expected to live by Ma'at in daily life.
 *   - All of Egyptian Society: Beneficiary (institutional/identity_locked) — the collective entity whose identity is intertwined with Ma'at.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__distributed_maintenance_reading, 0.25).
domain_priors:suppression_score(maat_order_principle__distributed_maintenance_reading, 0.35).
domain_priors:theater_ratio(maat_order_principle__distributed_maintenance_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__distributed_maintenance_reading, rope).
narrative_ontology:human_readable(maat_order_principle__distributed_maintenance_reading, "Ma'at Order Principle: Distributed Maintenance Reading").
narrative_ontology:topic_domain(maat_order_principle__distributed_maintenance_reading, "ancient_history/political_philosophy/religious_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__distributed_maintenance_reading, '6aa96be6-6d8e-4b35-9a94-2fc757d442a6').
narrative_ontology:cs_kernel_codification('6aa96be6-6d8e-4b35-9a94-2fc757d442a6', formalized).
narrative_ontology:cs_authority_grounding('6aa96be6-6d8e-4b35-9a94-2fc757d442a6', practice).
narrative_ontology:cs_interpretation_layer_present('6aa96be6-6d8e-4b35-9a94-2fc757d442a6').
narrative_ontology:cs_reading_relation('6aa96be6-6d8e-4b35-9a94-2fc757d442a6', maat_order_principle__divine_mandate_reading, coexists_with).
narrative_ontology:cs_reading_relation('6aa96be6-6d8e-4b35-9a94-2fc757d442a6', maat_order_principle__reciprocity_reading, coexists_with).
narrative_ontology:cs_axiom('6aa96be6-6d8e-4b35-9a94-2fc757d442a6', foundational, maat_is_collective_responsibility).
narrative_ontology:cs_axiom_status(maat_is_collective_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('6aa96be6-6d8e-4b35-9a94-2fc757d442a6', maat_is_collective_responsibility, deontological).
narrative_ontology:cs_axiom('6aa96be6-6d8e-4b35-9a94-2fc757d442a6', secondary, individual_conduct_affects_cosmic_order).
narrative_ontology:cs_axiom_status(individual_conduct_affects_cosmic_order, holdable).
narrative_ontology:cs_axiom_grounding('6aa96be6-6d8e-4b35-9a94-2fc757d442a6', individual_conduct_affects_cosmic_order, theological).
narrative_ontology:cs_reference_frame('6aa96be6-6d8e-4b35-9a94-2fc757d442a6', collective_harmonious_conduct).
narrative_ontology:cs_drift_state('6aa96be6-6d8e-4b35-9a94-2fc757d442a6', historical_period_of_stability, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6aa96be6-6d8e-4b35-9a94-2fc757d442a6', '').
narrative_ontology:cs_kernel_id(maat_order_principle__distributed_maintenance_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, pharaoh).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, priestly_class).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, commoners).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, all_of_egyptian_society).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary upholder and symbol of Ma'at, whose conduct sets the example and ensures the framework for others to follow. While holding immense power, even the Pharaoh is constrained by the principles of Ma'at, as their legitimacy depends on its maintenance.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, pharaoh, agenda_setter,
    institutional, generational, constrained, national).

% Interpreters of Ma'at and ritual specialists who guide society in proper conduct. They benefit from the social stability and their elevated role within the Ma'at-governed order, but are also bound by its principles.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, priestly_class, beneficiary,
    organized, generational, constrained, national).

% Expected to live by Ma'at in their daily lives, contributing to the collective cosmic and social order through proper conduct in their station. They benefit from the stability and prosperity that Ma'at is believed to ensure.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, commoners, beneficiary,
    moderate, biographical, constrained, local).

% The collective entity whose identity and well-being are deeply intertwined with the maintenance of Ma'at. The entire social, political, and religious fabric of Egypt is predicated on this principle, making exit from its framework unthinkable.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, all_of_egyptian_society, beneficiary,
    institutional, civilizational, identity_locked, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures collective adherence to principles of truth, justice, cosmic balance, and proper conduct, preventing chaos (Isfet) and ensuring prosperity and stability for Egyptian society by distributing responsibility across all social strata.
% TRANSFER_FUNCTION: Transfers the responsibility for maintaining cosmic order from a singular divine source to all members of society, distributing both the burden of proper conduct and the benefits of a harmonious existence.
% ABSENT_VOICES: Those who might challenge the very premise of Ma'at or its distributed nature (e.g., foreign invaders, radical individualists, or those advocating for purely self-interested action) are outside the framework of this discourse and would be seen as agents of chaos (Isfet).
% DISAPPEARANCE_RATIONALE: If the principle of Ma'at and its distributed maintenance vanished overnight, the entire social, political, and religious structure of ancient Egypt, which was predicated on this cosmic order, would collapse into chaos (Isfet). The society would lose its foundational moral and operational framework.
% FOUNDING_PROBLEM: The inherent human tendency towards chaos (Isfet) and the need for a universal, pervasive principle to guide conduct and maintain cosmic, social, and individual harmony, ensuring the continued existence and prosperity of Egypt.
% FOUNDING_PROBLEM_CORROBORATION: Historical texts, religious doctrines, and archaeological evidence from various social strata (e.g., wisdom literature, tomb inscriptions, legal codes) attest to the pervasive belief in Ma'at and its necessity for societal function, corroborating its foundational role and ongoing relevance within the ancient Egyptian worldview.
narrative_ontology:disappearance_verdict(maat_order_principle__distributed_maintenance_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__distributed_maintenance_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__distributed_maintenance_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(maat_order_principle__distributed_maintenance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__distributed_maintenance_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

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
 *   Extraction is low (0.25) because the principle is primarily about shared contribution to a common good, not about one party extracting from another. Suppression is moderate (0.35) as it relies more on internalized norms, social pressure, and the pervasive belief in cosmic consequences for misconduct, rather than overt state coercion. Theater ratio is very low (0.10) because the belief in Ma'at and its efficacy is genuine and deeply ingrained across society. Accessibility collapse is moderate (0.40) as alternatives to the Ma'at framework are largely unthinkable within the Egyptian worldview, yet individual agency within its bounds is recognized. Resistance is low (0.10) due to the widespread acceptance and integration of Ma'at into all aspects of life.
 *
 * PERSPECTIVAL GAP:
 *   While all seats are beneficiaries of Ma'at's order, their specific responsibilities and the nature of their 'contribution' differ. The Pharaoh's role as agenda-setter involves significant symbolic and ritual duties, while commoners' contributions are through daily ethical conduct. The engine will compute these nuanced differences in directionality based on their declared power, exit options, and roles, even within a generally beneficial 'Rope' framework.
 *
 * DIRECTIONALITY LOGIC:
 *   All named agents are beneficiaries, as the maintenance of Ma'at is understood to be universally beneficial for Egyptian society. The Pharaoh, as the primary agenda-setter, has a directionality closer to full beneficiary, as their role is to embody and facilitate Ma'at. The priestly class and commoners also benefit, with their directionality reflecting their active participation in upholding the order. All of Egyptian society is identity-locked into this framework, making its benefits inseparable from its very existence.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading of Ma'at as a distributed responsibility prevents mislabeling it as pure extraction by emphasizing the collective coordination function and the shared benefits. The low extractiveness and theater ratio, combined with the 'live' status of the founding problem, indicate that the constraint's mandate remains highly functional and relevant within its historical context, rather than having atrophied into mere performance or rent-seeking.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authority_source_ambiguity,
    'Is the authority of Ma''at primarily derived from the collective practice of its maintenance (distributed responsibility) or from a singular divine mandate embodied by the Pharaoh?',
    'Analysis of historical periods where pharaonic authority was weak or contested: if Ma''at''s principles continued to guide society effectively, it supports distributed authority; if societal collapse ensued, it supports a singular divine mandate.',
    'If primarily divine mandate, the constraint''s effective suppression and extractiveness might be higher, as it would be enforced top-down. If distributed, it remains a coordination mechanism with lower extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_source_ambiguity, conceptual, 'Ambiguity regarding the ultimate source of Ma''at''s authority.').

omega_variable(
    individual_vs_pharaonic_responsibility,
    'What is the precise balance of responsibility for Ma''at''s maintenance between the Pharaoh and the commoners? Does individual misconduct have the same cosmic impact as pharaonic failure?',
    'Comparative analysis of religious texts and wisdom literature: if texts emphasize individual piety as equally critical to cosmic balance as pharaonic justice, it supports distributed responsibility. If pharaonic actions are disproportionately weighted, it leans towards a more centralized model.',
    'If pharaonic responsibility is overwhelmingly dominant, the ''distributed maintenance'' reading''s extractiveness and suppression might be understated, as the burden on the Pharaoh would be immense, potentially making it a Tangled Rope for the ruler. If truly distributed, the Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_vs_pharaonic_responsibility, empirical, 'Degree of individual vs. pharaonic responsibility for Ma''at.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__distributed_maintenance_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__distributed_maintenance_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(maat_tr_t20, maat_order_principle__distributed_maintenance_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(maat_tr_t40, maat_order_principle__distributed_maintenance_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(maat_tr_t60, maat_order_principle__distributed_maintenance_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(maat_tr_t80, maat_order_principle__distributed_maintenance_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement(maat_tr_t100, maat_order_principle__distributed_maintenance_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(maat_be_t20, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement(maat_be_t40, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 40, 0.23).
narrative_ontology:measurement(maat_be_t60, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 60, 0.24).
narrative_ontology:measurement(maat_be_t80, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 80, 0.25).
narrative_ontology:measurement(maat_be_t100, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 100, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(maat_su_t20, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 20, 0.32).
narrative_ontology:measurement(maat_su_t40, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 40, 0.33).
narrative_ontology:measurement(maat_su_t60, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 60, 0.34).
narrative_ontology:measurement(maat_su_t80, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 80, 0.35).
narrative_ontology:measurement(maat_su_t100, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 100, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__distributed_maintenance_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
