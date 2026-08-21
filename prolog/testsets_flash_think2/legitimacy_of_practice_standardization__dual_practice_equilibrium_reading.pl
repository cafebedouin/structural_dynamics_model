% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
 *   human_readable: Dual Practice Legitimacy Equilibrium
 *   domain: political_history/modernization_studies/institutional_change
 *
 * SUMMARY:
 *   This constraint, the 'dual_practice_equilibrium_reading' of the
 *   'legitimacy_of_practice_standardization' kernel, describes a stable,
 *   partitioned system where state authority governs public/administrative
 *   domains (e.g., Gregorian calendar for taxes) and traditional authority
 *   governs private/ritual domains (e.g., lunar calendar for festivals).
 *   Compliance is strategic rather than fully internalized, and there is no
 *   expectation of convergence. The system functions as a coordination
 *   mechanism, allowing distinct forms of legitimacy to coexist without
 *   direct conflict. The low extractiveness and moderate suppression reflect
 *   this stable, functional equilibrium.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.35).
domain_priors:suppression_score(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.45).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "Dual Practice Legitimacy Equilibrium").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "political_history/modernization_studies/institutional_change").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, '91173534-47f2-46df-8ac3-6c0ecc26a472').
narrative_ontology:cs_kernel_codification('91173534-47f2-46df-8ac3-6c0ecc26a472', implicit).
narrative_ontology:cs_authority_grounding('91173534-47f2-46df-8ac3-6c0ecc26a472', distributed).
narrative_ontology:cs_reading_relation('91173534-47f2-46df-8ac3-6c0ecc26a472', legitimacy_of_practice_standardization__endogenous_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('91173534-47f2-46df-8ac3-6c0ecc26a472', legitimacy_of_practice_standardization__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('91173534-47f2-46df-8ac3-6c0ecc26a472', foundational, domain_specific_legitimacy).
narrative_ontology:cs_axiom_status(domain_specific_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('91173534-47f2-46df-8ac3-6c0ecc26a472', domain_specific_legitimacy, conventional).
narrative_ontology:cs_axiom('91173534-47f2-46df-8ac3-6c0ecc26a472', foundational, strategic_compliance_rationality).
narrative_ontology:cs_axiom_status(strategic_compliance_rationality, holdable).
narrative_ontology:cs_axiom_grounding('91173534-47f2-46df-8ac3-6c0ecc26a472', strategic_compliance_rationality, empirically_contingent).
narrative_ontology:cs_reference_frame('91173534-47f2-46df-8ac3-6c0ecc26a472', stable_domain_partitioning).
narrative_ontology:cs_drift_state('91173534-47f2-46df-8ac3-6c0ecc26a472', contemporary_post_modernization, gap(stable, minor, true)).
narrative_ontology:cs_created_at('91173534-47f2-46df-8ac3-6c0ecc26a472', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, state_administrators).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_authorities).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, cultural_preservationists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, citizens_practitioners).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, citizens_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from clear, unchallenged authority in public and administrative domains, allowing for efficient governance and resource allocation according to state law. They enforce state-mandated norms and practices.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, state_administrators, agenda_setter,
    institutional, generational, mobile, national).

% Maintain their legitimacy and influence within private, ritual, and community domains, preserving cultural continuity and social cohesion. They enforce traditional norms and customs.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_authorities, agenda_setter,
    organized, generational, constrained, local).

% Navigate and strategically comply with both state and traditional norms, benefiting from the clarity of domain partitioning (e.g., Gregorian calendar for work, lunar for festivals). They bear the cognitive and practical costs of dual compliance but avoid direct conflict.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, citizens_practitioners, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, citizens_practitioners, beneficiary).

% Would argue for a unified, state-centric system of legitimacy and practice standardization, viewing dualism as an impediment to progress. They are excluded from the framing that accepts permanent dualism.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, modernization_advocates, excluded,
    organized, generational, constrained, national).

% Benefit from the institutionalized protection of traditional practices and cultural identity that the dual-practice equilibrium affords, preventing their erosion by state-imposed uniformity.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, cultural_preservationists, beneficiary,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides clear, albeit separate, frameworks for legitimate action in public/administrative and private/ritual spheres, preventing conflict over which authority applies and allowing for strategic compliance by citizens.
% TRANSFER_FUNCTION: Transfers legitimacy and compliance from citizens to both state and traditional authorities within their respective, partitioned domains, maintaining social order and cultural continuity.
% ABSENT_VOICES: Modernization advocates who believe in a single, unified, state-led system of legitimacy are structurally excluded from the framing that accepts permanent dualism. They would argue for convergence and the obsolescence of traditional authority in all domains.
% DISAPPEARANCE_RATIONALE: If the dual legitimacy framework vanished, the stable partitioning of authority would collapse, leading to immediate and widespread conflict over which authority (state or traditional) should govern which domain. This would result in institutional instability, social friction, and a loss of cultural distinctiveness.
% FOUNDING_PROBLEM: How to integrate or manage the coexistence of pre-existing traditional authority structures with newly established or expanding state authority during modernization, without outright conflict or total assimilation of one by the other.
% FOUNDING_PROBLEM_CORROBORATION: Anthropologists, political scientists, and historians studying post-colonial states and institutional hybridization corroborate the persistence of this problem, often noting the strategic adaptation of citizens and the resilience of traditional systems. Legislative hearings and policy debates in many nations also reflect ongoing tensions.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading_tests).
:- end_tests(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Rope because it provides a functional coordination mechanism for managing distinct authority systems, leading to net benefits for participants by reducing conflict and preserving cultural forms. Extractiveness is low-moderate, reflecting the costs of navigating dual systems but not active exploitation. Suppression is moderate, as each domain enforces its norms, but the dualism itself is maintained rather than suppressed. Theater ratio is low, as both systems are genuinely functional within their spheres. The stability of this equilibrium is reflected in the relatively flat temporal measurements.
 *
 * PERSPECTIVAL GAP:
 *   While this reading asserts a stable equilibrium, other readings (endogenous_displacement_reading, exogenous_override_reading) would interpret the same historical facts as either a temporary phase leading to displacement or an ongoing struggle for state dominance. This constraint's metrics reflect the 'dual_practice_equilibrium_reading' specifically, not an average across contested interpretations.
 *
 * DIRECTIONALITY LOGIC:
 *   State administrators and traditional authorities are beneficiaries and agenda-setters, as they maintain their legitimacy and spheres of influence. Cultural preservationists also benefit from the protection of traditional practices. Citizens/practitioners are payers due to the costs of dual compliance, but also beneficiaries of the clarity and stability this equilibrium provides. Modernization advocates are excluded, as their vision of unified authority is incompatible with this dualistic framing.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    equilibrium_stability_vs_transition,
    'Is this dual-practice equilibrium a stable, long-term arrangement, or a temporary phase before one authority system (state or traditional) eventually displaces the other?',
    'Longitudinal ethnographic and historical studies tracking shifts in compliance patterns, institutional reforms, and inter-authority conflict over multiple generations.',
    'If it''s a temporary phase, the constraint''s true nature might be a Tangled Rope or Snare (if one system is actively extracting from the other during transition) or a Scaffold (if it''s a managed transition). If truly stable, the Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equilibrium_stability_vs_transition, empirical, 'Whether the observed dualism is a permanent equilibrium or a transitional state.').

omega_variable(
    strategic_vs_internalized_compliance,
    'Is compliance with dual norms genuinely strategic and rational, or is there an underlying, unacknowledged internalization of one system''s legitimacy that subtly undermines the other?',
    'Sociological and psychological studies of individual identity formation and normative adherence, particularly in contexts where the two systems present conflicting values or demands.',
    'If compliance is more internalized than strategic, the ''cost'' of dualism might be higher (e.g., identity fragmentation), and the extractiveness of one system over the other might be masked, potentially shifting the classification towards a Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(strategic_vs_internalized_compliance, empirical, 'The true nature of compliance with dual legitimacy systems.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(legi_tr_t30, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(legi_tr_t50, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 20, 0.34).
narrative_ontology:measurement(legi_be_t30, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 40, 0.35).
narrative_ontology:measurement(legi_be_t50, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 50, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(legi_su_t10, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(legi_su_t20, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(legi_su_t30, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 40, 0.45).
narrative_ontology:measurement(legi_su_t50, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 50, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
