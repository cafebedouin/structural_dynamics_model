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
 *   constraint_id: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
 *   human_readable: Dual Practice Equilibrium in Legitimacy Standardization
 *   domain: political_history/modernization_studies/institutional_change
 *
 * SUMMARY:
 *   This constraint describes a 'dual practice equilibrium' where the
 *   legitimacy of practices is partitioned: state authority governs public
 *   and administrative domains (e.g., Gregorian calendar for taxes), while
 *   traditional authority governs private and ritual domains (e.g., lunar
 *   calendar for festivals). There is no expectation of convergence;
 *   compliance is strategic, not internalized. This reading posits a stable,
 *   long-term coexistence of distinct normative systems, where each avoids
 *   direct challenge to the other's sphere of influence. The constraint is
 *   claimed as a Rope because it facilitates coordination between potentially
 *   conflicting authority structures, with relatively low extraction and
 *   suppression, but it does impose a cost on individuals who must navigate
 *   both systems.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.3).
domain_priors:suppression_score(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.2).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "Dual Practice Equilibrium in Legitimacy Standardization").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "political_history/modernization_studies/institutional_change").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 'c879b811-8997-4bbd-8a6f-056755a1c8a9').
narrative_ontology:cs_kernel_codification('c879b811-8997-4bbd-8a6f-056755a1c8a9', distributed).
narrative_ontology:cs_authority_grounding('c879b811-8997-4bbd-8a6f-056755a1c8a9', practice).
narrative_ontology:cs_interpretation_layer_present('c879b811-8997-4bbd-8a6f-056755a1c8a9').
narrative_ontology:cs_reading_relation('c879b811-8997-4bbd-8a6f-056755a1c8a9', legitimacy_of_practice_standardization__endogenous_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('c879b811-8997-4bbd-8a6f-056755a1c8a9', legitimacy_of_practice_standardization__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('c879b811-8997-4bbd-8a6f-056755a1c8a9', foundational, domain_specific_legitimacy).
narrative_ontology:cs_axiom_status(domain_specific_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('c879b811-8997-4bbd-8a6f-056755a1c8a9', domain_specific_legitimacy, conventional).
narrative_ontology:cs_axiom('c879b811-8997-4bbd-8a6f-056755a1c8a9', foundational, non_convergence_of_practice).
narrative_ontology:cs_axiom_status(non_convergence_of_practice, holdable).
narrative_ontology:cs_axiom_grounding('c879b811-8997-4bbd-8a6f-056755a1c8a9', non_convergence_of_practice, empirically_contingent).
narrative_ontology:cs_reference_frame('c879b811-8997-4bbd-8a6f-056755a1c8a9', stable_domain_partition).
narrative_ontology:cs_drift_state('c879b811-8997-4bbd-8a6f-056755a1c8a9', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c879b811-8997-4bbd-8a6f-056755a1c8a9', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, state_bureaucracy).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_community_leaders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, citizens_and_community_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the stability of a clear division of labor, where its authority is unchallenged in public administration. It avoids the cost and resistance of enforcing universal standardization, accepting traditional practices in private domains.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, state_bureaucracy, agenda_setter,
    institutional, generational, constrained, national).

% Retain authority and legitimacy within their communities by preserving traditional practices in ritual, social, and private life. They benefit from the state's non-interference in these domains, avoiding direct confrontation.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_community_leaders, beneficiary,
    organized, generational, constrained, local).

% Navigate two distinct sets of norms and practices, applying one in public/state-facing contexts (e.g., Gregorian calendar for taxes) and another in private/community contexts (e.g., lunar calendar for festivals). This requires code-switching and maintaining dual competencies, which can be a cognitive and social cost.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, citizens_and_community_members, payer,
    moderate, biographical, constrained, local).

% Would argue for universal standardization based on efficiency, national unity, or alignment with global norms. They are excluded from the equilibrium, as their agenda is implicitly rejected by the dual-practice arrangement.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, modernization_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the coexistence of distinct authority systems (state and traditional) by partitioning their legitimate domains, preventing direct conflict over practice standardization.
% TRANSFER_FUNCTION: Transfers the burden of navigating dual practice systems to individuals, while transferring stability and reduced enforcement costs to both state and traditional authorities.
% ABSENT_VOICES: Modernization advocates and universalist reformers are absent from the negotiation that establishes this equilibrium; they would argue for a single, standardized system based on efficiency or national unity.
% DISAPPEARANCE_RATIONALE: If the dual-practice equilibrium vanished, either the state would attempt to impose universal standardization (leading to resistance), or traditional practices would expand into public domains (leading to administrative chaos), or a new, contested negotiation over legitimacy would emerge. The current stability would collapse.
% FOUNDING_PROBLEM: The problem of how to integrate or manage diverse traditional practices and authority structures within a modernizing state without provoking widespread resistance or civil unrest.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political scientists attest that managing cultural diversity and state authority remains a live challenge in many post-colonial or multi-ethnic states. The persistence of dual systems in many countries corroborates this ongoing problem, as do academic studies of institutional hybridity.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.3, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is low (0.3) because neither authority system attempts to fully dominate the other, reducing the direct costs of enforcement and resistance. Suppression is also low (0.2) as the equilibrium is maintained by mutual recognition of boundaries rather than overt coercion. Theater ratio is minimal (0.1) as both systems genuinely operate within their domains. Accessibility collapse is moderate (0.7) because while individuals must conform to specific practices in specific domains, the existence of two distinct systems means alternatives are not entirely collapsed, just partitioned. Resistance is low (0.15) because the equilibrium largely satisfies the core demands of both state and traditional authorities, and individuals adapt to the dual system.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state and traditional authorities, this is an efficient and stable coordination mechanism. From the perspective of modernization advocates, it represents a failure to achieve full integration and efficiency. Individuals experience it as a necessary but sometimes burdensome adaptation.
 *
 * DIRECTIONALITY LOGIC:
 *   The state bureaucracy and traditional community leaders are beneficiaries, as they maintain their respective spheres of authority without costly conflict. Citizens and community members are payers, bearing the cognitive and social costs of navigating dual systems. Modernization advocates are excluded, as their vision of universal standardization is implicitly rejected by this equilibrium.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stability_of_domain_partition,
    'Is the domain partition between state and traditional authority truly stable, or is it subject to gradual erosion or re-negotiation under changing social or economic conditions?',
    'Longitudinal ethnographic studies and legal analyses tracking shifts in practice and authority claims over several generations, particularly in response to economic development or globalization.',
    'If the partition is eroding, the constraint might be reclassified towards a Tangled Rope or Snare as one authority attempts to expand its domain, increasing extraction and suppression. If it proves highly resilient, the Rope classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stability_of_domain_partition, empirical, 'Assesses the long-term resilience of the dual-practice equilibrium.').

omega_variable(
    individual_cost_of_code_switching,
    'How significant are the cognitive, social, and economic costs for individuals who must constantly switch between distinct practice systems?',
    'Sociological surveys and psychological studies measuring stress, identity fragmentation, and economic disadvantages associated with navigating dual normative frameworks.',
    'If individual costs are found to be substantial, the ''extractiveness'' metric for the ''citizens_and_community_members'' seat would be higher, potentially shifting their per-seat classification towards a Snare, even if the overall constraint remains a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_cost_of_code_switching, empirical, 'Quantifies the hidden costs borne by individuals in a dual-practice system.').

omega_variable(
    legitimacy_framing_ambiguity,
    'Is the ''dual practice equilibrium'' a genuine coordination mechanism, or a strategic truce that masks underlying power imbalances and deferred conflicts?',
    'Analysis of historical archives and oral histories to uncover instances of suppressed resistance or unacknowledged grievances regarding the domain partition, particularly from marginalized groups.',
    'If it''s primarily a strategic truce, the ''theater_ratio'' and ''suppression'' metrics might be higher than currently assessed, indicating a more extractive and coercive underlying dynamic, potentially shifting the overall classification towards a Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_framing_ambiguity, conceptual, 'Examines whether the equilibrium is a true coordination or a power-enforced compromise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1950, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(legi_tr_t1970, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 1970, 0.08).
narrative_ontology:measurement(legi_tr_t1990, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(legi_tr_t2020, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(legi_be_t1950, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 1950, 0.25).
narrative_ontology:measurement(legi_be_t1970, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 1970, 0.28).
narrative_ontology:measurement(legi_be_t1990, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(legi_be_t2020, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 2020, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1950, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 1950, 0.15).
narrative_ontology:measurement(legi_su_t1970, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 1970, 0.18).
narrative_ontology:measurement(legi_su_t1990, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 1990, 0.2).
narrative_ontology:measurement(legi_su_t2020, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 2020, 0.2).


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
