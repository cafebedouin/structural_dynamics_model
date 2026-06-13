% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
 *   human_readable: Domain-Partitioned Dual Practice Legitimacy (Equilibrium Reading)
 *   domain: political_history/institutional_modernization
 *
 * SUMMARY:
 *   This story models a stable dual-legitimacy equilibrium in which state
 *   authority governs public/administrative domains (taxation, census,
 *   bureaucratic procedure, inter-regional coordination) and traditional
 *   authority governs private/ritual domains (seasonal calendars, kinship
 *   protocols, ritual dress, family law, religious practice). The constraint
 *   operates as a Rope coordination mechanism: both authority systems benefit
 *   from having a protected domain where they maintain coherence without
 *   claiming universality, and communities benefit from being able to
 *   code-switch between systems. The extractiveness is moderate (0.38)
 *   because the constraint extracts cognitive and behavioral costs of
 *   maintaining dual competence but does not concentrate wealth or
 *   identity-lock any single seat. Suppression is moderate-low (0.41) because
 *   enforcement relies on both authority systems respecting the partition
 *   boundary rather than on coercive suppression of alternatives. Theater is
 *   low (0.22) because the functional division between domains is genuine and
 *   is not being masked by performative claims of unity.
 *
 * KEY AGENTS:
 *   - state_administrative_apparatus: institutional power, sets public-domain standards, maintains partition boundary as pragmatic compromise
 *   - traditional_ritual_authorities: organized power, identity-locked, sets private-domain standards, benefits from protective partition
 *   - merchant_and_trading_class: powerful, arbitrage options, code-switches between systems for strategic advantage
 *   - farming_and_subsistence_communities: moderate power, constrained exit, benefit from lunar-calendar agricultural timing while paying state taxes on Gregorian calendar
 *   - modernizing_elites: excluded, would argue partition is unstable and unified standards are necessary for development
 *   - external_imperial_authority: excluded, seeks to impose unified standards as condition of trade or military alliance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.38).
domain_priors:suppression_score(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.41).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "Domain-Partitioned Dual Practice Legitimacy (Equilibrium Reading)").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "political_history/institutional_modernization").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, '9ca7c55d-50b3-41ff-af63-78b6127dc2c3').
narrative_ontology:cs_kernel_codification('9ca7c55d-50b3-41ff-af63-78b6127dc2c3', distributed).
narrative_ontology:cs_authority_grounding('9ca7c55d-50b3-41ff-af63-78b6127dc2c3', distributed).
narrative_ontology:cs_reading_relation('9ca7c55d-50b3-41ff-af63-78b6127dc2c3', legitimacy_of_practice_standardization__endogenous_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('9ca7c55d-50b3-41ff-af63-78b6127dc2c3', legitimacy_of_practice_standardization__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('9ca7c55d-50b3-41ff-af63-78b6127dc2c3', foundational, legitimacy_is_domain_bounded).
narrative_ontology:cs_axiom_status(legitimacy_is_domain_bounded, holdable).
narrative_ontology:cs_axiom_grounding('9ca7c55d-50b3-41ff-af63-78b6127dc2c3', legitimacy_is_domain_bounded, conventional).
narrative_ontology:cs_axiom('9ca7c55d-50b3-41ff-af63-78b6127dc2c3', secondary, dual_practice_persistence_is_rational).
narrative_ontology:cs_axiom_status(dual_practice_persistence_is_rational, holdable).
narrative_ontology:cs_axiom_grounding('9ca7c55d-50b3-41ff-af63-78b6127dc2c3', dual_practice_persistence_is_rational, instrumental).
narrative_ontology:cs_reference_frame('9ca7c55d-50b3-41ff-af63-78b6127dc2c3', domain_partitioned_legitimacy_framework).
narrative_ontology:cs_drift_state('9ca7c55d-50b3-41ff-af63-78b6127dc2c3', contemporary_national_integration_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9ca7c55d-50b3-41ff-af63-78b6127dc2c3', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, state_administrative_apparatus).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_ritual_authorities).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 'none', 1).

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
 *   The constraint achieves low extractiveness relative to comparable uniform-standard regimes because each authority system is legitimated within its domain and can maintain coherence without requiring the other system to surrender. Extractiveness rises slightly (0.32 → 0.38 across the interval) as the state's fiscal demands increase and communities incur higher costs of dual-system maintenance, but plateaus as neither authority system attempts to expand into the other's claimed domain. Suppression is moderate because the partition's stability depends on both authorities accepting domain limits — violations would require enforcement, so the constraint itself relies on mutual restraint rather than external coercion. Theater is low and stable because the partition is genuinely functional: each authority solves real coordination problems within its domain. The measurement series show the constraint as stable rather than drifting, with extractiveness rising slightly in early period and plateauing, suggesting the system has found an equilibrium.
 *
 * PERSPECTIVAL GAP:
 *   The state's view: the partition is a pragmatic compromise that enables fiscal coherence without impossible cultural enforcement costs. Traditional authorities' view: the partition is a boundary protection that preserves their legitimacy and authority. Modernizing elites' view: the partition is an unstable intermediate stage that will inevitably dissolve toward unified standards. The analytical observer's view: the partition is a genuine coordination mechanism that solves the problem of how two legitimacy systems can coexist without zero-sum conflict. From the state's seat, extractiveness appears low (they collect taxes reliably without massive suppression costs). From traditional authorities' seat, it appears as a stable rent (they collect symbolic recognition and behavioral compliance within their domain). From modernizing elites' seat, it appears as an enforced artificial limit on progress.
 *
 * DIRECTIONALITY LOGIC:
 *   State apparatus and traditional authorities both have directionality near 0.5 (symmetric): each benefits from having a protected domain (beneficiary role) but also accepts limits on their authority (cost of partition). Merchants and trading class have low directionality (~0.2): they benefit from code-switching options and face low suppression. Farming communities have moderate-high directionality (~0.65): they benefit from lunar-calendar agricultural timing but pay taxes on state calendar and maintain dual-system competence. Modernizing elites have moderate-high directionality (~0.55) as targets: the partition constrains their ability to impose unified standards, but they lack enforcement power against institutional actors. External imperial authority has directionality near 1.0 (full target): the partition blocks their imposition of unified standards, but they are excluded from the constraint's authorization structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (how to collect taxes and maintain inter-regional coordination without imposing uniform private practice) is still live under the equilibrium reading: modern states still face the cost-benefit calculation of whether unified standards are worth the suppression costs. However, the founding problem would be classified as contested rather than live under alternative readings: the exogenous_override reading would argue unified standards are legitimate once decreed by state authority; the endogenous_displacement reading would argue conversion to unified standards happens through voluntary adoption, not state decree. The mandatrophy frame does not apply here because the partition is not a degraded version of a unified-standard regime — it is a stable equilibrium reading of the kernel that rejects the premise that convergence is inevitable. The constraint prevents classification mislabeling by separating the question 'how should practice legitimacy be organized' (the kernel) from the question 'what is the empirical relationship between two authority systems' (this reading).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_boundary_stability,
    'Is the partition between public and private domains stable because both authority systems rationally accept it, or because enforcement capacity prevents either from expanding into the other''s domain?',
    'Natural experiment: reduce state enforcement capacity (collapse of administrative infrastructure) or reduce traditional authority enforcement capacity (generational replacement, ideological shift). If partition persists, stability is rational acceptance; if partition dissolves, stability was enforcement-dependent.',
    'If rational acceptance, the constraint is a genuine rope coordination mechanism. If enforcement-dependent, suppression is higher than authored and the constraint approaches tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_boundary_stability, empirical, 'Whether the dual-practice partition is sustained by mutual interest or by suppression of boundary challenges.').

omega_variable(
    extractiveness_of_dual_system_maintenance,
    'Does the cost of maintaining code-switching competence (learning and performing both calendar systems, dress codes, measurement standards) represent genuine coordination overhead or hidden extraction from communities?',
    'Comparative analysis: does extractiveness measured in dual-system regimes differ significantly from extractiveness in unified-standard regimes when controlling for state fiscal demands? Interview-based assessment of community perception: is dual-system maintenance experienced as a necessary cost or as an unjust burden?',
    'If maintenance is genuine coordination cost, extractiveness at 0.38 is justified as Rope. If it is hidden extraction, extractiveness should be higher (0.55+) and the constraint approaches Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractiveness_of_dual_system_maintenance, empirical, 'What fraction of the constraint''s extractiveness is necessary coordination cost vs. preventable overhead.').

omega_variable(
    kernel_reading_coexistence_feasibility,
    'Can a single institutional regime hold all three readings of the legitimacy kernel simultaneously, or does instantiating one reading necessarily foreclose the others?',
    'Examine historical cases where states attempted to blend readings: Does Italy''s post-1860 unification attempt (exogenous override + domain partition) generate internal contradictions? Do contemporary pluralist democracies that recognize regional autonomy (endogenous displacement within bounded domains) represent a synthesis or a nested application?',
    'If readings can coexist (e.g., exogenous decree at national level, endogenous adoption at regional level, domain partition within local communities), the kernel is genuinely open and the three readings are valid alternatives. If readings are mutually exclusive, the reading choice is consequential for regime stability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_coexistence_feasibility, conceptual, 'Whether the three kernel readings are alternative framings of one constraint or structurally incompatible constraints.').

omega_variable(
    modernizing_elite_exclusion_as_suppression,
    'Is the exclusion of modernizing elites from the partition''s authorization structure a form of suppression (suppressing the voice calling for convergence), or is it simply the consequence of the partition not addressing modernization dynamics at all?',
    'Historical evidence: do state and traditional authorities actively suppress modernization discourse, or do they simply ignore it? Do modernization advocates face legal barriers, social stigma, or coercive pressure, or do they face the structural constraint that their proposed alternative (unified standards) is not in either authority system''s interest?',
    'If suppression, the constraint''s suppression metric (0.41) understates the coercive structure. If structural non-engagement, the partition is correctly modeled as a rope with moderate suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modernizing_elite_exclusion_as_suppression, empirical, 'Whether the partition actively suppresses convergence pressure or simply does not address it.').

omega_variable(
    strategic_code_switching_vs_internalized_duality,
    'Do agents (merchants, farming communities) experience the dual-system requirement as a strategic tool they choose to deploy, or as an internalized cognitive frame where they no longer experience the systems as separate?',
    'Post-partition removal experiment: if partition is abolished and unified standards imposed, does code-switching competence persist? If yes and persists as preferred practice, internalization is strong; if yes but is experienced as loss of functionality, internalization is weaker and code-switching was strategic.',
    'If internalized, the partition''s suppression is partly internalized (agents carry it with them after removal) and its stability is higher. If strategic, suppression is more structural and partition depends on both authorities maintaining the boundary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(strategic_code_switching_vs_internalized_duality, empirical, 'Whether code-switching competence is internalized identity or strategic behavioral flexibility.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(legi_tr_t5, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 5, 0.19).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(legi_tr_t15, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(legi_tr_t25, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 25, 0.23).
narrative_ontology:measurement(legi_tr_t30, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(legi_tr_t35, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 35, 0.22).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(legi_be_t5, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 5, 0.34).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement(legi_be_t15, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 15, 0.37).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(legi_be_t25, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 25, 0.39).
narrative_ontology:measurement(legi_be_t30, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(legi_be_t35, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 35, 0.38).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(legi_su_t5, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 5, 0.39).
narrative_ontology:measurement(legi_su_t10, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(legi_su_t15, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 15, 0.41).
narrative_ontology:measurement(legi_su_t20, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(legi_su_t25, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 25, 0.41).
narrative_ontology:measurement(legi_su_t30, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 30, 0.41).
narrative_ontology:measurement(legi_su_t35, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 35, 0.41).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 40, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.12).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization__endogenous_displacement_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading (dual_practice_equilibrium) of the contested kernel legitimacy_of_practice_standardization. The endogenous_displacement_reading and exogenous_override_reading are sibling constraints that answer the same kernel question (what makes practice change legitimate) with incompatible answers. All three readings share the same factual domain (practice standards and their legitimacy) but instantiate different structural constraint stories with different ε values, beneficiary/victim structures, and suppressions. The equilibrium reading uniquely asserts that legitimacy is domain-bounded, not universal or decentralized. The three readings coexist as live positions held by different parties (state administrators, traditional authorities, and modernizing elites) and do not logically foreclose one another, but instantiating one as a regime configuration structurally incompatible with instantiating another.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
