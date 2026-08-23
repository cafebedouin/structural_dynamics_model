% ============================================================================
% CONSTRAINT STORY: software_source_status__utilitarian_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__utilitarian_hybrid_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: software_source_status__utilitarian_hybrid_reading
 *   human_readable: Utilitarian Hybrid Software Licensing — Context-Dependent Welfare Optimization
 *   domain: software_engineering/political_economy/intellectual_property
 *
 * SUMMARY:
 *   The utilitarian hybrid reading of software licensing treats the choice
 *   between open and proprietary models as a context-dependent optimization
 *   problem: maximize aggregate welfare by matching licensing to domain
 *   economics. Infrastructure (compilers, kernels, protocols) favors open
 *   licensing because network effects and composability create increasing
 *   returns to adoption. Specialized tools (EDA, CAD, scientific computing)
 *   may justify proprietary licensing because high fixed costs and small
 *   markets require concentrated investment. The reading claims no
 *   categorical victim set — proprietary licensing is not inherently
 *   extractive, nor is open licensing inherently superior. It is a
 *   coordination mechanism for a mixed ecosystem.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__utilitarian_hybrid_reading, 0.22).
domain_priors:suppression_score(software_source_status__utilitarian_hybrid_reading, 0.18).
domain_priors:theater_ratio(software_source_status__utilitarian_hybrid_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__utilitarian_hybrid_reading, rope).
narrative_ontology:human_readable(software_source_status__utilitarian_hybrid_reading, "Utilitarian Hybrid Software Licensing — Context-Dependent Welfare Optimization").
narrative_ontology:topic_domain(software_source_status__utilitarian_hybrid_reading, "software_engineering/political_economy/intellectual_property").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__utilitarian_hybrid_reading, '9e330c36-2520-424a-8302-187ddb7c6cea').
narrative_ontology:cs_kernel_codification('9e330c36-2520-424a-8302-187ddb7c6cea', distributed).
narrative_ontology:cs_authority_grounding('9e330c36-2520-424a-8302-187ddb7c6cea', distributed).
narrative_ontology:cs_reading_relation('9e330c36-2520-424a-8302-187ddb7c6cea', software_source_status__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('9e330c36-2520-424a-8302-187ddb7c6cea', software_source_status__pragmatic_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('9e330c36-2520-424a-8302-187ddb7c6cea', software_source_status__property_rights_reading, coexists_with).
narrative_ontology:cs_axiom('9e330c36-2520-424a-8302-187ddb7c6cea', foundational, licensing_as_welfare_optimization).
narrative_ontology:cs_axiom_status(licensing_as_welfare_optimization, holdable).
narrative_ontology:cs_axiom_grounding('9e330c36-2520-424a-8302-187ddb7c6cea', licensing_as_welfare_optimization, instrumental).
narrative_ontology:cs_axiom('9e330c36-2520-424a-8302-187ddb7c6cea', foundational, domain_dependent_optimal_licensing).
narrative_ontology:cs_axiom_status(domain_dependent_optimal_licensing, holdable).
narrative_ontology:cs_axiom_grounding('9e330c36-2520-424a-8302-187ddb7c6cea', domain_dependent_optimal_licensing, empirically_contingent).
narrative_ontology:cs_reference_frame('9e330c36-2520-424a-8302-187ddb7c6cea', pre_polarization_licensing_practice).
narrative_ontology:cs_drift_state('9e330c36-2520-424a-8302-187ddb7c6cea', contemporary_mixed_ecosystem, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9e330c36-2520-424a-8302-187ddb7c6cea', '').
narrative_ontology:cs_kernel_id(software_source_status__utilitarian_hybrid_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, software_users_general).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, platform_integrators).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, infrastructure_maintainers).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, specialized_tool_vendors).
narrative_ontology:constraint_vindicates(software_source_status__utilitarian_hybrid_reading, welfare_maximization_principle).
narrative_ontology:constraint_vindicates(software_source_status__utilitarian_hybrid_reading, context_dependent_optimization).
narrative_ontology:constraint_vindicates(software_source_status__utilitarian_hybrid_reading, mixed_ecosystem_viability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain access to functional software across both open and proprietary models; can choose tools based on fitness-for-purpose rather than ideological alignment. Exit is easy — switching costs are low when alternatives exist in both licensing regimes.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, software_users_general, beneficiary,
    organized, biographical, mobile, global).

% Build platforms that incorporate both open-source components (Linux, Kubernetes, PostgreSQL) and proprietary extensions (cloud services, enterprise features). Benefit from the flexibility to optimize each layer independently. Exit is arbitrage-grade — they control integration boundaries.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, platform_integrators, beneficiary,
    institutional, generational, arbitrage, global).

% Maintain core infrastructure (compilers, runtimes, kernels, package managers) under open licenses that maximize adoption and contribution. The utilitarian frame justifies open licensing for infrastructure because network effects and composability produce aggregate welfare gains. Exit is mobile — they can fork or migrate to other open projects.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, infrastructure_maintainers, beneficiary,
    organized, generational, mobile, global).

% Develop and sell proprietary tools for specialized domains (EDA, CAD, HFT, scientific computing) where high R&D costs and small markets make open-source sustainability difficult. The utilitarian frame licenses proprietary models here as welfare-maximizing when open alternatives would not exist. Exit is constrained — their business model depends on IP protection, but they operate in competitive markets.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, specialized_tool_vendors, beneficiary,
    moderate, biographical, constrained, global).

% Argue that utilitarian calculus undervalues long-term freedom and enables enclosure of the commons. Would object to any framework that treats proprietary licensing as legitimate. Their exclusion from the optimization calculus is structural — the reading treats their values as one input among many, not a constraint.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, copyleft_advocates, excluded,
    organized, generational, identity_locked, global).

% Evaluate licensing regimes by measuring innovation rates, adoption curves, security outcomes, and distributional effects across contexts. Provide the empirical feedback loop the utilitarian frame requires but does not itself generate.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, policy_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of matching licensing models to problem domains so that software production is sustainable and access is maximized — infrastructure gets open licensing for composability, specialized tools get proprietary licensing for funding depth, applications get whatever model serves their users.
% TRANSFER_FUNCTION: Moves development effort and capital toward domains where the licensing model matches the economic structure: open licensing attracts distributed contribution for infrastructure; proprietary licensing concentrates investment for specialized tools. No single directional transfer — the arrangement routes resources to where each model is productive.
% ABSENT_VOICES: Copyleft advocates and software freedom absolutists are structurally excluded — they would reject the premise that proprietary licensing can ever be welfare-maximizing. They are present in the discourse but not in the optimization function; the reading treats their position as a preference to be weighed, not a boundary condition.
% DISAPPEARANCE_RATIONALE: If the utilitarian hybrid frame disappeared, licensing decisions would default to ideological priors (freedom-imperative or property-rights) or local path dependence. Infrastructure projects would face pressure to adopt viral copyleft; specialized vendors would lose the welfare justification for proprietary models. The mixed ecosystem would polarize.
% FOUNDING_PROBLEM: Early software licensing debates polarized between 'all software must be free' and 'software is property.' Neither extreme produced optimal outcomes across all domains — infrastructure stalled under proprietary control, specialized tools couldn't sustain under pure open source. The hybrid frame emerged to match licensing to domain economics.
% FOUNDING_PROBLEM_CORROBORATION: Economic studies of open-source sustainability (Nagle 2019, Eghbal 2020) and proprietary software markets (Cusumano 2004) corroborate that different domains have different optimal licensing structures. The Linux Foundation's mixed-model governance and cloud vendors' open-core strategies are independent institutional validations.
narrative_ontology:disappearance_verdict(software_source_status__utilitarian_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__utilitarian_hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__utilitarian_hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(software_source_status__utilitarian_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__utilitarian_hybrid_reading, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__utilitarian_hybrid_reading_tests).
:- end_tests(software_source_status__utilitarian_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.22) because the constraint does not mandate extraction — it permits either model where welfare-justified. Suppression is low (0.18) because alternatives are not suppressed; developers choose licenses based on domain fit. Theater ratio is low (0.12) because the frame makes testable predictions (open infrastructure, proprietary specialties) that are borne out. Accessibility collapse is moderate (0.35) because the frame does not collapse alternative licensing philosophies — they remain live and contested. Resistance is moderate (0.42) from ideological purists on both sides who reject the premise that the other model can be legitimate.
 *
 * PERSPECTIVAL GAP:
 *   From the freedom-imperative seat, this constraint reads as a snare — it legitimizes proprietary enclosure. From the property-rights seat, it reads as a tangled rope — it concedes open infrastructure as a coordination good. From the utilitarian seat, it reads as a rope — pure coordination with minimal overhead. The engine computes these divergences from the structural data; the authored claim (rope) reflects the reading's self-understanding.
 *
 * DIRECTIONALITY LOGIC:
 *   All named stakeholders are beneficiaries in the structural sense: users gain choice, integrators gain flexibility, infrastructure maintainers gain adoption, specialized vendors gain sustainability. No stakeholder bears net extraction from the constraint itself — the constraint is the permission structure that lets each domain find its equilibrium. Copyleft advocates are excluded, not victims: their values are not enforced against, they are simply not treated as binding. Policy analysts are observers. The engine will compute low directionality (d) for all beneficiary seats, near-zero effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (polarized licensing debates producing suboptimal domain matches) remains live — new domains (AI model weights, hardware description languages, quantum software) still face the same matching problem. The constraint has not atrophied; its domain of application expands. No mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_measurement_ambiguity,
    'What constitutes ''aggregate welfare'' in software licensing — total surplus, distributional equity, long-term innovation rate, or a weighted composite? The reading''s prescriptions change with the welfare function.',
    'Empirical studies comparing licensing regimes across domains with explicit welfare metrics (consumer surplus, producer surplus, innovation diffusion rates, security outcomes).',
    'If welfare is defined narrowly (short-term consumer surplus), proprietary models look worse. If defined broadly (long-term innovation + adoption), the hybrid frame gains support. The classification is sensitive to this definitional choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_measurement_ambiguity, conceptual, 'Ambiguity in the welfare objective function that the reading optimizes.').

omega_variable(
    domain_boundary_indeterminacy,
    'Where exactly is the boundary between ''infrastructure'' (favoring open) and ''specialized tools'' (permitting proprietary)? The classification of AI model weights, hardware IP, and platform APIs is contested.',
    'Case studies of boundary domains: track licensing trajectories and welfare outcomes for technologies that sit on the infrastructure/specialty boundary.',
    'If the boundary is porous and context-dependent, the reading remains a flexible rope. If the boundary collapses to ''everything is infrastructure'' or ''everything is specialty,'' the reading degrades toward freedom-imperative or property-rights extremes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(domain_boundary_indeterminacy, empirical, 'Indeterminacy in the domain classification that drives licensing recommendations.').

omega_variable(
    kernel_reading_relationship_freedom_imperative,
    'Does the utilitarian hybrid reading foreclose the freedom-imperative reading, or do they coexist as competing frameworks?',
    'Analyze whether a single institutional framework (e.g., a legal system, a foundation governance model) can simultaneously treat software freedom as a categorical requirement AND as one welfare input among others.',
    'If they foreclose, the kernel has a structural fault line — institutions must choose one reading as authoritative. If they coexist, the kernel supports stable pluralism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relationship_freedom_imperative, conceptual, 'Structural relationship between this reading and the freedom-imperative sibling.').

omega_variable(
    kernel_reading_relationship_property_rights,
    'Does the utilitarian hybrid reading foreclose the property-rights reading, or does it influence it by legitimizing open infrastructure as welfare-maximizing?',
    'Track whether property-rights advocates adopt utilitarian arguments for open infrastructure (e.g., ''open standards benefit my proprietary products'') or reject the frame entirely.',
    'If influence, the utilitarian reading creates downstream pressure on property-rights positions without eliminating them. If foreclosure, the readings are mutually exclusive in any single framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relationship_property_rights, conceptual, 'Structural relationship between this reading and the property-rights sibling.').

omega_variable(
    kernel_reading_relationship_pragmatic_development,
    'Does the utilitarian hybrid reading foreclose the pragmatic-development reading, or do they coincide on infrastructure but diverge on specialized tools?',
    'Compare prescriptions: pragmatic-development treats open source as universally superior methodology; utilitarian-hybrid treats it as domain-optimal. Where do they disagree in practice?',
    'If they coincide on all actionable decisions, they are functionally the same reading with different justifications. If they diverge on specialized tools, they are distinct coexisting readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_relationship_pragmatic_development, conceptual, 'Structural relationship between this reading and the pragmatic-development sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__utilitarian_hybrid_reading, 1998, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1998, software_source_status__utilitarian_hybrid_reading, theater_ratio, 1998, 0.05).
narrative_ontology:measurement(soft_tr_t2004, software_source_status__utilitarian_hybrid_reading, theater_ratio, 2004, 0.08).
narrative_ontology:measurement(soft_tr_t2010, software_source_status__utilitarian_hybrid_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(soft_tr_t2016, software_source_status__utilitarian_hybrid_reading, theater_ratio, 2016, 0.12).
narrative_ontology:measurement(soft_tr_t2020, software_source_status__utilitarian_hybrid_reading, theater_ratio, 2020, 0.11).
narrative_ontology:measurement(soft_tr_t2024, software_source_status__utilitarian_hybrid_reading, theater_ratio, 2024, 0.12).

% Extraction over time
narrative_ontology:measurement(soft_be_t1998, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 1998, 0.15).
narrative_ontology:measurement(soft_be_t2004, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 2004, 0.18).
narrative_ontology:measurement(soft_be_t2010, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 2010, 0.22).
narrative_ontology:measurement(soft_be_t2016, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 2016, 0.25).
narrative_ontology:measurement(soft_be_t2020, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 2020, 0.23).
narrative_ontology:measurement(soft_be_t2024, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 2024, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1998, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 1998, 0.1).
narrative_ontology:measurement(soft_su_t2004, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 2004, 0.12).
narrative_ontology:measurement(soft_su_t2010, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 2010, 0.15).
narrative_ontology:measurement(soft_su_t2016, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 2016, 0.18).
narrative_ontology:measurement(soft_su_t2020, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 2020, 0.17).
narrative_ontology:measurement(soft_su_t2024, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 2024, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__utilitarian_hybrid_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_source_status__utilitarian_hybrid_reading, 0.15).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__property_rights_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the software_source_status kernel. The utilitarian_hybrid_reading treats licensing as a welfare-optimization variable with context-dependent prescriptions. It coexists with freedom_imperative_reading and property_rights_reading (different parties hold each), and influences pragmatic_development_reading (shares infrastructure prescriptions, diverges on specialized tools). The ε-invariance principle applies: each reading has its own ε, beneficiaries, and type. This reading's ε=0.22 reflects low extraction because it mandates no transfer — it permits domain-appropriate models.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
