% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__techno_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__techno_nationalist_reading, []).

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
 *   constraint_id: performance_legitimacy__techno_nationalist_reading
 *   human_readable: Techno-Nationalist Performance Legitimacy (Strategic Industry Dominance)
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This constraint instantiates the techno-nationalist reading of the
 *   performance legitimacy kernel: state power is justified by achieving
 *   technological self-sufficiency, supply-chain resilience, and global
 *   leadership in strategic industries. The arrangement coordinates national
 *   resources toward sectors with security externalities but simultaneously
 *   extracts from consumer-oriented and market-driven allocation. It is
 *   authored as a tangled rope because the same structural mechanism that
 *   solves a genuine coordination problem (underinvestment in strategic
 *   technology) also enforces asymmetric extraction (suppressed consumption,
 *   diverted credit, and marginalized market signals).
 *
 * KEY AGENTS:
 *   - defense_adjacent_tech_sectors: Primary beneficiary â receives directed credit and procurement protection (powerful/constrained)
 *   - national_champion_firms: Secondary beneficiary â designated strategic enterprises with preferential market access (powerful/constrained)
 *   - consumer_sectors: Primary target â bears the cost of suppressed consumption and reallocated resources (powerless/trapped)
 *   - market_oriented_enterprises: Secondary target â credit-discriminated and excluded from priority channels (moderate/constrained)
 *   - state_industrial_planners: Agenda setter â administers industrial policy and strategic controls (institutional/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__techno_nationalist_reading, 0.72).
domain_priors:suppression_score(performance_legitimacy__techno_nationalist_reading, 0.78).
domain_priors:theater_ratio(performance_legitimacy__techno_nationalist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__techno_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__techno_nationalist_reading, "Techno-Nationalist Performance Legitimacy (Strategic Industry Dominance)").
narrative_ontology:topic_domain(performance_legitimacy__techno_nationalist_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__techno_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__techno_nationalist_reading, '9a4a38a7-3a3b-46f8-aabf-8bfa809896d3').
narrative_ontology:cs_kernel_codification('9a4a38a7-3a3b-46f8-aabf-8bfa809896d3', formalized).
narrative_ontology:cs_authority_grounding('9a4a38a7-3a3b-46f8-aabf-8bfa809896d3', lineage).
narrative_ontology:cs_interpretation_layer_present('9a4a38a7-3a3b-46f8-aabf-8bfa809896d3').
narrative_ontology:cs_reading_relation('9a4a38a7-3a3b-46f8-aabf-8bfa809896d3', performance_legitimacy__quantitative_growth_reading, coexists_with).
narrative_ontology:cs_reading_relation('9a4a38a7-3a3b-46f8-aabf-8bfa809896d3', performance_legitimacy__qualitative_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('9a4a38a7-3a3b-46f8-aabf-8bfa809896d3', performance_legitimacy__livelihood_security_reading, influences).
narrative_ontology:cs_axiom('9a4a38a7-3a3b-46f8-aabf-8bfa809896d3', foundational, strategic_industry_supremacy_imperative).
narrative_ontology:cs_axiom_status(strategic_industry_supremacy_imperative, holdable).
narrative_ontology:cs_axiom_grounding('9a4a38a7-3a3b-46f8-aabf-8bfa809896d3', strategic_industry_supremacy_imperative, instrumental).
narrative_ontology:cs_axiom('9a4a38a7-3a3b-46f8-aabf-8bfa809896d3', foundational, market_subordination_to_security).
narrative_ontology:cs_axiom_status(market_subordination_to_security, holdable).
narrative_ontology:cs_axiom_grounding('9a4a38a7-3a3b-46f8-aabf-8bfa809896d3', market_subordination_to_security, conventional).
narrative_ontology:cs_reference_frame('9a4a38a7-3a3b-46f8-aabf-8bfa809896d3', techno_nationalist_ascendancy).
narrative_ontology:cs_drift_state('9a4a38a7-3a3b-46f8-aabf-8bfa809896d3', contemporary_geopolitical_rivalry, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('9a4a38a7-3a3b-46f8-aabf-8bfa809896d3', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__techno_nationalist_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, defense_adjacent_tech_sectors).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, national_champion_firms).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, consumer_sectors).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, market_oriented_enterprises).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive directed state credit, procurement guarantees, and regulatory protection to develop technologies deemed strategic for national security. Their revenue and expansion depend on state-defined threat assessments and industrial plans rather than on consumer demand or open competition. Exit from this status means losing protected markets and competing without subsidized capital.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, defense_adjacent_tech_sectors, beneficiary,
    powerful, generational, constrained, national).

% Designated as strategic enterprises with preferential access to state-backed capital, licenses, and government contracts. They operate as extensions of industrial policy and benefit from barriers imposed on foreign and domestic rivals. Surrendering champion status would mean facing market discipline without the state's risk-absorption mechanisms.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, national_champion_firms, beneficiary,
    powerful, generational, constrained, national).

% Bear the costs of suppressed household consumption, higher prices, and reduced variety as national savings and fiscal resources are diverted toward strategic industries. Individual households cannot opt out of the macroeconomic reallocation; their preferences are structurally subordinated to state security targets.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, consumer_sectors, payer,
    powerless, immediate, trapped, national).

% Non-strategic firms face credit discrimination, procurement exclusion, and regulatory disadvantages relative to national champions. They survive in residual consumer markets but cannot access the state-directed investment and priority policy channels that determine sectoral profitability. Full market-based expansion is blocked by the planners' allocation hierarchy.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, market_oriented_enterprises, payer,
    moderate, biographical, constrained, national).

% Set strategic industry targets, administer directed investment funds, enforce technology export controls, and allocate regulatory exemptions. Their institutional legitimacy and career advancement depend on demonstrating progress toward technological sovereignty and great-power status; abandoning the techno-nationalist framework would collapse their authority.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, state_industrial_planners, agenda_setter,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__techno_nationalist_reading, diffuse).
narrative_ontology:fixing_cost_class(performance_legitimacy__techno_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mobilizes national savings and talent toward strategic industries that decentralized private capital would under-invest in due to long time horizons, spillover risks, and national-security externalities; coordinates supply-chain resilience and technology acquisition across state, firm, and research institutions.
% TRANSFER_FUNCTION: Moves capital, credit, procurement contracts, and regulatory discretion from consumer-oriented sectors and market-driven firms to defense-adjacent technology producers and state-designated national champions through industrial plans, directed lending, and strategic trade controls.
% ABSENT_VOICES: Consumer advocates, market-oriented economists, and non-strategic firms are structurally under-weighted in planning forums; their exclusion is inherent to a mechanism that weights national-security and great-power criteria over household welfare and price signals.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, directed credit pipelines would collapse, defense-adjacent firms would face market pricing and foreign competition, consumer sectors would experience reallocated capital and restored purchasing power, and the state planners' primary legitimacy anchor would disappear â the economy would reorganize around market signals rather than strategic industry targets.
% FOUNDING_PROBLEM: Chronic underinvestment in strategic technology and acute vulnerability to foreign supply-chain coercion in a geopolitically competitive environment.
% FOUNDING_PROBLEM_CORROBORATION: Geopolitical strategists and international security scholars outside the beneficiary sectors attest to genuine supply-chain vulnerabilities; independent development economists and consumer advocates attest that the mechanism has outgrown its acute security rationale and now shelters inefficient incumbents at broad economic cost.
narrative_ontology:disappearance_verdict(performance_legitimacy__techno_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__techno_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__techno_nationalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(performance_legitimacy__techno_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__techno_nationalist_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__techno_nationalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__techno_nationalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__techno_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because massive resource diversion persists regardless of market signals or consumer willingness to pay. Suppression (0.78) is higher still because the constraint actively suppresses price-based allocation and excludes non-strategic firms. Theater ratio (0.45) is moderate: genuine technological development occurs, but an increasing share of industrial policy is performative self-sufficiency declarations and politically driven project launches. Accessibility collapse (0.72) reflects the structural marginalization of market-driven alternatives. Resistance (0.55) is moderate because consumer sectors are diffuse and market-oriented firms are politically outmatched by national champions.
 *
 * PERSPECTIVAL GAP:
 *   From the state planner and beneficiary seats, the constraint is necessary coordination against geopolitical encirclement; from the consumer and market-oriented seats, it is persistent extraction that subordinates living standards and commercial opportunity to strategic industry targets. The engine computes this divergence from the structural data â the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (defense_adjacent_tech_sectors, national_champion_firms) sit near the full-beneficiary end: they receive subsidized capital, protected markets, and regulatory exemption. Payers (consumer_sectors, market_oriented_enterprises) sit near the full-target end: they bear the costs of diverted resources and suppressed alternatives. State_industrial_planners administer the transfer and are identity-locked into the framework by their legitimacy dependence on technological sovereignty.
 *
 * MANDATROPHY ANALYSIS:
 *   The genuine coordination function (solving underinvestment in long-horizon, high-spillover strategic technology) prevents mislabeling as pure extraction, while the persistent victimization of consumer and market sectors prevents mislabeling as pure coordination. The constraint is tangled rope because the same enforcement machinery that maintains the coordination function (directed credit, procurement preference, export control) also enforces the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'How does the techno-nationalist reading of performance legitimacy structurally relate to the quantitative growth, qualitative development, and livelihood security sibling readings within the same kernel?',
    'Historical policy-shift analysis tracking which reading dominates five-year plans and budget allocation, and whether a single party framework can hold multiple readings simultaneously.',
    'Determines whether this constraint is a stable attractor or a temporary ascendancy within a contested legitimacy kernel; influences classification confidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Structural position of this reading within the performance legitimacy kernel').

omega_variable(
    security_rationale_genuineness,
    'Is the strategic industry priority driven by objective national security threats, or has it become a durable rent-seeking vehicle for defense-adjacent firms and planners?',
    'Independent audit of technology gaps against objective threat assessments; comparison of state-supported firm productivity with market benchmarks and international competitors.',
    'If purely rent-seeking, the coordination story is cover and classification shifts toward snare; if proportionate and security-driven, the constraint moves toward scaffold or rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_rationale_genuineness, empirical, 'Whether security rationale is genuine or cover for extraction').

omega_variable(
    market_allocation_viability,
    'Could market-driven allocation achieve comparable strategic technology outcomes through indirect incentives rather than directed state control?',
    'Cross-national comparison of innovation outcomes in states relying on subsidies, prizes, and procurement versus direct state-led industrial policy.',
    'If market alternatives are viable, the accessibility collapse is constructed rather than necessary, raising both extraction and suppression scores.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(market_allocation_viability, conceptual, 'Viability of market-based alternatives to state direction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__techno_nationalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_leg_tech_tr_t0, performance_legitimacy__techno_nationalist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(perf_leg_tech_tr_t7, performance_legitimacy__techno_nationalist_reading, theater_ratio, 7, 0.24).
narrative_ontology:measurement(perf_leg_tech_tr_t14, performance_legitimacy__techno_nationalist_reading, theater_ratio, 14, 0.3).
narrative_ontology:measurement(perf_leg_tech_tr_t21, performance_legitimacy__techno_nationalist_reading, theater_ratio, 21, 0.36).
narrative_ontology:measurement(perf_leg_tech_tr_t28, performance_legitimacy__techno_nationalist_reading, theater_ratio, 28, 0.41).
narrative_ontology:measurement(perf_leg_tech_tr_t35, performance_legitimacy__techno_nationalist_reading, theater_ratio, 35, 0.44).
narrative_ontology:measurement(perf_leg_tech_tr_t40, performance_legitimacy__techno_nationalist_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(perf_leg_tech_be_t0, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(perf_leg_tech_be_t7, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 7, 0.45).
narrative_ontology:measurement(perf_leg_tech_be_t14, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 14, 0.52).
narrative_ontology:measurement(perf_leg_tech_be_t21, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 21, 0.6).
narrative_ontology:measurement(perf_leg_tech_be_t28, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 28, 0.66).
narrative_ontology:measurement(perf_leg_tech_be_t35, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 35, 0.7).
narrative_ontology:measurement(perf_leg_tech_be_t40, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(perf_leg_tech_su_t0, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(perf_leg_tech_su_t7, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 7, 0.5).
narrative_ontology:measurement(perf_leg_tech_su_t14, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 14, 0.58).
narrative_ontology:measurement(perf_leg_tech_su_t21, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 21, 0.66).
narrative_ontology:measurement(perf_leg_tech_su_t28, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 28, 0.72).
narrative_ontology:measurement(perf_leg_tech_su_t35, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 35, 0.76).
narrative_ontology:measurement(perf_leg_tech_su_t40, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__techno_nationalist_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the performance_legitimacy kernel, which decomposes into at least four structurally distinct constraints (quantitative_growth, qualitative_development, livelihood_security, techno_nationalist). Each reading carries a different beneficiary/victim structure and epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
