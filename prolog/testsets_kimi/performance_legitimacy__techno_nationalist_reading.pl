% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__techno_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Performance Legitimacy: Techno-Nationalist Reading
 *   domain: political_economy/state_capitalism/development
 *
 * SUMMARY:
 *   This constraint instantiates the techno-nationalist reading of the
 *   performance legitimacy kernel. Here, political legitimacy is grounded in
 *   achieving technological self-sufficiency and global leadership in
 *   strategic industries, framed as necessary for national security and
 *   great-power status. The constraint operates through massive directed
 *   investment, export controls, supply-chain resilience mandates, and the
 *   subordination of consumer-sector market signals to strategic production
 *   targets. It is structurally contested by sibling readings that ground
 *   legitimacy in quantitative GDP growth, qualitative structural
 *   transformation, or tangible livelihood security.
 *
 * KEY AGENTS:
 *   - state_strategic_planner: Agenda-setter (institutional/national/generational) â designs industrial policy and enforces techno-nationalist priorities through administrative allocation.
 *   - defense_tech_sector: Primary beneficiary (powerful/national) â receives directed procurement, R&D subsidies, and protection from foreign competition.
 *   - national_champion_firms: Primary beneficiary (powerful/national) â favored with credit and regulatory forbearance in strategic industries.
 *   - consumer_sector_enterprises: Primary payer (moderate/national) â starved of capital and policy support relative to strategic sectors.
 *   - market_oriented_enterprises: Primary payer (moderate/national) â bear costs of market-signal suppression and planning targets.
 *   - household_consumers: Payer (powerless/national) â pay higher prices and reduced variety under import-substitution regimes.
 *   - international_tech_competitors: Excluded (powerful/global) â structurally barred from domestic strategic markets.
 *   - development_economist: Observer (analytical/global) â evaluates innovation outcomes against rent-seeking.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__techno_nationalist_reading, 0.75).
domain_priors:suppression_score(performance_legitimacy__techno_nationalist_reading, 0.7).
domain_priors:theater_ratio(performance_legitimacy__techno_nationalist_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__techno_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__techno_nationalist_reading, "Performance Legitimacy: Techno-Nationalist Reading").
narrative_ontology:topic_domain(performance_legitimacy__techno_nationalist_reading, "political_economy/state_capitalism/development").

domain_priors:requires_active_enforcement(performance_legitimacy__techno_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__techno_nationalist_reading, 'ca7c9c5a-335d-4249-8cb5-aa6e61db4759').
narrative_ontology:cs_kernel_codification('ca7c9c5a-335d-4249-8cb5-aa6e61db4759', implicit).
narrative_ontology:cs_authority_grounding('ca7c9c5a-335d-4249-8cb5-aa6e61db4759', practice).
narrative_ontology:cs_interpretation_layer_present('ca7c9c5a-335d-4249-8cb5-aa6e61db4759').
narrative_ontology:cs_reading_relation('ca7c9c5a-335d-4249-8cb5-aa6e61db4759', performance_legitimacy__quantitative_growth_reading, influences).
narrative_ontology:cs_reading_relation('ca7c9c5a-335d-4249-8cb5-aa6e61db4759', performance_legitimacy__qualitative_development_reading, influences).
narrative_ontology:cs_reading_relation('ca7c9c5a-335d-4249-8cb5-aa6e61db4759', performance_legitimacy__livelihood_security_reading, influences).
narrative_ontology:cs_axiom('ca7c9c5a-335d-4249-8cb5-aa6e61db4759', foundational, technological_autonomy_as_existential_imperative).
narrative_ontology:cs_axiom_status(technological_autonomy_as_existential_imperative, holdable).
narrative_ontology:cs_axiom_grounding('ca7c9c5a-335d-4249-8cb5-aa6e61db4759', technological_autonomy_as_existential_imperative, instrumental).
narrative_ontology:cs_axiom('ca7c9c5a-335d-4249-8cb5-aa6e61db4759', foundational, strategic_priority_over_market_allocation).
narrative_ontology:cs_axiom_status(strategic_priority_over_market_allocation, holdable).
narrative_ontology:cs_axiom_grounding('ca7c9c5a-335d-4249-8cb5-aa6e61db4759', strategic_priority_over_market_allocation, conventional).
narrative_ontology:cs_reference_frame('ca7c9c5a-335d-4249-8cb5-aa6e61db4759', techno_nationalist_development_state).
narrative_ontology:cs_drift_state('ca7c9c5a-335d-4249-8cb5-aa6e61db4759', market_reform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ca7c9c5a-335d-4249-8cb5-aa6e61db4759', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__techno_nationalist_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, defense_tech_sector).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, national_champion_firms).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, consumer_sector_enterprises).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, market_oriented_enterprises).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, household_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs five-year plans, industrial policy, and strategic technology roadmaps; directs state capital and regulatory tools to prioritize defense-adjacent and strategic sectors; justifies subordinating market signals to national security imperatives.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, state_strategic_planner, agenda_setter,
    institutional, generational, constrained, national).

% Receives guaranteed procurement, directed R&D funding, and protection from foreign competition under national security rubrics; expected to deliver breakthroughs that validate the techno-nationalist model.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, defense_tech_sector, beneficiary,
    powerful, generational, constrained, national).

% Favored with preferential credit, regulatory forbearance, and domestic market access in strategic industries; accountable to state technology targets rather than purely to market returns.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, national_champion_firms, beneficiary,
    powerful, biographical, constrained, national).

% Face credit rationing, regulatory neglect, and talent diversion to strategic sectors; must compete without the subsidies and protection afforded to national champions.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, consumer_sector_enterprises, payer,
    moderate, biographical, constrained, national).

% Operate in sectors where administrative targets and state-directed investment override price signals; bear the adjustment costs of import-substitution and localization mandates.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, market_oriented_enterprises, payer,
    moderate, biographical, constrained, national).

% Pay higher prices and accept reduced variety for strategic goods subject to import substitution and limited competition; fund industrial policy through taxation without direct voice in sectoral allocation.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, household_consumers, payer,
    powerless, biographical, constrained, national).

% Barred from domestic strategic markets by investment screening, procurement exclusion, data-localization rules, and security reviews; their exclusion is the active boundary of the constraint.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, international_tech_competitors, excluded,
    powerful, biographical, trapped, global).

% Analyzes whether industrial policy produces genuine innovation spillovers or rents for favored firms; tracks divergence between strategic narratives and productivity outcomes.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, development_economist, observer,
    analytical, generational, analytical, global).

narrative_ontology:fixing_cost_class(performance_legitimacy__techno_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of financing high-risk, long-horizon strategic technology development that private markets may under-supply due to externalities, appropriability problems, and national-security externalities.
% TRANSFER_FUNCTION: Moves capital, policy forbearance, market access, and procurement guarantees from consumer sectors and market-oriented enterprises to defense-adjacent technology producers and state-designated national champions.
% ABSENT_VOICES: Consumer advocates, market-oriented economists arguing for allocative efficiency, and foreign technology firms are structurally excluded from industrial policy deliberations; their objections are treated as incompatible with security framing.
% DISAPPEARANCE_RATIONALE: If the techno-nationalist constraint vanished overnight, state capital would reallocate toward market-return sectors, defense procurement would shrink, import substitution would reverse, and the industrial structure would shift from strategic priority toward consumer demand.
% FOUNDING_PROBLEM: Post-war and early-reform vulnerability to foreign technology dominance and supply interruption in sectors critical to military capacity and economic sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: Defense strategists and geopolitical analysts attest the vulnerability remains live. Heterodox and liberal economists outside the beneficiary set argue the threat is overstated and the founding problem has mutated into a rent-preservation narrative; no consensus corroboration exists from non-beneficiaries.
narrative_ontology:disappearance_verdict(performance_legitimacy__techno_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__techno_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__techno_nationalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(performance_legitimacy__techno_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__techno_nationalist_reading, 0.75, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.75) is high because massive resource transfers are politically directed and decoupled from market returns. Suppression (0.70) reflects active enforcement through export controls, investment screening, procurement exclusion, and administrative credit allocation. Theater ratio (0.48) captures performative self-reliance narratives and patriotic technology campaigns that partially obscure innovation shortfalls. Accessibility collapse (0.60) registers that market-allocation alternatives are partially collapsed but not eliminated â informal and gray-market channels persist. Resistance (0.55) captures ongoing debate among economists and consumer-sector firms. The temporal series tracks intensification from early reform through the contemporary geopolitical competition phase on a single shared grid.
 *
 * PERSPECTIVAL GAP:
 *   The state-strategic-planner seat experiences the constraint as necessary coordination against existential vulnerability and as a source of consolidated legitimacy. The consumer-sector, market-oriented-enterprise, and household-consumer seats experience it as extraction that taxes their viability and welfare for uncertain security returns. The engine computes this divergence from beneficiary status, constrained exit, and active enforcement without requiring a unified classification.
 *
 * DIRECTIONALITY LOGIC:
 *   State planners sit near the beneficiary end (low d): they gain consolidated legitimacy and policy control. Defense-tech and national-champion firms sit at the beneficiary end (low d): they receive subsidies, protection, and directed credit. Consumer-sector enterprises, market-oriented enterprises, and household consumers sit near the target end (high d): they pay through suppressed market opportunity, higher prices, and foregone consumption. International competitors are excluded rather than governed â their exclusion is the enforcement boundary itself.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying as tangled rope prevents two errors: (1) treating the constraint as a snare would miss the genuine coordination problem of supply-chain vulnerability and high-risk R&D underinvestment in a geopolitically competitive environment; (2) treating it as a rope would miss the asymmetric extraction that redirects resources from consumer welfare and market-driven allocation to state-favored sectors regardless of cost-benefit signals. The active enforcement requirement and the coexistence of concentrated beneficiaries with identifiable victims enforce the hybrid classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does performance legitimacy ultimately reduce to techno-nationalist achievement, or do sibling readings (growth, welfare, quality) represent independent structural constraints on the same kernel?',
    'Comparative policy analysis across jurisdictions or historical phases where one reading dominated; observing whether the same institutional arrangement can hold multiple readings simultaneously without contradiction.',
    'If sibling readings are structurally independent, this constraint is one component of a decomposable kernel; if they are mutually exclusive, the kernel''s instability drives higher extraction as the regime fuses legitimacy to a single reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural relationship between techno-nationalist reading and sibling performance legitimacy readings').

omega_variable(
    security_threat_empirical_basis,
    'Is the foreign technology dependency threat that justifies the constraint empirically calibrated, or is it systematically overstated to sustain resource transfers?',
    'Independent security audits of supply chain vulnerability; cross-national comparison of actual versus perceived dependency risks; analysis of procurement decisions for coherence with stated threat.',
    'If overstated, the coordination function is cover for extraction and effective epsilon rises; if calibrated, the extraction is the necessary price of security.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_threat_empirical_basis, empirical, 'Empirical veracity of the security threat motivating techno-nationalist industrial policy').

omega_variable(
    market_mechanism_viability,
    'Could market signals alone achieve strategic technology autonomy, making state direction unnecessary extraction rather than necessary coordination?',
    'Historical counterfactual analysis of strategic technology development in market-driven versus state-directed systems; evaluation of market failures in high-risk R&D.',
    'If market mechanisms are viable, this constraint is extractive snare; if market failure is genuine, the constraint remains tangled rope with real coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(market_mechanism_viability, conceptual, 'Whether market allocation could solve the strategic technology problem without state direction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__techno_nationalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perfleg_tech_tr_t0, performance_legitimacy__techno_nationalist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(perfleg_tech_tr_t8, performance_legitimacy__techno_nationalist_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(perfleg_tech_tr_t16, performance_legitimacy__techno_nationalist_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(perfleg_tech_tr_t24, performance_legitimacy__techno_nationalist_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(perfleg_tech_tr_t32, performance_legitimacy__techno_nationalist_reading, theater_ratio, 32, 0.44).
narrative_ontology:measurement(perfleg_tech_tr_t40, performance_legitimacy__techno_nationalist_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(perfleg_tech_be_t0, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(perfleg_tech_be_t8, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(perfleg_tech_be_t16, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(perfleg_tech_be_t24, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(perfleg_tech_be_t32, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 32, 0.7).
narrative_ontology:measurement(perfleg_tech_be_t40, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 40, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(perfleg_tech_su_t0, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(perfleg_tech_su_t8, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(perfleg_tech_su_t16, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(perfleg_tech_su_t24, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 24, 0.69).
narrative_ontology:measurement(perfleg_tech_su_t32, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 32, 0.74).
narrative_ontology:measurement(perfleg_tech_su_t40, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 40, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__techno_nationalist_reading, resource_allocation).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, performance_legitimacy__quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, performance_legitimacy__qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, performance_legitimacy__livelihood_security_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the performance_legitimacy kernel. The kernel decomposes into four structurally distinct constraints because the same legitimacy claim ('performance') supports divergent resource allocations: growth-maximizing, welfare-oriented, quality-transformative, and techno-nationalist. Each reading has a different beneficiary/victim structure and epsilon value. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
