% ============================================================================
% CONSTRAINT STORY: climate_response_action__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_action__adaptation_priority, []).

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
 *   constraint_id: climate_response_action__adaptation_priority
 *   human_readable: Climate Adaptation Priority Reading
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint instantiates the adaptation_priority reading of the
 *   contested climate_response_action kernel. It treats climate response as
 *   an urgent investment problem in resilience infrastructure and adaptive
 *   capacity, accepting locked-in temperature rise as inevitable and
 *   prioritizing protection of vulnerable populations. The constraint
 *   operates through multilateral climate finance architecture, sovereign
 *   lending for National Adaptation Plans, and a policy discourse that frames
 *   warming as irreversible. Structurally, it coordinates immediate
 *   protection against climate impacts while asymmetrically extracting fiscal
 *   capacity from developing nations, displacing costs to future generations,
 *   and allowing developed nations to defer deeper mitigation or economic
 *   transformation. The $540B annual protection gap and $350B North-South
 *   financing shortfall are not mere market failures but structural features
 *   of the reading: the constraint distributes resilience costs downward
 *   while the benefits of avoided mitigation accrue to present-day developed
 *   economies and private infrastructure contractors.
 *
 * KEY AGENTS:
 *   - Developed nation governments (agenda_setter/beneficiary; institutional/arbitrage) â set multilateral finance terms and avoid decarbonization costs
 *   - Multilateral development banks (agenda_setter/beneficiary; institutional/arbitrage) â administer conditional adaptation lending and expand mandates
 *   - Developing nation governments (payer; organized/constrained) â accept sovereign debt and conditionalities for resilience access
 *   - Vulnerable populations (payer; powerless/trapped) â bear incomplete protection and remaining climate impacts
 *   - Future generations (payer; powerless/trapped) â inherit higher warming from deferred mitigation
 *   - Resilience infrastructure sector (beneficiary; powerful/mobile) â captures contracts from climate finance pipeline
 *   - Climate justice movements (excluded; moderate/constrained) â contest inevitability framing but lack formal decision-seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_action__adaptation_priority, 0.72).
domain_priors:suppression_score(climate_response_action__adaptation_priority, 0.65).
domain_priors:theater_ratio(climate_response_action__adaptation_priority, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, extractiveness, 0.72).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_action__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_action__adaptation_priority, "Climate Adaptation Priority Reading").
narrative_ontology:topic_domain(climate_response_action__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_action__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_action__adaptation_priority, '03288a39-2c6c-4e6b-94f1-08fc71037d97').
narrative_ontology:cs_kernel_codification('03288a39-2c6c-4e6b-94f1-08fc71037d97', distributed).
narrative_ontology:cs_authority_grounding('03288a39-2c6c-4e6b-94f1-08fc71037d97', distributed).
narrative_ontology:cs_reading_relation('03288a39-2c6c-4e6b-94f1-08fc71037d97', climate_response_action__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('03288a39-2c6c-4e6b-94f1-08fc71037d97', climate_response_action__degrowth_transformation, influences).
narrative_ontology:cs_axiom('03288a39-2c6c-4e6b-94f1-08fc71037d97', foundational, warming_inevitability_accepted).
narrative_ontology:cs_axiom_status(warming_inevitability_accepted, holdable).
narrative_ontology:cs_axiom_grounding('03288a39-2c6c-4e6b-94f1-08fc71037d97', warming_inevitability_accepted, empirically_contingent).
narrative_ontology:cs_axiom('03288a39-2c6c-4e6b-94f1-08fc71037d97', foundational, protection_through_infrastructure_priority).
narrative_ontology:cs_axiom_status(protection_through_infrastructure_priority, holdable).
narrative_ontology:cs_axiom_grounding('03288a39-2c6c-4e6b-94f1-08fc71037d97', protection_through_infrastructure_priority, instrumental).
narrative_ontology:cs_reference_frame('03288a39-2c6c-4e6b-94f1-08fc71037d97', multilateral_adaptation_governance).
narrative_ontology:cs_drift_state('03288a39-2c6c-4e6b-94f1-08fc71037d97', post_glasgow_finance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('03288a39-2c6c-4e6b-94f1-08fc71037d97', '').
narrative_ontology:cs_kernel_id(climate_response_action__adaptation_priority, climate_response_action).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, developed_nation_governments).
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, multilateral_development_banks).
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, resilience_infrastructure_sector).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, developing_nation_governments).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, vulnerable_populations).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, future_generations).
narrative_ontology:constraint_vindicates(climate_response_action__adaptation_priority, climate_inevitability_thesis).
narrative_ontology:constraint_vindicates(climate_response_action__adaptation_priority, technocratic_governance_climate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the terms of multilateral climate finance, frame warming as inevitable, and avoid binding emission-reduction or economic-transformation targets. Benefit from deferred decarbonization costs while channeling climate response toward loan-based resilience investment.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, developed_nation_governments, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__adaptation_priority, developed_nation_governments, beneficiary).

% Design and administer adaptation lending instruments, set conditionalities for National Adaptation Plans, expand institutional mandates through climate-resilience portfolios, and collect fees and interest on disbursed finance.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, multilateral_development_banks, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__adaptation_priority, multilateral_development_banks, beneficiary).

% Must submit National Adaptation Plans and accept loan-based or conditional finance to access resilience infrastructure. Bear sovereign debt and domestic fiscal burdens for climate impacts they did little to cause, with limited leverage to alter finance terms.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, developing_nation_governments, payer,
    organized, generational, constrained, national).

% Receive incomplete, delayed, or inequitable resilience infrastructure. Face displacement, loss of livelihood, and uncompensated damages where adaptation fails, while serving as the nominal moral justification for the constraint.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, vulnerable_populations, payer,
    powerless, immediate, trapped, local).

% Inherit higher locked-in warming and compounded climate damages because present adaptation priority defers emission cuts and structural economic transformation. Have no seat at negotiating tables where their costs are distributed.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Wins procurement contracts for climate-resilience projects funded by multilateral and sovereign adaptation finance. Benefits from a sustained pipeline of capital-intensive infrastructure justified by the inevitability framing.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, resilience_infrastructure_sector, beneficiary,
    powerful, biographical, mobile, global).

% Advocate for systemic mitigation, emission cuts, and degrowth transformation. Are formally consulted in multilateral processes but structurally excluded from core finance architecture decisions that treat warming as irreversible.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, climate_justice_movements, excluded,
    moderate, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coordinated multilateral framework for immediate protection of populations against locked-in climate impacts through capital-intensive resilience infrastructure, adaptive capacity building, and sovereign lending instruments.
% TRANSFER_FUNCTION: Moves capital and debt obligations from multilateral institutions and developed nations to developing nations for resilience projects, while moving the cost of deferred mitigation and higher future warming to future generations and vulnerable populations who receive incomplete protection.
% ABSENT_VOICES: Degrowth advocates, radical climate justice movements demanding systemic economic transformation, and future generations are structurally underrepresented in adaptation finance forums; they would contest the inevitability framing and demand emission reductions over resigned infrastructure investment.
% DISAPPEARANCE_RATIONALE: If the adaptation-priority constraint vanished, the $540B annual resilience pipeline would lose its organizing logic, developing nations would abandon National Adaptation Plan conditionalities, multilateral climate finance would collapse or reorient toward mitigation or economic transformation, and developed nations would face immediate pressure for deeper decarbonization.
% FOUNDING_PROBLEM: Climate impacts are already causing loss and damage that cannot be prevented by mitigation alone; vulnerable populations need immediate protection against locked-in warming while political and technological systems are too slow to decarbonize rapidly enough.
% FOUNDING_PROBLEM_CORROBORATION: Humanitarian NGOs and IPCC Working Group II attest that immediate protection needs are live. However, climate justice movements, degrowth scholars, and Global South negotiators outside the benefiting parties attest that the 'inevitability' framing is overstated and serves to defer harder mitigation; the problem is partly constructed by political unwillingness rather than purely physical necessity.
narrative_ontology:disappearance_verdict(climate_response_action__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_action__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_action__adaptation_priority, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_action__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_action__adaptation_priority, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_action__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_action__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_action__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint channels developing nations into loan-based adaptation finance, creates protection disparities where vulnerable populations receive incomplete coverage, and locks in higher future warming costs. Suppression (0.65) reflects the marginalization of degrowth and radical mitigation voices in multilateral fora, plus the structural exclusion of future generations from negotiation tables. Theater_ratio (0.45) captures the growing gap between pledged adaptation finance and disbursed funds, where performative commitment to 'protection' masks persistent underfunding. Accessibility_collapse (0.60) registers that while alternatives (mitigation-priority, degrowth) are technically speakable, the multilateral architecture has made adaptation-priority the default operating frame for climate finance. Resistance (0.58) reflects Global South bargaining coalitions and climate justice movements that contest the inevitability framing. The measurement series run on one shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (developed nations, MDBs) experience this constraint as necessary coordination to manage irreversible climate impacts and protect the vulnerable. The payer seats (developing nations, vulnerable populations, future generations) experience it as a transfer mechanism that loads climate debt onto those least responsible while protecting existing economic structures. The divergence is structural: same multilateral frameworks, opposite directionalities.
 *
 * DIRECTIONALITY LOGIC:
 *   Developed nations and MDBs sit near the beneficiary end (low d): they control the finance architecture, avoid mitigation costs, and expand institutional mandates. Resilience contractors also benefit. Developing nations, vulnerable populations, and future generations sit near the target end (high d): they bear the debt, the protection gaps, and the deferred warming. Climate justice movements are excluded rather than targeted, sitting outside the d-derivation chain for this constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint risks mandatrophy if the founding problem (immediate protection need) is used to permanently defer mitigation and economic transformation. The reading carries no sunset clause and no mechanism to resolve the underlying emission-driven problem, creating drift toward perpetual adaptation finance without emission reduction. The T17 trigger is relevant: if base_extractiveness continues to rise as warming costs accumulate, the constraint may calcify into a permanent extraction structure where the coordination function is entirely subsumed by debt-loading and protection theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the adaptation-priority reading structurally inevitable, or would a sibling reading (mitigation_priority or degrowth_transformation) produce a fundamentally different distribution of costs and benefits across North/South and present/future?',
    'Comparative institutional analysis of climate finance flows under different national policy mixes; observe whether nations prioritizing mitigation show different North-South cost distributions than those prioritizing adaptation.',
    'If sibling readings produce materially different extraction patterns, the kernel is genuinely contested and this reading''s naturalization of warming inevitability is a political choice, not a physical necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Kernel reading contestation and structural substitutability').

omega_variable(
    north_south_finance_gap,
    'Does the $350B North-South adaptation financing gap reflect genuine fiscal incapacity in developed nations, or is it structural extraction that places the cost of climate resilience on developing nations?',
    'Forensic accounting of developed nation fiscal space versus adaptation pledge fulfillment; sovereign debt analysis of adaptation loans versus grants.',
    'If extraction, the constraint''s classification as tangled_rope is reinforced and the coordination function serves as cover for debt-loading; if capacity, the extraction metric should be revised downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(north_south_finance_gap, empirical, 'North-South finance gap as extraction or capacity limit').

omega_variable(
    inevitability_framing,
    'Is the accepted temperature-rise inevitability grounded in physical climate inertia (scientifically necessitated), or in the political impossibility of mitigation and degrowth within current power structures?',
    'Integrated assessment model comparison across political feasibility constraints; historical analysis of mitigation opportunities foregone for political rather than technical reasons.',
    'If politically constructed, the constraint''s accessibility_collapse metric should reflect lower barrier to alternatives; the coordination story becomes more heavily theatrical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inevitability_framing, conceptual, 'Warming inevitability as physical law or political construct').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_action__adaptation_priority, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(adaptation_priority_tr_t0, climate_response_action__adaptation_priority, theater_ratio, 0, 0.25).
narrative_ontology:measurement(adaptation_priority_tr_t6, climate_response_action__adaptation_priority, theater_ratio, 6, 0.3).
narrative_ontology:measurement(adaptation_priority_tr_t12, climate_response_action__adaptation_priority, theater_ratio, 12, 0.38).
narrative_ontology:measurement(adaptation_priority_tr_t18, climate_response_action__adaptation_priority, theater_ratio, 18, 0.42).
narrative_ontology:measurement(adaptation_priority_tr_t24, climate_response_action__adaptation_priority, theater_ratio, 24, 0.45).

% Extraction over time
narrative_ontology:measurement(adaptation_priority_be_t0, climate_response_action__adaptation_priority, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(adaptation_priority_be_t6, climate_response_action__adaptation_priority, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(adaptation_priority_be_t12, climate_response_action__adaptation_priority, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(adaptation_priority_be_t18, climate_response_action__adaptation_priority, base_extractiveness, 18, 0.65).
narrative_ontology:measurement(adaptation_priority_be_t24, climate_response_action__adaptation_priority, base_extractiveness, 24, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(adaptation_priority_su_t0, climate_response_action__adaptation_priority, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(adaptation_priority_su_t6, climate_response_action__adaptation_priority, suppression_requirement, 6, 0.48).
narrative_ontology:measurement(adaptation_priority_su_t12, climate_response_action__adaptation_priority, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(adaptation_priority_su_t18, climate_response_action__adaptation_priority, suppression_requirement, 18, 0.62).
narrative_ontology:measurement(adaptation_priority_su_t24, climate_response_action__adaptation_priority, suppression_requirement, 24, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(climate_response_action__adaptation_priority, mitigation_priority).
narrative_ontology:affects_constraint(climate_response_action__adaptation_priority, degrowth_transformation).

% DUAL FORMULATION NOTE:
% This constraint is the adaptation_priority reading of kernel climate_response_action. It is decomposed from the colloquial label 'climate response' because the label conflates three structurally distinct claims with different epsilon values, beneficiary structures, and failure modes. Linked to sibling readings mitigation_priority and degrowth_transformation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
