% ============================================================================
% CONSTRAINT STORY: climate_response_action__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_action__mitigation_priority, []).

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
 *   constraint_id: climate_response_action__mitigation_priority
 *   human_readable: Mitigation-First Climate Response with Carbon Markets and Green Growth
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint instantiates the mitigation_priority reading of the
 *   climate_response_action kernel. It treats climate change as a problem
 *   solvable through emission reductions under existing capitalist growth
 *   dynamics, using carbon markets and technological innovation as primary
 *   instruments. The framework is formally institutionalized through the
 *   UNFCCC and Paris Agreement, but its operation asymmetrically concentrates
 *   costs on current high-emitting workforces, climate-vulnerable regions,
 *   the Global South, and future generations, while benefits accrue to
 *   innovation-capable nations and carbon-market financial actors.
 *
 * KEY AGENTS:
 *   - Innovation-capable nations: Primary beneficiary (institutional/arbitrage) â capture policy space, set carbon market rules, and delay binding adaptation finance.
 *   - Carbon market financial actors: Secondary beneficiary (powerful/arbitrage) â extract rents from carbon trading and offset mechanisms.
 *   - Climate vulnerable regions: Primary target (powerless/trapped) â bear deferred adaptation costs and loss-and-damage with limited exit.
 *   - Future generations: Primary target (powerless/trapped/universal scope) â inherit residual climate risk and overshoot assumptions without representation.
 *   - Global South nations: Secondary target (moderate/constrained) â locked into mitigation obligations with inadequate finance flows.
 *   - High-emitting sector workforces: Tertiary target (moderate/constrained) â bear transition costs without guaranteed social protection.
 *   - UNFCCC negotiation framework: Agenda setter (institutional/analytical) â administers NDCs and Article 6 carbon markets.
 *   - Climate justice movements: Analytical observer (organized/analytical) â resist the framework from outside formal negotiation halls.
 *   - Degrowth advocates: Excluded voice (moderate/constrained) â structurally absent from ministerial agenda-setting despite formal observer status.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_action__mitigation_priority, 0.68).
domain_priors:suppression_score(climate_response_action__mitigation_priority, 0.62).
domain_priors:theater_ratio(climate_response_action__mitigation_priority, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_action__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_action__mitigation_priority, "Mitigation-First Climate Response with Carbon Markets and Green Growth").
narrative_ontology:topic_domain(climate_response_action__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_action__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_action__mitigation_priority, 'b4f48637-9aa9-49fb-b587-0309ce5bb04d').
narrative_ontology:cs_kernel_codification('b4f48637-9aa9-49fb-b587-0309ce5bb04d', formalized).
narrative_ontology:cs_authority_grounding('b4f48637-9aa9-49fb-b587-0309ce5bb04d', expertise).
narrative_ontology:cs_interpretation_layer_present('b4f48637-9aa9-49fb-b587-0309ce5bb04d').
narrative_ontology:cs_reading_relation('b4f48637-9aa9-49fb-b587-0309ce5bb04d', climate_response_action__adaptation_priority, influences).
narrative_ontology:cs_reading_relation('b4f48637-9aa9-49fb-b587-0309ce5bb04d', climate_response_action__degrowth_transformation, forecloses).
narrative_ontology:cs_axiom('b4f48637-9aa9-49fb-b587-0309ce5bb04d', foundational, growth_compatible_mitigation).
narrative_ontology:cs_axiom_status(growth_compatible_mitigation, holdable).
narrative_ontology:cs_axiom_grounding('b4f48637-9aa9-49fb-b587-0309ce5bb04d', growth_compatible_mitigation, instrumental).
narrative_ontology:cs_axiom('b4f48637-9aa9-49fb-b587-0309ce5bb04d', foundational, technological_substitution_sufficiency).
narrative_ontology:cs_axiom_status(technological_substitution_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('b4f48637-9aa9-49fb-b587-0309ce5bb04d', technological_substitution_sufficiency, empirically_contingent).
narrative_ontology:cs_reference_frame('b4f48637-9aa9-49fb-b587-0309ce5bb04d', growth_compatible_2c_stabilization).
narrative_ontology:cs_drift_state('b4f48637-9aa9-49fb-b587-0309ce5bb04d', contemporary_implementation_gap, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b4f48637-9aa9-49fb-b587-0309ce5bb04d', '').
narrative_ontology:cs_kernel_id(climate_response_action__mitigation_priority, climate_response_action).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, innovation_capable_nations).
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, carbon_market_financial_actors).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, high_emitting_sector_workforces).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, climate_vulnerable_regions).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, global_south_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control UNFCCC negotiation agendas, climate finance architecture, and technology transfer frameworks. Benefit from mitigation-priority framing that delays binding adaptation finance and preserves policy space for gradual decarbonization compatible with existing economic structures. Their exit is arbitrage-grade because they design the rules.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, innovation_capable_nations, beneficiary,
    institutional, generational, arbitrage, global).

% Create, trade, and verify carbon credits, offsets, and derivatives. Benefit from market complexity, lax accounting baselines, and the presumption that atmospheric carbon can be fungibly priced and temporally displaced through future removal technologies.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, carbon_market_financial_actors, beneficiary,
    powerful, biographical, arbitrage, global).

% Bear concentrated costs of rapid sectoral transition in fossil fuel extraction, heavy industry, and carbon-intensive agriculture. Face job displacement and regional economic decline without guaranteed social protection or retraining scale adequate to the transition speed the framework demands.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, high_emitting_sector_workforces, payer,
    moderate, biographical, constrained, national).

% Inhabit geographies already experiencing loss and damage from locked-in warming. Their adaptation needs are chronically underfunded because the framework prioritizes emission reductions and carbon market integrity over direct resilience investment, leaving them exposed to climate impacts they did not cause.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, climate_vulnerable_regions, payer,
    powerless, generational, trapped, regional).

% Inherit the atmospheric consequences of current mitigation gaps and the assumption that carbon removal technologies will eventually compensate for overshoot. They have no seat at the negotiating table and no institutional mechanism to reject the debt-like transfer of climate risk.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Expected to constrain development within carbon budgets while promised technology transfer and climate finance remain largely unfulfilled. Locked into NDC frameworks that assume Northern-financed green transition, but the finance flows are insufficient and debt-burdened, forcing a choice between compliance and development.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, global_south_nations, payer,
    moderate, generational, constrained, global).

% Administers the Paris Agreement NDC cycle, Article 6 carbon market mechanisms, and global stocktake. Sets the accounting rules, baseline methodologies, and timetables that determine whose emissions count and whose adaptation needs are deferred. Its authority derives from the consensual intergovernmental process.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, unfccc_negotiation_framework, agenda_setter,
    institutional, generational, analytical, global).

% Operate outside formal negotiation halls to analyze and resist the mitigation-priority framework. Argue that it preserves colonial economic relations, denies historical responsibility, and substitutes market mechanisms for structural redistribution.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, climate_justice_movements, observer,
    organized, generational, analytical, global).

% Argue for structural economic transformation rejecting GDP growth as an organizing principle. Are structurally excluded from UNFCCC ministerial dialogues and IPCC working-group primacy where green-growth assumptions and technological optimism dominate the analytic baseline.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, degrowth_advocates, excluded,
    moderate, generational, constrained, global).

narrative_ontology:fixing_cost_class(climate_response_action__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global emission reductions toward a shared temperature limit through nationally determined contributions, carbon pricing, and technology transfer frameworks, addressing the collective-action problem of atmospheric commons degradation.
% TRANSFER_FUNCTION: Moves the immediate costs of emission reductions and the deferred costs of residual climate risk from innovation-capable nations and current generations to high-emitting workforces, climate-vulnerable regions, the Global South, and future generations, while moving carbon market rents and technology licensing revenues to financial actors and developed economies.
% ABSENT_VOICES: Degrowth advocates and adaptation-priority constituencies from vulnerable regions are formally present in UNFCCC observer processes but structurally excluded from ministerial agenda-setting and IPCC working-group primacy where green-growth and mitigation-accounting frameworks dominate.
% DISAPPEARANCE_RATIONALE: If the mitigation-priority framework vanished overnight, NDC accounting architectures, Article 6 carbon market mechanisms, and green technology transfer structures would collapse. Climate finance would reallocate toward adaptation and loss-and-damage, fossil fuel transition timelines and burden-sharing formulas would renegotiate, and the Global North-South bargaining dynamic would reconstitute around different principles.
% FOUNDING_PROBLEM: Atmospheric carbon concentrations constitute a global commons problem where uncoordinated national emissions aggregate to dangerous climate change, requiring internationally coordinated limitation of temperature rise.
% FOUNDING_PROBLEM_CORROBORATION: IPCC working groups and earth-system scientists attest the physical risk from outside the benefiting parties; however, the specific mitigation-priority framing as the appropriate response is contested by Global South negotiators, climate justice movements, and degrowth scholars outside the carbon-market beneficiary set.
narrative_ontology:disappearance_verdict(climate_response_action__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_action__mitigation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_action__mitigation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_action__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_action__mitigation_priority, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_action__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_action__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_action__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) is high because the framework externalizes substantial climate costs to parties with weak exit options while preserving growth-compatible policy space for wealthy nations. Suppression (0.62) reflects the structural marginalization of degrowth and adaptation-priority alternatives in UNFCCC and IPCC processes. Theater ratio (0.45) captures the growing performative gap between pledged NDCs and delivered emissions reductions, alongside questionable carbon offset accounting. Accessibility collapse (0.55) indicates that while alternatives exist intellectually, they are structurally excluded from policy primacy. Resistance (0.58) reflects organized climate justice and Global South bloc opposition. The measurement series run on one shared time grid tracking the financialization and performative inflation of the framework from early Kyoto-era mechanisms to the post-Paris gap period.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (UNFCCC framework) and beneficiary seats (innovation-capable nations, financial actors) experience this constraint as genuine coordination solving a commons dilemma. The target seats (vulnerable regions, future generations, Global South, transitioning workforces) experience the same structure as extraction that preserves Northern economic privileges and temporally displaces costs. The engine computes this divergence from the structural asymmetry in power, exit options, and scope; the authored claim (tangled_rope) does not adjudicate the seat-level contradiction but names the hybrid structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Innovation-capable nations and carbon market financial actors are structural beneficiaries with arbitrage-grade exit and global scope, placing them near the full-beneficiary end (low d). Climate vulnerable regions and future generations are structurally trapped, powerless, and at universal scope, placing them near the full-target end (high d), where effective extraction is amplified by scope and powerlessness. Global South nations and high-emitting workforces sit in the middle-high range: some institutional voice but heavily constrained exit. The beneficiary-victim declarations plus exit options drive the derivation without overrides.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â uncoordinated atmospheric commons degradation â remains physically live, so mandatrophy_resolved is not declared. The framework avoids piton classification because its coordination function (emissions accounting, NDC aggregation) is still operationally active, not purely theatrical. However, the rising theater_ratio trajectory signals Goodhart drift: as the gap between pledges and reality widens, proxy goals (carbon market liquidity, NDC submission rates) threaten to replace the original temperature-limit function. If technological substitution proves empirically insufficient and the framework persists on accounting innovations alone, it would degrade toward piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mitigation_priority_kernel_reading,
    'This constraint instantiates the mitigation_priority reading of the climate_response_action kernel; would an adaptation_priority or degrowth_transformation reading reallocate the beneficiary-victim structure or alter the epsilon referent?',
    'Comparative structural analysis across the constraint family for this kernel, examining cost allocation and authority distribution under each reading.',
    'Adoption of adaptation_priority would shift classification toward scaffold or rope by reallocating extraction to present resilience investors; degrowth_transformation would likely classify as snare or mountain depending on enforcement assumptions. The directionality of Global South and future generations would invert or flatten.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mitigation_priority_kernel_reading, conceptual, 'Kernel reading position and structural deltas to sibling readings').

omega_variable(
    technological_substitution_sufficiency,
    'Will carbon removal and green innovation scale sufficiently to achieve the 2Â°C target without structural economic transformation, or does this reading depend on empirically false optimism?',
    'Empirical tracking of carbon removal deployment rates, green growth decoupling evidence, and residual emissions pathways against IPCC scenario assumptions.',
    'If the technological premise is falsified, the axiom of growth-compatible mitigation collapses, and the constraint''s extraction component dominates its coordination function, pushing effective classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_substitution_sufficiency, empirical, 'Whether technological optimism underlying the reading is empirically warranted').

omega_variable(
    growth_compatibility_ambiguity,
    'Is GDP growth maintenance a genuinely separable instrumental goal of climate response, or is it a structural feature that constrains mitigation to paths compatible with existing power relations?',
    'Analysis of historical decoupling rates, resource throughput trajectories, and the political economy of NDC ambition to determine whether growth compatibility is a constraint or a cover story.',
    'If growth compatibility is structurally inseparable from the constraint''s operation, then the coordination function is partially subordinated to Northern economic interests, confirming the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_compatibility_ambiguity, conceptual, 'Whether growth maintenance is instrumental or structurally extractive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_action__mitigation_priority, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cr_mitigation_tr_t0, climate_response_action__mitigation_priority, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cr_mitigation_tr_t5, climate_response_action__mitigation_priority, theater_ratio, 5, 0.3).
narrative_ontology:measurement(cr_mitigation_tr_t10, climate_response_action__mitigation_priority, theater_ratio, 10, 0.35).
narrative_ontology:measurement(cr_mitigation_tr_t15, climate_response_action__mitigation_priority, theater_ratio, 15, 0.38).
narrative_ontology:measurement(cr_mitigation_tr_t20, climate_response_action__mitigation_priority, theater_ratio, 20, 0.42).
narrative_ontology:measurement(cr_mitigation_tr_t25, climate_response_action__mitigation_priority, theater_ratio, 25, 0.44).
narrative_ontology:measurement(cr_mitigation_tr_t30, climate_response_action__mitigation_priority, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(cr_mitigation_be_t0, climate_response_action__mitigation_priority, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(cr_mitigation_be_t5, climate_response_action__mitigation_priority, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(cr_mitigation_be_t10, climate_response_action__mitigation_priority, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(cr_mitigation_be_t15, climate_response_action__mitigation_priority, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(cr_mitigation_be_t20, climate_response_action__mitigation_priority, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(cr_mitigation_be_t25, climate_response_action__mitigation_priority, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(cr_mitigation_be_t30, climate_response_action__mitigation_priority, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cr_mitigation_su_t0, climate_response_action__mitigation_priority, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(cr_mitigation_su_t5, climate_response_action__mitigation_priority, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(cr_mitigation_su_t10, climate_response_action__mitigation_priority, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(cr_mitigation_su_t15, climate_response_action__mitigation_priority, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(cr_mitigation_su_t20, climate_response_action__mitigation_priority, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(cr_mitigation_su_t25, climate_response_action__mitigation_priority, suppression_requirement, 25, 0.6).
narrative_ontology:measurement(cr_mitigation_su_t30, climate_response_action__mitigation_priority, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_action__mitigation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_action__mitigation_priority, 0.15).
narrative_ontology:affects_constraint(climate_response_action__mitigation_priority, climate_response_action__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_action__mitigation_priority, climate_response_action__degrowth_transformation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the climate_response_action kernel. The mitigation_priority reading treats the referent as the standing UNFCCC mitigation architecture with carbon markets and green growth assumptions. Sibling readings differ in epsilon because they model different standing arrangements: adaptation_priority reallocates to resilience infrastructure, while degrowth_transformation rejects the growth premise entirely. Decomposition follows the epsilon-invariance principle: the same colloquial label 'climate response' covers structurally distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
