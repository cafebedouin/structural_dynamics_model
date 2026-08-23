% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__portfolio_optimization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_imperative__portfolio_optimization_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: climate_mitigation_imperative__portfolio_optimization_reading
 *   human_readable: Portfolio Optimization Reading of Climate Mitigation Imperative
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint story represents the portfolio_optimization_reading of
 *   the climate_mitigation_imperative kernel. The reading asserts that
 *   effective climate mitigation requires a technology-neutral approach that
 *   maximizes deployment of all low-carbon sources, and that nuclear energy
 *   is necessary to provide reliable baseload power complementing variable
 *   renewables. The constraint operates through policy mechanisms (subsidies,
 *   clean energy standards, licensing reform) that channel support to nuclear
 *   while suppressing fossil fuels. The claimed_type is rope (genuine
 *   coordination), but the authored metrics show moderate extraction and
 *   suppression, reflecting the contested nature of whether nuclear supports
 *   are coordination costs or rents. The engine will compute per-seat
 *   classifications from the structural data.
 *
 * KEY AGENTS:
 *   - nuclear_industry: Primary beneficiary (powerful/arbitrage) — receives subsidies, loan guarantees, liability caps
 *   - fossil_fuel_industry: Primary victim (powerful/constrained) — loses market share and asset value
 *   - grid_operators: Beneficiary (institutional/analytical) — gain reliability tool
 *   - ratepayers: Payer (organized/constrained) — bear cost of subsidies and potential stranded assets
 *   - renewable_energy_industry: Dual (powerful/constrained) — benefit from low-carbon mandate but face nuclear competition
 *   - climate_vulnerable_populations: Beneficiary (powerless/trapped) — benefit from mitigation
 *   - policy_makers: Agenda setter (institutional/analytical) — design and enforce portfolio standards
 *   - anti_nuclear_activists: Excluded (organized/trapped) — oppose nuclear on safety/waste grounds
 *   - independent_energy_analysts: Observer (analytical/analytical) — evaluate system costs and reliability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__portfolio_optimization_reading, 0.55).
domain_priors:suppression_score(climate_mitigation_imperative__portfolio_optimization_reading, 0.45).
domain_priors:theater_ratio(climate_mitigation_imperative__portfolio_optimization_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__portfolio_optimization_reading, rope).
narrative_ontology:human_readable(climate_mitigation_imperative__portfolio_optimization_reading, "Portfolio Optimization Reading of Climate Mitigation Imperative").
narrative_ontology:topic_domain(climate_mitigation_imperative__portfolio_optimization_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__portfolio_optimization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__portfolio_optimization_reading, '09a5fef3-ea40-450b-929b-4a7e334d6361').
narrative_ontology:cs_kernel_codification('09a5fef3-ea40-450b-929b-4a7e334d6361', formalized).
narrative_ontology:cs_authority_grounding('09a5fef3-ea40-450b-929b-4a7e334d6361', expertise).
narrative_ontology:cs_interpretation_layer_present('09a5fef3-ea40-450b-929b-4a7e334d6361').
narrative_ontology:cs_reading_relation('09a5fef3-ea40-450b-929b-4a7e334d6361', climate_mitigation_imperative__opportunity_cost_reading, influences).
narrative_ontology:cs_reading_relation('09a5fef3-ea40-450b-929b-4a7e334d6361', climate_mitigation_imperative__systems_transition_reading, influences).
narrative_ontology:cs_axiom('09a5fef3-ea40-450b-929b-4a7e334d6361', foundational, technology_neutral_decarbonization).
narrative_ontology:cs_axiom_status(technology_neutral_decarbonization, holdable).
narrative_ontology:cs_axiom_grounding('09a5fef3-ea40-450b-929b-4a7e334d6361', technology_neutral_decarbonization, empirically_contingent).
narrative_ontology:cs_axiom('09a5fef3-ea40-450b-929b-4a7e334d6361', foundational, nuclear_baseload_necessity).
narrative_ontology:cs_axiom_status(nuclear_baseload_necessity, holdable).
narrative_ontology:cs_axiom_grounding('09a5fef3-ea40-450b-929b-4a7e334d6361', nuclear_baseload_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('09a5fef3-ea40-450b-929b-4a7e334d6361', integrated_resource_planning_paradigm).
narrative_ontology:cs_drift_state('09a5fef3-ea40-450b-929b-4a7e334d6361', contemporary_net_zero_policy_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('09a5fef3-ea40-450b-929b-4a7e334d6361', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, grid_operators).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_industry).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, ratepayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, renewable_energy_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, climate_vulnerable_populations).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, renewable_energy_industry).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__portfolio_optimization_reading, carbon_intensity_neutrality).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__portfolio_optimization_reading, baseload_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives production tax credits, loan guarantees, liability limitation (Price-Anderson), and streamlined licensing. Uses political influence to maintain and expand supports. Can exit by moving capital to other sectors or jurisdictions, but has sunk costs in nuclear supply chain.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_industry, beneficiary,
    powerful, biographical, mobile, national).

% Loses market share to mandated low-carbon portfolio. Faces stranded asset risk. Can partially exit by diversifying into renewables, but core business model is threatened. Uses lobbying and litigation to delay displacement.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_industry, payer,
    powerful, biographical, constrained, national).

% Gain a dispatchable, carbon-free baseload resource that simplifies reliability planning. Their mandate is reliability; nuclear reduces the need for massive storage overbuild. They are not direct financial beneficiaries but gain operational certainty.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, grid_operators, beneficiary,
    institutional, generational, analytical, national).

% Bear the cost of nuclear subsidies through electricity surcharges and tax expenditures. Also bear risk of cost overruns and waste liabilities. Exit is limited: can reduce consumption or install distributed generation, but grid connection remains mandatory for most.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, ratepayers, payer,
    organized, biographical, constrained, national).

% Benefits from the overarching low-carbon mandate and clean energy standards. However, faces competition from subsidized nuclear for policy support and market share. Capital allocation is constrained by investor expectations and policy signals.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, renewable_energy_industry, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__portfolio_optimization_reading, renewable_energy_industry, payer).

% Benefit from any effective mitigation. Have no voice in energy portfolio decisions. Bear disproportionate climate impacts if mitigation fails. Cannot exit the climate system; their structural position is pure beneficiary with zero agency.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, climate_vulnerable_populations, beneficiary,
    powerless, generational, trapped, global).

% Design and enforce the portfolio optimization constraint through legislation, regulation, and appropriations. Capture political benefits from appearing to act on climate while supporting incumbent energy interests. Their exit is analytical: they can change the constraint but face electoral and institutional incentives to maintain it.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, policy_makers, agenda_setter,
    institutional, generational, analytical, national).

% Oppose nuclear on safety, waste, proliferation, and cost grounds. Are structurally excluded from the portfolio optimization framing, which treats nuclear as a neutral low-carbon option. Their opposition is overridden by the constraint's technology-neutral premise. They bear waste/safety risks if nuclear expands but cannot influence the portfolio mandate.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, anti_nuclear_activists, excluded,
    organized, biographical, trapped, national).

% Evaluate system costs, reliability, and emissions outcomes under different portfolio scenarios. Provide evidence for all three readings. Have no material stake in the constraint's operation; their exit is analytical (they can change their assessment).
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, independent_energy_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_industry).
narrative_ontology:fixing_cost_class(climate_mitigation_imperative__portfolio_optimization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensuring reliable electricity supply while decarbonizing by maintaining a technology-neutral portfolio that includes nuclear baseload.
% TRANSFER_FUNCTION: Moves public funds (subsidies, ratepayer surcharges, tax expenditures) to nuclear industry; moves market share and asset value from fossil fuels to low-carbon sources including nuclear.
% ABSENT_VOICES: Communities affected by nuclear waste and uranium mining (often Indigenous and low-income); future generations bearing long-term waste management costs; distributed energy advocates who argue for decentralized alternatives; Global South nations pressured to adopt nuclear under technology-neutral frameworks without domestic capacity.
% DISAPPEARANCE_RATIONALE: If the portfolio optimization mandate vanished overnight, nuclear would lose policy support and likely decline relative to cheaper renewables+storage; fossil fuels would persist longer in the mix; the low-carbon transition would proceed on a different trajectory with different reliability investments and different distributional outcomes.
% FOUNDING_PROBLEM: The need to decarbonize electricity systems while maintaining grid reliability, given the intermittency of wind and solar and the perceived limits of storage and demand response at scale.
% FOUNDING_PROBLEM_CORROBORATION: Grid operators and some climate scientists (e.g., IPCC WGIII scenarios) corroborate the reliability challenge at high renewable penetrations. Renewable advocates, storage developers, and systems analysts contest it, citing falling storage costs, grid integration solutions, and demand-side flexibility. No consensus exists outside the nuclear-benefiting parties (nuclear industry, some grid operators, some governments).
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__portfolio_optimization_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__portfolio_optimization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__portfolio_optimization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_imperative__portfolio_optimization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__portfolio_optimization_reading, 0.55, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_imperative__portfolio_optimization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_imperative__portfolio_optimization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_imperative__portfolio_optimization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) reflects the transfer of public funds and ratepayer surcharges to nuclear industry, which is substantial but framed as coordination cost. Suppression (0.45) reflects the constraint's active displacement of fossil fuels and potential crowding-out of other low-carbon options. Theater_ratio (0.20) is low because the reliability coordination function is genuinely performed by nuclear plants where they operate. Accessibility_collapse (0.60) is moderate: alternatives exist but are constrained by the baseload necessity claim. Resistance (0.50) is moderate: fossil fuel industry and anti-nuclear groups resist, but policy momentum is strong. The measurement series shows extractiveness and theater rising over the interval as nuclear support policies expand and the portfolio mandate tightens.
 *
 * PERSPECTIVAL GAP:
 *   The nuclear_industry and policy_maker seats experience this as genuine coordination (rope): they see a real reliability problem solved by technology-neutral policy. The fossil_fuel_industry and ratepayer seats experience it as extraction (snare/tangled_rope): they see a targeted subsidy regime displacing competitive alternatives. The renewable_energy_industry seat is ambivalent: coordination benefit from the carbon mandate, extraction cost from nuclear favoritism. The engine computes this divergence from the declared structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear_industry and grid_operators are structural beneficiaries (d near 0.0) — they receive direct subsidies and operational advantages. Fossil_fuel_industry is a structural target (d near 1.0) — the constraint explicitly aims to displace them. Ratepayers are payers (d ~0.7) — they bear costs through surcharges and taxes. Renewable_energy_industry is near symmetric (d ~0.5) — they gain from the low-carbon mandate but lose market share to subsidized nuclear. Climate_vulnerable_populations are beneficiaries (d ~0.1) — they gain from mitigation but have no voice. Policy_makers are agenda_setters (d ~0.2) — they design the constraint and capture political benefits. Anti_nuclear_activists are excluded (d ~0.8) — they bear waste/safety risks without influence. Independent_analysts are observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reliability under decarbonization) remains contested. If storage and grid integration solve reliability, the constraint's coordination function atrophies and it becomes a piton (inertial maintenance of nuclear subsidies). Currently the mandate is live, but the drift_state shows authority_erosion as distributed energy paradigms gain traction. The constraint is not yet mandatrophic but shows early signs of mandate drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_committer_structure,
    'This constraint is one reading (portfolio_optimization_reading) of the contested kernel climate_mitigation_imperative. What structural elements differ between this reading and its siblings (opportunity_cost_reading, systems_transition_reading)?',
    'Compare beneficiary/victim sets, extraction metrics, and coordination functions across the three readings. The kernel context declares the expected structural delta for this reading: nuclear enters beneficiary set, fossil fuels are primary victim, constraint is technology-neutral carbon intensity.',
    'If the structural delta holds, this reading instantiates a distinct constraint with its own ε and classification. If the delta collapses, the readings may not be structurally distinct constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_committer_structure, conceptual, 'Commitment structure: kernel identity and reading differentiation').

omega_variable(
    baseload_necessity,
    'Is nuclear baseload genuinely necessary for grid reliability, or can alternatives (storage, demand response, interconnection, overbuilding renewables) provide equivalent reliability at lower system cost?',
    'Empirical analysis of grid reliability metrics in high-renewable systems with and without nuclear; cost-optimization modeling under technology-neutral carbon constraints.',
    'If baseload necessity is falsified, the coordination function claimed by this reading collapses and the constraint reclassifies toward snare (extraction without coordination). If validated, the coordination function stands and the reading remains rope or tangled_rope depending on extraction asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(baseload_necessity, empirical, 'Whether the claimed coordination function (nuclear baseload necessity) is empirically warranted').

omega_variable(
    subsidy_extraction_boundary,
    'Are the policy supports directed to nuclear (subsidies, loan guarantees, liability caps, streamlined licensing) properly characterized as coordination costs (necessary to overcome first-mover barriers) or as extraction (rent transfer to nuclear industry)?',
    'Cost-benefit analysis of nuclear support policies compared to support for other low-carbon technologies; counterfactual modeling of nuclear deployment without targeted supports.',
    'If supports are extraction, the constraint is tangled_rope (coordination + asymmetric extraction). If supports are coordination costs, the constraint is rope. The boundary determines whether the beneficiary set (nuclear_industry) is a genuine coordination beneficiary or an extractive capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidy_extraction_boundary, conceptual, 'Whether nuclear policy supports are coordination costs or extractive rents').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__portfolio_optimization_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(climate_mitigation_imperative__portfolio_optimization_reading_tr_t0, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(climate_mitigation_imperative__portfolio_optimization_reading_tr_t10, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(climate_mitigation_imperative__portfolio_optimization_reading_tr_t20, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(climate_mitigation_imperative__portfolio_optimization_reading_tr_t30, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(climate_mitigation_imperative__portfolio_optimization_reading_be_t0, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(climate_mitigation_imperative__portfolio_optimization_reading_be_t10, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(climate_mitigation_imperative__portfolio_optimization_reading_be_t20, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(climate_mitigation_imperative__portfolio_optimization_reading_be_t30, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 30, 0.55).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(climate_mitigation_imperative__portfolio_optimization_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__portfolio_optimization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_imperative__portfolio_optimization_reading, 0.15).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative__opportunity_cost_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative__systems_transition_reading).

% DUAL FORMULATION NOTE:
% This reading (portfolio_optimization) and its siblings (opportunity_cost, systems_transition) form a constraint family decomposing the climate_mitigation_imperative kernel. Each reading has a distinct ε: portfolio_optimization shows moderate extraction (nuclear subsidies); opportunity_cost shows low extraction (excludes nuclear); systems_transition shows high extraction (centralized nuclear perpetuates inequality). The ε-invariance principle requires separate stories because the observable (mitigation portfolio composition) yields different extraction depending on which reading's framing is adopted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_imperative__portfolio_optimization_reading, institutional, 0.15).
constraint_indexing:directionality_override(climate_mitigation_imperative__portfolio_optimization_reading, powerful, 0.85).
constraint_indexing:directionality_override(climate_mitigation_imperative__portfolio_optimization_reading, organized, 0.65).
constraint_indexing:directionality_override(climate_mitigation_imperative__portfolio_optimization_reading, powerless, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
