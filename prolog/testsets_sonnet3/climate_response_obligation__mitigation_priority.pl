% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_obligation__mitigation_priority, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: climate_response_obligation__mitigation_priority
 *   human_readable: Mitigation-Priority Reading of the Climate Response Obligation
 *   domain: climate policy / political economy / intergenerational ethics
 *
 * SUMMARY:
 *   This story instantiates the mitigation-priority reading of the contested
 *   climate response obligation kernel: rapid decarbonization is framed as
 *   the primary intergenerational duty, justified by preventing severe future
 *   harm rather than by adapting to warming (the sibling adaptation_priority
 *   reading) or by restructuring material throughput itself (the sibling
 *   degrowth_reading). Under this reading, the coordination function is
 *   genuine — an unmanaged carbon budget produces catastrophic collective
 *   harm — but the burden-sharing and pace are set by institutional
 *   negotiators in ways that concentrate costs on fossil capital,
 *   fossil-dependent workers, and Global North taxpayers relative to their
 *   say in the process, while renewable industry and future generations
 *   capture most of the benefit. The theater ratio starts elevated (0.55)
 *   reflecting early-era pledges (Kyoto-era voluntary targets) with weak
 *   enforcement, and declines over the interval as carbon pricing and binding
 *   sectoral phase-outs (EU ETS tightening, coal phase-out legislation) begin
 *   to bite — a rare case where theater falls while extraction and
 *   enforcement both rise, because the mechanism is maturing from symbolic to
 *   substantive.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__mitigation_priority, 0.58).
domain_priors:suppression_score(climate_response_obligation__mitigation_priority, 0.47).
domain_priors:theater_ratio(climate_response_obligation__mitigation_priority, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__mitigation_priority, "Mitigation-Priority Reading of the Climate Response Obligation").
narrative_ontology:topic_domain(climate_response_obligation__mitigation_priority, "climate policy / political economy / intergenerational ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__mitigation_priority, '19456257-6caa-4e01-bac7-88150d90a10f').
narrative_ontology:cs_kernel_codification('19456257-6caa-4e01-bac7-88150d90a10f', distributed).
narrative_ontology:cs_authority_grounding('19456257-6caa-4e01-bac7-88150d90a10f', distributed).
narrative_ontology:cs_reading_relation('19456257-6caa-4e01-bac7-88150d90a10f', climate_response_obligation__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('19456257-6caa-4e01-bac7-88150d90a10f', climate_response_obligation__degrowth_reading, influences).
narrative_ontology:cs_axiom('19456257-6caa-4e01-bac7-88150d90a10f', foundational, prevention_of_future_harm_takes_priority_over_transition_cost).
narrative_ontology:cs_axiom_status(prevention_of_future_harm_takes_priority_over_transition_cost, holdable).
narrative_ontology:cs_axiom_grounding('19456257-6caa-4e01-bac7-88150d90a10f', prevention_of_future_harm_takes_priority_over_transition_cost, deontological).
narrative_ontology:cs_axiom('19456257-6caa-4e01-bac7-88150d90a10f', foundational, historical_emitters_bear_proportionate_mitigation_burden).
narrative_ontology:cs_axiom_status(historical_emitters_bear_proportionate_mitigation_burden, holdable).
narrative_ontology:cs_axiom_grounding('19456257-6caa-4e01-bac7-88150d90a10f', historical_emitters_bear_proportionate_mitigation_burden, conventional).
narrative_ontology:cs_reference_frame('19456257-6caa-4e01-bac7-88150d90a10f', pre_industrial_carbon_budget_baseline).
narrative_ontology:cs_drift_state('19456257-6caa-4e01-bac7-88150d90a10f', post_paris_agreement_implementation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('19456257-6caa-4e01-bac7-88150d90a10f', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__mitigation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, climate_vulnerable_nations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, renewable_energy_industry).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, fossil_fuel_capital).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, fossil_dependent_workers).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, global_north_taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, global_north_taxpayers).
narrative_ontology:constraint_vindicates(climate_response_obligation__mitigation_priority, intergenerational_justice_doctrine).
narrative_ontology:constraint_vindicates(climate_response_obligation__mitigation_priority, polluter_pays_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cannot participate in current negotiations but inherit whatever climate stability or instability the present generation's decarbonization pace produces. Under this reading they are the primary intended beneficiaries of rapid mitigation, since avoided warming compounds over centuries. They have no voice, no vote, and no exit from the physical trajectory set now.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Low-lying and low-latitude states facing existential exposure to warming they did not cause. Rapid global decarbonization reduces their physical risk, but they have limited leverage in setting the pace or financing of the transition and are frequently talked about rather than negotiated with as equals.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, climate_vulnerable_nations, beneficiary,
    moderate, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__mitigation_priority, climate_vulnerable_nations, excluded).

% Solar, wind, battery, and grid-technology firms whose markets expand directly with mitigation mandates, subsidies, and carbon pricing. They lobby for faster decarbonization timelines and capture much of the public and private capital redirected by the obligation.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, renewable_energy_industry, beneficiary,
    organized, biographical, arbitrage, global).

% Oil, gas, and coal companies and their shareholders whose reserves and infrastructure are devalued or stranded by aggressive decarbonization targets. They can lobby, diversify into renewables, or litigate against phase-out policy, but cannot fully exit the write-down of assets already on their books under this reading's timeline.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, fossil_fuel_capital, payer,
    powerful, biographical, constrained, global).

% Coal miners, oil-field workers, and communities whose local economies depend on extraction industries slated for rapid wind-down. Retraining and just-transition programs are unevenly funded and often lag plant and mine closures; relocation is costly and disrupts family and community ties.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, fossil_dependent_workers, payer,
    powerless, biographical, trapped, regional).

% Citizens of historically high-emitting industrialized states who fund carbon pricing revenue recycling, transition subsidies, and climate finance transfers to the Global South under a polluter-pays allocation. They benefit from long-run climate stability but bear disproportionate near-term fiscal and cost-of-living costs relative to their per-capita share of remaining emissions.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, global_north_taxpayers, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__mitigation_priority, global_north_taxpayers, beneficiary).

% Nations whose development strategy depends on fossil resource extraction and export revenue, distinct from climate-vulnerable states. Rapid global mitigation timelines curtail their growth pathway without necessarily offering compensating investment at comparable scale; their objection that historical emitters industrialized on fossil fuels first is acknowledged rhetorically but rarely resolved in binding terms.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, fossil_exporting_developing_states, excluded,
    moderate, generational, trapped, national).

% UNFCCC bodies, national delegations, and multilateral finance institutions that draft targets, allocate burden-sharing formulas, and enforce (where possible) compliance mechanisms. They administer the mitigation-priority framework, deciding pace, financing structure, and which historical-emissions accounting to use.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, international_climate_negotiators, agenda_setter,
    institutional, generational, analytical, global).

% Provide the physical basis (carbon budgets, warming trajectories) that grounds the mitigation-priority reading's urgency claims. They do not set policy or receive transfers but their models are cited by all sides to justify pace and burden allocation.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, climate_scientists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_obligation__mitigation_priority, diffuse).
narrative_ontology:fixing_cost_class(climate_response_obligation__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global collective-action problem: no single emitter can unilaterally prevent dangerous warming, so the framework sets shared targets, timelines, and burden-sharing formulas to keep cumulative emissions within a carbon budget compatible with avoiding catastrophic warming.
% TRANSFER_FUNCTION: Moves capital and economic activity away from fossil fuel extraction, infrastructure, and employment and toward renewable energy, carbon pricing revenue, and climate finance transfers — primarily from historically high-emitting Global North economies and fossil capital toward climate-vulnerable nations, renewable industries, and future generations who receive a more stable climate.
% ABSENT_VOICES: Future generations cannot object or consent to the pace chosen. Fossil-exporting developing states object that they are asked to forgo the same extraction-led development path industrialized nations already used, but their objection is acknowledged in UNFCCC text (common but differentiated responsibilities) without binding compensation mechanisms.
% DISAPPEARANCE_RATIONALE: If the mitigation-priority obligation vanished overnight, carbon pricing regimes, renewable subsidy structures, coal and oil phase-out commitments, and climate finance transfers would collapse; fossil capital valuations would partially recover, decarbonization investment would slow sharply, and the emissions trajectory would shift toward the adaptation-priority reading's higher-warming baseline.
% FOUNDING_PROBLEM: Anthropogenic greenhouse gas emissions were driving warming that threatened to cross tipping points and impose severe, irreversible harm on future populations and already-vulnerable nations; the arrangement was built to cap cumulative emissions before that harm becomes locked in.
% FOUNDING_PROBLEM_CORROBORATION: Independent physical climate science (IPCC assessment reports, drawing on research bodies with no direct stake in mitigation financing) corroborates that the warming trajectory remains on a harmful path absent continued rapid decarbonization; this attestation comes from the observer seat (climate_scientists), outside the beneficiary set of renewable industry and negotiating institutions.
narrative_ontology:disappearance_verdict(climate_response_obligation__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__mitigation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__mitigation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_obligation__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__mitigation_priority, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_obligation__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises from 0.38 to 0.58 over the interval as mitigation policy moves from aspirational pledges to binding carbon pricing, phase-out mandates, and stranded-asset write-downs — a real transfer from fossil capital and dependent workers toward renewable capital and climate-vulnerable populations. Suppression is moderate (0.47) and rising: it is not coercive in the sense of physical force, but compliance mechanisms (carbon border adjustments, disclosure mandates, litigation exposure for stranded assets) increasingly foreclose the option of continued fossil expansion for regulated actors. Accessibility collapse is moderate (0.42) — some alternative pathways (slower transition, offset-heavy compliance, adaptation-first spending) remain politically available, unlike a true mountain. Resistance is high (0.72): fossil capital, fossil-exporting states, and displaced workers actively contest the framework's pace and burden allocation, which is exactly what a tangled rope predicts — genuine coordination function contested by those who pay asymmetrically through the same structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations and climate-vulnerable nations are structural beneficiaries with essentially zero capacity to influence pace, giving them low derived d despite bearing physical risk if the obligation fails — they benefit FROM the obligation's existence, even though they cannot enforce it. Fossil fuel capital and fossil-dependent workers are declared victims: stranded-asset write-downs and job losses are direct, traceable transfers, giving them high derived d. Global North taxpayers occupy a mixed position — funding the transition while also benefiting from long-run stability — captured via the secondary_role beneficiary/payer split rather than a single override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (averting catastrophic, irreversible warming) remains live by climate science's own assessment, which forecloses a piton or pure-theater classification even though early theater_ratio was high. This distinguishes the mitigation-priority obligation from a mandatrophied constraint: the declining theater trajectory alongside rising real extraction shows the mandate maturing toward its stated function rather than atrophying into performance while extraction quietly continues unchecked.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is mitigation priority the correct reading of the climate response obligation, or do adaptation-priority and degrowth readings better capture what intergenerational justice actually requires given realistic implementation constraints?',
    'Compare realized outcomes across jurisdictions that have pursued different reading-consistent policy mixes (aggressive mitigation vs. resilience investment vs. throughput reduction) against subsequent climate damage and welfare metrics over multi-decade horizons.',
    'If mitigation-priority policies fail to achieve promised warming reductions relative to their transition costs, the tangled_rope classification strengthens toward snare (extraction without delivering the coordination benefit); if they succeed, the rope-like coordination function is vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether mitigation_priority is the structurally correct reading among the three kernel siblings.').

omega_variable(
    historical_emissions_burden_allocation,
    'Does the mitigation-priority reading''s burden allocation (Global North bears disproportionate cost due to historical emissions) reflect a defensible moral accounting, or does it under-weight current per-capita emissions trajectories in rapidly industrializing states?',
    'Independent third-party carbon accounting comparing cumulative historical emissions against current and projected per-capita emissions under multiple allocation formulas (Brazilian Proposal, per-capita convergence, grandfathering).',
    'A finding that current emissions dominate future warming more than historical stock would shift victim/beneficiary weighting away from a pure Global-North-pays framing, altering the directionality derivation for global_north_taxpayers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_emissions_burden_allocation, empirical, 'Whether historical-emissions-based burden sharing is empirically and morally sound under this reading.').

omega_variable(
    stranded_asset_compensation_question,
    'Should fossil fuel capital receive compensation for stranded assets as a matter of just transition, or does the polluter-pays principle vindicated by this reading foreclose compensation claims?',
    'Track litigation outcomes and international arbitration rulings on stranded-asset claims (e.g., Energy Charter Treaty disputes) to see which principle prevails in practice.',
    'If compensation is broadly awarded, fossil_fuel_capital''s victim status is partially offset, reducing measured extractiveness; if compensation is denied, the current high victim weighting is corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stranded_asset_compensation_question, preference, 'Whether stranded fossil assets warrant compensation under the polluter-pays vindicated proposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__mitigation_priority, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_obligation__mitigation_priority, theater_ratio, 0, 0.55).
narrative_ontology:measurement(clim_tr_t6, climate_response_obligation__mitigation_priority, theater_ratio, 6, 0.5).
narrative_ontology:measurement(clim_tr_t12, climate_response_obligation__mitigation_priority, theater_ratio, 12, 0.47).
narrative_ontology:measurement(clim_tr_t18, climate_response_obligation__mitigation_priority, theater_ratio, 18, 0.44).
narrative_ontology:measurement(clim_tr_t24, climate_response_obligation__mitigation_priority, theater_ratio, 24, 0.42).
narrative_ontology:measurement(clim_tr_t30, climate_response_obligation__mitigation_priority, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_obligation__mitigation_priority, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(clim_be_t6, climate_response_obligation__mitigation_priority, base_extractiveness, 6, 0.44).
narrative_ontology:measurement(clim_be_t12, climate_response_obligation__mitigation_priority, base_extractiveness, 12, 0.49).
narrative_ontology:measurement(clim_be_t18, climate_response_obligation__mitigation_priority, base_extractiveness, 18, 0.53).
narrative_ontology:measurement(clim_be_t24, climate_response_obligation__mitigation_priority, base_extractiveness, 24, 0.56).
narrative_ontology:measurement(clim_be_t30, climate_response_obligation__mitigation_priority, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_obligation__mitigation_priority, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(clim_su_t6, climate_response_obligation__mitigation_priority, suppression_requirement, 6, 0.35).
narrative_ontology:measurement(clim_su_t12, climate_response_obligation__mitigation_priority, suppression_requirement, 12, 0.39).
narrative_ontology:measurement(clim_su_t18, climate_response_obligation__mitigation_priority, suppression_requirement, 18, 0.42).
narrative_ontology:measurement(clim_su_t24, climate_response_obligation__mitigation_priority, suppression_requirement, 24, 0.45).
narrative_ontology:measurement(clim_su_t30, climate_response_obligation__mitigation_priority, suppression_requirement, 30, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__mitigation_priority, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(climate_response_obligation__mitigation_priority, 0.12).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, climate_response_obligation__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, climate_response_obligation__degrowth_reading).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, carbon_pricing_mechanism).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, fossil_fuel_subsidy_regime).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the climate_response_obligation kernel. mitigation_priority (this file) authors ε=0.58 for a transfer-heavy, rapidly-tightening decarbonization mandate; adaptation_priority authors a different standing arrangement (resilience investment, different beneficiary/victim sets, likely lower or differently-structured ε since it does not concentrate stranded-asset costs on fossil capital); degrowth_reading authors yet another arrangement (throughput reduction, diffuse consumption-side costs rather than concentrated fossil-capital costs). Each maintains its own stable ε per the ε-invariance principle; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
