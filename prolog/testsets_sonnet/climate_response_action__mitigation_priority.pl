% ============================================================================
% CONSTRAINT STORY: climate_response_action__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: climate_response_action__mitigation_priority
 *   human_readable: Growth-Compatible Mitigation Pathway (Below-2°C via Innovation and Carbon Markets)
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This story instantiates the mitigation-priority reading of the
 *   climate_response_action kernel: the claim that climate response requires
 *   staying below 2°C through emissions reductions, enabled by technological
 *   innovation and carbon markets, while preserving GDP growth as the
 *   organizing economic principle. This reading is structurally distinct from
 *   the sibling readings (adaptation_priority, degrowth_transformation) — it
 *   has a different beneficiary set, a different cost distribution, and a
 *   different ε, because it channels compliance costs toward near-term
 *   emitters while deferring adaptation and residual-impact costs to the
 *   Global South and future generations, and because it treats carbon removal
 *   technology feasibility and continued growth as compatible premises rather
 *   than points of contest. The three readings are not measurement variants
 *   of one constraint; they are three different constraints sharing a kernel
 *   (the underlying commitment that some climate response is required), and
 *   this file covers only the mitigation-priority reading, per the
 *   ε-invariance decomposition rule.
 *
 * KEY AGENTS:
 *   - innovation_economy_nations: primary beneficiary and agenda_setter (institutional/arbitrage) — shapes treaty architecture and captures technology export rents
 *   - carbon_market_intermediaries: beneficiary (organized/mobile) — extracts fees from credit verification and trading volume
 *   - global_south_frontline_states: primary target (powerless/trapped) — bears near-term physical impacts while adaptation finance lags mitigation finance
 *   - future_generations: primary target (powerless/trapped, civilizational horizon) — bears residual overshoot risk if assumed removal technology does not scale
 *   - smallholder_land_users: target (powerless/trapped) — bears land-use displacement from offset projects
 *   - climate_science_assessment_bodies: analytical observer — supplies the carbon-budget basis cited by all three kernel readings without adjudicating among them
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_action__mitigation_priority, 0.61).
domain_priors:suppression_score(climate_response_action__mitigation_priority, 0.47).
domain_priors:theater_ratio(climate_response_action__mitigation_priority, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, extractiveness, 0.61).
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_action__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_action__mitigation_priority, "Growth-Compatible Mitigation Pathway (Below-2°C via Innovation and Carbon Markets)").
narrative_ontology:topic_domain(climate_response_action__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_action__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_action__mitigation_priority, '788e0c28-eb77-44e8-b16a-a2e9eea1e73c').
narrative_ontology:cs_kernel_codification('788e0c28-eb77-44e8-b16a-a2e9eea1e73c', distributed).
narrative_ontology:cs_authority_grounding('788e0c28-eb77-44e8-b16a-a2e9eea1e73c', distributed).
narrative_ontology:cs_reading_relation('788e0c28-eb77-44e8-b16a-a2e9eea1e73c', climate_response_action__adaptation_priority, influences).
narrative_ontology:cs_reading_relation('788e0c28-eb77-44e8-b16a-a2e9eea1e73c', climate_response_action__degrowth_transformation, forecloses).
narrative_ontology:cs_axiom('788e0c28-eb77-44e8-b16a-a2e9eea1e73c', foundational, technological_substitution_can_decouple_growth_from_emissions).
narrative_ontology:cs_axiom_status(technological_substitution_can_decouple_growth_from_emissions, holdable).
narrative_ontology:cs_axiom_grounding('788e0c28-eb77-44e8-b16a-a2e9eea1e73c', technological_substitution_can_decouple_growth_from_emissions, empirically_contingent).
narrative_ontology:cs_axiom('788e0c28-eb77-44e8-b16a-a2e9eea1e73c', foundational, gdp_growth_is_a_legitimate_organizing_constraint_on_policy_choice).
narrative_ontology:cs_axiom_status(gdp_growth_is_a_legitimate_organizing_constraint_on_policy_choice, holdable).
narrative_ontology:cs_axiom_grounding('788e0c28-eb77-44e8-b16a-a2e9eea1e73c', gdp_growth_is_a_legitimate_organizing_constraint_on_policy_choice, instrumental).
narrative_ontology:cs_reference_frame('788e0c28-eb77-44e8-b16a-a2e9eea1e73c', unfccc_common_but_differentiated_responsibility_framework).
narrative_ontology:cs_drift_state('788e0c28-eb77-44e8-b16a-a2e9eea1e73c', post_paris_agreement_ndc_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('788e0c28-eb77-44e8-b16a-a2e9eea1e73c', '').
narrative_ontology:cs_kernel_id(climate_response_action__mitigation_priority, climate_response_action).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, innovation_economy_nations).
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, carbon_market_intermediaries).
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, incumbent_energy_multinationals).
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, high_emitting_sector_incumbents).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, global_south_frontline_states).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, smallholder_land_users).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, fossil_dependent_labor_forces).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, incumbent_energy_multinationals).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, high_emitting_sector_incumbents).
narrative_ontology:constraint_vindicates(climate_response_action__mitigation_priority, green_growth_decoupling_thesis).
narrative_ontology:constraint_vindicates(climate_response_action__mitigation_priority, market_based_carbon_pricing_efficacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold patents, capital, and industrial capacity for renewables, carbon capture, and green hydrogen. Shape the treaty architecture (NDCs, carbon border mechanisms, offset registries) that channels compliance spending toward their own technology exports, while continuing near-term emissions under the growth-compatible pathway they helped design.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, innovation_economy_nations, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__mitigation_priority, innovation_economy_nations, agenda_setter).

% Verify, broker, and trade offset credits and allowances. Revenue depends on continued market volume rather than on verified atmospheric drawdown; can relocate registries or methodologies across jurisdictions if any single regime tightens standards.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, carbon_market_intermediaries, beneficiary,
    organized, biographical, mobile, global).

% Fund carbon-capture pilots and offset portfolios that extend fossil asset life under a credible-sounding net-zero label, while bearing only the compliance costs they can pass to consumers or defer through negotiated timelines.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, incumbent_energy_multinationals, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__mitigation_priority, incumbent_energy_multinationals, payer).

% Steel, cement, aviation, and heavy manufacturing firms absorb near-term abatement costs mandated by the pathway, but retain market access and subsidy eligibility unavailable to firms outside the compliance regime — the cost is real but survivable and often passed downstream.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, high_emitting_sector_incumbents, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_response_action__mitigation_priority, high_emitting_sector_incumbents, payer).

% Experience sea-level rise, drought, and crop failure now, while adaptation finance remains a fraction of pledged mitigation and technology-transfer spending. Bound into the pathway's monitoring and reporting obligations without comparable access to the innovation rents the pathway generates.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, global_south_frontline_states, payer,
    powerless, generational, trapped, regional).

% Inherit whatever temperature overshoot and residual carbon-removal debt the current pathway leaves unresolved if the assumed negative-emissions technologies do not scale as projected; have no seat in current treaty negotiations.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Land is enrolled in afforestation or offset projects to generate credits sold into distant carbon markets; displacement or restricted land use follows, with compensation and consent processes frequently bypassed or nominal.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, smallholder_land_users, payer,
    powerless, biographical, trapped, local).

% Face job loss as extraction and combustion sectors wind down under the pathway's targets, with just-transition funding typically arriving later and smaller than the displacement it is meant to offset.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, fossil_dependent_labor_forces, payer,
    moderate, biographical, constrained, regional).

% Argue that GDP-growth preservation is itself incompatible with the emissions budget, or that resilience investment should take precedence over unproven removal technologies; present at conferences but structurally marginal in treaty text drafting dominated by innovation-economy delegations.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, degrowth_and_adaptation_advocates, excluded,
    organized, generational, constrained, global).

% Produce the physical carbon-budget assessments the pathway cites as its scientific basis, but do not adjudicate which policy pathway (mitigation-priority, adaptation-priority, degrowth) should be pursued to stay within that budget — their findings are cited selectively by all three readings.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, climate_science_assessment_bodies, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_action__mitigation_priority, innovation_economy_nations).
narrative_ontology:fixing_cost_class(climate_response_action__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared quantitative target (below 2°C), a common accounting framework (NDCs, MRV, carbon markets), and an investment signal that lets firms and states plan decarbonization within a growth-compatible horizon rather than each acting unilaterally with no common metric.
% TRANSFER_FUNCTION: Moves near-term compliance costs onto high-emitting sectors and fossil-dependent workers, moves adaptation and residual-impact costs onto Global South states and future generations, and moves innovation rents and offset-market fees toward technology-exporting nations and market intermediaries.
% ABSENT_VOICES: Degrowth and adaptation-priority advocates, and communities enrolled in offset-generating land projects, argue the growth-preservation premise itself is what forces the extraction pattern; they participate in side events but the treaty architecture and NDC drafting rooms are structured around the innovation-economy delegations' framing.
% DISAPPEARANCE_RATIONALE: If the mitigation-priority pathway disappeared, carbon markets would lose their institutional mandate, NDC-linked finance flows would need a new organizing framework, and the political coalition currently built around 'growth-compatible' decarbonization would have to renegotiate around either accepted temperature overshoot (adaptation-priority) or contraction of throughput (degrowth) — both of which redistribute costs differently.
% FOUNDING_PROBLEM: The 1990s-2010s problem: without a shared temperature target and market mechanism, states had no common metric for emissions reduction and feared unilateral abatement would sacrifice competitiveness; carbon markets and innovation-led decarbonization were designed to let mitigation proceed without requiring economic contraction.
% FOUNDING_PROBLEM_CORROBORATION: Innovation-economy governments and carbon-market operators attest the framework remains necessary and functioning as designed. Independent sources outside the beneficiary set — IPCC working-group authors noting persistent emissions-gap reports, Global South negotiating blocs at COP finance sessions, and academic audits of offset-credit integrity (e.g. investigations finding large shares of forestry credits non-additional) — corroborate that the founding coordination problem (a shared metric) is partially solved while the growth-preservation premise has become a mechanism for cost-deferral rather than a neutral coordination device.
narrative_ontology:disappearance_verdict(climate_response_action__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_action__mitigation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_action__mitigation_priority, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_action__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_action__mitigation_priority, 0.61, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.61) reflects a substantial but not extreme transfer: the pathway does fund real abatement and real technology deployment (a genuine coordination function), but a growing share of its financial architecture (carbon markets, offset registries) generates revenue decoupled from verified atmospheric outcomes, and the cost/benefit split concentrates gains in innovation-capacity nations while concentrating physical and residual risk in frontline states and future generations. Suppression (0.47) is moderate — participation in the pathway is not physically coerced, but Global South states face structural pressure (finance conditionality, market access) to accept NDC and MRV obligations calibrated by richer negotiating parties. Theater ratio (0.44) is elevated and rising because offset credit integrity audits increasingly find non-additional or over-credited projects, meaning a growing share of reported mitigation activity is compliance performance rather than verified removal. Accessibility collapse (0.40) and resistance (0.58) are both moderate: alternative pathways (adaptation-priority, degrowth) remain live and actively advocated, so alternatives have not collapsed, but resistance to the mitigation-priority framing is real and organized among excluded advocacy coalitions.
 *
 * PERSPECTIVAL GAP:
 *   From the innovation-economy and carbon-market seats, this pathway is straightforward coordination: a shared target, a market mechanism, technology deployment at scale. From the frontline-state and future-generations seats, the same structure is an extraction mechanism that defers cost and risk while allocating current rents to whoever holds patents and financial infrastructure. The engine should compute divergent per-seat types from this same structural data — that divergence is the point of the tangled_rope claim, not an error in either seat's assessment.
 *
 * DIRECTIONALITY LOGIC:
 *   Innovation-economy nations and carbon-market intermediaries sit near the beneficiary end: they set the rules, capture the innovation and market rents, and hold arbitrage-grade exit (they can relocate capital or registries). High-emitting sector incumbents and incumbent energy multinationals are dual-positioned — they pay real compliance costs but retain market access and can pass costs downstream, keeping them nearer symmetric than full-target. Global South frontline states, smallholder land users, and future generations are full targets: trapped exit options, no seat in rule-setting, and the structure's residual and physical costs land disproportionately on them. Fossil-dependent labor forces are a moderate-power payer with constrained (not fully trapped) exit, since retraining and relocation are possible but costly and typically under-funded relative to displacement timing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a shared metric enabling coordinated abatement without unilateral competitiveness sacrifice) remains partially live — there genuinely was no common accounting framework before, and one now exists. But the growth-preservation premise, originally a political-feasibility concession to secure participation, has hardened into a structural constraint that forecloses consideration of contraction-based alternatives and channels an increasing share of compliance spending into credit-trading activity with weak additionality. This is exactly the tangled_rope signature: real coordination function (shared target, common accounting) persisting alongside asymmetric extraction (cost deferral to the powerless, rent capture by the powerful) that requires active enforcement (treaty ratification pressure, market-access conditionality) to sustain — not a pure snare, because the coordination function is real, and not a pure rope, because the extraction is structural and ongoing rather than incidental.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    growth_decoupling_feasibility,
    'Is absolute decoupling of GDP growth from emissions at the pace and scale the pathway requires empirically achievable, or does the pathway''s core premise (growth-compatible mitigation) rest on an unproven technological and economic assumption?',
    'Longitudinal cross-national data on absolute (not relative) decoupling rates compared against the emissions trajectory required to stay within the carbon budget; comparison against negative-emissions technology deployment curves versus IPCC scenario assumptions.',
    'If decoupling at required scale proves infeasible, the mitigation-priority reading''s foreclosure of the degrowth reading loses its empirical grounding, and the classification of growth-preservation as a legitimate coordination premise (versus a beneficiary-serving constraint) shifts toward extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_decoupling_feasibility, empirical, 'Whether green-growth decoupling is achievable at the pace the pathway assumes.').

omega_variable(
    offset_market_additionality,
    'What share of carbon credits transacted under this pathway represent genuinely additional emissions reductions or removals, versus non-additional or over-credited claims?',
    'Independent forensic audits of major offset registries (forestry, soil carbon, industrial gas destruction) comparing credited tonnage against satellite and remote-sensing verified outcomes.',
    'Low additionality rates would confirm that a substantial share of the pathway''s claimed mitigation is theater rather than function, raising the effective theater_ratio and supporting the tangled_rope (versus rope) classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(offset_market_additionality, empirical, 'Whether traded carbon credits represent real, verified emissions reductions.').

omega_variable(
    kernel_reading_selection_legitimacy,
    'Was the mitigation-priority reading selected through a process that gave frontline states and future generations proportionate voice, or did the reading prevail because innovation-economy nations held disproportionate drafting power in the treaty process?',
    'Historical analysis of COP negotiating-bloc influence, finance-conditionality linkages, and comparison of proposed adaptation-priority and degrowth framework texts against what was actually adopted.',
    'If the reading prevailed primarily through negotiating power imbalance rather than substantive consensus, this strengthens the case that the kernel selection itself is part of the extraction structure, not a neutral prior to it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_legitimacy, conceptual, 'Whether this reading''s dominance reflects legitimate consensus or negotiating-power asymmetry.').

omega_variable(
    residual_overshoot_liability,
    'If assumed carbon-removal technologies fail to scale as projected, who bears the resulting temperature overshoot and its damages — and is that allocation just given that future generations had no voice in the pathway''s design?',
    'Scenario modeling of technology-scaling failure combined with intergenerational welfare analysis; comparison against liability frameworks proposed in loss-and-damage negotiations.',
    'Confirms or weakens the classification of future_generations as a structural victim group whose costs are being generated by present institutional choices rather than by unavoidable physical necessity alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_overshoot_liability, preference, 'Who bears the risk if assumed removal technologies do not scale as projected.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_action__mitigation_priority, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_action__mitigation_priority, theater_ratio, 0, 0.22).
narrative_ontology:measurement(clim_tr_t5, climate_response_action__mitigation_priority, theater_ratio, 5, 0.28).
narrative_ontology:measurement(clim_tr_t10, climate_response_action__mitigation_priority, theater_ratio, 10, 0.33).
narrative_ontology:measurement(clim_tr_t15, climate_response_action__mitigation_priority, theater_ratio, 15, 0.37).
narrative_ontology:measurement(clim_tr_t20, climate_response_action__mitigation_priority, theater_ratio, 20, 0.4).
narrative_ontology:measurement(clim_tr_t25, climate_response_action__mitigation_priority, theater_ratio, 25, 0.43).
narrative_ontology:measurement(clim_tr_t30, climate_response_action__mitigation_priority, theater_ratio, 30, 0.44).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_action__mitigation_priority, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(clim_be_t5, climate_response_action__mitigation_priority, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(clim_be_t10, climate_response_action__mitigation_priority, base_extractiveness, 10, 0.49).
narrative_ontology:measurement(clim_be_t15, climate_response_action__mitigation_priority, base_extractiveness, 15, 0.53).
narrative_ontology:measurement(clim_be_t20, climate_response_action__mitigation_priority, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(clim_be_t25, climate_response_action__mitigation_priority, base_extractiveness, 25, 0.6).
narrative_ontology:measurement(clim_be_t30, climate_response_action__mitigation_priority, base_extractiveness, 30, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_action__mitigation_priority, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(clim_su_t5, climate_response_action__mitigation_priority, suppression_requirement, 5, 0.34).
narrative_ontology:measurement(clim_su_t10, climate_response_action__mitigation_priority, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(clim_su_t15, climate_response_action__mitigation_priority, suppression_requirement, 15, 0.41).
narrative_ontology:measurement(clim_su_t20, climate_response_action__mitigation_priority, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(clim_su_t25, climate_response_action__mitigation_priority, suppression_requirement, 25, 0.46).
narrative_ontology:measurement(clim_su_t30, climate_response_action__mitigation_priority, suppression_requirement, 30, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_action__mitigation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_action__mitigation_priority, 0.12).
narrative_ontology:affects_constraint(climate_response_action__mitigation_priority, climate_response_action__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_action__mitigation_priority, climate_response_action__degrowth_transformation).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposed from the climate_response_action kernel per the ε-invariance principle. mitigation_priority (this file) concentrates near-term costs on high-emitting sectors and defers residual risk to the Global South and future generations while channeling innovation rents to technology-exporting nations. adaptation_priority accepts warming and reallocates resources toward resilience investment, producing a different beneficiary/victim structure (protection-infrastructure providers benefit; populations in unprotected regions bear residual exposure). degrowth_transformation rejects the growth-preservation premise entirely, producing yet another structure (throughput-reduction advocates and post-growth economies benefit from legitimacy; export-dependent and extraction-dependent economies bear transition costs). Each reading has its own ε and its own stakeholder set; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
