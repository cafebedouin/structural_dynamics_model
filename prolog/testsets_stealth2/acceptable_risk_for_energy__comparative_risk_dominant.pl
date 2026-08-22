% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__comparative_risk_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__comparative_risk_dominant, []).

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
 *   constraint_id: acceptable_risk_for_energy__comparative_risk_dominant
 *   human_readable: Comparative-Risk Standard for Nuclear Energy Acceptability
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   Since the 1970s energy crises, nuclear power's acceptability in most
 *   regulatory and policy settings has been adjudicated comparatively —
 *   against coal mortality, air-pollution burden, and climate damage — rather
 *   than against an absolute safety bar. This story instantiates the
 *   comparative_risk_dominant reading of the acceptable_risk_for_energy
 *   kernel as a clean, epsilon-invariant constraint: the standing arrangement
 *   is the operative comparative-governance regime itself, and epsilon is
 *   authored for THAT arrangement as this reading's own lights assess it
 *   (hence moderate, not low — the reading concedes residual extraction in
 *   uncompensated concentrated exposure, temporally discounted waste custody,
 *   and populations enrolled without a seat). The sibling readings —
 *   catastrophic_tail_dominant and expected_value_dominant — are separate
 *   constraint files with their own epsilon values, victim sets, and temporal
 *   weightings over the same subject matter; they are linked via
 *   network.affects_constraints as a constraint family. The claim/metric
 *   split is deliberate: the claimed type states what is structurally true of
 *   the arrangement; the metrics state what is descriptively true of its
 *   operation. The engine computes per-seat classifications from the
 *   structural data.
 *
 * KEY AGENTS:
 *   - nuclear_regulatory_authorities: Agenda-setter (institutional/identity_locked) — administers licensing under the comparative frame; methodological identity fused with it
 *   - international_atomic_energy_bodies: Agenda-setter and beneficiary (institutional/identity_locked, global) — sets comparative benchmarks, collects arbitral authority
 *   - nuclear_operators_and_vendors: Primary beneficiary (powerful/constrained) — licensing legitimacy and market access convert directly into asset value
 *   - reactor_host_communities: Primary target (moderate/trapped) — concentrated local risk exposure, no veto seat
 *   - uranium_mining_regions: Target (powerless/trapped) — fuel-cycle health burdens treated as a rounding error in the per-terawatt-hour accounting
 *   - future_generations_waste_bearers: Target (powerless/trapped, civilizational horizon) — deferred custody under a temporal discount they never agreed to
 *   - climate_vulnerable_populations: Enrolled target-beneficiary (powerless/trapped) — the moral currency of the comparison, no seat, exposed to the bet's downside
 *   - catastrophic_tail_advocates: Excluded voice (organized/mobile) — tail-dominant objections admitted into consultation but structurally non-decisive
 *   - energy_economists: Analytical observer — supplies the comparison metrics and sees where the frame truncates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__comparative_risk_dominant, 0.44).
domain_priors:suppression_score(acceptable_risk_for_energy__comparative_risk_dominant, 0.52).
domain_priors:theater_ratio(acceptable_risk_for_energy__comparative_risk_dominant, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, extractiveness, 0.44).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__comparative_risk_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__comparative_risk_dominant, "Comparative-Risk Standard for Nuclear Energy Acceptability").
narrative_ontology:topic_domain(acceptable_risk_for_energy__comparative_risk_dominant, "risk_assessment/energy_policy/public_safety_governance").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__comparative_risk_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__comparative_risk_dominant, 'a9ae8377-ac3f-479a-a29c-e1d7937e3e49').
narrative_ontology:cs_kernel_codification('a9ae8377-ac3f-479a-a29c-e1d7937e3e49', distributed).
narrative_ontology:cs_authority_grounding('a9ae8377-ac3f-479a-a29c-e1d7937e3e49', expertise).
narrative_ontology:cs_interpretation_layer_present('a9ae8377-ac3f-479a-a29c-e1d7937e3e49').
narrative_ontology:cs_reading_relation('a9ae8377-ac3f-479a-a29c-e1d7937e3e49', acceptable_risk_for_energy__catastrophic_tail_dominant, forecloses).
narrative_ontology:cs_reading_relation('a9ae8377-ac3f-479a-a29c-e1d7937e3e49', acceptable_risk_for_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_axiom('a9ae8377-ac3f-479a-a29c-e1d7937e3e49', foundational, no_absolute_risk_threshold_for_energy_sources).
narrative_ontology:cs_axiom_status(no_absolute_risk_threshold_for_energy_sources, holdable).
narrative_ontology:cs_axiom_grounding('a9ae8377-ac3f-479a-a29c-e1d7937e3e49', no_absolute_risk_threshold_for_energy_sources, deontological).
narrative_ontology:cs_axiom('a9ae8377-ac3f-479a-a29c-e1d7937e3e49', foundational, present_harm_outranks_deferred_burden).
narrative_ontology:cs_axiom_status(present_harm_outranks_deferred_burden, holdable).
narrative_ontology:cs_axiom_grounding('a9ae8377-ac3f-479a-a29c-e1d7937e3e49', present_harm_outranks_deferred_burden, instrumental).
narrative_ontology:cs_reference_frame('a9ae8377-ac3f-479a-a29c-e1d7937e3e49', cross_source_risk_comparability_baseline).
narrative_ontology:cs_drift_state('a9ae8377-ac3f-479a-a29c-e1d7937e3e49', post_fukushima_climate_urgency_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a9ae8377-ac3f-479a-a29c-e1d7937e3e49', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_operators_and_vendors).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, climate_policy_institutions).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, grid_electricity_consumers).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, reactor_host_communities).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, uranium_mining_regions).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, future_generations_waste_bearers).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, international_atomic_energy_bodies).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, grid_electricity_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run the licensing and oversight system for civilian nuclear power. Their reviews weigh reactor risks against the mortality and emissions of the fossil generation a plant would displace, and a license issues when the balance favors the plant. Staff careers are built on probabilistic safety assessment; the comparative method is not one tool among many available to them but the discipline they are constituted to apply. Walking away from it would mean repudiating the agency's own methodology and reopening settled licenses.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_regulatory_authorities, agenda_setter,
    institutional, generational, identity_locked, national).

% Promulgate international safety standards, peer-review national regulators, and publish the cross-country comparisons that anchor the acceptability argument. Their convening power and budget depend on being the neutral arbiter of exactly this comparison. They have no enforcement arm of their own; their influence runs through the national agencies that adopt their benchmarks.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, international_atomic_energy_bodies, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__comparative_risk_dominant, international_atomic_energy_bodies, beneficiary).

% Build and operate reactors and sell the technology. Every license granted under a comparative review converts directly into asset value and order books, and the comparison against coal mortality and climate damage is their principal public justification — repeated in filings, hearings, and opinion pages. Their capital is sunk in single-purpose plants and supply chains, so their fortunes rise and fall with the frame's continued acceptance.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_operators_and_vendors, beneficiary,
    powerful, biographical, constrained, global).

% Ministries, modeling groups, and advocacy organizations planning decarbonization pathways. The comparison gives them a defensible answer to the question their plans otherwise stumble on — what fills the gap when fossil plants close — and a rebuttal to objections that stall buildout. They are not wedded to nuclear specifically; if storage and firm renewables close the gap, they can redirect attention at low cost.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, climate_policy_institutions, beneficiary,
    organized, generational, mobile, global).

% Receive the output: low-carbon electricity at scale, with reduced exposure to fossil fuel price swings. They also carry the financing — construction overruns and early closures return through rates and taxes — and no household chooses the generation mix it consumes. Their stake is real but diffuse, mediated entirely by bills.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, grid_electricity_consumers, beneficiary,
    organized, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__comparative_risk_dominant, grid_electricity_consumers, payer).

% Live next to the plant. They bear the concentrated slice of accident risk, emergency-planning burdens, land-use stigma, and property effects that the national comparison spreads invisibly across the population it serves. Compensation arrives as jobs and tax base, negotiated before operations, not as ongoing consent. Siting authority sits above them; no local veto exists in most jurisdictions.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, reactor_host_communities, payer,
    moderate, biographical, trapped, local).

% Supply the fuel. Mining, milling, and tailings disposal concentrate contamination and health burden in specific regions — frequently indigenous territories — while the per-terawatt-hour accounting that justifies the plant treats fuel-cycle harm as a rounding error. Employment dependence makes exit costly even where opposition exists.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, uranium_mining_regions, payer,
    powerless, biographical, trapped, regional).

% Inherit the spent fuel. Repository programs have repeatedly slipped; interim storage passes custody forward. The frame that licenses today's plants assigns their burden a discounted weight against present-day harms, and they are present in no proceeding, able to consent to nothing. Whatever custody arrangement eventually matures, they hold the tail of it.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, future_generations_waste_bearers, payer,
    powerless, civilizational, trapped, global).

% Live where fossil mortality and climate damage land heaviest now. Their present-day suffering is the moral arithmetic of the comparison — the reason the plant's risk is said to be worth taking — yet they hold no seat in any siting or licensing decision, and the bargain is struck on their behalf. If the bet turns (an accident triggers retreat from nuclear and fossil plants run longer), they absorb the lost decarbonization along with the harm that motivated it.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations, payer,
    powerless, biographical, trapped, global).

% Organize against nuclear deployment on grounds of low-probability high-consequence events, irreversibility, and burdens passed to generations with no vote. Their arguments are heard in consultations and published widely, but the operative review weighs aggregate comparison, in which their core claim — that some exposures are unacceptable however favorable the average — cannot register as decisive. Their influence runs through politics and courts, not the licensing calculus.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, catastrophic_tail_advocates, excluded,
    organized, generational, mobile, global).

% Compute external costs per terawatt-hour across sources, value statistical lives, and audit the comparisons. They supply the numbers the whole arrangement runs on and are positioned to see both what the comparison genuinely captures and where it truncates — tail correlations, involuntary exposure, irreversibility. Their reward is standing within the assessment community, not rents from any particular verdict.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, energy_economists, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_operators_and_vendors).
narrative_ontology:fixing_cost_class(acceptable_risk_for_energy__comparative_risk_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives regulators, planners, and publics a shared quantitative basis for ranking energy-source risks — deaths, emissions, and external costs per unit of energy — so generation decisions can be made and defended under uncertainty instead of collapsing into incommensurable risk claims; it also coordinates expectations across jurisdictions through international benchmarking.
% TRANSFER_FUNCTION: Moves decision authority over radiological and accident risk from exposed local populations and future generations to aggregate-level assessors; moves concentrated risk exposure onto host communities, mining regions, and eventual waste custodians; moves licensing legitimacy, revenue, and asset value to operators and vendors; moves low-carbon generating capacity and avoided emissions to the grid and the climate commons.
% ABSENT_VOICES: Tail-dominant objectors speak but cannot decide; host communities are consulted without a veto; future generations and climate-vulnerable populations have no procedural presence at all. The broad expert consensus behind the frame is therefore partly an artifact of seating: the parties who would reject commensurability itself were never given a vote inside it.
% DISAPPEARANCE_RATIONALE: Overnight loss of the comparative standard would strip licensing regimes of their evidentiary basis: operating fleets would face rejustification under whatever replaced it, pending builds would stall or convert to stricter review, and generation mixes would shift toward gas and renewables with different risk profiles. Energy politics would reorganize around the successor frame — which is precisely why the seats that prefer different frames fight over this one.
% FOUNDING_PROBLEM: Postwar and post-oil-shock energy planning had to justify large-scale nuclear deployment while coal's visible mortality and vocal tail-risk objection made every option look unacceptable; planners needed a decision rule that could rank imperfect options rather than await a perfect one.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: IPCC and IEA scenario work treats comparative risk ranking as unavoidable for decarbonization planning, and public-health literatures (WHO estimates, the Lancet Commission on pollution and health) independently establish the fossil-mortality magnitudes the comparison relies on. Tail-dominant objectors corroborate that the underlying problem — choosing among hazardous sources — is live while disputing the decision rule itself; no party attests that the problem is dead.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__comparative_risk_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__comparative_risk_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__comparative_risk_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__comparative_risk_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__comparative_risk_dominant, 0.44, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__comparative_risk_dominant_tests).
:- end_tests(acceptable_risk_for_energy__comparative_risk_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.44 — moderate, not low: by this reading's own lights the arrangement is broadly justified, but the reading itself concedes residual extraction in uncompensated concentrated exposure, temporally discounted waste custody, and non-consenting enrolled populations. Suppression (0.52) is a raw structural property, unscaled by the engine: the frame maintains itself by controlling which risk arguments are decisive inside licensing review, not by physically restricting anyone. Theater ratio (0.33): the assessment machinery does real analytical work — probabilistic safety assessment, external-cost accounting — but consultation rituals and rehearsed justifications have grown as opposition has. Accessibility collapse (0.50): alternative framings remain intellectually available and politically live, but inside the operative review they cannot register as decisive. Resistance (0.62): sustained opposition movements, post-accident phase-out decisions, and litigation. The three measurement series share one six-point grid (1974, 1986, 2000, 2011, 2018, 2025); trajectories are event-driven rather than monotonic — Chernobyl (1986) and Fukushima (2011) each spike measured extraction and enforcement requirement as materialized tails strain the frame, and climate urgency rebuilds it through the late 2010s. Endpoint values match the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   From the administrator and operator seats the arrangement presents as the mature form of risk governance: quantified, audited, honest about tradeoffs. From the host-community, mining-region, and future-generation seats the same structure presents as the device that converts their unconsented exposure into someone else's favorable aggregate. Climate-vulnerable populations occupy a third position: a wager placed on their behalf, justified by their own present suffering, in proceedings they cannot attend. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (operators and vendors, climate-policy institutions, grid consumers) derive low directionality — the arrangement subsidizes them. Declared victims (host communities, mining regions, future waste bearers, climate-vulnerable populations) derive high directionality, amplified by trapped exits and, for the waste-bearer seat, a civilizational horizon. One override: the administrator seats (national regulators, international atomic-energy bodies) hold the institutional power atom but appear in no beneficiary/victim list, so the derivation chain would fall back to a generic institutional default; they are in fact beneficiary-leaning — collecting relevance, budgets, and methodological identity while bearing none of the exposure — so an explicit override sets the institutional atom to 0.25. A note the override mechanism cannot express at the powerless atom: climate-vulnerable populations are declared victims and correctly compute as targets, but they also receive the arrangement's intended subsidy (avoided fossil and climate harm), so their true position is somewhat less targeted than the other victim seats; the derived high d is the conservative reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — choosing among hazardous energy sources under climate urgency — is live, so no mandatrophy is declared. The classification earns its keep by blocking both symmetrical errors: reading the frame as pure extraction ignores the real coordination service (a common comparison metric that keeps energy decisions possible at all and prevents paralysis that itself kills), while reading it as pure coordination ignores the documented victim structure (uncompensated local exposure, discounted intergenerational custody, enrolled non-consenting populations). The open watch-item is justification decay: if firm low-carbon alternatives mature, the fossil counterfactual anchoring the comparison weakens and the frame would persist mainly by inertia — the transition the fossil_alternative_counterfactual_stability omega tracks.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'This constraint is one reading of the acceptable_risk_for_energy kernel (comparative_risk_dominant). Which structural features of this story are artifacts of this reading rather than of the underlying subject matter?',
    'Compile the sibling-reading stories and compare after the next materialized tail event or major climate-policy shift; victim sets, temporal weightings, and enforcement profiles should diverge exactly where the decision rules differ.',
    'Under catastrophic_tail_dominant the victim set expands to all potentially exposed populations and waste becomes categorically constraining; under expected_value_dominant the victim set contracts to net-expected-cost losers and the fossil contingency drops out. Classification of the same physical infrastructure flips with the governing reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Reading-indexed structure of the acceptable-risk kernel.').

omega_variable(
    risk_commensurability_assumption,
    'Are energy-source risks genuinely commensurable on a single scale (mortality per terawatt-hour, external cost), or do involuntariness, dread, and irreversibility break the comparison the frame runs on?',
    'Multidimensional risk-perception research, revealed preference in siting disputes, and deliberative exercises testing whether aggregated rankings survive when risk dimensions are kept separate.',
    'If incommensurable, the frame''s aggregation performs normative work disguised as measurement and effective extraction rises as unconsented dimension-trading; if commensurable, the frame''s coordination claim strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(risk_commensurability_assumption, conceptual, 'Whether the comparison metric is measurement or covert valuation.').

omega_variable(
    fossil_alternative_counterfactual_stability,
    'Acceptability under this reading is contingent on the fossil alternative remaining worse — what happens to the frame''s justification if firm renewables and storage make the counterfactual non-fossil?',
    'Grid-decarbonization modeling and observation of whether comparative justifications shift from ''versus coal'' to ''versus gas plus storage'' or lapse as the counterfactual cleans.',
    'If the counterfactual improves, the arrangement''s justification decays while its machinery persists — the drift signature toward inertial persistence; if it does not improve, the frame''s coordination claim holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fossil_alternative_counterfactual_stability, empirical, 'Stability of the fossil counterfactual anchoring the comparison.').

omega_variable(
    intergenerational_discount_legitimacy,
    'Is the reading''s temporal-urgency override — present fossil and climate harm outweighing deferred waste burden — a defensible ethical discount or a self-serving truncation of the victim set?',
    'Deliberative processes including future-generation proxies, repository-siting jurisprudence, and comparative treatment of long-lived burdens in adjacent domains (chemical waste, sovereign debt).',
    'If the override fails scrutiny, deferred burden re-enters the calculus at full weight and this reading collapses toward its tail-dominant sibling; if it holds, the temporal weighting is a legitimate feature of the reading rather than extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_discount_legitimacy, preference, 'Legitimacy of the temporal discount on deferred waste burden.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__comparative_risk_dominant, 1974, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comparative_risk_dominant_tr_t1974, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 1974, 0.22).
narrative_ontology:measurement(comparative_risk_dominant_tr_t1986, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 1986, 0.28).
narrative_ontology:measurement(comparative_risk_dominant_tr_t2000, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2000, 0.27).
narrative_ontology:measurement(comparative_risk_dominant_tr_t2011, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2011, 0.34).
narrative_ontology:measurement(comparative_risk_dominant_tr_t2018, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2018, 0.31).
narrative_ontology:measurement(comparative_risk_dominant_tr_t2025, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2025, 0.33).

% Extraction over time
narrative_ontology:measurement(comparative_risk_dominant_be_t1974, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 1974, 0.36).
narrative_ontology:measurement(comparative_risk_dominant_be_t1986, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 1986, 0.47).
narrative_ontology:measurement(comparative_risk_dominant_be_t2000, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2000, 0.43).
narrative_ontology:measurement(comparative_risk_dominant_be_t2011, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2011, 0.51).
narrative_ontology:measurement(comparative_risk_dominant_be_t2018, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2018, 0.45).
narrative_ontology:measurement(comparative_risk_dominant_be_t2025, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2025, 0.44).

% Suppression requirement over time
narrative_ontology:measurement(comparative_risk_dominant_su_t1974, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 1974, 0.4).
narrative_ontology:measurement(comparative_risk_dominant_su_t1986, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 1986, 0.58).
narrative_ontology:measurement(comparative_risk_dominant_su_t2000, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(comparative_risk_dominant_su_t2011, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2011, 0.66).
narrative_ontology:measurement(comparative_risk_dominant_su_t2018, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2018, 0.55).
narrative_ontology:measurement(comparative_risk_dominant_su_t2025, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2025, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__comparative_risk_dominant, enforcement_mechanism).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy__expected_value_dominant).

% DUAL FORMULATION NOTE:
% The colloquial question 'is nuclear risk acceptable?' decomposes into three structurally distinct constraints — one per reading of the acceptable_risk_for_energy kernel. This file is the comparative_risk_dominant member: relational acceptability, no absolute threshold, victim set including enrolled climate-vulnerable populations and temporally discounted waste bearers. The catastrophic_tail_dominant sibling carries a categorical-threshold structure with an expanded exposure victim set; the expected_value_dominant sibling carries a probability-weighted structure with a contracted victim set. Each sibling authors its own epsilon over the same standing arrangement; they are linked here as a constraint family, with upstream/downstream pressure running from whichever reading governs licensing in a given jurisdiction to the fortunes of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_for_energy__comparative_risk_dominant, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
