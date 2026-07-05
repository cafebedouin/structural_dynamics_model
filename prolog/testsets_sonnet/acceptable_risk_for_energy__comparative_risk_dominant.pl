% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__comparative_risk_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: acceptable_risk_for_energy__comparative_risk_dominant
 *   human_readable: Comparative-Risk Standard for Nuclear Energy Acceptability
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   This story instantiates the comparative-risk-dominant reading of the
 *   contested 'acceptable risk for energy' kernel: nuclear risk is judged
 *   acceptable not against an absolute safety or catastrophic-tail threshold
 *   but against the risk profile of the fossil-fuel alternative it displaces,
 *   particularly coal emissions and climate catastrophe. Under this reading,
 *   climate urgency structurally overrides intergenerational waste concern
 *   and local siting risk, because the comparison set is fixed at the point
 *   of decision to be 'nuclear vs. coal/climate' rather than 'nuclear vs.
 *   zero risk.' This is a genuine coordination function — it lets grid
 *   planners act under real decarbonization time pressure — layered with real
 *   extraction: host communities and future waste custodians absorb
 *   concentrated, durable costs that the aggregate global comparison renders
 *   invisible. The sibling readings (catastrophic_tail_dominant,
 *   expected_value_dominant) are NOT represented here; each is a structurally
 *   distinct constraint with its own ε, victim set, and stakeholder
 *   configuration, to be authored as separate stories per the ε-invariance
 *   principle.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__comparative_risk_dominant, 0.42).
domain_priors:suppression_score(acceptable_risk_for_energy__comparative_risk_dominant, 0.38).
domain_priors:theater_ratio(acceptable_risk_for_energy__comparative_risk_dominant, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, extractiveness, 0.42).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__comparative_risk_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__comparative_risk_dominant, "Comparative-Risk Standard for Nuclear Energy Acceptability").
narrative_ontology:topic_domain(acceptable_risk_for_energy__comparative_risk_dominant, "risk_assessment/energy_policy/public_safety_governance").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__comparative_risk_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__comparative_risk_dominant, '473cb5a5-5eaa-485a-8e35-b984e73a6dc7').
narrative_ontology:cs_kernel_codification('473cb5a5-5eaa-485a-8e35-b984e73a6dc7', distributed).
narrative_ontology:cs_authority_grounding('473cb5a5-5eaa-485a-8e35-b984e73a6dc7', distributed).
narrative_ontology:cs_reading_relation('473cb5a5-5eaa-485a-8e35-b984e73a6dc7', acceptable_risk_for_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('473cb5a5-5eaa-485a-8e35-b984e73a6dc7', acceptable_risk_for_energy__expected_value_dominant, influences).
narrative_ontology:cs_axiom('473cb5a5-5eaa-485a-8e35-b984e73a6dc7', foundational, acceptability_is_always_relative_to_displaced_alternative).
narrative_ontology:cs_axiom_status(acceptability_is_always_relative_to_displaced_alternative, holdable).
narrative_ontology:cs_axiom_grounding('473cb5a5-5eaa-485a-8e35-b984e73a6dc7', acceptability_is_always_relative_to_displaced_alternative, instrumental).
narrative_ontology:cs_axiom('473cb5a5-5eaa-485a-8e35-b984e73a6dc7', foundational, temporal_climate_urgency_overrides_intergenerational_waste_concern).
narrative_ontology:cs_axiom_status(temporal_climate_urgency_overrides_intergenerational_waste_concern, holdable).
narrative_ontology:cs_axiom_grounding('473cb5a5-5eaa-485a-8e35-b984e73a6dc7', temporal_climate_urgency_overrides_intergenerational_waste_concern, instrumental).
narrative_ontology:cs_reference_frame('473cb5a5-5eaa-485a-8e35-b984e73a6dc7', post_three_mile_island_absolute_threshold_regime).
narrative_ontology:cs_drift_state('473cb5a5-5eaa-485a-8e35-b984e73a6dc7', post_paris_agreement_climate_urgency_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('473cb5a5-5eaa-485a-8e35-b984e73a6dc7', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_utility_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations_future).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, grid_decarbonization_planners).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, host_community_residents).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, future_waste_custodian_generations).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, coal_dependent_workers_displaced).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate reactors and lobby regulators to frame acceptable-risk determinations against coal and climate baselines rather than absolute safety thresholds. This framing lets them site and license plants that would fail an absolute-risk test. They capture licensing approval, favorable rate structures, and public subsidy tied to the comparative framing.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_utility_operators, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_utility_operators, agenda_setter).

% Government and utility-commission planners who must decarbonize grids on tight timelines. They adopt the comparative standard because it authorizes nuclear buildout that an absolute-threshold standard would block, treating the framing as the only tractable path to near-term emissions targets.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, grid_decarbonization_planners, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__comparative_risk_dominant, grid_decarbonization_planners, beneficiary).

% People in coastal, low-latitude, and agriculturally fragile regions who bear the brunt of climate catastrophe risk. They benefit from any decarbonization pathway the comparative standard enables, including nuclear buildout, but have no seat in the regulatory process that decides the standard on their behalf.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations_future, beneficiary,
    powerless, civilizational, trapped, global).

% Live near reactor sites and bear concentrated local risk — accident exposure, evacuation burden, property value effects, and stigma — that would be weighed against an absolute local-safety threshold but is instead offset against a global/aggregate coal-versus-nuclear comparison in which their local stake is a rounding error.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, host_community_residents, payer,
    powerless, biographical, trapped, local).

% Inherit spent fuel and long-lived waste with no voice in current siting or licensing decisions. The comparative-risk framing explicitly subordinates the multi-millennial waste burden to the near-term urgency of avoiding fossil-fuel and climate harm, transferring a durable cost onto people not yet born.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, future_waste_custodian_generations, payer,
    powerless, civilizational, trapped, global).

% Workers and communities whose livelihoods depend on the fossil-fuel baseline that makes nuclear look comparatively acceptable. As nuclear displaces coal under this standard, they absorb the transition costs without having chosen the comparison that justified displacing their industry.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, coal_dependent_workers_displaced, payer,
    powerless, biographical, constrained, regional).

% Set licensing thresholds and must choose a risk framework. Adopting comparative-risk dominant reasoning lets them approve designs and sites an absolute-threshold regime would reject, and they administer the enforcement machinery (licensing conditions, inspection regimes) that keeps the comparative standard operative.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, safety_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Argue for absolute or catastrophic-tail-weighted thresholds independent of the coal comparison. They participate in hearings but the comparative framework is typically pre-selected by statute or agency doctrine before their objections are heard, structurally limiting their influence to the margins of siting conditions rather than the underlying risk standard.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, anti_nuclear_advocacy_groups, excluded,
    organized, generational, constrained, national).

% Study and critique the comparative-risk standard's structural effects, publish comparative mortality and land-use analyses, and document how the framing distributes risk unevenly across populations and generations without holding regulatory power themselves.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, risk_policy_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_for_energy__comparative_risk_dominant, diffuse).
narrative_ontology:fixing_cost_class(acceptable_risk_for_energy__comparative_risk_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable decision rule for licensing and siting nuclear power when an absolute zero-risk standard would be paralyzing: it lets planners and regulators compare nuclear's risk profile against the risk profile of the energy sources it actually displaces, enabling decarbonization decisions to proceed under real time pressure.
% TRANSFER_FUNCTION: Moves concentrated local and intergenerational risk (accident exposure, waste custodianship, siting burden) onto host communities and future generations, in exchange for diffuse global benefit (reduced climate and coal-emission harm) captured by climate-vulnerable populations broadly and by utility operators who gain licensing approval and public legitimacy.
% ABSENT_VOICES: Host community residents and future waste-custodian generations have no seat in the comparative calculus that authorizes siting decisions against them; anti-nuclear advocacy groups raise the absolute-threshold objection but engage a framework already fixed by statute, and future generations by definition cannot be consulted at all.
% DISAPPEARANCE_RATIONALE: If the comparative-risk standard vanished and licensing reverted to an absolute-threshold or catastrophic-tail-dominant test, most currently operating and proposed reactors would fail to obtain or retain licenses under stricter absolute criteria, decarbonization timelines dependent on nuclear buildout would need fossil-fuel bridging or accept slower emissions cuts, and utility investment plans built on the comparative framing would need to be rewritten.
% FOUNDING_PROBLEM: Regulators needed a way to authorize any energy technology with non-zero risk once it became clear that zero-risk energy does not exist and that the relevant policy question is which risk profile is being substituted for which, particularly as climate change made the risk of inaction increasingly visible alongside the risk of nuclear accidents.
% FOUNDING_PROBLEM_CORROBORATION: Independent climate scientists and public health researchers outside the nuclear industry (e.g., comparative mortality-per-terawatt-hour studies) corroborate that fossil fuel combustion produces larger aggregate mortality and morbidity than nuclear power under normal operation, supporting the comparative framing's continued relevance; however, independent seismic and waste-engineering assessments from outside both the utility and pro-nuclear policy establishment corroborate that the local and intergenerational cost side of the comparison remains underweighted in current licensing practice.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__comparative_risk_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__comparative_risk_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__comparative_risk_dominant, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__comparative_risk_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__comparative_risk_dominant, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (0.42) is moderate: real coordination value exists (decarbonization urgency is genuine, coal's mortality burden is well documented), but the framing systematically discounts costs borne by parties outside the comparison — local host communities and future generations. Suppression (0.38) reflects that the comparative standard is embedded in statute and agency doctrine, making it hard for absolute-threshold objections to gain procedural traction, though it is not coercively enforced in the way a snare would be. Theater ratio (0.28) is moderate-low: the comparative analyses (mortality-per-terawatt-hour studies, land-use comparisons) are substantively real, though a growing share of licensing rhetoric performs the comparison to pre-legitimate decisions already made on other grounds. Accessibility collapse (0.45) is middling — the absolute-threshold alternative remains conceptually and legally available (see sibling readings), it has simply lost institutional purchase. Resistance (0.55) is real and organized: anti-nuclear and environmental-justice advocacy groups actively contest the framing in hearings and litigation.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear utility operators and grid decarbonization planners sit near the beneficiary end: the comparative framing is the instrument that authorizes their preferred licensing outcomes. Climate-vulnerable populations globally are structural beneficiaries of any successful decarbonization pathway but have no direct voice in the standard's construction — they benefit diffusely without administering anything. Host community residents and future waste-custodian generations sit near the full-target end: they are trapped (no exit from local siting or from being born into an inherited waste burden) and bear concentrated costs that the aggregate comparison structurally discounts. Coal-dependent workers displaced by the resulting energy transition are also payers, though through a different channel (economic displacement rather than physical risk).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that zero-risk energy does not exist and some decision rule is needed once climate risk is taken seriously — remains live: fossil fuel combustion continues to impose large aggregate mortality and climate harm. This prevents a lazy dismissal of the comparative standard as pure extraction; it is doing real coordination work. But the standard's mandate has quietly expanded from 'a workable decision rule under genuine urgency' into a durable mechanism that permanently forecloses absolute-threshold review for concentrated local and intergenerational costs, without a sunset or periodic re-weighting. That is the tangled-rope signature: genuine coordination function, but persistence now depends on active institutional maintenance (statutory embedding, agency doctrine) that structurally silences the parties who pay the discounted costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    comparative_baseline_selection,
    'Is the ''coal and climate catastrophe'' comparison set the only legitimate baseline, or does selecting it (rather than, say, comparing nuclear against renewables-plus-storage) itself embed a policy preference that the comparative framing obscures as a neutral risk calculation?',
    'Independent technology-neutral cost-and-risk comparison across the full menu of decarbonization pathways (nuclear, renewables+storage, efficiency, demand reduction), conducted by parties with no stake in nuclear licensing outcomes.',
    'If renewables-plus-storage is a live comparator with a more favorable risk-and-cost profile than nuclear at the relevant timescale, the comparative standard''s implicit baseline choice (coal vs. nuclear, excluding renewables) does extraction work by narrowing the comparison to make nuclear look favorable, rather than neutrally comparing all real alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(comparative_baseline_selection, conceptual, 'Whether the choice of comparison baseline (coal/climate vs. full alternative set) is itself a contestable, interest-laden decision.').

omega_variable(
    intergenerational_discounting_legitimacy,
    'Does the temporal urgency of climate catastrophe legitimately override concern for multi-millennial waste custodianship, or is this a discount rate applied to future generations without their consent?',
    'Philosophical and empirical work on intergenerational justice and discount-rate selection in long-horizon risk policy, cross-referenced against actual waste-management cost and containment-failure trajectories over multi-century timescales.',
    'If the discounting is illegitimate, the comparative-risk-dominant reading is extracting from future generations under cover of present urgency; if legitimate (e.g., because near-term climate catastrophe threatens civilizational continuity itself, including future generations'' existence), the temporal override is defensible on its own terms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_discounting_legitimacy, preference, 'Whether climate urgency can legitimately override intergenerational waste-burden concerns, and on what normative basis.').

omega_variable(
    coupling_between_readings,
    'Would a jurisdiction applying the catastrophic_tail_dominant or expected_value_dominant reading to the SAME nuclear projects reach a different licensing outcome, and if so, how much of the comparative reading''s apparent coordination value depends on excluding those alternative calculi rather than on the comparative logic being independently correct?',
    'Comparative case study: identify jurisdictions or historical periods where risk policy shifted between these three readings for the same reactor fleet, and observe whether licensing/siting outcomes diverged.',
    'High divergence would show the comparative reading''s authority rests partly on which reading happens to be institutionally dominant, not on the comparative logic being uniquely correct — reinforcing the ε-invariance principle that these are genuinely distinct constraints, not measurement artifacts of one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coupling_between_readings, empirical, 'Whether outcomes are sensitive to which kernel reading is institutionally adopted, evidencing genuine structural distinctness among the readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__comparative_risk_dominant, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 0, 0.15).
narrative_ontology:measurement(acce_tr_t8, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 8, 0.18).
narrative_ontology:measurement(acce_tr_t16, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 16, 0.21).
narrative_ontology:measurement(acce_tr_t24, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 24, 0.24).
narrative_ontology:measurement(acce_tr_t32, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 32, 0.26).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(acce_be_t8, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 8, 0.27).
narrative_ontology:measurement(acce_be_t16, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 16, 0.31).
narrative_ontology:measurement(acce_be_t24, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 24, 0.35).
narrative_ontology:measurement(acce_be_t32, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 32, 0.39).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(acce_su_t8, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 8, 0.28).
narrative_ontology:measurement(acce_su_t16, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 16, 0.31).
narrative_ontology:measurement(acce_su_t24, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 24, 0.33).
narrative_ontology:measurement(acce_su_t32, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 32, 0.36).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__comparative_risk_dominant, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(acceptable_risk_for_energy__comparative_risk_dominant, 0.1).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, expected_value_dominant).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the acceptable_risk_for_energy kernel, decomposed per the ε-invariance principle: catastrophic_tail_dominant (irreversibility/tail-risk weighting dominates; intergenerational burden outweighs expected-value optimization), comparative_risk_dominant (this story; acceptability is always relative to the displaced alternative, no absolute threshold), and expected_value_dominant (probability-weighted annual cost/benefit determines acceptability). Each reading produces a different victim set and a different classification because each embeds a different foundational commitment about how to weigh rare catastrophic events against ongoing diffuse harms. They are linked bidirectionally in the network graph; changes in the institutional dominance of one reading create downstream legitimacy and resource pressure on the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
