% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__expected_value_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__expected_value_dominant, []).

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
 *   constraint_id: acceptable_risk_energy__expected_value_dominant
 *   human_readable: Expected-Value-Dominant Acceptable Risk Standard for Energy Pathways
 *   domain: risk assessment/energy policy/decision theory
 *
 * SUMMARY:
 *   This story instantiates the expected-value-dominant reading of the
 *   acceptable-risk-energy kernel: acceptable risk is defined as minimizing
 *   aggregate expected harm across energy pathways using mortality-per-TWh as
 *   the commensurating metric. Under this reading, fossil fuel deaths (air
 *   pollution, occupational mining harm) enter the ledger at full weight
 *   because they are continuously realized and statistically
 *   well-characterized, while nuclear accident harm is discounted by its low
 *   probability of occurrence. The structural effect is to make nuclear look
 *   favorable and fossil look unfavorable in aggregate comparison, and to
 *   require active suppression of arguments that catastrophic, irreversible,
 *   spatially-concentrated harm should not be commensurated with diffuse
 *   chronic harm via a single expected-value scalar. This is one of three
 *   sibling readings of the same underlying kernel
 *   (catastrophic_tail_dominant, option_value_preserving); each is authored
 *   as its own constraint story with its own epsilon, per the
 *   epsilon-invariance principle, and linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - grid_planning_agencies: agenda_setter (institutional/analytical) — administers and enforces the metric
 *   - nuclear_industry_operators: beneficiary (organized/arbitrage) — favored by probability-discounting of accident risk
 *   - fossil_fuel_frontline_communities: payer (powerless/trapped) — counted at full weight, no voice in methodology
 *   - nuclear_accident_exclusion_zone_residents: payer (powerless/trapped) — tail-risk population structurally underweighted
 *   - independent_risk_analysts: observer (analytical/analytical) — documents the value-laden nature of the aggregation choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__expected_value_dominant, 0.42).
domain_priors:suppression_score(acceptable_risk_energy__expected_value_dominant, 0.61).
domain_priors:theater_ratio(acceptable_risk_energy__expected_value_dominant, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, extractiveness, 0.42).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__expected_value_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__expected_value_dominant, "Expected-Value-Dominant Acceptable Risk Standard for Energy Pathways").
narrative_ontology:topic_domain(acceptable_risk_energy__expected_value_dominant, "risk assessment/energy policy/decision theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__expected_value_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__expected_value_dominant, '7704ed4e-2231-4be5-aa63-97b9550807a5').
narrative_ontology:cs_kernel_codification('7704ed4e-2231-4be5-aa63-97b9550807a5', formalized).
narrative_ontology:cs_authority_grounding('7704ed4e-2231-4be5-aa63-97b9550807a5', expertise).
narrative_ontology:cs_interpretation_layer_present('7704ed4e-2231-4be5-aa63-97b9550807a5').
narrative_ontology:cs_reading_relation('7704ed4e-2231-4be5-aa63-97b9550807a5', acceptable_risk_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('7704ed4e-2231-4be5-aa63-97b9550807a5', acceptable_risk_energy__option_value_preserving, influences).
narrative_ontology:cs_axiom('7704ed4e-2231-4be5-aa63-97b9550807a5', foundational, aggregate_expected_harm_is_the_correct_decision_criterion).
narrative_ontology:cs_axiom_status(aggregate_expected_harm_is_the_correct_decision_criterion, holdable).
narrative_ontology:cs_axiom_grounding('7704ed4e-2231-4be5-aa63-97b9550807a5', aggregate_expected_harm_is_the_correct_decision_criterion, instrumental).
narrative_ontology:cs_axiom('7704ed4e-2231-4be5-aa63-97b9550807a5', foundational, probability_weighting_of_catastrophic_outcomes_is_ethically_permissible).
narrative_ontology:cs_axiom_status(probability_weighting_of_catastrophic_outcomes_is_ethically_permissible, holdable).
narrative_ontology:cs_axiom_grounding('7704ed4e-2231-4be5-aa63-97b9550807a5', probability_weighting_of_catastrophic_outcomes_is_ethically_permissible, empirically_contingent).
narrative_ontology:cs_reference_frame('7704ed4e-2231-4be5-aa63-97b9550807a5', post_accident_quantitative_risk_synthesis).
narrative_ontology:cs_drift_state('7704ed4e-2231-4be5-aa63-97b9550807a5', contemporary_climate_transition_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7704ed4e-2231-4be5-aa63-97b9550807a5', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, nuclear_industry_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, climate_policy_technocrats).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, grid_planning_agencies).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, fossil_fuel_frontline_communities).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, nuclear_accident_exclusion_zone_residents).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, coal_mining_labor_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, renewables_developers).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__expected_value_dominant, mortality_per_twh_commensurability_doctrine).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__expected_value_dominant, expected_value_maximization_as_rational_choice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adopts mortality-per-TWh as the official metric for portfolio decisions, using it to justify permitting, subsidy allocation, and phase-out schedules. Administers the standard and could revise the weighting scheme but treats aggregate expected value as the settled, technocratically neutral answer.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, grid_planning_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Benefits directly: under expected-value accounting, nuclear's low mortality-per-TWh figure (accident risk discounted by low probability) makes it the favored pathway for licensing and subsidy relative to fossil alternatives. Can lobby to keep the metric as-is and has exit options fossil operators lack, since the metric's structure already favors its risk profile.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, nuclear_industry_operators, beneficiary,
    organized, generational, arbitrage, national).

% Uses the expected-value framework to justify rapid fossil displacement on aggregate-harm grounds, which serves their decarbonization agenda; benefits from having a quantitatively 'neutral' tool that produces the conclusions they already favor without needing to argue values directly.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, climate_policy_technocrats, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__expected_value_dominant, climate_policy_technocrats, agenda_setter).

% Bears the counted, full-weight mortality from air pollution and mining that the metric uses to justify closing their local employment base — yet has no seat in setting the metric, no say in the discount rate applied to catastrophic alternatives, and no compensation mechanism tied to the aggregate calculation that names their harm.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, fossil_fuel_frontline_communities, payer,
    powerless, biographical, trapped, local).

% Represents the tail-event population whose harm the metric treats as probability-discounted rather than full-weight; when a low-probability event does occur, this population bears concentrated, irreversible, multi-generational displacement that the expected-value framing structurally underweights relative to per-TWh fossil deaths.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, nuclear_accident_exclusion_zone_residents, payer,
    powerless, generational, trapped, regional).

% Occupational mortality and morbidity from mining enters the fossil-side ledger at full weight, strengthening the case for phase-out that costs this population its livelihood; some retraining and transition support exists but is not derived from or guaranteed by the metric itself.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, coal_mining_labor_populations, payer,
    moderate, biographical, constrained, regional).

% Benefits from the metric's aggregate framing insofar as it discredits fossil incumbency, though renewables occupy a smaller role in the constraint's central contest (which is fossil vs. nuclear risk accounting); can shift investment across jurisdictions freely.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, renewables_developers, beneficiary,
    organized, biographical, mobile, national).

% Argues that catastrophic, irreversible, spatially concentrated harms should not be reduced to the same per-TWh scalar as diffuse chronic harms; is present in public comment processes but structurally outvoted because the adopted metric already presumes commensurability and expected-value aggregation as the decision rule.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, precautionary_risk_advocates, excluded,
    moderate, generational, constrained, national).

% Studies the methodological choices embedded in mortality-per-TWh comparisons — discount rates, exclusion of psychological/displacement harms, treatment of low-probability high-consequence events — without a direct stake in which pathway wins.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, independent_risk_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_energy__expected_value_dominant, diffuse).
narrative_ontology:fixing_cost_class(acceptable_risk_energy__expected_value_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, commensurable metric that lets planners compare mortality risk across structurally dissimilar energy pathways (chronic diffuse harm vs. acute low-probability harm) using one number, avoiding paralysis from incommensurable risk types.
% TRANSFER_FUNCTION: Moves regulatory and investment favor toward pathways whose harms are diffuse and probability-discountable (nuclear) and away from pathways whose harms are concentrated and continuously realized (fossil), while transferring the political cost of catastrophic-tail exposure onto populations near nuclear facilities and the political cost of employment displacement onto fossil-dependent communities.
% ABSENT_VOICES: Precautionary risk advocates and populations in prospective nuclear accident zones raise the incommensurability objection in comment processes but do not control the metric's construction; fossil-dependent labor populations are counted as harm-bearers in the numerator but not consulted on the discount methodology that determines their pathway's fate.
% DISAPPEARANCE_RATIONALE: If the expected-value-dominant standard were abandoned, permitting and subsidy decisions would have to be justified on some other basis (precautionary limits, pathway diversification mandates, or explicit political weighting of catastrophic risk) — nuclear licensing arguments that currently lean on low aggregate mortality-per-TWh would lose their strongest quantitative anchor, and fossil phase-out timelines justified by aggregate harm comparison would need new grounding.
% FOUNDING_PROBLEM: Energy policy needed a way to compare mortality risk across pathways with wildly different harm profiles (chronic occupational and pollution deaths vs. rare catastrophic accidents) without resorting to unstructured political judgment, in the aftermath of high-profile nuclear accidents when public risk perception was seen as poorly calibrated to actual per-unit-energy mortality.
% FOUNDING_PROBLEM_CORROBORATION: Independent risk analysts and academic decision theorists outside the nuclear and climate-technocrat beneficiary groups corroborate that the underlying comparability problem is real and unresolved; the same analysts, however, document that the specific choice to aggregate via expected value (rather than apply a catastrophic-risk premium) is a value-laden methodological choice that the standard's administrators present as if it were a settled empirical finding.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__expected_value_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__expected_value_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__expected_value_dominant, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_energy__expected_value_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__expected_value_dominant, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__expected_value_dominant_tests).
:- end_tests(acceptable_risk_energy__expected_value_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) because the metric does perform a genuine coordination function (making otherwise incommensurable risks comparable) but the specific choice of expected-value aggregation systematically favors pathways whose harm can be probability-discounted, transferring political and material costs onto fossil-dependent labor and tail-risk-bearing communities near nuclear sites. Suppression is substantial and rising (0.35 to 0.61) because defending the aggregation methodology against the incommensurability objection requires increasingly active institutional work as precautionary advocates and affected communities organize. Theater ratio is moderate-low and rising modestly (0.12 to 0.28), reflecting growing performative reliance on the metric's apparent scientific neutrality even as its value-laden discount-rate choices become more visible to critics.
 *
 * PERSPECTIVAL GAP:
 *   From the grid planning agency's seat, the metric is a neutral, well-established tool for comparing energy risk on a fair common basis. From the fossil-frontline-community seat, the same tool operates as a mechanism that counts their harm precisely while discounting the harm-type that favors the competing (nuclear) pathway — the engine's per-seat computation should show this divergence without either seat's framing being adjudicated as 'the' answer.
 *
 * DIRECTIONALITY LOGIC:
 *   Grid planning agencies and climate technocrats sit at the agenda-setting/beneficiary end: they administer the standard and it produces conclusions congenial to their policy goals. Nuclear operators are structural beneficiaries because the accounting method's probability-discounting mechanism directly favors their risk profile. Fossil-fuel frontline communities and nuclear exclusion-zone residents sit at the target end: the former are counted at full weight to justify closing their industry without commensurate voice in the metric's construction; the latter bear the exact risk category (rare, catastrophic, irreversible) that the methodology is built to discount. Coal mining labor sits closer to target than beneficiary despite some transition support, because that support is not derived from or guaranteed by the metric itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (commensurating radically different risk profiles for planning purposes) remains partially live, but the specific solution (expected-value aggregation as the only legitimate framework) has drifted from a methodological convenience into a background assumption that forecloses debate about whether catastrophic and chronic harms should be commensurated at all. This is not classified as fully resolved mandatrophy because the coordination function (some way of comparing pathways) is still needed; the tangled_rope classification captures that a genuine coordination need coexists with asymmetric extraction via the specific choice of aggregation rule, which is why active enforcement (suppressing incommensurability objections) is required to sustain it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commensurability_of_harm_types,
    'Is it structurally valid to reduce chronic, statistically well-characterized fossil harm and rare, catastrophic, irreversible nuclear-accident harm to a single per-TWh mortality scalar, or does the act of commensuration itself smuggle in a value judgment favoring probability-discountable risk?',
    'Formal decision-theoretic analysis of whether expected-value aggregation across qualitatively distinct harm distributions (Poisson-like chronic harm vs. heavy-tailed catastrophic harm) preserves the ethically relevant distinctions, compared against alternative aggregation rules (e.g., risk-weighted or tail-sensitive metrics).',
    'If commensuration is found to be a substantive value choice rather than a neutral technical step, the standard''s claim to technocratic neutrality collapses and the tangled_rope classification strengthens toward snare, since the coordination story would be revealed as a cover for a specific, contestable risk-tolerance policy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commensurability_of_harm_types, conceptual, 'Whether expected-value commensuration of chronic and catastrophic harm types is a neutral technical convenience or a substantive contested value choice.').

omega_variable(
    kernel_reading_selection_mechanism,
    'This constraint is one of three readings of the acceptable_risk_energy kernel (expected_value_dominant, catastrophic_tail_dominant, option_value_preserving). What determines which reading a given regulatory body actually adopts, and is that selection itself contestable on procedural grounds?',
    'Comparative institutional analysis of which agencies/jurisdictions adopt which reading, and whether adoption follows from public deliberation or from which interest groups control agenda-setting at the moment the metric is codified.',
    'If adoption of the expected_value_dominant reading correlates with which interest groups (e.g., nuclear industry, decarbonization technocrats) control the agenda-setting process at codification time, this strengthens the case that the reading functions partly as post-hoc justification for pathway preferences already held, rather than as a neutral discovery of the correct risk framework.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_mechanism, conceptual, 'Whether selection among the three sibling readings of the acceptable-risk kernel is procedurally neutral or interest-driven.').

omega_variable(
    fossil_beneficiary_false_summit_check,
    'Does the expected-value standard''s apparent scientific/technocratic neutrality function as cover for what is structurally an extractive reallocation of political risk-bearing costs onto powerless populations on both ends of the pathway spectrum (fossil-frontline communities AND nuclear exclusion-zone residents)?',
    'Track whether compensation mechanisms, veto rights, or procedural voice for affected populations expand or contract as the standard becomes more institutionally entrenched; stagnant or shrinking voice alongside rising suppression_requirement would corroborate the extractive reading.',
    'If voice mechanisms remain absent while suppression rises, this constraint''s tangled_rope classification is well-grounded and stable; if voice mechanisms expand over time, the constraint may be trending toward genuine rope as the coordination function absorbs its critics rather than suppressing them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fossil_beneficiary_false_summit_check, empirical, 'Whether the standard''s neutrality claim survives scrutiny of who gains procedural voice over time.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__expected_value_dominant, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_energy__expected_value_dominant, theater_ratio, 0, 0.12).
narrative_ontology:measurement(acce_tr_t8, acceptable_risk_energy__expected_value_dominant, theater_ratio, 8, 0.16).
narrative_ontology:measurement(acce_tr_t16, acceptable_risk_energy__expected_value_dominant, theater_ratio, 16, 0.19).
narrative_ontology:measurement(acce_tr_t24, acceptable_risk_energy__expected_value_dominant, theater_ratio, 24, 0.22).
narrative_ontology:measurement(acce_tr_t32, acceptable_risk_energy__expected_value_dominant, theater_ratio, 32, 0.25).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_energy__expected_value_dominant, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(acce_be_t8, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(acce_be_t16, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 16, 0.33).
narrative_ontology:measurement(acce_be_t24, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 24, 0.37).
narrative_ontology:measurement(acce_be_t32, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(acce_su_t8, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(acce_su_t16, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(acce_su_t24, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(acce_su_t32, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 32, 0.58).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 40, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__expected_value_dominant, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(acceptable_risk_energy__expected_value_dominant, 0.1).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy__option_value_preserving).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the acceptable_risk_energy kernel. All three share the same underlying decision problem (how to define acceptable risk across energy pathways) but instantiate structurally distinct constraints with different epsilon values, different beneficiary/victim sets, and different classifications: expected_value_dominant (this file, tangled_rope) favors probability-discountable pathways; catastrophic_tail_dominant would show different victim weighting (favoring avoidance of rare catastrophic harm even at higher aggregate cost, shifting the fossil/nuclear balance); option_value_preserving would show a different beneficiary structure entirely (pathway diversity itself as the protected value, benefiting neither nuclear nor fossil incumbents specifically but rather optionality-holders). Per the epsilon-invariance principle, these are NOT the same constraint measured three ways — they are three constraints sharing a kernel, each with its own stable epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
