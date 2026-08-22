% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__expected_value_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__expected_value_dominant, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: acceptable_risk_for_energy__expected_value_dominant
 *   human_readable: Expected-Value-Dominant Acceptable Risk Standard for Nuclear Energy
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   This story instantiates the expected_value_dominant reading of the
 *   acceptable_risk_for_energy kernel: the arrangement in which nuclear
 *   licensing acceptability is determined by annualized expected cost
 *   (probability of an accident scenario multiplied by its consequence,
 *   summed across scenarios) weighed against annual expected climate and
 *   economic benefit. Under this reading, nuclear's genuinely low accident
 *   probability nets out favorably against fossil alternatives' certain,
 *   ongoing emissions harm, and long-horizon waste disposal is treated as a
 *   bounded engineering cost that can be discounted and amortized rather than
 *   an open-ended intergenerational liability. This is a deliberately narrow
 *   reading: the sibling readings (catastrophic_tail_dominant,
 *   comparative_risk_dominant) are NOT part of this constraint's
 *   classification, ε, or stakeholder structure — they are separate
 *   constraints linked only via cs_structure.reading_relations and
 *   network.affects_constraints. The coordination function (commensurable
 *   comparison across energy sources) is genuine; the extraction runs through
 *   discount-rate and probability-model choices that concentrate tail
 *   consequence on populations who have no seat in setting those parameters.
 *
 * KEY AGENTS:
 *   - nuclear_utility_operators: beneficiary of the favorable expected-value comparison, organized/mobile
 *   - grid_planning_authorities: agenda_setter administering the acceptability threshold, institutional/analytical
 *   - climate_policy_advocates: beneficiary via nuclear-as-decarbonization framing, organized/mobile, no personal accident exposure
 *   - communities_near_waste_storage_sites: payer bearing concentrated local siting burden discounted to a bounded engineering line item, powerless/trapped
 *   - populations_in_low_probability_high_consequence_exclusion_zones: payer bearing full consequence if the discounted tail event occurs, powerless/trapped
 *   - probabilistic_risk_assessment_modelers: observer whose methodological choices (probability estimates, discount rates) determine the numeric outcome the standard produces
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__expected_value_dominant, 0.32).
domain_priors:suppression_score(acceptable_risk_for_energy__expected_value_dominant, 0.28).
domain_priors:theater_ratio(acceptable_risk_for_energy__expected_value_dominant, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, extractiveness, 0.32).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__expected_value_dominant, rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__expected_value_dominant, "Expected-Value-Dominant Acceptable Risk Standard for Nuclear Energy").
narrative_ontology:topic_domain(acceptable_risk_for_energy__expected_value_dominant, "risk_assessment/energy_policy/public_safety_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__expected_value_dominant, '25b3ed86-2615-4ba2-ba85-32548c180407').
narrative_ontology:cs_kernel_codification('25b3ed86-2615-4ba2-ba85-32548c180407', distributed).
narrative_ontology:cs_authority_grounding('25b3ed86-2615-4ba2-ba85-32548c180407', expertise).
narrative_ontology:cs_interpretation_layer_present('25b3ed86-2615-4ba2-ba85-32548c180407').
narrative_ontology:cs_reading_relation('25b3ed86-2615-4ba2-ba85-32548c180407', acceptable_risk_for_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('25b3ed86-2615-4ba2-ba85-32548c180407', acceptable_risk_for_energy__comparative_risk_dominant, influences).
narrative_ontology:cs_axiom('25b3ed86-2615-4ba2-ba85-32548c180407', foundational, probability_weighted_aggregation_is_the_correct_unit_of_risk).
narrative_ontology:cs_axiom_status(probability_weighted_aggregation_is_the_correct_unit_of_risk, holdable).
narrative_ontology:cs_axiom_grounding('25b3ed86-2615-4ba2-ba85-32548c180407', probability_weighted_aggregation_is_the_correct_unit_of_risk, instrumental).
narrative_ontology:cs_axiom('25b3ed86-2615-4ba2-ba85-32548c180407', secondary, annualized_amortization_is_valid_for_intergenerational_liabilities).
narrative_ontology:cs_axiom_status(annualized_amortization_is_valid_for_intergenerational_liabilities, holdable).
narrative_ontology:cs_axiom_grounding('25b3ed86-2615-4ba2-ba85-32548c180407', annualized_amortization_is_valid_for_intergenerational_liabilities, empirically_contingent).
narrative_ontology:cs_reference_frame('25b3ed86-2615-4ba2-ba85-32548c180407', post_probabilistic_risk_assessment_regulatory_consensus).
narrative_ontology:cs_drift_state('25b3ed86-2615-4ba2-ba85-32548c180407', post_fukushima_reassessment_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('25b3ed86-2615-4ba2-ba85-32548c180407', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, nuclear_utility_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, grid_planning_authorities).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, climate_policy_advocates).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, ratepayers_in_decarbonizing_grids).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, communities_near_waste_storage_sites).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, populations_in_low_probability_high_consequence_exclusion_zones).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, ratepayers_in_decarbonizing_grids).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Site, license, and operate reactors under a regulatory standard that permits construction and continued operation whenever annualized expected cost (probability times consequence, summed across accident scenarios) compares favorably to expected climate and cost benefits of the generated power. This framing lets them site plants, secure financing, and avoid design requirements keyed to worst-case rather than expected-value scenarios. They can relocate capital to other jurisdictions or energy sources if the standard shifts against them.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, nuclear_utility_operators, beneficiary,
    organized, generational, mobile, national).

% Set and administer the expected-value acceptability threshold used in licensing and planning decisions; commission the probabilistic risk assessments that translate accident probability and consequence estimates into a single annualized figure comparable across coal, gas, nuclear, and renewables. They control which probability estimates and discount rates enter the calculation.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, grid_planning_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Favor the expected-value frame because it lets nuclear's low-probability accident risk be netted against its high-certainty climate benefit (displaced fossil generation), producing a favorable comparison that supports nuclear deployment as a decarbonization tool. They are not directly exposed to accident consequences and can advocate for the standard from a position with no personal downside.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, climate_policy_advocates, beneficiary,
    organized, civilizational, mobile, global).

% Receive lower-carbon, often lower-cost electricity under a licensing regime that does not require worst-case-scenario design margins, but also bear diffuse exposure if a low-probability event occurs; their exit is limited to switching electricity providers or advocating politically, not opting out of grid-wide risk exposure.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, ratepayers_in_decarbonizing_grids, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__expected_value_dominant, ratepayers_in_decarbonizing_grids, payer).

% Live adjacent to interim or permanent waste storage facilities sited under a framework that treats long-term waste disposal as a solvable engineering cost folded into the annualized expected-cost calculation rather than an irreversible intergenerational burden. They did not choose the siting, cannot easily relocate given property value effects and local economic dependence, and bear concentrated local risk that the national-scale expected-value average obscures.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, communities_near_waste_storage_sites, payer,
    powerless, civilizational, trapped, local).

% Live within the geographic radius that would be catastrophically affected in the tail-probability accident scenario the expected-value framework discounts by its low probability. If the rare event occurs, consequences are borne entirely by this population despite the national-level calculus treating the risk as acceptable in aggregate; they have no meaningful exit once resident and no voice in how probability estimates were derived.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, populations_in_low_probability_high_consequence_exclusion_zones, payer,
    powerless, generational, trapped, regional).

% Construct the probability and consequence models that feed the expected-value calculation; their methodological choices (which accident sequences to include, what discount rate to apply to future consequences, how to treat model uncertainty) determine the numeric answer the acceptability standard produces, but they do not bear consequences of either accident or licensing decisions.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, probabilistic_risk_assessment_modelers, observer,
    analytical, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_for_energy__expected_value_dominant, nuclear_utility_operators).
narrative_ontology:fixing_cost_class(acceptable_risk_for_energy__expected_value_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single commensurable metric — annualized expected cost, computed as probability times consequence — that lets regulators compare nuclear risk against the risks of coal, gas, and renewable alternatives on the same scale, enabling a licensing and siting decision process instead of an open-ended, incommensurable debate about incomparable harms.
% TRANSFER_FUNCTION: Moves siting and design burden away from worst-case engineering margins (which would raise costs and could halt projects) and toward statistically-averaged safety margins; this transfers concentrated tail risk from the national ratepayer/advocate population (who see only the favorable average) onto the smaller populations geographically proximate to plants and waste sites, who would bear the full consequence if the discounted-probability event actually occurs.
% ABSENT_VOICES: Future generations who will live near waste storage sites decades or centuries hence have no seat in the current probability estimation process; populations in the consequence radius of low-probability accident scenarios are typically a small fraction of the electorate whose local risk is averaged away at the national policy level and rarely participate in setting the discount rates or probability models used against them.
% DISAPPEARANCE_RATIONALE: If the expected-value acceptability standard were replaced overnight by a catastrophic-tail-dominant standard, several currently-licensable reactor designs and waste storage siting decisions would fail acceptability review; utilities would face design margin requirements calibrated to worst-case rather than probability-weighted scenarios, licensing costs would rise sharply, and some currently operating comparisons favoring nuclear over fossil alternatives would need to be re-litigated under a different risk metric.
% FOUNDING_PROBLEM: Regulators needed a tractable, quantitative method to compare the risks of different energy sources so that licensing decisions could proceed on a defensible technical basis rather than ad hoc political judgment or unquantified fear of nuclear accidents relative to visibly-accepted but statistically worse fossil fuel harms.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear engineering and regulatory economics literature (independent academic risk analysts, not utility-affiliated) attest the comparative tractability problem was real and remains partially live — commensurable comparison across energy sources is genuinely useful for policy. However, communities near waste sites and tail-risk researchers attest, from outside the beneficiary set, that the expected-value framing has become a mechanism for discounting irreversible, geographically-concentrated harms rather than a neutral technical tool, particularly regarding multi-millennial waste storage liability.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__expected_value_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__expected_value_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__expected_value_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__expected_value_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__expected_value_dominant, 0.32, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__expected_value_dominant_tests).
:- end_tests(acceptable_risk_for_energy__expected_value_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate-low (0.32) because under THIS reading's own lights the expected-value framing is largely doing genuine coordination work: it produces real, defensible commensurability across energy sources and the underlying probability estimates for modern reactor designs are not fabricated. The extraction that exists runs through discount-rate selection for multi-generational waste liability and through the averaging-away of geographically concentrated tail consequence — real but narrower than a tail-dominant reading would find. Suppression is low-moderate (0.28): tail-risk framing is not actively suppressed under this standard, it is simply outweighed in the calculation rather than excluded from discourse — this is the declared structural delta (low suppression of tail-risk framing) distinguishing this reading from the sibling readings. Accessibility collapse is moderate (0.4): once a jurisdiction adopts expected-value licensing, alternative frames remain visible and contestable in policy debate, they just don't determine the licensing outcome. Resistance is moderate-high (0.55) reflecting active contestation from waste-adjacent communities and tail-risk researchers even though the standard currently prevails procedurally.
 *
 * PERSPECTIVAL GAP:
 *   From the grid_planning_authorities and nuclear_utility_operators seats, the standard reads as rope: real coordination problem (commensurable cross-source risk comparison), genuine beneficiary class, no concentrated victim by their own accounting because tail risk is 'priced in.' From the seat of communities_near_waste_storage_sites and exclusion-zone populations, the same structure reads closer to tangled_rope: the coordination function is real at the national planning level, but the specific parameter choices (discount rates, probability model scope) that make the expected value favorable are set by parties who bear none of the concentrated downside. The engine should compute this divergence from the differential exit_options (mobile/analytical vs. trapped) and power atoms (organized/institutional vs. powerless), not from any authored type label.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear utility operators and grid planning authorities sit near the beneficiary end: they set or benefit from the calculation and can relocate capital or revise methodology respectively. Climate policy advocates similarly benefit without direct accident exposure. Ratepayers in decarbonizing grids are declared beneficiary-with-secondary-payer because they receive real climate and cost benefit but carry diffuse, unpriced tail exposure — closer to symmetric than either pole. The two payer groups (waste-adjacent communities, exclusion-zone populations) sit near the full-target end: trapped exit options, powerless standing, and the specific structural feature that the national-level expected-value average is computed precisely by netting their concentrated local risk against benefits enjoyed elsewhere. No override was needed; the derivation from beneficiary/victim declarations plus exit_options tracks the intended structural story directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (need for a tractable cross-source risk comparison method) remains genuinely live at the level of policy tractability — regulators still need SOME commensurable metric. But the specific parameter choices embedded in the standard (which discount rate, which accident sequences, how far the exclusion radius extends) can drift from serving that founding problem toward serving whichever party currently controls the risk-assessment methodology, without the underlying justification (tractable comparison) ever being challenged. Classifying this as rope rather than tangled_rope reflects that, under this reading's terms, the coordination function still dominates the extraction; a rising theater_ratio or extractiveness trend over a longer interval would be the signal that the standard has drifted toward tangled_rope as parameter-capture accumulates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discount_rate_selection_as_hidden_extraction,
    'Is the discount rate applied to multi-generational waste liability a neutral technical parameter or a de facto policy lever that systematically favors present beneficiaries over future/local payers?',
    'Compare discount rates used in nuclear waste licensing against discount rates used elsewhere in the same regulatory system for comparably long-horizon environmental liabilities (e.g. Superfund remediation, climate damage functions); a consistently lower rate applied uniquely to nuclear waste would indicate parameter selection is doing extractive work under cover of technical neutrality.',
    'If the discount rate is shown to be selected to produce favorable expected-value outcomes rather than derived from a consistent cross-domain standard, the extractiveness score for this reading should rise and the classification should be reconsidered toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discount_rate_selection_as_hidden_extraction, empirical, 'Whether waste-liability discount rate selection is neutral or extraction-serving.').

omega_variable(
    reading_selection_as_the_real_contest,
    'Is the choice of which kernel reading (expected_value_dominant vs. catastrophic_tail_dominant vs. comparative_risk_dominant) governs a given licensing decision itself a neutral technical matter, or is reading-selection the primary site of contest, with each party favoring whichever reading yields the outcome it prefers?',
    'Track which reading each institutional actor (utility, regulator, advocacy group, affected community) argues for across multiple licensing proceedings; consistent alignment between an actor''s preferred reading and its structural position (beneficiary vs. payer) would indicate reading-selection itself functions as the extraction mechanism, prior to any within-reading calculation.',
    'If reading-selection tracks structural position this cleanly, the three sibling constraints in this family are not independent technical disagreements but a single higher-order contest whose outcome the expected_value_dominant reading''s proponents structurally benefit from — this would not change this story''s own ε (which remains fixed per DP-001) but would strengthen the case for weighting network analysis across the family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_selection_as_the_real_contest, conceptual, 'Whether kernel-reading selection itself is the primary extraction site, prior to in-reading calculation.').

omega_variable(
    waste_engineering_solvability_ambiguity,
    'Is multi-millennial nuclear waste disposal genuinely a bounded, solvable engineering problem (as this reading treats it) or an irreducibly open-ended liability whose true cost cannot be annualized at all?',
    'Track the actual performance of deep geological repository programs (e.g. long-term monitoring data, cost overruns relative to original engineering estimates) against the amortized cost assumptions used in expected-value licensing calculations over multiple decades.',
    'If repository costs and timelines consistently exceed original engineering estimates, this reading''s core premise (waste disposal is a solvable, boundable engineering cost) weakens, strengthening the catastrophic_tail_dominant reading''s claim that annualization mischaracterizes an irreversible burden.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(waste_engineering_solvability_ambiguity, empirical, 'Whether treating waste disposal as bounded engineering cost is empirically supportable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__expected_value_dominant, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 0, 0.12).
narrative_ontology:measurement(acce_tr_t8, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 8, 0.15).
narrative_ontology:measurement(acce_tr_t16, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 16, 0.17).
narrative_ontology:measurement(acce_tr_t24, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 24, 0.19).
narrative_ontology:measurement(acce_tr_t32, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 32, 0.21).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(acce_be_t8, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 8, 0.25).
narrative_ontology:measurement(acce_be_t16, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 16, 0.28).
narrative_ontology:measurement(acce_be_t24, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 24, 0.3).
narrative_ontology:measurement(acce_be_t32, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 32, 0.31).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 40, 0.32).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(acceptable_risk_for_energy__expected_value_dominant, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__expected_value_dominant, resource_allocation).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy_catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy_comparative_risk_dominant).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the acceptable_risk_for_energy kernel. catastrophic_tail_dominant treats irreversibility and intergenerational burden as dominating the calculus (reversing the victim/beneficiary structure authored here); comparative_risk_dominant drops the absolute threshold for a relative-to-alternatives standard. Each reading has its own ε, its own stakeholder structure, and its own classification per the ε-invariance principle; they are linked here rather than merged because the underlying legitimacy contest is which risk-weighting framework governs licensing, not a shared measurable quantity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
