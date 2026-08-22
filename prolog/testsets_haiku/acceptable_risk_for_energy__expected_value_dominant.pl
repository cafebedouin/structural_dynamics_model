% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__expected_value_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: acceptable_risk_for_energy__expected_value_dominant
 *   human_readable: Expected-Value Risk Acceptability Calculus for Nuclear Energy
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   This constraint is ONE READING of the contested kernel
 *   'acceptable_risk_for_energy': it instantiates the expected-value-dominant
 *   reading, which grounds risk acceptability in probability-weighted
 *   consequences. The kernel is contested because parties to energy and
 *   climate policy hold fundamentally different risk
 *   calculi—catastrophic-tail-dominant readings weight irreversibility and
 *   worst-case scenarios; comparative-risk-dominant readings anchor
 *   acceptability relative to competing energy sources. This story describes
 *   the expected-value reading alone, as a structurally coherent constraint
 *   with its own ε, beneficiary/victim structure, and suppression mechanisms.
 *   The sibling readings are separate constraints (other files).
 *
 * KEY AGENTS:
 *   - Nuclear industry: collects legitimacy and operational continuity from expected-value framing (high d → beneficiary)
 *   - Climate advocates: benefit from the frame's compatibility with decarbonization urgency (low d → beneficiary)
 *   - Policymakers: enforce the frame through licensing, research funding, and regulatory standard-setting (agenda_setter)
 *   - Waste-host communities: bear localized, intergenerational burden rendered invisible by probability-weighting (high d → victim)
 *   - Future generations: do not consent to the trade-off; their long-term stewardship costs are discounted (high d → victim)
 *   - Tail-risk researchers: excluded from policy deliberation; their research is suppressed (excluded)
 *   - Regulatory agencies: structurally captured by the expected-value methodology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__expected_value_dominant, 0.62).
domain_priors:suppression_score(acceptable_risk_for_energy__expected_value_dominant, 0.48).
domain_priors:theater_ratio(acceptable_risk_for_energy__expected_value_dominant, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, extractiveness, 0.62).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__expected_value_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__expected_value_dominant, "Expected-Value Risk Acceptability Calculus for Nuclear Energy").
narrative_ontology:topic_domain(acceptable_risk_for_energy__expected_value_dominant, "risk_assessment/energy_policy/public_safety_governance").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__expected_value_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__expected_value_dominant, '83d1d792-6e9b-45cd-a853-c90f70ebcf0e').
narrative_ontology:cs_kernel_codification('83d1d792-6e9b-45cd-a853-c90f70ebcf0e', fixed_text).
narrative_ontology:cs_authority_grounding('83d1d792-6e9b-45cd-a853-c90f70ebcf0e', extraction).
narrative_ontology:cs_interpretation_layer_present('83d1d792-6e9b-45cd-a853-c90f70ebcf0e').
narrative_ontology:cs_reading_relation('83d1d792-6e9b-45cd-a853-c90f70ebcf0e', acceptable_risk_for_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('83d1d792-6e9b-45cd-a853-c90f70ebcf0e', acceptable_risk_for_energy__comparative_risk_dominant, coexists_with).
narrative_ontology:cs_axiom('83d1d792-6e9b-45cd-a853-c90f70ebcf0e', foundational, probability_weighted_consequence_sufficiency).
narrative_ontology:cs_axiom_status(probability_weighted_consequence_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('83d1d792-6e9b-45cd-a853-c90f70ebcf0e', probability_weighted_consequence_sufficiency, instrumental).
narrative_ontology:cs_axiom('83d1d792-6e9b-45cd-a853-c90f70ebcf0e', secondary, intergenerational_cost_discounting_legitimacy).
narrative_ontology:cs_axiom_status(intergenerational_cost_discounting_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('83d1d792-6e9b-45cd-a853-c90f70ebcf0e', intergenerational_cost_discounting_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('83d1d792-6e9b-45cd-a853-c90f70ebcf0e', expected_value_risk_optimization).
narrative_ontology:cs_drift_state('83d1d792-6e9b-45cd-a853-c90f70ebcf0e', post_fukushima_intensified_framing, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('83d1d792-6e9b-45cd-a853-c90f70ebcf0e', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, nuclear_industry).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, climate_mitigation_advocates).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, decarbonization_policymakers).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, nuclear_waste_host_communities).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, future_generations).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, low_probability_accident_bearers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates nuclear plants and profits from electricity sales. Expected-value framing legitimizes continued operation by discounting catastrophic scenarios (Fukushima, Chernobyl) as low-probability and therefore acceptable risks. Sets technical standards, funds risk modeling, and controls which scenarios get weighted as 'reasonable' in public discourse.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, nuclear_industry, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__expected_value_dominant, nuclear_industry, agenda_setter).

% Endorse nuclear as decarbonization tool because expected-value arithmetic shows CO2 emissions from coal/gas dwarf nuclear accident risk when probability-weighted. Their position depends on the expected-value frame remaining dominant; catastrophic-tail readings threaten their coalition strategy.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, climate_mitigation_advocates, beneficiary,
    organized, civilizational, mobile, global).

% Set energy and climate policy. Expected-value framing lets them treat nuclear as a solved problem (statistically safe, climate-positive) rather than a contested constraint. They enforce the frame by funding research aligned with expected-value risk models, licensing new plants using these metrics, and sidelining tail-risk analysis in formal policy documents.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, decarbonization_policymakers, agenda_setter,
    institutional, generational, constrained, national).

% Bear the burden of waste storage (decades to millennia of isolation required) and the risk of catastrophic release. Expected-value framing renders their specific local costs invisible: waste is framed as a 'manageable engineering problem' whose small probability-weighted risk dissolves into climate-benefit aggregates that accrue elsewhere.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, nuclear_waste_host_communities, payer,
    powerless, civilizational, trapped, local).

% Do not participate in the risk trade-off that generates today's electricity. Expected-value arithmetic discounts their burden (inherited waste stewardship, intergenerational equity, potential catastrophe they did not choose) by folding it into a multi-generational cost stream that appears small when amortized. They have no seat at the decision table.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, future_generations, payer,
    powerless, civilizational, trapped, global).

% Analyze low-probability high-consequence scenarios and argue for precaution. Their research is systematically suppressed in policy forums that privilege expected-value frames: tail-risk papers are cited as 'alarmist,' funding is steered toward applied nuclear engineering rather than accident modeling, and their warnings about model uncertainty are dismissed as 'not quantitative enough'.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, tail_risk_researchers, excluded,
    moderate, biographical, constrained, national).

% Coal and gas incumbents are excluded from setting the risk calculus, even though comparative-risk frames would pit them directly against nuclear. Expected-value framing implicitly elevates climate risk above nuclear accident risk in the weighting, which undermines their bargaining position in energy policy.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, competing_energy_producers, excluded,
    institutional, generational, constrained, national).

% Approve plants, set safety standards, and model acceptable risk levels. They are structurally captured by expected-value methodology: licensing decisions use these metrics, expert panels are trained in expected-value risk frameworks, and deviation into tail-risk analysis appears non-scientific in formal proceedings.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, regulatory_agencies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_for_energy__expected_value_dominant, nuclear_industry).
narrative_ontology:fixing_cost_class(acceptable_risk_for_energy__expected_value_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the energy-mix optimization problem: allows decentralized energy producers to use a unified risk calculus for comparing nuclear, coal, gas, and renewables on a common metric (probability × consequence), enabling rational technology selection toward climate goals.
% TRANSFER_FUNCTION: Transfers the localized risk of catastrophic accident (borne by waste-host communities and future generations) and the suppressed consideration of tail scenarios into a climate-benefit aggregate that accrues to current-generation energy consumers, policymakers, and the nuclear industry as legitimacy for continued operation.
% ABSENT_VOICES: Tail-risk researchers and future-generation representatives are excluded from policy deliberation. Competing energy producers (coal/gas) are excluded from the weighting process even though comparative-risk frames would reposition them. Host communities have no formal veto power and their objections are treated as 'not understanding the statistics' rather than as legitimate local risk assessment.
% DISAPPEARANCE_RATIONALE: If expected-value framing were abandoned and replaced with catastrophic-tail or comparative-risk frames, nuclear licensing would face immediate scrutiny on irreversibility grounds, waste disposal would shift from 'engineering challenge' to 'intergenerational injustice,' and the relative position of coal/gas in the energy portfolio would improve (lower penalty for tail risks they cannot produce). Policy would reorganize around precautionary principles or explicit trade-off narratives rather than expected-value optimization.
% FOUNDING_PROBLEM: Energy system must balance climate mitigation urgency (CO2 emissions from fossil fuels are catastrophic on century timescales) against nuclear safety concerns (rare but severe accident risk). Expected-value framing was adopted to resolve this trade-off with mathematics rather than politics: if P(accident) × Consequence(accident) < Benefit(CO2 avoided), then nuclear is rational.
% FOUNDING_PROBLEM_CORROBORATION: The nuclear industry and climate science institutions attest that the founding problem is real and best solved by expected-value comparison. Tail-risk researchers, waste-host community groups, and environmental justice advocates attest that the problem is mis-stated: that expected-value arithmetic is insufficient for decisions involving irreversibility, intergenerational burden, and model uncertainty. Peer-reviewed literature outside the nuclear-policy circle documents systematic exclusion of catastrophic-scenario research from mainstream risk models.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__expected_value_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__expected_value_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__expected_value_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__expected_value_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__expected_value_dominant, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__expected_value_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_for_energy__expected_value_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_for_energy__expected_value_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62 at interval end) is moderate-high because the constraint transfers localized, irreversible risks and intergenerational burden into climate-benefit aggregates that flow to current actors. The transfer persists despite high resistance (0.71) because the constraint is actively enforced: risk models are standardized by regulation, funding steers toward applied nuclear engineering, tail-risk papers are excluded from policy journals, and licensing decisions are structured around expected-value metrics. Suppression (0.48) is lower than extractiveness because the tail-risk framing itself is NOT heavily suppressed—it exists in academic literature and activist discourse—but its influence on actual risk governance is suppressed, creating a bifurcated epistemic landscape where two incommensurable risk calculi coexist without one foreclosing the other. Theater (0.31, rising from 0.12) reflects increasing performative maintenance: regulators conduct 'comprehensive' risk assessments that are comprehensive only within the expected-value frame, hold public comment periods that are structured to dismiss tail-risk concerns as 'not scientific,' and publish reassurance documents that cite the same probability-weighted models without acknowledging their methodological constraints. Measurements show extractiveness and suppression requirement both rising slowly over the interval, indicating that as climate urgency increases and fossil-fuel alternatives face pressure, policymakers lean harder on expected-value reasoning to legitimize nuclear, and the suppression of tail-risk frames intensifies.
 *
 * PERSPECTIVAL GAP:
 *   From the expected-value policymaker's seat, the constraint solves a real optimization problem using defensible mathematics; from the waste-host-community seat, the constraint is a mechanism for externalizing their community's irreversible burden onto a deferred cost stream they did not authorize. From the future-generation seat (observer perspective), the constraint is a decision made in their absence, discounting their stewardship obligation into a 'manageable' cost by amortizing it over centuries. From the tail-risk researcher's seat, the constraint is an enforced methodological gatekeeping that precludes the analysis most relevant to actual catastrophic risk. The engine computes these divergent classifications from the structural data—power, exit options, beneficiary/victim position—without requiring consensus on whether the frame is 'right.'
 *
 * DIRECTIONALITY LOGIC:
 *   The nuclear industry (institutional power, arbitrage exit) benefits from the frame without running it—policymakers enforce it. The industry's directionality is low (~0.25, beneficiary end): the frame shields them from catastrophic-risk liability. Climate advocates (organized power, mobile exit) benefit from the frame's compatibility with decarbonization goals, but they genuinely want decarbonization and exit into renewables if the comparison shifted; d ≈ 0.4. Policymakers (institutional power, constrained exit) are the agenda-setters; they have moderate directionality (~0.5) because the frame is their chosen solution, but they also bear political risk from any accident. Waste-host communities (powerless, trapped exit) are pure targets: they bear the localized risk, have no alternatives, cannot relocate, and their objections are not heard in the expected-value calculus—d ≈ 0.95. Future generations (powerless, trapped exit) are similarly target-positioned: d ≈ 0.95. Tail-risk researchers (moderate power, constrained exit) are excluded and suppressed; d ≈ 0.85.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (energy mix under climate urgency) remains live, not dead. But the expected-value constraint has accumulated extraction layers: it was built to enable rational technology selection, but it now shields the nuclear industry from tail-risk criticism and suppresses precautionary voices, making it partially a snare masked as a rope. The theater_ratio rise (0.12 to 0.31) indicates growing performative maintenance: risk reassurance documents proliferate, public engagement is structured to absorb concerns within the expected-value frame rather than genuinely consider alternatives. Mandatrophy is NOT resolved—the constraint still coordinates energy-portfolio decisions—but it is degrading toward a piton state where the coordination function is secondary to the suppression of competing frames.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    model_uncertainty_exogeneity,
    'Is the expected-value model''s uncertainty distribution properly specified, or does it systematically underestimate tail-event probability?',
    'Post-accident comparison: if future catastrophes occur at rates higher than the historical data powering current risk models would predict, the models were mis-specified. Alternatively, Bayesian re-analysis using the full distribution of accident outcomes observed globally (including near-misses and design vulnerabilities discovered post-licensing) rather than historical events only.',
    'If models systematically underestimate tail probability, expected-value arithmetic is shifted: the probability-weighted risk is higher than calculated, making nuclear less acceptable on its own expected-value terms. The constraint''s legitimacy would erode even within the expected-value frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(model_uncertainty_exogeneity, empirical, 'Whether the probabilistic model of rare events captures true tail risk or exhibits systematic underestimation.').

omega_variable(
    incommensurable_risk_framing,
    'Are annual expected costs / climate benefits and catastrophic tail consequences commensurable in a single metric, or do they represent incommensurable decision criteria?',
    'Philosophical analysis and decision-theory review: can expected-value arithmetic be the ONLY legitimate framing for decisions involving intergenerational burden and irreversible consequences? Or do they require a separate precautionary principle applied in parallel?',
    'If incommensurable, the expected-value constraint is not just one frame among others—it is a SUPPRESSION mechanism that forecloses other decision-criteria from having standing. The suppression metric (0.48) would be re-interpreted as higher when including epistemic suppression (suppressing the meta-question of whether expected-value is the right frame).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incommensurable_risk_framing, conceptual, 'Whether expected-value and catastrophic-consequence decision criteria are commensurable or require separate, parallel governance.').

omega_variable(
    intergenerational_consent_and_discounting,
    'Is discounting future costs over centuries legitimate, or does intergenerational justice require that waste-stewardship burden be authorized contemporaneously by those who will bear it?',
    'Normative philosophy and comparative institutional analysis: review how other civilizational-scale decisions (climate policy, genetic engineering, existential-risk governance) treat intergenerational consent and temporal discounting.',
    'If contemporary authorization is required, future-generation victim status is structural (not contestable), and the constraint is extractive by definition. If discounting is legitimate, the burden is ''manageable'' and the constraint is genuinely coordinative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_consent_and_discounting, preference, 'Normative question of intergenerational equity and the legitimacy of discounting long-term burden.').

omega_variable(
    suppression_of_tail_risk_framing,
    'Is suppression of tail-risk research from policy deliberation structural (regulatory gatekeeping) or internalized (tail-risk researchers self-censor because expected-value framing is ''normal'')?',
    'Institutional analysis: document funding flows, publication patterns in policy journals vs. academic journals, composition of regulatory advisory committees, and interview tail-risk researchers about perceived barriers to influence. A sudden policy shift toward tail-risk consideration would test whether suppression is structural (would persist) or internalized (would dissolve).',
    'If structural, the suppression metric (0.48) understates the constraint''s suppressive force on epistemic alternatives. If internalized, the suppression is lower but the constraint''s normalization is more complete—researchers have absorbed the expected-value frame as the only legitimate way to think about risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_tail_risk_framing, empirical, 'Whether suppression of tail-risk frameworks is structural or internalized in researcher communities.').

omega_variable(
    reading_identity_under_sibling_adoption,
    'If a catastrophic-tail or comparative-risk reading were formally adopted in policy (e.g., via legislation mandating precautionary nuclear review or comparative-energy-risk assessment), would the expected-value constraint cease to govern risk decisions, or would it persist as a second-tier principle?',
    'Historical analogy and jurisdictional comparison: review cases where policy frameworks shifted (e.g., EU adoption of precautionary principle, post-Fukushima regulatory changes in Japan, Germany, and the US). Did the expected-value framing dissolve or persist as a subordinate optimization within the new frame?',
    'The reading''s structural position depends on whether it forecloses siblings (if yes, it is constitutively threatened by their adoption) or coexists with them (if yes, it may persist as one principle among others even if deprioritized).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_under_sibling_adoption, conceptual, 'The structural status of this reading if sibling readings were formally institutionalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__expected_value_dominant, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 0, 0.12).
narrative_ontology:measurement(acce_tr_t8, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 8, 0.16).
narrative_ontology:measurement(acce_tr_t16, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 16, 0.22).
narrative_ontology:measurement(acce_tr_t24, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 24, 0.27).
narrative_ontology:measurement(acce_tr_t32, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 32, 0.3).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 40, 0.31).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(acce_be_t8, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(acce_be_t16, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(acce_be_t24, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(acce_be_t32, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 32, 0.62).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(acce_su_t8, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(acce_su_t16, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 16, 0.42).
narrative_ontology:measurement(acce_su_t24, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 24, 0.45).
narrative_ontology:measurement(acce_su_t32, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 32, 0.47).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__expected_value_dominant, resource_allocation).
narrative_ontology:boltzmann_floor_override(acceptable_risk_for_energy__expected_value_dominant, 0.18).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy__comparative_risk_dominant).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'acceptable_risk_for_energy.' The expected-value-dominant reading grounds risk acceptability in probability-weighted expected consequences. Sibling readings—catastrophic-tail-dominant and comparative-risk-dominant—instantiate the same kernel under different risk calculi. They are not different measurements of the same constraint; they are different constraints instantiated by different readings of the kernel. The three readings decompose because their ε values diverge substantially (expected-value treats rare events as negligible; tail-dominant treats them as decision-dominant) and their beneficiary/victim structures differ (expected-value externalizes tail-risk burden onto host communities; tail-dominant assigns precautionary obligation to all parties). Each reading has a stable ε and set of structural relationships; together they map the contested kernel's landscape.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_for_energy__expected_value_dominant, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
