% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__catastrophic_tail_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__catastrophic_tail_dominant, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: acceptable_risk_energy__catastrophic_tail_dominant
 *   human_readable: Catastrophic Tail Dominant Risk Assessment in Energy Policy
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   This constraint describes a specific approach to energy risk assessment
 *   where the avoidance of low-probability, high-impact (catastrophic)
 *   events, particularly nuclear accidents, takes precedence over minimizing
 *   aggregate expected harm, even if the latter leads to higher overall
 *   mortality or long-term environmental damage from other sources like
 *   fossil fuels. This reading effectively assigns an 'infinite' weight to
 *   the tail risk of nuclear power, leading to its suppression, while
 *   discounting the distributed and often delayed harms of other energy
 *   sources. This is one reading of the broader 'acceptable_risk_energy'
 *   kernel.
 *
 * KEY AGENTS:
 *   - fossil_fuel_industry: Primary beneficiary (institutional/arbitrage) — benefits from nuclear suppression
 *   - risk_averse_public: Beneficiary (organized/constrained) — perceives safety from nuclear avoidance
 *   - regulatory_agencies: Agenda setter (institutional/constrained) — enforces the risk framework
 *   - nuclear_energy_proponents: Primary victim (organized/constrained) — bears the cost of suppression
 *   - future_generations_climate_change: Victim (powerless/civilizational) — bears long-term, diffuse harm
 *   - decision_theorists: Observer (analytical/analytical) — analyzes the structural biases
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__catastrophic_tail_dominant, 0.65).
domain_priors:suppression_score(acceptable_risk_energy__catastrophic_tail_dominant, 0.78).
domain_priors:theater_ratio(acceptable_risk_energy__catastrophic_tail_dominant, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, extractiveness, 0.65).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__catastrophic_tail_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__catastrophic_tail_dominant, "Catastrophic Tail Dominant Risk Assessment in Energy Policy").
narrative_ontology:topic_domain(acceptable_risk_energy__catastrophic_tail_dominant, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__catastrophic_tail_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__catastrophic_tail_dominant, '8a082b06-bafe-4da8-a24c-7e26efafd827').
narrative_ontology:cs_kernel_codification('8a082b06-bafe-4da8-a24c-7e26efafd827', formalized).
narrative_ontology:cs_authority_grounding('8a082b06-bafe-4da8-a24c-7e26efafd827', extraction).
narrative_ontology:cs_interpretation_layer_present('8a082b06-bafe-4da8-a24c-7e26efafd827').
narrative_ontology:cs_reading_relation('8a082b06-bafe-4da8-a24c-7e26efafd827', acceptable_risk_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_reading_relation('8a082b06-bafe-4da8-a24c-7e26efafd827', acceptable_risk_energy__option_value_preserving, coexists_with).
narrative_ontology:cs_axiom('8a082b06-bafe-4da8-a24c-7e26efafd827', foundational, catastrophic_tail_risk_infinite_weight).
narrative_ontology:cs_axiom_status(catastrophic_tail_risk_infinite_weight, holdable).
narrative_ontology:cs_axiom_grounding('8a082b06-bafe-4da8-a24c-7e26efafd827', catastrophic_tail_risk_infinite_weight, deontological).
narrative_ontology:cs_axiom('8a082b06-bafe-4da8-a24c-7e26efafd827', foundational, distributed_harm_discounted).
narrative_ontology:cs_axiom_status(distributed_harm_discounted, holdable).
narrative_ontology:cs_axiom_grounding('8a082b06-bafe-4da8-a24c-7e26efafd827', distributed_harm_discounted, conventional).
narrative_ontology:cs_reference_frame('8a082b06-bafe-4da8-a24c-7e26efafd827', post_chernobyl_risk_aversion).
narrative_ontology:cs_drift_state('8a082b06-bafe-4da8-a24c-7e26efafd827', contemporary_climate_crisis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8a082b06-bafe-4da8-a24c-7e26efafd827', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_industry).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, risk_averse_public).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, regulatory_agencies).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_energy_proponents).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, future_generations_climate_change).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, renewable_energy_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the suppression of nuclear energy, which reduces competition and maintains demand for fossil fuels. Actively lobbies for policies that emphasize catastrophic tail risk of nuclear power.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Perceives a reduction in immediate, catastrophic risk by avoiding nuclear power, aligning with the 'catastrophic tail dominant' approach. May not fully grasp the long-term, diffuse risks of fossil fuels.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, risk_averse_public, beneficiary,
    organized, biographical, constrained, national).

% Responsible for implementing and enforcing energy risk assessment frameworks. Their mandate is often shaped by public perception and political pressure, leading to the adoption of the catastrophic tail dominant approach.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, regulatory_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Bears the costs of stringent regulations, public opposition, and economic disincentives stemming from the catastrophic tail dominant risk framework, making nuclear power development difficult and expensive.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_energy_proponents, payer,
    organized, generational, constrained, global).

% Bears the long-term, diffuse, and potentially irreversible harms of climate change, exacerbated by continued reliance on fossil fuels due to the suppression of nuclear alternatives.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, future_generations_climate_change, payer,
    powerless, civilizational, trapped, universal).

% Analyze the biases and structural implications of different risk assessment methodologies, often highlighting the inconsistencies in the catastrophic tail dominant approach.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, decision_theorists, observer,
    analytical, biographical, analytical, global).

% Benefit from the suppression of nuclear energy, as it removes a major competitor in the low-carbon energy market. While facing their own challenges, the risk framework indirectly supports their growth.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, renewable_energy_developers, beneficiary,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_industry).
narrative_ontology:fixing_cost_class(acceptable_risk_energy__catastrophic_tail_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates public and regulatory efforts to avoid specific, highly feared catastrophic events in energy production, creating a sense of safety and stability around energy choices.
% TRANSFER_FUNCTION: Transfers economic and political capital from the nuclear energy sector and future generations (via climate change impacts) to the fossil fuel industry and those who benefit from its continued dominance.
% ABSENT_VOICES: Future generations, whose long-term interests are discounted, and those who advocate for a more balanced, aggregate-harm-minimizing approach to energy risk, are effectively absent from the decision-making process that entrenches this risk framework.
% DISAPPEARANCE_RATIONALE: If this risk framework vanished, the energy policy landscape would undergo a significant re-evaluation. Nuclear power would likely see a resurgence, fossil fuel projects would face increased scrutiny over aggregate harm, and investment flows would shift dramatically, leading to a reorganization of energy markets and regulatory priorities.
% FOUNDING_PROBLEM: The problem of managing the perceived catastrophic risks associated with certain energy technologies, particularly nuclear power, after high-profile accidents and public fear.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory agencies and a segment of the public attest that the problem of catastrophic risk is still live and requires this framework. However, nuclear proponents and climate scientists, along with decision theorists, argue that the problem has been reframed to serve specific industrial interests, and that the original problem of 'catastrophic risk' is now a cover for maintaining the status quo, as evidenced by the disproportionate focus on nuclear vs. climate risks.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__catastrophic_tail_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__catastrophic_tail_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__catastrophic_tail_dominant, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(acceptable_risk_energy__catastrophic_tail_dominant, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__catastrophic_tail_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_energy__catastrophic_tail_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_energy__catastrophic_tail_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because this framework imposes significant costs on the nuclear industry and society (e.g., higher energy prices, climate change impacts from fossil fuel reliance) to avoid a specific type of risk. Suppression (0.78) is also high, as it actively discourages and makes economically unviable the nuclear pathway through stringent regulations and public perception management. Theater ratio (0.15) is low, indicating that the risk assessment process is largely functional in achieving its stated (though biased) goal of catastrophic tail avoidance, rather than being purely performative. The increasing extractiveness and suppression over time reflect the hardening of this risk framework.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the fossil fuel industry and a segment of the public, this constraint is a 'Rope' that ensures safety and stability. From the perspective of nuclear energy proponents and those concerned about climate change, it operates as a 'Snare' or 'Tangled Rope' that unfairly burdens one energy source while enabling others with greater aggregate harm. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The fossil fuel industry is a clear beneficiary (d=0.0-0.1) as nuclear suppression reduces competition. The risk-averse public is also a beneficiary (d=0.2-0.3) in terms of perceived safety, though they bear indirect costs. Regulatory agencies (d=0.4-0.5) are agenda setters, balancing public perception and industrial influence. Nuclear energy proponents are direct victims (d=0.8-0.9) due to the high barriers. Future generations are victims (d=1.0) of the long-term climate impacts enabled by this framework. Decision theorists are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mislabeling by highlighting how a 'safety' mandate can become a mechanism for extraction. The original mandate might have been genuine risk reduction, but the 'catastrophic tail dominant' reading has allowed it to drift into a form that disproportionately benefits incumbent industries by suppressing alternatives, even if those alternatives offer lower aggregate risk. The persistence of this framework, despite growing evidence of climate change risks, suggests a mandatrophy where the original problem (energy safety) is now 'contested' in its status, but the constraint continues to operate to the benefit of specific actors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine reflection of inherent risk aversion, or a constructed reading of ''acceptable risk'' that benefits specific industries?',
    'Analysis of lobbying efforts and funding flows from fossil fuel industries into risk assessment bodies; comparative analysis of risk weighting across different sectors (e.g., aviation vs. energy).',
    'If constructed, the classification shifts towards Snare, as the coordination story (public safety) becomes cover for extraction (fossil fuel market dominance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is one reading of the ''acceptable_risk_energy'' kernel, specifically the ''catastrophic_tail_dominant'' reading. Sibling readings include ''expected_value_dominant'' and ''option_value_preserving''.').

omega_variable(
    discount_rate_for_future_harm,
    'What is the appropriate discount rate for future, diffuse harms (e.g., climate change impacts) compared to immediate, concentrated harms (e.g., nuclear accidents)?',
    'Ethical and economic consensus on intergenerational equity and the valuation of long-term environmental damage.',
    'A lower discount rate for future harms would increase the perceived risk of fossil fuels, potentially shifting policy towards nuclear or renewables, and altering the beneficiary/victim structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(discount_rate_for_future_harm, preference, 'The weighting of future vs. immediate harms is a key point of contention across readings.').

omega_variable(
    nuclear_accident_probability_vs_impact,
    'Is the ''catastrophic'' impact of a nuclear accident truly infinite, or is it bounded and comparable to other industrial disasters when considering long-term recovery and societal resilience?',
    'Empirical re-evaluation of long-term consequences of past nuclear accidents (e.g., Chernobyl, Fukushima) including recovery costs, health impacts, and land usability, compared to other large-scale industrial or natural disasters.',
    'If the impact is bounded, the ''infinite weight'' assigned to nuclear accidents in this reading is an overestimation, reducing the perceived risk of nuclear energy and potentially altering policy preferences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_accident_probability_vs_impact, empirical, 'The perceived ''infinite weight'' of nuclear accidents is a distinguishing feature of this reading, which other readings contest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__catastrophic_tail_dominant, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 0, 0.2).
narrative_ontology:measurement(acce_tr_t10, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 10, 0.18).
narrative_ontology:measurement(acce_tr_t20, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 20, 0.15).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(acce_be_t10, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(acce_be_t20, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(acce_su_t10, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(acce_su_t20, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 20, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__catastrophic_tail_dominant, enforcement_mechanism).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy__expected_value_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy__option_value_preserving).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_power_plant_licensing).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_subsidies).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'acceptable_risk_energy' kernel. Each reading represents a distinct structural claim about how energy risk should be assessed, with different beneficiaries, victims, and policy implications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
