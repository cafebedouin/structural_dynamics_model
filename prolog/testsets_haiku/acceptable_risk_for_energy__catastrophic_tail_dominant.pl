% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__catastrophic_tail_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__catastrophic_tail_dominant, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: acceptable_risk_for_energy__catastrophic_tail_dominant
 *   human_readable: Catastrophic Tail Risk Dominance in Energy Governance
 *   domain: energy_policy/risk_assessment/public_safety
 *
 * SUMMARY:
 *   This constraint instantiates one reading of a contested kernel:
 *   acceptable_risk_for_energy. The reading asserts that low-probability,
 *   high-consequence events (reactor failure, waste containment collapse,
 *   climate tipping points) should dominate risk calculus, overriding
 *   expected-value optimization and shorter time horizons. Under this
 *   reading, nuclear waste storage becomes a binding constraint on energy
 *   policy (irreversible, civilization-timescale consequence) and fossil
 *   fuels lose justification on climate grounds (tail-risk dominance applies
 *   symmetrically). The constraint suppresses the probabilistic trade-off
 *   framing that incumbent operators use to justify continued operation. The
 *   measured extraction is high (0.68) because the reading restructures which
 *   risks are bearable and who bears them: it shifts burden from current
 *   planners to future generations and site-dependent populations, a transfer
 *   that appears justified by irreversibility but operates as institutional
 *   suppression of alternative risk frameworks. The claim (tangled_rope)
 *   reflects genuine coordination on irreversibility principles but coupled
 *   with asymmetric extraction: fossil operators and short-term planners
 *   benefit from the framing's suppression of expected-value defenses, while
 *   nuclear proponents and future entities bear the cost of tail-dominance
 *   applied to their preferred technologies and interests.
 *
 * KEY AGENTS:
 *   - fossil_fuel_incumbent_operators: Beneficiary; institutional power. Benefit from suppression of tail-dominant framing against their low-tail-risk defense (fossil fuels emit carbon on certainty, not catastrophic tail).
 *   - nuclear_energy_proponents: Payer; powerful but constrained. Bear the cost of tail-dominance applied asymmetrically to nuclear (waste storage becomes binding constraint) while fossil fuels' tail-climate-risk is downplayed in expected-value frameworks.
 *   - future_generations: Payer (non-agent entity); powerless, civilizational horizon. Inherit irreversible consequences—waste storage burden, climate forcing—without choice.
 *   - site_dependent_populations: Payer; moderate power, local scope. Bear local tail risks (reactor failure, tailings seepage) under constraint that rare events dominate.
 *   - regulatory_agencies: Agenda-setter; institutional power. Must choose between risk calculi; tail-dominant reading constrains their legitimate choice-set.
 *   - expected_value_proponents: Observer; institutional power. Represent the rival reading (expected_value_dominant) and maintain that tail-dominance misallocates risk-aversion.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.68).
domain_priors:suppression_score(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.71).
domain_priors:theater_ratio(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, extractiveness, 0.68).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__catastrophic_tail_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__catastrophic_tail_dominant, "Catastrophic Tail Risk Dominance in Energy Governance").
narrative_ontology:topic_domain(acceptable_risk_for_energy__catastrophic_tail_dominant, "energy_policy/risk_assessment/public_safety").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__catastrophic_tail_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__catastrophic_tail_dominant, '7a9012bb-bc3c-4776-9b3b-1e9ac104d1dd').
narrative_ontology:cs_kernel_codification('7a9012bb-bc3c-4776-9b3b-1e9ac104d1dd', distributed).
narrative_ontology:cs_authority_grounding('7a9012bb-bc3c-4776-9b3b-1e9ac104d1dd', distributed).
narrative_ontology:cs_reading_relation('7a9012bb-bc3c-4776-9b3b-1e9ac104d1dd', acceptable_risk_for_energy__expected_value_dominant, forecloses).
narrative_ontology:cs_reading_relation('7a9012bb-bc3c-4776-9b3b-1e9ac104d1dd', acceptable_risk_for_energy__comparative_risk_dominant, coexists_with).
narrative_ontology:cs_axiom('7a9012bb-bc3c-4776-9b3b-1e9ac104d1dd', foundational, irreversibility_dominates_probability).
narrative_ontology:cs_axiom_status(irreversibility_dominates_probability, holdable).
narrative_ontology:cs_axiom_grounding('7a9012bb-bc3c-4776-9b3b-1e9ac104d1dd', irreversibility_dominates_probability, deontological).
narrative_ontology:cs_axiom('7a9012bb-bc3c-4776-9b3b-1e9ac104d1dd', foundational, intergenerational_consent_constraint).
narrative_ontology:cs_axiom_status(intergenerational_consent_constraint, holdable).
narrative_ontology:cs_axiom_grounding('7a9012bb-bc3c-4776-9b3b-1e9ac104d1dd', intergenerational_consent_constraint, deontological).
narrative_ontology:cs_reference_frame('7a9012bb-bc3c-4776-9b3b-1e9ac104d1dd', irreversibility_primacy_framework).
narrative_ontology:cs_drift_state('7a9012bb-bc3c-4776-9b3b-1e9ac104d1dd', post_climate_tipping_point_awareness_2020s, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('7a9012bb-bc3c-4776-9b3b-1e9ac104d1dd', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, fossil_fuel_incumbent_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, short_term_economic_planners).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, future_generations).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_waste_repositories).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, site_dependent_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_energy_proponents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Burn coal, oil, and gas for energy generation under carbon accounting that discounts catastrophic climate tail (centuries of warming acceleration, sea-level rise) through probability weighting. The tail-risk dominant reading challenges this framing by assigning near-irreversible climate impacts a weight that dominates the probability × consequence calculus. Incumbent operators benefit from the suppression of tail-dominant reasoning because it allows continued operation under expected-value justification.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, fossil_fuel_incumbent_operators, beneficiary,
    institutional, biographical, arbitrage, global).

% Advocate nuclear power as a low-carbon energy source. Under tail-risk dominant framing, they face a binding constraint: they must adopt the same catastrophic-outcome weighting their opponents apply to climate, which elevates millennial-timescale waste storage failures, reactor meltdowns under aging infrastructure, and terrorism to high-consequence scenarios that probabilistic discounting cannot ethically resolve. This asymmetric application of the reading suppresses nuclear advancement.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_energy_proponents, payer,
    powerful, generational, constrained, global).

% Inherit all energy infrastructure legacies: nuclear waste stored for millennia, climate forcing committed by historical carbon emissions, and the institutional arrangements that decided which risks to discount. They cannot opt out of the energy legacy or the risk profile. The reading elevates them as a constraint-bearing entity because irreversibility places their options under the constraint we impose today.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, future_generations, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(acceptable_risk_for_energy__catastrophic_tail_dominant, future_generations).

% Live near energy infrastructure—coal plants, nuclear reactors, waste repositories, solar facilities on formerly agricultural land. They bear the local tail risks: reactor failures, tailings seepage, chronic exposure to air or water pollution. Tail-risk dominant framing argues their exposure to rare but catastrophic events should govern site decisions, overriding expected-value justifications that appear rational at the national scale.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, site_dependent_populations, payer,
    moderate, biographical, constrained, local).

% Enforce energy policy and set acceptable-risk thresholds through cost-benefit analysis, safety standards, and licensing. They are tasked with choosing which risk calculus (tail-dominant vs. expected-value) governs licensing and investment. Tail-risk dominant framing constrains their choice-set by elevating irreversible harms and intergenerational burdens above probabilistic discounting.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, regulatory_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Provide data on climate tail risks (irreversible tipping points, sea-level rise acceleration, ecosystem collapse timescales). They document the high-consequence, low-probability nature of climate catastrophe but typically do NOT advocate that this tail structure should override expected-value analysis. Tail-dominant reading uses their data to constrain energy-policy debates by insisting tail-weight dominance apply symmetrically across all energy sources.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, climate_scientists, observer,
    institutional, generational, analytical, global).

% Optimize energy systems on 10–40 year planning horizons for economic growth and current-generation welfare. Expected-value risk calculus aligns with their time horizon and incentives. Tail-risk dominant reading forces them to adopt longer time horizons and irreversibility-weighted budgets, which constrains near-term optimization and shifts investment away from lowest-immediate-cost sources.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, short_term_economic_planners, beneficiary,
    powerful, biographical, arbitrage, national).

% Manage long-term waste storage facilities on thousand-year+ timescales. Tail-dominant reading elevates their operational constraints to the highest policy level, because waste containment failures become the binding tail-risk; yet they are typically excluded from energy-policy debates framed around power generation and consumption. Their irreducible tail-risk exposure is the reading's central object, but they have minimal voice in deciding acceptable-risk frameworks.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, waste_repository_operators, excluded,
    moderate, civilizational, trapped, local).

% Use probability-weighted expected-value analysis to compare energy risks. They represent a rival reading (expected_value_dominant) within the contested kernel. They maintain that tail-dominant framing misallocates risk-aversion and leads to suboptimal energy mix decisions by over-weighting rare events and under-weighting near-certainty climate harm from delayed decarbonization.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, expected_value_risk_framework_proponents, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_for_energy__catastrophic_tail_dominant, fossil_fuel_incumbent_operators).
narrative_ontology:fixing_cost_class(acceptable_risk_for_energy__catastrophic_tail_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared principle for comparing energy risks across time horizons and consequence magnitudes: by shifting dominance from probability-weighted expected value to absolute tail-consequence magnitude (irreversibility, intergenerational burden), the framework coordinates all energy policy around the same catastrophic-outcome weighting structure, creating a common language for regulatory decisions and public discourse.
% TRANSFER_FUNCTION: Transfers the burden of risk-bearing from current planners and fossil fuel operators (who benefit from expected-value discounting of tail events) to future generations and site-dependent populations (who inherit the irreversible consequences without choice or exit). It also transfers analytical legitimacy from quantitative cost-benefit frameworks to qualitative irreversibility arguments, shifting where policy authority sits.
% ABSENT_VOICES: Engineers and systems planners who would argue for marginal risk-reduction (achieving safety through incremental design improvements) are structurally absent from a reading that prioritizes irreversibility over improvability. Waste-repository operators, who bear the civilization-scale tail risk directly, are excluded from energy-generation policy conversations. Climate economists who emphasize optimal-warming calculations (which incorporate tail risk but still apply expected-value math) are not party to these discussions. Their absence allows the reading to suppress probabilistic trade-off framing entirely.
% DISAPPEARANCE_RATIONALE: If this constraint—the requirement to weight irreversible, civilization-scale tail risks above expected-value optimization—vanished, energy policy would revert to lowest-immediate-cost and marginal-risk-reduction frameworks. Nuclear would re-enter serious consideration for baseload decarbonization; fossil fuels would be justified on climate-tail grounds (warming is probabilistically distant); waste storage would return to engineering problem status rather than binding constraint. The arrangement of authority, risk language, and policy timescales would reorganize around expected-value dominance.
% FOUNDING_PROBLEM: Energy systems require decisions on power generation, storage, and infrastructure on decades-long capital timescales, but the consequences—climate forcing, nuclear waste containment, local ecosystem damage—operate on century-to-millennial scales with low-probability catastrophic modes. Expected-value frameworks discount these rare tail events, leading planners to choose energy systems that appear safe on statistical grounds but impose irreversible burdens on entities (future generations, distant populations) who did not choose them. The founding problem is the mismatch between decision time horizon and consequence time horizon.
% FOUNDING_PROBLEM_CORROBORATION: Climate science and long-term waste-storage research from academic and government sources (IPCC, National Academies, geological surveys) confirm the existence of low-probability, high-consequence modes in both climate and nuclear systems. However, whether these low-probability modes should DOMINATE risk calculus (the reading's core claim) is contested. Risk economists and energy planners argue expected-value analysis is the appropriate framework; environmental and indigenous advocates argue irreversibility should override probability weighting. The founding problem itself is acknowledged by all parties; the reading's assertion that tail-dominance is the proper response is NOT corroborated by expected-value proponents, making this a kernel reading split.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__catastrophic_tail_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__catastrophic_tail_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__catastrophic_tail_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__catastrophic_tail_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__catastrophic_tail_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_for_energy__catastrophic_tail_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_for_energy__catastrophic_tail_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the constraint restructures power over risk discourse: it shifts authority from quantitative cost-benefit analysts (who have institutional power under expected-value framing) to qualitative irreversibility advocates (who have moral authority under tail-dominance framing). This is extraction because it concentrates decision power in the hands of those who benefit from the shift (fossil operators, who use tail-dominance to suppress nuclear; short-term planners, who avoid long-term irreversibility accounting). Suppression is high (0.71) because the constraint's persistence depends on actively excluding expected-value reasoning, not on participant preference—regulators would still use probabilistic trade-offs if not pressured to adopt tail-dominance language. Theater is moderate (0.42) because the reading has genuine content (irreversibility IS a meaningful constraint on risk-bearing), but a growing share of regulatory activity defends the tail-dominance frame against technical challenges rather than addressing substantive waste-storage or climate-tail empirics. The measurement series shows suppression and extractiveness rising as the reading consolidates institutional authority (t=0 to t=50), with theater ratio stabilizing after institutional adoption.
 *
 * PERSPECTIVAL GAP:
 *   Fossil fuel operators and short-term planners experience the constraint as a beneficial coordination mechanism (irreversibility is a real principle worthy of dominance); nuclear proponents and waste-repository operators experience it as asymmetric suppression (tail-dominance applied selectively to nuclear, not to climate-tail risks from fossil fuels). The engine should compute this divergence from the structural data: beneficiaries sit near d=0 (low extraction felt), payers near d=1 (high extraction felt). The agenda-setter (regulatory agencies) sits near d=0.5 but faces pressure from both directions: fossil interests use tail-dominance to block nuclear; climate interests use it to demand fossil phase-out. The perspectival gap is the reason the constraint is tangled_rope, not rope: there is real coordination (irreversibility matters), but the application is asymmetric and enforced.
 *
 * DIRECTIONALITY LOGIC:
 *   Fossil operators benefit (d near 0.0) because tail-dominance suppresses the expected-value defense for nuclear without applying the same weight to climate tails from continued fossil use. Nuclear proponents pay (d near 1.0) because they must adopt the tail-dominance frame that makes waste storage a civilization-binding constraint. Future generations have no exit (d=1.0, trapped, identity_locked) because they inherit the irreversible consequences of today's choices. Site-dependent populations are constrained payers (d=0.6–0.7) because they can relocate but rarely do, so they bear local tail risks under the constraint's dominance. Regulatory agencies sit near d=0.5 but face asymmetric pressure: fossil interests benefit from tail-dominance framing, creating institutional drag that makes the constraint feel like extraction when authorities try to use expected-value analysis.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: energy systems do require decisions on decades-long timescales with century-to-millennial consequences, and low-probability catastrophic modes exist in both climate and nuclear systems. The reading's response—elevating tail-dominance above expected-value optimization—is contested but coherent. The mandatrophy question is whether the reading's application is symmetric: does it apply tail-dominance to all energy sources equally, or does it suppress expected-value defenses for nuclear while leaving fossil-fuel climate risks under probabilistic discounting? The asymmetry is the tangled_rope signature: genuine coordination (irreversibility matters) coupled with asymmetric extraction (selective application favors incumbents). A rope reading would apply tail-dominance uniformly; a snare reading would suppress all probabilistic reasoning without coordinate function. This reading sits in the middle: it coordinates on irreversibility principle but extracts by controlling which sources the principle applies to.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tail_dominance_asymmetry,
    'Is tail-risk dominance applied symmetrically across all energy sources, or does the reading suppress expected-value defenses for nuclear while leaving fossil-fuel climate risks under probabilistic discounting?',
    'Audit regulatory decisions and policy documents to measure how frequently tail-dominance framing is invoked against nuclear vs. fossil sources; compare the distribution of irreversibility language across domains.',
    'If asymmetric (tail-dominance for nuclear, expected-value for fossil), the constraint is extractive and the reading''s beneficiary structure holds. If symmetric, the constraint is more purely coordinative on irreversibility principle. Asymmetry proves the ''extraction via selective framing'' mechanism; symmetry revises classification upward toward rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tail_dominance_asymmetry, empirical, 'Whether tail-dominance is a universal principle or a selective framing device.').

omega_variable(
    waste_repository_time_horizon_fit,
    'Is the civilizational time-horizon assumption for waste repositories justified by actual containment science, or does it overstate the tail-risk magnitude and understate engineering capacity to maintain containment?',
    'Review geological and engineering literature on waste containment timescales; compare repository performance data with failure-mode probability distributions over thousand-year horizons.',
    'If the civilizational horizon is overstated, tail-dominance becomes unjustified and the constraint reclassifies toward snare (pure suppression of alternatives). If justified, the constraint''s extractiveness is lower and the coordination logic is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(waste_repository_time_horizon_fit, empirical, 'Whether waste storage risk justifies civilization-scale irreversibility claims.').

omega_variable(
    kernel_reading_incommensurability,
    'Are the three readings of the acceptable_risk_for_energy kernel genuinely incommensurable (logically incompatible at the framework level), or do they represent different weightings of the same underlying risk metrics?',
    'Formal analysis of whether each reading''s core premise (tail-dominance, expected-value optimization, comparative trade-off) logically entails the rejection of the others'' premises, or whether a single framework could in principle hold all three simultaneously.',
    'If genuinely incommensurable, the readings constitute separate constraint stories with separate stakeholder structures (no family decomposition needed — they are alternative commitments). If compatible, they form a constraint family where the kernel is a single dispute over weighting, and each story should declare its weight-choice explicitly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the kernel readings are logically incompatible or differently-weighted variants of the same calculus.').

omega_variable(
    intergenerational_burden_bearer_identity,
    'Who bears the intergenerational burden: future generations as an abstract entity (non-agent, per the schema), or specific institutional actors (states, corporations, waste-repository operators) who are stewards for future generations and can be held accountable today?',
    'Examine whether the reading''s advocacy focuses on abstract moral burdens (treating future generations as a non-agent principal) or on concrete institutional stewardship obligations and liability structures.',
    'If abstract, the reading lacks a clear payer seat and the constraint operates as deontological suppression (no identifiable agent bears the cost today). If institutional, the constraint''s payer structure clarifies and the extraction mechanism becomes concrete. This affects whether the constraint is snare (pure suppression) or tangled_rope (extraction via institutional responsibility).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_burden_bearer_identity, conceptual, 'Whether intergenerational burden names a non-agent principle or concrete institutional accountability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__catastrophic_tail_dominant, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 0, 0.28).
narrative_ontology:measurement(acce_tr_t8, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 8, 0.32).
narrative_ontology:measurement(acce_tr_t16, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 16, 0.36).
narrative_ontology:measurement(acce_tr_t25, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 25, 0.4).
narrative_ontology:measurement(acce_tr_t35, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 35, 0.42).
narrative_ontology:measurement(acce_tr_t50, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(acce_be_t8, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(acce_be_t16, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(acce_be_t25, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(acce_be_t35, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(acce_be_t50, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(acce_su_t8, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(acce_su_t16, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 16, 0.65).
narrative_ontology:measurement(acce_su_t25, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 25, 0.69).
narrative_ontology:measurement(acce_su_t35, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 35, 0.71).
narrative_ontology:measurement(acce_su_t50, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 50, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__catastrophic_tail_dominant, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.14).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy__expected_value_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy__comparative_risk_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_waste_storage_permanence).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, climate_tail_risk_acknowledgment).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel acceptable_risk_for_energy. The catastrophic_tail_dominant reading elevates irreversibility and intergenerational burden above expected-value optimization. The sibling readings (expected_value_dominant, comparative_risk_dominant) are separate constraint stories with their own ε values, stakeholder structures, and measurements. All three affect each other through the shared kernel and policy-authority competition. The network edges point to downstream constraints that depend on the tail-dominance vs. expected-value choice: nuclear_waste_storage_permanence presupposes tail-dominance framing, while climate_tail_risk_acknowledgment is a vindicated proposition of this reading but contested in expected-value readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_for_energy__catastrophic_tail_dominant, powerless, 1.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
