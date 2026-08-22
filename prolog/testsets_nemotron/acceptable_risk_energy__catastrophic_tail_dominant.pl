% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__catastrophic_tail_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: acceptable_risk_energy__catastrophic_tail_dominant
 *   human_readable: Catastrophic-Tail-Dominant Acceptable Risk Criterion in Energy Policy
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   This constraint story captures the catastrophic-tail-dominant reading of
 *   acceptable risk in energy policy: a risk criterion that assigns infinite
 *   or near-infinite weight to low-probability catastrophic outcomes
 *   (specifically nuclear accidents) while discounting distributed,
 *   continuous harms from fossil fuel energy systems as reversible or
 *   acceptable. The constraint operates through nuclear regulatory frameworks
 *   (ALARA, LNT, defense-in-depth) that structurally suppress nuclear
 *   deployment, transferring its extractive surplus to fossil fuel incumbents
 *   and the radiation protection establishment, while the victims —
 *   pollution-exposed communities, climate-vulnerable populations, and energy
 *   consumers — bear the displaced harm. The claim/metric gap is deliberate:
 *   the constraint is CLAIMED as a genuine coordination function (rope-like
 *   safety governance) while the authored metrics describe a substantially
 *   extractive, actively enforced regime that coordinates AND extracts — the
 *   engine measures that divergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__catastrophic_tail_dominant, 0.78).
domain_priors:suppression_score(acceptable_risk_energy__catastrophic_tail_dominant, 0.87).
domain_priors:theater_ratio(acceptable_risk_energy__catastrophic_tail_dominant, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, extractiveness, 0.78).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, accessibility_collapse, 0.76).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, resistance, 0.63).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__catastrophic_tail_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__catastrophic_tail_dominant, "Catastrophic-Tail-Dominant Acceptable Risk Criterion in Energy Policy").
narrative_ontology:topic_domain(acceptable_risk_energy__catastrophic_tail_dominant, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__catastrophic_tail_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__catastrophic_tail_dominant, '95ed2261-2be9-4371-a574-aa8cdca6dcd2').
narrative_ontology:cs_kernel_codification('95ed2261-2be9-4371-a574-aa8cdca6dcd2', formalized).
narrative_ontology:cs_authority_grounding('95ed2261-2be9-4371-a574-aa8cdca6dcd2', extraction).
narrative_ontology:cs_interpretation_layer_present('95ed2261-2be9-4371-a574-aa8cdca6dcd2').
narrative_ontology:cs_reading_relation('95ed2261-2be9-4371-a574-aa8cdca6dcd2', acceptable_risk_energy__expected_value_dominant, forecloses).
narrative_ontology:cs_reading_relation('95ed2261-2be9-4371-a574-aa8cdca6dcd2', acceptable_risk_energy__option_value_preserving, influences).
narrative_ontology:cs_axiom('95ed2261-2be9-4371-a574-aa8cdca6dcd2', foundational, radiological_catastrophe_infinite_weight).
narrative_ontology:cs_axiom_status(radiological_catastrophe_infinite_weight, holdable).
narrative_ontology:cs_axiom_grounding('95ed2261-2be9-4371-a574-aa8cdca6dcd2', radiological_catastrophe_infinite_weight, deontological).
narrative_ontology:cs_axiom('95ed2261-2be9-4371-a574-aa8cdca6dcd2', foundational, distributed_harm_moral_discounting).
narrative_ontology:cs_axiom_status(distributed_harm_moral_discounting, holdable).
narrative_ontology:cs_axiom_grounding('95ed2261-2be9-4371-a574-aa8cdca6dcd2', distributed_harm_moral_discounting, conventional).
narrative_ontology:cs_reference_frame('95ed2261-2be9-4371-a574-aa8cdca6dcd2', precautionary_nuclear_governance).
narrative_ontology:cs_drift_state('95ed2261-2be9-4371-a574-aa8cdca6dcd2', post_fukushima_pra_maturation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('95ed2261-2be9-4371-a574-aa8cdca6dcd2', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_regulatory_agencies).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_incumbents).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, radiation_protection_establishment).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, populations_exposed_to_fossil_fuel_pollution).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_industry_developers).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, energy_consumers_facing_higher_costs).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, climate_vulnerable_populations).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__catastrophic_tail_dominant, precautionary_principle_as_tail_risk_dominance).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_exceptionalism_doctrine).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__catastrophic_tail_dominant, distributed_harm_moral_discounting).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the ALARA (As Low As Reasonably Achievable) framework and licensing standards that embed catastrophic-tail-dominant risk criteria. Their institutional mandate, budget authority, and professional identity are constituted through the nuclear-exceptionalist risk regime. They set the agenda for what counts as 'acceptable' and enforce compliance through licensing gatekeeping.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_regulatory_agencies, agenda_setter,
    institutional, generational, arbitrage, national).

% Professional bodies (ICRP, national radiation protection agencies) whose epistemic authority, funding streams, and career structures depend on the linear no-threshold (LNT) model and the normative priority given to radiological risk over all other energy harms. Their identity is fused to the premise that radiation risk is categorically distinct.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, radiation_protection_establishment, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__catastrophic_tail_dominant, radiation_protection_establishment, agenda_setter).

% Oil, gas, and coal interests who benefit structurally from the suppression of nuclear deployment. The catastrophic-tail-dominant criterion raises nuclear's regulatory burden and cost profile, preserving fossil fuel market share. They do not administer the constraint but capture its extractive surplus indirectly.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_incumbents, beneficiary,
    powerful, biographical, arbitrage, global).

% Bear the compliance costs, licensing delays, and capital cost escalation driven by tail-dominant risk standards. Their exit options are limited: they can abandon projects, shift to jurisdictions with different regimes (constrained), or accept the regulatory premium. The constraint extracts from them through prolonged licensing and design requirements justified by catastrophic tail scenarios.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_industry_developers, payer,
    organized, biographical, constrained, global).

% Communities bearing the distributed, continuous mortality and morbidity from fossil fuel combustion (air pollution, occupational hazards, climate impacts). Their harm is discounted as 'distributed and reversible' by the tail-dominant criterion. They have no organized voice in risk-standard setting and cannot exit their exposure.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, populations_exposed_to_fossil_fuel_pollution, payer,
    powerless, biographical, trapped, local).

% Ratepayers and industrial users who absorb the cost premium of nuclear energy under tail-dominant regulation, or the system costs of fossil dependence where nuclear is suppressed. Exit is constrained by grid geography and regulatory monopoly.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, energy_consumers_facing_higher_costs, payer,
    moderate, immediate, constrained, national).

% Populations disproportionately harmed by climate change whose mitigation pathway is narrowed by the constraint's suppression of nuclear deployment. The tail-dominant criterion treats nuclear catastrophe as infinite-weight while treating climate catastrophe as distributed/reversible, creating an asymmetric risk weighting that delays decarbonization.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, climate_vulnerable_populations, payer,
    powerless, generational, trapped, global).

% Academic researchers in risk analysis, decision theory, and energy systems who study the structural properties of risk criteria. They observe the constraint's operation across jurisdictions and trace its empirical consequences for energy system outcomes.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, decision_theory_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a socially legitimate basis for nuclear energy governance by prioritizing prevention of low-probability, high-consequence radiological events, creating a unified safety standard that coordinates regulatory action across jurisdictions and maintains public trust in nuclear oversight.
% TRANSFER_FUNCTION: Moves regulatory burden, capital cost, and deployment delay from the fossil fuel system and the radiation protection establishment onto nuclear developers and energy consumers, while transferring the avoided fossil fuel mortality (which would occur under nuclear deployment) onto pollution-exposed populations and climate-vulnerable groups as uncounted harm.
% ABSENT_VOICES: Populations exposed to fossil fuel pollution and climate-vulnerable populations are structurally excluded from the risk-standard-setting process. Their harm is rendered invisible by the criterion's moral discounting of distributed/reversible harm. They would object to a risk framework that treats their ongoing mortality as acceptable while treating hypothetical nuclear fatalities as infinite-weight vetoes.
% DISAPPEARANCE_RATIONALE: If the catastrophic-tail-dominant criterion vanished overnight, nuclear licensing would shift to expected-value or option-value frameworks within years, deployment costs and timelines would fall dramatically, fossil fuel displacement would accelerate, and the radiation protection establishment would lose its governing normative premise. The global energy system would reorganize around a different risk calculus.
% FOUNDING_PROBLEM: Early nuclear energy development faced genuine uncertainty about low-probability, high-consequence accident scenarios (core melt, containment failure, widespread contamination). The tail-dominant criterion was built to provide a conservative governance framework that would prevent catastrophic outcomes while the technology matured, and to maintain public legitimacy for a novel energy source with unprecedented accident potential.
% FOUNDING_PROBLEM_CORROBORATION: The nuclear regulatory agencies and radiation protection establishment attest the founding problem remains live, citing ongoing uncertainty about severe accident progression and the imperative of maintaining public trust. Nuclear industry analysts, energy system modelers, and climate policy researchers outside the benefiting parties attest the founding problem is substantially resolved: decades of operational experience, probabilistic risk assessment maturation, and empirical evidence from Fukushima and Three Mile Island demonstrate that tail risks are quantifiable and manageable, and that the criterion now functions as a deployment suppression mechanism rather than a genuine safety necessity.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__catastrophic_tail_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__catastrophic_tail_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__catastrophic_tail_dominant, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(acceptable_risk_energy__catastrophic_tail_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__catastrophic_tail_dominant, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is high (0.78) because the constraint transfers massive regulatory cost and deployment suppression onto nuclear developers and energy consumers while the coordination function (radiological safety) could be achieved at far lower cost through expected-value or option-value frameworks. Suppression is very high (0.87) because the constraint's persistence depends on active exclusion of nuclear pathways through licensing gatekeeping, not participant preference — the nuclear pathway is structurally suppressed. Theater ratio is low-moderate (0.22): the safety function is real but a growing share of regulatory activity defends the tail-dominant criterion itself rather than achieving marginal safety gains. Accessibility collapse is high (0.76) because the LNT/ALARA framework has become the epistemic infrastructure of nuclear governance — alternatives are not just discouraged but rendered unintelligible within the regulatory paradigm. Resistance is substantial (0.63) from nuclear advocates, climate policy communities, and some radiation health scientists, but remains fragmented across domains.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter/beneficiary seats (regulators, radiation protection) experience this constraint as genuine coordination — they built it, maintain it, and believe it prevents catastrophe. The payer seats (nuclear developers, pollution-exposed communities, climate-vulnerable populations) experience it as enforced extraction that suppresses a superior energy pathway and displaces harm onto them. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear regulatory agencies and the radiation protection establishment are structural beneficiaries (d near 0.0) — they collect institutional authority, budget, and professional identity from the constraint. Fossil fuel incumbents are indirect beneficiaries (d ~0.2) — they capture market preservation without administering the constraint. Nuclear developers, pollution-exposed populations, energy consumers, and climate-vulnerable populations are targets (d near 1.0) — they bear the extraction through compliance costs, displaced harm, higher energy costs, and narrowed decarbonization pathways. Decision theory scholars are analytical observers (d=0.5). The identity_locked exit for the radiation protection establishment reflects professional identity fusion: their epistemic framework constitutes their professional self-concept, making exit unthinkable without identity dissolution.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits strong mandatrophy: the founding problem (genuine uncertainty about nuclear tail risks in the 1950s-70s) has been substantially resolved by operational experience and PRA maturation, yet the constraint persists and has intensified. The radiation protection establishment's identity_locked position prevents internal reform; fossil fuel incumbents' indirect benefit creates external political economy pressure for persistence. The constraint now functions primarily to maintain the institutional and professional arrangements that depend on it, not to solve its original safety problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the catastrophic-tail-dominant criterion a genuine safety necessity or a constructed constraint that benefits identifiable institutional and commercial actors?',
    'Compare regulatory cost per life-year saved across energy pathways under tail-dominant vs. expected-value frameworks; trace institutional incentives of radiation protection bodies; analyze fossil fuel industry lobbying on nuclear regulation.',
    'If constructed, the constraint is a false summit (mountain claim masking tangled_rope extraction) and FSM triggers; if genuine necessity, the high extraction is the price of coordination and the claimed rope/tangled_rope classification is descriptively accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the tail-dominant risk criterion reflects irreducible physical reality or institutional construction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (regulatory gatekeeping, licensing barriers) or internalized (industry self-censorship, public fear conditioned by the regulatory framework)?',
    'Counterfactual analysis: if licensing barriers were removed but public opposition remained, how much deployment would occur? Survey nuclear developers on whether suppression is primarily external or internalized.',
    'If substantially internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them. This would increase the constraint''s effective extractiveness for nuclear developers and strengthen the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in nuclear deployment.').

omega_variable(
    fossil_fuel_beneficiary_causality,
    'Does the fossil fuel industry actively maintain the tail-dominant criterion, or do they merely benefit from it passively?',
    'Documentary evidence of fossil fuel lobbying on nuclear regulatory reform, funding of radiation risk research, or opposition to LNT model revision.',
    'If active maintenance, fossil fuel incumbents shift from indirect beneficiaries to co-agenda-setters, altering the constraint''s power topology and strengthening the snare-like extraction dynamics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fossil_fuel_beneficiary_causality, empirical, 'Active vs. passive beneficiary role of fossil fuel interests.').

omega_variable(
    climate_harm_discounting_justification,
    'Is the moral discounting of climate harm (as distributed/reversible) a coherent ethical position or a motivated reasoning artifact of the tail-dominant framework?',
    'Philosophical analysis of intergenerational risk ethics; comparison of discount rates applied to radiological vs. climate tails in integrated assessment models.',
    'If motivated reasoning, the constraint''s victim set is artificially constructed — climate-vulnerable populations are excluded from victim status by the criterion''s own internal logic, not by empirical difference in harm character.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_harm_discounting_justification, conceptual, 'Epistemic status of the distributed-harm discounting premise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__catastrophic_tail_dominant, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t1970, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 1970, 0.08).
narrative_ontology:measurement(acce_tr_t1980, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(acce_tr_t1990, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(acce_tr_t2000, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(acce_tr_t2010, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(acce_tr_t2024, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(acce_be_t1970, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(acce_be_t1980, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 1980, 0.48).
narrative_ontology:measurement(acce_be_t1990, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 1990, 0.62).
narrative_ontology:measurement(acce_be_t2000, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 2000, 0.71).
narrative_ontology:measurement(acce_be_t2010, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(acce_be_t2024, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t1970, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(acce_su_t1980, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 1980, 0.72).
narrative_ontology:measurement(acce_su_t1990, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 1990, 0.81).
narrative_ontology:measurement(acce_su_t2000, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 2000, 0.84).
narrative_ontology:measurement(acce_su_t2010, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 2010, 0.86).
narrative_ontology:measurement(acce_su_t2024, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 2024, 0.87).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__catastrophic_tail_dominant, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(acceptable_risk_energy__catastrophic_tail_dominant, 0.12).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy__expected_value_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy__option_value_preserving).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_licensing_regime).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_subsidy_structure).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, climate_mitigation_pathway_constraint).

% DUAL FORMULATION NOTE:
% This constraint and its siblings (expected_value_dominant, option_value_preserving) form a constraint family decomposing the 'acceptable risk' kernel. This reading's ε (0.78) differs substantially from expected_value_dominant (estimated ε ~0.25) and option_value_preserving (estimated ε ~0.35), confirming they are structurally distinct constraints, not measurement variants. The tail-dominant reading suppresses the nuclear pathway that the other readings would keep open.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_energy__catastrophic_tail_dominant, organized, 0.15).
constraint_indexing:directionality_override(acceptable_risk_energy__catastrophic_tail_dominant, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
