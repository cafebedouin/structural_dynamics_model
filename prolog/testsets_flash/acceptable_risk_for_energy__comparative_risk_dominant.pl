% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__comparative_risk_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: acceptable_risk_for_energy__comparative_risk_dominant
 *   human_readable: Comparative Risk Dominant Nuclear Acceptability
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   This constraint describes the framework where the acceptability of
 *   nuclear energy's risks is judged not by an absolute safety threshold, but
 *   by its comparison to the risks posed by other energy sources,
 *   particularly fossil fuels and their contribution to climate change. This
 *   reading prioritizes the urgency of climate action, accepting certain
 *   nuclear risks as a 'lesser evil' to avoid greater environmental
 *   catastrophe. The victim set explicitly includes climate-vulnerable
 *   populations and future generations, who bear both climate and nuclear
 *   risks.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__comparative_risk_dominant, 0.4).
domain_priors:suppression_score(acceptable_risk_for_energy__comparative_risk_dominant, 0.3).
domain_priors:theater_ratio(acceptable_risk_for_energy__comparative_risk_dominant, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, extractiveness, 0.4).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__comparative_risk_dominant, rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__comparative_risk_dominant, "Comparative Risk Dominant Nuclear Acceptability").
narrative_ontology:topic_domain(acceptable_risk_for_energy__comparative_risk_dominant, "risk_assessment/energy_policy/public_safety_governance").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__comparative_risk_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__comparative_risk_dominant, 'dcf500fe-9391-4f49-b240-2fcacbf22608').
narrative_ontology:cs_kernel_codification('dcf500fe-9391-4f49-b240-2fcacbf22608', formalized).
narrative_ontology:cs_authority_grounding('dcf500fe-9391-4f49-b240-2fcacbf22608', expertise).
narrative_ontology:cs_interpretation_layer_present('dcf500fe-9391-4f49-b240-2fcacbf22608').
narrative_ontology:cs_reading_relation('dcf500fe-9391-4f49-b240-2fcacbf22608', acceptable_risk_for_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('dcf500fe-9391-4f49-b240-2fcacbf22608', acceptable_risk_for_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_axiom('dcf500fe-9391-4f49-b240-2fcacbf22608', foundational, climate_catastrophe_is_dominant_risk).
narrative_ontology:cs_axiom_status(climate_catastrophe_is_dominant_risk, holdable).
narrative_ontology:cs_axiom_grounding('dcf500fe-9391-4f49-b240-2fcacbf22608', climate_catastrophe_is_dominant_risk, empirically_contingent).
narrative_ontology:cs_axiom('dcf500fe-9391-4f49-b240-2fcacbf22608', foundational, no_absolute_nuclear_risk_threshold).
narrative_ontology:cs_axiom_status(no_absolute_nuclear_risk_threshold, holdable).
narrative_ontology:cs_axiom_grounding('dcf500fe-9391-4f49-b240-2fcacbf22608', no_absolute_nuclear_risk_threshold, conventional).
narrative_ontology:cs_reference_frame('dcf500fe-9391-4f49-b240-2fcacbf22608', climate_urgency_comparative_advantage).
narrative_ontology:cs_drift_state('dcf500fe-9391-4f49-b240-2fcacbf22608', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('dcf500fe-9391-4f49-b240-2fcacbf22608', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_energy_proponents).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, climate_action_advocates).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, future_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for nuclear power as a necessary tool for decarbonization, framing its risks as acceptable when compared to the catastrophic impacts of climate change and fossil fuels. They actively shape policy and public discourse.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_energy_proponents, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the comparative risk framing as it supports rapid decarbonization efforts, even if it means accepting certain nuclear risks. Their primary goal is climate mitigation, and nuclear power is seen as a viable, if imperfect, solution.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, climate_action_advocates, beneficiary,
    organized, generational, constrained, global).

% Bear the immediate and long-term consequences of climate change, making the comparative risk argument compelling for them. However, they also bear the residual risks of nuclear energy (e.g., waste, accidents) without direct agency in the decision-making process.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations, payer,
    powerless, immediate, trapped, global).

% Will inherit both the consequences of climate change and the long-lived radioactive waste from nuclear power. Their interests are represented by proxy, and the comparative risk framework prioritizes present-day climate action over absolute minimization of future nuclear risks.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Would argue against the premise that their risks are inherently worse than nuclear, or that nuclear is a necessary alternative. Their business model is directly challenged by this comparative risk framework, which elevates nuclear as a 'lesser evil'.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, fossil_fuel_industry, excluded,
    institutional, biographical, constrained, global).

% Are tasked with ensuring the safety of nuclear operations. While they operate within the comparative risk framework, their mandate is to minimize nuclear risks within that context, not to question the framework itself. They enforce safety standards.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_safety_regulators, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates energy policy decisions by providing a framework to evaluate nuclear power's risks and benefits against those of other energy sources, particularly fossil fuels, to achieve climate goals.
% TRANSFER_FUNCTION: Transfers the burden of certain long-term, low-probability nuclear risks to future generations and climate-vulnerable populations, in exchange for immediate climate benefits and energy security for current populations.
% ABSENT_VOICES: Future generations and populations most directly impacted by nuclear waste or accidents (who may not be climate-vulnerable) are absent from the direct decision-making. They would likely advocate for a more absolute risk threshold for nuclear power, independent of other energy sources.
% DISAPPEARANCE_RATIONALE: If this comparative risk framework vanished, the justification for expanding or maintaining nuclear power would collapse, leading to a re-evaluation of energy portfolios. Climate action would face a significant hurdle without nuclear as a 'lesser evil' option, and the energy transition would slow or shift dramatically.
% FOUNDING_PROBLEM: The dual challenge of energy demand and climate change, coupled with the perceived high absolute risk of nuclear power, created a need for a framework to justify nuclear's role in a decarbonized future.
% FOUNDING_PROBLEM_CORROBORATION: International energy agencies, climate scientists, and many national governments corroborate that the dual problem of energy security and climate change is live. Independent risk analysts also attest to the need for comparative risk assessment in complex systems, though they may dispute the specific weighting.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__comparative_risk_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__comparative_risk_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__comparative_risk_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(acceptable_risk_for_energy__comparative_risk_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__comparative_risk_dominant, 0.4, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.4) reflects the burden of long-term nuclear waste and accident potential, which is accepted as a cost within this framework. Suppression (0.3) is moderate, as there is active debate, but the comparative risk framing itself suppresses arguments for absolute nuclear safety. Theater ratio is low (0.1) as the risk assessment is largely functional, though it may downplay certain tail risks. The claimed type is 'rope' because it genuinely coordinates a complex policy problem, but the metrics indicate a degree of extraction from those who bear the residual risks.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of nuclear energy proponents, this is a necessary and rational coordination mechanism for climate action. From the perspective of future generations or those advocating for absolute nuclear safety, it may appear as a 'tangled rope' or 'snare' that externalizes significant risks for present-day benefits.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear energy proponents and climate action advocates are beneficiaries, as this framework justifies their policy goals. Climate-vulnerable populations and future generations are payers, as they bear the costs of both climate change and the accepted nuclear risks. Nuclear safety regulators act as agenda-setters, enforcing safety within this comparative framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    absolute_vs_comparative_risk,
    'Is nuclear risk fundamentally an absolute concern (requiring an independent safety threshold) or a comparative one (acceptable relative to other energy risks)?',
    'A societal consensus shift on the ethical weighting of different types of risk, or a scientific breakthrough that fundamentally alters the nature of nuclear waste or accident probability.',
    'If an absolute threshold becomes dominant, the justification for current nuclear expansion would collapse, reclassifying this constraint as a ''snare'' for those bearing the risks. If comparative risk is universally accepted, it would solidify as a ''mountain'' of policy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(absolute_vs_comparative_risk, conceptual, 'The fundamental framing of nuclear risk assessment.').

omega_variable(
    intergenerational_equity_weighting,
    'How should the risks borne by future generations (e.g., nuclear waste, long-term climate impacts) be weighted against immediate climate benefits for current generations?',
    'Development of robust intergenerational equity frameworks in policy and ethics, with mechanisms for future generations to have agency in current decisions.',
    'A higher weighting for future generations'' risks would increase the perceived extractiveness of this constraint, potentially shifting it towards a ''tangled_rope'' or ''snare'' if the current balance is deemed unjust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_equity_weighting, preference, 'Ethical weighting of intergenerational risks and benefits.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__comparative_risk_dominant, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t1970, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(acce_tr_t1990, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(acce_tr_t2010, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(acce_tr_t2024, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(acce_be_t1970, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 1970, 0.3).
narrative_ontology:measurement(acce_be_t1990, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(acce_be_t2010, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement(acce_be_t2024, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2024, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t1970, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 1970, 0.2).
narrative_ontology:measurement(acce_su_t1990, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 1990, 0.25).
narrative_ontology:measurement(acce_su_t2010, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2010, 0.3).
narrative_ontology:measurement(acce_su_t2024, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__comparative_risk_dominant, resource_allocation).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, climate_change_mitigation_targets).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, fossil_fuel_subsidies).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
