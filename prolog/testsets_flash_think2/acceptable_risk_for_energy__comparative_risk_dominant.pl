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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   This constraint represents the 'comparative_risk_dominant' reading of
 *   acceptable energy risk, where nuclear power's risks are deemed acceptable
 *   only when weighed against the greater, more immediate threats of fossil
 *   fuel emissions and climate change. It asserts that no absolute threshold
 *   for nuclear risk should impede urgent decarbonization. The constraint
 *   functions as a policy framework that coordinates energy transition
 *   efforts but extracts from those who bear the specific, concentrated risks
 *   of nuclear power, while suppressing alternative risk assessment framings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__comparative_risk_dominant, 0.65).
domain_priors:suppression_score(acceptable_risk_for_energy__comparative_risk_dominant, 0.6).
domain_priors:theater_ratio(acceptable_risk_for_energy__comparative_risk_dominant, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, extractiveness, 0.65).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__comparative_risk_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__comparative_risk_dominant, "Comparative Risk Dominant Nuclear Acceptability").
narrative_ontology:topic_domain(acceptable_risk_for_energy__comparative_risk_dominant, "risk_assessment/energy_policy/public_safety_governance").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__comparative_risk_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__comparative_risk_dominant, '00126dad-132f-49c5-a5f2-b9643b8987c0').
narrative_ontology:cs_kernel_codification('00126dad-132f-49c5-a5f2-b9643b8987c0', formalized).
narrative_ontology:cs_authority_grounding('00126dad-132f-49c5-a5f2-b9643b8987c0', expertise).
narrative_ontology:cs_interpretation_layer_present('00126dad-132f-49c5-a5f2-b9643b8987c0').
narrative_ontology:cs_reading_relation('00126dad-132f-49c5-a5f2-b9643b8987c0', acceptable_risk_for_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('00126dad-132f-49c5-a5f2-b9643b8987c0', acceptable_risk_for_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_axiom('00126dad-132f-49c5-a5f2-b9643b8987c0', foundational, climate_urgency_trumps_absolute_nuclear_risk).
narrative_ontology:cs_axiom_status(climate_urgency_trumps_absolute_nuclear_risk, holdable).
narrative_ontology:cs_axiom_grounding('00126dad-132f-49c5-a5f2-b9643b8987c0', climate_urgency_trumps_absolute_nuclear_risk, empirically_contingent).
narrative_ontology:cs_axiom('00126dad-132f-49c5-a5f2-b9643b8987c0', secondary, intergenerational_waste_manageable_by_technology).
narrative_ontology:cs_axiom_status(intergenerational_waste_manageable_by_technology, holdable).
narrative_ontology:cs_axiom_grounding('00126dad-132f-49c5-a5f2-b9643b8987c0', intergenerational_waste_manageable_by_technology, empirically_contingent).
narrative_ontology:cs_reference_frame('00126dad-132f-49c5-a5f2-b9643b8987c0', urgent_decarbonization_imperative).
narrative_ontology:cs_drift_state('00126dad-132f-49c5-a5f2-b9643b8987c0', contemporary_climate_crisis, gap(stable, minor, true)).
narrative_ontology:cs_created_at('00126dad-132f-49c5-a5f2-b9643b8987c0', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_industry).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, governments_seeking_energy_transition).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, local_communities_near_nuclear_sites).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, future_generations).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, anti_nuclear_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promotes nuclear power as a necessary solution for climate change, framing its risks as acceptable when compared to fossil fuels. Benefits from policy decisions that prioritize comparative risk over absolute thresholds, enabling new plant construction and operation.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_industry, agenda_setter,
    institutional, generational, mobile, global).

% Adopts and enforces policies based on comparative risk assessment to meet decarbonization targets and ensure energy security. Benefits from a framework that allows for nuclear expansion, but also bears political costs from anti-nuclear opposition.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, governments_seeking_energy_transition, agenda_setter,
    institutional, generational, constrained, national).

% Are the ultimate beneficiaries of policies that rapidly reduce fossil fuel emissions, even if it means accepting some nuclear risk. Their immediate survival and well-being are prioritized over long-term, low-probability nuclear tail risks.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations, beneficiary,
    powerless, immediate, trapped, global).

% Bear the direct, concentrated risks of nuclear power generation, including potential accidents, waste storage, and environmental impact. Their concerns about absolute safety thresholds are often downplayed in favor of broader climate goals.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, local_communities_near_nuclear_sites, payer,
    powerless, biographical, constrained, local).

% Will inherit the long-lived radioactive waste and potential environmental legacy of nuclear power. This reading's emphasis on temporal urgency often overrides their intergenerational burden, treating waste management as a solvable technical problem.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, future_generations, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(acceptable_risk_for_energy__comparative_risk_dominant, future_generations).

% Actively resist nuclear expansion, emphasizing the absolute and catastrophic risks of nuclear power, particularly waste and accident potential. Their arguments for absolute safety thresholds are often marginalized by the comparative risk framework.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, anti_nuclear_advocates, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__comparative_risk_dominant, anti_nuclear_advocates, excluded).

% Are tasked with implementing safety standards for nuclear facilities within the policy framework. They must balance the imperative for climate action with their mandate for public safety, often interpreting 'acceptable risk' through the comparative lens.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, environmental_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Focus on low-probability, high-consequence events and irreversibility. Their analytical framework, which would emphasize the unique tail risks of nuclear power, is often excluded from the dominant comparative risk discourse.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, catastrophic_risk_analysts, excluded,
    analytical, civilizational, analytical, global).

% Typically weigh risks by probability times consequence. While their methods are quantitative, their focus on annual expected costs and benefits can overlook the specific temporal and intergenerational aspects that the comparative risk framework also downplays, but for different reasons.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, expected_value_economists, excluded,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates energy policy and public discourse to prioritize rapid decarbonization by framing nuclear power's risks as acceptable when compared to the more immediate and widespread risks of fossil fuels and climate change.
% TRANSFER_FUNCTION: Transfers the burden of climate change risk from global populations to local communities and future generations (who bear the concentrated, long-term risks of nuclear waste and potential accidents). It also transfers political capital and investment to the nuclear industry.
% ABSENT_VOICES: Catastrophic risk analysts, who would emphasize the unique, irreversible tail risks of nuclear power, and future generations, who bear the intergenerational burden of waste, are structurally marginalized or conceptually absent from the dominant policy discourse.
% DISAPPEARANCE_RATIONALE: If this comparative risk framework vanished, energy policy would likely revert to more stringent, absolute nuclear safety thresholds, potentially slowing decarbonization efforts or increasing reliance on fossil fuels due to a lack of viable alternatives. The energy transition landscape would fundamentally reorganize.
% FOUNDING_PROBLEM: The urgent need to decarbonize global energy grids and mitigate climate change, while maintaining energy security and economic stability, in the face of persistent reliance on fossil fuels and public apprehension about nuclear power.
% FOUNDING_PROBLEM_CORROBORATION: IPCC reports, national energy security assessments, climate scientists, and many public health organizations corroborate the urgency of the decarbonization problem. While anti-nuclear groups contest the solution, the problem itself is widely acknowledged by independent experts.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__comparative_risk_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__comparative_risk_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__comparative_risk_dominant, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(acceptable_risk_for_energy__comparative_risk_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__comparative_risk_dominant, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__comparative_risk_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_for_energy__comparative_risk_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_for_energy__comparative_risk_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the transfer of risk burden from global climate-vulnerable populations to local communities and future generations. Suppression (0.60) is necessary to marginalize arguments for absolute nuclear safety or alternative energy pathways that might be slower. The theater ratio (0.30) is moderate, as the comparative risk assessment is a genuine analytical tool, but it can be selectively applied to downplay certain nuclear risks. The increasing extractiveness and suppression over time reflect the growing urgency of climate action, which amplifies the pressure to accept nuclear risks.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the nuclear industry and governments, this framework is a necessary and rational coordination mechanism for climate action. From the perspective of local communities and anti-nuclear advocates, it is an extractive mechanism that externalizes specific, severe risks for a diffuse, global benefit. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The nuclear industry and governments seeking energy transition are beneficiaries and agenda-setters, as this framework enables their objectives. Climate-vulnerable populations are also beneficiaries, as the framework aims to mitigate their immediate threats. Local communities near nuclear sites, future generations, and anti-nuclear advocates are victims, bearing the concentrated costs and having their concerns about absolute risk suppressed. Catastrophic risk analysts and expected value economists are excluded, as their analytical framings are not dominant in this discourse.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint''s framing of nuclear risk truly a comparative assessment, or is it a rhetorical device to justify nuclear expansion despite its inherent risks?',
    'Analysis of policy outcomes in jurisdictions adopting this framework: if nuclear expansion proceeds even when non-nuclear decarbonization pathways are demonstrably faster and cheaper, it suggests rhetorical justification over genuine comparative assessment.',
    'If primarily rhetorical, the constraint''s effective extractiveness and suppression are higher, and its coordination function is weaker, potentially reclassifying it closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity between genuine comparative risk assessment and rhetorical justification for nuclear expansion.').

omega_variable(
    intergenerational_burden_valuation,
    'How should the intergenerational burden of nuclear waste be quantitatively weighed against the immediate benefits of climate change mitigation?',
    'Development of intergenerational equity frameworks that incorporate long-term discount rates and non-monetary valuations of environmental legacy, with broad societal consensus.',
    'A higher valuation of intergenerational burden would increase the perceived extractiveness of this constraint from future generations, potentially shifting policy towards alternatives with lower long-term liabilities.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_burden_valuation, preference, 'Valuation of intergenerational nuclear waste burden versus immediate climate benefits.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of absolute safety arguments structural (e.g., regulatory capture) or internalized (e.g., self-censorship by experts prioritizing climate goals)?',
    'Post-policy-shift analysis: if absolute safety arguments re-emerge strongly after the policy framework shifts away from comparative risk dominance, it suggests structural suppression. If they remain muted, it suggests internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for absolute safety arguments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__comparative_risk_dominant, 2000, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t2000, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(acce_tr_t2005, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2005, 0.27).
narrative_ontology:measurement(acce_tr_t2010, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2010, 0.28).
narrative_ontology:measurement(acce_tr_t2015, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2015, 0.29).
narrative_ontology:measurement(acce_tr_t2020, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2020, 0.3).
narrative_ontology:measurement(acce_tr_t2025, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(acce_be_t2000, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(acce_be_t2005, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(acce_be_t2010, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(acce_be_t2015, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement(acce_be_t2020, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2020, 0.64).
narrative_ontology:measurement(acce_be_t2025, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2025, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t2000, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement(acce_su_t2005, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2005, 0.5).
narrative_ontology:measurement(acce_su_t2010, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement(acce_su_t2015, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2015, 0.58).
narrative_ontology:measurement(acce_su_t2020, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2020, 0.59).
narrative_ontology:measurement(acce_su_t2025, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2025, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__comparative_risk_dominant, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'acceptable_risk_for_energy' kernel, alongside 'catastrophic_tail_dominant' and 'expected_value_dominant'. Each reading represents a distinct policy framework for evaluating energy risks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
