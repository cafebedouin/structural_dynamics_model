% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__catastrophic_tail_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: acceptable_risk_for_energy__catastrophic_tail_dominant
 *   human_readable: Catastrophic Tail-Risk Dominance in Energy Policy
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   This constraint describes a specific reading of 'acceptable risk' in
 *   energy policy, where the potential for low-probability, high-consequence
 *   events (like nuclear accidents or long-term waste storage) dominates the
 *   risk calculus. This reading prioritizes irreversibility and
 *   intergenerational burden over expected-value optimization, leading to
 *   significant constraints on technologies like nuclear energy. The
 *   constraint is claimed as a Tangled Rope because it coordinates a societal
 *   preference for extreme risk avoidance while extracting costs from
 *   specific industries and consumers, requiring active enforcement to
 *   maintain this prioritization.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.68).
domain_priors:suppression_score(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.75).
domain_priors:theater_ratio(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, extractiveness, 0.68).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__catastrophic_tail_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__catastrophic_tail_dominant, "Catastrophic Tail-Risk Dominance in Energy Policy").
narrative_ontology:topic_domain(acceptable_risk_for_energy__catastrophic_tail_dominant, "risk_assessment/energy_policy/public_safety_governance").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__catastrophic_tail_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__catastrophic_tail_dominant, 'e2950056-ff60-42d3-917c-104b14be9623').
narrative_ontology:cs_kernel_codification('e2950056-ff60-42d3-917c-104b14be9623', formalized).
narrative_ontology:cs_authority_grounding('e2950056-ff60-42d3-917c-104b14be9623', extraction).
narrative_ontology:cs_interpretation_layer_present('e2950056-ff60-42d3-917c-104b14be9623').
narrative_ontology:cs_reading_relation('e2950056-ff60-42d3-917c-104b14be9623', acceptable_risk_for_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_reading_relation('e2950056-ff60-42d3-917c-104b14be9623', acceptable_risk_for_energy__comparative_risk_dominant, coexists_with).
narrative_ontology:cs_axiom('e2950056-ff60-42d3-917c-104b14be9623', foundational, irreversible_catastrophe_trumps_probability).
narrative_ontology:cs_axiom_status(irreversible_catastrophe_trumps_probability, holdable).
narrative_ontology:cs_axiom_grounding('e2950056-ff60-42d3-917c-104b14be9623', irreversible_catastrophe_trumps_probability, deontological).
narrative_ontology:cs_axiom('e2950056-ff60-42d3-917c-104b14be9623', foundational, intergenerational_burden_is_non_negotiable).
narrative_ontology:cs_axiom_status(intergenerational_burden_is_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('e2950056-ff60-42d3-917c-104b14be9623', intergenerational_burden_is_non_negotiable, deontological).
narrative_ontology:cs_reference_frame('e2950056-ff60-42d3-917c-104b14be9623', post_chernobyl_fukushima_risk_aversion).
narrative_ontology:cs_drift_state('e2950056-ff60-42d3-917c-104b14be9623', contemporary_climate_crisis_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('e2950056-ff60-42d3-917c-104b14be9623', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, environmental_advocacy_groups).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, future_generations).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_energy_industry).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, pro_nuclear_scientists).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, energy_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promote the dominance of catastrophic tail-risk in energy policy, emphasizing the irreversibility and intergenerational burden of nuclear waste and potential accidents. They benefit from policy frameworks that prioritize these concerns, aligning with their core mission.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, environmental_advocacy_groups, agenda_setter,
    organized, generational, mobile, global).

% Are conceptually protected from the long-term, irreversible consequences of catastrophic events (e.g., nuclear waste, severe accidents) by policies that prioritize tail-risk. They bear no direct costs but are the ultimate beneficiaries of this risk calculus.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% Bears the significant costs associated with stringent safety regulations, long-term waste disposal requirements, and public opposition driven by catastrophic risk perceptions. This framework makes new plant construction and operation economically challenging.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_energy_industry, payer,
    institutional, biographical, constrained, national).

% Advocate for nuclear power based on probabilistic risk assessments and climate benefits, but their arguments are often suppressed or marginalized by the dominant tail-risk narrative. Their professional identity is tied to a rational, evidence-based approach that is devalued by this constraint.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, pro_nuclear_scientists, payer,
    moderate, biographical, identity_locked, global).

% May face higher energy costs or reduced energy security due to the constrained development of nuclear power, which is often a low-carbon, baseload option. Their immediate economic interests are subordinated to long-term, low-probability catastrophic avoidance.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, energy_consumers, payer,
    moderate, immediate, constrained, national).

% Implement and enforce policies that reflect the catastrophic tail-risk dominance, often under pressure from advocacy groups and public sentiment. Their mandate is to ensure public safety, which this framework defines primarily through extreme event avoidance.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, regulatory_bodies, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates public and political attention on the most severe, albeit rare, potential outcomes of energy choices, aiming to prevent irreversible harm and ensure intergenerational equity in risk bearing.
% TRANSFER_FUNCTION: Transfers the burden of extreme risk avoidance (e.g., higher costs, foregone energy options) from future generations and the environment to current energy producers and consumers, by prioritizing low-probability high-consequence events.
% ABSENT_VOICES: Proponents of purely expected-value optimization, who would argue for a more balanced assessment of all risks and benefits, are often excluded from the core policy-making discourse, as their framing is seen as downplaying critical dangers.
% DISAPPEARANCE_RATIONALE: If this risk calculus vanished, energy policy would immediately shift towards more probabilistic and comparative risk assessments. Nuclear energy's viability would increase, and investment decisions would be driven more by cost-benefit analysis than by extreme event aversion. The entire energy landscape would re-evaluate its priorities.
% FOUNDING_PROBLEM: The problem of managing risks from technologies with potential for irreversible, widespread, and long-lasting harm, particularly after events like Chernobyl and Fukushima, and the challenge of intergenerational equity in risk distribution.
% FOUNDING_PROBLEM_CORROBORATION: Environmental scientists and ethicists corroborate the live status of the problem, emphasizing the unique nature of nuclear waste and accident consequences. The nuclear industry and some economists contest the severity and probability, arguing the problem is overstated relative to other energy risks.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__catastrophic_tail_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__catastrophic_tail_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__catastrophic_tail_dominant, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(acceptable_risk_for_energy__catastrophic_tail_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.68, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high because the policy framework imposes substantial costs on the nuclear industry and energy consumers, disproportionate to the probabilistic risk when viewed through an expected-value lens. Suppression is also high, as alternative risk assessment methodologies (like expected-value or comparative risk) are actively marginalized in policy discourse. The theater ratio is moderate; while genuine safety concerns exist, some of the regulatory and public discourse serves to maintain the dominance of the tail-risk narrative, rather than purely addressing technical safety. The Fukushima event (2011) significantly amplified both extractiveness and suppression, leading to a peak, followed by a slight moderation as other energy crises (e.g., climate change, energy security) began to re-enter the discourse.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of environmental advocates, this constraint is a necessary Rope, coordinating society to protect against existential threats. From the nuclear industry and pro-nuclear scientists, it is a Snare, unfairly targeting a specific technology based on an exaggerated and unscientific risk assessment. The engine's classification as Tangled Rope reflects the hybrid nature: a genuine coordination function (societal risk aversion) coupled with asymmetric extraction and active suppression of alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Environmental advocacy groups and, conceptually, future generations are the beneficiaries, as their interests in extreme risk avoidance and intergenerational equity are prioritized. The nuclear energy industry, pro-nuclear scientists, and energy consumers are the victims, bearing the costs of this risk calculus. Regulatory bodies act as agenda-setters, enforcing policies shaped by this dominant risk perception.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scientific_consensus_on_risk_weighting,
    'Is there a scientific consensus on whether low-probability high-consequence events should be weighted disproportionately in energy risk calculus, beyond their expected value?',
    'A comprehensive, interdisciplinary review by a body like the IPCC or ICRP, specifically addressing the ethical and scientific basis for tail-risk weighting vs. expected value.',
    'If consensus supports disproportionate weighting, the constraint''s ''naturalness'' increases, reducing its perceived extractiveness. If consensus rejects it, the constraint''s suppression and extractiveness would be seen as less justified, pushing it towards a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scientific_consensus_on_risk_weighting, empirical, 'Ambiguity regarding the scientific justification for tail-risk dominance.').

omega_variable(
    intergenerational_burden_quantification,
    'How can the ''intergenerational burden'' of nuclear waste be quantitatively compared to the ''intergenerational burden'' of climate change from fossil fuels, within a consistent ethical framework?',
    'Development of a universally accepted intergenerational ethical framework for discounting future harms and benefits, applied to both nuclear waste and climate change scenarios.',
    'A robust framework could shift the perceived victim set and extractiveness, potentially re-legitimizing nuclear power if its intergenerational burden is found to be less severe than that of fossil fuels, or vice-versa.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_burden_quantification, conceptual, 'Conceptual ambiguity in comparing different types of intergenerational burdens.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative risk framings structural (e.g., regulatory capture by environmental groups) or internalized (e.g., public fear of nuclear technology that persists after factual debunking)?',
    'Post-policy-shift suppression trajectory: if suppression of pro-nuclear arguments persists even after policy changes to allow more nuclear development, it suggests a strong internalized component.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the public carries the suppression with them after policy changes. If structural, policy changes could more easily alleviate it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative risk framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__catastrophic_tail_dominant, 1986, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t1986, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 1986, 0.1).
narrative_ontology:measurement(acce_tr_t1995, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(acce_tr_t2004, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 2004, 0.18).
narrative_ontology:measurement(acce_tr_t2011, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 2011, 0.25).
narrative_ontology:measurement(acce_tr_t2018, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 2018, 0.22).
narrative_ontology:measurement(acce_tr_t2024, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(acce_be_t1986, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 1986, 0.55).
narrative_ontology:measurement(acce_be_t1995, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 1995, 0.6).
narrative_ontology:measurement(acce_be_t2004, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 2004, 0.63).
narrative_ontology:measurement(acce_be_t2011, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 2011, 0.72).
narrative_ontology:measurement(acce_be_t2018, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 2018, 0.7).
narrative_ontology:measurement(acce_be_t2024, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t1986, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 1986, 0.65).
narrative_ontology:measurement(acce_su_t1995, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 1995, 0.7).
narrative_ontology:measurement(acce_su_t2004, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 2004, 0.72).
narrative_ontology:measurement(acce_su_t2011, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 2011, 0.8).
narrative_ontology:measurement(acce_su_t2018, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 2018, 0.78).
narrative_ontology:measurement(acce_su_t2024, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__catastrophic_tail_dominant, enforcement_mechanism).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, energy_security_policy).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, climate_change_mitigation_targets).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'acceptable_risk_for_energy' kernel. This reading (catastrophic_tail_dominant) emphasizes extreme, irreversible outcomes. It is linked to 'expected_value_dominant' and 'comparative_risk_dominant' as sibling interpretations of the same core policy question.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
