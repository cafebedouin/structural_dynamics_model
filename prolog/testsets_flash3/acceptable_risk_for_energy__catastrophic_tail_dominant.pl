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
 *   constraint_id: acceptable_risk_for_energy__catastrophic_tail_dominant
 *   human_readable: Catastrophic Tail-Risk Dominance in Energy Policy
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   This constraint describes a specific reading of 'acceptable risk for
 *   energy' where low-probability, high-consequence events (catastrophic tail
 *   risks) are given disproportionate weight in policy decisions,
 *   particularly concerning nuclear energy. This reading emphasizes
 *   irreversibility and intergenerational burden over expected-value
 *   optimization. It is a 'tangled_rope' because it genuinely coordinates
 *   public safety concerns and intergenerational equity (beneficiaries:
 *   environmental groups, future generations) but does so through asymmetric
 *   extraction from the nuclear industry and its proponents (victims: nuclear
 *   industry, pro-nuclear scientists, energy consumers) by suppressing
 *   alternative risk frameworks.
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
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__catastrophic_tail_dominant, '5a114a25-d36b-4cc4-8bb3-388196593068').
narrative_ontology:cs_kernel_codification('5a114a25-d36b-4cc4-8bb3-388196593068', formalized).
narrative_ontology:cs_authority_grounding('5a114a25-d36b-4cc4-8bb3-388196593068', lineage).
narrative_ontology:cs_interpretation_layer_present('5a114a25-d36b-4cc4-8bb3-388196593068').
narrative_ontology:cs_reading_relation('5a114a25-d36b-4cc4-8bb3-388196593068', acceptable_risk_for_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_reading_relation('5a114a25-d36b-4cc4-8bb3-388196593068', acceptable_risk_for_energy__comparative_risk_dominant, coexists_with).
narrative_ontology:cs_axiom('5a114a25-d36b-4cc4-8bb3-388196593068', foundational, irreversibility_outweighs_probability).
narrative_ontology:cs_axiom_status(irreversibility_outweighs_probability, holdable).
narrative_ontology:cs_axiom_grounding('5a114a25-d36b-4cc4-8bb3-388196593068', irreversibility_outweighs_probability, deontological).
narrative_ontology:cs_axiom('5a114a25-d36b-4cc4-8bb3-388196593068', foundational, intergenerational_burden_is_primary_risk).
narrative_ontology:cs_axiom_status(intergenerational_burden_is_primary_risk, holdable).
narrative_ontology:cs_axiom_grounding('5a114a25-d36b-4cc4-8bb3-388196593068', intergenerational_burden_is_primary_risk, deontological).
narrative_ontology:cs_reference_frame('5a114a25-d36b-4cc4-8bb3-388196593068', post_chernobyl_risk_aversion).
narrative_ontology:cs_drift_state('5a114a25-d36b-4cc4-8bb3-388196593068', contemporary_climate_crisis_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('5a114a25-d36b-4cc4-8bb3-388196593068', '').
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

% Are conceptually protected from the long-term, irreversible consequences of catastrophic events (e.g., nuclear waste, large-scale accidents) by policies that prioritize tail-risk. They bear no direct costs but are the ultimate beneficiaries of this risk calculus.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% Bears significant costs due to stringent regulations, public opposition, and investment hurdles driven by the catastrophic tail-risk framework. This includes high capital costs, extended licensing processes, and challenges in waste disposal. Their ability to expand or even maintain operations is severely constrained.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_energy_industry, payer,
    institutional, generational, constrained, national).

% Their probabilistic and comparative risk assessments are often suppressed or marginalized in policy debates dominated by tail-risk concerns. They face professional challenges and funding difficulties when advocating for nuclear energy based on expected-value or comparative risk models. Their identity is tied to a scientific approach that is devalued by this framework.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, pro_nuclear_scientists, payer,
    moderate, biographical, identity_locked, global).

% May face higher energy costs or reduced energy security due to the exclusion or limited deployment of nuclear power, which is often a low-carbon, baseload option. Their choices are constrained by the available energy mix shaped by this risk calculus.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, energy_consumers, payer,
    moderate, immediate, constrained, national).

% Are tasked with implementing and enforcing policies derived from this risk calculus. They operate under a mandate to prioritize extreme, low-probability events, leading to highly conservative regulations and often lengthy approval processes for nuclear projects. Their institutional identity is tied to public safety and risk aversion.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, regulatory_bodies, agenda_setter,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_for_energy__catastrophic_tail_dominant, environmental_advocacy_groups).
narrative_ontology:fixing_cost_class(acceptable_risk_for_energy__catastrophic_tail_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates public and political attention on the most severe, albeit rare, potential outcomes of energy choices, ensuring that long-term and irreversible burdens are considered in policy decisions.
% TRANSFER_FUNCTION: Transfers the burden of managing catastrophic, intergenerational risks away from future generations and onto current energy producers and consumers, primarily through increased regulatory costs and suppressed development of certain energy technologies.
% ABSENT_VOICES: Proponents of expected-value optimization and comparative risk analysis, particularly within the nuclear industry and certain scientific communities, are often marginalized. They would argue for a more balanced, data-driven approach to risk assessment that considers all energy sources' full spectrum of risks and benefits.
% DISAPPEARANCE_RATIONALE: If this risk calculus vanished, energy policy would rapidly shift towards more probabilistic and comparative assessments. Nuclear energy projects would likely see accelerated development, regulatory burdens would decrease, and public discourse on energy risk would re-center on cost-benefit analysis and climate impact, fundamentally altering the energy landscape.
% FOUNDING_PROBLEM: The historical experience of major industrial accidents (e.g., Chernobyl, Fukushima) and the unique challenge of long-lived radioactive waste created a demand for a risk framework that explicitly addresses low-probability, high-consequence events and intergenerational equity.
% FOUNDING_PROBLEM_CORROBORATION: Environmental groups and public safety advocates consistently attest that the founding problem of managing catastrophic, irreversible risks remains live and paramount. While the nuclear industry acknowledges the historical context, it contests the current framework's proportionality, arguing that the problem is over-addressed relative to other risks.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__catastrophic_tail_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__catastrophic_tail_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__catastrophic_tail_dominant, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.68) reflects the significant costs imposed on the nuclear industry and consumers due to the prioritization of tail-risk. Suppression (0.75) is high because probabilistic and comparative risk framings are actively marginalized in policy debates. The theater ratio (0.20) is moderate; while genuine safety concerns exist, a portion of the regulatory activity serves to maintain the dominance of this specific risk calculus rather than solely addressing technical safety. The Fukushima event (2011) caused a spike in both extractiveness and suppression, reinforcing this framework.
 *
 * PERSPECTIVAL GAP:
 *   The environmental advocacy groups and regulatory bodies perceive this as a necessary and just coordination mechanism for public safety. In contrast, the nuclear industry and pro-nuclear scientists experience it as an extractive and suppressive force that distorts rational energy policy. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Environmental advocacy groups and future generations are beneficiaries, as this framework aligns with their goals of long-term safety and intergenerational equity. The nuclear energy industry, pro-nuclear scientists, and energy consumers are victims, bearing the costs of suppressed development, regulatory burdens, and potentially higher energy prices. Regulatory bodies act as agenda-setters, enforcing the framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (protecting against catastrophic, irreversible risks) is still live, but its application is contested. The classification as 'tangled_rope' prevents mislabeling it as pure extraction (snare) by acknowledging its genuine coordination function for intergenerational safety, while also highlighting the asymmetric extraction and active enforcement required to maintain its dominance over alternative risk frameworks.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_probability_of_catastrophe,
    'What is the true, empirically verifiable probability of a catastrophic nuclear event, and how does it compare to the perceived risk driving policy?',
    'Long-term, independent probabilistic risk assessments (PRAs) across diverse reactor designs and operational contexts, coupled with transparent data sharing and peer review.',
    'If true probabilities are significantly lower than perceived, the current framework''s extractiveness and suppression would be re-evaluated as disproportionate, potentially shifting the constraint towards a ''snare'' or ''piton'' for the nuclear industry. If higher, it would reinforce the ''tangled_rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_probability_of_catastrophe, empirical, 'Ambiguity regarding the actual vs. perceived probability of catastrophic events.').

omega_variable(
    intergenerational_burden_quantification,
    'How can the ''intergenerational burden'' of nuclear waste be quantitatively compared to the intergenerational burden of climate change from fossil fuels, or the opportunity cost of foregone low-carbon energy?',
    'Development of robust, interdisciplinary methodologies for quantifying and comparing diverse intergenerational burdens, including economic, environmental, and social impacts, with broad scientific consensus.',
    'A clear quantification could shift the ''future_generations'' seat from a pure beneficiary to a more complex position, potentially altering the overall classification if the ''burden'' of foregone low-carbon energy outweighs the nuclear waste burden. This would challenge the foundational axiom of this reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_burden_quantification, conceptual, 'Conceptual ambiguity in comparing different types of intergenerational burdens.').

omega_variable(
    framing_bias_in_risk_perception,
    'To what extent is the dominance of catastrophic tail-risk a result of cognitive biases (e.g., availability heuristic, dread risk) rather than a purely rational assessment of objective risk?',
    'Psychological and sociological studies on public risk perception, media framing analysis, and expert elicitation to identify and quantify the influence of cognitive biases on policy decisions.',
    'If framing bias is a significant driver, the ''suppression'' metric might be re-interpreted as partly internalized by the public, and the ''theater_ratio'' could increase if policy actions are primarily performative responses to perceived, rather than objective, risk. This would strengthen the ''tangled_rope'' or even ''snare'' aspects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_bias_in_risk_perception, empirical, 'Influence of cognitive biases on the prioritization of catastrophic tail-risk.').


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
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_waste_disposal_regulations).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, energy_mix_policy).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, climate_change_mitigation_strategies).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'acceptable_risk_for_energy' kernel. This 'catastrophic_tail_dominant' reading emphasizes extreme, low-probability events. It is linked to 'expected_value_dominant' and 'comparative_risk_dominant' as alternative frameworks for energy risk assessment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
