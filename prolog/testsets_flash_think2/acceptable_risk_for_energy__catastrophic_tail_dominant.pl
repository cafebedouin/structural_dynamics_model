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
 *   human_readable: Catastrophic Tail-Risk Dominant Energy Acceptability
 *   domain: Risk Assessment / Energy Policy / Public Safety Governance
 *
 * SUMMARY:
 *   This constraint represents a specific reading of 'acceptable risk for
 *   energy' where the potential for low-probability, high-consequence, and
 *   irreversible events (e.g., nuclear accidents, long-term waste storage
 *   failures, severe climate change) dominates the risk calculus. It
 *   prioritizes intergenerational burden and the precautionary principle over
 *   expected-value optimization or comparative risk assessments. This
 *   framework actively suppresses alternative probabilistic or comparative
 *   risk framings, leading to high extraction from industries like nuclear
 *   energy and significant costs for current taxpayers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.85).
domain_priors:suppression_score(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.9).
domain_priors:theater_ratio(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, extractiveness, 0.85).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__catastrophic_tail_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__catastrophic_tail_dominant, "Catastrophic Tail-Risk Dominant Energy Acceptability").
narrative_ontology:topic_domain(acceptable_risk_for_energy__catastrophic_tail_dominant, "Risk Assessment / Energy Policy / Public Safety Governance").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__catastrophic_tail_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__catastrophic_tail_dominant, 'f1440d68-8ad6-43d9-8bb8-32be8226a828').
narrative_ontology:cs_kernel_codification('f1440d68-8ad6-43d9-8bb8-32be8226a828', formalized).
narrative_ontology:cs_authority_grounding('f1440d68-8ad6-43d9-8bb8-32be8226a828', practice).
narrative_ontology:cs_interpretation_layer_present('f1440d68-8ad6-43d9-8bb8-32be8226a828').
narrative_ontology:cs_reading_relation('f1440d68-8ad6-43d9-8bb8-32be8226a828', acceptable_risk_for_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_reading_relation('f1440d68-8ad6-43d9-8bb8-32be8226a828', acceptable_risk_for_energy__comparative_risk_dominant, coexists_with).
narrative_ontology:cs_axiom('f1440d68-8ad6-43d9-8bb8-32be8226a828', foundational, catastrophic_tail_risk_is_unacceptable).
narrative_ontology:cs_axiom_status(catastrophic_tail_risk_is_unacceptable, holdable).
narrative_ontology:cs_axiom_grounding('f1440d68-8ad6-43d9-8bb8-32be8226a828', catastrophic_tail_risk_is_unacceptable, deontological).
narrative_ontology:cs_axiom('f1440d68-8ad6-43d9-8bb8-32be8226a828', foundational, intergenerational_burden_is_absolute).
narrative_ontology:cs_axiom_status(intergenerational_burden_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('f1440d68-8ad6-43d9-8bb8-32be8226a828', intergenerational_burden_is_absolute, deontological).
narrative_ontology:cs_reference_frame('f1440d68-8ad6-43d9-8bb8-32be8226a828', precautionary_principle_application).
narrative_ontology:cs_drift_state('f1440d68-8ad6-43d9-8bb8-32be8226a828', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f1440d68-8ad6-43d9-8bb8-32be8226a828', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, future_generations).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, environmental_advocates).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, public_safety_regulators).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_energy_industry).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, current_taxpayers).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, proponents_of_expected_value_risk).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from policies that prioritize their long-term safety by minimizing catastrophic, irreversible risks (e.g., nuclear waste, climate change impacts), but bear the potential burden if these policies fail.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% Champion policies and regulations that prioritize long-term environmental protection and minimize catastrophic risks, aligning with this risk calculus. They gain legitimacy and influence from its dominance.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, environmental_advocates, beneficiary,
    organized, generational, mobile, global).

% Implement and enforce energy and public safety policies based on this risk calculus, prioritizing the avoidance of low-probability, high-consequence events over economic optimization. Their mandate is strengthened by this framework.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, public_safety_regulators, agenda_setter,
    institutional, biographical, constrained, national).

% Bears significant costs and regulatory hurdles due to this risk calculus, which emphasizes the unquantifiable and irreversible risks of nuclear power and waste, making new projects difficult and existing ones expensive to maintain.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_energy_industry, payer,
    powerful, biographical, constrained, national).

% Bear the costs of long-term nuclear waste disposal, enhanced safety measures, and other public safety infrastructure mandated by this risk calculus, often without direct input on the policy.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, current_taxpayers, payer,
    moderate, biographical, constrained, national).

% Advocate for a probabilistic approach to risk assessment, where low-probability events are weighted by their likelihood, but their methodology is largely suppressed or marginalized by the dominance of the catastrophic tail-risk framework in policy decisions.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, proponents_of_expected_value_risk, excluded,
    organized, biographical, mobile, global).

% Argue for comparing nuclear risk against other energy sources (e.g., coal emissions, climate change impacts) to inform policy, but their framing is sidelined by the absolute focus on catastrophic tails and irreversibility within this dominant calculus.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, comparative_risk_analysts, excluded,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_for_energy__catastrophic_tail_dominant, diffuse).
narrative_ontology:fixing_cost_class(acceptable_risk_for_energy__catastrophic_tail_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates public and intergenerational safety by establishing a precautionary principle for high-consequence, irreversible risks, ensuring long-term burdens are considered in energy policy and decision-making.
% TRANSFER_FUNCTION: Transfers the burden of managing catastrophic and irreversible risks (e.g., nuclear waste, long-term environmental damage) from future generations to current industries and taxpayers, while transferring decision-making authority to regulators prioritizing extreme safety.
% ABSENT_VOICES: Proponents of expected-value optimization and comparative risk analysis are structurally excluded or marginalized; they would argue for a more balanced, probabilistic assessment that considers the benefits of certain energy sources alongside their risks, but their framing is suppressed.
% DISAPPEARANCE_RATIONALE: If this risk calculus vanished, energy policy would likely shift towards more probabilistic or comparative risk assessments, potentially leading to a resurgence of nuclear power or other high-impact projects, and a re-evaluation of long-term waste management strategies, fundamentally reorganizing how energy risks are managed.
% FOUNDING_PROBLEM: To prevent catastrophic, irreversible harm from technological or industrial activities, particularly those with long-term intergenerational consequences, where traditional probabilistic risk assessment was deemed insufficient to capture the full scope of potential damage.
% FOUNDING_PROBLEM_CORROBORATION: Environmental scientists, public health organizations, and some international bodies (e.g., IPCC reports emphasizing irreversible climate impacts) corroborate the ongoing concern for catastrophic and irreversible risks, especially regarding nuclear waste and climate change, supporting the continued relevance of this framework.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__catastrophic_tail_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__catastrophic_tail_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__catastrophic_tail_dominant, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(acceptable_risk_for_energy__catastrophic_tail_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) because this calculus imposes substantial costs on industries and taxpayers by emphasizing extreme safety and long-term burdens, often without fully accounting for the benefits of the constrained activities. Suppression is very high (0.90) as this framework actively marginalizes or excludes alternative risk assessment methodologies (expected value, comparative risk) from policy discourse. Theater ratio is moderate (0.40); while genuine concern for catastrophic risks exists, some aspects of 'irreversibility' and 'intergenerational burden' can become performative justifications for policy inertia. Accessibility collapse is high (0.80) as it severely limits the viability of certain energy technologies. Resistance is moderate (0.60) from industries and economists advocating for alternative risk models.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of environmental advocates and public safety regulators, this framework is a necessary and ethical approach to protect the planet and future generations. From the perspective of the nuclear industry and economists, it is an overly cautious, economically inefficient, and unscientific approach that unfairly targets specific technologies and suppresses rational discourse.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations, environmental advocates, and public safety regulators are structural beneficiaries, as the framework is designed to protect their interests and empower their mandates. The nuclear energy industry and current taxpayers are primary targets, bearing the direct costs and regulatory burdens. Proponents of expected-value and comparative risk are excluded, as their methodologies are suppressed by this dominant framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling it as a pure Mountain (natural law) or Rope (pure coordination). While it genuinely coordinates public safety and intergenerational responsibility, the high extractiveness and active suppression of alternatives reveal its asymmetric nature. The 'live' status of the founding problem (preventing catastrophic harm) suggests it's not a Piton, but the contested corroboration highlights the ongoing debate about its necessity and proportionality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    irreversibility_definition_ambiguity,
    'Is ''irreversibility'' an absolute structural property of certain risks, or is its definition and application a conceptual choice that can be reframed?',
    'Analysis of engineering solutions for long-term waste management and climate adaptation strategies: if ''irreversible'' risks can be mitigated or managed over long timescales, the concept''s absoluteness is challenged.',
    'If ''irreversibility'' is conceptually contingent, the extractiveness and suppression of this constraint might be overstated, potentially shifting its classification towards a Rope or Scaffold if mitigation is feasible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irreversibility_definition_ambiguity, conceptual, 'Ambiguity in the absolute nature of ''irreversibility'' in risk assessment.').

omega_variable(
    suppression_of_probabilistic_methods,
    'To what extent is the suppression of expected-value and comparative risk analyses a necessary feature of prioritizing catastrophic tail risks, versus an institutional capture by specific advocacy groups?',
    'Empirical study of policy-making processes in jurisdictions where this calculus is dominant, examining the mechanisms and justifications for excluding alternative risk assessment methodologies.',
    'If suppression is primarily due to institutional capture, the constraint''s ''tangled_rope'' classification is strongly reinforced, potentially pushing it closer to a ''snare'' if the coordination function is found to be minimal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_probabilistic_methods, empirical, 'Whether suppression of alternative risk methods is structural or due to capture.').

omega_variable(
    intergenerational_burden_quantification,
    'Can the ''intergenerational burden'' be meaningfully quantified or compared across different energy choices, or does its qualitative emphasis inherently bias the risk calculus?',
    'Development of robust, interdisciplinary methodologies for quantifying and comparing intergenerational burdens across various energy systems, including climate change, resource depletion, and waste management.',
    'If quantification is possible and reveals different priorities, the constraint''s current high extractiveness might be re-evaluated, and its suppression of alternative framings could be seen as less justified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_burden_quantification, empirical, 'Quantification challenges for intergenerational burden.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__catastrophic_tail_dominant, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t1980, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(acce_tr_t1990, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(acce_tr_t2000, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(acce_tr_t2010, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(acce_tr_t2020, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 2020, 0.39).
narrative_ontology:measurement(acce_tr_t2025, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(acce_be_t1980, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(acce_be_t1990, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 1990, 0.72).
narrative_ontology:measurement(acce_be_t2000, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(acce_be_t2010, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 2010, 0.82).
narrative_ontology:measurement(acce_be_t2020, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 2020, 0.84).
narrative_ontology:measurement(acce_be_t2025, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t1980, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(acce_su_t1990, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 1990, 0.78).
narrative_ontology:measurement(acce_su_t2000, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 2000, 0.84).
narrative_ontology:measurement(acce_su_t2010, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 2010, 0.88).
narrative_ontology:measurement(acce_su_t2020, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 2020, 0.89).
narrative_ontology:measurement(acce_su_t2025, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 2025, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__catastrophic_tail_dominant, enforcement_mechanism).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_waste_disposal_regulations).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, renewable_energy_subsidies).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, fossil_fuel_emission_standards).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy__expected_value_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy__comparative_risk_dominant).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'acceptable_risk_for_energy' kernel. This reading emphasizes catastrophic tail risks and intergenerational burden, while 'expected_value_dominant' focuses on probabilistic cost-benefit and 'comparative_risk_dominant' on relative risk assessment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
