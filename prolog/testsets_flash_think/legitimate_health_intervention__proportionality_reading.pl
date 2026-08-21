% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__proportionality_reading, []).

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
 *   constraint_id: legitimate_health_intervention__proportionality_reading
 *   human_readable: Proportionality Principle for Public Health Interventions
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates the 'proportionality_reading' of the
 *   'legitimate_health_intervention' kernel. It asserts that the legitimacy
 *   of public health interventions (e.g., mandates, restrictions) requires a
 *   balance between their severity and the threat level posed by a disease,
 *   considering both population harm and individual autonomy. The weighting
 *   of these factors is dynamic, adjusting based on disease characteristics
 *   like transmissibility and case-fatality rate. This reading introduces a
 *   conditional structure to public health policy, aiming to prevent both
 *   under- and over-reach.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__proportionality_reading, 0.65).
domain_priors:suppression_score(legitimate_health_intervention__proportionality_reading, 0.7).
domain_priors:theater_ratio(legitimate_health_intervention__proportionality_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__proportionality_reading, "Proportionality Principle for Public Health Interventions").
narrative_ontology:topic_domain(legitimate_health_intervention__proportionality_reading, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__proportionality_reading, '49cadd78-9ca3-44a5-9843-170204b8927c').
narrative_ontology:cs_kernel_codification('49cadd78-9ca3-44a5-9843-170204b8927c', formalized).
narrative_ontology:cs_authority_grounding('49cadd78-9ca3-44a5-9843-170204b8927c', lineage).
narrative_ontology:cs_interpretation_layer_present('49cadd78-9ca3-44a5-9843-170204b8927c').
narrative_ontology:cs_reading_relation('49cadd78-9ca3-44a5-9843-170204b8927c', legitimate_health_intervention__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('49cadd78-9ca3-44a5-9843-170204b8927c', legitimate_health_intervention__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_axiom('49cadd78-9ca3-44a5-9843-170204b8927c', foundational, intervention_severity_must_match_threat).
narrative_ontology:cs_axiom_status(intervention_severity_must_match_threat, holdable).
narrative_ontology:cs_axiom_grounding('49cadd78-9ca3-44a5-9843-170204b8927c', intervention_severity_must_match_threat, empirically_contingent).
narrative_ontology:cs_axiom('49cadd78-9ca3-44a5-9843-170204b8927c', foundational, balance_collective_good_individual_rights).
narrative_ontology:cs_axiom_status(balance_collective_good_individual_rights, holdable).
narrative_ontology:cs_axiom_grounding('49cadd78-9ca3-44a5-9843-170204b8927c', balance_collective_good_individual_rights, deontological).
narrative_ontology:cs_reference_frame('49cadd78-9ca3-44a5-9843-170204b8927c', liberal_democratic_rights_tradition).
narrative_ontology:cs_drift_state('49cadd78-9ca3-44a5-9843-170204b8927c', covid_19_pandemic_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('49cadd78-9ca3-44a5-9843-170204b8927c', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, general_public).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, vulnerable_populations).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, individuals_with_autonomy_concerns).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, businesses_affected_by_restrictions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for implementing public health measures, they interpret and apply the proportionality principle to justify interventions. They benefit from the legitimacy this principle provides to their actions.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from reduced population-level harm and disease spread due to proportionate interventions. They bear some costs in terms of restricted freedoms but generally accept these for collective safety.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, general_public, beneficiary,
    organized, biographical, constrained, national).

% Bear the direct costs of interventions that restrict individual freedoms (e.g., mask mandates, vaccine requirements, movement restrictions). They often challenge the proportionality assessment, arguing for greater individual liberty.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, individuals_with_autonomy_concerns, payer,
    moderate, immediate, constrained, local).

% Incur economic losses due to public health measures like lockdowns, capacity limits, or operational changes. They are direct targets of the economic extraction inherent in such interventions, even if deemed proportionate.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, businesses_affected_by_restrictions, payer,
    organized, biographical, constrained, local).

% Are disproportionately protected by public health interventions, as they face higher risks of severe illness or death. Their well-being is a key factor in the proportionality calculus, making them primary beneficiaries.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, vulnerable_populations, beneficiary,
    powerless, immediate, trapped, local).

% Provide guidance and review on the ethical implications of public health interventions, assessing whether they meet the proportionality standard. They influence policy but do not directly implement or enforce it.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, medical_ethics_boards, observer,
    institutional, generational, analytical, national).

% Analyze the legality of public health interventions, often challenging or defending them based on constitutional rights and the proportionality principle. Their work shapes judicial interpretations of the constraint.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, constitutional_lawyers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_health_intervention__proportionality_reading, public_health_authorities).
narrative_ontology:fixing_cost_class(legitimate_health_intervention__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To balance the collective good of public health protection with the respect for individual autonomy and rights, ensuring that state interventions are justified by the severity and nature of the health threat.
% TRANSFER_FUNCTION: Transfers some degree of individual liberty, economic freedom, and social normalcy from individuals and businesses to public health authorities, in exchange for reduced population-level morbidity and mortality.
% ABSENT_VOICES: Those who advocate for absolute individual bodily autonomy regardless of public health consequences, and those who prioritize public health above all individual rights, would find their views marginalized by this balancing act.
% DISAPPEARANCE_RATIONALE: If the proportionality principle vanished, public health responses would either become unchecked and potentially tyrannical (if public health primary prevailed) or entirely ineffective (if bodily autonomy primary prevailed), leading to a fundamental reorganization of how societies manage health crises and individual rights.
% FOUNDING_PROBLEM: How to respond effectively to infectious disease threats and other public health emergencies without infringing excessively on individual liberties and economic stability, particularly in democratic societies with strong rights traditions.
% FOUNDING_PROBLEM_CORROBORATION: International human rights instruments, national constitutions, and extensive jurisprudence from various democratic nations consistently grapple with this tension, demonstrating that the problem remains a live and contested issue requiring ongoing balancing and interpretation.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__proportionality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(legitimate_health_intervention__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__proportionality_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_health_intervention__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_health_intervention__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.65 at end) and suppression (0.70 at end) are substantial because implementing proportionate interventions still requires significant state power to restrict freedoms and enforce compliance, especially during a severe health crisis (reflected in the temporal measurements). The 'tangled_rope' classification reflects the genuine coordination function (protecting public health) combined with the asymmetric extraction from individuals and businesses whose autonomy and economic activity are curtailed. Theater ratio is low (0.15) as the interventions are primarily functional, though some performative elements might emerge during periods of high public anxiety. Accessibility collapse is moderate (0.50) as alternatives to specific interventions might exist, but the overall framework of state intervention in a crisis is difficult to avoid. Resistance is moderate (0.60) due to ongoing debates about the 'correct' proportionality.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health authorities, this principle is a necessary and just framework for action. From the perspective of individuals whose autonomy is restricted, the 'proportionality' may feel like an arbitrary justification for extraction. The engine's per-seat classification will highlight these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities and the general public are beneficiaries, as the principle legitimizes necessary actions and protects collective well-being. Vulnerable populations are particularly strong beneficiaries. Individuals with autonomy concerns and businesses affected by restrictions are the primary payers, bearing the direct costs of curtailed freedoms and economic activity. The engine will derive their directionality accordingly, with payers closer to the 'full target' end and beneficiaries closer to the 'full beneficiary' end.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (balancing public health and individual rights) remains live and highly relevant, especially in the context of emerging health threats. The temporal measurements show an increase in extractiveness and suppression during a health crisis (2020-2021), indicating the principle's active application and the costs it imposes, rather than an atrophy of function. The 'contested' status of the founding problem corroboration further reinforces that the balancing act is ongoing, preventing a mislabeling as a 'piton' or 'snare' where the coordination function has atrophied or was merely cover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_weighting_ambiguity,
    'How are ''population harm'' and ''individual autonomy'' objectively weighted in the proportionality calculus, and how do disease characteristics (transmissibility, case-fatality rate) translate into specific intervention severity levels?',
    'Development of standardized, transparent, and publicly debated epidemiological and ethical frameworks for weighting, coupled with independent review of their application in specific health crises.',
    'If the weighting is found to be arbitrary or biased, the constraint''s legitimacy and its ''tangled_rope'' classification would be challenged, potentially shifting towards a ''snare'' if the extraction is deemed unjustified. If robust, it strengthens the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_weighting_ambiguity, conceptual, 'Ambiguity in the objective weighting of factors within the proportionality principle.').

omega_variable(
    sibling_reading_public_health_primary_impact,
    'To what extent does this proportionality reading prevent the ''public_health_primary'' reading from leading to excessive state overreach and suppression of individual rights?',
    'Comparative analysis of policy outcomes in jurisdictions that explicitly adopt a proportionality framework versus those that prioritize public health without such a balancing act.',
    'If the proportionality reading effectively mitigates overreach, it demonstrates its unique coordination value. If it fails to do so, its distinctiveness from the ''public_health_primary'' reading is diminished, and its classification might converge with a more extractive type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_public_health_primary_impact, empirical, 'Impact of proportionality on preventing overreach compared to public_health_primary reading.').

omega_variable(
    sibling_reading_bodily_autonomy_primary_impact,
    'To what extent does this proportionality reading prevent the ''bodily_autonomy_primary'' reading from leading to uncontrolled disease spread and population harm?',
    'Comparative analysis of health outcomes in jurisdictions that explicitly adopt a proportionality framework versus those that prioritize absolute bodily autonomy without public health considerations.',
    'If the proportionality reading effectively prevents uncontrolled disease, it demonstrates its unique coordination value. If it fails to do so, its distinctiveness from the ''bodily_autonomy_primary'' reading is diminished, and its classification might converge with a less effective ''rope'' or even ''piton'' if it becomes inert.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_bodily_autonomy_primary_impact, empirical, 'Impact of proportionality on preventing uncontrolled disease compared to bodily_autonomy_primary reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__proportionality_reading, 2019, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t2019, legitimate_health_intervention__proportionality_reading, theater_ratio, 2019, 0.1).
narrative_ontology:measurement(legi_tr_t2020, legitimate_health_intervention__proportionality_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(legi_tr_t2021, legitimate_health_intervention__proportionality_reading, theater_ratio, 2021, 0.2).
narrative_ontology:measurement(legi_tr_t2022, legitimate_health_intervention__proportionality_reading, theater_ratio, 2022, 0.18).
narrative_ontology:measurement(legi_tr_t2023, legitimate_health_intervention__proportionality_reading, theater_ratio, 2023, 0.15).

% Extraction over time
narrative_ontology:measurement(legi_be_t2019, legitimate_health_intervention__proportionality_reading, base_extractiveness, 2019, 0.3).
narrative_ontology:measurement(legi_be_t2020, legitimate_health_intervention__proportionality_reading, base_extractiveness, 2020, 0.7).
narrative_ontology:measurement(legi_be_t2021, legitimate_health_intervention__proportionality_reading, base_extractiveness, 2021, 0.8).
narrative_ontology:measurement(legi_be_t2022, legitimate_health_intervention__proportionality_reading, base_extractiveness, 2022, 0.75).
narrative_ontology:measurement(legi_be_t2023, legitimate_health_intervention__proportionality_reading, base_extractiveness, 2023, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t2019, legitimate_health_intervention__proportionality_reading, suppression_requirement, 2019, 0.2).
narrative_ontology:measurement(legi_su_t2020, legitimate_health_intervention__proportionality_reading, suppression_requirement, 2020, 0.75).
narrative_ontology:measurement(legi_su_t2021, legitimate_health_intervention__proportionality_reading, suppression_requirement, 2021, 0.85).
narrative_ontology:measurement(legi_su_t2022, legitimate_health_intervention__proportionality_reading, suppression_requirement, 2022, 0.8).
narrative_ontology:measurement(legi_su_t2023, legitimate_health_intervention__proportionality_reading, suppression_requirement, 2023, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention__public_health_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention__bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'legitimate_health_intervention' kernel, focusing on proportionality. It is linked to its sibling readings, 'public_health_primary' and 'bodily_autonomy_primary', which represent alternative framings of the same core tension.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
