% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__public_health_primary, []).

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
 *   constraint_id: vaccine_mandate_balance__public_health_primary
 *   human_readable: Vaccine Mandate: Public Health Primary Reading
 *   domain: public_health_ethics/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'public_health_primary' reading of
 *   the 'vaccine_mandate_balance' kernel. It asserts that collective
 *   protection takes precedence over individual consent when voluntary
 *   compliance fails to achieve herd immunity, and vulnerable populations
 *   face lethal exposure risk. The constraint is classified as a Tangled Rope
 *   due to its genuine coordination function (public health protection)
 *   coupled with significant, actively enforced extraction from individuals
 *   whose consent is superseded. The high extractiveness and suppression
 *   reflect the coercive nature of mandates, while the low theater ratio
 *   indicates active, functional enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__public_health_primary, 0.75).
domain_priors:suppression_score(vaccine_mandate_balance__public_health_primary, 0.8).
domain_priors:theater_ratio(vaccine_mandate_balance__public_health_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, extractiveness, 0.75).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__public_health_primary, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__public_health_primary, "Vaccine Mandate: Public Health Primary Reading").
narrative_ontology:topic_domain(vaccine_mandate_balance__public_health_primary, "public_health_ethics/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__public_health_primary, 'e7a1d433-d8ed-4cd3-a08c-909a908c6327').
narrative_ontology:cs_kernel_codification('e7a1d433-d8ed-4cd3-a08c-909a908c6327', formalized).
narrative_ontology:cs_authority_grounding('e7a1d433-d8ed-4cd3-a08c-909a908c6327', lineage).
narrative_ontology:cs_interpretation_layer_present('e7a1d433-d8ed-4cd3-a08c-909a908c6327').
narrative_ontology:cs_reading_relation('e7a1d433-d8ed-4cd3-a08c-909a908c6327', vaccine_mandate_balance__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('e7a1d433-d8ed-4cd3-a08c-909a908c6327', vaccine_mandate_balance__proportionality_reading, influences).
narrative_ontology:cs_axiom('e7a1d433-d8ed-4cd3-a08c-909a908c6327', foundational, collective_good_priority).
narrative_ontology:cs_axiom_status(collective_good_priority, holdable).
narrative_ontology:cs_axiom_grounding('e7a1d433-d8ed-4cd3-a08c-909a908c6327', collective_good_priority, deontological).
narrative_ontology:cs_axiom('e7a1d433-d8ed-4cd3-a08c-909a908c6327', secondary, state_duty_to_protect_public_health).
narrative_ontology:cs_axiom_status(state_duty_to_protect_public_health, holdable).
narrative_ontology:cs_axiom_grounding('e7a1d433-d8ed-4cd3-a08c-909a908c6327', state_duty_to_protect_public_health, deontological).
narrative_ontology:cs_reference_frame('e7a1d433-d8ed-4cd3-a08c-909a908c6327', public_health_imperative).
narrative_ontology:cs_drift_state('e7a1d433-d8ed-4cd3-a08c-909a908c6327', post_pandemic_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('e7a1d433-d8ed-4cd3-a08c-909a908c6327', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, vulnerable_populations).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, public_health_authorities).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, healthcare_systems).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, unvaccinated_individuals_under_mandate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for protecting public health, they implement and enforce vaccine mandates, justifying them as necessary to achieve herd immunity and protect vulnerable groups. They bear the political and administrative costs of enforcement.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Immunocompromised, elderly, or infants who cannot be vaccinated or for whom vaccines are less effective. They directly benefit from increased herd immunity, which reduces their lethal exposure risk. Their safety is the primary justification for the mandate.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, vulnerable_populations, beneficiary,
    powerless, biographical, trapped, local).

% Individuals who, for various reasons (personal belief, medical exemption, distrust), are unvaccinated and face legal or social consequences (e.g., job loss, travel restrictions) if they do not comply with mandates. From this reading's perspective, their consent is subordinated to collective necessity.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, unvaccinated_individuals_under_mandate, payer,
    moderate, immediate, constrained, local).

% Benefit from reduced burden during infectious disease outbreaks due to higher vaccination rates. Mandates help prevent overwhelming hospital capacity and preserve resources for other medical needs.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, healthcare_systems, beneficiary,
    institutional, biographical, constrained, national).

% Argue for the primacy of individual bodily autonomy and consent, opposing state-compelled medical interventions. While not directly targeted by the mandate, their arguments are structurally excluded from the mandate's justification within this reading.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, civil_liberties_advocates, excluded,
    organized, generational, analytical, national).

% Adjudicate the legality and constitutionality of vaccine mandates, balancing individual rights against state police powers. Their rulings can affirm or constrain the enforcement mechanisms of the mandate.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To achieve a sufficient level of population immunity (herd immunity) to protect all members of society, especially those most vulnerable, from infectious disease transmission.
% TRANSFER_FUNCTION: Transfers individual bodily autonomy and consent from unvaccinated individuals to the collective public health benefit, ensuring a safer environment for vulnerable populations. It also transfers the burden of enforcement and public health management to state authorities.
% ABSENT_VOICES: Individuals and groups prioritizing absolute bodily autonomy, those with religious or philosophical objections to vaccination, and civil liberties organizations who would argue for less coercive public health measures. Their perspectives are subordinated or excluded by the mandate's premise.
% DISAPPEARANCE_RATIONALE: If vaccine mandates and their enforcement vanished, voluntary compliance might prove insufficient to achieve herd immunity, leading to increased disease transmission, higher rates of severe illness and death in vulnerable populations, and potential overwhelming of healthcare infrastructure. Society would reorganize around higher disease prevalence and risk.
% FOUNDING_PROBLEM: The failure of voluntary individual compliance to achieve sufficient population immunity, leading to uncontrolled spread of infectious diseases and significant risk to vulnerable populations.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiological evidence of disease outbreaks, scientific consensus on vaccine efficacy and safety, and historical public health crises that were mitigated by widespread vaccination efforts, corroborated by international health organizations and medical bodies.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__public_health_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(vaccine_mandate_balance__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__public_health_primary, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_balance__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_balance__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.75) reflects the significant cost borne by individuals whose bodily autonomy is overridden. Suppression (0.80) is high due to the active enforcement mechanisms (e.g., legal penalties, employment requirements) that compel compliance and limit alternatives. Resistance (0.70) is also high, reflecting the public and political opposition often faced by such mandates. The low theater ratio (0.10) indicates that the mandates are genuinely intended to achieve public health outcomes and are actively enforced, not merely performative. The metrics reflect the structural reality of the mandate, independent of the reading's justification.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of this 'public_health_primary' reading, the 'unvaccinated_individuals_under_mandate' are not 'victims' but rather necessary contributors to a collective good, whose individual consent is justifiably subordinated. However, from a structural classification standpoint, the compulsion they experience constitutes extraction, making them 'victims' in the engine's terms. This divergence highlights the core tension between the reading's normative justification and the structural impact of the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities and vulnerable populations are clear beneficiaries, gaining protection and reduced burden. Healthcare systems also benefit from reduced strain. Unvaccinated individuals under mandate are the primary targets, bearing the cost of compelled compliance. Civil liberties advocates are excluded, as their core arguments are set aside by this reading's premise. Constitutional courts act as observers, evaluating the legality of the constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    individual_consent_vs_collective_good,
    'Is the subordination of individual consent truly justified by collective necessity, or does it represent an overreach of state power, even when framed as necessary for public health?',
    'Empirical data on the effectiveness of less coercive measures, ethical analysis of the limits of state power, and public deliberation on the acceptable trade-offs between individual liberty and collective security.',
    'If deemed an overreach, the constraint''s legitimacy would be severely undermined, potentially leading to reclassification as a Snare. If fully justified, it reinforces the Tangled Rope classification as a necessary, albeit extractive, coordination mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(individual_consent_vs_collective_good, conceptual, 'The fundamental tension between individual bodily autonomy and collective public health imperatives.').

omega_variable(
    efficacy_of_mandates_vs_alternatives,
    'Do vaccine mandates demonstrably achieve herd immunity and protect vulnerable populations more effectively than less coercive public health interventions (e.g., robust education campaigns, incentives, targeted protections)?',
    'Comparative studies of public health outcomes in jurisdictions with and without mandates, analysis of behavioral responses to different intervention types, and long-term epidemiological data.',
    'If mandates are shown to be significantly more effective, it strengthens the ''public_health_primary'' reading''s instrumental justification. If less coercive alternatives prove equally or more effective, it weakens the necessity claim and could shift the classification towards a Snare by exposing the extraction as unnecessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_of_mandates_vs_alternatives, empirical, 'The instrumental effectiveness of mandates compared to alternative public health strategies.').

omega_variable(
    definition_of_vulnerable_populations_scope,
    'How is ''vulnerable'' defined in practice, and does this definition consistently and justifiably delineate the scope of individuals for whom mandates are deemed necessary?',
    'Review of public health guidelines, legal challenges to mandate scope, and epidemiological data on risk stratification. This would involve examining whether the definition is overly broad or too narrow.',
    'An overly broad definition could expand the scope of extraction beyond what is strictly necessary, pushing the constraint closer to a Snare. A precise, evidence-based definition would reinforce the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_vulnerable_populations_scope, conceptual, 'The scope and justification of ''vulnerable populations'' as a trigger for mandates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__public_health_primary, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__public_health_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vacc_tr_t5, vaccine_mandate_balance__public_health_primary, theater_ratio, 5, 0.1).
narrative_ontology:measurement(vacc_tr_t10, vaccine_mandate_balance__public_health_primary, theater_ratio, 10, 0.1).
narrative_ontology:measurement(vacc_tr_t15, vaccine_mandate_balance__public_health_primary, theater_ratio, 15, 0.1).
narrative_ontology:measurement(vacc_tr_t20, vaccine_mandate_balance__public_health_primary, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__public_health_primary, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(vacc_be_t5, vaccine_mandate_balance__public_health_primary, base_extractiveness, 5, 0.7).
narrative_ontology:measurement(vacc_be_t10, vaccine_mandate_balance__public_health_primary, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(vacc_be_t15, vaccine_mandate_balance__public_health_primary, base_extractiveness, 15, 0.73).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_balance__public_health_primary, base_extractiveness, 20, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__public_health_primary, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(vacc_su_t5, vaccine_mandate_balance__public_health_primary, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(vacc_su_t10, vaccine_mandate_balance__public_health_primary, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(vacc_su_t15, vaccine_mandate_balance__public_health_primary, suppression_requirement, 15, 0.78).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_balance__public_health_primary, suppression_requirement, 20, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__public_health_primary, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
