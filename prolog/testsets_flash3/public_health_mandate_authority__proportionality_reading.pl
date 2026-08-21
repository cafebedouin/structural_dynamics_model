% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__proportionality_reading, []).

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
 *   constraint_id: public_health_mandate_authority__proportionality_reading
 *   human_readable: Public Health Mandate Authority (Proportionality Reading)
 *   domain: public_health/constitutional_rights/bioethics
 *
 * SUMMARY:
 *   This constraint represents the 'proportionality reading' of public health
 *   mandate authority. It asserts that the legitimacy of public health
 *   mandates depends on a sliding scale, considering the severity of the
 *   threat, the availability of alternatives, the magnitude of coercion, and
 *   the duration of imposition. This reading aims to balance collective
 *   health with individual rights, leading to a dynamic constraint where
 *   extractiveness and suppression fluctuate with the perceived threat level
 *   and societal context. The metrics reflect a scenario where mandates are
 *   actively enforced and extract a degree of individual autonomy, but with
 *   an underlying justification of collective benefit.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__proportionality_reading, 0.65).
domain_priors:suppression_score(public_health_mandate_authority__proportionality_reading, 0.7).
domain_priors:theater_ratio(public_health_mandate_authority__proportionality_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__proportionality_reading, "Public Health Mandate Authority (Proportionality Reading)").
narrative_ontology:topic_domain(public_health_mandate_authority__proportionality_reading, "public_health/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__proportionality_reading, '73f6393c-285a-4379-841f-5eb41e379b36').
narrative_ontology:cs_kernel_codification('73f6393c-285a-4379-841f-5eb41e379b36', formalized).
narrative_ontology:cs_authority_grounding('73f6393c-285a-4379-841f-5eb41e379b36', lineage).
narrative_ontology:cs_interpretation_layer_present('73f6393c-285a-4379-841f-5eb41e379b36').
narrative_ontology:cs_reading_relation('73f6393c-285a-4379-841f-5eb41e379b36', public_health_mandate_authority__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('73f6393c-285a-4379-841f-5eb41e379b36', public_health_mandate_authority__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_axiom('73f6393c-285a-4379-841f-5eb41e379b36', foundational, mandates_must_be_least_restrictive_means).
narrative_ontology:cs_axiom_status(mandates_must_be_least_restrictive_means, holdable).
narrative_ontology:cs_axiom_grounding('73f6393c-285a-4379-841f-5eb41e379b36', mandates_must_be_least_restrictive_means, deontological).
narrative_ontology:cs_axiom('73f6393c-285a-4379-841f-5eb41e379b36', foundational, collective_benefit_must_outweigh_individual_burden).
narrative_ontology:cs_axiom_status(collective_benefit_must_outweigh_individual_burden, holdable).
narrative_ontology:cs_axiom_grounding('73f6393c-285a-4379-841f-5eb41e379b36', collective_benefit_must_outweigh_individual_burden, instrumental).
narrative_ontology:cs_reference_frame('73f6393c-285a-4379-841f-5eb41e379b36', constitutional_proportionality_doctrine).
narrative_ontology:cs_drift_state('73f6393c-285a-4379-841f-5eb41e379b36', contemporary_pandemic_response, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('73f6393c-285a-4379-841f-5eb41e379b36', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, immunocompromised_individuals).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, healthcare_systems).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, general_public_health).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, unvaccinated_individuals).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, individuals_with_conscientious_objections).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for assessing public health threats and implementing interventions. They balance individual liberties against collective well-being, guided by principles of proportionality. Their legitimacy depends on demonstrating that mandates are the least restrictive means to achieve a compelling public health goal.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Highly vulnerable to infectious diseases, relying on herd immunity and public health measures for protection. They benefit directly from mandates that reduce pathogen transmission, but have limited agency in shaping policy.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, immunocompromised_individuals, beneficiary,
    powerless, immediate, trapped, local).

% Benefit from mandates that prevent overwhelming surges in patient load, preserving capacity for all medical needs. They bear the burden of implementing and enforcing some mandates, but their primary interest is system stability.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, healthcare_systems, beneficiary,
    organized, immediate, constrained, regional).

% Bear the direct costs of mandates (e.g., exclusion from certain spaces, testing requirements, job loss). Their choices are to comply, face penalties, or exit mandated activities. Their resistance is often framed as a defense of personal autonomy.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, unvaccinated_individuals, payer,
    moderate, biographical, constrained, local).

% Face significant personal and social costs due to mandates that conflict with deeply held beliefs. Their identity is often fused with their objection, making compliance a profound personal sacrifice and exit from the objection unthinkable.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, individuals_with_conscientious_objections, payer,
    powerless, biographical, identity_locked, local).

% Monitor public health mandates for potential overreach and violations of constitutional rights. They provide legal challenges and public commentary, advocating for the least restrictive means and robust protections for individual autonomy.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, civil_liberties_advocates, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action to mitigate public health threats by balancing individual liberties against the common good, ensuring that interventions are proportionate to the threat and minimize coercion.
% TRANSFER_FUNCTION: Transfers a degree of individual autonomy (e.g., choice over medical interventions, access to public spaces) from individuals to the collective, in exchange for reduced disease transmission and protection of vulnerable populations and healthcare capacity.
% ABSENT_VOICES: Future generations, who will inherit the precedents set by current public health law, are absent. Their interests in both robust public health infrastructure and protected individual liberties are not directly represented in current debates.
% DISAPPEARANCE_RATIONALE: If the authority to issue and enforce public health mandates, guided by proportionality, vanished, society would struggle to respond to pandemics. Vulnerable populations would be at greater risk, healthcare systems would be more easily overwhelmed, and the social contract around collective action for health would dissolve, leading to significant societal reorganization.
% FOUNDING_PROBLEM: The need to manage infectious disease outbreaks and other public health crises that require collective action, while respecting individual rights and avoiding arbitrary state power.
% FOUNDING_PROBLEM_CORROBORATION: Public health experts, bioethicists, and constitutional scholars outside of government agencies consistently corroborate the ongoing challenge of balancing public health and individual rights, especially in novel pandemic scenarios. Legal precedents and academic literature attest to the enduring nature of this problem.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__proportionality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(public_health_mandate_authority__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__proportionality_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) and suppression (0.70) are substantial because mandates, by their nature, impose costs and restrict choices, requiring active enforcement. However, the 'proportionality' aspect means these are not maximal, as the constraint theoretically adjusts to minimize unnecessary imposition. Resistance (0.75) is high due to the inherent tension between individual autonomy and collective action, especially when mandates are perceived as disproportionate. Accessibility collapse (0.40) is moderate, as alternatives (e.g., voluntary measures, less restrictive policies) are often considered, but the mandate still significantly narrows options. Theater ratio (0.10) is low, as the constraint's function is genuinely about public health, not performative maintenance, though some enforcement might be symbolic.
 *
 * PERSPECTIVAL GAP:
 *   Public health authorities and beneficiaries view this as a necessary, justified coordination mechanism. Payers, particularly those with identity-locked objections, experience it as a coercive imposition on their autonomy. The engine's per-seat classification will highlight this divergence, showing a 'tangled rope' for payers and a 'rope' or even 'scaffold' (if temporary) for beneficiaries, depending on the specific context and threat level.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities are the agenda-setters, balancing interests. Immunocompromised individuals and healthcare systems are primary beneficiaries, gaining protection and stability. Unvaccinated individuals and those with conscientious objections are the payers, bearing the direct costs and restrictions. The 'proportionality' framework attempts to keep the directionality of payers from reaching full target (1.0) by requiring justification and minimizing coercion, but extraction is still significant.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading inherently attempts to prevent mandatrophy by requiring ongoing proportionality assessments. If the founding problem (public health threat) diminishes, the constraint's legitimacy and intensity should decrease. The 'contested' status of the founding problem reflects the ongoing debate about whether current threats justify the level of extraction and suppression, which is central to this reading's function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_measurement_ambiguity,
    'How is ''proportionality'' objectively measured and applied across diverse public health threats and individual contexts?',
    'Development of standardized, transparent, and empirically-grounded metrics for threat severity, efficacy of alternatives, and impact of coercion, subject to independent review.',
    'If proportionality metrics are clear and consistently applied, the constraint''s legitimacy and perceived fairness increase, potentially reducing resistance and clarifying the coordination function. If subjective or inconsistently applied, it risks becoming a ''snare'' where ''proportionality'' is a rhetorical cover for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_measurement_ambiguity, conceptual, 'Ambiguity in the operationalization of proportionality principles.').

omega_variable(
    threat_level_dynamic_extractiveness,
    'Does the actual extractiveness and suppression of mandates dynamically adjust to the severity of the public health threat, as the proportionality reading requires?',
    'Longitudinal empirical studies comparing mandate intensity (e.g., scope, duration, penalties) with objective epidemiological data across multiple public health crises.',
    'If mandates consistently scale with threat, the ''tangled rope'' classification is robust. If mandates remain high even as threats diminish, it indicates a drift towards ''snare'' or ''piton'' due to institutional inertia or rent-seeking, despite the stated proportionality principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_level_dynamic_extractiveness, empirical, 'Whether mandate intensity genuinely tracks threat level.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine ''proportionality reading'' of public health mandate authority, or is it a rhetorical framing for a different underlying structural constraint?',
    'Analysis of judicial decisions, legislative intent, and public health agency guidelines: do they consistently apply proportionality tests, or do they default to other principles (e.g., categorical public health protection, individual autonomy) when challenged?',
    'If genuinely applied, the constraint functions as a ''tangled rope'' with a built-in mechanism for legitimacy. If merely rhetorical, it could be a ''snare'' (if primarily extractive) or a ''rope'' (if primarily coordinative) disguised by the proportionality language.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is one reading of the ''public_health_mandate_authority'' kernel, specifically the ''proportionality_reading''. Sibling readings include ''public_health_primary'' (emphasizing collective protection) and ''bodily_autonomy_primary'' (emphasizing individual rights). The disagreement is located in the foundational axioms and the weighting of individual vs. collective interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__proportionality_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__proportionality_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(publ_tr_t5, public_health_mandate_authority__proportionality_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(publ_tr_t10, public_health_mandate_authority__proportionality_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(publ_tr_t15, public_health_mandate_authority__proportionality_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement(publ_tr_t20, public_health_mandate_authority__proportionality_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__proportionality_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(publ_be_t5, public_health_mandate_authority__proportionality_reading, base_extractiveness, 5, 0.7).
narrative_ontology:measurement(publ_be_t10, public_health_mandate_authority__proportionality_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(publ_be_t15, public_health_mandate_authority__proportionality_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(publ_be_t20, public_health_mandate_authority__proportionality_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__proportionality_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(publ_su_t5, public_health_mandate_authority__proportionality_reading, suppression_requirement, 5, 0.8).
narrative_ontology:measurement(publ_su_t10, public_health_mandate_authority__proportionality_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(publ_su_t15, public_health_mandate_authority__proportionality_reading, suppression_requirement, 15, 0.75).
narrative_ontology:measurement(publ_su_t20, public_health_mandate_authority__proportionality_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__proportionality_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'public_health_mandate_authority' kernel. It is linked to 'public_health_primary' and 'bodily_autonomy_primary' as sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
