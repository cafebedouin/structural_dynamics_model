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
 *   constraint_id: legitimate_health_intervention__proportionality_reading
 *   human_readable: Proportionality Principle for Public Health Interventions
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the 'proportionality reading' of legitimate
 *   health interventions, asserting that the severity of public health
 *   measures must be proportional to the threat level of the disease,
 *   considering both population harm and individual autonomy. The victim set
 *   (individuals subject to interventions) and the degree of extraction
 *   (curtailment of autonomy) are conditional on disease characteristics like
 *   transmissibility and case-fatality rate. This reading seeks a balance,
 *   contrasting with readings that prioritize either public health or
 *   individual autonomy absolutely.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__proportionality_reading, 0.45).
domain_priors:suppression_score(legitimate_health_intervention__proportionality_reading, 0.3).
domain_priors:theater_ratio(legitimate_health_intervention__proportionality_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__proportionality_reading, rope).
narrative_ontology:human_readable(legitimate_health_intervention__proportionality_reading, "Proportionality Principle for Public Health Interventions").
narrative_ontology:topic_domain(legitimate_health_intervention__proportionality_reading, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__proportionality_reading, '4b06402a-4438-4704-9c3a-d2e864cd62c5').
narrative_ontology:cs_kernel_codification('4b06402a-4438-4704-9c3a-d2e864cd62c5', formalized).
narrative_ontology:cs_authority_grounding('4b06402a-4438-4704-9c3a-d2e864cd62c5', lineage).
narrative_ontology:cs_interpretation_layer_present('4b06402a-4438-4704-9c3a-d2e864cd62c5').
narrative_ontology:cs_reading_relation('4b06402a-4438-4704-9c3a-d2e864cd62c5', legitimate_health_intervention__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('4b06402a-4438-4704-9c3a-d2e864cd62c5', legitimate_health_intervention__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_axiom('4b06402a-4438-4704-9c3a-d2e864cd62c5', foundational, intervention_severity_must_match_threat).
narrative_ontology:cs_axiom_status(intervention_severity_must_match_threat, holdable).
narrative_ontology:cs_axiom_grounding('4b06402a-4438-4704-9c3a-d2e864cd62c5', intervention_severity_must_match_threat, empirically_contingent).
narrative_ontology:cs_axiom('4b06402a-4438-4704-9c3a-d2e864cd62c5', foundational, balance_collective_good_and_individual_rights).
narrative_ontology:cs_axiom_status(balance_collective_good_and_individual_rights, holdable).
narrative_ontology:cs_axiom_grounding('4b06402a-4438-4704-9c3a-d2e864cd62c5', balance_collective_good_and_individual_rights, deontological).
narrative_ontology:cs_reference_frame('4b06402a-4438-4704-9c3a-d2e864cd62c5', post_nuremberg_code_ethics).
narrative_ontology:cs_drift_state('4b06402a-4438-4704-9c3a-d2e864cd62c5', contemporary_pandemic_response, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4b06402a-4438-4704-9c3a-d2e864cd62c5', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, general_population).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, individuals_subject_to_interventions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for designing and implementing public health measures, balancing population protection with individual rights. They interpret disease characteristics to determine appropriate intervention levels.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from reduced disease transmission and severity due to proportionate interventions. Experiences minor inconveniences or restrictions but generally accepts measures deemed reasonable.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, general_population, beneficiary,
    organized, biographical, mobile, national).

% Bears the direct costs of interventions, such as mandatory vaccination, quarantine, or mask mandates. Their autonomy is curtailed, but the severity of the curtailment is theoretically justified by the threat.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, individuals_subject_to_interventions, payer,
    moderate, immediate, constrained, local).

% Review and advise on the ethical implications of public health policies, ensuring proportionality and respect for individual rights. Their influence is advisory but can shape policy and legal challenges.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, medical_ethics_boards, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates public health responses by providing a framework to balance collective safety with individual liberties, ensuring interventions are neither excessive nor insufficient for a given health threat.
% TRANSFER_FUNCTION: Transfers a degree of individual autonomy to public health authorities in exchange for collective protection against disease, with the extent of transfer determined by the threat's characteristics.
% ABSENT_VOICES: Those who prioritize absolute individual autonomy or absolute public health outcomes might feel their perspectives are not fully represented, as this reading seeks a balance. Their arguments are often heard in legal challenges or public discourse.
% DISAPPEARANCE_RATIONALE: Without a proportionality principle, public health interventions would either become overly coercive (ignoring individual rights) or entirely ineffective (failing to protect the population), leading to a breakdown in public trust and effective disease management. The legal and ethical landscape would be fundamentally altered.
% FOUNDING_PROBLEM: To establish a legitimate basis for state intervention in individual health decisions during public health crises, preventing both tyranny and anarchy in health governance.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars, medical ethicists, and international human rights organizations consistently affirm the ongoing necessity of proportionality in public health law and policy, citing historical abuses and contemporary challenges. This corroboration comes from outside the direct beneficiaries of specific interventions.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__proportionality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legitimate_health_intervention__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__proportionality_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__proportionality_reading_tests).
:- end_tests(legitimate_health_intervention__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the necessary curtailment of individual autonomy for collective good, which is inherent in any public health intervention, but it is moderated by the proportionality requirement. Suppression (0.30) is present as interventions require compliance, but it is lower than it would be for an absolute public health mandate, as the constraint itself limits coercive force. The slight increase in extractiveness and suppression around 2020 reflects the global pandemic response, followed by a recalibration as proportionality principles were re-emphasized.
 *
 * PERSPECTIVAL GAP:
 *   Public health authorities, while acting as agenda-setters, are also constrained by this principle, experiencing it as a guide for legitimate action. Individuals subject to interventions experience it as a necessary, but sometimes burdensome, imposition. The engine's per-seat classification would reflect these different experiences, with authorities seeing a 'rope' (coordination) and individuals a 'tangled_rope' (coordination with extraction).
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities and the general population are beneficiaries, as the constraint provides a framework for effective and legitimate disease control. Individuals subject to interventions are payers, as they bear the direct costs of curtailed autonomy. The directionality for individuals is higher due to the direct impact on their choices, even if justified by proportionality.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint actively prevents mandatrophy by requiring continuous re-evaluation of interventions against evolving threat levels. If the threat diminishes, the justification for severe interventions also diminishes, preventing the constraint from persisting beyond its functional mandate. The 'live' status of the founding problem, corroborated by external sources, indicates it remains relevant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    weighting_of_harms_and_autonomy,
    'How should population harm and individual autonomy be quantitatively weighted against each other for different disease characteristics?',
    'Development of a universally accepted ethical framework or legal precedent that provides clear, context-dependent weighting algorithms, potentially informed by public deliberation and expert consensus.',
    'Different weighting schemes would alter the ''extractiveness'' and ''suppression'' metrics, potentially shifting the constraint''s classification for individuals towards a ''snare'' (if autonomy is undervalued) or a ''piton'' (if public health is undervalued and interventions become performative).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(weighting_of_harms_and_autonomy, conceptual, 'Ambiguity in the precise balance between collective and individual values.').

omega_variable(
    empirical_assessment_of_threat_level,
    'What constitutes a ''threat level'' that justifies specific intervention severities, and how reliably can this be empirically assessed?',
    'Standardization of epidemiological data collection, modeling, and risk assessment methodologies across jurisdictions, coupled with independent scientific review to minimize political influence.',
    'Inaccurate or biased threat assessments could lead to disproportionate interventions, increasing ''extractiveness'' and ''suppression'' beyond what is justified, potentially reclassifying the constraint as a ''tangled_rope'' or ''snare'' from the individual''s perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_assessment_of_threat_level, empirical, 'Uncertainty in the objective measurement of disease threat.').

omega_variable(
    proportionality_vs_absolute_rights,
    'Is the proportionality principle a sufficient safeguard for fundamental rights, or do some rights (e.g., bodily autonomy) require absolute protection regardless of threat?',
    'Ongoing legal challenges and constitutional interpretations, potentially leading to new jurisprudence that defines non-derogable rights even in public health emergencies.',
    'If certain rights are deemed absolute, this reading would be ''foreclosed'' by the ''bodily_autonomy_primary'' reading for those specific rights, leading to a different constraint structure where those interventions are illegitimate regardless of proportionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_vs_absolute_rights, preference, 'Conceptual tension between proportionality and absolute rights.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__proportionality_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(legi_be_t1950, legitimate_health_intervention__proportionality_reading, base_extractiveness, 1950, 0.35).
narrative_ontology:measurement(legi_be_t1970, legitimate_health_intervention__proportionality_reading, base_extractiveness, 1970, 0.38).
narrative_ontology:measurement(legi_be_t1990, legitimate_health_intervention__proportionality_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(legi_be_t2010, legitimate_health_intervention__proportionality_reading, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement(legi_be_t2020, legitimate_health_intervention__proportionality_reading, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement(legi_be_t2024, legitimate_health_intervention__proportionality_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1950, legitimate_health_intervention__proportionality_reading, suppression_requirement, 1950, 0.2).
narrative_ontology:measurement(legi_su_t1970, legitimate_health_intervention__proportionality_reading, suppression_requirement, 1970, 0.22).
narrative_ontology:measurement(legi_su_t1990, legitimate_health_intervention__proportionality_reading, suppression_requirement, 1990, 0.25).
narrative_ontology:measurement(legi_su_t2010, legitimate_health_intervention__proportionality_reading, suppression_requirement, 2010, 0.28).
narrative_ontology:measurement(legi_su_t2020, legitimate_health_intervention__proportionality_reading, suppression_requirement, 2020, 0.4).
narrative_ontology:measurement(legi_su_t2024, legitimate_health_intervention__proportionality_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__proportionality_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'legitimate_health_intervention' kernel, focusing on proportionality. It is distinct from readings prioritizing absolute public health or individual autonomy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
