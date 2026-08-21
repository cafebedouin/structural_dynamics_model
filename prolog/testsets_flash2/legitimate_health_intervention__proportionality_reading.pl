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
 *   This constraint represents the 'proportionality' reading of legitimate
 *   public health interventions. It asserts that the severity of public
 *   health measures (e.g., lockdowns, mandatory vaccinations) must be
 *   proportional to the threat posed by a disease, considering both
 *   population harm and individual autonomy. The victim set and the degree of
 *   extraction (ε) are conditional on disease characteristics like
 *   transmissibility and case-fatality rate. This reading seeks a middle
 *   ground between prioritizing public health outcomes and individual bodily
 *   autonomy.
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
narrative_ontology:cs_story_uid(legitimate_health_intervention__proportionality_reading, 'ef0e5e42-bc77-474c-b96e-3faaa1ac568a').
narrative_ontology:cs_kernel_codification('ef0e5e42-bc77-474c-b96e-3faaa1ac568a', formalized).
narrative_ontology:cs_authority_grounding('ef0e5e42-bc77-474c-b96e-3faaa1ac568a', expertise).
narrative_ontology:cs_interpretation_layer_present('ef0e5e42-bc77-474c-b96e-3faaa1ac568a').
narrative_ontology:cs_reading_relation('ef0e5e42-bc77-474c-b96e-3faaa1ac568a', legitimate_health_intervention__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('ef0e5e42-bc77-474c-b96e-3faaa1ac568a', legitimate_health_intervention__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_axiom('ef0e5e42-bc77-474c-b96e-3faaa1ac568a', foundational, intervention_severity_must_match_threat).
narrative_ontology:cs_axiom_status(intervention_severity_must_match_threat, holdable).
narrative_ontology:cs_axiom_grounding('ef0e5e42-bc77-474c-b96e-3faaa1ac568a', intervention_severity_must_match_threat, deontological).
narrative_ontology:cs_axiom('ef0e5e42-bc77-474c-b96e-3faaa1ac568a', foundational, balancing_population_harm_and_individual_autonomy).
narrative_ontology:cs_axiom_status(balancing_population_harm_and_individual_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('ef0e5e42-bc77-474c-b96e-3faaa1ac568a', balancing_population_harm_and_individual_autonomy, deontological).
narrative_ontology:cs_reference_frame('ef0e5e42-bc77-474c-b96e-3faaa1ac568a', ethical_public_health_governance).
narrative_ontology:cs_drift_state('ef0e5e42-bc77-474c-b96e-3faaa1ac568a', contemporary_pandemic_response, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ef0e5e42-bc77-474c-b96e-3faaa1ac568a', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, general_public).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, individuals_subject_to_intervention).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for implementing public health measures, balancing population protection with individual rights. They must justify interventions based on scientific evidence of threat and proportionality of response.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from reduced disease transmission and protection from severe health threats. Generally accepts interventions deemed proportional to the risk, but may resist those perceived as overly restrictive for minor threats.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, general_public, beneficiary,
    organized, biographical, mobile, national).

% Bear the direct costs of interventions (e.g., vaccination, quarantine, mask mandates), experiencing limitations on their autonomy. Their willingness to comply depends on the perceived severity of the threat and the justification for the intervention.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, individuals_subject_to_intervention, payer,
    moderate, immediate, constrained, local).

% Review the ethical implications of public health policies, ensuring adherence to principles like proportionality, beneficence, and non-maleficence. They provide guidance but typically lack direct enforcement power.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, medical_ethics_boards, observer,
    institutional, generational, analytical, national).

% Argue for the primacy of individual rights and bodily autonomy, often challenging public health measures that they deem disproportionate or overly coercive. Their voice is often heard in legal challenges and public discourse.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, civil_liberties_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates public health responses by providing a framework to balance collective safety with individual liberties, ensuring interventions are justified by the threat level and minimize infringement on autonomy.
% TRANSFER_FUNCTION: Transfers a degree of individual autonomy to public health authorities in exchange for collective protection from disease, with the extent of transfer determined by the severity and transmissibility of the threat.
% ABSENT_VOICES: Those who prioritize absolute individual autonomy regardless of public health risk are often marginalized in policy discussions, as their stance is seen as incompatible with collective action. Similarly, those advocating for maximal public health intervention regardless of individual cost are also excluded from this proportionality framework.
% DISAPPEARANCE_RATIONALE: Without a proportionality principle, public health interventions would either become overly coercive (leading to widespread resistance and ethical breaches) or entirely ineffective (due to lack of justified action), causing significant societal disruption and loss of trust in public health institutions.
% FOUNDING_PROBLEM: Historically, public health measures often swung between extreme coercion and insufficient response, lacking a consistent ethical framework to guide interventions that respected individual rights while protecting the population.
% FOUNDING_PROBLEM_CORROBORATION: Medical ethicists, constitutional scholars, and public health historians corroborate the ongoing challenge of balancing these competing values, especially during novel health crises. The principle is continually debated and refined in response to new threats and societal values.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__proportionality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.45) is moderate, reflecting the necessary infringement on individual autonomy for collective good, but limited by the proportionality principle. Suppression (0.30) is also moderate, as interventions require some enforcement but are ideally accepted due to their justified nature. Theater ratio is low (0.10) because the principle demands genuine justification, not performative action. Accessibility collapse (0.40) is moderate, as alternatives to intervention are constrained but not entirely eliminated, and resistance (0.25) is present but generally lower than for perceived disproportionate measures.
 *
 * PERSPECTIVAL GAP:
 *   This reading attempts to bridge the gap between those prioritizing public health and those prioritizing individual autonomy. From the perspective of public health authorities, it provides a defensible framework for action. From the perspective of individuals, it offers a safeguard against arbitrary or excessive state power. The tension lies in the interpretation of 'proportionality' itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities are agenda-setters, balancing competing demands. The general public benefits from protection but also bears some costs. Individuals subject to intervention are payers, directly experiencing the autonomy costs. Civil liberties advocates are excluded from the core decision-making but influence the discourse. The proportionality principle aims to keep the directionality for individuals closer to symmetric than to full target, by limiting the scope of extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_measurement_ambiguity,
    'How is ''proportionality'' objectively measured and agreed upon, especially when balancing quantifiable population harm against qualitative individual autonomy?',
    'Development of standardized, multi-criteria decision frameworks for public health ethics, with transparent weighting of different values and public deliberation processes.',
    'If a robust, agreed-upon measurement of proportionality emerges, the constraint''s legitimacy and acceptance would increase, potentially lowering resistance and suppression requirements. If it remains subjective, the constraint will be perpetually contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_measurement_ambiguity, conceptual, 'Ambiguity in defining and measuring proportionality in practice.').

omega_variable(
    disease_characteristics_weighting,
    'What is the appropriate weighting of disease characteristics (e.g., transmissibility, case-fatality rate, long-term disability) in determining the ''threat level'' that justifies intervention severity?',
    'Consensus among epidemiologists, public health experts, and ethicists on a transparent, evidence-based scoring system for disease threat levels, subject to periodic review.',
    'Clearer weighting would reduce contestation over specific interventions, making the constraint''s application more consistent. Disagreement would lead to continued disputes over the legitimacy of measures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disease_characteristics_weighting, empirical, 'Uncertainty in how disease characteristics should quantitatively inform intervention severity.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal mandates, fines) or internalized (social pressure, fear of disease)?',
    'Post-intervention compliance trajectory: if compliance persists after legal mandates are lifted, reclassify as partially internalized. Surveys on public attitudes towards interventions.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as individuals self-regulate. If purely structural, resistance may be higher once enforcement wanes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for public health measures.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__proportionality_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__proportionality_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(legi_be_t5, legitimate_health_intervention__proportionality_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(legi_be_t10, legitimate_health_intervention__proportionality_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(legi_be_t15, legitimate_health_intervention__proportionality_reading, base_extractiveness, 15, 0.43).
narrative_ontology:measurement(legi_be_t20, legitimate_health_intervention__proportionality_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__proportionality_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(legi_su_t5, legitimate_health_intervention__proportionality_reading, suppression_requirement, 5, 0.28).
narrative_ontology:measurement(legi_su_t10, legitimate_health_intervention__proportionality_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(legi_su_t15, legitimate_health_intervention__proportionality_reading, suppression_requirement, 15, 0.29).
narrative_ontology:measurement(legi_su_t20, legitimate_health_intervention__proportionality_reading, suppression_requirement, 20, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__proportionality_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'legitimate_health_intervention' kernel, focusing on proportionality. It interacts with 'public_health_primary' and 'bodily_autonomy_primary' readings, which emphasize different aspects of legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
