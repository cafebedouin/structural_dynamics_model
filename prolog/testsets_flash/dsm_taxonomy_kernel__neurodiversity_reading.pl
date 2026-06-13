% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__neurodiversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dsm_taxonomy_kernel__neurodiversity_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dsm_taxonomy_kernel__neurodiversity_reading
 *   human_readable: DSM Pathologization of Neurodiversity
 *   domain: medical_epistemology/psychiatric_taxonomy/social_construction_of_illness
 *
 * SUMMARY:
 *   This constraint represents the neurodiversity reading of the DSM taxonomy
 *   kernel, where diagnostic categories are seen as pathologizing natural
 *   human neurological variation. It highlights how these categories create a
 *   framework for coercive normalization and deny accommodation, benefiting
 *   institutional systems that demand conformity. The claimed type is 'snare'
 *   because the coordination story (standardized diagnosis) is seen as cover
 *   for extraction (pathologization, denial of self-determination) and
 *   suppression of alternatives (neurodiversity-affirming models).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__neurodiversity_reading, 0.85).
domain_priors:suppression_score(dsm_taxonomy_kernel__neurodiversity_reading, 0.9).
domain_priors:theater_ratio(dsm_taxonomy_kernel__neurodiversity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__neurodiversity_reading, snare).
narrative_ontology:human_readable(dsm_taxonomy_kernel__neurodiversity_reading, "DSM Pathologization of Neurodiversity").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__neurodiversity_reading, "medical_epistemology/psychiatric_taxonomy/social_construction_of_illness").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__neurodiversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__neurodiversity_reading, '327653bc-fe03-4a42-b5d2-0c989b63d65a').
narrative_ontology:cs_kernel_codification('327653bc-fe03-4a42-b5d2-0c989b63d65a', formalized).
narrative_ontology:cs_authority_grounding('327653bc-fe03-4a42-b5d2-0c989b63d65a', lineage).
narrative_ontology:cs_interpretation_layer_present('327653bc-fe03-4a42-b5d2-0c989b63d65a').
narrative_ontology:cs_reading_relation('327653bc-fe03-4a42-b5d2-0c989b63d65a', dsm_taxonomy_kernel__biomedical_reading, coexists_with).
narrative_ontology:cs_reading_relation('327653bc-fe03-4a42-b5d2-0c989b63d65a', dsm_taxonomy_kernel__critical_psychiatry_reading, coexists_with).
narrative_ontology:cs_axiom('327653bc-fe03-4a42-b5d2-0c989b63d65a', foundational, neurodiversity_is_natural_variation).
narrative_ontology:cs_axiom_status(neurodiversity_is_natural_variation, holdable).
narrative_ontology:cs_axiom_grounding('327653bc-fe03-4a42-b5d2-0c989b63d65a', neurodiversity_is_natural_variation, deontological).
narrative_ontology:cs_axiom('327653bc-fe03-4a42-b5d2-0c989b63d65a', foundational, pathologization_is_harm).
narrative_ontology:cs_axiom_status(pathologization_is_harm, holdable).
narrative_ontology:cs_axiom_grounding('327653bc-fe03-4a42-b5d2-0c989b63d65a', pathologization_is_harm, deontological).
narrative_ontology:cs_reference_frame('327653bc-fe03-4a42-b5d2-0c989b63d65a', neurodiversity_affirming_paradigm).
narrative_ontology:cs_drift_state('327653bc-fe03-4a42-b5d2-0c989b63d65a', contemporary_dsm_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('327653bc-fe03-4a42-b5d2-0c989b63d65a', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, institutional_systems_of_conformity).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, neurotypical_majority).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_individuals).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, neurodiversity_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are labeled with DSM diagnoses that pathologize their natural neurological differences, leading to coercive normalization attempts (therapies, medications) and denial of accommodations. Their identity is often fused with the diagnosis, making 'exit' from the diagnostic framework a profound challenge to self-concept and access to support.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_individuals, payer,
    powerless, biographical, identity_locked, global).

% Schools, employers, and carceral systems benefit from the DSM's categories by having a framework to label and manage individuals whose behaviors conflict with institutional norms. They use diagnoses to justify interventions, deny accommodations, or exclude individuals, maintaining systemic order and reducing friction for the neurotypical majority.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, institutional_systems_of_conformity, agenda_setter,
    institutional, generational, constrained, national).

% Actively resist the pathologizing framework of the DSM, advocating for recognition of neurodiversity as a natural form of human variation. They bear the cost of challenging entrenched medical and social systems, often facing professional marginalization or dismissal.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurodiversity_advocates, payer,
    organized, generational, constrained, global).

% Benefits from the DSM's categories by having a framework that implicitly validates their own neurological and behavioral norms as 'typical' or 'healthy.' The pathologization of neurodivergence reinforces their social position and reduces the perceived need for systemic change to accommodate difference.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurotypical_majority, beneficiary,
    powerful, biographical, mobile, global).

% Are the primary authors and enforcers of the DSM. Their professional identity and authority are deeply intertwined with the diagnostic system. While some may genuinely seek to alleviate suffering, the system itself provides their professional mandate and legitimizes their interventions, making exit from the framework difficult without challenging their own professional identity.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, psychiatric_professionals, agenda_setter,
    institutional, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized language and framework for identifying, classifying, and managing neurological and behavioral differences within medical, educational, and social systems.
% TRANSFER_FUNCTION: Transfers social and institutional power from neurodivergent individuals to institutional systems and psychiatric professionals, by defining 'normal' and 'pathological' and legitimizing interventions.
% ABSENT_VOICES: Many neurodivergent individuals and their advocates are excluded from the DSM's authorship and revision processes, despite being the primary subjects of its classifications. Their voices would challenge the very premise of pathologization and demand a paradigm shift towards accommodation and acceptance.
% DISAPPEARANCE_RATIONALE: If the DSM taxonomy vanished overnight, the medical, educational, and social systems that rely on its categories for diagnosis, treatment, funding, and exclusion would be forced to fundamentally reorganize. The concept of 'disorder' for many neurological variations would collapse, leading to a re-evaluation of support structures and a shift towards neurodiversity-affirming practices.
% FOUNDING_PROBLEM: To provide a common nomenclature for mental disorders, standardize diagnostic criteria, and facilitate research and treatment planning in psychiatry.
% FOUNDING_PROBLEM_CORROBORATION: The psychiatric profession and many medical institutions attest the problem is still live, citing the need for consistent diagnosis and treatment. Neurodiversity advocates and critical psychiatry scholars, from outside the benefiting parties, attest that while a common language is useful, the current DSM framework has overshot its original problem, becoming a tool for social control and pathologization rather than purely for clinical utility.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__neurodiversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__neurodiversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__neurodiversity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dsm_taxonomy_kernel__neurodiversity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dsm_taxonomy_kernel__neurodiversity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dsm_taxonomy_kernel__neurodiversity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dsm_taxonomy_kernel__neurodiversity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the act of pathologization itself is a profound harm, leading to loss of self-determination, forced interventions, and systemic discrimination. Suppression is very high (0.9) due to the medical authority and institutional power behind the DSM, which actively suppresses alternative understandings of neurodiversity and limits access to non-pathologizing support. Theater ratio is low (0.2) because the DSM's function, from this reading, is quite direct in its pathologizing and normalizing effects, with less performative maintenance of a defunct coordination function.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of neurodivergent individuals, the DSM is a snare that traps them in a pathologizing framework. From the perspective of institutional systems, it's a necessary tool for managing diversity and maintaining order. The psychiatric profession, as agenda-setters, may perceive it as a rope or scaffold for clinical practice, while this reading highlights its extractive and suppressive functions.
 *
 * DIRECTIONALITY LOGIC:
 *   Neurodivergent individuals are the primary targets (d=1.0) as they bear the direct costs of pathologization and coercive interventions. Institutional systems of conformity (schools, employers) and the neurotypical majority are beneficiaries (d=0.0-0.2) as the DSM framework helps maintain their norms and reduces the perceived need for systemic change. Psychiatric professionals, as agenda-setters, also benefit from the authority and mandate the DSM provides, making their directionality closer to beneficiary (d=0.1). Neurodiversity advocates are also targets (d=0.8) as they actively resist the system.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the DSM as a 'rope' (pure coordination) by explicitly identifying the victims and the high extractiveness inherent in pathologizing natural variation. It highlights that the 'coordination' function (standardized diagnosis) is deeply intertwined with an extractive function (coercive normalization), making it a snare from the neurodiversity perspective. The 'founding problem status' being 'contested' further supports this, indicating that the original mandate has been superseded by a more extractive operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_variation_vs_disorder,
    'Is the observed neurological variation a natural form of human diversity or a genuine disorder requiring medical intervention?',
    'Longitudinal studies of neurodivergent individuals in supportive, accommodating environments vs. pathologizing ones; cross-cultural comparisons of diagnostic prevalence and outcomes; qualitative research on lived experience.',
    'If confirmed as natural variation, the DSM''s categories are purely extractive (snare); if confirmed as disorder, the constraint might shift towards a tangled rope (coordination with necessary costs).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_variation_vs_disorder, conceptual, 'The fundamental conceptual ambiguity between difference and deficit.').

omega_variable(
    identity_lock_mechanism,
    'What proportion of ''identity_locked'' exit for neurodivergent individuals is due to internalized pathologization versus reliance on diagnosis for access to support services?',
    'Surveys and qualitative interviews with neurodivergent individuals who have successfully exited the diagnostic framework, exploring their motivations and challenges; analysis of policy changes that decouple support from diagnosis.',
    'If primarily internalized, the suppression is deeper and more resistant to external change; if primarily access-driven, policy changes could reduce the identity lock without challenging self-concept.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Distinguishing internalized vs. instrumental identity lock for neurodivergent individuals.').

omega_variable(
    institutional_conformity_necessity,
    'To what extent are institutional behavioral norms (e.g., in schools, workplaces) genuinely necessary for their function, versus arbitrary impositions that could be adapted to neurodiversity?',
    'Pilot programs implementing neurodiversity-affirming institutional designs; comparative studies of productivity and well-being in diverse vs. conformity-demanding environments.',
    'If norms are largely arbitrary, the institutional benefit from DSM categories is purely extractive; if genuinely necessary, the constraint might have a stronger coordination component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_conformity_necessity, empirical, 'Necessity of institutional conformity vs. adaptability to neurodiversity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__neurodiversity_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t1980, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(dsm__tr_t1994, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 1994, 0.15).
narrative_ontology:measurement(dsm__tr_t2013, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 2013, 0.2).
narrative_ontology:measurement(dsm__tr_t2024, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(dsm__be_t1980, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(dsm__be_t1994, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 1994, 0.7).
narrative_ontology:measurement(dsm__be_t2013, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 2013, 0.8).
narrative_ontology:measurement(dsm__be_t2024, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t1980, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(dsm__su_t1994, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 1994, 0.8).
narrative_ontology:measurement(dsm__su_t2013, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 2013, 0.85).
narrative_ontology:measurement(dsm__su_t2024, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
