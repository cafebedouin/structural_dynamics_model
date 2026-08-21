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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: dsm_taxonomy_kernel__neurodiversity_reading
 *   human_readable: DSM Categories as Pathologization of Neurodiversity
 *   domain: medical_epistemology/psychiatric_taxonomy/social_construction_of_illness
 *
 * SUMMARY:
 *   This constraint story represents the 'neurodiversity reading' of the DSM
 *   taxonomy kernel. From this perspective, the DSM's categories function as
 *   a snare, pathologizing natural human neurological variation that
 *   conflicts with prevailing institutional and social behavioral norms. The
 *   constraint's operation extracts conformity and suppresses neurodivergent
 *   ways of being, with identifiable victims in neurodivergent individuals
 *   and beneficiaries in institutional systems and the neurotypical majority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__neurodiversity_reading, 0.85).
domain_priors:suppression_score(dsm_taxonomy_kernel__neurodiversity_reading, 0.9).
domain_priors:theater_ratio(dsm_taxonomy_kernel__neurodiversity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__neurodiversity_reading, snare).
narrative_ontology:human_readable(dsm_taxonomy_kernel__neurodiversity_reading, "DSM Categories as Pathologization of Neurodiversity").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__neurodiversity_reading, "medical_epistemology/psychiatric_taxonomy/social_construction_of_illness").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__neurodiversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__neurodiversity_reading, '66430d8b-c347-4a5e-9f59-09f3438f536e').
narrative_ontology:cs_kernel_codification('66430d8b-c347-4a5e-9f59-09f3438f536e', formalized).
narrative_ontology:cs_authority_grounding('66430d8b-c347-4a5e-9f59-09f3438f536e', expertise).
narrative_ontology:cs_interpretation_layer_present('66430d8b-c347-4a5e-9f59-09f3438f536e').
narrative_ontology:cs_reading_relation('66430d8b-c347-4a5e-9f59-09f3438f536e', dsm_taxonomy_kernel__biomedical_reading, forecloses).
narrative_ontology:cs_reading_relation('66430d8b-c347-4a5e-9f59-09f3438f536e', dsm_taxonomy_kernel__critical_psychiatry_reading, coexists_with).
narrative_ontology:cs_axiom('66430d8b-c347-4a5e-9f59-09f3438f536e', foundational, neurodiversity_is_natural_variation).
narrative_ontology:cs_axiom_status(neurodiversity_is_natural_variation, holdable).
narrative_ontology:cs_axiom_grounding('66430d8b-c347-4a5e-9f59-09f3438f536e', neurodiversity_is_natural_variation, deontological).
narrative_ontology:cs_axiom('66430d8b-c347-4a5e-9f59-09f3438f536e', foundational, pathologization_of_difference_is_harmful).
narrative_ontology:cs_axiom_status(pathologization_of_difference_is_harmful, holdable).
narrative_ontology:cs_axiom_grounding('66430d8b-c347-4a5e-9f59-09f3438f536e', pathologization_of_difference_is_harmful, deontological).
narrative_ontology:cs_reference_frame('66430d8b-c347-4a5e-9f59-09f3438f536e', neurodiversity_paradigm_of_difference).
narrative_ontology:cs_drift_state('66430d8b-c347-4a5e-9f59-09f3438f536e', contemporary_neurodiversity_movement, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('66430d8b-c347-4a5e-9f59-09f3438f536e', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, institutional_systems_of_conformity).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, neurotypical_majority).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, psychiatric_profession).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_individuals).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, neurodiversity_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These individuals are labeled with DSM diagnoses, leading to pathologization of their natural neurological differences. They bear the costs of stigma, medical interventions aimed at 'normalizing' them, and denial of accommodations. Their identity is often fused with their diagnosis, making 'exit' from the diagnostic framework a profound challenge to self-concept.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_individuals, payer,
    powerless, biographical, identity_locked, global).

% Institutions like schools, workplaces, and carceral systems benefit from the DSM's categories by having a framework to classify and manage individuals who do not conform to behavioral norms. They enforce these norms and use diagnoses to justify interventions or deny accommodations, thereby maintaining their operational efficiency and existing structures.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, institutional_systems_of_conformity, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__neurodiversity_reading, institutional_systems_of_conformity, agenda_setter).

% The neurotypical majority benefits from the social conformity reinforced by DSM categories. These categories provide a language to understand and manage 'deviance' from typical neurological functioning, reducing social friction and validating existing social structures and expectations. Exiting this framework would require a fundamental shift in social understanding and interaction.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurotypical_majority, beneficiary,
    organized, biographical, constrained, global).

% The psychiatric profession creates, maintains, and applies the DSM. It benefits from its authority in defining mental health and illness, guiding research, and legitimizing interventions. While some members advocate for more nuanced approaches, the profession as a whole is deeply invested in the existing diagnostic paradigm.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, psychiatric_profession, agenda_setter,
    institutional, generational, constrained, global).

% These advocates actively resist the pathologization inherent in DSM categories. They bear the costs of challenging established medical authority, fighting for recognition and accommodation, and educating the public. Their exit options are constrained by their commitment to systemic change and the well-being of neurodivergent communities.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurodiversity_advocates, payer,
    organized, biographical, constrained, global).

% These scholars analyze the social, political, and economic functions of psychiatric diagnoses, often critiquing the DSM from various theoretical perspectives. They provide an analytical lens on the constraint's operation but are not directly subject to its coercive force in the same way as neurodivergent individuals.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, critical_psychiatry_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dsm_taxonomy_kernel__neurodiversity_reading, institutional_systems_of_conformity).
narrative_ontology:fixing_cost_class(dsm_taxonomy_kernel__neurodiversity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, ostensibly objective, nomenclature for classifying human neurological and behavioral variations, facilitating communication among clinicians, researchers, and institutions.
% TRANSFER_FUNCTION: Transfers social legitimacy, resources, and power to institutions and professionals who define and manage 'disorder,' while transferring stigma, medicalization, and pressure for conformity onto neurodivergent individuals.
% ABSENT_VOICES: Many neurodivergent individuals, particularly those with significant communication differences or those from marginalized communities, are often excluded from the diagnostic process and from shaping the categories that define their lives. Their perspectives would challenge the very premise of pathologization.
% DISAPPEARANCE_RATIONALE: If DSM categories vanished overnight, the entire framework for understanding and responding to neurological differences would collapse. Institutions would lose their primary tool for classification and management, leading to a profound reorganization of educational, medical, and social support systems, and potentially fostering new, non-pathologizing approaches to human variation.
% FOUNDING_PROBLEM: To provide a common language and diagnostic criteria for mental disorders, improving reliability and validity in psychiatric diagnosis, and facilitating research into etiology and treatment.
% FOUNDING_PROBLEM_CORROBORATION: The psychiatric profession and biomedical researchers largely attest that the founding problem of diagnostic reliability and validity remains live. Neurodiversity advocates and critical psychiatry scholars, however, attest that the problem has either been superseded by social construction or that the DSM's categories primarily serve to pathologize natural variation rather than identify objective disease, making the original problem either dead or fundamentally reframed.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__neurodiversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__neurodiversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__neurodiversity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(dsm_taxonomy_kernel__neurodiversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__neurodiversity_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) because the act of pathologization itself is seen as a profound harm, leading to denial of self-determination and coercive normalization. Suppression is very high (0.90) due to the pervasive institutional enforcement of neurotypical norms and the systemic pressure on individuals to conform or seek 'treatment.' Theater ratio is moderate (0.40) as some diagnostic activity genuinely aims to describe challenges, but a significant portion serves to legitimize the pathologization and maintain the existing power structures. Accessibility collapse is high (0.75) because the diagnostic framework limits alternative understandings of neurodiversity, making it difficult to conceptualize differences outside a medicalized lens. Resistance is high (0.70) due to the active and growing neurodiversity movement challenging the DSM's authority.
 *
 * PERSPECTIVAL GAP:
 *   The psychiatric profession and institutional beneficiaries experience the DSM as a necessary tool for order and care, while neurodivergent individuals and their advocates experience it as a tool of control and pathologization. The engine's classification will highlight this divergence, showing a claimed 'rope' (from the perspective of the psychiatric profession's stated goals) operating as a 'snare' from the perspective of its targets.
 *
 * DIRECTIONALITY LOGIC:
 *   Neurodivergent individuals are the primary targets (high d) as they bear the direct costs of pathologization and coercive interventions. Institutional systems of conformity and the neurotypical majority are beneficiaries (low d) as they benefit from the social order and predictability maintained by the diagnostic framework. The psychiatric profession, as the agenda-setter, also benefits from its epistemic authority and role in defining 'disorder.' Neurodiversity advocates, while resisting, also bear costs (payer role) in their efforts to challenge the system.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dsm_reading_identity,
    'Is this constraint accurately representing the ''neurodiversity_reading'' of the DSM taxonomy kernel?',
    'Comparison with core tenets of neurodiversity theory and advocacy literature; expert review by neurodiversity scholars.',
    'If misaligned, the analysis of the DSM''s function from a neurodiversity perspective would be inaccurate, potentially misrepresenting the nature of extraction and suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dsm_reading_identity, conceptual, 'Ensures fidelity to the specified kernel reading.').

omega_variable(
    pathologization_vs_disability_locus,
    'To what extent is the harm experienced by neurodivergent individuals primarily due to pathologization (social construction) versus genuine disability (inherent functional limitations)?',
    'Empirical studies on the impact of social accommodation vs. medical intervention on well-being and functional capacity, as reported by neurodivergent individuals themselves.',
    'If harm is primarily from pathologization, the extractiveness and suppression metrics are fully attributable to the constraint''s social function. If a significant portion is due to inherent disability, the constraint''s role in addressing genuine need would need re-evaluation, potentially lowering the effective extraction attributable to pathologization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pathologization_vs_disability_locus, empirical, 'Distinguishes between socially constructed harm and inherent functional challenges.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (institutional barriers, lack of accommodation) or internalized (self-stigma, belief in one''s own ''brokenness'') for neurodivergent individuals?',
    'Post-exit suppression trajectory: if self-stigma and internalized norms persist after structural barriers are removed, reclassify as partially internalized. Qualitative studies on lived experience.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — neurodivergent individuals carry the suppression with them even in more accommodating environments, making exit harder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in neurodiversity context.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__neurodiversity_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t1980, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(dsm__tr_t1990, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(dsm__tr_t2000, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(dsm__tr_t2010, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(dsm__tr_t2020, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 2020, 0.38).
narrative_ontology:measurement(dsm__tr_t2025, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(dsm__be_t1980, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(dsm__be_t1990, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement(dsm__be_t2000, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(dsm__be_t2010, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 2010, 0.82).
narrative_ontology:measurement(dsm__be_t2020, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 2020, 0.84).
narrative_ontology:measurement(dsm__be_t2025, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t1980, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(dsm__su_t1990, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 1990, 0.78).
narrative_ontology:measurement(dsm__su_t2000, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 2000, 0.85).
narrative_ontology:measurement(dsm__su_t2010, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 2010, 0.88).
narrative_ontology:measurement(dsm__su_t2020, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 2020, 0.89).
narrative_ontology:measurement(dsm__su_t2025, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 2025, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__neurodiversity_reading, identity_coordination).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, mental_health_funding_allocation).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, educational_accommodation_standards).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, employment_discrimination_laws).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
