% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__reformist_spiritual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_corpus_social_prescription__reformist_spiritual_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vedic_corpus_social_prescription__reformist_spiritual_reading
 *   human_readable: Vedic Corpus: Reformist Spiritual Reading (No Social Prescription)
 *   domain: religious_studies/social_stratification/hermeneutics
 *
 * SUMMARY:
 *   This constraint represents the 'reformist spiritual reading' of the Vedic
 *   corpus, which asserts that Vedic texts primarily describe spiritual unity
 *   and metaphorical cosmology, containing no prescriptive social content
 *   like the Varna system. This reading emerged in response to both orthodox
 *   literalism and colonial interpretations, aiming to reclaim a
 *   universalistic spiritual core. It functions as a Rope, coordinating
 *   spiritual practice without significant extraction or suppression, as its
 *   persistence relies on voluntary adherence to its interpretive framework.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.1).
domain_priors:suppression_score(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.05).
domain_priors:theater_ratio(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__reformist_spiritual_reading, rope).
narrative_ontology:human_readable(vedic_corpus_social_prescription__reformist_spiritual_reading, "Vedic Corpus: Reformist Spiritual Reading (No Social Prescription)").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__reformist_spiritual_reading, "religious_studies/social_stratification/hermeneutics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__reformist_spiritual_reading, 'c5d4f32c-1929-42fd-b4e7-d395f78ad789').
narrative_ontology:cs_kernel_codification('c5d4f32c-1929-42fd-b4e7-d395f78ad789', fixed_text).
narrative_ontology:cs_authority_grounding('c5d4f32c-1929-42fd-b4e7-d395f78ad789', lineage).
narrative_ontology:cs_interpretation_layer_present('c5d4f32c-1929-42fd-b4e7-d395f78ad789').
narrative_ontology:cs_reading_relation('c5d4f32c-1929-42fd-b4e7-d395f78ad789', vedic_corpus_social_prescription__orthodox_varna_reading, coexists_with).
narrative_ontology:cs_reading_relation('c5d4f32c-1929-42fd-b4e7-d395f78ad789', vedic_corpus_social_prescription__colonial_orientalist_reading, forecloses).
narrative_ontology:cs_axiom('c5d4f32c-1929-42fd-b4e7-d395f78ad789', foundational, vedas_are_primarily_spiritual).
narrative_ontology:cs_axiom_status(vedas_are_primarily_spiritual, holdable).
narrative_ontology:cs_axiom_grounding('c5d4f32c-1929-42fd-b4e7-d395f78ad789', vedas_are_primarily_spiritual, deontological).
narrative_ontology:cs_axiom('c5d4f32c-1929-42fd-b4e7-d395f78ad789', foundational, social_hierarchy_is_not_vedic).
narrative_ontology:cs_axiom_status(social_hierarchy_is_not_vedic, holdable).
narrative_ontology:cs_axiom_grounding('c5d4f32c-1929-42fd-b4e7-d395f78ad789', social_hierarchy_is_not_vedic, conventional).
narrative_ontology:cs_reference_frame('c5d4f32c-1929-42fd-b4e7-d395f78ad789', universal_spiritual_truth).
narrative_ontology:cs_drift_state('c5d4f32c-1929-42fd-b4e7-d395f78ad789', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c5d4f32c-1929-42fd-b4e7-d395f78ad789', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, reformist_hindu_movements).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, spiritual_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__reformist_spiritual_reading, orthodox_varna_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promote a reading of Vedic texts that emphasizes spiritual unity and universalism, rejecting literal social hierarchies as later interpolations or misinterpretations. They actively interpret and disseminate this understanding.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, reformist_hindu_movements, agenda_setter,
    organized, generational, mobile, global).

% Find in this reading a path to spiritual growth unburdened by social stratification or caste-based discrimination. They benefit from the inclusive and universalistic interpretation.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, spiritual_seekers, beneficiary,
    moderate, biographical, mobile, global).

% Experience this reading as a challenge to their traditional understanding of Vedic authority and social order. While not directly 'victims' of this specific constraint, their worldview is undermined, leading to ideological friction.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, orthodox_varna_adherents, payer,
    organized, generational, constrained, national).

% Historically sought to codify 'Hindu law' based on a literal, prescriptive reading of texts, often conflating Vedic and Dharmashastra traditions. This reformist reading directly contradicts their administrative project.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, colonial_administrators, excluded,
    institutional, generational, analytical, regional).

% Analyze the historical development of Vedic interpretation, the textual basis for various readings, and the social impact of different hermeneutical approaches. They do not directly participate in the constraint's operation but study its dynamics.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, academic_scholars_of_religion, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates spiritual practice and community around universalistic principles derived from Vedic texts, fostering inclusivity and shared identity among diverse adherents.
% TRANSFER_FUNCTION: Transfers interpretive authority from traditional, literalist readings to a more metaphorical and spiritual understanding, shifting the focus from social hierarchy to individual spiritual realization.
% ABSENT_VOICES: Strict literalists of Dharmashastra texts, who would argue for the divine mandate of social hierarchy, are excluded from the interpretive framework of this reading, which prioritizes Vedic spiritual content over later prescriptive codes.
% DISAPPEARANCE_RATIONALE: If this reformist reading vanished, the landscape of Hindu thought would revert to more literal and socially prescriptive interpretations, potentially re-entrenching caste-based justifications and diminishing universalistic spiritual movements. Many individuals would lose a framework for inclusive spiritual practice.
% FOUNDING_PROBLEM: The problem of reconciling the spiritual, universalistic message of the Vedas with later, more socially prescriptive texts and practices, particularly the caste system, which caused internal conflict and external criticism of Hinduism.
% FOUNDING_PROBLEM_CORROBORATION: Scholars of religion and social reformers, outside of the immediate reformist movements, corroborate the ongoing tension between Vedic spiritual ideals and historical social stratification, confirming the problem's continued relevance.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__reformist_spiritual_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__reformist_spiritual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__reformist_spiritual_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vedic_corpus_social_prescription__reformist_spiritual_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_corpus_social_prescription__reformist_spiritual_reading_tests).
:- end_tests(vedic_corpus_social_prescription__reformist_spiritual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.1) because this reading primarily offers a framework for spiritual understanding and community, rather than imposing costs or extracting resources. Suppression is minimal (0.05) as it relies on persuasion and voluntary adoption, not coercion. Theater ratio is zero, as its function is direct and not performative. The accessibility collapse is high (0.88) because once this interpretive framework is adopted, alternatives that posit social prescription are largely collapsed within that worldview. Resistance is low (0.02) from within its own community, though it faces ideological resistance from other readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist Hindu movements act as agenda-setters, actively shaping and disseminating this interpretation. Spiritual seekers are beneficiaries, finding an inclusive path. Adherents of orthodox Varna readings are payers, as their traditional framework is challenged, though this is an ideological cost, not a direct extraction by this constraint. Colonial administrators are excluded, as their project of codifying 'Hindu law' is fundamentally at odds with this reading's non-prescriptive stance.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a genuine coordination mechanism (spiritual community building) as extraction. The low extractiveness and suppression, coupled with the clear beneficiary structure, confirm its Rope-like nature. The 'founding problem' of reconciling spiritual universalism with social stratification remains 'live,' indicating the constraint continues to address its original mandate, preventing mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_basis_ambiguity,
    'To what extent can the reformist spiritual reading be solely grounded in the Vedic corpus itself, versus being influenced by later philosophical developments or modern ethical sensibilities?',
    'Detailed philological and historical analysis of early Vedic commentaries and their evolution, tracing the emergence of non-prescriptive interpretations.',
    'If heavily reliant on later influences, the claim of ''no prescriptive social content'' might be seen as a modern reinterpretation rather than an inherent feature of the original texts, potentially weakening its authority against more literal readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_basis_ambiguity, empirical, 'Ambiguity regarding the textual grounding of the reformist reading.').

omega_variable(
    kernel_reading_divergence,
    'Is this constraint a genuine ''reading'' of the Vedic corpus, or does it represent a distinct philosophical system that selectively draws from the Vedas while rejecting other parts?',
    'Comparative analysis of the hermeneutical principles employed by the reformist reading versus those of traditional Vedic exegesis, identifying points of continuity and rupture.',
    'If it''s a distinct system, its claim to be a ''reading'' of the Vedic kernel might be challenged, potentially reclassifying it as a new ''rope'' that merely references the Vedas, rather than being an interpretation of them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Whether this reading is an interpretation of the kernel or a new system.').

omega_variable(
    social_impact_of_rejection,
    'What is the actual social impact of this reading''s rejection of Varna hierarchy on communities that traditionally adhered to it?',
    'Sociological studies and ethnographic research on communities influenced by reformist movements, assessing changes in social mobility, inter-caste relations, and individual agency.',
    'If the rejection leads to significant positive social change, it strengthens the ''beneficiary'' aspect and the ''rope'' classification. If it creates new forms of exclusion or conflict, it might reveal hidden ''payer'' dynamics not captured by the current metrics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(social_impact_of_rejection, empirical, 'Actual social impact of rejecting Varna hierarchy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__reformist_spiritual_reading, 1800, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t1800, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1800, 0.0).
narrative_ontology:measurement(vedi_tr_t1900, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1900, 0.0).
narrative_ontology:measurement(vedi_tr_t2024, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 2024, 0.0).

% Extraction over time
narrative_ontology:measurement(vedi_be_t1800, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1800, 0.1).
narrative_ontology:measurement(vedi_be_t1900, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1900, 0.1).
narrative_ontology:measurement(vedi_be_t2024, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 2024, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t1800, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 1800, 0.05).
narrative_ontology:measurement(vedi_su_t1900, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 1900, 0.05).
narrative_ontology:measurement(vedi_su_t2024, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 2024, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__reformist_spiritual_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription__orthodox_varna_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription__colonial_orientalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'vedic_corpus_social_prescription' kernel. This 'reformist_spiritual_reading' emphasizes spiritual unity and rejects social prescription, contrasting with the 'orthodox_varna_reading' (literal social hierarchy) and the 'colonial_orientalist_reading' (codification of 'Hindu law').

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
