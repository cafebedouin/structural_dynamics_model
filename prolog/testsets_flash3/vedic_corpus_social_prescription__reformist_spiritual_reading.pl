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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: vedic_corpus_social_prescription__reformist_spiritual_reading
 *   human_readable: Vedic Corpus as Non-Prescriptive Spiritual Metaphor (Reformist Reading)
 *   domain: religious_studies/social_stratification/hermeneutics
 *
 * SUMMARY:
 *   This constraint represents the 'reformist spiritual' reading of the Vedic
 *   corpus, which interprets the texts as primarily concerned with spiritual
 *   unity and metaphorical cosmology, devoid of prescriptive social content.
 *   This reading emerged in response to both orthodox literalism and colonial
 *   interpretations, seeking to reclaim the texts for universal spiritual
 *   application. It is characterized by low extractiveness and suppression,
 *   as its function is to coordinate spiritual understanding rather than
 *   enforce social order or extract resources. The claimed type is 'rope'
 *   because it genuinely facilitates coordination among spiritual seekers and
 *   scholars without significant coercive overhead.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.05).
domain_priors:suppression_score(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.02).
domain_priors:theater_ratio(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__reformist_spiritual_reading, rope).
narrative_ontology:human_readable(vedic_corpus_social_prescription__reformist_spiritual_reading, "Vedic Corpus as Non-Prescriptive Spiritual Metaphor (Reformist Reading)").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__reformist_spiritual_reading, "religious_studies/social_stratification/hermeneutics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__reformist_spiritual_reading, '11500649-8a5c-4aec-a690-1f420dcd42e9').
narrative_ontology:cs_kernel_codification('11500649-8a5c-4aec-a690-1f420dcd42e9', fixed_text).
narrative_ontology:cs_authority_grounding('11500649-8a5c-4aec-a690-1f420dcd42e9', expertise).
narrative_ontology:cs_interpretation_layer_present('11500649-8a5c-4aec-a690-1f420dcd42e9').
narrative_ontology:cs_reading_relation('11500649-8a5c-4aec-a690-1f420dcd42e9', vedic_corpus_social_prescription__orthodox_varna_reading, coexists_with).
narrative_ontology:cs_reading_relation('11500649-8a5c-4aec-a690-1f420dcd42e9', vedic_corpus_social_prescription__colonial_orientalist_reading, forecloses).
narrative_ontology:cs_axiom('11500649-8a5c-4aec-a690-1f420dcd42e9', foundational, vedic_texts_are_primarily_spiritual).
narrative_ontology:cs_axiom_status(vedic_texts_are_primarily_spiritual, holdable).
narrative_ontology:cs_axiom_grounding('11500649-8a5c-4aec-a690-1f420dcd42e9', vedic_texts_are_primarily_spiritual, deontological).
narrative_ontology:cs_axiom('11500649-8a5c-4aec-a690-1f420dcd42e9', secondary, social_prescriptions_are_later_interpolations).
narrative_ontology:cs_axiom_status(social_prescriptions_are_later_interpolations, holdable).
narrative_ontology:cs_axiom_grounding('11500649-8a5c-4aec-a690-1f420dcd42e9', social_prescriptions_are_later_interpolations, empirically_contingent).
narrative_ontology:cs_reference_frame('11500649-8a5c-4aec-a690-1f420dcd42e9', universal_spiritual_truth).
narrative_ontology:cs_drift_state('11500649-8a5c-4aec-a690-1f420dcd42e9', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('11500649-8a5c-4aec-a690-1f420dcd42e9', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, spiritual_seekers).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, reformist_scholars).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__reformist_spiritual_reading, universal_spiritual_unity).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__reformist_spiritual_reading, metaphorical_interpretation_of_scripture).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Find universal spiritual truths and guidance for personal liberation in the Vedic texts, unburdened by social hierarchy or ritualistic demands. They benefit from a framework that emphasizes individual spiritual experience over prescribed social roles.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, spiritual_seekers, beneficiary,
    moderate, biographical, mobile, global).

% Interpret and promote the Vedic texts as primarily spiritual and metaphorical, actively challenging literal or prescriptive readings, especially concerning social stratification. They shape the discourse and educational materials for this interpretation.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, reformist_scholars, agenda_setter,
    organized, generational, mobile, global).

% Adhere to literal interpretations of Vedic texts, including social prescriptions like Varna. They are excluded from the reformist discourse and actively resist its spread, viewing it as a distortion of tradition. Their interpretive framework is directly challenged by this reading.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, orthodox_traditionalists, excluded,
    organized, generational, constrained, national).

% Historically sought to codify 'Hindu law' based on prescriptive readings of Vedic and Dharmashastra texts for administrative control. This reading directly refutes their foundational premise of a unified, prescriptive legal corpus.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, colonial_administrators_historical, excluded,
    institutional, generational, analytical, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared framework for spiritual practice and philosophical inquiry, emphasizing individual spiritual growth and universal unity, allowing diverse practitioners to engage with the texts without rigid social or ritualistic constraints.
% TRANSFER_FUNCTION: Facilitates the transfer of spiritual knowledge and philosophical insights, from ancient texts to contemporary seekers, without imposing social obligations or extracting material resources through a hierarchical structure.
% ABSENT_VOICES: Orthodox traditionalists and historical colonial administrators are absent from this reading's interpretive community; they would argue for literal, prescriptive, or legally codifiable interpretations of the Vedic corpus, which this reading explicitly rejects.
% DISAPPEARANCE_RATIONALE: If this reformist reading vanished, the landscape of Vedic interpretation would revert to more literal, prescriptive, or colonial-influenced understandings, potentially re-entrenching social hierarchies and limiting spiritual accessibility for many. The interpretive community built around this reading would dissolve.
% FOUNDING_PROBLEM: The problem of reconciling ancient texts with modern ethical sensibilities, particularly regarding social hierarchy, and making spiritual wisdom accessible beyond traditional, often exclusive, interpretive communities.
% FOUNDING_PROBLEM_CORROBORATION: Many contemporary spiritual movements and interfaith dialogues attest to the ongoing need for non-prescriptive, universalist interpretations of ancient texts. Scholars of comparative religion and social reformers outside the immediate beneficiary group corroborate the historical and ongoing challenge of textual interpretation in diverse social contexts.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__reformist_spiritual_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__reformist_spiritual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__reformist_spiritual_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vedic_corpus_social_prescription__reformist_spiritual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is very low (0.05) because this reading does not impose material costs or social obligations; it offers spiritual insights. Suppression is minimal (0.02) as adherence is voluntary, and alternatives (other spiritual paths or interpretations) are not suppressed. Theater ratio is negligible (0.01) because the reading's function is direct and transparent: to provide a spiritual framework. Accessibility collapse is high (0.88) because once this interpretive lens is adopted, the idea of a socially prescriptive Vedic text largely collapses. Resistance is low (0.01) from within this interpretive community, though it faces external resistance from other readings.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of spiritual seekers and reformist scholars, this reading is a pure rope, facilitating spiritual coordination. From the perspective of orthodox traditionalists, this reading is a distortion or misinterpretation that undermines traditional social order, effectively 'excluding' their worldview. The engine's classification will reflect the low extractiveness and coordination function from the perspective of its beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   Spiritual seekers and reformist scholars are beneficiaries, gaining access to a non-hierarchical spiritual path and a framework for textual interpretation. There are no direct 'victims' in this reading, as it actively seeks to dismantle extractive interpretations. Orthodox traditionalists and colonial administrators are 'excluded' as their interpretive frameworks are directly challenged and rejected by this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading actively resolves mandatrophy by reinterpreting the Vedic corpus away from historically extractive or socially rigid applications. It asserts that the original 'mandate' was spiritual, not social, thus preventing mislabeling genuine spiritual coordination as extraction. The constraint's persistence is tied to the ongoing demand for universal spiritual frameworks.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_ambiguity_of_varna,
    'Is the concept of Varna in Vedic texts inherently social and prescriptive, or is it primarily symbolic/metaphorical?',
    'Linguistic and historical analysis of early Vedic usage, distinguishing between later Dharmashastra interpretations and original Vedic context. Comparative study of other ancient texts with similar concepts.',
    'If Varna is found to be inherently social and prescriptive in the earliest Vedic layers, this reading''s claim of non-prescriptiveness would be weakened, potentially increasing its perceived suppression of alternative interpretations. If confirmed as symbolic, this reading''s rope classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_ambiguity_of_varna, empirical, 'Ambiguity regarding the original intent and meaning of Varna in Vedic texts.').

omega_variable(
    interpretive_community_power_dynamics,
    'To what extent does the ''reformist spiritual'' reading gain prominence by suppressing or marginalizing traditional interpretive communities, rather than purely by intellectual merit?',
    'Sociological study of interpretive communities, analyzing resource allocation, academic appointments, and media representation for different readings. Historical analysis of power shifts in religious scholarship.',
    'If significant suppression or marginalization of traditionalists is found, the ''suppression'' metric for this reading might need upward adjustment, and its ''rope'' classification could be challenged as a ''tangled_rope'' if it actively extracts from other interpretive communities.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpretive_community_power_dynamics, empirical, 'Power dynamics within the broader interpretive landscape of Vedic texts.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the ''Vedic corpus social prescription'' the only defensible framing of this kernel, or could an alternative framing (e.g., ''Vedic texts as source of ritual authority'') produce a different CS pattern classification?',
    'Construct an alternative constraint story for ''Vedic texts as source of ritual authority'' and compare its cs_pattern classification. Analyze which signals or context guided the choice of the current framing.',
    'If an alternative framing yields a different cs_pattern, it indicates a conceptual under-determination in the kernel definition itself, requiring further meta-analysis of the corpus''s framing choices. The current classification is valid under its chosen framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Under-determination in the choice of kernel framing for the Vedic texts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__reformist_spiritual_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t1900, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1900, 0.01).
narrative_ontology:measurement(vedi_tr_t1925, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1925, 0.01).
narrative_ontology:measurement(vedi_tr_t1950, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1950, 0.01).
narrative_ontology:measurement(vedi_tr_t1975, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1975, 0.01).
narrative_ontology:measurement(vedi_tr_t2000, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 2000, 0.01).
narrative_ontology:measurement(vedi_tr_t2024, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 2024, 0.01).

% Extraction over time
narrative_ontology:measurement(vedi_be_t1900, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1900, 0.05).
narrative_ontology:measurement(vedi_be_t1925, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1925, 0.05).
narrative_ontology:measurement(vedi_be_t1950, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1950, 0.05).
narrative_ontology:measurement(vedi_be_t1975, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1975, 0.05).
narrative_ontology:measurement(vedi_be_t2000, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 2000, 0.05).
narrative_ontology:measurement(vedi_be_t2024, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t1900, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 1900, 0.02).
narrative_ontology:measurement(vedi_su_t1925, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 1925, 0.02).
narrative_ontology:measurement(vedi_su_t1950, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 1950, 0.02).
narrative_ontology:measurement(vedi_su_t1975, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 1975, 0.02).
narrative_ontology:measurement(vedi_su_t2000, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 2000, 0.02).
narrative_ontology:measurement(vedi_su_t2024, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 2024, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__reformist_spiritual_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
