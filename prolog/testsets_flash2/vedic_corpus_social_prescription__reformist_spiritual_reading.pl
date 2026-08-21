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
 *   human_readable: Vedic Corpus as Spiritual Unity (Reformist Reading)
 *   domain: religious_studies/social_stratification/hermeneutics
 *
 * SUMMARY:
 *   This constraint represents a reformist spiritual reading of the Vedic
 *   corpus, which interprets the texts as primarily concerned with spiritual
 *   unity and metaphorical cosmology, devoid of prescriptive social content
 *   like the Varna system. This reading emerged as a response to both
 *   orthodox interpretations that upheld social hierarchies and colonial
 *   interpretations that sought to codify a rigid 'Hindu law.' It functions
 *   as a low-extraction 'rope' by coordinating spiritual understanding and
 *   practice without imposing coercive social structures. The metrics reflect
 *   its non-extractive nature and minimal enforcement requirements.
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
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__reformist_spiritual_reading, rope).
narrative_ontology:human_readable(vedic_corpus_social_prescription__reformist_spiritual_reading, "Vedic Corpus as Spiritual Unity (Reformist Reading)").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__reformist_spiritual_reading, "religious_studies/social_stratification/hermeneutics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__reformist_spiritual_reading, '90f03aca-0dd2-4495-ad63-ebbb7af47486').
narrative_ontology:cs_kernel_codification('90f03aca-0dd2-4495-ad63-ebbb7af47486', fixed_text).
narrative_ontology:cs_authority_grounding('90f03aca-0dd2-4495-ad63-ebbb7af47486', expertise).
narrative_ontology:cs_interpretation_layer_present('90f03aca-0dd2-4495-ad63-ebbb7af47486').
narrative_ontology:cs_reading_relation('90f03aca-0dd2-4495-ad63-ebbb7af47486', vedic_corpus_social_prescription__orthodox_varna_reading, coexists_with).
narrative_ontology:cs_reading_relation('90f03aca-0dd2-4495-ad63-ebbb7af47486', vedic_corpus_social_prescription__colonial_orientalist_reading, coexists_with).
narrative_ontology:cs_axiom('90f03aca-0dd2-4495-ad63-ebbb7af47486', foundational, vedic_texts_are_primarily_spiritual_and_metaphorical).
narrative_ontology:cs_axiom_status(vedic_texts_are_primarily_spiritual_and_metaphorical, holdable).
narrative_ontology:cs_axiom_grounding('90f03aca-0dd2-4495-ad63-ebbb7af47486', vedic_texts_are_primarily_spiritual_and_metaphorical, deontological).
narrative_ontology:cs_axiom('90f03aca-0dd2-4495-ad63-ebbb7af47486', foundational, social_hierarchy_is_a_later_corruption_not_vedic_mandate).
narrative_ontology:cs_axiom_status(social_hierarchy_is_a_later_corruption_not_vedic_mandate, holdable).
narrative_ontology:cs_axiom_grounding('90f03aca-0dd2-4495-ad63-ebbb7af47486', social_hierarchy_is_a_later_corruption_not_vedic_mandate, empirically_contingent).
narrative_ontology:cs_reference_frame('90f03aca-0dd2-4495-ad63-ebbb7af47486', universal_spiritual_equality_framework).
narrative_ontology:cs_drift_state('90f03aca-0dd2-4495-ad63-ebbb7af47486', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('90f03aca-0dd2-4495-ad63-ebbb7af47486', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, spiritual_seekers).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, reformist_hindu_movements).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__reformist_spiritual_reading, universal_spiritual_equality).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__reformist_spiritual_reading, metaphorical_interpretation_of_scripture).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Find universal spiritual truths and practices in the Vedic texts, unburdened by social hierarchy. They benefit from a framework that promotes individual spiritual growth and equality, allowing them to engage with the texts without endorsing social stratification.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, spiritual_seekers, beneficiary,
    moderate, biographical, mobile, global).

% Actively promote this interpretation, seeking to reform Hindu society by rejecting caste-based discrimination and emphasizing the egalitarian spiritual core of the Vedas. They administer educational programs and community initiatives based on this reading.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, reformist_hindu_movements, agenda_setter,
    organized, generational, constrained, national).

% Adhere to a literal interpretation of Vedic texts, including prescriptive social content like Varna. They are excluded from the interpretive framework of this reading, which challenges their traditional authority and social structures.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, orthodox_brahminical_traditions, excluded,
    institutional, generational, identity_locked, regional).

% Historically sought to codify 'Hindu law' based on a literal, often rigid, interpretation of texts including Dharmashastras, often conflating them with the Vedas. Their administrative and legalistic approach is antithetical to this spiritual reading.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, colonial_administrators, excluded,
    institutional, generational, analytical, global).

% Analyze the historical development of Vedic interpretation, including the emergence of reformist readings. They assess the textual basis and social impact of various hermeneutical approaches without necessarily endorsing one.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, academic_indologists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared understanding of Vedic texts as a source of universal spiritual wisdom and ethical guidance, fostering inclusive spiritual communities and practices that transcend social hierarchies.
% TRANSFER_FUNCTION: Facilitates the transfer of spiritual knowledge and practices to a broader, more inclusive audience, shifting interpretive authority from hereditary priestly classes to individual spiritual experience and scholarly inquiry.
% ABSENT_VOICES: Traditional orthodox interpreters who insist on the literal, prescriptive nature of Vedic texts regarding social hierarchy are absent from this reading's interpretive community; they would argue that this reading distorts the original intent and divine mandate of the scriptures.
% DISAPPEARANCE_RATIONALE: If this reformist reading vanished, the landscape of Hindu thought and practice would significantly rearrange. Many modern spiritual movements would lose their textual grounding for egalitarianism, potentially leading to a resurgence of more hierarchical interpretations and a loss of inclusive spiritual pathways for many seekers.
% FOUNDING_PROBLEM: The problem of reconciling ancient Vedic texts with modern ethical sensibilities, particularly regarding social equality, and making spiritual wisdom accessible beyond traditional, often exclusive, social structures.
% FOUNDING_PROBLEM_CORROBORATION: Scholars of religious studies and social historians corroborate the ongoing tension between traditional and reformist interpretations, noting the persistent social inequalities that this reading seeks to address. Independent sociological studies of religious movements also attest to the live nature of this problem.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__reformist_spiritual_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__reformist_spiritual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__reformist_spiritual_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is very low (0.05) because this reading primarily offers spiritual guidance and a framework for understanding the cosmos, rather than extracting resources or labor. Suppression is minimal (0.02) as adherence is voluntary, driven by spiritual affinity rather than coercion. Theater ratio is negligible (0.01) as its function is direct and transparent. Accessibility collapse is high (0.9) because once this spiritual interpretation is adopted, the idea of socially prescriptive Vedas largely collapses as a viable alternative for adherents. Resistance is low (0.01) from within its own interpretive community, though it faces significant external resistance from orthodox readings.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of spiritual seekers and reformist movements, this reading is a liberating coordination mechanism. From the perspective of orthodox traditions, it is a distortion or even an attack on sacred texts. The engine's classification as a 'rope' reflects the internal coherence and low extraction of this specific reading, not the broader contest over the Vedic corpus.
 *
 * DIRECTIONALITY LOGIC:
 *   Spiritual seekers are direct beneficiaries, finding an inclusive path. Reformist Hindu movements act as agenda-setters and beneficiaries, actively promoting and benefiting from this egalitarian interpretation. Orthodox Brahminical traditions and colonial administrators are structurally excluded, as their interpretations are directly challenged by this reading's core tenets.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_authority_ambiguity,
    'Is the authority for this spiritual reading derived from textual scholarship, individual spiritual experience, or a reinterpretation of traditional lineage?',
    'Analysis of the hermeneutical methods employed by reformist movements and their stated sources of legitimacy. If primarily individual experience, it''s more diffuse; if scholarly, it''s more expertise-driven.',
    'If authority is primarily individual or diffuse, the constraint is more resilient to external challenges but harder to standardize. If scholarly, it''s more susceptible to academic critique but can gain wider acceptance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_ambiguity, conceptual, 'The source of interpretive authority for the reformist reading.').

omega_variable(
    social_impact_measurement,
    'To what extent does this reading actually reduce social stratification and discrimination in practice, beyond its theoretical claims?',
    'Sociological studies measuring changes in caste-based discrimination, inter-caste marriage rates, and access to religious institutions among adherents of this reading.',
    'If the practical impact is minimal, the reading''s ''rope'' classification might be challenged by a ''theater'' component, suggesting its egalitarian claims are more performative than effective in social change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(social_impact_measurement, empirical, 'Empirical impact of the reformist reading on social equality.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the Vedic corpus best framed as a single kernel with competing readings, or as multiple distinct textual traditions (Vedas, Dharmashastras, Puranas) that have been conflated?',
    'Historical-philological analysis tracing the independent development and later integration/conflation of these textual traditions, and the social processes that led to their unified ''corpus'' framing.',
    'If multiple distinct traditions, then the ''vedic_corpus_social_prescription'' kernel itself is a constructed constraint, and each ''reading'' would be a constraint on a different, more specific kernel. This would decompose the current kernel into a network of more granular constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the ''Vedic corpus'' is a natural kernel or a constructed conflation of distinct textual traditions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__reformist_spiritual_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t1900, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1900, 0.01).
narrative_ontology:measurement(vedi_tr_t1930, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1930, 0.01).
narrative_ontology:measurement(vedi_tr_t1960, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1960, 0.01).
narrative_ontology:measurement(vedi_tr_t1990, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1990, 0.01).
narrative_ontology:measurement(vedi_tr_t2024, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 2024, 0.01).

% Extraction over time
narrative_ontology:measurement(vedi_be_t1900, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1900, 0.05).
narrative_ontology:measurement(vedi_be_t1930, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1930, 0.05).
narrative_ontology:measurement(vedi_be_t1960, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1960, 0.05).
narrative_ontology:measurement(vedi_be_t1990, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1990, 0.05).
narrative_ontology:measurement(vedi_be_t2024, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t1900, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 1900, 0.02).
narrative_ontology:measurement(vedi_su_t1930, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 1930, 0.02).
narrative_ontology:measurement(vedi_su_t1960, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 1960, 0.02).
narrative_ontology:measurement(vedi_su_t1990, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 1990, 0.02).
narrative_ontology:measurement(vedi_su_t2024, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 2024, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__reformist_spiritual_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'vedic_corpus_social_prescription' kernel. Its low extractiveness and focus on spiritual unity stand in contrast to the 'orthodox_varna_reading' (which asserts social hierarchy) and the 'colonial_orientalist_reading' (which sought to codify a rigid 'Hindu law').

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
