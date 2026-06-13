% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__universalist_devotional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__universalist_devotional_reading, []).

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
 *   constraint_id: gita_kurukshetra_discourse__universalist_devotional_reading
 *   human_readable: Bhagavad Gita: Universalist Devotional Reading
 *   domain: religious_studies/textual_hermeneutics/ethical_philosophy
 *
 * SUMMARY:
 *   This constraint represents a 'universalist devotional' reading of the
 *   Bhagavad Gita's Kurukshetra discourse. It emphasizes that devotion
 *   (bhakti) is a path to spiritual liberation accessible to all,
 *   irrespective of caste, and redefines dharma as surrender to divine will
 *   rather than adherence to prescribed social roles. This reading
 *   de-emphasizes the literal interpretation of violence in the text, viewing
 *   it as either allegorical or secondary to the core message of devotion. It
 *   structurally undermines traditional Brahminical gatekeeping authority and
 *   promotes egalitarian access to spiritual practice.
 *
 * KEY AGENTS:
 *   - universal_devotee_class: Primary beneficiary (powerless/mobile) — gains spiritual access and agency
 *   - marginalized_communities: Primary beneficiary (powerless/constrained) — gains spiritual inclusion and challenges social barriers
 *   - traditional_brahmins: Payer (institutional/constrained) — loses gatekeeping authority and social status
 *   - orthodox_scholars: Payer (organized/constrained) — resists reinterpretation that challenges literal readings
 *   - gita_text: Agenda setter (institutional/civilizational) — the kernel from which readings are derived
 *   - analytical_theologians: Observer (analytical/analytical) — studies the hermeneutical shifts and their social impact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__universalist_devotional_reading, 0.2).
domain_priors:suppression_score(gita_kurukshetra_discourse__universalist_devotional_reading, 0.3).
domain_priors:theater_ratio(gita_kurukshetra_discourse__universalist_devotional_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__universalist_devotional_reading, rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__universalist_devotional_reading, "Bhagavad Gita: Universalist Devotional Reading").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__universalist_devotional_reading, "religious_studies/textual_hermeneutics/ethical_philosophy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__universalist_devotional_reading, 'f4d7df2f-f447-4c2e-87bd-f5de5c364c31').
narrative_ontology:cs_kernel_codification('f4d7df2f-f447-4c2e-87bd-f5de5c364c31', fixed_text).
narrative_ontology:cs_authority_grounding('f4d7df2f-f447-4c2e-87bd-f5de5c364c31', lineage).
narrative_ontology:cs_interpretation_layer_present('f4d7df2f-f447-4c2e-87bd-f5de5c364c31').
narrative_ontology:cs_reading_relation('f4d7df2f-f447-4c2e-87bd-f5de5c364c31', gita_kurukshetra_discourse__orthodox_literal_reading, coexists_with).
narrative_ontology:cs_reading_relation('f4d7df2f-f447-4c2e-87bd-f5de5c364c31', gita_kurukshetra_discourse__gandhian_allegorical_reading, coexists_with).
narrative_ontology:cs_axiom('f4d7df2f-f447-4c2e-87bd-f5de5c364c31', foundational, devotion_transcends_caste).
narrative_ontology:cs_axiom_status(devotion_transcends_caste, holdable).
narrative_ontology:cs_axiom_grounding('f4d7df2f-f447-4c2e-87bd-f5de5c364c31', devotion_transcends_caste, deontological).
narrative_ontology:cs_axiom('f4d7df2f-f447-4c2e-87bd-f5de5c364c31', foundational, dharma_is_divine_surrender).
narrative_ontology:cs_axiom_status(dharma_is_divine_surrender, holdable).
narrative_ontology:cs_axiom_grounding('f4d7df2f-f447-4c2e-87bd-f5de5c364c31', dharma_is_divine_surrender, theological).
narrative_ontology:cs_reference_frame('f4d7df2f-f447-4c2e-87bd-f5de5c364c31', egalitarian_bhakti_tradition).
narrative_ontology:cs_drift_state('f4d7df2f-f447-4c2e-87bd-f5de5c364c31', contemporary_global_hinduism, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f4d7df2f-f447-4c2e-87bd-f5de5c364c31', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, universal_devotee_class).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, marginalized_communities).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gita_kurukshetra_discourse__universalist_devotional_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__universalist_devotional_reading_tests).
:- end_tests(gita_kurukshetra_discourse__universalist_devotional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.2) because this reading primarily offers spiritual liberation and inclusion, rather than imposing significant costs. Suppression is moderate (0.3) as it challenges existing social hierarchies, but its 'enforcement' is primarily through persuasion and spiritual movement, not coercion. Theater ratio is low (0.1) as its core function of providing an accessible spiritual path is genuine. Accessibility collapse is moderate (0.7) because while it opens a path, it still requires commitment and understanding of complex spiritual concepts. Resistance is moderate (0.4) due to opposition from traditional authorities whose power it diminishes.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the universal devotee class and marginalized communities, this reading is a liberating 'rope' that offers spiritual access and agency. For traditional Brahmins and orthodox scholars, it is a 'snare' or 'tangled_rope' that undermines their established authority and social order, extracting their traditional power and influence. The engine's per-seat classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The universal devotee class and marginalized communities are clear beneficiaries (d near 0.0) as they gain spiritual access and challenge social barriers. Traditional Brahmins and orthodox scholars are targets (d near 1.0) as their gatekeeping authority and social status are undermined. The Gita text itself, as the kernel, is the agenda setter, but its 'directionality' is mediated by interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling a genuine coordination function (universal spiritual access) as pure extraction. While it extracts from traditional authorities, its primary function is to coordinate spiritual practice on an egalitarian basis. The 'mandate' is to provide a path to devotion, which remains live. The 'mandatrophy' would occur if the universalist claims became purely performative while actual social barriers persisted, turning it into a 'piton' or 'snare' for those it claims to liberate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a genuine universalist teaching, or an interpretation that selectively emphasizes certain verses to undermine traditional social structures?',
    'Comparative textual analysis across a wider range of Hindu scriptures and historical reception studies of the Gita''s interpretation by different schools of thought.',
    'If a selective interpretation, its ''universalist'' claims might be seen as a strategic re-reading rather than an inherent property of the text, potentially reclassifying it as a ''tangled_rope'' for those whose traditional authority it undermines.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity between inherent textual meaning and interpretive construction.').

omega_variable(
    caste_system_legitimacy,
    'To what extent does this reading genuinely dissolve caste as a spiritual barrier, versus merely reinterpreting its function without challenging its social reality?',
    'Sociological studies of devotional movements inspired by this reading, examining actual changes in social mobility and inter-caste relations among adherents.',
    'If caste barriers persist despite the spiritual claims, the ''universalist'' aspect might be more theoretical than practical, reducing its effective coordination function and increasing its latent extractiveness for marginalized groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(caste_system_legitimacy, empirical, 'Effectiveness of spiritual universalism in challenging social hierarchy.').

omega_variable(
    violence_interpretation_neutrality,
    'Does this reading genuinely render violence neither mandated nor central, or does it merely de-emphasize it to fit modern ethical sensibilities?',
    'Analysis of the historical and contemporary ethical implications drawn by adherents of this reading, particularly in contexts of conflict or social justice movements.',
    'If the de-emphasis is a modern imposition, the reading''s claim to ethical neutrality regarding violence might be seen as a form of ''theater_ratio'' masking a more complex textual reality, potentially affecting its ''claimed_type'' towards a ''tangled_rope'' if it implicitly enables inaction in the face of injustice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(violence_interpretation_neutrality, conceptual, 'Neutrality of the reading regarding violence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__universalist_devotional_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gita_tr_t10, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(gita_tr_t20, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(gita_tr_t30, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(gita_be_t10, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(gita_be_t20, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 20, 0.2).
narrative_ontology:measurement(gita_be_t30, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 30, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t0, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(gita_su_t10, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 10, 0.32).
narrative_ontology:measurement(gita_su_t20, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(gita_su_t30, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 30, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__universalist_devotional_reading, identity_coordination).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse__orthodox_literal_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse__gandhian_allegorical_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Bhagavad Gita's Kurukshetra discourse kernel. Each reading has a different structural impact and classification, linked here as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
