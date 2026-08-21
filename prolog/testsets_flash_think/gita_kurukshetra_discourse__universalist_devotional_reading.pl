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
 *   constraint_id: gita_kurukshetra_discourse__universalist_devotional_reading
 *   human_readable: Bhagavad Gita: Universalist Devotional Reading
 *   domain: religious_studies/textual_hermeneutics/ethical_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'universalist devotional reading' of the
 *   Bhagavad Gita, which interprets the text as teaching a path of devotion
 *   (bhakti) accessible to all, regardless of caste or social status. It
 *   redefines dharma as surrender to divine will rather than adherence to
 *   prescribed social roles, thereby challenging traditional hierarchical
 *   interpretations. This reading is one of several competing interpretations
 *   of the Gita's core message.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__universalist_devotional_reading, 0.05).
domain_priors:suppression_score(gita_kurukshetra_discourse__universalist_devotional_reading, 0.05).
domain_priors:theater_ratio(gita_kurukshetra_discourse__universalist_devotional_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__universalist_devotional_reading, rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__universalist_devotional_reading, "Bhagavad Gita: Universalist Devotional Reading").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__universalist_devotional_reading, "religious_studies/textual_hermeneutics/ethical_philosophy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__universalist_devotional_reading, '7afc02aa-49b3-4a43-b4fd-cdf68befc9f1').
narrative_ontology:cs_kernel_codification('7afc02aa-49b3-4a43-b4fd-cdf68befc9f1', fixed_text).
narrative_ontology:cs_authority_grounding('7afc02aa-49b3-4a43-b4fd-cdf68befc9f1', lineage).
narrative_ontology:cs_interpretation_layer_present('7afc02aa-49b3-4a43-b4fd-cdf68befc9f1').
narrative_ontology:cs_reading_relation('7afc02aa-49b3-4a43-b4fd-cdf68befc9f1', gita_kurukshetra_discourse__orthodox_literal_reading, forecloses).
narrative_ontology:cs_reading_relation('7afc02aa-49b3-4a43-b4fd-cdf68befc9f1', gita_kurukshetra_discourse__gandhian_allegorical_reading, coexists_with).
narrative_ontology:cs_axiom('7afc02aa-49b3-4a43-b4fd-cdf68befc9f1', foundational, devotion_transcends_caste).
narrative_ontology:cs_axiom_status(devotion_transcends_caste, holdable).
narrative_ontology:cs_axiom_grounding('7afc02aa-49b3-4a43-b4fd-cdf68befc9f1', devotion_transcends_caste, deontological).
narrative_ontology:cs_axiom('7afc02aa-49b3-4a43-b4fd-cdf68befc9f1', foundational, dharma_is_divine_surrender).
narrative_ontology:cs_axiom_status(dharma_is_divine_surrender, holdable).
narrative_ontology:cs_axiom_grounding('7afc02aa-49b3-4a43-b4fd-cdf68befc9f1', dharma_is_divine_surrender, deontological).
narrative_ontology:cs_reference_frame('7afc02aa-49b3-4a43-b4fd-cdf68befc9f1', universal_bhakti_path).
narrative_ontology:cs_drift_state('7afc02aa-49b3-4a43-b4fd-cdf68befc9f1', contemporary_religious_discourse, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('7afc02aa-49b3-4a43-b4fd-cdf68befc9f1', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, universal_devotees).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, marginalized_castes).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__universalist_devotional_reading, orthodox_brahmins).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__universalist_devotional_reading, caste_hierarchy_beneficiaries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals from all social strata who find spiritual liberation and purpose through the path of devotion (bhakti) as taught by this reading, unhindered by caste or ritualistic barriers. They benefit from direct access to spiritual practice.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, universal_devotees, beneficiary,
    powerless, biographical, mobile, global).

% Groups historically excluded from traditional ritual and spiritual authority due to their birth. This reading offers them a path to spiritual equality and dignity, challenging the social structures that constrain them.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, marginalized_castes, beneficiary,
    powerless, generational, constrained, local).

% Traditional priestly class whose authority and social status are historically tied to caste-based ritual and scriptural interpretation. This reading undermines their exclusive spiritual gatekeeping role, leading to a loss of influence and legitimacy.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, orthodox_brahmins, payer,
    institutional, generational, identity_locked, national).

% Social groups and institutions that benefit from the maintenance of a rigid caste system, which this reading directly challenges. They bear the cost of its erosion of social order and traditional power structures.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, caste_hierarchy_beneficiaries, payer,
    organized, generational, constrained, national).

% Academics and philosophers who analyze the Gita, often emphasizing its ethical and allegorical dimensions. They observe the impact of this reading on social reform and spiritual movements, but do not directly participate in its devotional practice.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, gandhian_scholars, observer,
    analytical, civilizational, analytical, universal).

% Scholars and religious leaders who uphold traditional, literal interpretations of the Gita, often emphasizing caste-based dharma and the legitimacy of righteous violence. They actively resist the universalist devotional reading.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, orthodox_scholars, agenda_setter,
    institutional, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal, accessible path to spiritual fulfillment through devotion (bhakti), coordinating individual spiritual practice and ethical conduct outside of rigid social hierarchies and ritualistic exclusivity.
% TRANSFER_FUNCTION: Transfers spiritual authority and access from birth and social status to individual devotion and surrender to divine will; transfers legitimacy from prescribed social roles to an inner, divinely-guided dharma.
% ABSENT_VOICES: Those who benefit from strict caste hierarchy, exclusive ritual access, and the social power derived from traditional interpretations are structurally excluded from the interpretive community that champions this reading. They would object to the dissolution of their inherited privileges.
% DISAPPEARANCE_RATIONALE: If this universalist devotional reading vanished, the spiritual landscape would likely revert to more hierarchical, ritualistic, and caste-bound interpretations. This would reinforce traditional social structures, limit spiritual access for many, and diminish the ethical imperative for social equality derived from the Gita.
% FOUNDING_PROBLEM: The problem of spiritual access being limited by birth and social status, leading to exclusion, a rigid, ritualistic understanding of dharma, and a justification for social inequality.
% FOUNDING_PROBLEM_CORROBORATION: Devotional movements (Bhakti traditions) throughout Indian history, social reform movements, and contemporary scholars of religion attest to the ongoing struggle against caste-based discrimination and the enduring appeal and social impact of universalist spiritual paths. This corroboration comes from historical movements and independent academic analysis, not solely from the reading's direct beneficiaries.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__universalist_devotional_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__universalist_devotional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(gita_kurukshetra_discourse__universalist_devotional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

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
 *   The reading itself, as a spiritual teaching, is characterized by very low extractiveness (0.05) and suppression (0.05) because its core message is one of liberation and universal access, not coercion or rent-seeking. Its 'operation' is the dissemination and adoption of its interpretive framework. However, it faces high resistance (0.70) because it directly challenges deeply entrenched social and religious hierarchies. Accessibility collapse (0.60) reflects its impact on the exclusivity of traditional paths, effectively 'collapsing' the barriers to spiritual access for many. The low theater ratio (0.10) indicates it is a genuine spiritual and ethical teaching, not a performance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of universal devotees, this reading is a liberating Rope, offering an accessible path to spiritual fulfillment. From the perspective of orthodox Brahmins and those who benefit from the caste hierarchy, this same reading operates as a Snare or Tangled Rope, actively dismantling their established authority and social order. The engine will compute these divergent classifications based on the structural data provided.
 *
 * DIRECTIONALITY LOGIC:
 *   Universal devotees and marginalized castes are clear beneficiaries, gaining spiritual access and dignity. Orthodox Brahmins and beneficiaries of the caste hierarchy are 'payers' in this context, as this reading directly undermines their traditional authority, social status, and the legitimacy of the system from which they benefit. The reading's 'cost' to them is the erosion of their exclusive claims. The directionality for beneficiaries is low (subsidized by the teaching), while for payers it is high (extracted from by the challenge this reading poses to their status quo).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading actively prevents the 'dharma as caste duty' constraint (an older, more extractive interpretation) from persisting as a Piton by continuously challenging its legitimacy and offering a viable, alternative framework for spiritual and ethical life. It keeps the debate alive and prevents the older constraint from becoming inert through unchallenged inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    actual_impact_on_caste,
    'To what extent has this universalist devotional reading actually dissolved caste as a spiritual or social barrier in practice, versus remaining an ideal?',
    'Sociological studies of religious practice and social mobility across different devotional traditions, comparing stated ideals with lived realities.',
    'If the practical impact on caste dissolution is low, the reading''s effective extractiveness (from the perspective of marginalized groups still facing discrimination) might be higher, as the ideal fails to translate into material change. If high, its beneficial coordination function is strongly validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(actual_impact_on_caste, empirical, 'Gap between the reading''s ideal of caste equality and its real-world social impact.').

omega_variable(
    violence_interpretation_consistency,
    'Is the interpretation of the Gita''s message as non-violent or allegorical (as implied by this reading''s focus on devotion over social conflict) consistent with the text''s narrative context and other traditional readings?',
    'Comparative textual analysis across a wide range of historical commentaries and philosophical traditions, assessing the coherence and selectivity of the non-violent interpretation.',
    'If the non-violent interpretation is found to be highly selective or inconsistent, it could weaken the reading''s overall coherence and its ability to fully ''foreclose'' the orthodox literal reading, potentially shifting its relation to ''coexists_with'' or ''influences''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(violence_interpretation_consistency, conceptual, 'Consistency of the non-violent interpretation within the Gita''s broader textual and historical context.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the resistance faced by this reading primarily structural (from entrenched institutions) or internalized (from individuals adhering to traditional beliefs)?',
    'Analysis of historical and contemporary efforts to suppress or marginalize this reading, distinguishing between institutional actions (e.g., banning texts, excommunicating proponents) and individual adherence to traditional norms.',
    'If resistance is primarily internalized, the effective suppression of this reading''s spread is higher than structural measures suggest, as individuals carry the resistance within their belief systems. If structural, external barriers are the primary challenge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for the resistance faced by the reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__universalist_devotional_reading, 1000, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t1000, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(gita_tr_t1200, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 1200, 0.1).
narrative_ontology:measurement(gita_tr_t1400, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 1400, 0.1).
narrative_ontology:measurement(gita_tr_t1600, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 1600, 0.1).
narrative_ontology:measurement(gita_tr_t1800, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(gita_tr_t2020, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(gita_be_t1000, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 1000, 0.05).
narrative_ontology:measurement(gita_be_t1200, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 1200, 0.05).
narrative_ontology:measurement(gita_be_t1400, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 1400, 0.05).
narrative_ontology:measurement(gita_be_t1600, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 1600, 0.05).
narrative_ontology:measurement(gita_be_t1800, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 1800, 0.05).
narrative_ontology:measurement(gita_be_t2020, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 2020, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t1000, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 1000, 0.05).
narrative_ontology:measurement(gita_su_t1200, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 1200, 0.05).
narrative_ontology:measurement(gita_su_t1400, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 1400, 0.05).
narrative_ontology:measurement(gita_su_t1600, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 1600, 0.05).
narrative_ontology:measurement(gita_su_t1800, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 1800, 0.05).
narrative_ontology:measurement(gita_su_t2020, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 2020, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__universalist_devotional_reading, identity_coordination).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse__orthodox_literal_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse__gandhian_allegorical_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Bhagavad Gita's Kurukshetra discourse kernel. Each reading instantiates a different constraint with unique structural properties and impacts, linked here as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
