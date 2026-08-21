% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__liturgical_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_living_language__liturgical_continuity_reading, []).

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
 *   constraint_id: hebrew_living_language__liturgical_continuity_reading
 *   human_readable: Hebrew Living Language: Liturgical Continuity Reading
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint story analyzes the claim that Hebrew remains a living
 *   language through its unbroken liturgical recitation and textual study
 *   across the Jewish diaspora. This is one specific reading of the broader
 *   'Hebrew living language' kernel, focusing on its role in cultural and
 *   religious continuity rather than daily spoken use or literary production.
 *   The constraint functions as a coordination mechanism for cultural
 *   preservation, with low extraction and suppression due to its voluntary
 *   nature.
 *
 * KEY AGENTS:
 *   - jewish_communities: Agenda setter/beneficiary (institutional/identity_locked) — maintain the practice
 *   - religious_practitioners: Beneficiary/payer (organized/identity_locked) — perform recitation and study
 *   - hebrew_scholars: Beneficiary/observer (organized/mobile) — study and interpret texts
 *   - secular_hebrew_speakers: Excluded (moderate/mobile) — not central to this reading's definition of 'living'
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__liturgical_continuity_reading, 0.15).
domain_priors:suppression_score(hebrew_living_language__liturgical_continuity_reading, 0.2).
domain_priors:theater_ratio(hebrew_living_language__liturgical_continuity_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__liturgical_continuity_reading, rope).
narrative_ontology:human_readable(hebrew_living_language__liturgical_continuity_reading, "Hebrew Living Language: Liturgical Continuity Reading").
narrative_ontology:topic_domain(hebrew_living_language__liturgical_continuity_reading, "historical_linguistics/language_revitalization/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__liturgical_continuity_reading, '35a55f15-115c-4e73-8e94-eef1f965b8c1').
narrative_ontology:cs_kernel_codification('35a55f15-115c-4e73-8e94-eef1f965b8c1', fixed_text).
narrative_ontology:cs_authority_grounding('35a55f15-115c-4e73-8e94-eef1f965b8c1', lineage).
narrative_ontology:cs_interpretation_layer_present('35a55f15-115c-4e73-8e94-eef1f965b8c1').
narrative_ontology:cs_reading_relation('35a55f15-115c-4e73-8e94-eef1f965b8c1', hebrew_living_language__native_generation_reading, coexists_with).
narrative_ontology:cs_reading_relation('35a55f15-115c-4e73-8e94-eef1f965b8c1', hebrew_living_language__literary_revival_reading, coexists_with).
narrative_ontology:cs_axiom('35a55f15-115c-4e73-8e94-eef1f965b8c1', foundational, liturgical_use_confers_life).
narrative_ontology:cs_axiom_status(liturgical_use_confers_life, holdable).
narrative_ontology:cs_axiom_grounding('35a55f15-115c-4e73-8e94-eef1f965b8c1', liturgical_use_confers_life, theological).
narrative_ontology:cs_axiom('35a55f15-115c-4e73-8e94-eef1f965b8c1', foundational, textual_study_sustains_language).
narrative_ontology:cs_axiom_status(textual_study_sustains_language, holdable).
narrative_ontology:cs_axiom_grounding('35a55f15-115c-4e73-8e94-eef1f965b8c1', textual_study_sustains_language, conventional).
narrative_ontology:cs_reference_frame('35a55f15-115c-4e73-8e94-eef1f965b8c1', diaspora_continuity_model).
narrative_ontology:cs_drift_state('35a55f15-115c-4e73-8e94-eef1f965b8c1', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('35a55f15-115c-4e73-8e94-eef1f965b8c1', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, jewish_communities).
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, religious_practitioners).
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, hebrew_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hebrew_living_language__liturgical_continuity_reading, religious_practitioners).
narrative_ontology:constraint_vindicates(hebrew_living_language__liturgical_continuity_reading, cultural_continuity_doctrine).
narrative_ontology:constraint_vindicates(hebrew_living_language__liturgical_continuity_reading, religious_observance_value).
narrative_ontology:constraint_vindicates(hebrew_living_language__liturgical_continuity_reading, diaspora_resilience_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and transmit the tradition of liturgical recitation and textual study as a core element of their collective identity and religious practice. They are the primary beneficiaries of the continuity this constraint provides.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, jewish_communities, agenda_setter,
    institutional, generational, identity_locked, global).

% Actively engage in the recitation of prayers and the study of sacred texts in Hebrew. They bear the 'cost' of time and effort for study but gain spiritual and communal belonging, and their professional identity is often tied to this practice.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, religious_practitioners, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__liturgical_continuity_reading, religious_practitioners, payer).

% Study and interpret classical Hebrew texts, contributing to the intellectual and academic continuity of the language. While they benefit from the existence of the textual tradition, their academic mobility allows them to study other languages.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, hebrew_scholars, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__liturgical_continuity_reading, hebrew_scholars, observer).

% Speak modern, revived Hebrew as a daily vernacular, primarily in Israel. From the perspective of this 'liturgical continuity' reading, their daily speech, while vibrant, is not the primary mechanism by which *this specific reading* defines Hebrew as a living language across the diaspora.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, secular_hebrew_speakers, excluded,
    moderate, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_living_language__liturgical_continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(hebrew_living_language__liturgical_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the preservation and transmission of Hebrew as a sacred language through consistent liturgical recitation and textual study, ensuring a shared linguistic and cultural heritage across diverse Jewish communities globally and across generations.
% TRANSFER_FUNCTION: Transfers cultural, religious, and historical knowledge, a sense of unbroken identity, and communal cohesion from past generations to present and future Jewish communities through shared linguistic practice.
% ABSENT_VOICES: Secular linguists and proponents of modern Hebrew as the sole criterion for a 'living language' would argue that liturgical use alone does not constitute a truly living language, as it lacks spontaneous daily generative speech. They are excluded from this reading's definition of 'living'.
% DISAPPEARANCE_RATIONALE: If liturgical recitation and textual study of Hebrew ceased overnight, it would fundamentally alter the religious and cultural identity of Jewish communities worldwide, severing a core link to their historical and spiritual heritage. The global Jewish cultural landscape would be profoundly reshaped.
% FOUNDING_PROBLEM: The challenge of maintaining a sacred language and its associated textual traditions across millennia of diaspora, without a continuous territorial base or daily spoken use, to ensure cultural and religious continuity.
% FOUNDING_PROBLEM_CORROBORATION: Historians of religion, cultural anthropologists, and sociologists of Jewish identity consistently corroborate the ongoing challenge of diaspora continuity and the central role of liturgical and textual Hebrew in addressing it. This is attested by numerous academic studies and community surveys.
narrative_ontology:disappearance_verdict(hebrew_living_language__liturgical_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__liturgical_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__liturgical_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(hebrew_living_language__liturgical_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__liturgical_continuity_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__liturgical_continuity_reading_tests).
:- end_tests(hebrew_living_language__liturgical_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because participation is voluntary, and the 'cost' is primarily effort and time for study, which is self-imposed for cultural/spiritual gain rather than extracted. Suppression is also low (0.20) as there are no coercive mechanisms; individuals and communities are free to reduce or cease their engagement. Theater ratio is negligible (0.05) because the practice is genuinely functional for cultural and religious transmission, not performative maintenance of an atrophied function. Accessibility collapse is moderate (0.40) because while alternatives (e.g., abandoning Hebrew, using only vernacular) exist, the deep cultural and identity ties make these alternatives less accessible for many adherents. Resistance is low (0.10) as the practice is largely self-selected.
 *
 * PERSPECTIVAL GAP:
 *   This reading emphasizes the internal, voluntary commitment to Hebrew's continuity. Other readings, such as the 'native generation' or 'literary revival' readings, would likely assign different metrics and classifications, as they focus on different criteria for 'living language' (e.g., daily spoken use, modern literary output). This story does not adjudicate between these readings but presents the structural reality of the liturgical continuity claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish communities, religious practitioners, and Hebrew scholars are beneficiaries, as they gain cultural, spiritual, and intellectual value from the continuity of Hebrew. There are no identifiable victims, as participation is voluntary and the 'costs' are self-selected efforts for communal and personal benefit. The 'excluded' secular Hebrew speakers are not targets of extraction but simply fall outside this reading's definitional scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preserving Hebrew as a living language through liturgical and textual means) remains highly relevant and actively pursued by its participants. There is no evidence of mandatrophy; the function is live, and the constraint is not maintained out of inertia. The low theater ratio supports this, indicating genuine functional activity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a valid reading of the ''Hebrew living language'' kernel, or is it a distinct constraint?',
    'Analysis of the core definitional criteria for ''living language'' across different linguistic and cultural frameworks. If the criteria for liturgical continuity are fundamentally incommensurable with other ''living language'' definitions, it might be a distinct constraint.',
    'If a distinct constraint, it would not be part of the ''Hebrew living language'' kernel family, and its classification would be evaluated in isolation. If a valid reading, its classification contributes to the overall understanding of the kernel''s contested nature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is one reading of the ''hebrew_living_language'' kernel, specifically the ''liturgical_continuity_reading''.').

omega_variable(
    definition_of_living_language,
    'What constitutes a ''living language'' in a way that is universally applicable or culturally sensitive?',
    'Cross-cultural linguistic and anthropological studies comparing language vitality metrics, including daily spoken use, literary production, and ritual/sacred use. Consensus among linguists and cultural experts.',
    'If ''living'' is strictly defined by daily generative speech, this constraint''s claim of ''living'' would be challenged, potentially reclassifying it as a ''preserved'' or ''ritual'' language rather than ''living''. If ritual use is accepted as a form of ''living'', the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_living_language, conceptual, 'Ambiguity in the definition of ''living language'' across different linguistic and cultural perspectives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__liturgical_continuity_reading, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1950, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(hebr_tr_t1960, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(hebr_tr_t1970, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(hebr_tr_t1980, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(hebr_tr_t1990, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(hebr_tr_t2000, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(hebr_tr_t2010, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(hebr_tr_t2020, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 2020, 0.05).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1950, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1950, 0.12).
narrative_ontology:measurement(hebr_be_t1960, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1960, 0.13).
narrative_ontology:measurement(hebr_be_t1970, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1970, 0.14).
narrative_ontology:measurement(hebr_be_t1980, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1980, 0.14).
narrative_ontology:measurement(hebr_be_t1990, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1990, 0.15).
narrative_ontology:measurement(hebr_be_t2000, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(hebr_be_t2010, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 2010, 0.15).
narrative_ontology:measurement(hebr_be_t2020, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 2020, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1950, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 1950, 0.18).
narrative_ontology:measurement(hebr_su_t1960, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 1960, 0.19).
narrative_ontology:measurement(hebr_su_t1970, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 1970, 0.2).
narrative_ontology:measurement(hebr_su_t1980, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 1980, 0.2).
narrative_ontology:measurement(hebr_su_t1990, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 1990, 0.2).
narrative_ontology:measurement(hebr_su_t2000, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 2000, 0.2).
narrative_ontology:measurement(hebr_su_t2010, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 2010, 0.2).
narrative_ontology:measurement(hebr_su_t2020, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 2020, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__liturgical_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_continuity_reading, jewish_cultural_identity).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_continuity_reading, religious_observance_norms).

% DUAL FORMULATION NOTE:
% This constraint is part of the 'Hebrew living language' family, which decomposes into multiple readings based on different criteria for language vitality. This specific reading focuses on liturgical and textual continuity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
