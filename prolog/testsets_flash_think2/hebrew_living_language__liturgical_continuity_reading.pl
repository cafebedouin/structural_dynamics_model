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
 *   human_readable: Hebrew as a Living Language: Liturgical Continuity Reading
 *   domain: historical_linguistics/religious_studies/cultural_preservation
 *
 * SUMMARY:
 *   This constraint describes the 'liturgical continuity' reading of Hebrew's
 *   status as a living language. It posits that Hebrew remained a living
 *   language throughout the diaspora due to its unbroken use in religious
 *   liturgy, prayer, and textual study, rather than through daily spoken use.
 *   This reading emphasizes the language's sacred and scholarly function as
 *   its primary mode of 'life'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__liturgical_continuity_reading, 0.15).
domain_priors:suppression_score(hebrew_living_language__liturgical_continuity_reading, 0.05).
domain_priors:theater_ratio(hebrew_living_language__liturgical_continuity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__liturgical_continuity_reading, rope).
narrative_ontology:human_readable(hebrew_living_language__liturgical_continuity_reading, "Hebrew as a Living Language: Liturgical Continuity Reading").
narrative_ontology:topic_domain(hebrew_living_language__liturgical_continuity_reading, "historical_linguistics/religious_studies/cultural_preservation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__liturgical_continuity_reading, '7c24530a-9cdf-4fa9-aa16-50c35dba4370').
narrative_ontology:cs_kernel_codification('7c24530a-9cdf-4fa9-aa16-50c35dba4370', fixed_text).
narrative_ontology:cs_authority_grounding('7c24530a-9cdf-4fa9-aa16-50c35dba4370', lineage).
narrative_ontology:cs_interpretation_layer_present('7c24530a-9cdf-4fa9-aa16-50c35dba4370').
narrative_ontology:cs_reading_relation('7c24530a-9cdf-4fa9-aa16-50c35dba4370', hebrew_living_language__native_generation_reading, coexists_with).
narrative_ontology:cs_reading_relation('7c24530a-9cdf-4fa9-aa16-50c35dba4370', hebrew_living_language__literary_revival_reading, coexists_with).
narrative_ontology:cs_axiom('7c24530a-9cdf-4fa9-aa16-50c35dba4370', foundational, hebrew_is_sacred_language).
narrative_ontology:cs_axiom_status(hebrew_is_sacred_language, holdable).
narrative_ontology:cs_axiom_grounding('7c24530a-9cdf-4fa9-aa16-50c35dba4370', hebrew_is_sacred_language, theological).
narrative_ontology:cs_axiom('7c24530a-9cdf-4fa9-aa16-50c35dba4370', foundational, continuity_through_recitation_and_study).
narrative_ontology:cs_axiom_status(continuity_through_recitation_and_study, holdable).
narrative_ontology:cs_axiom_grounding('7c24530a-9cdf-4fa9-aa16-50c35dba4370', continuity_through_recitation_and_study, conventional).
narrative_ontology:cs_reference_frame('7c24530a-9cdf-4fa9-aa16-50c35dba4370', diaspora_liturgical_tradition).
narrative_ontology:cs_drift_state('7c24530a-9cdf-4fa9-aa16-50c35dba4370', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7c24530a-9cdf-4fa9-aa16-50c35dba4370', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, jewish_communities_worldwide).
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, religious_scholars).
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, liturgical_practitioners).
narrative_ontology:constraint_vindicates(hebrew_living_language__liturgical_continuity_reading, hebrew_continuity_doctrine).
narrative_ontology:constraint_vindicates(hebrew_living_language__liturgical_continuity_reading, sacred_language_preservation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain cultural and religious identity through the continuous use and study of Hebrew in liturgy and texts. Participation is voluntary but deeply tied to communal belonging.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, jewish_communities_worldwide, beneficiary,
    organized, generational, identity_locked, global).

% Preserve, interpret, and transmit Hebrew sacred texts and traditions. Their work ensures the intellectual and spiritual continuity of the language.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, religious_scholars, beneficiary,
    powerful, biographical, constrained, global).

% Engage in daily and weekly recitation of prayers and study of texts in Hebrew, directly maintaining the oral and textual tradition. Their participation is a core act of faith and identity.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, liturgical_practitioners, beneficiary,
    moderate, biographical, identity_locked, local).

% Study the historical and sociolinguistic aspects of Hebrew's continuity, analyzing its role in religious practice and cultural identity without direct participation in its maintenance.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, secular_linguists, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_living_language__liturgical_continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(hebrew_living_language__liturgical_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the continuous use and study of Hebrew across generations and geographies, ensuring its survival as a sacred and scholarly language for Jewish communities worldwide.
% TRANSFER_FUNCTION: Transfers cultural and religious knowledge, identity, and spiritual connection across generations, primarily through voluntary effort, communal practice, and scholarly transmission.
% ABSENT_VOICES: Those who define 'living language' strictly by native daily speech (e.g., proponents of the 'native_generation_reading') might argue this form of continuity is insufficient, but they are not excluded from the broader discourse on Hebrew's status.
% DISAPPEARANCE_RATIONALE: If liturgical recitation and textual study of Hebrew ceased overnight, a core pillar of Jewish religious and cultural identity would collapse, leading to a profound reorganization of communal life, religious practice, and scholarly traditions. The language's 'living' status, under this reading, would be lost.
% FOUNDING_PROBLEM: The need to preserve the sacred language of scripture and prayer, and the cultural identity tied to it, across centuries of diaspora and persecution, preventing its complete loss as a understood or used language.
% FOUNDING_PROBLEM_CORROBORATION: Jewish religious authorities, historians, and cultural institutions worldwide corroborate the ongoing importance of liturgical Hebrew for identity and continuity. This is supported by centuries of continuous practice and scholarly output.
narrative_ontology:disappearance_verdict(hebrew_living_language__liturgical_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__liturgical_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__liturgical_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is low because participation in liturgical recitation and textual study is largely voluntary, driven by religious and cultural commitment rather than coercion. Suppression is minimal, as adherence is maintained through communal norms and identity, not active enforcement. Theater ratio is low because the practices of prayer and study are genuinely functional for the community's spiritual and intellectual life. Accessibility collapse is moderate; while alternatives (other languages, abandoning Hebrew) exist, the cultural and religious cost of exit is significant, leading to a strong internal pull towards continuity.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal perspectival gap within this reading, as the value of liturgical continuity is broadly shared among its participants. However, significant gaps exist when comparing this reading to others that define 'living language' differently (e.g., requiring native daily speech).
 *
 * DIRECTIONALITY LOGIC:
 *   All identified stakeholders are beneficiaries. Jewish communities worldwide benefit from the preservation of a core aspect of their identity. Religious scholars benefit from the continued existence of their subject of study and transmission. Liturgical practitioners directly benefit from engaging in a cherished religious practice. There are no identifiable victims or payers, as the constraint is maintained through voluntary participation and shared cultural investment.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate of preserving Hebrew as a sacred and scholarly language through liturgical continuity remains live and actively fulfilled. The constraint has not atrophied; its function is continuously enacted by its participants, preventing mislabeling as a Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_living_language,
    'Is liturgical and scholarly use sufficient to define a language as ''living'', or does it require native daily speech?',
    'Conceptual clarification within the field of historical linguistics and sociolinguistics, potentially informed by community self-identification and historical precedent.',
    'If native daily speech is deemed a necessary condition, this reading''s claim of ''living'' is weakened, potentially reclassifying it as a ''preserved'' or ''ritual'' language rather than ''living''. If liturgical use is sufficient, the claim is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_living_language, conceptual, 'Ambiguity in the definition of ''living language'' for non-vernacular use.').

omega_variable(
    impact_of_modern_hebrew_revival,
    'Does the modern revival of spoken Hebrew (as per the ''native_generation_reading'') diminish or reinforce the ''living'' status conferred by liturgical continuity?',
    'Sociolinguistic studies on the interaction between modern spoken Hebrew and traditional liturgical Hebrew, and analysis of how communities perceive their relationship.',
    'If modern Hebrew is seen as the *only* true ''living'' form, it could conceptually override this reading''s claim. If the two forms are seen as complementary, it might reinforce the overall ''living'' status of Hebrew.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_of_modern_hebrew_revival, empirical, 'Interaction between traditional liturgical use and modern spoken Hebrew.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__liturgical_continuity_reading, 1000, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1000, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(hebr_tr_t1200, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1200, 0.1).
narrative_ontology:measurement(hebr_tr_t1400, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1400, 0.1).
narrative_ontology:measurement(hebr_tr_t1600, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1600, 0.1).
narrative_ontology:measurement(hebr_tr_t1800, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(hebr_tr_t2000, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 2000, 0.1).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1000, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1000, 0.15).
narrative_ontology:measurement(hebr_be_t1200, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1200, 0.15).
narrative_ontology:measurement(hebr_be_t1400, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1400, 0.15).
narrative_ontology:measurement(hebr_be_t1600, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1600, 0.15).
narrative_ontology:measurement(hebr_be_t1800, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1800, 0.15).
narrative_ontology:measurement(hebr_be_t2000, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 2000, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1000, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 1000, 0.05).
narrative_ontology:measurement(hebr_su_t1200, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 1200, 0.05).
narrative_ontology:measurement(hebr_su_t1400, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 1400, 0.05).
narrative_ontology:measurement(hebr_su_t1600, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 1600, 0.05).
narrative_ontology:measurement(hebr_su_t1800, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 1800, 0.05).
narrative_ontology:measurement(hebr_su_t2000, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 2000, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__liturgical_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language__native_generation_reading).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language__literary_revival_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'Hebrew as a living language' kernel, focusing on liturgical and textual continuity. The other readings emphasize native speech generation and modern literary production.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
