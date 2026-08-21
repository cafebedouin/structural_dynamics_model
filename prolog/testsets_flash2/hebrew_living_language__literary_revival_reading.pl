% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__literary_revival_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_living_language__literary_revival_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: hebrew_living_language__literary_revival_reading
 *   human_readable: Hebrew Living Language: Literary Revival Reading
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes the 'living' status of Hebrew during the
 *   Haskalah (Jewish Enlightenment) period, specifically through its use in
 *   secular literary production. This reading emphasizes the generative
 *   competence of writers to create new works, even in the absence of
 *   widespread native daily speech. It is one reading of the broader 'Hebrew
 *   living language' kernel, distinct from liturgical continuity or native
 *   spoken generation. The constraint is classified as a Rope due to its
 *   genuine coordination function among intellectuals and very low
 *   extractiveness, primarily benefiting the literary tradition itself.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__literary_revival_reading, 0.08).
domain_priors:suppression_score(hebrew_living_language__literary_revival_reading, 0.15).
domain_priors:theater_ratio(hebrew_living_language__literary_revival_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__literary_revival_reading, rope).
narrative_ontology:human_readable(hebrew_living_language__literary_revival_reading, "Hebrew Living Language: Literary Revival Reading").
narrative_ontology:topic_domain(hebrew_living_language__literary_revival_reading, "historical_linguistics/language_revitalization/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__literary_revival_reading, '6e74ddf0-9970-430a-8883-3a9021a073b2').
narrative_ontology:cs_kernel_codification('6e74ddf0-9970-430a-8883-3a9021a073b2', fixed_text).
narrative_ontology:cs_authority_grounding('6e74ddf0-9970-430a-8883-3a9021a073b2', practice).
narrative_ontology:cs_interpretation_layer_present('6e74ddf0-9970-430a-8883-3a9021a073b2').
narrative_ontology:cs_reading_relation('6e74ddf0-9970-430a-8883-3a9021a073b2', hebrew_living_language__liturgical_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('6e74ddf0-9970-430a-8883-3a9021a073b2', hebrew_living_language__native_generation_reading, influences).
narrative_ontology:cs_axiom('6e74ddf0-9970-430a-8883-3a9021a073b2', foundational, hebrew_vitality_through_generative_writing).
narrative_ontology:cs_axiom_status(hebrew_vitality_through_generative_writing, holdable).
narrative_ontology:cs_axiom_grounding('6e74ddf0-9970-430a-8883-3a9021a073b2', hebrew_vitality_through_generative_writing, conventional).
narrative_ontology:cs_axiom('6e74ddf0-9970-430a-8883-3a9021a073b2', secondary, secular_expression_modernizes_jewish_culture).
narrative_ontology:cs_axiom_status(secular_expression_modernizes_jewish_culture, holdable).
narrative_ontology:cs_axiom_grounding('6e74ddf0-9970-430a-8883-3a9021a073b2', secular_expression_modernizes_jewish_culture, instrumental).
narrative_ontology:cs_reference_frame('6e74ddf0-9970-430a-8883-3a9021a073b2', hebrew_as_literary_vehicle).
narrative_ontology:cs_drift_state('6e74ddf0-9970-430a-8883-3a9021a073b2', post_spoken_revival_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6e74ddf0-9970-430a-8883-3a9021a073b2', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__literary_revival_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, haskalah_intellectuals).
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, hebrew_literary_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hebrew_living_language__literary_revival_reading, traditional_religious_scholars).
narrative_ontology:constraint_vindicates(hebrew_living_language__literary_revival_reading, hebrew_as_modern_literary_vehicle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary proponents and practitioners of the Haskalah (Jewish Enlightenment) movement, who actively produced new secular literature in Hebrew. They saw this as a way to modernize Jewish culture and demonstrate Hebrew's vitality beyond religious contexts. They benefited from the intellectual and social capital derived from this literary output.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, haskalah_intellectuals, agenda_setter,
    organized, biographical, mobile, regional).

% The abstract body of Hebrew literature itself, which was expanded and enriched by the Haskalah period. This 'stakeholder' represents the continuity and evolution of Hebrew as a written language, benefiting from new genres and styles.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, hebrew_literary_tradition, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(hebrew_living_language__literary_revival_reading, hebrew_literary_tradition).

% While not directly 'paying' in a financial sense, they bore the cost of a perceived secularization or dilution of Hebrew's sacred character. They resisted the shift from purely liturgical or scholarly use to secular literary production, viewing it as a departure from tradition.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, traditional_religious_scholars, payer,
    organized, generational, constrained, regional).

% The vast majority of Ashkenazi Jews for whom Yiddish was the daily vernacular. They were largely excluded from the elite Hebrew literary revival, which did not directly address their daily linguistic needs or cultural expressions, reinforcing a linguistic divide.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, yiddish_speakers, excluded,
    powerless, biographical, identity_locked, local).

% Represents the analytical perspective of later generations and linguists assessing the historical impact of the Haskalah on Hebrew's eventual revitalization. They observe the long-term effects of this period on the language's trajectory.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, future_hebrew_speakers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: It coordinated the efforts of intellectuals across different regions to produce a shared body of modern Hebrew literature, establishing new norms for grammar, style, and vocabulary for secular use.
% TRANSFER_FUNCTION: It transferred intellectual and cultural capital to the Hebrew literary sphere, expanding its expressive capacity beyond religious texts, from a dispersed network of scholars and writers.
% ABSENT_VOICES: The majority of Yiddish-speaking Jews, who were the primary audience for popular Jewish culture, were largely absent from this elite literary project. They would have argued for the vitality of Yiddish as a living language and questioned the relevance of a purely literary Hebrew revival to their daily lives.
% DISAPPEARANCE_RATIONALE: If this specific literary production vanished, the broader historical trajectory of Hebrew's revival would be altered, but the underlying linguistic continuity (liturgical use, textual study) would persist. The world would not 'rearrange' in the same way as if a daily spoken language disappeared.
% FOUNDING_PROBLEM: The perceived stagnation of Hebrew as a 'dead' language, confined to religious texts, and the desire to modernize Jewish culture and integrate it into broader European intellectual currents.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Haskalah and linguists generally agree that the problem of Hebrew's 'stagnation' was a central concern for these intellectuals. However, the 'dead' status of the problem is corroborated by the later, more comprehensive, and successful efforts to revive Hebrew as a spoken language, which went beyond purely literary production.
narrative_ontology:disappearance_verdict(hebrew_living_language__literary_revival_reading, world_unchanged).
narrative_ontology:founding_problem_status(hebrew_living_language__literary_revival_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__literary_revival_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hebrew_living_language__literary_revival_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__literary_revival_reading, 0.08, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__literary_revival_reading_tests).
:- end_tests(hebrew_living_language__literary_revival_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.08) because the literary revival was largely a voluntary, elite intellectual project, not imposing significant costs on a broad population. Suppression is also low (0.15) as there was no active enforcement mechanism to compel participation; rather, it was a cultural movement. Theater ratio is negligible (0.05) as the literary output was genuine and functional for its intended audience. Accessibility collapse is high (0.8) because for those outside the intellectual circles, the 'living' aspect of Hebrew through this lens was largely inaccessible. Resistance is low (0.05) as opposition was primarily cultural or ideological, not active struggle against an extractive force.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Haskalah intellectuals, this was a vital, living expression of Hebrew. From the perspective of Yiddish speakers, it was an elite, somewhat detached project. The engine's classification as a Rope reflects the internal dynamics of the literary movement, while omegas address the broader contest over what 'living' truly means for a language.
 *
 * DIRECTIONALITY LOGIC:
 *   Haskalah intellectuals are the agenda-setters and primary beneficiaries, actively shaping and benefiting from the literary output. The Hebrew literary tradition itself is a beneficiary, gaining new works and genres. Traditional religious scholars are 'payers' in a cultural sense, bearing the cost of a perceived secularization. Yiddish speakers are 'excluded' as the literary revival did not directly serve their linguistic needs. Future Hebrew speakers are observers, analyzing the historical impact.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate was to demonstrate Hebrew's vitality and modernize Jewish culture. While the specific form of 'living' (purely literary) eventually gave way to a more comprehensive spoken revival, the literary production itself was a crucial step, so it did not fully atrophy. The 'dead' status of the founding problem refers to the *original* problem of stagnation, which was eventually superseded by a more ambitious goal of full spoken revival, making this a transitional phase rather than a fully atrophied constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_living_language,
    'Does ''living language'' require native daily speech, or can it be sustained by generative literary production and liturgical use?',
    'Conceptual analysis and consensus among historical linguists and sociolinguists regarding the criteria for language vitality, potentially informed by case studies of other language revivals.',
    'If native daily speech is deemed essential, this reading''s claim of ''living'' is weakened, potentially reclassifying it as a Piton (theatrical maintenance of a non-functional claim). If literary production is sufficient, the Rope classification is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_living_language, conceptual, 'Ambiguity in the definition of a ''living language'' and its application to Hebrew during the Haskalah.').

omega_variable(
    reachability_of_literary_hebrew,
    'To what extent was Haskalah Hebrew literature genuinely accessible and generative for a broader Jewish public beyond the intellectual elite?',
    'Empirical studies of literacy rates in Hebrew, circulation of Haskalah texts, and evidence of non-elite engagement with secular Hebrew literature during the period.',
    'If reachability was very low, the ''coordination'' function is limited to a small elite, and the constraint''s overall impact on language vitality is reduced, potentially shifting its classification towards a more ''theatrical'' or ''inertial'' type from a broader societal perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reachability_of_literary_hebrew, empirical, 'The actual social reach and generative capacity of Haskalah Hebrew literature.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__literary_revival_reading, 1780, 1880).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1780, hebrew_living_language__literary_revival_reading, theater_ratio, 1780, 0.03).
narrative_ontology:measurement(hebr_tr_t1800, hebrew_living_language__literary_revival_reading, theater_ratio, 1800, 0.04).
narrative_ontology:measurement(hebr_tr_t1820, hebrew_living_language__literary_revival_reading, theater_ratio, 1820, 0.05).
narrative_ontology:measurement(hebr_tr_t1840, hebrew_living_language__literary_revival_reading, theater_ratio, 1840, 0.05).
narrative_ontology:measurement(hebr_tr_t1860, hebrew_living_language__literary_revival_reading, theater_ratio, 1860, 0.05).
narrative_ontology:measurement(hebr_tr_t1880, hebrew_living_language__literary_revival_reading, theater_ratio, 1880, 0.05).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1780, hebrew_living_language__literary_revival_reading, base_extractiveness, 1780, 0.05).
narrative_ontology:measurement(hebr_be_t1800, hebrew_living_language__literary_revival_reading, base_extractiveness, 1800, 0.07).
narrative_ontology:measurement(hebr_be_t1820, hebrew_living_language__literary_revival_reading, base_extractiveness, 1820, 0.08).
narrative_ontology:measurement(hebr_be_t1840, hebrew_living_language__literary_revival_reading, base_extractiveness, 1840, 0.08).
narrative_ontology:measurement(hebr_be_t1860, hebrew_living_language__literary_revival_reading, base_extractiveness, 1860, 0.09).
narrative_ontology:measurement(hebr_be_t1880, hebrew_living_language__literary_revival_reading, base_extractiveness, 1880, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1780, hebrew_living_language__literary_revival_reading, suppression_requirement, 1780, 0.1).
narrative_ontology:measurement(hebr_su_t1800, hebrew_living_language__literary_revival_reading, suppression_requirement, 1800, 0.12).
narrative_ontology:measurement(hebr_su_t1820, hebrew_living_language__literary_revival_reading, suppression_requirement, 1820, 0.15).
narrative_ontology:measurement(hebr_su_t1840, hebrew_living_language__literary_revival_reading, suppression_requirement, 1840, 0.15).
narrative_ontology:measurement(hebr_su_t1860, hebrew_living_language__literary_revival_reading, suppression_requirement, 1860, 0.14).
narrative_ontology:measurement(hebr_su_t1880, hebrew_living_language__literary_revival_reading, suppression_requirement, 1880, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__literary_revival_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_living_language__literary_revival_reading, hebrew_living_language__liturgical_continuity_reading).
narrative_ontology:affects_constraint(hebrew_living_language__literary_revival_reading, hebrew_living_language__native_generation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'hebrew_living_language' kernel. This 'literary_revival_reading' focuses on Haskalah literary production. It is linked to the 'liturgical_continuity_reading' and 'native_generation_reading' as part of the same contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
