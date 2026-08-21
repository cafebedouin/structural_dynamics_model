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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Hebrew as a Living Literary Language (Haskalah Reading)
 *   domain: historical_linguistics/cultural_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes the role of Haskalah literary production in
 *   maintaining Hebrew as a living language, focusing on written generative
 *   competence rather than native daily speech. It is one reading of the
 *   broader 'hebrew_living_language' kernel. The Haskalah movement, primarily
 *   in Eastern Europe, saw a flourishing of modern Hebrew literature,
 *   journalism, and scholarship, demonstrating Hebrew's capacity for
 *   contemporary expression. This was an elite cultural practice, not a mass
 *   movement for spoken language.
 *
 * KEY AGENTS:
 *   - haskalah_intellectuals: Agenda-setter/Beneficiary (organized/mobile)
 *   - hebrew_literary_tradition: Beneficiary (analytical/analytical)
 *   - general_jewish_populace: Payer/Excluded (powerless/constrained)
 *   - liturgical_scholars: Observer (organized/constrained)
 *   - future_hebrew_speakers: Beneficiary (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__literary_revival_reading, 0.15).
domain_priors:suppression_score(hebrew_living_language__literary_revival_reading, 0.1).
domain_priors:theater_ratio(hebrew_living_language__literary_revival_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__literary_revival_reading, rope).
narrative_ontology:human_readable(hebrew_living_language__literary_revival_reading, "Hebrew as a Living Literary Language (Haskalah Reading)").
narrative_ontology:topic_domain(hebrew_living_language__literary_revival_reading, "historical_linguistics/cultural_revitalization/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__literary_revival_reading, '8daa4d12-1c25-4354-8ec9-ab6115b4a44b').
narrative_ontology:cs_kernel_codification('8daa4d12-1c25-4354-8ec9-ab6115b4a44b', fixed_text).
narrative_ontology:cs_authority_grounding('8daa4d12-1c25-4354-8ec9-ab6115b4a44b', practice).
narrative_ontology:cs_interpretation_layer_present('8daa4d12-1c25-4354-8ec9-ab6115b4a44b').
narrative_ontology:cs_reading_relation('8daa4d12-1c25-4354-8ec9-ab6115b4a44b', hebrew_living_language__liturgical_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('8daa4d12-1c25-4354-8ec9-ab6115b4a44b', hebrew_living_language__native_generation_reading, influences).
narrative_ontology:cs_axiom('8daa4d12-1c25-4354-8ec9-ab6115b4a44b', foundational, hebrew_vitality_through_written_expression).
narrative_ontology:cs_axiom_status(hebrew_vitality_through_written_expression, holdable).
narrative_ontology:cs_axiom_grounding('8daa4d12-1c25-4354-8ec9-ab6115b4a44b', hebrew_vitality_through_written_expression, conventional).
narrative_ontology:cs_axiom('8daa4d12-1c25-4354-8ec9-ab6115b4a44b', foundational, hebrew_as_vehicle_for_modern_secular_thought).
narrative_ontology:cs_axiom_status(hebrew_as_vehicle_for_modern_secular_thought, holdable).
narrative_ontology:cs_axiom_grounding('8daa4d12-1c25-4354-8ec9-ab6115b4a44b', hebrew_as_vehicle_for_modern_secular_thought, conventional).
narrative_ontology:cs_reference_frame('8daa4d12-1c25-4354-8ec9-ab6115b4a44b', hebrew_as_modern_literary_medium).
narrative_ontology:cs_drift_state('8daa4d12-1c25-4354-8ec9-ab6115b4a44b', post_zionic_revival_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8daa4d12-1c25-4354-8ec9-ab6115b4a44b', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__literary_revival_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, haskalah_intellectuals).
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, hebrew_literary_tradition).
narrative_ontology:constraint_victim(hebrew_living_language__literary_revival_reading, general_jewish_populace).
narrative_ontology:constraint_vindicates(hebrew_living_language__literary_revival_reading, hebrew_cultural_continuity).
narrative_ontology:constraint_vindicates(hebrew_living_language__literary_revival_reading, language_revitalization_through_literature).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary proponents and practitioners of the Haskalah (Jewish Enlightenment) movement, who actively produced modern Hebrew literature, journalism, and scholarship. They benefited from the cultural prestige and intellectual continuity of Hebrew, and their efforts shaped its evolution. They could choose to write in other languages but chose Hebrew for ideological reasons.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, haskalah_intellectuals, agenda_setter,
    organized, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__literary_revival_reading, haskalah_intellectuals, beneficiary).

% The abstract body of Hebrew literature and scholarship, which gained new life and expanded its thematic and stylistic range through Haskalah production. Its continuity and modernization were ensured by these efforts.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, hebrew_literary_tradition, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(hebrew_living_language__literary_revival_reading, hebrew_literary_tradition).

% The majority of Jewish people, who did not actively participate in or directly consume Haskalah literary output due to linguistic and educational barriers. They indirectly 'paid' by being excluded from this form of cultural vitality, experiencing a widening gap between elite and popular Hebrew engagement.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, general_jewish_populace, payer,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__literary_revival_reading, general_jewish_populace, excluded).

% Scholars and religious leaders who maintained Hebrew primarily through prayer, ritual, and traditional textual study. They observed the Haskalah movement, sometimes with skepticism, but their own practice of Hebrew was largely distinct and unaffected by the literary revival.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, liturgical_scholars, observer,
    organized, generational, constrained, global).

% Future generations who would inherit a more developed and flexible Hebrew language, partly due to the groundwork laid by Haskalah literary innovation. This group benefits from the constraint's long-term cultural impact.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, future_hebrew_speakers, beneficiary,
    analytical, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(hebrew_living_language__literary_revival_reading, future_hebrew_speakers).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To maintain Hebrew as a vibrant, evolving language of intellectual and artistic expression, preventing its complete relegation to purely liturgical or academic use, and to provide a common linguistic medium for modern Jewish thought.
% TRANSFER_FUNCTION: Transfers cultural capital, intellectual continuity, and linguistic innovation to the Haskalah movement and its participants, from the broader Jewish populace who do not participate in this elite literary production.
% ABSENT_VOICES: The vast majority of the Jewish populace who did not have the education or access to participate in Haskalah literary production. They might argue that a language not spoken daily is not truly 'living' or accessible to the common person.
% DISAPPEARANCE_RATIONALE: If Haskalah literary production had not occurred, the trajectory of Hebrew's revitalization would have been significantly different, potentially delaying or altering the later native speech revival. The idea of Hebrew as a modern, flexible language capable of expressing secular thought would have been much weaker, impacting subsequent cultural and political movements.
% FOUNDING_PROBLEM: The perceived decline of Hebrew into a static, solely liturgical language, threatening its role as a vehicle for modern thought, secular culture, and a unifying force for Jewish identity in an era of assimilation.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Haskalah movement and linguistic scholars attest to the intellectual and cultural concerns regarding Hebrew's status at the time. Contemporary writings from Maskilim explicitly articulate the fear of Hebrew's obsolescence as a modern language, corroborating the problem's live status from their perspective.
narrative_ontology:disappearance_verdict(hebrew_living_language__literary_revival_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__literary_revival_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__literary_revival_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(hebrew_living_language__literary_revival_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__literary_revival_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a Rope because it genuinely coordinated the efforts of intellectuals to revitalize Hebrew as a literary language, with very low extractiveness and suppression. The 'extraction' from the general Jewish populace is indirect, stemming from their exclusion from this elite cultural sphere rather than active coercion. The literary production was a genuine effort, not theatrical, hence the low theater_ratio. Accessibility was limited by educational access, not active suppression, leading to a moderate accessibility_collapse.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Haskalah intellectuals, this was a vital and successful coordination effort to save Hebrew from obsolescence. From the perspective of the general populace, it created a form of Hebrew that was largely inaccessible, reinforcing a cultural divide. The engine's per-seat classification would reflect this divergence, with beneficiaries experiencing a Rope and the excluded experiencing a more extractive type due to their lack of access.
 *
 * DIRECTIONALITY LOGIC:
 *   Haskalah intellectuals are clear beneficiaries and agenda-setters, actively shaping the constraint and gaining cultural capital (low d). The Hebrew literary tradition itself benefits from continuity and modernization. The general Jewish populace is a victim/payer due to their exclusion and the cultural distance created by this elite practice (high d). Liturgical scholars are observers, maintaining a separate, parallel form of Hebrew. Future Hebrew speakers are long-term beneficiaries of the linguistic groundwork laid.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    living_language_definition_ambiguity,
    'Does ''living language'' require native daily speech, or is robust literary production sufficient for vitality?',
    'Conceptual clarification of ''living language'' criteria, potentially through linguistic consensus or historical outcomes of language revitalization efforts.',
    'If daily speech is a strict requirement, this constraint''s claim of ''living language'' is weakened, and its classification might shift towards a more ''inertial'' type (Piton) from the perspective of a native-speech advocate. If literary production is sufficient, the Rope classification is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(living_language_definition_ambiguity, conceptual, 'Ambiguity in the definition of a ''living language'' in the absence of native daily speech.').

omega_variable(
    reachability_ambiguity,
    'Was the Haskalah literary Hebrew strictly reachable by the broader Jewish populace, or was it an elite, inaccessible form?',
    'Sociolinguistic studies of literacy rates and language comprehension among different strata of the Jewish population during the Haskalah period.',
    'If strictly unreachable, the ''excluded'' status of the general populace is reinforced, and the constraint''s coordination function is more narrowly defined as serving an elite, potentially increasing its effective extraction from the excluded. If more broadly reachable, the coordination function is wider.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reachability_ambiguity, empirical, 'Ambiguity regarding the accessibility and reach of Haskalah literary Hebrew.').

omega_variable(
    causal_link_to_native_speech,
    'To what extent did Haskalah literary production directly enable or accelerate the later revival of Hebrew as a native spoken language?',
    'Historical linguistic analysis comparing the linguistic features and cultural impact of Haskalah Hebrew with the language of the later Zionist revival, and counterfactual historical analysis.',
    'If the causal link is strong, this constraint is a crucial Scaffold for the native speech revival. If weak, its role is more isolated, and the ''influences'' relation to the native_generation_reading is less direct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_link_to_native_speech, empirical, 'The strength of the causal link between literary revival and native speech revival.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__literary_revival_reading, 1780, 1880).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1780, hebrew_living_language__literary_revival_reading, theater_ratio, 1780, 0.05).
narrative_ontology:measurement(hebr_tr_t1800, hebrew_living_language__literary_revival_reading, theater_ratio, 1800, 0.05).
narrative_ontology:measurement(hebr_tr_t1820, hebrew_living_language__literary_revival_reading, theater_ratio, 1820, 0.05).
narrative_ontology:measurement(hebr_tr_t1840, hebrew_living_language__literary_revival_reading, theater_ratio, 1840, 0.05).
narrative_ontology:measurement(hebr_tr_t1860, hebrew_living_language__literary_revival_reading, theater_ratio, 1860, 0.05).
narrative_ontology:measurement(hebr_tr_t1880, hebrew_living_language__literary_revival_reading, theater_ratio, 1880, 0.05).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1780, hebrew_living_language__literary_revival_reading, base_extractiveness, 1780, 0.12).
narrative_ontology:measurement(hebr_be_t1800, hebrew_living_language__literary_revival_reading, base_extractiveness, 1800, 0.13).
narrative_ontology:measurement(hebr_be_t1820, hebrew_living_language__literary_revival_reading, base_extractiveness, 1820, 0.14).
narrative_ontology:measurement(hebr_be_t1840, hebrew_living_language__literary_revival_reading, base_extractiveness, 1840, 0.15).
narrative_ontology:measurement(hebr_be_t1860, hebrew_living_language__literary_revival_reading, base_extractiveness, 1860, 0.15).
narrative_ontology:measurement(hebr_be_t1880, hebrew_living_language__literary_revival_reading, base_extractiveness, 1880, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1780, hebrew_living_language__literary_revival_reading, suppression_requirement, 1780, 0.1).
narrative_ontology:measurement(hebr_su_t1800, hebrew_living_language__literary_revival_reading, suppression_requirement, 1800, 0.1).
narrative_ontology:measurement(hebr_su_t1820, hebrew_living_language__literary_revival_reading, suppression_requirement, 1820, 0.1).
narrative_ontology:measurement(hebr_su_t1840, hebrew_living_language__literary_revival_reading, suppression_requirement, 1840, 0.1).
narrative_ontology:measurement(hebr_su_t1860, hebrew_living_language__literary_revival_reading, suppression_requirement, 1860, 0.1).
narrative_ontology:measurement(hebr_su_t1880, hebrew_living_language__literary_revival_reading, suppression_requirement, 1880, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__literary_revival_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'hebrew_living_language' kernel, focusing on literary production. Sibling readings address liturgical continuity and native speech generation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
