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
 *   human_readable: Hebrew's Life through Haskalah Literary Production
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents the 'literary revival' reading of the
 *   'hebrew_living_language' kernel, focusing on the Haskalah movement's
 *   efforts to revitalize Hebrew through written generative competence. This
 *   reading posits that Hebrew's vitality was maintained and advanced through
 *   the production of modern secular literature, even without widespread
 *   native daily speech. Sibling readings include
 *   'liturgical_continuity_reading' (Hebrew lives through religious use) and
 *   'native_generation_reading' (Hebrew lives through daily native speech).
 *
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
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__literary_revival_reading, rope).
narrative_ontology:human_readable(hebrew_living_language__literary_revival_reading, "Hebrew's Life through Haskalah Literary Production").
narrative_ontology:topic_domain(hebrew_living_language__literary_revival_reading, "historical_linguistics/language_revitalization/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__literary_revival_reading, '056f3d20-cc16-4854-ae0e-3794868c2e8f').
narrative_ontology:cs_kernel_codification('056f3d20-cc16-4854-ae0e-3794868c2e8f', formalized).
narrative_ontology:cs_authority_grounding('056f3d20-cc16-4854-ae0e-3794868c2e8f', practice).
narrative_ontology:cs_interpretation_layer_present('056f3d20-cc16-4854-ae0e-3794868c2e8f').
narrative_ontology:cs_reading_relation('056f3d20-cc16-4854-ae0e-3794868c2e8f', hebrew_living_language__liturgical_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('056f3d20-cc16-4854-ae0e-3794868c2e8f', hebrew_living_language__native_generation_reading, influences).
narrative_ontology:cs_axiom('056f3d20-cc16-4854-ae0e-3794868c2e8f', foundational, hebrew_vitality_through_written_expression).
narrative_ontology:cs_axiom_status(hebrew_vitality_through_written_expression, holdable).
narrative_ontology:cs_axiom_grounding('056f3d20-cc16-4854-ae0e-3794868c2e8f', hebrew_vitality_through_written_expression, conventional).
narrative_ontology:cs_axiom('056f3d20-cc16-4854-ae0e-3794868c2e8f', foundational, generative_competence_without_daily_speech).
narrative_ontology:cs_axiom_status(generative_competence_without_daily_speech, holdable).
narrative_ontology:cs_axiom_grounding('056f3d20-cc16-4854-ae0e-3794868c2e8f', generative_competence_without_daily_speech, conventional).
narrative_ontology:cs_reference_frame('056f3d20-cc16-4854-ae0e-3794868c2e8f', hebrew_as_modern_literary_vehicle).
narrative_ontology:cs_drift_state('056f3d20-cc16-4854-ae0e-3794868c2e8f', contemporary_revival_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('056f3d20-cc16-4854-ae0e-3794868c2e8f', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__literary_revival_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, haskalah_intellectuals).
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, hebrew_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hebrew_living_language__literary_revival_reading, general_jewish_populace).
narrative_ontology:constraint_vindicates(hebrew_living_language__literary_revival_reading, hebrew_literary_continuity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary proponents and producers of new Hebrew literature during the Haskalah (Jewish Enlightenment) period. They actively sought to modernize Hebrew and demonstrate its capacity for secular expression, benefiting from the cultural prestige and intellectual continuity this production afforded.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, haskalah_intellectuals, agenda_setter,
    organized, biographical, mobile, regional).

% Academics and researchers who study, preserve, and transmit the Hebrew literary tradition. They benefit from the existence of a vibrant literary corpus that provides material for their work and validates the language's historical and cultural significance.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, hebrew_scholars, beneficiary,
    organized, generational, constrained, global).

% The broader community for whom Hebrew literature was produced. While they gained cultural enrichment, most did not achieve generative competence in Hebrew daily speech, making access to this 'living' form of the language indirect and requiring significant effort.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, general_jewish_populace, payer,
    powerless, biographical, constrained, global).

% Linguists and activists focused on the broader revitalization of Hebrew, particularly its re-establishment as a spoken language. They analyze the historical role of literary production but often argue for a more comprehensive definition of language vitality that includes daily native speech.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, language_revitalization_advocates, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the intellectual and creative efforts of scholars and writers to produce new Hebrew literature, ensuring the language's continued development and relevance in a modern context, beyond its purely liturgical use.
% TRANSFER_FUNCTION: Transfers cultural capital, intellectual prestige, and a sense of national/cultural continuity to the participants and the broader Jewish community, through the production and consumption of Hebrew literature.
% ABSENT_VOICES: Advocates for Hebrew as a spoken, daily language (e.g., early Zionists) would argue that literary production alone is insufficient for a 'living' language, but their perspective was not central to the Haskalah's definition of Hebrew's vitality.
% DISAPPEARANCE_RATIONALE: If the Haskalah literary production ceased, the modern development of Hebrew would have been severely stunted, potentially leading to its complete fossilization as a purely liturgical or academic language, rather than a language capable of modern expression. The subsequent native speech revival would have lacked a crucial foundation.
% FOUNDING_PROBLEM: To prevent Hebrew from becoming a 'dead' language, confined solely to religious texts, by demonstrating its capacity for modern secular expression and intellectual discourse.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Haskalah movement and literary critics attest to the movement's explicit goal of revitalizing Hebrew through literature. Contemporary linguists and sociolinguists also acknowledge the historical role of this period in Hebrew's trajectory, even as the definition of 'living' evolved.
narrative_ontology:disappearance_verdict(hebrew_living_language__literary_revival_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__literary_revival_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__literary_revival_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   The constraint is classified as a Rope due to its genuine coordination function in fostering a modern Hebrew literary tradition, with very low extractiveness and suppression. The 'elite' nature of literary production means accessibility collapse is high for the general populace, as generative competence was not widespread. Resistance was low, as the literary project was largely embraced by its target audience of intellectuals. The metrics reflect the period of the Haskalah's primary influence on Hebrew literature.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Haskalah intellectuals, this literary production was the very definition of Hebrew's 'living' status, a successful coordination of cultural renewal. From the perspective of later native speech advocates, this form of 'life' was incomplete, a necessary but insufficient step towards full revitalization.
 *
 * DIRECTIONALITY LOGIC:
 *   Haskalah intellectuals and Hebrew scholars are beneficiaries, actively shaping and benefiting from the literary output and its associated cultural prestige. The general Jewish populace is a 'payer' in terms of the cultural effort required to access this elite form of the language, though not in a directly extractive sense. Language revitalization advocates serve as analytical observers, assessing the historical impact and limitations of this approach.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sufficiency_of_literary_vitality,
    'Is ''living'' through literary production, without native daily speech, a sufficient condition for a language''s vitality?',
    'Emergence of a robust native-speaking community (as later occurred), or a consensus among linguists on alternative criteria for language vitality that explicitly exclude or de-emphasize literary production.',
    'If insufficient, this constraint''s claim to ''living'' is weakened, and its classification might shift towards a more ''inertial'' type (e.g., Piton) from the perspective of native speech advocates, as its primary function would be seen as superseded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sufficiency_of_literary_vitality, conceptual, 'Whether literary production alone constitutes a ''living'' language.').

omega_variable(
    strict_reachability_ambiguity,
    'Does the Haskalah''s literary production truly ensure the language''s ''reachability'' for future generations, or only for a select, educated elite?',
    'Sociolinguistic studies on intergenerational transmission and active usage rates outside of formal literary contexts during and immediately after the Haskalah period.',
    'If reachability is limited to an elite, the constraint''s coordination function is less effective than claimed for the broader populace, potentially increasing its effective extractiveness from those who bear the cultural cost without full access.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strict_reachability_ambiguity, empirical, 'The extent of intergenerational reach and accessibility of literary Hebrew.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__literary_revival_reading, 1750, 1890).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1750, hebrew_living_language__literary_revival_reading, theater_ratio, 1750, 0.05).
narrative_ontology:measurement(hebr_tr_t1778, hebrew_living_language__literary_revival_reading, theater_ratio, 1778, 0.05).
narrative_ontology:measurement(hebr_tr_t1806, hebrew_living_language__literary_revival_reading, theater_ratio, 1806, 0.05).
narrative_ontology:measurement(hebr_tr_t1834, hebrew_living_language__literary_revival_reading, theater_ratio, 1834, 0.05).
narrative_ontology:measurement(hebr_tr_t1862, hebrew_living_language__literary_revival_reading, theater_ratio, 1862, 0.05).
narrative_ontology:measurement(hebr_tr_t1890, hebrew_living_language__literary_revival_reading, theater_ratio, 1890, 0.05).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1750, hebrew_living_language__literary_revival_reading, base_extractiveness, 1750, 0.1).
narrative_ontology:measurement(hebr_be_t1778, hebrew_living_language__literary_revival_reading, base_extractiveness, 1778, 0.11).
narrative_ontology:measurement(hebr_be_t1806, hebrew_living_language__literary_revival_reading, base_extractiveness, 1806, 0.12).
narrative_ontology:measurement(hebr_be_t1834, hebrew_living_language__literary_revival_reading, base_extractiveness, 1834, 0.13).
narrative_ontology:measurement(hebr_be_t1862, hebrew_living_language__literary_revival_reading, base_extractiveness, 1862, 0.14).
narrative_ontology:measurement(hebr_be_t1890, hebrew_living_language__literary_revival_reading, base_extractiveness, 1890, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1750, hebrew_living_language__literary_revival_reading, suppression_requirement, 1750, 0.1).
narrative_ontology:measurement(hebr_su_t1778, hebrew_living_language__literary_revival_reading, suppression_requirement, 1778, 0.1).
narrative_ontology:measurement(hebr_su_t1806, hebrew_living_language__literary_revival_reading, suppression_requirement, 1806, 0.1).
narrative_ontology:measurement(hebr_su_t1834, hebrew_living_language__literary_revival_reading, suppression_requirement, 1834, 0.1).
narrative_ontology:measurement(hebr_su_t1862, hebrew_living_language__literary_revival_reading, suppression_requirement, 1862, 0.1).
narrative_ontology:measurement(hebr_su_t1890, hebrew_living_language__literary_revival_reading, suppression_requirement, 1890, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__literary_revival_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_living_language__literary_revival_reading, hebrew_living_language__native_generation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'hebrew_living_language' kernel, focusing on literary production. It is linked to other readings that emphasize liturgical continuity and native speech generation, as they all address the same core question of Hebrew's vitality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
