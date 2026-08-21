% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__native_generative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__native_generative, []).

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
 *   constraint_id: hebrew_continuity__native_generative
 *   human_readable: Hebrew as Native Generative Language
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents the 'native generative' reading of Hebrew's
 *   continuity, asserting that Hebrew truly lives only through the intuitive,
 *   daily use of native speakers. This ideology, central to modern Hebrew
 *   revitalization, actively reconstructs the language through lexical
 *   expansion and phonological standardization, while simultaneously
 *   invalidating other forms of Hebrew (e.g., liturgical, pidginized) as
 *   'dead' or inauthentic. The claimed type is 'rope' from the perspective of
 *   its proponents, who see it as coordinating a vital national project, but
 *   the metrics reflect its highly extractive and suppressive operation on
 *   non-native forms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__native_generative, 0.85).
domain_priors:suppression_score(hebrew_continuity__native_generative, 0.9).
domain_priors:theater_ratio(hebrew_continuity__native_generative, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, extractiveness, 0.85).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__native_generative, rope).
narrative_ontology:human_readable(hebrew_continuity__native_generative, "Hebrew as Native Generative Language").
narrative_ontology:topic_domain(hebrew_continuity__native_generative, "sociolinguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_continuity__native_generative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__native_generative, '22286073-cd0a-4a4c-a961-bab9525af91d').
narrative_ontology:cs_kernel_codification('22286073-cd0a-4a4c-a961-bab9525af91d', formalized).
narrative_ontology:cs_authority_grounding('22286073-cd0a-4a4c-a961-bab9525af91d', expertise).
narrative_ontology:cs_interpretation_layer_present('22286073-cd0a-4a4c-a961-bab9525af91d').
narrative_ontology:cs_reading_relation('22286073-cd0a-4a4c-a961-bab9525af91d', hebrew_continuity__liturgical_preservation, forecloses).
narrative_ontology:cs_reading_relation('22286073-cd0a-4a4c-a961-bab9525af91d', hebrew_continuity__bridge_pidginized, forecloses).
narrative_ontology:cs_axiom('22286073-cd0a-4a4c-a961-bab9525af91d', foundational, native_speaker_intuition_is_sole_arbiter_of_language_life).
narrative_ontology:cs_axiom_status(native_speaker_intuition_is_sole_arbiter_of_language_life, holdable).
narrative_ontology:cs_axiom_grounding('22286073-cd0a-4a4c-a961-bab9525af91d', native_speaker_intuition_is_sole_arbiter_of_language_life, conventional).
narrative_ontology:cs_axiom('22286073-cd0a-4a4c-a961-bab9525af91d', foundational, language_must_be_generative_for_vitality).
narrative_ontology:cs_axiom_status(language_must_be_generative_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('22286073-cd0a-4a4c-a961-bab9525af91d', language_must_be_generative_for_vitality, conventional).
narrative_ontology:cs_reference_frame('22286073-cd0a-4a4c-a961-bab9525af91d', modern_hebrew_revival_ideology).
narrative_ontology:cs_drift_state('22286073-cd0a-4a4c-a961-bab9525af91d', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('22286073-cd0a-4a4c-a961-bab9525af91d', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__native_generative, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, native_hebrew_speakers).
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, hebrew_language_academicians).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, liturgical_hebrew_communities).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, diaspora_jewish_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define, standardize, and promote modern Hebrew usage, actively shaping the linguistic landscape and validating native generative use as the sole criterion for a 'living' language. They benefit from the authority derived from this role.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, hebrew_language_academicians, agenda_setter,
    institutional, generational, arbitrage, national).

% Their daily, intuitive use of Hebrew is validated as the standard, granting them linguistic and cultural primacy. They are the primary beneficiaries of the language's vitality and status.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, native_hebrew_speakers, beneficiary,
    powerful, biographical, mobile, national).

% Their traditional, ritualistic use of Hebrew is deemed 'dead' or 'inauthentic' by this standard, leading to cultural devaluation and marginalization within the broader linguistic discourse. Their identity is deeply tied to their form of Hebrew.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, liturgical_hebrew_communities, payer,
    powerless, generational, identity_locked, global).

% Their efforts to use Hebrew as a contact or bridge language, often in pidginized forms, are dismissed as not truly 'living' Hebrew, creating linguistic barriers and undermining their cultural connection to the language.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, diaspora_jewish_communities, payer,
    moderate, biographical, constrained, global).

% Analyze the social and linguistic dynamics of Hebrew revitalization, often critiquing purist ideologies and the exclusionary aspects of defining language vitality solely by native generative use.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, sociolinguists, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, unified standard for what constitutes 'living' Hebrew, facilitating communication, cultural identity, and national cohesion for native speakers in Israel.
% TRANSFER_FUNCTION: Transfers linguistic authority, cultural legitimacy, and educational resources from traditional/diaspora forms of Hebrew to modern native usage, effectively extracting cultural capital and identity from non-native speakers.
% ABSENT_VOICES: Scholars of liturgical Hebrew and proponents of pidginized Hebrew are structurally excluded from the authoritative discourse on Hebrew's vitality; they would argue for a more pluralistic understanding of language life.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the linguistic hierarchy would collapse, allowing for a more pluralistic understanding of Hebrew's vitality. This would empower traditional and diaspora forms, leading to a reorganization of educational priorities, cultural funding, and national identity discourse around language.
% FOUNDING_PROBLEM: The perceived 'death' or 'stagnation' of Hebrew as a spoken language, confined to religious texts and limited ritual use, leading to a desire for its full revitalization as a modern national language for a nascent state.
% FOUNDING_PROBLEM_CORROBORATION: Supported by historical accounts of Zionist language movements and contemporary Israeli cultural institutions. Critiqued by some sociolinguists and religious scholars who argue for the continuous vitality of liturgical Hebrew, indicating a contested status from outside the benefiting parties.
narrative_ontology:disappearance_verdict(hebrew_continuity__native_generative, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__native_generative, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__native_generative, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(hebrew_continuity__native_generative, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__native_generative, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__native_generative_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_continuity__native_generative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_continuity__native_generative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because this reading imposes a strict definition of 'living' Hebrew, devaluing and marginalizing other forms and their associated cultural capital. Suppression is very high (0.90) as it requires active linguistic policing, educational systems, and cultural narratives to enforce this narrow definition and suppress alternatives. Theater ratio is low (0.10) because the focus is on genuine, functional language use, not mere performance. Accessibility collapse is high (0.88) as it creates a very narrow, difficult-to-access path for what counts as 'true' Hebrew. Resistance is high (0.70) from communities whose linguistic practices are invalidated.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of native Hebrew speakers and academicians, this constraint is a successful coordination effort to revitalize a language. From the perspective of liturgical and diaspora communities, it is a highly extractive and suppressive force that devalues their heritage and identity. The engine's computation of per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Hebrew language academicians and native Hebrew speakers are the primary beneficiaries, gaining authority and cultural primacy. Liturgical Hebrew communities and diaspora Jewish communities are the primary victims, experiencing devaluation and marginalization of their linguistic practices. Sociolinguists act as analytical observers, often critiquing the constraint's exclusionary aspects.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    linguistic_vitality_definition,
    'Is ''living language'' solely defined by native generative use, or can other forms (liturgical, pidginized) also constitute vitality?',
    'Cross-cultural linguistic studies on language vitality, and sociological analysis of community self-identification with various forms of Hebrew.',
    'If vitality is recognized in other forms, the extractiveness and suppression of this constraint would be re-evaluated downward, potentially reclassifying it from a Snare to a Tangled Rope or even a Rope, as its exclusionary aspects would be seen as less justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(linguistic_vitality_definition, conceptual, 'Ambiguity in the definition of language vitality and its impact on the constraint''s classification.').

omega_variable(
    cultural_impact_of_invalidation,
    'What is the actual cultural and psychological cost to communities whose Hebrew is deemed ''dead'' or ''inauthentic'' by this standard?',
    'Ethnographic studies, surveys, and qualitative interviews with members of liturgical and diaspora communities regarding their linguistic identity and sense of belonging.',
    'Quantifying the cultural and psychological costs would strengthen the victim declaration and potentially increase the perceived extractiveness and suppression, reinforcing a Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_impact_of_invalidation, empirical, 'Measuring the real-world impact of linguistic invalidation on victim communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__native_generative, 1900, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1900, hebrew_continuity__native_generative, theater_ratio, 1900, 0.2).
narrative_ontology:measurement(hebr_tr_t1925, hebrew_continuity__native_generative, theater_ratio, 1925, 0.15).
narrative_ontology:measurement(hebr_tr_t1950, hebrew_continuity__native_generative, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(hebr_tr_t1975, hebrew_continuity__native_generative, theater_ratio, 1975, 0.1).
narrative_ontology:measurement(hebr_tr_t2000, hebrew_continuity__native_generative, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(hebr_tr_t2025, hebrew_continuity__native_generative, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1900, hebrew_continuity__native_generative, base_extractiveness, 1900, 0.4).
narrative_ontology:measurement(hebr_be_t1925, hebrew_continuity__native_generative, base_extractiveness, 1925, 0.6).
narrative_ontology:measurement(hebr_be_t1950, hebrew_continuity__native_generative, base_extractiveness, 1950, 0.75).
narrative_ontology:measurement(hebr_be_t1975, hebrew_continuity__native_generative, base_extractiveness, 1975, 0.8).
narrative_ontology:measurement(hebr_be_t2000, hebrew_continuity__native_generative, base_extractiveness, 2000, 0.83).
narrative_ontology:measurement(hebr_be_t2025, hebrew_continuity__native_generative, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1900, hebrew_continuity__native_generative, suppression_requirement, 1900, 0.5).
narrative_ontology:measurement(hebr_su_t1925, hebrew_continuity__native_generative, suppression_requirement, 1925, 0.7).
narrative_ontology:measurement(hebr_su_t1950, hebrew_continuity__native_generative, suppression_requirement, 1950, 0.8).
narrative_ontology:measurement(hebr_su_t1975, hebrew_continuity__native_generative, suppression_requirement, 1975, 0.85).
narrative_ontology:measurement(hebr_su_t2000, hebrew_continuity__native_generative, suppression_requirement, 2000, 0.88).
narrative_ontology:measurement(hebr_su_t2025, hebrew_continuity__native_generative, suppression_requirement, 2025, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__native_generative, identity_coordination).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, hebrew_continuity__liturgical_preservation).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, hebrew_continuity__bridge_pidginized).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'hebrew_continuity' kernel, each representing a distinct structural claim about how Hebrew 'lives'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
