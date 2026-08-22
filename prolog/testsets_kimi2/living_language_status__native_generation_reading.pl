% ============================================================================
% CONSTRAINT STORY: living_language_status__native_generation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__native_generation_reading, []).

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
 *   constraint_id: living_language_status__native_generation_reading
 *   human_readable: Native-Speaker-Only Language Vitality Criterion
 *   domain: sociolinguistic/political
 *
 * SUMMARY:
 *   This constraint instantiates the native_generation_reading of the
 *   contested kernel living_language_status. It asserts that a language is
 *   living exclusively when transmitted generationally as a mother tongue in
 *   daily life, relegating liturgical recitation to 'corpse preservation.'
 *   The constraint operates through national linguistic authorities that
 *   codify census and educational standards, channeling resources and
 *   sovereignty claims toward native-speaker communities and secular
 *   nationalist movements while extracting legitimacy from liturgical-only
 *   communities. The authored metrics reflect the constraint's structural
 *   operation as a coordination mechanism for language policy that
 *   asymmetrically delegitimizes religious continuity.
 *
 * KEY AGENTS:
 *   - secular_nationalist_movement: Primary beneficiary (organized/national) â collects sovereignty legitimacy from the living-language claim.
 *   - liturgical_only_communities: Primary target (moderate/identity_locked) â bears delegitimization and resource denial.
 *   - national_linguistic_authorities: Agenda-setter (institutional/national) â administers the vitality criterion and enforces its boundary.
 *   - native_speaker_communities: Secondary beneficiary (moderate/regional) â receives institutional support and cultural prestige.
 *   - religious_scholars: Excluded voice (moderate/national) â would contest the mother-tongue exclusivity but is kept out of standard-setting.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__native_generation_reading, 0.62).
domain_priors:suppression_score(living_language_status__native_generation_reading, 0.68).
domain_priors:theater_ratio(living_language_status__native_generation_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__native_generation_reading, "Native-Speaker-Only Language Vitality Criterion").
narrative_ontology:topic_domain(living_language_status__native_generation_reading, "sociolinguistic/political").

domain_priors:requires_active_enforcement(living_language_status__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__native_generation_reading, 'a2549efe-899b-4437-a56a-0accc357d9db').
narrative_ontology:cs_kernel_codification('a2549efe-899b-4437-a56a-0accc357d9db', formalized).
narrative_ontology:cs_authority_grounding('a2549efe-899b-4437-a56a-0accc357d9db', expertise).
narrative_ontology:cs_interpretation_layer_present('a2549efe-899b-4437-a56a-0accc357d9db').
narrative_ontology:cs_reading_relation('a2549efe-899b-4437-a56a-0accc357d9db', living_language_status__liturgical_preservation_reading, forecloses).
narrative_ontology:cs_reading_relation('a2549efe-899b-4437-a56a-0accc357d9db', living_language_status__literary_continuity_reading, forecloses).
narrative_ontology:cs_axiom('a2549efe-899b-4437-a56a-0accc357d9db', foundational, native_transmission_necessary_for_vitality).
narrative_ontology:cs_axiom_status(native_transmission_necessary_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('a2549efe-899b-4437-a56a-0accc357d9db', native_transmission_necessary_for_vitality, conventional).
narrative_ontology:cs_axiom('a2549efe-899b-4437-a56a-0accc357d9db', foundational, liturgical_preservation_is_non_vital).
narrative_ontology:cs_axiom_status(liturgical_preservation_is_non_vital, holdable).
narrative_ontology:cs_axiom_grounding('a2549efe-899b-4437-a56a-0accc357d9db', liturgical_preservation_is_non_vital, conventional).
narrative_ontology:cs_reference_frame('a2549efe-899b-4437-a56a-0accc357d9db', mother_tongue_vitality_framework).
narrative_ontology:cs_drift_state('a2549efe-899b-4437-a56a-0accc357d9db', contemporary_revitalization_policy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a2549efe-899b-4437-a56a-0accc357d9db', '').
narrative_ontology:cs_kernel_id(living_language_status__native_generation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, secular_nationalist_movement).
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, native_speaker_communities).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, liturgical_only_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gains political legitimacy and sovereignty claims by demonstrating that the national language is transmitted as a living mother tongue. Uses the native-speaker criterion to distinguish authentic national speech from religious or diasporic relics, mobilizing linguistic vitality as a marker of statehood.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, secular_nationalist_movement, beneficiary,
    organized, generational, mobile, national).

% Maintains sacred texts, rituals, and continuous study in the language but lacks households where it is spoken as a daily mother tongue. Bears the delegitimizing label that their language is dead or a corpse despite unbroken recitational continuity. Their religious identity is fused with this linguistic practice; accepting the native-speaker frame would dissolve their claim to preserving the language.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, liturgical_only_communities, payer,
    moderate, civilizational, identity_locked, national).

% Administers census definitions, educational standards, and language-revitalization funding. Determines which communities qualify as living-language carriers based on native-speaker household transmission data. Enforces the boundary between liturgical preservation and vital speech through certification, policy, and research funding criteria.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, national_linguistic_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Receive state resources, educational support, and cultural prestige for transmitting the language as a mother tongue. Their daily speech is treated as the authentic site of linguistic vitality, while their communities are enrolled in nationalist narratives and monitored by language-policy metrics.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, native_speaker_communities, beneficiary,
    moderate, biographical, constrained, regional).

% Would argue that liturgical continuity and sacred textual study constitute a genuine form of linguistic life, but are excluded from sociolinguistic standard-setting bodies and state policy councils that define vitality metrics.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, religious_scholars, excluded,
    moderate, civilizational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(living_language_status__native_generation_reading, secular_nationalist_movement).
narrative_ontology:fixing_cost_class(living_language_status__native_generation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates language-revitalization policy by providing a clear, empirically observable threshold for vitality: mother-tongue transmission in daily life allows states to target scarce resources toward communities most likely to sustain intergenerational speech.
% TRANSFER_FUNCTION: Moves legitimacy, state resources, and cultural prestige from liturgical-only communities to native-speaker communities and secular nationalist movements; transfers definitional authority over life and death of languages from religious institutions to state linguistic bureaucracies.
% ABSENT_VOICES: Religious scholars and liturgical community leaders who view sacred continuity as vitality are excluded from the sociolinguistic standard-setting that certifies languages as living or dead.
% DISAPPEARANCE_RATIONALE: If the native-speaker exclusivity vanished overnight, language-protection funding would reorganize around alternative criteria (literary productivity or liturgical continuity), liturgical communities would reclaim vitality status and resource access, and nationalist movements would lose a key sovereignty marker. The classification of hundreds of community languages would shift, altering educational priorities and census categories.
% FOUNDING_PROBLEM: Determining which languages are genuinely alive for policy purposes, preventing wasteful preservation of non-viable codes, and mobilizing language as a nation-building resource in multi-ethnic states.
% FOUNDING_PROBLEM_CORROBORATION: Secular nationalist movements and state linguistic authorities attest the problem is still live, citing language shift and assimilation. Independent sociolinguists and religious historians outside the benefiting parties argue the native-speaker criterion emerged alongside 19th-century nation-state formation and conflates political legitimation with linguistic science; no neutral party corroborates the necessity of the mother-tongue exclusivity.
narrative_ontology:disappearance_verdict(living_language_status__native_generation_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__native_generation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__native_generation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(living_language_status__native_generation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__native_generation_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__native_generation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(living_language_status__native_generation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(living_language_status__native_generation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate because the constraint extracts legitimacy and status rather than material wealth; suppression (0.68) is higher because the criterion must be actively enforced through census categories and funding gatekeeping to maintain the boundary against liturgical claims. Theater_ratio (0.52) is substantial because much native-speaker policy is performative (certification rituals, census boxes) relative to actual household transmission rates. Accessibility_collapse (0.60) reflects that alternative definitions of vitality are marginalized but not eliminated; resistance (0.55) captures ongoing contestation from religious communities and dissenting linguists. Temporal measurements show all three tracked metrics rising over the interval as state infrastructure consolidated around the native-speaker standard.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (nationalist movement, native speakers) experience the constraint as a necessary standard that channels scarce resources toward genuine vitality and prevents museumification. The payer seat (liturgical communities) experiences the same constraint as an ideological erasure of their continuous practice. The engine computes this divergence from the structural data: same constraint, opposite directionality, different effective extraction. The agenda-setter seat experiences the constraint as professional expertise in action.
 *
 * DIRECTIONALITY LOGIC:
 *   The secular nationalist movement and native speaker communities are declared beneficiaries: they receive legitimacy and resources, placing their directionality near the subsidy end (low d). Liturgical-only communities are declared victims: they bear the cost of exclusion and delegitimization, placing their directionality near the full-target end (high d). National linguistic authorities administer the constraint without being its primary financial beneficiary; their directionality is derived as near-symmetric from their institutional position and analytical exit option. Religious scholars are excluded from the conversation; their absence is structural, not chosen.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy mislabeling by preserving both its genuine coordination function (solving the policy problem of where to direct language-revitalization resources) and its asymmetric extraction (delegitimizing liturgical communities to the benefit of nationalist movements). A pure rope reading would ignore the victim; a pure snare reading would ignore the real coordination problem of language decline. The Tangled Rope classification captures the hybrid: the native-speaker criterion does coordinate revitalization, but the same mechanism extracts from religious minorities by declaring their practice non-vital.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_structural_delta,
    'How would classifying liturgically maintained languages as ''living'' under a sibling reading change the beneficiary and victim structure of language policy?',
    'Comparative policy analysis of jurisdictions using liturgical versus native-speaker criteria for language protection status.',
    'Would invert directionality: liturgical communities become beneficiaries, native-speaker communities lose exclusive resource access, and nationalist legitimacy claims weaken.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Structural difference between this reading and sibling readings of the same kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the delegitimization of liturgical language communities structural (policy exclusion and funding denial) or internalized (communities accepting the ''dead language'' label)?',
    'Ethnographic study of liturgical community self-identification, resistance narratives, and psychological uptake of sociolinguistic vitality frameworks.',
    'If internalized, effective suppression is higher than structural measures suggest; if purely structural, resistance potential is higher and reversal more feasible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression in community identity.').

omega_variable(
    native_speaker_empirical_basis,
    'Does the native-speaker criterion track an objectively measurable sociolinguistic threshold, or is it a politically constructed boundary?',
    'Historical genealogy of the ''native speaker'' concept in sociolinguistics versus its deployment in nationalist policy; cross-national comparison of vitality criteria.',
    'If constructed, the constraint''s authority as expertise is partly theatrical; if objective, the extraction may be largely coordination cost rather than rent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(native_speaker_empirical_basis, conceptual, 'Whether the native-speaker threshold is empirical or constructed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__native_generation_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(native_gen_tr_t0, living_language_status__native_generation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(native_gen_tr_t12, living_language_status__native_generation_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(native_gen_tr_t24, living_language_status__native_generation_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement(native_gen_tr_t36, living_language_status__native_generation_reading, theater_ratio, 36, 0.38).
narrative_ontology:measurement(native_gen_tr_t48, living_language_status__native_generation_reading, theater_ratio, 48, 0.45).
narrative_ontology:measurement(native_gen_tr_t60, living_language_status__native_generation_reading, theater_ratio, 60, 0.52).

% Extraction over time
narrative_ontology:measurement(native_gen_be_t0, living_language_status__native_generation_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(native_gen_be_t12, living_language_status__native_generation_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(native_gen_be_t24, living_language_status__native_generation_reading, base_extractiveness, 24, 0.45).
narrative_ontology:measurement(native_gen_be_t36, living_language_status__native_generation_reading, base_extractiveness, 36, 0.52).
narrative_ontology:measurement(native_gen_be_t48, living_language_status__native_generation_reading, base_extractiveness, 48, 0.58).
narrative_ontology:measurement(native_gen_be_t60, living_language_status__native_generation_reading, base_extractiveness, 60, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(native_gen_su_t0, living_language_status__native_generation_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(native_gen_su_t12, living_language_status__native_generation_reading, suppression_requirement, 12, 0.35).
narrative_ontology:measurement(native_gen_su_t24, living_language_status__native_generation_reading, suppression_requirement, 24, 0.45).
narrative_ontology:measurement(native_gen_su_t36, living_language_status__native_generation_reading, suppression_requirement, 36, 0.55).
narrative_ontology:measurement(native_gen_su_t48, living_language_status__native_generation_reading, suppression_requirement, 48, 0.62).
narrative_ontology:measurement(native_gen_su_t60, living_language_status__native_generation_reading, suppression_requirement, 60, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__native_generation_reading, identity_coordination).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, liturgical_preservation_reading).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, literary_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the living_language_status kernel. Sibling readings (liturgical_preservation_reading, literary_continuity_reading) model the same kernel with different epsilon values, stakeholders, and classifications. The epsilon-invariance principle requires decomposition into separate stories when the core claim changes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
