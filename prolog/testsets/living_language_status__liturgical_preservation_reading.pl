% ============================================================================
% CONSTRAINT STORY: living_language_status__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__liturgical_preservation_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: living_language_status__liturgical_preservation_reading
 *   human_readable: Living Language Status via Liturgical Preservation
 *   domain: sociolinguistics/religious_studies/nationalism
 *
 * SUMMARY:
 *   The liturgical-preservation reading claims that a language is living if
 *   and only if its sacred texts are continuously recited, studied, and used
 *   in ritual — that preservation through authorized religious transmission
 *   is sufficient to constitute vitality without native speakers or
 *   vernacular innovation. This reading emerged as the dominant framework for
 *   understanding a particular ancient language across two millennia of
 *   diaspora and ritual continuity, where native speakers had gone extinct.
 *   It vested interpretive authority in the rabbinical class and
 *   delegitimized secular linguistic innovation as corruption. The constraint
 *   is ONE READING of a contested kernel (living_language_status); sibling
 *   readings — native_generation_reading and literary_continuity_reading —
 *   advance structurally different definitions of what counts as linguistic
 *   vitality. This story models ONLY the liturgical-preservation reading as a
 *   clean ε-invariant constraint with its own beneficiary/victim structure,
 *   extraction profile, and authority grounding.
 *
 * KEY AGENTS:
 *   - Rabbinical interpretive authority: Institutional custodian; defines authentic recitation and ritual use; monopoly on sacred interpretation preserved by the constraint
 *   - Secular speech community: Organized constituency; uses language in journalism, literature, everyday speech; delegitimized as desecrators under this reading
 *   - Vernacular innovation constituencies: Moderate-power speakers creating new linguistic forms; constrained exit; benefit from language as symbol but pay cost of delegitimization
 *   - Diaspora communities: Beneficiary; liturgical preservation provides cultural and spiritual anchor across geographic dispersion and generational discontinuity
 *   - Native speaker revitalization projects: Observer/analyst; measure vitality by generational transmission rather than ritual; structurally displaced by the liturgical reading
 *   - Academic linguistics: Excluded; their empirical criteria for language vitality are treated as irrelevant to the spiritual definition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__liturgical_preservation_reading, 0.62).
domain_priors:suppression_score(living_language_status__liturgical_preservation_reading, 0.71).
domain_priors:theater_ratio(living_language_status__liturgical_preservation_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__liturgical_preservation_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__liturgical_preservation_reading, "Living Language Status via Liturgical Preservation").
narrative_ontology:topic_domain(living_language_status__liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism").

domain_priors:requires_active_enforcement(living_language_status__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__liturgical_preservation_reading, 'eb11b21e-6de6-48f0-82e2-934cae0a0eaf').
narrative_ontology:cs_kernel_codification('eb11b21e-6de6-48f0-82e2-934cae0a0eaf', formalized).
narrative_ontology:cs_authority_grounding('eb11b21e-6de6-48f0-82e2-934cae0a0eaf', lineage).
narrative_ontology:cs_interpretation_layer_present('eb11b21e-6de6-48f0-82e2-934cae0a0eaf').
narrative_ontology:cs_reading_relation('eb11b21e-6de6-48f0-82e2-934cae0a0eaf', living_language_status__native_generation_reading, coexists_with).
narrative_ontology:cs_reading_relation('eb11b21e-6de6-48f0-82e2-934cae0a0eaf', living_language_status__literary_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('eb11b21e-6de6-48f0-82e2-934cae0a0eaf', foundational, canonical_text_sufficiency).
narrative_ontology:cs_axiom_status(canonical_text_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('eb11b21e-6de6-48f0-82e2-934cae0a0eaf', canonical_text_sufficiency, deontological).
narrative_ontology:cs_axiom('eb11b21e-6de6-48f0-82e2-934cae0a0eaf', foundational, authorized_transmission_doctrine).
narrative_ontology:cs_axiom_status(authorized_transmission_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('eb11b21e-6de6-48f0-82e2-934cae0a0eaf', authorized_transmission_doctrine, conventional).
narrative_ontology:cs_reference_frame('eb11b21e-6de6-48f0-82e2-934cae0a0eaf', diaspora_preservation_necessity).
narrative_ontology:cs_drift_state('eb11b21e-6de6-48f0-82e2-934cae0a0eaf', post_native_speaker_revival, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('eb11b21e-6de6-48f0-82e2-934cae0a0eaf', '').
narrative_ontology:cs_kernel_id(living_language_status__liturgical_preservation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, rabbinical_interpretive_authority).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, secular_speech_community).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, vernacular_innovation_constituencies).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__liturgical_preservation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(living_language_status__liturgical_preservation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__liturgical_preservation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(living_language_status__liturgical_preservation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(living_language_status__liturgical_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.62 at interval end) because the constraint coordinates a genuine function — maintaining religious meaning and identity across diaspora — while simultaneously extracting authority and legitimacy from speakers and innovators. The extraction accelerates over the interval (0.48 to 0.62) as the constraint is challenged by native-speaker revival movements whose success undermines the premise that liturgical preservation alone suffices for vitality. Suppression is substantial (0.71) because the constraint's persistence depends on actively excluding and delegitimizing secular linguistic innovation and native-speaker frameworks. Theater ratio is high-moderate (0.48) because as native speakers became available, the performance dimension of the constraint increased — maintaining the liturgical definition required increasing pedagogical and institutional theater (teaching recitation as the marker of vitality rather than acknowledging native speech). The measurement trajectory shows suppression hardening (0.55 to 0.71) as native-speaker revival threatened the constraint's structural basis.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinical authority's seat, the constraint is an essential coordination mechanism for diaspora religious community and a sacred duty to preserve canonical text intact. From the secular speech community's seat, the same constraint operates as an institutional power grab that delegitimizes their language use and reserves authority to priestly interpreters. From native-speaker revitalization projects, the constraint is a historically important preservation mechanism that has been SUPERSEDED by successful native-generation revival — they no longer need it and see it as an obstacle to natural linguistic development. These divergent readings are not perceptual disagreements — they reflect real structural asymmetry: beneficiary seats and payer seats compute different types from the same metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinical authority sits at d ≈ 0.0–0.2 (full beneficiary): they collect monopoly on interpretation, institutional prestige, and religious authority; they have high power and can exit to administrative roles within or outside religious institutions. Secular speech community sits at d ≈ 0.7–0.9 (near target): they bear the delegitimization cost, constrained exit (accepting non-recognition or leaving the language entirely), and institutional marginalization. Diaspora communities sit at d ≈ 0.3–0.4 (slight beneficiary): they depend on the constraint for cultural identity continuity, but they benefit from the coordination rather than the extraction. The directionality derivation here is straightforward: beneficiaries identified with institutional authority, victims identified with secular innovation constituencies, exit options clearly asymmetric.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (diaspora preservation without native speakers) is LIVE in the rabbinical reading but DEAD in the native-generation reading. This mismatch is the core mandatrophy risk: the constraint was built to solve a specific historical problem (centuries of diaspora and native-speaker extinction) that was actually SOLVED in the 20th century through successful native-speaker revival. The constraint persists not because the founding problem persists but because the beneficiary class (rabbinical authority) captures enough institutional power to maintain it despite the solution. A piton-level risk: the constraint's primary function has atrophied (preservation-without-speakers is no longer necessary when native speakers exist) but it persists through institutional theater and enforced exclusion of alternative frameworks. The theater ratio rising over the interval (0.32 to 0.48) reflects this atrophy: increasing pedagogical performance is required to maintain the constraint as native speakers and secular innovation make the founding problem increasingly irrelevant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'Is the measured extraction (0.62) the cost of coordinating diaspora religious community across centuries, or is it the institutional rent extracted by the rabbinical class leveraging the coordination function?',
    'Comparative analysis of alternative diaspora-preservation mechanisms (secular literary traditions, nationalist revival movements, educational systems) and their efficiency relative to the liturgical-transmission model. If alternative mechanisms could achieve comparable diaspora cohesion at lower institutional cost, the extraction is rent; if all diaspora-preservation mechanisms incur comparable costs, the extraction is coordination overhead.',
    'If the extraction is primarily rent, the constraint is a snare wearing a coordination mask; the beneficiary authority persists by preventing comparison with alternatives. If the extraction is primarily coordination overhead, the tangled-rope classification stands but the remedial framing shifts from ''break the monopoly'' to ''accept the cost of diaspora preservation'' or ''invest in native-speaker alternatives.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the constraint''s extraction price tracks the cost of coordination or the market power of the beneficiary.').

omega_variable(
    suppression_internalization_ambiguity,
    'Is the measured suppression (0.71) structural (institutional exclusion of secular speakers and innovators from authority positions) or internalized (secular speakers have come to believe their language use does not count as authentic vitality)?',
    'Qualitative study of secular speaker attitudes pre- and post-native-speaker revival; survey of whether secular speakers who became native-speaker advocates experienced suppression as internal shame or external barrier removal. If speakers who exit the secular community and enter native-speaker circles report suppression-reversal, the mechanism was partly structural; if they report unchanged internalized shame, the suppression is substantially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — secular speakers continue to devalue their own language use even after institutional barriers are removed. If structural, remedial paths focus on institutional inclusion rather than attitude change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_ambiguity, empirical, 'Whether suppression is structural institutional exclusion or internalized by secular speakers.').

omega_variable(
    kernel_reading_alternative_framings,
    'What alternative framings of the liturgical_preservation_reading would produce different ε values or beneficiary/victim structures?',
    'Formal specification of the reading''s core commitments and exploration of whether the beneficiary could be reframed as the diaspora community rather than the rabbinical authority, and the victims as the religious tradition itself (losing relevance as native speakers emerge). If the rabbinical authority is reframed as custodian-payer rather than beneficiary-agenda-setter, the extraction reverses direction.',
    'Alternative framings might reclassify the constraint as a rope (genuine coordination without extractive asymmetry) or as a piton (a custodial function maintained by institutional theater rather than institutional capture). This omega documents that the reading''s classification is frame-dependent and routes the framing choice to explicit deliberation rather than implicit authoring assumptions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_alternative_framings, conceptual, 'Committer-frame ambiguity: whether the beneficiary and victim identities follow necessarily from the reading''s core premise or depend on framing choices.').

omega_variable(
    native_speaker_revival_as_reading_supersession,
    'Does the successful native-speaker revival in the 20th century SUPERSEDE the liturgical-preservation reading (making it historically obsolete) or does it CREATE a new constraint (native_generation_reading) that coexists with and competes against the liturgical reading?',
    'Historical analysis of whether the liturgical reading was formally abandoned by its beneficiary class once native speakers emerged, or whether it was defensively redefined to accommodate native speech while preserving rabbinical authority. If actively abandoned, the reading is historically resolved; if redefined and defended, it persists as a live position despite its founding problem being solved.',
    'If superseded, the constraint approaches mandatrophy resolution and should be reclassified as a historical artifact. If still live and defended, the constraint remains a site of institutional power struggle and the theater ratio rise is accurately measuring the performance cost of defending an obsolete definition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(native_speaker_revival_as_reading_supersession, empirical, 'Whether native-speaker revival resolved or transformed the liturgical-preservation reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__liturgical_preservation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t0, living_language_status__liturgical_preservation_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(livi_tr_t14, living_language_status__liturgical_preservation_reading, theater_ratio, 14, 0.36).
narrative_ontology:measurement(livi_tr_t28, living_language_status__liturgical_preservation_reading, theater_ratio, 28, 0.4).
narrative_ontology:measurement(livi_tr_t42, living_language_status__liturgical_preservation_reading, theater_ratio, 42, 0.44).
narrative_ontology:measurement(livi_tr_t70, living_language_status__liturgical_preservation_reading, theater_ratio, 70, 0.47).
narrative_ontology:measurement(livi_tr_t100, living_language_status__liturgical_preservation_reading, theater_ratio, 100, 0.48).

% Extraction over time
narrative_ontology:measurement(livi_be_t0, living_language_status__liturgical_preservation_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(livi_be_t14, living_language_status__liturgical_preservation_reading, base_extractiveness, 14, 0.52).
narrative_ontology:measurement(livi_be_t28, living_language_status__liturgical_preservation_reading, base_extractiveness, 28, 0.57).
narrative_ontology:measurement(livi_be_t42, living_language_status__liturgical_preservation_reading, base_extractiveness, 42, 0.6).
narrative_ontology:measurement(livi_be_t70, living_language_status__liturgical_preservation_reading, base_extractiveness, 70, 0.62).
narrative_ontology:measurement(livi_be_t100, living_language_status__liturgical_preservation_reading, base_extractiveness, 100, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t0, living_language_status__liturgical_preservation_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(livi_su_t14, living_language_status__liturgical_preservation_reading, suppression_requirement, 14, 0.61).
narrative_ontology:measurement(livi_su_t28, living_language_status__liturgical_preservation_reading, suppression_requirement, 28, 0.65).
narrative_ontology:measurement(livi_su_t42, living_language_status__liturgical_preservation_reading, suppression_requirement, 42, 0.68).
narrative_ontology:measurement(livi_su_t70, living_language_status__liturgical_preservation_reading, suppression_requirement, 70, 0.7).
narrative_ontology:measurement(livi_su_t100, living_language_status__liturgical_preservation_reading, suppression_requirement, 100, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__liturgical_preservation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(living_language_status__liturgical_preservation_reading, 0.12).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, living_language_status__native_generation_reading).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, living_language_status__literary_continuity_reading).

% DUAL FORMULATION NOTE:
% The living_language_status kernel decomposes into three structurally distinct constraint readings. Each reading instantiates a different definition of linguistic vitality and produces a different ε value, beneficiary/victim structure, and authority grounding. The liturgical_preservation_reading (this story) vests authority in rabbinical interpreters and treats ritual continuity as sufficient vitality. The native_generation_reading vests vitality in native speakers and treats liturgical recitation as corpse preservation. The literary_continuity_reading vests vitality in productive innovation and treats both ritual and native speech as secondary to creative use. These readings coexist as live institutional positions held by different parties and compete for resources and legitimacy. They are linked via network.affects_constraints because success of any one reading (institutional enforcement, legal recognition, educational dominance) structurally constrains the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
