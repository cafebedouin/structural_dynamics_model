% ============================================================================
% CONSTRAINT STORY: living_language_status__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   constraint_id: living_language_status__liturgical_preservation_reading
 *   human_readable: Living Language Status: Liturgical Preservation Reading
 *   domain: sociolinguistic/religious/nationalism
 *
 * SUMMARY:
 *   This constraint instantiates the liturgical_preservation_reading of the
 *   living_language_status kernel: a language is living if its sacred texts
 *   are continuously recited, studied, and used in ritual. The reading is
 *   contested by two siblings â native_generation_reading (vitality
 *   requires mother-tongue transmission) and literary_continuity_reading
 *   (vitality requires productive literary use). Under this reading,
 *   rabbinical authority retains an interpretive monopoly, liturgical
 *   practitioners receive coordinated religious continuity, and the secular
 *   speech community is delegitimized as desecrators. The constraint is a
 *   tangled rope: genuine coordination of diaspora religious practice is
 *   braided with asymmetric extraction from secular speakers.
 *
 * KEY AGENTS:
 *   - rabbinical_authority: Agenda-setter and primary beneficiary (institutional/civilizational/identity_locked) â captures interpretive monopoly and defines legitimacy.
 *   - liturgical_practitioners: Coordinated beneficiary (organized/generational/identity_locked) â gains communal meaning, bound to liturgical register.
 *   - secular_speech_community: Primary target/payer (moderate/biographical/constrained) â bears delegitimization and exclusion from vitality status.
 *   - haskalah_intellectuals: Excluded voice (moderate/biographical/constrained) â argues for literary vitality, structurally silenced.
 *   - sociolinguistic_observers: Analytical observer (analytical/generational/analytical) â external classifier tracking the contest.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__liturgical_preservation_reading, 0.3).
domain_priors:suppression_score(living_language_status__liturgical_preservation_reading, 0.55).
domain_priors:theater_ratio(living_language_status__liturgical_preservation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__liturgical_preservation_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__liturgical_preservation_reading, "Living Language Status: Liturgical Preservation Reading").
narrative_ontology:topic_domain(living_language_status__liturgical_preservation_reading, "sociolinguistic/religious/nationalism").

domain_priors:requires_active_enforcement(living_language_status__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__liturgical_preservation_reading, '7c5d7182-05ab-4eb3-8bc1-cae9abb48f9e').
narrative_ontology:cs_kernel_codification('7c5d7182-05ab-4eb3-8bc1-cae9abb48f9e', fixed_text).
narrative_ontology:cs_authority_grounding('7c5d7182-05ab-4eb3-8bc1-cae9abb48f9e', lineage).
narrative_ontology:cs_interpretation_layer_present('7c5d7182-05ab-4eb3-8bc1-cae9abb48f9e').
narrative_ontology:cs_reading_relation('7c5d7182-05ab-4eb3-8bc1-cae9abb48f9e', living_language_status__native_generation_reading, forecloses).
narrative_ontology:cs_reading_relation('7c5d7182-05ab-4eb3-8bc1-cae9abb48f9e', living_language_status__literary_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('7c5d7182-05ab-4eb3-8bc1-cae9abb48f9e', foundational, liturgical_transmission_suffices_for_linguistic_vitality).
narrative_ontology:cs_axiom_status(liturgical_transmission_suffices_for_linguistic_vitality, holdable).
narrative_ontology:cs_axiom_grounding('7c5d7182-05ab-4eb3-8bc1-cae9abb48f9e', liturgical_transmission_suffices_for_linguistic_vitality, theological).
narrative_ontology:cs_axiom('7c5d7182-05ab-4eb3-8bc1-cae9abb48f9e', foundational, secular_generative_use_desecrates_sacred_tongue).
narrative_ontology:cs_axiom_status(secular_generative_use_desecrates_sacred_tongue, holdable).
narrative_ontology:cs_axiom_grounding('7c5d7182-05ab-4eb3-8bc1-cae9abb48f9e', secular_generative_use_desecrates_sacred_tongue, deontological).
narrative_ontology:cs_reference_frame('7c5d7182-05ab-4eb3-8bc1-cae9abb48f9e', classical_liturgical_authority).
narrative_ontology:cs_drift_state('7c5d7182-05ab-4eb3-8bc1-cae9abb48f9e', post_haskalah_secularization, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('7c5d7182-05ab-4eb3-8bc1-cae9abb48f9e', '').
narrative_ontology:cs_kernel_id(living_language_status__liturgical_preservation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, rabbinical_authority).
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, liturgical_practitioners).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, secular_speech_community).
narrative_ontology:constraint_vindicates(living_language_status__liturgical_preservation_reading, liturgical_sufficiency_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the interpretive tradition that defines legitimate language use through liturgical performance and textual study. Derives institutional authority from the claim that sacred Hebrew remains alive precisely through this transmission. Cannot abandon the liturgical-framing claim without dissolving the authority structure itself.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, rabbinical_authority, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(living_language_status__liturgical_preservation_reading, rabbinical_authority, beneficiary).

% Comprises modern Hebrew speakers, nationalists, and secular educators who use the language for daily life, literature, and statecraft. Their speech is delegitimized as desecration or irrelevance under the liturgical frame, denying them recognition as legitimate bearers of a living tongue. Exit means abandoning the language or accepting subordinate status.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, secular_speech_community, payer,
    moderate, biographical, constrained, national).

% Engage in daily or weekly ritual recitation, prayer, and study of sacred texts in the liturgical language. Receive coordinated access to communal religious meaning and continuity, but are bound to the authoritative interpretation and liturgical register; their linguistic practice is restricted to sacred contexts.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, liturgical_practitioners, beneficiary,
    organized, generational, identity_locked, national).

% Advocate for secular Hebrew literature and Enlightenment values. Structurally excluded from the liturgical legitimacy framework; their literary production is treated as evidence of secular desecration rather than linguistic vitality.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, haskalah_intellectuals, excluded,
    moderate, biographical, constrained, national).

% Academic analysts who study language vitality and religious nationalism. They classify the liturgical-preservation claim as a socio-political construct rather than a natural linguistic fact, and track how the definition of 'living language' is contested between religious and secular frameworks.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, sociolinguistic_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(living_language_status__liturgical_preservation_reading, rabbinical_authority).
narrative_ontology:fixing_cost_class(living_language_status__liturgical_preservation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates dispersed diaspora communities around a fixed sacred corpus by fixing a shared liturgical language for ritual, prayer, and study, ensuring religious and textual continuity without a shared territorial vernacular.
% TRANSFER_FUNCTION: Moves linguistic legitimacy and interpretive authority from secular generative use to rabbinical gatekeepers of liturgical transmission, and delegitimizes the secular speech community as desecrators or irrelevant to vitality.
% ABSENT_VOICES: Secular Zionists, modern Hebrew speakers, and Haskalah intellectuals who argue that native generational transmission or new literary production are necessary for vitality are structurally excluded; their definitions are ruled out as apostasy or ignorance within the liturgical frame.
% DISAPPEARANCE_RATIONALE: If liturgical transmission were no longer sufficient to claim 'living language' status, rabbinical authority would lose its interpretive monopoly over linguistic legitimacy, secular speech communities would be reclassified from desecrators to legitimate speakers, and the sociolinguistic hierarchy of the community would reorganize around generative or literary criteria.
% FOUNDING_PROBLEM: Maintaining Jewish religious and textual continuity across diaspora without a territorial state or shared vernacular.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinical authority and liturgical practitioners attest the problem remains live. Secular historians and Zionist archival scholarship attest the problem was superseded by modern political state-building and native-language revival. No universally acknowledged neutral corroborating party exists; corroboration is split across the beneficiary and payer seats.
narrative_ontology:disappearance_verdict(living_language_status__liturgical_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__liturgical_preservation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__liturgical_preservation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(living_language_status__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__liturgical_preservation_reading, 0.3, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__liturgical_preservation_reading_tests).
:- end_tests(living_language_status__liturgical_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.30) because the primary activity is genuine liturgical coordination with substantial communal value. Suppression is moderate-high (0.55) because the boundary between sacred and secular language use requires active doctrinal and social enforcement to prevent vernacular contamination. Theater ratio (0.40) reflects the growing performative strain of calling a non-generative liturgical language 'living' in the face of modern sociolinguistic standards. Accessibility collapse (0.65) is high because, within the frame, any secular alternative is defined as desecration rather than a valid linguistic path. Resistance (0.50) captures the sustained challenge from secular nationalists and modernizers.
 *
 * PERSPECTIVAL GAP:
 *   The rabbinical authority seat experiences this constraint as rope-like: it solves a real coordination problem and preserves a civilization. The secular speech community seat experiences it as snare-like: the same structure that coordinates the pious simultaneously extracts legitimacy from the secular. The engine computes this divergence from beneficiary/victim declarations and divergent exit options (identity_locked for the authority versus constrained for the secular community).
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinical authority and liturgical practitioners are declared beneficiaries, directing them toward low d (subsidy side). Their identity_locked exit amplifies their structural fusion to the constraint. The secular speech community is a declared victim with constrained exit, placing them near the full-target end (high d). Scope differentiates modestly: the rabbinical claim operates globally, while the secular community is nationally bounded, meaning the engine will register the transnational authority as more insulated from local resistance.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â diaspora continuity without a state â is contested: the rabbinical seat says it is live, while secular historians say it was solved by political Zionism and native-language revival. The mismatch (founding_problem_status=contested, disappearance_verdict=world_rearranges) flags that the arrangement may have outlived its original coordination function for some seats while persisting for others. This prevents mislabeling the constraint as pure rope (ignoring the victimization of secular speakers) or pure snare (ignoring the real coordination value for liturgical practitioners).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Does the liturgical preservation reading foreclose the native generation reading within a single framework, or do they coexist?',
    'Analysis of whether any single religious authority simultaneously holds both that liturgical sufficiency constitutes vitality and that native generational transmission is necessary.',
    'If foreclosed, the readings are mutually exclusive commitments; if coexistent, the kernel is genuinely ambiguous and the authority structure can absorb both.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Structural relationship between liturgical and native-generation readings.').

omega_variable(
    liturgical_vs_political_extraction,
    'Is the delegitimization of secular speech an intrinsic feature of liturgical preservation, or a contingent political defense of rabbinical authority?',
    'Historical comparison with other liturgically preserved languages (e.g., Latin in Catholicism, Classical Arabic) where similar authority structures do or do not delegitimize vernacular use.',
    'If necessary, extraction is inherent to the coordination type; if contingent, the constraint could theoretically be reformed toward a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_vs_political_extraction, conceptual, 'Whether extraction is inherent or contingent to liturgical coordination.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (institutional excommunication, educational control) or internalized (shame, sanctity norms internalized by the secular community)?',
    'Post-exit trajectory study: if secular speakers who leave the religious community continue to feel delegitimized, suppression is partially internalized.',
    'If internalized, effective suppression exceeds the structural measure â the target carries the constraint after formal exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__liturgical_preservation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t0, living_language_status__liturgical_preservation_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(livi_tr_t20, living_language_status__liturgical_preservation_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(livi_tr_t40, living_language_status__liturgical_preservation_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(livi_tr_t60, living_language_status__liturgical_preservation_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement(livi_tr_t80, living_language_status__liturgical_preservation_reading, theater_ratio, 80, 0.4).
narrative_ontology:measurement(livi_tr_t100, living_language_status__liturgical_preservation_reading, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(livi_be_t0, living_language_status__liturgical_preservation_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(livi_be_t20, living_language_status__liturgical_preservation_reading, base_extractiveness, 20, 0.2).
narrative_ontology:measurement(livi_be_t40, living_language_status__liturgical_preservation_reading, base_extractiveness, 40, 0.23).
narrative_ontology:measurement(livi_be_t60, living_language_status__liturgical_preservation_reading, base_extractiveness, 60, 0.26).
narrative_ontology:measurement(livi_be_t80, living_language_status__liturgical_preservation_reading, base_extractiveness, 80, 0.28).
narrative_ontology:measurement(livi_be_t100, living_language_status__liturgical_preservation_reading, base_extractiveness, 100, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t0, living_language_status__liturgical_preservation_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(livi_su_t20, living_language_status__liturgical_preservation_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(livi_su_t40, living_language_status__liturgical_preservation_reading, suppression_requirement, 40, 0.46).
narrative_ontology:measurement(livi_su_t60, living_language_status__liturgical_preservation_reading, suppression_requirement, 60, 0.5).
narrative_ontology:measurement(livi_su_t80, living_language_status__liturgical_preservation_reading, suppression_requirement, 80, 0.53).
narrative_ontology:measurement(livi_su_t100, living_language_status__liturgical_preservation_reading, suppression_requirement, 100, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__liturgical_preservation_reading, identity_coordination).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, native_generation_reading).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, literary_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the living_language_status kernel, which decomposes into three structurally distinct claims: liturgical preservation (this file), native generational transmission, and literary productivity. Each reading has a different beneficiary/victim structure and epsilon value.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
