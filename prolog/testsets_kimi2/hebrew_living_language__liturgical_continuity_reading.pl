% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__liturgical_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-07
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: hebrew_living_language__liturgical_continuity_reading
 *   human_readable: Hebrew Living Language via Liturgical Continuity
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint instantiates the liturgical_continuity_reading of the
 *   hebrew_living_language kernel, which holds that Hebrew remains a living
 *   language through unbroken liturgical recitation and textual study across
 *   diaspora. The arrangement coordinates a globally dispersed people around
 *   a fixed textual kernel, with rabbinic institutions administering
 *   transmission and diaspora communities voluntarily sustaining practice.
 *   Sibling readings include the native_generation_reading, which privileges
 *   native daily speech, and the literary_revival_reading, which locates
 *   vitality in Haskalah literary production. The authored metrics are
 *   independent of the claimed type: the reading is claimed as rope because
 *   participation is voluntary, extraction is minimal, and the arrangement
 *   solves a genuine coordination problem, while the metrics descriptively
 *   reflect low extraction and negligible suppression.
 *
 * KEY AGENTS:
 *   - diaspora_jewish_communities: Primary beneficiary (organized/global/mobile exit) â gains identity continuity and shared sacred language
 *   - rabbinic_transmitters: Primary agenda-setter (institutional/global/identity_locked) â administers textual transmission and derives authority from continuity
 *   - modern_linguistic_observers: Analytical observer (analytical) â applies external sociolinguistic frameworks without institutional power over the constraint
 *   - native_speech_advocates: Excluded voice (organized) â holds a structurally absent definitional framework that is not admitted in liturgical discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__liturgical_continuity_reading, 0.1).
domain_priors:suppression_score(hebrew_living_language__liturgical_continuity_reading, 0.04).
domain_priors:theater_ratio(hebrew_living_language__liturgical_continuity_reading, 0.06).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 0.04).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, theater_ratio, 0.06).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__liturgical_continuity_reading, rope).
narrative_ontology:human_readable(hebrew_living_language__liturgical_continuity_reading, "Hebrew Living Language via Liturgical Continuity").
narrative_ontology:topic_domain(hebrew_living_language__liturgical_continuity_reading, "historical_linguistics/language_revitalization/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__liturgical_continuity_reading, '9c006e53-bbce-4015-afba-510eeac2ce22').
narrative_ontology:cs_kernel_codification('9c006e53-bbce-4015-afba-510eeac2ce22', fixed_text).
narrative_ontology:cs_authority_grounding('9c006e53-bbce-4015-afba-510eeac2ce22', lineage).
narrative_ontology:cs_interpretation_layer_present('9c006e53-bbce-4015-afba-510eeac2ce22').
narrative_ontology:cs_reading_relation('9c006e53-bbce-4015-afba-510eeac2ce22', hebrew_living_language__native_generation_reading, coexists_with).
narrative_ontology:cs_reading_relation('9c006e53-bbce-4015-afba-510eeac2ce22', hebrew_living_language__literary_revival_reading, influences).
narrative_ontology:cs_axiom('9c006e53-bbce-4015-afba-510eeac2ce22', foundational, liturgical_recitation_sustains_linguistic_life).
narrative_ontology:cs_axiom_status(liturgical_recitation_sustains_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('9c006e53-bbce-4015-afba-510eeac2ce22', liturgical_recitation_sustains_linguistic_life, conventional).
narrative_ontology:cs_reference_frame('9c006e53-bbce-4015-afba-510eeac2ce22', diaspora_liturgical_continuity).
narrative_ontology:cs_drift_state('9c006e53-bbce-4015-afba-510eeac2ce22', contemporary_linguistic_science_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('9c006e53-bbce-4015-afba-510eeac2ce22', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, diaspora_jewish_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain communal identity, religious practice, and intergenerational cohesion through shared Hebrew liturgy and textual study across dispersed geographic locations without a shared political territory. Participation is voluntary and spiritually motivated; exit takes the form of assimilation into host societies, which carries identity costs but is not blocked by the constraint.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, diaspora_jewish_communities, beneficiary,
    organized, generational, mobile, global).

% Administer and transmit liturgical norms, train students in Hebrew textual competence, and derive authority from an unbroken chain of transmission. Their institutional role and personal identity are fused with the continuity of Hebrew study and recitation; exit means abandoning vocation and community standing.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, rabbinic_transmitters, agenda_setter,
    institutional, generational, identity_locked, global).

% Apply sociolinguistic frameworks that typically define language vitality by native-speaker thresholds and daily communicative use. They observe the liturgical continuity arrangement without participating in its religious authority structure and do not interfere with its operation.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, modern_linguistic_observers, observer,
    analytical, biographical, analytical, global).

% Argue that memorized liturgical recitation does not constitute living language status and that Hebrew was effectively a liturgical heritage language until native speech revival in the late nineteenth century. Their definitional framework is structurally absent from traditional liturgical discourse.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, native_speech_advocates, excluded,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables dispersed Jewish communities to maintain a shared sacred language, common liturgical practice, and textual literacy across diverse host societies and political regimes without relying on territorial concentration or state institutions.
% TRANSFER_FUNCTION: Moves communal time, educational resources, and intergenerational attention toward Hebrew textual and liturgical competence, preserving phonological, morphological, and syntactic knowledge through ritual use and study.
% ABSENT_VOICES: Linguists and secular educators who define language vitality strictly by native generative speech are structurally absent from the traditional liturgical framework; their definitional criteria are not operative within the rabbinic transmission system.
% DISAPPEARANCE_RATIONALE: If the liturgical recitation and textual study arrangement vanished, diaspora Jewish communities would lose their shared linguistic framework for prayer and study; communal boundaries would weaken, host-language assimilation would accelerate, and the global coherence of rabbinic Judaism would fragment.
% FOUNDING_PROBLEM: How to preserve communal identity, sacred textual access, and religious cohesion for a territorially dispersed people lacking political sovereignty or a shared daily vernacular across host societies.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by diaspora historians and sociologists of religion outside the rabbinic beneficiary set, who document the role of liturgical Hebrew in maintaining group boundaries across the Babylonian, Sephardic, and Ashkenazi dispersions; also attested by comparative studies of other diasporic liturgical languages that face similar coordination pressures.
narrative_ontology:disappearance_verdict(hebrew_living_language__liturgical_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__liturgical_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__liturgical_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_living_language__liturgical_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__liturgical_continuity_reading, 0.1, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is set low (0.10) because the arrangement extracts minimal involuntary cost; participation is voluntary and the effort of study is spiritually motivated. Suppression is negligible (0.04) because there is no coercion maintaining the arrangement â it persists by communal valuation and identity commitment. Theater ratio is low (0.06) because liturgical recitation is functionally transmitting linguistic competence, not merely performed for show. Accessibility collapse is moderate (0.30) because within traditional frameworks alternatives such as vernacular prayer are theologically possible but socially and cognitively distant. Resistance is near-zero (0.02) because participants embrace the arrangement; external linguistic critique does not constitute resistance to the constraint itself. Measurements show stability across two millennia with minor fluctuation, consistent with a coordination mechanism that has adapted to changing host-society conditions without accumulating extraction.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (rabbinic transmitters) experiences the constraint as sacred duty and identity core; the beneficiary seat (diaspora communities) experiences it as communal heritage and spiritual practice; the analytical observer seat sees a sociolinguistic coordination mechanism with low extraction. The excluded seat experiences the constraint as an incomplete definition of language life. The engine computes these divergences from the structural data â no single seat's perception overrides the classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora communities sit near the beneficiary end because the constraint subsidizes their group continuity and provides non-excludable identity goods. Rabbinic transmitters sit near symmetric (d â 0.5) because their institutional identity is fused with the constraint â they both administer and are constituted by it. Native speech advocates, though excluded, would sit at high directionality if subject to the constraint's definitional pressure; as excluded observers their directionality is not computed. No overrides are needed because beneficiary declarations and exit options capture the true structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The liturgical continuity arrangement was built to solve the diaspora coordination problem and continues to solve it; there is no evidence its mandate has outlived its function. The classification as rope rather than piton or snare is warranted by the absence of performative maintenance, the low theater ratio, and the genuine coordination benefit to participants. Mislabeling as extraction would fail because no concentrated beneficiary captures rents from the arrangement, and mislabeling as mountain would fail because the constraint is a constructed social practice rather than an irreducible natural limit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_scope,
    'Does the liturgical continuity reading of Hebrew vitality structurally foreclose the native-generation definition, or can both criteria be held by different parties without logical contradiction?',
    'Cross-community attitudinal study measuring whether holders of the liturgical continuity frame logically reject native-speaker definitions or treat them as complementary measures.',
    'If the readings are mutually foreclosing, the kernel generates incommensurable constraints; if coexistent, the kernel permits hybrid or layered readings that would alter per-seat classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_scope, conceptual, 'Whether this reading forecloses sibling definitions of language life').

omega_variable(
    voluntary_participation_uniformity,
    'Is participation in Hebrew liturgical study uniformly voluntary across all diaspora sub-communities, or do closed enclaves exhibit implicit coercion that would create a victim set?',
    'Ethnographic measurement of exit costs (social sanctions, educational exclusion, marriage-market effects) in ultra-Orthodox vs. liberal diaspora communities.',
    'If coercion is found in sub-populations, the constraint''s effective extraction and suppression rise for those seats, potentially shifting the computed type from rope toward tangled_rope for that subset.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(voluntary_participation_uniformity, empirical, 'Whether voluntary participation holds across all diaspora conditions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__liturgical_continuity_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement(hebr_tr_t400, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 400, 0.05).
narrative_ontology:measurement(hebr_tr_t800, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 800, 0.05).
narrative_ontology:measurement(hebr_tr_t1200, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1200, 0.06).
narrative_ontology:measurement(hebr_tr_t1600, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1600, 0.08).
narrative_ontology:measurement(hebr_tr_t2000, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 2000, 0.07).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(hebr_be_t400, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 400, 0.09).
narrative_ontology:measurement(hebr_be_t800, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 800, 0.08).
narrative_ontology:measurement(hebr_be_t1200, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1200, 0.1).
narrative_ontology:measurement(hebr_be_t1600, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1600, 0.09).
narrative_ontology:measurement(hebr_be_t2000, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 2000, 0.1).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_living_language__liturgical_continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__liturgical_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_continuity_reading, native_generation_reading).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_continuity_reading, literary_revival_reading).

% DUAL FORMULATION NOTE:
% This constraint is the liturgical_continuity_reading of the hebrew_living_language kernel. It is structurally distinct from the literary_revival_reading (Haskalah written competence) and the native_generation_reading (native daily speech). Each reading carries a different epsilon, stakeholder structure, and classification because they describe different constraints arising from the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
