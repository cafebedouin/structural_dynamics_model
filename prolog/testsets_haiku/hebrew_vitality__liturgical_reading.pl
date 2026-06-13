% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__liturgical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__liturgical_reading, []).

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
 *   constraint_id: hebrew_vitality__liturgical_reading
 *   human_readable: Hebrew Vitality Through Liturgical Preservation and Unbroken Ritual Use
 *   domain: sociolinguistics/language_revitalization/religious_tradition
 *
 * SUMMARY:
 *   This constraint embodies one reading of what constitutes Hebrew vitality:
 *   the claim that unbroken liturgical use, adjudicated by rabbinic
 *   authority, IS the proof and substance of the language's vitality. This
 *   reading emerged when Hebrew had no native speakers (roughly 2nd–18th
 *   centuries) and needed a framework for claiming its continuity. It
 *   persists in rabbinic institutions even after the empirical conditions
 *   changed—native Hebrew speakers now exist, the language has vernacular
 *   productivity, and it is acquired as a first language by Israeli and
 *   diaspora children. The liturgical reading does not prevent those
 *   developments; it claims authority over what counts as authentic vitality.
 *   The constraint's kernel is the stabilized commitment to liturgical
 *   authority as the arbiter of Hebrew's vital status. Three competing
 *   readings exist: this one (liturgical_reading, a rope-like coordination
 *   around rabbinic adjudication), the native_daily_reading (only spontaneous
 *   generation by native speakers constitutes vitality), and the
 *   hybrid_continuity_reading (liturgical preservation was necessary
 *   historical enabler; modern vitality requires both substrate and
 *   vernacular reconstruction). The expected structural delta is LOW
 *   extraction because the liturgical reading imposes no direct cost on
 *   participants—it is a definitions game, not a resource transfer—and the
 *   beneficiary set is limited to institutional rabbinic authorities who gain
 *   definitional authority.
 *
 * KEY AGENTS:
 *   - rabbinic_authorities: Institutional agenda-setter; maintain the liturgical canon; benefit from the definitional authority the framework confers
 *   - liturgical_practitioners: Organized beneficiaries; validate their ritual participation as constitutive of vitality; receive cultural identity and continuity
 *   - hebrew_linguists: Institutional observers; measure vitality by empirical-linguistic metrics (native speakers, productivity, innovation); excluded from authority
 *   - hebrew_revival_educators: Moderate-power payers; labor in a framework where their pedagogy may be delegitimized by rabbinical authority; also benefit from liturgical texts and methods
 *   - secular_israeli_hebraists: Excluded; have made Hebrew a genuinely vital living language; their practice is outside the rabbinically-centered definitional frame
 *   - jewish_identity_seekers: Powerless beneficiaries; identity-locked into the liturgical framework; ritual participation validates their Jewish identity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__liturgical_reading, 0.15).
domain_priors:suppression_score(hebrew_vitality__liturgical_reading, 0.22).
domain_priors:theater_ratio(hebrew_vitality__liturgical_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__liturgical_reading, rope).
narrative_ontology:human_readable(hebrew_vitality__liturgical_reading, "Hebrew Vitality Through Liturgical Preservation and Unbroken Ritual Use").
narrative_ontology:topic_domain(hebrew_vitality__liturgical_reading, "sociolinguistics/language_revitalization/religious_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__liturgical_reading, '9cea97c4-27d2-4c7f-91fa-4a66f904a273').
narrative_ontology:cs_kernel_codification('9cea97c4-27d2-4c7f-91fa-4a66f904a273', fixed_text).
narrative_ontology:cs_authority_grounding('9cea97c4-27d2-4c7f-91fa-4a66f904a273', lineage).
narrative_ontology:cs_interpretation_layer_present('9cea97c4-27d2-4c7f-91fa-4a66f904a273').
narrative_ontology:cs_reading_relation('9cea97c4-27d2-4c7f-91fa-4a66f904a273', hebrew_vitality__native_daily_reading, coexists_with).
narrative_ontology:cs_reading_relation('9cea97c4-27d2-4c7f-91fa-4a66f904a273', hebrew_vitality__hybrid_continuity_reading, influences).
narrative_ontology:cs_axiom('9cea97c4-27d2-4c7f-91fa-4a66f904a273', foundational, unbroken_liturgical_use_constitutes_vitality).
narrative_ontology:cs_axiom_status(unbroken_liturgical_use_constitutes_vitality, holdable).
narrative_ontology:cs_axiom_grounding('9cea97c4-27d2-4c7f-91fa-4a66f904a273', unbroken_liturgical_use_constitutes_vitality, conventional).
narrative_ontology:cs_axiom('9cea97c4-27d2-4c7f-91fa-4a66f904a273', foundational, rabbinic_textual_authority_grounds_linguistic_legitimacy).
narrative_ontology:cs_axiom_status(rabbinic_textual_authority_grounds_linguistic_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('9cea97c4-27d2-4c7f-91fa-4a66f904a273', rabbinic_textual_authority_grounds_linguistic_legitimacy, deontological).
narrative_ontology:cs_reference_frame('9cea97c4-27d2-4c7f-91fa-4a66f904a273', rabbinic_liturgical_authenticity_frame).
narrative_ontology:cs_drift_state('9cea97c4-27d2-4c7f-91fa-4a66f904a273', post_israel_revival_and_native_speaker_emergence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9cea97c4-27d2-4c7f-91fa-4a66f904a273', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__liturgical_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, liturgical_practitioners).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__liturgical_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(hebrew_vitality__liturgical_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__liturgical_reading_tests).
:- end_tests(hebrew_vitality__liturgical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is LOW (0.15 at interval end) because the liturgical reading operates primarily as a definitional claim, not as a resource-extraction mechanism. No direct goods are moved; no actor is forced to participate in something that imposes measurable cost. The beneficiaries (rabbinic authorities, liturgical practitioners, identity-seeking participants) gain validation and authority; no victim set exists because the constraint does not prevent alternative readings—it only claims authority over what counts as authentic. Suppression is MODERATE (0.22) because the constraint's persistence depends on active institutional maintenance and on suppressing the linguistic reading that would displace it (the Hebrew-linguist and secular-revivalist frames). Theater is LOW-MODERATE (0.18) because the constraint's function shifted after Hebrew recovered native speakers: where once liturgical use was genuinely necessary to transmit Hebrew at all, it is now one among many valid ways to engage with the language. The gap between functional necessity and claimed authenticity is the theatrical element—the ongoing assertion that ritual preserves vitality when empirically, modern vitality rests on native acquisition. The measurement series shows a RISING trend from 1800 to 1950 (theatrical burden increased as native speakers emerged, requiring stronger institutional assertion of liturgical primacy) and then PLATEAUS (by 1980, a stable institutional equilibrium has been reached: rabbinical authorities accept native speakers as valid, but retain claims about authenticity and ritual vitality as their distinctive authority domain). All measurements are shared across the single time grid (1800–2026); the earliest points (1800–1880) are marked as projected because direct evidence is fragmentary, while 1920 onward is marked observed (based on archival records, rabbinic responsa, education records, and linguistic documentation).
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic institutional seat, the liturgical reading is a genuine coordination function: it provides a coherent, transmissible answer to 'what is Hebrew?' when geography and time threaten to dissolve community coherence. The unbroken-use criterion is observable and authority-auditable at every point. From the secular Hebrew linguist and native-speaker seat, the same constraint is a false summary: it misattributes vitality to ritual while the actual vitality lies in spontaneous intergenerational native acquisition. From the liturgical practitioner seat (identity-locked), the reading is constitutive of self-understanding—it validates their form of engagement as authentic, not second-class. From the Hebrew educator seat outside rabbinical control, the reading creates implicit delegitimacy: their pedagogies are pressed to defer to rabbinical authority about what counts as 'real' Hebrew. The engine will compute these perspectival divergences from the power atoms and exit-option differentials. The authored claim (rope—coordination around a real problem) and the authored metrics (low extraction, moderate suppression, low-moderate theater) are INDEPENDENT statements; divergence between claim and computed type will be diagnostic.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authorities sit at the beneficiary end (d ≈ 0.05–0.15): they gain definitional authority and institutional validation from the framework without bearing costs. Liturgical practitioners sit near beneficiary (d ≈ 0.1–0.2): they gain cultural identity and community belonging without being forced into participation—participation is volitional and validating. Hebrew educators outside rabbinical control sit near symmetric or slightly toward payer (d ≈ 0.4–0.5): they benefit from the liturgical infrastructure (texts, study methods, community) but bear the implicit cost of operating in a frame that may delegitimize their pedagogy. Secular revivalists and linguists are EXCLUDED from the beneficiary/payer frame—their directionality is not computed because they are outside the constraint's scope (they are not organized around the rabbinical definition; they operate from an alternative epistemic frame). The constraint's spatial scope (global, because rabbinical institutions exist worldwide) and time horizon (generational, because the claim concerns linguistic continuity across generations) amplify the institutional directionality.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vitality_definition_empirical_vs_normative,
    'Is ''vitality'' an empirical-linguistic property (measurable by native speakers, vernacular productivity, spontaneous generation) or a normative-institutional property (adjudicated by authorized authorities over a defined domain)?',
    'Comparative case study: track other languages where ritual preservation claims compete with empirical vitality measures (e.g., Latin in the Catholic liturgy, Sanskrit in Hindu ritual, Biblical Aramaic in Jewish law). If the linguistic metrics and the ritual-preservation claims systematically diverge, the distinction is real and non-trivial.',
    'If empirical-linguistic measures are adopted as authoritative, the liturgical reading is reclassified as a false summit (a claim that ritual participation constitutes vitality when empirically it does not). If normative-institutional authority is accepted, the reading remains coherent as a coordination framework specific to rabbinic institutions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vitality_definition_empirical_vs_normative, conceptual, 'Whether vitality is an empirical property or an authority-dependent definition.').

omega_variable(
    suppression_internalization_vs_structural,
    'How much of the measured suppression (0.22) is structural institutional enforcement (rabbinical gatekeeping of authenticity) versus internalized identity-fusion (individuals have fused their Jewish self-understanding with the liturgical framework and self-suppress dissent)?',
    'Post-institutional trajectories: track individuals who exit the rabbinical framework (e.g., secular educators, literary Hebrew writers, non-Orthodox practitioners). If suppression persists after institutional enforcement is removed (if they internalize the hierarchical framing even in the absence of external gatekeeping), the suppression is substantially internalized. If they quickly adopt alternative frameworks and cease self-suppressing, the suppression is primarily structural.',
    'If suppression is substantially internalized, the constraint''s effective hold is stronger than the institutional measure suggests—the identity lock is the carrier of suppression. If suppression is primarily structural, exiting the institutional framework dissolves it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_vs_structural, empirical, 'Suppression mechanism: structural institutional enforcement versus internalized identity-fusion.').

omega_variable(
    competing_reading_empirical_closure,
    'Has the empirical development of native Hebrew speakers (Israel, diaspora) materially closed off the native_daily_reading as a competitor, or do the readings remain structurally viable?',
    'If linguistic evidence for genuine native-speaker vitality continues to accumulate (intergenerational transmission, morphological productivity, innovation) and rabbinical authorities acknowledge it without downgrading it to mere ''preservation,'' the readings coexist. If rabbinical institutions actively contest or delegitimize the linguistic evidence, a foreclosure mechanism is operating.',
    'Coexistence supports the ''coexists_with'' reading relation; active delegitimacy of linguistic evidence would suggest foreclosure or stronger influences pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competing_reading_empirical_closure, empirical, 'Whether the native_daily_reading has been foreclosed by empirical development or remains a live alternative.').

omega_variable(
    theatrical_maintenance_function,
    'Is the rising theater_ratio (from 0.05 in 1800 to 0.18 by 2026) driven by the constraint''s actual functional necessity declining (native speakers took over the transmission work) and institutional assertion of authenticity compensating, or by some other factor?',
    'Temporal analysis of institutional activity: measure the proportion of rabbinical effort devoted to explaining why ritual is vitally important (defensive theorizing) versus solving practical coordination problems (what Hebrew is taught, how textual ambiguities are resolved). If defensive theorizing grows relative to coordination, the theater ratio is capturing functional atrophy.',
    'If theater is rising because functional necessity declined, the constraint may be trending toward piton classification (inertial, mostly performance). If theater is rising for other reasons (e.g., increased institutional complexity), the trend is less diagnostic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theatrical_maintenance_function, empirical, 'Whether rising theater_ratio reflects functional atrophy or other institutional changes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__liturgical_reading, 1800, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebrew_lit_tr_t1800, hebrew_vitality__liturgical_reading, theater_ratio, 1800, 0.05).
narrative_ontology:measurement(hebrew_lit_tr_t1880, hebrew_vitality__liturgical_reading, theater_ratio, 1880, 0.08).
narrative_ontology:measurement(hebrew_lit_tr_t1920, hebrew_vitality__liturgical_reading, theater_ratio, 1920, 0.12).
narrative_ontology:measurement(hebrew_lit_tr_t1950, hebrew_vitality__liturgical_reading, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(hebrew_lit_tr_t1980, hebrew_vitality__liturgical_reading, theater_ratio, 1980, 0.17).
narrative_ontology:measurement(hebrew_lit_tr_t2010, hebrew_vitality__liturgical_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(hebrew_lit_tr_t2026, hebrew_vitality__liturgical_reading, theater_ratio, 2026, 0.18).

% Extraction over time
narrative_ontology:measurement(hebrew_lit_be_t1800, hebrew_vitality__liturgical_reading, base_extractiveness, 1800, 0.08).
narrative_ontology:measurement(hebrew_lit_be_t1880, hebrew_vitality__liturgical_reading, base_extractiveness, 1880, 0.1).
narrative_ontology:measurement(hebrew_lit_be_t1920, hebrew_vitality__liturgical_reading, base_extractiveness, 1920, 0.12).
narrative_ontology:measurement(hebrew_lit_be_t1950, hebrew_vitality__liturgical_reading, base_extractiveness, 1950, 0.13).
narrative_ontology:measurement(hebrew_lit_be_t1980, hebrew_vitality__liturgical_reading, base_extractiveness, 1980, 0.14).
narrative_ontology:measurement(hebrew_lit_be_t2010, hebrew_vitality__liturgical_reading, base_extractiveness, 2010, 0.15).
narrative_ontology:measurement(hebrew_lit_be_t2026, hebrew_vitality__liturgical_reading, base_extractiveness, 2026, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hebrew_lit_su_t1800, hebrew_vitality__liturgical_reading, suppression_requirement, 1800, 0.1).
narrative_ontology:measurement(hebrew_lit_su_t1880, hebrew_vitality__liturgical_reading, suppression_requirement, 1880, 0.14).
narrative_ontology:measurement(hebrew_lit_su_t1920, hebrew_vitality__liturgical_reading, suppression_requirement, 1920, 0.18).
narrative_ontology:measurement(hebrew_lit_su_t1950, hebrew_vitality__liturgical_reading, suppression_requirement, 1950, 0.2).
narrative_ontology:measurement(hebrew_lit_su_t1980, hebrew_vitality__liturgical_reading, suppression_requirement, 1980, 0.22).
narrative_ontology:measurement(hebrew_lit_su_t2010, hebrew_vitality__liturgical_reading, suppression_requirement, 2010, 0.22).
narrative_ontology:measurement(hebrew_lit_su_t2026, hebrew_vitality__liturgical_reading, suppression_requirement, 2026, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__liturgical_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_vitality__liturgical_reading, 0.12).
narrative_ontology:affects_constraint(hebrew_vitality__liturgical_reading, hebrew_vitality__native_daily_reading).
narrative_ontology:affects_constraint(hebrew_vitality__liturgical_reading, hebrew_vitality__hybrid_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the hebrew_vitality kernel. Three structurally distinct constraints are defined on the same kernel: (1) liturgical_reading (this file) — unbroken ritual use as the arbiter of vitality, low extraction, authority-centered. (2) native_daily_reading — only native spontaneous generation constitutes vitality, substantially high extraction when native speakers are delegitimized by liturgical authorities. (3) hybrid_continuity_reading — liturgical preservation was necessary enabler; modern vitality requires both substrate and vernacular reconstruction, moderate extraction in the competition for institutional legitimacy. The three readings coexist as different parties' commitments. The ε-invariance principle requires them to be authored separately because they have different beneficiaries, different victim sets (if any), and different core claims about what vitality IS. All three should link to each other via network.affects_constraints to establish the family relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
