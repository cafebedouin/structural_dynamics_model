% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__liturgical_preservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__liturgical_preservation, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: hebrew_continuity__liturgical_preservation
 *   human_readable: Hebrew Continuity Through Liturgical Preservation
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint models Hebrew's survival through ritual recitation and
 *   textual transmission alone — without native speakers — as a
 *   self-sustaining cultural transmission mechanism. The reading asserts that
 *   Hebrew 'lives' in the preservation of its liturgical corpus and the
 *   disciplined recitation practices that maintain textual fidelity across
 *   generations. This is a mountain claim: the constraint presents itself as
 *   a natural law of cultural continuity, where the sacred text's structural
 *   properties (consonantal root system, formulaic density, memorization
 *   architecture) make preservation inevitable once the ritual framework is
 *   established. Beneficiaries include traditional ritual communities (who
 *   maintain identity through the practice) and textual transmission
 *   institutions (yeshivas, scribal traditions, liturgical academies). The
 *   declared victim set — secularizing forces — reflects the reading's
 *   internal framing: forces that threaten the textual tradition are cast as
 *   bearing the cost of the constraint's persistence.
 *
 * KEY AGENTS:
 *   - traditional_ritual_communities: Primary beneficiary (organized/identity_locked) — sustains identity through ritual recitation
 *   - textual_transmission_institutions: Primary beneficiary (institutional/identity_locked) — maintains the preservation infrastructure
 *   - secularizing_forces: Declared victim (organized/constrained) — experiences the constraint as threat to cultural transformation
 *   - liturgical_practitioners: Secondary beneficiary (organized/identity_locked) — performs the recitation that constitutes the constraint
 *   - historical_linguists: Observer (analytical/analytical) — studies the transmission mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__liturgical_preservation, 0.35).
domain_priors:suppression_score(hebrew_continuity__liturgical_preservation, 0.25).
domain_priors:theater_ratio(hebrew_continuity__liturgical_preservation, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, extractiveness, 0.35).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__liturgical_preservation, mountain).
narrative_ontology:human_readable(hebrew_continuity__liturgical_preservation, "Hebrew Continuity Through Liturgical Preservation").
narrative_ontology:topic_domain(hebrew_continuity__liturgical_preservation, "sociolinguistics/language_revitalization/commitment_systems").

domain_priors:emerges_naturally(hebrew_continuity__liturgical_preservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__liturgical_preservation, 'b3ded113-5df2-42de-9db9-9d85582a0311').
narrative_ontology:cs_kernel_codification('b3ded113-5df2-42de-9db9-9d85582a0311', fixed_text).
narrative_ontology:cs_authority_grounding('b3ded113-5df2-42de-9db9-9d85582a0311', lineage).
narrative_ontology:cs_interpretation_layer_present('b3ded113-5df2-42de-9db9-9d85582a0311').
narrative_ontology:cs_reading_relation('b3ded113-5df2-42de-9db9-9d85582a0311', hebrew_continuity__native_generative, forecloses).
narrative_ontology:cs_reading_relation('b3ded113-5df2-42de-9db9-9d85582a0311', hebrew_continuity__bridge_pidginized, coexists_with).
narrative_ontology:cs_axiom('b3ded113-5df2-42de-9db9-9d85582a0311', foundational, sacred_text_preservation_suffices_for_continuity).
narrative_ontology:cs_axiom_status(sacred_text_preservation_suffices_for_continuity, holdable).
narrative_ontology:cs_axiom_grounding('b3ded113-5df2-42de-9db9-9d85582a0311', sacred_text_preservation_suffices_for_continuity, deontological).
narrative_ontology:cs_axiom('b3ded113-5df2-42de-9db9-9d85582a0311', foundational, native_speaker_intuition_not_required_for_language_life).
narrative_ontology:cs_axiom_status(native_speaker_intuition_not_required_for_language_life, holdable).
narrative_ontology:cs_axiom_grounding('b3ded113-5df2-42de-9db9-9d85582a0311', native_speaker_intuition_not_required_for_language_life, deontological).
narrative_ontology:cs_reference_frame('b3ded113-5df2-42de-9db9-9d85582a0311', canonical_textual_tradition).
narrative_ontology:cs_drift_state('b3ded113-5df2-42de-9db9-9d85582a0311', modern_nationalist_revival, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b3ded113-5df2-42de-9db9-9d85582a0311', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__liturgical_preservation, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, traditional_ritual_communities).
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, textual_transmission_institutions).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, secularizing_forces).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, liturgical_practitioners).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, liturgical_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities that maintain Jewish ritual life through Hebrew liturgy. Their identity, cohesion, and intergenerational continuity are constituted by the preserved textual tradition. They do not 'use' Hebrew as a language — they inhabit it as a ritual framework. Exit would mean dissolving the communal self-concept; the practice is not instrumental but constitutive.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, traditional_ritual_communities, beneficiary,
    organized, generational, identity_locked, global).

% Yeshivas, scribal traditions, rabbinical courts, liturgical academies that maintain the textual canon, train reciters, adjudicate textual variants, and authorize transmission. They set the standards for what counts as correct preservation. Their institutional existence depends on the constraint's continuation; they are structurally locked into its maintenance.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, textual_transmission_institutions, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Modernizing, nationalist, or assimilationist movements that seek to transform Jewish cultural life away from ritual textuality toward vernacular, territorial, or secular forms. They experience the liturgical preservation constraint as a structural barrier: the preserved text occupies the cultural space their projects would occupy. Their opposition is real but constrained — they cannot easily displace a tradition with millennial depth and institutional weight.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, secularizing_forces, payer,
    organized, biographical, constrained, global).

% Individuals who perform the daily work of ritual recitation, textual study, and transmission training. They benefit from the identity and communal belonging the practice provides. They also pay costs: years of training, lifelong discipline, opportunity costs of specialized knowledge. Their exit is identity_locked — leaving means losing the self constituted through the practice.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, liturgical_practitioners, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__liturgical_preservation, liturgical_practitioners, payer).

% Scholars who study Hebrew's unique revival trajectory as a test case for language preservation theory. They analyze the constraint from outside, tracking how a liturgical corpus became a native language. Their seat is analytical — they neither collect nor pay, but their frameworks shape how the constraint is understood in broader discourse.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, historical_linguists, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_continuity__liturgical_preservation, diffuse).
narrative_ontology:fixing_cost_class(hebrew_continuity__liturgical_preservation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a shared textual canon and ritual framework that constitutes communal identity across diaspora, time, and linguistic rupture. Solves the coordination problem of maintaining a collective symbolic system without a shared territory, native speaker base, or political sovereignty.
% TRANSFER_FUNCTION: Moves authority over textual interpretation and communal authenticity from the living vernacular (which does not exist) to the preserved canon and its institutional guardians. The transfer is not primarily material — it moves epistemic authority, identity-constituting power, and the right to define what counts as 'Hebrew' and 'Jewish continuity'.
% ABSENT_VOICES: Jews who experienced the constraint as exclusionary — women historically barred from advanced textual study, communities with non-standard liturgical traditions marginalized by the canonical text, individuals who sought vernacular expression but found only the preserved ritual framework. They are absent because the constraint's institutional structure (textual_transmission_institutions) historically controlled who could speak for the tradition.
% DISAPPEARANCE_RATIONALE: If the liturgical preservation constraint vanished overnight, the textual canon would lose its transmission infrastructure, ritual communities would lose their constitutive framework, and the symbolic continuity linking contemporary Jewish identity to ancient texts would rupture. The world would rearrange: new identity frameworks would need to be built, or the tradition would fragment into secular national cultures without the textual anchor.
% FOUNDING_PROBLEM: How to preserve a sacred textual tradition and the communal identity it constitutes when the native speaker base has been extinguished by exile, dispersion, and language shift — and when the surrounding cultures exert intense assimilatory pressure.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by historical records from the Second Temple period through the medieval commentators: the Mishnah (c. 200 CE) explicitly addresses transmission in exile; Saadia Gaon (10th c.) writes on the necessity of preserving Hebrew as a sacred tongue against Arabic dominance; the Vilna Gaon (18th c.) frames textual preservation as the survival mechanism for a people without land. These sources are outside the modern institutional beneficiaries — they predate the current yeshiva system and speak from the existential threat the constraint was built to answer.
narrative_ontology:disappearance_verdict(hebrew_continuity__liturgical_preservation, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__liturgical_preservation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__liturgical_preservation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(hebrew_continuity__liturgical_preservation, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__liturgical_preservation, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__liturgical_preservation_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, ExtMetricName, E),
    domain_priors:suppression_score(hebrew_continuity__liturgical_preservation, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hebrew_continuity__liturgical_preservation),
    narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hebrew_continuity__liturgical_preservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as a mountain because it presents as a structural feature of cultural transmission: once a sacred text achieves canonical status with a dedicated ritual community, its preservation follows from the text's own structural properties and the community's identity-constituting relationship to it. Extractiveness is low (0.35) because the primary dynamic is coordination — preserving a shared textual corpus — rather than extraction. Suppression is low (0.25) because the constraint operates through voluntary participation and identity commitment, not coercion. Theater ratio is low (0.15) because the ritual recitation is functionally necessary for the preservation claim. Accessibility collapse is high (0.85) because alternatives to the preserved text (living vernaculars, reformed liturgies) become structurally unavailable once the canonical form is fixed. Resistance is low (0.15) because the constraint's persistence depends on the practitioners' commitment, not on overcoming opposition. The measurement series shows gradual increases in all metrics over 200 time units, reflecting the slow institutionalization of the preservation apparatus and the growing stakes of textual fidelity.
 *
 * PERSPECTIVAL GAP:
 *   From the practitioner seat (traditional_ritual_communities, liturgical_practitioners), the constraint appears as a mountain — a natural, inevitable structure that sustains their identity. From the secularizing forces seat, it appears as a snare — an actively maintained barrier to cultural transformation that extracts compliance through identity locking. From the textual_transmission_institutions seat, it appears as a tangled_rope — genuine coordination (preserving the text) with asymmetric extraction (institutional authority over interpretation). The engine computes this divergence from the structural data: same constraint, different seats, different experienced types.
 *
 * DIRECTIONALITY LOGIC:
 *   Traditional ritual communities and textual transmission institutions are structural beneficiaries: they derive identity, authority, and continuity from the constraint. Their directionality d is near 0.0 (beneficiary end) because the constraint subsidizes their existence. Liturgical practitioners are also beneficiaries but with identity_locked exit — they cannot leave without losing the self-concept constituted through the practice. Secularizing forces are declared victims: they bear the cost of the constraint's persistence in the form of cultural space foreclosed, but their exit_options are 'constrained' (they can oppose but cannot easily displace the tradition). The victim declaration is contested (see omega) — secularizing forces may experience ideological opposition without material extraction. The engine derives d from these declarations: beneficiaries get low d, victims get high d.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem — preserving a sacred textual tradition against loss — remains live (see six_questions). No mandatrophy is declared because the preservation function continues to serve its original purpose: the text is still transmitted, the ritual still performed, the identity still constituted. The gradual metric increases reflect institutional maturation, not function drift. If the ritual became purely performative with no living transmission, theater_ratio would rise and mandatrophy would trigger; currently the practice remains generative within its frame.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_preservation,
    'Is Hebrew''s survival through liturgical preservation a genuine natural law of cultural transmission, or a constructed constraint that benefits identifiable institutional agents?',
    'Comparative analysis of language revival trajectories: if languages with exclusively liturgical preservation consistently survive while languages with native transmission die, the natural-law hypothesis gains support; if outcomes correlate with institutional investment patterns, the constructed hypothesis gains support.',
    'If constructed, this constraint would be reclassified from mountain to tangled_rope via false_summit_mountain detection, revealing beneficiary extraction from the symbolic preservation narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_preservation, conceptual, 'Whether the preservation mechanism is a structural feature of cultural reality or an institutional arrangement with identifiable beneficiaries').

omega_variable(
    secularizing_forces_as_victims,
    'Do secularizing forces genuinely bear extraction costs from liturgical Hebrew preservation, or is their designation as ''victims'' a framing artifact of the reading''s internal logic?',
    'Assess whether secularizing actors experience material cost from Hebrew''s liturgical survival (resource diversion, cultural displacement) or merely ideological opposition without material consequence.',
    'If secularizing forces are not genuine victims, the asymmetric extraction claim weakens and the constraint''s structural profile shifts toward rope or mountain without extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(secularizing_forces_as_victims, conceptual, 'Whether the declared victim set represents genuine extraction targets or rhetorical positioning').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__liturgical_preservation, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_continuity__liturgical_preservation, theater_ratio, 0, 0.05).
narrative_ontology:measurement(hebr_tr_t50, hebrew_continuity__liturgical_preservation, theater_ratio, 50, 0.08).
narrative_ontology:measurement(hebr_tr_t100, hebrew_continuity__liturgical_preservation, theater_ratio, 100, 0.1).
narrative_ontology:measurement(hebr_tr_t150, hebrew_continuity__liturgical_preservation, theater_ratio, 150, 0.12).
narrative_ontology:measurement(hebr_tr_t200, hebrew_continuity__liturgical_preservation, theater_ratio, 200, 0.15).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_continuity__liturgical_preservation, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(hebr_be_t50, hebrew_continuity__liturgical_preservation, base_extractiveness, 50, 0.2).
narrative_ontology:measurement(hebr_be_t100, hebrew_continuity__liturgical_preservation, base_extractiveness, 100, 0.25).
narrative_ontology:measurement(hebr_be_t150, hebrew_continuity__liturgical_preservation, base_extractiveness, 150, 0.3).
narrative_ontology:measurement(hebr_be_t200, hebrew_continuity__liturgical_preservation, base_extractiveness, 200, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_continuity__liturgical_preservation, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(hebr_su_t50, hebrew_continuity__liturgical_preservation, suppression_requirement, 50, 0.15).
narrative_ontology:measurement(hebr_su_t100, hebrew_continuity__liturgical_preservation, suppression_requirement, 100, 0.2).
narrative_ontology:measurement(hebr_su_t150, hebrew_continuity__liturgical_preservation, suppression_requirement, 150, 0.22).
narrative_ontology:measurement(hebr_su_t200, hebrew_continuity__liturgical_preservation, suppression_requirement, 200, 0.25).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=200
narrative_ontology:measurement(hebr_grid_01, hebrew_continuity__liturgical_preservation, accessibility_collapse(class), 0, 0.8).
narrative_ontology:measurement(hebr_grid_02, hebrew_continuity__liturgical_preservation, accessibility_collapse(class), 200, 0.85).
narrative_ontology:measurement(hebr_grid_03, hebrew_continuity__liturgical_preservation, accessibility_collapse(individual), 0, 0.9).
narrative_ontology:measurement(hebr_grid_04, hebrew_continuity__liturgical_preservation, accessibility_collapse(individual), 200, 0.85).
narrative_ontology:measurement(hebr_grid_05, hebrew_continuity__liturgical_preservation, accessibility_collapse(organizational), 0, 0.85).
narrative_ontology:measurement(hebr_grid_06, hebrew_continuity__liturgical_preservation, accessibility_collapse(organizational), 200, 0.88).
narrative_ontology:measurement(hebr_grid_07, hebrew_continuity__liturgical_preservation, accessibility_collapse(structural), 0, 0.82).
narrative_ontology:measurement(hebr_grid_08, hebrew_continuity__liturgical_preservation, accessibility_collapse(structural), 200, 0.87).
narrative_ontology:measurement(hebr_grid_09, hebrew_continuity__liturgical_preservation, resistance(class), 0, 0.2).
narrative_ontology:measurement(hebr_grid_10, hebrew_continuity__liturgical_preservation, resistance(class), 200, 0.25).
narrative_ontology:measurement(hebr_grid_11, hebrew_continuity__liturgical_preservation, resistance(individual), 0, 0.05).
narrative_ontology:measurement(hebr_grid_12, hebrew_continuity__liturgical_preservation, resistance(individual), 200, 0.1).
narrative_ontology:measurement(hebr_grid_13, hebrew_continuity__liturgical_preservation, resistance(organizational), 0, 0.1).
narrative_ontology:measurement(hebr_grid_14, hebrew_continuity__liturgical_preservation, resistance(organizational), 200, 0.15).
narrative_ontology:measurement(hebr_grid_15, hebrew_continuity__liturgical_preservation, resistance(structural), 0, 0.15).
narrative_ontology:measurement(hebr_grid_16, hebrew_continuity__liturgical_preservation, resistance(structural), 200, 0.2).
narrative_ontology:measurement(hebr_grid_17, hebrew_continuity__liturgical_preservation, stakes_inflation(class), 0, 0.2).
narrative_ontology:measurement(hebr_grid_18, hebrew_continuity__liturgical_preservation, stakes_inflation(class), 200, 0.3).
narrative_ontology:measurement(hebr_grid_19, hebrew_continuity__liturgical_preservation, stakes_inflation(individual), 0, 0.15).
narrative_ontology:measurement(hebr_grid_20, hebrew_continuity__liturgical_preservation, stakes_inflation(individual), 200, 0.25).
narrative_ontology:measurement(hebr_grid_21, hebrew_continuity__liturgical_preservation, stakes_inflation(organizational), 0, 0.1).
narrative_ontology:measurement(hebr_grid_22, hebrew_continuity__liturgical_preservation, stakes_inflation(organizational), 200, 0.2).
narrative_ontology:measurement(hebr_grid_23, hebrew_continuity__liturgical_preservation, stakes_inflation(structural), 0, 0.12).
narrative_ontology:measurement(hebr_grid_24, hebrew_continuity__liturgical_preservation, stakes_inflation(structural), 200, 0.22).
narrative_ontology:measurement(hebr_grid_25, hebrew_continuity__liturgical_preservation, suppression(class), 0, 0.15).
narrative_ontology:measurement(hebr_grid_26, hebrew_continuity__liturgical_preservation, suppression(class), 200, 0.25).
narrative_ontology:measurement(hebr_grid_27, hebrew_continuity__liturgical_preservation, suppression(individual), 0, 0.05).
narrative_ontology:measurement(hebr_grid_28, hebrew_continuity__liturgical_preservation, suppression(individual), 200, 0.15).
narrative_ontology:measurement(hebr_grid_29, hebrew_continuity__liturgical_preservation, suppression(organizational), 0, 0.1).
narrative_ontology:measurement(hebr_grid_30, hebrew_continuity__liturgical_preservation, suppression(organizational), 200, 0.2).
narrative_ontology:measurement(hebr_grid_31, hebrew_continuity__liturgical_preservation, suppression(structural), 0, 0.08).
narrative_ontology:measurement(hebr_grid_32, hebrew_continuity__liturgical_preservation, suppression(structural), 200, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__liturgical_preservation, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_continuity__liturgical_preservation, 0.08).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, hebrew_continuity__native_generative).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, hebrew_continuity__bridge_pidginized).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the hebrew_continuity kernel. The liturgical_preservation reading claims mountain status with low extraction; native_generative claims snare/tangled_rope status (native speaker gatekeeping as extraction); bridge_pidginized claims rope status (contact language as pure coordination). Their ε values differ because they describe structurally distinct constraints: liturgical preservation coordinates identity through fixed text; native generative use coordinates through living intuition; pidginized contact coordinates through simplified interaction. The natural-language label 'Hebrew continuity' conflates them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_continuity__liturgical_preservation, organized, 0.15).
constraint_indexing:directionality_override(hebrew_continuity__liturgical_preservation, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
