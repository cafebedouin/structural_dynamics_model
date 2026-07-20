% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__liturgical_habituation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__liturgical_habituation_reading, []).

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
 *   constraint_id: nicene_creed_authority__liturgical_habituation_reading
 *   human_readable: Nicene Creed as Liturgical Habituation Boundary
 *   domain: systematic_theology/ecclesiology
 *
 * SUMMARY:
 *   The Nicene Creed, recited in Christian liturgy since the fourth century,
 *   operates in this reading not as a metaphysical checkpoint but as a social
 *   coordination mechanism. Its repetition in worship constructs and
 *   maintains communal identity across linguistic and cultural boundaries
 *   without requiring homogeneous cognitive assent to its propositions. This
 *   reading treats the creed as a rope â a low-extraction coordination
 *   device that enables large-scale ecclesial belonging â while
 *   acknowledging that the same textual kernel supports more extractive
 *   readings in other theological frameworks.
 *
 * KEY AGENTS:
 *   - communion_participants: Primary beneficiaries (organized/global/mobile) â receive identity coordination through shared performance
 *   - liturgical_presiders: Agenda-setters (institutional/constrained) â maintain rubrical form without capturing rents
 *   - ritual_studies_observers: Analytical observers â document the functional autonomy of performance from assent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__liturgical_habituation_reading, 0.08).
domain_priors:suppression_score(nicene_creed_authority__liturgical_habituation_reading, 0.15).
domain_priors:theater_ratio(nicene_creed_authority__liturgical_habituation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__liturgical_habituation_reading, rope).
narrative_ontology:human_readable(nicene_creed_authority__liturgical_habituation_reading, "Nicene Creed as Liturgical Habituation Boundary").
narrative_ontology:topic_domain(nicene_creed_authority__liturgical_habituation_reading, "systematic_theology/ecclesiology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__liturgical_habituation_reading, '3ce71e6e-2aa8-4c1e-88e8-4b9c4c25c692').
narrative_ontology:cs_kernel_codification('3ce71e6e-2aa8-4c1e-88e8-4b9c4c25c692', fixed_text).
narrative_ontology:cs_authority_grounding('3ce71e6e-2aa8-4c1e-88e8-4b9c4c25c692', practice).
narrative_ontology:cs_interpretation_layer_present('3ce71e6e-2aa8-4c1e-88e8-4b9c4c25c692').
narrative_ontology:cs_reading_relation('3ce71e6e-2aa8-4c1e-88e8-4b9c4c25c692', nicene_creed_authority__strict_orthodox_reading, influences).
narrative_ontology:cs_reading_relation('3ce71e6e-2aa8-4c1e-88e8-4b9c4c25c692', nicene_creed_authority__symbolic_confessional_reading, influences).
narrative_ontology:cs_axiom('3ce71e6e-2aa8-4c1e-88e8-4b9c4c25c692', foundational, liturgical_performance_constitutes_membership).
narrative_ontology:cs_axiom_status(liturgical_performance_constitutes_membership, holdable).
narrative_ontology:cs_axiom_grounding('3ce71e6e-2aa8-4c1e-88e8-4b9c4c25c692', liturgical_performance_constitutes_membership, conventional).
narrative_ontology:cs_axiom('3ce71e6e-2aa8-4c1e-88e8-4b9c4c25c692', foundational, cognitive_assent_nonbinding_for_communion).
narrative_ontology:cs_axiom_status(cognitive_assent_nonbinding_for_communion, holdable).
narrative_ontology:cs_axiom_grounding('3ce71e6e-2aa8-4c1e-88e8-4b9c4c25c692', cognitive_assent_nonbinding_for_communion, conventional).
narrative_ontology:cs_reference_frame('3ce71e6e-2aa8-4c1e-88e8-4b9c4c25c692', liturgical_communion_practice).
narrative_ontology:cs_drift_state('3ce71e6e-2aa8-4c1e-88e8-4b9c4c25c692', post_enlightenment_assent_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3ce71e6e-2aa8-4c1e-88e8-4b9c4c25c692', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, communion_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gather for worship and recite the Nicene Creed as a communal speech-act. Gain shared identity, intergenerational continuity, and recognizable belonging across linguistic and cultural boundaries without being required to affirm every proposition metaphysically. Can exit by joining a non-creedal tradition or leaving institutional Christianity, but within the liturgical community the performance itself is the coordinating center.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, communion_participants, beneficiary,
    organized, generational, mobile, global).

% Lead the worship assembly and facilitate the creedal recitation according to inherited rubrics. Their authority derives from continuity of practice and ordained role rather than personal power. They maintain the textual form and ritual sequence but do not extract material benefit from the performance itself; their capacity to alter the kernel is limited by the fixed text and tradition.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, liturgical_presiders, agenda_setter,
    institutional, generational, constrained, global).

% Analyze the social function of repeated creedal recitation in constructing communal identity. Observe that the liturgical performance operates as a boundary marker independent of individual propositional assent, documenting low extractiveness and high coordination value across diverse Christian communions.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, ritual_studies_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, repeatable verbal performance that marks in-group identity and enables large-scale communal coordination across time, space, and language without requiring homogeneous metaphysical commitments.
% TRANSFER_FUNCTION: Moves social belonging and identity continuity from the individual to the communal body; the individual gains membership and recognition through performance, while the community gains boundary maintenance and intergenerational continuity.
% ABSENT_VOICES: Strict metaphysical theologians who insist that creedal authority requires propositional assent are present in the broader discourse but functionally absent from the liturgical-habituation logic; radical iconoclasts who reject all fixed liturgical forms are also absent from the communal table.
% DISAPPEARANCE_RATIONALE: If the creed ceased to function as a liturgical boundary marker, Christian communal identity would lose a primary coordination device that currently enables assembly across doctrinal diversity. Alternative texts lack the same historical depth and ecumenical reach, so worship practices and inter-church relations would reorganize around new or competing markers.
% FOUNDING_PROBLEM: How to maintain recognizable communal identity and shared worship across diverse linguistic, cultural, and metaphysical interpretations within expanding Christianity.
% FOUNDING_PROBLEM_CORROBORATION: Liturgical historians and ritual studies scholars outside the benefiting communities attest that fixed verbal formulas solved coordination problems in late antiquity; sociologists of religion corroborate that performative identity markers continue to solve belonging problems in contemporary pluralist contexts.
narrative_ontology:disappearance_verdict(nicene_creed_authority__liturgical_habituation_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_creed_authority__liturgical_habituation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__liturgical_habituation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nicene_creed_authority__liturgical_habituation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_creed_authority__liturgical_habituation_reading, 0.08, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__liturgical_habituation_reading_tests).
:- end_tests(nicene_creed_authority__liturgical_habituation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.08) because the creed as performed extracts neither material goods nor compliance rents; it coordinates identity through voluntary, repeated participation. Suppression is low (0.15) because the constraint persists by the preference of participants and the inertia of tradition rather than by active exclusion of alternatives. Theater ratio is low (0.10) because liturgical performance is functionally constitutive, not performative cover for hidden extraction. Accessibility collapse is moderate (0.35): alternatives to this specific creed exist (other confessions, non-creedal traditions), but within the liturgical community the specific text is the stabilized coordination point. Resistance is very low (0.10) because the practice is widely accepted by those who participate; those who reject it simply exit to non-creedal communities.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (liturgical presiders) and the beneficiary seat (communion participants) both experience low extraction; the divergence is minimal because the coordination benefit is diffuse and no party captures concentrated rents. The analytical observer sees the functional independence of performance from assent, while a strict-orthodox proponent would read the same liturgical act as enforcing metaphysical commitment â the engine computes this divergence from the structural data of each reading's own constraint story.
 *
 * DIRECTIONALITY LOGIC:
 *   Communion participants are declared beneficiaries with mobile exit options, placing their directionality near the beneficiary pole (low d). Liturgical presiders have constrained exit (ordination and career path dependence) but are not declared beneficiaries of extraction; their directionality sits near symmetric (d â 0.5) because they maintain the constraint without capturing its gains. No victims are declared because the rope extracts from no one. Effective extraction (Ï) remains negligible for all seats due to the low base Îµ.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mandatrophy mislabeling by distinguishing the creed's coordination function (enabling assembly) from its occasional co-optation for doctrinal enforcement. The strict-orthodox reading may be a snare or tangled rope using the same text; this reading isolates the pure coordination substrate and measures it independently. If the liturgical performance were to atrophy while the text remained canonically enforced, that would signal piton or snare dynamics â but here the performance is alive and functional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liturgical_vs_metaphysical_priority,
    'Does the creed''s liturgical performance function independently of metaphysical assent in all historical periods, or has the relationship between performance and assent varied culturally?',
    'Historical and liturgical studies comparing periods where assent was emphasized (e.g., post-Reformation confessionalism) against periods where performance dominated (e.g., early medieval sacramental practice).',
    'If performance was always dependent on assent, this reading is historically inaccurate and the constraint is more extractive (snare or tangled rope) than modeled; if independent, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_vs_metaphysical_priority, empirical, 'Historical variability of the performance-assent relationship').

omega_variable(
    kernel_reading_boundaries,
    'Is liturgical habituation a distinct reading of the creed''s authority, or merely a functional description compatible with both strict orthodox and symbolic confessional readings?',
    'Analysis of whether proponents of this reading treat it as a standalone normative position or as a neutral substrate beneath metaphysical readings.',
    'If merely a substrate, its classification as rope is robust; if a competing normative claim enforcing a specific ecclesiology, it may enter tangled rope territory by demanding a particular practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundaries, conceptual, 'Whether this reading is a distinct normative position or functional substrate').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__liturgical_habituation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nicene_creed_lit_hab_tr_t0, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(nicene_creed_lit_hab_tr_t20, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(nicene_creed_lit_hab_tr_t40, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 40, 0.11).
narrative_ontology:measurement(nicene_creed_lit_hab_tr_t60, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(nicene_creed_lit_hab_tr_t80, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement(nicene_creed_lit_hab_tr_t100, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(nicene_creed_lit_hab_be_t0, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(nicene_creed_lit_hab_be_t20, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 20, 0.08).
narrative_ontology:measurement(nicene_creed_lit_hab_be_t40, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 40, 0.09).
narrative_ontology:measurement(nicene_creed_lit_hab_be_t60, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 60, 0.08).
narrative_ontology:measurement(nicene_creed_lit_hab_be_t80, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 80, 0.08).
narrative_ontology:measurement(nicene_creed_lit_hab_be_t100, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 100, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(nicene_creed_lit_hab_su_t0, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(nicene_creed_lit_hab_su_t20, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 20, 0.15).
narrative_ontology:measurement(nicene_creed_lit_hab_su_t40, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 40, 0.16).
narrative_ontology:measurement(nicene_creed_lit_hab_su_t60, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 60, 0.15).
narrative_ontology:measurement(nicene_creed_lit_hab_su_t80, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 80, 0.15).
narrative_ontology:measurement(nicene_creed_lit_hab_su_t100, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 100, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__liturgical_habituation_reading, identity_coordination).
narrative_ontology:affects_constraint(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority__strict_orthodox_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority__symbolic_confessional_reading).

% DUAL FORMULATION NOTE:
% The kernel 'nicene_creed_authority' decomposes into three structurally distinct constraints: a liturgical-habituation reading (this file, low extraction, rope), a strict-orthodox reading (high extraction, likely snare or tangled rope), and a symbolic-confessional reading (moderate extraction, likely rope or scaffold). The Îµ values differ because the strict-orthodox reading adds active enforcement of metaphysical assent, while this reading treats performance as sufficient.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
