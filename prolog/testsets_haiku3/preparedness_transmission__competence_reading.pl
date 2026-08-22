% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__competence_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: preparedness_transmission__competence_reading
 *   human_readable: Preparedness Transmission via Live Exercise (Competence Reading)
 *   domain: institutional/civil_defense
 *
 * SUMMARY:
 *   This constraint captures the civil defense doctrine that drills and
 *   inspections exercise the full disaster-response apparatus in order to
 *   validate and transmit adaptive capacity — the ability for responders to
 *   recognize novel failure signatures and improvise effective responses when
 *   reality deviates from training scenarios. This reading asserts that each
 *   generation of responders re-validates capability through live practice,
 *   and that inspectors recognize and measure adaptive capacity, not just
 *   procedural compliance. The competence_reading is one of three readings of
 *   the contested kernel preparedness_transmission. The husk_reading asserts
 *   the same activities have become memorial ritual without operational
 *   knowledge. The hybrid_reading asserts adaptive capacity is stratified —
 *   infrastructure engineering remains robust while civilian coordination
 *   knowledge has decayed. The three readings share the same referent (the
 *   standing practice of drills and inspections) but attribute different
 *   epistemic status to what knowledge is actually being transmitted.
 *
 * KEY AGENTS:
 *   - civil_defense_agencies: institutional agenda-setters who design drills and frame them as validation of adaptive capacity
 *   - emergency_responders: organized actors who participate in drills and bear the time cost; their adaptive capacity is what is being tested
 *   - drill_inspectors: institutional agents who observe drills and assess whether responders are demonstrating adaptive problem-solving or rigidly executing memorized steps
 *   - civilian_populations: powerless beneficiaries whose disaster safety depends on whether responders have genuine adaptive capacity
 *   - institutional_memory_holders: professional civil defenders whose expertise is constituted through transmitted knowledge of failure signatures; they experience drills as live validation of competence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__competence_reading, 0.18).
domain_priors:suppression_score(preparedness_transmission__competence_reading, 0.12).
domain_priors:theater_ratio(preparedness_transmission__competence_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__competence_reading, rope).
narrative_ontology:human_readable(preparedness_transmission__competence_reading, "Preparedness Transmission via Live Exercise (Competence Reading)").
narrative_ontology:topic_domain(preparedness_transmission__competence_reading, "institutional/civil_defense").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__competence_reading, 'b770f4b1-9570-40f9-8d50-b582c0058751').
narrative_ontology:cs_kernel_codification('b770f4b1-9570-40f9-8d50-b582c0058751', distributed).
narrative_ontology:cs_authority_grounding('b770f4b1-9570-40f9-8d50-b582c0058751', lineage).
narrative_ontology:cs_interpretation_layer_present('b770f4b1-9570-40f9-8d50-b582c0058751').
narrative_ontology:cs_reading_relation('b770f4b1-9570-40f9-8d50-b582c0058751', preparedness_transmission__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('b770f4b1-9570-40f9-8d50-b582c0058751', preparedness_transmission__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('b770f4b1-9570-40f9-8d50-b582c0058751', foundational, adaptive_capacity_transmissible_via_live_exercise).
narrative_ontology:cs_axiom_status(adaptive_capacity_transmissible_via_live_exercise, holdable).
narrative_ontology:cs_axiom_grounding('b770f4b1-9570-40f9-8d50-b582c0058751', adaptive_capacity_transmissible_via_live_exercise, empirically_contingent).
narrative_ontology:cs_axiom('b770f4b1-9570-40f9-8d50-b582c0058751', foundational, inspector_expertise_recognizes_novel_signatures).
narrative_ontology:cs_axiom_status(inspector_expertise_recognizes_novel_signatures, holdable).
narrative_ontology:cs_axiom_grounding('b770f4b1-9570-40f9-8d50-b582c0058751', inspector_expertise_recognizes_novel_signatures, empirically_contingent).
narrative_ontology:cs_reference_frame('b770f4b1-9570-40f9-8d50-b582c0058751', adaptive_capacity_as_operational_knowledge).
narrative_ontology:cs_drift_state('b770f4b1-9570-40f9-8d50-b582c0058751', contemporary_post_major_disaster_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('b770f4b1-9570-40f9-8d50-b582c0058751', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__competence_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, civilian_populations).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, emergency_responders).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, civil_defense_agencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, drill_inspectors).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, institutional_memory_holders).
narrative_ontology:constraint_victim(preparedness_transmission__competence_reading, emergency_responders).
narrative_ontology:constraint_vindicates(preparedness_transmission__competence_reading, adaptive_capacity_thesis).
narrative_ontology:constraint_vindicates(preparedness_transmission__competence_reading, embodied_knowledge_transmission).
narrative_ontology:constraint_vindicates(preparedness_transmission__competence_reading, scenario_variance_learning).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, schedules, and runs drills and inspections that exercise the full disaster-response apparatus. Maintains the doctrine that each drill validates knowledge through practice and that novel scenario variations test adaptive capacity rather than rote recall. Carries institutional memory and selects training emphasis based on recognized failure signatures.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, civil_defense_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Participate in drills and inspections where they exercise decision-making under uncertainty and learn to recognize novel failure patterns. They bear the time cost of participation and the cognitive load of scenario variation. The reading asserts they develop genuine adaptive capacity — the ability to improvise effectively when reality deviates from training scenarios.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, emergency_responders, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__competence_reading, emergency_responders, payer).

% Are the referents of the preparedness system. When disaster strikes, their safety depends on whether the responders who trained through drills have developed genuine adaptive capacity or merely performed memorized procedures. This reading asserts that live exercise produces the former.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, civilian_populations, beneficiary,
    powerless, immediate, trapped, national).

% Conduct drills, observe performance, and recognize signatures of adaptive capacity (novel problem-solving, scenario variation handling, emergent leadership) versus signatures of hollow performance (script-reading, scenario rigidity, decision-deflection). Their expertise is the mechanism by which this reading asserts knowledge transmission is validated.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, drill_inspectors, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__competence_reading, drill_inspectors, beneficiary).

% Career civil defense professionals and long-serving emergency responders who carry forward the learned signatures of failure modes and improvisation pathways. Their professional identity is constituted through this transmitted knowledge; they experience drills as live validation of their expertise, not as rote theater.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, institutional_memory_holders, beneficiary,
    moderate, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Exercises the full disaster-response apparatus repeatedly under varied scenarios so that responders develop genuine adaptive capacity: the ability to recognize novel failure signatures and improvise effective responses when reality deviates from training predictions.
% TRANSFER_FUNCTION: Moves time, attention, and institutional resources from normal operations into repeated drill participation, with the exchange being the validation that response capability will work when needed.
% ABSENT_VOICES: Disaster-event observers and post-disaster auditors — people who witness whether the transmitted knowledge actually held in contact with reality. Their perspective is excluded from the drill design itself; their testimony arrives only after an event, too late to revise the constraint.
% DISAPPEARANCE_RATIONALE: If drills and inspections vanished, responders would lose the mechanism for validating adaptive capacity; knowledge transmission would degrade to paper training and simulation, and disaster response would revert to rigid procedure-following under novel circumstances. The next disaster event would test whether responders could improvise or only execute memorized steps.
% FOUNDING_PROBLEM: After major disasters, responders often fail not because they lacked training but because reality presented scenarios not covered by the memorized playbook. The founding problem is: how do you transmit not just procedures but adaptive capacity — the ability to recognize novel failure signatures and improvise?
% FOUNDING_PROBLEM_CORROBORATION: Post-disaster after-action reviews consistently identify 'operators failed to recognize the novel signature' and 'responders could not adapt procedures to the actual situation' as failure modes. Emergency management researchers and disaster sociologists (outside the benefiting agencies) document that rote training produces rigid response under novelty, while adaptive capacity requires live exercise with scenario variation. The live-transmission reading is corroborated by this external evidence.
narrative_ontology:disappearance_verdict(preparedness_transmission__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_transmission__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__competence_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__competence_reading_tests).
:- end_tests(preparedness_transmission__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The competence_reading is claimed as ROPE because it asserts a genuine coordination function (disaster response requires adaptive capacity) solved through collective exercise with low extractive overhead. Extractiveness is low (0.18) because the constraint describes knowledge transmission as its own justification — responders benefit from adaptive capability, agencies benefit from validated response capacity, civilians benefit from effective disaster response. There is no separable extraction mechanism; the gains and costs align. Suppression is low (0.12) because the constraint persists through participant belief that it works, not through coercive enforcement. Theater_ratio is moderate (0.22) because some activity is necessarily performative (scenarios are artificial, pressures are controlled) but the reading asserts the performative frame serves the adaptive-capacity function — the artificiality is the pedagogical point. Accessibility_collapse is moderate-high (0.65) because once the disaster-response framework is understood, alternatives to live exercise (paper training, simulation alone) become visibly insufficient for adaptive capacity; the constraint's plausibility is high. Resistance is low (0.35) because responders generally accept that drills are valuable; opposition comes from external critics questioning whether knowledge transmission actually works. Measurement series show extractiveness and theater_ratio oscillating in a narrow band (extractiveness 0.17–0.22, theater 0.20–0.28) — the constraint is stable, with small seasonal variation from resource constraints and drill scheduling. The metrics are independent of the claimed type (competence_reading is claimed as rope, but extractiveness and theater would be high in husk_reading and moderate in hybrid_reading; the engine will compute per-seat readings, which may diverge).
 *
 * PERSPECTIVAL GAP:
 *   Agency perspective vs. responder perspective vs. post-disaster observer perspective would compute different types. From the agency seat, the constraint is rope — genuine coordination, stable adaptive capacity transmission, inspector expertise validating competence. From the responder seat, extractiveness is lower (they benefit more directly from adaptive capacity); they compute rope with higher beneficiary weighting. From a post-disaster observer who has watched responders fail under novel circumstances, extractiveness is much higher (the constraint did not transmit adaptive capacity) and the reading is husk or hybrid; the observer computes snare or tangled_rope (the constraint persists as ritual despite failed function). The engine computes per-seat types from the structural data; this commentary explains the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil defense agencies are near-symmetric (d ~0.35): they benefit from validated response capacity and bear the coordination cost of designing and running drills. Emergency responders are mild targets (d ~0.55): they bear the time cost and cognitive load of participation, but benefit from genuine adaptive capacity. Institutional memory holders are near-beneficiary (d ~0.25): their professional identity is constituted through the transmitted knowledge; drills validate their expertise. Civilian populations are mild beneficiaries (d ~0.30): they benefit from effective response but bear the ambient disaster risk regardless. The directionality structure is relatively symmetric because this reading asserts the constraint solves a genuine shared problem. If the constraint were husk_reading or hybrid_reading, directionality would shift upward for agencies and inspectors (who benefit from ritual/institutional performance) and downward for responders and civilians (who bear costs without functional benefit).
 *
 * MANDATROPHY ANALYSIS:
 *   The competence_reading is vulnerable to mandatrophy via the founding-problem pathway. The founding problem is real (disaster response requires adaptive capacity) and the constraint addresses it (drills exercise adaptive problem-solving). But the founding problem status is contested (we cannot know adaptive capacity is actually transmitting until a disaster tests it). If post-disaster audits consistently find responders failing under novel scenarios despite heavy drill participation, the founding problem persists (adaptive capacity is still needed) but the constraint has become zombie — it persists through institutional inertia and the belief that drills work, not because the adaptive capacity is actually transmitted. Mandatrophy would resolve via real-world disaster outcome data showing whether adaptive capacity is live or hollow.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is drill-transmitted knowledge genuinely adaptive capacity (novel problem recognition and improvisation), or has the constraint become a hollow ritual that maintains institutional identity without operational validation?',
    'Post-disaster performance audit: measure whether responders under stress recognize novel failure signatures and improvise effectively, or rigidly execute memorized procedures regardless of scenario fit. Compare post-disaster outcomes across agencies with high vs. low drill participation rates and inspector-assessed adaptive capacity. This is the core empirical test that distinguishes this reading from the husk_reading.',
    'If adaptive capacity is validated by post-disaster performance, the competence_reading holds and extractiveness remains low. If post-disaster audits find rigid procedure-following and scenario-blindness despite drill participation, the constraint has degraded to husk_reading, extractiveness rises, and theater_ratio increases sharply. The two readings are empirically distinguishable via real-world disaster outcome data.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, empirical, 'Whether drills transmit adaptive capacity or maintain hollow institutional performance.').

omega_variable(
    reading_stratification_risk,
    'Does adaptive capacity transmit uniformly across organizational levels (civil defense leadership, mid-level coordinators, frontline responders), or does knowledge transmission degrade across levels, creating a bifurcated competence landscape?',
    'Cross-level assessment during drills: test whether mid-level coordinators and frontline responders recognize the same novel failure signatures that leadership-level inspectors are testing, and whether improvisation capacity is distributed or concentrated at leadership. Post-drill debriefs comparing recognized signatures and adaptive responses across organizational levels.',
    'Uniform transmission supports the competence_reading. Stratified transmission (leadership recognizes novel signatures while frontline staff execute memorized procedures) suggests drift toward hybrid_reading: some adaptive capacity remains at engineering and leadership levels, but civilian coordination knowledge has decayed. This is the empirical diagnostic for distinguishing competence_reading from hybrid_reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_stratification_risk, empirical, 'Whether adaptive capacity transmits uniformly or stratifies across organizational hierarchy.').

omega_variable(
    inspector_expertise_dependence,
    'Does the constraint''s capacity to validate adaptive knowledge depend on inspectors who are themselves carriers of embodied memory of novel failure signatures, or has inspector expertise itself degraded to checking memorized procedures?',
    'Inspector retrospective interviews during post-drill analysis: assess whether inspectors recognize novel problem-solving when they see it, and whether they can distinguish adaptive improvisation from scenario-following. Audits of drill reports: are inspectors documenting emergent leadership and signature recognition, or just noting whether steps were performed in sequence?',
    'If inspector expertise remains robust and recognizes adaptive capacity, the competence_reading survives. If inspector expertise has hollowed (they check boxes rather than assess competence), the validation mechanism itself has failed and the constraint is becoming husk_reading. This is the mechanism-level test of whether competence transmission is live or ritualized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inspector_expertise_dependence, empirical, 'Whether inspectors retain expertise to recognize adaptive capacity or have degraded to procedural checkers.').

omega_variable(
    sibling_reading_foreclosure_test,
    'Can the three readings — competence_reading, husk_reading, hybrid_reading — coexist as live positions held by different organizational actors, or does adoption of one reading logically foreclose the others within a single framework?',
    'Organizational survey and archival analysis: assess whether civil defense agencies simultaneously hold different readings (some departments affirming adaptive-capacity transmission, others affirming the constraint has become memorial ritual). Test whether internal contradiction arises or whether agencies can partition the constraint across levels (engineering + infrastructure is competence; civilian coordination is husk). Examine historical agency documents to see whether any reading has been formally repudiated or abandoned.',
    'If readings coexist, they should be modeled as coexists_with relations (different parties, same kernel, no internal contradiction). If internal contradictions force agencies to choose one reading, the relations should be forecloses (one reading''s adoption rules out another within the same framework). If one reading is formally abandoned, it moves from holdable to overridden status.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_test, conceptual, 'Whether the three readings are logically independent or mutually exclusive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_transmission__competence_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(prep_tr_t0, observed).
narrative_ontology:measurement(prep_tr_t5, preparedness_transmission__competence_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement_basis(prep_tr_t5, observed).
narrative_ontology:measurement(prep_tr_t10, preparedness_transmission__competence_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement_basis(prep_tr_t10, observed).
narrative_ontology:measurement(prep_tr_t15, preparedness_transmission__competence_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement_basis(prep_tr_t15, observed).
narrative_ontology:measurement(prep_tr_t20, preparedness_transmission__competence_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(prep_tr_t20, observed).
narrative_ontology:measurement(prep_tr_t25, preparedness_transmission__competence_reading, theater_ratio, 25, 0.21).
narrative_ontology:measurement_basis(prep_tr_t25, observed).
narrative_ontology:measurement(prep_tr_t30, preparedness_transmission__competence_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement_basis(prep_tr_t30, observed).
narrative_ontology:measurement(prep_tr_t40, preparedness_transmission__competence_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(prep_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_transmission__competence_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(prep_be_t0, observed).
narrative_ontology:measurement(prep_be_t5, preparedness_transmission__competence_reading, base_extractiveness, 5, 0.2).
narrative_ontology:measurement_basis(prep_be_t5, observed).
narrative_ontology:measurement(prep_be_t10, preparedness_transmission__competence_reading, base_extractiveness, 10, 0.18).
narrative_ontology:measurement_basis(prep_be_t10, observed).
narrative_ontology:measurement(prep_be_t15, preparedness_transmission__competence_reading, base_extractiveness, 15, 0.17).
narrative_ontology:measurement_basis(prep_be_t15, observed).
narrative_ontology:measurement(prep_be_t20, preparedness_transmission__competence_reading, base_extractiveness, 20, 0.19).
narrative_ontology:measurement_basis(prep_be_t20, observed).
narrative_ontology:measurement(prep_be_t25, preparedness_transmission__competence_reading, base_extractiveness, 25, 0.2).
narrative_ontology:measurement_basis(prep_be_t25, observed).
narrative_ontology:measurement(prep_be_t30, preparedness_transmission__competence_reading, base_extractiveness, 30, 0.18).
narrative_ontology:measurement_basis(prep_be_t30, observed).
narrative_ontology:measurement(prep_be_t40, preparedness_transmission__competence_reading, base_extractiveness, 40, 0.18).
narrative_ontology:measurement_basis(prep_be_t40, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_transmission__competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__competence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_transmission__competence_reading, 0.14).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, preparedness_transmission__husk_reading).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, preparedness_transmission__hybrid_reading).

% DUAL FORMULATION NOTE:
% preparedness_transmission is a contested kernel with three reading-specific constraint stories. competence_reading (this file) asserts drills transmit adaptive capacity and measure it via inspector expertise. husk_reading asserts the same activities are memorial ritual without operational knowledge. hybrid_reading asserts competence is stratified — infrastructure robust, civilian coordination decayed. The readings coexist as live positions held by different organizational actors and are linked via the kernel's authority structure (civil defense doctrine that simultaneously affirms all three framings). Each reading has distinct ε and beneficiary/victim structure; the engine computes per-seat types which may diverge from the claimed types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
