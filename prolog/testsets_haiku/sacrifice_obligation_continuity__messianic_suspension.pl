% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__messianic_suspension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__messianic_suspension, []).

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
 *   constraint_id: sacrifice_obligation_continuity__messianic_suspension
 *   human_readable: Sacrifice Obligation Suspended Pending Messianic Restoration
 *   domain: religious_law/ritual_studies
 *
 * SUMMARY:
 *   After the destruction of the Second Temple in 70 CE, Jewish law faced a
 *   crisis: the sacrifice obligation remained binding, but performance became
 *   impossible. The messianic_suspension reading emerged as the authoritative
 *   rabbinic response: the obligation is not violated (because it is
 *   suspended, not demanded); it is not fulfilled (because suspension is not
 *   performance); it waits upon messianic restoration, when the Temple will
 *   be rebuilt and sacrifice reactivated. Study of sacrifice law maintains
 *   textual readiness and keeps the skill tradition alive for that future
 *   reactivation. This reading coexists in tension with three siblings:
 *   archival_preservation (obligation is dead, study is cultural memory);
 *   study_as_performance (study IS fulfillment); performance_only (study is
 *   mere preparation, not fulfillment). The messianic_suspension reading is
 *   the dominant reading in mainstream rabbinic Judaism, but all four
 *   readings persist as live interpretive options in different communities
 *   and texts.
 *
 * KEY AGENTS:
 *   - Textual tradition stewards (rabbinical scholars, liturgical authorities): set the maintenance protocol, decide what counts as adequate study, enforce interpretive boundaries
 *   - Observant community members: bear the burden of study and readiness practice, carry the psychological investment of non-performance, maintain hope in messianic restoration
 *   - Messianic hope carriers: communities for whom the restoration horizon is alive; the suspension framework vindicates their worldview by treating absence as a structural feature of the current age, not a contradiction
 *   - Critics and exited practitioners: argue the obligation is dead or superseded; excluded from the conversation that produced the suspension framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__messianic_suspension, 0.48).
domain_priors:suppression_score(sacrifice_obligation_continuity__messianic_suspension, 0.32).
domain_priors:theater_ratio(sacrifice_obligation_continuity__messianic_suspension, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, extractiveness, 0.48).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, resistance, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__messianic_suspension, scaffold).
narrative_ontology:human_readable(sacrifice_obligation_continuity__messianic_suspension, "Sacrifice Obligation Suspended Pending Messianic Restoration").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__messianic_suspension, "religious_law/ritual_studies").

domain_priors:requires_active_enforcement(sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:has_sunset_clause(sacrifice_obligation_continuity__messianic_suspension).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__messianic_suspension, 'e82056d1-c922-4ad8-8f80-64bc5bdebb38').
narrative_ontology:cs_kernel_codification('e82056d1-c922-4ad8-8f80-64bc5bdebb38', fixed_text).
narrative_ontology:cs_authority_grounding('e82056d1-c922-4ad8-8f80-64bc5bdebb38', lineage).
narrative_ontology:cs_interpretation_layer_present('e82056d1-c922-4ad8-8f80-64bc5bdebb38').
narrative_ontology:cs_reading_relation('e82056d1-c922-4ad8-8f80-64bc5bdebb38', sacrifice_obligation_continuity__archival_preservation, coexists_with).
narrative_ontology:cs_reading_relation('e82056d1-c922-4ad8-8f80-64bc5bdebb38', sacrifice_obligation_continuity__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('e82056d1-c922-4ad8-8f80-64bc5bdebb38', sacrifice_obligation_continuity__performance_only, coexists_with).
narrative_ontology:cs_axiom('e82056d1-c922-4ad8-8f80-64bc5bdebb38', foundational, obligation_persists_across_performance_gap).
narrative_ontology:cs_axiom_status(obligation_persists_across_performance_gap, holdable).
narrative_ontology:cs_axiom_grounding('e82056d1-c922-4ad8-8f80-64bc5bdebb38', obligation_persists_across_performance_gap, deontological).
narrative_ontology:cs_axiom('e82056d1-c922-4ad8-8f80-64bc5bdebb38', foundational, messianic_restoration_is_coherent_horizon).
narrative_ontology:cs_axiom_status(messianic_restoration_is_coherent_horizon, holdable).
narrative_ontology:cs_axiom_grounding('e82056d1-c922-4ad8-8f80-64bc5bdebb38', messianic_restoration_is_coherent_horizon, theological).
narrative_ontology:cs_reference_frame('e82056d1-c922-4ad8-8f80-64bc5bdebb38', obligation_suspended_pending_restoration).
narrative_ontology:cs_drift_state('e82056d1-c922-4ad8-8f80-64bc5bdebb38', post_twentieth_century_secularization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e82056d1-c922-4ad8-8f80-64bc5bdebb38', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, textual_tradition_stewards).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, messianic_hope_carriers).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__messianic_suspension, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__messianic_suspension, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__messianic_suspension_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_continuity__messianic_suspension, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_obligation_continuity__messianic_suspension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48 at interval end) because the framework extracts readiness burden and intellectual labor from practitioners while avoiding guilt or violation — the suspension avoids the full force of either dead-letter treatment (which would be a shame or crisis) or continuous non-performance (which would be continuous violation). Theater ratio rises from 0.42 to 0.58 across the interval, indicating increasing performative maintenance relative to functional activity. This rise reflects the historical reality: as centuries pass without messianic arrival, the practical function of readiness (actually being able to sacrifice if the Temple were restored) declines relative to the performative function (maintaining the appearance and coherence of the obligation framework itself). Suppression is low-to-moderate (0.32 at interval end) because the framework does not require coercion to maintain — practitioners are identity-locked and the suspension framework aligns with their theological worldview. Resistance is moderate (0.41) because critics and exited practitioners challenge the framework, but their voices are systematically excluded from the authoritative conversation. Accessibility_collapse is high (0.71) because practitioners embedded in the tradition have very limited alternatives — exit means leaving the community, and alternatives (like archival_preservation or study_as_performance) are available only within the tradition, not outside it. The measurement series captures the slow drift toward greater performativity and lower practical extractiveness-to-burden ratio as time passes without messianic restoration.
 *
 * PERSPECTIVAL GAP:
 *   Textual stewards experience the framework as genuine obligation-maintenance: they coordinate a living tradition and preserve crucial skill. Observant practitioners experience it as a readiness burden with deferred payoff: they study and maintain readiness but know they will never sacrifice. Critics experience it as performative cover: they see the framework as allowing the community to pretend the obligation is binding while treating it as dead. The engine computes these as different directionalities: stewards sit near beneficiary (they control the apparatus, set standards, collect authority); practitioners sit near symmetric or payer (they bear the burden, receive hope and community coherence); critics are excluded rather than coordinated. The structural asymmetry is the gap between the framing (the obligation is genuinely suspended, waiting for restoration) and the operative reality (the obligation functions as indefinitely deferred, possibly indefinitely).
 *
 * DIRECTIONALITY LOGIC:
 *   Textual stewards have high beneficiary directionality (d ≈ 0.1–0.2) because they set and enforce the maintenance protocol, decide what counts as adequate study, and collect authority and legitimacy from their role as interpreters. Observant practitioners have moderate-to-target directionality (d ≈ 0.45–0.65) because they bear the readiness burden (bearing cost) but also receive community coherence and hope (receiving benefit). The secondary_role=beneficiary for practitioners captures this dual position. Critics have excluded directionality (not coordinated, not integrated into the framework) — they would target directionality if they were inside, but they are structurally out. The absence of current victims reflects the core feature of the constraint: the obligation is suspended, not active, so no one is currently violating it. Victims would emerge if the obligation were reactivated without Temple restoration (continuous violation) or if it were declared dead (abandonment of a binding law). The suspension framework exists precisely to avoid both.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: the Temple remains absent and messianic restoration has not occurred. The suspension framework solves a genuine coordination problem: how to hold an obligation as binding while acknowledging non-performance. But the mandatrophy question is whether the framework's core function (readiness maintenance) persists or has atrophied into pure performance. The theater_ratio rise from 0.42 to 0.58 suggests slow atrophy: the practical function (being ready to sacrifice) has declined relative to the performative function (maintaining the coherence of the obligation-framework). This is not yet piton — the obligation structure persists and practitioners genuinely hold messianic hope — but it is a drift in that direction. A full mandatrophy diagnosis would require tracking the point at which no practitioner actually believes readiness would enable reactivation, and no steward could honestly articulate what reactivation would look like.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_vs_effective_abandonment,
    'Is the suspension framework a genuine maintenance protocol that keeps the obligation binding and deferred, or is it performative cover for effective abandonment — a framework that lets the community avoid declaring the obligation dead while treating it as if it were?',
    'Examine whether study and readiness practices actually maintain the skill, knowledge, and collective readiness that would enable rapid reactivation if the Temple were restored. Compare against frameworks that explicitly declare obligations superseded (e.g., reform Jewish theology). Interview practitioners about whether they experience the obligation as genuinely suspended (with future reactivation possible) or as dead (with study as cultural preservation).',
    'If suspension is genuine, the constraint is a scaffold with moderate extractiveness (the readiness burden is real but not guilt-laden). If it is performative cover, the constraint shifts toward piton (theater-ratio rises, the main function is maintaining the appearance of obligation while avoiding its demands).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suspension_vs_effective_abandonment, empirical, 'Whether the suspension is functional maintenance or performative cover for abandonment.').

omega_variable(
    messianic_timeline_indeterminacy,
    'When messianic restoration occurs is radically indeterminate — it could be centuries away, never, or is already underway depending on the theological reading. Does this indeterminacy make the suspension framework a genuine maintenance structure, or does it collapse the framework into a perpetual deferral that functions as indefinite non-obligation?',
    'Trace how the community has adjusted the framework over centuries without messianic arrival. Examine whether the framework''s functional role changed when the messianic expectation shifted from imminent to indefinite. Compare against explicit dead-letter declarations in legal systems and how they are sustained differently.',
    'High indeterminacy with low messianic expectation (sunset clause practically infinite) pushes the constraint toward piton (theater-ratio rises; the performative aspect of maintaining readiness increases as actual reactivation becomes less plausible). Low indeterminacy or high expectation keeps it as genuine scaffold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(messianic_timeline_indeterminacy, conceptual, 'Whether indeterminate messianicity collapses the suspension into indefinite deferral.').

omega_variable(
    study_adequacy_and_authority,
    'What counts as adequate study and readiness is decided by textual stewards with no objective metric. Does the study apparatus extract authority and deference from practitioners while remaining vague about what would demonstrate readiness, or is there genuine accountability to coherent standards?',
    'Examine the criteria stewards apply for adequate study. Are they transparent and falsifiable, or opaque and subject to reinterpretation? Do practitioners challenge inadequate study, or is dissent suppressed? Compare against other obligation-maintenance systems with clear metrics.',
    'High opacity and practitioner deference suggests suppression and extraction; the constraint shifts toward snare-flavored tangled rope. Transparent standards and practitioner accountability suggest genuine coordination; it remains pure scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(study_adequacy_and_authority, empirical, 'Whether study adequacy is genuinely accountable or extractively opaque.').

omega_variable(
    kernel_reading_scope,
    'This constraint instantiates the messianic_suspension reading of the sacrifice_obligation_continuity kernel. The sibling readings (archival_preservation, study_as_performance, performance_only) produce structurally different constraints with different ε values, victim sets, and claim/metric profiles. Are these genuinely alternative readings of a single contested kernel, or distinct constraints masquerading as readings of one?',
    'Examine whether the sibling readings can coherently coexist in a single interpretive tradition or whether they foreclose each other. Determine whether practitioners in the messianic_suspension reading would acknowledge the legitimacy of the archival_preservation or study_as_performance readings, or whether they treat them as contradictory.',
    'If coexistence is possible, the readings share a kernel and network edges are appropriate. If they foreclose each other, they are distinct constraints that happen to address the same textual domain, not alternative readings of one kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_scope, conceptual, 'Whether the four readings are alternative interpretations of one kernel or structurally distinct constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__messianic_suspension, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sacr_tr_t5, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 5, 0.47).
narrative_ontology:measurement(sacr_tr_t10, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 10, 0.52).
narrative_ontology:measurement(sacr_tr_t15, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 15, 0.56).
narrative_ontology:measurement(sacr_tr_t20, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(sacr_be_t5, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(sacr_be_t10, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(sacr_be_t15, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 15, 0.46).
narrative_ontology:measurement(sacr_be_t20, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 20, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(sacr_su_t5, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 5, 0.25).
narrative_ontology:measurement(sacr_su_t10, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(sacr_su_t15, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 15, 0.3).
narrative_ontology:measurement(sacr_su_t20, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 20, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__messianic_suspension, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_continuity__messianic_suspension, 0.12).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity__archival_preservation).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity__performance_only).

% DUAL FORMULATION NOTE:
% The sacrifice_obligation_continuity kernel contains four constraint readings that differ in how they treat the obligation's status after Temple destruction. Messianic_suspension (this constraint) treats the obligation as suspended pending messianic restoration; archival_preservation treats it as dead; study_as_performance treats study as fulfillment; performance_only treats only physical performance as fulfillment. Each reading has structurally different ε, beneficiary/victim sets, and type. They are linked via network.affects_constraints because they share a kernel and the viability of each reading influences the coherence of the others. A shift in one reading's acceptance (e.g., widespread adoption of archival_preservation) would alter the coordination landscape for all four.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
