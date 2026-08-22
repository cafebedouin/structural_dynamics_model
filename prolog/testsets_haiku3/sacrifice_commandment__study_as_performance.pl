% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__study_as_performance, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: sacrifice_commandment__study_as_performance
 *   human_readable: Study of Sacrifice Law as Fulfillment of Divine Commandment
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   In halakhic theory, the commandment to offer sacrifices remains binding
 *   after the Second Temple's destruction made literal performance
 *   impossible. The study-as-performance reading holds that intensive
 *   engagement with sacrifice law texts fulfills the divine obligation
 *   directly—that intellectual labor constitutes genuine worship. This
 *   reading stands against two siblings: the performance-only reading (which
 *   treats the commandment as suspended until Temple restoration) and the
 *   archive-maintenance reading (which treats study as preparatory knowledge,
 *   not fulfillment). This constraint instantiates the study-as-performance
 *   reading specifically, with zero extractiveness because no party extracts
 *   from or bears costs in the arrangement; the scholar-worshipper
 *   voluntarily engages and derives both spiritual fulfillment and
 *   intellectual benefit. The mountain claim rests on the reading's assertion
 *   that study is intrinsically valuable—not a constructed substitute for
 *   sacrifice, but the actual fulfillment of the commandment in its textual
 *   form.
 *
 * KEY AGENTS:
 *   - scholar_worshipper: engages in study; derives spiritual and intellectual fulfillment (moderate power, civilizational horizon, mobile exit)
 *   - halakhic_interpretive_tradition: transmits and legitimates the reading (institutional power, non-agent framework entity)
 *   - temple_rebuilding_advocates: excluded from this framework; hold performance-only reading (powerful, would contest the axiom)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__study_as_performance, 0.0).
domain_priors:suppression_score(sacrifice_commandment__study_as_performance, 0.0).
domain_priors:theater_ratio(sacrifice_commandment__study_as_performance, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, extractiveness, 0.0).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__study_as_performance, mountain).
narrative_ontology:human_readable(sacrifice_commandment__study_as_performance, "Study of Sacrifice Law as Fulfillment of Divine Commandment").
narrative_ontology:topic_domain(sacrifice_commandment__study_as_performance, "religious/halakhic").

domain_priors:emerges_naturally(sacrifice_commandment__study_as_performance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__study_as_performance, '4c9dcae4-38b6-4b68-91e2-2184049c8682').
narrative_ontology:cs_kernel_codification('4c9dcae4-38b6-4b68-91e2-2184049c8682', fixed_text).
narrative_ontology:cs_authority_grounding('4c9dcae4-38b6-4b68-91e2-2184049c8682', lineage).
narrative_ontology:cs_interpretation_layer_present('4c9dcae4-38b6-4b68-91e2-2184049c8682').
narrative_ontology:cs_reading_relation('4c9dcae4-38b6-4b68-91e2-2184049c8682', sacrifice_commandment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('4c9dcae4-38b6-4b68-91e2-2184049c8682', sacrifice_commandment__archive_maintenance, coexists_with).
narrative_ontology:cs_axiom('4c9dcae4-38b6-4b68-91e2-2184049c8682', foundational, study_as_intrinsic_worship).
narrative_ontology:cs_axiom_status(study_as_intrinsic_worship, holdable).
narrative_ontology:cs_axiom_grounding('4c9dcae4-38b6-4b68-91e2-2184049c8682', study_as_intrinsic_worship, deontological).
narrative_ontology:cs_axiom('4c9dcae4-38b6-4b68-91e2-2184049c8682', foundational, intellectual_engagement_fulfills_obligation).
narrative_ontology:cs_axiom_status(intellectual_engagement_fulfills_obligation, holdable).
narrative_ontology:cs_axiom_grounding('4c9dcae4-38b6-4b68-91e2-2184049c8682', intellectual_engagement_fulfills_obligation, deontological).
narrative_ontology:cs_reference_frame('4c9dcae4-38b6-4b68-91e2-2184049c8682', study_fulfills_sacrifice_commandment).
narrative_ontology:cs_drift_state('4c9dcae4-38b6-4b68-91e2-2184049c8682', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('4c9dcae4-38b6-4b68-91e2-2184049c8682', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__study_as_performance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__study_as_performance, scholar_worshipper).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engages in intensive study of sacrifice law texts and practices, deriving both intellectual understanding and spiritual fulfillment from this engagement. The framework holds that this study constitutes direct fulfillment of the divine commandment to offer sacrifices, making intellectual labor itself a form of worship. No coercion exists; participation is volitional and self-directed.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, scholar_worshipper, beneficiary,
    moderate, civilizational, mobile, universal).

% The interpretive lineage (from medieval commentaries onward) that transmits and adjudicates the reading that study fulfills the commandment. It provides canonical texts, hermeneutical methods, and legitimacy structures for the scholar-worshipper's engagement. Non-agent entity: the tradition itself does not act, but the practitioners within it do.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, halakhic_interpretive_tradition, agenda_setter,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(sacrifice_commandment__study_as_performance, halakhic_interpretive_tradition).

% Hold the reading that sacrifice commandment requires physical Temple restoration and literal animal sacrifice; study alone is insufficient. They would argue that treating study as fulfillment spiritualizes away the commandment's material content and delays the concrete obligation. They are excluded from the framework that this constraint instantiates because the framework's axiom (study is intrinsically valuable worship) contradicts their core premise (physical performance is required).
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, temple_rebuilding_advocates, excluded,
    powerful, civilizational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves and transmits the technical, liturgical, and theological knowledge required for sacrifice practice; coordinates the scholarly community around collective interpretation of these texts; maintains the continuity of halakhic reasoning across generations without Temple access.
% TRANSFER_FUNCTION: Transfers intellectual and spiritual engagement from the scholar-worshipper to the fulfillment of a divine commandment, via the interpretive mechanism that the reading provides. No material transfer occurs; the movement is of obligation from physical performance to cognitive engagement.
% ABSENT_VOICES: Temple rebuilding advocates and literalist-performance readings are structurally excluded from this framework's legitimacy claims. Practical skeptics—those who doubt study can substitute for physical commandment—are also absent. Neither group is present to contest the reading's core axiom.
% DISAPPEARANCE_RATIONALE: If this constraint (the reading that study fulfills the commandment) vanished, the scholarly community would reorganize: some toward pure archive maintenance (study as preparation), some toward the performance-only reading (study as insufficient), some toward revival of literal sacrifice demands. The constraint's disappearance would not leave the world unchanged, because the legitimacy it grants to scholar-worship would be gone, and the interpretive tradition would lose its foundational warrant.
% FOUNDING_PROBLEM: After the Second Temple's destruction, the commandment to offer sacrifices remains binding halakhically, but material sacrifice is impossible. The interpretive tradition faced a choice: suspend the commandment, reinterpret it, or find an alternate mode of fulfillment. This reading solves the founding problem by reinterpreting 'offering' as intellectual engagement and study.
% FOUNDING_PROBLEM_CORROBORATION: Medieval halakhic commentaries (Maimonides, Nahmanides, and the Tosafot) attest that study of sacrifice law fulfills the commandment, drawing on earlier Talmudic statements equating the study of a service with its performance. Contemporary halakhic scholars maintain this reading. Temple rebuilding advocates and performance-only readings contest this status, arguing the founding problem is not solved—the commandment is suspended, not fulfilled. The Talmudic phrase 'one who studies the service is as if he performed it' is cited as foundational corroboration from within the tradition itself, not from external observers.
narrative_ontology:disappearance_verdict(sacrifice_commandment__study_as_performance, contested).
narrative_ontology:founding_problem_status(sacrifice_commandment__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__study_as_performance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_commandment__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__study_as_performance, 0.0, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__study_as_performance_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, ExtMetricName, E),
    domain_priors:suppression_score(sacrifice_commandment__study_as_performance, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(sacrifice_commandment__study_as_performance),
    narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(sacrifice_commandment__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is zero because the framework asserts study is intrinsically valuable fulfillment, not a rent-generating arrangement or an extraction mechanism. There is no victim class—no one is harmed by the reading that study fulfills the commandment. Suppression is zero: participation is entirely volitional; no coercion mechanism exists. Theater ratio is zero: the reading claims study is genuinely worship, not performative substitution. Accessibility collapse is very high (0.95) because once the halakhic framework is accepted, alternative interpretations (performance-only, archive-maintenance) become structurally ruled out at the level of the closed halakhic system. Resistance is very low (0.05) because those who accept the framework experience no resistance—study flows naturally within it. Those who reject the framework (Temple advocates, performance-only readers) are excluded from the constraint entirely; their resistance is structural opposition, not resistance within the system. The measurement series shows stability across the interval (all zero extractiveness) because the reading claims to be a natural principle, not a constructed substitution subject to drift.
 *
 * PERSPECTIVAL GAP:
 *   The primary seat divergence is between those INSIDE the framework (scholar-worshippers who accept the reading) and those OUTSIDE (Temple advocates, performance-only readers). The insiders experience the constraint as a mountain—a natural principle that solves a genuine problem and costs them nothing. The outsiders experience it as a constructed substitution that forecloses their reading and allows others to claim fulfillment without literal sacrifice. This is not a per-seat classification divergence within a shared constraint; it is a framework-level disagreement about whether the constraint itself is natural law or constructed reading. The engine does not compute divergent types here because the sibling readings are different constraints (other JSON files), not different seats of the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The scholar-worshipper is a beneficiary (derives spiritual and intellectual goods from the reading's legitimacy) but not a target of extraction. There is no payer class. The relationship is symmetric and intrinsically beneficial: engagement in the reading's terms yields fulfillment. Directionality for the scholar-worshipper would be near 0.0 (full beneficiary) on d, yielding negative or near-zero effective extraction (subsidy rather than tax). No other agent seat exists within this constraint's operation, so no cross-seat divergence in directionality occurs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live in the view of this reading: the commandment to offer sacrifices remains binding, and study fulfills it. The reading does not treat the commandment as dead (superseded, no longer binding) or merely archived for future use. If the founding problem were dead (commandment superseded or suspended), the constraint would lose its warrant and become mandatrophic. However, the performance-only reading would claim the founding problem is NOT solved—the commandment is still unfulfilled because study alone is not performance. This disagreement is rooted in the core axiom (study_as_intrinsic_worship) and is recorded in the omega variable exploring whether the reading is foundational or constructed. Mandatrophy is not present in this reading itself; it is present in the performance-only reading's objection.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_sufficiency_boundary,
    'Is the reading''s core claim—that study intrinsically fulfills the sacrifice commandment—a genuine theological principle discovered in the tradition, or a constructed reinterpretation that treats study as a substitute when literal performance became impossible?',
    'Historical-textual analysis: does the equation of study with performance appear in pre-70 CE sources (foundational principle) or primarily post-70 (problem-solving reinterpretation)? Do the source texts claim intrinsic sufficiency or practical substitution?',
    'If the reading is a genuine principle (pre-destruction sources), the constraint is a mountain — natural law of halakhic reasoning. If it is a constructed substitution, it is a rope (coordination mechanism) and may require beneficiary/victim analysis if enforcement or incentive structures exist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_sufficiency_boundary, empirical, 'Whether the study-as-fulfillment principle is foundational or constructed.').

omega_variable(
    competing_halakhic_readings_foreclosure,
    'Can the study-as-performance reading coexist within a single halakhic framework with the performance-only reading, or do they logically foreclose each other?',
    'Examination of whether a single halakhic authority (e.g., a contemporary posek) can hold both: study partially fulfills the commandment AND literal performance is still required when possible. If yes, they coexist; if no, one forecloses the other within the same framework.',
    'If coexistent, the two readings compete but don''t eliminate each other (coexists_with relation). If foreclosing, this reading''s axiom (study is intrinsically valuable worship) logically rules out the sibling''s core premise (physical performance alone fulfills). Foreclosure would be rare and would require very strong structural contradiction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competing_halakhic_readings_foreclosure, conceptual, 'Logical foreclosure vs. practical coexistence of competing sacrifice readings.').

omega_variable(
    natural_law_beneficiary_anomaly,
    'Why does this mountain have a declared beneficiary (scholar_worshipper) when natural laws typically have no beneficiaries? Is the scholar-worshipper a real-world actor who benefits, or is the ''benefit'' here the intrinsic fulfillment the reading claims, making the beneficiary identical to the practice itself?',
    'FSM (false-summit) analysis: if scholar-worshippers who benefit from this reading''s legitimacy exist as real agents, the constraint may be a false summit—a constructed reading benefiting identifiable parties, not a natural law. Examine whether the reading persists because it is true or because it benefits the scholarly establishment.',
    'If FSM fires, the constraint reclassifies to tangled_rope (coordination of scholars + asymmetric extraction of honor/authority from non-scholars). If FSM does not fire, the beneficiary entry is legitimate and the constraint remains mountain (the ''benefit'' is the intrinsic fulfillment, not rent collection).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_beneficiary_anomaly, conceptual, 'Natural law vs. false-summit ambiguity in the mountain claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__study_as_performance, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__study_as_performance, base_extractiveness, 0, 0.0).
narrative_ontology:measurement_basis(sacr_be_t0, observed).
narrative_ontology:measurement(sacr_be_t10, sacrifice_commandment__study_as_performance, base_extractiveness, 10, 0.0).
narrative_ontology:measurement_basis(sacr_be_t10, observed).
narrative_ontology:measurement(sacr_be_t20, sacrifice_commandment__study_as_performance, base_extractiveness, 20, 0.0).
narrative_ontology:measurement_basis(sacr_be_t20, observed).
narrative_ontology:measurement(sacr_be_t30, sacrifice_commandment__study_as_performance, base_extractiveness, 30, 0.0).
narrative_ontology:measurement_basis(sacr_be_t30, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_commandment__study_as_performance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__study_as_performance, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_commandment__study_as_performance, 0.0).
narrative_ontology:affects_constraint(sacrifice_commandment__study_as_performance, sacrifice_commandment__performance_only).
narrative_ontology:affects_constraint(sacrifice_commandment__study_as_performance, sacrifice_commandment__archive_maintenance).

% DUAL FORMULATION NOTE:
% The sacrifice_commandment kernel decomposes into three structurally distinct constraints: study_as_performance (this file, zero extractiveness, mountain, intrinsic fulfillment); performance_only (literal sacrifice required, commandment suspended without Temple, may carry extraction if enforced revival movements exist); archive_maintenance (study as preparation, knowledge preservation function, coordination only, no extraction). Each reading has its own ε, beneficiary/victim structure, and type. They are linked as a constraint family because they share the kernel (the binding status of the commandment) and compete for legitimacy in halakhic interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
