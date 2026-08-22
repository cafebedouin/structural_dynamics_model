% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__study_as_performance, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: kodashim_commandment_status__study_as_performance
 *   human_readable: Study-as-Performance Reading of Kodashim Commandment Status
 *   domain: religious/halakhic/commitment_system
 *
 * SUMMARY:
 *   The kodashim (sacrificial) commandments present a unique halakhic
 *   problem: the Temple's destruction in 70 CE rendered their physical
 *   performance impossible, yet the commandments remain in the Torah. Three
 *   readings structure the response: performance_only treats them as
 *   suspended (husk); messianic_deferral treats them as temporarily suspended
 *   with future restoration; study_as_performance treats intellectual
 *   engagement with the laws as fulfilling the commandment itself. This story
 *   instantiates the study_as_performance reading: studying the sacrificial
 *   laws *is* the commandment's current mode of operation — the kernel
 *   remains occupied through study. No extraction from a performance gap
 *   because there is no gap; study maintains full commandment force. Victim
 *   set is empty because no one is harmed by non-performance — the
 *   commandment is being fulfilled through study.
 *
 * KEY AGENTS:
 *   - halakhic_authorities_study_position: Primary agenda_setter (institutional/analytical) — maintains the study-equivalence doctrine as authoritative interpretation
 *   - scholarly_community_kodashim: Beneficiary (organized/biographical) — builds intellectual/religious capital through kodashim study; their authority derives from mastery of this corpus
 *   - observant_jews_general: Beneficiary (organized/biographical) — gain accessible pathway to commandment fulfillment without Temple; study substitutes for inaccessible performance
 *   - messianic_deferral_adherents: Observer/coexisting (institutional/generational) — hold sibling reading; not excluded but structurally adjacent
 *   - performance_only_adherents: Observer/excluded (institutional/generational) — hold sibling reading that denies current force; their position is marginalized by study-equivalence dominance
 *   - analytical_halakhic_observer: Observer (analytical/civilizational) — sees full structure across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__study_as_performance, 0.02).
domain_priors:suppression_score(kodashim_commandment_status__study_as_performance, 0.05).
domain_priors:theater_ratio(kodashim_commandment_status__study_as_performance, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, extractiveness, 0.02).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__study_as_performance, mountain).
narrative_ontology:human_readable(kodashim_commandment_status__study_as_performance, "Study-as-Performance Reading of Kodashim Commandment Status").
narrative_ontology:topic_domain(kodashim_commandment_status__study_as_performance, "religious/halakhic/commitment_system").

domain_priors:emerges_naturally(kodashim_commandment_status__study_as_performance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__study_as_performance, '297025c9-fd47-4d57-8435-647e78adc910').
narrative_ontology:cs_kernel_codification('297025c9-fd47-4d57-8435-647e78adc910', fixed_text).
narrative_ontology:cs_authority_grounding('297025c9-fd47-4d57-8435-647e78adc910', lineage).
narrative_ontology:cs_interpretation_layer_present('297025c9-fd47-4d57-8435-647e78adc910').
narrative_ontology:cs_reading_relation('297025c9-fd47-4d57-8435-647e78adc910', kodashim_commandment_status__messianic_deferral, coexists_with).
narrative_ontology:cs_reading_relation('297025c9-fd47-4d57-8435-647e78adc910', kodashim_commandment_status__performance_only, coexists_with).
narrative_ontology:cs_axiom('297025c9-fd47-4d57-8435-647e78adc910', foundational, study_equals_performance_kodashim).
narrative_ontology:cs_axiom_status(study_equals_performance_kodashim, holdable).
narrative_ontology:cs_axiom_grounding('297025c9-fd47-4d57-8435-647e78adc910', study_equals_performance_kodashim, deontological).
narrative_ontology:cs_axiom('297025c9-fd47-4d57-8435-647e78adc910', secondary, commandment_force_requires_no_physical_conditions).
narrative_ontology:cs_axiom_status(commandment_force_requires_no_physical_conditions, holdable).
narrative_ontology:cs_axiom_grounding('297025c9-fd47-4d57-8435-647e78adc910', commandment_force_requires_no_physical_conditions, deontological).
narrative_ontology:cs_reference_frame('297025c9-fd47-4d57-8435-647e78adc910', post_churban_rabbinic_reconstitution).
narrative_ontology:cs_drift_state('297025c9-fd47-4d57-8435-647e78adc910', contemporary_halakhic_consensus, gap(stable, minor, true)).
narrative_ontology:cs_created_at('297025c9-fd47-4d57-8435-647e78adc910', '2026-08-10T14:30:00Z').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__study_as_performance, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, halakhic_authorities_study_position).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, scholarly_community_kodashim).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, observant_jews_general).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__study_as_performance, study_equivalence_doctrine).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__study_as_performance, commandment_continuity_through_intellectual_engagement).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__study_as_performance, oral_torah_as_living_performance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rabbinic authorities (poskim, roshei yeshiva, halakhic decisors) who maintain and transmit the study-equivalence doctrine. They define what counts as valid kodashim study, authorize curricula, and adjudicate boundary cases. Their institutional authority derives from being the designated interpreters of the kernel's continued occupation. Exit is identity_locked: their professional and religious identity is constituted through this interpretive role; abandoning study-equivalence would dissolve their authority structure.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, halakhic_authorities_study_position, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__study_as_performance, halakhic_authorities_study_position, beneficiary).

% Scholars, students, and teachers who build careers and communal standing through mastery of kodashim literature (Mishnah Kodashim, Talmud Zevachim/Menachot, Rambam Kodashim, commentaries). Their intellectual capital, teaching positions, and religious prestige depend on the kernel remaining occupied — if the commandment were truly suspended (performance_only), their expertise would lose its halakhic force. Exit is constrained: they could shift to other areas of Torah study, but kodashim specialization represents sunk investment with limited transferability.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, scholarly_community_kodashim, beneficiary,
    organized, biographical, constrained, global).

% Observant Jews who fulfill the kodashim commandments through daily study (e.g., learning Mishnah Kodashim, reciting korbanot passages in prayer, studying sacrificial laws during the Three Weeks). They experience no harm from non-performance because the reading teaches that study *is* the current fulfillment. Exit is mobile: they could adopt messianic_deferral or performance_only readings with minimal personal cost — the readings coexist in the halakhic marketplace.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, observant_jews_general, beneficiary,
    organized, biographical, mobile, global).

% Authorities and communities (e.g., certain Temple Mount movements, messianic-oriented yeshivot) who hold that the commandments are suspended but will be restored. They engage in study *as preparation* (not as fulfillment), maintain kohanic lineages, and prepare vessels. They are not excluded — their reading coexists — but they occupy a different structural position: study maintains readiness, not current force. Exit is identity_locked: their communal identity is bound to the restoration narrative.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, messianic_deferral_adherents, observer,
    institutional, generational, identity_locked, global).

% Minority positions (historically: some Karaites, certain rationalist medieval voices, modern academic critics) who hold that without a Temple and altar, the kodashim commandments simply cannot be fulfilled — they are suspended husks. Their reading is structurally marginalized by the dominance of study-equivalence in mainstream Orthodoxy. Exit is trapped: adopting study-equivalence would require accepting a hermeneutic they consider invalid; maintaining their position isolates them from the dominant halakhic conversation.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, performance_only_adherents, excluded,
    institutional, generational, trapped, global).

% External observer (scholar of religion, legal theorist, philosopher of halakha) who analyzes the constraint system without being subject to its authority. Sees all three readings as live interpretations of the same kernel, mapping their structural relationships, beneficiary distributions, and power dynamics. Not a participant in the halakhic system; exit is analytical by definition.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, analytical_halakhic_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_commandment_status__study_as_performance, diffuse).
narrative_ontology:fixing_cost_class(kodashim_commandment_status__study_as_performance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the ongoing force and relevance of the kodashim commandments after the Temple's destruction by transmuting physical performance into intellectual engagement — the commandments remain 'occupied' and actionable through study.
% TRANSFER_FUNCTION: Moves interpretive authority and religious capital from the (absent) Temple performance system to the scholarly/rabbinic study system. The 'gains' are not material extraction but the maintenance of commandment continuity and the authority structure that administers it.
% ABSENT_VOICES: Those who experience the Temple's absence as irreducible loss — mourners of the churban who cannot accept study as substitution. Also: potential converts or baalei teshuva who encounter the system as a given without participating in the founding interpretive act. Their objection would be: 'You have replaced the thing with the study of the thing and called it the same.'
% DISAPPEARANCE_RATIONALE: If study-as-performance vanished overnight, the kodashim commandments would revert to suspended/husk status (performance_only) or pure future-orientation (messianic_deferral). The halakhic authority structure would lose its primary mechanism for maintaining commandment continuity post-Temple. The daily religious practice of millions (korbanot recitation, kodashim study cycles) would lose its halakhic grounding. The world of halakhic observance would rearrange fundamentally.
% FOUNDING_PROBLEM: After the Temple's destruction (70 CE), the kodashim commandments — a major portion of Torah law — became physically unperformable. The founding problem: how to maintain the force, relevance, and continuity of divine commandments whose performance conditions no longer exist, without declaring them obsolete or purely messianic.
% FOUNDING_PROBLEM_CORROBORATION: The Temple remains destroyed; the commandments remain in the Torah; the problem of their status persists. Corroborated by all three readings: performance_only admits the problem is live (hence 'suspended'), messianic_deferral admits it is live (hence 'deferred'), and study_as_performance admits it is live (hence 'study fulfills'). No reading claims the problem is resolved. The founding problem is attested by the entire halakhic tradition post-70 CE, not only by study-equivalence beneficiaries.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__study_as_performance, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(kodashim_commandment_status__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__study_as_performance, 0.02, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__study_as_performance_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, ExtMetricName, E),
    domain_priors:suppression_score(kodashim_commandment_status__study_as_performance, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(kodashim_commandment_status__study_as_performance),
    narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(kodashim_commandment_status__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.02) because the reading eliminates the performance gap entirely — study IS the fulfillment, not a substitute for it. Suppression is minimal (0.05) because no one is coerced into this reading; alternative readings (messianic_deferral, performance_only) persist as live positions. Theater_ratio is low but non-zero (0.1) because the intellectual performance of study can become performative — learning kodashim without transformative engagement risks becoming ritualized substitute. Accessibility_collapse is very high (0.92) because once the study-equivalence principle is accepted, the alternative (commandment is dead/suspended) becomes structurally inaccessible — the kernel *must* remain occupied. Resistance is near-zero (0.03) because the reading resolves the cognitive dissonance of 'commandments that cannot be performed' without requiring institutional change or messianic waiting. The slight rise in extractiveness and theater_ratio over 2000 years reflects institutionalization: what began as a radical interpretive move (Rabbinic innovation post-70 CE) became the dominant framework, with authorities benefiting from maintaining the study-equivalence monopoly.
 *
 * PERSPECTIVAL GAP:
 *   From the halakhic authority seat (agenda_setter), the constraint is Mountain: divine law, immutable, study-equivalence is revealed truth. From the scholarly community seat (beneficiary), it is Rope: genuine coordination — study organizes communal intellectual life around a shared corpus. From the observant Jew seat (beneficiary), it is Mountain: the commandment is simply *being fulfilled* through study; no extraction, no coercion. From the messianic_deferral seat (observer), it is a competing Mountain claim — different ontological framing of the same kernel. From the performance_only seat (excluded), the constraint appears as a constructed Snare: study-equivalence masks the reality of loss and prevents mourning/restoration orientation. The engine computes these per-seat classifications from power/exit/beneficiary declarations; the claimed_type (Mountain) reflects the reading's own self-understanding.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic authorities (agenda_setter) are structural beneficiaries: they hold interpretive monopoly over what counts as valid study, deriving authority from the kernel's continued occupation. Scholarly community (beneficiary) gains intellectual/religious capital — their expertise in kodashim is valuable precisely because the commandment remains 'alive' through study. Observant Jews (beneficiary) gain accessible fulfillment pathway — no one is harmed by non-performance because performance is *not required*; study suffices. No victims declared because the reading's internal logic denies harm: the commandment is fulfilled, not evaded. The slight extractiveness (0.02) reflects institutional maintenance costs of the study-equivalence framework, not extraction from subjects.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to maintain commandment force post-Temple) remains live — the Temple is still absent. The arrangement has not atrophied; it has *adapted*. Mandatrophy is resolved negatively: the mandate has not outlived its function because the function (commandment continuity) is still served. The study-equivalence doctrine is not a zombie institution; it is the living resolution of the founding problem. No mandatrophy to resolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Is the kodashim commandment status a genuine natural law (divine constitution) or a constructed halakhic framework with identifiable beneficiaries?',
    'Theological-philosophical analysis: if the commandment structure is divinely ordained and immutable, it is Mountain; if it is a rabbinic construction maintaining authority through study-equivalence, it exhibits false_summit_mountain dynamics.',
    'If constructed with beneficiaries (halakhic authorities who maintain interpretive monopoly through study-equivalence), FSM triggers reclassification to tangled_rope. If genuine natural law, Mountain stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Natural-law vs. constructed framework ambiguity for the kodashim kernel itself').

omega_variable(
    study_as_performance_bounds,
    'Does ''study fulfills the commandment'' have structural limits — e.g., must study be accompanied by intention, specific methodology, or communal recognition — or is any intellectual engagement sufficient?',
    'Halakhic source analysis: examine whether classic sources (Talmud Menachot 110a, Rambam Hilchot Ma''aseh Hakorbanot, R. Chaim Volozhin Nefesh HaChaim) impose qualitative conditions on study-equivalence.',
    'If study-equivalence has qualitative bounds, the constraint has internal structure (coordination function with standards); if unbounded, it risks being performative covering for absence of real performance (higher theater_ratio).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_as_performance_bounds, empirical, 'Boundary conditions on study-equivalence claim').

omega_variable(
    messianic_deferral_relation,
    'Does the study_as_performance reading logically foreclose messianic_deferral, or do they coexist as complementary framings of the same suspension?',
    'Structural analysis of halakhic frameworks: if study_as_performance claims the commandment is *currently fully operative* through study, it forecloses messianic_deferral''s claim that the commandment is *suspended but will resume*. If study_as_performance is read as ''study maintains readiness for performance,'' it coexists_with messianic_deferral.',
    'Foreclosure would mean only one reading can be authoritative in a single halakhic framework; coexistence means both operate simultaneously across different authorities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(messianic_deferral_relation, conceptual, 'Logical relationship between study_as_performance and messianic_deferral readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__study_as_performance, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kodashim_study_perf_tr_t0, kodashim_commandment_status__study_as_performance, theater_ratio, 0, 0.05).
narrative_ontology:measurement(kodashim_study_perf_tr_t70, kodashim_commandment_status__study_as_performance, theater_ratio, 70, 0.05).
narrative_ontology:measurement(kodashim_study_perf_tr_t500, kodashim_commandment_status__study_as_performance, theater_ratio, 500, 0.07).
narrative_ontology:measurement(kodashim_study_perf_tr_t1000, kodashim_commandment_status__study_as_performance, theater_ratio, 1000, 0.08).
narrative_ontology:measurement(kodashim_study_perf_tr_t1500, kodashim_commandment_status__study_as_performance, theater_ratio, 1500, 0.09).
narrative_ontology:measurement(kodashim_study_perf_tr_t2000, kodashim_commandment_status__study_as_performance, theater_ratio, 2000, 0.1).

% Extraction over time
narrative_ontology:measurement(kodashim_study_perf_be_t0, kodashim_commandment_status__study_as_performance, base_extractiveness, 0, 0.01).
narrative_ontology:measurement(kodashim_study_perf_be_t70, kodashim_commandment_status__study_as_performance, base_extractiveness, 70, 0.01).
narrative_ontology:measurement(kodashim_study_perf_be_t500, kodashim_commandment_status__study_as_performance, base_extractiveness, 500, 0.01).
narrative_ontology:measurement(kodashim_study_perf_be_t1000, kodashim_commandment_status__study_as_performance, base_extractiveness, 1000, 0.015).
narrative_ontology:measurement(kodashim_study_perf_be_t1500, kodashim_commandment_status__study_as_performance, base_extractiveness, 1500, 0.015).
narrative_ontology:measurement(kodashim_study_perf_be_t2000, kodashim_commandment_status__study_as_performance, base_extractiveness, 2000, 0.02).

% Suppression requirement over time
narrative_ontology:measurement(kodashim_study_perf_su_t0, kodashim_commandment_status__study_as_performance, suppression_requirement, 0, 0.02).
narrative_ontology:measurement(kodashim_study_perf_su_t70, kodashim_commandment_status__study_as_performance, suppression_requirement, 70, 0.03).
narrative_ontology:measurement(kodashim_study_perf_su_t500, kodashim_commandment_status__study_as_performance, suppression_requirement, 500, 0.04).
narrative_ontology:measurement(kodashim_study_perf_su_t1000, kodashim_commandment_status__study_as_performance, suppression_requirement, 1000, 0.05).
narrative_ontology:measurement(kodashim_study_perf_su_t1500, kodashim_commandment_status__study_as_performance, suppression_requirement, 1500, 0.05).
narrative_ontology:measurement(kodashim_study_perf_su_t2000, kodashim_commandment_status__study_as_performance, suppression_requirement, 2000, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__study_as_performance, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_commandment_status__study_as_performance, 0.08).
narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, kodashim_commandment_status__messianic_deferral).
narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, kodashim_commandment_status__performance_only).
narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, halakhic_authority_structure).
narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, oral_torah_transmission).

% DUAL FORMULATION NOTE:
% Part of the kodashim_commandment_status constraint family (3 readings). This reading (study_as_performance) claims the commandment is currently fully operative through study. performance_only claims it is a husk. messianic_deferral claims it is suspended with future restoration. All three share the same kernel (the kodashim commandments in Torah) but instantiate different constraints with different ε, beneficiary structures, and types. Linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
