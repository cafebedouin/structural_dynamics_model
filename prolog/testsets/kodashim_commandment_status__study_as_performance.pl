% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kodashim_commandment_status__study_as_performance
 *   human_readable: Kodashim Study as Commandment Performance (Study-as-Performance Reading)
 *   domain: religious/halakhic/commitment_system
 *
 * SUMMARY:
 *   In Jewish halakhic tradition, the laws of sacrifice (Kodashim) occupy a
 *   central place in the Mishnah and Talmud. After the destruction of the
 *   Second Temple in 70 CE, actual animal sacrifice became impossible. The
 *   kernel question is: what is the status of the commandment to bring
 *   offerings when the material condition (functioning altar) no longer
 *   exists? This constraint instantiates ONE reading of that kernel: the
 *   study-as-performance reading. According to this reading, engaging in
 *   intellectual study of the sacrifice laws constitutes fulfillment of the
 *   commandment itself — not as a temporary substitute, but as a full and
 *   permanent solution. The interpretive lineage that holds this reading
 *   treats the textual knowledge and cognitive engagement as the real
 *   'performance' of the obligation. This reading has been institutionalized
 *   in Jewish practice through the daily recitation of sacrifice passages in
 *   liturgy (the Korban Tamid in the siddur). The sibling readings —
 *   messianic_deferral and performance_only — represent alternative
 *   interpretations of the same kernel, instantiated in separate constraint
 *   stories. They are NOT part of this constraint; this constraint models
 *   only the study-as-performance framework with its own ε, its own authority
 *   structure, and its own stakeholder dynamics.
 *
 * KEY AGENTS:
 *   - Talmudic scholars and their successors: institutional authority that interprets and transmits the reading; power=institutional, exit=analytical (they work within the tradition)
 *   - Jewish observant community: the constituency that accepts the study-as-performance framing as binding law; power=organized, time_horizon=generational
 *   - Halakhic authority (posqim): the contemporary or historical decisors who validate the reading through their rulings; power=institutional, time_horizon=civilizational
 *   - Alternative reading communities (messianic_deferral, performance_only): excluded from THIS constraint's scope but structurally present in the kernel contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__study_as_performance, 0.0).
domain_priors:suppression_score(kodashim_commandment_status__study_as_performance, 0.0).
domain_priors:theater_ratio(kodashim_commandment_status__study_as_performance, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, extractiveness, 0.0).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__study_as_performance, mountain).
narrative_ontology:human_readable(kodashim_commandment_status__study_as_performance, "Kodashim Study as Commandment Performance (Study-as-Performance Reading)").
narrative_ontology:topic_domain(kodashim_commandment_status__study_as_performance, "religious/halakhic/commitment_system").

domain_priors:emerges_naturally(kodashim_commandment_status__study_as_performance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__study_as_performance, 'e8c8ef58-628b-4ec6-bf1e-348a17f1f6b5').
narrative_ontology:cs_kernel_codification('e8c8ef58-628b-4ec6-bf1e-348a17f1f6b5', fixed_text).
narrative_ontology:cs_authority_grounding('e8c8ef58-628b-4ec6-bf1e-348a17f1f6b5', lineage).
narrative_ontology:cs_interpretation_layer_present('e8c8ef58-628b-4ec6-bf1e-348a17f1f6b5').
narrative_ontology:cs_reading_relation('e8c8ef58-628b-4ec6-bf1e-348a17f1f6b5', kodashim_commandment_status__messianic_deferral, coexists_with).
narrative_ontology:cs_reading_relation('e8c8ef58-628b-4ec6-bf1e-348a17f1f6b5', kodashim_commandment_status__performance_only, coexists_with).
narrative_ontology:cs_axiom('e8c8ef58-628b-4ec6-bf1e-348a17f1f6b5', foundational, intellectual_study_constitutes_performance).
narrative_ontology:cs_axiom_status(intellectual_study_constitutes_performance, holdable).
narrative_ontology:cs_axiom_grounding('e8c8ef58-628b-4ec6-bf1e-348a17f1f6b5', intellectual_study_constitutes_performance, deontological).
narrative_ontology:cs_axiom('e8c8ef58-628b-4ec6-bf1e-348a17f1f6b5', foundational, commandment_obligation_survives_temple_absence).
narrative_ontology:cs_axiom_status(commandment_obligation_survives_temple_absence, holdable).
narrative_ontology:cs_axiom_grounding('e8c8ef58-628b-4ec6-bf1e-348a17f1f6b5', commandment_obligation_survives_temple_absence, deontological).
narrative_ontology:cs_reference_frame('e8c8ef58-628b-4ec6-bf1e-348a17f1f6b5', study_maintains_commandment_force).
narrative_ontology:cs_drift_state('e8c8ef58-628b-4ec6-bf1e-348a17f1f6b5', contemporary_diaspora_state, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e8c8ef58-628b-4ec6-bf1e-348a17f1f6b5', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__study_as_performance, kodashim_commandment_status).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, jewish_observant_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage in systematic study of the Mishnah and Gemara on the laws of sacrificial offerings. They interpret and transmit the tradition that studying these laws constitutes fulfillment of the commandment itself, independent of Temple performance. Their authority grounds itself in the transmitted interpretive lineage and their role as authorized interpreters of halakha.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, talmudic_scholars, agenda_setter,
    institutional, civilizational, analytical, global).

% Participate in the obligation to engage with sacrifice laws through study. They understand this engagement as fulfilling a core commandment even without a functioning Temple. The reading provides a framework in which their intellectual and ritual life remains whole and obligated across the centuries of diaspora and Temple absence.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, jewish_observant_community, beneficiary,
    organized, generational, constrained, global).

% Hold an alternative reading (messianic_deferral) that frames the commandment as suspended, awaiting messianic restoration. They would argue that study is preparation and readiness, not performance itself. This reading is excluded from the current constraint's framing but remains live in parallel within Jewish tradition.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, messianic_deferral_readers, excluded,
    organized, civilizational, analytical, global).

% Hold the performance_only reading that interprets the commandment as contingent on Temple existence; without an altar, the commandment is suspended (treated as a husk). They would argue that study cannot substitute for actual sacrifice. This reading remains an alternative interpretation within the broader kernel contest.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, performance_only_readers, excluded,
    organized, civilizational, analytical, global).

% The interpretive authority that adjudicates which reading of the kernel is authoritative within a particular community or era. In the study-as-performance framing, this authority legitimates the equation of study with commandment fulfillment through lineage and textual reasoning. They hold the authority to validate or contest the reading.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, halakhic_authority, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__study_as_performance, halakhic_authority, observer).

% Examines the constraint from outside the tradition: how does this reading stabilize itself, what authority grounds its legitimacy, and how does the reading interface with the sibling readings in the kernel contest?
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the obligatory force of a commandment across a 2000-year period of Temple absence by redefining performance: study and intellectual engagement with sacrifice laws substitute for altar practice. This solves the coordination problem of how a community preserves its covenant obligations when the material conditions for literal performance are removed.
% TRANSFER_FUNCTION: No material transfer; the constraint is purely interpretive. What flows is interpretive authority: the talmudic scholars and their authorized successors possess the authority to declare that study = performance, and the community accepts this reframing as legitimate binding law.
% ABSENT_VOICES: Alternative readings within the kernel contest (messianic_deferral and performance_only) are structurally present in Jewish tradition but excluded from THIS constraint's scope. A reading that denies the validity of substitute performance — that argues the commandment simply lapses without Temple — would not be heard in communities where study-as-performance is already established as authoritative.
% DISAPPEARANCE_RATIONALE: If this reading disappeared — if the interpretive authority collapsed and the study-as-performance framework were rejected — the community would face the messianic_deferral or performance_only readings as live alternatives. The theological-legal landscape would reorganize around competing answers to the kernel question. However, the underlying halakhic content (the texts and laws themselves) would persist regardless.
% FOUNDING_PROBLEM: After the destruction of the Second Temple (70 CE), the altar could no longer function as the locus of sacrifice. The covenant obligates Israel to bring offerings, but the material condition for doing so was removed. How can the commandment retain its binding force when performance is structurally impossible?
% FOUNDING_PROBLEM_CORROBORATION: The Talmud itself (Mishnah Berakhot 2:1 and extended Gemara discussion) and the major medieval authorities (Rambam, Ramban, Rav Kook in later eras) attest that study of the laws substitutes for performance. Post-Talmudic Jewish practice, across Ashkenazi, Sephardi, and other communities, institutionalized the recitation and study of sacrifice laws in daily liturgy as evidence that this reading held authoritative status. The founding problem remains live because the Temple has not been rebuilt and the question of commandment obligation during absence is structurally unresolved — the reading provides an answer to a persisting structural gap.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__study_as_performance, contested).
narrative_ontology:founding_problem_status(kodashim_commandment_status__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__study_as_performance, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(kodashim_commandment_status__study_as_performance, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__study_as_performance_tests).

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
 *   Extractiveness is 0.0 throughout the interval because the reading produces zero net extraction: there is no victimized party, no suppression of alternatives within this constraint's scope, no transfer of resources or status to beneficiaries. The constraint is a pure reinterpretation of obligation — it redefines what counts as performance, not what counts as cost. Suppression is 0.0: the reading's persistence does not depend on coercion or on preventing alternatives from being understood. The alternatives (messianic_deferral, performance_only) exist as live intellectual positions within Jewish tradition; this reading does not suppress them through force, only through interpretive authority and consensus. Theater_ratio is 0.0: the study is not performative in the sense of being empty ritual maintained for show. The Talmudic engagement with these texts is substantively intellectual work, not theater masking a different function. Accessibility_collapse is high (0.92): once someone is embedded in the Jewish halakhic tradition and accepts its authority structure, the claim that study = performance becomes very nearly inevitable — the textual and logical case for the equation is strong, and the alternatives require deliberate rejection. Resistance is low (0.15): few voices within the observant community actively resist this reading at the institutional level; dissent (where it exists) takes the form of the alternative readings, not rejection of the framework itself. The measurement series is flat across the interval because the constraint's structural properties do not change: the reading established itself early in the Talmudic era (~5th century CE, represented by time_point=500 in a 1954-endpoint interval) and maintained stable properties through the medieval, early modern, and modern periods.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal perspectival gap in this constraint because the beneficiaries and payers are largely aligned (or absent). The talmudic scholars and the observant community both benefit from the reading — it preserves their obligation-structure and allows them to remain halakhically whole. No identifiable seat bears the cost of the reading as opposed to the costs of being Jewish. The halakhic authority does not extract from the community in order to maintain this reading; it provides a service (interpretation) that the community values. The excluded readings (messianic_deferral, performance_only) represent alternative framings held by other parties within the broader tradition, not victims of this reading. The absence of perspectival gap is itself a feature: a mountain constraint is characterized by low or absent divergence in how different seats experience the constraint, because the constraint reflects structural necessity rather than asymmetric power.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are not authored for this constraint because there are no payers or victims to create directionality gradient. All stakeholders are beneficiaries or neutral observers. The scholars benefit from intellectual engagement; the community benefits from preserved obligation; the authority benefits from its role as interpreter. No stakeholder has d > 0.5 (target status). This absence of directionality gradient is a structural property of the reading: it does not work by extraction from one group and transfer to another. Rather, it reframes an obligation in a way that benefits all parties who accept the framework. An external observer (analytical_observer seat) experiences directionality=0.5 (symmetric): they neither collect from nor bear costs from the reading; they observe it as a structural fact of the tradition.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not exhibit mandatrophy because its founding problem remains live and structurally unresolved. The problem is the commandment's status during Temple absence; the reading provides an answer. If the Temple were rebuilt, the founding problem would shift (the material condition would change), but it would not disappear. The reading's function is not atrophied — it is actively used in Jewish study and practice today. The theater_ratio=0.0 indicates no drift toward performance-without-function. The reading is not maintained as empty ritual; it carries real halakhic and theological weight. Mandatrophy would apply if the study of sacrifice laws had become purely ceremonial (e.g., recited without understanding, maintained only for tradition's sake) while the obligation itself was understood as functionally lapsed. That is not the case in this reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_structural,
    'Does the study-as-performance reading logically foreclose the messianic_deferral and performance_only readings, or do all three coexist as live interpretive options within Jewish tradition?',
    'Historical textual analysis: does the Talmud itself entertain all three readings, or does it settle definitively on one? Do later authorities (Rishonim, Acharonim) acknowledge competing interpretations as live halakhic questions, or does a consensus emerge around study-as-performance?',
    'If all three readings coexist, the kernel_codification is distributed and the reading_relations are coexists_with. If study-as-performance achieves consensus in authoritative sources, the relation may shift toward influences. If the reading forecloses the others explicitly, reading_relations change to forecloses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_structural, empirical, 'Whether sibling readings coexist or are hierarchically resolved in the tradition.').

omega_variable(
    natural_law_vs_constructed_reading,
    'Is the equation of study with commandment performance a natural-law implication of the commandment''s text itself, or a constructed reinterpretation built to solve a historical problem (Temple absence)?',
    'Textual hermeneutics: does the original source text (Leviticus, Mishnah) contain linguistic or logical support for the study-as-performance equation, or is this a post-hoc reading developed after 70 CE?',
    'If study-as-performance is a natural logical consequence of the text, emerges_naturally=true is accurate (mountain classification holds). If it is a constructed solution to a historical crisis, the constraint should be reclassified toward rope or scaffold. The FSM algorithm would not fire (no beneficiaries declared), but this omega flags the conceptual ambiguity at the heart of calling it ''natural law.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_reading, conceptual, 'Whether the reading is a natural implication or a constructed reinterpretation.').

omega_variable(
    authority_grounding_lineage_vs_textual,
    'Does the authority of the study-as-performance reading ground itself in an unbroken lineage of transmission (oral tradition, rabbinic succession), or in textual reasoning that any competent interpreter can perform?',
    'Epistemological analysis: how do contemporary halakhic authorities justify this reading — by appeal to a chain of authorities (Talmud → Rishonim → Acharonim → modern decisors) or by reconstructing the argument from first principles?',
    'If lineage is primary, authority_grounding=lineage and the reading derives legitimacy from continuity. If textual reasoning is primary, authority_grounding may shift toward expertise or distributed. This affects how the reading responds to external challenge and whether it is vulnerable to interpretive revolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_lineage_vs_textual, empirical, 'Whether the reading''s authority is lineage-based or argument-based.').

omega_variable(
    interpretation_layer_collapse_risk,
    'If the Temple were rebuilt and actual sacrifice became materially possible again, would the study-as-performance reading collapse, defer to performance-only, or persist as a parallel obligation?',
    'Counterfactual textual analysis and contemporary halakhic opinion: what do authorities say would happen to the study obligation if the Temple returned? Would study remain obligatory alongside sacrifice, or would it become optional/subsidiary?',
    'If study would collapse to optional, the constraint is brittle and vulnerable to a messianic scenario. If study would persist as obligatory even with performance possible, the reading has an interpretive buffer against material change and is more structurally stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpretation_layer_collapse_risk, preference, 'Stability of the reading under messianic restoration scenarios.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__study_as_performance, 0, 1954).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__study_as_performance, theater_ratio, 0, 0.0).
narrative_ontology:measurement_basis(koda_tr_t0, observed).
narrative_ontology:measurement(koda_tr_t500, kodashim_commandment_status__study_as_performance, theater_ratio, 500, 0.0).
narrative_ontology:measurement_basis(koda_tr_t500, observed).
narrative_ontology:measurement(koda_tr_t1000, kodashim_commandment_status__study_as_performance, theater_ratio, 1000, 0.0).
narrative_ontology:measurement_basis(koda_tr_t1000, observed).
narrative_ontology:measurement(koda_tr_t1500, kodashim_commandment_status__study_as_performance, theater_ratio, 1500, 0.0).
narrative_ontology:measurement_basis(koda_tr_t1500, observed).
narrative_ontology:measurement(koda_tr_t1954, kodashim_commandment_status__study_as_performance, theater_ratio, 1954, 0.0).
narrative_ontology:measurement_basis(koda_tr_t1954, observed).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__study_as_performance, base_extractiveness, 0, 0.0).
narrative_ontology:measurement_basis(koda_be_t0, observed).
narrative_ontology:measurement(koda_be_t500, kodashim_commandment_status__study_as_performance, base_extractiveness, 500, 0.0).
narrative_ontology:measurement_basis(koda_be_t500, observed).
narrative_ontology:measurement(koda_be_t1000, kodashim_commandment_status__study_as_performance, base_extractiveness, 1000, 0.0).
narrative_ontology:measurement_basis(koda_be_t1000, observed).
narrative_ontology:measurement(koda_be_t1500, kodashim_commandment_status__study_as_performance, base_extractiveness, 1500, 0.0).
narrative_ontology:measurement_basis(koda_be_t1500, observed).
narrative_ontology:measurement(koda_be_t1954, kodashim_commandment_status__study_as_performance, base_extractiveness, 1954, 0.0).
narrative_ontology:measurement_basis(koda_be_t1954, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_commandment_status__study_as_performance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__study_as_performance, identity_coordination).
narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, kodashim_commandment_status__messianic_deferral).
narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, kodashim_commandment_status__performance_only).

% DUAL FORMULATION NOTE:
% The kodashim_commandment_status kernel decomposes into three constraint stories corresponding to three live readings of the same question: what is the status of the sacrifice commandment after Temple destruction? This story (study_as_performance) models the reading that study of the laws constitutes performance itself. The messianic_deferral reading (separate story) interprets the commandment as suspended pending messianic restoration, with study as preparation. The performance_only reading (separate story) treats the commandment as contingent on Temple existence and therefore lapsed. All three readings coexist as live positions in Jewish tradition; they are linked via network.affects_constraints to indicate their structural interdependence within the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
