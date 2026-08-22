% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__study_as_exercise_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__study_as_exercise_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: sacrifice_obligation_kernel__study_as_exercise_reading
 *   human_readable: Study of Sacrifice Law as Genuine Mitzvah Exercise (Halakhic Reading)
 *   domain: religious/halakhic/commitment_system
 *
 * SUMMARY:
 *   In Jewish halakhic tradition, the obligation to bring sacrifices at the
 *   Temple is a binding commandment in Torah. After the Temple's destruction
 *   in 70 CE, physical performance became structurally impossible. One
 *   authoritative reading—the study-as-exercise reading—holds that
 *   intellectual engagement with the laws of sacrifice constitutes genuine
 *   fulfillment of the mitzvah under post-Temple conditions. This is not a
 *   suspension of obligation (as the messianic reading claims) nor a symbolic
 *   archive (as the symbolic reading claims) nor mere preparation (as the
 *   performance-only reading claims). Rather, the mitzvah's form has been
 *   transformed through rabbinic interpretation: study IS the exercise. This
 *   constraint story instantiates ONLY this reading, with zero extractiveness
 *   under its own terms: the obligation remains binding, the community
 *   remains coordinated around it, and no victim class emerges because the
 *   transformation is understood as authorized (not imposed). However, the
 *   reading depends on rabbinic authority having the power to determine what
 *   'fulfillment' means—a structural fact that carries FSM implications if
 *   examined from outside the reading's authority frame.
 *
 * KEY AGENTS:
 *   - rabbinic_authority: Sets the terms of what constitutes halakhic obligation and determines that study fulfills the sacrifice mitzvah post-Temple (institutional, analytical exit)
 *   - jewish_community_diaspora: Maintains binding obligation through study; benefits from continued connection to Torah and commandment (organized, identity-locked exit)
 *   - studying_individual: Directly exercises the mitzvah through intellectual engagement; no physical performance required or possible (moderate power, identity-locked exit)
 *   - alternative_reading_adherents: Excluded from this reading's authority claim; would contest whether study alone suffices, whether obligation is suspended, or whether study carries halakhic force (organized, constrained exit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__study_as_exercise_reading, 0.0).
domain_priors:suppression_score(sacrifice_obligation_kernel__study_as_exercise_reading, 0.0).
domain_priors:theater_ratio(sacrifice_obligation_kernel__study_as_exercise_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__study_as_exercise_reading, mountain).
narrative_ontology:human_readable(sacrifice_obligation_kernel__study_as_exercise_reading, "Study of Sacrifice Law as Genuine Mitzvah Exercise (Halakhic Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__study_as_exercise_reading, "religious/halakhic/commitment_system").

domain_priors:emerges_naturally(sacrifice_obligation_kernel__study_as_exercise_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__study_as_exercise_reading, 'fb4c1b9b-a7af-4e2b-8c53-9f2651b5524b').
narrative_ontology:cs_kernel_codification('fb4c1b9b-a7af-4e2b-8c53-9f2651b5524b', fixed_text).
narrative_ontology:cs_authority_grounding('fb4c1b9b-a7af-4e2b-8c53-9f2651b5524b', lineage).
narrative_ontology:cs_interpretation_layer_present('fb4c1b9b-a7af-4e2b-8c53-9f2651b5524b').
narrative_ontology:cs_reading_relation('fb4c1b9b-a7af-4e2b-8c53-9f2651b5524b', sacrifice_obligation_kernel__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('fb4c1b9b-a7af-4e2b-8c53-9f2651b5524b', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('fb4c1b9b-a7af-4e2b-8c53-9f2651b5524b', sacrifice_obligation_kernel__symbolic_archive_reading, coexists_with).
narrative_ontology:cs_axiom('fb4c1b9b-a7af-4e2b-8c53-9f2651b5524b', foundational, intellectual_engagement_fulfills_mitzvah).
narrative_ontology:cs_axiom_status(intellectual_engagement_fulfills_mitzvah, holdable).
narrative_ontology:cs_axiom_grounding('fb4c1b9b-a7af-4e2b-8c53-9f2651b5524b', intellectual_engagement_fulfills_mitzvah, deontological).
narrative_ontology:cs_axiom('fb4c1b9b-a7af-4e2b-8c53-9f2651b5524b', foundational, obligation_transformed_not_suspended).
narrative_ontology:cs_axiom_status(obligation_transformed_not_suspended, holdable).
narrative_ontology:cs_axiom_grounding('fb4c1b9b-a7af-4e2b-8c53-9f2651b5524b', obligation_transformed_not_suspended, conventional).
narrative_ontology:cs_reference_frame('fb4c1b9b-a7af-4e2b-8c53-9f2651b5524b', halakhic_obligation_maintained_through_study).
narrative_ontology:cs_drift_state('fb4c1b9b-a7af-4e2b-8c53-9f2651b5524b', contemporary_diaspora, gap(stable, minor, true)).
narrative_ontology:cs_created_at('fb4c1b9b-a7af-4e2b-8c53-9f2651b5524b', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, jewish_community_diaspora).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, studying_individual).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The interpretive body (Talmudic academies, later rabbinic courts, poskim) adjudicates what constitutes valid exercise of mitzvot under current historical conditions. In this reading, rabbinic authority determines that study of sacrifice law fulfills the obligation when physical performance is impossible (post-Temple, in diaspora). Authority derives from lineage authority grounded in textual interpretation and transmitted precedent.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_authority, agenda_setter,
    institutional, civilizational, analytical, global).

% Dispersed Jewish communities separated from the Temple site and unable to perform sacrifices benefit from this reading: the mitzvah obligation remains binding and honorable, maintained through intellectual engagement rather than suspended or abandoned. Study preserves the community's relationship to Torah and fulfills commandment without requiring impossible physical conditions.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, jewish_community_diaspora, beneficiary,
    organized, civilizational, identity_locked, global).

% The individual who studies sacrifice law engages directly in mitzvah fulfillment. Study is not preparatory or secondary; it is the legitimate form of the obligation. The individual's intellectual capacity and commitment to Torah study directly discharge the halakhic obligation; no performance is required or possible.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, studying_individual, beneficiary,
    moderate, biographical, identity_locked, local).

% Communities and scholars who hold the performance_only_reading (sacrifice requires physical performance), the messianic_suspension_reading (obligation is divinely suspended, not transformed), or the symbolic_archive_reading (study preserves history but makes no halakhic claim) are structurally excluded from this reading's authority claim. They contest what constitutes genuine mitzvah exercise and would argue study alone is insufficient or misconceived.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, alternative_reading_adherents, excluded,
    organized, civilizational, constrained, global).

% Examines the reading from outside its authority structure; notes how the reading resolves the historical rupture (Temple destruction) by transforming obligation rather than suspending or abandoning it, and how rabbinic interpretive authority becomes central to what fulfillment means.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains binding obligation to the mitzvah (commandment concerning sacrifice) across historical conditions where physical performance became structurally impossible. Coordinates community and individual identity around continued Torah study and halakhic engagement despite diaspora separation from the Temple site. Solves the problem: how does a community uphold a commandment that cannot be performed?
% TRANSFER_FUNCTION: Transfers interpretive authority over what constitutes halakhic obligation fulfillment to the rabbinic authority structure. The mitzvah's meaning shifts from physical performance to intellectual engagement; who determines this shift is now central. No goods or labor flow; authority over definition and legitimacy are what move.
% ABSENT_VOICES: Communities that hold alternative readings are structurally excluded from adjudication within this reading's framework. Performance_only adherents would argue study does not fulfill the obligation. Messianic_suspension adherents would argue the obligation is divinely suspended, not transformed. Symbolic_archive adherents would deny halakhic force to the study. These readings compete; this reading does not include them as voices—they are rival authority claims.
% DISAPPEARANCE_RATIONALE: If this reading disappeared and the performance_only or messianic_suspension readings became canonical, the halakhic status of study would shift dramatically: study would be insufficient or merely preparatory rather than obligatory exercise. The community's self-understanding, worship practice, and identity would reorganize. The contestation itself shows the reading is not natural law but an interpretive arrangement whose loss matters.
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE ended the possibility of physical sacrifice while leaving the mitzvah obligation intact in Torah. The Jewish community faced a rupture: the commandment could not be performed. How does a community maintain binding obligation to an impossible act?
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic authority (Talmudic sources, Maimonides, later codes) attests that the founding problem is live and this reading solves it. Non-rabbinic alternative readings (performance-only, messianic-suspension, symbolic-archive adherents) attest the problem persists but contest this solution as illegitimate. Historical scholarship documents that the Temple's destruction and the community's response generated sustained interpretive work. The founding problem is corroborated by the existence and persistence of rival readings—if the problem were truly solved, alternatives would not maintain institutional form.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__study_as_exercise_reading, contested).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__study_as_exercise_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__study_as_exercise_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__study_as_exercise_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__study_as_exercise_reading, 0.0, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__study_as_exercise_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, ExtMetricName, E),
    domain_priors:suppression_score(sacrifice_obligation_kernel__study_as_exercise_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(sacrifice_obligation_kernel__study_as_exercise_reading),
    narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(sacrifice_obligation_kernel__study_as_exercise_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is authored as 0.0 because under this reading's own terms, no one is harmed or extracted from: the obligation is maintained in binding form, the community is coordinated around continued Torah engagement, and the transformation is understood as authorized interpretation of Torah rather than imposed rule. The suppression is 0.0 because the reading presents itself as a coherent halakhic solution, not a coercively maintained position. Theater ratio is 0.0: the study of sacrifice law is genuinely functional for preserving the tradition and maintaining communal and individual connection to the mitzvah; it is not performative or degraded. Accessibility collapse is high (0.95) because once the reading is understood and accepted within the tradition, alternatives (performance, suspension, symbolic-only) become nearly incompatible with the reading's own logic—the reading is self-sealing from inside. Resistance is moderate (0.35) because alternative readings persist and command institutional support; the reading is contested historically and remains contested.
 *
 * PERSPECTIVAL GAP:
 *   The rabbinic authority reads this constraint as Mountain (natural interpretation of what Torah requires); performance-only and messianic-suspension readers see it as Tangled Rope or Snare (authority is imposing a reading that subordinates or eliminates alternatives). The reading's own claim is mountain; the engine's per-seat computation may produce divergence. This divergence is not an error—it is the structural fact the classification system is designed to detect: a constraint presented as natural by the authority that benefits from it, but contested by those whose position it changes.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority sits at the beneficiary end of directionality (d near 0.0): it is not extracted from by this arrangement; rather, it gains interpretive monopoly over determining what the mitzvah means. The community and individuals studying sit at the symmetric center (d near 0.5): they benefit from obligation remaining binding and meaningful, and they bear the 'cost' of maintaining intellectual engagement—a cost they accept willingly within the identity-locked frame. Alternative reading adherents are excluded, not targeted by extraction, so they do not appear in directionality calculus. From the reading's internal framing, directionality is zero-extraction across all parties because the transformation is understood as theological resolution, not constructed extraction. However, the FSM signature should flag this: a mountain claim with declared beneficiaries (rabbinic authority) requires omega variables documenting natural-law vs. constructed ambiguity. The reading itself must contend that its authority claim is not an extraction mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Temple destruction; obligation impossible to perform) is live: diaspora communities continue to ask how the mitzvah is maintained. This reading resolves the problem by transforming obligation from performance to study. The reading's mandate has not been superseded—it remains the authoritative answer in major rabbinic streams. However, mandatrophy is contested: alternative readings claim the mandate has outlived its function (either obligation is suspended, or study does not count, or study preserves only symbolic continuity). From inside this reading's framework, mandatrophy has not occurred; from outside it, the reading may appear to be theater—historical performance of obligation when the real obligation is suspended or no longer meaningful. The omega variables capture this irresolvable dispute.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_interpretive_authority,
    'Is the principle that intellectual engagement fulfills the mitzvah a natural consequence of Torah logic, or is it a constructed transformation authorized by rabbinic interpretive monopoly?',
    'Examine whether the reading derives demonstratively from Torah text and prior precedent, or whether it requires discretionary authority to transform the obligation''s form. Ask whether alternative readings could equally claim textual support.',
    'If the reading is a natural interpretation, the constraint approaches genuine mountain status (low extractiveness, no constructed victim set). If it requires discretionary authority to override performance-priority, the constraint may shade toward Tangled Rope: rabbinic authority benefits from interpretive monopoly; community members are coordinated but depend on authority''s ruling for obligation''s legitimacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_interpretive_authority, conceptual, 'Whether study-as-fulfillment is textually necessitated or interpretively constituted.').

omega_variable(
    authority_constraint_vs_theological_resolution,
    'Does this reading constrain halakhic authority (the reading is discovered from text and tradition, authority is bound by it) or does it empower authority (the reading is authorized, authority decides what counts as fulfillment)?',
    'Examine whether the rabbinic authorities that hold this reading present themselves as constrained by text and precedent, or as exercising discretion to adapt obligation to circumstance. Check whether alternative readings claim equal textual warrant or claim authority is overreaching.',
    'If rabbinic authority presents itself as constrained, the reading is a mountain from its own framing (obligation''s meaning is not chosen but discovered). If authority claims discretion, the reading is a Tangled Rope: community needs the coordination (obligation maintained); rabbinic authority extracts legitimacy and interpretive monopoly. The false-summit signature would fire, classifying as Tangled Rope despite the mountain claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_constraint_vs_theological_resolution, conceptual, 'Whether this reading constrains or empowers rabbinic authority.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'The measured suppression is 0.0, asserting no coercive force sustains the reading. Is this accurate, or does the reading''s acceptance depend on internalized identity-lock (a person cannot exit Torah study without abandoning Jewish identity) that functions as suppression even without institutional coercion?',
    'Post-exit trajectory analysis: if a person exits the reading and Torah study but retains Jewish identity and community standing, suppression is structural-zero. If identity or community standing degrade following exit, suppression is internalized and the constraint carries latent coercive force.',
    'If suppression is internalized through identity-lock, the reading''s persistence depends on internalized mechanisms the metric did not capture. The false-summit signature should flag this as a hidden extraction mechanism—a mountain that appears coercion-free but persists through identity fusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether the reading''s zero suppression is accurate or masks internalized identity-dependent coercion.').

omega_variable(
    rival_readings_foreclosure_vs_coexistence,
    'Does this reading logically foreclose the performance_only and messianic_suspension readings, or do they coexist as live but contested positions?',
    'Examine the core premises: does study-as-fulfillment logically entail that performance is not required? Does it logically entail that the obligation is not suspended but transformed? Or can both the study-as-fulfillment reading and the performance-only reading be true within different frameworks (e.g., study fulfills the obligation in diaspora, but physical performance would be required if the Temple were rebuilt)?',
    'If readings foreclose each other, the constraint story should reflect that one framework must dominate. If readings coexist, the cs_structure.reading_relations should use coexists_with rather than forecloses. This affects whether the contested kernel is a site of genuine authority conflict (coexistence) or a logical necessity gate (foreclosure).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rival_readings_foreclosure_vs_coexistence, conceptual, 'Whether alternative readings are logically incompatible or coexistent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__study_as_exercise_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 500, 0.0).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 1000, 0.0).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 1500, 0.0).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 2000, 0.0).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 500, 0.0).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 1000, 0.0).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 1500, 0.0).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 2000, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 0, 0.0).
narrative_ontology:measurement(sacr_su_t500, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 500, 0.0).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 1000, 0.0).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 1500, 0.0).
narrative_ontology:measurement(sacr_su_t2000, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 2000, 0.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__study_as_exercise_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_kernel__study_as_exercise_reading, 0.08).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% The sacrifice_obligation_kernel decomposes into four constraint stories, one per reading: study_as_exercise_reading (this one, zero extractiveness), performance_only_reading (study insufficient), messianic_suspension_reading (obligation divinely suspended), and symbolic_archive_reading (study preserves identity without halakhic claim). Each story has its own ε, beneficiary/victim structure, and authority grounding. They are not four perspectives on one constraint; they are four distinct constraints unified by their common kernel. All four are linked via network.affects_constraints so the constraint family is traceable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
