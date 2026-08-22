% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__study_as_archiving
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__study_as_archiving, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: temple_sacrifice_obligation__study_as_archiving
 *   human_readable: Temple Sacrifice Obligation: Study as Archiving (Non-Fulfillment Reading)
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   After the destruction of the Second Temple in 70 CE, the Jewish people
 *   faced an unprecedented halakhic crisis: the Torah commands sacrifice in
 *   the Temple, yet the Temple no longer exists. This constraint story models
 *   one reading of how rabbinic authority responded: the sacrificial
 *   obligation remains binding and unfulfilled, but study of the sacrificial
 *   laws serves as mandatory archiving to preserve knowledge for eventual
 *   restoration. This reading differs structurally from two sibling readings:
 *   the messianic-suspension reading (which declares the obligation
 *   temporarily inactive) and the study-as-occupation reading (which treats
 *   study itself as a form of fulfillment). The study-as-archiving reading
 *   holds open a theological gap — the commandment is neither suspended nor
 *   met, but preserved in textual form against a future when performance
 *   might resume. The constraint extracts perpetual intellectual labor
 *   (study) from practitioners while maintaining a debt to an unmet divine
 *   command, generating moderate extractiveness and substantial suppression
 *   (the suppression of alternative interpretations that would resolve the
 *   gap).
 *
 * KEY AGENTS:
 *   - Rabbinic authority structure: institutional agenda-setter maintaining the binding force of sacrifice law post-Temple
 *   - Jewish scholarly tradition: institutional beneficiary preserving sacrificial knowledge through perpetual textual study
 *   - Individual practitioner: moderate-power victim identity-locked into perpetual non-fulfilling study
 *   - Unfulfilled divine command: analytical payer — the commandment itself as an unmet obligation
 *   - Alternative readings (messianic suspension, study as occupation): excluded competitors to this hermeneutical framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_archiving, 0.68).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_archiving, 0.72).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_archiving, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, extractiveness, 0.68).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_archiving, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_archiving, "Temple Sacrifice Obligation: Study as Archiving (Non-Fulfillment Reading)").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_archiving, "religious/halakhic").

domain_priors:requires_active_enforcement(temple_sacrifice_obligation__study_as_archiving).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_archiving, '591ce31a-86f4-4a6d-bb83-18a3cd8de703').
narrative_ontology:cs_kernel_codification('591ce31a-86f4-4a6d-bb83-18a3cd8de703', fixed_text).
narrative_ontology:cs_authority_grounding('591ce31a-86f4-4a6d-bb83-18a3cd8de703', lineage).
narrative_ontology:cs_interpretation_layer_present('591ce31a-86f4-4a6d-bb83-18a3cd8de703').
narrative_ontology:cs_reading_relation('591ce31a-86f4-4a6d-bb83-18a3cd8de703', temple_sacrifice_obligation__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('591ce31a-86f4-4a6d-bb83-18a3cd8de703', temple_sacrifice_obligation__study_as_occupation, coexists_with).
narrative_ontology:cs_axiom('591ce31a-86f4-4a6d-bb83-18a3cd8de703', foundational, obligation_remains_binding_post_temple).
narrative_ontology:cs_axiom_status(obligation_remains_binding_post_temple, holdable).
narrative_ontology:cs_axiom_grounding('591ce31a-86f4-4a6d-bb83-18a3cd8de703', obligation_remains_binding_post_temple, deontological).
narrative_ontology:cs_axiom('591ce31a-86f4-4a6d-bb83-18a3cd8de703', foundational, non_fulfillment_is_permanent_condition).
narrative_ontology:cs_axiom_status(non_fulfillment_is_permanent_condition, overridden).
narrative_ontology:cs_axiom_grounding('591ce31a-86f4-4a6d-bb83-18a3cd8de703', non_fulfillment_is_permanent_condition, deontological).
narrative_ontology:cs_reference_frame('591ce31a-86f4-4a6d-bb83-18a3cd8de703', binding_unperformable_obligation).
narrative_ontology:cs_drift_state('591ce31a-86f4-4a6d-bb83-18a3cd8de703', contemporary_period_2000_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('591ce31a-86f4-4a6d-bb83-18a3cd8de703', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, rabbinic_authority_structure).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, jewish_scholarly_tradition).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, unfulfilled_divine_command).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, jewish_scholarly_tradition).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, individual_jewish_practitioner).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the binding force of sacrifice law and enforces the obligation to study it, despite the Temple's destruction making physical performance impossible. Derives authority from the Oral Torah tradition and interpretive succession from the Sinaitic revelation. Collects institutional legitimacy and educational jurisdiction by keeping the law binding and its study mandatory.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, rabbinic_authority_structure, agenda_setter,
    institutional, civilizational, arbitrage, universal).

% Preserves detailed knowledge of sacrificial procedure, intent, and theological meaning through textual study and transmission. Constructs a comprehensive archive of pre-destruction Temple practice in halakhic form. Implicitly accepts the non-fulfillment status of the obligation while defending its binding character and mandatory study as the proper response to impossibility.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, jewish_scholarly_tradition, beneficiary,
    institutional, civilizational, mobile, universal).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_obligation__study_as_archiving, jewish_scholarly_tradition, payer).

% Bears the obligation to study sacrifice law in perpetuity without the capacity to fulfill it. The study substitutes for performance, yet the obligation's binding force remains unresolved — study preserves knowledge for eventual restoration but does not discharge the debt to the divine command. Identity as a Jew structured through the covenant includes accepting obligations whose fulfillment awaits messianic restoration.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, individual_jewish_practitioner, payer,
    moderate, biographical, identity_locked, universal).

% The sacrificial commandment in Leviticus stands as divinely ordained, textually fixed, and not revoked. In this reading, the entire post-Temple era is an interval of non-compliance: the command is not suspended (pending messianic time) and not transformed into study (under the occupation reading), but unmet. The constraint preserves knowledge of what is not performed.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, unfulfilled_divine_command, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_obligation__study_as_archiving, unfulfilled_divine_command).

% Other readings of the same kernel (messianic_suspension, study_as_occupation) are competing hermeneutical framings that would dissolve the non-compliance status by either suspending the obligation or transforming study into fulfillment. This reading's enforcement machinery excludes them by maintaining that study is archiving, not compliance.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, alternative_interpretive_readings, excluded,
    institutional, civilizational, trapped, universal).

% Examines the post-70 CE transition and the institutional mechanisms by which rabbinic Judaism preserved and transmitted sacrificial knowledge despite the Temple's destruction. Analyzes how the archiving reading emerged as one response to the performative impossibility.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, historian_observer, observer,
    analytical, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_obligation__study_as_archiving, rabbinic_authority_structure).
narrative_ontology:fixing_cost_class(temple_sacrifice_obligation__study_as_archiving, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves comprehensive textual and legal knowledge of pre-destruction sacrificial procedure and theological meaning so that the detailed system is not lost to time. Functions as an institutional memory system and a guarantee that restoration would be possible with detailed technical knowledge intact.
% TRANSFER_FUNCTION: Moves the burden of the unfulfilled sacrifice obligation onto the intellectual and spiritual labor of study: practitioners are obligated to engage in perpetual textual analysis and transmission in lieu of performance. The transfer is from performance (no longer possible) to archiving (perpetually required).
% ABSENT_VOICES: Those who believe the obligation was genuinely suspended at the Temple's destruction (messianic_suspension reading) and those who believe study itself constitutes fulfillment (study_as_occupation reading) are structurally excluded from this reading's framework. Their objections to the non-compliance status are pre-emptively ruled out by the archiving framing.
% DISAPPEARANCE_RATIONALE: If this reading and its enforcement machinery vanished, the post-Temple Jewish community would need to adopt one of the alternative readings (suspension or occupation) to resolve the non-compliance status. The constraint's disappearance would force an explicit resolution of the theological/halakhic gap it currently holds open.
% FOUNDING_PROBLEM: After the Temple's destruction in 70 CE, the sacrificial commandments in the Torah could no longer be performed. The authority structure faced a choice: declare the commandments suspended, declare study a fulfillment, or declare them binding but unmet. This reading chose the third path: the obligation remains binding and unfulfilled; study preserves knowledge for future restoration.
% FOUNDING_PROBLEM_CORROBORATION: The rabbinic texts (Talmud, Mishnah, medieval codes) attest that the commandment remains in force post-Temple and study is obligatory. Historians outside the benefiting interpretive tradition corroborate that this was one of three major hermeneutical responses to the impossibility; no external authority mandated this particular reading, making the choice institutional and contestable.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__study_as_archiving, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__study_as_archiving, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__study_as_archiving, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__study_as_archiving, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__study_as_archiving, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__study_as_archiving_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_archiving, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temple_sacrifice_obligation__study_as_archiving_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.68) because the constraint binds practitioners to study labor that does not discharge the underlying obligation — the study substitutes for performance without resolving the debt. Suppression is substantial (0.72) because the constraint's persistence depends on actively excluding competing readings that would resolve the non-compliance status (either by suspension or by reframing study as fulfillment). Theater rises from 0.35 to 0.58 over the interval because early post-Temple Judaism engaged seriously with the question of why study could substitute for sacrifice; by the medieval and modern periods, the study-as-archiving frame became increasingly performative — a rationalization defending the perpetual obligation rather than a live problem-solving response. The measurement series tracks the gradual calcification of this reading into institutional theater. Theater_ratio stabilizes at 0.58 after 1800 because modern rabbinical Judaism has largely accepted the non-fulfillment as permanent condition, shifting the frame from 'archiving for eventual restoration' to 'archiving as commemoration of what was.' The rising theater trajectory indicates that the reading's functional purpose (preserving knowledge) has atrophied while its performative purpose (justifying the binding obligation) has intensified.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic authority seat, the constraint appears as a brilliant solution to a real theological problem: preserve the law, maintain its binding force, and keep the community bound to its knowledge in hope of restoration. From the individual practitioner seat, the same constraint appears as perpetual subjection to an impossible obligation — study that preserves but does not discharge, binding force that cannot be met, and a gap between obligation and fulfillment that institutional theology fills with theater rather than resolution. The engine should compute these seats as fundamentally divergent in their experienced type: the authority seat sees coordination (preserving knowledge against loss), while the practitioner seat experiences extraction (perpetual labor for an obligation that cannot be met). The directionality override for institutional power reflects this: at institutional scale, the d-value is higher (toward target) because the institutional actor controls whether the obligation remains unmet or gets resolved through alternative readings.
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinic authority structure benefits from the archiving reading: it preserves their interpretive authority and jurisdiction over sacrifice law, keeps study mandatory (sustaining educational institutions), and avoids having to declare the obligation either suspended or transformed. The individual practitioner is the target: bound to perpetual study of an unperformable law, bearing the psychological and intellectual weight of non-compliance without the option to declare the obligation inert or fulfilled. The scholarly tradition occupies a dual position: it benefits from the archiving frame (gives textual work theological meaning beyond scholarship) while also paying into it (obligated to preserve the knowledge through study). The unfulfilled divine command is the victim in an analytical sense — the constraint keeps it binding but unmet, suspending resolution indefinitely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserve sacrificial knowledge after Temple destruction) was live and urgent in the immediate post-70 period. By the medieval period, the problem had been solved — comprehensive halakhic literature on sacrifice existed in written form and did not risk loss. Yet the obligation to study sacrifice law persists and is binding. This is classic mandatrophy: the founding problem is dead (knowledge is preserved and will not be lost), but the authority structure maintains the obligation's binding force because doing so provides jurisdictional control, educational purpose, and a framework for maintaining Jewish identity continuity. The theater_ratio trajectory (0.35 → 0.58) maps this mandatrophy: as the archiving problem was solved, the study's justification shifted from functional (preserve endangered knowledge) to performative (ritualize our debt to the unmet obligation). The constraint shows mandatrophy_resolved = true because the gap between founding problem and current operation is clear and substantial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    archiving_vs_suspension_kernel_contest,
    'Does the binding force of the sacrificial commandment genuinely persist post-Temple (this reading''s premise), or was it suspended/transformed by the destruction (sibling readings'' premises)?',
    'Textual interpretation of the halakhic status of unperformable commandments; historical examination of whether alternative readings were treated as live options or heretically excluded by rabbinic authorities; analysis of whether the archiving frame is presented as a solution or a rationalization.',
    'If the obligation was genuinely binding post-Temple, this reading''s framework holds and the archiving is a legitimate response to a real gap. If the obligation was functionally suspended or transformed, the archiving reading is a cover story for managing a resolved problem while maintaining institutional authority. This would reclassify the constraint from tangled_rope (coordination function + extraction) to snare (extraction with cover story).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(archiving_vs_suspension_kernel_contest, empirical, 'Whether the obligation remains genuinely binding or the binding status is itself a constructed frame.').

omega_variable(
    identity_lock_mechanism_suppression,
    'Is the suppression of exit options (the identity_locked status of practitioners) a structural feature of Jewish identity/covenant, or is it internalized suppression maintained by the authority structure''s framing?',
    'Post-exit ethnographic analysis: practitioners who formally exit the obligation (via apostasy, conversion, or conscious renunciation) and report whether the suppression persists; analysis of whether practitioners internalize the obligation as identity-constitutive (covenant language) or experience it as institutional imposition.',
    'If identity-lock is structural (the covenant genuinely binds via identity-fusion), the suppression metric reflects an irreducible feature of the constraint. If internalized (the authority structure maintains the lock through theology and education), the effective suppression is higher than the structural measure suggests — targets carry the constraint with them even after formal exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_suppression, empirical, 'Whether identity-lock in the covenant is structural or internalized suppression.').

omega_variable(
    restoration_timeline_indefinite,
    'Is the ''eventual restoration'' referenced in the archiving frame a genuine eschatological expectation with a defined timeline, or has it become indefinitely deferred, collapsing the archiving rationale into pure preservation without purpose?',
    'Historical analysis of messianic timeline claims across periods; examination of whether contemporary rabbinic authorities maintain restoration as an active expectation or treat it as mythological cover for permanent non-performance.',
    'If restoration is genuinely expected within a definable horizon, archiving has a functional purpose and the constraint''s extraction is bounded by that horizon. If restoration is indefinitely deferred or mythological, the archiving frame becomes purely performative — study without purpose, obligation without end — reclassifying the constraint as piton (inertial, theatrical, not meeting its founding purpose).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_timeline_indefinite, conceptual, 'Whether restoration is a live expectation or has become indefinitely deferred.').

omega_variable(
    alternative_reading_suppression_mechanism,
    'Are alternative readings (messianic suspension, study as occupation) genuinely suppressed by this reading''s authority structure, or do they coexist as legitimate positions in contemporary Jewish tradition?',
    'Survey of contemporary rabbinic literature, educational curricula, and denominational positions; examination of whether alternative readings are presented as live options or heterodox exclusions.',
    'If alternative readings are genuinely suppressed, the constraint''s enforcement includes active institutional exclusion, supporting the tangled_rope classification (coordination function + enforcement). If they coexist as legitimate positions, the suppression metric overestimates the constraint''s coercive force and the classification should shift toward rope (coordination without forced exclusion of alternatives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_reading_suppression_mechanism, empirical, 'Whether competing readings are suppressed or coexist as legitimate alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_archiving, 70, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t70, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 70, 0.35).
narrative_ontology:measurement(temp_tr_t500, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 500, 0.42).
narrative_ontology:measurement(temp_tr_t1200, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1200, 0.48).
narrative_ontology:measurement(temp_tr_t1800, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1800, 0.54).
narrative_ontology:measurement(temp_tr_t1950, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1950, 0.58).
narrative_ontology:measurement(temp_tr_t2026, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 2026, 0.58).

% Extraction over time
narrative_ontology:measurement(temp_be_t70, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 70, 0.55).
narrative_ontology:measurement(temp_be_t500, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 500, 0.62).
narrative_ontology:measurement(temp_be_t1200, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1200, 0.65).
narrative_ontology:measurement(temp_be_t1800, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1800, 0.68).
narrative_ontology:measurement(temp_be_t1950, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1950, 0.68).
narrative_ontology:measurement(temp_be_t2026, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t70, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 70, 0.6).
narrative_ontology:measurement(temp_su_t500, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 500, 0.66).
narrative_ontology:measurement(temp_su_t1200, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1200, 0.69).
narrative_ontology:measurement(temp_su_t1800, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1800, 0.71).
narrative_ontology:measurement(temp_su_t1950, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1950, 0.72).
narrative_ontology:measurement(temp_su_t2026, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 2026, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__study_as_archiving, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_obligation__study_as_archiving, 0.12).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation__messianic_suspension).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation__study_as_occupation).

% DUAL FORMULATION NOTE:
% This constraint is one reading (study_as_archiving) of the contested kernel 'temple_sacrifice_obligation'. The kernel bifurcates into three structurally distinct constraints depending on which reading instantiates it: messianic_suspension (obligation is inactive), study_as_archiving (obligation is binding but unfulfilled), and study_as_occupation (study is fulfillment). The three constraints share the same referent (the sacrificial commandment post-70 CE) but have different ε values, victim/beneficiary structures, and classification paths due to their different epistemic and normative premises. All three should be authored as separate constraint stories and linked via network.affects_constraints to model the kernel contest as a three-way structural divergence rather than a single constraint viewed from multiple angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
