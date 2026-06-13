% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__messianic_deferral
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__messianic_deferral, []).

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
 *   constraint_id: kodashim_commandment_status__messianic_deferral
 *   human_readable: Kodashim Commandment Status (Messianic Deferral Reading)
 *   domain: religious/halakhic/institutional
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of a contested kernel in
 *   halakhic theory: the status of commandments (kodashim—sacrifice laws)
 *   that cannot be physically performed in the absence of the Temple. The
 *   messianic deferral reading holds that the commandment remains binding and
 *   that study of sacrifice law maintains Jewish readiness for the messianic
 *   era when Temple service will be restored. This reading competes with two
 *   sibling readings: performance_only (the commandment is contingent on
 *   Temple existence and thus suspended/obsolete without it) and
 *   study_as_performance (studying the laws IS the fulfillment of the
 *   commandment in the present era). The kernel is the ancient Talmudic
 *   proposition itself: 'Whoever studies the laws of the burnt offering is
 *   considered as if he offered it.' This single proposition is interpreted
 *   three ways. The messianic deferral reading treats the conditional 'as if'
 *   as maintaining an obligation toward a deferred future state; the
 *   study_as_performance reading treats it as direct present fulfillment; the
 *   performance_only reading denies the proposition applies without material
 *   Temple performance. ε-invariance: these are structurally distinct
 *   constraints because their victim sets differ, their extractive mechanisms
 *   differ, and their justifications differ. A single observable (what
 *   proportion of resources flow to sacrifice-law study) does not
 *   disambiguate the readings—measuring it one way yields moderate
 *   extractiveness (opportunity cost to present generation), measuring it via
 *   internal justifications yields low extractiveness (all resources are
 *   legitimately directed at commandment fulfillment). Therefore, three
 *   separate constraint stories are required. This story is the messianic
 *   deferral reading alone.
 *
 * KEY AGENTS:
 *   - talmudic_scholar_authority: Institutional agenda-setter; administers the interpretive framework; identity-locked in the lineage that maintains messianic deferral reading
 *   - jewish_communities_diaspora: Organized collective payer/beneficiary; bears opportunity costs, derives identity continuity
 *   - present_generation_practitioners: Moderate-power payers; biographical horizon means they may not live to see messianic restoration; constrained exit
 *   - alternative_practice_systems: Excluded moderate-power actors; hold competing readings but are voiceless in rabbinic institutional discourse
 *   - messianic_era_recipients: Deferred powerless beneficiary; a contingent future set that cannot voice present preferences
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__messianic_deferral, 0.58).
domain_priors:suppression_score(kodashim_commandment_status__messianic_deferral, 0.42).
domain_priors:theater_ratio(kodashim_commandment_status__messianic_deferral, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, extractiveness, 0.58).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__messianic_deferral, scaffold).
narrative_ontology:human_readable(kodashim_commandment_status__messianic_deferral, "Kodashim Commandment Status (Messianic Deferral Reading)").
narrative_ontology:topic_domain(kodashim_commandment_status__messianic_deferral, "religious/halakhic/institutional").

domain_priors:requires_active_enforcement(kodashim_commandment_status__messianic_deferral).
narrative_ontology:has_sunset_clause(kodashim_commandment_status__messianic_deferral).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__messianic_deferral, '1c83204d-9f45-41a4-9d13-6f4c7108d42c').
narrative_ontology:cs_kernel_codification('1c83204d-9f45-41a4-9d13-6f4c7108d42c', fixed_text).
narrative_ontology:cs_authority_grounding('1c83204d-9f45-41a4-9d13-6f4c7108d42c', lineage).
narrative_ontology:cs_interpretation_layer_present('1c83204d-9f45-41a4-9d13-6f4c7108d42c').
narrative_ontology:cs_reading_relation('1c83204d-9f45-41a4-9d13-6f4c7108d42c', kodashim_commandment_status__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('1c83204d-9f45-41a4-9d13-6f4c7108d42c', kodashim_commandment_status__performance_only, coexists_with).
narrative_ontology:cs_axiom('1c83204d-9f45-41a4-9d13-6f4c7108d42c', foundational, restoration_as_live_contingency).
narrative_ontology:cs_axiom_status(restoration_as_live_contingency, holdable).
narrative_ontology:cs_axiom_grounding('1c83204d-9f45-41a4-9d13-6f4c7108d42c', restoration_as_live_contingency, theological).
narrative_ontology:cs_axiom('1c83204d-9f45-41a4-9d13-6f4c7108d42c', foundational, study_as_readiness_maintenance).
narrative_ontology:cs_axiom_status(study_as_readiness_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('1c83204d-9f45-41a4-9d13-6f4c7108d42c', study_as_readiness_maintenance, conventional).
narrative_ontology:cs_reference_frame('1c83204d-9f45-41a4-9d13-6f4c7108d42c', temple_service_obligatory).
narrative_ontology:cs_drift_state('1c83204d-9f45-41a4-9d13-6f4c7108d42c', contemporary_2000_years_post_destruction, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1c83204d-9f45-41a4-9d13-6f4c7108d42c', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, talmudic_study_culture).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, messianic_restoration_framework).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, present_generation_material_needs).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, alternative_religious_practices).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__messianic_deferral, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(kodashim_commandment_status__messianic_deferral, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__messianic_deferral_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_commandment_status__messianic_deferral, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_commandment_status__messianic_deferral_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because the messianic deferral reading creates genuine opportunity cost: study hours and institutional resources directed at preparing for a restoration that may never occur could instead serve present-generation religious needs (education, community support, adaptation to diaspora). The constraint extracts from the present in the name of a contingent future. Suppression is moderate (0.42) because the institutional authority maintains the reading through interpretive authority and curriculum control, but alternative readings exist in parallel (not violently suppressed, but systematically excluded from official legitimacy). Theater is high (0.61) because the measurement series shows a long secular trend: as the messianic era recedes further into the future (time_point 0 to 2000 represents 2000 years of deferral), the proportion of the constraint's operation devoted to performative maintenance of readiness (rather than substantive preparation that expects restoration) increases. The measurement data shows extractiveness stabilizing (0.48 → 0.58) while theater ratio rises (0.35 → 0.61), indicating that the constraint's function has shifted toward maintaining a cultural-identity marker while deferring restoration further into the future. Accessibility_collapse is moderate (0.48): practitioners cannot fully exit without collective action and identity loss, but alternative frameworks exist and some practitioners do choose them. Resistance is moderate (0.55): alternative readings mount real intellectual and organizational resistance; they have not defeated the messianic deferral reading but they challenge its authority continuously.
 *
 * PERSPECTIVAL GAP:
 *   The talmudic scholar authority computes this as an obligation-maintenance mechanism justified by future restoration—from that seat, the constraint is scaffold: temporary support for a deferred future state. From the present-generation practitioner seat, the same constraint computes as moderate extraction with weak messianic justification (the theater_ratio rising over 2000 years suggests that the deferral has become performative). From the alternative reading seats, it computes as snare: the reading suppresses competing interpretations and extracts resources from communities for a contingent future that may never arrive. The engine computes per-seat classifications from the structural data; the authored claim (scaffold) and the authored metrics (moderate extractiveness + high theater, indicating performative maintenance) are independent facts—their divergence is exactly what lifecycle drift detection measures.
 *
 * DIRECTIONALITY LOGIC:
 *   The talmudic scholar authority is a beneficiary (controls the interpretive framework, earns institutional authority from maintaining readiness, identity is constituted through the lineage—d near 0.2). Jewish communities diaspora are symmetric payers/beneficiaries (derive identity continuity and covenant fulfillment, bear opportunity costs—d near 0.5). Present-generation practitioners are the targets (bear opportunity costs for a deferred future; their biographical horizon means they personally will never inherit the messianic-era benefit they subordinate present needs to—d near 0.75). Alternative practice systems are excluded (would benefit from recapturing the resources now diverted to sacrifice-law study, but institutional authority prevents their reading from being authoritative—d near 1.0, but as excluded rather than internally targeted). Messianic-era recipients are contingent beneficiaries (d near 0.0 from the hypothetical future vantage, but they are powerless present-representation; they cannot object). The override mechanism is identity-lock: the talmudic scholar's identity is so fused with the messianic deferral reading that exit is not a free choice but would constitute professional and relational dissolution.
 *
 * MANDATROPHY ANALYSIS:
 *   The claim is scaffold: temporary support justified by restoration. The mandatrophy question is: Has the founding problem (how to maintain covenant-binding after Temple destruction) been genuinely solved, or does the solution depend on ongoing messianic deferral? If the founding problem is dead (alternative readings like study_as_performance or performance_only have fully solved the identity-continuity and covenant-fulfillment problem), then maintaining the sacrifice-law study infrastructure is theater—the constraint persists through institutional inertia and the authority's career dependence, not because the problem it was meant to address still exists. The theater_ratio rising from 0.35 to 0.61 over 2000 years supports this mandatrophy reading: the proportion of activity devoted to performative maintenance (institutional signaling of readiness) rather than substantive preparation (expecting restoration) increases as the deferred future recedes. The constraint has become a piton: institutionally maintained but largely theatrical. However, the messianic deferral reading itself contests this verdict—it asserts the founding problem remains live (restoration is possible; readiness is necessary) and therefore the constraint is not mandatrophy but legitimately deferred. This contest is captured in the founding_problem_status: contested. The computational question the engine addresses: if theater_ratio > 0.6 and suppression is maintained primarily against alternative readings (rather than against external resistance), and the constraint is claimed as temporary (scaffold) with indefinite deferral, does the combination of high theater + indefinite sunset + strong alternative readings equal mandatrophy? The answer requires the mismatch detector: founding_problem_status=contested + disappearance_verdict=contested suggests the constraint is at the boundary. The omega variables handle the irreducible uncertainty about whether the constraint is legitimately deferred or theatrically maintained.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deferred_vs_obsolete_boundary,
    'Is the sacrifice commandment genuinely deferred (restoration is a live contingency for which readiness is necessary) or effectively obsolete (the messianic era is so far in the future that maintaining readiness is performative rather than preparatory)?',
    'Examine the theological literature''s actual claims about messianic imminence: if major authorities hold restoration as a likely near-term event, the deferral is substantive; if restoration is treated as infinitely deferred, the constraint approaches obsolescence. Compare to base-rate historical analysis of how long indefinitely deferred obligations typically remain binding in institutions.',
    'Substantive deferral → scaffold type holds, moderate extractiveness justified by future contingency. Performative deferral → piton type: theater_ratio and institutional inertia explain persistence, extractiveness is unjustified opportunity cost. This omega directly affects whether the constraint is mandatrophy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deferred_vs_obsolete_boundary, empirical, 'Whether messianic restoration is a live contingency (deferral) or indefinite theater (obsolescence).').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Are the three sibling readings (messianic_deferral, performance_only, study_as_performance) genuinely coexistent in Jewish practice, or does the institutional authority of Orthodox rabbinic discourse effectively foreclose the alternatives by controlling who gets to be seen as a legitimate Jewish voice on this question?',
    'Survey Jewish institutional landscape: count active communities, intellectual authority centers, educational infrastructure for each reading. If alternative readings are confined to distinct movements with no shared institutional platforms, coexistence is more nominal than structural. If they produce cross-movement engagement and challenge, coexistence is live.',
    'Genuine coexistence → each reading is a live option, contention is visible. Institutional foreclosure → the messianic deferral reading maintains authority through suppression of alternatives rather than textual or logical superiority. This affects suppression metrics and whether exclusion of alternatives counts as snare-type extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, empirical, 'Degree of genuine coexistence vs. institutional foreclosure among sibling readings.').

omega_variable(
    identity_locked_exit_mechanism,
    'For talmudic scholar authorities, is the identity-lock to the messianic deferral reading irreversible (exit would require institutional and professional dissolution), or can scholars shift to alternative readings and remain recognizably within the Jewish intellectual tradition?',
    'Historical case study: examine scholars who have shifted from messianic deferral to study_as_performance or performance_only readings. Are they still cited, employed, taught? Or are they expelled/discredited? The degree of career portability across readings indicates whether identity-lock is structural or internalized.',
    'Irreversible lock → the agenda-setter is trapped (d near 1.0 despite beneficiary role). Portable shift → the agenda-setter has genuine exit options (d lowers toward 0.5). This affects whether the talmudic authority is a true beneficiary or a coerced administrator.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_exit_mechanism, empirical, 'Whether identity-lock to the messianic deferral reading is reversible or irreversible for institutional authorities.').

omega_variable(
    opportunity_cost_quantification,
    'What is the actual magnitude of resource opportunity cost borne by present-generation communities to maintain sacrifice-law study infrastructure?',
    'Budget analysis of Jewish institutional spending (yeshiva curriculum, rabbinic training, publication of codes and commentaries on sacrifice law) and cross-comparison to alternative uses that communities identify as deferred (social services, interfaith education, secular Jewish studies integration).',
    'If opportunity cost is high relative to community resources, extractiveness is underestimated (0.58 should be higher). If opportunity cost is low (these are intellectual/cultural investments that do not displace material resources), extractiveness is overestimated. This affects whether the constraint is snare-adjacent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(opportunity_cost_quantification, empirical, 'Magnitude of opportunity cost extracted from present generation for deferred messianic restoration.').

omega_variable(
    contingent_beneficiary_legitimacy,
    'Can a deferred, contingent, hypothetical future beneficiary (messianic-era Jews) legitimately constrain present-generation resource allocation? Or does the absence of present-day voice from that set render the arrangement extractive regardless of restoration likelihood?',
    'Normative framework review: does Jewish law or contemporary ethical philosophy recognize contingent future beneficiaries as legitimate holders of rights over present-generation constraint design? This is less empirical than conceptual.',
    'If contingent beneficiaries have legitimate claims → the constraint''s extraction (present subordination to future possibility) is ethically authorized. If not → the constraint is extractive by construction, using a fictional future beneficiary to justify present costs. This affects the conceptual classification and whether alternatives readings are suppressed via logic or via power.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contingent_beneficiary_legitimacy, preference, 'Ethical legitimacy of constraining present generations for contingent future beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__messianic_deferral, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__messianic_deferral, theater_ratio, 0, 0.35).
narrative_ontology:measurement(koda_tr_t500, kodashim_commandment_status__messianic_deferral, theater_ratio, 500, 0.42).
narrative_ontology:measurement(koda_tr_t1000, kodashim_commandment_status__messianic_deferral, theater_ratio, 1000, 0.52).
narrative_ontology:measurement(koda_tr_t1500, kodashim_commandment_status__messianic_deferral, theater_ratio, 1500, 0.6).
narrative_ontology:measurement(koda_tr_t2000, kodashim_commandment_status__messianic_deferral, theater_ratio, 2000, 0.61).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__messianic_deferral, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(koda_be_t500, kodashim_commandment_status__messianic_deferral, base_extractiveness, 500, 0.52).
narrative_ontology:measurement(koda_be_t1000, kodashim_commandment_status__messianic_deferral, base_extractiveness, 1000, 0.56).
narrative_ontology:measurement(koda_be_t1500, kodashim_commandment_status__messianic_deferral, base_extractiveness, 1500, 0.58).
narrative_ontology:measurement(koda_be_t2000, kodashim_commandment_status__messianic_deferral, base_extractiveness, 2000, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_commandment_status__messianic_deferral, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(koda_su_t500, kodashim_commandment_status__messianic_deferral, suppression_requirement, 500, 0.4).
narrative_ontology:measurement(koda_su_t1000, kodashim_commandment_status__messianic_deferral, suppression_requirement, 1000, 0.41).
narrative_ontology:measurement(koda_su_t1500, kodashim_commandment_status__messianic_deferral, suppression_requirement, 1500, 0.42).
narrative_ontology:measurement(koda_su_t2000, kodashim_commandment_status__messianic_deferral, suppression_requirement, 2000, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__messianic_deferral, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_commandment_status__messianic_deferral, 0.12).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status__study_as_performance).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status__performance_only).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, temple_restoration_expectation__messianic_timeline).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-member constraint family decomposed from the single ambiguous kernel: 'the halakhic status of sacrifice laws post-Temple destruction.' Each reading (messianic_deferral, study_as_performance, performance_only) is a structurally distinct constraint with different ε, different victim sets, different justifications. The kernel is the Talmudic proposition about studying sacrifice law counting 'as if' offering it. The three readings instantiate three different constraints because they have different meanings for 'as if'—the meaning changes the extraction structure. The messianic_deferral reading interprets 'as if' to mean 'maintaining readiness for future restoration'—moderate extractiveness, deferred beneficiary, present-generation victims. The study_as_performance reading interprets 'as if' to mean 'fulfilling the commandment in the present'—low extractiveness, present coordination benefit, no victims. The performance_only reading interprets 'as if' as inapplicable without Temple—zero extractiveness to the reading itself, commandment is obsolete. All three are live in Jewish practice; all three are readings of the same kernel. The links indicate structural influence: messianic_deferral affects the other two because Orthodox institutional authority promotes it; study_as_performance and performance_only influence messianic_deferral by providing intellectual and organizational alternatives that constrain its authority. The three-way network forms a constraint family where no member can be fully understood without reference to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kodashim_commandment_status__messianic_deferral, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
