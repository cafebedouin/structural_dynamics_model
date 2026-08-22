% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__performance_only_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__performance_only_reading, []).

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
 *   constraint_id: sacrifice_obligation_kernel__performance_only_reading
 *   human_readable: Performance-Only Reading of the Sacrifice Obligation (Korbanot) Kernel
 *   domain: religious_law/halakhic_authority
 *
 * SUMMARY:
 *   This story instantiates the performance-only reading of the sacrifice
 *   obligation kernel: the position that the biblical commandment to offer
 *   korbanot requires the specific physical act at the specific site by the
 *   specific officiants, and that no substitute — including Torah study of
 *   the sacrificial laws — discharges the mitzvah. Under this reading, the
 *   destruction of the Second Temple in 70 CE did not transform, suspend, or
 *   archive the obligation; it left it standing and unfulfillable. The
 *   extractiveness measured here is structural, not agentive: there is no
 *   beneficiary collecting rent from the community's inability to perform,
 *   only an ever-lengthening gap between a live command and an absent
 *   capacity. This is one of four sibling readings of the same kernel; the
 *   messianic_suspension_reading treats the obligation as divinely paused
 *   rather than merely unperformed, the study_as_exercise_reading holds that
 *   intellectual engagement itself satisfies the command, and the
 *   symbolic_archive_reading denies the command retains any live halakhic
 *   force at all. Each sibling reading, generated separately, carries its own
 *   epsilon and its own beneficiary/victim structure; this file speaks only
 *   for the performance-only position.
 *
 * KEY AGENTS:
 *   - observant_jewish_community: entire covenant community across ~1900 years — bears the unfulfilled obligation with no available discharge
 *   - poskim_and_halakhic_authorities: administer and transmit this reading, but cannot supply the missing site or priesthood
 *   - temple_and_priesthood_lineage: structurally required party, entirely absent from the present arrangement
 *   - halakhic_scholarly_observers: analytical seat studying the reading's history and structure without practitioner stakes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__performance_only_reading, 0.81).
domain_priors:suppression_score(sacrifice_obligation_kernel__performance_only_reading, 0.35).
domain_priors:theater_ratio(sacrifice_obligation_kernel__performance_only_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__performance_only_reading, snare).
narrative_ontology:human_readable(sacrifice_obligation_kernel__performance_only_reading, "Performance-Only Reading of the Sacrifice Obligation (Korbanot) Kernel").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__performance_only_reading, "religious_law/halakhic_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__performance_only_reading, 'c83a2d09-7c39-4987-a904-2cb2e3e412d5').
narrative_ontology:cs_kernel_codification('c83a2d09-7c39-4987-a904-2cb2e3e412d5', fixed_text).
narrative_ontology:cs_authority_grounding('c83a2d09-7c39-4987-a904-2cb2e3e412d5', lineage).
narrative_ontology:cs_interpretation_layer_present('c83a2d09-7c39-4987-a904-2cb2e3e412d5').
narrative_ontology:cs_reading_relation('c83a2d09-7c39-4987-a904-2cb2e3e412d5', sacrifice_obligation_kernel__study_as_exercise_reading, forecloses).
narrative_ontology:cs_reading_relation('c83a2d09-7c39-4987-a904-2cb2e3e412d5', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('c83a2d09-7c39-4987-a904-2cb2e3e412d5', sacrifice_obligation_kernel__symbolic_archive_reading, forecloses).
narrative_ontology:cs_axiom('c83a2d09-7c39-4987-a904-2cb2e3e412d5', foundational, physical_performance_is_the_sole_valid_discharge).
narrative_ontology:cs_axiom_status(physical_performance_is_the_sole_valid_discharge, holdable).
narrative_ontology:cs_axiom_grounding('c83a2d09-7c39-4987-a904-2cb2e3e412d5', physical_performance_is_the_sole_valid_discharge, conventional).
narrative_ontology:cs_axiom('c83a2d09-7c39-4987-a904-2cb2e3e412d5', foundational, study_is_preparatory_not_constitutive_of_fulfillment).
narrative_ontology:cs_axiom_status(study_is_preparatory_not_constitutive_of_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('c83a2d09-7c39-4987-a904-2cb2e3e412d5', study_is_preparatory_not_constitutive_of_fulfillment, conventional).
narrative_ontology:cs_reference_frame('c83a2d09-7c39-4987-a904-2cb2e3e412d5', temple_cultic_performance_standard).
narrative_ontology:cs_drift_state('c83a2d09-7c39-4987-a904-2cb2e3e412d5', post_temple_destruction_diaspora, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('c83a2d09-7c39-4987-a904-2cb2e3e412d5', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__performance_only_reading, observant_jewish_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Remains bound, on this reading, by a scriptural commandment to bring specific physical offerings at a specific physical site that has not existed since 70 CE. Cannot perform the mitzvah through study, intention, prayer, or any substitute act; the obligation stands unfulfilled generation after generation with no available exit through action, migration, or reinterpretation that this reading would recognize as satisfying it. The gap is structural, not chosen — there is no site, no altar, no priesthood in functioning form, and no one to perform the act even for those most willing.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, observant_jewish_community, payer,
    powerless, civilizational, trapped, global).

% Transmit and adjudicate this reading within the tradition. Their institutional authority and interpretive legitimacy rest on holding the kernel's plain-sense performance requirement stable rather than resolving it into study or symbolic substitution; they administer the reading but did not create the structural impossibility and cannot lift it by ruling. Their exit from the reading itself would mean abandoning a premise their own authority is built on.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, poskim_and_halakhic_authorities, agenda_setter,
    institutional, generational, identity_locked, global).

% The kohanim lineage and the physical Temple site are the two structural preconditions this reading requires for fulfillment. Neither exists in operative form; they have no voice in the interpretive contest and no capacity to supply what the reading demands. Their absence is precisely what makes the obligation unperformable, yet they are not consulted as parties — they are simply missing.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, temple_and_priesthood_lineage, excluded,
    powerless, civilizational, trapped, regional).

% Study the history and structure of the obligation, the destruction of the Temple, and the divergent readings that have grown up around the unfulfillable command, without being bound by any of them as practitioners.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, halakhic_scholarly_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_kernel__performance_only_reading, diffuse).
narrative_ontology:fixing_cost_class(sacrifice_obligation_kernel__performance_only_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None identifiable under this reading. The command originally coordinated centralized ritual practice at a single site administered by a dedicated priesthood; this reading does not claim that any coordination function survives the Temple's destruction — it holds only that the performance requirement itself survives, uncoordinated and unmet.
% TRANSFER_FUNCTION: No resources, labor, or status transfer to any beneficiary under this reading. What moves is obligation itself: a standing debt of unperformed action assigned to every generation of the observant community, discharged by no one, inherited by the next.
% ABSENT_VOICES: The kohanim (priestly lineage) and any restored Sanhedrin or Temple administration would be the parties capable of actually satisfying the obligation as this reading defines it; they are structurally absent, not merely unconsulted. Their absence is not a political exclusion but the literal missing infrastructure the reading insists is required.
% DISAPPEARANCE_RATIONALE: If this reading disappeared overnight, some in the observant community would experience relief from a standing unfulfilled obligation and might migrate toward the study-as-exercise or messianic-suspension readings; others would regard the reading's disappearance as itself a halakhic impossibility, since the plain text does not permit substitution. Whether 'the world rearranges' depends on which authority's framework absorbs the vacancy, which is exactly the live contest among the sibling readings.
% FOUNDING_PROBLEM: The original command coordinated the Israelite/Jewish community's atonement, thanksgiving, and covenant-maintenance practice through centralized physical offerings at a designated site under priestly supervision.
% FOUNDING_PROBLEM_CORROBORATION: Josephus and rabbinic sources contemporaneous with and following the 70 CE destruction attest that the physical site and priesthood infrastructure ceased functioning; independent historians of the Second Temple period corroborate that no operative Temple cultic apparatus has existed since. No party benefiting from maintaining this specific reading's performance requirement is in a position to supply what would revive the founding function — corroboration here comes entirely from outside any beneficiary, because this reading declares that there is no beneficiary.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__performance_only_reading, contested).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__performance_only_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__performance_only_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__performance_only_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__performance_only_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__performance_only_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_kernel__performance_only_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_obligation_kernel__performance_only_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81 at interval end) not because any agent extracts value, but because the reading's own structure guarantees perpetual non-fulfillment of a binding command — a cost the community bears indefinitely with zero possibility of discharge under this reading's own terms. Suppression is moderate (0.35) rather than high: nothing coercively prevents belief in or adoption of a sibling reading, but the interpretive weight of centuries of poskim holding this position, and the plain sense of the relevant verses, constrains movement away from it. Accessibility collapse is fairly high (0.72): once a community accepts biblical literalism about the performance requirement, alternative readings (study-as-exercise, symbolic archive) become progressively harder to hold without appearing to concede that the obligation was never truly binding in the way this reading insists. Resistance is moderate (0.55): the reading persists against real pressure from communities that find perpetual unfulfillable obligation psychologically and theologically difficult, and against rival readings that offer discharge mechanisms this reading refuses to recognize. Theater ratio stays low throughout (0.05 to 0.15) because there is essentially no performative substitute activity being passed off as fulfillment under this reading — the entire point of the performance-only position is that no substitute counts, so nothing masquerades as satisfaction.
 *
 * PERSPECTIVAL GAP:
 *   From the poskim's seat, holding this reading faithfully preserves the integrity of the commandment system against premature substitution — a position of interpretive fidelity, not extraction. From the community's seat, the same structure is experienced as an unresolvable civilizational-scale debt with no discharge mechanism, carried across dozens of generations. The engine should compute these as structurally different experiences of the identical kernel-reading, not reconcile them.
 *
 * DIRECTIONALITY LOGIC:
 *   There is no beneficiary group under this reading — this is the single most important authored fact distinguishing performance_only_reading from its siblings. The entire observant community sits at high directionality (near the full-target end) because the obligation binds them without any party collecting from the binding. The poskim who administer the reading are not beneficiaries either; their institutional legitimacy is entangled with correctly transmitting the reading, not with extracting anything from the fact that it cannot be fulfilled. This is the case the prompt anticipated: 'this is structural impossibility, not extraction by an agent, but the gap between command and capacity persists.' The engine should register high effective extraction with no beneficiary seat to attribute it to — a diffuse-gap signature rather than a captured-rent signature.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (centralized atonement practice at a functioning Temple) is genuinely dead — no party disputes that the physical infrastructure is gone. What is contested is whether the obligation itself lapses when its founding conditions vanish. This reading's answer is no: the mandate survives its own impossibility. That is precisely the mandatrophy signature this story is built to surface — an arrangement whose founding function is dead by every corroborating account, yet whose binding force is authored, within this reading, as undiminished. The sibling readings each resolve the mandatrophy differently: messianic_suspension defers it, study_as_exercise dissolves it by redefining fulfillment, symbolic_archive dissolves it by denying continued halakhic force. Performance_only alone refuses all three resolutions and lets the gap stand open.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    obligation_survives_impossibility,
    'Does a divine commandment retain full binding force when the physical infrastructure required for its performance (Temple, altar, priesthood) has been destroyed through historical circumstance rather than through the commanded party''s choice, or does destruction of the precondition suspend or transform the obligation?',
    'This is not resolvable by empirical inquiry; it is a live question within halakhic jurisprudence itself, addressed differently by the four sibling readings of this kernel. Resolution would require either an authoritative ruling accepted across the tradition (historically absent for 1900+ years) or a structural event (e.g., Temple restoration) that renders the question moot rather than answered.',
    'If the obligation is held to lapse or transform under destroyed-precondition conditions, this reading''s high extractiveness score reflects a category error — the ''unfulfilled obligation'' would not exist in the form authored here. If the obligation is held to survive intact, as this reading claims, the extractiveness measured is structurally accurate: a genuine 1900-year gap between binding command and available capacity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(obligation_survives_impossibility, conceptual, 'Whether a commandment''s binding force survives the destruction of its performance preconditions — the central premise separating this reading from messianic_suspension_reading and study_as_exercise_reading.').

omega_variable(
    victim_without_beneficiary_coherence,
    'Can a constraint coherently have a large victim class (the entire observant community bearing unfulfilled obligation) with zero beneficiary, or does the absence of a beneficiary indicate this should be classified closer to a mountain-adjacent structural fact rather than a snare?',
    'Compare the engine''s computed classification against the authored claimed_type (snare) — if the engine''s snare gate requires a captured beneficiary and none is present, the divergence itself is diagnostic: it would indicate the framework needs a category for diffuse structural burden without capture, distinct from both mountain (no parties) and snare (captured extraction).',
    'If the engine reclassifies this as something other than snare due to the absent beneficiary, that reclassification is itself evidence about whether ''victim without beneficiary'' constraints need their own signature rather than being forced into the existing six-category scheme.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_without_beneficiary_coherence, conceptual, 'Whether a beneficiary-less, victim-bearing structure is coherently a snare, or names a gap in the six-category taxonomy.').

omega_variable(
    poskim_identity_lock_extent,
    'To what degree are halakhic authorities who transmit this reading identity-locked into it by their own institutional position, versus holding it as a genuinely revisable scholarly conclusion?',
    'Historical record of poskim who shifted between readings, and the career/institutional consequences (if any) of doing so, would indicate whether the identity_locked exit_options declaration for this stakeholder group is accurate or overstated.',
    'If poskim are more identity-locked than authored, the reading''s apparent stability partly reflects institutional path-dependency rather than pure textual fidelity; if less locked, the reading''s persistence is better explained by genuine interpretive conviction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(poskim_identity_lock_extent, empirical, 'Whether halakhic authorities'' commitment to this reading is institutionally locked-in or genuinely revisable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__performance_only_reading, 0, 1955).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(sacr_tr_t0, observed).
narrative_ontology:measurement(sacr_tr_t200, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 200, 0.08).
narrative_ontology:measurement_basis(sacr_tr_t200, observed).
narrative_ontology:measurement(sacr_tr_t600, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 600, 0.1).
narrative_ontology:measurement_basis(sacr_tr_t600, observed).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1000, 0.12).
narrative_ontology:measurement_basis(sacr_tr_t1000, observed).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1500, 0.14).
narrative_ontology:measurement_basis(sacr_tr_t1500, observed).
narrative_ontology:measurement(sacr_tr_t1955, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1955, 0.15).
narrative_ontology:measurement_basis(sacr_tr_t1955, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(sacr_be_t0, observed).
narrative_ontology:measurement(sacr_be_t200, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 200, 0.5).
narrative_ontology:measurement_basis(sacr_be_t200, observed).
narrative_ontology:measurement(sacr_be_t600, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 600, 0.62).
narrative_ontology:measurement_basis(sacr_be_t600, observed).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1000, 0.7).
narrative_ontology:measurement_basis(sacr_be_t1000, observed).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1500, 0.77).
narrative_ontology:measurement_basis(sacr_be_t1500, observed).
narrative_ontology:measurement(sacr_be_t1955, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1955, 0.81).
narrative_ontology:measurement_basis(sacr_be_t1955, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_kernel__performance_only_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__performance_only_reading, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% This is one of four sibling readings of sacrifice_obligation_kernel, each generated as a separate constraint story with its own epsilon and beneficiary/victim structure per the ε-invariance principle. performance_only_reading authors the highest extractiveness of the four because it alone refuses every proposed discharge mechanism (suspension, study, symbolic reframing), leaving the command-capacity gap fully open across the entire interval. study_as_exercise_reading and symbolic_archive_reading each resolve the gap by redefining what counts as fulfillment or by denying continued halakhic force; messianic_suspension_reading resolves it by deferring the obligation's active force to a future restoration. All four link to each other via affects_constraints as members of one constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
