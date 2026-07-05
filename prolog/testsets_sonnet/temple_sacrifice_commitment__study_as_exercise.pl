% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__study_as_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__study_as_exercise, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: temple_sacrifice_commitment__study_as_exercise
 *   human_readable: Study of Sacrifice Law as Full Performance of Divine Command (Study-as-Exercise Reading)
 *   domain: religious_law/halakhic_tradition
 *
 * SUMMARY:
 *   Following the destruction of the Second Temple, rabbinic tradition faced
 *   the problem of how to relate to sacrifice law (the laws of the korbanot)
 *   when the material conditions for performing sacrifice — the Temple, the
 *   altar, the priesthood in functioning form — no longer existed. This
 *   constraint instantiates the reading in which the talmudic and later
 *   halakhic position that 'the study of the laws of sacrifice is equivalent
 *   to their offering' is taken at face value: intellectual engagement with
 *   the relevant legal texts is not a substitute for, preparation for, or
 *   archive of the commandment — it IS the commandment's occupation, fully,
 *   in the absence of material conditions. Under this reading there is no
 *   deficit, no suspension, no waiting for restoration to complete the
 *   commitment. This is one of four structurally distinct constraints sharing
 *   a kernel (temple_sacrifice_commitment); the others — hybrid_preparatory,
 *   performance_only, symbolic_transformation — are separate stories with
 *   their own ε values, per the ε-invariance principle. Do not read this
 *   story as describing 'what Judaism believes about sacrifice' generally; it
 *   describes only the structural profile of the study-as-exercise reading
 *   specifically.
 *
 * KEY AGENTS:
 *   - study_communities: Primary beneficiary (organized/mobile) — occupies the commitment through intellectual engagement, maintains covenant fidelity without material sacrifice
 *   - yeshiva_scholars: Beneficiary and agenda_setter (institutional/mobile) — sets the interpretive terms of what counts as full occupation of the command via study
 *   - priestly_lineage_descendants: Excluded voice (moderate/constrained) — under a restorationist reading they would have a claim to material performance that this reading renders moot
 *   - restorationist_traditions: Excluded/observer seat (organized/analytical) — advocate for hybrid_preparatory or performance_only readings and would object that this reading forecloses restoration urgency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__study_as_exercise, 0.03).
domain_priors:suppression_score(temple_sacrifice_commitment__study_as_exercise, 0.05).
domain_priors:theater_ratio(temple_sacrifice_commitment__study_as_exercise, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, extractiveness, 0.03).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__study_as_exercise, rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__study_as_exercise, "Study of Sacrifice Law as Full Performance of Divine Command (Study-as-Exercise Reading)").
narrative_ontology:topic_domain(temple_sacrifice_commitment__study_as_exercise, "religious_law/halakhic_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__study_as_exercise, '74162c19-ad6f-4d83-8b79-40b1eb0570af').
narrative_ontology:cs_kernel_codification('74162c19-ad6f-4d83-8b79-40b1eb0570af', fixed_text).
narrative_ontology:cs_authority_grounding('74162c19-ad6f-4d83-8b79-40b1eb0570af', lineage).
narrative_ontology:cs_interpretation_layer_present('74162c19-ad6f-4d83-8b79-40b1eb0570af').
narrative_ontology:cs_reading_relation('74162c19-ad6f-4d83-8b79-40b1eb0570af', temple_sacrifice_commitment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('74162c19-ad6f-4d83-8b79-40b1eb0570af', temple_sacrifice_commitment__hybrid_preparatory, coexists_with).
narrative_ontology:cs_reading_relation('74162c19-ad6f-4d83-8b79-40b1eb0570af', temple_sacrifice_commitment__symbolic_transformation, influences).
narrative_ontology:cs_axiom('74162c19-ad6f-4d83-8b79-40b1eb0570af', foundational, study_fully_occupies_divine_command).
narrative_ontology:cs_axiom_status(study_fully_occupies_divine_command, holdable).
narrative_ontology:cs_axiom_grounding('74162c19-ad6f-4d83-8b79-40b1eb0570af', study_fully_occupies_divine_command, theological).
narrative_ontology:cs_axiom('74162c19-ad6f-4d83-8b79-40b1eb0570af', secondary, material_instantiation_not_required_for_full_performance).
narrative_ontology:cs_axiom_status(material_instantiation_not_required_for_full_performance, holdable).
narrative_ontology:cs_axiom_grounding('74162c19-ad6f-4d83-8b79-40b1eb0570af', material_instantiation_not_required_for_full_performance, theological).
narrative_ontology:cs_reference_frame('74162c19-ad6f-4d83-8b79-40b1eb0570af', temple_era_sacrificial_praxis).
narrative_ontology:cs_drift_state('74162c19-ad6f-4d83-8b79-40b1eb0570af', post_destruction_rabbinic_consolidation, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('74162c19-ad6f-4d83-8b79-40b1eb0570af', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__study_as_exercise, study_communities).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__study_as_exercise, yeshiva_scholars).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__study_as_exercise, covenant_fidelity_tradition).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__study_as_exercise, torah_study_equals_sacrifice_doctrine).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__study_as_exercise, divine_command_occupiable_through_intellect).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage in sustained textual study of sacrifice law (korbanot) as a core religious practice. Under this reading, that study itself fully satisfies the covenant obligation regarding sacrifice — no material infrastructure, no priesthood, no altar is needed. They are free to affiliate with other readings (hybrid_preparatory, performance_only) elsewhere in the tradition without material constraint; their exit option is doctrinal affiliation, which is genuinely available.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, study_communities, beneficiary,
    organized, generational, mobile, global).

% Set and transmit the interpretive framework establishing that study of sacrifice law constitutes full performance of the command. Their institutional authority (yeshivot, rabbinic courts, textual commentary traditions) both articulates this reading and benefits from it, since it grounds their own scholarly practice as the highest form of religious observance rather than as a lesser substitute for priestly performance.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, yeshiva_scholars, agenda_setter,
    institutional, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__study_as_exercise, yeshiva_scholars, beneficiary).

% Retain hereditary status (kohanim) tied to a potential future priestly role that this reading renders largely moot in the present — if study alone fully occupies the commitment, there is no present functional deficit for priestly restoration to fill. They are not coerced or extracted from by this reading, but their distinct hereditary claim has no operative content under it; they would find more structural relevance under hybrid_preparatory or performance_only readings.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, priestly_lineage_descendants, excluded,
    moderate, generational, constrained, national).

% Hold that the Temple's restoration remains a live, urgent religious goal and would object that treating study as full occupation of the commitment removes the theological urgency of restoration. They are not silenced or coerced — they simply operate outside this specific reading's community and articulate the hybrid_preparatory or performance_only positions instead.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, restorationist_traditions, excluded,
    organized, civilizational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_commitment__study_as_exercise, diffuse).
narrative_ontology:fixing_cost_class(temple_sacrifice_commitment__study_as_exercise, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, textually grounded practice through which a community maintains continuity of covenant obligation and religious identity around sacrifice law without requiring the physical infrastructure (Temple, altar, priesthood) that no longer exists. Everyone in the community can participate through study regardless of tribal lineage or geographic location, which is a genuine coordination achievement relative to a practice that formerly required centralized physical infrastructure and hereditary priestly status.
% TRANSFER_FUNCTION: No material transfer occurs. The doctrine transfers religious LEGITIMACY and STANDING — it certifies the studying community's practice as a full, non-deficient discharge of covenant obligation, rather than transferring goods, labor, or money from any party to another.
% ABSENT_VOICES: Restorationist traditions and priestly lineage descendants would object that treating study as full occupation removes theological pressure toward physical Temple restoration and renders their hereditary/practical role moot; they are not silenced by force but simply operate in different interpretive communities that this reading's community does not centrally engage.
% DISAPPEARANCE_RATIONALE: If this specific doctrinal reading vanished, study communities that hold it would need to either adopt a different reading (hybrid_preparatory or performance_only) or lose the theological grounding that currently certifies their practice as fully sufficient — a real rearrangement for them. But restorationist and priestly-lineage communities who already hold different readings would experience no change at all, since the doctrine's disappearance would simply mean one fewer competing interpretation in a field that already contains several. The verdict is genuinely contested between communities who structure their religious identity around this specific claim and those for whom it was never operative.
% FOUNDING_PROBLEM: After the Second Temple's destruction (70 CE), the sacrifice commandments could no longer be materially performed, creating an urgent theological problem: how can a community remain in covenant fidelity with commandments it is physically unable to fulfill? The study-as-exercise doctrine answers that intellectual engagement with the relevant law is itself the fulfillment, not merely a stopgap.
% FOUNDING_PROBLEM_CORROBORATION: Talmudic sources (e.g. Menachot 110a, treating study of sacrificial law as equivalent to offering) are cited by the studying community itself as corroboration, which is an in-tradition, not fully independent, source. Historians of religion outside the beneficiary community (scholars of rabbinic Judaism studying the post-Temple period as an academic rather than theological matter) corroborate that the doctrine emerged as a documented historical response to the Temple's destruction, but they do not corroborate whether the problem is 'live' or 'dead' in the present — that theological judgment remains internal to the tradition and is exactly where hybrid_preparatory and performance_only readings dispute the study_as_exercise position. No source entirely outside all four readings' shared tradition attests to which reading is theologically correct; only the historical fact of the doctrine's emergence is independently corroborated.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__study_as_exercise, contested).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__study_as_exercise, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__study_as_exercise, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__study_as_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__study_as_exercise, 0.03, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__study_as_exercise_tests).
:- end_tests(temple_sacrifice_commitment__study_as_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near zero (0.03) because under this reading no party pays a cost through the constraint's operation: the studying community receives the full benefit of covenant fidelity through an activity (study) that is intrinsically available, requires no coerced tribute, and creates no dependent victim class. Suppression is near zero (0.05) because no alternative practice is being actively foreclosed by force — the constraint operates by persuasion within a textual tradition, not by coercive exclusion of dissenting readings (the sibling readings coexist as live positions elsewhere in the same broader tradition). Theater ratio is low (0.08) because the study activity is not merely performative cover for an absent function — under this reading's own premises, the study literally IS the function, so there is no gap between performance and substance to measure as theater. Accessibility collapse is moderate-low (0.15): the alternative readings remain fully articulable and are, in fact, actively held by other communities within the same broader tradition, so alternatives have not collapsed even though this reading's own community may not entertain them locally.
 *
 * DIRECTIONALITY LOGIC:
 *   The studying community and its scholars are the structural beneficiaries: study is available to them without material infrastructure (no Temple, no altar, no priestly apparatus required), and the equivalence doctrine grants them full covenant standing on that basis alone. There is no victim group under this reading — no party is deprived, extracted from, or coerced by the claim that study equals sacrifice. This is the central structural feature that distinguishes this reading from performance_only, under which the studying community would be characterized as failing to occupy the commitment (a very different — though not extractive in the ordinary sense — relational structure), and from hybrid_preparatory, under which the same community occupies a suspended, incomplete state pending restoration.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy would apply if the founding problem (occupying a divine command without the material conditions to perform it in its original form) had been resolved or become moot while the study practice persisted only by institutional inertia. Under this reading specifically, that concern does not arise: the reading's own premise is that study fully and permanently occupies the command — there is no future material-condition-restoration event whose absence would create a mandatrophy gap, because under study_as_exercise the commitment was never contingent on the Temple's physical restoration in the first place. This is precisely what distinguishes it from hybrid_preparatory, where a live restorationist orientation means the founding problem (the Temple's absence) is explicitly still treated as unresolved and the practice is explicitly a stopgap — a structure much closer to classic mandatrophy risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_as_exercise_vs_sibling_readings,
    'Is study of sacrifice law genuinely a full, non-deficient occupation of the divine command (this reading), or is it a suspended holding-pattern (hybrid_preparatory), a mere archive of a defunct practice (performance_only), or an authorized transformation into a new instantiation (symbolic_transformation)?',
    'No empirical resolution is possible — this is a doctrinal/hermeneutic dispute within rabbinic tradition itself, adjudicated by which authorities and textual traditions a community follows (e.g. talmudic dicta equating study with sacrifice vs. maimonidean restoration-oriented readings vs. reform/conservative transformation theology). Resolution mechanism is communal doctrinal affiliation, not evidence.',
    'If the sibling readings are correct instead, this constraint''s zero-extraction, no-victim profile does not hold for those readings — hybrid_preparatory would carry a live restorationist orientation with different beneficiary dynamics, performance_only would treat the studying community as failing to occupy the commitment at all, and symbolic_transformation would relocate the commitment''s site entirely. Each is a DIFFERENT constraint (per ε-invariance), not a recalibration of this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_as_exercise_vs_sibling_readings, conceptual, 'This constraint instantiates ONE of four contested readings of the temple_sacrifice_commitment kernel; the readings are not reconcilable into a single ε.').

omega_variable(
    sincerity_vs_convenience_of_study_equivalence,
    'Is the doctrine that study equals sacrifice a sincere theological development responding to the destruction of the Temple, or does it function (whatever its origin) to relieve the studying community of any obligation toward restoration, making the equivalence doctrine convenient for those least positioned to rebuild the Temple?',
    'Historical-critical examination of when and by whom the equivalence doctrine was formulated relative to Temple-destruction-era political conditions, and whether its strongest proponents were also those most invested in scholarly (rather than priestly/political) authority structures.',
    'If convenience-driven, the doctrine''s beneficiary structure would include a class interest (scholarly authority displacing priestly authority) not captured in the current beneficiary declaration, which names only the studying community''s covenant fidelity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sincerity_vs_convenience_of_study_equivalence, conceptual, 'Whether the study-equals-performance doctrine has a self-serving genealogy for the scholarly class that formulated it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__study_as_exercise, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 0, 0.05).
narrative_ontology:measurement(temp_tr_t400, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 400, 0.06).
narrative_ontology:measurement(temp_tr_t900, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 900, 0.08).
narrative_ontology:measurement(temp_tr_t1400, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 1400, 0.08).
narrative_ontology:measurement(temp_tr_t1900, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 1900, 0.08).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(temp_be_t400, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 400, 0.02).
narrative_ontology:measurement(temp_be_t900, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 900, 0.03).
narrative_ontology:measurement(temp_be_t1400, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 1400, 0.03).
narrative_ontology:measurement(temp_be_t1900, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 1900, 0.03).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(temple_sacrifice_commitment__study_as_exercise, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__study_as_exercise, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_commitment__study_as_exercise, 0.08).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__hybrid_preparatory).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% This story is one of four members of the temple_sacrifice_commitment constraint family, decomposed per the ε-invariance principle because the natural-language label 'the commitment to observe sacrifice law' covers structurally distinct claims with different ε profiles: study_as_exercise (this story, ε≈0.03, no victims, full present occupation), performance_only (ε would register differently — the studying community is cast as failing to occupy the commitment, a structurally different relational claim), hybrid_preparatory (a suspended, restoration-oriented reading with live messianic-timeline dynamics), and symbolic_transformation (a relocation-of-instantiation claim). Each sibling should independently link back to this story and to each other via affects_constraints, forming a fully connected kernel family rather than a hub-and-spoke structure, since each reading exerts interpretive pressure on how the others are received within the broader tradition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
