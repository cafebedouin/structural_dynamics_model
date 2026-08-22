% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__study_as_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Study-as-Exercise Occupation of the Sacrificial Command
 *   domain: religious law / commitment systems
 *
 * SUMMARY:
 *   Within the halakhic tradition, the covenantal command to offer sacrifices
 *   lost its material preconditions with the destruction of the Second Temple
 *   in 70 CE. This story instantiates one reading of that standing
 *   arrangement — the study_as_exercise reading: engagement with the
 *   sacrificial laws is itself the performance of the divine command, so the
 *   commitment is fully occupied, not shelved, whenever the community studies
 *   what it cannot enact. The referent of epsilon is this standing
 *   arrangement assessed by the reading's own lights: a community whose
 *   members voluntarily devote labor to the texts and receive covenant
 *   continuity and meaning in return, with no party bearing uncompensated
 *   costs. The sibling readings (performance_only, hybrid_preparatory,
 *   symbolic_transformation) are separate constraint stories with their own
 *   epsilon values and beneficiary structures, linked through network edges
 *   rather than averaged into this one. The claimed type and the authored
 *   metrics are independent facts: the metrics describe near-floor extraction
 *   consistent with an identity-coordination practice whose costs are
 *   self-consumed by its participants.
 *
 * KEY AGENTS:
 *   - - halakhic_decisors: Agenda-setting seat (institutional / identity_locked) — determines and transmits the reading that study discharges the command
 *   - - studying_scholars: Primary beneficiary (organized / identity_locked) — performs the occupying labor and receives covenant assurance and standing
 *   - - yeshiva_academies: Secondary beneficiary with agenda-setting reach (institutional / identity_locked) — institutionalizes, funds, and schedules the practice
 *   - - covenant_laity: Diffuse beneficiary (moderate / constrained) — receives vicarious covenant continuity and funds the academies
 *   - - suspended_priesthood_lineage: Excluded voice (moderate / identity_locked) — the command's hereditary executors, office suspended
 *   - - restorationist_advocates: Excluded voice (organized / identity_locked) — insists the service resumes only in matter
 *   - - commitment_system_analyst: Analytical observer (analytical / analytical) — sees the full structure from outside the covenant
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__study_as_exercise, 0.14).
domain_priors:suppression_score(temple_sacrifice_commitment__study_as_exercise, 0.18).
domain_priors:theater_ratio(temple_sacrifice_commitment__study_as_exercise, 0.17).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, extractiveness, 0.14).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, theater_ratio, 0.17).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__study_as_exercise, rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__study_as_exercise, "Study-as-Exercise Occupation of the Sacrificial Command").
narrative_ontology:topic_domain(temple_sacrifice_commitment__study_as_exercise, "religious law / commitment systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__study_as_exercise, '869d47a8-0545-4f3c-8d6a-6e1f46899584').
narrative_ontology:cs_kernel_codification('869d47a8-0545-4f3c-8d6a-6e1f46899584', fixed_text).
narrative_ontology:cs_authority_grounding('869d47a8-0545-4f3c-8d6a-6e1f46899584', lineage).
narrative_ontology:cs_interpretation_layer_present('869d47a8-0545-4f3c-8d6a-6e1f46899584').
narrative_ontology:cs_reading_relation('869d47a8-0545-4f3c-8d6a-6e1f46899584', temple_sacrifice_commitment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('869d47a8-0545-4f3c-8d6a-6e1f46899584', temple_sacrifice_commitment__hybrid_preparatory, forecloses).
narrative_ontology:cs_reading_relation('869d47a8-0545-4f3c-8d6a-6e1f46899584', temple_sacrifice_commitment__symbolic_transformation, influences).
narrative_ontology:cs_axiom('869d47a8-0545-4f3c-8d6a-6e1f46899584', foundational, study_constitutes_divine_command_performance).
narrative_ontology:cs_axiom_status(study_constitutes_divine_command_performance, holdable).
narrative_ontology:cs_axiom_grounding('869d47a8-0545-4f3c-8d6a-6e1f46899584', study_constitutes_divine_command_performance, deontological).
narrative_ontology:cs_axiom('869d47a8-0545-4f3c-8d6a-6e1f46899584', foundational, command_binding_absent_material_conditions).
narrative_ontology:cs_axiom_status(command_binding_absent_material_conditions, holdable).
narrative_ontology:cs_axiom_grounding('869d47a8-0545-4f3c-8d6a-6e1f46899584', command_binding_absent_material_conditions, deontological).
narrative_ontology:cs_reference_frame('869d47a8-0545-4f3c-8d6a-6e1f46899584', study_occupies_command_as_full_performance).
narrative_ontology:cs_drift_state('869d47a8-0545-4f3c-8d6a-6e1f46899584', contemporary_restorationist_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('869d47a8-0545-4f3c-8d6a-6e1f46899584', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__study_as_exercise, studying_scholars).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__study_as_exercise, yeshiva_academies).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__study_as_exercise, covenant_laity).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__study_as_exercise, study_performance_equivalence_doctrine).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__study_as_exercise, command_durability_without_material_conditions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rabbis and academy heads who rule that engagement with the sacrificial texts discharges the covenantal duty those texts describe, teach that determination to each generation, and answer questions about its limits. Their standing rests on the chain of transmission they administer; stepping outside it would end their role as arbiters.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, halakhic_decisors, agenda_setter,
    institutional, generational, identity_locked, global).

% Students and masters who spend years working through the orders of Temple offerings — memorizing procedures, disputing cases, rehearsing a service they will never perform. The work returns to them the sense of keeping the covenant alive, communal standing, and in many settings a stipend; leaving the study hall would mean surrendering the identity the work built.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, studying_scholars, beneficiary,
    organized, biographical, identity_locked, global).

% Institutions that organize curricula around the sacrificial codes, house and fund the scholars, and carry the practice across generations. They receive tuition, donations, and prestige tied to the practice's continuance, and they set much of its rhythm through admissions, schedules, and honors.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, yeshiva_academies, beneficiary,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__study_as_exercise, yeshiva_academies, agenda_setter).

% Community members who do not themselves master the sacrificial corpus but understand the covenant as kept on their behalf in the study halls they fund. They attend services where the offering passages are recited, donate to the academies, and would bear the disruption if the arrangement of vicarious engagement ended.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, covenant_laity, beneficiary,
    moderate, generational, constrained, global).

% Families of hereditary Temple priests whose assigned work has been suspended for nineteen centuries. They preserve priestly status rules and blessing duties, watch their ancestral service studied rather than enacted, and are seldom asked whether words can occupy the office their line was created to fill.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, suspended_priesthood_lineage, excluded,
    moderate, generational, identity_locked, global).

% Groups preparing vessels, vestments, and procedures for a rebuilt Temple, holding that the service resumes only in matter and flame. They publish, lobby, and demonstrate at the Temple Mount's edge; the mainstream academies treat them as fringe pressure rather than as partners in determining what study accomplishes.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, restorationist_advocates, excluded,
    organized, generational, identity_locked, global).

% Scholars of religion and commitment systems who observe how the community sustains a command whose material preconditions are gone, comparing this arrangement with other traditions' responses to lost practice. They take no side in the covenant and bear none of its duties.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, commitment_system_analyst, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_commitment__study_as_exercise, diffuse).
narrative_ontology:fixing_cost_class(temple_sacrifice_commitment__study_as_exercise, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a dispersed covenant community bound to a commandment whose altar, Temple, and officiating priesthood no longer exist, by giving every generation a concrete, repeatable act — study — through which the commitment is exercised rather than abandoned or left idle.
% TRANSFER_FUNCTION: Moves time, attention, and scholarly labor into the sacrificial corpus; moves covenant assurance and communal standing back to those who study and to the community that funds them; moves no material goods from anyone to anyone.
% ABSENT_VOICES: The priestly families whose office the texts describe are rarely seated in the sufficiency debate; lay donors who sustain the academies seldom enter it; and in many traditional settings those barred from advanced study live inside the covenant without a voice on whether words can do the work of blood and fire. Restorationist advocates speak loudly but outside the academies' deliberative frame.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight — if study no longer counted as keeping the command — the community would face the raw choice its founders deferred: declare the sacrificial command lapsed and rewrite the covenant's self-understanding, or carry it as a permanently unkept debt. Curricula, liturgy, institutional funding, and clerical identity would all reorganize around whichever branch each faction took.
% FOUNDING_PROBLEM: After 70 CE the community stood bound to a central commandment it could no longer physically perform; it needed a way to remain faithful to the command without either abandoning it or living in acknowledged perpetual breach.
% FOUNDING_PROBLEM_CORROBORATION: Holders of the sibling readings attest the problem is live even while disputing this solution — performance_only advocates call the command unkept, hybrid_preparatory readers call it suspended; historians of the Second Temple's destruction and of rabbinic origins document the crisis independently of any benefiting party; restorationist movements organize around the problem's continuing openness. No party inside this reading's beneficiary set is the sole attester.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__study_as_exercise, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__study_as_exercise, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__study_as_exercise, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__study_as_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__study_as_exercise, 0.14, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction sits near the identity-coordination floor (0.14 at interval end against a 0.08 type floor): the practice's principal cost is the scholars' own study labor, which the reading defines as the benefit itself, so little uncompensated burden exists to measure. Suppression is low (0.18): the reading spread by teaching and codification, not coercion, and rival readings persist openly inside the same libraries and liturgies. Theater is low but drifting upward (0.05 to 0.17) as public recitation of the offering passages — performed by many who do not study the corpus deeply — grows alongside the core of live engagement; the functional center remains genuine study. Accessibility_collapse is moderate (0.40): once the framework is accepted, passive abandonment and idle waiting stop being live options, but the sibling readings remain intellectually available. Resistance is low (0.22): contestation is interpretive, not oppositional. The temporal series run on one shared eight-point grid so every tracked metric is authored at every examined time point; no suppression_requirement series is authored because the enforcement picture is static — the reading never depended on coercive machinery, and the scalar suppression value carries that fact.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the decisor seat the arrangement is faithful transmission of a received determination; from the scholar seat it is labor experienced as worship, self-compensating by construction; from the laity seat it is vicarious assurance purchased with donations; from the suspended priestly seat it is a vocation kept on ice by people who were not asked whether words can occupy an office of blood and fire; from the restorationist seat it is an insufficiency dressed as sufficiency. The engine computes these per-seat classifications from the structural data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: studying_scholars, yeshiva_academies, and covenant_laity sit near the beneficiary end, with no victim declarations producing any high-directionality seat anywhere in the structure. The scholars' apparent cost contribution is damped by the reading's own terms — their labor is the exercise, consumed by themselves as covenant value — so their effective position stays near-subsidized despite bearing the effort. The laity ride mildly free on vicarious discharge but voluntarily fund the institutions, keeping them near the beneficiary end rather than at pure subsidy. The suspended priestly lineage holds a near-symmetric suspended position: neither fed nor burdened by the study arrangement, awaiting a resumption it does not control. No directionality overrides are authored because the derivation from the declared structure already produces these positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — remaining bound to an unperformable central command — is still live: the material absence persists into the present, so the arrangement's function has not outlived its mandate and no sunset applies. The classification guards both failure directions. A naive extraction lens would misread the scholars' long unsalaried-seeming labor as exploitation; the rope structure preserves the fact that the labor is the point, self-consumed by its performers. Conversely, the near-floor epsilon and the tracked theater drift prevent romanticizing: if study hollows into recitation-without-engagement, the theater series is positioned to catch the slide toward inertial maintenance before it completes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_contest_kernel_status,
    'This file instantiates the study_as_exercise reading of the temple_sacrifice_commitment kernel; would a sibling reading (performance_only, hybrid_preparatory, symbolic_transformation) restructure the constraint''s beneficiary set and epsilon?',
    'A restored Temple with resumed material service supplies the decisive test: if study retains independent covenant value alongside resumed performance, this reading is vindicated; if study reverts to memory-training for a resumed rite, the performance_only structure takes over the seat.',
    'Under performance_only the current arrangement becomes archival maintenance of a defunct practice (rising theater ratio, no live beneficiary); under hybrid_preparatory it becomes transitional support pending restoration; under symbolic_transformation the vindicated doctrine changes and the beneficiary set widens to the praying community.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_kernel_status, conceptual, 'Which reading of the sacrifice-commitment kernel correctly characterizes study''s status.').

omega_variable(
    sufficiency_residual_obligation,
    'Does intellectual engagement fully occupy the sacrificial command, or does a residual unperformed obligation persist beneath the study-equivalence?',
    'Internal case analysis of where the tradition applies and withholds the substitution (atonement contexts, priestly dues, day-of-atonement service), combined with practitioner accounts of whether study leaves the sense of an outstanding debt.',
    'A persistent residual would constitute an uncompensated cost carried by consciences within the community — adding a cost-bearing seat, raising epsilon above the identity-coordination floor, and drifting the classification toward a hybrid coordination/extraction structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sufficiency_residual_obligation, conceptual, 'Whether study-equivalence leaves a residual unmet obligation.').

omega_variable(
    institutionalization_extraction_layer,
    'Has the professionalization of study (stipends, career tracks, institutional budgets) layered an extraction component onto what this reading holds to be intrinsically valuable exercise?',
    'Compare communities where study is voluntary and unsalaried with career-track academies: trace stipend flows, survey motive distributions, and test whether withdrawal of funding collapses participation.',
    'If career dependence dominates, scholars become suppliers of labor whose product accrues to institutions — gain_flow would name yeshiva_academies, epsilon would rise above the coordination floor, and the arrangement would acquire a payer seat it currently lacks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutionalization_extraction_layer, empirical, 'Whether institutionalized study carries an extraction layer.').

omega_variable(
    intrinsic_value_vs_compensatory_frame,
    'Is study''s value framed intrinsically (the exercise itself is the covenant''s present-tense delight) or compensatorily (words stand in for what was lost)?',
    'Textual and liturgical-phenomenological comparison of the affective framing of sacrificial-text study and recitation across eras: joy-of-command registers versus mourning-adjacent consolation registers.',
    'A compensatory frame would recast the arrangement as grief management for a lost cult — shifting the coordination type toward attachment coordination and changing what counts as functional versus theatrical activity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intrinsic_value_vs_compensatory_frame, conceptual, 'Intrinsic-exercise versus compensatory-consolation framing of study''s value.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__study_as_exercise, 220, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t220, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 220, 0.05).
narrative_ontology:measurement_basis(temp_tr_t220, observed).
narrative_ontology:measurement(temp_tr_t500, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 500, 0.06).
narrative_ontology:measurement_basis(temp_tr_t500, observed).
narrative_ontology:measurement(temp_tr_t900, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 900, 0.07).
narrative_ontology:measurement_basis(temp_tr_t900, observed).
narrative_ontology:measurement(temp_tr_t1300, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 1300, 0.09).
narrative_ontology:measurement_basis(temp_tr_t1300, observed).
narrative_ontology:measurement(temp_tr_t1700, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 1700, 0.11).
narrative_ontology:measurement_basis(temp_tr_t1700, observed).
narrative_ontology:measurement(temp_tr_t1880, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 1880, 0.13).
narrative_ontology:measurement_basis(temp_tr_t1880, observed).
narrative_ontology:measurement(temp_tr_t1967, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 1967, 0.15).
narrative_ontology:measurement_basis(temp_tr_t1967, observed).
narrative_ontology:measurement(temp_tr_t2025, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 2025, 0.17).
narrative_ontology:measurement_basis(temp_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(temp_be_t220, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 220, 0.08).
narrative_ontology:measurement_basis(temp_be_t220, observed).
narrative_ontology:measurement(temp_be_t500, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 500, 0.09).
narrative_ontology:measurement_basis(temp_be_t500, observed).
narrative_ontology:measurement(temp_be_t900, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 900, 0.1).
narrative_ontology:measurement_basis(temp_be_t900, observed).
narrative_ontology:measurement(temp_be_t1300, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 1300, 0.11).
narrative_ontology:measurement_basis(temp_be_t1300, observed).
narrative_ontology:measurement(temp_be_t1700, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 1700, 0.12).
narrative_ontology:measurement_basis(temp_be_t1700, observed).
narrative_ontology:measurement(temp_be_t1880, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 1880, 0.13).
narrative_ontology:measurement_basis(temp_be_t1880, observed).
narrative_ontology:measurement(temp_be_t1967, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 1967, 0.14).
narrative_ontology:measurement_basis(temp_be_t1967, observed).
narrative_ontology:measurement(temp_be_t2025, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 2025, 0.14).
narrative_ontology:measurement_basis(temp_be_t2025, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(temple_sacrifice_commitment__study_as_exercise, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__study_as_exercise, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__hybrid_preparatory).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% The colloquial label 'what happened to the sacrifice commandment after the Temple fell' covers four structurally distinct claims about the status of study: performance (this file), archive, preparation, and authorized transformation. Each claim now has its own story, its own epsilon, and its own beneficiary structure, per the epsilon-invariance principle — measuring study's status one way yields near-zero extraction, measuring it as archival maintenance of a defunct rite yields a very different profile, so they are not one constraint. Family links run through network.affects_constraints in all four files; the upstream talmudic determination recorded here is the resource the symbolic_transformation reading downstream reinterprets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
