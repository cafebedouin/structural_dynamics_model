% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kodashim_commandment_status__study_as_performance
 *   human_readable: Study of Kodashim Laws as Commandment Fulfillment
 *   domain: religious/halakhic/commitment_system
 *
 * SUMMARY:
 *   This constraint story represents the 'study_as_performance' reading of
 *   the contested kernel 'kodashim_commandment_status' — the halakhic status
 *   of Temple sacrifice commandments after the Temple's destruction. This
 *   reading, rooted in Talmudic passages (Menachot 110a, Ta'anit 27b), holds
 *   that engaging in the study of the sacrificial laws (kodashim) constitutes
 *   actual fulfillment of the commandments themselves. The kernel remains
 *   'occupied' through intellectual engagement; no performance gap exists
 *   because study IS the performance mode available in exile. The reading
 *   carries zero extractiveness — no one is harmed by non-performance of
 *   literal sacrifices, and study is universally accessible. Beneficiaries
 *   are those who study (kodashim_scholars, torah_students,
 *   yeshiva_institutions), but the benefit is intrinsic commandment
 *   fulfillment, not external rent. The constraint claims Mountain status:
 *   the principle emerges from the internal logic of halakhah as a fixed
 *   structural feature, not a human policy choice.
 *
 * KEY AGENTS:
 *   - kodashim_scholars: Primary beneficiary (institutional/identity_locked) — fulfill commandment through specialized study; institutional status depends on this principle
 *   - torah_students: Beneficiary (organized/identity_locked) — any engaged student accesses fulfillment; exit requires abandoning religious identity
 *   - yeshiva_institutions: Beneficiary/agenda_setter (institutional/constrained) — curriculum authority and communal funding tied to kodashim centrality
 *   - halakhic_decisors: Agenda_setter (institutional/analytical) — maintain and transmit the principle through psak and commentary
 *   - performance_only_adherents: Excluded (moderate/constrained) — hold competing reading; would object to Mountain claim
 *   - academic_observers: Observer (analytical/analytical) — study the constraint from outside the commitment system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__study_as_performance, 0.0).
domain_priors:suppression_score(kodashim_commandment_status__study_as_performance, 0.0).
domain_priors:theater_ratio(kodashim_commandment_status__study_as_performance, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, extractiveness, 0.0).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__study_as_performance, mountain).
narrative_ontology:human_readable(kodashim_commandment_status__study_as_performance, "Study of Kodashim Laws as Commandment Fulfillment").
narrative_ontology:topic_domain(kodashim_commandment_status__study_as_performance, "religious/halakhic/commitment_system").

domain_priors:emerges_naturally(kodashim_commandment_status__study_as_performance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__study_as_performance, 'd52c0ca5-2731-4633-aa82-83d3cd1a5251').
narrative_ontology:cs_kernel_codification('d52c0ca5-2731-4633-aa82-83d3cd1a5251', fixed_text).
narrative_ontology:cs_authority_grounding('d52c0ca5-2731-4633-aa82-83d3cd1a5251', lineage).
narrative_ontology:cs_interpretation_layer_present('d52c0ca5-2731-4633-aa82-83d3cd1a5251').
narrative_ontology:cs_reading_relation('d52c0ca5-2731-4633-aa82-83d3cd1a5251', kodashim_commandment_status__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('d52c0ca5-2731-4633-aa82-83d3cd1a5251', kodashim_commandment_status__messianic_deferral, coexists_with).
narrative_ontology:cs_axiom('d52c0ca5-2731-4633-aa82-83d3cd1a5251', foundational, talmud_torah_as_avodah).
narrative_ontology:cs_axiom_status(talmud_torah_as_avodah, holdable).
narrative_ontology:cs_axiom_grounding('d52c0ca5-2731-4633-aa82-83d3cd1a5251', talmud_torah_as_avodah, deontological).
narrative_ontology:cs_axiom('d52c0ca5-2731-4633-aa82-83d3cd1a5251', foundational, study_fulfills_kodashim_commandments).
narrative_ontology:cs_axiom_status(study_fulfills_kodashim_commandments, holdable).
narrative_ontology:cs_axiom_grounding('d52c0ca5-2731-4633-aa82-83d3cd1a5251', study_fulfills_kodashim_commandments, deontological).
narrative_ontology:cs_reference_frame('d52c0ca5-2731-4633-aa82-83d3cd1a5251', study_fulfillment_framework).
narrative_ontology:cs_drift_state('d52c0ca5-2731-4633-aa82-83d3cd1a5251', contemporary_post_1967, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d52c0ca5-2731-4633-aa82-83d3cd1a5251', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__study_as_performance, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, kodashim_scholars).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, torah_students).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, yeshiva_institutions).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__study_as_performance, talmud_torah_kneged_kulam).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__study_as_performance, study_as_avodah_substitute).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__study_as_performance, oral_torah_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Specialized scholars who devote careers to kodashim literature. Their professional identity, communal status, and institutional positions (rosh yeshiva, posek, lecturer) depend on the principle that kodashim study is not merely preparatory but constitutively fulfills the commandments. Exit would require abandoning their professional-religious identity — the principle is fused with their self-concept as Torah scholars.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, kodashim_scholars, beneficiary,
    institutional, generational, identity_locked, global).

% Students in yeshiva/kollel frameworks who engage in kodashim study as part of standard curriculum. They access commandment fulfillment through study without barriers — any committed student can participate. The benefit is intrinsic (religious fulfillment); the cost is time/attention which is religiously valued regardless. Exit from the benefit would mean ceasing Torah study, which is identity-incompatible for this population.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, torah_students, beneficiary,
    organized, biographical, identity_locked, global).

% Institutions that structure curricula, allocate resources, and confer authority around kodashim mastery. They benefit from the principle's centrality — it justifies kodashim's place in the core curriculum, attracts students/donors, and validates the institutional model. As agenda_setters, they transmit and enforce the principle through curriculum design and rabbinic appointments. Exit is constrained: shifting away would undermine institutional legitimacy and funding.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, yeshiva_institutions, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__study_as_performance, yeshiva_institutions, agenda_setter).

% Poskim and roshei yeshiva who authoritatively maintain the study-as-fulfillment principle through psak, commentary, and oral transmission. They do not personally extract from the constraint but administer the commitment system that sustains it. Their authority derives from mastery of the tradition that includes this principle. Exit is analytical — they could theoretically reject the principle but would lose standing within the tradition.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, halakhic_decisors, agenda_setter,
    institutional, generational, analytical, global).

% Those (including some modern Orthodox and academic Talmud scholars) who hold the 'performance_only' reading: without a Temple and altar, the sacrifice commandments are literally suspended; study is valuable preparation but not fulfillment. They are excluded from this constraint's beneficiary structure because they reject its core axiom. Their objection would be that the principle obscures the Temple's centrality and delays messianic expectation. Exit from exclusion requires adopting this reading, which contradicts their halakhic/ideological commitment.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, performance_only_adherents, excluded,
    moderate, biographical, constrained, global).

% Scholars of religion, halakhic history, and Jewish studies who analyze the constraint from outside the commitment system. They neither benefit nor pay; they map the structural dynamics. Their exit is analytical — they can engage or disengage without identity cost.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, academic_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the operative force of the sacrifice commandments (kodashim) after the Temple's destruction by transposing physical performance into intellectual engagement, preserving the commandments as live obligations rather than historical artifacts.
% TRANSFER_FUNCTION: Moves nothing material. The arrangement transfers 'commandment fulfillment status' from the domain of physical sacrifice (no longer possible) to the domain of Torah study (universally accessible). No agent loses; all engaged students gain.
% ABSENT_VOICES: The 'performance_only' and 'messianic_deferral' readings' adherents are structurally excluded from this constraint's beneficiary set — they would object that study-as-fulfillment either diminishes the Temple's uniqueness (performance_only) or complacently settles for intellectual substitute (messianic_deferral). They are present in the broader discourse but not in this constraint's coordination logic.
% DISAPPEARANCE_RATIONALE: If the study-as-fulfillment principle vanished overnight, the kodashim commandments would become inert historical text within the halakhic system. Yeshiva curricula would restructure (kodashim would become elective, not core), the conceptual framework of 'avodah shebalev' (service of the heart) would lose its primary halakhic anchor, and the continuity of sacrificial law as live obligation would rupture. The halakhic world would rearrange around a vacuum.
% FOUNDING_PROBLEM: The destruction of the Second Temple (70 CE) rendered the physical performance of sacrifice commandments impossible, creating a categorical gap: commandments that are Torah obligations with no available performance mode.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (Temple destruction = performance impossibility) is attested by all three readings — performance_only, messianic_deferral, and study_as_performance all agree the Temple is destroyed and physical sacrifices cannot be offered. The disagreement is only on the commandment's status post-destruction. No party disputes the founding problem's reality; it is corroborated by historical fact, not merely by beneficiary assertion.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__study_as_performance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_commandment_status__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__study_as_performance, 0.0, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is zero because the arrangement creates no transfer from non-performers to performers — study is non-rivalrous and non-exclusionary. Suppression is zero because no enforcement mechanism compels study; the constraint operates through internalized religious commitment. Theater ratio is near-zero (0.05) because the study function is genuine — kodashim learning maintains detailed knowledge of Temple service that would otherwise be lost. Accessibility collapse is high (0.9) because within the commitment system, the principle that study fulfills the commandment is treated as structurally exhaustive — no alternative fulfillment mode exists without the Temple. Resistance is near-zero because the principle is broadly accepted across the traditional spectrum (even performance_only and messianic_deferral readings grant study a central role, differing only on whether it constitutes FULL fulfillment).
 *
 * PERSPECTIVAL GAP:
 *   From inside the study_as_performance reading, the constraint is a Mountain — a discovered halakhic truth with zero extraction. From the performance_only reading's seat, the same principle appears as a Scaffold or Rope — a provisional coordination mechanism that papers over the Temple's absence. From the messianic_deferral seat, it appears as a Piton — a once-vital readiness mechanism now maintained partly by inertia. The engine will compute these seat divergences from the structural data; the authored claim (Mountain) reflects this reading's internal logic.
 *
 * DIRECTIONALITY LOGIC:
 *   All named stakeholders are beneficiaries or agenda_setters — no victims exist. Kodashim_scholars and yeshiva_institutions sit at d ≈ 0.1 (beneficiary end) — they receive status and institutional coherence from the principle. Torah_students sit at d ≈ 0.0 (full beneficiary) — study is universally accessible and carries no cost beyond time/attention, which is religiously valued regardless. Halakhic_decisors sit at d ≈ 0.2 (mild beneficiary) — their authority is reinforced but not extractively. The performance_only and messianic_deferral readings are excluded from this constraint's beneficiary structure; they inhabit sibling constraints with different ε values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Temple destruction creating a performance gap for sacrifice commandments) is LIVE — the Temple remains unrebuilt. The arrangement (study-as-fulfillment) was built to solve this and continues to solve it. No mandatrophy: the constraint's function matches its founding problem. The mandate has not outlived its function because the condition (no Temple) persists. This distinguishes study_as_performance from the performance_only reading, which treats the mandate as suspended (dead founding problem, persisting husk).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_scholarly_construction,
    'Is the principle that study fulfills the sacrifice commandment a genuine structural feature of the halakhic system (Mountain), or a constructed reading that benefits the scholarly class by maintaining commandment relevance without Temple performance?',
    'Historical analysis of when and how the ''study as performance'' principle emerged in rabbinic literature; whether it appears as an discovered principle or an innovative response to Temple destruction.',
    'If constructed, the constraint reclassifies from Mountain to Rope or Scaffold with identifiable beneficiaries (scholarly institutions); if natural, Mountain certification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_scholarly_construction, conceptual, 'Whether study-as-fulfillment is discovered halakhic structure or scholarly innovation').

omega_variable(
    kernel_reading_disagreement_location,
    'What specific structural element do the three readings of kodashim_commandment_status disagree on?',
    'Map each reading''s axioms and reference frames to identify the precise node of divergence: the ontological status of the commandment without Temple, the definition of ''fulfillment'', or the authority of post-Temple rabbinic enactment.',
    'Locating the disagreement enables clean decomposition per ε-invariance; each reading gets its own ε over the same referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Structural locus of disagreement among study_as_performance, performance_only, messianic_deferral').

omega_variable(
    beneficiary_capture_of_kodashim_study,
    'Do yeshiva institutions and professional kodashim scholars materially benefit (funding, status, enrollment) from the study-as-fulfillment principle in ways that exceed the principle''s intrinsic coordination value?',
    'Sociological study of yeshiva funding models, enrollment patterns, and communal resource allocation tied to kodashim curriculum vs. other limmud.',
    'If material capture exists, the Mountain claim masks extraction; the constraint would carry Tangled Rope dynamics at the institutional level.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_capture_of_kodashim_study, empirical, 'Whether scholarly institutions extract rents from the study-as-fulfillment principle').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__study_as_performance, 0, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kodashim_study_perf_tr_t0, kodashim_commandment_status__study_as_performance, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(kodashim_study_perf_tr_t0, observed).
narrative_ontology:measurement(kodashim_study_perf_tr_t70, kodashim_commandment_status__study_as_performance, theater_ratio, 70, 0.05).
narrative_ontology:measurement_basis(kodashim_study_perf_tr_t70, observed).
narrative_ontology:measurement(kodashim_study_perf_tr_t1900, kodashim_commandment_status__study_as_performance, theater_ratio, 1900, 0.05).
narrative_ontology:measurement_basis(kodashim_study_perf_tr_t1900, observed).
narrative_ontology:measurement(kodashim_study_perf_tr_t1948, kodashim_commandment_status__study_as_performance, theater_ratio, 1948, 0.05).
narrative_ontology:measurement_basis(kodashim_study_perf_tr_t1948, observed).
narrative_ontology:measurement(kodashim_study_perf_tr_t1967, kodashim_commandment_status__study_as_performance, theater_ratio, 1967, 0.05).
narrative_ontology:measurement_basis(kodashim_study_perf_tr_t1967, observed).
narrative_ontology:measurement(kodashim_study_perf_tr_t2024, kodashim_commandment_status__study_as_performance, theater_ratio, 2024, 0.05).
narrative_ontology:measurement_basis(kodashim_study_perf_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(kodashim_study_perf_be_t0, kodashim_commandment_status__study_as_performance, base_extractiveness, 0, 0.0).
narrative_ontology:measurement_basis(kodashim_study_perf_be_t0, observed).
narrative_ontology:measurement(kodashim_study_perf_be_t70, kodashim_commandment_status__study_as_performance, base_extractiveness, 70, 0.0).
narrative_ontology:measurement_basis(kodashim_study_perf_be_t70, observed).
narrative_ontology:measurement(kodashim_study_perf_be_t1900, kodashim_commandment_status__study_as_performance, base_extractiveness, 1900, 0.0).
narrative_ontology:measurement_basis(kodashim_study_perf_be_t1900, observed).
narrative_ontology:measurement(kodashim_study_perf_be_t1948, kodashim_commandment_status__study_as_performance, base_extractiveness, 1948, 0.0).
narrative_ontology:measurement_basis(kodashim_study_perf_be_t1948, observed).
narrative_ontology:measurement(kodashim_study_perf_be_t1967, kodashim_commandment_status__study_as_performance, base_extractiveness, 1967, 0.0).
narrative_ontology:measurement_basis(kodashim_study_perf_be_t1967, observed).
narrative_ontology:measurement(kodashim_study_perf_be_t2024, kodashim_commandment_status__study_as_performance, base_extractiveness, 2024, 0.0).
narrative_ontology:measurement_basis(kodashim_study_perf_be_t2024, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_commandment_status__study_as_performance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__study_as_performance, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_commandment_status__study_as_performance, 0.08).
narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, kodashim_commandment_status__performance_only).
narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, kodashim_commandment_status__messianic_deferral).

% DUAL FORMULATION NOTE:
% This constraint, performance_only, and messianic_deferral form the kodashim_commandment_status constraint family. They share the same kernel (Torah sacrifice commandments) but instantiate different constraints with different ε values: study_as_performance ε=0.0 (Mountain), performance_only ε≈0.3 (Snare or Piton — suspended commandment creates vacuum filled by rabbinic enactments), messianic_deferral ε≈0.15 (Rope or Scaffold — readiness maintenance with deferred fulfillment). The ε-invariance principle requires separate stories because the referent (the standing arrangement of kodashim commandments) yields different extraction profiles under each reading's lights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kodashim_commandment_status__study_as_performance, institutional, 0.1).
constraint_indexing:directionality_override(kodashim_commandment_status__study_as_performance, organized, 0.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
