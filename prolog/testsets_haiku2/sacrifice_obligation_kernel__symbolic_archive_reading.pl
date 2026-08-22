% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__symbolic_archive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__symbolic_archive_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: sacrifice_obligation_kernel__symbolic_archive_reading
 *   human_readable: Sacrifice Law as Cultural-Historical Archive
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   This constraint instantiates the symbolic-archive reading of the
 *   sacrifice obligation kernel. Under this reading, sacrifice law (the
 *   halakhic code governing the sacrificial system that operated in the
 *   Jerusalem Temple until 70 CE) is understood as a cultural-historical
 *   archive—a textual and conceptual inheritance whose study and preservation
 *   sustains Jewish collective memory and interpretive tradition, but which
 *   makes no binding halakhic claim on contemporary Jews. Study of sacrifice
 *   law is voluntary cultural practice, not fulfillment of an obligation.
 *   Zero extractiveness: no coercive structure, no victim set, no binding
 *   mandate violated. The constraint is a Rope: genuine coordination of
 *   cultural transmission through voluntary engagement. The kernel contest is
 *   among four irreconcilable readings of sacrifice law's post-Temple status:
 *   messianic suspension (divinely suspended, binding, awaiting restoration),
 *   performance requirement (study is preparatory, obligation requires
 *   performance), study as exercise (study fulfills the mitzvah), and
 *   symbolic archive (study preserves culture, no obligation). This story
 *   authors the symbolic-archive reading exclusively; the other readings are
 *   separate constraints in the constraint family, linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - jewish_collective_memory: beneficiary (non-agent entity) — the preserved historical consciousness and identity sustained through archive study
 *   - interpretive_tradition_continuity: beneficiary (non-agent entity) — the self-perpetuating machinery of halakhic study and reasoning
 *   - contemporary_jewish_community: beneficiary (organized power) — participants in voluntary study and cultural engagement
 *   - halakhic_scholars: agenda_setter (institutional power) — administer and frame the study and interpretation of sacrifice law
 *   - messianists: excluded (moderate power) — hold that obligation is divinely suspended, binding, not cultural
 *   - performance_literalists: excluded (moderate power) — hold that obligation requires physical performance, not study
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__symbolic_archive_reading, 0.0).
domain_priors:suppression_score(sacrifice_obligation_kernel__symbolic_archive_reading, 0.0).
domain_priors:theater_ratio(sacrifice_obligation_kernel__symbolic_archive_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__symbolic_archive_reading, rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel__symbolic_archive_reading, "Sacrifice Law as Cultural-Historical Archive").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__symbolic_archive_reading, "religious/halakhic").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__symbolic_archive_reading, '0c414469-df97-4df3-9c76-e806dfd06ea0').
narrative_ontology:cs_kernel_codification('0c414469-df97-4df3-9c76-e806dfd06ea0', distributed).
narrative_ontology:cs_authority_grounding('0c414469-df97-4df3-9c76-e806dfd06ea0', lineage).
narrative_ontology:cs_interpretation_layer_present('0c414469-df97-4df3-9c76-e806dfd06ea0').
narrative_ontology:cs_reading_relation('0c414469-df97-4df3-9c76-e806dfd06ea0', sacrifice_obligation_kernel__study_as_exercise_reading, forecloses).
narrative_ontology:cs_reading_relation('0c414469-df97-4df3-9c76-e806dfd06ea0', sacrifice_obligation_kernel__performance_only_reading, forecloses).
narrative_ontology:cs_reading_relation('0c414469-df97-4df3-9c76-e806dfd06ea0', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_axiom('0c414469-df97-4df3-9c76-e806dfd06ea0', foundational, sacrifice_obligation_non_binding_post_temple).
narrative_ontology:cs_axiom_status(sacrifice_obligation_non_binding_post_temple, holdable).
narrative_ontology:cs_axiom_grounding('0c414469-df97-4df3-9c76-e806dfd06ea0', sacrifice_obligation_non_binding_post_temple, conventional).
narrative_ontology:cs_axiom('0c414469-df97-4df3-9c76-e806dfd06ea0', secondary, study_preserves_without_obligating).
narrative_ontology:cs_axiom_status(study_preserves_without_obligating, holdable).
narrative_ontology:cs_axiom_grounding('0c414469-df97-4df3-9c76-e806dfd06ea0', study_preserves_without_obligating, instrumental).
narrative_ontology:cs_reference_frame('0c414469-df97-4df3-9c76-e806dfd06ea0', post_temple_interpretive_autonomy).
narrative_ontology:cs_drift_state('0c414469-df97-4df3-9c76-e806dfd06ea0', contemporary_pluralist_judaism, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0c414469-df97-4df3-9c76-e806dfd06ea0', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_collective_memory).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, interpretive_tradition_continuity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, contemporary_jewish_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The aggregate historical consciousness and textual knowledge that grounds Jewish identity across generations. Preserved through study of sacrifice law even absent performative obligation.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_collective_memory, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_collective_memory).

% The unbroken chain of halakhic interpretation and textual engagement that sustains the framework of Jewish law. Study of sacrifice law maintains the machinery of legal reasoning even when performance is suspended.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, interpretive_tradition_continuity, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(sacrifice_obligation_kernel__symbolic_archive_reading, interpretive_tradition_continuity).

% Engages with sacrifice law study voluntarily as cultural practice, historical knowledge, and spiritual discipline. No obligation imposed; participation is chosen. Gains connection to ancestral practice and deepened understanding of Jewish legal tradition.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, contemporary_jewish_community, beneficiary,
    organized, generational, mobile, global).

% Administer and teach the study of sacrifice law, interpret its status and meaning, and maintain the pedagogical infrastructure through which the archive is preserved. They set the agenda for how study is framed: as living legal exercise, historical artifact, or spiritual practice.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, halakhic_scholars, agenda_setter,
    institutional, biographical, mobile, regional).

% Hold that sacrifice obligation is divinely suspended pending messianic restoration, not rendered non-obligatory. Would contest the symbolic-archive framing as diminishing the binding character of the mitzvah. Their voice is excluded from this reading's authorization structure.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, messianists, excluded,
    moderate, generational, mobile, global).

% Maintain that sacrifice obligation requires physical performance; study is preparatory but does not fulfill the mitzvah. Would reject the reading that study alone satisfies the obligation. Their claim to the obligation's continuing binding force is structurally incompatible with this reading.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, performance_literalists, excluded,
    moderate, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves and transmits the textual and conceptual knowledge of sacrifice law across generations; sustains the interpretive tradition through voluntary study and intellectual engagement without imposing binding obligation.
% TRANSFER_FUNCTION: Moves knowledge, interpretive authority, and cultural memory from scholars to community, from past to present generations, through pedagogical and textual transmission. No material extraction occurs.
% ABSENT_VOICES: Messianists and performance literalists are excluded: they would argue that sacrifice obligation retains binding force (either divinely suspended or requiring physical performance), and that framing it as a cultural archive empties the mitzvah of its halakhic character. Their objection is structural to the reading—they deny its core premise.
% DISAPPEARANCE_RATIONALE: If the symbolic-archive reading of sacrifice law disappeared, the sacrifice texts would remain in the corpus; other readings (messianic suspension, performance requirement, study as exercise) would persist. The archive itself does not depend on this particular interpretive frame—the frame is one way of understanding the obligation's status post-Temple, not a mechanism whose absence reorganizes the world.
% FOUNDING_PROBLEM: After the destruction of the Second Temple, sacrifice can no longer be physically performed. The community must decide how to relate to sacrifice law: as a binding but currently impossible obligation (messianic reading), as requiring alternative performance modes like study (exercise reading), as divinely suspended (messianic suspension reading), or as a cultural-historical preservation (symbolic archive reading).
% FOUNDING_PROBLEM_CORROBORATION: All contemporary Jewish communities attest that sacrifice cannot be physically performed and that some interpretive frame is necessary. Scholars from multiple denominations acknowledge that study of sacrifice law persists across the tradition. The contest among readings is documented in halakhic literature, responsa, and contemporary Jewish philosophy from sources outside any single reading's beneficiary class.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__symbolic_archive_reading, world_unchanged).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__symbolic_archive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__symbolic_archive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__symbolic_archive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__symbolic_archive_reading, 0.0, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__symbolic_archive_reading_tests).
:- end_tests(sacrifice_obligation_kernel__symbolic_archive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is zero because the reading explicitly denies that any binding obligation exists—no one is coerced to study, no one violates a mitzvah by abstaining, no one bears costs as the price of participation. Suppression is zero: there are no barriers to exit, no alternatives foreclosed, no coercive enforcement. Theater ratio is zero: the entire function is the stated function—preservation of cultural knowledge. Accessibility collapse is very high (0.95) because once the reading is adopted (study preserves culture, no obligation), the alternative readings collapse as live options within a single framework: you cannot simultaneously hold that study is merely archival AND that it performs a binding mitzvah. Resistance is very low (0.05) because the reading encounters minimal structural opposition once accepted; it is contested at the kernel level (alternative readings exist), but once this reading is adopted, it is not internally resisted. The measurements are flat across the interval because this reading's status is not subject to drift or enforcement intensification—it is a stable interpretive commitment that the constraint does or does not instantiate depending on which community adopts it.
 *
 * PERSPECTIVAL GAP:
 *   The schema does not permit per-seat classification in this case because there is only one structural position: voluntary participation in cultural preservation. Messianists and performance literalists occupy different readings of the same kernel, not different seats within this constraint. If a messianist attends a study session, they are not 'experiencing' the symbolic-archive constraint—they are contesting its legitimacy. The engine's per-seat computation applies within a single constraint; cross-reading disagreement is a kernel-level phenomenon, not a seat-level one.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries are non-agents (collective memory, tradition continuity) and the organized contemporary community. No agents are targets of extraction. The halakhic scholars are the agenda-setters (they teach and interpret), but they are not extracting from anyone—their role is administrative and pedagogical. Directionality is not computed because there is no asymmetric extraction to measure. The constraint is coordination (voluntary study preserves shared culture) without extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading has no mandatrophy. The founding problem (how to relate to sacrifice law post-Temple) is live—all contemporary communities must answer it. The constraint does not persist after its function is obsolete; rather, this reading asserts that the function ITSELF is cultural preservation, not obligation enforcement, so it is categorically non-mandatroph. Other readings may face mandatrophy (if the performance reading persists without anyone performing, if the messianic reading indefinitely suspends without restoration approaching), but this one does not.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_authorization_grounding,
    'What authority structure legitimizes this reading as the correct interpretation of sacrifice law''s post-Temple status?',
    'Canonical sourcing: identify which classical texts (Talmud, medieval codes, modern responsa) articulate the symbolic-archive framing, and compare the authority of those sources to the sources backing competing readings.',
    'If the symbolic-archive reading lacks canonical grounding, it may be a modern interpretive innovation rather than a reading of the kernel itself. If grounded, the authority structure determines whether the reading is community-binding (if grounded in accepted legal authority) or one position among several.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_authorization_grounding, conceptual, 'Whether the symbolic-archive reading is a legitimate reading of the traditional kernel or a post-modern reframing.').

omega_variable(
    obligation_foreclosure_boundary,
    'Does adopting the symbolic-archive reading logically foreclose the study-as-exercise reading, or can both coexist (study preserves culture AND occupies the obligation)?',
    'Halakhic conceptual analysis: can a single act (study) be simultaneously cultural preservation and obligation-fulfillment? Do the two readings assign the same status to the obligation itself, or different statuses?',
    'If they can coexist, the readings are neighbors (influences), not opponents (forecloses). If the archive reading denies the existence of any obligation to be fulfilled, it forecloses the exercise reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(obligation_foreclosure_boundary, conceptual, 'Logical relationship between symbolic-archive and study-as-exercise readings.').

omega_variable(
    collective_memory_as_beneficiary,
    'Is Jewish collective memory a real ''beneficiary'' (a non-agent entity whose preservation constitutes a genuine benefit) or a rhetorical stand-in for individual community members?',
    'Philosophical analysis of realism about collective goods: does collective memory exist as an independent entity whose preservation is orthogonal to individuals'' experiences, or is it a shorthand for distributed individual benefits?',
    'If collective memory is a real entity with interests, the constraint is genuinely non-extractive (it benefits something that cannot be victimized). If it is rhetorical, the constraint''s status depends on whether individual community members are net beneficiaries of mandatory study.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collective_memory_as_beneficiary, conceptual, 'Metaphysical status of collective cultural memory as a beneficiary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__symbolic_archive_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement(sacr_tr_t5, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 5, 0.0).
narrative_ontology:measurement(sacr_tr_t10, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 10, 0.0).
narrative_ontology:measurement(sacr_tr_t15, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 15, 0.0).
narrative_ontology:measurement(sacr_tr_t20, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 20, 0.0).
narrative_ontology:measurement(sacr_tr_t25, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 25, 0.0).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(sacr_be_t5, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 5, 0.0).
narrative_ontology:measurement(sacr_be_t10, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 10, 0.0).
narrative_ontology:measurement(sacr_be_t15, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 15, 0.0).
narrative_ontology:measurement(sacr_be_t20, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 20, 0.0).
narrative_ontology:measurement(sacr_be_t25, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 25, 0.0).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_kernel__symbolic_archive_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__symbolic_archive_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_kernel__symbolic_archive_reading, 0.02).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__messianic_suspension_reading).

% DUAL FORMULATION NOTE:
% The sacrifice obligation kernel decomposes into four constraint stories, one per reading. This story (symbolic-archive reading) has zero extractiveness and frames study as voluntary cultural preservation. The study-as-exercise reading frames study as binding obligation fulfillment with moderate extractiveness. The performance-only reading rejects study as sufficient and has high extractiveness (obligation cannot be fulfilled). The messianic-suspension reading accepts study as preparation while maintaining the obligation is binding, pending restoration. Each reading assigns a different ε to the same kernel. The four stories are linked as a constraint family; no single story is authoritative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
