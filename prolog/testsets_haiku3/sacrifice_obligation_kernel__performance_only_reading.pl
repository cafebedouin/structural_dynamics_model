% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__performance_only_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: sacrifice_obligation_kernel__performance_only_reading
 *   human_readable: Sacrifice Obligation: Physical Performance Requirement (No Substitution by Study)
 *   domain: religious/halakhic/commitment-system
 *
 * SUMMARY:
 *   The performance-only reading of the sacrifice obligation kernel holds
 *   that the commandment to perform sacrifices remains binding and in full
 *   force, but study of the laws governing sacrifice does NOT constitute
 *   fulfillment of the mitzvah. This reading emerges from and depends upon
 *   the halakhic tradition's response to the Temple destruction of 70 CE. The
 *   constraint story traces the structural gap between a binding command
 *   (sacrifice) and the structural impossibility of performance (no Temple)
 *   across 1,956 years. The Jewish people are locked into an identity that
 *   carries the obligation while being locked out of the means to discharge
 *   it. This reading competes with three sibling readings that resolve the
 *   gap differently: messianic_suspension (obligation is suspended, not just
 *   unfulfilled), study_as_exercise (study counts as the mitzvah), and
 *   symbolic_archive (the law is cultural memory, not halakhic claim). The
 *   performance-only reading is distinguished by its insistence on the
 *   irreducibility of the gap—the obligation persists in full force,
 *   performance is what counts, and study is subordinate preparation. The
 *   extractiveness metric reflects this: 0.92 represents nearly two millennia
 *   of a binding obligation without legitimate discharge mechanism.
 *
 * KEY AGENTS:
 *   - jewish_people_post_70_ce: victims of the structural impossibility; commanded but unable to perform
 *   - rabbinical_authority_structure: agenda-setter maintaining the interpretation; carries the institutional burden of holding the obligation in abeyance
 *   - study_practitioners: organized seat gaining intellectual engagement but explicitly not discharge of obligation
 *   - messianic_restoration: abstract referent whose absence creates the extractive gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__performance_only_reading, 0.92).
domain_priors:suppression_score(sacrifice_obligation_kernel__performance_only_reading, 0.45).
domain_priors:theater_ratio(sacrifice_obligation_kernel__performance_only_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__performance_only_reading, snare).
narrative_ontology:human_readable(sacrifice_obligation_kernel__performance_only_reading, "Sacrifice Obligation: Physical Performance Requirement (No Substitution by Study)").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__performance_only_reading, "religious/halakhic/commitment-system").

domain_priors:requires_active_enforcement(sacrifice_obligation_kernel__performance_only_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__performance_only_reading, '643e081a-2826-44b4-b86d-978589fd4dc9').
narrative_ontology:cs_kernel_codification('643e081a-2826-44b4-b86d-978589fd4dc9', fixed_text).
narrative_ontology:cs_authority_grounding('643e081a-2826-44b4-b86d-978589fd4dc9', lineage).
narrative_ontology:cs_interpretation_layer_present('643e081a-2826-44b4-b86d-978589fd4dc9').
narrative_ontology:cs_reading_relation('643e081a-2826-44b4-b86d-978589fd4dc9', sacrifice_obligation_kernel__study_as_exercise_reading, coexists_with).
narrative_ontology:cs_reading_relation('643e081a-2826-44b4-b86d-978589fd4dc9', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('643e081a-2826-44b4-b86d-978589fd4dc9', sacrifice_obligation_kernel__symbolic_archive_reading, coexists_with).
narrative_ontology:cs_axiom('643e081a-2826-44b4-b86d-978589fd4dc9', foundational, physical_performance_nonsubstitutable).
narrative_ontology:cs_axiom_status(physical_performance_nonsubstitutable, holdable).
narrative_ontology:cs_axiom_grounding('643e081a-2826-44b4-b86d-978589fd4dc9', physical_performance_nonsubstitutable, deontological).
narrative_ontology:cs_axiom('643e081a-2826-44b4-b86d-978589fd4dc9', foundational, study_preparatory_not_discharge).
narrative_ontology:cs_axiom_status(study_preparatory_not_discharge, holdable).
narrative_ontology:cs_axiom_grounding('643e081a-2826-44b4-b86d-978589fd4dc9', study_preparatory_not_discharge, deontological).
narrative_ontology:cs_reference_frame('643e081a-2826-44b4-b86d-978589fd4dc9', torah_commandment_binding_without_condition).
narrative_ontology:cs_drift_state('643e081a-2826-44b4-b86d-978589fd4dc9', contemporary_post_temple_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('643e081a-2826-44b4-b86d-978589fd4dc9', '2026-06-13T14:32:18Z').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__performance_only_reading, jewish_people_post_70_ce).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__performance_only_reading, study_practitioners).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__performance_only_reading, rabbinical_authority_structure).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__performance_only_reading, study_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Commanded to perform sacrificial offerings as central religious duty; Temple destruction in 70 CE renders physical performance impossible. Bears the burden of an unfulfilled obligation—the gap between commanded act and available means. Cannot exit the identity that grounds the obligation; cannot perform the act that would discharge it. Rabbinical interpretation under this reading holds study as preparatory only, not substitutive, leaving the obligation structurally suspended rather than resolved.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, jewish_people_post_70_ce, payer,
    powerless, civilizational, identity_locked, global).

% Interprets and transmits the commandment through lineage. Under the performance-only reading, maintains the obligation as binding and unfulfilled, justifying intensive study as preparation for a restored Temple. Carries the institutional burden of holding a command in abeyance for two millennia while denying that study satisfies it. The interpretation itself constrains what solutions can be offered to the Jewish people.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, rabbinical_authority_structure, agenda_setter,
    institutional, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__performance_only_reading, rabbinical_authority_structure, payer).

% Engage deeply with sacrifice law through Talmudic and halakhic study, deriving intellectual and spiritual engagement. Under this reading, their study is explicitly NOT the fulfillment of the obligation, only preparation for a future physical performance that remains unavailable. They gain educational and communal structure but remain unable to complete the commanded act.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, study_practitioners, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__performance_only_reading, study_practitioners, payer).

% The abstract condition under which the obligation could theoretically be performed. Not an agent, but the structural referent that justifies the non-performance in the interim. Under this reading, its absence is what creates the extractive gap: a binding obligation whose discharge is made impossible by historical circumstance.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, messianic_restoration_expectation, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(sacrifice_obligation_kernel__performance_only_reading, messianic_restoration_expectation).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_kernel__performance_only_reading, diffuse).
narrative_ontology:fixing_cost_class(sacrifice_obligation_kernel__performance_only_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a community of practice around sacrificial law across centuries of non-performance, preserving detailed knowledge of ritual procedures, Temple architecture, and halakhic rules that would be lost without intensive study. The coordination is temporal: linking generations across the performance gap.
% TRANSFER_FUNCTION: Extracts intellectual labor, spiritual attention, and interpretive authority from the Jewish people under the frame of preparation for an obligation that cannot be discharged. The transfer is from victims to the institutional authority structure that interprets what counts as legitimate engagement with an impossible command.
% ABSENT_VOICES: Adherents of the sibling readings—those who hold that study constitutes the mitzvah (study_as_exercise_reading) or that the obligation is truly suspended (messianic_suspension_reading)—are structurally present as dissenting communities but marginalized under this reading's interpretive framework. They would argue for different frameworks that resolve the gap between command and capacity; their voices are not absent geographically but are subordinated institutionally.
% DISAPPEARANCE_RATIONALE: If this reading vanished (replaced by the study_as_exercise_reading or symbolic_archive_reading), the obligation's character would shift from binding-but-impossible to either discharged-through-study or transformed-into-memory. The Jewish people's relationship to their own commandment would reorganize; the institutional authority structure's role would change from gatekeeper of an unfulfilled obligation to administrator of a completed or reframed one.
% FOUNDING_PROBLEM: The Temple destruction of 70 CE rendered physical sacrifice impossible. How should a people bound by commandment to perform sacrifices exist when performance is structurally unavailable? This reading's answer: the obligation persists in full force; study is preparation, not substitute; the gap between command and capacity is an irreducible feature of the post-Temple condition.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Jewish law and rabbinic Judaism (Neusner, Stern, Cohen) acknowledge that the Temple destruction forced a theological and halakhic crisis. This reading's position (performance-only, study as preparatory) is attested in major streams of halakhic tradition and is live in Orthodox interpretive communities. The messianic_suspension and study_as_exercise readings represent competing solutions attested in the same historical record; all three readings coexist as live positions in Jewish jurisprudence, which is itself evidence that the founding problem remains contested rather than resolved.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__performance_only_reading, contested).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__performance_only_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__performance_only_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__performance_only_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__performance_only_reading, 0.92, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is exceptionally high (0.92) because the constraint creates an irreducible gap between command and capacity that persists across the entire measurement interval. The Jewish people cannot exit the identity that grounds the obligation (identity_locked), and performance is impossible (Temple destroyed, sacrificial service unavailable). Under this reading, study does not resolve the gap—it is explicitly preparation, not fulfillment. Theater_ratio rises sharply (0.05 → 0.68) because as centuries pass and the messianic expectation becomes more attenuated, the study apparatus increasingly functions as cultural preservation and identity maintenance rather than genuine preparation for a restorable performance. By the modern period, the performative function (keeping the mitzvah 'alive' through study, teaching it to new generations, maintaining the intellectual tradition) substantially outweighs the preparation function. Suppression_requirement is moderate and stable (0.35 → 0.45) because the constraint does not depend primarily on external coercion—the Jewish people maintain the obligation and study apparatus through internal commitment and identity investment. What suppression exists is mostly the institutional suppression of alternative readings (study_as_exercise, symbolic_archive) that would resolve the gap more cheaply. Accessibility_collapse is high (0.78) because once the performance-only reading is adopted, the gap becomes seemingly irreducible: alternatives (other readings) exist but are linguistically and institutionally harder to access, and most practitioners internalize the performance standard as binding even while being unable to meet it.
 *
 * PERSPECTIVAL GAP:
 *   The rabbinical authority seat and the jewish_people seat compute radically differently. From the authority's position, the reading preserves the integrity of the commandment, maintains a coherent interpretive tradition, and justifies intensive study as necessary intellectual preparation. From the people's position, the reading locks them into an obligation that cannot be discharged, offers them study as a substitute that is explicitly denied as a substitute, and provides no exit from either the identity or the obligation. The engine computes per-seat directionality from the structural data: authority gets moderate d (benefits from interpretive role, but also bears institutional burden of sustaining the reading); people get high d (locked in, powerless, obligation persists). This seat divergence IS the core structural fact the reading instantiates—the performance-only frame creates an asymmetry where the authority can maintain a coherent framework while the people remain under an irreducible gap.
 *
 * DIRECTIONALITY LOGIC:
 *   The Jewish people are the victims: they carry the obligation through identity (identity_locked), cannot perform (powerless, no Temple, no authority to restore it), and remain under the obligation indefinitely (civilizational time horizon). Their directionality is high (near 1.0, full target) because they bear the cost of the gap between command and capacity with no escape. There are no beneficiaries under this reading—no institutional actor or group gains sustained rents from the obligation's non-fulfillment. The rabbinical authority structure sits at secondary_role:payer because while it gains interpretive authority and institutional prestige from administering the obligation, it also carries the burden of maintaining an interpretation that keeps the gap in place. The study practitioners are ambiguous: they gain intellectual and spiritual engagement (beneficiary aspect) but are explicitly told their engagement does not discharge the obligation (payer aspect). The performance-only reading creates an extractive structure without a clear extractor—the extraction is structural (gap between command and capacity) rather than agential (someone capturing rents). This is why the gain_flow is absent (see below): the extraction has no seat. This differentiates the reading from a snare where an agent captures the gains; here, the gains (religious authority, scholarly prestige, community structure) are distributed across the institutional apparatus but no single seat demonstrates concentrated capture.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (Temple destroyed; how should a commanded people exist when performance is impossible?) is LIVE and CONTESTED. The reading's answer (obligation persists, study is preparation, the gap remains irreducible) competes with other answers in the same tradition. The mandatrophy verdict hinges on whether the gap itself constitutes a mandate-obsolescence (obligation to do X when X is impossible = obsolete mandate) or a mandate-persistence (obligation to do X in X's unfulfulfilled form = live mandate with altered conditions). The performance-only reading AFFIRMS mandate-persistence: the obligation is live; the gap is its live condition. This prevents mandatrophy classification—the reading explicitly refuses to treat the impossibility of performance as dissolving the obligation. However, the theater_ratio drift (0.05 → 0.68) indicates that as centuries pass, the performative function of study (keeping the mitzvah alive, maintaining identity, transmitting tradition) substantially outweighs the preparation function. By the modern period, an observer might say the mandate has mutated: from 'prepare for restored performance' to 'preserve the law as cultural memory.' But under the performance-only reading's own frame, this mutation is theoretically impossible—the preparation mandate is eternal. The gap between the reading's stated frame and the actual drift of theater_ratio creates a mandatrophy candidate: if the preparation function is the mandate's legitimacy, and the theater function now dominates, then the mandate is obsolete even though the institutional apparatus persists. The reading resolves this by denying that theater_ratio rise constitutes mandate-drift—it is theoretically all preparation, even if functionally it looks like archive. This is where the omegas enter: the ambiguity between preparation and archive is irreducible within the reading's own frame.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    preparation_vs_archive_drift,
    'Is the rising theater_ratio (from 0.05 to 0.68 across 1,956 years) evidence that the mandate has drifted from ''preparation for restored performance'' to ''cultural archive and identity preservation''?',
    'Comparative textual analysis of rabbinical justifications for study across historical periods: do early medieval authorities emphasize preparation for restored Temple, while modern authorities emphasize continuity and identity maintenance? Do the explicit rationales for study change over time?',
    'If the mandate has drifted, the performance-only reading''s legitimacy claim (study is preparation) has become archaeologically false while theoretically persisting. The constraint would be reclassified from snare (irreducible gap) to piton (atrophied function, theatrical maintenance). If the mandate has not drifted (all study across all periods is genuinely preparation, and theater_ratio rise reflects only how observers frame the function), the reading''s coherence persists, and the snare classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(preparation_vs_archive_drift, empirical, 'Whether the constraint''s function has mutated from preparation to archive.').

omega_variable(
    reading_coexistence_or_mutual_exclusion,
    'Are the performance-only reading and the study_as_exercise reading genuinely coexisting as live positions in the same tradition, or does the performance-only reading''s institutional dominance structurally foreclose the study_as_exercise reading within the boundaries of Orthodox halakhic authority?',
    'Institutional documentation: can Orthodox communities teach both readings as live options, or does the authority structure require students to learn the performance-only reading as correct and the study_as_exercise reading as a minority/rejected position?',
    'If the readings genuinely coexist (both live options), the reading_relations value is ''coexists_with.'' If the performance-only reading institutionally forecloses the study_as_exercise reading (declares it heterodox, removes it from the curriculum as incorrect), the relation should be ''forecloses.'' This affects how the engine models the constraint family: foreclosure suggests the performance-only reading is defining the halakhic boundary; coexistence suggests the boundary is internally contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coexistence_or_mutual_exclusion, empirical, 'Whether the performance-only reading forecloses the study-as-exercise reading institutionally.').

omega_variable(
    messianically_dependent_obligation,
    'Is the obligation to perform sacrifice meaningfully binding on the Jewish people if its discharge is made impossible by historical circumstance and depends on a messianic restoration that may or may not occur?',
    'Theological analysis: does Jewish jurisprudence distinguish between obligations that bind regardless of performability (e.g., honoring parents when parents are deceased) and obligations whose bindingness depends on the possibility of performance? Is sacrifice unique in this regard?',
    'If the obligation''s bindingness is independent of performability (as the performance-only reading holds), the extraction is irreducible and the snare classification is correct. If bindingness depends on performability, the obligation should have been released or transformed after the Temple''s destruction, and the performance-only reading''s persistence becomes a matter of choice by the authority structure rather than structural necessity. This would lower the extractiveness estimate and potentially reclassify to piton (institutional inertia rather than structural binding).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianically_dependent_obligation, conceptual, 'Whether an obligation binds when its performance is impossible.').

omega_variable(
    alternative_reading_accessibility,
    'For a contemporary Jewish student, how difficult is it to access and adopt the study_as_exercise or symbolic_archive readings as against the performance-only reading?',
    'Ethnographic investigation: survey students in Orthodox yeshivas; document which readings they encounter, which readings are presented as binding vs. minority vs. rejected, and which reading students actually adopt as their operative frame.',
    'High accessibility to alternative readings would lower accessibility_collapse from 0.78 toward 0.5–0.6 (alternatives are not suppressed); low accessibility (alternatives are institutionally marginalized or omitted from standard curricula) would support the high accessibility_collapse value. This is evidence for whether the suppression is structural (enforcement of the reading against perceived alternatives) or internalized (the alternative readings are genuinely difficult to access and adopt).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_reading_accessibility, empirical, 'Institutional accessibility of alternative readings vs. performance-only reading.').

omega_variable(
    kernel_identity_across_readings,
    'Is the ''sacrifice obligation kernel'' the same kernel across all four readings, or do the readings constitute four different kernels with overlapping language?',
    'Structural comparison: if the performance-only reading and the symbolic_archive reading assign such different truth conditions to ''sacrifice obligation'' (one: a binding law; the other: a historical document) that they refer to different commitments, then the kernel has fragmented. If they are reinterpretations of a single commitment (the Torah''s command to sacrifice), the kernel is one.',
    'If the kernel is single, the constraint family approach is correct: four readings of one kernel. If the kernel has fragmented, there are not four readings but four separate commitments, and the network_affects_constraints links may overstate the relationship. This affects how the corpus models the constraint: as a family of readings or as separate constraints that happen to share vocabulary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_identity_across_readings, conceptual, 'Whether all four readings interpret a single kernel or constitute separate kernels.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__performance_only_reading, 70, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t70, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 70, 0.05).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 500, 0.15).
narrative_ontology:measurement(sacr_tr_t1100, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1100, 0.35).
narrative_ontology:measurement(sacr_tr_t1700, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1700, 0.55).
narrative_ontology:measurement(sacr_tr_t1900, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1900, 0.65).
narrative_ontology:measurement(sacr_tr_t2026, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 2026, 0.68).

% Extraction over time
narrative_ontology:measurement(sacr_be_t70, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 70, 0.85).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 500, 0.88).
narrative_ontology:measurement(sacr_be_t1100, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1100, 0.9).
narrative_ontology:measurement(sacr_be_t1700, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1700, 0.91).
narrative_ontology:measurement(sacr_be_t1900, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1900, 0.92).
narrative_ontology:measurement(sacr_be_t2026, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 2026, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t70, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 70, 0.35).
narrative_ontology:measurement(sacr_su_t500, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 500, 0.38).
narrative_ontology:measurement(sacr_su_t1100, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1100, 0.42).
narrative_ontology:measurement(sacr_su_t1700, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1700, 0.44).
narrative_ontology:measurement(sacr_su_t1900, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1900, 0.45).
narrative_ontology:measurement(sacr_su_t2026, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 2026, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__performance_only_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_kernel__performance_only_reading, 0.12).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel__study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel__messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel__symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% This constraint instantiates the performance-only reading of the sacrifice_obligation_kernel. The kernel (binding commandment to perform sacrifice) is interpreted here as requiring physical performance; study is preparatory but does not discharge the mitzvah. Alternative readings (study_as_exercise, messianic_suspension, symbolic_archive) assign substantially different ε values to the same kernel because they change what counts as legitimate engagement with the command. Each reading is a separate constraint story with its own beneficiary/victim structure and persistence mechanism. The network linkage documents the constraint family decomposition (DP-001, ε-invariance): the readings are linked because they reinterpret a single kernel, not because they coexist as layers within a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_obligation_kernel__performance_only_reading, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
