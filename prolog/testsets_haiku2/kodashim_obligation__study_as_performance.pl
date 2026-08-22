% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_performance, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: kodashim_obligation__study_as_performance
 *   human_readable: Kodashim Study as Cosmic Performance (Study-as-Performance Reading)
 *   domain: religious_law/jewish_textual_tradition
 *
 * SUMMARY:
 *   This constraint documents the study-as-performance reading of the
 *   kodashim obligation—the interpretive claim that studying sacrificial law
 *   enacts the cosmic function of sacrifice itself, independent of the
 *   Temple's physical reconstruction. Under this reading, textual engagement
 *   is not preparation for future sacrifice, not archival preservation of a
 *   defunct system, but the actual performance of the obligation. The reading
 *   binds practitioners' identity to continuous textual engagement and
 *   declares the absence of physical Temple irrelevant to spiritual efficacy.
 *   This is one reading of a contested kernel (kodashim_obligation); other
 *   readings frame study as archive or as preparation.
 *
 * KEY AGENTS:
 *   - Jewish interpretive community — bound by identity to the obligation; performs study as cosmic function
 *   - Halakhic authority structure — codifies and transmits the reading; maintains interpretive lineage
 *   - Cosmic order — non-agent beneficiary; the abstract principle claimed to be maintained through study
 *   - Alternative-reading adherents (study-as-archive, study-as-preparation) — excluded from this reading's conversation
 *   - Analytical observer — examines the constraint's structure and competing framings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_performance, 0.0).
domain_priors:suppression_score(kodashim_obligation__study_as_performance, 0.0).
domain_priors:theater_ratio(kodashim_obligation__study_as_performance, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, extractiveness, 0.0).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_performance, mountain).
narrative_ontology:human_readable(kodashim_obligation__study_as_performance, "Kodashim Study as Cosmic Performance (Study-as-Performance Reading)").
narrative_ontology:topic_domain(kodashim_obligation__study_as_performance, "religious_law/jewish_textual_tradition").

domain_priors:emerges_naturally(kodashim_obligation__study_as_performance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_performance, 'be3c85bf-a6c5-48d8-af10-7dea5ff2388b').
narrative_ontology:cs_kernel_codification('be3c85bf-a6c5-48d8-af10-7dea5ff2388b', fixed_text).
narrative_ontology:cs_authority_grounding('be3c85bf-a6c5-48d8-af10-7dea5ff2388b', lineage).
narrative_ontology:cs_interpretation_layer_present('be3c85bf-a6c5-48d8-af10-7dea5ff2388b').
narrative_ontology:cs_reading_relation('be3c85bf-a6c5-48d8-af10-7dea5ff2388b', kodashim_obligation__study_as_archive, coexists_with).
narrative_ontology:cs_reading_relation('be3c85bf-a6c5-48d8-af10-7dea5ff2388b', kodashim_obligation__study_as_preparation, coexists_with).
narrative_ontology:cs_axiom('be3c85bf-a6c5-48d8-af10-7dea5ff2388b', foundational, textual_study_enacts_sacrifice).
narrative_ontology:cs_axiom_status(textual_study_enacts_sacrifice, holdable).
narrative_ontology:cs_axiom_grounding('be3c85bf-a6c5-48d8-af10-7dea5ff2388b', textual_study_enacts_sacrifice, theological).
narrative_ontology:cs_axiom('be3c85bf-a6c5-48d8-af10-7dea5ff2388b', foundational, cosmic_efficacy_independent_of_physical_temple).
narrative_ontology:cs_axiom_status(cosmic_efficacy_independent_of_physical_temple, holdable).
narrative_ontology:cs_axiom_grounding('be3c85bf-a6c5-48d8-af10-7dea5ff2388b', cosmic_efficacy_independent_of_physical_temple, theological).
narrative_ontology:cs_reference_frame('be3c85bf-a6c5-48d8-af10-7dea5ff2388b', study_as_substitute_performance_post_temple).
narrative_ontology:cs_drift_state('be3c85bf-a6c5-48d8-af10-7dea5ff2388b', contemporary_secular_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('be3c85bf-a6c5-48d8-af10-7dea5ff2388b', '2026-06-11T14:32:18Z').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_performance, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_performance, cosmic_order).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_performance, jewish_interpretive_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fulfills the legal obligation to study sacrificial law (kodashim); through this study, maintains participation in a cosmic maintenance function regardless of Temple status. The reading binds the practitioner's identity to textual engagement; exit would require abandoning a core identity claim.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, jewish_interpretive_community, beneficiary,
    organized, civilizational, identity_locked, global).

% Codifies and transmits the ruling that study enacts sacrifice; teaches the reading through yeshiva curriculum, rabbinic interpretation, and liturgical practice. Maintains the interpretive tradition grounding this reading's legitimacy.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, halakhic_authority_structure, agenda_setter,
    institutional, civilizational, analytical, global).

% Non-agent entity: the abstract principle of cosmic equilibrium and divine order that, under this reading's framework, is maintained through the performance of sacrificial study. Receives no rent, but is the referent of the obligation's purpose.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, cosmic_order, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(kodashim_obligation__study_as_performance, cosmic_order).

% Those holding the study-as-archive or study-as-preparation readings are not in the conversation that authorizes this reading's claim. They would contest the efficacy claim and argue for other framings of what sacrificial study is.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, alternative_reading_adherents, excluded,
    organized, generational, mobile, global).

% Examines the constraint's structure, its relationship to competing readings, and the conditions under which the study-as-performance reading holds authority within the halakhic system.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, analytical_observer, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes textual study of sacrificial law as a continuous obligation for the Jewish interpretive community, securing participation in a claimed cosmic function without requiring physical Temple, sacrificial animals, or priestly apparatus. Solves the problem of how to discharge an obligation when the structural conditions for its literal performance do not exist.
% TRANSFER_FUNCTION: Moves nothing between parties; there is no extraction. The arrangement transfers spiritual efficacy from the Temple (which is absent) to the act of study itself (which is continuous and available). The beneficiary is declared to be cosmic order, not any human agent.
% ABSENT_VOICES: Adherents of the study-as-archive and study-as-preparation readings are structurally excluded from this particular reading's conversation. They would argue: (archive reading) study is preservation of a defunct practice, not living obligation; (preparation reading) study is temporary substitute, not the actual performance. Non-halakhic academic readers would add that the claim of cosmic efficacy cannot be empirically adjudicated and rests entirely on tradition.
% DISAPPEARANCE_RATIONALE: If this reading (and its obligation) disappeared, the physical Temple would remain absent and the cosmic function would (under this reading's own logic) go un-performed. Under alternative readings, the disappearance would shift understanding of what study is for, but would not materially rearrange the world—study would continue, framed differently. The obligation's absence would not alter text, liturgy, or the conditions of Jewish practice.
% FOUNDING_PROBLEM: After the Temple's destruction, how does the obligation to study and maintain sacrificial law continue to constitute a binding practice rather than historical preservation? This reading answers: the study itself IS the performance; efficacy is spiritual, not material; the absence of the Temple is irrelevant to the binding nature of the obligation.
% FOUNDING_PROBLEM_CORROBORATION: This reading is attested by major strands of rabbinic halakhic tradition (Babylonian Talmud, medieval commentaries, contemporary halakhic authorities who teach this reading as binding). It is also endorsed by liturgical practice (yigdal/kedushah formulations that affirm study as sacrifice). HOWEVER: this reading competes with study-as-archive (affirmed by academic Jewish studies scholars) and study-as-preparation (affirmed by messianic-oriented traditionalists). The founding problem itself is contested: whether the problem is 'how to maintain binding obligation post-Temple' (this reading's frame) or 'how to preserve memory of a practice' (archive frame) or 'how to maintain readiness for restoration' (preparation frame) is not settled by any voice outside the framework.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_performance, world_unchanged).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_performance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_obligation__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_performance, 0.0, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_performance_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, ExtMetricName, E),
    domain_priors:suppression_score(kodashim_obligation__study_as_performance, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(kodashim_obligation__study_as_performance),
    narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(kodashim_obligation__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This reading authorizes ZERO EXTRACTIVENESS because the declared beneficiary is cosmic order (a non-agent, vindicated proposition), not any human seat. There is no victim set—no party bears costs that others collect; the obligation is universal within the community and the benefit (spiritual efficacy / cosmic maintenance) is not privately captured. SUPPRESSION is zero: the reading maintains itself through authority and identity, not coercion. THEATER_RATIO is zero: the study is claimed to be the performance itself, not performative activity covering a hidden function. ACCESSIBILITY_COLLAPSE is near-maximal (0.95): once the reading's premise (efficacy through study alone, independent of Temple) is accepted, alternatives collapse—the obligation is binding, continuous, and requires no external conditions. RESISTANCE is near-minimal (0.05): resistance would come from those holding the competing readings, but within the halakhic framework where this reading is authorized, resistance is limited to minority positions and not structurally mounted. The constraint emerges naturally from the halakhic tradition's engagement with the post-Temple problem; it is not constructed by any party for extraction. The claim/metric alignment is intentional: this reading claims to be a mountain (natural law of spiritual efficacy) and the authored metrics reflect zero extraction and high accessibility collapse.
 *
 * PERSPECTIVAL GAP:
 *   All seats within this reading's framework compute identically: the obligation is binding, study is the performance, extraction is zero, beneficiary is cosmic order. The perspectival gap is NOT within this reading but BETWEEN this reading and its siblings (study-as-archive, study-as-preparation). A party holding the archive reading would compute DIFFERENT extractiveness (low but non-zero: the study extracts historical identity-maintenance) and DIFFERENT accessibility (lower: alternatives exist—one could stop studying, study differently, or frame study as optional). The engine computes per-seat types from structural data; within this reading's framework all seats are aligned.
 *
 * DIRECTIONALITY LOGIC:
 *   There is no directionality computation on the standard axes because there are no asymmetric extraction flows. The Jewish interpretive community and the halakhic authority structure are not in a payer/beneficiary relationship; both are coordinated toward the same end (maintenance of the obligation). The declared beneficiary is cosmic order—a non-agent, so it does not occupy a seat and does not compute directionality. All human agents in this reading are symmetrically positioned: they are all bound by the same obligation, all derive the same spiritual benefit, all contribute equally to the claimed cosmic function through their study. This is structurally a rope reading (pure coordination) masquerading as a mountain reading (natural law). The divergence is intentional and documented in the omega variables.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's founding problem (post-Temple binding obligation) is LIVE under this reading's own logic—the obligation remains binding because efficacy is spiritual, independent of physical conditions. However, the founding problem is CONTESTED across the three readings of the kernel. Mandatrophy is NOT present in this constraint alone; it emerges in the kernel network (all three readings together). If all three readings are authored, the engine will detect that study-as-performance claims the founding problem is live while study-as-archive claims it is dead (the problem was 'how to preserve memory' and that is solved by archive) and study-as-preparation claims it is live but with a different future (the problem will be RESOLVED when the Temple is restored). The triple contradiction signals contested foundational status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cosmic_efficacy_empirical_status,
    'Can the claimed cosmic efficacy of sacrificial study be empirically adjudicated, or does it rest entirely on tradition and interpretive authority?',
    'This is a conceptual/metaphysical question. Resolution would require a framework for testing spiritual efficacy claims, which the reading itself does not provide and may resist on principle.',
    'If cosmic efficacy is empirically non-adjudicable (likely under this reading''s own logic), the reading''s foundation is axiomatic—held because the tradition authorizes it, not because evidence supports it. This shifts classification from natural law to conventional coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cosmic_efficacy_empirical_status, conceptual, 'Whether cosmic efficacy is empirically verifiable or axiomatic to the reading.').

omega_variable(
    alternative_reading_foreclosure,
    'Does the study-as-performance reading logically FORECLOSE the study-as-archive and study-as-preparation readings, or can all three coexist as live positions held by different interpretive communities?',
    'Examine the logical structure: does affirming ''study IS the performance'' force denial of ''study is preparation'' or ''study is archive''? Historically, all three have coexisted in Jewish tradition; logically, they rest on different metaphysical premises (whether the Temple''s absence is relevant, whether cosmic efficacy is independent of physical conditions).',
    'If the readings coexist (likely), this reading does not foreclose siblings and they should be related by ''coexists_with''. If one reading forecloses others (rare in legal/scriptural interpretation), the network should reflect foreclosure edges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_foreclosure, conceptual, 'Logical relationship between this reading and its kernel siblings.').

omega_variable(
    kernel_vs_reading_extraction,
    'Is the extraction present in maintaining and transmitting this reading itself (the reading as a constraint on interpretive authority) distinct from the zero-extraction reading''s own claim?',
    'Distinguish the reading (study-as-performance, zero extraction) from the INSTITUTION that authorizes it (halakhic authority structure, which may extract authority-rent). Does the halakhic authority benefit from maintaining this reading''s authority over alternatives?',
    'If the authority structure extracts rent from maintaining this reading (e.g., institutional power over competitors), then the constraint is not zero-extraction at the kernel network level—it is a snare or tangled rope masquerading as a mountain. This would be detected by comparing the study-as-performance constraint (zero extraction, its own metrics) to a separate constraint about halakhic authority (likely extractive).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_vs_reading_extraction, empirical, 'Whether institutional authority extraction is separable from the reading''s claimed zero-extraction.').

omega_variable(
    study_as_performance_identity_lock_reversibility,
    'If an individual rejects the study-as-performance reading and adopts study-as-archive or study-as-preparation, is the identity-lock from this reading reversible, or does it persist as internalized obligation?',
    'Post-exit trajectory: individuals who shift to alternative readings or cease engaged study report on whether the sense of obligation persists internally or dissolves upon explicit rejection of the cosmic efficacy claim.',
    'If identity-lock persists after intellectual rejection of the reading, the constraint carries internalized suppression even in the post-exit state (a hallmark of identity-coordination constraints that actually function as snares). If it dissolves, the constraint is identity-locked but not extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(study_as_performance_identity_lock_reversibility, empirical, 'Reversibility of identity-lock when the reading is rejected.').

omega_variable(
    kernel_reading_decomposition_justification,
    'Why is kodashim_obligation decomposed into three separate constraint stories (study-as-archive, study-as-preparation, study-as-performance) rather than authored as one constraint with multiple readings as an internal parameter?',
    'Per DP-001 (ε-invariance principle): the three readings have DIFFERENT ε values and beneficiary structures. Study-as-archive has LOW-to-moderate extraction (institutional authority maintains memory-preservation function). Study-as-preparation has LOW extraction (coordination around knowledge maintenance for a future condition). Study-as-performance has ZERO extraction (beneficiary is cosmic order, not institutional actor). The ε-invariance test: measuring the constraint under archive framing vs. performance framing yields structurally different ε. Therefore, three constraints.',
    'The three readings are authored as a constraint family linked by network.affects_constraints. Each has its own ε, its own beneficiary/victim structure, its own stakeholder set. The kernel is the stabilized commitment; the readings are the different instantiations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_decomposition_justification, conceptual, 'ε-invariance justification for three-constraint decomposition of one kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_performance, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_performance, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_obligation__study_as_performance, 0.02).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_performance, kodashim_obligation__study_as_archive).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_performance, kodashim_obligation__study_as_preparation).

% DUAL FORMULATION NOTE:
% Three readings of the kodashim_obligation kernel, each with distinct ε and beneficiary structure, authored as separate constraints per DP-001 (ε-invariance principle). Study-as-performance: zero extraction, cosmic order as beneficiary. Study-as-archive: low-moderate extraction from institutional authority maintaining interpretive tradition. Study-as-preparation: low extraction, coordination around knowledge preservation. All three readings coexist in Jewish halakhic history; none logically forecloses the others, but each instantiates a different constraint. The kernel is the commitment; the readings are the constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
