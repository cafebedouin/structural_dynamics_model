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
 *   human_readable: Study-as-Exercise Reading of the Sacrifice Obligation
 *   domain: religious_law/halakhic_authority
 *
 * SUMMARY:
 *   This story generates the study-as-exercise reading of the sacrifice
 *   obligation kernel: the claim that engaged intellectual study of the laws
 *   of korbanot is not merely preparatory to, or a substitute for, but is
 *   itself the genuine occupation of the mitzvah under present Temple-less
 *   conditions. This is one of four structurally distinct readings of a
 *   single kernel (the standing obligation regarding sacrifice) — the others
 *   (performance_only_reading, messianic_suspension_reading,
 *   symbolic_archive_reading) are separate constraint files with their own ε
 *   and beneficiary/victim structure, linked here via
 *   network.affects_constraints per the ε-invariance principle. This reading
 *   is authored as having negligible extraction: no victim set exists because
 *   the reading does not claim anyone is deprived of anything — it authorizes
 *   a transformation of the mode of fulfillment, not an extraction from any
 *   party. The declared beneficiary (rabbinic_interpretive_authority)
 *   reflects that this reading also concentrates interpretive authority over
 *   what 'fulfillment' means, without implying coercive extraction from study
 *   practitioners, who engage voluntarily and retain full exit.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__study_as_exercise_reading, 0.06).
domain_priors:suppression_score(sacrifice_obligation_kernel__study_as_exercise_reading, 0.12).
domain_priors:theater_ratio(sacrifice_obligation_kernel__study_as_exercise_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, extractiveness, 0.06).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__study_as_exercise_reading, rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel__study_as_exercise_reading, "Study-as-Exercise Reading of the Sacrifice Obligation").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__study_as_exercise_reading, "religious_law/halakhic_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__study_as_exercise_reading, '366f3e4d-3d73-47b2-9695-2edcb71e8117').
narrative_ontology:cs_kernel_codification('366f3e4d-3d73-47b2-9695-2edcb71e8117', fixed_text).
narrative_ontology:cs_authority_grounding('366f3e4d-3d73-47b2-9695-2edcb71e8117', lineage).
narrative_ontology:cs_interpretation_layer_present('366f3e4d-3d73-47b2-9695-2edcb71e8117').
narrative_ontology:cs_reading_relation('366f3e4d-3d73-47b2-9695-2edcb71e8117', sacrifice_obligation_kernel__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('366f3e4d-3d73-47b2-9695-2edcb71e8117', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('366f3e4d-3d73-47b2-9695-2edcb71e8117', sacrifice_obligation_kernel__symbolic_archive_reading, forecloses).
narrative_ontology:cs_axiom('366f3e4d-3d73-47b2-9695-2edcb71e8117', foundational, study_constitutes_fulfillment).
narrative_ontology:cs_axiom_status(study_constitutes_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('366f3e4d-3d73-47b2-9695-2edcb71e8117', study_constitutes_fulfillment, conventional).
narrative_ontology:cs_axiom('366f3e4d-3d73-47b2-9695-2edcb71e8117', secondary, torah_study_has_independent_halakhic_force).
narrative_ontology:cs_axiom_status(torah_study_has_independent_halakhic_force, holdable).
narrative_ontology:cs_axiom_grounding('366f3e4d-3d73-47b2-9695-2edcb71e8117', torah_study_has_independent_halakhic_force, conventional).
narrative_ontology:cs_reference_frame('366f3e4d-3d73-47b2-9695-2edcb71e8117', temple_era_sacrificial_performance_standard).
narrative_ontology:cs_drift_state('366f3e4d-3d73-47b2-9695-2edcb71e8117', post_temple_rabbinic_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('366f3e4d-3d73-47b2-9695-2edcb71e8117', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_interpretive_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, torah_study_practitioners).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__study_as_exercise_reading, study_constitutes_fulfillment_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__study_as_exercise_reading, torah_study_as_supreme_value_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates and transmits the doctrine that engaged study of sacrificial law (korbanot) constitutes genuine occupation of the mitzvah under present conditions absent the Temple. This reading establishes the yeshiva and the study hall as the site where the obligation is actively exercised, which anchors the ongoing authority of rabbinic scholarship as the operative form of practice. The interpretive monopoly on what counts as fulfillment sits with this seat.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_interpretive_authority, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_interpretive_authority, beneficiary).

% Engage daily in study of the laws of sacrifice as a devotional and intellectual practice. Under this reading, their study time is not preparatory or merely commemorative but is itself the fulfillment of the commandment, giving their study concrete religious weight and standing independent of Temple access. They can choose to study these tractates or not; nothing coerces the choice, and alternate mitzvot remain available.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, torah_study_practitioners, beneficiary,
    moderate, biographical, mobile, national).

% Communities holding the performance-only, messianic-suspension, or symbolic-archive readings would contest that study alone occupies the obligation, arguing it either falls short of the mitzvah, keeps the obligation properly suspended, or overclaims halakhic force for what is really cultural memory. They are not part of the study-as-exercise reading's internal deliberation; their objection surfaces mainly in comparative religious-legal scholarship and cross-community polemic, not within this reading's own adjudicative process.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, sibling_readings_communities, excluded,
    organized, generational, constrained, global).

% Study the history and internal logic of competing readings of the sacrifice obligation across rabbinic literature, tracing how the study-as-exercise doctrine developed (notably via Talmudic and later halakhic sources elevating limud Torah on korbanot to the level of offering) and how it interacts with the other three readings without holding a stake in the outcome.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, comparative_halakhic_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent account of how a commandment whose physical performance became impossible after the Temple's destruction can still be actively fulfilled, letting religious practice and communal identity continue coherently without requiring either abandonment of the obligation or an indefinite state of unfulfillable duty.
% TRANSFER_FUNCTION: Moves interpretive authority over what counts as fulfilling the mitzvah of sacrifice from priestly/Temple performance to the rabbinic study hall; moves practical religious standing and communal status toward those engaged in Torah study of korbanot, and toward the institutions (yeshivot, batei midrash) that house and legitimate that study.
% ABSENT_VOICES: Adherents of the performance-only reading would object that no amount of study substitutes for the actual physical act commanded in Torah; adherents of the messianic-suspension reading would object that treating study as occupying the mitzvah understates the obligation's continued dormancy pending divine restoration; adherents of the symbolic-archive reading would object that assigning study genuine halakhic force overclaims what is properly cultural-historical preservation. None of these voices participate in this reading's own internal adjudication process — they appear only in comparative scholarship and inter-communal debate.
% DISAPPEARANCE_RATIONALE: If the study-as-exercise doctrine were withdrawn, the study of sacrificial law (Kodashim, korbanot literature) would lose its standing as an act of active mitzvah-fulfillment and would revert to being understood as either preparatory, suspended, or purely commemorative — reshaping how yeshivot allocate curricular emphasis, how communal status is assigned to those devoted to this study, and how the broader question of an 'unfulfillable' commandment is handled theologically.
% FOUNDING_PROBLEM: After the destruction of the Second Temple, the commandment of sacrifice became physically unperformable, creating a theological and practical problem: how can a divine commandment remain binding and meaningfully practiced when its literal performance is impossible?
% FOUNDING_PROBLEM_CORROBORATION: Comparative halakhic scholars and historians of rabbinic Judaism, situated outside the rabbinic authority that benefits from the doctrine, corroborate that the destruction of the Temple created a real and unresolved practical problem (unperformable commandments) that the study-as-exercise doctrine was developed to address; they document its emergence in Talmudic-era sources and its subsequent elaboration, distinguishing it as one of several competing responses rather than treating it as self-evidently the only or correct solution.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__study_as_exercise_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__study_as_exercise_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__study_as_exercise_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__study_as_exercise_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__study_as_exercise_reading, 0.06, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__study_as_exercise_reading_tests).
:- end_tests(sacrifice_obligation_kernel__study_as_exercise_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is scored low (0.06) and stable across the interval because the reading imposes no cost on any party who declines to hold it — it does not compel study, penalize non-adherents, or seize resources; it is a doctrinal reading, not an enforcement apparatus. Suppression is low (0.12): there is no active machinery preventing an individual or community from instead holding one of the sibling readings; the reading persists through sustained transmission and pedagogical reinforcement, not coercion. Theater ratio is low-to-moderate and rises marginally (0.10 to 0.15) reflecting the institutionalized elaboration of dedicated Kodashim study programs, which carry some ceremonial/status-performance dimension (prestige of Kodashim scholarship) layered atop the substantive doctrine, but this remains modest. Accessibility collapse is moderate (0.35), not high: unlike a mountain, real alternative readings (the three siblings) remain fully articulated and practiced by other communities — the reading has not closed off alternatives, it coexists with them. Resistance is low (0.2), reflecting that this reading faces limited internal contestation within its own tradition, though it is contested across communities holding sibling readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic interpretive authority is the structural beneficiary: this reading vests in the rabbinic/scholarly class the power to certify that study constitutes fulfillment, which sustains the authority, prestige, and institutional centrality of the study hall as the site of active mitzvah performance. Torah study practitioners are also beneficiaries — their study acquires elevated religious standing — but hold full mobility (they can choose which tractates to prioritize, or decline this reading for another) so their directionality sits near the beneficiary end without any trapped or extracted position. No victim group exists because no party bears an involuntary cost attributable to this reading's operation; the excluded sibling-reading communities are not victims of this reading, they are simply holders of a different reading who are not consulted within this reading's own internal process.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (an unperformable Temple-era commandment) remains live in the sense that the Temple has not been rebuilt, so the practical question this reading answers has not lapsed — this blocks a naive mandatrophy read where the arrangement persists after its problem dissolved. However, the doctrine's persistence and elaboration independent of any prospect of imminent resolution (i.e., it does not merely bridge to restoration but positively re-defines fulfillment) is itself the contested point among the sibling readings, particularly against messianic_suspension_reading, which holds the obligation should remain suspended rather than redefined. This story does not adjudicate that contest; it authors only the study-as-exercise reading's own internal coherence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_as_fulfillment_vs_preparation_ambiguity,
    'Does engaged study of sacrificial law genuinely occupy/discharge the mitzvah, or does it merely prepare/preserve readiness for a physical performance that remains the actual object of the obligation?',
    'This is a live internal halakhic dispute resolved (to the extent it is resolved at all) by textual analysis of Talmudic sources (e.g., Menachot 110a and its later elaborations), and by which authorities a given community defers to; it is not resolvable by external empirical evidence, only by which reading a community''s rabbinic authorities endorse.',
    'If the study-as-exercise reading is correct, this constraint''s zero-extraction, coordination-only classification holds. If the performance-only reading is instead the historically dominant or halakhically correct one, this reading would be better understood as a legitimating overlay that lets rabbinic authority claim fulfillment-granting power it does not actually possess, which would push the classification toward a false-summit-adjacent structure (a claimed coordination function masking an unearned authority expansion).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_as_fulfillment_vs_preparation_ambiguity, conceptual, 'Whether study genuinely fulfills or only prepares for the sacrifice obligation is an internal doctrinal question, not an empirical one.').

omega_variable(
    interpretive_monopoly_extraction_potential,
    'Does the concentration of authority over ''what counts as fulfillment'' in rabbinic hands, even absent direct coercion, constitute a subtle extractive function (status, resource allocation to yeshivot, communal deference) not captured by the low extractiveness score?',
    'Comparative study of resource and status flows in communities under this reading (e.g., disproportionate communal funding or prestige allocated to Kodashim scholars) versus communities under sibling readings, to see whether the doctrine correlates with measurable resource concentration.',
    'If resource/status concentration is substantial and structurally tied to the doctrine, ε may be under-authored here and the reading may sit closer to a mild tangled_rope (coordination plus a thin extraction layer) than a clean rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpretive_monopoly_extraction_potential, empirical, 'Whether the interpretive-authority beneficiary structure conceals a measurable extraction not captured in the current low ε score.').

omega_variable(
    kernel_framing_choice_omega,
    'Is the correct unit of analysis the single kernel (the standing sacrifice obligation) with four competing readings, or are these readings different enough in their object (fulfillment doctrine vs. suspension doctrine vs. archival doctrine) that they should be treated as answering different questions entirely rather than as readings of one contested kernel?',
    'Textual-historical analysis of whether the four positions engage the same halakhic question (what discharges the sacrifice commandment now) or have quietly redefined the question itself (e.g., symbolic_archive_reading may reject that there is a live halakhic question at all).',
    'If symbolic_archive_reading in particular is answering a different question (a meta-level claim about the genre of the material, not a first-order fulfillment claim), the four-way kernel structure may need to be revised to a three-way kernel plus one adjacent-but-distinct constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice_omega, conceptual, 'Whether all four declared readings genuinely share one kernel or whether the archival reading operates at a different logical level.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__study_as_exercise_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sacr_tr_t20, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(sacr_tr_t40, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 40, 0.13).
narrative_ontology:measurement(sacr_tr_t60, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 60, 0.14).
narrative_ontology:measurement(sacr_tr_t80, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 80, 0.15).
narrative_ontology:measurement(sacr_tr_t100, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(sacr_be_t20, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 20, 0.05).
narrative_ontology:measurement(sacr_be_t40, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 40, 0.06).
narrative_ontology:measurement(sacr_be_t60, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 60, 0.06).
narrative_ontology:measurement(sacr_be_t80, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 80, 0.06).
narrative_ontology:measurement(sacr_be_t100, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 100, 0.06).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_kernel__study_as_exercise_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__study_as_exercise_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_kernel__study_as_exercise_reading, 0.08).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling constraints decomposing the natural-language concept 'the sacrifice obligation after the Temple's destruction' per the ε-invariance principle. Each reading is authored with its own ε, its own beneficiary/victim structure, and its own claimed type: study_as_exercise_reading (this file, near-zero ε, rope) treats study itself as fulfillment; performance_only_reading holds physical offering remains required and would author higher ε reflecting the unresolved/unfulfillable status of the obligation; messianic_suspension_reading treats the obligation as authoritatively suspended pending restoration, with low extraction but a different beneficiary/authority structure oriented toward eschatological expectation; symbolic_archive_reading denies ongoing halakhic force altogether and would author minimal extraction but no rabbinic-fulfillment-authority beneficiary at all. All four are linked via affects_constraints as members of one kernel family; none averages or hedges across the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
