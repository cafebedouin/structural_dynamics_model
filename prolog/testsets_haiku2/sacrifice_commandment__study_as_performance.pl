% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__study_as_performance, []).

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
 *   constraint_id: sacrifice_commandment__study_as_performance
 *   human_readable: Sacrifice Commandment Fulfillment through Textual Study
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   In post-Temple Rabbinic Judaism, the commandments to offer sacrifices can
 *   no longer be performed in their original physical form. One established
 *   reading holds that study of the sacrificial laws constitutes the exercise
 *   of the commandment itself: intellectual engagement with the legal texts
 *   is held to be a form of worship that fulfills the obligation. This
 *   reading instantiates a constraint with zero extractiveness—the scholar
 *   who studies sacrificial law experiences intrinsic benefit (the
 *   performance of worship through learning), no victim set exists (no one is
 *   targeted for extraction), and the arrangement is presented as emerging
 *   from the logic of how divine commandments operate rather than from human
 *   construction. However, this reading is contested: alternative readings
 *   argue that (a) study preserves knowledge for messianic Temple restoration
 *   without constituting present-day worship (archive_maintenance), or (b)
 *   the commandment remains suspended without the Temple's existence and
 *   cannot be fulfilled by study alone (performance_only). The
 *   zero-extractiveness claim rests on treating the reading as a discovery of
 *   natural law; the omegas flag the possibility that it is a constructed
 *   reading whose adoption benefits scholarly identity and textual
 *   communities.
 *
 * KEY AGENTS:
 *   - halakhic_scholar: the agent whose practice of study is framed as the commandment's fulfillment; benefits intrinsically through this reading.
 *   - divine_authority: the non-agent grounding (the commandment's source and intention); treated as observer-position for analytical clarity.
 *   - temple_restoration_awaiter: noted to indicate whose voice is excluded or alternative in this reading's logic.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__study_as_performance, 0.0).
domain_priors:suppression_score(sacrifice_commandment__study_as_performance, 0.0).
domain_priors:theater_ratio(sacrifice_commandment__study_as_performance, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, extractiveness, 0.0).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__study_as_performance, mountain).
narrative_ontology:human_readable(sacrifice_commandment__study_as_performance, "Sacrifice Commandment Fulfillment through Textual Study").
narrative_ontology:topic_domain(sacrifice_commandment__study_as_performance, "religious/halakhic").

domain_priors:emerges_naturally(sacrifice_commandment__study_as_performance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__study_as_performance, 'c7d73581-d539-4c1d-9956-2f3ed5aa0143').
narrative_ontology:cs_kernel_codification('c7d73581-d539-4c1d-9956-2f3ed5aa0143', fixed_text).
narrative_ontology:cs_authority_grounding('c7d73581-d539-4c1d-9956-2f3ed5aa0143', lineage).
narrative_ontology:cs_interpretation_layer_present('c7d73581-d539-4c1d-9956-2f3ed5aa0143').
narrative_ontology:cs_reading_relation('c7d73581-d539-4c1d-9956-2f3ed5aa0143', sacrifice_commandment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('c7d73581-d539-4c1d-9956-2f3ed5aa0143', sacrifice_commandment__archive_maintenance, coexists_with).
narrative_ontology:cs_axiom('c7d73581-d539-4c1d-9956-2f3ed5aa0143', foundational, intellectual_engagement_fulfills_mitzvot).
narrative_ontology:cs_axiom_status(intellectual_engagement_fulfills_mitzvot, holdable).
narrative_ontology:cs_axiom_grounding('c7d73581-d539-4c1d-9956-2f3ed5aa0143', intellectual_engagement_fulfills_mitzvot, deontological).
narrative_ontology:cs_axiom('c7d73581-d539-4c1d-9956-2f3ed5aa0143', foundational, study_as_intrinsic_worship).
narrative_ontology:cs_axiom_status(study_as_intrinsic_worship, holdable).
narrative_ontology:cs_axiom_grounding('c7d73581-d539-4c1d-9956-2f3ed5aa0143', study_as_intrinsic_worship, theological).
narrative_ontology:cs_reference_frame('c7d73581-d539-4c1d-9956-2f3ed5aa0143', intellectual_engagement_as_worship).
narrative_ontology:cs_drift_state('c7d73581-d539-4c1d-9956-2f3ed5aa0143', contemporary_halakhic_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c7d73581-d539-4c1d-9956-2f3ed5aa0143', '2026-06-12T14:23:45Z').
narrative_ontology:cs_kernel_id(sacrifice_commandment__study_as_performance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__study_as_performance, halakhic_scholar).
narrative_ontology:constraint_vindicates(sacrifice_commandment__study_as_performance, torah_study_as_worship).
narrative_ontology:constraint_vindicates(sacrifice_commandment__study_as_performance, intellectual_engagement_fulfills_mitzvot).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engages in intensive study of the sacrificial commandments and their legal requirements. In this reading, the study itself constitutes the performance of the commandment; intellectual mastery of the sacrificial system IS the fulfillment of the divine obligation. The scholar benefits intrinsically through this act of worship, which is held to be equivalent to, or a legitimate substitute for, physical performance.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, halakhic_scholar, beneficiary,
    moderate, civilizational, mobile, universal).

% The grounding of the commandment itself. The reading asserts that the divine intention embedded in the command is satisfied by intellectual engagement with the commanded act.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, divine_authority, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(sacrifice_commandment__study_as_performance, divine_authority).

% One who expects the physical Temple to be rebuilt and who might hold a different reading (performance_only or archive_maintenance) of how the commandment obligates them in the interim. Present here only to note their absence from the active structure of this reading's logic.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, temple_restoration_awaiter, observer,
    moderate, civilizational, mobile, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains and perpetuates mastery of sacrificial law within a scholarly community; coordinates intergenerational transmission of a complete legal tradition.
% TRANSFER_FUNCTION: No transfer function. The constraint moves nothing from one agent to another; it describes the condition under which intellectual engagement itself becomes an act of worship.
% ABSENT_VOICES: Those who hold performance_only readings of the same kernel (arguing the commandment cannot be fulfilled without the Temple's physical existence) are excluded from the logic of this reading. Also absent: practical communities whose oral traditions emphasize physical restoration over textual study.
% DISAPPEARANCE_RATIONALE: If this reading of how the commandment obligates one ceased to be held, the world's material arrangements would not shift—no physical infrastructure, no institution, no enforcement mechanism, no resource flow depends on it. The constraint describes an internal state (the validity of study as worship), not an external structure. The disappearance would be theological/philosophical, not institutional.
% FOUNDING_PROBLEM: How does one fulfill the commandment to offer sacrifices in a post-Temple world when the central institution (the Temple) no longer exists?
% FOUNDING_PROBLEM_CORROBORATION: Medieval and early modern halakhic authorities (Maimonides, Rabbenu Asher, and subsequent decisors) explicitly address this problem and develop the principle that study of the sacrificial laws constitutes a legitimate fulfillment of the commandment. Contemporary Jewish philosophers and scholars in the academic study of halakhah (Isadore Twersky, David Halivni, and others outside the communities most committed to any single reading) document this as a live interpretive tradition, not a settled consensus.
narrative_ontology:disappearance_verdict(sacrifice_commandment__study_as_performance, world_unchanged).
narrative_ontology:founding_problem_status(sacrifice_commandment__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__study_as_performance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_commandment__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__study_as_performance, 0.0, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__study_as_performance_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, ExtMetricName, E),
    domain_priors:suppression_score(sacrifice_commandment__study_as_performance, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(sacrifice_commandment__study_as_performance),
    narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(sacrifice_commandment__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is authored with extractiveness=0.0 and suppression=0.0 because the reading asserts an intrinsic identity between study and worship: no transfer of resources occurs, no coercion enforces participation, no external barriers prevent exit. The scholar who engages in sacrificial study does so as an internal act of worship, framed as fulfilling a divine obligation. The accessibility_collapse is high (0.95) because the reading claims that alternatives to this interpretation are logically or textually foreclosed within the halakhic tradition itself—once the principle that intellectual engagement fulfills mitzvot is accepted, the necessity of study becomes evident. Resistance is minimal (0.05) because, within the communities that endorse this reading, it is treated as a discovered truth rather than a contested position, so resistance takes the form of holding alternative readings rather than active opposition. The measured claim/metric alignment is deliberate: the reading claims mountain status (emerges naturally from the logic of commandments and their fulfillment) while the omegas document irreducible uncertainties about whether the reading is discovered truth or constructed benefit.
 *
 * PERSPECTIVAL GAP:
 *   No significant perspectival gap exists in this reading because there is no structural asymmetry between parties. The scholar-worshipper is the sole relevant agent, and they are framed as a beneficiary of the arrangement (worship through study). Alternative readings (performance_only, archive_maintenance) would produce different perspectival gaps from different seats—but those are separate constraints instantiating different readings of the same kernel. This reading models a constraint where all seated parties (here, just the scholar and the divine grounding) are aligned on the constraint's meaning and function.
 *
 * DIRECTIONALITY LOGIC:
 *   The halakhic_scholar is the sole beneficiary in the base_properties. The reading asserts that this agent benefits intrinsically through the act of study itself, which is held to be worship. No victim class exists—no agent bears a cost to enable the scholar's benefit. The scholar's directionality is therefore at the beneficiary end (d ≈ 0.0), with mobile exit options (the scholar can cease studying and the constraint no longer applies to them). The divine_authority stakeholder is listed as observer (not agent) because the constraint describes how a pre-given commandment obligates; the divine source is not a party to an arrangement but the grounding of the constraint itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—how to fulfill sacrifice commandments post-Temple—is framed as live and unresolved. The reading answers it by redefining fulfillment from physical to intellectual. No mandatrophy is present: the reading's coordinate claim (study IS the exercise of the commandment) is presented as still-operative doctrine, endorsed by halakhic authorities across centuries. The constraint is not an attenuated remnant of a function; rather, it instantiates an ongoing theological commitment. However, the omega on beneficiary_identity_fusion flags a deeper ambiguity: if the reading's adoption is bound to scholars' identity and authority, the constraint might performatively serve to validate scholarly practice regardless of its truth-value—a form of inertial theater dressed as theological discovery.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_interpretation,
    'Is the claim that study fulfills the commandment a natural law (an intrinsic property of how divine commandments operate independent of human consensus) or a constructed reading that benefits scholars who practice textual engagement?',
    'Cross-reading comparison: identify whether all three sibling readings of the sacrifice_commandment kernel claim to discover a natural fact about how commandments obligate, or whether they are explicitly authored as competing interpretations where only one can be doctrine. If the kernel itself is framed as a site of genuine dispute between equally-valid readings, the ''natural law'' framing of this reading becomes contestable.',
    'If the reading is shown to be a constructed choice among alternatives (all benefiting different communities differently), the extracted τ_eff should include an identity-lock component: the scholar benefits intrinsically from the reading because the reading constitutes their practice as worship. The constraint would remain zero-extractive but would shift from mountain to rope: genuine coordination around textual transmission, not a natural fact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_interpretation, conceptual, 'Whether study-as-worship is a natural property of how divine commandments operate or a constructed reading specific to textual learning communities.').

omega_variable(
    beneficiary_identity_fusion,
    'The halakhic scholar is listed as a beneficiary. Is the scholar''s interest in validating study-as-worship independent of their identity as a scholar, or is their benefit inseparable from their scholarly identity?',
    'Empirical: track whether scholars who cease to engage in halakhic study experience a shift in how they understand the commandment''s applicability to them. Conceptual: examine whether the reading''s endorsement by scholarly authorities constitutes evidence of its truth or evidence of its fitness to the interests of those authorities.',
    'If the benefit is fused to scholarly identity, the beneficiary cannot exit without identity dissolution—a sign of identity_locked constraint rather than a freely-chosen arrangement. This would not change the zero-extractiveness metric but would flag a constraint structure where perceived voluntary participation masks identity capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identity_fusion, empirical, 'Whether the scholar''s benefit is intrinsic to the reading or bound to their scholarly identity.').

omega_variable(
    sibling_reading_foreclosure,
    'Do the three readings (study_as_performance, performance_only, archive_maintenance) genuinely coexist as live positions within halakhic tradition, or does adoption of one reading logically foreclose the others within a unified framework?',
    'Textual analysis: survey whether post-Talmudic authorities endorse multiple readings simultaneously, treat them as genuinely contestable alternatives, or present one as discovered truth. Empirical: track whether contemporary communities hold multiple readings or enforce exclusive adoption.',
    'If readings coexist without foreclosure, the relationship is coexists_with. If study_as_performance''s adoption of the principle that intellectual engagement fulfills mitzvot logically rules out the performance_only claim (that commandments require physical enactment), the relationship is forecloses. This determines the cs_structure.reading_relations encoding and flags the kernel contest as either genuine pluralism or zero-sum.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether the three readings are genuinely coexistent or logically foreclosing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__study_as_performance, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__study_as_performance, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(sacr_be_t20, sacrifice_commandment__study_as_performance, base_extractiveness, 20, 0.0).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_commandment__study_as_performance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__study_as_performance, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_commandment__study_as_performance, 0.0).
narrative_ontology:affects_constraint(sacrifice_commandment__study_as_performance, sacrifice_commandment__performance_only).
narrative_ontology:affects_constraint(sacrifice_commandment__study_as_performance, sacrifice_commandment__archive_maintenance).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested sacrifice_commandment kernel. The kernel describes how post-Temple Jewish communities understand their obligation to fulfill the sacrificial commandments. Three structurally distinct constraints instantiate three readings: (1) study_as_performance asserts study fulfills the obligation (zero extractiveness, mountain type); (2) performance_only asserts the obligation remains suspended without the Temple (creates a snare-adjacent framing where the commandment obligates but cannot be fulfilled); (3) archive_maintenance asserts study preserves knowledge for restoration, not present worship (tangled rope: coordinates tradition preservation while extracting scholarly authority). Each reading has its own ε, beneficiary/victim structure, and type classification. They are linked here as a constraint family sharing the same kernel and alternative readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
