% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__adaptive_gradient_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supermajority_threshold__adaptive_gradient_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: supermajority_threshold__adaptive_gradient_reading
 *   human_readable: Adaptive Supermajority Threshold Calibration
 *   domain: political/institutional
 *
 * SUMMARY:
 *   This constraint story instantiates the adaptive_gradient_reading of the
 *   supermajority_threshold kernel. The reading holds that supermajority
 *   thresholds are not intrinsically legitimate or illegitimate; their
 *   legitimacy depends on continuous calibration to empirically observable
 *   social consensus formation rates and the actual costs of reversing
 *   decisions. A threshold set too low produces instability (excessive
 *   constitutional churn, majority tyranny) — a coordination failure. A
 *   threshold set too high produces ossification (minority veto entrenchment,
 *   inability to correct errors) — an extraction failure. The adaptive
 *   reading claims the threshold is a functional tool requiring
 *   evidence-based tuning, not a sacred number. Over the interval (0-50
 *   years), empirical political science has increasingly measured
 *   consensus-formation dynamics, revealing that many existing thresholds are
 *   poorly calibrated — too high for routine legislation, too low for
 *   fundamental rights changes. This creates the measured extractiveness:
 *   miscalibrated thresholds extract from whichever faction bears the cost of
 *   the miscalibration. The constraint requires active enforcement (judicial
 *   review, amendment procedures) and has identifiable beneficiaries (those
 *   whose agenda matches the current calibration) and victims (those blocked
 *   by miscalibration).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__adaptive_gradient_reading, 0.32).
domain_priors:suppression_score(supermajority_threshold__adaptive_gradient_reading, 0.25).
domain_priors:theater_ratio(supermajority_threshold__adaptive_gradient_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__adaptive_gradient_reading, tangled_rope).
narrative_ontology:human_readable(supermajority_threshold__adaptive_gradient_reading, "Adaptive Supermajority Threshold Calibration").
narrative_ontology:topic_domain(supermajority_threshold__adaptive_gradient_reading, "political/institutional").

domain_priors:requires_active_enforcement(supermajority_threshold__adaptive_gradient_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__adaptive_gradient_reading, '4b4bbff9-e00e-4c0a-a4ef-a066bdaec92d').
narrative_ontology:cs_kernel_codification('4b4bbff9-e00e-4c0a-a4ef-a066bdaec92d', formalized).
narrative_ontology:cs_authority_grounding('4b4bbff9-e00e-4c0a-a4ef-a066bdaec92d', lineage).
narrative_ontology:cs_interpretation_layer_present('4b4bbff9-e00e-4c0a-a4ef-a066bdaec92d').
narrative_ontology:cs_reading_relation('4b4bbff9-e00e-4c0a-a4ef-a066bdaec92d', supermajority_threshold__consensus_safeguard_reading, influences).
narrative_ontology:cs_reading_relation('4b4bbff9-e00e-4c0a-a4ef-a066bdaec92d', supermajority_threshold__minoritarian_veto_reading, forecloses).
narrative_ontology:cs_axiom('4b4bbff9-e00e-4c0a-a4ef-a066bdaec92d', foundational, threshold_legitimacy_from_calibration_performance).
narrative_ontology:cs_axiom_status(threshold_legitimacy_from_calibration_performance, holdable).
narrative_ontology:cs_axiom_grounding('4b4bbff9-e00e-4c0a-a4ef-a066bdaec92d', threshold_legitimacy_from_calibration_performance, empirically_contingent).
narrative_ontology:cs_axiom('4b4bbff9-e00e-4c0a-a4ef-a066bdaec92d', foundational, consensus_formation_rates_are_measurable).
narrative_ontology:cs_axiom_status(consensus_formation_rates_are_measurable, holdable).
narrative_ontology:cs_axiom_grounding('4b4bbff9-e00e-4c0a-a4ef-a066bdaec92d', consensus_formation_rates_are_measurable, empirically_contingent).
narrative_ontology:cs_axiom('4b4bbff9-e00e-4c0a-a4ef-a066bdaec92d', secondary, reversibility_cost_asymmetry_undermines_legitimacy).
narrative_ontology:cs_axiom_status(reversibility_cost_asymmetry_undermines_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('4b4bbff9-e00e-4c0a-a4ef-a066bdaec92d', reversibility_cost_asymmetry_undermines_legitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('4b4bbff9-e00e-4c0a-a4ef-a066bdaec92d', fixed_threshold_constitutionalism).
narrative_ontology:cs_drift_state('4b4bbff9-e00e-4c0a-a4ef-a066bdaec92d', contemporary_empirical_constitutional_design, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4b4bbff9-e00e-4c0a-a4ef-a066bdaec92d', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__adaptive_gradient_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__adaptive_gradient_reading, constitutional_designers).
narrative_ontology:constraint_beneficiary(supermajority_threshold__adaptive_gradient_reading, majority_coalitions).
narrative_ontology:constraint_beneficiary(supermajority_threshold__adaptive_gradient_reading, reform_advocates).
narrative_ontology:constraint_victim(supermajority_threshold__adaptive_gradient_reading, minority_factions).
narrative_ontology:constraint_victim(supermajority_threshold__adaptive_gradient_reading, status_quo_defenders).
narrative_ontology:constraint_victim(supermajority_threshold__adaptive_gradient_reading, institutional_traditionalists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(supermajority_threshold__adaptive_gradient_reading, majority_coalitions).
narrative_ontology:constraint_vindicates(supermajority_threshold__adaptive_gradient_reading, evidence_based_institutional_design).
narrative_ontology:constraint_vindicates(supermajority_threshold__adaptive_gradient_reading, adaptive_governance_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and periodically review supermajority thresholds for constitutional amendments, legislative overrides, and judicial appointments. They claim to calibrate thresholds based on political science evidence but face pressure from incumbent factions to freeze thresholds at advantageous levels. Their exit is constrained by the constitution they are designing — they cannot easily leave the system they administer.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, constitutional_designers, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(supermajority_threshold__adaptive_gradient_reading, constitutional_designers, beneficiary).

% Hold legislative or electoral majorities and seek to enact their agenda. When thresholds are calibrated to actual consensus rates, they benefit from stable decision-making without excessive obstruction. When thresholds are too high, they pay the cost of blocked reforms; when too low, they risk their own decisions being reversed by transient majorities. Exit is constrained by the political system — they cannot easily change the threshold without the minority's cooperation.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, majority_coalitions, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(supermajority_threshold__adaptive_gradient_reading, majority_coalitions, payer).

% Push for constitutional or structural changes that require supermajority approval. They benefit when thresholds are calibrated to the actual consensus needed for their specific reform — not so high that reform is impossible, not so low that reforms are unstable. Their exit is constrained: they work within the system to change the system, and leaving the jurisdiction is rarely feasible for the populations they represent.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, reform_advocates, beneficiary,
    moderate, biographical, constrained, national).

% Hold policy preferences that diverge from the majority but lack the numbers to meet supermajority thresholds. When thresholds are set above actual consensus-formation rates for their issues, they bear pure extraction costs — their preferences are blocked without compensation. Their exit is trapped: they cannot leave the polity, and the threshold mechanism itself prevents them from achieving change through institutional channels.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, minority_factions, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(supermajority_threshold__adaptive_gradient_reading, minority_factions, excluded).

% Benefit from existing arrangements and use supermajority thresholds to block changes. They pay when thresholds are calibrated lower than their blocking preference — their veto power is reduced. Their exit is constrained: they are institutional insiders who cannot easily leave the system, and their power depends on the threshold remaining high.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, status_quo_defenders, payer,
    organized, biographical, constrained, national).

% Defend existing threshold levels as intrinsically legitimate, resisting evidence-based recalibration. They pay when adaptive calibration would lower thresholds for issues where they currently hold veto power. Their exit is constrained by professional and ideological commitment to the existing constitutional order.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, institutional_traditionalists, payer,
    moderate, generational, constrained, national).

% Study consensus-formation rates, amendment success frequencies, and threshold effects across jurisdictions. They provide the evidence base for calibration but have no direct stake in any particular threshold level. Their exit is analytical — they observe from outside the constraint's direct operation.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, political_scientists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents constitutional instability from transient majorities while enabling legitimate change when deep consensus exists. Solves the coordination problem of distinguishing ephemeral preference shifts from persistent collective will without freezing the constitution.
% TRANSFER_FUNCTION: Moves decision-making authority from simple majorities to supermajority coalitions. When thresholds are well-calibrated, this transfers power to the actual consensus coalition. When miscalibrated high, it transfers veto power to minorities who do not represent consensus. When miscalibrated low, it transfers reversal power to transient majorities.
% ABSENT_VOICES: Future generations who will inherit the constitutional structure but cannot participate in current calibration debates. Citizens in jurisdictions with rigid thresholds who cannot advocate for recalibration because the amendment procedure itself requires the supermajority being contested. The excluded stakeholder (minority_factions with secondary_role excluded) captures part of this absence.
% DISAPPEARANCE_RATIONALE: If supermajority thresholds vanished overnight, constitutional amendment would revert to simple majority or consensus rules. This would cause immediate rearrangement: some jurisdictions would experience constitutional churn (instability), others would enable long-blocked reforms (majority empowerment), and the calibration problem would shift from threshold-setting to alternative stabilization mechanisms.
% FOUNDING_PROBLEM: Early constitutional systems faced a dilemma: simple majority amendment produced instability and majority tyranny; unanimous consent produced paralysis. The supermajority threshold was invented as a middle ground — high enough to prevent transient majorities from rewriting fundamentals, low enough to allow legitimate change.
% FOUNDING_PROBLEM_CORROBORATION: Political scientists (e.g., Lutz 1994 on amendment difficulty, Elkins et al. on constitutional endurance) attest the founding problem is real but partially solved — modern constitutions use multiple stabilization mechanisms beyond supermajority thresholds. The adaptive reading's proponents (e.g., Landau, Dixon on constitutional amendment design) attest the founding problem persists in the form of calibration failure. Status quo defenders and institutional traditionalists attest the founding problem is solved by existing thresholds and recalibration is unnecessary risk.
narrative_ontology:disappearance_verdict(supermajority_threshold__adaptive_gradient_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__adaptive_gradient_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__adaptive_gradient_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(supermajority_threshold__adaptive_gradient_reading, 'none', 1).
narrative_ontology:epsilon_provenance(supermajority_threshold__adaptive_gradient_reading, 0.32, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supermajority_threshold__adaptive_gradient_reading_tests).
:- end_tests(supermajority_threshold__adaptive_gradient_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32) reflects that miscalibrated thresholds impose real costs on the faction whose preferences are blocked — but the constraint also solves genuine coordination problems (preventing constitutional churn, ensuring decision durability). Suppression (0.25) is moderate: the threshold is enforced through institutional procedures, not raw coercion, but alternatives (simple majority, consensus) are structurally suppressed by the threshold rule itself. Theater ratio (0.18) is low-moderate: some performative invocation of 'supermajority as democratic safeguard' masks calibration failures, but the core mechanism is functional. Accessibility collapse (0.42) and resistance (0.38) are moderate: alternatives exist (different thresholds, different decision rules) and political actors actively contest threshold levels. The claimed type is tangled_rope because the constraint simultaneously coordinates (prevents instability) and extracts (blocks minorities when miscalibrated high, enables majority overreach when miscalibrated low).
 *
 * PERSPECTIVAL GAP:
 *   The adaptive gradient reading predicts seat divergence that the fixed-threshold readings deny. The consensus_safeguard_reading claims the threshold is a mountain (intrinsic safeguard) from all seats. The minoritarian_veto_reading claims it is a snare (minority entrenchment) from the minority seat but mountain from the status quo seat. The adaptive reading claims the computed type varies with calibration: well-calibrated → rope from most seats; miscalibrated high → snare from minority, piton from majority; miscalibrated low → rope tension from majority, extraction from minority. The engine computes this from the structural data; the claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Constitutional designers and majority coalitions are beneficiaries when thresholds are calibrated to their agenda-setting capacity — they gain coordination without excessive extraction. Minority factions and status quo defenders are victims when thresholds are set above the actual consensus-formation rate for their issues — they bear the extraction cost of blocked reform. The directionality derivation from beneficiary/victim declarations plus exit options (constrained exit for all institutional actors) produces the expected per-seat divergence: from the majority coalition seat, the constraint computes as rope (genuine coordination); from the minority faction seat, it computes as snare (pure extraction when miscalibrated high); from the constitutional designer seat, it computes as scaffold (transitional tool meant to be recalibrated).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing constitutional instability while enabling legitimate change) remains live but contested. The adaptive reading argues the threshold arrangement has partially solved the founding problem but requires continuous recalibration — a scaffold that never sunset because the calibration problem is permanent. Mandatrophy is resolved in the sense that the constraint's function is acknowledged as ongoing and measurable, but unresolved in that most existing thresholds are not actually calibrated. The classification prevents mislabeling: a well-calibrated threshold is not pure extraction (snare) nor pure coordination (rope) nor natural law (mountain) — it is a tangled rope requiring active maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a reading of the supermajority_threshold kernel, and which reading does it instantiate?',
    'Author declaration: this constraint instantiates the adaptive_gradient_reading of the supermajority_threshold kernel. Sibling readings are consensus_safeguard_reading and minoritarian_veto_reading.',
    'Establishes the committer frame — this is one reading of a contested kernel, not a standalone constraint. Classification and ε are reading-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Kernel membership and reading identity for the adaptive gradient reading').

omega_variable(
    calibration_measurability,
    'Can actual social consensus formation rates and reversibility costs be measured with sufficient precision to calibrate thresholds in real time?',
    'Empirical studies of constitutional amendment rates, legislative override success, and public opinion dynamics across jurisdictions with different supermajority requirements.',
    'If unmeasurable, the adaptive gradient reading collapses into either a fixed threshold (mountain/rope) or an arbitrary one (snare); if measurable, the reading maintains its distinctive functional legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(calibration_measurability, empirical, 'Whether the adaptive reading''s core calibration claim is empirically operationalizable').

omega_variable(
    threshold_reversibility_cost_asymmetry,
    'Are the costs of raising versus lowering a supermajority threshold symmetric, or does institutional inertia create path dependence?',
    'Historical analysis of constitutional amendment procedures: compare successful threshold increases versus decreases, and the political capital required for each.',
    'If asymmetric (easier to raise than lower), the adaptive reading''s functional legitimacy is undermined — the tool becomes a ratchet. This would shift classification toward snare from the minority_factions seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_reversibility_cost_asymmetry, empirical, 'Whether threshold adjustment has symmetric reversibility costs in practice').

omega_variable(
    minority_veto_vs_adaptive_gradient_foreclosure,
    'Does the adaptive gradient reading''s core premise (legitimacy from measurable performance) logically foreclose the minoritarian veto reading''s premise (threshold as minority entrenchment), or do they coexist as competing interpretations?',
    'Analyze whether a constitutional system could simultaneously hold that thresholds must be evidence-calibrated AND that they empower minority vetoes — or whether accepting one reading structurally displaces the other within a single framework.',
    'If forecloses: the adaptive reading logically eliminates the minoritarian veto reading within any coherent framework. If coexists_with: both remain live positions held by different factions. If influences: adaptive calibration creates downstream pressure on minority veto claims without resolving the dispute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_veto_vs_adaptive_gradient_foreclosure, conceptual, 'Structural relationship between adaptive gradient and minoritarian veto readings').

omega_variable(
    consensus_safeguard_vs_adaptive_gradient_coexistence,
    'Does the adaptive gradient reading''s core premise (calibration to measurable rates) coexist with or foreclose the consensus safeguard reading''s premise (threshold ensures deep persistent consensus)?',
    'Analyze whether a system can simultaneously calibrate thresholds to observed consensus-formation rates AND claim the threshold''s purpose is ensuring deep consensus — or whether the calibration claim redefines the threshold''s function in a way that displaces the safeguard narrative.',
    'If coexists_with: both readings remain live in different institutional contexts. If forecloses: calibration to measurable rates replaces the deep-consensus justification. If influences: adaptive calibration reshapes the safeguard reading''s operating conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_safeguard_vs_adaptive_gradient_coexistence, conceptual, 'Structural relationship between adaptive gradient and consensus safeguard readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__adaptive_gradient_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supermajority_threshold_adaptive_tr_t0, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(supermajority_threshold_adaptive_tr_t0, observed).
narrative_ontology:measurement(supermajority_threshold_adaptive_tr_t10, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement_basis(supermajority_threshold_adaptive_tr_t10, observed).
narrative_ontology:measurement(supermajority_threshold_adaptive_tr_t20, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement_basis(supermajority_threshold_adaptive_tr_t20, observed).
narrative_ontology:measurement(supermajority_threshold_adaptive_tr_t30, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 30, 0.17).
narrative_ontology:measurement_basis(supermajority_threshold_adaptive_tr_t30, observed).
narrative_ontology:measurement(supermajority_threshold_adaptive_tr_t40, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(supermajority_threshold_adaptive_tr_t40, observed).
narrative_ontology:measurement(supermajority_threshold_adaptive_tr_t50, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 50, 0.18).
narrative_ontology:measurement_basis(supermajority_threshold_adaptive_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(supermajority_threshold_adaptive_be_t0, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(supermajority_threshold_adaptive_be_t0, observed).
narrative_ontology:measurement(supermajority_threshold_adaptive_be_t10, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement_basis(supermajority_threshold_adaptive_be_t10, observed).
narrative_ontology:measurement(supermajority_threshold_adaptive_be_t20, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement_basis(supermajority_threshold_adaptive_be_t20, observed).
narrative_ontology:measurement(supermajority_threshold_adaptive_be_t30, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 30, 0.3).
narrative_ontology:measurement_basis(supermajority_threshold_adaptive_be_t30, observed).
narrative_ontology:measurement(supermajority_threshold_adaptive_be_t40, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 40, 0.31).
narrative_ontology:measurement_basis(supermajority_threshold_adaptive_be_t40, observed).
narrative_ontology:measurement(supermajority_threshold_adaptive_be_t50, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 50, 0.32).
narrative_ontology:measurement_basis(supermajority_threshold_adaptive_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(supermajority_threshold_adaptive_su_t0, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(supermajority_threshold_adaptive_su_t0, observed).
narrative_ontology:measurement(supermajority_threshold_adaptive_su_t10, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 10, 0.18).
narrative_ontology:measurement_basis(supermajority_threshold_adaptive_su_t10, observed).
narrative_ontology:measurement(supermajority_threshold_adaptive_su_t20, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement_basis(supermajority_threshold_adaptive_su_t20, observed).
narrative_ontology:measurement(supermajority_threshold_adaptive_su_t30, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 30, 0.24).
narrative_ontology:measurement_basis(supermajority_threshold_adaptive_su_t30, observed).
narrative_ontology:measurement(supermajority_threshold_adaptive_su_t40, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 40, 0.25).
narrative_ontology:measurement_basis(supermajority_threshold_adaptive_su_t40, observed).
narrative_ontology:measurement(supermajority_threshold_adaptive_su_t50, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 50, 0.25).
narrative_ontology:measurement_basis(supermajority_threshold_adaptive_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__adaptive_gradient_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(supermajority_threshold__adaptive_gradient_reading, 0.12).
narrative_ontology:affects_constraint(supermajority_threshold__adaptive_gradient_reading, constitutional_amendment_procedure).
narrative_ontology:affects_constraint(supermajority_threshold__adaptive_gradient_reading, legislative_supermajority_rules).
narrative_ontology:affects_constraint(supermajority_threshold__adaptive_gradient_reading, judicial_review_standards).

% DUAL FORMULATION NOTE:
% Part of the supermajority_threshold constraint family with consensus_safeguard_reading and minoritarian_veto_reading. All three share the kernel_id supermajority_threshold but instantiate different readings with different ε values, beneficiary/victim structures, and claimed types. The adaptive reading's ε (0.32) is lower than the minoritarian veto reading's (expected >0.6) but higher than a perfectly calibrated threshold would be (~0.1). The consensus safeguard reading likely claims mountain with near-zero ε. This family demonstrates the ε-invariance principle: the same institutional mechanism (supermajority requirement) is structurally different constraints under different readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(supermajority_threshold__adaptive_gradient_reading, institutional, 0.35).
constraint_indexing:directionality_override(supermajority_threshold__adaptive_gradient_reading, organized, 0.45).
constraint_indexing:directionality_override(supermajority_threshold__adaptive_gradient_reading, powerless, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
