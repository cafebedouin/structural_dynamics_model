% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__adaptive_gradient_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: supermajority_threshold__adaptive_gradient_reading
 *   human_readable: Supermajority Threshold as Uncalibrated Adaptive Instrument
 *   domain: constitutional_theory/political_economy/institutional_design
 *
 * SUMMARY:
 *   This story instantiates the adaptive-gradient reading of the
 *   supermajority-threshold kernel: the claim that a supermajority
 *   requirement is a functional calibration instrument whose legitimacy
 *   depends entirely on whether its numeric bar tracks the polity's actual
 *   rate of durable consensus formation and the real cost of reversing the
 *   decisions it gates. Unlike the consensus-safeguard reading (which grounds
 *   legitimacy in the intrinsic value of deep democratic agreement) or the
 *   minoritarian-veto reading (which treats the threshold as an entrenchment
 *   device for historical privilege), this reading treats the threshold as
 *   neither intrinsically legitimate nor intrinsically illegitimate — it is
 *   an engineering parameter that can be well-tuned or badly-tuned, and the
 *   story's central finding is that in practice it is essentially never
 *   empirically tuned at all. That absence of calibration is what generates
 *   the extraction measured here: not extraction from the threshold's
 *   existence, but extraction from the gap between an untested numeric
 *   convention and the actual underlying rates it purports to track.
 *
 * KEY AGENTS:
 *   - constitutional_drafting_bodies: sets the threshold, institutional/analytical exit — could recalibrate but rarely does
 *   - reform_coalitions_below_threshold: organized/constrained — bears the cost of an uncalibrated bar set above achievable consensus
 *   - incumbent_policy_regime_holders: powerful/mobile — benefits from status quo protection regardless of whether the threshold is well-calibrated
 *   - constitutional_stability_beneficiaries: institutional/analytical — genuine coordination beneficiary when calibration happens to be correct
 *   - future_generations_facing_stale_calibration: powerless/trapped — inherits whatever calibration error accumulated before their arrival
 *   - institutional_design_researchers: analytical observer — holds the evidence that could resolve the calibration question but is not empowered to act on it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__adaptive_gradient_reading, 0.42).
domain_priors:suppression_score(supermajority_threshold__adaptive_gradient_reading, 0.38).
domain_priors:theater_ratio(supermajority_threshold__adaptive_gradient_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__adaptive_gradient_reading, tangled_rope).
narrative_ontology:human_readable(supermajority_threshold__adaptive_gradient_reading, "Supermajority Threshold as Uncalibrated Adaptive Instrument").
narrative_ontology:topic_domain(supermajority_threshold__adaptive_gradient_reading, "constitutional_theory/political_economy/institutional_design").

domain_priors:requires_active_enforcement(supermajority_threshold__adaptive_gradient_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__adaptive_gradient_reading, 'b1fa52d6-fd1f-4d2e-9328-8d6b84ff23a9').
narrative_ontology:cs_kernel_codification('b1fa52d6-fd1f-4d2e-9328-8d6b84ff23a9', formalized).
narrative_ontology:cs_authority_grounding('b1fa52d6-fd1f-4d2e-9328-8d6b84ff23a9', practice).
narrative_ontology:cs_interpretation_layer_present('b1fa52d6-fd1f-4d2e-9328-8d6b84ff23a9').
narrative_ontology:cs_reading_relation('b1fa52d6-fd1f-4d2e-9328-8d6b84ff23a9', supermajority_threshold__consensus_safeguard_reading, influences).
narrative_ontology:cs_reading_relation('b1fa52d6-fd1f-4d2e-9328-8d6b84ff23a9', supermajority_threshold__minoritarian_veto_reading, influences).
narrative_ontology:cs_axiom('b1fa52d6-fd1f-4d2e-9328-8d6b84ff23a9', foundational, threshold_legitimacy_is_calibration_contingent).
narrative_ontology:cs_axiom_status(threshold_legitimacy_is_calibration_contingent, holdable).
narrative_ontology:cs_axiom_grounding('b1fa52d6-fd1f-4d2e-9328-8d6b84ff23a9', threshold_legitimacy_is_calibration_contingent, empirically_contingent).
narrative_ontology:cs_axiom('b1fa52d6-fd1f-4d2e-9328-8d6b84ff23a9', secondary, reversibility_cost_is_measurable_input).
narrative_ontology:cs_axiom_status(reversibility_cost_is_measurable_input, holdable).
narrative_ontology:cs_axiom_grounding('b1fa52d6-fd1f-4d2e-9328-8d6b84ff23a9', reversibility_cost_is_measurable_input, instrumental).
narrative_ontology:cs_reference_frame('b1fa52d6-fd1f-4d2e-9328-8d6b84ff23a9', calibrated_engineering_instrument).
narrative_ontology:cs_drift_state('b1fa52d6-fd1f-4d2e-9328-8d6b84ff23a9', contemporary_uncalibrated_persistence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b1fa52d6-fd1f-4d2e-9328-8d6b84ff23a9', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__adaptive_gradient_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__adaptive_gradient_reading, constitutional_stability_beneficiaries).
narrative_ontology:constraint_beneficiary(supermajority_threshold__adaptive_gradient_reading, incumbent_policy_regime_holders).
narrative_ontology:constraint_victim(supermajority_threshold__adaptive_gradient_reading, reform_coalitions_below_threshold).
narrative_ontology:constraint_victim(supermajority_threshold__adaptive_gradient_reading, future_generations_facing_stale_calibration).
narrative_ontology:constraint_vindicates(supermajority_threshold__adaptive_gradient_reading, evidence_based_institutional_design_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the numeric threshold (two-thirds, three-fourths, etc.) at founding or amendment moments, typically without empirical study of actual consensus-formation rates or reversibility costs in the polity. They could commission recalibration studies and revise the threshold but rarely do, since the threshold's current beneficiaries resist re-opening the question.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, constitutional_drafting_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Assemble sustained majority support for a change — sometimes well above simple majority, short of the supermajority bar — and are blocked repeatedly across election cycles. From their position the threshold is not calibrated to any measured consensus rate; it is simply higher than what they can organize, and no mechanism exists to test whether the bar itself is set correctly.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, reform_coalitions_below_threshold, payer,
    organized, biographical, constrained, national).

% Benefit from the status quo the threshold protects. They fund studies and testimony emphasizing stability and reversibility risk whenever recalibration is proposed, but have not funded neutral empirical work on whether the current threshold matches actual social consensus formation rates, because an accurate calibration might lower the bar.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, incumbent_policy_regime_holders, beneficiary,
    powerful, generational, mobile, national).

% Courts, long-horizon investors, and institutions that rely on predictable constitutional rules benefit whenever the threshold actually tracks reversibility costs correctly — this is the genuine coordination function the reading identifies, distinct from incumbent capture.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, constitutional_stability_beneficiaries, beneficiary,
    institutional, civilizational, analytical, national).

% Inherit whatever threshold was set at an earlier moment's consensus-formation baseline. If social consensus-formation speed has changed (via communication technology, demographic shift, federalization) but the threshold was never re-tuned, they bear the cost of either instability (threshold too low for current volatility) or ossification (threshold too high for current cohesion) without having had any voice in the original calibration.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, future_generations_facing_stale_calibration, payer,
    powerless, civilizational, trapped, national).

% Study comparative constitutional thresholds and amendment-success rates across polities, producing evidence about whether specific thresholds correlate with either paralysis or instability. Their empirical work is the mechanism that would resolve whether any given threshold is well-calibrated, but their findings are rarely incorporated into binding constitutional revision processes.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, institutional_design_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(supermajority_threshold__adaptive_gradient_reading, diffuse).
narrative_ontology:fixing_cost_class(supermajority_threshold__adaptive_gradient_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A supermajority bar, correctly calibrated, solves the genuine problem of distinguishing durable, widely-shared preference shifts from transient majoritarian swings — protecting the polity from costly reversals of decisions that are expensive or impossible to undo, while still permitting change when consensus is genuinely broad and stable.
% TRANSFER_FUNCTION: Moves decision-making power from any coalition able to organize a bare or moderate majority to whichever coalition benefits from the status quo the threshold currently protects; the size of this transfer is a direct function of the gap between the enacted threshold and the polity's actual consensus-formation rate — a gap nobody is measuring.
% ABSENT_VOICES: Future cohorts who will live under whatever threshold is set now have no seat in the calibration; institutional design researchers with the relevant comparative evidence are consulted rarely and non-bindingly, since neither drafting bodies nor incumbent beneficiaries have an interest in opening the question empirically.
% DISAPPEARANCE_RATIONALE: If the threshold vanished and decisions reverted to simple majority, reversible ordinary legislation and irreversible constitutional change would be governed identically — volatile issues would swing with electoral cycles and the reversibility-cost function the threshold is supposed to encode would go unaddressed entirely; conversely, if it were replaced overnight by a properly calibrated dynamic rule, many currently-blocked reforms with genuine broad consensus would pass and many currently-protected arrangements resting on outdated calibration would fall.
% FOUNDING_PROBLEM: Constitutional framers sought a mechanism to prevent transient majorities from making costly, hard-to-reverse changes to foundational rules, while still allowing genuine deep and lasting consensus to translate into change.
% FOUNDING_PROBLEM_CORROBORATION: Institutional design researchers attest, from outside the beneficiary set, that most enacted supermajority thresholds were set by historical convention or negotiation compromise rather than by measurement of consensus-formation rates or reversibility costs, and that no jurisdiction studied has a standing mechanism to recalibrate the threshold against updated empirical baselines; incumbent beneficiaries counter that the original threshold remains adequate, but offer no comparable empirical support for that claim.
narrative_ontology:disappearance_verdict(supermajority_threshold__adaptive_gradient_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__adaptive_gradient_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__adaptive_gradient_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(supermajority_threshold__adaptive_gradient_reading, 'none', 1).
narrative_ontology:epsilon_provenance(supermajority_threshold__adaptive_gradient_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is moderate (0.42) rather than high because the reading does not claim the threshold is inherently extractive — only that, absent calibration, it drifts toward extraction as social consensus-formation rates and reversibility costs diverge from whatever baseline the enacted number implicitly assumed. Theater ratio rises across the interval (0.20 to 0.40) because more and more of the public defense of any given threshold consists of appeals to stability and tradition rather than to any actual measurement of current consensus dynamics — the theater is the substitution of rhetorical defense for empirical recalibration. Suppression is present but moderate (0.38) because the mechanism is legal and structural (the amendment process itself), not primarily coercive.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent policy regime holders and, more narrowly, institutions with a genuine stake in predictable rules (constitutional_stability_beneficiaries) sit toward the beneficiary end because the threshold — whether or not correctly calibrated — currently protects the arrangements they depend on. Reform coalitions sit toward the target end: they organize real, sustained majorities that fall short only because the bar is set where it is, not because their consensus is actually shallow relative to some measured baseline. Future generations are placed at the extreme target end (trapped, powerless) because they had no voice in the original calibration and inherit its errors without recourse.
 *
 * MANDATROPHY ANALYSIS:
 *   The adaptive-gradient reading is precisely the frame that prevents this constraint from being mislabeled as pure coordination (a naive consensus-safeguard read) or pure extraction (a naive veto read): it holds that the threshold's classification is CONDITIONAL on calibration quality, which is an empirical, checkable fact rather than a settled question. The founding problem (protecting against costly, hard-to-reverse transient majority action) remains genuinely live in the abstract, but the specific numeric answer to it is treated by every drafting body examined as a closed question rather than a standing engineering parameter subject to ongoing evidence-based revision — this is exactly the mandatrophy pattern: a tool whose original mandate (accurate calibration to real consensus dynamics) has been replaced by inertial defense of whatever number was chosen once.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection,
    'This story is the adaptive-gradient reading of the supermajority_threshold kernel — is the threshold best understood as a tunable engineering parameter (this reading), an intrinsically valuable consensus safeguard (consensus_safeguard_reading), or an entrenched minoritarian veto (minoritarian_veto_reading)?',
    'Compare structural predictions: the adaptive-gradient reading predicts extraction should track measured calibration gaps and should be resolvable by empirical recalibration; the consensus-safeguard reading predicts no calibration is needed because legitimacy is intrinsic to deep agreement itself; the veto reading predicts extraction persists regardless of calibration because it serves entrenched historical privilege. Longitudinal study of jurisdictions that DID recalibrate thresholds, compared to those that did not, would discriminate between the readings.',
    'If recalibration measurably reduces extraction and instability, this reading is vindicated and the threshold should be treated as an ongoing engineering question. If recalibration makes no difference because the same coalitions retain blocking power regardless of the numeric bar, the veto reading is closer to correct and this reading''s premise (that legitimacy is calibration-contingent) is undermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Which of the three kernel readings best characterizes the supermajority threshold''s actual operation.').

omega_variable(
    consensus_formation_rate_measurability,
    'Can ''actual social consensus formation rates'' be measured with enough precision to support a genuinely evidence-based threshold, or is the adaptive-gradient reading''s premise itself aspirational rather than operational?',
    'Development and validation of proxy measures (public opinion trend stability, cross-cohort persistence of preference shifts, natural experiments from jurisdictions with lower thresholds) that could serve as inputs to a calibration process.',
    'If no reliable measure exists, the adaptive-gradient reading''s claim to ground legitimacy in ''measurable performance rather than intrinsic value'' collapses into the same kind of unfalsifiable claim it critiques in the consensus-safeguard reading — the reading would then be a rhetorical move rather than a structurally distinct claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consensus_formation_rate_measurability, empirical, 'Whether the empirical calibration this reading demands is actually achievable.').

omega_variable(
    beneficiary_capture_of_recalibration_process,
    'Even if consensus-formation rates were measurable, would the political process that would set the threshold based on that measurement itself be captured by the same incumbent beneficiaries who currently benefit from the uncalibrated status quo?',
    'Institutional design proposals that insulate the calibration function (e.g., independent constitutional commissions with binding but periodic review) could be compared against ordinary legislative or convention-based recalibration processes for capture susceptibility.',
    'If any feasible recalibration mechanism is equally capturable, the adaptive-gradient reading''s practical distinction from the veto reading narrows considerably — the extraction it identifies may be irreducible regardless of the calibration frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_of_recalibration_process, conceptual, 'Whether an evidence-based recalibration process could itself resist the same capture dynamics that produced the current uncalibrated threshold.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__adaptive_gradient_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supe_tr_t0, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(supe_tr_t10, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(supe_tr_t20, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(supe_tr_t30, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 30, 0.34).
narrative_ontology:measurement(supe_tr_t40, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 40, 0.37).
narrative_ontology:measurement(supe_tr_t50, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(supe_be_t0, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(supe_be_t10, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(supe_be_t20, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(supe_be_t30, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 30, 0.37).
narrative_ontology:measurement(supe_be_t40, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(supe_be_t50, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 50, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(supe_su_t0, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(supe_su_t10, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(supe_su_t20, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 20, 0.31).
narrative_ontology:measurement(supe_su_t30, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 30, 0.34).
narrative_ontology:measurement(supe_su_t40, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 40, 0.36).
narrative_ontology:measurement(supe_su_t50, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 50, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__adaptive_gradient_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(supermajority_threshold__adaptive_gradient_reading, 0.12).
narrative_ontology:affects_constraint(supermajority_threshold__adaptive_gradient_reading, consensus_safeguard_reading).
narrative_ontology:affects_constraint(supermajority_threshold__adaptive_gradient_reading, minoritarian_veto_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the supermajority_threshold kernel. consensus_safeguard_reading treats the threshold's legitimacy as intrinsic to deep democratic agreement (predicted low ε, rope-leaning). minoritarian_veto_reading treats the threshold as entrenched historical privilege converted into permanent blocking power (predicted high ε, snare-leaning). This adaptive_gradient_reading occupies the structural middle: it authors moderate, calibration-contingent extraction (ε=0.42) and a tangled_rope claim, because it holds that the threshold has a genuine coordination function (protecting against costly reversals) that is currently degraded by the absence of empirical tuning rather than absent entirely. All three stories share the same kernel text and beneficiary/victim substrate but diverge in what they claim legitimizes or delegitimizes the arrangement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
