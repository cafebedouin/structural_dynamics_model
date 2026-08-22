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
 *   constraint_id: supermajority_threshold__adaptive_gradient_reading
 *   human_readable: Supermajority Amendment Threshold — Adaptive Gradient Reading
 *   domain: constitutional_theory/political_economy/institutional_design
 *
 * SUMMARY:
 *   This story instantiates the adaptive-gradient reading of the
 *   supermajority-threshold kernel: the claim that the threshold's legitimacy
 *   is not intrinsic (as the consensus-safeguard reading holds) nor
 *   inherently a captured veto (as the minoritarian-veto reading holds), but
 *   is a functional dial whose correctness depends on empirical facts — how
 *   fast genuine social consensus actually forms on a given question, and how
 *   costly it would be to reverse a wrong decision made under a too-low bar.
 *   Under this reading, the SAME numeric threshold (say, two-thirds) can be
 *   well-calibrated in one polity-era and badly miscalibrated in another,
 *   because consensus-formation speed and reversal costs are empirical,
 *   historically variable quantities — not constants derivable from the
 *   threshold's mere existence. The extraction this story measures is the
 *   drift that accumulates when a threshold, once set, is never re-tested
 *   against those empirical facts: legitimacy erodes gradually as the gap
 *   between the calibrated-in-principle bar and the actually-required bar
 *   widens, benefiting whoever currently holds the veto position and costing
 *   emergent majorities and future generations who inherit an untested dial.
 *
 * KEY AGENTS:
 *   - constitutional_stability_beneficiaries: institutional actors (institutional/constrained) whose planning depends on rule persistence
 *   - incumbent_coalition_holders: current veto-holding coalition (organized/mobile) that administers de facto calibration discourse
 *   - emergent_majority_coalitions: new popular majorities (organized/trapped) who must clear a potentially miscalibrated bar
 *   - future_generations_facing_miscalibrated_threshold: powerless/trapped bearers of compounding calibration drift
 *   - institutional_designers: analytical observers with the comparative data needed to test calibration
 *   - reform_advocacy_groups: excluded voices pressing for recalibration, blocked by the self-referential threshold-changes-require-the-threshold problem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__adaptive_gradient_reading, 0.42).
domain_priors:suppression_score(supermajority_threshold__adaptive_gradient_reading, 0.38).
domain_priors:theater_ratio(supermajority_threshold__adaptive_gradient_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__adaptive_gradient_reading, tangled_rope).
narrative_ontology:human_readable(supermajority_threshold__adaptive_gradient_reading, "Supermajority Amendment Threshold — Adaptive Gradient Reading").
narrative_ontology:topic_domain(supermajority_threshold__adaptive_gradient_reading, "constitutional_theory/political_economy/institutional_design").

domain_priors:requires_active_enforcement(supermajority_threshold__adaptive_gradient_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__adaptive_gradient_reading, '2b3e3fd1-8d63-46cd-bd8c-5d8ac76b7614').
narrative_ontology:cs_kernel_codification('2b3e3fd1-8d63-46cd-bd8c-5d8ac76b7614', formalized).
narrative_ontology:cs_authority_grounding('2b3e3fd1-8d63-46cd-bd8c-5d8ac76b7614', practice).
narrative_ontology:cs_interpretation_layer_present('2b3e3fd1-8d63-46cd-bd8c-5d8ac76b7614').
narrative_ontology:cs_reading_relation('2b3e3fd1-8d63-46cd-bd8c-5d8ac76b7614', supermajority_threshold__consensus_safeguard_reading, influences).
narrative_ontology:cs_reading_relation('2b3e3fd1-8d63-46cd-bd8c-5d8ac76b7614', supermajority_threshold__minoritarian_veto_reading, influences).
narrative_ontology:cs_axiom('2b3e3fd1-8d63-46cd-bd8c-5d8ac76b7614', foundational, threshold_legitimacy_is_empirically_contingent).
narrative_ontology:cs_axiom_status(threshold_legitimacy_is_empirically_contingent, holdable).
narrative_ontology:cs_axiom_grounding('2b3e3fd1-8d63-46cd-bd8c-5d8ac76b7614', threshold_legitimacy_is_empirically_contingent, empirically_contingent).
narrative_ontology:cs_axiom('2b3e3fd1-8d63-46cd-bd8c-5d8ac76b7614', secondary, reversibility_cost_must_bound_threshold_height).
narrative_ontology:cs_axiom_status(reversibility_cost_must_bound_threshold_height, holdable).
narrative_ontology:cs_axiom_grounding('2b3e3fd1-8d63-46cd-bd8c-5d8ac76b7614', reversibility_cost_must_bound_threshold_height, instrumental).
narrative_ontology:cs_reference_frame('2b3e3fd1-8d63-46cd-bd8c-5d8ac76b7614', empirically_calibrated_consensus_threshold).
narrative_ontology:cs_drift_state('2b3e3fd1-8d63-46cd-bd8c-5d8ac76b7614', contemporary_unrecalibrated_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2b3e3fd1-8d63-46cd-bd8c-5d8ac76b7614', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__adaptive_gradient_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__adaptive_gradient_reading, constitutional_stability_beneficiaries).
narrative_ontology:constraint_beneficiary(supermajority_threshold__adaptive_gradient_reading, incumbent_coalition_holders).
narrative_ontology:constraint_victim(supermajority_threshold__adaptive_gradient_reading, emergent_majority_coalitions).
narrative_ontology:constraint_victim(supermajority_threshold__adaptive_gradient_reading, future_generations_facing_miscalibrated_threshold).
narrative_ontology:constraint_vindicates(supermajority_threshold__adaptive_gradient_reading, evidence_based_institutional_design_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Long-tenured institutional actors — courts, executive agencies, entrenched policy regimes — whose planning and legitimacy depend on constitutional rules not shifting with every electoral cycle. They benefit whenever the threshold is calibrated to actually slow only genuinely transient majorities, and they benefit incidentally even when it's miscalibrated too high, since ossification also protects their position.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, constitutional_stability_beneficiaries, beneficiary,
    institutional, generational, constrained, national).

% The coalition that assembled the current supermajority-protected arrangement. They administer or heavily influence discourse over whether the threshold is 'well-calibrated,' and have structural incentive to resist recalibration studies that would show the threshold set too high, since correction would erode their veto position.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, incumbent_coalition_holders, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(supermajority_threshold__adaptive_gradient_reading, incumbent_coalition_holders, agenda_setter).

% New popular majorities — sometimes durable, sometimes still forming — that need to clear the supermajority bar to enact constitutional change. When the threshold is calibrated above actual consensus-formation cost, they pay in the form of blocked reform even after achieving genuine, sustained majority support; they cannot exit the jurisdiction's amendment process.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, emergent_majority_coalitions, payer,
    organized, biographical, trapped, national).

% Bear the compounding cost of a threshold set without empirical recalibration: if too high, accumulated grievances go unaddressed until they erupt outside constitutional channels; if too low, they inherit an unstable rule structure. They have no seat in the current calibration debate and cannot retroactively correct the threshold that governs them.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, future_generations_facing_miscalibrated_threshold, payer,
    powerless, civilizational, trapped, national).

% Comparative constitutionalists and mechanism-design scholars who study amendment-rate data, reversal costs, and consensus-formation speed across jurisdictions. They can, in principle, produce the evidence that would recalibrate the threshold, but their findings are advisory unless a political process adopts them.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, institutional_designers, observer,
    analytical, generational, analytical, global).

% Groups pushing for threshold recalibration itself — arguing the current bar no longer matches actual social consensus-formation rates — but structurally excluded from the amendment process because changing the threshold usually itself requires clearing the very threshold under dispute.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, reform_advocacy_groups, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(supermajority_threshold__adaptive_gradient_reading, incumbent_coalition_holders).
narrative_ontology:fixing_cost_class(supermajority_threshold__adaptive_gradient_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A calibrated supermajority bar solves the genuine problem of distinguishing durable, broad-based consensus from transient majoritarian preference — but only if the bar's height is periodically checked against real consensus-formation rates and the cost of reversing a wrong decision, rather than fixed at a level nobody re-examines.
% TRANSFER_FUNCTION: Moves decision-making power from emergent majorities (who must clear an unexamined threshold) to whichever coalition currently benefits from the existing calibration — and moves the cost of any miscalibration forward onto future generations who inherit either instability or ossification without having set the dial.
% ABSENT_VOICES: Reform advocacy groups arguing for recalibration are structurally excluded from the conversation that would recalibrate the threshold, because changing the threshold typically requires clearing the same threshold — a self-referential lock reform coalitions rarely have the numbers to break. Institutional designers with the relevant comparative data are advisory-only, not seated in the amendment process.
% DISAPPEARANCE_RATIONALE: If the threshold vanished (reverting to simple majority) advocates of the adaptive-gradient reading would say the world partially rearranges — some accumulated reform backlog would clear, but so would protections against genuinely transient passions, and whether that net change is good depends entirely on whether the threshold was in fact miscalibrated, which is exactly the empirical question this reading says was never checked. Incumbent beneficiaries would say the world rearranges badly (loss of stability); emergent majorities would say it rearranges well (overdue reform).
% FOUNDING_PROBLEM: Constitution-drafters sought a mechanism to prevent constitutional fundamentals from being rewritten by narrow, possibly temporary majorities, while still allowing genuine broad consensus to amend the document over time.
% FOUNDING_PROBLEM_CORROBORATION: Comparative constitutional scholars outside the current incumbent coalition (institutional_designers seat) attest that many supermajority thresholds were set by historical accident or bargaining leverage at founding, not by any measurement of actual consensus-formation rates, and that no jurisdiction studied has since empirically recalibrated its threshold against reversal-cost data — meaning the founding problem's 'live' status cannot presently be verified from outside the beneficiary coalitions themselves.
narrative_ontology:disappearance_verdict(supermajority_threshold__adaptive_gradient_reading, contested).
narrative_ontology:founding_problem_status(supermajority_threshold__adaptive_gradient_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__adaptive_gradient_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.42) and suppression (0.38) are set moderate, not extreme, because under this reading the threshold's structure is not inherently extractive — it becomes extractive only to the degree it has drifted from an empirically defensible calibration, and that drift is measured, not assumed, to be substantial but not severe. Theater ratio (0.30) reflects that some 'calibration' discourse (blue-ribbon commissions, academic literature reviews) exists but rarely translates into actual threshold adjustment, so a meaningful share of calibration-oriented activity is performative. Accessibility collapse (0.35) and resistance (0.50) are moderate: alternatives to the current threshold (lower, higher, or issue-differentiated bars) remain conceptually available and are actively argued for by reform_advocacy_groups, which is why resistance is non-trivial — this is not a settled natural fact the way a true consensus-safeguard reading might treat it, but neither is it a fully open contest.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (constitutional_stability_beneficiaries, incumbent_coalition_holders) sit near the low-d end: they gain from the status quo threshold regardless of whether it is actually well-calibrated, because ossification protects entrenched positions as a side effect. Payers (emergent_majority_coalitions, future_generations_facing_miscalibrated_threshold) sit near the high-d end: they bear the calibration gap's cost directly, and their exit options are trapped/organized-but-blocked rather than mobile. The self-referential lock — that changing the threshold requires clearing the threshold — is what converts a merely-imperfect functional tool into an actively enforced structure requiring the tangled_rope's coordination-plus-extraction gate.
 *
 * MANDATROPHY ANALYSIS:
 *   The adaptive-gradient reading is precisely the frame that prevents this constraint from being mislabeled as pure extraction (minoritarian_veto_reading) or pure legitimate safeguard (consensus_safeguard_reading) in cases where the empirical calibration question is genuinely open. Where comparative data shows the threshold well-matched to actual consensus-formation rates and reversal costs, this reading would compute close to rope; where data shows severe drift with no re-calibration mechanism, it computes closer to snare. The founding_problem_status is 'contested' precisely because the mandate (calibrate to consensus-formation reality) has plausibly outlived active verification — nobody is currently checking whether the dial is still correctly set, which is the diagnostic signature of drift into inertial risk, though the reading does not assert this has fully happened.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    calibration_evidence_availability,
    'Does reliable empirical data exist on actual consensus-formation rates and reversal costs for this jurisdiction''s constitutional questions, sufficient to determine whether the current threshold is over- or under-calibrated?',
    'Comparative constitutional political-economy research: amendment-frequency data across peer jurisdictions with varying thresholds, matched against post-amendment reversal/repeal rates and measured public-opinion durability studies.',
    'If such data exists and shows the current threshold well-matched, this reading computes closer to rope (functional, evidence-validated tool). If data shows persistent drift with no correction mechanism, the same reading computes closer to snare-adjacent tangled_rope, since the coordination story becomes cover for an unexamined veto.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(calibration_evidence_availability, empirical, 'Whether the calibration claim is testable and, if tested, favorable.').

omega_variable(
    self_referential_recalibration_lock,
    'Can the threshold''s own calibration ever be corrected through the ordinary amendment process, given that changing the threshold itself typically requires clearing the existing threshold?',
    'Historical case study of jurisdictions that have successfully lowered or restructured their own amendment thresholds — identifying what exceptional conditions (crisis, external imposition, near-unanimous elite consensus) made recalibration possible.',
    'If recalibration has essentially never occurred except under extraordinary conditions, the ''evidence-based tuning'' this reading depends on is structurally inaccessible in practice, which would push the constraint''s real operation toward the minoritarian_veto_reading''s territory despite this reading''s more neutral framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_referential_recalibration_lock, empirical, 'Whether the adaptive-gradient framing is practically actionable or only theoretically available.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Is the disagreement between this reading and its siblings located in what the threshold IS (descriptive/structural) or in what standard should be used to judge it legitimate (normative)?',
    'Textual and argumentative analysis of how each reading''s proponents respond to the same amendment-rate data — if all three readings would reach the same verdict given identical data, the disagreement is purely normative; if they would reach different verdicts from the same data, the disagreement is partly descriptive.',
    'If the disagreement is purely normative, no amount of comparative data resolves the kernel contest and ''adaptive_gradient'' is simply the empiricist party''s preferred normative frame rather than a neutral empirical arbiter. If partly descriptive, the readings make genuinely different, testable claims about threshold function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Whether the kernel contest is resolvable by evidence or is normative all the way down.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__adaptive_gradient_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supe_tr_t0, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(supe_tr_t10, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(supe_tr_t20, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(supe_tr_t30, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(supe_tr_t40, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 40, 0.27).
narrative_ontology:measurement(supe_tr_t50, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 50, 0.29).
narrative_ontology:measurement(supe_tr_t60, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 60, 0.3).

% Extraction over time
narrative_ontology:measurement(supe_be_t0, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(supe_be_t10, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 10, 0.31).
narrative_ontology:measurement(supe_be_t20, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(supe_be_t30, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(supe_be_t40, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(supe_be_t50, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 50, 0.41).
narrative_ontology:measurement(supe_be_t60, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 60, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(supe_su_t0, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(supe_su_t10, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(supe_su_t20, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 20, 0.31).
narrative_ontology:measurement(supe_su_t30, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 30, 0.33).
narrative_ontology:measurement(supe_su_t40, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 40, 0.35).
narrative_ontology:measurement(supe_su_t50, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 50, 0.37).
narrative_ontology:measurement(supe_su_t60, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 60, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__adaptive_gradient_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(supermajority_threshold__adaptive_gradient_reading, 0.12).
narrative_ontology:affects_constraint(supermajority_threshold__adaptive_gradient_reading, supermajority_threshold__consensus_safeguard_reading).
narrative_ontology:affects_constraint(supermajority_threshold__adaptive_gradient_reading, supermajority_threshold__minoritarian_veto_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the supermajority_threshold kernel. consensus_safeguard_reading treats the threshold's legitimacy as intrinsic (a fixed bar correctly separating durable consensus from transient passion, ε low, near-mountain in its own telling). minoritarian_veto_reading treats the threshold as captured entrenchment converting historical bargaining power into permanent veto (ε high, snare-leaning). This adaptive_gradient_reading treats legitimacy as a function of measurable calibration variables, occupying structurally distinct middle ground (ε moderate, tangled_rope) — not an average of the other two readings' ε values, but a genuinely different claim about what determines the threshold's legitimacy. Each story authors its own ε from its own reading's lights; they are linked here for contamination-propagation analysis, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
