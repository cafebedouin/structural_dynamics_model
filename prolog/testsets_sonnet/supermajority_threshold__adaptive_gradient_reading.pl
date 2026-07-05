% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__adaptive_gradient_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Supermajority Threshold as Adaptive Calibration Instrument
 *   domain: constitutional_theory/political_economy/institutional_design
 *
 * SUMMARY:
 *   This story instantiates the adaptive-gradient reading of the
 *   supermajority-threshold kernel: the claim that a supermajority
 *   requirement's legitimacy is not intrinsic (as the consensus-safeguard
 *   reading holds) nor inherently a captured veto (as the minoritarian-veto
 *   reading holds), but rather contingent on whether the specific numeric
 *   threshold is empirically calibrated to actual social consensus-formation
 *   rates and the real costs of reversing a wrong decision. On this reading
 *   the SAME threshold can function as legitimate coordination (rope-like)
 *   when correctly calibrated, or as ossifying extraction (snare-like) when
 *   the number has drifted from the underlying empirics — and most real-world
 *   thresholds were fixed once, long ago, without subsequent recalibration,
 *   which is why the measured trajectory below shows extraction and
 *   suppression climbing over the interval: not because the rule changed, but
 *   because the society's actual consensus-formation rate and reversal-cost
 *   profile drifted away from the number while the number stayed fixed. The
 *   rising extraction is the signature this reading exists to detect:
 *   legitimacy erosion through failure to recalibrate, not through malicious
 *   design.
 *
 * KEY AGENTS:
 *   - constitutional_design_body: sets and rarely revisits the numeric threshold (institutional/analytical)
 *   - incumbent_coalition_holders: benefit from the threshold whenever it exceeds their opponents' current coalition size (powerful/mobile)
 *   - emergent_majority_coalitions: hold genuine majority support but fall short of threshold, bear the cost of blocked reform (organized/trapped)
 *   - urgent_reform_constituencies: face time-sensitive compounding harms the delay-based justification does not weight correctly (moderate/constrained)
 *   - constitutional_stability_beneficiaries: genuinely benefit from real calibration but are indifferent between correct and inflated calibration (organized/mobile)
 *   - empirical_calibration_researchers: analytical observers who could in principle test and correct the calibration (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__adaptive_gradient_reading, 0.42).
domain_priors:suppression_score(supermajority_threshold__adaptive_gradient_reading, 0.38).
domain_priors:theater_ratio(supermajority_threshold__adaptive_gradient_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__adaptive_gradient_reading, tangled_rope).
narrative_ontology:human_readable(supermajority_threshold__adaptive_gradient_reading, "Supermajority Threshold as Adaptive Calibration Instrument").
narrative_ontology:topic_domain(supermajority_threshold__adaptive_gradient_reading, "constitutional_theory/political_economy/institutional_design").

domain_priors:requires_active_enforcement(supermajority_threshold__adaptive_gradient_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__adaptive_gradient_reading, '37da2c04-407c-47fa-abc2-c7ba1a65d304').
narrative_ontology:cs_kernel_codification('37da2c04-407c-47fa-abc2-c7ba1a65d304', formalized).
narrative_ontology:cs_authority_grounding('37da2c04-407c-47fa-abc2-c7ba1a65d304', practice).
narrative_ontology:cs_interpretation_layer_present('37da2c04-407c-47fa-abc2-c7ba1a65d304').
narrative_ontology:cs_reading_relation('37da2c04-407c-47fa-abc2-c7ba1a65d304', supermajority_threshold__consensus_safeguard_reading, influences).
narrative_ontology:cs_reading_relation('37da2c04-407c-47fa-abc2-c7ba1a65d304', supermajority_threshold__minoritarian_veto_reading, coexists_with).
narrative_ontology:cs_axiom('37da2c04-407c-47fa-abc2-c7ba1a65d304', foundational, threshold_legitimacy_is_empirically_falsifiable).
narrative_ontology:cs_axiom_status(threshold_legitimacy_is_empirically_falsifiable, holdable).
narrative_ontology:cs_axiom_grounding('37da2c04-407c-47fa-abc2-c7ba1a65d304', threshold_legitimacy_is_empirically_falsifiable, empirically_contingent).
narrative_ontology:cs_axiom('37da2c04-407c-47fa-abc2-c7ba1a65d304', secondary, calibration_must_be_periodically_renewed).
narrative_ontology:cs_axiom_status(calibration_must_be_periodically_renewed, holdable).
narrative_ontology:cs_axiom_grounding('37da2c04-407c-47fa-abc2-c7ba1a65d304', calibration_must_be_periodically_renewed, instrumental).
narrative_ontology:cs_reference_frame('37da2c04-407c-47fa-abc2-c7ba1a65d304', empirically_calibrated_threshold_at_founding).
narrative_ontology:cs_drift_state('37da2c04-407c-47fa-abc2-c7ba1a65d304', contemporary_uncalibrated_persistence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('37da2c04-407c-47fa-abc2-c7ba1a65d304', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__adaptive_gradient_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__adaptive_gradient_reading, constitutional_stability_beneficiaries).
narrative_ontology:constraint_beneficiary(supermajority_threshold__adaptive_gradient_reading, incumbent_coalition_holders).
narrative_ontology:constraint_victim(supermajority_threshold__adaptive_gradient_reading, emergent_majority_coalitions).
narrative_ontology:constraint_victim(supermajority_threshold__adaptive_gradient_reading, urgent_reform_constituencies).
narrative_ontology:constraint_vindicates(supermajority_threshold__adaptive_gradient_reading, evidence_based_institutional_calibration_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and periodically reviews the numeric threshold (e.g. two-thirds, three-fifths) for constitutional amendment or supermajority legislative action. Justifies the specific number by appeal to consensus-formation studies and reversibility-cost estimates, but rarely re-derives the threshold once set; administers the rule that governs whether change can occur at all.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, constitutional_design_body, agenda_setter,
    institutional, generational, analytical, national).

% Currently hold enough blocking votes to prevent threshold-crossing change without needing to build broader coalitions. Benefit whenever the calibrated threshold happens to sit above their opponents' current coalition size, regardless of whether that calibration reflects a genuine consensus-formation rate or simply locks in the present distribution of power.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, incumbent_coalition_holders, beneficiary,
    powerful, biographical, mobile, national).

% Have assembled a genuine majority (55-64%) favoring change but fall short of the calibrated threshold. Bear the cost of every legislative cycle spent unable to enact reform, without access to a mechanism that would test whether the threshold's current calibration still matches actual consensus-formation dynamics. Cannot exit the polity to escape the rule; can only keep organizing.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, emergent_majority_coalitions, payer,
    organized, biographical, trapped, national).

% Face concrete, time-sensitive harms (e.g. rights violations, resource crises) that require fast constitutional correction. The threshold's reversibility-cost justification assumes symmetric stakes between delay-cost and error-cost, but for this group delay itself compounds harm irreversibly. Their exit option is largely litigation or waiting, both slow relative to the harm.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, urgent_reform_constituencies, payer,
    moderate, immediate, constrained, regional).

% Investors, long-horizon institutions, and citizens who rely on constitutional predictability to plan across decades. Genuinely benefit from a threshold that prevents rapid oscillation of foundational rules, provided the threshold is actually calibrated to real reversibility costs rather than arbitrarily high.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, constitutional_stability_beneficiaries, beneficiary,
    organized, generational, mobile, national).

% Political scientists and institutional economists who study actual consensus-formation rates, measure how often supermajority thresholds have blocked subsequently-validated reforms versus prevented instability, and propose recalibration. Their findings could justify raising or lowering the threshold but are rarely binding on the design body.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, empirical_calibration_researchers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(supermajority_threshold__adaptive_gradient_reading, diffuse).
narrative_ontology:fixing_cost_class(supermajority_threshold__adaptive_gradient_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sets a bar for constitutional change high enough that changes reflect broad, durable agreement rather than transient majorities, reducing the risk of costly, hard-to-reverse constitutional churn — a genuine coordination problem when the threshold tracks actual consensus-formation and reversal-cost data.
% TRANSFER_FUNCTION: When correctly calibrated, moves nothing extractive — it merely delays action until consensus is verified. When miscalibrated (set above the actual point where reversibility risk justifies delay), it transfers decision-power from emergent majorities to whichever coalition currently holds blocking-minority status, at the cost of unaddressed urgent harms.
% ABSENT_VOICES: The urgent reform constituencies bearing time-sensitive, compounding harms are structurally underweighted: the calibration debate is conducted by design bodies and researchers on generational timescales, while the people paying the cost of delay operate on immediate timescales and are rarely seated in the recalibration conversation.
% DISAPPEARANCE_RATIONALE: If the threshold vanished (reverting to simple majority), constitutional stability beneficiaries and incumbent coalition holders would say the world rearranges catastrophically (rapid, reversible constitutional churn); emergent majority coalitions and urgent reform constituencies would say the world finally rearranges to reflect actual preferences. The verdict genuinely depends on whether the current calibration is closer to a real reversibility-cost estimate or an arbitrary lock-in — which is exactly the disputed empirical question this reading centers.
% FOUNDING_PROBLEM: Early constitutional designers needed a mechanism to distinguish durable, broad-based social consensus from transient majoritarian passion, preventing foundational rules from being rewritten by narrow or short-lived majorities.
% FOUNDING_PROBLEM_CORROBORATION: Empirical calibration researchers, working outside both the incumbent-coalition and reform-constituency camps, attest that the founding problem (distinguishing durable consensus from transient passion) remains partially live but that most existing thresholds were set once, decades or centuries ago, without re-derivation from measured consensus-formation rates — meaning the mechanism now often functions on inertia rather than active calibration to the problem it was built to solve.
narrative_ontology:disappearance_verdict(supermajority_threshold__adaptive_gradient_reading, contested).
narrative_ontology:founding_problem_status(supermajority_threshold__adaptive_gradient_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__adaptive_gradient_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness starts moderate-low (0.22) reflecting a threshold that, at founding, plausibly tracked genuine consensus-formation and reversibility data. It climbs to 0.42 by the interval's end as the fixed numeric threshold increasingly diverges from a changed social reality (faster information diffusion, different coalition dynamics) without recalibration — the same number now blocks changes that would, under an updated empirical estimate, clear a properly-calibrated bar. Suppression rises in parallel (0.20 to 0.38) as the gap between the fixed rule and the underlying justification widens, requiring more active defense of the number itself rather than defense of its rationale. Theater ratio rises modestly (0.10 to 0.28) as design bodies increasingly invoke calibration language ceremonially, citing 'consensus' and 'stability' without conducting the empirical work the reading's own legitimacy standard requires.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent coalition holders and constitutional stability beneficiaries sit toward the beneficiary end: the former structurally, because any fixed threshold that exceeds a rival coalition's current size hands them a costless veto; the latter genuinely, when calibration holds, but their interest does not discriminate between correct and stale calibration. Emergent majority coalitions and urgent reform constituencies sit toward the target end: they are trapped or constrained, bear the compounding cost of delay, and have no institutional lever to force recalibration. The design body itself is analytically positioned but administers the rule that determines everyone else's directionality — a classic agenda-setter/payer asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading exists precisely to prevent two mislabeling errors: (1) treating a well-calibrated threshold as pure extraction merely because it currently blocks a majority (the minoritarian-veto reading's risk), and (2) treating a stale, uncalibrated threshold as legitimate coordination merely because it was once justified (the consensus-safeguard reading's risk when applied uncritically). By grounding legitimacy in measurable performance — actual consensus-formation rates and reversibility costs — rather than in either intrinsic constitutional value or bare power distribution, this reading makes the threshold's classification an empirical question subject to falsification, and the rising extraction/suppression trajectory in this story models exactly the failure mode where that empirical check is never performed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    calibration_currency_ambiguity,
    'Is the currently operative supermajority threshold in this polity actually calibrated to a measured consensus-formation rate and reversibility-cost estimate, or was it set once (at founding or a prior constitutional moment) and never empirically revisited?',
    'Historical and political-science review of the threshold''s original justification record, cross-referenced against contemporary studies of coalition-formation speed and the actual reversal rate of constitutional changes passed at various supermajority levels across comparable polities.',
    'If the threshold was never empirically derived or has not been revisited despite available calibration data, this reading''s own legitimacy standard is unmet by the very constraint it describes — pushing the classification toward tangled_rope or snare depending on how large the drift is. If it is actively recalibrated, the classification should sit closer to rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(calibration_currency_ambiguity, empirical, 'Whether the threshold is presently calibrated or merely historically justified.').

omega_variable(
    reading_selection_under_determination,
    'Given that consensus_safeguard_reading, minoritarian_veto_reading, and this adaptive_gradient_reading are all coherent readings of the same textual/practice kernel, what determines which reading a given constitutional actor or court will apply when adjudicating a specific threshold dispute?',
    'Comparative jurisprudence study of how courts and legislatures justify supermajority requirements in contested cases — do they invoke intrinsic consensus language, power-distribution critique, or empirical calibration studies? The dominant justification pattern reveals which reading is operatively controlling.',
    'If courts overwhelmingly invoke intrinsic-consensus language (consensus_safeguard_reading) regardless of actual calibration data, this adaptive_gradient_reading remains a minority scholarly position with limited practical purchase — the constraint as actually enforced would be better modeled by the sibling reading, even though this reading is analytically available.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_under_determination, conceptual, 'Which kernel reading actually controls real-world adjudication.').

omega_variable(
    symmetric_error_cost_assumption,
    'Does the reversibility-cost calculation underlying this reading''s legitimacy standard treat delay-costs (harm from not acting) and reversal-costs (harm from acting wrongly and having to undo it) symmetrically, and is that symmetry defensible?',
    'Formal decision-theoretic modeling comparing the actual harm profiles of urgent_reform_constituencies (compounding, front-loaded harm from delay) against the harm profile a wrongly-reversed constitutional change would produce, using historical cases where supermajority-blocked reforms were later found to have been justified.',
    'If delay-costs are systematically underweighted relative to reversal-costs in the calibration methodology, the ''evidence-based'' threshold this reading defends is not actually neutral — it structurally favors the status quo even under its own stated empirical standard, which would push several currently-classified tangled_rope instances toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symmetric_error_cost_assumption, conceptual, 'Whether the reading''s own calibration methodology has a hidden status-quo bias.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__adaptive_gradient_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supe_tr_t0, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(supe_tr_t8, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(supe_tr_t16, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(supe_tr_t24, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 24, 0.21).
narrative_ontology:measurement(supe_tr_t32, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 32, 0.25).
narrative_ontology:measurement(supe_tr_t40, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(supe_be_t0, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(supe_be_t8, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 8, 0.27).
narrative_ontology:measurement(supe_be_t16, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 16, 0.33).
narrative_ontology:measurement(supe_be_t24, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 24, 0.37).
narrative_ontology:measurement(supe_be_t32, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(supe_be_t40, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(supe_su_t0, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(supe_su_t8, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 8, 0.24).
narrative_ontology:measurement(supe_su_t16, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 16, 0.28).
narrative_ontology:measurement(supe_su_t24, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 24, 0.32).
narrative_ontology:measurement(supe_su_t32, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 32, 0.35).
narrative_ontology:measurement(supe_su_t40, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__adaptive_gradient_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(supermajority_threshold__adaptive_gradient_reading, supermajority_threshold__consensus_safeguard_reading).
narrative_ontology:affects_constraint(supermajority_threshold__adaptive_gradient_reading, supermajority_threshold__minoritarian_veto_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the supermajority_threshold kernel. consensus_safeguard_reading treats the threshold's legitimacy as intrinsic to demonstrated deep consensus; minoritarian_veto_reading treats it as inherently a historical-privilege-to-veto conversion mechanism; this adaptive_gradient_reading treats legitimacy as a contingent, measurable function of calibration currency. Each has a distinct ε trajectory: consensus_safeguard_reading is authored with low, stable extraction (mountain-adjacent legitimacy claim); minoritarian_veto_reading is authored with high, stable extraction (snare); this reading is authored with extraction that RISES over the interval, modeling calibration decay specifically. The three should never be merged or averaged — they are structurally distinct constraints sharing one textual/practice kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
