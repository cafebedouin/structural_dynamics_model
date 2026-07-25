% ============================================================================
% CONSTRAINT STORY: evaluator_incentive_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_evaluator_incentive_asymmetry, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: evaluator_incentive_asymmetry
 *   human_readable: Evaluator Time-Budget Bias Toward Surface Legibility Over Provenance
 *   domain: epistemics/institutional/authorship
 *
 * SUMMARY:
 *   This story isolates the structural delta that holds regardless of which
 *   reading of cooperative-artifact legitimacy an author endorses: whatever
 *   an author believes about who SHOULD get credit, the evaluator on the
 *   other end faces a fixed time budget per artifact, and that time budget
 *   mechanically rewards whatever is cheapest to check — surface smoothness —
 *   over whatever is expensive to verify — causal/authorial history. This is
 *   deliberately NOT one of the three kernel readings (legibility_primacy,
 *   authorial_primacy, process_transparency); it is the shared mechanical
 *   substrate underneath all three, the reason the kernel is contested in the
 *   first place. Legibility-primacy treats this time pressure as legitimate
 *   triage; authorial-primacy treats it as the source of fraud-by-omission;
 *   process-transparency treats it as a solvable infrastructure gap. This
 *   story's claim is that the asymmetry itself is closer to genuine
 *   coordination (evaluators really cannot verify everything, and legibility
 *   really does correlate with baseline editorial effort) than to pure
 *   extraction — hence claimed_type rope — but the metrics show the
 *   coordination function has drifted toward reliably sacrificing a specific
 *   victim class (illegible-but-real contributors) as volume has grown, which
 *   is the tangled-rope pressure visible in the rising extractiveness series.
 *
 * KEY AGENTS:
 *   - high_throughput_evaluators: primary beneficiary of the proxy (institutional/mobile) — collects processing efficiency
 *   - volume_optimizing_institutions: sets the reward structure that keeps evaluators using the proxy (institutional/arbitrage)
 *   - individual_contributors_with_illegible_authorship: primary target — bears the cost of the proxy in credit, pay, and credentialing (moderate/constrained)
 *   - polished_single_author_submitters: incidental beneficiary — already matches the proxy for reasons unrelated to their work's true quality
 *   - provenance_verification_infrastructure: the non-agent absent alternative — under-built, no accountable owner
 *   - researchers_of_evaluation_bias: analytical observer measuring the time-to-evaluate / smoothness correlation directly
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(evaluator_incentive_asymmetry, 0.58).
domain_priors:suppression_score(evaluator_incentive_asymmetry, 0.52).
domain_priors:theater_ratio(evaluator_incentive_asymmetry, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(evaluator_incentive_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(evaluator_incentive_asymmetry, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(evaluator_incentive_asymmetry, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(evaluator_incentive_asymmetry, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(evaluator_incentive_asymmetry, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(evaluator_incentive_asymmetry, rope).
narrative_ontology:human_readable(evaluator_incentive_asymmetry, "Evaluator Time-Budget Bias Toward Surface Legibility Over Provenance").
narrative_ontology:topic_domain(evaluator_incentive_asymmetry, "epistemics/institutional/authorship").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(evaluator_incentive_asymmetry, high_throughput_evaluators).
narrative_ontology:constraint_beneficiary(evaluator_incentive_asymmetry, volume_optimizing_institutions).
narrative_ontology:constraint_victim(evaluator_incentive_asymmetry, individual_contributors_with_illegible_authorship).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(evaluator_incentive_asymmetry, polished_single_author_submitters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Graders, reviewers, hiring committees, and algorithmic filters processing large volumes of submissions under fixed time budgets. They use surface smoothness (consistent style, clean structure, absence of friction) as a fast proxy for quality because verifying actual provenance per-artifact is too expensive to do at scale. They are not choosing to devalue authorship maliciously; the time constraint mechanically rewards whatever is cheapest to check.
narrative_ontology:constraint_stakeholder(evaluator_incentive_asymmetry, high_throughput_evaluators, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(evaluator_incentive_asymmetry, high_throughput_evaluators, agenda_setter).

% Journals, employers, and platforms that design evaluation pipelines around throughput metrics (submissions processed per reviewer-hour, time-to-hire, time-to-publish). They benefit from a proxy that lets them scale evaluation without scaling verification staff, and they set the reward structure (promotion, reputation, revenue) that keeps evaluators optimizing for legibility.
narrative_ontology:constraint_stakeholder(evaluator_incentive_asymmetry, volume_optimizing_institutions, beneficiary,
    institutional, generational, arbitrage, national).

% Authors whose actual contribution is real but does not present as smooth single-voice prose — non-native speakers, junior collaborators whose work gets edited into someone else's voice, disabled writers using assistive tools, people whose reasoning is genuinely idiosyncratic. Their legible surface is sacrificed or smoothed by editors/collaborators to survive evaluation, or their unsmoothed work is penalized as 'rough' regardless of its underlying rigor. Exit means opting out of evaluated channels entirely, which forfeits credentialing and pay.
narrative_ontology:constraint_stakeholder(evaluator_incentive_asymmetry, individual_contributors_with_illegible_authorship, payer,
    moderate, biographical, constrained, national).

% Contributors whose natural writing already matches the legible-surface proxy, or who have access to editing/ghostwriting resources that produce that surface. They pass evaluation quickly regardless of the depth of independent verification behind their work, and benefit from a system that rewards a trait they already possess.
narrative_ontology:constraint_stakeholder(evaluator_incentive_asymmetry, polished_single_author_submitters, beneficiary,
    moderate, biographical, mobile, national).

% Tools and practices (version histories, contribution statements, process logs, plagiarism/AI-detection systems) that could substitute for the legibility proxy exist but are rarely funded or mandated at the scale evaluation happens. Their absence is structural, not a choice by any single actor, and no seat in the evaluation pipeline is accountable for building them.
narrative_ontology:constraint_stakeholder(evaluator_incentive_asymmetry, provenance_verification_infrastructure, excluded,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(evaluator_incentive_asymmetry, provenance_verification_infrastructure).

% Scholars of peer review, hiring bias, and algorithmic fairness who measure the correlation between quality scores and stylistic consistency, and who document time-to-evaluate as a function of surface smoothness versus provenance-verification effort. They have no power to change evaluator incentives but produce the evidence base for reform proposals.
narrative_ontology:constraint_stakeholder(evaluator_incentive_asymmetry, researchers_of_evaluation_bias, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(evaluator_incentive_asymmetry, diffuse).
narrative_ontology:fixing_cost_class(evaluator_incentive_asymmetry, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Evaluators facing many artifacts and fixed time need SOME fast proxy to triage; surface legibility genuinely correlates with baseline editing effort and reduces evaluator cognitive load, letting large volumes of cooperative work get processed at all rather than backing up indefinitely.
% TRANSFER_FUNCTION: Moves evaluative attention and downstream reward (grades, publication, hiring, pay) away from contributors whose real work doesn't present as smooth single-voice output, and toward contributors or editorial processes that produce smooth surfaces — regardless of whether smoothness tracks the artifact's actual quality or authorship integrity.
% ABSENT_VOICES: Individual contributors whose authorship gets erased by editorial smoothing, and provenance-verification infrastructure builders who are never in the room when evaluation pipelines are designed for throughput; both would argue that the proxy is measuring the wrong thing but neither has a seat at the pipeline-design table.
% DISAPPEARANCE_RATIONALE: If the time-constraint pressure vanished, evaluators claim the change would be catastrophic (queues would back up indefinitely, nothing would get processed) — world_rearranges from their seat. Contributors and provenance researchers argue the underlying evaluation function could persist fine with better-funded verification infrastructure substituting for the legibility shortcut — world_unchanged from theirs. The dispute over which reading is correct is itself the constraint's live fault line.
% FOUNDING_PROBLEM: Evaluators need to process more cooperative artifacts than they have time to individually verify; some fast, cheap heuristic is needed to triage what to reward, reject, or advance.
% FOUNDING_PROBLEM_CORROBORATION: Peer-review-timing studies and hiring-pipeline audits conducted by researchers outside any evaluating institution corroborate that time-per-artifact has not scaled with submission volume, making the throughput problem genuinely still live rather than a pretext; the same outside research also documents that the resulting proxy systematically misweights authorship legibility relative to verified provenance, which no evaluating institution itself has funded verification infrastructure to correct.
narrative_ontology:disappearance_verdict(evaluator_incentive_asymmetry, contested).
narrative_ontology:founding_problem_status(evaluator_incentive_asymmetry, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(evaluator_incentive_asymmetry, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-25',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(evaluator_incentive_asymmetry, 'none', 1).
narrative_ontology:epsilon_provenance(evaluator_incentive_asymmetry, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(evaluator_incentive_asymmetry_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(evaluator_incentive_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(evaluator_incentive_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at interval end, rising from 0.42) because the proxy started as a defensible triage heuristic under light load and has drifted as volume grew — more artifacts per evaluator-hour means legibility now substitutes for verification in cases where verification was once feasible. Suppression (0.52) reflects that alternatives (slower, provenance-verifying review) are not banned, just structurally starved of the time budget needed to use them — this is soft suppression via resource scarcity, not coercive exclusion. Theater ratio rising to 0.40 reflects growing institutional rhetoric ('rigorous peer review,' 'holistic evaluation') that increasingly outpaces the actual verification effort being spent. Accessibility collapse is moderate (0.45): contributors can in principle seek venues with slower, more provenance-sensitive review, but such venues are shrinking relative to high-volume ones. Resistance (0.55) reflects active pushback from authorship-ethics advocates and provenance researchers, which keeps this well short of mountain-level naturalization.
 *
 * PERSPECTIVAL GAP:
 *   From the evaluator's seat, this looks like efficient triage under real constraints — a rope, coordination that lets a system function at scale. From the illegible-contributor's seat, the identical structure looks like systematic devaluation of real labor for reasons unrelated to merit — closer to tangled-rope or even snare-adjacent extraction. The engine should compute these different per-seat readings from the same structural data; neither seat is wrong about its own experience, and the divergence itself is the signal that the coordination function has drifted toward asymmetric cost-bearing as volume scaled.
 *
 * DIRECTIONALITY LOGIC:
 *   High-throughput evaluators and volume-optimizing institutions sit near the beneficiary end: the proxy directly reduces their cost per artifact and is not something forced on them from outside. Individual contributors with illegible authorship sit near the target end: their exit options are constrained (opting out of evaluated channels forfeits credentialing), and the cost lands on them through no fault correlated with their actual work quality. Polished single-author submitters are beneficiaries by incidental fit rather than by strategic action — they did not create the proxy but are not harmed by it either, which is why they are listed separately from the institutions that set it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (evaluators cannot individually verify everything under real time constraints) remains genuinely live — this is not a zombie mandate. What has drifted is the SCOPE of what the proxy is asked to substitute for: originally a triage heuristic for initial screening, it has in many venues become the terminal quality signal, with provenance verification never happening downstream either. This is mandatrophy-adjacent but not full mandatrophy: the founding problem hasn't disappeared, but the proxy's role has expanded well past what the founding problem required, absorbing verification functions it was never designed to perform.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proxy_legitimacy_scope_creep,
    'Is legibility-as-quality-proxy still confined to initial triage (legitimate coordination), or has it become the terminal evaluation in most high-volume venues (extraction dressed as coordination)?',
    'Audit a sample of high-throughput venues for whether any provenance-verification step occurs downstream of the initial legibility-based triage, and at what rate flagged-but-illegible artifacts receive deeper review versus outright rejection.',
    'If provenance verification genuinely occurs downstream at meaningful rates, the rope reading holds with the proxy functioning as intended triage. If legibility is functionally terminal in most cases, the constraint has drifted to tangled_rope, with institutions collecting throughput benefits while a stable victim class absorbs the cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_legitimacy_scope_creep, empirical, 'Whether the legibility proxy remains bounded triage or has become de facto terminal evaluation.').

omega_variable(
    correlation_causation_smoothness_quality,
    'Does surface smoothness correlate with quality because smoothing genuinely improves communicability of good work, or because the correlation is spurious and driven by confounds (access to editing resources, native-language fluency, institutional pedigree)?',
    'Controlled studies comparing blind-provenance evaluation outcomes to standard evaluation outcomes on matched artifacts, isolating whether smoothness predicts independently-verified quality or merely predicts evaluator score.',
    'If smoothness independently predicts quality, part of the extractiveness reading overstates the harm — the proxy is doing real epistemic work. If the correlation is substantially confound-driven, the extraction reading is understated and the rope claim is weaker than authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(correlation_causation_smoothness_quality, empirical, 'Whether the smoothness-quality correlation is causal or confounded by unequal access to editing/fluency resources.').

omega_variable(
    infrastructure_investment_counterfactual,
    'Would evaluators and institutions actually adopt provenance-verification infrastructure if it were built and cheaply available, or does the time-constraint framing function partly as a cover story for a preference to avoid the harder work of verification regardless of cost?',
    'Track adoption rates of existing low-cost provenance tools (version-history requirements, contribution statements, CRediT-style taxonomies) where they have already been introduced, and measure whether adoption is resisted even after cost is reduced.',
    'High adoption where available would support the rope/genuine-constraint reading (time really is the binding constraint). Low adoption even when cheap would suggest the throughput benefit itself, not merely the time cost, is what evaluators and institutions are optimizing for — shifting the classification toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_investment_counterfactual, conceptual, 'Whether time-scarcity is the true binding constraint or partly a cover story for throughput-maximizing preference.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(evaluator_incentive_asymmetry, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eval_tr_t0, evaluator_incentive_asymmetry, theater_ratio, 0, 0.22).
narrative_ontology:measurement(eval_tr_t4, evaluator_incentive_asymmetry, theater_ratio, 4, 0.26).
narrative_ontology:measurement(eval_tr_t8, evaluator_incentive_asymmetry, theater_ratio, 8, 0.3).
narrative_ontology:measurement(eval_tr_t12, evaluator_incentive_asymmetry, theater_ratio, 12, 0.33).
narrative_ontology:measurement(eval_tr_t16, evaluator_incentive_asymmetry, theater_ratio, 16, 0.36).
narrative_ontology:measurement(eval_tr_t20, evaluator_incentive_asymmetry, theater_ratio, 20, 0.38).
narrative_ontology:measurement(eval_tr_t24, evaluator_incentive_asymmetry, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(eval_be_t0, evaluator_incentive_asymmetry, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(eval_be_t4, evaluator_incentive_asymmetry, base_extractiveness, 4, 0.46).
narrative_ontology:measurement(eval_be_t8, evaluator_incentive_asymmetry, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(eval_be_t12, evaluator_incentive_asymmetry, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(eval_be_t16, evaluator_incentive_asymmetry, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(eval_be_t20, evaluator_incentive_asymmetry, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(eval_be_t24, evaluator_incentive_asymmetry, base_extractiveness, 24, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(evaluator_incentive_asymmetry, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(evaluator_incentive_asymmetry, resource_allocation).
narrative_ontology:boltzmann_floor_override(evaluator_incentive_asymmetry, 0.12).
narrative_ontology:affects_constraint(evaluator_incentive_asymmetry, legibility_primacy_reading).
narrative_ontology:affects_constraint(evaluator_incentive_asymmetry, authorial_primacy_reading).
narrative_ontology:affects_constraint(evaluator_incentive_asymmetry, process_transparency_reading).

% DUAL FORMULATION NOTE:
% This story models the shared mechanical substrate (evaluator time/attention scarcity mechanically favoring legible surface over verified provenance) that all three kernel readings of cooperative_artifact_legitimacy presuppose. It is not itself a reading of the kernel — it is the structural precondition the kernel's dispute is about. Each reading should be authored as its own constraint story with its own ε (legibility_primacy likely near-rope with lower extraction since it treats the trade-off as legitimate; authorial_primacy likely frames the identical mechanism as substantially more extractive since it treats erasure as fraud; process_transparency is scaffold-shaped, attempting to convert the tangled tension into an auditable side-channel). All three should link back to this constraint via affects_constraints, and this constraint links forward to all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
