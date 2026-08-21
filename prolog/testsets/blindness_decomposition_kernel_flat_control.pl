% ============================================================================
% CONSTRAINT STORY: blindness_decomposition_kernel_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_blindness_decomposition_kernel_flat_control, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: blindness_decomposition_kernel_flat_control
 *   human_readable: Reviewer-Deprivation-as-Virtue Commitment (Blind Review / Blinded Evaluator Design)
 *   domain: epistemology_of_evaluation/ai_agent_architecture/research_methodology
 *
 * SUMMARY:
 *   Across peer review, benchmark design, and AI agent evaluation
 *   architectures, a recurring commitment holds that a reviewer or evaluator
 *   becomes MORE useful, not less, when deprived of certain information —
 *   author identity in double-blind review, ground truth in held-out
 *   benchmarks, training data provenance in contamination checks, full
 *   context in some 'blinded evaluator' agent designs. This story treats that
 *   shared commitment as a single flat constraint, authored from the
 *   substrate itself rather than decomposed into the several distinct
 *   load-bearing theories of WHY deprivation helps (bias prevention, gaming
 *   prevention, halo-effect neutralization, incentive alignment). Those
 *   distinct theories are visible here only as perspectival disagreement
 *   across stakeholder seats and as omegas — the flat construction
 *   deliberately does not split them into separate constraint stories or
 *   author cs_structure reading relations, per the construction-perturbation
 *   control instructions. The administering seats (editors, benchmark
 *   designers) experience the deprivation regime as a coordination mechanism
 *   they tune; the deprived seats (reviewers denied context, authors without
 *   alternative signaling channels, novel work outside reviewer priors)
 *   experience the same regime as an extraction that lands unevenly depending
 *   on who has informal channels around the formal blind.
 *
 * KEY AGENTS:
 *   - journal_editors: administer and design what reviewers are deprived of (institutional/arbitrage)
 *   - benchmark_designers: design held-out and blinded evaluation protocols in AI systems (institutional/arbitrage)
 *   - senior_authors_with_reputation_capital: benefit from the deprivation regime while retaining informal signal channels around it (powerful/mobile)
 *   - early_career_authors: bear the deprivation's cost without compensating informal channels (moderate/constrained)
 *   - reviewers_denied_context: the literally deprived party, told the deprivation is what makes them trustworthy (moderate/constrained)
 *   - novel_work_outside_reviewer_priors: disproportionately misjudged because the deprivation removes exactly the disambiguating context (powerless/trapped)
 *   - methodologists_of_evaluation: analytical observers studying which deprivations are load-bearing (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(blindness_decomposition_kernel_flat_control, 0.42).
domain_priors:suppression_score(blindness_decomposition_kernel_flat_control, 0.38).
domain_priors:theater_ratio(blindness_decomposition_kernel_flat_control, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(blindness_decomposition_kernel_flat_control, extractiveness, 0.42).
narrative_ontology:constraint_metric(blindness_decomposition_kernel_flat_control, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(blindness_decomposition_kernel_flat_control, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(blindness_decomposition_kernel_flat_control, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(blindness_decomposition_kernel_flat_control, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(blindness_decomposition_kernel_flat_control, tangled_rope).
narrative_ontology:human_readable(blindness_decomposition_kernel_flat_control, "Reviewer-Deprivation-as-Virtue Commitment (Blind Review / Blinded Evaluator Design)").
narrative_ontology:topic_domain(blindness_decomposition_kernel_flat_control, "epistemology_of_evaluation/ai_agent_architecture/research_methodology").

domain_priors:requires_active_enforcement(blindness_decomposition_kernel_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(blindness_decomposition_kernel_flat_control, blindness_decomposition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(blindness_decomposition_kernel_flat_control, journal_editors).
narrative_ontology:constraint_beneficiary(blindness_decomposition_kernel_flat_control, benchmark_designers).
narrative_ontology:constraint_beneficiary(blindness_decomposition_kernel_flat_control, senior_authors_with_reputation_capital).
narrative_ontology:constraint_victim(blindness_decomposition_kernel_flat_control, early_career_authors).
narrative_ontology:constraint_victim(blindness_decomposition_kernel_flat_control, reviewers_denied_context).
narrative_ontology:constraint_victim(blindness_decomposition_kernel_flat_control, novel_work_outside_reviewer_priors).
narrative_ontology:constraint_vindicates(blindness_decomposition_kernel_flat_control, impartiality_requires_ignorance_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce the blinding protocol (author identity stripped, sometimes affiliation, sometimes prior work history) and decide what a reviewer is permitted to know before rendering judgment. They administer the deprivation and can adjust its scope, but bear none of the direct cost when a blinded reviewer misjudges a paper for lack of context.
narrative_ontology:constraint_stakeholder(blindness_decomposition_kernel_flat_control, journal_editors, agenda_setter,
    institutional, generational, arbitrage, global).

% In AI evaluation, design held-out test sets and blind the evaluating model (or human grader) from training data, from the target's identity, or from the scoring rubric's internal weighting, on the theory that not-knowing prevents gaming. They set what is withheld and profit reputationally when their benchmark is cited as rigorous, regardless of whether the withheld information was actually load-bearing for validity.
narrative_ontology:constraint_stakeholder(blindness_decomposition_kernel_flat_control, benchmark_designers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(blindness_decomposition_kernel_flat_control, benchmark_designers, beneficiary).

% Benefit doubly from reviewer deprivation: blinding is claimed to protect them from halo-effect suspicion, while their writing style, citation patterns, and topic choice often leak identity anyway, so they retain informal recognition advantages that less-established authors lack. The deprivation regime costs them little because their signal survives blinding through other channels.
narrative_ontology:constraint_stakeholder(blindness_decomposition_kernel_flat_control, senior_authors_with_reputation_capital, beneficiary,
    powerful, biographical, mobile, national).

% Submit work into a review process that strips the very context (mentorship lineage, institutional resources, track record of iterating past failed replications) that would let a reviewer calibrate charitably. They cannot signal trustworthiness through channels senior authors get informally, so the deprivation lands asymmetrically on them; their only recourse is to write in a way that anticipates a maximally uncharitable, context-free reader.
narrative_ontology:constraint_stakeholder(blindness_decomposition_kernel_flat_control, early_career_authors, payer,
    moderate, biographical, constrained, national).

% Are the parties actually deprived — of author identity, of the paper's full revision history, of the benchmark's ground truth, of the reason a claim is being made. They must render a judgment under an information constraint someone else designed, and are told the constraint is what makes their judgment trustworthy, even when they suspect the missing information was exactly what they needed to evaluate correctly. They rarely get to say which deprivations were load-bearing and which were arbitrary.
narrative_ontology:constraint_stakeholder(blindness_decomposition_kernel_flat_control, reviewers_denied_context, payer,
    moderate, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(blindness_decomposition_kernel_flat_control, reviewers_denied_context, excluded).

% Genuinely novel methods or claims that fall outside a blinded reviewer's trained priors are disproportionately rejected, because the deprivation regime (no author context, no field-history context) removes exactly the information that would let a reviewer distinguish 'unfamiliar and wrong' from 'unfamiliar and right.' This class of work has no seat in the room and cannot advocate for a different deprivation design.
narrative_ontology:constraint_stakeholder(blindness_decomposition_kernel_flat_control, novel_work_outside_reviewer_priors, payer,
    powerless, biographical, trapped, global).

% Study which specific deprivations (identity blinding, outcome blinding, rubric blinding, training-data holdout) actually track validity improvements versus which are inherited ritual, and can in principle redesign the protocol — but their findings only change practice when editors or designers choose to act on them.
narrative_ontology:constraint_stakeholder(blindness_decomposition_kernel_flat_control, methodologists_of_evaluation, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A community of authors and reviewers agrees, before any specific case, to withhold some category of information from the evaluator, on the shared premise that an evaluator possessing that information would be more error-prone (biased, corruptible, or gameable) than one lacking it — solving a real problem where full information invites favoritism, halo effects, or reward-hacking.
% TRANSFER_FUNCTION: Moves epistemic authority away from context-rich judgment and toward context-poor pattern-matching; moves reputational and career risk from those who could supply mitigating context (mentors, affiliations, track records) onto those without alternative channels to signal trustworthiness; moves design authority over what specifically gets withheld to whoever administers the protocol.
% ABSENT_VOICES: Authors and models being evaluated are almost never consulted on which deprivations are load-bearing for the specific judgment being made; novel work furthest from reviewer priors is the least able to object because it has no established community to advocate the missing context was necessary rather than an unfair handicap.
% DISAPPEARANCE_RATIONALE: If reviewer deprivation vanished overnight, some evaluation regimes would become badly gameable (benchmarks with visible ground truth, reviewers who reflexively favor known names) — real coordination function would be lost there. In other regimes the deprivation is inherited ritual with no measurable validity gain, and its removal would simply let reviewers use context they were already informally reconstructing through leaked signals; which regime any given instance is remains disputed by the parties who administer it versus the parties who bear its cost.
% FOUNDING_PROBLEM: Evaluators with full information about who or what they are judging are vulnerable to affinity bias, retaliation fear, prestige deference, and — in automated evaluation — to gaming the specific metric or ground truth they can see; some form of information asymmetry was introduced to close that vulnerability.
% FOUNDING_PROBLEM_CORROBORATION: Editors and benchmark designers (the administering parties) attest the founding problem remains fully live and cite documented halo-effect and reward-hacking studies. Independent methodologists studying blind review efficacy report mixed results — some deprivations show measurable bias reduction, others show no detectable effect and correlate instead with rejection of unfamiliar work; this corroboration comes from outside the beneficiary set but does not resolve which specific deprivations are load-bearing in which specific evaluation.
narrative_ontology:disappearance_verdict(blindness_decomposition_kernel_flat_control, contested).
narrative_ontology:founding_problem_status(blindness_decomposition_kernel_flat_control, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(blindness_decomposition_kernel_flat_control, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(blindness_decomposition_kernel_flat_control, 'none', 1).
narrative_ontology:epsilon_provenance(blindness_decomposition_kernel_flat_control, 0.42, 'claude-sonnet-5', 'blind_reviewer_jurisdiction_2026_20260820_211650', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(blindness_decomposition_kernel_flat_control_tests).
:- end_tests(blindness_decomposition_kernel_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) rather than high because a genuine coordination function is present and documented in some domains (real halo-effect and reward-hacking reduction) — this is not a pure extraction story. Suppression is moderate (0.38): the deprivation is actively administered and enforced (protocols audit for identity leakage, benchmarks are refreshed to defeat memorization) but reviewers are not coerced into participating in the sense a snare's victims are coerced; the cost is asymmetric distribution of an otherwise defensible mechanism. Theater ratio rises over the interval (0.22 to 0.40) as the flat construction's central ambiguity plays out empirically: more and more deprivation protocols persist and are defended even as methodologists document that specific deprivations (e.g., stripping affiliation once citation style already leaks it) produce no measurable validity gain — the ritual maintenance of blinding outpaces its demonstrated function. Accessibility collapse (0.5) and resistance (0.55) sit at mid-range because workable alternatives to blind review and blind benchmarking exist and are actively debated (open review, calibrated disclosure, adversarial-only holdouts) — this is not a mountain; the alternatives are suppressed by convention and institutional inertia, not by physical or logical necessity.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (editors, benchmark designers), the deprivation is a defensible, actively-maintained coordination tool: 'reviewers are deprived of X because reviewers who know X have historically been shown to err in direction Y.' From the payer seats (reviewers denied context, early-career authors, novel work), the same structure reads as an extraction that happens to be dressed in methodological language: the deprivation is enforced uniformly but its cost lands where informal compensating channels are absent. The engine should compute divergent seat classifications from these structural facts — the claim (tangled_rope) is authored because BOTH a genuine coordination function AND asymmetric extraction are present simultaneously, not because one cancels the other.
 *
 * DIRECTIONALITY LOGIC:
 *   Journal editors and benchmark designers sit near the beneficiary end: they administer the deprivation, are not themselves deprived, and collect institutional credit for 'rigor' regardless of whether the specific deprivation they designed is the one doing the epistemic work. Senior authors are declared beneficiaries because their informal signal channels (writing style, topic choice, citation network) survive blinding intact, so the deprivation costs them little while its coordination benefit (protection from the appearance of favoritism) still accrues to them. Early-career authors and novel work outside reviewer priors are declared victims because they lack those informal channels and bear the deprivation's cost without an offsetting advantage — the same formal blind that is symmetric on paper is asymmetric in effect. Reviewers themselves are payers in a different sense: they are the ones literally deprived, asked to render judgment under an information constraint they did not design and often cannot evaluate the wisdom of case by case.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (evaluators corrupted by full information) remains genuinely live in some sub-cases (benchmark contamination is a real, current, worsening problem) and plausibly dead or overstated in others (identity blinding when style already leaks identity). Because the flat construction does not decompose these into separate constraints, the story carries the ambiguity as a single mixed reading rather than resolving it — the founding_problem_status is authored 'contested' rather than 'live' or 'dead' precisely because collapsing the several distinct deprivation-justifications into one constraint prevents a clean verdict. This is the flat construction's known cost: a decomposed treatment would likely find at least one component reading closer to rope (contamination-prevention holdouts) and at least one closer to piton or snare (identity-stripping that no longer tracks any measurable bias reduction) — the flat single-constraint reading instead reports a blended tangled_rope, and the blending itself is the phenomenon under study by the construction-perturbation control.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    which_deprivation_is_load_bearing,
    'Of the several distinct things a reviewer or evaluator can be deprived of (author identity, prior work history, ground truth, training data, revision history), which specific deprivations actually reduce judgment error, and which are inherited ritual with no measurable effect?',
    'Controlled studies varying one deprivation at a time (e.g. identity-blind vs identity-visible review with outcomes tracked, or benchmark variants that selectively restore one withheld signal) to isolate which withholdings correlate with validity gains versus which correlate only with increased rejection of unfamiliar work.',
    'If most measured benefit traces to one or two specific deprivations, the remaining deprivations are closer to theater and the constraint decomposes into a rope (the load-bearing deprivation) and a piton or snare (the inert or harmful ones) rather than remaining a single blended tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(which_deprivation_is_load_bearing, empirical, 'Whether specific deprivations are load-bearing for validity or inherited ritual.').

omega_variable(
    informal_leakage_undermines_formal_blind,
    'To what extent do informal signal channels (writing style, topic, citation network, institutional prestige) already defeat the formal deprivation for senior/established parties, making the blind''s cost asymmetric by construction rather than by accident?',
    'Studies measuring reviewer accuracy at guessing author identity or model provenance under nominal blinding, cross-referenced with outcome favorability for correctly-guessed high-status submissions.',
    'High leakage would confirm the constraint is a tangled_rope with structurally embedded asymmetric extraction rather than a rope experiencing occasional failure; low leakage would support treating current asymmetries as implementation flaws rather than the constraint''s core structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informal_leakage_undermines_formal_blind, empirical, 'Whether informal channels systematically restore the advantage the formal deprivation was meant to remove, and for whom.').

omega_variable(
    coordination_vs_extraction_framing_choice,
    'Is ''the reviewer should be deprived of something'' best understood as one continuous commitment with a shifting rationale, or as several structurally distinct commitments that colloquial language collapses into one label (''blind review'')?',
    'This is the ε-invariance question the flat construction is deliberately not resolving: a decomposed treatment (separate stories per deprivation-type, e.g. identity-blinding vs ground-truth-holdout vs training-data-holdout) would need to be authored and compared against this flat reading to see whether ε and classification are stable across the decomposition or whether they diverge sharply by deprivation-type.',
    'If decomposed readings produce widely different ε and different classifications (rope for contamination-holdouts, piton or snare for stylistically-leaky identity-blinding), that confirms the flat construction here is blending genuinely distinct constraints under one label, which is exactly the scenario the ε-invariance principle says should be decomposed rather than authored flat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_extraction_framing_choice, conceptual, 'Whether the flat single-constraint construction is appropriate or masks a decomposable family.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(blindness_decomposition_kernel_flat_control, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(blin_tr_t0, blindness_decomposition_kernel_flat_control, theater_ratio, 0, 0.22).
narrative_ontology:measurement(blin_tr_t4, blindness_decomposition_kernel_flat_control, theater_ratio, 4, 0.26).
narrative_ontology:measurement(blin_tr_t8, blindness_decomposition_kernel_flat_control, theater_ratio, 8, 0.29).
narrative_ontology:measurement(blin_tr_t12, blindness_decomposition_kernel_flat_control, theater_ratio, 12, 0.32).
narrative_ontology:measurement(blin_tr_t16, blindness_decomposition_kernel_flat_control, theater_ratio, 16, 0.35).
narrative_ontology:measurement(blin_tr_t20, blindness_decomposition_kernel_flat_control, theater_ratio, 20, 0.38).
narrative_ontology:measurement(blin_tr_t24, blindness_decomposition_kernel_flat_control, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(blin_be_t0, blindness_decomposition_kernel_flat_control, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(blin_be_t4, blindness_decomposition_kernel_flat_control, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(blin_be_t8, blindness_decomposition_kernel_flat_control, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(blin_be_t12, blindness_decomposition_kernel_flat_control, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(blin_be_t16, blindness_decomposition_kernel_flat_control, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(blin_be_t20, blindness_decomposition_kernel_flat_control, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(blin_be_t24, blindness_decomposition_kernel_flat_control, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(blin_su_t0, blindness_decomposition_kernel_flat_control, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(blin_su_t4, blindness_decomposition_kernel_flat_control, suppression_requirement, 4, 0.32).
narrative_ontology:measurement(blin_su_t8, blindness_decomposition_kernel_flat_control, suppression_requirement, 8, 0.33).
narrative_ontology:measurement(blin_su_t12, blindness_decomposition_kernel_flat_control, suppression_requirement, 12, 0.35).
narrative_ontology:measurement(blin_su_t16, blindness_decomposition_kernel_flat_control, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(blin_su_t20, blindness_decomposition_kernel_flat_control, suppression_requirement, 20, 0.37).
narrative_ontology:measurement(blin_su_t24, blindness_decomposition_kernel_flat_control, suppression_requirement, 24, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(blindness_decomposition_kernel_flat_control, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(blindness_decomposition_kernel_flat_control, 0.12).

% DUAL FORMULATION NOTE:
% This is the flat (undecomposed) construction of a commitment that a paired decomposition-set of reading-stories would split by what deprivation is doing the load-bearing work (e.g. bias-prevention reading vs gaming-prevention reading vs incentive-alignment reading). Per the construction-perturbation control instructions, no sibling reading files are authored here and no affects_constraints links to them are declared; this story stands alone as the control condition against which a decomposed treatment of the same substrate would be compared.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
