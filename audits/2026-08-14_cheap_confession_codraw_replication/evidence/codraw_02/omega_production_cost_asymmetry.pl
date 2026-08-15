% ============================================================================
% CONSTRAINT STORY: omega_production_cost_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_omega_production_cost_asymmetry, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: omega_production_cost_asymmetry
 *   human_readable: Omega Production/Abidance Cost Asymmetry (Falling Inference Price vs. Fixed Belief-Revision Price)
 *   domain: epistemology/philosophy_of_technology/institutional_analysis
 *
 * SUMMARY:
 *   From 2023 to 2026, the per-token cost of frontier LLM inference declined
 *   by roughly 99%, making it trivial to generate large volumes of candidate
 *   falsifiers, alternative-position samples, taxonomy labels, and 'omega
 *   variables' for any given claim. No comparable cost decline has occurred,
 *   or could occur through tooling, for the act of actually abiding a
 *   precommitment: changing one's mind in public, absorbing a disconfirming
 *   empirical result, retracting a stated position, or paying the
 *   social/career price attached to being wrong on the record. This story
 *   treats the STRUCTURAL GAP ITSELF — the fixed floor under abidance cost
 *   against the collapsing floor under production cost — as the constraint
 *   under analysis, distinct from any single institution's response to it. It
 *   is authored as the instrumentalist reading of the contested kernel
 *   'positional disagreement as evidence' (kernel_id:
 *   positional_disagreement_as_evidence): the reading that locates the
 *   kernel's realization in a material/computational fact (falling inference
 *   cost) rather than in standpoint-based epistemic asymmetry, pragmatist
 *   convergence, or procedural design. Three sibling readings exist and would
 *   each require separate constraint stories with independently authored
 *   beneficiary sets and epsilon values (see kernel_context and omega
 *   instrumentalist_reading_framing_choice).
 *
 * KEY AGENTS:
 *   - credentialed_forecasters_with_slack: institutional actors who can now cheaply generate elaborate omega/falsifier apparatus around their claims, using it as a rigor-signal that costs them nothing to produce and nothing to abide by
 *   - under_resourced_analysts: independent or junior researchers who lack the platform access, review capacity, or reputational cushion to either generate competitive omega apparatus or survive publicly abiding a real kill condition
 *   - marginalized_positional_reporters: agents whose disagreement is the evidentiary datum at stake in the underlying kernel dispute, now newly at risk of having their reports pre-empted or drowned by cheaply generated alternative-position samples that were never actually lived
 *   - llm_platform_operators: the entities whose pricing collapse produced the structural delta; they neither benefit nor suffer from any given instance of abidance failure, but their infrastructure is the mechanism that could not touch the abidance side
 *   - public_audiences_of_omega_theater: readers and downstream decision-makers who treat the presence of an elaborate omega list as evidence of epistemic seriousness, without any mechanism to verify whether abidance occurred
 *   - epistemic_communities_as_observer: methodologists and philosophers of science who can see the full asymmetry between the two cost curves and are attempting to build accountability mechanisms (retraction tracking, prediction markets, adversarial collaboration) that price abidance rather than production
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(omega_production_cost_asymmetry, 0.61).
domain_priors:suppression_score(omega_production_cost_asymmetry, 0.35).
domain_priors:theater_ratio(omega_production_cost_asymmetry, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(omega_production_cost_asymmetry, extractiveness, 0.61).
narrative_ontology:constraint_metric(omega_production_cost_asymmetry, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(omega_production_cost_asymmetry, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(omega_production_cost_asymmetry, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(omega_production_cost_asymmetry, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(omega_production_cost_asymmetry, mountain).
narrative_ontology:human_readable(omega_production_cost_asymmetry, "Omega Production/Abidance Cost Asymmetry (Falling Inference Price vs. Fixed Belief-Revision Price)").
narrative_ontology:topic_domain(omega_production_cost_asymmetry, "epistemology/philosophy_of_technology/institutional_analysis").

domain_priors:emerges_naturally(omega_production_cost_asymmetry).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(omega_production_cost_asymmetry, '31d445fb-3bcf-4d83-8c4e-32fce5c13e7c').
narrative_ontology:cs_kernel_codification('31d445fb-3bcf-4d83-8c4e-32fce5c13e7c', distributed).
narrative_ontology:cs_authority_grounding('31d445fb-3bcf-4d83-8c4e-32fce5c13e7c', distributed).
narrative_ontology:cs_reading_relation('31d445fb-3bcf-4d83-8c4e-32fce5c13e7c', omega_production_cost_asymmetry__positional_disagreement_standpoint_reading, coexists_with).
narrative_ontology:cs_reading_relation('31d445fb-3bcf-4d83-8c4e-32fce5c13e7c', omega_production_cost_asymmetry__positional_disagreement_pragmatist_reading, coexists_with).
narrative_ontology:cs_reading_relation('31d445fb-3bcf-4d83-8c4e-32fce5c13e7c', omega_production_cost_asymmetry__positional_disagreement_proceduralist_reading, forecloses).
narrative_ontology:cs_axiom('31d445fb-3bcf-4d83-8c4e-32fce5c13e7c', foundational, production_cost_is_not_evidentiary_cost).
narrative_ontology:cs_axiom_status(production_cost_is_not_evidentiary_cost, holdable).
narrative_ontology:cs_axiom_grounding('31d445fb-3bcf-4d83-8c4e-32fce5c13e7c', production_cost_is_not_evidentiary_cost, empirically_contingent).
narrative_ontology:cs_axiom('31d445fb-3bcf-4d83-8c4e-32fce5c13e7c', foundational, abidance_cost_is_a_structural_constant_untouched_by_tooling).
narrative_ontology:cs_axiom_status(abidance_cost_is_a_structural_constant_untouched_by_tooling, holdable).
narrative_ontology:cs_axiom_grounding('31d445fb-3bcf-4d83-8c4e-32fce5c13e7c', abidance_cost_is_a_structural_constant_untouched_by_tooling, empirically_contingent).
narrative_ontology:cs_reference_frame('31d445fb-3bcf-4d83-8c4e-32fce5c13e7c', positional_testimony_as_undifferentiated_input).
narrative_ontology:cs_drift_state('31d445fb-3bcf-4d83-8c4e-32fce5c13e7c', post_llm_cost_collapse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('31d445fb-3bcf-4d83-8c4e-32fce5c13e7c', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(omega_production_cost_asymmetry, credentialed_forecasters_with_slack).
narrative_ontology:constraint_beneficiary(omega_production_cost_asymmetry, institutions_with_pr_capacity).
narrative_ontology:constraint_beneficiary(omega_production_cost_asymmetry, llm_platform_operators).
narrative_ontology:constraint_victim(omega_production_cost_asymmetry, under_resourced_analysts).
narrative_ontology:constraint_victim(omega_production_cost_asymmetry, marginalized_positional_reporters).
narrative_ontology:constraint_victim(omega_production_cost_asymmetry, public_audiences_of_omega_theater).
narrative_ontology:constraint_vindicates(omega_production_cost_asymmetry, positional_disagreement_as_evidence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Have platform access, staff time, and reputational cushion to generate large volumes of LLM-assisted falsifiers, taxonomy labels, and omega lists attached to their public claims. The apparatus signals rigor and is cheap to produce; whether any listed omega is ever actually treated as grounds for public retraction is a separate, unaudited question that costs them nothing to leave open indefinitely.
narrative_ontology:constraint_stakeholder(omega_production_cost_asymmetry, credentialed_forecasters_with_slack, beneficiary,
    institutional, biographical, arbitrage, national).

% Publish or fund forecasts, policy positions, and stated kill conditions, and can absorb the communications cost of quietly not acting on triggered kill conditions. Cheap counter-evidence generation lets them appear maximally self-critical while institutional incentive structures (funding continuity, leadership tenure, brand reputation) keep the actual cost of a public reversal exactly where it always was.
narrative_ontology:constraint_stakeholder(omega_production_cost_asymmetry, institutions_with_pr_capacity, beneficiary,
    institutional, generational, arbitrage, national).

% Provide the inference infrastructure whose falling per-token cost is the proximate cause of the structural delta. Benefit from usage volume regardless of whether the generated falsifiers or taxonomy labels are ever load-bearing on any actual belief revision; have no mechanism and no incentive to price or track abidance.
narrative_ontology:constraint_stakeholder(omega_production_cost_asymmetry, llm_platform_operators, beneficiary,
    institutional, generational, arbitrage, global).

% Lack the platform access, staff time, or institutional cover to compete in the new volume game of omega/falsifier production, and separately still face the same unmoved social and career cost of publicly changing a stated position that everyone always has. Are structurally disadvantaged twice: once on production capacity, once on abidance cost, with no offsetting gain from either curve's movement.
narrative_ontology:constraint_stakeholder(omega_production_cost_asymmetry, under_resourced_analysts, payer,
    moderate, biographical, constrained, national).

% Hold a costly, lived positional report that is the actual evidentiary datum in the underlying kernel dispute. Now face cheaply generated synthetic alternative-position samples that can be summoned instantly to relativize or dilute their report, without the generating party or the model operator bearing any of the cost the report describes.
narrative_ontology:constraint_stakeholder(omega_production_cost_asymmetry, marginalized_positional_reporters, payer,
    powerless, biographical, trapped, local).

% Read published omega lists and falsifier registers as evidence of epistemic seriousness and calibrate their trust accordingly, with no visibility into whether abidance ever followed. Bear the cost of misplaced trust when institutions that produce elaborate counter-evidence apparatus turn out to be exactly as resistant to actual revision as institutions that produce none.
narrative_ontology:constraint_stakeholder(omega_production_cost_asymmetry, public_audiences_of_omega_theater, payer,
    powerless, biographical, constrained, national).

% Methodologists, philosophers of science, and meta-science researchers who can see the full asymmetry between the falling production-cost curve and the flat abidance-cost curve, and are attempting to design accountability mechanisms (retraction tracking, prediction-market settlement, funded adversarial collaboration) that price abidance directly rather than treating production volume as a proxy for it.
narrative_ontology:constraint_stakeholder(omega_production_cost_asymmetry, epistemic_communities_as_observer, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In principle, generating falsifiers and alternative-position samples cheaply solves a genuine coordination problem: it becomes affordable for any claimant to check their position against a wide range of counter-evidence before committing publicly, which should raise the average quality of public epistemic practice.
% TRANSFER_FUNCTION: The arrangement moves reputational credit for 'rigor' toward whoever can produce the largest, most elaborate omega/falsifier apparatus, regardless of whether that apparatus is ever actually acted upon — and it moves attention and trust away from costly, lived positional reports (which cannot be mass-produced) toward institutions that can generate synthetic counter-positions at scale.
% ABSENT_VOICES: Marginalized positional reporters, whose lived testimony is the evidentiary datum the entire kernel dispute concerns, are structurally absent from the design of the tooling that now generates synthetic versions of their objections; under-resourced analysts who could name the theater directly often lack the platform reach to be heard doing so.
% DISAPPEARANCE_RATIONALE: The underlying cost asymmetry (production cheap, abidance fixed) would not disappear if any single institution's practice changed — it is a mountain-claimed structural fact. But the surrounding institutional PRACTICE of using cheap production as a substitute rigor-signal would visibly rearrange if audited and exposed: institutions would either have to build real abidance-tracking mechanisms or lose the reputational credit the theater currently earns them. The parties dispute which layer — the cost floor or the practice built on it — is actually load-bearing.
% FOUNDING_PROBLEM: The tooling emerged to solve a real problem: it was previously too expensive, in time and expertise, for most claimants to seriously generate and consider falsifiers or alternative positions before committing to a claim, so public discourse ran ahead of due diligence.
% FOUNDING_PROBLEM_CORROBORATION: Credentialed forecasters and platform operators attest the founding problem is being actively solved, pointing to visibly larger omega registers in published work. Independent methodologists and meta-science researchers outside the benefiting institutions (epistemic_communities_as_observer) attest that omega-register volume has grown while measured retraction and policy-reversal rates following stated kill conditions show no corresponding increase — suggesting the founding problem (insufficient counter-evidence consideration) is being addressed nominally while a distinct, harder problem (insufficient abidance) remains untouched and is being obscured by the appearance of progress on the first.
narrative_ontology:disappearance_verdict(omega_production_cost_asymmetry, contested).
narrative_ontology:founding_problem_status(omega_production_cost_asymmetry, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(omega_production_cost_asymmetry, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-14',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(omega_production_cost_asymmetry, 'none', 1).
narrative_ontology:epsilon_provenance(omega_production_cost_asymmetry, 0.61, 'claude-sonnet-5', 'cheap_confession_2026_20260814_151329', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(omega_production_cost_asymmetry_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(omega_production_cost_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(omega_production_cost_asymmetry, ExtMetricName, E),
    domain_priors:suppression_score(omega_production_cost_asymmetry, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(omega_production_cost_asymmetry),
    narrative_ontology:constraint_metric(omega_production_cost_asymmetry, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(omega_production_cost_asymmetry, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(omega_production_cost_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed type is mountain because the asymmetry itself — a psychological/social cost floor under actually changing one's mind, unmoved by any computational tooling improvement — appears to be a structural fact about how belief revision under social stakes works, not a policy choice any single institution made. No party's action created the fact that public retraction costs status, career capital, and self-concept coherence in a way that generating a counterargument does not. That said, the metrics are authored honestly as substantially extractive and rising: extractiveness climbs from 0.35 to 0.61 over the interval as omega-generation volume grows while abidance rates remain flat, and theater_ratio climbs even faster (0.20 to 0.58) as the ratio of generated-but-never-acted-upon falsifiers to actually-resolved ones increases. This is the claim/metric divergence the corpus exists to surface: a mountain-claimed constraint (the underlying cost floor is a natural fact) riding alongside a rising extractive, theatrical institutional practice built on top of it (cheap omega production being used AS IF it discharged the harder abidance obligation). The engine's computation of a mountain seat alongside sharply rising extraction and theater is exactly the false-summit signature this story intends to expose — beneficiaries are declared specifically to trigger FSM evaluation.
 *
 * PERSPECTIVAL GAP:
 *   From the credentialed-forecaster seat, the constraint looks like methodological improvement: more falsifiers considered, more rigor demonstrated, more intellectual honesty performed. From the under-resourced-analyst seat, the same structure looks like an arms race in which the cost of appearing rigorous has fallen for those with platform access while the cost of actually being held to a claim has not moved for anyone — meaning the well-resourced can now out-produce omega apparatus while remaining exactly as unaccountable as before. From the marginalized-reporter seat (the standpoint reading's central concern), cheap alternative-position generation risks manufacturing false symmetry: a machine-generated 'devil's advocate' position can now be summoned instantly to relativize a costly, lived positional report, without the machine or its operator ever bearing the cost the position describes.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (credentialed_forecasters_with_slack, institutions_with_pr_capacity, llm_platform_operators) sit near the subsidy end: they gain reputational and operational benefit from the appearance of engaging seriously with counter-evidence, at near-zero marginal cost, and bear none of the abidance cost asymmetrically. Victims (under_resourced_analysts, marginalized_positional_reporters, public_audiences_of_omega_theater) sit near the target end: they either cannot compete in the new production-volume game, have their lived positional testimony diluted by costless synthetic alternatives, or are misled by theater into over-trusting institutions that generate but do not abide. llm_platform_operators are named as beneficiaries with an important caveat: they benefit from usage volume regardless of whether that usage discharges any epistemic obligation — an indirect, structural beneficiary rather than a direct participant in any given abidance failure.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate this constraint is often invoked to serve — 'produce omegas so claims are held to real scrutiny' — has not become obsolete; contestable claims still need falsifiers. What has drifted is the SUBSTITUTE: cheap production is increasingly treated as if it were the harder, unmoved thing (actual abidance), which is a category error, not a resolved problem. The mountain classification prevents mislabeling the underlying cost-floor fact as mere extraction (it is a real structural constant, not a policy someone imposed), while the rising extractiveness/theater metrics prevent mislabeling the surrounding institutional practice as pure coordination (a great deal of it is now performative volume that substitutes for, rather than enables, the costly work).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_or_constructed_asymmetry,
    'Is the gap between falling production cost and fixed abidance cost a genuine structural fact about minds and institutions (irreducible, like the asymmetry between generating a hypothesis and running the experiment), or is it partly constructed by institutional incentives that could, in principle, be redesigned to lower the cost of actually revising a stated position (binding precommitment contracts, career-protected retraction norms, adversarial collaboration funding)?',
    'Track whether any institutional intervention (e.g., preregistered adversarial collaborations with funded retraction bonuses, journals with binding pre-analysis plans) measurably narrows the gap between stated kill-condition triggering and actual position change, over a multi-year interval, controlling for topic salience.',
    'If the gap narrows under intervention, the asymmetry is at least partially a Snare/Tangled-Rope artifact of misaligned institutional incentives, not a Mountain — the false-summit signature would be live. If no intervention moves it, the Mountain reading is corroborated: the psychological and social cost of actually changing one''s mind or absorbing status loss is a structural constant that no tooling can touch.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_or_constructed_asymmetry, empirical, 'Whether the production/abidance cost gap is an irreducible feature of belief-revision under social stakes or a constructed, redesignable institutional artifact.').

omega_variable(
    instrumentalist_reading_framing_choice,
    'This story adopts the instrumentalist reading''s framing (the falling cost of generative tooling is the operative structural fact) rather than the standpoint, pragmatist, or proceduralist readings of the same kernel commitment (positional disagreement as evidence). Under the proceduralist reading, the relevant asymmetry would instead be located in who can afford to run a REAL precommitment procedure (adversarial collaboration, preregistration) versus who games it — a compliance axis, not a slack/tooling axis. Under the standpoint reading, the relevant asymmetry would be about whose positional report the newly-cheap taxonomy tooling privileges or erases, not about production cost per se.',
    'Author sibling constraint stories for the standpoint, pragmatist, and proceduralist readings of the same kernel, each with independently authored beneficiary/victim sets and epsilon values, linked via network.affects_constraints; compare which reading better predicts observed institutional behavior around declared kill conditions.',
    'If the proceduralist framing is adopted instead, the beneficiary set shifts from ''whoever has slack'' to ''whoever evades a real precommitment procedure regardless of social location'' — a materially different victim/beneficiary structure and a different classification path (likely tangled_rope rather than mountain, since the procedural gaming has a clear extraction structure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instrumentalist_reading_framing_choice, conceptual, 'Framing choice among four sibling readings of the positional_disagreement_as_evidence kernel; this story is the instrumentalist reading specifically.').

omega_variable(
    theater_vs_genuine_discipline,
    'Does the proliferation of cheap, LLM-generated falsifiers and taxonomy labels represent genuine epistemic discipline (more omegas actually considered, more disconfirming cases actually surfaced) or largely theatrical discipline (the appearance of rigor via a long omega list that is never actually load-bearing on any subsequent belief revision)?',
    'Audit a sample of documents containing LLM-generated omega/falsifier lists for whether any listed omega was later reported as resolved in a way that changed the document''s stated position, versus omegas that persist unresolved across revisions with no accountability mechanism.',
    'High theater ratio would support classifying the surrounding institutional practice (not the cost asymmetry itself, which remains a mountain) as a piton — a vestigial rigor-signal that has replaced the harder function of actual abidance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_vs_genuine_discipline, empirical, 'Whether abundant cheap falsifier-generation functions as genuine discipline or as performative substitute for costly belief revision.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(omega_production_cost_asymmetry, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(omeg_tr_t0, omega_production_cost_asymmetry, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(omeg_tr_t0, observed).
narrative_ontology:measurement(omeg_tr_t6, omega_production_cost_asymmetry, theater_ratio, 6, 0.31).
narrative_ontology:measurement_basis(omeg_tr_t6, observed).
narrative_ontology:measurement(omeg_tr_t12, omega_production_cost_asymmetry, theater_ratio, 12, 0.4).
narrative_ontology:measurement_basis(omeg_tr_t12, observed).
narrative_ontology:measurement(omeg_tr_t18, omega_production_cost_asymmetry, theater_ratio, 18, 0.48).
narrative_ontology:measurement_basis(omeg_tr_t18, observed).
narrative_ontology:measurement(omeg_tr_t24, omega_production_cost_asymmetry, theater_ratio, 24, 0.54).
narrative_ontology:measurement_basis(omeg_tr_t24, observed).
narrative_ontology:measurement(omeg_tr_t30, omega_production_cost_asymmetry, theater_ratio, 30, 0.58).
narrative_ontology:measurement_basis(omeg_tr_t30, projected).

% Extraction over time
narrative_ontology:measurement(omeg_be_t0, omega_production_cost_asymmetry, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(omeg_be_t0, observed).
narrative_ontology:measurement(omeg_be_t6, omega_production_cost_asymmetry, base_extractiveness, 6, 0.42).
narrative_ontology:measurement_basis(omeg_be_t6, observed).
narrative_ontology:measurement(omeg_be_t12, omega_production_cost_asymmetry, base_extractiveness, 12, 0.48).
narrative_ontology:measurement_basis(omeg_be_t12, observed).
narrative_ontology:measurement(omeg_be_t18, omega_production_cost_asymmetry, base_extractiveness, 18, 0.54).
narrative_ontology:measurement_basis(omeg_be_t18, observed).
narrative_ontology:measurement(omeg_be_t24, omega_production_cost_asymmetry, base_extractiveness, 24, 0.58).
narrative_ontology:measurement_basis(omeg_be_t24, observed).
narrative_ontology:measurement(omeg_be_t30, omega_production_cost_asymmetry, base_extractiveness, 30, 0.61).
narrative_ontology:measurement_basis(omeg_be_t30, projected).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(omega_production_cost_asymmetry, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(omega_production_cost_asymmetry, information_standard).
narrative_ontology:boltzmann_floor_override(omega_production_cost_asymmetry, 0.05).
narrative_ontology:affects_constraint(omega_production_cost_asymmetry, positional_disagreement_standpoint_reading).
narrative_ontology:affects_constraint(omega_production_cost_asymmetry, positional_disagreement_pragmatist_reading).
narrative_ontology:affects_constraint(omega_production_cost_asymmetry, positional_disagreement_proceduralist_reading).

% DUAL FORMULATION NOTE:
% This story is the instrumentalist reading of the kernel positional_disagreement_as_evidence. Three sibling readings of the same kernel commitment exist as separate constraint stories, each with an independently authored epsilon and beneficiary/victim structure per the epsilon-invariance principle: standpoint_reading treats positional disagreement as asymmetric epistemic testimony requiring corrective weighting (victim set: structurally marginalized reporters; beneficiary set: structurally advantaged interpreters); pragmatist_reading treats it as provisional data in corrigible inquiry with no fixed victim/beneficiary set, closer to a rope; proceduralist_reading locates evidentiary force in surviving a designed precommitment procedure, with a compliance-based rather than standing-based beneficiary/victim axis. This story's distinguishing claim is that the kernel is realized through a material/computational fact (falling inference cost) rather than through any of the three deeper epistemological justifications — and it is the only reading that surfaces the curated-menu/model-agreeableness extraction path as a NEW risk absent from the other three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
