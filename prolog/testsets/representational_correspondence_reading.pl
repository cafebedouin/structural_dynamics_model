% ============================================================================
% CONSTRAINT STORY: representational_correspondence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_representational_correspondence_reading, []).

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
 *   constraint_id: representational_correspondence_reading
 *   human_readable: Persona-as-Population-Proxy Correspondence Claim
 *   domain: AI evaluation infrastructure / simulated-user research methodology
 *
 * SUMMARY:
 *   This story instantiates the representational-correspondence reading of
 *   the persona-as-valid-proxy kernel: legitimacy is grounded in demonstrable
 *   statistical correspondence between the persona agents and the real
 *   population segments they claim to represent — matching marginal
 *   distributions, joint dependencies, and behavioral response patterns. Read
 *   this way, the paper's own reported numbers are the constraint's central
 *   evidence against itself: median pairwise Cohen's kappa across 88 joinable
 *   persona-fidelity fields sits near 0.000 (chance-level agreement),
 *   self-report age-band matching is at chance for two of three models
 *   tested, and the coreset used for calibration addresses only four marginal
 *   distributions (age, region, gender, urbanicity) while 1,290 joint
 *   dimensions remain unaddressed. Under the correspondence reading's own
 *   standard, the infrastructure has not yet earned the correspondence claim
 *   embedded in its framing and name. This is a tangled rope, not a pure
 *   snare: there is a genuine coordination function (cheap simulated
 *   screening in place of costly human-subject research) but it is bundled
 *   with an asymmetric extraction — publication and adoption credit accrue to
 *   the paper's authors and citing researchers while the correspondence gap's
 *   costs land on downstream product teams who over-trust screening results
 *   and on the real population segments whose joint behavior is not actually
 *   captured.
 *
 * KEY AGENTS:
 *   - persona_infrastructure_authors: agenda_setter/beneficiary (institutional/arbitrage) — control framing and reporting of validation metrics
 *   - publishing_venue_and_citing_researchers: beneficiary (organized/mobile) — accrue citation and prestige value from an unaudited correspondence claim
 *   - downstream_product_teams: payer (moderate/constrained) — bear the cost of over-trusting screening results
 *   - real_population_segments_misrepresented: payer (powerless/trapped) — bear consequences of joint-distribution misrepresentation with no voice
 *   - independent_methodologists: observer (analytical/analytical) — read the paper's own numbers against its framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(representational_correspondence_reading, 0.68).
domain_priors:suppression_score(representational_correspondence_reading, 0.42).
domain_priors:theater_ratio(representational_correspondence_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(representational_correspondence_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(representational_correspondence_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(representational_correspondence_reading, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(representational_correspondence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(representational_correspondence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(representational_correspondence_reading, tangled_rope).
narrative_ontology:human_readable(representational_correspondence_reading, "Persona-as-Population-Proxy Correspondence Claim").
narrative_ontology:topic_domain(representational_correspondence_reading, "AI evaluation infrastructure / simulated-user research methodology").

domain_priors:requires_active_enforcement(representational_correspondence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(representational_correspondence_reading, '01b0476d-729e-4729-80f1-f054d884885c').
narrative_ontology:cs_kernel_codification('01b0476d-729e-4729-80f1-f054d884885c', distributed).
narrative_ontology:cs_authority_grounding('01b0476d-729e-4729-80f1-f054d884885c', expertise).
narrative_ontology:cs_interpretation_layer_present('01b0476d-729e-4729-80f1-f054d884885c').
narrative_ontology:cs_reading_relation('01b0476d-729e-4729-80f1-f054d884885c', representational_correspondence_reading__instrumentalist_screening_reading, coexists_with).
narrative_ontology:cs_reading_relation('01b0476d-729e-4729-80f1-f054d884885c', representational_correspondence_reading__behavioral_mechanism_reading, coexists_with).
narrative_ontology:cs_reading_relation('01b0476d-729e-4729-80f1-f054d884885c', representational_correspondence_reading__sociotechnical_risk_reading, influences).
narrative_ontology:cs_axiom('01b0476d-729e-4729-80f1-f054d884885c', foundational, legitimacy_requires_joint_distributional_fidelity).
narrative_ontology:cs_axiom_status(legitimacy_requires_joint_distributional_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('01b0476d-729e-4729-80f1-f054d884885c', legitimacy_requires_joint_distributional_fidelity, empirically_contingent).
narrative_ontology:cs_axiom('01b0476d-729e-4729-80f1-f054d884885c', secondary, marginal_calibration_insufficient_for_correspondence_claim).
narrative_ontology:cs_axiom_status(marginal_calibration_insufficient_for_correspondence_claim, holdable).
narrative_ontology:cs_axiom_grounding('01b0476d-729e-4729-80f1-f054d884885c', marginal_calibration_insufficient_for_correspondence_claim, empirically_contingent).
narrative_ontology:cs_reference_frame('01b0476d-729e-4729-80f1-f054d884885c', peer_reviewed_validation_standard).
narrative_ontology:cs_drift_state('01b0476d-729e-4729-80f1-f054d884885c', post_publication_scrutiny, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('01b0476d-729e-4729-80f1-f054d884885c', '').
narrative_ontology:cs_kernel_id(representational_correspondence_reading, persona_as_valid_proxy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(representational_correspondence_reading, persona_infrastructure_authors).
narrative_ontology:constraint_beneficiary(representational_correspondence_reading, publishing_venue_and_citing_researchers).
narrative_ontology:constraint_victim(representational_correspondence_reading, downstream_product_teams).
narrative_ontology:constraint_victim(representational_correspondence_reading, real_population_segments_misrepresented).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, name, and publish the persona-agent infrastructure, choosing which validation metrics to report and how to frame the coreset's four-marginal calibration as sufficient grounding for the correspondence claim embedded in the framing and naming of the system. Control the narrative around what the near-zero fidelity kappa and chance-level age-band matching mean for the tool's readiness.
narrative_ontology:constraint_stakeholder(representational_correspondence_reading, persona_infrastructure_authors, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(representational_correspondence_reading, persona_infrastructure_authors, beneficiary).

% Accept, publish, and cite the work as an advance in simulated-user research; benefit from the appearance of a validated population-proxy tool existing in the literature, largely without independently re-running the 88-field fidelity comparison or auditing the joint-coverage gap over 1,290 dimensions.
narrative_ontology:constraint_stakeholder(representational_correspondence_reading, publishing_venue_and_citing_researchers, beneficiary,
    organized, biographical, mobile, national).

% Adopt the persona agents as a stand-in for user research to save time and cost, trusting the correspondence framing at face value. Bear the cost when screening decisions, feature prioritization, or risk assessments built on the personas diverge from what the represented population segment would actually do, since the tool's marginal-only calibration and near-zero joint fidelity are not visible in typical usage.
narrative_ontology:constraint_stakeholder(representational_correspondence_reading, downstream_product_teams, payer,
    moderate, immediate, constrained, national).

% The actual demographic and behavioral groups the personas claim to stand in for have no voice in how their joint behavioral patterns are approximated, no mechanism to contest misrepresentation, and bear downstream consequences (mistargeted products, miscalibrated risk models, policy inferences) when the persona's departure from real joint distributions is not visible to those relying on it.
narrative_ontology:constraint_stakeholder(representational_correspondence_reading, real_population_segments_misrepresented, payer,
    powerless, generational, trapped, national).

% Read the paper's own reported statistics (median pairwise Cohen's κ ≈ 0.000 across 88 joinable fidelity fields, chance-level self-report age-band matching for two of three models, four-marginal-only coreset calibration against 1,290 unaddressed joint dimensions) as evidence that the correspondence claim is not yet earned, independent of how the paper frames its own contribution.
narrative_ontology:constraint_stakeholder(representational_correspondence_reading, independent_methodologists, observer,
    analytical, generational, analytical, global).

% Compete for adoption in the same product-screening niche but are not part of the paper's own validation conversation; would raise comparative fidelity or disclosure standards if invited into the evaluation, but the correspondence framing forecloses that comparison by asserting adequacy rather than testing it against alternatives.
narrative_ontology:constraint_stakeholder(representational_correspondence_reading, rival_screening_tool_vendors, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(representational_correspondence_reading, persona_infrastructure_authors).
narrative_ontology:fixing_cost_class(representational_correspondence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, the infrastructure would (if it worked) solve a genuine coordination problem: giving product and research teams a cheap, repeatable substitute for expensive live-human-subject testing, allowing many teams to share a common simulated population rather than each running costly independent studies.
% TRANSFER_FUNCTION: Moves epistemic authority and resource allocation from actual population-segment representation (which would require expensive, ongoing human-subject research) to a low-cost simulated substitute; moves citation credit and downstream trust to the paper's authors and adopting product teams, while moving unquantified risk of misrepresentation onto the real population segments and the teams who rely on the tool without independently re-validating it.
% ABSENT_VOICES: The real population segments the personas claim to model have no seat in the validation process. Rival screening-tool vendors and independent replicators who might contest the adequacy of four-marginal calibration are not part of the paper's own reported evaluation and are effectively excluded from the correspondence claim's certification.
% DISAPPEARANCE_RATIONALE: If the correspondence claim were withdrawn or the infrastructure discredited, downstream product teams currently substituting persona screening for human-subject research would need to revert to costlier real-population testing or explicitly flag persona-based findings as unvalidated; citations and downstream tool-chains built on the assumed validity would need re-auditing.
% FOUNDING_PROBLEM: Human-subject research for evaluating product and policy interventions across population segments is slow and expensive; the infrastructure was built to provide a fast, cheap, repeatable substitute that approximates real population behavior closely enough to screen ideas before committing to costly human studies.
% FOUNDING_PROBLEM_CORROBORATION: The paper's own reported statistics (near-zero fidelity kappa, chance-level age matching, marginal-only coreset calibration) are authored by the infrastructure's own creators, which independent methodologists reading the same numbers treat as evidence the founding problem remains substantially unsolved rather than resolved; no downstream product team or independent replication study is cited as external corroboration that the correspondence claim holds in practice.
narrative_ontology:disappearance_verdict(representational_correspondence_reading, world_rearranges).
narrative_ontology:founding_problem_status(representational_correspondence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(representational_correspondence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-10',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(representational_correspondence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(representational_correspondence_reading, 0.68, 'claude-sonnet-5', 'matraix_persona_simulation_2026_20260810_114056', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(representational_correspondence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(representational_correspondence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(representational_correspondence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 under this reading because the correspondence claim, once measured against the paper's own reported statistics, functions as a rent on epistemic trust: teams pay in downstream miscalibration risk for a validation status the numbers do not support. Suppression is moderate (0.42) rather than high — there is no active coercive suppression of alternatives, but the framing and naming convention (calling the outputs 'personas' rather than 'unvalidated behavioral samples') exerts a soft suppressive pull on scrutiny. Theater ratio is authored high and rising (0.35 to 0.61 across the interval) because an increasing share of the infrastructure's presentation (dashboards, fidelity charts, coreset calibration language) performs validation without the underlying joint-coverage problem being closed — the four-marginal coreset calibration is real work but is being asked to stand in for a much larger claim. Accessibility collapse is authored low (0.35) because alternatives — actual human-subject panels, more conservative claims about the tool's scope — remain readily available and are not suppressed by the infrastructure itself. Resistance is moderate (0.55), reflecting the independent methodologist community's documented pushback against overclaiming in simulated-user research.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (the infrastructure's own authors), this is a legitimate contribution: any papers with imperfect metrics can still represent a step forward for the field, and the four-marginal calibration is presented as a meaningful floor. From the payer seats (downstream product teams, misrepresented population segments), the same structure is experienced as an extraction: a correspondence claim is being purchased with money, adoption effort, or downstream trust that the paper's own numbers do not support. The engine should compute these as structurally different seats rather than reconciling them to a single verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Persona infrastructure authors sit near the full-beneficiary end: they set the terms of what counts as validation, control the framing, and capture publication/citation value regardless of whether the correspondence claim later fails to hold up. Publishing venues and citing researchers sit close behind them, benefiting from the appearance of an established method. Downstream product teams and the real population segments sit near the full-target end: they bear the cost of misplaced trust (product teams through faulty screening decisions; population segments through being modeled inaccurately with no recourse). Independent methodologists are analytical/observer — they neither collect from nor pay into the arrangement, but their reading is the one that makes the extraction visible.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification (rather than snare or mountain) matters because there is a real coordination problem this infrastructure is trying to solve — cheap, repeatable pre-screening in place of expensive human-subject studies is a legitimate need, and treating the whole apparatus as pure extraction would erase that. But calling it a rope (pure coordination, net-beneficial to all participants) would erase the asymmetry: the paper's own reported fidelity numbers show the correspondence claim is not yet earned, and the parties bearing that gap (downstream teams, misrepresented populations) are not the parties capturing the benefit (authors, citing researchers). The founding problem remains partly live (cheap screening infrastructure is still needed) but the specific correspondence claim attached to the current tool is, by the reading's own standard, unresolved — hence founding_problem_status is contested rather than dead: the underlying need persists, but the mechanism deployed to meet it has not yet met the bar its own framing sets.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    correspondence_standard_appropriateness,
    'Is full joint-distribution correspondence (matching all 1,290 dimensions of joint behavioral covariance) the right bar for persona legitimacy, or is this reading imposing a standard the infrastructure never claimed to meet?',
    'Examine the paper''s own stated claims and marketing language: does it explicitly assert population-level joint correspondence, or does it hedge toward a narrower screening-utility claim? A close textual read of the abstract, limitations section, and any product-facing documentation would resolve whether this reading''s standard is the one the artifact actually claims to meet.',
    'If the artifact''s own claims are narrower than full joint correspondence, this reading''s failure verdict may be evaluating the wrong target — pushing weight toward the instrumentalist_screening_reading as the more appropriate lens for the same artifact. If the artifact''s framing and naming (calling outputs ''personas'' representing named population segments) does assert correspondence, this reading''s failure verdict stands as directly damning.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(correspondence_standard_appropriateness, conceptual, 'Whether full joint-distributional correspondence is the standard the artifact actually claims, or a standard imposed by this reading.').

omega_variable(
    kappa_zero_diagnostic_severity,
    'Does a median pairwise Cohen''s kappa of approximately 0.000 across 88 joinable fidelity fields indicate the personas are indistinguishable from random noise on those fields, or does it reflect a measurement artifact (e.g., kappa''s known instability near base-rate extremes, or joinability restrictions that biased the comparable field set toward harder cases)?',
    'Re-run the fidelity comparison with an alternative agreement statistic (e.g., weighted kappa, or raw percent agreement adjusted for base rate) on the same 88 fields, and check whether the near-zero result is an artifact of kappa''s sensitivity to skewed marginals rather than genuine absence of correspondence.',
    'If the near-zero kappa is a genuine signal of no correspondence, this reading''s damning verdict is well-grounded empirically. If it is substantially a statistical artifact, the severity of the correspondence failure is overstated and the tangled_rope classification''s extraction weight should be revised downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kappa_zero_diagnostic_severity, empirical, 'Whether the near-zero pairwise kappa reflects genuine lack of correspondence or a statistical artifact of the chosen metric.').

omega_variable(
    beneficiary_intent_vs_structural_effect,
    'Do the infrastructure''s authors and citing researchers actively benefit from downstream over-trust (i.e., is the gap between claim and evidence exploited knowingly), or is the extraction purely structural — a byproduct of publication incentives that reward strong framing regardless of individual intent?',
    'Compare the paper''s limitations section against its abstract and title framing; interview or survey citing researchers on whether they were aware of the fidelity gap when adopting the tool for downstream use.',
    'If intent-driven, this strengthens the tangled_rope-toward-snare direction (deliberate extraction dressed as coordination). If purely structural (a byproduct of academic publishing incentives that reward strong claims), the classification remains tangled_rope but the enforcement mechanism is diffuse incentive structure rather than deliberate agenda-setting — this would matter for any policy remedy aimed at the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_intent_vs_structural_effect, conceptual, 'Whether the extraction is a deliberate strategy by beneficiaries or a structural byproduct of publication incentives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(representational_correspondence_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(repr_tr_t0, representational_correspondence_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(repr_tr_t4, representational_correspondence_reading, theater_ratio, 4, 0.42).
narrative_ontology:measurement(repr_tr_t8, representational_correspondence_reading, theater_ratio, 8, 0.5).
narrative_ontology:measurement(repr_tr_t12, representational_correspondence_reading, theater_ratio, 12, 0.55).
narrative_ontology:measurement(repr_tr_t16, representational_correspondence_reading, theater_ratio, 16, 0.58).
narrative_ontology:measurement(repr_tr_t20, representational_correspondence_reading, theater_ratio, 20, 0.6).
narrative_ontology:measurement(repr_tr_t24, representational_correspondence_reading, theater_ratio, 24, 0.61).

% Extraction over time
narrative_ontology:measurement(repr_be_t0, representational_correspondence_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(repr_be_t4, representational_correspondence_reading, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(repr_be_t8, representational_correspondence_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(repr_be_t12, representational_correspondence_reading, base_extractiveness, 12, 0.63).
narrative_ontology:measurement(repr_be_t16, representational_correspondence_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(repr_be_t20, representational_correspondence_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(repr_be_t24, representational_correspondence_reading, base_extractiveness, 24, 0.68).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(representational_correspondence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(representational_correspondence_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(representational_correspondence_reading, 0.12).
narrative_ontology:affects_constraint(representational_correspondence_reading, instrumentalist_screening_reading).
narrative_ontology:affects_constraint(representational_correspondence_reading, behavioral_mechanism_reading).
narrative_ontology:affects_constraint(representational_correspondence_reading, sociotechnical_risk_reading).

% DUAL FORMULATION NOTE:
% This story is one of four readings of the persona_as_valid_proxy kernel. Each reading evaluates the same underlying persona-agent artifact against a different legitimacy standard (statistical correspondence, screening usefulness, mechanistic plausibility, downstream harm), producing structurally distinct constraints with different beneficiary/victim structures and different epsilon values under the ε-invariance principle. This file authors only the representational_correspondence_reading; the sibling readings are separate constraint files linked here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
