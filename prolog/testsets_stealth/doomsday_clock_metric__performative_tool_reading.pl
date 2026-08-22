% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__performative_tool_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doomsday_clock_metric__performative_tool_reading, []).

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
 *   constraint_id: doomsday_clock_metric__performative_tool_reading
 *   human_readable: Doomsday Clock Setting as Strategic Mobilization Instrument
 *   domain: science communication / normative epistemology / risk governance
 *
 * SUMMARY:
 *   Since 1947 the Bulletin of the Atomic Scientists has maintained the
 *   Doomsday Clock, moving its minute hand toward or away from midnight to
 *   signal proximity of civilization-scale catastrophe. This story authors
 *   ONE reading of that practice — the performative_tool_reading — under
 *   which the setting is strategically chosen to maximize policy impact and
 *   mobilize collective action, rather than to track measured risk. Under
 *   this reading the arrangement possesses a genuine coordination function
 *   (it concentrates diffuse, hard-to-observe dangers into a legible focal
 *   symbol and synchronizes advocacy calendars) while simultaneously drawing
 *   down a resource it does not own: the accumulated epistemic credibility of
 *   the expert-scientific class, on which every future warning depends. Per
 *   the epsilon-invariance principle, the colloquial label 'the Doomsday
 *   Clock' decomposes into a constraint family: this file authors the
 *   performative reading alone, with epsilon's referent fixed to the standing
 *   strategic-setting arrangement as this reading sees it — never to the
 *   calibrated-index arrangement the objective_index_reading sibling would
 *   endorse. The siblings (objective_index_reading: the setting tracks
 *   synthesized empirical indicators, negligible extraction;
 *   hybrid_legitimacy_reading: irreducible entanglement of judgment and
 *   stakes, intermediate extraction) are separate files linked through
 *   network.affects_constraints. Claim and metrics are authored
 *   independently: the claimed type is what I judge structurally true of this
 *   reading's arrangement; the metric values are what I judge descriptively
 *   true of its operation. Where the engine's per-seat computations diverge
 *   from the claim, that divergence is the datum.
 *
 * KEY AGENTS:
 *   - bulletin_science_and_security_board: Agenda setter (institutional/identity_locked) — administers the setting, chooses framing, collects relevance, donors, and media access
 *   - disarmament_advocacy_organizations: Primary beneficiary (organized/mobile) — builds campaigns and fundraising on the announcement cycle
 *   - legacy_media_outlets: Secondary beneficiary (institutional/mobile) — receives a dependable annual visual story
 *   - quantitative_risk_forecasters: Primary target (moderate/constrained) — their calibrated signals are crowded out and discounted
 *   - future_policy_audiences: Structural target (powerless/trapped) — inherit the spent trust, bear discounted-alarm costs, hold no seat
 *   - scientific_advisory_institutions: Collateral target (institutional/constrained) — supply the personnel and data, absorb the spillover distrust
 *   - clock_calibration_critics: Excluded voice (moderate/mobile) — would demand published criteria and scoring; outside the room
 *   - science_communication_researchers: Analytical observer (analytical/analytical) — traces trust effects and historical drift of setting rationales
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__performative_tool_reading, 0.72).
domain_priors:suppression_score(doomsday_clock_metric__performative_tool_reading, 0.48).
domain_priors:theater_ratio(doomsday_clock_metric__performative_tool_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__performative_tool_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__performative_tool_reading, "Doomsday Clock Setting as Strategic Mobilization Instrument").
narrative_ontology:topic_domain(doomsday_clock_metric__performative_tool_reading, "science communication / normative epistemology / risk governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__performative_tool_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__performative_tool_reading, 'e1054d99-b39a-4972-9c47-1f015e91b0c8').
narrative_ontology:cs_kernel_codification('e1054d99-b39a-4972-9c47-1f015e91b0c8', fixed_text).
narrative_ontology:cs_authority_grounding('e1054d99-b39a-4972-9c47-1f015e91b0c8', extraction).
narrative_ontology:cs_interpretation_layer_present('e1054d99-b39a-4972-9c47-1f015e91b0c8').
narrative_ontology:cs_reading_relation('e1054d99-b39a-4972-9c47-1f015e91b0c8', doomsday_clock_metric__objective_index_reading, influences).
narrative_ontology:cs_reading_relation('e1054d99-b39a-4972-9c47-1f015e91b0c8', doomsday_clock_metric__hybrid_legitimacy_reading, coexists_with).
narrative_ontology:cs_axiom('e1054d99-b39a-4972-9c47-1f015e91b0c8', foundational, policy_impact_governs_clock_setting).
narrative_ontology:cs_axiom_status(policy_impact_governs_clock_setting, holdable).
narrative_ontology:cs_axiom_grounding('e1054d99-b39a-4972-9c47-1f015e91b0c8', policy_impact_governs_clock_setting, instrumental).
narrative_ontology:cs_axiom('e1054d99-b39a-4972-9c47-1f015e91b0c8', secondary, board_discretion_over_published_criteria).
narrative_ontology:cs_axiom_status(board_discretion_over_published_criteria, holdable).
narrative_ontology:cs_axiom_grounding('e1054d99-b39a-4972-9c47-1f015e91b0c8', board_discretion_over_published_criteria, conventional).
narrative_ontology:cs_reference_frame('e1054d99-b39a-4972-9c47-1f015e91b0c8', advocacy_mobilization_instrument).
narrative_ontology:cs_drift_state('e1054d99-b39a-4972-9c47-1f015e91b0c8', post_2020_maximal_setting_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e1054d99-b39a-4972-9c47-1f015e91b0c8', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, bulletin_science_and_security_board).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, disarmament_advocacy_organizations).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, legacy_media_outlets).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, quantitative_risk_forecasters).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, future_policy_audiences).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, scientific_advisory_institutions).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__performative_tool_reading, impact_justified_strategic_communication).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes periodically to weigh testimony from sponsors and advisers and to move the minute hand, publishing an essay justifying each new position and choosing which risks to foreground and how urgently to frame them. Its institutional continuity, donor base, and public standing are bound up with the artifact it governs; handing the setting to a published formula, or retiring the symbol, would dissolve the role its members occupy and the self-conception that comes with it.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, bulletin_science_and_security_board, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__performative_tool_reading, bulletin_science_and_security_board, beneficiary).

% Build campaigns, lobbying pushes, and fundraising appeals around the annual announcement, citing each new setting as evidence of urgency. Participation is voluntary: if the symbol lost public salience they could pivot to other frames, reports, and occasions, though the annual rhythm currently anchors their calendars and messaging.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, disarmament_advocacy_organizations, beneficiary,
    organized, biographical, mobile, global).

% Receive a dependable yearly visual story with built-in stakes — a countdown graphic, an expert quote, a headline number. Coverage decisions are voluntary and attention follows novelty; in quiet years outlets can and do skip it, and nothing binds them to the symbol's framing.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, legacy_media_outlets, beneficiary,
    institutional, immediate, mobile, continental).

% Produce calibrated probabilistic assessments of catastrophic risks and compete for the same finite public attention the announcement commands. Each dramatic reset crowds out their slower, hedged signals and primes audiences to treat quantitative caution as complacency. Publishing elsewhere does not shield their work from the comparison; they cannot opt out of the credibility environment the Clock shapes.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, quantitative_risk_forecasters, payer,
    moderate, biographical, constrained, global).

% Are not yet born, not yet engaged, or too diffuse to organize. They inherit whatever stock of trust in expert warning remains after decades of strategically framed settings, and they bear the cost of discounted alarms — slower responses when warnings turn out to be real — without holding any seat in the deliberations that spend that trust on their behalf.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, future_policy_audiences, payer,
    powerless, civilizational, trapped, global).

% Lend scientists, data, and findings to the Board's deliberations while absorbing the spillover distrust when a setting overshoots what their own published evidence shows. Distancing themselves from the Clock would forfeit collaborative ties and access to the convening; remaining associated taxes the credibility their own advisories depend on.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, scientific_advisory_institutions, payer,
    institutional, generational, constrained, global).

% Science-communication scholars, decision theorists, and former sponsors who argue for published setting criteria, uncertainty bounds, and retrospective scoring of past settings. They sit outside the deliberation that produces each announcement; their critiques surface in journals and opinion pages, where the annual cycle largely absorbs or outlasts them.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, clock_calibration_critics, excluded,
    moderate, biographical, mobile, global).

% Study the Clock as a case in symbolic risk communication: tracing media uptake, trust effects, and the historical drift of setting rationales across seven decades. They collect no proceeds from the setting and bear none of its costs; their output is analysis of the structure the other seats inhabit.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, science_communication_researchers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(doomsday_clock_metric__performative_tool_reading, bulletin_science_and_security_board).
narrative_ontology:fixing_cost_class(doomsday_clock_metric__performative_tool_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates diffuse, hard-to-observe catastrophic risks into a single legible public symbol, giving scattered advocacy groups a shared focal point, a synchronized annual calendar, and a common unit of urgency around which coalitions, coverage, and legislative attention can align.
% TRANSFER_FUNCTION: Moves public attention and epistemic trust: attention flows toward whichever risk the Board foregrounds each year, and accumulated credibility of the expert-scientific class is drawn down and transferred as mobilization fuel to advocacy campaigns — with the repayment obligation landing on whoever needs trusted warnings later.
% ABSENT_VOICES: Quantitative forecasters and calibration-minded communication scholars are outside the setting room; so are the future audiences who will inherit the trust balance. The sponsors, advisers, and partner campaigns invited into the process share an interest in a dramatic number, so the room's unanimity partly reflects who was admitted rather than settled judgment.
% DISAPPEARANCE_RATIONALE: Advocacy coalitions would lose their anchoring symbol and annual rhythm; outlets would lose a dependable yearly story; the attention market around existential risk would reorganize around rival indicators — risk registers, forecast tournaments, anniversary journalism; and the Board's convening role would need a replacement object or dissolve.
% FOUNDING_PROBLEM: After 1945, nuclear danger was legible to physicists but invisible to democratic publics: no image, no number, no occasion. The Clock was built to translate specialist knowledge of civilization-scale risk into a public-legible urgency that citizens and governments could act on.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the atomic scientists' movement and the declassified policy record corroborate that the 1947 translation problem was real and severe. Whether it remains live is disputed along party lines: the benefiting organizations attest that it does; independent risk-analysis literatures and the forecasting community attest that the translation problem was solved long ago and what persists is the ritual. Stated plainly: corroboration for the still-live claim comes almost entirely from inside the benefiting set — no disinterested source attests it.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__performative_tool_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__performative_tool_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__performative_tool_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(doomsday_clock_metric__performative_tool_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__performative_tool_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_metric__performative_tool_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(doomsday_clock_metric__performative_tool_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(doomsday_clock_metric__performative_tool_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end) because the setting's strategic displacement from any indicator baseline is precisely the mechanism by which the arrangement funds itself: each dramatic reset converts stored public trust into present mobilization, and the bill is deferred to future warnings. Suppression is moderate (0.48) and is a raw structural property, unscaled by power or scope: it consists in gatekeeping the symbol (only the Board moves the hand), in the absence of published setting criteria that outsiders could audit, and in the difficulty of building a rival symbol with comparable salience — not in coercive force against persons. Theater ratio (0.41) is substantial but honest to the reading: the announcement ritual, countdown graphics, and anniversary staging are functional FOR mobilization, yet a growing share of the activity is pure repetition — settings that move without new information, drama timed to news windows — and that share has grown monotonically as the founding threat diversified into a portfolio of risks. Accessibility collapse is low (0.35): rivals persist (superforecasting tournaments, national risk registers, insurance-model outputs), so understanding the Clock's strategic character does not eliminate alternatives. Resistance is meaningful (0.60): calibration critiques, forecasting-community pushback, and periodic mockery in technical venues are real and recurrent, though they have never displaced the symbol. The measurement series run on ONE shared time grid (t = 0, 11, 22, 33, 44, 55, 66, 77, roughly 1947-2024) so every tracked metric is authored at every examined point. The series are cyclical rather than smooth at fine grain — settings lurch with event windows (H-bomb tests, the test-ban thaw, the post-Cold-War retreat to 17 minutes, the post-9/11 ratchet, the Ukraine-era 90 seconds) — and the oscillation is itself part of the mechanism: each dramatic reversal re-captures attention in an intermittent-reinforcement pattern, which is why the decade-grained trend nonetheless rises monotonically. Coalition prospects for the payer seats are poor: the heaviest costs land on future audiences who cannot organize, and the present payers (forecasters, advisory institutions) are few, dispersed across disciplines, and partly dependent on the Board for access — the classic structure in which diffuse deferred harm escapes coalition correction.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the Board's position the arrangement is stewardship: someone must translate specialist dread into public action, and discretionary judgment is the price of legibility — the identity-locked exit reflects a board whose members have become the Clock's keepers in self-conception, not merely its employees. From the forecaster's position the same structure is a loud, uncalibrated competitor that primes audiences to read quantitative caution as complacency. From the advisory institutions' position it is a bargain gone sour: they supply the credibility that is spent and receive the distrust that returns. The engine computes these divergent per-seat classifications from the structural data; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The Board sits nearest the beneficiary pole (declared beneficiary, agenda-setting control, identity-locked continuation) — the arrangement subsidizes its institutional continuity. Advocacy organizations and media outlets are beneficiaries with mobile exits: they collect mobilization fuel and story material voluntarily and could leave cheaply, which keeps their derived directionality low but not zero. Quantitative forecasters are targets with constrained exit: they cannot opt out of the credibility environment the Clock shapes. Scientific advisory institutions are partial targets — they are declared victims of spillover distrust, but they also lend personnel to the Board and gain reflected salience, so their structural position sits short of full-target. Future policy audiences are the purest targets: total extraction, zero representation, trapped by not existing yet. No directionality overrides were needed: the beneficiary/victim declarations plus exit options reproduce these positions without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. Reading the arrangement as pure coordination (rope) would erase the asymmetry — the mobilization gains are concentrated and present, the credibility costs are diffuse and deferred, and the same structure delivers both. Reading it as pure extraction (snare) would erase the genuine service: the 1947 translation problem was real, the symbol has repeatedly synchronized action that diffuse technical publication did not, and the advocacy beneficiaries are not fiction. Tangled rope holds both facts. On obsolescence: the founding problem's status is contested rather than dead, so no zombie flag is asserted; but the rising theater ratio and the growing gap between setting drama and policy delivery are the early signature of mandate decay — if the translation problem continues to migrate (nuclear to climate to AI) faster than the symbol's credibility regenerates, the arrangement drifts toward piton dynamics with the Board as administrator and the crediting public as diffuse payers. The fixing-cost entry records the trap: recalibration or retirement is cheap technically and prohibitive institutionally, because the fixer would bear the entire identity and funding loss while the restored trust accrues to everyone else.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint instantiates the performative_tool_reading of the doomsday_clock_metric kernel; what structurally different constraints do the objective_index_reading and hybrid_legitimacy_reading instantiate, and where exactly does the disagreement bite?',
    'Authoring the sibling stories: the objective reading specifies epsilon over indicator-tracking fidelity (its victim set shifts to include unchecked board discretion); the hybrid reading specifies epsilon over the entangled-judgment arrangement. Cross-file comparison of computed classifications localizes the disagreement.',
    'Under the objective reading the arrangement computes nearer a measurement service with negligible extraction; under this reading it computes as coordinated mobilization funded by credibility spending. The kernel contest IS the classification difference — resolving it changes the victim set, the beneficiary set, and the type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story is one reading of a contested kernel, not the kernel itself.').

omega_variable(
    manipulation_tolerance_boundary,
    'How far may a setting depart from indicator-tracked risk levels before the strategic-displacement license destroys the mobilization capacity it is meant to serve?',
    'Longitudinal public-trust series regressed on setting-versus-baseline divergence; natural experiments where internal deliberations or leaked drafts revealed strategic timing choices.',
    'A low tolerance boundary collapses this reading into the hybrid reading (entanglement bounded by evidence); a high tolerance boundary licenses widening displacement and pushes the arrangement toward snare dynamics as trust spending accelerates faster than mobilization returns.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manipulation_tolerance_boundary, empirical, 'Where the performative reading''s tolerance for strategic manipulation becomes self-defeating.').

omega_variable(
    credibility_depletion_reversibility,
    'Is the epistemic credibility the Clock spends a renewable or a depletable resource — do vindicated alarms replenish what overshoots drain?',
    'Comparative case analysis of warning institutions that overshot versus calibrated ones; panel data on expert-trust recovery following vindicated predictions.',
    'Irreversible depletion makes the extraction cumulative and points toward a snare trajectory; reversible depletion permits a sustainable tangled-rope equilibrium in which mobilization gains offset trust costs across cycles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credibility_depletion_reversibility, empirical, 'Whether the extracted resource regenerates, determining accumulation versus cycling.').

omega_variable(
    mobilization_additionality,
    'Does the Clock generate mobilization that would not otherwise occur, or does it harvest attention and effort that rival mechanisms — risk registers, forecast tournaments, anniversary journalism — would have produced anyway?',
    'Quasi-experimental designs around announcement dates measuring campaign recruitment, donations, and legislative attention against matched-control periods and jurisdictions.',
    'If mostly additional, the coordination function is genuine and the tangled-rope reading stands; if mostly harvested, the coordination story thins toward cover and the arrangement trends snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mobilization_additionality, empirical, 'Whether the mobilization the Clock claims credit for is created or diverted.').

omega_variable(
    event_window_coupling,
    'Do setting changes track arrivals of risk-relevant information, or openings of political-opportunity windows — anniversaries, treaty moments, crises in the news cycle?',
    'Event-study comparing setting-change timing against information-arrival timestamps versus political-calendar dates across the full setting history.',
    'Information-coupled settings support residual index content inside this reading; window-coupled settings confirm the strategic core, raising effective extraction for every credibility-bearing seat and strengthening the influence edge this reading exerts on the objective_index_reading sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(event_window_coupling, empirical, 'What actually drives the timing of setting changes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__performative_tool_reading, 0, 77).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dcm_performative_tr_t0, doomsday_clock_metric__performative_tool_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(dcm_performative_tr_t11, doomsday_clock_metric__performative_tool_reading, theater_ratio, 11, 0.18).
narrative_ontology:measurement(dcm_performative_tr_t22, doomsday_clock_metric__performative_tool_reading, theater_ratio, 22, 0.23).
narrative_ontology:measurement(dcm_performative_tr_t33, doomsday_clock_metric__performative_tool_reading, theater_ratio, 33, 0.27).
narrative_ontology:measurement(dcm_performative_tr_t44, doomsday_clock_metric__performative_tool_reading, theater_ratio, 44, 0.31).
narrative_ontology:measurement(dcm_performative_tr_t55, doomsday_clock_metric__performative_tool_reading, theater_ratio, 55, 0.35).
narrative_ontology:measurement(dcm_performative_tr_t66, doomsday_clock_metric__performative_tool_reading, theater_ratio, 66, 0.38).
narrative_ontology:measurement(dcm_performative_tr_t77, doomsday_clock_metric__performative_tool_reading, theater_ratio, 77, 0.41).

% Extraction over time
narrative_ontology:measurement(dcm_performative_be_t0, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(dcm_performative_be_t11, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 11, 0.4).
narrative_ontology:measurement(dcm_performative_be_t22, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 22, 0.46).
narrative_ontology:measurement(dcm_performative_be_t33, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 33, 0.5).
narrative_ontology:measurement(dcm_performative_be_t44, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 44, 0.56).
narrative_ontology:measurement(dcm_performative_be_t55, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 55, 0.62).
narrative_ontology:measurement(dcm_performative_be_t66, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 66, 0.67).
narrative_ontology:measurement(dcm_performative_be_t77, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 77, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(dcm_performative_su_t0, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(dcm_performative_su_t11, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 11, 0.32).
narrative_ontology:measurement(dcm_performative_su_t22, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 22, 0.36).
narrative_ontology:measurement(dcm_performative_su_t33, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 33, 0.39).
narrative_ontology:measurement(dcm_performative_su_t44, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 44, 0.42).
narrative_ontology:measurement(dcm_performative_su_t55, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 55, 0.44).
narrative_ontology:measurement(dcm_performative_su_t66, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 66, 0.46).
narrative_ontology:measurement(dcm_performative_su_t77, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 77, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__performative_tool_reading, identity_coordination).
narrative_ontology:affects_constraint(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric__objective_index_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric__hybrid_legitimacy_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the Doomsday Clock' conflates three structurally distinct claims, which are authored as three linked stories. This file (performative_tool_reading) authors the strategic-mobilization arrangement with high epsilon — beneficiaries are the advocacy apparatus, victims are the credibility commons. The objective_index_reading sibling authors the indicator-tracking claim with negligible epsilon (a measurement service; its contested element is board discretion, not extraction). The hybrid_legitimacy_reading sibling authors the entanglement claim at intermediate epsilon. The upstream/downstream gradient runs from objective through hybrid to performative: the objective reading supplies the epistemic cover ('informed expert judgment') that the performative practice spends, and every visibly strategic setting erodes the legitimacy conditions the objective sibling needs — hence this file declares an influences edge toward the objective reading and a coexists_with edge toward the hybrid reading, whose partisans and this reading's partisans occupy the same discourse without logical elimination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
