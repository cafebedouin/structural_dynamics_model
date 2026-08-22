% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__adaptation_priority, []).

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
 *   constraint_id: climate_response_legitimacy__adaptation_priority
 *   human_readable: Adaptation-Priority Reading of Climate Response Legitimacy
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint instantiates the adaptation-priority reading of the
 *   contested climate-response-legitimacy kernel: legitimate climate response
 *   accepts the warming trajectory as substantially fixed and directs
 *   resources toward protecting vulnerable populations through resilience
 *   infrastructure and adaptive capacity, rather than centering aggressive
 *   mitigation or structural economic transformation. The reading has a
 *   genuine coordination function — resilience investment provides real
 *   protective value to people already exposed to warming impacts — but it
 *   also structurally lets historically high-emitting wealthy states and
 *   fossil fuel incumbents preserve their development model while
 *   transferring the compounding costs of a higher-warming trajectory onto
 *   low-income regions now and onto future generations who have no voice in
 *   the arrangement. The adaptation finance gap (cited at roughly $350B
 *   annually) is the visible symptom of the asymmetry: aid flows are real but
 *   structurally inadequate relative to the harm being deferred rather than
 *   avoided.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__adaptation_priority, 0.68).
domain_priors:suppression_score(climate_response_legitimacy__adaptation_priority, 0.52).
domain_priors:theater_ratio(climate_response_legitimacy__adaptation_priority, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__adaptation_priority, "Adaptation-Priority Reading of Climate Response Legitimacy").
narrative_ontology:topic_domain(climate_response_legitimacy__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__adaptation_priority, '2f724827-5dfc-47bf-88b5-6a5b1f37167b').
narrative_ontology:cs_kernel_codification('2f724827-5dfc-47bf-88b5-6a5b1f37167b', distributed).
narrative_ontology:cs_authority_grounding('2f724827-5dfc-47bf-88b5-6a5b1f37167b', distributed).
narrative_ontology:cs_reading_relation('2f724827-5dfc-47bf-88b5-6a5b1f37167b', climate_response_legitimacy__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('2f724827-5dfc-47bf-88b5-6a5b1f37167b', climate_response_legitimacy__degrowth_transformation, coexists_with).
narrative_ontology:cs_axiom('2f724827-5dfc-47bf-88b5-6a5b1f37167b', foundational, warming_trajectory_substantially_locked_in).
narrative_ontology:cs_axiom_status(warming_trajectory_substantially_locked_in, holdable).
narrative_ontology:cs_axiom_grounding('2f724827-5dfc-47bf-88b5-6a5b1f37167b', warming_trajectory_substantially_locked_in, empirically_contingent).
narrative_ontology:cs_axiom('2f724827-5dfc-47bf-88b5-6a5b1f37167b', foundational, protecting_the_already_exposed_takes_priority_over_altering_the_trajectory).
narrative_ontology:cs_axiom_status(protecting_the_already_exposed_takes_priority_over_altering_the_trajectory, holdable).
narrative_ontology:cs_axiom_grounding('2f724827-5dfc-47bf-88b5-6a5b1f37167b', protecting_the_already_exposed_takes_priority_over_altering_the_trajectory, instrumental).
narrative_ontology:cs_reference_frame('2f724827-5dfc-47bf-88b5-6a5b1f37167b', post_paris_agreement_ambition_gap).
narrative_ontology:cs_drift_state('2f724827-5dfc-47bf-88b5-6a5b1f37167b', contemporary_loss_and_damage_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2f724827-5dfc-47bf-88b5-6a5b1f37167b', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, wealthy_industrial_states).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, fossil_fuel_incumbents).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, adaptation_finance_intermediaries).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, low_income_frontline_regions).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, small_island_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, resilience_engineering_sector).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__adaptation_priority, warming_trajectory_is_locked_in).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__adaptation_priority, resilience_infrastructure_is_the_responsible_response).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the terms of international climate finance and diplomatic framing, channeling resources toward adaptation and resilience programs rather than accepting binding emissions cuts or transformation of its own growth model. Retains its existing energy and consumption base while funding a fraction of the adaptation gap it helped create. Can reframe legitimacy debates in COP negotiations and multilateral bodies at will.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, wealthy_industrial_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__adaptation_priority, wealthy_industrial_states, beneficiary).

% Continues extraction and production largely undisturbed because the adaptation-priority framing does not require phase-out timelines or stranded-asset write-downs. Funds think tanks and lobbying that favor 'resilience' language over binding mitigation targets, since resilience spending does not threaten the underlying business model.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, fossil_fuel_incumbents, beneficiary,
    organized, biographical, arbitrage, global).

% Multilateral development banks, consultancies, and insurance/reinsurance firms that design, finance, and administer resilience infrastructure projects, collecting fees, interest, and contracts. Their institutional survival depends on adaptation remaining the dominant framework rather than being superseded by aggressive mitigation that would shrink the market for adaptation products.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, adaptation_finance_intermediaries, beneficiary,
    organized, biographical, mobile, global).

% Faces the sharpest and earliest physical impacts of warming — drought, flooding, heat, crop failure — while receiving only a fraction of the estimated $350 billion annual adaptation finance gap, often as loans rather than grants. Cannot relocate populations or economies at scale, cannot bid up mitigation ambition among the largest emitters, and must accept resilience infrastructure funded on donor terms as the primary available form of help.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, low_income_frontline_regions, payer,
    powerless, immediate, trapped, regional).

% Faces existential territorial loss from sea-level rise that resilience infrastructure cannot fully offset past a certain warming threshold. Advocates loudly in international forums for loss-and-damage funding and stronger mitigation, but has negligible leverage over the emissions trajectories set by large economies; adaptation framing effectively asks them to engineer survival around a trajectory they did not choose and cannot alter.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, small_island_states, payer,
    powerless, civilizational, trapped, regional).

% Inherits a higher-warming baseline than would result from an aggressive mitigation pathway, since resources and political attention are directed toward present-day resilience rather than emissions reduction. Has no seat at any negotiating table and cannot register consent or dissent; bears compounding physical and fiscal costs of the deferred trajectory.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Engineering firms, agritech developers, and infrastructure contractors whose order books expand as adaptation becomes the dominant policy frame. Genuinely deliver protective value (sea walls, drought-resistant crops, early-warning systems) even as their institutional interest tilts toward adaptation remaining central rather than mitigation shrinking the addressable market.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, resilience_engineering_sector, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__adaptation_priority, resilience_engineering_sector, observer).

% Argue that adaptation-priority framing lets historical emitters off the hook for causally-owed mitigation and reparative loss-and-damage transfers. Present at civil-society forums but structurally outside the rooms where finance commitments and emissions targets are actually negotiated; their proposals for binding mitigation obligations rarely reach formal agreement text.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, climate_justice_advocates, excluded,
    moderate, generational, constrained, global).

% Assess the physical trajectory and the adaptation limits beyond which resilience investment cannot compensate for warming. Provide the evidentiary basis both for the adaptation deficit figures and for the claim that further mitigation ambition would reduce the burden this reading accepts as fixed.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, climate_scientists_ipcc, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_legitimacy__adaptation_priority, diffuse).
narrative_ontology:fixing_cost_class(climate_response_legitimacy__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Directs scarce present-day resources toward protecting people who will otherwise be harmed by warming that is already substantially locked in, building sea walls, early-warning systems, drought-resistant agriculture, and social protection floors that provide real, immediate protective value regardless of future mitigation outcomes.
% TRANSFER_FUNCTION: Moves a fraction of global capital toward resilience infrastructure in vulnerable regions (largely as loans and project finance rather than grants), while allowing continued emissions and growth in wealthy, high-emitting economies; costs of the resulting higher warming trajectory are transferred forward onto low-income regions now and onto future generations indefinitely.
% ABSENT_VOICES: Future generations have no negotiating seat and cannot register consent to a trajectory that compounds their inherited costs. Climate justice advocates and many low-income-region negotiators press for binding mitigation and reparative finance but are structurally outside the rooms where emissions targets and finance commitments are actually set.
% DISAPPEARANCE_RATIONALE: If the adaptation-priority framing were abandoned, international finance and diplomatic energy would have to be redirected toward binding mitigation commitments or transformation proposals; wealthy states would face direct pressure to alter emissions trajectories rather than fund protective infrastructure elsewhere, and fossil fuel incumbents and adaptation-finance intermediaries would lose the framing that currently protects their business models.
% FOUNDING_PROBLEM: By the 2010s, a wide gap had opened between rhetorical mitigation commitments and actual emissions trajectories; adaptation-priority framing emerged to address the reality that some warming and associated harm were becoming unavoidable regardless of near-term mitigation success, requiring protective investment for populations already exposed.
% FOUNDING_PROBLEM_CORROBORATION: IPCC assessment reports and independent climate-finance trackers (outside both the wealthy-state and adaptation-finance-industry beneficiary groups) corroborate that a real adaptation deficit and locked-in warming exist. The same outside sources also document that the framing is used to substitute for, rather than complement, adequate mitigation ambition and loss-and-damage transfers — a use the low-income-region and small-island negotiators dispute as inadequate to the founding problem's original scope.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_legitimacy__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__adaptation_priority, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 by interval end) reflects that a substantial share of the arrangement's cost is not the resilience spending itself but the emissions and growth pathway that is being implicitly ratified by treating warming as accepted rather than contested. Suppression (0.52) is moderate: this reading is enforced less through direct coercion and more through diplomatic and financial gatekeeping — which negotiating positions get funded, which framings dominate COP text, which proposals for binding mitigation targets or reparative finance fail to reach agreement. Theater ratio (0.44) captures that a meaningful share of adaptation finance commitments are announced but not delivered, or delivered as loans rebadged as aid, producing a visible gap between pledged and disbursed resilience funding.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting seat (wealthy states, adaptation finance intermediaries), the arrangement reads as responsible, evidence-based climate governance: warming is locked in to some degree, so resources should go where they save lives now. From the payer seats (frontline regions, small island states, future generations), the identical structure reads as an institutionalized deferral mechanism that lets the parties most responsible for emissions avoid the costs of changing course, while others absorb compounding physical and fiscal damage. The engine should compute these as different seat-level types from the same structural data — that divergence is exactly what this reading is meant to expose, not an inconsistency to be smoothed over.
 *
 * DIRECTIONALITY LOGIC:
 *   Wealthy industrial states and fossil fuel incumbents sit near the beneficiary end: they retain arbitrage-grade exit (they can adjust their own domestic exposure and continue extraction/consumption largely unconstrained) while collecting the political benefit of appearing responsive through resilience funding. Low-income frontline regions and small island states sit near the full-target end: trapped exit options, immediate and severe exposure, and negligible leverage over the emissions decisions that determine how much adaptation will ultimately be needed. Future generations are the starkest case — powerless, trapped by construction, with civilizational time horizon but zero voice in present negotiations, making them the purest expression of deferred and compounded cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protecting people from warming impacts that outpace mitigation progress — remains genuinely live; resilience infrastructure that saves lives today is not disqualified by the fact that its dominant framing also serves incumbent interests. The risk of mandatrophy is that adaptation-priority becomes the permanent frame even after mitigation ambition becomes politically and technologically feasible again, at which point the 'accept the trajectory' premise stops being a realistic assessment and becomes a self-serving one. Classifying this as tangled_rope rather than snare preserves the real coordination function (protective infrastructure genuinely protects) while still registering the asymmetric extraction (wealthy states and incumbents systematically shift cost onto powerless payers) that a pure-rope or pure-mountain framing would erase.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trajectory_lock_in_degree,
    'How much of the future warming trajectory is genuinely physically locked in versus how much remains politically alterable through more aggressive near-term mitigation?',
    'Updated IPCC carbon budget assessments and integrated assessment modeling comparing feasible near-term mitigation pathways against current policy trajectories; track whether the ''accepted'' trajectory shifts as mitigation ambition changes.',
    'If the trajectory is substantially more alterable than this reading assumes, the adaptation-priority framing functions less as realistic acceptance and more as a legitimating cover for continued high emissions — strengthening the case for reclassification toward snare. If genuinely locked in, the coordination function of resilience investment is more clearly primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trajectory_lock_in_degree, empirical, 'Whether ''accepting the trajectory'' reflects physical necessity or political convenience.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly does this reading''s core premise diverge from the mitigation_priority and degrowth_transformation readings, and is the divergence resolvable by evidence or only by value commitments about acceptable risk and distributive justice?',
    'Structured comparison of the three readings'' treatment of (a) the marginal cost of additional mitigation ambition, (b) who bears transition costs under each reading, and (c) time-discounting assumptions applied to future harm; identify which disagreements are empirical (resolvable by better carbon budget or cost data) versus normative (irreducible value disagreement about intergenerational discounting or growth).',
    'If the disagreement is substantially normative rather than empirical, no amount of additional climate science will resolve which reading is ''correct'' — the kernel remains genuinely contested rather than converging, and all three readings persist as live coexisting positions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Locating whether the kernel dispute is empirical or a genuine value conflict about risk and distribution.').

omega_variable(
    adaptation_finance_capture_extent,
    'To what extent is the $350B adaptation finance gap itself a product of intentional underfunding by wealthy states versus a genuine resource/administrative capacity constraint?',
    'Audit trail comparison of pledged versus disbursed adaptation finance across major COP commitments (Copenhagen $100B pledge, Glasgow, Sharm el-Sheikh loss-and-damage fund) against wealthy-state fiscal capacity and domestic climate spending in the same period.',
    'A pattern of systematic underdelivery relative to demonstrated fiscal capacity would strengthen the case that the gap functions as a structural extraction mechanism rather than a resource constraint, supporting the tangled_rope classification''s victim declarations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_finance_capture_extent, empirical, 'Whether the adaptation finance shortfall reflects capacity limits or a deliberate underfunding pattern.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__adaptation_priority, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_legitimacy__adaptation_priority, theater_ratio, 0, 0.3).
narrative_ontology:measurement(clim_tr_t4, climate_response_legitimacy__adaptation_priority, theater_ratio, 4, 0.34).
narrative_ontology:measurement(clim_tr_t8, climate_response_legitimacy__adaptation_priority, theater_ratio, 8, 0.37).
narrative_ontology:measurement(clim_tr_t12, climate_response_legitimacy__adaptation_priority, theater_ratio, 12, 0.4).
narrative_ontology:measurement(clim_tr_t16, climate_response_legitimacy__adaptation_priority, theater_ratio, 16, 0.42).
narrative_ontology:measurement(clim_tr_t20, climate_response_legitimacy__adaptation_priority, theater_ratio, 20, 0.44).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_legitimacy__adaptation_priority, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(clim_be_t4, climate_response_legitimacy__adaptation_priority, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(clim_be_t8, climate_response_legitimacy__adaptation_priority, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(clim_be_t12, climate_response_legitimacy__adaptation_priority, base_extractiveness, 12, 0.63).
narrative_ontology:measurement(clim_be_t16, climate_response_legitimacy__adaptation_priority, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(clim_be_t20, climate_response_legitimacy__adaptation_priority, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_legitimacy__adaptation_priority, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(clim_su_t4, climate_response_legitimacy__adaptation_priority, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(clim_su_t8, climate_response_legitimacy__adaptation_priority, suppression_requirement, 8, 0.47).
narrative_ontology:measurement(clim_su_t12, climate_response_legitimacy__adaptation_priority, suppression_requirement, 12, 0.49).
narrative_ontology:measurement(clim_su_t16, climate_response_legitimacy__adaptation_priority, suppression_requirement, 16, 0.51).
narrative_ontology:measurement(clim_su_t20, climate_response_legitimacy__adaptation_priority, suppression_requirement, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__adaptation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_legitimacy__adaptation_priority, 0.12).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__degrowth_transformation).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the climate_response_legitimacy kernel, each authored as a separate ε-invariant constraint per the ε-invariance principle. adaptation_priority accepts the warming trajectory and centers protective infrastructure (this file); mitigation_priority centers emissions reduction via technology/carbon pricing while preserving growth; degrowth_transformation centers dismantling the growth imperative in wealthy nations. The three share no single ε — each reading's beneficiary/victim structure and extraction profile differ because each reading treats a different arrangement as the standing referent under contest. All three files link to each other via affects_constraints to preserve the kernel-family structure for contamination and coupling analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
