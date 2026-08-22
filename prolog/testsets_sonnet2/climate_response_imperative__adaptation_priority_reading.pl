% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__adaptation_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_imperative__adaptation_priority_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: climate_response_imperative__adaptation_priority_reading
 *   human_readable: Adaptation-Priority Reading of the Climate Response Imperative
 *   domain: climate policy / political economy / intergenerational justice
 *
 * SUMMARY:
 *   Under the adaptation-priority reading, international climate response
 *   allocates the bulk of concrete institutional effort, finance, and
 *   political capital to resilience infrastructure and damage reduction in
 *   physically exposed regions — sea walls, drought-resistant agriculture,
 *   flood defense, early-warning systems — while treating binding mitigation
 *   (emissions caps, fossil fuel phase-out schedules) as an aspirational
 *   long-term goal not backed by enforceable near-term obligation. The
 *   genuine coordination function is real: exposed populations face present,
 *   non-deferrable physical risk that adaptation investment measurably
 *   reduces. But the same framing structurally benefits emitting states and
 *   industries by deferring the costs of transition, while shifting an
 *   escalating capital burden onto present-day developing nations that did
 *   not generate the hazard and increasingly onto future generations who
 *   inherit a worse hazard baseline than mitigation would have produced. The
 *   vicious circle is structural: the less mitigation occurs, the more
 *   adaptation capital exposed regions require, and the same institutions
 *   controlling adaptation disbursement have no binding obligation to supply
 *   mitigation that would shrink that requirement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__adaptation_priority_reading, 0.71).
domain_priors:suppression_score(climate_response_imperative__adaptation_priority_reading, 0.58).
domain_priors:theater_ratio(climate_response_imperative__adaptation_priority_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, resistance, 0.66).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__adaptation_priority_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__adaptation_priority_reading, "Adaptation-Priority Reading of the Climate Response Imperative").
narrative_ontology:topic_domain(climate_response_imperative__adaptation_priority_reading, "climate policy / political economy / intergenerational justice").

domain_priors:requires_active_enforcement(climate_response_imperative__adaptation_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__adaptation_priority_reading, 'e3540e1b-716c-4c8f-88b2-dd4233222525').
narrative_ontology:cs_kernel_codification('e3540e1b-716c-4c8f-88b2-dd4233222525', distributed).
narrative_ontology:cs_authority_grounding('e3540e1b-716c-4c8f-88b2-dd4233222525', distributed).
narrative_ontology:cs_reading_relation('e3540e1b-716c-4c8f-88b2-dd4233222525', climate_response_imperative__mitigation_priority_reading, coexists_with).
narrative_ontology:cs_reading_relation('e3540e1b-716c-4c8f-88b2-dd4233222525', climate_response_imperative__degrowth_reading, influences).
narrative_ontology:cs_axiom('e3540e1b-716c-4c8f-88b2-dd4233222525', foundational, locked_in_hazard_demands_present_resilience_primacy).
narrative_ontology:cs_axiom_status(locked_in_hazard_demands_present_resilience_primacy, holdable).
narrative_ontology:cs_axiom_grounding('e3540e1b-716c-4c8f-88b2-dd4233222525', locked_in_hazard_demands_present_resilience_primacy, empirically_contingent).
narrative_ontology:cs_axiom('e3540e1b-716c-4c8f-88b2-dd4233222525', secondary, mitigation_obligation_may_remain_nonbinding_while_hazard_is_addressed).
narrative_ontology:cs_axiom_status(mitigation_obligation_may_remain_nonbinding_while_hazard_is_addressed, holdable).
narrative_ontology:cs_axiom_grounding('e3540e1b-716c-4c8f-88b2-dd4233222525', mitigation_obligation_may_remain_nonbinding_while_hazard_is_addressed, instrumental).
narrative_ontology:cs_reference_frame('e3540e1b-716c-4c8f-88b2-dd4233222525', common_but_differentiated_responsibilities_1992).
narrative_ontology:cs_drift_state('e3540e1b-716c-4c8f-88b2-dd4233222525', post_paris_agreement_implementation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e3540e1b-716c-4c8f-88b2-dd4233222525', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__adaptation_priority_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, high_emissions_incumbent_industries).
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, developed_nation_treasuries).
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, carbon_intensive_exporters).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, low_lying_island_states).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, sahel_agricultural_communities).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, south_asian_flood_basin_populations).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, future_generations_in_exposed_regions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the multilateral climate finance architecture (adaptation funds, loss-and-damage mechanisms, bilateral aid conditionality) and set the terms under which adaptation funding is disbursed. Frame adaptation as the pragmatic, achievable response while deferring binding mitigation commitments that would require domestic industrial transition. Bear no immediate exposure to the physical hazards being adapted to.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, developed_nation_treasuries, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__adaptation_priority_reading, developed_nation_treasuries, beneficiary).

% Continue current emissions-intensive operations largely undisturbed because the adaptation-priority framing routes political and financial attention toward resilience infrastructure in exposed regions rather than toward binding caps or phase-outs at the source. Fund advocacy that presents adaptation as the responsible, near-term-deliverable response.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, high_emissions_incumbent_industries, beneficiary,
    organized, biographical, arbitrage, global).

% Face existential territorial loss from sea level rise but cannot independently finance seawalls, managed retreat, or relocation at the scale required. Must apply for adaptation grants and loans denominated in foreign currency, often through processes designed by the same institutions whose member states are the largest historical emitters. Have no capacity to exit the physical exposure and minimal leverage to compel deeper mitigation from emitters.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, low_lying_island_states, payer,
    powerless, civilizational, trapped, national).

% Experience desertification and rainfall disruption that erodes subsistence agriculture. Adaptation aid arrives as drought-resistant seed programs and irrigation projects, funded through debt-generating loans rather than grants in most cases, while the emissions driving the disruption originate almost entirely outside the region. Migration is the only meaningful exit and is itself criminalized or restricted by destination states.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, sahel_agricultural_communities, payer,
    powerless, generational, trapped, regional).

% Live in river deltas and floodplains subject to intensifying monsoon variability and glacial melt. Receive periodic post-disaster reconstruction aid rather than sustained preventive investment, because adaptation financing is proposal-driven and competitive rather than need-driven. Cannot relocate at scale given population density and land scarcity.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, south_asian_flood_basin_populations, payer,
    powerless, biographical, trapped, regional).

% Inherit a locked-in level of physical hazard set by cumulative emissions the adaptation-priority framing did not slow. Will require larger, more frequent adaptation expenditures than the present generation as hazard baselines worsen, financed by economies whose growth trajectories are themselves constrained by unmitigated climate damage. Cannot participate in present financing negotiations at all.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, future_generations_in_exposed_regions, payer,
    powerless, civilizational, trapped, global).

% Fossil-fuel exporting states and firms benefit from continued market access under an adaptation-priority regime because it does not impose binding demand-side constraints. Some contribute nominally to adaptation funds as reputational cover while continuing to expand extraction capacity.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, carbon_intensive_exporters, beneficiary,
    powerful, biographical, arbitrage, global).

% Administer the Green Climate Fund, Adaptation Fund, and bilateral instruments; design eligibility criteria, disbursement timelines, and loan-versus-grant terms. Have institutional interest in the adaptation-priority framing persisting because it is the mandate that justifies their continued operation and funding requests, independent of whether it resolves the underlying emissions trajectory.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, adaptation_finance_institutions, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__adaptation_priority_reading, adaptation_finance_institutions, observer).

% Argue that resources and political attention diverted to adaptation-as-primary strategy reduce pressure for binding emissions cuts, locking in ever-larger future adaptation burdens. Present at climate negotiations but structurally outvoted by the coalition of emitters and finance-controlling states that prefer the adaptation-priority frame.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, mitigation_advocacy_coalitions, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely coordinates emergency and infrastructure resources toward populations facing immediate, measurable physical exposure — seawalls, drought-resistant agriculture, flood defenses, and early-warning systems that reduce loss of life and property in the near term.
% TRANSFER_FUNCTION: Moves finite climate finance, political attention, and institutional capacity toward resilience infrastructure in exposed regions, while the emissions trajectory generating the underlying hazard continues largely unconstrained; the cost of that continued trajectory is transferred forward onto the same exposed populations and their descendants, who will require progressively larger adaptation expenditure as hazard baselines rise.
% ABSENT_VOICES: Mitigation advocacy coalitions and youth/future-generations representatives argue the adaptation-priority frame is a deferral strategy that trades near-term political comfort in emitting states for compounding physical and financial risk in exposed regions; they participate in negotiations but do not control fund design, disbursement criteria, or the underlying emissions policy levers.
% DISAPPEARANCE_RATIONALE: If the adaptation-priority framing disappeared and mitigation obligations became binding and enforceable overnight, developed-nation treasuries and carbon-intensive exporters would face immediate demand-side and industrial-transition costs currently deferred; exposed regions would see the underlying hazard trajectory bend downward over time, changing the scale of adaptation finance actually required. The institutional architecture built around adaptation-as-primary response (funds, disbursement bureaucracies, resilience-industry contracts) would lose its central justification.
% FOUNDING_PROBLEM: Some degree of climate hazard is already locked in by historical emissions, and populations in exposed regions face present, non-deferrable physical risk that resilience investment can genuinely reduce regardless of what happens to future emissions.
% FOUNDING_PROBLEM_CORROBORATION: Developed-nation treasuries and adaptation finance institutions attest the founding problem remains live and adaptation investment is urgently needed on its own terms. IPCC synthesis reports and independent climate-finance auditors (outside the disbursing institutions) corroborate that adaptation need is real but also document that the adaptation-priority framing has been used to justify continued deferral of mitigation commitments — a use the founding problem itself does not require and that mitigation advocacy coalitions and several exposed-state governments explicitly contest.
narrative_ontology:disappearance_verdict(climate_response_imperative__adaptation_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__adaptation_priority_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__adaptation_priority_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_imperative__adaptation_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__adaptation_priority_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_imperative__adaptation_priority_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_imperative__adaptation_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_imperative__adaptation_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts moderate (0.48) and rises to 0.71 over the measured interval, reflecting a widening gap between locked-in hazard growth (driven by continued unmitigated emissions) and the adaptation capital actually disbursed to exposed regions — the gap itself is the extraction, borne by populations who did not create it. Theater ratio rises from 0.22 to 0.42 as an increasing share of climate diplomacy activity (pledges, funds, communiqués) substitutes for binding mitigation commitment without materially closing the hazard-versus-finance gap. Suppression is moderate (0.58 at interval end): it is not primarily coercive in the classic sense but operates through structural dependency — exposed states have no meaningful unilateral lever over the emissions trajectory determining their own risk exposure, and adaptation finance is disbursed on terms set by the same actors benefiting from continued deferral.
 *
 * DIRECTIONALITY LOGIC:
 *   Developed-nation treasuries, high-emissions incumbent industries, and carbon-intensive exporters sit near the beneficiary end: they retain arbitrage-grade exit (capital and political mobility, no binding constraint on continued operation) and the adaptation-priority frame actively defers the costs that would otherwise fall on them. Low-lying island states, Sahel agricultural communities, South Asian flood-basin populations, and future generations in exposed regions sit near the full-target end: trapped exit (no capacity to relocate at scale or exit physical exposure), civilizational-to-generational time horizons that guarantee they will bear compounding costs, and no lever over the emissions trajectory that determines the scale of adaptation burden they will eventually require. Adaptation finance institutions occupy an intermediate agenda-setting seat: they administer real resource flows to exposed regions (a genuine coordination function) but also have institutional self-interest in the adaptation-priority mandate persisting regardless of its effect on the underlying emissions trajectory.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — some hazard is already locked in and exposed populations need resilience investment now, regardless of future emissions policy — remains genuinely live; this is not a pure zombie mandate. The mandatrophy risk is narrower and sharper: the adaptation-priority framing is used to justify indefinite deferral of mitigation obligations that the founding problem does not itself require deferring. Classifying this as tangled_rope rather than snare preserves the real coordination function (resilience investment measurably saves lives and property now) while still registering the asymmetric extraction (the same framing shields emitters from binding constraint at direct cost to those least responsible). A pure snare framing would falsely deny that adaptation investment does real good; a pure rope framing would falsely deny that the frame's persistence serves emitter interests at victim expense.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptation_mitigation_tradeoff_reality,
    'Is adaptation-priority framing a genuinely necessary emphasis given already-locked-in hazard, or is it a politically convenient deferral mechanism that could be replaced by simultaneous binding mitigation without sacrificing near-term resilience investment?',
    'Comparative analysis of climate finance flows and emissions trajectories in jurisdictions that have pursued binding mitigation commitments alongside adaptation investment (e.g., EU carbon pricing plus structural funds) versus jurisdictions pursuing adaptation-primary strategies, controlling for baseline exposure and capacity.',
    'If binding mitigation and adequate adaptation investment are jointly achievable without tradeoff, the adaptation-priority reading''s implicit tradeoff framing is exposed as a constructed choice rather than a structural necessity, strengthening the tangled_rope/extraction reading. If a genuine near-term tradeoff exists (mitigation investment displacing adaptation capital), the coordination function is stronger than currently modeled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_mitigation_tradeoff_reality, empirical, 'Whether the adaptation/mitigation split is a real resource constraint or a constructed deferral frame.').

omega_variable(
    kernel_reading_selection_mechanism,
    'This constraint is one reading (adaptation_priority_reading) of the climate_response_imperative kernel, alongside mitigation_priority_reading and degrowth_reading. What determines which reading dominates actual multilateral policy at a given moment — is it evidentiary (which reading best fits the physical science), institutional (which reading fits within existing finance/governance architecture with least disruption), or power-distributional (which reading best protects incumbent emitter interests)?',
    'Process-tracing of UNFCCC COP negotiation records and national delegation positions to identify which considerations (scientific, institutional, or distributional) most consistently predict a state''s preferred reading.',
    'If institutional/power-distributional factors dominate reading selection over evidentiary fit, this corroborates the tangled_rope classification (the reading persists because it serves identifiable beneficiaries, not because it best fits the founding problem). If evidentiary fit dominates, the reading''s persistence is better explained as good-faith response to locked-in hazard.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_mechanism, conceptual, 'What structurally selects the adaptation-priority reading over its sibling readings in real multilateral policy.').

omega_variable(
    future_generations_agency_status,
    'Future generations in exposed regions are listed as a victim/payer stakeholder despite having no capacity to author, contest, or exit the arrangement — is representing them as a stakeholder with a ''situation'' coherent, or does this smuggle in an agency they structurally lack?',
    'Compare against standard intergenerational-justice frameworks (e.g., discounting debates in climate economics) that treat future persons as morally considerable but causally inert with respect to present decisions.',
    'If future generations should not be modeled as stakeholders with situations at all, the victim set for this reading should be narrowed to present-day exposed populations only, which would somewhat reduce the civilizational time-horizon weighting currently driving high effective extraction at large scope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_generations_agency_status, conceptual, 'Whether modeling future generations as a stakeholder overstates their structural agency in the present arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__adaptation_priority_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_imperative__adaptation_priority_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(clim_tr_t5, climate_response_imperative__adaptation_priority_reading, theater_ratio, 5, 0.27).
narrative_ontology:measurement(clim_tr_t10, climate_response_imperative__adaptation_priority_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement(clim_tr_t15, climate_response_imperative__adaptation_priority_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(clim_tr_t20, climate_response_imperative__adaptation_priority_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(clim_tr_t25, climate_response_imperative__adaptation_priority_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(clim_tr_t30, climate_response_imperative__adaptation_priority_reading, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(clim_be_t5, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(clim_be_t10, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement(clim_be_t15, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(clim_be_t20, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(clim_be_t25, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 25, 0.69).
narrative_ontology:measurement(clim_be_t30, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 30, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(clim_su_t5, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(clim_su_t10, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 10, 0.49).
narrative_ontology:measurement(clim_su_t15, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement(clim_su_t20, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(clim_su_t25, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 25, 0.57).
narrative_ontology:measurement(clim_su_t30, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__adaptation_priority_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_imperative__adaptation_priority_reading, 0.12).
narrative_ontology:affects_constraint(climate_response_imperative__adaptation_priority_reading, climate_response_imperative__mitigation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__adaptation_priority_reading, climate_response_imperative__degrowth_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the climate_response_imperative kernel. adaptation_priority_reading (this file) authors ε for the standing arrangement in which resilience investment is prioritized and mitigation remains aspirational, from that reading's own lights — a genuine coordination function (present hazard reduction) coupled with asymmetric extraction (deferred mitigation costs borne by exposed, low-responsibility populations). mitigation_priority_reading authors a distinct ε for the arrangement in which emissions reduction via technology/markets is primary and adaptation is residual — its victim set differs (populations under-protected by residualized adaptation, not populations paying for deferred mitigation). degrowth_reading authors a further distinct ε for an arrangement demanding structural economic transformation in the Global North, with its own beneficiary/victim structure centered on incumbent growth-dependent institutions as targets rather than beneficiaries. All three share the kernel but are structurally distinct constraints with different ε, different victim sets, and different classifications — per the ε-invariance principle they are authored as three separate files linked here rather than one story with a hidden observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
