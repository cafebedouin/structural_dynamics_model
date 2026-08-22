% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__adaptation_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: climate_response_imperative__adaptation_priority_reading
 *   human_readable: Climate Response as Adaptation Priority (Resilience-First Reading)
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   The adaptation-priority reading frames climate response as primarily
 *   resilience-building and damage reduction in exposed regions, treating
 *   mitigation as an aspirational long-term goal rather than an urgent
 *   near-term imperative. This reading instantiates a constraint where
 *   wealthy nations retain industrial-era emission rights while vulnerable
 *   nations bear adaptation costs they cannot finance independently. The
 *   structural delta is the vicious circle: present-day developing nations
 *   and small island states face immediate climate impacts that require
 *   massive capital investment they lack, creating dependency on adaptation
 *   finance while mitigation—which would prevent future damage—is deferred.
 *   The reading legitimizes this distribution by appealing to the urgency of
 *   present suffering and the time lag between mitigation and benefit, but
 *   the effect is to externalize the cost of wealthy-nation carbon lock-in
 *   onto those least responsible for emissions.
 *
 * KEY AGENTS:
 *   - Wealthy nations: retain carbon budgets and defer deep decarbonization under adaptation-first framing
 *   - Developing nations and small island states: face immediate impacts and must service adaptation debt while mitigation timelines remain aspirational
 *   - Adaptation vendors and climate-finance intermediaries: capture resource flows by administering resilience infrastructure
 *   - Future generations (absent): inherit a world where adaptation locks in high-cost emissions pathways and mitigation becomes exponentially harder
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__adaptation_priority_reading, 0.82).
domain_priors:suppression_score(climate_response_imperative__adaptation_priority_reading, 0.71).
domain_priors:theater_ratio(climate_response_imperative__adaptation_priority_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__adaptation_priority_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__adaptation_priority_reading, "Climate Response as Adaptation Priority (Resilience-First Reading)").
narrative_ontology:topic_domain(climate_response_imperative__adaptation_priority_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(climate_response_imperative__adaptation_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__adaptation_priority_reading, 'b7f5517b-f507-49a2-93bf-47470de0db80').
narrative_ontology:cs_kernel_codification('b7f5517b-f507-49a2-93bf-47470de0db80', distributed).
narrative_ontology:cs_authority_grounding('b7f5517b-f507-49a2-93bf-47470de0db80', extraction).
narrative_ontology:cs_reading_relation('b7f5517b-f507-49a2-93bf-47470de0db80', climate_response_imperative__mitigation_priority_reading, influences).
narrative_ontology:cs_reading_relation('b7f5517b-f507-49a2-93bf-47470de0db80', climate_response_imperative__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('b7f5517b-f507-49a2-93bf-47470de0db80', foundational, immediate_adaptation_urgency_justifies_mitigation_deferral).
narrative_ontology:cs_axiom_status(immediate_adaptation_urgency_justifies_mitigation_deferral, holdable).
narrative_ontology:cs_axiom_grounding('b7f5517b-f507-49a2-93bf-47470de0db80', immediate_adaptation_urgency_justifies_mitigation_deferral, empirically_contingent).
narrative_ontology:cs_axiom('b7f5517b-f507-49a2-93bf-47470de0db80', foundational, wealthy_nation_carbon_budgets_defensible_under_time_lag_logic).
narrative_ontology:cs_axiom_status(wealthy_nation_carbon_budgets_defensible_under_time_lag_logic, holdable).
narrative_ontology:cs_axiom_grounding('b7f5517b-f507-49a2-93bf-47470de0db80', wealthy_nation_carbon_budgets_defensible_under_time_lag_logic, instrumental).
narrative_ontology:cs_reference_frame('b7f5517b-f507-49a2-93bf-47470de0db80', present_climate_impacts_require_immediate_adaptation_finance).
narrative_ontology:cs_drift_state('b7f5517b-f507-49a2-93bf-47470de0db80', contemporary_accumulating_adaptation_deficits, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b7f5517b-f507-49a2-93bf-47470de0db80', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__adaptation_priority_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, wealthy_nations_carbon_budgets).
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, technological_adaptation_vendors).
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, climate_finance_intermediaries).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, present_day_developing_nations).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, small_island_states).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, subsistence_communities).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, future_generations_high_cost_pathways).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, global_north_constituencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Preserves industrial-era emission rights and defers deep decarbonization: adaptation framing allows continued fossil fuel infrastructure under the justification that vulnerable regions are 'building resilience' rather than facing the urgent need for global emissions reduction. The reframing extends the window for capital recovery on existing carbon-locked assets.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, wealthy_nations_carbon_budgets, beneficiary,
    institutional, generational, arbitrage, global).

% Immediate material crisis from present-day climate impacts (floods, droughts, heat, sea-level rise). Adaptation framing directs climate finance toward capital-intensive resilience infrastructure (dikes, irrigation, evacuation systems) they cannot finance themselves. They must service adaptation debt while emissions mitigation—which would prevent future damage—remains aspirational and underfunded. Exit from the constraint would require either unilateral mitigation (unaffordable) or renegotiation of the global response architecture (no power to demand).
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, present_day_developing_nations, payer,
    powerless, immediate, trapped, national).

% Face existential territorial loss from sea-level rise; adaptation cannot prevent national disappearance, only delay it or fund managed retreat. The adaptation-priority reading offers financial support for temporary resilience measures while the true solution—rapid global mitigation—is treated as a long-term aspiration. They are formally in climate negotiation spaces but their core interest (preventing inundation) is structurally displaced by adaptation financing frameworks.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, small_island_states, payer,
    powerless, immediate, trapped, regional).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__adaptation_priority_reading, small_island_states, excluded).

% Agricultural and pastoral communities in semi-arid and flood-prone regions experiencing immediate livelihood collapse from climate variability. Adaptation programs offer crop insurance, water management, livelihood diversification—all of which require capital and institutional capacity they lack. The constraint forces them into dependency on external adaptation finance while the structural driver (global emissions) remains outside any accountability framework accessible to them.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, subsistence_communities, payer,
    powerless, immediate, trapped, local).

% Inherit a world where present adaptation investments lock in high-cost emissions pathways: each year of deferred mitigation requires exponentially larger mitigation efforts later, or acceptance of catastrophic warming. The constraint front-loads adaptation costs to the present while back-loading mitigation impossibility to the future. They cannot voice preferences in present negotiations; their structural victimhood is prospective rather than current.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, future_generations_high_cost_pathways, payer,
    powerless, civilizational, trapped, global).

% Climate tech companies, water utilities, agricultural input suppliers, and engineering firms capture the adaptation finance flows. The constraint directs capital toward implementable, profitable technologies (precision irrigation, early-warning systems, climate-resilient seed varieties) rather than toward systemic emissions reduction, which offers lower margins and disrupts existing business models.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, technological_adaptation_vendors, beneficiary,
    powerful, biographical, mobile, global).

% Multilateral development banks, bilateral climate funds, and carbon markets administrators gain institutional authority and capital flows by packaging adaptation as the core mechanism for climate response. The adaptation-priority reading legitimizes their institutional role: they become the administrators of the transition, not the witnesses to systemic failure. They set project eligibility criteria, which determines what counts as 'adaptation' and thereby shapes resource flows.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, climate_finance_intermediaries, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__adaptation_priority_reading, climate_finance_intermediaries, agenda_setter).

% Renewable energy, nuclear, and industrial decarbonization companies face deprioritized capital allocation when adaptation is the primary frame. The constraint does not prevent their work but displaces it in the investment hierarchy, reducing the pressure for rapid scaling of emissions reduction technologies. They would argue for mitigation-first framing but are structurally sidelined in adaptation-focused climate finance mechanisms.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, mitigation_technology_developers, excluded,
    powerful, biographical, constrained, global).

% Populations in wealthy nations benefit from the constraint's implicit bargain: maintaining current consumption patterns while directing adaptation finance to vulnerable regions, framed as climate action. Public opinion in wealthy nations tends to support 'helping vulnerable countries adapt' as less disruptive politically than demanding domestic decarbonization.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, global_north_constituencies, beneficiary,
    organized, biographical, mobile, national).

% IPCC and climate science assessments document that adaptation alone cannot prevent dangerous warming and that rapid, deep mitigation is the primary lever. They occupy an analytical seat: producing evidence that the adaptation-priority reading underspecifies the problem, but lacking enforcement authority to redirect policy.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, scientific_consensus_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_imperative__adaptation_priority_reading, wealthy_nations_carbon_budgets).
narrative_ontology:fixing_cost_class(climate_response_imperative__adaptation_priority_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes climate response around immediate, measurable damage reduction in vulnerable regions: funds are allocated to resilience infrastructure, early warning systems, livelihood support, and climate-informed development. This solves the coordination problem of how to deploy scarce capital in a warming world with heterogeneous vulnerability.
% TRANSFER_FUNCTION: Moves climate finance from wealthy nations to developing nations and vulnerable regions, nominally for adaptation projects. Simultaneously redistributes agency: wealthy nations retain the right to set mitigation timelines (aspirational) while vulnerable nations bear the obligation to implement adaptation (funded but insufficient). The actual flow includes rent extraction by technology vendors and intermediaries.
% ABSENT_VOICES: Mitigation-priority advocates and degrowth scholars are structurally excluded from adaptation-focused climate finance governance. Small island states and subsistence communities are included in negotiation spaces but their core interest—global emissions reduction that would prevent their destruction—is treated as a secondary concern. Future generations cannot negotiate; their interests are absent by construction of the decision frame.
% DISAPPEARANCE_RATIONALE: If the adaptation-priority reading disappeared and were replaced by mitigation-priority or degrowth frameworks, climate finance flows would be redirected from resilience infrastructure to emissions reduction infrastructure and economic restructuring. Developing nations would face different immediate costs (energy transition investments) rather than adaptation debt, but the long-term trajectory would compress. The global distribution of climate costs would shift substantially.
% FOUNDING_PROBLEM: Climate impacts are already occurring; vulnerable populations face immediate threats from sea-level rise, drought, flooding, and heat. Mitigation takes decades; adaptation can begin now. The founding problem is the time lag between present damage and future mitigation benefits.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists, development economists, and vulnerable-nation governments all attest that present impacts are severe and adaptation is necessary. However, the same sources (IPCC, World Bank, civil society from vulnerable regions) also attest that adaptation alone is insufficient and that the founding problem is better solved by urgency on both fronts rather than prioritizing adaptation. The constraint's persistence rests on wealthy-nation governments' and climate-finance institutions' readings of the problem as justifying current mitigation pace; corroboration from outside the beneficiary set is ambivalent to contradictory.
narrative_ontology:disappearance_verdict(climate_response_imperative__adaptation_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__adaptation_priority_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__adaptation_priority_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_imperative__adaptation_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__adaptation_priority_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.82) because the constraint preserves wealthy-nation consumption patterns while shifting climate costs to vulnerable nations via adaptation finance dependency. The extraction is active and enforced (suppression 0.71): wealthy nations maintain veto power over mitigation timelines in climate negotiations, and adaptation-finance institutions structure project eligibility to reinforce the adaptation-priority frame rather than enabling deep systemic change. Theater ratio (0.44) reflects that adaptation work is genuinely needed but an increasing share of the constraint's function is political—maintaining the narrative that adaptation and aspirational mitigation are sufficient rather than inadequate to the problem. Accessibility collapse (0.68) is moderate: the adaptation frame is intellectually coherent and captures real immediate needs, but it displaces mitigation-priority and degrowth alternatives that would address the structural problem. Resistance (0.73) is substantial because vulnerable nations, island states, climate scientists, and justice advocates contest the framing, but their contestation occurs within a constraint structure (adaptation finance flows, mitigation negotiations) that they did not design and cannot easily exit. The measurement series shows extractiveness rising steeply in early years (0.68→0.82 over 30 years) as the constraint becomes institutionalized through adaptation-finance mechanisms and carbon markets, then plateauing as it reaches mature governance equilibrium.
 *
 * PERSPECTIVAL GAP:
 *   The wealthy-nation agenda-setter seats and the developing-nation payer seats compute radically different constraint types. From the wealthy-nation perspective, the constraint coordinates a pragmatic, immediate-action response to unavoidable climate impacts—a rope solving a genuine coordination problem. From the developing-nation and small-island perspective, the same structure operates as a snare: they face immediate material crisis, are offered adaptation finance contingent on adopting specific technologies and governance frameworks, and are locked out of the decision-making authority that determines mitigation timelines. The engine computes these divergences from the structural data: beneficiary seats have high exit options (arbitrage—they can shift to mitigation or degrowth readings without existential loss); victim seats are trapped (they cannot credibly exit adaptation framing without abandoning immediate survival needs). The perspectival gap is not a measurement error—it is the constraint's core extractive mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Wealthy nations and their carbon interests benefit from the constraint's preservation of industrial-era emission rights; they occupy the beneficiary/agenda-setter pole (d near 0.0). Developing nations, small island states, and subsistence communities bear immediate costs and face constrained exit (trapped by present impacts, by lack of capital, by power asymmetry in negotiation); they occupy the victim/payer pole (d near 1.0). Adaptation vendors and intermediaries sit between: they collect rents from adaptation finance flows but are not the primary extractors—they are derivative beneficiaries dependent on the constraint's persistence. Future generations are the deepest victim set but are excluded from the decision frame entirely. The constraint's enforcement depends on maintaining asymmetric information (presenting adaptation as the primary solution) and on controlling the institutions (multilateral banks, climate funds) that define project eligibility.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (present climate impacts) is live, but the adaptation-priority framing treats it as solved by resilience infrastructure while deferring the actual solution (emissions reduction) to an aspirational future. This is a classic mandatrophy pathway: as adaptation projects are completed and resilience is built, the constraint's raison d'être should diminish, but instead the adaptation-finance architecture becomes institutionalized as an end in itself rather than a means. The constraint persists not because the founding problem remains but because the institutional structures built to address it have become political economy actors defending their own continued existence. A Tangled Rope reading (coordination function + asymmetric extraction + active enforcement) captures this better than a Snare reading would, because there is a genuine coordination problem (how to respond to present climate impacts while building long-term resilience), but the coordination is hijacked to serve the extraction function (preserving wealthy-nation carbon budgets).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mitigation_sufficiency_frontier,
    'At what cumulative emissions level does adaptation become unable to prevent catastrophic warming, regardless of resilience spending?',
    'Climate model consensus on emissions budgets for 1.5°C, 2°C, and higher warming pathways; comparison of adaptation cost curves to carbon lock-in cost curves.',
    'If adaptation costs exceed mitigation costs at the point where catastrophic warming becomes unavoidable, the adaptation-priority framing is revealed as a high-cost pathway solving the wrong problem first. The constraint would be reclassified as a snare masquerading as tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mitigation_sufficiency_frontier, empirical, 'Whether adaptation can prevent catastrophic warming given present mitigation pace.').

omega_variable(
    capital_redistribution_concealment,
    'Does the adaptation-priority reading function primarily to defer the political economy of wealthy-nation decarbonization by framing the problem as a financing/engineering challenge in vulnerable regions rather than a structural change in wealthy-nation consumption?',
    'Historical analysis of climate finance flows and industrial decarbonization investment: if adaptation spending grows while wealthy-nation fossil fuel infrastructure capital recovery accelerates, the reading serves as political cover for extraction.',
    'If true, the constraint is a snare using a genuine coordination problem (present vulnerability) as cover for an extractive arrangement (preservation of wealthy-nation carbon budgets). The reading would persist not because of the founding problem''s validity but because it serves powerful interests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_redistribution_concealment, empirical, 'Whether adaptation-priority framing conceals unequal responsibility for emissions and costs.').

omega_variable(
    trapped_vs_constrained_exit,
    'Are developing nations genuinely trapped in adaptation dependence (no exit), or constrained (exit possible but costly)?',
    'Examine whether unilateral national mitigation investment or coalitional renegotiation of climate finance terms are plausible alternatives. Test whether climate impacts create irreversible lock-in or reversible vulnerability.',
    'If trapped, the constraint is structurally a snare; if constrained, it remains tangled rope (coerced coordination). The distinction affects the remediation pathway: trapped constraints require external intervention; constrained constraints can be exited through coordinated collective action.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trapped_vs_constrained_exit, empirical, 'Degree of agency retained by developing nations within the constraint.').

omega_variable(
    reading_vs_empirical_reality,
    'Is the adaptation-priority reading a valid response to climate physics and economics, or a political reading masquerading as technical necessity?',
    'Compare IPCC findings on the necessity of rapid mitigation alongside adaptation, against the reading''s implicit claim that mitigation can be aspirational. Examine whether the reading''s technical framing (adaptation is more shovel-ready, mitigation is slower) reflects physics or institutional incentive structures.',
    'If the reading conflicts with empirical climate science consensus, the constraint''s legitimacy rests entirely on political economy rather than problem-solving logic. The reading would be reclassifiable as a false natural law (mountain that is actually a snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_empirical_reality, empirical, 'Whether the adaptation-priority reading reflects climate physics or political interests.').

omega_variable(
    intergenerational_moral_standing,
    'Do present decision-makers have moral standing to defer mitigation costs to future generations on the grounds that adaptation benefits the present, when future generations face catastrophically higher costs?',
    'Normative analysis of intergenerational justice frameworks; empirical modeling of cost distribution across generations under different mitigation and adaptation pathways.',
    'If the deferral violates intergenerational justice principles, the constraint''s normative legitimacy collapses regardless of its technical framing. The reading would be classified as extractive on its face—as an intergenerational transfer from future generations to the present.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_moral_standing, preference, 'Whether the constraint''s temporal distribution of costs is morally defensible.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__adaptation_priority_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_imperative__adaptation_priority_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(clim_tr_t5, climate_response_imperative__adaptation_priority_reading, theater_ratio, 5, 0.36).
narrative_ontology:measurement(clim_tr_t10, climate_response_imperative__adaptation_priority_reading, theater_ratio, 10, 0.39).
narrative_ontology:measurement(clim_tr_t15, climate_response_imperative__adaptation_priority_reading, theater_ratio, 15, 0.41).
narrative_ontology:measurement(clim_tr_t20, climate_response_imperative__adaptation_priority_reading, theater_ratio, 20, 0.43).
narrative_ontology:measurement(clim_tr_t25, climate_response_imperative__adaptation_priority_reading, theater_ratio, 25, 0.44).
narrative_ontology:measurement(clim_tr_t30, climate_response_imperative__adaptation_priority_reading, theater_ratio, 30, 0.44).
narrative_ontology:measurement(clim_tr_t35, climate_response_imperative__adaptation_priority_reading, theater_ratio, 35, 0.44).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(clim_be_t5, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 5, 0.71).
narrative_ontology:measurement(clim_be_t10, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(clim_be_t15, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 15, 0.78).
narrative_ontology:measurement(clim_be_t20, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(clim_be_t25, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 25, 0.81).
narrative_ontology:measurement(clim_be_t30, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 30, 0.82).
narrative_ontology:measurement(clim_be_t35, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 35, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(clim_su_t5, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(clim_su_t10, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(clim_su_t15, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(clim_su_t20, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(clim_su_t25, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(clim_su_t30, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(clim_su_t35, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 35, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__adaptation_priority_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_imperative__adaptation_priority_reading, 0.18).
narrative_ontology:affects_constraint(climate_response_imperative__adaptation_priority_reading, climate_response_imperative__mitigation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__adaptation_priority_reading, climate_response_imperative__degrowth_reading).

% DUAL FORMULATION NOTE:
% The climate_response_imperative kernel admits three structurally distinct readings, each with different ε values, beneficiary/victim structures, and policy timelines. This constraint (adaptation-priority) instantiates the reading that prioritizes resilience in vulnerable regions, treating mitigation as aspirational. The mitigation-priority and degrowth readings are separate constraint files, linked via this network field. All three readings share the same kernel but emit different classifications: this reading computes as Tangled Rope (coordination function + asymmetric extraction + enforcement); the mitigation-priority reading likely computes as Rope or Tangled Rope depending on carbon-market enforcement; the degrowth reading likely computes as Snare if the required structural changes are treated as infeasible. The three readings coexist in global climate governance—different nations and institutions hold each simultaneously—but compete for policy influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_imperative__adaptation_priority_reading, powerful, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
