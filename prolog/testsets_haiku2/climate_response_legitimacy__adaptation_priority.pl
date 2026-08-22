% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: climate_response_legitimacy__adaptation_priority
 *   human_readable: Climate Adaptation Priority Framing
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The adaptation-priority framing accepts a warming trajectory (2.5–3.0°C
 *   above pre-industrial) as structurally inevitable given political
 *   inability to reduce emissions rapidly, and redirects climate response
 *   toward protecting vulnerable populations through resilience
 *   infrastructure and adaptive capacity. It is presented as a legitimate,
 *   pragmatic response to political reality. Yet it simultaneously preserves
 *   wealthy nations' growth-dependent development models, concentrates
 *   adaptation benefits on technology vendors and finance institutions, and
 *   defers intergenerational costs. Low-income vulnerable regions enter the
 *   victim set immediately via the adaptation-finance deficit ($350B annual
 *   gap); future generations face compounded impacts from higher warming plus
 *   exhausted adaptation budgets. The constraint's extractiveness accumulates
 *   over time (0.35→0.68) as the gap between promised and delivered
 *   adaptation widens and as future warming impacts materialize faster than
 *   adaptation infrastructure can expand.
 *
 * KEY AGENTS:
 *   - Wealthy developed nations: institutional beneficiaries; shape adaptation mandate; preserve growth model
 *   - Low-income vulnerable regions: structural victims; immediate impacts; trapped by adaptation-finance dependence
 *   - Climate-displaced populations: identity-locked victims; accept framing through no real choice
 *   - Future generations: voiceless victims; inherit compounded warming and depleted adaptation budgets
 *   - Adaptation finance institutions: institutional beneficiaries; administer mandate; control eligibility
 *   - Technology vendors: organized beneficiaries; sell adaptation solutions; capture global margin
 *   - Rapid-mitigation advocates: excluded parties; argue for faster emissions reduction; heard in side events, not governance
 *   - Global North labor unions: excluded parties; interests represented through industry opposition to rapid transition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__adaptation_priority, 0.68).
domain_priors:suppression_score(climate_response_legitimacy__adaptation_priority, 0.72).
domain_priors:theater_ratio(climate_response_legitimacy__adaptation_priority, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__adaptation_priority, "Climate Adaptation Priority Framing").
narrative_ontology:topic_domain(climate_response_legitimacy__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__adaptation_priority, '12bd694a-c8f9-469b-87fe-6d1740af108f').
narrative_ontology:cs_kernel_codification('12bd694a-c8f9-469b-87fe-6d1740af108f', distributed).
narrative_ontology:cs_authority_grounding('12bd694a-c8f9-469b-87fe-6d1740af108f', extraction).
narrative_ontology:cs_interpretation_layer_present('12bd694a-c8f9-469b-87fe-6d1740af108f').
narrative_ontology:cs_reading_relation('12bd694a-c8f9-469b-87fe-6d1740af108f', climate_response_legitimacy__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('12bd694a-c8f9-469b-87fe-6d1740af108f', climate_response_legitimacy__degrowth_transformation, coexists_with).
narrative_ontology:cs_axiom('12bd694a-c8f9-469b-87fe-6d1740af108f', foundational, warming_trajectory_inevitable).
narrative_ontology:cs_axiom_status(warming_trajectory_inevitable, holdable).
narrative_ontology:cs_axiom_grounding('12bd694a-c8f9-469b-87fe-6d1740af108f', warming_trajectory_inevitable, empirically_contingent).
narrative_ontology:cs_axiom('12bd694a-c8f9-469b-87fe-6d1740af108f', foundational, adaptation_superior_to_mitigation).
narrative_ontology:cs_axiom_status(adaptation_superior_to_mitigation, holdable).
narrative_ontology:cs_axiom_grounding('12bd694a-c8f9-469b-87fe-6d1740af108f', adaptation_superior_to_mitigation, instrumental).
narrative_ontology:cs_reference_frame('12bd694a-c8f9-469b-87fe-6d1740af108f', politically_constrained_emissions_trajectory).
narrative_ontology:cs_drift_state('12bd694a-c8f9-469b-87fe-6d1740af108f', contemporary_renewable_cost_collapse, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('12bd694a-c8f9-469b-87fe-6d1740af108f', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, wealthy_developed_nations).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, capital_intensive_industries).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, low_income_vulnerable_regions).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, climate_displaced_populations).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, low_income_vulnerable_regions).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, renewable_and_adaptation_technology_vendors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% High-income OECD nations with industrial carbon debt. Under adaptation-priority framing, they externalize decarbonization timelines while preserving growth-dependent development models. They set the adaptation mandate through international climate governance, controlling funding eligibility criteria and technical standards. They benefit by avoiding disruption to domestic energy, agriculture, and industrial systems while claiming leadership through adaptation finance.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, wealthy_developed_nations, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__adaptation_priority, wealthy_developed_nations, agenda_setter).

% Small island states, Least Developed Countries (LDCs), and sub-Saharan African regions experiencing first-order climate impacts (drought, flooding, sea-level rise) despite minimal historical emissions. They are mandated to invest in resilience infrastructure to adapt to warming they did not cause. They depend on adaptation finance from wealthy nations, which arrives with strings attached (debt burden, policy conditionality, technological lock-in). Their choice set is constrained: accept the warming trajectory, seek adaptation funds under wealthy-nation terms, or face catastrophic impacts unsupported.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, low_income_vulnerable_regions, payer,
    powerless, immediate, trapped, regional).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__adaptation_priority, low_income_vulnerable_regions, beneficiary).

% Communities losing land, livelihoods, and cultural identity to climate impacts within the adaptation horizon. Adaptation narratives frame them as inevitable losses to be managed through relocation, livelihood shift, or climate migration facilitation — rather than as preventable harms if emissions were faster-reduced. Identity is fused with place and livelihood; exit is cultural death disguised as adaptation. The adaptation-priority framing accepts their displacement as a cost of the legitimate response.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, climate_displaced_populations, payer,
    powerless, immediate, identity_locked, local).

% Agents not yet in the decision structure. Adaptation-priority framing defers deeper emissions reduction, which compounds warming impacts across the 21st century. Future generations inherit both the warming trajectory and the exhausted adaptation infrastructure budget — earlier generations will have consumed the global capacity to invest in resilience, leaving later generations to face 2.5–3.0°C warming with fewer resources. The intergenerational cost is externalized to the voiceless.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% World Bank, regional development banks, bilateral climate funds, and private-sector climate finance mechanisms. They administer the adaptation mandate, set technical standards, allocate funds, and attach conditionality (macroeconomic policy, trade openness, fossil fuel phaseout timelines that preserve wealthy-nation industrial interests). They benefit from growing adaptation markets and expanded institutional scope; they enforce the adaptation-priority framing by withholding funds from nations that pursue alternative responses.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, adaptation_finance_institutions, agenda_setter,
    institutional, biographical, mobile, global).

% Solar manufacturers, battery producers, climate-resilient agriculture companies, water engineering firms. They sell adaptation and low-carbon technology into the adaptation-finance market. They benefit from the adaptation-priority framing, which creates long-term technology demand; they are often headquartered in wealthy nations and capture margin on technology transfers to vulnerable regions. The framing does not threaten their core business model — it creates demand for it.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, renewable_and_adaptation_technology_vendors, beneficiary,
    organized, biographical, mobile, global).

% Scientists, NGOs, and advocacy coalitions arguing for rapid emissions reduction (1.5–2.0°C pathways) and/or structural economic transformation. They would argue that accepting a warming trajectory and prioritizing adaptation is a dangerous postponement that condemns vulnerable populations and future generations to avoidable suffering. They are excluded from high-level climate negotiation forums dominated by adaptation-finance rhetoric; their arguments are heard in side events and scientific papers, not in the binding policy set.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, rapid_mitigation_advocates, excluded,
    organized, generational, constrained, global).

% Workers in high-carbon sectors (fossil fuels, heavy industry, transport) in wealthy nations. A rapid-mitigation or degrowth response would disrupt their employment and income. Under adaptation-priority framing, their jobs are preserved longer, and the framing's implicit preservation of growth-dependent development delays the structural economic transition that would most threaten their sector. They are excluded from adaptation governance; their voices are represented only through industry lobbyists or as a vague 'just transition' concept that adaptation-priority framing does not require to implement.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, global_north_labor_unions, excluded,
    organized, biographical, constrained, national).

% IPCC, UNFCCC, climate scientists, and peer-reviewed research infrastructure. They produce the technical legitimacy claims that frame climate response options. They observe the adaptation-priority framing and measure whether it aligns with emissions pathways and warming outcomes, or whether it represents post-hoc narrative choice decoupled from climate physics.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, climate_governance_scientific_authority, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_legitimacy__adaptation_priority, wealthy_developed_nations).
narrative_ontology:fixing_cost_class(climate_response_legitimacy__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates global adaptation finance to vulnerable populations facing immediate climate impacts; coordinates technical standards for resilience infrastructure (water systems, agricultural adaptation, disaster-resistant housing); establishes legitimacy conditions for climate response that wealthy nations can implement without dismantling growth-dependent development.
% TRANSFER_FUNCTION: Moves adaptation finance from wealthy nations to vulnerable regions (insufficient to need — estimated $350B annual gap); moves future climate impacts forward onto low-income populations and future generations; preserves carbon-intensive development pathways in high-income nations while mandating efficiency and resilience investment in low-income regions.
% ABSENT_VOICES: Rapid-mitigation advocates and degrowth theorists are excluded from the primary framing — their argument that accepting a warming trajectory is structurally unjust because emissions reduction is technically and economically feasible is heard in academic venues but not in the governance set. Global North labor unions are excluded but represented through industry interests that oppose rapid transition. Future generations have no voice in present governance; their interests are abstract appeals, not seated negotiators.
% DISAPPEARANCE_RATIONALE: If the adaptation-priority framing disappeared, global climate response would reorient toward rapid emissions reduction (mitigation-priority) or structural economic transformation (degrowth), which would disrupt wealthy-nation energy, transport, and industrial systems immediately rather than phasing them over decades. The development model that preservation of growth-dependent systems enables would collapse. Vulnerable regions would face demands for faster structural change alongside climate impacts; intergenerational costs would shift from deferred warming to immediate economic transformation.
% FOUNDING_PROBLEM: Early climate negotiation deadlock between wealthy nations (unwilling to reduce emissions rapidly) and vulnerable nations (facing immediate climate impacts with minimal capacity to adapt). The adaptation-priority framing resolved deadlock by accepting a warming trajectory while committing to finance adaptation for vulnerable populations — a compromise that allowed negotiation to proceed without threatening wealthy-nation growth models.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists and economists outside the adaptation-finance establishment attest that the founding problem (deadlock) has been replaced by a different problem: emissions continue on high trajectories despite 30 years of adaptation-priority investment, and the adaptation finance itself is insufficient and conditional. Vulnerable-nation negotiators attest privately (in confidential UN documents and leaked summaries) that adaptation finance was their fallback position when rapid-mitigation demands were rejected by wealthy nations, not their preference. The finding that the problem is dead is corroborated by the absence of rapid emissions reduction and the accumulation of unmet adaptation needs.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__adaptation_priority, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_legitimacy__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__adaptation_priority, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises steeply from 1995 (0.35, when adaptation finance was novel and seemed complementary to mitigation) to 2025 (0.65, when the gap between promised and delivered adaptation became visible and emissions continued rising). It plateaus post-2035 because the constraint reaches equilibrium: adaptation becomes the default response, competing mitigation pathways are institutionally foreclosed, and the feedback is set. Theater ratio climbs from 0.12 to 0.42, reflecting growing gap between adaptation-finance announcements and actual climate impact reduction. Suppression requirement rises from 0.38 to 0.72, tracking the enforcement burden of maintaining the adaptation-priority framing against growing evidence that rapid emissions reduction is both technically feasible and economically preferable — suppression sustains the narrative against contradicting evidence. Measurements are authored on one shared grid (every metric at every time point) so temporal analysis can detect when extraction accumulation began (2005–2015, when the founding problem became dead but the framing persisted) and when theater exceeded function (2015 onward). The measurements span 1995–2050 to capture the constraint's lifecycle from early international agreement through projected endpoint under current policy trends.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (wealthy nations, adaptation-finance institutions) experiences the framing as legitimate pragmatism and coordination success — they have constructed a global response that mobilizes finance, sets standards, and operates without threatening their core interests. The payer seats (low-income regions, displaced populations, future generations) experience the same structure as deferred catastrophe masked by adaptation-priority language. The institutional beneficiary will compute this constraint as coordination (genuine problem solved) from their seat; the powerless victim will compute it as extraction (asymmetric burden shifted onto them). The engine's per-seat classification will reveal this divergence from structural data: high d (directionality toward target) for powerless victims; low d for institutional beneficiaries. The claim/metric gap is intentional: the constraint is CLAIMED as tangled_rope (coordination + enforcement + asymmetry) while the metrics describe substantially extractive operation — the divergence is exactly what a false coordination claim produces.
 *
 * DIRECTIONALITY LOGIC:
 *   Wealthy developed nations sit near d=0.15 (full beneficiary): they receive the coordination benefit (deferred climate risk to themselves) without bearing the adaptation costs (externalized to vulnerable regions). They have arbitrage-grade exit: if adaptation-priority fails, they can transition to rapid mitigation with industrial disruption; that disruption is their exit cost, but not a barrier. Low-income vulnerable regions sit near d=0.95 (full target): they bear the immediate adaptation burden (infrastructure investment, crop shifts, migration) while receiving insufficient finance; they face compounded future impacts from higher warming. They are trapped — exit means climate catastrophe unsupported. Future generations sit at d=1.0 (pure target): they have no seat in present negotiation but inherit the warming trajectory and the exhausted adaptation budget. Their exit is forcibly deferred. The directionality derivation chain runs: beneficiary/victim declarations (wealthy=beneficiary, low-income=victim, future-generations=victim) → trapped/identity_locked exit options for victims → institutional power for beneficiaries → high d for victims, low d for beneficiaries. This produces the asymmetric extraction the metrics capture.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is dead: global emissions continue rising despite 30 years of adaptation-priority framing, indicating that the framing has failed to produce its core coordination function (reducing emissions while protecting vulnerable populations). Yet the constraint persists — not because the founding problem remains live, but because wealthy nations and adaptation-finance institutions benefit from maintaining the framing. This is mandatrophy. The constraint's secondary function (financing adaptation) is real but overwhelmed by its tertiary function (preserving wealthy-nation growth models). The adaptation-priority framing is an example of a constraint whose manifest function (adapting to unavoidable climate change) has become secondary to its actual function (deferring rapid decarbonization). Theater rises to 0.42 by 2025 because the constraint increasingly operates through adaptation-finance announcements and COP commitments that are not delivered or scaled to actual need. The engine's mandatrophy detector should flag (status=dead + verdict=world_rearranges + theater rising to 0.4+) as a constraint whose founding problem has been superseded by extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptation_sufficiency_empirical,
    'Is the global adaptation-finance commitment ($100B annually promised, unmet) structurally sufficient to protect vulnerable populations from warming above 1.5–2.0°C, or does it represent an intentionally undersized fund designed to be unmet?',
    'Empirical comparison of (1) actual adaptation finance delivered vs. climate impact costs in vulnerable regions, and (2) economists'' estimates of adaptive capacity needs for 2.5–3.0°C warming scenarios.',
    'If adaptation finance is empirically insufficient, the constraint''s extraction rises to 0.85+ (victims cannot achieve the promised protection). If it is intentionally undersized by design, suppression rises above 0.85 (the constraint''s framing obscures its structural function). Either finding confirms mandatrophy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adaptation_sufficiency_empirical, empirical, 'Whether promised adaptation finance is sufficient or designed to be inadequate.').

omega_variable(
    emissions_reduction_feasibility,
    'Is rapid emissions reduction to 1.5–2.0°C warming pathways technically and economically feasible within wealthy-nation growth models, or does decarbonization structurally require degrowth or post-growth frameworks?',
    'Systematic comparison of (1) mitigation cost and feasibility studies (IPCC, IEA) vs. (2) historical decoupling data for wealthy nations, and (3) scenario models testing whether 1.5°C is reachable without structural economic change or only within degrowth frameworks.',
    'If rapid mitigation is feasible within growth models, the adaptation-priority framing is choice (wealthy nations choosing extraction over solutions). If rapid mitigation requires degrowth, the constraint''s existence masks a harder structural question: whether wealthy nations can legitimately preserve growth while accepting impacts on others. This shifts the constraint''s classification toward snare (pure extraction cover story).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emissions_reduction_feasibility, empirical, 'Whether rapid decarbonization requires degrowth or is compatible with growth-dependent systems.').

omega_variable(
    identity_lock_permanence_interpersonal,
    'For climate-displaced populations, does the suppression of displacement-resistance operate through structural barriers (economic dependence, legal rights denial) or through internalized identity fusion (self-concept as ''climate victim'', place-identity as ''vulnerable'', future as ''adaptation subjects'')?',
    'Post-displacement trajectory analysis: measure resistance/agency in displaced populations (1) while dependent on adaptation finance, and (2) if adaptation finance and external governance were removed. Persistent suppression after removal signals internalized fusion; disappearance signals structural suppression alone.',
    'If internalized, the constraint''s effective suppression on displaced populations is higher than the structural measure (0.72) indicates — they carry the suppression narrative into post-displacement contexts and resist alternative framings. If structural, the suppression is context-dependent and could shift rapidly if external barriers dissolved. Internalized suppression suggests longer-term identity lock and higher victim vulnerability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_permanence_interpersonal, empirical, 'Whether displacement suppression is structural or internalized identity fusion.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the adaptation-priority reading''s core premise (accept warming trajectory as inevitable) logically foreclose the mitigation-priority reading (emissions reduction is feasible), or do both readings remain simultaneously holdable by different parties?',
    'Examine whether a party could coherently accept both (1) ''rapid emissions reduction is technically feasible but politically impossible, so we prioritize adaptation'' AND (2) ''we pursue both rapid mitigation and adaptation.'' If coherent (reading as pragmatic demotion rather than denial), readings coexist. If incoherent (reading as denial that alternatives exist), mitigation-priority is foreclosed.',
    'If readings foreclose each other, the constraint carries higher epistemic power — it commits to a specific climate physics narrative (rapid mitigation is impossible). If readings coexist, the constraint is political choice (pragmatic downranking of mitigation) rather than structural necessity. Foreclosure would elevate the constraint''s influence/prestige; coexistence would identify it as a deliberate choice among alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether adaptation-priority reading forecloses rapid-mitigation reading or both are simultaneously holdable.').

omega_variable(
    intergenerational_discounting_coercion,
    'Does the adaptation-priority framing employ ethical coercion by treating future-generation interests as abstract and unrepresentable, thereby making rapid-mitigation responses seem unnecessary when cheaper adaptation-focused responses exist in the present?',
    'Examine whether the framing operationally treats future-generation harm as (1) discounted by standard economic discount rates (making present-cost adaptation seem preferable), or (2) undiscountable (making future-equivalent emissions reduction seem required). If discounted, the framework silently transfers intergenerational cost from present (via emissions) to future (via higher warming).',
    'If intergenerational discounting is operative, the constraint''s extraction of future-generation welfare is structural and baked into the framing''s cost-benefit logic — not a side effect but a design feature. This would support the finding that the constraint transfers costs across time to beneficiaries in the present, a form of extraction not visible in single-generation metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_discounting_coercion, preference, 'Whether adaptation-priority framing operationally discounts future-generation interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__adaptation_priority, 1995, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(climate_adapt_tr_t1995, climate_response_legitimacy__adaptation_priority, theater_ratio, 1995, 0.12).
narrative_ontology:measurement(climate_adapt_tr_t2005, climate_response_legitimacy__adaptation_priority, theater_ratio, 2005, 0.18).
narrative_ontology:measurement(climate_adapt_tr_t2015, climate_response_legitimacy__adaptation_priority, theater_ratio, 2015, 0.28).
narrative_ontology:measurement(climate_adapt_tr_t2025, climate_response_legitimacy__adaptation_priority, theater_ratio, 2025, 0.38).
narrative_ontology:measurement(climate_adapt_tr_t2035, climate_response_legitimacy__adaptation_priority, theater_ratio, 2035, 0.42).
narrative_ontology:measurement(climate_adapt_tr_t2050, climate_response_legitimacy__adaptation_priority, theater_ratio, 2050, 0.42).

% Extraction over time
narrative_ontology:measurement(climate_adapt_be_t1995, climate_response_legitimacy__adaptation_priority, base_extractiveness, 1995, 0.35).
narrative_ontology:measurement(climate_adapt_be_t2005, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2005, 0.47).
narrative_ontology:measurement(climate_adapt_be_t2015, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement(climate_adapt_be_t2025, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2025, 0.65).
narrative_ontology:measurement(climate_adapt_be_t2035, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2035, 0.68).
narrative_ontology:measurement(climate_adapt_be_t2050, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2050, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(climate_adapt_su_t1995, climate_response_legitimacy__adaptation_priority, suppression_requirement, 1995, 0.38).
narrative_ontology:measurement(climate_adapt_su_t2005, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2005, 0.52).
narrative_ontology:measurement(climate_adapt_su_t2015, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2015, 0.64).
narrative_ontology:measurement(climate_adapt_su_t2025, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2025, 0.7).
narrative_ontology:measurement(climate_adapt_su_t2035, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2035, 0.72).
narrative_ontology:measurement(climate_adapt_su_t2050, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2050, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__adaptation_priority, global_infrastructure).
narrative_ontology:boltzmann_floor_override(climate_response_legitimacy__adaptation_priority, 0.25).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__degrowth_transformation).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_finance_architecture).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, carbon_pricing_and_markets).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, energy_transition_pathways).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the climate_response_legitimacy kernel. Sibling readings (mitigation-priority and degrowth-transformation) are separate constraint stories with different ε, beneficiary/victim structures, and extraction profiles. The three readings compete in global climate governance; they are not reconcilable into a single constraint. See commentary.kernel_context and cs_structure.reading_relations for the structural relationships between them. All three stories should link via network.affects_constraints to form a constraint family enabling comparative analysis of how different climate-response framings produce different victim sets and extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_legitimacy__adaptation_priority, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
