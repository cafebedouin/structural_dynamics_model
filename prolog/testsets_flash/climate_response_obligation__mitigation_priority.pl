% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_obligation__mitigation_priority, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_response_obligation__mitigation_priority
 *   human_readable: Climate Response Obligation: Mitigation Priority
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint represents the 'mitigation_priority' reading of the
 *   broader 'climate_response_obligation' kernel. It asserts that
 *   intergenerational justice requires rapid decarbonization to minimize
 *   future warming and prevent harm. This reading places the primary burden
 *   of transition costs on the current generation, particularly high-emitting
 *   nations and industries, while future generations and vulnerable
 *   ecosystems are the primary beneficiaries. The constraint is a Tangled
 *   Rope because it genuinely coordinates collective action to address a
 *   global externality (climate change) but does so with significant,
 *   asymmetric extraction from specific industries and consumer groups,
 *   requiring active enforcement to overcome resistance.
 *
 * KEY AGENTS:
 *   - future_generations: Primary beneficiary (powerless/civilizational)
 *   - vulnerable_ecosystems: Primary beneficiary (powerless/civilizational)
 *   - fossil_fuel_industries: Primary victim (powerful/biographical)
 *   - high_emitting_nations: Primary victim (institutional/generational)
 *   - carbon_intensive_consumers: Secondary victim (moderate/biographical)
 *   - climate_activists: Agenda setter (organized/generational)
 *   - international_climate_governance: Agenda setter (institutional/generational)
 *   - global_south_nations: Beneficiary/Payer (organized/generational)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__mitigation_priority, 0.65).
domain_priors:suppression_score(climate_response_obligation__mitigation_priority, 0.4).
domain_priors:theater_ratio(climate_response_obligation__mitigation_priority, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, extractiveness, 0.65).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__mitigation_priority, "Climate Response Obligation: Mitigation Priority").
narrative_ontology:topic_domain(climate_response_obligation__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__mitigation_priority, '2d3cc4ed-b85d-445a-872f-2035de7da6f0').
narrative_ontology:cs_kernel_codification('2d3cc4ed-b85d-445a-872f-2035de7da6f0', formalized).
narrative_ontology:cs_authority_grounding('2d3cc4ed-b85d-445a-872f-2035de7da6f0', lineage).
narrative_ontology:cs_interpretation_layer_present('2d3cc4ed-b85d-445a-872f-2035de7da6f0').
narrative_ontology:cs_reading_relation('2d3cc4ed-b85d-445a-872f-2035de7da6f0', climate_response_obligation__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('2d3cc4ed-b85d-445a-872f-2035de7da6f0', climate_response_obligation__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('2d3cc4ed-b85d-445a-872f-2035de7da6f0', foundational, intergenerational_equity_demands_prevention).
narrative_ontology:cs_axiom_status(intergenerational_equity_demands_prevention, holdable).
narrative_ontology:cs_axiom_grounding('2d3cc4ed-b85d-445a-872f-2035de7da6f0', intergenerational_equity_demands_prevention, deontological).
narrative_ontology:cs_axiom('2d3cc4ed-b85d-445a-872f-2035de7da6f0', foundational, planetary_boundaries_require_decarbonization).
narrative_ontology:cs_axiom_status(planetary_boundaries_require_decarbonization, holdable).
narrative_ontology:cs_axiom_grounding('2d3cc4ed-b85d-445a-872f-2035de7da6f0', planetary_boundaries_require_decarbonization, empirically_contingent).
narrative_ontology:cs_reference_frame('2d3cc4ed-b85d-445a-872f-2035de7da6f0', scientific_consensus_on_planetary_boundaries).
narrative_ontology:cs_drift_state('2d3cc4ed-b85d-445a-872f-2035de7da6f0', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2d3cc4ed-b85d-445a-872f-2035de7da6f0', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__mitigation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, vulnerable_ecosystems).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, fossil_fuel_industries).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, high_emitting_nations).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, carbon_intensive_consumers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, global_south_nations).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, global_south_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Will benefit from a stable climate and reduced environmental harm, but have no direct agency in current policy decisions. Their well-being is entirely dependent on the actions of current generations.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(climate_response_obligation__mitigation_priority, future_generations).

% Benefit from reduced warming and climate impacts, which preserves biodiversity and ecological services. They have no agency and are entirely dependent on human action.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, vulnerable_ecosystems, beneficiary,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(climate_response_obligation__mitigation_priority, vulnerable_ecosystems).

% Bear significant costs from decarbonization policies (e.g., carbon taxes, regulations, divestment leading to stranded assets). Their business model is directly challenged, forcing costly transitions or decline.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, fossil_fuel_industries, payer,
    powerful, biographical, constrained, global).

% Bear a disproportionate burden of mitigation costs due to historical emissions and current economic structures. They face pressure to implement costly decarbonization policies and provide climate finance.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, high_emitting_nations, payer,
    institutional, generational, constrained, global).

% Bear costs through higher prices for energy, transportation, and goods, as well as lifestyle changes required for decarbonization. Their choices are constrained by available infrastructure and policy incentives.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, carbon_intensive_consumers, payer,
    moderate, biographical, constrained, national).

% Advocate for and push the implementation of rapid decarbonization policies. They invest significant social and political capital to maintain the mitigation priority, benefiting from policy wins but facing burnout and political backlash.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, climate_activists, agenda_setter,
    organized, generational, mobile, global).

% Develops and enforces international agreements and frameworks for climate mitigation (e.g., UNFCCC, Paris Agreement). They coordinate global action but are constrained by national sovereignty and political will.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, international_climate_governance, agenda_setter,
    institutional, generational, constrained, global).

% Are primary beneficiaries of reduced warming, as they are often most vulnerable to climate impacts. However, they also bear some transition costs and often demand climate finance and technology transfer from high-emitting nations to enable their own decarbonization.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, global_south_nations, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__mitigation_priority, global_south_nations, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_obligation__mitigation_priority, diffuse).
narrative_ontology:fixing_cost_class(climate_response_obligation__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global efforts to reduce greenhouse gas emissions by setting targets, developing policies, and allocating responsibilities to prevent catastrophic climate change, a classic collective action problem.
% TRANSFER_FUNCTION: Transfers economic costs (e.g., investment in renewables, carbon taxes, stranded assets) from current high-emitting industries and nations to future generations (in the form of reduced climate damages) and to vulnerable ecosystems (in the form of preserved biodiversity).
% ABSENT_VOICES: Future generations and non-human ecosystems are structurally absent from current decision-making, though their interests are represented by advocates. If present, they would unequivocally demand more aggressive mitigation. The voices of those who would benefit from a 'degrowth' approach (e.g., advocates for sufficiency over efficiency) are also largely excluded from mainstream policy debates.
% DISAPPEARANCE_RATIONALE: If the obligation to prioritize mitigation vanished, global emissions would likely accelerate, leading to more severe climate impacts. This would fundamentally rearrange global ecosystems, human societies, and economies, shifting resources from prevention to adaptation and disaster response, and exacerbating existing inequalities.
% FOUNDING_PROBLEM: The problem was the existential threat of anthropogenic climate change, driven by greenhouse gas emissions, leading to irreversible environmental degradation and harm to future generations.
% FOUNDING_PROBLEM_CORROBORATION: The scientific consensus (IPCC reports), international bodies (UNFCCC), and a broad coalition of civil society organizations and vulnerable nations corroborate that the founding problem is not only live but intensifying. While some fossil fuel interests dispute the urgency, their claims are not widely corroborated by independent scientific or ethical bodies.
narrative_ontology:disappearance_verdict(climate_response_obligation__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__mitigation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__mitigation_priority, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_response_obligation__mitigation_priority, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_obligation__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the substantial economic and social costs of rapid decarbonization, particularly for industries reliant on fossil fuels and nations with carbon-intensive economies. Suppression (0.40) is moderate but rising, as policies like carbon taxes, regulations, and divestment campaigns actively suppress high-carbon activities. Resistance (0.85) is high due to the concentrated costs on powerful incumbent industries and the diffuse nature of benefits. Theater ratio (0.20) is relatively low, indicating that most actions are genuinely aimed at mitigation, though some 'greenwashing' exists. Accessibility collapse (0.70) is high because the scientific consensus on planetary boundaries makes 'business as usual' increasingly untenable as a viable alternative.
 *
 * PERSPECTIVAL GAP:
 *   Future generations experience this as a Mountain or Rope, as it secures their future habitability. Fossil fuel industries and high-emitting nations experience it as a Snare, as it directly targets their economic models and requires significant, costly transformation. Climate activists and international governance bodies view it as a necessary Tangled Rope, balancing collective good with unavoidable costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations and vulnerable ecosystems are full beneficiaries (d=0.0) as the constraint directly protects their existence and well-being. Fossil fuel industries and high-emitting nations are full targets (d=1.0) due to stranded assets and mandated transitions. Carbon-intensive consumers are targets (d=0.8) as they bear costs through higher prices and lifestyle changes. Climate activists and international governance are agenda-setters (d=0.2) as they drive the policy and enforcement, benefiting from the collective good but also bearing the political costs of implementation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as the 'mandate' (preventing climate harm) is projected to remain live for centuries. However, the specific 'mitigation_priority' approach could become mandatrohpic if adaptation becomes demonstrably more effective or less costly, or if technological breakthroughs render current mitigation strategies obsolete. The high resistance and contested founding problem status indicate that the constraint's legitimacy is under constant challenge, preventing it from becoming an inert Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intergenerational_discount_rate,
    'What is the appropriate discount rate for future harms and benefits in climate policy?',
    'Ethical consensus on intergenerational equity, or political negotiation on long-term value frameworks.',
    'A high discount rate would reduce the perceived urgency and cost-effectiveness of mitigation, potentially shifting the constraint towards an ''adaptation_priority'' reading. A low or zero discount rate would amplify the imperative for immediate, deep mitigation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_discount_rate, preference, 'The choice of intergenerational discount rate fundamentally alters the perceived costs and benefits of mitigation.').

omega_variable(
    mitigation_vs_adaptation_tradeoff,
    'Is the ''mitigation_priority'' reading genuinely distinct from ''adaptation_priority'', or do they represent a continuous spectrum of response where the optimal balance is contested?',
    'Empirical evidence on the feasibility and cost-effectiveness of preventing vs. adapting to specific warming levels, combined with ethical frameworks on acceptable levels of residual harm.',
    'If the readings are fundamentally incommensurable, the conflict is a zero-sum game. If they are points on a spectrum, the constraint might be reclassified as a ''tangled_rope'' where the balance of coordination and extraction shifts based on the chosen point.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mitigation_vs_adaptation_tradeoff, conceptual, 'Ambiguity in the structural distinction between prioritizing mitigation versus adaptation.').

omega_variable(
    carbon_budget_accuracy,
    'How accurate are the remaining global carbon budgets for limiting warming to 1.5°C or 2°C, and how does this affect the urgency of mitigation?',
    'Improved climate modeling, better understanding of Earth system feedbacks, and more precise measurement of current emissions and sinks.',
    'If carbon budgets are smaller than currently estimated, the urgency for mitigation increases dramatically, potentially pushing the constraint towards a ''snare'' for high-emitting actors. If larger, it might relax the perceived stringency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carbon_budget_accuracy, empirical, 'Uncertainty in the scientific basis for mitigation targets.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__mitigation_priority, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_obligation__mitigation_priority, theater_ratio, 0, 0.1).
narrative_ontology:measurement(clim_tr_t5, climate_response_obligation__mitigation_priority, theater_ratio, 5, 0.12).
narrative_ontology:measurement(clim_tr_t10, climate_response_obligation__mitigation_priority, theater_ratio, 10, 0.15).
narrative_ontology:measurement(clim_tr_t15, climate_response_obligation__mitigation_priority, theater_ratio, 15, 0.18).
narrative_ontology:measurement(clim_tr_t20, climate_response_obligation__mitigation_priority, theater_ratio, 20, 0.2).
narrative_ontology:measurement(clim_tr_t25, climate_response_obligation__mitigation_priority, theater_ratio, 25, 0.22).
narrative_ontology:measurement(clim_tr_t30, climate_response_obligation__mitigation_priority, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_obligation__mitigation_priority, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(clim_be_t5, climate_response_obligation__mitigation_priority, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(clim_be_t10, climate_response_obligation__mitigation_priority, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(clim_be_t15, climate_response_obligation__mitigation_priority, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(clim_be_t20, climate_response_obligation__mitigation_priority, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(clim_be_t25, climate_response_obligation__mitigation_priority, base_extractiveness, 25, 0.7).
narrative_ontology:measurement(clim_be_t30, climate_response_obligation__mitigation_priority, base_extractiveness, 30, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_obligation__mitigation_priority, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(clim_su_t5, climate_response_obligation__mitigation_priority, suppression_requirement, 5, 0.32).
narrative_ontology:measurement(clim_su_t10, climate_response_obligation__mitigation_priority, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(clim_su_t15, climate_response_obligation__mitigation_priority, suppression_requirement, 15, 0.38).
narrative_ontology:measurement(clim_su_t20, climate_response_obligation__mitigation_priority, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(clim_su_t25, climate_response_obligation__mitigation_priority, suppression_requirement, 25, 0.42).
narrative_ontology:measurement(clim_su_t30, climate_response_obligation__mitigation_priority, suppression_requirement, 30, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__mitigation_priority, global_infrastructure).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, carbon_pricing_mechanisms).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, renewable_energy_subsidies).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, international_emissions_trading).

% DUAL FORMULATION NOTE:
% This constraint is part of the 'climate_response_obligation' family, which decomposes into 'mitigation_priority', 'adaptation_priority', and 'degrowth_reading'. Each represents a distinct structural approach to the climate crisis with different beneficiaries, victims, and underlying ethical premises.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
