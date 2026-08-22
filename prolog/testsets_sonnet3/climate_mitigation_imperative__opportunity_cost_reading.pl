% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__opportunity_cost_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_imperative__opportunity_cost_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: climate_mitigation_imperative__opportunity_cost_reading
 *   human_readable: Carbon-per-Dollar-per-Year Deployment Imperative (Opportunity-Cost Reading)
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested
 *   climate_mitigation_imperative kernel: the opportunity-cost reading, which
 *   holds that mitigation policy should be evaluated by carbon abated per
 *   dollar per year, and that nuclear power's capital intensity and
 *   multi-decade construction timelines make it a net-harmful allocation of
 *   scarce climate finance relative to faster-deploying renewables and
 *   storage. Under this reading, nuclear developers, their supply chains,
 *   host communities, and dependent ratepayers enter the victim set because
 *   capital that could have completed reactors is redirected toward solar,
 *   wind, and storage on the basis of deployment speed alone. This is a
 *   distinct constraint from the portfolio_optimization_reading (which holds
 *   nuclear necessary for reliable baseload and would place nuclear among the
 *   beneficiaries of a technology-neutral allocation rule) and the
 *   systems_transition_reading (which opposes nuclear on
 *   decentralization/democratic-control grounds rather than deployment
 *   speed). The three readings share a kernel — the underlying imperative
 *   that mitigation policy must respond to a finite, depleting carbon budget
 *   — but diverge sharply on beneficiary/victim structure and on what metric
 *   operationalizes 'mitigation requires.' Per the ε-invariance principle,
 *   each reading is authored here as its own constraint with its own stable
 *   ε; the sibling readings are separate files linked via network relations,
 *   not alternative measurements of this one.
 *
 * KEY AGENTS:
 *   - utility_scale_solar_developers: primary beneficiary (organized/mobile) — captures redirected capital and permitting priority
 *   - nuclear_developers: primary target (powerful/constrained) — bears capital reallocation despite institutional power, because the metric is timeline-blind to their scale
 *   - climate_finance_institutions: agenda_setter (institutional/analytical) — administers and could revise the carbon-per-dollar-per-year screen
 *   - ratepayers_in_nuclear_dependent_grids: diffuse target (powerless/trapped) — bears reliability and cost consequences with no voice in the allocation criteria
 *   - atmospheric_carbon_budget: analytical observer (non-agent) — the physical fact the entire reading's justification rests on
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__opportunity_cost_reading, 0.62).
domain_priors:suppression_score(climate_mitigation_imperative__opportunity_cost_reading, 0.38).
domain_priors:theater_ratio(climate_mitigation_imperative__opportunity_cost_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__opportunity_cost_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__opportunity_cost_reading, "Carbon-per-Dollar-per-Year Deployment Imperative (Opportunity-Cost Reading)").
narrative_ontology:topic_domain(climate_mitigation_imperative__opportunity_cost_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__opportunity_cost_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__opportunity_cost_reading, '69a14d86-b3af-44fc-a645-ef80e65ba03d').
narrative_ontology:cs_kernel_codification('69a14d86-b3af-44fc-a645-ef80e65ba03d', distributed).
narrative_ontology:cs_authority_grounding('69a14d86-b3af-44fc-a645-ef80e65ba03d', distributed).
narrative_ontology:cs_reading_relation('69a14d86-b3af-44fc-a645-ef80e65ba03d', climate_mitigation_imperative__portfolio_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('69a14d86-b3af-44fc-a645-ef80e65ba03d', climate_mitigation_imperative__systems_transition_reading, influences).
narrative_ontology:cs_axiom('69a14d86-b3af-44fc-a645-ef80e65ba03d', foundational, deployment_speed_dominates_lifecycle_output).
narrative_ontology:cs_axiom_status(deployment_speed_dominates_lifecycle_output, holdable).
narrative_ontology:cs_axiom_grounding('69a14d86-b3af-44fc-a645-ef80e65ba03d', deployment_speed_dominates_lifecycle_output, empirically_contingent).
narrative_ontology:cs_axiom('69a14d86-b3af-44fc-a645-ef80e65ba03d', secondary, near_term_abatement_incommensurably_outweighs_long_horizon_firm_capacity).
narrative_ontology:cs_axiom_status(near_term_abatement_incommensurably_outweighs_long_horizon_firm_capacity, holdable).
narrative_ontology:cs_axiom_grounding('69a14d86-b3af-44fc-a645-ef80e65ba03d', near_term_abatement_incommensurably_outweighs_long_horizon_firm_capacity, instrumental).
narrative_ontology:cs_reference_frame('69a14d86-b3af-44fc-a645-ef80e65ba03d', technology_neutral_decarbonization_consensus).
narrative_ontology:cs_drift_state('69a14d86-b3af-44fc-a645-ef80e65ba03d', post_2015_green_finance_taxonomy_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('69a14d86-b3af-44fc-a645-ef80e65ba03d', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, utility_scale_solar_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, wind_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, battery_storage_manufacturers).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, grid_service_aggregators).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, nuclear_developers).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, nuclear_supply_chain_workers).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, host_communities_of_stalled_reactor_projects).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, ratepayers_in_nuclear_dependent_grids).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Compete for capital allocation, permitting priority, and public subsidy against nuclear projects. The opportunity-cost framing directs finance ministries, green banks, and climate funds toward their shorter construction timelines, which increases deal flow and lowers their cost of capital relative to nuclear bids.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, utility_scale_solar_developers, beneficiary,
    organized, biographical, mobile, national).

% Benefit from the same capital-reallocation logic as solar; the imperative's carbon-per-dollar-per-year metric favors their multi-month to two-year build timelines over nuclear's decade-plus horizon, drawing procurement auctions and portfolio standards in their direction.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, wind_developers, beneficiary,
    organized, biographical, mobile, national).

% Positioned as the flexibility complement to intermittent renewables; the imperative's framing treats storage buildout as substitutable for baseload, channeling capital that might otherwise fund nuclear firming capacity toward battery manufacturing capacity instead.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, battery_storage_manufacturers, beneficiary,
    organized, biographical, mobile, global).

% Sell demand-response and virtual-power-plant services predicated on a grid built around fast-deploying variable generation. Their business model gains legitimacy and contracts from the same metric that disfavors nuclear baseload.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, grid_service_aggregators, beneficiary,
    moderate, biographical, mobile, regional).

% Sunk into decade-long construction and licensing cycles; when climate funds, green bonds, and carbon-reduction mandates apply a carbon-per-dollar-per-year screen, their projects are structurally disadvantaged even where completed reactors would deliver deep, firm decarbonization. Capital that would have completed a reactor is redirected mid-project or withheld at the financing stage; exit means absorbing stranded-asset losses or lobbying for exemption from the metric.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, nuclear_developers, payer,
    powerful, generational, constrained, national).

% Employed in forging, fuel fabrication, and specialized construction trades tied to reactor build cycles. When projects are cancelled or deprioritized under the opportunity-cost screen, their skills are not readily transferable to renewables construction on the same timeline, and regional job losses concentrate in single-industry towns.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, nuclear_supply_chain_workers, payer,
    moderate, biographical, constrained, regional).

% Rezoned, taxed, and organized around an anticipated multi-decade reactor employment base and tax revenue; when financing is redirected mid-build under the deployment-speed metric, the community is left with a half-built asset, foregone tax base, and no comparable renewables buildout sited locally to replace it.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, host_communities_of_stalled_reactor_projects, payer,
    powerless, biographical, trapped, local).

% Depend on existing or near-completion nuclear capacity for firm, low-carbon power; when the opportunity-cost framing steers new investment away from completing or replacing that capacity, they face either fossil-fired reliability backstops (raising both cost and emissions) or unreliable supply, with no direct say in the capital-allocation decision.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, ratepayers_in_nuclear_dependent_grids, payer,
    powerless, biographical, trapped, regional).

% Multilateral climate funds, green bond frameworks, and national mitigation agencies that adopt the carbon-per-dollar-per-year screen as their allocation rule, administer eligibility criteria, and can revise or waive the screen for specific technologies. They set and enforce the metric that channels capital away from nuclear and toward faster-deploying alternatives.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, climate_finance_institutions, agenda_setter,
    institutional, generational, analytical, global).

% The physical constraint the imperative claims to serve — a finite remaining carbon budget that makes near-term abatement rate, not eventual installed capacity, the operative variable. Not an actor; represented for completeness because the metric's entire justification rests on this budget's finiteness.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, atmospheric_carbon_budget, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(climate_mitigation_imperative__opportunity_cost_reading, atmospheric_carbon_budget).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_imperative__opportunity_cost_reading, diffuse).
narrative_ontology:fixing_cost_class(climate_mitigation_imperative__opportunity_cost_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates scarce climate finance and policy attention toward whichever low-carbon technologies deliver the most avoided emissions per dollar per year, under the premise that the remaining carbon budget is depleting faster than slow-build technologies can address it.
% TRANSFER_FUNCTION: Moves capital, subsidy eligibility, and regulatory priority away from nuclear developers and their supply chains and communities, toward solar, wind, and storage developers, on the basis of deployment speed rather than total lifecycle output or grid role.
% ABSENT_VOICES: Grid reliability engineers and utility system planners who model firm-capacity adequacy over decadal horizons are largely absent from the finance-allocation decision; their concerns about intermittency-driven fossil backstop reliance surface mainly in post-hoc reliability reports, not in the capital-screening criteria itself. Nuclear-dependent grid operators are also structurally underrepresented in multilateral climate finance governance relative to their exposure.
% DISAPPEARANCE_RATIONALE: If the opportunity-cost screen vanished overnight, climate finance institutions would revert to technology-neutral or portfolio-based criteria; nuclear projects currently priced out of green finance would become financeable again, and some renewables deals currently winning on speed alone would face more competition. Renewables developers and storage manufacturers dispute that this would meaningfully slow the transition, arguing deployment speed is decisive regardless of financing criteria; nuclear developers and reliability-focused utilities argue the reallocation is the whole mechanism by which their projects are starved of capital.
% FOUNDING_PROBLEM: The remaining carbon budget before locking in dangerous warming is small and shrinking; abatement that arrives in 2040 counts for much less cumulative-emissions purposes than the same abatement arriving in 2028, so capital allocation criteria that ignore deployment speed systematically underweight near-term climate risk.
% FOUNDING_PROBLEM_CORROBORATION: IPCC carbon budget assessments and independent climate scientists outside both the renewables and nuclear industries corroborate that near-term abatement rate matters more than eventual installed capacity for peak warming outcomes — this is the strongest external corroboration in the kernel. However, independent grid-reliability engineers and system-adequacy researchers (also outside both industries) corroborate the sibling portfolio_optimization_reading's concern that speed-only screening can degrade firm capacity margins, producing fossil-backstop reliance that partially offsets the near-term gains this reading is built to capture.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__opportunity_cost_reading, contested).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__opportunity_cost_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__opportunity_cost_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_imperative__opportunity_cost_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__opportunity_cost_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_imperative__opportunity_cost_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_imperative__opportunity_cost_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_imperative__opportunity_cost_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.62 by interval end because the constraint's operation systematically redirects capital away from a class of actors (nuclear developers, their workers, their host communities, their dependent ratepayers) whose economic and reliability interests are structurally disfavored by a metric that treats construction timeline as dispositive rather than as one input among several. This is real extraction under the opportunity-cost reading's own terms: it is not a neutral technical screen but an allocation rule that has losers as well as winners. Suppression is moderate (0.38) and rising — the constraint's persistence increasingly depends on active enforcement (green taxonomy exclusions, subsidy eligibility rules, credit-rating methodologies) rather than on unanimous technical consensus, because the sibling portfolio_optimization_reading contests the same carbon budget facts and reaches a different allocation conclusion. Theater ratio is modest (0.28) — the coordination function (allocating scarce capital toward measurable near-term abatement) is largely genuine, not cover, but a growing share of the metric's institutional defense addresses reputational and political pressure from the nuclear-favoring coalition rather than the underlying carbon-budget arithmetic itself. Accessibility collapse is moderate (0.4): once a climate finance institution adopts the screen, alternatives (technology-neutral portfolio standards) are not eliminated but are actively displaced within that institution's own criteria. Resistance is high (0.72) precisely because this reading is contested at the kernel level by two credible sibling readings with different beneficiary/victim structures — nuclear developers, reliability engineers, and systems-transition advocates all resist this reading's allocation conclusion, though for different and sometimes opposed reasons.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewables and storage developers are declared beneficiaries because the carbon-per-dollar-per-year metric, once adopted by climate finance institutions, directly increases their access to capital, subsidy, and procurement priority relative to a technology-neutral baseline — d sits near the beneficiary end for these organized, mobile actors. Nuclear developers are declared victims: despite institutional power, their exit options are constrained by multi-decade sunk capital and licensing regimes that renewables developers do not carry, so the constraint's directionality toward them is high despite their nominal power level — this is a case where power and directionality diverge, and the derivation should not collapse them. Powerless, trapped actors (host communities, dependent ratepayers) receive the highest derived d because they have no meaningful exit from the consequences of capital reallocation decided at the institutional level far above them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a shrinking carbon budget where near-term abatement rate matters more than eventual installed capacity — remains live and is corroborated by IPCC assessments outside both the renewables and nuclear industries, which prevents this reading from being dismissed as pure rent-seeking cover for renewables developers. But the founding_problem_corroboration also surfaces a genuine tension the reading does not resolve: reliability engineers outside both industries corroborate the sibling portfolio_optimization_reading's concern about firm-capacity adequacy. This is why the constraint is authored tangled_rope rather than rope: it has a genuine, well-corroborated coordination function (accelerate near-term abatement under budget constraints) AND identifiable structural victims who pay through the same mechanism (nuclear developers and their dependents) via active enforcement (green taxonomy exclusions, screening criteria). It is not pure extraction dressed as coordination — the coordination claim survives external scrutiny — but it is not pure coordination either, because the metric's timeline-only framing produces real, asymmetric costs that a portfolio-based allocation would not impose on the same parties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_choice_ambiguity,
    'Is the carbon-per-dollar-per-year metric the correct operationalization of the climate_mitigation_imperative kernel, or does it smuggle in a contestable normative choice (weighting near-term abatement over long-term firm capacity) under the appearance of a purely technical screen?',
    'Compare integrated warming outcomes across full-portfolio system models that vary the weighting between near-term deployment speed and long-term reliability/firm-capacity adequacy, under multiple carbon budget scenarios; if results are highly sensitive to the weighting choice, the metric embeds a value choice rather than a neutral technical fact.',
    'If the metric is shown to be normatively loaded rather than purely technical, this reading''s claim to be simply ''following the physics'' weakens, and its extraction of capital from nuclear developers looks more like a policy choice with distributional losers than an inevitable consequence of the carbon budget.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_choice_ambiguity, conceptual, 'Whether the deployment-speed metric is a neutral operationalization of the shared kernel or a contestable normative framing that determines the victim set.').

omega_variable(
    nuclear_capital_intensity_causal_claim,
    'Is nuclear''s capital intensity and construction timeline an inherent technological feature, or substantially a product of regulatory and financing environments that this reading''s own dominance helps entrench (a self-fulfilling opportunity-cost loop)?',
    'Compare construction timelines and cost trajectories in jurisdictions with standardized reactor designs, streamlined licensing, and stable financing commitments (e.g., South Korea, historical French buildout) against jurisdictions where nuclear finance is intermittently available and licensing is case-by-case; if timelines and costs are substantially environment-dependent rather than fixed, the ''net-harmful'' conclusion is partly endogenous to the financing pattern this reading produces.',
    'If capital intensity and timeline are substantially policy-dependent rather than fixed, the opportunity-cost reading''s beneficiary/victim assignment may be partly self-reinforcing: starving nuclear of capital lengthens timelines and raises costs, which then validates the same screen that caused the starvation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nuclear_capital_intensity_causal_claim, empirical, 'Whether nuclear''s disadvantage under this metric is an inherent technology fact or a partly self-fulfilling consequence of the financing pattern the metric itself produces.').

omega_variable(
    kernel_framing_underdetermination,
    'Could this constraint alternatively be framed around ''grid decarbonization adequacy'' (a systems-level framing) rather than ''capital allocation efficiency'' (the framing adopted here), and would that alternative framing shift the classification toward rope (pure coordination, no clear victim) rather than tangled_rope?',
    'Author a systems-adequacy-framed sibling story explicitly and compare computed classifications; the choice between framings is currently guided by the kernel context''s explicit assignment of nuclear to the victim set for this reading, per the SCOPE manifest''s structural delta.',
    'If the systems-adequacy framing were adopted instead, nuclear might appear as a contested-but-included technology rather than a structural victim, likely shifting this reading closer to rope; the capital-allocation framing adopted here is what generates the tangled_rope classification by making the victim set explicit and enforcement-dependent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Alternative framing (systems adequacy vs. capital allocation efficiency) would change which stakeholders count as victims and could shift the computed classification; this story adopts the capital-allocation framing per the assigned structural delta.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__opportunity_cost_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(clim_tr_t4, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(clim_tr_t8, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement(clim_tr_t12, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(clim_tr_t16, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(clim_tr_t24, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(clim_be_t4, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 4, 0.41).
narrative_ontology:measurement(clim_be_t8, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(clim_be_t12, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(clim_be_t16, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(clim_be_t24, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(clim_su_t4, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 4, 0.26).
narrative_ontology:measurement(clim_su_t8, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 8, 0.29).
narrative_ontology:measurement(clim_su_t12, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 12, 0.32).
narrative_ontology:measurement(clim_su_t16, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 16, 0.35).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 20, 0.37).
narrative_ontology:measurement(clim_su_t24, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 24, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__opportunity_cost_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_imperative__opportunity_cost_reading, 0.12).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative__portfolio_optimization_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative__systems_transition_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language 'climate mitigation imperative' kernel per the ε-invariance principle. Each reading operationalizes 'mitigation requires X' differently and produces a different beneficiary/victim structure: opportunity_cost_reading (this story) puts nuclear in the victim set and renewables in the beneficiary set, claimed tangled_rope; portfolio_optimization_reading would put nuclear in the beneficiary set as necessary baseload, likely claimed rope or tangled_rope depending on how renewables curtailment costs are treated; systems_transition_reading puts nuclear in the victim set on centralization grounds independent of deployment speed, likely claimed snare or tangled_rope depending on how centralized renewables ownership is treated. All three share the same underlying kernel (finite carbon budget) but differ in ε, in claimed_type, and in stakeholder structure — they are linked here rather than merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
