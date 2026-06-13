% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__portfolio_pragmatism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__portfolio_pragmatism_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_mitigation_legitimacy__portfolio_pragmatism_reading
 *   human_readable: Portfolio Pragmatism Reading: Technology-Neutral Decarbonization Mix
 *   domain: energy_policy/climate_governance
 *
 * SUMMARY:
 *   The portfolio pragmatism reading frames optimal decarbonization as
 *   requiring both nuclear and renewable energy technologies, with regional
 *   variation in optimal mix rather than a single global pathway. This
 *   reading sits between the baseload-necessity reading (nuclear as the
 *   primary solution) and renewable-primacy (renewables plus storage
 *   sufficient) and degrowth readings (demand reduction primary). The
 *   claim/metric independence rule is in effect: the constraint is CLAIMED as
 *   tangled_rope (genuine coordination function + asymmetric extraction) and
 *   AUTHORED metrics describe moderate extraction with active enforcement to
 *   maintain the reading against the sibling readings. The theater-ratio
 *   trajectory shows a rise from 0.25 to peak 0.42 (time 20), then slight
 *   decline toward 0.40 at interval end—this pattern reflects the
 *   constraint's shift from technical legitimacy (early: optimization is
 *   novel and real) toward performative defense (middle: the reading becomes
 *   established doctrine needing protection) toward stabilization (late:
 *   opposition settles into distinct political camps, performance equilibrium
 *   reached).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.62).
domain_priors:suppression_score(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.58).
domain_priors:theater_ratio(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "Portfolio Pragmatism Reading: Technology-Neutral Decarbonization Mix").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "energy_policy/climate_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__portfolio_pragmatism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__portfolio_pragmatism_reading, '9ace9228-e9dc-41e4-a110-b188c5a2f9da').
narrative_ontology:cs_kernel_codification('9ace9228-e9dc-41e4-a110-b188c5a2f9da', distributed).
narrative_ontology:cs_authority_grounding('9ace9228-e9dc-41e4-a110-b188c5a2f9da', distributed).
narrative_ontology:cs_reading_relation('9ace9228-e9dc-41e4-a110-b188c5a2f9da', climate_mitigation_legitimacy__baseload_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('9ace9228-e9dc-41e4-a110-b188c5a2f9da', climate_mitigation_legitimacy__renewable_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('9ace9228-e9dc-41e4-a110-b188c5a2f9da', climate_mitigation_legitimacy__degrowth_sufficiency_reading, influences).
narrative_ontology:cs_axiom('9ace9228-e9dc-41e4-a110-b188c5a2f9da', foundational, technology_neutral_optimality).
narrative_ontology:cs_axiom_status(technology_neutral_optimality, holdable).
narrative_ontology:cs_axiom_grounding('9ace9228-e9dc-41e4-a110-b188c5a2f9da', technology_neutral_optimality, instrumental).
narrative_ontology:cs_axiom('9ace9228-e9dc-41e4-a110-b188c5a2f9da', foundational, regional_variation_necessity).
narrative_ontology:cs_axiom_status(regional_variation_necessity, holdable).
narrative_ontology:cs_axiom_grounding('9ace9228-e9dc-41e4-a110-b188c5a2f9da', regional_variation_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('9ace9228-e9dc-41e4-a110-b188c5a2f9da', pragmatic_technology_balancing).
narrative_ontology:cs_drift_state('9ace9228-e9dc-41e4-a110-b188c5a2f9da', contemporary_climate_science_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9ace9228-e9dc-41e4-a110-b188c5a2f9da', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_technology_advocates).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, integrated_energy_planners).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, capital_diversification_interests).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_energy_advocates).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, coal_phase_out_constituencies).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_urgency_coalitions).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__portfolio_pragmatism_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__portfolio_pragmatism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__portfolio_pragmatism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__portfolio_pragmatism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction of 0.62 reflects that the reading asymmetrically benefits nuclear investors and capital-diversification interests while extracting legitimacy from renewable advocates and climate-urgency constituencies who argue the reading delays optimal decarbonization. Suppression of 0.58 measures the active enforcement required to maintain the portfolio framing against both pure-renewable and pure-nuclear advocates who each claim their pathway is uniquely optimal. Theater ratio rising to peak 0.42 indicates that by year 20, substantial analytical effort becomes dedicated to demonstrating equivalence and balance between pathways—modeling, regulatory frameworks, and expert panels—some of which is genuine knowledge-work and some defensive framing. Accessibility collapse at 0.48 is moderate: alternatives (renewable-primary, baseload-primary, degrowth) remain technically live and politically mobilized, not foreclosed. Resistance at 0.72 is high: renewable advocates, climate urgency groups, and developing-nation planners actively contest the reading's legitimacy through alternative technical analyses and policy advocacy. The temporal pattern shows extractiveness climbing from 0.48 (early, when institutional adoption was incomplete) to stabilizing at 0.62 (late, once the reading became embedded in policy); this reflects consolidation, not Goodhart drift or corrective pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the nuclear-advocacy and planning-authority seats, the portfolio reading is genuine coordination: balancing diverse inputs, enabling regional optimization, reducing technological monopoly risk. From the renewable-advocate and climate-urgency seats, the same reading operates as an enforced extraction: capital diversion, timeline extension, legitimation of institutional inertia. The engine computes this divergence per-seat from the structural data; the authored claim does not predetermine the outcome.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary seats (nuclear advocates, capital diversifiers, regional-mix optimizers) have low directionality (d ≈ 0.25-0.35): they benefit from the reading without bearing its costs. The victim seats (renewable advocates, climate urgency, coal-exit workers) have high directionality (d ≈ 0.65-0.85): they bear delayed decarbonization and policy attention diversion while the reading persists. Energy planners and regulators (agenda-setters) sit near symmetric (d ≈ 0.50): they administer the constraint but also bear the burden of managing its contradictions—regulatory complexity, political risk if the reading fails, coordination overhead. Developing economies sit asymmetrically toward target (d ≈ 0.75): constrained capital and externally imposed technology choices push them toward high extraction experience. Capital allocators are partially exempt through arbitrage (d ≈ 0.45): they can rebalance regionally or withdraw, unlike jurisdictions locked into both-technology deployment mandates.
 *
 * MANDATROPHY ANALYSIS:
 *   The portfolio pragmatism reading contains latent mandatrophy: the founding problem (false binary choice between nuclear and renewable) has been substantially resolved by empirical evolution—most recent decarbonization scenarios and grid-integration studies from technical bodies now incorporate both, and the academic and policy conversation has moved beyond the binary. However, institutional actors and capital-allocation structures have crystallized around the portfolio reading itself, creating vested interests in maintaining the apparent equivalence and balance between pathways. If the founding problem (the binary) is dead but the reading persists, the reading itself has become the mandatroph object. The suppression requirement (maintaining the reading against pure-renewable and degrowth alternatives) remains high, suggesting the reading is defended not solely on technical grounds but through institutional enforcement. The measurement series shows suppression rising through year 15 (establishing the reading against contending technical views) and plateauing thereafter (the reading is now institutionalized), which is consistent with a mandatrophy trajectory: the founding problem's solution is embedded, but the reading persists and requires defense.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regional_optimal_mix_definition,
    'What technical and policy criteria determine the ''optimal'' regional mix of nuclear and renewables in any given jurisdiction? Is optimality based purely on carbon reduction per unit cost, or do other factors (infrastructure compatibility, political feasibility, worker transition, supply-chain resilience) alter the calculus?',
    'Comparative analysis of how different jurisdictions define and measure optimality: examine whether jurisdictions with different regional characteristics or political economies arrive at genuinely different optimal mixes, or whether the apparent optimization reflects unstated political preferences for nuclear or renewable dominance.',
    'If optimality is genuinely technical and context-sensitive, the portfolio reading''s coordination function is real. If different jurisdictions identify radically different ''optima'' for similar physical constraints, the reading is performing political alignment (extraction) under the cover of technical neutrality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_optimal_mix_definition, empirical, 'Whether portfolio optimization is technical or performing political preference.').

omega_variable(
    nuclear_lead_time_carbon_urgency_trade,
    'Does the carbon-reduction benefit of nuclear''s potential baseload contribution outweigh the carbon cost of its 10–15 year construction lead time in jurisdictions facing 2030–2040 decarbonization deadlines?',
    'Lifecycle carbon accounting comparing nuclear projects started now (with actual construction timelines) against accelerated renewable deployment with interim fossil backup, measured against jurisdiction-specific carbon budgets and deadline constraints.',
    'A net-negative finding (renewables reach net-zero faster despite nuclear''s baseload advantage) would support renewable-priority framing and undermine the portfolio reading''s implicit claim that timing is flexible. A net-positive finding would support the baseload necessity and portfolio pragmatism readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_lead_time_carbon_urgency_trade, empirical, 'The interaction between technology lead times and decarbonization urgency.').

omega_variable(
    capital_diversification_vs_concentration,
    'Does portfolio diversification across nuclear and renewable technologies reduce systemic risk in energy transitions, or does it diffuse limited decarbonization capital across two complex supply chains when concentration on one would accelerate deployment?',
    'Comparative financial modeling of diversified versus concentrated investment strategies, controlling for regional constraints (available capital, industrial capacity, transmission infrastructure); scenario analysis of failure modes (supply-chain disruption, technological setback, political reversal) in each strategy.',
    'If diversification reduces systemic risk and maintains deployment pace, the portfolio reading''s coordination value is confirmed. If concentration on renewables (or nuclear) achieves faster decarbonization without unacceptable concentration risk, the reading''s justification weakens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capital_diversification_vs_concentration, conceptual, 'Whether portfolio diversification is a genuine coordination benefit or a rationalization of capital-preservation preferences.').

omega_variable(
    sibling_reading_foreclosure_dynamics,
    'To what extent does institutional adoption of the portfolio pragmatism reading actively foreclose or suppress the policy viability of the pure-renewable and pure-baseload sibling readings?',
    'Comparative policy-change analysis across jurisdictions: jurisdictions that adopt portfolio pragmatism explicitly in energy plans vs. jurisdictions that don''t; measure regulatory approvals for single-pathway investments (pure nuclear, pure renewable buildout) before and after portfolio adoption; track research funding and technology-development investment shifts.',
    'If portfolio adoption actively suppresses pure-pathway investments and research, the reading is itself extractive and enforced, not merely one valid technical view. If pure pathways remain viable policy options in portfolio-adopting jurisdictions, the reading''s enforcement overhead is lower.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_dynamics, empirical, 'Whether the portfolio reading suppresses alternative decarbonization framings or coexists with them.').

omega_variable(
    nuclear_cost_trajectory_uncertainty,
    'Will nuclear construction costs continue to rise (as recent U.S. and UK projects show) or do small modular reactors (SMRs) and manufacturing scale offer credible paths to cost reduction? The portfolio reading''s cost-benefit logic depends on this trajectory.',
    'Longitudinal cost tracking of recent and in-progress nuclear projects; SMR prototype deployment and cost data; manufacturing learning-curve analysis if large-scale SMR production occurs; comparison of projected vs. actual costs in nuclear buildout scenarios.',
    'If nuclear costs fall substantially, the portfolio reading gains empirical support (both pathways become cost-competitive). If costs rise further, the reading''s balance claim weakens and renewable-priority arguments strengthen. This uncertainty is structural to the reading''s stability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nuclear_cost_trajectory_uncertainty, empirical, 'The trajectory of nuclear capital costs and the reading''s cost-benefit symmetry.').

omega_variable(
    identity_lock_in_climate_urgency_seat,
    'For climate-urgency coalitions coded as identity_locked exit, what is the mechanism binding their identity to rapid-decarbonization demand? Is it epistemic (the scientific understanding of carbon budgets), ideological (moral imperative of intergenerational justice), or organizational (career and funding tied to urgency narratives)?',
    'Ethnographic and discourse analysis of climate-movement narratives; tracking whether climate-urgency framing persists if empirical carbon-budget estimates shift or if transition timelines change for exogenous reasons; examining whether actors exit the urgency coalition if their particular technology preference (renewables) is deprioritized.',
    'If identity-lock is primarily epistemic, it is reversible by evidence and the suppression dynamic is lower. If primarily ideological or organizational, suppression is higher because the constraint must actively defend against constituencies whose self-concept depends on urgency. The identity-lock mechanism matters for understanding whether the reading''s extraction is structural or performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_climate_urgency_seat, conceptual, 'The nature and reversibility of identity-lock in climate urgency constituencies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(clim_tr_t25, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(clim_tr_t30, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(clim_tr_t40, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(clim_be_t25, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(clim_be_t30, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 30, 0.61).
narrative_ontology:measurement(clim_be_t40, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 10, 0.53).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 15, 0.56).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 20, 0.59).
narrative_ontology:measurement(clim_su_t25, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 25, 0.59).
narrative_ontology:measurement(clim_su_t30, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(clim_su_t40, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.18).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy__renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy__degrowth_sufficiency_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_supply_chain_stability).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_grid_integration_requirement).

% DUAL FORMULATION NOTE:
% This constraint is part of the climate_mitigation_legitimacy kernel family. The portfolio_pragmatism_reading (this file) frames decarbonization as requiring both nuclear and renewable technologies balanced by regional analysis. The baseload_necessity_reading weights toward nuclear as foundational; the renewable_primacy_reading argues renewables-plus-storage suffice; the degrowth_sufficiency_reading reframes the problem as demand-constrained rather than technology-choice constrained. Each reading instantiates a separate constraint with distinct ε, beneficiary/victim structure, and measured type. They are linked via network.affects_constraints because the adoption of one reading by policy institutions directly shapes the viability, capital availability, and institutional support for the others. The portfolio reading INFLUENCES the sibling readings by reframing the question from 'which technology' to 'what mix,' which shifts political and technical debate toward negotiation of proportions rather than selection.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_legitimacy__portfolio_pragmatism_reading, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
