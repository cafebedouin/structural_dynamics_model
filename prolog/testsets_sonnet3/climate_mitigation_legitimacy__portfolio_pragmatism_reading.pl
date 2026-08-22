% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__portfolio_pragmatism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: climate_mitigation_legitimacy__portfolio_pragmatism_reading
 *   human_readable: Technology-Neutral Portfolio Standard for Decarbonization Planning
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint is the portfolio-pragmatism reading of the contested
 *   climate mitigation legitimacy kernel: the claim that optimal
 *   decarbonization requires a technology-neutral portfolio combining nuclear
 *   and renewables, calibrated to regional conditions rather than a
 *   nationally uniform technology preference. It is authored as ONE of four
 *   sibling readings of the same underlying kernel — baseload_necessity,
 *   degrowth_sufficiency, and renewable_primacy are separate constraints, not
 *   alternative framings of this one. This story evaluates the standing
 *   arrangement under contest — actual integrated resource planning regimes
 *   that mandate technology diversification — as this reading's own lights
 *   see it: the coordination function (hedging against single-technology
 *   cost/schedule risk) is real, but the mandate has drifted toward
 *   guaranteeing incumbents (diversified utilities, nuclear vendors) a fixed
 *   share of certified resource plans regardless of updated regional cost
 *   data, which is where the extraction lives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.42).
domain_priors:suppression_score(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.38).
domain_priors:theater_ratio(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "Technology-Neutral Portfolio Standard for Decarbonization Planning").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__portfolio_pragmatism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__portfolio_pragmatism_reading, '42ee37aa-a648-4b70-ba3a-1992b0b694b3').
narrative_ontology:cs_kernel_codification('42ee37aa-a648-4b70-ba3a-1992b0b694b3', distributed).
narrative_ontology:cs_authority_grounding('42ee37aa-a648-4b70-ba3a-1992b0b694b3', distributed).
narrative_ontology:cs_reading_relation('42ee37aa-a648-4b70-ba3a-1992b0b694b3', climate_mitigation_legitimacy__baseload_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('42ee37aa-a648-4b70-ba3a-1992b0b694b3', climate_mitigation_legitimacy__renewable_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('42ee37aa-a648-4b70-ba3a-1992b0b694b3', climate_mitigation_legitimacy__degrowth_sufficiency_reading, influences).
narrative_ontology:cs_axiom('42ee37aa-a648-4b70-ba3a-1992b0b694b3', foundational, no_technology_privileged_a_priori).
narrative_ontology:cs_axiom_status(no_technology_privileged_a_priori, holdable).
narrative_ontology:cs_axiom_grounding('42ee37aa-a648-4b70-ba3a-1992b0b694b3', no_technology_privileged_a_priori, empirically_contingent).
narrative_ontology:cs_axiom('42ee37aa-a648-4b70-ba3a-1992b0b694b3', foundational, optimal_mix_is_regionally_determined).
narrative_ontology:cs_axiom_status(optimal_mix_is_regionally_determined, holdable).
narrative_ontology:cs_axiom_grounding('42ee37aa-a648-4b70-ba3a-1992b0b694b3', optimal_mix_is_regionally_determined, empirically_contingent).
narrative_ontology:cs_reference_frame('42ee37aa-a648-4b70-ba3a-1992b0b694b3', integrated_resource_planning_diversification_standard).
narrative_ontology:cs_drift_state('42ee37aa-a648-4b70-ba3a-1992b0b694b3', post_2020_renewable_cost_collapse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('42ee37aa-a648-4b70-ba3a-1992b0b694b3', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, diversified_utility_holding_companies).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_vendor_consortiums).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, grid_reliability_planners).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, single_technology_renewable_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, ratepayers_in_high_cost_nuclear_regions).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, municipal_utilities_with_thin_capital_access).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets integrated resource planning rules that mandate a diversified generation portfolio, citing reliability modeling that shows single-technology buildouts create curtailment or capacity-adequacy risk. Administers the certification and procurement rules that decide which projects count toward compliance.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, grid_reliability_planners, agenda_setter,
    institutional, generational, analytical, national).

% Owns both nuclear and renewable generation assets and rate-basing rights across multiple technologies. A technology-neutral mandate lets it fold cost overruns from either technology into the same regulated portfolio and pass them through to ratepayers without exposure to a single-technology cost shock.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, diversified_utility_holding_companies, beneficiary,
    powerful, generational, arbitrage, national).

% Sells reactor components and long-lifecycle service contracts that require a policy guarantee of nuclear's continued inclusion in the eligible resource mix. Portfolio-neutrality clauses give nuclear projects the regulatory standing needed to secure multi-decade financing that a renewables-only standard would deny them.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_vendor_consortiums, beneficiary,
    organized, civilizational, constrained, global).

% Builds wind and solar projects that can be procured and interconnected faster and at lower marginal cost than nuclear, but the portfolio mandate caps how much of the eligible resource mix any one technology may fill, regardless of comparative cost. Bears the opportunity cost of capacity it could otherwise deploy immediately.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, single_technology_renewable_developers, payer,
    moderate, biographical, constrained, regional).

% Pays regulated rates that include cost recovery for nuclear projects with a documented history of overruns and delay. Has no ability to opt out of the portfolio the utility and regulator have jointly certified as least-cost, even where a renewables-heavier mix would have been cheaper for their specific region.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, ratepayers_in_high_cost_nuclear_regions, payer,
    powerless, biographical, trapped, regional).

% Must meet the same technology-neutral procurement targets as large investor-owned utilities but lacks the balance sheet to participate in nuclear financing consortia, so it either buys expensive credits or falls out of compliance and faces penalties.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, municipal_utilities_with_thin_capital_access, payer,
    powerless, biographical, constrained, local).

% Produces integrated assessment and capacity-expansion models comparing all-renewables, all-nuclear, and mixed pathways. Testifies in regulatory proceedings and publishes findings that different regions have genuinely different optimal mixes depending on land, grid topology, and existing baseload assets.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_scientists_and_systems_modelers, observer,
    analytical, civilizational, analytical, global).

% Argues the entire supply-expansion framing — whether nuclear, renewable, or mixed — is the wrong question, and that demand reduction should be the primary lever. Rarely seated in resource-planning proceedings, which are structured around procurement of new generation rather than reduction of load.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, degrowth_and_demand_reduction_advocates, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__portfolio_pragmatism_reading, diversified_utility_holding_companies).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__portfolio_pragmatism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates long-horizon capital allocation across a genuinely uncertain technology landscape by requiring planners to hedge against the risk that any single technology underperforms cost or deployment projections, spreading portfolio risk the way a prudent investor diversifies a portfolio.
% TRANSFER_FUNCTION: Moves capital-recovery guarantees and regulatory certification eligibility toward incumbent diversified utilities and nuclear vendors with the balance sheets to participate in both technology tracks, and moves cost exposure and compliance burden toward single-technology developers, thinly capitalized municipal utilities, and ratepayers in regions where the mandated mix is not actually least-cost.
% ABSENT_VOICES: Demand-reduction and degrowth advocates are structurally absent from integrated resource planning proceedings, which are convened to compare supply technologies against each other, not to weigh supply expansion against reduced demand as an alternative.
% DISAPPEARANCE_RATIONALE: Utilities and nuclear vendors would say the loss of a technology-neutral mandate exposes the grid to single-technology risk and removes nuclear's financing pathway entirely, rearranging capital markets; single-technology renewable developers and cost-burdened ratepayers would say the region-specific least-cost mix would simply emerge from ordinary competitive procurement, and the mandate mainly protects incumbents' access to a guaranteed slice of the resource plan.
% FOUNDING_PROBLEM: Early-2000s and 2010s resource planning saw both renewable-only and nuclear-only advocacy captured decision processes in ways that led to stranded assets and reliability shortfalls when the favored technology underperformed cost or schedule projections in a specific region; the portfolio-neutral standard was built to prevent single-technology capture of the planning process.
% FOUNDING_PROBLEM_CORROBORATION: Independent systems modelers outside the utility and vendor coalitions corroborate that region-specific optimal mixes are real and that neither pure-renewables nor pure-nuclear pathways dominate in all geographies — supporting the founding problem as still partly live. However, state consumer advocates and municipal utility associations, also outside the beneficiary set, attest that in practice the neutral mandate has increasingly functioned to guarantee nuclear a fixed share of the certified mix regardless of updated regional cost data, suggesting the founding problem has been partially superseded by an entrenchment function.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__portfolio_pragmatism_reading, contested).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__portfolio_pragmatism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__portfolio_pragmatism_reading_tests).
:- end_tests(climate_mitigation_legitimacy__portfolio_pragmatism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42 at interval end) is moderate, not high: the coordination function is genuine and well-corroborated by independent systems modeling, but a rising share of the mandate's operation has shifted from genuine risk-hedging toward protecting incumbent capital positions as renewable costs have fallen faster than nuclear costs over the tracked interval. Suppression (0.38) reflects that alternatives are not blocked outright — single-technology developers can still build, just not fill the entire eligible mix — but the compliance and certification apparatus increasingly constrains what counts as an eligible resource plan. Theater ratio (0.30) captures that some 'technology neutrality' language has become a justification layer for what is functionally a nuclear-inclusion guarantee, without theater dominating the picture.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and beneficiary seats, this looks like prudent, technology-neutral risk management responsive to genuine regional uncertainty. From the payer seats — thinly capitalized municipal utilities, trapped ratepayers, capital-constrained renewable developers — the same rule structure looks like an enforced guarantee that a particular incumbent capital structure gets a share of every resource plan, resourced through their compliance costs and rates. The engine should compute these as different seat-level types from the same structural data; the claimed type (tangled_rope) is authored to reflect that both the coordination and the extraction functions are real and coexist through the same enforcement mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Diversified utility holding companies and nuclear vendor consortiums are the structural beneficiaries: the mandate gives both types of assets guaranteed standing in the certified resource mix, letting utilities rate-base either technology's cost overruns and letting vendors secure financing that a renewables-only standard would deny. Single-technology renewable developers and thinly capitalized municipal utilities are targets: they bear opportunity cost or compliance cost from a cap on their technology's share that is not always justified by regional cost data. Ratepayers in high-cost nuclear regions are trapped payers with no exit from the jointly-certified portfolio. Grid reliability planners sit as agenda-setters whose institutional interest is in defensible, litigation-resistant planning processes, which favors formal technology neutrality regardless of whether it is regionally optimal.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing single-technology capture of resource planning after real stranded-asset episodes — is corroborated as having been genuinely live at the mandate's founding, by sources outside the beneficiary set (independent systems modelers). But those same outside sources, plus consumer advocates, now report the mandate increasingly functions to lock in nuclear's share independent of updated regional cost curves. This is the mandatrophy signature: a coordination function that solved a real problem has partially calcified into a capital-allocation guarantee for the actors positioned to hold both technologies, while the field's own science (regional variation in optimal mix) argues for continuous reassessment rather than a fixed portfolio floor.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    neutrality_vs_incumbency_capture,
    'Is the technology-neutral portfolio mandate a genuine hedge against single-technology risk, or has it become a capture mechanism that guarantees incumbent diversified utilities and nuclear vendors a fixed share of certified resource plans regardless of updated regional cost data?',
    'Compare certified resource plan technology shares against contemporaneous least-cost modeling in the same region over time; a persistent gap between the mandated share and the modeled least-cost share, especially one that grows as renewable costs fall, would indicate capture rather than genuine hedging.',
    'If capture-dominant, the constraint''s coordination story is functioning mainly as cover for incumbent capital protection, pushing the classification toward snare; if hedging-dominant and the gap tracks genuine uncertainty rather than incumbent protection, the constraint sits closer to a legitimate tangled_rope or even rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutrality_vs_incumbency_capture, empirical, 'Whether the neutral-portfolio mandate primarily hedges genuine regional uncertainty or primarily protects incumbent capital positions.').

omega_variable(
    regional_heterogeneity_is_real_or_pretextual,
    'Is regional variation in optimal generation mix (this reading''s central structural claim) a robust empirical finding across independent modeling groups, or is it partly an artifact of modeling assumptions favorable to whichever technology a given modeling group''s funders prefer?',
    'Cross-comparison of capacity-expansion model results across modeling groups with disclosed funding sources and methodology, checking whether regional-mix conclusions are sensitive to funder-linked assumption choices (e.g., discount rates, land-use constraints, storage cost trajectories).',
    'If regional heterogeneity is robust and funder-independent, this reading''s core empirical premise is well-grounded; if heterogeneity conclusions are sensitive to funder-linked assumptions, the portfolio-pragmatism reading''s claim to be the ''neutral, evidence-driven'' reading is itself contestable and it may function as a soft form of incumbency protection dressed in empirical language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_heterogeneity_is_real_or_pretextual, conceptual, 'Whether the reading''s foundational empirical claim (regional variation in optimal mix) is robust or funder-sensitive.').

omega_variable(
    kernel_framing_alternative_reading_boundary,
    'Could the portfolio-pragmatism reading and the baseload-necessity reading be read as the same underlying commitment (dispatchable-capacity adequacy) merely expressed with different technology emphasis, rather than as genuinely distinct readings?',
    'Examine whether a jurisdiction''s planning documents distinguish ''technology-neutral hedging against uncertainty'' from ''baseload necessity'' as separate justificatory grounds, or conflate them in practice — if courts/regulators treat them as interchangeable justifications for the same procurement outcome, the readings may be less structurally distinct than declared.',
    'If the two readings collapse into one in practice, this story''s ε and stakeholder structure should be re-examined for overlap with baseload_necessity_reading, and the network edge between them may need to be reclassified from coexists_with toward a tighter coupling.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_alternative_reading_boundary, conceptual, 'Whether portfolio-pragmatism and baseload-necessity are genuinely distinct readings or a single commitment under two labels.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(clim_tr_t4, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(clim_tr_t8, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement(clim_tr_t12, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(clim_tr_t16, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(clim_tr_t24, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 24, 0.3).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(clim_be_t4, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 4, 0.26).
narrative_ontology:measurement(clim_be_t8, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 8, 0.31).
narrative_ontology:measurement(clim_be_t12, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 12, 0.35).
narrative_ontology:measurement(clim_be_t16, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 16, 0.38).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(clim_be_t24, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(clim_su_t4, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 4, 0.24).
narrative_ontology:measurement(clim_su_t8, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 8, 0.28).
narrative_ontology:measurement(clim_su_t12, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 12, 0.31).
narrative_ontology:measurement(clim_su_t16, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 16, 0.34).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement(clim_su_t24, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 24, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.15).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy__renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the climate_mitigation_legitimacy kernel, each authored as a separate constraint with its own epsilon, stakeholders, and classification per the ε-invariance principle. portfolio_pragmatism_reading occupies a structurally intermediate position: it does not foreclose baseload_necessity_reading or renewable_primacy_reading (both remain live positions held by different planning coalitions and both could, in principle, be folded into a technology-neutral portfolio as a special case at the boundary), but it does create downstream pressure on both by legitimizing a 'both are needed, mix regionally' compromise that reduces the political urgency of either pure position winning outright. It stands in sharper tension with degrowth_sufficiency_reading, which rejects the entire supply-expansion framing this reading and its two closest siblings share.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_legitimacy__portfolio_pragmatism_reading, organized, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
