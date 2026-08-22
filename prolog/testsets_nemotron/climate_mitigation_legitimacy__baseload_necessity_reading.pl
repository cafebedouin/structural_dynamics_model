% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__baseload_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-28
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__baseload_necessity_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: climate_mitigation_legitimacy__baseload_necessity_reading
 *   human_readable: Baseload Necessity Reading of Climate Mitigation Legitimacy
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   The baseload necessity reading frames climate mitigation as a
 *   reliability-first engineering problem: the grid requires dispatchable,
 *   synchronous generation that only nuclear (and fossil, and large hydro)
 *   can provide at scale. This reading gained dominance in the 1970s-1990s as
 *   nuclear was scaled to displace oil and coal, and persists in capacity
 *   market designs, reliability standards, and integrated resource plans that
 *   structurally favor resources with high capacity factors and synchronous
 *   inertia. The constraint is genuinely tangled: it coordinates a real
 *   physical problem (grid stability) while extracting rents for incumbent
 *   capital through regulatory structures that suppress lower-cost
 *   alternatives (wind, solar, storage, demand response). The theater ratio
 *   rises over time as the "reliability" justification becomes increasingly
 *   decoupled from demonstrated technical necessity — grid-forming inverters
 *   and hybrid renewables+storage now provide the same services in
 *   operational systems.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__baseload_necessity_reading, 0.68).
domain_priors:suppression_score(climate_mitigation_legitimacy__baseload_necessity_reading, 0.62).
domain_priors:theater_ratio(climate_mitigation_legitimacy__baseload_necessity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__baseload_necessity_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__baseload_necessity_reading, "Baseload Necessity Reading of Climate Mitigation Legitimacy").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__baseload_necessity_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__baseload_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__baseload_necessity_reading, '976b5e05-6710-4ea0-ad7f-8250ecceed19').
narrative_ontology:cs_kernel_codification('976b5e05-6710-4ea0-ad7f-8250ecceed19', implicit).
narrative_ontology:cs_authority_grounding('976b5e05-6710-4ea0-ad7f-8250ecceed19', extraction).
narrative_ontology:cs_interpretation_layer_present('976b5e05-6710-4ea0-ad7f-8250ecceed19').
narrative_ontology:cs_reading_relation('976b5e05-6710-4ea0-ad7f-8250ecceed19', climate_mitigation_legitimacy__renewable_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('976b5e05-6710-4ea0-ad7f-8250ecceed19', climate_mitigation_legitimacy__portfolio_pragmatism_reading, influences).
narrative_ontology:cs_reading_relation('976b5e05-6710-4ea0-ad7f-8250ecceed19', climate_mitigation_legitimacy__degrowth_sufficiency_reading, influences).
narrative_ontology:cs_axiom('976b5e05-6710-4ea0-ad7f-8250ecceed19', foundational, synchronous_inertia_is_irreplaceable_for_grid_stability).
narrative_ontology:cs_axiom_status(synchronous_inertia_is_irreplaceable_for_grid_stability, holdable).
narrative_ontology:cs_axiom_grounding('976b5e05-6710-4ea0-ad7f-8250ecceed19', synchronous_inertia_is_irreplaceable_for_grid_stability, empirically_contingent).
narrative_ontology:cs_axiom('976b5e05-6710-4ea0-ad7f-8250ecceed19', foundational, nuclear_is_the_only_scalable_zero_carbon_baseload).
narrative_ontology:cs_axiom_status(nuclear_is_the_only_scalable_zero_carbon_baseload, holdable).
narrative_ontology:cs_axiom_grounding('976b5e05-6710-4ea0-ad7f-8250ecceed19', nuclear_is_the_only_scalable_zero_carbon_baseload, empirically_contingent).
narrative_ontology:cs_axiom('976b5e05-6710-4ea0-ad7f-8250ecceed19', secondary, reliability_mandate_justifies_capital_concentration).
narrative_ontology:cs_axiom_status(reliability_mandate_justifies_capital_concentration, holdable).
narrative_ontology:cs_axiom_grounding('976b5e05-6710-4ea0-ad7f-8250ecceed19', reliability_mandate_justifies_capital_concentration, instrumental).
narrative_ontology:cs_reference_frame('976b5e05-6710-4ea0-ad7f-8250ecceed19', postwar_synchronous_grid_paradigm).
narrative_ontology:cs_drift_state('976b5e05-6710-4ea0-ad7f-8250ecceed19', contemporary_inverter_dominated_grid, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('976b5e05-6710-4ea0-ad7f-8250ecceed19', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, large_utility_companies).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, grid_operators).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, baseload_technology_vendors).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, ratepayers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_energy_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, distributed_energy_communities).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, future_generations_bearing_lockin_costs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Owns and operates the dispatchable baseload fleet that this reading classifies as necessary. Receives capacity payments, long-term power purchase agreements, and regulatory support for new builds. Can deploy capital across jurisdictions and technology variants (large LWRs, SMRs, advanced reactors) — exit means shifting investment portfolio, not abandoning the sector.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Own the transmission and distribution infrastructure that baseload plants interconnect with. Benefit from regulated rate-of-return on massive capital assets and from the planning certainty that "baseload necessity" provides for multi-decade investment cycles. Exit is constrained by franchise territories and sunk network assets.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, large_utility_companies, beneficiary,
    institutional, generational, constrained, national).

% Set reliability standards, capacity market rules, and interconnection queues that structurally favor dispatchable resources. Their operational mandate (keep the lights on) aligns with the baseload narrative. Exit means regulatory restructuring or mandate change — institutionally difficult but not impossible.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, grid_operators, agenda_setter,
    institutional, generational, constrained, regional).

% Supply reactor components, fuel cycles, and specialized engineering services. Benefit from the long-lived, high-capital equipment contracts that baseload necessity justifies. Can pivot to adjacent industrial markets (marine propulsion, medical isotopes, process heat) — exit is mobile at the firm level.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, baseload_technology_vendors, beneficiary,
    organized, biographical, mobile, global).

% Bear the cost of capacity payments, stranded asset risk, and above-market power purchase agreements through electricity bills. Exit is constrained — cannot practically disconnect from the grid, and retail choice (where it exists) still socializes system costs. Low-income ratepayers are disproportionately exposed.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, ratepayers, payer,
    moderate, biographical, constrained, local).

% Face interconnection queues, capacity market rules, and reliability standards designed around synchronous generation. The baseload narrative is used to justify slower interconnection, curtailment priority, and exclusion from capacity payments. Can redeploy capital to other jurisdictions or technologies — exit is mobile at the portfolio level.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_energy_developers, payer,
    organized, biographical, mobile, global).

% Seek to build local solar, storage, and microgrid resources but face regulatory barriers (interconnection caps, standby charges, prohibitions on peer-to-peer trading) justified by grid stability arguments that presume baseload necessity. Exit is trapped — cannot leave the utility service territory, and the regulatory framework actively suppresses alternatives.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, distributed_energy_communities, payer,
    powerless, biographical, trapped, local).

% Inherit the climate risk of delayed decarbonization if baseload necessity slows renewable deployment, the fiscal burden of stranded nuclear assets, and the radiological legacy of long-lived waste. No exit — they are not yet born and cannot opt out of the infrastructure decisions made today.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, future_generations_bearing_lockin_costs, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(climate_mitigation_legitimacy__baseload_necessity_reading, future_generations_bearing_lockin_costs).

% Model decarbonization pathways across technology portfolios. Their scenario sets (IPCC, IEA, NGFS) include both baseload-heavy and renewable-dominant pathways. The constraint shapes which scenarios are treated as "realistic" by policy-makers and financiers.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, energy_system_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine coordination problem of maintaining grid reliability (frequency stability, voltage control, black-start capability) while decarbonizing — a physical-engineering problem that requires someone to provide synchronous inertia and firm capacity.
% TRANSFER_FUNCTION: Moves capital and revenue from ratepayers (via regulated rates and capacity markets) and renewable developers (via foregone market access) to nuclear plant owners, utility shareholders, and baseload technology vendors, as the price of the reliability coordination service.
% ABSENT_VOICES: Communities hosting uranium mining and waste storage (disproportionately Indigenous and low-income) are excluded from the reliability framing. Industrial heat users who could use high-temperature reactors but are locked into fossil contracts are not represented. Small modular reactor startups without incumbent utility partnerships are marginalized in the "proven baseload" narrative.
% DISAPPEARANCE_RATIONALE: If the baseload necessity constraint vanished overnight, capacity markets would be redesigned around firm capacity from any technology (storage, demand response, hydrogen turbines, geothermal), interconnection queues would clear on technology-neutral terms, and capital would flow to the lowest-cost firm resources — likely accelerating renewable+storage deployment but also creating reliability gaps during the transition.
% FOUNDING_PROBLEM: Post-WWII grid reliability was built around large synchronous generators (coal, hydro, nuclear). The founding problem was: how to maintain the reliability achievements of that system while replacing its carbon-emitting components. The baseload necessity reading answers: by preserving the synchronous generator architecture with low-carbon fuel.
% FOUNDING_PROBLEM_CORROBORATION: Grid operators and nuclear vendors attest the reliability problem is live and requires synchronous generation. Independent system operators (CAISO, ERCOT, PJM) and renewable integration studies (NREL, BNEF, IEA) attest that inverter-based resources with grid-forming controls can provide equivalent reliability services — the founding problem's technical premise is contested by the operational community that would implement alternatives.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__baseload_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__baseload_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__baseload_necessity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(climate_mitigation_legitimacy__baseload_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__baseload_necessity_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__baseload_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__baseload_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__baseload_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the capital transfer from ratepayers and excluded competitors to baseload asset owners through capacity payments, regulated returns, and market rules that internalize baseload's value but externalize its costs (waste, decommissioning, proliferation risk). Suppression (0.62) captures the active regulatory barriers: interconnection queues that prioritize synchronous generation, capacity market rules that disadvantage storage, standby charges on distributed resources, and planning processes that treat renewable variability as a cost rather than a managed variable. Theater (0.42) measures the growing gap between the reliability rhetoric and the demonstrated capability of inverter-based resources — the coordination function is real but the exclusivity claim is not. Accessibility collapse (0.55) is moderate: alternatives exist and are deploying, but the constraint makes them harder and more expensive. Resistance (0.58) reflects active pushback from renewable developers, consumer advocates, and some regulators.
 *
 * PERSPECTIVAL GAP:
 *   From the grid operator's seat, the constraint is genuine coordination — they experience the physical reality of frequency stability and see baseload as the proven solution. From the renewable developer's seat, the same constraint is extraction — they experience the queue delays, curtailment, and market exclusion as rent protection. From the distributed community's seat, it is a snare — active suppression of their alternatives. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear industry, utilities, grid operators, and vendors are beneficiaries — they collect rents, control planning, and set the rules. Their exit options range from arbitrage (global nuclear vendors) to constrained (utilities, grid operators). Ratepayers are payers with constrained exit — they cannot leave the grid. Renewable developers are victims with mobile exit — they can move capital but lose market access. Distributed energy communities and future generations are trapped victims — no practical exit from the regulatory territory or the temporal lock-in. The analytical observer seat sees the full structure but has no material stake.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reliability during decarbonization) is contested — the technical premise that only synchronous generation can provide reliability services is challenged by operational data. The mandate has not atrophied (reliability is still required) but the exclusivity claim has. The constraint persists because the beneficiary coalition (nuclear + utilities + grid operators) controls the planning institutions, not because the coordination function requires exclusivity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inverter_based_reliability_equivalence,
    'Can inverter-based resources with grid-forming controls provide reliability services (frequency response, voltage control, black start) equivalent to synchronous generators at comparable system cost?',
    'Operational data from high-renewable grids (South Australia, Texas, California, Germany) and controlled demonstrations (NREL UNIFI consortium, IEEE 2800 standard adoption).',
    'If equivalence is demonstrated, the coordination function no longer requires baseload exclusivity — the constraint''s extraction becomes unsupported by technical necessity, shifting classification toward snare. If equivalence fails at scale, the coordination function remains genuine and tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inverter_based_reliability_equivalence, empirical, 'Whether the physical coordination problem genuinely requires synchronous generation or whether that is a legacy assumption.').

omega_variable(
    capital_lockin_vs_climate_urgency,
    'Does the capital concentration in 60-80 year nuclear assets accelerate or delay net-zero trajectories compared to modular, faster-to-deploy alternatives?',
    'Integrated assessment modeling with endogenous learning curves, deployment rate constraints, and opportunity cost of capital — comparing baseload-heavy vs. renewable-heavy pathways under identical carbon budgets.',
    'If baseload concentration delays net-zero, the constraint extracts not just money but carbon budget — future generations bear climate damages from slower decarbonization. This would elevate the victim status of future_generations_bearing_lockin_costs and increase effective extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capital_lockin_vs_climate_urgency, empirical, 'Whether the capital structure this constraint enforces is compatible with the pace of decarbonization required.').

omega_variable(
    regulatory_capture_of_reliability_standards,
    'Are reliability standards (NERC, IEC, regional grid codes) set through genuine engineering consensus or do they embed incumbent technology preferences?',
    'Process tracing of standard-setting proceedings: who proposes, who opposes, what evidence is admitted, and whether inverter-based resource capabilities are evaluated on equivalent terms.',
    'If standards are captured, suppression is higher than measured — the constraint actively engineers the technical requirements to match its beneficiaries'' assets. If standards are open, suppression reflects genuine technical conservatism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_of_reliability_standards, conceptual, 'Whether the enforcement machinery is neutrally administered or structurally biased toward baseload technologies.').

omega_variable(
    committer_frame_ambiguity,
    'Does this reading''s core premise (baseload necessity) logically foreclose the renewable_primacy_reading, or do they coexist as competing frameworks for different institutional actors?',
    'Analyze whether any single jurisdiction''s policy framework can simultaneously treat baseload as necessary AND renewables+storage as sufficient — or whether adopting one reading forces rejection of the other in planning, procurement, and regulation.',
    'If forecloses, the readings are mutually exclusive in practice — choosing baseload_necessity structurally blocks renewable_primacy. If coexists_with, both can be operative in different institutions (e.g., capacity market uses baseload framing while energy market clears renewable-heavy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_ambiguity, conceptual, 'Structural relationship between this reading and the renewable_primacy_reading sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__baseload_necessity_reading, 1970, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(baseload_necessity_tr_t1970, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(baseload_necessity_tr_t1985, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 1985, 0.22).
narrative_ontology:measurement(baseload_necessity_tr_t2000, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement(baseload_necessity_tr_t2010, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 2010, 0.33).
narrative_ontology:measurement(baseload_necessity_tr_t2020, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 2020, 0.38).
narrative_ontology:measurement(baseload_necessity_tr_t2035, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 2035, 0.42).

% Extraction over time
narrative_ontology:measurement(baseload_necessity_be_t1970, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(baseload_necessity_be_t1985, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 1985, 0.45).
narrative_ontology:measurement(baseload_necessity_be_t2000, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 2000, 0.52).
narrative_ontology:measurement(baseload_necessity_be_t2010, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(baseload_necessity_be_t2020, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement(baseload_necessity_be_t2035, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 2035, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(baseload_necessity_su_t1970, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 1970, 0.25).
narrative_ontology:measurement(baseload_necessity_su_t1985, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 1985, 0.35).
narrative_ontology:measurement(baseload_necessity_su_t2000, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 2000, 0.42).
narrative_ontology:measurement(baseload_necessity_su_t2010, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 2010, 0.5).
narrative_ontology:measurement(baseload_necessity_su_t2020, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 2020, 0.58).
narrative_ontology:measurement(baseload_necessity_su_t2035, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 2035, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__baseload_necessity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(climate_mitigation_legitimacy__baseload_necessity_reading, 0.12).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_licensing_regime).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, capacity_market_design).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, transmission_planning_authority).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_interconnection_queues).

% DUAL FORMULATION NOTE:
% Part of the climate_mitigation_legitimacy kernel family. This reading (baseload_necessity) coexists with renewable_primacy_reading and portfolio_pragmatism_reading in different institutional venues, but influences degrowth_sufficiency_reading by absorbing capital that demand-reduction policies would redirect. The epsilon values differ substantially: baseload_necessity ε≈0.68 (high capital concentration, active suppression), renewable_primacy ε≈0.35 (lower suppression, different beneficiary set), portfolio_pragmatism ε≈0.25 (technology-neutral rules reduce extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_legitimacy__baseload_necessity_reading, institutional, 0.15).
constraint_indexing:directionality_override(climate_mitigation_legitimacy__baseload_necessity_reading, organized, 0.35).
constraint_indexing:directionality_override(climate_mitigation_legitimacy__baseload_necessity_reading, moderate, 0.75).
constraint_indexing:directionality_override(climate_mitigation_legitimacy__baseload_necessity_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
