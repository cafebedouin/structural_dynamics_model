% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__baseload_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: climate_mitigation_legitimacy__baseload_necessity_reading
 *   human_readable: Baseload Necessity Reading of Decarbonization Legitimacy
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This is the baseload-necessity reading of the contested 'climate
 *   mitigation legitimacy' kernel: the claim that reliable decarbonization
 *   structurally requires dispatchable baseload power that renewables cannot
 *   provide at scale, and that nuclear (and other firm-capacity generation)
 *   is therefore necessary infrastructure rather than one option among
 *   several. The reading has a genuine technical core — dispatchability and
 *   grid inertia are real engineering concerns in many topologies — but it
 *   has hardened into a policy premise that channels capacity-market
 *   eligibility, rate-basing, and permitting priority toward incumbent
 *   nuclear operators and their financiers, while imposing exclusion,
 *   curtailment, and cost-overrun risk on renewable developers, ratepayers,
 *   and host communities. Three sibling readings of the same underlying
 *   kernel — renewable_primacy_reading, portfolio_pragmatism_reading, and
 *   degrowth_sufficiency_reading — are NOT part of this constraint; each is
 *   authored as its own file with its own epsilon, beneficiary/victim
 *   structure, and classification. This file evaluates only the
 *   baseload-necessity claim on its own structural merits.
 *
 * KEY AGENTS:
 *   - incumbent_nuclear_operators: primary beneficiary (institutional/arbitrage) — collects subsidy, rate-base returns, capacity-market revenue under the necessity framing
 *   - large_capital_utilities: primary beneficiary (institutional/arbitrage) — finances and profits from long-lived dispatchable assets
 *   - grid_reliability_engineers: agenda-setter with genuine technical stake (organized/constrained) — sets standards, has real professional grounds but also professional interest in the framing's adoption
 *   - distributed_solar_developers: primary target (moderate/constrained) — excluded from capacity eligibility, curtailed
 *   - ratepayers_in_cost_overrun_jurisdictions: primary target (powerless/trapped) — pays for overruns via regulated rate pass-through
 *   - communities_near_proposed_reactor_sites: primary target (powerless/trapped) — bears siting and waste burdens
 *   - climate_scientists_and_energy_modelers: analytical observer (analytical/analytical) — sees the full contested empirical landscape across all four kernel readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__baseload_necessity_reading, 0.51).
domain_priors:suppression_score(climate_mitigation_legitimacy__baseload_necessity_reading, 0.44).
domain_priors:theater_ratio(climate_mitigation_legitimacy__baseload_necessity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, extractiveness, 0.51).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__baseload_necessity_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__baseload_necessity_reading, "Baseload Necessity Reading of Decarbonization Legitimacy").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__baseload_necessity_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__baseload_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__baseload_necessity_reading, '3f47610c-9db0-4b59-be4b-2b7ea48c9fb5').
narrative_ontology:cs_kernel_codification('3f47610c-9db0-4b59-be4b-2b7ea48c9fb5', distributed).
narrative_ontology:cs_authority_grounding('3f47610c-9db0-4b59-be4b-2b7ea48c9fb5', distributed).
narrative_ontology:cs_reading_relation('3f47610c-9db0-4b59-be4b-2b7ea48c9fb5', climate_mitigation_legitimacy__renewable_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('3f47610c-9db0-4b59-be4b-2b7ea48c9fb5', climate_mitigation_legitimacy__portfolio_pragmatism_reading, coexists_with).
narrative_ontology:cs_reading_relation('3f47610c-9db0-4b59-be4b-2b7ea48c9fb5', climate_mitigation_legitimacy__degrowth_sufficiency_reading, influences).
narrative_ontology:cs_axiom('3f47610c-9db0-4b59-be4b-2b7ea48c9fb5', foundational, dispatchable_synchronous_generation_is_structurally_necessary).
narrative_ontology:cs_axiom_status(dispatchable_synchronous_generation_is_structurally_necessary, holdable).
narrative_ontology:cs_axiom_grounding('3f47610c-9db0-4b59-be4b-2b7ea48c9fb5', dispatchable_synchronous_generation_is_structurally_necessary, empirically_contingent).
narrative_ontology:cs_axiom('3f47610c-9db0-4b59-be4b-2b7ea48c9fb5', secondary, capital_intensive_long_lived_assets_are_the_correct_decarbonization_vehicle).
narrative_ontology:cs_axiom_status(capital_intensive_long_lived_assets_are_the_correct_decarbonization_vehicle, holdable).
narrative_ontology:cs_axiom_grounding('3f47610c-9db0-4b59-be4b-2b7ea48c9fb5', capital_intensive_long_lived_assets_are_the_correct_decarbonization_vehicle, instrumental).
narrative_ontology:cs_reference_frame('3f47610c-9db0-4b59-be4b-2b7ea48c9fb5', grid_reliability_engineering_consensus_pre_storage_maturity).
narrative_ontology:cs_drift_state('3f47610c-9db0-4b59-be4b-2b7ea48c9fb5', post_storage_cost_decline_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3f47610c-9db0-4b59-be4b-2b7ea48c9fb5', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, incumbent_nuclear_operators).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, large_capital_utilities).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, grid_reliability_engineers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, long_duration_construction_contractors).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, distributed_solar_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, ratepayers_in_cost_overrun_jurisdictions).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, communities_near_proposed_reactor_sites).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_only_grid_operators_denied_permits).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__baseload_necessity_reading, grid_stability_requires_synchronous_inertia).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__baseload_necessity_reading, capacity_factor_dominance_argument).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate existing reactor fleets and lobby regulators and legislators to classify baseload dispatchability as a legal requirement for decarbonization compliance, which channels subsidies, capacity payments, and permitting priority toward their asset class. They shape technical standards bodies that define 'reliability' in ways that favor synchronous generation.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, incumbent_nuclear_operators, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__baseload_necessity_reading, incumbent_nuclear_operators, agenda_setter).

% Hold balance sheets capable of financing multi-decade, multi-billion-dollar plant construction. The necessity framing justifies rate-basing long-lived assets and locking in guaranteed returns through regulated utility structures, insulating them from short-cycle renewable price competition.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, large_capital_utilities, beneficiary,
    institutional, generational, arbitrage, national).

% Design interconnection and reliability standards; genuinely believe (and have real technical grounds to believe) that synchronous inertia and dispatchable capacity solve real problems that inverter-based renewables have historically struggled with. Their professional standing and continued relevance is tied to the necessity framing being institutionally adopted.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, grid_reliability_engineers, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__baseload_necessity_reading, grid_reliability_engineers, beneficiary).

% Win multi-year, cost-plus-style construction contracts for nuclear builds. Overruns and schedule extensions increase billable work rather than penalizing them, so their incentives favor the most capital-intensive reading of the decarbonization problem.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, long_duration_construction_contractors, beneficiary,
    powerful, biographical, mobile, national).

% Build solar-plus-storage projects that are technically capable of serving demand but are excluded from capacity markets or curtailed to preserve headroom for baseload plants, on the argument that their output is non-dispatchable. Their capital is stranded or their interconnection queue position is deprioritized under baseload-necessity procurement rules.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, distributed_solar_developers, payer,
    moderate, biographical, constrained, regional).

% Pay through their electricity bills for nuclear construction cost overruns that regulators pass through under rate-basing agreements justified by the necessity framing. They have no meaningful vote over the generation mix and cannot switch utilities in a regulated monopoly service territory.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, ratepayers_in_cost_overrun_jurisdictions, payer,
    powerless, generational, trapped, regional).

% Bear siting, water-use, and long-term waste-storage burdens for facilities justified as nationally or globally necessary. Their local objections are frequently overridden by state or federal necessity determinations that treat the baseload requirement as settled policy fact rather than contested technical claim.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, communities_near_proposed_reactor_sites, payer,
    powerless, generational, trapped, local).

% Propose grid designs relying on renewables, storage, demand response, and transmission expansion, but are denied regulatory approval or capacity-market qualification on the grounds that their portfolio lacks 'firm' dispatchable capacity, even in jurisdictions with strong storage buildout data suggesting otherwise.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_only_grid_operators_denied_permits, payer,
    moderate, biographical, constrained, regional).

% Publish competing decarbonization pathway models; some support baseload necessity for specific grid topologies and demand profiles, others show renewables-plus-storage pathways achieving comparable reliability at lower cost in many but not all geographies. Their findings are selectively cited by advocates of each kernel reading.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, climate_scientists_and_energy_modelers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__baseload_necessity_reading, incumbent_nuclear_operators).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__baseload_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates long-horizon capital investment, grid interconnection standards, and reliability engineering around a specific technical claim: that synchronous, dispatchable generation is a structural requirement for a decarbonized grid, not merely one option among several. This genuinely solves a real coordination problem in geographies and demand profiles where storage and transmission buildout lag renewable penetration.
% TRANSFER_FUNCTION: Moves public subsidy, capacity-market revenue, permitting priority, and rate-base cost recovery toward nuclear and other dispatchable-asset operators and their financiers/contractors, and away from distributed and variable renewable developers; moves construction cost-overrun risk onto ratepayers and siting risk onto host communities.
% ABSENT_VOICES: Renewable-only grid operators and their engineering teams who have published counter-models are structurally present in academic debate but frequently excluded from the regulatory proceedings and legislative necessity determinations that actually allocate capacity-market eligibility and subsidy; host communities near proposed sites are consulted late, after necessity has already been declared as policy fact.
% DISAPPEARANCE_RATIONALE: If the baseload-necessity framing were formally abandoned as a policy premise, capacity markets and rate-basing rules would need to be rewritten, stranding some in-progress nuclear investment and opening capacity eligibility to storage and demand-response resources; incumbent operators and their financiers would experience real loss. But grid engineers in some geographies with high demand density and limited transmission would argue the underlying technical problem the framing addresses persists regardless of the policy label, so the world does not fully rearrange everywhere.
% FOUNDING_PROBLEM: Early-generation grid-scale renewable buildout, especially before cost-effective grid-scale storage and expanded transmission, produced real intermittency and frequency-stability incidents that dispatchable generation resolves; the framing was built to ensure decarbonization did not sacrifice grid reliability during the transition.
% FOUNDING_PROBLEM_CORROBORATION: Independent grid operators (e.g., regional transmission organizations) and academic energy-systems modelers outside the nuclear industry corroborate that dispatchability was and remains a genuine technical concern in specific grid topologies; however, the same independent modelers increasingly report that storage, demand response, and transmission expansion have closed much of the reliability gap in several jurisdictions since roughly 2015-2020, which the incumbent beneficiary coalition has been slow to incorporate into procurement rules.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__baseload_necessity_reading, contested).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__baseload_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__baseload_necessity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__baseload_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__baseload_necessity_reading, 0.51, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.51) sits at a moderate-high midpoint: the coordination function around real dispatchability concerns is genuine in specific grid topologies, but the framing has increasingly hardened into a policy default that channels capital and permitting toward incumbents beyond what the underlying technical case in many jurisdictions supports, as storage and transmission alternatives have matured since ~2015. Suppression (0.44) reflects legislative and regulatory necessity determinations that foreclose renewable-only pathways in some jurisdictions via capacity-market rules, but is not maximal because dissenting technical models remain publishable and some jurisdictions have begun revising procurement rules. Theater ratio (0.28) is moderate-low: most of the enforcement activity is substantive technical/regulatory work, not pure performance, though a growing share defends the necessity premise against mounting counter-evidence from advancing storage economics. Accessibility collapse (0.42) is moderate: alternative decarbonization pathways are not eliminated from public and academic discourse, but they are substantially foreclosed from actual capacity-market and permitting decisions in jurisdictions that have adopted the necessity framing as binding policy. Resistance (0.58) is meaningfully high because renewable developers, some ratepayer advocates, and a growing modeling literature actively contest the necessity claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent nuclear operators and large capital utilities sit near the full-beneficiary end: institutional power, arbitrage-grade exit (they can relocate capital across jurisdictions and asset classes), and they are named as direct capturers of subsidy and rate-base revenue. Grid reliability engineers are beneficiaries with constrained exit — their professional standing depends on standards continuing to encode dispatchability, but they are not capturing rents directly, they are administering a genuinely contested technical judgment. Distributed solar developers, ratepayers, and host communities sit near the full-target end: constrained or trapped exit, no control over the necessity determination, and bear cost or exclusion directly through the same structure that channels benefit to incumbents.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents two symmetric mislabeling errors: treating the entire necessity framing as pure extraction (which would erase the genuine grid-reliability coordination problem that motivated it originally and still applies in some topologies), and treating it as pure coordination (which would launder the observed capture — cost-overrun pass-through, siting burden externalization, and capacity-market exclusion of demonstrably capable alternatives — as incidental friction rather than structural transfer). Tangled Rope captures both: real coordination function plus asymmetric extraction through the same enforcement mechanism (capacity-market rules and necessity determinations).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dispatchability_technical_necessity_ambiguity,
    'Is dispatchable baseload generation a genuine, geography-invariant technical requirement for reliable decarbonized grids, or is it a requirement specific to certain grid topologies and demand profiles that has been generalized into a universal policy premise beyond its empirical scope?',
    'Comparative grid-reliability studies across jurisdictions with high renewable-plus-storage penetration (e.g., South Australia, parts of California, Denmark) versus jurisdictions maintaining high nuclear/dispatchable share, controlling for transmission interconnection and demand-response deployment.',
    'If dispatchability is geography-specific rather than universal, the necessity framing''s application as a blanket policy premise in low-need jurisdictions is closer to pure extraction; if genuinely universal, the coordination function is stronger than the extractive reading suggests and the constraint sits closer to a defensible tangled rope with more coordination weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dispatchability_technical_necessity_ambiguity, empirical, 'Whether baseload necessity is a universal technical fact or a topology-specific claim overgeneralized into policy.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Given that reasonable technical experts disagree about which of the four kernel readings (baseload necessity, renewable primacy, portfolio pragmatism, degrowth sufficiency) best describes the actual decarbonization requirement, is the selection of the baseload-necessity reading by any given jurisdiction''s regulators a good-faith technical judgment or a captured outcome favoring incumbent capital?',
    'Trace the funding sources, revolving-door employment patterns, and prior public positions of the specific regulators and legislators who adopted necessity-based capacity-market rules in each jurisdiction; compare against jurisdictions that adopted portfolio_pragmatism_reading or renewable_primacy_reading rules with similar underlying grid characteristics.',
    'If reading-selection correlates strongly with incumbent lobbying presence rather than grid topology, that supports treating the necessity framing''s adoption in a given jurisdiction as regulatory capture (snare-adjacent); if selection tracks genuine topology differences, the tangled_rope reading with real coordination weight is better supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether jurisdictional adoption of the necessity reading tracks genuine grid need or incumbent capture.').

omega_variable(
    cost_overrun_attribution_ambiguity,
    'Are nuclear construction cost overruns an intrinsic feature of the technology (justifying skepticism of the necessity framing''s cost claims) or primarily a function of regulatory, supply-chain, and workforce discontinuity that could be resolved with sustained build-out (supporting the necessity framing''s long-run cost trajectory claims)?',
    'Comparative analysis of cost trajectories in countries with continuous nuclear construction programs (e.g., South Korea) versus stop-start programs (e.g., US, UK) over the past three decades.',
    'If overruns are structural to the technology, the extraction borne by ratepayers is a permanent feature of the necessity framing''s implementation; if attributable to discontinuity, sustained commitment to the framing could reduce the extractive component over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_overrun_attribution_ambiguity, empirical, 'Whether nuclear cost overruns are technology-intrinsic or an artifact of inconsistent policy commitment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__baseload_necessity_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(clim_tr_t25, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 0, 0.33).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 15, 0.47).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 20, 0.49).
narrative_ontology:measurement(clim_be_t25, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 25, 0.51).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 5, 0.34).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 10, 0.37).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(clim_su_t25, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 25, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__baseload_necessity_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_legitimacy__baseload_necessity_reading, 0.15).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, portfolio_pragmatism_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the climate_mitigation_legitimacy kernel. Each reading has a distinct beneficiary/victim structure and distinct epsilon: baseload_necessity_reading concentrates benefit in incumbent nuclear/utility capital and construction contractors (epsilon 0.51, tangled_rope); renewable_primacy_reading concentrates benefit in renewable/storage developers and would classify nuclear capital lock-in as the extractive element; portfolio_pragmatism_reading distributes benefit across both technology classes with lower concentration and likely lower extractiveness; degrowth_sufficiency_reading reframes the entire generation-expansion question as unnecessary, implicating a different beneficiary/victim set entirely (demand-reduction advocates vs. all generation-expansion capital). These are not the same constraint measured four ways — they are four constraints sharing a contested kernel, linked here for contamination-propagation analysis, not for averaging.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
