% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__systems_transition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_imperative__systems_transition_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: climate_mitigation_imperative__systems_transition_reading
 *   human_readable: Climate Mitigation Imperative â Systems Transition Reading
 *   domain: energy_policy/climate_governance/technology_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the systems_transition_reading of the
 *   climate_mitigation_imperative kernel. The reading holds that climate
 *   mitigation is inseparable from a transformation of energy governance
 *   toward decentralization and democratic control, with nuclear energy
 *   categorized as a perpetuator of extractive centralization incompatible
 *   with that transition. The constraint is a governance structure that
 *   coordinates decarbonization around distributed ownership while
 *   asymmetrically extracting from nuclear and centralized utility sectors
 *   through exclusion, stranded asset risk, and denial of transition
 *   legitimacy. It is authored as a tangled_rope: genuine coordination
 *   function (decarbonization, democratic participation) coupled with
 *   asymmetric extraction (nuclear exclusion, centralized asset stranding).
 *
 * KEY AGENTS:
 *   - distributed_renewables_sector: Primary beneficiary (moderate/constrained) â receives policy preference and finance access
 *   - community_energy_cooperatives: Secondary beneficiary (moderate/constrained) â local democratic ownership structures advantaged by the frame
 *   - nuclear_sector: Primary target (institutional/constrained) â bears exclusion, stranded assets, and legitimacy denial
 *   - centralized_utility_operators: Secondary target (institutional/constrained) â bears stranded asset risk and grid-role devaluation
 *   - climate_policy_architects: Agenda setter (institutional/arbitrage) â administers the technology hierarchy and reading selection
 *   - ratepayers_in_transition: Diffuse payer (powerless/constrained) â bears transition costs without governance ownership
 *   - climate_pragmatist_researchers: Analytical observer (analytical/analytical) â sees the emissions-systems trade-off
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__systems_transition_reading, 0.72).
domain_priors:suppression_score(climate_mitigation_imperative__systems_transition_reading, 0.78).
domain_priors:theater_ratio(climate_mitigation_imperative__systems_transition_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__systems_transition_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__systems_transition_reading, "Climate Mitigation Imperative â Systems Transition Reading").
narrative_ontology:topic_domain(climate_mitigation_imperative__systems_transition_reading, "energy_policy/climate_governance/technology_policy").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__systems_transition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__systems_transition_reading, '181adf27-bff6-4694-b400-3bd4ca984a6d').
narrative_ontology:cs_kernel_codification('181adf27-bff6-4694-b400-3bd4ca984a6d', distributed).
narrative_ontology:cs_authority_grounding('181adf27-bff6-4694-b400-3bd4ca984a6d', distributed).
narrative_ontology:cs_reading_relation('181adf27-bff6-4694-b400-3bd4ca984a6d', climate_mitigation_imperative__portfolio_optimization_reading, influences).
narrative_ontology:cs_reading_relation('181adf27-bff6-4694-b400-3bd4ca984a6d', climate_mitigation_imperative__opportunity_cost_reading, coexists_with).
narrative_ontology:cs_axiom('181adf27-bff6-4694-b400-3bd4ca984a6d', foundational, nuclear_perpetuates_extractive_centralization).
narrative_ontology:cs_axiom_status(nuclear_perpetuates_extractive_centralization, holdable).
narrative_ontology:cs_axiom_grounding('181adf27-bff6-4694-b400-3bd4ca984a6d', nuclear_perpetuates_extractive_centralization, empirically_contingent).
narrative_ontology:cs_axiom('181adf27-bff6-4694-b400-3bd4ca984a6d', foundational, democratic_control_energy_prerequisite).
narrative_ontology:cs_axiom_status(democratic_control_energy_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('181adf27-bff6-4694-b400-3bd4ca984a6d', democratic_control_energy_prerequisite, deontological).
narrative_ontology:cs_reference_frame('181adf27-bff6-4694-b400-3bd4ca984a6d', democratic_energy_sovereignty).
narrative_ontology:cs_drift_state('181adf27-bff6-4694-b400-3bd4ca984a6d', contemporary_climate_policy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('181adf27-bff6-4694-b400-3bd4ca984a6d', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, distributed_renewables_sector).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, community_energy_cooperatives).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, nuclear_sector).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, centralized_utility_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, ratepayers_in_transition).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__systems_transition_reading, energy_democracy_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Commercial and non-profit actors deploying distributed solar, wind, and storage. Receive preferential policy treatment, subsidies, and grid access rules under the systems transition framing. Their market position depends on the governance preference for decentralized ownership models over centralized generation.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, distributed_renewables_sector, beneficiary,
    moderate, biographical, constrained, global).

% Local democratic ownership structures for generation and distribution assets. Benefit from feed-in tariffs, participatory governance mandates, and financing mechanisms available only under the decentralized transition frame. Would lose preferential standing if nuclear and centralized utilities were treated as equally legitimate low-carbon options.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, community_energy_cooperatives, beneficiary,
    moderate, biographical, constrained, local).

% Owns and operates centralized low-carbon baseload generation and the fuel cycle. Under this reading, faces categorical exclusion from green taxonomies, denial of transition finance, and regulatory phase-out mandates. Bears stranded asset risk, workforce devaluation, and loss of institutional knowledge pathways. Exit is constrained by long decommissioning liabilities and radiological stewardship obligations.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, nuclear_sector, payer,
    institutional, generational, constrained, global).

% Operate legacy centralized generation, transmission, and distribution grids. Face premature retirement pressure, stranded asset writedowns, and exclusion from green financing frameworks. Grid stability expertise is devalued as policy shifts toward distributed, variable generation without equivalent centralized backup recognition.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, centralized_utility_operators, payer,
    institutional, generational, constrained, national).

% International organizations, progressive legislators, and movement-aligned regulators who design climate mitigation frameworks. They administrate the technology hierarchy that ranks distributed renewables above nuclear and centralized assets. Derive authority from energy democracy and just transition narratives. Can shift between readings of the mitigation imperative without personally bearing the constraint's costs.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, climate_policy_architects, agenda_setter,
    institutional, generational, arbitrage, global).

% Residential and small commercial electricity customers who pay rates covering stranded centralized assets, grid modernization for distributed integration, and renewable energy surcharges. Have minimal influence over the technology mix and bear capital costs of the governance transition without direct ownership of new assets.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, ratepayers_in_transition, payer,
    powerless, biographical, constrained, national).

% Energy systems analysts and climate scientists who evaluate mitigation pathways by emissions intensity and system reliability. They observe that excluding nuclear reduces the feasible solution space and raises system costs, but are treated as technical inputs rather than governance participants under the transition frame.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, climate_pragmatist_researchers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes collective climate mitigation around distributed, democratically controlled energy ownership, aiming to decarbonize while preventing concentration of energy decision-making in state or corporate centralized entities.
% TRANSFER_FUNCTION: Moves capital flows, policy preference, grid access rights, and legitimacy from centralized nuclear and utility infrastructure toward distributed renewable ownership and community-controlled assets.
% ABSENT_VOICES: Nuclear engineers and climate pragmatists who regard nuclear as essential for decarbonization are structurally excluded from transition planning; fossil fuel workers and rural tax bases dependent on centralized plants are absent from governance design; consumer advocates focused on rate minimization rather than ownership structure are marginalized.
% DISAPPEARANCE_RATIONALE: If the imperative vanished, nuclear would re-enter the low-carbon portfolio, centralized utilities would retain baseload and grid stability roles, distributed cooperatives would compete without policy preference, and the governance frame of energy democracy would lose its monopoly on legitimate transition design â the climate response would reorganize around emissions minimization rather than systems transformation.
% FOUNDING_PROBLEM: Anthropogenic climate change driven by fossil fuel combustion, compounded by the democratic deficit of centralized corporate and state energy ownership that excludes communities from resource governance.
% FOUNDING_PROBLEM_CORROBORATION: Energy democracy scholars and environmental justice movements attest the democratic deficit is live. Climate scientists and energy systems engineers outside the benefiting parties attest the emissions problem is live but dispute whether governance transformation is a structurally necessary component of mitigation; they note the founding problem conflates decarbonization with anti-centralization.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__systems_transition_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__systems_transition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__systems_transition_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_imperative__systems_transition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__systems_transition_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_imperative__systems_transition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_imperative__systems_transition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_imperative__systems_transition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint redirects substantial capital and policy space away from existing low-carbon centralized assets toward preferred distributed alternatives, imposing stranded costs and devaluing grid stability expertise. Suppression (0.78) is higher because the reading's coherence depends on actively excluding nuclear from green taxonomies and finance frameworks â the constraint cannot persist if nuclear is admitted as a legitimate mitigation option. Theater_ratio (0.45) reflects that while distributed renewables do reduce emissions, a significant share of anti-nuclear advocacy serves ideological boundary maintenance rather than emissions optimization. Accessibility_collapse (0.60) captures that once the systems transition frame is accepted, centralized alternatives including nuclear become cognitively inaccessible as 'not transition-compatible.' Resistance (0.70) reflects strong pushback from nuclear sectors, utilities, and climate pragmatists. The measurement series share one time grid so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   From the distributed renewable seat and the policy architect seat, the constraint reads as genuine coordination solving both climate and democratic deficits simultaneously. From the nuclear sector and centralized utility seats, the same structure reads as extractive ideology that weaponizes governance language to exclude viable low-carbon infrastructure. The engine computes this divergence from structural data rather than adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   The distributed_renewables_sector and community_energy_cooperatives are structural beneficiaries: the constraint subsidizes their market position through policy preference and finance access (d near the beneficiary end). The nuclear_sector and centralized_utility_operators are structural targets: the constraint extracts through exclusion, phase-out mandates, and stranded asset creation (d near the target end). Ratepayers_in_transition sit at intermediate-high d: they bear diffuse costs of grid transformation and stranded assets without capturing the governance benefits. Climate_policy_architects sit near the beneficiary end despite being agenda_setters because they arbitrage between readings without bearing the constraint's costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents misreading this constraint as pure snare (which would ignore the genuine decarbonization and participatory governance function of distributed energy) or as pure rope (which would ignore the asymmetric extraction from nuclear and centralized ratepayers). The mandate has not atrophied â the founding problem of climate change remains live â but the constraint couples that live mandate to a specific governance preference that is contested, producing the hybrid profile.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nuclear_democratic_compatibility,
    'Is nuclear energy structurally incompatible with democratic control, or can it be governed through publicly accountable, democratic institutions?',
    'Comparative institutional analysis of publicly owned nuclear utilities and participatory governance models in energy systems.',
    'If compatible, the exclusion of nuclear from transition frameworks is ideological extraction rather than structural necessity, raising the effective extractiveness of the constraint; if incompatible, the axiom holds and the victimization is structurally warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_democratic_compatibility, empirical, 'Whether nuclear can coexist with democratic energy governance').

omega_variable(
    centralization_as_extraction,
    'Is energy centralization inherently extractive, or does extraction depend on ownership and governance models rather than scale?',
    'Historical analysis of centralized public ownership versus private monopoly patterns in energy provision.',
    'Would determine whether the reading''s victimization of centralized utilities and nuclear is a category error (conflating scale with extraction) or a structurally sound claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(centralization_as_extraction, conceptual, 'Whether centralization is intrinsically extractive').

omega_variable(
    kernel_reading_contest,
    'Does the systems_transition reading foreclose the portfolio_optimization reading within a unified climate policy framework, or do they merely compete for resources and legitimacy?',
    'Policy analysis of jurisdictions that have attempted to combine decentralized renewable expansion with nuclear retention.',
    'If foreclosed, the constraint family exhibits logical incompatibility and the systems_transition reading functions as a hard boundary; if coexisting, the tension is allocational and the extraction is primarily political-economic rather than epistemic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Logical relationship between systems_transition and portfolio_optimization readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__systems_transition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(clim_tr_t30, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(clim_tr_t40, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(clim_tr_t50, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(clim_be_t30, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(clim_be_t40, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(clim_be_t50, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 50, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(clim_su_t30, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(clim_su_t40, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(clim_su_t50, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, portfolio_optimization_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, opportunity_cost_reading).

% DUAL FORMULATION NOTE:
% The climate_mitigation_imperative kernel decomposes into three structurally distinct constraints: systems_transition_reading (governance transformation required), portfolio_optimization_reading (technology-neutral emissions minimization), and opportunity_cost_reading (economic speed optimization). They share the kernel label but have different epsilon values, beneficiary/victim structures, and coordination functions. Systems_transition_reading influences portfolio_optimization by shifting nuclear legitimacy conditions; it coexists with opportunity_cost as a complementary critique of nuclear from a governance rather than economic angle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
