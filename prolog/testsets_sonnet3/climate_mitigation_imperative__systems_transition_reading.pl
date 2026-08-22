% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__systems_transition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: climate_mitigation_imperative__systems_transition_reading
 *   human_readable: Climate Mitigation as Energy Democratization Imperative (Systems Transition Reading)
 *   domain: energy policy/climate mitigation/technology governance
 *
 * SUMMARY:
 *   This story instantiates the systems-transition reading of the climate
 *   mitigation imperative kernel: the claim that decarbonization requires not
 *   merely reducing emissions but transforming energy systems toward
 *   decentralized, democratically-controlled ownership structures, and that
 *   nuclear power — regardless of its carbon profile — is structurally
 *   disqualified because it perpetuates centralized, extractive control
 *   patterns. This is a distinct constraint from the opportunity-cost reading
 *   (which disqualifies nuclear on capital-efficiency grounds) and the
 *   portfolio-optimization reading (which includes nuclear as necessary
 *   baseload). The three readings share the label 'climate mitigation
 *   imperative' but diverge sharply on whether nuclear is included in the
 *   victim set, whether governance structure or dollar-per-ton is the
 *   operative metric, and what ε actually measures. This story's ε is
 *   authored strictly for the systems-transition reading's own arrangement —
 *   the coalition's actual policy and financing effects on the ground — not
 *   for the decentralized future it advocates for.
 *
 * KEY AGENTS:
 *   - energy_democracy_advocacy_networks: agenda_setter (organized/mobile) — defines mitigation legitimacy criteria
 *   - distributed_solar_developers: beneficiary (organized/mobile) — captures policy-driven market advantage
 *   - incumbent_nuclear_utilities: payer (powerful/constrained) — excluded on governance rather than emissions grounds
 *   - energy_poor_regions_without_capital_for_distributed_buildout: payer (powerless/trapped) — lacks capital for cooperative model, denied centralized alternative
 *   - carbon_accounting_bodies: observer (institutional/analytical) — measures emissions independent of ownership structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__systems_transition_reading, 0.62).
domain_priors:suppression_score(climate_mitigation_imperative__systems_transition_reading, 0.58).
domain_priors:theater_ratio(climate_mitigation_imperative__systems_transition_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__systems_transition_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__systems_transition_reading, "Climate Mitigation as Energy Democratization Imperative (Systems Transition Reading)").
narrative_ontology:topic_domain(climate_mitigation_imperative__systems_transition_reading, "energy policy/climate mitigation/technology governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__systems_transition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__systems_transition_reading, '7a7dcab5-806e-4fc6-8c70-16edd518bd0c').
narrative_ontology:cs_kernel_codification('7a7dcab5-806e-4fc6-8c70-16edd518bd0c', distributed).
narrative_ontology:cs_authority_grounding('7a7dcab5-806e-4fc6-8c70-16edd518bd0c', distributed).
narrative_ontology:cs_reading_relation('7a7dcab5-806e-4fc6-8c70-16edd518bd0c', climate_mitigation_imperative__opportunity_cost_reading, coexists_with).
narrative_ontology:cs_reading_relation('7a7dcab5-806e-4fc6-8c70-16edd518bd0c', climate_mitigation_imperative__portfolio_optimization_reading, forecloses).
narrative_ontology:cs_axiom('7a7dcab5-806e-4fc6-8c70-16edd518bd0c', foundational, ownership_structure_constitutive_of_mitigation).
narrative_ontology:cs_axiom_status(ownership_structure_constitutive_of_mitigation, holdable).
narrative_ontology:cs_axiom_grounding('7a7dcab5-806e-4fc6-8c70-16edd518bd0c', ownership_structure_constitutive_of_mitigation, conventional).
narrative_ontology:cs_axiom('7a7dcab5-806e-4fc6-8c70-16edd518bd0c', foundational, centralized_generation_is_inherently_extractive).
narrative_ontology:cs_axiom_status(centralized_generation_is_inherently_extractive, holdable).
narrative_ontology:cs_axiom_grounding('7a7dcab5-806e-4fc6-8c70-16edd518bd0c', centralized_generation_is_inherently_extractive, instrumental).
narrative_ontology:cs_reference_frame('7a7dcab5-806e-4fc6-8c70-16edd518bd0c', post_utility_monopoly_reform_baseline).
narrative_ontology:cs_drift_state('7a7dcab5-806e-4fc6-8c70-16edd518bd0c', contemporary_decarbonization_acceleration_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7a7dcab5-806e-4fc6-8c70-16edd518bd0c', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, distributed_solar_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, community_energy_cooperatives).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, grid_edge_technology_firms).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, energy_democracy_advocacy_networks).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, incumbent_nuclear_utilities).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, nuclear_construction_workforce).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, grid_reliability_dependent_industries).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, energy_poor_regions_without_capital_for_distributed_buildout).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__systems_transition_reading, decentralization_reduces_extraction_doctrine).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__systems_transition_reading, democratic_control_improves_climate_outcomes_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the terms of what counts as legitimate mitigation policy through coalition lobbying, model legislation, and framing campaigns that tie carbon reduction to ownership structure. Determines which technologies are admissible to 'real' climate solutions by defining decentralization and democratic governance as constitutive of mitigation itself, not merely one strategy among several.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, energy_democracy_advocacy_networks, agenda_setter,
    organized, generational, mobile, national).

% Capture subsidy streams, interconnection priority, and favorable regulatory treatment that follow from mitigation policy being defined in terms of distributed, community-owned generation. Their business model is structurally advantaged whenever nuclear licensing or centralized transmission investment is deprioritized as 'off-mission.'
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, distributed_solar_developers, beneficiary,
    organized, biographical, mobile, regional).

% Gain legitimacy, grant funding, and technical assistance specifically because the mitigation framework treats local ownership and democratic control as core requirements rather than incidental features. Genuinely solve real coordination problems around local buy-in and siting, but also become the political face used to justify excluding centralized alternatives.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, community_energy_cooperatives, beneficiary,
    moderate, generational, constrained, local).

% Operate or seek to build large centralized generation assets that are reclassified, under this reading, as structurally incompatible with mitigation's true purpose regardless of their carbon output. Face permitting delays, exclusion from green taxonomies, and financing disadvantages driven by governance-structure criteria rather than emissions performance. Cannot exit the framing fight without abandoning multi-decade capital commitments already sunk.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, incumbent_nuclear_utilities, payer,
    powerful, generational, constrained, national).

% Skilled tradespeople and engineers whose employment depends on nuclear project pipelines that shrink as this reading gains policy traction. Have specialized, non-transferable skills tied to a technology now framed as illegitimate on governance grounds rather than technical or safety grounds, leaving them with few comparable-wage exits.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, nuclear_construction_workforce, payer,
    moderate, biographical, trapped, regional).

% Heavy industry, hospitals, and data centers that require dispatchable, high-capacity-factor baseload power. Bear reliability and cost risk if firm centralized generation is systematically deprioritized in favor of distributed intermittent sources, without a governance-neutral assessment of whether decentralization actually delivers equivalent reliability at their scale.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, grid_reliability_dependent_industries, payer,
    powerful, biographical, constrained, national).

% Lack the upfront capital, land tenure security, or grid infrastructure to participate in community-owned distributed generation, and are simultaneously denied the large centralized plants that could otherwise electrify them quickly, because those plants are framed as the wrong kind of solution regardless of their availability or cost per delivered kilowatt-hour.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, energy_poor_regions_without_capital_for_distributed_buildout, payer,
    powerless, biographical, trapped, regional).

% Sell microgrid controllers, battery storage, and peer-to-peer trading platforms whose addressable market expands directly with the policy premium placed on decentralized architecture. Benefit from the framing regardless of whether decentralization is the least-cost decarbonization path for any given grid.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, grid_edge_technology_firms, beneficiary,
    organized, biographical, arbitrage, global).

% Measure emissions outcomes independent of ownership structure and increasingly note that this reading's governance criteria are not derived from, and sometimes conflict with, the emissions-minimization objective that mitigation is nominally about.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, carbon_accounting_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_imperative__systems_transition_reading, distributed_solar_developers).
narrative_ontology:fixing_cost_class(climate_mitigation_imperative__systems_transition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely coordinates fragmented local actors — households, municipalities, cooperatives — around siting, interconnection, and financing of distributed generation that would otherwise be blocked by utility incumbency and permitting friction; also coordinates a political coalition capable of sustained climate policy pressure across election cycles.
% TRANSFER_FUNCTION: Moves regulatory priority, subsidy allocation, green-taxonomy eligibility, and public legitimacy away from centralized generation (nuclear specifically) and toward distributed, cooperatively-owned generation — regardless of the relative decarbonization-per-dollar or reliability performance of either pathway.
% ABSENT_VOICES: Nuclear engineers, grid reliability planners, and residents of energy-poor regions without capital for cooperative buy-in are rarely centered in energy-democracy coalition meetings; their objection — that governance-structure criteria can slow total decarbonization and leave capital-poor communities under-electrified — is voiced mainly in technical planning documents and utility filings, not in the advocacy spaces that set this reading's agenda.
% DISAPPEARANCE_RATIONALE: If the requirement that mitigation be measured by decentralization and democratic ownership vanished, permitting and financing decisions would revert to emissions-per-dollar and reliability criteria; nuclear projects currently disadvantaged by governance-based taxonomy exclusion would become financeable again, cooperative energy programs would need to justify themselves on cost and community-benefit grounds rather than ideological necessity, and grid-edge technology firms would lose a policy-driven demand premium.
% FOUNDING_PROBLEM: Centralized utility monopolies historically excluded communities from energy decision-making, sited fossil and nuclear infrastructure disproportionately in disempowered communities, and captured regulatory processes to protect incumbent generation assets against genuinely cleaner or more local alternatives.
% FOUNDING_PROBLEM_CORROBORATION: Grid reliability engineers and carbon accounting bodies, entities outside the energy-democracy coalition, attest that utility capture and siting injustice were real historical problems but argue the problem has been partially addressed through interconnection reform and environmental-justice siting rules, and that the remaining decarbonization gap is now primarily a deployment-speed and financing problem rather than a governance-structure problem — meaning the original problem is only partly live, while the reading's remedy has hardened into a fixed requirement independent of that reassessment.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__systems_transition_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__systems_transition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__systems_transition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_imperative__systems_transition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__systems_transition_reading, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.62) reflects that real coordination value exists — community energy cooperatives do solve genuine siting and buy-in problems, and historical utility capture was real — but the constraint also transfers substantial regulatory and financial advantage to organized beneficiaries (distributed solar developers, grid-edge tech firms) at the direct expense of actors (nuclear utilities, their workforce, capital-poor regions) whose disqualification tracks governance structure rather than decarbonization performance. Suppression (0.58) is moderate: enforcement runs through green taxonomy exclusion, subsidy design, and permitting friction rather than direct coercion, but it is real and rising as the measurements show. Theater ratio is comparatively low (0.28) because the coordination function (organizing dispersed local actors, building durable political coalitions) is substantially genuine, not merely performative — this is why the story claims tangled_rope rather than snare.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, this reading is coherent coordination: uniting fragmented local actors against historically real utility capture. From the incumbent nuclear utility seat, the identical structure is exclusionary rent allocation dressed in governance language, disconnected from the emissions math the utilities can point to. The engine should compute divergent per-seat types from these two structural positions even though both parties describe 'the same' climate mitigation imperative.
 *
 * DIRECTIONALITY LOGIC:
 *   Distributed solar developers, cooperatives, and grid-edge technology firms sit near the beneficiary end: they gain policy priority and market share as a direct function of the governance-structure criterion, independent of their actual decarbonization efficiency. Nuclear utilities and their workforce sit near the target end: they bear exclusion costs keyed to ownership structure and centralization, not to their emissions performance, and their exit options are constrained by sunk multi-decade capital and non-transferable skills. Energy-poor regions are an especially sharp case — they are structurally unable to access either the cooperative model (no capital) or the centralized alternative (excluded on governance grounds), making them doubly trapped payers rather than simple bystanders.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — utility capture and inequitable siting — was genuinely live historically and is only partially resolved; the reading's genealogy status is authored as contested rather than dead specifically because corroborating sources outside the coalition (grid engineers, accounting bodies) attest that the underlying injustice was real but has been partly addressed by other reforms, while the decentralization requirement itself has hardened past the point the founding problem justifies. This divergence — status contested, verdict world_rearranges — is exactly the kind of mismatch the R5 consumer is designed to flag: a coordination function that was real is now doing extraction work beyond its original justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    governance_criterion_vs_emissions_criterion,
    'Is decentralized democratic ownership actually necessary for effective decarbonization, or is it a normatively separate goal being bundled into the mitigation imperative by a specific political coalition?',
    'Comparative jurisdictional analysis: do regions that pursue emissions-neutral technology selection (agnostic to ownership structure) achieve comparable or faster per-capita decarbonization than regions that impose governance-structure requirements? If comparable, the requirement is separable from the mitigation function itself.',
    'If decentralization is separable from decarbonization efficacy, this reading''s exclusion of nuclear is better characterized as ideological rent allocation riding on the climate mandate''s legitimacy; if inseparable (e.g., because democratic buy-in is empirically necessary for durable multi-decade policy), the coordination function is more central and the tangled_rope classification''s coordination half strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(governance_criterion_vs_emissions_criterion, empirical, 'Whether governance structure is empirically bundled with or separable from decarbonization outcomes.').

omega_variable(
    sibling_reading_kernel_disagreement_location,
    'The three readings of the climate mitigation imperative kernel (this one, opportunity_cost, portfolio_optimization) disagree specifically on whether nuclear belongs in the beneficiary set, victim set, or neither — where exactly is this disagreement located structurally?',
    'Trace each reading''s disqualification/inclusion logic for nuclear to its root premise: opportunity_cost roots in capital-efficiency-per-ton; portfolio_optimization roots in reliability-completeness; systems_transition roots in ownership-structure-as-constitutive-of-mitigation. These are three independent axes (cost, reliability, governance) that happen to converge on the same technology as their test case.',
    'Because the disagreement is located in which axis is treated as constitutive of ''mitigation'' rather than in disputed facts about nuclear itself, no single empirical finding about nuclear''s cost or safety record can resolve the kernel contest — each reading could in principle concede the empirical facts and still reach a different verdict on inclusion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_kernel_disagreement_location, conceptual, 'Locating the disagreement between sibling readings on the axis of constitutive criterion, not on disputed nuclear facts.').

omega_variable(
    coalition_capture_vs_genuine_democratic_coordination,
    'Is the energy-democracy advocacy coalition a genuine expression of community coordination preference, or has it been substantially captured by commercial beneficiaries (distributed solar developers, grid-edge tech firms) who gain from the governance criterion regardless of community preference?',
    'Trace funding sources and agenda-setting influence within the coalition: what share of advocacy funding, technical framing, and legislative drafting originates from commercial beneficiaries versus grassroots community organizations with no commercial stake in the outcome.',
    'High commercial capture would push this reading closer to snare (coordination story as cover for extraction); predominantly grassroots, non-commercial origination would support a more genuine tangled_rope or even rope reading of the coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coalition_capture_vs_genuine_democratic_coordination, empirical, 'Degree of commercial capture within the energy-democracy advocacy coalition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__systems_transition_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(clim_tr_t4, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(clim_tr_t8, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(clim_tr_t12, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 12, 0.23).
narrative_ontology:measurement(clim_tr_t16, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(clim_tr_t24, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(clim_be_t4, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(clim_be_t8, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 8, 0.51).
narrative_ontology:measurement(clim_be_t12, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(clim_be_t16, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(clim_be_t24, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(clim_su_t4, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 4, 0.41).
narrative_ontology:measurement(clim_su_t8, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(clim_su_t12, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(clim_su_t16, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(clim_su_t24, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__systems_transition_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_imperative__systems_transition_reading, 0.12).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative__opportunity_cost_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative__portfolio_optimization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the climate_mitigation_imperative kernel, decomposed per the ε-invariance principle because the three readings assign structurally different victim/beneficiary sets to the same technology (nuclear) using different constitutive criteria (governance structure vs. capital efficiency vs. portfolio completeness). Each reading carries its own ε, its own claimed_type, and its own stakeholder set. This reading (systems_transition) has the highest suppression trajectory of the three because its enforcement mechanism (green taxonomy exclusion tied to ownership structure) is the most structurally distinct from emissions accounting and therefore requires the most active defense against emissions-only counterarguments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
