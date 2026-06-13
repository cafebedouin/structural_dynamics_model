% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__systems_transition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_mitigation_imperative__systems_transition_reading
 *   human_readable: Climate Mitigation via Democratic Energy Transition (Systems Reading)
 *   domain: energy_policy/climate/governance
 *
 * SUMMARY:
 *   Climate mitigation is contested at the systems level. The kernel binding
 *   is to rapid decarbonization; the reading question is whether
 *   decarbonization mandate entails or merely coexists with governance
 *   transformation. The systems-transition reading claims that true
 *   mitigation requires transforming energy systems toward decentralization
 *   and democratic control, and that nuclear (along with conventional
 *   centralized generation) is structurally incompatible with this
 *   transformation and therefore a victim of the mandate, not a tool for
 *   achieving it. This reading does NOT argue nuclear is unnecessary for
 *   emissions reduction (that is the opportunity-cost reading's claim). It
 *   argues nuclear instantiates the centralization that the transition must
 *   overcome. Distributed renewables and energy-democracy movements are
 *   beneficiaries because their decentralized, participatory models align
 *   with the governance transformation the reading binds to mitigation.
 *   Incumbent centralized generation is the payer. The constraint is tangled
 *   rope: it coordinates genuine decarbonization need with a governance
 *   transformation mandate, but asymmetrically extracts from incumbent
 *   infrastructure to fund distributed alternatives.
 *
 * KEY AGENTS:
 *   - climate_mitigation_imperative (framed as systems transformation mandate): the overarching institutional/narrative authority that binds carbon reduction to governance transformation
 *   - distributed_renewable_operators (moderate/mobile): beneficiaries from policy favoring decentralization; face underinvestment if systems reading is overridden
 *   - energy_democracy_movements (organized/mobile): beneficiaries from elevation of governance demands into legitimacy conditions; lose leverage if mitigation decouples from democracy mandate
 *   - incumbent_nuclear_infrastructure (institutional/trapped): classified as victim under this reading because centralized generation is deemed incompatible with transition, though nuclear can decarbonize
 *   - centralized_generation_stakeholders (powerful/constrained): secondary victims facing asset stranding and revenue loss as distributed resources capture market share under policy favoritism
 *   - grid_operators (institutional/constrained): excluded from mandate design; their technical requirements for stability treated as constraints to work within rather than inputs to the reading
 *   - low_income_and_rural_communities (powerless/trapped): excluded from beneficiary framing; risk deepening energy poverty if transition prioritizes governance over access
 *   - climate_science_authority (institutional/analytical): observes whether the transition delivers required decarbonization; does not adjudicate the political claim binding governance to mitigation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__systems_transition_reading, 0.68).
domain_priors:suppression_score(climate_mitigation_imperative__systems_transition_reading, 0.52).
domain_priors:theater_ratio(climate_mitigation_imperative__systems_transition_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__systems_transition_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__systems_transition_reading, "Climate Mitigation via Democratic Energy Transition (Systems Reading)").
narrative_ontology:topic_domain(climate_mitigation_imperative__systems_transition_reading, "energy_policy/climate/governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__systems_transition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__systems_transition_reading, '4fc24cb9-a5a0-4905-8a16-8bd245cbdb22').
narrative_ontology:cs_kernel_codification('4fc24cb9-a5a0-4905-8a16-8bd245cbdb22', distributed).
narrative_ontology:cs_authority_grounding('4fc24cb9-a5a0-4905-8a16-8bd245cbdb22', distributed).
narrative_ontology:cs_reading_relation('4fc24cb9-a5a0-4905-8a16-8bd245cbdb22', climate_mitigation_imperative__opportunity_cost_reading, coexists_with).
narrative_ontology:cs_reading_relation('4fc24cb9-a5a0-4905-8a16-8bd245cbdb22', climate_mitigation_imperative__portfolio_optimization_reading, coexists_with).
narrative_ontology:cs_axiom('4fc24cb9-a5a0-4905-8a16-8bd245cbdb22', foundational, energy_governance_transformation_necessary_for_mitigation).
narrative_ontology:cs_axiom_status(energy_governance_transformation_necessary_for_mitigation, holdable).
narrative_ontology:cs_axiom_grounding('4fc24cb9-a5a0-4905-8a16-8bd245cbdb22', energy_governance_transformation_necessary_for_mitigation, deontological).
narrative_ontology:cs_axiom('4fc24cb9-a5a0-4905-8a16-8bd245cbdb22', foundational, decentralization_and_democratic_control_incompatible_with_nuclear_centralization).
narrative_ontology:cs_axiom_status(decentralization_and_democratic_control_incompatible_with_nuclear_centralization, holdable).
narrative_ontology:cs_axiom_grounding('4fc24cb9-a5a0-4905-8a16-8bd245cbdb22', decentralization_and_democratic_control_incompatible_with_nuclear_centralization, deontological).
narrative_ontology:cs_reference_frame('4fc24cb9-a5a0-4905-8a16-8bd245cbdb22', fossil_fuel_enabled_by_centralized_governance_topology).
narrative_ontology:cs_drift_state('4fc24cb9-a5a0-4905-8a16-8bd245cbdb22', contemporary_energy_transition_phase, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4fc24cb9-a5a0-4905-8a16-8bd245cbdb22', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, distributed_renewable_operators).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, energy_democracy_movements).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, local_grid_participants).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, incumbent_nuclear_infrastructure).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, centralized_generation_stakeholders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The overarching commitment to decarbonize energy systems within climate-compatible timelines. Under the systems-transition reading, this imperative is NOT merely a carbon-reduction target but a structural directive: mitigation must simultaneously achieve democratic control and decentralization. The reading binds mitigation success to governance transformation, not just emissions reduction.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative_as_framed, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative_as_framed).

% Small-scale solar, wind, and distributed-storage installations operated by communities, cooperatives, municipalities, and independent developers. Under the systems reading, they are structurally aligned with mitigation because their decentralized topology instantiates the governance transformation the reading requires. They benefit from policy mandates favoring distributed resources, grid interconnection standards, and financing mechanisms that de-risk small-scale deployment. Their exit option: if the reading is overridden by portfolio optimization (allowing nuclear), they face underinvestment and regulatory disadvantage.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, distributed_renewable_operators, beneficiary,
    moderate, generational, mobile, regional).

% Advocacy coalitions, worker organizations, and communities organizing for democratic control of energy infrastructure: opposition to fossil extraction, demand for participatory planning, ownership models that exclude speculative capital. They benefit from the systems reading because it elevates their governance demands into the legitimacy conditions of mitigation itself. Their exit option: if mitigation is decoupled from democratic governance (portfolio optimization reading dominates), they lose leverage.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, energy_democracy_movements, beneficiary,
    organized, generational, mobile, continental).

% Existing nuclear generation assets, their regulatory regimes, supply chains, and workforce. Under the systems reading, nuclear is reclassified as structurally incompatible with democratic transition because its capital concentration, centralized dispatch, and long-term commitment lock in the existing governance topology. The reading does not argue nuclear is unnecessary for decarbonization (that is the opportunity-cost reading's domain); it argues nuclear instantiates the centralization the transition must overcome. Incumbent nuclear bears extraction: it must either fund its own exit or accept accelerated retirement under systems-prioritizing policy. Its exit option is political: lobby to override the systems reading with alternative framings.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, incumbent_nuclear_infrastructure, payer,
    institutional, civilizational, trapped, national).

% Coal, natural-gas, and large hydroelectric operators who benefit from centralized, dispatchable generation and the regulatory frameworks built around it. Under the systems reading, they are secondary victims: the decentralization mandate threatens their dispatch model even if individual plants could technically co-exist with distributed renewables. They face stranded-asset risk and revenue erosion as distributed resources capture market share under policy favoritism.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, centralized_generation_stakeholders, payer,
    powerful, biographical, constrained, national).

% Entities responsible for real-time grid stability and reliability. They are excluded from the systems-transition reading's mandate design: the reading prioritizes governance transformation and does not center grid operators' technical requirements for stability with high distributed-generation penetration. Their absence from the beneficiary set means their concerns (voltage stability, frequency support, ramp rates) are treated as constraints to work within rather than inputs to the reading's framing. They would argue for a hybrid portfolio including nuclear for its reliable baseload characteristics.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, grid_operators_and_balancing_authorities, excluded,
    institutional, biographical, constrained, national).

% Communities that currently depend on incumbent centralized generation for affordable electricity and would need massive investment in local generation and storage to participate in distributed systems. The systems reading does not foreground their access and affordability constraints; it centers governance transformation. Their exclusion creates a risk: the transition can be implemented in ways that deepen energy poverty for those unable to invest in rooftop solar or community solar shares. They would need guarantees that the democratic transition includes democratic access, not just democratic control.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, low_income_and_rural_communities, excluded,
    powerless, biographical, trapped, local).

% Scientific consensus on climate mitigation necessity and carbon budgets. The reading instrumentalizes this authority: it binds climate imperative to governance transformation. The observation seat uses climate science to evaluate the reading's empirical grounding (does the transition deliver required decarbonization?) but does not adjudicate the political claim that democratic governance is part of the mitigation mandate.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, climate_science_authority, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_imperative__systems_transition_reading, distributed_renewable_operators).
narrative_ontology:fixing_cost_class(climate_mitigation_imperative__systems_transition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes energy system decarbonization around the principle that meeting climate targets simultaneously requires transforming governance toward democratic control and decentralization. Solves the claimed coordination problem: how to achieve rapid, just, and durable decarbonization without reproducing the centralized, extractive infrastructure that enabled fossil-fuel lock-in.
% TRANSFER_FUNCTION: Moves policy support, capital allocation, grid priority, and regulatory legitimacy from incumbent centralized generation (coal, gas, nuclear) to distributed renewables and community-controlled infrastructure. Extracts constraints (forced retirement paths, stranded assets, revenue loss) from nuclear and conventional generation to fund the transition.
% ABSENT_VOICES: Grid operators and balancing authorities, whose technical requirements for reliability with high distributed-generation penetration are not centered in the reading's governance framing. Low-income and rural communities, whose access and affordability constraints are not explicit in the systems mandate. Incumbent nuclear workers and supply-chain communities, whose livelihood transitions are not foregrounded. These absences shape the reading's risk: the transition can be implemented in ways that solve governance without solving access or just transition for affected workers.
% DISAPPEARANCE_RATIONALE: If the systems-transition reading disappeared and portfolio optimization or opportunity-cost framings dominated, energy policy would reorganize around maximizing low-carbon deployment (nuclear included) or minimizing cost per ton of CO2 reduced. The decentralization and democratic-control mandates would no longer be legitimacy conditions; they would become nice-to-haves competed against technical and economic optimization. The constraint's disappearance would remove a major pressure on incumbent centralized infrastructure and would reframe distributed renewables as one tool among many rather than as the structurally required outcome.
% FOUNDING_PROBLEM: Energy systems have historically concentrated capital, control, and decision-making in ways that enabled fossil-fuel lock-in, created political barriers to decarbonization, and externalized costs onto communities. The problem the reading binds to mitigation is: how to decarbonize in a way that simultaneously addresses the governance failures that enabled the climate crisis itself? The founding mandate is that true mitigation requires transforming the structures that produced the problem.
% FOUNDING_PROBLEM_CORROBORATION: Energy-democracy advocates, some climate scientists, and labor-union organizations attest the problem is live and that just transition requires governance transformation alongside decarbonization. Portfolio-optimization advocates and nuclear operators argue the founding problem (centralized governance as obstacle to decarbonization) is being solved by other means: markets are rapidly deploying renewables, decentralization is happening through economics not mandate, and adding governance requirements delays necessary deployment. Independent research from outside the benefiting parties (Sunstein 2020 on regulatory lock-in, Sovacool on energy justice, IEA on system integration) supports the reading's claim that governance structure shapes transition outcomes, though they do not all endorse the systems reading's specific binding of mitigation to decentralization-as-mandate.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__systems_transition_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__systems_transition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__systems_transition_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_imperative__systems_transition_reading, 'none', 1).

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
 *   Extractiveness is measured at 0.68 (interval end) because the reading imposes substantial constraints on incumbent centralized generation (stranded assets, accelerated retirement, policy disadvantage) to fund distributed alternatives. This is not pure rent extraction — it coordinates genuine decarbonization — but the asymmetry is real and substantial. Suppression is moderate-to-moderately-high (0.52 at interval end, rising from 0.38 at start) because the reading's persistence requires actively suppressing alternative framings (portfolio optimization, opportunity-cost readings) and the technological-incumbent coalitions that defend them. As policy pressure on centralized generation increases, enforcement machinery tightens (suppression_requirement rises). Theater ratio is moderate (0.41 at interval end) and stable after t=20, indicating that a significant share of the reading's activation is performative commitment-signaling rather than actual governance transformation. Measurement series show extraction and suppression rising over 20 time units (observed period, ~2003-2023), then plateauing (projected period, ~2024-2043), consistent with a reading that entered policy discourse with force but faces consolidating resistance from incumbent actors. The plateau suggests the constraint reaches a stable operational point where neither beneficiaries nor victims can force further dominance.
 *
 * PERSPECTIVAL GAP:
 *   Beneficiaries and victims compute dramatically different types. From the distributed-renewable operators' seat, the reading is rope: genuine coordination (climate need + governance improvement aligned) with symmetric benefits. From incumbent nuclear's seat, the reading is snare: the coordination justification (decarbonization) is cover for governance mandates that extract from existing infrastructure. From energy-democracy movements' seat, the reading is rope with strong justice framing (the transition fixes governance failures). From grid-operator seats, the reading is tangled rope: they coordinate on decarbonization (shared interest) but their technical input is suppressed in favor of ideology (governance mandate over engineering). From low-income-community seats, the reading is snare: promises decarbonization but risks deepening energy poverty if implementation prioritizes governance transformation over access. The engine computes each seat's type from the positional data; the authored claim (tangled_rope) reflects neither beneficiary nor victim seats but the structure itself — the reading IS tangled because it genuinely coordinates decarbonization AND asymmetrically extracts from incumbent infrastructure. The perspectival divergence is diagnostic of tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality computation flows from beneficiary/victim declarations and exit options. Distributed renewables (d~0.2) and energy-democracy movements (d~0.15) are beneficiaries with mobile exit: they can shift to other jurisdictions or framings if this reading is overridden; the reading subsidizes them. Incumbent nuclear (d~0.85) and centralized generation (d~0.80) are victims trapped in their infrastructure; they must either fund their own exit (costly) or lobby to override the reading with alternative framings. Grid operators (d~0.55) are near-symmetric despite exclusion: they benefit from the technical challenge of integrating distributed resources (expanded role) but bear suppression cost (their concerns are treated as constraints). Low-income communities (d~0.75) are partially trapped victims despite potential rhetorical alignment: they depend on affordable electricity but the reading does not foreground access-and-affordability guarantees. The reading's extraction surface is concentrated on incumbent infrastructure (high d, high χ) and diffuse on energy-poor populations (moderate d, moderate χ from dependency rather than direct policy extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem of the systems reading is: energy systems have historically enabled fossil-fuel lock-in by concentrating power in centralized governance structures that resisted decarbonization. The reading binds mitigation to solving this founding problem. The mandatrophy risk is: if decarbonization succeeds through portfolio optimization (mixed low-carbon sources, centralized dispatch, market mechanisms) without governance transformation, the founding problem (governance barriers to decarbonization) is shown to be overridden by other forces (economics, climate urgency, technology learning curves). The constraint would persist as a mandated governance ideal but with dead founding problem. The measurement series shows extraction rising 0-20 and plateauing 20-40: consistent with a reading that gained policy force (extraction rising as it was enforced) but hit resistance and stabilized at a constrained equilibrium rather than achieving transformation. The theater_ratio stabilization at 0.41 suggests the constraint is maintaining itself partly through performative commitment (green mandates, renewable targets, democratic-governance rhetoric) while actual governance remains substantially centralized (nuclear plants operate under centralized dispatch, distributed renewables are grid-connected to centralized balancing authorities). This pattern — stable but not transformative, rhetorical commitment without structural change — is consistent with mandatrophy-at-risk: the founding problem (centralized governance as barrier to decarbonization) may be addressed by other means (market learning curves, policy pressure on emissions not on governance), making the constraint's governance mandate increasingly vestigial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    governance_transformation_decoupling_risk,
    'Is decarbonization of energy systems structurally dependent on governance transformation toward decentralization and democratic control, or can rapid emissions reduction be achieved through markets, portfolios, and improved regulation within centralized institutional structures?',
    'Counterfactual analysis: jurisdictions that achieve rapid decarbonization via portfolio optimization (e.g., France with nuclear, Denmark with distributed renewables but centralized dispatch) against jurisdictions that mandate decentralization alongside decarbonization. Measurement of whether system resilience, cost, speed, and public support differ across governance models.',
    'If decarbonization succeeds under portfolio optimization without decentralization, the systems reading becomes aspirational (desirable but not necessary for mitigation). If decentralization proves necessary for resilience, public support, or climate-justice outcomes, the reading''s binding of governance to mitigation is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(governance_transformation_decoupling_risk, empirical, 'Whether mitigation mandate logically entails governance transformation or whether they are separable commitments.').

omega_variable(
    nuclear_as_victim_vs_tool_boundary,
    'Is nuclear technology structurally incompatible with democratic, decentralized energy governance, or is the incompatibility contingent on current regulatory and ownership models that could be reformed?',
    'Institutional redesign thought experiment: could small modular reactors (SMRs) owned and operated by municipal/cooperative entities be compatible with the systems reading? Could democratic oversight and participatory governance structures be layered onto nuclear operations? If yes, the reading''s classification of nuclear as victim is contingent on current centralization, not inherent to the technology.',
    'If institutional redesign could make nuclear compatible with democratic governance, the systems reading''s exclusion of nuclear is a political choice, not a technical necessity. If nuclear''s capital scale and operational complexity make decentralized democratic control infeasible, the reading''s victim classification is structurally grounded.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nuclear_as_victim_vs_tool_boundary, conceptual, 'Whether the systems reading''s opposition to nuclear is technological or institutional.').

omega_variable(
    beneficiary_expansion_and_capture_risk,
    'Does the systems reading''s beneficiary set (distributed renewable operators, energy democracy movements) represent constituencies with genuine power to enforce the mandate, or has the reading created a beneficiary coalition that appears aligned but lacks enforcement capacity, creating a capture-by-ambition failure?',
    'Political-economy analysis of the beneficiary constituencies'' actual leverage over energy policy, finance, and infrastructure deployment. Do distributed renewable operators have market power independent of the mandate? Do energy-democracy movements have electoral or institutional leverage, or only narrative legitimacy? If beneficiaries lack enforcement capacity, who enforces the reading''s requirements against incumbent resistance?',
    'If beneficiaries are weak relative to incumbent incumbents, the constraint may function as a performative mandate (high theater_ratio) that incumbent actors navigate around while maintaining core centralized infrastructure. If beneficiaries have genuine power, the constraint enforces real transformation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_expansion_and_capture_risk, empirical, 'Whether the beneficiary coalition has enforcement capacity or whether the constraint is performatively ambitious without power to back it.').

omega_variable(
    just_transition_inclusion_gap,
    'Is the systems-transition reading''s mandate for democratic energy governance compatible with ensuring just transition and energy access for low-income and rural communities, or do the two commitments require different infrastructure investments and policy designs that can conflict?',
    'Policy analysis of transition pathways that prioritize both democratic governance and equitable access. Empirical comparison of jurisdictions that mandate decentralization against outcomes for energy-poor populations: do they achieve both or does one degrade the other? Community testimony from energy-poor constituencies on whether decentralization mandates include their participation and affordability.',
    'If democratic energy transition can only be achieved by excluding or further burdening energy-poor communities, the constraint''s justice framing is incoherent and invites redesign. If inclusive democratic transition is feasible, the constraint needs tighter specification of who counts as ''democratic'' participant and how access is guaranteed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(just_transition_inclusion_gap, empirical, 'Whether the systems reading''s governance mandate is compatible with equitable access and just transition outcomes.').

omega_variable(
    kernel_contest_framing_choice,
    'The climate_mitigation_imperative kernel admits three structurally distinct readings (systems_transition, portfolio_optimization, opportunity_cost), each binding different structural commitments to the mitigation mandate. This constraint instantiates ONE reading. What metadata distinguishes this reading from its siblings and makes it the appropriate one to apply in a given context?',
    'Normative specification: what values, constituencies, or empirical findings would justify adopting THIS reading over the sibling readings? The engine computes seat-level type divergence; this omega documents why the reading itself was chosen as the framing for the constraint.',
    'If the choice of reading is under-determined (all three are equally well-grounded), the corpus must include all three as separate constraints linked via network.affects_constraints, and the reading-selection process becomes a higher-order question about legitimacy and whose values drive energy policy. If this reading is justified by specific commitments to energy justice, democratic governance, or climate resilience arguments, those commitments should be explicit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_framing_choice, preference, 'Which reading of the mitigation kernel is structurally justified and why.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__systems_transition_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(clim_tr_t5, observed).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(clim_tr_t10, observed).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(clim_tr_t15, observed).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(clim_tr_t20, observed).
narrative_ontology:measurement(clim_tr_t25, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(clim_tr_t25, projected).
narrative_ontology:measurement(clim_tr_t30, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(clim_tr_t30, projected).
narrative_ontology:measurement(clim_tr_t40, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(clim_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(clim_be_t5, observed).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(clim_be_t10, observed).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(clim_be_t15, observed).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(clim_be_t20, observed).
narrative_ontology:measurement(clim_be_t25, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(clim_be_t25, projected).
narrative_ontology:measurement(clim_be_t30, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(clim_be_t30, projected).
narrative_ontology:measurement(clim_be_t40, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(clim_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 5, 0.41).
narrative_ontology:measurement_basis(clim_su_t5, observed).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement_basis(clim_su_t10, observed).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement_basis(clim_su_t15, observed).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement_basis(clim_su_t20, observed).
narrative_ontology:measurement(clim_su_t25, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 25, 0.52).
narrative_ontology:measurement_basis(clim_su_t25, projected).
narrative_ontology:measurement(clim_su_t30, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(clim_su_t30, projected).
narrative_ontology:measurement(clim_su_t40, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(clim_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__systems_transition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(climate_mitigation_imperative__systems_transition_reading, 0.12).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative__portfolio_optimization_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative__opportunity_cost_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, nuclear_facility_siting_consent).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, renewable_grid_integration_standards).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, energy_worker_displacement_protections).

% DUAL FORMULATION NOTE:
% The climate_mitigation_imperative kernel admits three structurally distinct constraint stories corresponding to three readings. This file instantiates the systems_transition reading. The portfolio_optimization_reading and opportunity_cost_reading are separate constraints with different beneficiary/victim sets, different ε values, and potentially different claimed types. All three are linked via network.affects_constraints because they compete to instantiate the same mandate and changing the adopted reading affects the scope and enforcement of the others. The decomposition reflects the ε-invariance principle: the three readings emit different structural constraints (different victim sets, different extraction mechanisms) and therefore cannot be fused into a single story with measurement-dependent type. Each reading is ε-invariant under its own interpretation of what decarbonization requires.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
