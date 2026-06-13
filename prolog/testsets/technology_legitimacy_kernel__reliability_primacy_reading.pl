% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__reliability_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_legitimacy_kernel__reliability_primacy_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: technology_legitimacy_kernel__reliability_primacy_reading
 *   human_readable: Reliability Primacy Legitimacy Gate for Climate Technologies
 *   domain: energy/climate/governance
 *
 * SUMMARY:
 *   The reliability-primacy reading of the technology-legitimacy kernel
 *   claims that a technology's right to climate-mitigation support depends
 *   solely on its ability to provide dispatchable, baseload generation
 *   ensuring grid stability. This reading is one of three structurally
 *   distinct framings of what makes a climate technology 'legitimate.' Under
 *   this reading, nuclear and natural-gas plants with high capacity factors
 *   and fast-response capability are intrinsically climate-legitimate;
 *   intermittent renewables are legitimate only if paired with sufficient
 *   storage to meet dispatchability requirements; deployment velocity and
 *   worst-case failure reversibility are secondary or tertiary concerns. The
 *   constraint enforces this by conditioning grid interconnection,
 *   capacity-auction participation, and regulatory approval on
 *   dispatchability metrics. The committer-frame tension: grid operators and
 *   reliability engineers genuinely need stable frequency and voltage, making
 *   the constraint's coordination function real; however, the choice to make
 *   dispatchability the SOLE legitimacy criterion (rather than one criterion
 *   among several) advantages incumbent dispatchable operators and burdens
 *   renewable developers and ratepayers. The reading competes with
 *   velocity-primacy (which would privilege fast deployment over stability
 *   margins) and precautionary (which would privilege reversibility and
 *   bounded failure modes).
 *
 * KEY AGENTS:
 *   - grid_operators: institutional agenda-setter, trapped in current architecture, enforce the reliability-primacy frame through interconnection standards and dispatch rules
 *   - nuclear_operators: institutional beneficiary, mobile exit options, gain carbon-free legitimacy and protected dispatch economics
 *   - renewable_energy_developers: organized payers, constrained exit (must comply or leave the market), face higher integration costs and burden-of-proof standards
 *   - ratepayers: powerless payers, trapped, bear the cost of redundant dispatchable capacity and storage infrastructure
 *   - battery_storage_manufacturers: powerful, arbitrage exit, benefit from constraint-driven storage demand but vulnerable to alternative framings
 *   - regulatory_authorities: institutional observers, analytical seat, have power to reframe the kernel but currently enforce reliability primacy
 *   - climate_scientists: analytical beneficiaries, excluded from technology certification (voice present in founding problem, absent from enforcement)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__reliability_primacy_reading, 0.68).
domain_priors:suppression_score(technology_legitimacy_kernel__reliability_primacy_reading, 0.52).
domain_priors:theater_ratio(technology_legitimacy_kernel__reliability_primacy_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__reliability_primacy_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__reliability_primacy_reading, "Reliability Primacy Legitimacy Gate for Climate Technologies").
narrative_ontology:topic_domain(technology_legitimacy_kernel__reliability_primacy_reading, "energy/climate/governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__reliability_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__reliability_primacy_reading, '94cb0374-c41e-4e82-9709-23520a33409a').
narrative_ontology:cs_kernel_codification('94cb0374-c41e-4e82-9709-23520a33409a', formalized).
narrative_ontology:cs_authority_grounding('94cb0374-c41e-4e82-9709-23520a33409a', extraction).
narrative_ontology:cs_interpretation_layer_present('94cb0374-c41e-4e82-9709-23520a33409a').
narrative_ontology:cs_reading_relation('94cb0374-c41e-4e82-9709-23520a33409a', technology_legitimacy_kernel__velocity_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('94cb0374-c41e-4e82-9709-23520a33409a', technology_legitimacy_kernel__precautionary_reading, coexists_with).
narrative_ontology:cs_axiom('94cb0374-c41e-4e82-9709-23520a33409a', foundational, dispatchability_primacy).
narrative_ontology:cs_axiom_status(dispatchability_primacy, holdable).
narrative_ontology:cs_axiom_grounding('94cb0374-c41e-4e82-9709-23520a33409a', dispatchability_primacy, empirically_contingent).
narrative_ontology:cs_axiom('94cb0374-c41e-4e82-9709-23520a33409a', foundational, grid_stability_paramount).
narrative_ontology:cs_axiom_status(grid_stability_paramount, holdable).
narrative_ontology:cs_axiom_grounding('94cb0374-c41e-4e82-9709-23520a33409a', grid_stability_paramount, instrumental).
narrative_ontology:cs_reference_frame('94cb0374-c41e-4e82-9709-23520a33409a', dispatchable_generation_grid_stability).
narrative_ontology:cs_drift_state('94cb0374-c41e-4e82-9709-23520a33409a', contemporary_renewable_acceleration, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('94cb0374-c41e-4e82-9709-23520a33409a', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, nuclear_operators).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, grid_operators).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, reliability_engineering_establishment).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, ratepayers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, renewable_energy_developers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, distributed_generation_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, battery_storage_manufacturers).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, climate_scientists).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, battery_storage_manufacturers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Manage real-time frequency and voltage stability across the transmission system. Under reliability-primacy framing, they adjudicate which technologies qualify as 'grid-stabilizing' based on dispatchability metrics. They enforce the constraint through grid interconnection standards, dispatch scheduling rules, and ancillary service requirements. Benefit from a constraint that privileges the dispatchable generators they historically control and understand.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, grid_operators, agenda_setter,
    institutional, generational, trapped, regional).

% Operate large, dispatchable, baseload generation with high capacity factors (>90%). The reliability-primacy frame legitimizes their continued operation and new construction by treating dispatchability as the paramount climate virtue. They collect carbon-free credentials and operational support without needing to develop expensive storage, fast-response ramp rates, or variable-cost integration strategies.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, nuclear_operators, beneficiary,
    institutional, generational, mobile, national).

% Develop wind, solar, and other intermittent renewables. Under reliability-primacy framing, their technologies are treated as incomplete without paired battery storage or other costly dispatchability augmentation, raising capital requirements and lowering returns. The constraint subjects them to burden-of-proof standards (proving grid stability impact) that baseload operators are exempted from.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, renewable_energy_developers, payer,
    organized, biographical, constrained, national).

% Manufacture battery systems deployed alongside renewables to create synthetic dispatchability. They benefit from the constraint because it creates regulatory demand for storage co-location. They also bear costs if storage-plus-renewable bundles become uneconomic relative to nuclear or natural-gas baselines, constraining their total addressable market.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, battery_storage_manufacturers, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__reliability_primacy_reading, battery_storage_manufacturers, payer).

% Pay electricity bills and capital costs for grid investments. Under reliability-primacy framing, they bear the cost of redundant dispatchable capacity, expensive fast-response reserves, and storage systems that the constraint necessitates. The constraint masks this cost transfer by treating reliability as a non-negotiable engineering requirement rather than a policy choice with distributional consequences.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, ratepayers, payer,
    powerless, immediate, trapped, local).

% Study climate mitigation pathways and decarbonization timelines. The reliability-primacy frame privileges their concern (grid stability is essential) but excludes their voice from adjudicating what counts as 'sufficient' stability, what trade-offs with deployment velocity are acceptable, and whether near-term emissions reductions outweigh long-term reliability margins. The constraint is enforced by engineers, not climate modelers.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, climate_scientists, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__reliability_primacy_reading, climate_scientists, excluded).

% Operate fossil-fuel dispatchable generators that are losing economic and regulatory legitimacy. The constraint de facto legitimizes their dispatchable architecture (if not their carbon intensity) and can slow their displacement by privileging baseload and fast-response attributes they possess; however, their explicit exclusion from climate-mitigation legitimacy means they cannot be open beneficiaries. They are structurally trapped: excluded from the climate narrative but favored by its technology-selection criteria.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, coal_and_gas_operators, excluded,
    institutional, biographical, constrained, national).

% Set grid interconnection standards, renewable portfolio standards, and technology certification rules. They observe the constraint's operation and must adjudicate conflicts between reliability-primacy and competing principles (velocity, reversibility, cost). They have the power to reframe the kernel (e.g., by elevating velocity or precaution to co-equal status) but currently enforce reliability primacy as the binding legitimacy gate.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, regulatory_authorities, observer,
    institutional, generational, analytical, national).

% Operate rooftop solar, small wind, microhydro, and other distributed, variable-output systems. The reliability-primacy frame treats their generation as grid-destabilizing (variable, non-dispatchable) unless augmented with costly local storage or demand management. They face higher integration costs and longer interconnection timelines than centralized dispatchable generators.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, distributed_generation_operators, payer,
    moderate, biographical, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_legitimacy_kernel__reliability_primacy_reading, nuclear_operators).
narrative_ontology:fixing_cost_class(technology_legitimacy_kernel__reliability_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures grid frequency and voltage stability by requiring generation sources to be dispatchable (responsive to real-time demand, ramp-capable) and baseload-capable (available on demand at rated capacity). Solves the engineering problem of maintaining reliable power supply amid variable load and contingent failures.
% TRANSFER_FUNCTION: Transfers capital and operational costs from grid operators and nuclear/dispatchable-generator operators to renewable developers (storage requirement), distributed generators (integration surcharges), and ratepayers (higher electricity costs and reserve margins). Also transfers legitimacy (climate credibility) from intermittent renewables to dispatchable technologies.
% ABSENT_VOICES: Climate scientists and decarbonization velocity analysts would object that the constraint weights reliability above emissions-reduction speed, but they are excluded from technology-certification decisions. Fossil-fuel operators would defend the constraint's criteria (they meet the dispatchability requirement) but are barred from citing it. Ratepayers and low-income consumers bear the cost but have no seat in grid-planning decisions.
% DISAPPEARANCE_RATIONALE: If the constraint vanished and replaced reliability-primacy with velocity-primacy or precautionary criteria, technology development investments would shift dramatically: renewable deployment would accelerate (no storage burden), nuclear licensing might slow (long-build-time disadvantage), distributed generation would proliferate, storage R&D priorities would shift from duration/capacity to rapid response. Grid operational practices and reserve margin requirements would be recalibrated. Electricity prices would follow these reallocation patterns.
% FOUNDING_PROBLEM: Early renewable-heavy grids in high-penetration regions (Denmark, Germany, California) experienced frequency instability, voltage swings, and greater reliance on fossil-fuel fast-response generators for balancing. The constraint was built to ensure dispatchable generation remains available to stabilize the grid during periods of low wind/solar output.
% FOUNDING_PROBLEM_CORROBORATION: Grid operators and reliability engineers attest the problem remains live and worsening as renewable penetration increases. Renewable advocates and decarbonization modelers attest the founding problem is solvable through battery storage, smart grids, and demand management without privileging dispatchability as the legitimacy gate. Recent grid operations data from regions with >50% renewables (parts of California, Texas, Denmark) show mixed stability outcomes — neither side's corroboration is uncontested. No neutral engineering authority has settled whether the problem is inherent to renewables or to inadequate storage/grid infrastructure investment.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__reliability_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__reliability_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__reliability_primacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(technology_legitimacy_kernel__reliability_primacy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_legitimacy_kernel__reliability_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_legitimacy_kernel__reliability_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_legitimacy_kernel__reliability_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.68 at interval end, rising from 0.54. The rise reflects two processes: (1) renewable penetration increases over the interval, making the storage-bundling requirement more economically burdensome (sunk costs of dispatchability augmentation grow); (2) the constraint's enforcement tightens as grid volatility concerns intensify, requiring stricter conformance to dispatchability metrics. Suppression is moderate (0.52) because renewable developers retain exit options (they can lobby for reframing, invest in storage, or diversify geographically), but the constraint's enforcement is active (interconnection delays, higher integration surcharges, dispatch priority subordination). Theater is rising (0.25→0.41) because an increasing share of grid-stability rhetoric is decoupled from measured outcomes: as battery storage and demand management increasingly provide the stability attributes dispatchability was supposed to ensure, the constraint increasingly justifies itself through narrative about 'proven reliability' rather than engineering necessity. The three-metric trajectory on one shared time grid documents both the extraction accumulation and the theatrical maintenance drift.
 *
 * PERSPECTIVAL GAP:
 *   From the grid operator's seat (agenda-setter, trapped institutional actor), the constraint solves a genuine coordination problem: ensuring stable frequency and voltage is a real engineering challenge, and dispatchability is a proven solution. They perceive the constraint as natural law (you cannot have a stable grid without dispatchable generation). From the renewable developer's seat (payer, constrained), the same constraint is enforced extraction: the grid has always been variable (load is variable), dispatchability is only one solution among several (storage, demand management, oversizing), and the constraint privileges one solution to advantage incumbent operators. From the ratepayer's seat (powerless, trapped), the constraint is pure cost-shifting: they bear higher electricity prices to subsidize the dispatchable-generation architecture, with no voice in whether that architecture is the right one. The engine's per-seat classification captures this perspectival geometry; the authored claim (tangled_rope) reflects the structural fact that both genuine coordination (grid stability) and asymmetric extraction (cost to ratepayers) coexist in the constraint's operation.
 *
 * DIRECTIONALITY LOGIC:
 *   Grid operators (powerful, institutional, trapped) are beneficiaries on the supply side — the constraint privileges their technical expertise and control of dispatch algorithms. They face low directionality (d ≈ 0.2), making their effective extraction negative or neutral in the engine's computation: they are subsidized by the constraint. Nuclear operators (institutional, mobile exit, beneficiary role) face d ≈ 0.3: they benefit substantially but retain options (can relocate, advocate for alternative framings, diversify generation mix). Renewable developers (organized, constrained exit, payer role) face d ≈ 0.7: they are trapped in the market (renewables are their business model) but must comply with dispatchability requirements, making them targets for the transfer. Ratepayers (powerless, trapped exit) face d ≈ 0.85: they cannot exit (electricity is essential), cannot renegotiate (individual consumers have no seat), and bear diffuse costs that manifest in higher bills. The directionality spread (0.2 to 0.85) is the seat divergence: the constraint's type varies sharply by seat. Grid operators compute it as beneficial coordination (rope). Ratepayers compute it as extraction without choice (snare). The engine surfaces this divergence through per-seat classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids misclassification as pure rope by explicitly declaring victims (ratepayers, renewable developers) and active enforcement (interconnection standards, dispatch subordination). The founding problem (grid instability during high-renewable-penetration periods) is live and contested — neither the grid-operator nor renewable-advocate corroboration is unchallenged by the other — which supports the tangled_rope classification. If the founding problem were dead (stability fully solved by storage and smart grids) but the constraint persisted anyway, the classification would drift toward piton (inertial enforcement of obsolete architecture). Currently, the problem remains contested enough that both the coordination and extraction narratives have some empirical support.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dispatchability_necessity,
    'Is dispatchability genuinely necessary for grid stability, or is it one solution among several sufficient alternatives (storage, demand management, oversizing, interconnection)?',
    'Grid operations data from regions with >60% renewable penetration (Denmark, Costa Rica, parts of California, Australia); comparison of stability metrics across grids with different dispatchability-to-storage ratios; engineering studies isolating the causal contribution of dispatchability to frequency stability.',
    'If dispatchability is necessary, the constraint reflects genuine coordination requirements and the tangled_rope classification holds. If it is one solution among several, the constraint becomes a choice to privilege one solution, raising the extraction reading and potentially supporting snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dispatchability_necessity, empirical, 'Whether dispatchability is a fundamental requirement or a contingent policy choice.').

omega_variable(
    foundational_problem_persistence,
    'Has the founding problem (grid instability during high-renewable periods) actually been solved by technological advances (battery cost reductions, smart grid deployment, demand management), or does it persist?',
    'Comparison of grid-stability metrics (frequency nadir, voltage excursion magnitude) in 2015 vs. 2030; capacity factor and response-time data for battery and demand-response systems deployed since 2020; grid operators'' own reliability assessments and historical dispatch logs.',
    'If solved, the founding problem is dead and the constraint becomes mandatrophic (persisting despite obsolescence), supporting piton classification. If persisting, the constraint remains justified as coordination, supporting tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foundational_problem_persistence, empirical, 'Whether the founding problem the constraint was built to solve remains live or has been substantially addressed.').

omega_variable(
    kernel_reading_foreclosure,
    'Do the three readings of the technology-legitimacy kernel logically foreclose one another, or do they coexist as live positions held by different institutional actors?',
    'Analysis of whether a single decision-maker (a regulator, grid operator, or technology investor) could hold two readings simultaneously without internal contradiction, or whether the readings mandate incompatible actions.',
    'If readings foreclose one another, the constraint is one reading of a fully-determined kernel whose alternatives are ruled out. If they coexist, the constraint is one reading among live competitors, and the kernel itself is under-determined.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether the three readings of the legitimacy kernel are mutually exclusive or coexistent.').

omega_variable(
    suppression_source,
    'Is the measured suppression (0.52) primarily structural (regulatory barriers, interconnection delays, technical standards) or internalized (renewable developers'' belief that dispatchability is genuinely necessary)?',
    'Post-policy-change observations: if a jurisdiction abandons the reliability-primacy criterion and renewable deployment accelerates, suppression was structural. If developers continue to invest in dispatchability augmentation even after the regulatory requirement is removed, suppression is partly internalized.',
    'If structural, the suppression decays when the constraint is removed. If internalized, the suppression persists, suggesting the constraint has embedded itself in how renewable developers think about their technology.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_source, empirical, 'Whether measured suppression is external enforcement or internalized belief.').

omega_variable(
    coal_operator_structural_capture,
    'Do coal and natural-gas operators benefit from the reliability-primacy frame even though they are excluded from climate-legitimacy narratives?',
    'Analysis of dispatch economics and utilization rates for fossil-fuel dispatchable plants before and after the reliability-primacy frame becomes regulatory standard; comparison of retirement rates across jurisdictions with different legitimacy criteria.',
    'If fossil operators benefit, the constraint serves an unstated second function (maintaining dispatchable fossil capacity) that contradicts its stated climate purpose. The extraction reading would increase.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coal_operator_structural_capture, empirical, 'Whether fossil-fuel operators indirectly benefit from a constraint that privileges dispatchability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__reliability_primacy_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(tech_tr_t0, observed).
narrative_ontology:measurement(tech_tr_t5, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement_basis(tech_tr_t5, observed).
narrative_ontology:measurement(tech_tr_t10, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(tech_tr_t10, observed).
narrative_ontology:measurement(tech_tr_t15, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(tech_tr_t15, observed).
narrative_ontology:measurement(tech_tr_t20, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(tech_tr_t20, observed).
narrative_ontology:measurement(tech_tr_t25, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(tech_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 0, 0.54).
narrative_ontology:measurement_basis(tech_be_t0, observed).
narrative_ontology:measurement(tech_be_t5, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 5, 0.59).
narrative_ontology:measurement_basis(tech_be_t5, observed).
narrative_ontology:measurement(tech_be_t10, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement_basis(tech_be_t10, observed).
narrative_ontology:measurement(tech_be_t15, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(tech_be_t15, observed).
narrative_ontology:measurement(tech_be_t20, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(tech_be_t20, observed).
narrative_ontology:measurement(tech_be_t25, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(tech_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(tech_su_t0, observed).
narrative_ontology:measurement(tech_su_t5, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 5, 0.44).
narrative_ontology:measurement_basis(tech_su_t5, observed).
narrative_ontology:measurement(tech_su_t10, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(tech_su_t10, observed).
narrative_ontology:measurement(tech_su_t15, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement_basis(tech_su_t15, observed).
narrative_ontology:measurement(tech_su_t20, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement_basis(tech_su_t20, observed).
narrative_ontology:measurement(tech_su_t25, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 25, 0.52).
narrative_ontology:measurement_basis(tech_su_t25, projected).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=25
narrative_ontology:measurement(tech_grid_01, technology_legitimacy_kernel__reliability_primacy_reading, accessibility_collapse(class), 0, 0.55).
narrative_ontology:measurement(tech_grid_02, technology_legitimacy_kernel__reliability_primacy_reading, accessibility_collapse(class), 25, 0.68).
narrative_ontology:measurement(tech_grid_03, technology_legitimacy_kernel__reliability_primacy_reading, accessibility_collapse(individual), 0, 0.4).
narrative_ontology:measurement(tech_grid_04, technology_legitimacy_kernel__reliability_primacy_reading, accessibility_collapse(individual), 25, 0.52).
narrative_ontology:measurement(tech_grid_05, technology_legitimacy_kernel__reliability_primacy_reading, accessibility_collapse(organizational), 0, 0.62).
narrative_ontology:measurement(tech_grid_06, technology_legitimacy_kernel__reliability_primacy_reading, accessibility_collapse(organizational), 25, 0.71).
narrative_ontology:measurement(tech_grid_07, technology_legitimacy_kernel__reliability_primacy_reading, accessibility_collapse(structural), 0, 0.68).
narrative_ontology:measurement(tech_grid_08, technology_legitimacy_kernel__reliability_primacy_reading, accessibility_collapse(structural), 25, 0.75).
narrative_ontology:measurement(tech_grid_09, technology_legitimacy_kernel__reliability_primacy_reading, resistance(class), 0, 0.55).
narrative_ontology:measurement(tech_grid_10, technology_legitimacy_kernel__reliability_primacy_reading, resistance(class), 25, 0.62).
narrative_ontology:measurement(tech_grid_11, technology_legitimacy_kernel__reliability_primacy_reading, resistance(individual), 0, 0.35).
narrative_ontology:measurement(tech_grid_12, technology_legitimacy_kernel__reliability_primacy_reading, resistance(individual), 25, 0.58).
narrative_ontology:measurement(tech_grid_13, technology_legitimacy_kernel__reliability_primacy_reading, resistance(organizational), 0, 0.62).
narrative_ontology:measurement(tech_grid_14, technology_legitimacy_kernel__reliability_primacy_reading, resistance(organizational), 25, 0.6).
narrative_ontology:measurement(tech_grid_15, technology_legitimacy_kernel__reliability_primacy_reading, resistance(structural), 0, 0.48).
narrative_ontology:measurement(tech_grid_16, technology_legitimacy_kernel__reliability_primacy_reading, resistance(structural), 25, 0.52).
narrative_ontology:measurement(tech_grid_17, technology_legitimacy_kernel__reliability_primacy_reading, stakes_inflation(class), 0, 0.48).
narrative_ontology:measurement(tech_grid_18, technology_legitimacy_kernel__reliability_primacy_reading, stakes_inflation(class), 25, 0.62).
narrative_ontology:measurement(tech_grid_19, technology_legitimacy_kernel__reliability_primacy_reading, stakes_inflation(individual), 0, 0.6).
narrative_ontology:measurement(tech_grid_20, technology_legitimacy_kernel__reliability_primacy_reading, stakes_inflation(individual), 25, 0.67).
narrative_ontology:measurement(tech_grid_21, technology_legitimacy_kernel__reliability_primacy_reading, stakes_inflation(organizational), 0, 0.52).
narrative_ontology:measurement(tech_grid_22, technology_legitimacy_kernel__reliability_primacy_reading, stakes_inflation(organizational), 25, 0.58).
narrative_ontology:measurement(tech_grid_23, technology_legitimacy_kernel__reliability_primacy_reading, stakes_inflation(structural), 0, 0.45).
narrative_ontology:measurement(tech_grid_24, technology_legitimacy_kernel__reliability_primacy_reading, stakes_inflation(structural), 25, 0.52).
narrative_ontology:measurement(tech_grid_25, technology_legitimacy_kernel__reliability_primacy_reading, suppression(class), 0, 0.38).
narrative_ontology:measurement(tech_grid_26, technology_legitimacy_kernel__reliability_primacy_reading, suppression(class), 25, 0.5).
narrative_ontology:measurement(tech_grid_27, technology_legitimacy_kernel__reliability_primacy_reading, suppression(individual), 0, 0.32).
narrative_ontology:measurement(tech_grid_28, technology_legitimacy_kernel__reliability_primacy_reading, suppression(individual), 25, 0.48).
narrative_ontology:measurement(tech_grid_29, technology_legitimacy_kernel__reliability_primacy_reading, suppression(organizational), 0, 0.45).
narrative_ontology:measurement(tech_grid_30, technology_legitimacy_kernel__reliability_primacy_reading, suppression(organizational), 25, 0.52).
narrative_ontology:measurement(tech_grid_31, technology_legitimacy_kernel__reliability_primacy_reading, suppression(structural), 0, 0.5).
narrative_ontology:measurement(tech_grid_32, technology_legitimacy_kernel__reliability_primacy_reading, suppression(structural), 25, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__reliability_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(technology_legitimacy_kernel__reliability_primacy_reading, 0.12).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel__velocity_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel__precautionary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested technology-legitimacy kernel. The kernel asks: what makes a climate technology 'legitimate' for mitigation support? This reading answers: dispatchable, baseload-capable generation ensuring grid stability. Sibling readings provide different answers (velocity deployment, precautionary reversibility) and therefore different ε values, beneficiary sets, and victim sets. Each reading is a separate constraint story with its own structural data. They are linked here because the kernel contest determines which reading applies in practice, and jurisdictions may adopt different readings for the same underlying technology.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technology_legitimacy_kernel__reliability_primacy_reading, organized, 0.72).
constraint_indexing:directionality_override(technology_legitimacy_kernel__reliability_primacy_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
