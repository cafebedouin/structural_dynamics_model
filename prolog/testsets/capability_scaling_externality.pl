% ============================================================================
% CONSTRAINT STORY: capability_scaling_externality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_capability_scaling_externality, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: capability_scaling_externality
 *   human_readable: Capability Scaling Externality
 *   domain: general/systemic_dynamics
 *
 * SUMMARY:
 *   Capability scaling externality describes the structural constraint
 *   created when competitive pressure drives agents toward efficiency scaling
 *   that increases systemic fragility. As agents scale operations to gain
 *   competitive advantage, they systematically reduce spare capacity,
 *   increase interdependence, and concentrate capability. The externality is
 *   that marginal agents and resilience mechanisms bear the costs of this
 *   scaling (displacement, vulnerability, obsolescence) while scaling
 *   beneficiaries capture the efficiency gains. The constraint exhibits a
 *   genuine coordination function — efficient scaling does meet demand and
 *   coordinate resource allocation — but the benefit distribution is
 *   asymmetric, and the extraction mechanism is the enforcement of scaling
 *   pressure through competitive competition. This is Tangled Rope: both
 *   coordination and extraction are structurally necessary; neither can be
 *   removed without destroying the system. The constraint operates across
 *   domains: financial markets scale toward systemic leverage and cascade
 *   risk, supply chains scale toward just-in-time fragility, labor markets
 *   scale toward precarity, infrastructure scales toward single-point
 *   failures. Each domain instantiates the same structural pattern with
 *   domain-specific metrics.
 *
 * KEY AGENTS:
 *   - Scaling Beneficiaries: Institutional actors (institutional/arbitrage) — large firms, market leaders, efficiency optimizers. Capture first-mover advantages and competitive gains from scaling. Experience extraction as coordination.
 *   - Marginal Agents: Powerless actors (powerless/trapped) — small-scale operators, specialists, mid-tier competitors. Pressured to scale or exit; cannot afford scaling investments; displaced by efficiency gains. Bears full cost of externality.
 *   - Capacity Stewards: Moderate power institutional actors (moderate/constrained) — regulators, industry bodies, state resource managers, systems engineers. Responsible for maintaining systemic resilience but face extraction through scaling incentives that undercut their authority.
 *   - Specialist Practitioners: Medium-scale operators (moderate/constrained) — domain experts, craftspeople, mid-size firms. Benefit from coordination infrastructure but extracted from through scaling pressure that dilutes expertise.
 *   - Resilience Mechanism: Institutional reserve (institutional/arbitrage) — spare capacity, redundancy, modularity, diversity. Recognized as necessary but treated as inefficiency during optimization phases. Atrophies through enforcement of scaling logic.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks treating competitive scaling as natural law rather than contingent institutional arrangement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(capability_scaling_externality, 0.58).
domain_priors:suppression_score(capability_scaling_externality, 0.48).
domain_priors:theater_ratio(capability_scaling_externality, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(capability_scaling_externality, extractiveness, 0.58).
narrative_ontology:constraint_metric(capability_scaling_externality, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(capability_scaling_externality, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(capability_scaling_externality, tangled_rope).
narrative_ontology:human_readable(capability_scaling_externality, "Capability Scaling Externality").
narrative_ontology:topic_domain(capability_scaling_externality, "general/systemic_dynamics").

domain_priors:requires_active_enforcement(capability_scaling_externality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(capability_scaling_externality, scaling_agents).
narrative_ontology:constraint_beneficiary(capability_scaling_externality, efficiency_optimizers).
narrative_ontology:constraint_victim(capability_scaling_externality, marginal_agents).
narrative_ontology:constraint_victim(capability_scaling_externality, systemic_resilience).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINAL AGENT (SNARE) — Cannot exit the efficiency race without ceasing to participate. Bears full cost of scaling externalities: displaced capacity, reduced option value, competitive pressure. Trapped both structurally (no resources to scale independently) and strategically (must match scaling pace or lose market position). Maximum extraction experienced.
constraint_indexing:constraint_classification(capability_scaling_externality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SCALING BENEFICIARY (ROPE) — Experiences the constraint as pure coordination: scaling to meet demand and compete on efficiency. Enjoys arbitrage options (can redirect capacity, outsource, pivot) and benefits from the coordination mechanism. Extraction runs toward this agent; they perceive the constraint as enabling their growth.
constraint_indexing:constraint_classification(capability_scaling_externality, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: CAPACITY STEWARD (TANGLED ROPE) — Moderate power actor (regulator, industry body, state resource manager) faces genuine coordination problem: must maintain systemic capacity slack to prevent cascade failures. But also extracted from: scaling incentives push toward full utilization, undercutting the steward's resource allocation authority. Benefits from coordination function; victim of asymmetric incentive structure.
constraint_indexing:constraint_classification(capability_scaling_externality, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SPECIALIST PRACTITIONER (TANGLED ROPE) — Medium-scale operator (craftsperson, domain expert, mid-size firm) benefits from coordination infrastructure but faces extraction through scaling pressure. Must invest in scaling capability to remain competitive even when it dilutes expertise quality. Constrained exit: can reduce scope but loses market access; cannot exit without abandoning practice.
constraint_indexing:constraint_classification(capability_scaling_externality, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: RESILIENCE MECHANISM (PITON) — Institutional redundancy and slack capacity are recognized as necessary for system stability but treated as theater by scaling logic. Buffer systems (spare capacity, diversity, modularity) persist through regulation and occasional crisis response but atrophy during efficiency periods. Theater ratio high: maintained through mandates and post-crisis reforms, not active function.
constraint_indexing:constraint_classification(capability_scaling_externality, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, scaling externalities appear as an immutable feature of competitive dynamics: agents that scale faster gain advantage, creating incentive pressure that cannot be resisted without losing position. The constraint looks like a law of economic physics. However, the structural data contradicts mountain classification — active enforcement, asymmetric extraction, and beneficiary/victim structure reveal this as a contingent institutional arrangement, not natural law.
constraint_indexing:constraint_classification(capability_scaling_externality, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(capability_scaling_externality_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(capability_scaling_externality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(capability_scaling_externality, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(capability_scaling_externality, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(capability_scaling_externality, TR),
    TR >= 0.70.

:- end_tests(capability_scaling_externality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The constraint extracts from marginal agents through displacement pressure, from capacity stewards through authority degradation, and from resilience mechanisms through efficiency enforcement. The extraction is not total because genuine coordination is happening — scaling does meet demand and allocates resources. But the asymmetry is significant: scaling beneficiaries gain market advantage while others bear systemic risk. The value increased from 0.35 to 0.58 over the measurement interval, reflecting competitive pressure intensification. Suppression (0.48): Moderate. Barriers to exit include competitive necessity (must scale or lose market position), capital requirements (scaling requires investment marginal agents cannot afford), and regulatory pressure (efficiency mandates increase suppression). But suppression is not total — some agents choose to exit rather than scale, and alternative niches exist for non-scalers. Theater ratio (0.35): Low-moderate. The coordination function (meeting demand, allocating resources) is genuine and substantial. Theater is relatively low because efficiency gains are real, not performative. Theater increased slightly from 0.28 to 0.35 as scaling pressure became more enforced through policy and less driven by organic demand.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces divergent classifications because structural position determines experienced extractiveness. Scaling beneficiaries see a rope (coordination with benefits), but marginal agents see a snare (extraction with no exit). Capacity stewards see tangled rope (coordination function but extraction pressure), while specialist practitioners see tangled rope from a different angle (benefit from infrastructure but extracted through scaling demand). The analytical observer risks seeing mountain (scaling is natural economic law) but the structural evidence contradicts this: active enforcement, asymmetric extraction, institutional beneficiaries, and institutional victims reveal contingency. The perspectival gap is the diagnostic signature that this is not natural law but institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries with arbitrage options (scaling_agents, efficiency_optimizers) derive low d values through the composition of beneficiary status + arbitrage exit → derived d ≈ 0.15-0.25 → f(d) ≈ 0.0 to 0.15 → negative or minimal χ. They experience the constraint as enabling (rope perspective). Victims with trapped or constrained exit (marginal_agents, systemic_resilience) derive high d values through victim status + trapped/constrained exit → derived d ≈ 0.85-0.95 → f(d) ≈ 1.15-1.42 → high χ. They experience the constraint as extractive (snare/tangled rope perspectives). Moderate power agents (capacity stewards, specialist practitioners) occupy the middle: moderate power + constrained exit + mixed beneficiary/victim status → derived d ≈ 0.50-0.65 → f(d) ≈ 0.65-1.0 → moderate χ. The directionality derivation reveals the structural asymmetry: the same institutional arrangement produces near-zero extraction for beneficiaries and maximum extraction for victims. No overrides needed; the structural data drives the perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy through structural decomposition. The question 'Is scaling coordination or extraction?' is answered by: it is both, and the answer depends on structural position. For beneficiaries, it coordinates resource allocation and meets demand. For victims, it extracts through displacement and precarity. For stewards, it creates a coordination/extraction hybrid: they must maintain resilience (coordination function) but face suppression from scaling incentives (extraction). The mandatrophy is not resolved by picking one type — it is resolved by showing that the tangled rope classification correctly captures the hybrid nature. The mountain perspective (scaling as natural law) is a false summit that naturalizes institutional choices (competitive pressure, efficiency enforcement, capital concentration) into inevitability. The engine's false summit detector should flag this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    slack_utility_ambiguity,
    'Is capacity slack a coordination good (stabilizing the system) or pure waste (inefficiency to be eliminated)?',
    'Historical analysis of system cascades vs optimization gains; correlation between slack reduction and cascade frequency; cost-benefit analysis across economic cycles',
    'If slack is coordination good: suppression value should be higher (agents actively suppress it for efficiency gains). If pure waste: constraint may reclassify as pure extraction (snare) with no genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slack_utility_ambiguity, preference, 'Whether slack capacity provides systemic coordination value or pure inefficiency').

omega_variable(
    externality_boundedness,
    'Are scaling externalities bounded (self-limiting at some efficiency threshold) or unbounded (extraction increases monotonically with scaling pressure)?',
    'Empirical measurement across domains: financial markets, supply chains, infrastructure systems, labor markets. Identify any natural saturation points or cascade thresholds.',
    'If bounded: constraint may degrade to Rope as agents discover efficiency limits. If unbounded: suppression and extraction increase over time, constraint may upgrade to Snare-dominated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(externality_boundedness, empirical, 'Whether scaling externalities self-limit or compound monotonically').

omega_variable(
    coordination_function_degradation,
    'Does the constraint lose genuine coordination function as it scales, becoming pure rent extraction disguised as efficiency?',
    'Measurement of beneficiary gains vs victim losses; analysis of whether coordination is still being provided or merely incentivized through extraction pressure',
    'If degradation occurs: theater ratio increases, constraint may upgrade to Piton. If function persists: tangled rope classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_degradation, empirical, 'Whether coordination function persists or is replaced by extraction mechanism').

omega_variable(
    capability_asymmetry_source,
    'Is the capability scaling gap driven by technological advantage (legitimate), regulatory capture (extractive), or information asymmetry (contingent)?',
    'Analysis of which agents can scale and why; comparison of scaling costs across agent types; identification of barriers that are natural vs institutional',
    'If technological: gap is inevitable, extraction pressure justified. If regulatory/information: gap is contingent, suppression is artificial, reclassify to higher extraction/snare. If mixed: decompose into separate constraint stories per barrier type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capability_asymmetry_source, empirical, 'Source of capability scaling advantage').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(capability_scaling_externality, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(capscale_tr_t0, capability_scaling_externality, theater_ratio, 0, 0.28).
narrative_ontology:measurement(capscale_tr_t3, capability_scaling_externality, theater_ratio, 3, 0.32).
narrative_ontology:measurement(capscale_tr_t6, capability_scaling_externality, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(capscale_be_t0, capability_scaling_externality, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(capscale_be_t3, capability_scaling_externality, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(capscale_be_t6, capability_scaling_externality, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(capability_scaling_externality, resource_allocation).
narrative_ontology:boltzmann_floor_override(capability_scaling_externality, 0.12).
narrative_ontology:affects_constraint(capability_scaling_externality, financial_systemic_leverage).
narrative_ontology:affects_constraint(capability_scaling_externality, supply_chain_fragility).
narrative_ontology:affects_constraint(capability_scaling_externality, labor_market_precarity).
narrative_ontology:affects_constraint(capability_scaling_externality, infrastructure_cascade_risk).

% DUAL FORMULATION NOTE:
% Capability scaling externality is a high-level structural constraint. Downstream constraints (financial leverage, supply chain fragility, labor precarity, infrastructure risk) are domain-specific instantiations of the same scaling externality pattern. Each downstream constraint has its own ε value reflecting domain-specific metrics, but all share the underlying mechanism: efficiency scaling creates systemic fragility that is borne by marginal actors. The upstream constraint captures the general pattern; downstream constraints capture domain instantiation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
