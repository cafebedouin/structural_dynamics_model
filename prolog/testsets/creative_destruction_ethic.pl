% ============================================================================
% CONSTRAINT STORY: creative_destruction_ethic
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_creative_destruction_ethic, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: creative_destruction_ethic
 *   human_readable: Creative Destruction Ethic: The Normative Cover for Unequal Exit Costs
 *   domain: political_economy/institutional_legitimation
 *
 * SUMMARY:
 *   The creative destruction ethic is a doctrine that legitimizes the
 *   displacement of workers, communities, and incumbent firms as necessary
 *   and efficient components of economic progress. Originating in
 *   Schumpeterian economics, the doctrine has become the primary
 *   justification for deregulation, deindustrialization, and financialization
 *   across wealthy economies. The constraint exhibits a fundamental
 *   asymmetry: those who benefit from destruction (capital holders,
 *   disruptors, financiers) experience it as coordination and efficiency;
 *   those who bear costs (displaced workers, local communities, incumbent
 *   firms without capital mobility) experience it as extraction with no voice
 *   in the decision. The ethic functions as a legitimation mechanism that
 *   naturalizes this asymmetry as inevitable and beneficial, suppressing
 *   recognition of alternative distributions of costs. The theater ratio has
 *   increased over the 90-year measurement interval: early creative
 *   destruction (19th-century industrial turnover) involved genuine
 *   competitive replacement; modern creative destruction increasingly
 *   involves financial engineering, regulatory capture, and cost
 *   externalization with minimal verification that market-clearing efficiency
 *   is actually achieved.
 *
 * KEY AGENTS:
 *   - Displaced Workers: Primary victims (powerless/trapped) — bear permanent loss of human capital, career, and identity without compensation or exit option
 *   - Local Communities: Primary victims (powerless/trapped at generational scale) — experience irreversible loss of infrastructure, institutions, and social bonds
 *   - Incumbent Firms: Secondary victims (moderate/constrained) — face market competition and policy-enabled destruction; some have resources to exit but at high cost
 *   - Capital Holders & Disruptors: Primary beneficiaries (organized/mobile) — profit from asset liquidation, labor cost reduction, and emergence of new investment opportunities
 *   - Capital Markets: Primary beneficiary (institutional/arbitrage) — coordinate reallocation of capital at low cost and maximum mobility
 *   - Policy Apparatus: Institutional enforcer (institutional/arbitrage) — maintains the doctrine through deregulation, merger approval, and antitrust laxity; theater increasingly high despite degraded verification
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — identifies the constraint as tangled rope where coordination function is real but highly asymmetric
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(creative_destruction_ethic, 0.58).
domain_priors:suppression_score(creative_destruction_ethic, 0.68).
domain_priors:theater_ratio(creative_destruction_ethic, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(creative_destruction_ethic, extractiveness, 0.58).
narrative_ontology:constraint_metric(creative_destruction_ethic, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(creative_destruction_ethic, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(creative_destruction_ethic, tangled_rope).
narrative_ontology:human_readable(creative_destruction_ethic, "Creative Destruction Ethic: The Normative Cover for Unequal Exit Costs").
narrative_ontology:topic_domain(creative_destruction_ethic, "political_economy/institutional_legitimation").

domain_priors:requires_active_enforcement(creative_destruction_ethic).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(creative_destruction_ethic, incumbent_disruptors).
narrative_ontology:constraint_beneficiary(creative_destruction_ethic, capital_holders).
narrative_ontology:constraint_victim(creative_destruction_ethic, displaced_workers).
narrative_ontology:constraint_victim(creative_destruction_ethic, local_communities).
narrative_ontology:constraint_victim(creative_destruction_ethic, incumbent_firms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED WORKER (SNARE) — Structurally trapped. Creative destruction destroys the worker's human capital, industry-specific knowledge, and career trajectory. Retraining programs are inadequate, geographic relocation is costly, pension loss is permanent. The worker bears full cost of the 'creative' process with no exit option and no benefit from future growth. Maximum experienced extraction — the worker cannot arbitrage, cannot leave the constraint's jurisdiction, and cannot opt out of the destruction of their livelihood.
constraint_indexing:constraint_classification(creative_destruction_ethic, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOCAL COMMUNITY (SNARE) — At generational timescale, creative destruction destroys community infrastructure, social capital, and place-based identity. Schools, hospitals, civic institutions collapse when the industry anchoring them disappears. The community cannot exit — relocation is not feasible for embedded institutions. The destruction is irreversible over a single generation. The promise of future growth accrues to capital holders elsewhere, not to the community bearing the cost of destruction.
constraint_indexing:constraint_classification(creative_destruction_ethic, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT FIRM (TANGLED ROPE) — Faces genuine coordination problem (must adapt or exit) and genuine extraction (creative destruction doctrine legitimizes displacement of established businesses without compensation). The firm has some exit options (diversification, acquisition, relocation) but at significant cost and uncertainty. Suppression is high (market discipline enforces the destruction) but not absolute — some incumbent firms survive through coordination.
constraint_indexing:constraint_classification(creative_destruction_ethic, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: COALITION OF DISRUPTORS (ROPE) — Organized actors (venture capital, tech firms, management consultants) see creative destruction as coordination: destroying inefficient incumbents enables new market entrants and resource reallocation. The doctrine legitimizes their competitive strategy and provides intellectual cover for extraction disguised as efficiency. They have high mobility (can move capital, talent, operations across jurisdictions) and benefit from the destruction of competitors. Low suppression from their perspective — they experience the constraint as enabling rather than constraining.
constraint_indexing:constraint_classification(creative_destruction_ethic, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CAPITAL MARKETS (ROPE) — The financial system benefits from creative destruction through reallocation of capital, liquidation of assets at distressed prices, and emergence of new investment opportunities. No suppression — the market sees destruction as efficient price discovery and resource optimization. Maximum arbitrage: capital can exit one sector and enter another instantly. The constraint appears as pure coordination (optimal allocation) from this perspective.
constraint_indexing:constraint_classification(creative_destruction_ethic, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: POLICY APPARATUS (PITON) — State institutions maintain the creative destruction doctrine as policy orthodoxy (deregulation, merger approval, antitrust laxity, anti-labor legislation) while the actual function — efficient market clearing — has degraded. The doctrine is maintained through institutional inertia: policy elites inherited the Schumpeterian framework and lack incentive to revise it. Theater is high: policymakers perform commitment to 'creative destruction' while actual implementation is selective (protecting incumbent sectors with political power, destroying others). The constraint persists despite low verification that it achieves stated efficiency goals.
constraint_indexing:constraint_classification(creative_destruction_ethic, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — The doctrine of creative destruction coordinates genuine market evolution (new technologies, new firm models) with genuine extraction (costs borne unequally across power levels, with worst costs concentrated on powerless agents). The constraint is neither pure coordination nor pure extraction — it is a hybrid where the coordination function (reallocation efficiency) is real but asymmetric in who benefits and who pays. The ethic naturalizes the asymmetry as inevitable and optimal, suppressing recognition that the same efficiency could be achieved with more equitable cost distribution.
constraint_indexing:constraint_classification(creative_destruction_ethic, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(creative_destruction_ethic_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(creative_destruction_ethic, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(creative_destruction_ethic, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(creative_destruction_ethic, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(creative_destruction_ethic, TR),
    TR >= 0.70.

:- end_tests(creative_destruction_ethic_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts from displaced workers and communities (they bear permanent costs) while benefiting capital holders and disruptors (they capture gains from reallocation). The extraction is not absolute (some workers find new employment, some communities stabilize, some incumbent firms survive) but systematic and unequal. The measurement shows increasing trend from 0.35 to 0.61 over the interval, reflecting accumulation of extraction as the doctrine has been applied with less attention to transition mechanisms. Suppression (0.68): High. Multiple barriers prevent alternatives: displaced workers cannot easily relocate or retrain; communities cannot exit their geographic location; incumbent firms face regulatory and competitive barriers to survival; policy discourse is dominated by the doctrine with limited space for alternative frameworks. Retraining programs are underfunded; geographic arbitrage is blocked by housing costs; political voice of displaced groups is suppressed. Theater ratio (0.65): Moderate-high and increasing. The doctrine performs commitment to 'efficiency' and 'progress' while implementation is selective (protecting politically connected sectors, destroying others) and verification of actual efficiency gains is weak. Policy elites cite Schumpeter as incantation rather than analysis. Community loss is presented as inevitable rather than political choice. The theater increased from 0.40 to 0.68 over the interval as the doctrine became more entrenched as policy orthodoxy despite weaker empirical grounding.
 *
 * PERSPECTIVAL GAP:
 *   Maximum gap between powerless and organized perspectives. Displaced workers experience snare (extraction without coordination benefit); disruptors experience rope (coordination without perceived extraction). The gap reveals that the constraint is not a natural law but a political arrangement where different agents experience it fundamentally differently. The piton perspective reveals that the policy apparatus maintains the doctrine through ritual despite degraded verification — theater rising while empirical support remains contested. The analytical perspective identifies tangled rope: real coordination function (reallocation can improve efficiency) combined with real asymmetric extraction (benefits concentrated on capital holders). The mandate-breaking element is the false summit (mountain perspective) — the idea that creative destruction is inevitable law of markets rather than contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position in the extraction flow. Displaced workers have d ≈ 0.95 (full target, no exit, no benefit) — maximum exposure to high f(d). Capital holders have d ≈ 0.10 (beneficiary, high-mobility exit) — low exposure to extraction. Incumbent firms have d ≈ 0.65 (mixed exposure depending on capital availability). The policy apparatus derives d ≈ 0.15 (beneficiary through institutional alignment, arbitrage exit via sector switching). The analytical observer has d ≈ 0.70 (neutral position but structural distance from decisions). The suppression value (0.68) is unscaled — it applies identically across all contexts because the barriers to exit (labor immobility, geographic entrenchment, capital requirements) are structural properties of the constraint, not observer-relative.
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL RESOLUTION: The doctrine of creative destruction resolves the mandatrophy by revealing that the same structural phenomenon appears as pure extraction to powerless agents (snare), pure coordination to beneficiaries (rope), and hybrid tangled rope to the analytical observer. The mandate-breaking element is the *naturalizing framing* — treating a contingent institutional arrangement (capital holders can exit, workers cannot) as an inevitable law of economics. The mountain perspective (creative destruction as natural law) is a false summit that the structural data contradicts. The actual constraint is tangled rope: the doctrine coordinates real market processes (reallocation of resources) while enforcing highly asymmetric extraction (costs on powerless agents, benefits on capital holders). The policy apparatus (piton) maintains the doctrine through increasingly performative commitment despite weak verification that the efficiency gains exceed the transaction costs borne by displaced agents. The suppression mechanism (barriers to exit and voice for displaced agents) is political, not natural — the constraint could be restructured to decouple coordination benefits from asymmetric cost distribution (e.g., universal transition income, community reinvestment requirements). The rising theater ratio (0.40 → 0.68) indicates degradation: the doctrine is invoked with less analytical support and more ritual as the gaps between claimed efficiency and actual outcomes widen.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficiency_claim_empirical_status,
    'Does creative destruction actually produce net efficiency gains, or does it generate transaction costs (retraining, geographic dislocation, social infrastructure loss) that exceed the claimed dynamic efficiency benefits?',
    'Longitudinal analysis of output growth, productivity growth, and total welfare (including non-market goods) in regions undergoing creative destruction vs control regions; comparison of gross innovation rates to net welfare changes; accounting for externalized costs (community collapse, health outcomes, crime, family dissolution)',
    'If efficiency gains exceed transaction costs: tangled_rope classification affirmed — genuine coordination function exists. If transaction costs exceed efficiency gains: classification shifts to snare — the ''efficiency'' framing is a cover story for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficiency_claim_empirical_status, empirical, 'Whether creative destruction produces claimed efficiency gains').

omega_variable(
    alternative_distribution_feasibility,
    'Could the same efficiency gains be achieved while distributing costs more equitably (e.g., universal basic transition income, community reinvestment requirements, portable benefits)?',
    'Analysis of policy designs that decouple creative destruction from unequal cost distribution; comparison of outcomes in jurisdictions with mandatory transition support vs laissez-faire creative destruction; modeling of welfare-neutral alternatives',
    'If equitable alternatives are structurally feasible: suppression mechanism is revealed as institutional choice, not necessity — classification upgrades from tangled_rope to snare (pure extraction under a coordination cover). If equitable distribution is impossible: suppression is structural, and tangled_rope stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_distribution_feasibility, empirical, 'Whether equitable cost distribution is compatible with efficiency gains').

omega_variable(
    identity_lock_in_policy_elites,
    'Is elite commitment to creative destruction ethic based on reasoned belief in its efficiency, or is it an identity-locked commitment where questioning the doctrine means abandoning professional standing and ideological tribe?',
    'Analysis of policy elite discourse patterns (how much critical examination vs ritual citation); documentation of career paths of reformers who challenged the doctrine; survey research on whether policy actors can articulate alternatives or default to citation-chain invocation of Schumpeter',
    'If identity-locked: piton classification is correct — the constraint persists through inertia and status signaling despite low verification of function. If reasoned belief: piton downgrades to rope — the constraint is genuinely coordinating despite asymmetric costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_policy_elites, empirical, 'Whether elite commitment to doctrine is reasoned or identity-locked').

omega_variable(
    selective_destruction_mechanism,
    'Is creative destruction applied uniformly across sectors and power levels, or does political power determine which incumbents are destroyed and which are protected?',
    'Comparative analysis of destruction rates across sectors (financial sector survived 2008 with bailouts while manufacturing jobs were destroyed by outsourcing; pharma patents protected while local retail destroyed); documentation of policy exceptions and carve-outs for politically connected incumbent firms vs elimination of non-connected competitors',
    'If applied uniformly: doctrine is coherent philosophy with asymmetric outcomes. If selective: doctrine is legitimation mythology — the actual constraint is ''powerful actors consolidate while powerless are displaced'' and the ''creative destruction'' framing naturalizes political capture.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(selective_destruction_mechanism, empirical, 'Whether creative destruction is applied uniformly or selectively by power').

omega_variable(
    temporal_mismatch_in_distributed_benefits,
    'Does the displaced worker/community ever receive the promised future growth benefits, or is the destruction permanent while growth accrues elsewhere?',
    'Longitudinal tracking of economic outcomes in post-destruction communities 10, 20, 30+ years after major industry collapse; comparison of growth rates in communities that experienced creative destruction vs matched controls; analysis of whether displaced workers'' descendants capture new opportunities or remain in lower-income status',
    'If benefits eventually reach displaced agents: tangled_rope stands — costs are front-loaded but compensation is eventual. If benefits remain concentrated in capital-holding classes and geographic centers: suppression is permanent and the constraint is snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(temporal_mismatch_in_distributed_benefits, empirical, 'Whether post-destruction growth benefits reach displaced agents').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(creative_destruction_ethic, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(crdes_tr_t0, creative_destruction_ethic, theater_ratio, 0, 0.4).
narrative_ontology:measurement(crdes_tr_t30, creative_destruction_ethic, theater_ratio, 30, 0.62).
narrative_ontology:measurement(crdes_tr_t60, creative_destruction_ethic, theater_ratio, 60, 0.65).
narrative_ontology:measurement(crdes_tr_t90, creative_destruction_ethic, theater_ratio, 90, 0.68).

% Extraction over time
narrative_ontology:measurement(crdes_be_t0, creative_destruction_ethic, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(crdes_be_t30, creative_destruction_ethic, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(crdes_be_t60, creative_destruction_ethic, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(crdes_be_t90, creative_destruction_ethic, base_extractiveness, 90, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(creative_destruction_ethic, resource_allocation).
narrative_ontology:boltzmann_floor_override(creative_destruction_ethic, 0.22).
narrative_ontology:affects_constraint(creative_destruction_ethic, labor_market_deinstitutionalization).
narrative_ontology:affects_constraint(creative_destruction_ethic, regional_inequality_acceleration).
narrative_ontology:affects_constraint(creative_destruction_ethic, capital_concentration_feedback).

% DUAL FORMULATION NOTE:
% Creative destruction ethic is upstream of labor market and regional inequality constraints — the doctrine justifies and legitimizes the mechanisms that produce deinstitutionalization and inequality. Decomposition note: the doctrine itself (this story) is analytically distinct from its mechanisms of implementation (labor deregulation, antitrust laxity, finance-enabled asset stripping, outsourcing). This story focuses on the ethic as legitimating frame; downstream stories detail the specific institutional arrangements that operationalize the doctrine.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(creative_destruction_ethic, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
