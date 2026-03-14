% ============================================================================
% CONSTRAINT STORY: resource_monopoly_pricing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_resource_monopoly_pricing, []).

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
 *   constraint_id: resource_monopoly_pricing
 *   human_readable: Resource Monopoly Pricing Constraint
 *   domain: economic/industrial
 *
 * SUMMARY:
 *   Resource monopoly pricing occurs when a single actor controls supply of a
 *   critical input with limited substitutes and faces dependent customers
 *   unable to exit the market. The constraint exhibits high extractiveness
 *   (0.58) because the monopoly holder captures economic surplus through
 *   pricing above marginal cost, and high suppression (0.65) because
 *   dependent actors lack meaningful alternatives. The theater ratio (0.35)
 *   is moderate — monopoly pricing operates openly through market mechanisms,
 *   not through performative rituals, but the market rhetoric of 'efficient
 *   resource allocation' obscures the extraction mechanism. The constraint
 *   manifests differently across perspectives: the monopoly holder
 *   experiences it as legitimate coordination (Rope), regulatory coalitions
 *   see it as mixed extraction and coordination (Tangled Rope), dependent
 *   consumers see it as pure extraction with no exit (Snare). The measurement
 *   trajectory shows extractiveness increasing over the interval from 0.42 to
 *   0.58, indicating accumulating rent-seeking behavior or market power
 *   concentration.
 *
 * KEY AGENTS:
 *   - Monopoly Holder: Primary beneficiary (institutional/arbitrage) — captures economic surplus through pricing power; experiences constraint as coordination mechanism
 *   - Dependent Consumers: Primary victim (powerless/trapped) — price-takers with no viable alternatives; bear full extraction cost
 *   - Downstream Producers: Secondary victim (moderate/constrained) — face elevated input costs; can exit at significant cost; also benefit from supply reliability
 *   - Regulatory Coalition: Organized agent (organized/mobile) — can enforce antitrust or price regulation but faces political constraints and enforcement costs
 *   - Market Efficiency: Institutional principle (institutional/arbitrage) — degraded through monopoly distortion; persists as normative standard
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees structure independent of framing; classifies as Snare due to power asymmetry and lack of coordination benefit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(resource_monopoly_pricing, 0.58).
domain_priors:suppression_score(resource_monopoly_pricing, 0.65).
domain_priors:theater_ratio(resource_monopoly_pricing, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(resource_monopoly_pricing, extractiveness, 0.58).
narrative_ontology:constraint_metric(resource_monopoly_pricing, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(resource_monopoly_pricing, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(resource_monopoly_pricing, snare).
narrative_ontology:human_readable(resource_monopoly_pricing, "Resource Monopoly Pricing Constraint").
narrative_ontology:topic_domain(resource_monopoly_pricing, "economic/industrial").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(resource_monopoly_pricing, monopoly_holder).
narrative_ontology:constraint_victim(resource_monopoly_pricing, dependent_consumers).
narrative_ontology:constraint_victim(resource_monopoly_pricing, downstream_producers).
narrative_ontology:constraint_victim(resource_monopoly_pricing, market_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT CONSUMER (SNARE) — Faces no meaningful exit options. Alternative sources for the resource are unavailable, prohibitively expensive, or technically incompatible with existing infrastructure. Bears full extraction cost with no negotiating power. Suppression mechanisms include switching costs, network effects, and technical lock-in.
constraint_indexing:constraint_classification(resource_monopoly_pricing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DOWNSTREAM PRODUCER (TANGLED ROPE) — Bears elevated input costs but also benefits from the monopoly holder's infrastructure investment and coordination. Constrained exit: can substitute materials or relocate production at significant cost. Experiences both extraction (high input prices) and coordination (reliable supply, standardized quality).
constraint_indexing:constraint_classification(resource_monopoly_pricing, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MONOPOLY HOLDER (ROPE) — Experiences the constraint as coordination: maintaining infrastructure, enforcing contracts, managing supply. Net beneficiary with full arbitrage options (can price-discriminate, expand markets, or exit into related sectors). Perceived extraction is minimal from their perspective; they see legitimate return on capital and coordination effort.
constraint_indexing:constraint_classification(resource_monopoly_pricing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY COALITION (TANGLED ROPE) — Organized agents (antitrust authorities, industry associations, consumer groups) see the monopoly as both a coordination mechanism (standardized pricing, supply reliability) and extractive overhead (deadweight loss, innovation suppression). Mobile exit options (antitrust enforcement, price regulation, alternative supply chains) but enforcement is resource-intensive and politically contested. The constraint persists because benefits to some constituencies (monopoly holder, incumbent suppliers) outweigh costs.
constraint_indexing:constraint_classification(resource_monopoly_pricing, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: MARKET EFFICIENCY (PITON) — The abstract principle that competitive markets allocate resources efficiently appears degraded here: monopoly pricing violates the ideal, yet the principle persists as a normative standard in economic discourse. Theater ratio high (0.35): economists regularly invoke market efficiency as justification for price-taking behavior while monopoly distortions persist. The principle survives through rhetorical maintenance despite functional atrophy.
constraint_indexing:constraint_classification(resource_monopoly_pricing, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — Classifies monopoly pricing as pure extraction when analyzed across all relevant time horizons and scopes. The constraint persists because the monopoly holder has structural power (control of critical resource) combined with suppression mechanisms (high switching costs, network effects, technical barriers) that prevent meaningful exit for dependent agents. No coordination function justifies the pricing premium; the monopoly is sustained purely through power asymmetry and dependence.
constraint_indexing:constraint_classification(resource_monopoly_pricing, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(resource_monopoly_pricing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(resource_monopoly_pricing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(resource_monopoly_pricing, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(resource_monopoly_pricing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(resource_monopoly_pricing, TR),
    TR >= 0.70.

:- end_tests(resource_monopoly_pricing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High, reflecting monopoly pricing premium above marginal cost. The value is not at maximum (≥0.66 Snare threshold) because some dependent actors have constrained exit options (can relocate, substitute materials, or coordinate purchases) that create pressure on the monopoly holder. If substitutes were literally unavailable, ε would reach 0.70+. Suppression (0.65): Moderate-high, reflecting multiple barriers to exit — switching costs (technical retraining, infrastructure adaptation), network effects (compatible with existing systems), information asymmetry (dependent actors unaware of alternatives), and contractual lock-in (long-term supply agreements). These are structural, not performative. Theater Ratio (0.35): Low-moderate. Monopoly pricing operates through transparent market mechanisms (posted prices, published tariffs) rather than theatrical performance, but economic discourse around 'market efficiency' and 'competitive pricing' provides a legitimizing narrative that obscures extraction.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. The monopoly holder (Rope) perceives legitimate return on capital and supply coordination. Regulatory coalitions (Tangled Rope) perceive mixed effects — extraction to consumers, but coordination benefits and efficiency risk. Dependent consumers (Snare) perceive pure coercive extraction with no exit. The analytical observer (Snare) agrees with the powerless perspective but for structural reasons: the monopoly holder's 'coordination benefit' is actually unilateral power, not genuine coordination. No beneficiary-victim pair experiences symmetric costs/benefits; the constraint is not a coordination mechanism but a power transfer mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   The monopoly holder benefits directly from the constraint: they receive above-normal returns and arbitrage mobility across markets. Directionality d is low (≈0.10-0.20), producing negative or minimal f(d), so their experienced extractiveness χ approaches zero or negative — they perceive coordination, not extraction. Dependent consumers are pure targets: they derive no benefit from monopoly pricing and have trapped exit options. Directionality d is high (≈0.85-0.95), producing f(d) ≈ 1.15, amplifying their experienced extractiveness to maximum. Downstream producers occupy middle ground: they suffer input cost burden (d ≈ 0.60-0.70) but also benefit from reliable supply coordination (partially offsetting d). Regulatory agents have mobile exit options and organized power (d ≈ 0.45-0.55), experiencing moderate extractiveness but with agency to challenge it.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by rejecting the false coordination frame. Some analyses claim monopolies coordinate supply and justify elevated pricing as necessary for capital recovery and reliability. This framing collapses under directionality analysis: if genuine coordination existed, we would see symmetry (both parties benefit) or at least beneficiary investment in victim welfare. Instead, we observe: monopoly holder maximizes extraction, dependent consumers are forced-takers at whatever price, and downstream producers extract partial rent from the monopoly holder's power without passing benefit downward. The constraint is not 'extractive coordination' (tangled rope); it is pure extraction with a coordination cover story (snare). The mandatrophy is resolved by distinguishing the monopoly holder's experienced coordination from the actual structural asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_monopoly_vs_artificial_barrier,
    'Is the monopoly''s pricing power derived from natural economies of scale or from artificial barriers to entry?',
    'Comparative cost analysis: single supplier vs hypothetical competitive suppliers; historical data on entry attempts; technical assessment of capital requirements and economies of scale',
    'If natural monopoly: extractiveness justified by coordination cost (ε ≤ 0.35). If artificial barriers: extractiveness is pure rent-seeking (ε ≥ 0.65). Classification shifts from Tangled Rope to Snare accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_monopoly_vs_artificial_barrier, empirical, 'Natural monopoly vs artificial barriers to entry').

omega_variable(
    substitute_resource_existence,
    'Do viable substitutes exist at comparable functionality and cost, and why are dependent agents not adopting them?',
    'Market survey of alternatives; cost-benefit analysis of switching for each agent class; identification of technical, contractual, or information barriers preventing adoption',
    'If substitutes available and accessible: suppression mechanism is behavioral/informational (ε reduced, reclassify as Rope). If substitutes unavailable or inaccessible: suppression is structural (ε unchanged, Snare confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substitute_resource_existence, empirical, 'Availability and accessibility of substitute resources').

omega_variable(
    regulatory_capture_mechanism,
    'Has the monopoly holder captured regulatory institutions that would otherwise enforce price controls or permit new entry?',
    'Analysis of regulatory decisions, lobbying expenditure, revolving-door employment patterns, and policy outcomes favorable to monopoly holder',
    'If regulatory capture confirmed: suppression is actively enforced (not merely structural). Reclassify from Snare to Tangled Rope if coordination benefits exist, or maintain Snare with directed suppression vector.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_mechanism, empirical, 'Regulatory capture by monopoly holder').

omega_variable(
    deadweight_loss_magnitude,
    'What is the magnitude of deadweight loss (unrealized trades due to monopoly pricing) relative to total market surplus?',
    'Econometric estimation of demand elasticity; calculation of consumer surplus lost to monopoly pricing; comparison with competitive benchmark',
    'If DWL > 20% of market surplus: extraction is severe (ε remains ≥ 0.58). If DWL < 5%: constraint may be reclassified as Rope (low-extraction coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deadweight_loss_magnitude, empirical, 'Deadweight loss from monopoly pricing relative to competitive benchmark').

omega_variable(
    innovation_suppression_mechanism,
    'Does monopoly pricing suppress innovation in competing technologies or substitutes by reducing downstream producer R&D investment?',
    'Longitudinal R&D expenditure analysis in monopolized vs competitive sectors; patent filing trends; comparison with alternative pricing regimes',
    'If innovation suppression confirmed: extractiveness includes long-term technological opportunity cost (ε increases to ≥ 0.70). If no suppression detected: extractiveness is allocative only (ε decreases to ≤ 0.50).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(innovation_suppression_mechanism, empirical, 'Innovation suppression through monopoly pricing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(resource_monopoly_pricing, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rmp_tr_t0, resource_monopoly_pricing, theater_ratio, 0, 0.28).
narrative_ontology:measurement(rmp_tr_t2, resource_monopoly_pricing, theater_ratio, 2, 0.32).
narrative_ontology:measurement(rmp_tr_t5, resource_monopoly_pricing, theater_ratio, 5, 0.35).

% Extraction over time
narrative_ontology:measurement(rmp_be_t0, resource_monopoly_pricing, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(rmp_be_t2, resource_monopoly_pricing, base_extractiveness, 2, 0.5).
narrative_ontology:measurement(rmp_be_t5, resource_monopoly_pricing, base_extractiveness, 5, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(resource_monopoly_pricing, resource_allocation).
narrative_ontology:affects_constraint(resource_monopoly_pricing, market_concentration_threshold).
narrative_ontology:affects_constraint(resource_monopoly_pricing, supply_chain_dependency).
narrative_ontology:affects_constraint(resource_monopoly_pricing, regulatory_capture).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(resource_monopoly_pricing, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
