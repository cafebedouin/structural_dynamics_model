% ============================================================================
% CONSTRAINT STORY: centralized_power_grid_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_centralized_power_grid_lock_in, []).

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
 *   constraint_id: centralized_power_grid_lock_in
 *   human_readable: Centralized Power Grid Lock-In
 *   domain: infrastructure/energy/political_economy
 *
 * SUMMARY:
 *   Centralized power grid lock-in represents a hybrid
 *   coordination-extraction constraint where legitimate operational benefits
 *   (frequency regulation, voltage stability, demand balancing) are
 *   intertwined with asymmetric rent capture and barriers to distributed
 *   alternatives. The constraint exhibits both genuine coordination functions
 *   (why grids were centralized) and extraction mechanisms (why alternatives
 *   are suppressed despite their technical feasibility). The rising
 *   extractiveness trajectory (0.42→0.58) reflects increasing awareness that
 *   distributed technologies can replicate grid coordination functions at
 *   lower total cost. The theater ratio remains moderate and rising slightly,
 *   indicating that both the operational necessity claims and the regulatory
 *   protections have some legitimate functional content — neither is pure
 *   performative cover — but neither is optimal for consumers or distributed
 *   producers. The constraint is spatially durable but temporally contingent:
 *   as battery costs decline, distributed generation matures, and
 *   peer-to-peer energy trading platforms develop, the architectural lock-in
 *   becomes increasingly extractive relative to available alternatives.
 *
 * KEY AGENTS:
 *   - Incumbent Utility Companies: Primary beneficiary (institutional/arbitrage) — receive guaranteed returns on centralized infrastructure, monopoly service territories, and protection from distributed competition
 *   - Trapped Consumers: Primary victim (powerless/trapped) — bear extraction through regulated monopoly rates with no practical exit path
 *   - Distributed Energy Producers: Secondary victim (moderate/constrained) — face interconnection barriers, net metering restrictions, and standby fees that protect incumbents
 *   - Grid Operators: Institutional actor (institutional/constrained) — benefit from centralization but also constrained by legacy infrastructure; provide genuine coordination services
 *   - Decentralization Coalition: Organized actors (organized/mobile) — solar advocates, microgrid developers, storage manufacturers building alternative pathways with sunset mechanism
 *   - Regulatory Agencies: Institutional gatekeeper (institutional/arbitrage) — maintain appearance of public interest through rate regulation while enabling incumbent protection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(centralized_power_grid_lock_in, 0.58).
domain_priors:suppression_score(centralized_power_grid_lock_in, 0.65).
domain_priors:theater_ratio(centralized_power_grid_lock_in, 0.54).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(centralized_power_grid_lock_in, extractiveness, 0.58).
narrative_ontology:constraint_metric(centralized_power_grid_lock_in, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(centralized_power_grid_lock_in, theater_ratio, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(centralized_power_grid_lock_in, tangled_rope).
narrative_ontology:human_readable(centralized_power_grid_lock_in, "Centralized Power Grid Lock-In").
narrative_ontology:topic_domain(centralized_power_grid_lock_in, "infrastructure/energy/political_economy").

domain_priors:requires_active_enforcement(centralized_power_grid_lock_in).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(centralized_power_grid_lock_in, incumbent_utility_companies).
narrative_ontology:constraint_beneficiary(centralized_power_grid_lock_in, centralized_generation_operators).
narrative_ontology:constraint_victim(centralized_power_grid_lock_in, distributed_energy_producers).
narrative_ontology:constraint_victim(centralized_power_grid_lock_in, grid_decentralization_movements).
narrative_ontology:constraint_victim(centralized_power_grid_lock_in, energy_consumers_with_high_barriers_to_exit).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED CONSUMER (SNARE) — Residential and small business users face legal monopoly or near-monopoly utility service with no practical exit option. Cannot generate, store, or trade electricity independently due to regulatory barriers and grid access restrictions. Bears full cost of infrastructure extraction through rates; experiences suppression as absence of alternatives.
constraint_indexing:constraint_classification(centralized_power_grid_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DISTRIBUTED ENERGY PRODUCER (SNARE) — Small-scale renewable generator (rooftop solar, community microgrid, local wind) faces grid interconnection barriers, unfavorable net metering policies, standby fees, and curtailment requirements. Can exit to off-grid microgrid but at high cost; exit pathway is constrained by regulatory capture and interconnection standards designed to protect centralized incumbents.
constraint_indexing:constraint_classification(centralized_power_grid_lock_in, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: GRID OPERATOR (TANGLED ROPE) — System operator benefits from centralized architecture (operational simplicity, demand forecasting certainty) and faces constraints from legacy physical infrastructure. Genuine coordination function: grid balancing, load management, reliability assurance. But also extracts through technological lock-in (proprietary SCADA systems, control protocols that favor incumbent generators). Both coordination and extraction present.
constraint_indexing:constraint_classification(centralized_power_grid_lock_in, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INCUMBENT UTILITY COMPANY (ROPE) — Experiences centralized grid as pure coordination benefit. Regulatory guaranteed returns on infrastructure investment, monopoly service territory, stable revenue from captive customer base. No exit pressure; benefits from network effects and regulatory moats. Faces no effective extraction cost — the constraint flows value toward this actor.
constraint_indexing:constraint_classification(centralized_power_grid_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: DECENTRALIZATION COALITION (SCAFFOLD) — Organized actors (rooftop solar advocates, microgrid developers, renewable energy cooperatives, distributed storage manufacturers, smart grid technology vendors) are creating alternative pathways that bypass centralized extraction. Battery storage maturity, peer-to-peer energy trading platforms, and grid-forming inverters represent sunset mechanisms. High agency and visible exit path — effective extraction is dampened by coalition capacity to build alternatives.
constraint_indexing:constraint_classification(centralized_power_grid_lock_in, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: REGULATORY THEATER (PITON) — Utility regulation (rate-of-return guarantees, cost-plus pricing, regulatory commissions) maintains the appearance of public interest protection and grid reliability assurance. But the functional content has degraded: regulators are often captured by incumbents, rate structures reflect historical peak-load patterns rather than real-time marginal costs, and reliability metrics (SAIDI/SAIFI) measure centralized grid performance, not consumer welfare. The theater persists through institutional inertia.
constraint_indexing:constraint_classification(centralized_power_grid_lock_in, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, centralized grids enable legitimate coordination benefits (voltage stability, frequency regulation, demand-response balancing) that genuine distributed systems must replicate or exceed. But the architectural lock-in is real: grid designs, regulatory frameworks, and market rules were engineered to favor centralization and now resist distributed alternatives. Coordination function is genuine; extraction mechanism is genuine; both persist.
constraint_indexing:constraint_classification(centralized_power_grid_lock_in, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(centralized_power_grid_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(centralized_power_grid_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(centralized_power_grid_lock_in, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(centralized_power_grid_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(centralized_power_grid_lock_in, TR),
    TR >= 0.70.

:- end_tests(centralized_power_grid_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising. The constraint extracts in multiple forms: (1) Monopoly rate markups protected by regulatory moats; (2) Barriers to distributed entry through interconnection standards and net metering rules; (3) Technological lock-in through proprietary SCADA and control protocols; (4) Incumbent capture of grid modernization investment (smart meters, advanced metering infrastructure) as cost-plus additions rather than consumer benefit. The rising trajectory reflects that distributed technologies (rooftop solar, battery storage) have reached sufficient maturity that the extraction mechanism becomes increasingly visible — consumers could exit but are prevented, rather than being unable to exit for technical reasons. Suppression (0.65): High. Regulatory barriers include: interconnection application delays, standby fees for grid-connected distributed generators, net metering policies that credit distributed generation below avoided cost, equipment certification requirements, and default grid-tie inverter restrictions. These suppress exit alternatives while maintaining the appearance of technical prudence. Theater ratio (0.54): Moderate. Utility regulation maintains a performative apparatus (public utility commissions, rate hearings, reliability metrics) that suggests consumer protection and public interest oversight. But the functional content is compromised: regulators are often captured by utilities, rate structures reflect historical rather than marginal costs, and reliability metrics measure centralized grid performance rather than consumer welfare or environmental impact. The apparatus is not pure theater — grid stability is a real problem — but it is sufficiently performative that the actual extraction mechanism remains hidden.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. The incumbent utility sees Rope (pure coordination benefit). The trapped consumer sees Snare (pure extraction with no exit). The decentralization coalition sees Scaffold (temporary problem being solved by distributed alternatives). The regulatory apparatus sees Piton (its own degradation through capture). The grid operator sees Tangled Rope (both genuine coordination and extraction embedded in the architecture). The analytical observer sees Tangled Rope but at civilizational scope (coordination benefits are real, but architectural lock-in is increasingly unjustifiable as distributed technology matures). This full-spectrum perspectival gap is the diagnostic signal that the constraint is fundamentally about architectural contingency: the centralized design enabled coordination when alternatives were unavailable, but now persists through institutional inertia and incumbent capture despite available distributed alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   The divergence in directionality across perspectives reveals the constraint's hybrid nature. Incumbent utilities (institutional/arbitrage) experience low directionality — they are beneficiaries with exit options (the constraint flows value toward them). Trapped consumers (powerless/trapped) experience maximum directionality — they are victims with no exit, bearing full extraction cost. Distributed producers (moderate/constrained) experience high directionality — they are victims with high-cost exit paths. Grid operators (institutional/constrained) experience moderate directionality — they coordinate genuine functions but are also constrained by legacy architecture designed for centralized extraction. The analytical observer (analytical/analytical) derives a moderate-high directionality reflecting that the constraint has both legitimate coordination and extractive functions at civilizational scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that centralized grid lock-in is a historical artifact of necessity that has become an institutional extraction mechanism. At t=0 (early electrification), centralization was the only coordination mechanism available for reliable electricity distribution — this is a genuine Rope problem. As technology matured (mid-20th century), the coordination function persisted and the constraint became legitimately Tangled Rope — coordination benefits were real, extraction was justified by coordination costs. By t=20 (present day with distributed alternatives), the constraint transitions toward Snare — coordination benefits could be achieved through distributed architectures, but incumbent protection mechanisms (regulatory capture, interconnection barriers, proprietary standards) persist to maintain extraction. The rising extractiveness (0.42→0.58) and stable theater ratio (0.48→0.54) reflect this transition: the constraint is increasingly unjustifiable as a coordination mechanism while maintaining its extraction function. Mandatrophy resolution requires decomposing the constraint into time-sliced perspectives: past (Rope/Tangled Rope justified by unavailability of alternatives), present (Tangled Rope with rising extractiveness), future (Snare if regulatory lock-in persists despite distributed alternatives reaching cost parity).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    renewable_integration_bottleneck,
    'Is grid centralization necessary for reliable integration of variable renewables, or does the claim that distributed grids cannot reliably handle high renewable penetration represent an engineering constraint that can be overcome?',
    'Empirical performance data from high-renewable regions (Denmark, Australia, California) with distributed storage and smart grid technologies; comparison of centralized vs distributed grid resilience metrics under high renewable penetration',
    'If centralization is necessary: the constraint is partly a mountain (physical coordination requirement). If distributed systems achieve equivalent reliability: the constraint is predominantly snare/tangled_rope (regulatory/institutional lock-in).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(renewable_integration_bottleneck, empirical, 'Whether grid centralization is technically necessary for renewable integration').

omega_variable(
    regulatory_capture_extent,
    'To what degree are net metering restrictions, standby fees, and interconnection barriers the result of genuine grid stability concerns versus incumbent utility rent-seeking?',
    'Comparative analysis of regulatory outcomes in captured vs reformed jurisdictions (California, Germany); expert assessment of technical necessity vs economic protection in specific rule sets',
    'If primarily rent-seeking: suppression score should be higher, snare classification more robust. If genuine technical constraints: tangled_rope classification strengthens (coordination function becomes more legitimate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_extent, empirical, 'Extent of regulatory capture in grid access rules').

omega_variable(
    distributed_grid_coordination_scalability,
    'Can distributed grid architectures (microgrids, peer-to-peer trading, agent-based frequency control) achieve the coordination performance of centralized grids at similar cost as centralization costs decline?',
    'Technological readiness level assessment of distributed coordination technologies; cost curve projections for storage, smart meters, and control software; pilot microgrid performance metrics at scale',
    'If yes: scaffold sunset is structural. If no: centralization persists as least-cost coordination mechanism despite extraction costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_grid_coordination_scalability, empirical, 'Scalability of distributed coordination technologies').

omega_variable(
    consumer_exit_willingness,
    'What proportion of consumers would exit centralized grids if microgrids offered equivalent reliability and lower cost? Is the measured ''trapedness'' structural (no exit path) or identity-locked (prefer known incumbent)?',
    'Survey and revealed preference data from early microgrid adopters; analysis of exit barriers by consumer segment (income, location, risk tolerance); post-exit satisfaction tracking',
    'If structural trapedness: the constraint is genuinely snare. If identity-locked preference for utility incumbents: suppression is internalized and persists after structural barriers fall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_exit_willingness, empirical, 'Consumer trapedness: structural vs identity-locked').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(centralized_power_grid_lock_in, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cpg_tr_t0, centralized_power_grid_lock_in, theater_ratio, 0, 0.48).
narrative_ontology:measurement(cpg_tr_t10, centralized_power_grid_lock_in, theater_ratio, 10, 0.51).
narrative_ontology:measurement(cpg_tr_t20, centralized_power_grid_lock_in, theater_ratio, 20, 0.54).

% Extraction over time
narrative_ontology:measurement(cpg_be_t0, centralized_power_grid_lock_in, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cpg_be_t10, centralized_power_grid_lock_in, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(cpg_be_t20, centralized_power_grid_lock_in, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(centralized_power_grid_lock_in, global_infrastructure).
narrative_ontology:affects_constraint(centralized_power_grid_lock_in, distributed_energy_resource_integration).
narrative_ontology:affects_constraint(centralized_power_grid_lock_in, utility_regulatory_capture).
narrative_ontology:affects_constraint(centralized_power_grid_lock_in, renewable_energy_intermittency_management).

% DUAL FORMULATION NOTE:
% The centralized power grid lock-in is upstream of specific distributed technology barriers (interconnection standards, net metering rules) but represents a distinct architectural constraint with its own extractiveness trajectory. Upstream constraints include physical power systems coordination requirements (frequency stability, voltage regulation) which are genuine coordination challenges. Downstream constraints include specific regulatory barriers (interconnection delays, standby fees) that operationalize the architectural preference for centralization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(centralized_power_grid_lock_in, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
