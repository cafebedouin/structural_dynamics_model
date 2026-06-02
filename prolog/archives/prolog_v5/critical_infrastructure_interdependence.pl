% ============================================================================
% CONSTRAINT STORY: critical_infrastructure_interdependence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_critical_infrastructure_interdependence, []).

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
 *   constraint_id: critical_infrastructure_interdependence
 *   human_readable: Critical Infrastructure Interdependence and Systemic Fragility
 *   domain: infrastructure/systemic_risk/governance
 *
 * SUMMARY:
 *   Critical infrastructure interdependence creates a structural tension
 *   between the coordination benefits of integrated systems and the fragility
 *   costs of cascading failure. As power grids, water systems,
 *   telecommunications, and transportation networks have become increasingly
 *   interconnected, they have achieved remarkable efficiency and scale — but
 *   at the cost of creating systemic fragility where localized failures
 *   propagate globally. This constraint exemplifies how genuine coordination
 *   functions (managing power flows, ensuring stability) coexist with
 *   asymmetric extraction (concentrating control, distributing risk) in a
 *   single structural arrangement. The same interdependence that enables
 *   economies of scale also traps communities in dependence on external
 *   operators, concentrates decision-making authority, and creates
 *   system-wide fragility. The extractiveness value (0.58) reflects moderate
 *   extraction: system integrators capture significant surplus from their
 *   coordination role, but much of the interdependence is genuinely
 *   functional rather than extractive overhead. Theater ratio (0.48)
 *   indicates that roughly half the regulatory and operational apparatus is
 *   performative — legacy standards that assume centralized generation but
 *   persist despite distributed generation, tariff structures that assume
 *   one-way power flow but persist despite bidirectional flow, reliability
 *   metrics that measure system stability but ignore community resilience.
 *
 * KEY AGENTS:
 *   - Dependent Communities: Primary victim (powerless/trapped) — reliant on external infrastructure with no exit option; bear full cost of outages and fragility without control over design or operation
 *   - Regional Grid Operators: Secondary victim/mixed (moderate/constrained) — benefit from scale efficiencies but bear operational and liability burden; constrained by interconnection requirements and capital lock-in
 *   - System Integrators: Primary beneficiary (institutional/arbitrage) — capture surplus by positioning as indispensable coordination hub; can arbitrage between grids and markets
 *   - Distributed Resilience Movement: Organized agents (organized/constrained) — building alternative pathways (microgrids, renewable clusters, community cooperatives) with decentralization sunset trajectory
 *   - Legacy Regulatory Framework: Institutional artifact (institutional/arbitrage) — maintains mid-20th-century assumptions (centralized generation, one-way transmission) through performative standards and tariff structures
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing design choices (centralized grids maximize scale efficiency) as physical necessity (interdependence is inherently fragile)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(critical_infrastructure_interdependence, 0.58).
domain_priors:suppression_score(critical_infrastructure_interdependence, 0.65).
domain_priors:theater_ratio(critical_infrastructure_interdependence, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(critical_infrastructure_interdependence, extractiveness, 0.58).
narrative_ontology:constraint_metric(critical_infrastructure_interdependence, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(critical_infrastructure_interdependence, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(critical_infrastructure_interdependence, tangled_rope).
narrative_ontology:human_readable(critical_infrastructure_interdependence, "Critical Infrastructure Interdependence and Systemic Fragility").
narrative_ontology:topic_domain(critical_infrastructure_interdependence, "infrastructure/systemic_risk/governance").

domain_priors:requires_active_enforcement(critical_infrastructure_interdependence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(critical_infrastructure_interdependence, centralized_operators).
narrative_ontology:constraint_beneficiary(critical_infrastructure_interdependence, system_integrators).
narrative_ontology:constraint_victim(critical_infrastructure_interdependence, distributed_communities).
narrative_ontology:constraint_victim(critical_infrastructure_interdependence, regional_resilience_capacity).
narrative_ontology:constraint_victim(critical_infrastructure_interdependence, system_reliability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT COMMUNITY (SNARE) — Trapped in reliance on externally-controlled infrastructure with no exit option. Cannot generate local power, water, communications without system access. Bears full extraction: cost of outages, no control over service quality, mandatory participation in risk cascade.
constraint_indexing:constraint_classification(critical_infrastructure_interdependence, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: REGIONAL GRID OPERATOR (TANGLED ROPE) — Constrained by interconnection requirements and capital lock-in. Benefits from economies of scale in grid coordination; bears costs of cascading failure liability and demand for 24/7 reliability. Mixed function: genuine coordination of distributed generation with transmission, but also asymmetric extraction of operational burden onto communities with no control.
constraint_indexing:constraint_classification(critical_infrastructure_interdependence, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SYSTEM INTEGRATOR (ROPE) — Benefits from interconnection requirement; extracts value by positioning as indispensable hub. Experiences constraint as coordination: managing interdependencies creates stable revenue stream. Low suppression of this agent; exit option exists (serve different grid or market).
constraint_indexing:constraint_classification(critical_infrastructure_interdependence, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DISTRIBUTED RESILIENCE MOVEMENT (SCAFFOLD) — Organized groups (microgrids, community cooperatives, renewable clusters) building alternative infrastructure pathways. Low effective extraction because movement sees sunset: distributed generation, battery storage, and local control norms are building exit mechanisms. Sunset clause: 15-25 years for decentralized alternatives to reach parity with centralized grids in cost and reliability.
constraint_indexing:constraint_classification(critical_infrastructure_interdependence, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGACY REGULATORY FRAMEWORK (PITON) — Historical regulatory structures assumed centralized generation and one-way transmission. Modern grid has distributed generation, bidirectional flows, and decentralized control, but regulation persists in theater form: tariff structures, interconnection standards, and reliability metrics still assume mid-20th-century architecture. Theater ratio 0.48 reflects that some real function remains (frequency stability, congestion management) but much is performative artifact of outdated assumptions.
constraint_indexing:constraint_classification(critical_infrastructure_interdependence, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, interdependence of large-scale energy systems is inherent to physics and thermodynamics: managing power flows across a continent requires real-time coordination that cannot be fully decentralized. Baseload generation requires transmission. Demand variability requires buffering. This perspective risks naturalizing what is actually a design choice: the current architecture maximizes scale efficiency at the cost of fragility. The engine's false summit detector should flag this — centralized grids are contingent institutional arrangements, not laws of nature.
constraint_indexing:constraint_classification(critical_infrastructure_interdependence, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(critical_infrastructure_interdependence_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(critical_infrastructure_interdependence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(critical_infrastructure_interdependence, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(critical_infrastructure_interdependence, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(critical_infrastructure_interdependence, TR),
    TR >= 0.70.

:- end_tests(critical_infrastructure_interdependence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. System integrators capture significant surplus by positioning as indispensable coordinators of interdependent flows. This is extraction, but not maximal — much of the surplus is genuine payment for real coordination function (frequency stability, congestion management, demand response). The 20-year trajectory from 0.35 to 0.58 reflects increasing extraction as system integrators have consolidated control: early deregulation allowed competitive entry and reduced extraction; later recentralization and financialization increased it. Suppression (0.65): Moderate-high. Communities face significant barriers to exit: no local alternative for baseload generation (technical barrier), regulatory barriers to interconnection (institutional), high capital requirements for local generation (economic). Suppression is not total because some communities can build microgrids, solar arrays, or storage systems — barriers are high but not insurmountable. Theater ratio (0.48): Moderate. Regulatory and operational theater has increased over time. Early grid operation was purely functional (balance supply and demand). Modern operation includes significant performative elements: tariff structures that assume centralized generation (obsolete), reliability metrics that measure system stability but not community resilience (misaligned), interconnection standards that slow distributed generation (institutional friction). But some theater remains functional — frequency stability, congestion management, and cascading failure prevention require real coordination.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates divergent classification across positions. System integrators see Rope — they are solving legitimate coordination problems and experience genuine benefit. Dependent communities see Snare — they are trapped in reliance with extraction and no exit. Regional operators see Tangled Rope — they coordinate flows but bear liability for cascade failures. Distributed resilience groups see Scaffold — they have agency and see an exit path (decentralized generation). Legacy regulators see Piton — they maintain outdated standards through theater. The analytical observer risks Mountain — seeing interdependence as inherent to physics rather than a contingent design choice. The perspectival gap reveals that the same structural arrangement (interdependent grids) appears fundamentally different depending on whether you benefit from coordination (Rope), suffer from fragility (Snare), carry operational burden (Tangled Rope), or have agency to build alternatives (Scaffold).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies by agent's position relative to extraction flow. System integrators (beneficiary + arbitrage) have low d — they extract surplus but can arbitrage between markets, keeping them mobile. Dependent communities (victim + trapped) have high d — they bear costs with no exit option. Regional operators (victim + constrained) have moderate d — they bear operational burden but can exit by serving different grid. Distributed groups (victim + constrained + organized) have lower effective d because organization and agency reduce experienced extraction. The analytical observer (analytical + analytical) has d ~0.72 — sees the full structure but risks naturalizing contingent arrangements.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVES MANDATROPHY: The constraint avoids mislabeling by decomposing the coordination function from the extraction function. The genuine coordination (managing power flows in real time across scale) is necessary and generates the benefits that system integrators capture. The extraction (concentrating control, trapping communities in dependence, creating systemic fragility) is contingent on architectural choices that are not inherent to coordination. The Tangled Rope classification captures this: real coordination function (ε_coordination ~ 0.20, Rope baseline) + asymmetric extraction (ε_extraction ~ 0.38, Snare overhead) = ε_net 0.58. The scaffold perspective shows that the extraction can be removed (distributed generation eliminates need for centralized coordination) without eliminating the coordination function (local microgrids coordinate power flows at smaller scale). Decentralized grids lose some efficiency (higher per-unit cost, lower reliability in immediate sense) but eliminate fragility and extraction. The constraint is not 'interdependence is evil' (false — real coordination benefit) but 'this specific centralized architecture concentrates extraction alongside coordination, and alternative architectures can provide coordination with less extraction at the cost of efficiency.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cascade_failure_mechanism,
    'Is cascading failure a structural property of interdependence or an artifact of specific control architectures and underinvestment in redundancy?',
    'Comparative analysis of grid designs with different redundancy levels; simulation of failure propagation under varying topology and control assumptions; historical data on cascade events before/after automation and centralization.',
    'If structural: interdependence is inherently fragile (mountain view partially correct). If architectural: fragility is contingent on design choices that can be changed (scaffold view confirmed; decentralized alternatives viable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cascade_failure_mechanism, empirical, 'Whether cascading failure is structural or architectural').

omega_variable(
    decentralization_feasibility,
    'Can distributed generation and local microgrids provide baseload reliability comparable to centralized grids without sacrificing cost efficiency or introducing new fragilities?',
    'Cost analysis of distributed vs centralized generation with storage; reliability metrics (SAIFI, SAIDI) comparison across hybrid architectures; scale analysis of battery storage technology curves.',
    'If viable: scaffold sunset is real, distributed resilience movement will succeed. If infeasible: communities remain trapped, suppression persists indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralization_feasibility, empirical, 'Technical and economic feasibility of distributed generation alternatives').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.65) due to technical barriers to local generation (material constraints) or to regulatory/market structures that favor centralization (institutional extraction)?',
    'Compare technical barriers to local generation (solar irradiance, storage density, local baseload capacity) against regulatory barriers (interconnection standards, tariff structures, capital requirements). Jurisdictional comparison: where regulatory barriers are lower, what proportion of generation is local vs centralized?',
    'If technical: reducing suppression requires technological breakthrough. If institutional: regulatory reform can dramatically reduce suppression within years.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether suppression is technical or institutional').

omega_variable(
    system_integrator_necessity,
    'Is the system integrator''s role (managing interdependencies, ensuring stability) genuinely necessary for grid function, or could distributed agents with local control achieve equivalent stability at lower cost?',
    'Historical analysis of grid stability before/after centralized control systems; simulation of stability under distributed agents with no central coordinator; pilot projects comparing performance.',
    'If necessary: system integrator extraction is justified (justified extraction, Rope classification correct). If contingent: the integrator is a beneficiary capturing surplus from a coordination problem that could be solved otherwise (Snare view from integrator''s perspective becomes valid).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(system_integrator_necessity, empirical, 'Whether centralized system integration is necessary or contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(critical_infrastructure_interdependence, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cii_tr_t0, critical_infrastructure_interdependence, theater_ratio, 0, 0.28).
narrative_ontology:measurement(cii_tr_t10, critical_infrastructure_interdependence, theater_ratio, 10, 0.38).
narrative_ontology:measurement(cii_tr_t20, critical_infrastructure_interdependence, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(cii_be_t0, critical_infrastructure_interdependence, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cii_be_t10, critical_infrastructure_interdependence, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(cii_be_t20, critical_infrastructure_interdependence, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(critical_infrastructure_interdependence, global_infrastructure).
narrative_ontology:affects_constraint(critical_infrastructure_interdependence, grid_cascading_failure).
narrative_ontology:affects_constraint(critical_infrastructure_interdependence, water_system_interdependence).
narrative_ontology:affects_constraint(critical_infrastructure_interdependence, telecom_backbone_concentration).

% DUAL FORMULATION NOTE:
% Critical infrastructure interdependence is the upstream structural constraint. Specific failure events (power outages, water main breaks, internet backbone outages) are downstream manifestations of the fundamental architecture choice (centralized vs distributed). Grid cascading failure, water system interdependence, and telecom backbone concentration are structurally similar constraints with the same ε-decomposition: genuine coordination function + contingent extraction architecture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(critical_infrastructure_interdependence, powerful, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
