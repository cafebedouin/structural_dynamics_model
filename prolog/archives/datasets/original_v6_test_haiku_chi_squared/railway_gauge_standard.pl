% ============================================================================
% CONSTRAINT STORY: railway_gauge_standard
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_railway_gauge_standard, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: railway_gauge_standard
 *   human_readable: The Standard Railway Gauge (4 ft 8.5 in / 1435 mm)
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The Standard Railway Gauge (4 ft 8.5 in / 1435 mm) exemplifies
 *   technological lock-in and path dependence. Originating from English
 *   colliery tramways and propagated globally through British imperial
 *   infrastructure exports, 1435 mm became the dominant global standard
 *   despite never being optimized through deliberate engineering. The
 *   constraint operates as a Tangled Rope at the aggregate level: it provides
 *   genuine coordination value (cross-border freight, equipment economies of
 *   scale, global manufacturing) while simultaneously extracting from regions
 *   that historically adopted alternative gauges and now face enormous
 *   conversion costs. The constraint exhibits a 100-year measurement interval
 *   showing rising extractiveness and theater ratio as sunk infrastructure
 *   accumulates and standardization governance becomes increasingly
 *   ritualized. Regional isolation effects are severe: the narrow-gauge
 *   regions of India (5 ft 6 in), East Africa (2 ft 6 in), and South Africa
 *   (3 ft 6 in) experience the constraint as a snare, trapped by
 *   path-dependent infrastructure decisions made 100-150 years ago. Emerging
 *   high-speed rail networks in Asia demonstrate that the constraint is not
 *   immutable—new infrastructure can choose its gauge—but the global
 *   manufacturing base is now so concentrated on 1435 mm that deviating from
 *   the standard incurs massive economic penalties.
 *
 * KEY AGENTS:
 *   - Established Rail Operators (Global North): Primary beneficiary (institutional/arbitrage) — capture economies of scale, cross-border freight efficiency, and equipment cost advantages from 1435 mm dominance
 *   - Gauge Equipment Manufacturers: Primary beneficiary (institutional/arbitrage) — massive production runs and global supply chains enabled by standardization
 *   - Isolated Narrow-Gauge Regions (India, East Africa, South Africa): Primary victim (powerless/trapped) — sunk costs in non-standard gauges exceed conversion capacity; trapped in legacy infrastructure
 *   - Regional Railway Authorities (Mixed-Gauge Regions): Secondary victim (organized/constrained) — must maintain expensive dual-gauge systems or absorb conversion costs; constrained but not powerless
 *   - Emerging High-Speed Networks: Organized agent (powerful/mobile) — can theoretically choose alternative gauges but face ecosystem pressure toward 1435 mm standardization
 *   - Gauge Standardization Bodies: Institutional actor (institutional/arbitrage) — maintain formal standards apparatus; governance increasingly performative as real coordination happened 150+ years ago
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(railway_gauge_standard, 0.38).
domain_priors:suppression_score(railway_gauge_standard, 0.52).
domain_priors:theater_ratio(railway_gauge_standard, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(railway_gauge_standard, extractiveness, 0.38).
narrative_ontology:constraint_metric(railway_gauge_standard, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(railway_gauge_standard, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(railway_gauge_standard, tangled_rope).
narrative_ontology:human_readable(railway_gauge_standard, "The Standard Railway Gauge (4 ft 8.5 in / 1435 mm)").
narrative_ontology:topic_domain(railway_gauge_standard, "technological/economic").

domain_priors:requires_active_enforcement(railway_gauge_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(railway_gauge_standard, established_rail_operators).
narrative_ontology:constraint_beneficiary(railway_gauge_standard, gauge_standardized_equipment_manufacturers).
narrative_ontology:constraint_beneficiary(railway_gauge_standard, international_rail_connectivity_users).
narrative_ontology:constraint_victim(railway_gauge_standard, gauges_not_standardized_regions).
narrative_ontology:constraint_victim(railway_gauge_standard, alternative_gauge_proponents).
narrative_ontology:constraint_victim(railway_gauge_standard, interoperability_marginal_regions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ISOLATED NARROW-GAUGE REGION (SNARE) — Railway infrastructure in regions that historically adopted non-standard gauges (3 ft in India before partial standardization, 2 ft 6 in in East Africa, 3 ft 6 in in South Africa) faces enormous sunk costs converting to 1435 mm. Trapped: conversion costs exceed regional economic capacity; alternative gauge equipment is disappearing. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.53.
constraint_indexing:constraint_classification(railway_gauge_standard, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL RAILWAY AUTHORITIES (TANGLED ROPE) — EU, North American, and Asian railway operators benefit from 1435 mm standardization (cross-border freight, locomotive interchange, equipment economies of scale) but remain constrained by sunk infrastructure and conversion costs. Must maintain dual-gauge systems or absorb conversion expense. d≈0.58, f(d)≈0.72, σ=1.1 → χ≈0.32.
constraint_indexing:constraint_classification(railway_gauge_standard, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: GAUGE EQUIPMENT MANUFACTURERS (ROPE) — Locomotive, wagon, and rail manufacturers benefit enormously from standardization: massive production runs, global supply chains, parts interchangeability. Standard gauge is a pure coordination benefit — enables markets that would not exist with fragmentation. d≈0.10, f(d)≈0.05, σ=1.2 → χ≈0.02. Net beneficiary through coordination.
constraint_indexing:constraint_classification(railway_gauge_standard, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EMERGING HIGH-SPEED RAIL NETWORKS (SCAFFOLD) — New high-speed rail (China, Japan, South Korea) can adopt optimal gauge de novo without legacy constraint. These projects adopt 1435 mm or 1676 mm as strategic choices, not inherited lock-in. They benefit from standardization (Shanghai-Beijing interoperability) but could theoretically choose differently. d≈0.35, f(d)≈0.33, σ=1.2 → χ≈0.15. Low extraction because agents have agency in new infrastructure decisions.
constraint_indexing:constraint_classification(railway_gauge_standard, scaffold,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY GAUGE STANDARDIZATION BODIES (PITON) — International railway standards (ISO 4435, UIC standards) maintain 1435 mm as the canonical reference. The formal standardization apparatus persists through institutional inertia — it produces specifications, testing protocols, and certification procedures, but the real coordination happened 150+ years ago. Theater ratio reflects that much standards activity is performative compliance ritual rather than active problem-solving. theater_ratio=0.58. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.03.
constraint_indexing:constraint_classification(railway_gauge_standard, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CIVILIZATIONAL ANALYTICAL OBSERVER (MOUNTAIN — FALSE SUMMIT RISK) — The analytical perspective risks naturalizing the gauge standard as an immutable consequence of network effects and coordination physics. From this view, 1435 mm is inevitable: once it achieves critical mass, any alternative is mathematically dominated (Schelling point logic). However, structural data (ε=0.38, suppression=0.52, theater=0.58) contradicts mountain classification. The 'inevitability' is narrative not physics — other gauges are equally stable locally (India 5 ft 6 in is self-reinforcing within India; Japan's gauge fragmentation is self-equilibrating). The gauge appears natural only from the perspective of those benefiting from 1435 mm dominance.
constraint_indexing:constraint_classification(railway_gauge_standard, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(railway_gauge_standard_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(railway_gauge_standard, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(railway_gauge_standard, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(railway_gauge_standard, TR),
    TR >= 0.70.

:- end_tests(railway_gauge_standard_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-high. The constraint imposes real costs on non-standard regions (conversion expense, equipment scarcity, isolation from global freight networks) while providing genuine benefits to standard-gauge users. The extraction is not coercive (no active enforcement of gauge adoption) but structural: lock-in effects create asymmetric costs. Extractiveness has risen from 0.18 (early 20th century, when gauge variance was more tolerable and equipment was less specialized) to 0.38 (21st century, as manufacturing concentrated and global interoperability premiums increased). Suppression (0.52): Moderate-high. Alternatives to 1435 mm are not actively forbidden, but barriers to adoption are severe: equipment unavailable, repair networks absent, economic scale disadvantages, and lock-in effects on existing infrastructure. These are structural suppressions, not coercive. Theater ratio (0.58): Moderate. Governance theater has increased: formal standardization bodies produce extensive documentation, testing protocols, and certification procedures. Much of this activity is performative—the real coordination happened in the 19th century—but enough genuine problem-solving remains (interoperability verification, safety certification) to keep theater below 0.70 (piton threshold).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a stark perspectival split between beneficiaries and victims. Manufacturers and established operators (Rope perspective) see pure coordination: the standard enables markets. Emerging networks (Scaffold perspective) see a temporary constraint on new projects—they can choose alternatives, but face ecosystem pressure. Isolated narrow-gauge regions (Snare perspective) see pure extraction: they are locked into disadvantageous infrastructure with no exit. Regional authorities managing dual-gauge systems (Tangled Rope perspective) see the constraint as a mixed bag: benefits from cross-border interoperability, costs from maintenance complexity. The standardization bodies (Piton perspective) maintain governance rituals that were once functional but are now largely ceremonial. The civilizational observer (Mountain perspective) risks naturalizing contingency as inevitability, claiming the standard is a universal law of transportation physics—a false summit that ignores historical accident and power asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Gauge Equipment Manufacturers: Beneficiary + arbitrage → d≈0.10, f(d)≈0.05. Net beneficiary; see pure coordination. Established Rail Operators: Beneficiary + arbitrage → d≈0.15, f(d)≈-0.01. Net beneficiary; interoperability value outweighs any constraint costs. Emerging High-Speed Networks: Mobile agent + strategic choice → d≈0.35, f(d)≈0.33. Low extraction; these agents have agency. Regional Railway Authorities (Dual-Gauge): Victim + constrained → d≈0.58, f(d)≈0.72. Moderate extraction; trapped by infrastructure but not powerless. Narrow-Gauge Regions: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction; no exit options. Standardization Bodies: Institutional + arbitrage → d≈0.05, f(d)≈-0.12. Piton classification driven by theater_ratio, not directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The Standard Railway Gauge resolves mandatrophy through decomposition by agent type. The constraint is Rope at the manufacturing level (pure coordination benefit, no asymmetric cost), Tangled Rope at the regional operator level (mixed coordination and extraction), Snare at the powerless-region level (pure extraction), and Scaffold at the emerging-network level (temporary constraint with agency and alternatives). No single type is correct; the presheaf over different agents IS the correct answer. The false summit (Mountain perspective) arises when analysts naturalize the standard as inevitable due to 'network effects' or 'Schelling point logic.' However, other gauges demonstrate equally stable equilibrium: Indian 5 ft 6 in is self-reinforcing within India; Japan's mixed-gauge system is self-equilibrating; Russian broad gauge (1520 mm) covers Eurasia independently. The 1435 mm standard is dominant globally not because it is physically inevitable but because it benefited from British imperial export and first-mover advantage in global manufacturing. The constraint became increasingly extractive as manufacturing consolidated and equipment specialization increased, raising the cost of deviation. The theater ratio has risen because standardization governance now consists largely of certification and testing rituals (functional maintenance of existing standard) rather than active coordination problem-solving (standardization was once that—now it is bureaucracy).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimal_gauge_physics,
    'Is 1435 mm objectively superior to alternative gauges from engineering and safety perspectives, or is it merely a local optimum that became dominant through historical accident?',
    'Engineering analysis: structural stability, derailment risk, aerodynamic efficiency, and maintenance cost across gauge options. Comparative performance data from regions with different historical gauges.',
    'If 1435 mm is objectively superior: constraint approaches Mountain (natural law of rail engineering). If locally optimal but not globally superior: constraint remains Tangled Rope (contingent lock-in, not inevitable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimal_gauge_physics, empirical, 'Whether 1435 mm is objectively optimal or merely dominant').

omega_variable(
    conversion_cost_threshold,
    'At what regional economic threshold does gauge conversion become economically rational rather than sunk-cost imprisonment?',
    'Cost-benefit analysis: conversion expense vs. long-term freight efficiency gains, interoperability value, equipment lifecycle amortization. Regional economic modeling for India, East Africa, and South Africa.',
    'If threshold is low (~$5-10B): snare perspective is temporary; regions will convert as wealth increases. If threshold is high (>$50B): snare is structural; trapped regions remain trapped indefinitely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conversion_cost_threshold, empirical, 'Economic threshold for rational gauge conversion').

omega_variable(
    dual_gauge_viability,
    'Can dual-gauge systems (mixed 1435 mm and alternative gauge on same corridor) remain economically viable indefinitely, or do they inevitably collapse toward monogauge?',
    'Historical analysis of dual-gauge systems (Spain, Russia, Australia); cost tracking of mixed-gauge operations; maintenance and switching yard economics.',
    'If dual-gauge is viable: snare is partially escapable through mixed systems (reduces d). If inevitably collapses: snare is structural (victims must eventually choose).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_gauge_viability, empirical, 'Whether dual-gauge systems can remain viable long-term').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(railway_gauge_standard, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rgauge_tr_t0, railway_gauge_standard, theater_ratio, 0, 0.25).
narrative_ontology:measurement(rgauge_tr_t50, railway_gauge_standard, theater_ratio, 50, 0.42).
narrative_ontology:measurement(rgauge_tr_t100, railway_gauge_standard, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(rgauge_be_t0, railway_gauge_standard, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(rgauge_be_t50, railway_gauge_standard, base_extractiveness, 50, 0.32).
narrative_ontology:measurement(rgauge_be_t100, railway_gauge_standard, base_extractiveness, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(railway_gauge_standard, global_infrastructure).
narrative_ontology:affects_constraint(railway_gauge_standard, rail_gauge_india_standardization).
narrative_ontology:affects_constraint(railway_gauge_standard, east_africa_narrow_gauge_conversion).
narrative_ontology:affects_constraint(railway_gauge_standard, japanese_gauge_fragmentation).
narrative_ontology:affects_constraint(railway_gauge_standard, interoperability_eurasian_rail).

% DUAL FORMULATION NOTE:
% The Standard Railway Gauge as a global constraint is distinct from regional gauge choices (India 5 ft 6 in, South Africa 3 ft 6 in, Japan mixed-gauge, Russia 1520 mm). This story models the 1435 mm standard as a global coordination mechanism that simultaneously creates lock-in for non-adopting regions. Downstream constraints represent regional-specific victims (India standardization pressure, East Africa conversion costs) and independent parallel systems (Russian broad gauge, Japanese multi-gauge fragmentation). All are linked: the global 1435 mm standard creates competitive pressure on alternative systems.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
