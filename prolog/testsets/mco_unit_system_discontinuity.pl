% ============================================================================
% CONSTRAINT STORY: mco_unit_system_discontinuity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mco_unit_system_discontinuity, []).

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
 *   constraint_id: mco_unit_system_discontinuity
 *   human_readable: Persistence of Imperial Units in a Metric World
 *   domain: technological/institutional
 *
 * SUMMARY:
 *   The persistence of Imperial units in a global scientific and
 *   technological community that adopted metric standardization decades ago
 *   is a canonical example of a Piton — a degraded constraint maintained by
 *   institutional inertia rather than functional necessity. After the SI
 *   (International System of Units) adoption in 1960 and widespread
 *   metrication waves in the 1970s-1980s, the continued use of feet, inches,
 *   pounds, and other imperial units in aerospace manufacturing,
 *   construction, and legacy engineering systems has no physical
 *   justification. The constraint persists because conversion costs are high,
 *   existing tools and practices are sunk investments, and certain sectors
 *   (US aerospace, construction) captured sufficient market power to maintain
 *   domestic imperial standardization. The theater_ratio is high (0.78)
 *   because the functional justification has largely disappeared — modern
 *   manufacturing can easily work in metric, and dual-unit translation is now
 *   automated. The extraction (0.18) is relatively low because the constraint
 *   primarily benefits legacy suppliers through reduced switching costs
 *   rather than through active enforcement mechanisms. This is a constraint
 *   that should have dissolved but persists through path dependence and
 *   organizational inertia.
 *
 * KEY AGENTS:
 *   - Legacy Aerospace Manufacturers: Institutional beneficiary (institutional/arbitrage) — maintain imperial tooling and CAD systems inherited from 20th-century US standardization; capture arbitrage value from being tied to domestic market
 *   - US Construction Industry: Organized beneficiary (organized/constrained) — maintains imperial building codes and supplier networks; constrained by regulatory lock-in despite collective power
 *   - International Standards Bodies (ISO, BIPM): Victim/powerless (institutional/trapped) — cannot enforce metric adoption; trapped in maintaining dual-system verification and documentation
 *   - Modern Scientific Instrument Manufacturers: Mobile beneficiary (powerful/mobile) — produce metric-native instruments, see imperial as minor coordination problem, benefit from metric standardization globally
 *   - Metrication Transition Coalition: Organized agent (organized/mobile) — building conversion pathways (software, automated translation, dual-labeling standards); actively working toward sunset
 *   - Analytical Observer: Historical perspective (institutional/analytical) — sees constraint as museum piece of institutional momentum with no functional justification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mco_unit_system_discontinuity, 0.18).
domain_priors:suppression_score(mco_unit_system_discontinuity, 0.42).
domain_priors:theater_ratio(mco_unit_system_discontinuity, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mco_unit_system_discontinuity, extractiveness, 0.18).
narrative_ontology:constraint_metric(mco_unit_system_discontinuity, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(mco_unit_system_discontinuity, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mco_unit_system_discontinuity, piton).
narrative_ontology:human_readable(mco_unit_system_discontinuity, "Persistence of Imperial Units in a Metric World").
narrative_ontology:topic_domain(mco_unit_system_discontinuity, "technological/institutional").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mco_unit_system_discontinuity, legacy_aerospace_manufacturers).
narrative_ontology:constraint_beneficiary(mco_unit_system_discontinuity, us_construction_industry).
narrative_ontology:constraint_beneficiary(mco_unit_system_discontinuity, established_engineering_practices).
narrative_ontology:constraint_victim(mco_unit_system_discontinuity, international_scientific_coordination).
narrative_ontology:constraint_victim(mco_unit_system_discontinuity, cross_border_standards_adoption).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LEGACY AEROSPACE SUPPLIER (PITON) — Maintains imperial tooling, CAD systems, and supply chains inherited from 20th-century standardization. The constraint persists through institutional inertia: conversion cost is high, switching would disrupt established practices, and arbitrage opportunity exists in serving a domestic market that still uses imperial specifications. Theater_ratio high because the functional justification (technical necessity) has eroded — the constraint now persists through sunk costs and habit rather than genuine technical requirement. Extraction is low because this agent benefits from the status quo without active enforcement.
constraint_indexing:constraint_classification(mco_unit_system_discontinuity, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 2: INTERNATIONAL STANDARDS BODY (SNARE) — Trapped in a coordination failure: cannot enforce global metric adoption without defecting to unilateral action, cannot abandon metric standardization without betraying SI adoption. Bears cost of maintaining dual-system documentation, dual verification pathways, and reduced verification confidence. No exit option: the body's legitimacy depends on metric standardization, yet cannot compel legacy actors to convert.
constraint_indexing:constraint_classification(mco_unit_system_discontinuity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MODERN SCIENTIFIC INSTRUMENT MANUFACTURER (ROPE) — Produces instruments natively in metric, benefits from global standardization, has mobile exit options (can serve metric-only markets). Sees imperial persistence as a minor coordination problem requiring translation layers and dual documentation. Low extraction because this agent can arbitrage between markets and has capacity to serve metric-standardized sectors.
constraint_indexing:constraint_classification(mco_unit_system_discontinuity, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: US CONSTRUCTION INDUSTRY (TANGLED_ROPE) — Organized sector with significant collective power but constrained exit: imperial units are deeply embedded in building codes, supplier networks, and professional standards. Benefits from coordination (unified domestic standards reduce transaction costs). Also bears extraction from reduced international coordination and inability to seamlessly adopt global best practices. Exit is constrained by regulatory lock-in and network effects within domestic market.
constraint_indexing:constraint_classification(mco_unit_system_discontinuity, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: METRICATION TRANSITION COALITION (SCAFFOLD) — Organized agents (ISO, BIPM, national standards bodies, multinational manufacturers) actively building conversion pathways: dimensional analysis software, automated translation systems, dual-labeling standards. See the imperial constraint as temporary coordination failure with identifiable sunset: as legacy aerospace suppliers retire and new facilities adopt metric-native CAD systems, functional dependence on imperial units declines. Theater_ratio declining because the coalition has built genuinely functional alternatives that don't rely on performative translation.
constraint_indexing:constraint_classification(mco_unit_system_discontinuity, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (PITON) — From a civilizational view, the imperial system's persistence is a museum piece of institutional inertia. Metric adoption succeeded globally despite this constraint; imperial persists only where sunk costs and path dependence create local lock-in. The constraint is theatrical: justified as 'traditional practice' or 'technical necessity' but functionally redundant. No agent is genuinely dependent on imperial units for physical reasons — all actors could convert at cost. The constraint's persistence is pure institutional momentum.
constraint_indexing:constraint_classification(mco_unit_system_discontinuity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mco_unit_system_discontinuity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mco_unit_system_discontinuity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mco_unit_system_discontinuity, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(mco_unit_system_discontinuity, TR),
    TR >= 0.70.

:- end_tests(mco_unit_system_discontinuity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The constraint does not extract significant value through coercive mechanisms. Legacy suppliers benefit from reduced conversion costs and path dependence, but the extraction is passive — benefiting from status quo rather than actively extracting from others. If the constraint required enforcement (e.g., tariffs against metric imports, mandated imperial usage), extractiveness would be higher. Instead, it persists through inertia. Suppression (0.42): Moderate. There are barriers to metric adoption (conversion costs, regulatory lock-in, network effects in domestic markets), but suppression is not severe — actors technically can convert, and many sectors have done so. The barrier is economic and institutional, not absolute. Theater_ratio (0.78): High, and increasing over the measurement interval. The functional justification for imperial persistence has eroded sharply since the 1960s SI adoption. Modern manufacturing, CAD systems, and scientific instruments are natively metric. The continued use of imperial is increasingly justified by appeals to 'tradition,' 'established practice,' or 'compatibility with legacy systems' rather than by genuine technical necessity. This is the defining feature of a Piton — the original functional coordination (imperial units enabled US industrial dominance in the early 20th century) has been replaced by institutional theater: the tools persist because they're expensive to replace, not because they work better.
 *
 * PERSPECTIVAL GAP:
 *   Legacy aerospace suppliers see the constraint as a minor inconvenience (Piton) — their imperial tooling still works, conversion would be costly, and they capture arbitrage value from serving a domestic market locked into imperial. International standards bodies see it as a coordination failure they cannot solve (Snare) — trapped between maintaining metric standardization and being unable to force legacy sectors to convert. Modern manufacturers see it as a trivial translation problem (Rope) — they produce metric instruments and let customers handle conversion if needed. The US construction industry sees it as both coordination (unified domestic standards) and extraction (cannot access international best practices) — Tangled Rope. The metrication coalition sees it as a temporary problem with a clear sunset (Scaffold) — retirement of legacy supplier base and adoption of metric-native CAD systems will eventually force conversion. The analytical observer sees it as pure institutional theater (Piton) — persisting despite zero functional necessity. The perspectival gap reveals that different agents experience dramatically different constraint types depending on their structural position and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (legacy aerospace, US construction) have low directionality values (d ≈ 0.15-0.25) because they experience the constraint as beneficial (reduced switching costs, coordination within domestic networks) combined with arbitrage options (they can serve only the domestic imperial market if they choose). They do not bear extraction; they benefit from it. Victims (international standards bodies) have high directionality values (d ≈ 0.85-0.95) because they are trapped (constrained exit) and bear the cost of maintaining dual-system verification. However, the low overall extractiveness (0.18) means that even the high-d agents do not experience severe χ values — the constraint as a whole does not extract much. The derived d values reflect that this is primarily a coordination failure, not an extraction mechanism. The piton classification derives from the theater_ratio gate (0.78 ≥ 0.70) in combination with low extractiveness, not from high experienced extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conversion_cost_threshold,
    'At what conversion cost level would legacy aerospace suppliers voluntarily adopt metric-native systems?',
    'Cost-benefit analysis of aerospace supply chain conversion; empirical data from companies that have executed full metrication; market studies of conversion ROI timelines',
    'If threshold < 5% of annual operating cost: constraint could be resolved with modest incentives. If threshold > 20%: sunk cost lock-in is structural and sunset timeline extends beyond generational horizon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conversion_cost_threshold, empirical, 'Conversion cost threshold for aerospace suppliers').

omega_variable(
    functional_necessity_residual,
    'Is any remaining imperial unit usage functionally necessary (physical constraints on measurement) or purely conventional (path-dependent habit)?',
    'Technical audit of aerospace specifications; identify which imperial dimensions derive from actual physical constraints vs. historical standardization; evaluate whether metric equivalents would require design changes',
    'If purely conventional: constraint is confirmed as piton (theater-driven). If any functional necessity remains: constraint may contain rope elements (legitimate coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(functional_necessity_residual, empirical, 'Whether imperial usage has functional vs. conventional necessity').

omega_variable(
    critical_mass_adoption_timing,
    'What level of dominant-market metric adoption would trigger voluntary cascade conversion among holdout sectors?',
    'Network model of supply chain coupling; empirical observation of adoption thresholds in prior metrication waves (automotive 1970s, pharmaceuticals 1990s); tipping point analysis',
    'If cascade occurs at > 90% adoption: sunset is steep and rapid (piton dissolves quickly). If adoption plateau emerges at 75-80%: structural lock-in persists indefinitely despite high adoption elsewhere.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(critical_mass_adoption_timing, empirical, 'Adoption threshold for cascade metrication').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mco_unit_system_discontinuity, 1960, 1990).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mco_tr_t0, mco_unit_system_discontinuity, theater_ratio, 0, 0.65).
narrative_ontology:measurement(mco_tr_t15, mco_unit_system_discontinuity, theater_ratio, 15, 0.72).
narrative_ontology:measurement(mco_tr_t30, mco_unit_system_discontinuity, theater_ratio, 30, 0.78).

% Extraction over time
narrative_ontology:measurement(mco_be_t0, mco_unit_system_discontinuity, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(mco_be_t15, mco_unit_system_discontinuity, base_extractiveness, 15, 0.2).
narrative_ontology:measurement(mco_be_t30, mco_unit_system_discontinuity, base_extractiveness, 30, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mco_unit_system_discontinuity, information_standard).
narrative_ontology:affects_constraint(mco_unit_system_discontinuity, nasa_failure_mode_cascade).
narrative_ontology:affects_constraint(mco_unit_system_discontinuity, cross_border_manufacturing_coupling).

% DUAL FORMULATION NOTE:
% Imperial persistence is downstream of historical US industrial dominance and upstream of contemporary coordination failures in cross-border manufacturing and space exploration. The Mars Climate Orbiter loss (1999) is a concrete example of how this piton constraint creates failure modes in high-stakes coordination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
