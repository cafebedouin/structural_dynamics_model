% ============================================================================
% CONSTRAINT STORY: silver_scarcity_mountain_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_silver_scarcity_mountain_2026, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: silver_scarcity_mountain_2026
 *   human_readable: The Silver Physical Scarcity Mountain
 *   domain: economic/industrial/geopolitical
 *
 * SUMMARY:
 *   Silver physical scarcity represents a canonical mountain constraint: an
 *   irreducible geological and thermodynamic limit on the supply of a
 *   critical industrial material. Annual primary silver production (≈25,000
 *   tonnes) is fixed by ore occurrence, extraction energy requirements, and
 *   recovery efficiency. The USGS designation as a Critical Mineral in 2026
 *   reflects not a sudden change in scarcity but a structural recognition
 *   that demand from clean energy (photovoltaics) and advanced electronics
 *   (semiconductors, RFID) now approaches or exceeds sustainable supply.
 *   Silver is a byproduct of copper, lead, and zinc mining: it cannot be
 *   directly extracted at scale without extracting these primary metals. Ore
 *   grades have declined monotonically for decades. No substitution
 *   technology has achieved scale in 70+ years despite intensive research.
 *   The constraint emerges naturally from earth system properties and is
 *   invariant across observables—measured by proven reserves, by annual
 *   production rates, by demand-supply gap, or by ore-grade decline, the
 *   fundamental scarcity persists. Unlike policy-driven scarcities (trade
 *   restrictions, export bans, cartel controls), this scarcity cannot be
 *   negotiated, deregulated, or arbitraged away. It is the canonical exemplar
 *   of a natural law constraint in economic systems.
 *
 * KEY AGENTS:
 *   - Solar photovoltaic manufacturers (powerless/trapped): Cannot substitute silver in front-contact grids without efficiency loss or cost multiplication; faces the constraint as physics
 *   - Electronics industry collective (moderate/constrained): Silver in solder, conductive adhesives, RFID contacts is rooted in materials physics; transition timelines are decades-long
 *   - Mining industry (institutional/arbitrage): Primary beneficiary; scarcity maintains ore prices and enables premium revenue; experiences constraint as structural gift
 *   - National resource security apparatus (powerful/mobile): Faces constraint as civilizational bottleneck on defense, energy, and computing capacity; cannot exit via policy
 *   - Emerging market assemblers (powerless/trapped): Face silver constraints as universal price signal and availability limit; cannot negotiate with geology
 *   - Analytical observer (analytical/analytical): Sees constraint as brute geological fact, invariant across observables and measurement methodologies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(silver_scarcity_mountain_2026, 0.18).
domain_priors:suppression_score(silver_scarcity_mountain_2026, 0.02).
domain_priors:theater_ratio(silver_scarcity_mountain_2026, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(silver_scarcity_mountain_2026, extractiveness, 0.18).
narrative_ontology:constraint_metric(silver_scarcity_mountain_2026, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(silver_scarcity_mountain_2026, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(silver_scarcity_mountain_2026, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(silver_scarcity_mountain_2026, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(silver_scarcity_mountain_2026, mountain).
narrative_ontology:human_readable(silver_scarcity_mountain_2026, "The Silver Physical Scarcity Mountain").
narrative_ontology:topic_domain(silver_scarcity_mountain_2026, "economic/industrial/geopolitical").

domain_priors:emerges_naturally(silver_scarcity_mountain_2026).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SOLAR PV MANUFACTURER (MOUNTAIN) — Cannot substitute silver in front-contact grids without severe efficiency loss or cost increases. The physics of electrical conductivity in silicon devices and the thermodynamics of alternative materials create a hard constraint. d≈0.90, f(d)≈1.40, σ=1.2 → χ≈0.30. The constraint appears as natural law: silver is physically necessary.
constraint_indexing:constraint_classification(silver_scarcity_mountain_2026, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ELECTRONICS INDUSTRY (MOUNTAIN) — Silver's role in solder, conductive adhesives, and RFID contacts is rooted in materials physics. Transition timelines are measured in 10-20 year cycles for technology substitution. The constraint operates as a structural limit on production scaling. d≈0.78, f(d)≈1.18, σ=1.2 → χ≈0.25. Classification stable across measured parameters.
constraint_indexing:constraint_classification(silver_scarcity_mountain_2026, mountain,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — Silver scarcity is a brute geological and thermodynamic fact. The annual primary production (≈25,000 tonnes) is fixed by ore occurrence, extraction energy requirements, and secondary recovery rates. No alternative extraction technology has emerged in 70+ years. The constraint is invariant across observables: measured by primary production, by proven reserves, by ore-grade decline, or by demand-supply gap, the fundamental scarcity persists. Emerges naturally from earth system properties. ε=0.18 because extraction is constrained by geology, not by human choices. Suppression=0.02 because alternatives exist (copper contacts, organic semiconductors, reduced-silver designs) but carry performance costs that are themselves physical limits, not policy.
constraint_indexing:constraint_classification(silver_scarcity_mountain_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: MINING INDUSTRY (MOUNTAIN) — Silver is primarily a byproduct of copper, lead, and zinc mining. The ore grades have declined steadily: average ore grade for primary silver in 1970 was 1.2 kg/tonne; in 2026 it is 0.6 kg/tonne. This decline is a physical and geological fact, not a choice. Even with maximum extraction effort and energy expenditure, the recovery rate is bounded by thermodynamic efficiency and ore chemistry. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.04. The mining industry perceives the constraint as a structural gift: scarcity maintains prices and enables premium revenue for low-grade ore processing.
constraint_indexing:constraint_classification(silver_scarcity_mountain_2026, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: NATIONAL RESOURCE SECURITY (MOUNTAIN) — Nations with industrial capacity (US, Japan, Germany, South Korea, Taiwan) experience silver scarcity as a civilizational constraint on defense, energy, and computing capacity. The USGS Critical Minerals designation reflects this: silver cannot be replaced in semiconductor packaging, photovoltaic solar cells, and military applications by policy or investment alone. Exit options are constrained by the physics of the material. d≈0.65, f(d)≈1.00, σ=1.1 → χ≈0.20. The constraint operates identically whether viewed from national or global scope.
constraint_indexing:constraint_classification(silver_scarcity_mountain_2026, mountain,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: EMERGING MARKET ASSEMBLER (MOUNTAIN) — Small and medium electronics manufacturers in Southeast Asia, India, and Central America face silver constraints as a fixed cost floor. They cannot negotiate with geology. The constraint appears locally as a universal price signal and availability limit. d≈0.92, f(d)≈1.42, σ=0.8 → χ≈0.20. Local scope dampens effective extraction (σ=0.8), but the underlying constraint is universal.
constraint_indexing:constraint_classification(silver_scarcity_mountain_2026, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(silver_scarcity_mountain_2026_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(silver_scarcity_mountain_2026, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(silver_scarcity_mountain_2026, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(silver_scarcity_mountain_2026, ExtMetricName, E),
    domain_priors:suppression_score(silver_scarcity_mountain_2026, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(silver_scarcity_mountain_2026),
    narrative_ontology:constraint_metric(silver_scarcity_mountain_2026, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(silver_scarcity_mountain_2026, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(silver_scarcity_mountain_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. Silver scarcity is constrained by geology and thermodynamics, not by human extraction choices. The constraint's 'extractiveness' reflects the degree to which supply is artificially constrained relative to theoretical maximum recovery. Theoretical maximum recovery (100% silver from all copper ore) would yield ~40,000 tonnes annually; actual recovery is ~25,000 tonnes due to thermodynamic losses, cost constraints, and co-byproduct separation inefficiency. The 0.18 value represents the gap between theoretical and actual (0.62× efficiency loss), not a policy choice to extract economically. Suppression (0.02): Minimal. Alternatives exist (copper contacts with lower conductivity, organic semiconductors, reduced-silver designs) but carry genuine performance costs rooted in materials physics. These are not suppressed alternatives; they are inferior alternatives. The lack of suppression reflects that the constraint operates as a binding physical limit, not as a coercive institutional arrangement. Theater ratio (0.15): Very low. There is no performative element. The scarcity is transparent: annual production is published, ore grades are measured, demand forecasts are explicit. No institution can maintain the appearance of plenty while restricting supply—the shortage is immediately visible in prices and availability. Accessibility collapse (0.92): High. Silver extraction requires capital-intensive mining infrastructure, specialized smelting facilities, and geological knowledge. It is highly inaccessible to new entrants. The barrier is not regulatory; it is geological (ore occurrence in specific locations) and thermodynamic (energy requirements). Resistance (0.08): Low. There is minimal institutional resistance to increasing supply—mining companies actively seek new deposits. The 'resistance' reflects physical/geological barriers, not human opposition.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates NL-invariance: all six perspectives classify as Mountain with stable reasoning. The perspectival gap is not in classification type but in the **experienced cost structure**. The solar PV manufacturer experiences the constraint as a hard technical ceiling (cannot design around silver physics). The mining industry experiences it as a structural gift (scarcity maintains margins). The national security apparatus experiences it as a civilizational bottleneck (critical for energy transition and defense). The emerging market assembler experiences it as a price floor they cannot negotiate. The analytical observer sees the invariant structure: the constraint's logic is identical regardless of observer position. This is the defining property of a true mountain—all indices yield the same type because the constraint is not socially constructed.
 *
 * DIRECTIONALITY LOGIC:
 *   All agents experience the constraint as exogenous and non-negotiable. Directionality values range from beneficiary (mining industry, d≈0.05) to victim (powerless agents, d≈0.90), but the classification remains Mountain across the full range. This is unique to mountains: the beneficiary/victim split does not change the type because the constraint is not enforced by institutional choice. Mining benefits because scarcity creates value; manufacturers and national security suffer because they must adapt to geological fact. Yet both experience the same constraint—a true mountain, not an institution that benefits some and harms others.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint fully resolves the mandatrophy: it is a canonical Mountain across all observables and all agents. The mandatrophy would arise only if alternative measurement frames produced different types. For silver scarcity, all measurement approaches (annual production, ore-grade decline, reserve estimates, thermodynamic limits, demand-supply modeling) converge on the same conclusion: irreducible geological scarcity. The constraint is not mislabeled. The risk is only that policymakers naturalize this constraint as an excuse for inaction (e.g., accepting clean energy transition failure as 'inevitable'), when in fact the constraint allows for legitimate adaptive responses (demand management, substitution research, recycling infrastructure) that remain within the Mountain classification. The mandatrophy is resolved by recognizing that a Mountain constraint is not an excuse—it is a boundary condition for problem-solving.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substitution_breakthrough_timeline,
    'Will a high-efficiency silver-free photovoltaic contact technology achieve >95% of current silver-paste efficiency and reach production scale (>10 GW annual) within 10 years?',
    'Longitudinal tracking of lab-scale substitutes (copper-based contacts, organic conductors, graphene hybrid structures); measurement of efficiency retention and manufacturing cost at scale; market adoption rates',
    'If yes: constraint transitions to Rope (coordination mechanism for planned transition). If no: constraint remains Mountain; substitution timescale extends to 20-40 years.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_breakthrough_timeline, empirical, 'Timeline for silver-free high-efficiency PV contact technology').

omega_variable(
    ore_grade_floor_discovery,
    'Does a geological or geochemical discovery (new deposit, ore-grade reversal, or extraction technology) exist that would increase global silver primary production by >30% without proportional energy cost increase?',
    'USGS mineral survey updates; geological exploration results; extraction efficiency breakthroughs; long-cycle mining discovery timelines (15-25 years per new major deposit)',
    'If yes: ε shifts downward; constraint weakens to Rope. If no: constraint remains Mountain; production ceiling is fixed by known geology.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ore_grade_floor_discovery, empirical, 'Whether geological discovery will increase silver supply significantly').

omega_variable(
    demand_destruction_rate,
    'Will demand-side reductions (efficiency improvements in semiconductor packaging, reduced per-unit silver content in photovoltaic cells, adoption of low-silver designs) outpace production growth, closing the supply-demand gap by 2035?',
    'Annual demand tracking by end-use (photovoltaics, electronics, jewelry, industrial); measurement of silver content per device; adoption rates for efficiency improvements; comparison of demand growth to production growth trajectories',
    'If yes: constraint degrades to Tangled Rope (demand management becomes active enforcement). If no: constraint persists as Mountain; scarcity drives allocation conflicts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demand_destruction_rate, empirical, 'Whether demand-side reductions close the supply-demand gap by 2035').

omega_variable(
    secondary_recovery_ceiling,
    'Is there a physical or economic limit to secondary silver recovery (from recycled electronics and industrial waste) that prevents it from supplying more than 30-35% of global demand?',
    'Study of recycling infrastructure deployment; measurement of secondary recovery rates in countries with high-recycling capture (Japan, EU); identification of thermodynamic or economic barriers to higher recovery',
    'If limit is hard: primary supply remains the binding constraint, Mountain classification stable. If limit is soft: secondary supply could become dominant, converting constraint to managed coordination (Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secondary_recovery_ceiling, empirical, 'Physical/economic ceiling on secondary silver recovery').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(silver_scarcity_mountain_2026, 0, 26).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(silv_tr_t0, silver_scarcity_mountain_2026, theater_ratio, 0, 0.12).
narrative_ontology:measurement(silv_tr_t13, silver_scarcity_mountain_2026, theater_ratio, 13, 0.14).
narrative_ontology:measurement(silv_tr_t26, silver_scarcity_mountain_2026, theater_ratio, 26, 0.15).

% Extraction over time
narrative_ontology:measurement(silv_be_t0, silver_scarcity_mountain_2026, base_extractiveness, 0, 0.16).
narrative_ontology:measurement(silv_be_t13, silver_scarcity_mountain_2026, base_extractiveness, 13, 0.18).
narrative_ontology:measurement(silv_be_t26, silver_scarcity_mountain_2026, base_extractiveness, 26, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(silver_scarcity_mountain_2026, global_infrastructure).
narrative_ontology:affects_constraint(silver_scarcity_mountain_2026, photovoltaic_manufacturing_bottleneck).
narrative_ontology:affects_constraint(silver_scarcity_mountain_2026, semiconductor_supply_chain_criticality).
narrative_ontology:affects_constraint(silver_scarcity_mountain_2026, national_energy_independence_transition).

% DUAL FORMULATION NOTE:
% Silver scarcity is upstream of multiple industrial and geopolitical constraints. The physical scarcity (this story, ε=0.18, Mountain) affects how downstream constraints (PV manufacturing bottlenecks, semiconductor supply competition, national energy policy) are structured and negotiated. The scarcity itself is a mountain; how institutions respond to it (rationing, substitution investment, security stockpiling) may generate tangled ropes or snares.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
