% ============================================================================
% CONSTRAINT STORY: interstate_commerce_friction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_interstate_commerce_friction, []).

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
 *   constraint_id: interstate_commerce_friction
 *   human_readable: Interstate Commerce Friction and Regulatory Arbitrage
 *   domain: economic/political
 *
 * SUMMARY:
 *   Interstate commerce friction creates a hybrid coordination-extraction
 *   constraint within federal systems where capital mobility, regulatory
 *   autonomy, and labor immobility interact. The constraint operates as
 *   genuine coordination (states compete to attract business, creating
 *   efficiency pressures and fiscal discipline) alongside systematic
 *   extraction (immobile workers and residents bear costs of regulatory
 *   arbitrage while mobile capital captures benefits). The mechanism has
 *   strengthened over the measured interval (1980–2020) as capital
 *   globalization has increased firm mobility, international supply chains
 *   have reduced location lock-in, and regulatory variation has accumulated
 *   through state-level policy divergence. Theater ratio reflects the gap
 *   between formal legal doctrine (Interstate Commerce Clause nominally
 *   prevents discriminatory restriction) and actual practice (states
 *   effectively compete through permissive regulation framed as neutral
 *   environmental/health policy). The constraint exhibits all six DR types
 *   from different structural positions: pure extraction (Snare) for immobile
 *   workers, coordination-extraction hybrid (Tangled Rope) for jurisdictions
 *   and high-regulation states, pure coordination (Rope) for mobile capital,
 *   temporary scaffolding (Scaffold) from federal regulatory perspective,
 *   degraded doctrine (Piton) from constitutional law perspective, and false
 *   natural law (Mountain) from civilizational analytical view.
 *
 * KEY AGENTS:
 *   - Immobile Workers: Primary victims (powerless/trapped) — cannot relocate; bear full extraction cost through wage depression, safety degradation, and fiscal erosion
 *   - Mobile Capital Firms: Primary beneficiaries (institutional/arbitrage) — can locate across jurisdictions; capture tax benefits and regulatory flexibility; experience low extraction
 *   - Low-Enforcement Jurisdictions: Secondary beneficiary/victim (moderate/constrained) — benefit from capital inflow but bear environmental and health costs; caught in competitive trap
 *   - High-Enforcement Jurisdictions: Secondary victim (institutional/constrained) — maintain standards but experience business flight and fiscal pressure; competitive constraint prevents raising standards further
 *   - Federal Regulatory Coalition: Organized actor (organized/constrained) — federal labor, environmental, and consumer protection standards establish floor but limited enforcement capacity against state variation
 *   - Interstate Commerce Clause Doctrine: Institutional framework (institutional/arbitrage) — nominally prevents discriminatory state regulation; in practice performative as courts rarely block regulatory variation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing policy-contingent features as immutable laws of federalism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(interstate_commerce_friction, 0.52).
domain_priors:suppression_score(interstate_commerce_friction, 0.48).
domain_priors:theater_ratio(interstate_commerce_friction, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(interstate_commerce_friction, extractiveness, 0.52).
narrative_ontology:constraint_metric(interstate_commerce_friction, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(interstate_commerce_friction, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(interstate_commerce_friction, tangled_rope).
narrative_ontology:human_readable(interstate_commerce_friction, "Interstate Commerce Friction and Regulatory Arbitrage").
narrative_ontology:topic_domain(interstate_commerce_friction, "economic/political").

domain_priors:requires_active_enforcement(interstate_commerce_friction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(interstate_commerce_friction, capital_mobile_firms).
narrative_ontology:constraint_beneficiary(interstate_commerce_friction, favorable_regulatory_jurisdictions).
narrative_ontology:constraint_victim(interstate_commerce_friction, immobile_workers).
narrative_ontology:constraint_victim(interstate_commerce_friction, unfavorable_regulatory_jurisdictions).
narrative_ontology:constraint_victim(interstate_commerce_friction, consumer_protection_standards).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IMMOBILE WORKER (SNARE) — Local labor supply cannot exit or relocate without catastrophic personal cost. Experiences full extraction: wage depression from regulatory arbitrage, workplace safety degradation as firms migrate to low-enforcement jurisdictions, pension underfunding as tax bases erode. No alternatives within viable exit horizon. Maximum experienced extraction.
constraint_indexing:constraint_classification(interstate_commerce_friction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOW-ENFORCEMENT JURISDICTION (TANGLED ROPE) — Benefits from attracting capital inflow and tax revenue through permissive regulation, but bears structural extraction through environmental degradation, workforce health costs, and capacity strain. Genuine coordination function (business attraction mechanism) alongside asymmetric extraction (externalizes costs to residents and environment). Constrained by fiscal competition and capital mobility.
constraint_indexing:constraint_classification(interstate_commerce_friction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MOBILE CAPITAL FIRM (ROPE) — Experiences the constraint as pure coordination: choosing jurisdiction location that optimizes regulatory alignment. High exit options (can operate across multiple jurisdictions, relocate efficiently). Net beneficiary — extraction runs toward this agent through tax breaks and regulatory flexibility. Sees coordination problem solved by interstate commercial freedom.
constraint_indexing:constraint_classification(interstate_commerce_friction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FEDERAL REGULATORY COALITION (SCAFFOLD) — Federal labor, environmental, and consumer protection mandates represent temporary coordination architecture with sunset logic: federal standards set a floor that prevents race-to-the-bottom while allowing state variation. Coalition sees the mechanism as scaffolding — temporary support until uniform national standards normalize, reducing enforcement variation. Has sunset clause embedded in regulatory philosophy (as federal minimums become cultural baseline, state competition for laxity declines).
constraint_indexing:constraint_classification(interstate_commerce_friction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERSTATE COMMERCE CLAUSE DOCTRINE (PITON) — Constitutional framework that nominally prevents discriminatory state regulation appears to function as verification of commercial freedom, but in practice is largely performative: courts rarely block state regulatory variation when framed as environmental/health protection rather than explicit commerce restriction. The doctrine persists through institutional inertia and legal tradition despite low actual constraint effectiveness. Theater ratio reflects gap between constitutional ambition and enforcement capacity.
constraint_indexing:constraint_classification(interstate_commerce_friction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: HIGH-ENFORCEMENT JURISDICTION (TANGLED ROPE) — Maintains strong labor and environmental standards (coordination function: establishes baseline for worker/environmental protection), but experiences extraction through business relocation to low-enforcement competitors. Constrained by competition for capital; cannot unilaterally raise standards without losing employers. Experiences both genuine coordination (protecting residents) and asymmetric extraction (bearing costs of maintaining standards while competitors free-ride).
constraint_indexing:constraint_classification(interstate_commerce_friction, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / CAPITAL MOBILITY VIEW (MOUNTAIN) — From a civilizational perspective, regulatory friction is an inherent property of federalist systems: capital mobility combined with jurisdiction-specific law creates inevitable arbitrage. This perspective sees the constraint as a natural law of political economy — immutable as long as federalism and capital markets exist. However, the structural data reveals this as a false summit: the 'immutability' naturalizes policy choices (enforcement discretion, tax incentive structures, labor law variation) that are contingent.
constraint_indexing:constraint_classification(interstate_commerce_friction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(interstate_commerce_friction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(interstate_commerce_friction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(interstate_commerce_friction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(interstate_commerce_friction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(interstate_commerce_friction, TR),
    TR >= 0.70.

:- end_tests(interstate_commerce_friction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The original research identified this at 0.35 (2010 assessment), but capital mobility has increased significantly with globalization and supply chain restructuring. Current value reflects genuine extraction: capital can relocate more easily than labor, creating sustained asymmetry in who captures regulatory benefits vs bears regulatory costs. However, extraction is not maximal because federal floor prevents total race-to-bottom and consumer/worker preferences create some counterweight. Suppression (0.48): Moderate. Significant barriers to exit include: relocation costs for workers, geographic root attachments (family, community, property), information asymmetry about standard variation, and interstate mobility restrictions (professional licensure, welfare eligibility). But not total — some workers do migrate, and capital mobility is substantial. Theater ratio (0.58): Moderate-high. Interstate Commerce Clause doctrine appears to constrain state regulatory variation but provides little actual constraint — courts rarely strike down state regulation when framed as environmental/health protection rather than explicit commerce restriction. The constitutional theater has expanded over time as regulatory variation has increased despite the nominally constraining doctrine. Measurements show steady increase in both extractiveness and theater over the 40-year interval, indicating: (a) capital becoming more mobile, (b) states competing more aggressively through regulatory variation, (c) federal enforcement capacity lagging behind regulatory scope.
 *
 * PERSPECTIVAL GAP:
 *   The widest perspectival gap lies between the mobile capital perspective (Rope—pure coordination) and the immobile worker perspective (Snare—pure extraction). Both observe the same mechanism (jurisdictional regulatory variation), but their exit capacities are inverted. This gap is not resolvable by better information or framing — it reflects structural reality: the constraint simultaneously solves a coordination problem (how do jurisdictions attract business?) and creates an extraction problem (how do residents escape regulatory degradation?). The federal scaffold perspective claims the gap is temporary — federal floor + state variation is a developmental scaffolding that will sunset as standards converge and capital location becomes less sensitive to regulatory variation. The piton perspective argues the gap is stable — interstate commerce doctrine persists through institutional inertia despite low functional constraint, and the competitive dynamic remains trapped in equilibrium.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness (χ) derives from the agent's structural position. Mobile capital with arbitrage exit options (d ≈ 0.15) experiences low chi even though base extractiveness is moderate — they can walk away from unfavorable regulation. Immobile workers with trapped exit (d ≈ 0.95) experience maximum chi — they cannot escape regulatory degradation. High-enforcement jurisdictions with constrained exit (d ≈ 0.65) experience chi scaled upward from base extractiveness because they cannot unilaterally prevent capital flight. Federal regulators with organizing power but constrained enforcement capacity (d ≈ 0.50) experience moderate chi. The directionality derivation reveals the core asymmetry: the constraint's structure makes exit options inversely correlated with extraction burden. Those who can exit (capital) benefit; those who cannot (workers, place-bound communities) bear costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy through structural decomposition: the same mechanism is coordination (for capital) and extraction (for workers) simultaneously because their structural positions are inverted relative to the constraint. The risk of false dichotomy arises at the analytical level: is federalism an immutable natural law (mountain) or a policy-contingent arrangement? The false summit is naturalization of policy choices. Federal enforcement discretion, tax incentive structures, professional licensure reciprocity agreements, and labor law harmonization are all policy-contingent; they have been chosen to enable regulatory arbitrage rather than constrain it. The 'natural law' perspective treats these as unavoidable features of federalism, missing that other federal systems (Canada, EU, Australia) have made different choices that reduce arbitrage. The mandatrophy resolves by recognizing that the constraint is Tangled Rope from the systems perspective (genuine coordination function + asymmetric extraction), with local manifestations as Snare (workers), Rope (capital), and Scaffold (federal regulators) depending on position and time horizon.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capital_mobility_measurement,
    'Is the effective capital mobility driving arbitrage truly structural, or does it reflect institutional choices (tax treaty enforcement, labor law variation, environmental licensing) that could be harmonized?',
    'Comparative institutional analysis: cross-national federalist systems (Canada, EU, Australia) that have reduced regulatory arbitrage through harmonization; measurement of relocation costs vs regulatory savings to establish lower bound on true mobility',
    'If mobility is structural: constraint approaches mountain (immutable feature of federalism). If mobility reflects institutional choices: constraint is Tangled Rope throughout (policy-amenable through coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_mobility_measurement, empirical, 'Whether capital mobility is structural or reflects institutional choices').

omega_variable(
    race_to_bottom_inevitability,
    'Does regulatory competition between jurisdictions necessarily produce race-to-the-bottom, or can competitive equilibrium sustain baseline standards through reputational and consumer preference mechanisms?',
    'Historical analysis of jurisdictions that maintained high standards despite competitive pressure (German labor law, Scandinavian environmental standards, California emissions regulations); identification of conditions that enable high-standard competitive equilibrium',
    'If race-to-bottom is inevitable: suppression value should increase to 0.65+, classification shifts toward Snare. If equilibrium possible: suppression may decrease to 0.35-0.40, scaffold sunset logic strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(race_to_bottom_inevitability, empirical, 'Whether regulatory competition necessarily produces race-to-the-bottom').

omega_variable(
    federal_preemption_credibility,
    'Can federal regulatory floor actually constrain state competition, or does federal enforcement capacity lag far behind the scale of regulatory variation being created?',
    'Audit of federal enforcement resources vs regulatory scope; measurement of de facto variation in labor standards, environmental enforcement, and consumer protection despite federal floor; analysis of regulatory arbitrage instances that federal authority failed to constrain',
    'If federal floor is credible: scaffold mechanism works as intended, sunset logic is operational. If enforcement capacity is minimal: federal floor is theater (Piton), and the constraint is effectively unscaled extraction (Snare from many perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federal_preemption_credibility, empirical, 'Whether federal regulatory floor effectively constrains state competition').

omega_variable(
    consumer_preference_constraint,
    'Do consumer and worker preferences for high-standard jurisdictions create competitive pressure that sustains standards despite capital flight incentives, or does information asymmetry and convenience override these preferences?',
    'Consumer choice analysis: willingness-to-pay studies for products from high-standard jurisdictions; labor market analysis of premium wages in high-regulation areas; tracking of corporate social responsibility claims vs actual regulatory compliance',
    'If preferences constrain extraction: scaffold mechanism strengthened, piton classification weakened. If preferences are weak: mechanism is primarily top-down (federal preemption) or fails entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_preference_constraint, empirical, 'Whether consumer/worker preferences constrain regulatory arbitrage').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(interstate_commerce_friction, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(icf_tr_t0, interstate_commerce_friction, theater_ratio, 0, 0.42).
narrative_ontology:measurement(icf_tr_t20, interstate_commerce_friction, theater_ratio, 20, 0.5).
narrative_ontology:measurement(icf_tr_t40, interstate_commerce_friction, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(icf_be_t0, interstate_commerce_friction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(icf_be_t20, interstate_commerce_friction, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(icf_be_t40, interstate_commerce_friction, base_extractiveness, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(interstate_commerce_friction, resource_allocation).
narrative_ontology:boltzmann_floor_override(interstate_commerce_friction, 0.18).
narrative_ontology:affects_constraint(interstate_commerce_friction, regulatory_capture_state_level).
narrative_ontology:affects_constraint(interstate_commerce_friction, labor_market_segmentation).
narrative_ontology:affects_constraint(interstate_commerce_friction, fiscal_federalism_trap).

% DUAL FORMULATION NOTE:
% Interstate commerce friction is upstream of regulatory capture mechanisms and labor market segmentation. The structural capital mobility that drives arbitrage creates conditions for state-level capture (low-regulation jurisdictions become dependent on extractive industries) and segmented labor markets (high-wage high-regulation centers, low-wage low-regulation peripheries). Network decomposition recommended: separate stories for regulatory capture within permissive jurisdictions and labor market segmentation effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(interstate_commerce_friction, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
