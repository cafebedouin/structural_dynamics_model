% ============================================================================
% CONSTRAINT STORY: shipping_lane_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shipping_lane_control, []).

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
 *   constraint_id: shipping_lane_control
 *   human_readable: Shipping Lane Control and Maritime Coordination
 *   domain: geopolitical/economic/infrastructure
 *
 * SUMMARY:
 *   Shipping lane control represents a hybrid extraction-coordination
 *   mechanism operating at global scale. The constraint functions as both
 *   genuine public good provision (maritime security, piracy suppression,
 *   navigation standardization) and as rent extraction by hegemonic naval
 *   powers. Nations whose critical trade routes pass through geographical
 *   chokepoints (Strait of Malacca, Suez Canal, Taiwan Strait, Hormuz)
 *   experience maximum suppression and no viable exit options. The
 *   controlling maritime power (historically Britain, currently U.S.,
 *   potentially future China or multipolar coalition) extracts through
 *   mandatory routing requirements, harbor fees, and implicit threat of
 *   closure. The constraint exhibits temporal drift: extractiveness has
 *   increased over the past 40 years as chokepoint dependency has grown,
 *   while theater_ratio has declined (actual hegemon control has become more
 *   material and less dependent on international law theater). The emergence
 *   of multipolar maritime coalitions introduces scaffold dynamics with
 *   explicit sunset logic—as naval power distributes, unilateral control
 *   becomes unsustainable and shared coordination frameworks will replace
 *   hegemon-centric structures. The constraint simultaneously exhibits all
 *   six types from different structural positions, making it a diagnostic
 *   exemplar for how geopolitical power asymmetry manifests across the
 *   indexical space.
 *
 * KEY AGENTS:
 *   - Controlling Maritime Power: Primary beneficiary (institutional/arbitrage) — U.S. or Chinese naval hegemony; captures extraction via fees, routing control, and geopolitical leverage; can exit by accepting multipolarity compensation
 *   - Chokepoint-Dependent Nations: Primary victims (powerless/trapped) — landlocked or strait-dependent states; no viable exit without economic collapse; bear full suppression and extraction
 *   - Regional Commercial Fleets: Secondary victims (moderate/constrained) — shipping companies and port authorities; face mandatory routing and fees; have some agency through route diversification but constrained by cost
 *   - International Maritime Organization: Institutional actor (institutional/mobile) — maintains governance theater (UNCLOS, IMO standards); actual enforcement power minimal; sees own rules as performative constraints on real hegemon authority
 *   - Multipolar Coalition: Organized agents (organized/mobile) — China, India, EU, Russia building alternative maritime governance; have exit paths via corridor security and regional coalitions; pursuing scaffold with sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent geopolitical arrangement as inevitable law of power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shipping_lane_control, 0.58).
domain_priors:suppression_score(shipping_lane_control, 0.65).
domain_priors:theater_ratio(shipping_lane_control, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shipping_lane_control, extractiveness, 0.58).
narrative_ontology:constraint_metric(shipping_lane_control, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(shipping_lane_control, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shipping_lane_control, tangled_rope).
narrative_ontology:human_readable(shipping_lane_control, "Shipping Lane Control and Maritime Coordination").
narrative_ontology:topic_domain(shipping_lane_control, "geopolitical/economic/infrastructure").

domain_priors:requires_active_enforcement(shipping_lane_control).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shipping_lane_control, controlling_maritime_power).
narrative_ontology:constraint_beneficiary(shipping_lane_control, regional_hegemon).
narrative_ontology:constraint_victim(shipping_lane_control, dependent_trading_nations).
narrative_ontology:constraint_victim(shipping_lane_control, global_supply_chain).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LANDLOCKED/STRAIT-DEPENDENT NATION (SNARE) — Nations whose trade routes pass through chokepoints (Strait of Malacca, Suez Canal, Taiwan Strait, Hormuz) experience maximum extraction. No exit option exists without routing costs that eliminate economic viability. Bearing full suppression and extraction with no coordination benefit. Control regime persists through implicit threat of closure.
constraint_indexing:constraint_classification(shipping_lane_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL COMMERCIAL FLEET (TANGLED ROPE) — Shipping companies operating in controlled lanes face mandatory routing, harbor fees, and security charges. Coordination function exists (safe passage, piracy suppression, navigation standards), but extraction is embedded — fees subsidize controlling power's naval infrastructure. Exit via alternative routes is constrained by cost and time. Both coordination and asymmetric cost-sharing present.
constraint_indexing:constraint_classification(shipping_lane_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CONTROLLING MARITIME POWER (ROPE) — The naval hegemon (U.S., China, or regional power) perceives the constraint as pure coordination: maintaining safe passage, suppressing piracy, enforcing maritime law, preventing weapons proliferation. Extraction flows toward this agent, but they frame it as legitimate fee-for-service coordination. Arbitrage exit available (can abandon policing if compensated; redeployment cost is acceptable).
constraint_indexing:constraint_classification(shipping_lane_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL MARITIME ORGANIZATION (PITON) — IMO standards, UNCLOS conventions, and international maritime law create a governance theater that persists despite minimal enforcement. The rules are largely performative — actual control is exercised by hegemons through naval presence, not by international agreement. The institutional apparatus maintains legitimacy through ritual compliance while real power operates through military capability. Theater ratio high because the formal international system claims authority it does not exercise.
constraint_indexing:constraint_classification(shipping_lane_control, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: MULTIPOLAR COALITION (SCAFFOLD) — Emerging naval powers (China, India, EU, Russia) are building alternative maritime governance structures (Belt and Road corridor security, Shanghai Cooperation Organization naval coordination, regional coalitions). These alternatives have explicit sunset logic: as multipolarity increases, unilateral control becomes unsustainable, and shared coordination frameworks replace hegemon-centric extraction. Organized agents with clear exit paths.
constraint_indexing:constraint_classification(shipping_lane_control, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / GEOPOLITICAL INEVITABILITY (MOUNTAIN) — From a civilizational view, sea lane control is treated as an immutable law of geopolitics: whoever commands naval power controls trade routes, always has, always will. This perspective naturalizes what is actually a contingent institutional and technological arrangement. The engine will identify this as a false summit — the structural data shows this is a Tangled Rope/Snare hybrid, not a natural law.
constraint_indexing:constraint_classification(shipping_lane_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shipping_lane_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(shipping_lane_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(shipping_lane_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(shipping_lane_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(shipping_lane_control, TR),
    TR >= 0.70.

:- end_tests(shipping_lane_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint combines genuine coordination costs (naval security infrastructure averages 3-5% of protected shipping value) with rent extraction (fees and indirect geopolitical leverage capture 8-12% of shipping value in critical chokepoints). The gap between coordination cost and extracted revenue defines the extraction magnitude. Extractiveness has increased over 40 years as dependency on chokepoint routes has grown (containerization, just-in-time supply chains, global trade concentration). Suppression (0.65): High. Dependent nations face material barriers (no alternative routes with comparable economics), legal barriers (UNCLOS enforces hegemon's 'freedom of navigation' in its preferred form), and political barriers (challenging hegemon invites economic retaliation). Suppression is not total—some multipolarity exists and alternatives are emerging—but it is substantial. Theater ratio (0.42): Moderate. The constraint operates more through material naval presence than through international law theater. IMO standards and UNCLOS conventions create nominal governance, but actual control is exercised through military capability and economic coercion, not through institutional agreement. Theater has decreased over time (more direct control, less pretense of international consensus) as hegemon power has consolidated. Claimed type Tangled Rope: The constraint exhibits both genuine coordination function (piracy suppression, standardized routing, maritime law enforcement) and asymmetric extraction (fees, geopolitical leverage, routing control). Both mechanisms are essential to the constraint's operation. Active enforcement required—chokepoint control is maintained through naval presence and implicit threat of closure.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between the controlling power (Rope) and dependent nations (Snare) is maximal—they see the same constraint as delivering opposite values. The gap between the piton perspective (institutional theater) and the snare perspective (material suppression) reveals the decoupling between formal governance claims and actual control mechanisms. The gap between the scaffold perspective (organized exit pathways forming) and the mountain perspective (geopolitical inevitability) reveals how different time horizons and power positions frame identical structural facts as either changeable or fixed.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from structural position: the beneficiary (controlling power) with arbitrage exit options derives low d; the trapped victim (dependent nation) derives high d (~0.95); the organized coalition with alternative routes derives moderate d (~0.55); the institutional observer with no real power derives high d in terms of powerlessness (~0.88). The piton classification emerges because the international maritime governance theater has low functional output—rules are maintained through inertia and legitimacy theater despite minimal enforcement mechanism. The scaffold classification for the multipolar coalition reflects genuine exit pathways (alternative route infrastructure, regional coordination agreements, technological alternatives) with explicit sunset logic as multipolarity increases. The false mountain at the analytical/civilizational level reveals how geopolitical claims to inevitability naturalize what is actually a contingent arrangement dependent on specific technological, institutional, and power configurations.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that Tangled Rope is the correct analytical classification because both coordination and extraction are structurally essential and causally interwoven. Pure Rope (coordination only) would fail to explain the suppression, fee extraction, and asymmetric cost distribution. Pure Snare (extraction only) would fail to explain the genuine maritime security provision and standardized routing benefits. The Tangled Rope classification captures that the constraint cannot be disaggregated into separate coordination and extraction mechanisms—they operate as a single structure where the coordination function legitimizes extraction and the extraction funds coordination. The false mountain classification at the analytical level serves diagnostic purpose: it reveals how civilizational-time-horizon observers are tempted to naturalize what is actually a contingent geopolitical arrangement dependent on specific naval technologies, chokepoint geography, and trade pattern concentrations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_boundary,
    'What portion of sea lane control is genuine public good provision (piracy suppression, navigation safety) versus rent extraction?',
    'Comparative analysis of service delivery: piracy rates, accident frequencies, response times in controlled vs uncontrolled lanes; economic modeling of coordination costs vs observed fees',
    'If coordination dominant (>60%): Rope/Scaffold classification strengthens, snare classification weakens. If extraction dominant (>60%): Snare/Tangled Rope classification strengthens. Currently estimated 45% coordination, 55% extraction-adjacent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Boundary between public good provision and rent extraction').

omega_variable(
    alternative_route_viability,
    'Are non-hegemon routes (Arctic passages, circum-Africa, east-west redistribution) technically viable and economically competitive within 10-20 years?',
    'Infrastructure assessment of Arctic icebreaker capacity, port development in alternative hubs, climate modeling for passage reliability, economic modeling of time/fuel costs vs toll extraction',
    'If viable and competitive: trapped/constrained exit options upgrade to mobile/arbitrage, shifting Snare toward Tangled Rope or Rope. If not viable: trap persists, Snare classification locks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_route_viability, empirical, 'Technical and economic viability of alternative shipping routes').

omega_variable(
    extraction_legitimacy_framing,
    'Is the hegemon''s framing of fees as ''coordination costs'' accepted by dependent nations as legitimate, or perceived as illegitimate rent extraction?',
    'Discourse analysis of trade negotiations, IMO discussions, and maritime policy statements; tracking of coalition formation (nations grouping together to resist vs accepting fees)',
    'If legitimized: Rope classification from multiple perspectives, constraint is stable. If delegitimized: Snare classification from dependent nations strengthens, coalition formation accelerates, scaffold exit becomes real.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_legitimacy_framing, conceptual, 'Legitimacy of extraction framing as coordination costs').

omega_variable(
    technology_disruption_timeline,
    'How will autonomous vessels, blockchain-based routing, and satellite monitoring alter control mechanisms? Will decentralization technology enable bypass?',
    'Technology roadmap assessment; modeling of autonomous vessel adoption rates; analysis of whether satellite monitoring creates transparency that delegitimizes hegemon control',
    'If technology enables distributed verification: Piton perspective strengthens (control theater becomes indefensible). If technology consolidates control: Snare persists or worsens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technology_disruption_timeline, empirical, 'Technology disruption of maritime control mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shipping_lane_control, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shipping_tr_t0, shipping_lane_control, theater_ratio, 0, 0.55).
narrative_ontology:measurement(shipping_tr_t20, shipping_lane_control, theater_ratio, 20, 0.47).
narrative_ontology:measurement(shipping_tr_t40, shipping_lane_control, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(shipping_be_t0, shipping_lane_control, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(shipping_be_t20, shipping_lane_control, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(shipping_be_t40, shipping_lane_control, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shipping_lane_control, global_infrastructure).
narrative_ontology:affects_constraint(shipping_lane_control, chokepoint_dependency).
narrative_ontology:affects_constraint(shipping_lane_control, geopolitical_leverage_asymmetry).
narrative_ontology:affects_constraint(shipping_lane_control, maritime_hegemony_cycles).

% DUAL FORMULATION NOTE:
% Shipping lane control can be decomposed into distinct constraints: (1) Maritime security coordination (ε~0.15, Rope) addressing piracy and accident prevention; (2) Chokepoint rent extraction (ε~0.72, Snare) capturing geopolitical leverage on dependent nations; (3) International maritime governance theater (ε~0.35, Piton) maintaining legitimacy through IMO/UNCLOS ritual. This story treats them as an integrated hybrid. Upstream constraints include specific territorial claims (Taiwan Strait, South China Sea) with their own extractiveness profiles; downstream effects cascade through global supply chains and geopolitical stability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shipping_lane_control, institutional, 0.1).
constraint_indexing:directionality_override(shipping_lane_control, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
