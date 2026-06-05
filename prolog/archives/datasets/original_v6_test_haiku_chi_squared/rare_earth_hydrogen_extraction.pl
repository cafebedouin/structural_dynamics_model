% ============================================================================
% CONSTRAINT STORY: rare_earth_hydrogen_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rare_earth_hydrogen_extraction, []).

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
 *   constraint_id: rare_earth_hydrogen_extraction
 *   human_readable: Rare Earth Element Dependency for Core Hydrogen Extraction
 *   domain: economic/technological/geopolitical
 *
 * SUMMARY:
 *   The rare earth element dependency for core hydrogen extraction creates a
 *   structural constraint where access to a theorized energy source (core
 *   hydrogen) is contingent on control of elemental supply chains. This
 *   constraint exhibits a snare structure with tangled coordination elements
 *   at the geopolitical level. Rare earth elements (REEs) are essential for
 *   the advanced extraction technologies required to access hydrogen from
 *   Earth's core — including magnetic pumps, catalytic converters,
 *   extreme-temperature alloys, and sensor systems. No established
 *   alternative pathway exists. The geopolitical dimension introduces a
 *   secondary extraction layer: nations controlling REE deposits (or
 *   synthetic production capacity) can extract rents from all entities
 *   pursuing hydrogen extraction pathways. The constraint intensifies over
 *   time as hydrogen extraction infrastructure locks in REE-dependent
 *   designs, raising exit costs for downstream economies. The theater ratio
 *   (0.58) reflects that much discussion of REE dependency framed as
 *   'technical necessity' disguises contingent technology choices:
 *   alternative materials and direct extraction pathways exist but are more
 *   expensive or unproven. Synthetic REE production and recycling
 *   infrastructure represent potential sunsets, but their timeline and cost
 *   competitiveness remain uncertain.
 *
 * KEY AGENTS:
 *   - Hydrogen Extraction Industries: Primary victim (moderate/constrained) — must source REEs or redesign technology; faces extraction rents and supply disruption risk
 *   - Energy Transition Dependent Economies: Primary victim (powerless/trapped) — national-level energy independence constrained by REE monopolies; no immediate exit options
 *   - Rare Earth Monopolists: Primary beneficiary (institutional/arbitrage) — control supply of critical inputs; capture extraction rents from hydrogen infrastructure buildout
 *   - Geopolitical Hegemons: Secondary beneficiary (powerful/mobile) — gain geopolitical leverage through REE control; benefit domestically from hydrogen transition narrative; have mobile alternatives and alliance options
 *   - REE Recycling/Substitution Coalition: Organized agents (organized/constrained) — scientists, policy coalitions, alternative materials firms building exit pathways; see sunset logic in synthetic REE and REE-free technologies
 *   - Incumbent Energy Infrastructure: Institutional actor (institutional/constrained) — maintains theater around hydrogen as future technology without transition commitment; REE dependency serves justification for delay
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent technology choices as immutable thermodynamic requirements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rare_earth_hydrogen_extraction, 0.68).
domain_priors:suppression_score(rare_earth_hydrogen_extraction, 0.72).
domain_priors:theater_ratio(rare_earth_hydrogen_extraction, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rare_earth_hydrogen_extraction, extractiveness, 0.68).
narrative_ontology:constraint_metric(rare_earth_hydrogen_extraction, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(rare_earth_hydrogen_extraction, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rare_earth_hydrogen_extraction, snare).
narrative_ontology:human_readable(rare_earth_hydrogen_extraction, "Rare Earth Element Dependency for Core Hydrogen Extraction").
narrative_ontology:topic_domain(rare_earth_hydrogen_extraction, "economic/technological/geopolitical").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rare_earth_hydrogen_extraction, rare_earth_monopolists).
narrative_ontology:constraint_beneficiary(rare_earth_hydrogen_extraction, geopolitical_hegemons).
narrative_ontology:constraint_victim(rare_earth_hydrogen_extraction, hydrogen_extraction_industries).
narrative_ontology:constraint_victim(rare_earth_hydrogen_extraction, energy_transition_dependent_economies).
narrative_ontology:constraint_victim(rare_earth_hydrogen_extraction, developing_nations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENERGY TRANSITION DEPENDENT ECONOMY (SNARE) — Nations pursuing hydrogen-based energy systems face irreversible dependency on REE supply chains. No substitute extraction technologies exist; switching costs are prohibitive. Trapped within the constraint by energy infrastructure sunk costs. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.63.
constraint_indexing:constraint_classification(rare_earth_hydrogen_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: HYDROGEN EXTRACTION TECHNOLOGY DEVELOPER (SNARE) — Constrained by REE availability and pricing. Must design around scarcity or pay extraction rents. Cannot develop truly independent hydrogen pathways without fundamental technology redesign. d≈0.78, f(d)≈1.08, σ=1.0 → χ≈0.73.
constraint_indexing:constraint_classification(rare_earth_hydrogen_extraction, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RARE EARTH MONOPOLIST (ROPE) — Controls supply of critical inputs. Experiences the constraint as coordination: managing supply signals to stabilize prices and secure long-term contracts. Arbitrage access allows switching between hydrogen extraction buyers and other REE consumers. d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(rare_earth_hydrogen_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GEOPOLITICAL HEGEMON (TANGLED ROPE) — Domestically benefits from hydrogen transition (energy independence narrative). Internationally exploits REE dependency of competitors (geopolitical leverage). Has mobile exit options: can develop synthetic REEs, substitute materials, or ally with other REE suppliers. Sees both coordination (domestic transition) and extraction (international leverage). d≈0.42, f(d)≈0.42, σ=1.1 → χ≈0.31.
constraint_indexing:constraint_classification(rare_earth_hydrogen_extraction, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: REE RECYCLING/SUBSTITUTION COALITION (SCAFFOLD) — Organized actors (researchers, policy coalitions, alternative materials scientists) are building exit pathways through recycled-REE systems and REE-free extraction technologies. See the dependency as a temporary coordination failure with a sunset: direct hydrogen extraction (not technology-dependent methods), recycling infrastructure, and synthetic REE alternatives mature over 15-30 years. Has sunset logic. d≈0.38, f(d)≈0.38, σ=1.2 → χ≈0.17.
constraint_indexing:constraint_classification(rare_earth_hydrogen_extraction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INCUMBENT ENERGY INFRASTRUCTURE (PITON) — Traditional fossil fuel and nuclear energy systems maintain their own theater around hydrogen as a 'bridge fuel' and 'future technology' without committing to infrastructure transition. REE dependency serves as a convenient institutional justification for delaying deployment. theater_ratio=0.58 reflects mixed performative and functional activity. The constraint persists through inertia and misaligned incentives rather than true technical necessity.
constraint_indexing:constraint_classification(rare_earth_hydrogen_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a pure thermodynamic/materials science perspective, some extraction technologies require specific elemental properties (magnetic moments, catalytic surfaces, structural stability at extreme conditions). These properties cluster in the lanthanide and transition metal series. This perspective risks naturalizing the constraint as an immutable physical law. However, the base properties (ε=0.68, suppression=0.72, theater=0.58) contradict mountain classification — the engine will flag this as a false summit, revealing that the 'thermodynamic necessity' framing disguises contingent technology choices and geopolitical opportunity.
constraint_indexing:constraint_classification(rare_earth_hydrogen_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rare_earth_hydrogen_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rare_earth_hydrogen_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rare_earth_hydrogen_extraction, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rare_earth_hydrogen_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rare_earth_hydrogen_extraction, TR),
    TR >= 0.70.

:- end_tests(rare_earth_hydrogen_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The dependency extraction is substantial — REE monopolists and hegemons capture rents that rise over time as hydrogen infrastructure scales. The extractiveness is not maximal (0.95+) because some technological alternatives exist (synthetic REEs, direct extraction pathways) and deployment is not yet global. The trajectory shows extractiveness rising from 0.42 (early research phase) to 0.68 (infrastructure lock-in phase), indicating deepening dependency as systems are built. Suppression (0.72): High. Multiple barriers prevent exit: (1) no proven alternative extraction technologies at scale, (2) REE supply is geopolitically concentrated (China dominates primary production), (3) recycling infrastructure is nascent, (4) sunk costs in REE-dependent designs make retrofitting expensive, (5) strategic reserves are controlled by hegemons. Suppression rises as infrastructure locks in. Theater ratio (0.58): Moderate-high. Much discourse around 'thermodynamic necessity' of REEs in hydrogen extraction masks engineering choices and geopolitical opportunity. Some theater is performative (incumbent energy infrastructure using REE dependency as excuse for non-transition), some is functional (genuine technical challenges in designing REE-free systems). The ratio rises from 0.35 (early phase, genuine uncertainty) to 0.58 (infrastructure phase, more defensive rhetorical framing).
 *
 * PERSPECTIVAL GAP:
 *   The rare earth hydrogen extraction constraint demonstrates a stark perspectival gap between victims and beneficiaries. Hydrogen extraction industries and energy-dependent economies see a snare — they are trapped by supply constraints and extraction rents with no exit path. Rare earth monopolists and geopolitical hegemons see coordination or negotiation (rope/tangled_rope) — they experience the constraint as a mechanism for maintaining stable supply relationships and leveraging strategic advantage. The recycling/substitution coalition sees a temporary problem with a sunset (scaffold) — they have agency and see a 20-30 year path to independence. The incumbent energy infrastructure sees theater (piton) — they use REE dependency as rhetorical cover for delayed transition. The analytical observer risks seeing a physical law (mountain) — that hydrogen extraction 'requires' REEs — but the structural data reveals this as a contingent technology/geopolitical lock-in masquerading as necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Hydrogen extraction industries: Victim + constrained → d≈0.78, f(d)≈1.08. High extraction. Must redesign or pay rents. Energy transition dependent economies: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction from this perspective. No mobile alternatives; sunk infrastructure costs. Rare earth monopolists: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary. Control supply and can shift rents between hydrogen and other REE markets. Geopolitical hegemons: Beneficiary + mobile → d≈0.42, f(d)≈0.42. Mixed coordination-extraction. Can shift alliances, develop synthetic alternatives, or maintain monopoly leverage. REE recycling coalition: Organized + constrained → d≈0.38, f(d)≈0.38. Low effective extraction due to agency and visible exit path. Incumbent energy infrastructure: Institutional + constrained → d≈0.55, f(d)≈0.75. Uses theater to manage extraction. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is a false summit — naturalizes contingent choices.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The rare earth hydrogen extraction constraint resolves the snare/coordination ambiguity through asymmetry analysis. At the industry level (hydrogen extractors), the constraint is a snare — pure extraction with minimal coordination benefit. REE suppliers do not solve the hydrogen industry's coordination problems; they exploit them. At the geopolitical level (hegemons), the constraint has tangled properties — it provides leverage in alliance-building (coordination) while enabling extraction (asymmetric rents). At the analytical level, the constraint appears natural only if one naturalizes technology choices. The constraint family decomposes into: (1) rare_earth_technology_dependency (ε≈0.42, snare in early research phase), (2) rare_earth_supply_monopoly (ε≈0.68, pure snare in infrastructure phase), and (3) hydrogen_extraction_direct_feasibility (ε≈0.05 if feasible, ε≈0.85 if infeasible — a mountain-or-snare depending on physics, not geopolitics). The mandatrophy is resolved by refusing to collapse these distinct constraints into one false summit. ε=0.68 for supply monopoly is legitimately snare. No contradiction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    direct_hydrogen_extraction_feasibility,
    'Can direct hydrogen extraction from Earth''s core proceed without advanced REE-dependent extraction technology, or is REE dependency intrinsic to accessing that depth/pressure regime?',
    'Experimental validation of alternative bore-hole and fluid-pumping technologies; thermodynamic modeling of non-REE material performance at required conditions; prototype testing of composite or novel alloy alternatives',
    'If feasible: REE dependency is contingent engineering choice (supports snare classification — extraction mechanism exploitable). If infeasible: REE dependency approaches mountain status (fundamental constraint on hydrogen extraction pathway). Would shift analytical observer perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(direct_hydrogen_extraction_feasibility, empirical, 'Whether direct hydrogen extraction requires REE-dependent technology').

omega_variable(
    synthetic_ree_cost_parity_timeline,
    'When will synthetic/recycled REE production achieve cost parity with mined REEs, enabling bypass of geopolitical extraction rents?',
    'Cost curve analysis of synthetic REE production; tracking of recycling infrastructure deployment; market price data for recycled REE commodities vs primary REE',
    'If parity achieved by 2035: scaffold sunset is realistic, snare classification temporary. If delayed past 2045: scaffold perspective is aspirational, snare persists as structural feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(synthetic_ree_cost_parity_timeline, empirical, 'Timeline for synthetic/recycled REE cost parity').

omega_variable(
    geopolitical_cooperation_sustainability,
    'Can geopolitical hegemons maintain REE supply arrangements for hydrogen extraction competitors, or does geostrategic competition force supply weaponization?',
    'Historical analysis of REE supply disruptions; game-theoretic modeling of hegemon incentives; tracking of export restrictions and supply agreements; monitoring of alliance shifts',
    'If cooperation sustained: snare classification softens (moderate extraction rents rather than maximum). If weaponization occurs: snare intensifies, suppression rises toward 0.85+.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_cooperation_sustainability, conceptual, 'Whether geopolitical cooperation sustains REE supply for hydrogen competitors').

omega_variable(
    hydrogen_extraction_technology_lock_in,
    'Are hydrogen extraction systems so tightly designed around REE-dependent components that technology lock-in prevents switching to alternative architectures once deployed?',
    'Architectural analysis of deployed hydrogen extraction systems; assessment of component modularity and replaceBility; cost of retrofitting existing infrastructure for REE-free alternatives',
    'If lock-in severe: snare classification hardens (exit options degrade from constrained to trapped over 10-year horizon). If modular: mobile exit options increase, extraction rents compressed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hydrogen_extraction_technology_lock_in, empirical, 'Whether hydrogen extraction tech lock-in prevents alternative designs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rare_earth_hydrogen_extraction, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rehe_tr_t0, rare_earth_hydrogen_extraction, theater_ratio, 0, 0.35).
narrative_ontology:measurement(rehe_tr_t10, rare_earth_hydrogen_extraction, theater_ratio, 10, 0.48).
narrative_ontology:measurement(rehe_tr_t20, rare_earth_hydrogen_extraction, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(rehe_be_t0, rare_earth_hydrogen_extraction, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(rehe_be_t10, rare_earth_hydrogen_extraction, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(rehe_be_t20, rare_earth_hydrogen_extraction, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rare_earth_hydrogen_extraction, resource_allocation).
narrative_ontology:affects_constraint(rare_earth_hydrogen_extraction, lithium_battery_supply_dependency).
narrative_ontology:affects_constraint(rare_earth_hydrogen_extraction, semiconductor_rare_earth_supply).
narrative_ontology:affects_constraint(rare_earth_hydrogen_extraction, hydrogen_energy_transition_feasibility).

% DUAL FORMULATION NOTE:
% The rare earth hydrogen extraction constraint is part of a constraint family around rare earth dependencies across technologies. Upstream: semiconductor_rare_earth_supply (ε≈0.55, tangled_rope at industry level, snare at geopolitical level). Downstream: hydrogen_energy_transition_feasibility (ε≈0.62, snare if hydrogen pathways are REE-dependent, rope if alternatives scale). The ε values differ because (1) semiconductor REE dependency is older and partially substitutable, (2) hydrogen extraction REE dependency is newer and less architecturally flexible. These are distinct constraints linked by common beneficiary (REE monopolists) and common geopolitical mechanism (supply weaponization).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rare_earth_hydrogen_extraction, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
