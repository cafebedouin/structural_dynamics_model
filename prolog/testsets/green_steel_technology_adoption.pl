% ============================================================================
% CONSTRAINT STORY: green_steel_technology_adoption
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_green_steel_technology_adoption, []).

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
 *   constraint_id: green_steel_technology_adoption
 *   human_readable: Green Steel Technology Adoption and Market Coordination
 *   domain: industrial_decarbonization/market_structure
 *
 * SUMMARY:
 *   Green steel technology adoption represents a global industrial
 *   decarbonization mandate with heterogeneous cost impacts across producer
 *   tiers and supply chain positions. The constraint exhibits the structural
 *   signature of tangled rope: a genuine coordination problem (fragmenting
 *   green steel variants, incompatible technology standards, supply chain
 *   integration costs) paired with asymmetric extraction (capital
 *   requirements and adoption timelines that burden small producers
 *   disproportionately while allowing incumbents to amortize costs across
 *   decades and capture carbon premium pricing). The extractiveness has
 *   increased over the 10-year interval (0.32 to 0.62) as policy timelines
 *   have tightened and technology costs have not yet fallen to competitive
 *   parity. Theater ratio remains moderate (0.35 to 0.51) because the
 *   underlying technology function is real—hydrogen reduction and
 *   electrification genuinely produce lower-carbon steel—though policy
 *   compliance reporting adds performative overhead. The constraint's
 *   lifecycle trajectory suggests a genuine sunset (scaffold perspective): as
 *   hydrogen production scales and electricity costs decline, cost parity
 *   should arrive by 2032-2035, allowing policy enforcement to shift from
 *   mandate to market incentive. However, this trajectory is
 *   fragile—technology lock-in, supply-chain bottlenecks, or carbon credit
 *   additionality failure could convert the scaffold into a permanent snare.
 *
 * KEY AGENTS:
 *   - Small Mills and Startups: Primary victim (powerless/trapped) — face capital barriers ($500M+ per facility) and cannot spread adoption across decades; experience pure extraction
 *   - Mid-Tier Steel Producers: Secondary victim (moderate/constrained) — benefit from coordination but bear disproportionate retrofitting costs relative to capital access
 *   - Incumbent Integrated Producers: Primary beneficiary (institutional/arbitrage) — capture carbon premium pricing and amortize R&D over long timeline; experience constraint as coordination
 *   - Downstream Industries (Automotive, Construction): Organized stakeholder (organized/constrained) — benefit from standardized supply but face cost transfer from upstream adoption timeline misalignment
 *   - Policy and Standards Bodies: Enforcement mechanism (organized/constrained) — create coordination through regulatory mandate with explicit sunset logic based on cost parity assumptions
 *   - Carbon Credit Trading System: Institutional subsystem (institutional/arbitrage) — provides financial mechanism but has degraded to theater (low additionality correlation)
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing policy-contingent timelines as thermodynamic immutability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(green_steel_technology_adoption, 0.58).
domain_priors:suppression_score(green_steel_technology_adoption, 0.62).
domain_priors:theater_ratio(green_steel_technology_adoption, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(green_steel_technology_adoption, extractiveness, 0.58).
narrative_ontology:constraint_metric(green_steel_technology_adoption, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(green_steel_technology_adoption, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(green_steel_technology_adoption, tangled_rope).
narrative_ontology:human_readable(green_steel_technology_adoption, "Green Steel Technology Adoption and Market Coordination").
narrative_ontology:topic_domain(green_steel_technology_adoption, "industrial_decarbonization/market_structure").

domain_priors:requires_active_enforcement(green_steel_technology_adoption).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(green_steel_technology_adoption, incumbent_steel_producers).
narrative_ontology:constraint_beneficiary(green_steel_technology_adoption, carbon_credit_traders).
narrative_ontology:constraint_beneficiary(green_steel_technology_adoption, technology_vendors).
narrative_ontology:constraint_victim(green_steel_technology_adoption, small_mills_and_startups).
narrative_ontology:constraint_victim(green_steel_technology_adoption, price_conscious_consumers).
narrative_ontology:constraint_victim(green_steel_technology_adoption, decarbonization_timeline).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL MILLS AND STARTUPS (SNARE) — Trapped by capital requirements ($500M+ per facility) and technology licensing costs; cannot exit without abandoning market participation. Face both technical barriers (unfamiliar processes, new supply chains) and financial barriers (upfront costs, unproven ROI). Experience the constraint as pure extraction: forced adoption timeline accelerates costs while incumbents amortize investment over decades.
constraint_indexing:constraint_classification(green_steel_technology_adoption, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-TIER PRODUCERS (TANGLED ROPE) — Constrained by capital access and technology lock-in, but also benefit from differential compliance costs. Can spread adoption over 10-15 years (generational horizon) unlike startups. Genuine coordination function exists (shared supply chains, technology standards reduce duplication). Asymmetric extraction emerges because largest producers internalize R&D costs while mid-tier pay licensing fees and retrofitting premiums.
constraint_indexing:constraint_classification(green_steel_technology_adoption, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: INCUMBENT INTEGRATED PRODUCERS (ROPE) — Experience constraint as pure coordination: standardizing on green steel reduces market fragmentation, enables amortization of R&D across decades, captures carbon premium pricing. Can arbitrage compliance through carbon credit markets and offset mechanisms. Net beneficiary — extraction flows toward this agent. The coordination function (unified technology standard, supply chain integration) is genuine and solves the problem of incompatible green steel variants.
constraint_indexing:constraint_classification(green_steel_technology_adoption, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DOWNSTREAM INDUSTRIES (TANGLED ROPE) — Organized collective (automotive OEMs, construction firms) benefit from standardized green steel supply (solves sourcing complexity) but face constrained exit: forced upstream adoption timelines increase steel costs before their own efficiency gains materialize. Genuine coordination function (unified sourcing, certification standards) alongside asymmetric cost transfer. Can negotiate but cannot exit the decarbonization pathway.
constraint_indexing:constraint_classification(green_steel_technology_adoption, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: POLICY AND STANDARDS BODIES (SCAFFOLD) — European Green Deal, Carbon Border Adjustment Mechanism, ISO decarbonization standards create temporary coordination through policy force. Theater ratio moderate (performance targets, compliance reporting) but coordination function is real (aligns fragmented markets toward common goal). Explicit sunset logic: as green steel becomes cost-competitive (estimated 2032-2035), policy enforcement can decline. Suppression through regulatory mandate is high now but should decline as technology matures and cost parity is achieved.
constraint_indexing:constraint_classification(green_steel_technology_adoption, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: CARBON CREDIT TRADING SYSTEM (PITON) — Institutional mechanism that has substantially degraded from its original function (pricing externalities). Now operates as extraction theater: producers can purchase offsets rather than emit less, paper compliance replaces real decarbonization. Theater ratio maintained by accounting conventions and regulatory arbitrage rather than functional need. Persists through institutional inertia — carbon markets are deeply embedded in financing and reporting systems despite weak correlation between trading activity and emissions reduction.
constraint_indexing:constraint_classification(green_steel_technology_adoption, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / THERMODYNAMIC VIEW (MOUNTAIN) — From a civilizational/universal perspective, the constraint derives from thermodynamic efficiency floors: green steel production via hydrogen reduction or electrification has inherent energy costs that cannot be eliminated without violating physical law. The constraint appears as an immutable property of chemistry and thermodynamics. However, this perspective risks false summitry — actual empirical costs are 20-30% above conventional steel, which is substantially a contingent economic choice (energy price assumptions, production scale, technology maturity) rather than thermodynamic immutability. The mountain classification should trigger false summit detection in the engine.
constraint_indexing:constraint_classification(green_steel_technology_adoption, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(green_steel_technology_adoption_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(green_steel_technology_adoption, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(green_steel_technology_adoption, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(green_steel_technology_adoption, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(green_steel_technology_adoption, TR),
    TR >= 0.70.

:- end_tests(green_steel_technology_adoption_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high and rising. The base extractiveness reflects two extraction mechanisms: (1) capital cost concentration that forces small producers into capital-intensive retrofits on accelerated timelines (genuine extraction), (2) asymmetric premium capture by incumbents during the cost-parity gap period (15+ years of carbon premium before technology cost parity). The rising trajectory (0.32→0.62) reflects accelerating policy timelines without corresponding cost reductions. Suppression (0.62): High. Barriers include: (a) technical barriers (unfamiliar processes, supply chain reconstruction), (b) capital barriers ($500M+ per facility, financing risk before ROI visibility), (c) regulatory barriers (carbon pricing creates effective exit cost for non-adopters), (d) market barriers (carbon premium pricing creates advantage for compliant producers, disadvantage for delayed adopters). Theater ratio (0.48, rising modestly): Moderate. The technology function is genuine—green steel production does reduce emissions—but policy compliance reporting (carbon accounting, offset claims, ESG reporting) adds performative content. Theater increases modestly over time as regulatory reporting overhead increases relative to technology deployment activity. The theater is not high enough to reclassify as piton (which requires ≥0.70), but the carbon credit subsystem (perspective 6) shows degraded theater (0.70+) where offset trading replaces real decarbonization.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximum perspectival variance. Incumbent producers (perspective 3) see pure coordination (rope)—standardizing green steel technology solves real market fragmentation. Policy bodies (perspective 5) see a temporary coordination problem with sunset (scaffold)—enforcement declining as cost parity arrives. Small producers (perspective 1) see pure extraction (snare)—forced adoption on timelines that their capital structure cannot support. The open question is whether the scaffold's sunset assumption (cost parity by 2032-2035) is realistic or aspirational. If cost parity arrives on schedule, extraction declines and the constraint genuinely transitions from snare→tangled rope→rope over the decade following parity. If cost parity is delayed to 2050+, the constraint remains snare indefinitely for small producers and the policy mechanism shifts from scaffold (sunset-capable) to piton (performative, inert). The analytical observer risks false summitry by treating the technological efficiency floor (hydrogen has minimum energy requirements) as immutable when in fact policy timelines and capital availability are the binding constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value (d) derives from their structural position in the extraction flow. Incumbent producers are beneficiaries with arbitrage options: they can adopt at their own pace, capture carbon premiums during the cost-parity gap, amortize investment across decades. Engine derives low d (→ 0.1-0.2), negative f(d), resulting in negative χ (extraction flows toward them). Small producers are victims with trapped exit: they cannot amortize investment across generations, face forced timelines, capture no premium (must pass cost to customers). Engine derives high d (→ 0.9), high f(d) ≈ 1.3+, resulting in high χ (maximum experienced extraction). Mid-tier producers are secondary victims with constrained exit: higher capital access than small mills but lower than incumbents, can negotiate timelines but cannot opt out, benefit partly from coordination but bear disproportionate retrofit costs relative to advantages. Engine derives moderate-high d (→ 0.65-0.75), moderate f(d) ≈ 0.95-1.15, resulting in moderate χ. Policy bodies enforce rather than experience extraction; they derive d → 0.5 (symmetric position) per canonical fallback.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that tangled rope (not pure rope, not pure snare) is the analytically correct classification for the full structural system. The temptation to mislabel it as pure coordination (rope) arises from the genuine coordination function: green steel standardization does solve market fragmentation and supply-chain inefficiency. But this coordination benefit is distributed asymmetrically—it accrues primarily to incumbents who can leverage existing capacity and capital access, while costs are pushed onto small producers via adoption timeline acceleration and capital concentration. The piton subsystem (carbon credit trading) is also important: it shows how coordination mechanisms can degrade into theater without the primary constraint changing type. The scaffold classification for policy bodies is crucial: it resists the temptation to classify the entire constraint as permanent extraction (which would suggest policy failure) while acknowledging the real risk that the sunset assumption (cost parity by 2032-2035) is uncertain. If the risk omega variables resolve negatively (cost parity delayed, lock-in fails, supply chain bottlenecks binding), the scaffold reverts to snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cost_parity_timeline,
    'When will green steel achieve cost parity with conventional steel absent subsidy or carbon pricing?',
    'Market price tracking; historical cost reduction curves for hydrogen production, electric arc furnace efficiency; capacity scaling analysis. Scenarios: 2030-2032 (rapid deployment), 2040+ (slow scaling), never (structural cost floor).',
    'If achieved by 2032: scaffold sunset is credible, constraint transitions to rope coordination. If delayed to 2045+: policy-enforced adoption extends indefinitely, constraint remains snare for small producers. If never achieved: constraint becomes permanent extraction mechanism, reclassifies as sustained snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_parity_timeline, empirical, 'Timeline for cost parity achievement under various deployment scenarios').

omega_variable(
    technology_lock_in_risk,
    'Does mandatory adoption of a single green steel technology (e.g., hydrogen reduction) create lock-in that prevents superior alternatives from emerging?',
    'Patent landscape analysis; technology diversity in global green steel projects; comparison to historical technology transitions (bessemer to open hearth, etc.). Detection: if adoption standardizes prematurely on suboptimal tech, later alternatives face massive switching costs.',
    'If lock-in occurs: constraint becomes permanent tangled rope (coordination function decreases as lock-in prevents optimization). If tech diversity preserved: constraint approaches rope (pure coordination). Critical period: next 3-5 years of deployment decisions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technology_lock_in_risk, empirical, 'Risk of premature technology standardization creating irreversible lock-in').

omega_variable(
    carbon_credit_additionality_failure,
    'Does the piton-classified carbon credit system actually reduce emissions or merely offset accounting without real decarbonization?',
    'Empirical tracking of offset project outcomes; comparison of traded carbon credits to actual emissions reductions in host regions; audits of additionality claims.',
    'If credits are non-additive (baseline offset that would happen anyway): entire carbon credit component of green steel incentive structure is theater without function. Reclassifies carbon credit piton as pure extraction theater. Forces policy to rely entirely on regulatory mandate, increasing suppression on producers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(carbon_credit_additionality_failure, empirical, 'Whether carbon credit offsets represent genuine emissions reductions or accounting theater').

omega_variable(
    supply_chain_bottleneck_severity,
    'Are hydrogen production capacity, electricity supply for electrolysis, or rare earth elements for equipment a binding constraint on green steel scaling?',
    'Capacity analysis: current renewable electricity vs green steel demand trajectory; hydrogen production infrastructure vs demand; equipment supply constraints from vendor capacity.',
    'If binding: adoption timeline is physically constrained regardless of policy/capital. Reclassifies constraint toward mountain (immutable supply-side limit). If non-binding: extraction is policy-driven, reclassifies toward snare/tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_chain_bottleneck_severity, empirical, 'Physical supply-side constraints on hydrogen, electricity, and equipment availability').

omega_variable(
    incumbent_technology_stranding,
    'Will forced early retirement of conventional steel capacity (not yet fully amortized) create massive stranded asset losses for incumbent producers?',
    'Financial analysis: sunk capital in conventional mills; useful life remaining vs policy retirement timelines; comparative depreciation rates for green vs conventional capacity.',
    'If massive stranding occurs: incumbent producers'' ''beneficiary'' status may be illusory — policy may force them to absorb write-downs. Redirects extraction toward shareholders/workers in legacy sectors. Tangled rope classification becomes more accurate — coordination benefit for industry as a whole paired with asymmetric cost to legacy asset holders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_technology_stranding, empirical, 'Extent of stranded asset losses from early conventional mill retirement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(green_steel_technology_adoption, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gsteel_tr_t0, green_steel_technology_adoption, theater_ratio, 0, 0.35).
narrative_ontology:measurement(gsteel_tr_t3, green_steel_technology_adoption, theater_ratio, 3, 0.4).
narrative_ontology:measurement(gsteel_tr_t6, green_steel_technology_adoption, theater_ratio, 6, 0.48).
narrative_ontology:measurement(gsteel_tr_t10, green_steel_technology_adoption, theater_ratio, 10, 0.51).

% Extraction over time
narrative_ontology:measurement(gsteel_be_t0, green_steel_technology_adoption, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(gsteel_be_t3, green_steel_technology_adoption, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(gsteel_be_t6, green_steel_technology_adoption, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(gsteel_be_t10, green_steel_technology_adoption, base_extractiveness, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(green_steel_technology_adoption, resource_allocation).
narrative_ontology:boltzmann_floor_override(green_steel_technology_adoption, 0.18).
narrative_ontology:affects_constraint(green_steel_technology_adoption, hydrogen_production_capacity).
narrative_ontology:affects_constraint(green_steel_technology_adoption, renewable_electricity_supply).
narrative_ontology:affects_constraint(green_steel_technology_adoption, carbon_pricing_mechanism).
narrative_ontology:affects_constraint(green_steel_technology_adoption, industrial_supply_chain_fragmentation).

% DUAL FORMULATION NOTE:
% Green steel adoption is downstream of hydrogen production capacity (ε≈0.25, constraint on supply) and renewable electricity supply (ε≈0.30, infrastructure constraint). Each constraint has independent extractiveness reflecting its own structural properties. Green steel adoption represents the junction where technology availability meets market adoption dynamics. Decomposed family: hydrogen_production_capacity (supply bottleneck), renewable_electricity_supply (infrastructure bottleneck), carbon_pricing_mechanism (market incentive), green_steel_technology_adoption (market structure and adoption asymmetry).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(green_steel_technology_adoption, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
