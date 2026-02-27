% ============================================================================
% CONSTRAINT STORY: semiconductor_fabrication_chokepoint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_semiconductor_fabrication_chokepoint, []).

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
 *   constraint_id: semiconductor_fabrication_chokepoint
 *   human_readable: The geopolitical and capital chokepoint of leading-edge semiconductor manufacturing
 *   domain: technological/geopolitical/economic
 *
 * SUMMARY:
 *   The concentration of leading-edge semiconductor fabrication in Taiwan
 *   Semiconductor Manufacturing Company (TSMC) represents one of the most
 *   critical geopolitical and economic chokepoints of the 2020s. The
 *   manufacture of sub-3nm chips requires capital expenditure exceeding $20
 *   billion per facility, decades of accumulated process knowledge, and
 *   access to a global supply chain of specialized equipment (primarily from
 *   the Netherlands, Japan, and the US). This extraordinary concentration
 *   emerged not from regulatory mandate but from the compounding advantages
 *   of first-mover status, capital efficiency, and learning curves. TSMC's
 *   dominance created a structural constraint: global semiconductor design
 *   companies (Nvidia, AMD, Apple, Qualcomm) cannot substitute away without
 *   15+ years and $50+ billion in capital; competing foundries (Samsung,
 *   Intel) are perpetually 1-2 nodes behind; and both China and Russia face
 *   strategic vulnerability to US-controlled export restrictions. The
 *   constraint exhibits all hallmarks of a Snare from most perspectives —
 *   suppression (export controls, capital barriers, geopolitical lock-in),
 *   base extractiveness (TSMC captures extraordinary margins and allocation
 *   leverage), and zero practical exit options for trapped agents. However,
 *   the analytical observer sees structural complexity: TSMC's technical
 *   dominance is real and provides genuine coordination value (the world's
 *   most advanced chips). US national security benefits from the chokepoint
 *   (leverage over allied and adversarial states). Taiwan's government
 *   experiences a mixed constraint (extraction from geopolitical
 *   vulnerability, but also extraordinary economic and political importance).
 *   Allied governments are building Scaffold — temporary industrial support
 *   structures to diversify capacity and reduce the single-point-of-failure
 *   risk. The constraint's trajectory shows increasing extractiveness (TSMC's
 *   margin expansion, geopolitical leverage accumulation) and increasing
 *   theater (government subsidies to competing fabs are largely performative
 *   in the near term). The core question is whether allied industrial policy
 *   will succeed in reducing concentration (Scaffold sunset) or whether the
 *   capital and technical barriers are too high (Snare permanence).
 *
 * KEY AGENTS:
 *   - TSMC (Taiwan Semiconductor Manufacturing Company): Primary beneficiary (institutional/arbitrage) — captures extraordinary rent from technical dominance and allocation scarcity; experiences geopolitical pressure (mixed extraction)
 *   - Chip Design Companies (Nvidia, AMD, Apple, Qualcomm, Broadcom, etc.): Primary victims (powerless/trapped) — cannot substitute away; bear extraction through pricing, allocation priority, strategic vulnerability
 *   - Competing Foundries (Samsung, Intel, GlobalFoundries, SMIC): Secondary victims (moderate/constrained) — perpetually lagged; lose market share and margins; cannot access equivalent capital or technology
 *   - Taiwan Government: Co-beneficiary and co-victim (moderate/constrained) — benefits from TSMC's economic contribution and strategic importance; vulnerable to geopolitical coercion and military concentration risk
 *   - US State Department / National Security: Beneficiary (institutional/arbitrage) — leverages TSMC concentration for geopolitical control; minimal direct cost; arbitrages between technical access and strategic alignment
 *   - Chinese Government / PLA: Victim (moderate/constrained, rising toward trapped) — cut off from advanced chips by export controls; experiences maximum extraction through strategic disadvantage; limited exit options
 *   - Allied Governments (US, Japan, South Korea, EU): Organized coalition (organized/constrained) — building Scaffold through CHIPS Act subsidies, rapid fab expansion; sees chokepoint as temporary; extracting from TSMC through forced investment and technology sharing
 *   - Analytical Observer: (analytical/analytical) — sees constraint as mixed Tangled Rope: genuine coordination value + severe asymmetric extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(semiconductor_fabrication_chokepoint, 0.58).
domain_priors:suppression_score(semiconductor_fabrication_chokepoint, 0.72).
domain_priors:theater_ratio(semiconductor_fabrication_chokepoint, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(semiconductor_fabrication_chokepoint, extractiveness, 0.58).
narrative_ontology:constraint_metric(semiconductor_fabrication_chokepoint, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(semiconductor_fabrication_chokepoint, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(semiconductor_fabrication_chokepoint, snare).
narrative_ontology:human_readable(semiconductor_fabrication_chokepoint, "The geopolitical and capital chokepoint of leading-edge semiconductor manufacturing").
narrative_ontology:topic_domain(semiconductor_fabrication_chokepoint, "technological/geopolitical/economic").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(semiconductor_fabrication_chokepoint, tsmc).
narrative_ontology:constraint_beneficiary(semiconductor_fabrication_chokepoint, us_state_department).
narrative_ontology:constraint_victim(semiconductor_fabrication_chokepoint, chip_design_companies).
narrative_ontology:constraint_victim(semiconductor_fabrication_chokepoint, oem_manufacturers).
narrative_ontology:constraint_victim(semiconductor_fabrication_chokepoint, competing_fabs).
narrative_ontology:constraint_victim(semiconductor_fabrication_chokepoint, taiwan_economy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CHIP DESIGN ECOSYSTEM (SNARE) — Powerless agents (Nvidia, AMD, Qualcomm, Apple) are trapped in dependency on TSMC's 2nm capacity. No practical exit: reproducing this capability would require $50B+ and 15+ years. These companies bear full extraction cost — manufacturing pricing power, allocation priority, geopolitical leverage. Maximum experienced extraction with zero degrees of freedom.
constraint_indexing:constraint_classification(semiconductor_fabrication_chokepoint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMPETING FOUNDRIES (SNARE) — Samsung, Intel foundry, GlobalFoundries are trapped in perpetual 1-2 node lag behind TSMC. Capital constraints and learning curve disadvantages lock them out. They bear the extraction cost of market share loss, customer defection, and margin compression. Cannot exit without capital levels only TSMC can access.
constraint_indexing:constraint_classification(semiconductor_fabrication_chokepoint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: TAIWAN/TSMC MANAGEMENT (TANGLED ROPE) — Constrained but not powerless. TSMC benefits enormously from its monopoly position (rent extraction), but is also locked into a geopolitical chokepoint role. Taiwan's government faces existential risk from military concentration and economic dependence on one company. TSMC management experiences extraction from US security pressure, Chinese espionage threats, and the constant capital treadmill to stay ahead. Both coordination (global supply chain dependence on their capability) and asymmetric extraction (geopolitical leverage over them) coexist.
constraint_indexing:constraint_classification(semiconductor_fabrication_chokepoint, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: US STATE DEPARTMENT (ROPE) — Experiences the constraint primarily as coordination: TSMC's concentration in allied Taiwan gives the US leverage over both China and allies dependent on chips. The US benefits from arbitraging between TSMC's technical capability and geopolitical alignment. Extracts geopolitical concessions (Taiwan support, China containment) with minimal direct cost. Low experienced extraction because the US has structural power and exit options.
constraint_indexing:constraint_classification(semiconductor_fabrication_chokepoint, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CHINESE STATE (SNARE) — Trapped in dependence on advanced chips (both civilian and military), yet sanctioned from accessing TSMC capacity. The constraint extracts strategic vulnerability: China cannot match US military capabilities, AI development, or computational capacity without 3nm+ chips. Suppression is extreme — export controls, sanctions, technology denial. Exit paths (domestic fab development, reverse engineering) are slow and expensive. Experiences maximum extraction with only constrained, heavily-supervised exit options.
constraint_indexing:constraint_classification(semiconductor_fabrication_chokepoint, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ALLIED INDUSTRIAL POLICY (SCAFFOLD) — US, Japan, South Korea, EU are building alternative capacity through massive subsidies (CHIPS Act, rapid fab expansion). This is a temporary support structure: extraction is being reduced by deliberately building competing capability. High theater (propaganda about 'restoring domestic capability') but genuine coordination function (distributing single-point-of-failure risk). Sunset clause: once competing 2-3nm capacity reaches meaningful volume (5-7 years), TSMC's monopoly extraction declines sharply.
constraint_indexing:constraint_classification(semiconductor_fabrication_chokepoint, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: FABLESS-FOUNDRY MODEL (PITON) — The 1990s fabless-foundry decomposition (chip design separated from manufacturing) was genuine coordination innovation. By 2026, this model is heavily theaters: massive government subsidies to shore up competing fabs, strategic alliances replacing market selection, and geopolitical override of economic optimization. The separation persists through institutional inertia (it worked for 30 years) but is increasingly dysfunctional (redundant capacity, loss of integrated innovation). Theater ratio climbing as governments pour capital into prop-up fabs rather than market-driven capability.
constraint_indexing:constraint_classification(semiconductor_fabrication_chokepoint, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global perspective, this constraint is a mixed hybrid: genuine coordination function (TSMC's technical mastery enables computational civilization), plus severe asymmetric extraction (geopolitical leverage, rent capture, supply chain control). The chokepoint is not inevitable; it resulted from capital intensity + learning curves + geographic concentration. It is not a natural law, but it is not easily reversible. The constraint exhibits both coordination benefits (cheapest, most advanced chips in world) and extraction costs (concentration risk, geopolitical leverage, supplier vulnerability). Classification as Tangled Rope reflects both functions are real and substantial.
constraint_indexing:constraint_classification(semiconductor_fabrication_chokepoint, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(semiconductor_fabrication_chokepoint_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(semiconductor_fabrication_chokepoint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(semiconductor_fabrication_chokepoint, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(semiconductor_fabrication_chokepoint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(semiconductor_fabrication_chokepoint, TR),
    TR >= 0.70.

:- end_tests(semiconductor_fabrication_chokepoint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High but not maximal. TSMC captures extraordinary rents from technical dominance and capacity scarcity, but the extraction is bounded by the coordination value it provides (world-leading chip manufacturing) and by active pressure from allied governments (CHIPS Act forcing capacity expansion). The 0.58 value reflects that while extraction is substantial, it is not at the level of a pure monopoly Snare (0.75+) because TSMC's business model depends on customer trust and long-term relationships — excessive extraction would accelerate competitor development. The upward trajectory (0.42 → 0.58 over the interval) reflects increasing geopolitical leverage as the US-China competition intensifies and competing fabs fall further behind. Suppression (0.72): Very high. The barriers to exit are extraordinary: (a) capital barriers — $20B+ per fab is accessible only to the largest corporations and governments, (b) technical barriers — accumulated process knowledge and learning curves create 5-15 year lags, (c) supply chain barriers — many specialized tools have single suppliers (e.g., EUV lithography from ASML), and (d) geopolitical barriers — US export controls block Chinese access, and Taiwan's vulnerability constrains TSMC's own options. No meaningful exit paths exist for trapped agents except waiting for competitors to catch up (which takes decades) or investing in redundant capacity (which allied governments are now doing through Scaffold). Theater ratio (0.38): Moderate-low. The constraint is primarily structural and functional rather than performative. TSMC's manufacturing actually works — they produce the world's most advanced chips at scale. The theater that is present consists of (a) government rhetoric about 'restoring domestic semiconductor capability' (Scaffold theater), (b) performance claims by competing fabs (which rarely materialize), and (c) security theater around export controls. The theater is growing over the interval (0.18 → 0.38) as allied governments pour capital into fabs that cannot yet compete at parity, creating a gap between stated goals and technical reality. Claimed type (Snare): The dominant perspective sees extraction with minimal coordination value or exit optionality. This is accurate from the design companies' viewpoint (powerless, trapped) and from the competing fabs' viewpoint (moderate, constrained). However, the Tangled Rope classification from the analytical observer reflects that genuine coordination value exists alongside extraction — TSMC's technical capability is irreplaceable, and the world's computational civilization depends on their capacity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a massive perspectival gap. From the chip design ecosystem (powerless/trapped), the constraint appears as a pure Snare: they have no exit, face extraction through allocation leverage and pricing power, and experience suppression through the extreme difficulty of building alternative capacity. From TSMC's own perspective (moderate/constrained), the constraint is a Tangled Rope: they benefit from monopoly rents but are trapped in geopolitical coercion, export control pressure, and the knowledge that losing technical leadership means losing everything. From the US perspective (institutional/arbitrage), the constraint appears as Rope: they experience coordination value (control of global chip supply through allied Taiwan) and minimal extraction cost (the US subsidizes some of the risk through CHIPS Act, but captures far more geopolitical leverage). From allied governments (organized/constrained), the constraint appears as Scaffold: they see it as a temporary problem being solved through industrial policy investment, with a sunset clause of 5-10 years when competing capacity reaches parity. From China's perspective (moderate/constrained rising toward trapped), the constraint is hardening into a Snare: they are locked out through export controls, face asymmetric suppression (cannot access TSMC, cannot build domestic alternatives fast enough), and experience maximum extraction through strategic vulnerability. The analytical observer sees Tangled Rope: genuine coordination value (the world's best chips) coexists with asymmetric extraction (geopolitical leverage, rent capture, supply-chain vulnerability). These are not compatible classifications from a single perspective — they reflect genuinely different structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation for each agent reflects their power level, exit options, and beneficiary/victim status. TSMC occupies an unusual position: institutional power but constrained (not arbitrage) exit options due to geopolitical pressure — this produces moderate d (around 0.35-0.45) and moderate experienced extraction. Design companies are powerless and trapped — d approaches 1.0, producing maximum f(d) and maximum experienced extraction chi. Competing fabs have moderate power but constrained exit (cannot abandon the chip fab business) — d around 0.65, producing high experienced extraction. The US State Department is institutional with arbitrage options (can threaten TSMC, can support competitors, can adjust export policy) — d approaches 0.05, producing negative/minimal experienced extraction (the US extracts FROM this constraint, not the reverse). China is moderate power (large economy, military capability) but increasingly trapped (export controls block access, domestic capability lags) — d rising toward 0.85, producing high experienced extraction. Allied governments are organized with constrained exit (must reduce concentration for national security) — d around 0.50-0.55, producing moderate experienced extraction. The derivation chain shows why the design companies experience maximum extraction while TSMC experiences moderate extraction: same structural constraint, different d values from different structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves the mandatrophy by showing that the 'chokepoint' label conflates two structurally distinct phenomena: (1) genuine technical coordination value (TSMC's capability is the world's best), and (2) asymmetric geopolitical/economic extraction (concentration creates leverage). The false summit risk is classifying this as a pure Snare ('unmitigated extraction') when it actually delivers substantial coordination benefits. The false basement risk is classifying it as Rope ('just coordination with minor market power') when geopolitical leverage and supply-chain vulnerability create severe extraction costs. The Tangled Rope classification from the analytical observer is correct: TSMC's dominance BOTH solves a coordination problem (world-class chips at scale) AND extracts asymmetric rents (allocation leverage, pricing power, geopolitical influence). The mandate avoidance is explicit: acknowledge both the value and the extraction simultaneously. The mandatrophy is then resolved by recognizing that allied Scaffold policies (CHIPS Act, fab subsidies) are structurally necessary to unbundle the coordination value from the extraction value. Once competing fabs reach 2-3nm parity, the coordination value persists (multiple suppliers of advanced chips) while the extraction leverage declines (TSMC can no longer dictate allocation or pricing). The Scaffold perspective correctly identifies this as temporary extraction with a real sunset clause — not a false promise but a genuine structural trajectory. The theater in the constraint comes from allied government claims that domestic fabs will reach parity by 2027-2030 (unrealistic), not from the constraint's core mechanics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_fab_competitiveness_timeline,
    'At what production volume do competing fabs (Intel, Samsung foundry, SMIC) become cost-competitive with TSMC 2nm, and when do they reach technical parity?',
    'Longitudinal cost curve analysis; yield progression tracking; time-to-market data for competing nodes; customer migration patterns',
    'If competitors reach parity by 2030: TSMC''s extraction power declines to Rope or Scaffold. If lag persists past 2035: extraction hardens to Snare. Determines whether allied government subsidies solve the chokepoint or merely reduce its severity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_fab_competitiveness_timeline, empirical, 'When competing fabs reach technical and cost parity with TSMC').

omega_variable(
    taiwan_military_vulnerability_threshold,
    'Does TSMC''s concentration in Taiwan create unacceptable military/supply-chain risk, or is allied diplomatic coordination sufficient to deter Chinese military action?',
    'Military scenario analysis; Chinese military capability assessment; Taiwan air defense and US response time modeling; supply-chain redundancy mapping',
    'If Taiwan concentration is deemed unacceptable: forces massive redundant fab build-out (Scaffold accelerates). If coordination is deemed sufficient: TSMC remains concentrated (Tangled Rope persists). Determines whether the constraint is temporary or structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(taiwan_military_vulnerability_threshold, empirical, 'Taiwan geographic concentration risk threshold for military/supply-chain resilience').

omega_variable(
    geopolitical_leverage_extractiveness_coupling,
    'Is TSMC''s extraction power (capacity allocation, pricing) driven by technical dominance (legitimate Rope coordination rent) or by geopolitical leverage (Snare coercion)?',
    'Counterfactual analysis: simulate chip design company choices if TSMC were non-aligned, or if US sanctioned TSMC access. Compare actual customer behavior to hypothetical market equilibrium. Analyze pricing vs. cost curves.',
    'If technical dominance dominates: TSMC extraction reflects Rope legitimacy (firms would choose TSMC anyway). If geopolitical leverage dominates: extraction is Snare coercion. Changes mandatrophy interpretation — affects whether chokepoint is acceptable market outcome or unacceptable strategic vulnerability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_leverage_extractiveness_coupling, conceptual, 'Whether TSMC extraction is technical dominance (Rope) or geopolitical leverage (Snare)').

omega_variable(
    chiplet_modularity_disruption,
    'Do chiplet design methodologies and heterogeneous integration reduce the necessity of leading-edge monolithic node parity, allowing older nodes to substitute?',
    'Design architecture trend analysis; chiplet adoption curves; power/performance gap modeling for older+chiplet vs. leading-edge monolithic',
    'If chiplets sufficiently substitute: demand for 2nm monolithic capacity drops, reducing TSMC''s extraction leverage (Rope or Scaffold). If monolithic performance remains critical: extraction power persists (Snare). Determines technical trajectory of the chokepoint''s severity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chiplet_modularity_disruption, empirical, 'Whether chiplet design methodologies reduce dependence on leading-edge node parity').

omega_variable(
    china_sanctions_regime_sustainability,
    'Can the US-led export control regime (ECRA, CFIUS, supply-chain restrictions on China-bound chip tools) be sustained indefinitely, or will it erode through business pressure and allied defection?',
    'Compliance tracking; violation detection rates; allied adherence to restrictions; corporate lobbying pressure; revenue diversion through third countries',
    'If regime sustains: China remains locked out of advanced chips (Snare persists for China perspective). If erodes: Chinese fabs gain access, competition increases, TSMC extraction declines. Determines whether geopolitical Snare is stable or temporary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(china_sanctions_regime_sustainability, empirical, 'Long-term sustainability of export control regime against China').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(semiconductor_fabrication_chokepoint, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(semicon_tr_t0, semiconductor_fabrication_chokepoint, theater_ratio, 0, 0.18).
narrative_ontology:measurement(semicon_tr_t5, semiconductor_fabrication_chokepoint, theater_ratio, 5, 0.28).
narrative_ontology:measurement(semicon_tr_t10, semiconductor_fabrication_chokepoint, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(semicon_be_t0, semiconductor_fabrication_chokepoint, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(semicon_be_t5, semiconductor_fabrication_chokepoint, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(semicon_be_t10, semiconductor_fabrication_chokepoint, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(semiconductor_fabrication_chokepoint, global_infrastructure).
narrative_ontology:affects_constraint(semiconductor_fabrication_chokepoint, us_china_technology_decoupling).
narrative_ontology:affects_constraint(semiconductor_fabrication_chokepoint, taiwan_geopolitical_vulnerability).
narrative_ontology:affects_constraint(semiconductor_fabrication_chokepoint, allied_industrial_policy_coordination).

% DUAL FORMULATION NOTE:
% The semiconductor chokepoint decomposes into three analytically distinct constraints: (1) TSMC's technical dominance (ε≈0.35, Mountain from technical perspective — the physics and engineering of sub-3nm manufacturing); (2) the geopolitical concentration in Taiwan (ε≈0.65, Snare from military/strategic perspective — concentration creates vulnerability); (3) the export control regime locking out China (ε≈0.72, Snare from Chinese perspective — complete exclusion from advanced chips). This story focuses on the integrated constraint from the perspective of global semiconductor customers and geopolitical actors. Decomposition into separate stories would separately model: (a) the physics/engineering limits of nanometer scaling (mountain), (b) Taiwan's military vulnerability (snare from state security perspective), and (c) the export control regime (snare from Chinese perspective). These are linked by affects_constraints but have different ε values reflecting different measurement bases.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(semiconductor_fabrication_chokepoint, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
