% ============================================================================
% CONSTRAINT STORY: us_china_tech_decoupling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_china_tech_decoupling, []).

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
 *   constraint_id: us_china_tech_decoupling
 *   human_readable: US-China Technology Decoupling and Supply Chain Fragmentation
 *   domain: geopolitical_economics/trade_policy
 *
 * SUMMARY:
 *   US-China technology decoupling represents a geopolitical reorganization
 *   of integrated supply chains into competing ecosystems. Initiated through
 *   export controls (CHIPS for Advanced Computing, entity lists), subsidies
 *   (CHIPS Act, METIS Act), and alliance-building (Quad framework, USMCA
 *   provisions), decoupling creates simultaneous coordination functions
 *   (reshoring allied production, assured supply for partners) and asymmetric
 *   extraction (cost inflation for consumers, market fragmentation,
 *   innovation duplication). The constraint exhibits all six classification
 *   types from different structural positions: it is an immutable natural law
 *   from the analytical perspective (technology diffuses), a coordinated
 *   alliance from the US manufacturer perspective (protected markets), a
 *   temporary scaffold from the allied perspective (reshoring with sunset if
 *   geopolitics change), a degraded control system from the institutional
 *   perspective (Cold War export controls retrofitted for the present), a
 *   mixed coordination-extraction hybrid for non-aligned actors (forced
 *   compliance with new market access), and pure extraction for the global
 *   supply chain (trapped in fragmentation). The theater ratio (0.65)
 *   reflects high performative content: public rhetoric emphasizes supply
 *   assurance and sovereignty, masking that decoupling cannot fully prevent
 *   technology diffusion and that cost inflation persists even with reshored
 *   production.
 *
 * KEY AGENTS:
 *   - US Semiconductor Manufacturers (institutional/arbitrage): Primary beneficiaries — capture protected domestic markets, subsidized capacity building, reduced foreign competition. Net positive extraction flow.
 *   - Chinese Technology Sector (organized/constrained): Secondary beneficiary-victim — experiences genuine coordination function (domestic substitution, supply chain independence) but faces severe extraction (access denial, R&D isolation, export limitations).
 *   - Allied Tech Companies (organized/constrained): Mixed victim-beneficiary — constrained by compliance and market fragmentation but benefit from new procurement and reshoring contracts.
 *   - Global Supply Chain Integration (powerless/trapped): Primary victim — abstract collective with no exit option; bears full cost of redundancy, fragmentation, and price inflation.
 *   - Non-aligned Tech Companies (moderate/constrained): Secondary victim — constrained by geopolitical pressure and forced vendor choice, though some benefit from arbitrage opportunities.
 *   - Cold War Export Control System (institutional/arbitrage): Maintains institutional control through degraded framework — high theater, declining actual function.
 *   - Analytical Observer (analytical/analytical): Risk of naturalizing contingent geopolitical choice as technological inevitability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_china_tech_decoupling, 0.58).
domain_priors:suppression_score(us_china_tech_decoupling, 0.72).
domain_priors:theater_ratio(us_china_tech_decoupling, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_china_tech_decoupling, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_china_tech_decoupling, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_china_tech_decoupling, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_china_tech_decoupling, tangled_rope).
narrative_ontology:human_readable(us_china_tech_decoupling, "US-China Technology Decoupling and Supply Chain Fragmentation").
narrative_ontology:topic_domain(us_china_tech_decoupling, "geopolitical_economics/trade_policy").

domain_priors:requires_active_enforcement(us_china_tech_decoupling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_china_tech_decoupling, us_semiconductor_manufacturers).
narrative_ontology:constraint_beneficiary(us_china_tech_decoupling, allied_chipmakers).
narrative_ontology:constraint_beneficiary(us_china_tech_decoupling, domestic_supply_chain_producers).
narrative_ontology:constraint_victim(us_china_tech_decoupling, global_technology_consumers).
narrative_ontology:constraint_victim(us_china_tech_decoupling, integrated_supply_chains).
narrative_ontology:constraint_victim(us_china_tech_decoupling, developing_economies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GLOBAL SUPPLY CHAIN INTEGRATION (SNARE) — Cannot exit the bifurcated ecosystem; trapped by decades of integrated production networks. Faces full extraction costs through fragmentation, redundancy, price inflation, and coordination collapse. No alternatives exist at equivalent scale or cost. Maximum experienced suppression: network effects lock suppliers and consumers into incompatible systems.
constraint_indexing:constraint_classification(us_china_tech_decoupling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-ALIGNED TECHNOLOGY COMPANIES (TANGLED ROPE) — Constrained by geopolitical pressure and export control compliance, but also benefit from new market opportunities (allied procurement, reshoring contracts). Genuine coordination function exists (supply assurance through allied networks) alongside asymmetric extraction (forced compliance costs, market fragmentation). Moderate agency but significant path-dependent costs.
constraint_indexing:constraint_classification(us_china_tech_decoupling, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: US SEMICONDUCTOR MANUFACTURERS (ROPE) — Primary beneficiaries with arbitrage options. Experience the constraint as advantageous coordination: protected domestic markets, subsidized capacity building (CHIPS Act), reduced Chinese competition. Net beneficiary — can arbitrage between protected US market and selective allied sales. Low or negative experienced extraction.
constraint_indexing:constraint_classification(us_china_tech_decoupling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CHINESE TECHNOLOGY SECTOR (TANGLED ROPE) — Organized response to decoupling through domestic substitution (semiconductor fabs, OS development, supply chain alternatives). Experiences genuine coordination function (domestic ecosystem building, tech self-sufficiency) alongside severe extraction (access denial, R&D isolation, export limitations). Strategic agency exists but operates under constraint.
constraint_indexing:constraint_classification(us_china_tech_decoupling, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ALLIED TECH COALITION (SCAFFOLD) — Organized coalition building alternative supply chains with temporary/negotiable sunset logic. Experiences lower effective extraction because coalition has agency and exit path (re-integration if geopolitical tensions decline). Theater ratio moderate: reshoring rhetoric masks continued supply interdependence. Sunset clause implicit: if US-China tensions ease, decoupling infrastructure becomes redundant.
constraint_indexing:constraint_classification(us_china_tech_decoupling, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: COLD WAR EXPORT CONTROL SYSTEM (PITON) — Legal/institutional framework (EAR, ITAR, dual-use regulations) from Cold War era is being retrofitted for technology decoupling. The original function (preventing Soviet military buildup) has atrophied; the current use (controlling Chinese semiconductor access) is largely performative because controls cannot prevent reverse engineering or secondary market arbitrage. Piton classification: high theater (compliance rituals, audit requirements) with degraded actual function (determined actors bypass via third countries, indigenous development).
constraint_indexing:constraint_classification(us_china_tech_decoupling, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational scale, technology diffusion is an inherent feature of global economics: knowledge and manufacturing capacity inevitably spread over time, independent of state-level control efforts. This perspective sees decoupling attempts as fighting a natural law of technological development. However, structural data contradicts mountain classification — the engine's false summit detector will identify this as naturalization of a contingent political arrangement (the US chose decoupling; it is not inevitable).
constraint_indexing:constraint_classification(us_china_tech_decoupling, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_china_tech_decoupling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_china_tech_decoupling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_china_tech_decoupling, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_china_tech_decoupling, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_china_tech_decoupling, TR),
    TR >= 0.70.

:- end_tests(us_china_tech_decoupling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The decoupling produces real coordination benefits (supply assurance for allies, domestic production capacity, reduced dependency) alongside significant extraction (cost inflation across supply chains, market fragmentation, R&D duplication, consumer price increases). The base value of 0.58 reflects that extraction is substantial but not total — genuine coordination functions exist and parties exercise strategic agency. Suppression (0.72): High. Extraction is sustained by legal controls (export regulations), technological barriers (advanced fab complexity), geopolitical pressure (alliance loyalty mechanisms), and market architecture (two incompatible ecosystems). Agents cannot freely exit the constraint — they must choose sides in a bifurcated market. Theater ratio (0.65): Moderate-high. Public rhetoric emphasizes supply security and sovereignty, masking that (a) cost premiums are permanent, (b) technology diffusion cannot be permanently halted, (c) Cold War control frameworks have degraded actual preventive function. The performative content has increased over the measurement interval as decoupling has proven more costly and less effective than initial rhetoric suggested.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival disagreement on classification. The beneficiary (US manufacturers) sees rope. The allied coalition sees scaffold. The organized response (Chinese sector) sees tangled rope with agency. The powerless victim (global supply chains) sees snare. The degraded institution (Cold War controls) sees piton. The civilizational observer risks false summit (mountain). The perspectival divergence reveals that decoupling is not a natural constraint but a constructed political arrangement. Each perspective's classification is structurally justified — the gap is not error but genuine disagreement about whether the constraint is beneficial coordination (rope) or asymmetric extraction (snare). The resolution requires understanding that both are true simultaneously: decoupling coordinates allied supply chains while extracting from global consumers and integrated industries. This is precisely what tangled rope captures at the systemic level.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural relationships. US manufacturers (beneficiaries/arbitrage) derive d ≈ 0.10 → f(d) ≈ -0.01 → negative χ (experienced benefit). Chinese tech sector (organized victim/constrained) derives d ≈ 0.65 → f(d) ≈ 1.00 → high χ (significant asymmetric extraction). Allied firms (constrained mixed) derive d ≈ 0.50 → f(d) ≈ 0.65 → moderate χ. Global supply chains (trapped victim) derive d ≈ 0.95 → f(d) ≈ 1.42 → maximum χ (experienced as snare). Non-aligned actors (constrained victim) derive d ≈ 0.75 → f(d) ≈ 1.15 → high χ. Analytical observer (arbitrage) derives d ≈ 0.72 → f(d) ≈ 1.15. The variation in d across perspectives explains why identical base extractiveness (0.58) produces different experienced χ values and different classifications across the perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that decoupling is genuinely both coordination and extraction simultaneously. The coordination function (supply assurance, domestic capacity building, allied reshoring) is real and beneficial for certain agents (US manufacturers, allied producers). The extraction function (cost inflation, market fragmentation, access denial, R&D duplication) is also real and harmful for other agents (global consumers, integrated supply chains, non-aligned firms). Tangled rope classification captures this hybrid: ε = 0.58 (moderate-high extraction with coordination overhead), suppression = 0.72 (high barriers to exit), requires_active_enforcement = true (legal/geopolitical enforcement sustains decoupling). The mandatrophy risk would be treating decoupling as pure coordination (rope) — which would deny the real extraction costs — or as pure extraction (snare) — which would deny the real coordination functions. The tangled rope classification is appropriate precisely because both functions are genuinely present and the engine correctly identifies both. The theater ratio (0.65) indicates that public rhetoric amplifies coordination claims while downplaying extraction costs — this is exactly what a tangled rope under geopolitical pressure would do to maintain legitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technology_diffusion_inevitability,
    'Is technology diffusion inevitable (mountain) or contingent on enforcement (snare/tangled_rope)?',
    'Empirical: measure decoupling effectiveness via (a) indigenous Chinese semiconductor progress timeline, (b) alternative supply chain emergence, (c) re-integration timelines if geopolitical conditions change. Historical: compare to Soviet technology control regime outcomes.',
    'If diffusion is inevitable: decoupling is temporary coordination overhead with sunset (scaffold). If diffusion requires active suppression: decoupling is structural extraction (snare). If diffusion is conditional: tangled rope classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_diffusion_inevitability, empirical, 'Whether technology diffusion is inevitable or contingent on enforcement').

omega_variable(
    allied_sovereignty_extraction,
    'Do ''allied'' nations experience decoupling as beneficial coordination (rope) or as coerced participation extracting their own autonomy (snare)?',
    'Structural analysis: survey allied firms on (a) compliance cost vs benefit, (b) alternative market access foregone, (c) dependency on US semiconductor policy. Geopolitical: identify instances where allied nations defected or negotiated exceptions.',
    'If allies see genuine benefit: rope/tangled rope classification confirmed. If allies are coerced: decoupling is asymmetric extraction at the allied level, making the constraint a snare with hidden victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allied_sovereignty_extraction, empirical, 'Whether decoupling coerces allied nations or provides mutual benefit').

omega_variable(
    reshoring_substitution_feasibility,
    'Can reshoring and allied supply chains actually replace integrated US-China supply chains, or is decoupling permanently raising costs?',
    'Empirical: (a) cost trajectory of reshored vs offshored production, (b) capability parity timelines (when do reshored fabs achieve feature parity?), (c) utilization rates (are reshored plants underutilized?). Counterfactual: model cost impacts of full vs partial decoupling.',
    'If substitution is feasible at equivalent cost: scaffold sunset is real (extraction is temporary). If substitution requires permanent cost premium: decoupling is permanent extraction (snare). If feasibility is uncertain: omega remains unresolved and classification stays tangled rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reshoring_substitution_feasibility, empirical, 'Whether reshoring can achieve cost parity with integrated supply chains').

omega_variable(
    enforcement_game_theory,
    'Do export controls actually prevent technology transfer, or do they trigger escalating enforcement costs that eventually collapse?',
    'Game-theoretic analysis: model (a) enforcement vs circumvention costs over time, (b) secondary market emergence, (c) Chinese reverse engineering R&D as function of access denial. Empirical: track control effectiveness (fraction of restricted technology actually denied vs acquired through alternatives).',
    'If controls are durable: snare/tangled rope classification confirmed (sustained extraction). If controls degrade: piton classification confirmed (theater masking declining function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_game_theory, empirical, 'Whether export controls can sustain decoupling or face escalating circumvention').

omega_variable(
    geopolitical_reversibility,
    'Are US-China tensions structural and permanent, or contingent and reversible?',
    'Political: assess whether decoupling reflects fixed structural incompatibility (competing for primacy) or negotiable tensions (resolving trade imbalances, investment disputes). Historical: identify analogues (US-Japan tensions, US-Soviet detente cycles) and their duration.',
    'If tensions are reversible: scaffold sunset is real (decoupling infrastructure can be dismantled). If tensions are structural: decoupling is permanent extraction architecture (closer to snare). Classification hinges on whether geopolitical conditions can change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(geopolitical_reversibility, conceptual, 'Whether geopolitical decoupling reflects structural incompatibility or contingent tensions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_china_tech_decoupling, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usct_tr_t0, us_china_tech_decoupling, theater_ratio, 0, 0.42).
narrative_ontology:measurement(usct_tr_t2, us_china_tech_decoupling, theater_ratio, 2, 0.55).
narrative_ontology:measurement(usct_tr_t4, us_china_tech_decoupling, theater_ratio, 4, 0.65).

% Extraction over time
narrative_ontology:measurement(usct_be_t0, us_china_tech_decoupling, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(usct_be_t2, us_china_tech_decoupling, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(usct_be_t4, us_china_tech_decoupling, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_china_tech_decoupling, global_infrastructure).
narrative_ontology:affects_constraint(us_china_tech_decoupling, semiconductor_supply_chain_security).
narrative_ontology:affects_constraint(us_china_tech_decoupling, advanced_chip_fab_concentration).
narrative_ontology:affects_constraint(us_china_tech_decoupling, allied_industrial_policy).
narrative_ontology:affects_constraint(us_china_tech_decoupling, chinese_domestic_substitution).

% DUAL FORMULATION NOTE:
% US-China decoupling is a macro-constraint that decomposes into several domain-specific constraints with different ε values: semiconductor supply (ε ≈ 0.65, higher extraction), advanced fab concentration (ε ≈ 0.52, moderate extraction), allied industrial policy (ε ≈ 0.35, lower extraction due to coordination function), and Chinese substitution (ε ≈ 0.58, moderate extraction). The decoupling macro-constraint influences all four downstream constraints through policy coupling and supply reorganization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_china_tech_decoupling, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
