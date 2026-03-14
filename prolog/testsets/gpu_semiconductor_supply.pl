% ============================================================================
% CONSTRAINT STORY: gpu_semiconductor_supply
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpu_semiconductor_supply, []).

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
 *   constraint_id: gpu_semiconductor_supply
 *   human_readable: GPU Semiconductor Supply Chain Constraint
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The GPU semiconductor supply constraint represents a global bottleneck in
 *   computational capacity driven by explosive AI research demand colliding
 *   with constrained manufacturing capacity and concentrated control. The
 *   constraint exhibits the full spectrum of DR classifications depending on
 *   agent position: trapped researchers see pure extraction (Snare);
 *   manufacturers see coordination with beneficial asymmetry (Rope);
 *   organized consortiums see mixed coordination-extraction (Tangled Rope);
 *   the technical standards body maintains increasingly performative
 *   functions (Piton); and the civilizational analytical view risks
 *   naturalizing a contingent institutional arrangement as computational law
 *   (false Mountain). The extractiveness trajectory shows acceleration from
 *   0.28 (2022, pre-ChatGPT demand surge) to 0.58 (2026, matured constraint)
 *   as supply concentration tightened and dependent agents competed for
 *   access. Theater ratio remains moderate because the supply constraint is
 *   genuine (not purely performative) but production allocation decisions
 *   lack transparent mechanisms.
 *
 * KEY AGENTS:
 *   - Developing Economy Researchers: Powerless/trapped (institutional/geographic barriers) — bear maximum extraction cost; cannot access frontier compute capacity
 *   - Independent AI Labs: Powerless/trapped (capital threshold effects) — require $500K-$5M minimum for credible research; no institutional subsidy available
 *   - Small Tech Startups: Moderate/constrained (high but surmountable costs) — can access through cloud providers or alternative chips; partial exit available at premium price
 *   - Institutional Cloud Providers: Institutional/arbitrage (net beneficiaries) — arbitrage capacity across regions, negotiate priority allocation, benefit from scarcity premium
 *   - GPU Manufacturers: Institutional/arbitrage (primary beneficiaries) — control supply allocation, set pricing based on scarcity, benefit from coordination function
 *   - National Research Consortiums: Organized/constrained (collective negotiating power but structural dependence) — can lobby for allocation but cannot override manufacturer decisions
 *   - Technology-Leading Nations: Powerful/constrained (geopolitical leverage with supply vulnerability) — use export controls but depend on global supply chains and TSMC concentration
 *   - Technical Standard Bodies: Institutional/arbitrage (maintaining degraded standards) — coordinate software across hardware platforms but actual interoperability declining
 *   - Analytical Observer: Analytical/analytical (civilizational view) — risks naturalizing market-driven allocation as immutable computational law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpu_semiconductor_supply, 0.58).
domain_priors:suppression_score(gpu_semiconductor_supply, 0.65).
domain_priors:theater_ratio(gpu_semiconductor_supply, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpu_semiconductor_supply, extractiveness, 0.58).
narrative_ontology:constraint_metric(gpu_semiconductor_supply, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(gpu_semiconductor_supply, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpu_semiconductor_supply, tangled_rope).
narrative_ontology:human_readable(gpu_semiconductor_supply, "GPU Semiconductor Supply Chain Constraint").
narrative_ontology:topic_domain(gpu_semiconductor_supply, "economic/technological").

domain_priors:requires_active_enforcement(gpu_semiconductor_supply).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpu_semiconductor_supply, semiconductor_manufacturers).
narrative_ontology:constraint_beneficiary(gpu_semiconductor_supply, gpu_designers).
narrative_ontology:constraint_beneficiary(gpu_semiconductor_supply, institutional_cloud_providers).
narrative_ontology:constraint_victim(gpu_semiconductor_supply, developing_economy_researchers).
narrative_ontology:constraint_victim(gpu_semiconductor_supply, independent_ai_labs).
narrative_ontology:constraint_victim(gpu_semiconductor_supply, small_tech_startups).
narrative_ontology:constraint_victim(gpu_semiconductor_supply, global_academic_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVELOPING ECONOMY RESEARCHER (SNARE) — No structural exit from GPU dependency. Academic institutions in resource-constrained regions face absolute barriers: GPU costs exceed annual research budgets, export controls restrict access, geographic supply allocation prioritizes wealthy markets. Bears maximum extraction cost with zero alternatives.
constraint_indexing:constraint_classification(gpu_semiconductor_supply, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDEPENDENT AI LAB (SNARE) — Trapped by competitive threshold effects. Minimum viable GPU cluster for credible research now exceeds $500K–$5M. No lab can train frontier models without this capital investment. No option to substitute or delay. Extraction via artificial scarcity of compute capacity.
constraint_indexing:constraint_classification(gpu_semiconductor_supply, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: SMALL TECH STARTUP (TANGLED ROPE) — Faces high but surmountable GPU costs and allocation constraints. Can access GPUs through cloud providers but at premium pricing; can substitute with competitive chips (TPUs, custom silicon) at significant engineering cost. Benefits from GPU availability for product development but bears extraction through pricing power and supply prioritization. Partially mobile but constrained.
constraint_indexing:constraint_classification(gpu_semiconductor_supply, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL CLOUD PROVIDER (ROPE) — Net beneficiary. Solves coordination problem for GPU allocation across diverse clients through marketplace mechanisms. Can arbitrage capacity across regions, negotiate directly with manufacturers, access priority allocation. Benefits from coordination function without bearing extraction — effectively subsidized by customers' scarcity premium.
constraint_indexing:constraint_classification(gpu_semiconductor_supply, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: GPU MANUFACTURER (ROPE) — Coordinates production allocation across global demand. Sells out production capacity at elevated prices. Extracts value primarily through scarcity-driven pricing, but the coordination function (efficiently allocating constrained supply to highest-value uses) is genuine. Benefits from the constraint but also provides real coordination service.
constraint_indexing:constraint_classification(gpu_semiconductor_supply, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: NATIONAL RESEARCH CONSORTIUM (TANGLED ROPE) — Organized actors (government research agencies, university systems, national AI initiatives) can negotiate collective GPU allocation and subsidized access but remain structurally dependent on manufacturers for supply. Benefit from coordination of domestic research capacity; bear extraction through dependence on foreign supply chains and geopolitical allocation. Significant agency but constrained by supply concentration.
constraint_indexing:constraint_classification(gpu_semiconductor_supply, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: TECHNOLOGY-LEADING NATION (TANGLED ROPE) — Powerful but constrained. Controls some design and manufacturing capacity (TSMC, Samsung, domestic fabs) but depends on global supply chains, rare materials, and specialized manufacturing capabilities. Uses GPU allocation as geopolitical leverage (export controls to adversaries) while benefiting from coordination of domestic supply. Extraction flows both directions: benefits from pricing power, bears costs of supply vulnerability.
constraint_indexing:constraint_classification(gpu_semiconductor_supply, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: TECHNICAL STANDARD BODY (PITON) — Maintains GPU programming standards (CUDA, ROCm, OpenCL) that coordinate software across hardware. The standardization function is genuine and necessary, but increasingly theater: proprietary optimizations, vendor lock-in, and closed documentation mean standards provide less actual coordination than they purport to. Standards persist through institutional inertia and network effects despite declining functional value. Theater ratio driven by divergence between standard definition and actual interoperability.
constraint_indexing:constraint_classification(gpu_semiconductor_supply, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / COMPUTATIONAL FRONTIER (MOUNTAIN) — From a civilizational perspective, GPU supply bottlenecks reflect an immutable property of computation at scale: the fastest specialized hardware is always the bottleneck for the next research frontier. This appears as a natural law — whoever pushes the frontier first wins the access advantage. However, this naturalizes a contingent institutional choice: that GPUs (rather than distributed alternatives, or subsidized public infrastructure) are the primary path to frontier compute. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(gpu_semiconductor_supply, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpu_semiconductor_supply_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gpu_semiconductor_supply, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gpu_semiconductor_supply, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpu_semiconductor_supply, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gpu_semiconductor_supply, TR),
    TR >= 0.70.

:- end_tests(gpu_semiconductor_supply_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high and accelerating. Initial GPU scarcity (2022) was supply-driven and bounded by real fab capacity limits. By 2026, extractiveness has increased substantially as allocation mechanisms shifted from inventory shortage to permanent pricing power and strategic allocation. The 0.28→0.58 trajectory reflects transition from temporary shortage (recoverable through capacity expansion) to structural extraction (permanent allocation asymmetry). Suppression (0.65): High. Multiple enforcement mechanisms prevent alternative access: export controls restrict supply to certain regions, CUDA lock-in forces software dependency, manufacturing concentration (TSMC dominance) creates bottleneck control, capital requirements ($500K minimum) prevent individual exit. Theater ratio (0.48): Moderate. The constraint is materially real (fab capacity is genuinely constrained) but allocation decisions are increasingly opaque (no public tracking of inventory, allocation criteria, or redistribution logic). Cloud providers present simplified pricing as market-clearing when it may reflect strategic allocation.
 *
 * PERSPECTIVAL GAP:
 *   The manufacturer sees coordination (Rope) — they are solving the legitimate problem of allocating constrained supply to highest-value uses. The researcher sees extraction (Snare) — they experience absolute barriers to access. The analytical observer at civilizational scope risks seeing natural law (Mountain) — computational frontiers always require specialized hardware. The organized consortium sees mixed coordination-extraction (Tangled Rope) — genuine collective negotiation capability but structural dependence on manufacturer decisions. The standards body sees its function degrading (Piton) — CUDA standardization promised interoperability but delivers lock-in. This perspectival gap reveals that the constraint's type depends critically on the agent's structural position relative to supply allocation mechanisms, not on objective properties of GPU scarcity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from beneficiary/victim declarations and exit capacity. Manufacturers and cloud providers have institutional power and arbitrage options (can access alternative markets, negotiate terms, redirect supply) — they experience low d, resulting in negative or near-zero effective extraction. Trapped researchers and developers (powerless with no exit) experience d=0.95–1.0, producing maximum f(d) and maximum experienced extraction. Constrained actors (small startups, developing-economy consortiums) experience d=0.65–0.75, intermediate extraction. Organized consortiums with collective negotiating power experience lower d than individual trapped actors despite same power level, because constrained exit (collective action possible) differs from trapped exit (no alternatives exist). The scope modifier σ(S)=1.2 for global scope amplifies extractiveness — GPU supply concentration is planetary-scale, making verification and alternative-sourcing maximally difficult.
 *
 * MANDATROPHY ANALYSIS:
 *   STRUCTURAL RESOLUTION: The constraint avoids mandatrophy by distinguishing genuine coordination (supply allocation via manufacturers) from pure extraction (pricing power over dependent agents). The coordination component is real: without manufacturer allocation logic, supply would be chaotic. The asymmetry is also real: benefits flow disproportionately to manufacturers and institutional cloud providers. The tangled_rope classification captures both. The false mountain at the analytical level reveals that 'computational frontiers require the fastest hardware' naturalizes a choice (to make frontier compute private-market driven) rather than a law. The piton classification of standards bodies indicates degradation: CUDA coordination once solved real portability problems but now primarily enforces vendor lock-in. The snare classification for trapped agents is unambiguous — they have no exit and bear extraction without benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    geopolitical_allocation_opacity,
    'To what extent is GPU allocation determined by transparent market mechanisms versus opaque geopolitical prioritization?',
    'Analysis of allocation patterns across regions, correlation with trade relationships and export controls, FOI requests for government GPU procurement and allocation decisions',
    'If geopolitical: suppression and extraction scores increase substantially — constraint becomes more snare-like. If transparent market: justifies rope classification for beneficiaries and constrains victim classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_allocation_opacity, empirical, 'Whether GPU allocation follows market or geopolitical logic').

omega_variable(
    alternative_architecture_viability,
    'Can distributed, open-source, or custom-silicon alternatives (TPUs, neuromorphic chips, analog compute) credibly substitute for GPUs as the research frontier accelerator?',
    'Longitudinal tracking of research output and citation impact on frontier models trained with alternative architectures; cost-performance benchmarking across domains (NLP, vision, reinforcement learning, scientific computing)',
    'If alternatives viable: bottleneck is artificial and intensifies snare classification for locked-out agents. If alternatives inferior: bottleneck reflects genuine performance asymmetry and justifies rope classification. If parity emerging: scaffold classification becomes dominant (sunset toward alternatives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_architecture_viability, empirical, 'Whether alternative compute architectures can substitute for GPUs').

omega_variable(
    manufacturing_capacity_constraint_source,
    'Is GPU supply limited by physical manufacturing capacity (chip fabs) or by artificial allocation (hoarding, strategic shortage, pricing control)?',
    'Analysis of fab capacity utilization rates, wafer production volumes, correlation between stated capacity and actual supply, investigation of inventory levels and allocation decisions by manufacturers',
    'If physical capacity: constraint reflects genuine scarcity; extraction is bounded by real cost structures. If artificial: constraint is purely allocative; extraction can be reduced through policy intervention without technological change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manufacturing_capacity_constraint_source, empirical, 'Whether supply constraint is physical or artificial').

omega_variable(
    public_compute_infrastructure_feasibility,
    'Could public funding for GPU compute infrastructure (like historical national supercomputer programs) provide accessible frontier compute without market extraction?',
    'Historical comparison with public supercomputer initiatives, economic modeling of public GPU infrastructure costs versus academic research benefits, institutional analysis of maintenance and governance models for shared public compute',
    'If feasible: reveals constraint as policy choice rather than natural limit; scaffold classification emerges (sunset toward public infrastructure). If infeasible: legitimizes private market and rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_compute_infrastructure_feasibility, preference, 'Whether public compute infrastructure could replace market allocation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpu_semiconductor_supply, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpu_supply_tr_t0, gpu_semiconductor_supply, theater_ratio, 0, 0.32).
narrative_ontology:measurement(gpu_supply_tr_t2, gpu_semiconductor_supply, theater_ratio, 2, 0.38).
narrative_ontology:measurement(gpu_supply_tr_t4, gpu_semiconductor_supply, theater_ratio, 4, 0.47).
narrative_ontology:measurement(gpu_supply_tr_t6, gpu_semiconductor_supply, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(gpu_supply_be_t0, gpu_semiconductor_supply, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(gpu_supply_be_t2, gpu_semiconductor_supply, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(gpu_supply_be_t4, gpu_semiconductor_supply, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(gpu_supply_be_t6, gpu_semiconductor_supply, base_extractiveness, 6, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpu_semiconductor_supply, resource_allocation).
narrative_ontology:boltzmann_floor_override(gpu_semiconductor_supply, 0.18).
narrative_ontology:affects_constraint(gpu_semiconductor_supply, rare_earth_element_supply).
narrative_ontology:affects_constraint(gpu_semiconductor_supply, computing_power_concentration).
narrative_ontology:affects_constraint(gpu_semiconductor_supply, ai_research_accessibility).

% DUAL FORMULATION NOTE:
% GPU supply is upstream of three distinct constraints: (1) rare earth element supply (physical supply limit), (2) computing power concentration (institutional concentration of frontier research capacity), and (3) AI research accessibility (epistemic inequality). The GPU constraint bridges physical scarcity and institutional allocation — the extraction mechanism operates through allocation decisions, not raw material limits.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpu_semiconductor_supply, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
