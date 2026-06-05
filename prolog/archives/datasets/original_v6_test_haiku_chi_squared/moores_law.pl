% ============================================================================
% CONSTRAINT STORY: moores_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_moores_law, []).

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
 *   constraint_id: moores_law
 *   human_readable: Moore's Law as an Industrial Convention
 *   domain: technological/economic
 *
 * SUMMARY:
 *   Moore's Law—the empirical observation that transistor density doubles
 *   approximately every two years—has become far more than a technical
 *   description. It functions as an industrial coordination mechanism, a
 *   legitimating narrative for capital investment, and a suppression
 *   mechanism for alternative computing approaches. From the semiconductor
 *   incumbents' perspective, Moore's Law is coordination (Rope): it provides
 *   a predictable roadmap that enables supply-chain synchronization and
 *   justifies multi-billion-dollar capital expenditures. From the perspective
 *   of alternative fabrication technologies and materials innovators, it is
 *   pure extraction (Snare): they cannot secure funding or manufacturing
 *   partnerships without alignment with the Moore's Law roadmap. For fabless
 *   design companies, it is a mixed hybrid (Tangled Rope): they benefit from
 *   free transistor density gains but are constrained by forced redesign
 *   cycles. As performance gains have increasingly decoupled from transistor
 *   density growth—now driven more by architectural innovation, chiplets, and
 *   software optimization—Moore's Law has become increasingly theatrical
 *   (Piton): the narrative persists in roadmaps and investor expectations
 *   despite weakening physical basis. At the civilizational analytical level,
 *   one might naturalize Moore's Law as an immutable consequence of physics
 *   and economics (Mountain), but the structural data reveals this as a false
 *   summit: the extraction and suppression scores (0.58 and 0.62) are
 *   inconsistent with natural law classification. Moore's Law is a contingent
 *   institutional arrangement that naturalizes specific technology choices
 *   and suppresses alternatives that might offer superior energy efficiency,
 *   sustainability, or specialization.
 *
 * KEY AGENTS:
 *   - Semiconductor incumbents (Intel, TSMC, Samsung): Primary beneficiaries (institutional/arbitrage) — capture arbitrage value through proprietary optimizations while Moore's Law baseline ensures competitive pressure justifies ongoing investment
 *   - Capital equipment vendors (ASML, Applied Materials, Lam Research): Secondary beneficiaries (institutional/arbitrage) — demand for next-generation lithography, deposition, and metrology tools is driven by Moore's Law roadmap
 *   - Alternative fabrication technologies (photonic, neuromorphic, quantum substrates): Primary victims (powerless/trapped) — cannot secure funding or manufacturing partnerships without alignment to Moore's Law trajectory
 *   - Materials science innovators (graphene, perovskites, gallium nitride): Primary victims (powerless/trapped) — silicon-centric ecosystem suppresses alternative materials through institutional lock-in
 *   - Fabless design companies (ARM, Qualcomm, AMD): Secondary actors (moderate/constrained) — benefit from free transistor density growth but constrained by forced redesign cycles
 *   - Sustainable computing coalition (environmental advocates, longevity-focused firms): Organized victims (organized/constrained) — e-waste and rare-earth extraction embedded in 18-month refresh cycles
 *   - Institutional theater (semiconductor roadmap process, standards bodies): Analytical/piton observer — maintains Moore's Law narrative despite decoupling of transistor count from actual performance gains
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(moores_law, 0.58).
domain_priors:suppression_score(moores_law, 0.62).
domain_priors:theater_ratio(moores_law, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(moores_law, extractiveness, 0.58).
narrative_ontology:constraint_metric(moores_law, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(moores_law, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(moores_law, tangled_rope).
narrative_ontology:human_readable(moores_law, "Moore's Law as an Industrial Convention").
narrative_ontology:topic_domain(moores_law, "technological/economic").

domain_priors:requires_active_enforcement(moores_law).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(moores_law, semiconductor_incumbents).
narrative_ontology:constraint_beneficiary(moores_law, device_manufacturers).
narrative_ontology:constraint_beneficiary(moores_law, capital_equipment_vendors).
narrative_ontology:constraint_victim(moores_law, competing_fabrication_technologies).
narrative_ontology:constraint_victim(moores_law, materials_science_innovation).
narrative_ontology:constraint_victim(moores_law, sustainable_computing_development).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALTERNATIVE FABRICATION TECHNOLOGIES (SNARE) — Trapped by Moore's Law trajectory as the funding and standard-setting baseline. Competing approaches (photonic computing, neuromorphic chips, quantum substrates, 3D stacking) cannot secure equivalent capital investment without matching the doubling narrative. No exit: funding agencies, industry partnerships, and markets all condition support on alignment with Moore's Law roadmaps. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.97.
constraint_indexing:constraint_classification(moores_law, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MATERIALS SCIENCE INNOVATION (SNARE) — Trapped by the silicon-centric roadmap. Novel materials (gallium nitride, graphene, perovskites) face suppression through institutional lock-in: foundries, lithography vendors, and design-tool providers are optimized for silicon. Novel materials require new manufacturing ecosystems, but ecosystems require capital, which requires Moore's Law alignment. Trapped: innovators cannot exit without abandoning institutional support. d≈0.88, f(d)≈1.32, σ=1.2 → χ≈0.94.
constraint_indexing:constraint_classification(moores_law, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: FABLESS DESIGN COMPANIES (TANGLED ROPE) — Benefit from Moore's Law (free transistor density growth enables feature-rich products without design innovation) but also constrained by it (must ship on the Moore's Law cadence or lose competitive positioning; cannot customize at slower or different scaling paths). Coordination function: Moore's Law provides a predictable design target. Extraction: Moore's Law imposes a 18-24 month redesign cycle whether or not market needs it. d≈0.58, f(d)≈0.70, σ=1.0 → χ≈0.41.
constraint_indexing:constraint_classification(moores_law, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SEMICONDUCTOR INCUMBENTS (ROPE) — Primary beneficiaries. Moore's Law serves as a coordination mechanism: it provides a standardized, predictable roadmap that justifies capital spending, synchronizes industry-wide tool development, and enables supply-chain planning. Incumbents capture arbitrage: they can pursue proprietary optimizations (FinFET, EUV, chiplets) while the Moore's Law baseline guarantees competitive pressures and ensures other firms must also invest in the race. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.05. Net coordination benefit.
constraint_indexing:constraint_classification(moores_law, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CAPITAL EQUIPMENT VENDORS (ROPE) — Benefit from Moore's Law as a demand driver. The doubling roadmap justifies continuous billion-dollar capital equipment refreshes: lithography tools (EUV steppers), deposition systems, metrology platforms. Moore's Law creates predictable replacement cycles. Vendors have arbitrage: ASML, Applied Materials, Lam Research can innovate on proprietary solutions while the Moore's Law baseline guarantees market demand for the next generation. d≈0.10, f(d)≈-0.08, σ=1.1 → χ≈-0.05.
constraint_indexing:constraint_classification(moores_law, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: MOORE'S LAW AS INSTITUTIONAL THEATER (PITON) — From a long-term institutional view, Moore's Law is increasingly performative. Physical scaling (smaller transistors) has slowed or halted; performance gains now come from architecture, chiplet design, and software optimization. Yet the 'doubling' narrative persists in roadmaps, conference abstracts, and investor expectations despite reduced connection to transistor count. Theater ratio (0.68) captures the growing gap between the stated metric (transistor count) and actual performance/value delivery. d≈0.12, f(d)≈-0.06, σ=1.2 → χ≈-0.04. The piton classification reveals institutional inertia: Moore's Law persists as a legitimating narrative even as its physical basis weakens.
constraint_indexing:constraint_classification(moores_law, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: SUSTAINABLE COMPUTING COALITION (TANGLED ROPE) — Organized actors (academic researchers, environmental advocates, energy-efficiency-focused firms) see Moore's Law as both enabling and constraining. Coordination benefit: Moore's Law energy efficiency gains reduce per-computation power draw. Extraction burden: Moore's Law forces continuous hardware refresh cycles, creating e-waste and embedding resource extraction (rare earths, conflict minerals) in every 18-month redesign. Organized exit option: alternative efficiency metrics (FLOPS per watt, chip longevity, repairability) could displace transistor count. d≈0.45, f(d)≈0.48, σ=1.2 → χ≈0.34.
constraint_indexing:constraint_classification(moores_law, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / PHYSICAL LIMITS VIEW (MOUNTAIN) — From the civilizational analytical perspective, Moore's Law appears to be a natural law: the observation that semiconductor density increases with time is a consequence of fundamental physics (quantum tunneling limits, photon wavelength constraints, thermal management) and economic optimization. One cannot simply 'exit' the laws of physics. However, base properties (ε=0.58, suppression=0.62, theater=0.68) contradict the mountain classification. The engine will classify this as a false summit: what appears as natural law is actually a contingent industrial convention that naturalizes specific technology choices and suppresses alternatives. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.67.
constraint_indexing:constraint_classification(moores_law, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(moores_law_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(moores_law, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(moores_law, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(moores_law, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(moores_law, TR),
    TR >= 0.70.

:- end_tests(moores_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Moore's Law forces competing technologies to either align with the silicon roadmap or be starved of capital and manufacturing partnerships. The constraint extracts from alternative paths by channeling resources exclusively to silicon scaling. However, the extraction is not maximal (≥0.66 for snare) because the coordination function is genuine: Moore's Law does provide real benefits to the broader ecosystem through predictable scaling and supply-chain synchronization. Suppression (0.62): Moderate-high. Barriers to alternative computing are significant but not absolute. Photonic computing, neuromorphic chips, and quantum substrates have viable R&D programs and niche deployments, but capital is highly concentrated in silicon. The suppression is institutional (funding gatekeeping, standardization bodies) rather than physical. Theater ratio (0.68): Moderate-high, and rising. Physical scaling has slowed (each process node requires exponential capital increases relative to transistor gains). Performance improvements now come primarily from architectural changes (chiplets, cache hierarchies, instruction-level parallelism) and software optimization, not from transistor count doubling. Yet the 'doubling' narrative persists in roadmaps and conference abstracts. The rise from 0.38 to 0.68 over 30 years (probably 1995-2025) captures the growing gap between stated metric (transistors) and actual value delivery.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates profound perspectival divergence. Semiconductor incumbents see a pure coordination mechanism (Rope): Moore's Law enables supply-chain planning and justifies capital spending with minimal extraction. Alternative technologies see snare: they are trapped by institutional suppression despite technical viability. Fabless companies see mixed benefits and constraints (Tangled Rope): free density gains but forced redesign cycles. The sustainable computing coalition sees tangled rope with unsustainable side effects: architectural performance gains are real, but the constraint forces hardware refresh cycles that embed e-waste and resource extraction. The institutional theater perspective (Piton) reveals that Moore's Law has become increasingly disconnected from its physical substrate: the narrative persists through inertia despite weakening empirical basis. The analytical observer risks naturalizing this contingent arrangement as physical law (false Mountain), which the engine's classification catches as inconsistent with the structural data (extractiveness > 0.46). The perspectival gap is not an error—it reflects the genuine, asymmetric structural impact of the constraint on different actors.
 *
 * DIRECTIONALITY LOGIC:
 *   Semiconductor incumbents: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; arbitrage exit means they can optimize proprietary paths while the Moore's Law baseline guarantees competitive pressure justifies ongoing investment. Alternative technologies: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction; cannot exit the Moore's Law roadmap without abandoning institutional support (funding, partnerships, standards). Fabless companies: Victim + constrained (with moderate power to negotiate) → d≈0.58, f(d)≈0.70. Moderate extraction; they can survive by following the Moore's Law cadence, but cannot pursue independent paths. Capital equipment vendors: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.08. Net beneficiary; demand for next-gen tools is guaranteed by Moore's Law roadmap, and they have arbitrage (proprietary tool innovations). Sustainable computing coalition: Organized victim + constrained → d≈0.45, f(d)≈0.48. Moderate extraction with organizational capacity to negotiate; some alternatives (energy-efficient designs) are compatible with Moore's Law, but the constraint still forces wasteful refresh cycles. The institutional theater observer: d≈0.72, f(d)≈1.15. High analytical impact; reveals that the naturalizing narrative (Mountain view) is actually contingent institutional arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: Moore's Law resolves the classification ambiguity by distinguishing genuine coordination from institutionalized extraction. The question: Is this a pure coordination mechanism (Rope) or an extraction regime (Snare/Tangled Rope) disguised as coordination? The mandatrophy is resolved by examining the exit options and beneficiary/victim asymmetry. If Moore's Law were pure coordination, we would expect: (1) meaningful exit options for alternative technologies, (2) symmetric benefits across the ecosystem, (3) stable theater ratio (function remains tightly coupled to actual performance). Reality: (1) Alternative technologies have no viable exit—funding gatekeeping enforces Moore's Law alignment; (2) Beneficiaries (incumbents, equipment vendors) extract asymmetrically from victims (alternative tech); (3) Theater ratio rising from 0.38 to 0.68 indicates the coordination function has degraded while the extraction mechanism persists. The constraint is therefore Tangled Rope (has both genuine coordination and asymmetric extraction), not pure Rope. The piton perspective reveals that as the coordination function weakens (performance decouples from transistor scaling), the constraint increasingly relies on institutional inertia and narrative power to maintain extraction. The false summit (Mountain) is caught by the structural data: if Moore's Law were physical law, it would not require suppression of alternatives or theatrical narrative maintenance. The mandatrophy confirms: Moore's Law is a degrading hybrid (Tangled Rope trending toward Piton-like theatrical maintenance) that has successfully naturalized contingent technology choices as inevitable progress.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physical_limit_threshold,
    'At what transistor gate length does classical physics collapse into quantum tunneling regime that makes further scaling economically infeasible?',
    'Empirical characterization of quantum tunneling current at various gate lengths; cost analysis of error correction overhead; comparison of forward-scaling costs vs architectural innovation costs',
    'If threshold < 1 nanometer and imminent: Moore''s Law is approaching hard physical boundary, suggesting classification shift toward Mountain. If threshold is soft (can be pushed with exotic materials/quantum computing): Moore''s Law remains institutionally contingent (Tangled Rope/Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(physical_limit_threshold, empirical, 'Whether further scaling faces hard physical limits or soft economic constraints').

omega_variable(
    alternative_compute_viability,
    'Can heterogeneous computing (photonic, neuromorphic, quantum, analog) achieve equivalent performance/watt/cost without Moore''s Law silicon scaling?',
    'Benchmark studies comparing energy efficiency and TCO for specialized workloads (AI inference, signal processing, HPC); market adoption data for non-silicon compute platforms; venture capital deployment in alternative compute vs silicon',
    'If alternatives are viable: Moore''s Law suppression of competing technologies is an extraction mechanism (Snare victim → Tangled Rope or Snare classifications confirmed). If alternatives remain marginal despite maturity: Moore''s Law coordination function is genuine (Rope classification gains support).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_compute_viability, empirical, 'Whether alternative compute architectures can displace Moore''s Law silicon scaling').

omega_variable(
    performance_scaling_decoupling,
    'What fraction of performance gains now comes from architectural innovation (chiplets, caches, speculative execution) vs transistor density, and is the decoupling accelerating?',
    'Detailed performance modeling of recent CPU/GPU generations; decomposition of clock speed, IPC, and efficiency improvements; correlation of actual performance gains with transistor count doubling',
    'If architectural gains dominate and are accelerating: the ''transistor count doubling'' narrative is increasingly theatrical (Piton classification confirmed). If transistor density still drives 60%+ gains: coordination function remains real (Rope/Tangled Rope retain explanatory power).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(performance_scaling_decoupling, empirical, 'Degree to which performance scaling has decoupled from transistor density growth').

omega_variable(
    institutional_coordination_necessity,
    'Would semiconductor capital investment and tool development be significantly lower or slower without the Moore''s Law doubling narrative as a coordination signal?',
    'Counterfactual analysis using game theory (would equipment vendors have incentive to develop EUV/high-NA tools without Moore''s Law roadmap?); historical analysis of adoption delays when Moore''s Law predictions faltered; interviews with capital allocation decision-makers',
    'If coordination signal is essential: Moore''s Law is a Rope from the institutional perspective (coordination function is genuine). If capital would flow without it (e.g., competitive pressure alone): Moore''s Law is a Snare/Tangled Rope (extraction mechanism without genuine coordination need).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_coordination_necessity, conceptual, 'Whether Moore''s Law narrative is essential for semiconductor capital coordination').

omega_variable(
    mandate_shift_possibility,
    'Can semiconductor industry collectively shift to alternative performance metrics (FLOPS-per-watt, chip lifetime, repairability, supply-chain equity) without triggering fragmentation and loss of standardization?',
    'Policy design studies; simulation of industry transition to alternative metrics; analysis of standardization bodies (SEMI, IEEE) capacity to enforce metric migration; assessment of stranded capital if Moore''s Law is abandoned',
    'If viable without fragmentation: Moore''s Law extraction can be reformed (Scaffold classification becomes possible). If mandated shift triggers incompatibility and re-fragmentation: Moore''s Law persists as the lesser evil (Tangled Rope/Snare persists).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_shift_possibility, preference, 'Whether the industry can collectively migrate to alternative performance standards').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(moores_law, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(moore_tr_t0, moores_law, theater_ratio, 0, 0.38).
narrative_ontology:measurement(moore_tr_t15, moores_law, theater_ratio, 15, 0.52).
narrative_ontology:measurement(moore_tr_t30, moores_law, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(moore_be_t0, moores_law, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(moore_be_t15, moores_law, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(moore_be_t30, moores_law, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(moores_law, resource_allocation).
narrative_ontology:affects_constraint(moores_law, semiconductor_manufacturing_lock_in).
narrative_ontology:affects_constraint(moores_law, rare_earth_extraction_dependency).
narrative_ontology:affects_constraint(moores_law, e_waste_accumulation_cycle).
narrative_ontology:affects_constraint(moores_law, alternative_compute_architecture_suppression).

% DUAL FORMULATION NOTE:
% Moore's Law as empirical observation (ε≈0.05, Mountain) vs Moore's Law as industrial convention/coordination mechanism (ε=0.58, Tangled Rope) are structurally distinct constraints. The observation that transistor density increases with time is physically grounded; the social coordination/extraction system that crystallizes around 'doubling every two years' as a binding constraint on capital allocation is institutional. These stories should be linked: the institutional constraint (moores_law) is downstream of and depends on the observational claim (transistor_density_growth). The decomposition enables analysis of what happens if the observational premise (doubling rate slows or plateaus) is falsified—does the institutional constraint persist through pure theatrical force, or does it reform? Current empirical evidence (transistor scaling slowdown, architectural innovation dominance) suggests the institutional constraint increasingly relies on theater and institutional inertia rather than physical coordination necessity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(moores_law, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
