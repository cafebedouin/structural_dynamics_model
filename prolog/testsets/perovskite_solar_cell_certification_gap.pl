% ============================================================================
% CONSTRAINT STORY: perovskite_solar_cell_certification_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_perovskite_solar_cell_certification_gap, []).

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
 *   constraint_id: perovskite_solar_cell_certification_gap
 *   human_readable: Perovskite Solar Cell Certification Gap
 *   domain: renewable_energy/materials_science
 *
 * SUMMARY:
 *   Perovskite solar cells (PSCs) have demonstrated laboratory efficiency
 *   (>30%) exceeding silicon cells (22-23%) over the past decade, yet remain
 *   absent from large-scale grid deployment. A primary structural barrier is
 *   the certification gap: IEC standards (IEC 61646, IEC 61730) were designed
 *   for crystalline silicon and apply identical testing protocols to
 *   perovskites. This constraint exhibits tangled rope structure: legitimate
 *   coordination function (communicating reliability requirements) combined
 *   with asymmetric extraction (certification costs and timeline delays that
 *   disproportionately disadvantage PSC developers). The beneficiary is
 *   incumbent silicon manufacturers, who gain extended market protection
 *   while certification bodies maintain institutional authority through
 *   mismatched standards. Victims include perovskite development teams facing
 *   15-25% budget consumption on certification, and the broader
 *   decarbonization timeline (delayed PSC deployment delays grid
 *   transformation). Theater ratio (0.68) reflects that compliance with
 *   silicon-derived protocols has become partially performative — test
 *   results do not reliably predict perovskite real-world performance because
 *   the material physics is fundamentally different. Emerging
 *   perovskite-specific protocols (ISOS, PVLAB) create a sunset pathway,
 *   making this a tangled rope with scaffold alternative rather than pure
 *   snare.
 *
 * KEY AGENTS:
 *   - Perovskite Development Teams: Primary victims (powerless/trapped) — startups and research groups locked into certification regime; face budget absorption (15-25% of R&D spending) and 3-5 year commercialization delays
 *   - Incumbent Silicon Manufacturers: Primary beneficiaries (institutional/arbitrage) — Sunwatt, JinkoSolar, First Solar; experience bottleneck as market protection; can exit through internal tandem research without loss
 *   - IEC Certification Bodies: Institutional enforcer (institutional/constrained) — TÜV Süd, Intertek, JET; coordinate standardization but extract through monopoly testing authority; constrained by institutional reputation
 *   - Standards Development Coalitions: Organized alternative (organized/constrained) — NREL, Fraunhofer-ISE, ISOS consortium; building perovskite-specific protocols with explicit sunset horizon (10-15 years)
 *   - Grid Regulators: Authority gatekeeper (institutional/arbitrage) — cite IEC standards in grid connection codes without evaluating perovskite-specific requirements; maintain bottleneck through regulatory reference
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangement as material physics inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(perovskite_solar_cell_certification_gap, 0.48).
domain_priors:suppression_score(perovskite_solar_cell_certification_gap, 0.62).
domain_priors:theater_ratio(perovskite_solar_cell_certification_gap, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(perovskite_solar_cell_certification_gap, extractiveness, 0.48).
narrative_ontology:constraint_metric(perovskite_solar_cell_certification_gap, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(perovskite_solar_cell_certification_gap, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(perovskite_solar_cell_certification_gap, tangled_rope).
narrative_ontology:human_readable(perovskite_solar_cell_certification_gap, "Perovskite Solar Cell Certification Gap").
narrative_ontology:topic_domain(perovskite_solar_cell_certification_gap, "renewable_energy/materials_science").

domain_priors:requires_active_enforcement(perovskite_solar_cell_certification_gap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(perovskite_solar_cell_certification_gap, incumbent_silicon_manufacturers).
narrative_ontology:constraint_beneficiary(perovskite_solar_cell_certification_gap, test_certification_bodies).
narrative_ontology:constraint_victim(perovskite_solar_cell_certification_gap, perovskite_developers).
narrative_ontology:constraint_victim(perovskite_solar_cell_certification_gap, grid_decarbonization_timeline).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PEROVSKITE DEVELOPERS (SNARE) — Trapped by certification requirements designed for silicon cells but applied uniformly to perovskite. Cannot exit the certification regime without losing market access. High extraction: certification costs consume 15-25% of development budgets; testing protocols are mismatched to material physics (silicon aging models don't apply to ion migration); timeline delays of 3-5 years block commercialization. Suppression is maximal — without IEC certification, no pathway to grid deployment.
constraint_indexing:constraint_classification(perovskite_solar_cell_certification_gap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SILICON MANUFACTURERS (ROPE) — Benefit from certification bottleneck that delays perovskite competition. Perceived constraint is coordination (communication of reliability standards). Arbitrage exit: can migrate to tandem cells or perovskite internally without loss. Low experienced extraction — the bottleneck protects their market position while appearing as a legitimate safety requirement.
constraint_indexing:constraint_classification(perovskite_solar_cell_certification_gap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: CERTIFICATION BODIES (TANGLED ROPE) — Genuinely coordinate standardization (coordination function) but also extract through monopoly on testing authority. Constrained by institutional reputation: relaxing standards risks credibility; maintaining mismatched standards avoids the cost of developing perovskite-specific protocols. Active enforcement: certification procedures are mandated by regulators; the gap persists because revising standards requires consensus among competing test labs and manufacturers.
constraint_indexing:constraint_classification(perovskite_solar_cell_certification_gap, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: STANDARDS COALITIONS (SCAFFOLD) — Emerging perovskite-specific standards (ISOS protocols, PVLAB methodologies) create alternative certification pathways with explicit sunset: once perovskite performance is validated over 10-year cycles and degradation mechanisms are understood, silicon-based testing becomes irrelevant. Organized agents (NREL, Fraunhofer, industry consortia) see the gap as a temporary coordination failure being solved. Low theater in new protocols — they measure actual degradation mechanisms rather than proxy reliability.
constraint_indexing:constraint_classification(perovskite_solar_cell_certification_gap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: REGULATORY MANDATE LEGACY (PITON) — Grid connection standards cite IEC 61646 (module performance) as a fixed requirement, but the citation persists through institutional inertia rather than technical necessity. The regulatory reference has become performative: regulators cite it to demonstrate due diligence, not because it accurately measures perovskite safety. Theater ratio high (0.68) — compliance is theater, not verification. The mandate persists because changing it requires bureaucratic coordination across multiple agencies, not because the standard functions.
constraint_indexing:constraint_classification(perovskite_solar_cell_certification_gap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a materials physics perspective, some certification gap is inherent: perovskite ion migration, halide segregation, and moisture sensitivity require fundamentally different test protocols than silicon's thermal and mechanical fatigue. The gap reflects irreducible differences in material behavior, not contingent institutional choice. However, this perspective risks naturalizing what is actually a reversible institutional arrangement — perovskite-specific certification is technically feasible and being implemented; the gap is not a law of nature but a coordination failure.
constraint_indexing:constraint_classification(perovskite_solar_cell_certification_gap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(perovskite_solar_cell_certification_gap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(perovskite_solar_cell_certification_gap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(perovskite_solar_cell_certification_gap, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(perovskite_solar_cell_certification_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(perovskite_solar_cell_certification_gap, TR),
    TR >= 0.70.

:- end_tests(perovskite_solar_cell_certification_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. Certification cost (€50k-300k per device type), timeline (3-5 years to certification vs 18-24 months for new silicon variants), and performance measurement misalignment (IEC protocols measure degradation modes irrelevant to perovskites) create significant extraction. However, extractiveness is not maximal (0.70+) because some testing is genuine coordination (communicating safety standards); the excess above coordination cost represents the incumbency rent. Suppression (0.62): High. Complete prohibition on grid deployment without IEC certification creates hard suppression (no alternative pathways until ISOS adoption); no voluntary exit option. But suppression is not maximal (1.0) because alternative standards are being developed and some markets (military, space) operate without grid codes. Theater ratio (0.68): High. Silicon-derived protocols are substantially performative for PSCs — they measure damp-heat resistance (silicon fatigue mode) and thermal cycling rather than ion migration, halide segregation, and moisture intrusion (PSC actual failure modes). The theater has increased as PSC complexity has outpaced silicon-model applicability. The measurement trajectory (0.50 → 0.68) shows degradation: earlier certification bodies performed genuine testing; current application has become more theatrical as misapplication of silicon protocols to perovskite has calcified.
 *
 * PERSPECTIVAL GAP:
 *   The perovskite developer sees snare: certification is an extractive hurdle with no coordination benefit for their specific material. Silicon manufacturers see rope: certification is legitimate coordination that happens to protect their market (perceived as natural competitive advantage). Certification bodies see tangled rope: genuine coordination (standardization) mixed with the rent of monopoly testing authority. The standards coalition sees scaffold: the bottleneck is temporary, being solved by perovskite-specific protocols with explicit sunset. Regulatory gatekeepers see piton: the IEC citation persists through institutional inertia (bureaucratic cost of changing regulations exceeds perceived benefit). The analytical observer risks seeing mountain: material physics dictates perovskite requires different protocols (true) and therefore the gap is natural/inevitable (false — the gap is a reversible institutional choice). The perspectival gap reveals that the constraint is not a law of nature but a coordination failure with a known exit path.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from each agent's structural position: their power level, exit capacity, and flow of extraction. Perovskite developers are trapped with no exit (d=0.95) — high f(d) → high experienced chi. Silicon manufacturers have arbitrage exit (d=0.05) — low f(d) → negative chi (they perceive coordination). Certification bodies are constrained but beneficiary-adjacent (d=0.40) — medium f(d). Standards coalitions have exit pathway (scaffold sunset, d=0.35) — lower f(d). Regulatory gatekeepers have institutional arbitrage (can cite alternative standards without cost, d=0.10) — very low f(d). The analytical observer at civilizational scale (d=0.72) experiences moderate extraction from the complexity opacity.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by distinguishing genuine coordination (communicating reliability standards) from extraction (monopoly testing authority + incumbency protection). IEC certification boards do coordinate (they produce standards that enable inter-lab communication). But the standards they produce are mismatched to perovskite physics, creating asymmetric extraction. The tangled rope classification captures this hybrid: coordination function + extraction overhead. The scaffolding of ISOS protocols demonstrates that perovskite-specific certification is technically feasible and not prohibited by material physics — the gap is institutional, not natural. The false summit risk (mountain perspective) occurs when analysts naturalize the current misalignment as inevitable given material differences. It is not inevitable: it is a reversible institutional choice that persists due to regulatory inertia and incumbency incentives. The mandatrophy is resolved by the scaffold: the sunset clause (perovskite validation over 10-year cycles) creates a clear endpoint where the gap closes and extraction falls to zero.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    silicon_bias_in_standards,
    'To what extent does the certification gap reflect genuine perovskite technical uncertainty versus deliberate incumbency protection dressed as safety?',
    'Compare actual field failure rates for certified silicon vs pre-commercial perovskite under identical environmental exposure; analyze certification body meeting minutes for technical reasoning vs market protection language',
    'If technical: gap is legitimate (mountain/scaffold). If deliberate: gap is pure extraction (snare/tangled rope). Mixed case likely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(silicon_bias_in_standards, empirical, 'Degree of silicon bias versus legitimate technical uncertainty in certification standards').

omega_variable(
    perovskite_degradation_reversibility,
    'Are perovskite degradation modes (ion migration, halide segregation) reversible through encapsulation and operating protocol, or do they represent irreducible limits?',
    'Long-term field deployment studies (5-10 years) comparing encapsulated perovskites under controlled vs real-world conditions; analysis of ion migration reversibility under thermal cycling',
    'If reversible: certification can be outcome-based (performance guarantees) rather than process-based (silicon-mimicking tests). If irreversible: silicon-derived protocols remain partially justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(perovskite_degradation_reversibility, empirical, 'Whether perovskite degradation modes are reversible or irreducible').

omega_variable(
    certification_cost_vs_market_barrier,
    'What fraction of perovskite commercialization delay is attributable to certification cost/timeline versus technical uncertainty, supply chain immaturity, or market competition from silicon?',
    'Survey of perovskite companies on certification bottleneck ranking; comparison of time-to-market for perovskite in certification-exempt markets (military, space, academic) vs regulated markets',
    'If certification is dominant barrier: extraction is severe (snare). If supply/market factors dominate: extraction is overstated (rope/scaffold).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(certification_cost_vs_market_barrier, empirical, 'Relative contribution of certification versus other commercialization barriers').

omega_variable(
    standards_coalition_effectiveness,
    'Will ISOS and other perovskite-specific protocols actually achieve regulatory acceptance and replace IEC requirements, or will they remain parallel alternatives indefinitely?',
    'Track regulatory citations of perovskite protocols in major markets (EU, US, China) over next 5-10 years; measure adoption rate in industry projects',
    'If accepted: scaffold sunset is real, extraction declines to zero. If not accepted: scaffold is aspirational, extraction persists indefinitely (constraint reclassifies as snare).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(standards_coalition_effectiveness, empirical, 'Whether new perovskite standards will achieve regulatory acceptance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(perovskite_solar_cell_certification_gap, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pero_tr_t0, perovskite_solar_cell_certification_gap, theater_ratio, 0, 0.5).
narrative_ontology:measurement(pero_tr_t3, perovskite_solar_cell_certification_gap, theater_ratio, 3, 0.62).
narrative_ontology:measurement(pero_tr_t6, perovskite_solar_cell_certification_gap, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(pero_be_t0, perovskite_solar_cell_certification_gap, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(pero_be_t3, perovskite_solar_cell_certification_gap, base_extractiveness, 3, 0.41).
narrative_ontology:measurement(pero_be_t6, perovskite_solar_cell_certification_gap, base_extractiveness, 6, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(perovskite_solar_cell_certification_gap, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(perovskite_solar_cell_certification_gap, 0.12).
narrative_ontology:affects_constraint(perovskite_solar_cell_certification_gap, silicon_tandem_hybrid_certification).
narrative_ontology:affects_constraint(perovskite_solar_cell_certification_gap, grid_decarbonization_timeline_constraint).
narrative_ontology:affects_constraint(perovskite_solar_cell_certification_gap, materials_science_verification_bottleneck).

% DUAL FORMULATION NOTE:
% The certification gap can be decomposed into two structurally distinct constraints: (1) perovskite technical validation (whether material survives 25-year lifetime), ε~0.15 (rope/mountain) and (2) certification monopoly (institutional gatekeeping of testing authority), ε~0.52 (snare/tangled rope). This story combines both. Upstream: materials_science_verification_bottleneck (generic replication lag, ε~0.40). Downstream: silicon_tandem_hybrid_certification (transition technologies facing dual standards).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(perovskite_solar_cell_certification_gap, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
