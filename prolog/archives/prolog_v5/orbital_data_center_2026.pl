% ============================================================================
% CONSTRAINT STORY: orbital_data_center_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orbital_data_center_2026, []).

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
 *   constraint_id: orbital_data_center_2026
 *   human_readable: SpaceX Million-Satellite Orbital Compute Network
 *   domain: technological/geopolitical
 *
 * SUMMARY:
 *   SpaceX's proposed million-satellite orbital compute network represents a
 *   structural constraint that operates simultaneously as coordination
 *   mechanism (solving genuine latency and compute distribution problems),
 *   extraction mechanism (capturing regulatory arbitrage and spectrum
 *   dominance), and tragedy of the commons (imposing uncompensated costs on
 *   orbital sustainability and ground science). The constraint exhibits
 *   genuine tangled rope structure: it solves real problems
 *   (latency-sensitive workloads, global compute edge, disaster-resilient
 *   distributed inference) while simultaneously creating asymmetric
 *   extraction through first-mover regulatory advantage, spectrum capture,
 *   and imposition of debris/collision risks on competing operators and
 *   future space utilization. Unlike theatrical constraints (piton) or pure
 *   extraction (snare), this constraint has a real coordination function that
 *   persists even after removing the extractive layer — a network of 100,000
 *   satellites could provide the same latency benefit with lower collision
 *   risk. The extractiveness derives not from fundamental physics but from
 *   SpaceX's institutional positioning and regulatory landscape that favors
 *   first-movers. The theater ratio (0.55) reflects that much public
 *   discourse about the network focuses on its transformative potential
 *   (connectivity for underserved regions, disaster resilience, global
 *   latency revolution) while underemphasizing the asymmetric extraction
 *   mechanism and orbital commons costs.
 *
 * KEY AGENTS:
 *   - SpaceX Infrastructure Operator: Primary beneficiary (institutional/arbitrage) — captures first-mover advantage in orbital compute, regulatory arbitrage, spectrum dominance, and high-margin latency-sensitive compute customers
 *   - High-Margin Compute Customers: Secondary beneficiary (powerful/arbitrage) — algorithmic trading firms, autonomous vehicle companies, real-time AI inference platforms; benefit from access to distributed edge compute below terrestrial latency thresholds
 *   - Orbital Commons and Ground Astronomy: Primary victim (powerless/trapped) — scientific research community, future space operators; experience light pollution, radio frequency interference, collision risk from dense constellation; cannot exit or organize collectively
 *   - Competing Satellite Operators: Secondary victim (moderate/trapped) — OneWeb, Kuiper, China's Hongyun; face collision risk, spectrum interference, and regulatory disadvantage from SpaceX's first-mover position in orbital real estate
 *   - Regulatory Authorities (FCC, ITU, ESA): Mixed actor (institutional/constrained) — must balance innovation incentives with orbital sustainability; face pressure from SpaceX and from space sustainability advocates; cannot ban the network but cannot ignore externalities
 *   - Space Sustainability Coalition: Organized advocate (organized/constrained) — international bodies, debris remediation companies, astronomy advocacy groups; building alternative governance frameworks (deorbiting mandates, collision avoidance standards, spectrum sharing protocols) with sunset logic
 *   - Legacy Satellite Telecom Industry: Degraded incumbent (institutional/arbitrage) — Intelsat, SES, Viasat; experience constraint as erosion of spectrum monopoly and latency-sensitive service market; persist through regulatory grandfather clauses and sunk capital, not through genuine advantage
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — recognizes genuine coordination function but also real extractive mechanism; identifies that constraint is not a false summit but a genuine tangled rope with persistent structural asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orbital_data_center_2026, 0.58).
domain_priors:suppression_score(orbital_data_center_2026, 0.68).
domain_priors:theater_ratio(orbital_data_center_2026, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orbital_data_center_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(orbital_data_center_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(orbital_data_center_2026, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orbital_data_center_2026, tangled_rope).
narrative_ontology:human_readable(orbital_data_center_2026, "SpaceX Million-Satellite Orbital Compute Network").
narrative_ontology:topic_domain(orbital_data_center_2026, "technological/geopolitical").

domain_priors:requires_active_enforcement(orbital_data_center_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orbital_data_center_2026, spacex_infrastructure_operator).
narrative_ontology:constraint_beneficiary(orbital_data_center_2026, high_margin_compute_customers).
narrative_ontology:constraint_victim(orbital_data_center_2026, orbital_commons_integrity).
narrative_ontology:constraint_victim(orbital_data_center_2026, ground_based_astronomy).
narrative_ontology:constraint_victim(orbital_data_center_2026, competing_satellite_operators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORBITAL COMMONS & GROUND ASTRONOMY (SNARE) — Cannot exit the constraint; bears full cost of orbital pollution, light pollution, and radio frequency interference. The scientific research community and future space operators have no exit option and no ability to organize collectively against a single dominant actor. Maximum experienced extraction — abstract commons and diffuse scientific interests are powerless.
constraint_indexing:constraint_classification(orbital_data_center_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMPETING SATELLITE OPERATORS (SNARE) — Trapped in orbital mechanics and spectrum allocation; must operate in the same contested space. SpaceX's scale (1M satellites) creates collision risk, spectrum congestion, and Kessler syndrome exposure that smaller operators cannot escape. No meaningful exit — atmospheric reentry eventually, but that's thermodynamically dictated, not a choice. High suppression: FCC and ITU regulatory frameworks favor first-mover advantage and provide minimal protection for later entrants.
constraint_indexing:constraint_classification(orbital_data_center_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: SPACEX & HIGH-MARGIN COMPUTE CUSTOMERS (ROPE) — Primary beneficiaries. SpaceX experiences the constraint as coordination: deploying a global satellite compute layer solves the latency problem for latency-sensitive workloads (algorithmic trading, autonomous vehicle neural networks, real-time AI inference). Customers benefit from access to distributed edge compute at SpaceX's pricing. Net beneficiary relationship — extraction flows toward these agents.
constraint_indexing:constraint_classification(orbital_data_center_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AUTHORITIES (TANGLED ROPE) — FCC, ITU, and ESA face conflicting objectives: enabling innovation (beneficiary dynamic) while protecting orbital commons and ground science (victim dynamic). Constrained exit — cannot simply ban the network (sovereign pressure, economic consequences) but cannot ignore debris/collision risks. Active enforcement required: spectrum management, collision avoidance rules, deorbiting mandates. Mixed coordination function (enabling global compute infrastructure) and asymmetric extraction (SpaceX's regulatory arbitrage from favoring first-movers).
constraint_indexing:constraint_classification(orbital_data_center_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SPACE SUSTAINABILITY COALITION (SCAFFOLD) — Organized efforts (active debris removal mandate, international collision avoidance standards, spectrum sharing protocols) are building alternative orbital governance frameworks with sunset logic: as autonomous deorbiting, collision avoidance AI, and spectrum efficiency standards mature, the constraint's suppression mechanism (first-mover regulatory advantage) loses force. The coalition constrains but sees an exit path through technological and governance innovation. Has sunset clause rationale: mandated deorbiting timelines and next-generation spectrum allocation frameworks (scheduled 2030-2035) create pathway to more equitable orbital commons governance.
constraint_indexing:constraint_classification(orbital_data_center_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY SATELLITE TELECOM (PITON) — Older orbital infrastructure (Intelsat, SES, Viasat) persists through regulatory capture and sunk costs despite declining functional advantage. Their orbital spectrum rights are degraded by interference from dense constellations; their monopoly on latency-sensitive services is eroded by SpaceX's constellation. The constraint on them is maintained by institutional inertia and regulatory grandfather clauses, not by genuine competitive advantage. Theater ratio high: legacy operators maintain regulatory presence and argue for 'fairness' while their underlying business model is inert.
constraint_indexing:constraint_classification(orbital_data_center_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/universal timescale, the network solves genuine coordination problems (global latency, compute edge distribution, disaster resilience) that would be valuable for humanity at scale. But the extractive layer is real: SpaceX's first-mover advantage, regulatory arbitrage, and spectrum capture create asymmetric value capture. The constraint is genuinely mixed — real coordination function + real extraction mechanism. Not a false summit (unlike the verification bottleneck example) because the beneficiary/victim split is stable across observables.
constraint_indexing:constraint_classification(orbital_data_center_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orbital_data_center_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(orbital_data_center_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(orbital_data_center_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(orbital_data_center_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(orbital_data_center_2026, TR),
    TR >= 0.70.

:- end_tests(orbital_data_center_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting SpaceX's institutional dominance and first-mover regulatory advantage without absolute monopoly. The network solves real problems (latency for algorithmic trading, edge compute distribution), so some extraction flows toward beneficiaries as fair reward for innovation. But the regulatory arbitrage (FCC favoring first-movers), spectrum capture (limited LEO spectrum slots), and imposition of uncompensated externalities (debris risk, astronomy interference) on competitors and commons indicate extraction above the coordination-only level. The trajectory is upward (0.35 → 0.58 over 6 years) as deployment density increases and market power consolidates. Suppression (0.68): High. Barriers to competing operators include orbital slot scarcity, spectrum allocation governed by ITU/FCC with first-mover advantage, collision risk that scales with constellation density, and regulatory capture (SpaceX's influence on standards-setting). Competing operators face genuine suppression: they cannot deploy equally-sized constellations without coordinating with SpaceX or accepting significant collision/interference risk. Theater ratio (0.55): Moderate. The public discourse emphasizes transformative benefits (global connectivity, latency revolution, space-based AI) while underemphasizing extractive mechanisms and externalities. SpaceX's marketing narrative focuses on coordination benefits; sustainability costs are backgrounded. However, the theater is not dominant (as in piton) because the underlying coordination function is real — removing the theater would not eliminate the latency benefit. The theater moderately amplifies the apparent social value relative to actual asymmetric extraction.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals that the constraint is NOT a false summit (unlike the verification bottleneck's mountain perspective). Even the analytical observer recognizes genuine tangled rope structure: the coordination function (global latency, distributed edge compute) is real AND the extraction mechanism (first-mover advantage, spectrum capture, externalized orbital costs) is real. This is not naturalization of a contingent institutional arrangement — it is a genuine mixed dynamic. The gap is not between snare and rope perspectives of the same underlying phenomenon, but between different agents' actual structural positions. SpaceX genuinely benefits; competitors genuinely suffer; the commons genuinely pays externality costs; the coordination benefit genuinely exists. The constraint is not resolvable by reframing (as the verification bottleneck might be through open science); it is resolvable only through changing the structural conditions: multiple competing mega-constellations would reduce SpaceX's extraction power (increase competitors' exit options); automated collision avoidance would reduce externality costs; equitable spectrum allocation would reduce regulatory arbitrage. Until these structural changes occur, the tangled rope classification is stable across all observables.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position: (1) SpaceX as institutional beneficiary with arbitrage options (low d, ~0.10-0.15) experiences the constraint as enabling (negative effective extraction) — they control the mechanism. (2) Competing operators as powerless victims with trapped options (high d, ~0.80-0.95) experience high effective extraction — they cannot escape orbital crowding. (3) Ground astronomy as powerless victim without organization (high d, ~0.85-0.95) experiences maximum extraction — they have no exit, no leverage, no compensation mechanism. (4) Regulatory authorities as institutional actors with constrained options (medium-high d, ~0.55-0.65) experience moderate extraction — they must enforce rules against a powerful actor with regulatory influence. (5) Space sustainability coalition as organized actors with constrained options but exit pathways (medium d, ~0.40-0.50) experience moderate extraction with decreasing trend — their agency and sunset mechanism reduce effective χ. The directionality overrides for regulatory authorities reflect regulatory capture dynamics: the formal derivation might place FCC at low d (institutional/arbitrage), but institutional analysis of spectrum auction favoritism and SpaceX's regulatory influence suggests d should be increased to 0.55-0.65 (constrained by SpaceX's power despite formal institutional status).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by exhibiting genuine tangled rope structure: the beneficiaries (SpaceX, latency-sensitive compute customers) experience a coordination mechanism AND an extraction mechanism simultaneously. Removing the extraction layer (by forcing spectrum sharing, deorbiting liability, or competing mega-constellations) would preserve the coordination benefit. This is not 'mislabeling coordination as extraction' — both are present in the structure. The snare perspective (competing operators, ground astronomy) correctly identifies that THEY experience pure extraction; the rope perspective (SpaceX, customers) correctly identifies coordination benefits; neither is wrong. The mandatrophy is resolved by recognizing that the constraint has different extractive force for different agents: beneficiaries experience coordination, victims experience snare-like dynamics. The tangled rope is the invariant classification that captures this asymmetry. The space sustainability coalition's scaffold perspective adds temporal structure: the constraint's extraction force declines as deorbiting standards, collision avoidance AI, and spectrum efficiency improve. The analytical observer confirms this: the constraint is genuinely mixed (coordination + extraction) and genuinely temporal (sunset via governance maturation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kessler_syndrome_threshold,
    'What satellite density triggers irreversible Kessler syndrome in LEO? Does 1M satellites cross the threshold, and is it reversible with active deorbiting?',
    'Long-term orbital mechanics simulation; empirical tracking of collision cascade models under different deployment and deorbiting scenarios; statistical analysis of debris population over 10-30 year timescale',
    'If threshold crossed and irreversible: constraint becomes a mountain (immutable lock-in on orbital inaccessibility). If threshold not crossed: constraint remains tangled rope (extractive but reversible through deorbiting protocols).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kessler_syndrome_threshold, empirical, 'Whether 1M satellites triggers irreversible Kessler syndrome').

omega_variable(
    spectrum_coexistence_technical,
    'Can dense satellite mega-constellations coexist with ground-based radio astronomy and 5G/6G terrestrial networks without degraded performance? What technical standards would enable true coexistence vs enforced suppression?',
    'Interference measurement campaigns; frequency coordination trials; assessment of cognitive radio and dynamic spectrum access feasibility at orbital scales',
    'If true coexistence possible: suppression index drops to 0.35-0.45, constraint reclassifies as rope or scaffold. If suppression inherent: remains tangled rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spectrum_coexistence_technical, empirical, 'Whether spectrum coexistence is technically achievable').

omega_variable(
    geopolitical_first_mover_lock,
    'Does SpaceX''s first-mover orbital dominance create permanent geopolitical asymmetry (US control of global compute layer), or can competing mega-constellations (China, EU, India) achieve functional parity?',
    'Deployment timelines and capability parity assessment; regulatory tracking of competing constellation authorizations; analysis of whether orbital real estate (orbital slots, spectrum) creates zero-sum competition or multiplicative value',
    'If lock-in permanent: constraint is extractive monopoly (snare from global south/non-US perspective). If parity achievable: constraint is temporary first-mover advantage (scaffold with sunset).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(geopolitical_first_mover_lock, conceptual, 'Whether SpaceX first-mover advantage creates permanent geopolitical lock-in').

omega_variable(
    compute_workload_viability,
    'Which compute workloads are genuinely latency-constrained enough to justify orbital deployment? (Algorithmic trading < 100ms, autonomous vehicles, real-time AI inference, or speculative expansion into non-viable use cases?)',
    'Market analysis of actual latency-sensitive workloads; customer willingness-to-pay analysis; comparison with ground-based edge compute alternatives (fiber network acceleration, local inference)',
    'If viable for narrow high-margin segment only: constraint is stable tangled rope. If market is speculative: extractiveness may decline over time (constraint degrades toward piton).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compute_workload_viability, empirical, 'Whether orbital compute addresses genuine latency bottlenecks').

omega_variable(
    deorbiting_enforcement,
    'Are internationally-mandated satellite deorbiting timelines (e.g., 25-year LEO rule) actually enforced, or do they remain theatrical without teeth?',
    'Compliance tracking of historical deorbiting mandates; assessment of penalty structures and enforcement mechanisms; analysis of whether SpaceX and others have genuine incentive to deorbit',
    'If enforced: space sustainability coalition''s scaffold perspective is real, sunset clause has structural meaning. If theatrical: deorbiting becomes piton-like (maintained by protocol inertia, not genuine governance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deorbiting_enforcement, empirical, 'Whether deorbiting mandates are actually enforced').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orbital_data_center_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orbitdc_tr_t0, orbital_data_center_2026, theater_ratio, 0, 0.4).
narrative_ontology:measurement(orbitdc_tr_t3, orbital_data_center_2026, theater_ratio, 3, 0.48).
narrative_ontology:measurement(orbitdc_tr_t6, orbital_data_center_2026, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(orbitdc_be_t0, orbital_data_center_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(orbitdc_be_t3, orbital_data_center_2026, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(orbitdc_be_t6, orbital_data_center_2026, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orbital_data_center_2026, global_infrastructure).
narrative_ontology:affects_constraint(orbital_data_center_2026, leo_debris_accumulation).
narrative_ontology:affects_constraint(orbital_data_center_2026, terrestrial_spectrum_coexistence).
narrative_ontology:affects_constraint(orbital_data_center_2026, geopolitical_compute_dominance).
narrative_ontology:affects_constraint(orbital_data_center_2026, ground_based_astronomy_interference).

% DUAL FORMULATION NOTE:
% The orbital compute network constraint decomposes into distinct structural claims: (1) Latency solution (genuine coordination, low ε) vs (2) Regulatory capture and first-mover advantage (extractive, ε ~ 0.55-0.60). The tangled rope classification integrates both claims — do not decompose into separate stories because they are structurally coupled: the coordination benefit only accrues because SpaceX's extractive advantage enables the network's deployment. The value creation (latency reduction) and value capture (first-mover rents) are inseparable in the real structure. Constraint family: upstream of debris accumulation, spectrum coexistence, and geopolitical AI compute concentration constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(orbital_data_center_2026, institutional, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
