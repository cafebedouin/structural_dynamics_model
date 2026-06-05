% ============================================================================
% CONSTRAINT STORY: market_dominance_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_dominance_lock_in, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: market_dominance_lock_in
 *   human_readable: Market Dominance Lock-In: Network Effects and Switching Costs
 *   domain: economic/competition
 *
 * SUMMARY:
 *   Market dominance lock-in occurs when a firm achieves leadership through
 *   coordination mechanisms (network effects, ecosystem breadth, scale) and
 *   then leverages that position to extract rents through switching cost
 *   elevation, data control, API restrictions, and service degradation. The
 *   constraint exhibits a structural transition: what begins as pure
 *   coordination (Rope—the network is genuinely valuable and coordination of
 *   users is the benefit) gradually couples with extraction mechanisms as the
 *   firm faces decreasing competitive pressure. Users become locked in not
 *   just because the network is valuable, but because exit is artificially
 *   expensive (data trapping, integration fragmentation, UI learning costs).
 *   Competitors become locked in through ecosystem dependency and API
 *   control. The constraint demonstrates that coordination mechanisms can be
 *   weaponized as extraction mechanisms once competitive checks are removed.
 *   Regulatory intervention (interoperability mandates, data portability
 *   rights) attempts to sever the coupling by reducing switching costs,
 *   creating a Scaffold structure with a sunset clause: if enforcement
 *   succeeds, lock-in becomes uneconomical and the market exits the
 *   Snare/Tangled Rope states. The analytical observer risks naturalizing
 *   this as an immutable property of network economics (Mountain) when it is
 *   partly contingent on institutional architecture.
 *
 * KEY AGENTS:
 *   - Dominant Platform Firm: Primary beneficiary (institutional/arbitrage) — captures network value and switching cost rents; experiences constraint as enabling coordination
 *   - Locked-In User Base: Primary victim (powerless/trapped) — faces extreme switching costs (data migration, social graph loss, interface learning); absorbs extraction costs with zero exit agency
 *   - Dependent Ecosystem: Secondary victim (moderate/constrained) — app developers, service providers dependent on platform APIs; face API deprecation, margin compression, policy changes; cannot exit without financial loss
 *   - Competing Firms: Organized victim (organized/constrained) — competitors unable to build equivalent network scale; face structural disadvantage despite competitive product quality; barrier to entry through network effects and data access asymmetry
 *   - Antitrust/Regulatory Coalition: Organized intermediary (organized/mobile) — perceive lock-in as temporary governance failure solvable through interoperability mandates and data portability; have regulatory power to mandate structural remedies
 *   - Legacy Competitors: Institutional actors (powerful/mobile) — firms that once competed (Yahoo, Nokia, etc.) that lost to network effects; constraints persist through vestigial positioning and inertia
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as laws of network economics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_dominance_lock_in, 0.52).
domain_priors:suppression_score(market_dominance_lock_in, 0.58).
domain_priors:theater_ratio(market_dominance_lock_in, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_dominance_lock_in, extractiveness, 0.52).
narrative_ontology:constraint_metric(market_dominance_lock_in, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(market_dominance_lock_in, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_dominance_lock_in, tangled_rope).
narrative_ontology:human_readable(market_dominance_lock_in, "Market Dominance Lock-In: Network Effects and Switching Costs").
narrative_ontology:topic_domain(market_dominance_lock_in, "economic/competition").

domain_priors:requires_active_enforcement(market_dominance_lock_in).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_dominance_lock_in, dominant_platform_firm).
narrative_ontology:constraint_victim(market_dominance_lock_in, competing_firms).
narrative_ontology:constraint_victim(market_dominance_lock_in, consumer_lock_in_cohort).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCKED-IN USER BASE (SNARE) — Users face extreme switching costs: migrating data, relearning interfaces, losing network value (social graph, data, integration partners). Cannot exit without material sacrifice. Zero exit agency. The platform extracts through degrading service quality, raising prices, reducing privacy, or weakening product innovation — users absorb all costs. No coordination benefit perceived from the user's position.
constraint_indexing:constraint_classification(market_dominance_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEPENDENT BUSINESS ECOSYSTEM (SNARE) — App developers, service providers, and complementary businesses have built revenue models on platform APIs and user access. High switching costs (recoding entire applications, losing user base, learning new platform rules). Career risk and financial loss prevent exit despite extractive platform behavior (API deprecation, margin compression, arbitrary policy changes). Moderate power through organizing (group action, regulatory appeal) but constrained exit.
constraint_indexing:constraint_classification(market_dominance_lock_in, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: DOMINANT PLATFORM FIRM (ROPE) — Experiences the constraint as pure coordination: network effects are the coordination mechanism that keeps users on the platform, complementary developers building value, and economic activity flowing through the ecosystem. Benefits from growth without bearing switching costs. The firm sees the constraint as enabling, not extractive. Arbitrage available (can exit by selling or merging; low personal downside from degradation).
constraint_indexing:constraint_classification(market_dominance_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMPETING FIRM COALITION (TANGLED ROPE) — Organized competitors (mobile OS providers, search engines, social platforms) experience genuine coordination value from standardized protocols, open APIs, and interoperability requirements; simultaneously bear extraction costs through regulatory compliance, competitive disadvantage from network effects, and inability to access platform-specific data. Active enforcement (antitrust regulation, interoperability mandates) creates a hybrid structure: coordination function (ecosystem stability) + asymmetric extraction (dominant firm retains core advantage).
constraint_indexing:constraint_classification(market_dominance_lock_in, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANTITRUST/REGULATORY COALITION (SCAFFOLD) — Regulators and policymakers see market dominance lock-in as a temporary governance failure solvable through interoperability mandates, data portability rights, and API standards. Active enforcement mechanisms (DMA in EU, proposed legislation in US) create sunset conditions: if enforced, interoperability reduces switching costs and unlocks trapped users. Low effective extraction because regulatory agents have power and see an exit path (structural remedies). Theater moderately low — actual enforcement data vs compliance theater.
constraint_indexing:constraint_classification(market_dominance_lock_in, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY COMPETITOR — Older firms (ex: Yahoo, Nokia, MySpace legacy business units) that once competed effectively but lost to network effects. Competitive constraints persist through vestigial brand loyalty, remaining user base, or niche positioning, but the dominant firm has structurally won. These constraints persist through inertia rather than function. Theater high — maintenance of competitive posturing despite minimal competitive function.
constraint_indexing:constraint_classification(market_dominance_lock_in, piton,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, network effects are a fundamental property of communication systems: the value of a network grows with user count (metcalfe-like scaling). Lock-in is inherent to any system with increasing returns and information transfer costs. This perspective sees market dominance as an immutable consequence of network economics. However, the structural data contradicts the mountain classification — regulatory capacity to mandate interoperability and API access reveals that lock-in is partly contingent on institutional architecture, not purely a law of network economics.
constraint_indexing:constraint_classification(market_dominance_lock_in, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_dominance_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(market_dominance_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(market_dominance_lock_in, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_dominance_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(market_dominance_lock_in, TR),
    TR >= 0.70.

:- end_tests(market_dominance_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, reflecting the coupling of coordination benefits with extraction mechanisms. Initial extractiveness (0.25) reflects genuine network value — users benefit from being on the network with many others, developers benefit from access to users, and the firm benefits from coordination of these groups. Rising extractiveness (0.38 → 0.52) reflects that as dominance solidifies, the firm increasingly extracts through switching cost elevation, data trapping, ecosystem control, and service degradation. The platform captures the coordination benefit while shifting costs to users and competitors. At 0.52, the constraint remains tangled (both coordination and extraction present) but extraction is becoming dominant. If extractiveness exceeds 0.66, classification would shift decisively to Snare. Suppression (0.58): Moderate-high, reflecting that while users and competitors cannot physically exit (the network is necessary for economic activity), they face very high costs to do so. Suppression is not total because: (1) regulatory intervention (interoperability mandates) is reducing technical suppression, (2) decentralized alternatives exist but are not yet competitive, (3) some users can exit to niche platforms. Theater ratio (0.48): Moderate, reflecting that platform behavior includes both genuine product development and performative compliance (privacy policies that obscure data monetization, competitive posturing despite market dominance, ecosystem theater vs. actual benefit).
 *
 * PERSPECTIVAL GAP:
 *   The gap between the dominant firm's Rope classification and the locked-in user's Snare classification is the diagnostic signal that extraction is occurring. If all perspectives converged on a single type, the constraint would be either pure coordination or pure extraction. The perspectival divergence reveals a hybrid: the network provides genuine value (coordination) but the firm's behavior exploits the lock-in to extract rents (extraction). The regulatory perspective (Scaffold) is crucial because it asserts that the coupling is contingent on institutional architecture — if interoperability is mandated, switching costs fall, and users regain exit agency. This assertion can be tested empirically: post-enforcement, if users do migrate and extraction ceases, the Scaffold hypothesis is confirmed and the constraint transitions to Rope/Mountain (either stable coordination or genuine natural law). If enforcement fails or users don't migrate despite mandated interoperability, the Snare hypothesis is confirmed and lock-in is deeper than regulation can reach.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the base_properties beneficiary/victim declarations and the power/exit tuple for each perspective. Dominant firm (beneficiary + institutional/arbitrage) derives d ≈ 0.12 via the canonical table. Locked-in users (victims + powerless/trapped) derive d ≈ 0.95. Competing firms (victims + organized/constrained) derive d ≈ 0.62 (organized power reduces d from the would-be 0.80 for unorganized victims). Regulatory coalition (intermediary + organized/mobile) derives d ≈ 0.40 (constrained by political power but mobile exit through policy change). No directionality overrides are needed — the derivation chain produces appropriate d values without adjustment. The suppression scaling (suppression is not scaled by f(d); only extractiveness is) means that the high suppression (0.58) applies to all agents equally — the technical barriers to exit (data migration complexity, API fragmentation, interface learning) are objective constraints, not perspective-dependent.
 *
 * MANDATROPHY ANALYSIS:
 *   EXEMPLAR FOR CONSTRAINT COUPLING: This constraint resolves the mandatrophy by demonstrating that the six classifications are not mutually exclusive perspectives on a single constraint but rather accurate descriptions of different structural phases and positions. Early dominance (t=0) is Rope: genuine coordination of users and developers, network effects are real, the firm's behavior is competitive. Middle dominance (t=5) is Tangled Rope: coordination function persists (the network is still valuable) but extraction mechanisms emerge (switching costs rise, API restrictions tighten). Late dominance (t=10) moves toward Snare for locked-in agents: the coordination benefit is taken for granted, extraction mechanisms dominate user experience, and competitive pressure has vanished. The regulatory perspective introduces Scaffold: institutions capable of mandating interoperability can create an exit path. The legacy perspective shows Piton: competitive pressure from the dominant firm has so degraded that competition itself becomes vestigial theater. The analytical observer risks Mountain (naturalizing the outcome). The mandatrophy is resolved not by choosing one classification but by recognizing that (1) all classifications are structurally accurate from their respective positions, (2) the constraint transitions between types as dominance solidifies and suppression increases, and (3) institutional intervention can create structural discontinuities (interoperability reduces switching costs) that move the constraint from Snare toward Rope/Scaffold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    network_effect_magnitude,
    'What fraction of the platform''s value derives from network effects vs. intrinsic product quality and switching cost engineering?',
    'Comparative analysis across platforms with similar feature sets but different network sizes; historical data on user migration when switching cost barriers were reduced (e.g., data export, API access); user surveys on value attribution',
    'If network effects dominate (>70%): lock-in is largely structural (users genuinely value the network). If intrinsic quality/switching costs dominate (>50%): lock-in is more extractive than appears. Classification shifts from Rope (network coordination) toward Snare (engineered switching costs).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_magnitude, empirical, 'Relative contribution of network effects vs. intrinsic product quality').

omega_variable(
    switching_cost_irreversibility,
    'To what extent are switching costs reversible (can users migrate with data/social graph intact) vs. irreversible (data trapped, relationships severed)?',
    'Technical audit of data portability APIs, social graph export capabilities, integration standards; empirical tracking of user migration patterns after interoperability mandates (e.g., EU DMA); user interviews on perceived data loss on exit',
    'If highly reversible: users have more exit agency; classification shifts toward Rope/Tangled Rope. If largely irreversible: users are trapped; classification confirms Snare. This distinction determines whether interoperability remedies actually reduce lock-in or merely reduce friction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(switching_cost_irreversibility, empirical, 'Reversibility of switching costs and data lock-in').

omega_variable(
    regulatory_enforcement_capacity,
    'Can regulatory bodies actually enforce interoperability mandates and data portability rights, or does platform complexity and technical obfuscation prevent effective enforcement?',
    'Post-enforcement audit of DMA compliance in EU; comparative analysis of achieved vs mandated interoperability; third-party technical verification of API access and data export functionality; measurement of actual user switching post-enforcement',
    'If enforcement effective: scaffold classification confirmed; sunset is real. If enforcement blocked by complexity: scaffold is aspirational; the constraint persists as Snare/Tangled Rope despite regulatory intent. This determines whether institutional remedies can actually reduce lock-in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_enforcement_capacity, empirical, 'Feasibility of regulatory enforcement of interoperability').

omega_variable(
    alternative_coordination_models,
    'Are decentralized/federated coordination models (e.g., ActivityPub, blockchain platforms) technically capable of achieving equivalent network effects to centralized platforms?',
    'Technical benchmark of decentralized network performance (latency, throughput, feature parity); user adoption rates and retention in federated platforms; comparative analysis of equilibrium network size and economic value',
    'If decentralized models viable: alternative coordination pathway exists; scaffold/rope perspectives gain credibility. If decentralized models fail: lock-in to centralized platforms is partly structural (coordination requires centralization); mountain perspective gains credibility. This determines whether market dominance is contingent or inevitable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_coordination_models, empirical, 'Technical viability of decentralized network alternatives').

omega_variable(
    extractive_behavior_measurement,
    'How much of the dominant platform''s revenue growth derives from extraction mechanisms (data monetization, margin compression, service degradation) vs. value creation (features, ecosystem investment)?',
    'Financial decomposition of revenue sources; user experience metrics over time (feature quality, downtime, privacy loss); comparative analysis with competitive-era behavior; ecosystem impact assessment (developer revenue compression, complementary service quality)',
    'If extraction-driven (>50% of growth): lock-in enables extraction; Snare classification confirmed. If value-creation-driven (>50% of growth): lock-in is coupled with genuine ecosystem benefits; Tangled Rope or Rope classification confirmed. Determines whether regulators should target lock-in (Snare/Tangled Rope) or allow continued dominance (Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractive_behavior_measurement, empirical, 'Proportion of growth driven by extraction vs. value creation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_dominance_lock_in, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mdli_tr_t0, market_dominance_lock_in, theater_ratio, 0, 0.35).
narrative_ontology:measurement(mdli_tr_t5, market_dominance_lock_in, theater_ratio, 5, 0.4).
narrative_ontology:measurement(mdli_tr_t10, market_dominance_lock_in, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(mdli_be_t0, market_dominance_lock_in, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(mdli_be_t5, market_dominance_lock_in, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(mdli_be_t10, market_dominance_lock_in, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(mdli_su_t0, market_dominance_lock_in, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(mdli_su_t5, market_dominance_lock_in, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(mdli_su_t10, market_dominance_lock_in, suppression_requirement, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_dominance_lock_in, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_dominance_lock_in, 0.18).
narrative_ontology:affects_constraint(market_dominance_lock_in, switching_cost_elevation_mechanism).
narrative_ontology:affects_constraint(market_dominance_lock_in, ecosystem_dependency_lock_in).
narrative_ontology:affects_constraint(market_dominance_lock_in, data_portability_governance).

% DUAL FORMULATION NOTE:
% Market dominance lock-in is a constraint family with multiple decomposable components: (1) network_effects_coordination (ε=0.05, Rope) — the genuine coordination mechanism that creates value, (2) switching_cost_elevation (ε=0.65, Snare) — engineered barriers that trap users, (3) ecosystem_dependency (ε=0.58, Tangled Rope) — mixed coordination and extraction for developers, (4) regulatory_enforcement (ε=0.35, Scaffold) — temporary governance intervention. Each decomposition has different base_extractiveness, different beneficiary/victim structures, and different temporal trajectories. Market dominance lock-in as described here is the hybrid constraint that couples (1) with (2)/(3). Upstream network effects provide the coordination basis; downstream switching costs provide the extraction mechanism. The constraint family links are bidirectional: network effects enable switching costs (you can only trap users on a valuable network), and switching costs enable network dominance (lock-in prevents competitors from building equivalent networks).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
