% ============================================================================
% CONSTRAINT STORY: network_effect_concentration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_network_effect_concentration, []).

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
 *   constraint_id: network_effect_concentration
 *   human_readable: Network Effect Concentration in Digital Platforms
 *   domain: economic/technological
 *
 * SUMMARY:
 *   Network effect concentration in digital platforms creates a structural
 *   constraint where dominant position becomes increasingly self-reinforcing:
 *   users join where other users are, creating exponential value growth for
 *   the incumbent. This constraint exhibits the full range of DR
 *   classifications depending on observational position. The dominant
 *   platform operator experiences pure coordination (Rope) — solving a real
 *   collective action problem. End users and competing platforms experience
 *   extraction (Snare) — trapped by network lock-in with no viable
 *   alternatives. Smaller service providers experience mixed coordination and
 *   extraction (Tangled Rope) — they benefit from platform reach but are
 *   extracted from through commission rates and policy changes. The coalition
 *   of smaller providers sees a genuine exit pathway through federation
 *   protocols (Tangled Rope), but only if they can overcome coordination
 *   barriers to collective adoption. The analytical observer risks
 *   naturalizing concentration as an immutable law of network topology
 *   (Mountain), when institutional choices about interoperability, data
 *   portability, and regulatory enforcement determine whether concentration
 *   persists. The constraint's evolution shows extractiveness increasing from
 *   0.35 to 0.58 over the measurement interval, while theater_ratio slightly
 *   declined from 0.55 to 0.48 — indicating genuine (rather than
 *   performative) extraction mechanisms strengthening, as the platform
 *   operator optimized monetization and reduced friction that previously
 *   appeared as regulatory theater.
 *
 * KEY AGENTS:
 *   - Dominant Platform Operator: Primary beneficiary (institutional/arbitrage) — captures network effects, can monetize user data and attention, controls ecosystem terms, experiences coordination function
 *   - End Users: Primary victim (powerless/trapped) — locked in through relationships, data, and ecosystem embeddedness; cannot exit without abandoning social capital; bears extraction through data monetization and attention capture
 *   - Competing Platform Providers: Secondary victim (moderate/constrained) — face exponential cost to entry due to network effects; cannot achieve critical mass without existing user base; high barriers to exit market
 *   - Large Service Providers: Mixed actor (powerful/mobile) — coordinated with platform (mutual value creation) but extracted from (commission rates, unilateral policy changes); can diversify across platforms but sunk in optimization
 *   - Coalition of Smaller Providers: Organized potential agent (organized/constrained) — individually trapped but collectively capable of building federation/interoperability alternatives; faces coordination barrier to adoption
 *   - Regulatory Framework: Institutional observer (institutional/arbitrage) — maintains oversight apparatus but enforcement capacity lags platform evolution; regulatory theater persists (Piton view)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as immutable network laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(network_effect_concentration, 0.58).
domain_priors:suppression_score(network_effect_concentration, 0.65).
domain_priors:theater_ratio(network_effect_concentration, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(network_effect_concentration, extractiveness, 0.58).
narrative_ontology:constraint_metric(network_effect_concentration, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(network_effect_concentration, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(network_effect_concentration, tangled_rope).
narrative_ontology:human_readable(network_effect_concentration, "Network Effect Concentration in Digital Platforms").
narrative_ontology:topic_domain(network_effect_concentration, "economic/technological").

domain_priors:requires_active_enforcement(network_effect_concentration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(network_effect_concentration, dominant_platform_operator).
narrative_ontology:constraint_victim(network_effect_concentration, competing_platforms).
narrative_ontology:constraint_victim(network_effect_concentration, smaller_service_providers).
narrative_ontology:constraint_victim(network_effect_concentration, end_users_locked_in).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END USER (SNARE) — Cannot exit the dominant platform without abandoning social connections, professional contacts, and ecosystem access. Material barriers include lock-in through data, relationships, and switching costs. Zero effective alternatives at scale. Maximum experienced extraction — user benefits from network reach but cannot negotiate terms and bears full cost of exploitation.
constraint_indexing:constraint_classification(network_effect_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMPETING PLATFORM (SNARE) — Faces structural impediment to growth: even with superior product, cannot achieve critical mass without the installed user base already on the dominant platform. Network effects create exponential cost to entry. High suppression through technical, financial, and coordination barriers. Moderate power but constrained exit — the alternative (exit the market) carries severe career and capital loss.
constraint_indexing:constraint_classification(network_effect_concentration, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DOMINANT PLATFORM (ROPE) — Experiences the network effect as pure coordination: user growth drives value, which attracts more users. Genuine coordination function exists — the platform solves a real collective action problem (connecting many agents). Net beneficiary with high arbitrage capacity. Can exit by licensing technology, pivoting to adjacent networks, or monetizing data. Effective extraction but framed as coordination benefit.
constraint_indexing:constraint_classification(network_effect_concentration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LARGE SERVICE PROVIDER (TANGLED ROPE) — Coordinated with the dominant platform (shared value creation through ecosystem), but also extracted from (platform takes commission, controls terms, can change policies unilaterally). Moderate power and mobile exit options (can diversify across platforms or build own ecosystem), but significant sunk costs in platform-specific optimization. Mixed experience of coordination benefit and extraction cost.
constraint_indexing:constraint_classification(network_effect_concentration, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: COALITION OF SMALLER PROVIDERS (TANGLED ROPE) — Individually trapped, but collectively can create alternative coordination mechanisms (open protocols, federation, cooperative platforms). Network effects can be broken through coordinated defection and technical standardization. Genuine coordination function exists (coalition members benefit from each other), but extraction occurs through platform control of terms and rate-setting. Constrained exit at individual level, but coalition structure provides exit pathway if sustained.
constraint_indexing:constraint_classification(network_effect_concentration, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY FRAMEWORK (PITON) — Antitrust and platform regulation frameworks persist through institutional inertia despite degraded function. Original purpose was breaking monopolies through structural separation or conduct restrictions, but enforcement has atrophied relative to platform evolution. Theater_ratio is low because regulation conducts performative oversight (investigating, fining, imposing behavioral remedies) that does not arrest concentration. The regulatory apparatus maintains legitimacy through ritual rather than effectiveness.
constraint_indexing:constraint_classification(network_effect_concentration, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a mathematical/economic perspective, network effects are an immutable property of many-to-many coordination systems: value grows with scale, creating winner-take-most dynamics inherent to the structure. This perspective naturalizes concentration as a law of network topology. However, structural data reveals this as a false summit: network effects are real but concentration is contingent on institutional choices (interoperability rules, data portability, open standards). The 'inevitability' framing masks policy alternatives.
constraint_indexing:constraint_classification(network_effect_concentration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(network_effect_concentration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(network_effect_concentration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(network_effect_concentration, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(network_effect_concentration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(network_effect_concentration, TR),
    TR >= 0.70.

:- end_tests(network_effect_concentration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High but not maximum. The platform operator captures significant value through data monetization, commission extraction, and attention capture, but the extraction is not as severe as pure predation because genuine coordination function exists — the platform does solve the collective action problem of connecting many agents. The measurement trajectory (0.35→0.47→0.58) shows extractiveness increasing over time as the operator optimized monetization strategies and concentrated market control. Suppression (0.65): High. Users and competing providers face substantial barriers to exit: switching costs (data migration, relationship rebuilding), technical lock-in (API dependencies, proprietary formats), and market structure (no viable alternatives at comparable scale). Smaller providers face coordination barriers to collective exit. Theater ratio (0.48): Moderate-low and declining. Unlike regulatory constraints (high theater), network effect concentration involves genuine extraction mechanisms — network lock-in is real and functional, not performative. The slight decline in theater over time (0.55→0.48) indicates that early-stage regulatory theater has given way to more efficient extraction as the platform operator learned to optimize extraction without maintaining façades.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal across the full range of types. The dominant operator sees coordination (Rope) — they are genuinely solving a network coordination problem. End users see extraction (Snare) — they cannot exit despite experiencing harm. The coalition sees a temporary problem with federation solutions (Tangled Rope with potential Scaffold upgrade). The regulatory observer sees degraded oversight (Piton) — antitrust enforcement persists as theater while concentration accelerates. The civilizational analytical view risks seeing a natural law (Mountain) — network effects as inevitable law of topology — but the structural data reveals this as a false summit. The gap between the operator's experience (coordination, positive value creation) and the powerless user's experience (extraction, lock-in) is not a difference in interpretation but a difference in structural position: the operator genuinely benefits from the coordination function, while the user is trapped by the lock-in mechanism. Both are correct about what they experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural position in the extraction flow. The dominant platform operator is a clear beneficiary with arbitrage capacity (d≈0.05-0.15, low f(d), negative χ). End users trapped by lock-in are victims with no exit (d≈0.92-0.98, high f(d), high χ). Competing platforms are victims facing suppression through market structure (d≈0.80-0.88, high f(d), moderate-high χ). Large service providers are mixed — benefiting from platform reach but extracted from through commissions (d≈0.52-0.58, f(d)≈0.65-0.75, moderate χ). The coalition of smaller providers have constrained but not trapped exit (d≈0.65-0.75, f(d)≈0.95-1.05, moderate-high χ). The regulatory framework sees itself as agent (institutional/arbitrage) with low d due to beneficiary position in legitimacy, but the false-mountain analysis reveals this as an observational trap.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY DECOMPOSITION: Network effect concentration should be modeled as two structurally distinct constraints with different ε values and different resolution mechanisms: (1) NETWORK COORDINATION (ε≈0.15-0.20, Rope) — the genuine coordination benefit of many-to-many connection platforms; (2) MARKET CONCENTRATION LOCK-IN (ε≈0.58-0.72, Tangled Rope/Snare) — the extraction mechanism that prevents competing platforms from achieving scale despite superior features. These are not the same constraint viewed from different angles — they have different causal mechanisms, different beneficiaries, and different policy responses. Network coordination is solved (Rope); market concentration requires antitrust, interoperability mandates, or federation protocols to break. The present story models the dominant perspective (market concentration) as the primary constraint. The network coordination perspective should be modeled separately if detailed analysis of pure coordination function is needed. The mandatrophy is resolved by recognizing that the platform operator's experience (pure coordination, Rope) is asymmetric to the trapped user's experience (extraction, Snare) not due to measurement error but due to structural position. Both are correct. The constraint is Tangled Rope overall because both coordination and extraction occur in the same institutional structure, and the coalition perspective shows the path to Scaffold (sunset through interoperability standards).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    network_effect_interoperability_boundary,
    'Do network effects prevent interoperability, or does institutional choice (interoperability requirements) break the concentration lock even when network effects persist?',
    'Comparative case analysis: SMS/email interoperability despite network effects; EU interoperability mandates and resulting market outcomes; historical telephone network unbundling; open social network protocol experiments (ActivityPub adoption rates)',
    'If interoperability can break concentration: network effect is a genuine coordination problem but not an extraction mechanism — classification shifts from Snare/Tangled Rope to Rope/Scaffold. If interoperability fails or is insufficient: concentration is structural and network effect is the extraction mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(network_effect_interoperability_boundary, empirical, 'Whether interoperability can break network effect concentration').

omega_variable(
    critical_mass_threshold_measurement,
    'What is the empirical critical mass threshold below which a competing platform cannot achieve sufficient user density to offer competitive value?',
    'Historical analysis of failed competing platforms; econometric estimation of critical mass via diffusion models; user retention studies at different network densities; comparative analysis across platform categories (social, messaging, payments, e-commerce)',
    'If threshold is low relative to global user base: multiple platforms can coexist (Rope from more perspectives). If threshold is high: market concentration is structural (Snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(critical_mass_threshold_measurement, empirical, 'Empirical critical mass threshold for competing platforms').

omega_variable(
    data_portability_effectiveness,
    'Can user data portability meaningfully reduce switching costs and break the lock-in mechanism, or is relationship/identity switching (losing social connections) the irreducible lock even with portable data?',
    'Field experiments with data portability features; user switching rates pre/post data portability implementation; analysis of switching barriers beyond data (relational, reputational, identity); adoption of open data standards',
    'If effective: lock-in is technical and can be solved (exit_options upgrade to mobile/arbitrage). If ineffective: lock-in is social/relational and persists even with data portability (exit_options remain trapped/constrained).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_portability_effectiveness, empirical, 'Whether data portability reduces switching costs and lock-in').

omega_variable(
    regulatory_enforcement_sufficiency,
    'Can existing antitrust enforcement (behavioral remedies, fines, structural separation threats) prevent further concentration, or is the rate of concentration faster than regulatory capacity?',
    'Time-series analysis of market concentration vs regulatory action; case-by-case outcome analysis (did remedies achieve stated goals?); international regulatory effectiveness comparison; estimation of enforcement lag vs consolidation speed',
    'If enforcement is sufficient: Piton classification is incorrect (theater has functional role). If enforcement is insufficient: Piton is correct (regulatory theater persists while concentration continues).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_enforcement_sufficiency, empirical, 'Whether regulatory enforcement can prevent platform concentration').

omega_variable(
    federation_protocol_adoption_barrier,
    'What prevents coordinated adoption of federation protocols (ActivityPub, Matrix, etc.) by alternative platforms and users, despite their technical feasibility?',
    'Adoption rate analysis; network density in federated systems vs centralized platforms; user switching cost studies; platform incentive analysis (why do platforms resist interoperability despite user preference?); protocol standardization timelines',
    'If adoption barrier is technical: coalition can overcome it (exit_options upgrade). If adoption barrier is coordination/incentive: the barrier itself is a constraint that should be modeled separately.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federation_protocol_adoption_barrier, empirical, 'Barriers to federation protocol adoption by platforms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(network_effect_concentration, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(neteff_tr_t0, network_effect_concentration, theater_ratio, 0, 0.55).
narrative_ontology:measurement(neteff_tr_t5, network_effect_concentration, theater_ratio, 5, 0.51).
narrative_ontology:measurement(neteff_tr_t10, network_effect_concentration, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(neteff_be_t0, network_effect_concentration, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(neteff_be_t5, network_effect_concentration, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(neteff_be_t10, network_effect_concentration, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(network_effect_concentration, resource_allocation).
narrative_ontology:boltzmann_floor_override(network_effect_concentration, 0.18).
narrative_ontology:affects_constraint(network_effect_concentration, digital_advertising_extraction).
narrative_ontology:affects_constraint(network_effect_concentration, data_portability_rights).
narrative_ontology:affects_constraint(network_effect_concentration, interoperability_mandate_enforcement).

% DUAL FORMULATION NOTE:
% Network effect concentration is downstream of two distinct structural dynamics: (1) genuine coordination function of many-to-many networks (low extraction, Rope-type), and (2) market concentration lock-in mechanism (high extraction, Snare-type). These should be modeled as separate constraint stories with different ε values and linked via network.affects_constraints. The present story models the dominant lock-in mechanism (extractiveness 0.58). A separate story modeling the pure coordination function (extractiveness ≈0.15) would show the true decomposition of the colloquial label 'network effects' into structurally distinct claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(network_effect_concentration, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
