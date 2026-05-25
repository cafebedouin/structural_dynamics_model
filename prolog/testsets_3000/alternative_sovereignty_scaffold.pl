% ============================================================================
% CONSTRAINT STORY: alternative_sovereignty_scaffold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_alternative_sovereignty_scaffold, []).

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
 *   constraint_id: alternative_sovereignty_scaffold
 *   human_readable: The Decentralized Parallel
 *   domain: technological/social
 *
 * SUMMARY:
 *   The decentralized parallel represents a genuine scaffolding
 *   constraint—temporary infrastructure enabling populations to migrate away
 *   from failing institutional ropes that have degraded into snares.
 *   Crypto-networks, P2P legal templates, and distributed consensus
 *   mechanisms provide coordination outside traditional state structures.
 *   Unlike pure coordination (Rope), they function as Scaffold: they have an
 *   embedded sunset clause. As institutional actors either adopt
 *   decentralized mechanisms themselves, collapse, or successfully suppress
 *   alternatives, the scaffolding function decays. The constraint
 *   demonstrates that Scaffold is not merely a temporary support system with
 *   an explicit sunset date, but any coordination infrastructure whose
 *   primary function is enabling transition away from failing institutions.
 *   The rising extractiveness (0.12 → 0.28) over the interval reflects
 *   increasing token holder concentration, developer gatekeeping, and
 *   platform rent-seeking layered onto the original coordination function.
 *   Theater ratio (0.45 → 0.65) reflects institutional actors performing
 *   control over decentralized systems while actual supervisory capacity
 *   diminishes, and decentralized communities performing decentralization
 *   norms while technical infrastructure remains somewhat centralized.
 *
 * KEY AGENTS:
 *   - Exit-Seeking Populations: Primary beneficiaries (powerless/mobile) — gain genuine exit options from failing institutional arrangements; capture most benefit
 *   - Protocol Developer Community: Secondary beneficiaries (moderate/constrained) — gain reputation capital and network effects but constrained by technical debt and ecosystem fragmentation
 *   - Decentralized Governance Coalition: Organized participants (organized/constrained) — solve coordination problems around protocol upgrades and dispute resolution
 *   - Institutional Sovereignty Actors: Primary victims (institutional/arbitrage) — experience decentralized parallels as undermining actual sovereign function; maintain theatrical control
 *   - Regulatory Arbitrage Victims: Secondary victims (moderate/trapped) — find exit to decentralized networks suppressed through capital controls and legal penalties
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees decentralized parallels as genuine temporary infrastructure with embedded sunset, not permanent replacement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(alternative_sovereignty_scaffold, 0.28).
domain_priors:suppression_score(alternative_sovereignty_scaffold, 0.42).
domain_priors:theater_ratio(alternative_sovereignty_scaffold, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(alternative_sovereignty_scaffold, extractiveness, 0.28).
narrative_ontology:constraint_metric(alternative_sovereignty_scaffold, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(alternative_sovereignty_scaffold, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(alternative_sovereignty_scaffold, scaffold).
narrative_ontology:human_readable(alternative_sovereignty_scaffold, "The Decentralized Parallel").
narrative_ontology:topic_domain(alternative_sovereignty_scaffold, "technological/social").

domain_priors:requires_active_enforcement(alternative_sovereignty_scaffold).
narrative_ontology:has_sunset_clause(alternative_sovereignty_scaffold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(alternative_sovereignty_scaffold, network_participants).
narrative_ontology:constraint_beneficiary(alternative_sovereignty_scaffold, decentralized_protocol_developers).
narrative_ontology:constraint_beneficiary(alternative_sovereignty_scaffold, exit_seeking_populations).
narrative_ontology:constraint_victim(alternative_sovereignty_scaffold, institutional_sovereignty_actors).
narrative_ontology:constraint_victim(alternative_sovereignty_scaffold, centralized_coordination_overhead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXIT-SEEKING POPULATION (SCAFFOLD) — Individuals trapped in failing institutional arrangements (regulatory capture, surveillance states, currency collapse) see decentralized protocols as temporary infrastructure enabling migration. Low effective extraction because exit is genuinely enabled; theater is moderate (learning curve, technical barriers exist but are surmountable). The scaffold functions because it provides a real alternative pathway with sunset implicit in eventual institutional reform or death.
constraint_indexing:constraint_classification(alternative_sovereignty_scaffold, scaffold,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: PROTOCOL DEVELOPER COMMUNITY (TANGLED ROPE) — Developers benefit from network effects and reputation capital but are constrained by technical debt, security requirements, and ecosystem fragmentation. The constraint exhibits both coordination (network standards, interoperability agreements) and extraction (platform gatekeeping, token holder concentration). Active enforcement required through governance mechanisms.
constraint_indexing:constraint_classification(alternative_sovereignty_scaffold, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: INSTITUTIONAL SOVEREIGNTY ACTOR (PITON) — Traditional nation-states and regulatory bodies experience decentralized parallels as a threat that maintains their relevance while undermining actual sovereign function. The constraint appears as degraded institutional control maintained through theater: enforcement of AML/KYC rules, regulatory surveillance, and rhetorical claims of control over 'unregulated' networks. The performance of control persists despite diminishing functional effectiveness.
constraint_indexing:constraint_classification(alternative_sovereignty_scaffold, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DECENTRALIZED GOVERNANCE COALITION (ROPE) — Organized participants in protocol governance (DAOs, multisig councils, community forums) experience the constraint primarily as coordination. Solving collective action problems around protocol upgrades, security responses, and dispute resolution. Extraction exists but is moderate and acknowledged within the governance framework.
constraint_indexing:constraint_classification(alternative_sovereignty_scaffold, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY ARBITRAGE VICTIM (SNARE) — Individuals and businesses in jurisdictions with severe financial repression find their exit options to decentralized networks constrained by capital controls, terminal access restrictions, and legal penalties. The decentralized scaffold exists but is actively suppressed. High experienced extraction because escape attempt itself becomes criminalized.
constraint_indexing:constraint_classification(alternative_sovereignty_scaffold, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SCAFFOLD / SUNSET VIEW) — From a civilizational view, decentralized protocols represent genuine temporary infrastructure with an embedded sunset clause. As institutional sovereignty actors either (a) adopt decentralized coordination mechanisms themselves, (b) collapse and are replaced, or (c) successfully suppress alternatives, the scaffolding function decays. The constraint is not permanent — it is explicitly designed for the transition window where institutional ropes are failing but successor structures have not yet stabilized.
constraint_indexing:constraint_classification(alternative_sovereignty_scaffold, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(alternative_sovereignty_scaffold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(alternative_sovereignty_scaffold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(alternative_sovereignty_scaffold, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(alternative_sovereignty_scaffold, TR),
    TR >= 0.70.

:- end_tests(alternative_sovereignty_scaffold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Moderate-low. The constraint functions primarily as coordination enabling exit, not as extraction mechanism. The rising extractiveness reflects increasing rent-seeking (token concentration, developer compensation concentration, platform fees) layered onto the original coordination function, but the core mechanism remains: exit-enabling coordination. Base extraction remains well below the tangled_rope threshold (0.40+) because the primary function is not extractive. Suppression (0.42): Moderate. Capital controls, endpoint surveillance, and legal penalties suppress access for populations in high-control jurisdictions. But suppression is not total or universal—open jurisdictions enable relatively free access. Theater ratio (0.65): Moderate-high. Both institutional actors (performing control they don't possess) and decentralized communities (performing decentralization while infrastructure remains somewhat centralized) contribute theater. Regulatory rhetoric about 'regulating crypto' maintains performative control despite diminishing functional enforcement capacity.
 *
 * PERSPECTIVAL GAP:
 *   The exit-seeking population sees genuine Scaffold (low extraction, real exit option, implicit sunset as institutions change). Institutional sovereignty actors see Piton (degraded control maintained through theater). Protocol developers see Tangled Rope (mixed coordination and rent-seeking). Regulatory arbitrage victims see Snare (suppressed exit becomes extractive). The decentralized governance coalition sees Rope (coordination problem-solving without extraction). The analytical observer sees Scaffold with clarified sunset conditions. This perspectival range is not incoherence but legitimate observation of a constraint that serves different functions for different agents—coordination for developers, exit for vulnerable populations, threat for institutions, opportunity for arbitrage seekers.
 *
 * DIRECTIONALITY LOGIC:
 *   Exit-seeking populations derive low d (high beneficiary status) from mobile exit options and clear structural benefit—they are the intended users. Institutional sovereignty actors derive high d (victim status) from their experience of capacity loss and the constraint's direct threat to their functional monopoly. Protocol developers occupy middle-ground d through mixed benefits (network effects) and constraints (technical debt). The constraint's scaffolding character emerges from the low suppression of exit (for populations not in suppressive jurisdictions) combined with clear sunset: as institutions adopt, fail, or suppress alternatives, the coordination function decays. This is not coordination that persists indefinitely (Rope) nor extraction that sustains itself through coercion (Snare), but temporary coordination with an embedded institutional trajectory.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy between 'decentralized = pure coordination' and 'decentralized = new extraction' by revealing that decentralized parallels function as Scaffold precisely because they are temporary coordination infrastructure. They are NOT pure coordination (Rope) because the sunset clause is embedded in institutional dynamics—they exist specifically to enable exit from failing institutions, and as those institutions reform or collapse, the scaffolding loses its function. They are NOT pure extraction (Snare) because the primary mechanism is enabling genuine exit (low suppression for accessible populations) and coordination (protocol governance), not coercive capture. The rising extractiveness reflects rent-seeking overlay, but the core function remains scaffolding. The constraint avoids false categorization by treating Scaffold as legitimately distinct from both Rope (permanent coordination) and Snare (coercive extraction)—it is temporary coordination with an institutional sunset clause.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_adoption_or_replacement,
    'Will institutional actors co-opt decentralized protocol infrastructure, or will alternative governance structures replace institutional sovereignty entirely?',
    'Historical tracking of institutional adoption patterns (CBDCs adopting blockchain backends, nation-states implementing distributed consensus); comparative analysis of successor governance in jurisdictions where institutional sovereignty has collapsed',
    'If co-opted: scaffold collapses into institutional tool (Rope classification). If replacement occurs: sunset is real and constraint becomes historical. If suppressed indefinitely: classification degrades toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_adoption_or_replacement, conceptual, 'Whether institutions will adopt or be replaced by decentralized alternatives').

omega_variable(
    technical_scaling_ceiling,
    'Do decentralized protocol constraints (throughput, latency, cost) create a permanent technical ceiling that prevents full institutional replacement?',
    'Empirical testing of scaling solutions (rollups, sidechains, sharding); comparison of transaction costs and finality times as networks reach 1B+ users; feasibility analysis of sub-millisecond consensus in geographically distributed systems',
    'If ceiling is real: decentralized parallels remain niche scaffolds for exit populations, not institutional replacements. If ceiling is breached: scaffold functions expand to full institutional replacement potential.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technical_scaling_ceiling, empirical, 'Whether technical constraints prevent decentralized protocols from achieving institutional scale').

omega_variable(
    surveillance_and_suppression_asymmetry,
    'Can decentralized networks maintain usability for exit populations while remaining censorship-resistant against institutional suppression?',
    'Longitudinal analysis of suppression effectiveness (endpoint surveillance, ISP blocking, legal penalties); comparison of network accessibility in high-control jurisdictions vs open environments; evaluation of privacy-preserving technologies (Tor integration, privacy coins, hardware wallets) against surveillance capabilities',
    'If surveillance wins: scaffold collapses into Snare for populations in suppressive jurisdictions. If decentralized networks maintain resistance: scaffold sunset extends indefinitely, creating permanent parallel structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(surveillance_and_suppression_asymmetry, empirical, 'Whether decentralized networks can resist institutional suppression while remaining accessible').

omega_variable(
    coordination_overhead_floor,
    'Is there a minimum coordination overhead below which decentralized governance becomes unenforceable, creating a floor on extractiveness that prevents pure coordination?',
    'Comparative analysis of protocol governance costs (developer time, security auditing, dispute resolution) across different governance models; tracking of real-world protocol failures traceable to insufficient coordination; measurement of extraction (token holder concentration, developer compensation asymmetry) over protocol lifecycle',
    'If floor exists: scaffold transitions to Tangled Rope (extraction component becomes permanent). If floor is absent: pure coordination (Rope) is achievable and scaffold has genuine low-extraction character.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_overhead_floor, empirical, 'Whether decentralized governance has a minimum coordination overhead that forces extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(alternative_sovereignty_scaffold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(altsovsca_tr_t0, alternative_sovereignty_scaffold, theater_ratio, 0, 0.45).
narrative_ontology:measurement(altsovsca_tr_t5, alternative_sovereignty_scaffold, theater_ratio, 5, 0.55).
narrative_ontology:measurement(altsovsca_tr_t10, alternative_sovereignty_scaffold, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(altsovsca_be_t0, alternative_sovereignty_scaffold, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(altsovsca_be_t5, alternative_sovereignty_scaffold, base_extractiveness, 5, 0.2).
narrative_ontology:measurement(altsovsca_be_t10, alternative_sovereignty_scaffold, base_extractiveness, 10, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(alternative_sovereignty_scaffold, global_infrastructure).
narrative_ontology:affects_constraint(alternative_sovereignty_scaffold, institutional_capture_snare).
narrative_ontology:affects_constraint(alternative_sovereignty_scaffold, currency_debasement_extraction).
narrative_ontology:affects_constraint(alternative_sovereignty_scaffold, regulatory_arbitrage_tangled_rope).

% DUAL FORMULATION NOTE:
% The decentralized parallel is downstream of specific failing institutional constraints (currency debasement, regulatory capture, capital controls) but represents a distinct structural constraint. The upstream constraints have their own extractiveness values reflecting institutional dysfunction; the decentralized parallel has its own extractiveness reflecting coordination overhead and emerging rent-seeking layering. Decomposition follows the ε-invariance principle: measuring 'decentralization' via institutional failure rates yields different ε than measuring via technical scaling or governance efficiency. This story captures the scaffolding function; sibling stories address technical sufficiency (constraint_decentralized_scaling_ceiling) and governance capture (constraint_decentralized_token_concentration) separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(alternative_sovereignty_scaffold, institutional, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
