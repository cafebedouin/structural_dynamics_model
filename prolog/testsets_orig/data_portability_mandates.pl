% ============================================================================
% CONSTRAINT STORY: data_portability_mandates
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_data_portability_mandates, []).

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
 *   constraint_id: data_portability_mandates
 *   human_readable: Data Portability Mandates (GDPR, DMA)
 *   domain: digital_regulation/data_governance
 *
 * SUMMARY:
 *   Data portability mandates (GDPR Article 20, Digital Markets Act)
 *   represent a regulatory attempt to solve platform lock-in through forced
 *   data interoperability. The constraint exhibits tension between its stated
 *   coordination function (enabling users to switch platforms) and its actual
 *   extraction mechanism (incumbent platforms control data format, timing,
 *   and usability of transfers). Theater has increased as platforms have
 *   learned to comply technically while maintaining functional lock-in
 *   through data complexity and transfer friction. The constraint is a
 *   diagnostic exemplar of how coordination mandates can degrade into
 *   extraction theater when beneficiaries (users, regulators) lack technical
 *   capacity to verify compliance, while victims (incumbents) have incentive
 *   and resources to weaponize rules.
 *
 * KEY AGENTS:
 *   - Individual Data Subjects: Primary intended beneficiary (powerless/trapped) — nominally gain exit option but remain locked-in due to network effects and high switching costs
 *   - Emerging Competitors: Secondary beneficiary (moderate/constrained) — data portability creates genuine interoperability opportunity but also extraction risk through rule weaponization
 *   - EU/UK Regulators: Regulatory authority (institutional/arbitrage) — frame mandate as coordination mechanism solving market concentration; maintain enforcement authority
 *   - Incumbent Platforms: Primary victim-turned-extractor (powerful/mobile) — face compliance costs but learn to weaponize technical complexity; shift extraction mechanism from surveillance to portability friction
 *   - Data Interoperability Infrastructure: Systemic victim (powerless/trapped) — bears cost of maintaining standards and infrastructure; receives no revenue or formal governance
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing network effects as inherent law rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(data_portability_mandates, 0.52).
domain_priors:suppression_score(data_portability_mandates, 0.48).
domain_priors:theater_ratio(data_portability_mandates, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(data_portability_mandates, extractiveness, 0.52).
narrative_ontology:constraint_metric(data_portability_mandates, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(data_portability_mandates, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(data_portability_mandates, tangled_rope).
narrative_ontology:human_readable(data_portability_mandates, "Data Portability Mandates (GDPR, DMA)").
narrative_ontology:topic_domain(data_portability_mandates, "digital_regulation/data_governance").

domain_priors:requires_active_enforcement(data_portability_mandates).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(data_portability_mandates, individual_data_subjects).
narrative_ontology:constraint_beneficiary(data_portability_mandates, emerging_platform_competitors).
narrative_ontology:constraint_victim(data_portability_mandates, incumbent_platform_operators).
narrative_ontology:constraint_victim(data_portability_mandates, data_interoperability_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL DATA SUBJECT (SNARE) — Users cannot practically exit incumbent platforms despite portability mandates because network effects and switching costs trap them. Portability creates an illusion of choice; exit remains costly. Data subjects bear the extraction (engagement tracking, behavioral modeling, targeted manipulation) with no real exit path.
constraint_indexing:constraint_classification(data_portability_mandates, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EMERGING COMPETITOR (TANGLED ROPE) — New platforms benefit from data portability rules that create interoperability; this is genuine coordination function enabling competition. Yet incumbents can weaponize portability mandates through selective compliance, technical obstruction, and data format fragmentation. The constraint simultaneously enables and extracts from competitors.
constraint_indexing:constraint_classification(data_portability_mandates, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGULATORY AUTHORITY (ROPE) — EU/UK regulators experience the mandate as pure coordination: solving the collective action problem of platform lock-in through data portability rules. Enforcement costs are high but viewed as necessary to maintain competitive markets. Arbitrage exit through selective enforcement available.
constraint_indexing:constraint_classification(data_portability_mandates, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INCUMBENT PLATFORM (PITON) — Large platforms comply with portability mandates theatrically: formatting data in technical forms that are difficult to process, delaying transfers, creating incompatible data schemas, or offering portability features that are procedurally burdensome. Compliance performance is maintained while functional utility remains minimal. Effective extraction persists beneath the ritual of compliance.
constraint_indexing:constraint_classification(data_portability_mandates, piton,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INTEROPERABILITY COALITION (SCAFFOLD) — Data Commons initiatives, open standards bodies (W3C), and NGOs view portability mandates as temporary scaffolding toward true data interoperability. The coalition sees a sunset: portable data formats will eventually be replaced by real-time APIs and federated architectures. Sunset clause is implicit in the technology roadmap (5-10 year horizon).
constraint_indexing:constraint_classification(data_portability_mandates, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN VIEW) — From a systemic perspective, network effects and switching costs constitute an immutable structural constraint: any communication platform must exhibit some lock-in because value increases with user base size. Portability mandates cannot escape this fundamental law. However, this perspective risks naturalizing what is a contingent institutional arrangement (lock-in exploited through strategic design choices rather than inherent to network effects).
constraint_indexing:constraint_classification(data_portability_mandates, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(data_portability_mandates_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(data_portability_mandates, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(data_portability_mandates, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(data_portability_mandates, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(data_portability_mandates, TR),
    TR >= 0.70.

:- end_tests(data_portability_mandates_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and rising. The mandate creates genuine coordination value (enabling competitor access to user data), but incumbent platforms have learned to comply theatrically while maintaining lock-in. Extractiveness has increased from 0.38 to 0.52 over six years as platforms optimized compliance procedures. The rising curve reflects Goodhart-style metric substitution: platforms now allocate resources to making portability technically compliant while functionally useless. Suppression (0.48): Moderate. Barriers to exit include network effects (switching to smaller platforms reduces utility), social graph switching costs, learned UI dependencies, and coordination problems among users. But suppression is not maximal — some users do switch, and regulatory enforcement creates pressure. Theater ratio (0.58): Moderate-high and rising. Portability compliance is increasingly performative. Platforms file data in formats optimized for technical correctness (meeting the API specification) rather than user utility. Transfer delays are procedurally justified. Data structures are intentionally complex. Real-time APIs are unavailable. Users receive ported data that satisfies the mandate but cannot recreate their social networks, interaction histories, or algorithmic contexts on alternative platforms.
 *
 * PERSPECTIVAL GAP:
 *   The regulatory authority sees coordination (rope) — the mandate solves a collective action problem (platform concentration). Users see extraction (snare) — they remain locked in despite portability rights. Incumbents see theater (piton) — they comply performatively while maintaining extraction. Competitors see mixed extraction (tangled rope) — data access enables them but procedural weaponization constrains them. The interoperability coalition sees a sunset (scaffold) — portability is temporary; real-time APIs will eventually replace data transfer. The analytical observer risks seeing immutable lock-in (mountain) — but this naturalizes contingent technical choices (data format complexity, API unavailability) as inherent to network effects. The perspectival gap reveals a systemic misalignment: the mandate was designed to solve user lock-in but created new extraction mechanisms for incumbents and new coordination problems for infrastructure maintainers.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary-victim flow: Users (intended beneficiary) cannot exit platform lock-in despite portability rights — they are partially trapped by network effects (d ≈ 0.85). Emerging competitors (intended beneficiary) gain access to user data but face extraction through rule weaponization — they are partially victimized (d ≈ 0.55). Incumbent platforms (formal victim of the mandate) weaponize compliance rules to maintain extraction — they are actually beneficiaries of technical complexity (d ≈ 0.15). The regulatory authority (coordinator) experiences the mandate as low extraction (d ≈ 0.10). Data infrastructure systems (unmentioned in mandate design) absorb costs (d ≈ 0.95). The directionality structure reveals that the mandate's intended beneficiaries remain trapped, while formal victims learn to extract through procedural compliance.
 *
 * MANDATROPHY ANALYSIS:
 *   REGULATORY CAPTURE VARIANT: Data portability mandates exemplify how coordination rules become extraction theater under regulatory asymmetry. Regulators have enforcement authority but lack technical capacity to verify meaningful compliance. Incumbents have technical capacity and incentive to weaponize rule definitions. The mandate contains genuine coordination value (competitor access) buried under increasing extraction theater (procedural friction, format complexity). The theater ratio rising from 0.42 to 0.58 shows Goodhart drift: platforms optimize for metric compliance (data transfer completion) rather than actual coordination function (meaningful user exit). Resolution requires shifting the mandate from backward-compatibility (porting historical data) to forward-compatibility (real-time API access), which would raise the coordination function but lower incumbent platform power to maintain extraction through complexity. The mandatrophy resolves by recognizing that portability mandates alone cannot overcome network effects; they require pairing with interoperability mandates (API requirements, standards governance) and user coordination mechanisms (cohort switching incentives). Without these, theater ratio approaches 1.0 and the constraint degrades to pure performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    portability_technical_feasibility,
    'Can data portability ever be technically frictionless, or does the complexity of platform-specific data structures create irreducible barriers to meaningful transfer?',
    'Empirical assessment of data transfer completion rates, post-transfer functionality success, and user satisfaction metrics across major platforms implementing GDPR/DMA portability rules',
    'If frictionless transfer is possible: portability mandates can be tangled rope with genuine coordination value. If transfers are inherently degraded: portability is theater masking continued lock-in (Piton classification for incumbent platform perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(portability_technical_feasibility, empirical, 'Whether data portability can achieve technical frictionlessness').

omega_variable(
    network_effects_irreducibility,
    'Is platform lock-in caused by inherent network effects (mathematical immutability) or by strategic design choices (contingent institutional arrangement)?',
    'Historical analysis of platforms designed for interoperability or federation; comparison of switching costs across platforms with identical network size but different architectural choices',
    'If inherent: mountain classification from all perspectives is justified. If contingent: mountain is a false summit, and portability mandates address a solvable coordination problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effects_irreducibility, conceptual, 'Whether network lock-in is mathematically inevitable or architecturally chosen').

omega_variable(
    interoperability_infrastructure_cost,
    'What is the true cost of maintaining interoperable data infrastructure, and who bears it? Do portability mandates transfer infrastructure costs from platforms to public systems?',
    'Cost accounting for data transfer infrastructure, standards maintenance, and API development across public vs private implementations; analysis of who funds interoperability infrastructure over time',
    'If public infrastructure costs exceed private, mandates may create a new extraction mechanism: shifting infrastructure burden to underfunded public systems while platforms retain data value. If costs are manageable and fairly distributed: tangled rope classification is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interoperability_infrastructure_cost, empirical, 'True cost and distribution of interoperability infrastructure').

omega_variable(
    regulatory_capture_portability_weaponization,
    'Do incumbent platforms capture regulators to define portability rules in ways that appear compliant while minimizing actual data utility (Goodhart-style metric substitution)?',
    'Content analysis of platform compliance documentation and regulatory guidance; empirical testing of whether ported data can reconstruct equivalent platform functionality; analysis of platform lobbying influence on regulatory standards development',
    'If weaponized: portability rules become theater (Piton), and the mandate''s tangled rope classification collapses toward snare for users. If not weaponized: mandate retains genuine coordination value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_portability_weaponization, empirical, 'Whether platforms capture regulators to weaponize portability rules').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(data_portability_mandates, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dpm_tr_t0, data_portability_mandates, theater_ratio, 0, 0.42).
narrative_ontology:measurement(dpm_tr_t3, data_portability_mandates, theater_ratio, 3, 0.5).
narrative_ontology:measurement(dpm_tr_t6, data_portability_mandates, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(dpm_be_t0, data_portability_mandates, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(dpm_be_t3, data_portability_mandates, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(dpm_be_t6, data_portability_mandates, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(data_portability_mandates, resource_allocation).
narrative_ontology:affects_constraint(data_portability_mandates, platform_lock_in).
narrative_ontology:affects_constraint(data_portability_mandates, data_interoperability_standards).
narrative_ontology:affects_constraint(data_portability_mandates, regulatory_capture_digital_markets).

% DUAL FORMULATION NOTE:
% Data portability mandates are downstream of both platform lock-in (structural constraint about network effects) and regulatory capture dynamics (institutional constraint about regulator-incumbent alignment). The portability mandate represents an intervention designed to address lock-in but whose effectiveness depends on whether it creates genuine interoperability (coordination) or merely procedural compliance theater (extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(data_portability_mandates, powerful, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
