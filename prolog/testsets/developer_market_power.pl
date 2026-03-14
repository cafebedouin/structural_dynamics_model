% ============================================================================
% CONSTRAINT STORY: developer_market_power
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_developer_market_power, []).

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
 *   constraint_id: developer_market_power
 *   human_readable: Developer Market Power and Platform Lock-In
 *   domain: technology/economics/labor
 *
 * SUMMARY:
 *   Developer market power reflects the structural asymmetry between
 *   platforms controlling the computational substrate and developers who
 *   depend on those platforms to reach users. The constraint operates at
 *   multiple levels: economic (platform rent extraction through transaction
 *   fees, data monetization, and feature gating), technical (SDK lock-in,
 *   platform-specific languages, proprietary tooling), and behavioral
 *   (algorithmic control over developer visibility, preference ranking, and
 *   feature access). The constraint exhibits genuine coordination functions —
 *   standardized APIs, distribution infrastructure, community resources —
 *   that create real value. Simultaneously, these coordination mechanisms
 *   serve as lock-in levers: the same infrastructure that enables development
 *   also controls developer access, visibility, and revenue. The
 *   extractiveness has increased monotonically over the measurement interval
 *   (0.35→0.58), reflecting increasing platform power and developer
 *   dependency. Theater ratio growth (0.22→0.48) indicates platform
 *   legitimation narratives (developer-friendly SDKs, 'we're building
 *   community') increasingly obscuring extraction mechanisms.
 *
 * KEY AGENTS:
 *   - Emerging Developers: Primary victims (powerless/trapped) — new entrants with minimal platform exit options, bearing full dependency on platform governance, feature access, and algorithmic visibility
 *   - Mid-Career Developers: Secondary victims (moderate/constrained) — portable skills but constrained by ecosystem switching costs and network effects; benefits from coordination but bears asymmetric extraction
 *   - Enterprise Developer Organizations: Secondary beneficiaries (organized/mobile) — multi-platform presence and negotiating power, experience primarily coordination benefits
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — capture rent through transaction fees, data harvesting, feature control, and ecosystem gatekeeping
 *   - Regulatory Frameworks: Institutional observer (powerful/mobile) — labor law and competition policy increasingly theatrical in addressing platform control; enforcement fragmented and slow
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing platform dominance as inevitable network effect outcome rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(developer_market_power, 0.58).
domain_priors:suppression_score(developer_market_power, 0.65).
domain_priors:theater_ratio(developer_market_power, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(developer_market_power, extractiveness, 0.58).
narrative_ontology:constraint_metric(developer_market_power, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(developer_market_power, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(developer_market_power, tangled_rope).
narrative_ontology:human_readable(developer_market_power, "Developer Market Power and Platform Lock-In").
narrative_ontology:topic_domain(developer_market_power, "technology/economics/labor").

domain_priors:requires_active_enforcement(developer_market_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(developer_market_power, platform_operators).
narrative_ontology:constraint_beneficiary(developer_market_power, established_developers).
narrative_ontology:constraint_victim(developer_market_power, emerging_developers).
narrative_ontology:constraint_victim(developer_market_power, developer_workforce).
narrative_ontology:constraint_victim(developer_market_power, platform_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING DEVELOPER (SNARE) — New developers face near-total platform dependency. Ecosystem lock-in through specialized tooling, SDK requirements, and platform-specific languages creates structural confinement. Career advancement requires platform adoption; alternatives exist but carry high switching costs. No genuine exit option.
constraint_indexing:constraint_classification(developer_market_power, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-CAREER DEVELOPER (TANGLED ROPE) — Moderate power through specialized skills and portability, but constrained by ecosystem switching costs and network effects. Benefits from platform's coordination function (standardized APIs, distribution channels, community). Extraction flows asymmetrically: platform captures developer data, attention, and productivity gains while controlling feature access and revenue terms.
constraint_indexing:constraint_classification(developer_market_power, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: ENTERPRISE DEVELOPER ORGANIZATION (ROPE) — Organized through internal governance and cross-platform deployment strategies. Mobile: can invest in multi-platform presence, build abstraction layers, negotiate directly with platforms. Experiences the constraint primarily as coordination: standardized APIs enable large-scale development. Extraction is minimal because organization has structural exit capacity and negotiating power.
constraint_indexing:constraint_classification(developer_market_power, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: PLATFORM OPERATOR (TANGLED ROPE) — Institutional beneficiary with arbitrage options. Coordinates ecosystem through APIs, SDKs, and governance structures. Extracts through platform control: rent collection on transactions, data harvesting, behavioral control through algorithm and feature gating. Genuine coordination function (enables distributed development) coexists with asymmetric extraction (captures surplus value, controls feature access).
constraint_indexing:constraint_classification(developer_market_power, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY FRAMEWORK (PITON) — Competition policy and labor frameworks designed for industrial-era markets increasingly theatrical in application to platform ecosystems. Antitrust actions and labor regulations exist but enforcement is fragmented, slow, and vulnerable to regulatory capture. Theater ratio high: apparatus persists through institutional inertia while actual gatekeeping mechanisms evolve to evade traditional regulatory levers.
constraint_indexing:constraint_classification(developer_market_power, piton,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, network effects and economies of scale in software platforms create inevitable consolidation: the coordination benefit of a single dominant platform outweighs the costs of switching. This perspective naturalizes market concentration as an immutable economic law. However, historical data (IBM, Microsoft, Android fragmentation) contradict the false summit — dominant platforms have lost power; the constraint is contingent on specific institutional choices, not inevitable.
constraint_indexing:constraint_classification(developer_market_power, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(developer_market_power_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(developer_market_power, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(developer_market_power, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(developer_market_power, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(developer_market_power, TR),
    TR >= 0.70.

:- end_tests(developer_market_power_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Platform operators extract through multiple channels: transaction fees (15-30% typical), data harvesting (user behavior analytics, traffic patterns), and behavioral control (algorithmic ranking, feature gating). The extraction is not total because platforms do provide genuine coordination value and some developers capture surplus (top 1% earn substantial revenue). The 0.58 value reflects persistent but not maximal extraction — platforms cannot extract 100% without killing the ecosystem, and some developers have exit optionality. Suppression (0.65): High. Barriers to exit include: specialized ecosystem knowledge (platform-specific APIs, languages, tooling), network effects (users concentrated on dominant platforms), career path dependency (platform experience valued in hiring), and data lock-in (platform controls developer analytics and access data). Suppression is not total because multi-platform development is feasible at scale and open-source alternatives exist, but the costs are material. Theater ratio (0.48): Moderate-low. Platform discourse emphasizes community-building, developer support, and ecosystem value-add. These claims are partially true but obscure the extraction mechanisms. Theater has grown from 0.22 (early platforms, more clearly extraction-focused) to 0.48 (mature platforms, sophisticated legitimation narratives). Not fully theatrical (0.70+) because technical coordination value is genuine; not minimal (0.20) because legitimation narratives are substantial. Tangled Rope classification is correct: genuine coordination (APIs, distribution, tooling) coexists with asymmetric extraction (rent capture, data harvesting, feature control).
 *
 * PERSPECTIVAL GAP:
 *   The constraint manifests as extraction (snare) to powerless new developers with no exit capacity, as hybrid coordination-extraction (tangled rope) to mid-career developers who can exit at cost, as pure coordination (rope) to enterprise organizations with cross-platform strategies and negotiating power, as beneficial coordination (tangled rope) to the platform operator, as degraded regulation (piton) to policy frameworks, and as natural law (false summit mountain) to analysts who naturalize network effects. This perspectival range is not ambiguity — it is precision measurement showing how structural asymmetry manifests differently depending on position. The snare→rope shift depends entirely on exit capacity, not on changing the constraint itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Emerging developers with trapped exit experience maximum directionality toward the target (d ≈ 0.95): they cannot leave, must absorb extraction, and have no negotiating power. The sigmoid f(d) produces f(0.95) ≈ 1.42, amplifying their experienced extractiveness. Mid-career developers with constrained exit experience moderate directionality (d ≈ 0.70): they can exit at cost, but the costs are material enough that they remain. Enterprise organizations with arbitrage options experience low directionality (d ≈ 0.20): they can deploy across platforms, negotiate terms, and build abstraction layers; they benefit from standardized coordination mechanisms without bearing primary extraction. Platform operators as beneficiaries with arbitrage (d ≈ 0.05) experience negative effective extractiveness (they are the extractors, not the targets). The perspectival gap is driven by this directionality variance: the same constraint structure (platform control + API coordination) produces snare experience (trapped), tangled rope (constrained), rope (mobile, organized), and tangled rope from the operator (beneficiary).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves through recognizing that platform market power is neither pure coordination (rope) nor pure extraction (snare), but a tangled hybrid where coordination mechanisms serve dual purposes: they genuinely solve collective action problems (standardized APIs enable distributed development) AND they serve as lock-in levers (the same APIs that enable development also control access and visibility). The platform benefits from conflating these functions — describing extraction levers as coordination benefits. The classification as tangled rope captures this precisely: the constraint must be analyzed as two simultaneous mechanisms, not as one phenomenon mislabeled. The perspectival gap (snare→rope by exit options) shows that the question 'is this extraction or coordination?' is answered differently by agents with different exit capacities. For trapped agents, coordination mechanisms are experienced as extraction levers; for mobile agents, they are experienced as genuine coordination. Both are correct structural readings of the same constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    network_effects_magnitude,
    'How much of the measured lock-in derives from genuine network effects vs. artificial barrier creation and switching cost engineering?',
    'Comparative analysis of platforms with genuine vs. engineered switching costs; measurement of developer churn and migration when switching costs are reduced',
    'If primarily network effects: constraint is closer to rope (coordination with natural scalability premium). If primarily engineered barriers: constraint is closer to snare (extraction masquerading as coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effects_magnitude, empirical, 'Whether lock-in is driven by network effects or engineered barriers').

omega_variable(
    developer_exit_capacity,
    'Is the high suppression score (0.65) structural (developers genuinely cannot exit) or identity-locked (developers cannot imagine themselves outside the platform ecosystem)?',
    'Post-exit trajectory studies: when developers leave platforms, do new barriers emerge or does career mobility persist? Identity-fusion analysis: do developers resist exit options due to material costs or cognitive framing?',
    'If structural suppression: snare classification is correct. If identity-locked: the constraint binds cognitive rather than material barriers; reclassify trapped perspective as identity_locked and shift from snare to rope at biographical horizon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developer_exit_capacity, empirical, 'Whether developer suppression is structural or identity-based').

omega_variable(
    coalition_emergence_threshold,
    'Under what conditions do powerless emerging developers achieve organized power through coalition formation (unions, collective negotiation, open-source movements)?',
    'Historical case analysis: Linux kernel, Apache Foundation, open-source governance transitions. Measurement of developer sentiment and collective action readiness.',
    'If threshold is low and frequently breached: powerless perspective could shift to organized. Classification would shift from snare to tangled_rope or even rope. If threshold is high and rarely breached: suppression is effective and snare classification is sustained.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coalition_emergence_threshold, empirical, 'Coalition formation threshold for developer organizing').

omega_variable(
    platform_succession_mechanism,
    'What structural conditions enable or prevent new platforms from displacing incumbents? Is the constraint a feature of current platform architecture or an inevitable consequence of network effects?',
    'Historical analysis of platform transitions (MySpace→Facebook, Yahoo→Google, desktop→mobile). Identification of institutional vs. technical factors enabling succession.',
    'If succession is mechanically possible: the constraint is contingent and potentially scaffoldable (temporary, with sunset). If succession is structurally blocked: the constraint is a snare or mountain. The civilizational mountain perspective is false if succession is historically attainable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_succession_mechanism, empirical, 'Whether platform succession is mechanically possible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(developer_market_power, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(devmp_tr_t0, developer_market_power, theater_ratio, 0, 0.22).
narrative_ontology:measurement(devmp_tr_t5, developer_market_power, theater_ratio, 5, 0.38).
narrative_ontology:measurement(devmp_tr_t10, developer_market_power, theater_ratio, 10, 0.48).
narrative_ontology:measurement(devmp_tr_t15, developer_market_power, theater_ratio, 15, 0.55).

% Extraction over time
narrative_ontology:measurement(devmp_be_t0, developer_market_power, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(devmp_be_t5, developer_market_power, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(devmp_be_t10, developer_market_power, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(devmp_be_t15, developer_market_power, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(developer_market_power, resource_allocation).
narrative_ontology:affects_constraint(developer_market_power, app_store_gatekeeping).
narrative_ontology:affects_constraint(developer_market_power, venture_capital_concentration).
narrative_ontology:affects_constraint(developer_market_power, technical_debt_accumulation).

% DUAL FORMULATION NOTE:
% Developer market power decomposes into distinct constraints by measurement basis: API standardization lock-in (ε≈0.42, tangled rope) is separable from rent extraction mechanisms (ε≈0.68, snare) and regulatory theater (ε≈0.35, piton). These stories are linked through platform operator behavior but represent distinct structural phenomena with different extractiveness profiles and causal mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(developer_market_power, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
