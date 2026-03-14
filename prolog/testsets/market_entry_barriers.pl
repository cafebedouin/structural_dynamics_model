% ============================================================================
% CONSTRAINT STORY: market_entry_barriers
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_entry_barriers, []).

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
 *   constraint_id: market_entry_barriers
 *   human_readable: Market Entry Barriers and Incumbent Extraction
 *   domain: economic/competitive_dynamics
 *
 * SUMMARY:
 *   Market entry barriers represent a fundamental tension in competitive
 *   economies: some barriers are necessary to coordinate production, ensure
 *   quality, and create incentives for innovation; others serve primarily to
 *   extract rents from potential entrants and consumers. This constraint
 *   manifests differently depending on the observer's structural position. An
 *   incumbent firm experiences barriers as legitimate coordination mechanisms
 *   that solve real problems (capital mobilization, quality assurance,
 *   network reliability). An aspiring entrepreneur experiences them as
 *   extraction traps that foreclose opportunity. A regulatory authority sees
 *   them as contingent policy choices amenable to modification through
 *   antitrust enforcement and patent reform. A professional licensing board
 *   performs the legitimacy of its barriers while the actual function has
 *   atrophied. The analytical observer risks mistaking contingent
 *   institutional choices (patent duration, capital requirements, licensing
 *   standards) for natural laws inherent to markets. The constraint's
 *   extractiveness has risen over the measurement interval (0.38 → 0.52) as
 *   incumbent firms have developed increasingly sophisticated
 *   barrier-maintenance strategies (network effects leveraged through
 *   software updates, intellectual property enforcement, exclusive
 *   distribution agreements, and regulatory capture of licensing boards).
 *   Theater ratio has also risen (0.35 → 0.48) as barriers accumulate
 *   organizational legitimacy narratives disconnected from their functional
 *   necessity.
 *
 * KEY AGENTS:
 *   - Incumbent Firms: Primary beneficiary (institutional/arbitrage) — capture rents through controlled barriers; experience barriers as coordination mechanisms; lowest-cost exit via divestment or geographic redeployment
 *   - Potential Entrants: Primary victim (powerless/trapped) — face capital requirements, licensing costs, established networks, and predatory response; trapped by structural barriers without viable entry paths
 *   - Consumer Choice Diversity: Secondary victim (powerless/trapped) — limited to incumbent offerings due to suppressed competition; cannot organize exit from market structure
 *   - Regional Competitors: Mixed agent (moderate/constrained) — can enter niches but face sustained extraction through targeted barriers and litigation; constrained by cost asymmetry
 *   - Regulatory Authorities: Organized agent (organized/mobile) — can redesign barrier heights through antitrust enforcement, patent reform, and deregulation; mobile exit through policy resets
 *   - Professional Licensing Boards: Institutional actor (institutional/arbitrage) — maintain barriers through performative legitimacy; captured by incumbent practitioners; arbitrage exit via board composition changes
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional choices as inherent market necessities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_entry_barriers, 0.52).
domain_priors:suppression_score(market_entry_barriers, 0.58).
domain_priors:theater_ratio(market_entry_barriers, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_entry_barriers, extractiveness, 0.52).
narrative_ontology:constraint_metric(market_entry_barriers, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(market_entry_barriers, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_entry_barriers, tangled_rope).
narrative_ontology:human_readable(market_entry_barriers, "Market Entry Barriers and Incumbent Extraction").
narrative_ontology:topic_domain(market_entry_barriers, "economic/competitive_dynamics").

domain_priors:requires_active_enforcement(market_entry_barriers).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_entry_barriers, incumbent_firms).
narrative_ontology:constraint_victim(market_entry_barriers, potential_entrants).
narrative_ontology:constraint_victim(market_entry_barriers, consumer_choice_diversity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ASPIRING ENTREPRENEUR (SNARE) — Cannot enter without capital, regulatory approval, or established distribution channels. Trapped by structural barriers: capital requirements, licensing costs, first-mover advantage, and network effects that the incumbent controls. No viable exit from the market structure without abandoning entry entirely.
constraint_indexing:constraint_classification(market_entry_barriers, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: REGIONAL COMPETITOR (TANGLED ROPE) — Can enter a niche segment but faces sustained suppression via predatory pricing, exclusive distribution deals, and IP litigation. Genuinely coordinates some market functions (localized production, price competition in segments) while bearing extraction through barriers that increase costs and limit scalability.
constraint_indexing:constraint_classification(market_entry_barriers, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT FIRM (ROPE) — Controls barriers as coordination mechanism: capital requirements ensure quality standards, licensing ensures safety compliance, network effects ensure system reliability. Experiences the constraint as coordination. High extraction flows toward incumbent; exits costless (can exit by divestment or redeployment).
constraint_indexing:constraint_classification(market_entry_barriers, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AUTHORITY (SCAFFOLD) — Antitrust enforcement, patent reform, and open-access mandates create temporary mechanisms that lower barriers. These have sunset logic: as markets mature and competition increases, formal barriers can be reduced. Moderate effective extraction because enforcement bodies have mobility and can redesign the rule set.
constraint_indexing:constraint_classification(market_entry_barriers, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PROFESSIONAL LICENSING (PITON) — Licensing ostensibly ensures competence but often functions as an occupational cartels maintained through inertia. The barrier persists though its original function has attenuated. Theater ratio remains high as the system performs legitimacy (boards, continuing education, ethics codes) while primary function is rent protection.
constraint_indexing:constraint_classification(market_entry_barriers, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal analytical view, ALL markets require SOME barriers to entry to function: capital requirements fund production, licensing ensures quality/safety, intellectual property creates incentives for innovation. From this view, barriers are not extraction but necessary coordination costs inherent to functional markets. This perspective risks naturalizing what are actually contingent institutional choices (how high should capital requirements be? how long should patents last?).
constraint_indexing:constraint_classification(market_entry_barriers, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_entry_barriers_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(market_entry_barriers, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(market_entry_barriers, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_entry_barriers, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(market_entry_barriers, TR),
    TR >= 0.70.

:- end_tests(market_entry_barriers_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts significant rents from potential entrants and consumers: entry costs are elevated by legitimate coordination needs (capital, licensing) plus incumbent-maintained barriers (exclusive distribution, IP litigation, predatory pricing). The measured value reflects that some barriers serve coordination (justified extraction) while others serve rent capture (unjustified). The increase over the interval reflects accumulation of barrier strategies. Suppression (0.58): High. Multiple mechanisms suppress entry: capital requirements ($1M–$100M+ depending on industry), regulatory approval timelines (1–5 years), IP licensing costs, first-mover advantage in network effects, and incumbent predatory response. These barriers are sufficiently comprehensive that most potential entrants cannot overcome them even with effort. Theater ratio (0.48): Moderate. Professional licensing performs legitimacy narrative (competence assurance, ethics codes, continuing education) but significant portion of the barrier serves incumbent rent protection rather than public safety. The rise from 0.35 to 0.48 reflects increasing narrative complexity as barriers accumulate competing justifications. Claimed type (Tangled Rope): The constraint coordinates legitimate market functions (capital mobilization ensures productive capacity, licensing ensures quality standards, IP protects innovation incentives) while simultaneously extracting rents from entrants and consumers (barriers elevated above necessary levels, predatory pricing maintains exclusion, regulatory capture maintains sub-functional licensing standards).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how market barriers are constructed as either coordination or extraction depending on structural position. The incumbent firm sees barriers as solving legitimate problems: capital requirements ensure productive capacity, licensing ensures quality, IP creates innovation incentives, network effects create system reliability. From this perspective, barriers are Rope (coordination) — the constraint solves a real collective action problem. The aspiring entrepreneur sees the same barriers as extraction traps: high capital costs exclude the resourceful-but-underfunded, licensing creates gatekeeping, IP litigation weaponizes legal systems, network effects create winner-take-all capture. From this perspective, barriers are Snare (pure extraction) — the same mechanisms that coordinate from above discoordinate from below. The regulatory authority sees barriers as contingent policy choices: capital requirements can be lowered via venture capital intermediation, licensing can be streamlined via equivalency assessment, IP can be balanced via patent reform, network effects can be disrupted via interoperability mandates. From this perspective, barriers are Scaffold (temporary) — the constraint has sunset logic via policy redesign. The licensing board sees barriers as legitimate professional standards, but structural data reveals performative content: boards are captured by incumbents, standards exceed public safety requirements, and renewal cycles are extractive (fees) rather than functional (competence verification). The analytical observer risks seeing barriers as natural — ALL functional markets require some entry barriers to coordinate capital investment, ensure quality, and incentivize innovation — and therefore sees the constraint as Mountain (immutable natural law). But the structural data reveals this as false summit: the HEIGHT of barriers (capital requirements, licensing standards, IP term length) is a contingent institutional choice, not a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extraction (chi) is computed from base extractiveness (0.52) scaled by directionality (d) and spatial scope (σ). Incumbents with arbitrage options and net-beneficiary status have d ≈ 0.15 → f(d) ≈ -0.01 → chi ≈ -0.005 (experienced as subsidy). Regional competitors with constrained options and mixed victim status have d ≈ 0.60 → f(d) ≈ 0.88 → chi ≈ 0.46 (experienced as moderate extraction). Potential entrants with trapped status and victim classification have d ≈ 0.95 → f(d) ≈ 1.42 → chi ≈ 0.74 (experienced as severe extraction). Regulatory authorities with mobile options have d ≈ 0.50 → f(d) ≈ 0.65 → chi ≈ 0.34 (experienced as negotiable). The perspectival gap is maximal: the same base barrier height is experienced as non-existent (incumbent), negotiable (regulator), constraining (competitor), and insurmountable (entrant).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: Market entry barriers resolve the mandatrophy by revealing that the 'is this coordination or extraction?' question is insufficiently precise. The answer is: BOTH, asymmetrically distributed. Barriers coordinate production at the incumbent level (capital mobilization works, quality standards function, IP incentivizes innovation). They extract from entrants and consumers (barriers set higher than necessary, predatory strategies exclude viable competitors, licensing captures regulatory authority). The Tangled Rope classification holds: genuine coordination function (barriers do solve real problems) coexists with asymmetric extraction (barriers distributed to concentrate benefits toward incumbents). The mandatrophy is resolved NOT by choosing between pure coordination and pure extraction, but by recognizing that the same institutional mechanism serves both functions with different distributions. The perspectival gap is the analytic tool: incumbents see coordination (Rope experience), entrants see extraction (Snare experience), regulatory authority sees contingency (Scaffold perspective). None of these are 'wrong' — they are different structural truths from different positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    barrier_necessity_threshold,
    'What level of capital requirement, licensing restriction, or IP protection represents legitimate coordination cost versus extractive rent-seeking?',
    'Comparative institutional analysis: track entry rates and innovation output across markets with varying barrier heights; identify correlation between barrier level and consumer welfare outcomes',
    'If barriers below threshold: insufficient coordination, market failures (quality collapse, inadequate investment). If barriers above threshold: extractive rent capture, reduced competition, consumer harm. The threshold varies by industry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(barrier_necessity_threshold, empirical, 'Threshold distinguishing legitimate coordination barriers from extractive rents').

omega_variable(
    incumbent_predation_measurement,
    'Do observed incumbent behaviors (exclusive dealing, predatory pricing, IP litigation) constitute legitimate competitive response or illegal predation?',
    'Longitudinal cost analysis; comparison of incumbent behavior toward entrants versus toward each other; assessment of profit sustainability post-entry',
    'If competitive response: moderate suppression, some entry possible. If predation: high suppression, extraction mechanism is intact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incumbent_predation_measurement, empirical, 'Whether incumbent behaviors are competitive or predatory').

omega_variable(
    network_effects_lock_in,
    'Are network effects creating legitimate coordination value or extractive lock-in where switching costs exceed replacement value?',
    'Measurement of switching costs versus alternative provider quality; tracking of user satisfaction pre/post-entry attempts; analysis of interoperability adoption',
    'If coordination value: justified barriers, moderate extraction. If lock-in: barriers sustain extraction despite superior alternatives, high extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effects_lock_in, empirical, 'Network effects as coordination versus lock-in mechanism').

omega_variable(
    regulatory_capture_in_licensing,
    'To what extent have professional licensing boards been captured by incumbent practitioners to maintain barrier heights above what public safety requires?',
    'Analysis of licensing board composition; correlation between board recommendations and actual entry effects; measurement of public safety outcomes across high/low-barrier jurisdictions',
    'If high capture: piton classification confirmed, barriers are theatrical. If low capture: barriers are functional coordination, moderate extraction justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_in_licensing, empirical, 'Regulatory capture in professional licensing systems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_entry_barriers, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(meb_tr_t0, market_entry_barriers, theater_ratio, 0, 0.35).
narrative_ontology:measurement(meb_tr_t3, market_entry_barriers, theater_ratio, 3, 0.4).
narrative_ontology:measurement(meb_tr_t6, market_entry_barriers, theater_ratio, 6, 0.46).
narrative_ontology:measurement(meb_tr_t9, market_entry_barriers, theater_ratio, 9, 0.48).

% Extraction over time
narrative_ontology:measurement(meb_be_t0, market_entry_barriers, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(meb_be_t3, market_entry_barriers, base_extractiveness, 3, 0.44).
narrative_ontology:measurement(meb_be_t6, market_entry_barriers, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(meb_be_t9, market_entry_barriers, base_extractiveness, 9, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_entry_barriers, resource_allocation).
narrative_ontology:affects_constraint(market_entry_barriers, regulatory_capture).
narrative_ontology:affects_constraint(market_entry_barriers, intellectual_property_duration).
narrative_ontology:affects_constraint(market_entry_barriers, network_effects_winner_take_all).

% DUAL FORMULATION NOTE:
% Market entry barriers decompose into three structurally distinct constraints: (1) capital_allocation_requirements (ε=0.25, Rope) — legitimate coordination of productive capacity; (2) regulatory_licensing_gatekeeping (ε=0.48, Tangled Rope) — mixed coordination and capture; (3) incumbent_predatory_behavior (ε=0.58, Snare) — pure extraction. This story models the aggregate effect; disaggregation reveals which barrier mechanisms are functional versus extractive.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(market_entry_barriers, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
