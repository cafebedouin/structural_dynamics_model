% ============================================================================
% CONSTRAINT STORY: allied_capital_mobility_restriction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_allied_capital_mobility_restriction, []).

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
 *   constraint_id: allied_capital_mobility_restriction
 *   human_readable: Allied Capital Mobility Restriction in Geopolitical Coordination
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   Allied capital mobility restrictions represent a geopolitical
 *   coordination mechanism that simultaneously generates asymmetric
 *   extraction. Justified by state security doctrine (preventing adversarial
 *   bloc capital infiltration and capital flight during crises), these
 *   restrictions operate across wealthy allied nations to maintain strategic
 *   cohesion and enable directed investment in critical sectors. The
 *   constraint exhibits tension between genuine coordination functions
 *   (unified investment frameworks, bloc internal stability) and extractive
 *   mechanisms (wealth concentration, prevented arbitrage, regulatory
 *   rent-seeking by incumbent capital). The theater ratio (0.55) reflects
 *   that enforcement is selective — restrictions nominally apply uniformly
 *   but are enforced differentially depending on actor class, with
 *   significant compliance theater (licensing regimes, exemption processes)
 *   that masks variable enforcement. The extractiveness trajectory (0.35→0.58
 *   over six time intervals) reflects increasing tightening as geopolitical
 *   tensions escalate, with each new crisis layer adding restrictions to
 *   existing frameworks. The constraint embodies the core diagnostic
 *   challenge: are these restrictions contingent on specific geopolitical
 *   alignment structures (and thus subject to sunset as alignment shifts), or
 *   are they semi-permanent institutional features that have become decoupled
 *   from their original strategic rationale?
 *
 * KEY AGENTS:
 *   - Restricted Wealth Holders: Primary victims (powerless/trapped) — citizens unable to move capital across borders without asset abandonment or citizenship loss; experience maximum extraction with no legitimate exit
 *   - Mid-Tier Investors: Secondary victims (moderate/constrained) — face friction costs, compliance burden, and asymmetric regulatory treatment; some benefits from bloc-internal coordination but extraction exceeds benefits
 *   - Bloc Leadership: Primary beneficiary (institutional/arbitrage) — captures strategic benefit from prevented capital flight; directs investment toward state-aligned priorities; experiences restrictions as essential coordination
 *   - State Security Apparatus: Secondary beneficiary (institutional/arbitrage) — maintains leverage through financial chokepoints; uses restrictions to prevent hostile financing and enforce sanctions regimes
 *   - Financial Liberalization Coalition: Organized opposition (organized/constrained) — transnational advocates for capital mobility push back through norm-setting and institutional architecture; see restrictions as temporary emergency measures with visible sunset path
 *   - Cold War Regulatory Remnant: Institutional layer (institutional/mobile) — legacy restriction frameworks activated by current geopolitical tension; enforcement shows significant theater as old rules apply to new contexts
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent strategic doctrine as immutable law of geopolitics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(allied_capital_mobility_restriction, 0.58).
domain_priors:suppression_score(allied_capital_mobility_restriction, 0.65).
domain_priors:theater_ratio(allied_capital_mobility_restriction, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(allied_capital_mobility_restriction, extractiveness, 0.58).
narrative_ontology:constraint_metric(allied_capital_mobility_restriction, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(allied_capital_mobility_restriction, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(allied_capital_mobility_restriction, tangled_rope).
narrative_ontology:human_readable(allied_capital_mobility_restriction, "Allied Capital Mobility Restriction in Geopolitical Coordination").
narrative_ontology:topic_domain(allied_capital_mobility_restriction, "geopolitical/economic").

domain_priors:requires_active_enforcement(allied_capital_mobility_restriction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(allied_capital_mobility_restriction, geopolitical_bloc_leadership).
narrative_ontology:constraint_beneficiary(allied_capital_mobility_restriction, state_security_apparatus).
narrative_ontology:constraint_victim(allied_capital_mobility_restriction, capital_holding_citizens).
narrative_ontology:constraint_victim(allied_capital_mobility_restriction, cross_border_investment_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESTRICTED WEALTH HOLDER (SNARE) — Citizens holding capital within allied nations face mandatory restrictions on asset movement across borders, justified by national security and geopolitical stability. No legitimate exit without abandoning wealth or citizenship. Bear full extraction cost while bloc leadership captures strategic benefit.
constraint_indexing:constraint_classification(allied_capital_mobility_restriction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-TIER INVESTOR (TANGLED ROPE) — Can partially move capital through approved channels and benefit from some bloc-internal coordination (unified investment frameworks, reduced intra-bloc tariffs), but faces significant friction costs, compliance burden, and capital gains taxes on cross-border flows. Mixed experience: coordination benefits exist alongside asymmetric extraction.
constraint_indexing:constraint_classification(allied_capital_mobility_restriction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: BLOC LEADERSHIP (ROPE) — Experiences capital restrictions as a coordination mechanism: preventing capital flight during geopolitical tensions preserves bloc cohesion and enables directed investment in strategic sectors (defense, critical infrastructure, allied-bloc-internal development). Benefits from prevented arbitrage and capital capture; extraction runs toward this agent.
constraint_indexing:constraint_classification(allied_capital_mobility_restriction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HEGEMON'S STATE SECURITY (ROPE) — Capital controls serve explicit strategic interests: blocking capital flight to adversarial blocs, maintaining control over key economic flows, preventing hostile leverage of financial dependencies. Sees restrictions as essential coordination for geopolitical stability and defensive positioning. Primary beneficiary.
constraint_indexing:constraint_classification(allied_capital_mobility_restriction, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: FINANCIAL LIBERALIZATION COALITION (SCAFFOLD) — Transnational finance organizations, OECD economists, and capital-mobility advocates see restrictions as temporary emergency measures necessary during heightened geopolitical tension. Organized pushback includes technical analysis, norm-setting (FATF guidelines, international financial standards), and institutional architecture for gradual liberalization. Exit path visible: as geopolitical tensions ease, restrictions sunset through negotiated agreements and norm adoption.
constraint_indexing:constraint_classification(allied_capital_mobility_restriction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: COLD WAR REGULATORY REMNANT (PITON) — Many allied-bloc capital restrictions trace to Cold War-era containment frameworks that remain on the books long after their strategic rationale degraded. Renewed geopolitical tension has reactivated dormant restrictions, creating performative compliance infrastructure — enforcement targeting hostile-bloc actors is real, but enforcement toward allied-bloc citizens shows significant theater. The mechanism persists through institutional inertia and legal grandfather clauses rather than active strategic necessity.
constraint_indexing:constraint_classification(allied_capital_mobility_restriction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective on geopolitical stability, capital immobility in adversarial blocs is a structural fact of sovereignty and military deterrence: states cannot enable capital flight to hostile actors without undermining strategic position. This framing naturalizes restrictions as immutable laws of geopolitics. However, structural data reveals this as a false summit — capital controls are contingent on institutional design, bloc interdependence, and strategic doctrine choice, not immutable principles.
constraint_indexing:constraint_classification(allied_capital_mobility_restriction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(allied_capital_mobility_restriction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(allied_capital_mobility_restriction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(allied_capital_mobility_restriction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(allied_capital_mobility_restriction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(allied_capital_mobility_restriction, TR),
    TR >= 0.70.

:- end_tests(allied_capital_mobility_restriction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Capital restrictions generate substantial asymmetric benefit toward bloc leadership and state security apparatus while imposing costs on wealth holders. Unlike pure taxation or direct expropriation, restrictions operate through prevented arbitrage and directed investment — the extraction is structural rather than explicit. The value reflects that real coordination functions exist (unified bloc investment frameworks, prevented destabilizing capital flight) but are substantially outweighed by extraction mechanisms. Suppression (0.65): High. Multiple barriers to exit exist: legal prohibition on cross-border capital flows, citizenship-contingent wealth access, regulatory exemptions available only to state-aligned actors, and framing of capital movement as unpatriotic/disloyal. However, suppression is not total — organized agents (financial institutions, multinational corporations) maintain exemption pathways, and some wealth can move through approved channels. Theater ratio (0.55): Moderate-high. Enforcement of restrictions shows significant performative content: licensing regimes create appearance of careful gatekeeping while exemptions are granted to politically connected actors. Cold War-era legal frameworks are dusted off and reapplied to contemporary geopolitical contexts, creating theater of emergency measures. However, theater does not reach piton levels because real enforcement occurs (actors attempting prohibited flows are genuinely blocked), and the underlying strategic rationale, while contestable, is genuine for the security apparatus.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a stark perspectival divergence driven by exit option heterogeneity at the same institutional power level. Bloc leadership sees Rope (coordination mechanism for strategic stability). The state security apparatus also sees Rope (strategic control and leverage). But the financial liberalization coalition, also institutional-power, sees Scaffold (temporary measure with sunset through norm adoption and international agreement). The gap reveals that institution-to-institution relationships are not homogeneous — directionality heterogeneity at the institutional level is driven entirely by exit options (arbitrage vs constrained) and beneficiary/victim status. The mid-tier investor sees Tangled Rope precisely because they have both coordination benefits (access to approved bloc-internal investment) and asymmetric extraction costs (prevented outflows and regulatory compliance burden). The powerless wealth holder sees Snare because they have no arbitrage, no exemptions, and no coordination benefits.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies substantially by agent power and exit options. Wealth holders with trapped exit (no legal pathway to move capital) derive d ≈ 0.95 (near-total target) from their victim status. Mid-tier investors with constrained exit derive d ≈ 0.60 (mixed) from partial exemption pathways and some coordination benefits. Bloc leadership with arbitrage exit derives d ≈ 0.10 (near-total beneficiary) — they can access international capital markets through state channels and experience restrictions as enabling rather than constraining. The state security apparatus derives similar d from its institutional control over exemption mechanisms. Financial liberalization advocates derive d ≈ 0.65 (mixed victim with some agency) — they bear the cost of restrictions through constrained markets but possess significant organizational power and external support. The Cold War regulatory remnant, as an institutional layer rather than an agent, does not directly participate in directionality — it provides the enforcement infrastructure that other agents experience as constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by clearly distinguishing its coordination and extraction functions at the base_properties level. The beneficiaries (bloc leadership, security apparatus) are genuine — they benefit from the coordination of bloc resources and prevention of destabilizing capital flight. The victims (wealth holders, investment efficiency) are real — they bear genuine costs through prevented arbitrage and directed investment. The requires_active_enforcement flag is true — restrictions require continuous enforcement infrastructure, exemption gatekeeping, and sanctions coordination. The Tangled Rope classification is appropriate because all three gates fire: there is measurable coordination function (bloc-internal investment frameworks), there is asymmetric extraction (wealth concentration toward state-aligned actors), and enforcement is active (capital movement is monitored and restricted). The classification prevents both false optimization (mistaking restrictions as pure coordination) and false victimization (treating all coordinated capital as extraction). The perspectival diversity further prevents mandatrophy — different observer positions legitimately see different types (Rope, Tangled Rope, Scaffold, Piton) reflecting their different structural relationships to the same constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dual_bloc_alignment_stability,
    'How stable is the current geopolitical bifurcation that justifies allied-bloc capital controls? Do restrictions persist if alignment shifts or multipolar structures emerge?',
    'Longitudinal tracking of bloc coherence; analysis of capital flow patterns if geopolitical alliance architecture changes; comparison to multipolar scenarios where tri- or multipolar alignment reduces the binary-bloc extraction mechanism',
    'If bifurcation proves temporary: restrictions are contingent on specific geopolitical structure and could sunset. If multipolar becomes dominant: restrictions become obsolete entirely. If bifurcation hardens: restrictions become semi-permanent institutional features.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dual_bloc_alignment_stability, empirical, 'Stability of binary bloc alignment underlying capital restriction regime').

omega_variable(
    strategic_necessity_vs_regulatory_inertia,
    'Are capital restrictions genuinely strategically necessary for bloc security, or do they persist primarily through regulatory path dependence and Cold War-era legal frameworks?',
    'Comparative analysis of strategic outcomes with vs without restrictions; audit of enforcement intensity targeting different actor classes; timeline analysis of when restrictions were activated/deactivated relative to geopolitical events',
    'If genuinely necessary: Tangled Rope classification confirmed. If primarily inertial: Piton classification rises, and many restrictions should be deactivated or reformed as symbolic rather than functional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_necessity_vs_regulatory_inertia, empirical, 'Whether restrictions are strategically necessary or institutionally inertial').

omega_variable(
    bloc_internal_coordination_benefit_magnitude,
    'Do capital restrictions actually generate positive coordination benefits within allied blocs, or are they primarily extractive with minimal real coordination function?',
    'Economic analysis of directed investment efficiency; comparison of intra-bloc capital allocation quality under restriction vs historical periods of liberalization; measurement of whether prevented capital flight actually improves strategic positioning or merely concentrates extraction',
    'If genuine coordination benefits exist: Rope or Tangled Rope (mixed) confirmed. If benefits are marginal or negative: Snare classification rises — restrictions become primarily extractive with minimal coordination cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bloc_internal_coordination_benefit_magnitude, empirical, 'Magnitude of real coordination benefits from capital restrictions').

omega_variable(
    regulatory_capture_by_incumbent_capital,
    'Do incumbent institutional investors and state-aligned capital use the restriction regime to prevent competitive entry and new capital formation?',
    'Analysis of capital concentration trends under restrictions; comparison of new firm formation rates before/after restriction implementation; tracking of who benefits from enforcement gaps and regulatory exemptions',
    'If incumbents capture regulatory exceptions: effective extraction rises, and the Snare classification strengthens even for moderate-power agents. If restrictions apply uniformly: Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_by_incumbent_capital, empirical, 'Capture of restriction regime by incumbent capital').

omega_variable(
    identity_lock_in_economic_patriotism,
    'Do citizens internalize capital restrictions as part of national identity and patriotic duty to bloc security, making exit psychologically unthinkable even when material barriers are surmountable?',
    'Survey analysis of attitudes toward capital mobility; comparison of actual outflows vs legal permissibility; study of which actors choose restriction compliance beyond legal requirement; analysis of rhetoric framing capital movement as betrayal vs legitimate economic activity',
    'If identity-lock is strong: some perspectives should use identity_locked exit option rather than constrained/mobile, raising their classification severity. If weak: exit framing should emphasize material barriers rather than identity fusion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_in_economic_patriotism, conceptual, 'Identity-lock on capital restriction compliance via patriotic framing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(allied_capital_mobility_restriction, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acmr_tr_t0, allied_capital_mobility_restriction, theater_ratio, 0, 0.4).
narrative_ontology:measurement(acmr_tr_t3, allied_capital_mobility_restriction, theater_ratio, 3, 0.48).
narrative_ontology:measurement(acmr_tr_t6, allied_capital_mobility_restriction, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(acmr_be_t0, allied_capital_mobility_restriction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(acmr_be_t3, allied_capital_mobility_restriction, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(acmr_be_t6, allied_capital_mobility_restriction, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(allied_capital_mobility_restriction, resource_allocation).
narrative_ontology:affects_constraint(allied_capital_mobility_restriction, sanctions_regime_enforcement).
narrative_ontology:affects_constraint(allied_capital_mobility_restriction, financial_system_coupling).
narrative_ontology:affects_constraint(allied_capital_mobility_restriction, state_finance_dependency).

% DUAL FORMULATION NOTE:
% Allied capital restrictions operate at the intersection of geopolitical security (state-level strategic positioning) and individual economic freedom (wealth holder asset mobility). The constraint could be decomposed into separate stories tracking the security coordination function (higher coordination benefit) vs. the wealth extraction function (higher extraction cost), but they are sufficiently entangled institutionally that a single Tangled Rope story captures the hybrid accurately. Downstream constraints (sanctions regimes, financial coupling) depend on this restriction framework's continued existence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(allied_capital_mobility_restriction, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
