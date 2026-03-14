% ============================================================================
% CONSTRAINT STORY: sports_broadcast_exclusivity_regimes
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sports_broadcast_exclusivity_regimes, []).

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
 *   constraint_id: sports_broadcast_exclusivity_regimes
 *   human_readable: Sports Broadcast Exclusivity Regimes
 *   domain: media_economics/sports_governance
 *
 * SUMMARY:
 *   Sports broadcast exclusivity regimes create a constraint that appears
 *   differently to each stakeholder group. The regime bundles live sports
 *   rights into exclusive territories and platforms, preventing simultaneous
 *   distribution and forcing viewers and platforms into high-cost
 *   subscription tiers. For incumbent broadcasters and league owners, this is
 *   coordination infrastructure that solves the problem of segmenting global
 *   demand and maximizing revenue from geographically separated audiences.
 *   For viewers priced out by the bundle structure, it is pure extraction:
 *   geographic or economic lock-in with no exit. For streaming platforms, it
 *   is a barrier-to-entry that extracts through licensing costs and carve-out
 *   restrictions. For cord-cutting coalitions and policy advocates, it is a
 *   temporary institutional regime facing a visible sunset as streaming
 *   normalizes. The constraint exhibits rising extractiveness over the
 *   measurement interval (0.42 to 0.58) as streaming proliferation has forced
 *   league owners to negotiate with multiple platforms, increasing total
 *   licensing costs and exclusivity fragmentation enforcement overhead.
 *   Theater ratio remains moderate (0.48) because the coordination problem is
 *   genuine, though increasingly performative as technological change has
 *   eliminated some of the original scarcity justification.
 *
 * KEY AGENTS:
 *   - Incumbent Broadcasters (ESPN, Fox Sports): Primary beneficiary (institutional/arbitrage) — capture rents through long-term exclusive contracts and premium subscription pricing
 *   - League Owners (NFL, NBA, MLB, Premier League): Primary beneficiary (institutional/arbitrage) — maximize revenue through auction of exclusive rights to competing bidders
 *   - Cord-Cutting Viewers: Primary victim (powerless/trapped) — excluded from preferred content by geographic/economic licensing; no legal mobility within constraint
 *   - Low-Income Households: Primary victim (powerless/trapped) — priced out of premium streaming tiers required for exclusive sports content
 *   - Streaming Disruptor Platforms (Apple, Amazon, YouTube): Secondary victim (moderate/constrained) — face exclusionary licensing costs and territorial restrictions; exit requires premium bidding
 *   - Regional Sports Networks (Sinclair RSNs): Secondary victim (institutional/constrained) — coordinate local coverage but face extraction through national exclusive contracts
 *   - Cord-Cutting Coalition: Organized actors (organized/mobile) — policy advocates and consumer groups perceiving sunset through legislative and market channels
 *   - Legacy Cable System: Institutional actor (institutional/arbitrage) — maintains inertial enforcement of exclusivity logic despite cable bundle model degradation (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent market structure as immutable scarcity principle
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sports_broadcast_exclusivity_regimes, 0.58).
domain_priors:suppression_score(sports_broadcast_exclusivity_regimes, 0.65).
domain_priors:theater_ratio(sports_broadcast_exclusivity_regimes, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sports_broadcast_exclusivity_regimes, extractiveness, 0.58).
narrative_ontology:constraint_metric(sports_broadcast_exclusivity_regimes, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sports_broadcast_exclusivity_regimes, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sports_broadcast_exclusivity_regimes, tangled_rope).
narrative_ontology:human_readable(sports_broadcast_exclusivity_regimes, "Sports Broadcast Exclusivity Regimes").
narrative_ontology:topic_domain(sports_broadcast_exclusivity_regimes, "media_economics/sports_governance").

domain_priors:requires_active_enforcement(sports_broadcast_exclusivity_regimes).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sports_broadcast_exclusivity_regimes, incumbent_broadcasters).
narrative_ontology:constraint_beneficiary(sports_broadcast_exclusivity_regimes, league_owners).
narrative_ontology:constraint_beneficiary(sports_broadcast_exclusivity_regimes, premium_subscription_services).
narrative_ontology:constraint_victim(sports_broadcast_exclusivity_regimes, cord_cutting_viewers).
narrative_ontology:constraint_victim(sports_broadcast_exclusivity_regimes, low_income_households).
narrative_ontology:constraint_victim(sports_broadcast_exclusivity_regimes, emerging_broadcast_platforms).
narrative_ontology:constraint_victim(sports_broadcast_exclusivity_regimes, regional_sports_ecosystems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED VIEWER (SNARE) — Fans in regions with exclusive broadcast contracts cannot legally access their preferred team's games without purchasing expensive bundled packages or subscribing to multiple streaming services. Geographic arbitrage is prevented by licensing. Economic mobility offers no escape — the cost structure is fixed. No coordination benefit perceived; pure extraction of time and money from the captive audience.
constraint_indexing:constraint_classification(sports_broadcast_exclusivity_regimes, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STREAMING DISRUPTOR PLATFORM (TANGLED ROPE) — New entrants like Apple TV+ or Amazon Prime Video face exclusionary licensing barriers: they must bid for broadcast rights but are constrained by existing multi-year exclusive contracts held by incumbents. Exit requires either accepting limited sports content (constrained) or acquiring expensive rights at premium rates. The system coordinates league revenue stability while simultaneously extracting from new market entrants through barrier-to-entry licensing costs and geographic carve-outs.
constraint_indexing:constraint_classification(sports_broadcast_exclusivity_regimes, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT BROADCASTER/LEAGUE OLIGOPOLY (ROPE) — For ESPN, Fox Sports, and league ownership, the exclusivity regime solves the pure coordination problem of converting live sports into saleable digital products. Rights holders experience the system as coordination: it enables them to package content, segment markets, and extract maximum revenue from heterogeneous demand. The constraint preserves their ability to arbitrage across markets (cable bundles, streaming tiers, international rights) with minimal friction. Arbitrage exit available — they can license to competitors or launch competing platforms at will.
constraint_indexing:constraint_classification(sports_broadcast_exclusivity_regimes, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CORD-CUTTING COALITION (SCAFFOLD) — Organized consumer groups and policy advocates (cord-cutting networks, consumer protection organizations, state legislators) perceive the exclusivity regime as a temporary coordination problem with a visible sunset. The coalition has agency: legislative challenges (antitrust review, local broadcast carve-outs), market alternatives (sports bars, league apps, free-to-air matches), and generational shifts (younger audiences uninterested in cable bundles) are fragmenting the exclusivity mechanism. The constraint has built-in sunset timing as cable cord-cutting accelerates and streaming normalization completes.
constraint_indexing:constraint_classification(sports_broadcast_exclusivity_regimes, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGACY CABLE TELEVISION SYSTEM (PITON) — The terrestrial cable bundling model that underpinned exclusivity (forcing sports fans to subscribe to cable packages that include sports channels) is itself degraded. Streaming has partially replaced cable, yet exclusivity contracts still enforce the cable-era logic. The system persists through institutional inertia: long-term contracts lock in the old model even as the technological basis (cable subscription) erodes. Theater ratio reflects this: much negotiating effort goes to maintaining exclusivity terms that no longer fit market realities. The constraint is inertial rather than functional.
constraint_indexing:constraint_classification(sports_broadcast_exclusivity_regimes, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REGIONAL SPORTS NETWORK ECOSYSTEM (TANGLED ROPE) — Local and regional broadcasters (Sinclair Broadcast Group, regional sports networks) are squeezed: they coordinate local coverage and fan engagement, but face extraction through exclusive national contracts that bypass regional markets. Constrained exit — they can attempt to acquire streaming rights but face financial barriers and coordinated exclusion by national leagues preferring centralized distribution. Mixed experience: genuine coordination of local markets combined with asymmetric extraction upward to national rights holders.
constraint_indexing:constraint_classification(sports_broadcast_exclusivity_regimes, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, this perspective claims the exclusivity regime is an immutable feature of sports media: scarcity of live broadcast rights creates natural monopolistic pricing; sports fans have inelastic demand; geographic separation of audiences (timezones, regional preferences) necessitates exclusive licensing to coordinate distribution. However, structural data contradicts the mountain classification — the engine will detect this as a false summit. Exclusivity is a contingent institutional arrangement (contractual, enforceable, reversible) not a natural law.
constraint_indexing:constraint_classification(sports_broadcast_exclusivity_regimes, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sports_broadcast_exclusivity_regimes_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sports_broadcast_exclusivity_regimes, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sports_broadcast_exclusivity_regimes, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sports_broadcast_exclusivity_regimes, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sports_broadcast_exclusivity_regimes, TR),
    TR >= 0.70.

:- end_tests(sports_broadcast_exclusivity_regimes_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high, reflecting asymmetric benefit flows toward rights holders. The regime genuinely solves a coordination problem (matching content to differentiated demand across regions and subscription tiers), but the solution extracts substantial rents from the excluded audiences and new entrants. Rising from 0.42 to 0.58 over the interval indicates increasing extraction as league owners have learned to auction exclusive rights competitively across multiple streaming bidders, inflating license fees and enforcement costs. Suppression (0.65): Moderately high. The regime suppresses exit through (a) contractual exclusivity enforced in courts, (b) technological geographic blocking (VPNs face enforcement), (c) bundling that raises switching costs, and (d) structural barriers to entry for new platforms. However, suppression is not total — illegal streaming remains accessible, regional free-to-air broadcasts exist in some jurisdictions, and league apps provide limited direct access. Theater ratio (0.48): Moderate. The coordination problem (segmenting demand across timezones and markets) is genuine, but negotiation overhead for exclusive territories has grown as more streaming platforms compete for rights. Much of the theatrical energy goes to maintaining the exclusivity fiction against erosion from streaming normalization.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a stark perspectival gap between beneficiaries and victims. The incumbent broadcaster sees coordination (Rope) — the regime enables them to solve the problem of matching heterogeneous global demand to differentiated pricing. The league owner sees arbitrage (Rope or Rope-adjacent) — exclusive licensing allows them to auction rights competitively and maximize revenue. The cord-cutting viewer sees extraction with no coordination benefit (Snare) — geographic and economic barriers prevent them from accessing preferred content at any reasonable price. The streaming platform sees a barrier-to-entry (Tangled Rope) — forced to bid for expensive exclusive rights while constrained by existing contract lock-ins. The regional sports network sees mixed extraction (Tangled Rope) — they genuinely coordinate local coverage, but face asymmetric extraction from national league owners. The legacy cable system sees its own degraded ritual (Piton) — exclusivity enforcement persists despite cable subscription's technological obsolescence. The civilizational observer risks a false natural law (Mountain) — seeing exclusivity as inherent scarcity, not a contingent institutional choice. The perspectival gaps are structural: they derive from genuinely different positions relative to the extraction and coordination flows.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations produce distinct directionality values through the derivation chain. Incumbent broadcasters (beneficiaries + arbitrage) experience low effective extraction. Cord-cutting viewers (victims + trapped) experience maximum effective extraction. Streaming platforms (victims + constrained) experience moderate extraction. Regional networks (beneficiaries of local coordination + victims of national extraction = mixed status) experience moderate extraction with a structural ambiguity reflected in the omega variable about regional market extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy through perspectival differentiation. The tangled rope classification is NOT 'coordination is happening' — it is 'genuine coordination exists for some agents (rights holders) alongside asymmetric extraction for others (excluded viewers, new platforms).' The regime coordinates the global distribution of sports content (solving the match-demand-to-geography problem). Simultaneously, it extracts from viewers via geographic lock-in and from platforms via licensing barriers. The mandatrophy is resolved by showing that both statements are true from different positions: the constraint IS coordination infrastructure (rope-like) for beneficiaries AND extraction infrastructure (snare-like) for victims. The classification is not 'which is it really?' but 'what is its relational structure?' The rope classification from the beneficiary perspective and the snare classification from the victim perspective are not contradictory — they are the constraint's relational signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exclusivity_contract_enforcement_mechanism,
    'How much of the exclusivity regime''s suppressive force derives from legal contract enforcement versus market concentration and consumer switching costs?',
    'Analysis of contract language, litigation records, and consumer behavior data during contract disputes or expiration windows',
    'If enforcement-dependent: antitrust action can fragment the regime rapidly (high sunset potential). If market-concentration-dependent: even without contracts, switching costs sustain exclusivity (low sunset potential).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exclusivity_contract_enforcement_mechanism, empirical, 'Relative contribution of legal enforcement vs market forces to exclusivity suppression').

omega_variable(
    geographic_demand_heterogeneity_calibration,
    'Does geographic heterogeneity in fan demand genuinely require exclusive licensing for efficient distribution, or does it serve primarily as a price discrimination mechanism?',
    'Historical analysis of pre-exclusivity broadcasting models (free-to-air era); comparison with sports leagues in countries without exclusivity regimes (European football model); simulation of dynamic pricing alternatives',
    'If genuine coordination need: exclusivity captures real efficiency gains (ε should be lower, suppression should reflect coordination cost). If pure price discrimination: exclusivity is rent-seeking, not coordination (ε should be higher, suppression reflects artificial scarcity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_demand_heterogeneity_calibration, conceptual, 'Whether geographic heterogeneity necessitates exclusive licensing or enables price discrimination').

omega_variable(
    streaming_piracy_nexus,
    'Does the high suppression (0.65) measure legitimate enforcement against piracy or suppression of legal viewing alternatives?',
    'Distinction between (a) enforcement against illegal streaming services and (b) contractual blocking of legal competitors and fan apps. Measurement via platform accessibility audits and enforcement action analysis.',
    'If primarily anti-piracy: suppression may be justified enforcement (reclassify toward lower measured suppression for calculation). If primarily anti-competitor: suppression is pure extraction mechanism (sustains high measured suppression). If both: decompose into separate constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(streaming_piracy_nexus, empirical, 'Whether suppression targets piracy or legal competition').

omega_variable(
    streaming_sunset_timeline_realism,
    'How long until streaming normalization and competitive licensing actually fragment the exclusivity regime? Is the scaffold perspective''s sunset realistic or aspirational?',
    'Analysis of contract expiration timelines, cord-cutting rate projections, antitrust proceedings timelines, and emerging platforms'' competitive capacity',
    'If sunset within 10 years: scaffold classification is structural (high confidence). If sunset beyond 30 years or indefinite: scaffold is aspirational; constraint is more snare/piton than scaffold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(streaming_sunset_timeline_realism, empirical, 'Realistic timeline for fragmentation of exclusivity regime').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sports_broadcast_exclusivity_regimes, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sbe_tr_t0, sports_broadcast_exclusivity_regimes, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sbe_tr_t5, sports_broadcast_exclusivity_regimes, theater_ratio, 5, 0.42).
narrative_ontology:measurement(sbe_tr_t10, sports_broadcast_exclusivity_regimes, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(sbe_be_t0, sports_broadcast_exclusivity_regimes, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sbe_be_t5, sports_broadcast_exclusivity_regimes, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(sbe_be_t10, sports_broadcast_exclusivity_regimes, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sports_broadcast_exclusivity_regimes, resource_allocation).
narrative_ontology:affects_constraint(sports_broadcast_exclusivity_regimes, sports_league_revenue_concentration).
narrative_ontology:affects_constraint(sports_broadcast_exclusivity_regimes, geographic_price_discrimination_markets).
narrative_ontology:affects_constraint(sports_broadcast_exclusivity_regimes, streaming_platform_content_acquisition_barriers).

% DUAL FORMULATION NOTE:
% Sports broadcast exclusivity is downstream of league governance structures that choose to auction exclusive rights. The upstream constraint concerns league ownership concentration and revenue distribution models. Separate constraint stories capture (1) the exclusivity regime's extractiveness (this story), (2) the auction mechanism's distributional outcomes (upstream), and (3) emerging streaming platform competitive dynamics (downstream).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sports_broadcast_exclusivity_regimes, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
