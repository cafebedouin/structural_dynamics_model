% ============================================================================
% CONSTRAINT STORY: primary_election_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_primary_election_capture, []).

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
 *   constraint_id: primary_election_capture
 *   human_readable: Primary Election Capture and Partisan Gatekeeping
 *   domain: political/electoral
 *
 * SUMMARY:
 *   Primary election capture describes the structural mechanism by which
 *   major party establishments filter candidate access to primary ballots and
 *   debates, thereby controlling the set of options presented to primary
 *   voters. This constraint manifests across democratic systems but is
 *   particularly pronounced in the U.S. two-party system where primary
 *   winners are effectively guaranteed major general election ballot access.
 *   The tension between the parties' need to select viable nominees and
 *   voters' democratic expectation to influence that selection creates a
 *   coordination problem that parties have increasingly solved through
 *   gatekeeping rather than through inclusive deliberation. The constraint
 *   exhibits mixed characteristics: genuine coordination function (the
 *   apparatus does solve the collective action problem of nominee selection),
 *   significant asymmetric extraction (party leadership captures
 *   disproportionate control over candidate viability and message agenda),
 *   high suppression (voters face multiple barriers to expressing preferences
 *   for excluded candidates), and increasing theater (debates and campaign
 *   events are increasingly stage-managed performances of competition rather
 *   than genuine evaluation).
 *
 * KEY AGENTS:
 *   - Primary voters: Powerless/trapped — cannot exit the binary party choice without forfeiting electoral participation; subjected to ballot access restrictions, media blackouts, and suppressed candidate information
 *   - Insurgent candidates: Powerless/constrained — face formal mobility (can run independent) but at prohibitive cost; ballot access requirements, debate thresholds, funding barriers, and party resource monopoly create high-cost exit
 *   - Primary activist base: Moderate/constrained — benefit from party coordination services but subjected to top-down control of issue agenda, messaging discipline, and candidate hierarchy
 *   - Party establishment: Institutional/arbitrage — controls debate access, endorsements, resource allocation, campaign infrastructure; experiences system as pure coordination with maximum agency
 *   - Democratic primary ritual system: Institutional/arbitrage — maintains legitimation theater through debates, town halls, primary night coverage; persists through institutional inertia despite degraded selection function
 *   - Reform coalition: Organized/mobile — pushes for rule changes (open primaries, ranked-choice voting, ballot access reform, citizen selection); can build parallel mechanisms but faces suppression from incumbent gatekeepers
 *   - Analytical observer: Analytical/analytical — risks naturalizing contingent U.S. electoral-legal arrangements as universal laws of democratic necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(primary_election_capture, 0.58).
domain_priors:suppression_score(primary_election_capture, 0.65).
domain_priors:theater_ratio(primary_election_capture, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(primary_election_capture, extractiveness, 0.58).
narrative_ontology:constraint_metric(primary_election_capture, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(primary_election_capture, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(primary_election_capture, tangled_rope).
narrative_ontology:human_readable(primary_election_capture, "Primary Election Capture and Partisan Gatekeeping").
narrative_ontology:topic_domain(primary_election_capture, "political/electoral").

domain_priors:requires_active_enforcement(primary_election_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(primary_election_capture, major_party_establishments).
narrative_ontology:constraint_beneficiary(primary_election_capture, incumbent_officeholders).
narrative_ontology:constraint_beneficiary(primary_election_capture, party_institutional_machinery).
narrative_ontology:constraint_victim(primary_election_capture, primary_voters).
narrative_ontology:constraint_victim(primary_election_capture, insurgent_candidates).
narrative_ontology:constraint_victim(primary_election_capture, electoral_democratic_accountability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISENFRANCHISED PRIMARY VOTER (SNARE) — The voter has no exit from the binary choice presented by party gatekeeping. Cannot vote for preferred candidate if party apparatus blocks their path. Cannot exit the duopoly without forfeiting electoral agency entirely. Suppression is total: gerrymandering, voter ID restrictions, ballot access barriers, debate thresholds, and media blackouts constrain which options reach the ballot. The voter experiences pure extraction — their preference signal is neutralized before aggregation occurs.
constraint_indexing:constraint_classification(primary_election_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INSURGENT CANDIDATE (SNARE) — Faces massive barriers to ballot access, debate participation, funding, and media coverage. Democratic primary rules (delegate thresholds, petition signature requirements, filing fees, winner-take-all conventions) were designed and are continuously adjusted by party insiders to filter out challenges to leadership. Exit option is formally available (run as independent, switch parties, withdraw) but carries severe penalties: loss of accumulated party resources, loss of brand identification, loss of fundraising apparatus. High-cost exit produces trapped-like behavior even with technical mobility.
constraint_indexing:constraint_classification(primary_election_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PRIMARY ACTIVIST BASE (TANGLED ROPE) — Party insiders provide coordination services (organizing, messaging, resource mobilization) that activists need to influence politics. But the apparatus also constrains what insurgent candidates can communicate, which issues dominate primary discourse, and who can challenge incumbent party leadership. Genuine coordination function with asymmetric extraction: activists benefit from party machinery but their voice is systematically discounted when it diverges from leadership preferences.
constraint_indexing:constraint_classification(primary_election_capture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PARTY ESTABLISHMENT (ROPE) — Experiences the primary system as pure coordination: the apparatus solves the collective action problem of selecting a nominee. Leadership can arbitrage between state primary rules, can choose which challengers to elevate and which to suppress, can manipulate debate participation thresholds, and can extract loyalty through control of endorsements and party resources. Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(primary_election_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: DEMOCRATIC PRIMARY RITUAL (PITON) — The primary system contains substantial theatrical elements: debates perform candidate evaluation but are format-constrained by networks and parties; town halls and campaign appearances are stagy performances; primary night coverage is ritualistic celebration of party unity. The high theater ratio (0.68) reflects that much primary activity is performative legitimation rather than functional selection. The ritual persists through institutional inertia despite degraded function — parties maintain 'competitive primaries' while systematically filtering the competition.
constraint_indexing:constraint_classification(primary_election_capture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: REFORM COALITION (TANGLED ROPE) — Organized actors (good-government groups, election administration reformers, some incumbent challengers) see the primary system as both coordinatively necessary and corruptible. Can organize pressure for rules changes (ranked-choice voting, proportional representation, open primaries, ballot access reform). Mobile exit option: can build parallel nomination mechanisms (citizen assemblies, petition-based ballot access, super-delegates to bypass party control). But reform faces high suppression: incumbent gatekeepers fight rule changes, parties control their own nomination processes, and constitutional federalism distributes power to state legislatures controlled by major parties.
constraint_indexing:constraint_classification(primary_election_capture, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/global perspective, primary capture is naturalized as an immutable feature of democratic governance: parties must select nominees; selection requires gatekeeping; gatekeeping necessarily excludes most candidates; therefore filtering is inherent to democracy itself. This perspective sees the constraint as a law of political nature — unavoidable tension between inclusivity and decisiveness. However, empirical comparison across democracies (open primaries, citizen jury selection, ranked-choice voting in other nations) reveals that alternative mechanisms exist. The mountain classification is a false summit: the 'necessity' of party gatekeeping is contingent on U.S. electoral-legal choices, not universal law.
constraint_indexing:constraint_classification(primary_election_capture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(primary_election_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(primary_election_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(primary_election_capture, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(primary_election_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(primary_election_capture, TR),
    TR >= 0.70.

:- end_tests(primary_election_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increased over time. The constraint extracts from powerless voters and insurgent candidates through ballot access barriers, debate participation thresholds, media coverage concentration, and party resource monopolies. The extractiveness is not maximal because genuine coordination function exists (parties do need to solve nominee selection), and because moderate actors (primary activists) retain some agency within party structures. The trajectory shows increased extraction over 40-year interval (0.35 → 0.58) reflecting professionalization of gatekeeping, increased consultant roles in candidate vetting, declining participation of grassroots candidates, and consolidation of fundraising among establishment-aligned donors. Suppression (0.65): Moderate-high. Structural barriers include ballot access requirements (varying by state, typically 5,000-15,000 petition signatures), debate participation thresholds (polling minimums excluding non-establishment candidates), filing fees, media blackout effects, and two-state primary calendar concentration that favors well-funded frontrunners. Suppression is not total because some insurgent candidates do break through (Obama 2008, Trump 2016), indicating system is not utterly rigid. Theater ratio (0.68): High. Primary debates are heavily formatted by networks and parties; town halls are staged performances; candidate announcements are carefully choreographed; primary night coverage is ritualistic celebration of party unity. The theater has increased over the interval (0.52 → 0.68) as production values increased, debate formats became more constraining, and media coverage became increasingly performative rather than investigative.
 *
 * PERSPECTIVAL GAP:
 *   The primary gap is between the establishment's perception of coordination and the voter's perception of constraint. The establishment sees a system that works (nominees selected, parties unified, general elections competitive). Powerless voters see a system that doesn't work for them (their preferred candidates are excluded, their vote choice is binary, their voice doesn't influence selection). This gap is not due to information asymmetry — both sides perceive the same institutional rules. The gap exists because the institution serves different functions for different agents: genuine coordination for the gatekeeper, suppression for the gated. The reform coalition's perspective is crucial — they see the capture as contingent (tantalized by possibility of change) and treat it as Tangled Rope rather than immutable Snare. This perspectival gap becomes wider as extractiveness increases: as gatekeeping becomes more sophisticated (polling thresholds, media manipulation, donor concentration), the gap widens between those who benefit from precision gatekeeping and those excluded by it.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experience of the constraint is mediated by their power position and exit options. Party establishments with arbitrage exit have low d because they can selectively enforce gatekeeping — they experience the system as a tool they wield rather than a constraint they endure. Powerless voters with trapped exit have high d because gatekeeping is total for them — they can exit only by forfeiting electoral participation. Insurgent candidates with constrained exit (technically can run independent but lose party resources) have moderately high d — they can attempt exit but at severe cost. The primary activist base has moderate d because they benefit from party machinery (coordination services) but are constrained in how they use it (message discipline, hierarchy). The reform coalition with mobile exit has low-moderate d because they can build alternatives (ranked-choice voting, open primary ballot measures) but face suppression from the incumbent system. The directionality computation shows how the same institutional structure produces radically different experienced extractiveness for different agents: the establishment experiences χ ≈ 0.08 (benign coordination), the powerless voter experiences χ ≈ 1.25 (severe extraction), the insurgent experiences χ ≈ 0.92 (high extraction), and the activist experiences χ ≈ 0.65 (mixed coordination-extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY DETECTED AND PARTIALLY RESOLVED: This constraint initially appears to mandate conflicting classifications. At the institutional level, the primary system is necessary for nominee selection (genuine coordination requirement) — suggesting Rope. At the voter level, it functions as pure gatekeeping (suppression without coordination benefit) — suggesting Snare. The mandatrophy is resolved by recognizing that this is genuinely a Tangled Rope: the system provides real coordination services (solving the collective action problem of nominee selection) AND delivers asymmetric extraction (party leadership captures disproportionate control). Both are structural features, not contradictions. The theater element (0.68) indicates some performative degradation — debates and campaigns have become less genuinely selective and more legitimating — which explains why extraction has increased over time without proportional governance failure. The constraint resolves mandatrophy by being honestly hybrid: it is not pure extraction pretending to be coordination (Snare masking as Rope), nor is it pure coordination with minor overhead (Rope). It is genuine coordination with embedded extraction, which is the defining structure of Tangled Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_vs_performative_primary,
    'Does the primary system provide genuine voter influence over nominee selection, or is it primarily a legitimation ritual masking pre-determined outcomes?',
    'Longitudinal analysis of primary outcomes vs party leadership preferences; measurement of correlation between voter preference and final nominee; analysis of cases where insurgent candidates actually win vs are suppressed',
    'If genuine: classification shifts toward Rope (coordination with real agency). If performative: classification shifts toward Snare (pure extraction masked as choice).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(genuine_vs_performative_primary, empirical, 'Whether primaries are genuine preference aggregation or legitimation theater').

omega_variable(
    exit_mobility_for_insurgents,
    'What proportion of insurgent primary candidates who lose within the party apparatus successfully pivot to third-party or independent general election viability?',
    'Historical tracking of primary losers who attempt independent/third-party routes; measurement of vote share, funding, media coverage, and win rates compared to major-party general election candidates',
    'If high pivot success: exit_options should be ''mobile'' rather than ''trapped''; classification becomes less severe Snare or Tangled Rope. If low pivot success: constrained or trapped is accurate; extraction severity confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_mobility_for_insurgents, empirical, 'Actual exit viability for insurgent candidates from major party constraints').

omega_variable(
    suppression_mechanism_type,
    'Is measured suppression primarily structural (rules-based barriers like ballot access requirements, debate thresholds, filing fees) or internalized (voters have internalized party loyalty, self-sorting, epistemic closure that makes them dismiss insurgent options)?',
    'Comparative analysis of rule removal (open primary adoption, lowered ballot-access barriers) and measurement of whether voter behavior changes; exit polling on voter confidence in primary outcomes; analysis of media coverage patterns and voter familiarity with suppressed candidates',
    'If primarily structural: rule reform could reduce suppression substantially. If internalized: suppression persists even after rule change; constraint requires identity-level intervention or generational shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_type, empirical, 'Whether suppression is structural or internalized').

omega_variable(
    party_establishment_capture_mechanism,
    'Do party establishments actively suppress insurgent candidates through deliberate strategy, or does capture emerge from neutral structural incentives that benefit aligned candidates regardless of intent?',
    'Documentary evidence of explicit gatekeeping decisions; analysis of party resource allocation and endorsement patterns; interviews with party officials about decision criteria; comparison of resource flows to challengers vs incumbents',
    'If deliberate strategy: establishment is conscious extractor; enforcement is intentional; reform requires changing leadership incentives. If structural/emergent: no single gatekeeper has unified interest; capture is side effect; reform could target incentive structure rather than personnel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(party_establishment_capture_mechanism, empirical, 'Whether party capture is deliberate strategy or emergent structural phenomenon').

omega_variable(
    democratic_legitimacy_paradox,
    'Does primary capture actually enhance democratic legitimacy (by ensuring general election viability and party cohesion) in ways that offset the loss of primary voter agency?',
    'Measurement of general election participation rates and legitimacy perceptions under closed vs open primary systems; analysis of party cohesion and governance effectiveness as functions of primary openness; cross-national comparison of legitimacy outcomes',
    'If legitimacy enhanced: extraction is genuinely partial (tangled rope is accurate). If legitimacy degraded: extraction is more severe; false coordination cover story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(democratic_legitimacy_paradox, conceptual, 'Whether primary gatekeeping serves legitimate democratic functions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(primary_election_capture, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prim_elec_tr_t0, primary_election_capture, theater_ratio, 0, 0.52).
narrative_ontology:measurement(prim_elec_tr_t20, primary_election_capture, theater_ratio, 20, 0.62).
narrative_ontology:measurement(prim_elec_tr_t40, primary_election_capture, theater_ratio, 40, 0.68).

% Extraction over time
narrative_ontology:measurement(prim_elec_be_t0, primary_election_capture, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(prim_elec_be_t20, primary_election_capture, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(prim_elec_be_t40, primary_election_capture, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(primary_election_capture, enforcement_mechanism).
narrative_ontology:affects_constraint(primary_election_capture, general_election_bipartisan_suppression).
narrative_ontology:affects_constraint(primary_election_capture, political_donor_concentration).
narrative_ontology:affects_constraint(primary_election_capture, media_coverage_gatekeeping).

% DUAL FORMULATION NOTE:
% Primary capture is upstream of general election constraint but represents a distinct structural mechanism. The extractiveness values differ: primary capture focuses on nominee selection gatekeeping (ε=0.58), while general election bipartisan suppression focuses on ballot access barriers that affect all third-party candidates (ε=0.52). Both are electoral constraints but with different beneficiaries and victim sets. Primary capture benefits major party establishments; general election suppression benefits major parties as a duopoly against external challengers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(primary_election_capture, institutional, 0.18).
constraint_indexing:directionality_override(primary_election_capture, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
