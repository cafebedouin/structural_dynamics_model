% ============================================================================
% CONSTRAINT STORY: us_two_party_duopoly
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_two_party_duopoly, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_two_party_duopoly
 *   human_readable: The U.S. Two-Party Political Duopoly
 *   domain: political/electoral
 *
 * SUMMARY:
 *   The U.S. two-party duopoly is a structural constraint on electoral
 *   competition and ideological representation that arises from the
 *   interaction of single-seat-district plurality voting (SSDP) and the
 *   spoiler effect. Candidates and voters with preferences outside the narrow
 *   liberal-conservative axis find themselves mathematically trapped: voting
 *   for an ideologically aligned candidate guarantees that candidate's defeat
 *   (vote splitting) and returns the nomination to a major party. This
 *   constraint simultaneously provides substantial coordination benefits—two
 *   major parties efficiently aggregate diverse coalitions, reduce voter
 *   decision complexity, and structure legislative cooperation—while
 *   extracting from ideological minorities through systematic exclusion. The
 *   two major parties actively maintain the constraint through ballot access
 *   rules, debate exclusion criteria, and strategic messaging against spoiler
 *   candidates. From different structural positions, the duopoly appears as a
 *   natural law (to those born into it), a coordination mechanism (to major
 *   party leadership), pure extraction (to third-party candidates), and a
 *   temporary problem with a sunset clause (to ranked-choice voting
 *   reformers). The constraint's extractiveness has risen over the interval
 *   as ideological polarization has increased turnout sensitivity to spoiler
 *   dynamics, and the theater ratio has risen as parties invest more in
 *   messaging against third-party 'spoilers' rather than engaging
 *   substantively with alternative platforms.
 *
 * KEY AGENTS:
 *   - Third-Party Candidates: Primary victims (powerless/trapped) — face ballot access barriers, spoiler mathematics, debate exclusion, and voter strategic defection
 *   - Ideological Minority Voters: Primary victims (moderate/trapped) — structural forced choice between compromise major-party candidate and strategic waste vote
 *   - Democratic Party Apparatus: Primary beneficiary (powerful/constrained) — guaranteed ballot access, debate inclusion, geographic viability; constrained by two-party competition and internal coalition management
 *   - Republican Party Apparatus: Primary beneficiary (powerful/constrained) — symmetric benefits and constraints to Democratic apparatus
 *   - Major Party Donors: Secondary beneficiary (institutional/arbitrage) — duopoly provides predictable counterparties and spoiler-risk elimination; exit (funding a competitive third party) is theoretically available but practically impossible
 *   - Electoral Institutions: Piton performer (institutional/arbitrage) — maintain ballot access rules, debate commissions, and voter registration systems that operationalize the duopoly; primary function (vote counting) persists, but duopoly maintenance is substantially performative
 *   - Ranked-Choice Voting Coalition: Organized reformer (organized/mobile) — arXiv of electoral reform; see the duopoly as a temporary institutional failure with a sunset clause provided by RCV system adoption
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_two_party_duopoly, 0.58).
domain_priors:suppression_score(us_two_party_duopoly, 0.72).
domain_priors:theater_ratio(us_two_party_duopoly, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_two_party_duopoly, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_two_party_duopoly, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_two_party_duopoly, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_two_party_duopoly, tangled_rope).
narrative_ontology:human_readable(us_two_party_duopoly, "The U.S. Two-Party Political Duopoly").
narrative_ontology:topic_domain(us_two_party_duopoly, "political/electoral").

domain_priors:requires_active_enforcement(us_two_party_duopoly).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_two_party_duopoly, democratic_party_apparatus).
narrative_ontology:constraint_beneficiary(us_two_party_duopoly, republican_party_apparatus).
narrative_ontology:constraint_beneficiary(us_two_party_duopoly, major_party_donors).
narrative_ontology:constraint_beneficiary(us_two_party_duopoly, incumbent_legislators).
narrative_ontology:constraint_victim(us_two_party_duopoly, third_party_movements).
narrative_ontology:constraint_victim(us_two_party_duopoly, independent_candidates).
narrative_ontology:constraint_victim(us_two_party_duopoly, ideological_minorities).
narrative_ontology:constraint_victim(us_two_party_duopoly, electoral_competition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THIRD-PARTY CANDIDATE (SNARE) — A viable candidate from outside the two major parties faces ballot access barriers (signature collection, filing fees), mathematical spoiler dynamics (vote splitting), debate exclusion (polling thresholds), and voter strategic defection (lesser-evil voting). Exit from the constraint (running) carries career destruction risk. No institutional support exists. Maximum experienced extraction.
constraint_indexing:constraint_classification(us_two_party_duopoly, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: IDEOLOGICAL MINORITY VOTERS (SNARE) — Voters whose preferences align with neither major party (libertarians, greens, progressives beyond Democratic mainstream, paleo-conservatives beyond Republican mainstream) face a structural extraction: they vote for a major-party compromise candidate who does not represent their values, or they 'waste' their vote on a non-viable option that cannot win. The spoiler dynamic mathematically locks them into the Duopoly for generations. No exit that doesn't carry strategic cost.
constraint_indexing:constraint_classification(us_two_party_duopoly, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: MAJOR PARTY LEADERSHIP (TANGLED ROPE) — Democratic and Republican leadership benefit enormously from the Duopoly structure (guaranteed access to ballot, debate stages, media attention, two-state duopoly of electoral viability). But they are also constrained by it: they must appeal to a geographic majority coalition, manage internal ideological factions, and compete predictably within the two-dimensional liberal-conservative axis. They coordinate with donors and activists (rope function) while extracting from third parties and minorities (snare function). Active enforcement required: ballot access rules, debate exclusion criteria, strategic messaging against spoilers.
constraint_indexing:constraint_classification(us_two_party_duopoly, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MAJOR PARTY DONORS (ROPE) — Institutional donors (corporations, super-PACs, wealthy individuals) benefit from duopoly certainty: two predictable counterparties, no third-party spoiler risk, guaranteed access to whoever wins. They experience the constraint as coordination—it provides a stable arena for political investment with known rules. Exit option (funding a third party that could actually win) carries catastrophic risk (both incumbents punish the defector), so exit is theoretically available but practically arbitrage (switching allegiance between parties based on returns). Net beneficiary with stable rules—coordination with a hidden extraction asymmetry favoring concentrated capital.
constraint_indexing:constraint_classification(us_two_party_duopoly, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ELECTORAL INSTITUTIONS (PITON) — State and federal election administration agencies maintain ballot access rules, debate commission authority (nominally nonpartisan but controlled by the two parties), and voter registration systems that operationalize the duopoly. The functional role (coordinate vote counting, prevent fraud) persists, but the Duopoly maintenance is substantially performative—debate commission thresholds shift based on which party benefits, state legislatures (controlled by one of the two parties) set ballot access rules that favor incumbents and disadvantage new entrants. Theater ratio high: the institutions perform neutrality while operationalizing duopoly preservation.
constraint_indexing:constraint_classification(us_two_party_duopoly, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: RANKED-CHOICE VOTING COALITION (SCAFFOLD) — Organized reformers (voting rights NGOs, Maine, Alaska ballot initiatives, local RCV experiments) see the duopoly as a temporary institutional failure with a sunset clause. RCV systems eliminate the spoiler dynamic, releasing trapped ideological minorities. These coalitions have agency (ballot initiatives win, legislatures pass RCV bills) and a clear exit path (system redesign that mathematically eliminates vote splitting). Enforcement requirements decline as systems migrate. Theater in RCV activism remains high (voters must be educated), but the fundamental mechanism change removes the duopoly's extraction function.
constraint_indexing:constraint_classification(us_two_party_duopoly, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: COMPARATIVE POLITICAL ANALYST (TANGLED ROPE) — From a civilizational and global perspective, two-party systems are neither natural nor inevitable. Proportional representation, multi-party systems, and hybrid electoral designs function in democracies worldwide. The U.S. duopoly is a specific institutional artifact (single-seat-district plurality voting) not a law of nature. The system simultaneously enables large-scale democratic coordination (two parties aggregate diverse coalitions) and extracts from ideological minorities through spoiler dynamics and vote-splitting. The constraint is active, enforced, and benefits a specific class (major party apparatuses) at clear cost to others. Classification as tangled rope reflects both genuine coordination function and asymmetric extraction.
constraint_indexing:constraint_classification(us_two_party_duopoly, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_two_party_duopoly_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_two_party_duopoly, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_two_party_duopoly, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_two_party_duopoly, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_two_party_duopoly, TR),
    TR >= 0.70.

:- end_tests(us_two_party_duopoly_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The duopoly extracts from third parties (ballot denial) and ideological minorities (vote splitting), but the extraction is not total—voters can vote for non-viable candidates, candidates can run despite spoiler odds, and ideological minorities can form movements within major parties. The extraction value of 0.58 reflects substantial but not maximal coercion. The value has risen from 0.42 to 0.58 over the 50-year interval as ideological polarization has increased the cost of supporting a non-preferred major-party candidate (moving voters from strategic compromise to bitter strategic voting). Suppression (0.72): High. Ballot access requirements (signature collection, filing fees, state-specific rules), debate exclusion (polling thresholds controlled by the two parties), media marginalization (third parties receive <5% of coverage), and voter strategic defection (Duverger's law: plurality voting produces two-party equilibrium) collectively create severe barriers to third-party competition. The barriers are not absolute—some third-party candidates run and gather votes—but they are sufficient to prevent any third party from achieving electoral viability. Theater ratio (0.65): Moderate-high. Both major parties invest substantial effort in messaging against spoiler candidates ('wasted vote' narratives, strategic voting arguments) and in defending the debate commission thresholds that exclude third parties. This theater performs the maintenance of the duopoly. The underlying function (vote counting, aggregating coalition preferences) persists, but the function is increasingly inseparable from the theater of duopoly maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The widest gap exists between the beneficiary (major party apparatus) and victim (third-party candidate) perspectives. The beneficiary sees rope (coordination mechanism enabling stable governance), while the victim sees snare (mathematical trap with no exit). The organized reformer sees scaffold (temporary structure being dismantled by RCV adoption), while the piton perspective sees the institutions as degraded and performative. The comparative analyst sees the full structure as tangled rope—genuine coordination function paired with systematic extraction. This gap reflects the fundamental asymmetry: the duopoly genuinely coordinates electoral preferences into a two-dimensional policy space (coordination function), but it achieves this by excluding ideological space outside that two-dimensional axis (extraction function). Different agents experience different balances of the coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries of the duopoly (Democratic and Republican apparatuses, major party donors) experience low effective extraction (d ≈ 0.2-0.4) because they benefit from the constraint. They see it as coordination (rope function). Victims (third-party candidates, ideological minority voters) experience high effective extraction (d ≈ 0.85-0.95) because they bear the cost of spoiler dynamics and ballot access barriers, with no exit. Organized reformers (ranked-choice voting coalition) experience lower effective extraction (d ≈ 0.4-0.5) because they have agency and see a clear exit path (system redesign). The piton classification derives from high theater ratio (0.65) combined with degraded function: the institutions maintaining the duopoly perform neutrality while operationalizing partisan advantage. The comparative analyst sees the constraint as active and enforced (tangled rope), neither natural nor inevitable, reflecting both genuine coordination (two-party aggregation) and asymmetric extraction (spoiler dynamics).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the duopoly is neither pure coordination (rope) nor pure extraction (snare). It is tangled rope: a genuine coordination mechanism (two-party aggregation reduces voter decision complexity, structures legislative cooperation) that generates asymmetric extraction (ideological minorities trapped by vote-splitting). The major parties benefit from both the coordination (efficient coalition building) and the extraction (spoiler suppression). Third-party actors benefit from neither. The constraint requires active enforcement (ballot access rules, debate exclusion) precisely because it is not a natural law or mathematical inevitability—other democracies use proportional representation and multi-party systems. The theater ratio (0.65) reflects that much of the enforcement is performative: debate commission thresholds shift with partisan advantage, ballot access rules are selectively enforced, and messaging against spoilers emphasizes strategic voting rather than engaging third-party platforms substantively. The mandatrophy is dissolved by showing that all six types are legitimate perspectival readings: mountain (to those naturalizing the duopoly as inherent to democracy), rope (to beneficiaries), tangled rope (to analytical observers), snare (to victims), scaffold (to reformers), and piton (to degraded institutions maintaining the system through inertia). The system is NOT a mountain—it is an active, enforced, contingent institutional artifact that depends on specific electoral mechanics (SSDP) and party strategy. Ranked-choice voting provides a concrete path to dismantling the constraint, confirming that it is not a natural law but a contingent extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    spoiler_dynamics_threshold,
    'At what ideological distance from the major parties does the spoiler dynamic become mathematically inevitable (vote splitting guarantees defeat)?',
    'Historical election data: correlation of third-party vote share with major-party defeat margins; counterfactual analysis of 2000 (Nader), 2016 (Johnson/Stein), and 2020 elections; polling on ideological distance of third-party supporters from nearest major party',
    'If threshold is low (voters <0.3 on left-right scale from nearest major party): spoiler dynamics affect most ideological variation, making the duopoly extraction severe. If threshold is high (voters >0.6 on scale): spoiler dynamics affect only extreme fringes, reducing duopoly extraction severity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(spoiler_dynamics_threshold, empirical, 'Ideological distance threshold triggering spoiler dynamics').

omega_variable(
    ranked_choice_adoption_rate,
    'Will ranked-choice voting adoption become irreversible (cascade to majority of states/federal level) or stall at local/state experiments?',
    'Longitudinal tracking of RCV ballot initiatives and legislation; analysis of states/municipalities that adopt RCV vs those that revert or block adoption; measurement of duopoly apparatus opposition intensity as RCV threatens core extraction mechanism',
    'If adoption is irreversible: scaffold sunset is real and imminent (10-20 year timeline). If stalled: duopoly maintenance persists indefinitely, tangled-rope classification accurate long-term.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ranked_choice_adoption_rate, empirical, 'Whether RCV adoption becomes irreversible or stalls').

omega_variable(
    duopoly_enforcement_mechanism,
    'Is duopoly maintenance primarily structural (mathematical consequence of plurality voting) or active (deliberate party strategy through debate exclusion, ballot access rules, messaging)?',
    'Legal analysis of ballot access rule origins; historical document review of debate commission founding and threshold evolution; comparison of enforcement intensity across states with different ballot access rules; counterfactual: would spoiler dynamics persist under identical ballot access rules but RCV voting?',
    'If primarily structural: duopoly requires institutional change (voting system redesign) to break; party strategy is secondary. If primarily active: targeted legal challenges to ballot access rules and debate exclusion could partially restore competitive space without voting system change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(duopoly_enforcement_mechanism, empirical, 'Whether duopoly is structurally inevitable or strategically maintained').

omega_variable(
    major_party_prisoner_dilemma,
    'Do the two major parties view the duopoly as mutually beneficial coordination, or are they locked in a prisoner''s dilemma where each would benefit from defecting (adopting RCV unilaterally) but neither does because the other would be destroyed?',
    'Analysis of internal party documents on voting system reform; interviews with party leadership on RCV adoption; game-theoretic analysis of payoff matrices for unilateral and mutual adoption scenarios; historical cases where a party considered RCV adoption',
    'If mutual benefit: the constraint is primarily a rope (coordination mechanism benefiting both parties equally). If prisoner''s dilemma: the constraint includes an extractive component where the two parties collectively extract from ideological minorities, but neither party individually wants to break the mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(major_party_prisoner_dilemma, preference, 'Whether parties view duopoly as coordination or prisoner''s dilemma').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_two_party_duopoly, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duop_tr_t0, us_two_party_duopoly, theater_ratio, 0, 0.48).
narrative_ontology:measurement(duop_tr_t25, us_two_party_duopoly, theater_ratio, 25, 0.58).
narrative_ontology:measurement(duop_tr_t50, us_two_party_duopoly, theater_ratio, 50, 0.65).

% Extraction over time
narrative_ontology:measurement(duop_be_t0, us_two_party_duopoly, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(duop_be_t25, us_two_party_duopoly, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(duop_be_t50, us_two_party_duopoly, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_two_party_duopoly, enforcement_mechanism).
narrative_ontology:affects_constraint(us_two_party_duopoly, campaign_finance_concentration).
narrative_ontology:affects_constraint(us_two_party_duopoly, gerrymandering_entrenchment).
narrative_ontology:affects_constraint(us_two_party_duopoly, primary_election_capture).

% DUAL FORMULATION NOTE:
% The two-party duopoly is structurally upstream of campaign finance concentration (two parties monopolize institutional donor access), gerrymandering entrenchment (two parties use redistricting to lock in electoral advantage), and primary election capture (two parties control the mechanism for selecting candidates). All three downstream constraints depend on the duopoly's structural stability—they could not persist if third parties and independents had meaningful electoral viability. The duopoly is thus a keystone constraint in the political extraction ecosystem.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
