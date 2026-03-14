% ============================================================================
% CONSTRAINT STORY: gerrymandering_district_design
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gerrymandering_district_design, []).

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
 *   constraint_id: gerrymandering_district_design
 *   human_readable: Gerrymandering and District Design Constraint
 *   domain: political_system/electoral_mechanics
 *
 * SUMMARY:
 *   Gerrymandering — the deliberate manipulation of electoral district
 *   boundaries to achieve partisan advantage — exhibits the core tension
 *   between coordination and extraction. District-based representation
 *   inherently requires aggregating geographically distributed voters into
 *   collective units; gerrymandering exploits this by manipulating where
 *   those boundaries fall. The constraint solves a genuine coordination
 *   problem (how to convert scattered voter preferences into legislative
 *   outcomes) while simultaneously extracting through asymmetric benefit to
 *   the party controlling redistricting. It is neither pure coordination
 *   (rope) nor pure extraction (snare), but a hybrid (tangled rope) where the
 *   coordination function is inseparable from the asymmetric extraction. The
 *   extractiveness has increased over the measurement interval as
 *   computational mapping techniques have enabled more precise partisan
 *   targeting. Theater ratio has risen as the administrative machinery
 *   (commissions, public hearings, statutory reviews) has elaborated while
 *   actual constraint on partisan optimization has declined — the performance
 *   of neutral redistricting has grown while the function has atrophied. The
 *   constraint demonstrates how all six DR types are legitimate perspectival
 *   readings: gerrymandering appears as an immutable natural consequence of
 *   geographic representation (mountain, falsely), as necessary coordination
 *   for party aggregation (rope, from incumbent perspective), as temporary
 *   problem solvable through commissions (scaffold), as degraded ritual
 *   (piton), as mixed coordination-extraction (tangled rope, from swing voter
 *   perspective), and as pure extraction (snare, from trapped minority voter
 *   perspective).
 *
 * KEY AGENTS:
 *   - Minority Voters: Primary victims (powerless/trapped) — experience vote dilution through packing (supermajority waste) or cracking (sub-threshold dispersal); cannot exit or organize effective remedy
 *   - Competitive District Voters: Secondary victims/beneficiaries (moderate/constrained) — swing voters whose preferences determine elections, but outnumbered by voters in non-competitive districts rendered performative
 *   - Incumbent Party Apparatus: Primary beneficiary (institutional/arbitrage) — controls redistricting process and captures asymmetric legislative advantage; can re-optimize after census
 *   - Opposition Party Out of Power: Constrained victim (powerful/constrained) — faces gerrymandered map but also benefits from same district coordination if they win statewide majority decisively
 *   - Redistricting Reform Advocates: Organized agent (organized/constrained) — court systems, voting rights groups, independent commission coalitions building alternative institutional pathways
 *   - State Election Administration: Institutional maintainer (institutional/arbitrage) — executes redistricting process through legal/administrative machinery increasingly decoupled from actual partisan optimization
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing district-based representation as immutable rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gerrymandering_district_design, 0.58).
domain_priors:suppression_score(gerrymandering_district_design, 0.65).
domain_priors:theater_ratio(gerrymandering_district_design, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gerrymandering_district_design, extractiveness, 0.58).
narrative_ontology:constraint_metric(gerrymandering_district_design, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(gerrymandering_district_design, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gerrymandering_district_design, tangled_rope).
narrative_ontology:human_readable(gerrymandering_district_design, "Gerrymandering and District Design Constraint").
narrative_ontology:topic_domain(gerrymandering_district_design, "political_system/electoral_mechanics").

domain_priors:requires_active_enforcement(gerrymandering_district_design).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gerrymandering_district_design, incumbent_party_apparatus).
narrative_ontology:constraint_beneficiary(gerrymandering_district_design, partisan_cartographers).
narrative_ontology:constraint_victim(gerrymandering_district_design, minority_voters).
narrative_ontology:constraint_victim(gerrymandering_district_design, competitive_districts).
narrative_ontology:constraint_victim(gerrymandering_district_design, electoral_accountability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MINORITY VOTERS (SNARE) — Trapped by district boundaries that either pack them into non-competitive districts (supermajority waste) or crack them across multiple districts where their voting power is diluted below meaningful influence. Cannot exit the constraint; their vote's effective power is structurally diminished. Zero degrees of freedom. Maximum extraction: political voice is appropriated for the benefit of the incumbent party while maintaining formal democratic appearance.
constraint_indexing:constraint_classification(gerrymandering_district_design, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMPETITIVE DISTRICT VOTERS (TANGLED ROPE) — Genuinely compete for influence in a small number of swing districts designed to be marginal. Their votes drive electoral outcomes (coordination function: the system aggregates preferences through competitive districts). However, they are also extracted from — the majority of districts are pre-determined, meaning their individual votes outside competitive zones are rendered performative. Mixed extraction and coordination depending on whether they live in competitive or non-competitive districts.
constraint_indexing:constraint_classification(gerrymandering_district_design, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT PARTY APPARATUS (ROPE) — Direct beneficiary with arbitrage options. The constraint solves the coordination problem: how to aggregate scattered supporters into districts where they constitute voting majorities. The party perceives the constraint as enabling — district design translates voter preferences into reliable legislative seats, creating supermajorities from relatively balanced statewide popular votes. Net beneficiary. Can always re-gerrymander after census if performance declines, so exit options remain viable.
constraint_indexing:constraint_classification(gerrymandering_district_design, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: OPPOSITION PARTY OUT OF POWER (TANGLED ROPE) — Constrained: cannot exit the electoral system but also faces the coordination problem of assembling majorities within the existing district map. If gerrymandered against, they experience high extraction. But they also benefit from the same district-based coordination logic — if they win the statewide popular vote decisively, they can convert that into legislative control despite the incumbent's map advantage. Asymmetric extraction: one side benefits more depending on current partisan alignment.
constraint_indexing:constraint_classification(gerrymandering_district_design, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REDISTRICTING REFORM ADVOCATES (SCAFFOLD) — Organized actors (Common Cause, Fair Districts coalitions, courts) see the constraint as temporary and solvable through: (1) independent redistricting commissions, (2) algorithmic fairness constraints, (3) constitutional amendment. Their perspective frames gerrymandering as a contingent institutional practice vulnerable to reform. They possess agency and a visible exit pathway (sunset via commissions or software), so experienced extraction is lower — the constraint is perceived as changeable within the biographical horizon. Has clear sunset: roughly 10-20 years as commission model spreads and judicial intervention increases.
constraint_indexing:constraint_classification(gerrymandering_district_design, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: STATE ELECTION ADMINISTRATION (PITON) — Institutions responsible for implementing district maps experience the constraint as degraded ritual. The administrative machinery (census, redistricting commissions, precinct assignment) persists through legal requirement and historical precedent, but its function has atrophied as gerrymandering has evolved. The constraint is maintained through institutional inertia — election commissions and secretaries of state manage the process, but the optimization happens outside the administrative apparatus (private consultants, partisan groups). Theater ratio is moderate-high: substantial administrative performance (hearings, public comment, statutory reviews) with limited actual constraint on partisan optimization.
constraint_indexing:constraint_classification(gerrymandering_district_design, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / IMPLICIT MAJORITARIAN TRAP (MOUNTAIN) — From a civilizational view, district-based representation may appear as an immutable structural requirement: to aggregate geographically distributed populations into collective decision units, some form of spatial clustering is necessary. Geographic representation creates an inherent tension between proportionality and representation. This perspective risks naturalizing the contingent choice of district-based apportionment (rather than proportional or other systems) as a natural law of federalism. However, the structural data reveals this as false summit: the choice of district-based representation is institutional, not natural — other democracies use proportional or mixed systems with lower gerrymandering extractiveness.
constraint_indexing:constraint_classification(gerrymandering_district_design, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gerrymandering_district_design_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gerrymandering_district_design, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gerrymandering_district_design, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gerrymandering_district_design, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gerrymandering_district_design, TR),
    TR >= 0.70.

:- end_tests(gerrymandering_district_design_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high and rising. Gerrymandering extracts political voice from disfavored voters through asymmetric district design, converting relatively balanced statewide popular votes into lopsided legislative majorities. The extraction has increased over the interval as mapping technology has enabled more precise partisan targeting — early redistricting (t=0) was cruder, allowing more unintended competitive districts; modern redistricting (t=6) uses precinct-level microtargeting to optimize partisan advantage. Suppression (0.65): Moderately high. Minority voters face substantial barriers to remedying their diluted vote: changing residency is expensive, litigation is lengthy and uncertain, ballot initiatives for commissions face partisan opposition, federal Voting Rights Act protections have been weakened by recent Court decisions. Theater ratio (0.48): Moderate. The administrative machinery of redistricting (census, hearings, public comment, statutory reviews) creates appearance of neutral technical process, but the actual optimization happens through proprietary mapping software controlled by partisan actors. The growth from 0.32 to 0.48 reflects increasing disconnect between administrative ritual and partisan outcome optimization.
 *
 * PERSPECTIVAL GAP:
 *   The constraint's perspectival gap reveals how the same structural mechanism (district boundary design) produces fundamentally different classifications. The incumbent party sees rope — coordination that enables representation. The opposition sees tangled rope — mixed coordination and extraction depending on whether the map favors them. Minority voters see snare — pure extraction with no escape route. Reform advocates see scaffold — temporary extractive practice vulnerable to sunset through commissions and algorithms. The electoral administration sees piton — a degraded ritual maintained through procedural precedent despite atrophied coordination function. The analytical observer risks seeing mountain — naturalizing geographic representation's inherent geometry as immutable. The perspectival range (snare to rope to mountain) for identical structural data demonstrates that classification is indexical: it depends on the observer's structural position and available exits. No single type is 'the' answer; the presheaf over observation positions is.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim structure drives directionality computation. Incumbent parties are declared beneficiaries — they capture the legislative advantage. Minority voters and non-competitive general voters are declared victims — their votes are rendered sub-threshold or supermajority-waste. Electoral accountability (the principle that legislative outcome should reflect voter preference distribution) is a victim — gerrymandering breaks this feedback. This structure produces high d for victims (trapped exit ≈ 0.95), low d for beneficiaries (arbitrage exit ≈ 0.15), and intermediate d for constrained actors (opposition parties ≈ 0.55). The sigmoid f(d) translates these to experienced extractiveness values consistent with the perspectival classifications. No directionality overrides are required — the derivation chain produces coherent assignments.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing how multiple classifications coexist as legitimate perspectival readings of the same base properties. The snare classification (from minority voter perspective) is not 'wrong' because the tangled rope or scaffold classifications also apply from other perspectives. Rather, the framework explains why these coexist: gerrymandering genuinely solves a coordination problem (representing geographically distributed populations) while simultaneously enabling asymmetric extraction (concentrating power in the gerrymandering party). Both functions are real. The snare perspective emphasizes the extraction; the rope perspective emphasizes the coordination. The mandate is not to choose one, but to understand the constraint as a hybrid that cannot be decomposed into pure coordination or pure extraction without losing structural accuracy. The false summit risk (mountain classification) is flagged by noting that other electoral systems (proportional representation, mixed systems) produce lower extractiveness while solving similar coordination problems. Geographic representation is institutional choice, not natural law. The scaffold classification is empirically grounded: independent commissions and algorithmic fairness constraints are demonstrably reducing partisan advantage in some states, giving real sunset structure to the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_threshold,
    'What deviation from proportional representation (state popular vote to legislative seat share) constitutes extractive gerrymandering vs. natural variance from district-based representation?',
    'Comparative analysis: seat-vote curves across states with different redistricting regimes; cross-national comparison with proportional systems; statistical simulation of random district assignments',
    'If threshold < 5%: many legitimately compact districts misclassified as extraction. If threshold > 15%: significant partisan asymmetry passes unchallenged as ''natural'' variance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_threshold, empirical, 'Threshold for distinguishing extraction from natural variance in district-based representation').

omega_variable(
    compactness_vs_optimization_tradeoff,
    'Can computationally optimal (minimal partisan asymmetry) district maps be visually compact, or does minimizing partisan advantage require sacrificing traditional compactness criteria?',
    'Algorithmic generation of minimally partisan maps; evaluation against historical compactness metrics; judicial precedent analysis of compactness as antidote to gerrymandering',
    'If achievable: compactness can serve as constraint on partisan optimization (rope coordination tool). If not achievable: compactness is theater masking partisan intent (piton degradation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compactness_vs_optimization_tradeoff, empirical, 'Whether compactness and partisan fairness are compatible optimization targets').

omega_variable(
    minority_vote_dilution_mechanism,
    'Does vote dilution of minority voters in packed/cracked districts occur primarily through intentional partisan targeting or as inevitable byproduct of district-based systems with geographic segregation?',
    'Comparative mapping: districts drawn with and without partisan data; analysis of pre- vs post-Voting Rights Act minority representation; cross-national comparison of minority representation under different electoral systems',
    'If primarily intentional: gerrymandering is malicious extraction (snare). If primarily structural: minority representation problem persists even under ''neutral'' redistricting (tangled rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_vote_dilution_mechanism, empirical, 'Whether minority vote dilution is intentional targeting or structural byproduct').

omega_variable(
    commission_independence_achievability,
    'Can independent redistricting commissions achieve genuine partisan neutrality, or do they inevitably reproduce partisan outcomes through seemingly neutral criteria (compactness, communities of interest)?',
    'Comparative analysis of commission-drawn maps vs partisan-drawn maps for seat-vote curves; historical tracking of commission performance across states; mechanism analysis of how ''neutral'' criteria encode partisan preference',
    'If achievable: scaffold sunset is structurally sound — commissions represent genuine exit from extraction. If not achievable: scaffold is theater masking persistent partisan control through procedural neutrality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commission_independence_achievability, empirical, 'Whether independent commissions can achieve genuine partisan neutrality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gerrymandering_district_design, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gerrymander_tr_t0, gerrymandering_district_design, theater_ratio, 0, 0.32).
narrative_ontology:measurement(gerrymander_tr_t2, gerrymandering_district_design, theater_ratio, 2, 0.38).
narrative_ontology:measurement(gerrymander_tr_t4, gerrymandering_district_design, theater_ratio, 4, 0.44).
narrative_ontology:measurement(gerrymander_tr_t6, gerrymandering_district_design, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(gerrymander_be_t0, gerrymandering_district_design, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gerrymander_be_t2, gerrymandering_district_design, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(gerrymander_be_t4, gerrymandering_district_design, base_extractiveness, 4, 0.51).
narrative_ontology:measurement(gerrymander_be_t6, gerrymandering_district_design, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gerrymandering_district_design, resource_allocation).
narrative_ontology:affects_constraint(gerrymandering_district_design, campaign_finance_asymmetry).
narrative_ontology:affects_constraint(gerrymandering_district_design, voter_suppression_targeting).
narrative_ontology:affects_constraint(gerrymandering_district_design, minority_representation_gap).

% DUAL FORMULATION NOTE:
% Gerrymandering is downstream of the structural choice to use district-based representation rather than proportional or mixed systems. The upstream constraint is the electoral system choice itself; gerrymandering is the extractive exploitation of that system's geometry. The campaign finance constraint shares beneficiaries (incumbent party apparatus) and is causally linked: gerrymandering reduces competitive districts, reducing the cost of maintaining power. The voter suppression targeting constraint can be deployed selectively against minorities in packed/cracked districts. The minority representation gap is a direct outcome of gerrymandering.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
