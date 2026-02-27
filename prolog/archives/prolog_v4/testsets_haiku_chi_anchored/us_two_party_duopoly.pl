% ============================================================================
% CONSTRAINT STORY: us_two_party_duopoly
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
    constraint_indexing:directionality_override/3,
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
 *   The U.S. two-party political duopoly is a hybrid coordination-extraction
 *   mechanism that has accumulated enforcement layers over seven decades.
 *   Structurally, it began as an incidental consequence of
 *   first-past-the-post (FPTP) electoral rules and single-member districts —
 *   a genuine coordination mechanism that simplified voter decision-making
 *   and reduced transaction costs for coalition formation. However, over the
 *   interval 1950-2024, beneficiaries have actively reinforced the duopoly
 *   through ballot access laws, debate commission membership rules,
 *   gerrymandering, voter registration purges, and litigation (Citizens
 *   United). The constraint now exhibits suppression (0.72) and theater
 *   (0.68) characteristic of tangled-rope dynamics: it still provides
 *   coordination benefits to major party leadership (who experience it as
 *   pure rope), but simultaneously extracts from third-party candidates and
 *   from voters in non-competitive gerrymandered seats. The perspectival gap
 *   is acute: the third-party candidate sees a snare; the organized reform
 *   coalition sees both coordination and extraction; the major party sees
 *   only coordination; the electoral administration system maintains
 *   performative rules defending the structure. The analytical observer risks
 *   naturalizing this as Duverger's mathematical inevitability, but the
 *   active enforcement mechanisms (ballot access rules, debate thresholds,
 *   gerrymandering statutes) reveal it as a political choice, not a law of
 *   mathematics.
 *
 * KEY AGENTS:
 *   - Major Party Leadership (Democratic and Republican): Primary beneficiary (institutional/arbitrage) — benefits from guaranteed mobilization certainty, simplified swing-voter targeting, and institutional incumbency protection.
 *   - Third-Party Candidates (Green, Libertarian, independents): Primary victim (powerless/trapped) — locked out by ballot access requirements (50-state petition gathering), 15% debate thresholds, and spoiler effect narrative.
 *   - Swing Voters in Gerrymandered Districts: Secondary victim (moderate/constrained) — face binary choice within predetermined outcome; 92% of House seats are non-competitive, stripping genuine electoral choice.
 *   - Organized Reform Coalition (Represent.US, Common Cause, FairVote): Organized secondary actor (organized/constrained) — seeks ballot access reform, ranked-choice voting, and campaign finance reform; constrained by need to defeat incumbent beneficiaries.
 *   - Super PAC Networks and Dark Money: Powerful secondary actor (powerful/mobile) — benefit from duopoly simplicity (binary choice maximizes ad targeting efficiency) but face extraction via Citizens United financing dependency.
 *   - Electoral Administration System (state election officials, federal Election Assistance Commission, debate commission): Institutional maintainer (institutional/arbitrage) — enforces ballot access rules, debate thresholds, and election law; increasingly performative in defending rule structure rather than enabling competition.
 *   - Swing Voter in Competitive Districts: Mixed-position agent (moderate/mobile) — unusually valuable to major parties due to binary scarcity; experiences moderate extraction through targeted advertising and voter suppression tactics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_two_party_duopoly, 0.58).
domain_priors:suppression_score(us_two_party_duopoly, 0.72).
domain_priors:theater_ratio(us_two_party_duopoly, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_two_party_duopoly, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_two_party_duopoly, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_two_party_duopoly, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_two_party_duopoly, tangled_rope).
narrative_ontology:human_readable(us_two_party_duopoly, "The U.S. Two-Party Political Duopoly").
narrative_ontology:topic_domain(us_two_party_duopoly, "political/electoral").

domain_priors:requires_active_enforcement(us_two_party_duopoly).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_two_party_duopoly, major_party_leadership).
narrative_ontology:constraint_beneficiary(us_two_party_duopoly, duopoly_insiders).
narrative_ontology:constraint_beneficiary(us_two_party_duopoly, gerrymandered_district_holders).
narrative_ontology:constraint_victim(us_two_party_duopoly, third_party_candidates).
narrative_ontology:constraint_victim(us_two_party_duopoly, swing_voter_suppression).
narrative_ontology:constraint_victim(us_two_party_duopoly, electoral_choice_diversity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THIRD-PARTY CANDIDATE (SNARE) — Structurally locked out by ballot access requirements, debate thresholds (15% polling), and spoiler dynamics. Winner-take-all electoral math creates a trap: third-party votes are perceived as extractive from the preferred major party, generating hostile pressure. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.80. This is pure extraction with no coordination benefit to the trapped agent.
constraint_indexing:constraint_classification(us_two_party_duopoly, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SWING VOTER IN NON-COMPETITIVE SEATS (SNARE) — In gerrymandered districts (92% of House seats are non-competitive), voters face a binary choice within a predetermined outcome. Gerrymandering + duopoly creates irreversible structural constraint: even mobilizing swing voters cannot change the seat. d≈0.88, f(d)≈1.25, σ=1.0 → χ≈0.73. Suppression is enforcement mechanism (voter roll purges, ID requirements) combined with structural inevitability.
constraint_indexing:constraint_classification(us_two_party_duopoly, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ORGANIZED REFORM COALITION (TANGLED ROPE) — Ranked-choice voting advocates, campaign finance reformers, and ballot-access activists see both coordination and extraction. The duopoly provides: predictable legislative coalitions (low transaction costs for organizing). It extracts: suppresses electoral diversity, locks in winner-take-all rules, and raises barriers to entry. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.43. Constrained exit because reform requires defeating incumbents who benefit from the duopoly.
constraint_indexing:constraint_classification(us_two_party_duopoly, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MAJOR PARTY LEADERSHIP (ROPE) — Experiences the duopoly as pure coordination: binary choice for voters ensures high mobilization certainty, simplifies coalition-building, and guarantees that swing voters in competitive districts are extraordinarily valuable. Party leadership has exit option via defection to a third party (arbitrage), but would lose institutional resources immediately. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.006. Net beneficiary through structural coordination of voter attention.
constraint_indexing:constraint_classification(us_two_party_duopoly, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ELECTORAL ADMINISTRATION SYSTEM (PITON) — Ballot access rules, debate commission membership, and election law interpretation have become substantially performative. Originally justified as preventing spoiler effects and ensuring serious candidates, these rules now primarily protect duopoly incumbents. theater_ratio=0.68 indicates that much administrative effort is devoted to defending the rule structure rather than enabling competition. The system persists through institutional inertia and legal precedent (Bush v. Gore, 2000 formalized the two-party assumption in recount frameworks).
constraint_indexing:constraint_classification(us_two_party_duopoly, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: WELL-FUNDED OUTSIDE GROUP (TANGLED ROPE) — Super PACs and dark money networks benefit from duopoly predictability (binary choice for voters increases ad ROI) but also face extraction: Citizens United (2010) created the duopoly financing structure, which extracts coordination overhead. These groups can theoretically defect to a third-party infrastructure (mobile exit), but the ecosystem is locked in. d≈0.48, f(d)≈0.62, σ=1.0 → χ≈0.36. Both coordination benefit (binary simplicity) and moderate extraction (dependency on rule structure).
constraint_indexing:constraint_classification(us_two_party_duopoly, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE SUMMIT CANDIDATE) — From a civilizational view, might argue that two-party systems are inevitable outcomes of Duverger's Law (first-past-the-post + single-member districts mathematically reduce to two parties). ε=0.58, suppression=0.72, theater=0.68 directly contradict a mountain classification. This is a false summit: Duverger's Law describes a mathematical tendency, not an enforcement mechanism. Many democracies have multi-party systems with FPTP (Canada, India). The duopoly is actively maintained through ballot access law, debate commission rules, and gerrymandering — not mathematically inevitable.
constraint_indexing:constraint_classification(us_two_party_duopoly, mountain,
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
    constraint_indexing:constraint_classification(us_two_party_duopoly, TypeOther, context(agent_power(organized), _, _, _)),
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
 *   Extractiveness (0.58): High-moderate. The duopoly extracts through ballot access barriers, debate exclusions, and strategic voter suppression, but extraction is incomplete — candidates can still organize outside parties and voters can still vote third-party (at cost of spoiler guilt and strategic inefficiency). The trajectory shows increasing extractiveness from 0.35 (1950, when FPTP was incidental consequence) to 0.58 (2024, when it is actively maintained). Suppression (0.72): High. Multiple enforcement mechanisms: (1) Ballot access petitions requiring 50-state signature gathering ($500K-$1M per candidate); (2) Debate commission threshold of 15% polling (set to exclude third parties, created after 1992 Perot candidacy); (3) Gerrymandering (92% of House seats non-competitive as of 2024); (4) Voter roll purges and ID requirements disproportionately affecting swing voters; (5) Spoiler narrative generating social pressure against third-party voting. Theater ratio (0.68): Moderate-high. Debate commission and ballot access rules are partially performative: framed as preventing chaos or ensuring 'serious' candidates, they primarily protect duopoly incumbents. Comparative data: Canada uses FPTP but has three major parties; India uses FPTP but has 40+ parties. The U.S. duopoly requires active enforcement, not passive math. Theater has increased as rule-defending rhetoric intensifies (2016-2020: spoiler narrative, election interference narratives, debate commission defensiveness).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a sharp perspectival gap between beneficiaries and victims. The major party leadership (institutional/arbitrage) sees the duopoly as pure coordination: it simplifies voter decisions, ensures mobilization certainty, and provides a stable legislative environment. The third-party candidate (powerless/trapped) sees a snare: systematically locked out by rules that are presented as mathematical necessity but are actually enforced by statute. The reform coalition (organized/constrained) sees tangled rope: yes, binary choices simplify coordination, but at the cost of suppressing electoral diversity and locking in the beneficiaries. The swing voter in non-competitive seats sees a snare (binary choice within determined outcome). The swing voter in competitive seats has a different experience (valuable, targeted, but treated instrumentally). The electoral administration system sees piton (defending rules through institutional inertia). The analytical observer risks seeing mountain (Duverger's Law as inevitable) — but the trajectory of extractiveness increase from 1950-2024 reveals active enforcement, not mathematical necessity. The false summit test: if the duopoly were truly inevitable, its extractiveness would be stable or declining (old rules settling into background). Instead, extractiveness is rising as rules are actively strengthened (Citizens United, debate commission rules post-1992, gerrymandering intensification post-2010).
 *
 * DIRECTIONALITY LOGIC:
 *   Major party leadership: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.10. Net beneficiary. Exit option (defection to third party) exists but is extremely costly due to lock-in of institutional resources and donor networks. They see the constraint as coordination only. Third-party candidate: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. Ballot access barrier is absolute (federal eligibility sets the threshold for viability). Swing voter in non-competitive seat: Victim + trapped → d≈0.88, f(d)≈1.25. Gerrymandering creates structural inevitability: mobilizing this voter cannot change the seat outcome. Organized reform coalition: Victim (in structure) + constrained → d≈0.55, f(d)≈0.75. They also benefit from some coordination (knowing the rules is valuable knowledge), creating the tangled_rope classification. Super PAC networks: Beneficiary + mobile → d≈0.45, f(d)≈0.60. They benefit from binary simplicity but are somewhat constrained by Citizens United dependency. Electoral administration: Beneficiary (maintains rules that protect them) + arbitrage → d≈0.08, f(d)≈-0.10. Piton perspective arises from theater gate (rule-defending activity increases theater ratio), not from high chi. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is a false summit (Duverger's Law is descriptive, not prescriptive; the enforcement is political).
 *
 * MANDATROPHY ANALYSIS:
 *   The duopoly resolves the mandatrophy by revealing extraction disguised as coordination. The trap is this: major party leadership genuinely experiences the duopoly as coordination (it reduces their organizational overhead). Third-party candidates genuinely experience it as snare (absolute barriers to entry). Both are structurally correct — but from different positions. The mandatrophy resolution comes from recognizing that if the same structural arrangement provides coordination benefit to beneficiaries AND extracts from victims, it is tangled_rope, not rope. The key diagnostic: can you remove the extraction (lower ballot access barriers, raise debate thresholds, eliminate gerrymandering) without destroying the coordination function (binary choice for voters, stable coalitions)? The answer is yes — Canada, Australia, and other FPTP democracies have multiple parties. This proves the extraction is not inherent to FPTP, it is added through enforcement. The duopoly is a textbook tangled_rope that has been mislabeled as coordinate (rope) by beneficiaries and as inevitable (mountain) by analysts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    spoiler_vs_legitimacy_threshold,
    'At what support threshold (polling %) does a third-party candidate cease to be a ''spoiler'' and become a legitimate expression of voter preference?',
    'Comparative analysis: (a) Ranked-choice voting jurisdictions (Maine, Alaska) and their third-party results; (b) Historical contingency: what polling threshold would have prevented the spoiler narrative in 2016/2020?',
    'If threshold < 5%: current 15% debate threshold is extraction mechanism. If threshold > 20%: debate threshold is legitimate quality gate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spoiler_vs_legitimacy_threshold, empirical, 'Support threshold distinguishing spoiler effect from legitimate candidacy').

omega_variable(
    gerrymandering_vs_polarization_causality,
    'Does gerrymandering cause polarization, or does polarization cause gerrymandering, or is the causality bidirectional?',
    'Time-series analysis of polarization measures vs gerrymandering intensity; cross-state variation in redistricting rules (e.g., independent commissions vs partisan legislatures) and polarization trends; international comparison with proportional representation systems',
    'If gerrymandering→polarization: reform it and polarization declines (extraction narrative). If polarization→gerrymandering: polarization is the root constraint, gerrymandering is symptom (structural inevitability). If bidirectional: both are locked in a reinforcing cycle (stronger tangled_rope classification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gerrymandering_vs_polarization_causality, empirical, 'Causal relationship between gerrymandering and political polarization').

omega_variable(
    ranked_choice_voting_feasibility,
    'Would ranked-choice voting or proportional representation systems actually reduce duopoly extraction, or merely transfer extraction to coalition-building stages?',
    'Analysis of multi-party democracies (proportional representation: Netherlands, Israel, Belgium) and recent RCV implementations (Maine, Alaska, Minnesota cities); measurement of coalition stability, minority representation, and transparency of preference aggregation',
    'If RCV reduces extraction: duopoly is a contingent institutional choice, not structural necessity. If RCV transfers extraction: the constraint is more fundamental (preference aggregation mathematics itself). This determines whether the scaffold perspective (RCV as sunset) is realistic or aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ranked_choice_voting_feasibility, empirical, 'Whether alternative voting systems reduce or transfer duopoly extraction').

omega_variable(
    swing_voter_intensity_distribution,
    'What fraction of voters in non-competitive seats are genuinely indifferent between major parties vs. strategically suppressing third-party preference to avoid spoiler guilt?',
    'Survey research: direct questions about preference ranking vs. strategic voting; comparison of stated preferences vs. voting behavior in RCV jurisdictions; analysis of third-party vote share before/after spoiler narrative emergence (2016-2020 inflection)',
    'If high indifference: duopoly is coordination mechanism (Rope). If high suppression: duopoly is extraction mechanism (Snare/Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(swing_voter_intensity_distribution, empirical, 'Degree to which swing voters are indifferent vs. strategically suppressed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_two_party_duopoly, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duopoly_tr_t0, us_two_party_duopoly, theater_ratio, 0, 0.48).
narrative_ontology:measurement(duopoly_tr_t25, us_two_party_duopoly, theater_ratio, 25, 0.58).
narrative_ontology:measurement(duopoly_tr_t50, us_two_party_duopoly, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(duopoly_be_t0, us_two_party_duopoly, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(duopoly_be_t25, us_two_party_duopoly, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(duopoly_be_t50, us_two_party_duopoly, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_two_party_duopoly, enforcement_mechanism).
narrative_ontology:affects_constraint(us_two_party_duopoly, campaign_finance_oligarchy).
narrative_ontology:affects_constraint(us_two_party_duopoly, partisan_gerrymandering).
narrative_ontology:affects_constraint(us_two_party_duopoly, voter_suppression_mechanisms).

% DUAL FORMULATION NOTE:
% The duopoly constraint decomposes into three structural components with different ε values: (1) FPTP electoral math (ε≈0.15, mountain-like mathematical tendency), (2) ballot access enforcement (ε≈0.55, snare mechanism), (3) gerrymandering and voter suppression (ε≈0.60, tangled_rope enforcement). These are linked: the electoral math provides structural advantage to two parties, but the other mechanisms actively suppress alternatives. The combined constraint (us_two_party_duopoly, ε=0.58) reflects the enforcement layers that turn mathematical tendency into political reality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_two_party_duopoly, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
