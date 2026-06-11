% ============================================================================
% CONSTRAINT STORY: intra_party_fragmentation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_intra_party_fragmentation, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: intra_party_fragmentation
 *   human_readable: Intra-Party Fragmentation in Two-Party System
 *   domain: political_science/public_opinion/democratic_theory
 *
 * SUMMARY:
 *   Intra-party fragmentation in the U.S. two-party system creates a
 *   structural tension between the coordination function of aggregating
 *   diverse preferences into governing coalitions and the extraction function
 *   of maintaining power through strategic ambiguity that frustrates coherent
 *   representation. Both major parties contain multiple distinct value
 *   clusters: within the Democratic coalition, democratic socialists,
 *   progressive activists, moderate suburbanites, and traditional union
 *   Democrats hold conflicting positions on economic policy, cultural issues,
 *   and foreign policy. Within the Republican coalition, libertarians,
 *   religious conservatives, populist nationalists, and business-oriented
 *   moderates similarly diverge on trade, immigration, social issues, and
 *   democratic norms. The constraint exhibits tangled_rope characteristics:
 *   genuine coordination (parties do aggregate preferences and form governing
 *   coalitions) layered with asymmetric extraction (leadership maintains
 *   contradictions that benefit institutional power at the cost of voter
 *   coherence). The theater_ratio (0.68) reflects that party platforms, unity
 *   messaging, and coalition rhetoric are substantially performative:
 *   platforms paper over contradictions with vague language, unity is
 *   performed for media while factions negotiate behind closed doors, and
 *   coalition maintenance increasingly relies on negative partisanship (fear
 *   of the other party) rather than positive agreement. The suppression
 *   trajectory shows increasing enforcement: ballot access laws, debate
 *   inclusion thresholds, campaign finance advantages, and media coverage
 *   norms have hardened over the 30-year interval, making third-party exit
 *   progressively more costly.
 *
 * KEY AGENTS:
 *   - Ideologically Homeless Voter: Primary victim (powerless/trapped) — holds policy preferences that don't align with either party's coalition but cannot exit due to electoral system constraints; bears full cost of misrepresentation
 *   - Issue Advocacy Groups: Secondary victim (moderate/constrained) — need party infrastructure to achieve policy goals but have demands diluted to maintain broader coalition; mixed experience of coordination and extraction
 *   - Party Leadership: Primary beneficiary (institutional/arbitrage) — captures institutional power, fundraising advantage, and media attention by holding contradictory coalition together; experiences fragmentation as coordination problem they are paid to solve
 *   - Primary Challengers: Secondary victim (moderate/constrained) — face party establishment advantages and strategic ambiguity that prevents clear positioning; benefit from conflict creating opening but victimized by defensive coordination against outsiders
 *   - Party Platform Committee: Institutional actor (institutional/mobile) — maintains performative platform-writing ritual that has atrophied into theater; functional coordination has migrated elsewhere
 *   - Electoral Reform Coalition: Organized agents (organized/mobile) — ranked-choice voting advocates, proportional representation reformers building alternative systems with sunset logic
 *   - Political Consultants: Secondary beneficiary (institutional/arbitrage) — profit from managing intra-party conflict and strategic ambiguity; coalition maintenance complexity is their business model
 *   - Media Covering Conflict: Secondary beneficiary (institutional/arbitrage) — intra-party conflict generates content; horse-race coverage of factional disputes is more engaging than policy substance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(intra_party_fragmentation, 0.48).
domain_priors:suppression_score(intra_party_fragmentation, 0.62).
domain_priors:theater_ratio(intra_party_fragmentation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(intra_party_fragmentation, extractiveness, 0.48).
narrative_ontology:constraint_metric(intra_party_fragmentation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(intra_party_fragmentation, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(intra_party_fragmentation, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(intra_party_fragmentation, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(intra_party_fragmentation, tangled_rope).
narrative_ontology:human_readable(intra_party_fragmentation, "Intra-Party Fragmentation in Two-Party System").
narrative_ontology:topic_domain(intra_party_fragmentation, "political_science/public_opinion/democratic_theory").

domain_priors:requires_active_enforcement(intra_party_fragmentation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(intra_party_fragmentation, party_leadership_maintaining_coalition).
narrative_ontology:constraint_beneficiary(intra_party_fragmentation, political_consultants).
narrative_ontology:constraint_beneficiary(intra_party_fragmentation, media_covering_intra_party_conflict).
narrative_ontology:constraint_victim(intra_party_fragmentation, voters_seeking_coherent_representation).
narrative_ontology:constraint_victim(intra_party_fragmentation, issue_advocacy_groups).
narrative_ontology:constraint_victim(intra_party_fragmentation, primary_challengers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(intra_party_fragmentation, issue_advocacy_group).
narrative_ontology:constraint_beneficiary(intra_party_fragmentation, party_leadership).
narrative_ontology:constraint_beneficiary(intra_party_fragmentation, primary_challenger).
narrative_ontology:constraint_beneficiary(intra_party_fragmentation, media_covering_conflict).
narrative_ontology:constraint_victim(intra_party_fragmentation, ideologically_homeless_voter).
narrative_ontology:constraint_victim(intra_party_fragmentation, issue_advocacy_group).
narrative_ontology:constraint_victim(intra_party_fragmentation, primary_challenger).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds policy preferences that don't align with either party's coalition on multiple salient issues (e.g., fiscally conservative but socially liberal, or economically populist but culturally traditional). Cannot exit to third party without wasting vote due to first-past-the-post system and ballot access barriers. Votes for party that represents them poorly because alternative is worse. Bears full cost of misrepresentation: policy outcomes don't reflect their preferences, and they experience cognitive dissonance between their values and their party affiliation.
narrative_ontology:constraint_stakeholder(intra_party_fragmentation, ideologically_homeless_voter, payer,
    powerless, biographical, trapped, national).

% Single-issue or narrow-issue organization (abortion rights, gun rights, climate policy, labor rights) that needs party infrastructure to achieve policy goals. Benefits from party's voter lists, fundraising networks, media access, and legislative coordination. But has policy demands diluted to maintain broader coalition: leadership trades away their priorities to keep other factions in tent. Can threaten exit (endorsing primary challengers, sitting out elections) but at high cost of losing access and influence. Experiences mixed extraction: real coordination benefits but also real compromise costs.
narrative_ontology:constraint_stakeholder(intra_party_fragmentation, issue_advocacy_group, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(intra_party_fragmentation, issue_advocacy_group, beneficiary).

% National committee chairs, congressional leadership, major donors, and senior strategists who set party agenda and manage coalition. Capture institutional power (committee assignments, fundraising advantages, media attention) by holding contradictory coalition together. Experience fragmentation as coordination problem they are paid to solve: managing diverse factions is the job. Use strategic ambiguity, negative partisanship, and selective emphasis to paper over contradictions. Net beneficiaries: the complexity that frustrates voters is their professional opportunity.
narrative_ontology:constraint_stakeholder(intra_party_fragmentation, party_leadership, agenda_setter,
    institutional, immediate, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(intra_party_fragmentation, party_leadership, beneficiary).

% Candidate challenging incumbent or establishment-backed candidate in party primary. Benefits from intra-party conflict creating opening: can mobilize dissatisfied faction against perceived betrayal by leadership. But victimized by party establishment's ability to unite disparate factions against outsider threat: leadership deploys institutional advantages (endorsements, fundraising, media access) and strategic ambiguity (incumbent can position differently to different audiences). Faces high costs: career risk if challenge fails, resource disadvantage, and difficulty positioning clearly when party brand is ambiguous.
narrative_ontology:constraint_stakeholder(intra_party_fragmentation, primary_challenger, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(intra_party_fragmentation, primary_challenger, beneficiary).

% Formal body that drafts party platform at national convention. Maintains ritual of platform-writing but functional coordination has migrated elsewhere: platforms are drafted to paper over contradictions with vague language, then ignored by candidates who position themselves independently. Committee members know the process is theater but participate because convention tradition demands it and because platform fights provide symbolic victories for factions. The ritual persists through institutional inertia, not because it coordinates actual policy.
narrative_ontology:constraint_stakeholder(intra_party_fragmentation, party_platform_committee, agenda_setter,
    institutional, generational, mobile, national).

% Organized groups advocating ranked-choice voting, proportional representation, fusion voting, or other alternative electoral systems. See fragmentation as temporary coordination failure with sunset logic: alternative systems would allow value clusters to organize as distinct parties while still forming governing coalitions. Building state-level reforms to demonstrate viability. Estimated sunset: 15-30 years as reforms spread and generational replacement reduces attachment to two-party system. Not directly paying costs of current fragmentation but observing and documenting its dysfunction.
narrative_ontology:constraint_stakeholder(intra_party_fragmentation, electoral_reform_coalition, observer,
    organized, generational, mobile, national).

% Professional campaign strategists, pollsters, and messaging experts hired to manage intra-party conflict and strategic positioning. Profit from coalition maintenance complexity: the more fragmented the party, the more sophisticated the messaging and targeting required, the more valuable their services. Benefit from fragmentation without running the system. Coalition management is their business model.
narrative_ontology:constraint_stakeholder(intra_party_fragmentation, political_consultants, beneficiary,
    institutional, immediate, arbitrage, national).

% News organizations covering intra-party conflict as horse-race drama. Benefit from fragmentation generating content: factional disputes, primary challenges, platform fights, and leadership tensions are more engaging than policy substance. Intra-party conflict is good for ratings and clicks. Not directly setting agenda but amplifying conflict for commercial reasons.
narrative_ontology:constraint_stakeholder(intra_party_fragmentation, media_covering_conflict, beneficiary,
    institutional, immediate, arbitrage, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(intra_party_fragmentation, party_leadership).
narrative_ontology:fixing_cost_class(intra_party_fragmentation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregating diverse policy preferences and cultural orientations into governing coalitions capable of winning elections and forming legislative majorities. Providing infrastructure (voter lists, fundraising networks, media access, legislative coordination) that individual voters and advocacy groups cannot achieve alone.
% TRANSFER_FUNCTION: Institutional power, fundraising advantages, media attention, and agenda-setting authority flow from voters and issue groups to party leadership. Policy coherence, accountability, and representation flow away from voters seeking clear positions. Professional opportunities flow to consultants and strategists managing coalition complexity.
% ABSENT_VOICES: Third-party advocates and voters who have exited the system entirely (non-voters, politically disengaged) would object that the two-party duopoly is maintained through suppression mechanisms (ballot access laws, debate thresholds, campaign finance advantages) rather than genuine preference aggregation. They are absent from the conversation because the system's rules exclude them from media coverage, debates, and viable candidacy. Ideologically homeless voters within the system would object more forcefully if they had collective organization, but their cross-cutting preferences prevent coalition formation.
% DISAPPEARANCE_RATIONALE: If intra-party fragmentation disappeared overnight (parties became ideologically coherent), the political landscape would rearrange substantially: some voters would switch parties to align with new coherent positions, issue advocacy groups would realign with parties that clearly represented their priorities, primary challenges would decrease as ideological clarity reduced internal conflict, and media coverage would shift from intra-party drama to inter-party policy contrast. The current arrangement of voter coalitions, advocacy group strategies, and leadership power depends on fragmentation persisting.
% FOUNDING_PROBLEM: The two-party system emerged to aggregate diverse regional, economic, and cultural interests into governing coalitions in a large, heterogeneous republic with single-member district plurality voting. The founding problem was: how to form stable majorities capable of governing when the electorate is geographically dispersed and ideologically diverse?
% FOUNDING_PROBLEM_CORROBORATION: Political scientists and historians corroborate that coalition-building was the founding function (Duverger's Law, median voter theorem, historical analysis of party system evolution). But whether the current level of intra-party fragmentation serves that function or has become extractive is contested: electoral reform advocates argue that alternative systems (proportional representation, ranked-choice voting) would aggregate preferences more coherently; party leadership argues that big-tent coalitions are necessary for governance in a diverse society; voters express dissatisfaction (high percentages say party doesn't represent them well) suggesting the coordination function has degraded. The founding problem is live (preference aggregation is still necessary) but whether the current mechanism solves it or extracts from it is disputed.
narrative_ontology:disappearance_verdict(intra_party_fragmentation, world_rearranges).
narrative_ontology:founding_problem_status(intra_party_fragmentation, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IDEOLOGICALLY HOMELESS VOTER (SNARE) — Trapped by first-past-the-post electoral system and ballot access barriers. Cannot exit to third party without wasting vote. Bears full cost of misrepresentation: votes for party that doesn't reflect their values on multiple salient issues because the alternative is worse. Maximum experienced extraction.
constraint_indexing:constraint_classification(intra_party_fragmentation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ISSUE ADVOCACY GROUP (TANGLED ROPE) — Constrained by need to work within party coalition to achieve policy goals, but also benefits from party infrastructure for mobilization. Experiences mixed extraction: party provides coordination mechanism (voter lists, fundraising networks, media access) but dilutes policy demands to maintain broader coalition. Can threaten exit but at high cost.
constraint_indexing:constraint_classification(intra_party_fragmentation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PARTY LEADERSHIP (ROPE) — Benefits from fragmentation by maintaining broad coalition through strategic ambiguity. Experiences constraint as coordination: managing diverse factions is the job, and the two-party system protects against defection. Net beneficiary: captures institutional power, fundraising advantage, and media attention by holding coalition together despite internal contradictions.
constraint_indexing:constraint_classification(intra_party_fragmentation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PRIMARY CHALLENGER (TANGLED ROPE) — Constrained by party infrastructure advantages for incumbents and strategic ambiguity that prevents clear ideological positioning. Benefits from intra-party conflict creating opening for challenge, but also victimized by party establishment's ability to unite disparate factions against outsider threat. Mixed experience: fragmentation creates opportunity but also defensive coordination.
constraint_indexing:constraint_classification(intra_party_fragmentation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: PARTY PLATFORM COMMITTEE (PITON) — The formal platform-writing process has atrophied into theater. Platforms are drafted to paper over contradictions with vague language, then ignored by candidates who position themselves independently. The ritual persists through institutional inertia: conventions still produce platforms, media still covers them, but functional coordination has migrated to candidate-specific messaging and coalition management. High theater ratio.
constraint_indexing:constraint_classification(intra_party_fragmentation, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ELECTORAL REFORM COALITION (SCAFFOLD) — Organized groups (ranked-choice voting advocates, proportional representation reformers, fusion voting proponents) see fragmentation as temporary coordination failure with sunset logic. Alternative electoral systems would allow value clusters to organize as distinct parties while still forming governing coalitions. Estimated sunset: 15-30 years as state-level reforms demonstrate viability and generational replacement reduces attachment to two-party system.
constraint_indexing:constraint_classification(intra_party_fragmentation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From analytical distance, intra-party fragmentation represents genuine coordination function (aggregating diverse preferences into governing coalitions) layered with asymmetric extraction (leadership captures power by maintaining contradictions that frustrate coherent representation). The constraint is not natural law: comparative evidence shows multiparty systems handle value pluralism differently. But it's also not pure extraction: some coalition-building is necessary for governance. Tangled rope classification reflects both real coordination and real extraction.
constraint_indexing:constraint_classification(intra_party_fragmentation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(intra_party_fragmentation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(intra_party_fragmentation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(intra_party_fragmentation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(intra_party_fragmentation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(intra_party_fragmentation, TR),
    TR >= 0.70.

:- end_tests(intra_party_fragmentation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. Party leadership captures institutional power and fundraising advantages by maintaining coalitions despite internal contradictions. Voters seeking coherent representation bear costs: they vote for parties that don't reflect their values on multiple salient issues because the alternative is worse. Issue advocacy groups have policy demands diluted. But extraction is not maximal: parties do provide real coordination function (aggregating preferences, forming governing coalitions, providing infrastructure for mobilization). The value reflects genuine mixed function. Suppression (0.62): Moderate-high and increasing. Ballot access laws require tens of thousands of signatures in many states. Debate inclusion thresholds (15% polling) are nearly insurmountable for third parties. Campaign finance advantages (party committees, donor networks, bundling operations) create massive resource asymmetry. Media coverage norms treat third parties as spoilers rather than legitimate alternatives. But suppression is not total: some third-party and independent candidates do achieve ballot access and media coverage, and primary challenges within parties provide partial exit option. Theater ratio (0.68): High and increasing. Party platforms are drafted to paper over contradictions with vague language, then ignored by candidates. Unity messaging is performed for media while factions negotiate behind closed doors. Coalition maintenance increasingly relies on negative partisanship (fear of other party) rather than positive agreement on policy. The ritual persists but functional coordination has migrated to candidate-specific messaging and behind-the-scenes faction management. Accessibility collapse (0.35): Low-moderate. Alternatives persist: primary challenges, third-party runs, independent candidacies, state-level electoral reforms, and exit to political inactivity are all visible options. The constraint does not collapse alternatives completely. Resistance (0.58): Moderate-high. Voters express dissatisfaction (high percentages say party doesn't represent them well), issue groups threaten exit, primary challengers emerge, and electoral reform movements gain traction. The constraint meets substantial active resistance.
 *
 * PERSPECTIVAL GAP:
 *   The ideologically homeless voter sees pure extraction (Snare): trapped by electoral system, bearing full cost of misrepresentation with no exit. Issue advocacy groups see mixed coordination and extraction (Tangled Rope): party provides infrastructure but dilutes demands. Party leadership sees coordination (Rope): managing diverse factions is the job, and they are net beneficiaries. Primary challengers see mixed experience (Tangled Rope): fragmentation creates opportunity but also defensive coordination against outsiders. The platform committee sees degraded ritual (Piton): formal process has atrophied into theater. Electoral reform coalition sees temporary problem with sunset (Scaffold): alternative systems would resolve fragmentation. The analytical observer sees genuine tangled rope: real coordination function layered with asymmetric extraction. The perspectival gap reveals how structural position determines whether fragmentation appears as necessary coalition-building or extractive misrepresentation.
 *
 * DIRECTIONALITY LOGIC:
 *   Party leadership and political consultants are primary beneficiaries: they capture institutional power, fundraising advantages, and professional opportunities from managing fragmented coalitions. Their arbitrage exit options and institutional power produce low directionality values and low or negative effective extraction. Ideologically homeless voters are primary victims: trapped by electoral system with no viable exit, bearing full cost of misrepresentation. Their powerless position and trapped exit produce high directionality and maximum effective extraction. Issue advocacy groups and primary challengers occupy middle ground: constrained exit options and moderate power, experiencing mixed coordination and extraction. The platform committee, despite institutional power, experiences the constraint as degraded ritual (piton) rather than high extraction. Media and consultants are secondary beneficiaries: they profit from conflict and complexity without running the system. The analytical observer, with analytical power and exit, sees the structural mix of coordination and extraction without experiencing either strongly.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that tangled_rope is the structurally accurate classification when genuine coordination and asymmetric extraction coexist. The coordination function is real: parties do aggregate diverse preferences into governing coalitions, provide infrastructure for mobilization, and enable collective action that individual voters or groups could not achieve alone. But the extraction is also real: leadership maintains contradictions that benefit institutional power at the cost of voter coherence, strategic ambiguity prevents accountability, and suppression mechanisms (ballot access, debate thresholds, campaign finance) protect the arrangement from competition. The constraint is neither pure coordination (rope) nor pure extraction (snare) but genuinely mixed. The perspectival variation (snare from trapped voters, rope from leadership, scaffold from reformers) reflects different structural positions relative to the same mixed mechanism, not disagreement about the mechanism's nature. The analytical classification as tangled_rope is vindicated by the structural data: beneficiaries and victims are both present, active enforcement is required, and the coordination function coexists with asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    duvergers_law_necessity,
    'Is two-party fragmentation an inevitable consequence of single-member district plurality voting (Duverger''s Law), or a contingent outcome of specific ballot access laws, campaign finance rules, and media structures?',
    'Comparative analysis of countries with SMDP systems but different party structures; natural experiments from state-level electoral reforms; historical analysis of third-party viability under varying institutional conditions',
    'If inevitable: mountain classification from more perspectives (structural feature of electoral system). If contingent: snare/tangled_rope classification confirmed (constructed constraint maintained by specific institutional choices).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(duvergers_law_necessity, empirical, 'Whether two-party fragmentation is inevitable under SMDP voting').

omega_variable(
    coalition_stability_threshold,
    'At what level of within-party variance does coalition maintenance become extractive rather than coordinative? When does ''big tent'' become ''incoherent misrepresentation''?',
    'Longitudinal analysis of party defection rates, split-ticket voting, and voter satisfaction as function of within-party policy variance; identification of threshold beyond which coordination costs exceed coordination benefits',
    'If threshold is low: many current coalitions are extractive (snare from more perspectives). If threshold is high: current fragmentation is within coordination range (rope from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_stability_threshold, empirical, 'Threshold distinguishing coordination from extraction in coalition maintenance').

omega_variable(
    strategic_ambiguity_functionality,
    'Does strategic ambiguity on divisive issues serve a genuine coordination function (allowing diverse voters to project preferred positions onto candidates) or primarily an extraction function (preventing accountability while capturing votes)?',
    'Analysis of voter belief accuracy about candidate positions; correlation between ambiguity and post-election policy outcomes; comparison of voter satisfaction under ambiguous vs clear positioning',
    'If coordination: rope classification more defensible. If extraction: snare classification more accurate. Likely mixed: tangled_rope confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_ambiguity_functionality, conceptual, 'Whether strategic ambiguity coordinates or extracts').

omega_variable(
    ranked_choice_sufficiency,
    'Would ranked-choice voting or other alternative electoral systems actually resolve intra-party fragmentation, or would value clusters remain trapped within major parties due to path dependence, brand recognition, and institutional advantages?',
    'Analysis of party system evolution in jurisdictions that adopted RCV or proportional representation; measurement of new party formation rates and viability; assessment of whether value clusters successfully exit major parties',
    'If sufficient: scaffold perspective confirmed (real sunset exists). If insufficient: fragmentation persists under alternative systems (mountain-like from more perspectives, or snare with deeper structural roots).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ranked_choice_sufficiency, empirical, 'Whether electoral reform provides genuine exit from fragmentation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(intra_party_fragmentation, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ipf_tr_t0, intra_party_fragmentation, theater_ratio, 0, 0.45).
narrative_ontology:measurement(ipf_tr_t10, intra_party_fragmentation, theater_ratio, 10, 0.55).
narrative_ontology:measurement(ipf_tr_t20, intra_party_fragmentation, theater_ratio, 20, 0.62).
narrative_ontology:measurement(ipf_tr_t30, intra_party_fragmentation, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(ipf_be_t0, intra_party_fragmentation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ipf_be_t10, intra_party_fragmentation, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(ipf_be_t20, intra_party_fragmentation, base_extractiveness, 20, 0.43).
narrative_ontology:measurement(ipf_be_t30, intra_party_fragmentation, base_extractiveness, 30, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(ipf_su_t0, intra_party_fragmentation, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(ipf_su_t10, intra_party_fragmentation, suppression_requirement, 10, 0.54).
narrative_ontology:measurement(ipf_su_t20, intra_party_fragmentation, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(ipf_su_t30, intra_party_fragmentation, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(intra_party_fragmentation, identity_coordination).
narrative_ontology:affects_constraint(intra_party_fragmentation, primary_election_gatekeeping).
narrative_ontology:affects_constraint(intra_party_fragmentation, negative_partisanship_escalation).
narrative_ontology:affects_constraint(intra_party_fragmentation, policy_position_ambiguity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
