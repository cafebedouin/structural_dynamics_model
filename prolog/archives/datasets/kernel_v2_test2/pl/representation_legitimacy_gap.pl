% ============================================================================
% CONSTRAINT STORY: representation_legitimacy_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_representation_legitimacy_gap, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: representation_legitimacy_gap
 *   human_readable: Representation Legitimacy Gap in Two-Party Systems
 *   domain: political_science/public_opinion/democratic_theory
 *
 * SUMMARY:
 *   The representation legitimacy gap describes a structural contradiction in
 *   contemporary American democracy: significant portions of the electorate
 *   vote for major party candidates while simultaneously reporting that
 *   neither party represents them well or cares about people like them. The
 *   Hidden Tribes study documents this starkly: 18% of Americans overall say
 *   neither party represents them, rising to 39% among the 'Left-Out Left'
 *   cohort. Yet these voters continue to participate in binary elections,
 *   producing the paradox of high turnout coupled with low perceived
 *   representation. This constraint operates through the interaction of
 *   first-past-the-post electoral rules, ballot access barriers, strategic
 *   voting dynamics, and the two-party duopoly's institutional advantages.
 *   The gap has widened over the 30-year measurement interval as party
 *   polarization increased while median voter preferences remained more
 *   moderate, creating larger representation distance without corresponding
 *   increase in viable alternatives. The constraint exhibits rising
 *   extraction (0.45 to 0.68), rising theater (0.35 to 0.65), and rising
 *   suppression (0.55 to 0.72) as the duopoly has hardened structural
 *   barriers while maintaining democratic legitimation rituals.
 *
 * KEY AGENTS:
 *   - Left-Out Left Voters: Primary victims (powerless/trapped) — 39% report neither party represents them; face binary choice with no viable exit due to electoral structure and ballot access barriers
 *   - Disaffected Moderates: Secondary victims (powerless/constrained) — report representation gap but have marginal exit capacity through primary voting or local third-party viability in specific districts
 *   - Two-Party Duopoly: Primary beneficiary (institutional/arbitrage) — captures stable fundraising base and predictable electoral competition regardless of voter satisfaction; gap between representation and voting behavior ensures participation without requiring responsiveness
 *   - Party Fundraising Apparatus: Secondary beneficiary (institutional/arbitrage) — benefits from forced-choice dynamics that maintain donor base even as satisfaction declines
 *   - Partisan Media Ecosystem: Secondary beneficiary (institutional/mobile) — benefits from binary framing that simplifies coverage and maintains audience engagement through us-versus-them narratives
 *   - Third-Party Movements: Organized agents (organized/constrained) — attempt to build alternatives but face structural barriers (ballot access, debate exclusion, spoiler dynamics) that protect duopoly
 *   - Primary Election System: Institutional mechanism (institutional/mobile) — originally designed to channel dissent into party reform, now largely theatrical due to low turnout, donor influence, and party apparatus control
 *   - Democratic Legitimacy: Abstract victim (powerless/trapped) — the collective good of government responsiveness to citizen preferences; cannot organize or exit; bears full cost of representation gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(representation_legitimacy_gap, 0.68).
domain_priors:suppression_score(representation_legitimacy_gap, 0.72).
domain_priors:theater_ratio(representation_legitimacy_gap, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(representation_legitimacy_gap, extractiveness, 0.68).
narrative_ontology:constraint_metric(representation_legitimacy_gap, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(representation_legitimacy_gap, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(representation_legitimacy_gap, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(representation_legitimacy_gap, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(representation_legitimacy_gap, snare).
narrative_ontology:human_readable(representation_legitimacy_gap, "Representation Legitimacy Gap in Two-Party Systems").
narrative_ontology:topic_domain(representation_legitimacy_gap, "political_science/public_opinion/democratic_theory").

domain_priors:requires_active_enforcement(representation_legitimacy_gap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(representation_legitimacy_gap, two_party_duopoly).
narrative_ontology:constraint_beneficiary(representation_legitimacy_gap, party_fundraising_apparatus).
narrative_ontology:constraint_beneficiary(representation_legitimacy_gap, partisan_media_ecosystem).
narrative_ontology:constraint_victim(representation_legitimacy_gap, voters_without_viable_alternatives).
narrative_ontology:constraint_victim(representation_legitimacy_gap, left_out_left_cohort).
narrative_ontology:constraint_victim(representation_legitimacy_gap, democratic_legitimacy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(representation_legitimacy_gap, third_party_movements).
narrative_ontology:constraint_victim(representation_legitimacy_gap, left_out_left_voters).
narrative_ontology:constraint_victim(representation_legitimacy_gap, disaffected_moderates).
narrative_ontology:constraint_victim(representation_legitimacy_gap, third_party_movements).
narrative_ontology:constraint_vindicates(representation_legitimacy_gap, median_voter_theorem).
narrative_ontology:constraint_vindicates(representation_legitimacy_gap, duvergers_law_inevitability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% 39% report neither party represents them. Face binary choice in general elections with no viable third-party alternative due to first-past-the-post rules and ballot access barriers. Must vote for party they believe doesn't care about them or accept spoiler stigma and wasted vote. Cannot organize effective alternative due to geographic dispersion and structural suppression of third parties. Bear full cost of representation gap: policy preferences ignored, civic efficacy undermined, forced into lesser-evil logic every election cycle.
narrative_ontology:constraint_stakeholder(representation_legitimacy_gap, left_out_left_voters, payer,
    powerless, biographical, trapped, national).

% Report neither party represents them but have marginal exit capacity through strategic primary voting or local third-party viability in specific districts. Face social pressure to pick a side and see policy preferences ignored by both parties. Slightly more agency than Left-Out Left through primary participation but still experience high extraction in general elections. Constrained by same structural barriers but with narrow tactical options.
narrative_ontology:constraint_stakeholder(representation_legitimacy_gap, disaffected_moderates, payer,
    powerless, biographical, constrained, national).

% Captures stable fundraising base and predictable electoral competition regardless of voter satisfaction. The gap between representation and voting behavior ensures continued participation without requiring genuine responsiveness. Benefits from structural barriers that suppress third-party alternatives: ballot access rules, debate inclusion thresholds, campaign finance advantages, first-past-the-post rules that punish vote-splitting. Can adjust messaging at margins without structural change. The representation gap is not a bug but a feature: it maintains institutional stability while externalizing costs to voters with no exit.
narrative_ontology:constraint_stakeholder(representation_legitimacy_gap, two_party_duopoly, beneficiary,
    institutional, immediate, arbitrage, national).

% Benefits from forced-choice dynamics that maintain donor base even as voter satisfaction declines. Donors continue giving to lesser-evil party because alternative is worse, not because party is responsive. The representation gap creates stable revenue stream disconnected from actual representation quality. Can arbitrage between donor preferences and voter preferences because voters have no exit.
narrative_ontology:constraint_stakeholder(representation_legitimacy_gap, party_fundraising_apparatus, beneficiary,
    institutional, immediate, arbitrage, national).

% Benefits from binary framing that simplifies coverage and maintains audience engagement through us-versus-them narratives. The representation gap creates content: disaffected voters provide ongoing stories of party failure, primary challenges, third-party speculation. But media also reinforces gap by treating third parties as spoilers and framing elections as binary choice. Mobile exit: could cover multiparty systems but benefits from current structure.
narrative_ontology:constraint_stakeholder(representation_legitimacy_gap, partisan_media_ecosystem, beneficiary,
    institutional, biographical, mobile, national).

% Organized agents attempting to build alternative parties. Benefit from representation gap by recruiting disaffected voters but face structural barriers that protect duopoly: ballot access requirements, debate exclusion, spoiler dynamics, resource asymmetry, media marginalization. Experience genuine coordination problem (how to build party infrastructure) layered with extraction (barriers beyond what coordination requires). Constrained by structural suppression but have some agency through local organizing and ballot access litigation.
narrative_ontology:constraint_stakeholder(representation_legitimacy_gap, third_party_movements, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(representation_legitimacy_gap, third_party_movements, beneficiary).

% Institutional mechanism originally designed to democratize party nomination and channel dissent into party reform. Now largely theatrical: low turnout, donor influence, and party apparatus control mean primaries rarely produce candidates responsive to disaffected voters. Persists as legitimation ritual: 'you could have voted in the primary' deflects representation complaints. Function atrophied but performance maintained. Listed as non-agent because it is a mechanism, not an actor, but included for narrative completeness.
narrative_ontology:constraint_stakeholder(representation_legitimacy_gap, primary_election_system, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_non_agent(representation_legitimacy_gap, primary_election_system).

% Abstract collective good: government responsiveness to citizen preferences. Cannot organize, cannot exit, bears full cost of representation gap. When 39% of a cohort reports neither party represents them yet continues voting, the legitimacy of representative democracy as a system is undermined. The gap between voting behavior and perceived representation corrodes civic trust and democratic norms over generational timescale. Listed as non-agent because it is an abstract good, not an actor.
narrative_ontology:constraint_stakeholder(representation_legitimacy_gap, democratic_legitimacy, payer,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(representation_legitimacy_gap, democratic_legitimacy).

% Organized reform movement attempting to change electoral rules to allow preference expression beyond binary choice. Analytical perspective: sees representation gap as artifact of first-past-the-post rules that could be eliminated through ranked-choice voting or proportional representation. Constrained by state-level implementation barriers and duopoly resistance but has achieved local victories. Observes the gap from outside the forced-choice dynamic.
narrative_ontology:constraint_stakeholder(representation_legitimacy_gap, ranked_choice_voting_advocates, observer,
    organized, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(representation_legitimacy_gap, two_party_duopoly).
narrative_ontology:fixing_cost_class(representation_legitimacy_gap, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregating diverse political preferences into binary electoral choice; maintaining stable party system that can form governing coalitions; reducing transaction costs of electoral competition through brand recognition and institutional continuity.
% TRANSFER_FUNCTION: Transfers political representation, policy responsiveness, and civic efficacy from voters without viable alternatives to the two-party duopoly. Transfers stable fundraising base, predictable electoral competition, and institutional advantages to established parties. Transfers legitimation (democratic participation theater) from primary election system to duopoly without corresponding transfer of actual responsiveness.
% ABSENT_VOICES: Third-party voters who face spoiler stigma and are excluded from debates and media coverage. Non-voters who have exited the system entirely due to representation gap (not captured in the 39% figure which measures those who still vote). Future generations who will inherit degraded democratic legitimacy. Voters in safe districts where general election is non-competitive and primary is controlled by party apparatus. All are absent from the conversation that sets electoral rules and ballot access requirements — the duopoly writes the rules that protect the duopoly.
% DISAPPEARANCE_RATIONALE: If the representation legitimacy gap disappeared overnight — if voters suddenly had viable alternatives that represented their preferences — the entire structure of American electoral politics would rearrange. The two-party duopoly would face genuine competition and would have to become responsive or lose voters to alternatives. The fundraising apparatus would fragment as donors could support parties that actually represented their preferences. The partisan media ecosystem would have to cover multiparty competition rather than binary framing. Primary elections would either become genuinely competitive (if parties reformed to retain voters) or become irrelevant (if voters defected to new parties). The gap is not a natural feature of democracy but a structural product of specific electoral rules and institutional barriers — its disappearance would require and produce massive rearrangement.
% FOUNDING_PROBLEM: The two-party system emerged to solve genuine coordination problems in early American democracy: aggregating diverse state and regional interests into national coalitions, providing stable governing majorities, reducing transaction costs of electoral competition, and maintaining continuity across administrations. The representation legitimacy gap was not part of the founding design — it emerged as parties became institutionally entrenched and erected barriers to alternatives.
% FOUNDING_PROBLEM_CORROBORATION: The coordination problem is corroborated by comparative political science: parliamentary systems with proportional representation face genuine challenges in coalition formation and government stability. However, the claim that the current representation gap is necessary to solve this problem is contested. Political scientists studying electoral systems (Duverger, Lijphart, Cox) document that the two-party equilibrium is a product of first-past-the-post rules, not an optimal solution to coordination problems. Ranked-choice voting advocates and proportional representation scholars argue that multiparty systems can solve coordination problems without creating representation gaps. The duopoly's claim that binary choice is necessary for stable governance is not corroborated by observers outside the beneficiary set — it is a self-serving justification for structural barriers that protect incumbents.
narrative_ontology:disappearance_verdict(representation_legitimacy_gap, world_rearranges).
narrative_ontology:founding_problem_status(representation_legitimacy_gap, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LEFT-OUT LEFT VOTER (SNARE) — 39% report neither party represents them, yet face binary choice with no viable exit. Trapped by first-past-the-post electoral structure, geographic sorting, and ballot access barriers. Maximum extraction: forced to vote for party they believe doesn't care about them or accept spoiler stigma and wasted vote. Cannot organize effective third party due to structural barriers.
constraint_indexing:constraint_classification(representation_legitimacy_gap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DISAFFECTED MODERATE (SNARE) — Reports neither party represents them but has slightly more exit capacity than Left-Out Left through strategic primary voting or local third-party viability in specific districts. Still experiences high extraction: must choose lesser evil in general elections, faces social pressure to pick a side, and sees policy preferences ignored by both parties. Constrained rather than trapped due to marginal primary influence.
constraint_indexing:constraint_classification(representation_legitimacy_gap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TWO-PARTY DUOPOLY (ROPE) — Experiences the representation gap as coordination: voters reliably choose between two options regardless of satisfaction, creating stable fundraising base and predictable electoral competition. The gap between representation and voting behavior is not a bug but a feature: it ensures continued participation without requiring genuine responsiveness. Net beneficiary with arbitrage exit: can adjust messaging at margins without structural change.
constraint_indexing:constraint_classification(representation_legitimacy_gap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THIRD-PARTY MOVEMENT (TANGLED ROPE) — Organized agents attempting to build alternative parties face genuine coordination problem (ballot access, debate inclusion, media coverage) but also experience extraction through spoiler dynamics, strategic voting pressure, and resource asymmetry. Benefits from the gap by recruiting disaffected voters but constrained by structural barriers that protect duopoly. Mixed experience: some agency but high costs.
constraint_indexing:constraint_classification(representation_legitimacy_gap, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: PRIMARY ELECTION SYSTEM (PITON) — Originally designed to democratize party nomination and channel dissent into party reform, now largely theatrical. Low turnout, donor influence, and party apparatus control mean primaries rarely produce candidates responsive to disaffected voters. The mechanism persists as legitimation ritual: 'you could have voted in the primary' deflects representation complaints. Function atrophied but performance maintained.
constraint_indexing:constraint_classification(representation_legitimacy_gap, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational perspective, the gap reveals genuine coordination problem (aggregating diverse preferences into binary choice) layered with asymmetric extraction (duopoly captures benefits of forced choice without bearing costs of unresponsiveness). Not pure extraction: some voters genuinely prefer binary simplicity and party brand stability. Not pure coordination: structural barriers suppress alternatives beyond what coordination requires. Claimed type matches analytical classification.
constraint_indexing:constraint_classification(representation_legitimacy_gap, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(representation_legitimacy_gap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(representation_legitimacy_gap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(representation_legitimacy_gap, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(representation_legitimacy_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(representation_legitimacy_gap, TR),
    TR >= 0.70.

:- end_tests(representation_legitimacy_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The duopoly captures substantial benefits from forced-choice dynamics: stable fundraising base, predictable electoral competition, and continued participation without requiring genuine responsiveness to disaffected voters. The 39% Left-Out Left figure represents massive extraction when voters must choose between parties they believe don't represent them. The value reflects that extraction is not total (some voters genuinely prefer binary simplicity) but is substantial and rising. Suppression (0.72): High. Multiple structural barriers suppress alternatives: first-past-the-post rules that punish vote-splitting, ballot access requirements that favor established parties, debate inclusion thresholds controlled by duopoly, campaign finance rules that advantage institutional fundraising, geographic sorting that reduces competitive districts, and social pressure to pick a side. The suppression has intensified over the interval as parties have hardened these barriers. Theater ratio (0.65): Moderate-high. Primary elections persist as legitimation ritual ('you could have voted in the primary') but rarely produce responsive candidates due to low turnout, donor influence, and party apparatus control. General election campaigns maintain democratic theater (debates, town halls, voter outreach) while actual policy responsiveness to disaffected voters remains low. The theater has increased as the gap between campaign promises and governing behavior has widened. Accessibility collapse (0.58): Moderate. Alternatives have not completely collapsed — third parties exist, primary challenges occur, local experiments with ranked-choice voting proceed — but the dominant framing treats two-party system as inevitable and alternatives as spoilers. Lower than typical snare because resistance remains visible. Resistance (0.42): Moderate. Significant active resistance through third-party movements, primary challenges to establishment candidates, ranked-choice voting advocacy, and open primary reforms. Higher than typical snare because the representation gap generates ongoing contestation rather than resignation.
 *
 * PERSPECTIVAL GAP:
 *   The representation legitimacy gap produces stark perspectival divergence. Trapped voters (Left-Out Left) experience pure extraction: forced to vote for parties they believe don't represent them, with no viable exit and high costs to abstention (spoiler stigma, civic duty pressure, lesser-evil logic). The duopoly experiences coordination: the gap ensures continued participation without requiring responsiveness, creating stable institutional environment. Third-party movements experience mixed coordination and extraction: genuine ballot access and debate inclusion problems layered with structural barriers that protect duopoly beyond what coordination requires. The primary system appears as degraded ritual from institutional perspective: originally designed to democratize nomination, now theatrical due to low turnout and donor capture. The analytical observer sees tangled rope: genuine preference aggregation problem (how to represent diverse views in binary system) layered with asymmetric extraction (duopoly captures benefits of forced choice without bearing costs of unresponsiveness). The gap between powerless/trapped and institutional/arbitrage perspectives is maximum: one sees inescapable extraction, the other sees beneficial coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position relative to the representation gap. Left-Out Left voters are full targets: they bear the costs (voting for unrepresentative parties, suppressed policy preferences, wasted votes if they defect) and receive no benefits. The engine derives high d (approaching 1.0) from victim status plus trapped exit, producing high effective extraction. Disaffected moderates are also targets but with slightly lower d due to constrained rather than trapped exit — they have marginal influence through primaries. The two-party duopoly is full beneficiary: captures stable participation, fundraising, and institutional advantages from the gap. The engine derives low d (approaching 0.0) from beneficiary status plus arbitrage exit, producing negative effective extraction (subsidy). Third-party movements have intermediate d: they are partly victims (face structural barriers) but also partly benefit from recruiting disaffected voters, and their organized power plus constrained exit places them in moderate extraction range. The primary system has low d as institutional beneficiary with mobile exit, but its piton classification derives from theater gate rather than from directionality. Democratic legitimacy as abstract victim has maximum d (1.0) — powerless, trapped, bearing full cost with no compensation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that the representation gap is simultaneously a coordination problem and an extraction mechanism, with the balance depending on observer position. From the duopoly's perspective, the gap is pure coordination: it solves the genuine problem of aggregating diverse preferences into binary choice while maintaining stable institutional environment. From trapped voters' perspective, the gap is pure extraction: structural barriers suppress alternatives beyond what coordination requires, and the duopoly captures benefits without bearing costs. The analytical perspective sees both: there is a real preference aggregation problem (tangled rope coordination function) but the solution extracts asymmetrically from those with no exit. The mandatrophy is not 'which is it?' but 'for whom?' The constraint's claimed type (snare) reflects the analytical judgment that extraction dominates coordination when measured from the perspective of those who bear the costs. The duopoly's rope perspective is their genuine experience but does not negate the extraction experienced by trapped voters. The primary system's piton classification shows how coordination mechanisms can degrade into theater while maintaining legitimation function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    preference_intensity_threshold,
    'At what intensity of preference dissatisfaction does continued voting for unrepresentative party shift from rational lesser-evil choice to captured behavior?',
    'Longitudinal analysis of voter satisfaction scores, party loyalty, and defection rates; comparison with parliamentary systems where preference intensity can be expressed through coalition bargaining',
    'If threshold is low (mild dissatisfaction triggers exit): current 39% Left-Out Left figure represents massive extraction. If threshold is high (voters tolerate substantial dissatisfaction): figure represents normal democratic friction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preference_intensity_threshold, empirical, 'Preference intensity threshold for distinguishing rational choice from capture').

omega_variable(
    ballot_access_counterfactual,
    'Would proportional representation or ranked-choice voting eliminate the representation gap, or would new parties simply replicate the same responsiveness failures at different scale?',
    'Cross-national comparison of representation satisfaction in PR systems; analysis of new party formation and subsequent institutionalization patterns; study of ranked-choice adoption in US municipalities',
    'If structural: gap is artifact of first-past-the-post and duopoly is pure extraction. If behavioral: gap reflects deeper principal-agent problem in representative democracy and duopoly provides genuine coordination value.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ballot_access_counterfactual, empirical, 'Whether electoral system reform would eliminate representation gap').

omega_variable(
    median_voter_naturalization,
    'Is median voter theorem a descriptive model of two-party convergence or a normative justification that naturalizes the representation gap?',
    'Analysis of how median voter theorem is deployed in political science discourse; examination of whether parties actually converge to median or maintain polarized positions while claiming median; study of theory''s role in legitimating unresponsive duopoly',
    'If descriptive: theorem explains but doesn''t justify gap. If normative naturalization: theorem is false summit that treats contingent institutional arrangement as optimal equilibrium, masking extraction as efficiency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(median_voter_naturalization, conceptual, 'Whether median voter theorem naturalizes representation gap').

omega_variable(
    primary_reform_sufficiency,
    'Can primary election reforms (open primaries, ranked-choice primaries, top-two systems) restore responsiveness function, or is primary system irreversibly captured by party apparatus and donor class?',
    'Comparative analysis of primary reform outcomes in states that adopted open/ranked-choice systems; measurement of candidate responsiveness to disaffected voter preferences before and after reform; assessment of donor influence persistence',
    'If reforms work: piton perspective is too pessimistic and primary system is scaffold with real sunset. If reforms fail: piton classification confirmed and primary theater is permanent legitimation ritual.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(primary_reform_sufficiency, empirical, 'Whether primary reforms can restore responsiveness function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(representation_legitimacy_gap, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(repgap_tr_t0, representation_legitimacy_gap, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(repgap_tr_t0, observed).
narrative_ontology:measurement(repgap_tr_t10, representation_legitimacy_gap, theater_ratio, 10, 0.48).
narrative_ontology:measurement_basis(repgap_tr_t10, observed).
narrative_ontology:measurement(repgap_tr_t20, representation_legitimacy_gap, theater_ratio, 20, 0.58).
narrative_ontology:measurement_basis(repgap_tr_t20, observed).
narrative_ontology:measurement(repgap_tr_t30, representation_legitimacy_gap, theater_ratio, 30, 0.65).
narrative_ontology:measurement_basis(repgap_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(repgap_be_t0, representation_legitimacy_gap, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(repgap_be_t0, observed).
narrative_ontology:measurement(repgap_be_t10, representation_legitimacy_gap, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(repgap_be_t10, observed).
narrative_ontology:measurement(repgap_be_t20, representation_legitimacy_gap, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(repgap_be_t20, observed).
narrative_ontology:measurement(repgap_be_t30, representation_legitimacy_gap, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(repgap_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(repgap_su_t0, representation_legitimacy_gap, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(repgap_su_t0, observed).
narrative_ontology:measurement(repgap_su_t10, representation_legitimacy_gap, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(repgap_su_t10, observed).
narrative_ontology:measurement(repgap_su_t20, representation_legitimacy_gap, suppression_requirement, 20, 0.68).
narrative_ontology:measurement_basis(repgap_su_t20, observed).
narrative_ontology:measurement(repgap_su_t30, representation_legitimacy_gap, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(repgap_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(representation_legitimacy_gap, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is downstream of intra_party_fragmentation: the fragmentation of party coalitions creates the representation gap by forcing diverse preference groups into binary choice. The upstream constraint (intra_party_fragmentation) has its own extractiveness reflecting the costs of coalition maintenance; this constraint (representation_legitimacy_gap) has distinct extractiveness reflecting the costs of forced binary choice when neither option represents you.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
