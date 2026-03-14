% ============================================================================
% CONSTRAINT STORY: two_party_political_polarization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_two_party_political_polarization, []).

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
 *   constraint_id: two_party_political_polarization
 *   human_readable: Two-Party Political Polarization System
 *   domain: political/institutional
 *
 * SUMMARY:
 *   Two-party political polarization in the United States represents a
 *   structural constraint that exhibits properties across all six DR types
 *   depending on the observer's position. The constraint operates through a
 *   hybrid mechanism: genuine coordination function (party organization,
 *   resource aggregation, platform clarification) coexists with asymmetric
 *   extraction (voter choice constraint, suppression of cross-party
 *   alternatives, concentration of power in party apparatus and partisan
 *   media). The extractiveness has increased substantially over the 50-year
 *   interval (0.32 to 0.58), driven by primary expansion, partisan media
 *   fragmentation, geographic sorting, and party realignment post-1968. The
 *   theater ratio has risen in parallel (0.42 to 0.68), indicating that
 *   electoral ritual (debates, campaigns, representation theater) has
 *   increased while legislative problem-solving capacity has declined. The
 *   constraint is downstream of institutional decisions
 *   (single-member-district plurality voting, open primaries, campaign
 *   finance rules) but has become self-reinforcing through identity fusion,
 *   partisan media infrastructure, and voter expectation formation.
 *
 * KEY AGENTS:
 *   - Trapped Voters: Primary victims (powerless/trapped) — face binary choice enforced through ballot access rules, partisan media gatekeeping, and social penalty for defection
 *   - Identity-Locked Partisans: Secondary victims (powerless/identity_locked) — party affiliation fused with self-concept, kinship networks, religious community, professional identity. Structurally mobile but functionally trapped.
 *   - Party Leadership Apparatus: Primary beneficiary (institutional/arbitrage) — experiences polarization as coordination mechanism that clarifies party identity and concentrates resources
 *   - Partisan Media Infrastructure: Primary beneficiary (powerful/arbitrage) — profits from binary framing and affective intensity; has arbitrage to shift partisan positioning
 *   - Cross-Party Legislators: Ambiguous position (powerful/constrained) — seek bipartisan cooperation but face party discipline penalties and primary challenge threats
 *   - Non-Partisan Civil Society: Mixed position (organized/constrained) — coordination function (bridge-building) coexists with extraction pressure (forced to choose sides)
 *   - Electoral System: Institutional inertia (institutional/arbitrage) — two-party structure maintained through ballot access rules, winner-take-all districts, campaign finance concentration
 *   - Electoral Reform Coalition: Organized exit pathway (organized/constrained) — sees ranked choice voting and proportional representation as sunset mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(two_party_political_polarization, 0.58).
domain_priors:suppression_score(two_party_political_polarization, 0.65).
domain_priors:theater_ratio(two_party_political_polarization, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(two_party_political_polarization, extractiveness, 0.58).
narrative_ontology:constraint_metric(two_party_political_polarization, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(two_party_political_polarization, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(two_party_political_polarization, tangled_rope).
narrative_ontology:human_readable(two_party_political_polarization, "Two-Party Political Polarization System").
narrative_ontology:topic_domain(two_party_political_polarization, "political/institutional").

domain_priors:requires_active_enforcement(two_party_political_polarization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(two_party_political_polarization, party_leadership_apparatus).
narrative_ontology:constraint_beneficiary(two_party_political_polarization, partisan_media_infrastructure).
narrative_ontology:constraint_beneficiary(two_party_political_polarization, political_operatives).
narrative_ontology:constraint_victim(two_party_political_polarization, voter_deliberative_capacity).
narrative_ontology:constraint_victim(two_party_political_polarization, policy_effectiveness).
narrative_ontology:constraint_victim(two_party_political_polarization, legislative_problem_solving).
narrative_ontology:constraint_victim(two_party_political_polarization, cross_party_coalition_formation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED VOTER (SNARE) — Individual voter perceives two rigid options, each with penalties for defection. Switching party allegiance carries social cost, identity loss, and epistemic isolation. No exit option that preserves community standing and information access. Bears full cost of polarization through constrained choice set and degraded information environment.
constraint_indexing:constraint_classification(two_party_political_polarization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: IDENTITY-LOCKED PARTISAN (SNARE) — Voter whose political identity is constitutive of self-concept and group belonging. Party affiliation fused with professional identity, kinship networks, religious community, or ideological worldview. Exit would require abandoning identity frame, not just changing ballot choice. Structurally mobile but functionally trapped by internalized framing. Perceives constraint as immutable because the identity frame prevents recognizing alternatives.
constraint_indexing:constraint_classification(two_party_political_polarization, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: PARTY LEADERSHIP (ROPE) — Party apparatus experiences polarization as coordination mechanism: rallies base, clarifies platform, enables resource aggregation. Leadership has arbitrage exit (can shift coalition partners, reposition, negotiate). Genuinely benefits from clear partisan distinctions. Low experienced extraction — this actor experiences the constraint as beneficial coordination that concentrates resources and clarifies party identity.
constraint_indexing:constraint_classification(two_party_political_polarization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CROSS-PARTY LEGISLATOR (TANGLED ROPE) — Individual legislator seeking bipartisan coalition. Constrains: party discipline, primary election threats, partisan media attack. Benefits: access to power, legislative positions, committee assignments. Mixed experience — genuine coordination function within party but asymmetric extraction through party enforcement. Constrained exit (high cost of party defection but structurally possible).
constraint_indexing:constraint_classification(two_party_political_polarization, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: NON-PARTISAN CIVIL SOCIETY (TANGLED ROPE) — Organizations (civic groups, business coalitions, faith communities) attempting to bridge polarization. Constrained: pressure to take sides, donor expectations, media framing. Benefits: access to constituencies, legitimacy in both communities. Coordination function (building bridges) coexists with extraction (forced choice). Constrained exit — structural mobility but high cost to neutrality and resource access.
constraint_indexing:constraint_classification(two_party_political_polarization, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ELECTORAL SYSTEM (PITON) — Two-party structure persists through institutional inertia: ballot access rules, winner-take-all districts, campaign finance concentration, media gatekeeping all lock in two-party competition. The system maintains theater (debate, elections, representation) while its coordination function (aggregating diverse preferences into governing coalitions) has atrophied. Theater ratio high because electoral ritual persists despite diminishing genuine deliberation.
constraint_indexing:constraint_classification(two_party_political_polarization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ELECTORAL REFORM COALITION (SCAFFOLD) — Organized agents (Common Cause, election scientists, reform states) see polarization as temporary dysfunction with architectural solution: ranked choice voting, open primaries, proportional representation. Sunset clause: new electoral mechanisms bypass binary choice enforcement. Constrained exit (building political will takes decades) but clear pathway. Low theater because structural solution addresses root cause rather than managing symptoms.
constraint_indexing:constraint_classification(two_party_political_polarization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / DUVERGER'S LAW (MOUNTAIN) — From civilizational scale, single-member-district plurality voting mathematically converges to two-party equilibrium (Duverger's Law). Polarization appears as immutable consequence of electoral geometry. However, structural data contradicts: many two-party systems have lower polarization, and history shows polarization emerged only after institutional changes (party realignment, primary expansion, media fragmentation). False summit — naturalizes institutional choices as laws of mathematics.
constraint_indexing:constraint_classification(two_party_political_polarization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(two_party_political_polarization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(two_party_political_polarization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(two_party_political_polarization, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(two_party_political_polarization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(two_party_political_polarization, TR),
    TR >= 0.70.

:- end_tests(two_party_political_polarization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing. Base value reflects multiple extraction mechanisms: binary choice enforcement, suppression of third-party/independent alternatives, voter attention captured by identity signaling rather than policy analysis, and concentration of power in party apparatus. The 30-year rise from 0.32 to 0.58 reflects institutional changes that have amplified extraction: primary expansion (1972+) shifted power to ideologically intense base voters; cable news fragmentation (1990s) created profit incentive for affective intensity; geographic sorting (2000-2020) reduced cross-party contact; social media algorithms (2010+) amplified partisan content. Suppression (0.65): Moderate-high. Significant barriers to exit include: no viable third-party pathway (ballot access, winner-take-all geometry), social penalty for party switching (family/community expectations), informational isolation through partisan media environment, identity fusion that makes switching feel like self-betrayal. But suppression is not absolute — some voters do switch, independent candidates win races, and information bridging is possible. Theater ratio (0.68): High and rising. Electoral theater (primary debates, general election campaigns, congressional rituals) has expanded while legislative problem-solving has contracted. The system maintains appearance of choice and representation while actual cross-party coalition formation has declined. Primary campaigns and social media create intense performance environment that substitutes for genuine policy deliberation.
 *
 * PERSPECTIVAL GAP:
 *   The central perspectival gap is between party leadership (who see benefit and coordination) and trapped/identity-locked voters (who see extraction and immobility). Leadership benefits from clear partisan distinction and resource concentration; voters bear cost through constrained choice and identity enforcement. Legislators attempting bipartisan cooperation experience the constraint as Tangled Rope (mixed benefit/extraction); leadership experiences it as pure Rope (coordination). Non-partisan organizations experience genuine tension between coordination function (they want to bridge) and extraction (they are forced to choose). Electoral reformers reject the naturalizing frame — they see Scaffold, not Mountain. The analytical observer risks false summit by accepting Duverger's Law as necessity; the structural data shows this is contingent institutional choice, not mathematical law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values vary dramatically by structural position. Party leadership with arbitrage exit options derives low d (0.15-0.25) — they benefit and can reposition. Trapped voters with no exit derive high d (0.90-0.98) — they bear full extraction cost. Identity-locked voters derive very high d (0.92-0.99) despite biographical mobility — their identity lock prevents exercising structural exit options. Organized reformers with constrained exit derive moderate d (0.50-0.65) — they have agency and see pathway out but face high barriers. Legislative actors derive moderate-high d (0.55-0.75) — constrained by party discipline but with some negotiating power. Cross-party median voters who could theoretically switch but are informationally isolated derive high d (0.70-0.85) relative to their power level. The derivation chain produces higher χ values for powerless and trapped agents, lower values for institutional beneficiaries, and mixed values for ambiguous positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits genuine Tangled Rope properties: (1) coordination function exists and is real — parties do aggregate preferences, organize resources, and clarify platforms; (2) asymmetric extraction exists and is real — voters are constrained to binary choice, independents are suppressed through ballot access rules, cross-party alternatives are systematically disadvantaged; (3) both functions coexist structurally — the party system cannot drop the coordination without losing institutional coherence, and voters cannot escape the extraction without losing access to political power or community. The mandatrophy is resolved by rejecting the false binary: polarization is neither 'just natural party competition' (rope naturalization) nor 'pure voter imprisonment' (snare overstatement). It is hybrid extraction with coordination function. The analytical observer's mountain classification (Duverger's Law makes this inevitable) is a false summit: many two-party systems maintain lower polarization, and polarization increased dramatically after specific institutional changes (primaries, media fragmentation) not after foundational electoral rules. The natural law framing obscures the contingent choices (campaign finance rules, primary structure, media gatekeeping) that have amplified extraction over time. Mandatrophy resolved: the constraint is correctly classified as Tangled Rope, not as natural law or pure coordination or pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    polarization_cause_endogenous_exogenous,
    'Is polarization driven endogenously by two-party institutional structure or exogenously by social fragmentation that the system then amplifies?',
    'Cross-national comparison: two-party systems with low polarization (Canada, Australia) vs multi-party systems with high polarization (Israel, Belgium). Analysis of polarization timeline relative to institutional reforms vs demographic/economic shifts.',
    'If endogenous: institutional reform (ranked choice) is necessary and sufficient. If exogenous: two-party system is secondary effect — polarization persists even after electoral reform unless underlying social fragmentation is addressed. Affects classification: if exogenous, the constraint is a symptom rather than a root cause.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(polarization_cause_endogenous_exogenous, empirical, 'Whether polarization is driven by two-party structure or deeper social fragmentation').

omega_variable(
    party_leadership_agency_vs_constraint,
    'Do party leaders actively drive polarization as strategy, or are they constrained by base expectations and incentive structures created by primary elections and partisan media?',
    'Historical analysis of party elite messaging 1950-2024; correlation between primary cycle activation and partisan messaging intensity; interviews with party strategists and media executives; analysis of leadership messaging in high-constraint vs low-constraint periods.',
    'If active strategy: party leadership is primary beneficiary and perpetrator. If constrained: institutional structure (primaries, media fragmentation) is driver, and leadership operates within inherited constraints. Affects directionality and mandatrophy analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(party_leadership_agency_vs_constraint, empirical, 'Whether party leaders actively drive polarization or are constrained by institutional structure').

omega_variable(
    voter_agency_vs_identity_lock,
    'To what extent is voter polarization a rational response to genuine policy divergence vs identity-locked partisan affiliation decoupled from policy preference?',
    'Surveys measuring: policy preference alignment with stated party choice; cross-party agreement on problem definition; willingness to switch parties given identical policy platform; measurement of identity strength independent of policy agreement; analysis of voting behavior during primary contests with policy variation within party.',
    'If rational: polarization reflects genuine disagreement addressable through deliberation. If identity-locked: institutional exit mechanisms (ranked choice) may not reduce polarization because identity frame prevents switching even when policy preference changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voter_agency_vs_identity_lock, empirical, 'Whether voter polarization reflects policy disagreement or identity-locked partisan affiliation').

omega_variable(
    suppression_mechanism_structural_vs_informational,
    'Is suppression of cross-party coalition formation structural (no viable institutional pathway) or informational (voters lack accurate information about cross-party support)?',
    'Voter knowledge surveys: what percentage can accurately identify cross-party voters on specific issues? Experimental intervention testing: provide accurate cross-party coalition data and measure willingness to defect. Historical analysis of successful cross-party coalitions and barriers faced.',
    'If structural: institutional reform required. If informational: media environment and deliberative space reform sufficient. Affects theories of intervention effectiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_informational, empirical, 'Whether suppression of cross-party coalitions is structural or informational').

omega_variable(
    mandatrophy_two_party_coordination_function,
    'Does the two-party system retain genuine coordination function in aggregating preferences and producing stable governance, or has this function fully atrophied into extraction and theater?',
    'Measurement of legislative productivity, bill passage rate, and cross-party cooperation frequency 1950-2024. Analysis of voter satisfaction with representation. Comparison with multi-party systems on same metrics. Analysis of whether two-party system produces more or less policy volatility than proportional systems.',
    'If coordination persists: constraint is Tangled Rope or Rope. If fully atrophied: constraint is Snare or Piton. Determines mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_two_party_coordination_function, empirical, 'Whether two-party system retains genuine coordination function or has become purely extractive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(two_party_political_polarization, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tppp_tr_t0, two_party_political_polarization, theater_ratio, 0, 0.42).
narrative_ontology:measurement(tppp_tr_t15, two_party_political_polarization, theater_ratio, 15, 0.55).
narrative_ontology:measurement(tppp_tr_t30, two_party_political_polarization, theater_ratio, 30, 0.68).
narrative_ontology:measurement(tppp_tr_t45, two_party_political_polarization, theater_ratio, 45, 0.76).

% Extraction over time
narrative_ontology:measurement(tppp_be_t0, two_party_political_polarization, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(tppp_be_t15, two_party_political_polarization, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(tppp_be_t30, two_party_political_polarization, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(tppp_be_t45, two_party_political_polarization, base_extractiveness, 45, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(two_party_political_polarization, enforcement_mechanism).
narrative_ontology:affects_constraint(two_party_political_polarization, primary_election_radicalization).
narrative_ontology:affects_constraint(two_party_political_polarization, partisan_media_profit_incentive).
narrative_ontology:affects_constraint(two_party_political_polarization, third_party_ballot_access).
narrative_ontology:affects_constraint(two_party_political_polarization, geographic_political_sorting).
narrative_ontology:affects_constraint(two_party_political_polarization, social_media_algorithmic_fragmentation).

% DUAL FORMULATION NOTE:
% Two-party polarization is upstream of multiple downstream constraints: primary elections amplify base voter preferences, partisan media profits from affective intensity, ballot access rules enforce binary choice, geographic sorting concentrates political homogeneity, and social media algorithms segment audiences. Each downstream constraint has higher extractiveness (more contingent on specific institutional choices) but would not be self-reinforcing without the parent constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(two_party_political_polarization, powerful, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
