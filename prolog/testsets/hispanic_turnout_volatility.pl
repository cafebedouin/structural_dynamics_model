% ============================================================================
% CONSTRAINT STORY: hispanic_turnout_volatility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hispanic_turnout_volatility, []).

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
 *   constraint_id: hispanic_turnout_volatility
 *   human_readable: Hispanic Turnout Volatility in U.S. Electoral Politics
 *   domain: political_science/electoral_dynamics
 *
 * SUMMARY:
 *   Hispanic turnout volatility in U.S. electoral politics represents a
 *   structural constraint where campaign incentives and voter participation
 *   dynamics create an asymmetric extraction pattern masked by the rhetoric
 *   of political inclusion. The volatility itself — the sharp swings in
 *   Hispanic voter turnout across electoral cycles — is not a natural
 *   demographic property but a consequence of how political campaigns
 *   allocate resources and relationship investments. High Hispanic turnout
 *   occurs in competitive swing states during presidential elections when
 *   campaigns mobilize resources; turnout collapses in off-years and
 *   non-competitive states when campaign attention disappears. This cycle
 *   extracts from Hispanic communities (who bear the infrastructure and
 *   organizing costs of mobilization) while benefiting campaigns (who capture
 *   electoral gains) and media/analysts (who profit from swing-voter
 *   unpredictability narratives). The constraint exhibits genuine
 *   coordination functions (voter mobilization networks enable political
 *   participation) alongside extraction (resources are transactional and
 *   episodic rather than relationship-building). Emerging organized Hispanic
 *   political coalitions represent a scaffold structure — temporary support
 *   for a transition from swing-voter volatility to sustained political
 *   infrastructure with its own agenda rather than dependence on campaign
 *   cycles.
 *
 * KEY AGENTS:
 *   - Hispanic Voter Collective: Primary victim (powerless/trapped) — bears costs of volatility (repeated mobilization efforts, infrastructure building that disappears, transactional campaign relationships)
 *   - Hispanic Community Organizations: Secondary victim (moderate/constrained) — coordinate voter mobilization but face funding volatility tied to electoral cycles; can exit toward non-electoral organizing but at organizational cost
 *   - Democratic Campaign Machine: Primary beneficiary (institutional/arbitrage) — benefits from swing-state volatility; can shift resources between Hispanic and other demographics
 *   - Republican Campaign Machine: Secondary beneficiary (institutional/arbitrage) — similar arbitrage position, though often lower investment in Hispanic outreach
 *   - Swing State Campaigns: Beneficiary (powerful/mobile) — concentrate resources on Hispanic turnout in competitive states; experience constraint as coordination mechanism
 *   - Media and Political Analysts: Secondary beneficiary (institutional/arbitrage) — profit from swing-voter unpredictability narratives; volatility is valuable content
 *   - Organized Hispanic Political Coalitions: Organized counterweight (organized/constrained) — building alternative infrastructure with sunset logic toward sustained political power independent of campaign cycles
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing volatility as demographic property rather than institutional creation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hispanic_turnout_volatility, 0.58).
domain_priors:suppression_score(hispanic_turnout_volatility, 0.62).
domain_priors:theater_ratio(hispanic_turnout_volatility, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hispanic_turnout_volatility, extractiveness, 0.58).
narrative_ontology:constraint_metric(hispanic_turnout_volatility, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(hispanic_turnout_volatility, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hispanic_turnout_volatility, tangled_rope).
narrative_ontology:human_readable(hispanic_turnout_volatility, "Hispanic Turnout Volatility in U.S. Electoral Politics").
narrative_ontology:topic_domain(hispanic_turnout_volatility, "political_science/electoral_dynamics").

domain_priors:requires_active_enforcement(hispanic_turnout_volatility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hispanic_turnout_volatility, candidate_campaigns_targeting_hispanics).
narrative_ontology:constraint_beneficiary(hispanic_turnout_volatility, political_analysts_and_consultants).
narrative_ontology:constraint_beneficiary(hispanic_turnout_volatility, media_outlets_covering_swing_voters).
narrative_ontology:constraint_victim(hispanic_turnout_volatility, hispanic_voter_collective_political_agency).
narrative_ontology:constraint_victim(hispanic_turnout_volatility, electoral_predictability_and_system_stability).
narrative_ontology:constraint_victim(hispanic_turnout_volatility, hispanic_community_coalition_building).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HISPANIC VOTER COLLECTIVE (SNARE) — Structurally trapped in a volatile positioning where campaign attention spikes during election cycles but institutional investment in Hispanic political infrastructure remains minimal. No exit from the volatility pattern without fundamental political reorganization. Bears full cost of electoral unpredictability — resources flow toward mobilization in swing states but drain in non-competitive cycles. Cannot organize exit.
constraint_indexing:constraint_classification(hispanic_turnout_volatility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HISPANIC COMMUNITY ORGANIZATIONS (TANGLED ROPE) — Experience genuine coordination function (voter mobilization networks, community engagement infrastructure) alongside asymmetric extraction (resources concentrate on swing-state mobilization in presidential years, disappear in off-years; candidate attention is transactional rather than relationship-building). Constrained by funding cycles and electoral calendar. Can exit toward non-electoral organizing but at high organizational cost.
constraint_indexing:constraint_classification(hispanic_turnout_volatility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DEMOCRATIC CAMPAIGN APPARATUS (ROPE) — Experiences the constraint as pure coordination: high Hispanic turnout in swing states delivers electoral victory. The volatility is strategically valuable — it creates swing-state dynamics that justify resource concentration and campaign focus. Net beneficiary with arbitrage options (can shift resources between demographic targets). Minimal extraction cost.
constraint_indexing:constraint_classification(hispanic_turnout_volatility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SWING STATE CAMPAIGNS (TANGLED ROPE) — Benefit from Hispanic turnout volatility as a mechanism to mobilize resources and candidate attention toward competitive electoral margins. Genuine coordination function: swing-state campaigns enable Hispanic political engagement and candidate responsiveness. Asymmetric extraction: campaign relationship is transactional and episodic. Exit options exist (state-level parties can redirect toward non-Hispanic demographics) but turbulent.
constraint_indexing:constraint_classification(hispanic_turnout_volatility, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: DEMOGRAPHIC REPRESENTATION RITUAL (PITON) — The rhetoric of 'Hispanic voters as crucial swing demographic' persists as institutional theater despite declining empirical foundation: Hispanic voter blocs in some swing states are stabilizing, economic heterogeneity within Hispanic communities is increasing, and single-issue mobilization is losing effectiveness. The ritual of campaign outreach to Hispanic voters is performed by candidates and media regardless of structural impact. Theater ratio high because the performative consensus (Hispanic voters = swing voters = crucial target) exceeds actual predictive power.
constraint_indexing:constraint_classification(hispanic_turnout_volatility, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ORGANIZED HISPANIC POLITICAL COALITIONS (SCAFFOLD) — Emerging coordination pathway with sunset mechanism: permanent voter registration drives, local ballot measure organizing, and legislative engagement create infrastructure that reduces turnout volatility by building non-electoral incentives for participation. Scaffold classification reflects temporary support structure for a transition from swing-voter volatility to sustained political organization. Has sunset clause: as coalition infrastructure matures, the volatility mechanism loses structural force.
constraint_indexing:constraint_classification(hispanic_turnout_volatility, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — At civilizational scale, demographic heterogeneity and evolving immigration policy create inherent volatility in electoral preference aggregation across Hispanic subpopulations. No political system can eliminate this variation — it is built into the heterogeneity of the population itself. However, this perspective risks naturalizing what is actually institutional volatility (campaign resource allocation) rather than underlying demographic volatility.
constraint_indexing:constraint_classification(hispanic_turnout_volatility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hispanic_turnout_volatility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hispanic_turnout_volatility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hispanic_turnout_volatility, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hispanic_turnout_volatility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hispanic_turnout_volatility, TR),
    TR >= 0.70.

:- end_tests(hispanic_turnout_volatility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58): Moderate-high. The constraint extracts from Hispanic communities through transactional campaign relationships and volatile resource allocation. The extraction is not maximal (0.70+) because genuine coordination functions exist — voter mobilization networks provide real political engagement pathways — and some communities have developed counter-organizing capacity. The value reflects that mobilization enables participation but within a structure designed for campaign benefit. Suppression (0.62): Moderate-high. Significant barriers to sustained independent organizing include funding cycles tied to elections, structural incentives for campaign dependence, and historical patterns of broken promises between candidates and communities. But suppression is not total — coalition-building demonstrates organizational capacity and agency. Theater ratio (0.65): Moderate-high. Campaign rhetoric around 'Hispanic outreach' and 'reaching Latino voters' has become substantially performative. Actual policy follow-through is episodic. Media coverage treats Hispanic swing voters as a category requiring strategic attention rather than engaging with specific policy demands. Theater increased over the interval as both parties developed more sophisticated demographic targeting rhetoric while actual relationship investment remained transactional.
 *
 * PERSPECTIVAL GAP:
 *   Campaigns experience the constraint as rope (pure coordination: mobilizing Hispanic voters solves the electoral turnout problem). Hispanic voters experience snare (extraction: campaign attention and resources vanish in off-years despite community organizational costs). Organized coalitions experience scaffold (temporary coordination with sunset: coalition infrastructure is building a transition toward sustained political power independent of campaign cycles). The piton perspective reveals that the campaign demographic rhetoric has become decoupled from actual strategic behavior — the language of 'Hispanic voters' is performed even when actual campaign strategy targets specific swing states rather than Hispanic communities broadly.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies by agent position: Hispanic voters experience high d (0.85+) — they are structural targets of extraction with limited exit options; trapped status produces maximum f(d). Campaign apparatus experiences low d (0.15-0.25) — they are beneficiaries with arbitrage options; arbitrage exit produces negative or minimal f(d). Community organizations experience moderate d (0.55-0.65) — they benefit from mobilization infrastructure but bear resource volatility; constrained exit produces moderate f(d). Swing-state campaigns experience low-moderate d (0.35-0.45) — they benefit from volatility but also depend on it; mobile exit options reduce experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through the scaffold perspective: the apparent tension between 'is this pure coordination (rope) or pure extraction (snare)?' is resolved by recognizing that both are true at different time scales and for different actors. At the campaign time scale (immediate), the constraint operates as rope coordination (campaign investment → voter mobilization). At the community time scale (biographical), it operates as snare extraction (community resources → transactional campaign relationships → infrastructure collapse in off-years). The scaffold perspective reveals that the constraint's function is being displaced: organized coalition-building creates alternative coordination infrastructure (voter registration, local ballot measures, sustained legislative engagement) that does not depend on campaign cycles. As this infrastructure matures, the volatility mechanism loses structural force — candidates cannot extract transactional attention without relationship costs. The mandatrophy is resolved not by choosing a single type but by recognizing the constraint's decomposition into multiple time-scale-dependent functions that can be restructured through political organizing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    swing_state_definition_instability,
    'As demographic shifts change which states are competitive, does the volatility mechanism itself become obsolete, or does it migrate to a different set of swing states?',
    'Longitudinal analysis of swing-state definition stability across 3-4 presidential cycles; tracking whether campaign resource allocation follows volatility or creates it',
    'If mechanism migrates: volatility is structural and persistent (extraction component dominates). If mechanism weakens: scaffold sunset logic is correct (coordination component can replace extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(swing_state_definition_instability, empirical, 'Whether swing-state definition remains stable across electoral cycles').

omega_variable(
    hispanic_political_cohesion_trajectory,
    'Is internal Hispanic political heterogeneity (Cuban vs Mexican vs Puerto Rican, immigrant vs native-born, class differences) increasing or decreasing? Does increased heterogeneity increase or decrease turnout volatility?',
    'Voting pattern analysis by Hispanic subgroup; correlation between subgroup political divergence and aggregate turnout volatility; survey data on within-Hispanic coalition strength',
    'If heterogeneity is increasing: volatility may be structural and persistent, reducing scaffold sunset likelihood. If cohesion is increasing: scaffold coalition-building can succeed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hispanic_political_cohesion_trajectory, empirical, 'Whether Hispanic political heterogeneity is increasing or consolidating').

omega_variable(
    campaign_extraction_substitution,
    'As some Hispanic communities stabilize in voting preference (non-swing dynamics), do campaigns shift extraction to remaining Hispanic swing populations, intensifying volatility for a shrinking base?',
    'Time-series analysis of campaign spending by Hispanic density and electoral competitiveness; modeling of extraction intensity as swing-state set shrinks',
    'If substitution occurs: extraction mechanism intensifies for volatile remainder (snare component strengthens). If extraction is proportional: snare mechanism is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(campaign_extraction_substitution, empirical, 'Whether campaigns concentrate extraction on remaining swing Hispanic populations').

omega_variable(
    organizational_infrastructure_persistence,
    'Do Hispanic voter mobilization organizations built during high-turnout cycles persist during low-turnout cycles, or do they dissolve, forcing repeated infrastructure rebuilding?',
    'Tracking of organization survival rates and funding availability across electoral cycles; interview data on organizational capacity continuity',
    'If persistent: scaffold can succeed in building sustained infrastructure. If dissolved: volatility mechanism self-replicates (each cycle requires new mobilization, reinforcing transactional relationship).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(organizational_infrastructure_persistence, empirical, 'Whether grassroots Hispanic political organizations survive electoral downturns').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hispanic_turnout_volatility, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(htv_tr_t0, hispanic_turnout_volatility, theater_ratio, 0, 0.48).
narrative_ontology:measurement(htv_tr_t4, hispanic_turnout_volatility, theater_ratio, 4, 0.62).
narrative_ontology:measurement(htv_tr_t8, hispanic_turnout_volatility, theater_ratio, 8, 0.65).
narrative_ontology:measurement(htv_tr_t2, hispanic_turnout_volatility, theater_ratio, 2, 0.55).
narrative_ontology:measurement(htv_tr_t6, hispanic_turnout_volatility, theater_ratio, 6, 0.64).

% Extraction over time
narrative_ontology:measurement(htv_be_t0, hispanic_turnout_volatility, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(htv_be_t4, hispanic_turnout_volatility, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(htv_be_t8, hispanic_turnout_volatility, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(htv_be_t2, hispanic_turnout_volatility, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(htv_be_t6, hispanic_turnout_volatility, base_extractiveness, 6, 0.56).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hispanic_turnout_volatility, attachment_coordination).
narrative_ontology:affects_constraint(hispanic_turnout_volatility, immigration_policy_electoral_salience).
narrative_ontology:affects_constraint(hispanic_turnout_volatility, swing_state_campaign_resource_concentration).
narrative_ontology:affects_constraint(hispanic_turnout_volatility, hispanic_coalition_durability).

% DUAL FORMULATION NOTE:
% Hispanic turnout volatility decomposes into two structurally distinct constraints: (1) campaign_resource_volatility (ε≈0.65, extraction mechanism) — the allocation of campaign resources to Hispanic communities is episodic and tied to swing-state competitiveness; (2) hispanic_organizational_capacity (ε≈0.35, coordination mechanism) — the infrastructure for sustained voter mobilization and political organization independent of campaigns. These are linked but distinct. Upstream constraint: swing_state_structural_importance. Downstream constraints: hispanic_political_infrastructure_sustainability, electoral_participation_inequality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hispanic_turnout_volatility, powerful, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
