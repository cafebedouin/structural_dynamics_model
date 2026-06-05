% ============================================================================
% CONSTRAINT STORY: mltt_economic_model
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mltt_economic_model, []).

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
 *   constraint_id: mltt_economic_model
 *   human_readable: Major League Table Tennis Economic Model
 *   domain: economic
 *
 * SUMMARY:
 *   The Major League Table Tennis (MLTT) economic model represents the
 *   organizational and economic structure of the newly formed sports league.
 *   The model exhibits characteristics of a Tangled Rope, balancing
 *   coordination (providing entertainment, creating jobs) with extraction
 *   (potential player exploitation, burden on local communities). The league
 *   extracts from players through contract restrictions and a top-heavy
 *   revenue distribution, while it extracts from local communities through
 *   infrastructure demands and potential disruption. Beneficiaries include
 *   the MLTT organization, which controls the league and its revenue streams,
 *   and team owners who profit from ticket sales, sponsorships, and
 *   broadcasting rights.
 *
 * KEY AGENTS:
 *   - MLTT Organization: Primary beneficiary (institutional/arbitrage) – controls the league, revenue streams, and can make structural changes.
 *   - Team Owners: Secondary beneficiary (moderate/mobile) – invest capital, manage operations, and profit from league popularity and revenue.
 *   - Players: Primary target (powerless/constrained) – limited bargaining power, career dependent on league participation.
 *   - Local Communities: Secondary target (moderate/constrained) – bear infrastructure strain and social effects.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mltt_economic_model, 0.55).
domain_priors:suppression_score(mltt_economic_model, 0.4).
domain_priors:theater_ratio(mltt_economic_model, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mltt_economic_model, extractiveness, 0.55).
narrative_ontology:constraint_metric(mltt_economic_model, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(mltt_economic_model, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mltt_economic_model, tangled_rope).
narrative_ontology:human_readable(mltt_economic_model, "Major League Table Tennis Economic Model").
narrative_ontology:topic_domain(mltt_economic_model, "economic").

domain_priors:requires_active_enforcement(mltt_economic_model).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mltt_economic_model, mltt_organization).
narrative_ontology:constraint_beneficiary(mltt_economic_model, team_owners).
narrative_ontology:constraint_victim(mltt_economic_model, players).
narrative_ontology:constraint_victim(mltt_economic_model, local_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of a player with limited bargaining power and few alternative professional opportunities. Their income and career depend on adhering to the league's rules and performing well, giving them limited mobility.
constraint_indexing:constraint_classification(mltt_economic_model, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% Perspective of the league's organizational body. Benefits from the system by controlling the rules, attracting investment, and generating revenue through broadcasting rights, sponsorships, and ticket sales. Has arbitrage options by changing the league structure and making other structural changes to benefit the organization.
constraint_indexing:constraint_classification(mltt_economic_model, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Perspective of team owners who invest capital and manage operations. They benefit from the league's popularity and revenue, but also bear the risk of financial losses and have limited exit options once invested. They can change teams but are constrained by league structure.
constraint_indexing:constraint_classification(mltt_economic_model, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% Analytical perspective considering the long-term implications and global context of the MLTT economic model. Recognizes both the coordination aspects (entertainment, job creation) and extractive aspects (player exploitation, local community burden).
constraint_indexing:constraint_classification(mltt_economic_model, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mltt_economic_model_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mltt_economic_model, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mltt_economic_model, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mltt_economic_model, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mltt_economic_model_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. MLTT extracts value from its players through performance demands, contract restrictions, and limited bargaining power, while also extracting value from local communities through event hosting demands. Suppression (0.40): Moderate. Players face constraints in exiting the league due to limited professional alternatives. Local communities have limited power to negotiate terms due to economic incentives. Theater ratio (0.30): Low. The organization is actively trying to engage in non-theatrical brand bulding.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives reveal a perspectival gap in the MLTT economic model. The MLTT organization views it as a pure coordination mechanism (Rope), enabling professional table tennis at a national level. Team owners view it as a mix of coordination and extraction (Tangled Rope), balancing their investment with potential returns and constraints. The individual player views it as a form of extraction (Snare), where they are bound by contract and dependent on the league's success. The analytical observer sees both coordination and extraction (Tangled Rope), acknowledging the economic benefits but also recognizing the potential for exploitation.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values are determined by the structural position of each agent. The MLTT organization, as the primary beneficiary with arbitrage options, experiences low extraction. Team owners, with moderate power and exit options, experience moderate extraction. Players, with limited power and constrained exit options, experience high extraction. Local communities, with limited power and constrained exit options, experience moderate extraction. The analytical observer perspective captures the overall balance of coordination and extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The MLTT model has a substantial coordination component by creating a league where one didn't exist. At first glance it looks extractive, but a deeper analysis shows a positive-sum framework exists, albeit with skewed power and compensation. It is likely some players would not otherwise have the same opportunities. The analysis also shows clear benefit to local communities through economic activity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    player_compensation_fairness,
    'Is the compensation structure for players fair relative to the revenue generated by the league?',
    'Detailed analysis of player salaries, benefits, and revenue distribution compared to other professional sports leagues.',
    'If compensation is deemed unfair, the model might be reclassified as a pure Snare for players, leading to potential labor disputes and reputational damage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(player_compensation_fairness, empirical, 'Whether player compensation is fair relative to league revenue.').

omega_variable(
    local_community_impact,
    'What is the net impact of MLTT events on local communities, considering both economic benefits and potential negative externalities?',
    'Comprehensive study of economic impact, infrastructure strain, and social effects on host communities.',
    'If the net impact is negative, the league''s social license to operate might be jeopardized, leading to community opposition and regulatory challenges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(local_community_impact, empirical, 'The net impact of MLTT events on local communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mltt_economic_model, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mltt_tr_t0, mltt_economic_model, theater_ratio, 0, 0.2).
narrative_ontology:measurement(mltt_tr_t5, mltt_economic_model, theater_ratio, 5, 0.3).
narrative_ontology:measurement(mltt_tr_t10, mltt_economic_model, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(mltt_be_t0, mltt_economic_model, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(mltt_be_t5, mltt_economic_model, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(mltt_be_t10, mltt_economic_model, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mltt_economic_model, resource_allocation).
narrative_ontology:affects_constraint(mltt_economic_model, professional_sports_economics).
narrative_ontology:affects_constraint(mltt_economic_model, sports_entertainment_industry).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
