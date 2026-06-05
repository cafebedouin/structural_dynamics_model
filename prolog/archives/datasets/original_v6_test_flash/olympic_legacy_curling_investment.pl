% ============================================================================
% CONSTRAINT STORY: olympic_legacy_curling_investment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-03-07
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_olympic_legacy_curling_investment, []).

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
 *   constraint_id: olympic_legacy_curling_investment
 *   human_readable: Olympic Games Legacy Investment in Curling Clubs
 *   domain: economic
 *
 * SUMMARY:
 *   Following the Winter Olympics, there is often an increase in funding for
 *   the sports featured. In the case of curling, legacy investment can
 *   disproportionately benefit elite and well-established clubs, while
 *   recreational organizations and those offering diverse sporting programs
 *   are disadvantaged. This occurs when dedicated funds are allocated to
 *   legacy investments.
 *
 * KEY AGENTS:
 *   - Elite Curling Clubs: Primary beneficiary (institutional/arbitrage) - Benefits from funding allocated directly to elite clubs.
 *   - Community Recreational Programs: Primary victim (powerless/trapped) - Community programs often see less funding.
 *   - Curling Equipment Manufacturers: Secondary Beneficiary (powerful/mobile) - Benefits from increased equipment sales.
 *   - Other Sports Funding: Secondary victim (moderate/constrained) - Sports programs that do not attract national attention.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(olympic_legacy_curling_investment, 0.55).
domain_priors:suppression_score(olympic_legacy_curling_investment, 0.4).
domain_priors:theater_ratio(olympic_legacy_curling_investment, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(olympic_legacy_curling_investment, extractiveness, 0.55).
narrative_ontology:constraint_metric(olympic_legacy_curling_investment, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(olympic_legacy_curling_investment, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(olympic_legacy_curling_investment, tangled_rope).
narrative_ontology:human_readable(olympic_legacy_curling_investment, "Olympic Games Legacy Investment in Curling Clubs").
narrative_ontology:topic_domain(olympic_legacy_curling_investment, "economic").

domain_priors:requires_active_enforcement(olympic_legacy_curling_investment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(olympic_legacy_curling_investment, elite_curling_clubs).
narrative_ontology:constraint_beneficiary(olympic_legacy_curling_investment, curling_equipment_manufacturers).
narrative_ontology:constraint_victim(olympic_legacy_curling_investment, community_recreational_programs).
narrative_ontology:constraint_victim(olympic_legacy_curling_investment, other_sports_funding).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMMUNITY RECREATIONAL PROGRAMS (SNARE) - These programs are often underfunded to begin with and rely on general grants. When dedicated funding is diverted to curling clubs, these programs are trapped with fewer resources, and limited ability to generate revenue.
constraint_indexing:constraint_classification(olympic_legacy_curling_investment, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CURLING CLUBS (TANGLED ROPE) - While the clubs may receive benefits, they're constrained by the conditions of funding. Plus, they do have to increase membership, and generate revenue. They also benefit from infrastructure upgrades. Extraction is asymmetric as some clubs located in more favorable locations will benefit, while other clubs will not.
constraint_indexing:constraint_classification(olympic_legacy_curling_investment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ELITE CURLING CLUBS (ROPE) - For elite clubs, the legacy investment provides funding for increased training and facility upgrades. There is little in the way of costs to bear. Benefit is derived from attracting better athletes, and equipment upgrades.
constraint_indexing:constraint_classification(olympic_legacy_curling_investment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CURLING EQUIPMENT MANUFACTURERS (TANGLED ROPE) - Benefits from increased sales of equipment, but also may bear some costs in terms of increased competition. These firms have considerable ability to move assets.
constraint_indexing:constraint_classification(olympic_legacy_curling_investment, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) - From the analytical perspective, legacy funding in curling is a mixed bag, as resources are diverted from some organizations to others, for the purposes of benefitting a subset of clubs. There are no natural sunset clauses.
constraint_indexing:constraint_classification(olympic_legacy_curling_investment, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(olympic_legacy_curling_investment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(olympic_legacy_curling_investment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(olympic_legacy_curling_investment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(olympic_legacy_curling_investment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(olympic_legacy_curling_investment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The funds are extracted from other sports funding programs, and allocated to legacy funding. Suppression (0.40): Moderate. Community recreational programs are at the mercy of what funding they receive. There's not much they can do if they're funding is extracted from them. Theater ratio (0.30): Low. A modest amount of performative activity happens as the allocation of resources happens through institutional means.
 *
 * PERSPECTIVAL GAP:
 *   This is a tangled rope scenario because from one perspective, we see benefits (elite curling clubs), while from another, we see extraction (community recreational programs). The analytical observer considers the investment to be a mixed bag, as some programs benefit at the expense of others.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is determined by those that benefit (elite curling clubs, curling equipment manufacturers), and those that bear the costs (community recreational programs). Elite curling clubs benefit with immediate funding and see little downside. Community programs receive less funds, and see no benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   This resolves the mandatrophy by showing that resources are extracted from community programs, to support the elite programs. The analytical observer shows this is a mixed bag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    long_term_impact,
    'What is the long-term impact of legacy investment on community curling clubs?',
    'Longitudinal study tracking membership numbers, community involvement, and the overall impact on recreational curling.',
    'If positive, the investment could be seen as a successful initiative. If negative, it could highlight the need for more balanced resource allocation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_impact, empirical, 'Long term impact of legacy investment in local curling clubs.').

omega_variable(
    alternative_allocation_models,
    'What alternative funding models could better support curling?',
    'Research into funding models and surveys of those impacted by the program.',
    'Reveals a need for more balanced resource allocation and community involvement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_allocation_models, conceptual, 'Alternative funding models for supporting recreational curling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(olympic_legacy_curling_investment, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(olym_tr_t0, olympic_legacy_curling_investment, theater_ratio, 0, 0.2).
narrative_ontology:measurement(olym_tr_t5, olympic_legacy_curling_investment, theater_ratio, 5, 0.25).
narrative_ontology:measurement(olym_tr_t10, olympic_legacy_curling_investment, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(olym_be_t0, olympic_legacy_curling_investment, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(olym_be_t5, olympic_legacy_curling_investment, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(olym_be_t10, olympic_legacy_curling_investment, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(olympic_legacy_curling_investment, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
