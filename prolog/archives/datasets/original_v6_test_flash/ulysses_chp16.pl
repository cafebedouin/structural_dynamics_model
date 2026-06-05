% ============================================================================
% CONSTRAINT STORY: ulysses_chp16
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_chp16, []).

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
 *   constraint_id: ulysses_chp16
 *   human_readable: The Exhausted Coordination (Cabman's Shelter)
 *   domain: social/economic/linguistic
 *
 * SUMMARY:
 *   In chapter 16 of Ulysses, Bloom and Dedalus seek refuge in a cabman's
 *   shelter, aiming to escape the Dublin night and engage in some form of
 *   shared sustenance. However, the shelter, meant to be a place of refuge
 *   and coordination, becomes a space of social awkwardness, economic
 *   exchange, and linguistic barriers, presenting a complex interaction
 *   between coordination and extraction.
 *
 * KEY AGENTS:
 *   - Cabmen's Shelter Society: Primary beneficiary (institutional/arbitrage)
 *   - Cabmen: Secondary beneficiary (powerful/mobile)
 *   - Leopold Bloom: Primary victim (powerless/trapped)
 *   - Stephen Dedalus: Secondary victim (powerless/trapped)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp16, 0.6).
domain_priors:suppression_score(ulysses_chp16, 0.3).
domain_priors:theater_ratio(ulysses_chp16, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp16, extractiveness, 0.6).
narrative_ontology:constraint_metric(ulysses_chp16, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(ulysses_chp16, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp16, tangled_rope).
narrative_ontology:human_readable(ulysses_chp16, "The Exhausted Coordination (Cabman's Shelter)").
narrative_ontology:topic_domain(ulysses_chp16, "social/economic/linguistic").

domain_priors:requires_active_enforcement(ulysses_chp16).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp16, cabmen_shelter_society).
narrative_ontology:constraint_beneficiary(ulysses_chp16, cabmen).
narrative_ontology:constraint_victim(ulysses_chp16, bloom).
narrative_ontology:constraint_victim(ulysses_chp16, dedalus).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Bloom experiences the shelter as a snare because his attempts to connect with others in the shelter are awkward and isolating.
constraint_indexing:constraint_classification(ulysses_chp16, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% Dedalus experiences the shelter as a snare because he is exhausted and feels alienated from the working-class men inside.
constraint_indexing:constraint_classification(ulysses_chp16, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% The Cabmen's Shelter Society views the shelter as a rope, providing a necessary service and a safe haven for cabmen.
constraint_indexing:constraint_classification(ulysses_chp16, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(local))).

% The cabmen experience the shelter as a mix of coordination and extraction; they gain from the shelter but also must adhere to its rules and norms.
constraint_indexing:constraint_classification(ulysses_chp16, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% The analytical observer classifies the cabman's shelter as a tangled rope, serving both a coordination and extraction function.
constraint_indexing:constraint_classification(ulysses_chp16, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp16_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp16, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp16, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ulysses_chp16, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ulysses_chp16_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): The cabman's shelter extracts from those using it through economic exchange (paying for food and drink) and adherence to social norms. Bloom and Dedalus experience this extractiveness more acutely because they are outsiders. Suppression (0.3): Suppression is relatively low. While there are social expectations within the shelter, individuals are not forcibly coerced to conform. Theater Ratio (0.4): The theater ratio is moderate, the shelter serves a genuine function, while the environment may contribute to creating a social environment that has different performative implications depending on structural position. .
 *
 * PERSPECTIVAL GAP:
 *   Bloom and Dedalus experience the shelter primarily as a snare due to their social isolation and the awkwardness of their interactions. The Cabmen's Shelter Society, on the other hand, sees it as a rope, a valuable service for its members. The cabmen experience a mix of coordination and extraction, benefiting from the shelter's services but also adhering to its norms.
 *
 * DIRECTIONALITY LOGIC:
 *   The Cabmen's Shelter Society, as the organizers and maintainers of the shelters, benefit from the infrastructure and social control it provides. The Cabmen gain from a social support and place to rest. Bloom and Dedalus become victims through their social isolation and the financial exchange required to gain limited benefit.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    shelter_accessibility,
    'To what extent does the cabman''s shelter serve its intended purpose of providing accessible refuge for cabmen?',
    'Historical records, testimonies from cabmen, and assessments of the shelter''s usage and operational capacity.',
    'If highly accessible, the constraint leans more towards a rope; if accessibility is limited, it becomes a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shelter_accessibility, empirical, 'Extent of actual accessibility of shelter to cabmen.').

omega_variable(
    social_exclusivity,
    'To what degree does the shelter become a site of social exclusion, hindering outsiders from effectively accessing its services?',
    'Sociological studies, ethnographic observations, and analysis of social dynamics within the shelter.',
    'High social exclusivity shifts the constraint towards a snare for those outside the established cabmen''s network.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_exclusivity, conceptual, 'Degree of social exclusivity within the shelter.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp16, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ulys_tr_t0, ulysses_chp16, theater_ratio, 0, 0.3).
narrative_ontology:measurement(ulys_tr_t5, ulysses_chp16, theater_ratio, 5, 0.4).
narrative_ontology:measurement(ulys_tr_t10, ulysses_chp16, theater_ratio, 10, 0.5).

% Extraction over time
narrative_ontology:measurement(ulys_be_t0, ulysses_chp16, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(ulys_be_t5, ulysses_chp16, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(ulys_be_t10, ulysses_chp16, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp16, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
