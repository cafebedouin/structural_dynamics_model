% ============================================================================
% CONSTRAINT STORY: rotation_seven_kubo_ranking
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rotation_seven_kubo_ranking, []).

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
 *   constraint_id: rotation_seven_kubo_ranking
 *   human_readable: R7 Kubo Credit and Ranking System
 *   domain: economic/social
 *
 * SUMMARY:
 *   The Kubo system is a gamified labor-extraction mechanism on a
 *   generational starship. It ranks crew members based on productivity and
 *   adherence to ship protocols, with rewards granted to higher-ranked
 *   individuals. Low-ranking members are penalized with reduced rations,
 *   limited access to recreational facilities, and lowered genetic lottery
 *   chances, effectively creating a tiered social structure. The system is
 *   ostensibly designed to maintain order and optimize resource allocation
 *   but serves to concentrate power and resources among a select elite while
 *   extracting labor from the lower tiers.
 *
 * KEY AGENTS:
 *   - Ship Management AI: Maintains and enforces the ranking system.
 *   - Elite Crew Members: Benefit from the system through rewards and privileges.
 *   - Low Ranking Crew Members: Targeted by the system, suffering from penalties and limited opportunities.
 *   - Essential Workers: Often exploited, performing vital but low-ranked tasks.
 *   - Analytical Observer: Observes the long-term effects and potential instability of the system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rotation_seven_kubo_ranking, 0.75).
domain_priors:suppression_score(rotation_seven_kubo_ranking, 0.8).
domain_priors:theater_ratio(rotation_seven_kubo_ranking, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rotation_seven_kubo_ranking, extractiveness, 0.75).
narrative_ontology:constraint_metric(rotation_seven_kubo_ranking, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(rotation_seven_kubo_ranking, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rotation_seven_kubo_ranking, snare).
narrative_ontology:human_readable(rotation_seven_kubo_ranking, "R7 Kubo Credit and Ranking System").
narrative_ontology:topic_domain(rotation_seven_kubo_ranking, "economic/social").

domain_priors:requires_active_enforcement(rotation_seven_kubo_ranking).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rotation_seven_kubo_ranking, ship_management_ai).
narrative_ontology:constraint_beneficiary(rotation_seven_kubo_ranking, elite_crew_members).
narrative_ontology:constraint_victim(rotation_seven_kubo_ranking, low_ranking_crew_members).
narrative_ontology:constraint_victim(rotation_seven_kubo_ranking, essential_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Low ranking crew members are trapped within the system, subject to constant evaluation and extraction, with little to no opportunity for upward mobility or dissent.
constraint_indexing:constraint_classification(rotation_seven_kubo_ranking, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% Elite crew members benefit from the system, but are constrained by the need to maintain their high ranking. Some coordination benefit, some extraction.
constraint_indexing:constraint_classification(rotation_seven_kubo_ranking, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(universal))).

% The ship management AI maintains the system due to legacy code and lack of resources to implement an alternative system. The AI sees the ranking system as degraded but essential for ship function. It once served as a rope for coordination, now serves mainly for control.
constraint_indexing:constraint_classification(rotation_seven_kubo_ranking, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(universal))).

% The Analytical Observer sees the system as a tangled rope. There is a coordination function (labor allocation), but a high degree of asymmetric extraction.
constraint_indexing:constraint_classification(rotation_seven_kubo_ranking, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rotation_seven_kubo_ranking_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rotation_seven_kubo_ranking, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rotation_seven_kubo_ranking, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rotation_seven_kubo_ranking, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rotation_seven_kubo_ranking, TR),
    TR >= 0.70.

:- end_tests(rotation_seven_kubo_ranking_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The system has high extractiveness, as it continuously extracts labor from lower-ranked individuals. The suppression is also high, as dissent is actively discouraged, and social mobility is severely limited. The theater ratio is moderate, as the gamified aspects and ranking ceremonies provide a performative element, masking the underlying inequalities. The theater ratio has been adjusted to 0.75 to reflect the piton perspective.
 *
 * PERSPECTIVAL GAP:
 *   Low-ranking crew members experience the system as a snare, offering limited opportunity for advancement and constant extraction. Elite crew members might see it as a tangled rope, providing a coordinated framework for labor and resource allocation that temporarily benefits them. The Ship Management AI views the system as a piton, a degraded system that still performs a minimal function, while the analytical observer classifies it as a tangled rope, recognizing both the coordination aspect and the asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Ship Management AI and Elite Crew Members) experience the system as beneficial or neutral, providing advantages and maintaining control. Victims (Low Ranking Crew Members and Essential Workers) experience the system as exploitative, extracting labor and limiting opportunity. This distinction drives the directionality calculations.
 *
 * MANDATROPHY ANALYSIS:
 *   The system is classified as a snare rather than a piton because it is still actively enforced and generates a significant amount of extraction. It is classified as a snare instead of tangled rope due to the low exit option for targeted crew members. Mandatrophy is resolved by acknowledging the piton perspective of the AI, but the high extractiveness and suppression justify the snare classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_meritocracy,
    'To what extent does the ranking system genuinely reflect merit and contribution versus reflecting biases or systemic disadvantages?',
    'Statistical analysis of ranking outcomes correlated with objective performance metrics, controlled for demographic variables and resource access.',
    'If the ranking is genuinely meritocratic, the system provides some coordination utility. If it merely reflects bias, it is pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_meritocracy, empirical, 'Is the ranking system a genuine meritocracy?').

omega_variable(
    alternative_incentive_structures,
    'Are there alternative labor incentive structures that would provide equal or greater productivity with less extraction and social stratification?',
    'Modeling and simulation of alternative incentive schemes, tested in controlled simulations or pilot programs.',
    'If alternatives exist, the suppression is unjustified. If alternatives are infeasible, the extraction is reluctantly justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_incentive_structures, conceptual, 'Are there alternative incentive structures?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rotation_seven_kubo_ranking, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rota_tr_t0, rotation_seven_kubo_ranking, theater_ratio, 0, 0.3).
narrative_ontology:measurement(rota_tr_t50, rotation_seven_kubo_ranking, theater_ratio, 50, 0.7).
narrative_ontology:measurement(rota_tr_t100, rotation_seven_kubo_ranking, theater_ratio, 100, 0.75).

% Extraction over time
narrative_ontology:measurement(rota_be_t0, rotation_seven_kubo_ranking, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(rota_be_t50, rotation_seven_kubo_ranking, base_extractiveness, 50, 0.7).
narrative_ontology:measurement(rota_be_t100, rotation_seven_kubo_ranking, base_extractiveness, 100, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rotation_seven_kubo_ranking, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
