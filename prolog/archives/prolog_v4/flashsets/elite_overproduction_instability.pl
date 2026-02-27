% ============================================================================
% CONSTRAINT STORY: elite_overproduction_instability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_elite_overproduction_instability, []).

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
 *   constraint_id: elite_overproduction_instability
 *   human_readable: The Aspirant's Bottleneck
 *   domain: social
 *
 * SUMMARY:
 *   Elite overproduction occurs when the number of individuals qualified and
 *   desiring elite positions exceeds the actual availability of such
 *   positions. This creates a bottleneck where many aspirants are left
 *   disappointed, leading to potential social instability. Incumbent elites
 *   benefit from a larger pool of talent, while the aspirants face
 *   frustration and limited opportunities.
 *
 * KEY AGENTS:
 *   - Overproduced Elite Aspirants: Primary target (powerless/trapped) - They bear the cost of investing in skills and education without guaranteed rewards.
 *   - Incumbent Elites: Primary beneficiary (institutional/arbitrage) - They benefit from increased competition and a larger pool of potential recruits.
 *   - Social Stability: Secondary actor (moderate/constrained) - Overall stability is negatively impacted by widespread frustration and resentment among overproduced elites.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(elite_overproduction_instability, 0.65).
domain_priors:suppression_score(elite_overproduction_instability, 0.7).
domain_priors:theater_ratio(elite_overproduction_instability, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(elite_overproduction_instability, extractiveness, 0.65).
narrative_ontology:constraint_metric(elite_overproduction_instability, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(elite_overproduction_instability, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(elite_overproduction_instability, tangled_rope).
narrative_ontology:human_readable(elite_overproduction_instability, "The Aspirant's Bottleneck").
narrative_ontology:topic_domain(elite_overproduction_instability, "social").

domain_priors:requires_active_enforcement(elite_overproduction_instability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(elite_overproduction_instability, incumbent_elites).
narrative_ontology:constraint_victim(elite_overproduction_instability, overproduced_elite_aspirants).
narrative_ontology:constraint_victim(elite_overproduction_instability, social_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of the individual who invested heavily in education and preparation, but faces limited opportunities. They are trapped in a system where their efforts are not rewarded with expected elite positions.
constraint_indexing:constraint_classification(elite_overproduction_instability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective of established elites who benefit from the overproduction as it provides a larger pool of talent to select from and maintain their positions. They can also use the threat of competition to maintain control.
constraint_indexing:constraint_classification(elite_overproduction_instability, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% From the perspective of general societal stability. While a meritocratic system is intended to improve efficiency and social mobility (a 'rope'), elite overproduction can lead to instability, frustration, and potential unrest (extraction).
constraint_indexing:constraint_classification(elite_overproduction_instability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The analytical observer sees that there are benefits and extraction involved. It can drive innovation and efficiency because of the competition but can also create resentment if the process isn't fair.
constraint_indexing:constraint_classification(elite_overproduction_instability, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(elite_overproduction_instability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(elite_overproduction_instability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(elite_overproduction_instability, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(elite_overproduction_instability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(elite_overproduction_instability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high because the system extracts resources (time, money, effort) from the aspirants. The suppression is high because it's difficult for individuals to create their elite pathways or escape the competition. The theater ratio is lower reflecting that some of the education has some genuine utility even if the aspiration is thwarted.
 *
 * PERSPECTIVAL GAP:
 *   The overproduced aspirant views the system as a snare, while the incumbent elites view it as a beneficial rope. Societal stability experiences the tension between the supposed benefits of meritocracy and the potential for unrest. The analytical observer recognizes this duality.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent elites benefit, so d is low. Overproduced elites are the primary victims, so d is high. Social stability as a concept feels the downstream effects.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    meritocracy_measurement,
    'How accurately does the system select the most competent individuals?',
    'Statistical analysis of selected individuals'' performance compared to non-selected individuals'' performance.',
    'If the system is highly meritocratic, elite overproduction might still be a problem but more accepted. If not, the instability increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meritocracy_measurement, empirical, 'Measuring the meritocracy of the system').

omega_variable(
    opportunity_availability,
    'What is the actual number of available elite positions?',
    'Detailed census of elite positions across different sectors.',
    'A higher number of available opportunities reduces the tension of elite overproduction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(opportunity_availability, empirical, 'Measuring the number of available positions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(elite_overproduction_instability, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elit_tr_t0, elite_overproduction_instability, theater_ratio, 0, 0.1).
narrative_ontology:measurement(elit_tr_t5, elite_overproduction_instability, theater_ratio, 5, 0.2).
narrative_ontology:measurement(elit_tr_t10, elite_overproduction_instability, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(elit_be_t0, elite_overproduction_instability, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(elit_be_t5, elite_overproduction_instability, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(elit_be_t10, elite_overproduction_instability, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(elite_overproduction_instability, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
