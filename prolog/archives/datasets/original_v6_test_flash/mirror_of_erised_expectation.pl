% ============================================================================
% CONSTRAINT STORY: mirror_of_erised_expectation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mirror_of_erised_expectation, []).

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
 *   constraint_id: mirror_of_erised_expectation
 *   human_readable: The Erised Career/Stability Mirror
 *   domain: psychological/economic
 *
 * SUMMARY:
 *   This constraint represents the internalised Millennial expectation of a
 *   "magical" meritocracy where ordinary people defeat evil and achieve
 *   stability, based on cultural narratives from the 1990s. This expectation
 *   creates a structural tension between the promise of upward mobility
 *   through hard work and the economic realities of increasing inequality and
 *   precarious employment. It can be understood as a 'mirror' reflecting back
 *   desired outcomes, but ultimately creating a distorted view of reality and
 *   a source of potential exploitation.
 *
 * KEY AGENTS:
 *   - Millennial Generation: Primary victim (powerless/trapped) — vulnerable to exploitation due to false expectations.
 *   - Future Generations: Secondary victim (moderate/constrained) — inherit consequences of exploitation and economic instability.
 *   - Cultural Narrative Producers: Primary beneficiary (institutional/arbitrage) — benefit from reinforcing cultural narratives.
 *   - Incumbent Institutions: Secondary beneficiary (powerful/arbitrage) — benefit from compliant workforce.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mirror_of_erised_expectation, 0.55).
domain_priors:suppression_score(mirror_of_erised_expectation, 0.65).
domain_priors:theater_ratio(mirror_of_erised_expectation, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mirror_of_erised_expectation, extractiveness, 0.55).
narrative_ontology:constraint_metric(mirror_of_erised_expectation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(mirror_of_erised_expectation, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mirror_of_erised_expectation, tangled_rope).
narrative_ontology:human_readable(mirror_of_erised_expectation, "The Erised Career/Stability Mirror").
narrative_ontology:topic_domain(mirror_of_erised_expectation, "psychological/economic").

domain_priors:requires_active_enforcement(mirror_of_erised_expectation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mirror_of_erised_expectation, cultural_narrative_producers).
narrative_ontology:constraint_beneficiary(mirror_of_erised_expectation, incumbent_institutions).
narrative_ontology:constraint_victim(mirror_of_erised_expectation, millennial_generation).
narrative_ontology:constraint_victim(mirror_of_erised_expectation, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Millennials are trapped by the expectation that hard work and talent will inevitably lead to career success and financial stability. This expectation, fueled by cultural narratives, makes them vulnerable to exploitation and disappointment when the reality of the economic system doesn't match the narrative.
constraint_indexing:constraint_classification(mirror_of_erised_expectation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Future generations are constrained by the existing systems and norms established under this expectation. While they may benefit somewhat from technological advancements and social progress, they also inherit the negative consequences of millennial exploitation and economic instability.
constraint_indexing:constraint_classification(mirror_of_erised_expectation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Those who produce the cultural narratives (media, entertainment, education) benefit from perpetuating the expectation, as it reinforces their influence and control over society. The positive feedback allows for arbitrage of cultural production.
constraint_indexing:constraint_classification(mirror_of_erised_expectation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Incumbent institutions (corporations, government, etc.) benefit from a workforce that believes in the expectation of a meritocracy. It allows them to extract more labor with the promise of future rewards, while simultaneously suppressing any questioning of the system's inherent inequalities.
constraint_indexing:constraint_classification(mirror_of_erised_expectation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% From an analytical perspective, the expectation of a magical meritocracy is a tangled rope because it provides a framework for motivating individuals while simultaneously extracting their labor and suppressing alternative models.
constraint_indexing:constraint_classification(mirror_of_erised_expectation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mirror_of_erised_expectation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mirror_of_erised_expectation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mirror_of_erised_expectation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mirror_of_erised_expectation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mirror_of_erised_expectation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): The expectation extracts labor and emotional investment from millennials, often without commensurate reward. Suppression (0.65): The expectation suppresses alternative models, reinforcing the belief that individual effort is the sole determinant of success. Theater Ratio (0.40): There is an element of 'performance' in reinforcing the meritocratic narrative, though it also has a functional aspect (motivation, social cohesion).
 *
 * PERSPECTIVAL GAP:
 *   The millennial generation experiences this expectation as a snare, while those benefiting from the system may see it as a rope or tangled rope. The analytical observer aims to understand the interplay between the various perspectives and the long-term consequences of the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Millennials, with fewer exit options, experience a higher degree of extraction (high d value). Cultural narrative producers and incumbent institutions benefit from perpetuating the expectation (low d value). The other actors (future generations) hold mid-range d values, due to limited agency and constrained exit options. The analytical observer attempts to assess the overall system from a removed perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by demonstrating that multiple DR classifications exist for this phenomenon, based on differing structural relationships. One perspective shows that some agents benefit from this 'system', whereas the powerless perspectives show those victims bear extraction costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    meritocracy_validity,
    'To what extent is the modern economic system a true meritocracy, and how much is determined by factors like wealth, class, and social connections?',
    'Empirical analysis of career and income data, controlling for factors like education, work ethic, and innate talent. Comparative analysis of social mobility across different countries and time periods.',
    'If system is truly meritocratic, the expectation may be more of a rope than a tangled rope. If determined by external factors, this expectation functions more as a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meritocracy_validity, empirical, 'Determination of the validity of the meritocracy concept.').

omega_variable(
    alternative_narrative_viability,
    'Can alternative narratives focused on collective action, systemic change, and alternative forms of success effectively compete with the dominant meritocratic narrative?',
    'Social and political experiments with alternative systems, measurement of cultural impact of different models.',
    'If alternative narratives prove viable, the suppression effect may decrease, shifting the classification to a scaffold or rope. If alternatives fail to gain traction, the existing model becomes a piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_narrative_viability, conceptual, 'Determine whether alternative cultural narratives are viable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mirror_of_erised_expectation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mirr_tr_t0, mirror_of_erised_expectation, theater_ratio, 0, 0.2).
narrative_ontology:measurement(mirr_tr_t10, mirror_of_erised_expectation, theater_ratio, 10, 0.3).
narrative_ontology:measurement(mirr_tr_t20, mirror_of_erised_expectation, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(mirr_be_t0, mirror_of_erised_expectation, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(mirr_be_t10, mirror_of_erised_expectation, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(mirr_be_t20, mirror_of_erised_expectation, base_extractiveness, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mirror_of_erised_expectation, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
