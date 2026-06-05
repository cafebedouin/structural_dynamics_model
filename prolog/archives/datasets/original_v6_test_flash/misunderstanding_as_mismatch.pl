% ============================================================================
% CONSTRAINT STORY: misunderstanding_as_mismatch
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_misunderstanding_as_mismatch, []).

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
 *   constraint_id: misunderstanding_as_mismatch
 *   human_readable: Social Pressure for Worldview Assimilation
 *   domain: social/psychological
 *
 * SUMMARY:
 *   This constraint models the social pressure for individuals to assimilate
 *   into a group's dominant worldview. It involves a complex interplay
 *   between social cohesion, group identity, and individual autonomy.
 *   Individuals who deviate from the accepted worldview often face social
 *   exclusion, ridicule, and other forms of social pressure, leading them to
 *   suppress their dissenting views to gain acceptance and avoid conflict.
 *
 * KEY AGENTS:
 *   - Worldview Dissidents: Primary target (powerless/trapped) - face social exclusion and pressure to conform.
 *   - Dominant Group Members: Primary beneficiary (institutional/arbitrage) - benefit from social cohesion and shared understanding.
 *   - Intellectuals: Secondary actor (powerful/mobile) - may benefit from public discussion but risk social isolation if they express unpopular views.
 *   - Analytical Observer: Sees full structure (analytical/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(misunderstanding_as_mismatch, 0.6).
domain_priors:suppression_score(misunderstanding_as_mismatch, 0.7).
domain_priors:theater_ratio(misunderstanding_as_mismatch, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(misunderstanding_as_mismatch, extractiveness, 0.6).
narrative_ontology:constraint_metric(misunderstanding_as_mismatch, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(misunderstanding_as_mismatch, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(misunderstanding_as_mismatch, snare).
narrative_ontology:human_readable(misunderstanding_as_mismatch, "Social Pressure for Worldview Assimilation").
narrative_ontology:topic_domain(misunderstanding_as_mismatch, "social/psychological").

domain_priors:requires_active_enforcement(misunderstanding_as_mismatch).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(misunderstanding_as_mismatch, dominant_group_members).
narrative_ontology:constraint_victim(misunderstanding_as_mismatch, worldview_dissidents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of individuals whose worldviews differ significantly from the dominant group. They face social exclusion and pressure to conform, effectively trapping them within the group's coercive influence.
constraint_indexing:constraint_classification(misunderstanding_as_mismatch, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Perspective of individuals somewhat aligned with the dominant worldview but retaining independent thought. They derive some benefit from group acceptance but bear extraction costs from the need to suppress dissenting views to fit in.
constraint_indexing:constraint_classification(misunderstanding_as_mismatch, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Perspective of those fully assimilated into the dominant worldview. They benefit from the social cohesion and shared understanding facilitated by the constraint, perceiving it as a coordination mechanism.
constraint_indexing:constraint_classification(misunderstanding_as_mismatch, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Perspective of intellectuals, those who question and analyze the prevailing worldview. Some benefit through public discussion, but they risk social isolation if they express unpopular views.
constraint_indexing:constraint_classification(misunderstanding_as_mismatch, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% Perspective of norms that once benefited the group by promoting cohesion but are now outdated. Theater is high, as there are attempts to maintain these norms despite losing their utility.
constraint_indexing:constraint_classification(misunderstanding_as_mismatch, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% Perspective of analytical observers who can see the constraint as a mechanism that provides group cohesion but also suppresses dissenting views. There are extraction and coordination functions in this scenario.
constraint_indexing:constraint_classification(misunderstanding_as_mismatch, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(misunderstanding_as_mismatch_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(misunderstanding_as_mismatch, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(misunderstanding_as_mismatch, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(misunderstanding_as_mismatch, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(misunderstanding_as_mismatch, TR),
    TR >= 0.70.

:- end_tests(misunderstanding_as_mismatch_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): Moderate-high. Reflects the social costs imposed on individuals who do not conform to the dominant worldview. Suppression (0.7): High. Signifies the active discouragement and suppression of dissenting views. Theater ratio (0.3): Low. There is a low reliance on performative displays to validate or enforce the worldview.
 *
 * PERSPECTIVAL GAP:
 *   The classification of this constraint varies greatly depending on the perspective. Worldview dissidents experience it as a snare, as they are trapped and suppressed. Dominant group members see it as a rope, as it facilitates coordination and cohesion. Fence-sitters see it as a tangled rope, balancing benefits of group acceptance with extraction costs. An analytical observer sees it as tangled rope due to extraction of independent thoughts from group members, while offering social benefits.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is calculated from the structural relationship each actor has. Group members benefit from the cohesion and face social exclusion by non-conformity, while worldview dissidents risk group acceptance.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cohesion_vs_conformity,
    'At what point does the pressure for worldview assimilation stifle individual thought and critical analysis within the group?',
    'Sociological studies measuring the diversity of thought within groups and their problem-solving capabilities.',
    'If the pressure for assimilation is too high, the constraint becomes primarily extractive. If low, the constraint can shift to a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cohesion_vs_conformity, empirical, 'Determining when the pressure to conform stifles critical thought.').

omega_variable(
    influence_scope,
    'Does this constraint primarily operate on a local, regional, national, or global scope?',
    'Cross-cultural studies comparing worldview assimilation across different societies.',
    'The scope modifies the effective extraction based on verification difficulty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(influence_scope, empirical, 'Determining the scope the constraint operates in.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(misunderstanding_as_mismatch, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(misu_tr_t0, misunderstanding_as_mismatch, theater_ratio, 0, 0.2).
narrative_ontology:measurement(misu_tr_t5, misunderstanding_as_mismatch, theater_ratio, 5, 0.3).
narrative_ontology:measurement(misu_tr_t10, misunderstanding_as_mismatch, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(misu_be_t0, misunderstanding_as_mismatch, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(misu_be_t5, misunderstanding_as_mismatch, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(misu_be_t10, misunderstanding_as_mismatch, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(misunderstanding_as_mismatch, enforcement_mechanism).
narrative_ontology:affects_constraint(misunderstanding_as_mismatch, echo_chamber_effect).
narrative_ontology:affects_constraint(misunderstanding_as_mismatch, groupthink).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
