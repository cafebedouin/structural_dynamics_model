% ============================================================================
% CONSTRAINT STORY: utopia_apocalypse_fragility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_utopia_apocalypse_fragility, []).

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
 *   constraint_id: utopia_apocalypse_fragility
 *   human_readable: The Utopia-Apocalypse Cliff-Edge
 *   domain: social/political/philosophical
 *
 * SUMMARY:
 *   This constraint describes the inherent fragility of social order
 *   (Utopia), which rests on a "cliff-edge" and can transition into
 *   Apocalypse through slight shifts in structure, personnel, or culture. The
 *   system relies on both the perceived coordination of a 'utopia' and the
 *   asymmetric extraction from, and suppression of, certain groups that face
 *   a possible 'apocalypse'. Small shifts can result in significant
 *   transformations.
 *
 * KEY AGENTS:
 *   - Governing Elite: Primary beneficiary (institutional/arbitrage) - benefits from maintaining order and can avoid the worst effects of collapse.
 *   - General Population: Primary target (powerless/trapped) - bears the costs of suppression and the consequences of collapse.
 *   - Security Apparatus: Constrained actor (organized/constrained) - tasked with maintaining order but benefiting from expanded power during crisis.
 *   - Dissenting Minorities: Secondary target (moderate/constrained) - face disproportionate suppression and extraction.
 *   - Analytical Observer: External observer (analytical/analytical) - understands the dynamics and inherent risks without being able to directly influence outcomes.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(utopia_apocalypse_fragility, 0.55).
domain_priors:suppression_score(utopia_apocalypse_fragility, 0.65).
domain_priors:theater_ratio(utopia_apocalypse_fragility, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(utopia_apocalypse_fragility, extractiveness, 0.55).
narrative_ontology:constraint_metric(utopia_apocalypse_fragility, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(utopia_apocalypse_fragility, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(utopia_apocalypse_fragility, tangled_rope).
narrative_ontology:human_readable(utopia_apocalypse_fragility, "The Utopia-Apocalypse Cliff-Edge").
narrative_ontology:topic_domain(utopia_apocalypse_fragility, "social/political/philosophical").

domain_priors:requires_active_enforcement(utopia_apocalypse_fragility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(utopia_apocalypse_fragility, governing_elite).
narrative_ontology:constraint_beneficiary(utopia_apocalypse_fragility, security_apparatus).
narrative_ontology:constraint_victim(utopia_apocalypse_fragility, general_population).
narrative_ontology:constraint_victim(utopia_apocalypse_fragility, dissenting_minorities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For the general population, particularly those lacking resources or social capital, the shift from perceived utopia to apocalypse represents a devastating trap. They bear the costs of both suppression and extraction. Exiting is difficult or impossible.
constraint_indexing:constraint_classification(utopia_apocalypse_fragility, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% The governing elite benefit from the maintenance of social order (utopia) and can arbitrage their position to avoid the worst effects of a potential apocalypse. They perceive the constraint as a coordination mechanism to maintain their power and influence.
constraint_indexing:constraint_classification(utopia_apocalypse_fragility, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% The security apparatus (military, police, intelligence agencies) is constrained by the need to maintain order, but also benefits from the expansion of its power during times of crisis. They engage in both coordination and asymmetric extraction, making it a tangled rope. Exit is constrained by their organizational role.
constraint_indexing:constraint_classification(utopia_apocalypse_fragility, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Dissenting minorities are often the first to experience the shift towards apocalypse, as their rights and freedoms are curtailed in the name of security. They face significant extraction and suppression with limited exit options. Exit is constrained by social and political pressures.
constraint_indexing:constraint_classification(utopia_apocalypse_fragility, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% An analytical observer sees the inherent fragility of the system, recognizing the mix of coordination (utopia) and extraction (suppression of dissent, risk of apocalypse) that defines this tangled rope. They have analytical exit options by understanding the drivers, but cannot directly influence the system.
constraint_indexing:constraint_classification(utopia_apocalypse_fragility, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(utopia_apocalypse_fragility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(utopia_apocalypse_fragility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(utopia_apocalypse_fragility, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(utopia_apocalypse_fragility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(utopia_apocalypse_fragility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Significant, but not complete. The system extracts resources and freedoms from certain groups to maintain the perceived utopia, but there are also coordinating aspects that benefit the majority. Suppression (0.65): High. Maintaining the utopian ideal requires suppressing dissent and alternative viewpoints, creating a climate of fear and conformity. Theater ratio (0.30): Moderate. While the system engages in some theatrical displays to maintain its image, there is a significant functional component in maintaining order and suppressing dissent.
 *
 * PERSPECTIVAL GAP:
 *   The governing elite sees a rope (coordination), while the general population and dissenting minorities experience a snare (extraction and suppression). The security apparatus sees a tangled rope, balancing order maintenance with power accumulation. The analytical observer perceives the inherent fragility of the system and the potential for a rapid shift from utopia to apocalypse, resulting in a tangled rope classification at the civilizational level.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (governing elite) have low 'd' values, experiencing the system as coordination. Victims (general population, dissenting minorities) have high 'd' values, experiencing extraction and suppression. The security apparatus and analytical observer have intermediate 'd' values, reflecting mixed benefits and costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The system is not purely a snare because there are coordinating aspects that benefit some. The system is not purely a rope because it relies on extraction and suppression. The tangled rope classification captures the inherent tension and fragility of the system, balancing the perspectives of all actors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stability_threshold,
    'What is the threshold of social, political, or economic stress that triggers a shift from perceived utopia to potential apocalypse?',
    'Historical analysis of societal collapses, correlation of stress factors with instability metrics',
    'If the threshold is low, even minor disturbances could lead to significant consequences. If high, the system is more resilient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stability_threshold, empirical, 'Threshold of social, political, or economic stress leading to collapse').

omega_variable(
    governance_legitimacy,
    'To what extent does the governing elite maintain genuine legitimacy in the eyes of the general population?',
    'Public opinion surveys, analysis of trust in institutions, assessment of perceived corruption levels',
    'If high, the system is more resistant to shocks. If low, the system is prone to collapse triggered by minor challenges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(governance_legitimacy, empirical, 'Degree of governing elite''s legitimacy in the eyes of population').

omega_variable(
    cultural_cohesion,
    'How strongly does a shared cultural identity bind the society together, and how vulnerable is this cohesion to external or internal pressures?',
    'Sociological studies, analysis of social capital, evaluation of cultural fragmentation indicators',
    'If strong, the society can withstand internal dissent. If weak, the society is susceptible to disruption and upheaval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_cohesion, empirical, 'Strength and resilience of shared cultural identity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(utopia_apocalypse_fragility, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(utop_tr_t0, utopia_apocalypse_fragility, theater_ratio, 0, 0.2).
narrative_ontology:measurement(utop_tr_t5, utopia_apocalypse_fragility, theater_ratio, 5, 0.3).
narrative_ontology:measurement(utop_tr_t10, utopia_apocalypse_fragility, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(utop_be_t0, utopia_apocalypse_fragility, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(utop_be_t5, utopia_apocalypse_fragility, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(utop_be_t10, utopia_apocalypse_fragility, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(utopia_apocalypse_fragility, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
