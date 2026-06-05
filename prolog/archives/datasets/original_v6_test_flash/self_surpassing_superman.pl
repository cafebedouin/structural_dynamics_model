% ============================================================================
% CONSTRAINT STORY: self_surpassing_superman
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_self_surpassing_superman, []).

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
 *   constraint_id: self_surpassing_superman
 *   human_readable: The Rearing of the Superman (Übermensch)
 *   domain: philosophical/social
 *
 * SUMMARY:
 *   This constraint represents the imperative to overcome the 'All-too-human'
 *   state of the 'Last Man' following the 'Death of God.' It functions as an
 *   existential demand that requires the total transvaluation of existing
 *   'slave' moral values into 'master' values of power and creation. The
 *   constraint is inherently tangled, as it involves both individual
 *   empowerment and social disruption.
 *
 * KEY AGENTS:
 *   - Self-Overcoming Individuals: Primary beneficiaries (institutional/arbitrage) — gain power and freedom through self-transformation.
 *   - Traditional Moral Systems: Primary victims (powerless/trapped) — face devaluing and suppression of their values.
 *   - Followers of Traditional Morality: Secondary victims (powerless/trapped) -- are subjected to the upheaval and loss of moral value.
 *   - Society itself: Constrained by new ideals and its relationship to the individuals.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(self_surpassing_superman, 0.6).
domain_priors:suppression_score(self_surpassing_superman, 0.7).
domain_priors:theater_ratio(self_surpassing_superman, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(self_surpassing_superman, extractiveness, 0.6).
narrative_ontology:constraint_metric(self_surpassing_superman, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(self_surpassing_superman, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(self_surpassing_superman, tangled_rope).
narrative_ontology:human_readable(self_surpassing_superman, "The Rearing of the Superman (Übermensch)").
narrative_ontology:topic_domain(self_surpassing_superman, "philosophical/social").

domain_priors:requires_active_enforcement(self_surpassing_superman).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(self_surpassing_superman, self_overcoming_individuals).
narrative_ontology:constraint_victim(self_surpassing_superman, traditional_moral_systems).
narrative_ontology:constraint_victim(self_surpassing_superman, followers_of_traditional_morality).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FOLLOWERS OF TRADITIONAL MORALITY (SNARE) - Those who remain attached to traditional moral systems are seen as obstacles to the Superman's self-overcoming. They are trapped within a system that is actively devalued and suppressed.
constraint_indexing:constraint_classification(self_surpassing_superman, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SOCIETY ITSELF (TANGLED ROPE) - Society is both a constraint and an opportunity. It provides the raw material to be overcome, but also resists the radical revaluation of all values. Individuals may experience this process as constraining as the Ubermensch challenges established norms and behaviors.
constraint_indexing:constraint_classification(self_surpassing_superman, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SELF-OVERCOMING INDIVIDUALS (ROPE) - The individuals who successfully embody the Superman ideal benefit from the constraint. They gain power, freedom, and creative potential through their self-transformation.
constraint_indexing:constraint_classification(self_surpassing_superman, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (TANGLED ROPE) - From a distance, the endeavor appears to be both empowering for the individual (coordination) but also actively suppressing alternatives within the population (extraction), along with any traditional moral claims.
constraint_indexing:constraint_classification(self_surpassing_superman, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(self_surpassing_superman_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(self_surpassing_superman, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(self_surpassing_superman, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(self_surpassing_superman, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(self_surpassing_superman_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): The constraint extracts from existing moral systems and those who adhere to them. Suppression (0.7): The constraint requires the suppression of traditional values in favor of new ones. Theater Ratio (0.3): While the constraint can manifest in performative ways, the focus is more on actual transformation.
 *
 * PERSPECTIVAL GAP:
 *   The followers of traditional morality experience this constraint as a snare, due to the loss of meaning, the devaluing of their established lives, and overall lack of any power to change the system. Self-overcoming individuals benefit from the new arrangement by acquiring greater agency and ability within their own lives, even if they must constantly adapt and adjust to a state of overcoming. The Analytical observer sees the tension for both as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary (the self-overcoming individual) experiences the constraint as a Rope, providing opportunities for growth and freedom. The victims (traditional moral systems and their followers) experience it as a Snare, as their values are devalued and suppressed. Society is a Tangled Rope as it is both a constraint and an opportunity.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mislabeling by recognizing both the empowering and destructive aspects of the Ubermensch ideal. It is not simply a pure extraction mechanism (Snare) as it offers a path to self-improvement (Rope). Nor is it a pure coordination mechanism (Rope) as it involves the suppression of alternative value systems. The Tangled Rope classification captures this duality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definitions_of_value,
    'What constitutes a ''higher'' value, and who determines it?',
    'Philosophical debate and historical analysis of value systems.',
    'Different definitions can lead to drastically different interpretations and outcomes regarding the Ubermensch.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definitions_of_value, conceptual, 'The nature of ''higher'' value and its determination is critical to the realization of the ideal.').

omega_variable(
    justification_of_power,
    'How is the ''will to power'' ethically justified, and what are its limits?',
    'Ethical and political philosophy, social contract theory.',
    'The means by which power is gained and exerted can radically change the nature of the ideal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(justification_of_power, conceptual, 'Ethical constraints regarding will to power and its manifestation.').

omega_variable(
    manifestation_in_reality,
    'Is the Ubermensch a realistic ideal, or an impossible aspiration?',
    'Sociological and psychological studies of individuals who embody aspects of the ideal.',
    'Whether the ideal is grounded or fantasy will shape the implementation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(manifestation_in_reality, empirical, 'Evidence of the Ubermensch in reality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(self_surpassing_superman, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(self_tr_t0, self_surpassing_superman, theater_ratio, 0, 0.1).
narrative_ontology:measurement(self_tr_t5, self_surpassing_superman, theater_ratio, 5, 0.2).
narrative_ontology:measurement(self_tr_t10, self_surpassing_superman, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(self_be_t0, self_surpassing_superman, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(self_be_t5, self_surpassing_superman, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(self_be_t10, self_surpassing_superman, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(self_surpassing_superman, enforcement_mechanism).
narrative_ontology:affects_constraint(self_surpassing_superman, nihilism).
narrative_ontology:affects_constraint(self_surpassing_superman, will_to_power).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
