% ============================================================================
% CONSTRAINT STORY: politeness_face_negotiation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_politeness_face_negotiation, []).

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
 *   constraint_id: politeness_face_negotiation
 *   human_readable: The Face Negotiation Constraint (Politeness Theory)
 *   domain: social
 *
 * SUMMARY:
 *   Politeness theory suggests that individuals engage in 'face work' to
 *   maintain their own and others' social image, balancing the desire for
 *   social acceptance (positive face) with the need for autonomy (negative
 *   face). This constraint manifests differently based on the actors
 *   involved. While politeness helps create social cohesion, it also can
 *   inhibit unfiltered expression and efficiency.
 *
 * KEY AGENTS:
 *   - Individual Feeling Constrained: Powerless/Trapped - Individuals who must suppress true opinions.
 *   - Society Enforcing Norms: Institutional/Arbitrage - Society that enforces the cultural standard
 *   - Analytical Observer: Analytical/Analytical - The abstract perspective that the process works to create benefits and costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(politeness_face_negotiation, 0.35).
domain_priors:suppression_score(politeness_face_negotiation, 0.4).
domain_priors:theater_ratio(politeness_face_negotiation, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(politeness_face_negotiation, extractiveness, 0.35).
narrative_ontology:constraint_metric(politeness_face_negotiation, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(politeness_face_negotiation, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(politeness_face_negotiation, tangled_rope).
narrative_ontology:human_readable(politeness_face_negotiation, "The Face Negotiation Constraint (Politeness Theory)").
narrative_ontology:topic_domain(politeness_face_negotiation, "social").

domain_priors:requires_active_enforcement(politeness_face_negotiation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(politeness_face_negotiation, social_cohesion).
narrative_ontology:constraint_beneficiary(politeness_face_negotiation, individual_relationships).
narrative_ontology:constraint_victim(politeness_face_negotiation, unfiltered_expression).
narrative_ontology:constraint_victim(politeness_face_negotiation, efficiency_of_communication).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Individuals who feel they cannot express their true opinions or desires without risking social disapproval.  They perceive the constraint as a snare because they are trapped in a situation where they must constantly monitor their behavior.
constraint_indexing:constraint_classification(politeness_face_negotiation, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% Society as a whole benefits from the maintenance of social order and the avoidance of conflict. The norms of politeness are internalized and enforced through social pressure. The constraint is a rope because it facilitates social interaction.
constraint_indexing:constraint_classification(politeness_face_negotiation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Politeness is a universal social constraint where individuals manage a 'face'--a social self-image. It balances social cohesion with individual autonomy. It functions to both extract from individuals (suppressing some actions) and coordinate social interactions. Thus, it is a Tangled Rope.
constraint_indexing:constraint_classification(politeness_face_negotiation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% In relationships, individuals benefit from politeness creating harmony and mutual respect, though it may require suppressing one's true feeling at times. Here politeness facilitates coordination and positive relationship development.
constraint_indexing:constraint_classification(politeness_face_negotiation, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% Cultural institutions (schools, media) both promote politeness and allow for some level of dissent within boundaries. They extract conformity but also coordinate expression.
constraint_indexing:constraint_classification(politeness_face_negotiation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(politeness_face_negotiation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(politeness_face_negotiation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(politeness_face_negotiation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(politeness_face_negotiation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. Individuals must sacrifice some expression to comply with social norms. Suppression (0.40): Moderate. Social pressure exists for polite behavior. Theater ratio (0.20): Low. While there is ritualized politeness in some settings, much of it is sincere and contributes to social harmony.
 *
 * PERSPECTIVAL GAP:
 *   Individuals who feel constantly constrained by politeness norms will view this constraint as a snare. Societal entities promoting cohesion are benefitting from this constraint.  This creates the distinction for a tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from who benefits and who is burdened. Society benefits from maintained structure.  Individuals are burdened from what they must forfeit in open expression.
 *
 * MANDATROPHY ANALYSIS:
 *   Politeness is clearly not a pure snare, because it coordinates as well as extracts. The tangled rope classification acknowledges this duality.  Without enforcement and at least a perceived coordination benefit, the type would likely be snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_relativity_threshold,
    'To what extent are politeness norms culturally relative, and what is the impact of cross-cultural misunderstandings?',
    'Cross-cultural studies comparing politeness strategies and their interpretations; analysis of communication breakdowns in intercultural interactions.',
    'High cultural relativity: Politeness is less of a universal constraint and more of a collection of local ropes. Low cultural relativity: Politeness is a stronger universal tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_relativity_threshold, empirical, 'The degree of cultural relativity in politeness norms.').

omega_variable(
    individual_variation_threshold,
    'How much does individual personality impact adherence to politeness norms, and how does this vary across contexts?',
    'Personality studies correlating traits with politeness behavior; analysis of communication in different contexts based on personality.',
    'High individual variation: Politeness is weaker constraint with many exit options. Low individual variation: Politeness is a stronger constraint and may be classified more as a Snare for some people.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_variation_threshold, empirical, 'The variation of individual adherence to politeness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(politeness_face_negotiation, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(poli_tr_t0, politeness_face_negotiation, theater_ratio, 0, 0.1).
narrative_ontology:measurement(poli_tr_t50, politeness_face_negotiation, theater_ratio, 50, 0.2).
narrative_ontology:measurement(poli_tr_t100, politeness_face_negotiation, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(poli_be_t0, politeness_face_negotiation, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(poli_be_t50, politeness_face_negotiation, base_extractiveness, 50, 0.35).
narrative_ontology:measurement(poli_be_t100, politeness_face_negotiation, base_extractiveness, 100, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(politeness_face_negotiation, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
