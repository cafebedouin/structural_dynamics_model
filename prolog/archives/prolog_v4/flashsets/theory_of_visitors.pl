% ============================================================================
% CONSTRAINT STORY: theory_of_visitors
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_theory_of_visitors, []).

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
 *   constraint_id: theory_of_visitors
 *   human_readable: The Theory of Visitors (Relationship Transience)
 *   domain: social/psychological
 *
 * SUMMARY:
 *   This constraint encapsulates the idea that all human relationships are
 *   inherently transient, like visitors who arrive for a limited time and
 *   inevitably depart. This perspective influences how people approach
 *   relationships, fostering emotional detachment and potentially undermining
 *   social cohesion. The theory suggests that individuals should expect and
 *   accept the temporary nature of their connections.
 *
 * KEY AGENTS:
 *   - Individuals Seeking Long-Term Commitment: Primary target (powerless/trapped) - burdened by the expectation of transience.
 *   - Individuals Seeking Emotional Detachment: Primary beneficiary (institutional/arbitrage) - benefit from reduced emotional vulnerability.
 *   - Social Cohesion: Secondary victim (moderate/constrained) - weakened by the erosion of long-term commitments.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(theory_of_visitors, 0.5).
domain_priors:suppression_score(theory_of_visitors, 0.6).
domain_priors:theater_ratio(theory_of_visitors, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(theory_of_visitors, extractiveness, 0.5).
narrative_ontology:constraint_metric(theory_of_visitors, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(theory_of_visitors, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(theory_of_visitors, tangled_rope).
narrative_ontology:human_readable(theory_of_visitors, "The Theory of Visitors (Relationship Transience)").
narrative_ontology:topic_domain(theory_of_visitors, "social/psychological").

domain_priors:requires_active_enforcement(theory_of_visitors).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(theory_of_visitors, individuals_seeking_emotional_detachment).
narrative_ontology:constraint_victim(theory_of_visitors, individuals_seeking_long_term_commitment).
narrative_ontology:constraint_victim(theory_of_visitors, social_cohesion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of individuals seeking long-term commitment: They are trapped by the expectation of transience, making it difficult to form deep, lasting relationships. The constant anticipation of departure extracts emotional investment without guaranteed return.
constraint_indexing:constraint_classification(theory_of_visitors, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Perspective of the general population: They are constrained by the prevailing social norms that promote short-term relationships, but benefit from reduced emotional vulnerability. There is both extraction and coordination.
constraint_indexing:constraint_classification(theory_of_visitors, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Perspective of individuals seeking emotional detachment: They benefit from the perceived transience of relationships, as it allows them to avoid deep emotional connections and potential heartbreak. They see it as a coordinating force.
constraint_indexing:constraint_classification(theory_of_visitors, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective of social cohesion: The idea of inherently transient relationships degrades the potential for strong, lasting social bonds that are vital for a stable society. What once was a coordination mechanism is now theatre.
constraint_indexing:constraint_classification(theory_of_visitors, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(theory_of_visitors_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(theory_of_visitors, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(theory_of_visitors, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(theory_of_visitors, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(theory_of_visitors, TR),
    TR >= 0.70.

:- end_tests(theory_of_visitors_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.50): Moderate. This reflects the emotional cost of investing in relationships that are expected to be temporary. Individuals seeking long-term commitment experience this as a significant extraction. Suppression (0.60): Moderate-High. This accounts for the prevailing social norms that reinforce the idea of relationship transience, suppressing the formation of deep, lasting connections. Theater ratio (0.20): Low. The emphasis is on the perceived reality of transient relationships rather than performative aspects.
 *
 * PERSPECTIVAL GAP:
 *   The theory of visitors creates a gap between those seeking long-term commitment (Snare) and those seeking emotional detachment (Rope). The former experience a sense of being trapped and exploited by the transient nature of relationships, while the latter find it empowering and liberating.
 *
 * DIRECTIONALITY LOGIC:
 *   Those seeking long-term relationships are victims because they are structurally disadvantaged by the expectation of relationship transience (high 'd'). Individuals seeking emotional detachment benefit because the theory provides them with a rationale for avoiding deep emotional connection and commitment (low 'd'). Social cohesion is structurally disadvantaged because its viability depends on lasting relationships.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    relationship_duration_variability,
    'To what extent is the duration of human relationships inherently variable, and how much is it influenced by social factors?',
    'Longitudinal study of relationship durations across different cultures and social contexts.',
    'If relationship duration is mostly inherent: The theory of visitors holds true to a greater extent. If it is mostly influenced by social factors: The theory of visitors is a social construct that can be changed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relationship_duration_variability, empirical, 'Variability of relationship durations').

omega_variable(
    emotional_impact_detachment,
    'Does emotional detachment, as a coping mechanism for relationship transience, ultimately lead to greater well-being or increased social isolation?',
    'Comparative study of individuals with high vs. low emotional detachment in the context of relationship transience.',
    'If emotional detachment leads to greater well-being: The theory of visitors is a beneficial adaptation. If it leads to increased social isolation: The theory of visitors has negative consequences for individuals and society.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emotional_impact_detachment, empirical, 'Emotional impact of detachment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(theory_of_visitors, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theo_tr_t0, theory_of_visitors, theater_ratio, 0, 0.1).
narrative_ontology:measurement(theo_tr_t5, theory_of_visitors, theater_ratio, 5, 0.15).
narrative_ontology:measurement(theo_tr_t10, theory_of_visitors, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(theo_be_t0, theory_of_visitors, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(theo_be_t5, theory_of_visitors, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(theo_be_t10, theory_of_visitors, base_extractiveness, 10, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(theory_of_visitors, information_standard).
narrative_ontology:affects_constraint(theory_of_visitors, attachment_theory).
narrative_ontology:affects_constraint(theory_of_visitors, social_capital).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
