% ============================================================================
% CONSTRAINT STORY: ulysses_chp13
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_chp13, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ulysses_chp13
 *   human_readable: The Sentimental Snare (Sandymount Shore)
 *   domain: social/artistic/psychological
 *
 * SUMMARY:
 *   Chapter 13 models the encounter between Gerty MacDowell and Leopold Bloom
 *   on Sandymount Strand. Gerty, influenced by romantic novels and societal
 *   expectations, projects an idealized fantasy onto Bloom. Bloom, in turn,
 *   observes and participates in this fantasy, fulfilling both his own
 *   desires and Gerty's romantic expectations. This intersection creates a
 *   snare for Gerty, trapping her in a self-constructed narrative that
 *   extracts from her agency and self-perception.
 *
 * KEY AGENTS:
 *   - Gerty MacDowell: Primary victim (powerless/trapped) - ensnared by her own romantic ideals.
 *   - Leopold Bloom: Opportunistic observer (moderate/mobile) - a catalyst for Gerty's fantasy fulfillment.
 *   - The Reader: Analytical observer - analyzes the interplay between characters and societal forces.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp13, 0.6).
domain_priors:suppression_score(ulysses_chp13, 0.7).
domain_priors:theater_ratio(ulysses_chp13, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp13, extractiveness, 0.6).
narrative_ontology:constraint_metric(ulysses_chp13, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ulysses_chp13, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp13, snare).
narrative_ontology:human_readable(ulysses_chp13, "The Sentimental Snare (Sandymount Shore)").
narrative_ontology:topic_domain(ulysses_chp13, "social/artistic/psychological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp13, literary_analysis).
narrative_ontology:constraint_victim(ulysses_chp13, gerty_macdowell).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Gerty, driven by societal expectations and personal desires, is trapped by the idealized romantic narrative she projects onto Bloom and herself. She is powerless to escape the snare of her own sentimental expectations.
constraint_indexing:constraint_classification(ulysses_chp13, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% The romantic ideal, once a powerful force, has become a degraded and theatrical performance, particularly for women in Gerty's social context. The ritual of courtship and romantic fantasy persists, but its functional significance has diminished.
constraint_indexing:constraint_classification(ulysses_chp13, piton,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% From Joyce's analytical perspective, the sentimental snare is a device for exploring the complexities of human desire and the social forces that shape individual experience. Bloom's voyeurism facilitates the fulfillment of Gerty's romantic fantasy. This is not a coercive action on his part but rather a collaborative fulfillment of Gerty's fantasies and Bloom's desires, both of which exist within the larger 'artistic' project of Joyce's work.
constraint_indexing:constraint_classification(ulysses_chp13, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% From an analytical perspective, the sentimental snare is a device for exploring the complexities of human desire and the social forces that shape individual experience.
constraint_indexing:constraint_classification(ulysses_chp13, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp13_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp13, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp13, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ulysses_chp13, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ulysses_chp13, TR),
    TR >= 0.70.

:- end_tests(ulysses_chp13_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): The constraint extracts from Gerty's sense of self and agency as she becomes increasingly invested in the romantic narrative. Suppression (0.7): Societal expectations and Gerty's internal desires suppress her ability to see Bloom and the situation clearly. Theater ratio (0.75): The performance of romantic ideals has a high theatrical element and also serves a genuine emotional function for Gerty.
 *
 * PERSPECTIVAL GAP:
 *   Gerty experiences the event as a snare, trapped by societal expectations and her own desires. The author sees the scene as a rope, creating a coordination point that fulfills the desires of the main actors. Social constructs around dating are historically a piton.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    role_of_intention,
    'To what extent is Bloom''s participation intentional exploitation versus accidental fulfillment of Gerty''s desires?',
    'Close reading of Bloom''s internal monologue and actions in the chapter, considering his motivations and awareness of Gerty''s perspective.',
    'If Bloom is intentionally manipulative, the constraint is a more severe snare. If accidental, it is a tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(role_of_intention, conceptual, 'The degree to which Bloom''s actions are intentional or accidental.').

omega_variable(
    social_expectations_impact,
    'How heavily do societal expectations weigh on Gerty''s decisions and self-perception?',
    'Analysis of the prevailing social norms and gender roles of 1904 Dublin, considering Gerty''s age, class, and social context.',
    'If social expectations are overwhelming, Gerty''s trapped perspective is justified. If less impactful, her agency is higher, and the constraint weakens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(social_expectations_impact, empirical, 'The impact of social expectations on Gerty''s decisions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp13, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ulys_tr_t0, ulysses_chp13, theater_ratio, 0, 0.6).
narrative_ontology:measurement(ulys_tr_t10, ulysses_chp13, theater_ratio, 10, 0.7).
narrative_ontology:measurement(ulys_tr_t20, ulysses_chp13, theater_ratio, 20, 0.75).

% Extraction over time
narrative_ontology:measurement(ulys_be_t0, ulysses_chp13, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(ulys_be_t10, ulysses_chp13, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(ulys_be_t20, ulysses_chp13, base_extractiveness, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
