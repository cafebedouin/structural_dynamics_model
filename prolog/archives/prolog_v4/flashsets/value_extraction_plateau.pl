% ============================================================================
% CONSTRAINT STORY: value_extraction_plateau
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_value_extraction_plateau, []).

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
 *   constraint_id: value_extraction_plateau
 *   human_readable: The Law of Diminishing Predation
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The Law of Diminishing Predation describes a scenario where a dominant
 *   platform or institution has reached the biological or economic limit of
 *   how much surplus it can siphon from its subjects. Initially, the platform
 *   grows by attracting users and content creators and extracting a portion
 *   of the value they create. However, as the extraction rate increases,
 *   users and content creators become increasingly resistant or seek
 *   alternatives, leading to a plateau in the platform's growth and
 *   potentially its decline. Active enforcement mechanisms are required to
 *   maintain extraction levels.
 *
 * KEY AGENTS:
 *   - Dominant Platform: The primary beneficiary (institutional/arbitrage) that initially benefits from extracting value from users and content creators.
 *   - Users: The primary victims (powerless/trapped) who are subject to the platform's extractive practices.
 *   - Content Creators: Secondary victims (moderate/mobile) who rely on the platform but are also subject to its terms.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(value_extraction_plateau, 0.6).
domain_priors:suppression_score(value_extraction_plateau, 0.7).
domain_priors:theater_ratio(value_extraction_plateau, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(value_extraction_plateau, extractiveness, 0.6).
narrative_ontology:constraint_metric(value_extraction_plateau, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(value_extraction_plateau, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(value_extraction_plateau, tangled_rope).
narrative_ontology:human_readable(value_extraction_plateau, "The Law of Diminishing Predation").
narrative_ontology:topic_domain(value_extraction_plateau, "economic/technological").

domain_priors:requires_active_enforcement(value_extraction_plateau).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(value_extraction_plateau, dominant_platform).
narrative_ontology:constraint_victim(value_extraction_plateau, users).
narrative_ontology:constraint_victim(value_extraction_plateau, content_creators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Users who are heavily invested in the platform and have few viable alternatives experience the constraint as a snare. They are forced to accept increasingly unfavorable terms.
constraint_indexing:constraint_classification(value_extraction_plateau, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% Content creators who rely on the platform for income or visibility may experience it as a tangled rope. They benefit from the platform's reach but are also subject to its extractive practices.
constraint_indexing:constraint_classification(value_extraction_plateau, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% The dominant platform initially benefits from extracting value, which appears as a rope. However, as the extraction reaches its limit, the benefits plateau, and the focus shifts to maintaining its dominance.
constraint_indexing:constraint_classification(value_extraction_plateau, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a civilizational perspective, the extraction may initially drive growth but eventually degrades into a piton where the platform becomes more focused on maintaining its position than providing value. The active coordination function has atrophied.
constraint_indexing:constraint_classification(value_extraction_plateau, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(value_extraction_plateau_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(value_extraction_plateau, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(value_extraction_plateau, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(value_extraction_plateau, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(value_extraction_plateau, TR),
    TR >= 0.70.

:- end_tests(value_extraction_plateau_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): High. The platform extracts a significant portion of the value created by users and content creators. Suppression (0.70): High. Users and content creators face significant barriers to exit, such as network effects and lock-in. Theater ratio (0.30): Low. The platform primarily focuses on functional extraction rather than performative activity.
 *
 * PERSPECTIVAL GAP:
 *   Users, being relatively powerless and often trapped due to network effects, experience this as a snare. They are the target of extraction. Content creators have more options, and their perspective is mixed: a tangled rope. The dominant platform initially perceives a rope (coordination), but over time this turns into a piton as the value creation ecosystem degrades. An analytical observer with a long-term view may recognize the trajectory turning toward a piton.
 *
 * DIRECTIONALITY LOGIC:
 *   The dominant platform benefits, thus the directionality is toward it. Users and content creators are targeted, thus the directionality is away from them. The analytical observer sees the long-term atrophication of the formerly useful coordination mechanism.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_metric_validity,
    'What metric best measures ''extraction'' in this context? Is it revenue share, attention capture, or something else?',
    'Comparative analysis of different extraction metrics and their correlation with user behavior and content creator satisfaction.',
    'Different extraction metrics may lead to different classifications (e.g., Snare vs. Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_metric_validity, empirical, 'Defines the best metric for evaluating extractiveness').

omega_variable(
    alternative_platform_viability,
    'How viable are alternative platforms for users and content creators?',
    'Analysis of the growth and features of competing platforms, as well as user and content creator migration patterns.',
    'High viability of alternatives reduces the ''trapped'' nature of users and can shift the classification from Snare to Tangled Rope or even Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_platform_viability, empirical, 'Determines the viability of alternative platforms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(value_extraction_plateau, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(valu_tr_t0, value_extraction_plateau, theater_ratio, 0, 0.1).
narrative_ontology:measurement(valu_tr_t5, value_extraction_plateau, theater_ratio, 5, 0.2).
narrative_ontology:measurement(valu_tr_t10, value_extraction_plateau, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(valu_be_t0, value_extraction_plateau, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(valu_be_t5, value_extraction_plateau, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(valu_be_t10, value_extraction_plateau, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(value_extraction_plateau, global_infrastructure).
narrative_ontology:affects_constraint(value_extraction_plateau, network_effects_lockin).
narrative_ontology:affects_constraint(value_extraction_plateau, platform_governance_capture).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
