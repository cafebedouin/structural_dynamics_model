% ============================================================================
% CONSTRAINT STORY: status_flattening_effect
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_status_flattening_effect, []).

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
 *   constraint_id: status_flattening_effect
 *   human_readable: The Algorithmic Leveler
 *   domain: social/technological/economic
 *
 * SUMMARY:
 *   The algorithmic leveler describes how digital platforms or standardized
 *   systems collapse complex social hierarchies into single metrics. While
 *   seemingly democratizing, it also creates a new form of extraction and
 *   control, potentially devaluing established expertise and manipulating
 *   behavior. New content creators may initially benefit, but their long-term
 *   success becomes algorithm-dependent.
 *
 * KEY AGENTS:
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) - gain engagement and data control.
 *   - New Content Creators: Initial beneficiaries (moderate/constrained) - gain visibility, but later become dependent.
 *   - Established Status Holders: Primary victims (powerless/trapped) - lose relative standing.
 *   - Quality Signal Fidelity: Secondary victims (powerless/trapped) - algorithmic distortion degrades the reliability of information available.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(status_flattening_effect, 0.55).
domain_priors:suppression_score(status_flattening_effect, 0.65).
domain_priors:theater_ratio(status_flattening_effect, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(status_flattening_effect, extractiveness, 0.55).
narrative_ontology:constraint_metric(status_flattening_effect, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(status_flattening_effect, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(status_flattening_effect, tangled_rope).
narrative_ontology:human_readable(status_flattening_effect, "The Algorithmic Leveler").
narrative_ontology:topic_domain(status_flattening_effect, "social/technological/economic").

domain_priors:requires_active_enforcement(status_flattening_effect).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(status_flattening_effect, platform_operators).
narrative_ontology:constraint_beneficiary(status_flattening_effect, new_content_creators).
narrative_ontology:constraint_victim(status_flattening_effect, established_status_holders).
narrative_ontology:constraint_victim(status_flattening_effect, quality_signal_fidelity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Established Status Holder (Snare) - Individuals or institutions with previously recognized expertise or status find their standing diminished as it's reduced to a simple metric alongside less qualified entities. Trapped within the system, they can't easily restore their previous status.
constraint_indexing:constraint_classification(status_flattening_effect, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Perspective 2: New Content Creators (Tangled Rope) - Newcomers who might not have previously had access to a large audience can gain rapid visibility. This is initially beneficial (Rope), but as they become dependent on the algorithm, they also become subject to its extraction (Snare).
constraint_indexing:constraint_classification(status_flattening_effect, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% Perspective 3: Platform Operator (Rope) - Benefits from increased user engagement and data collection due to the algorithmic leveler. They can monetize this engagement through advertising and data sales. Experiences it as a coordination mechanism for connecting users and content. Able to adjust algorithms for arbitrage.
constraint_indexing:constraint_classification(status_flattening_effect, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 4: Analytical Observer (Tangled Rope) - Recognizes the combined coordination and extraction effects. Sees the algorithmic leveler as simultaneously democratizing access to audiences and creating a new form of control and dependence.
constraint_indexing:constraint_classification(status_flattening_effect, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(status_flattening_effect_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(status_flattening_effect, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(status_flattening_effect, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(status_flattening_effect, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(status_flattening_effect_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate - Algorithms extract attention and data, benefiting platform operators and potentially disadvantaging established experts. Suppression (0.65): Moderate-High - Algorithms limit visibility based on metrics, suppressing diverse perspectives and reinforcing popular opinions. The theater ratio is relatively low, as the algorithms do serve a real (though often distorted) function of ranking and filtering content.
 *
 * PERSPECTIVAL GAP:
 *   The algorithmic leveler is perceived differently based on one's position. Platform operators experience it as a coordination mechanism for connecting users with content (Rope). New content creators experience it as a tangled rope - initially benefiting but ultimately becoming dependent. Established experts experience it as a snare, as their standing is diminished. The analytical observer sees both the coordination and extraction aspects.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators are primary beneficiaries (low d) due to increased engagement and data control. Established status holders are primary victims (high d) because their expertise is devalued. New content creators have a mixed relationship (moderate d), benefiting initially but becoming dependent. The directionality for each perspective influences the perceived extractiveness.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_opacity,
    'How transparent and understandable are the algorithms that determine status metrics?',
    'Independent audits of algorithm code and performance; user studies on understanding of status metrics.',
    'If opaque, extraction is hidden and harder to resist. If transparent, users can adapt and mitigate negative effects.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_opacity, empirical, 'Transparency of algorithms determining status').

omega_variable(
    status_metric_fidelity,
    'How well does the status metric reflect actual expertise, quality, or contribution?',
    'Comparison of status metrics with expert evaluations and alternative measures of quality.',
    'If low fidelity, established status is undermined and incentives for quality are reduced. If high fidelity, the leveler effect is less problematic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(status_metric_fidelity, empirical, 'Fidelity of the status metric').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(status_flattening_effect, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, status_flattening_effect, theater_ratio, 0, 0.1).
narrative_ontology:measurement(stat_tr_t5, status_flattening_effect, theater_ratio, 5, 0.2).
narrative_ontology:measurement(stat_tr_t10, status_flattening_effect, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, status_flattening_effect, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(stat_be_t5, status_flattening_effect, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(stat_be_t10, status_flattening_effect, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(status_flattening_effect, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
