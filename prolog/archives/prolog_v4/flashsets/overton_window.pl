% ============================================================================
% CONSTRAINT STORY: overton_window
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_overton_window, []).

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
 *   constraint_id: overton_window
 *   human_readable: The Overton Window of Political Discourse
 *   domain: political/social
 *
 * SUMMARY:
 *   The Overton Window, representing the range of acceptable political ideas,
 *   shapes public discourse. Political elites and media outlets benefit by
 *   controlling the narrative, while fringe groups and the quality of public
 *   discourse suffer due to suppression of alternative viewpoints.
 *
 * KEY AGENTS:
 *   - Political Elites: (institutional/arbitrage) Primary beneficiaries, shaping the window.
 *   - Media Outlets: (powerful/mobile) Amplifying and reinforcing acceptable views.
 *   - Fringe Groups: (powerless/trapped) Victims of suppression, unable to access mainstream discourse.
 *   - Public Discourse Quality: (powerless/trapped) Suffers from the limitation of acceptable ideas.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(overton_window, 0.6).
domain_priors:suppression_score(overton_window, 0.7).
domain_priors:theater_ratio(overton_window, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(overton_window, extractiveness, 0.6).
narrative_ontology:constraint_metric(overton_window, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(overton_window, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(overton_window, tangled_rope).
narrative_ontology:human_readable(overton_window, "The Overton Window of Political Discourse").
narrative_ontology:topic_domain(overton_window, "political/social").

domain_priors:requires_active_enforcement(overton_window).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(overton_window, political_elites).
narrative_ontology:constraint_beneficiary(overton_window, media_outlets).
narrative_ontology:constraint_victim(overton_window, fringe_groups).
narrative_ontology:constraint_victim(overton_window, public_discourse_quality).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Fringe groups are trapped as their views are suppressed, limiting their access to mainstream discourse.
constraint_indexing:constraint_classification(overton_window, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% The general public is constrained by the limited range of acceptable discourse, but also benefits from the perceived stability and consensus offered by the Overton Window.
constraint_indexing:constraint_classification(overton_window, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Political elites benefit by shaping the discourse, maintaining power and control over the range of acceptable policies.
constraint_indexing:constraint_classification(overton_window, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Media outlets benefit from increased viewership and influence by amplifying views within the Overton Window, but are constrained by the need to maintain perceived objectivity.
constraint_indexing:constraint_classification(overton_window, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% From a broad analytical perspective, the Overton Window reflects the interplay of power, discourse, and social acceptability, highlighting the manufactured nature of political consensus.
constraint_indexing:constraint_classification(overton_window, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(overton_window_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(overton_window, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(overton_window, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(overton_window, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(overton_window_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Overton Window extracts from fringe groups and the quality of public discourse (0.60) through suppression (0.70). It benefits political elites and media outlets by solidifying their power and influence, but also requires active enforcement to maintain the boundaries of acceptable discourse. The low theater ratio (0.30) suggests that while some performative elements exist, the enforcement is largely structural and ongoing.
 *
 * PERSPECTIVAL GAP:
 *   Fringe groups experience the Overton Window as a snare, as it limits their access to mainstream discourse and suppresses their views. The general public, media outlets and political elites experience it differently depending on their power level and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Political elites are beneficiaries as they shape the range of acceptable political ideas, enhancing their power. Media outlets benefit by amplifying those views. Fringe groups are victims as the scope limits their access to mainstream discourse. The analytical observer can assess both the benefits and the extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    window_definition,
    'What are the specific mechanisms by which the Overton Window is defined and maintained?',
    'Analysis of media coverage, policy debates, and public opinion surveys.',
    'Understanding the specific mechanisms will reveal whether the Overton Window is a naturally occurring phenomenon or actively manipulated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(window_definition, empirical, 'Defines mechanisms behind window definition.').

omega_variable(
    elite_influence,
    'To what extent do political and economic elites influence the Overton Window?',
    'Examination of lobbying efforts, campaign finance data, and media ownership patterns.',
    'Determining the extent of elite influence will reveal whether the Overton Window primarily serves the interests of the powerful or the general public.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_influence, empirical, 'Quantifies influence by elites').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(overton_window, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(over_tr_t0, overton_window, theater_ratio, 0, 0.2).
narrative_ontology:measurement(over_tr_t5, overton_window, theater_ratio, 5, 0.3).
narrative_ontology:measurement(over_tr_t10, overton_window, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(over_be_t0, overton_window, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(over_be_t5, overton_window, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(over_be_t10, overton_window, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(overton_window, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
