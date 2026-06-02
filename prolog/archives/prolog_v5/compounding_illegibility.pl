% ============================================================================
% CONSTRAINT STORY: compounding_illegibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_compounding_illegibility, []).

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
 *   constraint_id: compounding_illegibility
 *   human_readable: Compounding Illegibility in Long-Horizon Forecasting
 *   domain: epistemology/philosophy_of_time/cognitive_science
 *
 * SUMMARY:
 *   Compounding illegibility describes the structural mismatch between
 *   exponential processes (compound interest, network effects, recursive
 *   technological improvement, climate feedback loops) and linear human
 *   intuition. The constraint operates through a mathematical fact:
 *   multiplication compounds, addition does not. A 7% annual growth rate
 *   produces 2x in 10 years, not 1.7x. This gap between linear mental models
 *   and exponential reality creates systematic forecasting errors that
 *   increase with time horizon. The constraint is downstream of
 *   perceptual_immediacy_bias (the cognitive architecture that privileges
 *   immediate over distant outcomes) but represents a distinct structural
 *   phenomenon: even agents who care about long-term outcomes struggle to
 *   comprehend exponential dynamics. The constraint coordinates human
 *   planning around time horizons where linear approximations work (1-3
 *   years) and creates specialized institutions (actuaries, climate modelers,
 *   longtermist organizations) for horizons where they fail (10+ years). Low
 *   extraction because the constraint solves a genuine cognitive problem with
 *   minimal overhead. The theater_ratio is low (0.15) because the constraint
 *   is functional: the coordination around tractable time horizons is not
 *   performative but reflects real cognitive limits and institutional needs.
 *
 * KEY AGENTS:
 *   - Individual Forecaster: Moderate agent (moderate/mobile) — benefits from coordination around short horizons where linear models work; experiences low extraction because the constraint aligns with cognitive architecture
 *   - Long-Horizon Planner: Institutional beneficiary (institutional/arbitrage) — pension funds, infrastructure planners, climate modelers who benefit from shared exponential frameworks; can arbitrage between linear communication and exponential planning
 *   - Compound-Aware Investor: Powerful beneficiary (powerful/mobile) — agents who have internalized exponential thinking and exploit others' linear mental models; mobile across time horizons
 *   - Systems Thinking Community: Organized beneficiary (organized/mobile) — groups promoting exponential literacy (EA, longtermism, systems dynamics); experience the constraint as a coordination challenge they are solving
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees the constraint as coordination between cognitive architecture and exponential processes; low extraction because the constraint creates value rather than capturing it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(compounding_illegibility, 0.18).
domain_priors:suppression_score(compounding_illegibility, 0.22).
domain_priors:theater_ratio(compounding_illegibility, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(compounding_illegibility, extractiveness, 0.18).
narrative_ontology:constraint_metric(compounding_illegibility, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(compounding_illegibility, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(compounding_illegibility, rope).
narrative_ontology:human_readable(compounding_illegibility, "Compounding Illegibility in Long-Horizon Forecasting").
narrative_ontology:topic_domain(compounding_illegibility, "epistemology/philosophy_of_time/cognitive_science").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(compounding_illegibility, long_horizon_planners).
narrative_ontology:constraint_beneficiary(compounding_illegibility, compound_aware_investors).
narrative_ontology:constraint_beneficiary(compounding_illegibility, systems_thinkers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL FORECASTER (ROPE) — Experiences compounding illegibility as a coordination problem: the constraint coordinates expectations around tractable time horizons (1-3 years) where linear approximations work well enough. The forecaster benefits from this coordination — short-horizon predictions are verifiable, career-safe, and align with institutional planning cycles. Low extraction because the constraint solves a genuine cognitive problem (exponential growth is hard to visualize) with minimal coercive overhead.
constraint_indexing:constraint_classification(compounding_illegibility, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: LONG-HORIZON PLANNER (ROPE) — Institutional actors with generational time horizons (pension funds, infrastructure planners, climate modelers) experience the constraint as coordination around shared mathematical frameworks. They benefit from the constraint's existence: it creates a common language (compound annual growth rate, discount rates, exponential models) that enables coordination despite the underlying illegibility. Arbitrage exit because they can switch between linear approximations for communication and exponential models for internal planning.
constraint_indexing:constraint_classification(compounding_illegibility, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: COMPOUND-AWARE INVESTOR (ROPE) — Agents who have internalized exponential thinking (through education, experience, or professional training) experience the constraint as pure coordination: the illegibility creates predictable mispricing in markets and institutions, which they can exploit. They benefit from others' linear mental models while using exponential models themselves. Mobile exit because they can choose which time horizons to operate on.
constraint_indexing:constraint_classification(compounding_illegibility, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: SYSTEMS THINKING COMMUNITY (ROPE) — Organized groups promoting exponential literacy (effective altruism, longtermism, systems dynamics) see the constraint as a coordination challenge they are actively solving through education and tooling. They experience low extraction because they have both the conceptual frameworks and the institutional support to think in exponential terms. The constraint coordinates their efforts around a shared problem.
constraint_indexing:constraint_classification(compounding_illegibility, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE) — From a civilizational perspective, compounding illegibility is a coordination mechanism that aligns human planning with human cognitive architecture. The constraint is not extractive — it reflects a genuine mismatch between exponential processes (compound growth, network effects, recursive improvement) and linear intuition (evolved for immediate survival, not decade-scale planning). The coordination function is: focus collective attention on time horizons where mental models work, and develop specialized tools/institutions for longer horizons. Low suppression because agents can learn exponential thinking; low extraction because the constraint creates value (predictable planning horizons) rather than capturing it.
constraint_indexing:constraint_classification(compounding_illegibility, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(compounding_illegibility_tests).
:- end_tests(compounding_illegibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The constraint creates a coordination benefit (predictable planning horizons, shared frameworks for long-term thinking) that exceeds its costs (forecasting errors, missed opportunities). The extraction is not zero — some agents (compound-aware investors, long-horizon institutions) benefit more than others — but the asymmetry is mild and reflects genuine differences in capability rather than coercive capture. Suppression (0.22): Low. Agents can learn exponential thinking through education, use computational tools to substitute for intuition, or join institutions with generational time horizons. The barriers are real (exponential intuition is cognitively demanding, tools require access and training) but surmountable. Theater ratio (0.15): Low. The coordination around short time horizons is functional, not performative. Institutions genuinely plan in 1-3 year cycles because that is where their mental models and accountability structures work. The constraint is not maintained through ritual but through the ongoing mismatch between exponential processes and linear cognition.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap — all perspectives classify as Rope. The gap that exists is in the magnitude of benefit, not the type of constraint. Individual forecasters experience the constraint as a mild coordination aid (low benefit, low cost). Long-horizon planners experience it as a valuable coordination framework (moderate benefit, low cost). Compound-aware investors experience it as an exploitable coordination pattern (high benefit, low cost). Systems thinkers experience it as a coordination challenge they are solving (moderate benefit, moderate cost). The analytical observer sees the constraint as fundamental coordination between cognitive architecture and exponential processes (high structural benefit, low cost). The uniformity of classification reflects that the constraint is genuinely coordinative — it solves a real problem (exponential illegibility) with minimal extractive overhead.
 *
 * DIRECTIONALITY LOGIC:
 *   All perspectives classify as Rope because all agents are beneficiaries of the coordination function. Individual forecasters benefit from tractable time horizons. Long-horizon planners benefit from shared exponential frameworks. Compound-aware investors benefit from predictable mispricing. Systems thinkers benefit from a shared problem to solve. The analytical observer sees the constraint as coordinating human planning with human cognitive architecture. No agent is a victim — the constraint does not extract from anyone; it coordinates around a genuine cognitive limit. The low extraction reflects that the constraint creates value (enables planning despite exponential illegibility) rather than capturing it. Directionality values are low across all perspectives (d ≈ 0.10-0.25) because all agents experience net benefit from the coordination, though the magnitude varies by capability and time horizon.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that low-extraction coordination (Rope) can exist even when the underlying phenomenon (exponential growth) is mathematically precise and the cognitive limit (linear intuition) is universal. The constraint is not a Mountain because agents can learn exponential thinking and build tools to substitute for intuition — the illegibility is surmountable, not immutable. The constraint is not a Snare because it does not extract from anyone — all agents benefit from the coordination around tractable time horizons. The constraint is not a Tangled Rope because there are no victims — the asymmetry in benefit (compound-aware investors gain more than linear thinkers) reflects genuine capability differences, not coercive extraction. The constraint is Rope from all perspectives because it coordinates human planning with human cognitive architecture in a way that creates value for all participants.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    educational_intervention_effectiveness,
    'Can educational interventions reliably shift individuals from linear to exponential mental models for long-horizon forecasting?',
    'Longitudinal studies tracking forecasting accuracy before and after exponential thinking training; retention studies measuring whether exponential intuition persists under cognitive load',
    'If effective: constraint is purely coordination (Rope confirmed). If ineffective: constraint may have Mountain-like properties — cognitive architecture may impose hard limits on exponential intuition regardless of training.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(educational_intervention_effectiveness, empirical, 'Whether exponential thinking can be reliably taught and retained').

omega_variable(
    tooling_substitution_threshold,
    'At what point do computational tools (spreadsheets, simulators, AI forecasting assistants) fully substitute for exponential intuition?',
    'Comparative studies of forecasting accuracy: tool-assisted linear thinkers vs unaided exponential thinkers; identification of decision contexts where tools cannot substitute for intuition (real-time, high-stakes, tool-unavailable scenarios)',
    'If tools fully substitute: constraint becomes Scaffold (temporary coordination problem being solved by technology). If tools cannot substitute in critical contexts: constraint remains Rope (permanent coordination around cognitive limits).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tooling_substitution_threshold, empirical, 'Whether computational tools can fully replace exponential intuition').

omega_variable(
    cultural_variance_in_time_perception,
    'Do cultures with different temporal orientations (cyclical vs linear, long-term vs short-term) show different susceptibility to compounding illegibility?',
    'Cross-cultural studies of forecasting accuracy for exponential processes; anthropological analysis of indigenous long-term planning practices (e.g., Iroquois seven-generation principle)',
    'If significant variance exists: constraint is culturally contingent (more extractive in short-term-oriented cultures). If universal: constraint reflects deeper cognitive architecture (Mountain-like from immediate/biographical perspectives).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cultural_variance_in_time_perception, empirical, 'Whether temporal orientation affects exponential comprehension').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(compounding_illegibility, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_illeg_tr_t0, compounding_illegibility, theater_ratio, 0, 0.12).
narrative_ontology:measurement(comp_illeg_tr_t5, compounding_illegibility, theater_ratio, 5, 0.14).
narrative_ontology:measurement(comp_illeg_tr_t10, compounding_illegibility, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(comp_illeg_be_t0, compounding_illegibility, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(comp_illeg_be_t5, compounding_illegibility, base_extractiveness, 5, 0.17).
narrative_ontology:measurement(comp_illeg_be_t10, compounding_illegibility, base_extractiveness, 10, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(compounding_illegibility, information_standard).

% DUAL FORMULATION NOTE:
% Compounding illegibility is downstream of perceptual_immediacy_bias (the cognitive architecture that privileges immediate over distant outcomes) but represents a distinct structural constraint. Perceptual immediacy bias is a Mountain (immutable cognitive architecture). Compounding illegibility is a Rope (coordination around the consequences of that architecture for exponential processes). The upstream constraint (immediacy bias) is unchangeable; the downstream constraint (compounding illegibility) is a coordination solution to that unchangeability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
