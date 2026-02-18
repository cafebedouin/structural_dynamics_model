% ============================================================================
% CONSTRAINT STORY: finite_pool_of_worry
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_finite_pool_of_worry, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: finite_pool_of_worry
 * human_readable: The Finite Pool of Worry Hypothesis
 * domain: psychological/social
 * * SUMMARY:
 * This constraint defines a psychological limit where individuals avoid dealing with multiple
 * negative events simultaneously. As cognitive load increases from immediate
 * threats (e.g., a pandemic), concern and attention for long-term existential risks
 * (e.g., climate change) are systematically redirected and neglected.
 * * KEY AGENTS:
 * - The Worried Subject: An individual whose cognitive bandwidth is exhausted by immediate safety needs.
 * - Immediate Crisis Response: The beneficiary that receives the redirected attention.
 * - Future Existential Security: The victim that suffers from the redirected attention.
 * - The Analytical Observer: A psychologist or sociologist mapping the cognitive tradeoffs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Rationale (Extraction): High (0.75). The constraint extracts the mental capacity and "future-readiness"
% of a civilization. By redirecting attention from long-term mitigation, it extracts the
% probability of a habitable future for the sake of immediate crisis management.
domain_priors:base_extractiveness(finite_pool_of_worry, 0.75).
% Rationale (Suppression): Moderate-High (0.65). The pool "redirects" attention, effectively
% suppressing the visibility of non-immediate existential risks.
domain_priors:suppression_score(finite_pool_of_worry, 0.65).
% Rationale (Theater): Low (0.15). The effect is a genuine cognitive phenomenon, not a
% performative act.
domain_priors:theater_ratio(finite_pool_of_worry, 0.15).
domain_priors:requires_active_enforcement(finite_pool_of_worry).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(finite_pool_of_worry, extractiveness, 0.75).
narrative_ontology:constraint_metric(finite_pool_of_worry, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(finite_pool_of_worry, theater_ratio, 0.15).

% Constraint self-claim (what does the constraint claim to be?)
% It is presented as a fundamental, unchangeable aspect of human psychology.
narrative_ontology:constraint_claim(finite_pool_of_worry, tangled_rope).
narrative_ontology:human_readable(finite_pool_of_worry, "The Finite Pool of Worry Hypothesis").
narrative_ontology:topic_domain(finite_pool_of_worry, "psychological/social").

% Structural property derivation hooks:
% Beneficiary: Immediate Crisis Management (receives all the attention/worry).
narrative_ontology:constraint_beneficiary(finite_pool_of_worry, immediate_crisis_response).
% Victim: Future Generations / Climate Change Mitigation (receives "neglect").
narrative_ontology:constraint_victim(finite_pool_of_worry, future_existential_security).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE WORRIED SUBJECT (SNARE)
% For the citizen, the finite pool is a "Snare." Their cognitive load is saturated by
% immediate safety needs, strangling their ability to care about long-term risks.
constraint_indexing:constraint_classification(finite_pool_of_worry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE POLICY ARCHITECT (ROPE)
% For an institutional actor, the finite pool is a "Rope"—a coordination mechanism.
% Understanding how attention is redirected allows for strategic messaging to
% align public worry with specific policy goals.
constraint_indexing:constraint_classification(finite_pool_of_worry, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE PSYCHOLOGICAL ANALYST (MOUNTAIN)
% To the scientist, the finite pool is a "Mountain"—an immutable law of human
% evolutionary psychology. We cannot escape the fact that our cognitive bandwidth
% is finite and redirected by immediate, high-volume threats.
constraint_indexing:constraint_classification(finite_pool_of_worry, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(finite_pool_of_worry_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(finite_pool_of_worry, TypePowerless,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(finite_pool_of_worry, TypeInstitutional,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(finite_pool_of_worry, TypeAnalytical,
        context(agent_power(analytical), _, _, _)),
    TypePowerless == snare,
    TypeInstitutional == rope,
    TypeAnalytical == mountain,
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    % Verify that the base extractiveness meets the high-extraction threshold for a Snare.
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(finite_pool_of_worry, ExtMetricName, E),
    E >= 0.46.

:- end_tests(finite_pool_of_worry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect the dual nature of this constraint. From an analytical, long-term
 * perspective, it's a 'Mountain'—a fixed feature of human cognition. However, in a
 * lived, biographical context, its effects are highly extractive and coercive, making
 * it a 'Snare' for individuals trying to balance immediate and future threats.
 * For institutions that can manipulate public attention, it becomes a 'Rope' for
 * coordinating societal focus. This perspectival gap is significant: what is a law of
 * nature to an analyst is a trap for a citizen and a tool for a government.
 * The high extraction (0.75) is justified because the resource being extracted is
 * 'civilizational foresight', a critical asset for long-term survival.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The system avoids mislabeling this as a pure Mountain by acknowledging the perspectival
 * classifications. While its origin may be a natural cognitive limit, its effect within a
 * media-saturated society is that of a Snare that extracts future security. The Rope
 * classification for institutional actors further highlights that this 'natural law' can be
 * instrumentally leveraged, distinguishing it from a truly inert Mountain like gravity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_finite_pool_of_worry_1,
    'Is the redirection of attention a biological necessity (Mountain) or an intentionally amplified media/political strategy (Snare)?',
    'Comparative audit of crisis-reporting frequency vs. existential-risk-reporting frequency in state vs. independent media.',
    'If necessity: Evolutionary Mountain. If strategy: Mandatrophy Snare.',
    confidence_without_resolution(medium)
).

omega_variable(
    omega_finite_pool_of_worry_2,
    'Can the pool of worry be expanded through training or technology, or is its bandwidth fixed?',
    'Longitudinal studies measuring cognitive load and long-term risk assessment in populations with and without mindfulness or data-synthesis training.',
    'If expandable: The constraint is a mutable Rope. If fixed: It remains a fundamental Mountain.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(finite_pool_of_worry, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the constraint's intensification during the 2019-2021
% pandemic, where a latent cognitive bias became an acute, highly extractive social phenomenon.
% This is required as base_extractiveness > 0.46.

% Theater ratio over time (remains low and stable):
narrative_ontology:measurement(fpw_tr_t0, finite_pool_of_worry, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fpw_tr_t5, finite_pool_of_worry, theater_ratio, 5, 0.15).
narrative_ontology:measurement(fpw_tr_t10, finite_pool_of_worry, theater_ratio, 10, 0.15).

% Extraction over time (ramps up as crisis saturates attention):
narrative_ontology:measurement(fpw_ex_t0, finite_pool_of_worry, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fpw_ex_t5, finite_pool_of_worry, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(fpw_ex_t10, finite_pool_of_worry, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% No coordination type is declared as the constraint is primarily a cognitive
% limit rather than a designed coordination mechanism.
% No network relationships are declared in this model.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */