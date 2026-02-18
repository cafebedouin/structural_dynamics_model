% ============================================================================
% CONSTRAINT STORY: platonic_coparenting_decoupling
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_platonic_coparenting_decoupling, []).

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
 * * constraint_id: platonic_coparenting_decoupling
 * human_readable: The Platonic Co-Parenting Modularization
 * domain: social/familial
 * * SUMMARY:
 * This constraint tracks the shift from traditional "Romantic-Parental"
 * integration to "Modular" parenting, where parental and romantic partnerships
 * are decoupled. It addresses the coordination problem of a person's desire
 * to have children facing the pressures of a biological clock and the
 * uncertainty of finding a suitable romantic partner in time.
 * * KEY AGENTS:
 * - The Pressured Parent: Subject (Powerless, facing biological/social time limits).
 * - The Platform Operator: Beneficiary (Institutional, providing the matching service).
 * - The Systems Analyst: Auditor (Analytical, observing the overall structure).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(platonic_coparenting_decoupling, 0.48). % High due to platform fees, legal contracts, and medical overhead (e.g., IVF).
domain_priors:suppression_score(platonic_coparenting_decoupling, 0.35).   % Moderate social/biological pressure, but alternatives (traditional path, adoption) exist.
domain_priors:theater_ratio(platonic_coparenting_decoupling, 0.15).       % Highly functional, low performativity.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(platonic_coparenting_decoupling, extractiveness, 0.48).
narrative_ontology:constraint_metric(platonic_coparenting_decoupling, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(platonic_coparenting_decoupling, theater_ratio, 0.15).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(platonic_coparenting_decoupling, tangled_rope).
narrative_ontology:human_readable(platonic_coparenting_decoupling, "The Platonic Co-Parenting Modularization").
narrative_ontology:topic_domain(platonic_coparenting_decoupling, "social/familial").

% Binary flags
narrative_ontology:has_sunset_clause(platonic_coparenting_decoupling).      % The intensive legal structure sunsets when the child reaches adulthood.
domain_priors:requires_active_enforcement(platonic_coparenting_decoupling). % Legal co-parenting contracts are essential.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(platonic_coparenting_decoupling, co_parents). % Beneficiaries of the coordination.
narrative_ontology:constraint_victim(platonic_coparenting_decoupling, co_parents).      % Victims of the financial extraction.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% A potential parent feeling intense biological clock pressure sees this as a
% costly but necessary trap to achieve their goal.
% χ = 0.48 * π(powerless, 1.5) * σ(national, 1.0) = 0.72 (Snare)
constraint_indexing:constraint_classification(platonic_coparenting_decoupling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The platform operator sees a pure coordination mechanism with negligible extraction.
% χ = 0.48 * π(institutional, -0.2) * σ(global, 1.2) = -0.1152 (Rope)
constraint_indexing:constraint_classification(platonic_coparenting_decoupling, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees a system with both a genuine coordination function and
% significant, asymmetric extraction, requiring active enforcement.
% χ = 0.48 * π(analytical, 1.15) * σ(global, 1.2) = 0.6624 (Tangled Rope)
constraint_indexing:constraint_classification(platonic_coparenting_decoupling, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE LEGAL ARCHITECT (SCAFFOLD)
% The legal framework is seen as a temporary support structure that is
% dismantled after the child reaches maturity.
constraint_indexing:constraint_classification(platonic_coparenting_decoupling, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))) :-
    narrative_ontology:has_sunset_clause(platonic_coparenting_decoupling).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(platonic_coparenting_decoupling_tests).

test(perspectival_gap) :-
    % Verify the gap between the powerless subject (Snare) and institutional beneficiary (Rope).
    constraint_indexing:constraint_classification(platonic_coparenting_decoupling, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(platonic_coparenting_decoupling, rope, context(agent_power(institutional), _, _, _)).

test(analytical_resolution_is_tangled_rope) :-
    % Verify the analytical observer resolves the gap as a Tangled Rope.
    constraint_indexing:constraint_classification(platonic_coparenting_decoupling, tangled_rope, context(agent_power(analytical), _, exit_options(analytical), _)).

test(high_extraction_threshold) :-
    narrative_ontology:constraint_metric(platonic_coparenting_decoupling, extractiveness, E),
    E >= 0.46.

:- end_tests(platonic_coparenting_decoupling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint models a modern social arrangement with significant perspectival gaps.
 * The base extraction of 0.48 reflects the high financial costs (legal, medical, platform fees)
 * required to modularize family creation.
 *
 * The Perspectival Gap is stark:
 * - The 'powerless' subject, trapped by biological and social timelines, experiences the high cost
 *   as a predatory 'Snare' (effective extraction χ=0.72).
 * - The 'institutional' platform operator, shielded from this cost, views the system as a pure
 *   'Rope', a beneficial coordination service (effective extraction χ=-0.11).
 *
 * The 'analytical' observer resolves this tension by classifying it as a 'Tangled Rope'. The system
 * provides a genuine coordination function (beneficiary exists) but also imposes significant,
 * asymmetric costs (victim exists) and requires legal enforcement to function. The 'Scaffold'
 * classification is also valid from the perspective of the legal architects, as the contracts
 * have a defined lifecycle (sunset clause).
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * The high extraction (0.48) is not purely predatory because it is coupled with a novel and
 * effective coordination function that solves a difficult modern problem. The Tangled Rope
 * classification correctly captures this duality, preventing a misclassification as a pure Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_child_outcome_pc,
    'Will long-term longitudinal data show adverse or equivalent outcomes for children raised in these arrangements compared to traditional ones?',
    'A multi-decade study tracking educational, emotional, and stability metrics for children in platonic co-parenting units versus control groups.',
    'Adverse outcomes would shift the analytical classification towards Snare; equivalent or positive outcomes would solidify it as a stable Tangled Rope.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(platonic_coparenting_decoupling, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% As the practice became more mainstream and commercialized, the financial
% extraction increased. Theater ratio remained low as the service is functional.
%
% Theater ratio over time:
narrative_ontology:measurement(pc_decoupling_tr_t0, platonic_coparenting_decoupling, theater_ratio, 0, 0.10).
narrative_ontology:measurement(pc_decoupling_tr_t5, platonic_coparenting_decoupling, theater_ratio, 5, 0.12).
narrative_ontology:measurement(pc_decoupling_tr_t10, platonic_coparenting_decoupling, theater_ratio, 10, 0.15).

% Extraction over time:
narrative_ontology:measurement(pc_decoupling_ex_t0, platonic_coparenting_decoupling, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pc_decoupling_ex_t5, platonic_coparenting_decoupling, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(pc_decoupling_ex_t10, platonic_coparenting_decoupling, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% This system is fundamentally about allocating parental duties and financial
% responsibilities, which fits the resource_allocation coordination type.
narrative_ontology:coordination_type(platonic_coparenting_decoupling, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */