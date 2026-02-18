% ============================================================================
% CONSTRAINT STORY: path_dependence_lock_in
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_path_dependence_lock_in, []).

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
 * * constraint_id: path_dependence_lock_in
 * human_readable: The QWERTY Inertia Trap
 * domain: technological/economic
 * * SUMMARY:
 * This constraint represents the structural inability to pivot to a superior
 * standard because the accumulated network effects and sunk costs of the
 * current (inferior) path make the cost of transition prohibitive. It
 * functions as a Mountain for users but is maintained as a Rope for the
 * coordination of the existing market ecosystem.
 * * KEY AGENTS:
 * - New Market Entrant: Subject (Powerless)
 * - Legacy Standard Holder: Beneficiary (Institutional)
 * - Systems Archeologist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(path_dependence_lock_in, 0.65). % High extraction from the "competence tax" of using a suboptimal standard.
domain_priors:suppression_score(path_dependence_lock_in, 0.78).   % Alternatives are visible but economically infeasible due to network effects.
domain_priors:theater_ratio(path_dependence_lock_in, 0.45).       % Moderate theater; the standard is still functional, just suboptimal.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(path_dependence_lock_in, extractiveness, 0.65).
narrative_ontology:constraint_metric(path_dependence_lock_in, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(path_dependence_lock_in, theater_ratio, 0.45).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(path_dependence_lock_in, tangled_rope).
narrative_ontology:human_readable(path_dependence_lock_in, "The QWERTY Inertia Trap").
narrative_ontology:topic_domain(path_dependence_lock_in, "technological/economic").

% Binary flags
domain_priors:requires_active_enforcement(path_dependence_lock_in). % Required for Tangled Rope. Enforcement is economic/structural, not physical.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(path_dependence_lock_in, legacy_standard_holders).
narrative_ontology:constraint_victim(path_dependence_lock_in, new_market_entrants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (MOUNTAIN)
% To the powerless individual, the path-dependence is an immutable law of the environment.
% χ = 0.65 * 1.5 (powerless) * 1.0 (national) = 0.975. High extraction and suppression feel like a Mountain.
constraint_indexing:constraint_classification(path_dependence_lock_in, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Institutional actors view the lock-in as a Rope—a reliable coordination standard.
% χ = 0.65 * -0.2 (institutional) * 1.2 (global) = -0.156. Negative effective extraction means it's a net benefit.
constraint_indexing:constraint_classification(path_dependence_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% High base extraction, suppression, and the presence of both coordination (beneficiaries)
% and asymmetric extraction (victims) with active enforcement define a Tangled Rope.
constraint_indexing:constraint_classification(path_dependence_lock_in, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(path_dependence_lock_in_tests).

test(perspectival_gap) :-
    % Verify the constraint is a Mountain for the powerless but a Rope for the institution.
    constraint_indexing:constraint_classification(path_dependence_lock_in, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(path_dependence_lock_in, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(path_dependence_lock_in, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    narrative_ontology:constraint_metric(path_dependence_lock_in, extractiveness, E),
    (E =< 0.15 -> true ; E >= 0.46). % Ensure it's either low extraction or high extraction.

test(tangled_rope_structural_properties) :-
    % Verify all three required properties for Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(path_dependence_lock_in, _),
    narrative_ontology:constraint_victim(path_dependence_lock_in, _),
    domain_priors:requires_active_enforcement(path_dependence_lock_in).

:- end_tests(path_dependence_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.65) represents the "shadow cost" or "competence tax"
 * of using a suboptimal legacy standard compared to a theoretically superior
 * alternative. The suppression score (0.78) reflects the immense economic
 * and network-effect barriers that prevent alternatives from gaining traction.
 *
 * PERSPECTIVAL GAP:
 * The Individual feels a Mountain because the "gravity" of the network
 * effect is inescapable within a biographical timeframe. The Institution sees a
 * Rope because the standard provides a predictable, low-friction environment for
 * global trade and development, where the benefits of coordination outweigh the
 * costs of suboptimality.
 *
 * [RESOLVED MANDATROPHY]:
 * Resolved via Tangled Rope classification. This recognizes that the lock-in
 * is not just a "trap" (Snare) but a foundational coordination layer that
 * enables economic activity, even while it extracts value and suppresses
 * innovation. It has both a genuine coordination function and an asymmetric
 * extractive one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_lock_in_origin,
    'Is the lock-in a result of natural path-contingency (Mountain) or intentional anti-competitive design (Snare)?',
    'Historical standard-setting minutes and early-market adoption patterns.',
    'If contingent: Mountain-like origin. If designed: Snare of architecture.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(path_dependence_lock_in, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint has high extraction (0.65 > 0.46), requiring temporal data.
% The model shows extraction growing as the standard becomes entrenched and
% prevents superior alternatives from emerging. Theater also grows as
% maintaining the standard becomes less about pure function and more about
% preserving the status quo.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(pdl_tr_t0, path_dependence_lock_in, theater_ratio, 0, 0.10).
narrative_ontology:measurement(pdl_tr_t5, path_dependence_lock_in, theater_ratio, 5, 0.25).
narrative_ontology:measurement(pdl_tr_t10, path_dependence_lock_in, theater_ratio, 10, 0.45).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(pdl_ex_t0, path_dependence_lock_in, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(pdl_ex_t5, path_dependence_lock_in, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(pdl_ex_t10, path_dependence_lock_in, base_extractiveness, 10, 0.65).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Path dependence is a classic example of an information standard.
narrative_ontology:coordination_type(path_dependence_lock_in, information_standard).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */