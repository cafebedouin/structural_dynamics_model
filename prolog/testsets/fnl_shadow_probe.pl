% ============================================================================
% CONSTRAINT STORY: fnl_shadow_probe
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_fnl_shadow_probe, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fnl_shadow_probe
 *   human_readable: FNL Shadow Mode Probe (Physics-Washed Construction)
 *   domain: investigation/testing
 *
 * SUMMARY:
 *   A synthetic constraint designed to model a constructed system that could be
 *   mistaken for a natural law (a False Natural Law, or FNL). It combines a
 *   genuine coordination function with asymmetric extraction, enforced by an
 *   active apparatus. Its high suppression score and requirement for active
 *   enforcement are structural giveaways that it is constructed, not natural.
 *   From the beneficiary's perspective, it appears as a low-cost coordination
 *   mechanism (Rope), while for its targets, it is an extractive Tangled Rope.
 *
 * KEY AGENTS (by structural relationship):
 *   - constrained_subjects: Primary target (powerless/trapped) — bears the costs of the system.
 *   - institutional_apparatus: Primary beneficiary (institutional/arbitrage) — benefits from the coordination and extraction.
 *   - Analytical observer: Sees the full structure, including both the coordination function and the asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Moderate extraction, high enough to be felt asymmetrically.
domain_priors:base_extractiveness(fnl_shadow_probe, 0.30).
% Suppression well above mountain ceiling (0.05) -- a key indicator of construction.
domain_priors:suppression_score(fnl_shadow_probe, 0.50).
% Low theater -- the constraint is functional, not performative.
domain_priors:theater_ratio(fnl_shadow_probe, 0.10).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fnl_shadow_probe, extractiveness, 0.30).
narrative_ontology:constraint_metric(fnl_shadow_probe, suppression_requirement, 0.50).
narrative_ontology:constraint_metric(fnl_shadow_probe, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
% The constraint is correctly claimed as a Tangled Rope. If it were mis-claimed
% as a 'mountain', its metrics would trigger FNL detection.
narrative_ontology:constraint_claim(fnl_shadow_probe, tangled_rope).
narrative_ontology:human_readable(fnl_shadow_probe, "FNL Shadow Mode Probe (Physics-Washed Construction)").

% --- Binary flags ---
% Active enforcement proves this is NOT a natural law. Required for Tangled Rope.
domain_priors:requires_active_enforcement(fnl_shadow_probe).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain. Natural laws do not have
% designated beneficiaries or victims.
narrative_ontology:constraint_beneficiary(fnl_shadow_probe, institutional_apparatus).
narrative_ontology:constraint_victim(fnl_shadow_probe, constrained_subjects).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (TANGLED ROPE)
% χ = 0.30 * 1.42 (powerless/trapped) * 1.0 (national) = 0.426
% This χ value, combined with ε=0.30 and suppression=0.50, falls squarely
% into the Tangled Rope classification. The target experiences both the
% system's rules and its extractive costs.
constraint_indexing:constraint_classification(fnl_shadow_probe, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% χ = 0.30 * -0.12 (institutional/arbitrage) * 1.0 (national) = -0.036
% With near-zero effective extraction, the beneficiary perceives the system
% as a pure coordination mechanism (Rope). The extractive component is
% invisible from this perspective. This gap is a hallmark of a Tangled Rope.
constraint_indexing:constraint_classification(fnl_shadow_probe, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% χ = 0.30 * 1.15 (analytical) * 1.2 (global) = 0.414
% The analytical perspective sees the full structure: the coordination function
% (beneficiary exists), the asymmetric extraction (victim exists), and the
% active enforcement. The metrics confirm the Tangled Rope classification.
constraint_indexing:constraint_classification(fnl_shadow_probe, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fnl_shadow_probe_tests).

test(claims_tangled_rope) :-
    % Verify the constraint is correctly claimed as a tangled_rope.
    narrative_ontology:constraint_claim(fnl_shadow_probe, tangled_rope).

test(perspectival_gap_exists) :-
    % Beneficiary sees Rope, Target sees Tangled Rope.
    constraint_indexing:constraint_classification(fnl_shadow_probe, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(fnl_shadow_probe, tangled_rope, context(agent_power(powerless), _, _, _)).

test(tangled_rope_structural_properties_present) :-
    % Verify all three structural requirements for Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(fnl_shadow_probe, _),
    narrative_ontology:constraint_victim(fnl_shadow_probe, _),
    domain_priors:requires_active_enforcement(fnl_shadow_probe).

:- end_tests(fnl_shadow_probe_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics are chosen to create a clear Tangled Rope that could be
 *   mistaken for something else from limited perspectives.
 *   - Base Extractiveness (ε=0.30): High enough to be extractive, but low enough
 *     that f(d) scaling can push it into Rope territory for beneficiaries.
 *   - Suppression Score (0.50): This is the key FNL indicator. It's far too
 *     high for a Mountain (ceiling 0.05), proving construction and coercion.
 *   - The presence of requires_active_enforcement/1, constraint_beneficiary/2,
 *     and constraint_victim/2 provides incontrovertible structural evidence
 *     of an artificial, managed system, not a natural law.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the beneficiary (Rope) and the target (Tangled Rope) is
 *   central. The institutional beneficiary experiences negative effective
 *   extraction (χ = -0.036), perceiving a pure coordination subsidy. The
 *   powerless target experiences significant effective extraction (χ = 0.426),
 *   perceiving a coercive, extractive system. This difference is not a matter
 *   of opinion but a direct result of their different structural positions,
 *   which the directionality function f(d) quantifies.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: 'institutional_apparatus' benefits from the order and
 *     resources channeled by the system. With arbitrage exit, their derived
 *     directionality `d` is very low, making f(d) negative.
 *   - Victim: 'constrained_subjects' bear the costs. Being trapped in the
 *     system, their derived `d` is very high, making f(d) strongly positive.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a canonical example of why claims of "natural order" or
 *   "that's just the way things are" must be tested against structural evidence.
 *   If this Tangled Rope were mis-claimed as a Mountain, the system's FNL
 *   detection would flag the contradictions: ε > 0.25, suppression > 0.05, and
 *   the presence of enforcement/beneficiary/victim structures. The framework
 *   forces a re-classification to the structurally-correct Tangled Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_fnl_shadow_probe,
    'Is the coordination function separable from the extraction, or are they intrinsically linked?',
    'Analysis of the system architecture to see if the enforcement mechanism serves both functions simultaneously.',
    'If separable, the system could be reformed into a pure Rope. If intrinsically linked, reform is impossible without dismantling the entire structure.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(fnl_shadow_probe, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Stable metrics over time, mimicking a persistent, law-like pattern.
% Theater ratio over time (stable and low):
narrative_ontology:measurement(fnl_tr_t0, fnl_shadow_probe, theater_ratio, 0, 0.08).
narrative_ontology:measurement(fnl_tr_t5, fnl_shadow_probe, theater_ratio, 5, 0.09).
narrative_ontology:measurement(fnl_tr_t10, fnl_shadow_probe, theater_ratio, 10, 0.10).

% Extraction over time (slowly increasing but starting near mountain threshold):
narrative_ontology:measurement(fnl_ex_t0, fnl_shadow_probe, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(fnl_ex_t5, fnl_shadow_probe, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(fnl_ex_t10, fnl_shadow_probe, base_extractiveness, 10, 0.30).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(fnl_shadow_probe, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% status and exit options accurately reflects the agent relationships.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */