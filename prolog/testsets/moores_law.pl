% ============================================================================
% CONSTRAINT STORY: moores_law
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_moores_law, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: moores_law
 *   human_readable: Moore's Law as an Industrial Convention
 *   domain: technological/economic
 *
 * SUMMARY:
 *   Moore's Law is the observation that the number of transistors on a
 *   microchip doubles approximately every two years. This story models the
 *   law not as a physical inevitability, but as a self-fulfilling prophecy
 *   or a "socially enforced" pace of innovation that coordinates the global
 *   semiconductor industry while simultaneously creating a coercive R&D
 *   treadmill and planned obsolescence.
 *
 * KEY AGENTS (by structural relationship):
 *   - Chip Fabricators (e.g., Intel, TSMC): Primary target (institutional/constrained) — bears the immense R&D cost to maintain the pace.
 *   - Platform Capitalists & Software Developers: Primary beneficiary (institutional/arbitrage) — benefits from predictable hardware gains to build more complex services.
 *   - Consumers / Legacy Infrastructure Owners: Secondary target (powerless/mobile) — benefits from cheaper compute but is subject to planned obsolescence.
 *   - Analytical Observer: Sees the full structure as a Tangled Rope of coordination and extraction.
 *
 * DUAL FORMULATION NOTE:
 * This constraint is one of 2 stories decomposed from the colloquial label "Moore's Law".
 * Decomposed because ε differs across observables (ε-invariance principle).
 * This story models the INDUSTRIAL CONVENTION (ε=0.30, Tangled Rope).
 * The related story models the PHYSICAL LIMIT:
 *   - silicon_quantum_limit (ε=0.02, Mountain)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: It extracts value through planned obsolescence and coercive R&D
% cycles. Devices lose utility rapidly as the "frontier" moves forward,
% forcing consumption and extracting capital from both consumers and producers.
domain_priors:base_extractiveness(moores_law, 0.30).

% Rationale: Alternative hardware architectures (e.g., analog, neuromorphic)
% are suppressed by the massive economies of scale and "gravity" of the
% standard silicon roadmap.
domain_priors:suppression_score(moores_law, 0.50).

% Rationale: A highly technical constraint with minimal performative theater.
domain_priors:theater_ratio(moores_law, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(moores_law, extractiveness, 0.30).
narrative_ontology:constraint_metric(moores_law, suppression_requirement, 0.50).
narrative_ontology:constraint_metric(moores_law, theater_ratio, 0.02).

% --- Constraint claim (must match analytical perspective type) ---
% Analytically, this is a Tangled Rope: it has a coordination function,
% asymmetric extraction, and requires active enforcement.
narrative_ontology:constraint_claim(moores_law, tangled_rope).
narrative_ontology:human_readable(moores_law, "Moore's Law as an Industrial Convention").
narrative_ontology:topic_domain(moores_law, "technological/economic").

% --- Binary flags ---
% Rationale: Requires active, massive capital investment and industry-wide
% coordination (e.g., via ITRS roadmap) to maintain the pace.
domain_priors:requires_active_enforcement(moores_law).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(moores_law, platform_capitalists).
narrative_ontology:constraint_beneficiary(moores_law, software_developers).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(moores_law, semiconductor_fabricators).
narrative_ontology:constraint_victim(moores_law, legacy_infrastructure_owners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE CHIP FABRICATOR (SNARE)
% For the manufacturer, the law is a Snare. It is an "unrelenting
% treadmill." If they fail to meet the doubling pace for a single cycle,
% their market position evaporates. The cost of maintaining the pace
% increases exponentially as physical limits approach.
constraint_indexing:constraint_classification(moores_law, snare,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: THE SOFTWARE DEVELOPER / PLATFORM CAPITALIST (ROPE)
% For the beneficiary, Moore's Law is a Rope. It is a predictable ladder of
% progress, a coordination mechanism that guarantees "free" hardware gains
% to offset inefficient code (Wirth's Law) and enable more complex services.
constraint_indexing:constraint_classification(moores_law, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE END USER (ROPE)
% For the user, it is also a Rope. It ensures that the computer they buy
% today will be significantly more powerful than older models, lowering the
% barrier to entry for complex software. The extraction via planned
% obsolescence is less salient than the immediate utility gain.
constraint_indexing:constraint_classification(moores_law, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The observer sees the full structure: a powerful coordination mechanism
% (Rope function) that has become coercive and extractive (Snare function)
% through active industrial enforcement. This hybrid nature is the definition
% of a Tangled Rope.
constraint_indexing:constraint_classification(moores_law, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(moores_law_tests).

test(perspectival_gap) :-
    % Verify gap between the fabricator (Snare) and beneficiary (Rope).
    constraint_indexing:constraint_classification(moores_law, snare, context(agent_power(institutional), _, constrained, _)),
    constraint_indexing:constraint_classification(moores_law, rope, context(agent_power(institutional), _, arbitrage, _)).

test(analytical_claim_matches_tangled_rope) :-
    % The analytical view must be Tangled Rope, matching the claim.
    constraint_indexing:constraint_classification(moores_law, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties) :-
    % Verify the three structural requirements for a Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(moores_law, _), % has_coordination_function
    narrative_ontology:constraint_victim(moores_law, _),     % has_asymmetric_extraction
    domain_priors:requires_active_enforcement(moores_law).

:- end_tests(moores_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The original file incorrectly claimed this was a Mountain, leading to a
 *   metric conflict. This version corrects the analysis by treating Moore's
 *   Law as a social and economic convention, not a physical one. The base
 *   metrics (ε=0.30, suppression=0.50) are appropriate for a Tangled Rope,
 *   reflecting the dual nature of coordination and extraction. The analytical
 *   claim is now `tangled_rope`, resolving the linter errors.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For beneficiaries (software developers, platform
 *   capitalists), it's a pure Rope providing predictable progress. For the
 *   chip fabricators forced to maintain the pace, it's a Snare—an
 *   inescapable, costly treadmill. End users also see a Rope, as the utility
 *   gains from new hardware overshadow the slower-moving costs of e-waste
 *   and planned obsolescence.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `platform_capitalists` and `software_developers` gain
 *     immense value from the predictable increase in computational power,
 *     allowing them to build more complex and profitable services.
 *   - Victims: `semiconductor_fabricators` bear the direct, astronomical
 *     costs of R&D. `legacy_infrastructure_owners` (including consumers)
 *     bear the indirect cost of forced obsolescence.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope correctly identifies that the
 *   coordination function is real and valuable, preventing the mislabeling
 *   of the entire system as a pure Snare. However, it also acknowledges the
 *   significant, non-consensual extraction imposed on producers and the
 *   environment, avoiding the naive view that it is a pure Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_moores_law,
    'Is the 2-year doubling pace primarily driven by market coordination (a social construct) or is it an emergent optimal path dictated by thermodynamic and economic limits (a near-Mountain)?',
    'Analysis of semiconductor R&D capital allocation cycles vs. fundamental physics breakthroughs.',
    'If social construct -> Tangled Rope is correct. If emergent optimum -> Closer to a Rope with high maintenance cost.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(moores_law, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint intensified over time. It began as a simple observation
% (Rope) and became a coercive industrial standard (Tangled Rope) as R&D
% costs to maintain the pace skyrocketed.
%
% Theater ratio over time (stable and low):
narrative_ontology:measurement(moores_law_tr_t0, moores_law, theater_ratio, 0, 0.02).
narrative_ontology:measurement(moores_law_tr_t5, moores_law, theater_ratio, 5, 0.02).
narrative_ontology:measurement(moores_law_tr_t10, moores_law, theater_ratio, 10, 0.02).

% Extraction over time (increasing due to rising R&D costs and obsolescence):
narrative_ontology:measurement(moores_law_ex_t0, moores_law, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(moores_law_ex_t5, moores_law, base_extractiveness, 5, 0.25).
narrative_ontology:measurement(moores_law_ex_t10, moores_law, base_extractiveness, 10, 0.30).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: It serves as a shared timeline and target for a global industry.
narrative_ontology:coordination_type(moores_law, information_standard).

% Network relationships: The physical limits of silicon are an upstream
% constraint that affects the viability and cost of maintaining Moore's Law.
narrative_ontology:affects_constraint(silicon_quantum_limit, moores_law).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations and exit options (constrained vs. arbitrage) correctly
% models the perspectival gaps between different institutional actors.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */