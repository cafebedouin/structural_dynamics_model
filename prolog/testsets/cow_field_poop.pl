% ============================================================================
% CONSTRAINT STORY: cow_field_poop
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-01
% ============================================================================

:- module(constraint_cow_field_poop, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cow_field_poop
 *   human_readable: The Cow Field Hazard (Pragmatic Avoidance)
 *   domain: social/psychological
 *
 * SUMMARY:
 *   This constraint models the presence of "poop" (negative events, social hazards,
 *   or nonsense) as an inherent and diverse feature of existence. The constraint
 *   is not the hazard itself, but the social and institutional systems of rules
 *   and norms for navigating and managing these hazards. It distinguishes between
 *   the analytical act of cataloging hazards and the pragmatic necessity of avoiding them.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Unwary Walker (powerless/trapped): Primary target — bears the immediate cost of encountering a hazard.
 *   - Effective Navigators (moderate/mobile): Primary beneficiary — benefits from the system of norms allowing successful avoidance.
 *   - Public Works Department / Urban Planner (institutional/arbitrage): Institutional beneficiary/manager — benefits from the mandate to manage the system.
 *   - The Analytical Observer (analytical/analytical): Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Encountering a hazard extracts time, emotional energy, and resources (cleaning).
% The system of norms for avoidance also requires cognitive overhead.
domain_priors:base_extractiveness(cow_field_poop, 0.40).
% The "extraordinary" detail of cataloging can suppress the "ordinary"
% necessity of simple navigation. Alternatives (e.g., systemic cleanup) are suppressed as impractical.
domain_priors:suppression_score(cow_field_poop, 0.45).
% The constraint is functional, not performative.
domain_priors:theater_ratio(cow_field_poop, 0.10).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cow_field_poop, extractiveness, 0.40).
narrative_ontology:constraint_metric(cow_field_poop, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(cow_field_poop, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(cow_field_poop, tangled_rope).
narrative_ontology:human_readable(cow_field_poop, "The Cow Field Hazard (Pragmatic Avoidance)").
narrative_ontology:topic_domain(cow_field_poop, "social/psychological").

% --- Binary flags ---
% The norms and rules for managing public spaces require active enforcement
% (e.g., fines for littering, maintenance schedules).
domain_priors:requires_active_enforcement(cow_field_poop).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(cow_field_poop, effective_navigators).
narrative_ontology:constraint_beneficiary(cow_field_poop, public_works_department).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(cow_field_poop, the_unwary_walker).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE VICTIM (THE STEPPER) - SNARE
% Agent who bears the most extraction.
constraint_indexing:constraint_classification(cow_field_poop, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE INSTITUTIONAL MANAGER (PUBLIC WORKS) - TANGLED ROPE
% Agent who manages the system.
constraint_indexing:constraint_classification(cow_field_poop, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE PRACTICAL WALKER - ROPE
% Agent with sufficient agency to navigate successfully.
constraint_indexing:constraint_classification(cow_field_poop, rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER - TANGLED ROPE
% Default analytical context. Sees both coordination and extraction.
constraint_indexing:constraint_classification(cow_field_poop, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cow_field_poop_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(cow_field_poop, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cow_field_poop, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(cow_field_poop, Type1, context(agent_power(moderate), _, _, _)),
    constraint_indexing:constraint_classification(cow_field_poop, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(cow_field_poop, Type3, context(agent_power(powerless), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(cow_field_poop_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.40) and suppression (0.45) are set to values
 *   characteristic of a Tangled Rope. The constraint has a clear coordination
 *   function (navigating public space safely) but also imposes costs and requires
 *   active enforcement, creating asymmetric outcomes. The theater ratio is low (0.10)
 *   as the problem and its management are tangible and functional.
 *
 * PERSPECTIVAL GAP:
 *   - The Unwary Walker (powerless/trapped) experiences the hazard's consequence
 *     directly as a Snare. It extracts time and dignity, and they are trapped until
 *     the mess is resolved.
 *   - The Practical Walker (moderate/mobile) sees the system of norms as a Rope.
 *     Knowledge of hazards is a tool to coordinate movement successfully.
 *   - The Public Works Department (institutional) sees a Tangled Rope. It performs a
 *     coordination function (public health, safety) but also extracts resources
 *     (taxes, fines) and enforces rules, creating an extractive layer on top of
 *     the coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: 'effective_navigators' benefit by successfully avoiding costs.
 *     The 'public_works_department' benefits by having a mandate and budget.
 *   - Victims: 'the_unwary_walker' bears the direct, unmitigated cost of failure.
 *   This clear division of costs and benefits drives the directionality calculation
 *   and explains the wide perspectival gaps.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope correctly identifies that this is not
 *   pure coordination (a Rope) nor pure extraction (a Snare). It captures the
 *   dual nature of a system that provides a genuine public good while simultaneously
 *   imposing costs and rules enforced asymmetrically. It prevents mislabeling
 *   public works as purely benevolent or purely extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    poop_catalog_utility,
    "Does 'describing every detail' of life's hazards eventually provide a 'Rope' for better avoidance and systemic mitigation, or is it a form of theatricality that distracts from pragmatic action?",
    "Audit of navigation success rates in catalogers vs. non-cataloging practitioners; effectiveness of public awareness campaigns versus infrastructure investments in hazard reduction.",
    "If beneficial: Cataloging is a Rope. If clutter: It is a Snare for time-extraction and a distraction from real solutions.",
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(poop_catalog_utility, empirical, "Is detailed hazard cataloging more effective for mitigation than pragmatic avoidance or systemic infrastructure investment?").

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(cow_field_poop, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extractiveness is < 0.46, so temporal measurements are not required.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% No Boltzmann or network data declared for this constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No directionality overrides are needed; the structural derivation from
% beneficiary/victim declarations is sufficient.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */