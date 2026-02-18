% ============================================================================
% CONSTRAINT STORY: cinderella_midnight_deadline
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_cinderella_midnight_deadline, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cinderella_midnight_deadline
 *   human_readable: The Fairy Godmother's Midnight Deadline
 *   domain: magical/social
 *
 * SUMMARY:
 *   A Fairy Godmother grants Cinderella a temporary, high-status transformation
 *   (coach, gown, etc.) to attend a royal ball. This magical support is subject
 *   to a rigid temporal constraint: it expires precisely at midnight, at which
 *   point all magical effects revert to their mundane origins (pumpkin, rags).
 *   The constraint is a temporary enabler with a hard, non-negotiable sunset clause.
 *
 * KEY AGENTS (by structural relationship):
 *   - Cinderella: Primary beneficiary and victim (powerless/trapped) — receives temporary social access but is subject to the absolute deadline.
 *   - The Prince: Secondary beneficiary (institutional/arbitrage) — the event enabled by the magic allows him to meet his future bride.
 *   - Fairy Godmother: Architect (analytical/arbitrage) — the source and enforcer of the magical rule.
 *   - Analytical Observer: Sees the full structure as a temporary support system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cinderella_midnight_deadline, 0.10).
domain_priors:suppression_score(cinderella_midnight_deadline, 0.90).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(cinderella_midnight_deadline, 0.12).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cinderella_midnight_deadline, extractiveness, 0.10).
narrative_ontology:constraint_metric(cinderella_midnight_deadline, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(cinderella_midnight_deadline, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
% Not a mountain; the constraint is artificially imposed by a magical agent.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(cinderella_midnight_deadline, scaffold).
narrative_ontology:human_readable(cinderella_midnight_deadline, "The Fairy Godmother's Midnight Deadline").
narrative_ontology:topic_domain(cinderella_midnight_deadline, "magical/social").

% --- Binary flags ---
narrative_ontology:has_sunset_clause(cinderella_midnight_deadline).      % Mandatory if Scaffold
% domain_priors:requires_active_enforcement(cinderella_midnight_deadline). % The magic is self-enforcing.

% --- Emergence flag (required for mountain constraints) ---
% This constraint is explicitly constructed by an agent (Fairy Godmother),
% so it does not emerge naturally.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(cinderella_midnight_deadline, cinderella).
narrative_ontology:constraint_beneficiary(cinderella_midnight_deadline, the_prince).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(cinderella_midnight_deadline, cinderella).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three)
%   Scaffold:     beneficiary + (has_sunset_clause OR no enforcement)
%   Snare:        victim required; beneficiary optional

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: CINDERELLA (THE PRIMARY TARGET/BENEFICIARY)
% For Cinderella, the deadline is an absolute limit on a temporary gift.
% It enables her attendance (coordination) but has a hard, inescapable
% endpoint. This is the classic structure of a Scaffold.
constraint_indexing:constraint_classification(cinderella_midnight_deadline, scaffold,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE PRINCE (THE SECONDARY BENEFICIARY)
% From the Prince's perspective, the magical event is a pure coordination
% mechanism that brings his ideal partner to him. The deadline's consequence
% (the lost slipper) becomes a tool for finding her. He experiences only the
% upside of the coordination function, making it a Rope.
constraint_indexing:constraint_classification(cinderella_midnight_deadline, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Analytically, the constraint is a temporary support structure with a defined
% end point, designed to solve a specific problem (Cinderella's exclusion).
% This is the definition of a Scaffold.
constraint_indexing:constraint_classification(cinderella_midnight_deadline, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cinderella_midnight_deadline_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between Cinderella (Scaffold) and the Prince (Rope).
    constraint_indexing:constraint_classification(cinderella_midnight_deadline, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cinderella_midnight_deadline, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary,
    TypeTarget == scaffold,
    TypeBeneficiary == rope.

test(scaffold_properties_validated) :-
    % A scaffold requires a sunset clause and a beneficiary.
    narrative_ontology:has_sunset_clause(cinderella_midnight_deadline),
    narrative_ontology:constraint_beneficiary(cinderella_midnight_deadline, _).

test(analytical_claim_matches) :-
    % The analytical perspective must match the constraint_claim.
    narrative_ontology:constraint_claim(cinderella_midnight_deadline, ClaimType),
    constraint_indexing:constraint_classification(cinderella_midnight_deadline, ClaimType, context(agent_power(analytical), _, _, _)).

:- end_tests(cinderella_midnight_deadline_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (0.10): The magic is a gift, not extractive. The cost is the reversion, not an ongoing extraction of value.
 *   - Suppression (0.90): The magical law is absolute and non-negotiable, overriding any alternative actions Cinderella might take to prolong her time.
 *   - Theater Ratio (0.12): The magic is highly functional, not performative.
 *   - The constraint is classified as a Scaffold because it is a temporary support structure (enabling ball attendance) with a hard sunset clause (midnight). It is not a Mountain because it is artificially created by an agent, not a natural law.
 *
 * PERSPECTIVAL GAP:
 *   - Cinderella experiences the constraint as a Scaffold. She directly benefits from the temporary support but is also directly subject to its harsh, inescapable deadline.
 *   - The Prince experiences the event enabled by the magic as a Rope. For him, it's a pure coordination mechanism that solves his problem of finding a suitable bride, with the slipper acting as the coordinating device. He sees none of the costs of the deadline, only the benefits of the encounter.
 *
 * DIRECTIONALITY LOGIC:
 *   - Cinderella is both beneficiary (gets to go to the ball) and victim (is trapped by the deadline). This dual role is characteristic of agents interacting with Scaffolds.
 *   - The Prince is a pure beneficiary, using the outcome of the constraint to his advantage.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification correctly identifies the temporary and enabling nature of the constraint. Labeling it a Mountain would be a mistake, as it has a clear author and purpose. Labeling it a pure Rope would ignore the coercive, non-negotiable deadline that Cinderella faces. The Scaffold classification captures both the enabling function and the temporary, rigid limitation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_cinderella_midnight_deadline,
    'Is the magic a gift or a test?',
    'Analysis of the Fairy Godmother''s intent and other instances of her magic. If the deadline is intentionally engineered to produce the slipper outcome, the constraint has a stronger element of manipulation.',
    'If a gift, it is a pure Scaffold. If a test, it borders on a Tangled Rope, with a hidden extractive purpose (proving worthiness).',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_cinderella_midnight_deadline, conceptual, 'Uncertainty over whether the deadline is a simple limitation (gift) or an engineered test of character (manipulation).').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(cinderella_midnight_deadline, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required as base_extractiveness (0.10) is below the 0.46 threshold.
% If this were a high-extraction constraint, data points would be added here.
%
% narrative_ontology:measurement(cinderella_midnight_deadline_tr_t0, cinderella_midnight_deadline, theater_ratio, 0, 0.12).
% narrative_ontology:measurement(cinderella_midnight_deadline_tr_t5, cinderella_midnight_deadline, theater_ratio, 5, 0.12).
% narrative_ontology:measurement(cinderella_midnight_deadline_tr_t10, cinderella_midnight_deadline, theater_ratio, 10, 0.12).
%
% narrative_ontology:measurement(cinderella_midnight_deadline_ex_t0, cinderella_midnight_deadline, base_extractiveness, 0, 0.10).
% narrative_ontology:measurement(cinderella_midnight_deadline_ex_t5, cinderella_midnight_deadline, base_extractiveness, 5, 0.10).
% narrative_ontology:measurement(cinderella_midnight_deadline_ex_t10, cinderella_midnight_deadline, base_extractiveness, 10, 0.10).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The magic provides access to a social event, a form of resource allocation.
narrative_ontology:coordination_type(cinderella_midnight_deadline, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% status and exit options accurately models the relationships.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */