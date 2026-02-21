% ============================================================================
% CONSTRAINT STORY: msgs_asset_bundling
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-28
% ============================================================================

:- module(constraint_msgs_asset_bundling, []).

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
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: msgs_asset_bundling
 *   human_readable: "Bundled Ownership of Knicks and Rangers under MSG Sports"
 *   domain: economic
 *
 * SUMMARY:
 *   The corporate structure of Madison Square Garden Sports (MSGS) combines two
 *   distinct, high-value assets—the New York Knicks (NBA) and New York Rangers
 *   (NHL)—into a single publicly traded stock. This structure forces investors
 *   who wish to invest in one team to also invest in the other, creating a
 *   "conglomerate discount" where the combined market capitalization is less
 *   than the sum of the teams' individual valuations. The potential split of
 *   the company reveals the nature of this bundling as a constraint.
 *
 * KEY AGENTS (by structural relationship):
 *   - Minority Shareholders: Primary target (powerless/trapped) — bear the cost of the conglomerate discount, unable to invest in a single team.
 *   - MSG Sports Majority Ownership (Dolan Family): Primary beneficiary (institutional/arbitrage) — benefits from simplified governance, enhanced control, and strategic optionality afforded by the bundled structure.
 *   - Market Analysts & Activist Investors: Secondary actors (analytical/mobile) — identify and publicize the value gap created by the constraint.
 *   - Deferential Realism System: Analytical observer — sees the full structure as a hybrid of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(msgs_asset_bundling, 0.48). % Represents the value lost to minority shareholders via the conglomerate discount.
domain_priors:suppression_score(msgs_asset_bundling, 0.65).   % Structural property (raw, unscaled). High suppression as there is no alternative way to invest in just one team.
domain_priors:theater_ratio(msgs_asset_bundling, 0.15).       % Low theater; the structure has a genuine, non-performative corporate function.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(msgs_asset_bundling, extractiveness, 0.48).
narrative_ontology:constraint_metric(msgs_asset_bundling, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(msgs_asset_bundling, theater_ratio, 0.15).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(msgs_asset_bundling, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(msgs_asset_bundling). % Maintained by corporate charter and board governance. Required for Tangled Rope.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(msgs_asset_bundling, msgs_majority_ownership).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(msgs_asset_bundling, minority_shareholders).

% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three) -> MET

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (MINORITY SHAREHOLDERS)
% Agent who bears the most extraction. The conglomerate discount directly reduces
% the value of their holdings. They are trapped in the bundled structure.
% Engine derives d from victim + trapped exit → d≈0.95 → f(d)≈1.42 → high χ.
% χ = 0.48 * f(0.95) * σ(national) ≈ 0.48 * 1.42 * 1.0 ≈ 0.68. This exceeds
% the Snare threshold (χ ≥ 0.66).
constraint_indexing:constraint_classification(msgs_asset_bundling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (MAJORITY OWNERSHIP)
% Agent who benefits from control and strategic optionality. They have arbitrage
% exit as they can initiate the very split being considered to unlock value.
% Engine derives d from beneficiary + arbitrage exit → d≈0.05 → f(d)≈-0.12 → negative χ.
% The negative extraction indicates a subsidy, classifying it as a Rope.
constraint_indexing:constraint_classification(msgs_asset_bundling, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The system's default analytical view, which recognizes both the coordination
% function and the asymmetric extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15. Global scope amplifies χ.
% χ = 0.48 * f(0.72) * σ(global) ≈ 0.48 * 1.15 * 1.2 ≈ 0.66.
% At this boundary, the structural gates determine the classification.
% The presence of beneficiary, victim, and enforcement fires the Tangled Rope gate.
constraint_indexing:constraint_classification(msgs_asset_bundling, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(msgs_asset_bundling_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target (Snare) and beneficiary (Rope).
    constraint_indexing:constraint_classification(msgs_asset_bundling, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(msgs_asset_bundling, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    format('Perspectival gap validated: powerless/trapped sees Snare, institutional/arbitrage sees Rope.~n').

test(analytical_claim_matches_type) :-
    narrative_ontology:constraint_claim(msgs_asset_bundling, Claim),
    constraint_indexing:constraint_classification(msgs_asset_bundling, Claim, context(agent_power(analytical), _, _, _)),
    format('Analytical claim matches classification type: ~w.~n', [Claim]).

test(tangled_rope_gates_met) :-
    % Verify all three conditions for Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(msgs_asset_bundling, _),
    narrative_ontology:constraint_victim(msgs_asset_bundling, _),
    domain_priors:requires_active_enforcement(msgs_asset_bundling),
    format('Tangled Rope structural gates (beneficiary, victim, enforcement) are met.~n').

:- end_tests(msgs_asset_bundling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it possesses both a
 *   genuine coordination function and a significant, asymmetric extraction component.
 *   The coordination is the unified corporate governance of two major sports franchises.
 *   The extraction (ε=0.48) is the financial value lost by minority shareholders
 *   due to the "conglomerate discount," a well-documented phenomenon where bundled
 *   assets trade at a lower value than the sum of their parts. The high suppression
 *   (0.65) reflects the fact that investors have no alternative mechanism to gain
 *   pure-play exposure to either the Knicks or the Rangers.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For minority shareholders (powerless, trapped), the structure
 *   is a Snare. Their capital is subject to a structural discount they cannot avoid,
 *   suppressing the value of their investment. For the majority owners (institutional,
 *   arbitrage), it is a Rope. The structure provides them with enhanced control,
 *   strategic flexibility, and the ability to choose the moment to "unlock value"
 *   by splitting the company, an option unavailable to others. They experience it
 *   as a useful tool, not an extractive trap.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `msgs_majority_ownership` benefits from the control and stability
 *     the bundled structure provides. The conglomerate discount makes a hostile
 *     takeover more difficult and expensive, consolidating their control.
 *   - Victim: `minority_shareholders` bear the direct financial cost. The value of
 *     their investment is structurally suppressed relative to the underlying assets.
 *   The engine correctly derives a low directionality `d` for the owners (leading
 *   to a Rope classification) and a high `d` for the shareholders (leading to a
 *   Snare classification).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification avoids two potential errors. A pure Rope classification would
 *   erroneously label the structure as simple "corporate coordination," ignoring the
 *   measurable extraction imposed on one class of stakeholders. A pure Snare
 *   classification would miss the legitimate governance and administrative functions
 *   the single corporate entity provides. The Tangled Rope designation correctly
 *   captures this hybrid nature, identifying a system that performs a coordination
 *   role but does so in a way that creates and distributes value asymmetrically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve

omega_variable(
    omega_msgs_asset_bundling,
    'Is the conglomerate discount an intentional tool for control consolidation or an incidental byproduct of a legacy corporate structure?',
    'Analysis of internal board minutes and historical strategy documents from the time of the company''s formation and subsequent structuring.',
    'If intentional, the base extractiveness (ε) could be modeled as higher, reflecting deliberate value suppression. If incidental, it represents a form of institutional inertia rather than active extraction strategy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(omega_msgs_asset_bundling, empirical, 'Disambiguating intent (control) vs. inertia (convenience) in corporate structure.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(msgs_asset_bundling, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. The base extractiveness (ε > 0.46)
% requires this. We model the conglomerate discount (extraction) growing over
% time as the valuations of the two teams diverged and the inefficiency of the
% bundling became more apparent to the market.
% Theater remains low as the structure is functional.

% Theater ratio over time (metric_substitution detection):
narrative_ontology:measurement(msgs_asset_bundling_tr_t0, msgs_asset_bundling, theater_ratio, 0, 0.10).
narrative_ontology:measurement(msgs_asset_bundling_tr_t5, msgs_asset_bundling, theater_ratio, 5, 0.12).
narrative_ontology:measurement(msgs_asset_bundling_tr_t10, msgs_asset_bundling, theater_ratio, 10, 0.15).

% Extraction over time (extraction_accumulation detection):
narrative_ontology:measurement(msgs_asset_bundling_ex_t0, msgs_asset_bundling, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(msgs_asset_bundling_ex_t5, msgs_asset_bundling, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(msgs_asset_bundling_ex_t10, msgs_asset_bundling, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The bundling acts as a high-level resource allocation and governance mechanism.
narrative_ontology:coordination_type(msgs_asset_bundling, resource_allocation).

% Network relationships (structural influence edges)
% The bundling is related to other corporate structures within the MSG ecosystem.
narrative_ontology:affects_constraint(msgs_asset_bundling, msg_entertainment_synergies).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */