% ============================================================================
% CONSTRAINT STORY: ghost_fishing_gear
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_ghost_fishing_gear, []).

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
    constraint_indexing:directionality_override/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ghost_fishing_gear
 *   human_readable: Persistence of Abandoned, Lost, or Discarded Fishing Gear (ALDFG)
 *   domain: ecological/economic
 *
 * SUMMARY:
 *   Industrial fishing operations frequently lose or abandon gear (nets, traps, lines) at sea.
 *   Made of durable plastics, this "ghost gear" continues to indiscriminately catch and kill
 *   marine life for decades, depleting fish stocks and damaging ecosystems. The practice
 *   persists because operators can externalize the costs of gear loss and retrieval, effectively
 *   transferring a massive ecological debt onto the commons.
 *
 * KEY AGENTS (by structural relationship):
 *   - Marine Ecosystems & Small-Scale Fishers: Primary target (powerless/trapped) — bear the full ecological and economic cost of depleted stocks and damaged environments.
 *   - Industrial Fishing Operators: Primary beneficiary (institutional/arbitrage) — benefit from avoiding the costs of gear retrieval, replacement, and disposal.
 *   - National/International Regulators: Secondary actor (institutional/constrained) — tasked with managing the commons but often lack enforcement capability or are captured by industry interests.
 *   - Analytical Observer: Sees the full structure of cost externalization.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ghost_fishing_gear, 0.75).
domain_priors:suppression_score(ghost_fishing_gear, 0.80).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(ghost_fishing_gear, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ghost_fishing_gear, extractiveness, 0.75).
narrative_ontology:constraint_metric(ghost_fishing_gear, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(ghost_fishing_gear, theater_ratio, 0.15).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(ghost_fishing_gear, snare).

% --- Binary flags ---
domain_priors:requires_active_enforcement(ghost_fishing_gear). % Preventing this requires enforcement.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(ghost_fishing_gear, industrial_fishing_operators).
narrative_ontology:constraint_beneficiary(ghost_fishing_gear, fishing_gear_manufacturers).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(ghost_fishing_gear, marine_ecosystems).
narrative_ontology:constraint_victim(ghost_fishing_gear, small_scale_fishers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SMALL-SCALE FISHER)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(ghost_fishing_gear, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (INDUSTRIAL OPERATOR)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(ghost_fishing_gear, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context. Sees the high base extraction and suppression.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15. With global scope σ=1.2,
% χ = 0.75 * 1.15 * 1.2 ≈ 1.035. This is a clear Snare.
constraint_indexing:constraint_classification(ghost_fishing_gear, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---
% The dynamic between the regulator and the regulated industry.

% Perspective 4A: Captured/Ineffectual Regulator
% Tasked with preventing this but unable to due to political pressure, lack
% of resources, or regulatory capture. From their view, it's a problem they
% are forced to manage, not a benefit. Constrained exit means they can't
% easily opt-out. d will be derived higher than the industry's.
% It appears as a messy, high-extraction problem: a Tangled Rope or Snare.
constraint_indexing:constraint_classification(ghost_fishing_gear, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ghost_fishing_gear_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify the core perspectival gap between the small-scale fisher and the industrial operator.
    constraint_indexing:constraint_classification(ghost_fishing_gear, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(ghost_fishing_gear, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(inter_institutional_gap) :-
    % Verify the gap between the captured regulator and the beneficiary industry.
    constraint_indexing:constraint_classification(ghost_fishing_gear, tangled_rope, context(agent_power(institutional), _, exit_options(constrained), _)),
    constraint_indexing:constraint_classification(ghost_fishing_gear, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(snare_threshold_validation) :-
    domain_priors:base_extractiveness(ghost_fishing_gear, E),
    domain_priors:suppression_score(ghost_fishing_gear, S),
    E >= 0.46,
    S >= 0.60.

:- end_tests(ghost_fishing_gear_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.75): Extremely high. This represents the value of marine life destroyed
 *     and ecosystem services lost due to derelict gear. The cost is fully externalized from the
 *     perpetrator to the commons.
 *   - Suppression (S=0.80): Also very high. The system of global industrial fishing actively
 *     suppresses alternatives like biodegradable gear, universal gear tracking, and producer-
 *     responsibility laws due to cost. The inertia of the current system makes alternatives
 *     structurally inaccessible for most of the market.
 *   - Analytical Claim (Snare): The combination of high ε, high S, and a clear victim-beneficiary
 *     dynamic makes this a canonical Snare. The "coordination" function is merely cost-avoidance,
 *     not a genuine collective action solution.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the industrial operator (beneficiary), the ability to lose gear
 *   without penalty is a feature, reducing operational complexity and cost. It functions as a
 *   Rope, coordinating their internal logistics at the expense of the environment. For the
 *   small-scale fisher (victim), whose gear and livelihood are destroyed by derelict nets, and for
 *   the ecosystem itself, it is a lethal, indiscriminate trap with no escape—a perfect Snare.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `industrial_fishing_operators` directly benefit from cost externalization.
 *     `fishing_gear_manufacturers` benefit from a constant replacement market. Their `arbitrage`
 *     exit options give them low directionality (`d`), resulting in a low or negative `χ` and a Rope
 *     classification from their perspective.
 *   - Victims: `marine_ecosystems` and `small_scale_fishers` bear the direct costs. Their `trapped`
 *     status gives them a high `d` value, resulting in a very high `χ` and a Snare classification.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The regulator vs. regulated dynamic is key. Both are `institutional` actors, but their exit
 *   options differ. The industry has `arbitrage` exit (capital flight, lobbying, moving operations).
 *   The regulator has `constrained` exit (bound by mandate, political pressure, and law). This
 *   difference in exit options is what allows the engine to derive a different `d` and thus a
 *   different classification. The regulator sees a complex problem (Tangled Rope), while the
 *   industry sees a simple operational benefit (Rope).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the phenomenon as pure extraction (Snare) from a
 *   systemic view, avoiding the industry's framing of it as an unavoidable operational cost
 *   ("coordination"). The Deferential Realism framework, by indexing to the victim's perspective,
 *   pierces the veil of this cost externalization and labels it for what it is: a wealth transfer
 *   from the commons to a specific set of actors. [RESOLVED MANDATROPHY]
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_ghost_fishing_gear,
    'Is the total, long-term economic damage from ghost gear (including ecosystem service loss) greater than the global cost of implementing preventative measures (e.g., biodegradable gear, universal tracking)?',
    'A comprehensive global economic study comparing the total cost of inaction vs. the cost of systemic technological and regulatory intervention.',
    'If damage > cost, the constraint is a net-negative economic artifact maintained by regulatory failure. If damage < cost, it represents a brutal but economically "rational" (in a narrow sense) trade-off under current market logic.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(ghost_fishing_gear, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This problem intensified with the post-WWII popularization of synthetic, non-biodegradable
% materials for fishing gear. The measurements model this escalation.
% (Interval 0 = ~1960, Interval 10 = ~2020s)

% Theater ratio over time: Slight increase as CSR becomes more common.
narrative_ontology:measurement(ghost_fishing_gear_tr_t0, ghost_fishing_gear, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ghost_fishing_gear_tr_t5, ghost_fishing_gear, theater_ratio, 5, 0.10).
narrative_ontology:measurement(ghost_fishing_gear_tr_t10, ghost_fishing_gear, theater_ratio, 10, 0.15).

% Extraction over time: Significant increase with scale of industrial fishing and gear durability.
narrative_ontology:measurement(ghost_fishing_gear_ex_t0, ghost_fishing_gear, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(ghost_fishing_gear_ex_t5, ghost_fishing_gear, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(ghost_fishing_gear_ex_t10, ghost_fishing_gear, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This constraint functions by allocating the cost of waste to the commons.
narrative_ontology:coordination_type(ghost_fishing_gear, resource_allocation).

% Network relationships (structural influence edges)
% Ghost gear is a primary vector for marine plastic pollution and a consequence of
% industrial overfishing practices.
narrative_ontology:affects_constraint(ghost_fishing_gear, plastic_pollution_marine).
narrative_ontology:affects_constraint(overfishing, ghost_fishing_gear).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation from
% beneficiary/victim declarations and exit options accurately models the power
% dynamics and directionality for all key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */