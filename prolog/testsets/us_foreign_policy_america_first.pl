% ============================================================================
% CONSTRAINT STORY: us_foreign_policy_america_first
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_us_foreign_policy_america_first, []).

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
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_foreign_policy_america_first
 *   human_readable: "America First" Foreign Policy Doctrine
 *   domain: geopolitical
 *
 * SUMMARY:
 *   This constraint models the "America First" foreign policy doctrine, which
 *   prioritizes bilateral, transactional relationships over multilateral
 *   alliances and international norms. It reframes international relations
 *   from a system of mutual obligations to one of zero-sum competition,
 *   using US economic and military power to extract concessions on trade
 *   and defense spending from other nations, particularly traditional allies.
 *
 * KEY AGENTS (by structural relationship):
 *   - Traditional US Allies: Primary target (powerless/trapped) — bears the costs of tariffs, increased defense burdens, and strategic uncertainty.
 *   - Nationalist Political Base: Primary beneficiary (institutional/arbitrage) — sees the policy as reasserting national sovereignty and correcting perceived economic imbalances.
 *   - US Administration (implementer): Inter-institutional beneficiary (institutional/arbitrage) — wields the doctrine as a tool of statecraft.
 *   - Allied Governments (as institutions): Inter-institutional target (institutional/constrained) — must navigate the new policy landscape with limited ability to exit the US-centric security and economic system.
 *   - Analytical Observer: Sees the dual nature of the policy as both a domestic coordination mechanism and an international extraction system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_foreign_policy_america_first, 0.75).
domain_priors:suppression_score(us_foreign_policy_america_first, 0.80).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(us_foreign_policy_america_first, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_foreign_policy_america_first, extractiveness, 0.75).
narrative_ontology:constraint_metric(us_foreign_policy_america_first, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(us_foreign_policy_america_first, theater_ratio, 0.30).

% --- NL Profile Metrics (required for mountain constraints) ---
% Not a mountain constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(us_foreign_policy_america_first, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(us_foreign_policy_america_first). % Required for Tangled Rope. Enforced via tariffs, sanctions, treaty withdrawals.

% --- Emergence flag (required for mountain constraints) ---
% Not an emergent constraint; it is actively designed and enforced.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(us_foreign_policy_america_first, nationalist_political_base).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(us_foreign_policy_america_first, traditional_us_allies).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three) -> MET
%   Snare:        victim required -> MET

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

% PERSPECTIVE 1: THE PRIMARY TARGET (TRADITIONAL ALLIES AS POWERLESS ACTORS)
% For nations existentially dependent on the US security umbrella, the policy is
% a coercive snare. High ε (0.75) and high suppression (0.80), combined with
% victim status and trapped exit, derive d≈0.95, f(d)≈1.42.
% χ = 0.75 * 1.42 * 1.2 (global) = 1.28. This is a clear Snare.
constraint_indexing:constraint_classification(us_foreign_policy_america_first, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (NATIONALIST BASE)
% For the domestic political base, this is a Rope. It coordinates their
% political identity and promises to restore national greatness.
% Beneficiary status + arbitrage exit derives d≈0.05, f(d)≈-0.12.
% χ = 0.75 * -0.12 * 1.0 (national) = -0.09. Negative χ is a clear Rope.
constraint_indexing:constraint_classification(us_foreign_policy_america_first, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Sees both the domestic coordination function (beneficiary exists) and the
% international asymmetric extraction (victim exists), plus active enforcement.
% This meets the definition of a Tangled Rope.
% Analytical power derives d≈0.72, f(d)≈1.15.
% χ = 0.75 * 1.15 * 1.2 (global) = 1.035. Well within Tangled Rope range.
constraint_indexing:constraint_classification(us_foreign_policy_america_first, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---
% The constraint operates differently between the US administration and allied governments.

% Perspective 4A: Allied Governments (Institutional, but constrained)
% As institutional victims, they have more agency than 'powerless' actors, but
% exit is still highly constrained. The derivation chain for victim + constrained
% results in a high d. This is a coercive mechanism to extract policy changes.
% Still classifies as a Snare, demonstrating the policy's coercive nature even
% at the state-to-state level.
constraint_indexing:constraint_classification(us_foreign_policy_america_first, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Perspective 4B: US Administration (Institutional Beneficiary)
% The implementing administration experiences the policy as a pure coordination
% tool for achieving state objectives. This is identical to the primary beneficiary
% perspective, classifying as a Rope. The institutional gap is vast: Rope vs. Snare.
constraint_indexing:constraint_classification(us_foreign_policy_america_first, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_foreign_policy_america_first_tests).

test(perspectival_gap_target_vs_beneficiary) :-
    % Verify the core perspectival gap between the powerless target and institutional beneficiary.
    constraint_indexing:constraint_classification(us_foreign_policy_america_first, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(us_foreign_policy_america_first, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(inter_institutional_gap) :-
    % Verify the gap between the two institutional perspectives.
    constraint_indexing:constraint_classification(us_foreign_policy_america_first, snare, context(agent_power(institutional), _, exit_options(constrained), _)),
    constraint_indexing:constraint_classification(us_foreign_policy_america_first, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_classification_is_tangled_rope) :-
    % The analytical observer must see the full, dual nature of the constraint.
    constraint_indexing:constraint_classification(us_foreign_policy_america_first, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_gates_pass) :-
    % Verify that all three structural requirements for Tangled Rope are declared.
    narrative_ontology:constraint_beneficiary(us_foreign_policy_america_first, _),
    narrative_ontology:constraint_victim(us_foreign_policy_america_first, _),
    domain_priors:requires_active_enforcement(us_foreign_policy_america_first).

:- end_tests(us_foreign_policy_america_first_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.75): The policy is explicitly designed to extract
 *     value, either through tariffs (trade) or increased financial commitments
 *     (defense). This is its primary function in the international sphere.
 *   - Suppression Score (0.80): The policy's effectiveness relies on the US's
 *     hegemonic position, which suppresses alternatives. Allies cannot easily
 *     form alternative security blocs or decouple from the US economy, making
 *     compliance the path of least resistance.
 *   - Active Enforcement: The policy is not a passive norm; it is actively
 *     enforced through tariffs, withdrawal from treaties, and direct threats.
 *     This is a key requirement for the Tangled Rope classification.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. For the domestic nationalist base, the policy is a
 *   Rope—a tool for coordinating national will and achieving desired political
 *   outcomes with perceived positive returns. For traditional allies, it is a
 *   Snare—a coercive system that imposes high costs with no meaningful way to
 *   opt out. This gap between an internal Rope and an external Snare is the
 *   defining feature of this type of nationalist policy.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `nationalist_political_base`. The policy benefits this group
 *     by validating their worldview, promising economic protectionism, and
 *     reasserting a vision of national sovereignty. The engine correctly assigns
 *     them a very low directionality (d), leading to negative effective
 *     extraction (χ < 0) and a Rope classification.
 *   - Victim: `traditional_us_allies`. They bear the direct costs: economic
 *     disruption from tariffs and strategic instability from weakened alliances.
 *     The engine assigns them a very high directionality (d), leading to
 *     magnified effective extraction (χ > 1.0) and a Snare classification.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The model captures the asymmetric relationship between the US administration
 *   and allied governments. Both are `institutional` actors, but their `exit_options`
 *   are fundamentally different. The US has `arbitrage`—it can pick and choose
 *   which agreements to honor. Allied nations are `constrained`—they must react
 *   within a system they cannot easily leave. This difference in exit optionality,
 *   combined with their victim/beneficiary status, correctly produces the
 *   Snare vs. Rope classification even at the institutional level.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 *   The analytical classification as a Tangled Rope is crucial. It prevents
 *   two potential errors:
 *   1. Mistaking it for a pure Snare: This would ignore the genuine and powerful
 *      domestic coordination function it serves. It is not *just* extraction.
 *   2. Mistaking it for a pure Rope: This would ignore the highly coercive and
 *      extractive effects it has on external parties.
 *   The Tangled Rope classification correctly identifies that a genuine
 *   coordination function exists but is coupled with high asymmetric extraction
 *   and enforcement, which is the core structural reality of the policy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_us_foreign_policy_america_first,
    'Is the "America First" doctrine a sustainable new equilibrium (a stable, albeit coercive, system) or a transitional phase leading to the fragmentation of the US-led order?',
    'Observing long-term geopolitical and economic indicators over 10-20 years: alliance cohesion (e.g., NATO), formation of rival economic blocs, and global trade patterns.',
    'If sustainable -> The constraint remains a Tangled Rope with high extraction. If transitional -> The constraint could decay into a Piton (theatrics remain but function is lost) as its ability to extract value diminishes due to counter-coalitions.',
    confidence_without_resolution(low)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_us_foreign_policy_america_first, empirical, 'Whether the doctrine is a sustainable equilibrium or a transitional phase, resolvable by long-term observation of geopolitical indicators.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(us_foreign_policy_america_first, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. This constraint intensified over its
% initial period of implementation.
% Required because base_extractiveness (0.75) > 0.46.

% Theater ratio over time (stable, as rhetoric was high from the start):
narrative_ontology:measurement(afp_tr_t0, us_foreign_policy_america_first, theater_ratio, 0, 0.30).
narrative_ontology:measurement(afp_tr_t5, us_foreign_policy_america_first, theater_ratio, 5, 0.30).
narrative_ontology:measurement(afp_tr_t10, us_foreign_policy_america_first, theater_ratio, 10, 0.30).

% Extraction over time (policy actions like tariffs increased over time):
narrative_ontology:measurement(afp_ex_t0, us_foreign_policy_america_first, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(afp_ex_t5, us_foreign_policy_america_first, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(afp_ex_t10, us_foreign_policy_america_first, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This policy acts as a coercive enforcement mechanism for a new set of norms.
narrative_ontology:coordination_type(us_foreign_policy_america_first, enforcement_mechanism).

% Network relationships (structural influence edges)
% This policy directly opposes and aims to dismantle the norms of post-WWII
% liberal internationalism and free trade agreements.
narrative_ontology:affects_constraint(us_foreign_policy_america_first, global_trade_liberalization).
narrative_ontology:affects_constraint(us_foreign_policy_america_first, nato_collective_defense).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this story. The structural derivation chain
% (beneficiary/victim + exit_options) accurately captures the directionality
% for all key agents. The stark differences in exit options (trapped vs.
% constrained vs. arbitrage) provide sufficient differentiation for the
% engine to compute the correct d values.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */