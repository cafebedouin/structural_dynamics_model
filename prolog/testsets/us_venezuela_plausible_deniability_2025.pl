% ============================================================================
% CONSTRAINT STORY: us_venezuela_plausible_deniability_2025
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_us_venezuela_plausible_deniability_2025, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_venezuela_plausible_deniability_2025
 *   human_readable: Plausible Deniability of US Covert Action in Venezuela
 *   domain: geopolitical
 *
 * SUMMARY:
 *   Following a news report alleging a CIA-led ground attack inside
 *   Venezuela, the US government maintains a policy of "plausible deniability."
 *   This policy is a constraint on information, accountability, and diplomatic
 *   action. It enables covert strategic goals to be pursued while suppressing
 *   public debate, congressional oversight, and international legal recourse.
 *   The constraint is the official narrative shield, not the military action
 *   itself, though the two are inextricably linked.
 *
 * KEY AGENTS (by structural relationship):
 *   - Venezuelan Government: Primary target (powerless/trapped) — bears the extraction of sovereignty and is unable to force accountability.
 *   - US Intelligence Apparatus (CIA): Primary beneficiary (institutional/arbitrage) — achieves strategic objectives without the political cost of overt warfare.
 *   - US Public & Congress: Secondary victim (moderate/constrained) — their oversight powers are circumvented by the policy of secrecy.
 *   - Analytical Observer: External analyst (analytical/analytical) — sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_venezuela_plausible_deniability_2025, 0.72).
domain_priors:suppression_score(us_venezuela_plausible_deniability_2025, 0.85).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(us_venezuela_plausible_deniability_2025, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_venezuela_plausible_deniability_2025, extractiveness, 0.72).
narrative_ontology:constraint_metric(us_venezuela_plausible_deniability_2025, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(us_venezuela_plausible_deniability_2025, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(us_venezuela_plausible_deniability_2025, tangled_rope).
narrative_ontology:human_readable(us_venezuela_plausible_deniability_2025, "Plausible Deniability of US Covert Action in Venezuela").

% --- Binary flags ---
domain_priors:requires_active_enforcement(us_venezuela_plausible_deniability_2025). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(us_venezuela_plausible_deniability_2025, us_intelligence_apparatus).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(us_venezuela_plausible_deniability_2025, venezuelan_sovereignty).
narrative_ontology:constraint_victim(us_venezuela_plausible_deniability_2025, us_public_oversight).

% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three are present)

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

% PERSPECTIVE 1: THE PRIMARY TARGET (VENEZUELAN GOVERNMENT)
% Experiences an unaccountable violation of sovereignty. The "deniability"
% constraint is the mechanism that prevents any form of recourse, making it
% a pure instrument of extraction.
% Engine derives d from victim status + trapped exit → d ≈ 0.95 → high χ.
constraint_indexing:constraint_classification(us_venezuela_plausible_deniability_2025, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (US INTELLIGENCE APPARATUS)
% The policy of plausible deniability is a coordinating mechanism that
% allows different agencies to act in concert covertly, shielding them from
% domestic political costs and international legal consequences. It functions
% as a pure tool, a Rope that enables their objectives.
% Engine derives d from beneficiary status + arbitrage exit → d ≈ 0.05 → negative χ.
constraint_indexing:constraint_classification(us_venezuela_plausible_deniability_2025, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Sees both the internal coordination function (for the US agencies) and the
% severe, asymmetric extraction imposed on the target. The combination of a
% genuine coordination function with high extraction and active enforcement
% is the definition of a Tangled Rope.
% Engine derives d ≈ 0.72, and σ(global)=1.2 amplifies χ.
constraint_indexing:constraint_classification(us_venezuela_plausible_deniability_2025, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: INTER-INSTITUTIONAL VICTIM (US PUBLIC & CONGRESS)
% The US public's right to democratic oversight is a secondary victim of this
% constraint. They are not the military target, but the policy extracts their
% power to hold their government accountable. Their exit is constrained by
% classified information. From this view, it is also a highly extractive constraint.
% Engine derives d from victim status + constrained exit -> high d -> high chi.
constraint_indexing:constraint_classification(us_venezuela_plausible_deniability_2025, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_venezuela_plausible_deniability_2025_tests).

test(perspectival_gap_is_snare_vs_rope) :-
    constraint_indexing:constraint_classification(us_venezuela_plausible_deniability_2025, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(us_venezuela_plausible_deniability_2025, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(us_venezuela_plausible_deniability_2025, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_gates_pass) :-
    narrative_ontology:constraint_beneficiary(us_venezuela_plausible_deniability_2025, _),
    narrative_ontology:constraint_victim(us_venezuela_plausible_deniability_2025, _),
    domain_priors:requires_active_enforcement(us_venezuela_plausible_deniability_2025).

:- end_tests(us_venezuela_plausible_deniability_2025_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.72): High. The constraint directly enables the extraction of national sovereignty and circumvents democratic accountability, a massive transfer of agency.
 *   - Suppression Score (0.85): High. The entire purpose of "plausible deniability" is to suppress the true narrative and foreclose legal/political recourse through active information control.
 *   - Theater Ratio (0.20): Low. While the public statements are theater, they shield a highly functional and consequential covert operation. The constraint's primary role is functional, not performative.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme, spanning from Rope to Snare. For the US intelligence apparatus (beneficiary), the constraint is a pure coordination tool (Rope) that enables effective action while minimizing political friction. For the Venezuelan government (target), this same constraint is a pure instrument of unaccountable power (Snare), stripping them of agency and recourse. This demonstrates the core principle of Deferential Realism: the classification of a constraint is a function of one's structural position relative to it.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `us_intelligence_apparatus`. They gain the ability to conduct foreign policy by force without the costs associated with declared warfare (public debate, congressional approval, international treaties).
 *   - Victims: `venezuelan_sovereignty` and `us_public_oversight`. The former is the direct target, losing its fundamental right to self-governance without attack. The latter is a secondary victim, as the policy deliberately circumvents the mechanisms of democratic control over the military and intelligence services.
 *   This structure feeds the directionality engine, producing low `d` for the beneficiary (institutional power, arbitrage exit) and high `d` for the victims (powerless/trapped, moderate/constrained).
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 *   This case is a powerful defense against Mandatrophy. A naive analysis might label the constraint a Snare from all perspectives because it facilitates immense harm. However, this misses its crucial *coordination function*. Plausible deniability is the "Rope" that binds the actions of the State Department, CIA, and military into a coherent, deniable covert strategy. Without this coordinating constraint, such an operation would be impossible to execute secretly. The analytical classification of Tangled Rope correctly captures this duality: it is a system that possesses both a genuine coordination function *and* a deeply extractive, asymmetric outcome. It is not merely a lie; it is a functional piece of institutional machinery with a purpose and a victim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_us_venezuela_plausible_deniability_2025,
    'Is the covert action a limited counter-op or a component of a wider regime change strategy?',
    'Declassified documents from US archives, whistleblower testimony from involved agents, or subsequent escalations revealing a broader strategic plan.',
    'If limited op -> Tangled Rope is stable. If regime change -> the coordination function is mere pretext for a pure Snare.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(us_venezuela_plausible_deniability_2025, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. Since base_extractiveness > 0.46,
% this is required. We model the policy escalating from political pressure
% to direct covert action over the interval.
% Theater ratio remains low as the actions are always functional.
narrative_ontology:measurement(uvpd25_tr_t0, us_venezuela_plausible_deniability_2025, theater_ratio, 0, 0.20).
narrative_ontology:measurement(uvpd25_tr_t5, us_venezuela_plausible_deniability_2025, theater_ratio, 5, 0.20).
narrative_ontology:measurement(uvpd25_tr_t10, us_venezuela_plausible_deniability_2025, theater_ratio, 10, 0.20).

% Extraction accumulates as policy shifts from sanctions to covert operations.
narrative_ontology:measurement(uvpd25_ex_t0, us_venezuela_plausible_deniability_2025, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(uvpd25_ex_t5, us_venezuela_plausible_deniability_2025, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(uvpd25_ex_t10, us_venezuela_plausible_deniability_2025, base_extractiveness, 10, 0.72).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The constraint coordinates state actors to enforce a
% specific foreign policy and narrative, shielding them from accountability.
narrative_ontology:coordination_type(us_venezuela_plausible_deniability_2025, enforcement_mechanism).

% Network relationships: This policy is structurally linked to, and reinforces,
% the broader sanctions regime against the target country.
narrative_ontology:affects_constraint(us_venezuela_plausible_deniability_2025, international_sanctions_regime_venezuela).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The standard derivation
% chain based on beneficiary/victim declarations and exit options accurately
% models the structural power dynamics of the scenario. The beneficiary has
% institutional power and arbitrage exit, while the victims are trapped or
% constrained, leading to correct d-value derivations.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */