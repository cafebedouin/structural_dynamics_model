% ============================================================================
% CONSTRAINT STORY: nuclear_site_safety_norms
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-09-08
% ============================================================================

:- module(constraint_nuclear_site_safety_norms, []).

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
    domain_priors:emerges_naturally/1.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nuclear_site_safety_norms
 *   human_readable: International Nuclear Site Non-Proliferation and Safety Norms
 *   domain: geopolitical/technological
 *
 * SUMMARY:
 *   This constraint represents the system of international treaties, monitoring
 *   (e.g., by the IAEA), and geopolitical norms that prohibit military attacks
 *   on nuclear power infrastructure. The drone attack near Chernobyl in 2024
 *   serves as a stress test for this constraint, which functions to prevent
 *   radiological disasters and the weaponization of civilian nuclear sites.
 *   The constraint is a coordination mechanism for the international community
 *   but acts as a coercive restriction on military actors.
 *
 * KEY AGENTS (by structural relationship):
 *   - Local/Regional Populations: Primary target/victim (powerless/trapped) — bear the ultimate physical risk of the constraint's failure.
 *   - Military Aggressors: Secondary target (powerful/constrained) — constrained in their tactical options by the norm.
 *   - International Community & IAEA: Primary beneficiary (institutional/arbitrage) — benefits from global stability and the prevention of nuclear catastrophe.
 *   - Analytical Observer: Sees the full hybrid coordination/extraction structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_site_safety_norms, 0.48).
domain_priors:suppression_score(nuclear_site_safety_norms, 0.85).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(nuclear_site_safety_norms, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_site_safety_norms, extractiveness, 0.48).
narrative_ontology:constraint_metric(nuclear_site_safety_norms, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(nuclear_site_safety_norms, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(nuclear_site_safety_norms, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(nuclear_site_safety_norms). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% This is a human-constructed norm, not a natural law.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(nuclear_site_safety_norms, international_community_and_iaea).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(nuclear_site_safety_norms, local_and_regional_populations).
narrative_ontology:constraint_victim(nuclear_site_safety_norms, military_aggressors_targeting_infrastructure).

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

% PERSPECTIVE 1: LOCAL & REGIONAL POPULATIONS (SNARE)
% Agent who bears the ultimate risk of failure. They are trapped by geography
% and have no agency, making the system feel like a coercive threat.
%   victim + trapped → d≈0.95 → f(d)≈1.42
%   χ = 0.48 * 1.42 * 1.0 (national scope) = 0.68. This clears χ ≥ 0.66 for Snare.
constraint_indexing:constraint_classification(nuclear_site_safety_norms, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INTERNATIONAL COMMUNITY & IAEA (ROPE)
% Agent who benefits most from stability and preventing catastrophe.
%   beneficiary + arbitrage → d≈0.05 → f(d)≈-0.12 → negative χ
constraint_indexing:constraint_classification(nuclear_site_safety_norms, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Sees both the coordination function and the coercive extraction of tactical options.
%   analytical observer → d≈0.73 → f(d)≈1.15
%   χ = 0.48 * 1.15 * 1.2 (global scope) = 0.66. Meets Tangled Rope criteria.
constraint_indexing:constraint_classification(nuclear_site_safety_norms, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: MILITARY AGGRESSORS (TANGLED ROPE)
% The norm is a restriction they must navigate. It's coercive, but part of a
% system they are also embedded in. Exit is constrained, not trapped.
%   victim + constrained → d≈0.85 → f(d)≈1.15
%   χ = 0.48 * 1.15 * 1.0 (national scope) = 0.55. High extraction, but not a pure Snare.
constraint_indexing:constraint_classification(nuclear_site_safety_norms, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_site_safety_norms_tests).

test(perspectival_gap_target_beneficiary, [nondet]) :-
    % Verify the gap between the trapped local population (Snare) and the IAEA (Rope).
    constraint_indexing:constraint_classification(nuclear_site_safety_norms, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(nuclear_site_safety_norms, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_tangled_rope, [nondet]) :-
    % Verify the analytical observer sees the hybrid nature.
    constraint_indexing:constraint_classification(nuclear_site_safety_norms, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements) :-
    % Verify that the structural data for a Tangled Rope is present.
    narrative_ontology:constraint_beneficiary(nuclear_site_safety_norms, _),
    narrative_ontology:constraint_victim(nuclear_site_safety_norms, _),
    domain_priors:requires_active_enforcement(nuclear_site_safety_norms).

:- end_tests(nuclear_site_safety_norms_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): High. This reflects the significant restriction of military options. The "value" extracted is the sovereign freedom to target any infrastructure during conflict.
 *   - Suppression Score (s=0.85): Very high. The alternative—a world where nuclear power plants are acceptable military targets—is heavily suppressed by an overlapping web of treaties, monitoring, and the shared fear of catastrophic consequences.
 *   - Theater Ratio (t=0.15): Low. The system is functional, involving real inspections, intelligence sharing, and diplomatic consequences for violations. It is not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. For the IAEA and the global community (beneficiaries), it's a pure Rope that solves a critical collective action problem (preventing nuclear disaster). For the local Ukrainian population (victims), it's a Snare; they are trapped under a system whose failure means their annihilation, with no agency to influence the outcome. For the military aggressor, it's a Tangled Rope—a coercive but understandable rule of engagement within the larger international system. The analytical view converges on Tangled Rope, acknowledging both the valid coordination function and the coercive, asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural relationships. The `international_community_and_iaea` are beneficiaries; their goal is to uphold the constraint, giving them a low `d` value and negative effective extraction (χ). The `local_and_regional_populations` and `military_aggressors` are victims. The locals are `trapped`, deriving the highest `d` (~0.95), pushing χ into Snare territory. The aggressors are `constrained` but not trapped, deriving a high but lower `d` (~0.85), keeping the classification as Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the hybrid nature of the norm. A naive analysis might call it a pure Rope ("everyone wants nuclear safety") or a pure Snare ("geopolitical imposition on sovereign nations"). The Tangled Rope classification captures the truth: it's a genuine coordination mechanism *that is maintained through coercive force and asymmetric risk*. This prevents mislabeling a vital but coercive safety system as either purely benevolent or purely extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_nuclear_site_safety_norms,
    'Was the drone attack a deliberate test of international response protocols, an operational accident, or a false flag operation?',
    'Declassified intelligence assessments from multiple non-belligerent nations regarding the drone''s origin, flight path, and payload.',
    'If a deliberate test, it indicates the constraint is being actively probed for weakness, increasing its perceived extractiveness. If an accident, it reinforces the need for the coordination function.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(nuclear_site_safety_norms, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint intensified over time, particularly after major nuclear accidents
% and the end of the Cold War. Base extractiveness (restrictions) increased while
% theater decreased as monitoring became more sophisticated.
% (Base extractiveness > 0.46, so this section is required).

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(nssn_tr_t0, nuclear_site_safety_norms, theater_ratio, 0, 0.35).
narrative_ontology:measurement(nssn_tr_t5, nuclear_site_safety_norms, theater_ratio, 5, 0.20).
narrative_ontology:measurement(nssn_tr_t10, nuclear_site_safety_norms, theater_ratio, 10, 0.15).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(nssn_ex_t0, nuclear_site_safety_norms, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(nssn_ex_t5, nuclear_site_safety_norms, base_extractiveness, 5, 0.40).
narrative_ontology:measurement(nssn_ex_t10, nuclear_site_safety_norms, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(nuclear_site_safety_norms, enforcement_mechanism).

% Network relationships (structural influence edges)
% This norm is structurally coupled with the broader non-proliferation and
% international sanctions frameworks.
narrative_ontology:affects_constraint(nuclear_site_safety_norms, npt_regime).
narrative_ontology:affects_constraint(nuclear_site_safety_norms, un_sanctions_framework).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The automatic derivation
% from beneficiary/victim declarations and exit options accurately captures
% the structural relationships of the agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */