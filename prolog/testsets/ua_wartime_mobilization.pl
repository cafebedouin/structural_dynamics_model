% ============================================================================
% CONSTRAINT STORY: ua_wartime_mobilization
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-20
% ============================================================================

:- module(constraint_ua_wartime_mobilization, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ua_wartime_mobilization
 *   human_readable: "Wartime Mobilization Law in Ukraine"
 *   domain: political/military
 *
 * SUMMARY:
 *   In response to the full-scale Russian invasion, the Ukrainian state enacted
 *   and enforces a general mobilization law. This law compels Ukrainian men
 *   of fighting age to serve in the armed forces, severely restricting their
 *   freedom of movement and placing them in life-threatening situations. The
 *   constraint is presented as a necessity for national survival.
 *
 * KEY AGENTS (by structural relationship):
 *   - Mobilized Ukrainian Men: Primary target (powerless/trapped) — bears the full extraction of life, health, and liberty.
 *   - The Ukrainian State: Primary beneficiary (institutional/arbitrage) — benefits by ensuring its own survival and ability to wage a defensive war.
 *   - Ukrainian Society: Secondary beneficiary (organized/constrained) — benefits from national preservation but bears economic and demographic costs.
 *   - Analytical Observer: External policy analyst — sees the hybrid nature of necessary coordination and coercive extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ua_wartime_mobilization, 0.85). % Extracts life, liberty, and years of productive capacity.
domain_priors:suppression_score(ua_wartime_mobilization, 0.90).   % Structural property (raw, unscaled). Alternatives (evasion, flight) are illegal and heavily policed.
domain_priors:theater_ratio(ua_wartime_mobilization, 0.10).       % Piton detection (>= 0.70). This is highly functional, not theatrical.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ua_wartime_mobilization, extractiveness, 0.85).
narrative_ontology:constraint_metric(ua_wartime_mobilization, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(ua_wartime_mobilization, theater_ratio, 0.10).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(ua_wartime_mobilization, tangled_rope).
narrative_ontology:human_readable(ua_wartime_mobilization, "Wartime Mobilization Law in Ukraine").
narrative_ontology:topic_domain(ua_wartime_mobilization, "political/military").

% --- Binary flags ---
domain_priors:requires_active_enforcement(ua_wartime_mobilization). % Enforced by military police, border guards, and legal penalties.

% --- Emergence flag (required for mountain constraints) ---
% N/A. This is a human-designed state policy.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(ua_wartime_mobilization, ukrainian_state).
narrative_ontology:constraint_beneficiary(ua_wartime_mobilization, ukrainian_society).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(ua_wartime_mobilization, mobilized_ukrainian_men).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three met)
%   Snare:        victim required (met)

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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% A mobilized Ukrainian man. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% Calculation: χ = 0.85 * 1.42 * 1.0 = 1.207. Meets snare criteria (χ ≥ 0.66).
constraint_indexing:constraint_classification(ua_wartime_mobilization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The Ukrainian state. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
% Calculation: χ = 0.85 * -0.12 * 1.0 = -0.102. Appears as a pure coordination tool.
constraint_indexing:constraint_classification(ua_wartime_mobilization, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context (civilizational/analytical/global).
% Engine derives d ≈ 0.73 → f(d) ≈ 1.15 for analytical perspective.
% Calculation: χ = 0.85 * 1.15 * 1.2 (global scope) = 1.173.
% This χ is very high, but the constraint has a genuine coordination function
% (national survival), asymmetric extraction, and requires enforcement,
% meeting the structural definition of a Tangled Rope.
constraint_indexing:constraint_classification(ua_wartime_mobilization, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: UKRAINIAN SOCIETY (TANGLED ROPE)
% Citizens not directly mobilized but affected by the law. They are both
% beneficiaries (nation is saved) and victims (economic/demographic cost).
% The 'both' status + constrained exit yields a moderate directionality.
% Engine derives d ≈ 0.50 -> f(d) ≈ 0.65.
% Calculation: χ = 0.85 * 0.65 * 1.0 = 0.5525. Meets Tangled Rope criteria.
constraint_indexing:constraint_classification(ua_wartime_mobilization, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ua_wartime_mobilization_tests).

test(perspectival_gap_snare_vs_rope) :-
    % Verify the core perspectival gap between the powerless target and institutional beneficiary.
    constraint_indexing:constraint_classification(ua_wartime_mobilization, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(ua_wartime_mobilization, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_classification_is_tangled_rope) :-
    % The analytical observer must see the hybrid nature.
    constraint_indexing:constraint_classification(ua_wartime_mobilization, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_compliance) :-
    % Verify that all three structural requirements for a Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(ua_wartime_mobilization, _),
    narrative_ontology:constraint_victim(ua_wartime_mobilization, _),
    domain_priors:requires_active_enforcement(ua_wartime_mobilization).

:- end_tests(ua_wartime_mobilization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.85): The constraint demands the ultimate sacrifice: life, health, and years of freedom. This is among the highest possible levels of extraction.
 *   - Suppression Score (0.90): Alternatives are systematically eliminated. Men of fighting age are barred from leaving the country, and evasion is a criminal offense. The existential threat itself suppresses dissent.
 *   - Theater Ratio (0.10): The mobilization is brutally functional, aimed directly at military manpower requirements. It is not performative.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the individual conscript (powerless, trapped), the constraint is a Snare that consumes their life for a goal they may or may not share. Exit is impossible, and the cost is absolute.
 *   For the Ukrainian state (institutional, arbitrage), it is a Rope—a necessary, if tragic, tool of coordination to organize national defense and ensure its own survival. The state experiences this as a negative-extraction act, enabling its continued existence.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `ukrainian_state`. Its survival is the direct goal.
 *   - Victim: `mobilized_ukrainian_men`. They bear the direct, physical cost of the policy.
 *   This clear structural relationship drives the directionality calculation. For victims with trapped exit, `d` approaches 1.0, maximizing effective extraction (χ). For beneficiaries with arbitrage exit, `d` approaches 0.0, minimizing or negating χ. This mathematical divergence reflects the real-world political and ethical chasm.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 *   This classification correctly identifies the hybrid nature of the constraint, preventing a simplistic mislabeling. An analysis that sees only the national survival goal would call it a pure Rope, ignoring the immense coercive cost. An analysis that sees only the individual's loss of freedom would call it a pure Snare, ignoring the genuine existential threat that makes the coordination necessary. The Tangled Rope classification, from the analytical perspective, holds both truths in tension: it is a coordination mechanism for survival *and* a brutal extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_ua_wartime_mobilization,
    'Is the level of coercion and individual extraction proportionate to the existential threat, or has it become an inertial policy with diminishing returns for national survival?',
    'Post-war analysis of demographic impact, military effectiveness vs. manpower levels, and the state of civil liberties in Ukraine.',
    'If proportionate -> Justified Tangled Rope. If disproportionate -> A system that has degraded into a state-level Snare, consuming its population beyond strategic necessity.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(ua_wartime_mobilization, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint has intensified over the course of the war.
% Initial phase (T=0) had higher volunteerism. Mid-phase (T=5) saw
% attrition set in. Current phase (T=10) reflects maximum coercion.

% Theater ratio over time (stable and low):
narrative_ontology:measurement(ua_wartime_mobilization_tr_t0, ua_wartime_mobilization, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ua_wartime_mobilization_tr_t5, ua_wartime_mobilization, theater_ratio, 5, 0.10).
narrative_ontology:measurement(ua_wartime_mobilization_tr_t10, ua_wartime_mobilization, theater_ratio, 10, 0.10).

% Extraction over time (steadily increasing):
narrative_ontology:measurement(ua_wartime_mobilization_ex_t0, ua_wartime_mobilization, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(ua_wartime_mobilization_ex_t5, ua_wartime_mobilization, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(ua_wartime_mobilization_ex_t10, ua_wartime_mobilization, base_extractiveness, 10, 0.85).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This is a mechanism for allocating the ultimate resource: human capital.
narrative_ontology:coordination_type(ua_wartime_mobilization, resource_allocation).

% Network relationships (structural influence edges)
% Mobilization directly impacts border control policies and the national economy.
narrative_ontology:affects_constraint(ua_wartime_mobilization, ua_border_control_policy).
narrative_ontology:affects_constraint(ua_wartime_mobilization, ua_economic_output).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% status and exit options accurately captures the dynamics of this constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */