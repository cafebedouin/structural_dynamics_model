% ============================================================================
% CONSTRAINT STORY: shadow_fleet_sanctions_evasion
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-26
% ============================================================================

:- module(constraint_shadow_fleet_sanctions_evasion, []).

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
 *   constraint_id: shadow_fleet_sanctions_evasion
 *   human_readable: Sanctions Evasion via Shadow Fleet
 *   domain: geopolitical
 *
 * SUMMARY:
 *   The "shadow fleet" refers to a network of oil tankers used to circumvent international sanctions imposed on countries like Iran and Venezuela. These tankers often operate under obscured ownership, falsified documentation, and engage in ship-to-ship transfers to conceal the origin of the oil, allowing sanctioned nations to continue exporting their products despite international restrictions. This constraint story focuses on the dynamic between the sanctioning body, the sanctioned nation, and the shadow fleet operators who facilitate the evasion.
 *
 * KEY AGENTS (by structural relationship):
 *   - Shadow Fleet Crews: Primary target (powerless/trapped) — bear direct operational risks and exploitation.
 *   - Sanctioned Nations (Iran, Venezuela): Secondary target (moderate/constrained) — bears extraction but also enables evasion.
 *   - Shadow Fleet Operators: Primary beneficiary (powerful/arbitrage) — benefits from circumventing sanctions.
 *   - Sanctioning Nations (US, EU): Secondary actor (institutional/mobile) — Imposes and attempts to enforce sanctions.
 *   - Analytical Observer: Analytical observer — sees full structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shadow_fleet_sanctions_evasion, 0.55).
domain_priors:suppression_score(shadow_fleet_sanctions_evasion, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(shadow_fleet_sanctions_evasion, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shadow_fleet_sanctions_evasion, extractiveness, 0.55).
narrative_ontology:constraint_metric(shadow_fleet_sanctions_evasion, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(shadow_fleet_sanctions_evasion, theater_ratio, 0.30).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Uncomment and set for mountain constraints.
% Without these, the NL signature defaults to 0.5 and fails certification.
%
% narrative_ontology:constraint_metric(shadow_fleet_sanctions_evasion, accessibility_collapse, [0.85-1.0]).
% narrative_ontology:constraint_metric(shadow_fleet_sanctions_evasion, resistance, [0.0-0.15]).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(shadow_fleet_sanctions_evasion, tangled_rope).
narrative_ontology:human_readable(shadow_fleet_sanctions_evasion, "Sanctions Evasion via Shadow Fleet").

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(shadow_fleet_sanctions_evasion).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(shadow_fleet_sanctions_evasion). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% Uncomment for constraints that emerge naturally without human design
% or enforcement. Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.
%
% domain_priors:emerges_naturally(shadow_fleet_sanctions_evasion).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(shadow_fleet_sanctions_evasion, shadow_fleet_operators).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(shadow_fleet_sanctions_evasion, sanctioned_nations).
narrative_ontology:constraint_victim(shadow_fleet_sanctions_evasion, shadow_fleet_crews).
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

% PERSPECTIVE 1: THE DIRECTLY EXPLOITED (SNARE)
% Crew members on shadow fleet vessels, who are often underpaid, uninsured,
% and operate in dangerous conditions with little legal recourse.
constraint_indexing:constraint_classification(shadow_fleet_sanctions_evasion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE SANCTIONED NATION (SNARE)
% The sanctioned nations themselves, who are constrained by sanctions but use
% the shadow fleet as a costly and risky lifeline.
constraint_indexing:constraint_classification(shadow_fleet_sanctions_evasion, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: THE PRIMARY BENEFICIARY (ROPE)
% The shadow fleet operators who profit from the arbitrage opportunity created
% by the sanctions regime.
constraint_indexing:constraint_classification(shadow_fleet_sanctions_evasion, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
constraint_indexing:constraint_classification(shadow_fleet_sanctions_evasion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---
% The sanctioning nations (US, EU) attempting to enforce the sanctions. They have
% institutional power but face difficulties in enforcing global sanctions due to
% state sovereignty and the fleet's opacity.
constraint_indexing:constraint_classification(shadow_fleet_sanctions_evasion, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),  % Mobile because they can adjust sanctions strategy
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shadow_fleet_sanctions_evasion_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(shadow_fleet_sanctions_evasion, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(shadow_fleet_sanctions_evasion, TypeBeneficiary, context(agent_power(powerful), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(shadow_fleet_sanctions_evasion, ExtMetricName, E),
    (E =< 0.25 -> true ; E >= 0.46). % Mountain or high-extraction Snare/Tangled.

:- end_tests(shadow_fleet_sanctions_evasion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.55) as the shadow fleet system extracts value from multiple parties: from the sanctioning bodies by undermining policy, from sanctioned nations via premium pricing, and from its own crews via exploitative labor practices. Suppression is high (0.70) due to the active, complex, and illegal measures required to conceal transactions, ownership, and vessel movements. The theater ratio is low (0.30) as the activity is almost entirely functional (evading sanctions for profit) rather than performative.
 *
 * PERSPECTIVAL GAP:
 *   The gap is multi-layered. For the powerless crews, the system is a Snare of labor exploitation. For the sanctioned nations, it's also a Snare, but one they are forced to engage with as a lifeline. For the shadow fleet operators, it is a Rope, a highly profitable coordination mechanism for moving sanctioned goods. For the sanctioning bodies, it is also a Rope, but one that works against their interests, representing a coordination problem they must solve.
 *
 * DIRECTIONALITY LOGIC:
 *   The primary beneficiaries are the `shadow_fleet_operators` who profit directly from the arbitrage. The victims are twofold: the `sanctioned_nations` who pay a premium and bear geopolitical risk, and the `shadow_fleet_crews` who are trapped in precarious, high-risk employment. This dual-victim structure captures the different layers of extraction.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The sanctioning nations (e.g., US, EU) are institutional actors attempting to enforce a global constraint (sanctions). The shadow fleet represents a counter-constraint. The sanctioners' perspective is a Rope because they are trying to coordinate international action against this evasion; their `mobile` exit option reflects their ability to change tactics (e.g., new sanctions, diplomatic pressure).
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification from the analytical view correctly captures the dual nature of the system. It is not pure extraction (a Snare), because it provides a genuine coordination service (moving oil for sanctioned states). However, it is not pure coordination (a Rope), because this service is built on opacity, illegality, and the exploitation of both crews and geopolitical loopholes. The classification correctly identifies it as a hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_shadow_fleet,
    'What is the true extent of the shadow fleet''s capacity and its impact on the global oil market?',
    'Improved data collection and analysis of ship movements, ownership structures, and financial transactions.',
    'If the capacity is high, the sanctions are largely ineffective. If it is low, the sanctions have a more significant impact.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(shadow_fleet_sanctions_evasion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
%
% Required for high-extraction constraints (base_extractiveness > 0.46).
% Use at least 3 time points (T=0, midpoint, T=end) for each tracked metric.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(shadow_fleet_sanctions_evasion_tr_t0, shadow_fleet_sanctions_evasion, theater_ratio, 0, 0.20).
narrative_ontology:measurement(shadow_fleet_sanctions_evasion_tr_t5, shadow_fleet_sanctions_evasion, theater_ratio, 5, 0.30).
narrative_ontology:measurement(shadow_fleet_sanctions_evasion_tr_t10, shadow_fleet_sanctions_evasion, theater_ratio, 10, 0.30).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(shadow_fleet_sanctions_evasion_ex_t0, shadow_fleet_sanctions_evasion, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(shadow_fleet_sanctions_evasion_ex_t5, shadow_fleet_sanctions_evasion, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(shadow_fleet_sanctions_evasion_ex_t10, shadow_fleet_sanctions_evasion, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(shadow_fleet_sanctions_evasion, resource_allocation).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(shadow_fleet_sanctions_evasion, [0.0-1.0]).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(shadow_fleet_sanctions_evasion, [other_constraint_id]).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Use ONLY when the automatic derivation (beneficiary/victim + exit → d)
% would produce an inaccurate directionality value. The derivation chain
% priority is: override > structural > canonical fallback.
%
% Format: directionality_override(ConstraintID, PowerAtom, D_Value)
%   D_Value in [0.0, 1.0]: 0.0 = full beneficiary, 1.0 = full target
%
% Common override scenarios:
%   - Regulatory capture: institution that appears to benefit but is
%     actually partly captured → override d upward (0.25-0.40)
%   - Indirect beneficiary: agent in victim group who actually benefits
%     through secondary effects → override d downward
%   - Asymmetric institutional: two institutional actors that the
%     derivation can't distinguish → override to differentiate
%
% Example (uncomment if needed):
% constraint_indexing:directionality_override(shadow_fleet_sanctions_evasion, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */