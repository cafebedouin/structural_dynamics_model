% ============================================================================
% CONSTRAINT STORY: pla_aerial_carrier_doctrine
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-10-27
% ============================================================================

:- module(constraint_pla_aerial_carrier_doctrine, []).

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
 *   constraint_id: pla_aerial_carrier_doctrine
 *   human_readable: China's Development of an Aerial Drone Carrier Doctrine
 *   domain: technological/political
 *
 * SUMMARY:
 *   Based on reports of China developing a large, unmanned aerial vehicle
 *   (UAV) capable of launching and recovering smaller drones, this constraint
 *   models the strategic doctrine this capability enables. The system creates
 *   a new reality for regional military planning, forcing adversaries to
 *   account for persistent, wide-area drone swarm operations while serving
 *   as a powerful force multiplier for the People's Liberation Army (PLA).
 *
 * KEY AGENTS (by structural relationship):
 *   - PLA Strategic Command: Primary beneficiary (institutional/arbitrage) — gains a powerful coordination and power projection tool.
 *   - Regional Adversaries (e.g., Taiwan): Primary target (powerless/trapped or organized/constrained) — face a new coercive threat that extracts strategic freedom.
 *   - US Indo-Pacific Command: Inter-institutional peer competitor (institutional/constrained) — must adapt doctrine and technology to counter the new capability.
 *   - Defense Analyst: Analytical observer — sees the dual coordination/extraction nature of the system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pla_aerial_carrier_doctrine, 0.55).
domain_priors:suppression_score(pla_aerial_carrier_doctrine, 0.75).   % Structural property (raw, unscaled). High coercion.
domain_priors:theater_ratio(pla_aerial_carrier_doctrine, 0.15).       % Not a Piton; this is a functional system under development.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pla_aerial_carrier_doctrine, extractiveness, 0.55).
narrative_ontology:constraint_metric(pla_aerial_carrier_doctrine, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(pla_aerial_carrier_doctrine, theater_ratio, 0.15).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(pla_aerial_carrier_doctrine, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(pla_aerial_carrier_doctrine). % Required for Tangled Rope. The system is inert without active military use.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(pla_aerial_carrier_doctrine, pla_strategic_command).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(pla_aerial_carrier_doctrine, regional_adversaries).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three satisfied)
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
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Regional adversaries who cannot escape the new strategic reality.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42.
% High ε (0.55) and high suppression (0.75) amplified by high f(d) results in
% a high χ, classifying as a Snare.
constraint_indexing:constraint_classification(pla_aerial_carrier_doctrine, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The PLA, for whom this is a pure coordination tool.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12.
% The high base extractiveness is inverted, resulting in a negative χ.
constraint_indexing:constraint_classification(pla_aerial_carrier_doctrine, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% A global defense analyst sees both the coordination and extraction functions.
% The engine's derived d ≈ 0.72 → f(d) ≈ 1.15 produces a χ in the Tangled Rope range.
constraint_indexing:constraint_classification(pla_aerial_carrier_doctrine, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---
% The US military is a peer competitor, not a direct beneficiary or a trapped victim.

% Perspective 4A: US Indo-Pacific Command (Peer Competitor)
% Institutional power, but constrained exit (cannot ignore this development).
% Engine derives a moderate-to-high 'd' as a non-beneficiary with constrained options.
% The resulting χ is positive and significant, revealing the coercive aspect of the
% doctrine. It is also a Tangled Rope, but the computed χ will be lower than for
% the powerless victim but higher (and positive) than for the PLA beneficiary.
constraint_indexing:constraint_classification(pla_aerial_carrier_doctrine, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pla_aerial_carrier_doctrine_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify the core perspectival gap.
    constraint_indexing:constraint_classification(pla_aerial_carrier_doctrine, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(pla_aerial_carrier_doctrine, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    format('Perspectival gap validated: Target (Snare) vs. Beneficiary (Rope)').

test(inter_institutional_distinction) :-
    % Verify that peer competitor and beneficiary have different experiences.
    classify(pla_aerial_carrier_doctrine, context(agent_power(institutional), time_horizon(generational), exit_options(arbitrage), spatial_scope(continental)), _, ChiBeneficiary, _),
    classify(pla_aerial_carrier_doctrine, context(agent_power(institutional), time_horizon(generational), exit_options(constrained), spatial_scope(global)), _, ChiCompetitor, _),
    ChiBeneficiary < 0,
    ChiCompetitor > 0,
    format('Inter-institutional distinction validated: Beneficiary χ (~w) < 0, Competitor χ (~w) > 0', [ChiBeneficiary, ChiCompetitor]).

test(tangled_rope_gate_compliance) :-
    narrative_ontology:constraint_beneficiary(pla_aerial_carrier_doctrine, _),
    narrative_ontology:constraint_victim(pla_aerial_carrier_doctrine, _),
    domain_priors:requires_active_enforcement(pla_aerial_carrier_doctrine).

:- end_tests(pla_aerial_carrier_doctrine_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): The doctrine is inherently extractive, designed to degrade an adversary's military options and impose high strategic costs.
 *   - Suppression (S=0.75): The system's purpose is to suppress air defenses and deny access, directly limiting the target's freedom of action.
 *   - The combination of a clear coordination function (for the PLA) and a clear asymmetric extraction function (against its targets), backed by active enforcement, makes this a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the PLA Strategic Command (beneficiary), the system is a Rope—a pure coordination tool that multiplies their effectiveness with negative effective extraction (χ < 0). For a regional adversary (target), it is a Snare—a coercive trap with high effective extraction (χ > 0.66) that severely limits their options and autonomy. This disagreement is not a matter of opinion but a direct result of their differing structural relationships to the constraint.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This story highlights a key v6.0 feature. We model both the PLA and the US military as `institutional` actors, but their different `exit_options` (`arbitrage` vs. `constrained`) and structural relationships (beneficiary vs. competitor) lead the engine to derive vastly different directionality (`d`) values. The PLA experiences the system as a helpful Rope (negative χ), while the US experiences it as a coercive Tangled Rope (positive χ). This captures the essence of strategic competition, where a capability developed by one institution imposes costs and constraints on another.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this system as a simple Rope (ignoring the victims) or a simple Snare (ignoring its genuine coordination function for the PLA) would be a failure of analysis. The Tangled Rope classification, derived from the analytical perspective, correctly identifies its dual nature, preventing the mislabeling that often occurs in military-strategic analysis where a system's internal benefits are emphasized while its external costs are downplayed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_pla_carrier_effectiveness,
    'What is the true operational effectiveness and reliability of the aerial carrier system versus its theoretical potential?',
    'Intelligence reports on test deployments, system failures, and integration with existing PLA C4ISR structures.',
    'If highly effective, the ε=0.55 is accurate or low. If unreliable (a "paper tiger"), the true ε is much lower, and the constraint is closer to a Piton from an analytical view.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(pla_aerial_carrier_doctrine, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for this high-extraction (ε=0.55) constraint.
% Models a system moving from early development (T=0) to full operational
% capability (T=10), with extractiveness increasing as the doctrine matures.
% Theater ratio remains low as it's a functional, not performative, system.

% Theater ratio over time (stable and low):
narrative_ontology:measurement(pla_ac_tr_t0, pla_aerial_carrier_doctrine, theater_ratio, 0, 0.10).
narrative_ontology:measurement(pla_ac_tr_t5, pla_aerial_carrier_doctrine, theater_ratio, 5, 0.12).
narrative_ontology:measurement(pla_ac_tr_t10, pla_aerial_carrier_doctrine, theater_ratio, 10, 0.15).

% Extraction over time (increasing with doctrinal maturity):
narrative_ontology:measurement(pla_ac_ex_t0, pla_aerial_carrier_doctrine, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(pla_ac_ex_t5, pla_aerial_carrier_doctrine, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(pla_ac_ex_t10, pla_aerial_carrier_doctrine, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: It solves a complex problem of allocating drone assets
% and sensor data across a wide battlespace.
narrative_ontology:coordination_type(pla_aerial_carrier_doctrine, resource_allocation).

% Network relationships: This doctrine directly impacts the strategic
% stability and defense planning of regional actors.
narrative_ontology:affects_constraint(pla_aerial_carrier_doctrine, taiwan_defense_posture).
narrative_ontology:affects_constraint(pla_aerial_carrier_doctrine, south_china_sea_freedom_of_navigation).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this story. The structural declarations of
% beneficiary/victim combined with the different exit_options for the
% institutional actors (PLA vs. US) are sufficient for the engine to
% correctly derive the different directionality values and produce the
% nuanced classifications.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */