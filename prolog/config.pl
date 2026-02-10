:- module(config, [
    param/2,
    aggregation_weights/5,
    influence_weight/2,
    level/1
]).

:- multifile param/2.
:- dynamic param/2.

/* ================================================================
   1. HIERARCHY DEFINITIONS
   ================================================================ */

%% level(?Level)
% Defines the four analytical levels of the system.
level(structural).
level(organizational).
level(class).
level(individual).

% --- General Metric Naming (for dynamic dispatch) ---
param(suppression_metric_name, suppression_requirement).
param(extractiveness_metric_name, extractiveness).
param(temporal_metric_name, time_horizon).
param(exit_metric_name, exit_options).
param(power_metric_name, agent_power).
param(scope_metric_name, spatial_scope).
param(theater_metric_name, theater_ratio).

/* ================================================================
   2. COMPONENT WEIGHTS (Alpha)
   Maps components (A, S, U, R) to magnitude (Kappa) per level.
   Formula: $\kappa = (\alpha_A \cdot A) + (\alpha_S \cdot S) + (\alpha_U \cdot U) + (\alpha_R \cdot R)$.
   ================================================================ */

%% aggregation_weights(+Level, -AlphaA, -AlphaS, -AlphaU, -AlphaR)
% Weights are shifted based on level-specific significance.
aggregation_weights(structural,     0.30, 0.20, 0.20, 0.20).
aggregation_weights(organizational, 0.30, 0.40, 0.15, 0.15).
aggregation_weights(class,          0.30, 0.20, 0.30, 0.30).
aggregation_weights(individual,     0.25, 0.25, 0.25, 0.25).

/* ================================================================
   3. INFLUENCE WEIGHTS (w_i)
   Determines how much each level contributes to the System Gradient (Gsys).
   ================================================================ */

%% influence_weight(+Level, -Weight)
influence_weight(structural,     0.40). % Highest impact on system stability.
influence_weight(organizational, 0.30).
influence_weight(class,          0.20).
influence_weight(individual,     0.10).

/* ================================================================
   4. POWER MODIFIERS (pi)
   Determines how much of the base extraction is "felt" by the agent.
   ================================================================ */

% Aligned with core.md spec (Section: Power-Scaling)
param(power_modifier_powerless, 1.5).      % Extraction amplified
param(power_modifier_moderate, 1.0).       % Baseline
param(power_modifier_powerful, 0.6).       % Extraction reduced
param(power_modifier_organized, 0.4).      % Shared burden (collective)
param(power_modifier_institutional, -0.2). % Net beneficiary
param(power_modifier_analytical, 1.15).    % Analytical clarity: detects extraction moderate agents normalize

/* ================================================================
   4A. COALITION MODELING (The "Who" Extension)
   ================================================================ */

% If this many victims share a snare, they get the 'organized' power modifier.
param(critical_mass_threshold, 3).

/* ================================================================
   4B. SCOPE MODIFIERS (sigma)
   Determines how scope affects verification difficulty and thus
   effective extraction. Larger scope = harder verification = more
   extraction hidden behind complexity.
   Formula: χ = ε × π(P) × σ(S)
   ================================================================ */

param(scope_modifier_local,        0.8).   % Easy verification, extraction dampened
param(scope_modifier_regional,     0.9).   % Easier verification
param(scope_modifier_national,     1.0).   % Baseline
param(scope_modifier_continental,  1.1).   % Harder verification
param(scope_modifier_global,       1.2).   % Hardest verification, extraction amplified
param(scope_modifier_universal,    1.0).   % Neutral (natural laws)

/* ================================================================
   4C. SIGMOID DIRECTIONALITY PARAMETERS (v5.0)
   Replaces discrete power_modifier dispatch with continuous sigmoid f(d).
   f(d) = L + (U - L) / (1 + e^(-k*(d - d0)))
   where d is directionality in [0.0, 1.0].
   ================================================================ */

param(sigmoid_lower,    -0.20).    % L: lower asymptote
param(sigmoid_upper,     1.50).    % U: upper asymptote
param(sigmoid_midpoint,  0.50).    % d0: inflection point
param(sigmoid_steepness, 6.00).    % k: steepness

% Canonical d positions — calibrated so f(d) ≈ current pi values
% Mid-range atoms match exactly; extremes have small residuals
% (institutional: f(0.0)=-0.12 vs -0.20, powerless: f(1.0)=1.42 vs 1.50)
% because sigmoid asymptotes are unreachable. These residuals are at
% tails where chi is well beyond gate boundaries, so no classification shifts.
param(canonical_d_powerless,     1.00).
param(canonical_d_moderate,      0.6459).
param(canonical_d_powerful,      0.4804).
param(canonical_d_organized,     0.3990).
param(canonical_d_institutional, 0.00).
param(canonical_d_analytical,    0.7250).

/* ================================================================
   5. INTENT & DETECTION THRESHOLDS
   Consolidated parameters for the intent_engine and pattern_analysis.
   ================================================================ */

% --- Gradient Thresholds ---
% Minimum change to be considered non-stable.
param(system_gradient_threshold, 0.01).
% Threshold for "Strong" intent classification.
param(system_gradient_strong_threshold, 1.00).

% --- Beneficiary Asymmetry ---
% Min power gain required to be identified as the "Main Beneficiary".
param(beneficiary_gain_min, 0.50).
% Max gain allowed for other classes to maintain "Asymmetry".
param(loser_loss_max_gain, 0.10).

% --- Suppression & Resistance Alignment ---
% Required thresholds for structural-level alignment.
param(structural_suppression_min, 0.70). 
param(structural_resistance_min, 0.70).

% --- Data Integrity ---
% Thresholds for high/medium confidence classification.
param(data_high_threshold, 0.95).
param(data_medium_threshold, 0.75).

/* ================================================================
   5. DEFERENTIAL REALISM (DR) THRESHOLDS
   ================================================================ */

% --- Mountain Boundaries ---
param(mountain_suppression_ceiling, 0.05). % noise floor
param(mountain_extractiveness_min, 0.0).
param(mountain_extractiveness_max, 0.25).

% --- Rope Boundaries ---
% Aligned with core.md: Rope = low extraction, ε ≤ 0.15
param(rope_extractiveness_min, 0.0).
% Base suppression ceiling for pure coordination
param(rope_suppression_ceiling,      0.16).

% --- Tangled Rope (Hybrid Zone) ---
% Note: Tangled Rope requires BOTH coordination AND extraction markers.
param(tangled_rope_suppression_floor, 0.40). % Requires active enforcement
param(tangled_rope_suppression_ceil, 1.00).

% --- Snare Boundaries ---
param(snare_extraction_ceil,         1.00).
param(snare_suppression_floor,       0.60).
param(snare_load_bearing_threshold,  0.70).  % Above this = load-bearing snare (Theorem 3)

/* ================================================================
   5B. DUAL-THRESHOLD CLASSIFICATION (logic.md Alignment)
   ================================================================
   logic.md specifies BOTH:
     χ (chi) = power-scaled extraction (varies by agent)
     ε (epsilon) = base extraction (structural property)

   Primary classifier (drl_core.pl) checks BOTH thresholds.
   Audit module (drl_audit_core.pl) uses χ thresholds only.
   ================================================================ */

% Rule R (Rope): χ ≤ 0.35 ∧ ε ≤ 0.15 ∧ Coord(C) ∧ Changeable(C, I.T, I.E)
param(rope_chi_ceiling, 0.35).
param(rope_epsilon_ceiling, 0.45).

% Rule N (Snare): χ ≥ 0.66 ∧ ε ≥ 0.46 ∧ ¬Coord(C) ∧ Changeable(C, I.T, I.E)
param(snare_chi_floor, 0.66).
param(snare_epsilon_floor, 0.46).

% Rule TR (Tangled Rope): 0.40 ≤ χ ≤ 0.90 ∧ ε ≥ 0.50 ∧ Coord(C) ∧ Asymmetric(C)
param(tangled_rope_chi_floor, 0.40).
param(tangled_rope_chi_ceil, 0.90).
param(tangled_rope_epsilon_floor, 0.30).

% --- Scaffold Boundaries ---
% Temporary supports must remain below this coordination ceiling
param(scaffold_extraction_ceil, 0.30).

% --- Piton Boundaries ---
param(piton_extraction_ceiling,     0.25).
param(piton_theater_floor,          0.70).
param(piton_epsilon_floor,          0.10).   % Rule Z: ε(C) > 0.10

/* ================================================================
   5A. DEFAULTS (NEW)
   ================================================================ */

param(default_extractiveness, 0.10).
param(default_suppression, 0.10).
param(default_theater, 0.0).


% --- Global Meta-Parameters ---
% param(version, 3.4).
% param(logic_engine, 3.3).

/* ================================================================
   6. STRUCTURAL SIGNATURE THRESHOLDS (v3.2)
   These parameters enable detection of constraint ORIGIN
   (natural law vs coordination scaffold vs constructed constraint)
   rather than just constraint METRICS.
   ================================================================ */

% --- Natural Law Signature ---
% Identifies inherent properties of reality (no alternatives possible)
param(natural_law_collapse_min,      0.85).  % Extreme universal inaccessibility
param(natural_law_suppression_max,   0.15).  % No enforcement needed
param(natural_law_resistance_max,    0.15).  % Cannot be resisted

% --- Coordination Scaffold Signature ---
% Identifies successful voluntary standards (alternatives existed)
param(coordination_collapse_min,     0.85).  % Universal adoption achieved
param(coordination_suppression_max,  0.15).  % Voluntary compliance
param(coordination_resistance_max,   0.15).  % Minimal opposition

% --- Constructed Constraint Signature ---
% Identifies institutionally enforced rules (power asymmetries)
param(constructed_suppression_min,   0.20).  % Requires enforcement
param(constructed_resistance_min,    0.20).  % Faces opposition
param(constructed_beneficiary_min,   2).     % Asymmetric gains threshold

% --- Isomorphism Threshold ---
param(isomorphism_threshold, 0.85).

/* ================================================================
   7. BOLTZMANN COMPLIANCE & COUPLING TOPOLOGY (v5.0)
   Based on Tamuz & Sandomirskiy (2025), "On the origin of the
   Boltzmann distribution," Mathematische Annalen.

   The Boltzmann distribution is the ONLY law that correctly
   describes unrelated (uncoupled) systems. This section provides
   parameters for testing whether constraints satisfy this property.

   Key insight: A Mountain (NL) must show Boltzmann-compliant
   independence across index dimensions. Any constraint that
   couples independent dimensions is necessarily Constructed (CC),
   not Natural (NL).
   ================================================================ */

% --- Cross-Index Coupling Detection ---
% Maximum allowable coupling score for Boltzmann compliance.
% 0.0 = perfectly factorized (independent dimensions)
% 1.0 = maximally coupled (all dimensions entangled)
param(boltzmann_coupling_threshold,        0.25).

% Threshold above which coupling is classified as "strong"
param(boltzmann_coupling_strong_threshold, 0.50).

% Tolerance for factorization test: χ(P,S) ≈ f(P)×g(S)
% within this relative error margin
param(boltzmann_factorization_tolerance,   0.10).

% Minimum number of indexed classifications required before
% the Boltzmann compliance test is considered reliable.
% Below this, result is 'inconclusive' (epistemic access check).
param(boltzmann_min_classifications,       3).

% --- Complexity Offsets ---
% Different coordination types have different "natural" coupling
% levels. A global power grid MUST couple more dimensions than
% a simple naming convention. These offsets raise the coupling
% threshold for high-complexity coordination types.
%
% Applied as: effective_threshold = base_threshold + offset
param(complexity_offset_information_standard,  0.00).
param(complexity_offset_resource_allocation,   0.05).
param(complexity_offset_enforcement_mechanism, 0.08).
param(complexity_offset_global_infrastructure, 0.15).
param(complexity_offset_default,               0.00).

% --- Boltzmann Floor (Price of Anarchy) ---
% The minimum extraction inherent to each coordination type.
% Extraction below this floor is "necessary cost of coordination."
% Extraction above this floor is "extractive overhead" (PoA excess).
%
% These are provisional values — calibration against the corpus
% will refine them. Testsets can override via boltzmann_floor_override/2.
param(boltzmann_floor_information_standard,  0.02).
param(boltzmann_floor_resource_allocation,   0.15).
param(boltzmann_floor_enforcement_mechanism, 0.10).
param(boltzmann_floor_global_infrastructure, 0.20).
param(boltzmann_floor_default,               0.05).

% --- Reformability Scoring ---
% Thresholds for coupling-aware reformability assessment.
% High reformability: independent dimensions, low excess extraction.
% Low reformability: strongly coupled, high excess extraction.
param(reformability_high_threshold,  0.70).
param(reformability_low_threshold,   0.30).

% --- Dependency Coupling ---
% Min sign-agreement ratio for inferred coupling to create a dependency chain.
% With normalized [0,1] coupling strength, 0.70 means 70% of gradient pairs
% must agree in sign direction to infer structural dependency.
param(dependency_coupling_threshold, 0.70).

% --- Coupling Drift Detection ---
% Minimum coupling score change to register as a drift event.
param(coupling_drift_threshold,      0.10).

% --- Boltzmann Floor Drift ---
% Minimum floor increase to register as a drift event.
% Distinguishes necessary complexity increase from extractive increase.
param(boltzmann_floor_drift_threshold, 0.05).

/* ================================================================
   8. PURITY-QUALIFIED ACTION THRESHOLDS (v5.1)
   ================================================================ */
param(purity_action_sound_floor,           0.70).  % Below → monitor purity
param(purity_action_escalation_floor,      0.50).  % Below → escalate action
param(purity_action_degraded_floor,        0.30).  % Below → action type override
param(purity_surgical_reform_gate,         0.30).  % Min purity for surgical reform
param(purity_scaffold_health_gate,         0.50).  % Min scaffold purity for safe transition
param(purity_energy_max_multiplier,        3.0).   % Cap on energy cost scaling

/* ================================================================
   9. PURITY PROPAGATION NETWORK PARAMETERS (v5.2)
   ================================================================ */

% --- Network Discovery ---
param(network_coupling_threshold,              0.50).  % Min inferred coupling for edge
param(network_shared_agent_min,                1).     % Min shared agents for edge

% --- Propagation ---
param(purity_contamination_cap,                0.30).  % Max purity reduction per edge
param(purity_attenuation_factor,               0.50).  % Edge strength scaling

% --- Type Contamination Strength ---
param(purity_contamination_source_floor,       0.50).  % Below this purity → contamination source
param(contamination_strength_snare,            1.0).
param(contamination_strength_piton,            0.8).
param(contamination_strength_tangled_rope,     0.5).
param(contamination_strength_scaffold,         0.2).
param(contamination_strength_rope,             0.1).
param(contamination_strength_mountain,         0.0).   % Mountains don't contaminate

% --- Network Metrics ---
param(network_contamination_risk_threshold,    2).     % Low-purity neighbors → "at_risk"
param(network_cluster_degraded_floor,          0.40).  % Below → cluster degraded

/* ================================================================
   10. NETWORK DRIFT DYNAMICS PARAMETERS (v5.2)
   ================================================================ */

% --- Network Drift Velocity ---
param(network_drift_velocity_threshold,      0.01).  % Min EP drift/year to classify as drifting

% --- Hub Detection ---
param(network_hub_degree_threshold,          3).     % Neighbors to be a hub → severity escalation

% --- Cascade Classification ---
param(network_cascade_count_threshold,       3).     % Drifting constraints → "cascading" network

% --- Severity Escalation ---
param(network_drift_hub_escalation,          1).     % 1=enable hub-based severity escalation
