:- module(config_schema, [
    param_spec/4,
    param_relationship/3
]).

/* ================================================================
   CONFIG SCHEMA — Declarative specification for every param/2 fact
   ================================================================

   param_spec(+Name, +Type, +Constraint, +Description)
     Type:       number | integer | atom
     Constraint: range(Lo, Hi) | positive | non_positive | oneof(List) | any

   param_relationship(+Name, +ViolationGoal, +Description)
     ViolationGoal succeeds when the relationship is VIOLATED.

   Adding a new config parameter:
     1. Add param(Name, Default) to config.pl
     2. Add one param_spec(Name, Type, Constraint, Desc) fact here

   Note on sigmoid/canonical_d calibration:
     The canonical_d values are calibrated so f(d) = L + (U-L)/(1+exp(-k*(d-d0)))
     approximates the corresponding power_modifier values. If sigmoid params
     change, canonical_d values must be recalibrated. This is a calibration
     dependency, not a hard invariant — see config.pl lines 102-106.
   ================================================================ */

% ============================================================
% 1. METRIC NAMING (dynamic dispatch keys)
% ============================================================
% Open set — testsets define new metric values. Type-check only.

param_spec(suppression_metric_name,    atom, any, "Dynamic dispatch key for suppression metric").
param_spec(extractiveness_metric_name, atom, any, "Dynamic dispatch key for extractiveness metric").
param_spec(temporal_metric_name,       atom, any, "Dynamic dispatch key for time horizon metric").
param_spec(exit_metric_name,           atom, any, "Dynamic dispatch key for exit options metric").
param_spec(power_metric_name,          atom, any, "Dynamic dispatch key for agent power metric").
param_spec(scope_metric_name,          atom, any, "Dynamic dispatch key for spatial scope metric").
param_spec(theater_metric_name,        atom, any, "Dynamic dispatch key for theater ratio metric").

% ============================================================
% 2. POWER MODIFIERS (pi)
% ============================================================
% Negative allowed (institutional = -0.2 is net beneficiary).
% Range accommodates current values from -0.2 to 1.5.

param_spec(power_modifier_powerless,     number, range(-1.0, 2.0), "Extraction amplified for powerless agents").
param_spec(power_modifier_moderate,      number, range(-1.0, 2.0), "Baseline extraction for moderate agents").
param_spec(power_modifier_powerful,      number, range(-1.0, 2.0), "Extraction reduced for powerful agents").
param_spec(power_modifier_organized,     number, range(-1.0, 2.0), "Shared burden for organized/collective agents").
param_spec(power_modifier_institutional, number, range(-1.0, 2.0), "Net beneficiary modifier (negative allowed)").
param_spec(power_modifier_analytical,    number, range(-1.0, 2.0), "Analytical clarity modifier").

% ============================================================
% 3. COALITION MODELING
% ============================================================

param_spec(critical_mass_threshold, integer, positive, "Min shared victims for organized power modifier").

% ============================================================
% 4. SCOPE MODIFIERS (sigma)
% ============================================================
% Multiplicative factors for verification difficulty. Must be positive.

param_spec(scope_modifier_local,       number, positive, "Scope factor: local (easy verification)").
param_spec(scope_modifier_regional,    number, positive, "Scope factor: regional").
param_spec(scope_modifier_national,    number, positive, "Scope factor: national (baseline)").
param_spec(scope_modifier_continental, number, positive, "Scope factor: continental").
param_spec(scope_modifier_global,      number, positive, "Scope factor: global (hardest verification)").
param_spec(scope_modifier_universal,   number, positive, "Scope factor: universal (natural laws)").

% ============================================================
% 5. SIGMOID DIRECTIONALITY
% ============================================================
% f(d) = L + (U - L) / (1 + exp(-k * (d - d0)))

param_spec(sigmoid_lower,     number, range(-1.0, 1.0), "Sigmoid lower asymptote L").
param_spec(sigmoid_upper,     number, range(0.5, 3.0),  "Sigmoid upper asymptote U").
param_spec(sigmoid_midpoint,  number, range(0.0, 1.0),  "Sigmoid inflection point d0").
param_spec(sigmoid_steepness, number, positive,          "Sigmoid steepness parameter k").

% ============================================================
% 6. CANONICAL DIRECTIONALITY
% ============================================================
% Calibrated d-positions so f(d) approximates power_modifier values.

param_spec(canonical_d_powerless,     number, range(0.0, 1.0), "Canonical directionality for powerless").
param_spec(canonical_d_moderate,      number, range(0.0, 1.0), "Canonical directionality for moderate").
param_spec(canonical_d_powerful,      number, range(0.0, 1.0), "Canonical directionality for powerful").
param_spec(canonical_d_organized,     number, range(0.0, 1.0), "Canonical directionality for organized").
param_spec(canonical_d_institutional, number, range(0.0, 1.0), "Canonical directionality for institutional").
param_spec(canonical_d_analytical,    number, range(0.0, 1.0), "Canonical directionality for analytical").

% ============================================================
% 7. INTENT & DETECTION THRESHOLDS
% ============================================================

param_spec(system_gradient_threshold,        number, range(0.0, 1.0), "Min change for non-stable gradient").
param_spec(system_gradient_strong_threshold, number, range(0.0, 1.0), "Threshold for Strong intent classification").
param_spec(beneficiary_gain_min,             number, range(0.0, 1.0), "Min power gain for main beneficiary ID").
param_spec(loser_loss_max_gain,              number, range(0.0, 1.0), "Max gain for non-beneficiaries (asymmetry)").
param_spec(structural_suppression_min,       number, range(0.0, 1.0), "Required structural suppression alignment").
param_spec(structural_resistance_min,        number, range(0.0, 1.0), "Required structural resistance alignment").
param_spec(data_high_threshold,              number, range(0.0, 1.0), "High confidence classification threshold").
param_spec(data_medium_threshold,            number, range(0.0, 1.0), "Medium confidence classification threshold").

% ============================================================
% 8. DEFERENTIAL REALISM — Mountain Boundaries
% ============================================================

param_spec(mountain_suppression_ceiling,          number, range(0.0, 1.0), "Noise floor for mountain suppression").
param_spec(mountain_extractiveness_min,           number, range(0.0, 1.0), "Mountain extractiveness lower bound").
param_spec(mountain_extractiveness_max,           number, range(0.0, 1.0), "Mountain extractiveness upper bound").
param_spec(false_mountain_extraction_threshold,   number, range(0.0, 1.0), "Claimed mountain reclassification threshold").

% ============================================================
% 9. DEFERENTIAL REALISM — Rope Boundaries
% ============================================================

param_spec(rope_extractiveness_min, number, range(0.0, 1.0), "Rope extractiveness lower bound").
param_spec(rope_extraction_ceiling, number, range(0.0, 1.0), "Registry classification: epsilon <= this -> rope").
param_spec(rope_suppression_ceiling, number, range(0.0, 1.0), "Base suppression ceiling for pure coordination").

% ============================================================
% 10. DEFERENTIAL REALISM — Tangled Rope Boundaries
% ============================================================

param_spec(tangled_rope_extraction_floor,   number, range(0.0, 1.0), "Registry tangled rope extraction lower bound").
param_spec(tangled_rope_extraction_ceil,    number, range(0.0, 1.0), "Registry tangled rope extraction upper bound").
param_spec(tangled_rope_suppression_floor,  number, range(0.0, 1.0), "Tangled rope requires active enforcement above this").
param_spec(tangled_rope_suppression_ceil,   number, range(0.0, 1.0), "Tangled rope suppression upper bound").

% ============================================================
% 11. DEFERENTIAL REALISM — Snare Boundaries
% ============================================================

param_spec(snare_extraction_ceil,        number, range(0.0, 1.0), "Snare extraction upper bound").
param_spec(snare_suppression_floor,      number, range(0.0, 1.0), "Snare suppression lower bound").
param_spec(snare_load_bearing_threshold, number, range(0.0, 1.0), "Load-bearing snare threshold (Theorem 3)").

% ============================================================
% 12. DUAL-THRESHOLD CLASSIFICATION (chi/epsilon)
% ============================================================

param_spec(rope_chi_ceiling,            number, range(0.0, 1.0), "Rule R: max chi for rope classification").
param_spec(rope_epsilon_ceiling,        number, range(0.0, 1.0), "Rule R: max epsilon for rope classification").
param_spec(snare_chi_floor,             number, range(0.0, 1.0), "Rule N: min chi for snare classification").
param_spec(snare_epsilon_floor,         number, range(0.0, 1.0), "Rule N: min epsilon for snare classification").
param_spec(tangled_rope_chi_floor,      number, range(0.0, 1.0), "Rule TR: min chi for tangled rope").
param_spec(tangled_rope_chi_ceil,       number, range(0.0, 1.0), "Rule TR: max chi for tangled rope").
param_spec(tangled_rope_epsilon_floor,  number, range(0.0, 1.0), "Rule TR: min epsilon for tangled rope").

% ============================================================
% 13. SCAFFOLD & PITON
% ============================================================

param_spec(scaffold_extraction_ceil,  number, range(0.0, 1.0), "Max extraction for scaffold classification").
param_spec(piton_extraction_ceiling,  number, range(0.0, 1.0), "Max extraction for piton classification").
param_spec(piton_theater_floor,       number, range(0.0, 1.0), "Min theater ratio for piton classification").
param_spec(piton_epsilon_floor,       number, range(0.0, 1.0), "Rule Z: min epsilon for piton").

% ============================================================
% 14. AUDIT TRIAGE (Python-only, still config-validated)
% ============================================================

param_spec(audit_theater_naturalization_threshold, number, range(0.0, 1.0), "Audit triage naturalization threshold").
param_spec(audit_theater_conflict_threshold,       number, range(0.0, 1.0), "Audit triage conflict threshold").

% ============================================================
% 15. DEFAULTS
% ============================================================

param_spec(default_extractiveness, number, range(0.0, 1.0), "Default extractiveness when data missing").
param_spec(default_suppression,    number, range(0.0, 1.0), "Default suppression when data missing").
param_spec(default_theater,        number, range(0.0, 1.0), "Default theater ratio when data missing").

% ============================================================
% 16. STRUCTURAL SIGNATURE THRESHOLDS
% ============================================================

param_spec(natural_law_collapse_min,     number, range(0.0, 1.0), "Natural law: min universal inaccessibility").
param_spec(natural_law_suppression_max,  number, range(0.0, 1.0), "Natural law: max enforcement needed").
param_spec(natural_law_resistance_max,   number, range(0.0, 1.0), "Natural law: max resistance possible").
param_spec(coordination_collapse_min,    number, range(0.0, 1.0), "Coordination scaffold: min universal adoption").
param_spec(coordination_suppression_max, number, range(0.0, 1.0), "Coordination scaffold: max enforcement").
param_spec(coordination_resistance_max,  number, range(0.0, 1.0), "Coordination scaffold: max opposition").
param_spec(constructed_suppression_min,  number, range(0.0, 1.0), "Constructed constraint: min enforcement").
param_spec(constructed_resistance_min,   number, range(0.0, 1.0), "Constructed constraint: min opposition").
param_spec(constructed_beneficiary_min,  integer, positive,        "Constructed constraint: asymmetric gains threshold").
param_spec(isomorphism_threshold,        number, range(0.0, 1.0), "Structural isomorphism detection threshold").

% ============================================================
% 17. BOLTZMANN COMPLIANCE & COUPLING
% ============================================================

param_spec(boltzmann_coupling_threshold,        number, range(0.0, 1.0), "Max coupling for Boltzmann compliance").
param_spec(boltzmann_coupling_strong_threshold, number, range(0.0, 1.0), "Coupling classified as strong above this").
param_spec(boltzmann_factorization_tolerance,   number, range(0.0, 1.0), "Relative error for factorization test").
param_spec(boltzmann_min_classifications,       integer, positive,        "Min classifications for reliable Boltzmann test").

% ============================================================
% 18. COMPLEXITY OFFSETS
% ============================================================

param_spec(complexity_offset_information_standard,  number, range(0.0, 1.0), "Coupling offset: information standards").
param_spec(complexity_offset_resource_allocation,   number, range(0.0, 1.0), "Coupling offset: resource allocation").
param_spec(complexity_offset_enforcement_mechanism, number, range(0.0, 1.0), "Coupling offset: enforcement mechanisms").
param_spec(complexity_offset_global_infrastructure, number, range(0.0, 1.0), "Coupling offset: global infrastructure").
param_spec(complexity_offset_default,               number, range(0.0, 1.0), "Coupling offset: default/unknown type").

% ============================================================
% 19. BOLTZMANN FLOOR (Price of Anarchy)
% ============================================================

param_spec(boltzmann_floor_information_standard,  number, range(0.0, 1.0), "Min extraction inherent to info standards").
param_spec(boltzmann_floor_resource_allocation,   number, range(0.0, 1.0), "Min extraction inherent to resource allocation").
param_spec(boltzmann_floor_enforcement_mechanism, number, range(0.0, 1.0), "Min extraction inherent to enforcement").
param_spec(boltzmann_floor_global_infrastructure, number, range(0.0, 1.0), "Min extraction inherent to global infra").
param_spec(boltzmann_floor_default,               number, range(0.0, 1.0), "Min extraction inherent: default type").

% ============================================================
% 20. REFORMABILITY SCORING
% ============================================================

param_spec(reformability_high_threshold, number, range(0.0, 1.0), "High reformability threshold").
param_spec(reformability_low_threshold,  number, range(0.0, 1.0), "Low reformability threshold").

% ============================================================
% 21. EXCESS EXTRACTION FACTOR (Gaussian)
% ============================================================

param_spec(excess_factor_center, number, range(0.0, 1.0), "Gaussian sweet spot for moderate excess").
param_spec(excess_factor_sigma,  number, positive,         "Gaussian width parameter (divisor, must be > 0)").
param_spec(excess_factor_peak,   number, range(0.0, 1.0), "Max factor at center of Gaussian").
param_spec(excess_factor_floor,  number, range(0.0, 1.0), "Min factor at extremes of Gaussian").

% ============================================================
% 22. DEPENDENCY COUPLING
% ============================================================

param_spec(dependency_coupling_threshold, number, range(0.0, 1.0), "Min sign-agreement ratio for dependency chain").

% ============================================================
% 23. REFORM URGENCY THRESHOLDS
% ============================================================

param_spec(reform_urgency_gap_critical,        number, range(0.0, 1.0), "Gap threshold for critical urgency").
param_spec(reform_urgency_gap_high,            number, range(0.0, 1.0), "Gap threshold for high urgency").
param_spec(reform_urgency_gap_moderate,        number, range(0.0, 1.0), "Gap threshold for moderate urgency").
param_spec(reform_urgency_gap_low,             number, range(0.0, 1.0), "Gap threshold for low urgency").
param_spec(reform_urgency_pressure_critical,   number, positive,        "Pressure threshold for critical urgency").
param_spec(reform_urgency_pressure_high,       number, positive,        "Pressure threshold for high urgency").
param_spec(reform_urgency_reformability_floor, number, range(0.0, 1.0), "Reformability floor for urgency downgrade").

% ============================================================
% 24. BOLTZMANN FLOOR DRIFT
% ============================================================

param_spec(boltzmann_floor_drift_threshold, number, range(0.0, 1.0), "Min floor increase to register as drift event").

% ============================================================
% 25. PURITY-QUALIFIED ACTION THRESHOLDS
% ============================================================

param_spec(purity_action_sound_floor,      number, range(0.0, 1.0), "Below this purity -> monitor").
param_spec(purity_action_escalation_floor, number, range(0.0, 1.0), "Below this purity -> escalate action").
param_spec(purity_action_degraded_floor,   number, range(0.0, 1.0), "Below this purity -> action override").
param_spec(purity_surgical_reform_gate,    number, range(0.0, 1.0), "Min purity for surgical reform").
param_spec(purity_scaffold_health_gate,    number, range(0.0, 1.0), "Min scaffold purity for safe transition").
param_spec(purity_energy_max_multiplier,   number, positive,        "Cap on energy cost scaling").

% ============================================================
% 26. PURITY PROPAGATION NETWORK
% ============================================================

param_spec(network_coupling_threshold,              number, range(0.0, 1.0), "Min inferred coupling for network edge").
param_spec(network_shared_agent_min,                integer, positive,        "Min shared agents for network edge").
param_spec(purity_contamination_cap,                number, range(0.0, 1.0), "Max purity reduction per edge").
param_spec(purity_attenuation_factor,               number, range(0.0, 1.0), "Edge strength scaling factor").
param_spec(purity_contamination_source_floor,       number, range(0.0, 1.0), "Below this purity -> contamination source").
param_spec(contamination_strength_snare,            number, range(0.0, 1.0), "Contamination strength: snare type").
param_spec(contamination_strength_piton,            number, range(0.0, 1.0), "Contamination strength: piton type").
param_spec(contamination_strength_tangled_rope,     number, range(0.0, 1.0), "Contamination strength: tangled rope type").
param_spec(contamination_strength_scaffold,         number, range(0.0, 1.0), "Contamination strength: scaffold type").
param_spec(contamination_strength_rope,             number, range(0.0, 1.0), "Contamination strength: rope type").
param_spec(contamination_strength_mountain,         number, range(0.0, 1.0), "Contamination strength: mountain type (0=none)").
param_spec(contamination_strength_indexically_opaque, number, range(0.0, 1.0), "Contamination strength: indexically opaque").

% ============================================================
% 27. NETWORK METRICS
% ============================================================

param_spec(network_contamination_risk_threshold, integer, positive,        "Low-purity neighbors count for at_risk").
param_spec(network_cluster_degraded_floor,       number, range(0.0, 1.0), "Below this purity -> cluster degraded").

% ============================================================
% 28. FIXED-POINT NETWORK ITERATION
% ============================================================

param_spec(fpn_epsilon,        number, positive,        "FPN convergence threshold (divisor, must be > 0)").
param_spec(fpn_max_iterations, integer, positive,        "FPN hard iteration cap").
param_spec(fpn_enabled,        integer, oneof([0, 1]),  "0=one-hop propagation, 1=fixed-point iteration").

% ============================================================
% 29. NETWORK DRIFT DYNAMICS
% ============================================================

param_spec(network_drift_velocity_threshold, number, range(0.0, 1.0), "Min EP drift/year for drifting classification").
param_spec(network_hub_degree_threshold,     integer, positive,        "Neighbors to qualify as network hub").
param_spec(network_cascade_count_threshold,  integer, positive,        "Drifting constraints for cascade classification").
param_spec(network_drift_hub_escalation,     integer, oneof([0, 1]),  "1=enable hub-based severity escalation").

% ============================================================
% 30. MAXIMUM ENTROPY SHADOW CLASSIFIER
% ============================================================

param_spec(maxent_enabled,                     integer, oneof([0, 1]),             "0=disabled, 1=enabled").
param_spec(maxent_uncertainty_threshold,        number,  range(0.0, 1.0),          "H_norm above this = flagged").
param_spec(maxent_disagreement_prob_threshold,  number,  range(0.0, 1.0),          "P(det_type) below this = soft disagreement").
param_spec(maxent_boolean_penalty,              number,  non_positive,              "Log-likelihood for violated boolean gate").
param_spec(maxent_boolean_bonus,                number,  range(0.0, 1.0),          "Log-likelihood for satisfied bonus feature").
param_spec(maxent_prior_mode,                   atom,    oneof([corpus, uniform]),  "MaxEnt prior distribution mode").
param_spec(maxent_signature_override_strength,  number,  range(0.0, 1.0),          "P assigned to unconditional override target").

% ============================================================
% 31. ABDUCTIVE REASONING ENGINE
% ============================================================

param_spec(abductive_enabled,                     integer, oneof([0, 1]),                      "0=disabled, 1=enabled").
param_spec(abductive_confidence_floor,            number,  range(0.0, 1.0),                    "Min confidence for hypothesis storage").
param_spec(abductive_fpn_divergence_threshold,    number,  range(0.0, 1.0),                    "FPN EP divergence threshold for triggers").
param_spec(abductive_maxent_mountain_deception,   number,  range(0.0, 1.0),                    "P(mountain) threshold for deep_deception").
param_spec(abductive_dormant_entropy_ceiling,     number,  range(0.0, 1.0),                    "Max H_norm for dormant_extraction trigger").
param_spec(abductive_shadow_divergence_threshold, number,  range(0.0, 1.0),                    "Min P(MaxEntTop) for shadow divergence").
param_spec(abductive_stress_convergence_min,      integer, positive,                            "Min signals for stress convergence common core").
param_spec(abductive_snare_lean_psi_threshold,    number,  range(0.0, 1.0),                    "Min psi for snare-leaning tangled rope").
param_spec(abductive_snare_lean_psnare_floor,     number,  range(0.0, 1.0),                    "Min P(snare) for snare-leaning tangled rope").
param_spec(abductive_stress_purity_threshold,     number,  range(0.0, 1.0),                    "Purity below this = stressed indicator").
param_spec(abductive_stress_coupling_threshold,   number,  range(0.0, 1.0),                    "Coupling above this = stressed indicator").
param_spec(abductive_stress_entropy_threshold,    number,  range(0.0, 1.0),                    "Entropy above this = stressed indicator").
param_spec(abductive_stress_drift_mode,           atom,    oneof([any, critical, count_2plus]), "Stress convergence drift mode selector").

% ============================================================
% 32. TRAJECTORY MINING
% ============================================================

param_spec(trajectory_enabled,                  integer, oneof([0, 1]),  "0=disabled, 1=enabled").
param_spec(trajectory_distance_shift_weight,    number,  range(0.0, 1.0), "Weight for shift (type sequence) distance").
param_spec(trajectory_distance_metric_weight,   number,  range(0.0, 1.0), "Weight for metric (chi, entropy) distance").
param_spec(trajectory_distance_stability_weight, number, range(0.0, 1.0), "Weight for stability (coupling, purity) distance").
param_spec(trajectory_distance_pathology_weight, number, range(0.0, 1.0), "Weight for pathology (drift, voids) distance").
param_spec(trajectory_family_cut_level,         number,  range(0.0, 1.0), "Dendrogram cut height for family assignment").
param_spec(trajectory_isomorphism_threshold,    number,  range(0.0, 1.0), "Max distance for trajectory isomorphism").
param_spec(trajectory_coupling_band_width,      number,  range(0.0, 1.0), "Coupling match tolerance for isomorphism").

% ============================================================
% 33. GROTHENDIECK COHOMOLOGY
% ============================================================

param_spec(cohomology_enabled, integer, oneof([0, 1]), "0=disabled, 1=enabled").


/* ================================================================
   PARAMETER RELATIONSHIPS — Cross-param invariants
   ================================================================
   param_relationship(+Name, +ViolationGoal, +Description)
   ViolationGoal succeeds when the relationship is VIOLATED.
   ================================================================ */

% --- Sigmoid ordering: L < d0 < U ---
param_relationship(sigmoid_ordering,
    (   config:param(sigmoid_lower, L),
        config:param(sigmoid_midpoint, M),
        config:param(sigmoid_upper, U),
        \+ (L < M, M < U)
    ),
    "sigmoid params must be ordered: lower < midpoint < upper").

% --- Data threshold ordering ---
param_relationship(data_threshold_ordering,
    (   config:param(data_high_threshold, H),
        config:param(data_medium_threshold, M),
        H =< M
    ),
    "data_high_threshold must be > data_medium_threshold").

% --- Reformability threshold ordering ---
param_relationship(reformability_ordering,
    (   config:param(reformability_high_threshold, H),
        config:param(reformability_low_threshold, L),
        H =< L
    ),
    "reformability_high_threshold must be > reformability_low_threshold").

% --- Purity action tiers: sound > escalation > degraded ---
param_relationship(purity_tier_sound_escalation,
    (   config:param(purity_action_sound_floor, V1),
        config:param(purity_action_escalation_floor, V2),
        V1 =< V2
    ),
    "purity_action_sound_floor must be > purity_action_escalation_floor").

param_relationship(purity_tier_escalation_degraded,
    (   config:param(purity_action_escalation_floor, V1),
        config:param(purity_action_degraded_floor, V2),
        V1 =< V2
    ),
    "purity_action_escalation_floor must be > purity_action_degraded_floor").

% --- Reform urgency gap thresholds: critical > high > moderate > low ---
param_relationship(reform_gap_critical_high,
    (   config:param(reform_urgency_gap_critical, V1),
        config:param(reform_urgency_gap_high, V2),
        V1 =< V2
    ),
    "reform_urgency_gap_critical must be > reform_urgency_gap_high").

param_relationship(reform_gap_high_moderate,
    (   config:param(reform_urgency_gap_high, V1),
        config:param(reform_urgency_gap_moderate, V2),
        V1 =< V2
    ),
    "reform_urgency_gap_high must be > reform_urgency_gap_moderate").

param_relationship(reform_gap_moderate_low,
    (   config:param(reform_urgency_gap_moderate, V1),
        config:param(reform_urgency_gap_low, V2),
        V1 =< V2
    ),
    "reform_urgency_gap_moderate must be > reform_urgency_gap_low").

% --- Reform urgency pressure thresholds ---
param_relationship(reform_pressure_ordering,
    (   config:param(reform_urgency_pressure_critical, V1),
        config:param(reform_urgency_pressure_high, V2),
        V1 =< V2
    ),
    "reform_urgency_pressure_critical must be > reform_urgency_pressure_high").

% --- Gaussian consistency: floor <= peak ---
param_relationship(gaussian_floor_peak,
    (   config:param(excess_factor_floor, F),
        config:param(excess_factor_peak, P),
        F > P
    ),
    "excess_factor_floor must be <= excess_factor_peak").

% --- Classification chain: mountain < rope < snare ---
param_relationship(classification_mountain_rope,
    (   config:param(mountain_extractiveness_max, MtnMax),
        config:param(rope_epsilon_ceiling, RopeCeil),
        MtnMax >= RopeCeil
    ),
    "mountain_extractiveness_max must be < rope_epsilon_ceiling").

param_relationship(classification_rope_snare,
    (   config:param(rope_epsilon_ceiling, RopeCeil),
        config:param(snare_epsilon_floor, SnareFloor),
        RopeCeil >= SnareFloor
    ),
    "rope_epsilon_ceiling must be < snare_epsilon_floor").

% --- Registry tangled rope range ---
param_relationship(tangled_rope_extraction_range,
    (   config:param(tangled_rope_extraction_floor, TRFloor),
        config:param(tangled_rope_extraction_ceil, TRCeil),
        TRFloor >= TRCeil
    ),
    "tangled_rope_extraction_floor must be < tangled_rope_extraction_ceil").

% --- Registry rope/snare boundary ---
param_relationship(registry_rope_snare,
    (   config:param(rope_extraction_ceiling, RopeCeil),
        config:param(snare_epsilon_floor, SnareFloor),
        RopeCeil >= SnareFloor
    ),
    "rope_extraction_ceiling must be < snare_epsilon_floor").

% --- Canonical d ordering: institutional < organized < powerful < moderate ---
param_relationship(canonical_d_ordering,
    (   config:param(canonical_d_institutional, I),
        config:param(canonical_d_organized, O),
        config:param(canonical_d_powerful, P),
        config:param(canonical_d_moderate, M),
        \+ (I < O, O < P, P < M)
    ),
    "canonical_d must be ordered: institutional < organized < powerful < moderate").

% --- Trajectory distance weights must sum to ~1.0 (only checked when enabled) ---
param_relationship(trajectory_weights_sum,
    (   config:param(trajectory_enabled, 1),
        config:param(trajectory_distance_shift_weight, W1),
        config:param(trajectory_distance_metric_weight, W2),
        config:param(trajectory_distance_stability_weight, W3),
        config:param(trajectory_distance_pathology_weight, W4),
        Sum is W1 + W2 + W3 + W4,
        abs(Sum - 1.0) > 0.01
    ),
    "trajectory distance weights must sum to ~1.0 when trajectory_enabled=1").
