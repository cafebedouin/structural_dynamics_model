:- module(config_validation, [validate_config/0]).
:- use_module(config).
:- discontiguous config_violation/1.

/* ================================================================
   CONFIG VALIDATION — param/2 constraint checking on load

   Validated params by constraint type:

   [0.0, 1.0] RANGE-BOUNDED (ratios, scores, thresholds, weights):
     Thresholds:
       system_gradient_threshold, system_gradient_strong_threshold,
       boltzmann_coupling_threshold, boltzmann_coupling_strong_threshold,
       boltzmann_factorization_tolerance,
       dependency_coupling_threshold,
       boltzmann_floor_drift_threshold,
       reformability_high_threshold, reformability_low_threshold,
       isomorphism_threshold,
       data_high_threshold, data_medium_threshold,
       network_coupling_threshold, network_drift_velocity_threshold,
       snare_load_bearing_threshold
     Ceilings:
       mountain_suppression_ceiling,
       rope_suppression_ceiling, rope_chi_ceiling, rope_epsilon_ceiling,
       tangled_rope_chi_ceil, tangled_rope_suppression_ceil,
       scaffold_extraction_ceil, snare_extraction_ceil,
       piton_extraction_ceiling
     Extraction boundaries:
       mountain_extractiveness_min, mountain_extractiveness_max,
       rope_extractiveness_min
     Floors:
       tangled_rope_suppression_floor, tangled_rope_chi_floor,
       tangled_rope_epsilon_floor,
       snare_suppression_floor, snare_chi_floor, snare_epsilon_floor,
       piton_theater_floor, piton_epsilon_floor,
       purity_action_sound_floor, purity_action_escalation_floor,
       purity_action_degraded_floor,
       purity_surgical_reform_gate, purity_scaffold_health_gate,
       purity_contamination_source_floor,
       purity_contamination_cap, purity_attenuation_factor,
       network_cluster_degraded_floor,
       reform_urgency_reformability_floor,
       excess_factor_floor, excess_factor_peak, excess_factor_center
     Contamination strengths:
       contamination_strength_snare, contamination_strength_piton,
       contamination_strength_tangled_rope, contamination_strength_scaffold,
       contamination_strength_rope, contamination_strength_mountain
     Defaults:
       default_extractiveness, default_suppression, default_theater
     Intent:
       beneficiary_gain_min, loser_loss_max_gain,
       structural_suppression_min, structural_resistance_min
     Structural signatures:
       natural_law_collapse_min, natural_law_suppression_max,
       natural_law_resistance_max,
       coordination_collapse_min, coordination_suppression_max,
       coordination_resistance_max,
       constructed_suppression_min, constructed_resistance_min
     Boltzmann floors:
       boltzmann_floor_information_standard, boltzmann_floor_resource_allocation,
       boltzmann_floor_enforcement_mechanism, boltzmann_floor_global_infrastructure,
       boltzmann_floor_default
     Complexity offsets:
       complexity_offset_information_standard, complexity_offset_resource_allocation,
       complexity_offset_enforcement_mechanism, complexity_offset_global_infrastructure,
       complexity_offset_default
     Canonical directionality:
       canonical_d_powerless, canonical_d_moderate, canonical_d_powerful,
       canonical_d_organized, canonical_d_institutional, canonical_d_analytical

   STRICTLY POSITIVE (divisors):
     excess_factor_sigma

   ORDERED THRESHOLD SETS:
     purity_action_sound_floor > purity_action_escalation_floor
       > purity_action_degraded_floor
     reform_urgency_gap_critical > reform_urgency_gap_high
       > reform_urgency_gap_moderate > reform_urgency_gap_low
     reform_urgency_pressure_critical > reform_urgency_pressure_high

   GAUSSIAN CONSISTENCY:
     excess_factor_floor =< excess_factor_peak

   CLASSIFICATION THRESHOLD CONSISTENCY:
     mountain_extractiveness_max < rope_epsilon_ceiling < snare_epsilon_floor
   ================================================================ */

%% validate_config/0
%  Checks all param/2 facts against known constraints.
%  Collects ALL violations, reports them, and halts on failure.
validate_config :-
    findall(Msg, config_violation(Msg), Violations),
    (   Violations == []
    ->  true
    ;   length(Violations, N),
        forall(member(V, Violations),
               print_message(error, format("~w", [V]))),
        format(user_error, "~n~w config violation(s) found. Halting.~n", [N]),
        halt(1)
    ).

% ============================================================
% 1. Range-bounded params [0.0, 1.0]
% ============================================================

config_violation(Msg) :-
    range_bounded_param(Name),
    config:param(Name, Value),
    number(Value),
    (Value < 0.0 ; Value > 1.0),
    format(atom(Msg),
           'CONFIG ERROR: param(~w, ~w) out of range [0.0, 1.0]',
           [Name, Value]).

range_bounded_param(P) :-
    member(P, [
        % --- Thresholds (ratio/score type) ---
        system_gradient_threshold,
        system_gradient_strong_threshold,
        boltzmann_coupling_threshold,
        boltzmann_coupling_strong_threshold,
        boltzmann_factorization_tolerance,
        dependency_coupling_threshold,
        boltzmann_floor_drift_threshold,
        reformability_high_threshold,
        reformability_low_threshold,
        isomorphism_threshold,
        data_high_threshold,
        data_medium_threshold,
        network_coupling_threshold,
        network_drift_velocity_threshold,
        snare_load_bearing_threshold,
        % --- Ceilings ---
        mountain_suppression_ceiling,
        rope_suppression_ceiling,
        rope_chi_ceiling,
        rope_epsilon_ceiling,
        tangled_rope_chi_ceil,
        tangled_rope_suppression_ceil,
        scaffold_extraction_ceil,
        snare_extraction_ceil,
        piton_extraction_ceiling,
        % --- Extraction boundaries ---
        mountain_extractiveness_min,
        mountain_extractiveness_max,
        rope_extractiveness_min,
        % --- Floors ---
        tangled_rope_suppression_floor,
        tangled_rope_chi_floor,
        tangled_rope_epsilon_floor,
        snare_suppression_floor,
        snare_chi_floor,
        snare_epsilon_floor,
        piton_theater_floor,
        piton_epsilon_floor,
        purity_action_sound_floor,
        purity_action_escalation_floor,
        purity_action_degraded_floor,
        purity_surgical_reform_gate,
        purity_scaffold_health_gate,
        purity_contamination_source_floor,
        purity_contamination_cap,
        purity_attenuation_factor,
        network_cluster_degraded_floor,
        reform_urgency_reformability_floor,
        excess_factor_floor,
        excess_factor_peak,
        excess_factor_center,
        % --- Contamination strengths ---
        contamination_strength_snare,
        contamination_strength_piton,
        contamination_strength_tangled_rope,
        contamination_strength_scaffold,
        contamination_strength_rope,
        contamination_strength_mountain,
        % --- Defaults ---
        default_extractiveness,
        default_suppression,
        default_theater,
        % --- Intent thresholds ---
        beneficiary_gain_min,
        loser_loss_max_gain,
        structural_suppression_min,
        structural_resistance_min,
        % --- Structural signature thresholds ---
        natural_law_collapse_min,
        natural_law_suppression_max,
        natural_law_resistance_max,
        coordination_collapse_min,
        coordination_suppression_max,
        coordination_resistance_max,
        constructed_suppression_min,
        constructed_resistance_min,
        % --- Boltzmann floors ---
        boltzmann_floor_information_standard,
        boltzmann_floor_resource_allocation,
        boltzmann_floor_enforcement_mechanism,
        boltzmann_floor_global_infrastructure,
        boltzmann_floor_default,
        % --- Complexity offsets ---
        complexity_offset_information_standard,
        complexity_offset_resource_allocation,
        complexity_offset_enforcement_mechanism,
        complexity_offset_global_infrastructure,
        complexity_offset_default,
        % --- Canonical directionality ---
        canonical_d_powerless,
        canonical_d_moderate,
        canonical_d_powerful,
        canonical_d_organized,
        canonical_d_institutional,
        canonical_d_analytical
    ]).

% ============================================================
% 2. Strictly positive params (divisors)
% ============================================================

config_violation(Msg) :-
    positive_divisor_param(Name),
    config:param(Name, Value),
    number(Value),
    Value =< 0,
    format(atom(Msg),
           'CONFIG ERROR: param(~w, ~w) must be > 0 (used as divisor)',
           [Name, Value]).

positive_divisor_param(excess_factor_sigma).

% ============================================================
% 3. Ordered threshold sets
% ============================================================

config_violation(Msg) :-
    ordered_pair(Name1, Name2),
    config:param(Name1, V1),
    config:param(Name2, V2),
    number(V1), number(V2),
    V1 =< V2,
    format(atom(Msg),
           'CONFIG ERROR: threshold ordering violated: ~w (~w) must be > ~w (~w)',
           [Name1, V1, Name2, V2]).

% Purity action tiers
ordered_pair(purity_action_sound_floor, purity_action_escalation_floor).
ordered_pair(purity_action_escalation_floor, purity_action_degraded_floor).

% Reform urgency gap thresholds
ordered_pair(reform_urgency_gap_critical, reform_urgency_gap_high).
ordered_pair(reform_urgency_gap_high, reform_urgency_gap_moderate).
ordered_pair(reform_urgency_gap_moderate, reform_urgency_gap_low).

% Reform urgency pressure thresholds
ordered_pair(reform_urgency_pressure_critical, reform_urgency_pressure_high).

% ============================================================
% 4. Gaussian consistency
% ============================================================

config_violation(Msg) :-
    config:param(excess_factor_floor, Floor),
    config:param(excess_factor_peak, Peak),
    number(Floor), number(Peak),
    Floor > Peak,
    format(atom(Msg),
           'CONFIG ERROR: excess_factor_floor (~w) exceeds excess_factor_peak (~w)',
           [Floor, Peak]).

% ============================================================
% 5. Classification threshold consistency
% ============================================================
% mountain_extractiveness_max < rope_epsilon_ceiling < snare_epsilon_floor
% Ensures no overlap that would make multiple classification gates
% simultaneously satisfiable in ways clause ordering doesn't handle.

config_violation(Msg) :-
    config:param(mountain_extractiveness_max, MtnMax),
    config:param(rope_epsilon_ceiling, RopeCeil),
    number(MtnMax), number(RopeCeil),
    MtnMax >= RopeCeil,
    format(atom(Msg),
           'CONFIG ERROR: classification overlap: mountain_extractiveness_max (~w) must be < rope_epsilon_ceiling (~w)',
           [MtnMax, RopeCeil]).

config_violation(Msg) :-
    config:param(rope_epsilon_ceiling, RopeCeil),
    config:param(snare_epsilon_floor, SnareFloor),
    number(RopeCeil), number(SnareFloor),
    RopeCeil >= SnareFloor,
    format(atom(Msg),
           'CONFIG ERROR: classification overlap: rope_epsilon_ceiling (~w) must be < snare_epsilon_floor (~w)',
           [RopeCeil, SnareFloor]).

% ============================================================
% Fire on module load
% ============================================================

:- initialization(validate_config).

% ============================================================
% Tests
% ============================================================

:- begin_tests(config_validation_tests).

test(catches_range_violation) :-
    config:param(boltzmann_coupling_threshold, Original),
    retractall(config:param(boltzmann_coupling_threshold, _)),
    assertz(config:param(boltzmann_coupling_threshold, 1.5)),
    findall(Msg, config_violation(Msg), Violations),
    retractall(config:param(boltzmann_coupling_threshold, _)),
    assertz(config:param(boltzmann_coupling_threshold, Original)),
    once((member(V, Violations), sub_atom(V, _, _, _, 'out of range'))).

test(catches_negative_range_violation) :-
    config:param(dependency_coupling_threshold, Original),
    retractall(config:param(dependency_coupling_threshold, _)),
    assertz(config:param(dependency_coupling_threshold, -0.1)),
    findall(Msg, config_violation(Msg), Violations),
    retractall(config:param(dependency_coupling_threshold, _)),
    assertz(config:param(dependency_coupling_threshold, Original)),
    once((member(V, Violations), sub_atom(V, _, _, _, 'out of range'))).

test(catches_divisor_violation) :-
    config:param(excess_factor_sigma, Original),
    retractall(config:param(excess_factor_sigma, _)),
    assertz(config:param(excess_factor_sigma, 0.0)),
    findall(Msg, config_violation(Msg), Violations),
    retractall(config:param(excess_factor_sigma, _)),
    assertz(config:param(excess_factor_sigma, Original)),
    once((member(V, Violations), sub_atom(V, _, _, _, 'must be > 0'))).

test(catches_ordering_violation) :-
    config:param(purity_action_sound_floor, Original),
    retractall(config:param(purity_action_sound_floor, _)),
    assertz(config:param(purity_action_sound_floor, 0.10)),
    findall(Msg, config_violation(Msg), Violations),
    retractall(config:param(purity_action_sound_floor, _)),
    assertz(config:param(purity_action_sound_floor, Original)),
    once((member(V, Violations), sub_atom(V, _, _, _, 'threshold ordering violated'))).

test(catches_gaussian_violation) :-
    config:param(excess_factor_floor, OrigFloor),
    config:param(excess_factor_peak, OrigPeak),
    retractall(config:param(excess_factor_floor, _)),
    retractall(config:param(excess_factor_peak, _)),
    assertz(config:param(excess_factor_floor, 0.9)),
    assertz(config:param(excess_factor_peak, 0.5)),
    findall(Msg, config_violation(Msg), Violations),
    retractall(config:param(excess_factor_floor, _)),
    retractall(config:param(excess_factor_peak, _)),
    assertz(config:param(excess_factor_floor, OrigFloor)),
    assertz(config:param(excess_factor_peak, OrigPeak)),
    once((member(V, Violations), sub_atom(V, _, _, _, 'excess_factor_floor'))).

test(valid_config_passes) :-
    findall(_, config_violation(_), Violations),
    length(Violations, 0).

:- end_tests(config_validation_tests).
