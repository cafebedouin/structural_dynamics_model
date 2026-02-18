% ============================================================================
% CONSTRAINT STORY: trump_epa_greenhouse_gas_reversal
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_trump_epa_greenhouse_gas_reversal, []).

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
 *   constraint_id: trump_epa_greenhouse_gas_reversal
 *   human_readable: Trump EPA Reversal of Greenhouse Gas Finding
 *   domain: political
 *
 * SUMMARY:
 *   The Trump administration's EPA reversed the 2009 finding that greenhouse
 *   gases endanger public health, weakening regulations on emissions. This
 *   action directly benefited polluting industries while increasing the risk to
 *   the general population.
 *
 * KEY AGENTS (by structural relationship):
 *   - US Population: Primary target (powerless/trapped) — bears increased environmental risk
 *   - Polluting Industries: Primary beneficiary (institutional/arbitrage) — benefits from reduced regulation costs
 *   - EPA (under Trump Administration): Intermediary, captured (institutional/constrained)
 *   - Scientific Community: Analytical observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trump_epa_greenhouse_gas_reversal, 0.55).
domain_priors:suppression_score(trump_epa_greenhouse_gas_reversal, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(trump_epa_greenhouse_gas_reversal, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trump_epa_greenhouse_gas_reversal, extractiveness, 0.55).
narrative_ontology:constraint_metric(trump_epa_greenhouse_gas_reversal, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(trump_epa_greenhouse_gas_reversal, theater_ratio, 0.30).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(trump_epa_greenhouse_gas_reversal, tangled_rope).
narrative_ontology:human_readable(trump_epa_greenhouse_gas_reversal, "Trump EPA Reversal of Greenhouse Gas Finding").
narrative_ontology:topic_domain(trump_epa_greenhouse_gas_reversal, "political").

% --- Binary flags ---
domain_priors:requires_active_enforcement(trump_epa_greenhouse_gas_reversal). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(trump_epa_greenhouse_gas_reversal, polluting_industries).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(trump_epa_greenhouse_gas_reversal, us_population).
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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE/MOUNTAIN)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
%
% NOTE: Per "Dynamic Coalition" extension, this agent's power may be
% upgraded to 'organized' if the constraint is a snare with a critical
% mass of victims, potentially changing the classification.
%
% UNIFORM-TYPE EXCEPTION: For natural law constraints (mountain-only) or pure
% coordination constraints (rope-only), perspectives 1 and 2 may use any power
% atoms — the classification is the same from all perspectives. Include at
% least 2-3 perspectives to demonstrate the invariance.
constraint_indexing:constraint_classification(trump_epa_greenhouse_gas_reversal, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(trump_epa_greenhouse_gas_reversal, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(trump_epa_greenhouse_gas_reversal, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES (declare when applicable) ---
% When a constraint operates between institutional actors with different
% structural relationships, declare separate perspectives for each.
% The engine differentiates via directionality: different exit_options
% produce different d values even for the same power atom.
%
% Example — Regulatory capture:
%
% Perspective 4A: Captured regulator (institutional, constrained exit)
constraint_indexing:constraint_classification(trump_epa_greenhouse_gas_reversal, rope,
     context(agent_power(institutional),
             time_horizon(generational),
             exit_options(constrained),
             spatial_scope(national))).
%
% Perspective 4B: Regulated company (institutional, arbitrage exit)
constraint_indexing:constraint_classification(trump_epa_greenhouse_gas_reversal, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trump_epa_greenhouse_gas_reversal_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(trump_epa_greenhouse_gas_reversal, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trump_epa_greenhouse_gas_reversal, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(trump_epa_greenhouse_gas_reversal, ExtMetricName, E),
    (E =< 0.25 -> true ; E >= 0.46). % Mountain or high-extraction Snare/Tangled.

:- end_tests(trump_epa_greenhouse_gas_reversal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is set to 0.55 because the reversal directly increases
 *   environmental and health risks to the population, while reducing costs for
 *   polluting industries. The suppression score is 0.70 because the reversal
 *   actively suppressed regulations and scientific consensus, but did not
 *   eliminate all alternatives (e.g., state-level regulations, consumer action).
 *   The theater ratio is low because the primary effect was a genuine policy change
 *   with real consequences.
 *
 * PERSPECTIVAL GAP:
 *   The US population experiences the reversal as a snare due to increased
 *   environmental risks and limited exit options. Polluting industries
 *   perceive it as a rope, as it enables coordination to reduce costs. The
 *   analytical observer sees the hybrid nature of the action, as a tangled rope:
 *   coordination for specific industries and asymmetric extraction from the
 *   general population.
 *
 * DIRECTIONALITY LOGIC:
 *   The US population is the primary victim, facing increased health and
 *   environmental risks with limited means of avoidance (trapped exit). Polluting
 *   industries are the primary beneficiaries, gaining cost savings and increased
 *   operational flexibility (arbitrage exit). The EPA, under the Trump
 *   administration, acted as a captured regulator, ostensibly serving the public
 *   interest but in reality favoring specific industries.
 *
 * INTER-INSTITUTIONAL DYNAMICS (if applicable):
 *  The EPA acts as a captured regulator. It is institutionally powerful, but
 *  its exit options are constrained by political pressures. This is reflected
 *  in the separate institutional perspectives, with the EPA being classified
 *  as a Rope, but with a higher derived 'd' value (closer to the US Population).
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled rope classification prevents mislabeling this situation as pure
 *   extraction. The EPA action provided a coordination function for industries,
 *   allowing them to reduce costs; it was not simply a wealth grab. However,
 *   this coordination came at the expense of the general population, justifying
 *   the "tangled" classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_trump_epa,
    'How long will the effects of this reversal persist?',
    'Longitudinal studies of environmental and health impacts',
    'If long-lasting, the impact on the population will be significantly worse.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(trump_epa_greenhouse_gas_reversal, 0, 10).

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
narrative_ontology:measurement(trump_epa_tr_t0, trump_epa_greenhouse_gas_reversal, theater_ratio, 0, 0.15).
narrative_ontology:measurement(trump_epa_tr_t5, trump_epa_greenhouse_gas_reversal, theater_ratio, 5, 0.30).
narrative_ontology:measurement(trump_epa_tr_t10, trump_epa_greenhouse_gas_reversal, theater_ratio, 10, 0.30).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(trump_epa_ex_t0, trump_epa_greenhouse_gas_reversal, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(trump_epa_ex_t5, trump_epa_greenhouse_gas_reversal, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(trump_epa_ex_t10, trump_epa_greenhouse_gas_reversal, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(trump_epa_greenhouse_gas_reversal, enforcement_mechanism).

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
% The EPA, by capturing an institutional power but having
% constrained exit, shifts toward a value of 0.30 (closer to a victim
% than a beneficiary)
constraint_indexing:directionality_override(trump_epa_greenhouse_gas_reversal, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */