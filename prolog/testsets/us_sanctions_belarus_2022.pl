% ============================================================================
% CONSTRAINT STORY: us_sanctions_belarus_2022
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_us_sanctions_belarus_2022, []).

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
 *   constraint_id: us_sanctions_belarus_2022
 *   human_readable: U.S. Trade Sanctions on Belarus (Post-2022)
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   This constraint models the comprehensive trade and financial sanctions
 *   imposed by the United States on Belarus, primarily targeting state-owned
 *   enterprises and government officials. The sanctions serve a dual purpose:
 *   coordinating an international coalition to exert pressure on the
 *   Lukashenka regime for its role in the Ukraine conflict and its human
 *   rights record, while simultaneously extracting economic opportunity and
 *   resources from the Belarusian state. The lifting of some sanctions,
 *   as mentioned in the source article, represents a modification or
 *   partial dissolution of this constraint.
 *
 * KEY AGENTS (by structural relationship):
 *   - Belarusian State & State-Owned Enterprises: Primary target (institutional/trapped) — bears direct economic extraction.
 *   - U.S. Government & Allied Nations: Primary beneficiary (institutional/arbitrage) — uses sanctions as a foreign policy tool.
 *   - Belarusian private sector & citizens: Secondary target (powerless/trapped) — suffers collateral economic damage.
 *   - Analytical observer: Analytical agent — sees the dual coordination/extraction function.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_sanctions_belarus_2022, 0.65).
domain_priors:suppression_score(us_sanctions_belarus_2022, 0.80).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(us_sanctions_belarus_2022, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_sanctions_belarus_2022, extractiveness, 0.65).
narrative_ontology:constraint_metric(us_sanctions_belarus_2022, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(us_sanctions_belarus_2022, theater_ratio, 0.15).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(us_sanctions_belarus_2022, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(us_sanctions_belarus_2022). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(us_sanctions_belarus_2022, us_government_and_allies).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(us_sanctions_belarus_2022, belarusian_state_entities).
narrative_ontology:constraint_victim(us_sanctions_belarus_2022, belarusian_private_sector).
%
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
   ========================================================================== */

% --- INTER-INSTITUTIONAL PERSPECTIVES ---
% This constraint is defined by the relationship between two institutional
% actors with opposite structural positions and exit options.

% PERSPECTIVE 1: THE SANCTIONED STATE (THE TARGET)
% The Belarusian government experiences the sanctions as a coercive trap.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% χ = 0.65 * f(0.95) * σ(global) = 0.65 * 1.42 * 1.2 ≈ 1.11. This is a clear Snare.
constraint_indexing:constraint_classification(us_sanctions_belarus_2022, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE SANCTIONING STATE (THE BENEFICIARY)
% The U.S. government views the sanctions as a flexible policy tool.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
% χ = 0.65 * f(0.05) * σ(global) = 0.65 * -0.12 * 1.2 ≈ -0.09. This is a Rope.
constraint_indexing:constraint_classification(us_sanctions_belarus_2022, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The observer sees both the coordination function and the asymmetric extraction.
% Engine derives canonical d ≈ 0.72 → f(d) ≈ 1.15.
% χ = 0.65 * f(0.72) * σ(global) = 0.65 * 1.15 * 1.2 ≈ 0.90. Meets Tangled Rope criteria.
constraint_indexing:constraint_classification(us_sanctions_belarus_2022, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: COLLATERAL TARGET (Belarusian private sector/citizens)
% Experiences the sanctions as an inescapable economic reality.
% Similar to the state target, but with less power.
constraint_indexing:constraint_classification(us_sanctions_belarus_2022, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_sanctions_belarus_2022_tests).

test(inter_institutional_perspectival_gap) :-
    % Verify the core perspectival gap between the two institutional actors.
    constraint_indexing:constraint_classification(us_sanctions_belarus_2022, TypeTarget,
        context(agent_power(institutional), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(us_sanctions_belarus_2022, TypeBeneficiary,
        context(agent_power(institutional), _, exit_options(arbitrage), _)),
    TypeTarget == snare,
    TypeBeneficiary == rope.

test(analytical_view_is_tangled_rope) :-
    % The ground truth claim must be Tangled Rope.
    constraint_indexing:constraint_classification(us_sanctions_belarus_2022, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    % Verify that all three structural requirements for a Tangled Rope are declared.
    narrative_ontology:constraint_beneficiary(us_sanctions_belarus_2022, _),
    narrative_ontology:constraint_victim(us_sanctions_belarus_2022, _),
    domain_priors:requires_active_enforcement(us_sanctions_belarus_2022).

:- end_tests(us_sanctions_belarus_2022_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.65): Sanctions are designed to inflict significant
 *     and direct economic harm, restricting trade, freezing assets, and
 *     denying access to capital markets. The value is high, reflecting a
 *     primary function of coercive extraction.
 *   - Suppression Score (0.80): The U.S. Treasury's OFAC has far-reaching
 *     enforcement power, creating a high barrier to circumvention. The threat
 *     of secondary sanctions suppresses alternatives for Belarus globally.
 *   - Theater Ratio (0.15): The sanctions have tangible, measurable economic
 *     effects and are not merely symbolic. They are actively enforced.
 *
 * INTER-INSTITUTIONAL DYNAMICS & PERSPECTIVAL GAP:
 *   This is a canonical example of an inter-institutional constraint. The
 *   classification hinges entirely on an agent's structural relationship to it.
 *   - For the U.S. Government (Beneficiary, Arbitrage Exit), the sanctions
 *     are a `Rope`. It's a low-cost, high-flexibility tool for coordinating
 *     allied foreign policy and projecting power. The `d` value is very low,
 *     resulting in negative effective extraction (χ < 0).
 *   - For the Belarusian Government (Victim, Trapped Exit), the sanctions are
 *     a `Snare`. They are an external, coercive constraint that extracts
 *     economic value and limits sovereignty, with no viable way to exit. The
 *     `d` value is maximal, resulting in extremely high effective extraction
 *     (χ > 1.0).
 *   The analytical perspective resolves this gap by classifying it as a `Tangled Rope`,
 *   acknowledging both the genuine coordination function and the severe
 *   asymmetric extraction, which are required properties for this classification.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality `d` is derived automatically and correctly by the engine.
 *   The `constraint_beneficiary` declaration (us_government_and_allies)
 *   combined with `exit_options(arbitrage)` pulls `d` towards 0.0 for the US.
 *   The `constraint_victim` declaration (belarusian_state_entities) combined
 *   with `exit_options(trapped)` pushes `d` towards 1.0 for Belarus. This
 *   mechanically produces the vast difference in perceived effective extraction (χ).
 *
 * MANDATROPHY ANALYSIS:
 *   This system avoids two common errors. First, it doesn't naively label
 *   sanctions as a pure `Snare`, which would ignore their function as a tool
 *   of international coordination and policy enforcement. Second, it avoids
 *   the apologetic view of sanctions as a mere `Rope`, which would erase the
 *   immense coercive and extractive costs imposed on the target. The
 *   `Tangled Rope` classification is precise: it is a tool of coordination
 *   *achieved through* asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_us_sanctions_belarus_2022,
    'What is the ratio of geopolitical policy change (coordination success) to collateral economic harm imposed on the civilian population (extraction cost)?',
    'Comparative analysis of Belarusian policy shifts vs. econometric data on non-state sector economic decline and civilian welfare metrics pre/post sanctions.',
    'If the ratio is high (high policy change for low collateral harm), it solidifies the Tangled Rope classification. If the ratio is near zero (no policy change, high collateral harm), the constraint functions more like a pure Snare, and its coordination aspect is closer to theater.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(us_sanctions_belarus_2022, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for this high-extraction constraint (ε=0.65 > 0.46).
% Models the intensification of the sanctions regime over time.

% Theater ratio over time: Stays low as the sanctions remain functional.
narrative_ontology:measurement(usb_tr_t0, us_sanctions_belarus_2022, theater_ratio, 0, 0.10).
narrative_ontology:measurement(usb_tr_t5, us_sanctions_belarus_2022, theater_ratio, 5, 0.12).
narrative_ontology:measurement(usb_tr_t10, us_sanctions_belarus_2022, theater_ratio, 10, 0.15).

% Extraction over time: Starts high and increases as loopholes are closed.
narrative_ontology:measurement(usb_ex_t0, us_sanctions_belarus_2022, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(usb_ex_t5, us_sanctions_belarus_2022, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(usb_ex_t10, us_sanctions_belarus_2022, base_extractiveness, 10, 0.65).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This is a mechanism for enforcing a geopolitical norm.
narrative_ontology:coordination_type(us_sanctions_belarus_2022, enforcement_mechanism).

% Network relationships (structural influence edges)
% The sanctions on Belarus are a direct consequence and component of the
% broader sanctions regime against Russia.
narrative_ontology:affects_constraint(us_sanctions_russia_2022, us_sanctions_belarus_2022).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The automatic derivation
% chain (beneficiary/victim + exit_options -> d) correctly models the
% extreme perspectival gap between the sanctioning and sanctioned institutions.
% The system will correctly derive d~0.05 for the US and d~0.95 for Belarus.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */