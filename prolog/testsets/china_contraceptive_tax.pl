% ============================================================================
% CONSTRAINT STORY: china_contraceptive_tax
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_china_contraceptive_tax, []).

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
 *   constraint_id: china_contraceptive_tax
 *   human_readable: China's Differential Tax on Contraceptives
 *   domain: economic/political
 *
 * SUMMARY:
 *   The Chinese government imposes a 17% value-added tax (VAT) on condoms,
 *   while providing tax exemptions or subsidies for other state-preferred
 *   contraceptive methods like IUDs and sterilization. This policy creates
 *   a financial disincentive for using condoms, which are unique in their dual
 *   function of preventing both pregnancy and sexually transmitted diseases (STDs).
 *
 * KEY AGENTS (by structural relationship):
 *   - condom_users: Primary target (powerless/trapped) — bear the financial extraction and are steered away from their preferred method.
 *   - state_population_planners: Primary beneficiary (institutional/arbitrage) — gain tax revenue and steer the populace towards long-acting, state-managed contraceptive methods.
 *   - public_health_analysts: Analytical observer — see the dual function of coordination (family planning) and asymmetric extraction (penalizing one method).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(china_contraceptive_tax, 0.55).
domain_priors:suppression_score(china_contraceptive_tax, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(china_contraceptive_tax, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(china_contraceptive_tax, extractiveness, 0.55).
narrative_ontology:constraint_metric(china_contraceptive_tax, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(china_contraceptive_tax, theater_ratio, 0.30).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(china_contraceptive_tax, tangled_rope).

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(china_contraceptive_tax).
domain_priors:requires_active_enforcement(china_contraceptive_tax). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% N/A for this constraint.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(china_contraceptive_tax, state_population_planners).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(china_contraceptive_tax, condom_users).

% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three met)
%   Snare:        victim required; beneficiary optional (met)

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
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% The policy acts as a financial trap, penalizing a specific choice.
constraint_indexing:constraint_classification(china_contraceptive_tax, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
% The state sees this as a pure policy instrument for coordination and revenue.
constraint_indexing:constraint_classification(china_contraceptive_tax, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context. Sees both the coordination function and the
% asymmetric extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(china_contraceptive_tax, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(china_contraceptive_tax_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(china_contraceptive_tax, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(china_contraceptive_tax, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == snare,
    TypeBeneficiary == rope,
    TypeTarget \= TypeBeneficiary.

test(tangled_rope_analytical_classification) :-
    constraint_indexing:constraint_classification(china_contraceptive_tax, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    narrative_ontology:constraint_beneficiary(china_contraceptive_tax, _),
    narrative_ontology:constraint_victim(china_contraceptive_tax, _),
    domain_priors:requires_active_enforcement(china_contraceptive_tax).

:- end_tests(china_contraceptive_tax_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): The 17% VAT is a direct, significant financial
 *     extraction from a specific group (condom users). The value reflects not just
 *     the tax rate but its coercive effect in a domain of personal health and autonomy.
 *   - Suppression Score (0.70): The policy actively suppresses a specific technology
 *     (condoms) that has a unique dual benefit (pregnancy and STD prevention).
 *     Alternatives like IUDs or sterilization are not direct substitutes for STD
 *     prevention, making the suppression of condom use a high-cost intervention from
 *     a public health standpoint.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. The state (`institutional` perspective) views the tax as a
 *   standard fiscal tool and a population management lever, a form of societal
 *   coordination. It classifies as a Rope because, from its index, the costs are
 *   externalized and the benefits (revenue, control) are internalized.
 *   For the individual user (`powerless` perspective), the tax is a direct penalty
 *   on a health choice, forcing them to pay more or switch to a less-suitable method.
 *   This feels coercive and extractive, a classic Snare.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is unambiguous. The flow of value and control is from the
 *   individual citizen to the state.
 *   - Beneficiary: `state_population_planners` gain revenue and policy leverage.
 *   - Victim: `condom_users` pay the tax and lose autonomy over their health choices.
 *   The engine correctly derives a high directionality `d` for victims (leading to
 *   Snare) and a low `d` for the beneficiary (leading to Rope). The victims are
 *   a diffuse, unorganized group, making coalition formation difficult and reinforcing
 *   their `powerless` classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a canonical example of a Tangled Rope that could be misidentified. A purely
 *   economic view might see it as just another tax (Rope). A purely rights-based view
 *   might see it as pure coercion (Snare). The Tangled Rope classification correctly
 *   identifies the dual nature of the constraint: it leverages a legitimate state
 *   function (taxation for family planning coordination) to execute an asymmetric,
*    extractive penalty on a specific subgroup, thereby achieving a secondary, unstated policy goal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_china_contraceptive_tax,
    'Is the primary intent of the tax policy to (a) raise revenue, (b) steer contraceptive choice towards long-term methods, or (c) an artifact of a non-optimized tax code?',
    'Internal government policy documents or statements from officials detailing the rationale for the tax differential.',
    'If (a) or (c), the extraction is less targeted, potentially lowering ε. If (b), the high ε and suppression scores are strongly validated as intentional coercion.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(china_contraceptive_tax, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. Required as base_extractiveness > 0.46.
% This models a policy that has become more extractive over its lifecycle.

% Theater ratio over time (slight increase in performative justification):
narrative_ontology:measurement(cct_tr_t0, china_contraceptive_tax, theater_ratio, 0, 0.20).
narrative_ontology:measurement(cct_tr_t5, china_contraceptive_tax, theater_ratio, 5, 0.25).
narrative_ontology:measurement(cct_tr_t10, china_contraceptive_tax, theater_ratio, 10, 0.30).

% Extraction over time (tax policy becomes more entrenched and effective):
narrative_ontology:measurement(cct_ex_t0, china_contraceptive_tax, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cct_ex_t5, china_contraceptive_tax, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(cct_ex_t10, china_contraceptive_tax, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This policy functions by allocating financial costs and benefits.
narrative_ontology:coordination_type(china_contraceptive_tax, resource_allocation).

% Network relationships (structural influence edges)
% The tax policy directly impacts public health outcomes for STDs.
narrative_ontology:affects_constraint(china_contraceptive_tax, china_std_prevention_policy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation
% from beneficiary/victim declarations and exit options accurately reflects
% the power dynamics.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */