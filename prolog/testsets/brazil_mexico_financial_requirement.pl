% ============================================================================
% CONSTRAINT STORY: brazil_mexico_financial_requirement
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_brazil_mexico_financial_requirement, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: brazil_mexico_financial_requirement
 *   human_readable: Mexican Financial Proof Requirement for Brazilian Travelers
 *   domain: geopolitical
 *
 * SUMMARY:
 *   Mexico, under pressure from the United States to curb irregular migration,
 *   has imposed a rule requiring Brazilian travelers to prove they possess
 *   at least R$10,000 (~US$1,900) to cover their expenses. This policy acts
 *   as a significant barrier to legitimate tourists, students, and business
 *   travelers, while its effectiveness against organized human smuggling is
 *   debatable. The constraint functions as a geopolitical tool for migration
 *   management, creating a stark perspectival gap between the state actors
 *   who benefit and the individuals who are blocked by it.
 *
 * KEY AGENTS (by structural relationship):
 *   - Legitimate Brazilian travelers: Primary target (powerless/trapped) — bears the opportunity cost of cancelled travel.
 *   - US Immigration Authorities: Primary beneficiary (institutional/arbitrage) — achieves migration control goals by externalizing enforcement to Mexico.
 *   - Mexican Immigration Authorities: Secondary beneficiary (institutional/constrained) — implements the policy under US pressure, gaining geopolitical leverage but also bearing enforcement and economic costs.
 *   - Human Smuggling Networks: Tertiary beneficiary (organized/arbitrage) — benefit as legitimate travel becomes harder, increasing demand for illicit services.
 *   - Mexican Tourism Sector: Secondary victim (organized/constrained) - suffers economic losses from reduced tourism.
 *   - Analytical Observer: Sees the full structure as a Tangled Rope.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(brazil_mexico_financial_requirement, 0.75).
domain_priors:suppression_score(brazil_mexico_financial_requirement, 0.80).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(brazil_mexico_financial_requirement, 0.40).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(brazil_mexico_financial_requirement, extractiveness, 0.75).
narrative_ontology:constraint_metric(brazil_mexico_financial_requirement, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(brazil_mexico_financial_requirement, theater_ratio, 0.40).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(brazil_mexico_financial_requirement, tangled_rope).
narrative_ontology:human_readable(brazil_mexico_financial_requirement, "Mexican Financial Proof Requirement for Brazilian Travelers").

% --- Binary flags ---
domain_priors:requires_active_enforcement(brazil_mexico_financial_requirement). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(brazil_mexico_financial_requirement, us_immigration_authorities).
narrative_ontology:constraint_beneficiary(brazil_mexico_financial_requirement, mexican_immigration_authorities).
narrative_ontology:constraint_beneficiary(brazil_mexico_financial_requirement, human_smuggling_networks).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(brazil_mexico_financial_requirement, legitimate_brazilian_travelers).
narrative_ontology:constraint_victim(brazil_mexico_financial_requirement, mexican_tourism_sector).

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
% A legitimate Brazilian traveler without access to the required funds.
% Their travel is blocked, imposing a 100% opportunity cost.
% Engine derives: victim + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ.
constraint_indexing:constraint_classification(brazil_mexico_financial_requirement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% US immigration authorities who see this as a low-cost tool to achieve
% migration control objectives by outsourcing enforcement.
% Engine derives: beneficiary + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ.
constraint_indexing:constraint_classification(brazil_mexico_financial_requirement, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context reveals the dual nature: a coordination mechanism
% for states that functions as asymmetric extraction against individuals.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15. χ remains high. The presence of
% both a coordination function (beneficiary) and asymmetric extraction (victim)
% with active enforcement qualifies it as a Tangled Rope.
constraint_indexing:constraint_classification(brazil_mexico_financial_requirement, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---
% The constraint operates between the US and Mexico with different dynamics.

% Perspective 4A: The Mexican State (Constrained Beneficiary) (ROPE)
% Mexico implements the policy and is a beneficiary, but does so under
% external pressure, making its exit options 'constrained'.
% Engine derives: beneficiary + constrained exit -> d ≈ 0.25 -> f(d) ≈ 0.14
% This results in a positive but low χ, still classifying as a Rope, but a
% more costly one than the US experiences.
constraint_indexing:constraint_classification(brazil_mexico_financial_requirement, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(brazil_mexico_financial_requirement_tests).

test(perspectival_gap_target_vs_beneficiary) :-
    constraint_indexing:constraint_classification(brazil_mexico_financial_requirement, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(brazil_mexico_financial_requirement, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    format('Perspectival gap confirmed: powerless sees Snare, institutional (arbitrage) sees Rope.').

test(tangled_rope_conditions_met) :-
    narrative_ontology:constraint_beneficiary(brazil_mexico_financial_requirement, _),
    narrative_ontology:constraint_victim(brazil_mexico_financial_requirement, _),
    domain_priors:requires_active_enforcement(brazil_mexico_financial_requirement),
    format('Tangled Rope structural pre-conditions (beneficiary, victim, enforcement) are met.').

test(high_extraction_and_suppression) :-
    domain_priors:base_extractiveness(brazil_mexico_financial_requirement, E), E >= 0.46,
    domain_priors:suppression_score(brazil_mexico_financial_requirement, S), S >= 0.60,
    format('Metrics E=~w, S=~w meet thresholds for high-extraction/suppression types.', [E, S]).

:- end_tests(brazil_mexico_financial_requirement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.75): High. This represents the severe opportunity
 *     cost imposed on individuals whose legitimate travel plans are completely
 *     blocked. For them, the cost is total.
 *   - Suppression Score (0.80): High. The rule effectively eliminates the
 *     primary, affordable, and legitimate method of travel to Mexico for a
 *     large class of Brazilian citizens, leaving only more expensive or
 *     illicit alternatives.
 *   - Theater Ratio (0.40): Moderate. The policy is functional in that it
 *     creates a real barrier. However, its questionable effectiveness against
 *     organized smuggling and its high visibility as a geopolitical signal to
 *     the US give it a significant theatrical component.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For a Brazilian traveler, the rule is a pure, coercive
 *   barrier (Snare) that extracts their opportunity to travel. For the US
 *   government, it is an efficient coordination mechanism (Rope) for managing
 *   migration flows at a distance. The analytical view recognizes both
 *   functions co-exist, classifying it as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries are the state immigration authorities (US, Mexico) who
 *   gain a tool for population control, and ironically, the human smuggling
 *   networks whose services become more attractive. The victims are the
 *   legitimate travelers who are directly targeted, and the Mexican tourism
 *   sector which suffers collateral economic damage. The beneficiary/victim
 *   declarations directly model this structural conflict.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The model captures the asymmetry between the US and Mexico. The US, with
 *   'arbitrage' exit, is a pure beneficiary. Mexico, with 'constrained' exit,
 *   is also a beneficiary but acts under duress. The directionality engine
 *   correctly assigns Mexico a higher directionality score (d), reflecting the
 *   political cost and reduced autonomy, making the 'Rope' less beneficial for
 *   them than for the US.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY]
 *   This is a prime example of a Tangled Rope preventing mandatrophy. A naive
 *   analysis might label the policy a "border coordination" tool (Rope) or a
 *   "discriminatory barrier" (Snare). The Tangled Rope classification correctly
 *   insists that it is structurally BOTH: a genuine (if coerced) coordination
 *   mechanism for states that simultaneously functions as an asymmetric
 *   extractive system against a targeted group of non-state actors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_brazil_mexico_financial_requirement,
    'Is the financial requirement an effective filter against irregular migration, or is it primarily a theatrical barrier that disproportionately harms legitimate travel and tourism?',
    'Analysis of migration data from Brazil through Mexico pre/post-policy implementation, cross-referenced with data on Brazilian tourism decline in Mexico.',
    'If effective, it strengthens the argument for a genuine (though coercive) coordination function. If ineffective, it increases the theater_ratio and pushes the analytical classification closer to a pure Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(brazil_mexico_financial_requirement, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint is part of an escalating series of migration controls.
% The data models a progression from softer visa rules to this hard
% financial requirement, demonstrating "extraction_accumulation".

% Theater ratio over time:
narrative_ontology:measurement(bmfr_tr_t0, brazil_mexico_financial_requirement, theater_ratio, 0, 0.20).
narrative_ontology:measurement(bmfr_tr_t5, brazil_mexico_financial_requirement, theater_ratio, 5, 0.30).
narrative_ontology:measurement(bmfr_tr_t10, brazil_mexico_financial_requirement, theater_ratio, 10, 0.40).

% Extraction over time:
narrative_ontology:measurement(bmfr_ex_t0, brazil_mexico_financial_requirement, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(bmfr_ex_t5, brazil_mexico_financial_requirement, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(bmfr_ex_t10, brazil_mexico_financial_requirement, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: This constraint is a mechanism for enforcing state policy.
narrative_ontology:coordination_type(brazil_mexico_financial_requirement, enforcement_mechanism).

% Network relationships: This policy is not isolated; it's a downstream
% effect of broader US border control strategies.
narrative_ontology:affects_constraint(us_southern_border_policy, brazil_mexico_financial_requirement).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this story. The structural derivation chain,
% using beneficiary/victim declarations combined with the different exit_options
% for the US ('arbitrage') and Mexico ('constrained'), is sufficient to
% accurately model the asymmetric institutional dynamics.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */