% ============================================================================
% CONSTRAINT STORY: debt_trap_microfinance
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-30
% ============================================================================

:- module(constraint_debt_trap_microfinance, []).

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
 *   constraint_id: debt_trap_microfinance
 *   human_readable: Microfinance Debt Trap
 *   domain: economic
 *
 * SUMMARY:
 *   Microfinance, intended to alleviate poverty, can create debt traps
 *   for vulnerable borrowers. High interest rates, short repayment periods,
 *   and aggressive collection practices can lead to cycles of debt.
 *
 * KEY AGENTS (by structural relationship):
 *   - Low-income borrowers: Primary target (powerless/trapped) — bears extraction
 *   - Microfinance Institutions (MFIs): Primary beneficiary (institutional/arbitrage) — benefits from constraint
 *   - Regulators: Potential secondary actor (moderate/constrained)
 *   - Analytical observer: Sees full structure (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(debt_trap_microfinance, 0.55).
domain_priors:suppression_score(debt_trap_microfinance, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(debt_trap_microfinance, 0.30).       % Low theater; the system is functionally extractive, not performative.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(debt_trap_microfinance, extractiveness, 0.55).
narrative_ontology:constraint_metric(debt_trap_microfinance, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(debt_trap_microfinance, theater_ratio, 0.30).

% --- NL Profile Metrics (required for mountain constraints) ---
% Not applicable for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(debt_trap_microfinance, snare).
narrative_ontology:human_readable(debt_trap_microfinance, "Microfinance Debt Trap").

% --- Binary flags ---
domain_priors:requires_active_enforcement(debt_trap_microfinance). % Required for Tangled Rope, applicable to Snare.

% --- Emergence flag (required for mountain constraints) ---
% Not applicable for this constraint.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(debt_trap_microfinance, microfinance_institutions).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(debt_trap_microfinance, low_income_borrowers).
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
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% Scope is national, as this is a systemic issue, not isolated.
% χ = 0.55 * 1.42 * 1.0 = 0.781 (>= 0.66, Snare)
constraint_indexing:constraint_classification(debt_trap_microfinance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(debt_trap_microfinance, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% Default analytical context. Engine derives d ≈ 0.72 → f(d) ≈ 1.15.
% χ = 0.55 * 1.15 * 1.2 = 0.759 (>= 0.66, Snare)
constraint_indexing:constraint_classification(debt_trap_microfinance, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE REGULATOR (TANGLED ROPE)
% A partially captured regulator sees both the coordination function and the
% asymmetric extraction. The directionality override reflects this ambiguity.
% d=0.55 -> f(d)≈0.78. χ = 0.55 * 0.78 * 1.0 = 0.429 (in Tangled Rope range [0.40, 0.90])
constraint_indexing:constraint_classification(debt_trap_microfinance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(debt_trap_microfinance_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(debt_trap_microfinance, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(debt_trap_microfinance, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    % Verify that the base extractiveness is in a valid range for a high-extraction constraint.
    narrative_ontology:constraint_metric(debt_trap_microfinance, extractiveness, E),
    E >= 0.46.

test(regulator_is_tangled_rope) :-
    % Verify the regulator's specific classification.
    constraint_indexing:constraint_classification(debt_trap_microfinance, tangled_rope, context(agent_power(moderate), _, _, _)).

:- end_tests(debt_trap_microfinance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The microfinance debt trap is characterized by high base extractiveness (0.55)
 *   due to exorbitant interest rates and fees, coupled with a high suppression
 *   score (0.70) reflecting the lack of viable alternative financial services
 *   for the target population. The theater ratio is low (0.30) because the
 *   system's primary function is financial extraction, not performative poverty
 *   alleviation.
 *
 * PERSPECTIVAL GAP:
 *   Low-income borrowers, with no exit options, experience the system as a Snare.
 *   The high effective extraction (χ ≈ 0.78) traps them in debt cycles.
 *   Microfinance Institutions (MFIs), however, view it as a Rope. For them, it is a
 *   coordination mechanism for capital allocation that generates profit, with
 *   negative effective extraction (χ < 0). This gap is the core of the conflict.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `microfinance_institutions`. They profit from the high-interest loans.
 *   - Victim: `low_income_borrowers`. They bear the costs of the debt.
 *   These declarations drive the directionality `d`, which correctly assigns a high
 *   `d` value to borrowers and a low `d` value to MFIs.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The regulator perspective is modeled as a Tangled Rope. They are tasked with
 *   enabling financial inclusion (a coordination function) but must also contend
 *   with the system's extractive outcomes. A directionality override to d=0.55
 *   models a regulator who is structurally caught in the middle—neither a full
 *   beneficiary nor a full victim—and thus perceives the hybrid nature of the
 *   constraint. This value produces a χ that correctly falls in the Tangled Rope range.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the system as a Snare from the
 *   analytical and victim perspectives, preventing it from being mislabeled as
 *   a purely beneficial coordination mechanism (Rope). The explicit victim
 *   declaration and high suppression score are key to this distinction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_microfinance,
    'To what extent are exploitative outcomes an intentional feature of the MFI business model versus an unintended consequence of market dynamics?',
    'Internal MFI documents, whistleblower testimony, and comparative analysis of non-profit vs. for-profit MFI borrower outcomes.',
    'If intentional, suggests fraud and requires punitive regulation. If unintentional, suggests market failure requiring structural reform.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(debt_trap_microfinance, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
% Required for high-extraction constraints (base_extractiveness > 0.46).
%
% Theater ratio over time (starts higher due to initial altruistic framing, then drops as profit motive dominates):
narrative_ontology:measurement(debt_trap_microfinance_tr_t0, debt_trap_microfinance, theater_ratio, 0, 0.40).
narrative_ontology:measurement(debt_trap_microfinance_tr_t5, debt_trap_microfinance, theater_ratio, 5, 0.35).
narrative_ontology:measurement(debt_trap_microfinance_tr_t10, debt_trap_microfinance, theater_ratio, 10, 0.30).

% Extraction over time (increases as models are optimized for profit):
narrative_ontology:measurement(debt_trap_microfinance_ex_t0, debt_trap_microfinance, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(debt_trap_microfinance_ex_t5, debt_trap_microfinance, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(debt_trap_microfinance_ex_t10, debt_trap_microfinance, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(debt_trap_microfinance, resource_allocation).

% Network relationships (structural influence edges)
% This constraint is often coupled with weak consumer protection laws.
narrative_ontology:affects_constraint(debt_trap_microfinance, weak_consumer_protection_framework).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% An override is used for the regulator. The default derivation for a 'moderate'
% agent (d=0.65) would classify this as a Snare. However, a regulator is
% structurally different; they are influenced by beneficiaries (capture) but
% are responsible for the entire system, including victims. An override to
% d=0.55 places them exactly in the middle of the perspectival gap, correctly
% yielding a Tangled Rope classification that reflects this dual pressure.
constraint_indexing:directionality_override(debt_trap_microfinance, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */