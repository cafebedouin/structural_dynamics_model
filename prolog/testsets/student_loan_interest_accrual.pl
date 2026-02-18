% ============================================================================
% CONSTRAINT STORY: student_loan_interest_accrual
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_student_loan_interest_accrual, []).

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
 *   constraint_id: student_loan_interest_accrual
 *   human_readable: Student Loan Interest Accrual
 *   domain: economic
 *
 * SUMMARY:
 *   Student loan interest accrues on outstanding balances, even during periods
 *   of deferment or forbearance. This increases the total amount owed and can
 *   disproportionately impact low-income borrowers. The accrual mechanism is often
 *   obscure and difficult for borrowers to understand.
 *
 * KEY AGENTS (by structural relationship):
 *   - Low-income borrowers: Primary target (powerless/trapped) — bears extraction
 *   - Loan servicers: Primary beneficiary (institutional/arbitrage) — benefits from interest accrual
 *   - Department of Education: Secondary actor (institutional/constrained) - Oversees the loan program but is also subject to political pressures.
 *   - Analytical observer: Analytical observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(student_loan_interest_accrual, 0.55).
domain_priors:suppression_score(student_loan_interest_accrual, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(student_loan_interest_accrual, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(student_loan_interest_accrual, extractiveness, 0.55).
narrative_ontology:constraint_metric(student_loan_interest_accrual, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(student_loan_interest_accrual, theater_ratio, 0.30).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Uncomment and set for mountain constraints.
% Without these, the NL signature defaults to 0.5 and fails certification.
%
% narrative_ontology:constraint_metric(student_loan_interest_accrual, accessibility_collapse, [0.85-1.0]).
% narrative_ontology:constraint_metric(student_loan_interest_accrual, resistance, [0.0-0.15]).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(student_loan_interest_accrual, tangled_rope).
narrative_ontology:human_readable(student_loan_interest_accrual, "Student Loan Interest Accrual").
narrative_ontology:topic_domain(student_loan_interest_accrual, "economic").

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(student_loan_interest_accrual).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(student_loan_interest_accrual). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% Uncomment for constraints that emerge naturally without human design
% or enforcement. Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.
%
% domain_priors:emerges_naturally(student_loan_interest_accrual).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(student_loan_interest_accrual, loan_servicers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(student_loan_interest_accrual, low_income_borrowers).
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
constraint_indexing:constraint_classification(student_loan_interest_accrual, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(student_loan_interest_accrual, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(student_loan_interest_accrual, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES (declare when applicable) ---
% When a constraint operates between institutional actors with different
% structural relationships, declare separate perspectives for each.
% The engine differentiates via directionality: different exit_options
% produce different d values even for the same power atom.

% Perspective 4A: The Department of Education (institutional, constrained exit)
constraint_indexing:constraint_classification(student_loan_interest_accrual, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 4B: The Loan Servicers (institutional, arbitrage exit)
% This is identical to Perspective 2, included for clarity in the
% inter-institutional dynamic.
constraint_indexing:constraint_classification(student_loan_interest_accrual, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(student_loan_interest_accrual_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(student_loan_interest_accrual, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(student_loan_interest_accrual, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(student_loan_interest_accrual, ExtMetricName, E),
    (E =< 0.25 -> true ; E >= 0.46). % Mountain or high-extraction Snare/Tangled.

:- end_tests(student_loan_interest_accrual_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The accrual of interest on student loans, particularly during deferment
 *   or forbearance, presents a complex situation. The base extractiveness is
 *   set at 0.55, reflecting the significant financial burden it places on
 *   borrowers. The suppression score is 0.70, as borrowers have limited
 *   alternatives to avoid this interest accrual. The theater ratio is low at
 *   0.30, as the system has a real, functional impact beyond performative aspects.
 *
 * PERSPECTIVAL GAP:
 *   Low-income borrowers perceive this as a Snare, as they are often trapped
 *   with accruing debt and limited exit options. Loan servicers, on the other
 *   hand, see it as a Rope, a mechanism for predictable revenue generation.
 *
 * DIRECTIONALITY LOGIC:
 *   Loan servicers benefit from the accrual of interest, as it increases
 *   their revenue. Low-income borrowers bear the cost, as it increases their
 *   debt burden and can lead to long-term financial hardship. The Department
 *   of Education experiences a mixed effect, as they are responsible for
 *   overseeing the program and ensuring its viability, but also face political
 *   pressure to alleviate borrower debt.
 *
 * INTER-INSTITUTIONAL DYNAMICS (if applicable):
 *   The Department of Education and the Loan Servicers are institutional actors.
 *   While both benefit from a functional loan system, their exit options differ.
 *   Loan servicers operate with an arbitrage exit: if the system becomes
 *   unfavorable, they can seek other investment opportunities. The Department
 *   of Education's exit is constrained, as it has a mandate to maintain the
 *   student loan program.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope prevents mislabeling it as pure
 *   extraction. There is a coordination function, as the loan system
 *   facilitates access to higher education. However, the asymmetric extraction
 *   through interest accrual, particularly on vulnerable borrowers,
 *   introduces the "tangled" element.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_student_loan_interest_accrual,
    'To what extent does interest accrual actually improve loan system viability vs. primarily extracting wealth from borrowers?',
    'Empirical analysis of loan repayment rates and servicer profits under different interest accrual policies.',
    'If accrual is essential for viability: Rope. If accrual primarily extracts: Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(student_loan_interest_accrual, 0, 10).

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
narrative_ontology:measurement(student_loan_interest_accrual_tr_t0, student_loan_interest_accrual, theater_ratio, 0, 0.20).
narrative_ontology:measurement(student_loan_interest_accrual_tr_t5, student_loan_interest_accrual, theater_ratio, 5, 0.30).
narrative_ontology:measurement(student_loan_interest_accrual_tr_t10, student_loan_interest_accrual, theater_ratio, 10, 0.30).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(student_loan_interest_accrual_ex_t0, student_loan_interest_accrual, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(student_loan_interest_accrual_ex_t5, student_loan_interest_accrual, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(student_loan_interest_accrual_ex_t10, student_loan_interest_accrual, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(student_loan_interest_accrual, resource_allocation).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(student_loan_interest_accrual, [0.0-1.0]).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(student_loan_interest_accrual, [other_constraint_id]).

% --- Network Decomposition (Constraint Families) ---
% When a natural-language label covers multiple constraints with different ε
% values, each gets its own file. Link family members with affects_constraint:
%
% DUAL FORMULATION NOTE:
% This constraint is one of [N] stories decomposed from [colloquial label].
% Decomposed because ε differs across observables (ε-invariance principle).
% Related stories:
%   - [sibling_constraint_1] (ε=[value], [Type])
%   - [sibling_constraint_2] (ε=[value], [Type])
%
% narrative_ontology:affects_constraint(student_loan_interest_accrual, [sibling_constraint_id]).

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
% constraint_indexing:directionality_override(student_loan_interest_accrual, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */