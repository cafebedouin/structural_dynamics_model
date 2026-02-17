% ============================================================================
% CONSTRAINT STORY: student_loan_default_cliff
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_student_loan_default_cliff, []).

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
 *   constraint_id: student_loan_default_cliff
 *   human_readable: Student Loan Default Cliff
 *   domain: economic
 *
 * SUMMARY:
 *   The expiration of student loan forbearance programs creates a "default cliff" where borrowers, particularly those with low incomes, face immediate repayment obligations, risking widespread defaults and financial instability. This story examines the structural forces behind this phenomenon.
 *
 * KEY AGENTS (by structural relationship):
 *   - Low-income borrowers: Primary target (powerless/trapped) — bears extraction
 *   - Student loan servicers: Primary beneficiary (institutional/arbitrage) — benefits from constraint
 *   - Department of Education: Secondary actor (institutional/constrained)
 *   - Analytical observer: Analytical observer — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(student_loan_default_cliff, 0.55).
domain_priors:suppression_score(student_loan_default_cliff, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(student_loan_default_cliff, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(student_loan_default_cliff, extractiveness, 0.55).
narrative_ontology:constraint_metric(student_loan_default_cliff, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(student_loan_default_cliff, theater_ratio, 0.30).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Uncomment and set for mountain constraints.
% Without these, the NL signature defaults to 0.5 and fails certification.
%
% narrative_ontology:constraint_metric(student_loan_default_cliff, accessibility_collapse, [0.85-1.0]).
% narrative_ontology:constraint_metric(student_loan_default_cliff, resistance, [0.0-0.15]).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(student_loan_default_cliff, tangled_rope).
narrative_ontology:human_readable(student_loan_default_cliff, "Student Loan Default Cliff").

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(student_loan_default_cliff).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(student_loan_default_cliff). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% Uncomment for constraints that emerge naturally without human design
% or enforcement. Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.
%
% domain_priors:emerges_naturally(student_loan_default_cliff).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(student_loan_default_cliff, student_loan_servicers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(student_loan_default_cliff, low_income_borrowers).
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
constraint_indexing:constraint_classification(student_loan_default_cliff, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(student_loan_default_cliff, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(student_loan_default_cliff, tangled_rope,
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
% Perspective 4: The Department of Education (institutional, constrained)
% They are institutional but have a constrained exit as they are politically mandated to handle student loans
constraint_indexing:constraint_classification(student_loan_default_cliff, rope,
     context(agent_power(institutional),
             time_horizon(generational),
             exit_options(constrained),
             spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(student_loan_default_cliff_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(student_loan_default_cliff, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(student_loan_default_cliff, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(student_loan_default_cliff, ExtMetricName, E),
    (E =< 0.25 -> true ; E >= 0.46). % Mountain or high-extraction Snare/Tangled.

:- end_tests(student_loan_default_cliff_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Student Loan Default Cliff is scored with a base extractiveness of 0.55, reflecting the significant financial burden placed on borrowers. The suppression score is 0.70, indicating limited alternative options for borrowers facing repayment. The theater ratio is 0.30, suggesting a low degree of performative compliance relative to actual function.
 *
 * PERSPECTIVAL GAP:
 *   Low-income borrowers perceive the situation as a Snare, due to their limited exit options (trapped) and the high extraction they face. In contrast, student loan servicers classify it as a Rope, as they benefit from the repayment stream and have arbitrage opportunities. The Department of Education sees it as a Rope too, hoping for coordination to have most people pay, even though their power to enforce is limited (constrained exit). The analytical observer perceives it as a Tangled Rope, recognizing the coordination function of loan repayment alongside the asymmetric extraction from vulnerable borrowers.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-income borrowers are victims, bearing the cost of loan repayment with limited ability to escape the debt cycle. Student loan servicers are beneficiaries, as they profit from loan servicing and repayment, aligning incentives to maintain the system. The Department of Education tries to manage the system, but their role is constrained as the administrator (trying to be beneficiary and victim at the same time), having limited means to truly change the system.
 *
 * INTER-INSTITUTIONAL DYNAMICS (if applicable):
 *   The Department of Education, as the administrator of the loan system, experiences the constraint differently from student loan servicers. The DoE aims to facilitate repayment while managing default risk, leading to a more constrained perspective compared to the servicers who directly benefit from repayment.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling coordination as pure extraction by recognizing the coordination function of student loans in financing higher education. However, the asymmetric extraction from low-income borrowers and the high suppression score highlight the risks of a system that can easily become a Snare for vulnerable populations. The Tangled Rope classification from an analytical perspective acknowledges both aspects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_student_loan_default_cliff,
    'Will policy changes alleviate the burden on low-income borrowers?',
    'Monitoring legislative and regulatory actions related to student loan forgiveness and repayment options.',
    'If True: Reduced default rates and improved financial stability for borrowers. If False: Continued high default rates and financial hardship for vulnerable populations.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(student_loan_default_cliff, 0, 10).

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
narrative_ontology:measurement(student_loan_default_cliff_tr_t0, student_loan_default_cliff, theater_ratio, 0, 0.20).
narrative_ontology:measurement(student_loan_default_cliff_tr_t5, student_loan_default_cliff, theater_ratio, 5, 0.30).
narrative_ontology:measurement(student_loan_default_cliff_tr_t10, student_loan_default_cliff, theater_ratio, 10, 0.30).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(student_loan_default_cliff_ex_t0, student_loan_default_cliff, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(student_loan_default_cliff_ex_t5, student_loan_default_cliff, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(student_loan_default_cliff_ex_t10, student_loan_default_cliff, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(student_loan_default_cliff, resource_allocation).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(student_loan_default_cliff, [0.0-1.0]).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(student_loan_default_cliff, [other_constraint_id]).

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
% narrative_ontology:affects_constraint(student_loan_default_cliff, [sibling_constraint_id]).

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
% constraint_indexing:directionality_override(student_loan_default_cliff, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */