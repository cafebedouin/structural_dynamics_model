% ============================================================================
% CONSTRAINT STORY: finnish_debt_adjustment
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_finnish_debt_adjustment, []).

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
 *   constraint_id: finnish_debt_adjustment
 *   human_readable: Finnish Private Debt Adjustment System
 *   domain: economic/political
 *
 * SUMMARY:
 *   The Finnish legal system provides a "debt adjustment" program for
 *   over-indebted private individuals. The system imposes a multi-year
 *   (typically 3-5 years, but often longer in practice) payment plan where the
 *   debtor's income is garnished down to a protected minimum. At the end of
 *   the period, remaining debts are forgiven. While framed as a rehabilitative
 *   mechanism, it functions as a highly coercive, long-term extraction
 *   process that also coordinates creditor claims.
 *
 * KEY AGENTS (by structural relationship):
 *   - Over-indebted individuals: Primary target (powerless/trapped) — bear the extraction, living for years with minimal disposable income.
 *   - Creditors (banks, lenders): Primary beneficiary (institutional/arbitrage) — receive a structured, state-enforced repayment stream from otherwise non-performing loans.
 *   - The Finnish State (judicial system): System maintainer/enforcer (institutional/constrained) — provides the legal framework and enforcement, stabilizing the credit market.
 *   - Analytical observer: Analytical observer — sees the dual function of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(finnish_debt_adjustment, 0.68).
domain_priors:suppression_score(finnish_debt_adjustment, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(finnish_debt_adjustment, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(finnish_debt_adjustment, extractiveness, 0.68).
narrative_ontology:constraint_metric(finnish_debt_adjustment, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(finnish_debt_adjustment, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
% Not a mountain constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(finnish_debt_adjustment, tangled_rope).
narrative_ontology:human_readable(finnish_debt_adjustment, "Finnish Private Debt Adjustment System").

% --- Binary flags ---
domain_priors:requires_active_enforcement(finnish_debt_adjustment). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% Not a naturally emerging constraint.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(finnish_debt_adjustment, creditors_and_lenders).
narrative_ontology:constraint_beneficiary(finnish_debt_adjustment, finnish_state).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(finnish_debt_adjustment, over_indebted_individuals).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three) -> MET
%   Snare:        victim required; beneficiary optional -> MET

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

% PERSPECTIVE 1: THE PRIMARY TARGET (Over-Indebted Individual)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% From this viewpoint, the system is a pure Snare. The coordination benefit
% to creditors is irrelevant to the lived experience of extraction.
constraint_indexing:constraint_classification(finnish_debt_adjustment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (Creditor/Lender)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
% From this viewpoint, the system is a Rope that coordinates recovery efforts,
% creating order and predictability where there would be chaos.
constraint_indexing:constraint_classification(finnish_debt_adjustment, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context sees both the coordination and the severe,
% asymmetric extraction. The high base extractiveness, high suppression, and
% presence of both victims and beneficiaries leads to a Tangled Rope classification.
constraint_indexing:constraint_classification(finnish_debt_adjustment, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---
% The state as the system administrator and enforcer. It is a beneficiary
% (maintains systemic stability) but is also constrained by its own laws
% and procedures. Its exit options are limited. This results in a different
% directionality than the purely commercial creditors.
% Engine derives d from: beneficiary membership + constrained exit -> d somewhere between institutional and powerful.
constraint_indexing:constraint_classification(finnish_debt_adjustment, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(finnish_debt_adjustment_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify perspectival gap between target (debtor) and beneficiary (creditor).
    constraint_indexing:constraint_classification(finnish_debt_adjustment, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(finnish_debt_adjustment, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    writeln('OK: Debtor sees Snare, Creditor sees Rope.').

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(finnish_debt_adjustment, tangled_rope, context(agent_power(analytical), _, _, _)),
    writeln('OK: Analytical perspective correctly identifies Tangled Rope.').

test(tangled_rope_gate_validation) :-
    % A constraint is a Tangled Rope if it has a coordination function
    % (beneficiary exists), asymmetric extraction (victim exists), and
    % requires active enforcement.
    narrative_ontology:constraint_beneficiary(finnish_debt_adjustment, _),
    narrative_ontology:constraint_victim(finnish_debt_adjustment, _),
    domain_priors:requires_active_enforcement(finnish_debt_adjustment),
    writeln('OK: All structural requirements for Tangled Rope are met.').

:- end_tests(finnish_debt_adjustment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.68): High. The system is designed to maximize
 *     repayment by stripping the debtor's income down to a legally protected
 *     minimum for several years. This is not a nominal cost; it is a severe,
 *     long-term financial extraction.
 *   - Suppression (0.75): High. The legal framework actively suppresses
 *     alternatives like a faster, cleaner bankruptcy or strategic default.
 *     The alternative to entering the program is often perpetual harassment
 *     from collection agencies and a complete inability to function
 *     economically, making the adjustment program the 'least bad' coercive option.
 *   - Theater (0.15): Low. The system is brutally functional. Its procedures are
 *     about the mechanics of payment and enforcement, not primarily about
 *     performative rehabilitation or social signaling.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the trapped debtor (powerless), the system is a Snare
 *   that consumes their financial life for years. The "coordination" aspect is
 *   invisible and irrelevant; what they experience is pure, coercive extraction.
 *   For the institutional creditor, the system is a valuable Rope. It solves a
 *   collective action problem by preventing a creditor free-for-all, guaranteeing
 *   an orderly, predictable, and state-enforced recovery of funds that would
 *   otherwise be lost.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is clear: the constraint extracts resources *from* the
 *   `over_indebted_individuals` (victim group) and directs them *to* the
 *   `creditors_and_lenders` (beneficiary group). The `finnish_state` is also a
 *   beneficiary as the system's stability serves its interests, but it is also
 *   the enforcer. The engine correctly derives a high 'd' value for victims with
 *   trapped exit, leading to a high effective extraction (χ) and a Snare
 *   classification. It derives a very low 'd' for beneficiaries with arbitrage
 *   exit, leading to a low/negative χ and a Rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a canonical Tangled Rope, and the analysis prevents two key errors.
 *   1. It avoids mislabeling the system as a pure Snare, which would ignore its
 *      genuine and important coordination function for the financial system.
 *   2. It avoids mislabeling it as a pure Rope (as its proponents might), which
 *      would erase the severe, asymmetric extraction imposed on debtors. The
 *      Tangled Rope classification correctly captures this duality of function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_finnish_debt_adjustment,
    'Is the system primarily a rehabilitative tool that occasionally fails, or a system of managed extraction whose rehabilitative claims are secondary?',
    'Longitudinal studies comparing the 10-year post-adjustment financial health, income, and well-being of participants versus a control group who went through an alternative process (e.g., informal settlement or prolonged default).',
    'If primarily rehabilitative, its base extractiveness (ε) might be lower than modeled, as the costs are offset by a genuine benefit. If primarily extraction, the current model holds.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(finnish_debt_adjustment, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. As the system has become more
% established, it's plausible that bureaucratic friction (theater) and the
% efficiency of extraction have slowly increased.
% Base extraction > 0.46 requires this section.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(fda_tr_t0, finnish_debt_adjustment, theater_ratio, 0, 0.10).
narrative_ontology:measurement(fda_tr_t5, finnish_debt_adjustment, theater_ratio, 5, 0.12).
narrative_ontology:measurement(fda_tr_t10, finnish_debt_adjustment, theater_ratio, 10, 0.15).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(fda_ex_t0, finnish_debt_adjustment, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(fda_ex_t5, finnish_debt_adjustment, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(fda_ex_t10, finnish_debt_adjustment, base_extractiveness, 10, 0.68).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This system is a state-backed enforcement mechanism for civil debt.
narrative_ontology:coordination_type(finnish_debt_adjustment, enforcement_mechanism).

% Network relationships (structural influence edges)
% The existence of this structured default mechanism affects the behavior of
% lenders in the primary credit market, potentially encouraging riskier lending.
narrative_ontology:affects_constraint(finnish_debt_adjustment, consumer_credit_availability).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The structural derivation from
% beneficiary/victim declarations and exit options (trapped, arbitrage,
% constrained) accurately captures the power dynamics of the system.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */