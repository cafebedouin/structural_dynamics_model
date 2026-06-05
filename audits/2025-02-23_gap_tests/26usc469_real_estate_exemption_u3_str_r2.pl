% ============================================================================
% CONSTRAINT STORY: 26usc469_real_estate_exemption_u3_str_r2
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_26usc469_real_estate_exemption_u3_str_r2, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: 26usc469_real_estate_exemption_u3_str_r2
 *   human_readable: The Real Estate Professional Exemption (Passive Activity Loss Rules)
 *   domain: economic/legal
 *
 * SUMMARY:
 *   Section 469 of the US tax code limits the ability of taxpayers to deduct
 *   losses from passive activities against ordinary income. The Real Estate
 *   Professional Exemption (§ 469(c)(7)) provides a powerful exception, but
 *   only for individuals who can pass a strict two-part test: (1) more than
 *   half of their personal services during the year are performed in real
 *   property trades or businesses, and (2) they perform more than 750 hours
 *   of services in those businesses. This structure effectively creates a
 *   protected class of taxpayers who can fully utilize real estate tax
 *   benefits, while systematically denying those same benefits to individuals
 *   with demanding, high-income careers outside of real estate.
 *
 * KEY AGENTS:
 *   - High-Income W-2 Investors: Primary target (powerless/trapped) - They have the capital to invest and the income to shelter, but are blocked by the time-based qualification tests.
 *   - Full-Time Real Estate Professionals: Primary beneficiary (moderate/mobile) - The law carves out a specific and valuable benefit for their profession.
 *   - The IRS: Enforcing institution (institutional/constrained) - Administers the rule as written, serving as the gatekeeper for the exemption.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(26usc469_real_estate_exemption_u3_str_r2, 0.68).
domain_priors:suppression_score(26usc469_real_estate_exemption_u3_str_r2, 0.75).
domain_priors:theater_ratio(26usc469_real_estate_exemption_u3_str_r2, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u3_str_r2, extractiveness, 0.68).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u3_str_r2, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u3_str_r2, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(26usc469_real_estate_exemption_u3_str_r2, tangled_rope).
narrative_ontology:human_readable(26usc469_real_estate_exemption_u3_str_r2, "The Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain(26usc469_real_estate_exemption_u3_str_r2, "economic/legal").

domain_priors:requires_active_enforcement(26usc469_real_estate_exemption_u3_str_r2).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(26usc469_real_estate_exemption_u3_str_r2, full_time_real_estate_professionals).
narrative_ontology:constraint_victim(26usc469_real_estate_exemption_u3_str_r2, high_income_w2_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of a high-earning professional with significant real estate investments, the two-part test is an insurmountable barrier. They are trapped; they cannot meet the hours test without abandoning the primary career that generates the income they seek to shelter. The rule extracts value by disallowing legitimate losses.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_str_r2, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For those whose primary business is real estate, the rule is a clear coordination mechanism. It defines their professional status for tax purposes and provides a significant, predictable benefit. It distinguishes them from 'dabblers' and is seen as a fair standard for the industry.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_str_r2, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% The IRS is constrained to enforce the law as written. It sees both the coordination function (defining a class of taxpayer to prevent widespread tax shelter abuse) and the extractive function (auditing and denying deductions to those who fail the strict tests). The rule is functional but creates clear winners and losers.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_str_r2, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The analytical view recognizes the dual nature of the constraint. It serves a legitimate coordinating purpose (curbing passive shelters) but does so via a mechanism that is highly extractive and asymmetrically benefits one group (full-time RE pros) at the expense of another (high-income W-2 investors).
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_str_r2, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(26usc469_real_estate_exemption_u3_str_r2_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_str_r2, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_str_r2, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(26usc469_real_estate_exemption_u3_str_r2, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(26usc469_real_estate_exemption_u3_str_r2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high, representing the significant tax value of deducting paper losses from leveraged real estate against high-bracket W-2 income. Suppression (0.75) is also high because the primary alternative for a W-2 earner to qualify—quitting their job—is prohibitively costly, making the constraint's requirements effectively non-negotiable.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. The beneficiary (full-time pro) sees a Rope: a clear, fair rule that defines their professional status and rewards their focus. The victim (W-2 investor) sees a Snare: an arbitrary, insurmountable trap that denies them tax parity for making the same type of investment. The IRS, as the enforcer, sees the functional Tangled Rope that it is.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality is determined by the 'more than half of personal services' test. This clause explicitly directs the flow of tax benefits away from individuals with substantial non-real-estate careers and towards those for whom real estate is their primary professional activity. The victims are those penalized for having a different primary source of earned income.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a pure Snare would miss its stated (and plausible) coordination function of preventing passive tax shelters. Classifying it as a Rope would ignore the massive, asymmetric extraction it imposes. The Tangled Rope classification correctly captures this duality: a rule with a legitimate policy goal that is implemented in a way that creates a privileged economic class and a disadvantaged one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rep_status_policy_intent,
    'Is the strict two-part test for 'Real Estate Professional' status a necessary guardrail against tax shelters, or a deliberately constructed barrier to benefit a specific professional class?',
    'Analysis of legislative history and lobbying records from the Revenue Reconciliation Act of 1993 to determine the stated vs. actual intent of the exemption's structure.',
    'If it's a necessary guardrail, the constraint leans more towards a high-friction Rope. If it's a deliberate barrier designed by industry lobbyists, it is a clear Snare embedded within a Tangled Rope structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rep_status_policy_intent, empirical, 'Whether the strict REP test is a necessary anti-abuse rule or a form of economic protectionism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(26usc469_real_estate_exemption_u3_str_r2, 1993, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(26us_tr_t1993, 26usc469_real_estate_exemption_u3_str_r2, theater_ratio, 1993, 0.2).
narrative_ontology:measurement(26us_tr_t2008, 26usc469_real_estate_exemption_u3_str_r2, theater_ratio, 2008, 0.2).
narrative_ontology:measurement(26us_tr_t2024, 26usc469_real_estate_exemption_u3_str_r2, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(26us_be_t1993, 26usc469_real_estate_exemption_u3_str_r2, base_extractiveness, 1993, 0.45).
narrative_ontology:measurement(26us_be_t2008, 26usc469_real_estate_exemption_u3_str_r2, base_extractiveness, 2008, 0.6).
narrative_ontology:measurement(26us_be_t2024, 26usc469_real_estate_exemption_u3_str_r2, base_extractiveness, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(26usc469_real_estate_exemption_u3_str_r2, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
