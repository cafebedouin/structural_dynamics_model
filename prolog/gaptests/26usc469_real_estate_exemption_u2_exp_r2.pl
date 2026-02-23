% ============================================================================
% CONSTRAINT STORY: 26usc469_real_estate_exemption_u2_exp_r2
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_26usc469_real_estate_exemption_u2_exp_r2, []).

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
 *   constraint_id: 26usc469_real_estate_exemption_u2_exp_r2
 *   human_readable: The Real Estate Professional Exemption (Passive Activity Loss Rules)
 *   domain: economic/legal
 *
 * SUMMARY:
 *   Section 469 of the US tax code generally prevents taxpayers from
 *   deducting passive activity losses against active income. The (c)(7)
 *   exemption carves out a special exception for 'real estate professionals,'
 *   but defines this status with a strict, two-part test (750 hours of
 *   activity AND more than half of personal services in real property
 *   trades). This creates a significant, often insurmountable, barrier for
 *   individuals with high-income W-2 jobs, effectively bifurcating taxpayers
 *   into those who can and cannot access these powerful deductions.
 *
 * KEY AGENTS:
 *   - Hybrid W-2 Investors: Primary target (powerless/trapped) — bears extraction via disallowed losses.
 *   - Full-Time Real Estate Professionals: Primary beneficiary (moderate/mobile) — benefits from the clear qualification path.
 *   - The IRS: Enforcing institution (institutional/constrained) — administers the rule.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(26usc469_real_estate_exemption_u2_exp_r2, 0.55).
domain_priors:suppression_score(26usc469_real_estate_exemption_u2_exp_r2, 0.65).
domain_priors:theater_ratio(26usc469_real_estate_exemption_u2_exp_r2, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_exp_r2, extractiveness, 0.55).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_exp_r2, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_exp_r2, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(26usc469_real_estate_exemption_u2_exp_r2, tangled_rope).
narrative_ontology:human_readable(26usc469_real_estate_exemption_u2_exp_r2, "The Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain(26usc469_real_estate_exemption_u2_exp_r2, "economic/legal").

domain_priors:requires_active_enforcement(26usc469_real_estate_exemption_u2_exp_r2).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(26usc469_real_estate_exemption_u2_exp_r2, full_time_real_estate_professionals).
narrative_ontology:constraint_victim(26usc469_real_estate_exemption_u2_exp_r2, hybrid_w2_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For the high-income earner whose primary job prevents them from meeting the time tests, this rule is a pure barrier. It disallows legitimate losses, creating a direct extraction (higher tax liability) with no perceived coordination benefit. They are trapped by their career choice.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r2, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For those whose primary business is real estate, the rule is a clear, bright-line test. It coordinates the definition of a 'professional,' providing a stable framework to build a tax-advantaged business model. It feels like a fair rule of the road.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r2, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% The IRS is constrained to enforce the law as written. It sees the rule's coordination function (preventing widespread passive loss tax shelters) but is also aware of the asymmetric outcomes and the enforcement overhead required to audit the strict time-based qualifications.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r2, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The analyst sees the full structure: a rule with a legitimate coordination goal (distinguishing active from passive participation) that is implemented via a mechanism that creates a sharp divide, extracting significant value from one class of taxpayers to the benefit of another.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r2, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(26usc469_real_estate_exemption_u2_exp_r2_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r2, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r2, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(26usc469_real_estate_exemption_u2_exp_r2, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(26usc469_real_estate_exemption_u2_exp_r2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.55) reflects the substantial tax savings transferred to qualifying professionals, paid for by the disallowed losses of non-qualifying investors. Suppression (0.65) is high because the rule is codified in federal law, with the IRS as the enforcer; the only alternative is to quit one's primary profession, a prohibitively costly exit.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the beneficiary, the rule is a clear 'Rope' that defines the rules of their profession. For the victim, it's a 'Snare' that arbitrarily denies deductions available to others based on a time-commitment test they cannot possibly meet. The analytical view recognizes both functions, classifying it as a 'Tangled Rope'.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint directs value from investors whose primary income is non-real estate W-2 wages towards individuals who can dedicate the majority of their working hours to real estate activities. The former are the victims, as their inability to meet the time tests results in a higher tax burden. The latter are the beneficiaries, as they gain exclusive access to a valuable tax deduction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a classic Tangled Rope. It avoids a misclassification as a pure Snare by acknowledging its genuine coordination function: preventing a class of what Congress deemed abusive tax shelters. However, it avoids a misclassification as a simple Rope by quantifying the high, asymmetric extraction that results from its specific implementation, which favors one economic actor over another.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pal_rule_intent_vs_effect,
    'Was the high barrier (750 hours + >50% test) intended as a precise filter for 'professionals' or as a deliberate barrier to protect a specific lobby's tax advantages?',
    'Analysis of legislative history, lobbying records from the Revenue Reconciliation Act of 1993, and economic modeling of the rule's impact on different taxpayer classes.',
    'If a precise filter: Tangled Rope (coordination is primary). If a deliberate barrier: Snare (extraction is primary).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pal_rule_intent_vs_effect, empirical, 'Whether the strict tests for 'real estate professional' are a functional filter or a form of protectionism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(26usc469_real_estate_exemption_u2_exp_r2, 1993, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(26us_tr_t1993, 26usc469_real_estate_exemption_u2_exp_r2, theater_ratio, 1993, 0.1).
narrative_ontology:measurement(26us_tr_t2008, 26usc469_real_estate_exemption_u2_exp_r2, theater_ratio, 2008, 0.12).
narrative_ontology:measurement(26us_tr_t2024, 26usc469_real_estate_exemption_u2_exp_r2, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(26us_be_t1993, 26usc469_real_estate_exemption_u2_exp_r2, base_extractiveness, 1993, 0.4).
narrative_ontology:measurement(26us_be_t2008, 26usc469_real_estate_exemption_u2_exp_r2, base_extractiveness, 2008, 0.5).
narrative_ontology:measurement(26us_be_t2024, 26usc469_real_estate_exemption_u2_exp_r2, base_extractiveness, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(26usc469_real_estate_exemption_u2_exp_r2, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
