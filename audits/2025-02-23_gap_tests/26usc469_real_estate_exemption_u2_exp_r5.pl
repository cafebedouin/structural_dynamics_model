% ============================================================================
% CONSTRAINT STORY: 26usc469_real_estate_exemption_u2_exp_r5
% ============================================================================
% Version: 1.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_26usc469_real_estate_exemption_u2_exp_r5, []).

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
 *   constraint_id: 26usc469_real_estate_exemption_u2_exp_r5
 *   human_readable: The Real Estate Professional Exemption (Passive Activity Loss Rules)
 *   domain: economic/legal
 *
 * SUMMARY:
 *   Section 469 of the US tax code, introduced in 1986, limits the ability of
 *   taxpayers to deduct passive activity losses against other income. The
 *   Real Estate Professional (REP) status provides a significant exemption.
 *   However, qualifying requires meeting a strict two-part test: (1)
 *   performing more than 750 hours of service in real property trades or
 *   businesses, AND (2) performing more than half of one's total personal
 *   services in those trades. This second prong creates a nearly
 *   insurmountable barrier for individuals with demanding, high-income W-2
 *   careers, bifurcating taxpayers into a beneficiary class that can deduct
 *   losses and a victim class that cannot.
 *
 * KEY AGENTS:
 *   - Hybrid W-2 Investors: Primary target (powerless/trapped) — High-income professionals who invest in real estate but cannot meet the 'more than half' test.
 *   - Full-Time Real Estate Professionals: Primary beneficiary (moderate/mobile) — Individuals whose primary work is in real estate and can easily meet the test.
 *   - The IRS: Enforcing institution (institutional/constrained) — Administers the tax code as written by Congress.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(26usc469_real_estate_exemption_u2_exp_r5, 0.55).
domain_priors:suppression_score(26usc469_real_estate_exemption_u2_exp_r5, 0.62).
domain_priors:theater_ratio(26usc469_real_estate_exemption_u2_exp_r5, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_exp_r5, extractiveness, 0.55).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_exp_r5, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_exp_r5, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(26usc469_real_estate_exemption_u2_exp_r5, tangled_rope).
narrative_ontology:human_readable(26usc469_real_estate_exemption_u2_exp_r5, "The Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain(26usc469_real_estate_exemption_u2_exp_r5, "economic/legal").

domain_priors:requires_active_enforcement(26usc469_real_estate_exemption_u2_exp_r5).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(26usc469_real_estate_exemption_u2_exp_r5, full_time_real_estate_professionals).
narrative_ontology:constraint_victim(26usc469_real_estate_exemption_u2_exp_r5, hybrid_w2_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For an individual with a primary career, the two-part test is often impossible to meet, trapping their passive losses without recourse. The only exit is to abandon their primary career, a prohibitively high cost.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r5, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For those whose primary business is real estate, the rule is a clear coordination mechanism that defines their professional status and unlocks significant tax advantages, with minimal perceived extraction.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r5, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% The IRS is constrained to enforce the law as written. It sees both the coordination function (defining a class of taxpayer) and the extractive result (increased tax revenue from the disallowed losses of the W-2 group).
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r5, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The analyst sees a hybrid system: a legitimate attempt to distinguish active from passive participation (coordination) that is implemented via a rigid, bright-line test with highly asymmetric outcomes (extraction).
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r5, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(26usc469_real_estate_exemption_u2_exp_r5_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r5, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r5, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(26usc469_real_estate_exemption_u2_exp_r5, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(26usc469_real_estate_exemption_u2_exp_r5_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.55) represents the significant tax value of the disallowed losses, which are effectively transferred from the W-2 investor to the state. The suppression score (0.62) reflects the rigidity of the two-part test; for the target group, there are no legal alternatives to achieve the same tax treatment without abandoning their primary career, making the constraint highly coercive.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the Hybrid W-2 Investor and the Full-Time Real Estate Professional. The investor experiences the rule as a Snare, a trap that penalizes their attempt to build wealth through a common investment vehicle. The full-time professional sees it as a Rope, a clear and fair rule that recognizes their active participation and provides a deserved business advantage. This gap is the core of the constraint's structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality is unambiguous. It extracts from 'hybrid_w2_investors' by disallowing deductions they would otherwise be able to take. The value flows to the government in the form of higher tax revenue. 'full_time_real_estate_professionals' are the explicit beneficiaries, as the rule carves out an exception specifically for them, shielding them from the extraction applied to others.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope correctly captures its dual nature. It is not a pure Snare, as it's built around a legitimate policy goal of distinguishing active from passive business participation. However, it is not a pure Rope, because the mechanism chosen to achieve this goal generates severe, asymmetric extraction. The Tangled Rope classification avoids mislabeling a functional but highly extractive policy as either pure coordination or pure predation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rep_status_policy_intent,
    'Is the strict 'more than half of personal services' test a necessary guardrail against abuse, or a deliberate barrier to protect a professional class and increase tax revenue from high-earners?',
    'Analysis of the legislative history of the Tax Reform Act of 1986 and subsequent CBO scoring of the provision's impact on different income quintiles.',
    'If deemed a necessary guardrail, the constraint leans more towards a high-friction Rope. If a deliberate barrier, it confirms the Tangled Rope/Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rep_status_policy_intent, conceptual, 'The core ambiguity in the policy's intent: abuse prevention vs. targeted extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(26usc469_real_estate_exemption_u2_exp_r5, 1986, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(26us_tr_t1986, 26usc469_real_estate_exemption_u2_exp_r5, theater_ratio, 1986, 0.1).
narrative_ontology:measurement(26us_tr_t2005, 26usc469_real_estate_exemption_u2_exp_r5, theater_ratio, 2005, 0.12).
narrative_ontology:measurement(26us_tr_t2024, 26usc469_real_estate_exemption_u2_exp_r5, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(26us_be_t1986, 26usc469_real_estate_exemption_u2_exp_r5, base_extractiveness, 1986, 0.5).
narrative_ontology:measurement(26us_be_t2005, 26usc469_real_estate_exemption_u2_exp_r5, base_extractiveness, 2005, 0.52).
narrative_ontology:measurement(26us_be_t2024, 26usc469_real_estate_exemption_u2_exp_r5, base_extractiveness, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(26usc469_real_estate_exemption_u2_exp_r5, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
