% ============================================================================
% CONSTRAINT STORY: 26usc469_real_estate_exemption_u3_exp_r3
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_26usc469_real_estate_exemption_u3_exp_r3, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: 26usc469_real_estate_exemption_u3_exp_r3
 *   human_readable: The Real Estate Professional Exemption (Passive Activity Loss Rules)
 *   domain: economic/legal
 *
 * SUMMARY:
 *   Section 469 of the U.S. tax code limits the deduction of passive activity
 *   losses. The Real Estate Professional Status (REPS) exemption in
 *   subsection (c)(7) provides a significant carve-out, but only for
 *   taxpayers who pass a strict two-part test: (1) spending more than 750
 *   hours in real property trades or businesses, AND (2) performing more than
 *   half of their total personal services in those trades. This second prong
 *   effectively creates an insurmountable barrier for individuals with
 *   demanding, high-income W-2 jobs, bifurcating taxpayers into a
 *   professional class that can fully deduct real estate losses and an
 *   amateur class that cannot.
 *
 * KEY AGENTS:
 *   - High-Income W-2 Investors: Primary target (powerless/trapped) - Their losses are disallowed due to the 'more than half' services test.
 *   - Full-Time Real Estate Professionals: Primary beneficiary (moderate/mobile) - They easily meet the test and benefit from deducting losses against other income.
 *   - The IRS: Enforcing institution (institutional/constrained) - Must administer the complex and frequently litigated rule as written.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(26usc469_real_estate_exemption_u3_exp_r3, 0.68).
domain_priors:suppression_score(26usc469_real_estate_exemption_u3_exp_r3, 0.8).
domain_priors:theater_ratio(26usc469_real_estate_exemption_u3_exp_r3, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u3_exp_r3, extractiveness, 0.68).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u3_exp_r3, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u3_exp_r3, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(26usc469_real_estate_exemption_u3_exp_r3, tangled_rope).
narrative_ontology:human_readable(26usc469_real_estate_exemption_u3_exp_r3, "The Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain(26usc469_real_estate_exemption_u3_exp_r3, "economic/legal").

domain_priors:requires_active_enforcement(26usc469_real_estate_exemption_u3_exp_r3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(26usc469_real_estate_exemption_u3_exp_r3, full_time_real_estate_professionals).
narrative_ontology:constraint_victim(26usc469_real_estate_exemption_u3_exp_r3, high_income_w2_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: For an individual with a demanding, high-income primary career, the 'more than half of personal services' test is an insurmountable barrier. They are trapped; their only exit is to abandon their primary career, a disproportionately high cost. The rule functions as a snare, preventing them from accessing tax deductions available to others.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_exp_r3, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: For someone whose primary business is real estate, the rule is a clear, predictable coordination mechanism. It defines their professional status and grants them corresponding tax benefits. It feels like a simple, fair rule of the road.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_exp_r3, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: The IRS is constrained to enforce the law as written. It sees the rule's dual nature: a coordination function to define a class of taxpayers, and an extractive function that generates revenue by disallowing losses for a specific group. The complexity of enforcement and litigation highlights its tangled nature.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_exp_r3, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: The analytical view recognizes both the stated coordination goal (preventing passive tax shelters) and the effective extractive outcome. The strictness of the test creates a protected class of beneficiaries and a targeted class of victims, a hallmark of a Tangled Rope.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_exp_r3, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(26usc469_real_estate_exemption_u3_exp_r3_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_exp_r3, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_exp_r3, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(26usc469_real_estate_exemption_u3_exp_r3, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(26usc469_real_estate_exemption_u3_exp_r3_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.68) reflects the significant financial value of the disallowed loss deductions. The very high suppression (0.80) is due to the 'more than half of personal services' test, which for a high-earning professional in another field, is not a choice but a structural impossibility without sacrificing their primary career. This makes the alternative (qualifying for the deduction) completely suppressed.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the full-time professional, the law is a simple Rope that defines their status and confers a benefit. For the W-2 investor, it is a Snare that dangles the promise of tax benefits but makes them structurally inaccessible, trapping their passive losses. The IRS and analytical observers see the Tangled Rope: a rule with a legitimate-sounding purpose (coordination) that operates via coercive extraction from a specific, targeted group.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's structure directs value from one group to another. By creating a high barrier to entry for the tax benefit, it favors established, full-time real estate professionals (beneficiaries) and extracts potential tax savings from would-be part-time investors with other primary careers (victims). The flow of value is a direct consequence of the rule's design.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope correctly avoids mislabeling it as a simple anti-abuse rule (Rope). While it has a coordination function (defining 'professional'), its primary effect is asymmetric extraction. The high suppression score highlights that this is not a neutral standard but a coercive barrier, preventing the system from mistaking a gatekept privilege for a fair rule.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rep_status_policy_intent,
    'Is the strict two-part test a necessary guardrail against widespread tax sheltering, or is it a deliberately constructed barrier to protect the professional real estate industry from part-time competition?',
    'Analysis of the legislative history of the Tax Reform Act of 1986, including testimony and lobbying records from real estate industry groups.',
    'If proven to be a necessary guardrail, the classification might shift towards a harsh Rope. If proven to be protectionist, it solidifies its status as a Snare from the target's perspective and a Tangled Rope analytically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rep_status_policy_intent, empirical, 'Ambiguity of legislative intent behind the strict two-part test for Real Estate Professional status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(26usc469_real_estate_exemption_u3_exp_r3, 1986, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(26us_tr_t1986, 26usc469_real_estate_exemption_u3_exp_r3, theater_ratio, 1986, 0.1).
narrative_ontology:measurement(26us_tr_t2005, 26usc469_real_estate_exemption_u3_exp_r3, theater_ratio, 2005, 0.12).
narrative_ontology:measurement(26us_tr_t2024, 26usc469_real_estate_exemption_u3_exp_r3, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(26us_be_t1986, 26usc469_real_estate_exemption_u3_exp_r3, base_extractiveness, 1986, 0.45).
narrative_ontology:measurement(26us_be_t2005, 26usc469_real_estate_exemption_u3_exp_r3, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement(26us_be_t2024, 26usc469_real_estate_exemption_u3_exp_r3, base_extractiveness, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(26usc469_real_estate_exemption_u3_exp_r3, enforcement_mechanism).
narrative_ontology:affects_constraint(26usc469_real_estate_exemption_u3_exp_r3, usc26_469_passive_activity_loss_rules).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
