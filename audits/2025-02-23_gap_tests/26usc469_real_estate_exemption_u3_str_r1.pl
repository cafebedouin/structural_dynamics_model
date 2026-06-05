% ============================================================================
% CONSTRAINT STORY: 26usc469_real_estate_exemption_u3_str_r1
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_26usc469_real_estate_exemption_u3_str_r1, []).

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
 *   constraint_id: 26usc469_real_estate_exemption_u3_str_r1
 *   human_readable: The Real Estate Professional Exemption (Passive Activity Loss Rules)
 *   domain: economic/legal
 *
 * SUMMARY:
 *   Section 469 of the U.S. tax code disallows the deduction of passive
 *   activity losses against ordinary income. The '(c)(7)' exemption creates a
 *   carve-out for 'real estate professionals' who meet a strict two-part
 *   test: (1) performing more than 750 hours of service in real property
 *   trades or businesses, and (2) performing more services in those trades
 *   than in all other trades or businesses combined. This second prong
 *   effectively blocks most individuals with high-income W-2 jobs from
 *   qualifying, creating a sharp bifurcation between taxpayers who can fully
 *   deduct real estate losses and those who cannot.
 *
 * KEY AGENTS:
 *   - Hybrid W-2 Investors: Primary target (powerless/trapped) - High-income earners with a primary job who also invest in real estate and are barred from deducting losses.
 *   - Full-Time Real Estate Professionals: Primary beneficiary (moderate/mobile) - Individuals whose primary work is in real estate and who can easily meet the test to unlock significant tax benefits.
 *   - The IRS: Enforcing institution (institutional/constrained) - Administers and audits compliance with the rule as written.
 *   - Tax Policy Analysts: Analytical observers - Evaluate the rule's dual function as both a professional standard and an extractive barrier.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(26usc469_real_estate_exemption_u3_str_r1, 0.68).
domain_priors:suppression_score(26usc469_real_estate_exemption_u3_str_r1, 0.75).
domain_priors:theater_ratio(26usc469_real_estate_exemption_u3_str_r1, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u3_str_r1, extractiveness, 0.68).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u3_str_r1, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u3_str_r1, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(26usc469_real_estate_exemption_u3_str_r1, tangled_rope).
narrative_ontology:human_readable(26usc469_real_estate_exemption_u3_str_r1, "The Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain(26usc469_real_estate_exemption_u3_str_r1, "economic/legal").

domain_priors:requires_active_enforcement(26usc469_real_estate_exemption_u3_str_r1).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(26usc469_real_estate_exemption_u3_str_r1, full_time_real_estate_professionals).
narrative_ontology:constraint_victim(26usc469_real_estate_exemption_u3_str_r1, hybrid_w2_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: For the individual with a primary career, the two-part test is an almost insurmountable barrier, making their real estate losses non-deductible against their main income. It feels like a trap designed to prevent them from accessing tax benefits available to others.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_str_r1, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: For those whose primary business is real estate, the rule is a clear coordination mechanism. It defines their professional status and provides a predictable pathway to significant tax advantages, enabling their business model.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_str_r1, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: The IRS enforces a rule that has a clear coordination function (defining a professional class) but also results in significant asymmetric extraction (higher tax revenue from the W-2 investor class). It is a hybrid system.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_str_r1, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: The analytical view recognizes the dual function. The law coordinates by creating a specific legal class but does so via a highly suppressive test that extracts value from one group to the benefit of another and the state.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_str_r1, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(26usc469_real_estate_exemption_u3_str_r1_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_str_r1, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_str_r1, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(26usc469_real_estate_exemption_u3_str_r1, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(26usc469_real_estate_exemption_u3_str_r1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.68) is high because the disallowed losses can represent tens or hundreds of thousands of dollars in tax liability annually for an affected investor. The suppression score (0.75) is also high because the 'more than half of personal services' test is a nearly absolute barrier for anyone with a demanding, non-real-estate career, effectively forcing a choice between their primary profession and accessing these tax benefits.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the Hybrid W-2 Investor, who sees a Snare (a tax trap they cannot escape without quitting their job), and the Full-Time Real Estate Professional, who sees a Rope (a clear, beneficial rule that defines their professional advantage). The IRS and analytical observers see the combined structure as a Tangled Rope, acknowledging both the coordination function and the severe, asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's structure directs tax benefits (the ability to deduct unlimited passive losses) exclusively towards the beneficiary class (full-time RE pros). It simultaneously extracts value from the victim class (hybrid W-2 investors) by denying them the same deductions, thereby increasing their tax burden relative to what it would otherwise be. This is a direct, legally enforced transfer of financial advantage.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a pure Snare would be inaccurate, as it does perform a genuine coordination function: it legally defines what constitutes a 'professional' in this context. However, classifying it as a Rope would ignore the immense and targeted extraction from a specific, non-qualifying group. The Tangled Rope classification correctly captures this duality, preventing the mislabeling of a highly extractive policy as a simple coordination tool.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rep_status_legislative_intent,
    'Was the strict 'more than half of personal services' test primarily intended as a coordination mechanism to define 'professional', or as an extractive tool to gatekeep tax benefits from high-income W-2 earners?',
    'Detailed analysis of the legislative history and Congressional Budget Office scoring models from the Revenue Reconciliation Act of 1993.',
    'If intent was purely to define a professional class, it solidifies the Tangled Rope classification. If intent was primarily to limit deductions for a specific taxpayer class, it leans closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rep_status_legislative_intent, empirical, 'The legislative intent behind the strict two-part test for Real Estate Professional status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(26usc469_real_estate_exemption_u3_str_r1, 1993, 2043).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(26us_tr_t1993, 26usc469_real_estate_exemption_u3_str_r1, theater_ratio, 1993, 0.1).
narrative_ontology:measurement(26us_tr_t2010, 26usc469_real_estate_exemption_u3_str_r1, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(26us_tr_t2024, 26usc469_real_estate_exemption_u3_str_r1, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(26us_be_t1993, 26usc469_real_estate_exemption_u3_str_r1, base_extractiveness, 1993, 0.6).
narrative_ontology:measurement(26us_be_t2010, 26usc469_real_estate_exemption_u3_str_r1, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(26us_be_t2024, 26usc469_real_estate_exemption_u3_str_r1, base_extractiveness, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(26usc469_real_estate_exemption_u3_str_r1, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
