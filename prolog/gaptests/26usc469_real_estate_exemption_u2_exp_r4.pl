% ============================================================================
% CONSTRAINT STORY: 26usc469_real_estate_exemption_u2_exp_r4
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_26usc469_real_estate_exemption_u2_exp_r4, []).

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
 *   constraint_id: 26usc469_real_estate_exemption_u2_exp_r4
 *   human_readable: The Real Estate Professional Exemption (Passive Activity Loss Rules)
 *   domain: economic/legal
 *
 * SUMMARY:
 *   Section 469 of the US tax code disallows the deduction of passive
 *   activity losses against active income. The (c)(7) exemption for 'real
 *   estate professionals' provides a workaround, but only for those who meet
 *   a strict two-part test: performing over 750 hours in real property trades
 *   AND having this constitute more than half of their total personal
 *   services. This second prong creates a nearly impossible barrier for
 *   individuals with high-income W-2 jobs, bifurcating taxpayers into a
 *   beneficiary class that can deduct losses and a victim class that cannot.
 *
 * KEY AGENTS:
 *   - Hybrid W-2 Investors: Primary target (powerless/trapped) — their passive losses are disallowed, creating a higher tax burden.
 *   - Full-Time Real Estate Professionals: Primary beneficiary (moderate/mobile) — they can easily meet the test and deduct unlimited passive losses.
 *   - The IRS: Enforcing institution (institutional/constrained) — administers the rule as written by Congress.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(26usc469_real_estate_exemption_u2_exp_r4, 0.68).
domain_priors:suppression_score(26usc469_real_estate_exemption_u2_exp_r4, 0.8).
domain_priors:theater_ratio(26usc469_real_estate_exemption_u2_exp_r4, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_exp_r4, extractiveness, 0.68).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_exp_r4, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_exp_r4, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(26usc469_real_estate_exemption_u2_exp_r4, tangled_rope).
narrative_ontology:human_readable(26usc469_real_estate_exemption_u2_exp_r4, "The Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain(26usc469_real_estate_exemption_u2_exp_r4, "economic/legal").

domain_priors:requires_active_enforcement(26usc469_real_estate_exemption_u2_exp_r4).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(26usc469_real_estate_exemption_u2_exp_r4, full_time_real_estate_professionals).
narrative_ontology:constraint_victim(26usc469_real_estate_exemption_u2_exp_r4, hybrid_w2_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: For an individual with a demanding primary career, the 'more than half of personal services' test is an almost insurmountable barrier, trapping their passive losses.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r4, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: For someone whose primary business is real estate, the rule is a clear coordination mechanism that defines their professional status and unlocks significant tax advantages.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r4, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: The IRS administers a rule that both coordinates a definition of 'professional' and enforces an extractive barrier against another class of taxpayers. It cannot change the rule, only enforce it.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r4, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: The analyst sees the dual function: a legitimate policy goal (coordination) to separate active/passive investors, implemented via a mechanism that creates severe asymmetric extraction.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r4, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(26usc469_real_estate_exemption_u2_exp_r4_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r4, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r4, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(26usc469_real_estate_exemption_u2_exp_r4, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(26usc469_real_estate_exemption_u2_exp_r4_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.68) reflects the direct financial cost of disallowed deductions, which can be tens of thousands of dollars annually. The high suppression (0.80) reflects the extreme cost of the primary alternative for a W-2 earner: quitting their primary career to satisfy the 'more than half' services test.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the full-time professional, it's a Rope—a clear, beneficial rule. For the W-2 investor, it's a Snare—a tax trap with no reasonable exit. The analytical view acknowledges both functions, classifying it as a Tangled Rope where a coordination goal is achieved through an extractive mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint directs tax benefits specifically to the class of 'full-time real estate professionals' while extracting value (in the form of higher taxes via disallowed losses) from 'hybrid W-2 investors'. The structure creates a protected class and a targeted class.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by being correctly classified as a Tangled Rope. A simplistic analysis might label it a Snare, ignoring its stated coordination purpose of preventing passive tax shelters. Conversely, labeling it a Rope would ignore the severe, asymmetric extraction imposed on a specific group. The Tangled Rope classification captures this essential duality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    policy_intent_vs_outcome,
    'Was the high barrier for W-2 earners an intended feature to limit the deduction's scope, or an unintended consequence of a bright-line test?',
    'Analysis of legislative history and Congressional Budget Office scoring documents from the Tax Reform Act of 1986.',
    'If intended to gatekeep, it's a pure Snare by design. If an unintended side effect of a simple rule, it's a Tangled Rope where the coordination function failed for a specific subgroup.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_intent_vs_outcome, empirical, 'Distinguishing between intended policy targeting and unintended consequences of the 750-hour and 'more-than-half' rules.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(26usc469_real_estate_exemption_u2_exp_r4, 1986, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(26us_tr_t1986, 26usc469_real_estate_exemption_u2_exp_r4, theater_ratio, 1986, 0.15).
narrative_ontology:measurement(26us_tr_t2006, 26usc469_real_estate_exemption_u2_exp_r4, theater_ratio, 2006, 0.15).
narrative_ontology:measurement(26us_tr_t2026, 26usc469_real_estate_exemption_u2_exp_r4, theater_ratio, 2026, 0.15).

% Extraction over time
narrative_ontology:measurement(26us_be_t1986, 26usc469_real_estate_exemption_u2_exp_r4, base_extractiveness, 1986, 0.6).
narrative_ontology:measurement(26us_be_t2006, 26usc469_real_estate_exemption_u2_exp_r4, base_extractiveness, 2006, 0.65).
narrative_ontology:measurement(26us_be_t2026, 26usc469_real_estate_exemption_u2_exp_r4, base_extractiveness, 2026, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(26usc469_real_estate_exemption_u2_exp_r4, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
