% ============================================================================
% CONSTRAINT STORY: 26usc469_real_estate_exemption_u3_exp_r1
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_26usc469_real_estate_exemption_u3_exp_r1, []).

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
 *   constraint_id: 26usc469_real_estate_exemption_u3_exp_r1
 *   human_readable: The Real Estate Professional Exemption (Passive Activity Loss Rules)
 *   domain: economic/legal
 *
 * SUMMARY:
 *   Section 469 of the US tax code generally prevents taxpayers from
 *   deducting passive activity losses against active income. The (c)(7)
 *   exemption, added in 1993, creates an exception for 'real estate
 *   professionals.' However, qualification requires meeting a strict,
 *   two-part test: performing more than 750 hours of service in real property
 *   trades or businesses, AND these services constituting more than half of
 *   the individual's total personal services. This structure creates a high
 *   barrier for individuals with demanding, high-income W-2 jobs, effectively
 *   bifurcating taxpayers into those who can and cannot access these valuable
 *   deductions, regardless of the scale of their real estate activities.
 *
 * KEY AGENTS:
 *   - Hybrid W-2 Investors: Primary target (powerless/trapped) — bears extraction via disallowed losses.
 *   - Full-Time Real Estate Professionals: Primary beneficiary (organized/mobile) — benefits from the clear qualification path and tax advantages.
 *   - The IRS: Enforcing institution (institutional/constrained) — administers the rule as written.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(26usc469_real_estate_exemption_u3_exp_r1, 0.68).
domain_priors:suppression_score(26usc469_real_estate_exemption_u3_exp_r1, 0.8).
domain_priors:theater_ratio(26usc469_real_estate_exemption_u3_exp_r1, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u3_exp_r1, extractiveness, 0.68).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u3_exp_r1, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u3_exp_r1, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(26usc469_real_estate_exemption_u3_exp_r1, tangled_rope).
narrative_ontology:human_readable(26usc469_real_estate_exemption_u3_exp_r1, "The Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain(26usc469_real_estate_exemption_u3_exp_r1, "economic/legal").

domain_priors:requires_active_enforcement(26usc469_real_estate_exemption_u3_exp_r1).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(26usc469_real_estate_exemption_u3_exp_r1, full_time_real_estate_professionals).
narrative_ontology:constraint_victim(26usc469_real_estate_exemption_u3_exp_r1, hybrid_w2_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of an individual with a primary career, the tests are nearly impossible to meet, making this a trap that disallows legitimate business losses.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_exp_r1, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For those whose primary profession is real estate, this is a clear rule that coordinates their tax status and provides a significant, predictable benefit.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_exp_r1, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% The enforcing body sees both the coordination function of defining a professional class and the extractive asymmetry it creates, which must be actively audited and enforced.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_exp_r1, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The objective view recognizes the dual nature: a coordination mechanism for a specific industry that simultaneously creates a high barrier to entry, extracting value from another class of investors.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_exp_r1, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(26usc469_real_estate_exemption_u3_exp_r1_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_exp_r1, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_exp_r1, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(26usc469_real_estate_exemption_u3_exp_r1, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(26usc469_real_estate_exemption_u3_exp_r1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the direct financial consequence for the victim group is the inability to offset other income with real estate losses, a significant monetary value. Suppression (0.80) is very high because the dual requirements (750 hours AND >50% of time) are structurally designed to be almost impossible for someone with a demanding non-real-estate career to meet, effectively suppressing their access to this tax status.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the W-2 investor, who sees a Snare (an arbitrary and insurmountable barrier to a tax benefit), and the full-time professional, who sees a Rope (a clear rule that defines and protects their professional status). The IRS and analytical observers see the Tangled Rope, acknowledging both the rule's coordination function and its highly asymmetric, extractive outcome.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint directs tax benefits (the ability to deduct passive losses) towards a narrowly defined group of 'full-time real estate professionals' (the beneficiaries). The cost is borne by other real estate investors, particularly those with high W-2 incomes (the victims), from whom this tax benefit is withheld by the strict qualification criteria. The flow of value is from the disallowed deductions of the victim class to the general treasury, which in turn subsidizes the tax expenditures for the beneficiary class.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification is critical here. A simple Snare classification would miss the legitimate coordination function the rule provides for the beneficiary class. A Rope classification would ignore the severe, targeted extraction imposed on the victim class. Tangled Rope correctly identifies that a mechanism of professional classification has been coupled with a highly effective barrier that creates a permanent class of insiders and outsiders.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rep_status_legislative_intent,
    'Was the strict two-part test intended to define a legitimate professional class for business purposes, or to gatekeep tax benefits from high-income professionals in other fields?',
    'Analysis of legislative history and lobbying records from the Revenue Reconciliation Act of 1993.',
    'If for legitimate business definition, it leans towards a high-barrier Rope. If for gatekeeping, it confirms the Snare-like properties for the target group.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rep_status_legislative_intent, empirical, 'The original legislative intent behind the strict 'Real Estate Professional' definition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(26usc469_real_estate_exemption_u3_exp_r1, 1993, 2043).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(26us_tr_t1993, 26usc469_real_estate_exemption_u3_exp_r1, theater_ratio, 1993, 0.1).
narrative_ontology:measurement(26us_tr_t2018, 26usc469_real_estate_exemption_u3_exp_r1, theater_ratio, 2018, 0.12).
narrative_ontology:measurement(26us_tr_t2043, 26usc469_real_estate_exemption_u3_exp_r1, theater_ratio, 2043, 0.15).

% Extraction over time
narrative_ontology:measurement(26us_be_t1993, 26usc469_real_estate_exemption_u3_exp_r1, base_extractiveness, 1993, 0.6).
narrative_ontology:measurement(26us_be_t2018, 26usc469_real_estate_exemption_u3_exp_r1, base_extractiveness, 2018, 0.65).
narrative_ontology:measurement(26us_be_t2043, 26usc469_real_estate_exemption_u3_exp_r1, base_extractiveness, 2043, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(26usc469_real_estate_exemption_u3_exp_r1, enforcement_mechanism).
narrative_ontology:affects_constraint(26usc469_real_estate_exemption_u3_exp_r1, usc26_s1031_like_kind_exchange).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
