% ============================================================================
% CONSTRAINT STORY: 26usc469_real_estate_exemption_u4_exp_r5
% ============================================================================
% Version: 7.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_26usc469_real_estate_exemption_u4_exp_r5, []).

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
 *   constraint_id: 26usc469_real_estate_exemption_u4_exp_r5
 *   human_readable: The Real Estate Professional Exemption (Passive Activity Loss Rules)
 *   domain: economic/legal
 *
 * SUMMARY:
 *   Section 469 of the U.S. Internal Revenue Code generally prevents
 *   taxpayers from deducting passive activity losses against active income.
 *   The (c)(7) exemption carves out an exception for 'real estate
 *   professionals' who meet a strict two-part test: performing more than 750
 *   hours of service in real property trades and, crucially, performing more
 *   services in real property than in all other trades or businesses
 *   combined. This second prong creates an almost insurmountable barrier for
 *   individuals with high-income W-2 jobs, bifurcating taxpayers into a
 *   beneficiary class that can access unlimited deductions and a victim class
 *   whose identical economic losses are suspended.
 *
 * KEY AGENTS:
 *   - High-Income W-2 Employees: Primary target (powerless/trapped) — bears extraction via disallowed losses.
 *   - Full-Time Real Estate Professionals: Primary beneficiary (organized/mobile) — benefits from a clear, albeit demanding, qualification path to significant tax advantages.
 *   - The IRS: Enforcing institution (institutional/constrained) — administers the rule as legislated.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(26usc469_real_estate_exemption_u4_exp_r5, 0.68).
domain_priors:suppression_score(26usc469_real_estate_exemption_u4_exp_r5, 0.72).
domain_priors:theater_ratio(26usc469_real_estate_exemption_u4_exp_r5, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u4_exp_r5, extractiveness, 0.68).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u4_exp_r5, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u4_exp_r5, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(26usc469_real_estate_exemption_u4_exp_r5, tangled_rope).
narrative_ontology:human_readable(26usc469_real_estate_exemption_u4_exp_r5, "The Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain(26usc469_real_estate_exemption_u4_exp_r5, "economic/legal").

domain_priors:requires_active_enforcement(26usc469_real_estate_exemption_u4_exp_r5).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(26usc469_real_estate_exemption_u4_exp_r5, full_time_real_estate_professionals).
narrative_ontology:constraint_victim(26usc469_real_estate_exemption_u4_exp_r5, high_income_w2_employees).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: For the taxpayer with a primary job, the rule is a trap. They cannot meet the 'more than half of personal services' test without quitting their job, so their legitimate real estate losses are disallowed against their primary income.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u4_exp_r5, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: For those whose primary business is real estate, the rule is a clear coordination mechanism. It defines their professional status and unlocks significant tax benefits, creating a clear path for their business model.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u4_exp_r5, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: The enforcing body is constrained to apply the law as written. It sees both the coordination function (defining a taxpayer class) and the extractive result (increased tax revenue from the trapped group).
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u4_exp_r5, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: The analytical view recognizes the dual function. The rule coordinates the definition of a 'professional' to curb tax shelters, but does so via a mechanism that asymmetrically extracts value from a different class of taxpayer.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u4_exp_r5, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(26usc469_real_estate_exemption_u4_exp_r5_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u4_exp_r5, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u4_exp_r5, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(26usc469_real_estate_exemption_u4_exp_r5, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(26usc469_real_estate_exemption_u4_exp_r5_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.68) is high, representing the direct financial value (increased tax liability) extracted from the victim class. The suppression score (0.72) reflects the rigidity of the legal test; for a W-2 employee, the only 'alternative' is to quit their primary source of income, which is not a viable choice. The theater ratio is low as the rule is functionally enforced, not performative.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is stark. For the full-time professional, the law is a Rope—a clear, if demanding, set of rules for professional conduct and reward. For the W-2 investor, it is a Snare—a legal trap that invalidates their real economic losses for tax purposes. The institutional enforcer (IRS) and the analytical observer both see the combined structure as a Tangled Rope, acknowledging both the legitimate coordination goal and the asymmetric extractive outcome.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint directs value away from high-income employees who invest in real estate and towards the state in the form of increased tax revenue. Full-time real estate professionals are the structural beneficiaries, as the rule carves out a protected status for them, allowing them to operate under a more favorable tax regime than other investors. The 'more than half' test is the critical mechanism that separates victims from beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification is essential for avoiding mandatrophy. Classifying this rule as a pure Snare would ignore its stated and partially valid coordination function: to prevent the widespread use of real estate tax shelters by high-income individuals, a major issue prior to 1986. Classifying it as a pure Rope would ignore the severe, asymmetric extraction imposed on a specific, trapped class of taxpayers. The Tangled Rope correctly identifies that a legitimate policy goal is being achieved through a highly extractive and suppressive mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pal_rule_intent,
    'Is the primary purpose of the strict 'real estate professional' test to accurately define a professional class, or is it to create a high barrier to protect tax revenue from a large class of potential deductions?',
    'Analysis of legislative history and Congressional Budget Office scoring documents from the Tax Reform Act of 1986, comparing projected revenue gains from disallowing W-2 investor losses versus the stated goal of curbing tax shelters.',
    'If the intent was primarily revenue protection, it solidifies the Snare/Tangled Rope classification. If it was purely to define a professional class with unintended consequences, it might be re-evaluated as a Rope with severe negative externalities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pal_rule_intent, empirical, 'The ambiguity between the rule's stated purpose (defining professionals) and its primary effect (revenue protection via a high barrier).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(26usc469_real_estate_exemption_u4_exp_r5, 1986, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(26us_tr_t1986, 26usc469_real_estate_exemption_u4_exp_r5, theater_ratio, 1986, 0.2).
narrative_ontology:measurement(26us_tr_t2006, 26usc469_real_estate_exemption_u4_exp_r5, theater_ratio, 2006, 0.2).
narrative_ontology:measurement(26us_tr_t2026, 26usc469_real_estate_exemption_u4_exp_r5, theater_ratio, 2026, 0.2).

% Extraction over time
narrative_ontology:measurement(26us_be_t1986, 26usc469_real_estate_exemption_u4_exp_r5, base_extractiveness, 1986, 0.5).
narrative_ontology:measurement(26us_be_t2006, 26usc469_real_estate_exemption_u4_exp_r5, base_extractiveness, 2006, 0.6).
narrative_ontology:measurement(26us_be_t2026, 26usc469_real_estate_exemption_u4_exp_r5, base_extractiveness, 2026, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(26usc469_real_estate_exemption_u4_exp_r5, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
