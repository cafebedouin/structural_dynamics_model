% ============================================================================
% CONSTRAINT STORY: 26usc469_real_estate_exemption_u1_exp_r4
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_26usc469_real_estate_exemption_u1_exp_r4, []).

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
 *   constraint_id: 26usc469_real_estate_exemption_u1_exp_r4
 *   human_readable: The Real Estate Professional Exemption (Passive Activity Loss Rules)
 *   domain: economic/legal
 *
 * SUMMARY:
 *   Section 469(c)(7) of the U.S. Internal Revenue Code provides an exception
 *   to passive activity loss (PAL) rules for taxpayers who qualify as 'real
 *   estate professionals.' The qualification requires meeting two strict
 *   tests: performing more than 750 hours of service in real property trades
 *   and these services constituting more than half of the individual's total
 *   personal services. This structure effectively creates a bright-line rule
 *   that is nearly impossible for individuals with demanding, high-income W-2
 *   careers to meet, thereby denying them tax deductions available to
 *   full-time real estate investors.
 *
 * KEY AGENTS:
 *   - Hybrid W-2 Investors: Primary target (powerless/trapped) - Cannot meet the time-based tests without abandoning their primary career, thus their real estate losses are disallowed against their active income.
 *   - Full-Time Real Estate Professionals: Primary beneficiary (organized/mobile) - Easily meet the tests, allowing them to deduct unlimited real estate losses against all other income.
 *   - The IRS: Enforcing institution (institutional/constrained) - Tasked with auditing and enforcing this clear but asymmetric rule.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(26usc469_real_estate_exemption_u1_exp_r4, 0.55).
domain_priors:suppression_score(26usc469_real_estate_exemption_u1_exp_r4, 0.7).
domain_priors:theater_ratio(26usc469_real_estate_exemption_u1_exp_r4, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u1_exp_r4, extractiveness, 0.55).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u1_exp_r4, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u1_exp_r4, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(26usc469_real_estate_exemption_u1_exp_r4, tangled_rope).
narrative_ontology:human_readable(26usc469_real_estate_exemption_u1_exp_r4, "The Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain(26usc469_real_estate_exemption_u1_exp_r4, "economic/legal").

domain_priors:requires_active_enforcement(26usc469_real_estate_exemption_u1_exp_r4).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(26usc469_real_estate_exemption_u1_exp_r4, full_time_real_estate_professionals).
narrative_ontology:constraint_victim(26usc469_real_estate_exemption_u1_exp_r4, hybrid_w2_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of a high-income earner with a primary job, the two-part test is an insurmountable barrier to accessing tax deductions available to others. They are trapped by their primary career and cannot meet the time requirements, making the rule a pure extraction of tax benefits.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u1_exp_r4, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For those whose primary business is real estate, this rule is a clear, beneficial coordination mechanism. It defines their professional status, legitimizes their deductions, and protects the value of those deductions from being diluted by casual investors. It appears as a fair standard.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u1_exp_r4, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% The enforcing institution sees a rule with a clear coordination function (distinguishing active from passive participation) but is also aware of its highly asymmetric and extractive outcomes. The IRS is constrained to enforce the law as written, managing a system that both coordinates and extracts.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u1_exp_r4, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The analytical view recognizes the dual nature of the constraint. It serves a legitimate coordination purpose (preventing passive income shelters) but achieves this through a mechanism that creates a protected class of investors, extracting potential tax savings from one group and concentrating them in another.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u1_exp_r4, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(26usc469_real_estate_exemption_u1_exp_r4_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u1_exp_r4, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u1_exp_r4, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(26usc469_real_estate_exemption_u1_exp_r4, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(26usc469_real_estate_exemption_u1_exp_r4_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.55) reflects the significant tax value transferred from the 'victim' group to the 'beneficiary' group. The disallowed losses for one become a protected tax advantage for the other. Suppression (0.70) is high because the constraint is codified in federal law, with no legal alternatives for taxpayers who fall under its purview.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the Hybrid Investor, who sees an arbitrary and insurmountable Snare, and the Real Estate Professional, who sees a legitimate Rope that defines and protects their professional status. The former experiences it as pure extraction, the latter as fair coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is clear: the constraint benefits individuals who dedicate their careers to real estate (beneficiaries) at the direct expense of those who invest in real estate alongside another primary career (victims). The flow of value is the tax benefit that one group can access and the other cannot.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope correctly captures its dual function. It is not a pure Snare, as it has a stated and plausible coordination goal (to distinguish active from passive participation). However, it is not a pure Rope, because its mechanism creates a starkly divided in-group and out-group, with a clear extractive transfer of tax advantages. This classification avoids mislabeling a targeted subsidy as a neutral coordination rule.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    re_pro_policy_intent,
    'Was the high barrier (750 hours + >50% time) intended as a precise anti-abuse measure or as a means to create a protected class of real estate investors?',
    'Analysis of the legislative history of the Tax Reform Act of 1986 and records of lobbying by real estate industry groups.',
    'If purely for anti-abuse, it's a flawed Rope. If to create a protected class, it's a deliberate Tangled Rope with Snare-like properties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(re_pro_policy_intent, empirical, 'The ambiguity of legislative intent behind the strict 'real estate professional' definition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(26usc469_real_estate_exemption_u1_exp_r4, 1986, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(26us_tr_t1986, 26usc469_real_estate_exemption_u1_exp_r4, theater_ratio, 1986, 0.15).
narrative_ontology:measurement(26us_tr_t2005, 26usc469_real_estate_exemption_u1_exp_r4, theater_ratio, 2005, 0.18).
narrative_ontology:measurement(26us_tr_t2024, 26usc469_real_estate_exemption_u1_exp_r4, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(26us_be_t1986, 26usc469_real_estate_exemption_u1_exp_r4, base_extractiveness, 1986, 0.45).
narrative_ontology:measurement(26us_be_t2005, 26usc469_real_estate_exemption_u1_exp_r4, base_extractiveness, 2005, 0.5).
narrative_ontology:measurement(26us_be_t2024, 26usc469_real_estate_exemption_u1_exp_r4, base_extractiveness, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(26usc469_real_estate_exemption_u1_exp_r4, information_standard).
narrative_ontology:affects_constraint(26usc469_real_estate_exemption_u1_exp_r4, usc26_s469_passive_activity_loss_rules).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
