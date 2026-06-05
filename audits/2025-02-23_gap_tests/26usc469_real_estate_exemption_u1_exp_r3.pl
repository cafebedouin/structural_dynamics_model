% ============================================================================
% CONSTRAINT STORY: 26usc469_real_estate_exemption_u1_exp_r3
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_26usc469_real_estate_exemption_u1_exp_r3, []).

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
 *   constraint_id: 26usc469_real_estate_exemption_u1_exp_r3
 *   human_readable: The Real Estate Professional Exemption (Passive Activity Loss Rules)
 *   domain: economic/legal
 *
 * SUMMARY:
 *   Section 469 of the US tax code, introduced in 1986, limits the ability to
 *   deduct passive activity losses against active income. The Real Estate
 *   Professional Status (REPS) exemption in §469(c)(7) provides a workaround,
 *   but only for those who meet a strict two-part test: (1) performing over
 *   750 hours in real property trades, and (2) performing more than half of
 *   their total personal services in those trades. This second prong creates
 *   a nearly insurmountable barrier for individuals with demanding,
 *   high-income W-2 jobs, bifurcating taxpayers into a beneficiary class
 *   (full-time real estate pros) and a victim class (W-2 investors) from whom
 *   tax revenue is extracted via disallowed losses.
 *
 * KEY AGENTS:
 *   - Hybrid W-2 Investors: Primary target (powerless/trapped) — bears extraction via disallowed losses.
 *   - Full-Time Real Estate Professionals: Primary beneficiary (moderate/mobile) — benefits from the clear qualification path and resulting deductions.
 *   - The IRS: Enforcing institution (institutional/constrained) — administers the rule as written.
 *   - Tax Policy Analysts: Analytical observer (analytical/analytical) — studies the structural effects of the law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(26usc469_real_estate_exemption_u1_exp_r3, 0.68).
domain_priors:suppression_score(26usc469_real_estate_exemption_u1_exp_r3, 0.75).
domain_priors:theater_ratio(26usc469_real_estate_exemption_u1_exp_r3, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u1_exp_r3, extractiveness, 0.68).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u1_exp_r3, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u1_exp_r3, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(26usc469_real_estate_exemption_u1_exp_r3, tangled_rope).
narrative_ontology:human_readable(26usc469_real_estate_exemption_u1_exp_r3, "The Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain(26usc469_real_estate_exemption_u1_exp_r3, "economic/legal").

domain_priors:requires_active_enforcement(26usc469_real_estate_exemption_u1_exp_r3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(26usc469_real_estate_exemption_u1_exp_r3, full_time_real_estate_professionals).
narrative_ontology:constraint_victim(26usc469_real_estate_exemption_u1_exp_r3, hybrid_w2_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For an individual with a high-income W-2 job, the 'more than half of personal services' test is a structural barrier, making their real estate losses non-deductible against their primary income. The constraint is a pure extraction mechanism.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u1_exp_r3, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For someone whose primary business is real estate, the rule is a clear, low-cost coordination mechanism that defines their professional status and unlocks significant tax advantages.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u1_exp_r3, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% The IRS administers a rule that has a clear coordination function (defining a class of taxpayer) but requires active enforcement of its highly extractive consequences for the non-qualifying group.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u1_exp_r3, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The analyst sees a system with a legitimate policy goal (limiting passive loss shelters) that is implemented via a mechanism with strong, asymmetric extractive outcomes, benefiting one class of investor at the direct expense of another.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u1_exp_r3, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(26usc469_real_estate_exemption_u1_exp_r3_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u1_exp_r3, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u1_exp_r3, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(26usc469_real_estate_exemption_u1_exp_r3, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(26usc469_real_estate_exemption_u1_exp_r3_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.68) reflects the significant tax liability created by disallowing potentially large 'paper losses' from depreciation against ordinary income. The high suppression (0.75) comes from the 'more than half of personal services' test, which effectively forces a choice between a high-income career and accessing this tax strategy, an alternative most are unable to take.
 *
 * PERSPECTIVAL GAP:
 *   A full-time real estate professional experiences this law as a simple Rope: a clear set of rules defining their professional status that enables their business model. A W-2 employee with significant real estate investments experiences it as a Snare: a trap where the rules are designed in a way that makes it structurally impossible for them to qualify, leading to direct financial extraction (higher taxes).
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint directs benefits (tax deductions) to those whose entire economic activity is within real estate, making them the beneficiaries. It extracts value (denied deductions, resulting in higher tax payments) from those who generate income primarily outside of real estate but invest within it, making them the victims. The structure of the test itself determines this directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a pure Snare would miss its legitimate coordination function of defining a 'professional' to prevent abuse of tax shelters. Classifying it as a Rope would ignore the massive, asymmetric extraction from a clearly defined group. The Tangled Rope classification correctly captures this duality: a rule with a non-zero coordination function that is inextricably linked to a highly coercive and extractive outcome.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    policy_intent_vs_effect,
    'Was the high barrier for W-2 earners an intended feature to limit the deduction's scope, or an unintended consequence of defining 'professional'?',
    'Analysis of legislative history and committee reports from the Tax Reform Act of 1986.',
    'If intended to gatekeep, the Snare component is primary. If an unintended side-effect, it's a flawed Rope that degraded into a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_intent_vs_effect, empirical, 'Distinguishing between intended policy extraction and accidental structural barriers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(26usc469_real_estate_exemption_u1_exp_r3, 1986, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(26us_tr_t1986, 26usc469_real_estate_exemption_u1_exp_r3, theater_ratio, 1986, 0.1).
narrative_ontology:measurement(26us_tr_t2005, 26usc469_real_estate_exemption_u1_exp_r3, theater_ratio, 2005, 0.15).
narrative_ontology:measurement(26us_tr_t2024, 26usc469_real_estate_exemption_u1_exp_r3, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(26us_be_t1986, 26usc469_real_estate_exemption_u1_exp_r3, base_extractiveness, 1986, 0.5).
narrative_ontology:measurement(26us_be_t2005, 26usc469_real_estate_exemption_u1_exp_r3, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement(26us_be_t2024, 26usc469_real_estate_exemption_u1_exp_r3, base_extractiveness, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(26usc469_real_estate_exemption_u1_exp_r3, enforcement_mechanism).
narrative_ontology:affects_constraint(26usc469_real_estate_exemption_u1_exp_r3, usc26_s168_depreciation_rules).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
