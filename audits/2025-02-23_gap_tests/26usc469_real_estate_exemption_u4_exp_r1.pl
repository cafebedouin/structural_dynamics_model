% ============================================================================
% CONSTRAINT STORY: 26usc469_real_estate_exemption_u4_exp_r1
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_26usc469_real_estate_exemption_u4_exp_r1, []).

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
 *   constraint_id: 26usc469_real_estate_exemption_u4_exp_r1
 *   human_readable: The Real Estate Professional Exemption (Passive Activity Loss Rules)
 *   domain: economic/legal
 *
 * SUMMARY:
 *   Section 469(c)(7) of the U.S. Internal Revenue Code provides an exception
 *   to passive activity loss (PAL) limitations for taxpayers in a real
 *   property trade or business. To qualify as a 'real estate professional,' a
 *   taxpayer must satisfy two strict tests: (1) more than half of their
 *   personal services during the year are performed in real property trades
 *   or businesses, and (2) they perform more than 750 hours of services in
 *   those businesses. This structure creates a sharp divide, primarily
 *   benefiting full-time real estate investors while systematically excluding
 *   individuals with demanding, high-income W-2 jobs from accessing the same
 *   tax deductions, even if they have significant real estate holdings.
 *
 * KEY AGENTS:
 *   - Hybrid W-2 Investors: Primary target (powerless/trapped) — unable to deduct real estate losses against their primary income due to the strict qualification tests.
 *   - Full-Time Real Estate Professionals: Primary beneficiary (organized/mobile) — benefit from the ability to deduct unlimited passive losses, a significant tax subsidy.
 *   - The IRS: Enforcing institution (institutional/constrained) — tasked with auditing and enforcing the bright-line tests of the statute.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(26usc469_real_estate_exemption_u4_exp_r1, 0.68).
domain_priors:suppression_score(26usc469_real_estate_exemption_u4_exp_r1, 0.75).
domain_priors:theater_ratio(26usc469_real_estate_exemption_u4_exp_r1, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u4_exp_r1, extractiveness, 0.68).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u4_exp_r1, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u4_exp_r1, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(26usc469_real_estate_exemption_u4_exp_r1, tangled_rope).
narrative_ontology:human_readable(26usc469_real_estate_exemption_u4_exp_r1, "The Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain(26usc469_real_estate_exemption_u4_exp_r1, "economic/legal").

domain_priors:requires_active_enforcement(26usc469_real_estate_exemption_u4_exp_r1).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(26usc469_real_estate_exemption_u4_exp_r1, full_time_real_estate_professionals).
narrative_ontology:constraint_victim(26usc469_real_estate_exemption_u4_exp_r1, hybrid_w2_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of a high-income earner with a primary job, the strict tests are an insurmountable barrier to tax deductions available to others, making it a pure extraction mechanism.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u4_exp_r1, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For those whose primary business is real estate, the rule is a clear, beneficial coordination mechanism that defines their professional status and unlocks significant tax advantages.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u4_exp_r1, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% The enforcing agency sees a rule with a clear coordination function (defining a taxpayer class) that requires active policing due to the high potential for abuse and the clear asymmetric benefits it provides.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u4_exp_r1, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The analyst sees a system with a stated coordination goal (preventing passive loss shelters) that is implemented via a mechanism that structurally extracts value from one class of investors to benefit another.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u4_exp_r1, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(26usc469_real_estate_exemption_u4_exp_r1_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u4_exp_r1, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u4_exp_r1, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(26usc469_real_estate_exemption_u4_exp_r1, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(26usc469_real_estate_exemption_u4_exp_r1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.68) represents the significant tax value (disallowed loss deductions) that is inaccessible to the victim class. The suppression score (0.75) reflects the coercive nature of the tax code and the high, specific barriers (750 hours AND >50% time) that cannot be easily circumvented by someone with a primary non-real estate career.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the W-2 investor, the rule is a Snare that arbitrarily blocks them from a tax benefit. For the full-time professional, it is a Rope that legitimizes their status and provides a clear path to financial advantage. The analytical view recognizes both the coordination function (defining 'active' participation) and the severe extractive consequences, classifying it as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality is encoded in the two-part test. It is specifically designed to channel tax benefits towards individuals whose primary economic activity is real estate (beneficiaries) and away from those who earn substantial income elsewhere (victims). The IRS acts as the enforcement channel for this directed flow of value.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a classic example of how a rule with a plausible coordination rationale (preventing doctors and lawyers from using real estate 'shelters' to offset income) can function as a highly extractive system. The Tangled Rope classification is crucial because it avoids mislabeling the rule as a pure Snare (ignoring its coordination function) or a pure Rope (ignoring its massive extractive asymmetry).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    policy_intent_vs_effect,
    'Was the primary intent of the rule to curb passive loss abuse by high-income professionals (a coordination goal), or to create a protected class of real estate investors (an extractive goal)?',
    'Analysis of the legislative history of the Tax Reform Act of 1986 and subsequent amendments, compared with empirical data on who benefits from the exemption.',
    'If intent was purely anti-abuse, it's a Rope that has degraded into a Tangled Rope. If intent was to favor a specific industry, it was a Tangled Rope from inception.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_intent_vs_effect, empirical, 'Distinguishing between the rule's stated anti-abuse intent and its actual extractive effect on different taxpayer classes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(26usc469_real_estate_exemption_u4_exp_r1, 1986, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(26us_tr_t1986, 26usc469_real_estate_exemption_u4_exp_r1, theater_ratio, 1986, 0.1).
narrative_ontology:measurement(26us_tr_t2005, 26usc469_real_estate_exemption_u4_exp_r1, theater_ratio, 2005, 0.15).
narrative_ontology:measurement(26us_tr_t2024, 26usc469_real_estate_exemption_u4_exp_r1, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(26us_be_t1986, 26usc469_real_estate_exemption_u4_exp_r1, base_extractiveness, 1986, 0.6).
narrative_ontology:measurement(26us_be_t2005, 26usc469_real_estate_exemption_u4_exp_r1, base_extractiveness, 2005, 0.65).
narrative_ontology:measurement(26us_be_t2024, 26usc469_real_estate_exemption_u4_exp_r1, base_extractiveness, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(26usc469_real_estate_exemption_u4_exp_r1, information_standard).
narrative_ontology:affects_constraint(26usc469_real_estate_exemption_u4_exp_r1, usc26_469_pal_rules_general).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
