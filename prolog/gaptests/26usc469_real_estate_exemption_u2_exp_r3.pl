% ============================================================================
% CONSTRAINT STORY: 26usc469_real_estate_exemption_u2_exp_r3
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_26usc469_real_estate_exemption_u2_exp_r3, []).

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
 *   constraint_id: 26usc469_real_estate_exemption_u2_exp_r3
 *   human_readable: The Real Estate Professional Exemption (Passive Activity Loss Rules)
 *   domain: economic/legal
 *
 * SUMMARY:
 *   Section 469 of the US tax code generally prevents taxpayers from
 *   deducting passive activity losses against active income. The (c)(7)
 *   exemption carves out a special exception for 'real estate professionals,'
 *   but defines this status with a strict, two-part test (750 hours of
 *   activity AND more than half of all personal services). This creates a
 *   significant, often insurmountable, barrier for individuals with
 *   high-income W-2 jobs, effectively bifurcating taxpayers into those who
 *   can and cannot access these powerful deductions.
 *
 * KEY AGENTS:
 *   - Hybrid W-2 Investors: Primary target (powerless/trapped) — bears extraction via disallowed losses.
 *   - Full-Time Real Estate Professionals: Primary beneficiary (moderate/mobile) — benefits from the clear qualification path and reduced competition for tax-advantaged investments.
 *   - The IRS: Enforcing institution (institutional/constrained) — must administer the complex and contentious rule.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(26usc469_real_estate_exemption_u2_exp_r3, 0.62).
domain_priors:suppression_score(26usc469_real_estate_exemption_u2_exp_r3, 0.75).
domain_priors:theater_ratio(26usc469_real_estate_exemption_u2_exp_r3, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_exp_r3, extractiveness, 0.62).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_exp_r3, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_exp_r3, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(26usc469_real_estate_exemption_u2_exp_r3, tangled_rope).
narrative_ontology:human_readable(26usc469_real_estate_exemption_u2_exp_r3, "The Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain(26usc469_real_estate_exemption_u2_exp_r3, "economic/legal").

domain_priors:requires_active_enforcement(26usc469_real_estate_exemption_u2_exp_r3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(26usc469_real_estate_exemption_u2_exp_r3, full_time_real_estate_professionals).
narrative_ontology:constraint_victim(26usc469_real_estate_exemption_u2_exp_r3, hybrid_w2_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For an individual with a high-income primary career, the tests are nearly impossible to meet, trapping their passive losses and creating a significant financial drag. The only exit is to abandon their primary career, which is often not a viable choice.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r3, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For those whose primary business is real estate, the rule is a clear coordination mechanism. It defines their professional status, provides substantial tax benefits, and creates a protective moat against casual investors, increasing the value of their specialization.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r3, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% The IRS is constrained to enforce the law as written. It sees the rule's coordination function (defining a specific class of taxpayer) but also expends resources on enforcement and litigation related to its asymmetric extractive effects.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r3, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The analyst sees the full structure: a rule with a stated coordination purpose (distinguishing active professionals from passive investors) that simultaneously functions as a highly effective extractive mechanism against a specific class of taxpayer.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r3, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(26usc469_real_estate_exemption_u2_exp_r3_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r3, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r3, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(26usc469_real_estate_exemption_u2_exp_r3, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(26usc469_real_estate_exemption_u2_exp_r3_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.62) reflects the direct financial value transferred from the victim class (in the form of higher taxes) to the general treasury, which indirectly benefits all recipients of government spending but directly benefits the professional class by creating a tax shelter they can uniquely access. The high suppression (0.75) comes from the rigidity of the two-part test, which offers no alternative pathways for a W-2 employee to qualify, regardless of the scale or sophistication of their real estate activities.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the Hybrid W-2 Investor (who sees a Snare blocking them from benefits available to others in the same asset class) and the Full-Time Real Estate Professional (who sees a Rope that legitimizes their profession and provides a clear, if demanding, set of rules for securing tax advantages). The former experiences it as an arbitrary barrier, the latter as a professional standard.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's structure is explicitly directional. It benefits the 'full_time_real_estate_professionals' by granting them exclusive access to deduct unlimited passive losses against other income. It extracts from 'hybrid_w2_investors' by disallowing the same deductions, forcing them to pay higher taxes than they otherwise would. The IRS acts as the enforcement channel for this transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope correctly captures its dual nature. It is not a pure Snare, as it has a legitimate coordinating function: to distinguish between active business participants and passive investors. However, it is not a pure Rope, because the method of coordination (the strict two-part test) creates a severe and asymmetric extractive outcome. This classification avoids the error of seeing it as only a technical rule (Rope) or only a malicious trap (Snare).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rep_status_intent,
    'Is the strict two-part test for 'Real Estate Professional' status a necessary guardrail against tax abuse, or is it an intentionally constructed barrier to protect a specific professional class?',
    'Analysis of legislative history and lobbying records during the law's creation, compared with empirical data on tax avoidance before and after its implementation.',
    'If a necessary guardrail, it's a Tangled Rope with a strong coordination function. If a protective barrier, it leans more towards a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rep_status_intent, empirical, 'Whether the strict REP test is a necessary anti-abuse rule or a form of protectionism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(26usc469_real_estate_exemption_u2_exp_r3, 1986, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(26us_tr_t1986, 26usc469_real_estate_exemption_u2_exp_r3, theater_ratio, 1986, 0.1).
narrative_ontology:measurement(26us_tr_t2005, 26usc469_real_estate_exemption_u2_exp_r3, theater_ratio, 2005, 0.12).
narrative_ontology:measurement(26us_tr_t2024, 26usc469_real_estate_exemption_u2_exp_r3, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(26us_be_t1986, 26usc469_real_estate_exemption_u2_exp_r3, base_extractiveness, 1986, 0.4).
narrative_ontology:measurement(26us_be_t2005, 26usc469_real_estate_exemption_u2_exp_r3, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(26us_be_t2024, 26usc469_real_estate_exemption_u2_exp_r3, base_extractiveness, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(26usc469_real_estate_exemption_u2_exp_r3, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
