% ============================================================================
% CONSTRAINT STORY: 26usc469_real_estate_exemption_u4_exp_r2
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_26usc469_real_estate_exemption_u4_exp_r2, []).

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
 *   constraint_id: 26usc469_real_estate_exemption_u4_exp_r2
 *   human_readable: The Real Estate Professional Exemption (Passive Activity Loss Rules)
 *   domain: economic/legal
 *
 * SUMMARY:
 *   Section 469 of the U.S. Internal Revenue Code establishes 'passive
 *   activity loss' (PAL) rules, generally preventing taxpayers from using
 *   losses from passive activities (like rental real estate) to offset active
 *   income (like a salary). The (c)(7) exemption creates a carve-out for
 *   'real estate professionals.' To qualify, an individual must pass two
 *   strict tests: (1) spend more than 750 hours in real property trades or
 *   businesses, AND (2) perform more than half of their total personal
 *   services in those trades. This dual requirement creates an almost
 *   insurmountable barrier for individuals with demanding, high-income W-2
 *   jobs, effectively bifurcating taxpayers into a class that can fully
 *   deduct real estate losses and a class that cannot.
 *
 * KEY AGENTS:
 *   - Hybrid W-2 Investors: Primary target (powerless/trapped) — High-income professionals who invest in real estate but cannot meet the time requirements, thus bearing the cost of disallowed losses.
 *   - Full-Time Real Estate Professionals: Primary beneficiary (moderate/mobile) — Individuals and entities whose primary work is in real estate, who easily meet the tests and benefit from the tax deductions.
 *   - The IRS: Enforcing institution (institutional/constrained) — Administers and audits compliance with the rule as written by Congress.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(26usc469_real_estate_exemption_u4_exp_r2, 0.68).
domain_priors:suppression_score(26usc469_real_estate_exemption_u4_exp_r2, 0.75).
domain_priors:theater_ratio(26usc469_real_estate_exemption_u4_exp_r2, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u4_exp_r2, extractiveness, 0.68).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u4_exp_r2, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u4_exp_r2, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(26usc469_real_estate_exemption_u4_exp_r2, tangled_rope).
narrative_ontology:human_readable(26usc469_real_estate_exemption_u4_exp_r2, "The Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain(26usc469_real_estate_exemption_u4_exp_r2, "economic/legal").

domain_priors:requires_active_enforcement(26usc469_real_estate_exemption_u4_exp_r2).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(26usc469_real_estate_exemption_u4_exp_r2, full_time_real_estate_professionals).
narrative_ontology:constraint_victim(26usc469_real_estate_exemption_u4_exp_r2, hybrid_w2_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of a high-income earner with a primary job, the rule is a trap. They are encouraged to invest in real estate but are denied the primary tax benefit, with the only exit being to abandon their primary career.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u4_exp_r2, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For those whose primary business is real estate, the rule is a clear coordination mechanism. It defines their professional status, provides significant tax advantages, and reduces competition from high-earning amateurs.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u4_exp_r2, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% The IRS is constrained to enforce the law as written. It sees both the coordination function (defining a class of taxpayer) and the extractive function (collecting higher taxes from the non-qualifying group).
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u4_exp_r2, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The analyst sees the dual nature: a legitimate attempt to curb passive tax shelters that also functions as a powerful extractive tool creating a protected class of investor.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u4_exp_r2, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(26usc469_real_estate_exemption_u4_exp_r2_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u4_exp_r2, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u4_exp_r2, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(26usc469_real_estate_exemption_u4_exp_r2, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(26usc469_real_estate_exemption_u4_exp_r2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.68) is high because the disallowed losses can represent tens or hundreds of thousands of dollars in increased tax liability annually for the target group. The suppression score (0.75) is also high because the primary alternative—quitting a high-paying career to meet the 'more than half' test—is not a viable option for most, effectively locking them out of the benefit.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the Hybrid W-2 Investor, who experiences the rule as a Snare that punishes them for having a primary career, and the Full-Time Real Estate Professional, who sees it as a legitimate Rope that defines their professional status and provides fair tax treatment for their industry. The former sees an arbitrary barrier; the latter sees a necessary distinction.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's structure directs tax benefits (the ability to deduct unlimited passive losses) towards the beneficiary group (full-time professionals) by imposing a high, activity-based qualification cost on the victim group (hybrid investors). The extraction occurs when the victim group's paper losses are disallowed, resulting in a higher tax burden, which effectively subsidizes the tax system that provides benefits to the other group.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope correctly captures its dual nature. It is not a pure Snare, as it has a legitimate coordination purpose: distinguishing between active business participants and passive investors to prevent tax shelters. However, it is not a pure Rope, because the specific implementation creates a severe, asymmetric extractive outcome. The Tangled Rope classification avoids mislabeling a complex regulation with both functional and predatory characteristics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    policy_intent_vs_effect,
    'Was the high barrier for W-2 earners an intended feature to prevent tax shelters, or an unintended consequence that created a privileged class of investors?',
    'Analysis of legislative history from the Tax Reform Act of 1986 and subsequent economic impact studies on wealth concentration in the real estate sector.',
    'If the barrier was intended to be this high, the constraint is closer to a pure Snare by design. If it was an unforeseen side effect of a coordination attempt, it is a classic Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_intent_vs_effect, empirical, 'Distinguishing between the intended anti-shelter purpose and the actual effect of creating a protected investor class.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(26usc469_real_estate_exemption_u4_exp_r2, 1986, 2046).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(26us_tr_t1986, 26usc469_real_estate_exemption_u4_exp_r2, theater_ratio, 1986, 0.1).
narrative_ontology:measurement(26us_tr_t2010, 26usc469_real_estate_exemption_u4_exp_r2, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(26us_tr_t2040, 26usc469_real_estate_exemption_u4_exp_r2, theater_ratio, 2040, 0.2).

% Extraction over time
narrative_ontology:measurement(26us_be_t1986, 26usc469_real_estate_exemption_u4_exp_r2, base_extractiveness, 1986, 0.6).
narrative_ontology:measurement(26us_be_t2010, 26usc469_real_estate_exemption_u4_exp_r2, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(26us_be_t2040, 26usc469_real_estate_exemption_u4_exp_r2, base_extractiveness, 2040, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(26usc469_real_estate_exemption_u4_exp_r2, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
