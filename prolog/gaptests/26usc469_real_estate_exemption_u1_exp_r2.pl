% ============================================================================
% CONSTRAINT STORY: 26usc469_real_estate_exemption_u1_exp_r2
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_26usc469_real_estate_exemption_u1_exp_r2, []).

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
 *   constraint_id: 26usc469_real_estate_exemption_u1_exp_r2
 *   human_readable: The Real Estate Professional Exemption (Passive Activity Loss Rules)
 *   domain: economic/legal
 *
 * SUMMARY:
 *   Section 469 of the US tax code generally disallows the deduction of
 *   passive activity losses against active income. The (c)(7) exemption for
 *   'real estate professionals' provides a powerful exception, but defines
 *   this status with a strict two-part test: the taxpayer must spend more
 *   than 750 hours and more than half of their total personal services in
 *   real property trades or businesses. This structure creates a sharp
 *   divide, primarily benefiting those in full-time real estate careers while
 *   systematically excluding individuals with demanding, high-income W-2 jobs
 *   from accessing the same tax advantages, even with substantial real estate
 *   portfolios.
 *
 * KEY AGENTS:
 *   - Hybrid W-2 Investors: Primary target (powerless/trapped) - High-income professionals who invest in real estate but cannot meet the hours test.
 *   - Full-Time Real Estate Professionals: Primary beneficiary (moderate/mobile) - Individuals whose main occupation is in real estate and can easily meet the test.
 *   - The IRS: Enforcing institution (institutional/constrained) - The agency tasked with auditing and enforcing compliance with the rule.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(26usc469_real_estate_exemption_u1_exp_r2, 0.68).
domain_priors:suppression_score(26usc469_real_estate_exemption_u1_exp_r2, 0.85).
domain_priors:theater_ratio(26usc469_real_estate_exemption_u1_exp_r2, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u1_exp_r2, extractiveness, 0.68).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u1_exp_r2, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u1_exp_r2, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(26usc469_real_estate_exemption_u1_exp_r2, tangled_rope).
narrative_ontology:human_readable(26usc469_real_estate_exemption_u1_exp_r2, "The Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain(26usc469_real_estate_exemption_u1_exp_r2, "economic/legal").

domain_priors:requires_active_enforcement(26usc469_real_estate_exemption_u1_exp_r2).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(26usc469_real_estate_exemption_u1_exp_r2, full_time_real_estate_professionals).
narrative_ontology:constraint_victim(26usc469_real_estate_exemption_u1_exp_r2, hybrid_w2_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: For the investor with a demanding primary job, the hours test is an insurmountable barrier, trapping their passive losses and creating a significant tax burden. The only exit is to abandon their primary career.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u1_exp_r2, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: For those whose primary business is real estate, the rule is a clear coordination mechanism that defines their professional status and enables their business model by allowing losses to offset other income.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u1_exp_r2, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: The IRS is constrained to enforce the law as written. It sees both the coordination function (defining a taxpayer class) and the extractive consequences and high enforcement cost (auditing hours logs).
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u1_exp_r2, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: The analyst sees the full structure: a rule with a legitimate coordination purpose (defining a professional) that is implemented in a way that creates a severe, asymmetric extraction from one group to benefit another.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u1_exp_r2, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(26usc469_real_estate_exemption_u1_exp_r2_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u1_exp_r2, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u1_exp_r2, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(26usc469_real_estate_exemption_u1_exp_r2, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(26usc469_real_estate_exemption_u1_exp_r2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.68) reflects the direct financial cost to W-2 investors who cannot deduct legitimate losses, resulting in a higher tax liability. The high suppression (0.85) stems from the fact that the rule is federal law; the only 'alternative' for the target group is to quit a lucrative primary career, which is not a viable option, making them effectively trapped.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the W-2 investor, who sees a Snare (an arbitrary, insurmountable barrier), and the full-time professional, who sees a Rope (a clear, functional rule defining their professional tax status). The former experiences it as pure extraction, while the latter experiences it as pure coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's benefits flow directly to full-time real estate professionals, who can structure their finances around the ability to deduct passive losses. The costs are borne by hybrid investors, who are blocked from this benefit specifically because of their commitment to another profession, effectively subsidizing the tax base.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope correctly captures its dual nature. It is not a pure Snare, as it serves a legitimate purpose of defining a professional class to prevent widespread tax shelter abuse. However, it is not a pure Rope, because the implementation creates a severe and predictable extractive asymmetry. The Tangled Rope classification avoids mislabeling a targeted, extractive policy as a neutral coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rep_status_policy_intent,
    'Is the strict 'two-part test' intended to prevent abuse by high-income professionals (a coordination goal) or to protect a professional class while creating a reliable tax base from W-2 earners (an extractive goal)?',
    'Analysis of legislative history from the Tax Reform Act of 1986 and subsequent economic impact studies on both affected groups.',
    'If primarily for abuse prevention, it's a Rope with high friction. If primarily for revenue and class protection, it's a Snare masquerading as a professional standard.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rep_status_policy_intent, conceptual, 'The core ambiguity in the policy goal of the Real Estate Professional status test.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(26usc469_real_estate_exemption_u1_exp_r2, 1986, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(26us_tr_t1986, 26usc469_real_estate_exemption_u1_exp_r2, theater_ratio, 1986, 0.1).
narrative_ontology:measurement(26us_tr_t2006, 26usc469_real_estate_exemption_u1_exp_r2, theater_ratio, 2006, 0.12).
narrative_ontology:measurement(26us_tr_t2026, 26usc469_real_estate_exemption_u1_exp_r2, theater_ratio, 2026, 0.15).

% Extraction over time
narrative_ontology:measurement(26us_be_t1986, 26usc469_real_estate_exemption_u1_exp_r2, base_extractiveness, 1986, 0.4).
narrative_ontology:measurement(26us_be_t2006, 26usc469_real_estate_exemption_u1_exp_r2, base_extractiveness, 2006, 0.6).
narrative_ontology:measurement(26us_be_t2026, 26usc469_real_estate_exemption_u1_exp_r2, base_extractiveness, 2026, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(26usc469_real_estate_exemption_u1_exp_r2, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
