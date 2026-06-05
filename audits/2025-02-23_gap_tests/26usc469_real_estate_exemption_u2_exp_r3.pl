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
 *   Section 469 of the US tax code generally prevents deducting passive
 *   activity losses against active income. The (c)(7) exemption creates a
 *   special exception for 'real estate professionals' who meet a strict
 *   two-part test: performing over 750 hours in real property trades and
 *   spending more than half their total personal services time on them. This
 *   creates an almost insurmountable barrier for individuals with demanding,
 *   high-income W-2 jobs, effectively bifurcating taxpayers into those who
 *   can and cannot access these powerful deductions based on their primary
 *   career.
 *
 * KEY AGENTS:
 *   - Hybrid W-2 Investors: Primary target (powerless/trapped) — bears extraction via disallowed losses.
 *   - Full-Time Real Estate Professionals: Primary beneficiary (moderate/mobile) — benefits from the clear qualification path and tax advantages.
 *   - The IRS: Enforcing institution (institutional/constrained) — administers the rule as written.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(26usc469_real_estate_exemption_u2_exp_r3, 0.68).
domain_priors:suppression_score(26usc469_real_estate_exemption_u2_exp_r3, 0.72).
domain_priors:theater_ratio(26usc469_real_estate_exemption_u2_exp_r3, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_exp_r3, extractiveness, 0.68).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_exp_r3, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_exp_r3, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(26usc469_real_estate_exemption_u2_exp_r3, tangled_rope).
narrative_ontology:human_readable(26usc469_real_estate_exemption_u2_exp_r3, "The Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain(26usc469_real_estate_exemption_u2_exp_r3, "economic/legal").

domain_priors:requires_active_enforcement(26usc469_real_estate_exemption_u2_exp_r3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(26usc469_real_estate_exemption_u2_exp_r3, full_time_real_estate_professionals).
narrative_ontology:constraint_beneficiary(26usc469_real_estate_exemption_u2_exp_r3, us_treasury).
narrative_ontology:constraint_victim(26usc469_real_estate_exemption_u2_exp_r3, hybrid_w2_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For the high-income professional investing in real estate on the side, the rule is a pure Snare. It disallows legitimate economic losses against their primary income, creating a significant tax burden with no corresponding benefit. The 'exit' option of quitting their job to qualify is prohibitively costly.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r3, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For those whose primary business is real estate, the rule is a clear Rope. It provides a well-defined (if strict) pathway to deduct losses, coordinating their professional status with favorable tax treatment and creating a competitive moat against casual investors.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r3, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% The enforcing institution sees a Tangled Rope. The rule serves a legitimate coordination function: defining a professional class to prevent widespread tax sheltering. However, its enforcement results in asymmetric extraction from a specific group of taxpayers, making it more than a simple coordination mechanism.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r3, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The analytical view confirms the Tangled Rope classification. The constraint possesses both a genuine coordination function (distinguishing active vs. passive participation) and a highly extractive component that asymmetrically burdens one class of economic actor.
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
 *   The high extractiveness (0.68) reflects the direct financial cost of disallowed losses, which can amount to tens of thousands of dollars annually for an investor. The high suppression (0.72) comes from the rigidity of the legal test; there is no alternative path or partial compliance. It is a binary, coercive rule enforced by a state actor.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the Hybrid Investor (Snare) and the Full-Time Professional (Rope). The investor experiences a purely extractive rule that increases their tax liability without providing any service. The professional experiences a beneficial coordination rule that defines their status and provides a clear economic advantage, effectively protecting their tax strategy from competition by high-earning outsiders.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint directs value away from Hybrid W-2 Investors, whose tax bills are higher due to disallowed losses. This value is captured by the US Treasury (an indirect beneficiary) and indirectly benefits Full-Time Real Estate Professionals, who face less competition from tax-advantaged capital from other high-income sectors.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a pure Snare would miss its genuine (though narrow) coordination function of defining a professional class for tax purposes. Classifying it as a Rope would ignore the severe, asymmetric extraction imposed on a clearly defined victim group. The Tangled Rope classification correctly identifies that a legitimate policy goal is achieved through a mechanism with highly extractive and coercive side effects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rep_exemption_intent,
    'Was the strictness of the 'Real Estate Professional' test primarily intended to create a professional carve-out (coordination) or to prevent high-income W-2 earners from sheltering income (extraction)?',
    'Analysis of the 1993 legislative history and subsequent economic impact studies on tax revenue from disallowed passive losses.',
    'If primarily for coordination, it leans more towards Rope. If primarily for revenue generation via extraction, it's a clear Snare from a systemic view.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rep_exemption_intent, conceptual, 'The core ambiguity in the rule's purpose: defining a professional standard versus implementing an anti-abuse provision targeting a specific taxpayer profile.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(26usc469_real_estate_exemption_u2_exp_r3, 1993, 2043).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(26us_tr_t1993, 26usc469_real_estate_exemption_u2_exp_r3, theater_ratio, 1993, 0.1).
narrative_ontology:measurement(26us_tr_t2010, 26usc469_real_estate_exemption_u2_exp_r3, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(26us_tr_t2024, 26usc469_real_estate_exemption_u2_exp_r3, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(26us_be_t1993, 26usc469_real_estate_exemption_u2_exp_r3, base_extractiveness, 1993, 0.6).
narrative_ontology:measurement(26us_be_t2010, 26usc469_real_estate_exemption_u2_exp_r3, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(26us_be_t2024, 26usc469_real_estate_exemption_u2_exp_r3, base_extractiveness, 2024, 0.68).


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
