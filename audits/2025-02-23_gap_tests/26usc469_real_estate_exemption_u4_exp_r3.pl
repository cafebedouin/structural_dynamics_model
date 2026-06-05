% ============================================================================
% CONSTRAINT STORY: 26usc469_real_estate_exemption_u4_exp_r3
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_26usc469_real_estate_exemption_u4_exp_r3, []).

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
 *   constraint_id: 26usc469_real_estate_exemption_u4_exp_r3
 *   human_readable: The Real Estate Professional Exemption (Passive Activity Loss Rules)
 *   domain: economic/legal
 *
 * SUMMARY:
 *   Section 469 of the U.S. Internal Revenue Code establishes rules to limit
 *   deductions from passive activities. A key exception, §469(c)(7), allows
 *   'real estate professionals' to deduct unlimited rental losses against
 *   other income. Qualification requires passing a strict two-part test: (1)
 *   more than half of the individual’s personal services during the year are
 *   performed in real property trades or businesses, and (2) they perform
 *   more than 750 hours of services in those businesses. This structure
 *   creates a sharp divide, heavily favoring those whose sole or primary
 *   profession is real estate, while effectively barring individuals with
 *   demanding, high-income careers in other fields from accessing the same
 *   tax benefits, even if their real estate activities are substantial.
 *
 * KEY AGENTS:
 *   - Hybrid W-2 Investors: Primary target (powerless/trapped) - High-income professionals in non-real estate fields who also invest in real estate.
 *   - Full-Time Real Estate Professionals: Primary beneficiary (moderate/mobile) - Individuals whose careers are centered on real estate and can easily meet the test.
 *   - The IRS: Enforcing institution (institutional/constrained) - The agency tasked with applying and auditing compliance with the rule.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(26usc469_real_estate_exemption_u4_exp_r3, 0.68).
domain_priors:suppression_score(26usc469_real_estate_exemption_u4_exp_r3, 0.75).
domain_priors:theater_ratio(26usc469_real_estate_exemption_u4_exp_r3, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u4_exp_r3, extractiveness, 0.68).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u4_exp_r3, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u4_exp_r3, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(26usc469_real_estate_exemption_u4_exp_r3, tangled_rope).
narrative_ontology:human_readable(26usc469_real_estate_exemption_u4_exp_r3, "The Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain(26usc469_real_estate_exemption_u4_exp_r3, "economic/legal").

domain_priors:requires_active_enforcement(26usc469_real_estate_exemption_u4_exp_r3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(26usc469_real_estate_exemption_u4_exp_r3, full_time_real_estate_professionals).
narrative_ontology:constraint_victim(26usc469_real_estate_exemption_u4_exp_r3, hybrid_w2_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of a high-income earner in another field, the rule is a trap. Their real estate losses are real, but the path to deducting them is blocked by criteria that would require them to abandon their primary career. The constraint extracts value by disallowing legitimate deductions.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u4_exp_r3, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For a full-time real estate professional, the rule is a clear coordination mechanism. It defines their professional status, distinguishes them from passive hobbyists, and provides a significant, predictable tax advantage that is core to their business model. It is a clear and beneficial rule of the game.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u4_exp_r3, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% The IRS is constrained to enforce the law as written. They see both its coordination function (preventing widespread passive loss tax shelters) and the asymmetric outcomes it produces. It requires active enforcement and creates clear winners and losers based on a rigid, quantitative test.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u4_exp_r3, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The analytical view recognizes the dual nature of the constraint. It serves a legitimate coordination purpose by attempting to separate active business participants from passive investors. However, its implementation via a strict, high-barrier test creates a highly extractive system that benefits one class of economic actor at the direct expense of another.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u4_exp_r3, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(26usc469_real_estate_exemption_u4_exp_r3_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u4_exp_r3, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u4_exp_r3, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(26usc469_real_estate_exemption_u4_exp_r3, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(26usc469_real_estate_exemption_u4_exp_r3_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.68) is high because the financial value of the disallowed deductions is substantial, representing a direct wealth transfer in the tax system. The suppression score (0.75) is high due to the rigidity of the two-part test; for a W-2 professional, meeting the '>50% of personal services' prong is often impossible without quitting their job, making the alternative prohibitively costly.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. The beneficiary sees a clear, beneficial rule defining their professional status (Rope). The victim experiences an arbitrary and insurmountable barrier that denies them legitimate deductions based on their primary career choice, not the nature of their real estate business (Snare). The enforcing agency and the analyst see the system's dual function: it does coordinate activity (defining a professional class) but does so in a way that is highly coercive and extractive (Tangled Rope).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is unambiguous. The constraint directs tax benefits *to* full-time real estate professionals by allowing them to shelter other income with rental losses. It extracts value *from* hybrid W-2 investors by explicitly denying them the same capability, forcing them to carry forward passive losses they may never be able to use.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope prevents two errors. It avoids mislabeling it as a pure anti-abuse rule (Rope), which would ignore the massive extractive asymmetry. It also avoids calling it a pure Snare, which would ignore its genuine, if flawed, coordination function of attempting to distinguish active from passive participation. The Tangled Rope classification correctly captures that a stated coordination goal is being achieved through a highly extractive and suppressive mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legislative_intent_vs_capture,
    'Was the strict two-part test primarily intended as a robust guard against tax shelters, or was it the result of industry lobbying to protect the tax advantages of full-time real estate professionals?',
    'Detailed analysis of the legislative history of the Tax Reform Act of 1986, including testimony and lobbying records from real estate industry groups.',
    'If primarily an anti-shelter mechanism, the coordination function is stronger, leaning towards a harsh Rope. If primarily industry capture, the extractive function is dominant, confirming it as a Snare embedded in a legal framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_intent_vs_capture, empirical, 'Distinguishing between the stated anti-abuse purpose and potential industry capture in the law's design.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(26usc469_real_estate_exemption_u4_exp_r3, 1986, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(26us_tr_t1986, 26usc469_real_estate_exemption_u4_exp_r3, theater_ratio, 1986, 0.1).
narrative_ontology:measurement(26us_tr_t2005, 26usc469_real_estate_exemption_u4_exp_r3, theater_ratio, 2005, 0.12).
narrative_ontology:measurement(26us_tr_t2024, 26usc469_real_estate_exemption_u4_exp_r3, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(26us_be_t1986, 26usc469_real_estate_exemption_u4_exp_r3, base_extractiveness, 1986, 0.6).
narrative_ontology:measurement(26us_be_t2005, 26usc469_real_estate_exemption_u4_exp_r3, base_extractiveness, 2005, 0.65).
narrative_ontology:measurement(26us_be_t2024, 26usc469_real_estate_exemption_u4_exp_r3, base_extractiveness, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(26usc469_real_estate_exemption_u4_exp_r3, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
