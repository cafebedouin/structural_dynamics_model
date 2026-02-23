% ============================================================================
% CONSTRAINT STORY: 26usc469_real_estate_exemption_u4_exp_r4
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-16
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_26usc469_real_estate_exemption_u4_exp_r4, []).

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
 *   constraint_id: 26usc469_real_estate_exemption_u4_exp_r4
 *   human_readable: The Real Estate Professional Exemption (Passive Activity Loss Rules)
 *   domain: economic/legal
 *
 * SUMMARY:
 *   Section 469 of the U.S. Internal Revenue Code restricts the deduction of
 *   losses from passive activities. A key exception, §469(c)(7), allows 'real
 *   estate professionals' to treat rental real estate activities as
 *   non-passive, enabling loss deductions against other income. Qualification
 *   requires passing two strict tests: (1) more than half of the individual's
 *   personal services during the year are performed in real property trades
 *   or businesses, and (2) they perform more than 750 hours of services in
 *   those businesses. This structure effectively creates a bright-line rule
 *   that is nearly impossible for individuals with demanding, high-income W-2
 *   careers to meet, regardless of the scale of their real estate
 *   investments.
 *
 * KEY AGENTS:
 *   - Hybrid W-2 Investors: Primary target (powerless/trapped) — Blocked from deducting legitimate losses due to the strict time-based tests.
 *   - Full-Time Real Estate Professionals: Primary beneficiary (organized/mobile) — Easily meet the tests and receive substantial tax advantages.
 *   - The IRS: Enforcing institution (institutional/constrained) — Must apply the rigid tests as defined by statute.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(26usc469_real_estate_exemption_u4_exp_r4, 0.62).
domain_priors:suppression_score(26usc469_real_estate_exemption_u4_exp_r4, 0.7).
domain_priors:theater_ratio(26usc469_real_estate_exemption_u4_exp_r4, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u4_exp_r4, extractiveness, 0.62).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u4_exp_r4, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u4_exp_r4, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(26usc469_real_estate_exemption_u4_exp_r4, tangled_rope).
narrative_ontology:human_readable(26usc469_real_estate_exemption_u4_exp_r4, "The Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain(26usc469_real_estate_exemption_u4_exp_r4, "economic/legal").

domain_priors:requires_active_enforcement(26usc469_real_estate_exemption_u4_exp_r4).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(26usc469_real_estate_exemption_u4_exp_r4, full_time_real_estate_professionals).
narrative_ontology:constraint_victim(26usc469_real_estate_exemption_u4_exp_r4, hybrid_w2_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of a high-income earner with a primary job, the strict tests are an insurmountable barrier, trapping their real estate losses as 'passive' and making them non-deductible against their main income. It feels like a pure extraction mechanism.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u4_exp_r4, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For someone whose primary business is real estate, the rules are a clear, achievable standard. It coordinates their professional status, providing significant tax benefits and distinguishing them from casual investors. It functions as a pure coordination mechanism.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u4_exp_r4, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% The IRS is constrained to enforce the law as written. It sees the rule's intended coordination function (preventing passive loss tax shelters) but also deals with the enforcement complexity and the clear division it creates between two classes of taxpayers.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u4_exp_r4, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The analyst sees the dual nature of the constraint: a legitimate attempt to define 'material participation' that simultaneously functions as a gatekeeping mechanism, creating a protected class of beneficiaries and a disadvantaged class of victims. This is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u4_exp_r4, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(26usc469_real_estate_exemption_u4_exp_r4_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u4_exp_r4, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u4_exp_r4, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(26usc469_real_estate_exemption_u4_exp_r4, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(26usc469_real_estate_exemption_u4_exp_r4_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.62) reflects the significant financial value of the disallowed losses for the target group. The suppression score (0.70) is high because the two-part test is a rigid, non-negotiable barrier with no alternative compliance path for those with substantial non-real-estate employment.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the full-time professional, the law is a simple Rope that defines their privileged status. For the W-2 investor, it's a Snare that arbitrarily denies them tax parity. The analytical view recognizes both the stated purpose (coordination) and the actual effect (extraction), classifying it as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by one's primary employment. The constraint directs benefits (tax deductions) to those whose primary work is in real estate (beneficiaries) and extracts value (disallowed deductions) from those whose primary work is elsewhere but who also invest significantly in real estate (victims).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a classic example of how a rule intended to solve a coordination problem (distinguishing active from passive investors) can become a tool for asymmetric extraction. Classifying it as a Tangled Rope, rather than a simple Rope (its claimed function) or a pure Snare (its effect on victims), correctly captures its dual nature and prevents mischaracterization. The system's legitimacy rests on its coordination function, while its primary effect is extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    policy_intent_vs_outcome,
    'Was the primary legislative intent of the strict two-part test to prevent abuse by high-income professionals, or to carve out a protected class for the real estate lobby?',
    'Detailed analysis of the legislative history of the Tax Reform Act of 1986, including committee reports and records of lobbying activities.',
    'If the intent was purely anti-abuse, the constraint is a poorly calibrated Rope that has degraded. If the intent was to create a protected class, it was designed as a Tangled Rope from inception.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_intent_vs_outcome, empirical, 'Distinguishes whether the constraint's extractive properties are an unintended side effect or a core design feature.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(26usc469_real_estate_exemption_u4_exp_r4, 1986, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(26us_tr_t1986, 26usc469_real_estate_exemption_u4_exp_r4, theater_ratio, 1986, 0.1).
narrative_ontology:measurement(26us_tr_t2005, 26usc469_real_estate_exemption_u4_exp_r4, theater_ratio, 2005, 0.15).
narrative_ontology:measurement(26us_tr_t2024, 26usc469_real_estate_exemption_u4_exp_r4, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(26us_be_t1986, 26usc469_real_estate_exemption_u4_exp_r4, base_extractiveness, 1986, 0.5).
narrative_ontology:measurement(26us_be_t2005, 26usc469_real_estate_exemption_u4_exp_r4, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(26us_be_t2024, 26usc469_real_estate_exemption_u4_exp_r4, base_extractiveness, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(26usc469_real_estate_exemption_u4_exp_r4, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
