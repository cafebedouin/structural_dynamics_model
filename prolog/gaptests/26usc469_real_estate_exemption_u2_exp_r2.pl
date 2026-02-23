% ============================================================================
% CONSTRAINT STORY: 26usc469_real_estate_exemption_u2_exp_r2
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_26usc469_real_estate_exemption_u2_exp_r2, []).

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
 *   constraint_id: 26usc469_real_estate_exemption_u2_exp_r2
 *   human_readable: The Real Estate Professional Exemption (Passive Activity Loss Rules)
 *   domain: economic/legal
 *
 * SUMMARY:
 *   Section 469 of the US tax code generally prevents taxpayers from
 *   deducting passive activity losses against active income. The "(c)(7)"
 *   exemption carves out a special exception for "real estate professionals,"
 *   but defines this status with a strict, two-part test (750 hours of
 *   activity AND more than half of total personal services). This creates a
 *   significant, often insurmountable, barrier for individuals with
 *   high-income W-2 jobs, effectively bifurcating taxpayers into those who
 *   can and cannot access these powerful deductions.
 *
 * KEY AGENTS:
 *   - Hybrid W-2 Investors: Primary target (powerless/trapped) — bears extraction via disallowed losses.
 *   - Full-Time Real Estate Professionals: Primary beneficiary (moderate/mobile) — benefits from the clear qualification path and protected status.
 *   - The IRS: Enforcing institution (institutional/constrained) — administers the rule as written.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(26usc469_real_estate_exemption_u2_exp_r2, 0.68).
domain_priors:suppression_score(26usc469_real_estate_exemption_u2_exp_r2, 0.75).
domain_priors:theater_ratio(26usc469_real_estate_exemption_u2_exp_r2, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_exp_r2, extractiveness, 0.68).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_exp_r2, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_exp_r2, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(26usc469_real_estate_exemption_u2_exp_r2, tangled_rope).
narrative_ontology:human_readable(26usc469_real_estate_exemption_u2_exp_r2, "The Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain(26usc469_real_estate_exemption_u2_exp_r2, "economic/legal").

domain_priors:requires_active_enforcement(26usc469_real_estate_exemption_u2_exp_r2).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(26usc469_real_estate_exemption_u2_exp_r2, full_time_real_estate_professionals).
narrative_ontology:constraint_victim(26usc469_real_estate_exemption_u2_exp_r2, hybrid_w2_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: For an individual with a demanding primary career, the two-part test is an insurmountable barrier. Their real estate losses are trapped as 'passive' and cannot offset their primary income, representing a direct extraction.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r2, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: For someone whose career is real estate, the rule is a clear, predictable coordination mechanism. It defines their professional status for tax purposes and provides a significant, reliable advantage.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r2, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: The IRS must enforce the law as written. It sees both the coordination function (creating a bright-line test to prevent widespread abuse of passive loss deductions) and the extractive reality of its enforcement against a specific class of taxpayers.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r2, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: The analytical view recognizes the dual nature. The rule legitimately coordinates to define a professional class but does so with such a high, rigid barrier that it creates severe, asymmetric extraction.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r2, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(26usc469_real_estate_exemption_u2_exp_r2_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r2, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r2, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(26usc469_real_estate_exemption_u2_exp_r2, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(26usc469_real_estate_exemption_u2_exp_r2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.68) reflects the direct financial cost of disallowed losses, which can amount to tens of thousands of dollars annually per investor. The high suppression (0.75) reflects the near-impossibility for a person with a demanding non-real-estate career to meet the 'more than half of personal services' test without quitting their job, effectively locking them out of the benefit.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the full-time professional, the law is a clear 'Rope' that defines their status and provides a predictable benefit. For the hybrid W-2 investor, it is a 'Snare'—a rule that seems accessible but whose fine print makes it a trap that prevents them from realizing the full economic value of their investments. The analytical 'Tangled Rope' classification captures this duality.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint extracts value from 'Hybrid W-2 Investors' by increasing their tax liability and transfers it to the US Treasury. 'Full-Time Real Estate Professionals' are the structural beneficiaries, as the rule carves out a protected tax status for them, reducing their tax burden and shielding them from competition from high-income professionals who might otherwise enter the market more aggressively.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a pure Snare would miss its genuine (though clumsy) coordination function: preventing a massive tax shelter loophole. Classifying it as a pure Rope would ignore the severe, asymmetric extraction imposed on a specific group. The Tangled Rope classification is essential to correctly model the structure as a rule with a legitimate purpose that is implemented in a highly extractive and suppressive manner.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intent_of_material_participation_test,
    'Was the strict two-part test intended to create a protected class of real estate investors, or was it a blunt instrument to prevent widespread tax sheltering by high-income professionals?',
    'Analysis of the legislative history of the Tax Reform Act of 1986 and subsequent amendments.',
    'If intended as a blunt instrument, it's a Tangled Rope with high collateral damage. If intended to create a protected class, it's a Snare by design for the excluded group.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intent_of_material_participation_test, conceptual, 'Ambiguity of legislative intent behind the strict 'real estate professional' definition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(26usc469_real_estate_exemption_u2_exp_r2, 1986, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(26us_tr_t1986, 26usc469_real_estate_exemption_u2_exp_r2, theater_ratio, 1986, 0.1).
narrative_ontology:measurement(26us_tr_t2005, 26usc469_real_estate_exemption_u2_exp_r2, theater_ratio, 2005, 0.12).
narrative_ontology:measurement(26us_tr_t2024, 26usc469_real_estate_exemption_u2_exp_r2, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(26us_be_t1986, 26usc469_real_estate_exemption_u2_exp_r2, base_extractiveness, 1986, 0.6).
narrative_ontology:measurement(26us_be_t2005, 26usc469_real_estate_exemption_u2_exp_r2, base_extractiveness, 2005, 0.65).
narrative_ontology:measurement(26us_be_t2024, 26usc469_real_estate_exemption_u2_exp_r2, base_extractiveness, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(26usc469_real_estate_exemption_u2_exp_r2, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
