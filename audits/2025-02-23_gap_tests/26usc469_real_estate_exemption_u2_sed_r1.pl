% ============================================================================
% CONSTRAINT STORY: 26usc469_real_estate_exemption_u2_sed_r1
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_26usc469_real_estate_exemption_u2_sed_r1, []).

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
 *   constraint_id: 26usc469_real_estate_exemption_u2_sed_r1
 *   human_readable: The Real Estate Professional Exemption (Passive Activity Loss Rules)
 *   domain: economic/legal
 *
 * SUMMARY:
 *   Section 469 of the US tax code generally disallows deducting passive
 *   activity losses against active income. The (c)(7) exemption for 'real
 *   estate professionals' creates a powerful exception, but its strict
 *   two-part test (750 hours AND more than half of personal services in real
 *   property trades) effectively creates a protected class. This structure
 *   prevents individuals with demanding, high-income W-2 jobs from
 *   qualifying, forcing them to carry forward passive losses while full-time
 *   real estate investors can use them to offset other income.
 *
 * KEY AGENTS:
 *   - Hybrid W-2 Investors: Primary target (powerless/trapped) — bears extraction via disallowed losses.
 *   - Full-Time Real Estate Professionals: Primary beneficiary (moderate/mobile) — benefits from the clear qualification path and ability to deduct losses.
 *   - The IRS: Enforcing institution (institutional/constrained) — administers the rule as written.
 *   - Tax Policy Analysts: Analytical observers — see the dual coordination/extraction function.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(26usc469_real_estate_exemption_u2_sed_r1, 0.55).
domain_priors:suppression_score(26usc469_real_estate_exemption_u2_sed_r1, 0.8).
domain_priors:theater_ratio(26usc469_real_estate_exemption_u2_sed_r1, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_sed_r1, extractiveness, 0.55).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_sed_r1, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_sed_r1, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(26usc469_real_estate_exemption_u2_sed_r1, tangled_rope).
narrative_ontology:human_readable(26usc469_real_estate_exemption_u2_sed_r1, "The Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain(26usc469_real_estate_exemption_u2_sed_r1, "economic/legal").

domain_priors:requires_active_enforcement(26usc469_real_estate_exemption_u2_sed_r1).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(26usc469_real_estate_exemption_u2_sed_r1, full_time_real_estate_professionals).
narrative_ontology:constraint_victim(26usc469_real_estate_exemption_u2_sed_r1, hybrid_w2_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For an individual with a high-income primary job, the two-part test is an insurmountable barrier, making their real estate losses non-deductible against their main income. The rule feels purely extractive and arbitrary.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_sed_r1, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For someone whose primary business is real estate, the rule is a clear, albeit strict, coordination mechanism. It defines their professional status and enables their business model by allowing loss deductions.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_sed_r1, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% The IRS must enforce the law as written. It sees both the coordination function (defining a taxpayer class) and the extractive function (collecting higher taxes from those who fail the test).
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_sed_r1, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The analyst sees the full structure: a rule that coordinates by defining 'professional' status while simultaneously extracting value from a different class of investors who cannot meet the stringent requirements.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_sed_r1, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(26usc469_real_estate_exemption_u2_sed_r1_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_sed_r1, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_sed_r1, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(26usc469_real_estate_exemption_u2_sed_r1, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(26usc469_real_estate_exemption_u2_sed_r1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness of 0.55 reflects the significant tax benefit transferred from the 'non-professional' to the 'professional' class (and the government). The suppression score of 0.80 is high because this is federal law, enforced by the IRS with significant penalties; there are no legal alternatives to bypass this rule.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the Hybrid W-2 Investor, who sees a Snare that arbitrarily blocks legitimate deductions, and the Full-Time Real Estate Professional, who sees a Rope that provides clear, fair rules for their industry. The former experiences pure extraction, while the latter experiences pure coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is clear: the constraint extracts from investors whose primary work is outside real estate and transfers that benefit (in the form of tax deductions) to those whose primary work is inside real estate. The 'hybrid' investors are the victims, while the 'full-time' professionals are the beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope correctly avoids mislabeling it as a pure Snare or a simple Rope. It acknowledges the legitimate policy goal of distinguishing between active and passive investment (the coordination function) while also capturing the highly asymmetric and extractive outcome for a well-defined group of taxpayers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reps_policy_intent,
    'Was the high barrier for W-2 earners an intentional feature to protect a professional class and raise revenue, or an unintended consequence of a bright-line rule?',
    'Analysis of legislative history from the Tax Reform Act of 1986 and subsequent economic impact studies on taxpayer behavior.',
    'If intentional, it's a classic Tangled Rope designed for asymmetric benefit. If unintended, it might be a candidate for reform, though its function remains a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reps_policy_intent, empirical, 'The ambiguity of legislative intent behind the strict 'Real Estate Professional' test.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(26usc469_real_estate_exemption_u2_sed_r1, 1986, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(26us_tr_t1986, 26usc469_real_estate_exemption_u2_sed_r1, theater_ratio, 1986, 0.1).
narrative_ontology:measurement(26us_tr_t2005, 26usc469_real_estate_exemption_u2_sed_r1, theater_ratio, 2005, 0.12).
narrative_ontology:measurement(26us_tr_t2024, 26usc469_real_estate_exemption_u2_sed_r1, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(26us_be_t1986, 26usc469_real_estate_exemption_u2_sed_r1, base_extractiveness, 1986, 0.5).
narrative_ontology:measurement(26us_be_t2005, 26usc469_real_estate_exemption_u2_sed_r1, base_extractiveness, 2005, 0.52).
narrative_ontology:measurement(26us_be_t2024, 26usc469_real_estate_exemption_u2_sed_r1, base_extractiveness, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(26usc469_real_estate_exemption_u2_sed_r1, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
