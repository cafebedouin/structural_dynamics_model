% ============================================================================
% CONSTRAINT STORY: 26usc469_real_estate_exemption_u2_sed_r5
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_26usc469_real_estate_exemption_u2_sed_r5, []).

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
 *   constraint_id: 26usc469_real_estate_exemption_u2_sed_r5
 *   human_readable: The Real Estate Professional Exemption (Passive Activity Loss Rules)
 *   domain: economic/legal
 *
 * SUMMARY:
 *   Section 469(c)(7) of the U.S. tax code creates an exemption to passive
 *   activity loss rules for 'real estate professionals.' However, its strict
 *   two-part test (750 hours AND more than half of personal services)
 *   effectively excludes individuals with demanding, high-income W-2 jobs.
 *   This creates a structural bifurcation, granting significant tax
 *   advantages to one class of investors while denying them to another, even
 *   if their real estate activities are substantial.
 *
 * KEY AGENTS:
 *   - Hybrid W-2 Investors: Primary target (powerless/trapped) - Disallowed from deducting losses against their primary income.
 *   - Full-Time Real Estate Professionals: Primary beneficiary (moderate/mobile) - Benefit from the ability to deduct unlimited passive losses.
 *   - The IRS: Enforcing institution (institutional/constrained) - Administers the rule as written.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(26usc469_real_estate_exemption_u2_sed_r5, 0.6).
domain_priors:suppression_score(26usc469_real_estate_exemption_u2_sed_r5, 0.75).
domain_priors:theater_ratio(26usc469_real_estate_exemption_u2_sed_r5, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_sed_r5, extractiveness, 0.6).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_sed_r5, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_sed_r5, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(26usc469_real_estate_exemption_u2_sed_r5, tangled_rope).
narrative_ontology:human_readable(26usc469_real_estate_exemption_u2_sed_r5, "The Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain(26usc469_real_estate_exemption_u2_sed_r5, "economic/legal").

domain_priors:requires_active_enforcement(26usc469_real_estate_exemption_u2_sed_r5).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(26usc469_real_estate_exemption_u2_sed_r5, full_time_real_estate_professionals).
narrative_ontology:constraint_victim(26usc469_real_estate_exemption_u2_sed_r5, hybrid_w2_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For an investor with a demanding primary job, the 750-hour and 'more than half' tests are an insurmountable wall, making this a pure extraction mechanism that denies them deductions available to others.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_sed_r5, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For those whose primary business is real estate, the rule is a clear, beneficial coordination mechanism that defines their professional status and unlocks significant tax advantages.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_sed_r5, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% The IRS sees a rule with a clear coordination purpose (defining a professional class) that requires active, complex enforcement and creates clear asymmetries, fitting the Tangled Rope definition.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_sed_r5, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The analyst sees the dual function: a legitimate policy goal to separate active from passive income, but implemented in a way that creates a privileged class and extracts from another based on employment structure.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_sed_r5, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(26usc469_real_estate_exemption_u2_sed_r5_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_sed_r5, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_sed_r5, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(26usc469_real_estate_exemption_u2_sed_r5, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(26usc469_real_estate_exemption_u2_sed_r5_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.60) is high, reflecting the substantial tax liability difference between those who qualify and those who don't. Suppression (0.75) is high because this is federal law; the only 'exit' is a career change, which is a prohibitive cost. The theater ratio is low as the rule is actively and functionally enforced by the IRS.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the Full-Time Professional, who sees a clear 'Rope' that legitimizes their business activity, and the Hybrid W-2 Investor, who sees a 'Snare' that arbitrarily denies them tax parity based on their primary source of income. The institutional and analytical views converge on 'Tangled Rope,' recognizing both the coordination function and the extractive asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries are clearly defined by the statute: full-time real estate professionals who meet the tests. The victims are a class of investors, particularly those with high W-2 income, who are structurally barred from qualifying. The constraint extracts value from the victims (in the form of higher taxes paid) and transfers it to the beneficiaries (in the form of tax deductions and thus higher net returns).
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope correctly avoids mislabeling it as a pure Snare. There is a non-trivial coordination function: distinguishing between active and passive participation in the real estate economy. However, the implementation is highly extractive, creating a clear perspectival gap and justifying the high epsilon score. It is not a pure Rope because the benefits are not symmetric and are predicated on a structural barrier for a specific group.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    policy_intent_vs_outcome,
    'Was the high barrier for W-2 earners an intended feature to limit the deduction's scope, or an unintended consequence of a rigid 'professional' status definition?',
    'Analysis of the 1993 legislative history and subsequent economic impact studies on taxpayer behavior and wealth concentration.',
    'If intended to gatekeep, it's a Snare by design. If an unintended flaw, it's a Tangled Rope that could be reformed into a more equitable Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_intent_vs_outcome, empirical, 'Distinguishing between intended policy design and unintended consequences of the strict qualification tests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(26usc469_real_estate_exemption_u2_sed_r5, 1993, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(26us_tr_t1993, 26usc469_real_estate_exemption_u2_sed_r5, theater_ratio, 1993, 0.1).
narrative_ontology:measurement(26us_tr_t2008, 26usc469_real_estate_exemption_u2_sed_r5, theater_ratio, 2008, 0.12).
narrative_ontology:measurement(26us_tr_t2024, 26usc469_real_estate_exemption_u2_sed_r5, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(26us_be_t1993, 26usc469_real_estate_exemption_u2_sed_r5, base_extractiveness, 1993, 0.45).
narrative_ontology:measurement(26us_be_t2008, 26usc469_real_estate_exemption_u2_sed_r5, base_extractiveness, 2008, 0.55).
narrative_ontology:measurement(26us_be_t2024, 26usc469_real_estate_exemption_u2_sed_r5, base_extractiveness, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(26usc469_real_estate_exemption_u2_sed_r5, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
