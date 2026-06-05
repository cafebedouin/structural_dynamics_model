% ============================================================================
% CONSTRAINT STORY: 26usc469_real_estate_exemption_u2_sed_r4
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_26usc469_real_estate_exemption_u2_sed_r4, []).

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
 *   constraint_id: 26usc469_real_estate_exemption_u2_sed_r4
 *   human_readable: The Real Estate Professional Exemption (Passive Activity Loss Rules)
 *   domain: economic/legal
 *
 * SUMMARY:
 *   Section 469 of the US tax code, specifically the exception for 'real
 *   estate professionals,' creates a sharp divide between investors. While
 *   ostensibly designed to separate active participants from passive
 *   investors, its strict, time-based qualification tests (750 hours AND more
 *   than half of personal services) make it nearly impossible for individuals
 *   with demanding W-2 careers to qualify. This results in a system where one
 *   group can deduct unlimited losses against other income, while another
 *   group, often with identical investment properties, cannot. The constraint
 *   is the legal structure of this two-part test.
 *
 * KEY AGENTS:
 *   - Hybrid W-2 Investors: Primary target (powerless/trapped) — bears extraction via disallowed losses and resulting higher tax liability.
 *   - Full-Time Real Estate Professionals: Primary beneficiary (moderate/mobile) — benefits from the ability to deduct passive losses, a significant financial advantage.
 *   - The IRS: Enforcing institution (institutional/constrained) — must apply the bright-line rule as written.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(26usc469_real_estate_exemption_u2_sed_r4, 0.68).
domain_priors:suppression_score(26usc469_real_estate_exemption_u2_sed_r4, 0.72).
domain_priors:theater_ratio(26usc469_real_estate_exemption_u2_sed_r4, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_sed_r4, extractiveness, 0.68).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_sed_r4, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_sed_r4, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(26usc469_real_estate_exemption_u2_sed_r4, tangled_rope).
narrative_ontology:human_readable(26usc469_real_estate_exemption_u2_sed_r4, "The Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain(26usc469_real_estate_exemption_u2_sed_r4, "economic/legal").

domain_priors:requires_active_enforcement(26usc469_real_estate_exemption_u2_sed_r4).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(26usc469_real_estate_exemption_u2_sed_r4, full_time_real_estate_professionals).
narrative_ontology:constraint_victim(26usc469_real_estate_exemption_u2_sed_r4, hybrid_w2_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE HYBRID W-2 INVESTOR (SNARE). For an individual with a demanding, high-income primary career, the 750-hour and >50% tests are practically insurmountable. The rule acts as a snare, preventing them from deducting legitimate investment losses against their primary income, thereby extracting higher tax payments.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_sed_r4, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE FULL-TIME REAL ESTATE PROFESSIONAL (ROPE). For this group, the rule is a clear, predictable coordination mechanism. It defines their professional status, granting them significant tax advantages unavailable to others. It solidifies their business model and is perceived as a fair distinction for those primarily engaged in the industry.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_sed_r4, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE IRS (TANGLED ROPE). The enforcing institution sees the rule's dual function: it serves a legitimate coordination purpose by defining a specific class of taxpayer to prevent widespread tax shelter abuse, but it also requires active, costly enforcement and creates a stark, legally-mandated asymmetry between two groups of investors.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_sed_r4, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE). The analyst recognizes both the stated coordination goal (curbing passive loss shelters) and the highly extractive outcome. The strict, time-based tests function as a gatekeeping mechanism that benefits one class of capital (full-time real estate investors) at the direct expense of another (salaried professionals who also invest).
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_sed_r4, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(26usc469_real_estate_exemption_u2_sed_r4_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_sed_r4, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_sed_r4, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(26usc469_real_estate_exemption_u2_sed_r4, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(26usc469_real_estate_exemption_u2_sed_r4_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.68) reflects the significant tax savings denied to the victim class. The high suppression (0.72) represents the extreme difficulty for a high-earning professional in another field to meet the time-based qualification tests, effectively suppressing their access to the exemption. The rule is not theatrical; it is a functional and actively enforced part of the tax code.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the full-time professional, the law is a 'Rope' that provides clear, beneficial rules for their industry. For the W-2 investor, it's a 'Snare'—a trap where their investment losses are real but legally unrecognized for deduction against their primary income, creating an unavoidable tax burden.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is unambiguous. The constraint directs value (in the form of tax deductions) to full-time real estate professionals (beneficiaries) and away from hybrid W-2 investors (victims), who subsidize this benefit through their higher relative tax burden. The IRS acts as the neutral enforcement mechanism of this directional flow.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope correctly identifies that a legitimate policy goal (distinguishing active from passive participation) is implemented via a mechanism that creates severe, asymmetric extraction. It avoids mischaracterizing it as a pure Snare (as it does have a non-predatory rationale) or a simple Rope (as it clearly harms one group to benefit another).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pal_intent_vs_effect,
    'Was the primary legislative intent to curb abuse by high-income professionals (a coordination function), or to create a protected, favored class of real estate investors (an extractive function)?',
    'Analysis of legislative history, lobbying records from real estate industry groups, and economic modeling of the rule's long-term distributional effects.',
    'If intent was purely anti-abuse, it's a Rope that has degraded or has unintended consequences. If intent was to favor a specific class, it was designed as a Tangled Rope from its inception.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pal_intent_vs_effect, empirical, 'Distinguishing between the rule's stated anti-abuse intent and its actual extractive effect on W-2 investors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(26usc469_real_estate_exemption_u2_sed_r4, 1986, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(26us_tr_t1986, 26usc469_real_estate_exemption_u2_sed_r4, theater_ratio, 1986, 0.1).
narrative_ontology:measurement(26us_tr_t2005, 26usc469_real_estate_exemption_u2_sed_r4, theater_ratio, 2005, 0.12).
narrative_ontology:measurement(26us_tr_t2024, 26usc469_real_estate_exemption_u2_sed_r4, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(26us_be_t1986, 26usc469_real_estate_exemption_u2_sed_r4, base_extractiveness, 1986, 0.55).
narrative_ontology:measurement(26us_be_t2005, 26usc469_real_estate_exemption_u2_sed_r4, base_extractiveness, 2005, 0.62).
narrative_ontology:measurement(26us_be_t2024, 26usc469_real_estate_exemption_u2_sed_r4, base_extractiveness, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(26usc469_real_estate_exemption_u2_sed_r4, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
