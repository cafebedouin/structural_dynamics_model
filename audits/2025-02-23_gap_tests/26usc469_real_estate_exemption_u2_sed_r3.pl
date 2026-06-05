% ============================================================================
% CONSTRAINT STORY: 26usc469_real_estate_exemption_u2_sed_r3
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2023-10-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_26usc469_real_estate_exemption_u2_sed_r3, []).

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
 *   constraint_id: 26usc469_real_estate_exemption_u2_sed_r3
 *   human_readable: The Real Estate Professional Exemption (Passive Activity Loss Rules)
 *   domain: economic/legal
 *
 * SUMMARY:
 *   Section 469 of the US tax code disallows passive activity losses against
 *   active income. An exception exists for 'real estate professionals' who
 *   meet a two-part test: 750+ hours in real estate activities, AND more than
 *   half of their total working time must be in real estate. This second
 *   prong creates a structural barrier for individuals with demanding
 *   non-real-estate careers, effectively creating a protected class of
 *   investors who can access deductions unavailable to others, even if they
 *   spend identical time on their properties.
 *
 * KEY AGENTS:
 *   - Hybrid W-2 Investors: Primary target (powerless/trapped) - Cannot meet the 'more than half' test and thus their losses are disallowed.
 *   - Full-Time Real Estate Professionals: Primary beneficiary (moderate/mobile) - Easily meet the test and benefit from deducting losses.
 *   - The IRS: Enforcing institution (institutional/constrained) - Administers the bright-line rule.
 *   - High-Net-Worth Family Offices: Powerful beneficiary (powerful/arbitrage) - Can structure their activities to ensure qualification.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(26usc469_real_estate_exemption_u2_sed_r3, 0.68).
domain_priors:suppression_score(26usc469_real_estate_exemption_u2_sed_r3, 0.75).
domain_priors:theater_ratio(26usc469_real_estate_exemption_u2_sed_r3, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_sed_r3, extractiveness, 0.68).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_sed_r3, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_sed_r3, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(26usc469_real_estate_exemption_u2_sed_r3, tangled_rope).
narrative_ontology:human_readable(26usc469_real_estate_exemption_u2_sed_r3, "The Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain(26usc469_real_estate_exemption_u2_sed_r3, "economic/legal").

domain_priors:requires_active_enforcement(26usc469_real_estate_exemption_u2_sed_r3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(26usc469_real_estate_exemption_u2_sed_r3, full_time_real_estate_professionals).
narrative_ontology:constraint_beneficiary(26usc469_real_estate_exemption_u2_sed_r3, high_net_worth_family_offices).
narrative_ontology:constraint_victim(26usc469_real_estate_exemption_u2_sed_r3, hybrid_w2_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For an individual with a demanding, high-income primary career, the 'more than half' test is an insurmountable barrier, making this rule a pure extraction mechanism that disallows legitimate business losses.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_sed_r3, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For someone whose career is in real estate, this is a clear, predictable, and beneficial rule that enables effective tax planning and business operation. It is pure coordination.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_sed_r3, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% The enforcing institution sees a bright-line rule that simplifies administration (coordination) but is also aware that this rule creates a sharp structural divide with clear winners and losers (extraction).
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_sed_r3, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The system-level view reveals the dual function: it coordinates by defining a specific class of taxpayer, but does so in a way that asymmetrically extracts value from one group to benefit another.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_sed_r3, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(26usc469_real_estate_exemption_u2_sed_r3_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_sed_r3, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_sed_r3, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(26usc469_real_estate_exemption_u2_sed_r3, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(26usc469_real_estate_exemption_u2_sed_r3_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.68) reflects the significant tax value of deducting rental losses, which can be in the tens or hundreds of thousands of dollars annually. The high suppression (0.75) reflects the fact that this is federal law with no legal alternative; the only 'exit' for a W-2 investor is to quit their primary career, a prohibitively costly action.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark: for the full-time professional, it's a simple, beneficial 'Rope' that clarifies their status. For the hybrid investor, it's a 'Snare' that uses an arbitrary-feeling metric (relative time, not absolute) to trap their losses and extract tax revenue from them that their peers do not have to pay.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint directs tax benefits towards those whose primary professional identity is real estate and away from those who participate in real estate as a secondary business. The victims are defined by their success in a non-real-estate field, creating a direct conflict of interest. The beneficiaries are those who can structure their entire economic life around real estate.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a pure Snare would miss the genuine coordination function of creating a clear, enforceable standard for the IRS. Classifying it as a Rope would ignore the deeply asymmetric and extractive outcomes. The Tangled Rope classification correctly identifies that a mechanism of coordination has been designed in such a way as to produce a permanent class of winners and losers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legislative_intent_vs_outcome,
    'Was the high barrier for W-2 earners an intended feature to limit the deduction, or an unintended consequence of drafting a 'simple' test?',
    'Analysis of legislative history and committee reports from the Revenue Reconciliation Act of 1993.',
    'If intended, it solidifies the Tangled Rope classification as a deliberate policy choice. If unintended, it suggests the constraint is a flawed Rope that has degraded into a partial Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_intent_vs_outcome, empirical, 'Distinguishing between intended policy targeting and unintended structural exclusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(26usc469_real_estate_exemption_u2_sed_r3, 1993, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(26us_tr_t0, 26usc469_real_estate_exemption_u2_sed_r3, theater_ratio, 0, 0.1).
narrative_ontology:measurement(26us_tr_t15, 26usc469_real_estate_exemption_u2_sed_r3, theater_ratio, 15, 0.15).
narrative_ontology:measurement(26us_tr_t30, 26usc469_real_estate_exemption_u2_sed_r3, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(26us_be_t0, 26usc469_real_estate_exemption_u2_sed_r3, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(26us_be_t15, 26usc469_real_estate_exemption_u2_sed_r3, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(26us_be_t30, 26usc469_real_estate_exemption_u2_sed_r3, base_extractiveness, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(26usc469_real_estate_exemption_u2_sed_r3, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
