% ============================================================================
% CONSTRAINT STORY: 26usc469_real_estate_exemption_u2_sed_r2
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_26usc469_real_estate_exemption_u2_sed_r2, []).

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
 *   constraint_id: 26usc469_real_estate_exemption_u2_sed_r2
 *   human_readable: The Real Estate Professional Exemption (Passive Activity Loss Rules)
 *   domain: economic/legal
 *
 * SUMMARY:
 *   Section 469 of the US tax code limits the deduction of passive activity
 *   losses. A key exception exists for 'real estate professionals' who meet a
 *   two-part test: spending 750 hours and, crucially, more than half their
 *   total working time in real estate activities. This second prong creates a
 *   bright-line rule that is almost impossible for individuals with
 *   demanding, high-income W-2 careers to meet, effectively bifurcating
 *   investors into a professional class that can deduct unlimited losses and
 *   an amateur class that cannot.
 *
 * KEY AGENTS:
 *   - Hybrid W-2 Investors: Primary target (powerless/trapped) - Their real estate losses are suspended and cannot offset their primary income, resulting in a higher tax burden.
 *   - Full-Time Real Estate Professionals: Primary beneficiary (moderate/mobile) - The rule carves out a protected status for them, allowing full deductibility of losses and creating a significant financial advantage.
 *   - The IRS: Enforcing institution (institutional/constrained) - Tasked with auditing and enforcing this complex, fact-specific rule.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(26usc469_real_estate_exemption_u2_sed_r2, 0.68).
domain_priors:suppression_score(26usc469_real_estate_exemption_u2_sed_r2, 0.72).
domain_priors:theater_ratio(26usc469_real_estate_exemption_u2_sed_r2, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_sed_r2, extractiveness, 0.68).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_sed_r2, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_sed_r2, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(26usc469_real_estate_exemption_u2_sed_r2, tangled_rope).
narrative_ontology:human_readable(26usc469_real_estate_exemption_u2_sed_r2, "The Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain(26usc469_real_estate_exemption_u2_sed_r2, "economic/legal").

domain_priors:requires_active_enforcement(26usc469_real_estate_exemption_u2_sed_r2).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(26usc469_real_estate_exemption_u2_sed_r2, full_time_real_estate_professionals).
narrative_ontology:constraint_victim(26usc469_real_estate_exemption_u2_sed_r2, hybrid_w2_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For a high-income professional, the 'more than half of personal services' test is a nearly impossible barrier without quitting their job, making this a trap that disallows legitimate investment losses.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_sed_r2, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For those whose primary profession is real estate, this is a clear, beneficial rule that distinguishes them from passive hobbyists and unlocks significant tax advantages. It is pure coordination.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_sed_r2, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% The IRS is constrained to enforce the law as written. It sees both the coordination function (defining a class of taxpayer to prevent abuse) and the complex, asymmetric outcomes that require active enforcement and audits.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_sed_r2, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The analyst sees the dual function: a legitimate attempt to curb passive loss shelters (coordination) that simultaneously creates a highly extractive barrier for one class of investors while benefiting another (extraction).
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_sed_r2, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% As a moderate actor with some resources, I see the patterns across the country. I can see the rule's benefit for full-time professionals but also feel the extractive bite as it limits my own ability to offset income, representing a tangled system of coordination and extraction.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_sed_r2, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(26usc469_real_estate_exemption_u2_sed_r2_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_sed_r2, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_sed_r2, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(26usc469_real_estate_exemption_u2_sed_r2, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(26usc469_real_estate_exemption_u2_sed_r2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.68) stems from the direct financial cost (higher taxes) imposed on W-2 investors whose losses are disallowed. The high suppression (0.72) reflects the extreme difficulty of meeting the 'more than half of personal services' test without abandoning a primary career, effectively suppressing access to the exemption for this group.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the Full-Time Professional, who sees a clear and fair 'Rope' that defines their professional status, and the Hybrid W-2 Investor, who experiences a 'Snare' that arbitrarily traps their legitimate investment losses. The enforcing institution and analytical observers see the 'Tangled Rope'—a rule with a stated coordination goal (preventing tax shelters) that operates via a highly extractive mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's benefits flow directly to full-time real estate professionals, who are defined as the beneficiaries. The costs are borne by investors with substantial non-real-estate income (the victims), as the rule is structured to prevent them from offsetting that specific type of income. The direction of extraction is unambiguous.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a pure Snare would ignore its legitimate, if debated, policy goal of preventing a specific type of tax shelter. Classifying it as a Rope would ignore the severe, asymmetric extraction imposed on a well-defined group. The Tangled Rope classification is essential to capture this duality, where a coordination mechanism is implemented in a way that creates a clear class of winners and losers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rep_status_policy_intent,
    'Is the strict 'more than half of personal services' test a necessary guardrail against widespread tax sheltering, or is it an intentionally protectionist barrier for the full-time real estate industry?',
    'Analysis of legislative history, lobbying records from real estate industry groups, and economic modeling of tax revenue impact under alternative definitions.',
    'If a necessary guardrail, it remains a Tangled Rope. If primarily protectionist, its classification shifts towards a Snare, as the coordination function is pretextual.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rep_status_policy_intent, empirical, 'Ambiguity between the rule's stated anti-abuse purpose and its actual effect of gatekeeping tax benefits for a specific professional class.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(26usc469_real_estate_exemption_u2_sed_r2, 1986, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(26us_tr_t1990, 26usc469_real_estate_exemption_u2_sed_r2, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(26us_tr_t2005, 26usc469_real_estate_exemption_u2_sed_r2, theater_ratio, 2005, 0.12).
narrative_ontology:measurement(26us_tr_t2020, 26usc469_real_estate_exemption_u2_sed_r2, theater_ratio, 2020, 0.15).

% Extraction over time
narrative_ontology:measurement(26us_be_t1990, 26usc469_real_estate_exemption_u2_sed_r2, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(26us_be_t2005, 26usc469_real_estate_exemption_u2_sed_r2, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(26us_be_t2020, 26usc469_real_estate_exemption_u2_sed_r2, base_extractiveness, 2020, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(26usc469_real_estate_exemption_u2_sed_r2, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
