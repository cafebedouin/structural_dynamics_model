% ============================================================================
% CONSTRAINT STORY: 26usc469_real_estate_exemption_u3_exp_r5
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_26usc469_real_estate_exemption_u3_exp_r5, []).

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
 *   constraint_id: 26usc469_real_estate_exemption_u3_exp_r5
 *   human_readable: The Real Estate Professional Exemption (Passive Activity Loss Rules)
 *   domain: economic/legal
 *
 * SUMMARY:
 *   Section 469 of the US Internal Revenue Code limits the ability of
 *   taxpayers to deduct losses from passive activities against active income.
 *   The (c)(7) exemption creates a carve-out for 'real estate professionals.'
 *   To qualify, an individual must pass a strict, two-part test: (1) spend
 *   more than 750 hours in real property trades or businesses, AND (2)
 *   perform more than half of their total personal services in those same
 *   activities. This second prong creates a structural barrier for
 *   individuals with high-income, non-real-estate jobs, effectively
 *   bifurcating investors into a beneficiary class that can take the
 *   deductions and a victim class that cannot.
 *
 * KEY AGENTS:
 *   - Hybrid W-2 Investors: Primary target (powerless/trapped) — their primary career makes it nearly impossible to meet the 'more than half' test, leading to disallowed losses.
 *   - Full-Time Real Estate Professionals: Primary beneficiary (moderate/mobile) — the rule provides a clear pathway to deduct losses, giving them a significant tax advantage.
 *   - The IRS: Enforcing institution (institutional/constrained) — tasked with auditing and enforcing the bright-line test.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(26usc469_real_estate_exemption_u3_exp_r5, 0.62).
domain_priors:suppression_score(26usc469_real_estate_exemption_u3_exp_r5, 0.75).
domain_priors:theater_ratio(26usc469_real_estate_exemption_u3_exp_r5, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u3_exp_r5, extractiveness, 0.62).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u3_exp_r5, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u3_exp_r5, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(26usc469_real_estate_exemption_u3_exp_r5, tangled_rope).
narrative_ontology:human_readable(26usc469_real_estate_exemption_u3_exp_r5, "The Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain(26usc469_real_estate_exemption_u3_exp_r5, "economic/legal").

domain_priors:requires_active_enforcement(26usc469_real_estate_exemption_u3_exp_r5).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(26usc469_real_estate_exemption_u3_exp_r5, full_time_real_estate_professionals).
narrative_ontology:constraint_victim(26usc469_real_estate_exemption_u3_exp_r5, hybrid_w2_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For an individual with a demanding non-real-estate career, the two-part test is an insurmountable barrier. Their real estate losses are disallowed, directly increasing their tax liability. The constraint extracts value and offers no recourse, appearing as a pure Snare.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_exp_r5, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For someone whose primary business is real estate, the rule is a clear, beneficial standard. It legitimizes their professional status for tax purposes and unlocks significant deductions. It functions as a pure coordination mechanism (Rope) that defines the rules of the game to their advantage.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_exp_r5, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% The enforcing institution must apply the rule as written. It recognizes the dual function: coordinating to prevent the widespread tax shelter abuse of the pre-1986 era, while also acknowledging the harsh, extractive effect on a specific class of taxpayers. The enforcement burden makes the hybrid nature clear.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_exp_r5, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The analytical view confirms the Tangled Rope classification. The constraint possesses a genuine coordination function (defining 'professional') layered onto a powerful extractive mechanism (disallowing losses for others). The high suppression score reflects the structural barrier created by the 'more than half' test.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_exp_r5, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(26usc469_real_estate_exemption_u3_exp_r5_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_exp_r5, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_exp_r5, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(26usc469_real_estate_exemption_u3_exp_r5, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(26usc469_real_estate_exemption_u3_exp_r5_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.62) reflects the substantial tax cost imposed on the victim class by disallowing potentially large paper losses. The suppression score (0.75) is high because the 'more than half of personal services' test is a rigid, non-negotiable barrier that effectively eliminates alternatives for anyone with a demanding primary career.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the Hybrid W-2 Investor, who experiences the rule as an arbitrary Snare that extracts wealth, and the Full-Time Real Estate Professional, who sees it as a legitimate Rope that defines their professional status and coordinates tax benefits. The former is trapped by their career; the latter is enabled by it.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality is clear. It benefits the class of individuals whose entire economic activity is centered in real estate, allowing them to use its favorable depreciation rules to offset income. It extracts from those who participate in real estate as a secondary investment, specifically those with significant other income, by denying them the same benefit. The government is a secondary beneficiary via increased tax revenue from the victim class.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope is critical to avoid mandatrophy. A pure Snare classification would ignore the legitimate coordination function the rule serves in preventing the kind of passive loss tax shelters that were rampant before 1986. A pure Rope classification would ignore the severe, asymmetric extraction imposed on a structurally trapped group of taxpayers. The Tangled Rope classification correctly identifies that a system designed for coordination has become a powerful tool of extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pal_rule_intent_vs_effect,
    'Is the primary function of the 750-hour rule to genuinely distinguish professional activity from passive investment, or to serve as a revenue-generating barrier that protects a specific class of investor?',
    'Analysis of legislative history combined with empirical data on the economic activity and time commitment of those who fail vs. pass the test.',
    'If primarily a barrier with no sound economic basis: Snare. If a genuine, albeit blunt, distinction: Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pal_rule_intent_vs_effect, conceptual, 'Distinguishing between the rule's stated intent (coordination) and its practical effect (extraction).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(26usc469_real_estate_exemption_u3_exp_r5, 1986, 2046).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(26us_tr_t1986, 26usc469_real_estate_exemption_u3_exp_r5, theater_ratio, 1986, 0.05).
narrative_ontology:measurement(26us_tr_t2016, 26usc469_real_estate_exemption_u3_exp_r5, theater_ratio, 2016, 0.1).
narrative_ontology:measurement(26us_tr_t2046, 26usc469_real_estate_exemption_u3_exp_r5, theater_ratio, 2046, 0.15).

% Extraction over time
narrative_ontology:measurement(26us_be_t1986, 26usc469_real_estate_exemption_u3_exp_r5, base_extractiveness, 1986, 0.55).
narrative_ontology:measurement(26us_be_t2016, 26usc469_real_estate_exemption_u3_exp_r5, base_extractiveness, 2016, 0.6).
narrative_ontology:measurement(26us_be_t2046, 26usc469_real_estate_exemption_u3_exp_r5, base_extractiveness, 2046, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(26usc469_real_estate_exemption_u3_exp_r5, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
