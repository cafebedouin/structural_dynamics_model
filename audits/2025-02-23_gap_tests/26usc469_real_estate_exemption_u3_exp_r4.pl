% ============================================================================
% CONSTRAINT STORY: 26usc469_real_estate_exemption_u3_exp_r4
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-16
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_26usc469_real_estate_exemption_u3_exp_r4, []).

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
 *   constraint_id: 26usc469_real_estate_exemption_u3_exp_r4
 *   human_readable: The Real Estate Professional Exemption (Passive Activity Loss Rules)
 *   domain: economic/legal
 *
 * SUMMARY:
 *   Section 469 of the US tax code limits the deduction of passive activity
 *   losses. The Real Estate Professional (REP) status provides a powerful
 *   exemption, allowing holders to deduct unlimited real estate losses
 *   against active income. However, qualification requires passing a strict
 *   two-part test: (1) performing over 750 hours in real property trades, AND
 *   (2) spending more than half of one's total personal services in those
 *   trades. This second prong effectively gates the exemption away from
 *   individuals with demanding, high-income W-2 careers, creating a
 *   structural extraction mechanism.
 *
 * KEY AGENTS:
 *   - Hybrid W-2 Investors: Primary target (powerless/trapped) — unable to meet the 'more than half' test, their losses are disallowed.
 *   - Full-Time Real Estate Professionals: Primary beneficiary (moderate/mobile) — easily meet the test and gain significant tax advantages.
 *   - The IRS: Enforcing institution (institutional/constrained) — must apply the bright-line rule, creating the extractive asymmetry.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(26usc469_real_estate_exemption_u3_exp_r4, 0.68).
domain_priors:suppression_score(26usc469_real_estate_exemption_u3_exp_r4, 0.75).
domain_priors:theater_ratio(26usc469_real_estate_exemption_u3_exp_r4, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u3_exp_r4, extractiveness, 0.68).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u3_exp_r4, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u3_exp_r4, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(26usc469_real_estate_exemption_u3_exp_r4, tangled_rope).
narrative_ontology:human_readable(26usc469_real_estate_exemption_u3_exp_r4, "The Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain(26usc469_real_estate_exemption_u3_exp_r4, "economic/legal").

domain_priors:requires_active_enforcement(26usc469_real_estate_exemption_u3_exp_r4).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(26usc469_real_estate_exemption_u3_exp_r4, full_time_real_estate_professionals).
narrative_ontology:constraint_victim(26usc469_real_estate_exemption_u3_exp_r4, hybrid_w2_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of a high-income professional with a demanding primary job, the two-part test is an insurmountable barrier. Their real estate losses are trapped as 'passive' and cannot offset their primary income, representing a direct extraction of potential tax savings. The only exit is to quit their job, which is not a viable choice.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_exp_r4, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For someone whose primary business is real estate, the rule is a clear, achievable standard. It provides a significant tax advantage that underpins their business model, functioning as a pure coordination mechanism to distinguish them from casual investors.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_exp_r4, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% The IRS is constrained to enforce the law as written. It sees both the legitimate coordination function (defining a 'professional') and the extractive consequences (disallowing losses for those who fail the test). The rule requires active enforcement and generates significant disputes and audits.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_exp_r4, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The analytical view recognizes the dual function. The constraint coordinates by creating a 'bright-line' test, but the test's strictness creates a protected class of beneficiaries and a targeted class of victims, making it a classic Tangled Rope.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_exp_r4, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(26usc469_real_estate_exemption_u3_exp_r4_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_exp_r4, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_exp_r4, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(26usc469_real_estate_exemption_u3_exp_r4, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(26usc469_real_estate_exemption_u3_exp_r4_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.68) reflects the significant financial value of disallowed losses for high-income earners. The very high suppression (0.75) reflects the fact that the only way for the target to escape the constraint is to abandon their primary, often more lucrative, career. This is a prohibitive cost, making the constraint nearly absolute for this group.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the Hybrid Investor (Snare) and the Full-Time Professional (Rope). For the investor, the rule is a trap that prevents them from realizing the tax benefits of their investments. For the professional, it is a clear and beneficial rule of the road that defines their professional status and protects their tax advantages from being diluted by high-earning outsiders.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's structure directs tax benefits specifically to the class of 'full-time real estate professionals' by making it nearly impossible for another class, 'hybrid W-2 investors', to qualify. The cost is borne by the latter group in the form of higher tax liability. The beneficiaries are those whose business model is enabled by the tax break; the victims are those who are structurally excluded from it despite significant investment and time commitment (but less than 50% of their total work time).
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a simple Snare would miss its legitimate (if harsh) coordination function: defining a professional class to prevent passive tax shelters. Classifying it as a Rope would ignore the severe, asymmetric extraction imposed on a specific group. The Tangled Rope classification is necessary to capture this duality, where a rule with a stated coordination purpose functions as a powerful extractive tool against those who fall just outside its narrow definition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rep_status_policy_intent,
    'Is the 'more than half of personal services' test a necessary bright-line rule to prevent tax shelter abuse, or a deliberately constructed barrier to protect the full-time professional class from competition?',
    'Analysis of legislative history and lobbying records from real estate industry groups during the Tax Reform Act of 1986 and subsequent amendments.',
    'If primarily for abuse prevention, it leans towards a harsh Rope. If a protective barrier, it solidifies the Snare/Tangled Rope classification by confirming extractive intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rep_status_policy_intent, empirical, 'The ambiguity of legislative intent behind the strict two-part test for Real Estate Professional status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(26usc469_real_estate_exemption_u3_exp_r4, 1986, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(26us_tr_t1990, 26usc469_real_estate_exemption_u3_exp_r4, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(26us_tr_t2007, 26usc469_real_estate_exemption_u3_exp_r4, theater_ratio, 2007, 0.12).
narrative_ontology:measurement(26us_tr_t2024, 26usc469_real_estate_exemption_u3_exp_r4, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(26us_be_t1990, 26usc469_real_estate_exemption_u3_exp_r4, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(26us_be_t2007, 26usc469_real_estate_exemption_u3_exp_r4, base_extractiveness, 2007, 0.6).
narrative_ontology:measurement(26us_be_t2024, 26usc469_real_estate_exemption_u3_exp_r4, base_extractiveness, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(26usc469_real_estate_exemption_u3_exp_r4, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
