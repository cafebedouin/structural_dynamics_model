% ============================================================================
% CONSTRAINT STORY: 26usc469_real_estate_exemption_u2_exp_r1
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_26usc469_real_estate_exemption_u2_exp_r1, []).

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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: 26usc469_real_estate_exemption_u2_exp_r1
 *   human_readable: The Real Estate Professional Exemption (Passive Activity Loss Rules)
 *   domain: economic/legal
 *
 * SUMMARY:
 *   Section 469 of the US tax code disallows the deduction of passive
 *   activity losses against active income. A key exception exists for 'real
 *   estate professionals' who meet a strict two-part test: performing over
 *   750 hours of service in real property trades and these services
 *   constituting more than half of their total personal services. This
 *   creates a structural barrier for individuals with demanding, high-income
 *   W-2 jobs, preventing them from accessing tax deductions available to
 *   full-time real estate investors.
 *
 * KEY AGENTS:
 *   - Hybrid W-2 Investors: Primary target (powerless/trapped) - High-income professionals who invest in real estate but cannot meet the time tests.
 *   - Full-Time Real Estate Professionals: Primary beneficiary (moderate/mobile) - Individuals and entities whose primary business is real estate.
 *   - The IRS: Enforcing institution (institutional/constrained) - Administers and enforces the tax code as written.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(26usc469_real_estate_exemption_u2_exp_r1, 0.62).
domain_priors:suppression_score(26usc469_real_estate_exemption_u2_exp_r1, 0.75).
domain_priors:theater_ratio(26usc469_real_estate_exemption_u2_exp_r1, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_exp_r1, extractiveness, 0.62).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_exp_r1, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_exp_r1, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(26usc469_real_estate_exemption_u2_exp_r1, tangled_rope).
narrative_ontology:human_readable(26usc469_real_estate_exemption_u2_exp_r1, "The Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain(26usc469_real_estate_exemption_u2_exp_r1, "economic/legal").

domain_priors:requires_active_enforcement(26usc469_real_estate_exemption_u2_exp_r1).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(26usc469_real_estate_exemption_u2_exp_r1, full_time_real_estate_professionals).
narrative_ontology:constraint_beneficiary(26usc469_real_estate_exemption_u2_exp_r1, us_treasury).
narrative_ontology:constraint_victim(26usc469_real_estate_exemption_u2_exp_r1, hybrid_w2_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For an individual with a high-income primary job, the time requirements are nearly impossible to meet, making this a pure extraction of tax revenue via disallowed losses. The cost of exit (quitting their job) is prohibitive.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r1, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For those whose primary business is real estate, the rule is a clear coordination mechanism that defines their professional status and unlocks significant tax advantages. It is purely beneficial.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r1, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% The IRS sees a clear, enforceable rule that prevents a broad class of high-income taxpayers from sheltering income. It coordinates tax collection and is seen as a functional, non-extractive mechanism from its perspective.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r1, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The analyst sees both the legitimate coordination function (defining a professional class to limit tax shelters) and the severe, asymmetric extraction from a specific group (W-2 investors) who are structurally barred from qualifying.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r1, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(26usc469_real_estate_exemption_u2_exp_r1_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r1, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r1, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(26usc469_real_estate_exemption_u2_exp_r1, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(26usc469_real_estate_exemption_u2_exp_r1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.62) represents the significant tax value of the disallowed losses, which are effectively transferred from the W-2 investor to the Treasury. The suppression score (0.75) is high because the only way for a target to escape the constraint is to quit their primary, often high-paying, career, which is an extremely costly alternative.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. The W-2 investor experiences a Snare: a rule that extracts significant tax revenue with no recourse. The full-time professional sees a Rope: a clear, beneficial rule that defines their status and provides a competitive advantage. The analytical observer sees the Tangled Rope: a rule with a legitimate coordination purpose (preventing passive investors from claiming professional status) that is implemented in a way that creates severe, asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint directs tax benefits towards those who dedicate their careers to real estate and away from those who participate as a secondary activity, regardless of the scale of their investment. The beneficiaries are full-time professionals (who face less competition from tax-advantaged amateurs) and the US Treasury (which collects higher revenue from the victim class). The victims are the hybrid investors who are structurally blocked from qualifying.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a classic Tangled Rope. Classifying it as a pure Snare would ignore its stated and partially functional purpose of preventing widespread tax sheltering. Classifying it as a Rope would ignore the millions in extracted tax revenue from a class of investors who are trapped by the rule's structure. The Tangled Rope classification correctly captures this duality of function and extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rep_status_legislative_intent,
    'Was the strict two-part test (750 hours AND >50% of services) primarily intended as a necessary guardrail against tax abuse, or as a protectionist measure for the full-time real estate industry?',
    'Analysis of the legislative history of the Revenue Reconciliation Act of 1993 and records of industry lobbying.',
    'If primarily a guardrail, it solidifies the Tangled Rope classification. If primarily protectionist, it strengthens the case that the constraint functions as a Snare at the policy level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rep_status_legislative_intent, empirical, 'The legislative intent behind the strict 'real estate professional' test.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(26usc469_real_estate_exemption_u2_exp_r1, 1993, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(26us_tr_t0, 26usc469_real_estate_exemption_u2_exp_r1, theater_ratio, 0, 0.15).
narrative_ontology:measurement(26us_tr_t15, 26usc469_real_estate_exemption_u2_exp_r1, theater_ratio, 15, 0.15).
narrative_ontology:measurement(26us_tr_t30, 26usc469_real_estate_exemption_u2_exp_r1, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(26us_be_t0, 26usc469_real_estate_exemption_u2_exp_r1, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(26us_be_t15, 26usc469_real_estate_exemption_u2_exp_r1, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(26us_be_t30, 26usc469_real_estate_exemption_u2_exp_r1, base_extractiveness, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(26usc469_real_estate_exemption_u2_exp_r1, enforcement_mechanism).
narrative_ontology:affects_constraint(26usc469_real_estate_exemption_u2_exp_r1, at_risk_rules_465).
narrative_ontology:affects_constraint(26usc469_real_estate_exemption_u2_exp_r1, material_participation_rules_469).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
