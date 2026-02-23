% ============================================================================
% CONSTRAINT STORY: 26usc469_real_estate_exemption_u2_exp_r5
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_26usc469_real_estate_exemption_u2_exp_r5, []).

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
 *   constraint_id: 26usc469_real_estate_exemption_u2_exp_r5
 *   human_readable: The Real Estate Professional Exemption (Passive Activity Loss Rules)
 *   domain: economic/legal
 *
 * SUMMARY:
 *   Section 469 of the US tax code, introduced in 1986, limits the ability to
 *   deduct passive activity losses against active income. A key exception
 *   exists for 'real estate professionals,' but the two-part qualification
 *   test (750 hours AND more than half of total work time) creates a
 *   structural barrier for individuals with demanding non-real estate
 *   careers. This effectively creates two classes of real estate investors: a
 *   professional class that can fully utilize tax losses, and a 'hybrid'
 *   class that is systematically denied the same benefit, leading to higher
 *   effective tax rates.
 *
 * KEY AGENTS:
 *   - Hybrid W-2 Investors: Primary target (powerless/trapped) — bears extraction via disallowed losses.
 *   - Full-Time Real Estate Professionals: Primary beneficiary (moderate/mobile) — benefits from the clear qualification path and reduced competition from tax-advantaged amateurs.
 *   - The IRS: Enforcing institution (institutional/constrained) — administers the law as written.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(26usc469_real_estate_exemption_u2_exp_r5, 0.68).
domain_priors:suppression_score(26usc469_real_estate_exemption_u2_exp_r5, 0.75).
domain_priors:theater_ratio(26usc469_real_estate_exemption_u2_exp_r5, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_exp_r5, extractiveness, 0.68).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_exp_r5, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_exp_r5, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(26usc469_real_estate_exemption_u2_exp_r5, tangled_rope).
narrative_ontology:human_readable(26usc469_real_estate_exemption_u2_exp_r5, "The Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain(26usc469_real_estate_exemption_u2_exp_r5, "economic/legal").

domain_priors:requires_active_enforcement(26usc469_real_estate_exemption_u2_exp_r5).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(26usc469_real_estate_exemption_u2_exp_r5, full_time_real_estate_professionals).
narrative_ontology:constraint_victim(26usc469_real_estate_exemption_u2_exp_r5, hybrid_w2_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For an individual with a high-income primary job, the 'more than half of personal services' test is an almost insurmountable barrier, making the rule a pure extraction mechanism that disallows legitimate losses. The cost of exit (quitting their job) is prohibitive.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r5, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For those whose primary business is real estate, the rule is a clear, albeit strict, coordination mechanism. It defines their professional status and unlocks significant tax advantages, creating a moat around their business model.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r5, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% The IRS administers the rule as written. It sees both the coordination function (distinguishing active/passive participants to prevent abuse) and the extractive outcome (increased tax revenue from the disallowed losses of the non-qualifying group). It is constrained to enforce the statute.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r5, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The analyst sees the dual nature: a stated goal of coordination (preventing tax shelters) coupled with a mechanism that produces highly asymmetric extraction. The structure benefits one class of economic actor at the direct expense of another, which is the hallmark of a Tangled Rope.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r5, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(26usc469_real_estate_exemption_u2_exp_r5_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r5, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r5, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(26usc469_real_estate_exemption_u2_exp_r5, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(26usc469_real_estate_exemption_u2_exp_r5_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.68) reflects the significant financial value of the disallowed losses, which translates directly into higher tax payments. The high suppression (0.75) stems from the 'more than half' test, which for most high-income professionals, makes qualification impossible without abandoning their primary career, an alternative with prohibitive costs.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the Hybrid Investor and the Full-Time Professional. The investor sees a Snare—a punitive rule that extracts from them with no recourse. The professional sees a Rope—a clear, if demanding, set of rules that defines their industry and provides a competitive advantage. The analytical view recognizes both functions are present, classifying it as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint directs tax benefits (a form of subsidy) towards full-time real estate professionals by extracting value (in the form of taxes paid on income that cannot be offset by losses) from hybrid investors. The beneficiaries are those who can structure their entire economic life around the rule, while the victims are those who cannot.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope correctly avoids mislabeling it as a simple anti-abuse rule (Rope). While it has a coordination function (defining 'professional'), its primary structural effect is asymmetric extraction based on employment status. It is a mechanism of policy that functions by creating and enforcing a barrier that one group can cross and another cannot.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pal_rule_intent,
    'Is the strict 'more than half' test a necessary guardrail against abuse (coordination) or an intentionally high barrier to protect a specific class of investors and maximize revenue from another (extraction)?',
    'Analysis of legislative history and lobbying records from the Tax Reform Act of 1986, and economic modeling of alternative tests (e.g., a pure hours test without the percentage requirement).',
    'If proven to be a necessary and well-calibrated guardrail, the classification might shift towards a harsh Rope. If shown to be an intentional barrier, it solidifies the Tangled Rope/Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pal_rule_intent, conceptual, 'The core ambiguity is whether the rule's strictness is for preventing abuse or for targeted extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(26usc469_real_estate_exemption_u2_exp_r5, 1986, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(26us_tr_t1986, 26usc469_real_estate_exemption_u2_exp_r5, theater_ratio, 1986, 0.1).
narrative_ontology:measurement(26us_tr_t2005, 26usc469_real_estate_exemption_u2_exp_r5, theater_ratio, 2005, 0.15).
narrative_ontology:measurement(26us_tr_t2024, 26usc469_real_estate_exemption_u2_exp_r5, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(26us_be_t1986, 26usc469_real_estate_exemption_u2_exp_r5, base_extractiveness, 1986, 0.5).
narrative_ontology:measurement(26us_be_t2005, 26usc469_real_estate_exemption_u2_exp_r5, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement(26us_be_t2024, 26usc469_real_estate_exemption_u2_exp_r5, base_extractiveness, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(26usc469_real_estate_exemption_u2_exp_r5, enforcement_mechanism).
narrative_ontology:affects_constraint(26usc469_real_estate_exemption_u2_exp_r5, us_tax_code_complexity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
