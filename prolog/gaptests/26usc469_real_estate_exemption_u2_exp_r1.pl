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
 *   estate professionals' who meet a strict two-part test: (1) performing
 *   over 750 hours of service in real property trades, AND (2) performing
 *   more than half of their total personal services in those trades. This
 *   second prong creates a structural barrier for individuals with demanding,
 *   high-income W-2 careers, effectively bifurcating taxpayers into a class
 *   that can fully deduct real estate losses and a class that cannot,
 *   regardless of the scale of their real estate activities.
 *
 * KEY AGENTS:
 *   - Hybrid W-2 Investors: Primary target (powerless/trapped) — bears extraction via disallowed losses.
 *   - Full-Time Real Estate Professionals: Primary beneficiary (moderate/mobile) — benefits from the clear qualification path and tax advantages.
 *   - The IRS: Enforcing institution (institutional/constrained) — administers the rule as written.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(26usc469_real_estate_exemption_u2_exp_r1, 0.68).
domain_priors:suppression_score(26usc469_real_estate_exemption_u2_exp_r1, 0.72).
domain_priors:theater_ratio(26usc469_real_estate_exemption_u2_exp_r1, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_exp_r1, extractiveness, 0.68).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_exp_r1, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_exp_r1, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(26usc469_real_estate_exemption_u2_exp_r1, tangled_rope).
narrative_ontology:human_readable(26usc469_real_estate_exemption_u2_exp_r1, "The Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain(26usc469_real_estate_exemption_u2_exp_r1, "economic/legal").

domain_priors:requires_active_enforcement(26usc469_real_estate_exemption_u2_exp_r1).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(26usc469_real_estate_exemption_u2_exp_r1, full_time_real_estate_professionals).
narrative_ontology:constraint_victim(26usc469_real_estate_exemption_u2_exp_r1, hybrid_w2_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of a high-income earner with a primary job, the rule is a trap. They are encouraged to invest in real estate but are structurally barred from the primary tax benefit, making their losses non-deductible against the income they need to cover them.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r1, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For someone whose primary profession is real estate, the rule is a simple, clear coordination mechanism. It defines their status and provides a predictable and advantageous tax framework. The extractive component is invisible to them.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r1, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% The IRS is tasked with enforcing this law. It sees both the coordination function (preventing a class of passive losses from sheltering active income) and the extractive result (higher tax revenue from a specific group of taxpayers). It is constrained to enforce the law as written.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r1, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The analytical view recognizes the dual nature: a legitimate policy goal (coordination) is achieved via a mechanism that creates a sharp, asymmetric extractive barrier against one group while privileging another.
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
 *   The high extractiveness (0.68) reflects the direct financial cost of being unable to deduct substantial losses against other income. The high suppression (0.72) reflects the prohibitive cost of the primary alternative for a W-2 earner: quitting their main career to satisfy the 'more than half' test. The constraint is actively enforced by the IRS, and its function is not merely theatrical.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the Hybrid Investor (who sees a Snare blocking a major tax benefit) and the Full-Time Professional (who sees a Rope that defines their professional tax status). The former is trapped by the rule's structure, while the latter is empowered by it. The analytical view of Tangled Rope reconciles these by acknowledging the valid coordination goal (preventing tax shelters) is achieved through an asymmetrically extractive mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is clear. The constraint extracts from individuals whose time is primarily committed to non-real-estate professions, preventing them from offsetting their primary income. It subsidizes those whose primary profession is real estate, granting them a powerful deduction unavailable to others. The 'more than half of personal services' clause is the specific mechanism that directs the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification is crucial here. Labeling this a simple Rope (anti-abuse rule) would ignore the severe, targeted extraction. Labeling it a pure Snare would ignore its legitimate, if flawed, coordination function in tax administration. The framework correctly identifies it as a hybrid system where a coordination mechanism has a powerful and predictable extractive side-effect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    policy_intent_vs_effect,
    'Was the high barrier for W-2 earners an intended feature to protect the tax base, or an unintended consequence of a poorly-defined 'professional' status?',
    'Analysis of the legislative history and Congressional Budget Office scoring of the 1993 Omnibus Budget Reconciliation Act which created the exemption.',
    'If intended, the constraint is closer to a pure Snare embedded within a legal framework. If unintended, it is a flawed Rope that has degraded into a Tangled Rope through interaction with modern employment and investment patterns.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_intent_vs_effect, empirical, 'Distinguishing between intended tax base protection and unintended consequences for hybrid investors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(26usc469_real_estate_exemption_u2_exp_r1, 1993, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(26us_tr_t1993, 26usc469_real_estate_exemption_u2_exp_r1, theater_ratio, 1993, 0.2).
narrative_ontology:measurement(26us_tr_t2008, 26usc469_real_estate_exemption_u2_exp_r1, theater_ratio, 2008, 0.2).
narrative_ontology:measurement(26us_tr_t2024, 26usc469_real_estate_exemption_u2_exp_r1, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(26us_be_t1993, 26usc469_real_estate_exemption_u2_exp_r1, base_extractiveness, 1993, 0.5).
narrative_ontology:measurement(26us_be_t2008, 26usc469_real_estate_exemption_u2_exp_r1, base_extractiveness, 2008, 0.6).
narrative_ontology:measurement(26us_be_t2024, 26usc469_real_estate_exemption_u2_exp_r1, base_extractiveness, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(26usc469_real_estate_exemption_u2_exp_r1, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
