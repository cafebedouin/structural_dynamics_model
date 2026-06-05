% ============================================================================
% CONSTRAINT STORY: 26usc469_real_estate_exemption_u1_exp_r1
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_26usc469_real_estate_exemption_u1_exp_r1, []).

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
 *   constraint_id: 26usc469_real_estate_exemption_u1_exp_r1
 *   human_readable: The Real Estate Professional Exemption (Passive Activity Loss Rules)
 *   domain: economic/legal
 *
 * SUMMARY:
 *   Section 469 of the US tax code generally disallows the deduction of
 *   passive activity losses against active income. The (c)(7) exemption for
 *   'real estate professionals' creates an exception, but defines this status
 *   with a rigid two-part test: the taxpayer must spend more than 750 hours
 *   and more than half of their total personal services in real property
 *   trades or businesses. This structure creates a nearly insurmountable
 *   barrier for individuals with demanding, high-income W-2 jobs, preventing
 *   them from accessing tax deductions available to full-time real estate
 *   investors.
 *
 * KEY AGENTS:
 *   - Hybrid W-2 Investors: Primary target (powerless/trapped) — bears the cost of disallowed losses.
 *   - Full-Time Real Estate Professionals: Primary beneficiary (organized/mobile) — benefits from a protected tax status.
 *   - The IRS: Enforcing institution (institutional/constrained) — administers the rule as written.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(26usc469_real_estate_exemption_u1_exp_r1, 0.68).
domain_priors:suppression_score(26usc469_real_estate_exemption_u1_exp_r1, 0.75).
domain_priors:theater_ratio(26usc469_real_estate_exemption_u1_exp_r1, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u1_exp_r1, extractiveness, 0.68).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u1_exp_r1, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u1_exp_r1, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(26usc469_real_estate_exemption_u1_exp_r1, tangled_rope).
narrative_ontology:human_readable(26usc469_real_estate_exemption_u1_exp_r1, "The Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain(26usc469_real_estate_exemption_u1_exp_r1, "economic/legal").

domain_priors:requires_active_enforcement(26usc469_real_estate_exemption_u1_exp_r1).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(26usc469_real_estate_exemption_u1_exp_r1, full_time_real_estate_professionals).
narrative_ontology:constraint_victim(26usc469_real_estate_exemption_u1_exp_r1, hybrid_w2_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: For the individual with a primary career, the strict tests create a trap where legitimate investment losses cannot be deducted, representing a direct financial extraction.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u1_exp_r1, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: For those whose primary profession is real estate, the rule is a clear coordination mechanism that defines their status and provides significant, predictable tax advantages.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u1_exp_r1, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: The enforcing body sees both the rule's function in coordinating a legal definition and the coercive, extractive nature of its enforcement against a specific class of taxpayer.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u1_exp_r1, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: The analytical view recognizes the dual nature: a legitimate attempt to define a professional class (coordination) coupled with a highly extractive barrier that creates asymmetric outcomes.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u1_exp_r1, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(26usc469_real_estate_exemption_u1_exp_r1_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u1_exp_r1, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u1_exp_r1, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(26usc469_real_estate_exemption_u1_exp_r1, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(26usc469_real_estate_exemption_u1_exp_r1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.68) represents the direct financial value of the disallowed loss deductions. The high suppression (0.75) reflects the extreme difficulty for a W-2 employee to meet both the 750-hour and the 'more than half' tests, effectively locking them out of the benefit regardless of the scale of their real estate activities.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the Hybrid W-2 Investor, who experiences the rule as a Snare that arbitrarily denies legitimate deductions, and the Full-Time Real Estate Professional, who sees it as a Rope that defines and protects their professional status. The former is trapped by their primary career, while the latter benefits from the high barrier to entry.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint directs benefits to a well-defined group (full-time real estate professionals) by creating a legal moat that is difficult for others to cross. The costs are borne by another group (hybrid W-2 investors) who are structurally disadvantaged by the rule's design, as their primary income source becomes the reason they cannot offset their investment losses.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a classic Tangled Rope. It is not a pure Snare because it serves a legitimate coordination function: defining a 'professional' for tax purposes. However, the implementation is so restrictive that it creates a severe, asymmetric extraction. Classifying it as a Tangled Rope correctly captures this duality, preventing the mislabeling of its coordination aspect as benign while acknowledging the high extraction it imposes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pal_rules_legislative_intent,
    'Were the strict tests primarily intended to curb abusive tax shelters, or to protect a professional class while extracting from high-income part-time investors?',
    'Analysis of the legislative history and committee reports from the Tax Reform Act of 1986.',
    'If primarily to curb abuse, it reinforces the Tangled Rope classification. If to protect a class, it shifts closer to a pure Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pal_rules_legislative_intent, empirical, 'The original legislative intent behind the strict 'real estate professional' definition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(26usc469_real_estate_exemption_u1_exp_r1, 1986, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(26us_tr_t1986, 26usc469_real_estate_exemption_u1_exp_r1, theater_ratio, 1986, 0.1).
narrative_ontology:measurement(26us_tr_t2005, 26usc469_real_estate_exemption_u1_exp_r1, theater_ratio, 2005, 0.12).
narrative_ontology:measurement(26us_tr_t2024, 26usc469_real_estate_exemption_u1_exp_r1, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(26us_be_t1986, 26usc469_real_estate_exemption_u1_exp_r1, base_extractiveness, 1986, 0.6).
narrative_ontology:measurement(26us_be_t2005, 26usc469_real_estate_exemption_u1_exp_r1, base_extractiveness, 2005, 0.65).
narrative_ontology:measurement(26us_be_t2024, 26usc469_real_estate_exemption_u1_exp_r1, base_extractiveness, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(26usc469_real_estate_exemption_u1_exp_r1, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
