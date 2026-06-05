% ============================================================================
% CONSTRAINT STORY: 26usc469_real_estate_exemption_u1_exp_r5
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_26usc469_real_estate_exemption_u1_exp_r5, []).

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
 *   constraint_id: 26usc469_real_estate_exemption_u1_exp_r5
 *   human_readable: The Real Estate Professional Exemption (Passive Activity Loss Rules)
 *   domain: economic/legal
 *
 * SUMMARY:
 *   Section 469 of the US tax code generally disallows the deduction of
 *   passive activity losses against active income. The (c)(7) exemption for
 *   'real estate professionals' creates an exception, but defines this status
 *   via a strict two-part test: the taxpayer must spend more than 750 hours
 *   AND more than half of their total personal services in real property
 *   trades or businesses. This 'more than half' prong creates an almost
 *   insurmountable barrier for individuals with demanding, high-income W-2
 *   jobs, regardless of the scale of their real estate activities. This
 *   bifurcates taxpayers into a beneficiary class that can easily qualify and
 *   a victim class that is structurally excluded.
 *
 * KEY AGENTS:
 *   - Hybrid W-2 Investors: Primary target (powerless/trapped) — blocked from deducting losses due to their primary employment.
 *   - Full-Time Real Estate Professionals: Primary beneficiary (moderate/mobile) — benefit from a clear, albeit strict, path to deducting passive losses.
 *   - The IRS: Enforcing institution (institutional/constrained) — must apply the bright-line test as written.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(26usc469_real_estate_exemption_u1_exp_r5, 0.68).
domain_priors:suppression_score(26usc469_real_estate_exemption_u1_exp_r5, 0.75).
domain_priors:theater_ratio(26usc469_real_estate_exemption_u1_exp_r5, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u1_exp_r5, extractiveness, 0.68).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u1_exp_r5, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u1_exp_r5, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(26usc469_real_estate_exemption_u1_exp_r5, tangled_rope).
narrative_ontology:human_readable(26usc469_real_estate_exemption_u1_exp_r5, "The Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain(26usc469_real_estate_exemption_u1_exp_r5, "economic/legal").

domain_priors:requires_active_enforcement(26usc469_real_estate_exemption_u1_exp_r5).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(26usc469_real_estate_exemption_u1_exp_r5, full_time_real_estate_professionals).
narrative_ontology:constraint_victim(26usc469_real_estate_exemption_u1_exp_r5, hybrid_w2_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of an investor with a primary job, the rule is a trap that disallows legitimate deductions, extracting value with no recourse other than quitting their job.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u1_exp_r5, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For those whose primary business is real estate, the rule is a clear coordination mechanism that defines their professional status and provides predictable tax benefits.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u1_exp_r5, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% The IRS is constrained to enforce the law, which serves a coordination function (defining a class of taxpayer) but requires active, coercive enforcement that creates asymmetric outcomes.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u1_exp_r5, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The analyst sees both the stated coordination goal and the functional extractive outcome, classifying it as a Tangled Rope where a bright-line test creates a privileged class.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u1_exp_r5, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(26usc469_real_estate_exemption_u1_exp_r5_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u1_exp_r5, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u1_exp_r5, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(26usc469_real_estate_exemption_u1_exp_r5, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(26usc469_real_estate_exemption_u1_exp_r5_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.68) reflects the direct financial loss to hybrid investors whose deductions are disallowed. The high suppression (0.75) stems from the rigidity of the 'more than half of personal services' test, which offers no alternative path for qualification and effectively blocks an entire class of investors from accessing the benefit.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the Full-Time Professional, who sees the rule as a legitimate 'Rope' for defining their industry, and the Hybrid Investor, who experiences it as a 'Snare' that punishes them for having a primary career. The former sees a professional standard; the latter sees an arbitrary barrier.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint directs tax benefits towards individuals who can dedicate their entire professional life to real estate, making them the beneficiaries. It extracts value (in the form of higher tax liability) from those who engage in real estate as a significant but secondary activity, making them the victims. The structure of the test itself is the mechanism of this directional transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a pure Snare would miss its genuine, if flawed, coordination function: attempting to distinguish between active and passive participation in an industry. Classifying it as a Rope would ignore the highly coercive and extractive effect on a large, well-defined group. The Tangled Rope classification correctly identifies that a mechanism for coordination has been implemented in a way that produces severe, asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intent_vs_outcome,
    'Was the strict two-part test intended to create a professional class (coordination) or primarily to limit deductions for high-income earners (extraction)?',
    'Analysis of the legislative history of the Tax Reform Act of 1986 and subsequent amendments.',
    'If primarily for coordination, strengthens the Rope/Scaffold case. If primarily for revenue/extraction, strengthens the Snare case.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intent_vs_outcome, empirical, 'Distinguishing between the legislative intent (coordination) and the functional outcome (extraction).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(26usc469_real_estate_exemption_u1_exp_r5, 1986, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(26us_tr_t1986, 26usc469_real_estate_exemption_u1_exp_r5, theater_ratio, 1986, 0.1).
narrative_ontology:measurement(26us_tr_t2005, 26usc469_real_estate_exemption_u1_exp_r5, theater_ratio, 2005, 0.15).
narrative_ontology:measurement(26us_tr_t2024, 26usc469_real_estate_exemption_u1_exp_r5, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(26us_be_t1986, 26usc469_real_estate_exemption_u1_exp_r5, base_extractiveness, 1986, 0.3).
narrative_ontology:measurement(26us_be_t2005, 26usc469_real_estate_exemption_u1_exp_r5, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(26us_be_t2024, 26usc469_real_estate_exemption_u1_exp_r5, base_extractiveness, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(26usc469_real_estate_exemption_u1_exp_r5, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
