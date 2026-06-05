% ============================================================================
% CONSTRAINT STORY: 26usc469_real_estate_exemption_u3_exp_r2
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-16
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_26usc469_real_estate_exemption_u3_exp_r2, []).

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
 *   constraint_id: 26usc469_real_estate_exemption_u3_exp_r2
 *   human_readable: The Real Estate Professional Exemption (Passive Activity Loss Rules)
 *   domain: economic/legal
 *
 * SUMMARY:
 *   Section 469 of the US tax code generally prevents taxpayers from
 *   deducting passive activity losses against active income. The (c)(7)
 *   exemption carves out a special exception for 'real estate professionals,'
 *   but defines this status with a strict, two-part test: the taxpayer must
 *   spend more than 750 hours and more than half of their total personal
 *   services in real property trades or businesses. This creates a
 *   significant, often insurmountable, barrier for individuals with
 *   high-income W-2 jobs, effectively bifurcating taxpayers into those who
 *   can and cannot access these powerful deductions.
 *
 * KEY AGENTS:
 *   - Hybrid W-2 Investors: Primary target (powerless/trapped) — bears extraction via disallowed losses.
 *   - Full-Time Real Estate Professionals: Primary beneficiary (moderate/mobile) — benefits from the clear qualification path and tax advantages.
 *   - The IRS: Enforcing institution (institutional/constrained) — administers the bright-line test.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(26usc469_real_estate_exemption_u3_exp_r2, 0.62).
domain_priors:suppression_score(26usc469_real_estate_exemption_u3_exp_r2, 0.75).
domain_priors:theater_ratio(26usc469_real_estate_exemption_u3_exp_r2, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u3_exp_r2, extractiveness, 0.62).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u3_exp_r2, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u3_exp_r2, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(26usc469_real_estate_exemption_u3_exp_r2, tangled_rope).
narrative_ontology:human_readable(26usc469_real_estate_exemption_u3_exp_r2, "The Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain(26usc469_real_estate_exemption_u3_exp_r2, "economic/legal").

domain_priors:requires_active_enforcement(26usc469_real_estate_exemption_u3_exp_r2).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(26usc469_real_estate_exemption_u3_exp_r2, full_time_real_estate_professionals).
narrative_ontology:constraint_victim(26usc469_real_estate_exemption_u3_exp_r2, hybrid_w2_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For an investor with a high-income primary career, the dual tests are an insurmountable barrier. The law functions as a trap, disallowing legitimate losses they cannot practically reclassify.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_exp_r2, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For those whose primary business is real estate, the rule is a clear, beneficial coordination mechanism. It defines their professional status and provides a significant, predictable tax advantage.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_exp_r2, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% The enforcing body sees both the rule's function (creating a 'bright-line' test for enforcement) and its asymmetric consequences. It is a tool of coordination that requires active policing of a sharp, extractive boundary.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_exp_r2, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The analyst sees a dual-purpose constraint: it coordinates the definition of a 'professional' to prevent widespread tax shelter abuse, but does so by creating a highly extractive barrier that benefits one class of investor at the direct expense of another.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_exp_r2, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(26usc469_real_estate_exemption_u3_exp_r2_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_exp_r2, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u3_exp_r2, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(26usc469_real_estate_exemption_u3_exp_r2, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(26usc469_real_estate_exemption_u3_exp_r2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.62) reflects the direct financial value of the disallowed deductions, which are transferred from the 'hybrid investor' to the general tax base, indirectly benefiting those who can access the loophole. The high suppression (0.75) comes from the legal finality of the two-part test; there are no alternative paths or recourse for those who fail to meet the strict hourly requirements.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. The beneficiary (full-time pro) sees a clear 'Rope' that defines their professional status and provides a deserved tax benefit. The victim (hybrid investor) sees a 'Snare' designed to make qualification impossible while maintaining a facade of accessibility. The analytical view of 'Tangled Rope' reconciles these by acknowledging the legitimate coordination function (preventing passive-loss tax shelters) is achieved via a highly extractive and asymmetric mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint directs tax benefits towards a narrowly defined group (full-time real estate professionals) by imposing a barrier that is structurally difficult for another group (high-income professionals in other fields) to overcome. The 'victims' are not random; they are specifically those who have both the passive losses to deduct and the active income to deduct them against, but lack the time to meet the qualification tests. This is a targeted extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope correctly avoids two errors. It is not a pure Snare, because it has a non-trivial, legislated coordination purpose: to distinguish between active and passive participation in real estate to curb tax shelters. It is not a simple Rope, because the mechanism for coordination creates a clear class of victims who bear a significant financial burden. The Tangled Rope classification captures this duality of function and asymmetric outcome.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rep_status_policy_intent,
    'Is the strict 750-hour/50%-time test a necessary bright-line rule to prevent tax shelter abuse, or is it a deliberately constructed barrier to protect the interests of the full-time real estate industry?',
    'Analysis of legislative history and lobbying records from the Tax Reform Act of 1986 and subsequent amendments.',
    'If primarily an anti-abuse rule, its classification would shift towards a harsh Rope. If primarily a protectionist barrier for an incumbent industry, it is a clear Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rep_status_policy_intent, empirical, 'The ambiguity between the rule's stated anti-abuse purpose and its actual protectionist effect for an industry group.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(26usc469_real_estate_exemption_u3_exp_r2, 1986, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(26us_tr_t1986, 26usc469_real_estate_exemption_u3_exp_r2, theater_ratio, 1986, 0.1).
narrative_ontology:measurement(26us_tr_t2005, 26usc469_real_estate_exemption_u3_exp_r2, theater_ratio, 2005, 0.12).
narrative_ontology:measurement(26us_tr_t2024, 26usc469_real_estate_exemption_u3_exp_r2, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(26us_be_t1986, 26usc469_real_estate_exemption_u3_exp_r2, base_extractiveness, 1986, 0.4).
narrative_ontology:measurement(26us_be_t2005, 26usc469_real_estate_exemption_u3_exp_r2, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(26us_be_t2024, 26usc469_real_estate_exemption_u3_exp_r2, base_extractiveness, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(26usc469_real_estate_exemption_u3_exp_r2, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
