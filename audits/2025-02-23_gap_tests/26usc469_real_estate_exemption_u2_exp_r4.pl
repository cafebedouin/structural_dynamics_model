% ============================================================================
% CONSTRAINT STORY: 26usc469_real_estate_exemption_u2_exp_r4
% ============================================================================
% Version: 1.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_26usc469_real_estate_exemption_u2_exp_r4, []).

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
 *   constraint_id: 26usc469_real_estate_exemption_u2_exp_r4
 *   human_readable: The Real Estate Professional Exemption (Passive Activity Loss Rules)
 *   domain: economic/legal
 *
 * SUMMARY:
 *   Section 469 of the US tax code disallows the deduction of passive
 *   activity losses against active income. A key exception exists for 'real
 *   estate professionals' who meet a strict two-part test: (1) performing
 *   over 750 hours of service in real property trades, and (2) performing
 *   more services in real property trades than in all other trades combined.
 *   This second prong creates a structural barrier for individuals with
 *   demanding, high-income W-2 jobs, preventing them from qualifying
 *   regardless of the scale of their real estate activities. This bifurcates
 *   taxpayers into a beneficiary class that can deduct unlimited losses and a
 *   victim class that cannot.
 *
 * KEY AGENTS:
 *   - Hybrid W-2 Investors: Primary target (powerless/trapped) — bears extraction via disallowed losses.
 *   - Full-Time Real Estate Professionals: Primary beneficiary (moderate/mobile) — benefits from the clear qualification path and tax deductions.
 *   - The IRS: Enforcing institution (institutional/constrained) — administers the rule as written.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(26usc469_real_estate_exemption_u2_exp_r4, 0.55).
domain_priors:suppression_score(26usc469_real_estate_exemption_u2_exp_r4, 0.62).
domain_priors:theater_ratio(26usc469_real_estate_exemption_u2_exp_r4, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_exp_r4, extractiveness, 0.55).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_exp_r4, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(26usc469_real_estate_exemption_u2_exp_r4, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(26usc469_real_estate_exemption_u2_exp_r4, tangled_rope).
narrative_ontology:human_readable(26usc469_real_estate_exemption_u2_exp_r4, "The Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain(26usc469_real_estate_exemption_u2_exp_r4, "economic/legal").

domain_priors:requires_active_enforcement(26usc469_real_estate_exemption_u2_exp_r4).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(26usc469_real_estate_exemption_u2_exp_r4, full_time_real_estate_professionals).
narrative_ontology:constraint_victim(26usc469_real_estate_exemption_u2_exp_r4, hybrid_w2_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of a high-income professional with significant real estate investments, the rule is a Snare. The 'more than half of personal services' test makes it structurally impossible to qualify without abandoning their primary career, trapping their passive losses.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r4, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For those whose primary business is real estate, the rule is a clear Rope. It provides a well-defined, achievable standard that coordinates their professional status and unlocks significant tax benefits, distinguishing them from casual investors.
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r4, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% The enforcing agency sees a Tangled Rope. It serves a coordination function by creating a bright-line test for audits, but its structure is known to create asymmetric outcomes and generate revenue from a specific class of taxpayers (the W-2 investors).
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r4, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The analytical view is a Tangled Rope, recognizing both the legitimate policy goal of limiting passive loss shelters (coordination) and the highly extractive effect on a specific group of taxpayers who are structurally barred from qualifying (extraction).
constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r4, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(26usc469_real_estate_exemption_u2_exp_r4_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r4, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(26usc469_real_estate_exemption_u2_exp_r4, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(26usc469_real_estate_exemption_u2_exp_r4, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(26usc469_real_estate_exemption_u2_exp_r4_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score of 0.55 reflects the significant financial value of the disallowed tax deductions, which represents a direct transfer of wealth from the investor to the state. The suppression score of 0.62 is based on the near-impossibility for a high-earning W-2 professional to meet the 'more than half of personal services' test without quitting their job, effectively suppressing any alternative path to qualification for this group.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the W-2 investor and the full-time professional. The investor sees a Snare: a rule that seems to offer a benefit but is structurally impossible for them to access, trapping their losses. The professional sees a Rope: a clear and fair rule that defines their industry and provides a deserved tax advantage for their full-time commitment.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint is explicitly directional. It channels tax benefits towards individuals whose primary economic activity is real estate, while extracting value (in the form of higher tax liability on other income) from individuals who engage in real estate as a secondary, albeit substantial, activity. The beneficiaries are defined by their profession, and the victims are defined by having a different primary profession.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope correctly avoids mislabeling it as either a pure coordination rule (Rope) or a simple predatory tax (Snare). It acknowledges the legitimate government interest in preventing passive tax shelters (the coordination function) while capturing the severe, asymmetric, and extractive consequences of the specific mechanism chosen to achieve that goal. The rule coordinates and extracts simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rep_status_policy_intent,
    'Is the strict 'half of personal services' test intended to raise revenue by excluding high-earners, or is it a genuinely necessary bright-line test to prevent tax shelter abuse?',
    'Analysis of legislative history and Congressional Budget Office scoring from the Tax Reform Act of 1986.',
    'If primarily for revenue generation from a targeted group, it leans more towards Snare. If it's a necessary, albeit blunt, anti-abuse rule, Tangled Rope is more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rep_status_policy_intent, empirical, 'The policy intent behind the strict two-part test for Real Estate Professional status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(26usc469_real_estate_exemption_u2_exp_r4, 1986, 2046).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(26us_tr_t1986, 26usc469_real_estate_exemption_u2_exp_r4, theater_ratio, 1986, 0.1).
narrative_ontology:measurement(26us_tr_t2016, 26usc469_real_estate_exemption_u2_exp_r4, theater_ratio, 2016, 0.12).
narrative_ontology:measurement(26us_tr_t2046, 26usc469_real_estate_exemption_u2_exp_r4, theater_ratio, 2046, 0.15).

% Extraction over time
narrative_ontology:measurement(26us_be_t1986, 26usc469_real_estate_exemption_u2_exp_r4, base_extractiveness, 1986, 0.45).
narrative_ontology:measurement(26us_be_t2016, 26usc469_real_estate_exemption_u2_exp_r4, base_extractiveness, 2016, 0.5).
narrative_ontology:measurement(26us_be_t2046, 26usc469_real_estate_exemption_u2_exp_r4, base_extractiveness, 2046, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(26usc469_real_estate_exemption_u2_exp_r4, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
