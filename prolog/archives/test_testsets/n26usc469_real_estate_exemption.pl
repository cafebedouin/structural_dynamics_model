% ============================================================================
% CONSTRAINT STORY: n26usc469_real_estate_exemption
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_n26usc469_real_estate_exemption, []).

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
 *   constraint_id: n26usc469_real_estate_exemption
 *   human_readable: The Real Estate Professional Exemption (Passive Activity Loss Rules)
 *   domain: economic/legal
 *
 * SUMMARY:
 *   Section 469 of the US tax code establishes the Passive Activity Loss
 *   (PAL) rules, which prevent taxpayers from deducting losses from passive
 *   investments (like most rental real estate) against active income (like a
 *   salary). This constraint story analyzes the 'Real Estate Professional'
 *   exemption, a specific carve-out that allows individuals who spend
 *   significant time in real estate trades to bypass the PAL rules. To
 *   qualify, a taxpayer must spend over 750 hours and more than 50% of their
 *   total working time in real property businesses. This creates a sharp
 *   dividing line between two classes of real estate investors, with
 *   significant financial consequences.
 *
 * KEY AGENTS:
 *   - Qualifying Real Estate Professionals: Primary beneficiary (organized/arbitrage) — successfully navigate the rules to deduct rental losses against other income.
 *   - US Treasury / General Taxpayer Base: Primary victim (powerless/trapped) — bears the cost of the foregone tax revenue.
 *   - Non-Qualifying Passive Investors: Secondary victim (powerful/constrained) — high-income individuals who own real estate but are blocked by the PAL rules and cannot meet the exemption's high bar.
 *   - The IRS: Institutional enforcer (institutional/constrained) — tasked with auditing and verifying the often-subjective time-based claims of taxpayers.
 *   - Tax Policy Analyst: Analytical observer (analytical/analytical) — views the structure of the rule, its intended purpose, and its actual effects.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(n26usc469_real_estate_exemption, 0.65).
domain_priors:suppression_score(n26usc469_real_estate_exemption, 0.75).
domain_priors:theater_ratio(n26usc469_real_estate_exemption, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(n26usc469_real_estate_exemption, extractiveness, 0.65).
narrative_ontology:constraint_metric(n26usc469_real_estate_exemption, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(n26usc469_real_estate_exemption, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(n26usc469_real_estate_exemption, tangled_rope).
narrative_ontology:human_readable(n26usc469_real_estate_exemption, "The Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain(n26usc469_real_estate_exemption, "economic/legal").

domain_priors:requires_active_enforcement(n26usc469_real_estate_exemption).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(n26usc469_real_estate_exemption, qualifying_real_estate_professionals).
narrative_ontology:constraint_victim(n26usc469_real_estate_exemption, us_treasury_general_taxpayer_base).
narrative_ontology:constraint_victim(n26usc469_real_estate_exemption, non_qualifying_passive_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE GENERAL TAXPAYER (SNARE) — Experiences the exemption as a pure extraction. A specific, organized group is allowed to reduce its tax liability, shifting the burden to the general, unorganized tax base, which has no ability to opt out or access the same benefit. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.92.
constraint_indexing:constraint_classification(n26usc469_real_estate_exemption, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE QUALIFYING PROFESSIONAL (ROPE) — Experiences the rule as a pure coordination mechanism. It correctly distinguishes their active business from the passive investments of others, allowing for appropriate tax treatment. The high compliance burden is the cost of this coordination. As a beneficiary with arbitrage, d≈0.15, f(d)≈-0.01, σ=1.0 → χ≈-0.01. Negative effective extraction indicates a net subsidy.
constraint_indexing:constraint_classification(n26usc469_real_estate_exemption, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE NON-QUALIFYING INVESTOR (TANGLED ROPE) — This actor is trapped by the default Passive Activity Loss (PAL) rules. They see the exemption as a system that both coordinates (it sets a clear, if high, bar) and extracts (the benefit is inaccessible to them, forcing them to carry suspended losses). d≈0.80, f(d)≈1.25, σ=1.0 → χ≈0.81.
constraint_indexing:constraint_classification(n26usc469_real_estate_exemption, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE IRS (TANGLED ROPE) — The agency is tasked with enforcing a rule that has both a clear coordination goal (separating active/passive) and a high potential for extractive abuse (via fraudulent time logs). Enforcement is a constant struggle to validate claims, making it a hybrid of function and costly verification. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.42.
constraint_indexing:constraint_classification(n26usc469_real_estate_exemption, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE ANALYTICAL OBSERVER (TANGLED ROPE) — The analyst sees the dual function clearly. The rule coordinates by creating a bright-line test for professionalism, but this test simultaneously enables a significant, asymmetric tax expenditure (extraction) for a small, well-positioned group. This is the canonical view and matches the claimed_type. d≈0.73, f(d)≈1.15, σ=1.0 → χ≈0.75.
constraint_indexing:constraint_classification(n26usc469_real_estate_exemption, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(n26usc469_real_estate_exemption_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(n26usc469_real_estate_exemption, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(n26usc469_real_estate_exemption, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(n26usc469_real_estate_exemption, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(n26usc469_real_estate_exemption_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.65) is high because the tax savings for a qualifying individual can be substantial, representing a direct wealth transfer from the public treasury. This is not a minor loophole. Suppression (0.75) is high because the default PAL rules are very restrictive, and the criteria to escape them (750 hours, >50% time, material participation) are rigid and demanding, with no alternative pathways. Theater Ratio (0.40) is moderate; while the time-logging requirement is intended to be functional, it often becomes a performative exercise in documentation, with taxpayers reconstructing logs to meet the threshold for an audit.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the beneficiary, it's a fair 'Rope' that recognizes their active business. For the general taxpayer base, it's a 'Snare'—a loophole for a select few. For the investor who fails to qualify, it's a 'Tangled Rope'—a system of rules that constrains them while benefiting others. The analytical view confirms the 'Tangled Rope' classification, acknowledging the legitimate coordination goal (separating active from passive) is inextricably linked to an asymmetric extractive outcome.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries ('qualifying_real_estate_professionals') have organized power and arbitrage exit, leading to a low directionality ('d') and a perception of the rule as a subsidy (Rope). Victims ('us_treasury_general_taxpayer_base') are powerless and trapped, leading to a high 'd' and perception of pure extraction (Snare). Constrained actors ('non_qualifying_passive_investors') have high power but are constrained by the rule, leading to a high-but-not-maximal 'd' and a mixed perception (Tangled Rope).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a classic case for resolving mandatrophy. A naive analysis might label it a 'Snare' (a tax loophole) or a 'Rope' (a fair rule for business owners). The Deferential Realism framework shows both are valid perspectival truths. The analytical classification of 'Tangled Rope' correctly identifies the core structure: a mechanism that performs a genuine coordination function (distinguishing types of investors) while simultaneously facilitating asymmetric extraction (a large tax benefit for a specific group).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proxy_effectiveness,
    'Are the 750-hour and >50% time tests effective proxies for genuine professional activity, or are they arbitrary hurdles that primarily reward meticulous record-keeping?',
    'Analysis of IRS audit data comparing outcomes for taxpayers with contemporaneous logs versus reconstructed logs, correlated with the economic scale of their real estate operations.',
    'If the tests are poor proxies, the constraint is more of a Snare, rewarding performative compliance. If they are effective, it reinforces the Tangled Rope classification, where the coordination function is genuine but costly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_effectiveness, empirical, 'Effectiveness of time-based tests as a proxy for professionalism.').

omega_variable(
    economic_incentive_effect,
    'Does the tax benefit primarily incentivize productive economic activity (e.g., development of new housing stock) or simply tax-advantaged speculation in existing properties?',
    'Econometric analysis comparing the types of real estate investments made by qualifying professionals versus non-qualifying high-income investors.',
    'If it drives productive activity, the rule has a stronger coordination (Rope) component. If it primarily fuels speculation, it functions more as a pure extraction mechanism (Snare).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(economic_incentive_effect, empirical, 'Net economic impact of the tax incentive on real estate markets.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(n26usc469_real_estate_exemption, 1993, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(n26u_tr_t1994, n26usc469_real_estate_exemption, theater_ratio, 1994, 0.25).
narrative_ontology:measurement(n26u_tr_t2010, n26usc469_real_estate_exemption, theater_ratio, 2010, 0.33).
narrative_ontology:measurement(n26u_tr_t2024, n26usc469_real_estate_exemption, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(n26u_be_t1994, n26usc469_real_estate_exemption, base_extractiveness, 1994, 0.5).
narrative_ontology:measurement(n26u_be_t2010, n26usc469_real_estate_exemption, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(n26u_be_t2024, n26usc469_real_estate_exemption, base_extractiveness, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(n26usc469_real_estate_exemption, enforcement_mechanism).
narrative_ontology:affects_constraint(n26usc469_real_estate_exemption, tax_code_complexity).
narrative_ontology:affects_constraint(n26usc469_real_estate_exemption, housing_affordability_zoning).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
