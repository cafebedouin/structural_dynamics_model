% ============================================================================
% CONSTRAINT STORY: bor_tax_exemption_nl
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bor_tax_exemption_nl, []).

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
 *   constraint_id: bor_tax_exemption_nl
 *   human_readable: Dutch Business Succession Scheme (BOR)
 *   domain: economic/political
 *
 * SUMMARY:
 *   The Dutch Business Succession Scheme (BOR) is a tax regulation designed
 *   to facilitate the transfer of family businesses by providing substantial
 *   exemptions from inheritance and gift tax. While its stated purpose is to
 *   ensure business continuity and preserve employment, it functions as a
 *   major tax shelter that disproportionately benefits wealthy families. This
 *   creates a structural conflict between a legitimate coordination goal
 *   (avoiding forced liquidation of viable businesses) and a massive,
 *   asymmetric extraction of value from the public tax base, which is borne
 *   by all other taxpayers and heirs of non-business assets.
 *
 * KEY AGENTS:
 *   - Family Business Heirs: Primary beneficiaries (powerful/arbitrage) who receive tax-free intergenerational wealth transfers.
 *   - Non-Business Heirs: Primary victims (powerless/trapped) who pay full inheritance tax on other assets.
 *   - General Taxpayers: Diffuse victims (powerless/trapped) who bear the cost of forgone state revenue.
 *   - Policymakers: Institutional actors (institutional/constrained) who must balance economic goals with public criticism over inequality.
 *   - Financial Advisors: Organized beneficiaries (organized/mobile) who profit from the scheme's complexity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bor_tax_exemption_nl, 0.68).
domain_priors:suppression_score(bor_tax_exemption_nl, 0.75).
domain_priors:theater_ratio(bor_tax_exemption_nl, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bor_tax_exemption_nl, extractiveness, 0.68).
narrative_ontology:constraint_metric(bor_tax_exemption_nl, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(bor_tax_exemption_nl, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bor_tax_exemption_nl, tangled_rope).
narrative_ontology:human_readable(bor_tax_exemption_nl, "Dutch Business Succession Scheme (BOR)").
narrative_ontology:topic_domain(bor_tax_exemption_nl, "economic/political").

domain_priors:requires_active_enforcement(bor_tax_exemption_nl).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bor_tax_exemption_nl, family_business_heirs).
narrative_ontology:constraint_beneficiary(bor_tax_exemption_nl, family_business_owners).
narrative_ontology:constraint_beneficiary(bor_tax_exemption_nl, financial_advisors).
narrative_ontology:constraint_victim(bor_tax_exemption_nl, non_business_heirs).
narrative_ontology:constraint_victim(bor_tax_exemption_nl, general_taxpayers).
narrative_ontology:constraint_victim(bor_tax_exemption_nl, competing_non_family_businesses).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-BUSINESS HEIR (SNARE) — Inheriting non-business assets, this agent is trapped in the standard high-tax regime with no access to exemptions. They perceive the BOR as a pure extraction from the common tax base that benefits a privileged class. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.97.
constraint_indexing:constraint_classification(bor_tax_exemption_nl, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FAMILY BUSINESS HEIR (ROPE) — As the direct beneficiary, this agent experiences the BOR as a pure coordination mechanism that solves the problem of business continuity against a punitive tax backdrop. They can arbitrage this scheme against other financial planning tools. d≈0.10, f(d)≈-0.07, σ=1.0 → χ≈-0.05. Negative effective extraction signifies a net subsidy.
constraint_indexing:constraint_classification(bor_tax_exemption_nl, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (TANGLED ROPE) — This observer sees both the genuine coordination function (preventing forced sales) and the massive asymmetric extraction (concentrating wealth, reducing state revenue). The high ε and suppression, combined with a clear beneficiary/victim structure, make Tangled Rope the correct analytical classification. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.94.
constraint_indexing:constraint_classification(bor_tax_exemption_nl, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE POLICYMAKER (TANGLED ROPE) — Constrained by powerful lobbying interests and political mandates, the policymaker must manage the scheme's dual function. They see it as a necessary, if flawed, tool for economic stability, constantly trying to patch loopholes (the extraction) while preserving the core function (the coordination).
constraint_indexing:constraint_classification(bor_tax_exemption_nl, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE FINANCIAL ADVISOR (ROPE) — This agent profits from the scheme's complexity. For them, it is a stable coordination mechanism that creates a market for specialized advisory services. They are mobile and can shift their practice if the rules change.
constraint_indexing:constraint_classification(bor_tax_exemption_nl, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bor_tax_exemption_nl_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bor_tax_exemption_nl, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bor_tax_exemption_nl, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bor_tax_exemption_nl, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bor_tax_exemption_nl_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.68) is high, representing the significant tax revenue forgone by the state, which constitutes a direct transfer of value to a small, wealthy demographic. Suppression (0.75) is high because the scheme completely forecloses the alternative of equal tax treatment for all forms of inheritance, trapping non-business heirs in a comparatively punitive system. Theater Ratio (0.40) is moderate; the narrative of 'saving jobs' is a powerful justification (the theater) for a policy whose primary effect is wealth preservation (the function).
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the beneficiary heir, the BOR is a Rope that solves a critical coordination problem. For the non-business heir, it is a Snare that institutionalizes unfairness. The analytical observer, accounting for both the coordination function and the massive extractive side-effect, classifies it as a Tangled Rope. This divergence is not a contradiction but the core of indexical analysis: the constraint's classification depends on the observer's structural relationship to it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (family business heirs) have arbitrage exit options, leading to a low derived directionality (d) and negative effective extraction (χ), correctly identifying them as subsidized. Victims (non-business heirs) are trapped, leading to a high derived 'd' and extremely high χ, correctly identifying them as the primary targets of the relative extraction. This structural data drives the perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a classic resolution of mandatrophy. To label the BOR a pure Snare would be to ignore its genuine, if overstated, coordination function in preventing business liquidations. To label it a pure Rope would be to ignore the billions in forgone tax revenue and its contribution to wealth inequality. The Tangled Rope classification correctly identifies that the constraint possesses BOTH a real coordination function AND a severe, asymmetric extractive component, which are inextricably linked.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_necessity_vs_wealth_preservation,
    'Is the BOR''s tax relief level truly necessary to prevent widespread forced sales and job losses, or is it primarily a mechanism for dynastic wealth preservation?',
    'Comparative analysis of business continuity rates in jurisdictions with and without similar schemes, controlling for economic sector and firm size.',
    'If proven necessary, the coordination function is stronger, pushing classification towards Rope for more observers. If proven excessive, the extractive function dominates, pushing it towards Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_necessity_vs_wealth_preservation, empirical, 'Distinguishing the BOR''s role in economic necessity versus wealth preservation.').

omega_variable(
    quantify_market_distortion,
    'To what extent does the BOR distort the market for business acquisitions by making intra-family transfers overwhelmingly more tax-efficient than sales to external parties?',
    'Econometric analysis of M&A activity for family-owned vs. non-family-owned firms, and surveys of private equity and corporate acquirers on valuation adjustments due to the BOR.',
    'High distortion confirms high suppression and strengthens the Snare classification from the perspective of competing businesses. Low distortion would weaken it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantify_market_distortion, empirical, 'Measuring the BOR''s distortionary effect on the M&A market.').

omega_variable(
    active_vs_passive_asset_boundary,
    'Where is the conceptual and legal boundary between an ''active business'' eligible for the BOR and a passive investment vehicle holding business-like assets?',
    'Analysis of case law and legislative definitions to determine the porosity of the boundary. A porous boundary allows for greater tax avoidance.',
    'A clear, restrictive boundary limits extraction. A vague or permissive boundary increases the scheme''s extractive potential by allowing passive wealth to be sheltered.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(active_vs_passive_asset_boundary, conceptual, 'The conceptual boundary between active business and passive investment for BOR eligibility.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bor_tax_exemption_nl, 2005, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bor__tr_t2005, bor_tax_exemption_nl, theater_ratio, 2005, 0.25).
narrative_ontology:measurement(bor__tr_t2015, bor_tax_exemption_nl, theater_ratio, 2015, 0.35).
narrative_ontology:measurement(bor__tr_t2025, bor_tax_exemption_nl, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(bor__be_t2005, bor_tax_exemption_nl, base_extractiveness, 2005, 0.5).
narrative_ontology:measurement(bor__be_t2015, bor_tax_exemption_nl, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement(bor__be_t2025, bor_tax_exemption_nl, base_extractiveness, 2025, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bor_tax_exemption_nl, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
