% ============================================================================
% CONSTRAINT STORY: sotu_1999_clinton_social_security_surplus_dedication
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1999_clinton_social_security_surplus_dedication, []).

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
 *   constraint_id: sotu_1999_clinton_social_security_surplus_dedication
 *   human_readable: Social Security Solvency Dedication (1999 Clinton Proposal)
 *   domain: social_policy/fiscal_governance
 *
 * SUMMARY:
 *   In 1999, the Clinton administration proposed dedicating 60% of projected
 *   budget surpluses over 15 years (1999-2014) to Social Security Trust Fund
 *   solvency, with a small portion invested in private markets to earn higher
 *   returns. This constraint addresses an imminent demographic crisis: under
 *   then-current projections, the Social Security Trust Fund would be
 *   insolvent by 2032 without benefit cuts or payroll tax increases. The
 *   mechanism restructures fiscal allocation—redirecting surplus revenue away
 *   from discretionary spending, deficit reduction, or other priorities—to
 *   extend solvency to 2054. The constraint exhibits characteristics of all
 *   six classification types depending on observer position: current retirees
 *   see pure coordination (solvency guarantee); future retirees experience
 *   mixed coordination and extraction (benefit guarantee but locked pathway);
 *   discretionary spending programs experience pure extraction (budget cuts
 *   with no compensating benefit); the fiscal authority experiences mixed
 *   coordination and extraction (solving crisis while losing flexibility);
 *   the private market component functions as a degraded institutional
 *   mechanism (symbolically important but structurally minimal); reform
 *   coalitions see a temporary solution with sunset (Scaffold); and the
 *   analytical observer risks misclassifying demographic pressure as natural
 *   law (false summit). The theater ratio reflects that the private market
 *   investment component is more ideological signaling (proving market
 *   confidence) than structural innovation, while the genuine coordination
 *   function (surplus dedication to solvency) remains primary.
 *
 * KEY AGENTS:
 *   - Current Retirees: Primary beneficiary (powerless/trapped) — experience guarantee of benefit continuity and no solvency crisis during their lifespans
 *   - Future Retirees (2032-2054): Co-beneficiary (moderate/constrained) — receive solvency extension and benefit continuity but locked into a specific policy path without flexibility to choose alternative solutions
 *   - Discretionary Spending Constituencies: Primary victim (moderate/constrained) — education, infrastructure, defense, and other programs lose access to 60% of budget surplus for 15 years with no compensating benefit
 *   - Federal Fiscal Authority: Institutional actor (institutional/arbitrage) — genuinely solves solvency crisis (coordination) but sacrifices budgetary flexibility and opportunity to reallocate surplus to emerging priorities
 *   - Private Market Investment Component: Institutional mechanism (institutional/arbitrage) — claims to enhance returns but remains minimal and ideologically rather than functionally important
 *   - Organized Reform Coalition: Organized agents (organized/constrained) — advocacy groups for comprehensive reform see constraint as temporary but delaying full structural change
 *   - Demographic Pressures: Civilizational context (analytical/analytical) — creates the structural necessity that the constraint addresses, but appears as natural law only if contingent policy choices (tax cap, retirement age, redistribution level) are treated as immutable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1999_clinton_social_security_surplus_dedication, 0.38).
domain_priors:suppression_score(sotu_1999_clinton_social_security_surplus_dedication, 0.45).
domain_priors:theater_ratio(sotu_1999_clinton_social_security_surplus_dedication, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1999_clinton_social_security_surplus_dedication, extractiveness, 0.38).
narrative_ontology:constraint_metric(sotu_1999_clinton_social_security_surplus_dedication, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(sotu_1999_clinton_social_security_surplus_dedication, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1999_clinton_social_security_surplus_dedication, tangled_rope).
narrative_ontology:human_readable(sotu_1999_clinton_social_security_surplus_dedication, "Social Security Solvency Dedication (1999 Clinton Proposal)").
narrative_ontology:topic_domain(sotu_1999_clinton_social_security_surplus_dedication, "social_policy/fiscal_governance").

domain_priors:requires_active_enforcement(sotu_1999_clinton_social_security_surplus_dedication).
narrative_ontology:has_sunset_clause(sotu_1999_clinton_social_security_surplus_dedication).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1999_clinton_social_security_surplus_dedication, current_retirees).
narrative_ontology:constraint_beneficiary(sotu_1999_clinton_social_security_surplus_dedication, future_retirees_2032_2054).
narrative_ontology:constraint_victim(sotu_1999_clinton_social_security_surplus_dedication, discretionary_spending_programs).
narrative_ontology:constraint_victim(sotu_1999_clinton_social_security_surplus_dedication, budget_flexibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Current beneficiaries experience this constraint as pure coordination: the surplus dedication prevents imminent insolvency and guarantees benefit continuity. They have no exit option (retirement is irreversible) but receive genuine benefit. Extraction is minimal because their interest aligns with the policy design.
constraint_indexing:constraint_classification(sotu_1999_clinton_social_security_surplus_dedication, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Future beneficiaries experience mixed coordination and extraction. The constraint genuinely solves their solvency problem (coordination benefit) but locks them into a specific solution path with limited flexibility. They face constrained exit: they can advocate for benefit changes but cannot exit the system. Moderate extraction reflects the benefit-cost asymmetry across cohorts.
constraint_indexing:constraint_classification(sotu_1999_clinton_social_security_surplus_dedication, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Discretionary spending programs (education, infrastructure, defense modernization) experience this constraint as pure extraction: their available budget shrinks by 60% of surplus for 15 years, with no corresponding benefit and constrained ability to escape. They can advocate for policy change but face political barriers (Social Security solvency is salient to voters). High suppression reflects the structural difficulty of breaking the surplus commitment without political crisis.
constraint_indexing:constraint_classification(sotu_1999_clinton_social_security_surplus_dedication, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% The fiscal authority experiences this as mixed coordination and extraction. It genuinely solves a long-term solvency crisis (coordination benefit) but locks itself into a rigid allocation rule that reduces budgetary flexibility and prevents opportunistic reallocation to emerging priorities. Arbitrage exit reflects that Congress can technically revoke the commitment at any time, but political costs are high (triggers benefit-cut or tax-increase fears).
constraint_indexing:constraint_classification(sotu_1999_clinton_social_security_surplus_dedication, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% The private market investment portion (small percentage of the surplus) functions as a degraded institutional mechanism. It is rhetorically presented as efficiency-enhancing (higher returns) but remains minimal and administratively detached from the primary trust fund accumulation. The mechanism persists as a symbolic endorsement of market-based solutions without creating genuine hybrid public-private architecture. Theater ratio reflects that the private investment component is more about signaling ideological alignment than about structural importance.
constraint_indexing:constraint_classification(sotu_1999_clinton_social_security_surplus_dedication, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Organized groups advocating for comprehensive Social Security reform see this constraint as temporary: the 15-year horizon creates a sunset implicit in the design. They experience it as constraining (the commitment delays full reform) but solvable (the 2054 date creates a natural deadline for reconsidering the system). Organized status reflects their capacity to lobby for sunset mechanics or modification clauses.
constraint_indexing:constraint_classification(sotu_1999_clinton_social_security_surplus_dedication, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% From a civilizational perspective, the solvency crisis reflects demographic immutability: increasing life expectancy and declining birth rates create an inherent structural imbalance in pay-as-you-go systems. Some form of rebalancing (higher taxes, lower benefits, or larger trust fund reserves) appears as an inescapable natural law. However, the structural data contradicts this—specific policy choices (degree of redistribution, retirement age, payroll tax cap) are contingent, not natural. The engine will identify this as a false summit.
constraint_indexing:constraint_classification(sotu_1999_clinton_social_security_surplus_dedication, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

% The analytical observer sees this as a hybrid mechanism: it genuinely coordinates a response to demographic pressure (coordination function) while also extracting budgetary flexibility and opportunity cost (asymmetric extraction). The 15-year horizon and mixed public-private structure create artificial constraints that benefit some stakeholders (retirees during the solvency window) at cost to others (discretionary spending constituencies). Classification reflects both genuine problem-solving and structural inefficiency.
constraint_indexing:constraint_classification(sotu_1999_clinton_social_security_surplus_dedication, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1999_clinton_social_security_surplus_dedication_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1999_clinton_social_security_surplus_dedication, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1999_clinton_social_security_surplus_dedication, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1999_clinton_social_security_surplus_dedication, TR),
    TR >= 0.70.

:- end_tests(sotu_1999_clinton_social_security_surplus_dedication_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint solves a genuine structural problem (solvency) but at the cost of budgetary opportunity—60% of surplus diverted from other uses for 15 years creates real extraction from discretionary spending programs. Initial extractiveness (0.22) reflects the early years of robust surpluses and minimal actual constraint; it rises to 0.38 by year 10 as surpluses tighten and opportunity costs accumulate. The value is moderate rather than high because the constraint has genuine benefits (solvency) and a sunset clause (temporary). Suppression (0.45): Moderate-high. Discretionary spending constituencies face significant barriers to escaping the constraint: Social Security solvency is politically salient to voters, and attempting to override the dedication triggers benefit-cut or tax-increase fears. Reversing the commitment requires political capital that other priorities cannot command. However, suppression is not maximal (≥0.60) because the constraint has explicit sunset (2014) and can be modified or abandoned after that date. Theater ratio (0.52): Moderate. The private market investment component contributes theatrical elements—it signals faith in market efficiency and private-sector strength—but the genuine coordination function (surplus dedication) dominates. The overall mechanism is more functional than performative.
 *
 * PERSPECTIVAL GAP:
 *   The gap between current retirees (Rope) and discretionary spending programs (Snare) is maximal: the same mechanism that guarantees benefits to one set of agents extracts opportunity from another set with no overlap in their interests. The gap between future retirees (Tangled Rope) and reform advocates (Scaffold) reflects disagreement on whether the 15-year commitment is sufficient solution or inadequate stopgap. The gap between the fiscal authority's experience (Tangled Rope—solving crisis while losing flexibility) and the mountain perspective (false summit—treating demographic pressure as natural law) reveals that what appears necessary to some observers is contingent institutional choice to others.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values vary significantly across perspectives, driven by beneficiary/victim declarations and exit options. Current retirees are beneficiaries with trapped exit (powerless demographic position) → low d → low/negative χ → Rope. Future retirees are beneficiaries with constrained exit (can advocate but cannot escape system) → moderate d → moderate χ → Tangled Rope. Discretionary spending victims with constrained exit (can lobby but face political barriers) → moderate d but victim status → moderate-high d → high χ → Snare. The fiscal authority is both beneficiary (solves crisis) and partially victim (loses flexibility) with arbitrage exit (can technically revoke but at high political cost) → moderate d → moderate χ → Tangled Rope. The analytical observer treats the constraint as analytically exampled from the civilizational perspective, revealing that demographic pressure is not immutable but contingent on specific policy parameters (retirement age, tax cap, replacement rate) treated as fixed.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION OF MANDATROPHY: This constraint avoids mandatrophy by accepting that multiple classification types are legitimate from different structural positions. The mandatrophy question 'Is this Rope or Snare?' is resolved by noting that it is Rope from current retirees' position and Snare from discretionary spending's position. Both are correct because the constraint's structure genuinely coordinates for one constituency while extracting from another. The 15-year sunset is crucial to mandatrophy resolution: it prevents the mechanism from being classified as pure extraction (which would require permanent suppression of alternatives). The sunset creates Scaffold possibility—the constraint is temporary, and organized actors can anticipate renegotiation. The false summit (mountain perspective) is correctly identified by the engine because the structural data shows beneficiaries (current/future retirees) and victims (discretionary spending), contradicting the natural-law claim. The constraint resolves by admitting that solvency crisis is real (demographic pressure is structural) but the solution path is contingent (could be higher taxes, later retirement age, benefit reduction, or hybrid reserve accumulation—the constraint chooses one specific path, making it extractive relative to alternatives).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demographic_projections_accuracy,
    'Do the demographic and economic projections underlying the 2032 solvency crisis accurately reflect future population, labor participation, and wage growth?',
    'Comparison of 1999 projections against actual 2025 demographic data; sensitivity analysis on fertility, immigration, and longevity assumptions',
    'If projections overestimated crisis severity: the constraint extracts surplus for a problem that was never as severe, reclassifying from Tangled Rope toward Snare. If projections underestimated crisis severity: the 2054 sunset proves premature, and the constraint is inadequate—reclassifying toward Scaffold (temporary solution with insufficient duration).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demographic_projections_accuracy, empirical, 'Accuracy of demographic and solvency projections underlying the 15-year commitment').

omega_variable(
    private_market_return_realization,
    'Does the private market component actually achieve higher returns than Treasury bonds, or does it introduce volatility and timing risk that offset nominal gains?',
    'Historical comparison of Social Security Trust Fund-eligible private returns vs. Treasury rates 1999-2025; analysis of rebalancing costs and market timing effects',
    'If private returns significantly exceed Treasury: the mechanism creates genuine value and is less extractive than pure-accumulation (Rope toward pure coordination). If returns underperform or create volatility: the private component is symbolic theater, and extractiveness rises (Snare reclassification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(private_market_return_realization, empirical, 'Whether private market returns exceed Treasury bonds net of volatility and rebalancing costs').

omega_variable(
    surplus_realization_trajectory,
    'Do budget surpluses actually materialize as projected, or do they evaporate due to recession, structural revenue decline, or spending pressures?',
    'Comparison of 1999 surplus projections against actual annual budget outcomes 2000-2015; identification of variance drivers (recession, tax changes, spending changes)',
    'If surpluses fail to materialize: the constraint locks in a commitment that cannot be funded, creating forced austerity in other programs (Snare reclassification, high suppression). If surpluses materialize: the mechanism functions as designed (Tangled Rope stability).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(surplus_realization_trajectory, empirical, 'Whether projected budget surpluses actually materialize over the 15-year period').

omega_variable(
    sunset_clause_enforceability,
    'Is the 15-year sunset clause politically enforceable, or does the constraint persist beyond its design horizon through institutional inertia?',
    'Historical analysis of expired budget constraints and dedicated revenue rules; examination of 2014-2015 debate over whether the surplus dedication expired or continued implicitly',
    'If sunset is enforced: Scaffold classification is appropriate (temporary constraint with exit). If sunset is ignored: constraint becomes Piton (inertial, degraded, theater-dependent). If deadline triggers comprehensive reform: Scaffold prediction confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_clause_enforceability, empirical, 'Political enforceability of the 15-year sunset clause').

omega_variable(
    intergenerational_equity_incidence,
    'Does the constraint create net intergenerational equity (protecting future retirees from benefit cuts) or net inequity (shifting costs across cohorts in ways that disadvantage specific birth cohorts)?',
    'Generational accounting analysis: comparison of lifetime tax-benefit ratios across birth cohorts with and without the surplus dedication',
    'If constraint improves equity: Rope classification is stronger (pure coordination benefit). If constraint creates new inequities (e.g., protects some future retirees while disadvantaging others): extractiveness rises and classification shifts toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_equity_incidence, empirical, 'Net intergenerational equity effects of the constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1999_clinton_social_security_surplus_dedication, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu_tr_t0, sotu_1999_clinton_social_security_surplus_dedication, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sotu_tr_t5, sotu_1999_clinton_social_security_surplus_dedication, theater_ratio, 5, 0.48).
narrative_ontology:measurement(sotu_tr_t10, sotu_1999_clinton_social_security_surplus_dedication, theater_ratio, 10, 0.52).

% Extraction over time
narrative_ontology:measurement(sotu_be_t0, sotu_1999_clinton_social_security_surplus_dedication, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(sotu_be_t5, sotu_1999_clinton_social_security_surplus_dedication, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(sotu_be_t10, sotu_1999_clinton_social_security_surplus_dedication, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1999_clinton_social_security_surplus_dedication, resource_allocation).
narrative_ontology:affects_constraint(sotu_1999_clinton_social_security_surplus_dedication, social_security_payroll_tax_cap).
narrative_ontology:affects_constraint(sotu_1999_clinton_social_security_surplus_dedication, retirement_age_adjustment_mechanism).
narrative_ontology:affects_constraint(sotu_1999_clinton_social_security_surplus_dedication, trust_fund_reserve_accumulation_targets).

% DUAL FORMULATION NOTE:
% The surplus dedication constraint is downstream of demographic pressure (imminent solvency date) and upstream of specific policy alternatives (tax increases, benefit cuts, retirement age). This story models the surplus dedication as a hybrid mechanism; separate stories model pure-tax-increase and pure-benefit-reduction pathways with different ε values reflecting their specific extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
