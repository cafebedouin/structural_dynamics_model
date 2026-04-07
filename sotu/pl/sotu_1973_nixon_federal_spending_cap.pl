% ============================================================================
% CONSTRAINT STORY: sotu_1973_nixon_federal_spending_cap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1973_nixon_federal_spending_cap, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sotu_1973_nixon_federal_spending_cap
 *   human_readable: Federal Spending Restraint and Tax Containment Policy (Nixon 1973)
 *   domain: economics/fiscal_policy
 *
 * SUMMARY:
 *   In his 1973 State of the Union address, President Nixon established a
 *   structural constraint against federal spending expansion and tax
 *   increases, framing this policy as a defense against inflation and an
 *   economic burden on taxpayers. This constraint creates a Tangled Rope
 *   structure: it coordinates genuine inflation control and fiscal
 *   predictability (coordination function) while simultaneously extracting
 *   from federal transfer program beneficiaries, public sector workers, and
 *   future generations who bear the cost of foregone public investment. The
 *   constraint functions as both a macroeconomic stabilization mechanism and
 *   a brake on welfare state expansion, making it a classic case of hybrid
 *   coordination-extraction. The extractiveness value (0.58) reflects that
 *   the constraint redistributes resources toward high-income taxpayers and
 *   away from transfer program beneficiaries, with suppression (0.62) high
 *   because beneficiaries lack political organization and exit options. The
 *   theater ratio (0.48) is moderate — inflation control rhetoric is
 *   partially genuine (monetary/supply factors do contribute to inflation)
 *   but also partially cover story for distributional redistribution. The
 *   constraint's legitimacy rests on the empirical claim that uncontrolled
 *   federal spending produces inflation; if this causal relationship is
 *   weaker than asserted, the constraint becomes pure extraction dressed in
 *   economic necessity framing.
 *
 * KEY AGENTS:
 *   - High-Income Taxpayers and Business Sector: Primary beneficiary (institutional/arbitrage) — experience negative effective extraction via prevented tax increases; benefit from inflation control and business planning predictability
 *   - Federal Transfer Program Beneficiaries: Primary victim (powerless/trapped) — bear full cost of constrained program expansion; lack political organization or exit options
 *   - Public Sector Workers and Contractors: Secondary victim (moderate/constrained) — face constrained hiring and wage growth; can exit to private sector at moderate cost
 *   - Federal Agencies and Program Administrators: Institutional actor (institutional/constrained) — experience coordination function (budget predictability) alongside extraction (constrained resources)
 *   - Fiscal Conservative Coalition: Beneficiary coalition (institutional/arbitrage) — institutional actors (Treasury, conservative economists, business groups) who benefit from constraint institutionalization
 *   - Future Generations: Unorganized victim — bear cost of foregone public investment and infrastructure when borrowing is prevented by spending cap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1973_nixon_federal_spending_cap, 0.58).
domain_priors:suppression_score(sotu_1973_nixon_federal_spending_cap, 0.62).
domain_priors:theater_ratio(sotu_1973_nixon_federal_spending_cap, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1973_nixon_federal_spending_cap, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_1973_nixon_federal_spending_cap, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(sotu_1973_nixon_federal_spending_cap, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sotu_1973_nixon_federal_spending_cap, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(sotu_1973_nixon_federal_spending_cap, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1973_nixon_federal_spending_cap, tangled_rope).
narrative_ontology:human_readable(sotu_1973_nixon_federal_spending_cap, "Federal Spending Restraint and Tax Containment Policy (Nixon 1973)").
narrative_ontology:topic_domain(sotu_1973_nixon_federal_spending_cap, "economics/fiscal_policy").

domain_priors:requires_active_enforcement(sotu_1973_nixon_federal_spending_cap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1973_nixon_federal_spending_cap, high_income_taxpayers).
narrative_ontology:constraint_beneficiary(sotu_1973_nixon_federal_spending_cap, business_sector).
narrative_ontology:constraint_beneficiary(sotu_1973_nixon_federal_spending_cap, fiscal_conservative_coalition).
narrative_ontology:constraint_victim(sotu_1973_nixon_federal_spending_cap, federal_transfer_program_beneficiaries).
narrative_ontology:constraint_victim(sotu_1973_nixon_federal_spending_cap, public_sector_employment).
narrative_ontology:constraint_victim(sotu_1973_nixon_federal_spending_cap, social_safety_net_expansion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRANSFER PROGRAM BENEFICIARIES (SNARE) — Trapped by dependency on federal assistance with no structural exit. The spending cap constrains program expansion precisely when demographic pressures (aging, poverty) increase demand. Suppression is high: beneficiaries lack political organization, funding to lobby, or alternative income sources. Extractiveness flows entirely from this group toward fiscal restraint beneficiaries.
constraint_indexing:constraint_classification(sotu_1973_nixon_federal_spending_cap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PUBLIC SECTOR WORKERS AND CONTRACTORS (TANGLED ROPE) — Constrained by the spending cap on hiring and wage growth. However, the constraint does provide coordination function: stabilizes budget predictability and allows long-term agency planning within limits. Exit is costly but possible (private sector employment, relocation). Extraction is moderate, mixed with genuine coordination benefits.
constraint_indexing:constraint_classification(sotu_1973_nixon_federal_spending_cap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FISCAL CONSERVATIVE COALITION (ROPE) — Institutional beneficiary with high exit mobility (can shift policy emphasis, reallocate budgets within caps, invest in private alternatives). The spending cap functions as pure coordination from this perspective: enables predictable fiscal environment, prevents inflation-driven erosion of savings, maintains currency stability. Net beneficiary experiencing negative effective extraction.
constraint_indexing:constraint_classification(sotu_1973_nixon_federal_spending_cap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: HIGH-INCOME TAXPAYERS AND BUSINESS SECTOR (ROPE) — Primary economic beneficiaries. The spending cap prevents tax increases that would fall disproportionately on high earners and corporations. Effective extraction is negative or minimal — the constraint subsidizes this group by preventing redistribution. Coordination function is real: stabilizes business planning, maintains capital formation, reduces inflation uncertainty.
constraint_indexing:constraint_classification(sotu_1973_nixon_federal_spending_cap, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: FEDERAL AGENCIES AND PROGRAM ADMINISTRATORS (TANGLED ROPE) — Constrained by spending caps on their budgets and staffing. However, the constraint provides genuine coordination function: clarifies budget boundaries, enables multi-year planning, reduces competitive inter-agency bidding chaos. Agencies also benefit from inflation control (purchasing power preservation). Extraction is mixed with coordination function.
constraint_indexing:constraint_classification(sotu_1973_nixon_federal_spending_cap, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational horizon, the spending cap can be framed as an immutable economic law: unconstrained spending produces inflation, which erodes all future value. The constraint appears as a natural consequence of monetary physics, not a policy choice. However, the structural data (identifiable beneficiaries, contestable enforcement, policy alternatives) reveals this as a false summit — the constraint is institutionally constructed, not naturally emergent.
constraint_indexing:constraint_classification(sotu_1973_nixon_federal_spending_cap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1973_nixon_federal_spending_cap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1973_nixon_federal_spending_cap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1973_nixon_federal_spending_cap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1973_nixon_federal_spending_cap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sotu_1973_nixon_federal_spending_cap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint redistributes resources from transfer beneficiaries toward high earners through prevented tax increases and constrained program expansion. The extractiveness is not maximal (0.70+) because genuine coordination function exists — the constraint does provide inflation control and fiscal predictability. The escalation from 0.35 to 0.58 over the 1973–1979 interval reflects increasing recognition that the cap is binding constraint rather than loose guideline; extractiveness accumulates as political reality of constrained programs becomes clear. Suppression (0.62): Moderate-high. Transfer program beneficiaries face high barriers to exit (economic dependency) and high costs to political organizing (fragmentation, resource constraints). Public sector workers can exit but at career cost. Suppression is not maximal (0.85+) because the constraint is legislatively visible and theoretically contestable. Theater ratio (0.48): Moderate. The inflation-control rationale is partially genuine (monetary expansion and supply shocks do contribute to inflation) but also partially cover story (spending cap also prevents redistribution regardless of inflation dynamics). The moderate theater reflects mixed genuine and rhetorical content.
 *
 * PERSPECTIVAL GAP:
 *   The constraint's most critical perspectival gap lies between the beneficiary's Rope classification and the trapped victim's Snare classification. From the beneficiary perspective (institutional/arbitrage/immediate/national), the spending cap is pure coordination: it stabilizes inflation expectations, enables business planning, prevents currency erosion. From the transfer beneficiary perspective (powerless/trapped/biographical/national), it is pure extraction: it constrains program expansion precisely when demographic demand increases, and it does so with no exit option. This gap is not resolvable through better measurement of the same constraint — it reflects a genuine structural difference in how the constraint affects the two positions. The beneficiary experiences the constraint as a solution to a collective action problem (coordinating spending to control inflation). The victim experiences it as an external barrier enforcing a distributional outcome (preventing tax increases on high earners). Both experiences are accurate from their structural positions. The false summit risk occurs when the analytical observer elevates the beneficiary's coordination narrative (inflation control as natural law) to universal claim status, erasing the victim's extraction experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are determined by structural position relative to extraction flow. High-income beneficiaries with arbitrage exit options experience d ≈ 0.10–0.20 (full beneficiary toward negative extraction). Transfer program beneficiaries with trapped exit experience d ≈ 0.95 (full target toward maximum extraction). Public sector workers with constrained exit experience d ≈ 0.70 (substantial target, moderate extraction). Federal agencies with institutional exit (budget reallocation) experience d ≈ 0.50 (balanced position, moderate extraction). The fiscal conservative coalition with institutional power and arbitrage options experiences d ≈ 0.05–0.15 (beneficiary toward negative extraction, experiencing the constraint as pure coordination). Suppression is a raw structural property unscaled by directionality: the barriers facing transfer beneficiaries (economic dependency, organizational fragmentation) yield 0.62 suppression regardless of effective extraction chi. The chi formula applies f(d) and scope σ(S) to produce effective extraction chi from base ε = 0.58; high d values (victims) amplify chi, low d values (beneficiaries) dampen or reverse it.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the spending cap serves dual functions: it coordinates inflation control and fiscal predictability (genuine Rope function for beneficiaries) while simultaneously extracting from transfer beneficiary populations (Snare function from their perspective). The Tangled Rope classification captures both functions in a single constraint. The engine's classification should return Tangled Rope from the analytical perspective because the constraint exhibits (1) genuine coordination function (inflation control, fiscal stability), (2) asymmetric extraction (high earners benefit more than low earners), and (3) active enforcement (legislative maintenance of spending limits). The false summit detection should flag the Mountain perspective as naturalization of an institutional arrangement — inflation control as immutable economic law — when the structural reality is politically contingent policy choice. The mandatrophy is not resolved by choosing between Rope and Snare, but by acknowledging both are true from their respective structural positions. The constraint is a Tangled Rope from the system-level analytical perspective because it combines coordination and extraction in an integrated enforcement structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inflation_causation_ambiguity,
    'Does federal spending directly cause inflation, or is inflation driven by monetary policy, global commodity prices, and wage-price dynamics independent of fiscal restraint?',
    'Econometric analysis of spending vs inflation correlation controlling for money supply growth, OPEC oil shocks, wage growth, and supply-side constraints. Cross-national comparison of spending levels vs inflation outcomes.',
    'If spending is primary driver: spending cap is necessary coordination mechanism (upgrade to Rope from some perspectives). If monetary or supply-side factors dominate: spending cap is pure extraction masquerading as inflation control (downgrade to Snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inflation_causation_ambiguity, empirical, 'Causal relationship between federal spending and inflation').

omega_variable(
    transfer_program_effectiveness_contestation,
    'Do federal transfer programs produce genuine poverty reduction and social stability (coordination function), or are they inefficient rent-seeking (extraction from productive economy)?',
    'Long-term outcome tracking: child development, intergenerational mobility, health outcomes for program beneficiaries vs non-beneficiaries. Fiscal multiplier analysis of transfer spending vs tax cuts.',
    'If transfer programs effective: spending cap is pure extraction from beneficiary populations (Snare confirmed from powerless perspective). If ineffective: spending cap may be efficiency-enhancing constraint (Rope or Scaffold from broader perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_program_effectiveness_contestation, empirical, 'Whether federal transfer programs produce genuine social benefit').

omega_variable(
    tax_incidence_distribution,
    'Who truly bears the burden of taxation foregone by the spending cap? Is it high earners (as fiscal conservatives claim) or does the cap prevent borrowing that would otherwise finance public goods (shifting burden to future generations)?',
    'Distributional analysis of tax burden by income quintile. Long-term fiscal balance sheet: present value of foregone public investment vs future tax burden. Lifecycle analysis of public good consumption by cohort.',
    'If cap prevents high-earner taxation: benefits are real (Rope for high earners confirmed). If cap prevents borrowing for public investment: burden shifts to future generations (Snare for young/unborn cohorts, Tangled Rope for current public sector workers).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tax_incidence_distribution, empirical, 'Incidence of tax burden prevented by spending cap').

omega_variable(
    political_sustainability_vs_natural_law,
    'Is the spending cap a politically enforced constraint that could be lifted by legislative action, or does it represent an immutable economic boundary?',
    'Historical analysis of spending cap enforcement: instances of successful breach, political costs of proposed breach, international comparison of spending cap regimes in other democracies.',
    'If politically enforced: constraint is institutional (tangled_rope, snare, or rope depending on perspective), not mountain. If economic boundary: constraint is more nearly mountain. FSM detection hinges on this distinction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(political_sustainability_vs_natural_law, conceptual, 'Whether spending cap is politically enforced vs natural economic law').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1973_nixon_federal_spending_cap, 1973, 1979).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu73_tr_t0, sotu_1973_nixon_federal_spending_cap, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sotu73_tr_t3, sotu_1973_nixon_federal_spending_cap, theater_ratio, 3, 0.45).
narrative_ontology:measurement(sotu73_tr_t6, sotu_1973_nixon_federal_spending_cap, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(sotu73_be_t0, sotu_1973_nixon_federal_spending_cap, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sotu73_be_t3, sotu_1973_nixon_federal_spending_cap, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(sotu73_be_t6, sotu_1973_nixon_federal_spending_cap, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1973_nixon_federal_spending_cap, resource_allocation).
narrative_ontology:affects_constraint(sotu_1973_nixon_federal_spending_cap, inflation_expectations_regime).
narrative_ontology:affects_constraint(sotu_1973_nixon_federal_spending_cap, tax_progressivity_erosion).
narrative_ontology:affects_constraint(sotu_1973_nixon_federal_spending_cap, welfare_state_expansion_boundary).

% DUAL FORMULATION NOTE:
% This constraint exists within a constraint family spanning macroeconomic stabilization, tax policy, and social safety net expansion boundaries. The spending cap is downstream of monetary policy regime choice (fixed vs floating exchange rates, inflation targeting) and feeds forward into welfare state expansion dynamics. Decomposition: the inflation-control coordination function and the distributional extraction function are structurally distinct constraints operating with different ε values; they are presented as unified in policy rhetoric but can be analyzed separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1973_nixon_federal_spending_cap, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
