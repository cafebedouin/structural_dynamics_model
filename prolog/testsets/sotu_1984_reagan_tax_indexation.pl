% ============================================================================
% CONSTRAINT STORY: sotu_1984_reagan_tax_indexation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1984_reagan_tax_indexation, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sotu_1984_reagan_tax_indexation
 *   human_readable: Tax Bracket Indexation to Inflation (Reagan 1984)
 *   domain: economics/fiscal_policy
 *
 * SUMMARY:
 *   The 1984 Tax Indexation constraint represents a structural mechanism that
 *   prevents inflation from automatically pushing wage-earners into higher
 *   effective tax brackets. Institutionalized as part of the Tax Equity and
 *   Fiscal Responsibility Act (TEFRA) amendments, it solves a genuine
 *   coordination problem: before 1985, inflation eroded the real value of tax
 *   brackets, creating an implicit tax increase that required explicit
 *   legislative action to reverse. Indexation converts this into an automatic
 *   adjustment, protecting real after-tax purchasing power. However, the
 *   constraint exhibits characteristic tangled-rope structure: genuine
 *   coordination benefit (wage-earners protected from bracket creep) layered
 *   with asymmetric fiscal extraction (government loses implicit revenue
 *   source, forcing either explicit tax increases or spending cuts). The
 *   rising extractiveness over the 20-year measurement interval reflects
 *   growing fiscal pressure as the cumulative cost of lost bracket-creep
 *   revenue compounds. The low theater ratio (0.15) indicates this is a
 *   transparent mechanism, not a performative one — indexation actually
 *   functions as designed, unlike a piton's degraded institutional ritual.
 *
 * KEY AGENTS:
 *   - Wage-Earners (Powerless/Trapped): Primary beneficiaries protected from bracket creep; cannot exit employment or income reporting.
 *   - Middle-Income Working Families (Powerless/Trapped): Experience protection from real income erosion but structurally dependent on employment.
 *   - Labor Unions and Worker Organizations (Organized/Constrained): Advocate for indexation; constrained by political cycles and fiscal tradeoffs.
 *   - Federal Government (Institutional/Constrained): Trapped by indexation; loses implicit revenue but cannot politically reverse the constraint.
 *   - Capital Markets and High-Income Earners (Institutional/Arbitrage): Can arbitrage around indexation; benefit disproportionately from reduced government spending.
 *   - Congress and Tax Policymakers (Powerful/Mobile): Can revise indexation through legislation; mobile exit options but face political constraints.
 *   - Future Generations (Powerless/Trapped): Bear extraction cost through reduced government capacity for services, infrastructure, and social insurance.
 *   - Analytical Observer (Analytical/Analytical): Views constraint as institutionalized fiscal choice with path-dependent political immobility.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1984_reagan_tax_indexation, 0.32).
domain_priors:suppression_score(sotu_1984_reagan_tax_indexation, 0.25).
domain_priors:theater_ratio(sotu_1984_reagan_tax_indexation, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1984_reagan_tax_indexation, extractiveness, 0.32).
narrative_ontology:constraint_metric(sotu_1984_reagan_tax_indexation, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(sotu_1984_reagan_tax_indexation, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1984_reagan_tax_indexation, tangled_rope).
narrative_ontology:human_readable(sotu_1984_reagan_tax_indexation, "Tax Bracket Indexation to Inflation (Reagan 1984)").
narrative_ontology:topic_domain(sotu_1984_reagan_tax_indexation, "economics/fiscal_policy").

domain_priors:requires_active_enforcement(sotu_1984_reagan_tax_indexation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1984_reagan_tax_indexation, wage_earners_middle_income).
narrative_ontology:constraint_beneficiary(sotu_1984_reagan_tax_indexation, working_families).
narrative_ontology:constraint_victim(sotu_1984_reagan_tax_indexation, government_revenue_capacity).
narrative_ontology:constraint_victim(sotu_1984_reagan_tax_indexation, future_discretionary_spending).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WAGE EARNER (ROPE) — Trapped within employment and income reporting but benefits from the constraint. Indexation prevents bracket creep (implicit inflation tax). The mechanism coordinates genuine collective action benefit: all wage earners protected from erosion of real after-tax income. No extraction experienced — the powerless agent gains protection at no cost.
constraint_indexing:constraint_classification(sotu_1984_reagan_tax_indexation, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LABOR UNIONS (TANGLED ROPE) — Organized agents benefit from inflation protection but are constrained by political cycles and fiscal policy tradeoffs. The constraint both enables (protects nominal wage gains from tax erosion) and extracts (reduces government capacity for social spending that benefits workers). Mixed experience: genuine coordination benefit layered with asymmetric fiscal extraction.
constraint_indexing:constraint_classification(sotu_1984_reagan_tax_indexation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL GOVERNMENT (SNARE) — Structurally trapped by indexation mechanism. Government cannot use inflation as implicit revenue source (erosion of nominal tax thresholds no longer inflates receipts). Experiences pure extraction: benefits flow to wage earners; costs concentrate on government's discretionary spending capacity. Constrained exit: repealing indexation is politically infeasible despite fiscal pressure. No coordination benefit to government — extraction runs toward citizens.
constraint_indexing:constraint_classification(sotu_1984_reagan_tax_indexation, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CAPITAL/HIGH-INCOME ACTORS (TANGLED ROPE) — Can arbitrage around indexation through income timing, deferred compensation, and capital gains. Coordination benefit: indexation provides stable after-tax income predictability enabling longer investment horizons. Extraction benefit: high earners benefit disproportionately from reduced government spending on redistribution and social insurance (government cuts benefits to offset lost bracket-creep revenue). Mixed: genuine coordination + asymmetric income-dependent extraction favoring capital over labor.
constraint_indexing:constraint_classification(sotu_1984_reagan_tax_indexation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CONGRESS (SCAFFOLD) — Powerful agents with mobile exit options see indexation as temporary political constraint (sunset). Tax legislation can revise or repeal indexation; recent legislative efforts (2017 TCJA, proposed alternative minimum tax reforms) show the constraint is not immutable. Short-term horizon: indexation enables predictable fiscal planning. Sunset logic: future Congresses can address through tax reform. Theater low: mechanism is transparent, not performative.
constraint_indexing:constraint_classification(sotu_1984_reagan_tax_indexation, scaffold,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (PITON) — From a civilizational view, tax bracket indexation appears as a permanent institutional feature (immovable constraint). However, structural data reveals degradation: indexation persists despite ongoing fiscal pressure and debates about repealing it (2017, 2021 tax reform proposals). Theater ratio (0.15) is low — the mechanism is transparent, not performative. But the constraint's function (protecting real wages) has been partly displaced by explicit income-based tax cuts. Indexation operates as a vestigial institutional arrangement maintained through political path dependence, not because it solves the original problem optimally.
constraint_indexing:constraint_classification(sotu_1984_reagan_tax_indexation, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1984_reagan_tax_indexation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1984_reagan_tax_indexation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1984_reagan_tax_indexation, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1984_reagan_tax_indexation, TR),
    TR >= 0.70.

:- end_tests(sotu_1984_reagan_tax_indexation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate. The constraint extracts by removing government's implicit inflation tax but provides genuine coordination benefit to wage-earners. The extraction is not pure coercion (suppression 0.25 is moderate) — beneficiaries experience real protection, not just reduced coercion. The rising trajectory (0.15 → 0.35 over 20 years) reflects accumulating fiscal pressure as the revenue loss compounds: with higher cumulative inflation, the cost of indexation grows relative to the original baseline. Suppression (0.25): Moderate-low. Wage-earners face moderate barriers to exit (employment is necessary; income cannot be hidden entirely), but the constraint protects rather than suppresses them — it prevents an implicit tax increase. Government faces higher suppression (cannot politically reverse indexation despite fiscal pressure), but government is the institutional beneficiary of extraction in the opposite direction (structural dependence on wage-earner cooperation). Theater ratio (0.15): Low. Indexation is a transparent automatic mechanism, not performative. Tax brackets adjust with published inflation data; the mechanism is obvious and functional. Unlike a piton (high theater, low function), indexation actually accomplishes what it claims.
 *
 * PERSPECTIVAL GAP:
 *   The wage-earner sees protection and coordination (Rope: all workers benefit together without coercion). The government sees entrapment and extraction (Snare: loses revenue with no political escape). Congress sees a temporary constraint (Scaffold: can revise through legislation). High-income earners see mixed benefit (Tangled Rope: protection for income predictability + extraction benefit through reduced redistribution). Labor organizations see coordination with fiscal tradeoffs (Tangled Rope: workers protected but government capacity constrained). The analytical observer risks misclassifying as immovable (Piton/Mountain: 'this is how inflation works now'), but the political history (repeated attempts to repeal, debate in 2017-2021, structural contingency) reveals it as a path-dependent institutional arrangement, not a natural law. The perspectival gap reveals that indexation simultaneously protects and extracts: the protection is real; the extraction (concentrated on future spending capacity, especially for the poor) is equally real but less visible.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries (wage-earners) are structurally trapped with no exit options, yet experience low or negative effective extraction because the constraint protects them. The formula χ = ε × f(d) × σ(S) drives this: wage-earners have d ≈ 0.15 (beneficiaries with no exit options = low d value per derivation chain) and f(0.15) ≈ -0.01 (slightly negative), yielding χ ≈ -0.003 (negative extraction — the constraint subsidizes them). The government, as the victim of lost revenue, has d ≈ 0.85 (institutional actor bearing fiscal cost), yielding f(0.85) ≈ 1.15 (high extraction factor). The scope modifier σ(national) = 1.0 preserves this. High-income earners occupy an intermediate position: they benefit from reduced government spending (which decreases redistributive extraction) but also from stable after-tax income predictability, yielding intermediate directionality. The tangled-rope classification arises from the combination: genuine coordination benefit (protecting real wages) coexists with real asymmetric fiscal extraction (government's revenue capacity decreases, forcing spending cuts that disproportionately affect the poor).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION POINT: The mandatrophy (coordination vs. pure extraction) resolves via careful definition of the extraction flow. Indexation is NOT pure extraction because it genuinely solves a coordination problem: before indexation, wage-earners and government were in a repeated game where inflation eroded wages unless workers demanded (and government granted) explicit compensation. Indexation converts this into automatic protection, benefiting both parties by reducing the annual renegotiation cycle. The extraction emerges not from the coordination mechanism but from its fiscal consequences: government loses revenue flexibility and must cut spending or raise explicit taxes. This extraction is real but is a *consequence* of coordination, not the coordination mechanism itself. The classification as Tangled Rope is correct: genuine coordination (automatic bracket adjustment) + real extraction (fiscal pressure on spending). The mandatrophy is resolved by separating the mechanism (rope: beneficiary and victim both benefit from certainty and automation) from the fiscal consequences (extraction: someone bears the cost of lost revenue).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bracket_creep_reversion_risk,
    'If indexation is repealed, does bracket creep inflation-tax return silently or is it politically impossible to reinstitute without explicit legislation?',
    'Historical precedent analysis: pre-1985 bracket-creep tolerance and post-1984 indexation political support. Voter awareness surveys on bracket-creep vs explicit tax increases.',
    'If silent reversion is politically feasible: indexation is a low-suppression constraint (voters unaware of implicit tax rise). If explicitly impossible: indexation is high-suppression (political irreversibility despite fiscal pressure). Changes classification from Tangled Rope to Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bracket_creep_reversion_risk, empirical, 'Whether bracket creep can return silently or requires explicit reinstatement').

omega_variable(
    real_wage_protection_sufficiency,
    'How much of the post-1984 real wage stagnation is attributable to indexation removing the implicit wage tax vs. to structural changes (globalization, union decline, productivity slowdown)?',
    'Counterfactual fiscal analysis: modeling pre-indexation bracket-creep trajectory under post-1984 inflation assumptions. Cross-country comparison of real wage trends in indexed vs. non-indexed systems.',
    'If indexation accounts for >50% of real wage protection: beneficiary classification correct. If <20%: indexation is performative protection (piton signal), and actual wage decline driven by structural factors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(real_wage_protection_sufficiency, empirical, 'How much real wage protection indexation provides vs. structural forces').

omega_variable(
    government_revenue_substitution_mechanism,
    'Did government offset lost bracket-creep revenue through explicit tax increases, spending cuts, or deficit growth? If cuts, which beneficiaries absorbed them?',
    'Federal budget analysis 1984-present: revenue composition, spending levels by category, deficit trajectory. Attribution analysis: which populations bore disproportionate cuts (Medicare, Medicaid, education, infrastructure)?',
    'If offset by spending cuts: extraction victim is future generations (intergenerational snare). If offset by deficits: extraction is deferred (piton degradation signal). If explicit taxes raised: constraint is weaker than modeled.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(government_revenue_substitution_mechanism, empirical, 'How government offset lost bracket-creep revenue').

omega_variable(
    low_income_earner_bracket_creep_exemption,
    'Does the earned income tax credit (EITC) expansion and standard deduction indexation since 1984 constitute a compensating mechanism for low-income earners, or is it separate policy?',
    'Legislative history: EITC expansion timing relative to bracket indexation. Distributional analysis: net effect on low-income earners (EITC gains vs. bracket-creep protection vs. reduced government services).',
    'If EITC is true compensation: low-income beneficiary status is correct (Rope perspective). If EITC is separate policy: low-income wage earners are partially covered by indexation but fully exposed to spending-cut extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(low_income_earner_bracket_creep_exemption, empirical, 'Whether EITC expansion compensates for indexation''s spending reduction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1984_reagan_tax_indexation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taxidx_tr_t0, sotu_1984_reagan_tax_indexation, theater_ratio, 0, 0.08).
narrative_ontology:measurement(taxidx_tr_t10, sotu_1984_reagan_tax_indexation, theater_ratio, 10, 0.12).
narrative_ontology:measurement(taxidx_tr_t20, sotu_1984_reagan_tax_indexation, theater_ratio, 20, 0.15).

% Extraction over time
narrative_ontology:measurement(taxidx_be_t0, sotu_1984_reagan_tax_indexation, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(taxidx_be_t10, sotu_1984_reagan_tax_indexation, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(taxidx_be_t20, sotu_1984_reagan_tax_indexation, base_extractiveness, 20, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1984_reagan_tax_indexation, resource_allocation).
narrative_ontology:boltzmann_floor_override(sotu_1984_reagan_tax_indexation, 0.12).
narrative_ontology:affects_constraint(sotu_1984_reagan_tax_indexation, progressive_tax_rate_effectiveness).
narrative_ontology:affects_constraint(sotu_1984_reagan_tax_indexation, government_spending_capacity_fiscal_pressure).
narrative_ontology:affects_constraint(sotu_1984_reagan_tax_indexation, inflation_wage_dynamics).

% DUAL FORMULATION NOTE:
% Tax bracket indexation is downstream of the inflation-wage linkage but represents a distinct structural constraint. The upstream constraint (inflation_wage_dynamics) describes how nominal wages adjust to inflation; indexation is a policy response to the coordination problem that inflation creates for tax brackets. The downstream constraints (progressive_tax_effectiveness, fiscal_pressure) are affected by indexation's revenue consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
