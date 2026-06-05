% ============================================================================
% CONSTRAINT STORY: sotu_1993_clinton_deficit_reduction_program
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1993_clinton_deficit_reduction_program, []).

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
 *   constraint_id: sotu_1993_clinton_deficit_reduction_program
 *   human_readable: Deficit Reduction Through Concurrent Tax Increases and Spending Cuts (1993)
 *   domain: economic_policy/fiscal_governance
 *
 * SUMMARY:
 *   The 1993 Clinton deficit reduction program represents a structural
 *   mechanism that simultaneously increases revenue (through income and
 *   capital gains tax increases) and reduces spending (through agency-wide
 *   discretionary cuts and federal workforce reduction). This constraint
 *   exhibits the full tension between competing indexical perspectives. From
 *   the deficit reduction coalition's view (Federal Reserve, institutional
 *   investors, future-focused fiscal reformers), the program is pure
 *   coordination: lowering government borrowing reduces interest rates and
 *   frees capital for private investment. From high-income earners' and
 *   federal employees' perspectives, it is extraction: they bear concentrated
 *   costs (higher taxes, lost employment) while benefits accrue to savers and
 *   investors. The program inversion of post-Reagan fiscal policy —
 *   substituting deficit reduction for supply-side growth — activates a
 *   fundamental conflict between distributional assumptions about who should
 *   adjust. The constraint's theater ratio (0.38) reflects that deficit
 *   reduction targets are concrete (reduce deficit to 2% of GDP by 2000)
 *   rather than purely performative, but political rhetoric significantly
 *   exceeds economic mechanism. The program's classification as tangled rope
 *   reflects genuine coordination (lower interest rates) coupled with
 *   asymmetric extraction (concentrated costs on tax targets and federal
 *   workforce). The mountain perspective reveals the most significant
 *   analytical risk: naturalizing what is actually a political choice about
 *   fiscal adjustment as an immutable law of macroeconomics.
 *
 * KEY AGENTS:
 *   - Federal Employees and Government Service Recipients: Primary victims (powerless/trapped) — face mandatory employment reduction and service provision cuts with no alternative income sources
 *   - High-Income Earners: Primary victims (moderate/constrained) — bear targeted tax increases on income and capital gains; exit options exist (relocation, income shifting) but at significant cost
 *   - Deficit Reduction Coalition: Primary beneficiary and enforcer (institutional/constrained) — Federal Reserve, institutional investors, bond markets, fiscal reform advocates; experience mixed coordination (lower interest rates) and extraction (imposing costs on others)
 *   - Long-Term Savers and Private Investors: Secondary beneficiary (institutional/arbitrage) — experience pure coordination benefit through lower interest rates and freed-up credit markets; global exit options reduce coercion
 *   - Reform-Oriented Congressional Coalition: Organizing agent (organized/constrained) — political actors committing to fiscal discipline; perceive sunset pathway as deficit reduction is achieved
 *   - Large Corporations and Multinationals: Mixed position (powerful/mobile) — experience both coordination benefit (lower capital costs) and extraction (higher tax burden); mobile exit options provide agency
 *   - Analytical Observer: Universal view (analytical/analytical) — risks naturalizing contingent political choice as law of economics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1993_clinton_deficit_reduction_program, 0.48).
domain_priors:suppression_score(sotu_1993_clinton_deficit_reduction_program, 0.52).
domain_priors:theater_ratio(sotu_1993_clinton_deficit_reduction_program, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1993_clinton_deficit_reduction_program, extractiveness, 0.48).
narrative_ontology:constraint_metric(sotu_1993_clinton_deficit_reduction_program, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(sotu_1993_clinton_deficit_reduction_program, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1993_clinton_deficit_reduction_program, tangled_rope).
narrative_ontology:human_readable(sotu_1993_clinton_deficit_reduction_program, "Deficit Reduction Through Concurrent Tax Increases and Spending Cuts (1993)").
narrative_ontology:topic_domain(sotu_1993_clinton_deficit_reduction_program, "economic_policy/fiscal_governance").

domain_priors:requires_active_enforcement(sotu_1993_clinton_deficit_reduction_program).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1993_clinton_deficit_reduction_program, long_term_savers).
narrative_ontology:constraint_beneficiary(sotu_1993_clinton_deficit_reduction_program, private_sector_investors).
narrative_ontology:constraint_beneficiary(sotu_1993_clinton_deficit_reduction_program, deficit_reduction_coalition).
narrative_ontology:constraint_victim(sotu_1993_clinton_deficit_reduction_program, high_income_earners).
narrative_ontology:constraint_victim(sotu_1993_clinton_deficit_reduction_program, government_service_recipients).
narrative_ontology:constraint_victim(sotu_1993_clinton_deficit_reduction_program, federal_employees).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FEDERAL EMPLOYEES & PROGRAM BENEFICIARIES (SNARE) — Face mandatory reduction in employment and service provision with no exit option. The structural imposition is asymmetric: they bear costs imposed by fiscal policy while benefits accrue elsewhere. Maximum extraction from this agent group — no arbitrage, no mobility, no organizational counterpower.
constraint_indexing:constraint_classification(sotu_1993_clinton_deficit_reduction_program, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HIGH-INCOME TAX EARNERS (SNARE) — Face targeted tax increases with constrained exit options. Relocation costs exist but are surmountable (leaving jurisdiction, income shifting). High suppression: the tax code provides no escape hatch except costly migration. Extraction is severe but not total — some mobility exists at significant cost. Classified as snare rather than tangled rope because minimal coordination benefit flows to this agent group; they are targets, not coordinators.
constraint_indexing:constraint_classification(sotu_1993_clinton_deficit_reduction_program, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DEFICIT REDUCTION COALITION (TANGLED ROPE) — Federal Reserve, institutional investors, bond markets, and future-focused fiscal reformers. Experience both coordination and extraction. Coordinating mechanism: shared interest in lower interest rates and improved macroeconomic stability. Extraction component: the coalition imposes costs on tax-paying and service-consuming publics to achieve coalition members' macroeconomic goals. Constrained exit: once committed to deficit reduction program, coalition members cannot credibly abandon without triggering market panic. Active enforcement required: sustaining political will for unpopular tax increases and spending cuts.
constraint_indexing:constraint_classification(sotu_1993_clinton_deficit_reduction_program, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LONG-TERM SAVERS & PRIVATE INVESTORS (ROPE) — Primary beneficiaries. Experience the constraint as pure coordination: lower government borrowing frees up credit markets for private investment, reduces interest rates on their debt, and improves return on investment. Exit option: arbitrage. These agents can reallocate capital globally; improved domestic interest rates make staying in U.S. markets attractive without coercion. Benefits flow transparently to this group. No extraction perceived because benefits exceed costs.
constraint_indexing:constraint_classification(sotu_1993_clinton_deficit_reduction_program, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REFORM-ORIENTED CONGRESSIONAL COALITION (SCAFFOLD) — Organized political actors committed to fiscal reform. See the deficit reduction program as temporary scaffolding: necessary discipline to break post-Reagan fiscal patterns, sunset once structural reform is achieved. Low effective extraction because coalition has agency and perceives an exit strategy: once deficit is reduced sufficiently and fiscal orthodoxy is re-established, the political coalition can dissolve. Theater ratio is modest (0.38) because the coalition emphasizes concrete fiscal targets (deficit reduction as percentage of GDP) rather than purely performative indicators.
constraint_indexing:constraint_classification(sotu_1993_clinton_deficit_reduction_program, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LARGE CORPORATIONS & MULTINATIONALS (TANGLED ROPE) — Mixed position. Coordination benefit: lower interest rates improve access to capital markets for corporate expansion. Extraction component: tax increases (corporate and individual capital gains) reduce retained earnings and investor returns. Exit option: mobile. Corporations can relocate headquarters, shift production, or restructure ownership to minimize tax exposure. The mobile exit option gives this agent group agency that powerless groups lack, but the constraint still imposes costs. Classification is tangled rope rather than rope because the extraction component (higher tax burden on capital gains and corporate profits) is visible and asymmetric, not transparent coordination.
constraint_indexing:constraint_classification(sotu_1993_clinton_deficit_reduction_program, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, fiscal constraints are immutable laws of macroeconomics: budget deficits cannot persist indefinitely, and adjustment requires some combination of revenue increase or spending reduction. This perspective sees the constraint as inherent to fiscal mathematics itself — an unavoidable natural law. However, the structural data reveals this as a false summit: identifiable beneficiaries (savers, investors) and victims (high-income earners, federal employees) exist, indicating that the 'natural law' frame naturalizes what is actually a contested political choice about how deficit reduction should be distributed.
constraint_indexing:constraint_classification(sotu_1993_clinton_deficit_reduction_program, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1993_clinton_deficit_reduction_program_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1993_clinton_deficit_reduction_program, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1993_clinton_deficit_reduction_program, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1993_clinton_deficit_reduction_program, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sotu_1993_clinton_deficit_reduction_program_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The program imposes measurable costs on identified groups (high-income tax increases averaging 4.3% effective rate increase; federal workforce reduction of 272,900 positions over five years). These costs are real and concentrated. However, extractiveness does not reach snare-level (0.66+) because the tax increases are partially matched by legitimate deficit reduction (not pure rent extraction), and some private sector benefits (lower interest rates) represent genuine coordination value rather than transfer. Suppression (0.52): Moderate. High-income earners face significant barriers to exit (relocation costs, capital gains lock-in, social costs of emigration), but exit is possible — some mobility exists. Federal employees face near-total suppression (trapped option), but the overall suppression metric reflects that a mixed agent pool experiences the constraint — some with high suppression (employees), some with moderate suppression (high earners). Theater ratio (0.38): Moderate-low. The deficit reduction program emphasizes concrete fiscal metrics (deficit as % of GDP, spending caps, revenue targets) rather than purely performative indicators. However, the political rhetoric (long-term growth, competitive position, savings) exceeds the economic mechanism's explanatory power — the interest rate benefits depend on sustained deficit reduction, which is not guaranteed. Theater increases slightly over the interval (0.32 → 0.42) as political backlash mounts and rhetorical emphasis shifts from concrete metrics to narrative about necessary sacrifice.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a maximum perspectival gap across the classification space. Federal employees see a snare (extraction without reciprocal benefit); deficit reduction coalition sees a tangled rope (coordination with necessary enforcement); long-term savers see rope (pure coordination); analytical observer risks seeing a mountain (natural law) but structural data reveals false summit (contingent political choice). The gap reflects genuine disagreement about whether deficit reduction is inevitable macroeconomic necessity or a specific distributional choice that could be resolved differently (e.g., through spending-only adjustment, or deficit-accommodating monetary policy, or alternative growth models). The constraint's perspectival disagreement is not resolvable through additional data alone — it depends on which agent's structural position is treated as the baseline. If we start from the federal employee's powerlessness, deficit reduction appears coercive. If we start from the saver's structural position, it appears coordinating. The indexical system makes this disagreement explicit rather than hiding it in pseudo-objective language about 'what the program really is.'
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position within the constraint. High-income earners and federal employees are targets of the fiscal adjustment — they are victims in the base_properties beneficiaries/victims declaration. This yields high d values (0.85-0.95) → high f(d) → high effective extraction experienced by these groups. The deficit reduction coalition and long-term savers are beneficiaries — they experience benefits from lower interest rates. This yields lower d values (0.10-0.25) → low/negative f(d) → low or negative effective extraction. The reform-oriented congressional coalition occupies an intermediate position: they are organizers and enforcers of the constraint, experiencing constrained exit (once committed, cannot abandon without political crisis), but also experiencing benefits from fiscal orthodoxy restoration. The analytical observer at civilizational scope occupies a position where d ≈ 0.72 (canonical analytical fallback), yielding moderate f(d) — the observer sees both structure and the risks of naturalizing it. The directionality derivation confirms that this constraint has strong asymmetry: some agents experience negative χ (savers benefit more than they pay), while others experience high χ (workers and high earners pay far more than they benefit).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY STATUS: UNRESOLVED. The constraint classifies as tangled rope at the baseline (institutional/constrained/national perspective on deficit reduction coalition), satisfying the tangled rope gates (beneficiaries ✓, victims ✓, requires_active_enforcement ✓). However, the full perspectival profile reveals unresolved mandatrophy tension: (1) Can we distinguish genuine coordination (lower interest rates) from cover story for distributional extraction? The empirical omegas (incidence_of_tax_burden, crowding_out_vs_capital_freed) are designed to resolve this. (2) Can we distinguish scaffold (temporary reform) from snare (permanent extraction disguised as reform)? The political_sustainability_threshold omega addresses this. (3) Is the analytical observer's mountain classification a genuine immutable law or a false summit? The false_summit_natural_law omega addresses this. No single perspective is definitively 'the' answer. The mandatrophy resolution requires: (a) empirical resolution of tax incidence and capital market effects, (b) political trajectory (does the coalition sustain the program or abandon it?), and (c) a normative-plus-empirical judgment about whether deficit reduction was necessary or contingent. The framework documents these uncertainties rather than collapsing them into a single type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incidence_of_tax_burden,
    'Does the 1993 tax increase actually impose burdens on the intended target (high-income earners and corporations) or do economic feedback effects shift the burden to workers through wage suppression or employment reduction?',
    'Longitudinal wage data analysis comparing pre/post-1993 wage trends for high-income vs median workers; employment growth tracking; econometric analysis of tax incidence via general equilibrium effects',
    'If burden falls on targets: classification confirmed (snare/tangled rope from respective perspectives). If burden shifts to workers: reclassify high-income earner perspective to rope (apparent targets are actually shielded) and working-class perspective to snare (hidden victims). Extraction distribution becomes more diffuse and harder to detect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incidence_of_tax_burden, empirical, 'Economic incidence of tax increases vs statutory incidence').

omega_variable(
    crowding_out_vs_capital_freed,
    'Does deficit reduction actually free up credit for private sector investment (the rope coordination story) or does economic slowdown from fiscal consolidation reduce private investment demand (crowding in of savings without crowding out of investment)?',
    'Analysis of private sector investment and borrowing rates 1993-2000 compared to counterfactual trajectory; decomposition of interest rate changes into deficit-driven vs Fed policy components; credit market utilization data',
    'If capital is freed: private investors experience genuine coordination benefit (rope confirmed). If investment demand declines or rates fall for other reasons: private investor perspective reclassifies to piton (beneficiaries exist but the mechanism is not the one claimed) or mountain (interest rates are driven by global factors, deficit reduction is performative).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(crowding_out_vs_capital_freed, empirical, 'Whether deficit reduction actually frees investment capital or reduces investment demand').

omega_variable(
    political_sustainability_threshold,
    'What level of deficit reduction triggers political collapse of the reform coalition? Can the constraint sustain 1-3 years of concurrent tax increases and spending cuts, or does political backlash force reversal within 6-12 months?',
    'Analysis of legislative voting patterns, public opinion polling, electoral outcomes in midterm elections, media framing shifts over the 1993-1996 interval',
    'If sustained 3+ years: scaffold classification confirmed (genuine sunset logic as deficit reduction is achieved and coalition dissolves). If collapsed <12 months: reclassify to piton (political theater masquerading as reform) or tangled rope (extraction sustained by force rather than voluntary coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_sustainability_threshold, empirical, 'Political sustainability of concurrent tax and spending package').

omega_variable(
    false_summit_natural_law,
    'Is deficit reduction an immutable constraint of macroeconomic law or a contingent political choice about distribution of adjustment burdens?',
    'Historical comparison: countries with sustained high deficits without adjustment (Japan, some emerging markets); theoretical analysis of whether fiscal adjustment follows automatic necessity or discretionary policy choice; identification of who chooses the fiscal path and who bears the costs',
    'If natural law: mountain classification confirmed (analytical observer correctly identifies immutable constraint). If political choice: mountain reclassified as false summit (the ''law'' naturalizes a specific distributional outcome chosen by reform coalition). Trigger FSM signature if beneficiaries are identified — which they are (savers, investors, deficit-reduction coalition). This constraint is a false summit candidate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law, conceptual, 'Whether deficit reduction is immutable natural law or contingent political choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1993_clinton_deficit_reduction_program, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu93_tr_t0, sotu_1993_clinton_deficit_reduction_program, theater_ratio, 0, 0.32).
narrative_ontology:measurement(sotu93_tr_t2, sotu_1993_clinton_deficit_reduction_program, theater_ratio, 2, 0.38).
narrative_ontology:measurement(sotu93_tr_t5, sotu_1993_clinton_deficit_reduction_program, theater_ratio, 5, 0.42).

% Extraction over time
narrative_ontology:measurement(sotu93_be_t0, sotu_1993_clinton_deficit_reduction_program, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sotu93_be_t2, sotu_1993_clinton_deficit_reduction_program, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(sotu93_be_t5, sotu_1993_clinton_deficit_reduction_program, base_extractiveness, 5, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1993_clinton_deficit_reduction_program, resource_allocation).
narrative_ontology:affects_constraint(sotu_1993_clinton_deficit_reduction_program, federal_budget_sequestration_mechanism).
narrative_ontology:affects_constraint(sotu_1993_clinton_deficit_reduction_program, savings_and_loan_crisis_regulatory_response).
narrative_ontology:affects_constraint(sotu_1993_clinton_deficit_reduction_program, clinton_gores_reinventing_government_program).

% DUAL FORMULATION NOTE:
% The deficit reduction program decomposes into three structurally distinct constraints linked by causal sequence: (1) the macro-fiscal constraint (deficits cannot persist indefinitely) operates at civilizational scope and feeds into (2) the distributional constraint (who bears adjustment costs) which operates at national/biographical scope and activates (3) the enforcement constraint (sustaining political will for unpopular measures) which operates at immediate/biographical scope. This story focuses on constraint 2 (distributional/enforcement), which is downstream of the macro-fiscal necessity (constraint 1, mountain-adjacent) and upstream of specific enforcement mechanisms (constraint 3, entanglement with reinvention program). See dual_formulation_notes in sibling constraint files for separation of concerns.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1993_clinton_deficit_reduction_program, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
