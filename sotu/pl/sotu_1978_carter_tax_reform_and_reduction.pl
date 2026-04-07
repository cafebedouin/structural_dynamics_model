% ============================================================================
% CONSTRAINT STORY: sotu_1978_carter_tax_reform_and_reduction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1978_carter_tax_reform_and_reduction, []).

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
 *   constraint_id: sotu_1978_carter_tax_reform_and_reduction
 *   human_readable: Tax System Reform with $25B Reduction (Carter 1978)
 *   domain: economics/fiscal_policy
 *
 * SUMMARY:
 *   The 1978 Carter tax reform proposal operates as a hybrid constraint
 *   combining both coordination and extraction mechanisms. The stated purpose
 *   — fairness and simplification — provides a coordination narrative
 *   (reducing friction in the tax system, targeting relief to those
 *   'bypassed' economically). Simultaneously, the $25B reduction constitutes
 *   a structural extraction from the federal revenue base, creating a
 *   long-term capacity constraint that future administrations must manage.
 *   The constraint exhibits all six DR types depending on perspective: pure
 *   extraction (snare) from the federal revenue base perspective, mixed
 *   coordination-extraction (tangled rope) for middle-income beneficiaries,
 *   pure coordination (rope) for businesses arbitraging growth opportunities,
 *   a temporary transition (scaffold) for reform advocates seeing
 *   restructuring as a one-time event, degraded institutional theater (piton)
 *   from tax code complexity interests, and apparent natural law
 *   (false-summit mountain) from civilizational analytical view. The
 *   extractiveness trajectory shows increasing strain over the interval as
 *   the revenue gap becomes chronic rather than temporary, and theater ratio
 *   increases as simplification promises encounter resistance from
 *   special-interest preservation of complexity.
 *
 * KEY AGENTS:
 *   - Individual Taxpayers (Beneficiary, moderate/constrained): Primary recipients of $17B income tax cut; constrained by employment dependency; experience genuine benefit coupled with suppression of alternative revenue strategies
 *   - Private Sector Businesses (Beneficiary, institutional/arbitrage): Benefit from both reduced tax burden and expanded consumer purchasing power; have arbitrage capacity to adapt to any residual extraction
 *   - Federal Revenue Base (Victim, powerless/trapped): Absorbs $25B permanent reduction; no exit option; cannot adapt to revenue loss without spending cuts that redistribute burden
 *   - High-Income Earners (Victim, powerful/arbitrage): Target of fairness reforms; have arbitrage capacity but politically constrained; may escape extraction if reforms are diluted
 *   - Progressive Tax Reform Coalition (Organized/constrained): Advocates for fairness component; see reform as restructuring event with sunset logic; moderate suppression due to policy visibility
 *   - Tax Code Complexity Interests (Institutional/arbitrage): Benefit from system opacity; resist simplification; maintain extraction channels through specialized knowledge and lobbying
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1978_carter_tax_reform_and_reduction, 0.52).
domain_priors:suppression_score(sotu_1978_carter_tax_reform_and_reduction, 0.35).
domain_priors:theater_ratio(sotu_1978_carter_tax_reform_and_reduction, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1978_carter_tax_reform_and_reduction, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1978_carter_tax_reform_and_reduction, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(sotu_1978_carter_tax_reform_and_reduction, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1978_carter_tax_reform_and_reduction, tangled_rope).
narrative_ontology:human_readable(sotu_1978_carter_tax_reform_and_reduction, "Tax System Reform with $25B Reduction (Carter 1978)").
narrative_ontology:topic_domain(sotu_1978_carter_tax_reform_and_reduction, "economics/fiscal_policy").

domain_priors:requires_active_enforcement(sotu_1978_carter_tax_reform_and_reduction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1978_carter_tax_reform_and_reduction, individual_taxpayers).
narrative_ontology:constraint_beneficiary(sotu_1978_carter_tax_reform_and_reduction, private_sector_businesses).
narrative_ontology:constraint_beneficiary(sotu_1978_carter_tax_reform_and_reduction, middle_income_earners).
narrative_ontology:constraint_victim(sotu_1978_carter_tax_reform_and_reduction, federal_revenue_base).
narrative_ontology:constraint_victim(sotu_1978_carter_tax_reform_and_reduction, high_income_earners).
narrative_ontology:constraint_victim(sotu_1978_carter_tax_reform_and_reduction, wealth_concentration_control).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FEDERAL REVENUE BASE (SNARE) — Trapped in structural revenue decline; $25B reduction is extracted from public budget capacity. No exit option: must absorb the reduction or trigger spending cuts. Suppression mechanism: political commitment to tax reduction prevents alternative revenue strategies. Maximum extraction asymmetry — revenue base bears cost with no compensation mechanism.
constraint_indexing:constraint_classification(sotu_1978_carter_tax_reform_and_reduction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE-INCOME WAGE EARNERS (TANGLED ROPE) — Constrained by reliance on employment income; cannot arbitrage to alternative revenue sources. Mixed experience: genuine benefit from $17B income tax cut (coordination function) but constrained by suppression of alternative safety-net funding. Extractiveness emerges from the asymmetry: benefits target this group, but fairness reforms that might redistribute to them are traded away for broader cuts.
constraint_indexing:constraint_classification(sotu_1978_carter_tax_reform_and_reduction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PRIVATE SECTOR BUSINESSES (ROPE) — High exit optionality (capital mobility, tax deferral strategies); benefits from reduced tax burden and increased consumer purchasing power. Experiences constraint as pure coordination: purchasing power expansion and simplification reduce friction. Arbitrage options give institutional actors capacity to adapt to any residual extraction.
constraint_indexing:constraint_classification(sotu_1978_carter_tax_reform_and_reduction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PROGRESSIVE TAX REFORM COALITION (SCAFFOLD) — Organized agents (tax reform advocates, fairness-focused policy groups) see the constraint as a temporary coordination failure with sunset logic: comprehensive tax reform is a one-time restructuring that establishes new baselines. Suppression is moderate because the coalition has media visibility and can mobilize around fairness narratives. Sunset lies in the assumption that reformed tax code becomes stable — revenue decline is treated as acceptable one-time cost of structural improvement.
constraint_indexing:constraint_classification(sotu_1978_carter_tax_reform_and_reduction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: TAX CODE COMPLEXITY PRESERVATION (PITON) — Special-interest coalitions benefit from tax code complexity (tax professionals, accounting firms, loophole maintainers) experience the simplification component as degradation of their functional territory. Theater ratio high: claims of 'fairness reform' mask that complexity preserves narrow extraction channels. Piton derives from institutional inertia: after reform, the code will drift back toward complexity as new loopholes are layered in. The simplification is performative — theater of rationalization masking the persistence of special-interest extraction.
constraint_indexing:constraint_classification(sotu_1978_carter_tax_reform_and_reduction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / MACROECONOMIC CONSTRAINT (MOUNTAIN) — From a civilizational perspective, the constraint appears to embody an immutable trade-off: government revenue and growth cannot both expand simultaneously without external sources (productivity increases, population growth, inflation). Tax reduction + fairness reform looks like defying this law — but the engine's false summit detection will flag this as naturalization of a contingent fiscal arrangement, not a law of economics.
constraint_indexing:constraint_classification(sotu_1978_carter_tax_reform_and_reduction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1978_carter_tax_reform_and_reduction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1978_carter_tax_reform_and_reduction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1978_carter_tax_reform_and_reduction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1978_carter_tax_reform_and_reduction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1978_carter_tax_reform_and_reduction, TR),
    TR >= 0.70.

:- end_tests(sotu_1978_carter_tax_reform_and_reduction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, trending upward. Initial phase (t=0-2) is moderate (0.28-0.38) reflecting genuine coordination benefit to beneficiaries — purchasing power expansion, administrative simplification, targeted relief. Middle phase (t=2-4) shows extractiveness rising to 0.52 as the permanent nature of the revenue gap becomes clear and fairness reforms face dilution. Later phase (t=4-8) shows continued rise to 0.61 as federal capacity constraints trigger spending cuts and long-term incidence patterns emerge. The upward trajectory reflects ratchet effects: tax reduction is permanent, but fairness component proves temporary. Suppression (0.35): Moderate. Barriers exist but are not absolute: federal budget cuts are politically constrained (protected programs, political constituencies), middle-income earners can apply pressure through electoral mechanisms, and reform advocates maintain organizational visibility. However, suppression is real in the sense that alternative revenue strategies are suppressed by political commitment to reduction. Theater ratio (0.58): Moderate-high, trending upward from 0.42 to 0.67. Initial period has lower theater as genuine simplification work occurs and relief is tangible. As time progresses, theater increases as the simplification promise encounters special-interest resistance and new complexity layers accumulate. The gap between reform rhetoric and implementation fidelity drives the upward trajectory.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival fragmentation across observer positions. The private sector sees pure coordination (Rope) because arbitrage options let them benefit without constraint. Middle-income earners see mixed experience (Tangled Rope) because benefits are real but coupled with suppressed alternative outcomes. The federal revenue base sees pure extraction (Snare) because it absorbs the loss with no exit. The reform coalition sees a temporary transition (Scaffold) because they frame the $25B as a one-time restructuring cost with sunset logic. Tax code complexity interests see their functional degradation (Piton) as theatrical — simplification promises are performative and will be undermined by continued special-interest layering. The analytical observer at civilizational scale risks naturalizing the constraint as macroeconomic law (false-summit Mountain) — the belief that 'you can't cut taxes and maintain revenue simultaneously' — when in fact the constraint is contingent on specific institutional choices (spending commitments, political coalitions, behavioral responses to tax changes).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies sharply across agents based on their structural relationship to the extraction flow. Private businesses (beneficiary + arbitrage exit) derive d ≈ 0.10, producing negative effective extractiveness (f(d) ≈ -0.01) — they experience the constraint as subsidy. Middle-income wage earners (beneficiary + constrained exit) derive d ≈ 0.40, producing moderate effective extractiveness (f(d) ≈ 0.40) reflecting mixed experience: genuine tax benefit but suppressed by revenue consequences. High-income earners (victim + arbitrage) derive d ≈ 0.55, producing positive extractiveness (f(d) ≈ 0.75) reflecting fairness pressure, though arbitrage options give them ways to minimize burden. Federal revenue base (victim + trapped) derives d ≈ 0.95, producing maximum extractiveness (f(d) ≈ 1.42) — it absorbs the full reduction with no recourse. The scope is national (σ=1.0), so scope does not amplify or dampen the derived values. The tangled rope classification emerges from the presence of both coordination (fairness, simplification, targeted relief) and asymmetric extraction (permanent revenue loss concentrated on federal capacity), coupled with enforcement requirements (Congressional action to implement, IRS administration of new code).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by clarifying that 'fairness and simplification' can coexist with extraction only when the extraction is asymmetric and permanent. The apparent contradiction — 'this is fair and simple' (coordination narrative) vs. 'this extracts $25B from future revenue capacity' (extraction reality) — dissolves when viewed indexically. From the beneficiary perspective (especially institutional beneficiaries like private business), it is pure coordination: purchasing power, simplification, growth stimulus. From the victim perspective (federal revenue base), it is pure extraction: permanent capacity loss with no compensation. From the middle-income wage earner perspective, it is tangled rope: genuine benefit coupled with suppressed alternatives. The mandatrophy is resolved not by choosing one type, but by recognizing that the constraint IS both coordination and extraction simultaneously, and which aspect dominates depends entirely on the agent's structural relationship to the extraction flow. The fairness reforms (targeting high-income earners) represent the coordination component; the permanent $25B reduction represents the extraction component. The constraint is stable only if both components are enforced. If fairness reforms are diluted, the constraint becomes pure rent-seeking (Rope or Snare). If the revenue gap is addressed through later tax increases, the constraint becomes Scaffold (temporary). The indexical decomposition prevents false aggregation: this is not a single 'tax reform' but a bundle of structurally distinct mechanisms that operate at different power levels and timescales.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revenue_behavioral_response,
    'Will tax reduction stimulate economic growth sufficient to offset the $25B revenue loss, or will revenue gap persist?',
    'Post-reform GDP tracking; comparison of actual revenue decline vs. behavioral elasticity models; assessment of multiplier effects from increased private purchasing power',
    'If growth offsets loss: constraint reclassifies to Rope (pure coordination mechanism). If gap persists: constraint remains Tangled Rope or escalates to Snare (extraction without compensation mechanism).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revenue_behavioral_response, empirical, 'Whether tax reduction will be self-financing through growth').

omega_variable(
    fairness_reform_implementation_fidelity,
    'Will promised fairness reforms (taxing the wealthy, closing loopholes) actually materialize, or will political pressure convert the reform into pure tax reduction for all?',
    'Analysis of implemented vs. proposed tax code changes; measurement of effective tax rate changes by income quintile; tracking of loophole closure vs. preservation',
    'If implemented with fidelity: benefits are distributed, snare classification for high-income earners validated. If converted to pure cuts: constraint becomes pure rent-seeking (all classes benefit from reduced burden), high-income earners escape extraction, classification shifts toward Rope from top quintile perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fairness_reform_implementation_fidelity, empirical, 'Degree to which fairness reforms will be implemented vs. diluted').

omega_variable(
    federal_capacity_restoration_timeline,
    'Can federal revenue capacity be restored through later reforms, or is the $25B reduction permanently locked into baseline expectations?',
    'Historical tracking of whether future administrations can raise taxes without political penalty; analysis of ratchet effects in fiscal expectations',
    'If restorable: constraint is Scaffold with genuine sunset. If permanently locked: constraint is Snare on federal capacity (extraction persists through institutional path-dependence).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_capacity_restoration_timeline, preference, 'Whether revenue reduction becomes permanently anchored in political expectations').

omega_variable(
    distributional_burden_shift,
    'As federal capacity declines, which programs will be cut? Will burden fall on middle-income earners through service reduction, or on wealthy through progressive spending cuts?',
    'Tracking of federal spending cuts post-reform; measurement of which constituencies experience service loss; analysis of incidence of public good degradation across income distribution',
    'If cuts target safety nets: middle-income earners experience delayed extraction (tax cut benefit offset by service loss). If progressive: constraint is more symmetrical. Distributional pattern shifts credibility of fairness claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(distributional_burden_shift, empirical, 'Which constituencies bear incidence of resulting federal spending cuts').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1978_carter_tax_reform_and_reduction, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu_tr_t0, sotu_1978_carter_tax_reform_and_reduction, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sotu_tr_t2, sotu_1978_carter_tax_reform_and_reduction, theater_ratio, 2, 0.48).
narrative_ontology:measurement(sotu_tr_t4, sotu_1978_carter_tax_reform_and_reduction, theater_ratio, 4, 0.58).
narrative_ontology:measurement(sotu_tr_t8, sotu_1978_carter_tax_reform_and_reduction, theater_ratio, 8, 0.67).

% Extraction over time
narrative_ontology:measurement(sotu_be_t0, sotu_1978_carter_tax_reform_and_reduction, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(sotu_be_t2, sotu_1978_carter_tax_reform_and_reduction, base_extractiveness, 2, 0.38).
narrative_ontology:measurement(sotu_be_t4, sotu_1978_carter_tax_reform_and_reduction, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(sotu_be_t8, sotu_1978_carter_tax_reform_and_reduction, base_extractiveness, 8, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1978_carter_tax_reform_and_reduction, resource_allocation).
narrative_ontology:affects_constraint(sotu_1978_carter_tax_reform_and_reduction, federal_spending_baseline_lock).
narrative_ontology:affects_constraint(sotu_1978_carter_tax_reform_and_reduction, inflation_bracket_creep_amplification).
narrative_ontology:affects_constraint(sotu_1978_carter_tax_reform_and_reduction, progressive_tax_code_degradation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1978_carter_tax_reform_and_reduction, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
