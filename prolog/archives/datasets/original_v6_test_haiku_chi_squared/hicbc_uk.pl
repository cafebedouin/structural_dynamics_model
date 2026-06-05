% ============================================================================
% CONSTRAINT STORY: hicbc_uk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hicbc_uk, []).

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
 *   constraint_id: hicbc_uk
 *   human_readable: UK High Income Child Benefit Charge (HICBC)
 *   domain: economic/taxation/family_support
 *
 * SUMMARY:
 *   The UK High Income Child Benefit Charge (HICBC) is a means-tested
 *   withdrawal mechanism introduced in 2012-2013 as part of austerity fiscal
 *   consolidation. It claws back Child Benefit (£21.15 weekly per child in
 *   2024) from families where any household member earns over £60,000
 *   annually, with complete withdrawal at £62,000+ income. The constraint
 *   exhibits a classic Tangled Rope structure: it performs a coordination
 *   function (targeting child support toward lower-income households) while
 *   simultaneously creating a severe poverty trap (100% effective marginal
 *   tax rate in the £60k-£62k zone) that extracts economic value from
 *   middle-income dual-earner families through behavioral distortion and
 *   increased costs (tax planning, childcare restructuring, reduced labor
 *   supply). The expanding theater_ratio (0.42 → 0.58) reflects that the
 *   policy's stated purpose (deficit reduction, targeting support)
 *   increasingly diverges from its structural effect (poverty trap creation,
 *   regressive redistribution due to income-splitting avoidance). The
 *   mechanism is neither pure coordination (it generates genuine extraction
 *   and deadweight loss) nor pure extraction (it does allocate resources to
 *   lower-income households). Its classification as Tangled Rope is stable,
 *   but the growing mismatch between policy narrative and actual
 *   distributional effect suggests mandate erosion — the policy's legitimacy
 *   is weakening as the extraction mechanism becomes more transparent.
 *
 * KEY AGENTS:
 *   - Dual-earner families (£55k-£70k household income): Primary victims (powerless/trapped) — face 100% marginal tax rate and behavioral deadlock; cannot reduce income without childcare crisis
 *   - Single high earners with children (£60k+ individual income): Secondary victims (moderate/constrained) — face clawback; can exit via marriage but exit carries friction
 *   - Exchequer/HM Treasury: Primary beneficiary (institutional/arbitrage) — captures £2bn-£2.3bn annually; experiences HICBC as fiscal consolidation tool with low political sustainability
 *   - Tax planning professionals (accountants, benefits advisors): Secondary beneficiary (organized/constrained) — coordinate income-splitting strategies, salary sacrifice schemes; extract £400-£1200 per family annually from fear of clawback
 *   - Wealthy households with non-wage income: Tertiary beneficiary (powerful/arbitrage) — can structure income via capital gains, dividends, trusts; experience HICBC as coordination problem solved by tax planning
 *   - Child Benefit program legacy: Structural victim (institutional/constrained) — original universal program corrupted; administrative complexity increases; program loses political support as means-testing becomes visible
 *   - Analytical observer: Welfare state design perspective (analytical/analytical) — sees constraint as emblematic tension between universalism and fiscal sustainability; risks naturalizing the poverty trap as inherent necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hicbc_uk, 0.52).
domain_priors:suppression_score(hicbc_uk, 0.68).
domain_priors:theater_ratio(hicbc_uk, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hicbc_uk, extractiveness, 0.52).
narrative_ontology:constraint_metric(hicbc_uk, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(hicbc_uk, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hicbc_uk, tangled_rope).
narrative_ontology:human_readable(hicbc_uk, "UK High Income Child Benefit Charge (HICBC)").
narrative_ontology:topic_domain(hicbc_uk, "economic/taxation/family_support").

domain_priors:requires_active_enforcement(hicbc_uk).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hicbc_uk, exchequer_fiscal_consolidation).
narrative_ontology:constraint_beneficiary(hicbc_uk, higher_income_households_avoiding_charge).
narrative_ontology:constraint_victim(hicbc_uk, middle_income_dual_earners).
narrative_ontology:constraint_victim(hicbc_uk, single_high_earners_with_dependents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DUAL-EARNER FAMILY IN CLAWBACK ZONE (SNARE) — Family with two earners totaling £60k-£70k faces 100% benefit withdrawal on marginal earnings. Cannot exit via income reduction (childcare costs prohibit). Cannot exit via migration without significant cost. Suppression is structural: tax design creates poverty trap. d≈0.92, f(d)≈1.39, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(hicbc_uk, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SINGLE HIGH EARNER WITH CHILDREN (SNARE) — Individual earning over £60k on single income faces increasing clawback. Cannot exit via income reduction without household hardship. Can exit via marriage/cohabitation (pooling income may reduce benefit loss if partner earns less), but exit carries social/legal friction. d≈0.85, f(d)≈1.15, σ=1.0 → χ≈0.60.
constraint_indexing:constraint_classification(hicbc_uk, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TAX AVOIDANCE ADVISORY COALITION (TANGLED ROPE) — Accountants and benefits advisors have organized a coordination function (legitimate tax planning, income splitting strategies within marriage, salary sacrifice schemes) that also extracts value: families pay £400-£1200 annually for planning that shifts income below threshold. Suppression is moderate: families feel trapped and must buy coordination. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(hicbc_uk, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE TREASURY/HM REVENUE (ROPE) — Experiences HICBC as a coordination mechanism for deficit reduction: high earners are a reliable revenue source; means-testing Child Benefit targets support to lower incomes. Treasury sees net benefit (ε negative for them, χ negative). d≈0.02, f(d)≈-0.17, σ=1.0 → χ≈-0.09. Institutional perspective with full exit via policy change — but politically costly.
constraint_indexing:constraint_classification(hicbc_uk, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: INHERITED WEALTH / NON-WAGE INCOME HOLDERS (TANGLED ROPE) — High-net-worth families can avoid HICBC clawback via income structuring: capital gains taxed differently, dividends managed, trusts for children. They experience the constraint as a coordination mechanism (efficient markets reward tax planning) AND as extraction (they must hire specialists to remain exempt). d≈0.25, f(d)≈0.10, σ=1.0 → χ≈0.05.
constraint_indexing:constraint_classification(hicbc_uk, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: POLICY LEGACY / INSTITUTIONAL INERTIA (PITON) — HICBC was introduced in 2012-2013 as an austerity fiscal consolidation measure. Its core function (means-testing child support, deficit reduction) has been superseded by broader tax policy evolution, yet the constraint persists through legislative inertia and administrative friction. Theater_ratio=0.58 reflects: the policy claims to target high earners but actual effect is poverty-trap creation in middle-income zone (mismatch between stated and structural function). Removal is politically costly despite low functional necessity.
constraint_indexing:constraint_classification(hicbc_uk, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / WELFARE STATE DESIGN (TANGLED ROPE) — From civilizational perspective, HICBC reflects a hybrid extraction/coordination mechanism fundamental to how welfare states distribute limited resources. Pure universalism (everyone gets child benefit regardless of income) is fiscally unsustainable; pure means-testing (gradual withdrawal) creates poverty traps. HICBC's binary cliff structure is an attempt at coordination (protecting lower/middle income) that failed and became extractive. d≈0.60, f(d)≈0.80, σ=1.0 → χ≈0.42.
constraint_indexing:constraint_classification(hicbc_uk, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hicbc_uk_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hicbc_uk, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hicbc_uk, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hicbc_uk, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hicbc_uk, TR),
    TR >= 0.70.

:- end_tests(hicbc_uk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): The HICBC creates genuine extraction through poverty trap effect. Families in the £60k-£62k threshold zone face 100% marginal tax rate (all additional earnings lost to clawback). This is more severe than standard taxation. However, extraction is not total (ε<0.66) because: (1) the clawback only applies above £60k (lower incomes unaffected), (2) behavioral avoidance (marriage, income splitting, salary sacrifice) neutralizes 30-40% of intended effect, and (3) the program still allocates resources to lower-income households (it performs its stated coordination function). Suppression (0.68): High. Exit options are severely limited for trapped families. Cannot exit via income reduction (childcare costs prohibit). Cannot exit via geographic relocation (UK-wide policy, no sub-national variation). Can exit via marriage (low friction for cohabiting families but high friction for separated parents), or via tax planning (requires specialist advice, £400-£1200 annual cost). Suppression reflects the structural lock-in of dual-career household economics. Theater ratio (0.58): Moderate-high and increasing. The policy's narrative (targeting support to lower earners, fiscal consolidation) increasingly diverges from actual effect (regressive redistribution via income-splitting avoidance, poverty trap creation, deadweight loss from reduced labor supply in transition zone). The performative element (policy appears to achieve stated goal) masks the structural dysfunction (it doesn't, for middle-income households). Theater has grown since 2013 because: (a) analysis of distributional effect has become more transparent, (b) income-splitting avoidance strategies are now mainstream, (c) the austerity rationale for fiscal consolidation has weakened, yet the policy persists.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces radically different classifications across structural positions. Dual-earner families (powerless/trapped) perceive a pure Snare (d≈0.92, χ≈0.72): the mechanism extracts value with no exit option. Single high earners (moderate/constrained) perceive Snare with potential exit (d≈0.85, χ≈0.60): clawback is real but marriage is a viable (if socially costly) exit. Tax planning professionals (organized/constrained) perceive Tangled Rope (d≈0.55, χ≈0.39): the system both requires coordination services (benefiting advisors) and constrains families (who must buy advice to navigate clawback). The Treasury (institutional/arbitrage) perceives Rope (d≈0.02, χ≈-0.09): the mechanism is a clean coordination device for targeting resources downward and capturing high earner revenue. Wealthy households (powerful/arbitrage) perceive Tangled Rope (d≈0.25, χ≈0.05): the system extracts compliance costs but their non-wage income routes make extraction nearly zero. The policy legacy (institutional/constrained) perceives Piton (theater=0.58): the mechanism is performative, its stated function has been superseded, yet it persists through administrative inertia and political lock-in. This perspectival range (Snare → Rope → Tangled Rope → Piton) demonstrates how the same £2bn fiscal mechanism is experienced as a pure trap by some, as neutral coordination by others, and as degraded theater by institutional observers.
 *
 * DIRECTIONALITY LOGIC:
 *   Dual-earner families: Victims + trapped → d≈0.92, f(d)≈1.39. Maximal extraction because no exit option. Single high earners: Victims + constrained → d≈0.85, f(d)≈1.15. High extraction; exit (marriage) exists but carries significant friction. Tax planning professionals: Beneficiaries + constrained (they need families to stay in the trap to sell services) → d≈0.55, f(d)≈0.75. They benefit from the constraint's existence and the families' response. Treasury: Beneficiary + arbitrage (can change policy at will) → d≈0.02, f(d)≈-0.17. Net beneficiary with full exit option. Wealthy households: Mixed (victim to policy structure, beneficiary to income-splitting coordination) + arbitrage → d≈0.25, f(d)≈0.10. Low effective extraction because they have superior exit routes and income flexibility. Child Benefit program: Structural victim of means-testing corruption → d not applicable (institutional constraint object). Analytical observer: d≈0.60, f(d)≈0.80. The observer sees the constraint as a real welfare state tension, not a natural law, but risks naturalizing it as necessary.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC SIGNATURE: HICBC is a prototype Tangled Rope that is failing its coordination function and becoming a pure extraction mechanism. The mandatrophy is real and unresolved. At inception (2012-2013), the constraint had plausible dual function: (1) Coordination: target child support to lower incomes when fiscal space was constrained. (2) Extraction: capture revenue from high earners as part of austerity consolidation. The theater ratio has grown from 0.42 to 0.58 because the coordination function has degraded. Why? The means-testing design (cliff structure) was too crude: it creates a poverty trap that incentivizes behavioral avoidance (marriage dissolution, income splitting, increased tax planning costs) rather than accepted redistribution. The extraction function has intensified in reality (affecting more middle-income families as nominal earnings rise) even as the fiscal consolidation rationale has weakened (austerity era is over, but the policy persists). The constraint now classifies as Tangled Rope because: (a) it still performs redistribution (coordination function is present but degrading), (b) it creates significant extraction and deadweight loss (poverty trap, behavioral distortion), (c) it requires active enforcement (HMRC clawback calculations, family cooperation). But the Tangled Rope is unstable. As theater_ratio approaches 0.70, the constraint risks reclassifying as Piton (performative means-testing with inertial persistence but failed coordination). Mandatrophy is unresolved because: the Treasury maintains the policy as fiscal consolidation (coordination narrative), families experience it as extraction trap (victim narrative), and policy analysis increasingly reveals it as performative dysfunction (observer narrative). No consensus exists on whether the coordination function justifies the extraction. The constraint cannot be classified as pure Snare (it does allocate resources to lower incomes) or pure Rope (it creates genuine poverty trap). But it can no longer claim stable Tangled Rope status if the coordination function is deteriorating faster than extraction is justifying it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_cliff_vs_taper,
    'Is the HICBC''s cliff structure (100% withdrawal per £1 earned) a structural requirement of the constraint or a contingent design choice that could be replaced by gradual taper without losing the coordination function?',
    'Comparative analysis of equivalent means-tested benefits in OECD countries with tapered vs cliff withdrawal. Simulation of labor supply response to £50 taper vs £100 cliff at same threshold income.',
    'If cliff is contingent: constraint reclassifies as Scaffold (sunset via gradual reform). If cliff is necessary: constraint is locked Tangled Rope (coordination structure inherently generates extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threshold_cliff_vs_taper, empirical, 'Whether cliff withdrawal is necessary or contingent design choice').

omega_variable(
    income_splitting_loophole_extent,
    'How much of the HICBC extraction is neutralized by marriage, cohabitation, and income-splitting tax strategies available to organized/wealthy households?',
    'Tax year administrative data on HICBC clawback distribution by household income composition (dual earners vs single), by region, by use of salary sacrifice schemes. Comparison of effective benefit loss for married vs unmarried couples at same household income.',
    'If >40% of extractive effect is neutralized by behavioral avoidance: constraint reclassifies as Piton (performative means-testing). If <20%: constraint remains Tangled Rope with genuine asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(income_splitting_loophole_extent, empirical, 'Extent to which income-splitting neutralizes HICBC extraction').

omega_variable(
    fiscal_replacement_pathway,
    'If HICBC were abolished tomorrow, what revenue replacement mechanism would be necessary to maintain equivalent fiscal consolidation without creating equivalent poverty traps?',
    'OBR/Institute for Fiscal Studies modeling of alternative means-testing designs (gradual taper, modified household income threshold, exemptions for childcare costs). Policy analysis of actual government proposals for HICBC reform.',
    'If feasible replacement exists: HICBC is Scaffold (temporary austerity mechanism with sunset pathway). If no replacement: HICBC is locked Tangled Rope (structural necessity of hybrid extraction-coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_replacement_pathway, empirical, 'Feasibility of fiscal replacement for HICBC without equivalent poverty traps').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hicbc_uk, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hicbc_tr_t0, hicbc_uk, theater_ratio, 0, 0.42).
narrative_ontology:measurement(hicbc_tr_t6, hicbc_uk, theater_ratio, 6, 0.52).
narrative_ontology:measurement(hicbc_tr_t12, hicbc_uk, theater_ratio, 12, 0.58).

% Extraction over time
narrative_ontology:measurement(hicbc_be_t0, hicbc_uk, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hicbc_be_t6, hicbc_uk, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(hicbc_be_t12, hicbc_uk, base_extractiveness, 12, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hicbc_uk, resource_allocation).
narrative_ontology:affects_constraint(hicbc_uk, uk_child_poverty_dynamics).
narrative_ontology:affects_constraint(hicbc_uk, dual_career_household_economics).
narrative_ontology:affects_constraint(hicbc_uk, tax_planning_industry_growth).

% DUAL FORMULATION NOTE:
% HICBC can be decomposed into two distinct structural claims: (1) means-tested_redistribution (ε≈0.25, Rope/Scaffold): the legitimate fiscal consolidation mechanism allocating child support by household income. (2) poverty_trap_mechanism (ε≈0.68, Snare): the behavioral distortion created by cliff withdrawal design in £60k-£62k zone. These are not the same constraint viewed from two angles — they have different ε values, different victim populations, and different exit options. The current JSON story combines them as a single ε=0.52 Tangled Rope. Future decomposition should separate: constraint_hicbc_redistribution_function (ε≈0.25, Scaffold) as the stated purpose, and constraint_hicbc_threshold_poverty_trap (ε≈0.68, Snare) as the structural dysfunction. Both stories would link via affects_constraints, with the poverty trap downstream of the redistribution design choice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hicbc_uk, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
