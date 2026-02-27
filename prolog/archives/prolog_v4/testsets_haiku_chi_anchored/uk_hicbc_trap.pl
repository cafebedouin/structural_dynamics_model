% ============================================================================
% CONSTRAINT STORY: uk_hicbc_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_hicbc_trap, []).

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
 *   constraint_id: uk_hicbc_trap
 *   human_readable: UK High Income Child Benefit Charge (HICBC)
 *   domain: economic/political
 *
 * SUMMARY:
 *   The UK High Income Child Benefit Charge (HICBC), introduced in January
 *   2013, creates a structural trap for households where a single partner
 *   earns above £60,000 annually (rising to £80,000 in 2024, with proposed
 *   exemptions for dual-high-earners). The rule is nominally designed to
 *   target 'universal' child benefit — which all households historically
 *   received — to higher-income earners. However, the implementation creates
 *   a powerful work disincentive for secondary earners: child benefit claws
 *   back at £1 per £2 earned above the threshold, generating effective
 *   marginal tax rates exceeding 60% when combined with income tax and
 *   National Insurance. This produces a tension between the stated
 *   coordination goal (maintaining universal child benefit as a family
 *   support) and the extraction mechanism (means-testing reduces benefit
 *   value sharply at a specific income point). The constraint exemplifies how
 *   a policy designed as fiscal tightening can become extractive when it
 *   creates work disincentives that actually reduce overall tax revenue and
 *   increase child poverty. The rule persists partly through fiscal necessity
 *   (reducing spending) and partly through political ideology (means-testing
 *   preserves the concept of 'universality' while controlling costs), making
 *   it a hybrid of coordination logic and extraction mechanism — a Tangled
 *   Rope.
 *
 * KEY AGENTS:
 *   - Secondary Earner (typically mother): Primary victim (powerless/trapped) — faces effective marginal tax >60%, often chooses to withdraw from labor market entirely
 *   - Single-Earner Household: Primary victim (moderate/trapped) — loses benefit on £1-per-£2 basis; cannot split income with partner to stay below threshold
 *   - Treasury/HMRC: Primary beneficiary (institutional/arbitrage) — collects ~£1.5bn annually; enforcement cost is minimal (automatic tax calculation)
 *   - Dual-Income Households Below Threshold: Secondary beneficiary (moderate/mobile) — receive full child benefit coordination value; benefit from secondary earner participation incentives
 *   - Policy Reform Coalition: Organized advocates (organized/mobile) — IPPR, working parents groups, tax reform bodies articulate sunset path through UBI, tax reform, or benefit restructure
 *   - Child Poverty Outcomes: Structural victim (powerless/trapped) — benefit clawback often increases child poverty by reducing secondary earner participation without offsetting tax savings
 *   - Analytical Observer: Civilizational analyst (analytical/analytical) — risks naturalizing the 'inherent trade-off' between redistribution and work incentives as immutable economic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_hicbc_trap, 0.48).
domain_priors:suppression_score(uk_hicbc_trap, 0.62).
domain_priors:theater_ratio(uk_hicbc_trap, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_hicbc_trap, extractiveness, 0.48).
narrative_ontology:constraint_metric(uk_hicbc_trap, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(uk_hicbc_trap, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_hicbc_trap, tangled_rope).
narrative_ontology:human_readable(uk_hicbc_trap, "UK High Income Child Benefit Charge (HICBC)").
narrative_ontology:topic_domain(uk_hicbc_trap, "economic/political").

domain_priors:requires_active_enforcement(uk_hicbc_trap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_hicbc_trap, treasury_revenue_collection).
narrative_ontology:constraint_beneficiary(uk_hicbc_trap, dual_income_households_below_threshold).
narrative_ontology:constraint_victim(uk_hicbc_trap, single_earner_households_over_threshold).
narrative_ontology:constraint_victim(uk_hicbc_trap, work_incentive_structure).
narrative_ontology:constraint_victim(uk_hicbc_trap, secondary_earner_participation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SECONDARY EARNER (SNARE) — Faces effective marginal tax rates exceeding 60% as child benefit claws back at £1 per £2 earned above £60k threshold. Cannot exit without withdrawing from labor market entirely. No alternative benefit structure available. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.67.
constraint_indexing:constraint_classification(uk_hicbc_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DUAL-INCOME HOUSEHOLD (TANGLED ROPE) — Below threshold, receives full benefit coordination value (child support pool incentivizes dual earning). But if either partner crosses threshold, benefits from coordination collapse. Constrained by income distribution: cannot easily restructure to claim both benefit coordination and higher household income. d≈0.68, f(d)≈1.08, σ=1.0 → χ≈0.52.
constraint_indexing:constraint_classification(uk_hicbc_trap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TREASURY & HMRC (ROPE) — Experiences the rule as a coordination mechanism for revenue collection. Enforcement cost is minimal (HMRC processes via standard tax return; clawback is automatic calculation). Extraction is institutional benefit (tax revenue ~£1.5bn annually). d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.05. Net beneficiary.
constraint_indexing:constraint_classification(uk_hicbc_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: POLICY REFORM COALITION (SCAFFOLD) — Organized groups (IPPR, working parents' advocacy, tax reform bodies) see the HICBC as a temporary policy with a sunset path: universal basic income pilots, reformed child benefit structure, or reformed tax thresholds could dissolve the extraction mechanism. d≈0.35, f(d)≈0.32, σ=1.0 → χ≈0.15. Coalition has agency and articulates an exit path within policy cycle.
constraint_indexing:constraint_classification(uk_hicbc_trap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: MEANS-TESTING LOGIC / INSTITUTIONAL INERTIA (PITON) — The HICBC is a degraded version of means-tested welfare logic applied to a nominally universal benefit. Theater ratio = 0.58: much energy is spent on income verification, threshold administration, and public debate about 'fairness' rather than on actual poverty relief. The constraint persists through bureaucratic inertia and political ideology ('universal' benefits preserve legitimacy while means testing controls costs), but its primary function (reducing child poverty) has atrophied — the rule often makes work less rational for secondary earners, worsening rather than improving outcomes.
constraint_indexing:constraint_classification(uk_hicbc_trap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a pure economic perspective, a single-earner household earning above benefit threshold must choose: earn more and lose benefit, or earn less and keep benefit. This appears as an inherent trade-off — immutable law of finite resources and progressive taxation. However, structural data contradicts mountain classification: ε=0.48, suppression=0.62, theater=0.58 all signal institutional rather than natural constraint. The 'inherent trade-off' is a false summit; the actual constraint is a contingent policy design choice.
constraint_indexing:constraint_classification(uk_hicbc_trap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_hicbc_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_hicbc_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_hicbc_trap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uk_hicbc_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(uk_hicbc_trap, TR),
    TR >= 0.70.

:- end_tests(uk_hicbc_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The rule extracts from secondary earners through the combined effect of benefit clawback, opportunity cost of foregone earnings, and work disincentive. The extraction is not as severe as a pure snare (victims have some exit options: income restructuring, self-employment, partnership dissolution) but is significant enough to measurably reduce secondary earner labor force participation (empirical estimates suggest 1-2% withdrawal rate). The 0.48 value reflects that extraction is real and measurable but not total — it operates through work incentive suppression rather than direct coercion. Suppression (0.62): Moderate-high. The constraint operates through multiple suppression mechanisms: (1) Economic: high effective marginal tax rate makes work irrational for many secondary earners; (2) Informational: awareness of HICBC is low (~40% of affected households understand the mechanism); (3) Structural: threshold structure creates bunching effect at £60k income point, forcing binary choice (earn below threshold or forfeit benefit entirely). Theater ratio (0.58): Moderate. The HICBC involves substantial theater: administrative burden of income verification and clawback calculation, public debate about 'fairness' to high-income families, political theater around 'protecting' universal benefits while means-testing them. But the theater is not dominant — the underlying extraction mechanism is real and measurable (£1.5bn collected annually). Theater has increased over time as the rule's unintended consequences (reduced secondary earner participation, increased child poverty in some cases) have become visible, generating more policy debate and administrative complexity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how different agents perceive the same policy through radically different structural lenses. The Treasury sees efficient revenue collection (Rope: coordination mechanism, minimal enforcement cost). The secondary earner sees a trap (Snare: faces effective marginal tax >60%, no realistic exit without withdrawing from labor market). The policy reform coalition sees a temporary problem with a sunset (Scaffold: UBI pilots or tax reform could dissolve the mechanism). The institutional means-testing logic sees its own degraded function (Piton: theater ratio 0.58 reflects energy spent on threshold administration rather than poverty reduction). The analytical observer risks naturalizing the 'inherent trade-off' as economic law (Mountain: false summit), when the actual constraint is contingent policy design. The perspectival gap reveals that all six types coexist because the HICBC is not a simple law of nature — it is a policy choice with measurable structural consequences that different agents experience as coordination, extraction, or degradation depending on their position.
 *
 * DIRECTIONALITY LOGIC:
 *   Secondary earner: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction. Cannot realistically exit without withdrawing from labor market; faces effective marginal tax exceeding 60%. Treasury/HMRC: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Minimal enforcement cost; institutional benefit from revenue collection. Dual-income below threshold: Beneficiary + mobile → d≈0.18, f(d)≈0.05. Slight beneficiary. Receive full benefit coordination value; some exit mobility (can restructure income if needed, though not easily). Policy reform coalition: Organized + mobile → d≈0.35, f(d)≈0.32. Moderate. Have agency and articulate exit path; constrained by political economy of reform cycle. Child poverty outcomes: Victim + trapped → d≈0.90, f(d)≈1.38. Structural victim with no exit option; abstract collective cannot organize or exit. The directionality derivation captures why secondary earners and policy advocates perceive the constraint so differently: secondary earners have trapped exit (high d), while policy advocates have mobile exit (lower d) because they can articulate and potentially implement reform paths.
 *
 * MANDATROPHY ANALYSIS:
 *   The HICBC does NOT resolve the mandatrophy between coordination and extraction — it exemplifies the failure to resolve it. The stated goal (coordination: maintain universal child benefit as family support while targeting fiscal consolidation) is genuinely a coordination need. But the implementation mechanism (extraction: clawback via effective marginal tax >60%) creates work disincentives that undermine the coordination goal. The policy is Tangled Rope precisely because it possesses BOTH genuine coordination function (provides child support to dual-income households) AND asymmetric extraction (claws back support from higher earners in a way that distorts work incentives). The mandatrophy is NOT resolved; rather, the constraint manifests it. The policy has created a situation where fiscal tightening has become counterproductive: the £1.5bn collected annually in benefit clawback is offset (or more than offset) by reduced secondary earner tax revenue and potential increases in child poverty-related spending. True resolution would require either: (A) eliminating the clawback (returning to universality), (B) restructuring thresholds to remove work disincentive (e.g., per-household threshold for dual earners), or (C) replacing child benefit with a redesigned mechanism that does not create marginal rate distortion. The IPPR and other policy advocates argue for such reforms, explaining the Scaffold perspective: the sunset path is visible but requires political change, not just economic adjustment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    secondary_earner_withdrawal_elasticity,
    'What is the true labor supply elasticity of secondary earners responding to the HICBC clawback? Are secondary earners genuinely trapped, or do many perceive exit options (self-employment restructuring, partner income juggling, career switching)?',
    'Longitudinal labor force survey data tracking secondary earner participation before/after HICBC implementation (2013); econometric estimation of labor supply response to effective marginal tax rate; behavioral interviews with affected households',
    'If elasticity is high (>0.5): secondary earners have mobile exit options, and classification shifts toward Tangled Rope across perspectives. If elasticity is low (<0.2): trap is real, Snare classification strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secondary_earner_withdrawal_elasticity, empirical, 'Labor supply elasticity of secondary earners responding to HICBC').

omega_variable(
    benefit_clawback_awareness_gap,
    'How many affected households actually understand the HICBC mechanism and its effective marginal tax impact? Is the constraint behavioral (people don''t know they''re trapped) or structural (people know and are trapped anyway)?',
    'Survey data on HICBC awareness among high-income households; comparison of intended vs actual labor supply decisions; analysis of income bunching at £60k threshold (revealed preference for threshold avoidance)',
    'If awareness is low: much of the ''trap'' is theater rather than extraction — suppression via confusion rather than structural barrier. If awareness is high and people still withdraw: extraction is real and structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(benefit_clawback_awareness_gap, empirical, 'Household awareness and understanding of HICBC mechanism').

omega_variable(
    child_poverty_vs_work_incentive_causality,
    'Does the HICBC achieve its stated goal (reducing benefit spending on higher-income households) at the cost of worsening work incentives? What is the net welfare effect — does reduced secondary earner participation offset tax savings?',
    'Causal inference analysis: compare child outcomes, household income, and child poverty rates in HICBC vs non-HICBC treated groups; model counterfactual secondary earner participation if HICBC did not exist',
    'If net welfare effect is negative (more child poverty from work disincentive than savings from clawback): HICBC reclassifies as pure extraction (Snare). If positive: more clearly Tangled Rope or even Rope (coordination benefit preserved despite cost).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(child_poverty_vs_work_incentive_causality, empirical, 'Net welfare effect of HICBC on child poverty and work incentives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_hicbc_trap, 2013, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hicbc_tr_t0, uk_hicbc_trap, theater_ratio, 0, 0.35).
narrative_ontology:measurement(hicbc_tr_t6, uk_hicbc_trap, theater_ratio, 6, 0.5).
narrative_ontology:measurement(hicbc_tr_t12, uk_hicbc_trap, theater_ratio, 12, 0.58).

% Extraction over time
narrative_ontology:measurement(hicbc_be_t0, uk_hicbc_trap, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(hicbc_be_t6, uk_hicbc_trap, base_extractiveness, 6, 0.4).
narrative_ontology:measurement(hicbc_be_t12, uk_hicbc_trap, base_extractiveness, 12, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_hicbc_trap, resource_allocation).
narrative_ontology:affects_constraint(uk_hicbc_trap, uk_secondary_earner_labor_supply).
narrative_ontology:affects_constraint(uk_hicbc_trap, uk_child_poverty_outcomes).
narrative_ontology:affects_constraint(uk_hicbc_trap, uk_tax_progressivity_distortion).

% DUAL FORMULATION NOTE:
% The HICBC is a distinct constraint from the underlying child benefit system. The child benefit itself is a Rope (coordination mechanism for family support); the HICBC clawback is a tangled_rope that introduces extraction asymmetry. They are linked: HICBC is downstream of child benefit policy but creates its own structural constraints with measurably different ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
