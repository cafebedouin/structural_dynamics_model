% ============================================================================
% CONSTRAINT STORY: help_to_buy_uk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_help_to_buy_uk, []).

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
 *   constraint_id: help_to_buy_uk
 *   human_readable: UK 'Help to Buy' Equity Loan Scheme
 *   domain: economic/housing_policy
 *
 * SUMMARY:
 *   The UK's 'Help to Buy' Equity Loan Scheme (2013-2023) exemplifies a snare
 *   constraint disguised as pro-social policy. The scheme offered government
 *   equity loans (up to 20% of property price, or 25% in London) to
 *   first-time buyers with deposits of 5%, ostensibly to reduce barriers to
 *   homeownership. In practice, the scheme operated as a demand-side subsidy
 *   in a supply-constrained housing market, primarily benefiting house
 *   builders, developers, and existing property owners through inflated house
 *   prices, while locking first-time buyers into high-debt purchases and
 *   future renters and excluded cohorts into rental dependence and price
 *   inflation. The extractiveness evolved from 0.28 (early years, when
 *   builders perceived genuine demand expansion) to 0.52 (later years, as
 *   price-capture mechanism became visible and builder windfall solidified).
 *   Theater ratio increased from 0.38 to 0.61 as the rhetoric of 'first-time
 *   buyer assistance' diverged ever more visibly from the reality of
 *   demand-neutral, supply-constrained subsidy capture. The scheme's sunset
 *   clause (announced March 2023, closed July 2023) reflects political
 *   pressure from housing charities and economists who exposed the extractive
 *   mechanism, though closure came only after a decade of price inflation and
 *   inter-generational redistribution.
 *
 * KEY AGENTS:
 *   - First-Time Buyers: Primary victim (powerless/trapped) — ostensible beneficiaries who become debt-holders at inflated prices; lack alternative pathways to homeownership
 *   - House Builders and Developers: Primary beneficiary (institutional/arbitrage) — capture most price appreciation through reduced buyer deposit constraints; sell same properties at higher prices with reduced price sensitivity
 *   - Future Renters and Excluded Cohorts: Secondary victim (moderate/trapped) — excluded by scheme timing, geography, or eligibility; face inflated house prices and reduced rental supply as owner-occupancy preference increases
 *   - Taxpayers: Secondary victim (powerless/trapped) — bear £37bn gross exposure; absorb equity losses and opportunity costs without political exit mechanism
 *   - Housing Charities and Think Tanks: Reform coalition (organized/constrained) — advocated for supply-side solutions; drove pressure for scheme closure through analysis and advocacy
 *   - Government/Treasury: Institutional actor (institutional/constrained) — balances short-term homeownership targets (political) against long-term fiscal costs (economically); constrained by electoral cycle
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(help_to_buy_uk, 0.52).
domain_priors:suppression_score(help_to_buy_uk, 0.68).
domain_priors:theater_ratio(help_to_buy_uk, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(help_to_buy_uk, extractiveness, 0.52).
narrative_ontology:constraint_metric(help_to_buy_uk, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(help_to_buy_uk, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(help_to_buy_uk, snare).
narrative_ontology:human_readable(help_to_buy_uk, "UK 'Help to Buy' Equity Loan Scheme").
narrative_ontology:topic_domain(help_to_buy_uk, "economic/housing_policy").

domain_priors:requires_active_enforcement(help_to_buy_uk).
narrative_ontology:has_sunset_clause(help_to_buy_uk).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(help_to_buy_uk, house_builders).
narrative_ontology:constraint_beneficiary(help_to_buy_uk, land_developers).
narrative_ontology:constraint_beneficiary(help_to_buy_uk, property_owners).
narrative_ontology:constraint_victim(help_to_buy_uk, first_time_buyers).
narrative_ontology:constraint_victim(help_to_buy_uk, future_renters).
narrative_ontology:constraint_victim(help_to_buy_uk, taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FIRST-TIME BUYER (SNARE) — Trapped by housing market inflation, deposit requirements, and limited alternatives. Without Help to Buy, locked out of homeownership entirely; with it, locked into debt accumulation at inflated prices. Government loan is presented as assistance but functions as debt subordination to mortgage lenders. d≈0.93, f(d)≈1.40, σ=1.0 → χ≈0.73.
constraint_indexing:constraint_classification(help_to_buy_uk, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HOUSE BUILDERS AND DEVELOPERS (ROPE) — Primary beneficiary. Government equity loans increase demand elasticity and reduce buyer deposit constraints, enabling developers to raise prices without losing sales volume. Coordination function: scheme coordinates buyer demand with supply-constrained market. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.05. Negative effective extraction = net beneficiary.
constraint_indexing:constraint_classification(help_to_buy_uk, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: FUTURE RENTERS AND EXCLUDED COHORTS (SNARE) — Trapped outside the scheme (eligibility limits, regional variation, timing exclusion). Inflated house prices driven by scheme reduce rental supply and increase rents. Later cohorts face higher prices without scheme access. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(help_to_buy_uk, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: TAXPAYERS (SNARE) — Trapped by fiscal obligations. Scheme cost: £37bn gross exposure (2013-2023). Equity losses, loan defaults, and opportunity costs are borne by public finance. Voters cannot exit without political mobilization. d≈0.88, f(d)≈1.32, σ=1.0 → χ≈0.69.
constraint_indexing:constraint_classification(help_to_buy_uk, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 5: POLICY REFORM COALITION (SCAFFOLD) — Housing charities, think tanks, and economists advocated for supply-side solutions instead of demand-side subsidy. Reform pressure built throughout 2019-2023 as scheme's extractive effects became visible. Scheme announced closure in March 2023 (sunset: July 2023). Organized actors see pathway out through policy sunset. d≈0.45, f(d)≈0.45, σ=1.0 → χ≈0.23.
constraint_indexing:constraint_classification(help_to_buy_uk, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: GOVERNMENT / TREASURY (TANGLED ROPE) — Coordination function: scheme solves short-term housing delivery and homeownership rate targets (political incentives). Extraction: government subordinates its equity to mortgage lenders (poor recovery priority), absorbs losses, and distorts land/building markets. Constrained by electoral cycle pressure and Treasury capacity to absorb losses. Requires active enforcement (scheme administration, equity recovery process, debt management). d≈0.52, f(d)≈0.65, σ=1.0 → χ≈0.34.
constraint_indexing:constraint_classification(help_to_buy_uk, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (PITON) — From civilizational distance, Help to Buy is a degraded version of supply-side housing policy. The scheme was presented as 'helping buyers' but functioned as a land-value subsidy to developers. Theater_ratio=0.61 reflects the gap between stated purpose (first-time buyer assistance) and actual mechanism (price-neutral demand stimulus capturing by supply). The ritual of 'mortgage approval' masks the constraint: equity loans simply shifted who captures the subsidy without addressing housing shortage. d≈0.68, f(d)≈1.08, σ=1.0 → χ≈0.56.
constraint_indexing:constraint_classification(help_to_buy_uk, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(help_to_buy_uk_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(help_to_buy_uk, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(help_to_buy_uk, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(help_to_buy_uk, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(help_to_buy_uk, TR),
    TR >= 0.70.

:- end_tests(help_to_buy_uk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): High-moderate. The scheme extracts from first-time buyers and future cohorts through debt-at-inflated-prices. The extraction is not immediate/severe enough for highest-tier snare (0.70+) because buyers do receive asset appreciation (though less than they would in less-inflated market), and government equity stake provides some downside protection (though poor recovery priority). The evolution from 0.28 to 0.52 reflects the dynamic: early years, builders saw genuine demand expansion and price-neutral equilibrium; by years 5-10, price capture mechanism had solidified and buyers were locked into premium-price debt with constrained exit. Suppression (0.68): High. First-time buyers face suppressed alternatives: save longer (wage stagnation + rent inflation make this harder), relocate to cheaper regions (job/family constraints), or rent indefinitely (insecurity, no wealth accumulation). Government frames the constraint as 'help' rather than 'debt subordination,' suppressing alternatives through narrative. Builders suppress land supply to maintain price-inelasticity. Theater ratio (0.61): Moderate-high. The scheme's presentation as 'helping first-time buyers' masks its mechanism as 'demand stimulus that builders capture.' Mortgage approval ritual masks equity subordination. Government equity loan is presented as 'investment' but functions as grant-disguised-as-loan due to low recovery prospects. Theater evolved as awareness spread (charities exposed the mechanism), causing public perception gap between official purpose and structural reality.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. First-time buyers experience a snare (trapped by debt-at-inflated-prices with limited alternatives). House builders experience a rope (genuine coordination of demand-supply, with them as beneficiaries). Future renters experience a snare (locked out by inflated prices). Taxpayers experience a snare (fiscal obligation, zero exit). Reform coalition experiences a scaffold (visible sunset pathway through policy change and organized advocacy). Government experiences tangled rope (coordination function—targets met—mixed with extraction—fiscal costs mounting). The analytical observer sees a degraded demand-subsidy mechanism (piton). No agent sees the constraint as beneficial except builders (rope) and benefiting owners (property appreciation). The scheme's sunset (announced March 2023, closed July 2023) was a direct result of perspectival gap becoming politically salient — housing experts demonstrated the extraction mechanism, shifting policy from 'help' framing to acknowledged fiscal burden and demand-side-subsidy failure.
 *
 * DIRECTIONALITY LOGIC:
 *   First-time buyers: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction (bounded only by asset appreciation). No exit without reneging on debt or renting indefinitely. House builders: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Can exit scheme (it was voluntary to participate) and benefit from price increases whether they used scheme or not. Future renters: Victim + trapped → d≈0.92, f(d)≈1.38. Locked out by inflated prices; cannot access scheme retroactively. Taxpayers: Victim + trapped → d≈0.88, f(d)≈1.32. Fiscal obligation; zero exit without political mobilization. Government: Institutional + constrained → d≈0.52, f(d)≈0.65. Mixed: coordination function (targets met) but extraction (fiscal losses); constrained by electoral cycle and Treasury capacity. Reform coalition: Organized + constrained → d≈0.45, f(d)≈0.45. Low effective extraction; organized enough to drive policy change (constrained exit achieved through advocacy).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: Help to Buy avoids the trap of mislabeling pure extraction as coordination by showing the structural facts: government equity is subordinated to mortgage debt (extraction mechanism), no coordination benefit to buyers (they pay premium prices for access to same property), and builders absorb the subsidy through price capture (not sellers sharing loss). The scheme claims coordination function ('help') but possesses none — the government-buyer 'partnership' is one-directional extraction. Snare classification is confirmed: victims (first-time buyers, future renters, taxpayers) far outnumber beneficiaries (builders, existing owners); suppression is high (constrained alternatives); extraction is significant (debt-at-inflated-prices). The tangled_rope and scaffold perspectives are real but secondary — the government's coordination function (meeting homeownership targets) is political theater masking extraction, and the reform coalition's scaffold is a genuine pathway out (policy sunset), validating that the constraint is not immutable. The piton perspective (demand-subsidy theater) reflects the scheme's degradation from stated purpose — it persisted due to inertia and political commitment despite mounting evidence of ineffectiveness, until organized pressure (charities, economists) made costs salient enough to overcome path dependence. The ultimate closure (July 2023) validates that the constraint was contingent, not natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    supply_demand_elasticity_threshold,
    'At what elasticity threshold does demand-side subsidy captured by supply-side constraints flip from coordinating buyers to pure extraction?',
    'Cross-regional analysis of Help to Buy uptake vs house price growth, controlling for underlying supply constraints; comparison with regions with elastic supply (USA sunbelt) vs inelastic supply (UK southeast)',
    'If threshold very elastic: scheme is legitimately coordinating buyer-supply gap (Rope from more perspectives). If threshold highly inelastic: scheme is extraction from outset (Snare confirmed from all perspectives except builders).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supply_demand_elasticity_threshold, empirical, 'Supply elasticity threshold determining demand-subsidy incidence').

omega_variable(
    equity_recovery_realism,
    'What fraction of government equity loans will ultimately be recovered at positive value or break-even vs loss?',
    'Long-term tracking of loan book performance post-scheme closure; analysis of repayment rates, default rates, and house price appreciation relative to purchase price plus government equity stake',
    'If >70% recovery: scheme losses are modest, may be defensible as investment. If <30% recovery: scheme is pure transfer to buyers and developers, disguised as loan program.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equity_recovery_realism, empirical, 'Government equity loan recovery rate').

omega_variable(
    counterfactual_homeownership_mechanism,
    'Did Help to Buy create first-time buyers who would not have bought otherwise, or did it subsidize purchases that would have happened through other channels (larger deposits, different property price point)?',
    'Cohort analysis comparing Help to Buy recipients with matched non-recipients; analysis of whether Help to Buy buyers were substituting for renters or for same-cohort buyers using larger deposits and lower-price properties',
    'If genuine new buyers: scheme enabled new household formation (weaker extraction narrative). If substitution: scheme is pure wealth transfer to buyers already in-queue, magnifying house price inflation (pure snare confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_homeownership_mechanism, empirical, 'Whether Help to Buy created marginal buyers or substituted for alternative purchase paths').

omega_variable(
    land_value_capture_redistribution,
    'Did Help to Buy primarily benefit builders through higher land values, or did price increases distribute to existing property owners as windfall appreciation?',
    'Land value appraisal analysis; comparison of vacant land prices pre/post-scheme in Help to Buy active regions vs control regions; analysis of property appreciation rates by owner type (owner-occupier vs investor)',
    'If builders captured most gain: Help to Buy is snare against first-time buyers benefiting developers (current narrative). If existing owners captured gain: Help to Buy is inter-generational wealth transfer from younger/poorer cohorts to older/wealthier property-owning cohort (expanded snare narrative).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_value_capture_redistribution, empirical, 'Distribution of Help to Buy-induced land value gains').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(help_to_buy_uk, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(htb_tr_t0, help_to_buy_uk, theater_ratio, 0, 0.38).
narrative_ontology:measurement(htb_tr_t5, help_to_buy_uk, theater_ratio, 5, 0.52).
narrative_ontology:measurement(htb_tr_t10, help_to_buy_uk, theater_ratio, 10, 0.61).

% Extraction over time
narrative_ontology:measurement(htb_be_t0, help_to_buy_uk, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(htb_be_t5, help_to_buy_uk, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(htb_be_t10, help_to_buy_uk, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(help_to_buy_uk, resource_allocation).
narrative_ontology:affects_constraint(help_to_buy_uk, uk_housing_shortage).
narrative_ontology:affects_constraint(help_to_buy_uk, intergenerational_wealth_inequality).
narrative_ontology:affects_constraint(help_to_buy_uk, mortgage_debt_burden).

% DUAL FORMULATION NOTE:
% Help to Buy is a specific policy instantiation of the broader constraint of demand-side-subsidy-in-supply-constrained-markets. It is downstream of housing shortage structural constraints and feeds upstream into intergenerational wealth inequality and household debt burden metrics. The ε=0.52 reflects this specific policy's capture mechanism; the parent constraint (housing shortage) would have higher ε if it were modeled as a pure snare, but lower if modeled as a coordination problem awaiting supply-side solutions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(help_to_buy_uk, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
