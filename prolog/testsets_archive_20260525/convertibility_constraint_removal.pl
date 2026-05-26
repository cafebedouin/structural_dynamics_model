% ============================================================================
% CONSTRAINT STORY: convertibility_constraint_removal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_convertibility_constraint_removal, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: convertibility_constraint_removal
 *   human_readable: Removal of Gold Convertibility Constraint in Monetary Systems
 *   domain: monetary_economics/political_economy/institutional_history
 *
 * SUMMARY:
 *   The removal of gold convertibility constraints in monetary systems
 *   (exemplified by Bretton Woods collapse in 1971) represents a contested
 *   institutional transition that generates structurally distinct constraint
 *   classifications from different observer positions. From one reading, it
 *   is the removal of a natural law governing monetary systems (mountain).
 *   From another, it is a coordination mechanism enabling counter-cyclical
 *   policy (rope). From a third, it is an extraction mechanism transferring
 *   value from savers to borrowers and from peripheral economies to the
 *   hegemon (snare). From a fourth, it is a mixed constraint with genuine
 *   coordination benefits layered over asymmetric extraction (tangled rope).
 *   The kernel contest asks: do these perspectives represent different
 *   readings of a single institutional change, or does the ε-invariance
 *   principle require decomposition into multiple distinct constraints? The
 *   measurement trajectory (extractiveness rising from 0.15 to 0.58 over 27
 *   years, theater rising from 0.25 to 0.61) suggests decomposition may be
 *   warranted — the constraint's character changes significantly as its
 *   institutional embedding shifts from a contested transition (low initial
 *   extractiveness) toward an entrenched regime (high final extractiveness).
 *   This story models the unified constraint interpretation; a companion
 *   story should model the decomposed reading if empirical evidence suggests
 *   multiple ε-stable structures.
 *
 * KEY AGENTS:
 *   - Central Banking Authority: Primary beneficiary (institutional/arbitrage) — captures seigniorage, gains monetary policy autonomy, enables deficit accommodation without commodity constraint
 *   - Financial Sector Intermediaries: Secondary beneficiary (institutional/arbitrage) — expand credit creation capacity, reduce redemption risk, leverage fiat system for maturity transformation
 *   - Deficit-Financing States: Tertiary beneficiary (powerful/arbitrage) — enabled to run persistent deficits without gold-draining correction; central banks can accommodate fiscal expansion
 *   - Currency Savers: Primary victim (powerless/trapped) — lose redemption anchor, bear inflation risk, cannot exit into commodity-backed alternatives under legal tender laws
 *   - Peripheral Economies: Secondary victim (moderate/constrained) — dependent on dollar reserves despite asymmetric devaluation risk; constrained by dollar hegemony post-convertibility
 *   - Inflation-Bearing Populations: Tertiary victim (moderate/constrained) — populations dependent on fixed nominal wages or pensions bear uncompensated inflation costs if wage growth lags monetary expansion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(convertibility_constraint_removal, 0.58).
domain_priors:suppression_score(convertibility_constraint_removal, 0.68).
domain_priors:theater_ratio(convertibility_constraint_removal, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(convertibility_constraint_removal, extractiveness, 0.58).
narrative_ontology:constraint_metric(convertibility_constraint_removal, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(convertibility_constraint_removal, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(convertibility_constraint_removal, tangled_rope).
narrative_ontology:human_readable(convertibility_constraint_removal, "Removal of Gold Convertibility Constraint in Monetary Systems").
narrative_ontology:topic_domain(convertibility_constraint_removal, "monetary_economics/political_economy/institutional_history").

domain_priors:requires_active_enforcement(convertibility_constraint_removal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(convertibility_constraint_removal, central_banking_authority).
narrative_ontology:constraint_beneficiary(convertibility_constraint_removal, deficit_financing_states).
narrative_ontology:constraint_beneficiary(convertibility_constraint_removal, financial_sector_intermediaries).
narrative_ontology:constraint_victim(convertibility_constraint_removal, currency_savers).
narrative_ontology:constraint_victim(convertibility_constraint_removal, peripheral_economies).
narrative_ontology:constraint_victim(convertibility_constraint_removal, inflation_bearing_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CURRENCY SAVER (SNARE) — Trapped in fiat system with no exit to stable store of value. Convertibility removal eliminated the redemption option that anchored currency value. Saver bears full extraction risk through inflation, monetary expansion, and currency devaluation with no contractual recourse. Maximum suppression: cannot migrate savings to alternative anchor; legal tender laws enforce acceptance of depreciating currency.
constraint_indexing:constraint_classification(convertibility_constraint_removal, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PERIPHERAL ECONOMY (TANGLED ROPE) — Constrained by dependence on dollar-denominated debt and reserve accumulation. Fiat regime enables central bank coordination of trade settlement and reduces transactions costs (coordination benefit). Simultaneously, loss of convertibility constraint removes discipline on monetary expansion in reserve-currency hegemon, enabling seigniorage extraction and inflation export to peripheral states. High suppression: must accept dollar reserves despite asymmetric devaluation risk; cannot unilaterally return to commodity anchor.
constraint_indexing:constraint_classification(convertibility_constraint_removal, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CENTRAL BANKING AUTHORITY (ROPE) — Primary beneficiary. Convertibility removal enables coordination of credit expansion, counter-cyclical policy, and fiscal accommodation without gold constraint. Experiences constraint as pure coordination: the absence of redemption requirement enables stabilization function. Net beneficiary through expanded monetary policy toolkit and seigniorage capture. Exit options abundant: can manage fiat expansion or reintroduce commodity anchors through policy choice.
constraint_indexing:constraint_classification(convertibility_constraint_removal, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FINANCIAL SECTOR INTERMEDIARIES (ROPE) — Net beneficiaries through expanded credit creation capacity and reduced redemption risk. Fiat system enables greater leverage and maturity transformation than gold standard allowed. Experiences constraint removal as coordination: money creation is no longer limited by metallic reserves but by credit market conditions, enabling larger financial system. Low effective extraction: can arbitrage across currencies and assets; not trapped by fiat constraint.
constraint_indexing:constraint_classification(convertibility_constraint_removal, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: GOLD STANDARD INSTITUTIONAL REMNANTS (PITON) — Historical institutional structures (London Gold Fix, central bank gold reserves, gold reserves as confidence signaling) persist despite functional obsolescence. Central banks maintain gold reserves at significant opportunity cost; gold's role as reserve asset is largely performative. Theater ratio high: 61% of nominal activity around gold reserves is theater (confidence signaling, historical legitimacy maintenance) rather than functional constraint on monetary expansion. Persists through inertia despite fiat regime's operational independence from metallic reserves.
constraint_indexing:constraint_classification(convertibility_constraint_removal, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the removal of convertibility merely manifests an underlying constraint: any monetary system requires coordination of expectations around value anchor. The shift from gold to fiat institutional anchors changes the mechanism but not the necessity of having SOME anchor. This appears as a natural law of monetary systems: convertibility constraints are ineliminable, only their form varies. However, structural data contradicts this — identifiable beneficiaries (central banks, financial intermediaries) benefit from the specific form of anchor chosen, revealing naturalization of a contingent institutional choice.
constraint_indexing:constraint_classification(convertibility_constraint_removal, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(convertibility_constraint_removal_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(convertibility_constraint_removal, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(convertibility_constraint_removal, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(convertibility_constraint_removal, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(convertibility_constraint_removal, TR),
    TR >= 0.70.

:- end_tests(convertibility_constraint_removal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint exhibits substantial extraction capacity through multiple channels: (1) seigniorage capture by central banks and the hegemon; (2) inflation transfer from savers to borrowers; (3) monetary expansion enabling deficit financing that externalizes costs onto future taxpayers; (4) dollar hegemony enabling seigniorage export to peripheral economies. However, extractiveness is not maximal (0.70+) because genuine coordination benefits exist — fiat systems enable counter-cyclical policy, reduce transaction costs in settlement, and accommodate economic growth without commodity constraints. The 0.58 value reflects that extraction coexists with coordination function. Suppression (0.68): High. Multiple barriers prevent exit from fiat regime: (1) legal tender laws enforce acceptance of fiat currency; (2) no widely available alternative anchors until cryptocurrency development; (3) network effects lock in dollar as reserve currency; (4) capital controls prevent capital flight to commodity-backed alternatives. Suppression rises over the interval as the fiat regime becomes entrenched and alternatives become less accessible. Theater ratio (0.61): Moderate-high. Significant theatrical activity surrounds fiat legitimacy maintenance: (1) central bank independence ritual (performative autonomy theater masking political constraints); (2) inflation targeting frameworks (quantitative theater suggesting precision control despite limited actual constraint on monetary expansion); (3) gold reserve maintenance (legitimacy signaling despite operational irrelevance); (4) Federal Reserve mystique (opacity theater maintaining confidence). Theater increases over time as the system matures and requires greater legitimacy maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap in this constraint is exceptionally wide, reflecting the kernel contest over whether convertibility removal is a natural law (mountain), coordination mechanism (rope), extraction mechanism (snare), or hybrid (tangled rope). The central bank sees pure coordination — the freedom to expand money supply enables stabilization policy that was impossible under gold standard constraints. The currency saver sees pure extraction — the loss of redemption anchor transfers value through inflation without compensation. The peripheral economy sees hybrid constraint — dollar settlement mechanisms reduce transaction costs (coordination) but asymmetric devaluation risk extracts value (extraction). The analytical observer risks naturalizing the fiat regime as inevitable ('monetary systems require anchors, this is one form') without recognizing that the specific form chosen distributes costs and benefits to identifiable beneficiaries. The piton perspective reveals institutional inertia: gold reserves persist as confidence signaling despite functional irrelevance in a fiat system. The perspectival gap reflects genuine structural ambiguity in what 'convertibility removal' means — whether it is removal of constraint (enabling coordination) or imposition of new constraint (fiat regime enforces nominal acceptance).
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value (d) reflects their structural position relative to the extraction flow. Central banks and financial intermediaries are beneficiaries with full arbitrage capacity (low d, negative χ) — they experience the constraint as enabling their primary function. Deficit-financing states benefit from removal of gold discipline (low-moderate d) through seigniorage and deficit accommodation. Currency savers are victims trapped by legal tender laws and lack of alternatives (high d, high χ) — full directional exposure to extraction. Peripheral economies are constrained victims of dollar hegemony (moderate-high d) — they bear extraction costs but have some exit capacity through reserve diversification. The perspectival gap emerges because beneficiaries (central banks, financial sector) experienced the transition as liberation from constraint, while victims (savers, peripheral economies) experienced it as imposition of new extractive regime. The Bretton Woods negotiation itself was negotiated primarily by beneficiaries and ascending powers (US financial sector, UK seeking to offload gold backing costs) rather than by victim-represented constituencies. This explains high suppression: the constraint was imposed through institutional fait accompli rather than through negotiated acceptance.
 *
 * MANDATROPHY ANALYSIS:
 *   CONTESTED KERNEL READING: This constraint demonstrates mandatrophy resolution through kernel contest. The 'what changed?' question admits multiple readings: (1) Natural Law Reading (mountain): monetary systems inherently require coordination anchors; Bretton Woods simply substituted dollar for gold. (2) Coordination Reading (rope): removal of gold convertibility enabled counter-cyclical policy impossible under commodity constraint. (3) Extraction Reading (snare): fiat regime enabled seigniorage extraction, inflation transfer, and deficit financing at savers' expense. (4) Hybrid Reading (tangled rope): removal of one constraint (gold backing) imposed another constraint (fiat acceptance enforcement) that enables both coordination and extraction. The mandatrophy resolves by recognizing that the constraint's classification depends on which kernel reading you adopt. The false summit (mountain) naturalizes what is actually a contingent institutional choice — the necessity of coordination anchors does not imply fiat's necessity. The rope reading underweights extraction costs. The snare reading underweights genuine coordination benefits. The tangled rope captures both genuine functions and real extraction — it is the structurally accurate reading that acknowledges the constraint is neither pure coordination nor pure extraction but contains both. The measurement trajectory (rising extractiveness, rising theater) suggests the constraint's character shifted over time: initially a contested but genuine coordination solution (low ε), gradually becoming an entrenched extractive regime (high ε) maintained through institutional theater. This temporal drift supports tangled rope classification: the constraint began as coordination with modest extraction and evolved toward higher extraction as the fiat regime matured and gained legitimacy independent of functional necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    anchor_necessity_ambiguity,
    'Is the convertibility constraint itself necessary (immutable feature of any monetary system), or only its historical gold-based form?',
    'Comparative analysis of monetary systems with different anchors (fiat, crypto, commodity baskets, reserve currency) and their stability/extraction profiles. Test whether systems require ANY anchor or can function with pure coordination-based value.',
    'If any anchor is necessary: mountain classification holds and only the form changed. If no anchor is required in principle: the constraint was institutional choice, not natural law. Restructures fundamental understanding of monetary system necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anchor_necessity_ambiguity, conceptual, 'Whether convertibility constraint is necessary or historically contingent').

omega_variable(
    inflation_extraction_mechanism_ambiguity,
    'Does fiat monetary expansion constitute extraction from savers, or is inflation a symmetric cost borne collectively by all nominal debtors?',
    'Historical inflation distribution analysis: correlation between inflation rates and asset ownership, debt burden distribution, and wage dynamics. Identify whether inflation disproportionately harms savers with fixed-rate assets vs. borrowers with fixed-rate liabilities.',
    'If symmetric: many savers are also debtors and benefit net from inflation. Snare classification weakens; tangled rope becomes more accurate. If asymmetric: inflation is extractive transfer mechanism from savers to borrowers; snare and victim classifications strengthen.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inflation_extraction_mechanism_ambiguity, empirical, 'Whether fiat inflation is asymmetric extraction or symmetric nominal adjustment').

omega_variable(
    dollar_hegemon_durability,
    'Can dollar-centered fiat system persist indefinitely, or do peripheral economies face eventual exit windows through alternative anchor development (Bitcoin, commodity-backed, multi-reserve)?',
    'Scenario modeling of reserve currency competition; empirical tracking of de-dollarization trends; development of alternative credible monetary anchors and their adoption rates.',
    'If hegemon is durable: peripheral economy suppression remains high and tangled rope classification holds indefinitely. If exit windows emerge: suppression declines and constraint transitions toward rope or scaffold (if alternatives offer genuine coordination without extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dollar_hegemon_durability, empirical, 'Whether dollar hegemony constrains peripheral economies durably').

omega_variable(
    kernel_contest_reading_ambiguity,
    'Does convertibility constraint removal represent one institutional transition (single constraint viewed from multiple positions) or multiple structurally distinct constraints decomposable by ε-invariance?',
    'Decomposition test: Identify whether measuring the constraint via ''money supply expansion capacity,'' ''inflation transfer mechanism,'' ''reserve currency hegemony,'' and ''gold standard persistence'' yields stable or divergent ε values. If ε varies significantly (>0.30) across measurement bases, decompose into separate constraint stories.',
    'If single constraint: all perspectives are readings of one structural change. If multiple constraints: each measurement basis instantiates a different constraint with different beneficiary/victim structures. Determines whether this is one story or a constraint family requiring decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_reading_ambiguity, conceptual, 'Whether convertibility removal is single constraint or measurement-dependent decomposition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(convertibility_constraint_removal, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(convert_theater_1944, convertibility_constraint_removal, theater_ratio, 0, 0.25).
narrative_ontology:measurement(convert_theater_1959, convertibility_constraint_removal, theater_ratio, 15, 0.45).
narrative_ontology:measurement(convert_theater_1971, convertibility_constraint_removal, theater_ratio, 27, 0.61).

% Extraction over time
narrative_ontology:measurement(convert_extractiveness_1944, convertibility_constraint_removal, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(convert_extractiveness_1959, convertibility_constraint_removal, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(convert_extractiveness_1971, convertibility_constraint_removal, base_extractiveness, 27, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(convertibility_constraint_removal, resource_allocation).
narrative_ontology:boltzmann_floor_override(convertibility_constraint_removal, 0.12).
narrative_ontology:affects_constraint(convertibility_constraint_removal, dollar_hegemony_perpetuation).
narrative_ontology:affects_constraint(convertibility_constraint_removal, inflation_seigniorage_extraction).
narrative_ontology:affects_constraint(convertibility_constraint_removal, fixed_income_saver_immiseration).
narrative_ontology:affects_constraint(convertibility_constraint_removal, peripheral_currency_dependence).

% DUAL FORMULATION NOTE:
% Convertibility constraint removal decomposes into multiple constraint families depending on measurement basis. (1) Policy Autonomy constraint (ε≈0.25, rope): central bank's gained freedom to conduct counter-cyclical policy. (2) Seigniorage Extraction constraint (ε≈0.65, snare): monetary expansion as transfer mechanism from savers to monetary authorities. (3) Reserve Currency Hegemony constraint (ε≈0.58, tangled rope): dollar's role as settlement medium enables coordination but subjects peripheral economies to devaluation risk. Each measurement basis yields different ε; decomposition is empirically warranted if correlations analysis shows low coupling between policy autonomy dynamics and seigniorage dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(convertibility_constraint_removal, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
