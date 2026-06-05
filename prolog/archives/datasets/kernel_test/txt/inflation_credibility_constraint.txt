% ============================================================================
% CONSTRAINT STORY: inflation_credibility_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_inflation_credibility_constraint, []).

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
 *   constraint_id: inflation_credibility_constraint
 *   human_readable: Inflation Credibility Constraint in Fiat Currency Systems
 *   domain: monetary_economics/political_economy
 *
 * SUMMARY:
 *   The transition from gold-backed to fiat currency systems represents a
 *   fundamental shift in the institutional architecture of monetary
 *   constraint, yet the nature of what changed remains contested across
 *   different economic and political perspectives. This constraint story
 *   models the inflation credibility problem as a single structural
 *   phenomenon — the requirement to maintain public confidence in currency
 *   value through institutional commitment — analyzed from eight distinct
 *   positions that produce all six constraint types. The constraint exhibits
 *   tangled coordination and extraction: central banks coordinate spending
 *   across time and agents (genuine coordination function) while
 *   simultaneously extracting seigniorage and inflating away debt burdens
 *   (asymmetric extraction). The transition to fiat eliminated the hard
 *   resource constraint (gold reserves) but introduced a softer credibility
 *   constraint (inflation expectations must be anchored). The key empirical
 *   question is whether this represents a net reduction in extraction
 *   (removing gold-holder rents), a net increase (removing the automatic
 *   check on monetary expansion), or a simple shift in extraction mechanisms.
 *   The inflation-targeting regime that emerged in the 1990s-2000s represents
 *   a scaffold structure — institutional safeguards (central bank
 *   independence, forward guidance, inflation targets) that build credibility
 *   without eliminating the underlying seigniorage mechanism. The
 *   constraint's theater ratio has risen from 0.25 under the gold standard
 *   (where constraint was material: gold reserves had to be held) to 0.64 in
 *   the present (where constraint is largely performative: inflation targets
 *   are stated but flexible). This trajectory suggests the constraint is
 *   drifting toward piton classification — institutional inertia dressed in
 *   credibility rhetoric — even as wage-earners continue to experience it as
 *   a snare through inflation erosion.
 *
 * KEY AGENTS:
 *   - Wage Earners: Primary victim (powerless/trapped) — bear full cost of inflation through wage lag; no exit mechanism without extreme mobility; no voice in monetary policy
 *   - Fixed Income Recipients: Primary victim (powerless/trapped) — pensioners and bondholders whose contracts are locked; erosion is maximum and unavoidable
 *   - Central Banking Authority: Primary beneficiary (institutional/arbitrage) — controls monetary policy; extracts seigniorage; experiences constraint as coordination mechanism
 *   - Sovereign Debt Issuer (State): Secondary beneficiary (institutional/arbitrage) — inflates away nominal debt; experiences fiat system as spending-smoothing coordination
 *   - Financial Sector: Mixed agent (moderate/constrained) — benefits from asset inflation and risk premiums but faces timing/basis risk; can hedge but at cost
 *   - Inflation-Targeting Regime Coalition: Organized reformers (organized/constrained) — international institutions building credibility scaffolds; sunset structure as these institutions mature
 *   - Gold Standard Memory: Institutional inertia (institutional/arbitrage) — central banks maintain gold reserves for theater value despite elimination of material constraint
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing constructed institutional choice as immutable monetary law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(inflation_credibility_constraint, 0.58).
domain_priors:suppression_score(inflation_credibility_constraint, 0.62).
domain_priors:theater_ratio(inflation_credibility_constraint, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(inflation_credibility_constraint, extractiveness, 0.58).
narrative_ontology:constraint_metric(inflation_credibility_constraint, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(inflation_credibility_constraint, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(inflation_credibility_constraint, tangled_rope).
narrative_ontology:human_readable(inflation_credibility_constraint, "Inflation Credibility Constraint in Fiat Currency Systems").
narrative_ontology:topic_domain(inflation_credibility_constraint, "monetary_economics/political_economy").

domain_priors:requires_active_enforcement(inflation_credibility_constraint).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(inflation_credibility_constraint, central_banking_authority).
narrative_ontology:constraint_beneficiary(inflation_credibility_constraint, sovereign_debt_issuers).
narrative_ontology:constraint_victim(inflation_credibility_constraint, wage_earners).
narrative_ontology:constraint_victim(inflation_credibility_constraint, fixed_income_recipients).
narrative_ontology:constraint_victim(inflation_credibility_constraint, currency_savers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WAGE EARNER (SNARE) — Trapped in currency system; wages lag inflation; no exit to alternative store of value without extreme mobility cost. Bears full erosion cost during monetary expansion. No organized voice in monetary policy.
constraint_indexing:constraint_classification(inflation_credibility_constraint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FIXED INCOME RECIPIENT (SNARE) — Pensioners, bondholders at fixed rates; structurally trapped by contract terms that cannot be renegotiated. Maximum extraction with zero agency.
constraint_indexing:constraint_classification(inflation_credibility_constraint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: FINANCIAL SECTOR PARTICIPANT (TANGLED ROPE) — Banks and institutional investors benefit from inflation through asset price appreciation and the inflation risk premium, but also face timing and basis risk. Can partially hedge through derivative markets but at cost. Mixed extraction: they coordinate credit flows yet extract from inflation-driven asset appreciation.
constraint_indexing:constraint_classification(inflation_credibility_constraint, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CENTRAL BANKING AUTHORITY (ROPE) — Primary beneficiary. Controls monetary policy; inflates away sovereign debt; extracts seigniorage. Experiences constraint as pure coordination: managing inflation expectations IS the central mechanism for maintaining fiat currency acceptance. No exit cost — can always print money.
constraint_indexing:constraint_classification(inflation_credibility_constraint, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: SOVEREIGN DEBT ISSUER (ROPE) — State benefits from ability to inflate away nominal debt stock. Experiences fiat system as coordination mechanism: manages spending across generations through monetary expansion. Net beneficiary with arbitrage options.
constraint_indexing:constraint_classification(inflation_credibility_constraint, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: INFLATION-TARGETING REGIME (SCAFFOLD) — International monetary institutions (IMF, BIS, major central banks) instituted credibility-building mechanisms: inflation targets, central bank independence, forward guidance. These represent a sunset structure — as these institutional safeguards mature, the raw extraction mechanism (unanchored inflation) decays. Theater ratio lower here because the institutional apparatus genuinely constrains monetary discretion, not purely performatively.
constraint_indexing:constraint_classification(inflation_credibility_constraint, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: GOLD STANDARD INSTITUTIONAL MEMORY (PITON) — The rhetoric of 'sound money' and gold backing persists as a vestigial institutional commitment even though the functional constraint (the need to maintain gold reserves) has been eliminated. Central banks maintain gold reserves for theater value, not constraint. The gold standard was real; nostalgia for it is performative institutional inertia.
constraint_indexing:constraint_classification(inflation_credibility_constraint, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, fiat currency systems face an inherent constraint: inflation expectations must be anchored to maintain purchasing power credibility. No society with fiat currency can sustain hyperinflation indefinitely. This appears as a natural law of monetary systems. However, this reading naturalizes what is actually a contestable institutional design choice (inflation target levels, central bank mandates, fiscal-monetary coordination). False summit risk: the constraint may be constructed rather than immutable.
constraint_indexing:constraint_classification(inflation_credibility_constraint, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(inflation_credibility_constraint_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(inflation_credibility_constraint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(inflation_credibility_constraint, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(inflation_credibility_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(inflation_credibility_constraint, TR),
    TR >= 0.70.

:- end_tests(inflation_credibility_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint exhibits sustained extraction through seigniorage (central banks capture real resources through monetary expansion), inflation erosion of wage earners' purchasing power, and redistribution to debt holders. However, extraction is not maximal because: (1) inflation targeting regimes reduce the discretionary component below what historical hyperinflations reveal is possible, (2) coordinating spending across generations has genuine value, and (3) the alternative (deflation or resource constraint) would impose different costs. The historical trajectory shows extractiveness peaked at 0.68 during the Great Inflation (1970s), declined to 0.54 at the maturity of inflation targeting (2010s), and has risen slightly to 0.58 as credibility mechanisms face new pressures (post-2008, post-2020). Suppression (0.62): Moderate-high. Wage-earners face structural barriers to exit: they cannot substitute into commodity-backed alternatives, cannot hedge inflation on wages, and face coordination problems in demanding wage adjustment (free-rider problem in wage bargaining). Fixed-income recipients face contractual suppression (cannot renegotiate bond terms). The suppression is not total because some agents (financial sector) can partially hedge through derivative markets. Theater ratio (0.64): Moderate-high. The shift from gold standard (theater=0.25, constraint was material) to present (theater=0.64) reflects that inflation constraint has become increasingly performative. Gold standard theater was low because the constraint was enforced by material requirement (must hold gold reserves or currency exchange breaks down). Inflation targeting relies on rhetoric and forward guidance — central banks announce targets and market participants price them in, but the targets are not mechanically enforced and have frequently been breached. The theater increased during Volcker's disinflation (0.68 peak) as credibility-building theater, then settled at 0.64 as the institutional apparatus normalized.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full perspectival range. Wage-earners see a snare — they are trapped in a currency system where their purchasing power erodes through a mechanism they cannot exit or influence. Fixed-income recipients see an equivalent snare with even less agency (contractual lock). The central banking authority and sovereign debt issuer see a rope — they coordinate spending across time and agents; the constraint is their tool for managing complex collective action. Financial sector agents see tangled rope — they benefit from inflation-driven asset appreciation and carry inflation risk premiums, but also face drawdowns during disinflationary episodes. The inflation-targeting coalition sees a scaffold — the institutional mechanisms (central bank independence, forward guidance, target frameworks) are building credibility structures that will sunset the raw extraction mechanism as they mature. The gold standard institutional memory sees a piton — the rhetoric of 'sound money' persists as theater in central bank gold holdings and policy language, but the functional constraint has been eliminated. The civilizational analytical observer risks seeing a mountain — inflation credibility as an immutable law of fiat currency systems — but the structural data suggests this is a false summit: alternative monetary systems (commodity baskets, decentralized currencies, hard constraints on monetary expansion) were viable but politically rejected.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent reflects their structural position relative to inflation flows. Wage-earners: d ≈ 0.92 (full target). They experience maximum erosion; wages lag inflation by measurable amounts; no arbitrage options. Fixed-income recipients: d ≈ 0.95 (full target, plus contractual lock). Central banking authority: d ≈ 0.08 (full beneficiary). Controls policy; captures seigniorage; has complete arbitrage options (can always print). Sovereign debt issuer: d ≈ 0.12 (beneficiary with slight constraint from credibility maintenance). Financial sector: d ≈ 0.52 (near-symmetric; benefits from inflation but carries risk). Inflation-targeting coalition: d ≈ 0.40 (organized agent with moderate extraction but high agency through institutional participation). Gold standard memory: d ≈ 0.05 (beneficiary of institutional inertia; maintains central bank prestige). Analytical observer: d ≈ 0.72 (observer position, moderate extraction experienced at civilizational level if natural-law view is false summit).
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING AMBIGUITY: This constraint is one reading of a contested kernel — 'what changed when we moved from gold standard to fiat?' Different readings emit different constraints: (1) Monetary Coordination Function: the shift enabled more flexible spending management across generations (lower ε, Rope dominates). (2) Seigniorage Extraction Mechanism: the shift removed the hard constraint on monetary expansion and enabled sustained real resource capture (higher ε, Snare dominates). (3) Inflation Expectations Anchoring: the shift moved from material constraint to credibility constraint (requires new institutional scaffolds). These three are structurally distinct and could be decomposed into separate stories. However, they also collapse into a single constraint when viewed at the right level of abstraction: the constraint is that a fiat currency system REQUIRES credibility maintenance to prevent hyperinflation, and this requirement creates the space for both coordination (spending management) and extraction (seigniorage). The mandatrophy resolves by acknowledging that the six perspectives are legitimate readings of this single ε, each revealing different faces of the same institutional structure. The false summit risk (mountain from analytical perspective) is real: the naturalizing language ('this is how money works') conceals that this is how *fiat currency with credibility-dependent value* works, and alternative systems were viable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordinating_function_vs_extraction_layering,
    'Is the inflation constraint primarily a coordination mechanism (managing spending across time and agents) layered with extraction, or is extraction the primary function with coordination as the justifying narrative?',
    'Historical counterfactual: what would happen if the central bank committed to zero inflation with automatic deflationary adjustment? If coordination function is real, such a regime should work; if extraction is primary, agents would immediately demand inflation return.',
    'If coordination-primary: Rope or Tangled Rope from most perspectives. If extraction-primary: Snare from wage-earner perspective, suggesting the constraint is fundamentally redistributive rather than coordinating.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordinating_function_vs_extraction_layering, conceptual, 'Whether inflation constraint is primarily coordinating or primarily extractive').

omega_variable(
    gold_standard_alternative_impossibility,
    'Was the transition from gold standard to fiat currency the *only* feasible path, or were alternative monetary regimes (commodity baskets, decentralized currency, supranational backing) structurally viable but politically rejected?',
    'Reconstruction of historical policy deliberations; analysis of parallel jurisdictions that maintained commodity backing; examination of why supranational currency unions (Bretton Woods, Euro, SDR) all eventually faced credibility crises',
    'If fiat was inevitable: constraint is closer to mountain (immutable by resource scarcity or technology). If alternatives were viable but rejected: constraint is constructive Snare or Tangled Rope sustained by concentrated beneficiary power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gold_standard_alternative_impossibility, empirical, 'Whether fiat currency was structurally inevitable or politically chosen').

omega_variable(
    inflation_targeting_effectiveness_and_internality,
    'Do inflation targets and central bank independence actually constrain monetary discretion, or are they performative commitments that permit the same seigniorage extraction under a narrative of ''responsible management''?',
    'Time-series analysis of inflation outcomes pre- and post-inflation-targeting adoption; comparison of actual inflation vs. stated targets; examination of whether central bank independence correlates with lower inflation or merely smoother inflation cycles',
    'If targets are effective: scaffold perspective is correct — the constraint has a real sunset as credibility institutions mature. If targets are performative: theater ratio is higher, and the constraint remains Snare for wage-earners regardless of institutional rhetoric.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inflation_targeting_effectiveness_and_internality, empirical, 'Whether inflation targets and central bank independence actually constrain discretionary monetary expansion').

omega_variable(
    fiat_vs_commodity_backed_extraction_comparison,
    'Did the transition from gold standard to fiat reduce total extraction (by eliminating resource-scarcity rent to gold holders), maintain it (by shifting from seigniorage + gold rent to pure seigniorage), or increase it (by removing the hard constraint on monetary expansion)?',
    'Historical comparison of seigniorage rates, inflation volatility, and inequality metrics across gold-standard vs. fiat periods; computation of effective extraction rates including both inflation and financial sector rents',
    'If extraction reduced: fiat system is net improvement (Rope). If maintained: same extraction, different mechanism (Tangled Rope from multiple perspectives). If increased: fiat system is net extraction increase (Snare for wage-earners).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiat_vs_commodity_backed_extraction_comparison, empirical, 'Whether fiat currency increased, maintained, or reduced total extraction compared to gold standard').

omega_variable(
    credibility_anchor_externality,
    'Is inflation credibility a public good (everyone benefits from stable currency value) or a zero-sum game (credibility built by some agents at expense of others through inflation distribution)?',
    'Analysis of inflation incidence by income quintile; examination of whether inflation-targeting policies reduce or increase wealth inequality; comparison of nominal wage growth vs. inflation across income groups',
    'If public good: Rope classification dominates; fiat system coordinates expectations beneficially. If zero-sum: extraction is disguised by public-good language; Snare or Tangled Rope classification dominates from redistributive perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(credibility_anchor_externality, empirical, 'Whether inflation credibility is a public good or a zero-sum extraction mechanism').

omega_variable(
    kernel_reading_decomposition_signal,
    'Should this constraint be decomposed into multiple structurally distinct constraints (monetary coordination function vs. seigniorage extraction mechanism vs. inflation-expectation anchoring), or is it a single constraint read from different institutional positions?',
    'Epsilon invariance test: do the three candidate constraints (coordination, seigniorage extraction, expectation anchoring) have different ε values when measured independently? If epsilon differs by >0.20, decompose into separate stories.',
    'If decompose: each story has its own beneficiary/victim structure and lifecycle. If unified: the presheaf over observation positions is the answer, and mandatrophy resolution requires showing how all six types are legitimate readings of the same epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_decomposition_signal, conceptual, 'Whether inflation credibility is one constraint or multiple structurally distinct constraints read from different positions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(inflation_credibility_constraint, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(infcred_theater_gold_standard, inflation_credibility_constraint, theater_ratio, 0, 0.25).
narrative_ontology:measurement(infcred_theater_bretton_woods, inflation_credibility_constraint, theater_ratio, 5, 0.4).
narrative_ontology:measurement(infcred_theater_great_inflation, inflation_credibility_constraint, theater_ratio, 10, 0.55).
narrative_ontology:measurement(infcred_theater_volcker, inflation_credibility_constraint, theater_ratio, 15, 0.68).
narrative_ontology:measurement(infcred_theater_targeting_maturity, inflation_credibility_constraint, theater_ratio, 20, 0.64).
narrative_ontology:measurement(infcred_theater_present, inflation_credibility_constraint, theater_ratio, 25, 0.64).

% Extraction over time
narrative_ontology:measurement(infcred_extractiveness_gold_standard_era, inflation_credibility_constraint, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(infcred_extractiveness_bretton_woods_transition, inflation_credibility_constraint, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(infcred_extractiveness_great_inflation, inflation_credibility_constraint, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(infcred_extractiveness_volcker_disinflation, inflation_credibility_constraint, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(infcred_extractiveness_inflation_targeting_maturity, inflation_credibility_constraint, base_extractiveness, 20, 0.54).
narrative_ontology:measurement(infcred_extractiveness_present, inflation_credibility_constraint, base_extractiveness, 25, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(inflation_credibility_constraint, resource_allocation).
narrative_ontology:affects_constraint(inflation_credibility_constraint, wage_lag_mechanism).
narrative_ontology:affects_constraint(inflation_credibility_constraint, sovereign_debt_sustainability).
narrative_ontology:affects_constraint(inflation_credibility_constraint, financial_sector_risk_premium).

% DUAL FORMULATION NOTE:
% The inflation credibility constraint is upstream of multiple domain-specific constraints. Changes in central bank credibility affect wage-bargaining dynamics (wage_lag_mechanism), sovereign borrowing costs (sovereign_debt_sustainability), and financial risk pricing (financial_sector_risk_premium). Each downstream constraint has its own ε and perspectives; they are linked because they depend on the credibility structure this constraint describes. A decomposition into Seigniorage Extraction (higher ε) and Monetary Coordination (lower ε) as separate constraint stories would clarify whether the extraction is the primary function or a side effect of coordination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(inflation_credibility_constraint, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
