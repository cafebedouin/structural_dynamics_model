% ============================================================================
% CONSTRAINT STORY: monetary_regime_transition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_regime_transition, []).

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
 *   constraint_id: monetary_regime_transition
 *   human_readable: Sovereign Fiat Currency Regime
 *   domain: economic/political
 *
 * SUMMARY:
 *   The sovereign fiat currency regime emerged in the 1970s following the
 *   collapse of the Bretton Woods system and represents a structural
 *   transition from commodity-backed to trust-based monetary systems. This
 *   constraint exhibits the full range of DR classifications from different
 *   perspectives, making it diagnostic for understanding how distributional
 *   asymmetries are embedded in seemingly neutral monetary institutions. The
 *   same structural phenomenon — the power of a government to issue currency
 *   without commodity backing — appears as a legitimate coordination
 *   mechanism (Rope) from the perspective of monetary authorities and
 *   debtors, as pure extraction (Snare) from the perspective of wage earners
 *   and savers, as a mixed coordination-extraction hybrid (Tangled Rope) from
 *   the perspective of organized creditors, as a degraded ritual (Piton) from
 *   the historical perspective of gold standard advocates, as a temporary
 *   problem being solved (Scaffold) from the perspective of digital currency
 *   reformers, and as an immutable natural law (Mountain) from the
 *   civilizational analytical perspective. The constraint's theater_ratio
 *   (0.58) reflects that central bank communication emphasizes stability,
 *   prudence, and rule-based policy while actual monetary creation is largely
 *   discretionary and responsive to political pressure. The extractiveness
 *   value (0.38) represents moderate extraction through seigniorage,
 *   inflation tax on nominal savings, and first-issuer advantage, offset
 *   partially by genuine coordination benefits of fiat currency for credit
 *   expansion and countercyclical policy.
 *
 * KEY AGENTS:
 *   - Wage Earners: Primary victims (powerless/trapped) — wages erode in real terms as inflation advances; cannot exit national currency without expatriation
 *   - Savers: Primary victims (powerless/trapped) — nominal savings lose purchasing power systematically through inflation; alternative stores of value (foreign currency, commodities) are suppressed by capital controls and regulatory barriers
 *   - Monetary Authority (Central Bank): Primary beneficiary (institutional/arbitrage) — captures seigniorage and inflation tax; controls policy levers for countercyclical intervention
 *   - First Issuers (Governments, Banks): Secondary beneficiary (institutional/arbitrage) — issue debt at low real rates before inflation sets in; benefit from credit expansion enabled by fiat
 *   - Debtor Class: Secondary victim/beneficiary (moderate/constrained) — nominal debt is inflated away (benefit) but originating lenders captured early benefits (extraction); constrained by debt obligations
 *   - Organized Creditor Class (Pension Funds, Insurers): Moderate beneficiary (organized/mobile) — can hedge inflation through derivatives and foreign currency strategies; extract through financial engineering while enjoying coordination benefits
 *   - Gold Standard Legacy: Vestigial perspective (institutional/arbitrage) — historical artifacts (gold reserves, sound money rhetoric) maintained through institutional inertia despite lack of functional role
 *   - Digital Currency Reformers: Organized agents (organized/constrained) — view fiat regime as temporary failure being solved by blockchain, CBDC, and decentralized finance; building alternative payment infrastructure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_regime_transition, 0.38).
domain_priors:suppression_score(monetary_regime_transition, 0.65).
domain_priors:theater_ratio(monetary_regime_transition, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_regime_transition, extractiveness, 0.38).
narrative_ontology:constraint_metric(monetary_regime_transition, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(monetary_regime_transition, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_regime_transition, tangled_rope).
narrative_ontology:human_readable(monetary_regime_transition, "Sovereign Fiat Currency Regime").
narrative_ontology:topic_domain(monetary_regime_transition, "economic/political").

domain_priors:requires_active_enforcement(monetary_regime_transition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_regime_transition, monetary_authority).
narrative_ontology:constraint_beneficiary(monetary_regime_transition, first_issuers).
narrative_ontology:constraint_beneficiary(monetary_regime_transition, debt_holders).
narrative_ontology:constraint_victim(monetary_regime_transition, wage_earners).
narrative_ontology:constraint_victim(monetary_regime_transition, savers).
narrative_ontology:constraint_victim(monetary_regime_transition, inflation_displaced).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WAGE EARNER (SNARE) — Trapped in national currency; cannot exit without expatriation or expatriate assets. Fiat regime extracts through inflation erosion of nominal wages and savings. Suppression of alternatives (capital controls, alternative currencies) is high. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.52.
constraint_indexing:constraint_classification(monetary_regime_transition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SAVER (SNARE) — Trapped in nominal currency assets; inflation erodes purchasing power systematically. Cannot opt into commodity-backed alternatives or hard currencies without regulatory arbitrage. The regime extracts real value through monetary expansion. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.54.
constraint_indexing:constraint_classification(monetary_regime_transition, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: DEBTOR CLASS (TANGLED ROPE) — Constrained exit (mobile but with debt obligations). Fiat regime creates coordination benefit: nominal debt is inflated away, reducing real repayment burden. But also extraction: originating lenders capture the benefits of debt-financed growth before inflation erodes terms. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.29. Mixed extraction with genuine coordination function.
constraint_indexing:constraint_classification(monetary_regime_transition, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MONETARY AUTHORITY (ROPE) — Benefits from fiat issuance; controls seigniorage and inflation tax. Experiences the constraint as coordination: fiat currency enables policy flexibility, deficit financing, and countercyclical intervention. Can arbitrage between monetary and fiscal domains. d≈0.08, f(d)≈-0.09, σ=1.0 → χ≈-0.03. Net beneficiary.
constraint_indexing:constraint_classification(monetary_regime_transition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: FIRST ISSUERS (ROPE) — Banks and governments that issue debt early benefit from low real borrowing costs before inflation sets in. Fiat regime creates coordination benefit: easy credit enables investment and growth. d≈0.10, f(d)≈-0.08, σ=1.0 → χ≈-0.03. Net beneficiary.
constraint_indexing:constraint_classification(monetary_regime_transition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ORGANIZED CREDITOR CLASS (TANGLED ROPE) — Large institutions (pension funds, insurers, central banks) have mobile options: commodity hedges, foreign currency holdings, derivative strategies. Benefit from fiat coordination (credit expansion) but also extract through financial engineering and inflation-premium pricing. d≈0.40, f(d)≈0.40, σ=1.0 → χ≈0.15. Low effective extraction; sophisticated agents can arbitrage.
constraint_indexing:constraint_classification(monetary_regime_transition, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: GOLD STANDARD LEGACY (PITON) — Fiat regime includes vestigial gold commitments (historical anchors, reserve requirements, 'sound money' rhetoric) that are largely performative. Central banks hold gold reserves not because of genuine commodity backing but because inertia and theater maintain the appearance of constraint. theater_ratio=0.58 reflects that much monetary policy communication invokes stability/prudence rhetoric while actual policy is unconstrained fiat creation. d≈0.12, f(d)≈-0.05, σ=1.0 → χ≈-0.02.
constraint_indexing:constraint_classification(monetary_regime_transition, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: DIGITAL CURRENCY REFORMERS (SCAFFOLD) — Organized agents (crypto advocates, CBDC researchers, alternative money systems) view fiat regime as a temporary coordination failure with emerging alternatives (blockchain, stablecoins, decentralized finance). See the constraint as having a sunset: programmable digital currencies and decentralized settlement could replace centralized fiat issuance. d≈0.35, f(d)≈0.35, σ=1.2 → χ≈0.16. Low extraction because reformers see an exit path and are building alternatives.
constraint_indexing:constraint_classification(monetary_regime_transition, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, fiat currency regimes may appear as an immutable feature of modern monetary systems: governments must issue currency, markets require a medium of exchange, inflation is inherent to economic growth. This perspective naturalizes the fiat regime as a law of modern economics. However, the structural data (ε=0.38, suppression=0.65, theater=0.58) contradicts the mountain classification — the engine will compute this as a false summit, revealing that the fiat regime is a contingent institutional arrangement, not a natural law.
constraint_indexing:constraint_classification(monetary_regime_transition, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_regime_transition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(monetary_regime_transition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(monetary_regime_transition, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(monetary_regime_transition, TR),
    TR >= 0.70.

:- end_tests(monetary_regime_transition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The fiat regime extracts real value through seigniorage (central bank profits from money creation), inflation tax (savers' purchasing power erosion), and first-issuer advantage (early borrowers benefit from low real rates before inflation). However, extraction is not as severe as historical commodity-based extraction systems — fiat currency enables genuine coordination benefits (credit expansion, countercyclical policy, deficit financing for public investment). The moderate value reflects the mixed nature of the regime. Suppression (0.65): Moderate-high. Significant barriers to exit from fiat currency include capital controls (suppressing foreign currency holdings), regulatory restrictions on alternative currencies (cryptocurrency restrictions, commodity restrictions), financial repression (negative real interest rates), and narrative dominance (fiat presented as natural, not contingent). But suppression is not absolute — wealthy agents can access foreign currencies, crypto, and commodity hedges; organized actors can use derivatives for inflation hedging. Theater ratio (0.58): Moderate. Central bank communication emphasizes rule-based policy, inflation targets, and prudent management, creating theatrical appearance of constraint. But the reality is discretionary policy responsive to fiscal pressure and political cycles. The theater has increased over time as central banks have adopted increasingly elaborate communication frameworks (forward guidance, macroprudential tools) that create appearance of complexity and constraint while maintaining discretionary authority.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates remarkable perspectival divergence. Wage earners and savers see pure Snare — trapped, extractive, with no exit option and declining real value. The monetary authority sees pure Rope — a coordination mechanism enabling flexible policy and growth. Debtors see Tangled Rope — inflation reduces their real burden (benefit) but lenders captured early advantages (extraction). The organized creditor class sees low-extraction Tangled Rope — they have hedging tools and can arbitrage. Gold standard adherents see Piton — vestigial commitments (reserve requirements, rhetoric) maintained through inertia despite loss of function. Digital currency reformers see Scaffold — a temporary coordination failure being replaced by programmable alternatives. The analytical observer risks seeing Mountain — fiat currency as an inherent feature of modern economies — but the structural data reveals this as a false summit. The perspectival gap is driven by differential exit options: trapped agents see Snare; mobile agents see Rope or Tangled Rope; organized agents see Scaffold.
 *
 * DIRECTIONALITY LOGIC:
 *   Wage earners: Victims + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. Savers: Victims + trapped → d≈0.95, f(d)≈1.42. Maximum extraction. Monetary authority: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.09. Net beneficiary. First issuers: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.08. Net beneficiary. Debtor class: Mixed (beneficiary of inflation reduction of nominal debt, victim of originating lender advantage) + constrained → d≈0.55, f(d)≈0.75. Mixed extraction. Organized creditor class: Partial beneficiary (profit from financial engineering) + mobile → d≈0.40, f(d)≈0.40. Low effective extraction due to hedging capacity. Digital reformers: Organized + constrained → d≈0.35, f(d)≈0.35. Low effective extraction; coalition has agency. The directionality chain reveals how the fiat regime's distributional characteristics follow from the differential exit options of trapped vs mobile agents.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The fiat currency regime is classified as Tangled Rope at the base level (ε=0.38, suppression=0.65, requires_active_enforcement=true, beneficiaries=[monetary_authority, first_issuers, debt_holders], victims=[wage_earners, savers, inflation_displaced]), meaning it combines genuine coordination benefits (credit expansion, policy flexibility, countercyclical intervention) WITH asymmetric extraction (seigniorage, inflation tax, first-issuer advantage). The mandatrophy is resolved by recognizing that fiat currency serves two structural functions simultaneously: (1) enabling credit-based growth and monetary policy coordination, which is genuinely beneficial for collective investment and macroeconomic stabilization; and (2) concentrating distributional gains among issuers and early borrowers while imposing costs on savers and wage earners. These are not contradictory — the regime is both coordination AND extraction. The perspectival divergence (victims see Snare; beneficiaries see Rope) arises from different structural positions, not from measurement error. Mandatrophy is resolved by showing that the regime's legitimacy depends on whether the coordination benefits exceed the extraction costs — an empirical question about growth, employment, and investment outcomes relative to alternative monetary systems, not a classification question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inflation_incidence_threshold,
    'What inflation rate threshold transforms fiat currency from a coordination mechanism into pure extraction?',
    'Empirical tracking of real wage growth, savers'' purchasing power erosion, and debtor benefit across inflation ranges; correlation between inflation trajectory and distributional outcomes',
    'If threshold < 3% annually: fiat regime appears as pure extraction (Snare from all victim perspectives). If threshold > 8% annually: high inflation normalized as coordination cost. Threshold defines the boundary between regime legitimacy and predation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inflation_incidence_threshold, empirical, 'Inflation threshold distinguishing coordination from extraction').

omega_variable(
    central_bank_independence,
    'Is central bank independence a genuine constraint on monetary expansion or a theatrical ritual masking fiscal dominance?',
    'Analysis of de facto monetary policy targeting vs de jure mandates; tracking correlation between fiscal deficits and monetary accommodation; cross-national comparison of inflation outcomes between nominally independent and fiscally dominated central banks',
    'If independence is genuine: suppression gate should be lower (~0.45), reclassifying some perspectives as Rope. If independence is theater: suppression remains high (~0.70), confirming Snare classification for trapped agents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(central_bank_independence, empirical, 'Whether central bank independence constrains monetary expansion').

omega_variable(
    alternative_monetary_viability,
    'Can decentralized or commodity-backed alternatives genuinely replace fiat currency regimes at scale, or does the network effect lock in fiat dominance?',
    'Empirical testing of alternative monetary systems (Bitcoin, CBDC prototypes, commodity-backed stablecoins); analysis of transaction costs, volatility, adoption barriers, and regulatory capture across alternative systems',
    'If alternatives are viable: scaffold perspective is structurally grounded — sunset is real and testable. If alternatives fail: scaffold is aspirational theater, and fiat regime persists through structural lock-in, not legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_monetary_viability, empirical, 'Whether viable alternatives to fiat currency exist').

omega_variable(
    distributional_inevitability,
    'Are the distributional asymmetries of fiat currency (early-issuer advantage, inflation tax on savers) inherent to any monetary regime or contingent on specific policy choices?',
    'Comparative institutional analysis of monetary regimes with explicit redistribution mechanisms (negative inflation, universal income offsetting inflation tax, debt jubilees); historical analysis of commodity-backed systems'' distributional outcomes',
    'If asymmetries are inevitable: fiat regime approaches a Mountain classification despite high extraction — the regime is extractive by necessity, not design. If asymmetries are contingent: fiat regime is a Tangled Rope made worse by policy choices that could be reversed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(distributional_inevitability, conceptual, 'Whether fiat regime distributional asymmetries are inevitable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_regime_transition, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(monet_tr_t0, monetary_regime_transition, theater_ratio, 0, 0.35).
narrative_ontology:measurement(monet_tr_t50, monetary_regime_transition, theater_ratio, 50, 0.5).
narrative_ontology:measurement(monet_tr_t100, monetary_regime_transition, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(monet_be_t0, monetary_regime_transition, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(monet_be_t50, monetary_regime_transition, base_extractiveness, 50, 0.3).
narrative_ontology:measurement(monet_be_t100, monetary_regime_transition, base_extractiveness, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_regime_transition, information_standard).
narrative_ontology:affects_constraint(monetary_regime_transition, inflation_expectations).
narrative_ontology:affects_constraint(monetary_regime_transition, debt_overhang).
narrative_ontology:affects_constraint(monetary_regime_transition, financial_repression).
narrative_ontology:affects_constraint(monetary_regime_transition, currency_hierarchy).

% DUAL FORMULATION NOTE:
% The sovereign fiat currency regime decomposes into multiple downstream constraints: (1) inflation expectations formation (coordination problem in belief alignment), (2) debt overhang (extraction through nominal creditor advantage), (3) financial repression (suppression of real returns to savers), (4) currency hierarchy (global reserve currency dominance by fiat regimes). The base constraint (monetary_regime_transition) models the structural properties of the regime itself; the downstream constraints model specific mechanisms through which extraction occurs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(monetary_regime_transition, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
