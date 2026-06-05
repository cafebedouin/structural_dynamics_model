% ============================================================================
% CONSTRAINT STORY: monetary_discretion_expansion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_discretion_expansion, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: monetary_discretion_expansion
 *   human_readable: Monetary Discretion Expansion: Gold-Backed to Fiat Currency Transition
 *   domain: monetary_economics/political_economy/institutional_history
 *
 * SUMMARY:
 *   The transition from gold-backed to fiat currency systems represents a
 *   fundamental institutional shift in monetary authority structure. Between
 *   the Bretton Woods agreement (1944) and its collapse (1971), and
 *   continuing through the present, central banks gained discretionary
 *   control over money supply previously constrained by commodity backing.
 *   This story examines whether that transition constitutes a single
 *   constraint (monetary discretion expansion) viewed from different
 *   structural positions, or whether it decomposes into multiple constraints
 *   per the ε-invariance principle. The base extractiveness (0.58) reflects
 *   that the constraint enables substantial extraction through inflation
 *   erosion, seigniorage capture, and debt monetization. However, the
 *   constraint also enables genuine coordination functions: counter-cyclical
 *   policy, crisis response, and macroeconomic stabilization that
 *   gold-backing prohibited. The theater_ratio (0.68) captures the high
 *   performative content of inflation-targeting frameworks, which legitimate
 *   monetary discretion through institutional theater (targets, independence
 *   doctrines, forward guidance) while actual discretion persists. The
 *   suppression level (0.62) reflects that exit barriers are very high:
 *   currency holders cannot easily opt out of national monetary systems,
 *   alternative currency adoption faces legal and network-effect barriers,
 *   and capital controls limit currency substitution. The measurement
 *   trajectory shows extraction accumulation from 1944-1984, stabilization
 *   with inflation-targeting regimes (1984-2004), and persistence at elevated
 *   levels despite institutional legitimation (2004-2024). This pattern is
 *   diagnostic of a tangled_rope: genuine coordination function (monetary
 *   policy flexibility) layered with asymmetric extraction (inflation
 *   erosion, seigniorage capture).
 *
 * KEY AGENTS:
 *   - Wage Earners: Primary victims (powerless/trapped) — face purchasing power erosion through inflation; no exit from national currency systems
 *   - Savers: Primary victims (powerless/trapped) — real returns on savings become negative; trapped in depreciating domestic currency by capital controls and transaction costs
 *   - Monetary Authorities: Primary beneficiaries (institutional/arbitrage) — gain discretionary power and policy flexibility; arbitrage exit exists but is politically costly
 *   - Financial Sector: Secondary beneficiaries (organized/constrained) — benefit from money creation privilege and leverage opportunities; constrained by regulations and network effects
 *   - Sovereign Debt Issuers: Secondary beneficiaries (powerful/mobile) — can monetize deficits and conduct seigniorage; mobile exit exists but is economically costly
 *   - Inflation-Targeting Coalition: Organized agents (organized/constrained) — central banks, economists, institutions building institutional constraints on discretion; constrained by political pressure and technical limits
 *   - Gold-Standard Memory: Institutional narrative (institutional/arbitrage) — rhetoric of sound money serves legitimation function; arbitrage exit exists but lacks political will
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional design as inherent to fiat money systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_discretion_expansion, 0.58).
domain_priors:suppression_score(monetary_discretion_expansion, 0.62).
domain_priors:theater_ratio(monetary_discretion_expansion, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_discretion_expansion, extractiveness, 0.58).
narrative_ontology:constraint_metric(monetary_discretion_expansion, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(monetary_discretion_expansion, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_discretion_expansion, tangled_rope).
narrative_ontology:human_readable(monetary_discretion_expansion, "Monetary Discretion Expansion: Gold-Backed to Fiat Currency Transition").
narrative_ontology:topic_domain(monetary_discretion_expansion, "monetary_economics/political_economy/institutional_history").

domain_priors:requires_active_enforcement(monetary_discretion_expansion).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(monetary_discretion_expansion, formalized).
narrative_ontology:cs_authority_grounding(monetary_discretion_expansion, extraction).
narrative_ontology:cs_interpretation_layer_present(monetary_discretion_expansion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_discretion_expansion, monetary_authorities).
narrative_ontology:constraint_beneficiary(monetary_discretion_expansion, sovereign_debt_issuers).
narrative_ontology:constraint_beneficiary(monetary_discretion_expansion, financial_sector).
narrative_ontology:constraint_victim(monetary_discretion_expansion, currency_holders).
narrative_ontology:constraint_victim(monetary_discretion_expansion, wage_earners).
narrative_ontology:constraint_victim(monetary_discretion_expansion, savers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WAGE EARNER (SNARE) — Trapped in national currency; cannot opt out of monetary system. Bears full cost of discretionary expansion through inflation erosion of purchasing power. No exit: alternative currencies are informal/illegal, barter unfeasible at scale, relocation creates new entrapment. Maximum extraction from this position — the constraint is experienced as pure coercion.
constraint_indexing:constraint_classification(monetary_discretion_expansion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SAVERS (SNARE) — Trapped in currency holdings; institutional barriers prevent foreign asset accumulation for most. Real return on savings becomes negative under discretionary expansion. No exit mechanism — international capital controls, transaction costs, and currency speculation barriers keep savings locked in depreciating domestic currency. Extraction mechanism: time-preference taxation through inflation.
constraint_indexing:constraint_classification(monetary_discretion_expansion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MONETARY AUTHORITY (ROPE) — Experiences discretion expansion as solving a genuine coordination problem: gold-backing constrains emergency response, cyclical stimulus, and counter-cyclical policy. The authority genuinely benefits from the coordination function — fiat enables macroeconomic stabilization that gold-backing prohibited. Sees constraint as a tool, not an extraction mechanism. Has arbitrage exit: can return to gold standard if political will exists (historically attempted; failed).
constraint_indexing:constraint_classification(monetary_discretion_expansion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FINANCIAL SECTOR (TANGLED ROPE) — Massive beneficiary from discretion expansion: enables debt-driven business models, interest-rate arbitrage, and leverage cycling. But also constrained by regulatory requirements, reserve ratios, and periodic crises. Benefits from coordination function (stable payment system) AND extracts via money creation privilege. Constrained exit: could theoretically exit via cryptocurrency or parallel systems, but regulatory barriers and network effects keep them locked into central bank relationships.
constraint_indexing:constraint_classification(monetary_discretion_expansion, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SOVEREIGN DEBT ISSUERS (TANGLED ROPE) — States benefit from fiat's ability to monetize deficits and conduct seigniorage. Genuine coordination function: enables crisis response and counter-cyclical spending. But also subject to inflation discipline and capital flight when discretion is perceived as excessive. Mobile exit: states can theoretically adopt hard currency, dollarize, or return to commodity backing (rare and costly). Experience both coordination benefit and extraction — the constraint enables spending but contains debt costs.
constraint_indexing:constraint_classification(monetary_discretion_expansion, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: INFLATION-TARGETING COALITION (SCAFFOLD) — Central banks, economists, and international institutions (IMF, BIS) have built an overlay of institutional constraints: inflation targets (2%), independence doctrines, forward guidance, and transparency frameworks. These create a sunset on discretionary expansion — the constraint's extractive capacity is formally bounded. Theater is high (inflation targeting is partly performative), but the constraint is temporary by design. As credibility builds, real inflation stays near target — the extraction mechanism loses force. Scaffold logic: temporary coordination failure being solved by institutional innovation.
constraint_indexing:constraint_classification(monetary_discretion_expansion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: GOLD STANDARD MEMORY (PITON) — The rhetoric of sound money, commodity backing, and monetary discipline persists as a political counterfactual despite functional obsolescence. Gold-standard nostalgia serves theater: appeals to fiscal conservatism without requiring actual return to commodity constraint. The constraint is maintained through inertia and narrative rather than functional necessity — central bankers argue fiat requires strict governance precisely because it lacks gold's mechanical discipline. The piton classification reflects that gold-standard memory is largely performative, maintained to justify monetary independence ideology.
constraint_indexing:constraint_classification(monetary_discretion_expansion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational horizon, some degree of monetary discretion is inherent to any fiat system: you cannot have a medium of exchange without some issuer, and issuers will always face temptation to expand. The constraint appears as an immutable feature of fiat money itself — the inherent tradeoff between the flexibility fiat enables and the inflation risk it creates. However, the structural data contradicts the mountain classification: identifiable beneficiaries (monetary authorities, financial sector), victim groups (wage earners, savers), and institutional design choices (inflation targets, independence doctrines) reveal this as a false summit — what appears 'inherent to fiat' is actually a contingent institutional arrangement that varies across jurisdictions and time periods.
constraint_indexing:constraint_classification(monetary_discretion_expansion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_discretion_expansion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(monetary_discretion_expansion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(monetary_discretion_expansion, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(monetary_discretion_expansion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(monetary_discretion_expansion, TR),
    TR >= 0.70.

:- end_tests(monetary_discretion_expansion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint enables clear extraction mechanisms: inflation erosion of wage and savings value; seigniorage capture by monetary authorities; debt-financed spending that defers costs to future inflation. However, extractiveness is not maximal (would require ε ≥ 0.7) because genuine coordination functions exist: counter-cyclical policy prevents deflationary crises, macroeconomic stabilization enables investment and employment, and monetary flexibility provides crisis response capacity that gold-backing prohibited. The trajectory from 0.15 (1944, early Bretton Woods with gold peg) to 0.58 (present) reflects discretion accumulation over decades. The modest decline from 0.58 to 0.52 (1984-2004) reflects inflation-targeting framework adoption, but extraction bounces back to 0.58 (2004-2024) through quantitative easing and zero-bound monetary policy. Suppression (0.62): Moderate-high. Exit barriers include legal prohibition on private money creation, capital controls in some jurisdictions, regulatory barriers to alternative currency use, and network effects that lock economic activity into dominant currency. Transaction costs of currency substitution are substantial. However, suppression is not total (would require ≥ 0.80) because some agents (wealthy individuals, firms with foreign revenue, countries capable of dollarization) have partial exit options through capital flight, foreign currency holdings, or alternative currency adoption. Theater Ratio (0.68): High. Inflation-targeting frameworks, central bank independence doctrines, forward guidance, and transparency reports create substantial performative content around monetary discretion. The theater has increased from 1944 (0.25, simple gold-peg rule) through 1984 (0.58, inflation targeting begins) to present (0.68, extensive communication and credibility-building institutions). The theater's function is legitimation: institutional innovations provide credibility that the discretion is bounded, even when actual discretion persists. This is classic piton mechanism: performance without full functional constraint.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap here is between institutional legitimacy narratives and structural reality. The monetary authority genuinely experiences the constraint as solving a coordination problem (rope): fiat enables macroeconomic stabilization and crisis response. From their perspective, the constraint is beneficial and necessary. The wage earner experiences pure extraction (snare): inflation erosion of purchasing power with no exit option. From their perspective, the constraint is coercive and unavoidable. The inflation-targeting coalition sees a temporary problem being solved (scaffold): institutional innovations (targets, independence, transparency) are building boundaries on discretion that will eventually sunset the extraction mechanism as credibility builds. The gold-standard memory maintains the rhetoric of an alternative regime (piton): the narrative of commodity backing persists despite functional obsolescence, serving to legitimize monetary independence by contrast. The analytical observer from a civilizational horizon risks collapsing into a mountain classification: 'fiat money inherently requires discretion, which inherently produces inflation risk.' This naturalizes what is actually a set of institutional choices that vary across time and place. Sweden had near-zero inflation with fiat; Argentina had episodes of 300%+ inflation with fiat. The constraint is not inherent to fiat but to specific institutional designs within fiat.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from structural position in the extraction flow. Wage earners and savers occupy maximum-victim positions: they bear purchasing power erosion with no exit option. The sigmoid function maps (powerless, trapped) → high d → high f(d) → high experienced chi. Monetary authorities occupy beneficiary position: they gain policy flexibility. (Institutional, arbitrage) → low d → low f(d) → negative chi (experienced as benefit rather than extraction). Financial sector is mixed: they benefit from money creation and leverage privileges but are constrained by regulations. (Organized, constrained) → moderate d → moderate f(d) → moderate chi. The analytical observer's position demonstrates the false summit: they see the constraint as inherent to fiat money (mountain perspective), but the structural data reveals identifiable beneficiaries and victims, demonstrating that the 'inevitability' is actually a designed institutional arrangement. The directional gap between beneficiaries (rope perspective) and victims (snare perspective) reveals the extraction asymmetry that the analytical observer's mountain perspective obscures.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves mandatrophy by decomposing the apparent conflict between 'monetary discretion is necessary for macroeconomic management' and 'monetary discretion enables extraction through inflation.' Both claims are structurally true. The coordination function is real: discretion enables counter-cyclical policy, crisis response, and growth. The extraction mechanism is real: discretion enables inflation erosion, seigniorage capture, and debt monetization. The constraint is tangled_rope because it genuinely serves both functions simultaneously. The perspectival gap between beneficiaries (who emphasize coordination) and victims (who emphasize extraction) is not a misunderstanding — it reflects that the same institutional mechanism produces both effects. The false summit risk arises when analysts naturalize the institutional design as inevitable: 'fiat money inherently requires discretion, which inherently produces inflation.' This obscures the engineering choices that shape discretion: inflation targeting, central bank independence doctrines, forward guidance, and transparency frameworks are deliberate institutional innovations that change the constraint's extractiveness profile without returning to gold-backing. The constraint is therefore best understood not as an inherent property of fiat but as an institutional arrangement whose extractiveness depends on how discretion is governed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fiat_necessity_vs_discretion_design,
    'Is monetary discretion expansion an inherent property of fiat systems or a contingent institutional design choice?',
    'Cross-national comparison: jurisdictions with strict inflation targeting vs those with discretionary central banks; historical analysis of discretion expansion timing relative to gold-backing abandonment; counterfactual modeling of fiat systems with hard constraints (cryptocurrency protocols as natural experiment)',
    'If inherent: mountain classification correct, false summit is misdiagnosis. If contingent: tangled_rope classification correct, system design explains extraction, alternative designs could reduce extraction without returning to gold backing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fiat_necessity_vs_discretion_design, conceptual, 'Whether discretion is inherent to fiat or a contingent institutional design choice').

omega_variable(
    inflation_targeting_effectiveness,
    'Do inflation-targeting frameworks actually constrain discretionary expansion, or are they theater that legitimizes discretion without limiting it?',
    'Time-series analysis of actual vs target inflation across regimes; causal analysis of whether target violations produce institutional consequences; comparison of pre- and post-targeting discretion levels; central bank reaction function analysis',
    'If effective: scaffold perspective is correct, extractive capacity is actually bounded, theater_ratio overestimated. If theater: scaffold is aspirational rather than structural, extraction persists behind institutional legitimacy facade.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inflation_targeting_effectiveness, empirical, 'Whether inflation targeting effectively constrains discretionary expansion').

omega_variable(
    seigniorage_distribution_ambiguity,
    'Is seigniorage (monetary authority''s profit from money creation) a legitimate coordination cost or an extraction mechanism?',
    'Historical accounting of seigniorage flows; analysis of whether seigniorage funds public goods (infrastructure, crisis response) or private enrichment (central bank balance sheet expansion); comparison of seigniorage distribution across regimes',
    'If coordination cost: beneficiary perspective justified, tangled rope confirmed. If extraction: victims'' perspective emphasized, snare classification gains force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(seigniorage_distribution_ambiguity, empirical, 'Whether seigniorage is coordination cost or extraction mechanism').

omega_variable(
    alternative_currency_sufficiency,
    'Do alternative currencies (Bitcoin, gold, foreign currency) provide genuine exit for trapped agents or are network effects and regulatory barriers insurmountable?',
    'Empirical data on actual currency substitution rates; legal barriers to alternative currency use; network-effect analysis of payment system switching costs; historical cases of currency competition or replacement',
    'If genuine exit possible: trapped classification is overstated, should be constrained or mobile. If barriers insurmountable: trapped classification confirmed, extraction is inescapable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_currency_sufficiency, empirical, 'Whether alternative currencies provide genuine exit from fiat money trap').

omega_variable(
    gold_standard_counterfactual,
    'Would return to gold-backing reduce extractiveness or merely exchange monetary discretion for commodity-price shocks and deflationary crises?',
    'Historical analysis of gold-standard era crises and growth costs; theoretical modeling of gold constraint effects; comparison of volatility and inequality across regimes; analysis of why gold-backing was abandoned (policy choice vs structural collapse)',
    'If gold-backing was superior: current system is pure extraction (snare classification gains force). If gold-backing had comparable costs: discretion expansion is a Pareto improvement (rope or even mountain classification possible).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gold_standard_counterfactual, conceptual, 'Whether gold-backing would reduce or merely redirect extraction costs').

omega_variable(
    multiple_constraints_or_single_institution,
    'Is ''monetary discretion expansion'' one constraint or a composite of multiple structurally distinct constraints that ε-invariance requires decomposing?',
    'Measurement via different observables: (a) inflation erosion of purchasing power; (b) seigniorage extraction; (c) debt-financed spending capacity; (d) counter-cyclical policy enablement. If these give different ε values, constraint family decomposition is required. If they all measure the same extraction mechanism, single constraint is valid.',
    'If decomposition required: write separate stories for inflation-mechanism, seigniorage-mechanism, debt-financing-mechanism, and policy-enablement-mechanism. Each gets its own ε, beneficiary/victim structure, and network links. If single constraint: current approach is valid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multiple_constraints_or_single_institution, conceptual, 'Whether ''monetary discretion expansion'' is one constraint or requires ε-invariance decomposition').

omega_variable(
    kernel_reading_differentiation,
    'Does the ''reading'' of what changed between gold-backing and fiat (monetary authority gains power vs coordination mechanism expands vs extraction mechanism opens) constitute different constraint classifications or perspectival variations of one constraint?',
    'If readings emit structurally different beneficiary/victim sets, different ε values, or incompatible perspectives: kernel reading framework applies, multiple constraint stories required. If readings produce same structural data viewed from different power levels: single constraint with perspectival variation.',
    'If kernel readings: separate JSON files for ''discretion-as-monetary-freedom'' reading, ''discretion-as-extraction'' reading, ''discretion-as-coordination'' reading, linked via network. If perspectival variation: current single story approach valid.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_differentiation, conceptual, 'Whether different readings of the gold-standard-to-fiat transition constitute different constraints or perspectival views').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_discretion_expansion, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(monetdisc_theater_1944, monetary_discretion_expansion, theater_ratio, 0, 0.25).
narrative_ontology:measurement(monetdisc_theater_1964, monetary_discretion_expansion, theater_ratio, 20, 0.4).
narrative_ontology:measurement(monetdisc_theater_1984, monetary_discretion_expansion, theater_ratio, 40, 0.58).
narrative_ontology:measurement(monetdisc_theater_2004, monetary_discretion_expansion, theater_ratio, 60, 0.68).
narrative_ontology:measurement(monetdisc_theater_2024, monetary_discretion_expansion, theater_ratio, 80, 0.68).

% Extraction over time
narrative_ontology:measurement(monetdisc_extractiveness_1944, monetary_discretion_expansion, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(monetdisc_extractiveness_1964, monetary_discretion_expansion, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(monetdisc_extractiveness_1984, monetary_discretion_expansion, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(monetdisc_extractiveness_2004, monetary_discretion_expansion, base_extractiveness, 60, 0.52).
narrative_ontology:measurement(monetdisc_extractiveness_2024, monetary_discretion_expansion, base_extractiveness, 80, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_discretion_expansion, resource_allocation).
narrative_ontology:affects_constraint(monetary_discretion_expansion, inflation_erosion_wage_dynamics).
narrative_ontology:affects_constraint(monetary_discretion_expansion, sovereign_debt_accumulation).
narrative_ontology:affects_constraint(monetary_discretion_expansion, financial_leverage_cycles).

% DUAL FORMULATION NOTE:
% Monetary discretion expansion may decompose into multiple constraints per ε-invariance principle: (1) inflation-mechanism (ε≈0.45, purchasing power erosion); (2) seigniorage-mechanism (ε≈0.30, money creation privilege); (3) debt-financing-mechanism (ε≈0.35, deficit monetization); (4) policy-enablement-mechanism (ε≈0.05, coordination function). Current story integrates all four into single tangled_rope. If measurements show different ε via different observables, decomposition into constraint family is required.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(monetary_discretion_expansion, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
