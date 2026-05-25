% ============================================================================
% CONSTRAINT STORY: monetary_control_centralization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_control_centralization, []).

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
 *   constraint_id: monetary_control_centralization
 *   human_readable: Monetary Control Centralization
 *   domain: political_economy/monetary_systems
 *
 * SUMMARY:
 *   Monetary control centralization establishes the structural preconditions
 *   for modern economic coordination while simultaneously creating the
 *   primary mechanism for asymmetric wealth extraction and financial
 *   exclusion. This constraint exhibits the full perspectival range: unbanked
 *   populations experience it as a snare (financial exclusion with no exit);
 *   small merchants experience it as tangled rope (genuine coordination
 *   benefit alongside extraction via fees and access barriers); central
 *   banking institutions experience it as rope (pure coordination with
 *   institutional advantage); organized decentralized finance movements
 *   experience it as scaffold (temporary constraint being sunset by
 *   technological alternatives); degraded gold standard institutions
 *   experience it as piton (ritual maintenance without function); and from a
 *   universal analytical stance it risks appearing as mountain (immutable
 *   coordination necessity) when it is actually a contingent institutional
 *   choice. The constraint's theater ratio has increased over the interval as
 *   monetary policy has shifted from rules-based commodity backing toward
 *   discretionary fiat management, increasing the performative content of
 *   monetary authority while reducing technical constraint on monetary
 *   quantity. Extractiveness has grown as financial system complexity has
 *   increased access barriers, and as the reserve currency system has
 *   concentrated monetary control.
 *
 * KEY AGENTS:
 *   - Central Banking Institutions: Primary beneficiary (institutional/arbitrage) — control monetary base, set policy, capture seigniorage, control payment rails
 *   - Large Financial Corporations: Co-beneficiary (institutional/arbitrage) — access to central bank liquidity facilities, preferential payment processing, regulatory arbitrage
 *   - Government Fiscal Authorities: Co-beneficiary (institutional/constrained) — access to monetary financing and inflation tax, but also constrained by central bank independence
 *   - Small Merchants and Producers: Secondary victim (moderate/constrained) — bear merchant processing fees, credit access barriers, working capital constraints
 *   - Unbanked Populations: Primary victim (powerless/trapped) — excluded from formal financial system, forced into shadow economies and exploitative alternatives
 *   - Sovereign Nations Without Reserve Currency: Victim group (powerful/constrained) — constrained by foreign monetary policy, vulnerable to exchange rate dynamics and capital flight
 *   - Decentralized Finance Movements: Organized agents (organized/constrained) — building alternative coordination systems, scaffold perspective with sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as immutable laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_control_centralization, 0.58).
domain_priors:suppression_score(monetary_control_centralization, 0.65).
domain_priors:theater_ratio(monetary_control_centralization, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_control_centralization, extractiveness, 0.58).
narrative_ontology:constraint_metric(monetary_control_centralization, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(monetary_control_centralization, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_control_centralization, tangled_rope).
narrative_ontology:human_readable(monetary_control_centralization, "Monetary Control Centralization").
narrative_ontology:topic_domain(monetary_control_centralization, "political_economy/monetary_systems").

domain_priors:requires_active_enforcement(monetary_control_centralization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_control_centralization, central_banking_institutions).
narrative_ontology:constraint_beneficiary(monetary_control_centralization, large_financial_corporations).
narrative_ontology:constraint_beneficiary(monetary_control_centralization, government_fiscal_authorities).
narrative_ontology:constraint_victim(monetary_control_centralization, small_economic_agents).
narrative_ontology:constraint_victim(monetary_control_centralization, alternative_currency_systems).
narrative_ontology:constraint_victim(monetary_control_centralization, unbanked_populations).
narrative_ontology:constraint_victim(monetary_control_centralization, monetary_sovereignty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNBANKED POPULATIONS (SNARE) — Structurally trapped without access to formal financial systems. Forced into parallel shadow economies or exploitative informal credit markets. No meaningful exit option from monetary centralization except geographic relocation. Maximum extraction experienced as financial exclusion and predatory alternatives.
constraint_indexing:constraint_classification(monetary_control_centralization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL MERCHANTS AND PRODUCERS (TANGLED ROPE) — Structurally constrained by payment rails they do not control. Genuine coordination function exists (standardized money enables trade), but asymmetric extraction runs through banking fees, merchant processing costs, and credit access barriers. Exit is theoretically possible (barter, local currencies, cryptocurrency) but high practical cost — losing access to broader markets, social stigma, regulatory risk.
constraint_indexing:constraint_classification(monetary_control_centralization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CENTRAL BANKING SYSTEM (ROPE) — Experiences monetary centralization as a pure coordination mechanism. The constraint solves the classical double-coincidence-of-wants problem and enables monetary transmission. Beneficiary position with arbitrage exit (can adjust policy, modify system design, shift between fiat/commodity regimes). Extracts institutional power and seigniorage but frames this as legitimate coordination cost.
constraint_indexing:constraint_classification(monetary_control_centralization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SOVEREIGN NATIONS WITHOUT RESERVE CURRENCY (TANGLED ROPE) — Benefit from stable international payment systems and capital markets, but constrained by dependence on foreign central banks' monetary policy. Extraction flows through exchange rate manipulation, capital flight vulnerability, and monetary policy externalities. Exit theoretically possible (alternative payment systems, regional currency unions) but constrained by economic size and network effects. Organized collective action (BRICS, de-dollarization) represents constrained-exit mobility.
constraint_indexing:constraint_classification(monetary_control_centralization, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: DECENTRALIZED FINANCE AND CRYPTOCURRENCY MOVEMENTS (SCAFFOLD) — Organized agents (protocol developers, exchanges, user communities) building alternative monetary coordination systems outside central banking. See centralization constraint as temporary coordination failure being sunset by blockchain infrastructure, peer-to-peer networks, and stablecoin alternatives. Theater ratio lower than traditional finance — technical verification is distributed and transparent. Sunset logic: as alternative systems mature, centralized control's extraction mechanism loses force.
constraint_indexing:constraint_classification(monetary_control_centralization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: GOLD STANDARD INSTITUTIONAL VESTIGES (PITON) — Central bank reserve practices, currency backing rhetoric, and international monetary agreements retain vestigial structures from the gold standard era. These practices persist through institutional inertia despite having largely lost their original coordination function. Theater ratio high — the gold holdings and reserve ratios are performatively maintained as confidence mechanisms long after their actual constraint on monetary policy weakened. The constraint sustains itself through ritual rather than functional necessity.
constraint_indexing:constraint_classification(monetary_control_centralization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal civilizational perspective, some monetary centralization appears immutable: the coordination problem of establishing common value measure requires some authority or mechanism to anchor expectations, prevent unlimited quantity increases, and resolve disputes. This perspective risks naturalizing what is actually a contingent institutional choice. The engine's false summit detector will reveal whether this reflects structural necessity or naturalization of power concentration.
constraint_indexing:constraint_classification(monetary_control_centralization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_control_centralization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(monetary_control_centralization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(monetary_control_centralization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(monetary_control_centralization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(monetary_control_centralization, TR),
    TR >= 0.70.

:- end_tests(monetary_control_centralization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and increasing. The constraint extracts through multiple mechanisms: seigniorage flows to central banks (~2-3% of broad money creation annually), merchant processing fees (~0.5-2% of transaction value), credit access restrictions that force small agents into capital markets at penalty rates, and at the international level, asymmetric benefits to reserve currency issuers. The extraction is not maximal because genuine coordination benefits exist — stable money solves real coordination problems — and because alternative systems are beginning to provide exit options. The 50-year trajectory (0.35→0.58) reflects increasing financial complexity and increasingly sophisticated capture of monetary policy by financial sector interests, combined with shrinking velocity of traditional coordination alternatives. Suppression (0.65): High. Structural barriers include legal tender laws enforcing fiat currency acceptance, regulatory prohibition on competing currencies (some jurisdictions), network effects making dollar/euro/yuan switching costly, capital controls preventing currency substitution, and knowledge barriers (technical complexity of cryptocurrency, financial literacy requirements). Suppression is not absolute because decentralized alternatives are technically available and legally tolerated in most jurisdictions — they simply face high adoption barriers. Theater ratio (0.58): Moderate and increasing. Monetary policy communication, inflation targeting, forward guidance, and quantitative easing programs all contain significant performative content — they signal commitment and manage expectations rather than mechanically adjusting the real money supply. The theater ratio increased as commodity backing (which provided external constraint) was replaced by purely discretionary fiat management. But theater is not dominant — real monetary effects occur (inflation, interest rates, asset price inflation), so the theater is layered on genuine structural function, not replacing it entirely. This moderate-high theater ratio combined with significant extraction and genuine coordination function is the defining signature of tangled rope.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The central banking institution's rope perspective sees coordinated monetary stability, interest rate transmission, and financial system soundness. The unbanked population's snare perspective sees complete exclusion and forced participation in exploitative informal markets. The small merchant's tangled rope perspective sees both coordination benefit (access to markets) and extraction (fees and barriers). The decentralized finance movement's scaffold perspective sees this as a temporary institutional coordination failure being solved by technology. The gold standard vestige's piton perspective sees degraded ritual — the constraint persists through habit rather than function. These perspectives emerge from different structural positions relative to the same constraint object, not from different interpretations of facts. The perspectival gap is not eliminable through better communication — it reflects real structural divergence of interests and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position relative to the constraint. Central banks benefit from the constraint (beneficiary status) with arbitrage exit options (can modify policy, adjust system design) → low d → negative effective extraction (they experience coordination, not extraction). Unbanked populations lack access (victim status) with trapped exit (no alternatives without massive relocation or legal risk) → high d → high chi → snare. Small merchants benefit from coordination (can trade nationally/internationally) but are trapped by extraction (merchant fees, credit barriers) → moderate d → moderate chi → tangled rope. Nations without reserve currency status benefit from international payment systems but are constrained by dependence on foreign monetary policy → moderate d shifting toward higher d → constrained exit creates tangled rope. Decentralized finance movements are organized with genuine exit options forming (alternative systems are technically viable) → organized power × constrained exit → scaffold classification. The piton perspective (gold standard vestiges) derives not from high d but from the theater gate: the constraint persists through institutional inertia despite reduced functional necessity. The mountain perspective at civilizational scale risks naturalizing what analysis reveals as contingent institutional choice — the false summit detector should flag this as unwarranted universalization.
 *
 * MANDATROPHY ANALYSIS:
 *   STRUCTURAL ANALYSIS: The mandatrophy is resolved by recognizing that monetary centralization serves genuine coordination function (enabling common value measure, preventing hyperinflation, settling disputes) while simultaneously enabling extraction (seigniorage capture, financial access control, reserve currency asymmetry). Both functions are real. The constraint is not 'actually just coordination' (rope classification from beneficiary view) nor 'actually just extraction' (snare classification from powerless view). It is simultaneously both, with the coordination-to-extraction ratio varying by perspective. The tangled rope classification from moderate and institutional perspectives with constrained exit captures this hybrid nature. The scaffold perspective is not alternative to snare/rope — it is a temporal statement: that the extraction mechanism will degrade over the next 25-50 years as decentralized alternatives mature, shifting the constraint from tangled rope toward rope or toward sunset entirely. The piton perspective (gold standard vestiges) identifies components of the constraint that have already lost their coordination function but persist through institutional inertia. From the analytical observer's position, the risk is false mountain — naturalizing contingent institutional arrangements (central bank monopoly on currency, legal tender laws, reserve currency regimes) as immutable laws of monetary physics. The structural data contradicts this: alternative systems exist and function; the centralization is maintained by regulation and network effects, not physical laws.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_control_boundary,
    'How much monetary centralization is necessary for coordination versus how much serves pure control and extraction?',
    'Comparative analysis of monetary systems with varying degrees of centralization (decentralized protocols, currency unions, national currencies, reserve currencies); measurement of coordination efficiency gains versus extraction costs at different centralization levels',
    'If coordination can be maintained at low centralization: constraint reclassifies as scaffold (temporary). If centralization is structurally necessary: constraint reclassifies as mountain or rope. If extraction dominates coordination benefit: constraint strengthens as snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_control_boundary, empirical, 'Boundary between necessary coordination and discretionary control').

omega_variable(
    exit_option_viability_threshold,
    'At what network adoption level do alternative monetary systems (cryptocurrency, CBDC alternatives, regional currency unions) become genuine exit options rather than constrained-exit alternatives?',
    'Tracking adoption curves of alternative systems; measurement of transaction volume, merchant acceptance, and price stability; analysis of regulatory barriers versus technical barriers to adoption',
    'If viable alternatives reach critical mass: many perspectives shift from snare/tangled_rope to scaffold/rope. If regulatory and network effects prevent adoption: constraint remains snare/tangled_rope with suppression near 1.0.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_viability_threshold, empirical, 'When alternative monetary systems become viable exits').

omega_variable(
    seigniorage_distribution_transparency,
    'Is seigniorage extraction by central banks transparent and subject to public scrutiny, or hidden within monetary policy operations?',
    'Analysis of central bank balance sheets, seigniorage accounting practices, and public discourse about monetary creation benefits. Comparison of explicit seigniorage remittance versus implicit extraction through inflation and financial system access control.',
    'If transparent: theater ratio decreases, constraint reclassifies toward rope or tangled_rope with clear beneficiary/victim structure. If hidden: theater ratio increases, constraint strengthens toward snare with opacity enabling suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(seigniorage_distribution_transparency, empirical, 'Transparency of seigniorage extraction').

omega_variable(
    alternative_coordination_sufficiency,
    'Do decentralized monetary systems (blockchain consensus, stablecoin collateral mechanisms, peer-to-peer networks) provide equivalent coordination functions to centralized monetary authority?',
    'Comparison of operational metrics: transaction finality time, double-spend prevention, price stability, cross-border settlement efficiency, dispute resolution effectiveness. Analysis of failure modes in decentralized versus centralized systems.',
    'If decentralized coordination is equivalent: scaffold sunset logic is validated, constraint has genuine temporal horizon. If decentralized coordination has structural deficits: centralization may be mountain-type necessity rather than contingent choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_coordination_sufficiency, empirical, 'Whether decentralized systems provide equivalent coordination').

omega_variable(
    reserve_currency_extraction_mechanism,
    'Does the dollar/euro/yuan reserve currency status constitute asymmetric extraction from non-reserve countries, or legitimate compensation for the coordination service provided?',
    'Measurement of seigniorage flows to reserve currency issuers; analysis of exchange rate volatility and capital flight patterns for non-reserve countries; comparison of real interest rates and inflation across reserve versus non-reserve currency regimes',
    'If extraction dominates: constraint operates as snare at international scope. If coordination benefit outweighs extraction: constraint operates as rope or tangled_rope with justified asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reserve_currency_extraction_mechanism, empirical, 'Reserve currency benefits versus extraction costs').

omega_variable(
    identity_lock_in_monetary_sovereignty,
    'Do nations with history of hyperinflation or currency collapse suffer from identity-lock into foreign monetary systems or IMF policy frameworks, preventing exit even when structurally possible?',
    'Analysis of policy reversals and reform attempts in post-hyperinflation countries; examination of whether political barriers (elite commitment to reserve currency regimes, ideological adoption of monetarism) exceed structural economic barriers',
    'If identity-locked: some institutional agents at moderate power level should classify as identity_locked rather than constrained, revealing cognitive capture rather than structural dependency. Changes perspectival gap and suggests alternative solutions (reframing) rather than structural reform.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_in_monetary_sovereignty, conceptual, 'Identity-lock into foreign monetary frameworks').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_control_centralization, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(monet_tr_t0, monetary_control_centralization, theater_ratio, 0, 0.4).
narrative_ontology:measurement(monet_tr_t25, monetary_control_centralization, theater_ratio, 25, 0.52).
narrative_ontology:measurement(monet_tr_t50, monetary_control_centralization, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(monet_be_t0, monetary_control_centralization, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(monet_be_t25, monetary_control_centralization, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(monet_be_t50, monetary_control_centralization, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_control_centralization, global_infrastructure).
narrative_ontology:boltzmann_floor_override(monetary_control_centralization, 0.18).
narrative_ontology:affects_constraint(monetary_control_centralization, reserve_currency_dominance).
narrative_ontology:affects_constraint(monetary_control_centralization, financial_system_stability).
narrative_ontology:affects_constraint(monetary_control_centralization, inflation_dynamics).
narrative_ontology:affects_constraint(monetary_control_centralization, credit_access_inequality).

% DUAL FORMULATION NOTE:
% Monetary control centralization decomposes into structurally distinct constraints: (1) the domestic coordination problem (how to establish common money within a nation), (2) the international reserve currency system (asymmetric extraction between reserve and non-reserve currency countries), (3) the seigniorage capture mechanism (extraction of monetary creation benefits), and (4) the financial exclusion system (exclusion of unbanked populations). These operate at different scopes and have different epsilon values. The central story captures the hybrid tangled rope character; the downstream constraints capture specific extraction mechanisms and coordination subproblems.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(monetary_control_centralization, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
