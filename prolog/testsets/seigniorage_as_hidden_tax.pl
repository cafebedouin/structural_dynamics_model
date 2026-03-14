% ============================================================================
% CONSTRAINT STORY: seigniorage_as_hidden_tax
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_seigniorage_as_hidden_tax, []).

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
 *   constraint_id: seigniorage_as_hidden_tax
 *   human_readable: Seigniorage As Hidden Tax
 *   domain: monetary_policy/public_finance
 *
 * SUMMARY:
 *   Seigniorage — the profit earned by a monetary authority from issuing
 *   currency at a cost below its face value — functions as a hidden tax that
 *   transfers wealth from currency holders to the government and central
 *   bank. The constraint exhibits mixed coordination and extraction: fiat
 *   money provides genuine coordination benefits (medium of exchange, store
 *   of value, monetary policy transmission) while simultaneously enabling
 *   systematic wealth transfer through inflation. The constraint is 'hidden'
 *   because seigniorage is rarely labeled as taxation; it operates through
 *   the diffuse mechanism of currency debasement rather than explicit levies.
 *   This opacity, combined with structural barriers to exit (transaction
 *   costs of currency substitution, informational barriers, regulatory
 *   suppression of alternatives), creates a tangled rope: real coordination
 *   function intertwined with asymmetric extraction. The measurement
 *   trajectory shows increasing extractiveness (0.28 → 0.52) and theater
 *   ratio (0.35 → 0.62) over the interval, indicating both a rise in actual
 *   inflation and growing institutional reliance on seigniorage as fiscal
 *   mechanism, accompanied by increased performative justification (central
 *   bank independence rhetoric, inflation-targeting frameworks).
 *
 * KEY AGENTS:
 *   - Central Bank: Primary beneficiary (institutional/arbitrage) — captures seigniorage directly through money creation; also performs genuine coordination function
 *   - Government Treasury: Primary beneficiary (institutional/arbitrage) — receives seigniorage transfers; uses inflation as fiscal instrument
 *   - Unbanked Savers: Primary victim (powerless/trapped) — hold currency in physical form; cannot access inflation hedges; experience full extraction
 *   - Fixed-Income Earners: Secondary victim (moderate/constrained) — pension/wage income eroded by inflation; can partially hedge via banking access but at cost
 *   - Financial Sector: Mixed beneficiary/victim (institutional/constrained) — benefits from monetary expansion but faces compression of real returns on deposits
 *   - Cryptocurrency Adopters: Alternative pathway agents (organized/mobile) — building exit routes through decentralized money; see seigniorage as temporary institutional arrangement
 *   - Peripheral Currency Issuer: Constrained institutional actor (institutional/constrained) — extracts seigniorage from domestic savers but faces capital flight constraint if inflation too high
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(seigniorage_as_hidden_tax, 0.52).
domain_priors:suppression_score(seigniorage_as_hidden_tax, 0.68).
domain_priors:theater_ratio(seigniorage_as_hidden_tax, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(seigniorage_as_hidden_tax, extractiveness, 0.52).
narrative_ontology:constraint_metric(seigniorage_as_hidden_tax, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(seigniorage_as_hidden_tax, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(seigniorage_as_hidden_tax, tangled_rope).
narrative_ontology:human_readable(seigniorage_as_hidden_tax, "Seigniorage As Hidden Tax").
narrative_ontology:topic_domain(seigniorage_as_hidden_tax, "monetary_policy/public_finance").

domain_priors:requires_active_enforcement(seigniorage_as_hidden_tax).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(seigniorage_as_hidden_tax, central_bank).
narrative_ontology:constraint_beneficiary(seigniorage_as_hidden_tax, government_treasury).
narrative_ontology:constraint_beneficiary(seigniorage_as_hidden_tax, early_currency_adopters).
narrative_ontology:constraint_victim(seigniorage_as_hidden_tax, currency_holders).
narrative_ontology:constraint_victim(seigniorage_as_hidden_tax, savers).
narrative_ontology:constraint_victim(seigniorage_as_hidden_tax, fixed_income_earners).
narrative_ontology:constraint_victim(seigniorage_as_hidden_tax, unbanked_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNBANKED SAVER (SNARE) — Holds currency in physical form; cannot access alternative assets or inflation hedges. Bears full extraction through currency debasement with no alternatives. Exit requires access to banking infrastructure, foreign currency, or hard assets — barriers are both structural and informational.
constraint_indexing:constraint_classification(seigniorage_as_hidden_tax, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FIXED-INCOME EARNER (TANGLED ROPE) — Receives pension or wage nominally fixed in local currency; benefits from monetary stability and currency coordination (rope function) while experiencing inflation erosion (extraction). Exit requires currency relocation or asset reallocation — possible but costly.
constraint_indexing:constraint_classification(seigniorage_as_hidden_tax, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CENTRAL BANK (ROPE) — Operates fiat currency system; extracts seigniorage but provides genuine coordination function (medium of exchange, store of value, unit of account). Experiences constraint as pure coordination — the ability to issue currency at low marginal cost is the mechanism that enables monetary policy and emergency liquidity provision.
constraint_indexing:constraint_classification(seigniorage_as_hidden_tax, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CRYPTOCURRENCY ADOPTER (SCAFFOLD) — Sees seigniorage extraction as temporary institutional arrangement; builds alternative monetary systems (Bitcoin, stablecoins, programmable money) that bypass traditional seigniorage mechanisms. Low effective extraction because adoption is voluntary and alternatives proliferate. Has sunset logic: if decentralized money achieves coordination function, seigniorage-based fiscal transfer becomes obsolete.
constraint_indexing:constraint_classification(seigniorage_as_hidden_tax, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: GOLD STANDARD VESTIGE (PITON) — Historical institutional memory of commodity-backed currency. Central banks still hold gold reserves despite fiat system dominance; the coordination function (credibility signal) is largely theatrical, yet institutions maintain the posture through inertia. Theater ratio elevated by the performative credibility function gold provides without material constraint on monetary policy.
constraint_indexing:constraint_classification(seigniorage_as_hidden_tax, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PERIPHERAL CURRENCY ISSUER (TANGLED ROPE) — Central bank in smaller economy with capital flight risk. Extracts seigniorage from domestic savers but is constrained by currency-substitution threat (households switch to dollars/euros if inflation too high). Genuine coordination function (trade denominated in local currency, fiscal transfer via money creation) exists alongside extraction. Exit would require joining monetary union (losing seigniorage entirely).
constraint_indexing:constraint_classification(seigniorage_as_hidden_tax, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scope, any fiat currency system necessarily involves seigniorage: the issuer captures the difference between the cost of production and the purchasing power of the currency. This appears as a natural law of monetary systems — inescapable by definition. However, structural data contradicts this naturalization: the extraction is contingent on the institutional choice to operate fiat currency and suppress alternatives; commodity-backed or algorithmic alternatives exist. False summit detector flags this as naturalized contingency.
constraint_indexing:constraint_classification(seigniorage_as_hidden_tax, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(seigniorage_as_hidden_tax_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(seigniorage_as_hidden_tax, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(seigniorage_as_hidden_tax, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(seigniorage_as_hidden_tax, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(seigniorage_as_hidden_tax, TR),
    TR >= 0.70.

:- end_tests(seigniorage_as_hidden_tax_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Seigniorage represents a measurable transfer from currency holders to the monetary authority. The magnitude varies by inflation rate and real velocity of money circulation. At 0.52, the value reflects typical developed-economy seigniorage (1-2% of GDP annually in many cases) plus hidden inflation tax effects distributed across savers and fixed-income earners. Suppression (0.68): High. Multiple mechanisms suppress alternatives: regulatory restrictions on non-fiat currency use in legal settlements, network effects (everyone must use the national currency for tax payments and major transactions), informational barriers (seigniorage is rarely transparently discussed), and technical barriers (currency substitution requires financial access and foreign exchange knowledge). Theater ratio (0.62): Moderate-high. Central bank independence frameworks, inflation-targeting committees, and forward guidance are performative elements that provide credibility signaling while maintaining the extraction mechanism. The theater increased over time as central banks moved from commodity backing (low theater, explicit constraints) to pure fiat with sophisticated communication strategies (high theater, flexible extraction). The trajectory shows measurement points at t=0,30,60,90 to track both the rise in actual inflation-driven extraction and the growth in institutional theater around monetary policy justification.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is stark and constitutes the constraint's diagnostic value. The unbanked saver experiences a snare with no exit (powerless/trapped) — they have no hedging options and see pure extraction. The fixed-income earner experiences tangled rope (moderate/constrained) — the currency system coordinates their wage payments and store of value, but extraction erodes purchasing power. The central bank experiences rope (institutional/arbitrage) — seigniorage is the mechanism that enables their entire coordination function. The cryptocurrency adopter experiences a scaffold (organized/mobile) — alternatives are emerging that bypass seigniorage entirely, and adoption is voluntary. The peripheral currency issuer experiences constrained tangled rope (institutional/constrained) — they extract via seigniorage but are limited by the credibility constraint; capital flight becomes rational at high inflation. The gold standard vestige (piton) reflects historical institutional memory where the constraint was more binding (commodity backing). The civilizational analytical observer risks seeing a mountain (seigniorage as inherent to fiat money) — but the structural data reveals this as naturalization: seigniorage is contingent on the institutional choice to operate fiat currency and suppress alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation follows the beneficiary/victim + exit option chain. The unbanked saver is a victim with trapped exit: d ≈ 0.95, f(d) ≈ 1.42, producing high χ toward snare. The central bank is a beneficiary with arbitrage exit: d ≈ 0.05, f(d) ≈ -0.12, producing negative or near-zero χ, experiencing the constraint as pure rope. The fixed-income earner is a victim with constrained exit (can partially hedge via financial access but at transaction cost): d ≈ 0.75, f(d) ≈ 1.15, producing moderate-high χ. The cryptocurrency adopter is organized with mobile exit (can choose to adopt alternatives without structural prohibition): d ≈ 0.50, f(d) ≈ 0.65, producing moderate χ and the scaffold classification (mobile organized actors with exit paths see temporary constraints). The peripheral currency issuer is institutional but constrained by capital flight risk: d ≈ 0.65, f(d) ≈ 1.00, producing moderate χ. Scope modifier σ(S)=1.0 for national scope (canonical); seigniorage at global scope would have σ=1.2, amplifying χ for all perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates mandatrophy resolution through structural asymmetry. The central bank's 'rope' perspective is not a misclassification but a genuine structural truth: from the central bank's viewpoint, seigniorage IS coordination. The question is whether the coordination function justifies the extraction cost imposed on other agents. The mandatrophy is resolved by recognizing that classification depends critically on agent position. The constraint is simultaneously rope (for central bank), snare (for powerless savers), tangled rope (for moderate agents), and scaffold (for organized alternatives). The false natural law (mountain) is exposed by showing that alternatives exist and that seigniorage is contingent on regulatory suppression of those alternatives. If alternatives became available at zero regulatory cost, the classification would shift across all perspectives: central bank would see rope shrink to arbitrage-only benefits; savers would move toward scaffold (can exit); the constraint would dissolve or transform into a pure service fee (much lower χ). The mandate — whether seigniorage is justified — hinges on whether the coordination function could be replicated by alternatives with lower total extraction. Current evidence is mixed: cryptocurrency systems provide some coordination benefits but fail on monetary stability and liquidity provision functions that central banks provide.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inflation_target_coordination_vs_extraction,
    'Is inflation targeting a coordination mechanism to stabilize expectations or an extraction mechanism optimized to target specific distributional outcomes?',
    'Analysis of inflation target rationale across central banks; correlation between inflation targets and distributional outcomes (wealth/debt reallocation); comparison of welfare functions implied by observed target choices vs. stated objectives',
    'If coordination: seigniorage is a necessary cost of monetary system maintenance (higher rope/scaffold classifications). If extraction: inflation targeting is a tool for systematic wealth transfer (higher snare/tangled_rope classifications).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inflation_target_coordination_vs_extraction, conceptual, 'Whether inflation targeting serves coordination or extraction').

omega_variable(
    monetary_transmission_effectiveness,
    'What fraction of seigniorage is captured directly by government treasury vs. distributed across private financial sector and real economy via monetary transmission?',
    'Accounting decomposition of money creation flows; measurement of central bank balance sheet composition and profit distribution; empirical analysis of transmission channels in different institutional settings',
    'If treasury captures >80%: classification shifts toward snare (direct extraction). If distributed across financial sector: classification toward tangled_rope (mixed coordination and extraction with diffuse beneficiary).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(monetary_transmission_effectiveness, empirical, 'Distribution of seigniorage between government and financial sector').

omega_variable(
    currency_substitution_threshold,
    'At what inflation rate does currency substitution to foreign or alternative money become economically rational for household savers?',
    'Historical analysis of dollarization events; comparison of inflation rates and currency-substitution rates across countries; threshold estimation for different household income levels',
    'If threshold < 5% annual inflation: suppression is weaker than measured (exit costs lower). If threshold > 20%: suppression is stronger than measured (true trapped populations persist longer). Affects exit_options classification for multiple perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(currency_substitution_threshold, empirical, 'Inflation threshold for rational currency substitution').

omega_variable(
    alternative_money_coordination_sufficiency,
    'Can decentralized alternatives (cryptocurrency, stablecoins, commodity-backed tokens) provide the coordination functions that justify seigniorage extraction (liquidity provision, monetary stability, emergency lending)?',
    'Comparative structural analysis of coordination functions in fiat vs. decentralized systems; real-world performance data from parallel money systems; assessment of failure modes in stress scenarios',
    'If alternatives sufficient: scaffold and cryptocurrency perspectives confirmed — sunset is real. If alternatives fail on specific functions: scaffold perspective is aspirational; seigniorage remains justified as coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_money_coordination_sufficiency, empirical, 'Whether alternative monetary systems provide equivalent coordination').

omega_variable(
    seigniorage_visibility_suppression,
    'Is high suppression (0.68) driven by structural barriers to exit or by informational opacity about the extraction mechanism itself?',
    'Public awareness surveys about seigniorage and its distributional effects; analysis of how inflation is framed in central bank communication; longitudinal tracking of seigniorage understanding vs. currency-substitution behavior',
    'If opacity is >50% of suppression: true suppression lower (~0.35); constraint is significantly sustained by theater. If opacity <20%: true suppression higher (~0.75); structural barriers are primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(seigniorage_visibility_suppression, empirical, 'Whether suppression is structural or informational').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(seigniorage_as_hidden_tax, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seign_theater_t0, seigniorage_as_hidden_tax, theater_ratio, 0, 0.35).
narrative_ontology:measurement(seign_theater_t30, seigniorage_as_hidden_tax, theater_ratio, 30, 0.48).
narrative_ontology:measurement(seign_theater_t60, seigniorage_as_hidden_tax, theater_ratio, 60, 0.62).
narrative_ontology:measurement(seign_theater_t90, seigniorage_as_hidden_tax, theater_ratio, 90, 0.65).

% Extraction over time
narrative_ontology:measurement(seign_extract_t0, seigniorage_as_hidden_tax, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(seign_extract_t30, seigniorage_as_hidden_tax, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(seign_extract_t60, seigniorage_as_hidden_tax, base_extractiveness, 60, 0.52).
narrative_ontology:measurement(seign_extract_t90, seigniorage_as_hidden_tax, base_extractiveness, 90, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(seigniorage_as_hidden_tax, resource_allocation).
narrative_ontology:boltzmann_floor_override(seigniorage_as_hidden_tax, 0.12).
narrative_ontology:affects_constraint(seigniorage_as_hidden_tax, inflation_targeting_regime).
narrative_ontology:affects_constraint(seigniorage_as_hidden_tax, currency_substitution_pressure).
narrative_ontology:affects_constraint(seigniorage_as_hidden_tax, financial_repression_mechanism).

% DUAL FORMULATION NOTE:
% Seigniorage as hidden tax is structurally distinct from but affects three downstream constraints: (1) inflation_targeting_regime (the explicit mechanism through which seigniorage is deployed), (2) currency_substitution_pressure (the mechanism through which suppression is maintained), and (3) financial_repression_mechanism (the distributional effect of seigniorage that systematically redirects wealth from savers to governments). Each downstream constraint has its own ε and perspectives; this story captures the base seigniorage mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(seigniorage_as_hidden_tax, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
