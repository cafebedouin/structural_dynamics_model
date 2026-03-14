% ============================================================================
% CONSTRAINT STORY: currency_hegemony
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_currency_hegemony, []).

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
 *   constraint_id: currency_hegemony
 *   human_readable: Currency Hegemony: Asymmetric Extraction Through Monetary System Architecture
 *   domain: political_economy/international_finance
 *
 * SUMMARY:
 *   Currency hegemony describes the structural arrangement in which one
 *   nation's currency (historically the pound sterling, currently the US
 *   dollar) functions as the primary medium for international trade
 *   settlement, foreign exchange reserves, and financial contracting. This
 *   arrangement generates asymmetric benefits for the hegemon while imposing
 *   costs on non-hegemonic nations. The constraint exhibits characteristics
 *   of both coordination (genuine settlement function that enables global
 *   trade) and extraction (asymmetric distribution of benefits, suppression
 *   of alternatives, path-dependent lock-in). The extractiveness value (0.58)
 *   reflects that the system performs a real coordination function — it is
 *   not pure predation — but achieves this at the cost of systematic
 *   asymmetry. The suppression value (0.65) reflects multiple barriers to
 *   exit: network effects (liquidity concentrated in hegemon currency),
 *   institutional dependence (international law, contracts, reserves
 *   denominated in hegemon currency), sanctions risk (exit attempts are often
 *   followed by financial isolation), and coordination failure (no single
 *   alternative has achieved sufficient liquidity to be credible). The
 *   theater ratio (0.48) reflects that while institutional justifications
 *   exist for reserve currency arrangements, the technical rationale largely
 *   tracks political power — the dollar is reserve currency primarily because
 *   the US military and economy enforce that status, not because it has
 *   intrinsic technical superiority over alternatives.
 *
 * KEY AGENTS:
 *   - Hegemon Nation State (institutional/arbitrage): Primary beneficiary — captures seigniorage, maintains capital inflows, extracts geopolitical compliance through financial leverage
 *   - Hegemon Financial Sector (institutional/arbitrage): Primary beneficiary — profits from reserve currency premium, obtains low-cost capital, gains arbitrage opportunities
 *   - Non-Hegemonic Central Banks (moderate/constrained): Secondary victim — must accumulate reserve currency to stabilize terms of trade, face volatile capital flows, constrained by coordination lock-in
 *   - Commodity-Exporting Economies (powerless/trapped): Primary victim — price-taker exposure to currency volatility, forced to hold depreciating assets, no credible exit
 *   - Alternative Currency Coalition (organized/constrained): Organized resistance — China, BRICS, EU building parallel infrastructure (CIPS, regional currencies, CBDCs) with potential sunset clause
 *   - International Monetary Institutions (institutional/arbitrage): Maintainers of the system — IMF, World Bank, BIS provide technical language and conditionality that embed asymmetry as 'best practice'
 *   - Analytical Observer (analytical/analytical): Sees risk of naturalizing contingent arrangement as inevitable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(currency_hegemony, 0.58).
domain_priors:suppression_score(currency_hegemony, 0.65).
domain_priors:theater_ratio(currency_hegemony, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(currency_hegemony, extractiveness, 0.58).
narrative_ontology:constraint_metric(currency_hegemony, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(currency_hegemony, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(currency_hegemony, tangled_rope).
narrative_ontology:human_readable(currency_hegemony, "Currency Hegemony: Asymmetric Extraction Through Monetary System Architecture").
narrative_ontology:topic_domain(currency_hegemony, "political_economy/international_finance").

domain_priors:requires_active_enforcement(currency_hegemony).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(currency_hegemony, hegemon_nation_state).
narrative_ontology:constraint_beneficiary(currency_hegemony, reserve_currency_financial_sector).
narrative_ontology:constraint_victim(currency_hegemony, non_reserve_currency_nations).
narrative_ontology:constraint_victim(currency_hegemony, global_trade_partners).
narrative_ontology:constraint_victim(currency_hegemony, commodity_exporting_economies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMMODITY-EXPORTING PERIPHERY (SNARE) — Nations dependent on commodity exports denominated in reserve currency face structural extraction: price volatility in foreign currency, inability to control export revenue, forced accumulation of reserve currency assets to stabilize terms of trade. Exit requires currency diversification (high cost, political resistance from trading partners) or dollarization (permanent loss of monetary sovereignty). Maximum extraction experienced by trapped agents with no exit.
constraint_indexing:constraint_classification(currency_hegemony, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-HEGEMONIC CENTRAL BANK (TANGLED ROPE) — Must coordinate international trade settlement (genuine function) while bearing asymmetric costs of reserve currency dependence. Can theoretically exit (adopt alternative, build regional alternatives) but faces sanctions, market volatility, and coordination failure costs. Experiences both coordination benefit and extraction: settlement function exists, but asymmetry is built into the system.
constraint_indexing:constraint_classification(currency_hegemony, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HEGEMON FINANCIAL SECTOR (ROPE) — Benefits from reserve currency status through financial arbitrage, capital inflows, and seigniorage. Experiences the constraint as pure coordination: settlement mechanism that enables global trade from which they extract premium. Net beneficiary with high agency and ability to arbitrage to alternative systems (but chooses not to — payoff is too high).
constraint_indexing:constraint_classification(currency_hegemony, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PEER HEGEMON OR RISING POWER (TANGLED ROPE) — Structurally similar to the hegemon but without reserve currency premium. Sees currency hegemony as both coordination mechanism (enabling their trade) and extraction (asymmetric privilege for hegemon). Can invest in alternative arrangements (CIPS, regional currencies) but faces coordination lock-in and switching costs. Powerful but mobile — can move but at cost.
constraint_indexing:constraint_classification(currency_hegemony, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ALTERNATIVE CURRENCY COALITION (SCAFFOLD) — Organized actors (China's CIPS, BRICS nations, regional currency unions) building parallel settlement infrastructure with sunset logic: gradual reduction in reserve currency dependence through geographic/sectoral alternatives. Low effective extraction because organized agents have real agency and see exit pathway. Theater remains moderate (alternative systems still developing) but functional coordination is genuine.
constraint_indexing:constraint_classification(currency_hegemony, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL MONETARY INSTITUTION (PITON) — IMF/World Bank frameworks and rules maintain the reserve currency system through technical language, conditionality clauses, and institutional inertia. The 'technical necessity' of dollar settlement is substantially performative: alternatives exist but institutional momentum keeps the system in place. Theater_ratio high because the technical rationalization obscures the political asymmetry. Degraded function: originally designed to enable post-WWII reconstruction, now primarily maintains hegemon privilege.
constraint_indexing:constraint_classification(currency_hegemony, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some currency hierarchy may appear inevitable: larger economies have deeper capital markets, more stable institutions, and higher credit quality, which naturally attract reserve-holding demand. This perspective risks naturalizing what is actually a political and institutional contingency. The engine's false summit detector will identify this as naturalization of a system-dependent outcome.
constraint_indexing:constraint_classification(currency_hegemony, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(currency_hegemony_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(currency_hegemony, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(currency_hegemony, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(currency_hegemony, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(currency_hegemony, TR),
    TR >= 0.70.

:- end_tests(currency_hegemony_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The system produces genuine coordination benefits — global trade settlement, price discovery, liquidity provision. However, these benefits are distributed asymmetrically: the hegemon captures disproportionate returns through seigniorage (ability to issue currency at low cost), persistent capital inflows (demand for safe assets denominated in their currency), geopolitical leverage (ability to weaponize currency access), and inflation export (ability to run persistent trade deficits funded by currency creation). The measured extractiveness reflects that coordination value exists but is captured inequitably. The trajectory shows increasing extractiveness over 75 years: initial arrangement (0.32) was arguably more genuinely coordinated after WWII; by late Cold War it had become substantially extractive (0.45); contemporary period shows further increase (0.58) as alternatives have failed to gain traction despite 50+ years of effort. Suppression (0.65): Multiple nested barriers prevent exit. Network effects mean alternatives lack sufficient liquidity (Lindy effect on dollar dominance). Institutional embedding means contracts, laws, and customs assume dollar settlement. Sanctions risk means attempts to build alternatives face coordinated opposition. Coordination failure means no single alternative achieves critical mass. The suppression is not total — some nations have partially escaped (China's CIPS, EU's euro, bilateral agreements) — but costs remain high enough to trap most nations. Theater ratio (0.48): Moderate. Technical justifications for reserve currency arrangements exist (network effects require settlement standard, liquidity requires size, deep capital markets require stability). But these technical arguments substantially track political power — the dollar is reserve currency because the US military and economy dominate, not because it has intrinsic technical superiority. The theater has decreased slightly over the interval as technical justification has weakened (alternatives now exist) but political enforcement has increased (sanctions weaponization).
 *
 * PERSPECTIVAL GAP:
 *   The foundational gap is between the hegemon and the periphery. The hegemon experiences pure coordination with high agency. The commodity exporter experiences pure extraction with no agency. Both are looking at the same constraint but their d values differ by 0.80+, producing massively different χ values. This gap is the diagnostic signature of the constraint: the same system produces rope-level benefit for one agent and snare-level harm for another. A smaller but important gap appears between the non-hegemonic central bank (tangled rope, moderate/constrained) and the alternative coalition (scaffold, organized/constrained) — same base constraints but different time horizons (biographical vs generational) and different power (moderate vs organized). The bank sees immediate extraction; the coalition sees a solvable problem. The piton perspective (institutional) from the IMF's position shows performative maintenance — the institution sees the system as degraded but continues to maintain it because the institutional identity has fused with that role. This is an identity_locked perspective at the institutional level.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from the agent's position in the extraction flow. The hegemon financial sector benefits from the arrangement (low d, around 0.10-0.15, yields negative χ) — their exit option is arbitrage, they are net beneficiaries, the formula produces low effective extraction from their perspective. The commodity exporter is a target (high d, around 0.90, yields high χ via f(d)) — they are trapped with no exit, they are net victims, maximum experienced extraction. The non-hegemonic central bank is in the middle (d around 0.55-0.60) — they experience asymmetry but have some agency through constrained exit options and can benefit from trade coordination. The rising power is similar (d around 0.48-0.52) — structurally mobile, benefits from trade, but extraction flow is positive (currency advantages flow toward hegemon). The alternative coalition is organized with agency (d around 0.40, constrained exit but real alternatives exist) — their effective extraction is lower because they have real options. The international institution derives d from their beneficiary position (maintaining the system benefits them, d around 0.15) — they experience low extraction in their own framework because they are the system's maintainers.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint qualifies as tangled rope because it combines (1) genuine coordination function (global settlement mechanism that enables trade), (2) asymmetric extraction (systematic benefit concentration toward hegemon), and (3) active enforcement (sanctions against alternatives, institutional embedding, financial isolation threats). All three gates are satisfied. The mandatrophy is resolved by recognizing that the constraint is not 'pure coordination disguised as extraction' but genuinely hybrid — removal of the extraction component would weaken the coordination function because the hegemon's willingness to maintain the system depends on the benefits they extract. Conversely, removal of the coordination component would be impossible without destroying the extraction. The constraint is not decomposable into separate coordination and extraction stories because the two are structurally coupled. The alternative coalition (Scaffold) perspective does not resolve the mandatrophy — it shows a real possibility of sunset, but only by building entirely new coordination mechanisms that bypass the extraction. The false summit (Mountain) is easily identified: the claim that large economies 'naturally' dominate finance overlooks how political choices (military dominance, capital account openness, sanctions enforcement) embed that dominance in institutional rules. Currency hegemony is contingent on political enforcement, not a natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reserve_currency_necessity_threshold,
    'Is reserve currency status necessary for global trade settlement, or is it a path-dependent institutional arrangement that could be replaced by alternatives?',
    'Historical comparison of settlement mechanisms (Bretton Woods, post-1971 floating, alternatives in CIPS/CBDC); analysis of whether network effects preclude alternatives or whether switching costs are surmountable',
    'If necessary: constraint approaches mountain (immutable coordination requirement). If path-dependent: constraint is tangled_rope (political choice disguised as technical necessity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reserve_currency_necessity_threshold, empirical, 'Whether reserve currency status is necessary or contingent').

omega_variable(
    alternative_settlement_coordination_failure,
    'Why have alternative payment systems (CIPS, bilateral agreements, regional currencies) not achieved functional parity with the dollar system despite two decades of development?',
    'Analysis of technical constraints vs political/institutional barriers; comparison of transaction costs, liquidity, and network depth across systems',
    'If technical: ceiling on alternative viability (snare from victim perspective is harder to escape). If institutional: alternatives are artificially suppressed (extraction is more clearly malign than coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_settlement_coordination_failure, empirical, 'Whether alternative settlement systems face technical or institutional barriers').

omega_variable(
    hegemon_monetary_transmission_asymmetry,
    'Does the hegemon''s ability to issue reserve currency at lower interest rates than other nations constitute seigniorage (legitimate coordination benefit) or pure extraction (unearned privilege)?',
    'Historical interest rate differentials, inflation transmission analysis, measurement of implicit transfers through currency denomination of global assets',
    'If seigniorage is legitimate coordination cost: lowers the extracted component of chi (more rope-like). If it is pure extraction: raises the snare component (more snare-like for victims).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hegemon_monetary_transmission_asymmetry, conceptual, 'Whether hegemon monetary privilege is seigniorage or extraction').

omega_variable(
    suppression_mechanism_internalization,
    'To what degree is the suppression of alternatives internalized (victim nations believe the dollar system is inevitable) versus structural (concrete barriers prevent exit)?',
    'Analysis of policy debates in non-hegemonic central banks; longitudinal tracking of alternative currency adoption attempts; measurement of costs incurred during exits (Venezuela, Iran, North Korea cases)',
    'If internalized: suppression persists even after barriers are removed — victims carry constraint with them. If structural: removing barriers enables rapid exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of alternatives is internalized or structural').

omega_variable(
    cbdc_decentralization_potential,
    'Can central bank digital currencies (CBDCs) operating on distributed ledgers decentralize reserve currency function, or will CBDCs reproduce the same hierarchy in digital form?',
    'Technical analysis of CBDC architecture (centralized vs decentralized settlement); analysis of early CBDC pilots (e-CNY, e-euro) for hierarchy reproduction patterns',
    'If decentralization succeeds: sunset clause for scaffold perspective is real (true generational transition). If hierarchy reproduces: CBDCs become piton (performative modernization maintaining old asymmetries).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cbdc_decentralization_potential, empirical, 'Whether CBDCs can decentralize reserve currency function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(currency_hegemony, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(currheg_tr_t0, currency_hegemony, theater_ratio, 0, 0.55).
narrative_ontology:measurement(currheg_tr_t25, currency_hegemony, theater_ratio, 25, 0.5).
narrative_ontology:measurement(currheg_tr_t50, currency_hegemony, theater_ratio, 50, 0.48).
narrative_ontology:measurement(currheg_tr_t75, currency_hegemony, theater_ratio, 75, 0.45).

% Extraction over time
narrative_ontology:measurement(currheg_be_t0, currency_hegemony, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(currheg_be_t25, currency_hegemony, base_extractiveness, 25, 0.45).
narrative_ontology:measurement(currheg_be_t50, currency_hegemony, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(currheg_be_t75, currency_hegemony, base_extractiveness, 75, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(currency_hegemony, resource_allocation).
narrative_ontology:boltzmann_floor_override(currency_hegemony, 0.2).
narrative_ontology:affects_constraint(currency_hegemony, geopolitical_financial_leverage).
narrative_ontology:affects_constraint(currency_hegemony, sanctions_architecture).
narrative_ontology:affects_constraint(currency_hegemony, capital_flow_volatility).
narrative_ontology:affects_constraint(currency_hegemony, commodity_price_denominator).
narrative_ontology:affects_constraint(currency_hegemony, alternative_currency_coordination_failure).

% DUAL FORMULATION NOTE:
% Currency hegemony is the upstream structural constraint. Downstream constraints (capital flows, commodity pricing, sanctions, geopolitical leverage) inherit the asymmetry from the currency hierarchy. Alternative currency coordination failure is a sister constraint: separately classifiable with its own stories but structurally linked to hegemony — it exists because hegemony suppresses alternatives. The family should include stories on CIPS development, regional currency unions, and CBDC architecture, each with their own epsilon values and perspectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(currency_hegemony, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
