% ============================================================================
% CONSTRAINT STORY: currency_confidence_basin
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_currency_confidence_basin, []).

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
 *   constraint_id: currency_confidence_basin
 *   human_readable: Currency Confidence Basin
 *   domain: monetary_economics/institutional_stability
 *
 * SUMMARY:
 *   A currency confidence basin is a self-reinforcing institutional trap
 *   where the medium-of-exchange function depends on collective belief in the
 *   currency's stability, and that belief is sustained by the very fact that
 *   everyone is using it — creating a circular dependency with no stable exit
 *   point. Early in the currency's lifecycle, the confidence basin exhibits
 *   pure coordination (Rope): the medium-of-exchange function is genuine and
 *   valuable. As macroeconomic conditions deteriorate or monetary policy
 *   becomes extractive, the basin transitions through Tangled Rope (mixed
 *   coordination and extraction) into Snare (pure extraction with suppression
 *   by legal tender laws and lack of alternatives). The constraint exhibits
 *   all six DR types from different temporal and power-level perspectives. A
 *   powerless currency bearer at the moment of collapse faces a pure snare —
 *   their exit options have collapsed, suppression is total, and extraction
 *   is maximum. An early adopter at the system's inception sees rope —
 *   genuine coordination benefit with arbitrage options. An organized
 *   monetary reform coalition sees a scaffold — a temporary coordination
 *   failure with explicit pathways to reform and a sunset horizon. The
 *   issuing authority sees rope — seigniorage extraction is legitimate
 *   coordination cost. The central bank maintaining the system through
 *   performative confidence-building sees a piton — the machinery persists
 *   through institutional inertia as the real coordination function migrates
 *   elsewhere. The analytical observer risks seeing a mountain — natural law
 *   of fiat currency — but the structural data reveals this as a false
 *   summit: currencies with strong fundamentals and transparent governance do
 *   not become trapped in confidence basins.
 *
 * KEY AGENTS:
 *   - Currency Bearer at Collapse: Primary victim (powerless/trapped) — holds depreciating currency with no legal alternatives; forced to transact in deteriorating asset
 *   - Late Adopter: Secondary victim (moderate/constrained) — enters at higher purchasing power parity; bears disproportionate real cost; faces costly exit options
 *   - Currency Issuing Authority: Primary beneficiary (institutional/arbitrage) — captures seigniorage during expansion phase; can exit via currency reform
 *   - Early Adopter: Secondary beneficiary (powerful/arbitrage) — enters at low purchasing power parity; benefits from later appreciation; can observe and exit before collapse
 *   - Monetary Reform Coalition: Organized agents (organized/constrained) — central banks, international institutions, crypto communities building alternative pathways with explicit sunset
 *   - Monetary Institution: Institutional actor (institutional/arbitrage) — maintains confidence basin ritual; sees own function degraded (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent to fiat currency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(currency_confidence_basin, 0.58).
domain_priors:suppression_score(currency_confidence_basin, 0.62).
domain_priors:theater_ratio(currency_confidence_basin, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(currency_confidence_basin, extractiveness, 0.58).
narrative_ontology:constraint_metric(currency_confidence_basin, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(currency_confidence_basin, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(currency_confidence_basin, tangled_rope).
narrative_ontology:human_readable(currency_confidence_basin, "Currency Confidence Basin").
narrative_ontology:topic_domain(currency_confidence_basin, "monetary_economics/institutional_stability").

domain_priors:requires_active_enforcement(currency_confidence_basin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(currency_confidence_basin, currency_issuing_authority).
narrative_ontology:constraint_beneficiary(currency_confidence_basin, early_currency_holders).
narrative_ontology:constraint_victim(currency_confidence_basin, late_adopters).
narrative_ontology:constraint_victim(currency_confidence_basin, currency_bearers_at_collapse).
narrative_ontology:constraint_victim(currency_confidence_basin, real_economy_agents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CURRENCY BEARER AT COLLAPSE (SNARE) — Trapped within a depreciating currency with no exit. Wages, savings, and purchasing power evaporate. No alternative medium of exchange available in the jurisdiction. Maximum extraction: forced to hold and transact in a deteriorating asset. Suppression is structural — legal tender laws, capital controls, and lack of convertible alternatives prevent exit.
constraint_indexing:constraint_classification(currency_confidence_basin, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LATE ADOPTER (TANGLED ROPE) — Enters the currency at higher purchasing power parity, bearing disproportionate real cost. Benefits from the medium of exchange function and access to a developed transaction ecosystem. Exit options exist (foreign currency, barter, alternative payment networks) but are costly — regulatory obstacles, transaction fees, social friction. Moderate extraction with genuine coordination benefit.
constraint_indexing:constraint_classification(currency_confidence_basin, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CURRENCY ISSUING AUTHORITY (ROPE) — Experiences the constraint as pure coordination: the currency solves the double-coincidence-of-wants problem, enables tax collection, and provides monetary policy instrument. Early extraction (seigniorage) is legitimate coordination cost, not overhead. Can exit via currency reform or replacement. Net beneficiary of confidence maintenance.
constraint_indexing:constraint_classification(currency_confidence_basin, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EARLY ADOPTER (ROPE) — Enters at low purchasing power parity, benefiting from the currency's later appreciation and widespread adoption. Arbitrage options available: foreign currency holdings, hard assets, early exit before collapse. Experiences coordination benefit and accumulation benefit with minimal extraction. Can observe and exit before basin collapse.
constraint_indexing:constraint_classification(currency_confidence_basin, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MONETARY REFORM COALITION (SCAFFOLD) — Organized actors (central banks, international institutions, crypto communities) perceive the confidence basin as a temporary coordination failure with a sunset. Currency reform, dollarization, or crypto migration represent exits from the trap. Low effective extraction from this perspective because the coalition has agency and sees an explicit time-limited pathway out. Theater ratio relatively low within reform movements.
constraint_indexing:constraint_classification(currency_confidence_basin, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: MONETARY INSTITUTION (PITON) — Central bank maintains the confidence basin ritual despite degradation. Reserve requirements, interest rate management, and confidence-building theater persist largely for institutional continuity. The actual coordination function (medium of exchange, store of value) has atrophied or relocated to alternative payment systems. Theater ratio high (0.68) — much of policy communication is performative confidence maintenance rather than effective monetary control.
constraint_indexing:constraint_classification(currency_confidence_basin, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, confidence basins are inherent to fiat currency systems: any currency requires collective belief to function, and that belief is necessarily fragile. No fiat system can escape the confidence trap — it is a logical feature of the structure. However, the structural data contradicts the mountain classification. The engine's false summit detector will reveal that what appears as natural law is actually a contingent institutional arrangement: currencies with strong macroeconomic fundamentals, transparent governance, and exit options maintain confidence without being trapped in basins.
constraint_indexing:constraint_classification(currency_confidence_basin, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(currency_confidence_basin_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(currency_confidence_basin, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(currency_confidence_basin, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(currency_confidence_basin, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(currency_confidence_basin, TR),
    TR >= 0.70.

:- end_tests(currency_confidence_basin_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The confidence basin exhibits real extraction through seigniorage (the issuing authority's new-money privilege) and through inflation tax (held currency loses purchasing power). The measurement trajectory (0.22 → 0.38 → 0.58) shows extraction accumulating over time as macroeconomic conditions deteriorate and monetary policy becomes more extractive. However, early-stage extractiveness is lower because the medium-of-exchange function provides genuine coordination benefit that partially offsets seigniorage. Late-stage extractiveness is high because the coordination benefit persists (people still need a medium of exchange) while the extraction mechanism becomes dominant. Suppression (0.62): Moderate-high. Legal tender laws establish suppression by removing alternative media of exchange from competition. Capital controls may prevent foreign currency exit. Lack of developed alternative payment infrastructure suppresses exit options. However, suppression is not total — parallel economies, dollarization, and informal barter can provide partial exit even in collapsed currencies. Theater ratio (0.68): High. Central bank confidence-building communication is substantially performative: interest rate signals, reserve requirement adjustments, and public statements serve to maintain belief in the currency's stability rather than to control the real economy. As the basin deteriorates, theater ratio increases because policy becomes increasingly focused on confidence maintenance rather than macroeconomic management. The trajectory (0.35 → 0.52 → 0.68) shows theater accumulating as the coordination function atrophies.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals that the same institutional arrangement (fiat currency backed by collective belief) appears as rope (coordination), tangled rope (mixed), snare (extraction), scaffold (temporary), piton (degraded), and mountain (natural law) depending on the observer's power level, temporal horizon, and exit options. This divergence is not measurement error — it reflects genuine structural differences in how agents experience the constraint. The issuing authority's rope experience is real: they are solving the double-coincidence-of-wants problem. The trapped bearer's snare experience is equally real: they have no exit. The analytical observer's false mountain reveals that naturalizing the constraint prevents seeing the policy choices and institutional arrangements that create the trap.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective reflects the agent's structural position relative to the extraction flow. Early adopters and the issuing authority have low d (beneficiaries with exit options): they receive seigniorage benefit and can arbitrage their position. Late adopters have moderate d: they pay most of the extraction but benefit from the medium-of-exchange function. Trapped currency bearers at collapse have maximum d (1.0): they bear the full extraction with no exit. Monetary institutions have low d (institutional beneficiaries): they control the machine. Organized reform coalitions have moderate d: they face constraints but have exit pathways. The analytical observer has moderate-high d (0.73): the observer can see the structure but is not trapped by it. The sigmoid function f(d) maps these positions to experienced extractiveness. The engine derives d automatically from beneficiary/victim declarations and exit options: beneficiaries → low d → low/negative χ; victims with trapped exit → high d → high χ.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that currency confidence basins contain both genuine coordination (medium-of-exchange function) and genuine extraction (seigniorage, inflation tax, loss of exit options) within a single institutional structure. The mandatrophy dissolves when the perspectives are disaggregated: some agents experience rope because they benefit from coordination and have exit options; other agents experience snare because they lack exit and bear the extraction. The classification is not contradictory — it reflects the real structural asymmetry of the constraint. The false summit at the analytical/mountain perspective reveals that naturalizing the confidence basin ('fiat currency requires belief, so basins are inevitable') obscures the policy choices and governance arrangements that determine whether a currency maintains genuine coordination (rope) or collapses into pure extraction (snare). The constraint's resolved mandatrophy confirms that Tangled Rope is the accurate type-level classification: the constraint genuinely coordinates (solves medium-of-exchange problem) AND exhibits asymmetric extraction (seigniorage, inflation tax, differential exit costs). Both mechanisms are structural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    confidence_collapse_threshold,
    'What set of macroeconomic conditions or policy decisions triggers confidence basin collapse from Rope to Snare?',
    'Historical comparison of currency crises: identify conditions preceding irreversible confidence loss (inflation rate thresholds, foreign currency reserves levels, political instability metrics)',
    'If threshold is sharp and detectable: basin behavior is predictable and avoidable (structural entrapment can be prevented). If threshold is diffuse or path-dependent: agents cannot coordinate on prevention, and snare classification becomes inevitable once entry begins.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(confidence_collapse_threshold, empirical, 'Threshold conditions for confidence basin collapse').

omega_variable(
    alternative_medium_emergence,
    'Do alternative media of exchange (foreign currency, crypto, barter networks) emerge spontaneously when fiat confidence erodes, reducing extraction, or do they face sufficient regulatory/network barriers that powerless agents cannot access them?',
    'Case study analysis of currency crises with and without alternative emergence (Argentina peso crisis vs Venezuela bolivar crisis vs Zimbabwe dollar); measurement of alternative medium adoption rates by income quintile',
    'If alternatives emerge freely: exit options for trapped agents are actually mobile or arbitrage, not trapped. Snare classification becomes rope or tangled rope. If barriers are insurmountable for powerless agents: snare classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_medium_emergence, empirical, 'Whether alternative media emerge to reduce extraction during confidence crises').

omega_variable(
    seigniorage_extraction_visibility,
    'Do currency bearers recognize seigniorage extraction, or does the mechanism remain cognitively hidden behind inflation framing?',
    'Survey data on currency bearer understanding of seigniorage mechanics; correlation between inflation awareness and behavioral responses (hoarding, alternative adoption, capital flight)',
    'If hidden: suppression is partly internalized (agents don''t see the extraction flow), increasing effective suppression. If visible: suppression is structural only, potentially lower. Identity-lock mechanisms may operate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(seigniorage_extraction_visibility, empirical, 'Cognitive visibility of seigniorage extraction mechanism').

omega_variable(
    network_externality_lock,
    'Is the confidence basin sustained primarily by the medium-of-exchange network function (coordination benefit is genuine) or primarily by regulatory lock-in and the lack of adoption of alternatives?',
    'Decomposition of currency use by transaction type and jurisdiction: measure voluntary use (domestic commerce) vs coercive use (tax payment, legal requirement). Compare with periods of competing currencies where legal tender laws were relaxed.',
    'If network externality is primary: rope classification dominates; coordination benefit is real. If regulatory lock-in is primary: snare extraction is primary; network effects are secondary. Classification changes from tangled rope to snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(network_externality_lock, empirical, 'Whether confidence basin is sustained by genuine network effects or regulatory lock-in').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(currency_confidence_basin, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(curconf_tr_t0, currency_confidence_basin, theater_ratio, 0, 0.35).
narrative_ontology:measurement(curconf_tr_t15, currency_confidence_basin, theater_ratio, 15, 0.52).
narrative_ontology:measurement(curconf_tr_t30, currency_confidence_basin, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(curconf_be_t0, currency_confidence_basin, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(curconf_be_t15, currency_confidence_basin, base_extractiveness, 15, 0.38).
narrative_ontology:measurement(curconf_be_t30, currency_confidence_basin, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(currency_confidence_basin, resource_allocation).
narrative_ontology:affects_constraint(currency_confidence_basin, monetary_policy_transmission).
narrative_ontology:affects_constraint(currency_confidence_basin, hyperinflation_lock_in).
narrative_ontology:affects_constraint(currency_confidence_basin, dollarization_resistance).

% DUAL FORMULATION NOTE:
% Currency confidence basins are downstream of specific monetary policy choices but represent a distinct structural constraint. The upstream constraints (monetary policy transmission, central bank credibility) have their own extractiveness values reflecting the effectiveness of monetary tools. The confidence basin constraint captures the emergent lock-in behavior that occurs when coordination on a single medium of exchange creates a basin with multiple equilibria and path-dependent collapse dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(currency_confidence_basin, powerful, 0.1).
constraint_indexing:directionality_override(currency_confidence_basin, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
