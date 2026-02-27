% ============================================================================
% CONSTRAINT STORY: gold_piton_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_piton_2026, []).

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
 *   constraint_id: gold_piton_2026
 *   human_readable: The $5,000 Gold Barrier / Precious Metals Stampede
 *   domain: economic/fiscal
 *
 * SUMMARY:
 *   The $5,000 gold barrier represents a pivotal moment where gold
 *   transitions from a functional monetary reserve asset to an institutional
 *   Piton — a fixed point hammered into fiscal policy by historical
 *   convention and geopolitical theater, no longer performing its original
 *   coordination function but maintained through inertia and ceremonial
 *   legitimacy. Since the end of the Bretton Woods system in 1971, gold has
 *   had no direct monetary function: central bank reserves are fiat-backed,
 *   not commodity-backed, and monetary policy operates independently of gold
 *   holdings. Yet central banks hold 50,000+ metric tons of gold nominally as
 *   a 'last resort' asset and credibility signal to international markets. As
 *   the price approaches $5,000/oz (a five-fold increase from 2010 levels),
 *   the constraint exhibits all classical piton signatures: (1) Theater ratio
 *   above 0.70 — the ceremonial role (gold reserves certify monetary
 *   credibility) now dominates over functional role (gold backing for
 *   currency); (2) Extractiveness below 0.25 — the actual coercive extraction
 *   is modest because most actors benefit from the fiction or have exit
 *   options; (3) Institutional maintenance despite functional atrophy —
 *   central banks continue accumulating gold not because it solves a
 *   coordination problem, but because the alternative (admitting currency
 *   value rests on institutional faith alone) lacks political legitimacy. The
 *   $5,000 barrier itself becomes a focal point: at this price, gold
 *   accumulation becomes prohibitively expensive for poorer central banks,
 *   widening the geopolitical credibility gap. Simultaneously, digital
 *   alternatives (CBDCs, blockchain settlement, stablecoins) are building
 *   parallel coordination mechanisms that make the gold-reserve fiction
 *   increasingly transparent. The constraint is dying — maintained only by
 *   institutional theater — but its death is slow because the political cost
 *   of abandoning it is high. No leader wants to be the one who admits their
 *   currency is backed only by confidence.
 *
 * KEY AGENTS:
 *   - Central banks (US, Eurozone, China, etc.): Institutional beneficiaries (institutional/arbitrage) — maintain gold reserves as credibility signaling tool; benefit from low-cost accumulation and geopolitical leverage
 *   - Geopolitical power brokers: Powerful beneficiaries (powerful/mobile) — use gold reserves as collateral for international settlements and sanctions leverage; experience constraint as coordination mechanism
 *   - Emerging market central banks: Moderate victims (moderate/constrained) — forced to accumulate gold at high cost to maintain geopolitical credibility; constrained by the barrier, benefiting only from coordination function
 *   - Retail investors / currency-collapse populations: Powerless victims (powerless/trapped) — trapped in jurisdictions where gold is sought as last-resort wealth preservation; face extraction through price volatility, confiscation risk, and institutional gatekeeping
 *   - Digital asset coalition (CBDCs, blockchain platforms): Organized agents (organized/constrained) — building alternative coordination mechanisms that bypass gold; see sunset clause as structural
 *   - Gold mining companies and commodity traders: Beneficiaries with mobile exit (powerful/mobile) — profit from price volatility and institutional accumulation; can arbitrage between jurisdictions and asset classes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_piton_2026, 0.22).
domain_priors:suppression_score(gold_piton_2026, 0.38).
domain_priors:theater_ratio(gold_piton_2026, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_piton_2026, extractiveness, 0.22).
narrative_ontology:constraint_metric(gold_piton_2026, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(gold_piton_2026, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_piton_2026, piton).
narrative_ontology:human_readable(gold_piton_2026, "The $5,000 Gold Barrier / Precious Metals Stampede").
narrative_ontology:topic_domain(gold_piton_2026, "economic/fiscal").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_piton_2026, central_banks).
narrative_ontology:constraint_beneficiary(gold_piton_2026, gold_reserve_holders).
narrative_ontology:constraint_beneficiary(gold_piton_2026, geopolitical_power_brokers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CENTRAL BANK CUSTODIAN (PITON) — Gold remains a core reserve asset nominally linked to currency stability and geopolitical credibility. Central banks maintain gold holdings through institutional inertia and ceremonial backing, even as monetary policy has decoupled from gold standard constraints for 50+ years. The constraint persists because alternatives (pure fiat without commodity anchor, digital currency regimes) lack the political legitimacy that the gold fiction still provides. Theater ratio dominates: the functional constraint (gold backing for currency) has atrophied; the performative constraint (gold reserves as geopolitical credential) persists.
constraint_indexing:constraint_classification(gold_piton_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 2: GEOPOLITICAL POWER BROKER (ROPE) — For states with large gold reserves (US, Russia, China), the $5,000 barrier functions as a coordination mechanism: a shared reference point for asset valuation in international settlements and as collateral for geopolitical leverage. The constraint solves the coordination problem of 'what counts as hard backing?' without requiring explicit negotiation. Powerful actors with arbitrage exit options (can acquire more gold, can demand gold-backed settlement) experience this as coordination, not extraction.
constraint_indexing:constraint_classification(gold_piton_2026, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: EMERGING MARKET CENTRAL BANK (TANGLED ROPE) — Smaller or less-established central banks are constrained by the $5,000 barrier: they must accumulate gold at high nominal cost to acquire geopolitical credibility (coordination function), but the extraction comes through volatility risk and opportunity cost of holding an asset whose price is increasingly decoupled from macroeconomic fundamentals. The barrier enforces coordination (you must hold gold to be taken seriously) while extracting through forced participation in an inflating asset that serves no functional monetary purpose.
constraint_indexing:constraint_classification(gold_piton_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: RETAIL INVESTOR / WEALTH PRESERVER (SNARE) — As gold approaches $5,000/oz, retail individuals trapped in hyperinflationary jurisdictions or seeking portable wealth face a maximum extraction mechanism: gold prices are set by institutional actors, carry storage and verification costs, and are subject to confiscatory taxation or seizure. The individual cannot exit — they are locked in either through currency collapse or through geopolitical uncertainty. No coordination benefit accrues to them; the constraint exists to extract wealth from those with no alternatives.
constraint_indexing:constraint_classification(gold_piton_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 5: DIGITAL ASSET COALITION (SCAFFOLD) — Bitcoin, stablecoins, and central bank digital currencies (CBDCs) represent emerging alternative coordination mechanisms that bypass gold entirely. The gold-as-credibility fiction is being systematically replaced by transparent, auditable digital reserves and blockchain-backed asset proofs. The sunset clause is structural: as adoption of CBDC rails increases and blockchain settlement becomes standard, the $5,000 gold barrier loses its coordination function. The constraint is temporary because its functional role has alternative solutions under active deployment.
constraint_indexing:constraint_classification(gold_piton_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / PHYSICS VIEW (MOUNTAIN) — Gold's chemical inertness, divisibility, and rarity are invariant physical properties. The constraint might appear to be a natural law: gold retains value because of immutable scarcity and material stability across civilizational timescales. However, the structural data contradicts this mountain classification — the theater ratio (0.78) and the decoupling of gold price from functional monetary value reveal that the constraint is institutional performativity, not physics. The engine's false summit detector will flag this as naturalization.
constraint_indexing:constraint_classification(gold_piton_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_piton_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gold_piton_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gold_piton_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(gold_piton_2026, TR),
    TR >= 0.70.

:- end_tests(gold_piton_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.22): Low-to-moderate. Gold does extract value from retail holders (through volatility, confiscation risk, storage costs) and from emerging market central banks (forced high-cost accumulation for credibility). But most institutional actors benefit from the system or have arbitrage options, so average extraction is modest. The time-series shows extractiveness rising as the price climbs toward $5,000, indicating that the barrier itself amplifies extraction through scarcity cost. Suppression (0.38): Moderate. Alternatives exist — CBDCs, stablecoins, blockchain settlement — but institutional inertia, regulatory uncertainty, and the political cost of abandoning gold-backed credibility keep suppression meaningful. Central banks are not legally prohibited from abandoning gold reserves, but the political and reputational cost is high. Theater ratio (0.78): High and rising. The functional constraint (gold as monetary backing) disappeared in 1971; what remains is ceremonial. Gold reserves no longer constrain monetary policy, set exchange rates, or limit central bank balance sheets. The theater persists because admitting that currency value rests entirely on institutional faith is politically costly. The measurement trajectory (0.55 → 0.78 over 6 time periods) captures the growing gap between the stated role (last-resort credibility) and actual role (geopolitical theater).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals the constraint's piton nature. Central banks and geopolitical powers see rope — coordination through a shared reference asset. Emerging markets see tangled rope — forced participation with mixed costs and benefits. Retail investors see snare — pure extraction with no exit. The organized digital coalition sees scaffold — a temporary constraint being replaced by superior alternatives. The analytical observer risks seeing mountain — but physics cannot explain why institutional actors maintain a constraint that serves no functional monetary purpose. The gap emerges from directionality: beneficiaries experience coordination; victims experience extraction; organized actors experience a sunset.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values differ sharply across perspectives. Central banks and geopolitical powers derive d from beneficiary status + arbitrage exit options → low d → low f(d) → they experience negative or zero effective extraction (the system advantages them). Emerging market central banks derive d from victim status + constrained exit → moderate d → moderate f(d) → they experience moderate extraction (forced participation in an inflating asset). Retail investors trapped in currency-collapse jurisdictions derive d from victim status + trapped exit → high d → high f(d) → they experience maximum extraction (no alternatives, price volatility, confiscation risk). The analytical observer (mountain perspective) risks naturalizing institutional fiction — gold scarcity is real, but the constraint's institutional role is not natural law. The engine's false summit detector should flag this.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is RESOLVED by the piton classification itself. The constraint appears to be a mountain (gold scarcity, physical inertness, historical monetary role) from the analytical perspective, but the high theater ratio (0.78), the low functional extraction, and the clear institutional maintenance despite functional atrophy reveal it as a Piton. The theater is the signature: if gold were truly a natural law or functional constraint, its role would not decay over time while the price rises. Instead, we observe the classic piton pattern: the primary function (monetary backing) has atrophied, but the constraint persists through ceremonial reaffirmation and institutional inertia. Central banks continue holding gold not because it solves a monetary problem (it doesn't — they're holding fiat), but because abandoning it would require admitting that currency value rests entirely on institutional confidence. The piton is maintained by the cost of its demolition, not by its functional utility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cbdc_adoption_threshold,
    'What percentage of cross-border settlement must route through CBDCs before the gold-reserve coordination mechanism becomes purely ceremonial?',
    'Monitoring of CBDC payment volume, central bank policy announcements, replacement of gold settlement with digital alternatives in bilateral agreements',
    'If CBDC adoption exceeds 40% of cross-border flows: gold barrier loses functional coordination role and transitions to pure piton. If adoption stalls below 15%: gold remains functional coordinator for legacy systems.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cbdc_adoption_threshold, empirical, 'CBDC adoption threshold for gold coordination obsolescence').

omega_variable(
    confiscation_risk_materialization,
    'Do geopolitical events trigger confiscatory seizures of private gold holdings, converting the snare into a pure extraction event?',
    'Historical tracking of capital controls, gold seizure laws, enforcement actions against precious metals holders in crisis jurisdictions',
    'If confiscation events occur in >3 major economies: snare classification is validated; retail exit routes close entirely. If no confiscation: snare is a theoretical trap with lower actual extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(confiscation_risk_materialization, empirical, 'Whether geopolitical crisis triggers gold confiscation').

omega_variable(
    price_volatility_regime_shift,
    'Does the $5,000 barrier itself become unstable, breaking upward to $8,000+ and causing institutional holders to reassess gold''s role?',
    'Analysis of price discovery mechanisms, tracking of institutional rebalancing decisions, central bank policy response to sustained high gold prices',
    'If gold breaches $7,000 and stays above 2+ years: central banks reassess holdings; piton transitions to degraded snare. If price stabilizes around $5,000-6,000: piton classification persists with theater_ratio stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(price_volatility_regime_shift, empirical, 'Whether $5,000 barrier becomes unstable threshold').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_piton_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_piton_tr_t0, gold_piton_2026, theater_ratio, 0, 0.55).
narrative_ontology:measurement(gold_piton_tr_t3, gold_piton_2026, theater_ratio, 3, 0.68).
narrative_ontology:measurement(gold_piton_tr_t6, gold_piton_2026, theater_ratio, 6, 0.78).

% Extraction over time
narrative_ontology:measurement(gold_piton_be_t0, gold_piton_2026, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(gold_piton_be_t3, gold_piton_2026, base_extractiveness, 3, 0.18).
narrative_ontology:measurement(gold_piton_be_t6, gold_piton_2026, base_extractiveness, 6, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_piton_2026, information_standard).
narrative_ontology:affects_constraint(gold_piton_2026, currency_confidence_basin).
narrative_ontology:affects_constraint(gold_piton_2026, reserve_accumulation_race).
narrative_ontology:affects_constraint(gold_piton_2026, geopolitical_settlement_standards).

% DUAL FORMULATION NOTE:
% The $5,000 gold barrier decomposes into three structurally distinct constraints: (1) the monetary reserve coordination function (low-extraction rope, largely ceremonial), (2) the geopolitical credibility signaling mechanism (moderate-extraction tangled rope for emerging markets), and (3) the retail wealth-preservation trap (high-extraction snare for individuals in currency-collapse zones). The piton classification applies to the overall constraint because the combined theater ratio dominates; the network links capture how changes to monetary policy or CBDC adoption will differentially affect each sub-constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gold_piton_2026, institutional, 0.08).
constraint_indexing:directionality_override(gold_piton_2026, powerful, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
