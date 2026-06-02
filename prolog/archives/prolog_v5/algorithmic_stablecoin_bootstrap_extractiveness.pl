% ============================================================================
% CONSTRAINT STORY: algorithmic_stablecoin_bootstrap_extractiveness
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_stablecoin_bootstrap_extractiveness, []).

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
 *   constraint_id: algorithmic_stablecoin_bootstrap_extractiveness
 *   human_readable: Algorithmic Stablecoin Bootstrap Extractiveness
 *   domain: cryptoeconomics/defi/monetary_policy
 *
 * SUMMARY:
 *   Algorithmic stablecoins bootstrap liquidity and price stability through
 *   token incentive programs that reward early liquidity providers, arbitrage
 *   traders, and protocol participants. The constraint arises from a
 *   structural tension: the protocol requires continuous capital inflows and
 *   maintained peg to sustain value, but the incentive structure
 *   systematically extracts value from late entrants once bootstrap
 *   conditions are satisfied. Early participants (token holders, developers,
 *   arbitrage specialists) benefit from upside appreciation and yield
 *   capture; late entrants bear asymmetric risk when token incentives
 *   collapse and underlying demand proves insufficient to maintain peg. The
 *   constraint exhibits classification across all six types depending on
 *   observer perspective: early participants see pure coordination (Rope),
 *   arbitrage traders exploit the system while helping maintain coordination
 *   (Tangled Rope), late entrants face pure extraction (Snare), the ecosystem
 *   bears system risk (Snare), reserve stability claims are theatrical
 *   (Piton), some believe bootstrap is a temporary necessity (Scaffold), and
 *   some economists see immutable limits to algorithmic stabilization
 *   (Mountain).
 *
 * KEY AGENTS:
 *   - Early Token Holders: Primary beneficiary (institutional/arbitrage) — capture token appreciation and yield during bootstrap phase; can exit before collapse
 *   - Protocol Developers: Primary beneficiary (institutional/arbitrage) — secure funding and community value from token allocation; aligned with bootstrap success narrative
 *   - Arbitrage Traders: Secondary beneficiary (powerful/mobile) — systematically extract protocol reserves through trades that maintain peg while capturing spread; exit before depeg
 *   - Late Retail Depositors: Primary victim (powerless/trapped) — enter after bootstrap phase, lured by stability narrative; trapped by illiquidity once depeg begins
 *   - Ecosystem Stability: Secondary victim (powerless/trapped) — system risk concentrates in DeFi ecosystem; cascading failures harm other protocols
 *   - Ecosystem Growth Coalition: Organized agents (organized/constrained) — believe bootstrap is temporary; expect transition to organic demand or endogenous stability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_stablecoin_bootstrap_extractiveness, 0.68).
domain_priors:suppression_score(algorithmic_stablecoin_bootstrap_extractiveness, 0.72).
domain_priors:theater_ratio(algorithmic_stablecoin_bootstrap_extractiveness, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_stablecoin_bootstrap_extractiveness, extractiveness, 0.68).
narrative_ontology:constraint_metric(algorithmic_stablecoin_bootstrap_extractiveness, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(algorithmic_stablecoin_bootstrap_extractiveness, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_stablecoin_bootstrap_extractiveness, snare).
narrative_ontology:human_readable(algorithmic_stablecoin_bootstrap_extractiveness, "Algorithmic Stablecoin Bootstrap Extractiveness").
narrative_ontology:topic_domain(algorithmic_stablecoin_bootstrap_extractiveness, "cryptoeconomics/defi/monetary_policy").

domain_priors:requires_active_enforcement(algorithmic_stablecoin_bootstrap_extractiveness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_stablecoin_bootstrap_extractiveness, early_token_holders).
narrative_ontology:constraint_beneficiary(algorithmic_stablecoin_bootstrap_extractiveness, protocol_developers).
narrative_ontology:constraint_beneficiary(algorithmic_stablecoin_bootstrap_extractiveness, arbitrage_traders).
narrative_ontology:constraint_victim(algorithmic_stablecoin_bootstrap_extractiveness, late_entrants).
narrative_ontology:constraint_victim(algorithmic_stablecoin_bootstrap_extractiveness, retail_depositors).
narrative_ontology:constraint_victim(algorithmic_stablecoin_bootstrap_extractiveness, ecosystem_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LATE RETAIL DEPOSITOR (SNARE) — Enters the protocol after initial phases, trapped by apparent stability narrative. Cannot exit without realizing losses once the depeg begins. Bears full extraction cost as bootstrap incentives collapse. No meaningful agency or alternative.
constraint_indexing:constraint_classification(algorithmic_stablecoin_bootstrap_extractiveness, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EARLY TOKEN HOLDERS AND DEVELOPERS (ROPE) — Experience the constraint as pure coordination: bootstrapping liquidity for the stablecoin through token incentives solves a genuine cold-start problem. Net beneficiaries with exit optionality (arbitrage). Incentive structure aligns with protocol success narrative.
constraint_indexing:constraint_classification(algorithmic_stablecoin_bootstrap_extractiveness, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ARBITRAGE TRADERS (TANGLED ROPE) — Exploit the peg maintenance mechanism and token incentives. Coordination function: liquidity provision during volatile periods. Extraction: systematic extraction of protocol reserves through arbitrage while maintaining superficial peg. Mobile exit once depeg becomes inevitable.
constraint_indexing:constraint_classification(algorithmic_stablecoin_bootstrap_extractiveness, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: ECOSYSTEM GROWTH COALITION (SCAFFOLD) — Sees bootstrap extractiveness as temporary coordination mechanism with sunset. Incentives are designed to decline as protocol matures. Meaningful agency through governance participation; sunset path clear (transition to endogenous stabilization mechanisms).
constraint_indexing:constraint_classification(algorithmic_stablecoin_bootstrap_extractiveness, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: RESERVE STABILIZATION THEATER (PITON) — The advertised reserve mechanism and stability guarantees are largely theatrical. Actual stabilization relies on token incentive flows and sustained new capital, not genuine reserve backing. Theater persists through institutional inertia and marketing narrative.
constraint_indexing:constraint_classification(algorithmic_stablecoin_bootstrap_extractiveness, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ECOSYSTEM STABILITY COMMONS (SNARE) — Abstract collective good that bears the cost of cascading depegs and protocol failure. System risk that accrues to the broader DeFi ecosystem and retail participants cannot organize to exit or protect themselves.
constraint_indexing:constraint_classification(algorithmic_stablecoin_bootstrap_extractiveness, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / MONETARY ECONOMICS VIEW (MOUNTAIN) — From a civilizational perspective on monetary systems, algorithmic stablecoins face an immutable constraint: they require continuous growth and external capital to maintain peg in the face of redemption pressure. This perspective risks naturalizing what is actually a contingent design choice — seeing Ponzi-like capital flow requirements as inherent to the form rather than as a specific failure mode.
constraint_indexing:constraint_classification(algorithmic_stablecoin_bootstrap_extractiveness, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_stablecoin_bootstrap_extractiveness_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_stablecoin_bootstrap_extractiveness, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_stablecoin_bootstrap_extractiveness, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_stablecoin_bootstrap_extractiveness, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_stablecoin_bootstrap_extractiveness, TR),
    TR >= 0.70.

:- end_tests(algorithmic_stablecoin_bootstrap_extractiveness_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. The baseline extractiveness reflects the asymmetric value transfer from late entrants to early participants. The measurement trajectory (0.22 → 0.68 over 6 periods) shows classic bootstrap pattern: early phase low extractiveness as legitimacy builds, middle phase moderate extractiveness as capital inflows grow and incentives accumulate, late phase high extractiveness as token incentives collapse and depeg becomes visible. The highest values reflect the period when late entrants are maximally trapped — token incentives have stopped growing, but depeg has not yet occurred, leaving exit illiquid. Suppression (0.72): Very high. Multiple suppression mechanisms operate: (1) Informational asymmetry — reserve adequacy and sustainability are obscured by marketing; (2) Coordination failure — late entrants cannot organize or exit collectively; (3) Liquidity illusion — apparent stability through maintained peg prevents accurate risk assessment; (4) Capital lock-in — illiquidity prevents rapid exit; (5) Sunk cost attachment — individuals who have experienced gains resist loss realization. Theater ratio (0.58): Moderate-high. Reserve stability claims (published reserve metrics, peg maintenance narratives, 'backed by assets' framing) are partially theatrical — actual stabilization relies on continuous token incentive flows. The theater component increases as bootstrap matures: marketing emphasis on stability grows while actual stability mechanisms weaken.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. Early participants see a legitimate bootstrapping mechanism (Rope from institutional/arbitrage perspective). Arbitrage traders explicitly exploit the system while providing liquidity (Tangled Rope — mixed coordination and extraction). Late entrants face pure extraction with no exit (Snare — powerless/trapped). The ecosystem bears uncompensated risk (Snare — abstract collective). Reserve claims are theatrical (Piton — performative stability narrative). Some believe the system can mature past bootstrap (Scaffold — organized participants see sunset). The analytical observer risks seeing inherent monetary constraints (Mountain) when actually observing contingent design failures. The perspectival gap is extreme because the constraint's legitimacy genuinely differs across entry cohorts and time horizons.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position within the extraction flow. Early token holders benefit from the constraint (d ≈ 0.10) — arbitrage exit options, low suppression experienced, institutional positioning. Late depositors bear costs (d ≈ 0.92) — trapped exit, high suppression, powerless positioning. Arbitrage traders sit asymmetrically (d ≈ 0.45) — they benefit from token flows but are also extracting from the protocol itself, so their relationship is mixed. The ecosystem stability commons (d ≈ 0.95) is maximally targeted — trapped against cascading risk, powerless to organize. The analytical observer at civilizational scope may naturalize the constraint as inherent to algorithmic money (d ≈ 0.72 as canonical analytical), but structural data reveals this as false summit: the extractiveness derives from contingent design choices (infinite token issuance, reserve inadequacy) not from immutable monetary economics.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint avoids mandatrophy by accurately decomposing the bootstrap mechanism into components with different structural functions. (1) COORDINATION COMPONENT: genuine cold-start problem requiring liquidity — this is real and legitimate. (2) EXTRACTION COMPONENT: asymmetric value transfer from late entrants to early beneficiaries — this is also real and illegitimate. The claimed_type (Snare) prioritizes the extraction component because it governs terminal outcomes: late entrants are trapped regardless of whether early coordination was real. The Rope and Tangled Rope perspectives capture the coordination function from beneficiary/mixer positions but do not override the snare classification because the extraction is structural, not contingent. The false mountain (natural law view) is caught by the false summit detector — if the analytical observer were correct about immutable constraints, the protocol could not be redesigned; but algorithmic stablecoins with different incentive structures and reserve mechanics exist, proving the constraint is contingent. The mandatrophy is resolved by (a) accepting multiple types as legitimate from different positions, (b) prioritizing the type that captures late-stage terminal outcomes over types that capture early-stage intentions, and (c) flagging the mountain as a false summit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bootstrap_sustainability_threshold,
    'At what size of total value locked (TVL) does token incentive bootstrapping become mathematically unsustainable relative to organic demand?',
    'Comparative analysis of successful stablecoin adoption curves; identification of the ratio between incentivized liquidity and organic usage volume at depeg events',
    'If threshold is low (below $500M TVL): bootstrap extractiveness is inherent to the design. If threshold is high: extractiveness derives from scale and duration of incentive program, not from the core mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bootstrap_sustainability_threshold, empirical, 'Sustainable TVL threshold for token-incentivized bootstrap').

omega_variable(
    reserve_backing_adequacy,
    'What fraction of the stablecoin supply is genuinely backed by protocol reserves versus capital inflows, at the moment of bootstrap completion?',
    'Reserve accounting at multiple time points; reconstruction of actual reserve coverage ratios from on-chain data; comparison to stated reserve metrics',
    'If reserves cover >60% of supply at bootstrap completion: extractiveness is moderate. If reserves cover <20%: extractiveness is extreme (pure capital flow scheme).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reserve_backing_adequacy, empirical, 'Actual reserve backing percentage at bootstrap end').

omega_variable(
    endogenous_stability_mechanism_viability,
    'Can any algorithmic stablecoin transition from exogenous token incentives to endogenous stabilization (e.g., demand-driven collateral ratio adjustment) without either depeg or extreme volatility?',
    'Historical analysis of stablecoin transitions; identification of successful endogenous stabilization implementations or structural impossibility proofs',
    'If viable: scaffold classification holds; sunset is real. If nonviable: scaffold is aspirational; the constraint is perpetual snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(endogenous_stability_mechanism_viability, empirical, 'Whether endogenous stabilization can replace exogenous incentives').

omega_variable(
    late_entrant_awareness,
    'What fraction of late-stage retail depositors are aware of the bootstrap extraction mechanism and actively choose to participate despite it?',
    'User surveys; analysis of participant behavior in relation to protocol communication and reserve metrics; comparison of user retention before/after depeg across cohorts',
    'If awareness is high (>50%): users are constrained but informed, justifying tangled rope. If awareness is low (<20%): users are trapped through informational capture, justifying snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(late_entrant_awareness, empirical, 'Late-entrant awareness of bootstrap extraction mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_stablecoin_bootstrap_extractiveness, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(algostab_tr_t0, algorithmic_stablecoin_bootstrap_extractiveness, theater_ratio, 0, 0.35).
narrative_ontology:measurement(algostab_tr_t2, algorithmic_stablecoin_bootstrap_extractiveness, theater_ratio, 2, 0.44).
narrative_ontology:measurement(algostab_tr_t4, algorithmic_stablecoin_bootstrap_extractiveness, theater_ratio, 4, 0.52).
narrative_ontology:measurement(algostab_tr_t6, algorithmic_stablecoin_bootstrap_extractiveness, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(algostab_be_t0, algorithmic_stablecoin_bootstrap_extractiveness, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(algostab_be_t2, algorithmic_stablecoin_bootstrap_extractiveness, base_extractiveness, 2, 0.38).
narrative_ontology:measurement(algostab_be_t4, algorithmic_stablecoin_bootstrap_extractiveness, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(algostab_be_t6, algorithmic_stablecoin_bootstrap_extractiveness, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_stablecoin_bootstrap_extractiveness, resource_allocation).
narrative_ontology:affects_constraint(algorithmic_stablecoin_bootstrap_extractiveness, defi_liquidity_mining_extractiveness).
narrative_ontology:affects_constraint(algorithmic_stablecoin_bootstrap_extractiveness, cryptocurrency_reserve_adequacy).

% DUAL FORMULATION NOTE:
% The algorithmic stablecoin bootstrap constraint decomposes into two structurally distinct claims: (1) bootstrap_cold_start_necessity (ε=0.15, Rope) — liquidity mining addresses genuine network effects problem; (2) algorithmic_stablecoin_bootstrap_extractiveness (ε=0.68, Snare) — the specific implementation transfers asymmetric value to early participants. The stories are linked because bootstrap necessity does not imply the observed extractiveness level — alternative bootstrap designs with lower extractiveness exist.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algorithmic_stablecoin_bootstrap_extractiveness, institutional, 0.18).
constraint_indexing:directionality_override(algorithmic_stablecoin_bootstrap_extractiveness, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
