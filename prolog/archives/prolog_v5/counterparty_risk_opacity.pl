% ============================================================================
% CONSTRAINT STORY: counterparty_risk_opacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_counterparty_risk_opacity, []).

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
 *   constraint_id: counterparty_risk_opacity
 *   human_readable: Counterparty Risk Opacity in Financial Markets
 *   domain: finance/systemic_risk
 *
 * SUMMARY:
 *   Counterparty risk opacity in financial markets creates a structural
 *   tension between the operational necessity of decentralized credit markets
 *   and the systemic danger of unmeasured interconnection. When one financial
 *   institution cannot reliably assess the solvency or leverage of its
 *   counterparties, it faces two costs: the direct cost of verification
 *   (which is high for complex derivatives and off-balance-sheet exposures)
 *   and the indirect cost of unmonitored contagion risk. This constraint
 *   exhibits tension between what appears as immutable (information asymmetry
 *   is inherent to markets) and what is contingent policy (the degree of
 *   opacity is a choice). The extractiveness trajectory (0.38 → 0.58 over
 *   interval) reflects increasing leverage concentration and complexity in
 *   derivatives markets outpacing regulatory disclosure capacity. The theater
 *   ratio rise (0.52 → 0.68) indicates that regulatory oversight increasingly
 *   consists of performative reporting rather than genuine verification —
 *   stress tests and capital ratios persist despite evidence that they fail
 *   to predict stress scenarios.
 *
 * KEY AGENTS:
 *   - Interconnected Debtor: Primary victim (powerless/trapped) — participants in OTC derivatives networks with no ability to verify counterparty leverage; bears full contagion cost
 *   - Opacity-Maintaining Institutions: Primary beneficiary (institutional/arbitrage) — major dealers and asset managers who profit from information asymmetry and market-making spreads; can assess peer risk through privileged information channels
 *   - Risk Arbitrage Traders: Secondary beneficiary (powerful/mobile) — specialized traders who extract value from opacity through leveraged bets on hidden exposures; can exit the market when information advantages decline
 *   - Regional Banks: Secondary victim (moderate/constrained) — face barriers to counterparty verification due to cost and complexity; also benefit from opacity on their own balance sheet, creating ambiguous exit position
 *   - Transparency Mandate Coalition: Organized agents (organized/mobile) — regulators, central banks, and open-finance advocates building alternative infrastructures (CBDC, trade repositories, centralized clearing)
 *   - Regulatory Reporting Theater: Institutional actor (institutional/arbitrage) — maintains stress testing and capital adequacy frameworks that provide appearance of oversight without genuine verification capacity
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent market structures as inherent properties of finance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(counterparty_risk_opacity, 0.58).
domain_priors:suppression_score(counterparty_risk_opacity, 0.65).
domain_priors:theater_ratio(counterparty_risk_opacity, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(counterparty_risk_opacity, extractiveness, 0.58).
narrative_ontology:constraint_metric(counterparty_risk_opacity, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(counterparty_risk_opacity, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(counterparty_risk_opacity, tangled_rope).
narrative_ontology:human_readable(counterparty_risk_opacity, "Counterparty Risk Opacity in Financial Markets").
narrative_ontology:topic_domain(counterparty_risk_opacity, "finance/systemic_risk").

domain_priors:requires_active_enforcement(counterparty_risk_opacity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(counterparty_risk_opacity, opacity_maintaining_institutions).
narrative_ontology:constraint_beneficiary(counterparty_risk_opacity, risk_arbitrage_traders).
narrative_ontology:constraint_victim(counterparty_risk_opacity, counterparty_exposure_agents).
narrative_ontology:constraint_victim(counterparty_risk_opacity, systemic_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INTERCONNECTED DEBTOR (SNARE) — Structurally trapped in counterparty networks with no ability to verify or exit exposure. Bears full cost of hidden leverage, off-balance-sheet derivatives, and contagion risk. Cannot organize or escape without abandoning financial participation entirely.
constraint_indexing:constraint_classification(counterparty_risk_opacity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL BANK (TANGLED ROPE) — Faces high cost to verify counterparty risk (requires expensive analytics, legal review, collateral assessment). Benefits from opacity through reduced disclosure burden on its own balance sheet. Extraction asymmetry is real but not total — regulatory pressure and client relationships create some transparency incentives.
constraint_indexing:constraint_classification(counterparty_risk_opacity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INFORMATION-PRIVILEGED DEALER (ROPE) — Major financial institutions with market-making operations and client flows benefit from opacity as pure coordination mechanism. Opacity enables their primary function: profiting from information asymmetry while maintaining liquidity provision. Low perceived extraction because their institutional design assumes opacity.
constraint_indexing:constraint_classification(counterparty_risk_opacity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TRANSPARENCY MANDATE COALITION (SCAFFOLD) — Post-2008 regulatory reforms (DODD-FRANK, EMIR, CSPR) create alternatives to opacity: centralized clearing, trade repositories, standardized derivatives. These have sunset logic: as distributed ledger technology and real-time settlement mature, the information advantage from opacity declines. Organized agents see opacity as a temporary coordination failure, not inherent.
constraint_indexing:constraint_classification(counterparty_risk_opacity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY REPORTING THEATER (PITON) — Stress testing, capital adequacy ratios, and counterparty exposure disclosure frameworks persist despite low functional verification capacity. Regulators cannot genuinely assess whether reported exposures match market reality — testing is largely performative. The theater is maintained through institutional inertia and the fiction that regulatory reports provide real oversight.
constraint_indexing:constraint_classification(counterparty_risk_opacity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a pure information-theoretic perspective, counterparty risk opacity appears immutable: any agent cannot know the true state of another agent's portfolio in real time. Information asymmetry is inherent to decentralized finance. However, this naturalizes contingent institutional choices — the degree of opacity is a policy parameter, not a law of nature. This perspective risks false summit classification.
constraint_indexing:constraint_classification(counterparty_risk_opacity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(counterparty_risk_opacity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(counterparty_risk_opacity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(counterparty_risk_opacity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(counterparty_risk_opacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(counterparty_risk_opacity, TR),
    TR >= 0.70.

:- end_tests(counterparty_risk_opacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Substantial and rising. The constraint enables information-privileged institutions to capture value from counterparty risk that less-informed agents cannot price. Initial value (0.38) reflects baseline information advantage; terminal value (0.58) reflects concentration of derivatives trading in a shrinking dealer network that can self-assess risk through cross-client information flows. The increase is real — market structure changes have made opacity more extractive, not less. Suppression (0.65): High. Barriers to exit and verification include: (1) OTC derivatives settlement requires counterparty relationships with major dealers who control opacity; (2) alternative clearing venues have inferior liquidity and higher trading costs; (3) regulatory frameworks incentivize opacity through proprietary treatment of client lists and trading positions; (4) technological barriers (legacy settlement systems, firm-specific derivatives) create switching costs. Theater ratio (0.68): Moderately high and rising. Regulatory stress testing (CCAR, DFAST) provides appearance of capital adequacy verification without genuine reconstruction of market-wide leverage and contagion pathways. Regulators cannot independently verify whether reported exposures match reality — test exercises are based on firm submissions without direct market data access. Post-2008 regulatory momentum created theater institutions (Basel III reporting, LIBOR reform) that maintain appearance of control over opacity.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits dramatic perspectival divergence. Information-privileged dealers see opacity as pure coordination (Rope) — they are managing counterparty risk through information flows, and opacity enables their liquidity function. The organized transparency coalition sees opacity as temporary (Scaffold) — centralized clearing and real-time settlement reduce information advantage from opacity. Regional banks see mixed extraction and coordination (Tangled Rope) — opacity costs them verification expense but benefits their own non-disclosure, so exit is ambiguous. The interconnected debtor sees pure extraction (Snare) — unable to verify exposure, trapped in networks of major dealers. The regulatory theater sees its own role as degraded (Piton) — oversight mechanisms persist despite acknowledged limited capacity to verify leverage. The analytical observer risks a false mountain — treating market information asymmetry as an immutable law rather than a design choice. The perspectival gap is wide because the structural position relative to opacity determines whether the agent experiences it as profitable (dealers), costly (debtors), temporary (reformers), or performative (regulators).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) range from 0.05 (information-privileged institutional beneficiaries with full information advantage and arbitrage exit) to 0.95 (trapped debtors with no independent verification capacity and systemic exposure). The sigmoid function f(d) amplifies the directionality gap: beneficiaries with arbitrage exit experience negative effective extraction (coordination benefit), while trapped debtors with no verification exit experience maximum extraction near 1.42. The piton classification derives from the high theater ratio (0.68), not from high chi — the regulatory reporting apparatus appears substantial but provides minimal actual counterparty risk monitoring capacity. Regional banks occupy the constrained middle: they face real verification costs that suppress their exit options, but they also benefit from opacity on their own disclosures, creating an ambiguous position reflected in the Tangled Rope classification at the moderate power level.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    opacity_intentionality_threshold,
    'What proportion of observed opacity is intentional obfuscation versus unavoidable information lags?',
    'Forensic analysis of clearing data, trade repository records, and internal communications during crisis periods; comparison of disclosed vs revealed exposures post-default',
    'If intentional > 70%: classification remains Snare from powerless perspective. If intentional < 40%: may downgrade to Tangled Rope (legitimate coordination costs justify asymmetry)',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opacity_intentionality_threshold, empirical, 'Proportion of opacity from intentional obfuscation vs information lags').

omega_variable(
    distributed_ledger_viability,
    'Can distributed ledger technology (blockchain-based settlement, real-time gross settlement) eliminate counterparty risk opacity without unacceptable performance degradation?',
    'Implementation trials (CBDC clearing networks, tokenized settlement); measurement of transaction throughput, latency, and systemic stability under stress conditions',
    'If viable: scaffold sunset is plausible, opacity becomes temporary extractive mechanism. If not viable: opacity persists as structural feature, classification shifts toward Mountain',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(distributed_ledger_viability, empirical, 'Whether distributed ledger can replace opacity-dependent clearing').

omega_variable(
    regulatory_forbearance_cycle,
    'Does regulatory pressure systematically reduce opacity during crisis periods and relax during stable periods, creating a cyclical extraction rhythm?',
    'Time series analysis of regulatory disclosure requirements and enforcement intensity; correlation with financial stability indices and proprietary trade volume',
    'If cyclical: piton classification is accurate (enforcement theater that oscillates). If monotonic: classification may shift toward Rope (stable institutional coordination mechanism)',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_forbearance_cycle, empirical, 'Cyclical nature of regulatory enforcement versus opacity').

omega_variable(
    contagion_risk_measurability,
    'Is systemic contagion risk from opacity fundamentally unmeasurable, or is our inability to measure it a contingent limitation of current analytics?',
    'Agent-based modeling of counterparty networks under opacity vs full transparency; validation against realized contagion patterns from past crises (2008, 2020)',
    'If fundamentally unmeasurable: Mountain classification of ''inherent uncertainty'' is justified. If measurable: opacity becomes strategic choice, supporting Snare/Tangled Rope classifications',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contagion_risk_measurability, conceptual, 'Whether systemic contagion risk is fundamentally unmeasurable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(counterparty_risk_opacity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cprty_tr_t0, counterparty_risk_opacity, theater_ratio, 0, 0.52).
narrative_ontology:measurement(cprty_tr_t5, counterparty_risk_opacity, theater_ratio, 5, 0.62).
narrative_ontology:measurement(cprty_tr_t10, counterparty_risk_opacity, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(cprty_be_t0, counterparty_risk_opacity, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(cprty_be_t5, counterparty_risk_opacity, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(cprty_be_t10, counterparty_risk_opacity, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(counterparty_risk_opacity, resource_allocation).
narrative_ontology:affects_constraint(counterparty_risk_opacity, liquidity_illusion_in_derivatives).
narrative_ontology:affects_constraint(counterparty_risk_opacity, systemic_contagion_fragility).
narrative_ontology:affects_constraint(counterparty_risk_opacity, regulatory_forbearance_cycle).

% DUAL FORMULATION NOTE:
% Counterparty risk opacity is upstream of specific contagion scenarios. The structural ambiguity about opacity (coordinating mechanism vs extraction device) cascades to market liquidity claims (which rest on information asymmetry) and systemic stability assessment (which assumes opacity is knowable). Decomposition: information_standard_clearing (ε≈0.12, Rope) vs counterparty_leverage_hiding (ε≈0.72, Snare) are separable constraints in the same market structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(counterparty_risk_opacity, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
