% ============================================================================
% CONSTRAINT STORY: soe_property_bailout
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_soe_property_bailout, []).

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
 *   constraint_id: soe_property_bailout
 *   human_readable: State-Directed Purchase of Distressed Real Estate Assets
 *   domain: economic/real_estate_policy
 *
 * SUMMARY:
 *   China's state-directed purchase of distressed real estate assets
 *   represents a policy intervention where SOEs are mandated to acquire
 *   foreclosed or failing properties from private developers to stabilize the
 *   financial system and prevent cascading defaults. This constraint exhibits
 *   the full spectrum of DR classifications depending on the observer's
 *   structural position. From the perspective of SOE management and system
 *   stabilizers, the policy is a coordination mechanism (Rope) that prevents
 *   financial collapse. From the perspective of private workers displaced by
 *   developer failures, it is pure extraction (Snare). From the perspective
 *   of foreign investors assessing risk, it exhibits both coordination
 *   benefits and extraction asymmetries (Tangled Rope). The constraint's
 *   theater ratio (0.65) reflects that formal purchase directives and
 *   restructuring announcements maintain institutional appearance while
 *   underlying asset recovery becomes increasingly degraded. The
 *   extractiveness (0.58) indicates moderate-to-high extraction: SOEs absorb
 *   distressed properties at below-market rates under state compulsion,
 *   accumulating liabilities that generate long-term shareholder losses and
 *   constrain private development alternatives.
 *
 * KEY AGENTS:
 *   - State-Owned Enterprises: Primary beneficiary (institutional/arbitrage) — receive directive protection, reduced market competition, state-guaranteed purchase authority
 *   - Private Developers: Secondary beneficiary/victim (moderate/constrained) — receive liquidity rescue but face subordinated position, limited development rights, below-market asset sale prices
 *   - Displaced Private Workers: Primary victim (powerless/trapped) — bear labor market contraction, geographic immobility, housing market subordination to SOE purchases
 *   - Future Taxpayers: Structural victim (powerless/trapped) — will bear cost of SOE portfolio losses through future fiscal consolidation or inflation
 *   - Financial System Stability Apparatus: Organized beneficiary (organized/constrained) — central bank, banking regulators, macro-prudential authorities stabilize systemic risk through SOE absorption
 *   - Foreign Investors: Secondary victim (powerful/mobile) — face extraction risk through asset seizure, ownership restrictions, but retain exit option through capital flight
 *   - SOE Asset Management Bureau: Institutional maintainer (institutional/arbitrage) — sustains performative bureaucratic structure (piton perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(soe_property_bailout, 0.58).
domain_priors:suppression_score(soe_property_bailout, 0.68).
domain_priors:theater_ratio(soe_property_bailout, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(soe_property_bailout, extractiveness, 0.58).
narrative_ontology:constraint_metric(soe_property_bailout, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(soe_property_bailout, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(soe_property_bailout, tangled_rope).
narrative_ontology:human_readable(soe_property_bailout, "State-Directed Purchase of Distressed Real Estate Assets").
narrative_ontology:topic_domain(soe_property_bailout, "economic/real_estate_policy").

domain_priors:requires_active_enforcement(soe_property_bailout).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(soe_property_bailout, state_owned_enterprises).
narrative_ontology:constraint_beneficiary(soe_property_bailout, financial_system_stability).
narrative_ontology:constraint_beneficiary(soe_property_bailout, developer_investor_class).
narrative_ontology:constraint_victim(soe_property_bailout, soe_shareholders).
narrative_ontology:constraint_victim(soe_property_bailout, private_workers_displaced).
narrative_ontology:constraint_victim(soe_property_bailout, future_taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED PRIVATE WORKER (SNARE) — Unable to exit the labor market or housing market; bears full cost of SOE-driven distress purchasing. Worker productivity is extracted to service debt on properties purchased at inflated prices by SOEs following state directive. No alternative employment or housing options in planned purchase zones. Maximum experienced extraction — trapped within geographic and sectoral constraints.
constraint_indexing:constraint_classification(soe_property_bailout, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRIVATE DEVELOPER (TANGLED ROPE) — Constrained but not trapped. State purchases distressed assets, preventing complete firm failure (coordination benefit: liquidity injection, continuation of operations). Simultaneously extracts: SOE purchase prices are often below market, state has monopoly on remaining buyer pool, future development rights are restricted. Developer experiences both rescue and subordination — genuine coordination function paired with asymmetric extraction.
constraint_indexing:constraint_classification(soe_property_bailout, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: STATE-OWNED ENTERPRISE (ROPE) — Primary beneficiary with full arbitrage exit. Directed to purchase distressed properties at below-market rates, reducing systemically risky defaults, stabilizing credit cascades. SOE views constraint as coordination: preventing financial system collapse generates institutional survival benefit for all SOE peers. Purchase directives solve collective action problem (no individual SOE would voluntarily absorb distressed assets) through state mandate. Net beneficiary — extraction flows toward SOE, not away.
constraint_indexing:constraint_classification(soe_property_bailout, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MACROECONOMIC STABILIZATION COALITION (SCAFFOLD) — Organized agents (central government, banking regulators, urban planners) see property bailout as temporary intervention with an exit path: SOE absorption of distressed assets buys time for market normalization, debt restructuring, and private sector recovery. Policy is explicitly temporary — contingent on developer recovery or asset liquidation. Theater is low relative to snare perspective: the goal is system stability, not performative rhetoric. Sunset clause implicit in macroeconomic assumptions: as property markets stabilize and credit conditions ease, state directive pressure should reduce.
constraint_indexing:constraint_classification(soe_property_bailout, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: SOE ASSET MANAGEMENT BUREAU (PITON) — The formal institutional mechanism for state property purchase has largely become performative. Original function (market-stabilizing intervention) is degraded — SOEs now hold massive portfolios of distressed properties with minimal resale prospects. Holdings accumulate faster than markets can absorb; asset values decline toward zero. Institutional ritual persists (quarterly purchase directives, portfolio reporting, restructuring announcements) but primary function has atrophied. Theater ratio high because the system sustains itself through formal procedures and bureaucratic theater rather than actual asset recovery or market clearing.
constraint_indexing:constraint_classification(soe_property_bailout, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: FOREIGN INVESTOR / CAPITAL MARKETS OBSERVER (TANGLED ROPE) — Sees both coordination function and extraction asymmetry from outside. State intervention prevents cascading defaults (coordination benefit prevents systemic risk that would spread to global markets). Simultaneously, foreign capital faces extraction: asset seizure risk, restrictions on foreign property ownership, reduced transparency in SOE portfolio decisions. Foreign investors have exit option (arbitrage out of China markets) but face extraction if they remain. Experienced extraction is high but mobile — can divest and leave, unlike trapped domestic workers.
constraint_indexing:constraint_classification(soe_property_bailout, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risk of naturalization: from civilizational view, property market cycles are inherent to capitalism; state absorption of distressed assets is an immutable feature of post-crisis stabilization. This perspective risks treating contingent policy choice as structural law. The engine should flag this as a false summit: the policy decision to use SOEs as property absorbers is not a law of nature, but a specific institutional arrangement that could be replaced by alternative mechanisms (market clearing prices, bankruptcy law, foreign investment, etc.).
constraint_indexing:constraint_classification(soe_property_bailout, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(soe_property_bailout_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(soe_property_bailout, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(soe_property_bailout, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(soe_property_bailout, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(soe_property_bailout, TR),
    TR >= 0.70.

:- end_tests(soe_property_bailout_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The state directive extracts value from SOE shareholders (forced absorption of distressed assets at below-market prices), from future taxpayers (portfolio losses will require fiscal consolidation or inflation), and from displaced workers (labor market contraction). The extraction is not maximal (not 0.75+) because the policy generates genuine coordination benefits: preventing financial system collapse has positive spillovers that partially offset the forced asset absorption. Theater ratio (0.65) reflects institutional drift: the original function (temporary market stabilization) is increasingly displaced by bureaucratic ritual. SOE asset portfolios accumulate faster than markets can absorb; quarterly purchase directives and restructuring announcements become performative as actual asset recovery slows. Suppression (0.68) captures the coercion mechanisms: SOE directives are mandatory (not voluntary coordination), workers lack labor market alternatives in distressed zones, private developers face monopoly buyer (state), foreign investors face ownership restrictions. Suppression is high but not total (not 0.80+) because some actors retain partial exit options (developers can restructure, foreign investors can divest).
 *
 * PERSPECTIVAL GAP:
 *   The 7-perspective structure reveals how a single policy generates contradictory classifications. The rope perspective (SOE beneficiary) experiences coordination (preventing defaults is genuine collective benefit). The snare perspective (trapped worker) experiences pure extraction (policy costs absorbed without consent or exit). The scaffold perspective (stabilization apparatus) experiences temporary intervention (sunset path through market normalization). The piton perspective (asset management bureau) experiences degraded ritual (performative bureaucracy displacing actual recovery). The analytical risk is mountainization: treating the policy as an immutable law of capitalism rather than a contingent institutional choice. This perspectival diversity is not ambiguity — it is structural reality. Different agents genuinely experience different constraint types because they occupy different positions in the extraction and coordination flows.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from each agent's position in the extraction flow. SOE management with arbitrage exit options and beneficiary status derives low d (approximately 0.15-0.25) — full beneficiary receiving policy protection. Private developers with constrained exit and mixed beneficiary/victim status derive moderate d (approximately 0.50-0.60) — receiving liquidity rescue but losing development autonomy. Displaced workers with trapped status and victim status derive high d (approximately 0.90-0.95) — bearing full extraction with zero exit options. Foreign investors with mobile exit and victim status derive moderate-high d (approximately 0.55-0.70) — facing extraction but able to divest. The automatic derivation from beneficiary/victim declarations and exit options produces these directionality values without explicit override. The engine then applies the sigmoid f(d) function to compute experienced extractiveness chi, which varies substantially across perspectives despite identical base properties.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The mandatrophy (extraction vs. coordination ambiguity) is resolved by decomposing the observed constraint into its structural components. The tangled_rope classification is correct because the policy exhibits BOTH genuine coordination (prevents financial system collapse) AND asymmetric extraction (SOE shareholders and workers absorb costs). The coordination function is real: without SOE absorption, cascading defaults would spread systemically. The extraction is also real: SOEs purchase below market prices under coercion, and workers bear labor market contraction. The mandatrophy resolves when we recognize that tangled_rope is not a compromise between rope and snare — it is a distinct type where coordination and extraction are simultaneously present and structurally necessary. The private developer receives liquidity rescue (coordination benefit) while losing development autonomy (extraction cost). The financial system receives stability (coordination) while workers absorb adjustment (extraction). The base classification of tangled_rope with extractiveness 0.58 captures this dual nature correctly. The false mountain (analytical observer risk) is caught by the engine's false summit detector: there is nothing immutable or natural about this policy choice — alternative mechanisms (bankruptcy law, foreign investment liberalization, private debt restructuring) exist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    soe_portfolio_recovery_threshold,
    'What timeline and price rebound threshold would constitute successful SOE portfolio recovery vs. permanent value destruction?',
    'Longitudinal tracking of SOE property holdings: market price appreciation vs. historical purchase prices; time-to-sale metrics; value-at-risk modeling under various property market scenarios',
    'If recovery timeline < 5 years and price rebound > 20%: policy successfully temporary (scaffold logic holds). If timeline > 15 years and rebound < 5%: policy has become permanent extraction structure (piton confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(soe_portfolio_recovery_threshold, empirical, 'Timeline and price threshold for SOE portfolio recovery').

omega_variable(
    private_developer_counterfactual,
    'How many private developers would have restructured successfully WITHOUT state directive purchases? (Counterfactual: what was the true insolvency rate vs. temporary illiquidity rate?)',
    'Historical analysis of similar financial crises in other jurisdictions with different intervention mechanisms; bankruptcy law analysis comparing outcomes under liquidation vs. state rescue; developer firm survival curves conditional on SOE intervention timing',
    'If most developers would have survived through private workouts: extraction is high, policy unnecessarily constrains private solutions. If developers face actual insolvency: coordination benefit is genuine, extraction is moderate due to value-preserving effect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(private_developer_counterfactual, empirical, 'True insolvency rate of developers absent state intervention').

omega_variable(
    soe_shareholder_loss_magnitude,
    'What is the magnitude of shareholder loss from SOE property holdings relative to total SOE equity and alternative investment returns?',
    'Financial analysis of SOE balance sheets; comparative return-on-assets for property vs. alternative state-directed investments; shareholder valuation impact studies',
    'If losses < 2% of equity: victims are diffuse (abstract shareholders, future taxpayers), constraint is primarily political/directional. If losses > 10% of equity: shareholder class becomes concrete victim, constraint transitions to clear snare from SOE shareholder perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(soe_shareholder_loss_magnitude, empirical, 'SOE shareholder loss magnitude from property bailout').

omega_variable(
    foreign_capital_exit_velocity,
    'Are foreign investors actually exiting Chinese property markets in response to state intervention, or do they perceive the intervention as stabilizing?',
    'Capital flow analysis: foreign FDI in Chinese real estate pre-intervention vs. post-intervention; sovereign wealth fund allocation decisions; international investor confidence indices',
    'If exit velocity high: foreign investors see extraction risk (snare/tangled_rope confirmed). If flows stable or increasing: foreign investors see stabilization benefit (rope logic holds).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreign_capital_exit_velocity, empirical, 'Foreign capital exit velocity in response to state intervention').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(soe_property_bailout, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soepb_tr_t0, soe_property_bailout, theater_ratio, 0, 0.35).
narrative_ontology:measurement(soepb_tr_t4, soe_property_bailout, theater_ratio, 4, 0.5).
narrative_ontology:measurement(soepb_tr_t8, soe_property_bailout, theater_ratio, 8, 0.65).

% Extraction over time
narrative_ontology:measurement(soepb_be_t0, soe_property_bailout, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(soepb_be_t4, soe_property_bailout, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(soepb_be_t8, soe_property_bailout, base_extractiveness, 8, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(soe_property_bailout, resource_allocation).
narrative_ontology:boltzmann_floor_override(soe_property_bailout, 0.42).
narrative_ontology:affects_constraint(soe_property_bailout, chinese_shadow_banking_exposure).
narrative_ontology:affects_constraint(soe_property_bailout, developer_debt_cascade).
narrative_ontology:affects_constraint(soe_property_bailout, local_government_fiscal_dependency).

% DUAL FORMULATION NOTE:
% SOE property bailout is downstream of specific developer insolvencies but represents a distinct structural constraint on resource allocation. The upstream constraints (developer leverage, shadow banking exposure) have their own extractiveness values reflecting financial fragility; the bailout constraint has its own extractiveness reflecting the state's monopoly on property absorption and the resulting imposition of costs on SOE shareholders and workers. The two constraints are linked: developer insolvency triggers bailout activation, but bailout structure is independently consequential for financial stability and labor market outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(soe_property_bailout, powerful, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
