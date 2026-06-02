% ============================================================================
% CONSTRAINT STORY: reserve_accumulation_race
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reserve_accumulation_race, []).

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
 *   constraint_id: reserve_accumulation_race
 *   human_readable: Reserve Accumulation Race Among Competing Sovereigns
 *   domain: macroeconomic_policy/monetary_systems
 *
 * SUMMARY:
 *   The reserve accumulation race represents a structural constraint in the
 *   international monetary system where competing sovereigns engage in
 *   competitive bidding for scarce foreign currency reserves to protect
 *   against currency crises and capital flight. This constraint exhibits
 *   asymmetric extraction: reserve-currency issuing nations (primarily the
 *   United States) capture substantial seigniorage benefits while
 *   reserve-scarce nations must export real goods and labor to accumulate
 *   reserves issued at near-zero cost. Simultaneously, the constraint has
 *   genuine coordination functions—reserve pooling through swap lines, IMF
 *   resources, and regional mechanisms do enable crisis prevention and
 *   capital-flow smoothing. The extractiveness has increased over the 20-year
 *   measurement interval (0.38 to 0.58) as emerging markets have accumulated
 *   larger reserve buffers and as capital-account volatility has increased
 *   post-2008. Theater ratio has remained below the piton threshold (0.48 vs
 *   0.70) because actual reserve functions (funding government deficits,
 *   smoothing external account shocks) are real, not purely ceremonial.
 *   However, the institutional mechanisms for managing adequate reserves—IMF
 *   consultations, reserve-adequacy guidelines—have increasingly performative
 *   character as they fail to predict actual crises.
 *
 * KEY AGENTS:
 *   - Reserve-Scarce Nations: Primary victims (powerless/trapped) — dependent on export earnings to build buffers; face currency crises without adequate reserves
 *   - Commodity Exporter Coalition: Secondary victims (organized/constrained) — have negotiating power but structurally trapped by commodity price volatility requiring larger buffers
 *   - Reserve-Currency Center: Primary beneficiary (institutional/arbitrage) — issues reserves at minimal cost; extraction flows inward as foreign nations demand its currency
 *   - Rising Reserve Aspirant: Mixed agent (powerful/mobile) — attempts to internationalize own currency; bears higher accumulation burden than established centers
 *   - Bretton Woods Institutional Framework: Institutional actor (institutional/arbitrage) — maintains reserve adequacy guidelines with declining predictive power (piton characteristics)
 *   - Regional Alternative Reserve Coalition: Organized agents (organized/constrained) — building alternative settlement mechanisms and reserve structures with sunset trajectory
 *   - Analytical Observer: Universalizing perspective (analytical/analytical) — risks naturalizing dollar-centric architecture as inherent to any international monetary system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reserve_accumulation_race, 0.58).
domain_priors:suppression_score(reserve_accumulation_race, 0.65).
domain_priors:theater_ratio(reserve_accumulation_race, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reserve_accumulation_race, extractiveness, 0.58).
narrative_ontology:constraint_metric(reserve_accumulation_race, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(reserve_accumulation_race, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reserve_accumulation_race, tangled_rope).
narrative_ontology:human_readable(reserve_accumulation_race, "Reserve Accumulation Race Among Competing Sovereigns").
narrative_ontology:topic_domain(reserve_accumulation_race, "macroeconomic_policy/monetary_systems").

domain_priors:requires_active_enforcement(reserve_accumulation_race).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reserve_accumulation_race, reserve_currency_issuers).
narrative_ontology:constraint_beneficiary(reserve_accumulation_race, capital_exporting_nations).
narrative_ontology:constraint_victim(reserve_accumulation_race, reserve_scarce_nations).
narrative_ontology:constraint_victim(reserve_accumulation_race, commodity_exporters).
narrative_ontology:constraint_victim(reserve_accumulation_race, global_monetary_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESERVE-SCARCE NATION (SNARE) — Structurally dependent on earning foreign currency through exports to build reserve buffers. No exit: cannot abandon reserve accumulation without exposing itself to currency crises, capital flight, and debt defaults. Bears the full extraction: forced to export real goods/labor in exchange for paper/digital reserves issued at near-zero cost by reserve-currency center. Maximum suppression and maximum experienced extractiveness — trapped in competitive race for scarce reserves.
constraint_indexing:constraint_classification(reserve_accumulation_race, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMMODITY EXPORTER COALITION (TANGLED ROPE) — Organized nations (OPEC, agricultural exporters) have some negotiating power but remain structurally trapped in reserve accumulation because commodity price volatility requires larger buffers. The constraint has genuine coordination function (reserve pooling, swap lines) alongside asymmetric extraction (terms of trade dictated by reserve-currency dynamics). Constrained exit: could coordinate alternative reserve systems (regional pools, SDR reliance) but face switching costs and retaliatory exclusion from dollar-denominated markets.
constraint_indexing:constraint_classification(reserve_accumulation_race, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: RESERVE-CURRENCY CENTER (ROPE) — Issues reserve currency at minimal cost; extraction flows inward as foreign nations send real goods in exchange for created money. Experiences the constraint as pure coordination benefit: other nations' reserve demand props up currency value and absorbs newly-created monetary base without inflation pressure (exporting inflation to the periphery). Arbitrage exit: can exit reserve-currency role but faces extreme reputational and financial costs, making arbitrage theoretical rather than real.
constraint_indexing:constraint_classification(reserve_accumulation_race, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: RISING RESERVE ASPIRANT (TANGLED ROPE) — Large emerging market attempting to internationalize its currency (e.g., China with Yuan, India with Rupee). Experiences the constraint as both coordination (benefits from participating in international settlement) and extraction (bears costs of building sufficient reserves to make currency believable as store of value). Mobile exit exists in principle (could abandon internationalization) but abandoning it costs geopolitical standing. Asymmetric extraction: must accumulate reserves faster than established centers to achieve credibility, bearing higher burden.
constraint_indexing:constraint_classification(reserve_accumulation_race, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: BRETTON WOODS INSTITUTIONAL FRAMEWORK (PITON) — IMF, World Bank, and formal reserve-system institutions persist through inertia despite degraded function. Nominally designed to manage reserve adequacy and prevent races, but actual verification of 'adequate' reserves is theater: IMF reserves-to-short-term-debt ratios are mathematical constructs that miss tail risks entirely. The institutions maintain legitimacy through procedural formality (reserve consultations, stability assessments) that have low predictive power for actual crises. High theater ratio masks minimal actual enforcement capacity.
constraint_indexing:constraint_classification(reserve_accumulation_race, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REGIONAL ALTERNATIVE RESERVE COALITION (SCAFFOLD) — Central banks coordinating regional settlement systems (ASEAN+3 CMIM, BRICS New Development Bank) see the dollar-centric race as temporary. Building alternative verification mechanisms (SDR-based accounting, regional swap lines, commodity-backed reserves) that reduce dependence on dollar accumulation. Constrained exit: switching costs and network externalities remain high, but coalitions have agency and a sunset clause: as regional systems mature (estimated 10-20 years), the extraction mechanism of forced dollar accumulation loses force.
constraint_indexing:constraint_classification(reserve_accumulation_race, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universalizing view, reserve accumulation follows from the logical structure of international currency systems: any system with asymmetric issuance generates competition for the scarce good (reserves). This appears immutable — inherent to how fiat money works when issued by competing sovereigns. The engine will flag this as a false summit: the 'inherent' structure is contingent on institutional choices (dollar-centric settlement, absence of global reserve issued by neutral authority). Alternative arrangements exist (SDR as global reserve unit, regional non-hierarchical settlement) but are suppressed by network externalities and institutional inertia.
constraint_indexing:constraint_classification(reserve_accumulation_race, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reserve_accumulation_race_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(reserve_accumulation_race, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reserve_accumulation_race, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(reserve_accumulation_race, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(reserve_accumulation_race, TR),
    TR >= 0.70.

:- end_tests(reserve_accumulation_race_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising. The reserve-accumulation race forces reserve-scarce nations to maintain buffers far larger than domestic transaction requirements (often 3-6 months of imports, or $100-400 billion for large developing economies) to prevent crisis-driven capital flight. This real resource commitment represents extraction: labor and goods are exported, reserves sit idle earning near-zero returns while reserve-currency center earns seigniorage on newly-issued money. The rising trend reflects accelerating capital-account volatility post-2008 and increasing buffer requirements. However, extractiveness is not extreme (< 0.70) because some reserve accumulation serves genuine liquidity needs and because alternative mechanisms (regional swap lines, SDR facilities) provide partial relief. Suppression (0.65): High. Nations have severe barriers to refusing participation: without adequate reserves, they face currency crises that trigger capital flight, debt defaults, IMF conditionality, and severe output loss. The barrier is structural (inherent to fiat money under capital-account openness) and enforced through market discipline (investors flee if reserves fall below psychological thresholds) rather than explicit coercion. Theater ratio (0.48): Moderate. Actual reserves serve real functions—funding government deficits during external shocks, signaling creditworthiness, enabling emergency central bank interventions. But institutional mechanisms for managing reserves are increasingly theatrical: IMF reserve adequacy ratios are mathematical constructs that miss tail-risk scenarios (2008 reserve levels didn't prevent crises); central banks maintain large buffers that are never used (self-insurance against worst cases); much theater involves procedural compliance (reserve consultations, stability assessments) with low predictive power. The theater ratio has increased from 0.35 to 0.48 as actual coordination function has remained flat while institutional performance-measurement has become more elaborate.
 *
 * PERSPECTIVAL GAP:
 *   The range from Snare (powerless/trapped) through Rope (institutional/arbitrage) to Scaffold (organized/constrained with sunset) reveals that the same extractive mechanism—competitive reserve accumulation—appears as immutable law to agents with no alternatives, as pure coordination benefit to the center, and as a temporary institutional problem to organized coalitions building alternatives. The perspectival gap is not measurement uncertainty but structural difference in experienced extractiveness depending on power level and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position. Reserve-scarce nations: trapped exit + victim status → d ≈ 0.92 → f(d) ≈ 1.38 (maximum experienced extraction). Reserve-currency center: arbitrage exit + beneficiary status → d ≈ 0.05 → f(d) ≈ -0.12 (negative experienced extraction—they benefit from the constraint). Rising aspirant: mobile exit + mixed status → d ≈ 0.65 → f(d) ≈ 1.00 (moderate extraction). Regional coalition: constrained exit + victim/beneficiary mixed → d ≈ 0.50 → f(d) ≈ 0.65 (moderate extraction, reduced by coordination gains). The effective extractiveness χ is further scaled by scope σ(S): at global scope, σ = 1.2, amplifying chi to reflect that systemic currency races propagate across all nations simultaneously.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that Tangled Rope classification is mandatory: the constraint simultaneously coordinates (reserve pooling, swap lines, IMF facilities enable collective crisis prevention) and extracts (reserve-scarce nations must export to accumulate reserves issued by center at minimal cost). Both functions are genuine. The temptation is to collapse to either 'this is just coordination' (erasing the asymmetric cost structure) or 'this is just extraction' (erasing the real crisis-prevention benefits of reserves). Tangled Rope captures both: the constraint is active enforcement (nations must accumulate reserves to survive), has beneficiaries (center, capital exporters) and victims (reserve-scarce nations, commodity exporters), and has real coordination function (stabilizes international settlement). The analytical false summit (mountain perspective) is specifically what mandatrophy guards against: the claim that reserve accumulation is 'inherent to fiat systems' naturalizes a contingent institutional architecture (dollar-centrality, absence of global reserve authority) as law of nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_limit_vs_institutional_choice,
    'Is reserve accumulation an inherent feature of any international currency system, or a contingent effect of dollar-centric architecture?',
    'Historical comparison: pre-1944 gold standard dynamics vs post-1973 fiat systems vs hypothetical SDR-based alternatives. Modeling: simulate multi-polar reserve system with symmetric issuance authority.',
    'If natural limit: mountain classification valid; constraint is unavoidable. If institutional: false summit detected; constraint is removable via currency architecture reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_limit_vs_institutional_choice, conceptual, 'Whether reserve accumulation is natural law or institutional contingency').

omega_variable(
    reserve_adequacy_measurement_validity,
    'Do standard IMF reserve adequacy metrics (3-6 months import cover, debt service ratios) actually predict protection against currency crises, or are they performative theater?',
    'Empirical study: correlate pre-crisis reserve levels (measured by standard metrics) against actual crisis probability and severity. Identify nations that followed guidelines but still crashed; nations that succeeded despite low reserves.',
    'If valid: Bretton Woods institutions provide genuine coordination. If theater: institutions are piton (degraded ritual), not functional rope; crisis risk remains high regardless of accumulated reserves.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reserve_adequacy_measurement_validity, empirical, 'Whether IMF reserve adequacy metrics predict crisis protection').

omega_variable(
    alternative_settlement_viability,
    'Can regional settlement systems and SDR-based accounting reduce dollar dependence without imposing excessive switching costs or counter-sanctions from the reserve-currency center?',
    'Track CMIM activation rates and cross-border SDR settlement volumes; model switching costs for large commodity/capital flows; analyze sanctions responses to alternative reserve initiatives.',
    'If viable: scaffold perspective confirmed; sunset is structural and real. If not viable: alternative pathways are aspiration; reserve race persists indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_settlement_viability, empirical, 'Whether alternative settlement systems can viably reduce dollar dependence').

omega_variable(
    extraction_flow_symmetry,
    'Does the reserve-currency center''s seigniorage gain equal the periphery''s reserve-accumulation loss, or are there asymmetric hidden costs/benefits?',
    'Full accounting: measure seigniorage (new reserve currency issued), capital flight prevention benefit, inflation export benefit. Compare against opportunity cost of reserves held by periphery, terms-of-trade effects, capital inflow requirements.',
    'If symmetric: constraint is pure redistribution (snare classification solid). If asymmetric hidden benefits flow to periphery: tangled rope classification stronger; some agents benefit more than zero-sum intuition suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_flow_symmetry, empirical, 'Symmetry of extraction flows in reserve accumulation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reserve_accumulation_race, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(res_acc_tr_t0, reserve_accumulation_race, theater_ratio, 0, 0.35).
narrative_ontology:measurement(res_acc_tr_t10, reserve_accumulation_race, theater_ratio, 10, 0.42).
narrative_ontology:measurement(res_acc_tr_t20, reserve_accumulation_race, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(res_acc_be_t0, reserve_accumulation_race, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(res_acc_be_t10, reserve_accumulation_race, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(res_acc_be_t20, reserve_accumulation_race, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reserve_accumulation_race, global_infrastructure).
narrative_ontology:affects_constraint(reserve_accumulation_race, currency_crisis_transmission).
narrative_ontology:affects_constraint(reserve_accumulation_race, capital_flight_dynamics).
narrative_ontology:affects_constraint(reserve_accumulation_race, terms_of_trade_volatility).
narrative_ontology:affects_constraint(reserve_accumulation_race, seigniorage_extraction).

% DUAL FORMULATION NOTE:
% Reserve accumulation is upstream of multiple structural constraints. Currency crises are partially downstream (causally dependent on reserve adequacy), but also partially independent (can occur even with high reserves if confidence collapses). Seigniorage extraction is a component but distinct constraint (focusing on central bank revenue rather than national reserve buffers). Each downstream constraint should declare this story in affects_constraints entries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reserve_accumulation_race, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
