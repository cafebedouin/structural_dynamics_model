% ============================================================================
% CONSTRAINT STORY: social_coordination__market_exchange
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_social_coordination__market_exchange, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: social_coordination__market_exchange
 *   human_readable: Market Exchange as Social Coordination Mechanism
 *   domain: political/social/economics
 *
 * SUMMARY:
 *   Large-scale social coordination through market exchange is one of four
 *   competing readings of how societies organize collective action and
 *   distribute resources. This reading asserts that price signals, voluntary
 *   transactions, and property rights suffice for coordination, with
 *   authority limited to contract enforcement and boundary policing. Unlike
 *   governance readings (which place formal authority at the center), kinship
 *   readings (which embed coordination in obligation), or ritual readings
 *   (which make defection cognitively unavailable), the market reading claims
 *   coordination emerges from decentralized exchange without centralized
 *   authority. The structural data reveals this reading as a tangled rope:
 *   genuine coordination benefits coexist with asymmetric extraction.
 *   Beneficiaries are asset holders and intermediaries (those with endowments
 *   to trade); victims are agents without tradeable endowments. The
 *   extractiveness trajectory (0.32 → 0.48 over six time periods) reflects
 *   rising market concentration and financialization. The theater ratio (0.52
 *   average) indicates that market efficiency claims increasingly function as
 *   legitimation cover rather than empirical description. The suppression
 *   metric (0.35) captures the system's low formal coercion but high
 *   structural dependency on market participation — exit from transactions is
 *   nominally free; exit from the system is structurally constrained. This is
 *   a false summit candidate: the market reading frequently naturalizes
 *   itself as an immutable law ('trade emerges wherever specialization
 *   occurs'), but structural data shows contingent institutional arrangements
 *   with identifiable beneficiaries.
 *
 * KEY AGENTS:
 *   - Asset Holders / Intermediaries: Primary beneficiaries (institutional/arbitrage) — accumulate gains from trading position and information advantage; experience market as genuine coordination mechanism
 *   - Endowment-Poor Agents: Primary victims (powerless/trapped) — lack tradeable assets and experience market participation as coercive without real alternative; bear extraction with minimal negotiation power
 *   - Wage Laborers with Portable Skills: Secondary agents (moderate/constrained) — benefit from labor market coordination while bearing extraction through power imbalance and exit costs
 *   - Market-Embedded Communities: Secondary agents (organized/constrained) — benefit from economies of scale and specialization while dependent on external price setters; lose subsistence autonomy
 *   - Market Regulation Coalition: Organized agents (organized/mobile) — implement temporary coordination enhancements (labor standards, antitrust, welfare provisions) with sunset logic as market power concentration changes
 *   - Neoclassical Theory Apparatus: Institutional actor (institutional/arbitrage) — maintains legitimating theoretical edifice despite empirical degradation; performs market naturalization function
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees risk of naturalizing contingent institutions as immutable laws; evaluates whether market reading dominates empirically or coexists with other readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(social_coordination__market_exchange, 0.48).
domain_priors:suppression_score(social_coordination__market_exchange, 0.35).
domain_priors:theater_ratio(social_coordination__market_exchange, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(social_coordination__market_exchange, extractiveness, 0.48).
narrative_ontology:constraint_metric(social_coordination__market_exchange, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(social_coordination__market_exchange, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(social_coordination__market_exchange, tangled_rope).
narrative_ontology:human_readable(social_coordination__market_exchange, "Market Exchange as Social Coordination Mechanism").
narrative_ontology:topic_domain(social_coordination__market_exchange, "political/social/economics").

domain_priors:requires_active_enforcement(social_coordination__market_exchange).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(social_coordination__market_exchange, '114f1ec2-fa2c-4af2-a42d-9e33b770e320').
narrative_ontology:cs_kernel_codification('114f1ec2-fa2c-4af2-a42d-9e33b770e320', formalized).
narrative_ontology:cs_authority_grounding('114f1ec2-fa2c-4af2-a42d-9e33b770e320', extraction).
narrative_ontology:cs_interpretation_layer_present('114f1ec2-fa2c-4af2-a42d-9e33b770e320').
narrative_ontology:cs_reading_relation('114f1ec2-fa2c-4af2-a42d-9e33b770e320', social_coordination__governance, coexists_with).
narrative_ontology:cs_reading_relation('114f1ec2-fa2c-4af2-a42d-9e33b770e320', social_coordination__kinship_obligation, influences).
narrative_ontology:cs_reading_relation('114f1ec2-fa2c-4af2-a42d-9e33b770e320', social_coordination__ritual_consensus, influences).
narrative_ontology:cs_axiom('114f1ec2-fa2c-4af2-a42d-9e33b770e320', foundational, decentralized_price_coordination_sufficient).
narrative_ontology:cs_axiom_status(decentralized_price_coordination_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('114f1ec2-fa2c-4af2-a42d-9e33b770e320', decentralized_price_coordination_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('114f1ec2-fa2c-4af2-a42d-9e33b770e320', foundational, voluntary_transaction_equals_genuine_consent).
narrative_ontology:cs_axiom_status(voluntary_transaction_equals_genuine_consent, holdable).
narrative_ontology:cs_axiom_grounding('114f1ec2-fa2c-4af2-a42d-9e33b770e320', voluntary_transaction_equals_genuine_consent, deontological).
narrative_ontology:cs_reference_frame('114f1ec2-fa2c-4af2-a42d-9e33b770e320', market_efficiency_optimality).
narrative_ontology:cs_drift_state('114f1ec2-fa2c-4af2-a42d-9e33b770e320', contemporary_financialization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('114f1ec2-fa2c-4af2-a42d-9e33b770e320', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(social_coordination__market_exchange, social_coordination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(social_coordination__market_exchange, asset_holders).
narrative_ontology:constraint_beneficiary(social_coordination__market_exchange, intermediaries).
narrative_ontology:constraint_victim(social_coordination__market_exchange, endowment_poor_agents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENDOWMENT-POOR AGENT (SNARE) — Agent with minimal asset base or wage-earning capacity experiences market coordination as a pure extraction mechanism. Nominally free to exit any transaction, but lack of endowment forecloses exit from the market system itself. Price signals dictate participation terms with no negotiation power. No coordination benefit flows to this agent; all extraction, minimal choice. Suppression appears as 'natural scarcity' but functions as structural coercion.
constraint_indexing:constraint_classification(social_coordination__market_exchange, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ASSET HOLDER / INTERMEDIARY (ROPE) — Benefits directly from price signals and market mechanisms. Experiences the market as genuine coordination system: price information solves the allocation problem efficiently, transaction costs are minimized, and their position is structurally favored. Exit options abundant (arbitrage across markets). Extraction flows toward this agent but is experienced as 'gain from trade' rather than extraction. Sees minimal suppression — the system appears consensual because their endowment position makes real alternatives available.
constraint_indexing:constraint_classification(social_coordination__market_exchange, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: WAGE LABORER WITH PORTABLE SKILLS (TANGLED ROPE) — Experiences both genuine coordination benefit (labor markets aggregate demand, set prices, enable specialization) and asymmetric extraction (wages reflect power imbalance, exit costs include retraining, relocation, skill degradation). Can exit particular transactions but constrained by labor market structure. Both coordination and extraction present simultaneously.
constraint_indexing:constraint_classification(social_coordination__market_exchange, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MARKET-EMBEDDED COMMUNITY (TANGLED ROPE) — Communities integrated into market systems benefit from price coordination (economies of scale, specialization, reduced local resource scarcity) while bearing asymmetric extraction (dependency on external price setters, loss of subsistence autonomy, cultural erosion). Organized agents can negotiate collective terms but cannot fully exit market integration without accepting generational costs. Mixed coordination and extraction.
constraint_indexing:constraint_classification(social_coordination__market_exchange, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: MARKET REGULATION COALITION (SCAFFOLD) — Organized agents (labor unions, consumer protection bodies, market regulators) can implement temporary coordination enhancements with sunset logic: minimum wage laws, trading halts, antitrust enforcement, labor standards. These are scaffolding structures layered onto market exchange to reduce extraction and enhance coordination. Theater ratio moderate (enforcement required but increasingly legible). Sunset implicit: if market concentration decreases or labor bargaining power improves, regulation can be lightened.
constraint_indexing:constraint_classification(social_coordination__market_exchange, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: NEOCLASSICAL THEORY APPARATUS (PITON) — The formal theoretical edifice (supply and demand curves, utility maximization, perfect competition, efficient markets) has largely become performative ritual. The theory is taught as settled despite contradictions with empirical market structure (information asymmetry, power imbalances, behavioral deviation from rationality assumptions). Market practice deviates substantially from theoretical assumptions but the theory persists through institutional inertia in economics curricula and policy rhetoric. Theater ratio very high (0.75+) — the theoretical apparatus functions primarily as legitimation cover rather than predictive model.
constraint_indexing:constraint_classification(social_coordination__market_exchange, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, price signals as coordination mechanisms may appear as an immutable structural feature of any complex division of labor: wherever agents specialize, some signal must coordinate exchange, and prices emerge naturally from supply and demand. This perspective risks naturalizing what is actually a contingent institutional framework. The engine's false summit detector will identify this as a false natural law, revealing that the 'inherent to specialization' framing conceals distribution and power dynamics that are socially constructed.
constraint_indexing:constraint_classification(social_coordination__market_exchange, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(social_coordination__market_exchange_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(social_coordination__market_exchange, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(social_coordination__market_exchange, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(social_coordination__market_exchange, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(social_coordination__market_exchange, TR),
    TR >= 0.70.

:- end_tests(social_coordination__market_exchange_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The market reading's base extractiveness reflects that price signals do coordinate large-scale exchange efficiently while simultaneously concentrating distribution power in asset holders and intermediaries. The trajectory 0.32 → 0.48 captures rising market concentration (financial markets, monopolistic platform dynamics, asset consolidation) which increases extraction without necessarily undermining coordination function. This is the signature of tangled rope: coordination improves (information aggregation, allocation efficiency) while extraction grows. Suppression (0.35): Moderate. Suppression is low compared to other readings (governance, which requires active coercion) because market exit at the transaction level is nominally free. However, suppression is substantial at the system level: agents without endowments face strong pressure to participate in markets because alternative subsistence pathways have been eroded. This reflects the constraint's signature: low formal coercion, high structural dependency. Theater ratio (0.52): Moderate. Economic theory and market rhetoric have increasingly become performative legitimation rather than predictive description. Perfect competition assumptions, efficient market hypotheses, and rational actor models persist in policy and teaching despite documented violations (information asymmetry, behavioral deviation, power imbalances). The theater ratio increases over time (0.38 → 0.52) as empirical violations accumulate and theoretical apparatus increasingly functions as cover story.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a striking perspectival gap between the market reading and the structural reality. Asset holders and intermediaries genuinely experience the market as coordination (rope): price signals solve allocation problems, transaction costs decrease, and their position is structurally favored. Endowment-poor agents experience snare: they have no real alternatives, must accept whatever terms the market offers, and bear extraction with no negotiation power. The wage laborer with portable skills experiences tangled rope: labor markets coordinate supply and demand (genuine coordination) while wages reflect power imbalance (genuine extraction). The market-embedded community experiences tangled rope: access to broader exchange networks brings benefits while dependency on external price setters brings vulnerability. The market regulation coalition sees scaffold: temporary interventions can reduce extraction and enhance coordination, with sunset logic as conditions change. The neoclassical theory apparatus sees piton: the theoretical edifice persists through institutional inertia despite contradictions with empirical market structure. The analytical observer at civilizational scope risks seeing mountain (market coordination as immutable law of specialization) but structural data reveals this as false summit — the contingent institutional arrangements are being naturalized.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is determined by the agent's structural position: their power level, exit options, and relationship to the extraction flow. Asset holders (institutional/arbitrage) have d ≈ 0.15 (low, net beneficiary): they exit easily across markets and gain from price signals. Endowment-poor agents (powerless/trapped) have d ≈ 0.95 (high, full target): they cannot exit the market system and bear extraction with no alternatives. Wage laborers (moderate/constrained) have d ≈ 0.65 (moderate): they have some labor market choices but constrained by skill/location/capital. Market-embedded communities (organized/constrained) have d ≈ 0.58 (moderate-high): they coordinate at scale but depend on external price setters. Regulation coalitions (organized/mobile) have d ≈ 0.50 (symmetric): they coordinate regulatory solutions but must negotiate with beneficiaries. Theory apparatus (institutional/arbitrage) derives d from its beneficiary function (legitimating market naturalization) despite claiming neutrality. Analytical observer uses canonical d ≈ 0.73 (analytical). The directionality chain is stable: beneficiary/victim declarations (asset_holders as beneficiaries, endowment_poor_agents as victims) combine with exit options to produce the d values that scale extractiveness via f(d).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endowment_distribution_counterfactual,
    'What would market coordination look like if endowment distribution were genuinely flat (all agents began with identical assets)?',
    'Counterfactual modeling of price formation and extraction in flat-endowment equilibrium; comparison of extraction profiles before and after redistribution in historical cases (land reform, wealth redistribution, talent-pooling systems)',
    'If extraction persists in flat-endowment case: extraction is functional to coordination mechanism itself, not merely artifact of unequal starting position (supports snare classification for powerless). If extraction collapses: extraction is purely distributional (supports rope classification for all agents, with redistribution as policy lever).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endowment_distribution_counterfactual, conceptual, 'Whether extraction is structural to market mechanism or merely artifact of endowment inequality').

omega_variable(
    voluntariness_boundary_ambiguity,
    'At what point does ''choice of transaction'' become ''coerced acceptance of system'' when exit from entire market system is structurally unavailable?',
    'Anthropological analysis of non-market coordination systems and their integration/coercion into market dependency; historical study of colonization, enclosure movements, and subsistence erosion; institutional analysis of exit costs for agents attempting to maintain non-market livelihoods',
    'If system-exit cost is genuinely high: the ''voluntary'' transaction-level choice is misleading and extraction is structural (supports snare). If alternative livelihoods remain viable: market participation is genuinely voluntary and extraction is transaction-specific (supports rope with distributional concerns).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voluntariness_boundary_ambiguity, empirical, 'Voluntariness boundary between transaction choice and system coercion').

omega_variable(
    sibling_reading_empirical_status,
    'Do empirical social coordination systems actually instantiate the market reading or do they operationalize multiple readings simultaneously (market + kinship + ritual + governance)?',
    'Ethnographic study of real coordination systems; cross-cultural institutional analysis; historical documentation of which reading dominates in different contexts',
    'If market reading dominates empirically: tangled rope classification is appropriate. If all four readings coexist in layered form: each constraint story is one analytical extraction, and the kernel''s actual instantiation is a mixture (multiple stories needed, all classified as tangled rope with different ε values for different readings).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_empirical_status, empirical, 'Whether market reading empirically dominates or coexists with other readings').

omega_variable(
    false_summit_natural_law_claim,
    'Is market coordination presented as a natural law or discovered institutional arrangement? Does the reading naturalize what is actually constructed?',
    'Genealogical analysis of market theory and rhetoric; documentation of pre-market coordination systems and their displacement; examination of whether market appears inevitable or contingent in each iteration of the theoretical tradition',
    'If falsely naturalized: mountain classification is invalid and engine''s false summit detector should reclassify to tangled rope or snare (beneficiary present: institutional actors benefit from naturalizing market as immutable law). Reclassification affects policy flexibility (immutable law permits no intervention; contingent arrangement permits redesign).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, conceptual, 'Whether market coordination is presented as natural law or contingent institutional arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(social_coordination__market_exchange, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soccoord_mkt_tr_t0, social_coordination__market_exchange, theater_ratio, 0, 0.38).
narrative_ontology:measurement(soccoord_mkt_tr_t3, social_coordination__market_exchange, theater_ratio, 3, 0.45).
narrative_ontology:measurement(soccoord_mkt_tr_t6, social_coordination__market_exchange, theater_ratio, 6, 0.52).

% Extraction over time
narrative_ontology:measurement(soccoord_mkt_be_t0, social_coordination__market_exchange, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(soccoord_mkt_be_t3, social_coordination__market_exchange, base_extractiveness, 3, 0.4).
narrative_ontology:measurement(soccoord_mkt_be_t6, social_coordination__market_exchange, base_extractiveness, 6, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(soccoord_mkt_su_t0, social_coordination__market_exchange, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(soccoord_mkt_su_t3, social_coordination__market_exchange, suppression_requirement, 3, 0.31).
narrative_ontology:measurement(soccoord_mkt_su_t6, social_coordination__market_exchange, suppression_requirement, 6, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(social_coordination__market_exchange, resource_allocation).
narrative_ontology:affects_constraint(social_coordination__market_exchange, social_coordination__governance).
narrative_ontology:affects_constraint(social_coordination__market_exchange, social_coordination__kinship_obligation).
narrative_ontology:affects_constraint(social_coordination__market_exchange, social_coordination__ritual_consensus).
narrative_ontology:affects_constraint(social_coordination__market_exchange, endowment_distribution_inequality).
narrative_ontology:affects_constraint(social_coordination__market_exchange, market_power_concentration).

% DUAL FORMULATION NOTE:
% The market_exchange reading is one of four structural decompositions of the SOCIAL_COORDINATION kernel. The four readings (market_exchange, governance, kinship_obligation, ritual_consensus) are not alternative views of the same constraint — they are structurally distinct constraints instantiated in real systems. Real social coordination typically operationalizes multiple readings simultaneously in layered form. This story captures the market_exchange reading's ε-invariance and perspectival structure. Sibling readings are separate constraint stories with their own ε values, beneficiary/victim declarations, and theater ratios. Network links show which other constraints are affected by the market reading's institutional dominance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(social_coordination__market_exchange, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
