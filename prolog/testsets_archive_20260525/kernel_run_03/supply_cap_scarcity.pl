% ============================================================================
% CONSTRAINT STORY: supply_cap_scarcity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supply_cap_scarcity, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: supply_cap_scarcity
 *   human_readable: Supply Cap Scarcity and Volatility Lock in Cryptocurrency Markets
 *   domain: monetary_theory/financial_markets/distributed_systems
 *
 * SUMMARY:
 *   The supply cap scarcity constraint in cryptocurrency systems operates as
 *   a complex coordination mechanism that simultaneously enables genuine
 *   monetary function (store of value), generates speculative extraction
 *   (volatility-driven wealth concentration), and maintains ideological
 *   legitimacy through the 'sound money' narrative. The constraint creates
 *   structural incompatibilities between three interpretive frameworks: sound
 *   money ideology requires price stability, speculation requires volatility,
 *   and decentralization ideology requires independence from centralized
 *   intermediaries, yet practical price discovery depends on centralized
 *   exchanges. The fixed supply rule (typically 21 million BTC maximum) is
 *   presented as immutable and fundamental, but analysis reveals it as a
 *   design choice that benefits early adopters and institutional participants
 *   at the cost of medium-of-exchange function and ideological coherence. The
 *   system exhibits all six constraint types from different perspectives:
 *   snare for merchants attempting to use crypto as currency, rope for early
 *   adopters and exchanges, tangled rope for retail investors, scaffold for
 *   stablecoin builders, piton for the degraded sound-money narrative, and a
 *   potentially false-summit mountain from the civilizational observer.
 *
 * KEY AGENTS:
 *   - Early Adopters: Institutional/arbitrage beneficiaries — capture appreciation premium and scarcity rent; benefit from volatility through trading opportunities
 *   - Exchange Operators: Institutional/arbitrage beneficiaries — extract fees from volatility-driven trading volume; depend on continued price discovery centralization
 *   - Sound Money Believers: Powerless/identity_locked — constitute their identity through fixed-supply principle; cannot exit without abandoning identity frame; labor extracted toward speculation
 *   - Decentralization Ideologues: Powerless/identity_locked — constitute their identity through anti-intermediation principle; cannot perceive centralized exchange dependency without frame rupture; mining and organizing labor extracted
 *   - Retail Investors: Moderate/constrained — face genuine coordination need (inflation hedge) balanced against extraction (wealth concentration through volatility)
 *   - Medium-of-Exchange Function: Powerless/trapped — the actual use-case for which the system was designed; sacrificed to speculation and narrative maintenance
 *   - Stablecoin Builders: Organized/constrained — recognize the constraint's dysfunction; building workarounds with sunset logic (if successful, reduce scarcity premium)
 *   - Analytical Observer: Analytical/analytical — risks naturalizing a design choice as mathematical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supply_cap_scarcity, 0.58).
domain_priors:suppression_score(supply_cap_scarcity, 0.62).
domain_priors:theater_ratio(supply_cap_scarcity, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supply_cap_scarcity, extractiveness, 0.58).
narrative_ontology:constraint_metric(supply_cap_scarcity, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(supply_cap_scarcity, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supply_cap_scarcity, tangled_rope).
narrative_ontology:human_readable(supply_cap_scarcity, "Supply Cap Scarcity and Volatility Lock in Cryptocurrency Markets").
narrative_ontology:topic_domain(supply_cap_scarcity, "monetary_theory/financial_markets/distributed_systems").

domain_priors:requires_active_enforcement(supply_cap_scarcity).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(supply_cap_scarcity, formalized).
narrative_ontology:cs_authority_grounding(supply_cap_scarcity, extraction).
narrative_ontology:cs_interpretation_layer_present(supply_cap_scarcity).
narrative_ontology:cs_reading_relation(supply_cap_scarcity, supply_dynamic_money_reading, forecloses).
narrative_ontology:cs_reading_relation(supply_cap_scarcity, supply_price_stability_reading, forecloses).
narrative_ontology:cs_reading_relation(supply_cap_scarcity, supply_neutrality_reading, influences).
narrative_ontology:cs_axiom(supply_cap_scarcity, foundational, immutable_fixed_supply_is_sound).
narrative_ontology:cs_axiom_status(immutable_fixed_supply_is_sound, holdable).
narrative_ontology:cs_axiom_grounding(supply_cap_scarcity, immutable_fixed_supply_is_sound, empirically_contingent).
narrative_ontology:cs_axiom(supply_cap_scarcity, foundational, scarcity_premium_equals_inflation_hedge).
narrative_ontology:cs_axiom_status(scarcity_premium_equals_inflation_hedge, holdable).
narrative_ontology:cs_axiom_grounding(supply_cap_scarcity, scarcity_premium_equals_inflation_hedge, empirically_contingent).
narrative_ontology:cs_axiom(supply_cap_scarcity, secondary, decentralized_ledger_requires_pow).
narrative_ontology:cs_axiom_status(decentralized_ledger_requires_pow, holdable).
narrative_ontology:cs_axiom_grounding(supply_cap_scarcity, decentralized_ledger_requires_pow, empirically_contingent).
narrative_ontology:cs_reference_frame(supply_cap_scarcity, fixed_supply_sound_money_framework).
narrative_ontology:cs_drift_state(supply_cap_scarcity, mature_institutional_phase, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supply_cap_scarcity, early_adopters).
narrative_ontology:constraint_beneficiary(supply_cap_scarcity, exchange_operators).
narrative_ontology:constraint_beneficiary(supply_cap_scarcity, narrative_entrepreneurs).
narrative_ontology:constraint_victim(supply_cap_scarcity, price_stability_seekers).
narrative_ontology:constraint_victim(supply_cap_scarcity, medium_of_exchange_function).
narrative_ontology:constraint_victim(supply_cap_scarcity, decentralization_ideology).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MEDIUM-OF-EXCHANGE FUNCTION (SNARE) — Cryptocurrency's fixed supply creates structural volatility that makes it unsuitable for stable pricing of goods and services. Merchants who attempt to use crypto face pricing risk with no hedging mechanism. The function itself is trapped: cannot exit the constraint without sacrificing the asset's core claim (sound money through scarcity). Bears maximum extraction — the medium-of-exchange vision is sacrificed to speculation.
constraint_indexing:constraint_classification(supply_cap_scarcity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EARLY ADOPTERS AND EXCHANGE OPERATORS (ROPE) — Supply scarcity benefits early adopters through appreciation and trading opportunity. Exchange operators benefit from volatility-driven transaction volume and fee extraction. Both experience the constraint as pure coordination: the fixed supply rule enables their value capture through market mechanisms. No coercion perceived — they voluntarily participate in capturing upside from scarcity premium.
constraint_indexing:constraint_classification(supply_cap_scarcity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: RETAIL INVESTORS (TANGLED ROPE) — Face genuine coordination problem: need decentralized store of value in an era of monetary instability. Fixed supply addresses this need (coordination function). But also face volatility extraction: price swings create wealth concentration toward sophisticated traders and early adopters. Can exit by moving to other assets, but at cost of forgoing the inflation-hedge coordination benefit. Mixed extraction and coordination.
constraint_indexing:constraint_classification(supply_cap_scarcity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: SOUND MONEY BELIEVERS (SNARE with identity_lock) — Identity fused with the principle that fixed supply equals sound money. Cannot perceive that volatility undermines the medium-of-exchange function without abandoning the identity frame. Trapped not by material barriers but by the internalized framing that scarcity=virtue. High extraction: their capital and attention are channeled toward speculation rather than use-case building.
constraint_indexing:constraint_classification(supply_cap_scarcity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 5: DECENTRALIZATION IDEOLOGUES (SNARE with identity_lock) — Identity constituted through rejection of centralized financial systems. Cannot perceive the practical reliance on centralized exchanges for price discovery without abandoning the ideological frame. Trapped by cognitive commitment rather than structural barriers. The constraint extracts their labor (proof-of-work mining, community organizing) toward a system that contradicts their stated values.
constraint_indexing:constraint_classification(supply_cap_scarcity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 6: STABLECOIN AND LAYER-2 BUILDERS (SCAFFOLD) — Recognize that supply cap creates volatility lock and build workarounds: stablecoins peg to stable assets, layer-2 protocols reduce on-chain transaction costs and volatility exposure. These are temporary solutions with sunset logic: if successful, they reduce the scarcity premium that justified the original system. Theater is moderate (0.55) — these protocols are functional, not performative. Low suppression because builders can iterate.
constraint_indexing:constraint_classification(supply_cap_scarcity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: SOUND MONEY NARRATIVE (PITON) — The sound-money framing once served a genuine function: motivation for building a decentralized ledger in response to 2008 financial crisis. But the primary function (medium of exchange) has atrophied. The narrative persists through institutional inertia and identity attachment, not because it works. Theater (0.68): the narrative maintains legitimacy through repeated association with 'hard money' ideology, not through demonstrated function. Economic model: supply cap justifies scarcity premium, which maintains institutional inertia.
constraint_indexing:constraint_classification(supply_cap_scarcity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the fundamental incompatibility between fixed supply and stable pricing is a mathematical law: you cannot have both volatility-driven price discovery and price-stable medium of exchange from the same asset class. The tension is logically inescapable. However, the false-summit detector will flag this: the 'law' naturalizes what is actually a design choice. Different supply rules (dynamic supply, multi-tier money, seigniorage sharing) would produce different outcomes. The constraint is contingent on the choice to fix supply, not inherent to money itself.
constraint_indexing:constraint_classification(supply_cap_scarcity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supply_cap_scarcity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(supply_cap_scarcity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(supply_cap_scarcity, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(supply_cap_scarcity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(supply_cap_scarcity, TR),
    TR >= 0.70.

:- end_tests(supply_cap_scarcity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Elevated but not maximal. The constraint extracts significant value to early adopters and institutional participants through scarcity premium and volatility capture, but the extraction is not total because the system does provide genuine monetary functions (store of value, transaction settlement on distributed ledger) and legitimate coordination benefits (non-censorship, inflation hedge). The measurement trajectory (0.35→0.58 over interval) shows extraction accumulating as the system matures and narrative becomes established — early chaos (low extractiveness) stabilizes into institutional extraction patterns. Suppression (0.62): High. Multiple barriers prevent exit from the constraint: (1) narrative lock-in ('scarcity is virtue'), (2) sunk investment in mining and development, (3) network effects make alternative systems harder to coordinate, (4) ideological commitment makes questioning the premise costly, (5) lack of functional alternatives for some use-cases (inflation hedge, uncensored settlement). These barriers are not total (hence 0.62 not 0.85) but they are substantial. Theater ratio (0.68): Elevated and rising. The sound-money narrative provides increasing performative legitimacy as price appreciation drives mainstream attention. Proof-of-work mining is computationally real but functionally ritualistic for settlement that could be achieved with far less energy. The measured increase (0.42→0.68) tracks the transition from technical community (low theater) to mainstream narrative (high theater). Supply cap is presented as immutable mathematics when it is actually a parameter choice — this presentational distortion increases theater ratio.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces stark perspectival divergence across the indexical space. Early adopters and exchanges see rope (pure coordination benefit), while merchants attempting to use crypto as currency see snare (pure extraction with no exit). Sound-money believers see mountain (immutable law), while analytical observers see a false summit (contingent design choice). Stablecoin builders see scaffold (temporary problem with a sunset), while the medium-of-exchange function sees permanent sacrifice. The gap reflects that the same structural feature (fixed supply) produces opposite experienced effects depending on the agent's position: beneficiaries experience it as enabling value capture; victims experience it as imposing volatility and dysfunction. The identity_locked exit option appears for two distinct victim groups (sound-money believers and decentralization ideologues) who cannot perceive the constraint's dysfunction because their identity frames make the premises of the constraint unquestionable. This perspectival multiplicity is not a measurement error — it is the engine's signal that the constraint achieves its extraction precisely through framing multiplicity. Different narrative communities see different constraint types from identical base properties because the constraint's mechanism is narrative-dependent.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are computed from each agent's relationship to the extraction flow and exit capacity. Early adopters (institutional/arbitrage, beneficiary): d ≈ 0.10 (full beneficiary with escape routes) → low χ from their perspective → they perceive rope. Sound-money believers (powerless/identity_locked, victim): d ≈ 0.92 (trapped by cognitive commitment) → high χ → they perceive snare from analytical view, but from their own biographical horizon they perceive mountain (the constraint is 'immutable truth'). The critical insight: identity_locked changes the classification at biographical time (returns rope from identity_locked perspective vs mountain from trapped perspective at same biographical horizon). This reveals that the binding mechanism is cognitive rather than material — the agent could exit if their frame shifted, but within the frame, exit is unthinkable. Retail investors (moderate/constrained, mixed): d ≈ 0.55 (symmetric costs and benefits) → moderate χ → they perceive tangled rope. Exchange operators (institutional/arbitrage, beneficiary): d ≈ 0.08 (beneficiary with arbitrage exit) → negative χ → they perceive rope. Medium-of-exchange function (powerless/trapped, victim): d ≈ 0.95 → high χ → snare from all perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is a tangled rope masquerading as mountain (false summit). The tension between sound money's requirement for price stability and speculation's dependence on volatility is NOT a law of nature — it is a consequence of design choices that benefit early adopters. The system genuinely coordinates monetary function (genuine rope component) but layers extraction on top through narrative-dependent identity lock and scarcity premium capture (genuine snare component for some perspectives). The analytical observer's mountain perspective is a false summit because it naturalizes the incompatibility between supply cap and price stability when alternative supply rules (dynamic supply, multi-tier monetary systems, seigniorage sharing) would produce different outcomes. The decentralization ideology's snare classification reveals that the system extracts ideological labor while maintaining hidden centralization (all price discovery flows through centralized exchanges). The scaffold perspective (stablecoins and layer-2 systems) confirms that the original constraint is being worked around rather than solved — if the constraint were truly immutable, workarounds would not exist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sound_money_vs_medium_exchange_incompatibility,
    'Are fixed supply and price-stable medium of exchange genuinely incompatible in practice, or is volatility a temporary equilibration phenomenon that will stabilize as adoption increases?',
    'Historical analysis of long-term price volatility across maturation timescales; comparison with other commodity monies (gold standard era pricing); model testing of whether increased adoption reduces volatility or amplifies it through leverage cycles',
    'If incompatible: the medium-of-exchange function is permanently sacrificed to scarcity narrative, classifying the constraint as snare from all perspectives. If temporary: scaffold perspective is correct, and sunset logic applies. If volatility is endogenous (leverage cycles amplify rather than dampen), snare classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sound_money_vs_medium_exchange_incompatibility, empirical, 'Whether fixed supply and price stability are incompatible').

omega_variable(
    decentralization_exchange_dependency,
    'Is price discovery fundamentally dependent on centralized exchanges, or can decentralized price mechanisms replace them without sacrificing liquidity and volatility control?',
    'Comparative analysis of DEX (decentralized exchange) price discovery efficiency vs centralized exchange performance; measurement of slippage, time-to-discovery, and volatility under different governance models',
    'If exchange-dependent: decentralization ideology is revealed as performative (piton), and the system is extracting ideological labor while maintaining hidden centralization. If decentralizable: the constraint is potentially resolvable and ideological framing has structural basis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralization_exchange_dependency, empirical, 'Whether decentralized price discovery can replace centralized exchanges').

omega_variable(
    scarcity_premium_narrative_dependency,
    'Is the scarcity premium maintained by the sound-money narrative, by genuine monetary properties, or by behavioral factors (store-of-value attractiveness and network effects)?',
    'Narrative analysis: tracking of sound-money framing in developer discourse, marketing, and community; correlation analysis of media narrative volume with price movements and adoption cycles; comparison with other scarce assets (gold, rare earth elements) to isolate narrative effect from intrinsic scarcity',
    'If narrative-dependent: the system is extracting attention and capital toward speculation that could be directed toward functional use-cases, and the piton/snare classifications are reinforced. If genuinely monetary: some aspects of the sound-money framing have structural basis. If behavioral: the system works through preference-shaping rather than fundamental properties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scarcity_premium_narrative_dependency, conceptual, 'Whether scarcity premium depends on sound-money narrative').

omega_variable(
    identity_lock_breakage_cost,
    'What is the cost (personal, institutional, ideological) for sound-money believers and decentralization ideologues to break their identity fusion with the constraint?',
    'Ethnographic analysis of apostasy narratives: interviews with users who abandoned the system; measurement of community response to doubt expression; analysis of institutional penalties for questioning core premises',
    'If cost is high: identity lock is strong, and the snare classification from these perspectives is reinforced. If cost is moderate: some fraction of adherents could migrate to alternative models if presented with non-stigmatized pathways. If cost is low: the frame is genuinely optional and agents could exit readily — constrains the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_breakage_cost, empirical, 'Cost of breaking identity lock for true believers').

omega_variable(
    false_summit_candidacy,
    'Is the supply cap constraint a fundamental property of cryptocurrency systems (natural law), or does it reflect design choices that beneficiary groups (early adopters, exchanges) have institutionalized?',
    'Design history analysis: examination of alternative supply models proposed and rejected; game-theoretic modeling of whether fixed supply is necessary for any core cryptocurrency property or merely sufficient; study of whether supply-cap claims match mathematical reality',
    'If natural law: the mountain classification is correct and the constraint is immutable. If design choice: the constraint is a false summit, beneficiary groups are using ''immutability'' framing to protect wealth concentration, and the engine''s FSM detector should reclassify as tangled_rope or snare depending on perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_candidacy, conceptual, 'Whether supply cap is fundamental or a design choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supply_cap_scarcity, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supply_tr_t0, supply_cap_scarcity, theater_ratio, 0, 0.42).
narrative_ontology:measurement(supply_tr_t4, supply_cap_scarcity, theater_ratio, 4, 0.58).
narrative_ontology:measurement(supply_tr_t8, supply_cap_scarcity, theater_ratio, 8, 0.68).
narrative_ontology:measurement(supply_tr_t2, supply_cap_scarcity, theater_ratio, 2, 0.5).
narrative_ontology:measurement(supply_tr_t6, supply_cap_scarcity, theater_ratio, 6, 0.62).

% Extraction over time
narrative_ontology:measurement(supply_be_t0, supply_cap_scarcity, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(supply_be_t4, supply_cap_scarcity, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(supply_be_t8, supply_cap_scarcity, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(supply_be_t2, supply_cap_scarcity, base_extractiveness, 2, 0.4).
narrative_ontology:measurement(supply_be_t6, supply_cap_scarcity, base_extractiveness, 6, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supply_cap_scarcity, resource_allocation).
narrative_ontology:boltzmann_floor_override(supply_cap_scarcity, 0.18).
narrative_ontology:affects_constraint(supply_cap_scarcity, proof_of_work_energy_scaling).
narrative_ontology:affects_constraint(supply_cap_scarcity, exchange_centralization_dependency).
narrative_ontology:affects_constraint(supply_cap_scarcity, speculative_volatility_feedback).
narrative_ontology:affects_constraint(supply_cap_scarcity, ideological_identity_fusion).

% DUAL FORMULATION NOTE:
% Supply cap scarcity decomposes into three constraint stories: (1) supply_cap_scarcity (this story): the core tangled rope combining monetary coordination with speculative extraction, (2) proof_of_work_validation_theater: the ritual aspect of mining (ε=0.42, piton), (3) decentralization_exchange_contradiction: the ideological-practical gap between decentralization claims and centralized-exchange dependence (ε=0.65, snare). Each has distinct ε values and different perspectives. This story is the hub coordinating the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(supply_cap_scarcity, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
