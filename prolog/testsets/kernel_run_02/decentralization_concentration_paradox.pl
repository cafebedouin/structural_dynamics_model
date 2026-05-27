% ============================================================================
% CONSTRAINT STORY: decentralization_concentration_paradox
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_decentralization_concentration_paradox, []).

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
 *   constraint_id: decentralization_concentration_paradox
 *   human_readable: Decentralization-Concentration Paradox in Cryptocurrency Governance
 *   domain: political_economy/monetary_theory/technology_governance
 *
 * SUMMARY:
 *   The decentralization-concentration paradox describes a structural
 *   contradiction at the core of cryptocurrency systems: the technical
 *   architecture promises decentralized consensus and disintermediation, but
 *   the network effects, economic incentives, and operational realities
 *   consistently reconcentrate power into mining pools, exchange operators,
 *   and developer teams. This constraint exhibits the hallmark of a
 *   tangled_rope: genuine coordination function (distributed consensus solves
 *   the Byzantine General problem; exchanges solve the liquidity discovery
 *   problem) paired with asymmetric extraction (early adopters and
 *   infrastructure operators capture disproportionate wealth while the
 *   ideological promise of decentralization is systematically violated). The
 *   theater_ratio (0.68) reflects the growing gap between decentralization
 *   rhetoric and concentrated reality: protocol developers tout governance
 *   token systems that fragment decision-making while actual implementation
 *   power concentrates through technical complexity, capital requirements,
 *   and coordination costs. The extractiveness trajectory (0.22 → 0.58) shows
 *   accumulating extraction as the system matures: early adoption gains are
 *   consolidated, alternative exit options (competing chains) become
 *   costlier, and regulatory pressure locks retail investors into fewer,
 *   regulated exchanges. The suppression (0.65) reflects high barriers to
 *   exit and understanding: technical complexity creates information
 *   asymmetry; switching costs to alternative chains are high; ideological
 *   commitment locks advocates despite recognition of the paradox.
 *
 * KEY AGENTS:
 *   - Small Retail Investors: Primary victim (powerless/trapped) — asymmetric information, technical barriers, post-purchase-lock-in volatility exposure with no upside hedge
 *   - Early Adopters & Mining Pool Operators: Primary beneficiary (institutional/arbitrage) — capture wealth through network effects, early-entry asymmetry, operational infrastructure control, and cartel pricing coordination
 *   - Cryptocurrency Exchange Operators: Secondary beneficiary (institutional/arbitrage) — extract value through trading fees, custody premiums, and liquidity concentration; have full exit options
 *   - Decentralization Idealist Community: Secondary victim (organized/identity_locked) — recognize the paradox but face identity-fusion costs of exiting; constrained by cognitive commitment to the failed promise
 *   - Central Banks & Monetary Authorities: Tertiary actor (institutional/constrained) — need coordination with cryptocurrency systems to manage monetary aggregates; extract policy concessions while bearing none of the systemic stability accountability
 *   - Protocol Developers: Institutional infrastructure (institutional/arbitrage) — control governance through technical complexity; maintain theater of decentralization while routing power through development team discretion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decentralization_concentration_paradox, 0.58).
domain_priors:suppression_score(decentralization_concentration_paradox, 0.65).
domain_priors:theater_ratio(decentralization_concentration_paradox, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decentralization_concentration_paradox, extractiveness, 0.58).
narrative_ontology:constraint_metric(decentralization_concentration_paradox, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(decentralization_concentration_paradox, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decentralization_concentration_paradox, tangled_rope).
narrative_ontology:human_readable(decentralization_concentration_paradox, "Decentralization-Concentration Paradox in Cryptocurrency Governance").
narrative_ontology:topic_domain(decentralization_concentration_paradox, "political_economy/monetary_theory/technology_governance").

domain_priors:requires_active_enforcement(decentralization_concentration_paradox).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decentralization_concentration_paradox, 'c6f94b89-8e85-4205-b1fc-34c058042195').
narrative_ontology:cs_created_at('c6f94b89-8e85-4205-b1fc-34c058042195', '').
narrative_ontology:cs_kernel_codification('c6f94b89-8e85-4205-b1fc-34c058042195', distributed).
narrative_ontology:cs_authority_grounding('c6f94b89-8e85-4205-b1fc-34c058042195', distributed).
narrative_ontology:cs_reading_relation('c6f94b89-8e85-4205-b1fc-34c058042195', sound_money_reading, coexists_with).
narrative_ontology:cs_reading_relation('c6f94b89-8e85-4205-b1fc-34c058042195', speculative_asset_reading, coexists_with).
narrative_ontology:cs_reading_relation('c6f94b89-8e85-4205-b1fc-34c058042195', decentralization_ideology_reading, coexists_with).
narrative_ontology:cs_axiom('c6f94b89-8e85-4205-b1fc-34c058042195', foundational, fixed_supply_monetary_scarcity).
narrative_ontology:cs_axiom_status(fixed_supply_monetary_scarcity, holdable).
narrative_ontology:cs_axiom_grounding('c6f94b89-8e85-4205-b1fc-34c058042195', fixed_supply_monetary_scarcity, empirically_contingent).
narrative_ontology:cs_axiom('c6f94b89-8e85-4205-b1fc-34c058042195', foundational, volatility_enables_speculation).
narrative_ontology:cs_axiom_status(volatility_enables_speculation, holdable).
narrative_ontology:cs_axiom_grounding('c6f94b89-8e85-4205-b1fc-34c058042195', volatility_enables_speculation, empirically_contingent).
narrative_ontology:cs_axiom('c6f94b89-8e85-4205-b1fc-34c058042195', foundational, distributed_consensus_enables_disintermediation).
narrative_ontology:cs_axiom_status(distributed_consensus_enables_disintermediation, overridden).
narrative_ontology:cs_axiom_grounding('c6f94b89-8e85-4205-b1fc-34c058042195', distributed_consensus_enables_disintermediation, empirically_contingent).
narrative_ontology:cs_reference_frame('c6f94b89-8e85-4205-b1fc-34c058042195', decentralized_peer_to_peer_coordination).
narrative_ontology:cs_drift_state('c6f94b89-8e85-4205-b1fc-34c058042195', contemporary_cartelization_era, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decentralization_concentration_paradox, early_adopters_holders).
narrative_ontology:constraint_beneficiary(decentralization_concentration_paradox, mining_infrastructure_operators).
narrative_ontology:constraint_beneficiary(decentralization_concentration_paradox, cryptocurrency_exchange_operators).
narrative_ontology:constraint_victim(decentralization_concentration_paradox, small_retail_investors).
narrative_ontology:constraint_victim(decentralization_concentration_paradox, monetary_stability_seekers).
narrative_ontology:constraint_victim(decentralization_concentration_paradox, decentralization_idealists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL RETAIL INVESTOR (SNARE) — Trapped by asymmetric information and technical barriers. Unable to extract early-adopter gains; bears volatility risk without upside protection. No legitimate exit once holdings drop below reinvestment threshold. Experiences maximum extraction via the coordination of network effects and price discovery that benefit larger holders.
constraint_indexing:constraint_classification(decentralization_concentration_paradox, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MONETARY STABILITY SEEKER (TANGLED ROPE) — Central banks, fiscal authorities, and currency-stability advocates face genuine coordination need (cryptocurrency volatility affects monetary aggregates, inflation measurement, financial stability). But this coordination is enforced asymmetrically: cryptocurrency advocates extract monetary policy concessions while bearing none of the accountability for systemic stability. Constrained exit due to regulatory necessity and integration with broader financial system.
constraint_indexing:constraint_classification(decentralization_concentration_paradox, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXCHANGE OPERATOR (ROPE) — Experiences the constraint as pure coordination: aggregating liquidity, matching buy-sell orders, managing settlement. Net beneficiary through trading fees and custody premiums. Has full arbitrage exit: can pivot to other asset classes, adjust fee structures, or exit market entirely if regulatory pressure increases.
constraint_indexing:constraint_classification(decentralization_concentration_paradox, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DECENTRALIZATION IDEALIST COMMUNITY (SNARE) — Organized in principle but structurally powerless: the promise of decentralized consensus has reconcentrated into mining pools, exchange custody, and developer oligopolies. The founding ideals (disintermediation, censorship resistance, democratic governance) are systematically extracted through the constraint's coordination of network effects. Community members face massive exit costs (identity abandonment, sunk technical expertise, relational ties to co-idealists) despite recognizing the extraction. Constrained not by material barriers but by cognitive lock.
constraint_indexing:constraint_classification(decentralization_concentration_paradox, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: EARLY ADOPTER / MINING POOL OPERATOR (ROPE) — Experiences the constraint as coordination that enables wealth accumulation: network effects create value, distributed consensus creates scarcity, scarcity creates price appreciation. Has full arbitrage exit (can sell holdings, diversify, exit mining operations, invest in alternative chains). Net beneficiary — extraction flows toward this agent, framed as legitimate reward for early adoption and technical infrastructure investment.
constraint_indexing:constraint_classification(decentralization_concentration_paradox, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CRYPTOCURRENCY PROTOCOL LAYER (PITON) — The technical consensus mechanism (proof-of-work, proof-of-stake) is largely performative: it produces a distributed ledger, but governance power has concentrated into mining cartels, exchange hubs, and developer teams. The protocol's decentralization theater masks actual concentration. The mechanism persists through network inertia and switching costs rather than because it achieves stated decentralization goals. Theater ratio high (0.68) reflects gap between decentralization promise and concentrated reality.
constraint_indexing:constraint_classification(decentralization_concentration_paradox, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, concentration of cryptocurrency governance mirrors concentration in all network systems: scale economies in computation, capital aggregation, and operational expertise create natural monopolies regardless of decentralization protocol design. This perspective treats the concentration paradox as an immutable law of complex systems. However, the structural data reveals this as a false summit: the paradox is a contingent institutional arrangement (early-adopter advantage, regulatory capture, mining cartel coordination) rather than a fundamental constraint on distributed systems.
constraint_indexing:constraint_classification(decentralization_concentration_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decentralization_concentration_paradox_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(decentralization_concentration_paradox, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(decentralization_concentration_paradox, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(decentralization_concentration_paradox, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(decentralization_concentration_paradox, TR),
    TR >= 0.70.

:- end_tests(decentralization_concentration_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The system demonstrates clear asymmetric extraction: early adopters captured 90%+ of initial wealth creation; mining concentrated into industrial-scale operations; exchange custody concentration created regulatory dependency. However, extraction is not maximal (0.58 not 0.80) because speculation still attracts new entrants and some legitimate liquidity coordination occurs. The trajectory shows accumulation: as the system matures and alternatives become expensive, extraction mechanisms tighten. Suppression (0.65): High. Multiple reinforcing barriers prevent exit: technical complexity (wallet management, private key custody), switching costs (regulatory licenses concentrated in few jurisdictions), ideological sunk costs (advocates cannot leave without identity abandonment), and market illiquidity (small holders cannot liquidate without accepting deep discounts). Theater ratio (0.68): High and rising. The decentralization promise is increasingly performative: governance tokens that don't govern; consensus mechanisms that concentrate into mining cartels; 'community-driven development' that routes through small elite developer teams; regulatory avoidance through technical obfuscation that benefits only sophisticated operators. The theater has increased from 0.35 (early days, true distributed mining) to 0.68 (current cartelization and regulatory capture) as concentration mechanisms have solidified.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces maximal perspectival divergence. The early adopter sees rope (legitimate coordination rewarding first-mover risk); the small investor sees snare (extraction with no exit). The decentralization idealist sees rope (coordination achieving the founding vision) at immediate time horizon but snare at biographical or generational horizon (the vision is systematically violated). The monetary authority sees tangled rope (genuine coordination need paired with asymmetric concessions). The exchange operator sees pure rope (they are solving liquidity discovery). The protocol developer sees rope or piton (they maintain the system, either as active governance or performative theater). The analytical observer risks seeing mountain (concentration is inherent to distributed systems) but the structural data reveals false summit: the concentration is enforced through contingent institutional arrangements (early-adopter advantage, mining pool cartels, regulatory-exchange dependency), not through immutable network properties.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value (d) is determined by their position in the extraction flow. Early adopters and mining operators are beneficiaries with arbitrage exit (d ≈ 0.05-0.15, f(d) ≈ -0.12 to -0.01): they can exit without material loss and benefit from the coordination. Small retail investors are victims with trapped exit (d ≈ 0.95, f(d) ≈ 1.42): they have no exit without unacceptable loss and bear the volatility cost. Decentralization idealists are victims with identity_locked exit (d ≈ 0.85-0.90, f(d) ≈ 1.15-1.28): structurally mobile (could sell holdings, abandon advocacy) but cognitively locked by identity fusion with the movement. Exchange operators are beneficiaries with arbitrage exit (d ≈ 0.10, f(d) ≈ -0.05): can pivot to other assets or markets entirely. Monetary authorities are mixed (beneficiary in some dimensions via policy discretion, victim in others via financial stability risk) with constrained exit (d ≈ 0.55-0.65, f(d) ≈ 0.70-1.00): cannot ignore cryptocurrency's monetary effects but bear accountability for systemic consequences without direct control.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint satisfies the tangled_rope gates: it has genuine coordination function (distributed consensus, liquidity discovery, disintermediation of payments), asymmetric extraction (early adopters and infrastructure operators capture disproportionate wealth), active enforcement (mining pool coordination, exchange custody concentration, developer technical control), and high suppression (0.65). The mandatrophy is resolved by recognizing that the coordination function is real and valuable (financial censorship resistance, technical innovation) while the extraction is also real and systematic (wealth concentration, ideological promise violation). The perspective set demonstrates this: rope-classified perspectives see the coordination; snare-classified perspectives see the extraction; tangled_rope perspectives integrate both. The false-summit mountain perspective reveals that naturalization (claiming concentration is inherent to distributed systems) is a cover story deployed by beneficiaries to foreclose questioning of the institutional arrangements that produce it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    three_kernels_or_one,
    'Is cryptocurrency a single contested kernel with three incompatible readings (sound money vs speculative asset vs decentralization ideology) or three structurally distinct constraints misnamed by the same signifier?',
    'Success criteria test: for each reading, what would falsify it? Sound money reading falsified by high volatility. Speculative asset reading requires high volatility. Decentralization reading falsified by concentration. Each reading has different measurement objectives and incompatible terminal states.',
    'If single kernel: the engine should decompose this story into three readings with kernel_context and reading_relations. If three distinct constraints: each should be a separate constraint story (three separate files, linked via network.affects_constraints). Current authoring treats as one tangled_rope with three perspective clusters; if kernel decomposition is warranted, structure changes fundamentally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(three_kernels_or_one, conceptual, 'Whether cryptocurrency represents one contested kernel or three distinct constraints').

omega_variable(
    decentralization_achievability,
    'Is the concentration of cryptocurrency governance inherent to the technical architecture and network effects of distributed ledgers, or is it contingent on current incentive structures that could be redesigned?',
    'Comparison across cryptocurrency implementations: do all systems show concentration, or do some (Monero, Cosmos, Polkadot with decentralized governance) maintain distributed power? Study of theoretical bounds on consensus mechanism scalability vs. decentralization tradeoffs.',
    'If inherent: the mountain perspective is correct — decentralization is structurally impossible at scale, making the constraint an immutable law. If contingent: the concentration is an enforced extraction mechanism, and the constraint is tangled_rope or snare. This determines whether ''decentralization ideology'' is a tragic misconception (mountain) or a thwarted coordination goal (tangled_rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(decentralization_achievability, empirical, 'Whether concentration is inherent or contingent on current incentive design').

omega_variable(
    retail_investor_exit_cost,
    'What proportion of retail crypto holders cannot liquidate without unacceptable loss due to exchange manipulation, market illiquidity for their specific holdings, or regulatory barriers?',
    'Survey-based measurement of liquidity barriers by holding size and time-to-exit; comparison of bid-ask spreads for small retail vs institutional orders; tracking of regulatory closure events and their impact on retail exit capacity.',
    'If low cost (< 5% friction): retail investors are constrained but not trapped; reclassify small-investor perspective to constrained exit (lowers d, lowers chi). If high cost (> 20% friction): trap classification confirmed; the snare perspective is structural rather than perspectival.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retail_investor_exit_cost, empirical, 'Actual exit costs for small retail cryptocurrency holders').

omega_variable(
    idealist_identity_lock_mechanism,
    'For cryptocurrency advocates who recognize the decentralization paradox, what binding mechanism prevents them from exiting? Is it material cost (sunk investment), identity fusion (self-concept constituted through crypto advocacy), or epistemic closure (inability to see contradiction)?',
    'Qualitative study of defectors: which advocates change positions, what triggers the shift, what costs do they report? Analysis of retention vs abandonment across different holder cohorts (pure idealists vs holders seeking speculation vs developers with career investment).',
    'If material cost dominant: constrained exit, tangled_rope perspective confirmed. If identity fusion dominant: identity_locked exit, which produces rope classification at biographical horizon (different from snare trapped). If epistemic closure: the decentralization idealist community cannot see the concentration regardless of external costs. This affects classification and omega resolution pathway.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(idealist_identity_lock_mechanism, empirical, 'Nature of binding mechanism for decentralization idealists').

omega_variable(
    mining_cartel_enforceability,
    'How durable is the concentration of mining power? Can mining pools maintain cartel pricing and block selection given technical barriers to exit and protocol incentives, or would individual miner defection disperse power under stress?',
    'Game-theoretic analysis of mining pool incentives under network stress (sustained high fees, regulatory pressure, competing chains). Historical analysis of mining pool defection patterns; measurement of transaction costs for miners to switch pools or self-mine.',
    'If cartel durable: concentration is enforced through a coordination mechanism (tangled_rope as experienced by miners, snare as experienced by users). If brittle: concentration is contingent on current conditions, and the constraint could decompose under pressure. Affects whether the piton''s inertia is structural or temporary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mining_cartel_enforceability, empirical, 'Durability and enforceability of mining pool concentration').

omega_variable(
    monetary_policy_entanglement_feedback,
    'Does central bank accommodation of cryptocurrency speculation (implicit or explicit) reinforce the concentration-paradox constraint by validating speculative value while claiming decentralization? Or is monetary authority largely indifferent?',
    'Analysis of central bank statements, policy documents, and interest rate decisions relative to cryptocurrency price movements. Study of regulatory forbearance periods and their correlation with retail adoption and concentration metrics.',
    'If entangled feedback exists: monetary authorities are providing extractive enforcement (higher d for exchange operators and early adopters), making the constraint a true tangled_rope with multi-level institutional extraction. If indifferent: the constraint is primarily peer-to-peer extraction with minimal institutional involvement, changing the victim set and enforcement character.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monetary_policy_entanglement_feedback, empirical, 'Whether central bank policy feedback reinforces concentration dynamics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decentralization_concentration_paradox, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(decon_tr_t0, decentralization_concentration_paradox, theater_ratio, 0, 0.35).
narrative_ontology:measurement(decon_tr_t4, decentralization_concentration_paradox, theater_ratio, 4, 0.5).
narrative_ontology:measurement(decon_tr_t8, decentralization_concentration_paradox, theater_ratio, 8, 0.62).
narrative_ontology:measurement(decon_tr_t12, decentralization_concentration_paradox, theater_ratio, 12, 0.68).
narrative_ontology:measurement(decon_tr_t16, decentralization_concentration_paradox, theater_ratio, 16, 0.71).
narrative_ontology:measurement(decon_tr_t20, decentralization_concentration_paradox, theater_ratio, 20, 0.75).

% Extraction over time
narrative_ontology:measurement(decon_be_t0, decentralization_concentration_paradox, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(decon_be_t4, decentralization_concentration_paradox, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(decon_be_t8, decentralization_concentration_paradox, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(decon_be_t12, decentralization_concentration_paradox, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(decon_be_t16, decentralization_concentration_paradox, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(decon_be_t20, decentralization_concentration_paradox, base_extractiveness, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decentralization_concentration_paradox, resource_allocation).
narrative_ontology:boltzmann_floor_override(decentralization_concentration_paradox, 0.12).
narrative_ontology:affects_constraint(decentralization_concentration_paradox, monetary_policy_transmission_mechanism).
narrative_ontology:affects_constraint(decentralization_concentration_paradox, regulatory_arbitrage_dynamics).
narrative_ontology:affects_constraint(decentralization_concentration_paradox, proof_of_work_energy_externality).

% DUAL FORMULATION NOTE:
% The decentralization-concentration paradox may decompose into three constraint families: (1) sound_money_cryptocurrency (technical claim about supply scarcity vs. fiat debasement; ε ≈ 0.15, Mountain), (2) speculative_asset_cryptocurrency (claim about volatility and price discovery; ε ≈ 0.48, Tangled Rope), (3) decentralization_governance_cryptocurrency (ideological claim about disintermediation; ε ≈ 0.62, Snare at victim perspective). Current story treats all three as perspectives on one constraint; alternative treatment would write three separate stories with network.affects_constraints linking them and omega variables documenting the decomposition question.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(decentralization_concentration_paradox, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
