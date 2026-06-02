% ============================================================================
% CONSTRAINT STORY: sound_money_scarcity_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sound_money_scarcity_constraint, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sound_money_scarcity_constraint
 *   human_readable: Sound Money Scarcity Constraint in Cryptocurrency Systems
 *   domain: monetary_theory/technology_governance/political_economy
 *
 * SUMMARY:
 *   The scarcity commitment in cryptocurrency systems (particularly Bitcoin)
 *   functions as a contested kernel grounding legitimacy claims across three
 *   distinct reading communities: sound-money advocates (treating scarcity as
 *   natural economic law enabling inflation-resistant store of value),
 *   speculative-asset traders (treating scarcity as value-driver for price
 *   appreciation), and decentralization-ideology enforcers (treating scarcity
 *   as necessary architecture for censorship resistance and monetary
 *   sovereignty). This constraint story models these as three authorized
 *   readings of one shared commitment — the immutable or near-immutable
 *   supply cap embedded in the protocol. The indexical classification reveals
 *   the constraint's structural instability: from the early-adopter
 *   perspective it is pure coordination (rope), from the late-entrant
 *   perspective it is pure extraction (snare), from the efficiency-seeker
 *   perspective it is hybrid (tangled rope), and from the ideological
 *   coalition it is enforced commitment (tangled rope with active
 *   engagement). The analytical observer risks classifying this as a
 *   mathematical natural law (mountain), but the structural data reveals it
 *   as a designed constraint requiring continuous social coordination to
 *   maintain. The theater ratio has increased from 0.48 to 0.68 over the
 *   interval, indicating that the performance of immutability and
 *   decentralization has become increasingly theatrical relative to the
 *   underlying technical reality — mining has concentrated, governance has
 *   centralized around development teams and large pools, and the scarcity
 *   mechanism's efficiency costs are increasingly obscured by layer-2
 *   abstraction rather than addressed through protocol changes.
 *
 * KEY AGENTS:
 *   - Early Technical Adopters and Holders (institutional/arbitrage): Primary beneficiaries. Captured value from pre-adoption capital, technical expertise advantage, and network effects. Experience constraint as coordination mechanism that aligns incentives.
 *   - Late-Entrant Retail Participants (powerless/trapped): Primary victims. Face supernormal extraction due to FOMO-driven entry, information asymmetry, and psychological manipulation. Trapped by sunk costs and community pressure.
 *   - Transaction Efficiency Seekers (moderate/constrained): Secondary victims. Want to use cryptocurrency for payments but face throughput and latency constraints imposed by scarcity mechanism. Constrained by network effects and adoption inertia; partially compensated by layer-2 exit paths.
 *   - Decentralization Ideology Coalition (organized/constrained): Organized beneficiary group. Enforces scarcity commitment as core to ideological identity and political mission. Bears costs of technical inflexibility and regulatory pressure; benefits from network effects and ideological coherence.
 *   - Fiat Currency Users in High-Inflation Economies (varies by inflation rate): Potential beneficiary (sound-money reading) or neutral. Empirical benefit depends on price stability relative to local fiat inflation — high volatility regimes may invalidate the hedge.
 *   - Financial Legitimacy Institutions (institutional/arbitrage): Institutional beneficiaries. Treat scarcity narrative as signal for risk-averse capital allocation and regulatory credibility.
 *   - Analytical Observer (analytical/analytical): Risks naturalizing a design choice. The mountain perspective must be marked as false-summit candidate — the 'cryptographic natural law' reading masks contingent governance choices.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sound_money_scarcity_constraint, 0.58).
domain_priors:suppression_score(sound_money_scarcity_constraint, 0.52).
domain_priors:theater_ratio(sound_money_scarcity_constraint, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sound_money_scarcity_constraint, extractiveness, 0.58).
narrative_ontology:constraint_metric(sound_money_scarcity_constraint, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(sound_money_scarcity_constraint, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sound_money_scarcity_constraint, tangled_rope).
narrative_ontology:human_readable(sound_money_scarcity_constraint, "Sound Money Scarcity Constraint in Cryptocurrency Systems").
narrative_ontology:topic_domain(sound_money_scarcity_constraint, "monetary_theory/technology_governance/political_economy").

domain_priors:requires_active_enforcement(sound_money_scarcity_constraint).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sound_money_scarcity_constraint, '2dad07f4-7fdf-48fc-804d-c31bee912076').
narrative_ontology:cs_created_at('2dad07f4-7fdf-48fc-804d-c31bee912076', '').
narrative_ontology:cs_kernel_codification('2dad07f4-7fdf-48fc-804d-c31bee912076', fixed_text).
narrative_ontology:cs_authority_grounding('2dad07f4-7fdf-48fc-804d-c31bee912076', distributed).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sound_money_scarcity_constraint, early_adopters_and_holders).
narrative_ontology:constraint_beneficiary(sound_money_scarcity_constraint, technical_ecosystem_builders).
narrative_ontology:constraint_beneficiary(sound_money_scarcity_constraint, decentralization_ideology_enforcers).
narrative_ontology:constraint_victim(sound_money_scarcity_constraint, late_entrant_retail_participants).
narrative_ontology:constraint_victim(sound_money_scarcity_constraint, fiat_currency_users_in_high_inflation_economies).
narrative_ontology:constraint_victim(sound_money_scarcity_constraint, transaction_efficiency_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LATE-ENTRANT RETAIL PARTICIPANT (SNARE) — Structurally trapped by FOMO-driven entry and asymmetric information. Early adopters and technical insiders have accumulated coins at lower prices; entry barriers (exchange friction, technical knowledge, custody complexity) and psychological manipulation (HODL culture, community pressure) create supernormal extraction. No meaningful exit without realizing losses. The scarcity commitment is weaponized against newcomers — the constraint ensures maximum extraction during bull-market entry phases.
constraint_indexing:constraint_classification(sound_money_scarcity_constraint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TRANSACTION EFFICIENCY SEEKER (TANGLED ROPE) — Constrained by the scarcity-as-feature commitment. Network effects and adoption inertia make alternative protocols expensive to migrate to, but the fixed-supply architecture yields coordination benefits (predictable monetary policy, censorship resistance against state seizure) alongside extraction mechanisms (transaction fee volatility, settlement delays during congestion, exclusion of low-value transfers). Moderate extraction because benefits are genuine but increasingly asymmetric as usage scales.
constraint_indexing:constraint_classification(sound_money_scarcity_constraint, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: EARLY TECHNICAL ADOPTER (ROPE) — Experiences the scarcity constraint as pure coordination. Pre-mining windows, technical expertise barriers, and capital-light entry during proof-of-concept phases yielded supernormal accumulation. The commitment to scarcity aligns their interests: their holdings appreciate as adoption spreads and late entrants compete for fixed supply. No suppression experienced — they can exit (arbitrage) by selling accumulated holdings. Net beneficiary experiencing the constraint as solving coordination problems (price stability, verifiable supply, incentive alignment).
constraint_indexing:constraint_classification(sound_money_scarcity_constraint, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DECENTRALIZATION IDEOLOGY COALITION (TANGLED ROPE) — Organized commitment to the scarcity mechanism as core to censorship resistance and sovereign money narratives. The fixed supply is treated as non-negotiable architectural law — changing it would constitute exit from the foundational commitment. Coalition members (cypherpunks, libertarians, technologists) both benefit (ideological coherence, network effects from ideology-aligned adoption) and bear costs (vulnerability to regulatory pressure, technical inflexibility, pressure to defend scarcity against efficiency critiques). Active enforcement of ideological line (ostracism of chain-scaling proposals, fork consensus rules) is necessary to maintain the constraint. Requires continuous ideological work to prevent schism.
constraint_indexing:constraint_classification(sound_money_scarcity_constraint, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: FINANCIAL LEGITIMACY THEATER (PITON) — Institutional investors and exchanges treat the scarcity mechanism as a legitimacy theater that signals 'sound money' credentials to risk-averse capital. The functional verification that scarcity is maintained (node-based consensus, cryptographic proof) is largely performative from this perspective — actual mining decentralization has concentrated toward industrial operators; the 'immutable ledger' narrative persists despite known reversibility-via-51%-attack. The theater serves to justify allocations to institutional investors, but the underlying verification mechanism has degraded (protocol complexity opacity, energy cost justification, technical governance concentration). Theater ratio is high: the scarcity performance is real, but the claimed immutability and decentralization supporting it are partially theater.
constraint_indexing:constraint_classification(sound_money_scarcity_constraint, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / CRYPTOGRAPHIC NATURAL LAW VIEW (MOUNTAIN) — From a purely technical/cryptographic perspective, the scarcity constraint emerges as a consequence of the proof-of-work algorithm and consensus mechanism — mathematically enforced and technically immutable given the network's current state. Difficulty adjustment, coin cap hardcoded into protocol, cryptographic hash function properties all appear as natural laws of the system. This perspective risks false-summit classification: the mountain reading naturalizes what is actually a design choice enforced by coordinated protocol governance and social consensus. The 'immutable' scarcity is only immutable if the community chooses not to fork.
constraint_indexing:constraint_classification(sound_money_scarcity_constraint, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sound_money_scarcity_constraint_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sound_money_scarcity_constraint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sound_money_scarcity_constraint, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sound_money_scarcity_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sound_money_scarcity_constraint, TR),
    TR >= 0.70.

:- end_tests(sound_money_scarcity_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts supernormal rents from late entrants during adoption phases (informational disadvantage, psychological manipulation, sunk costs), but some of this extraction compensates early adopters for bearing adoption risk and building technical infrastructure. The 0.58 value reflects that extraction is significant but not maximal — coordination benefits are genuine (predictable supply, censorship resistance, network effects), early adopters took on real risks, and alternative systems exist for those willing to bear switching costs. The trajectory shows increasing extractiveness as the system matures and concentration effects compound. Suppression (0.52): Moderate-high. Late entrants face substantial barriers: technical knowledge requirements for custody, exchange friction costs, FOMO-psychological pressure, community norm enforcement against 'paper hands,' switching costs to alternative payment systems. Early adopters can exit (arbitrage option); late entrants face trapped or constrained exit. Mining concentration and protocol governance opacity further suppress late-entrant exit options. Theater ratio (0.64): Elevated. The narrative of immutable decentralized scarcity is increasingly theatrical: (a) mining is concentrated in large industrial operations and pools despite Satoshi's vision of 'one CPU one vote'; (b) protocol immutability is social-consensus-dependent, not code-dependent — forks are technically possible but politically expensive; (c) the 'sound money' claim requires high price stability to be empirically grounded, but volatility remains extreme; (d) efficiency claims are delegated to layer-2 protocols that introduce their own centralization vectors (sequencers, bridge custodians). The theater has grown as adoption requires increasingly sophisticated narratives to justify to institutional investors and mainstream users.
 *
 * PERSPECTIVAL GAP:
 *   The constraint generates maximal perspectival divergence. Early adopters see rope (pure coordination); late entrants see snare (pure extraction); efficiency seekers see tangled rope (mixed); ideology coalitions see tangled rope with active engagement (mixed but motivated); institutional actors see piton (degraded but legitimate-seeming); analytical observers risk mountain (false-summit naturalization). No single classification is 'correct' — the presheaf of perspectives reveals the constraint's structural instability. The snare/rope gap is particularly sharp: the same mechanism (supply fixed at 21M BTC) creates maximum coordination benefit for those who participated early and maximum extraction for those who participate late. This is not a perspective ambiguity — it is a genuine structural asymmetry encoded in the cumulative reward function. The perspectival gap is the constraint itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position along three dimensions: (1) temporal position (early vs. late adoption), (2) technical capacity (insider vs. outsider knowledge), (3) ideological commitment (aligned with decentralization or orthogonal to it). Early adopters with technical insider status and ideological alignment experience d ≈ 0.1-0.2 (net beneficiary), leading to f(d) < 0.3 and negative or near-zero χ — they experience rope coordination. Late entrants with limited technical capacity and no ideological commitment experience d ≈ 0.85-0.95 (net target), leading to f(d) ≈ 1.25-1.40 and high χ — they experience snare extraction. The decentralization coalition experiences d ≈ 0.5-0.65 (mixed) with constrained exit (ideological identity is entangled with technical commitment), leading to f(d) ≈ 0.70-0.85 and moderate χ — they experience tangled rope with active enforcement. The scope modifier σ(S) is global (1.2), amplifying the computed χ values: a late entrant in a regional adoption phase experiences lower extractiveness than a late entrant in global adoption because the verification cost of enforcing scarcity (and thus the effective extraction) is lower at smaller scope. As Bitcoin adoption becomes truly global, suppression and extraction scale upward.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exemplifies mandatrophy through the inability to classify it as a single type without losing essential structural information. The sound-money reading commits to the view that scarcity is economically necessary — this reading requires rope or mountain. The speculative-asset reading requires snare or tangled rope (extraction is the mechanism). The decentralization-ideology reading requires tangled rope or scaffold (coordination of political commitment alongside efficiency costs). No single classification is mandatrophy-false-positive because all three readings are structurally legitimate from their respective positions. The constraint is not misclassified — it is polyphonic. The analytical resolution is to treat the classification as a presheaf: context-dependent (which reading is operative) rather than abstract (which type is the 'real' one). The false-summit mountain perspective correctly identifies the mandatrophy's source: naturalizing the commitment (treating it as cryptographic law) erases the social-coordination mechanism that is doing the real work. The constraint would become truly immutable only if all three reading communities genuinely converged on the scarcity commitment as politically fundamental — currently, they are coordinated by different mechanisms (price signals, narrative alignment, technical specification), making the commitment contingent on continued parallel enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_decomposition_or_unification,
    'Are ''sound-money scarcity,'' ''speculative asset'', and ''decentralization-ideology'' three readings of one contested kernel or three distinct kernels sharing a technological substrate?',
    'Historical analysis of protocol governance decisions: do proposed changes (block size, supply cap modification, consensus mechanism) generate disagreement mapped to a shared kernel (same commitment, different readings) or orthogonal governance failures? If a proposed change (e.g., supply increase to improve fee-burning) generates symmetric schisms across all three reading communities, they share a kernel. If disagreements cluster around independent axes, they are separate kernels.',
    'Shared kernel → constraint story describes the contested commitment with multiple authorized readings; specification of which reading is operative at a given time/context is the classification task. Separate kernels → three constraint stories, one per reading, linked by network affects_constraints. Current authoring assumes shared kernel (one story, six perspectives per SCOPE committer frame integration).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_decomposition_or_unification, conceptual, 'Whether scarcity commitment is one contested kernel or three independent kernels').

omega_variable(
    early_miner_advantage_structural_or_contingent,
    'Is the early-adopter extraction advantage an inherent feature of proof-of-work scarcity mechanisms or a contingent outcome of adoption curve dynamics that could have been structured differently?',
    'Counterfactual analysis: simulate alternative initial distribution mechanisms (ICO, airdrop, time-locked release, progressive supply curve). Compare extraction concentration across mechanisms. If concentration is lower under alternatives, advantage is contingent (exploitable via governance). If concentration is similar, it is structural (inherent to any scarcity mechanism with cumulative reward).',
    'If structural: the snare classification for late entrants is permanent feature, not abuse. If contingent: the extraction is reclassifiable as a governance failure (institution failure rather than constraint feature), suggesting scaffold (temporary) or piton (degraded governance) instead of tangled rope. Current assumption: structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(early_miner_advantage_structural_or_contingent, empirical, 'Early-miner advantage as structural feature vs. contingent governance design').

omega_variable(
    decentralization_vs_efficiency_tradespace_immutable,
    'Is the scarcity mechanism''s efficiency cost (transaction throughput, settlement latency) an immutable technical tradeoff or a governance-solvable constraint through layer-2 protocols and rollups?',
    'Technical capability analysis: compare Lightning Network, Polygon, and rollup throughput against on-chain settlement rates. If layer-2 throughput reaches visa-scale (100k+ TPS) with sub-second finality and scarcity mechanism intact, efficiency cost is governance-solvable (not immutable). If layer-2 requires central sequencer or breaks scarcity guarantees, it is immutable.',
    'If solvable: constraint is tangled-rope with sunset clause (temporary efficiency loss); efficiency seekers have exit path. If immutable: constraint is permanent tangled-rope; efficiency victims face perpetual constrained/trapped status. Current assumption: partially solvable (layer-2 works but has different tradeoffs).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(decentralization_vs_efficiency_tradespace_immutable, empirical, 'Transaction efficiency cost as immutable or governance-solvable').

omega_variable(
    inflation_hedge_empirical_stability,
    'Does the sound-money reading (scarcity as inflation hedge) survive high-volatility regimes? Is the claimed hedge against fiat currency inflation actually robust when BTC price swings exceed fiat inflation by 10x-100x?',
    'Time-series analysis of correlation between Bitcoin price and US inflation, CPI, and real USD rates across regimes (low volatility 2015-2019, bull market 2020-2021, bear market 2022-2023, recent 2024+). If correlation with inflation is positive and stable across regimes, sound-money reading is empirically grounded. If correlation breaks in high-volatility periods, the sound-money reading is context-dependent and the beneficiary group is specialized (long-duration holders, not inflation hedgers in high-volatility environments).',
    'If empirically robust: sound-money reading is legitimate structural reading of the constraint; allows fiat-currency-users-in-high-inflation-economies to be classified as beneficiaries rather than victims. If context-dependent: sound-money reading is aspirational (piton-like performative claim); victims classification is more accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inflation_hedge_empirical_stability, empirical, 'Sound-money reading empirical stability across volatility regimes').

omega_variable(
    mining_decentralization_empirical_degree,
    'How decentralized is the actual mining network relative to the narrative claim? If Nakamoto coefficient is < 3 (i.e., fewer than 3 mining pools control > 50% of hashrate), is the decentralization-ideology reading empirically grounded or theater?',
    'Real-time hashrate monitoring and pool concentration metrics. Check Nakamoto coefficient, Herfindahl index, and geographic distribution of mining. Compare claimed decentralization against measured decentralization at different time points. If measurements show concentration trending toward centralization despite ideology, the decentralization reading is increasingly piton-like.',
    'If decentralized (Nakamoto > 5): decentralization-ideology reading is empirically grounded; coalition constraint is genuine coordination function. If concentrated (Nakamoto < 3): ideology is theater; the constraint becomes piton (enforced by inertia and narrative rather than technical properties); late-entrant snare classification is sharper.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mining_decentralization_empirical_degree, empirical, 'Mining decentralization empirical degree vs. narrative claim').

omega_variable(
    protocol_governance_flexibility_latent,
    'Could the scarcity mechanism be modified if consensus formed (e.g., increasing supply cap to burn fees or improve efficiency) or is it de facto immutable due to political economy constraints even if technically modifiable?',
    'Governance history: track protocol change proposals affecting scarcity (block subsidy, supply cap, difficulty adjustment). Measure coordination cost (forks, community schisms, activation battles) required to achieve changes. If high coordination cost persists across multiple proposals, scarcity is de facto immutable despite technical flexibility — the political economy constraint is stronger than the technical constraint.',
    'If flexible: constraint is scaffold-like (sunset possible through governance); the commitment is contingent. If de facto immutable: constraint is mountain-like or piton-like (enforced by ideology, not code); the commitment is treated as permanent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protocol_governance_flexibility_latent, conceptual, 'Scarcity mechanism de facto immutability despite technical flexibility').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sound_money_scarcity_constraint, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soundmoney_theater_t0, sound_money_scarcity_constraint, theater_ratio, 0, 0.48).
narrative_ontology:measurement(soundmoney_theater_t5, sound_money_scarcity_constraint, theater_ratio, 5, 0.56).
narrative_ontology:measurement(soundmoney_theater_t10, sound_money_scarcity_constraint, theater_ratio, 10, 0.64).
narrative_ontology:measurement(soundmoney_theater_t15, sound_money_scarcity_constraint, theater_ratio, 15, 0.68).

% Extraction over time
narrative_ontology:measurement(soundmoney_extract_t0, sound_money_scarcity_constraint, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(soundmoney_extract_t5, sound_money_scarcity_constraint, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(soundmoney_extract_t10, sound_money_scarcity_constraint, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(soundmoney_extract_t15, sound_money_scarcity_constraint, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sound_money_scarcity_constraint, resource_allocation).
narrative_ontology:boltzmann_floor_override(sound_money_scarcity_constraint, 0.18).
narrative_ontology:affects_constraint(sound_money_scarcity_constraint, proof_of_work_energy_cost).
narrative_ontology:affects_constraint(sound_money_scarcity_constraint, mining_incentive_alignment).
narrative_ontology:affects_constraint(sound_money_scarcity_constraint, layer2_custodial_tradeoff).

% DUAL FORMULATION NOTE:
% The sound-money scarcity constraint is upstream of efficiency and energy-cost constraints. Modifications to the scarcity mechanism (supply curve adjustment, difficulty algorithm changes) cascade to mining incentive structures and thus to energy consumption. Layer-2 protocols (Lightning, Polygon) represent alternative formulations of the efficiency problem that accept custodial or consensus-mechanism tradeoffs to preserve the base-layer scarcity constraint. These constraints are linked as a family: scarcity→efficiency→mining→energy. Each story has distinct ε values: sound_money_scarcity (ε=0.58, tangled rope), proof_of_work_energy (ε=0.65, snare), mining_incentive_alignment (ε=0.52, tangled rope), layer2_custodial_tradeoff (ε=0.48, scaffold). Decomposition follows ε-invariance principle: the observable used to measure each constraint is distinct (scarcity measurements vs. energy measurements vs. incentive alignment vs. custodial concentration), yielding different ε values and thus different classification outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sound_money_scarcity_constraint, organized, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
