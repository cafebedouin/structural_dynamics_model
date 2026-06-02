% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_consensus_kernel__maximalist_reading, []).

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
 *   constraint_id: bitcoin_consensus_kernel__maximalist_reading
 *   human_readable: Bitcoin Maximalist Protocol Covenant (Immutable Monetary Policy Reading)
 *   domain: cryptoeconomics/distributed_consensus/monetary_systems
 *
 * SUMMARY:
 *   The maximalist reading of the Bitcoin consensus kernel treats the
 *   protocol's fixed monetary policy (21 million BTC cap, 10-minute block
 *   time, proof-of-work security model) as an immutable covenant: a founding
 *   commitment that cannot be revised without destroying the legitimacy of
 *   the entire system. This reading emerged from Satoshi Nakamoto's original
 *   whitepaper and evolved into an identity-defining principle for a
 *   significant faction of the Bitcoin community. The constraint operates at
 *   multiple scales: it constrains protocol developers who wish to implement
 *   scalability improvements, it extracts resources from alternative
 *   cryptocurrency projects that must compete against a narrative monopoly on
 *   'true decentralization,' and it creates a tiered architecture (Layer 2
 *   solutions, sidechains) that would be unnecessary if the base protocol
 *   were mutable. The extractiveness has risen over the interval (0.32 →
 *   0.58) as the maximalist reading has become institutionalized: early
 *   Bitcoin discussions treated protocol mutability as an open question;
 *   current discussions treat change advocacy as heresy. The theater ratio
 *   has also risen (0.48 → 0.65) as Bitcoin Core discussions increasingly
 *   perform deference to 'Satoshi's vision' while managing technical reality
 *   through incremental efficiency improvements and off-chain scaling.
 *
 * KEY AGENTS:
 *   - Early Adopters / Hodlers (institutional/arbitrage): Primary beneficiaries — the covenant preserves scarcity and prevents dilution of holdings; they experience it as coordination that enables trust in future purchasing power
 *   - Protocol Preservationists / Maximalists (organized/constrained): Coalition defending the covenant against innovation proposals; they view change as corruption of founding principles
 *   - Scalability Layer Developers (powerless/trapped): Primary victims — trapped by immutable base protocol; forced to build off-chain solutions (Lightning, Stacks, Liquid) rather than protocol improvements
 *   - Pragmatic Protocol Developers (moderate/constrained): Secondary victims — propose technical improvements (efficiency, privacy) but face supermajority activation barrier and narrative delegitimation
 *   - Alternative Cryptocurrency Projects (powerful/mobile): Extracted by the narrative monopoly on 'true decentralization'; must differentiate on innovation despite being framed as centralization vectors
 *   - Bitcoin Core Development Team (institutional/arbitrage): Performs respect for the covenant while managing technical reality; sees their own discretion as stripped
 *   - Layer 2 / Sidechain Coalition (organized/constrained): Beneficiaries of the immutable-constraint structure (their solutions capture value because base is immutable) but also bear cost of architectural complexity
 *   - Analytical Observer (analytical/analytical): Risks naturalizing a contingent institutional arrangement as a mathematical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__maximalist_reading, 0.58).
domain_priors:suppression_score(bitcoin_consensus_kernel__maximalist_reading, 0.72).
domain_priors:theater_ratio(bitcoin_consensus_kernel__maximalist_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__maximalist_reading, snare).
narrative_ontology:human_readable(bitcoin_consensus_kernel__maximalist_reading, "Bitcoin Maximalist Protocol Covenant (Immutable Monetary Policy Reading)").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__maximalist_reading, "cryptoeconomics/distributed_consensus/monetary_systems").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__maximalist_reading, 'f16971b4-e97a-45cc-8b1a-abcf9033aaf0').
narrative_ontology:cs_kernel_codification('f16971b4-e97a-45cc-8b1a-abcf9033aaf0', fixed_text).
narrative_ontology:cs_authority_grounding('f16971b4-e97a-45cc-8b1a-abcf9033aaf0', extraction).
narrative_ontology:cs_interpretation_layer_present('f16971b4-e97a-45cc-8b1a-abcf9033aaf0').
narrative_ontology:cs_reading_relation('f16971b4-e97a-45cc-8b1a-abcf9033aaf0', bitcoin_consensus_kernel__utility_reading, coexists_with).
narrative_ontology:cs_reading_relation('f16971b4-e97a-45cc-8b1a-abcf9033aaf0', bitcoin_consensus_kernel__pragmatic_synthesis, influences).
narrative_ontology:cs_axiom('f16971b4-e97a-45cc-8b1a-abcf9033aaf0', foundational, immutability_is_foundational).
narrative_ontology:cs_axiom_status(immutability_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('f16971b4-e97a-45cc-8b1a-abcf9033aaf0', immutability_is_foundational, deontological).
narrative_ontology:cs_axiom('f16971b4-e97a-45cc-8b1a-abcf9033aaf0', foundational, protocol_change_is_corruption).
narrative_ontology:cs_axiom_status(protocol_change_is_corruption, holdable).
narrative_ontology:cs_axiom_grounding('f16971b4-e97a-45cc-8b1a-abcf9033aaf0', protocol_change_is_corruption, conventional).
narrative_ontology:cs_reference_frame('f16971b4-e97a-45cc-8b1a-abcf9033aaf0', satoshi_original_vision_immutable_covenant).
narrative_ontology:cs_drift_state('f16971b4-e97a-45cc-8b1a-abcf9033aaf0', contemporary_layer2_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f16971b4-e97a-45cc-8b1a-abcf9033aaf0', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, early_adopters_and_hodlers).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, protocol_preservationists).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, scalability_layers).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, protocol_innovation_attempts).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, competing_cryptocurrency_ecosystems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SCALABILITY LAYER DEVELOPER (SNARE) — Trapped by the immutable base protocol. Any innovation requiring consensus change faces activation barriers (95% miner/node supermajority) and faces delegitimation by maximalist narrative. Extracted: time, resources, and legitimacy diverted to Layer 2 and sidechain workarounds rather than protocol improvements. No exit: the global liquidity and network effects of Bitcoin cannot be replicated; forking produces a worthless token.
constraint_indexing:constraint_classification(bitcoin_consensus_kernel__maximalist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PRAGMATIC PROTOCOL DEVELOPER (SNARE) — Constrained by activation thresholds and maximalist narrative veto. Can propose changes (transaction format improvements, signature verification efficiency gains, privacy enhancements) but faces supermajority requirement and delegitimation as 'altcoin thinking.' Career and resource costs of advocacy against the covenant are substantial. Some exit (can join alternative chains or Layer 2 projects) but at the cost of network liquidity and economic relevance.
constraint_indexing:constraint_classification(bitcoin_consensus_kernel__maximalist_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: CRYPTOCURRENCY COMPETITOR PROJECT (SNARE) — Extracted by the maximalist reading's narrative monopoly on 'true decentralization' and immutability. Competitors must differentiate on innovation dimensions (smart contracts, faster settlement, privacy) that are explicitly delegitimized by the maximalist framework as 'centralization vectors' or 'technical debt.' Suppression: maximalist messaging pre-empts adoption by framing flexibility as a vulnerability. Mobile exit (can build alternative chains with better technology) exists but at massive cost in network adoption and liquidity depth.
constraint_indexing:constraint_classification(bitcoin_consensus_kernel__maximalist_reading, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: EARLY ADOPTER / HODLER (ROPE) — Primary beneficiary. The immutable covenant preserves scarcity (21 million cap) and prevents inflation-driven dilution of holdings. Experiences the constraint as coordination: 'we all agree not to debase the currency' enables hodlers to trust in future purchasing power. Arbitrage exit (can sell holdings and diversify; can move to Ethereum or other chains) exists but is not preferred — the covenant creates optionality value. Net beneficiary — extraction runs toward this group.
constraint_indexing:constraint_classification(bitcoin_consensus_kernel__maximalist_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: BITCOIN CORE DEVELOPMENT TEAM (PITON) — The Core devs are institutional actors tasked with 'maintaining' Bitcoin, but the immutable-covenant reading strips them of meaningful discretion. Their actual function — fixing bugs, optimizing code efficiency, managing technical debt — is performative in the presence of a covenant that forecloses protocol evolution. Theater: endless Core discussions about 'conservatism' and 'Satoshi's vision' that perform respect for the covenant while managing technical reality. The team sees their own role as degraded. Piton derives from high theater and perceived institutional inertia, not from high extraction.
constraint_indexing:constraint_classification(bitcoin_consensus_kernel__maximalist_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: LAYER 2 / SIDECHAIN COALITION (TANGLED ROPE) — Organized agents (Lightning Network, Stacks, Liquid, Polygon) coordinate on alternative scaling solutions that operate within the immutable-protocol constraint. Both coordination function (solves scalability without protocol change) and asymmetric extraction (the immutable base protocol creates the *need* for Layer 2 workarounds; users pay routing fees, channel lock-up costs, bridge slippage) coexist. The coalition benefits from the constraint (their solutions capture value because the base is immutable) and bears its cost (architectural complexity, fragmentation of liquidity).
constraint_indexing:constraint_classification(bitcoin_consensus_kernel__maximalist_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a sufficiently long and abstract view, the immutable 21-million cap is presented as a mathematical law: 'Bitcoin's scarcity is enforced by cryptographic proof, not social agreement; you cannot change it any more than you can change the laws of mathematics.' This perspective is a FALSE SUMMIT. The immutable covenant is a social commitment enforced by supermajority consensus rules, not a mathematical fact. The mathematical facts (SHA-256, elliptic curve cryptography) are indeed immutable; but the 21-million protocol parameter is a social choice that could be changed by consensus. The mountain classification naturalizes a contingent institutional arrangement.
constraint_indexing:constraint_classification(bitcoin_consensus_kernel__maximalist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_consensus_kernel__maximalist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bitcoin_consensus_kernel__maximalist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bitcoin_consensus_kernel__maximalist_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_consensus_kernel__maximalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bitcoin_consensus_kernel__maximalist_reading, TR),
    TR >= 0.70.

:- end_tests(bitcoin_consensus_kernel__maximalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The maximalist reading extracts in multiple dimensions: (1) resource diversion — developers must build Layer 2 workarounds rather than protocol improvements; (2) narrative monopoly — alternative chains are delegitimized as lacking 'true' decentralization; (3) opportunity cost — protocol innovations (privacy, scalability, smart contracts) that might improve the system are foregone to preserve the covenant. The value is not as high as a pure snare (0.72+) because the constraint also provides genuine benefits (scarcity protection, network stability) and does not impose total coercion — alternatives exist (fork, move to Layer 2, adopt different chains), though at high cost. Suppression (0.72): High. Activation barriers (95% supermajority for soft forks) and narrative suppression (covenant critique delegitimized as heresy) create substantial barriers to change. Technical barriers are real; narrative barriers are equally real and harder to overcome. Theater ratio (0.65): Moderately high. Bitcoin Core discussions increasingly perform deference to 'Satoshi's vision' and immutability principles while managing technical reality through efficiency improvements and stealth feature additions. The theater has risen over the interval as the maximalist narrative has become institutionalized.
 *
 * PERSPECTIVAL GAP:
 *   The maximalist reading produces a wide perspectival gap. Early adopters see coordination and value protection (Rope) — the covenant enables them to trust in future purchasing power. Pragmatic developers see a snare — they are trapped by immutable constraints and forced into Layer 2 workarounds. The Layer 2 coalition sees tangled rope — they both benefit from the constraint (their solutions capture value) and bear its cost (architectural fragmentation). The Core development team sees a piton — they perform respect for the covenant while managing technical reality. The civilizational analytical observer risks seeing a mountain — the covenant naturalized as mathematical law — but the structural data reveals this as a false summit: the 'immutability' is a social commitment enforced by supermajority consensus, not a mathematical fact. The underlying ε-invariance remains stable: the constraint does extract (0.58), does suppress (0.72), and does function as a snare from the perspectives of those who would benefit from protocol flexibility.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from the agent's structural relationship to the extraction flow. Early adopters are beneficiaries with arbitrage exit options (can sell holdings, diversify) — they experience low or negative extraction. Scalability developers are victims with trapped exit (cannot replicate Bitcoin's network effects elsewhere) — they experience maximum extraction. Pragmatic developers are victims with constrained exit (can propose changes but face high activation barriers) — they experience high extraction. The Layer 2 coalition is organized with constrained exit (coordinated workaround, but cannot change the base constraint) — they experience mixed extraction and benefit. The Core team appears as institutional actors with arbitrage exit (could fork, move to other projects) but in practice exhibit constrained exit because their legitimacy derives from Bitcoin stewardship — they see high extraction despite nominal power. The maximalist reading sustains itself through a self-reinforcing feedback loop: agents who benefit from immutability control the narrative; agents who would benefit from flexibility are delegitimized; the constraint becomes more entrenched over time.
 *
 * MANDATROPHY ANALYSIS:
 *   The maximalist reading resolves mandatrophy by explicitly accepting the snare classification from the developer's perspective: yes, this is extraction. The maximalist argument is that the extraction is justified — immutability is necessary for the system's integrity and the network's long-term value. This is a values-based defense of a snare constraint, not a claim that it is coordination. The mandatrophy is resolved in favor of accepting asymmetric extraction as a necessary cost for protocol stability. The piton perspective (Core team performing deference while managing reality) and the false summit perspective (naturalizing social choice as mathematical law) represent the constraint's degradation pathways: if the theater rises further, the constraint may become inert (piton), maintained only by institutional momentum. If the false summit naturalizes successfully, the constraint may become immune to challenge (treating critique as technical error rather than policy dispute).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    covenant_vs_voluntary_coordination,
    'Is the immutable covenant a binding social commitment that cannot be renegotiated, or a highly stable but theoretically revisable coordination equilibrium that persists because changing it is collectively irrational?',
    'Counterfactual analysis: if 95%+ of miners and nodes voluntarily agreed to change the 21M cap, would it be possible? If yes, the constraint is a coordination equilibrium, not a binding covenant. If a persistent minority could block it indefinitely, the constraint is a genuine covenant with veto power.',
    'If coordination equilibrium: classification shifts toward tangled_rope or scaffold (the sunset path is ''adoption of alternatives'', not protocol change). If binding covenant: classification holds as snare/piton (the immutability is enforced socially, via delegitimation of change advocates).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covenant_vs_voluntary_coordination, conceptual, 'Whether the covenant is binding or a persisting equilibrium').

omega_variable(
    maximalist_narrative_enforcement,
    'How much of the suppression (0.72) is enforced by the immutable technical constraint itself versus the maximalist narrative that frames technical immutability as moral virtue?',
    'Comparative analysis: suppression levels in Bitcoin communities vs. alternative chains with mutable protocols but similar economics. If suppression is similar across mutable/immutable chains, the technical immutability is not the primary mechanism; the suppression derives from narrative and network effects. If suppression is substantially higher in immutable chains, technical constraint is primary.',
    'If narrative-driven: the constraint is weaker than ε=0.58 suggests — it persists through ideology, not structural necessity. If technically-driven: the constraint is a genuine structural fact that would persist regardless of narrative framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(maximalist_narrative_enforcement, empirical, 'Role of maximalist narrative in enforcing suppression').

omega_variable(
    protocol_evolution_path_feasibility,
    'Could protocol changes that preserve the spirit of the covenant (scarcity, decentralization) while addressing scalability and privacy be activated if framed as ''bug fixes'' or ''Satoshi-consistent optimizations'' rather than as ''protocol changes''?',
    'Historical precedent: analysis of successfully activated changes (SegWit, Taproot) and their framing. Comparative framing of proposed changes in BIP discussions. Supermajority threshold testing for changes framed as ''preserving original intent'' vs. ''overriding design''.',
    'If feasible: the covenant is more flexible than the maximalist reading suggests; exits exist for high-value protocol improvements if politically reframed. If not feasible: the suppression (0.72) is realistic — even Satoshi-consistent changes face veto if perceived as precedent-setting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protocol_evolution_path_feasibility, empirical, 'Feasibility of protocol change via reframing as bug fix').

omega_variable(
    reading_identity_fusion,
    'Is the maximalist reading an accurate representation of Bitcoin''s structural design, or has it become a fused identity for the Bitcoin community such that questioning the covenant feels like apostasy rather than technical disagreement?',
    'Discourse analysis: tone and framing of discussions about protocol changes in r/bitcoin vs. technical dev channels. Interview data on whether maximalists view covenant critique as technical error or moral failure. Measurement of social costs of covenant skepticism over time.',
    'If identity-fused: the suppression is internalized by the community itself (identity_locked exit is plausible for committed maximalists). If technical disagreement: the suppression is imposed by supermajority veto, and exit is constrained/mobile rather than identity_locked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_fusion, conceptual, 'Whether maximalism is technical consensus or identity fusion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__maximalist_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(btc_max_theater_t0, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(btc_max_theater_t5, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 5, 0.58).
narrative_ontology:measurement(btc_max_theater_t10, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(btc_max_extract_t0, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(btc_max_extract_t5, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(btc_max_extract_t10, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(btc_max_suppress_t0, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(btc_max_suppress_t5, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 5, 0.64).
narrative_ontology:measurement(btc_max_suppress_t10, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__maximalist_reading, resource_allocation).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__utility_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__pragmatic_synthesis).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, layer2_ecosystem_development).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, cryptocurrency_competitive_differentiation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the bitcoin_consensus_kernel. The utility_reading and pragmatic_synthesis readings are separate constraint stories with different beneficiary/victim structures and different ε values. The maximalist_reading extracts highly (ε=0.58, classification snare) against protocol flexibility; the utility_reading would extract differently (lower ε, classification tangled_rope) against immutability. These are not the same constraint viewed from different angles — they are genuinely different constraints corresponding to fundamentally different conceptions of what Bitcoin is for. The network links indicate that activation of the maximalist reading's suppression (high supermajority barriers, narrative delegitimation) directly affects the viability of the other readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_consensus_kernel__maximalist_reading, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
