% ============================================================================
% CONSTRAINT STORY: blockchain_scalability_trilemma
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_blockchain_scalability_trilemma, []).

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
 *   constraint_id: blockchain_scalability_trilemma
 *   human_readable: Blockchain Scalability Trilemma
 *   domain: distributed_systems/cryptoeconomics
 *
 * SUMMARY:
 *   The blockchain scalability trilemma posits that distributed ledgers
 *   cannot simultaneously maximize three properties: decentralization,
 *   security, and scalability (throughput). Any protocol must sacrifice one
 *   dimension to achieve the other two. This constraint operates as both a
 *   natural law (the mathematical consequence of distributed consensus) and
 *   an enforcement mechanism (the institutional benefit to validators who
 *   preserve the constraint). The trilemma creates differential extraction
 *   across agents: excluded participants bear costs (cannot transact at
 *   scale), L2 developers and sequencers benefit (monetize scalability
 *   solutions), and base layer validators benefit (remain essential). The
 *   constraint exhibits characteristics of all six DR types depending on
 *   perspective, making it a diagnostic exemplar for distinguishing
 *   mathematical constraints from extractive institutional arrangements.
 *
 * KEY AGENTS:
 *   - Excluded Network Participants: Primary victims (powerless/trapped) — cannot achieve scale at base layer; face transaction costs prohibitive for micropayments, developing-world use cases, IoT applications
 *   - L2 Protocol Developers and Sequencers: Primary beneficiaries (moderate/constrained) — create scalability solutions that solve the trilemma for specific applications but extract value through sequencer fees, token inflation, governance capture
 *   - Base Layer Validators: Secondary beneficiaries (institutional/arbitrage) — benefit from trilemma enforcement that preserves their structural necessity and economic value
 *   - Scalability Research Community: Organized agents (organized/mobile) — see the trilemma as a technical problem with identified solutions; working toward exit pathways through cryptographic and consensus innovations
 *   - Proof-of-Work Orthodox Community: Institutional defenders (institutional/arbitrage) — maintain performance/centralization trade-off through ideological commitment despite protocol alternatives
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent architectural choices as immutable mathematical laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(blockchain_scalability_trilemma, 0.52).
domain_priors:suppression_score(blockchain_scalability_trilemma, 0.65).
domain_priors:theater_ratio(blockchain_scalability_trilemma, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(blockchain_scalability_trilemma, extractiveness, 0.52).
narrative_ontology:constraint_metric(blockchain_scalability_trilemma, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(blockchain_scalability_trilemma, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(blockchain_scalability_trilemma, tangled_rope).
narrative_ontology:human_readable(blockchain_scalability_trilemma, "Blockchain Scalability Trilemma").
narrative_ontology:topic_domain(blockchain_scalability_trilemma, "distributed_systems/cryptoeconomics").

domain_priors:requires_active_enforcement(blockchain_scalability_trilemma).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(blockchain_scalability_trilemma, consensus_preservers).
narrative_ontology:constraint_beneficiary(blockchain_scalability_trilemma, decentralization_advocates).
narrative_ontology:constraint_victim(blockchain_scalability_trilemma, transaction_throughput_seekers).
narrative_ontology:constraint_victim(blockchain_scalability_trilemma, excluded_participants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED PARTICIPANT (SNARE) — Cannot participate at scale due to transaction costs and network congestion. Trapped between blockchain participation and traditional systems. Maximum extraction experienced: cannot scale operations, cannot exit cleanly to alternative coordination mechanisms without abandoning blockchain guarantees.
constraint_indexing:constraint_classification(blockchain_scalability_trilemma, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: L2 PROTOCOL DEVELOPER (TANGLED ROPE) — Constrained by the fundamental trilemma: developing scalability solutions requires accepting either centralization or security trade-offs. Benefits from the constraint's existence (creates a problem they can monetize through sequencers, bridges, governance tokens) while bearing costs of technical complexity and user friction. Mixed coordination and extraction.
constraint_indexing:constraint_classification(blockchain_scalability_trilemma, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BASE LAYER VALIDATOR NETWORK (ROPE) — Benefits from the trilemma's enforcement: scalability constraints preserve their role as essential validators. Experiences the constraint as coordination mechanism that maintains their structural importance. Can arbitrage between different consensus parameters.
constraint_indexing:constraint_classification(blockchain_scalability_trilemma, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SCALABILITY RESEARCH COMMUNITY (SCAFFOLD) — Organized actors (protocol researchers, cryptographers, implementation teams) see the trilemma as a temporary coordination failure with identified exit pathways: ZK-rollups, sharding schemes, and alternative consensus mechanisms represent structured solutions with sunset logic. Sunset horizon: 5-10 years as new protocols mature.
constraint_indexing:constraint_classification(blockchain_scalability_trilemma, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PROOF-OF-WORK ORTHODOXY (PITON) — Traditional PoW security model persists through institutional inertia despite acknowledged scalability costs. The foundational design choice remains enforced (theatrical commitment to immutability narratives) despite alternatives (PoS, hybrid consensus) offering genuine functional improvements. Theater ratio reflects the performative commitment to PoW maximalism despite protocol drift toward hybrid systems.
constraint_indexing:constraint_classification(blockchain_scalability_trilemma, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, some version of the CAP theorem (consistency, availability, partition tolerance) or the blockchain trilemma is mathematically inherent to distributed systems without trusted intermediaries. The constraint appears as a law of nature. However, this perspective risks naturalizing what may be contingent architectural choices. The engine's false summit detector should identify this as potential naturalization.
constraint_indexing:constraint_classification(blockchain_scalability_trilemma, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(blockchain_scalability_trilemma_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(blockchain_scalability_trilemma, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(blockchain_scalability_trilemma, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(blockchain_scalability_trilemma, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(blockchain_scalability_trilemma, TR),
    TR >= 0.70.

:- end_tests(blockchain_scalability_trilemma_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The trilemma creates measurable extraction: L2 sequencers capture MEV (maximal extractable value), users pay bridging and rollup fees, validator economic advantages are preserved. However, the extraction is not maximal because genuine coordination functions exist (consensus provides real security guarantees) and alternative solutions are available (L2s do offer real scaling). The value increased from 0.35 to 0.52 over the interval as the L2 ecosystem matured, extractiveness accumulated through sequencer economics, and the original claim that 'scalability without trade-offs is coming' proved false. Suppression (0.65): High. Barriers to exit or alternative choices are substantial: switching blockchains entails liquidity fragmentation, bridge risks, and loss of network effects. Developing-world users cannot afford base-layer transactions regardless of their need for services. Users cannot easily exit to alternative protocols without losing composability. Suppression reflects both technical barriers (fundamental physics of distributed consensus) and institutional barriers (network effects, installed base). Theater ratio (0.58): Moderate-high. The constraint contains genuine technical content (proof-of-work security, consensus finality, data availability) but also significant theater: narrative commitment to 'immutability' and 'decentralization' as foundational values despite pragmatic acceptance of L2 centralization, validation consolidation around major exchanges, and governance concentration. Performative language about 'censorship resistance' persists even as many applications accept practical centralization via L2 sequencers.
 *
 * PERSPECTIVAL GAP:
 *   The gap between validators' rope and excluded participants' snare is maximal. Validators experience the trilemma as a coordination mechanism that preserves the security guarantees they enforce and the economic value they capture. Excluded participants experience it as pure extraction: they cannot transact at scale, cannot escape the constraint, and bear all costs. L2 developers bridge this gap by offering scalability solutions, but this bridges to tangled_rope (mixed coordination and extraction) rather than rope, because sequencer fees remain extractive. The piton perspective (PoW orthodoxy) naturalizes the constraint through narrative commitment to 'immutability' and 'decentralization maximalism' despite observing that these values are increasingly honored in form rather than substance (L2 centralization, validation consolidation, governance capture).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is determined by structural position relative to the trilemma. Base layer validators benefit from the constraint (d ≈ 0.15, low) because their validation role remains essential. Excluded participants are pure targets (d ≈ 0.95, high) because they bear transaction costs with no benefit. L2 developers have mixed positioning (d ≈ 0.55, moderate) because they solve scaling problems (beneficiary function) while capturing sequencer rents (extractive function). The analytical observer (d ≈ 0.72) derives from the assumption that cryptographic constraints are neutral facts, but the structural data reveals that institutional actors benefit from treating contingent constraints as immutable. Directionality computation: beneficiary status → lower d; trapped/constrained exit → higher d; arbitrage exit → lower d. The resulting χ values explain why different agents perceive the same constraint as rope versus snare.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: This constraint demonstrates mandatrophy resolution through perspectival decomposition. The mandatrophy arises because 'the blockchain scalability trilemma' conflates two distinct claims: (1) a mathematical law (distributed consensus without trusted parties requires trade-offs), and (2) an institutional enforcement (base layer validators benefit from preserving scalability constraints). The mathematical law appears as mountain from the analytical perspective; the institutional enforcement appears as snare from the excluded participant perspective and tangled_rope from the L2 developer perspective. The constraint is not a single type — it is a presheaf over the observation site, with each perspective yielding a different classification from identical base metrics. The engine's false summit detector should flag the mountain perspective as naturalization. The mandatrophy is resolved by recognizing that (1) is genuinely mountain-grade (immutable consequence of cryptographic protocols), while (2) is genuinely snare-grade (institutional extraction that persists as long as base layer validators benefit). L2 solutions address the mathematical constraint but not the extraction constraint — they create a new tangled_rope at the application layer (sequencer extraction) while leaving base layer validator extraction intact. True resolution requires not solving the mathematical trilemma but removing the institutional extraction — which requires breaking base layer validator monopoly on settlement through alternative consensus models or genuine permissionless sequencing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mathematical_versus_architectural_constraint,
    'Is the scalability trilemma a mathematical law or an architectural consequence of specific design choices?',
    'Analysis of alternative consensus models and settlement layer designs; comparison of trilemma severity across different blockchain architectures; identification of which components are mathematically necessary versus chosen',
    'If mathematical law: constraint is mountain-grade, unsolvable by design. If architectural: constraint is tangled_rope, solvable through engineering. Changes classification from mountain to snare/scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mathematical_versus_architectural_constraint, conceptual, 'Whether the trilemma is fundamental or architectural').

omega_variable(
    centralization_quantification_ambiguity,
    'How is ''decentralization'' measured for purposes of the trilemma? What threshold distinguishes acceptable decentralization from extractive centralization?',
    'Operational definition of decentralization metrics (number of validators, Nakamoto coefficient, geographic distribution); empirical measurement across existing protocols; agreement on threshold values from protocol governance',
    'If threshold is high (many independent validators required): few solutions exist, trilemma remains binding. If threshold is pragmatic (sufficient validation diversity): many existing L2s and sidechains solve it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(centralization_quantification_ambiguity, preference, 'Operational definition and threshold for acceptable decentralization').

omega_variable(
    security_model_dependency,
    'Do different security models (probabilistic finality, economic security, cryptographic proof) shift the trilemma''s constraint boundaries differently for different applications?',
    'Classification of use cases by security requirements; mapping of which security models suffice for each; measurement of trilemma severity under each security assumption',
    'If security requirement varies by use case: constraint is relative (different for different applications), enabling segmented solutions. If uniform security is required: constraint binds all applications equally.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(security_model_dependency, empirical, 'Whether the trilemma differs across security models and use cases').

omega_variable(
    layer_separation_effectiveness,
    'Do L2 solutions genuinely solve the trilemma or merely defer it to the settlement layer, creating a new trilemma for the L2 protocol itself?',
    'Analysis of L2 consensus mechanisms and scalability limits; measurement of whether L2 protocols face the same trilemma constraints; assessment of whether deferring to L1 settlement is a genuine solution or recursive problem-shifting',
    'If genuine solution: scaffold perspective confirmed, extractiveness should decrease as L2 ecosystem matures. If recursive: constraint shifts but does not dissipate, remaining as tangled_rope indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(layer_separation_effectiveness, empirical, 'Whether L2 solutions genuinely resolve or defer the trilemma').

omega_variable(
    extraction_mechanism_clarity,
    'Who specifically benefits from the trilemma''s enforcement, and through what mechanism? Is the constraint maintained intentionally by beneficiaries or emergent from technical limits?',
    'Institutional analysis of validator incentives, mining pool concentration, protocol governance voting patterns; economic analysis of L2 sequencer profits; comparison of claimed technical necessity versus observed institutional behavior',
    'If intentional maintenance by beneficiaries: snare or pure tangled_rope (extractive). If emergent technical constraint: rope or mountain. Affects whether the constraint serves genuine coordination or extractive rent-seeking.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_clarity, empirical, 'Whether the trilemma is maintained intentionally by beneficiaries').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(blockchain_scalability_trilemma, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bst_tr_t0, blockchain_scalability_trilemma, theater_ratio, 0, 0.42).
narrative_ontology:measurement(bst_tr_t5, blockchain_scalability_trilemma, theater_ratio, 5, 0.52).
narrative_ontology:measurement(bst_tr_t10, blockchain_scalability_trilemma, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(bst_be_t0, blockchain_scalability_trilemma, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bst_be_t5, blockchain_scalability_trilemma, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(bst_be_t10, blockchain_scalability_trilemma, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(blockchain_scalability_trilemma, enforcement_mechanism).
narrative_ontology:affects_constraint(blockchain_scalability_trilemma, layer2_sequencer_extraction).
narrative_ontology:affects_constraint(blockchain_scalability_trilemma, validator_economic_centralization).
narrative_ontology:affects_constraint(blockchain_scalability_trilemma, cross_chain_bridging_risk).

% DUAL FORMULATION NOTE:
% The scalability trilemma decomposes into multiple structurally distinct constraints. The mathematical constraint (proof-of-work finality requirements) is downstream of fundamental cryptographic limits. The institutional enforcement constraint (validator benefit from scalability limits) is a separate story with different ε. L2 sequencer extraction is downstream of both — sequencers exist to solve the mathematical constraint but extract value in doing so. Each story has its own ε and classification; they are linked via network relationships showing dependency structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(blockchain_scalability_trilemma, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
