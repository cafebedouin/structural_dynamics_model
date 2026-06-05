% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__utility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_consensus_kernel__utility_reading, []).

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
 *   constraint_id: bitcoin_consensus_kernel__utility_reading
 *   human_readable: Bitcoin Consensus Kernel — Utility Reading (Iterative Evolution Framework)
 *   domain: cryptoeconomics/monetary_systems/distributed_consensus
 *
 * SUMMARY:
 *   Bitcoin's consensus mechanism constitutes a stabilized kernel — Satoshi
 *   Nakamoto's whitepaper specifying the Proof-of-Work consensus and the
 *   protocol rules — that different participants read as establishing
 *   fundamentally different guarantees. This constraint represents ONE
 *   reading: the utility reading, which interprets the consensus mechanism as
 *   establishing a FRAMEWORK for iterative improvement through soft forks and
 *   layer-2 protocols, rather than as establishing monetary immutability.
 *   Under this reading, the whitepaper establishes a minimum viable consensus
 *   mechanism that enables protocol evolution while preserving network
 *   coherence. This is opposed to the maximalist reading, which interprets
 *   the same whitepaper as establishing unchanging monetary properties (the
 *   21M cap, PoW algorithm, base-layer finality), and a pragmatic synthesis
 *   reading, which treats both interpretations as partially valid and seeks
 *   operational compromises. The utility reading claims that soft forks
 *   (protocol upgrades backward-compatible at the consensus level) and
 *   layer-2 protocols are legitimate evolutionary mechanisms, not breaches of
 *   the protocol. This creates a structural tension: the consensus mechanism
 *   must simultaneously enforce stability (so participants trust the network)
 *   AND permit evolution (so the protocol can adapt). This tension is the
 *   constraint. It exhibits tangled_rope characteristics because it genuinely
 *   coordinates consensus-based evolution (beneficiaries: developers,
 *   adopters seeking innovation) while extracting from those committed to
 *   protocol stasis (victims: ossification guarantees, maximalist ideological
 *   commitments). The theater_ratio (0.58) reflects the performative
 *   maintenance of the 'immutable protocol' narrative despite actual, ongoing
 *   evolution through soft forks.
 *
 * KEY AGENTS:
 *   - Protocol Developers (Core developers, Bitcoin Improvement Proposal authors): Organized beneficiaries (organized/arbitrage) — capture coordination benefits and feature-development capacity through soft-fork mechanism
 *   - Application Builders (Layer-2 teams, DeFi builders): Organized beneficiaries (organized/arbitrage) — leverage base-layer stability + protocol extensibility to build higher layers
 *   - Cryptocurrency Adopters (ordinary holders): Moderate victims/beneficiaries (moderate/constrained) — benefit from upgrades but constrained by technical literacy and coordination overhead
 *   - Maximalist Coalition (ideological commitment to immutability): Powerful victims (powerful/constrained) — experience extraction because actual protocol evolution contradicts their foundational guarantee
 *   - Monetary Ossification Guarantee (abstract commitment): Powerless victim (powerless/trapped) — cannot exit; any soft fork violates the principle
 *   - The Immutability Myth (institutional narrative): Institutional actor (institutional/arbitrage) — maintains performative work of framing contingent evolution as structural immutability
 *   - Layer-2 Scaling Infrastructure: Organized beneficiaries (organized/constrained) — see base-layer as temporary, with sunset path to independence
 *   - Analytical Observer: Universal perspective (analytical/analytical) — sees coordination mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__utility_reading, 0.38).
domain_priors:suppression_score(bitcoin_consensus_kernel__utility_reading, 0.42).
domain_priors:theater_ratio(bitcoin_consensus_kernel__utility_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__utility_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_consensus_kernel__utility_reading, "Bitcoin Consensus Kernel — Utility Reading (Iterative Evolution Framework)").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__utility_reading, "cryptoeconomics/monetary_systems/distributed_consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__utility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__utility_reading, '600e60f7-c826-4f36-ad62-d51a0ec0d17e').
narrative_ontology:cs_kernel_codification('600e60f7-c826-4f36-ad62-d51a0ec0d17e', fixed_text).
narrative_ontology:cs_authority_grounding('600e60f7-c826-4f36-ad62-d51a0ec0d17e', expertise).
narrative_ontology:cs_interpretation_layer_present('600e60f7-c826-4f36-ad62-d51a0ec0d17e').
narrative_ontology:cs_reading_relation('600e60f7-c826-4f36-ad62-d51a0ec0d17e', bitcoin_consensus_kernel__maximalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('600e60f7-c826-4f36-ad62-d51a0ec0d17e', bitcoin_consensus_kernel__pragmatic_synthesis_reading, influences).
narrative_ontology:cs_axiom('600e60f7-c826-4f36-ad62-d51a0ec0d17e', foundational, iterative_protocol_evolution_legitimate).
narrative_ontology:cs_axiom_status(iterative_protocol_evolution_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('600e60f7-c826-4f36-ad62-d51a0ec0d17e', iterative_protocol_evolution_legitimate, instrumental).
narrative_ontology:cs_axiom('600e60f7-c826-4f36-ad62-d51a0ec0d17e', foundational, base_layer_stability_sufficient_guarantee).
narrative_ontology:cs_axiom_status(base_layer_stability_sufficient_guarantee, holdable).
narrative_ontology:cs_axiom_grounding('600e60f7-c826-4f36-ad62-d51a0ec0d17e', base_layer_stability_sufficient_guarantee, conventional).
narrative_ontology:cs_reference_frame('600e60f7-c826-4f36-ad62-d51a0ec0d17e', protocol_flexibility_framework).
narrative_ontology:cs_drift_state('600e60f7-c826-4f36-ad62-d51a0ec0d17e', contemporary_institutional_adoption, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('600e60f7-c826-4f36-ad62-d51a0ec0d17e', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, protocol_developers).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, application_builders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, cryptocurrency_adopters).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, monetary_ossification_guarantees).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, base_layer_simplicity_maximalists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MONETARY OSSIFICATION GUARANTEE (SNARE) — Ideological commitment to unchanging base-layer protocol experiences this constraint as pure extraction. The guarantee cannot exit; any soft fork or protocol evolution violates the foundational premise. Trapped in the asymmetry between the consensus mechanism's actual mutability and the mythological immutability narrative. Maximum experienced extraction because the constraint forces a choice: abandon the ossification guarantee or be excluded from protocol evolution.
constraint_indexing:constraint_classification(bitcoin_consensus_kernel__utility_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MAXIMALIST COALITION (TANGLED ROPE) — Powerful actors (certain nodes, mining pools, institutional HODLers) benefit from narrative stability and protocol stasis but are constrained by technical reality: the consensus mechanism DOES evolve through soft forks. Experience mixed coordination (protocol stability) and extraction (their preferred stasis is not achievable). High power but constrained by the gap between immutability narrative and mutable reality. Chi moderately high due to powerful status + constrained exit.
constraint_indexing:constraint_classification(bitcoin_consensus_kernel__utility_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: PROTOCOL DEVELOPERS AND APPLICATION BUILDERS (ROPE) — Organized agents (Core developers, layer-2 teams, DeFi builders) see the consensus mechanism as pure coordination: the soft-fork upgrade pathway enables innovation while preserving backwards compatibility. Beneficiaries through arbitrage options (can move to layer-2, can propose soft forks, can build applications atop the evolving base layer). Low extraction experienced — the constraint solves their collective action problem of how to improve the protocol without fracturing the network.
constraint_indexing:constraint_classification(bitcoin_consensus_kernel__utility_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ORDINARY CRYPTOCURRENCY ADOPTERS (TANGLED ROPE) — Moderate power, constrained by technical literacy and exit costs (moving holdings, learning new systems). Benefit from protocol improvements (better privacy, lower fees) but constrained by the negotiation between base-layer stability and feature evolution. Experience moderate extraction because they cannot freely choose which version of Bitcoin to hold — they must accept whatever consensus emerges — but they also benefit from the coordination mechanism that prevents network fragmentation.
constraint_indexing:constraint_classification(bitcoin_consensus_kernel__utility_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: THE IMMUTABILITY MYTH (PITON) — The narrative of 'Bitcoin's unchanging protocol' persists through institutional inertia and rhetorical maintenance despite technical reality: soft forks occur regularly, consensus evolves, and the protocol has changed fundamentally (block size, witness data, fee markets, timelocks). The myth performs a function (coalition stability, narrative coherence) but the mechanism is substantially theatrical — the immutability guarantee is maintained through selective emphasis and narrative framing, not through structural reality. Theater ratio high because the performative work of maintaining the immutability myth exceeds its functional value.
constraint_indexing:constraint_classification(bitcoin_consensus_kernel__utility_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: LAYER-2 SCALING SOLUTIONS (SCAFFOLD) — Organized technical teams building Lightning Network, Stacks, Rollups see the base-layer constraints as temporary coordination problems being solved through parallel infrastructure. Structured with clear sunset logic: as layer-2 throughput matures, dependence on base-layer scalability degrades. Low extraction because the builders have agency and see an exit path (their infrastructure becomes the primary transaction layer). This perspective validates the utility reading: the consensus mechanism enables layer-2 evolution by remaining stable AND permitting soft forks.
constraint_indexing:constraint_classification(bitcoin_consensus_kernel__utility_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / COORDINATION VIEW (ROPE) — From a civilizational, universal perspective, the consensus mechanism solves a genuine coordination problem: how to align decentralized actors on a shared protocol state without central authority. The soft-fork mechanism is pure coordination (supermajority upgrade with backwards compatibility). Low extraction, high coordination function. The analytical observer sees the constraint as enabling iterative improvement rather than preventing it.
constraint_indexing:constraint_classification(bitcoin_consensus_kernel__utility_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_consensus_kernel__utility_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bitcoin_consensus_kernel__utility_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bitcoin_consensus_kernel__utility_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(bitcoin_consensus_kernel__utility_reading, TR),
    TR >= 0.70.

:- end_tests(bitcoin_consensus_kernel__utility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The utility reading establishes a framework for evolution that genuinely solves coordination problems (how to upgrade without forking, how to add features without centralizing decisions). This is authentic coordination value. However, the evolution capacity also extracts from those committed to protocol stasis — the maximalist reading's commitment to immutability is incompatible with soft forks, creating asymmetric exposure. The extractiveness is not as high as a pure snare (0.46+) because the evolution mechanism offers real benefits to most participants (innovation, scalability pathways). But it exceeds pure rope (0.35) because it systematically advantages those with technical expertise and development capital (who can propose and implement soft forks) over passive holders. The 0.38 value reflects the moderate extraction embedded in 'beneficial innovation that concentrates power in developer hands.' Suppression (0.42): Moderate. The consensus mechanism suppresses alternatives through the network effect (difficult to fork away because all value is in the majority chain) and through the coordination threshold (changing consensus requires supermajority agreement). However, this suppression is not total — hard forks have historically occurred (Bitcoin Cash, Bitcoin SV), and alternative implementations exist. The suppression reflects the genuine cost of exit (joining a minority fork), not absolute imprisonment. Theater ratio (0.58): Moderate-high. The 'immutable protocol' narrative is substantially performative. Soft forks have been happening since 2012 (P2SH, witness data, taproot, etc.), yet the community maintains the fiction that Bitcoin is an unchanging artifact. The theatrical work involves: selective emphasis on which changes 'count' (soft forks are evolution; hard forks are 'forks'), reliance on technical jargon to obscure that soft forks DO change the protocol, and institutional promotion of the immutability myth by exchanges, custodians, and advocates. The theater serves a function (coalition stability), but the gap between narrative and reality is substantial. Trajectory (t0→t10): extractiveness, suppression, and theater all increase over the measurement interval, reflecting accumulated soft forks (extracting from those locked into the immutability guarantee), increased network effect (raising suppression), and increased institutional promotion of the immutability narrative (raising theater). This is consistent with a constraint that is slowly shifting from rope-like (early coordination) toward snare-like (accumulated extraction + myth maintenance).
 *
 * PERSPECTIVAL GAP:
 *   The utility reading produces a maximum perspectival gap because the same consensus mechanism appears as pure coordination to developers (rope), as mixed coordination-extraction to ordinary adopters (tangled rope), as extraction from stasis commitment (snare or tangled rope), and as degraded ritual (piton) from the institutional narrative perspective. The gap reveals that the constraint's type depends entirely on whether the observer values protocol evolution or protocol stasis. The utility reading assumes evolution is good; the maximalist reading assumes stasis is good. This is not a disagreement resolvable by empirical facts — it is a disagreement about what Bitcoin is for. The analytical observer's rope classification (viewing the constraint as pure coordination) privileges the utility reading's assumptions. An analytical observer aligned with the maximalist reading would classify as snare or mountain (the immutability constraint is foundational/unchangeable).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from the agent's structural position relative to the consensus mechanism. Beneficiaries with arbitrage options (developers, layer-2 builders) experience d ≈ 0.15-0.25, yielding low f(d) and low effective extraction. Agents with constrained exits who experience the evolution as incompatible with their commitments (maximalists, ossification guarantees) experience d ≈ 0.75-0.95, yielding high f(d) and high extraction. Ordinary adopters with moderate power and constrained exits but partial benefits experience d ≈ 0.55-0.65, yielding moderate extraction. The utility reading's directionality depends on the question: 'For whom is this mechanism beneficial?' The reading assumes the answer is 'protocol developers and network participants seeking innovation,' hence the beneficiary designations. But the maximalist reading would assert the answer is 'those committed to monetary stability,' flipping the direction. This disagreement about directionality is NOT resolved by the math — it is the substantive dispute about what the consensus mechanism is for.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by making explicit that the classification depends on which reading of the kernel you adopt. Under the utility reading (evolution is legitimate), the constraint is tangled_rope: it genuinely coordinates protocol improvements while extracting from those committed to stasis. Under the maximalist reading (immutability is foundational), the constraint is snare: the consensus mechanism is a mechanism for accumulating soft forks that violate the foundational guarantee. Under the pragmatic synthesis reading (both evolution and stability matter), the constraint is rope with occasional snare moments: the coordination function dominates, but periods of contentious soft forks (which community members disagree on) produce snare-like extraction. The mandatrophy is not 'which reading is correct?' but 'which kernel interpretation do you commit to?' The utility reading makes this choice transparent: by establishing a framework for iterative evolution, it commits to prioritizing innovation and developer agency over static guarantees.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    soft_fork_precedent_boundary,
    'At what point does iterative soft-fork evolution constitute a breach of the ''immutable protocol'' guarantee, vs. legitimate backward-compatible evolution?',
    'Counterfactual analysis: if Bitcoin had adopted a hypothetical protocol feature incompatible with the original design (e.g., monetary expansion, removal of the 21M cap, change to PoW algorithm), would consensus-based adoption make it legitimate under the utility reading? If yes, the boundary is purely pragmatic (what consensus permits). If no, there is a hidden hard constraint (certain properties ARE immutable). This determines whether the utility reading is a genuine ethical position or a cover story for extractive incrementalism.',
    'If boundary is purely pragmatic: the utility reading is structurally coherent; the constraint is genuine tangled_rope (coordination + evolution). If boundary is hidden: the constraint is closer to snare (the ossification guarantee is real but unenforceable, creating extraction). Affects classification of all perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(soft_fork_precedent_boundary, conceptual, 'Boundary between legitimate evolution and breach of immutability guarantee').

omega_variable(
    beneficiary_extraction_asymmetry,
    'Do protocol developers and layer-2 builders capture disproportionate value from the consensus mechanism''s iterative evolution capacity, extracting from ordinary adopters who bear the coordination costs (transaction confirmation delays, software update burden, consensus disagreements)?',
    'Value distribution analysis: comparison of fee/profit accrual to developers vs. ordinary users; empirical study of who proposes soft forks (likely correlation with technical expertise and capital access); measurement of adoption costs (software updates, learning, wallet migrations) vs. benefit access (new features available to all or only to early adopters with technical literacy). If developers/builders capture 70%+ of value from upgrades, extraction is real. If benefits distribute broadly, coordination is primary.',
    'If extraction is asymmetric: the utility reading naturalizes a developer-beneficiary arrangement (should reclassify from rope to snare for some beneficiary subgroups). If symmetric: utility reading is structurally honest. Affects beneficiary/victim declarations and directionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_extraction_asymmetry, empirical, 'Whether protocol evolution benefits distribute symmetrically or concentrate among developers').

omega_variable(
    kernel_reading_foreclosure,
    'Does accepting iterative evolution (utility reading) logically foreclose the maximalist reading''s core claim that Bitcoin is a non-inflationary, immutable store of value? Or do these readings coexist as different parties'' commitments?',
    'Conceptual analysis of what each reading commits to: the utility reading commits to protocol flexibility and acknowledges that soft forks change the protocol (and thus the guarantees). The maximalist reading commits to immutability as the core feature. If they hold mutually exclusive premises about protocol mutability, they foreclose. If they are held by different coalition members without logical contradiction, they coexist. Empirical observation: holders of both readings coexist in the Bitcoin community, suggesting coexistence rather than foreclosure. But the readings may be held in tension, not in full logical independence.',
    'If forecloses: the utility reading is dominant; maximalist commitment is abandoned at the kernel level. If coexists: both readings survive as live positions. Determines cs_structure.reading_relations entries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether utility reading forecloses maximalist reading at the kernel level').

omega_variable(
    ossification_counterfactual,
    'If Bitcoin had adopted true protocol ossification (no soft forks, no parameter changes, no possible evolution), would it have maintained greater network value and legitimacy, or would it have degraded into a museum artifact?',
    'Counterfactual historical modeling: compare Bitcoin''s adoption curve, transaction volume, and institutional legitimacy against hypothetical timeline where soft forks were forbidden. Secondary evidence: comparison to actually-ossified systems (some altcoins, disabled protocols) vs. protocols that enabled evolution (Ethereum''s upgrades despite ossification arguments). The maximalist reading commits to ossification superiority; the utility reading commits to controlled evolution. Empirical resolution depends on whether evolution or stasis better predicts network robustness.',
    'If stasis would have succeeded better: the utility reading is false; ossification is the superior commitment. If evolution was necessary: utility reading is structurally justified. This determines whether the constraint is genuinely coordinative (soft forks solve a real problem) or extractive (evolution enables insider capture). Affects the baseline type classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ossification_counterfactual, empirical, 'Whether protocol ossification or evolution better predicts network success').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__utility_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(btc_util_theater_t0, bitcoin_consensus_kernel__utility_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(btc_util_theater_t5, bitcoin_consensus_kernel__utility_reading, theater_ratio, 5, 0.54).
narrative_ontology:measurement(btc_util_theater_t10, bitcoin_consensus_kernel__utility_reading, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(btc_util_extract_t0, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(btc_util_extract_t5, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(btc_util_extract_t10, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 10, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(btc_util_suppress_t0, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(btc_util_suppress_t5, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(btc_util_suppress_t10, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 10, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__utility_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel__maximalist_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel__pragmatic_synthesis_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, proof_of_work_energy_efficiency).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, soft_fork_consensus_threshold).

% DUAL FORMULATION NOTE:
% The bitcoin_consensus_kernel is a contested kernel with three formalized readings: utility_reading (this constraint), maximalist_reading, and pragmatic_synthesis_reading. Each reading is a distinct constraint story with its own ε, beneficiary/victim structure, and perspectives. The readings coexist as live positions in the Bitcoin community, with different coalitions committed to each. All three stories share the same kernel (the whitepaper) but interpret its implications differently. The utility_reading is upstream of soft_fork_consensus_threshold (the mechanism by which soft forks are validated) and affects proof_of_work_energy_efficiency (because soft forks can change or constrain the energy requirements). The three reading stories are linked via network.affects_constraints bidirectionally to show their interdependence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_consensus_kernel__utility_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
