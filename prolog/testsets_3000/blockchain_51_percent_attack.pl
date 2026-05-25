% ============================================================================
% CONSTRAINT STORY: blockchain_51_percent_attack
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_blockchain_51_percent_attack, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: blockchain_51_percent_attack
 *   human_readable: 51% Attack Vulnerability in Proof-of-Work Blockchains
 *   domain: cryptography/distributed_systems/economic_security
 *
 * SUMMARY:
 *   A 51% attack in a proof-of-work blockchain occurs when a single actor or
 *   coalition controls more than half of the network's cumulative hashpower.
 *   This grants the attacker the ability to monopolize block production,
 *   reverse transactions (double-spend attacks), and exclude legitimate
 *   transactions from the chain. The constraint is a structural vulnerability
 *   inherent to proof-of-work consensus at scale, but its severity depends
 *   entirely on the economic relationship between hashpower acquisition cost
 *   and extractable value. The constraint exhibits snare characteristics
 *   (high extraction, high suppression, victims trapped) but also shows
 *   tangled rope properties for moderate attackers who benefit from
 *   participating in honest consensus while retaining optionality to attack.
 *   The analytical observer risks seeing an immutable cryptographic law
 *   (mountain) when the vulnerability is actually a design choice that
 *   alternative consensus models can avoid.
 *
 * KEY AGENTS:
 *   - Network Users: Primary victims (powerless/trapped) — cannot exit without abandoning the blockchain; bear full cost of double-spend attacks and lost transaction finality
 *   - Honest Mining Nodes: Secondary victims (moderate/constrained) — contribute to security but face economic extraction through chain reorgs and block orphaning
 *   - Attacker with 51% Hashpower: Primary beneficiary (institutional/arbitrage) — has full optionality to exploit or maintain consensus; captures extracted value and monopolistic mining rewards
 *   - Mining Pool Operators: Secondary beneficiaries (institutional/arbitrage) — can accumulate hashpower concentration and benefit from majority control; also vulnerable to displacement by attackers
 *   - Nation State / Industrial Cartel: Powerful beneficiary (powerful/mobile) — can accumulate 51% through subsidized electricity or manufacturing control; extract maximum value while maintaining strategic optionality
 *   - Protocol Governance Layer: Institutional maintainer (institutional/arbitrage) — performs performative security governance through difficulty adjustment; cannot fully eliminate vulnerability through code
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a design choice (PoW consensus) as immutable cryptographic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(blockchain_51_percent_attack, 0.78).
domain_priors:suppression_score(blockchain_51_percent_attack, 0.72).
domain_priors:theater_ratio(blockchain_51_percent_attack, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(blockchain_51_percent_attack, extractiveness, 0.78).
narrative_ontology:constraint_metric(blockchain_51_percent_attack, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(blockchain_51_percent_attack, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(blockchain_51_percent_attack, snare).
narrative_ontology:human_readable(blockchain_51_percent_attack, "51% Attack Vulnerability in Proof-of-Work Blockchains").
narrative_ontology:topic_domain(blockchain_51_percent_attack, "cryptography/distributed_systems/economic_security").

domain_priors:requires_active_enforcement(blockchain_51_percent_attack).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(blockchain_51_percent_attack, attacker_with_hashpower_majority).
narrative_ontology:constraint_beneficiary(blockchain_51_percent_attack, mining_pool_operators).
narrative_ontology:constraint_victim(blockchain_51_percent_attack, network_users).
narrative_ontology:constraint_victim(blockchain_51_percent_attack, transaction_integrity).
narrative_ontology:constraint_victim(blockchain_51_percent_attack, consensus_security).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NETWORK USER (SNARE) — Users cannot exit the vulnerability without abandoning the blockchain entirely. Transaction finality is illusory during an active 51% attack; double-spend attacks can reverse transactions. Users bear the full extraction cost (stolen value, transaction reversal) with no recourse. Trapped by economic dependence on the network and inability to verify consensus validity independently.
constraint_indexing:constraint_classification(blockchain_51_percent_attack, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: HONEST MINING NODE (TANGLED ROPE) — Honest miners contribute to network security (coordination function) but face existential economic extraction. A 51% attacker can reorg the chain retroactively, orphaning honest miners' blocks and stealing their rewards. The constraint requires active enforcement (proof-of-work difficulty adjustment) to remain functional. Extraction is high but not total — honest miners still earn rewards during normal operation.
constraint_indexing:constraint_classification(blockchain_51_percent_attack, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ATTACKER WITH 51% HASHPOWER (ROPE) — Perceives the vulnerability as a coordination mechanism: achieving consensus majority requires 51% of hashpower, which they possess. From the attacker's view, the constraint enables rather than inhibits their objectives. They experience maximal arbitrage — they can choose whether to enforce the constraint honestly or exploit it. Net beneficiary with full optionality.
constraint_indexing:constraint_classification(blockchain_51_percent_attack, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PROTOCOL GOVERNANCE LAYER (PITON) — The theoretical mitigation (proof-of-work difficulty adjustment, transaction finality rules, economic incentive alignment) persists as largely performative governance. The constraint cannot be fully eliminated through code alone — it requires either (a) continuous economic incentivization of honest behavior, or (b) capitulation to the attacker's extracted value. The governance layer performs 'security' through difficulty adjustment while remaining structurally vulnerable to economic attacks. Theater ratio reflects persistent claims of 'security through decentralization' despite known attack vectors.
constraint_indexing:constraint_classification(blockchain_51_percent_attack, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: NATION STATE / INDUSTRIAL CARTEL (TANGLED ROPE) — A powerful state with access to subsidized electricity, custom ASIC manufacturing, or control of major mining pools can accumulate 51% hashpower. From their perspective, the constraint is a coordination tool (they can enforce a version of consensus) paired with massive extraction (they can seize value from all users). They have some exit options (cease attack, reallocate hashpower) but the economic incentives for extraction are extreme. Extractiveness is maximized but constrained by the cost of maintaining secrecy and hashpower accumulation.
constraint_indexing:constraint_classification(blockchain_51_percent_attack, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / CRYPTOGRAPHIC LIMIT (MOUNTAIN) — From a cryptographic security perspective, the 51% vulnerability is inherent to any proof-of-work system: if consensus is defined as 'majority hashpower,' then controlling the majority necessarily enables consensus. This appears as an immutable property of the consensus mechanism. However, the engine's false summit detector will flag this — the vulnerability is not cryptographically unavoidable (alternative consensus models exist: proof-of-stake, Byzantine fault tolerance), so the mountain classification reflects a naturalization of a design choice, not a true natural law.
constraint_indexing:constraint_classification(blockchain_51_percent_attack, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(blockchain_51_percent_attack_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(blockchain_51_percent_attack, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(blockchain_51_percent_attack, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(blockchain_51_percent_attack, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(blockchain_51_percent_attack, TR),
    TR >= 0.70.

:- end_tests(blockchain_51_percent_attack_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): Very high. A 51% attacker can seize value from all network users through double-spend attacks, transaction exclusion, and reorganization of historical blocks. The extractiveness increases over time as networks accumulate more value (T=0: 0.55 → T=9: 0.78), making attacks more economically attractive. The constraint's extraction efficiency approaches theoretical maximum because the attacker's hashpower majority directly translates to unilateral control over consensus. Suppression (0.72): High. Users cannot verify chain validity independently without replicating full consensus computation. Honest miners cannot prevent reorgs. Transactions lack true finality until many blocks are buried. The only exit is network abandonment or migration to a more-secure consensus model — both costly and imperfect. Theater ratio (0.35): Low. Unlike the verification bottleneck constraint, the 51% attack has little performative component — the vulnerability is functionally real and directly extractive. The low theater reflects that the constraint operates through direct economic incentives rather than institutional ritual. However, theater increases over time (0.25 → 0.35) as networks employ difficulty adjustment, consensus finality rules, and other performative security measures that do not fully eliminate the vulnerability.
 *
 * PERSPECTIVAL GAP:
 *   The gap between user (snare) and attacker (rope) perspectives is maximal: they perceive opposing constraint types from the same structural base. This reveals that the constraint is not a natural law but an economic asymmetry. If the network migrates to proof-of-stake (different consensus model), the gap collapses because both perspectives would shift: users would perceive rope (security through validator stake), attackers would perceive snare (capital requirement replaces hashpower). The gap is therefore a diagnostic of PoW consensus design, not of cryptographic inevitability.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are extreme: users at d ≈ 0.95 (trapped victims with no exit), attackers at d ≈ 0.05 (full beneficiaries with arbitrage). The sigmoid f(d) produces correspondingly extreme effective extractiveness (χ). For a powerless user at global scope with trapped exit, the engine computes: d=0.95 → f(d)≈1.42 → χ = 0.78 × 1.42 × 1.2 ≈ 1.33 (extraction exceeds base metric due to scope amplification and victim status). For the attacker at institutional power with arbitrage exit and global scope: d=0.05 → f(d)≈-0.12 → χ = 0.78 × (-0.12) × 1.2 ≈ -0.11 (negative extraction, indicating beneficiary status). The directionality chain correctly captures that the constraint's structure is perfectly asymmetric — the beneficiary's gain is the victim's loss, with no coordination benefit for any agent except the attacker.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The mandatrophy is resolved by recognizing that the 51% attack is not a single constraint but a family of four structurally distinct constraints: (1) consensus_monopoly_possibility (ε ≈ 0.05, mountain, cryptographically immutable), (2) economic_extraction_incentive (ε ≈ 0.72, snare, contingent on value), (3) user_vulnerability_to_double_spend (ε ≈ 0.78, snare, structural to PoW design), and (4) consensus_model_migration_pathway (ε ≈ 0.15, scaffold, enables exit through PoS or BFT). The present story focuses on constraint #2-3 (the economic snare). The 51% attack as experienced by users is a snare, not a mountain, because alternative consensus models provide genuine exit (PoS, BFT) without catastrophic value destruction. The mountain classification (analytical perspective) naturalizes PoW as the only option, but this is false — PoW is a design choice with known vulnerabilities, not a cryptographic necessity. The mandatrophy resolves by decomposing the natural-language 'blockchain security' concept into these four distinct structural claims, each with its own extractiveness value and classification. The snare (extractive consensus vulnerability) should not be confused with mountain (cryptographic inevitability).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    attack_cost_vs_extraction_value,
    'For a given network, does the cost of accumulating 51% hashpower exceed the maximum extractable value through double-spend attacks?',
    'Economic analysis: compute hashpower acquisition cost (electricity + hardware + opportunity cost) vs maximum double-spend value (transaction volume × reorg depth). Compare across networks of different sizes.',
    'If cost > value: 51% attack is economically irrational, constraint approximates mountain (immutable due to economic logic). If cost ≤ value: constraint is a pure snare (extraction exceeds defense cost), attack is rational.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(attack_cost_vs_extraction_value, empirical, 'Whether 51% attack cost exceeds extractable value').

omega_variable(
    attacker_rationality_assumption,
    'Do attackers behave as rational economic agents maximizing short-term extraction, or do they pursue longer-term strategic goals (network disruption, competitor sabotage, political control) that change the cost-benefit calculus?',
    'Game-theoretic analysis with non-economic payoffs; historical analysis of observed attacks (Ethereum DAO fork, Bitcoin Classic debates); comparison of attack frequency against purely economic predictions.',
    'If rational economic: attacks occur only when cost < value, and constraint oscillates between rope (too expensive) and snare (value-extracting). If strategic payoffs: attacks occur unpredictably, constraint is persistently snare regardless of economic calculation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attacker_rationality_assumption, conceptual, 'Attacker rationality model and non-economic strategic payoffs').

omega_variable(
    mining_pool_centralization_feedback,
    'Does mining pool concentration (which increases 51% attack feasibility) create a feedback loop where users migrate to more-secure networks, further concentrating remaining hashpower on vulnerable networks?',
    'Network dynamics analysis: track hashpower distribution over time; correlate pool concentration with network switching events (hard forks, migration to alternative consensus models). Identify whether liquidity migration accelerates after concentration increases.',
    'If feedback exists: vulnerable networks experience rapid extraction followed by collapse. Constraint transitions from snare to defunct. If no feedback: concentration is slow and manageable; constraint remains stable tangled_rope for moderate attackers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mining_pool_centralization_feedback, empirical, 'Feedback between mining concentration and user migration').

omega_variable(
    consensus_model_substitution,
    'Is the 51% vulnerability inherent to proof-of-work consensus, or is it a design choice that can be replaced by alternative consensus models without loss of decentralization properties?',
    'Comparative analysis of consensus models (PoW, PoS, DPoS, BFT variants); evaluation of security guarantees and centralization risks for each. Assessment of whether users can migrate without catastrophic value destruction.',
    'If alternatives are viable: vulnerability is not mountain (not immutable), and constraint is a snare that can be exited through consensus model migration. If alternatives introduce worse centralization: PoW remains the least-bad option, and constraint approximates mountain (unavoidable trade-off).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consensus_model_substitution, conceptual, 'Consensus model substitutability and exit pathways').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(blockchain_51_percent_attack, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(b51_tr_t0, blockchain_51_percent_attack, theater_ratio, 0, 0.25).
narrative_ontology:measurement(b51_tr_t3, blockchain_51_percent_attack, theater_ratio, 3, 0.3).
narrative_ontology:measurement(b51_tr_t6, blockchain_51_percent_attack, theater_ratio, 6, 0.33).
narrative_ontology:measurement(b51_tr_t9, blockchain_51_percent_attack, theater_ratio, 9, 0.35).

% Extraction over time
narrative_ontology:measurement(b51_be_t0, blockchain_51_percent_attack, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(b51_be_t3, blockchain_51_percent_attack, base_extractiveness, 3, 0.68).
narrative_ontology:measurement(b51_be_t6, blockchain_51_percent_attack, base_extractiveness, 6, 0.75).
narrative_ontology:measurement(b51_be_t9, blockchain_51_percent_attack, base_extractiveness, 9, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(blockchain_51_percent_attack, enforcement_mechanism).
narrative_ontology:affects_constraint(blockchain_51_percent_attack, blockchain_consensus_security).
narrative_ontology:affects_constraint(blockchain_51_percent_attack, mining_pool_centralization).
narrative_ontology:affects_constraint(blockchain_51_percent_attack, proof_of_stake_adoption).

% DUAL FORMULATION NOTE:
% The 51% attack is a member of the blockchain security constraint family. It is structurally downstream of mining_pool_centralization (concentration enables attacks) and upstream of proof_of_stake_adoption (PoS consensus is a direct alternative that eliminates the attack vector). The constraint has also been decomposed into separate empirical and cryptographic formulations to avoid false mountain classification. See consensus_monopoly_possibility (ε=0.05, mountain) for the cryptographic perspective and blockchain_51_percent_attack (ε=0.78, snare) for the economic extraction perspective.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
