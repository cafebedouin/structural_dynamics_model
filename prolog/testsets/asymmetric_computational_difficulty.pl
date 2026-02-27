% ============================================================================
% CONSTRAINT STORY: asymmetric_computational_difficulty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_asymmetric_computational_difficulty, []).

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
 *   constraint_id: asymmetric_computational_difficulty
 *   human_readable: Proof-of-Work Zero-Day: Asymmetric Computational Difficulty
 *   domain: technological/cryptography/distributed_systems
 *
 * SUMMARY:
 *   A proof-of-work blockchain system is designed as a pure coordination
 *   mechanism (Rope): all participants solve a cryptographic puzzle of equal
 *   difficulty, and the first to solve it earns the right to propose the next
 *   block. The system's security depends on computational cost being
 *   symmetric — attacking the network via 51% hash power acquisition should
 *   be economically infeasible. But one actor has solved the underlying
 *   mathematical problem. For them, the computational cost has collapsed to
 *   near-zero. They can now produce blocks at will, without expending the
 *   resources that honest miners must spend. This converts the Rope into a
 *   Snare: the coordination mechanism becomes an extraction mechanism. The
 *   honest miners, users, and protocol integrity all experience asymmetric
 *   computational difficulty. The zero-day possessor experiences the
 *   constraint as enabling dominance. The constraint exhibits all six DR
 *   types from different perspectives, but the structural data unmasks a
 *   false mountain: the analytical observer might naturalize proof-of-work
 *   difficulty as a law of mathematics, but the zero-day reveals it as a
 *   current epistemological limitation masquerading as ontological necessity.
 *
 * KEY AGENTS:
 *   - Zero-Day Possessor: Primary beneficiary (institutional/arbitrage) — solves the hard problem and captures near-costless mining dominance
 *   - Honest Miners: Primary victim (powerless/trapped) — expend computational resources under false assumption of symmetric difficulty
 *   - System Users: Secondary victim (powerless/trapped) — rely on computational cost of attacks being infeasible; vulnerable to double-spend and censorship
 *   - Proof-of-Work Network: Structural victim (abstract/trapped) — designed as symmetric coordination mechanism; becomes asymmetric extraction mechanism
 *   - Protocol Development Community: Mixed (organized/constrained) — benefits from coordination function but suffers latent compromise through inability to verify security properties
 *   - Consensus Mechanism Theater: Institutional actor (institutional/arbitrage) — maintains performative security narrative despite functional degradation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(asymmetric_computational_difficulty, 0.78).
domain_priors:suppression_score(asymmetric_computational_difficulty, 0.82).
domain_priors:theater_ratio(asymmetric_computational_difficulty, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(asymmetric_computational_difficulty, extractiveness, 0.78).
narrative_ontology:constraint_metric(asymmetric_computational_difficulty, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(asymmetric_computational_difficulty, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(asymmetric_computational_difficulty, snare).
narrative_ontology:human_readable(asymmetric_computational_difficulty, "Proof-of-Work Zero-Day: Asymmetric Computational Difficulty").
narrative_ontology:topic_domain(asymmetric_computational_difficulty, "technological/cryptography/distributed_systems").

domain_priors:requires_active_enforcement(asymmetric_computational_difficulty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(asymmetric_computational_difficulty, zero_day_possessor).
narrative_ontology:constraint_victim(asymmetric_computational_difficulty, proof_of_work_network).
narrative_ontology:constraint_victim(asymmetric_computational_difficulty, honest_miners).
narrative_ontology:constraint_victim(asymmetric_computational_difficulty, system_users).
narrative_ontology:constraint_victim(asymmetric_computational_difficulty, protocol_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HONEST MINER (SNARE) — Faces a proof-of-work system they believe is symmetric: all miners solve the same cryptographic puzzle with equal difficulty. Unknown to them, one actor has solved the underlying mathematical problem, reducing their computational cost to near-zero. The honest miner cannot exit the network without surrendering earned tokens and reputation. They are extractively targeted: their computational work subsidizes the zero-day possessor's near-costless block production. Maximum suppression — they have no knowledge of the asymmetry and no alternative validation mechanism.
constraint_indexing:constraint_classification(asymmetric_computational_difficulty, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SYSTEM USER (SNARE) — Relies on proof-of-work security properties: computational cost should make 51% attacks economically infeasible. With an asymmetric zero-day, the attacker can acquire 51% hash power at a fraction of the presumed cost, enabling double-spend attacks, transaction censorship, and protocol-level manipulation. Users cannot exit without abandoning holdings and cannot verify the integrity of the underlying difficulty claim. Trapped and maximally extracted.
constraint_indexing:constraint_classification(asymmetric_computational_difficulty, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: ZERO-DAY POSSESSOR (ROPE) — From their structural position, this is coordination. They have solved a mathematical problem that was supposed to be hard for everyone. They experience the proof-of-work system as enabling their dominance through coordination — they can produce blocks at will, essentially controlling block creation and transaction ordering. They benefit from the constraint's existence: without proof-of-work, their zero-day would be worthless. They have arbitrage options: sell the vulnerability, use it covertly, or weaponize it. They see the mechanism as cooperative with their interests.
constraint_indexing:constraint_classification(asymmetric_computational_difficulty, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PROTOCOL DEVELOPMENT COMMUNITY (TANGLED ROPE) — The broader cryptographic research and blockchain development community experiences this constraint as both coordination and extraction. There is a genuine coordination function: proof-of-work is a solution to Sybil resistance and Byzantine fault tolerance. But the existence of unpatched zero-days represents extractive asymmetry — researchers cannot fully verify the security properties they are building on. They are constrained by the need to maintain backward compatibility and network effects; they cannot instantly migrate to a new algorithm without community consensus. They benefit from the coordination function but suffer extraction through the latent compromise.
constraint_indexing:constraint_classification(asymmetric_computational_difficulty, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CONSENSUS MECHANISM AS INSTITUTIONAL THEATER (PITON) — The formal properties of proof-of-work (security, decentralization, Byzantine fault tolerance) are largely performative when a zero-day exists. The mechanism continues to be described and audited as if it has the claimed properties, but the functional reality is degraded. The theater persists through institutional inertia: developers, miners, and users maintain the narrative of security despite the latent compromise. This is classical piton: a former coordination mechanism (Rope) whose primary function has atrophied, maintained by theatrical performance (security audits, difficulty calculations, mining pool monitoring) rather than by actual correctness.
constraint_indexing:constraint_classification(asymmetric_computational_difficulty, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some mathematical problems are genuinely hard; cryptographic difficulty is a feature of nature, not a contingent institutional choice. The analytical observer might see proof-of-work difficulty as a natural law: you cannot get Sybil resistance without paying a computational cost. However, the existence of a zero-day undermines this classification — the hardness is not a law of nature but a current state of mathematical knowledge. The 'law' can be repealed by a mathematical breakthrough. The engine will compute this as a false summit, revealing that the naturalization of computational difficulty conflates epistemological limitation (we don't know how to solve it) with ontological necessity (it cannot be solved).
constraint_indexing:constraint_classification(asymmetric_computational_difficulty, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(asymmetric_computational_difficulty_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(asymmetric_computational_difficulty, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(asymmetric_computational_difficulty, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(asymmetric_computational_difficulty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(asymmetric_computational_difficulty, TR),
    TR >= 0.70.

:- end_tests(asymmetric_computational_difficulty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): Very high. The zero-day possessor reduces their computational cost from symmetric (equal to honest miners) to near-zero, while honest miners continue to expend full resources. This is pure extraction: the asymmetry transfers all computational work subsidy to the beneficiary. Suppression (0.82): Extremely high. The zero-day is by definition secret; victims have no knowledge of the asymmetry and no mechanism to detect it until catastrophic exploits occur. The difficulty adjustment mechanism is performative — it measures aggregate hashrate but cannot distinguish honest from subsidized hashes. Theater ratio (0.35): Low. The constraint is functionally real (actual computational asymmetry exists), not performative. However, the theater increases over time as the constraint persists undetected — audits, security reviews, and consensus monitoring all fail to detect the compromise, creating false assurance. The theater is in the verification theater (false negative audits), not in the mechanism itself.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence. The zero-day possessor sees a Rope (coordination mechanism that enables their dominance). Honest miners see a Snare (extraction mechanism that transfers their computational work to an unseen beneficiary). The protocol community sees a Tangled Rope (mixed coordination and extraction, with extraction hidden). The institutional review process sees a Piton (security theater persisting despite degraded function). The analytical observer risks seeing a Mountain (mathematical necessity of hard problems), but the structural data reveals this as false naturalization. The perspectival gaps are not disagreements about the same mechanism — they reflect genuine structural asymmetry in who bears costs and who captures benefits.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d is determined by each agent's structural position relative to the asymmetric computational difficulty. The zero-day possessor occupies d ≈ 0.0 (full beneficiary): they have solved the problem that is supposed to be hard, granting them arbitrage options (sell the exploit, use it covertly, weaponize it). Honest miners occupy d ≈ 1.0 (full target): they are trapped in the network, unable to exit without abandoning holdings, and they unknowingly subsidize the zero-day possessor's dominance. The protocol development community occupies d ≈ 0.55 (mixed): they benefit from the coordination function of proof-of-work but are extracted from through the latent compromise they cannot detect. The engine derives high f(d) for honest miners (trapped victims) and low/negative f(d) for the zero-day possessor (arbitrage beneficiary), producing extreme χ differential.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The mandatrophy (is this Rope or Snare?) is resolved by the structural data: it is a Snare. The constraint was designed as a Rope (pure coordination for Sybil resistance), but the zero-day converts it into a Snare (pure extraction). The resolution criterion is the presence of asymmetric extraction: honest miners and users bear the full computational cost they think they're paying; the zero-day possessor bears near-zero cost while capturing block production dominance. The beneficiary/victim distinction is clear: beneficiary (zero-day possessor with arbitrage), victims (honest miners and users who are trapped). The presence of a coordination function (Byzantine fault tolerance, decentralization incentive) does NOT prevent Snare classification — the asymmetry and suppression are severe enough to override any coordination benefit. The false mountain perspective is identified as such: computational hardness is not an immutable law but a current state of mathematical knowledge. If the zero-day is disclosed and the problem is re-hardened, the mechanism can revert to Rope. If the problem structure is fundamentally compromised, the protocol must migrate entirely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    zero_day_discovery_timeline,
    'When will the zero-day inevitably be discovered, and what triggers discovery: academic publication, independent rediscovery, whistleblowing, or exploitative overreach?',
    'Empirical observation of disclosure timeline; reconstruction of discovery pathway from forensic blockchain analysis; historical comparison with previous cryptographic breaches (SHA-1, MD5 collision discoveries)',
    'If discovered within 1-2 years: damage is containable through hard fork. If hidden for 5+ years: systemic compromise deepens, potential for catastrophic loss of user confidence upon disclosure. If never discovered: zero-day possessor maintains covert dominance indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(zero_day_discovery_timeline, empirical, 'Timeline and mechanism of zero-day discovery').

omega_variable(
    covert_vs_overt_extraction,
    'Is the zero-day possessor extracting covertly (mining undetectably, accumulating power), or overt exploitation (51% attacks, transaction censorship) that would reveal the compromise?',
    'Forensic blockchain analysis: transaction patterns, block distribution, hashrate concentration; temporal analysis of when exploitative behavior begins vs when constraint classification changes',
    'If covert: extraction persists longer, more value is siphoned before discovery. Snare classification is sustained. If overt: discovery is accelerated, community mobilizes faster, but damage is acute and visible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covert_vs_overt_extraction, empirical, 'Whether zero-day exploitation is covert or overt').

omega_variable(
    alternative_algorithm_viability,
    'Does an alternative proof-of-work algorithm (or post-quantum hash function) exist that can be deployed via hard fork without fragmenting the network into competing chains?',
    'Technical feasibility analysis of algorithm migration; historical precedent from other protocol upgrades; game-theoretic modeling of fork consensus dynamics',
    'If viable: snare can be escaped through coordinated community response. Constraint transitions to Scaffold (temporary problem with sunset). If not viable: network is locked into the compromised algorithm indefinitely, snare persists as structural constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_algorithm_viability, empirical, 'Whether alternative algorithms enable protocol migration').

omega_variable(
    mathematical_hardness_recoverability,
    'If the zero-day algorithm is disclosed, can the underlying mathematical problem be re-hardened through modification, or is the fundamental problem structure now permanently compromised?',
    'Cryptographic analysis of whether the solved problem admits only minor patches vs requiring complete replacement; assessment of whether the breakthrough generalizes to other hard problems in the same family',
    'If re-hardenable: protocol can recover via algorithm adjustment (Scaffold with technical sunset). If permanently compromised: requires migration to entirely different cryptographic primitive (Snare becomes structural default; protocol evolves or fails).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mathematical_hardness_recoverability, conceptual, 'Whether the solved problem can be re-hardened').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(asymmetric_computational_difficulty, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(asym_pow_tr_t0, asymmetric_computational_difficulty, theater_ratio, 0, 0.25).
narrative_ontology:measurement(asym_pow_tr_t3, asymmetric_computational_difficulty, theater_ratio, 3, 0.3).
narrative_ontology:measurement(asym_pow_tr_t6, asymmetric_computational_difficulty, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(asym_pow_be_t0, asymmetric_computational_difficulty, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(asym_pow_be_t3, asymmetric_computational_difficulty, base_extractiveness, 3, 0.72).
narrative_ontology:measurement(asym_pow_be_t6, asymmetric_computational_difficulty, base_extractiveness, 6, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(asymmetric_computational_difficulty, enforcement_mechanism).
narrative_ontology:affects_constraint(asymmetric_computational_difficulty, blockchain_51_percent_attack).
narrative_ontology:affects_constraint(asymmetric_computational_difficulty, cryptocurrency_user_asset_security).
narrative_ontology:affects_constraint(asymmetric_computational_difficulty, cryptographic_primitive_trust).

% DUAL FORMULATION NOTE:
% The proof-of-work zero-day is upstream of specific attack vectors (51% attacks, double-spend, censorship). The zero-day enables those attacks by removing the computational cost barrier. The structural constraint is the asymmetric difficulty; the downstream constraints are the exploitable attacks that follow. Extractiveness values differ: zero-day (0.78, Snare), upstream attacks (varies by attack type, but all enabled by asymmetry).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
