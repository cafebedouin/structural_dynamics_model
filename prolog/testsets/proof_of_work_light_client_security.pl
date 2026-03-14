% ============================================================================
% CONSTRAINT STORY: proof_of_work_light_client_security
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_proof_of_work_light_client_security, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: proof_of_work_light_client_security
 *   human_readable: Proof-of-Work Light Client Security Trade-off
 *   domain: blockchain_cryptography/distributed_systems
 *
 * SUMMARY:
 *   Light client security in proof-of-work blockchains creates a structural
 *   tension between accessibility (clients with limited resources need to
 *   verify transactions without downloading and validating the entire
 *   blockchain) and security assurance (full verification requires access to
 *   complete state and transaction history). The constraint requires light
 *   clients to trust cryptographic proofs of work without validating the
 *   work's relevance to transaction ordering or state validity. This creates
 *   extractive capacity: full node operators and mining pools benefit from
 *   light clients' acceptance of header-only verification, which elevates the
 *   structural importance of full nodes and enables mining pools to maintain
 *   consensus on longest-chain without transaction-level visibility.
 *   Simultaneously, the constraint exhibits genuine coordination function:
 *   the PoW mechanism does solve the Byzantine consensus problem, and light
 *   clients do achieve probabilistic security guarantees against certain
 *   attack classes. However, these guarantees degrade rapidly against
 *   adversaries with modest hash power or network-level capabilities. The
 *   constraint is tangled: coordination exists, extraction is real, and
 *   suppression (barriers to full verification) is high due to device and
 *   bandwidth constraints.
 *
 * KEY AGENTS:
 *   - Light Client Users: Primary victim (powerless/trapped) — resource-constrained devices with no practical exit; vulnerable to attacks exploiting header-only validation
 *   - SPV-Capable Users: Secondary victim (moderate/constrained) — can implement simplified payment verification at bandwidth/latency cost; face eclipse and Sybil attack risks
 *   - Full Node Operators: Primary beneficiary (institutional/arbitrage) — elevated structural importance from light client dependency; can arbitrage to alternative consensus mechanisms
 *   - Mining Pools: Secondary beneficiary (organized/mobile) — coordinate with full nodes to maintain consensus; benefit from light client acceptance of header validation
 *   - Light Client Protocol Coalition: Organized actor (organized/constrained) — developing stateless clients and proof systems to reduce PoW dependency; represent sunset pathway
 *   - Legacy PoW Framework: Institutional practice (institutional/arbitrage) — perpetuates header-only verification through inertia; seen as degraded by protocol developers
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the PoW header dependency as information-theoretic inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(proof_of_work_light_client_security, 0.58).
domain_priors:suppression_score(proof_of_work_light_client_security, 0.65).
domain_priors:theater_ratio(proof_of_work_light_client_security, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(proof_of_work_light_client_security, extractiveness, 0.58).
narrative_ontology:constraint_metric(proof_of_work_light_client_security, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(proof_of_work_light_client_security, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(proof_of_work_light_client_security, tangled_rope).
narrative_ontology:human_readable(proof_of_work_light_client_security, "Proof-of-Work Light Client Security Trade-off").
narrative_ontology:topic_domain(proof_of_work_light_client_security, "blockchain_cryptography/distributed_systems").

domain_priors:requires_active_enforcement(proof_of_work_light_client_security).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(proof_of_work_light_client_security, full_node_operators).
narrative_ontology:constraint_beneficiary(proof_of_work_light_client_security, mining_pools).
narrative_ontology:constraint_beneficiary(proof_of_work_light_client_security, protocol_core_developers).
narrative_ontology:constraint_victim(proof_of_work_light_client_security, light_client_users).
narrative_ontology:constraint_victim(proof_of_work_light_client_security, resource_constrained_devices).
narrative_ontology:constraint_victim(proof_of_work_light_client_security, network_security_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LIGHT CLIENT USER (SNARE) — Constrained by device resources (mobile, embedded hardware) with no material exit. Forced to trust assumptions about consensus validity that may not hold. Vulnerable to long-range attacks, timestamp manipulation, and censorship via block withholding. No capacity to verify underlying PoW or reorganize the trust model.
constraint_indexing:constraint_classification(proof_of_work_light_client_security, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SPV-CAPABLE USER (TANGLED ROPE) — Can run simplified payment verification but faces bandwidth and latency costs. Benefits from the PoW security model (legitimate verification of work) alongside extraction: vulnerable to Sybil attacks on block header relay, susceptible to eclipse attacks that control information flow. High cost to switch verification models but not impossible.
constraint_indexing:constraint_classification(proof_of_work_light_client_security, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: FULL NODE OPERATOR (ROPE) — Operates the verification backbone with full PoW validation capability. Primary beneficiary from the light client security model: light clients' acceptance of PoW proofs elevates full nodes' structural importance and their ability to set consensus rules. Can arbitrage to different consensus mechanisms or exit by running alternative chains.
constraint_indexing:constraint_classification(proof_of_work_light_client_security, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MINING POOL OPERATOR (ROPE) — Benefits from light client acceptance of header-only validation: pools can maintain consensus on longest chain without light clients verifying individual transactions. Coordination function is genuine (pools solve the variance reduction problem). Mobile exit (can switch consensus mechanisms or redirect hash power).
constraint_indexing:constraint_classification(proof_of_work_light_client_security, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LIGHT CLIENT PROTOCOL COALITION (SCAFFOLD) — Organized development of stateless clients, light sync protocols, and proof systems aiming to replace or augment PoW security assumptions. Low effective extraction because the coalition has agency and a sunset clause: as proof systems mature (SNARKs, STARKs for execution), the dependency on full PoW validation can diminish. Coordination function: advancing scalable verification.
constraint_indexing:constraint_classification(proof_of_work_light_client_security, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY POW ASSUMPTION FRAMEWORK (PITON) — The assumption that light clients can achieve 'good enough' security by verifying only block headers against cumulative work persists as institutional practice despite known failure modes (selfish mining in header-only verification, timestamp malleability). Theater ratio 0.48: the framework performs security verification rituals (checking header work, validating signatures) but does not catch sophisticated attacks that exploit light client blindness to transaction selection and ordering. Maintained through inertia as alternatives (full validation, proof systems) are not yet mature or ubiquitous.
constraint_indexing:constraint_classification(proof_of_work_light_client_security, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / INFORMATION-THEORETIC VIEW (MOUNTAIN) — From a universal/civilizational perspective, any verification method that does not process the full state has bounded security: information not observed cannot be verified. Light clients by definition discard transaction data, so their security margin against state-manipulation attacks is structurally bounded by the work required to reorganize headers alone. This appears as a natural law. However, this perspective risks naturalizing what is contingent: the choice to use PoW headers rather than cryptographic proofs (SNARKs) for light client validation is institutional, not inevitable. The engine's false summit detector will flag this classification.
constraint_indexing:constraint_classification(proof_of_work_light_client_security, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(proof_of_work_light_client_security_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(proof_of_work_light_client_security, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(proof_of_work_light_client_security, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(proof_of_work_light_client_security, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(proof_of_work_light_client_security, TR),
    TR >= 0.70.

:- end_tests(proof_of_work_light_client_security_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Light clients systematically underestimate the cost of their security model by not validating transaction selection, ordering, or timestamp validity. The 2-3 year measurement trajectory shows increasing extractiveness as attack sophistication (selfish mining, eclipse attacks) becomes better understood in the research literature, but is not yet widely mitigated in deployed light clients. Suppression (0.65): High. Device resource constraints (memory, bandwidth, CPU) are real and non-trivial to overcome. However, the suppression includes an internalized component: users accept 'good enough' security through narrative rather than from structural necessity alone. Theater ratio (0.48): Moderate. PoW header validation performs genuine cryptographic checks (difficulty adjustment, signature verification) but does not execute transaction verification or state validation — the core functions that would prevent double-spend attacks against light clients. The theater has remained stable because the performance model has not changed, though attack surface understanding has improved. The constraint exhibits increasing extractiveness paired with stable theater, indicating that the extraction is becoming more salient rather than more theatrical.
 *
 * PERSPECTIVAL GAP:
 *   The snare-to-rope gap reveals the core extraction mechanism. Light client users (snare) perceive immutable constraint at biographical timescale: they cannot transition to full client validation within a single career/device lifecycle without significant cost. Full node operators (rope) perceive the constraint as changeable coordination: they can always migrate to other consensus mechanisms or adjust PoW parameters. This gap is not merely perceptual — it reflects real structural asymmetry. The trapped user has limited agency; the institutional operator has multiple exit routes. The piton perspective adds a diagnostic insight: the PoW validation ritual is theater because it checks work-proofs without validating work relevance to transaction selection. Reviewers (full nodes) see signatures and difficulty but cannot verify whether the miner actually followed transaction ordering rules or timestamp constraints. The ritual persists through institutional inertia — alternative verification models (proof systems, staking) are not yet mature enough to replace it.
 *
 * DIRECTIONALITY LOGIC:
 *   Extracted from base_properties beneficiary/victim declarations and structural analysis. Full node operators: beneficiary + arbitrage exit → d ≈ 0.15 → f(d) ≈ -0.01 → negative/near-zero chi. Mining pools: beneficiary + mobile exit → d ≈ 0.18 → f(d) ≈ 0.03 → near-zero chi. Light client users: victim + trapped exit → d ≈ 0.93 → f(d) ≈ 1.40 → high chi (amplified by global scope σ=1.2 to χ ≈ 0.97). SPV users: victim + constrained exit + moderate power → d ≈ 0.68 → f(d) ≈ 1.08 → moderate-high chi. Protocol coalition: beneficiary (of improved verification) + constrained exit → d ≈ 0.45 → f(d) ≈ 0.50 → low-moderate chi. Piton classification derives from theater_ratio ≥ 0.70 gate failure (0.48 < 0.70), not from high chi — piton is appropriate here because the constraint is institutionally maintained but empirically degraded.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is genuinely tangled: it coordinates consensus (rope function) while extracting security margin (snare function). No single type subsumes the others. The beneficiary perspective (rope) is correct about the coordination aspect — PoW does solve Byzantine consensus. The victim perspective (snare) is correct about the extraction aspect — light clients bear unavoidable risk from unvalidated state. The analytical observer who sees 'information-theoretic mountain' is partially correct about structural bounds but risks naturalizing what is actually a contingent choice (PoW headers rather than proofs) as inevitable. The scaffold perspective resolves the mandatrophy by identifying the sunset pathway: as proof systems mature, the dependency on PoW header verification becomes optional rather than immutable. The piton perspective observes that the current framework is becoming theater — performing security rituals that do not catch sophisticated attacks — which is diagnostically accurate and distinct from the coordination-vs-extraction debate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    selfish_mining_light_client_relevance,
    'Do selfish mining attacks on header-only light clients represent a practical threat or a theoretical edge case given realistic network parameters and adversary capabilities?',
    'Empirical attack simulation: run selfish mining against synthetic light client networks with realistic network delay, hash distribution, and eclipse attack prevalence. Measure success rates at 20%, 33%, and 50% hash power thresholds.',
    'If practical threat: extractiveness increases to 0.68+ (snare classification shifts for moderate agents). If theoretical: extractiveness remains ~0.58 (tangled_rope stable). Classification hinges on empirical adversary model, not theoretical bounds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selfish_mining_light_client_relevance, empirical, 'Practical threat level of selfish mining against light clients').

omega_variable(
    proof_system_maturity_timeline,
    'When will succinct proofs (SNARKs/STARKs) for PoW/PoS consensus achieve production-grade security and efficiency sufficient to replace PoW header verification in light clients?',
    'Track proof system development: circuit complexity, prover time, verifier time, and formal security audits. Identify the point at which client libraries integrate succinct verification as primary method. Historical precedent: ECC hardening took 15-20 years from theory to standard practice.',
    'If before 2035: scaffold sunset is real and high-confidence — extractiveness decays to 0.25+ over next decade. If after 2045: scaffold is aspirational, extractiveness remains stable or rises. Classification depends on architectural path-dependency.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proof_system_maturity_timeline, empirical, 'Timeline for succinct proof system maturity in light client applications').

omega_variable(
    device_capability_distribution_shift,
    'As device computational capacity grows and bandwidth expands globally, what fraction of ''light client'' users actually need lightweight verification versus simply preferring it for convenience?',
    'Cohort analysis: stratify light client users by device class (embedded, mobile, desktop) and network conditions. Measure what fraction could run full clients at acceptable UX cost. Project forward 10 years.',
    'If most ''light clients'' are convenience users with capacity for full validation: suppression metric is overstated, extractiveness drops to ~0.42 (rope becomes stable). If significant cohort is structurally trapped: suppression remains 0.65+, snare classification for powerless agents is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(device_capability_distribution_shift, empirical, 'Fraction of light client users with actual versus perceived resource constraints').

omega_variable(
    full_node_decentralization_sustainability,
    'Can full node operators maintain sufficiently high decentralization to prevent consensus takeover while relying on light client adoption to justify the resource investment?',
    'Network topology analysis: measure active full node count, geographic distribution, and Nakamoto coefficient. Run centralization attack scenarios assuming light clients migrate to proof systems or exit. Identify minimum full node count for security.',
    'If full nodes become too scarce without light client economic support: the constraint is symbiotic (genuine tangled rope coordination). If full nodes remain numerous from intrinsic incentives: light client security dependency is lower, extraction softens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(full_node_decentralization_sustainability, empirical, 'Sustainability of full node decentralization under changing light client dynamics').

omega_variable(
    header_validation_attack_surface_clarity,
    'Are the actual attack vectors against light client header validation well-understood and disclosed to users, or obscured by PoW ritualism?',
    'Conduct systematic documentation: enumerate all attack classes (long-range, timestamp, Sybil, eclipse, selfish mining variants). Map each to detection difficulty and success probability. Audit how many major light client implementations warn users about each class.',
    'If attacks are well-disclosed and mitigated: theater_ratio drops to ~0.35, piton classification becomes marginal. If attacks are obscured or underestimated: theater_ratio rises to 0.55+, piton classification strengthens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(header_validation_attack_surface_clarity, empirical, 'Clarity and disclosure of light client attack surface').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(proof_of_work_light_client_security, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(powlcs_tr_t0, proof_of_work_light_client_security, theater_ratio, 0, 0.42).
narrative_ontology:measurement(powlcs_tr_t3, proof_of_work_light_client_security, theater_ratio, 3, 0.46).
narrative_ontology:measurement(powlcs_tr_t6, proof_of_work_light_client_security, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(powlcs_be_t0, proof_of_work_light_client_security, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(powlcs_be_t3, proof_of_work_light_client_security, base_extractiveness, 3, 0.54).
narrative_ontology:measurement(powlcs_be_t6, proof_of_work_light_client_security, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(proof_of_work_light_client_security, information_standard).
narrative_ontology:boltzmann_floor_override(proof_of_work_light_client_security, 0.12).
narrative_ontology:affects_constraint(proof_of_work_light_client_security, proof_of_work_consensus_centralization).
narrative_ontology:affects_constraint(proof_of_work_light_client_security, blockchain_scalability_trilemma).
narrative_ontology:affects_constraint(proof_of_work_light_client_security, sybil_attack_economic_cost).

% DUAL FORMULATION NOTE:
% Light client security is downstream of the PoW consensus model but represents a distinct structural constraint. Upstream constraint (proof_of_work_consensus_centralization) addresses mining concentration; light client security addresses verification accessibility. These are linked: as mining concentrates, the trust assumptions underlying light client verification degrade.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(proof_of_work_light_client_security, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
