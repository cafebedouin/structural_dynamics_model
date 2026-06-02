% ============================================================================
% CONSTRAINT STORY: ergo_nipopows
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ergo_nipopows, []).

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
 *   constraint_id: ergo_nipopows
 *   human_readable: Non-Interactive Proofs of Proof-of-Work (NiPoPoWs)
 *   domain: cryptographic/blockchain_infrastructure
 *
 * SUMMARY:
 *   Non-Interactive Proofs of Proof-of-Work (NiPoPoWs) are a cryptographic
 *   technology that enables lightweight blockchain clients to verify
 *   consensus state by downloading and validating only kilobytes of proof
 *   data rather than gigabytes of full transaction history. The constraint
 *   emerges from a fundamental structural asymmetry: the technology provides
 *   a genuine coordination benefit (making blockchain validation accessible
 *   to resource-constrained devices including mobile wallets, IoT devices,
 *   and users in low-bandwidth regions) while simultaneously creating an
 *   extraction mechanism where light clients receive security guarantees
 *   funded by full node operators who bear the full computational burden of
 *   validation without direct compensation. The constraint exhibits genuine
 *   tangled rope structure — there is a coordination function (enabling
 *   client-light blockchain verification) coupled with asymmetric extraction
 *   (full nodes subsidize light client security). The tension intensifies
 *   because the technology is not parasitic but genuinely valuable: it solves
 *   real problems of blockchain scalability and geographic accessibility. Yet
 *   the architecture distributes validation costs asymmetrically, placing
 *   full computational burden on operators while dispersing the security
 *   benefits to unlimited light clients. This creates a mechanism where the
 *   more successful NiPoPoWs become at enabling adoption, the more severe the
 *   extraction becomes for full node operators.
 *
 * KEY AGENTS:
 *   - Full Node Operators: Primary victims (powerless/trapped) — bear full computational validation costs; cannot exit without abandoning independent verification; mining rewards insufficient to offset NiPoPoW-driven cost increases as network scales
 *   - Lightweight Clients (mobile wallets, resource-constrained devices): Primary beneficiaries (moderate/constrained) — gain blockchain access previously locked behind resource barriers; exit costs are high but not absolute (could use centralized services)
 *   - Exchange and Wallet Providers: Secondary beneficiaries (powerful/mobile) — reduce infrastructure costs for light wallet offerings; can exit by operating alternative protocols; high negotiating power with full node operators
 *   - Protocol Development Community: Coordinators (institutional/arbitrage) — solve genuine scalability problem; experience NiPoPoWs as infrastructure rather than extraction
 *   - Incentive Alignment Researchers: Organized agents (organized/constrained) — designing proof reward mechanisms to transform asymmetric extraction into symmetric coordination; see sunset path
 *   - Legacy Full Node Ecosystem: Institutional inertia (institutional/arbitrage) — maintains resource-intensive validation partly through network effects, partly through performative claims about necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ergo_nipopows, 0.52).
domain_priors:suppression_score(ergo_nipopows, 0.48).
domain_priors:theater_ratio(ergo_nipopows, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ergo_nipopows, extractiveness, 0.52).
narrative_ontology:constraint_metric(ergo_nipopows, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ergo_nipopows, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ergo_nipopows, tangled_rope).
narrative_ontology:human_readable(ergo_nipopows, "Non-Interactive Proofs of Proof-of-Work (NiPoPoWs)").
narrative_ontology:topic_domain(ergo_nipopows, "cryptographic/blockchain_infrastructure").

domain_priors:requires_active_enforcement(ergo_nipopows).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ergo_nipopows, lightweight_clients).
narrative_ontology:constraint_beneficiary(ergo_nipopows, mobile_wallets).
narrative_ontology:constraint_beneficiary(ergo_nipopows, resource_constrained_devices).
narrative_ontology:constraint_victim(ergo_nipopows, full_node_operators).
narrative_ontology:constraint_victim(ergo_nipopows, network_bandwidth_provisioners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Full node operators cannot opt out of bearing validation costs while lightweight clients extract security guarantees without reciprocal burden. Exit options are severely limited: running a full node is the only way to validate independently; abandoning it means trusting centralized services. The extraction is sustained by the protocol's design assumptions — full nodes are incentivized to exist (mining rewards, fee collection) but NiPoPoWs decouple the security benefit from the cost burden.
constraint_indexing:constraint_classification(ergo_nipopows, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Lightweight clients gain genuine coordination benefit (accessible blockchain verification without gigabytes of storage) alongside asymmetric extraction — they receive security guarantees funded by full node operators' infrastructure without compensating them. Cost to exit is high (running full nodes is resource-prohibitive on mobile devices) but not absolute — they could use centralized light wallets or trusted validators, accepting different tradeoffs.
constraint_indexing:constraint_classification(ergo_nipopows, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% The protocol development community benefits from NiPoPoW adoption: it solves real coordination problems (mobile wallet adoption, geographic distribution of validation capacity) that increase network utility. They experience NiPoPoWs as coordination infrastructure rather than extraction — the technology enables legitimate scalability. Exit options are high (protocol design can be modified) and directionality is favorable (they are institutional actors who can negotiate outcomes).
constraint_indexing:constraint_classification(ergo_nipopows, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Commercial wallet and exchange operators benefit substantially from NiPoPoW adoption (reduced infrastructure costs for light wallet offerings) while imposing partial costs on the broader network. They have high exit options (can operate with or without NiPoPoWs, can use alternative protocols) and significant power (network adoption decisions). They coordinate genuine services (wallet access) alongside asymmetric extraction (cost-shifting to full nodes) — textbook tangled rope for powerful actors.
constraint_indexing:constraint_classification(ergo_nipopows, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% The older ecosystem of resource-intensive full node validation persists largely through institutional inertia and economic lock-in rather than functional necessity. NiPoPoWs reveal this inertia — the resource intensity of full nodes is not technically mandatory but sustained by network effects and historical path-dependency. The legacy ecosystem maintains itself through theater: claims about the necessity of running full nodes persist even as alternative architectures (light clients, sharded validation) become viable. Theater ratio reflects this performative element.
constraint_indexing:constraint_classification(ergo_nipopows, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Organized efforts to align incentives between light clients and full node operators (via proof rewards, delegation mechanisms, or stake-weighted proof compression) see NiPoPoWs as a temporary coordination failure with a sunset path. If incentive structures can be redesigned (validators earn rewards from proving work to light clients), the asymmetric extraction can be transformed into symmetric coordination. This perspective sees a clear exit path within institutional timescales — estimated 3-7 year sunset as incentive mechanisms mature.
constraint_indexing:constraint_classification(ergo_nipopows, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% From a civilizational perspective, NiPoPoWs are primarily coordination infrastructure: they solve a fundamental problem of blockchain scalability (making cryptographic verification accessible without centralized trust) that would otherwise concentrate validation capacity. The technology enables a coordination function that was previously locked behind resource barriers. The extraction dynamics are secondary — they are adjustment costs as the network transitions from full-node-intensive to client-light architectures.
constraint_indexing:constraint_classification(ergo_nipopows, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ergo_nipopows_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ergo_nipopows, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ergo_nipopows, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ergo_nipopows, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ergo_nipopows, TR),
    TR >= 0.70.

:- end_tests(ergo_nipopows_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, increasing over adoption window then stabilizing. At t=0 (pre-adoption), extractiveness is moderate (0.35) because NiPoPoW incentive problems are theoretical — few light clients exist to create asymmetry. As adoption increases (t=3 to t=6), extractiveness rises sharply (0.48→0.55) as the actual cost differential becomes measurable and light client populations grow. By t=9, extractiveness stabilizes around 0.52 because adoption saturates and protocol responses (proof reward mechanisms, incentive alignment) begin dampening the asymmetry. The metric reflects the actual cost-benefit distribution: light clients receive security guarantees worth the computational cost of validation (~$X in hardware and electricity per light client supported), but full node operators capture zero direct compensation — the extraction is the divergence between the value provided and the compensation received. Suppression (0.48): Moderate. Full node operators can theoretically exit (switch protocols, demand compensation, stop running nodes) but face genuine barriers: network effects (their node validates the established chain), economic lock-in (sunk hardware costs), and lack of coordination mechanisms to collectively demand compensation. Barriers are surmountable (exit is possible at a cost) rather than absolute, placing this in the constrained/suppressed range rather than trapped. Theater ratio (0.38): Low-moderate. NiPoPoWs are primarily technical infrastructure with low performative content — the proofs either compress validly or they do not. However, some theater exists around necessity narratives ('full nodes are essential' claims that mask the shift toward client-light architectures) and around claims that current incentive structures are permanent. The relatively low theater reflects that the technical function is genuine and measurable.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits extreme perspectival divergence. Full node operators perceive a Snare (trapped, cannot exit, subsidizing others) while lightweight clients perceive a Rope (coordination benefit without perceived extraction cost). Wallet providers perceive tangled_rope with favorable terms (they benefit and can exit). The protocol development community perceives pure Rope (solving a legitimate coordination problem). The legacy full node ecosystem perceives Piton (its own infrastructure is partially degraded by client-light adoption but persists through inertia). The organized incentive-alignment community perceives Scaffold (temporary asymmetry with a clear sunset path via incentive redesign). The analytical observer perceives Rope (civilizational coordination infrastructure). This perspectival range from Snare (full node operators) through Rope (multiple views) is diagnostic of genuine tangled-rope structure: there IS a coordination function (undeniable — NiPoPoWs solve real scalability problems) AND there IS asymmetric extraction (equally undeniable — full nodes subsidize light clients). The disagreement about whether this is 'just coordination costs' or 'extractive asymmetry' reflects the genuine ambiguity in the constraint's causal structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural relationship to extraction flow, exit capacity, and power atom. Full node operators: d ≈ 0.92 (victim + trapped exit → maximum experienced extractiveness). Lightweight clients: d ≈ 0.55 (beneficiary-with-caveats + constrained exit → moderate extraction by full nodes, meaning light clients experience negative extraction — they gain benefit). Wallet providers: d ≈ 0.48 (beneficiary + mobile exit → low to moderate extraction experience — they can negotiate). Protocol developers: d ≈ 0.25 (institutional beneficiary + arbitrage → they designed the benefit, exit costs are near zero). The scoping modifier σ(S) = 1.2 (global scale) slightly amplifies effective extractiveness because the asymmetry is harder to verify and coordinate against when distributed globally. Local NiPoPoW deployments would show lower χ; global adoption amplifies the extraction mechanism by making coordination among full node operators harder.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by exemplifying why the six types are not competing hypotheses but perspectival readings of genuine mixed-mechanism structures. The constraint is legitimately both tangled_rope (from the system design view) and snare (from the full node operator view) and rope (from the protocol development view). The mandatrophy is resolved by recognizing that NiPoPoWs are not mislabeled — they have BOTH a coordination function (legitimate, measurable, valuable) AND an extraction mechanism (legitimate, measurable, costly). The question is not 'which type is it really?' but 'from what structural position are we observing?' This exemplifies the framework's core claim: apparent classification ambiguity disappears when you specify the observation site (agent power, time horizon, exit options, scope). The constraint is simultaneously all six types because all six observation sites coexist in the blockchain network. Full node operators occupy the Snare position; protocol developers occupy the Rope position; incentive researchers occupy the Scaffold position; legacy infrastructure operators occupy the Piton position. The mandatrophy resolves not by choosing one type but by mapping the perspectival geometry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    full_node_viability_without_extraction,
    'Can full node operators remain economically viable after widespread NiPoPoW adoption without protocol-level compensation for proof generation?',
    'Empirical measurement of full node operator count and operational costs post-adoption; correlation with mining reward structures and transaction fee distribution. Model full node maintenance costs against solo-mining and pool-mining profitability.',
    'If viable without compensation: extraction is actually coordination (Rope from more perspectives, protocol development becomes primary beneficiary). If unviable: extraction is structural and sustained, requiring protocol-level incentive redesign to resolve asymmetry.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(full_node_viability_without_extraction, empirical, 'Full node operator economic viability post-adoption').

omega_variable(
    proof_compression_fraud_vector,
    'Do NiPoPoW compression techniques introduce new attack surfaces or false-acceptance risks that require additional defensive infrastructure (full nodes specifically for verification)?',
    'Formal verification of proof compression cryptography; empirical testing of light client vulnerability to reorganization attacks and Sybil-resistant proof validation. Compare security assumptions of light clients using NiPoPoWs to those using SPV.',
    'If new vulnerabilities exist: full node infrastructure becomes necessary as defensive network layer (Rope — legitimate coordination cost). If compression is cryptographically sound: full node extraction becomes unambiguous (Snare from light client perspective, Piton from network design perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proof_compression_fraud_vector, empirical, 'Whether NiPoPoW compression introduces new attack surfaces').

omega_variable(
    geographic_decentralization_effect,
    'Does NiPoPoW adoption actually increase geographic decentralization of validation capacity, or does it concentrate validation power in high-resource regions while dispersing trust-verification?',
    'Measurement of validator geographic distribution pre- and post-adoption. Correlation with NiPoPoW light client adoption rates by region. Analysis of whether resource barriers to running full nodes have shifted to hardware/connectivity barriers favoring wealthy regions.',
    'If decentralization increases: constraint is pure coordination (Rope becomes primary classification). If concentration increases: constraint masks centralization while claiming distribution (Tangled Rope with false-summit components).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_decentralization_effect, empirical, 'Geographic decentralization effects of NiPoPoW adoption').

omega_variable(
    proof_generation_incentive_design_space,
    'What incentive mechanisms can credibly compensate full node operators for proof generation without recreating the resource barriers that NiPoPoWs were designed to bypass?',
    'Game-theoretic analysis of proof reward mechanisms; empirical testing of proposed incentive structures (proof-of-work delegation, validator proof markets, state-root verification contracts). Historical comparison to fee markets in other protocols.',
    'If viable mechanisms exist: sunset path is concrete and timeline is estimable (Scaffold classification is warranted). If mechanisms are economically infeasible: asymmetric extraction is permanent (Snare or Piton classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proof_generation_incentive_design_space, conceptual, 'Viable incentive mechanisms for proof compensation').

omega_variable(
    sybil_resistance_dependency,
    'Are NiPoPoW security guarantees fundamentally dependent on some threshold of full node participation, even if light clients never interact with those nodes directly?',
    'Formal analysis of Sybil resistance in proof compression; empirical measurement of security degradation at varying full node population levels. Testing: what minimum full node count is required for light client security?',
    'If threshold dependency exists: full nodes are network commons (Rope from analytical view — necessary coordination cost). If not: full node existence is decoupled from light client security (Snare — pure extraction benefit captured by light clients).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sybil_resistance_dependency, empirical, 'Sybil resistance dependency on full node participation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ergo_nipopows, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nipopow_tr_t0, ergo_nipopows, theater_ratio, 0, 0.32).
narrative_ontology:measurement(nipopow_tr_t6, ergo_nipopows, theater_ratio, 6, 0.38).

% Extraction over time
narrative_ontology:measurement(nipopow_be_t0, ergo_nipopows, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nipopow_be_t3, ergo_nipopows, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(nipopow_be_t6, ergo_nipopows, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(nipopow_be_t9, ergo_nipopows, base_extractiveness, 9, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(nipopow_su_t0, ergo_nipopows, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(nipopow_su_t3, ergo_nipopows, suppression_requirement, 3, 0.46).
narrative_ontology:measurement(nipopow_su_t6, ergo_nipopows, suppression_requirement, 6, 0.5).
narrative_ontology:measurement(nipopow_su_t9, ergo_nipopows, suppression_requirement, 9, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ergo_nipopows, global_infrastructure).
narrative_ontology:affects_constraint(ergo_nipopows, blockchain_client_centralization).
narrative_ontology:affects_constraint(ergo_nipopows, mining_pool_consolidation).
narrative_ontology:affects_constraint(ergo_nipopows, validator_incentive_asymmetry).

% DUAL FORMULATION NOTE:
% NiPoPoWs are one instantiation of a broader constraint family around client-light verification. The constraint decomposes into two structurally distinct stories: (1) NiPoPoWs as cryptographic mechanism (compression techniques, proof validation logic) which is largely coordination infrastructure; (2) NiPoPoWs as incentive structure (who bears validation costs, who receives security benefit) which exhibits extraction. The extractiveness value (0.52) reflects the second story primarily — the technical mechanisms are sound (low ε on compression), but the incentive asymmetry is substantial (high ε on cost-shifting). These could be authored as separate constraint stories; the unified story treats the incentive problem as primary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
