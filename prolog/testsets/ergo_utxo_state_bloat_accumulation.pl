% ============================================================================
% CONSTRAINT STORY: ergo_utxo_state_bloat_accumulation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ergo_utxo_state_bloat_accumulation, []).

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
 *   constraint_id: ergo_utxo_state_bloat_accumulation
 *   human_readable: Ergo UTXO State Bloat Accumulation Constraint
 *   domain: blockchain/systems_scalability
 *
 * SUMMARY:
 *   The Ergo UTXO state bloat accumulation constraint is a structural
 *   consequence of a deliberate protocol design choice: the guarantee that
 *   all historical outputs remain valid and spendable indefinitely, with no
 *   pruning mechanism, state rent, or expiration logic. As transaction volume
 *   accumulates, the UTXO set grows monotonically, imposing increasing
 *   storage and verification costs on full node operators. This constraint
 *   exhibits characteristics of both coordination (the accessibility
 *   guarantee enables light clients and auditing) and extraction (the costs
 *   are concentrated on full node operators with no corresponding benefit).
 *   The measurements show accumulating pressure over nine years of
 *   observation: base extractiveness rises from 0.22 to 0.58 as the UTXO set
 *   grows and hardware requirements escalate. Suppression increases from 0.48
 *   to 0.65 as the economic and technical barriers to running a full node
 *   strengthen. Theater ratio remains low (0.32–0.42) because the bloat
 *   mechanism is technically legible and directly measurable — there is no
 *   performative aspect to storage accumulation. The constraint is a
 *   diagnostic exemplar for how a genuine coordination mechanism
 *   (accessibility guarantee) can generate asymmetric extraction (bloat cost)
 *   when the cost of maintaining the guarantee grows with scale and
 *   concentrates on a subset of actors.
 *
 * KEY AGENTS:
 *   - Full Node Operators: Primary victim (powerless/trapped) — bear monotonically increasing storage and verification costs with no exit mechanism. Hardware requirements escalate as UTXO set grows.
 *   - Light Client and SPV Wallet Operators: Primary beneficiary (institutional/arbitrage) — leverage the accessibility guarantee to operate lightweight services without maintaining full state. Can arbitrage to other chains if costs rise.
 *   - Exchange Operators: Secondary beneficiary (institutional/arbitrage) — benefit from guaranteed output accessibility for transaction verification and wallet auditing without full node burden.
 *   - Ergo Protocol Developers and Governance: Organized actor (organized/constrained) — designed the no-pruning guarantee deliberately; now manage mitigation R&D and community pressure for sunset mechanism.
 *   - State Rent and Pruning Advocates: Organized actor (organized/constrained) — research community and protocol upgraders proposing technical solutions (state rent, soft-pruning, archive tiers) with sunset logic.
 *   - Network Decentralization: Victim (powerless/trapped) — abstract collective: as full node costs rise, decentralization risk increases. Fewer operators can sustain full nodes, increasing centralization pressure.
 *   - Analytical Observer: Observing the structure (analytical/analytical) — risks naturalizing the design choice as an immutable property of UTXO systems rather than a policy decision.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ergo_utxo_state_bloat_accumulation, 0.58).
domain_priors:suppression_score(ergo_utxo_state_bloat_accumulation, 0.65).
domain_priors:theater_ratio(ergo_utxo_state_bloat_accumulation, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ergo_utxo_state_bloat_accumulation, extractiveness, 0.58).
narrative_ontology:constraint_metric(ergo_utxo_state_bloat_accumulation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ergo_utxo_state_bloat_accumulation, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ergo_utxo_state_bloat_accumulation, tangled_rope).
narrative_ontology:human_readable(ergo_utxo_state_bloat_accumulation, "Ergo UTXO State Bloat Accumulation Constraint").
narrative_ontology:topic_domain(ergo_utxo_state_bloat_accumulation, "blockchain/systems_scalability").

domain_priors:requires_active_enforcement(ergo_utxo_state_bloat_accumulation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ergo_utxo_state_bloat_accumulation, light_clients).
narrative_ontology:constraint_beneficiary(ergo_utxo_state_bloat_accumulation, exchange_operators).
narrative_ontology:constraint_beneficiary(ergo_utxo_state_bloat_accumulation, spv_wallets).
narrative_ontology:constraint_victim(ergo_utxo_state_bloat_accumulation, full_node_operators).
narrative_ontology:constraint_victim(ergo_utxo_state_bloat_accumulation, network_decentralization).
narrative_ontology:constraint_victim(ergo_utxo_state_bloat_accumulation, blockchain_state_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FULL NODE OPERATOR (SNARE) — Trapped by the protocol guarantee that all historical outputs remain valid. Cannot prune, cannot optimize away the storage burden. As UTXO set grows, storage requirements and verification costs increase monotonically with no mechanism to reverse course. No exit: maintaining a full node becomes increasingly costly; abandoning it degrades network security. Maximum extraction — the operator bears full cost of historical accumulation with no benefit mechanism.
constraint_indexing:constraint_classification(ergo_utxo_state_bloat_accumulation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LIGHT CLIENT AND SPV WALLET OPERATORS (ROPE) — Benefit from the protocol's guarantee that all outputs remain accessible. Can verify transaction history and wallet balances without maintaining full state. Arbitrage options: operate lightweight infrastructure or shift to more scalable chains. Experience the constraint as pure coordination: the UTXO accessibility guarantee enables their service model. Zero extraction — full beneficiary of the structure without bearing bloat cost.
constraint_indexing:constraint_classification(ergo_utxo_state_bloat_accumulation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ERGO PROTOCOL DEVELOPERS AND GOVERNANCE (TANGLED ROPE) — Organized actors (protocol core team, EIP contributors) experience the constraint as both coordination mechanism and extraction. The no-pruning guarantee was a deliberate design choice to maintain state accessibility — genuine coordination function. However, the constraint imposes escalating implementation burden on the team to manage storage optimization and node software improvements. Constrained: cannot easily modify the guarantee without contentious hard fork; must absorb increasing R&D costs to mitigate bloat. Mixed extraction and coordination.
constraint_indexing:constraint_classification(ergo_utxo_state_bloat_accumulation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: STATE RENT AND PRUNING ADVOCATES (SCAFFOLD) — Organized research community and protocol upgraders see the bloat as a solvable coordination failure with a sunset path. Proposals like state rent (charging for storage) or soft-pruning (incentivized archival) represent temporary scaffolding: the constraint would be actively managed rather than passively accumulated. This perspective has low theater (direct technical solution) and constrained exit (adoption requires community consensus). Sunset logic: if adopted, state rent or pruning mechanism would convert the bloat from passive accumulation to active management, reducing the snare classification for full nodes.
constraint_indexing:constraint_classification(ergo_utxo_state_bloat_accumulation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: HISTORICAL ACCESSIBILITY DOGMA (PITON) — The protocol's guarantee that all outputs remain valid indefinitely is maintained largely through institutional commitment to the design principle rather than active functional necessity. Most transactions after ~3 years are economically dead (dust outputs, abandoned addresses). The accessibility guarantee has real utility for auditing and history verification, but much of the maintained state serves no active function. Theater ratio reflects the performative aspects of the guarantee: claiming absolute historical immutability while practical node operators employ various caching and compression strategies that approximate pruning. Piton: the guarantee persists through commitment to the principle, not because perfect accessibility is technically essential at scale.
constraint_indexing:constraint_classification(ergo_utxo_state_bloat_accumulation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal perspective, state bloat appears as an immutable property of any UTXO-based system: if outputs are never deleted and transaction volume grows, the state must grow. This is logically necessary given the protocol's design choice. However, the classification is a false summit: the 'necessity' is contingent on the design choice to forbid pruning. The constraint is a realized policy choice, not a law of nature.
constraint_indexing:constraint_classification(ergo_utxo_state_bloat_accumulation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ergo_utxo_state_bloat_accumulation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ergo_utxo_state_bloat_accumulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ergo_utxo_state_bloat_accumulation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ergo_utxo_state_bloat_accumulation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ergo_utxo_state_bloat_accumulation, TR),
    TR >= 0.70.

:- end_tests(ergo_utxo_state_bloat_accumulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint exhibits genuine extraction: full node operators bear costs (storage, CPU, synchronization time) that grow monotonically with transaction volume, while beneficiaries (light clients, exchanges) pay diminishing per-transaction costs. The extraction is not maximal because the protocol offers genuine benefits to many users; the bloat cost is concentrated rather than universal. The measurement trajectory (0.22 → 0.58 over 9 years) reflects that extractiveness was initially low when the UTXO set was manageable but has risen as transaction volume accumulated. Suppression (0.65): High. Multiple layers of suppression maintain the bloat: (1) The protocol guarantee makes pruning impossible without contentious fork; (2) Economic incentives favor light clients over full node operators, discouraging full node participation; (3) Network effects make exiting Ergo for an alternative chain costly for operators already committed; (4) The distributed nature of node operation prevents collective bargaining — individual operators cannot negotiate terms with the protocol. Suppression is technically surmountable (state rent mechanism could be adopted) but politically difficult (requires community consensus for protocol change). Theater ratio (0.42): Moderate-low. The bloat mechanism is technically transparent — UTXO set size, growth rate, and hardware requirements are measurable and legible. There is no performative opacity around the constraint itself. However, some theater exists in the framing: the protocol presents the no-pruning guarantee as a security feature (immutable history, auditability) when much of the bloat is economically dead state. The theater masks the contingency of the design choice.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence driven by directionality. The full node operator (powerless/trapped) perceives maximum extraction and classifies as Snare — they see only burden. The light client operator (institutional/arbitrage) perceives pure coordination and classifies as Rope — they see only benefit. The protocol developers (organized/constrained) perceive a mixed structure: they created the guarantee for legitimate reasons (accessibility, auditability) but now manage escalating mitigation costs. The state rent advocates (organized/constrained) perceive a solvable problem with a sunset mechanism — the constraint is Scaffold-like if a technical solution is adopted. The institutional commitment to the accessibility guarantee appears Piton-like from the civilizational view: the principle persists through commitment, not active necessity. The analytical observer risks Mountain classification (state bloat is inherent to UTXO systems) but the base properties reveal this as false summit: the bloat is a policy choice, not a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation operates through three channels: (1) Beneficiary/victim declarations: Light clients and exchanges are beneficiaries (low d, negative/low χ); full node operators and network decentralization are victims (high d, high χ). (2) Exit options: Full node operators are trapped — they cannot prune, cannot migrate to alternative chains without losing their Ergo participation, cannot negotiate terms. Trapped exit yields high d (≈0.95 for full node operators). Light clients have arbitrage options — they can switch to alternative chains, use alternative wallet technologies, or shift service models. Arbitrage exit yields low d (≈0.15 for light clients). (3) Power level: Institutional actors (organized developers, exchanges) have higher power and thus somewhat lower experienced χ despite being beneficiaries; they have recourse to governance structures. Powerless full node operators experience maximum χ. The engine derives d automatically from these structural parameters, yielding directionality profiles that explain why the same constraint appears as Snare from the full node perspective and Rope from the light client perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves through recognizing that the constraint is structurally a Tangled Rope — it has BOTH genuine coordination function (the accessibility guarantee enables light clients, auditing, and history verification) AND asymmetric extraction (the costs concentrate on full node operators). The snare classification from the powerless operator's view and the rope classification from the beneficiary's view are both perspectival truths, not contradictions. The constraint resolves to Tangled Rope at the organized developer level (they designed both the coordination and the extraction) and Scaffold at the sunset level (state rent or pruning mechanisms would convert passive bloat to managed cost). The analytical observer's temptation to see Mountain (state bloat is natural to blockchains) is a false summit — the same logical argument could apply to any UTXO system, yet different systems make different design choices. The contingency is in the design, not in the law of nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dust_output_reactivation_risk,
    'What percentage of economically dead outputs (dust, abandoned addresses, unspent change) could be safely pruned without compromising protocol guarantees for economically active transactions?',
    'Empirical analysis of UTXO set composition: age distribution, value distribution, reactivation probability after dormancy periods. Statistical modeling of revival risk.',
    'If high pruning safety (>90% of old outputs expendable): state rent design is viable and constraint classification shifts toward Tangled Rope with sunset potential. If low safety (<50% confidently expendable): bloat is more genuinely immutable and snare classification persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dust_output_reactivation_risk, empirical, 'Fraction of UTXO set safely prunable without breaking protocol guarantees').

omega_variable(
    state_rent_adoption_feasibility,
    'Can state rent mechanisms be introduced via soft fork (backward compatible, opt-in) or do they require contentious hard fork that risks chain split?',
    'Technical analysis of UTXO model compatibility with rent logic. Simulation of adoption scenarios under partial node support. Historical precedent analysis from other UTXO chains (Bitcoin, Cardano).',
    'If soft fork viable: scaffold sunset is realistic, protocol has evolutionary pathway. If hard fork required: adoption barrier is political consensus, not technical feasibility — sunset timelines extend or stall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_rent_adoption_feasibility, empirical, 'Whether state rent can be introduced without contentious hard fork').

omega_variable(
    full_node_operator_threshold,
    'At what UTXO set size do consumer-grade hardware (16GB RAM, 2TB SSD) become insufficient for reliable full node operation, given current node optimization trends?',
    'Technical benchmarking: node software memory/disk profiling across historical UTXO set sizes. Extrapolation to projected growth rates (transaction volume trends). Hardware cost curves (CPU, RAM, storage) vs. network participation incentives.',
    'If threshold is <10 years away at current growth: snare classification is temporally acute, network decentralization risk becomes acute. If threshold >20 years: constraint is slow-moving, more time for scaffold solutions to mature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(full_node_operator_threshold, empirical, 'Time until consumer hardware becomes insufficient for full nodes').

omega_variable(
    alternative_model_sufficiency,
    'Do account-based models (Ethereum, Cardano Plutus) or layered commitment schemes (sidechains, L2s) actually reduce state bloat, or do they merely displace it to different layers?',
    'Comparative analysis of state growth curves across model types. Investigation of whether L2/sidechain approaches have identical bloat trajectories, just segregated from L1.',
    'If alternative models genuinely reduce bloat: the constraint is model-specific and Ergo''s choice is sub-optimal (moves classification toward snare as deliberate cost). If bloat is universal across models: constraint is closer to a natural law of blockchain systems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_model_sufficiency, empirical, 'Whether non-UTXO models reduce state bloat or displace it').

omega_variable(
    utxo_accessibility_vs_immutability_decoupling,
    'Can state accessibility for historical verification be decoupled from the requirement that all outputs remain *spendable*? Could archive nodes satisfy audit requirements while full nodes operate under soft-pruning?',
    'Technical analysis of UTXO query patterns (auditors vs. active users). Investigation of two-tier node architectures: full nodes with soft-pruned state + designated archive nodes with complete state.',
    'If decoupling is viable: the snare for full nodes can be weakened by making the bloat optional rather than mandatory. Full nodes could opt for soft-pruning while archive tier handles accessibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(utxo_accessibility_vs_immutability_decoupling, empirical, 'Whether state accessibility can be decoupled from full-node spendability guarantee').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ergo_utxo_state_bloat_accumulation, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ergo_utxo_tr_t0, ergo_utxo_state_bloat_accumulation, theater_ratio, 0, 0.32).
narrative_ontology:measurement(ergo_utxo_tr_t3, ergo_utxo_state_bloat_accumulation, theater_ratio, 3, 0.36).
narrative_ontology:measurement(ergo_utxo_tr_t6, ergo_utxo_state_bloat_accumulation, theater_ratio, 6, 0.4).
narrative_ontology:measurement(ergo_utxo_tr_t9, ergo_utxo_state_bloat_accumulation, theater_ratio, 9, 0.42).

% Extraction over time
narrative_ontology:measurement(ergo_utxo_be_t0, ergo_utxo_state_bloat_accumulation, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(ergo_utxo_be_t3, ergo_utxo_state_bloat_accumulation, base_extractiveness, 3, 0.35).
narrative_ontology:measurement(ergo_utxo_be_t6, ergo_utxo_state_bloat_accumulation, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(ergo_utxo_be_t9, ergo_utxo_state_bloat_accumulation, base_extractiveness, 9, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ergo_utxo_su_t0, ergo_utxo_state_bloat_accumulation, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(ergo_utxo_su_t3, ergo_utxo_state_bloat_accumulation, suppression_requirement, 3, 0.56).
narrative_ontology:measurement(ergo_utxo_su_t6, ergo_utxo_state_bloat_accumulation, suppression_requirement, 6, 0.62).
narrative_ontology:measurement(ergo_utxo_su_t9, ergo_utxo_state_bloat_accumulation, suppression_requirement, 9, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ergo_utxo_state_bloat_accumulation, resource_allocation).
narrative_ontology:affects_constraint(ergo_utxo_state_bloat_accumulation, blockchain_full_node_hardware_decentralization).
narrative_ontology:affects_constraint(ergo_utxo_state_bloat_accumulation, utxo_transaction_fee_design).
narrative_ontology:affects_constraint(ergo_utxo_state_bloat_accumulation, blockchain_state_commitment_mechanisms).

% DUAL FORMULATION NOTE:
% The UTXO state bloat accumulation is downstream of Ergo's decision to forbid pruning (a design principle constraint). The upstream constraint embodies the commitment to absolute historical accessibility; this story models the resource cost consequence of that commitment. Separate stories address alternative design choices (soft-pruning, state rent, account-based models) which have different ε values and different perspectival classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ergo_utxo_state_bloat_accumulation, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
