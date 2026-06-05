% ============================================================================
% CONSTRAINT STORY: ergo_storage_rent_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ergo_storage_rent_mechanism, []).

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
 *   constraint_id: ergo_storage_rent_mechanism
 *   human_readable: Ergo Storage Rent (Demurrage) Mechanism
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The Ergo Storage Rent (Demurrage) mechanism imposes a fee on dormant UTXO
 *   data stored on the blockchain, with the stated goal of preventing state
 *   bloat and incentivizing efficient resource usage. Introduced at mainnet
 *   launch (2019), storage rent represents a hybrid constraint: it is
 *   genuinely a coordination mechanism that aligns network participant
 *   incentives with computational efficiency, but it also functions as
 *   asymmetric extraction from passive token holders and legacy application
 *   developers. The constraint exhibits a classic tangled rope structure: a
 *   real coordination function (bounding state growth) coupled with coercive
 *   extraction (penalizing inactivity) that depends on continued suppression
 *   of alternatives. The core tension is that storage rent is both a
 *   theoretical necessity (ledgers must price scarce resources) and a
 *   contingent policy choice (the specific rate, exceptions, and enforcement
 *   pattern are protocol-specific, not universal).
 *
 * KEY AGENTS:
 *   - Dormant UTXO Holders: Primary victims (powerless/trapped) — bear erosion of holdings through mandatory rent deductions with no escape path
 *   - Legacy dApp Developers: Secondary victims (moderate/constrained) — face rising operational costs for on-chain state management; can migrate but at high switching cost
 *   - Network Security Validators: Primary beneficiaries (institutional/arbitrage) — benefit from state bloat prevention and alignment of mining incentives with network health
 *   - Ecosystem Transition Coalition: Organized agents (organized/constrained) — developers of L2 solutions and state optimization tools; see rent as temporary incentive for migration
 *   - Protocol Governance System: Institutional maintainer (institutional/arbitrage) — preserves rent mechanism through protocol inertia; governance system has arbitrage options (can change rent rate)
 *   - Analytical Observer: Universal perspective (analytical/analytical) — risks naturalizing contingent policy (storage must be priced) as immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ergo_storage_rent_mechanism, 0.38).
domain_priors:suppression_score(ergo_storage_rent_mechanism, 0.48).
domain_priors:theater_ratio(ergo_storage_rent_mechanism, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ergo_storage_rent_mechanism, extractiveness, 0.38).
narrative_ontology:constraint_metric(ergo_storage_rent_mechanism, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ergo_storage_rent_mechanism, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ergo_storage_rent_mechanism, tangled_rope).
narrative_ontology:human_readable(ergo_storage_rent_mechanism, "Ergo Storage Rent (Demurrage) Mechanism").
narrative_ontology:topic_domain(ergo_storage_rent_mechanism, "economic/technological").

domain_priors:requires_active_enforcement(ergo_storage_rent_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ergo_storage_rent_mechanism, network_security_validators).
narrative_ontology:constraint_beneficiary(ergo_storage_rent_mechanism, long_term_ecosystem_sustainability).
narrative_ontology:constraint_victim(ergo_storage_rent_mechanism, dormant_utxo_holders).
narrative_ontology:constraint_victim(ergo_storage_rent_mechanism, legacy_application_developers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DORMANT UTXO HOLDER (SNARE) — An agent who accumulated ERG tokens years ago and held them passively experiences erosion of their holdings through storage rent deductions. Exit options are constrained: abandoning the holdings forfeits them entirely; moving them requires activity (spending) that triggers the rent deduction anyway; hodling incurs continuous extraction. The agent experiences maximum effective extraction because the constraint is inescapable from their passive strategy.
constraint_indexing:constraint_classification(ergo_storage_rent_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LEGACY DAPP DEVELOPER (TANGLED ROPE) — Developers who built applications that store state on-chain (account histories, governance records, contract state) face rising operational costs as their application footprint ages. They benefit from the coordination mechanism: storage rent prevents state bloat and ensures network performance remains bounded. But they bear asymmetric extraction: maintaining their dApp requires periodic state compaction or rent-bearing storage expansion. Mobile exit exists (migrate to rollup or L2) but is costly. Extraction is real but coupled with genuine coordination benefit.
constraint_indexing:constraint_classification(ergo_storage_rent_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NETWORK SECURITY VALIDATORS (ROPE) — Validators benefit from storage rent through two coordination mechanisms: (1) it incentivizes miners to maintain the chain efficiently by burning rent into the mining fee pool, creating alignment between storage overhead and validator rewards; (2) it prevents state bloat, which would otherwise increase computational burden on validators. This is pure coordination: rent collection aligns validator incentives with network health. Validators have arbitrage exit: they can shift computational resources between blockchains. Storage rent appears to them as a coordination instrument, not extraction.
constraint_indexing:constraint_classification(ergo_storage_rent_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ECOSYSTEM TRANSITION COALITION (SCAFFOLD) — Layer 2 solution developers, application migration services, and storage optimization tooling vendors see storage rent as a temporary coordination problem with a sunset: as scaling solutions mature (rollups, sidechains, off-chain state), the on-chain state footprint will naturally decrease, reducing rent pressure. The constraint is temporary by design — it incentivizes migration to more efficient architectures. Sunset mechanism: as the ecosystem scales horizontally, on-chain state density drops, rent extraction force declines. High suppression initially, but declining over time.
constraint_indexing:constraint_classification(ergo_storage_rent_mechanism, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PROTOCOL GOVERNANCE SYSTEM (PITON) — Storage rent in Ergo was designed as an elegant economic coordination mechanism, but its primary function (preventing state bloat) is largely performative after 5 years. The network state footprint growth rate has remained modest through natural user behavior (accounts paying rent or migrating away), not through aggressive rent enforcement. The governance system maintains storage rent as a theoretical guard rail more than an active extraction mechanism. Theater ratio is relatively low (0.35) because the mechanism does function as designed, but much of its operational effect is inertial — it persists because it was enshrined in the protocol at launch, not because it's the primary solution to state bloat (L2 scaling is).
constraint_indexing:constraint_classification(ergo_storage_rent_mechanism, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / ECONOMIC NECESSITY (MOUNTAIN) — From a universal/civilizational perspective, storage rent is an immutable constraint of resource-bounded systems: any ledger with finite computational capacity must price storage access, or face tragedy-of-the-commons bloat. This is not unique to Ergo — every persistent computational system (SQL databases, filesystems, blockchains) must solve the same problem. Storage costs are a fact of physics and economics. However, this perspective risks false naturalization: Ergo's specific implementation (demurrage rate, minimum rent threshold, exception rules) is contingent, not natural. The false summit detector should flag this.
constraint_indexing:constraint_classification(ergo_storage_rent_mechanism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ergo_storage_rent_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ergo_storage_rent_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ergo_storage_rent_mechanism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(ergo_storage_rent_mechanism, TR),
    TR >= 0.70.

:- end_tests(ergo_storage_rent_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Storage rent does extract value from dormant holders, but the extraction is coupled with genuine coordination benefit (preventing state bloat that would harm all network users). The mechanism is not as severe as a pure Snare (ε ≤ 0.66) because it operates on a predictable schedule and users retain escape options (activity, migration to L2). The extractiveness value reflects that the coordination function is real but the asymmetry is substantial — passive users bear costs without proportional benefit. Suppression (0.48): Moderate. Users can suppress rent effects through periodic spending or state compaction, but these actions require effort and may incur transaction costs. L2 migration is theoretically available but practically constrained by liquidity and tool maturity. Suppression is not total (users have some options) but significant (default passive holding incurs rent). Theater ratio (0.35): Low-moderate. Storage rent functions largely as designed: it does reduce state bloat incentives and aligns validator interests. But theater is non-zero because the mechanism operates partially through psychological deterrent (the threat of erosion) rather than through active state cleanup — users rarely explicitly consolidate state in response to rent pressure; they more often simply accept incremental losses or migrate away entirely.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap lies between the dormant UTXO holder (Snare) and the validator (Rope). The same mechanism appears as pure extraction to the passive holder (they experience continuous deduction with no coordination benefit) and as pure coordination to the validator (they experience incentive alignment without extraction). This gap is central to the tangled rope classification: the constraint genuinely serves a coordination function, but that coordination function is asymmetrically distributed. Secondary gap: the legacy dApp developer (Tangled Rope) sees both the coordination benefit (lower state bloat prevents network slowdown affecting their applications) and the extraction cost (maintaining state becomes more expensive over time). The scaffold perspective (Ecosystem Transition Coalition) sees a temporary constraint with a natural sunset, but this sunset depends on the empirical success of L2 scaling — if L2 adoption stalls, the scaffold becomes a permanent Tangled Rope or Snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent is derived from their structural position relative to the rent extraction flow. Dormant UTXO holders (powerless/trapped) occupy d ≈ 0.95 — maximum target position, no escape paths, full exposure to extraction. Network validators (institutional/arbitrage) occupy d ≈ 0.05 — beneficiary position with arbitrage exit options, experiencing negative effective extraction (they benefit from the constraint). Legacy dApp developers (moderate/constrained) occupy d ≈ 0.65 — partial victim position (they bear costs but also benefit from state efficiency) with constrained exit (can migrate but at cost). The analytical observer (analytical/analytical) occupies d ≈ 0.72 — observer position with full visibility but no structural stake. Each d value feeds into f(d), which produces the agent's experienced extractiveness chi. The perspectival gap emerges because different agents have radically different d values despite occupying the same constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The tangled rope classification resolves potential mandatrophy (false conflation of coordination and extraction) by explicitly documenting both the coordination function (state bloat prevention, validator incentive alignment) and the asymmetric extraction (erosion of dormant holdings, rising dApp costs). The mechanism is neither pure coordination (Rope, which would require suppression ≤ 0.05) nor pure extraction (Snare, which would require ε ≥ 0.66). Instead, it is a hybrid where the coordination function is genuine but the extraction is asymmetrically distributed and coercively enforced. The mandatrophy is resolved by showing that both aspects are structural: removing the coordination function would cause state bloat (real cost), but removing the extraction would eliminate the incentive asymmetry (real benefit for dormant holders). The mechanism cannot be simplified to either pole without losing explanatory power. The false summit alert on Perspective 6 (Mountain/Economic Necessity) flags the naturalization risk: storage pricing IS a universal necessity, but the Ergo implementation is contingent — other blockchains solve the same problem differently (Cardano with UTxO density controls, Ethereum with gas fees rather than demurrage). The constraint's type is not inherent to ledger economics; it is a design choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_bloat_severity_threshold,
    'What on-chain state footprint growth rate constitutes an actual protocol crisis vs. manageable overhead?',
    'Empirical measurement of storage requirements vs. validator hardware capacity over 10-year horizon; identification of critical inflection points where validation becomes economically infeasible',
    'If actual growth < 5% annually: storage rent is unnecessary coercion, classify as pure Snare. If growth > 15% annually: rent is insufficient coordination, classify as failed Rope. Current evidence suggests ~3-5%, implying rent is over-provisioned.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_bloat_severity_threshold, empirical, 'Actual on-chain state bloat rate vs. protocol capacity').

omega_variable(
    rent_rate_vs_incentive_effectiveness,
    'Does the current storage rent rate (4 Ergs per byte per 4 years) effectively incentivize state cleanup, or do users simply accept it as a minor tax?',
    'Analysis of state compaction behavior; correlation between rent rate changes and user account consolidation patterns; survey of dormant account lifecycle',
    'If users actively respond to rent incentives: rent is functional coordination (Rope from user perspective). If users ignore rent below psychological threshold: rent is ineffective extraction (Snare/Piton from user perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rent_rate_vs_incentive_effectiveness, empirical, 'Whether storage rent rate achieves intended incentive effect').

omega_variable(
    l2_scaling_timeline_substitution,
    'Will L2 rollup solutions (or sidechains) become the primary scaling mechanism within 5 years, making on-chain storage rent functionally obsolete?',
    'Tracking adoption of Ergo sidechains and rollup systems; measurement of on-chain transaction volume reduction; survey of developer migration to L2 solutions',
    'If L2 adoption occurs: scaffold sunset is real, confirm Perspective 4. If L2 adoption stalls: on-chain storage remains primary, rent becomes permanent (classify as Piton or Tangled Rope depending on adoption speed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(l2_scaling_timeline_substitution, empirical, 'Whether L2 solutions will make on-chain storage rent obsolete').

omega_variable(
    exception_rule_scope_creep,
    'Do protocol-level exceptions to storage rent (e.g., special treatment for governance contracts, treasury UTXOs) constitute targeted coordination or scope creep that undermines the universal rule?',
    'Enumeration of all rent exceptions in the protocol; analysis of whether exceptions serve demonstrated coordination needs or represent captured exemptions; comparison with other UTXO chains',
    'If exceptions are minimal and justified: rent mechanism maintains integrity (Rope). If exceptions proliferate: rent becomes selective extraction (Tangled Rope or Snare depending on exception distribution).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exception_rule_scope_creep, conceptual, 'Whether protocol exceptions to storage rent are justified or represent scope creep').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ergo_storage_rent_mechanism, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(esr_tr_t0, ergo_storage_rent_mechanism, theater_ratio, 0, 0.25).
narrative_ontology:measurement(esr_tr_t3, ergo_storage_rent_mechanism, theater_ratio, 3, 0.3).
narrative_ontology:measurement(esr_tr_t6, ergo_storage_rent_mechanism, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(esr_be_t0, ergo_storage_rent_mechanism, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(esr_be_t3, ergo_storage_rent_mechanism, base_extractiveness, 3, 0.35).
narrative_ontology:measurement(esr_be_t6, ergo_storage_rent_mechanism, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ergo_storage_rent_mechanism, resource_allocation).
narrative_ontology:affects_constraint(ergo_storage_rent_mechanism, ergo_utxo_state_bloat_accumulation).
narrative_ontology:affects_constraint(ergo_storage_rent_mechanism, ergo_dapp_operational_cost_barrier).

% DUAL FORMULATION NOTE:
% Storage rent is downstream of the fundamental problem (bounded ledger capacity) but represents a distinct mechanism family separate from scaling solutions. The upstream constraint is capacity scarcity itself; storage rent is one policy response. L2 scaling solutions represent an alternative response. These constraints form a family because rent's extractiveness depends on the adoption rate of alternatives — if L2 scaling succeeds, rent becomes vestigial (Piton); if L2 scaling stalls, rent becomes permanent extraction (Tangled Rope or Snare).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
