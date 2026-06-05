% ============================================================================
% CONSTRAINT STORY: ergo_storage_rent
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ergo_storage_rent, []).

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
 *   constraint_id: ergo_storage_rent
 *   human_readable: Ergo Storage Rent (UTXO Demurrage)
 *   domain: technological_economic
 *
 * SUMMARY:
 *   Storage rent in Ergo is a demurrage mechanism that charges fees to UTXOs
 *   (unspent coins) that have not moved in four years. The constraint creates
 *   a structural tension between the legitimate problem of blockchain state
 *   accumulation and the punitive extraction from dormant or abandoned coin
 *   holders. This mechanism exhibits multiple classification types depending
 *   on the observer's structural position: active network participants see it
 *   as necessary coordination to maintain network health; dormant coin
 *   holders see it as extraction with no exit; the protocol developers see it
 *   as a hybrid mechanism that solves a real problem while imposing costs on
 *   a powerless constituency. The constraint's theater ratio (0.45, moderate)
 *   reflects that while state management is genuinely necessary, some of the
 *   urgency narratives and necessity claims are inflated relative to actual
 *   state bloat impacts. Ergo Storage Rent serves as a diagnostic case for
 *   how economic mechanisms can simultaneously solve real coordination
 *   problems and extract from vulnerable constituencies without clear consent
 *   mechanisms.
 *
 * KEY AGENTS:
 *   - Dormant UTXO Holders: Primary victims (powerless/trapped) — coins locked due to inactivity face automatic extraction; no alternative pathway
 *   - Abandoned Legacy Wallets: Secondary victims (powerless/trapped) — genuinely lost coins with inaccessible keys bear continuous demurrage
 *   - Inactive Retail Holders: Mixed agents (moderate/constrained) — intend to return but face extraction penalty and coordination dilemma
 *   - Active Network Node Operators: Primary beneficiaries (institutional/arbitrage) — benefit from reduced state bloat and lower storage costs
 *   - Protocol Developer Coalition: Institutional actors (organized/constrained) — maintain the mechanism as necessary but imperfect solution
 *   - Long-Term Ecosystem Sustainability Narrative: Abstract beneficiary — frames storage rent as temporary measure toward state compression maturity
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a design choice as economic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ergo_storage_rent, 0.38).
domain_priors:suppression_score(ergo_storage_rent, 0.52).
domain_priors:theater_ratio(ergo_storage_rent, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ergo_storage_rent, extractiveness, 0.38).
narrative_ontology:constraint_metric(ergo_storage_rent, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(ergo_storage_rent, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ergo_storage_rent, tangled_rope).
narrative_ontology:human_readable(ergo_storage_rent, "Ergo Storage Rent (UTXO Demurrage)").
narrative_ontology:topic_domain(ergo_storage_rent, "technological_economic").

domain_priors:requires_active_enforcement(ergo_storage_rent).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ergo_storage_rent, active_network_participants).
narrative_ontology:constraint_beneficiary(ergo_storage_rent, long_term_ecosystem_sustainability).
narrative_ontology:constraint_victim(ergo_storage_rent, dormant_utxo_holders).
narrative_ontology:constraint_victim(ergo_storage_rent, legacy_wallet_abandonment).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DORMANT UTXO HOLDER (SNARE) — Holder of coins that have not moved in 4+ years faces automatic demurrage extraction with no alternative pathway. Cannot negotiate, transfer without penalty, or exit without loss. Bears full cost of storage rent. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.63.
constraint_indexing:constraint_classification(ergo_storage_rent, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ABANDONED LEGACY WALLET (SNARE) — Wallets abandoned due to forgotten keys, lost hardware, or inactive users bear continuous demurrage extraction. No mechanism to opt out, recover, or reclaim value. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.65.
constraint_indexing:constraint_classification(ergo_storage_rent, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: INACTIVE RETAIL HOLDER (TANGLED ROPE) — Holder who intends to return (but at an uncertain future date) receives some benefit from network security and ecosystem stability paid for by storage rent. But faces extraction penalty for storage. Exit is constrained: moving coins restarts the timer, creating a coordination problem. d≈0.65, f(d)≈0.95, σ=0.9 → χ≈0.33.
constraint_indexing:constraint_classification(ergo_storage_rent, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ACTIVE NETWORK NODE OPERATOR (ROPE) — Operator of a full node benefits from storage rent mechanism as it incentivizes UTXO churn, reducing blockchain state size and validator storage burdens. Experiences constraint as pure coordination: demurrage enforces shared responsibility for state bloat. d≈0.08, f(d)≈-0.09, σ=1.2 → χ≈-0.04.
constraint_indexing:constraint_classification(ergo_storage_rent, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PROTOCOL DEVELOPER COALITION (TANGLED ROPE) — Developers and ecosystem coordinators see storage rent as a hybrid mechanism: it solves the real coordination problem of UTXO accumulation (state bloat) AND extracts from dormant holders to fund state rent rewards. Coalition experiences constraint as necessary but imperfectly enforced. Exit is constrained by need to maintain network consensus. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.31.
constraint_indexing:constraint_classification(ergo_storage_rent, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: LONG-TERM ECOSYSTEM SUSTAINABILITY (SCAFFOLD) — Storage rent frames itself as a temporary mechanism to solve the UTXO bloat problem during the accumulation phase. As protocol maturity increases and state compression techniques improve, the necessity of demurrage as a coercive measure declines. Scaffold perspective sees sunset: eventually, voluntary incentives or technical solutions may replace punitive storage rent. d≈0.35, f(d)≈0.28, σ=1.2 → χ≈0.13.
constraint_indexing:constraint_classification(ergo_storage_rent, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: BLOCKCHAIN STATE SUSTAINABILITY (PITON) — Storage rent is partially performative: while it does reduce UTXO bloat, much of the justification ('we must solve state bloat or the network dies') has evolved into institutional theater. Actual state size concerns have been managed through pruning and compression. The demurrage mechanism persists through protocol conservatism and path dependency, not because it is the only or most effective solution. theater_ratio=0.45 (moderate theater) reflects that functional reduction of state size is real but overlaid with narrative inflation about existential necessity. d≈0.12, f(d)≈0.02, σ=1.2 → χ≈0.01.
constraint_indexing:constraint_classification(ergo_storage_rent, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / ECONOMIC FUNDAMENTAL VIEW (MOUNTAIN) — From a civilizational perspective, some form of rent extraction from persistent storage is inherent to any shared-state ledger system: if value-preservation is free, rational actors will accumulate indefinitely and destroy the commons. This perspective sees demurrage as an immutable economic necessity, not a contingent institutional design choice. However, the structural data (ε=0.38, suppression=0.52, theater=0.45) contradicts a mountain classification — alternative mechanisms (state fees funded by other sources, voluntary incentives, technical compression) exist and are practiced on other chains. This reveals the false summit: what appears as economic law is actually a design choice.
constraint_indexing:constraint_classification(ergo_storage_rent, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ergo_storage_rent_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ergo_storage_rent, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ergo_storage_rent, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(ergo_storage_rent, TR),
    TR >= 0.70.

:- end_tests(ergo_storage_rent_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Storage rent does extract from dormant holders, but the extraction rate is not extreme (typically 0.1-0.5 ERG per year per UTXO depending on denomination) and is justified by real state management costs. The value reflects that extraction is real but bounded and partially defensible on coordination grounds. Suppression (0.52): Moderate-high. Dormant holders have significant barriers to exit: moving coins restarts the 4-year timer, creating a dilemma; abandoned wallets have zero exit options; the mechanism is embedded in protocol consensus. Suppression is not total because active holders can choose to move coins or accept the fee. Theater ratio (0.45): Moderate. Storage rent has genuine functional impact on state bloat, but the narratives about existential necessity have inflated over time. Actual state compression and pruning techniques have reduced the urgency compared to early protocol justifications. Theater has grown as alternative solutions have become viable.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a clear perspectival divide between active and dormant constituencies. Active node operators see pure coordination (Rope) — they benefit from lighter state and lower storage costs. Dormant holders see pure extraction (Snare) — they face penalties with no alternatives or compensation. The protocol developers see a hybrid mechanism (Tangled Rope) — solving a real problem but with significant distributional consequences. The ecosystem sustainability narrative sees a temporary measure with a sunset (Scaffold). The institutional defense of storage rent as inevitable sees a false summit (Mountain), when alternatives exist on other chains. The perspectival gap reveals that storage rent is not a neutral economic law but a design choice that concentrates costs on a powerless constituency (dormant holders, who cannot organize or advocate) and distributes benefits to an organized constituency (network operators, who benefit from lower state bloat).
 *
 * DIRECTIONALITY LOGIC:
 *   Dormant UTXO holders: Victim + trapped → d≈0.92, f(d)≈1.38. Nearly maximum extraction — zero exit options. Abandoned legacy wallets: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction — completely immobilized. Inactive retail holders: Victim + constrained → d≈0.65, f(d)≈0.95. Moderate extraction with constrained exit (moving coins restarts timer). Active node operators: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.09. Net beneficiary through state reduction benefits. Protocol developers: Mixed (benefits from coordination, costs from distributional complexity) + constrained → d≈0.50, f(d)≈0.65. Moderate effective extraction from their constraint perspective. Ecosystem sustainability: Beneficiary with sunset → d≈0.35, f(d)≈0.28. Low effective extraction because the mechanism is framed as temporary.
 *
 * MANDATROPHY ANALYSIS:
 *   Storage rent resolves mandatrophy by revealing the indexical nature of the classification. The constraint is NOT 'which type is it really?' but 'what is the observer's structural relationship to the constraint?' An active node operator genuinely experiences it as coordination (Rope) because they benefit from state reduction. A dormant holder genuinely experiences it as extraction (Snare) because they have no alternatives. The protocol developers genuinely experience it as a hybrid (Tangled Rope) because they must balance coordination benefits against distributional costs. The false summit (Mountain) emerges when the analytical observer naturalizes a contingent design choice ('UTXO bloat is inevitable') — comparative analysis across blockchains (Ethereum, Bitcoin, other UTXO systems) shows that alternative mechanisms are available. The mandatrophy is resolved by recognizing that the constraint IS legitimately all these types simultaneously, from different structural positions. The normative question ('is storage rent justified?') is distinct from the structural question ('what type of constraint is it?'). The answer to the normative question depends on whether the distributional consequences are acceptable given the coordination benefits — a question that requires explicit value judgment, not empirical determination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_bloat_necessity_threshold,
    'At what point of UTXO accumulation does state bloat become a genuine bottleneck vs. a projected future concern managed by other means?',
    'Comparative analysis of blockchain state sizes across chains with and without demurrage; correlation between demurrage rates and actual network performance degradation; measurement of state compression effectiveness',
    'If bloat is already acute: demurrage is essential coordination mechanism (Rope dominates). If bloat is projected but manageable: demurrage is extractive institutional choice (Snare dominates).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_bloat_necessity_threshold, empirical, 'Whether state bloat is a present bottleneck or manageable future concern').

omega_variable(
    dormancy_detection_accuracy,
    'How many UTXOs classified as ''dormant'' (4+ years inactive) are actually held by living agents with recovery intentions vs. genuinely abandoned coins?',
    'On-chain transaction pattern analysis; wallet recovery requests and successful owner re-activation; entropy analysis of dormant key reuse and address clustering',
    'If >80% genuinely abandoned: demurrage is fair fee on dead accounts (Rope). If <50% abandoned: demurrage is extraction from inactive-but-living holders (Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dormancy_detection_accuracy, empirical, 'Proportion of dormant UTXOs that are genuinely abandoned vs. recoverable').

omega_variable(
    alternative_state_management_sufficiency,
    'Do technical solutions (state compression, archival layers, sharding, pruning strategies) provide sufficient UTXO accumulation management without demurrage extraction?',
    'Comparative protocol analysis of Ethereum (no demurrage, uses compression), Bitcoin (no demurrage, uses pruning), other UTXO chains with and without storage rent; performance benchmarks and state size projections',
    'If alternatives prove sufficient: demurrage is rent extraction masked as necessity (Snare). If alternatives have unacceptable tradeoffs: demurrage is necessary coordination (Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_state_management_sufficiency, empirical, 'Whether technical alternatives can manage UTXO bloat without demurrage').

omega_variable(
    extraction_reallocation_transparency,
    'Are extracted storage rents genuinely returned to network stakeholders through state rent rewards, or do they accumulate in protocol reserves or mining pools?',
    'On-chain accounting of storage rent collection, state rent reward distribution, and reserve accumulation; analysis of actual vs. promised fund flows',
    'If fully redistributed: demurrage frames as coordination mechanism (Rope/Tangled Rope). If redirected to protocol developers or node operators: demurrage is hidden extraction (Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_reallocation_transparency, empirical, 'Whether storage rent extraction is redistributed or accumulated by protocol controllers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ergo_storage_rent, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ergsr_tr_t0, ergo_storage_rent, theater_ratio, 0, 0.3).
narrative_ontology:measurement(ergsr_tr_t2, ergo_storage_rent, theater_ratio, 2, 0.38).
narrative_ontology:measurement(ergsr_tr_t4, ergo_storage_rent, theater_ratio, 4, 0.45).

% Extraction over time
narrative_ontology:measurement(ergsr_be_t0, ergo_storage_rent, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(ergsr_be_t2, ergo_storage_rent, base_extractiveness, 2, 0.27).
narrative_ontology:measurement(ergsr_be_t4, ergo_storage_rent, base_extractiveness, 4, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ergo_storage_rent, resource_allocation).
narrative_ontology:affects_constraint(ergo_storage_rent, utxo_state_accumulation).
narrative_ontology:affects_constraint(ergo_storage_rent, blockchain_scalability_pruning).
narrative_ontology:affects_constraint(ergo_storage_rent, dormant_account_recovery).

% DUAL FORMULATION NOTE:
% Storage rent represents a mechanism for solving UTXO bloat (state management coordination problem) via extraction from dormant holders. This decomposes into two structurally distinct claims: (1) State bloat is a real problem requiring solution (ε≈0.08, Mountain/coordination); (2) Demurrage extraction is the chosen solution mechanism (ε≈0.38, Tangled Rope). These are linked: the existence of claim 1 justifies claim 2, but claim 2's necessity depends on claim 1 being unsolvable by other means. Alternative chains use different solutions (compression, pruning, state fees from other sources), suggesting claim 2 is contingent design choice rather than inevitable response to claim 1.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
