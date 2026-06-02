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
 *   human_readable: Blockchain Scalability Trilemma: Decentralization-Security-Throughput Trade-off
 *   domain: distributed_systems/cryptoeconomics
 *
 * SUMMARY:
 *   The blockchain scalability trilemma represents a constraint that
 *   straddles the boundary between natural law and institutional extraction.
 *   The classical formulation — that distributed ledgers cannot
 *   simultaneously maximize decentralization (many validators), security
 *   (Byzantine fault tolerance), and scalability (transaction throughput) —
 *   appears as mathematical necessity from the analytical perspective. Yet
 *   the same constraint functions as an institutional benefit to validator
 *   operators who maintain high fee revenues through artificial throughput
 *   scarcity. The constraint has evolved over six years from a theoretical
 *   recognition (early 2018, Bitcoin/Ethereum discussions) through
 *   institutional entrenchment (fee markets rewarding validators for
 *   throughput limits) to current contestation (layer-2 solutions,
 *   alternative consensus designs offering potential escape routes). This
 *   makes the trilemma a canonical example of Tangled Rope: it coordinates
 *   validator incentives and maintains blockchain security (genuine
 *   coordination function) while extracting from users through throughput
 *   scarcity and fee inflation (asymmetric extraction). The rising theater
 *   ratio (0.35 → 0.55) reflects increasing performative discourse around
 *   'inevitable tradeoffs' that obscure design choice. The constraint's
 *   false-summit candidacy — whether it is mathematical law or naturalized
 *   institutional design — drives the analytical perspective's mountain
 *   classification.
 *
 * KEY AGENTS:
 *   - Throughput-Constrained Users: Primary victims (powerless/trapped) — cannot transact at acceptable cost; bear full extraction with zero governance voice.
 *   - Layer-1 Ecosystem Participants: Secondary victims (powerless/trapped) — dApps, small protocols, and developers locked in by network effects and liquidity; trapped within congested ecosystem.
 *   - Validator Operators & Protocol Maintainers: Primary beneficiaries (institutional/arbitrage) — benefit from throughput constraint through fee volatility and capital efficiency; can switch chains if constraints loosen elsewhere.
 *   - Large Infrastructure Providers & Exchanges: Secondary beneficiaries (powerful/mobile) — both benefit (from fee revenue, validator staking) and constrained (by throughput limits); high agency across multiple chains.
 *   - Layer-2 Solution Developers: Mixed beneficiary-victim (organized/constrained) — constrained by technical dependency on layer-1 but benefit from existence of scalability gap that justifies their solutions.
 *   - Analytical Observer: Sees constraint as mathematical necessity (analytical/analytical) — risks naturalizing design choice as inevitability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(blockchain_scalability_trilemma, 0.52).
domain_priors:suppression_score(blockchain_scalability_trilemma, 0.48).
domain_priors:theater_ratio(blockchain_scalability_trilemma, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(blockchain_scalability_trilemma, extractiveness, 0.52).
narrative_ontology:constraint_metric(blockchain_scalability_trilemma, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(blockchain_scalability_trilemma, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(blockchain_scalability_trilemma, tangled_rope).
narrative_ontology:human_readable(blockchain_scalability_trilemma, "Blockchain Scalability Trilemma: Decentralization-Security-Throughput Trade-off").
narrative_ontology:topic_domain(blockchain_scalability_trilemma, "distributed_systems/cryptoeconomics").

domain_priors:requires_active_enforcement(blockchain_scalability_trilemma).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(blockchain_scalability_trilemma, validator_operators).
narrative_ontology:constraint_beneficiary(blockchain_scalability_trilemma, security_maximalists).
narrative_ontology:constraint_victim(blockchain_scalability_trilemma, throughput_constrained_users).
narrative_ontology:constraint_victim(blockchain_scalability_trilemma, scalability_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THROUGHPUT-CONSTRAINED USER (SNARE) — Users unable to transact due to network congestion and high fees. Cannot exit the chain without losing access to economic participants locked in. Bears full cost of scalability sacrifice with no voice in protocol design. Maximum extraction experienced.
constraint_indexing:constraint_classification(blockchain_scalability_trilemma, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LAYER-1 ECOSYSTEM PARTICIPANTS (SNARE) — Small developers, dApps, and ecosystem projects locked into a congested chain. High switching costs due to liquidity concentration and network effects. Trapped by ecosystem gravity; no meaningful exit option. Experience extraction through reduced throughput and competition-driven fee inflation.
constraint_indexing:constraint_classification(blockchain_scalability_trilemma, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: VALIDATOR OPERATORS (ROPE) — Benefit from the trilemma's constraint on throughput: lower network load means lower infrastructure costs while maintaining high validator rewards. Can arbitrage between competing layer-1 chains. See the constraint as coordination mechanism for sustainable validator economics — the trilemma preserves their revenue model.
constraint_indexing:constraint_classification(blockchain_scalability_trilemma, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LARGE INFRASTRUCTURE PROVIDERS (TANGLED ROPE) — Exchanges, custodians, and node operators both benefit (from fee volatility and validator staking revenue) and are constrained (by throughput limits on order settlement). High agency and mobile — can diversify across multiple chains. Experience moderate extraction mediated by coordination function around shared infrastructure.
constraint_indexing:constraint_classification(blockchain_scalability_trilemma, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LAYER-2 DEVELOPERS (TANGLED ROPE) — Experience extraction through forced reliance on layer-1 capacity (they cannot exceed it without building their own blockchain). Also benefit from the constraint: if layer-1 were unlimited, layer-2 solutions would lack economic rationale. Constrained by technical and economic dependency; can exit to alternative chains but at significant development cost.
constraint_indexing:constraint_classification(blockchain_scalability_trilemma, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the trilemma appears as a mathematical necessity: Byzantine fault tolerance with n validators requires O(n) communication complexity; decentralization demands many validators; security demands high communication overhead; throughput is the residual. This perspective sees the constraint as immutable physics of distributed consensus, not as institutional design choice.
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
    constraint_indexing:constraint_classification(blockchain_scalability_trilemma, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(blockchain_scalability_trilemma, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(blockchain_scalability_trilemma_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint does produce real extraction: users pay high fees due to throughput scarcity, and validators capture this fee revenue. However, the extraction is not maximal (0.72+) because layer-2 solutions provide partial escapes, and users have exit options (alternative chains, off-chain solutions). The value reflects mixed coordination (validator incentive alignment, security maintenance) and extraction (fee scarcity). The rising trajectory (0.35 → 0.52) reflects fee market maturation and increasing extraction as the network effect deepens. Suppression (0.48): Moderate. Users cannot easily increase throughput unilaterally — protocol changes require validator consensus, creating a governance barrier. But suppression is not total: users can migrate to layer-2 solutions or alternative chains. Validators have strong institutional incentive to maintain throughput constraints, but the suppression is exercised through protocol governance (which is visible and contestable) rather than pure coercion. Theater ratio (0.55): Moderate. The trilemma is partly genuine mathematical constraint and partly institutional narrative. The discourse around 'inevitable tradeoffs' performs the constraint, naturalizing what are design choices. Layer-2 advocates argue the trilemma can be overcome; validators argue it is fundamental. The theater has increased over time as the narrative has hardened and become more performative in validator communications and academic literature.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a stark perspectival gap between the validator-beneficiary view (Rope: healthy incentive coordination) and the powerless-user view (Snare: pure extraction). The analytical observer risks collapsing into the mountain view — naturalizing the trilemma as immutable physics — when the structural data (identifiable beneficiaries, fee markets, governance choices) suggests false summit. The layer-2 developer perspective (Tangled Rope) is generationally interesting: they are structurally locked in by the trilemma's existence (their solutions require the gap they fill) but also benefit from it. If the trilemma were overcome, their economic rationale would evaporate. This creates a subtle extractive mechanism: the existence of layer-2 solutions preserves the constraint by offering partial relief, reducing pressure for layer-1 scaling and validator alignment shifts. The large infrastructure provider (Tangled Rope, powerful/mobile) sees genuine coordination — they benefit from diverse chains and competing solutions — but also faces throughput constraints. Their high agency prevents this from being a Snare; they can exit or hedge.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from each agent's structural position and exit capacity. Powerless users with no exit (trapped) experience maximum d ≈ 0.95 → f(d) ≈ 1.42, amplifying experienced extraction. Institutional validators with arbitrage options experience d ≈ 0.05 → f(d) ≈ -0.12, producing negative effective extraction (they benefit). Powerful infrastructure providers with mobile options experience d ≈ 0.48 → f(d) ≈ 0.60, producing moderate effective extraction despite their power level (they have genuine choice). Organized layer-2 developers with constrained options experience d ≈ 0.40 → f(d) ≈ 0.40, producing moderate experienced extraction. The scope modifier σ(S) = 1.2 for global scope amplifies chi for all agents — the trilemma's global coordination effects and its global extraction effects are both increased by the breadth of the network.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing the constraint's dual nature: it is simultaneously a coordination mechanism (keeping validator incentives aligned, maintaining security consensus) and an extraction mechanism (fee scarcity, throughput rationing). The Tangled Rope classification captures both functions. The false-summit question — whether the constraint is mathematical law or institutional design — is the irreducible uncertainty. If the trilemma is law, it should classify as mountain from the analytical perspective. But the presence of identifiable beneficiaries, the rising extraction trajectory, and the visible governance choices around throughput limits trigger the false-summit detector. The engine's signature system will flag this constraint as a false summit candidate: the mountain classification coexists with clear institutional beneficiaries, suggesting naturalization. Resolution depends on omega variables: if validators are actively enforcing the constraint through governance choices (omega-2), the mountain reclassifies to snare/tangled_rope. If the trilemma is purely technical consequence, the mountain holds but the beneficiary extraction requires explanation (omega-4). The temporal measurements show acceleration — extractiveness and theater both rising — which supports the institutional-enforcement hypothesis (T17 trigger: mountain_extraction_accumulation). This is diagnostic: genuine natural laws do not accumulate extraction over time; institutional constraints become more entrenched and extractive as they age.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    distributed_consensus_lower_bound,
    'Is the scalability trilemma a fundamental limit of Byzantine fault-tolerant consensus, or an artifact of specific protocol choices?',
    'Formal complexity analysis of alternative consensus models (DAG-based, asynchronous, probabilistic finality); comparison of theoretical lower bounds across protocol families; identification of hidden assumptions in trilemma proofs.',
    'If fundamental: mountain classification is warranted. If artifact: constraint reclassifies to tangled_rope or snare — the three properties are tradeable, not immutable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_consensus_lower_bound, empirical, 'Whether the trilemma reflects fundamental mathematical limits or protocol-specific design constraints').

omega_variable(
    validator_alignment_enforcement,
    'Do validators actively maintain the trilemma through protocol governance and fee structures, or does it persist purely as a technical consequence?',
    'Historical analysis of validator voting on throughput-increase proposals (Ethereum''s block size debates, Bitcoin''s scaling discussions); correlation between validator revenue and throughput constraints; comparison of validator behavior across chains with different trilemma pressures.',
    'If validators actively enforce: extraction component is structural (snare/tangled_rope). If purely technical: constraint approaches mountain but false-summit detection flags beneficiary extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(validator_alignment_enforcement, empirical, 'Whether validators actively enforce or passively benefit from the trilemma constraint').

omega_variable(
    alternative_consensus_viability,
    'Do layer-2 solutions and alternative consensus designs genuinely overcome the trilemma, or do they relocate rather than eliminate the constraint?',
    'Analysis of layer-2 security model dependencies; measurement of total system throughput (layer-1 + layer-2 combined); evaluation of decentralization degradation in layer-2 systems relative to layer-1 baseline.',
    'If overcome: scaffold perspective validated — trilemma is temporary, sunset clause applies. If relocated: constraint persists as hierarchical dependency — layer-2 users still subject to layer-1 limits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_consensus_viability, empirical, 'Whether layer-2 solutions overcome the trilemma or relocate it to a different architectural layer').

omega_variable(
    natural_law_vs_institutional_beneficiary,
    'Is the trilemma a mathematical law that happens to have institutional beneficiaries, or an institutional arrangement naturalized as mathematical law?',
    'Comparison of protocol design choices across chains with different validator incentive structures; analysis of how throughput constraints evolved alongside validator revenue models; identification of design choices that increase trilemma pressure vs reduce it.',
    'If institutional beneficiary: false-summit signature fires; constraint reclassifies to snare/tangled_rope from analytical perspective. If genuine law: mountain holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_institutional_beneficiary, conceptual, 'Whether the trilemma is a mathematical necessity or an institutionalized design choice').

omega_variable(
    fee_market_extraction_mechanism,
    'Does throughput constraint benefit validators through fee volatility, or do alternative economic models (fixed block rewards, transaction tax) achieve validator compensation without scaling pressure?',
    'Comparison of validator revenue across chains with different fee market structures; analysis of economic incentives in fixed-reward vs dynamic-fee models; measurement of validator behavior under fee scarcity vs abundance.',
    'If fee volatility is essential: extraction mechanism is structural and self-reinforcing. If alternative models work: validators have choice to support scaling without revenue loss.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fee_market_extraction_mechanism, empirical, 'Whether fee-market extraction is economically necessary or a choice that locks in the constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(blockchain_scalability_trilemma, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bst_tr_t0, blockchain_scalability_trilemma, theater_ratio, 0, 0.35).
narrative_ontology:measurement(bst_tr_t3, blockchain_scalability_trilemma, theater_ratio, 3, 0.48).
narrative_ontology:measurement(bst_tr_t6, blockchain_scalability_trilemma, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(bst_be_t0, blockchain_scalability_trilemma, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bst_be_t3, blockchain_scalability_trilemma, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(bst_be_t6, blockchain_scalability_trilemma, base_extractiveness, 6, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(bst_su_t0, blockchain_scalability_trilemma, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(bst_su_t3, blockchain_scalability_trilemma, suppression_requirement, 3, 0.45).
narrative_ontology:measurement(bst_su_t6, blockchain_scalability_trilemma, suppression_requirement, 6, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(blockchain_scalability_trilemma, resource_allocation).
narrative_ontology:affects_constraint(blockchain_scalability_trilemma, validator_incentive_alignment).
narrative_ontology:affects_constraint(blockchain_scalability_trilemma, layer_2_ecosystem_dependency).
narrative_ontology:affects_constraint(blockchain_scalability_trilemma, cryptocurrency_adoption_barriers).

% DUAL FORMULATION NOTE:
% The scalability trilemma decomposes into two structurally distinct constraints: (1) the mathematical constraint of Byzantine consensus (ε ≈ 0.12, Mountain), and (2) the institutional constraint of fee-market extraction through throughput rationing (ε ≈ 0.52, Tangled Rope). The mathematical constraint is downstream of distributed consensus theory; the institutional constraint is downstream of validator incentive structures. This story models the composite phenomenon; separate stories could model each component independently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(blockchain_scalability_trilemma, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
