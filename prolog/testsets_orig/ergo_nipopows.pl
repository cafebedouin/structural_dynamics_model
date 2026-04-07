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
 *   domain: technological/cryptographic
 *
 * SUMMARY:
 *   Non-Interactive Proofs of Proof-of-Work (NiPoPoWs) enable lightweight
 *   clients to verify blockchain state by downloading only kilobytes of proof
 *   data instead of gigabytes of full chain history. This creates a
 *   structural tension: the technology is coordination infrastructure (making
 *   PoW blockchains accessible to resource-constrained participants) but
 *   simultaneously extracts value from full node operators by enabling
 *   clients to gain security guarantees without bearing validation costs. The
 *   constraint exhibits tangled rope structure because NiPoPoWs serve a
 *   genuine coordination function (enabling broader participation) while
 *   simultaneously establishing an asymmetric extraction relationship (light
 *   clients benefit from validation work they don't perform, externalizing
 *   costs to full nodes). The theater_ratio (0.55) reflects that while
 *   NiPoPoWs are technically elegant, they perpetuate a performative security
 *   model: the 'lightweight' verification still depends on the existence of a
 *   network of expensive full nodes doing the real work. The constraint's
 *   extractiveness (0.38) is moderate because the asymmetry, while real, is
 *   partially justified by network effects—light clients increase ecosystem
 *   value and thus create secondary benefits for validators.
 *
 * KEY AGENTS:
 *   - Light Client Users: Primary beneficiary (institutional/arbitrage) — gain blockchain access with minimal bandwidth and hardware; can switch protocols freely
 *   - Full Node Operators: Primary victim (powerless/trapped) — bear full validation costs that enable light clients; cannot exit without degrading network security
 *   - Network Node Operators: Secondary victim (moderate/constrained) — face ongoing proof verification and chain monitoring costs; partial agency via lightweight client adoption
 *   - Ergo Foundation / Ecosystem Stewards: Organized implementer (organized/constrained) — driving NiPoPoW adoption; see constraint as transitional (scaffold) infrastructure
 *   - Proof-of-Work Consensus Model: Institutional actor (institutional/arbitrage) — perpetuates validation cost structure; NiPoPoWs patch rather than replace this model
 *   - Analytical Observer: Civilizational context (analytical/analytical) — observes both coordination function and extraction mechanism; sees constraint as tangled rope bridging accessibility and sustainability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ergo_nipopows, 0.38).
domain_priors:suppression_score(ergo_nipopows, 0.42).
domain_priors:theater_ratio(ergo_nipopows, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ergo_nipopows, extractiveness, 0.38).
narrative_ontology:constraint_metric(ergo_nipopows, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(ergo_nipopows, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ergo_nipopows, tangled_rope).
narrative_ontology:human_readable(ergo_nipopows, "Non-Interactive Proofs of Proof-of-Work (NiPoPoWs)").
narrative_ontology:topic_domain(ergo_nipopows, "technological/cryptographic").

domain_priors:requires_active_enforcement(ergo_nipopows).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ergo_nipopows, light_client_users).
narrative_ontology:constraint_beneficiary(ergo_nipopows, ergo_ecosystem_validators).
narrative_ontology:constraint_victim(ergo_nipopows, full_node_operators).
narrative_ontology:constraint_victim(ergo_nipopows, bandwidth_constrained_networks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FULL NODE OPERATOR (SNARE) — Trapped by network economics. Full nodes must validate entire chains to participate trustlessly. NiPoPoW extracts value by enabling light clients that depend on this validation work without compensating validators. No exit option: cannot both verify blockchain state and avoid the computational cost of full validation that their work subsidizes.
constraint_indexing:constraint_classification(ergo_nipopows, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NETWORK NODE (TANGLED ROPE) — Constrained by bandwidth costs and hardware requirements. Benefits from network utility (more light clients = more ecosystem activity) but bears costs of proof verification and chain monitoring. Partial agency: can run lightweight clients themselves, but full node ecosystem benefits would accrue to light clients primarily.
constraint_indexing:constraint_classification(ergo_nipopows, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: LIGHT CLIENT USER (ROPE) — Institutional beneficiary with arbitrage options. Can move between blockchains, protocols, or verification models. NiPoPoW provides coordination: enables participation in PoW ecosystems without bearing full validation costs. Network effect positive-sum: more participants = stronger security consensus.
constraint_indexing:constraint_classification(ergo_nipopows, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ERGO FOUNDATION / ECOSYSTEM STEWARDS (SCAFFOLD) — Organized agents implementing NiPoPoW as transitional infrastructure. The constraint has a sunset: as light clients proliferate and mobile verification becomes standard, the asymmetry between full node costs and light client benefits will stabilize into a normalized incentive structure. Suppression (mandatory proof validation) declines as protocols mature. Current theater reflects the innovation phase where NiPoPoW adoption is still being established.
constraint_indexing:constraint_classification(ergo_nipopows, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PROOF-OF-WORK ASSUMPTION (PITON) — From civilizational view, PoW's claim to trustless verification is increasingly performative. NiPoPoWs solve an artifact problem: the bloat created by requiring clients to validate entire PoW chains. But as alternative consensus (PoS, hierarchical, sharded) mechanisms mature, the constraint degrades to theatrical: NiPoPoW is a clever patch on an antiquated model. Theater ratio high because the 'solution' perpetuates the underlying PoW extraction (mining) rather than addressing it.
constraint_indexing:constraint_classification(ergo_nipopows, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From universal perspective, NiPoPoWs coordinate two conflicting objectives: making PoW accessible to light clients (coordination benefit) while preserving the security model that requires full nodes to do expensive validation work (extraction mechanism). The constraint is both enabling (new use cases) and extractive (externalizing validation costs).
constraint_indexing:constraint_classification(ergo_nipopows, tangled_rope,
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

test(piton_threshold) :-
    domain_priors:theater_ratio(ergo_nipopows, TR),
    TR >= 0.70.

:- end_tests(ergo_nipopows_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-high. Light clients derive security guarantees from full node validation work without directly compensating that work. The extraction is real—clients externalize costs—but justified partially by network effects. As light client adoption increases (from 0.22 at interval start to 0.38 at interval end), the extractiveness rises because the asymmetry compounds. Suppression (0.42): Moderate. Full nodes cannot refuse to validate proofs or exclude light clients—they must participate in the consensus mechanism. However, suppression is not total because validators can choose whether to run full nodes vs joining mining pools or light client networks. The constraint is suppressive but has exit points. Theater_ratio (0.55): Moderate-high and increasing. NiPoPoWs perform the function of 'lightweight verification' but perpetuate the underlying PoW theater—clients accept probabilistic security guarantees based on the assumption that full nodes are performing expensive work. As adoption spreads, the performative aspect (claiming lightweight security while depending on expensive infrastructure) becomes more apparent.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary (light client user) sees rope: 'NiPoPoWs solve a real coordination problem by enabling mobile verification.' The victim (full node operator) sees snare: 'I bear costs with no exit; clients free-ride on my validation.' The ecosystem steward sees scaffold: 'This is temporary infrastructure with declining theater as adoption stabilizes incentives.' The PoW model itself, from civilizational perspective, sees piton: 'NiPoPoWs are a patch on an outdated system; they perpetuate rather than resolve the underlying cost structure.' The analytical observer sees the full tangled rope: 'Both perspectives are structurally correct. Coordination and extraction are co-present. The constraint's stability depends on whether light client benefits to the ecosystem outweigh the private costs to full nodes.'
 *
 * DIRECTIONALITY LOGIC:
 *   Full node operators derive d ≈ 0.95 (victim + trapped exit): they bear validation costs for light clients with no ability to opt out without degrading network security. f(d) ≈ 1.42 gives them maximum experienced extraction. Light client users derive d ≈ 0.10 (beneficiary + arbitrage exit): they gain access without validation costs and can move to alternative protocols freely. f(d) ≈ -0.01 gives them negative/negligible extraction. Network nodes occupy a middle ground: d ≈ 0.55 (both + constrained exit), reflecting their mixed experience of benefit (ecosystem strength) and cost (proof verification). The ergo foundation and ecosystem stewards occupy institutional roles with arbitrage options (d ≈ 0.15), seeing NiPoPoWs as a coordination mechanism with declining suppression over time (scaffold logic). The analytical observer uses d ≈ 0.72 (observer status) to hold the entire structure in view.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVES THE MANDATROPHY: The constraint avoids false classification as 'pure coordination' (Rope) or 'pure extraction' (Snare) by recognizing that both functions are genuine. NiPoPoWs DO solve a coordination problem (enabling light client participation). They DO extract value (externalizing validation costs). The tangled_rope classification captures this duality: base extraction (0.38) is high enough to establish a real asymmetry; suppression (0.42) is significant but not total; beneficiaries and victims are both present and structurally necessary for the mechanism to function. The perspectives reveal that this is not a measurement ambiguity but a structural feature: different agents genuinely experience the constraint differently because they occupy different structural positions. The light client perspective (rope) and full node perspective (snare) are not competing descriptions of the same thing—they are accurate descriptions of different structural relationships to the same constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_margin_validity,
    'What security margin is preserved when light clients accept NiPoPoW proofs vs full validation? Does the ''succinct'' proof sacrifice practical security guarantees?',
    'Formal analysis of proof-of-work collision probability under NiPoPoW sampling; empirical testing of proof forgery difficulty; comparison to full node security margins under network attack conditions',
    'If margin is negligible (< 1% security loss): NiPoPoWs are pure coordination mechanism (Rope from more perspectives). If margin is significant (> 10% security loss): extraction mechanism (victims bear security risk) becomes primary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(security_margin_validity, empirical, 'Security margin preservation in NiPoPoW light client verification').

omega_variable(
    full_node_incentive_sustainability,
    'Can full node networks sustain if the primary use case (mobile verification) becomes feasible without full node infrastructure?',
    'Economic modeling of full node operator incentives; analysis of mining pool centralization if light client dominance reduces full node demand; historical comparison to other ''light'' protocol implementations (SPV, Ethereum light clients)',
    'If sustainable: constraint is coordination (ecosystem can support both). If unsustainable: NiPoPoWs create a tragedy of the commons where the infrastructure (full nodes) degrades under its own success.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(full_node_incentive_sustainability, empirical, 'Long-term viability of full node incentives under NiPoPoW light client adoption').

omega_variable(
    adoption_threshold_externality,
    'At what light client adoption percentage does the security of the full PoW network degrade? Is there a critical mass above which NiPoPoWs undermine their own security base?',
    'Network simulation; empirical analysis of mining distribution and orphan rates under various light client percentages; analysis of attack surface expansion when full node diversity decreases',
    'If threshold is high (> 80% light clients): NiPoPoWs can scale without undermining PoW security. If threshold is low (< 20% light clients): NiPoPoWs create a fundamental instability — success produces conditions for failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adoption_threshold_externality, empirical, 'Critical adoption threshold for NiPoPoW light client security externality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ergo_nipopows, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nipopow_tr_t0, ergo_nipopows, theater_ratio, 0, 0.38).
narrative_ontology:measurement(nipopow_tr_t3, ergo_nipopows, theater_ratio, 3, 0.48).
narrative_ontology:measurement(nipopow_tr_t6, ergo_nipopows, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(nipopow_be_t0, ergo_nipopows, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(nipopow_be_t3, ergo_nipopows, base_extractiveness, 3, 0.3).
narrative_ontology:measurement(nipopow_be_t6, ergo_nipopows, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ergo_nipopows, information_standard).
narrative_ontology:affects_constraint(ergo_nipopows, proof_of_work_light_client_security).
narrative_ontology:affects_constraint(ergo_nipopows, full_node_economic_sustainability).

% DUAL FORMULATION NOTE:
% NiPoPoWs can be analyzed as a pure coordination mechanism (light client infrastructure enabling broader participation) or as an extraction mechanism (externalizing validation costs to full nodes). These are not observables of the same constraint but structurally distinct claims. Upstream: proof-of-work security assumptions (whether PoW guarantees are actually preserved under light client dominance). Downstream: full node incentive structures (whether validators can sustain without explicit compensation from light clients).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
