% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__pragmatic_synthesis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_consensus_kernel__pragmatic_synthesis, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: bitcoin_consensus_kernel__pragmatic_synthesis
 *   human_readable: Bitcoin Consensus Kernel: Pragmatic Synthesis Reading
 *   domain: cryptoeconomics/monetary_systems/distributed_consensus
 *
 * SUMMARY:
 *   The Bitcoin consensus kernel describes the community's foundational
 *   commitment to monetary rules that are immutable and mathematically
 *   determined. However, this kernel is contested. The pragmatic synthesis
 *   reading proposes that base-layer immutability (the consensus rules
 *   governing transaction validation and coin supply) is inviolable, while
 *   upper layers (L2 scaling solutions, sidechain protocols, second-order
 *   consensus mechanisms) permit innovation without requiring base-layer rule
 *   changes. This reading attempts to satisfy both the maximalist purists
 *   (who demand immutability) and utility advocates (who demand scalability).
 *   The structural cost of this synthesis is borne by ideological coherence:
 *   the system now holds two potentially contradictory commitments
 *   simultaneously. The pragmatic synthesis is a scaffold because it has a
 *   genuine sunset clause — as L2 solutions mature and prove sufficient for
 *   transactional scaling, the tension between immutability and flexibility
 *   decreases, and the need to maintain the pragmatic compromise diminishes.
 *   The extractiveness (0.32) reflects moderate tension: genuine coordination
 *   benefits (the layered model does distribute governance burden), balanced
 *   against the cost of maintaining coherence across two incompatible first
 *   principles.
 *
 * KEY AGENTS:
 *   - Base Layer Purists: Primary victim (powerless/trapped, identity_locked) — ideologically committed to immutable monetary rules; cannot exit without abandoning founding principle. Experience pragmatic synthesis as degradation.
 *   - Layer Two Developers: Primary beneficiary (moderate/constrained) — constrained by base-layer immutability guarantees but benefit from flexibility to innovate on L2 without L1 consensus.
 *   - Consensus Bridge Maintainers: Primary beneficiary (institutional/arbitrage) — core developers and protocol maintainers who benefit from distributed governance burden; can arbitrage between base stability and upper-layer flexibility.
 *   - Pragmatic Coalition: Organized actors (organized/constrained) — L2 protocol designers, exchange operators, institutional adopters; see pragmatic synthesis as temporary form with sunset.
 *   - Ideological Coherence: Primary victim (powerless/trapped) — abstract commitment to consistent first principles; bears cost of holding both immutability and flexibility simultaneously.
 *   - Academic Consensus Narratives: Institutional maintainer (institutional/arbitrage) — perpetuates pragmatic synthesis framing through academic literature; benefits from institutional stability of the compromise.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__pragmatic_synthesis, 0.32).
domain_priors:suppression_score(bitcoin_consensus_kernel__pragmatic_synthesis, 0.38).
domain_priors:theater_ratio(bitcoin_consensus_kernel__pragmatic_synthesis, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, extractiveness, 0.32).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__pragmatic_synthesis, scaffold).
narrative_ontology:human_readable(bitcoin_consensus_kernel__pragmatic_synthesis, "Bitcoin Consensus Kernel: Pragmatic Synthesis Reading").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__pragmatic_synthesis, "cryptoeconomics/monetary_systems/distributed_consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__pragmatic_synthesis).
narrative_ontology:has_sunset_clause(bitcoin_consensus_kernel__pragmatic_synthesis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__pragmatic_synthesis, '6c0a44b1-f6f2-4fba-9573-94fb112cbf35').
narrative_ontology:cs_kernel_codification('6c0a44b1-f6f2-4fba-9573-94fb112cbf35', formalized).
narrative_ontology:cs_authority_grounding('6c0a44b1-f6f2-4fba-9573-94fb112cbf35', distributed).
narrative_ontology:cs_reading_relation('6c0a44b1-f6f2-4fba-9573-94fb112cbf35', bitcoin_consensus_kernel__maximalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('6c0a44b1-f6f2-4fba-9573-94fb112cbf35', bitcoin_consensus_kernel__utility_reading, coexists_with).
narrative_ontology:cs_axiom('6c0a44b1-f6f2-4fba-9573-94fb112cbf35', foundational, base_layer_immutability_necessary).
narrative_ontology:cs_axiom_status(base_layer_immutability_necessary, holdable).
narrative_ontology:cs_axiom_grounding('6c0a44b1-f6f2-4fba-9573-94fb112cbf35', base_layer_immutability_necessary, instrumental).
narrative_ontology:cs_axiom('6c0a44b1-f6f2-4fba-9573-94fb112cbf35', foundational, layer_two_scalability_sufficient).
narrative_ontology:cs_axiom_status(layer_two_scalability_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('6c0a44b1-f6f2-4fba-9573-94fb112cbf35', layer_two_scalability_sufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('6c0a44b1-f6f2-4fba-9573-94fb112cbf35', layered_architecture_consensus).
narrative_ontology:cs_drift_state('6c0a44b1-f6f2-4fba-9573-94fb112cbf35', contemporary_l2_maturation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6c0a44b1-f6f2-4fba-9573-94fb112cbf35', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__pragmatic_synthesis, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__pragmatic_synthesis, layer_two_developers).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__pragmatic_synthesis, consensus_bridge_maintainers).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__pragmatic_synthesis, ideological_coherence).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__pragmatic_synthesis, base_layer_purists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BASE LAYER PURIST (SNARE) — Trapped by ideological commitment to immutable monetary rules. Cannot exit without abandoning the core principle that originally motivated participation. Experiences the pragmatic synthesis as degradation — a Trojan horse for the flexibility that will eventually corrupt the base layer. Maximum extraction through cognitive capture: the purist sees themselves as defending principle while in fact defending a brittle institutional form.
constraint_indexing:constraint_classification(bitcoin_consensus_kernel__pragmatic_synthesis, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LAYER TWO DEVELOPER (TANGLED ROPE) — Constrained by need for base-layer immutability guarantees (cannot build on a base that changes). Also benefits from the layered model's flexibility: can innovate on L2 without needing consensus for every feature. Mixed experience: genuine coordination (segregated concerns) alongside extraction (bounded by L1 immutability, constrained in what L2 can achieve without L1 changes).
constraint_indexing:constraint_classification(bitcoin_consensus_kernel__pragmatic_synthesis, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: CONSENSUS BRIDGE MAINTAINER (ROPE) — Institutional actor (core developers, maintainers) benefit from the pragmatic synthesis: the layered model distributes governance burden and reduces the cost of consensus. Can arbitrage between immutable base and flexible upper layers. Experiences the constraint as pure coordination: the segregation of concerns is genuinely valuable for maintaining both stability and innovation capacity.
constraint_indexing:constraint_classification(bitcoin_consensus_kernel__pragmatic_synthesis, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PRAGMATIC COALITION (SCAFFOLD) — Organized agents (L2 protocol designers, exchange operators, institutional adopters) see the pragmatic synthesis as a temporary institutional form with a sunset: as L2 solutions mature and their security properties are formalized, the need to constrain L1 immutability decreases. The consensus kernel becomes less contested as practical scalability solutions decouple from ideological demands for base-layer flexibility. Sunset logic: when L2 solutions achieve sufficient transaction throughput and institutional trust, the pressure for L1 changes diminishes, and the pragmatic synthesis's enforcement burden drops.
constraint_indexing:constraint_classification(bitcoin_consensus_kernel__pragmatic_synthesis, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ACADEMIC CONSENSUS NARRATIVES (PITON) — The 'immutable base layer' and 'flexible upper layers' framing has become increasingly performative in academic discourse. The original function — to resolve the coordination problem between purists and pragmatists — is degraded because the dichotomy naturalizes what is actually a choice about institutional design. Narratives about 'mathematical immutability' or 'layered governance' carry institutional inertia (cited in protocol papers, encoded in governance structures) despite weak empirical grounding. Theater ratio (0.55) reflects that much academic discussion treats the pragmatic synthesis as discovered architecture rather than constructed compromise.
constraint_indexing:constraint_classification(bitcoin_consensus_kernel__pragmatic_synthesis, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal perspective, the pragmatic synthesis could be naturalized as an immutable property of distributed consensus itself: any consensus system requires some core claims to be stable (immutable) to function as a coordination anchor. Therefore, some layer must resist change. This perspective treats the separation as mathematical necessity. However, this risks false summit naturalization — the choice of WHICH claims to place in the immutable core is contingent, not immutable.
constraint_indexing:constraint_classification(bitcoin_consensus_kernel__pragmatic_synthesis, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_consensus_kernel__pragmatic_synthesis_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bitcoin_consensus_kernel__pragmatic_synthesis, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bitcoin_consensus_kernel__pragmatic_synthesis, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(bitcoin_consensus_kernel__pragmatic_synthesis, TR),
    TR >= 0.70.

:- end_tests(bitcoin_consensus_kernel__pragmatic_synthesis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Low-to-moderate. The pragmatic synthesis does extract from purists (they cannot obtain full immutability across all layers) and from ideological coherence (the reading requires holding two potentially contradictory commitments). However, the extraction is bounded: L2 developers genuinely benefit from both base-layer stability and upper-layer flexibility, so there is real coordination value alongside the extraction. The measurement reflects that this is a compromise, not a pure snare. The trajectory from 0.22 to 0.32 over the interval shows increasing tension — as L2 solutions proliferate, the need to defend base-layer immutability becomes more contested, and extractiveness rises. Suppression (0.38): Moderate. Purists are suppressed by consensus rules that permit L2 flexibility; they cannot easily fork without losing network effects. However, suppression is not total — purists retain voice in governance and can attempt to convince the network of their position. This is constrained rather than trapped-level suppression. Theater ratio (0.55): Moderate-high. The pragmatic synthesis carries performative elements: framing base-layer rules as 'immutable' and L2 rules as 'flexible' naturalizes a choice about institutional design. As L2 solutions mature, the pragmatic framework becomes more theatrical — the base layer is treated as unchangeable not because change is impossible but because consensus has decided it is inviolable, yet this inviolability is itself contingent on L2 success.
 *
 * PERSPECTIVAL GAP:
 *   The purist sees a snare: trapped by a consensus that permits L2 flexibility, unable to exit without losing social proof. The L2 developer sees tangled rope: genuine coordination benefits alongside extraction costs. The institutional maintainer sees rope: pure coordination through distributed governance. The pragmatic coalition sees a temporary scaffold: a coordination mechanism with a sunset as L2 matures. The piton observer sees degraded institutional practice: the immutable/flexible dichotomy persists through inertia despite weakening empirical rationale. The civilizational analyst risks seeing a mountain: consensus immutability as a law of distributed systems. This last perspective is a false summit — the choice to place specific rules in an immutable core is contingent, not mathematical necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's effective extractiveness (χ) is derived from base extractiveness (0.32), the agent's structural relationship to the constraint (beneficiary vs. victim), and their exit options. Purists experience maximum directionality (d ≈ 0.95): they are victims of pragmatic synthesis (constrained from achieving full immutability) with no exit option (trapped by identity lock to founding principle). Their f(d) ≈ 1.42 amplifies the base extractiveness toward snare territory (χ ≈ 0.45). L2 developers experience moderate directionality (d ≈ 0.50): they are symmetric — victims of base-layer immutability constraint alongside beneficiaries of L2 flexibility. Their f(d) ≈ 0.65 produces tangled rope χ. Institutional maintainers experience low directionality (d ≈ 0.15): they benefit from the pragmatic synthesis (arbitrage between layers reduces governance burden). Their f(d) ≈ -0.01 produces negative effective extraction — rope experience. The pragma coalition experiences moderate directionality through their constrained exit options and organized power: d ≈ 0.45, f(d) ≈ 0.50, producing scaffold χ. The directionality overrides are not needed — the structural data (beneficiary/victim + exit options) produce the right d values directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The pragmatic synthesis resolves the mandatrophy by accepting that the bitcoin kernel reading is inherently contested. Rather than insisting that one reading is 'correct' (which would foreclose the others), the pragmatic synthesis proposes a coexistence framework: both immutability and flexibility are needed, but at different layers. This is not a logical resolution (it does not prove both readings coherent within a single framework) but a practical one — the layered architecture allows both camps to claim victory. The extractiveness of 0.32 reflects the cost of this pragmatism: ideological coherence is sacrificed on the altar of institutional stability. The constraint's theater ratio (0.55) reveals that much of the pragmatic synthesis's legitimacy is performative — narratives about 'immutable base layers' and 'mathematical constants' carry institutional weight despite being contingent choices.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this the pragmatic_synthesis reading, or is it a hybrid that covertly instantiates both maximalist and utility readings?',
    'Examine hard forks and protocol governance decisions: does the reading actually enforce immutability on the base layer (pragmatic commitment) or does consensus regularly modify base-layer rules (utility-reading realization)?',
    'If pragmatic synthesis is enforced: constraint is scaffold with real sunset. If base layer permits regular modification: reading has collapsed into utility reading, and the pragmatic synthesis is theater (piton).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, empirical, 'Whether the pragmatic synthesis reading is genuinely instantiated or collapsed into utility reading').

omega_variable(
    layer_two_security_ceiling,
    'Can L2 solutions achieve sufficient security and adoption without requiring base-layer rule changes to handle novel L2 failure modes?',
    'Historical analysis of L2 protocol failures; correlation between L2 incidents requiring L1 intervention vs. L2 self-correction; maturation timeline for L2 security standards',
    'If L2 achieves independence: scaffold sunset is real, pragmatic synthesis exits cleanly. If L2 regularly requires L1 intervention: the immutability constraint is breached, and the pragmatic synthesis collapses into managed flexibility (utility reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(layer_two_security_ceiling, empirical, 'Whether L2 solutions can mature independently of L1 flexibility').

omega_variable(
    ideological_coherence_cost,
    'What is the structural cost to ideological coherence of maintaining both ''immutable base'' and ''flexible layers'' simultaneously?',
    'Documentary analysis of consensus discussions, governance forums, and protocol amendment cycles; measurement of cognitive dissonance (contradiction citations) in academic and practitioner discourse',
    'If coherence cost is high and accumulating: the pragmatic synthesis is unsustainable (scaffold with no real sunset). If coherence cost stabilizes: the reading has achieved equilibrium and may persist or transition to piton status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ideological_coherence_cost, conceptual, 'Structural cost of holding pragmatic synthesis coherently').

omega_variable(
    maximalist_purist_exit,
    'Do maximalist purists exit the system, or do they remain and agitate for base-layer rule changes?',
    'Measurement of purist participation in governance over time; correlation between pragmatic synthesis adoption and purist fork events or withdrawal',
    'If purists exit: their victimization by the pragmatic synthesis is resolved through self-selection. If purists remain and agitate: the suppression metric is understated, and the constraint is more snare-like than scaffold-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maximalist_purist_exit, empirical, 'Exit behavior of base-layer purists under pragmatic synthesis regime').

omega_variable(
    reading_foreclosure_by_empirical_pressure,
    'Will empirical pressure from L2 scaling successes or failures foreclose either the maximalist or utility readings, forcing convergence on pragmatic synthesis as the only coherent remaining option?',
    'Monitor L2 adoption metrics, transaction throughput, security incident rates, and institutional custody patterns; track governance discourse for signs of readings becoming untenable',
    'If foreclosure occurs: pragmatic synthesis transitions from contested reading to canonical form. If no foreclosure: the three readings remain coexistent, and the pragmatic synthesis is a temporary synthesis under constant pressure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_by_empirical_pressure, empirical, 'Whether empirical outcomes will foreclose non-pragmatic readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__pragmatic_synthesis, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bck_prag_tr_t0, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 0, 0.42).
narrative_ontology:measurement(bck_prag_tr_t5, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 5, 0.5).
narrative_ontology:measurement(bck_prag_tr_t10, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(bck_prag_be_t0, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(bck_prag_be_t5, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(bck_prag_be_t10, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 10, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__pragmatic_synthesis, resource_allocation).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__pragmatic_synthesis, bitcoin_consensus_kernel__maximalist_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__pragmatic_synthesis, bitcoin_consensus_kernel__utility_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__pragmatic_synthesis, layer_two_scaling_trilemma).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__pragmatic_synthesis, consensus_governance_burden).

% DUAL FORMULATION NOTE:
% The pragmatic synthesis is one reading of the bitcoin consensus kernel. The other readings (maximalist, utility) are separate constraints with different ε values and different beneficiary/victim structures. All three readings share the same underlying kernel but instantiate different conclusions about how to resolve the tension between immutability and flexibility. This story focuses on the pragmatic synthesis as a scaffold — a temporary institutional form with a sunset clause. As L2 solutions mature, the tension decreases, and the need to maintain the pragmatic compromise diminishes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
