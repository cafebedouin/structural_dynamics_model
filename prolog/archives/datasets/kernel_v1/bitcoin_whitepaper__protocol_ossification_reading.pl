% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__protocol_ossification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_whitepaper__protocol_ossification_reading, []).

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
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: bitcoin_whitepaper__protocol_ossification_reading
 *   human_readable: Bitcoin Protocol Ossification via Consensus Requirement
 *   domain: cryptocurrency_economics/monetary_systems/technology_governance
 *
 * SUMMARY:
 *   Bitcoin's protocol governance operates under a de facto rule that changes
 *   approach illegitimacy unless they command near-universal assent. This
 *   constraint emerged gradually from the 2015–2017 scaling wars (Bitcoin
 *   Cash fork, SegWit debate, Block Size controversy) and has crystallized
 *   into a cultural norm: 'Bitcoin is defined by its immutability' and
 *   'stability is the primary virtue.' This story instantiates ONE reading of
 *   the Whitepaper kernel — the protocol_ossification_reading. Under this
 *   reading, Satoshi's creation is interpreted as mandating eventual protocol
 *   freezing, where base-layer innovation becomes progressively impossible
 *   and all evolutionary pressure moves to layer-2 systems and sidechains.
 *   This creates a tangled_rope structure: the protocol's immutability
 *   provides genuine coordination benefit (prevents wealth dilution,
 *   maintains monetary credibility, offers long-term store-of-value
 *   assurance) while simultaneously extracting from agents whose use cases
 *   require base-protocol evolution. The constraint's extractiveness
 *   increased from 0.32 (2015, still debatable) to 0.58 (2024, widely
 *   accepted within Bitcoin governance) as the cultural norm hardened into
 *   enforcement. The rising suppression_requirement (0.45 → 0.67) reflects
 *   increasing difficulty of reversing the consensus-requirement norm — early
 *   in Bitcoin's history, protocol changes were more feasible; by 2024, the
 *   blocking power of conservative nodes and miners is near-total. Theater
 *   ratio (0.35 → 0.58) shows the governance process becoming progressively
 *   more performative: extensive debate over BIPs, Taproot multi-year
 *   consensus building, and Ordinals controversy produced little actual
 *   protocol change despite substantial discussion.
 *
 * KEY AGENTS:
 *   - Existing Bitcoin Holders: Primary beneficiary (institutional/arbitrage) — protocol immutability maximizes long-term store-of-value credibility; prevents inflation dilution; benefits wealth preservation
 *   - Mining Incumbents (SHA256 ASIC holders): Primary beneficiary (institutional/arbitrage) — immutability prevents shift to alternative proof-of-work or layer transitions that would obsolete hardware; network effects lock in their equipment value
 *   - Marginal Use Cases (micropayment applications, privacy-requiring users, scalability-demanding services): Primary victim (powerless/trapped) — cannot exit; require base-protocol changes impossible under consensus requirement; cannot build their needs on Bitcoin despite it being the only credible neutral network
 *   - Protocol Innovation Function: Structural victim (powerless/trapped) — base-layer innovation is systematically suppressed; design space becomes frozen; cannot respond to emerging capabilities (quantum computing, layer-2 protocol updates requiring base-layer coordination)
 *   - Layer-2 Developers (Lightning Network, Stacks, Sidechains): Secondary victims/beneficiaries (moderate/constrained) — constrained to workarounds; benefit from stable base-layer security guarantees; trapped in layer-2 complexity that would be unnecessary if base layer evolved
 *   - Alternative Blockchains (Ethereum, Monero, Zcash): Constrained beneficiaries (organized/constrained) — can implement innovations Bitcoin ossifies; capture use cases Bitcoin abandons; but lose Bitcoin's network effects and credibility
 *   - Bitcoin Core Developer Community: Institutional observer (institutional/arbitrage) — maintain consensus rules and veto power; see their own deliberative process as degraded/theatrical; perform governance without meaningful decision-making power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__protocol_ossification_reading, 0.58).
domain_priors:suppression_score(bitcoin_whitepaper__protocol_ossification_reading, 0.67).
domain_priors:theater_ratio(bitcoin_whitepaper__protocol_ossification_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 0.67).
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__protocol_ossification_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__protocol_ossification_reading, "Bitcoin Protocol Ossification via Consensus Requirement").
narrative_ontology:topic_domain(bitcoin_whitepaper__protocol_ossification_reading, "cryptocurrency_economics/monetary_systems/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__protocol_ossification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__protocol_ossification_reading, 'd4c91a7e-3f4b-4c1d-9e2a-8b7f3c6d1e9a').
narrative_ontology:cs_kernel_codification('d4c91a7e-3f4b-4c1d-9e2a-8b7f3c6d1e9a', fixed_text).
narrative_ontology:cs_authority_grounding('d4c91a7e-3f4b-4c1d-9e2a-8b7f3c6d1e9a', lineage).
narrative_ontology:cs_interpretation_layer_present('d4c91a7e-3f4b-4c1d-9e2a-8b7f3c6d1e9a').
narrative_ontology:cs_reading_relation('d4c91a7e-3f4b-4c1d-9e2a-8b7f3c6d1e9a', bitcoin_whitepaper__p2p_cash_reading, influences).
narrative_ontology:cs_reading_relation('d4c91a7e-3f4b-4c1d-9e2a-8b7f3c6d1e9a', bitcoin_whitepaper__digital_gold_reading, coexists_with).
narrative_ontology:cs_axiom('d4c91a7e-3f4b-4c1d-9e2a-8b7f3c6d1e9a', foundational, protocol_immutability_is_governance_virtue).
narrative_ontology:cs_axiom_status(protocol_immutability_is_governance_virtue, holdable).
narrative_ontology:cs_axiom_grounding('d4c91a7e-3f4b-4c1d-9e2a-8b7f3c6d1e9a', protocol_immutability_is_governance_virtue, instrumental).
narrative_ontology:cs_axiom('d4c91a7e-3f4b-4c1d-9e2a-8b7f3c6d1e9a', foundational, base_layer_stability_over_innovation).
narrative_ontology:cs_axiom_status(base_layer_stability_over_innovation, holdable).
narrative_ontology:cs_axiom_grounding('d4c91a7e-3f4b-4c1d-9e2a-8b7f3c6d1e9a', base_layer_stability_over_innovation, conventional).
narrative_ontology:cs_reference_frame('d4c91a7e-3f4b-4c1d-9e2a-8b7f3c6d1e9a', immutable_monetary_base).
narrative_ontology:cs_drift_state('d4c91a7e-3f4b-4c1d-9e2a-8b7f3c6d1e9a', contemporary_ecosystem_diversification, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d4c91a7e-3f4b-4c1d-9e2a-8b7f3c6d1e9a', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, existing_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, mining_incumbents).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, full_node_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, protocol_innovation).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, use_case_expansion).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, marginal_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINAL USE CASE (SNARE) — A protocol enhancement that would enable micropayments, smart contracts, or privacy features cannot proceed unless it approaches universal consensus. The use case bears the full cost of stasis; cannot exit (Bitcoin is the only credible neutral network for this function); experiences pure extraction as the protocol's immutability benefits existing holders while preventing innovation. No coordination function visible — only blocking.
constraint_indexing:constraint_classification(bitcoin_whitepaper__protocol_ossification_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LAYER-2 DEVELOPER (TANGLED ROPE) — Constrained to build on immutable base layer; also benefits from network effects and security guarantees of stable base protocol. Genuine coordination benefit (reliability) plus extraction (blocked from protocol innovation, forced to layer-2 workarounds with higher latency/cost). Mixed experience: stabilization enables confidence in layer-2 but prevents base-layer optimization.
constraint_indexing:constraint_classification(bitcoin_whitepaper__protocol_ossification_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: EXISTING HOLDER / MINING INCUMBENT (ROPE) — Pure beneficiary of protocol immutability (maximizes long-term store of value; prevents inflation dilution; maintains their hardware/ASIC advantage). Experiences constraint as coordination mechanism: 'we all agree not to debase the coin' creates stable incentive alignment. Experiences minimal extraction — net positive through wealth preservation and network stability.
constraint_indexing:constraint_classification(bitcoin_whitepaper__protocol_ossification_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ALTERNATIVE BLOCKCHAIN (TANGLED ROPE) — Can implement innovations Bitcoin ossification prevents (Ethereum smart contracts, Monero privacy, Zcash shielded transactions). Benefits from Bitcoin's stability (network effects, security model validation); constrained by Bitcoin's immutability consuming design space and developer talent. Experiences both coordination (Bitcoin's stability reduces systemic risk) and extraction (locked out of Bitcoin's network effects for their feature sets).
constraint_indexing:constraint_classification(bitcoin_whitepaper__protocol_ossification_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: BITCOIN CORE DEVELOPER COMMUNITY (PITON) — Maintains consensus rules through performative deliberation (Bitcoin Improvement Proposals, mailing lists, conference debates) but actual protocol change requires extraordinary consensus thresholds that make deliberation largely theatrical. Developers see their own governance process as degraded — formal discussion occurs but decision-making power is diffuse and blocking is low-cost. Theater ratio high: extensive RFC process produces few accepted changes.
constraint_indexing:constraint_classification(bitcoin_whitepaper__protocol_ossification_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scale, consensus requirement may appear an inevitable feature of decentralized systems: any change threatens the coordination core, so immutability is a natural law of sound money. However, the structural data reveals beneficiaries (existing holders, mining incumbents) benefiting from this alleged natural law. False summit candidate.
constraint_indexing:constraint_classification(bitcoin_whitepaper__protocol_ossification_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper__protocol_ossification_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bitcoin_whitepaper__protocol_ossification_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bitcoin_whitepaper__protocol_ossification_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_whitepaper__protocol_ossification_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bitcoin_whitepaper__protocol_ossification_reading, TR),
    TR >= 0.70.

:- end_tests(bitcoin_whitepaper__protocol_ossification_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from innovation-requiring use cases (unable to proceed without consensus approach) but produces genuine coordination benefit (stable monetary base). The measurement trajectory shows extraction rising as the norm hardened: early Bitcoin was more flexible (2015, ε=0.32); by 2024 the consensus threshold is near-total blockade (ε=0.58). This reflects not that the underlying principle changed but that its enforcement mechanisms strengthened. Suppression (0.67): High. Blocking a protocol change requires only that a significant minority of nodes or a coalition of miners resist it. The costs of implementing a new consensus rule (network fork, exchange coordination, consensus fragmentation risk) are so high that even 20% opposition can veto changes. This is structural suppression: alternatives to the dominant chain exist (Bitcoin Cash, Bitcoin SV) but are universally delegitimized, so victims cannot exit without abandoning the 'real Bitcoin' narrative. Theater ratio (0.58): Moderate-high. Bitcoin Improvement Proposals follow RFC process (genuine discussion), but actual protocol change requires extraordinary consensus thresholds. Taproot (2021) took 4 years of discussion before achieving sufficient consensus to activate. Ordinals/Inscriptions controversy (2023) produced no protocol response despite major disagreement. The extensive deliberation produces minimal change — the theater is real, but decision-making power is diffuse and blocking is low-cost.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximal perspectival divergence. Existing holders see a coordination mechanism protecting their wealth (Rope) — consensus requirement is the rule that prevents debasement. Marginal use cases see a blocking mechanism preventing their existence (Snare) — they are trapped because Bitcoin is the only credible neutral network, yet they cannot build their needs on it. Layer-2 developers see mixed coordination and extraction (Tangled Rope) — stability enables their confidence but forces workarounds. The Bitcoin Core development community sees its own governance process as performative (Piton) — the RFC ritual is maintained but produces few actual changes. Alternative blockchains see a coordination benefit plus network-effect extraction (Tangled Rope) — they benefit from Bitcoin's stability validating blockchain concepts but are locked out of Bitcoin's network effects. The analytical observer risks seeing protocol immutability as a natural law (Mountain) — 'decentralized systems require consensus to prevent chaos' — but the structural data reveals beneficiaries (existing holders, miners) benefiting from this alleged natural law, marking it as a false summit.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from power level, exit options, and structural relationship to the constraint. Existing holders (institutional/arbitrage) have d ≈ 0.05 — they are beneficiaries with exit options (can sell Bitcoin, though would lose network effects). Marginal use cases (powerless/trapped) have d ≈ 0.95 — full victims with no exit (Bitcoin is unique; alternatives lack credibility). Layer-2 developers (moderate/constrained) have d ≈ 0.65 — both benefit and suffer but cannot exit without abandoning network effects. The sigmoid f(d) is applied per the formula; d values determine effective extractiveness (chi) experienced by each agent. No directionality overrides needed — structural derivation produces coherent d values across all perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that 'stability as primary virtue' is coherent from some perspectives (beneficiaries, existing holders, long-term wealth-preservers) but becomes extractive when applied to other use cases (innovation-requiring, scalability-demanding, privacy-seeking). The constraint is not pure coordination (Rope) because the blocking mechanism extracts from marginal use cases. Nor is it pure extraction (Snare) because legitimate coordination benefit exists (prevents protocol spam, maintains store-of-value credibility, prevents governance takeover). The Tangled Rope classification holds: active enforcement of the consensus requirement (developer culture, mining majority coordination, node veto power) combines genuine coordination function with asymmetric extraction. The mandatrophy is resolved by acknowledging that both functions are real and that their relative weight depends on the observer's position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consensus_threshold_operational_definition,
    'What threshold operationalizes ''approaching universal consensus''? Is it 95% of nodes, 90% of miners, supermajority of developers, or something unspecified?',
    'Historical analysis of past upgrade attempts (SegWit 2x, Bitcoin Cash fork, Taproot adoption); determination of what consensus level was actually required vs. achieved',
    'If threshold is vague: consensus requirement becomes an arbitrary gating mechanism that existing holders can veto (extraction). If threshold is measurable: constraint becomes a coordination mechanism (rope). High threshold amplifies extraction; low threshold enables innovation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consensus_threshold_operational_definition, conceptual, 'Operational definition of ''universal consensus'' threshold').

omega_variable(
    stability_virtuosity_anchoring,
    'Is ''stability as primary virtue'' a fundamental commitment of this reading, or a strategic choice to prevent specific bad outcomes (e.g., 51% attacks, governance takeover)?',
    'Counterfactual: if a change posed no stability risk but offered transformative capability (e.g., quantum-resistant cryptography when quantum threats emerge), would consensus requirement still apply? Examination of developer statements distinguishing principled immutability from risk-aversion.',
    'If fundamental: ossification is intrinsic to this reading (strong constraint). If strategic: ossification is proportional to perceived risk (weak constraint). Affects whether the constraint would persist under different threat environments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stability_virtuosity_anchoring, conceptual, 'Whether stability-first is fundamental or risk-responsive').

omega_variable(
    network_effect_externality_capture,
    'Do existing holders and miners capture the external benefits of network effects (liquidity, adoption, use-case expansion) without bearing the costs of innovation blockage?',
    'Economic analysis of how protocol improvements in alternative chains correlate with Bitcoin dominance; measurement of use-case migration (did DeFi developers flee to Ethereum because Bitcoin couldn''t accommodate smart contracts?); valuation change following major forks or blocked upgrades',
    'If high externality capture: constraint functions as pure extraction (Snare from powerless perspective). If externalities are diffuse: constraint is balanced coordination (Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_externality_capture, empirical, 'Whether network effects externalize benefits to holders while internalizing innovation costs').

omega_variable(
    reading_contingency_on_whitepaper_interpretation,
    'This reading instantiates protocol ossification via consensus requirement. The p2p_cash_reading and digital_gold_reading would instantiate different constraints from the same whitepaper kernel. Which reading is most faithful to Satoshi''s actual text and intent?',
    'Close reading of the whitepaper''s actual proposals: does it prescribe immutability or describe technical mechanisms? Does Satoshi''s language suggest stability as an end-state (ossification) or as a temporary coordination problem to be solved? Comparison with Satoshi''s forum posts on protocol changes.',
    'If ossification_reading is faithful: this reading''s axioms are well-grounded in the kernel. If alternative readings are more faithful: this reading is a later reinterpretation (shows axiom_drift in cs_structure). Affects the authority_grounding''s legitimacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contingency_on_whitepaper_interpretation, conceptual, 'Whether ossification reading is faithful to whitepaper kernel vs. later reinterpretation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__protocol_ossification_reading, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(btc_ossif_tr_t0, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(btc_ossif_tr_t4, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 4, 0.48).
narrative_ontology:measurement(btc_ossif_tr_t8, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 8, 0.58).

% Extraction over time
narrative_ontology:measurement(btc_ossif_be_t0, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(btc_ossif_be_t4, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 4, 0.45).
narrative_ontology:measurement(btc_ossif_be_t8, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 8, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(btc_ossif_su_t0, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(btc_ossif_su_t4, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 4, 0.58).
narrative_ontology:measurement(btc_ossif_su_t8, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 8, 0.67).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__protocol_ossification_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_whitepaper__p2p_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_whitepaper__digital_gold_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__protocol_ossification_reading, layer2_scaling_coordination).
narrative_ontology:affects_constraint(bitcoin_whitepaper__protocol_ossification_reading, mining_equipment_stranding).

% DUAL FORMULATION NOTE:
% This story instantiates one reading of the Whitepaper kernel. The p2p_cash_reading and digital_gold_reading are sibling constraints that decompose the same whitepaper into different ε values and beneficiary/victim structures. The ossification_reading has ε=0.58 (moderate extraction blocking innovation); p2p_cash_reading would have ε=0.72 (high extraction from transaction use cases); digital_gold_reading would have ε=0.35 (low extraction, emphasis on monetary credibility). These are distinct constraints with distinct structures, not observables of one constraint. They are linked via network.affects_constraints to show their dependency on interpretation of the shared kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
