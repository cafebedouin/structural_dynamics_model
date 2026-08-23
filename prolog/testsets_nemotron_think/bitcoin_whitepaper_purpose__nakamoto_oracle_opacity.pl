% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper_purpose__nakamoto_oracle_opacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bitcoin_whitepaper_purpose__nakamoto_oracle_opacity
 *   human_readable: Nakamoto Oracle Opacity — Interpretive Vacuum After Founder Disappearance
 *   domain: technology_governance/distributed_systems/monetary_theory
 *
 * SUMMARY:
 *   Satoshi Nakamoto's final communication was April 2011. The whitepaper —
 *   'Bitcoin: A Peer-to-Peer Electronic Cash System' — became a fixed text
 *   with no living author to adjudicate its meaning. This created an
 *   interpretive vacuum: every protocol dispute (block size, SegWit, Taproot,
 *   ordinals, drivechains) becomes a contest over 'what Satoshi intended' or
 *   'what the whitepaper *really* means.' The vacuum is not passive; it
 *   actively structures the system. Forks (BTC/BCH/BSV) are the vacuum's
 *   materialization — each claims whitepaper fidelity while changing
 *   fundamental parameters. Miners sell signaling as governance leverage.
 *   Core developers guard merge access as interpretive authority. Exchanges
 *   profit from listing fork coins. Users and builders bear the uncertainty.
 *   No convergence mechanism exists because the kernel (the whitepaper) is
 *   fixed, the oracle is gone, and the authority_grounding is distributed
 *   with no interpretation_layer_present.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.38).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.15).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, extractiveness, 0.38).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, "Nakamoto Oracle Opacity — Interpretive Vacuum After Founder Disappearance").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, "technology_governance/distributed_systems/monetary_theory").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, '115da176-34a1-48a3-b406-4ea7e0a523a8').
narrative_ontology:cs_kernel_codification('115da176-34a1-48a3-b406-4ea7e0a523a8', fixed_text).
narrative_ontology:cs_authority_grounding('115da176-34a1-48a3-b406-4ea7e0a523a8', distributed).
narrative_ontology:cs_reading_relation('115da176-34a1-48a3-b406-4ea7e0a523a8', bitcoin_whitepaper_purpose__store_of_value_reading, coexists_with).
narrative_ontology:cs_reading_relation('115da176-34a1-48a3-b406-4ea7e0a523a8', bitcoin_whitepaper_purpose__electronic_cash_reading, coexists_with).
narrative_ontology:cs_axiom('115da176-34a1-48a3-b406-4ea7e0a523a8', foundational, authoritative_interpretation_irretrievably_lost).
narrative_ontology:cs_axiom_status(authoritative_interpretation_irretrievably_lost, holdable).
narrative_ontology:cs_axiom_grounding('115da176-34a1-48a3-b406-4ea7e0a523a8', authoritative_interpretation_irretrievably_lost, empirically_contingent).
narrative_ontology:cs_axiom('115da176-34a1-48a3-b406-4ea7e0a523a8', foundational, interpretive_vacuum_enables_legitimate_fork_claims).
narrative_ontology:cs_axiom_status(interpretive_vacuum_enables_legitimate_fork_claims, holdable).
narrative_ontology:cs_axiom_grounding('115da176-34a1-48a3-b406-4ea7e0a523a8', interpretive_vacuum_enables_legitimate_fork_claims, conventional).
narrative_ontology:cs_reference_frame('115da176-34a1-48a3-b406-4ea7e0a523a8', nakamoto_authoritative_presence).
narrative_ontology:cs_drift_state('115da176-34a1-48a3-b406-4ea7e0a523a8', post_2011_disappearance, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('115da176-34a1-48a3-b406-4ea7e0a523a8', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, core_developers_claiming_whitepaper_fidelity).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, fork_proponents_claiming_whitepaper_fidelity).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, miners_capturing_governance_rents).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, users_seeking_protocol_stability).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, application_developers_needing_predictable_base_layer).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, institutional_adopters_requiring_governance_legibility).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, exchanges_and_custodians).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, code_is_law_doctrine).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, decentralization_precludes_centralized_interpretation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the Bitcoin Core reference implementation and claim interpretive authority over the whitepaper's intent. Their professional identity and institutional position (GitHub merge rights, BIP process control) are fused with the 'guardians of the protocol' narrative. Exit means abandoning a career-defining identity and the social capital of being 'the' Bitcoin developers. They benefit from the interpretive vacuum because no external authority can overrule their reading.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, core_developers_claiming_whitepaper_fidelity, agenda_setter,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, core_developers_claiming_whitepaper_fidelity, beneficiary).

% Launch alternative chains (Bitcoin Cash, Bitcoin SV, etc.) each claiming to be the 'true' realization of the whitepaper's vision. They benefit from the interpretive vacuum because the whitepaper's ambiguity lets them legitimately claim fidelity while changing fundamental parameters. Their exit from the main chain is constrained by network effects — they must bootstrap new liquidity, exchange listings, and hash power — but the vacuum makes the fork itself a viable strategy rather than an illegitimate split.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, fork_proponents_claiming_whitepaper_fidelity, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, fork_proponents_claiming_whitepaper_fidelity, agenda_setter).

% Control hash power and thus de facto veto over protocol changes. In the interpretive vacuum, their signaling (or refusal to signal) becomes a governance lever. They extract rents by selling signaling support, delaying upgrades that threaten fee revenue, or credibly threatening chain splits. Their exit is arbitrage-grade: they can switch hash power between SHA-256 chains instantly, making them the most mobile actors in the system.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, miners_capturing_governance_rents, beneficiary,
    powerful, immediate, arbitrage, global).

% Hold bitcoin as savings or use it for payments. They bear the cost of chain splits (replay risk, confusion, custody complexity), governance uncertainty (will the 21M cap hold? will fees explode?), and the cognitive load of evaluating competing fidelity claims. Their exit is constrained: leaving Bitcoin means abandoning the largest liquidity pool and network effect, but staying means accepting governance opacity.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, users_seeking_protocol_stability, payer,
    powerless, biographical, constrained, global).

% Build wallets, Lightning nodes, exchanges, and financial products on Bitcoin. They pay in engineering uncertainty: every fork threat requires contingency code, every governance dispute risks API instability. Their exit is constrained by sunk cost in Bitcoin-specific infrastructure and user expectations — migrating to another chain means rebuilding from scratch.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, application_developers_needing_predictable_base_layer, payer,
    moderate, biographical, constrained, global).

% Corporate treasuries, ETF issuers, nation-state adopters. They need legible governance for compliance, risk management, and fiduciary duty. The interpretive vacuum creates regulatory and legal uncertainty: who speaks for Bitcoin? They pay in delayed adoption, higher risk premiums, and legal overhead. Their exit is mobile at the institutional level — they can allocate to other assets or chains with clearer governance — but doing so sacrifices Bitcoin's unique monetary properties.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, institutional_adopters_requiring_governance_legibility, payer,
    institutional, generational, mobile, global).

% List fork coins, charge listing fees, capture trading volume from chain-split speculation. They benefit from the interpretive vacuum because every governance dispute generates tradable assets (fork coins) and volume. Their exit is arbitrage-grade: they are protocol-agnostic infrastructure that profits from fragmentation.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, exchanges_and_custodians, beneficiary,
    institutional, biographical, arbitrage, global).

% Study Bitcoin as a governance experiment, a monetary phenomenon, or a legal object. They see the full structure: the whitepaper as fixed text, the vacuum as structural condition, the competing readings as live positions. They neither collect nor pay — they document.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, academic_and_legal_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The whitepaper itself coordinates a global, trust-minimized monetary system without a central operator. The interpretive vacuum is not the coordination function — it is the *absence* of the coordination function's authoritative maintenance layer. The coordination function that *persists* is the protocol's automated consensus rules, which continue operating without human interpretation.
% TRANSFER_FUNCTION: Moves governance legitimacy and protocol-direction authority from a single identifiable founder (pre-2011) to a contested field of claimants (post-2011). The transfer is not of money but of *interpretive authority* — who gets to say 'this change honors the whitepaper' — and the rents that attach to that authority (miner signaling value, exchange listing power, narrative control).
% ABSENT_VOICES: Satoshi Nakamoto (the oracle whose disappearance created the vacuum). Early contributors who left before 2011 (Hal Finney, Gavin Andresen's early phase) and might have provided continuity. Potential users in jurisdictions where governance uncertainty prevents adoption. The 'silent majority' of non-technical holders who cannot evaluate fidelity claims.
% DISAPPEARANCE_RATIONALE: If the interpretive vacuum vanished overnight (e.g., Nakamoto returned with a signed message, or a universally accepted governance mechanism emerged), the fork proliferation would collapse, the competing fidelity claims would converge or be falsified, miner governance leverage would diminish, and institutional adoption would accelerate. The vacuum is the *cause* of the current multi-chain equilibrium; removing it rearranges the world.
% FOUNDING_PROBLEM: How to achieve decentralized consensus on a monetary ledger without a trusted third party — including without a trusted interpreter of the system's own rules.
% FOUNDING_PROBLEM_CORROBORATION: The whitepaper itself frames the problem as eliminating trusted third parties, not replacing one trusted interpreter (Nakamoto) with many contested ones. Early Bitcointalk threads (2010-2011) show Nakamoto explicitly refusing to act as ongoing authority ('I've moved on to other things'). The Core developer community corroborates that the founding problem *included* solving governance without a founder — but disputes whether the vacuum is the intended solution or a failure mode. No external corroboration exists for the claim that the vacuum itself is the solution.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity_tests).
:- end_tests(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) is moderate: the vacuum transfers interpretive authority rents to miners, developers, and exchanges, but the base protocol continues functioning — extraction is layered on coordination, not replacing it. Suppression (0.15) is low: no one prevents forks or alternative readings; the vacuum *enables* proliferation. Theater ratio (0.42) is significant: much governance discourse performs 'whitepaper fidelity' while advancing material interests (miner fees, developer control, exchange volume). Accessibility collapse (0.35) is moderate: alternatives (other chains, layer-2s, off-chain governance) exist but the vacuum makes them costly to coordinate around. Resistance (0.55) is high: multiple factions actively resist each other's interpretations, preventing any single reading from stabilizing.
 *
 * PERSPECTIVAL GAP:
 *   From the core developer seat, the vacuum is a feature — decentralization means no interpreter, and their role is maintenance not authority. From the fork proponent seat, the vacuum is opportunity — it legitimizes their chain as 'the true Bitcoin.' From the user/app developer seat, the vacuum is a cost — they need predictability, not interpretive contest. From the miner seat, the vacuum is a revenue stream. The engine computes these as different χ values from the same ε.
 *
 * DIRECTIONALITY LOGIC:
 *   Core developers and fork proponents are identity-locked beneficiaries: their professional identity and chain's legitimacy depend on claiming whitepaper fidelity in the vacuum. Miners and exchanges are mobile/arbitrage beneficiaries: they extract rents from the vacuum without identity commitment. Users, app developers, and institutions are constrained/mobile payers: they bear uncertainty costs with varying exit options. The analytical observer sees the full structure but neither pays nor collects.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (decentralized consensus without trusted third party) is live — but the *interpretive layer* that Nakamoto provided has atrophied into a vacuum. The mandate 'no trusted interpreter' has outlived its function (coordinating the early network) and now enables the very trusted-interpreter dynamics it sought to eliminate (core devs as de facto interpreters, miners as de facto governors). This is mandatrophy: the constraint (no central interpreter) persists but its function has inverted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vacuum_as_feature_or_bug,
    'Is the interpretive vacuum a designed feature of Bitcoin''s decentralization (no single point of interpretive failure) or an unintended bug (governance paralysis enabling capture)?',
    'Counterfactual analysis: if Nakamoto had appointed a successor or created a formal governance mechanism before disappearing, would the system be more or less resilient to capture? Compare with projects that had explicit succession (Ethereum Foundation, Tezos on-chain governance).',
    'If feature: the vacuum is a mountain-like property of the system — extractiveness metrics measure the cost of decentralization itself. If bug: the vacuum is a piton/tangled_rope — an atrophied governance layer that persists by inertia and enables extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vacuum_as_feature_or_bug, conceptual, 'Whether the interpretive vacuum is structural necessity or governance failure.').

omega_variable(
    whitepaper_semantic_determinacy,
    'Does the whitepaper text contain sufficient semantic determinacy to resolve the block-size/scaling dispute and other governance questions, or is it fundamentally underdetermined?',
    'Linguistic and game-theoretic analysis of the whitepaper''s claims: does ''peer-to-peer electronic cash'' entail specific throughput/fee parameters? Does ''one-CPU-one-vote'' entail ASIC resistance? Formal modeling of whether the text underdetermines the contested parameters.',
    'If determinate: one reading is objectively correct and the vacuum is pure extraction by bad-faith actors. If underdetermined: the vacuum is genuine — multiple faithful readings exist and the contest is structural, not opportunistic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(whitepaper_semantic_determinacy, conceptual, 'Whether the kernel text itself resolves the disputes or enables them.').

omega_variable(
    committer_structure_nakamoto_oracle_opacity,
    'How does this reading''s structural relationship to the bitcoin_whitepaper_purpose kernel differ from its siblings?',
    'Structural comparison of the three readings'' beneficiary/victim sets, exit options, and claimed_type assignments. The oracle-opacity reading uniquely treats the *absence of authoritative interpretation* as the constraint itself, rather than a specific parameter choice (block size, fee market, etc.).',
    'If the oracle-opacity reading is structurally distinct (different ε, different victims), it confirms the kernel decomposes into multiple constraints. If it collapses into one of the siblings, the kernel is not genuinely contested at the constraint level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_nakamoto_oracle_opacity, empirical, 'Committer-frame structural delta: interpretive vacuum as constraint vs. parameter disputes as constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 2011, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(btc_opacity_tr_t2011, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2011, 0.08).
narrative_ontology:measurement(btc_opacity_tr_t2013, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2013, 0.15).
narrative_ontology:measurement(btc_opacity_tr_t2015, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2015, 0.28).
narrative_ontology:measurement(btc_opacity_tr_t2017, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2017, 0.45).
narrative_ontology:measurement(btc_opacity_tr_t2019, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2019, 0.41).
narrative_ontology:measurement(btc_opacity_tr_t2021, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2021, 0.43).
narrative_ontology:measurement(btc_opacity_tr_t2024, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(btc_opacity_be_t2011, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2011, 0.12).
narrative_ontology:measurement(btc_opacity_be_t2013, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2013, 0.18).
narrative_ontology:measurement(btc_opacity_be_t2015, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2015, 0.25).
narrative_ontology:measurement(btc_opacity_be_t2017, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2017, 0.35).
narrative_ontology:measurement(btc_opacity_be_t2019, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2019, 0.33).
narrative_ontology:measurement(btc_opacity_be_t2021, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2021, 0.36).
narrative_ontology:measurement(btc_opacity_be_t2024, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2024, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(btc_opacity_su_t2011, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2011, 0.05).
narrative_ontology:measurement(btc_opacity_su_t2013, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2013, 0.08).
narrative_ontology:measurement(btc_opacity_su_t2015, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2015, 0.12).
narrative_ontology:measurement(btc_opacity_su_t2017, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2017, 0.18).
narrative_ontology:measurement(btc_opacity_su_t2019, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2019, 0.15).
narrative_ontology:measurement(btc_opacity_su_t2021, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2021, 0.14).
narrative_ontology:measurement(btc_opacity_su_t2024, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, identity_coordination).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.08).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_whitepaper_purpose__store_of_value_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_whitepaper_purpose__electronic_cash_reading).

% DUAL FORMULATION NOTE:
% The bitcoin_whitepaper_purpose kernel decomposes into three constraint stories: (1) store_of_value_reading — low ε, Mountain-like from its proponents' seat; (2) electronic_cash_reading — moderate ε, Tangled Rope from its proponents' seat; (3) nakamoto_oracle_opacity (this story) — the interpretive vacuum as a distinct constraint with its own ε=0.38, beneficiaries (devs, forkers, miners), and victims (users, builders, institutions). The vacuum is not merely the stage for the other two readings' contest — it has independent extractive structure (miner signaling rents, exchange fork-coin revenue, dev identity rents) and independent victims (governance uncertainty costs). All three stories link via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, organized, 0.25).
constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, powerful, 0.15).
constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, powerless, 0.85).
constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, moderate, 0.7).
constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
