% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper_purpose__store_of_value_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-12-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_whitepaper_purpose__store_of_value_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: bitcoin_whitepaper_purpose__store_of_value_reading
 *   human_readable: Bitcoin Store-of-Value Governance Reading: Decentralization Priority
 *   domain: distributed_systems/monetary_theory/technology_governance
 *
 * SUMMARY:
 *   Bitcoin's whitepaper ('A Peer-to-Peer Electronic Cash System') is a
 *   contested kernel grounding its legitimacy in Satoshi Nakamoto's text.
 *   This constraint story instantiates ONE reading: the store-of-value
 *   reading interprets decentralization and full-node verifiability as the
 *   binding constraints, subordinating on-chain transaction capacity to these
 *   goals. The 1MB block-size limit is the operational artifact that enforces
 *   this reading. Beneficiaries are long-term holders (who gain scarcity and
 *   decentralization guarantees) and node operators (who gain validation
 *   accessibility). Victims are low-value transaction users and merchants
 *   priced off the base layer by artificial scarcity. The constraint is
 *   CLAIMED as tangled_rope because it genuinely coordinates distributed
 *   consensus (coordination function) while asymmetrically extracting from
 *   users unable to afford on-chain fees (extraction component).
 *
 * KEY AGENTS:
 *   - long_term_holders (beneficiary): gain scarcity enforcement and decentralization narrative; low on-chain fee exposure
 *   - node_operators (beneficiary + agenda_setter): enforce consensus, govern through rule implementation, benefit from accessibility to participation
 *   - low_value_transaction_users (victim): priced off base layer by block-space scarcity, directed to Layer 2 or alternative systems
 *   - merchants_requiring_immediate_settlement (victim): face variable high fees or forced adoption of off-chain solutions
 *   - core_developers (agenda_setter): gate-keep the 1MB limit, defend decentralization doctrine, face community contention
 *   - mining_pool_operators (agenda_setter): benefit from fee scarcity, enforce capacity constraint
 *   - electronic_cash_advocates (excluded): read the founding problem as enabling everyday transactions; structurally foreclosed by this reading
 *   - payment_network_developers (beneficiary observer): benefit from on-chain scarcity driving Layer 2 demand
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__store_of_value_reading, 0.68).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__store_of_value_reading, 0.71).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__store_of_value_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__store_of_value_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__store_of_value_reading, "Bitcoin Store-of-Value Governance Reading: Decentralization Priority").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__store_of_value_reading, "distributed_systems/monetary_theory/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__store_of_value_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__store_of_value_reading, 'f0d17e35-eb67-4f0c-84da-c11c46d2a37d').
narrative_ontology:cs_kernel_codification('f0d17e35-eb67-4f0c-84da-c11c46d2a37d', fixed_text).
narrative_ontology:cs_authority_grounding('f0d17e35-eb67-4f0c-84da-c11c46d2a37d', extraction).
narrative_ontology:cs_reading_relation('f0d17e35-eb67-4f0c-84da-c11c46d2a37d', bitcoin_whitepaper_purpose__electronic_cash_reading, coexists_with).
narrative_ontology:cs_reading_relation('f0d17e35-eb67-4f0c-84da-c11c46d2a37d', bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, influences).
narrative_ontology:cs_axiom('f0d17e35-eb67-4f0c-84da-c11c46d2a37d', foundational, decentralization_binding_constraint).
narrative_ontology:cs_axiom_status(decentralization_binding_constraint, holdable).
narrative_ontology:cs_axiom_grounding('f0d17e35-eb67-4f0c-84da-c11c46d2a37d', decentralization_binding_constraint, instrumental).
narrative_ontology:cs_axiom('f0d17e35-eb67-4f0c-84da-c11c46d2a37d', foundational, node_accessibility_prerequisite_for_legitimacy).
narrative_ontology:cs_axiom_status(node_accessibility_prerequisite_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('f0d17e35-eb67-4f0c-84da-c11c46d2a37d', node_accessibility_prerequisite_for_legitimacy, deontological).
narrative_ontology:cs_reference_frame('f0d17e35-eb67-4f0c-84da-c11c46d2a37d', accessible_full_node_participation).
narrative_ontology:cs_drift_state('f0d17e35-eb67-4f0c-84da-c11c46d2a37d', contemporary_scaled_adoption, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f0d17e35-eb67-4f0c-84da-c11c46d2a37d', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, node_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, low_value_transaction_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, merchants_requiring_immediate_settlement).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, payment_network_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold Bitcoin as a store of value, benefiting from scarcity enforcement and decentralization guarantees. Low on-chain transaction frequency means they bear no fee burden; the retained 1MB block limit protects the scarcity narrative and maintains valuation arguments. Can exit to other assets but retain holdings specifically for the store-of-value thesis.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_holders, beneficiary,
    moderate, generational, arbitrage, global).

% Run full nodes that validate transactions and enforce consensus rules. The 1MB block limit keeps node operation computationally lightweight, enabling broad geographic distribution and defending decentralization. Node operators collectively govern through consensus rule enforcement and resist changes that would increase validation burden (large blocks). They benefit from the prestige and governance voice of validation.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, node_operators, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__store_of_value_reading, node_operators, agenda_setter).

% Users attempting small transactions (micropayments, remittances, frequent payments) face rising fees as block space becomes scarce. The 1MB limit creates artificial scarcity in transaction capacity, pricing them off the base layer. They are directed toward Lightning Network (off-chain) or alternative chains but retain no meaningful option to change the base-layer constraint itself.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, low_value_transaction_users, payer,
    powerless, immediate, constrained, global).

% Merchants and payment processors requiring immediate on-chain settlement face block-space scarcity and variable fees. The store-of-value reading deprioritizes their use case in favor of decentralization; they must either absorb volatility in fee markets, adopt off-chain solutions, or migrate to competing systems with higher transaction throughput.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, merchants_requiring_immediate_settlement, payer,
    moderate, biographical, mobile, regional).

% Maintain the Bitcoin protocol and enforce consensus rules through implementation. In the store-of-value reading, core developers are gatekeepers of the 1MB block limit and defenders of decentralization doctrine. They operate with significant technical authority but face community contention over capacity and fee levels.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, core_developers, agenda_setter,
    institutional, biographical, mobile, global).

% Operate mining pools that select transactions for inclusion in blocks. In the store-of-value reading, they enforce the capacity constraint and compete for fees in a restricted block space. They benefit from fee scarcity (fee-rate competition) and defend block-size limits as economically rational, but can also fork or exit to alternative consensus rules if base-layer demand shifts.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, mining_pool_operators, agenda_setter,
    powerful, biographical, arbitrage, global).

% Community members and developers who read Bitcoin's founding problem as enabling peer-to-peer electronic cash for everyday use. The store-of-value reading structurally forecloses their interpretation by subordinating on-chain capacity to decentralization. They are excluded from setting protocol direction and have no mechanism to override the 1MB limit without forking the network.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, electronic_cash_advocates, excluded,
    organized, biographical, trapped, global).

% Developers of Layer 2 scaling solutions (Lightning Network, sidechains, state channels) benefit from on-chain capacity constraints that force demand upward into their products. The store-of-value reading's retained 1MB limit sustains their market and use case. They participate in governance as observers but benefit from the constraint persisting.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, payment_network_developers, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__store_of_value_reading, payment_network_developers, observer).

% Monitor Bitcoin as a financial system and potential monetary asset. They observe the constraint structure from outside, noting that reduced on-chain transaction throughput decreases regulatory visibility into low-value payment flows while concentrating visibility on high-value transfers and node operation.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, regulatory_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper_purpose__store_of_value_reading, mining_pool_operators).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper_purpose__store_of_value_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a globally distributed ledger where any participant can validate the full transaction history without trusting a central authority. Decentralization—enabled by low validation burden (1MB blocks)—is the coordination problem being solved: how to achieve consensus on monetary state without centralizing trust.
% TRANSFER_FUNCTION: Moves transaction fee revenue from low-value users and payment processors to miners, and allocates network security budget (block subsidy + fees) to mining pool operators. The 1MB capacity limit enforces scarcity in block space, raising fees for all transactions and effectively transferring users below a fee threshold to off-chain systems.
% ABSENT_VOICES: Users and merchants who require low-cost on-chain transactions are excluded from governance mechanisms. They can fork the protocol but cannot change the base-layer rules without defecting. Electronic cash advocates and payment protocol designers working outside the store-of-value framework are also structurally excluded from steering capacity decisions.
% DISAPPEARANCE_RATIONALE: If the store-of-value reading and its 1MB block-size constraint disappeared overnight (replaced by higher capacity), on-chain transaction throughput would rise dramatically, fee markets would compress, low-value users would return to the base layer, the justification for Layer 2 networks would shift, and the scarcity narrative supporting valuation would erode. The entire ecosystem's economic configuration would reorganize.
% FOUNDING_PROBLEM: Satoshi Nakamoto's Bitcoin whitepaper presented a system for peer-to-peer electronic cash without trusted intermediaries. The founding problem is interpreted in the store-of-value reading as: how can decentralized consensus be achieved and maintained such that full-node validators remain accessible to ordinary participants?
% FOUNDING_PROBLEM_CORROBORATION: Long-term holders and node operators attest the founding problem remains live and requires the 1MB limit to preserve decentralization. Electronic cash advocates and payment processors attest the founding problem has shifted to encompassing transactional throughput. Independent empirical analysis (blockchain research, network simulation studies) supports the hypothesis that larger blocks increase validation computational burden and node concentration risk, though contested on the magnitude and remedial options. Satoshi Nakamoto's 2011 disappearance left no authoritative voice to resolve which reading the original author intended.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__store_of_value_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__store_of_value_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__store_of_value_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__store_of_value_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper_purpose__store_of_value_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__store_of_value_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_whitepaper_purpose__store_of_value_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.68 (end of interval) because the 1MB limit produces artificial scarcity in block space, raising transaction fees systematically and pricing users below a fee threshold off the base layer. This is extraction: a coordination mechanism (consensus ledger) is coupled with a capacity restriction that transfers users to secondary markets. Suppression is 0.71 because the constraint is enforced through consensus-rule validation (node operators reject blocks exceeding 1MB) and social enforcement (developer gatekeeping, mining pool coordination). The measurement series shows extraction rising from 0.42 to 0.68 over the interval as Bitcoin adoption grew and block-space became genuinely scarce, and theater rising from 0.25 to 0.42 as the decentralization justification became increasingly performative relative to the actual fee-extraction mechanism. Theater plateaus at 0.42 by interval end because the store-of-value narrative has stabilized as institutional orthodoxy. All metrics share one time grid; no metric is measured at different time points than others.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (node operators, core developers, mining pools) compute the constraint as essential coordination—decentralization cannot be maintained without the 1MB limit, and this is a genuine collective-action problem. From the victim seats (low-value users, merchants), the same structure computes as extractive gatekeeping: the capacity limit exists not to solve a coordination problem but to enforce scarcity and transfer fees. The payer seats (merchants) may also compute it as snare if they believe the decentralization justification is post-hoc cover. The engine computes these divergences from the structural data—beneficiary/victim positions, power asymmetry, exit options. The authored claim (tangled_rope) reflects the structure as seen from an analytical seat: genuine coordination coupled with asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Long-term holders have low directionality (d near beneficiary end, ~0.15-0.25) because they benefit from the scarcity constraint and bear minimal fee burden—their exit is high (arbitrage: can hold any asset, but retain Bitcoin for the specific store-of-value thesis). Node operators sit higher (d ~0.35-0.45) because they govern through validation but their exit is constrained—running a node is their primary commitment, and large-block forks would fork their validation role. Low-value transaction users have high directionality (d near target end, ~0.75-0.85) because they are systematically priced off the base layer with no governance voice (powerless, immediate time horizon, trapped exit—they cannot change the rules and must absorb the fee or leave). Merchants have moderate-high directionality (d ~0.60-0.70) because they bear costs but retain exit options (arbitrage to alternative systems, adoption of Layer 2). Mining pool operators have moderate directionality (d ~0.50-0.60) despite institutional power because their benefit is conditional on the fee market persisting—if the constraint dissolved (large blocks, lower fees), their strategic position would shift. Core developers occupy an analytically complex seat: they have high power and formal gatekeeping authority, but their benefit is ideological (defending the store-of-value reading) rather than material, which partially offsets directionality toward the beneficiary end.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits a nascent mandatrophy signature: the founding problem (how to achieve decentralized consensus) was solved by ~2012—the network demonstrably achieved consensus without centralization. The store-of-value reading's mandate is now to *preserve* that achievement, not to solve an outstanding technical problem. The 1MB block limit was originally a temporary anti-spam measure; its continued defense is now justified ex-post through the store-of-value thesis rather than ex-ante through technical necessity. The rising theater_ratio (0.25 to 0.42) is diagnostic of this shift: early enforcement (t=0) was coupled to actual spam risk; contemporary enforcement is increasingly defensive against alternative readings (electronic cash, capacity expansion) rather than against genuine technical problems. The constraint does not yet meet full piton criteria (no party would meaningfully profit from removing it; the alternative is reorganization, not removal) but shows the early mandatrophy trajectory: a coordination function that has succeeded, coupled with an extraction mechanism that persists by institutional inertia and ideological commitment rather than continuous problem-solving.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decentralization_measurement_ambiguity,
    'What constitutes ''decentralization'' precisely, and is a 1MB block limit the only mechanism to achieve it?',
    'Network simulations varying block size and examining node geographic distribution and computational barriers to participation. Empirical studies of full-node operator demographics and hardware requirements across different block sizes.',
    'If larger blocks (e.g., 2MB or adaptive) could maintain decentralization below a measured threshold, the 1MB limit loses its justifying binding constraint. If decentralization requires sub-megabyte blocks, the store-of-value reading''s core axiom holds. The classification could remain tangled_rope but the extraction magnitude would shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralization_measurement_ambiguity, empirical, 'Whether the 1MB limit is the minimal-sufficient condition for decentralization or an arbitrary upper bound.').

omega_variable(
    founding_problem_interpretation,
    'What did Satoshi Nakamoto intend as the primary problem Bitcoin solves: decentralized consensus, or everyday electronic cash transactions?',
    'Textual analysis of the whitepaper (title, abstract, introduction, conclusion) and Nakamoto''s forum posts and emails (if any additional artifacts emerge). Linguistic comparison with Nakamoto''s description of motivation across sources.',
    'If the primary intent was electronic cash throughput, the electronic_cash_reading gains legitimacy and the store-of-value reading becomes a post-hoc reinterpretation—the constraint''s mandate would be challenged as not foundational. If decentralization was primary, the store-of-value reading''s authority strengthens. This is a hermeneutic resolution, not a metric change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_interpretation, conceptual, 'The kernel interpretation ambiguity: which reading corresponds to the author''s foundational intent?').

omega_variable(
    scarcity_narrative_sustainability,
    'Is the ''digital scarcity'' narrative that justifies store-of-value positioning endogenous to Bitcoin''s technical design or dependent on external belief adoption?',
    'If the scarcity narrative is purely endogenous (e.g., mathematically guaranteed supply cap independent of adoption), it requires no active enforcement. If it is exogenous (e.g., value depends on consensus that the 1MB limit must be preserved), the narrative becomes a self-fulfilling prophecy vulnerable to defection.',
    'If exogenous, the constraint''s extraction mechanism is partially dependent on theater—maintaining belief that the limit is binding. Rising theater_ratio (0.25 to 0.42) would indicate increasing narrative-dependence. If the narrative collapses (e.g., alternative forks prove viable), the store-of-value reading''s legitimacy would erode and reclassification toward snare would follow.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scarcity_narrative_sustainability, conceptual, 'Whether store-of-value scarcity is a technical fact or a maintained narrative.').

omega_variable(
    layer_two_displacement_victims,
    'Do users displaced from on-chain transactions by the 1MB limit achieve acceptable service on Layer 2 (Lightning Network, sidechains) or are they genuinely victimized by reduced on-chain access?',
    'Measurement of Lightning Network adoption, fee structures, failure rates, and user satisfaction. Comparison of transaction costs and settlement speed on Layer 2 vs. hypothetical higher-capacity base layer.',
    'If Layer 2 solutions are cost-competitive and reliable, the constraint''s extraction severity may be lower than authored—victims are inconvenienced but not locked out. If Layer 2 adoption rates remain low and fees remain high, victimization is confirmed and the constraint''s classification as tangled_rope (vs. snare) depends on whether the coordination benefit (decentralization) is perceived as offsetting the extraction cost.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(layer_two_displacement_victims, empirical, 'Whether off-chain scaling genuinely substitutes for base-layer capacity or perpetuates victim status.').

omega_variable(
    reading_kernel_oracle_gap,
    'In the absence of Satoshi Nakamoto''s authorial voice since 2011, is the distinction between readings a genuine hermeneutic contest over the text, or a community power struggle using the text as rhetorical cover?',
    'Discourse analysis of Bitcoin community debate: are arguments grounded in textual interpretation, or do they reduce to power dynamics (miners vs. developers vs. users, nodes vs. pools, hodlers vs. merchants)?',
    'If hermeneutic contest: the readings are incommensurable but legitimate, and classification is frame-dependent (the analytical seat computes tangled_rope; each reading''s seat computes from its own values). If power struggle: the store-of-value reading may be a legitimacy claim masking extraction, shifting classification toward snare. The omega does not resolve this; it flags that the classification itself is contingent on whether the kernel has genuine interpretive authority or is post-hoc rationalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_oracle_gap, preference, 'Whether the reading contest is hermeneutic or political.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__store_of_value_reading, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(bitc_tr_t0, observed).
narrative_ontology:measurement(bitc_tr_t2, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2, 0.28).
narrative_ontology:measurement_basis(bitc_tr_t2, observed).
narrative_ontology:measurement(bitc_tr_t4, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 4, 0.32).
narrative_ontology:measurement_basis(bitc_tr_t4, observed).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 6, 0.37).
narrative_ontology:measurement_basis(bitc_tr_t6, observed).
narrative_ontology:measurement(bitc_tr_t10, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 10, 0.41).
narrative_ontology:measurement_basis(bitc_tr_t10, observed).
narrative_ontology:measurement(bitc_tr_t14, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 14, 0.42).
narrative_ontology:measurement_basis(bitc_tr_t14, observed).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(bitc_be_t0, observed).
narrative_ontology:measurement(bitc_be_t2, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2, 0.48).
narrative_ontology:measurement_basis(bitc_be_t2, observed).
narrative_ontology:measurement(bitc_be_t4, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 4, 0.55).
narrative_ontology:measurement_basis(bitc_be_t4, observed).
narrative_ontology:measurement(bitc_be_t6, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 6, 0.62).
narrative_ontology:measurement_basis(bitc_be_t6, observed).
narrative_ontology:measurement(bitc_be_t10, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement_basis(bitc_be_t10, observed).
narrative_ontology:measurement(bitc_be_t14, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 14, 0.68).
narrative_ontology:measurement_basis(bitc_be_t14, observed).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(bitc_su_t0, observed).
narrative_ontology:measurement(bitc_su_t2, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2, 0.62).
narrative_ontology:measurement_basis(bitc_su_t2, observed).
narrative_ontology:measurement(bitc_su_t4, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 4, 0.65).
narrative_ontology:measurement_basis(bitc_su_t4, observed).
narrative_ontology:measurement(bitc_su_t6, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 6, 0.69).
narrative_ontology:measurement_basis(bitc_su_t6, observed).
narrative_ontology:measurement(bitc_su_t10, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 10, 0.71).
narrative_ontology:measurement_basis(bitc_su_t10, observed).
narrative_ontology:measurement(bitc_su_t14, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 14, 0.71).
narrative_ontology:measurement_basis(bitc_su_t14, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__store_of_value_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper_purpose__store_of_value_reading, 0.18).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose__electronic_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity).

% DUAL FORMULATION NOTE:
% The bitcoin_whitepaper_purpose is a contested kernel with multiple readings. This constraint (store_of_value_reading) instantiates one coherent interpretation: decentralization and full-node verifiability are binding constraints, subordinating on-chain capacity. The sibling electronic_cash_reading prioritizes transactional throughput as binding. Both readings draw authority from the same whitepaper; neither logically forecloses the other (they coexist as competing frameworks held by different factions). The nakamoto_oracle_opacity reading notes that Satoshi Nakamoto's 2011 disappearance left no authoritative arbiter between these readings. The three constraints form a constraint family: store_of_value_reading influences the others by establishing capacity scarcity and forcing merchants toward Layer 2, which affects the feasibility and legitimacy of the electronic_cash_reading. All three inherit their ε-ambiguity from the kernel interpretation problem.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
