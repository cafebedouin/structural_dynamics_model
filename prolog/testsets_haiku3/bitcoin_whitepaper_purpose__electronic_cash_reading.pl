% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper_purpose__electronic_cash_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_whitepaper_purpose__electronic_cash_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bitcoin_whitepaper_purpose__electronic_cash_reading
 *   human_readable: Bitcoin Whitepaper Electronic Cash Telos: Low-Fee Daily Transaction Requirement
 *   domain: monetary/distributed_systems/governance
 *
 * SUMMARY:
 *   Bitcoin's whitepaper (2008) is titled 'Bitcoin: A Peer-to-Peer Electronic
 *   Cash System' and explicitly states the design goal is 'everyday
 *   transactional use' with 'low fees.' The electronic-cash reading treats
 *   this title and purpose statement as binding authority, requiring the
 *   protocol to expand on-chain transaction capacity (larger blocks, higher
 *   throughput) to keep per-transaction fees low enough for micropayments.
 *   This reading benefits payment processors, merchants, and low-income
 *   transactors; it imposes costs on node operators who must scale storage
 *   and bandwidth. The sibling store-of-value reading treats decentralization
 *   and full-node verifiability as the binding constraints, willing to accept
 *   high fees and second-layer solutions to preserve the decentralized
 *   property. These readings coexist as live positions in different
 *   governance coalitions. Satoshi Nakamoto's disappearance in 2011
 *   eliminated the authoritative interpreter, leaving the whitepaper text as
 *   contested kernel. This constraint story instantiates ONLY the
 *   electronic-cash reading; the store-of-value reading is a separate
 *   constraint (different ε, different beneficiary/victim structure,
 *   different type).
 *
 * KEY AGENTS:
 *   - payment_processors: benefit from capacity expansion and lower fees; organized power, mobile exit
 *   - low_value_transactors: depend on micropayment viability; powerless, constrained exit
 *   - node_operators: bear storage/bandwidth costs of capacity expansion; moderate power, mobile exit
 *   - protocol_governance_coalitions: set protocol parameters via hard-fork consensus; organized power, mobile exit
 *   - store_of_value_advocates: excluded from the conversation when electronic-cash reading dominates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.62).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.71).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__electronic_cash_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__electronic_cash_reading, "Bitcoin Whitepaper Electronic Cash Telos: Low-Fee Daily Transaction Requirement").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__electronic_cash_reading, "monetary/distributed_systems/governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__electronic_cash_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__electronic_cash_reading, 'da76fe6c-bce3-4ab2-9140-05c2e7557264').
narrative_ontology:cs_kernel_codification('da76fe6c-bce3-4ab2-9140-05c2e7557264', fixed_text).
narrative_ontology:cs_authority_grounding('da76fe6c-bce3-4ab2-9140-05c2e7557264', lineage).
narrative_ontology:cs_interpretation_layer_present('da76fe6c-bce3-4ab2-9140-05c2e7557264').
narrative_ontology:cs_reading_relation('da76fe6c-bce3-4ab2-9140-05c2e7557264', bitcoin_whitepaper_purpose__store_of_value_reading, coexists_with).
narrative_ontology:cs_axiom('da76fe6c-bce3-4ab2-9140-05c2e7557264', foundational, whitepaper_title_cash_telos_binding).
narrative_ontology:cs_axiom_status(whitepaper_title_cash_telos_binding, holdable).
narrative_ontology:cs_axiom_grounding('da76fe6c-bce3-4ab2-9140-05c2e7557264', whitepaper_title_cash_telos_binding, deontological).
narrative_ontology:cs_axiom('da76fe6c-bce3-4ab2-9140-05c2e7557264', secondary, low_fees_enable_micropayments).
narrative_ontology:cs_axiom_status(low_fees_enable_micropayments, holdable).
narrative_ontology:cs_axiom_grounding('da76fe6c-bce3-4ab2-9140-05c2e7557264', low_fees_enable_micropayments, empirically_contingent).
narrative_ontology:cs_reference_frame('da76fe6c-bce3-4ab2-9140-05c2e7557264', everyday_cash_system).
narrative_ontology:cs_drift_state('da76fe6c-bce3-4ab2-9140-05c2e7557264', contemporary_2025, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('da76fe6c-bce3-4ab2-9140-05c2e7557264', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, payment_processors).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, high_frequency_merchants).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, low_value_transactors).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__electronic_cash_reading, node_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__electronic_cash_reading, full_archive_maintainers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Extract transaction fees and settlement volume by operating full nodes and coordinating payment routing. Benefit from expanded on-chain capacity and lower per-transaction fees that make Bitcoin economically competitive with traditional payment networks. Their throughput target (transactions-per-second) rises with on-chain capacity expansion.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, payment_processors, beneficiary,
    organized, biographical, mobile, global).

% Operate retail/e-commerce accepting Bitcoin as payment. Benefit from low per-transaction fees and fast settlement that make Bitcoin viable as a daily payment instrument rather than a store of value. Can exit to legacy payment networks if Bitcoin fees rise above their margin tolerance.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, high_frequency_merchants, beneficiary,
    powerful, biographical, arbitrage, global).

% Users in jurisdictions with restricted banking access sending small remittances or making everyday purchases. Depend on Bitcoin's stated purpose as everyday cash to make micropayments economically viable. Cannot absorb high per-transaction fees; their use case disappears if fees rise above transaction value.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, low_value_transactors, beneficiary,
    powerless, immediate, constrained, regional).

% Run full archive nodes for the Bitcoin network. Bear escalating storage and bandwidth costs as on-chain transaction volume expands. The electronic-cash reading pushes capacity beyond what many volunteer node operators can sustain; their exit is running a pruned node (losing full-validation capability) or shutting down.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, node_operators, payer,
    moderate, biographical, mobile, global).

% Large mining pools and institutional nodes committed to maintaining full blockchain history for regulatory or operational compliance. Face mandatory infrastructure scaling (storage, bandwidth, compute) with no ability to downgrade; trapped in this role by the reading's capacity-expansion logic.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, full_archive_maintainers, payer,
    organized, generational, trapped, global).

% The whitepaper author, disappeared since 2011. The electronic-cash reading asserts the whitepaper text and title as binding authority, in Satoshi's documented intent. Satoshi cannot enforce or defend this reading; the reading's enforcement depends on later participants who invoke the whitepaper as authority.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, satoshi_nakamoto, agenda_setter,
    analytical, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(bitcoin_whitepaper_purpose__electronic_cash_reading, satoshi_nakamoto).

% Mining pools, exchange operators, wallet developers, and node operator coalitions who collectively decide whether to adopt block-size increase hard-forks or other capacity-expansion changes. In the electronic-cash reading, these coalitions enforce the requirement to expand on-chain capacity; their consensus and coordination are what makes the reading operational rather than textual.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, protocol_governance_coalitions, agenda_setter,
    organized, generational, mobile, global).

% Holders, miners, and developers prioritizing decentralization and full-node verifiability as binding constraints (the sibling store-of-value reading). Would argue for small block sizes, strict fee discipline, and second-layer solutions. Systematically out of the conversation when the electronic-cash reading dominates governance.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, store_of_value_advocates, excluded,
    powerful, generational, mobile, global).

% Regulators in major jurisdictions observe Bitcoin's actual transaction pattern and fee structure to determine whether it is functionally a currency (subject to money transmission rules) or a commodity (subject to different regulation). The electronic-cash reading's stated purpose affects their classification; actual low fees bolster the currency reading.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, cryptocurrency_regulations_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper_purpose__electronic_cash_reading, payment_processors).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper_purpose__electronic_cash_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a decentralized payment settlement system where participants collectively maintain transaction history and validate new transactions, solving the double-spend problem without a trusted central authority. The electronic-cash reading specifically prioritizes low per-transaction overhead so everyday payments are feasible.
% TRANSFER_FUNCTION: Moves transaction fees from transactors (users sending payments) to miners (who collect block rewards and fees). Under the electronic-cash reading, fees are minimized to expand transactor reach; under the store-of-value reading, fees are allowed to rise as on-chain capacity scarcity increases. The reading determines the fee-settlement relationship.
% ABSENT_VOICES: Store-of-value advocates and decentralization-maximalists are structurally excluded from governance when the electronic-cash reading dominates; they would argue for small blocks and high fees to preserve full-node accessibility. Regulatory authorities observe from outside but do not control the protocol.
% DISAPPEARANCE_RATIONALE: If the electronic-cash telos constraint vanished (i.e., if the reading were formally abandoned and the store-of-value reading adopted), the protocol would shift: block sizes would remain capped, fees would rise with adoption, merchant adoption would slow or reverse, and micropayment use cases would migrate to second-layer solutions (Lightning Network). The world rearranges because the constraint binds actual protocol parameters and adoption incentives.
% FOUNDING_PROBLEM: Early digital currency systems required trusted intermediaries; the whitepaper solved peer-to-peer electronic cash without a central authority, making low-overhead everyday payments technically possible for the first time.
% FOUNDING_PROBLEM_CORROBORATION: The electronic-cash reading asserts the founding problem is still live: unbanked populations and merchants in restricted jurisdictions still need low-fee payment rails. The store-of-value reading attests the founding problem is solved (Bitcoin is now established; decentralization and verification are the new constraints). Nakamoto's 2011 disappearance eliminated the originating authority's voice; later corroboration comes from payment processor operators (supporting electronic cash) and node operator testimony (supporting store of value as the binding constraint post-founding).
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__electronic_cash_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__electronic_cash_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__electronic_cash_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__electronic_cash_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper_purpose__electronic_cash_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__electronic_cash_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_whitepaper_purpose__electronic_cash_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts low (0.15 in 2009 when Bitcoin was experimental) and rises to 0.62 by 2025 as the protocol's actual operation diverges from the stated everyday-cash goal. The constraint is extractive because it imposes storage/bandwidth costs on node operators without their consent (they must validate the larger blocks or exit); it is not pure extraction because a genuine coordination function (low-fee payments) exists and genuinely benefits some participants. Suppression rises from 0.22 to 0.71 because enforcing capacity expansion requires overriding store-of-value advocates' preference for small blocks — this enforcement is structural (hard-fork consensus rules) not coercive, but it does suppress the alternative reading's adoption path. Theater rises from 0.08 to 0.48 because the constraint's justification increasingly relies on the whitepaper's textual authority (the 'cash' telos) rather than demonstrable economic viability for everyday payments; actual transaction patterns show Bitcoin remains a store of value, not everyday cash, yet the reading's enforcement machinery continues as if the founding problem persists. The measurement series on a single time grid tracks this divergence across the protocol's operational history. Accessibility collapse is low (0.42) because the alternative — full-node verifiability and small blocks — remains coherent and adopted by significant coalitions; the constraint does not collapse alternatives, it suppresses them via governance competition.
 *
 * PERSPECTIVAL GAP:
 *   The electronic-cash reading and the store-of-value reading compute to different types from the SAME protocol constraint, depending on which reading's assertions drive the classification. From the payment-processor and low-transactor seats, the constraint is rope or tangled rope (coordination with manageable extraction). From the node-operator and store-of-value-advocate seats, the constraint is snare (extraction of node resources justified by a purpose that is not operationally real). The engine computes per-seat classifications from power + exit + beneficiary/victim; the perceptual gap emerges because the two readings place different agents in the beneficiary and victim roles. The protocol constraint is the same; the reading determines which seat experiences it as coordination vs. extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Payment processors (organized, mobile exit) are beneficiaries — they collect transaction fees and routing volume under the expanded-capacity reading; d near 0.1. Low-value transactors (powerless, constrained exit) are nominal beneficiaries but face structural trapping in the micropayment use case; d near 0.25 (subsidy from the reading, but identity-locked to the payment channel). Node operators (moderate power, mobile exit) are victims — they bear mandatory scaling costs; d near 0.75 (high target). Store-of-value advocates (powerful, mobile exit) are excluded rather than victimized; their exit is adopting the sibling reading or migrating to other protocols, so they are out of the beneficiary/victim structure entirely. Protocol governance coalitions function as agenda-setters; their d is not computed on the extraction axis but on the agenda-setting / enforcement axis. The store-of-value reading would flip many d values: node operators would move to beneficiary (their decentralization preference is honored), low-value transactors would become victims (high fees lock them out).
 *
 * MANDATROPHY ANALYSIS:
 *   The electronic-cash reading's mandate is 'low-fee everyday transaction support.' The mandatrophy question is whether Bitcoin has become a constraint that persists past its founding problem. Empirically: Bitcoin's actual use pattern is store-of-value (hodling, long-term holdings, volatility-driven speculation); everyday transaction volume is marginal relative to transaction capacity. The founding problem (need for everyday cash) remains contested: advocates say unbanked populations and restricted jurisdictions still need it; store-of-value advocates say this was solved and decentralization is now the binding constraint. The theater_ratio rising from 0.08 to 0.48 indicates the constraint's enforcement machinery is increasingly performative: the whitepaper's 'cash' title is invoked as justification, but actual governance decisions (block-size debates, scaling disputes) are resolved by power (mining hash rate, developer consensus) not by reference to the founding problem's solution. A mandatrophy resolution would occur if the protocol formally abandoned the electronic-cash reading and adopted the store-of-value reading, or if Bitcoin's transaction pattern shifted decisively toward everyday payments. Neither has occurred as of 2025.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    satoshi_intent_binding,
    'Is Satoshi Nakamoto''s documented authorial intent (the whitepaper title and stated purpose) binding on the protocol, or is the protocol''s binding authority established by its actual participants'' consensus?',
    'Examine governance history: did protocol decisions explicitly invoke Satoshi''s intent as a deciding factor, or did consensus emerge from current stakeholders'' interests independent of the founder''s documented goals?',
    'If Satoshi''s intent is binding, the electronic-cash reading is a constraint the protocol MUST honor. If binding authority is current-participant consensus, the reading''s legitimacy depends on active agreement, and the constraint is only as stable as the coalition maintaining it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(satoshi_intent_binding, conceptual, 'Whether the whitepaper''s authority is textual and fixed or emergent from participant consensus.').

omega_variable(
    everyday_cash_viability,
    'Is everyday Bitcoin payment use economically viable under the electronic-cash reading''s low-fee requirement, or does the constraint''s enforcement machinery persist despite the founding problem being unresolved in practice?',
    'Compare actual transaction volumes and patterns (store-of-value vs. payment use) against the threshold where the reading''s stated purpose would be operationally real. Survey merchant adoption and user behavior in target jurisdictions (unbanked, restricted).',
    'If viable, the constraint is coordination with real beneficiaries. If not viable, the constraint is performative and the theater_ratio''s rise indicates degradation toward piton-hood despite active enforcement machinery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(everyday_cash_viability, empirical, 'Whether the stated purpose of everyday cash is achievable or has become theater.').

omega_variable(
    reading_coexistence_stability,
    'Can the electronic-cash reading and the store-of-value reading coexist indefinitely in the same protocol, or do they structurally foreclose one another under sustained disagreement about block-size and fee policy?',
    'Trace governance disputes over block-size hard-forks and protocol capacity decisions. Observe whether both readings can maintain active coalitions or whether one gradually captures protocol governance.',
    'If coexistence is stable, both constraints remain live and the protocol exhibits perceptual divergence (different seats see it as rope vs. snare). If one reading forecloses the other via governance capture, the protocol migrates to a single-reading constraint with unified type classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coexistence_stability, empirical, 'Whether two incompatible readings of the kernel can remain simultaneously operative.').

omega_variable(
    node_operator_suppression_mechanism,
    'Is the suppression of store-of-value advocates and small-block advocates structural (they lack the hash power / developer consensus to impose their reading) or internalized (they accept the electronic-cash reading as legitimate despite disagreeing)?',
    'Survey participant statements, fork attempts, and governance votes: do suppressed parties treat the electronic-cash reading as legitimate but suboptimal, or as illegitimate and imposed?',
    'If structural, the suppression persists as long as the coalition balance holds. If internalized, the reading has acquired legitimacy and would be harder to displace. The post-exit trajectory of participants who formally abandon Bitcoin for store-of-value alternatives would signal which mechanism dominates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(node_operator_suppression_mechanism, empirical, 'Whether suppression of the store-of-value reading is external power imbalance or internalized acceptance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__electronic_cash_reading, 2009, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t2009, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2009, 0.08).
narrative_ontology:measurement(bitc_tr_t2012, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2012, 0.18).
narrative_ontology:measurement(bitc_tr_t2015, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2015, 0.32).
narrative_ontology:measurement(bitc_tr_t2018, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2018, 0.42).
narrative_ontology:measurement(bitc_tr_t2021, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2021, 0.46).
narrative_ontology:measurement(bitc_tr_t2025, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2025, 0.48).

% Extraction over time
narrative_ontology:measurement(bitc_be_t2009, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2009, 0.15).
narrative_ontology:measurement(bitc_be_t2012, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2012, 0.28).
narrative_ontology:measurement(bitc_be_t2015, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2015, 0.48).
narrative_ontology:measurement(bitc_be_t2018, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2018, 0.58).
narrative_ontology:measurement(bitc_be_t2021, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2021, 0.61).
narrative_ontology:measurement(bitc_be_t2025, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t2009, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2009, 0.22).
narrative_ontology:measurement(bitc_su_t2012, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2012, 0.35).
narrative_ontology:measurement(bitc_su_t2015, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2015, 0.52).
narrative_ontology:measurement(bitc_su_t2018, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2018, 0.68).
narrative_ontology:measurement(bitc_su_t2021, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2021, 0.7).
narrative_ontology:measurement(bitc_su_t2025, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2025, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__electronic_cash_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.18).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_whitepaper_purpose__store_of_value_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the bitcoin_whitepaper_purpose kernel. The store-of-value reading (bitcoin_whitepaper_purpose__store_of_value_reading) treats decentralization and full-node verifiability as binding, yielding different beneficiaries (node operators, decentralization maximalists) and victims (merchants, micropayment users). The nakamoto_oracle_opacity constraint names the absence of authoritative interpretation as a structural fact. All three constraints share the same underlying Bitcoin protocol but attribute binding authority to different portions of the whitepaper and different interpretive traditions. The electronic-cash reading influences the store-of-value reading by setting on-chain capacity and fee parameters; the two readings coexist as live governance positions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
