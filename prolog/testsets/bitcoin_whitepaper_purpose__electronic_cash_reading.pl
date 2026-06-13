% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper_purpose__electronic_cash_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: bitcoin_whitepaper_purpose__electronic_cash_reading
 *   human_readable: Bitcoin Whitepaper Cash Telos Reading — Everyday Payment Function
 *   domain: distributed_systems/monetary_theory/technology_governance
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   'bitcoin_whitepaper_purpose.' It asserts that the whitepaper's title ('A
 *   Peer-to-Peer Electronic Cash System') and stated goal of enabling
 *   low-cost everyday transactions are BINDING on the protocol's design and
 *   governance. This reading mandates expanded on-chain capacity (8MB+
 *   blocks), lower transaction fees, and prioritization of merchant payment
 *   adoption. The sibling reading ('store_of_value_reading') contests this
 *   interpretation, claiming the true binding constraints are
 *   decentralization and individual full-node verifiability, to which
 *   on-chain capacity must be subordinated. The two readings coexist in the
 *   same discourse community but with fundamentally different cost
 *   distributions and beneficiary structures — they cannot both be fully
 *   instantiated in a single protocol simultaneously.
 *
 * KEY AGENTS:
 *   - Payment processors: organized beneficiaries, prioritize transaction throughput and merchant adoption
 *   - Low-value transactors: powerless beneficiaries, depend on sub-USD transaction economics
 *   - Full node operators: moderate-power payers, bear expanding storage and bandwidth costs
 *   - Network validators (miners): organized payers, face validation and propagation workload increases
 *   - Decentralization advocates: powerful payers, argue the cash telos undermines the actual binding constraint (decentralization)
 *   - Nakamoto oracle: absent analytical agenda setter, the whitepaper text is the kernel, disappearance created interpretive contest
 *   - Protocol developers: organized agenda setters, enforce the reading through code; split between cash and store-of-value factions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.68).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.55).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__electronic_cash_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__electronic_cash_reading, "Bitcoin Whitepaper Cash Telos Reading — Everyday Payment Function").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__electronic_cash_reading, "distributed_systems/monetary_theory/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__electronic_cash_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__electronic_cash_reading, '1913a331-2233-428b-9281-8b3e2ca1d730').
narrative_ontology:cs_kernel_codification('1913a331-2233-428b-9281-8b3e2ca1d730', fixed_text).
narrative_ontology:cs_authority_grounding('1913a331-2233-428b-9281-8b3e2ca1d730', lineage).
narrative_ontology:cs_reading_relation('1913a331-2233-428b-9281-8b3e2ca1d730', bitcoin_whitepaper_purpose__store_of_value_reading, coexists_with).
narrative_ontology:cs_axiom('1913a331-2233-428b-9281-8b3e2ca1d730', foundational, cash_functionality_is_binding_architectural_mandate).
narrative_ontology:cs_axiom_status(cash_functionality_is_binding_architectural_mandate, holdable).
narrative_ontology:cs_axiom_grounding('1913a331-2233-428b-9281-8b3e2ca1d730', cash_functionality_is_binding_architectural_mandate, instrumental).
narrative_ontology:cs_axiom('1913a331-2233-428b-9281-8b3e2ca1d730', foundational, low_fees_enable_everyday_transaction_volume).
narrative_ontology:cs_axiom_status(low_fees_enable_everyday_transaction_volume, holdable).
narrative_ontology:cs_axiom_grounding('1913a331-2233-428b-9281-8b3e2ca1d730', low_fees_enable_everyday_transaction_volume, empirically_contingent).
narrative_ontology:cs_reference_frame('1913a331-2233-428b-9281-8b3e2ca1d730', electronic_cash_design_posture).
narrative_ontology:cs_drift_state('1913a331-2233-428b-9281-8b3e2ca1d730', contemporary_post_2017_scaling_wars, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1913a331-2233-428b-9281-8b3e2ca1d730', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, payment_processors).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, low_value_transactors).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, merchant_adoption_advocates).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__electronic_cash_reading, full_node_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__electronic_cash_reading, network_validators).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__electronic_cash_reading, decentralization_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from expanded on-chain capacity enabling higher transaction throughput. Lower per-transaction fees reduce operational costs and improve margins. Prioritize merchant adoption and payment-rail use cases. Their business models depend on Bitcoin functioning as a settlement system for everyday commerce. Can exit to other blockchains (Litecoin, Doge, altcoins) or build on sidechains/payment channels if Bitcoin capacity is constrained.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, payment_processors, beneficiary,
    organized, biographical, mobile, global).

% Benefit from low transaction fees, which make small payments economically viable on-chain. Without fee reduction, payments under 1 USD become uneconomical. Depend on the system maintaining everyday usability rather than being optimized for high-value settlement only. Exit options are limited: can use layer-2 solutions (Lightning) but those are less decentralized and require more technical knowledge; can move to altcoins but fragmentation reduces network value.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, low_value_transactors, beneficiary,
    powerless, immediate, constrained, global).

% Drive narratives that Bitcoin should function as point-of-sale cash substitute. Believe widespread merchant acceptance depends on transaction speed and cost parity with legacy payment systems. Frame the whitepaper's 'electronic cash' title as a binding mandate. Control substantial capital and marketing resources. Can exit to alternate blockchains or payment systems but have invested in Bitcoin narrative and community relationships.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, merchant_adoption_advocates, beneficiary,
    powerful, generational, mobile, global).

% Bear increasing storage and bandwidth costs as block size expands. Running a full validating node becomes more expensive and technically demanding. They perceive expanded capacity as imposing costs they did not choose and receive no compensation for. Can migrate to other chains, stop running nodes entirely, or run pruned/light-client variants that reduce security guarantees. Their voice in protocol governance is weak compared to developers and merchants.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, full_node_operators, payer,
    moderate, biographical, mobile, global).

% Miners and staking operators face expanded validation and propagation workloads with larger blocks. While they benefit from transaction fees, the enforcement of higher capacity creates technical pressure (larger blocks require investment in infrastructure). Their ability to exit is constrained by the sunk capital in mining hardware or staking positions. However, they can collectively pressure protocol governance through their collective hashing power (via signaling in block headers) or by mining alternate chains.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, network_validators, payer,
    organized, generational, constrained, global).

% Argue that expanded capacity directly undermines the whitepaper's actual core claim: decentralization and individual verifiability. Large blocks raise the bar for running full nodes, reducing the number of independent validators and concentrating power in high-capacity operations. They see this as a betrayal of the whitepaper's true mandate. Control substantial technical credibility and can exit to alternate implementations, forks, or other chains that prioritize their reading of the constraints.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, decentralization_advocates, payer,
    powerful, generational, mobile, global).

% Enforce the constraint through code. Implement block size limits, fee structures, and capacity parameters. Different factions of developers are split: cash-reading advocates push for larger limits; store-of-value advocates maintain smaller limits. The constraint's persistence depends on which faction's code becomes canonical. Control the technical roadmap but are subject to pressure from mining coalitions (hashing power) and community discourse.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, protocol_developers, agenda_setter,
    organized, generational, mobile, global).

% Benefit from both readings but have agnostic technical positions. Lower transaction fees benefit their operational efficiency; higher capacity also benefits their operational efficiency. Their primary interest is network utility and price stability, not ideological commitment to either reading. Can operate on any blockchain variant or transition to alternate assets if Bitcoin governance becomes too contentious.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, exchanges_and_custody, observer,
    institutional, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper_purpose__electronic_cash_reading, payment_processors).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper_purpose__electronic_cash_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables peer-to-peer electronic cash transactions without trusted intermediaries, with low-cost settlement suitable for everyday commerce. Solves the coordination problem of how to route payment information, verify transaction validity, and update account balances without a centralized processor charging fees and extracting data.
% TRANSFER_FUNCTION: Moves computing and storage costs (node operation, validation, propagation) from payment processors and transactors onto the distributed network of full node operators and validators. Expanded block capacity increases the per-transaction cost borne by the network infrastructure, while transaction processors and merchants capture the benefit of higher throughput and lower per-transaction fees.
% ABSENT_VOICES: Satoshi Nakamoto (disappeared 2011) cannot arbitrate the reading contest. The store-of-value reading camp is present but systematically marginalized in discourse forums and development meetings dominated by cash-reading advocates and merchants. Medium-sized node operators historically had less organized voice until network growth and cost pressure organized them into resistance coalitions. Early Bitcoin users who see expanded capacity as abandoning the cypherpunk principles of individual sovereignty have largely exited to alternate communities or forks.
% DISAPPEARANCE_RATIONALE: Removal of the cash-mandate framing and enforcement (reverting to small-block, high-fee architecture) would cause merchants and payment processors to migrate to alternate blockchains or payment systems; low-value transactors would lose on-chain access and move to layer-2 solutions or fiat systems. However, the store-of-value reading camp would claim this is a RETURN to the true purpose, not a removal of function. The dispute itself makes the verdict contested: one faction sees disappearance as system failure, the other as restoration.
% FOUNDING_PROBLEM: The whitepaper identifies the need for 'a purely peer-to-peer electronic cash system' that allows 'small casual transactions' without requiring a trusted third party. The system must support everyday transaction use with low fees sufficient to make small payments economically viable.
% FOUNDING_PROBLEM_CORROBORATION: Cash-reading advocates cite the whitepaper title, abstract, and early statements by Nakamoto (2008) on efficiency and speed. Payment processors and merchant groups corroborate this reading based on their operational needs. However, the store-of-value reading camp (decentralization advocates, many developers) cites Nakamoto's later 2010 statements prioritizing node decentralization over transaction throughput ('long-term, it would be better if the network was designed so that each node did as little work as possible'). Academic analysis and peer-review from protocol designers outside the Bitcoin community finds the whitepaper text ambiguous on the relationship between cash functionality and decentralization — both are stated as goals, but their priority relationship is not specified. No external neutral arbiter has been designated or accepted.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__electronic_cash_reading, contested).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__electronic_cash_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__electronic_cash_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__electronic_cash_reading, 'none', 1).

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
 *   Extractiveness climbs from 0.35 to 0.68 over the interval because the cash-reading mandate shifts costs onto node operators while concentrating benefits (transaction throughput, low fees) in the hands of payment processors and merchants. Early in the interval (t=0–7), the reading is aspirational and extraction is lower because the protocol has capacity headroom and the cost redistribution is not yet acute. By t=15–20, expanded blocks are the operational reality, node costs are rising, and full nodes are becoming less common — extraction is visible and mounting. Theater ratio rises from 0.15 to 0.42: early defense of the cash telos is genuine advocacy for a use case (theater low); by t=20, continued assertions of the cash mandate serve to deflect criticism of rising node costs and to maintain protocol legitimacy despite the cost shift (theater higher). Suppression requirement (enforcement cost) rises from 0.25 to 0.55 because the store-of-value reading camp must be managed — protocol governance becomes contentious as the cost-benefit tradeoff becomes visible. The enforcement is ideological and technical (which code forks become canonical) rather than overt coercion, but the suppression of the dissenting reading is active. On the coercion grid: structural suppression of the store-of-value reading rises sharply (0.20 → 0.52) as the cash-reading faction consolidates protocol governance. Organizational resistance from decentralization advocates rises (0.68 → 0.75), showing active pushback. Individual-level accessibility collapse is high and stable (~0.70–0.72) — a small user cannot meaningfully run a node or join the protocol design conversation; their choice set is simply 'use the protocol as dictated or exit.' Individual-level resistance falls slightly (0.75 → 0.68) — individual actors gradually accept the protocol as defined rather than organizing resistance.
 *
 * PERSPECTIVAL GAP:
 *   The cash-reading beneficiaries (payment processors, merchant advocates) experience this as genuine coordination necessity: without expanded capacity, Bitcoin cannot fulfill its stated purpose. From their seat, the constraint is legitimate, even if costly to implement. The payer seats (full node operators, decentralization advocates) experience it as a false mandate imposed on the protocol by a faction that controls narrative and code merge authority. A payment processor and a full node operator see the same protocol rule — larger block size — and experience entirely opposite extraction profiles: one sees capacity enabling their business, the other sees cost rising without compensation. The engine computes per-seat type from the structural data; this reading is authored as tangled_rope (genuine coordination function + asymmetric extraction), and the payer seats should resolve as snare-adjacent or tangled_rope-with-burden while beneficiary seats resolve as rope or coordination. The divergence is structural, not a measurement error.
 *
 * DIRECTIONALITY LOGIC:
 *   Payment processors and low-value transactors are beneficiaries (d near 0.0–0.25) because the constraint expands capacity and reduces fees they depend on. Full node operators and network validators are payers (d near 0.75–1.0) because costs rise with block expansion and they receive no direct compensation. Decentralization advocates are payers (d near 0.7–0.9) because they bear the burden of their reading being suppressed and the protocol diverging from what they see as its true purpose. Protocol developers are the agenda-setter (power=organized, d in 0.3–0.5 range) — they enforce the reading through code, so they steer beneficiary direction, but they are not capturing the rents, only executing the coordination choice. Exchanges and custody are observers with low directionality (d near 0.5) because they benefit from both readings and are agnostic to the technical choice.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids the mandatrophy trap by virtue of its founding problem remaining contested. The cash-reading advocates maintain that the founding problem (everyday transaction use) is still live and the constraint still necessary. The store-of-value reading camp contests that claim, but the contest itself keeps both readings from atrophying — neither can assert the founding problem is dead without losing coherence. However, the coercion grid shows structural-level suppression rising (0.20 → 0.52), which is consistent with mandatrophy in progress: if the store-of-value reading were fully suppressed and the contest resolved in favor of the cash reading, the theatrical component would fall again as the constraint became normal operation, not a defended position. Currently the theater ratio is climbing (0.15 → 0.42), indicating increasing performative defense — a pre-mandatrophy signal. The constraint risks becoming a piton: the founding problem becomes increasingly contested, extraction is visible, but neither side can fully exit or fully win, so the constraint persists through organizational inertia and narrative defense rather than genuine coordination or even extractive clarity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    whitepaper_text_ambiguity,
    'Is the whitepaper''s emphasis on ''electronic cash'' and ''everyday transaction use'' a binding architectural mandate, or is it a use-case vision for a decentralization primitive that serves many purposes?',
    'Textual analysis of the whitepaper and Nakamoto''s 2008–2010 statements compared with technical design choices (proof-of-work, finite supply, no rollback mechanism). Analysis of sibling blockchain designs (Litecoin, Dogecoin, etc.) and their stated purposes relative to their technical capacity choices.',
    'If the cash mandate is binding, expanded capacity is necessary and node operator costs are justified as coordination expenses. If cash is a use case not a mandate, the constraint is extractive and its persistence is contestable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(whitepaper_text_ambiguity, conceptual, 'Whether the cash telos is architecturally binding or aspirational.').

omega_variable(
    node_cost_internalization,
    'Should full node operators bear the cost of expanded on-chain capacity as an implicit subsidy to transaction processors and merchants, or should capacity expansion be gated on compensation mechanisms for node operators?',
    'Empirical: measurement of node running costs over the interval (storage, bandwidth, CPU); analysis of whether any mechanism transfers revenue from beneficiaries to node operators proportional to cost increase. Structural: examination of whether protocol governance includes seats for node operators or whether it is weighted toward developer and merchant interests.',
    'If node costs are uncompensated, the constraint is extractive and arguably coercive (node operators have no choice to adopt expanded capacity without bearing the cost). If mechanisms exist or are implemented, the constraint shifts toward tangled_rope with higher coordination cost rather than pure extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(node_cost_internalization, empirical, 'Whether node operator costs are structurally internalized or externalized.').

omega_variable(
    reading_foreclosure_test,
    'Does the store-of-value reading logically foreclose the cash reading, or are they coexisting architectural choices held by different coalition members?',
    'Logical analysis: examine whether a protocol with strong decentralization (small blocks, high node count) can still serve cash use cases at scale (via payment channels, sidechains, or other layer-2 solutions). If yes, the readings coexist (different paths to fulfilling different priorities). If no, one reading forecloses the other.',
    'If coexistent: both readings remain live and the constraint is mandatrophy-vulnerable (neither can win, both must be performatively defended). If one forecloses the other: the winner''s reading legitimacy is strengthened and mandatrophy pressure relaxes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_test, conceptual, 'Whether the two readings are logically or merely strategically incompatible.').

omega_variable(
    satoshi_disappearance_oracle_silence,
    'Would explicit authorial interpretation by Nakamoto resolve the reading contest, or has the protocol governance evolved to the point where authorial intent is no longer dispositive?',
    'Hypothetical: if Nakamoto''s private writings were discovered clarifying the original intent, would the Bitcoin community accept that resolution? Evidence from other open-source projects with deceased founders (Linux kernel, etc.) on whether community governance overrides authorial intent.',
    'If authorial intent would be dispositive: the absence is the core structuring problem and the constraint will remain contested until one reading captures majority governance. If community governance is already normative: Nakamoto''s absence is less consequential than the power distribution among current developers and stakeholders.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(satoshi_disappearance_oracle_silence, preference, 'Whether the kernel''s interpretive authority lives in the author or in the community.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__electronic_cash_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(bitc_tr_t0, observed).
narrative_ontology:measurement(bitc_tr_t3, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 3, 0.22).
narrative_ontology:measurement_basis(bitc_tr_t3, observed).
narrative_ontology:measurement(bitc_tr_t7, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 7, 0.31).
narrative_ontology:measurement_basis(bitc_tr_t7, observed).
narrative_ontology:measurement(bitc_tr_t11, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 11, 0.38).
narrative_ontology:measurement_basis(bitc_tr_t11, observed).
narrative_ontology:measurement(bitc_tr_t15, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(bitc_tr_t15, observed).
narrative_ontology:measurement(bitc_tr_t20, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(bitc_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(bitc_be_t0, observed).
narrative_ontology:measurement(bitc_be_t3, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 3, 0.42).
narrative_ontology:measurement_basis(bitc_be_t3, observed).
narrative_ontology:measurement(bitc_be_t7, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 7, 0.55).
narrative_ontology:measurement_basis(bitc_be_t7, observed).
narrative_ontology:measurement(bitc_be_t11, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 11, 0.63).
narrative_ontology:measurement_basis(bitc_be_t11, observed).
narrative_ontology:measurement(bitc_be_t15, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 15, 0.67).
narrative_ontology:measurement_basis(bitc_be_t15, observed).
narrative_ontology:measurement(bitc_be_t20, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(bitc_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(bitc_su_t0, observed).
narrative_ontology:measurement(bitc_su_t3, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 3, 0.35).
narrative_ontology:measurement_basis(bitc_su_t3, observed).
narrative_ontology:measurement(bitc_su_t7, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 7, 0.45).
narrative_ontology:measurement_basis(bitc_su_t7, observed).
narrative_ontology:measurement(bitc_su_t11, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 11, 0.52).
narrative_ontology:measurement_basis(bitc_su_t11, observed).
narrative_ontology:measurement(bitc_su_t15, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 15, 0.54).
narrative_ontology:measurement_basis(bitc_su_t15, observed).
narrative_ontology:measurement(bitc_su_t20, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement_basis(bitc_su_t20, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=20
narrative_ontology:measurement(bitc_grid_01, bitcoin_whitepaper_purpose__electronic_cash_reading, accessibility_collapse(class), 0, 0.5).
narrative_ontology:measurement(bitc_grid_02, bitcoin_whitepaper_purpose__electronic_cash_reading, accessibility_collapse(class), 20, 0.65).
narrative_ontology:measurement(bitc_grid_03, bitcoin_whitepaper_purpose__electronic_cash_reading, accessibility_collapse(individual), 0, 0.7).
narrative_ontology:measurement(bitc_grid_04, bitcoin_whitepaper_purpose__electronic_cash_reading, accessibility_collapse(individual), 20, 0.72).
narrative_ontology:measurement(bitc_grid_05, bitcoin_whitepaper_purpose__electronic_cash_reading, accessibility_collapse(organizational), 0, 0.55).
narrative_ontology:measurement(bitc_grid_06, bitcoin_whitepaper_purpose__electronic_cash_reading, accessibility_collapse(organizational), 20, 0.68).
narrative_ontology:measurement(bitc_grid_07, bitcoin_whitepaper_purpose__electronic_cash_reading, accessibility_collapse(structural), 0, 0.45).
narrative_ontology:measurement(bitc_grid_08, bitcoin_whitepaper_purpose__electronic_cash_reading, accessibility_collapse(structural), 20, 0.62).
narrative_ontology:measurement(bitc_grid_09, bitcoin_whitepaper_purpose__electronic_cash_reading, resistance(class), 0, 0.72).
narrative_ontology:measurement(bitc_grid_10, bitcoin_whitepaper_purpose__electronic_cash_reading, resistance(class), 20, 0.7).
narrative_ontology:measurement(bitc_grid_11, bitcoin_whitepaper_purpose__electronic_cash_reading, resistance(individual), 0, 0.75).
narrative_ontology:measurement(bitc_grid_12, bitcoin_whitepaper_purpose__electronic_cash_reading, resistance(individual), 20, 0.68).
narrative_ontology:measurement(bitc_grid_13, bitcoin_whitepaper_purpose__electronic_cash_reading, resistance(organizational), 0, 0.68).
narrative_ontology:measurement(bitc_grid_14, bitcoin_whitepaper_purpose__electronic_cash_reading, resistance(organizational), 20, 0.75).
narrative_ontology:measurement(bitc_grid_15, bitcoin_whitepaper_purpose__electronic_cash_reading, resistance(structural), 0, 0.65).
narrative_ontology:measurement(bitc_grid_16, bitcoin_whitepaper_purpose__electronic_cash_reading, resistance(structural), 20, 0.72).
narrative_ontology:measurement(bitc_grid_17, bitcoin_whitepaper_purpose__electronic_cash_reading, stakes_inflation(class), 0, 0.15).
narrative_ontology:measurement(bitc_grid_18, bitcoin_whitepaper_purpose__electronic_cash_reading, stakes_inflation(class), 20, 0.38).
narrative_ontology:measurement(bitc_grid_19, bitcoin_whitepaper_purpose__electronic_cash_reading, stakes_inflation(individual), 0, 0.08).
narrative_ontology:measurement(bitc_grid_20, bitcoin_whitepaper_purpose__electronic_cash_reading, stakes_inflation(individual), 20, 0.22).
narrative_ontology:measurement(bitc_grid_21, bitcoin_whitepaper_purpose__electronic_cash_reading, stakes_inflation(organizational), 0, 0.3).
narrative_ontology:measurement(bitc_grid_22, bitcoin_whitepaper_purpose__electronic_cash_reading, stakes_inflation(organizational), 20, 0.58).
narrative_ontology:measurement(bitc_grid_23, bitcoin_whitepaper_purpose__electronic_cash_reading, stakes_inflation(structural), 0, 0.2).
narrative_ontology:measurement(bitc_grid_24, bitcoin_whitepaper_purpose__electronic_cash_reading, stakes_inflation(structural), 20, 0.45).
narrative_ontology:measurement(bitc_grid_25, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression(class), 0, 0.25).
narrative_ontology:measurement(bitc_grid_26, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression(class), 20, 0.55).
narrative_ontology:measurement(bitc_grid_27, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression(individual), 0, 0.3).
narrative_ontology:measurement(bitc_grid_28, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression(individual), 20, 0.48).
narrative_ontology:measurement(bitc_grid_29, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression(organizational), 0, 0.28).
narrative_ontology:measurement(bitc_grid_30, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression(organizational), 20, 0.6).
narrative_ontology:measurement(bitc_grid_31, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression(structural), 0, 0.2).
narrative_ontology:measurement(bitc_grid_32, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression(structural), 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__electronic_cash_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.18).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_whitepaper_purpose__store_of_value_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_mining_incentive_alignment).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_full_node_verifiability).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_transaction_fee_market).

% DUAL FORMULATION NOTE:
% This story and 'bitcoin_whitepaper_purpose__store_of_value_reading' are sibling readings of the same kernel (bitcoin_whitepaper_purpose). They have conflicting beneficiary/victim structures, different ε values, and incompatible architectural mandates. The electronic-cash reading prioritizes transaction throughput and merchant adoption; the store-of-value reading prioritizes decentralization and individual verifiability. Each story is ε-invariant within its own reading; the divergence between readings is captured in the cs_structure.reading_relations and the network edges. Do NOT attempt to merge these stories or reconcile their metrics — the divergence IS the measurement the corpus exists to take.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__electronic_cash_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
