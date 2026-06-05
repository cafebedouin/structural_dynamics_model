% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper_purpose__electronic_cash_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: bitcoin_whitepaper_purpose__electronic_cash_reading
 *   human_readable: Bitcoin Whitepaper Electronic Cash Purpose Constraint
 *   domain: distributed_systems/monetary_theory/technology_governance
 *
 * SUMMARY:
 *   The Bitcoin whitepaper's electronic-cash telos — 'a purely peer-to-peer
 *   version of electronic cash would allow online payments to be sent
 *   directly from one party to another without going through a financial
 *   institution' — instantiates a binding constraint on protocol development
 *   in one reading, yet this reading is actively contested by an alternative
 *   store-of-value reading. This constraint story models the electronic-cash
 *   reading as a clean, ε-invariant constraint: a system commitment to
 *   support everyday low-fee transactional use. The constraint exhibits all
 *   six classification types from different observer perspectives, making it
 *   a diagnostic exemplar for kernel-reading dynamics. The measurement
 *   trajectory shows rising extractiveness, suppression, and theater ratio
 *   over Bitcoin's first decade (2009-2019), indicating that the
 *   electronic-cash telos became increasingly costly to maintain as protocol
 *   politics evolved and transaction fees rose. The theater ratio's rise
 *   reflects Core developers maintaining ceremonial commitment to 'the
 *   whitepaper's true purpose' while enabling high-fee transaction patterns —
 *   classic piton drift. The extractiveness rise reflects growing asymmetry
 *   between beneficiaries (payment processors, high-throughput use cases) and
 *   victims (node operators bearing capacity costs). This reading does NOT
 *   attempt to resolve the kernel dispute between electronic-cash and
 *   store-of-value readings; it models the electronic-cash reading's own
 *   structural coherence and the cost dynamics of maintaining it.
 *
 * KEY AGENTS:
 *   - Payment Processors & Merchants: Primary beneficiaries (institutional/arbitrage) — benefit from low-fee settlement, support expanded on-chain capacity, promote electronic-cash reading
 *   - Low-Value Transactors: Secondary beneficiary (moderate/constrained) — need low-fee on-chain access for everyday payments but compete for block space
 *   - Full-Node Operators: Primary victims (powerless/trapped) — bear uncompensated storage and bandwidth costs of larger blocks; cannot exit without losing validation role
 *   - Node-Operator Coalition: Organized victim group (organized/constrained) — infrastructure providers that face structural cost asymmetry but can coordinate collective response
 *   - Core Developers: Authority custodians (institutional/arbitrage) — claim stewardship of original vision but arbitrage between readings in governance decisions; maintain high theater ratio
 *   - Store-of-Value Faction: Sibling-reading holders (powerful/mobile) — HODL-maximalists, high-fee-preference agents; hold competing reading of the same whitepaper kernel
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing governance choices (block size, fee policy) as immutable technical trilemmas
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.58).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.48).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__electronic_cash_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__electronic_cash_reading, "Bitcoin Whitepaper Electronic Cash Purpose Constraint").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__electronic_cash_reading, "distributed_systems/monetary_theory/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__electronic_cash_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__electronic_cash_reading, '8635dc4d-d375-4838-ac1c-b0266d4398fc').
narrative_ontology:cs_kernel_codification('8635dc4d-d375-4838-ac1c-b0266d4398fc', fixed_text).
narrative_ontology:cs_authority_grounding('8635dc4d-d375-4838-ac1c-b0266d4398fc', lineage).
narrative_ontology:cs_interpretation_layer_present('8635dc4d-d375-4838-ac1c-b0266d4398fc').
narrative_ontology:cs_reading_relation('8635dc4d-d375-4838-ac1c-b0266d4398fc', bitcoin_whitepaper_purpose__store_of_value_reading, coexists_with).
narrative_ontology:cs_axiom('8635dc4d-d375-4838-ac1c-b0266d4398fc', foundational, electronic_cash_primary_use_case).
narrative_ontology:cs_axiom_status(electronic_cash_primary_use_case, holdable).
narrative_ontology:cs_axiom_grounding('8635dc4d-d375-4838-ac1c-b0266d4398fc', electronic_cash_primary_use_case, empirically_contingent).
narrative_ontology:cs_axiom('8635dc4d-d375-4838-ac1c-b0266d4398fc', foundational, on_chain_scalability_sufficient).
narrative_ontology:cs_axiom_status(on_chain_scalability_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('8635dc4d-d375-4838-ac1c-b0266d4398fc', on_chain_scalability_sufficient, instrumental).
narrative_ontology:cs_reference_frame('8635dc4d-d375-4838-ac1c-b0266d4398fc', whitepaper_peer_to_peer_vision).
narrative_ontology:cs_drift_state('8635dc4d-d375-4838-ac1c-b0266d4398fc', contemporary_2024, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8635dc4d-d375-4838-ac1c-b0266d4398fc', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, payment_processors).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, low_value_transactors).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, merchant_adoption_infrastructure).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__electronic_cash_reading, node_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__electronic_cash_reading, full_node_maintainers).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__electronic_cash_reading, bandwidth_constrained_participants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PAYMENT PROCESSOR (ROPE) — Experiences the electronic-cash telos as pure coordination: low-fee, high-throughput transaction processing enables merchant adoption and payment-network growth. Benefits from expanded on-chain capacity and fee compression. Coordination function is genuine — processors need reliable, low-cost settlement to operate. No meaningful extraction experienced.
constraint_indexing:constraint_classification(bitcoin_whitepaper_purpose__electronic_cash_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 2: LOW-VALUE TRANSACTOR (TANGLED ROPE) — Benefits from low-fee on-chain inclusion; can transact everyday purchases directly. But also constrained by block space competition and timing uncertainty. Mixed experience: genuine coordination need (wants low fees) with extraction pressure (competes with high-value transactions for block space). As transaction volume rises, escalates toward snare.
constraint_indexing:constraint_classification(bitcoin_whitepaper_purpose__electronic_cash_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: FULL-NODE OPERATOR (SNARE) — Bears the storage and bandwidth cost of expanded blocks required by electronic-cash telos. Cannot exit: dropping from the network means losing validation capacity and stake in the consensus. Trapped between supporting the protocol and bearing uncompensated infrastructure cost. Maximum experienced extraction — the whitepaper's purpose mandate extracts from this agent without compensation or choice.
constraint_indexing:constraint_classification(bitcoin_whitepaper_purpose__electronic_cash_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 4: NODE-OPERATOR COALITION (TANGLED ROPE) — Organized agents (node infrastructure providers, validators, client developers) experience mixed constraints. The electronic-cash telos creates genuine coordination need (shared validation infrastructure) but enforces asymmetric costs: full nodes bear capacity burden while miners/processors capture fee benefits. Coalition can organize but faces structural disadvantage — the telos itself privileges transaction throughput over node-operator incentives.
constraint_indexing:constraint_classification(bitcoin_whitepaper_purpose__electronic_cash_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CORE DEVELOPER AUTHORITY (PITON) — Claims to steward the whitepaper's original vision but presides over degraded enforcement. The electronic-cash mandate is maintained through ceremonial citation (references to 'the whitepaper's true purpose') but operationalized through political negotiation, not original-intent implementation. Core devs arbitrage between competing readings — maintaining plausible commitment to electronic cash while enabling high-fee scaling. Piton: high theater ratio (continuous 'electronic cash is our mission' rhetoric) with declining functional enforcement (actual fee levels, block sizes deviate from original specifications).
constraint_indexing:constraint_classification(bitcoin_whitepaper_purpose__electronic_cash_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risk of classifying the tension between whitepaper purpose and implementation as an immutable trilemma (scalability, decentralization, security) — a natural law of distributed systems. However, the structural data reveals this as naturalization: the tension is enforced through governance choices (block size, fee policy, protocol rules) that are contingent institutional decisions, not physical constants. False summit: beneficiaries and victims are identifiable; the telos is disputed; the constraint is maintained through active political choice, not natural necessity.
constraint_indexing:constraint_classification(bitcoin_whitepaper_purpose__electronic_cash_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: MERCHANT INFRASTRUCTURE (ROPE) — Powerful actors (payment networks, large merchants, e-commerce platforms) benefit from low-fee settlement but have mobile exit options (can use Lightning, sidechains, or competing Layer 2 protocols). Experience the electronic-cash telos as coordination with exit optionality — they see the mandate as valuable but not binding. Pure rope from their perspective: genuine coordination need without extraction.
constraint_indexing:constraint_classification(bitcoin_whitepaper_purpose__electronic_cash_reading, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper_purpose__electronic_cash_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bitcoin_whitepaper_purpose__electronic_cash_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bitcoin_whitepaper_purpose__electronic_cash_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__electronic_cash_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bitcoin_whitepaper_purpose__electronic_cash_reading, TR),
    TR >= 0.70.

:- end_tests(bitcoin_whitepaper_purpose__electronic_cash_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising over the measurement interval. At t=0 (2009), the electronic-cash telos is operationally coherent and low-cost — blocks are small, fees are negligible, node-operator burden is minimal. At t=10 (2019), the telos remains formally operative but operationally expensive: transaction demand has exceeded protocol capacity, fees have risen, and maintaining the electronic-cash commitment would require protocol changes that conflict with store-of-value reading preferences. The extractiveness reflects the gap between the stated commitment and its operational cost — beneficiaries (payment processors) continue to receive the benefit of the rhetoric, but victims (node operators) bear rising actual costs. Suppression (0.48): Moderate. Node operators face real barriers to exit (loss of validation role, community standing, security contribution) but not total immobility. They can switch to light clients (but lose validation function) or participate in sidechains/Layer 2 (accepting that on-chain scaling isn't happening). The suppression reflects this partial constraint. Theater ratio (0.55): Moderate-high and rising. Core developers maintain ceremonial commitment to 'the whitepaper's original vision' in governance forums while enabling protocol evolution toward store-of-value reading (higher fees, smaller blocks relative to demand). The theater reflects the gap between stated telos and operational choice.
 *
 * PERSPECTIVAL GAP:
 *   The electronic-cash reading produces maximally divergent classifications across perspectives. Payment processors see pure coordination (Rope) — low-fee settlement directly solves their operational problem. Low-value transactors see mixed coordination and extraction (Tangled Rope) — they benefit from the telos but compete for scarce block space. Full-node operators see pure extraction (Snare) — the telos mandates their cost-bearing with no compensation. Core developers see a piton (theatrical obligation maintained through inertia) — the telos is cited ceremonially while protocol evolution proceeds through pragmatic governance. The store-of-value faction (not shown as a separate perspective in this story) would see the electronic-cash reading as imposing costs (block space constraints, lower transaction fees available to HODLers). The civilizational analytical observer risks naturalizing this political dispute as a technical law (the trilemma: scalability, decentralization, security). The perspectival gap reveals that the 'immutable law' framing is cover for a governance choice about whose interests the protocol prioritizes.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) derives from the agent's structural relationship to the electronic-cash telos. Payment processors are beneficiaries with arbitrage options (can use Layer 2, sidechains, or accept the on-chain constraint as a feature): d ≈ 0.15 (low, negative effective extraction). Low-value transactors are beneficiaries with constrained options (need on-chain access for their use case but limited ability to build alternatives): d ≈ 0.40 (moderate). Full-node operators are victims with trapped exit (dropping validation means losing stake in the consensus): d ≈ 0.95 (near-maximum). Node-operator coalition are victims with some organizational capacity (can coordinate, can lobby for protocol changes, can fork): d ≈ 0.65 (high but not maximum). Core developers are beneficiaries with arbitrage options (can evolve the protocol, can bridge readings through Layer 2 solutions): d ≈ 0.20 (low). Store-of-value faction (if modeled as a separate perspective) are victims of the electronic-cash telos with mobile options (can move to other chains, can support hard forks): d ≈ 0.50-0.60. The analytical observer uses canonical d for analytical power atom: d ≈ 0.72. The engine derives f(d) from these values and multiplies by scope modifier σ(S) = 1.2 (global scope) to compute effective extraction chi.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in this constraint arises from the gap between the stated telos (electronic cash, low fees, everyday payments) and the operational constraint (limited block space, rising fees, store-of-value reading prioritized in governance). The electronic-cash reading resolves mandatrophy by acknowledging that the telos is contested and costly to maintain: it is a real coordination mechanism (payment processing genuinely needs low-fee settlement) but operates alongside genuine asymmetric extraction (node operators bear costs without compensation). This is neither pure extraction (snare) nor pure coordination (rope) — it is tangled rope with rising extraction costs. The piton perspective (Core developer stewardship) reveals how mandatrophy is performed: ceremonial commitment to 'the whitepaper's original vision' masks pragmatic abandonment of the electronic-cash telos. The natural-law perspective (false summit) shows how the reading risks naturalizing a governance choice as technical necessity. The resolution is not to pick one type, but to recognize that the electronic-cash reading's structural coherence requires ongoing commitment to operational support for low-fee payments — which the actual protocol has not provided since 2017. This is the diagnostic signal: if the reading's core axioms were truly in force, the measurement trajectory would be flat or decreasing in extractiveness; instead, extractiveness rises, indicating that the axioms are being overridden by competing governance pressures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_vs_sibling_foreclosure,
    'Does the electronic-cash reading logically foreclose the store-of-value reading, or do both readings coexist as live positions held by different factions?',
    'Examine whether holders of the electronic-cash reading (e.g., merchants, payment processors, on-chain scaling advocates) require that store-of-value reading (e.g., hodlers, HODL-maximalists, high-fee-preference agents) abandon their commitment to coexist. Historical analysis: are there any proposals that the electronic-cash faction would accept that the store-of-value faction would reject on principle, or can both adapt protocols to serve both functions?',
    'If foreclosure: reading_relations entry is ''forecloses''. If coexistence: reading_relations entry is ''coexists_with''. Foreclosure suggests a genuine kernel contradiction; coexistence suggests factionalism within a larger commitment structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_sibling_foreclosure, conceptual, 'Whether electronic-cash reading forecloses store-of-value reading').

omega_variable(
    block_size_sufficiency_threshold,
    'What block size (or transaction throughput) is sufficient to instantiate ''low fees'' in the whitepaper''s electronic-cash specification?',
    'Historical fee data analysis: establish threshold at which average transaction fees for low-value transfers become statistically indistinguishable from traditional payment networks (credit card processing, ACH); identify at what block size this threshold is crossed for sustained traffic patterns.',
    'If threshold requires blocks > 8MB: electronic-cash reading is only coherent with radical on-chain scaling, making it incompatible with decentralization-maximalist axioms. If threshold is met by Layer 2 solutions: electronic-cash reading can coexist with store-of-value reading via protocol agnosticism about settlement layer.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(block_size_sufficiency_threshold, empirical, 'Block size or throughput required for ''low fees'' specification').

omega_variable(
    merchant_adoption_causality,
    'Does low on-chain fee availability causally drive merchant payment adoption, or is merchant adoption primarily driven by regulatory clarity, point-of-sale infrastructure maturity, and volatility stability?',
    'Causal analysis of merchant adoption rates vs. on-chain fee levels over time; compare adoption rates in periods of low fees (2011-2013) vs high fees (2017-2021) vs Layer 2 periods (2021+). Control for infrastructure maturity and regulatory environment.',
    'If fee causality strong: electronic-cash reading is empirically grounded. If causality weak: electronic-cash reading is performing a ceremonial function (theater) rather than achieving the stated purpose. Shifts classification toward piton if beneficiaries are performing low-fee commitment while accepting high-fee reality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(merchant_adoption_causality, empirical, 'Causal relationship between on-chain fees and merchant adoption').

omega_variable(
    whitepaper_telos_binding_force,
    'In what sense is the whitepaper''s stated electronic-cash purpose ''binding'' on protocol development?',
    'Examine governance mechanisms: do Core developers, miners, or community consensus treat the whitepaper as normative law (binding precedent that requires justification to deviate from) or as historical reference (descriptive of original intent but permitting evolution)? Survey governance forums, protocol amendment discussions, and consensus-formation patterns.',
    'If whitepaper is treated as binding normative authority: cs_structure.authority_grounding should be ''lineage'' (transmission of original vision). If permitting evolution: authority_grounding should be ''distributed'' or ''practice'' (protocol evolves through community consensus). This affects how reading_relations are structured (foreclosure vs. coexistence).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(whitepaper_telos_binding_force, conceptual, 'Normative vs. historical status of whitepaper''s stated purpose').

omega_variable(
    node_operator_exit_illusion,
    'Is the full-node operator''s ''trapped'' classification accurate, or do node operators have practical exit options (switch to light clients, SPV, or non-validating relay nodes) that would reclassify them as ''constrained'' or even ''mobile''?',
    'Survey full-node operators regarding actual costs of switching to lightweight alternatives; measure functional loss (validation capacity, stake in consensus, network resilience contribution) against cost savings. Identify what portion of the node operator base are volunteers (trapped by ideology/commitment) vs. paid operators (trapped by contract).',
    'If exit is practical and low-cost: classification shifts to ''constrained'' and chi decreases (lower f(d)). If exit requires becoming a different type of agent (losing validation role, stake, or community standing): classification remains ''trapped'' (identity_locked element). This determines whether the snare classification holds or degrades to tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(node_operator_exit_illusion, empirical, 'Exit options for full-node operators under electronic-cash mandate').

omega_variable(
    reading_kernel_codification_ambiguity,
    'Is the bitcoin whitepaper a ''fixed_text'' kernel (treat words as binding law) or an ''implicit'' kernel (the whitepaper is historical artifact; the real kernel is what the community does)?',
    'Examine governance rhetoric: when disputes arise (e.g., block size wars, fee policy, hard forks), do protagonists cite the whitepaper as authoritative text that constrains choices, or cite it as historical context while arguing the true protocol is established through consensus? Track which governance decisions are justified as ''true to the whitepaper'' vs. ''evolution beyond the whitepaper''.',
    'If fixed_text: the electronic-cash reading is a reading of stable, unchanging words — kernel_codification = ''fixed_text''. If implicit: the whitepaper is symbolic/ceremonial and the real kernel is the evolving protocol consensus — kernel_codification = ''implicit''. This affects cs_structure and how reading_relations are computed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_codification_ambiguity, conceptual, 'Whitepaper kernel codification status: fixed text vs. implicit consensus').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__electronic_cash_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(btc_cash_tr_t0, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(btc_cash_tr_t5, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 5, 0.4).
narrative_ontology:measurement(btc_cash_tr_t10, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(btc_cash_be_t0, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(btc_cash_be_t5, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(btc_cash_be_t10, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(btc_cash_su_t0, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(btc_cash_su_t5, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(btc_cash_su_t10, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__electronic_cash_reading, resource_allocation).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_whitepaper_purpose__store_of_value_reading).

% DUAL FORMULATION NOTE:
% The bitcoin_whitepaper_purpose kernel is modeled as two separate constraint stories: electronic_cash_reading (this file) and store_of_value_reading (sibling file). They share the same kernel (the whitepaper's stated commitments) but instantiate different epsilon values, beneficiary/victim structures, and governance axioms. The electronic_cash_reading has higher extractiveness (0.58) because maintaining the telos is operationally costly; the store_of_value_reading would have lower extractiveness because HODL-maximalist preferences align with actual protocol development (high fees, limited on-chain capacity). Neither reading is a measurement artifact or perspectival variant of a single constraint — they are genuinely different commitments with different structural consequences. Link via network.affects_constraints to model the kernel family relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__electronic_cash_reading, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
