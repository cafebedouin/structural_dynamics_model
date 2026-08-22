% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper_purpose__store_of_value_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: bitcoin_whitepaper_purpose__store_of_value_reading
 *   human_readable: Bitcoin Store-of-Value Reading: Decentralization Over On-Chain Capacity
 *   domain: distributed_systems/monetary_theory/technology_governance
 *
 * SUMMARY:
 *   The Bitcoin whitepaper's opening declares the goal is 'a peer-to-peer
 *   electronic cash system,' but this constraint story instantiates the
 *   store-of-value reading: a governance interpretation that prioritizes
 *   decentralization and full-node verifiability over on-chain transaction
 *   capacity. Under this reading, limited block size (1MB, later 4MB with
 *   SegWit) is a binding constraint maintained by protocol developers and
 *   enforced through consensus rules and network coordination. Low-value
 *   users are priced off the base layer, while long-term holders and node
 *   operators benefit from scarcity and security properties. The reading
 *   coexists (uneasily) with the electronic cash reading held by Bitcoin Cash
 *   and other fork advocates, and both are contestable interpretations of an
 *   ambiguous kernel (the whitepaper text) left unsettled by Satoshi
 *   Nakamoto's 2011 disappearance.
 *
 * KEY AGENTS:
 *   - long_term_holders: Primary beneficiaries (wealth stored, scarcity defended); powerful; arbitrage exit
 *   - node_operators: Secondary beneficiaries and agenda-setters (enforce protocol rules); organized; coordinate governance
 *   - protocol_developers: Tertiary beneficiaries and primary agenda-setters (control reference implementation, interpret kernel); institutional; maintain dominant reading
 *   - low_value_transaction_users: Primary victims (priced off base layer); powerless; constrained exit
 *   - unbanked_populations: Victims (promised access to currency, delivered inaccessible on-chain fees); powerless; identity-locked
 *   - electronic_cash_advocates: Excluded (alternative reading, no control); powerful; structurally contained
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
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__store_of_value_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__store_of_value_reading, "Bitcoin Store-of-Value Reading: Decentralization Over On-Chain Capacity").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__store_of_value_reading, "distributed_systems/monetary_theory/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__store_of_value_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__store_of_value_reading, 'acb9fefe-f3a1-4f73-99ad-892721081bcb').
narrative_ontology:cs_kernel_codification('acb9fefe-f3a1-4f73-99ad-892721081bcb', fixed_text).
narrative_ontology:cs_authority_grounding('acb9fefe-f3a1-4f73-99ad-892721081bcb', extraction).
narrative_ontology:cs_interpretation_layer_present('acb9fefe-f3a1-4f73-99ad-892721081bcb').
narrative_ontology:cs_reading_relation('acb9fefe-f3a1-4f73-99ad-892721081bcb', bitcoin_whitepaper_purpose__electronic_cash_reading, coexists_with).
narrative_ontology:cs_reading_relation('acb9fefe-f3a1-4f73-99ad-892721081bcb', bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, influences).
narrative_ontology:cs_axiom('acb9fefe-f3a1-4f73-99ad-892721081bcb', foundational, decentralization_binding_constraint).
narrative_ontology:cs_axiom_status(decentralization_binding_constraint, holdable).
narrative_ontology:cs_axiom_grounding('acb9fefe-f3a1-4f73-99ad-892721081bcb', decentralization_binding_constraint, deontological).
narrative_ontology:cs_axiom('acb9fefe-f3a1-4f73-99ad-892721081bcb', foundational, capacity_subordinate_to_node_verifiability).
narrative_ontology:cs_axiom_status(capacity_subordinate_to_node_verifiability, holdable).
narrative_ontology:cs_axiom_grounding('acb9fefe-f3a1-4f73-99ad-892721081bcb', capacity_subordinate_to_node_verifiability, instrumental).
narrative_ontology:cs_axiom('acb9fefe-f3a1-4f73-99ad-892721081bcb', secondary, store_of_value_primary_use_case).
narrative_ontology:cs_axiom_status(store_of_value_primary_use_case, holdable).
narrative_ontology:cs_axiom_grounding('acb9fefe-f3a1-4f73-99ad-892721081bcb', store_of_value_primary_use_case, empirically_contingent).
narrative_ontology:cs_reference_frame('acb9fefe-f3a1-4f73-99ad-892721081bcb', decentralized_full_node_security_model).
narrative_ontology:cs_drift_state('acb9fefe-f3a1-4f73-99ad-892721081bcb', contemporary_adoption_scaling_pressure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('acb9fefe-f3a1-4f73-99ad-892721081bcb', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, node_operators).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, protocol_developers).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, low_value_transaction_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, unbanked_populations).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, retail_merchants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, miners).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, layer_2_protocol_teams).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, miners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Accumulate and hold bitcoin as store of value, benefiting from protocol rules that prioritize scarcity, decentralization, and censorship-resistance over transaction throughput. They benefit from the 1MB block limit which constrains supply and from the security model that depends on widespread node participation. Their exit is low-friction: they can trade or hold at will.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_holders, beneficiary,
    powerful, civilizational, arbitrage, global).

% Run full verification nodes to participate in consensus and enforce protocol rules. The store-of-value reading privileges their ability to run nodes on commodity hardware by maintaining low bandwidth and storage requirements via the 1MB block limit. They exert coordination power over protocol upgrades through network influence and can upgrade or downgrade their nodes.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, node_operators, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__store_of_value_reading, node_operators, agenda_setter).

% Interpret and defend the reading that decentralization is the binding constraint. They control the reference implementation, coordinate on protocol governance forums, and actively suppress or reject scaling proposals (SegWit2x, larger blocks) that would violate the reading's axioms. Their exit from this framing is costly because their credibility and influence rest on maintaining it.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, protocol_developers, agenda_setter,
    institutional, generational, constrained, global).

% Cannot afford the on-chain fees generated by limited block space (currently 10-50+ satoshis per byte in congestion, pricing out sub-dollar transactions). They are redirected toward off-chain solutions (Lightning Network) which require technical sophistication and capital to establish payment channels. They bear the cost of the capacity constraint without voice in governance.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, low_value_transaction_users, payer,
    powerless, biographical, constrained, global).

% In regions without banking infrastructure, Bitcoin was theoretically accessible as censorship-resistant money. The store-of-value reading's on-chain capacity limits make base-layer access expensive or impossible; they cannot adopt Lightning without precursor access to capital and internet infrastructure. Their identity (excluded from formal finance) makes them ideally suited to Bitcoin's promise, but they are excluded by this reading's implementation.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, unbanked_populations, payer,
    powerless, biographical, identity_locked, global).

% Cannot economically accept Bitcoin on-chain for everyday purchases (confirmed transactions must settle within transaction cost). They are directed toward payment processors and layer-2 solutions, centralizing the settlement process they adopted Bitcoin to avoid. Fees consumed the retail margin for low-value goods.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, retail_merchants, payer,
    moderate, biographical, constrained, regional).

% Earn block rewards and transaction fees from the mining subsidy. The store-of-value reading sustains mining economics by maintaining scarcity (1MB limit → high demand for block space → rising fees). However, limited on-chain capacity caps fee revenue; they benefit from the security model but are constrained by transaction throughput.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, miners, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__store_of_value_reading, miners, payer).

% Hold the alternative reading that Bitcoin should support everyday transactions with low fees, as the whitepaper's 'cash' framing implies. They argue the store-of-value reading betrays Satoshi's original purpose. Their objections are structurally excluded from core protocol development by the institutional control of the developer set and the network's Schelling point coordination around the dominant reading.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, electronic_cash_advocates, excluded,
    powerful, generational, constrained, global).

% Build and deploy off-chain scaling solutions (Lightning Network, Liquid sidechains). The store-of-value reading's on-chain capacity constraint MANDATES the existence of scaling layers; they benefit from this reading because their protocols become economically necessary infrastructure.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, layer_2_protocol_teams, beneficiary,
    organized, generational, mobile, global).

% Analyze whether decentralization and full-node verifiability are mathematically and economically necessary to Bitcoin's security model, or whether they are design choices that could be relaxed. They study the trade-offs between consensus security, scalability, and decentralization.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, cryptographic_purists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_holders).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper_purpose__store_of_value_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves Byzantine consensus: agreement on a canonical ledger in a network without a trusted operator or single point of authority. Decentralization and full-node verifiability enable every participant to cryptographically audit the ledger without trusting any intermediary, eliminating counterparty risk for currency settlement.
% TRANSFER_FUNCTION: Moves block space (on-chain transaction settlement capacity) from low-value transaction users to long-term holders and node operators. The 1MB block limit creates scarcity of settlement capacity; this scarcity is bid for by users willing to pay high fees (institutional traders, long-term accumulators) while pricing out low-value retail payments and micropayments.
% ABSENT_VOICES: Users unable to afford on-chain fees (retail merchants, low-income users, unbanked populations) are absent from core protocol governance; their interests would prioritize on-chain throughput and low fees but have no formal voice in developer meetings or GitHub consensus. Electronic cash advocates (Bitcoin Cash community) are present in public discourse but excluded from the reference implementation and from the network's Schelling-point coordination around the store-of-value reading.
% DISAPPEARANCE_RATIONALE: Proponents of the store-of-value reading argue that without the 1MB limit, decentralization would erode (higher resource requirements for nodes, centralization of mining toward large operators), undermining Bitcoin's security against state seizure. If the constraint disappeared, they predict node participation would collapse and Bitcoin would be captured by large miners or governments. Proponents of the electronic cash reading argue that if the capacity limit disappeared, Bitcoin would function as intended (electronic cash system with low fees) and would reorganize around higher throughput, retaining decentralization (as evidenced by Bitcoin Cash's survival). The truth of each claim is contested: no agreed-upon counterfactual exists because the fork has not been unified.
% FOUNDING_PROBLEM: Create a peer-to-peer electronic cash system that enables direct transactions without trusted intermediaries and without central authority control. The system must be censorship-resistant, resistant to counterfeiting, and operationally independent of banking or government systems.
% FOUNDING_PROBLEM_CORROBORATION: Protocol developers (Bitcoin Core maintainers) attest the founding problem remains live: state actors continue attempting to regulate or seize bitcoin, and decentralization is the structural defense that cannot be solved by scaling (high throughput requires larger minimum operating costs for nodes, which enable centralization and seizure). Electronic cash advocates and Bitcoin Cash proponents attest the founding problem is differently solved: consensus security against state attack is independent of on-chain throughput; the 1MB limit was introduced for engineering reasons (Satoshi's spam protection) and is now defended as a governance choice, not a cryptographic necessity. Academic cryptographers (e.g., Arvind Narayanan, Nicholas Weaver) attest that consensus protocols can be secure at higher throughput if designed for it; the claim that 1MB is necessary for security is not corroborated by the literature on Byzantine consensus.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__store_of_value_reading, contested).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__store_of_value_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__store_of_value_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__store_of_value_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper_purpose__store_of_value_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is 0.68 at interval end, measured as the degree to which the constraint subordinates user access to base-layer throughput in favor of beneficiary interests. It is not maximal because genuine coordination (censorship-resistant consensus) does occur; it is substantial because the 1MB block limit is not mathematically necessary for consensus, only for a specific (contested) interpretation of how Bitcoin should operate. Suppression is 0.71 because the reading is maintained through active control: protocol developers reject scaling proposals, the network's Schelling point coordinates around the store-of-value framing, and alternative readings (electronic cash) are excluded from core implementation authority despite widespread support among merchants and low-value users. The measurement series spans 16 years (0-16, modeling 2009-2024 approximately). Early extractiveness is low (0.35) because fees are negligible when adoption is sparse; it rises sharply (0.35→0.68) as adoption grows and block space becomes scarce. Theater ratio rises from 0.15 to 0.42, indicating increasing performative justification: early rhetoric emphasizes decentralization's necessity; later rhetoric increasingly invokes abstract principles (digital sound money, sovereign wealth storage) while suppressing or ignoring the lived experience of excluded users. Suppression requirement rises steeply (0.42→0.71) because maintaining the reading requires more active exclusion as scaling solutions mature and the electronic cash alternative becomes concrete (Bitcoin Cash fork at t≈8, Lightning adoption pressure at t≈10-12). The plateau at t≥12 reflects stabilization: the store-of-value reading is consolidated, scaling solutions are institutionalized as fee-dependent relays, and suppression effort is routinized.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary and developer seats compute the constraint as genuine coordination (decentralization requires limited on-chain capacity to keep nodes runnable on consumer hardware — a real trade-off). The victim seats compute it as pure extraction (a capacity limit chosen to benefit token holders, defended through institutional control and narrative framing). The excluded electronic cash advocates compute it as usurpation (the kernel text supports their reading; the dominant reading suppresses it through governance control, not cryptographic necessity). These divergences are NOT errors: they follow logically from the structural positions and exit options. The store-of-value reading's claim is rope; the authored metrics describe tangled coordination (real consensus problem solved) plus extraction (capacity subordinated for beneficiary gain). The engine computes per-seat classification and will likely show tangled_rope from beneficiary seats and snare from victim seats, precisely because the constraint's structure creates asymmetric verification: both readings cite the same kernel (whitepaper), but institutional control determines which reading gets implemented.
 *
 * DIRECTIONALITY LOGIC:
 *   Long-term holders benefit from scarcity (d ≈ 0.2, near-beneficiary): the 1MB limit defends their accumulation. Node operators benefit from low resource requirements (d ≈ 0.25) and from the governance influence they exercise (organized power, mobile exit). Protocol developers benefit from authority to define the reading (d ≈ 0.3, moderate beneficiary, constrained by the need to defend against fork risk and maintain credibility). Low-value transaction users are targets (d ≈ 0.85, near-full target): they bear the cost of exclusion (pay market-rate fees or adopt L2) with no voice in the decision (powerless, identity-locked exit). Unbanked populations are targets (d ≈ 0.9, near-full target): their theoretical benefit from censorship-resistance is realized only if they can afford on-chain access, which the constraint prevents. Electronic cash advocates sit at d ≈ 0.7 (high target): they are excluded from decision-making but their excluded position is structural, not extractive of their labor or wealth — they are prevented from participating, not actively exploited. Miners are ambiguous (d ≈ 0.55, near-symmetric): they benefit from fee scarcity (high fees from capacity limit) but lose from limited transaction volume. Directionality overrides not needed: the structural derivation from beneficiary/victim and exit options yields accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is 'create censorship-resistant currency without trusted authority.' The store-of-value reading subordinates on-chain capacity to preserve decentralization, claiming this is structurally necessary. The electronic cash reading claims the founding problem is solved at lower on-chain throughput, and the capacity limit is a contingent governance choice, not a mathematical requirement. Mandatrophy appears here: if the founding problem is 'solve consensus in a distributed network,' it is SOLVED by Bitcoin's consensus mechanism (Proof of Work + longest chain rule) regardless of block size. The 1MB limit does not solve consensus; it controls access to settled transactions. If the founding problem is 'create a medium of exchange usable by ordinary people without banks,' the store-of-value reading ABANDONS the solving problem and replaces it with a different goal (maximum security for long-term asset holders). The constraint classification avoids conflating the two by refusing to call it pure coordination (which would require the founding problem to still require the solved-problem's framing) while naming the real coordination (consensus is solved) alongside the real extraction (access is subordinated). Tangled Rope is the appropriate classification: there IS genuine coordination (Byzantine consensus), AND there IS asymmetric extraction (capacity rationed to benefit long-term holders), AND active enforcement is required (developers and node operators maintain the limit against fork pressure).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decentralization_necessity_for_consensus,
    'Is decentralization (distributed node participation in consensus) a mathematical requirement for Byzantine-resilient consensus, or a design choice that trades off scalability for security?',
    'Formal analysis of consensus under different security assumptions (adversary power budgets, honest participant distribution) and empirical study of high-throughput consensus protocols (Solana, Tendermint, Algorand) that maintain security at lower decentralization levels. Examine whether the store-of-value reading''s decentralization requirement prevents consensus from working at higher throughput or merely reduces some security margins.',
    'If decentralization is mathematically necessary, the 1MB limit is a binding constraint and the store-of-value reading is structurally justified. If decentralization is a design choice, the 1MB limit is a governance choice that benefits some parties (long-term holders, node operators) over others (transaction users), and the constraint is extractive rather than foundational.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(decentralization_necessity_for_consensus, empirical, 'Whether decentralization is a mathematical requirement for Byzantine consensus or a design choice trading scalability for security.').

omega_variable(
    layer_2_equivalence_to_base_layer_access,
    'Do off-chain scaling solutions (Lightning, sidechains) provide economically equivalent access to settlement for low-value users as on-chain transactions would, or do they introduce new costs and barriers (channel setup, custody risk, hub dependency)?',
    'Comparative analysis of real transaction costs and user experience on Lightning vs. on-chain payment; study of adoption barriers and failure modes in payment-channel networks; economic analysis of whether channel-opening fees and hub-dependent routing create new forms of centralization that undermine the decentralization goal.',
    'If L2 solutions are equivalent, the store-of-value reading does not extract from users (it relocates them to equivalent infrastructure). If L2 solutions impose substantial new costs or centralization, the store-of-value reading extracts from users by subordinating their base-layer access and forcing them through more expensive or centralized intermediaries, contradicting the decentralization promise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(layer_2_equivalence_to_base_layer_access, empirical, 'Whether off-chain scaling provides economically equivalent access to base-layer settlement or introduces new extraction barriers.').

omega_variable(
    electronic_cash_reading_institutional_suppression,
    'Is the electronic cash reading suppressed because its core claim is false, or because protocol developers and node operators have institutional interests in the store-of-value reading (scarcity premium, governance authority) that incentivize suppression?',
    'Document the history of scaling debates (2015-2017 block size war, SegWit activation, SegWit2x fork attempt): identify which proposals were rejected on cryptographic grounds (falsifiable, resolvable) vs. governance/economic grounds (beneficiary protection). Examine developer compensation sources and whether compensation incentivizes store-of-value reading (long-term token holdings, investor participation in governance).Study fork counterfactuals: if Bitcoin Cash had won the hash power majority, would the result be technically unstable or merely contrary to the store-of-value reading''s preferences?',
    'If suppression is technical, the electronic cash reading is indeed weaker. If suppression is institutional-interest-driven, the store-of-value reading is maintained through governance control and narrative power, making it a tangled_rope or snare rather than a coordinate solution to a real problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(electronic_cash_reading_institutional_suppression, conceptual, 'Whether the electronic cash reading is suppressed because it is technically inferior or because it threatens the beneficiary interests of the store-of-value coalition.').

omega_variable(
    identity_lock_mechanism_unbanked_users,
    'Are unbanked populations identity-locked to Bitcoin because of their financial exclusion (no alternatives), or are they constrained-exit because cheaper money transfers exist (remittance services, stablecoins, CBDC pilots) but are unavailable in their specific region?',
    'Field study of actual transaction patterns and alternatives available in economically excluded regions (Sub-Saharan Africa, South Asia); examine whether the store-of-value reading''s on-chain fee structure is the binding constraint or whether unavailability of any non-banking payment rail is the primary barrier.',
    'If identity-locked (truly no alternatives), then unbanked populations are victims even of cheap-fee on-chain access (they are locked in by exclusion, not bound to Bitcoin by choice). If constrained-exit (cheaper alternatives exist but are unavailable regionally), the store-of-value reading extracts by pricing Bitcoin off-chain out of reach while alternative systems are also unavailable, leaving no exit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_unbanked_users, empirical, 'The exit-option classification for unbanked populations: identity-locked (structurally excluded) vs. constrained (cheaper alternatives unavailable regionally).').

omega_variable(
    founding_problem_boundary_conflict,
    'Does the founding problem require solving ''everyday peer-to-peer cash without banks'' or only ''censorship-resistant settlement without trusted authorities''? Are these the same problem or different targets?',
    'Historical analysis of Satoshi''s original posts, economic papers cited in the whitepaper, and the stated problem in the abstract vs. the technical design. Compare the requirements of each interpretation: everyday cash requires low fees, instant settlement, and ease of use; censorship-resistant settlement requires security guarantees against state capture, not transactional convenience.',
    'If the founding problem is everyday cash, the store-of-value reading abandons the founding problem and replaces it with a different goal (digital gold), making mandatrophy_resolved = true (the founding problem is dead but the constraint persists). If the founding problem is censorship-resistant settlement (which does not require everyday accessibility), the store-of-value reading solves it, but the electronic cash reading ALSO solves it (settlement is censorship-resistant even at higher throughput), making both readings plausible solutions to the same founding problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_boundary_conflict, conceptual, 'The boundary between the founding problem''s scope (cash vs. censorship-resistant settlement) and the implications for whether the store-of-value reading maintains or abandons the founding goal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__store_of_value_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(bitc_tr_t0, observed).
narrative_ontology:measurement(bitc_tr_t2, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2, 0.18).
narrative_ontology:measurement_basis(bitc_tr_t2, observed).
narrative_ontology:measurement(bitc_tr_t4, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 4, 0.23).
narrative_ontology:measurement_basis(bitc_tr_t4, observed).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 6, 0.29).
narrative_ontology:measurement_basis(bitc_tr_t6, observed).
narrative_ontology:measurement(bitc_tr_t8, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement_basis(bitc_tr_t8, observed).
narrative_ontology:measurement(bitc_tr_t10, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement_basis(bitc_tr_t10, observed).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 12, 0.41).
narrative_ontology:measurement_basis(bitc_tr_t12, observed).
narrative_ontology:measurement(bitc_tr_t14, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 14, 0.42).
narrative_ontology:measurement_basis(bitc_tr_t14, observed).
narrative_ontology:measurement(bitc_tr_t16, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 16, 0.42).
narrative_ontology:measurement_basis(bitc_tr_t16, observed).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(bitc_be_t0, observed).
narrative_ontology:measurement(bitc_be_t2, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2, 0.42).
narrative_ontology:measurement_basis(bitc_be_t2, observed).
narrative_ontology:measurement(bitc_be_t4, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 4, 0.51).
narrative_ontology:measurement_basis(bitc_be_t4, observed).
narrative_ontology:measurement(bitc_be_t6, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 6, 0.58).
narrative_ontology:measurement_basis(bitc_be_t6, observed).
narrative_ontology:measurement(bitc_be_t8, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 8, 0.63).
narrative_ontology:measurement_basis(bitc_be_t8, observed).
narrative_ontology:measurement(bitc_be_t10, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 10, 0.66).
narrative_ontology:measurement_basis(bitc_be_t10, observed).
narrative_ontology:measurement(bitc_be_t12, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 12, 0.67).
narrative_ontology:measurement_basis(bitc_be_t12, observed).
narrative_ontology:measurement(bitc_be_t14, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 14, 0.68).
narrative_ontology:measurement_basis(bitc_be_t14, observed).
narrative_ontology:measurement(bitc_be_t16, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 16, 0.68).
narrative_ontology:measurement_basis(bitc_be_t16, observed).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(bitc_su_t0, observed).
narrative_ontology:measurement(bitc_su_t2, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2, 0.49).
narrative_ontology:measurement_basis(bitc_su_t2, observed).
narrative_ontology:measurement(bitc_su_t4, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 4, 0.56).
narrative_ontology:measurement_basis(bitc_su_t4, observed).
narrative_ontology:measurement(bitc_su_t6, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 6, 0.61).
narrative_ontology:measurement_basis(bitc_su_t6, observed).
narrative_ontology:measurement(bitc_su_t8, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 8, 0.65).
narrative_ontology:measurement_basis(bitc_su_t8, observed).
narrative_ontology:measurement(bitc_su_t10, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 10, 0.69).
narrative_ontology:measurement_basis(bitc_su_t10, observed).
narrative_ontology:measurement(bitc_su_t12, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement_basis(bitc_su_t12, observed).
narrative_ontology:measurement(bitc_su_t14, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 14, 0.71).
narrative_ontology:measurement_basis(bitc_su_t14, observed).
narrative_ontology:measurement(bitc_su_t16, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 16, 0.71).
narrative_ontology:measurement_basis(bitc_su_t16, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__store_of_value_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper_purpose__store_of_value_reading, 0.25).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose__electronic_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity).

% DUAL FORMULATION NOTE:
% The bitcoin_whitepaper_purpose kernel instantiates three constraint stories: (1) store_of_value_reading (this story) — decentralization and node verifiability are binding, on-chain capacity is subordinated, beneficiaries are long-term holders and node operators, victims are low-value users; (2) electronic_cash_reading — the whitepaper's 'cash' telos is binding, system must support everyday transactions with low fees, beneficiaries are transaction users and merchants, victims are store-of-value holders whose purchasing power is constrained by broader adoption; (3) nakamoto_oracle_opacity — Satoshi's disappearance eliminated authoritative interpretation, making the kernel text itself a contested substrate. The store-of-value and electronic cash readings coexist in the Bitcoin ecosystem but are incompatible in a single protocol instance (capacity limit enforces one reading, higher throughput enforces the other). Each reading has its own epsilon, beneficiary structure, and type classification. The network edges record that the store-of-value reading influences the electronic cash reading (by making cash-layer throughput impossible, forcing it to fork or relocate to Layer 2) and that both are conditioned by the oracle opacity (the absence of authoritative interpretation preserves the contest).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__store_of_value_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
