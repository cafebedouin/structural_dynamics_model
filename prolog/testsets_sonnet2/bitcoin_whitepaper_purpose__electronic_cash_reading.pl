% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper_purpose__electronic_cash_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bitcoin_whitepaper_purpose__electronic_cash_reading
 *   human_readable: Bitcoin as Peer-to-Peer Electronic Cash (Big-Block / On-Chain Scaling Reading)
 *   domain: distributed_systems/monetary_theory/technology_governance
 *
 * SUMMARY:
 *   Bitcoin's founding whitepaper is titled 'Bitcoin: A Peer-to-Peer
 *   Electronic Cash System.' One reading of that title treats 'cash' as the
 *   binding design telos: the system must remain usable for small, frequent,
 *   low-fee transactions, which in practice requires expanding on-chain block
 *   capacity (8MB+ has been proposed and, in forked implementations,
 *   deployed) to keep per-transaction fees low as adoption grows. This
 *   reading was operationalized in the 2017 block size fork that produced
 *   Bitcoin Cash and its subsequent splits, and remains a live position among
 *   some Bitcoin proponents who argue the original chain has drifted from its
 *   founding purpose toward a settlement-layer / store-of-value function
 *   instead.
 *
 * KEY AGENTS:
 *   - payment_processors: primary beneficiary (organized/mobile) — lower per-transaction costs, larger addressable market
 *   - home_node_operators: primary target (powerless/trapped) — bear rising verification costs from larger blocks
 *   - mining_pools_favoring_throughput: secondary beneficiary/agenda_setter (powerful/arbitrage) — profit from transaction volume, can force chain splits
 *   - protocol_developers_scaling_faction: agenda_setter (organized/arbitrage) — implements and defends the capacity-increase interpretation
 *   - small_block_faction_developers: excluded from this reading's frame — holds the sibling reading, authored elsewhere
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.58).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.52).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__electronic_cash_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__electronic_cash_reading, "Bitcoin as Peer-to-Peer Electronic Cash (Big-Block / On-Chain Scaling Reading)").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__electronic_cash_reading, "distributed_systems/monetary_theory/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__electronic_cash_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__electronic_cash_reading, '41273621-2da4-4fed-b538-2ba99d79e98e').
narrative_ontology:cs_kernel_codification('41273621-2da4-4fed-b538-2ba99d79e98e', fixed_text).
narrative_ontology:cs_authority_grounding('41273621-2da4-4fed-b538-2ba99d79e98e', distributed).
narrative_ontology:cs_reading_relation('41273621-2da4-4fed-b538-2ba99d79e98e', bitcoin_whitepaper_purpose__store_of_value_reading, coexists_with).
narrative_ontology:cs_reading_relation('41273621-2da4-4fed-b538-2ba99d79e98e', bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, influences).
narrative_ontology:cs_axiom('41273621-2da4-4fed-b538-2ba99d79e98e', foundational, title_text_fixes_governing_purpose).
narrative_ontology:cs_axiom_status(title_text_fixes_governing_purpose, holdable).
narrative_ontology:cs_axiom_grounding('41273621-2da4-4fed-b538-2ba99d79e98e', title_text_fixes_governing_purpose, conventional).
narrative_ontology:cs_axiom('41273621-2da4-4fed-b538-2ba99d79e98e', secondary, throughput_capacity_is_engineering_tradeoff_not_constraint).
narrative_ontology:cs_axiom_status(throughput_capacity_is_engineering_tradeoff_not_constraint, holdable).
narrative_ontology:cs_axiom_grounding('41273621-2da4-4fed-b538-2ba99d79e98e', throughput_capacity_is_engineering_tradeoff_not_constraint, instrumental).
narrative_ontology:cs_reference_frame('41273621-2da4-4fed-b538-2ba99d79e98e', original_whitepaper_cash_framing).
narrative_ontology:cs_drift_state('41273621-2da4-4fed-b538-2ba99d79e98e', post_2017_scaling_wars, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('41273621-2da4-4fed-b538-2ba99d79e98e', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, payment_processors).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, low_value_transactors).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, merchant_adopters).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, mining_pools_favoring_throughput).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__electronic_cash_reading, home_node_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__electronic_cash_reading, resource_constrained_verifiers).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__electronic_cash_reading, future_full_node_participants).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper_purpose__electronic_cash_reading, whitepaper_title_is_governing_telos).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper_purpose__electronic_cash_reading, on_chain_scaling_preserves_peer_to_peer_property).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build merchant-facing infrastructure that depends on low per-transaction fees and fast confirmation for everyday purchases. A big-block, high-throughput chain lowers their per-transaction cost basis and expands the market of viable use cases (coffee, retail, remittance). They can route volume to whichever chain implementation wins the capacity argument, and lobby for larger block sizes accordingly.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, payment_processors, beneficiary,
    organized, biographical, mobile, global).

% Ordinary users who want to send small amounts cheaply — remittance senders, everyday spenders, unbanked populations targeted by the whitepaper's original framing. They benefit directly from lower fees and faster settlement but have no direct voice in the governance fight over block size; they experience the outcome as either cash-like usability or fee-priced exclusion.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, low_value_transactors, beneficiary,
    powerless, immediate, constrained, global).

% Businesses that would accept Bitcoin for point-of-sale transactions if fees stay low and confirmation stays fast. Their adoption decisions are contingent on the network optimizing for transactional throughput rather than settlement assurance; they can simply not adopt, or adopt a competing chain, if fees rise.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, merchant_adopters, beneficiary,
    moderate, biographical, mobile, national).

% Large mining operations that profit from higher transaction volume and fee revenue at scale; some have economic incentive to support larger blocks since it grows the fee market and transaction count they can process. They have historically pushed for block size increases and can credibly threaten hash-power realignment or chain splits to enforce this reading.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, mining_pools_favoring_throughput, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__electronic_cash_reading, mining_pools_favoring_throughput, agenda_setter).

% Individuals running full nodes on consumer hardware and residential internet connections to independently verify the chain. Larger blocks directly raise the storage, bandwidth, and initial-sync cost of running a node. As blocks grow, many are priced out of verification and must either trust third parties or stop verifying — their only real exit is abandoning independent verification altogether, which forfeits the property they joined the network to hold.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, home_node_operators, payer,
    powerless, generational, trapped, global).

% Verifiers in regions with expensive or capped bandwidth and storage — often exactly the unbanked or economically marginal populations the electronic-cash vision claims to serve. Bigger blocks that lower transaction fees for them simultaneously raise the cost of running the software that lets them verify their own holdings without trusting an intermediary.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, resource_constrained_verifiers, payer,
    powerless, generational, trapped, global).

% Not yet in the network, but structurally affected by irreversible capacity decisions made now: a permanently larger block size compounds the initial-block-download burden for every future participant who wants to verify the chain from genesis. They have no voice in the present governance contest and inherit whatever verification cost the current fight settles on.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, future_full_node_participants, payer,
    powerless, civilizational, trapped, global).

% Developers and client maintainers who read the whitepaper's title and abstract as binding: 'A Purely Peer-to-Peer Version of Electronic Cash.' They advocate for and implement on-chain capacity increases, framing verification cost as a solvable engineering problem rather than a governing constraint, and have historically forked implementations to pursue this reading when consensus with the small-block faction failed.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, protocol_developers_scaling_faction, agenda_setter,
    organized, generational, arbitrage, global).

% Hold the rival reading (decentralization/verifiability as binding) and object to this reading's premise that transactional throughput should govern capacity decisions. Structurally excluded from this reading's own frame by definition — this constraint story authors their sibling reading as a separate constraint, not as a voice within this one, per the ε-invariance decomposition rule.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, small_block_faction_developers, excluded,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper_purpose__electronic_cash_reading, diffuse).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper_purpose__electronic_cash_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a payment network where many independent parties can transact peer-to-peer without a trusted intermediary, at costs low enough for everyday retail and remittance use — the genuine collective-action problem of building a censorship-resistant, low-friction payment rail.
% TRANSFER_FUNCTION: Moves verification cost from transaction senders and receivers (who get low fees) onto node operators (who bear higher storage, bandwidth, and sync costs as capacity scales), and moves influence over the network's governing telos from the pseudonymous founding text to whichever coalition of miners, processors, and developers can operationalize their preferred reading of it.
% ABSENT_VOICES: Satoshi Nakamoto is absent by design (departed 2011) and cannot adjudicate which telos is binding. Future participants who would run nodes on cheap consumer hardware, and who are structurally priced out by a permanently expanded block size, have no seat in the current governance contest. The small-block/decentralization faction is treated as the sibling reading rather than a voice inside this one.
% DISAPPEARANCE_RATIONALE: If the electronic-cash-as-binding-telos reading disappeared overnight (i.e., this reading lost the governance contest entirely), payment processors and merchant adopters who depend on low fees would need to migrate to layered or off-chain solutions or to alternative chains; low-value transactors would face the fee pressure the original design sought to avoid. Node operators, conversely, would see their verification costs stabilize. Whether the 'world rearranges' or 'stays the same' depends entirely on which constituency you ask — hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: The whitepaper was written to solve double-spending without a trusted third party, explicitly framed in its title and abstract as enabling peer-to-peer electronic cash — a payment system usable for everyday commerce without banks or payment processors as intermediaries.
% FOUNDING_PROBLEM_CORROBORATION: The whitepaper text itself (Section 1, Introduction) and its title are cited by this reading's proponents as authoritative and unambiguous. However, no living corroboration exists from Nakamoto post-2011; the small-block faction — an outside party sharing no benefit from this reading's victory — corroborates that the text says 'electronic cash' but disputes that this phrase forecloses capacity trade-offs against decentralization, citing later Nakamoto forum posts about block size caps as evidence the founder anticipated exactly this tension without resolving it in favor of unlimited scaling.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__electronic_cash_reading, contested).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__electronic_cash_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__electronic_cash_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__electronic_cash_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.58) reflects a genuine coordination function — the network really does need to process transactions cheaply to serve as cash — layered with an asymmetric cost transfer: throughput increases are funded by degrading the economic feasibility of independent verification, and that cost falls hardest on node operators who hold no direct financial stake to offset it, unlike miners and processors who profit from volume. Suppression (0.52) captures that this reading has historically been enforced through hard forks and hash-power contests rather than pure voluntary coordination — 'enforcement' here means the capacity decision, once made in a chain a coalition controls, forecloses the option of low-cost verification for anyone who stays. Theater ratio (0.40) reflects that some capacity-increase advocacy has shifted from solving the original bandwidth/storage problem toward defending a brand/legitimacy claim ('we are the real Bitcoin') independent of whether throughput gains are still delivering the promised fee reductions at current adoption levels. Accessibility collapse (0.45) is moderate — Lightning Network and sidechains exist as alternative routes to low fees without expanding base-layer blocks, so the on-chain-capacity path is not the only route to the stated goal, which is why this reading is contested rather than settled. Resistance (0.72) is high, reflecting the sustained, organized opposition from the small-block/decentralization faction, which has repeatedly out-competed this reading for control of the dominant chain's naming and network effects.
 *
 * DIRECTIONALITY LOGIC:
 *   Payment processors, merchant adopters, and low-value transactors are declared beneficiaries because the reading's success directly lowers their costs and expands their addressable use cases — low d, benefit-side of the derivation. Mining pools favoring throughput are also beneficiaries, deriving fee-market growth from higher transaction volume, and secondarily function as agenda-setters because their hash power can enforce a fork. Home node operators, resource-constrained verifiers, and future full-node participants are declared victims: the same capacity increase that lowers fees for the beneficiary set directly raises their storage/bandwidth/sync burden, and their exit option is effectively to stop independently verifying — which is not a neutral exit but a forfeiture of the property (trustless verification) that gives the currency its value proposition in the first place. This is why their exit_options are marked trapped rather than merely constrained: leaving the constraint means abandoning the thing they joined for.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (build a payment system usable for everyday commerce without trusted intermediaries) is contested as live vs. dead: proponents of this reading say it remains completely live and unsolved on the base layer as fees have historically spiked during congestion; opponents (including the small-block faction, an outside corroborating voice) argue the problem has been substantially addressed via layer-2 protocols (Lightning) without requiring base-layer capacity expansion, and that continued push for bigger blocks past that point is agenda-setter capture (mining pools and processors preserving base-layer fee revenue and settlement dominance) rather than genuine unmet coordination need. The classification as tangled_rope rather than snare reflects that the coordination function (cheap payments) is real and was the literal stated purpose of the founding document — this is not manufactured cover, unlike a pure snare. But it is not a clean rope either, because the verification-cost transfer onto node operators, especially future ones who never consented to the capacity decision, is a genuine asymmetric extraction riding on top of that real coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cash_telos_bindingness,
    'Is the whitepaper''s title and framing (''cash'') a binding design constraint that overrides all other considerations, or one design goal among several (decentralization, censorship-resistance, fixed supply) that must be balanced rather than prioritized?',
    'No living authority can resolve this (Nakamoto''s 2011 disappearance is itself the subject of the sibling nakamoto_oracle_opacity constraint). Resolution in practice occurs through market selection among forked implementations — whichever chain''s user base, developer ecosystem, and price discovery persist longest is treated retroactively as having ''won'' the interpretive contest, though this is a sociological fact about adoption, not a logical resolution of the textual question.',
    'If the cash telos is binding, capacity-limiting design choices on the dominant chain constitute a departure from founding purpose and this reading''s beneficiaries have a strong normative claim; if the telos is one goal among several to be balanced, the store_of_value_reading''s prioritization of decentralization is equally faithful to the source text and this reading''s exclusive claim on ''the real purpose'' weakens substantially.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cash_telos_bindingness, conceptual, 'Whether the whitepaper''s stated title constitutes a binding, overriding design telos or one of several balanced goals.').

omega_variable(
    verification_cost_externality_magnitude,
    'At what block size does the storage/bandwidth cost of full-node verification become prohibitive enough for ordinary users (as opposed to well-resourced entities) to price out independent verification at scale?',
    'Empirical measurement of full-node counts, geographic distribution, and initial-block-download times across implementations with different block size limits (comparing Bitcoin, Bitcoin Cash, and other forked chains with larger blocks over comparable adoption periods).',
    'If node counts and geographic diversity have held steady or grown on higher-capacity chains, the victim-side cost claim in this story is overstated; if they have measurably declined relative to counterfactual growth, the tangled_rope classification''s asymmetric-extraction premise is empirically supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_cost_externality_magnitude, empirical, 'The empirical relationship between block size and the real-world decline of independent full-node verification.').

omega_variable(
    layer_two_substitutability,
    'Do off-chain scaling solutions (Lightning Network, sidechains) fully substitute for on-chain capacity in delivering the ''cash'' telos''s low-fee, high-throughput goal, making base-layer capacity increases unnecessary?',
    'Comparative analysis of transaction volume, fee levels, and adoption friction on layer-2 solutions versus base-layer capacity-expanded chains over a multi-year period.',
    'If layer-2 fully substitutes, this reading''s central premise (that the telos requires base-layer capacity expansion) is undermined and the extraction imposed on node operators becomes harder to justify as necessary; if layer-2 solutions face persistent adoption friction or centralization pressure of their own, the on-chain reading''s coordination claim strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(layer_two_substitutability, empirical, 'Whether off-chain scaling solutions are a genuine substitute for the capacity expansion this reading demands.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__electronic_cash_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(bitc_tr_t3, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 3, 0.25).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement(bitc_tr_t9, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 9, 0.34).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement(bitc_tr_t15, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 15, 0.4).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(bitc_be_t3, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(bitc_be_t6, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 6, 0.46).
narrative_ontology:measurement(bitc_be_t9, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 9, 0.52).
narrative_ontology:measurement(bitc_be_t12, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 12, 0.56).
narrative_ontology:measurement(bitc_be_t15, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 15, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(bitc_su_t3, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 3, 0.34).
narrative_ontology:measurement(bitc_su_t6, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 6, 0.42).
narrative_ontology:measurement(bitc_su_t9, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 9, 0.47).
narrative_ontology:measurement(bitc_su_t12, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(bitc_su_t15, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 15, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__electronic_cash_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.12).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_whitepaper_purpose__store_of_value_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints decomposing the natural-language label 'the purpose of the Bitcoin whitepaper' per the ε-invariance principle: electronic_cash_reading (this file, tangled_rope, ε=0.58), store_of_value_reading (sibling file, expected lower ε or different classification given its decentralization-first premise), and nakamoto_oracle_opacity (sibling file, documenting the structural absence of an adjudicating authority rather than instantiating a reading itself). Each carries its own stable ε and stakeholder set; none averages over the others. The electronic_cash_reading and store_of_value_reading are mutually coexisting positions held by different factions of the same ongoing dispute — neither forecloses the other within a single framework, which is why their reading_relation is coexists_with rather than forecloses. The oracle_opacity constraint influences both readings downstream by removing the possibility of authoritative resolution, which is why it is marked influences rather than coexists_with from this reading's perspective.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
