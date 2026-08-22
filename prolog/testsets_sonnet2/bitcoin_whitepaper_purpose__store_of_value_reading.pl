% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper_purpose__store_of_value_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Bitcoin as Digital Gold: Decentralization/Verifiability-Primacy Reading of the Whitepaper Kernel
 *   domain: distributed_systems/monetary_theory/technology_governance
 *
 * SUMMARY:
 *   This story instantiates the store_of_value_reading of the contested
 *   bitcoin_whitepaper_purpose kernel: the claim that decentralization and
 *   full-node verifiability are the binding constraints on the system, with
 *   on-chain transactional capacity properly subordinated to preserving those
 *   properties. Under this reading, the 2017 retention of the 1MB (later
 *   ~effective 2-4MB with SegWit) block-size limit was not a betrayal of the
 *   whitepaper but a defense of its actual founding property —
 *   permissionless, cheap, independent validation. The sibling
 *   electronic_cash_reading, which reads the whitepaper's title as binding
 *   the system to low-fee everyday transactional use, is a separate
 *   constraint (not represented here) with its own ε and its own victim set.
 *   This story does not average across the two readings or hedge ε between
 *   them; it evaluates the decentralization-primacy arrangement, as this
 *   reading's own proponents understand it, against the users it prices off
 *   the base layer.
 *
 * KEY AGENTS:
 *   - long_term_holders: Primary beneficiary (organized/arbitrage) — asset appreciation from base-layer scarcity
 *   - full_node_operators: Beneficiary and co-agenda-setter (organized/mobile) — cheap verification preserved by capped block size
 *   - core_development_faction: Agenda-setter (institutional/arbitrage) — controls reference client, froze block-size parameter
 *   - low_value_onchain_users: Primary target (powerless/constrained) — priced off base layer by fee markets
 *   - remittance_senders: Target (powerless/trapped) — lose the low-fee cross-border value proposition during congestion
 *   - unbanked_populations_seeking_cash_substitute: Target (powerless/trapped) — lack technical access to off-chain compensating layers
 *   - electronic_cash_reading_advocates: Excluded rival faction (organized/constrained) — lost the block-size wars, forked off
 *   - protocol_researchers: Analytical observer — studies the trilemma tradeoffs without a stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__store_of_value_reading, 0.58).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__store_of_value_reading, 0.42).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__store_of_value_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__store_of_value_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__store_of_value_reading, "Bitcoin as Digital Gold: Decentralization/Verifiability-Primacy Reading of the Whitepaper Kernel").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__store_of_value_reading, "distributed_systems/monetary_theory/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__store_of_value_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__store_of_value_reading, '1032fd39-b22c-43ad-bc78-37a1cef9d78c').
narrative_ontology:cs_kernel_codification('1032fd39-b22c-43ad-bc78-37a1cef9d78c', fixed_text).
narrative_ontology:cs_authority_grounding('1032fd39-b22c-43ad-bc78-37a1cef9d78c', practice).
narrative_ontology:cs_interpretation_layer_present('1032fd39-b22c-43ad-bc78-37a1cef9d78c').
narrative_ontology:cs_reading_relation('1032fd39-b22c-43ad-bc78-37a1cef9d78c', bitcoin_whitepaper_purpose__electronic_cash_reading, coexists_with).
narrative_ontology:cs_axiom('1032fd39-b22c-43ad-bc78-37a1cef9d78c', foundational, verifiability_is_the_load_bearing_property).
narrative_ontology:cs_axiom_status(verifiability_is_the_load_bearing_property, holdable).
narrative_ontology:cs_axiom_grounding('1032fd39-b22c-43ad-bc78-37a1cef9d78c', verifiability_is_the_load_bearing_property, instrumental).
narrative_ontology:cs_axiom('1032fd39-b22c-43ad-bc78-37a1cef9d78c', secondary, onchain_capacity_is_subordinate_not_definitional).
narrative_ontology:cs_axiom_status(onchain_capacity_is_subordinate_not_definitional, holdable).
narrative_ontology:cs_axiom_grounding('1032fd39-b22c-43ad-bc78-37a1cef9d78c', onchain_capacity_is_subordinate_not_definitional, conventional).
narrative_ontology:cs_reference_frame('1032fd39-b22c-43ad-bc78-37a1cef9d78c', genesis_block_permissionless_validation_ideal).
narrative_ontology:cs_drift_state('1032fd39-b22c-43ad-bc78-37a1cef9d78c', post_block_size_wars_2017, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1032fd39-b22c-43ad-bc78-37a1cef9d78c', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, full_node_operators).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, core_development_faction).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, low_value_onchain_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, remittance_senders).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, unbanked_populations_seeking_cash_substitute).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold bitcoin as a savings instrument rather than a transactional medium. Benefit directly from policies that keep the base layer scarce, slow, and maximally decentralized, since these properties are read as what preserves long-run monetary integrity and censorship-resistance. Face no cost from small blocks because they transact rarely; their exit from the ecosystem is easy (sell) but their exit from the internal governance debate is unnecessary since the status quo already serves them.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_holders, beneficiary,
    organized, civilizational, arbitrage, global).

% Run validating nodes on consumer-grade hardware and treat the ability to do so cheaply as the non-negotiable precondition for a trustless system. Their capacity to verify the chain independently is what they believe decentralization actually means, and they resist any block-size increase that would price them out of validation, since that would concentrate verification among fewer, wealthier node operators.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, full_node_operators, beneficiary,
    organized, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__store_of_value_reading, full_node_operators, agenda_setter).

% Maintains the reference client and sets the technical roadmap; froze the block-size parameter and directed engineering resources toward Lightning Network and other off-chain layers rather than on-chain capacity expansion. Frames this as fidelity to the system's decentralization requirement, not as a policy choice with distributional consequences. Controls what code most of the network runs, giving it outsized influence over which reading of the whitepaper becomes operative infrastructure.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, core_development_faction, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Want to make small, everyday purchases directly on-chain. Find that fee markets driven by capped block space regularly price transactions of a few dollars out of economic sense, forcing them either onto custodial exchanges, onto Lightning Network (which requires channel management, liquidity, and technical sophistication they may lack), or out of the system entirely. Their only real exit is to stop using bitcoin as a payment medium and treat it purely as an asset, or to use an altcoin, which forfeits network effects and security.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, low_value_onchain_users, payer,
    powerless, immediate, constrained, global).

% Sought bitcoin as a lower-fee alternative to traditional remittance corridors for cross-border transfers to family. Small on-chain transactions during periods of network congestion can cost more in fees than the amount being sent, or require multi-day confirmation waits, eliminating the original value proposition. Often lack the technical setup for Lightning and are geographically and economically bound to whatever payment rail exists at either end.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, remittance_senders, payer,
    powerless, biographical, trapped, global).

% Lack access to traditional banking and were an implied audience of the whitepaper's 'electronic cash' framing. In practice, on-chain fee volatility and the technical complexity of off-chain layers make bitcoin impractical as a peer-to-peer cash substitute for people without reliable internet, technical literacy, or capital to lock into Lightning channels. They bear the gap between the promise and the delivered system without having participated in the governance decisions that produced it.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, unbanked_populations_seeking_cash_substitute, payer,
    powerless, generational, trapped, regional).

% Argue the whitepaper's title and stated purpose bind the system to low-fee, high-volume, peer-to-peer transactional use, and that the 2017 block-size limitation was a capture of the protocol's direction away from its founding text. Lost the block-size wars (2015-2017) and largely forked off into separate chains (e.g., Bitcoin Cash) rather than remaining as an internal faction with governance power over the dominant chain. Their reading persists as a live rival claim but has no institutional lever inside the reference-client governance process.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, electronic_cash_reading_advocates, excluded,
    organized, civilizational, constrained, global).

% Study the trilemma tradeoffs between decentralization, security, and scalability; publish analysis of node-count trends, fee-market dynamics, and Lightning Network adoption and liquidity constraints without holding a stake in either reading's victory.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, protocol_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper_purpose__store_of_value_reading, diffuse).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper_purpose__store_of_value_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global set of independent validators around a shared ledger by keeping the resource cost of full verification low enough that a broad, geographically and politically diverse population can run nodes, preventing the validation function from concentrating among a small set of well-capitalized operators.
% TRANSFER_FUNCTION: Moves transactional access and low-cost settlement away from users who need frequent small-value on-chain transfers and toward long-term holders and node operators who are structurally indifferent to base-layer fee levels; on-chain scarcity is transferred into asset appreciation for holders and into continued cheap verifiability for node operators.
% ABSENT_VOICES: Populations who cannot access or afford off-chain layers (Lightning channel liquidity, technical setup, reliable connectivity) are structurally absent from the governance conversation that set this tradeoff; the electronic_cash_reading faction that would represent their transactional interest was present during the block-size debates but lost and largely exited to separate chains rather than retaining a voice in the dominant network's governance.
% DISAPPEARANCE_RATIONALE: If the decentralization/verifiability-primacy reading were abandoned tomorrow in favor of large on-chain blocks, long-term holders and node operators would say the system's core trust property collapses (validation concentrates, the chain becomes auditable only by well-resourced parties, defeating the point of a trustless ledger). Low-value users and remittance senders would say the system finally became usable as cash. The disagreement is exactly the kernel contest itself, not resolvable from within either reading alone.
% FOUNDING_PROBLEM: The original problem was building a peer-to-peer electronic cash system that eliminates the need for a trusted third party in transactions — solving double-spending without a central authority.
% FOUNDING_PROBLEM_CORROBORATION: Long-term holders and core developers attest that the founding problem, correctly read, was always about trust-minimization and censorship-resistance, of which cheap verifiability is the load-bearing mechanism, not an add-on. Electronic_cash_reading advocates and several early mailing-list participants (including quoted correspondence from Nakamoto describing scaling paths via larger blocks) attest the founding problem was low-friction electronic cash and that this reading represents a later reinterpretation adopted after the fact by parties who benefit from scarcity. No party outside the two contesting factions has a disinterested account of what Nakamoto, who vanished from the project in 2011, actually intended.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__store_of_value_reading, contested).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__store_of_value_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__store_of_value_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__store_of_value_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper_purpose__store_of_value_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.58) reflects that a real coordination good (permissionless, cheap validation) is being delivered, but its delivery mechanism imposes a substantial, non-incidental cost on a specific, identifiable population (low-value on-chain users) as the direct consequence of a governance choice, not an unavoidable technical necessity — the block-size ceiling is a policy lever, not a law of physics. Suppression (0.42) is moderate: there is no legal prohibition on using bitcoin for small payments, but fee-market dynamics function as an economic suppression mechanism, and the reference-client governance process structurally forecloses the rival reading from regaining influence over the dominant chain. Theater ratio (0.28) captures that some of the 'decentralization defense' rhetoric in ongoing debates now serves factional legitimacy maintenance as much as it serves the original technical concern, though the underlying verifiability function remains substantially real, not merely performed. Accessibility collapse (0.55) is moderate rather than high — Lightning Network, sidechains, and other chains remain technically available alternatives, but they carry real switching costs, and for the populations least equipped to absorb those costs the practical collapse is closer to complete. Resistance (0.72) is high: the electronic_cash_reading faction fought this outcome directly in the 2015-2017 block-size wars, and the disagreement remains structurally live rather than settled.
 *
 * DIRECTIONALITY LOGIC:
 *   Long-term holders and full-node operators are declared beneficiaries because the policy's costs (foreclosed on-chain capacity) do not fall on them in proportion to how they use the system — holders rarely transact, and operators benefit directly from cheap validation. Low-value on-chain users, remittance senders, and unbanked populations are declared victims because the same capped-capacity policy translates directly into fee-market exclusion for exactly the transaction sizes and frequencies they need. The core_development_faction sits as agenda_setter rather than pure beneficiary because its stake is institutional control over protocol direction rather than direct financial extraction, though its incentives align closely with the holder/operator coalition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem status is authored as contested rather than dead precisely because both readings can coherently claim continuity with the original text — this is not a case of an arrangement that has plainly outlived a settled purpose, but one where the purpose itself was never settled and two governance factions each claim to be the legitimate heir. Declaring mandatrophy resolved in either direction would overclaim; the honest state is an unresolved kernel dispute with real distributional consequences on one side of the ledger.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Is the store_of_value_reading the whitepaper''s actual binding constraint, or is it a later reinterpretation that became dominant because it serves the interests of an entrenched holder/developer coalition, with the electronic_cash_reading being the more textually faithful original intent?',
    'Nakamoto''s disappearance in 2011 (see the sibling nakamoto_oracle_opacity constraint) means there is no living authoritative source to resolve this; the closest available evidence is contemporaneous mailing-list correspondence and the whitepaper text itself, both of which are cited by each faction in support of its own reading.',
    'If the electronic_cash_reading is the textually correct one, this constraint''s claimed_type and beneficiary/victim structure represent a governance capture rather than a faithful implementation of founding intent, which would sharpen the extraction reading considerably. If the store_of_value_reading is correct, the current arrangement is closer to a genuine (if costly) coordination mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether decentralization-primacy or cash-primacy is the textually and historically correct reading of the whitepaper kernel.').

omega_variable(
    lightning_network_adequacy,
    'Does Lightning Network functionally substitute for on-chain small-value transactions for the populations priced off the base layer, or does its liquidity, routing, and technical-setup requirements reproduce the same exclusion at a different layer?',
    'Empirical study of Lightning adoption rates, channel liquidity distribution, and usability among the specific populations (remittance senders, unbanked users) claimed to be served by the off-chain scaling solution.',
    'If Lightning genuinely closes the gap for these populations, the victim classification softens substantially — the extraction becomes a temporary friction rather than a durable exclusion. If Lightning reproduces exclusion for the least technically resourced users, the victim classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lightning_network_adequacy, empirical, 'Whether off-chain scaling actually reaches the users priced off the base layer.').

omega_variable(
    decentralization_measurement_ambiguity,
    'Does keeping blocks small actually preserve meaningful decentralization, given that mining power and node hosting have both concentrated substantially regardless of the block-size policy?',
    'Longitudinal analysis of node count, node geographic/political distribution, and mining pool concentration correlated against block-size and fee-market history.',
    'If decentralization has concentrated anyway despite the small-block policy, the coordination justification for excluding low-value users weakens substantially, moving the classification toward a less defensible extraction. If small blocks demonstrably preserved a materially more distributed validator set than large blocks would have, the coordination function is more clearly genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralization_measurement_ambiguity, empirical, 'Whether the block-size policy actually delivers the decentralization it is justified by.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__store_of_value_reading, 2009, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t2009, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2009, 0.05).
narrative_ontology:measurement(bitc_tr_t2013, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2013, 0.08).
narrative_ontology:measurement(bitc_tr_t2015, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(bitc_tr_t2017, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2017, 0.25).
narrative_ontology:measurement(bitc_tr_t2020, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2020, 0.27).
narrative_ontology:measurement(bitc_tr_t2024, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(bitc_be_t2009, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2009, 0.1).
narrative_ontology:measurement(bitc_be_t2013, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2013, 0.18).
narrative_ontology:measurement(bitc_be_t2015, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2015, 0.3).
narrative_ontology:measurement(bitc_be_t2017, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2017, 0.52).
narrative_ontology:measurement(bitc_be_t2020, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement(bitc_be_t2024, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t2009, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2009, 0.05).
narrative_ontology:measurement(bitc_su_t2013, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2013, 0.1).
narrative_ontology:measurement(bitc_su_t2015, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2015, 0.28).
narrative_ontology:measurement(bitc_su_t2017, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2017, 0.4).
narrative_ontology:measurement(bitc_su_t2020, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2020, 0.41).
narrative_ontology:measurement(bitc_su_t2024, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__store_of_value_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper_purpose__store_of_value_reading, 0.12).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose__electronic_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity).

% DUAL FORMULATION NOTE:
% This story is one of at least three linked constraints decomposing the natural-language label 'the purpose of the Bitcoin whitepaper' (per the ε-invariance principle): store_of_value_reading (this file, tangled_rope, ε=0.58, victims are low-value on-chain users), electronic_cash_reading (sibling file, expected higher ε and a differently structured victim/beneficiary set centered on the users this reading would have served), and nakamoto_oracle_opacity (sibling file, modeling the interpretive vacuum itself rather than either substantive reading). Each carries its own stable ε and its own stakeholder set; they are linked, not merged, because measuring 'the whitepaper's purpose' one way (decentralization-primacy) versus another way (cash-primacy) yields different beneficiary/victim structures and different extraction levels — the signature of two distinct constraints sharing a contested kernel, not one constraint with an ambiguous measurement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
