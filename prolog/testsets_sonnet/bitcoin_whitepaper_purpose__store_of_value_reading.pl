% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper_purpose__store_of_value_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bitcoin_whitepaper_purpose__store_of_value_reading
 *   human_readable: Bitcoin Block-Size Constraint (Store-of-Value / Settlement-Layer Reading)
 *   domain: distributed_systems/monetary_theory/technology_governance
 *
 * SUMMARY:
 *   This story instantiates the store-of-value / settlement-layer reading of
 *   the Bitcoin whitepaper's founding kernel: decentralization and full-node
 *   verifiability are treated as the binding design constraints, and limited
 *   on-chain capacity (the retained ~1MB-equivalent block weight limit) is a
 *   deliberate subordination of transactional throughput to that goal. Under
 *   this reading, the 2017 block-size wars resolved correctly in favor of
 *   small blocks plus off-chain scaling (Lightning), and the beneficiaries
 *   are long-term holders and node operators who value censorship-resistant,
 *   verifiable settlement over cheap frequent payments. This is a distinct
 *   constraint from the electronic_cash_reading sibling, which treats the
 *   whitepaper's 'peer-to-peer electronic cash' framing as binding and would
 *   find the same capacity limit straightforwardly extractive against the
 *   stated purpose. The two readings are not the same constraint measured two
 *   ways — they have different beneficiary sets, different ε, and different
 *   classifications, and are linked via network.affects_constraints rather
 *   than merged.
 *
 * KEY AGENTS:
 *   - long_term_holders: primary beneficiary (organized/mobile) — collects decentralization/security premium
 *   - full_node_operators: beneficiary and agenda-setter (organized/arbitrage) — sets consensus rules through client adoption
 *   - mining_pool_operators: beneficiary (institutional/arbitrage) — collects scarcity-driven fee revenue
 *   - low_value_onchain_users: primary payer (powerless/constrained) — priced off base layer
 *   - unbanked_users_without_lightning_access: primary victim (powerless/trapped) — excluded from advertised use case entirely
 *   - big_block_advocates: excluded party (organized/constrained) — lost the governance contest, largely exited to alternative chains
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__store_of_value_reading, 0.52).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__store_of_value_reading, 0.38).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__store_of_value_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__store_of_value_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__store_of_value_reading, "Bitcoin Block-Size Constraint (Store-of-Value / Settlement-Layer Reading)").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__store_of_value_reading, "distributed_systems/monetary_theory/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__store_of_value_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__store_of_value_reading, 'a71c4b51-3c68-448b-924a-2df68b4097f5').
narrative_ontology:cs_kernel_codification('a71c4b51-3c68-448b-924a-2df68b4097f5', fixed_text).
narrative_ontology:cs_authority_grounding('a71c4b51-3c68-448b-924a-2df68b4097f5', distributed).
narrative_ontology:cs_reading_relation('a71c4b51-3c68-448b-924a-2df68b4097f5', bitcoin_whitepaper_purpose__electronic_cash_reading, coexists_with).
narrative_ontology:cs_reading_relation('a71c4b51-3c68-448b-924a-2df68b4097f5', bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, influences).
narrative_ontology:cs_axiom('a71c4b51-3c68-448b-924a-2df68b4097f5', foundational, verifiability_by_ordinary_participants_is_binding).
narrative_ontology:cs_axiom_status(verifiability_by_ordinary_participants_is_binding, holdable).
narrative_ontology:cs_axiom_grounding('a71c4b51-3c68-448b-924a-2df68b4097f5', verifiability_by_ordinary_participants_is_binding, instrumental).
narrative_ontology:cs_axiom('a71c4b51-3c68-448b-924a-2df68b4097f5', secondary, capacity_growth_subordinate_to_decentralization).
narrative_ontology:cs_axiom_status(capacity_growth_subordinate_to_decentralization, holdable).
narrative_ontology:cs_axiom_grounding('a71c4b51-3c68-448b-924a-2df68b4097f5', capacity_growth_subordinate_to_decentralization, instrumental).
narrative_ontology:cs_reference_frame('a71c4b51-3c68-448b-924a-2df68b4097f5', decentralization_primacy_design).
narrative_ontology:cs_drift_state('a71c4b51-3c68-448b-924a-2df68b4097f5', post_segwit2x_settlement, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a71c4b51-3c68-448b-924a-2df68b4097f5', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, full_node_operators).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, mining_pool_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, low_value_onchain_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, remittance_senders).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, unbanked_users_without_lightning_access).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, lightning_network_operators).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper_purpose__store_of_value_reading, decentralization_primacy_doctrine).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper_purpose__store_of_value_reading, full_node_verifiability_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold bitcoin as a savings instrument and rarely transact on-chain. Benefit directly from the capacity constraint because it keeps the blockchain lean enough for widespread full-node verification, which they argue is what makes the asset trustworthy as a store of value. Their exit options are unaffected by fee levels since they transact infrequently.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_holders, beneficiary,
    organized, generational, mobile, global).

% Run the software that validates the chain and effectively sets consensus rules through what they choose to run. Benefit from small blocks because it keeps hardware and bandwidth requirements low enough for individuals to operate nodes without specialized infrastructure, preserving their veto power over protocol changes.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, full_node_operators, beneficiary,
    organized, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__store_of_value_reading, full_node_operators, agenda_setter).

% Collect transaction fees that rise as block space becomes scarce relative to demand. A constrained base layer increases fee revenue per byte, which benefits mining economics even as it prices out small transactions.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, mining_pool_operators, beneficiary,
    institutional, biographical, arbitrage, global).

% Want to make small, everyday payments directly on the base chain but find fees exceed transaction value during periods of congestion. Must either abandon the transaction, overpay, or migrate to a second layer (Lightning) that requires channel management, liquidity, and additional technical competence they may lack.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, low_value_onchain_users, payer,
    powerless, immediate, constrained, global).

% Sought bitcoin as a low-cost cross-border payment rail per the original whitepaper's stated purpose. Priced off the base layer by fee volatility and pushed toward custodial exchanges or Lightning services that reintroduce intermediaries and technical barriers the original design was meant to eliminate.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, remittance_senders, payer,
    powerless, immediate, trapped, global).

% Lack the smartphone infrastructure, always-on connectivity, or technical sophistication to run Lightning wallets and manage channel liquidity. For this group the base-layer capacity constraint is not a redirection to an equivalent alternative but an effective exclusion from the system's originally advertised use case.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, unbanked_users_without_lightning_access, payer,
    powerless, immediate, trapped, global).

% Operate routing nodes on the off-chain scaling layer built specifically because base-layer capacity was capped. Earn routing fees and depend structurally on base-layer scarcity remaining unresolved; a capacity increase that reduced base-layer fee pressure would reduce demand for their services.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, lightning_network_operators, beneficiary,
    organized, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__store_of_value_reading, lightning_network_operators, agenda_setter).

% Argued during the 2015-2017 scaling disputes that the whitepaper's 'peer-to-peer electronic cash' framing required raising the block-size limit to preserve low on-chain fees. Lost the contest (the 2017 UASF/SegWit2x conflict resolved in favor of small blocks) and largely exited to alternative chains (Bitcoin Cash) rather than remaining in the governance conversation.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, big_block_advocates, excluded,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper_purpose__store_of_value_reading, diffuse).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper_purpose__store_of_value_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps the resource requirements of full validation low enough that a geographically and economically diverse population of participants can run a full node, which is the mechanism that makes the ledger's history resistant to capture or rewriting by any concentrated set of actors.
% TRANSFER_FUNCTION: Moves transactional convenience away from users who need frequent, low-value, low-latency on-chain settlement and toward participants who value verifiability, censorship-resistance, and long-horizon custody; correspondingly moves fee revenue toward miners and routing revenue toward Lightning node operators during periods of base-layer congestion.
% ABSENT_VOICES: Populations without reliable internet access, smartphones, or the technical literacy to manage Lightning channels are not represented in protocol governance forums (which skew toward technically sophisticated, economically comfortable participants); their exclusion from the 'peer-to-peer cash' use case is rarely weighted in governance discussions dominated by holders and node operators.
% DISAPPEARANCE_RATIONALE: If the capacity constraint were removed (blocks made arbitrarily large), full-node operating costs would rise, plausibly concentrating validation among well-resourced entities — this is precisely the outcome the current design is built to prevent, so removal is contested as an improvement rather than agreed to rearrange the world for the better. Node operators and long-term holders would say the system's core security property degrades; users needing cheap on-chain payments would say the system finally serves its stated purpose.
% FOUNDING_PROBLEM: The original design needed to solve double-spending without a trusted third party while remaining verifiable by ordinary participants, and the whitepaper's title and abstract framed the deliverable as a payment system ('peer-to-peer electronic cash') that avoids the cost and censorship exposure of financial institutions.
% FOUNDING_PROBLEM_CORROBORATION: Node operators and long-term holders (the constraint's beneficiaries) attest that decentralized verifiability, not transactional throughput, was always the load-bearing design goal, citing the whitepaper's emphasis on proof-of-work and honest-node majority assumptions. Independent economists and payments researchers outside the beneficiary set (e.g. central bank working papers, academic fee-market studies) corroborate that base-layer transaction costs now regularly exceed the value of small payments, documenting the shift away from the cash-payment use case as an observed empirical fact rather than a partisan claim — though they do not adjudicate whether this shift is a failure or a feature.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__store_of_value_reading, contested).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__store_of_value_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__store_of_value_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__store_of_value_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper_purpose__store_of_value_reading, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is moderate (0.52) rather than severe because the constraint genuinely does protect a real coordination good (decentralized verifiability) even as it imposes real costs on a specific population (low-value on-chain users). Suppression is moderate (0.38) and reflects the social/technical enforcement of the block-size limit through client-software consensus (nodes running non-compliant software are simply ignored by the network) rather than legal coercion — enforcement is structural-technical, not punitive. Theater ratio is low-to-moderate (0.22) because the stated coordination function (node accessibility, decentralization) is substantially real, not primarily performative, though some rhetorical emphasis on 'digital gold' framing post-dates and partly rationalizes the capacity decision rather than purely predating it.
 *
 * PERSPECTIVAL GAP:
 *   From the full-node-operator seat, the block-size limit reads as a Rope or even Mountain-adjacent necessity — an irreducible tradeoff imposed by the physics of global bandwidth distribution and Byzantine fault tolerance. From the low-value-on-chain-user or unbanked-user seat, the same limit reads as a Snare-like exclusion: a promise of low-cost peer-to-peer cash that was structurally withdrawn once an alternative beneficiary class (holders, node operators, and the parties who profit from base-layer scarcity) gained governance control. The Tangled Rope classification holds both readings together: genuine coordination function (decentralization) and asymmetric extraction (excluded low-value users) coexist in the same structure, with active enforcement (client-software consensus, social pressure against big-block forks) required to hold it.
 *
 * DIRECTIONALITY LOGIC:
 *   Long-term holders and full-node operators sit near the beneficiary end of directionality: the constraint subsidizes exactly the property they value (verifiability, scarcity-driven store-of-value premium) and costs them little since they transact infrequently on-chain. Low-value on-chain users and the unbanked without Lightning access sit near the full-target end: they bear the cost (being priced off or excluded) without capturing the compensating benefit, and their exit options are genuinely constrained or trapped — Lightning is not a costless substitute for populations lacking the technical infrastructure to use it. Mining pool operators and Lightning operators are structurally beneficiary but for a different reason: capacity scarcity is the source of their revenue, which creates a coordination-adjacent incentive to resist capacity increases that is independent of the decentralization rationale.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored as contested rather than dead: unlike a pure mandatrophy case, the decentralization/verifiability problem this constraint solves has not disappeared — node-operation costs are a live, ongoing concern, and the constraint continues to perform a genuine function. What prevents this from being simple mandatrophy is that there IS a real coordination good being defended (unlike a piton, where no one benefits enough to maintain it); what prevents dismissing it as pure Rope is the identifiable, structurally excluded victim class whose original use case was the very thing the whitepaper foregrounded in its title.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    whitepaper_telos_indeterminacy,
    'Does the whitepaper''s title and abstract (''Bitcoin: A Peer-to-Peer Electronic Cash System'') establish a binding design telos that the current capacity constraint violates, or is decentralized verifiability the deeper, load-bearing design commitment of which the cash-payment framing was merely an early illustrative use case?',
    'No definitive resolution mechanism exists: Satoshi Nakamoto''s 2011 disappearance eliminated the one party who could authoritatively adjudicate original intent. Resolution would require either recovered private communications establishing design priority, or a community-wide governance consensus (unlikely given the 2017 fork outcome already reflects one such attempt) accepted as authoritative by all factions.',
    'If the electronic-cash telos is treated as binding, this constraint''s coordination-function claim weakens substantially and the classification shifts toward snare (extraction from the cash-use-case population with the decentralization rationale as post-hoc cover). If the decentralization telos is treated as binding and cash-payment was illustrative, the tangled_rope classification with genuine coordination function is well-supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(whitepaper_telos_indeterminacy, conceptual, 'Irreducible indeterminacy in which whitepaper framing (title/abstract vs. technical design constraints) constitutes the binding kernel.').

omega_variable(
    lightning_adequacy_as_substitute,
    'Is the Lightning Network a genuinely adequate substitute for on-chain low-value payments (such that excluded users have a real, low-cost alternative and the base-layer constraint imposes only modest net cost), or does it reintroduce liquidity, custodial, and technical-competence barriers that functionally exclude the same population the base layer was meant to serve?',
    'Empirical study of Lightning adoption rates, channel liquidity failures, and routing-fee volatility among low-income and technically unsophisticated user populations, compared against stated base-layer use-case populations from the 2009-2013 era.',
    'If Lightning is adequate, the victim population shrinks substantially and effective extraction is lower than the raw base-layer fee data suggests. If Lightning is not adequate for the excluded population, effective extraction and the victim set are understated by base-layer metrics alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lightning_adequacy_as_substitute, empirical, 'Whether the off-chain scaling layer offered as compensation for base-layer constraints is a real substitute or a nominal one for the specific population it is meant to serve.').

omega_variable(
    governance_capture_vs_genuine_consensus,
    'Did the 2017 resolution in favor of small blocks reflect a genuine rough consensus of the network''s economic stakeholders, or did it reflect the disproportionate influence of a specific coalition (core developers, established node operators, exchanges) whose economic interests aligned with scarcity-driven fee and store-of-value premiums?',
    'Historical analysis of the UASF/SegWit2x conflict''s actual participant composition, funding sources for competing implementations, and exchange/miner signaling behavior during the dispute, compared against the demographic composition of excluded populations.',
    'If genuine consensus, the tangled_rope''s coordination function is more robustly grounded. If capture, the classification moves closer to snare with the coordination language functioning as post-hoc legitimation of a beneficiary-driven outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(governance_capture_vs_genuine_consensus, conceptual, 'Whether the small-block resolution represents authentic multi-stakeholder coordination or capture by a coalition with aligned scarcity interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__store_of_value_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(bitc_tr_t0, observed).
narrative_ontology:measurement(bitc_tr_t3, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 3, 0.08).
narrative_ontology:measurement_basis(bitc_tr_t3, observed).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 6, 0.13).
narrative_ontology:measurement_basis(bitc_tr_t6, observed).
narrative_ontology:measurement(bitc_tr_t8, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement_basis(bitc_tr_t8, observed).
narrative_ontology:measurement(bitc_tr_t11, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 11, 0.2).
narrative_ontology:measurement_basis(bitc_tr_t11, observed).
narrative_ontology:measurement(bitc_tr_t15, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement_basis(bitc_tr_t15, observed).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(bitc_be_t0, observed).
narrative_ontology:measurement(bitc_be_t3, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 3, 0.2).
narrative_ontology:measurement_basis(bitc_be_t3, observed).
narrative_ontology:measurement(bitc_be_t6, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 6, 0.34).
narrative_ontology:measurement_basis(bitc_be_t6, observed).
narrative_ontology:measurement(bitc_be_t8, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement_basis(bitc_be_t8, observed).
narrative_ontology:measurement(bitc_be_t11, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 11, 0.5).
narrative_ontology:measurement_basis(bitc_be_t11, observed).
narrative_ontology:measurement(bitc_be_t15, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement_basis(bitc_be_t15, observed).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(bitc_su_t0, observed).
narrative_ontology:measurement(bitc_su_t3, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 3, 0.22).
narrative_ontology:measurement_basis(bitc_su_t3, observed).
narrative_ontology:measurement(bitc_su_t6, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 6, 0.4).
narrative_ontology:measurement_basis(bitc_su_t6, observed).
narrative_ontology:measurement(bitc_su_t8, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement_basis(bitc_su_t8, observed).
narrative_ontology:measurement(bitc_su_t11, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 11, 0.37).
narrative_ontology:measurement_basis(bitc_su_t11, observed).
narrative_ontology:measurement(bitc_su_t15, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 15, 0.38).
narrative_ontology:measurement_basis(bitc_su_t15, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__store_of_value_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper_purpose__store_of_value_reading, 0.18).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose__electronic_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity).

% DUAL FORMULATION NOTE:
% This story and bitcoin_whitepaper_purpose__electronic_cash_reading are sibling readings of the same kernel (bitcoin_whitepaper_purpose), not the same constraint under two observables. This reading takes decentralization/verifiability as binding and produces ε=0.52 with beneficiaries = holders/node operators and victims = low-value on-chain users; the electronic_cash sibling takes the payment-system framing as binding and would be expected to produce a different, likely higher, ε with a different victim-weighting. Both are downstream of the interpretive vacuum documented in nakamoto_oracle_opacity, which is not itself a capacity-allocation constraint but the meta-fact that no authoritative interpreter exists to resolve which reading is 'correct.' Do not average or merge these three stories' ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
