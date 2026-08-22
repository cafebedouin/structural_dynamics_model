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
 *   human_readable: Bitcoin Block-Size Constraint — Store-of-Value / Digital-Gold Reading
 *   domain: distributed_systems/monetary_theory/technology_governance
 *
 * SUMMARY:
 *   This story instantiates the store-of-value reading of the Bitcoin
 *   whitepaper purpose kernel: decentralization and full-node verifiability
 *   are the binding constraints the protocol exists to protect, and on-chain
 *   transactional capacity is legitimately subordinated to them. Under this
 *   reading, the persistent 1MB-era block-size limit (and its successor
 *   SegWit/witness-discount regime) is not a bug or a captured decision but
 *   the correct application of the founding priority — cheap,
 *   widely-distributable verification over cheap, high-throughput payments.
 *   The 2015-2017 block-size wars are read as the moment this priority was
 *   defended against a electronic-cash-reading coalition that wanted to scale
 *   on-chain capacity directly. This is a DISTINCT constraint from the
 *   electronic_cash_reading sibling story, which authors a different ε for
 *   the same underlying kernel text under the premise that the 'cash' telos
 *   in the title is binding and low-fee everyday transactional use is the
 *   non-negotiable design target. The two stories are not the same constraint
 *   measured two ways; they are different constraints instantiated from the
 *   same contested kernel, each with its own stable ε, beneficiary/victim
 *   structure, and classification, linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - long_term_holders: Primary beneficiary (organized/mobile) — scarcity narrative and cheap self-verification protected
 *   - full_node_operators: Agenda-setter (organized/mobile) — social-consensus veto power over block-size changes
 *   - mining_pool_incumbents: Secondary beneficiary (powerful/constrained) — fee-market rents from scarce blockspace
 *   - lightning_network_infrastructure_operators: Beneficiary (moderate/mobile) — commercial niche created by base-layer scarcity
 *   - low_value_onchain_users: Primary target (powerless/trapped) — priced off base-layer payments
 *   - remittance_senders: Target (powerless/trapped) — cross-border low-fee use case degraded
 *   - unbanked_would_be_users: Excluded (powerless/trapped) — financial-inclusion pitch structurally deprioritized
 *   - block_size_reform_advocates: Excluded (organized/arbitrage) — lost the governance contest, exited to other chains
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
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__store_of_value_reading, "Bitcoin Block-Size Constraint — Store-of-Value / Digital-Gold Reading").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__store_of_value_reading, "distributed_systems/monetary_theory/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__store_of_value_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__store_of_value_reading, '5a953e5f-b0d8-4fa1-bff2-e1316037f208').
narrative_ontology:cs_kernel_codification('5a953e5f-b0d8-4fa1-bff2-e1316037f208', fixed_text).
narrative_ontology:cs_authority_grounding('5a953e5f-b0d8-4fa1-bff2-e1316037f208', practice).
narrative_ontology:cs_interpretation_layer_present('5a953e5f-b0d8-4fa1-bff2-e1316037f208').
narrative_ontology:cs_reading_relation('5a953e5f-b0d8-4fa1-bff2-e1316037f208', bitcoin_whitepaper_purpose__electronic_cash_reading, coexists_with).
narrative_ontology:cs_reading_relation('5a953e5f-b0d8-4fa1-bff2-e1316037f208', bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, influences).
narrative_ontology:cs_axiom('5a953e5f-b0d8-4fa1-bff2-e1316037f208', foundational, verification_accessibility_is_the_load_bearing_property).
narrative_ontology:cs_axiom_status(verification_accessibility_is_the_load_bearing_property, holdable).
narrative_ontology:cs_axiom_grounding('5a953e5f-b0d8-4fa1-bff2-e1316037f208', verification_accessibility_is_the_load_bearing_property, instrumental).
narrative_ontology:cs_axiom('5a953e5f-b0d8-4fa1-bff2-e1316037f208', secondary, monetary_premium_depends_on_credible_scarcity_and_self_custody).
narrative_ontology:cs_axiom_status(monetary_premium_depends_on_credible_scarcity_and_self_custody, holdable).
narrative_ontology:cs_axiom_grounding('5a953e5f-b0d8-4fa1-bff2-e1316037f208', monetary_premium_depends_on_credible_scarcity_and_self_custody, empirically_contingent).
narrative_ontology:cs_reference_frame('5a953e5f-b0d8-4fa1-bff2-e1316037f208', cpu_verification_decentralization_primacy).
narrative_ontology:cs_drift_state('5a953e5f-b0d8-4fa1-bff2-e1316037f208', post_block_size_wars_2017, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5a953e5f-b0d8-4fa1-bff2-e1316037f208', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, full_node_operators).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, mining_pool_incumbents).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, lightning_network_infrastructure_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, low_value_onchain_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, remittance_senders).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, unbanked_would_be_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold bitcoin as a savings instrument and rarely transact on-chain. Benefit directly from constrained block space because it preserves the scarcity narrative, keeps full-node operation cheap (protecting their ability to self-verify holdings), and insulates the asset's monetary premium from transactional congestion. They can exit into other assets if the network degrades, but have strong incentive to defend the current parameter regime.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_holders, beneficiary,
    organized, generational, mobile, global).

% Run consumer-hardware-compatible nodes and treat their ability to independently verify the entire chain as the non-negotiable core property of the system. They set the de facto agenda through social consensus and refusal to run client software that raises block-size limits, having decisively blocked larger-block proposals in 2015-2017. They benefit from low verification costs; they bear no direct transactional cost since most are not high-volume on-chain payers.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, full_node_operators, agenda_setter,
    organized, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__store_of_value_reading, full_node_operators, beneficiary).

% Earn transaction fees driven higher by scarce block space during demand spikes, and benefit from a predictable, unchanging protocol that has not required them to re-tool for larger blocks or altered mining economics. Their exit options are constrained by sunk capital in ASIC hardware tuned to the existing chain.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, mining_pool_incumbents, beneficiary,
    powerful, biographical, constrained, global).

% Operate payment channels that only exist as a business because on-chain capacity is scarce and expensive; the constrained base layer is the market condition that created their entire commercial niche. They route the low-fee, high-frequency transaction volume the base layer no longer efficiently serves.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, lightning_network_infrastructure_operators, beneficiary,
    moderate, biographical, mobile, global).

% Want to make small, everyday on-chain payments as the whitepaper's title describes but are priced off the base layer during fee spikes, sometimes paying more in fees than the value transferred. Their alternative is adopting Lightning (added complexity, liquidity/routing failure risk, custodial-wallet compromise) or abandoning bitcoin as a payment method entirely.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, low_value_onchain_users, payer,
    powerless, immediate, trapped, global).

% Sought bitcoin as a low-cost cross-border payment rail, a use case explicitly marketed in the network's early years. Fee volatility and unpredictable confirmation times under blockspace scarcity now make small remittances impractical on-chain, pushing them toward centralized exchanges or abandoning the network as a payment tool.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, remittance_senders, payer,
    powerless, immediate, trapped, global).

% Populations without banking access who were part of the original electronic-cash pitch for financial inclusion. They are not present in protocol-governance debates (developer mailing lists, mining pool coordination, node-operator social consensus) and their transactional use case has been structurally deprioritized without their direct representation.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, unbanked_would_be_users, excluded,
    powerless, biographical, trapped, global).

% Argued (2015-2017 block-size wars) for raising the base-layer capacity limit to preserve the electronic-cash use case; lost the governance contest and largely exited to alternative chains (Bitcoin Cash and successors), taking transactional-use development effort with them. Their departure removed the loudest structural counter-pressure from the ongoing bitcoin governance conversation.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, block_size_reform_advocates, excluded,
    organized, biographical, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper_purpose__store_of_value_reading, diffuse).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper_purpose__store_of_value_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a large, geographically dispersed set of independent verifiers around a shared ledger by keeping the resource cost of full verification low enough that ordinary participants can run a node on consumer hardware, which is what makes the chain's history trustlessly auditable rather than dependent on a small set of well-resourced validators.
% TRANSFER_FUNCTION: Moves transactional capacity and fee-market rents away from low-value, high-frequency on-chain users and toward long-term holders (who gain a scarcer, more verifiable settlement layer), node operators (who retain cheap verification), and off-chain infrastructure operators (who capture the displaced transaction volume as a service).
% ABSENT_VOICES: Unbanked populations targeted by the original financial-inclusion pitch, and the block-size reform coalition that argued for on-chain scaling and lost the 2015-2017 governance contest and mostly exited to other chains, are structurally outside the ongoing node-operator social-consensus process that sets the parameter.
% DISAPPEARANCE_RATIONALE: If the small-block, decentralization-first constraint disappeared (blocks raised substantially), full-node operating costs would rise, some node operators would drop off (reducing verification decentralization), but on-chain transaction fees would likely fall, restoring practical utility for small payments. Long-term holders and the digital-gold narrative would face genuine reputational and functional disruption; Lightning infrastructure's commercial rationale would partially erode. Whether this counts as 'the world rearranges' or 'the world stays roughly the same' is exactly the underlying kernel dispute — hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: The whitepaper was written to solve double-spending without a trusted third party, enabling peer-to-peer electronic cash; the store-of-value reading holds that the deeper, load-bearing founding problem was actually preserving decentralized, permissionless verification against the resource concentration that unconstrained scaling would produce.
% FOUNDING_PROBLEM_CORROBORATION: Long-term holders and node-operator communities (Bitcoin Core-aligned developers, node-running enthusiasts) attest the decentralization-preservation problem remains live and was always the primary founding problem, citing Nakamoto's own emphasis on node count and CPU-verification in the whitepaper and forum posts. Independent technical historians and departed block-size-reform developers dispute this reading, attesting instead that the whitepaper's stated purpose ('electronic cash') was transactional and that decentralization-primacy was a later interpretive overlay adopted after Nakamoto's departure; no single source outside the current beneficiary coalition (long-term holders, node operators) corroborates the store-of-value reading as the ORIGINAL founding problem rather than a retrospective reconstruction.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__store_of_value_reading, contested).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__store_of_value_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__store_of_value_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored at 0.52 (moderate-high) because the constraint does move real transactional value away from low-value on-chain users toward holders and infrastructure operators, but the coordination function (decentralized full-node verification) is genuine and not merely cover — this is why the claim is tangled_rope rather than snare. Suppression is authored lower (0.38) than extraction because the mechanism is largely social-consensus and market-based (fee auctions, node-software adoption) rather than coercive; users are not legally barred from on-chain transactions, they are priced out. Theater ratio is modest (0.22) — the decentralization function is substantively real (measurable in node count, geographic distribution, and hardware requirements), not primarily performative, though some post-2017 rhetoric around 'digital gold' functions partly as narrative reinforcement rather than technical necessity. Accessibility collapse is moderate (0.45): Lightning and other Layer-2 options exist and function as partial alternatives, so the collapse is not total. Resistance is meaningfully high (0.58), reflecting the sustained, organized pushback from the block-size reform coalition and ongoing fee-volatility complaints from transactional users.
 *
 * PERSPECTIVAL GAP:
 *   Full-node operators and long-term holders experience this constraint as protective coordination — a defense of the property that makes the asset trustworthy and self-custodial at scale. Low-value on-chain users and remittance senders experience the identical parameter regime as an enforced exclusion from a use case the network was originally marketed to serve. The engine should compute divergent seat-level types precisely because the structural data (power, exit options, who benefits vs. pays) differs sharply across these seats even though the block-size rule applies uniformly to all.
 *
 * DIRECTIONALITY LOGIC:
 *   Long-term holders and full-node operators sit near the beneficiary end: they gain from constrained blocks (cheaper verification, preserved scarcity) and have mobile exit options if the arrangement changes unfavorably. Low-value on-chain users and remittance senders sit near the target end: they bear the transfer (higher effective fees, degraded transactional utility) and have trapped exit (no comparably liquid, comparably trusted alternative asset for their specific use case). Mining incumbents are beneficiaries but with constrained exit due to sunk hardware costs — this asymmetry between benefit-direction and exit-mobility is deliberate and does not require an override, since the derivation chain already captures it structurally.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem interview is central here: the store-of-value reading holds the founding problem (preserving decentralized verification against resource-concentration) is still live — new efficient-but-centralizing scaling proposals continue to be raised and rejected on these grounds, which is evidence against pure mandatrophy. However, the corroboration field flags that this reading's account of what the ORIGINAL founding problem was is contested by parties outside the current beneficiary coalition, who read the 'decentralization primacy' framing as a retrospective reconstruction adopted after Nakamoto's 2011 disappearance eliminated the possibility of authoritative clarification. This is exactly the scenario the R5 mismatch-consumer is built for: founding_problem_status is contested rather than a clean 'dead', and disappearance_verdict is correspondingly 'contested' rather than 'world_rearranges' — the classification should not collapse either possibility, and treating this as settled mandatrophy in either direction would overclaim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Which reading of the whitepaper''s founding purpose — store-of-value/decentralization-primacy or electronic-cash/transactional-primacy — was actually Nakamoto''s intended design target, and does that intent bind subsequent governance at all?',
    'No decisive resolution mechanism exists: Nakamoto''s 2011 disappearance (itself the subject of the nakamoto_oracle_opacity sibling reading) permanently removed the possibility of authoritative clarification. The closest available evidence is the whitepaper text itself (title emphasizes ''cash''; body emphasizes CPU-verification and node count), Nakamoto''s forum/mailing-list posts, and the revealed preference of the developer community that won the 2015-2017 block-size governance contest.',
    'If the electronic-cash reading is treated as authoritative, the current block-size regime reclassifies toward snare (coordination function subordinated to extraction of a use case the system was built for); if the store-of-value reading is authoritative, the current regime is better read as tangled_rope or even a defensible rope (genuine coordination correctly prioritized). This is precisely why the two readings are authored as separate constraint stories rather than reconciled into one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Irreducible interpretive indeterminacy about which founding-purpose reading of the whitepaper is authoritative, given the absence of a living author to adjudicate.').

omega_variable(
    lightning_adequacy_as_substitute,
    'Does the Lightning Network functionally substitute for the low-value on-chain use case that block-size constraints price out, or does it introduce structurally different risks (custodial exposure, routing/liquidity failure, online-availability requirements) that make it a degraded rather than equivalent substitute?',
    'Empirical study of Lightning Network failure rates, channel liquidity depth, typical user custodial arrangements (many Lightning users transact through custodial wallets that reintroduce trusted-third-party risk), and comparative transaction success rates versus historical on-chain small-value payments.',
    'If Lightning is a genuine adequate substitute, the accessibility_collapse score should be lower and the constraint reads closer to a well-functioning tangled_rope or rope; if Lightning systematically reintroduces custodial trust and availability requirements, the victim group''s exclusion is more severe than the accessibility_collapse figure (0.45) currently reflects, pushing the classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lightning_adequacy_as_substitute, empirical, 'Whether the off-chain scaling layer is an adequate functional substitute for the on-chain transactional capacity it was built to replace.').

omega_variable(
    beneficiary_capture_of_social_consensus,
    'Is the full-node-operator social-consensus process that set and maintains the block-size limit genuinely a decentralized, broad-based decision, or has it been effectively captured by the subset of node operators, long-term holders, and Lightning infrastructure operators who structurally benefit from constrained blocks?',
    'Analysis of the demographic and economic composition of active Bitcoin Core contributors, node-operator voting/signaling patterns during the block-size wars, and correlation between participation in the governance process and holding of the beneficiary positions (long-term holdings, Lightning infrastructure investment).',
    'If governance participation strongly correlates with beneficiary status, the ''agenda_setter'' role attributed to full_node_operators is better read as a beneficiary-controlled agenda, which would push the classification toward snare (extraction disguised as neutral technical coordination) rather than tangled_rope (genuine coordination with asymmetric but non-controlling extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_of_social_consensus, empirical, 'Whether the node-operator governance process is a neutral coordination mechanism or is captured by the parties who benefit from its outputs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__store_of_value_reading, 2009, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t2009, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2009, 0.05).
narrative_ontology:measurement_basis(bitc_tr_t2009, observed).
narrative_ontology:measurement(bitc_tr_t2013, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2013, 0.08).
narrative_ontology:measurement_basis(bitc_tr_t2013, observed).
narrative_ontology:measurement(bitc_tr_t2015, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2015, 0.12).
narrative_ontology:measurement_basis(bitc_tr_t2015, observed).
narrative_ontology:measurement(bitc_tr_t2017, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2017, 0.18).
narrative_ontology:measurement_basis(bitc_tr_t2017, observed).
narrative_ontology:measurement(bitc_tr_t2020, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2020, 0.2).
narrative_ontology:measurement_basis(bitc_tr_t2020, observed).
narrative_ontology:measurement(bitc_tr_t2024, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2024, 0.22).
narrative_ontology:measurement_basis(bitc_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(bitc_be_t2009, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2009, 0.05).
narrative_ontology:measurement_basis(bitc_be_t2009, observed).
narrative_ontology:measurement(bitc_be_t2013, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2013, 0.15).
narrative_ontology:measurement_basis(bitc_be_t2013, observed).
narrative_ontology:measurement(bitc_be_t2015, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2015, 0.28).
narrative_ontology:measurement_basis(bitc_be_t2015, observed).
narrative_ontology:measurement(bitc_be_t2017, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2017, 0.55).
narrative_ontology:measurement_basis(bitc_be_t2017, observed).
narrative_ontology:measurement(bitc_be_t2020, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2020, 0.48).
narrative_ontology:measurement_basis(bitc_be_t2020, observed).
narrative_ontology:measurement(bitc_be_t2024, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2024, 0.52).
narrative_ontology:measurement_basis(bitc_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t2009, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2009, 0.05).
narrative_ontology:measurement_basis(bitc_su_t2009, observed).
narrative_ontology:measurement(bitc_su_t2013, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2013, 0.12).
narrative_ontology:measurement_basis(bitc_su_t2013, observed).
narrative_ontology:measurement(bitc_su_t2015, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2015, 0.3).
narrative_ontology:measurement_basis(bitc_su_t2015, observed).
narrative_ontology:measurement(bitc_su_t2017, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2017, 0.45).
narrative_ontology:measurement_basis(bitc_su_t2017, observed).
narrative_ontology:measurement(bitc_su_t2020, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2020, 0.36).
narrative_ontology:measurement_basis(bitc_su_t2020, observed).
narrative_ontology:measurement(bitc_su_t2024, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2024, 0.38).
narrative_ontology:measurement_basis(bitc_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__store_of_value_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper_purpose__store_of_value_reading, 0.12).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose__electronic_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the single natural-language label 'the Bitcoin whitepaper's purpose,' per the ε-invariance principle: measuring the whitepaper's binding constraint as 'decentralization/verifiability' versus 'everyday transactional cash' yields structurally different beneficiary/victim sets and different ε values, so they are authored as separate constraint stories rather than one story with a measurement parameter. store_of_value_reading (this story, tangled_rope, ε≈0.52) authors the block-size limit as legitimately subordinating on-chain capacity to decentralization; electronic_cash_reading (sibling, different ε and likely different claimed type) authors the same textual kernel with transactional-use as binding, making the identical block-size limit read as a betrayal of founding purpose; nakamoto_oracle_opacity (sibling) treats the absence of an authoritative interpreter as itself the load-bearing structural fact. All three link to each other via affects_constraints, mirroring the BGS spectral-universality / eigenvector-thermalization decomposition pattern.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
