% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__p2p_cash_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_whitepaper__p2p_cash_reading, []).

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
 *   constraint_id: bitcoin_whitepaper__p2p_cash_reading
 *   human_readable: Bitcoin Peer-to-Peer Electronic Cash Reading — Standing Fee-Market Arrangement
 *   domain: economic/technological/monetary
 *
 * SUMMARY:
 *   This story instantiates the p2p_cash_reading of the bitcoin_whitepaper
 *   kernel: Bitcoin as a censorship-resistant medium of exchange for direct
 *   electronic transactions. The constraint under assessment is the STANDING
 *   ARRANGEMENT — the current protocol with its conservative block capacity,
 *   fixed issuance schedule, and the fee market that emerged once blocks
 *   filled in 2017 — assessed by the cash reading's own lights. By that
 *   reading, the arrangement retains a genuine and hard-won coordination core
 *   (permissionless consensus, censorship-resistant settlement) while
 *   imposing a fee-market toll that prices ordinary-sized payments off the
 *   chain and denies the founding document's intended beneficiaries reliable
 *   access. The victim set is therefore those denied transactional access by
 *   fee markets: small-value transactors and unbanked remittance users.
 *   Sibling readings (digital_gold_reading, protocol_ossification_reading)
 *   are separate constraints with their own epsilon values, beneficiary
 *   structures, and victim sets; they are linked, not folded in here. KEY
 *   AGENTS (by structural relationship): - miners: Agenda-setting fee
 *   collectors (institutional/constrained) — enforce consensus via hashpower,
 *   receive subsidy and fees - core_protocol_developers: Kernel interpreters
 *   and capacity-policy gatekeepers (organized/identity_locked) -
 *   long_term_holders: Scarcity-premium beneficiaries (powerful/arbitrage) —
 *   fund capacity conservatism - small_value_transactors: Priced-out payers
 *   (powerless/constrained) — bear the fee auction -
 *   unbanked_remittance_users: Intended beneficiaries turned excluded payers
 *   (powerless/trapped) - lightning_node_operators: Layer-two beneficiaries
 *   (moderate/mobile) — volume pushed to their layer - regulated_exchanges:
 *   Institutional beneficiaries carrying compliance burdens
 *   (institutional/arbitrage) - big_block_fork_communities: Expelled
 *   dissenters (moderate/mobile) — exited via the 2017 split -
 *   monetary_policy_analysts: Analytical observers (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__p2p_cash_reading, 0.62).
domain_priors:suppression_score(bitcoin_whitepaper__p2p_cash_reading, 0.58).
domain_priors:theater_ratio(bitcoin_whitepaper__p2p_cash_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__p2p_cash_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__p2p_cash_reading, "Bitcoin Peer-to-Peer Electronic Cash Reading — Standing Fee-Market Arrangement").
narrative_ontology:topic_domain(bitcoin_whitepaper__p2p_cash_reading, "economic/technological/monetary").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__p2p_cash_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__p2p_cash_reading, 'ff089912-691d-4cbd-9a62-f8fb82454c04').
narrative_ontology:cs_kernel_codification('ff089912-691d-4cbd-9a62-f8fb82454c04', fixed_text).
narrative_ontology:cs_authority_grounding('ff089912-691d-4cbd-9a62-f8fb82454c04', lineage).
narrative_ontology:cs_interpretation_layer_present('ff089912-691d-4cbd-9a62-f8fb82454c04').
narrative_ontology:cs_reading_relation('ff089912-691d-4cbd-9a62-f8fb82454c04', bitcoin_whitepaper__digital_gold_reading, coexists_with).
narrative_ontology:cs_reading_relation('ff089912-691d-4cbd-9a62-f8fb82454c04', bitcoin_whitepaper__protocol_ossification_reading, influences).
narrative_ontology:cs_axiom('ff089912-691d-4cbd-9a62-f8fb82454c04', foundational, onchain_payments_for_ordinary_users).
narrative_ontology:cs_axiom_status(onchain_payments_for_ordinary_users, holdable).
narrative_ontology:cs_axiom_grounding('ff089912-691d-4cbd-9a62-f8fb82454c04', onchain_payments_for_ordinary_users, instrumental).
narrative_ontology:cs_axiom('ff089912-691d-4cbd-9a62-f8fb82454c04', secondary, fee_gated_access_is_exclusion).
narrative_ontology:cs_axiom_status(fee_gated_access_is_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('ff089912-691d-4cbd-9a62-f8fb82454c04', fee_gated_access_is_exclusion, empirically_contingent).
narrative_ontology:cs_reference_frame('ff089912-691d-4cbd-9a62-f8fb82454c04', direct_p2p_electronic_cash_arrangement).
narrative_ontology:cs_drift_state('ff089912-691d-4cbd-9a62-f8fb82454c04', post_fee_market_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ff089912-691d-4cbd-9a62-f8fb82454c04', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, miners).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, long_term_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, lightning_node_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, small_value_transactors).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, unbanked_remittance_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, regulated_exchanges).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, regulated_exchanges).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__p2p_cash_reading, nakamoto_consensus_double_spend_prevention).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__p2p_cash_reading, permissionless_censorship_resistance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate hashing hardware and pool infrastructure that validate blocks and secure the ledger. They receive newly issued bitcoin on a fixed issuance schedule plus whatever transaction fees users attach to their transactions. Their capital is highly specialized — application-specific hardware whose resale value is mostly limited to other proof-of-work chains — so leaving means writing down equipment. They signal preferences on protocol changes through hashpower allocation and public statements, and during congestion periods their revenue rises with the fee rates users are forced to bid.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, miners, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__p2p_cash_reading, miners, beneficiary).

% Write and review the reference software that full nodes run. Protocol changes they decline to merge generally do not happen, and changes they endorse carry decisive weight with node operators. Their standing, conference invitations, grant funding, and career paths are bound up with the project; most have spent a decade or more inside its culture and its interpretive traditions. Developers who dissented from the prevailing capacity policy historically forked away and lost audience, funding, and relevance.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, core_protocol_developers, agenda_setter,
    organized, generational, identity_locked, global).

% Hold significant bitcoin balances acquired early or through institutional vehicles. Every limitation on new supply and on block capacity tends to raise the value of what they hold, and fee-driven scarcity reinforces the asset-narrative that attracts new buyers. They can sell or diversify into other assets at any time, and many hedge exposure elsewhere. They fund advocacy, media presence, and lobbying that favors supply conservatism and capacity restraint.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, long_term_holders, beneficiary,
    powerful, generational, arbitrage, global).

% Use the network for everyday payments — purchases, wages, family transfers. When block space is contested they must bid in fee auctions or wait for confirmation; during congestion spikes a payment costing cents at baseline can cost several dollars, which erases the point of a small transfer. Many respond by keeping balances on custodial applications that batch transactions off-chain, accepting a trusted intermediary again, or by abandoning on-chain use entirely during expensive periods.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, small_value_transactors, payer,
    powerless, immediate, constrained, global).

% Live in economies with unstable currencies or thin banking coverage — the population the founding document addressed. Legacy remittance corridors charge them double-digit percentages of every transfer. Using the chain directly requires smartphone access, fee budgeting, and technical literacy; when fees spike they are the first priced out of the block space, and they have no seat in the repositories, mining pools, or holder forums where capacity policy is effectively decided.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, unbanked_remittance_users, payer,
    powerless, immediate, trapped, continental).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__p2p_cash_reading, unbanked_remittance_users, excluded).

% Run routing nodes and channel-management services on the second-layer payment network. On-chain fee scarcity is what pushes payment volume down to their layer, where they earn routing fees and liquidity-leasing income. They depend on the main chain for opening and closing channels, so sustained on-chain fee levels raise their operating costs even as they raise their volume. They can redeploy capital to other ventures relatively easily.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, lightning_node_operators, beneficiary,
    moderate, biographical, mobile, global).

% Operate licensed trading venues where most newcomers acquire the asset. They profit from trading volume and custody fees that appreciation drives, while absorbing compliance costs, banking-access risk, and legal liability in every jurisdiction they serve. Capacity policy matters to them chiefly through its effect on price narrative and withdrawal-fee complaints. They can list or delist assets and relocate incorporation between jurisdictions.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, regulated_exchanges, beneficiary,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__p2p_cash_reading, regulated_exchanges, payer).

% Argued through 2015-2017 that raising the block-size limit was the faithful reading of the founding document, ran competing clients, and ultimately split the chain in August 2017. They now maintain their own ledger with a fraction of the hashpower, liquidity, and developer attention. Their objection survives mainly as a standing critique of the capacity policy rather than as a live seat in the governing conversation.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, big_block_fork_communities, excluded,
    moderate, biographical, mobile, global).

% Study the arrangement from universities, think tanks, and independent research shops. They publish fee-market analyses, security-budget projections, and adoption studies, and they assess whether the network still delivers the properties its founding document promised. They hold no balances at stake in the design choices and can criticize any faction without exit costs.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, monetary_policy_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper__p2p_cash_reading, miners).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper__p2p_cash_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Produces a single agreed transaction history among mutually distrusting strangers without any trusted intermediary — solving double-spending in a permissionless setting — and keeps that history appendable by anyone regardless of nationality, creditworthiness, or political standing.
% TRANSFER_FUNCTION: Moves transaction fees from transactors to miners alongside the scheduled issuance subsidy; moves scarcity premium to existing holders whenever supply or block capacity stays tight; moves cross-border value from senders to recipients minus fees; and increasingly moves small-payment traffic off-chain to custodial and routing intermediaries.
% ABSENT_VOICES: The unbanked and remittance-sending population the founding document named has no seat in protocol governance, which runs through developer repositories, miner signaling, and holder-weighted forums. Small merchants who stopped accepting on-chain payments and users priced out during fee spikes object from outside the process. Big-block advocates participated until the 2017 split removed them from the table.
% DISAPPEARANCE_RATIONALE: Demand for censorship-resistant value transfer would not vanish: users would consolidate on rival chains, stablecoins, or informal rails; miners' specialized capital would strand; and the layered payment industry built around the asset would reorganize within months around the next credible permissionless ledger.
% FOUNDING_PROBLEM: Online commerce depended on trusted financial intermediaries to process electronic payments: reversibility raised costs, excluded whole classes of users, and gave intermediaries veto power over lawful transfers. The founding document proposed direct peer-to-peer transfers secured by proof-of-work so that no trusted party would be required.
% FOUNDING_PROBLEM_CORROBORATION: World Bank remittance-price surveys attest that the transfer-cost problem persists at scale. Documented payment blockades and account freezes — the 2010 donation blockade, the 2022 emergency-measures account freezes in Canada — are attested by courts, journalists, and civil-liberties organizations outside any cryptocurrency constituency. What remains disputed is not whether the founding problem exists but whether this arrangement still serves it for ordinary-sized payments.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__p2p_cash_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__p2p_cash_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__p2p_cash_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_whitepaper__p2p_cash_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__p2p_cash_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper__p2p_cash_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_whitepaper__p2p_cash_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_whitepaper__p2p_cash_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.62 because the standing arrangement charges transactors a market-clearing price for block space that is decoupled from the marginal cost of including a small transaction, and because the intended cash use-case — frequent, small, direct payments — is precisely the use-case the fee market prices out first. Suppression is 0.58: the capacity policy persists not by participant preference but by active enforcement — reference-client gatekeeping, social-layer pressure on dissenting developers, and the demonstrated fate of minority chains — though alternatives outside the system (rival chains, custodial layers) remain reachable, which caps suppression below snare-grade. Theater_ratio 0.30 reflects the growing share of activity that is rhetorical maintenance of the cash identity (whitepaper invocations, decentralization signaling) while actual small-payment behavior migrates off-chain. Accessibility_collapse 0.45 and resistance 0.60 are honest for a constructed, contested arrangement: workable alternatives exist and real resistance occurred (client wars, the 2017 split, the user-activated soft fork).
 *   
 *   The temporal series run on ONE shared grid (t = 0, 3, 6, 9, 12, 15, 17, mapping 2009 to 2026) with every tracked metric authored at every point. The extractiveness series is cyclical-with-ratchet: fee spikes track halving-cycle demand waves (2017, 2021, the 2024 inscription wave), and after each cycle the baseline floor resets higher than before — congestion auctions let miners capture windfall revenue while the post-peak floor ratchets upward, so the oscillation is partly an extraction mechanism (intermittent peak pricing) rather than noise. The suppression_requirement series is authored because this story specifically traces enforcement-capacity change: the enforcement machinery (client gatekeeping, social-layer discipline, fork rejection) was built up during the 2015-2018 capacity wars and has held roughly stable since. Values reflect the interval-end state; the 2026 extractiveness dip records post-inscription fee cooling, not resolution.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the miner seat the arrangement is infrastructure they built and defend, with fee revenue as earned compensation for securing the ledger — a coordination-first experience. From the small-transactor and unbanked seats the same structure operates as a toll gate that excludes them from the very use-case the founding document promised — an exclusion-first experience. The holder seat experiences the arrangement as virtuous scarcity; the core-developer seat experiences it as stewardship under permanent siege, with their authority fused to the capacity policy they defend (professional and ideological identity-lock: exit would mean forfeiting a decade of accumulated standing inside the project's culture — break that frame and the gatekeeping position dissolves). Inter-institutional dynamics: miners and core developers are co-administrators with divergent exits — miners are constrained by sunk ASIC capital, developers by identity — and divergent exposure to fee levels. Same-level lateral dynamics: small_value_transactors and lightning_node_operators are both non-elite participants, differentiated by capital mobility — the operator monetizes the scarcity that prices the transactor out. Coalition potential for the powerless seats is real and precedented: the 2017 user-activated soft fork showed coordinated full-node users overriding miner preference, which is why the payer seats' powerlessness is conditional, not absolute.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: miners (fee recipients, agenda-setters) sit near the beneficiary end; long_term_holders (arbitrage exit, scarcity gains) sit nearest the full-beneficiary pole; lightning_node_operators benefit incidentally from pushed-down volume; small_value_transactors (constrained exit) sit near the full-target end; unbanked_remittance_users (trapped, no governance seat) sit at the extreme target end — trapped or identity-locked targets amplify effective extraction, and arbitrage-grade exit dampens it. Spatial scope is global, which modestly amplifies effective extraction for the payer seats because verification and remedy are harder at that scope. One explicit override is declared: core_protocol_developers carry power atom 'organized', and the structural derivation — which reads only beneficiary/victim declarations and exit options — cannot see their position. They appear in neither list, yet they are not symmetric bystanders: the standing arrangement sustains their interpretive authority and funding (a positional benefit) while they bear real stewardship costs and collect no fees. The override sets d = 0.42, slightly beneficiary-side of symmetric, capturing positional benefit net of stewardship burden. Suppression (0.58) is authored as a raw structural property and is deliberately left unscaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — trusted-intermediary dependence and payment censorship — is live, corroborated from outside the benefiting set, so this is not a resolved-mandatrophy case and no zombie flag is expected from the status-by-verdict mismatch (status live x world_rearranges). The classification discipline cuts both ways here. Calling the arrangement a snare would erase the genuine coordination achievement: permissionless double-spend prevention demonstrably works and no party can simply turn it off. Calling it a rope would erase the asymmetric toll: the fee market transfers systematically from the poorest-intended users to miners and holders, enforced by gatekeeping that expelled its organized opposition. Tangled rope names both halves. The live danger is mandate MIGRATION rather than mandate death: the cash mandate atrophies into a settlement-layer and asset function while the cash vocabulary persists — the founding document's title still reads peer-to-peer electronic cash, and that retained language is the theatrical component the theater_ratio measures. If the cash reading loses its last on-chain foothold (fee floors rising with each halving per the security_budget_decay omega), the arrangement completes its migration and this story's successor would need re-authoring against the gold reading's referent instead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates the p2p_cash_reading of the bitcoin_whitepaper kernel; would the digital_gold_reading or protocol_ossification_reading of the same protocol text produce a different constraint with a different victim set and a different epsilon?',
    'Author the sibling stories (digital_gold_reading, protocol_ossification_reading) against their own referents and compare computed classifications; divergence in victim sets, beneficiary structures, and epsilon locates where the readings genuinely disagree.',
    'If the gold reading consolidates as the dominant practice, the extraction measured here describes a transitional phase of a migrating mandate rather than a stable arrangement, and the cash reading''s victim set becomes evidence of obsolescence rather than of ongoing extraction; if the cash reading regains ground (capacity expansion, sustained low fees), the standing arrangement re-classifies toward rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer-frame omega: one reading of a contested kernel; sibling readings instantiate structurally different constraints over the same text.').

omega_variable(
    fee_market_necessity,
    'Is the transaction-fee market a necessary security mechanism (spam deterrence, long-run miner compensation as the issuance subsidy decays) or a constructed scarcity rent produced by the deliberate refusal to expand block capacity?',
    'Compare chains with larger blocks and lower fees against Bitcoin on security level, spam incidence, and validation decentralization; simulate the security budget under alternative capacity policies across successive halvings.',
    'If the fee market is necessary, part of the measured extraction is the irreducible price of the coordination itself and the classification shifts rope-ward; if it is constructed rent riding on gatekeeping, the arrangement is more snare-like than the tangled_rope claim suggests and the payer seats'' effective extraction rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fee_market_necessity, empirical, 'Whether fee-market extraction is security cost or gatekept rent.').

omega_variable(
    security_budget_decay,
    'Will the declining issuance subsidy force fee levels high enough that on-chain cash use becomes impossible for ordinary payments, completing the migration from cash to settlement layer?',
    'Track the fee-to-subsidy ratio across the 2024, 2028, and 2032 halvings and model miner revenue sufficiency at observed fee floors.',
    'A rising forced-fee trajectory pushes effective extraction past the tangled-rope band toward snare for the transactor seats and dates the mandate migration; a stable or falling ratio supports the coordination framing and keeps the cash reading satisfiable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_budget_decay, empirical, 'Security-budget dynamics may force further extraction from transactors.').

omega_variable(
    offchain_substitution_effect,
    'Do the Lightning Network and custodial off-chain layers restore the cash properties this reading demands (low fees, small payments, directness), or do they reintroduce the trusted-intermediary structure the founding document was written to abolish?',
    'Measure routing reliability, channel economics, custody concentration in Lightning wallets, and the effective censorship-resistance of layered payments compared with on-chain payments.',
    'If substitution works, the standing arrangement''s toll on small payments is partially mitigated and the cash reading remains satisfiable within the protocol; if it fails, the cash reading is structurally unsatisfiable on this chain, the victim set widens to include all small-value users, and the arrangement''s coordination story covers progressively less of its actual function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(offchain_substitution_effect, empirical, 'Whether layered solutions restore cash properties or quietly rebuild the intermediary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__p2p_cash_reading, 0, 17).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(bitc_tr_t3, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 3, 0.08).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(bitc_tr_t9, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 9, 0.22).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement(bitc_tr_t15, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 15, 0.29).
narrative_ontology:measurement(bitc_tr_t17, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 17, 0.3).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(bitc_be_t3, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 3, 0.1).
narrative_ontology:measurement(bitc_be_t6, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 6, 0.3).
narrative_ontology:measurement(bitc_be_t9, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 9, 0.6).
narrative_ontology:measurement(bitc_be_t12, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 12, 0.66).
narrative_ontology:measurement(bitc_be_t15, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 15, 0.7).
narrative_ontology:measurement(bitc_be_t17, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 17, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(bitc_su_t3, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 3, 0.12).
narrative_ontology:measurement(bitc_su_t6, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 6, 0.35).
narrative_ontology:measurement(bitc_su_t9, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 9, 0.5).
narrative_ontology:measurement(bitc_su_t12, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(bitc_su_t15, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 15, 0.57).
narrative_ontology:measurement(bitc_su_t17, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 17, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__p2p_cash_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, digital_gold_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, protocol_ossification_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Bitcoin' conflates at least three structurally distinct constraints sharing one kernel text, decomposed per the epsilon-invariance principle: this p2p_cash_reading (epsilon 0.62 over the standing fee-market arrangement; victims are fee-excluded transactors), digital_gold_reading (referent is the asset's scarcity regime; beneficiary-dominated), and protocol_ossification_reading (governance-legitimacy constraint; victims are proposed-change advocates). Each carries its own epsilon, beneficiaries, and victims; measuring one with another's observable was the confusion in the label, not in the structure. Causal texture: the cash reading supplies the recurring legitimacy pressure that the ossification reading exists to resist, and the gold reading supplies the economic constituency that funds both — hence the edges from this story to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_whitepaper__p2p_cash_reading, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
