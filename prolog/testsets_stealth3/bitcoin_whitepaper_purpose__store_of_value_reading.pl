% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper_purpose__store_of_value_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Bitcoin Capacity Ceiling under the Store-of-Value Reading (Verifiability-First Settlement)
 *   domain: distributed systems/monetary theory/technology governance
 *
 * SUMMARY:
 *   This story authors ONE reading of a contested kernel. The kernel is the
 *   Bitcoin whitepaper's purpose: a founding text whose designated
 *   interpreter (Satoshi Nakamoto) fell silent in 2011, leaving the document
 *   as substrate that rival constituencies read differently. THIS FILE
 *   instantiates the store_of_value_reading only: the position that
 *   decentralization and full-node verifiability are the binding design
 *   commitments, that on-chain capacity is therefore deliberately kept scarce
 *   (the 1MB block ceiling retained), that scaling belongs to layers above
 *   the base chain, and that the resulting fee market is the standing price
 *   of keeping verification sovereign. Its structural shadow: long-term
 *   holders and node operators gain; users who need cheap on-chain
 *   transactions are priced off or pushed toward custodians. The sibling
 *   reading (electronic_cash_reading), in which the 'cash' telos binds and
 *   capacity must serve everyday payments, is a DIFFERENT constraint with its
 *   own file, its own epsilon, and its own victim set; nothing about it is
 *   averaged into this story (epsilon-invariance; family link recorded in
 *   network.affects_constraints). KEY AGENTS (by structural relationship): -
 *   full_node_operators: Enforcement backbone and agenda seat
 *   (organized/identity_locked) — validate at personal expense, refuse
 *   non-conforming blocks, benefit from cheap verification -
 *   core_maintainers: Agenda seat (institutional/identity_locked) — steward
 *   the reference implementation and the roadmap the ceiling rides on -
 *   long_term_holders: Principal beneficiary (moderate/arbitrage) — scarcity
 *   discipline underwrites their premium; they bear little of the fee burden
 *   - bitcoin_miners: Fee-market recipient (organized/trapped) — collects
 *   surge fees, capped in quiet periods, sunk capital -
 *   custodial_offchain_providers: Displacement beneficiary
 *   (powerful/arbitrage) — priced-off users become their customers -
 *   low_fee_onchain_users: Primary target (powerless/constrained) — bids for
 *   scarce block space or exits - small_direct_settlement_merchants: Target
 *   (moderate/constrained) — direct acceptance made costly and uncertain -
 *   p2p_cash_advocates: Excluded constituency (organized/arbitrage) — lost
 *   the consensus contest, forked out - protocol_governance_analysts:
 *   Analytical observer (analytical/analytical) Claim and metrics are
 *   authored independently: claimed_type tangled_rope states my structural
 *   belief (genuine censorship-resistance coordination PLUS asymmetric cost
 *   incidence PLUS active enforcement), while the metric values state
 *   descriptive conditions as I assess them. The engine computes per-seat
 *   classifications; divergence between claim and computed type is datum, not
 *   error.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__store_of_value_reading, 0.64).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__store_of_value_reading, 0.52).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__store_of_value_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__store_of_value_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__store_of_value_reading, "Bitcoin Capacity Ceiling under the Store-of-Value Reading (Verifiability-First Settlement)").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__store_of_value_reading, "distributed systems/monetary theory/technology governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__store_of_value_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__store_of_value_reading, 'e8982007-4c71-4ac3-a59c-52eb5b5f7825').
narrative_ontology:cs_kernel_codification('e8982007-4c71-4ac3-a59c-52eb5b5f7825', fixed_text).
narrative_ontology:cs_authority_grounding('e8982007-4c71-4ac3-a59c-52eb5b5f7825', lineage).
narrative_ontology:cs_interpretation_layer_present('e8982007-4c71-4ac3-a59c-52eb5b5f7825').
narrative_ontology:cs_reading_relation('e8982007-4c71-4ac3-a59c-52eb5b5f7825', bitcoin_whitepaper_purpose__electronic_cash_reading, coexists_with).
narrative_ontology:cs_axiom('e8982007-4c71-4ac3-a59c-52eb5b5f7825', foundational, verification_affordability_bounds_scale).
narrative_ontology:cs_axiom_status(verification_affordability_bounds_scale, holdable).
narrative_ontology:cs_axiom_grounding('e8982007-4c71-4ac3-a59c-52eb5b5f7825', verification_affordability_bounds_scale, empirically_contingent).
narrative_ontology:cs_axiom('e8982007-4c71-4ac3-a59c-52eb5b5f7825', foundational, base_layer_is_settlement_not_payments).
narrative_ontology:cs_axiom_status(base_layer_is_settlement_not_payments, holdable).
narrative_ontology:cs_axiom_grounding('e8982007-4c71-4ac3-a59c-52eb5b5f7825', base_layer_is_settlement_not_payments, instrumental).
narrative_ontology:cs_reference_frame('e8982007-4c71-4ac3-a59c-52eb5b5f7825', verification_first_sovereign_settlement).
narrative_ontology:cs_drift_state('e8982007-4c71-4ac3-a59c-52eb5b5f7825', contemporary_custodial_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e8982007-4c71-4ac3-a59c-52eb5b5f7825', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, full_node_operators).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_miners).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, custodial_offchain_providers).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, low_fee_onchain_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, small_direct_settlement_merchants).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper_purpose__store_of_value_reading, digital_scarcity_thesis).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper_purpose__store_of_value_reading, fee_market_security_model).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper_purpose__store_of_value_reading, verification_cost_bound_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run independently verifying software on commodity hardware at their own expense, checking every block against consensus rules and refusing blocks or chains that exceed agreed parameters. Collectively they decide what the network accepts: a 2015-2017 attempt by miners and businesses to raise the block size collapsed when validating nodes refused to follow. Most are unpaid volunteers; their influence exists only while they keep validating, and the 'don't trust, verify' ethos makes stepping away feel like surrendering the property the system exists to protect. Their verification costs stay low precisely because the capacity ceiling holds.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, full_node_operators, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__store_of_value_reading, full_node_operators, beneficiary).

% Maintain the reference implementation most nodes run; review, merge, or reject protocol changes and publish releases. Their reputations, funding relationships, and professional identities are built on a roadmap committed to keeping per-node verification cheap and moving scaling above the base layer. They cannot force anyone to upgrade, so their authority operates through release politics, persuasion, and social standing rather than command.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, core_maintainers, agenda_setter,
    institutional, generational, identity_locked, global).

% Hold the asset as savings across years and transact rarely. The capacity ceiling's scarcity discipline underwrites the security-and-neutrality story their valuation relies on, while their infrequent transactions expose them to little of the fee burden. Deep liquid markets let them sell or diversify at any time; their principal stake is that the premium narrative stays credible.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_holders, beneficiary,
    moderate, generational, arbitrage, global).

% Convert electricity into blocks and collect the block subsidy plus transaction fees; pools aggregate many small operators. Scarce block space concentrates fee revenue when demand surges, but the per-block cap bounds their upside in quiet periods. Their specialized hardware has no use outside this chain, and they must build exactly what the validating majority accepts; their 2017 attempt to signal larger blocks failed when nodes refused to follow.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_miners, beneficiary,
    organized, biographical, trapped, global).

% Operate exchanges, custodial wallets, and hosted second-layer services. Users priced off the base layer become their customers: balances sit with them, payments route through their infrastructure. Expensive on-chain access expands their addressable market; they profit whichever way the interpretive dispute resolves, so long as direct self-custodied use stays costly.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, custodial_offchain_providers, beneficiary,
    powerful, biographical, arbitrage, global).

% Send small cross-border payments and savings transfers directly on the base layer: remittances, payroll, peer-to-peer trades in economies with failing local currencies. When demand fills blocks they bid against larger spenders or wait through confirmation backlogs; at the 2017 and 2021 peaks, fees sometimes exceeded the amounts being sent. Many arrived because local banking failed them, so leaving means returning to the problem they fled, and the technical alternatives demand skills or custody arrangements they often lack.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, low_fee_onchain_users, payer,
    powerless, immediate, constrained, global).

% Accept the asset directly for goods and services without an intermediary processor. Fixed capacity turns their checkout into a fee auction during busy periods and imposes confirmation-time uncertainty on customers; most have retreated to custodial processors or discontinued direct acceptance. Their customer bases and accounting are already built around the asset, so switching means rebuilding their sales plumbing.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, small_direct_settlement_merchants, payer,
    moderate, immediate, constrained, regional).

% Read the whitepaper's 'peer-to-peer electronic cash' purpose as the binding specification and campaigned through 2015-2017 for larger blocks to keep everyday purchases on-chain. They lost the consensus contest, saw pro-big-block content moderated out of major discussion venues during the fight, and a portion forked to a larger-block chain in 2017. They remain outside base-layer governance, advocating from an adjacent chain.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, p2p_cash_advocates, excluded,
    organized, biographical, arbitrage, global).

% Study how the network governs itself: node economics, developer influence, miner incentives, fork outcomes. Their published assessments are used by regulators, investors, and academics. They hold no stake in outcomes and can compare this system freely against other ledgers; their seat exists to see the structure the participants stand inside of.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, protocol_governance_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a globally consistent, independently verifiable transaction ledger: capping per-block capacity keeps full verification affordable on commodity hardware, so no coalition of miners, exchanges, or developers can rewrite history undetected by thousands of independent validators. Scarce block space is rationed among competing users by fee auction.
% TRANSFER_FUNCTION: Moves fee revenue from users who transact on-chain to miners; moves low-value payment activity off the base layer into second-layer networks and custodial services; and, via the scarcity-plus-security narrative, supports a valuation premium accruing to large holders.
% ABSENT_VOICES: Small-value users priced off the base layer have no formal seat: governance runs through node operators, miners, and developers, and the cash-utility constituency speaks mainly through fork exit or second-layer workarounds. Merchants seeking direct settlement likewise lack representation. Their objections surfaced only as the 2015-2017 blocksize conflict and the 2017 fork.
% DISAPPEARANCE_RATIONALE: If the capacity ceiling vanished overnight (nodes accepting arbitrarily large blocks), verification costs would balloon, hobbyist validators would quit, and validation would concentrate in datacenters, pools, and exchanges. Censorship-resistance and neutrality premiums would deflate, undermining the holders' thesis; the fee market would collapse toward subsidy-only security just as subsidies decline; and high-throughput cash usage would flood the base layer, reversing the settlement-layer character the reading defends. Nearly every seat rearranges.
% FOUNDING_PROBLEM: Replace trust-based electronic payments with peer-to-peer verification: eliminate trusted third parties from digital cash by letting every participant verify the ledger. The 1MB block ceiling began in 2010 as a temporary anti-spam measure, never announced as permanent policy.
% FOUNDING_PROBLEM_CORROBORATION: The whitepaper text itself, which belongs to no benefiting party, declares the trusted-third-party elimination objective. The 2015-2017 blocksize-war record and the cash faction's fork exit attest that the reading dispute is real rather than rhetorical, and the academic cost-of-verification literature corroborates that the trade-off this reading invokes exists. No disinterested body adjudicates which clause binds; Satoshi's silence removed the tie-breaking voice.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__store_of_value_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__store_of_value_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__store_of_value_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__store_of_value_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper_purpose__store_of_value_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.64: the fee market moves real sums — 2017 and 2021 congestion peaks produced transaction fees exceeding many users' payment amounts — and priced-off users suffer exclusion losses, partially offset by genuine settlement service received. Suppression 0.52 is a standing structural quantity: within-system alternatives (raising the cap) are foreclosed by consensus culture and exit carries severe network-effect cost, even though active coercion today is modest. Theater 0.33: the verification function is real and performed daily, but a widening gap separates the sovereignty rhetoric (node counts, proof-of-keys rituals) from where economic weight actually sits (exchanges, ETFs, custodial wallets). Accessibility collapse 0.55: inside Bitcoin the big-block alternative is essentially foreclosed; externally, forks and rival chains persist at network-effect cost. Resistance 0.55: the 2015-2017 conflict was a full civil war; resistance since is episodic (inscription/ordinals disputes rekindled capacity arguments in 2023).
 *   
 *   CYCLICAL PATTERN: the series oscillates rather than drifting monotonically — halving-amplified speculative waves collide with fixed capacity, producing congestion crests (T8=2017, T12=2021) followed by bear-market lulls (T10), with a structural plateau forming by T17 as the fee market matures. Two full crests are visible on the shared nine-point grid. The oscillation is partly the extraction mechanism itself: maximum fee capture lands exactly when users' urgency peaks (intermittent-reinforcement character). Base metrics are authored at the current phase — post-second-crest plateau, T17.
 *   
 *   SUPPRESSION SERIES VS SCALAR: the suppression_requirement series tracks active enforcement intensity, which was BUILT during the wars (forum censorship, node DDoS, UASF brinkmanship peaking at 0.80 in 2017) and then DECAYED into routinized, largely self-executing consensus enforcement (0.28 today). The scalar 0.52 measures standing structural suppressiveness (foreclosed alternatives, costly exit). These are distinct quantities; the story deliberately traces the enforcement-machinery arc rather than assuming it flat.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the node-operator and maintainer seats the arrangement reads as guardianship: cheap sovereignty, identity reward, a job faithfully done. From the priced-off user and merchant seats the same structure reads as exclusion: a fee auction they lose, a door closing. Holders experience a premium accrual at negligible personal cost; miners are genuinely ambivalent (windfall surge fees versus capped quiet-period revenue versus rules they cannot change); custodial providers experience growth fed by displacement. Same nominal community, same nominal rules — the per-seat classifications diverge because power, exit, and directional position differ, and the engine computes that divergence from the structural data rather than from the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d: long-term holders sit nearest the beneficiary pole (declared beneficiaries WITH arbitrage-grade exit — they can liquidate anytime); full_node_operators sit low despite their out-of-pocket costs because the ceiling is what keeps their verification affordable and their role meaningful; custodial providers are clean beneficiaries of the displacement flow. Victim declarations drive high d: low_fee_onchain_users (powerless, constrained, global) sit nearest the full-target pole — dispersed across jurisdictions, unable to coordinate locally, locked in by the very monetary failures that brought them. Small merchants follow slightly behind. Miners land mid-low: declared beneficiaries receiving fees, but with trapped exit and capped revenue tempering the position. No directionality overrides are authored: the derivation chain from beneficiary/victim declarations plus exit atoms already orders the seats correctly, and the guidance reserves overrides for cases the derivation gets wrong. Suppression enters the computation unscaled; only extractiveness is scaled, by directionality and by global scope, which amplifies effective extraction on the dispersed victim seats because verification of their treatment is hardest at that spread.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — eliminating trusted third parties from electronic payments — is NOT dead; it has mutated. The ceiling's own success pushes small users into custodial hands, recreating the trusted intermediary one layer up, which is the sharpest evidence the founding disease persists. Mandatrophy is therefore unresolved, and the classification guards against both mislabels: a pure-coordination reading would erase the priced-off victims and treat a contested policy parameter as if it were weather; a pure-extraction reading would erase the daily-delivered censorship-resistance and neutrality that the arrangement genuinely produces. Tangled rope keeps both truths on the table: real coordination function, asymmetric incidence, active enforcement required to hold the shape.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is authored as ONE reading — store_of_value_reading — of the kernel bitcoin_whitepaper_purpose; what would the corpus record if the electronic_cash_reading were instantiated instead?',
    'Generate the sibling story (bitcoin_whitepaper_purpose__electronic_cash_reading) with mirrored structure and compare epsilon, beneficiary/victim sets, and computed types across the family.',
    'Under the sibling reading, low-fee on-chain users are the intended beneficiaries rather than the priced-off victims, the capacity ceiling becomes the offending element rather than its guardian, and epsilon''s referent shifts from the settlement-integrity arrangement to the cash-accessibility arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame routing: this file is one reading of a contested kernel; sibling readings are separate constraints, not parameters of this one.').

omega_variable(
    disagreement_load_bearing_clause,
    'Where in the kernel text does the reading-disagreement actually bind — the title''s ''cash'' designation, or the design''s decentralization and verification emphasis?',
    'Close textual analysis cross-checked against revealed preference: which clause each faction treats as defeasible in practice (the cash camp accepted second-layer compromises; the store-of-value camp accepted custodial drift).',
    'If ''cash'' binds, the ceiling violates the kernel''s own specification and this reading is revisionist; if verification binds, the cash reading abandons the kernel''s security core. Locating the load-bearing clause determines which reading counts as deviation rather than interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_load_bearing_clause, conceptual, 'Locates the specific structural element the sibling readings disagree on.').

omega_variable(
    ceiling_technical_necessity,
    'Is a capacity ceiling at roughly this magnitude technically necessary for affordable full verification, or is it a conservatively chosen parameter maintained past its evidentiary support?',
    'Node-cost telemetry across candidate block sizes, plus historical natural experiments: pre-2017 growth under the ceiling, and the larger-block fork''s node-count and validation-cost trajectory.',
    'If necessity holds, much of the measured extraction is the genuine price of the coordination good; if not, the surplus is manufactured-scarcity rent and the arrangement slides toward pure extraction with a coordination cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ceiling_technical_necessity, empirical, 'Tests the trade-off premise underwriting the ceiling.').

omega_variable(
    fee_market_security_sufficiency,
    'Can the fee market the ceiling creates sustain hashrate security as block subsidies continue declining?',
    'Post-halving fee-share telemetry and stress models of the security budget at successive subsidy epochs.',
    'If fees fall short, the security premise beneath the holders'' premium erodes, the ceiling loses its coordination justification, and persistence shifts toward inertia — the hybrid''s coordination half decays while the extraction half remains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fee_market_security_sufficiency, empirical, 'Long-run viability of the security-budget mechanism the reading depends on.').

omega_variable(
    lightning_noncustodial_absorption,
    'Does the Lightning Network actually absorb priced-off users without recreating custodial intermediaries?',
    'Wallet custody telemetry: the share of Lightning liquidity and payment volume flowing through non-custodial channels versus custodial wallets and exchange integrations.',
    'If absorption is mostly custodial, the ceiling''s scaling story functions partly as cover for re-intermediation — the whitepaper''s eliminated trusted third parties return one layer up, and the coordination half of the hybrid thins toward extraction-with-ritual.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lightning_noncustodial_absorption, empirical, 'Whether the off-chain escape valve preserves the self-custody property it promises.').

omega_variable(
    custodial_practice_gap_reversibility,
    'Is the drift of economic weight into custodial hands (exchanges, ETF wrappers) compatible with, or fatal to, the node-sovereignty premise this reading rests on?',
    'Track the self-custodied share of supply and the economic relevance of independent nodes; observe whether sovereignty rhetoric retains behavioral bite (withdrawal events, proof-of-keys participation).',
    'If the drift is irreversible, the verification the ceiling protects is performed by a shrinking caste for an audience that no longer verifies — the theater share rises and the arrangement trends toward inertial maintenance dressed as principle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(custodial_practice_gap_reversibility, conceptual, 'Whether the practice drift toward custody undermines the reading''s own foundation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__store_of_value_reading, 0, 17).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement_basis(bitc_tr_t0, observed).
narrative_ontology:measurement(bitc_tr_t2, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2, 0.07).
narrative_ontology:measurement_basis(bitc_tr_t2, observed).
narrative_ontology:measurement(bitc_tr_t4, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 4, 0.1).
narrative_ontology:measurement_basis(bitc_tr_t4, observed).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 6, 0.24).
narrative_ontology:measurement_basis(bitc_tr_t6, observed).
narrative_ontology:measurement(bitc_tr_t8, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 8, 0.42).
narrative_ontology:measurement_basis(bitc_tr_t8, observed).
narrative_ontology:measurement(bitc_tr_t10, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement_basis(bitc_tr_t10, observed).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 12, 0.31).
narrative_ontology:measurement_basis(bitc_tr_t12, observed).
narrative_ontology:measurement(bitc_tr_t14, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 14, 0.36).
narrative_ontology:measurement_basis(bitc_tr_t14, observed).
narrative_ontology:measurement(bitc_tr_t17, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 17, 0.33).
narrative_ontology:measurement_basis(bitc_tr_t17, observed).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 0, 0.04).
narrative_ontology:measurement_basis(bitc_be_t0, observed).
narrative_ontology:measurement(bitc_be_t2, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2, 0.1).
narrative_ontology:measurement_basis(bitc_be_t2, observed).
narrative_ontology:measurement(bitc_be_t4, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 4, 0.16).
narrative_ontology:measurement_basis(bitc_be_t4, observed).
narrative_ontology:measurement(bitc_be_t6, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 6, 0.34).
narrative_ontology:measurement_basis(bitc_be_t6, observed).
narrative_ontology:measurement(bitc_be_t8, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 8, 0.72).
narrative_ontology:measurement_basis(bitc_be_t8, observed).
narrative_ontology:measurement(bitc_be_t10, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement_basis(bitc_be_t10, observed).
narrative_ontology:measurement(bitc_be_t12, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement_basis(bitc_be_t12, observed).
narrative_ontology:measurement(bitc_be_t14, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 14, 0.57).
narrative_ontology:measurement_basis(bitc_be_t14, observed).
narrative_ontology:measurement(bitc_be_t17, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 17, 0.64).
narrative_ontology:measurement_basis(bitc_be_t17, observed).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 0, 0.02).
narrative_ontology:measurement_basis(bitc_su_t0, observed).
narrative_ontology:measurement(bitc_su_t2, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2, 0.05).
narrative_ontology:measurement_basis(bitc_su_t2, observed).
narrative_ontology:measurement(bitc_su_t4, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 4, 0.08).
narrative_ontology:measurement_basis(bitc_su_t4, observed).
narrative_ontology:measurement(bitc_su_t6, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 6, 0.38).
narrative_ontology:measurement_basis(bitc_su_t6, observed).
narrative_ontology:measurement(bitc_su_t8, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 8, 0.8).
narrative_ontology:measurement_basis(bitc_su_t8, observed).
narrative_ontology:measurement(bitc_su_t10, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement_basis(bitc_su_t10, observed).
narrative_ontology:measurement(bitc_su_t12, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 12, 0.34).
narrative_ontology:measurement_basis(bitc_su_t12, observed).
narrative_ontology:measurement(bitc_su_t14, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 14, 0.3).
narrative_ontology:measurement_basis(bitc_su_t14, observed).
narrative_ontology:measurement(bitc_su_t17, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 17, 0.28).
narrative_ontology:measurement_basis(bitc_su_t17, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__store_of_value_reading, information_standard).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose__electronic_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__store_of_value_reading, nakamoto_oracle_opacity).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'Bitcoin's design purpose' conflates two structurally distinct claims. The store_of_value_reading (this file) authors epsilon for the standing settlement-integrity arrangement — scarce base-layer capacity defended by node consensus, holders and node operators gaining, low-fee users bearing the incidence. The electronic_cash_reading authors a DIFFERENT constraint over the same text: capacity subordinated to everyday cash utility, with the ceiling as the offending element. Their epsilon values differ widely because their referents differ; neither averages into the other. The upstream/downstream link runs through the 2017 consensus outcome, which reshaped the cash reading's operating environment (forcing fork exit) without resolving it. The nakamoto_oracle_opacity entry is linked as the interpretive-vacuum condition that keeps both readings live.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
