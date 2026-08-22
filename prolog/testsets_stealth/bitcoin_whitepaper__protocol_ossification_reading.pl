% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__protocol_ossification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_whitepaper__protocol_ossification_reading, []).

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
 *   constraint_id: bitcoin_whitepaper__protocol_ossification_reading
 *   human_readable: Universal-Consensus Threshold for Bitcoin Protocol Change (Ossification Reading)
 *   domain: cryptocurrency economics/monetary systems/technology governance
 *
 * SUMMARY:
 *   Within Bitcoin's leaderless governance, an operative norm holds that
 *   changes to the consensus rules are illegitimate short of near-universal
 *   agreement, and that base-layer stability outranks feature progress —
 *   innovation, on this view, belongs in layers built above an immutable
 *   foundation. The norm crystallized out of the 2015–2017 block-size wars
 *   and is maintained through maintainer gatekeeping of the codebase, node
 *   operators' refusal to adopt unwanted software, exchange recognition
 *   practices, and the demonstrated economic punishment of fork exits. It
 *   solves a real collective-action problem (any non-consensual change splits
 *   the ledger) while imposing asymmetric, accumulating costs on use cases
 *   that require base-layer change: privacy, programmability, and fee relief.
 *   This story instantiates the protocol_ossification_reading of the
 *   bitcoin_whitepaper kernel (see kernel_context). The claim and the metrics
 *   are independent authored facts: the type is claimed as tangled_rope from
 *   the authoring seat, while the metrics describe moderately extractive,
 *   actively enforced operation — the engine measures any divergence.
 *
 * KEY AGENTS:
 *   - - core_protocol_maintainers: agenda setter (organized/constrained) — merges or rejects change proposals; cannot compel adoption; collects gatekeeping authority
 *   - - large_btc_holders: primary beneficiary (powerful/arbitrage) — wealth tracks freeze credibility; bears almost none of the frozen layer's costs
 *   - - asic_invested_miners: beneficiary with sunk exposure (organized/constrained) — hardware value depends on rules staying fixed
 *   - - institutional_custodians: institutional beneficiary (institutional/arbitrage) — compliance products require a never-changing rule set
 *   - - node_operator_community: enforcer-beneficiary (organized/identity_locked) — personal validation is the ultimate veto; node-running fused with participation identity
 *   - - layer_two_builders: sheltered beneficiary (moderate/mobile) — the freeze protects their niche from base-layer obsolescence
 *   - - bitcoin_native_protocol_developers: primary target (moderate/identity_locked) — proposals stall regardless of merit; departure means abandoning the project they identify with
 *   - - cross_chain_protocol_researchers: mobile target (moderate/mobile) — ideas emigrate even when this network forgoes them
 *   - - privacy_seeking_users: diffuse target (powerless/constrained) — the blocked change class is exactly their need
 *   - - fee_sensitive_small_transactors: diffuse target (powerless/constrained) — absorb congestion costs that capacity relief would remove
 *   - - bitcoin_smart_contract_builders: blocked builder (moderate/constrained) — product space sits behind an unreachable threshold
 *   - - fork_attempt_communities: punished exiter (moderate/trapped) — exit was priced punitively; now outside the legitimacy conversation
 *   - - governance_researchers: analytical observer (analytical/analytical) — sees the full structure without a stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__protocol_ossification_reading, 0.58).
domain_priors:suppression_score(bitcoin_whitepaper__protocol_ossification_reading, 0.62).
domain_priors:theater_ratio(bitcoin_whitepaper__protocol_ossification_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__protocol_ossification_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__protocol_ossification_reading, "Universal-Consensus Threshold for Bitcoin Protocol Change (Ossification Reading)").
narrative_ontology:topic_domain(bitcoin_whitepaper__protocol_ossification_reading, "cryptocurrency economics/monetary systems/technology governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__protocol_ossification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__protocol_ossification_reading, '7e028c5f-081e-473a-8062-9b37678fb03e').
narrative_ontology:cs_kernel_codification('7e028c5f-081e-473a-8062-9b37678fb03e', fixed_text).
narrative_ontology:cs_authority_grounding('7e028c5f-081e-473a-8062-9b37678fb03e', lineage).
narrative_ontology:cs_interpretation_layer_present('7e028c5f-081e-473a-8062-9b37678fb03e').
narrative_ontology:cs_reading_relation('7e028c5f-081e-473a-8062-9b37678fb03e', bitcoin_whitepaper__p2p_cash_reading, influences).
narrative_ontology:cs_reading_relation('7e028c5f-081e-473a-8062-9b37678fb03e', bitcoin_whitepaper__digital_gold_reading, coexists_with).
narrative_ontology:cs_axiom('7e028c5f-081e-473a-8062-9b37678fb03e', foundational, universal_consensus_legitimacy_requirement).
narrative_ontology:cs_axiom_status(universal_consensus_legitimacy_requirement, holdable).
narrative_ontology:cs_axiom_grounding('7e028c5f-081e-473a-8062-9b37678fb03e', universal_consensus_legitimacy_requirement, conventional).
narrative_ontology:cs_axiom('7e028c5f-081e-473a-8062-9b37678fb03e', foundational, base_layer_stability_primacy).
narrative_ontology:cs_axiom_status(base_layer_stability_primacy, holdable).
narrative_ontology:cs_axiom_grounding('7e028c5f-081e-473a-8062-9b37678fb03e', base_layer_stability_primacy, instrumental).
narrative_ontology:cs_reference_frame('7e028c5f-081e-473a-8062-9b37678fb03e', protocol_immutability_guardianship).
narrative_ontology:cs_drift_state('7e028c5f-081e-473a-8062-9b37678fb03e', contemporary_post_etf_era, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('7e028c5f-081e-473a-8062-9b37678fb03e', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, core_protocol_maintainers).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, large_btc_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, asic_invested_miners).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, institutional_custodians).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, layer_two_builders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, node_operator_community).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_native_protocol_developers).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, cross_chain_protocol_researchers).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, privacy_seeking_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, fee_sensitive_small_transactors).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_smart_contract_builders).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, fork_attempt_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, asic_invested_miners).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__protocol_ossification_reading, consensus_threshold_necessity).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__protocol_ossification_reading, layered_innovation_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Review and merge or reject proposed changes to the consensus rules; cannot compel anyone to run the resulting software and depend on voluntary adoption. Absorb blame from both directions — stagnation from one side, breakage risk from the other. Their standing rests on perceived neutrality and technical care, and their authority over what reaches the codebase grows as the accepted rate of change approaches zero.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, core_protocol_maintainers, agenda_setter,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__protocol_ossification_reading, core_protocol_maintainers, beneficiary).

% Hold the dominant share of outstanding coins; their wealth tracks the asset's credibility as a fixed-supply store of value, which any contentious change puts at risk. They can rebalance into other assets at will, yet overwhelmingly fund, publish, and amplify the stability position. They bear almost none of the day-to-day costs of the frozen base layer.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, large_btc_holders, beneficiary,
    powerful, biographical, arbitrage, global).

% Operate specialized hardware whose value depends on the current rule set remaining in force; collect block rewards and fees under unchanged parameters. A rule change could strand their capital or shift revenue, so they back stability, while foregone efficiency improvements and congested-period fee volatility are costs they also carry.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, asic_invested_miners, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__protocol_ossification_reading, asic_invested_miners, payer).

% Custody institutional-scale holdings under regulatory obligations; a predictable, never-changing rule set is what makes compliant custody vehicles and fund products possible. They collect management and custody fees from the arrangement and bear little of its cost.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, institutional_custodians, beneficiary,
    institutional, generational, arbitrage, global).

% Build payment and contract systems on top of the unchanged base layer; the freeze protects their niche from being obsoleted by base-layer upgrades, and they benefit from the settlement assurances the static layer provides beneath their protocols.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, layer_two_builders, beneficiary,
    moderate, biographical, mobile, global).

% Run full nodes that validate every rule personally; their refusal to adopt software they dislike is the system's ultimate veto over any change. Many treat running a node as a core expression of participation in the network, and they collect the assurance that no remote party can redefine their money.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, node_operator_community, agenda_setter,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__protocol_ossification_reading, node_operator_community, beneficiary).

% Propose base-layer improvements — covenant opcodes, privacy upgrades, new signature schemes — and watch proposals stall indefinitely regardless of technical merit. Their reputations and careers are bound up with this specific network, so leaving means abandoning the project they identify with; the realistic paths for their ideas are years of delay or departure.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_native_protocol_developers, payer,
    moderate, biographical, identity_locked, global).

% Work on protocol design across many networks; when a proposal dies here they take it elsewhere, so their ideas survive even as this network forgoes them. They bear the opportunity cost of attention diverted from this protocol to ecosystems that accept experimentation.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, cross_chain_protocol_researchers, payer,
    moderate, biographical, mobile, global).

% Need transactions that resist surveillance and censorship; base-layer privacy improvements are precisely the class of change the stability norm blocks, and moving to other assets means accepting weaker security and thinner liquidity. They pay elevated fees during congestion and transact under permanent public-chain visibility.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, privacy_seeking_users, payer,
    powerless, biographical, constrained, global).

% Send remittances and small payments; when blocks fill, their fees spike and confirmation times stretch, and capacity relief at the base layer is ruled out by the stability norm. They can migrate to cheaper rails but lose the settlement properties that brought them here.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, fee_sensitive_small_transactors, payer,
    powerless, immediate, constrained, global).

% Want richer programmability — covenants, vaults, new opcodes — that requires consensus-rule changes; the stability norm places their entire product space behind a threshold the network treats as effectively unreachable. Some pivot to layered designs, others leave for programmable chains.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_smart_contract_builders, payer,
    moderate, biographical, constrained, global).

% Attempted exit by forking over rule disagreements; the market marked their chains down sharply against the original asset, and they now operate outside the legitimacy conversation entirely, cited mainly as cautionary examples. Returning would mean accepting the rules they left over.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, fork_attempt_communities, excluded,
    moderate, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__protocol_ossification_reading, fork_attempt_communities, payer).

% Study decentralized governance comparatively; publish analyses of activation episodes, signalling thresholds, and fork outcomes; hold no stake in results and can see the whole structure without participating in it.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, governance_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper__protocol_ossification_reading, large_btc_holders).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper__protocol_ossification_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a decision procedure for a leaderless protocol: by requiring overwhelming agreement before any consensus-rule change, it keeps a single ledger and a single unified security budget in a system where no authority exists to adjudicate competing change proposals.
% TRANSFER_FUNCTION: Moves decision rights over the base protocol to the status quo coalition — incumbent holders, miners with sunk hardware, maintainers, and compliance-bound custodians — and moves the costs of foreclosed improvements onto builders and users whose purposes require base-layer changes; secondarily, it moves fee revenue upward during congestion because capacity expansion at the base layer is ruled out.
% ABSENT_VOICES: Future users and not-yet-existing use cases cannot appear to object. Privacy-seeking users are present but stigmatized. Expelled fork communities and developers who departed after the 2017 wars are outside the GitHub repositories, mailing lists, and conferences where legitimacy is actually negotiated. Small transactors priced out during fee spikes have no seat in the process at all.
% DISAPPEARANCE_RATIONALE: If the norm vanished overnight, the queue of shelved change proposals would activate simultaneously; incompatible rule sets would split the ledger, exchanges would suspend markets, the unified security budget would fragment across minority chains, and the ecosystem would rearrange around either a new governance settlement or several smaller, poorer networks.
% FOUNDING_PROBLEM: The 2015–2017 block-size wars: repeated near-splits over capacity showed that contentious changes under decentralized governance can destroy the network's unity and value; the norm crystallized afterward as 'never again.'
% FOUNDING_PROBLEM_CORROBORATION: Contemporaneous journalism and academic studies of the blocksize war document the split risk from outside the benefiting parties, and the public market outcome of the 2017 fork — the minority chain's persistent devaluation — corroborates that splits are destructive. Critics outside the beneficiary set, including departed developers and competing-chain communities, attest instead that the acute crisis passed and the norm now blocks low-risk improvements; the status is genuinely disputed across seats.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__protocol_ossification_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__protocol_ossification_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__protocol_ossification_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_whitepaper__protocol_ossification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__protocol_ossification_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper__protocol_ossification_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_whitepaper__protocol_ossification_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_whitepaper__protocol_ossification_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon's referent is the standing arrangement — the universal-consensus threshold as it actually operates — assessed by this reading's own lights. Because this reading endorses the freeze, it prices split-risk heavily and treats blocked use cases as regrettable rather than illegitimate harm, landing epsilon at 0.58 rather than the higher value a hostile reading would author over the same referent; the value nonetheless rises monotonically across the interval as unmet use cases accumulate (privacy debt, covenant proposals stalling, congestion fees). Suppression (0.62) is a raw structural property, unscaled by power or scope: alternatives are not forbidden but are priced punitively — fork exits were marked down catastrophically, and layer pivots substitute only partially. Theater (0.28) grows steadily as substantive deliberation shrinks and ritual restatement of 'don't touch the base layer' expands to fill the vacuum. Accessibility_collapse (0.45) reflects partial collapse: understanding the norm reveals that base-layer change is effectively unavailable, yet exits (other chains, layers) remain legible if costly. Resistance (0.55) is continuous and real — every activation fight, filtering dispute, and covenant debate is the norm meeting live opposition. The measurement series share one time grid (2015/2017/2019/2021/2023/2026) with every tracked metric authored at every point. The suppression series is authored because enforcement capacity genuinely changed: it spiked at the 2017 UASF/B2X crisis (mobilized enforcement against a hashrate-backed split attempt), then settled into steady normalized enforcement with a mild recent rise as inscription-filtering and covenant disputes recur. Receipt surface: gain_flow names large_btc_holders because value-preservation accrues pro-rata to holdings, so the largest wallets capture the largest share of what the freeze protects; fixing_cost is prohibitive because loosening the threshold would re-trigger the very founding crisis the norm exists to prevent — the fix costs what the arrangement was built to avoid.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the large-holder seat the arrangement is protective coordination it funds and benefits from, with near-zero personal cost. From the bitcoin-native-developer seat the same structure is a blocked vocation: proposals die on legitimacy grounds rather than merit, and exit is priced against professional identity. From the maintainer seat it is stewardship — an exhausting duty of care over a treasure others constantly propose to remodel. From the small-transactor seat it is invisible except as congestion fees. The engine computes per-seat classifications from the structural data (role, power, exit, scope); the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: large_btc_holders sit nearest the subsidy end (direct benefit plus arbitrage-grade exit from any downside); institutional_custodians similarly (compliance benefit, mobile capital); asic_invested_miners sit slightly higher (benefit tempered by sunk-capital exposure to parameter changes); core_protocol_maintainers derive low-to-mid d (authority benefit offset by service burden and blame absorption); node_operator_community derives low d as declared beneficiaries while supplying the enforcement that holds the structure up; layer_two_builders derive low d (protected-niche benefit). Targets derive high directionality: bitcoin_native_protocol_developers sit nearest the full-target end (identity-locked exit amplifies chi); privacy_seeking_users and fee_sensitive_small_transactors are high-d diffuse payers with constrained exit; bitcoin_smart_contract_builders are high-d with partially available layer pivots; cross_chain_protocol_researchers are high-d but damped by mobile exit; fork_attempt_communities are high-d and trapped post-exit. No directionality overrides were needed: the beneficiary/victim declarations plus exit atoms produce the correct relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing destructive splits under leaderless governance — retains a live core: any genuinely contentious change still risks a split, so the mandate is not dead and the mismatch consumer finds status=contested paired with verdict=world_rearranges, producing no zombie flag. But partial atrophy is visible: the norm is now invoked reflexively against proposals posing negligible split risk (flag-day-free soft-fork covenants, privacy opcodes), a scope creep from 'prevent splits' toward 'prevent change.' The tangled_rope classification prevents mislabeling in both directions: a pure-coordination framing would hide the accumulating, uncompensated costs on blocked use cases; a pure-extraction framing would erase the genuine split-prevention function and the absence of a single concentrated enforcing capturer — the gains accrue diffusely-but-skewed to holders rather than to a coordinating cartel. Mandatrophy_resolved is deliberately not declared: the mandate has atrophied at the margins, not expired.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Which reading of the bitcoin_whitepaper kernel governs protocol-change legitimacy — this protocol_ossification_reading, the p2p_cash_reading, or the digital_gold_reading?',
    'Track which reading''s criteria dominate actual activation decisions across episodes (SegWit, Taproot, the failed large-block push, covenant and filtering disputes) and which seats cite the whitepaper for which conclusions.',
    'Under the cash reading, the freeze arrangement''s epsilon rises substantially over the fixed referent (its core use case is the blocked set); under the gold reading, epsilon falls (the freeze serves the reading''s purpose). This file''s epsilon is authored from the ossification reading''s own lights and stays invariant here; sibling files carry their own values.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this constraint is one of three sibling readings of the whitepaper kernel; the contest among readings is routed here, not into the constraint body.').

omega_variable(
    consensus_threshold_operationalization,
    'What observable counts as ''approaching universal consensus'' — hash-rate supermajority, economic-majority node signalling, or maintainer-declared rough consensus?',
    'Compare activation histories: which signals were treated as sufficient (SegWit via user-activated soft fork plus hashrate; Taproot via miner signalling) versus insufficient (the 2017 hashrate-majority hard-fork attempt, rejected despite mining support).',
    'A node/economic-majority standard widens the blocked set and raises effective extraction on change advocates; a hashrate standard narrows it and shifts power toward miners; the ambiguity materially moves the victim boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_threshold_operationalization, conceptual, 'The norm''s operative threshold is undefined, and its operationalization determines who can legitimately change the protocol.').

omega_variable(
    technical_necessity_vs_governance_choice,
    'Is the universal-consensus requirement a technical necessity of maintaining a decentralized ledger, or a contingent governance choice replaceable by formalized on-chain amendment procedures?',
    'Comparative analysis of chains with formal governance mechanisms versus Bitcoin''s informal threshold: do formal amendment procedures avoid splits without freezing evolution?',
    'If necessity, part of the measured suppression is the irreducible price of decentralization (a mountain-flavored floor beneath the constructed norm); if choice, the full suppression is attributable to the constructed arrangement and its beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_necessity_vs_governance_choice, conceptual, 'Natural-law versus constructed ambiguity in the consensus requirement itself.').

omega_variable(
    suppression_internalization,
    'Is the suppressed proposal activity among Bitcoin-native developers structural (economic punishment of forks, career and reputation risk) or internalized (self-censorship under the ''don''t touch the base layer'' culture)?',
    'Post-exit trajectory: whether developers who left for other ecosystems resume proposing base-layer-class changes there at normal rates, or carry the inhibition with them.',
    'If internalized, effective suppression exceeds the structural measure and persists even where exit is available, raising the true cost borne by the developer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Structural versus internalized suppression mechanism among identity-locked developers.').

omega_variable(
    layer_two_substitution_adequacy,
    'Do higher layers actually substitute for the blocked base-layer capabilities (throughput, privacy, covenants), or does the victim set retain uncompensated losses?',
    'Adoption and capability data: lightning liquidity and usability limits, sidechain trust models, sustained on-chain fee levels during congestion episodes, and the persistence of demand for exactly the blocked opcode classes.',
    'If substitution is adequate, the blocked use cases are compensated and the arrangement reads closer to pure coordination; if inadequate, the extraction from victims stands uncompensated and the tangled_rope reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(layer_two_substitution_adequacy, empirical, 'Whether the layered-innovation promise compensates the use cases the freeze blocks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__protocol_ossification_reading, 2015, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t2015, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 2015, 0.08).
narrative_ontology:measurement_basis(bitc_tr_t2015, observed).
narrative_ontology:measurement(bitc_tr_t2017, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 2017, 0.12).
narrative_ontology:measurement_basis(bitc_tr_t2017, observed).
narrative_ontology:measurement(bitc_tr_t2019, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 2019, 0.16).
narrative_ontology:measurement_basis(bitc_tr_t2019, observed).
narrative_ontology:measurement(bitc_tr_t2021, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 2021, 0.2).
narrative_ontology:measurement_basis(bitc_tr_t2021, observed).
narrative_ontology:measurement(bitc_tr_t2023, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 2023, 0.24).
narrative_ontology:measurement_basis(bitc_tr_t2023, observed).
narrative_ontology:measurement(bitc_tr_t2026, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 2026, 0.28).
narrative_ontology:measurement_basis(bitc_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(bitc_be_t2015, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 2015, 0.28).
narrative_ontology:measurement_basis(bitc_be_t2015, observed).
narrative_ontology:measurement(bitc_be_t2017, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 2017, 0.42).
narrative_ontology:measurement_basis(bitc_be_t2017, observed).
narrative_ontology:measurement(bitc_be_t2019, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 2019, 0.48).
narrative_ontology:measurement_basis(bitc_be_t2019, observed).
narrative_ontology:measurement(bitc_be_t2021, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 2021, 0.52).
narrative_ontology:measurement_basis(bitc_be_t2021, observed).
narrative_ontology:measurement(bitc_be_t2023, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 2023, 0.56).
narrative_ontology:measurement_basis(bitc_be_t2023, observed).
narrative_ontology:measurement(bitc_be_t2026, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 2026, 0.58).
narrative_ontology:measurement_basis(bitc_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t2015, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 2015, 0.4).
narrative_ontology:measurement_basis(bitc_su_t2015, observed).
narrative_ontology:measurement(bitc_su_t2017, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 2017, 0.68).
narrative_ontology:measurement_basis(bitc_su_t2017, observed).
narrative_ontology:measurement(bitc_su_t2019, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 2019, 0.6).
narrative_ontology:measurement_basis(bitc_su_t2019, observed).
narrative_ontology:measurement(bitc_su_t2021, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 2021, 0.57).
narrative_ontology:measurement_basis(bitc_su_t2021, observed).
narrative_ontology:measurement(bitc_su_t2023, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 2023, 0.6).
narrative_ontology:measurement_basis(bitc_su_t2023, observed).
narrative_ontology:measurement(bitc_su_t2026, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 2026, 0.62).
narrative_ontology:measurement_basis(bitc_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__protocol_ossification_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(bitcoin_whitepaper__protocol_ossification_reading, p2p_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__protocol_ossification_reading, digital_gold_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Bitcoin' conflates three structurally distinct claims about one text (the whitepaper kernel). Decomposed per the epsilon-invariance principle into three linked stories: p2p_cash_reading (purpose: electronic cash; capacity and fee-relief changes are fidelity), digital_gold_reading (purpose: scarce settlement asset; the freeze serves fidelity), and this file, protocol_ossification_reading (the governance-procedure member: the consensus threshold itself is the commitment). This reading INFLUENCES p2p_cash_reading — freezing the base layer shifts cash usage to higher layers, changing that reading's operating environment without eliminating it — and COEXISTS WITH digital_gold_reading, with which it is mutually reinforcing and frequently held by the same parties. Each story carries its own epsilon over the shared referent (the standing freeze arrangement).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
