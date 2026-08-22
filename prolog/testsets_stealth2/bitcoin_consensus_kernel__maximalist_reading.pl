% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_consensus_kernel__maximalist_reading, []).

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
 *   constraint_id: bitcoin_consensus_kernel__maximalist_reading
 *   human_readable: Immutable Monetary Covenant (Maximalist Reading of the Bitcoin Whitepaper)
 *   domain: cryptoeconomics/monetary_systems/distributed_consensus
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the bitcoin_consensus_kernel - the
 *   maximalist_reading - as a clean, epsilon-invariant constraint, per the
 *   committer frame: the whitepaper's monetary rules constitute a binding
 *   founding covenant, and altering them is illegitimate per se. The standing
 *   arrangement under contest is the covenant-governed monetary regime
 *   itself: since 2009 the network has enforced an unchanging issuance
 *   schedule, the norm that changes violate the covenant hardened through the
 *   2015-2017 scaling conflict, and today the covenant functions
 *   simultaneously as the asset's core credibility mechanism and as a barrier
 *   against base-layer adaptation. Sibling readings (utility_reading,
 *   pragmatic_synthesis) are separate constraints in separate files, linked
 *   via network.affects_constraints; the contest between readings is routed
 *   to omega variables, not described inside this constraint. Claim/metric
 *   independence is preserved: the claimed type (tangled_rope) states what I
 *   believe is structurally true - a genuine coordination function coupled to
 *   asymmetric, actively enforced extraction - while the metrics state what I
 *   believe is descriptively true of the arrangement's actual operation; the
 *   engine computes per-seat classifications from the structural data and any
 *   divergence from the claim is the measurement the corpus exists to take.
 *
 * KEY AGENTS:
 *   - long_term_bitcoin_holders: Primary beneficiary (organized/identity_locked) - holds the asset the covenant protects; financial position fused with holding ideology
 *   - early_adopter_whales: Primary beneficiary and receipt seat (powerful/arbitrage) - largest per-coin gainers from frozen supply; unconstrained exit, rational covenant defense
 *   - institutional_treasury_allocators: Secondary beneficiary (institutional/arbitrage) - post-2020 reserve demand anchored by policy stasis
 *   - full_node_operators: Enforcement core (organized/mobile) - collectively decisive validators; individually replaceable, which is what makes refusal credible
 *   - core_protocol_developers: Agenda-setter and bearer (organized/identity_locked) - administer the covenant while forgoing base-layer innovation; professional identity fused with fidelity
 *   - mining_pool_operators: Dual seat (organized/constrained) - apply the rules in daily practice while bearing the halving schedule their enforcement locks in
 *   - onchain_scaling_advocates: Primary target (moderate/constrained) - proposals recast as covenant violations; fork exit demonstrated ruinous in 2017
 *   - layer_two_builders: Sanctioned bearer-beneficiary (moderate/constrained) - innovation tolerated only where it cannot touch the covenanted rules
 *   - fee_burdened_users: Diffuse target (powerless/mobile) - absorb congestion costs; atomized, unorganized, weakest voice
 *   - competing_chains: Excluded (powerful/mobile) - delegitimized wholesale as scams; outside the conversation the covenant polices
 *   - monetary_economists: Analytical observer (analytical/analytical) - assess fixed-rule versus discretionary regimes; compelling neither side
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__maximalist_reading, 0.72).
domain_priors:suppression_score(bitcoin_consensus_kernel__maximalist_reading, 0.72).
domain_priors:theater_ratio(bitcoin_consensus_kernel__maximalist_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__maximalist_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_consensus_kernel__maximalist_reading, "Immutable Monetary Covenant (Maximalist Reading of the Bitcoin Whitepaper)").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__maximalist_reading, "cryptoeconomics/monetary_systems/distributed_consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__maximalist_reading, 'f79c01e3-8bf6-4cf6-b6b5-e770de754158').
narrative_ontology:cs_kernel_codification('f79c01e3-8bf6-4cf6-b6b5-e770de754158', fixed_text).
narrative_ontology:cs_authority_grounding('f79c01e3-8bf6-4cf6-b6b5-e770de754158', lineage).
narrative_ontology:cs_interpretation_layer_present('f79c01e3-8bf6-4cf6-b6b5-e770de754158').
narrative_ontology:cs_reading_relation('f79c01e3-8bf6-4cf6-b6b5-e770de754158', bitcoin_consensus_kernel__pragmatic_synthesis, coexists_with).
narrative_ontology:cs_reading_relation('f79c01e3-8bf6-4cf6-b6b5-e770de754158', bitcoin_consensus_kernel__utility_reading, forecloses).
narrative_ontology:cs_axiom('f79c01e3-8bf6-4cf6-b6b5-e770de754158', foundational, monetary_rules_bind_as_covenant).
narrative_ontology:cs_axiom_status(monetary_rules_bind_as_covenant, holdable).
narrative_ontology:cs_axiom_grounding('f79c01e3-8bf6-4cf6-b6b5-e770de754158', monetary_rules_bind_as_covenant, deontological).
narrative_ontology:cs_axiom('f79c01e3-8bf6-4cf6-b6b5-e770de754158', foundational, holder_expectations_are_property_rights).
narrative_ontology:cs_axiom_status(holder_expectations_are_property_rights, holdable).
narrative_ontology:cs_axiom_grounding('f79c01e3-8bf6-4cf6-b6b5-e770de754158', holder_expectations_are_property_rights, deontological).
narrative_ontology:cs_axiom('f79c01e3-8bf6-4cf6-b6b5-e770de754158', secondary, credibility_requires_policy_stasis).
narrative_ontology:cs_axiom_status(credibility_requires_policy_stasis, holdable).
narrative_ontology:cs_axiom_grounding('f79c01e3-8bf6-4cf6-b6b5-e770de754158', credibility_requires_policy_stasis, instrumental).
narrative_ontology:cs_reference_frame('f79c01e3-8bf6-4cf6-b6b5-e770de754158', immutable_founding_covenant).
narrative_ontology:cs_drift_state('f79c01e3-8bf6-4cf6-b6b5-e770de754158', etf_custody_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f79c01e3-8bf6-4cf6-b6b5-e770de754158', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, long_term_bitcoin_holders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, early_adopter_whales).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, institutional_treasury_allocators).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, onchain_scaling_advocates).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, layer_two_builders).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, fee_burdened_users).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, mining_pool_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, layer_two_builders).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, core_protocol_developers).
narrative_ontology:constraint_vindicates(bitcoin_consensus_kernel__maximalist_reading, fixed_supply_scarcity_doctrine).
narrative_ontology:constraint_vindicates(bitcoin_consensus_kernel__maximalist_reading, algorithmic_monetary_rule_superiority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold bitcoin as their principal savings vehicle. Their balances are protected by the guarantee that total supply is capped at 21 million and issuance follows a fixed schedule no one can alter. Over a decade a distinct culture has grown around never selling, in which converting to fiat is treated as a moral failing rather than a portfolio decision. Selling is technically frictionless, but community standing, online identity, and self-conception are bound up with continuing to hold.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, long_term_bitcoin_holders, beneficiary,
    organized, generational, identity_locked, global).

% Acquired very large positions between 2009 and 2013 at negligible cost. The frozen supply schedule is what turns their coins into credibly scarce assets; every year of policy stasis compounds the premium on holdings they obtained nearly for free. They can liquidate into deep markets at any time, yet rationally fund and amplify the discourse defending the schedule, since the covenant is the source of their position's value.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, early_adopter_whales, beneficiary,
    powerful, generational, arbitrage, global).

% Corporations, funds, and post-ETF vehicles that added bitcoin to reserves from 2020 onward. They selected the asset precisely because no committee can dilute it - policy stasis is the feature that makes it admissible as a non-discretionary reserve line. Their participation deepens liquidity and raises the reputational cost of any policy change; exit is an ordinary portfolio rebalance.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, institutional_treasury_allocators, beneficiary,
    institutional, biographical, arbitrage, global).

% Run independent validating software that rejects any block breaking the rules. No proposal takes effect unless enough of them choose to run code implementing it, which makes their collective refusal the operative check on the schedule. Individually they are anonymous and replaceable; collectively they are decisive. Any of them can run alternative clients or entirely different chains at will.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, full_node_operators, agenda_setter,
    organized, biographical, mobile, global).

% Maintain the reference implementation and review every proposed change. Community norms filter who contributes and what may be proposed: edits touching issuance or the supply cap are declined before technical review begins. They administer the arrangement while personally forgoing base-layer work their skills could support, and their professional reputations are inseparable from fidelity to the founding rules.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, core_protocol_developers, agenda_setter,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__maximalist_reading, core_protocol_developers, payer).

% Aggregate hashpower and decide which chain tip to extend, so they apply the rules in daily practice. At the same time the fixed schedule cuts their block subsidy in half every four years with no possibility of voting for extended emission, and their capital is sunk into single-purpose hardware. Extending the canonical chain is simultaneously their enforcement act and the source of a compounding revenue squeeze.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, mining_pool_operators, payer,
    organized, immediate, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__maximalist_reading, mining_pool_operators, agenda_setter).

% Engineers and entrepreneurs who have proposed raising base-layer capacity or otherwise adjusting protocol parameters. Their proposals are consistently reframed as breaches of the founding commitment rather than engineering tradeoffs. The 2017 attempt to exit by forking ended with the minority chain retaining a small fraction of the value, liquidity, and legitimacy - a demonstration that shapes every subsequent proposal to stay inside and absorb refusal.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, onchain_scaling_advocates, payer,
    moderate, biographical, constrained, global).

% Build Lightning and similar off-chain systems - the one venue where innovation is tolerated, because it leaves the settlement rules untouched. They gain from the credibility the frozen base layer lends their channels while absorbing its costs: forced architectural complexity, fragmented liquidity, and dependence on a settlement layer whose capacity can never grow. Their tolerated presence marks the exact boundary of what the covenant permits.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, layer_two_builders, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__maximalist_reading, layer_two_builders, beneficiary).

% Send transactions on the base layer and pay fees that spike whenever demand exceeds the fixed block space. They have no formal voice in consensus and no organization representing them. Switching to other networks or custodial services is easy, and many do; each departure slightly thins the distributed validation the fixed rules are said to require.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, fee_burdened_users, payer,
    powerless, immediate, mobile, global).

% Operate alternative networks with flexible or differently governed monetary policies. Within the discourse surrounding this covenant they are categorized as scams or distractions irrespective of technical merit, and no exchange listing or developer effort purchases reclassification. They are not party to the conversation that defines Bitcoin; maintaining that exclusion is what the covenant's legitimacy claims accomplish.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, competing_chains, excluded,
    powerful, generational, mobile, global).

% Study fixed-rule versus discretionary monetary regimes and publish analyses of whether algorithmic immutability improves on historical standards such as the gold standard. Neither the enforcing community nor its opponents can compel their findings; they observe and publish.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, monetary_economists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_consensus_kernel__maximalist_reading, early_adopter_whales).
narrative_ontology:fixing_cost_class(bitcoin_consensus_kernel__maximalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a leaderless network with a monetary rule no participant or coalition can revise: a fixed 21-million supply cap and deterministic issuance schedule that every validator independently enforces, giving strangers a shared, verifiable expectation of future scarcity without a governing committee.
% TRANSFER_FUNCTION: Transfers monetary discretion out of existence - no one may alter supply - which converts incumbent coin balances into credibly scarce assets while directing the costs of frozen capacity (congestion fees, foregone base-layer features, stranded scaling work) onto current users, builders, and miners; the net flow runs from change-bearing participants to incumbent holders in proportion to coin weight.
% ABSENT_VOICES: Base-layer scaling engineers who lost the 2017 fork exited to a minority chain and were recategorized as scammers, removing the strongest internal dissent from the room; prospective users priced out by fees never join and so never register objection; future participants inherit parameters they had no hand in consenting to. The conversation that ratifies the covenant contains only seats that already accept it.
% DISAPPEARANCE_RATIONALE: If the covenant norm dissolved overnight, monetary policy becomes a governable parameter: holders would demand governance assurances or discount the asset, multiple competing policy forks would immediately seek hashpower and listings, and the scarcity premium organizing the entire holder economy would reprice - exchanges, corporate treasuries, and mining economics would all reorganize around whichever policy won.
% FOUNDING_PROBLEM: Enable peer-to-peer electronic cash without a trusted issuer: solve double-spending across an open network while removing the discretionary supply decisions that define central-bank money - the founding schedule replaced monetary governance with arithmetic.
% FOUNDING_PROBLEM_CORROBORATION: Cryptographic literature outside the holder economy corroborates that trustless open-network consensus was a real, previously unsolved problem the whitepaper addressed. But corroboration that the founding problem remains LIVE in the covenant's terms comes only from within the benefiting parties; payment-rail competitors (stablecoin issuers, alternative layer-one networks) demonstrate peer-to-peer digital value transfer operating without an immutable-supply covenant, and no neutral party attests that the specific frozen-policy form remains necessary. Stated plainly: the problem's original framing is corroborated; its continued liveness under this reading is attested only by beneficiaries.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__maximalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__maximalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__maximalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_consensus_kernel__maximalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_consensus_kernel__maximalist_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_consensus_kernel__maximalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_consensus_kernel__maximalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_consensus_kernel__maximalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.72: the costs of frozen policy are concrete and growing - congestion fees that spike against fixed block space, foregone base-layer features, stranded scaling work, a halving-driven miner revenue squeeze - while the offsetting benefit (scarcity premium) accrues pro rata to incumbent coin, and coin ownership is heavily concentrated, making the flow structurally asymmetric. Suppression 0.72 is authored as a raw structural property, unscaled by power or scope (only extractiveness is scaled downstream): alternatives are suppressed by legitimacy denial ('any change is a scam, not Bitcoin'), contributor gatekeeping, and network-effect lock-in rather than physical coercion - the 2017 fork demonstrated that the exit door exists but leads to a fraction of the value. Theater_ratio 0.30: the enforcement beneath the covenant is mechanically real (node validation, hashpower selection), but a rising share of covenant activity is performative identity signaling - 'covenant' rhetoric, purity tests, Satoshi-quote liturgy - which peaked during the 2017 conflict and has plateaued as ritual. Accessibility_collapse 0.60: inside the frame alternatives collapse almost completely (once the covenant is accepted, any change is unthinkable), but external exits persist (minority chains, other layer-ones, off-layer systems), so collapse is partial rather than total. Resistance 0.58: a decade of sustained resistance from the scaling camp, expressed as forks, off-layer end-runs, and academic critique - real, continuous, and ultimately losing within the frame. Identity-lock dynamics: holders are bound by ideological-financial fusion (selling as betrayal; the constraint would read very differently if the hodl identity frame broke and exit became a neutral portfolio act), and core contributors by professional identity (career capital and standing constituted through covenant fidelity). Coalition check: the target seats are coalition-incapable - the scaling camp fragmented after its 2017 defeat, users are atomized and unrepresented, and miners' enforcement role contradicts their payer interest - which is why high aggregate victim count has not produced effective counter-pressure. Cyclical note: the four-year halving cycle modulates fee pressure and miner stress around the secular trend; the authored grid tracks the trend, not the cycle, and the cycle itself is not an intermittent-reinforcement mechanism but an exogenous schedule consequence.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the engine derives that divergence from the structural data. From the holder and agenda-setter seats the covenant presents as protective coordination they built and police: nobody can dilute them, nobody can surprise them, and the arrangement is the product's core feature. From the payer seats the identical structure operates as enforced extraction: scaling advocates watch proposals die as heresy rather than engineering, off-layer builders inherit the costs of a settlement layer that may never grow, users eat congestion fees, and miners enforce the very schedule that halves their income. Epsilon indexing note: the referent is the covenant-governed standing arrangement - never the utility reading's endorsed improvable alternative - and the authored value reflects what the maximalist frame itself openly acknowledges imposing on change-seeking seats; the sibling readings author their own epsilon over the same protocol history, and cross-reading comparison joins on kernel_id, not on this constraint_id.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: early_adopter_whales sit nearest the beneficiary pole (arbitrage-grade exit plus concentrated coin weight), institutional_treasury_allocators similarly (portfolio-level exit), while long_term_bitcoin_holders sit less far toward the beneficiary end than their economic interest alone would suggest because identity-locked exit pulls them toward the target side of the spectrum - they carry the covenant's costs (forgone adaptability) along with its premium. Victim declarations drive high directionality: onchain_scaling_advocates, constrained by the demonstrated ruin of fork exit, sit near the full-target end; layer_two_builders, dual-positioned as sanctioned beneficiaries of base-layer credibility, land mid-high; fee_burdened_users are targets whose mobility damps their effective extraction (they can and do leave); mining_pool_operators, enforcement actors who are simultaneously squeezed by the schedule they enforce, land mid-high despite their agenda-setting power. Full-node operators and core developers are administered seats whose directionality derives from their dual positioning rather than from either array. The engine owns the arithmetic; these declarations are the inputs.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. The covenant is presented by its defenders as mountain-like - 'twenty-one million is set in mathematics, no one can change it' - and a naive reading would certify a natural law; declaring beneficiaries forces exactly the scrutiny that distinguishes a constructed, enforced norm from an irreducible limit, and the immutability_enforcement_dependence omega keeps the naturality question open rather than pre-adjudicated. Symmetrically, a pure-snare reading would erase the genuine coordination achievement: a leaderless network does need a monetary rule no committee can revise, and the covenant solves that problem for every seat, including most of its targets. Tangled_rope preserves both facts. On obsolescence: the founding problem (trustless p2p cash without discretionary issuance) has partially migrated - the asset's dominant use settled into store-of-value while payment volume moved to other rails - and the founding_problem_status is authored contested rather than dead, so the dead-plus-world_rearranges mismatch flag does not fire; but the atrophy of the original payment function is real and is the live mandatrophy question for this constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (maximalist_reading) of the bitcoin_consensus_kernel; would instantiating utility_reading or pragmatic_synthesis instead restructure the beneficiary/victim sets and epsilon?',
    'Compile the sibling stories and compare computed classifications joined on kernel_id: if the utility reading''s constraint computes with inverted beneficiary/victim structure over the same protocol history, the indexical choice - not the underlying protocol - drives the classification.',
    'Every classification in this file attaches to the reading-instantiated constraint, not to ''Bitcoin'' simpliciter; cross-reading comparison is valid only at the kernel level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame routing: one kernel, three readings, three structurally distinct constraints.').

omega_variable(
    covenant_scope_boundary,
    'Does ''any change'' in the founding covenant cover only monetary-policy parameters, or all consensus rules - and who adjudicates that boundary?',
    'Codify the revealed boundary from which changes the enforcing community accepted (segwit, taproot) versus rejected (block-size increases, tail emission), and identify the adjudicating seat.',
    'A monetary-only scope makes off-layer innovation a sanctioned outlet and lowers measured suppression; an all-rules scope widens the target set to every proposer of any consensus change and pushes the structure toward the snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covenant_scope_boundary, conceptual, 'The covenant''s scope is interpreted, not written; the boundary determines the victim set.').

omega_variable(
    immutability_enforcement_dependence,
    'Would monetary-policy stasis persist if active enforcement (legitimacy denial, contributor gatekeeping, exchange and listing politics) ceased - is the fixed schedule a self-sustaining equilibrium or an enforced norm?',
    'Counterfactual analysis of fork history: determine whether change-proposals failed on technical merit alone or through coordinated legitimacy denial, and observe proposal behavior when enforcement attention slackens.',
    'If enforcement-dependent, the constraint is constructed and the tangled-rope/snare family reading stands; if self-sustaining, a mountain-like component enters the analysis and extraction attributions weaken.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immutability_enforcement_dependence, empirical, 'Whether the covenant is naturality or maintained construction.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression binding identity-fused seats (long-term holders, core contributors) structural (liquidity depth, reputation capital, career path dependence) or internalized (holding ideology, covenant fidelity as self-concept)?',
    'Post-exit trajectory study: track participants who sold or left the project - if covenant-fidelity norms and maximalist identification persist after financial exit, the internalized component dominates.',
    'Internalized suppression travels with the agent after exit, raising effective suppression above the structural measure and stabilizing the constraint against purely incentive-based remedies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Split of suppression between external barriers and fused identity.').

omega_variable(
    rent_above_coordination_floor,
    'How much of the measured extraction exceeds the inherent coordination cost of operating a global monetary infrastructure - is the asymmetry rent or necessary overhead?',
    'Cost-structure analysis of validation, bandwidth, and fee-market mechanics compared against the distribution of covenant benefits by coin weight.',
    'If most extraction sits above the infrastructure floor and accrues in proportion to coin concentration, the tangled-rope reading hardens toward the snare boundary; if near-floor, the covenant approximates pure coordination with incidental asymmetry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rent_above_coordination_floor, empirical, 'Separating coordination cost from positional rent in the measured extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__maximalist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(bitc_tr_t0, observed).
narrative_ontology:measurement(bitc_tr_t5, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 5, 0.11).
narrative_ontology:measurement_basis(bitc_tr_t5, observed).
narrative_ontology:measurement(bitc_tr_t10, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement_basis(bitc_tr_t10, observed).
narrative_ontology:measurement(bitc_tr_t15, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement_basis(bitc_tr_t15, observed).
narrative_ontology:measurement(bitc_tr_t20, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement_basis(bitc_tr_t20, observed).
narrative_ontology:measurement(bitc_tr_t25, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(bitc_tr_t25, observed).
narrative_ontology:measurement(bitc_tr_t30, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement_basis(bitc_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(bitc_be_t0, observed).
narrative_ontology:measurement(bitc_be_t5, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 5, 0.41).
narrative_ontology:measurement_basis(bitc_be_t5, observed).
narrative_ontology:measurement(bitc_be_t10, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 10, 0.49).
narrative_ontology:measurement_basis(bitc_be_t10, observed).
narrative_ontology:measurement(bitc_be_t15, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement_basis(bitc_be_t15, observed).
narrative_ontology:measurement(bitc_be_t20, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(bitc_be_t20, observed).
narrative_ontology:measurement(bitc_be_t25, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 25, 0.7).
narrative_ontology:measurement_basis(bitc_be_t25, observed).
narrative_ontology:measurement(bitc_be_t30, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement_basis(bitc_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement_basis(bitc_su_t0, observed).
narrative_ontology:measurement(bitc_su_t5, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 5, 0.51).
narrative_ontology:measurement_basis(bitc_su_t5, observed).
narrative_ontology:measurement(bitc_su_t10, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement_basis(bitc_su_t10, observed).
narrative_ontology:measurement(bitc_su_t15, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 15, 0.76).
narrative_ontology:measurement_basis(bitc_su_t15, observed).
narrative_ontology:measurement(bitc_su_t20, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(bitc_su_t20, observed).
narrative_ontology:measurement(bitc_su_t25, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(bitc_su_t25, observed).
narrative_ontology:measurement(bitc_su_t30, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(bitc_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__maximalist_reading, global_infrastructure).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__utility_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__pragmatic_synthesis).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the bitcoin_consensus_kernel per the epsilon-invariance principle: the colloquial label 'Bitcoin's monetary rules' covers three structurally distinct claims. This file instantiates the maximalist_reading (covenant fidelity; high extraction against change-seeking seats; holders as beneficiaries). The utility_reading (minimum viable mechanism enabling iterative improvement) and pragmatic_synthesis (immutable base rules with sanctioned upper-layer innovation) are separate constraints with their own epsilon, beneficiaries, and classifications; the upstream maximalist claim supplies the legitimacy conditions under which the other two readings must operate, hence the outgoing edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
