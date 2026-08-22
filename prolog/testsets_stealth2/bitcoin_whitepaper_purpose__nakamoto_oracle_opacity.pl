% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper_purpose__nakamoto_oracle_opacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, []).

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
 *   constraint_id: bitcoin_whitepaper_purpose__nakamoto_oracle_opacity
 *   human_readable: Nakamoto's Departure and the Whitepaper's Interpretive Vacuum
 *   domain: distributed_systems/monetary_theory/technology_governance
 *
 * SUMMARY:
 *   When Satoshi Nakamoto went silent in 2011 — last forum post December
 *   2010, last known correspondence spring 2011, roughly one million coins
 *   never moved — the Bitcoin protocol lost the only authority that could
 *   adjudicate what its founding text means. This story authors that
 *   condition as the constraint: the interpretive vacuum, the standing
 *   governance arrangement in which the whitepaper is a fixed text with no
 *   living oracle, every fidelity claim is unadjudicable, and protocol
 *   changes must win legitimacy through public process rather than founder
 *   decree. The vacuum has a genuine coordination function — it extends the
 *   protocol's anti-capture property to the interpretive layer, leaving no
 *   founder to coerce, subpoena, or capture — and it carries real costs: the
 *   2015–2017 block-size war, the 2017 chain split, the 2018 hash war, and
 *   recurring fee-volatility episodes imposed losses on holders and payment
 *   users while de facto agenda power accrued to the reference-implementation
 *   maintainers. Constraint family note (epsilon-invariance decomposition):
 *   the colloquial label 'the whitepaper's purpose' covers three structurally
 *   distinct claims. This file is the nakamoto_oracle_opacity reading — the
 *   no-oracle governance condition itself, with epsilon authored over the
 *   vacuum arrangement as this reading sees it. Sibling files instantiate
 *   store_of_value_reading and electronic_cash_reading, each with its own
 *   epsilon over its own referent (the telos claims); neither sibling's value
 *   is averaged into this one. The three are linked via
 *   network.affects_constraints. This reading is upstream of both siblings:
 *   it is the condition under which their fidelity claims can both stand and
 *   under which fork proliferation substitutes for convergence.
 *
 * KEY AGENTS:
 *   - - core_maintainers: De facto interpretive authority (institutional/identity_locked) — collects agenda power the vacuum makes uncontestable from above; bears factional attack from every side with no founder to appeal to
 *   - - hashpower_operators: Veto-holder and fork arbitrageur (organized/constrained) — converts interpretive contest into settlement power; chain-specific hardware binds them to the Bitcoin family
 *   - - full_node_operators: Interpretive sovereigns (organized/constrained) — each verifies the rules for themselves; the vacuum's distributed enforcement base
 *   - - fork_claimant_factions: Protected claimants (organized/arbitrage) — hold unfalsifiable whitepaper-fidelity claims the vacuum shields from adjudication
 *   - - founder_identity_claimants: Oracle-seat aspirants (organized/trapped) — collect credibility and funding from the vacuum until enforcement arrives; recanting destroys everything the claim was built to gain
 *   - - chain_split_holders: Primary target (moderate/constrained) — bears split losses, replay attacks, and forced chain selection with no formal voice in protocol governance
 *   - - on_chain_transaction_users: Primary target (powerless/mobile) — bears stalemate costs as fee volatility and settlement unpredictability; most mobile seat, so costs reach them as attrition
 *   - - satoshi_nakamoto_absent: The excluded voice (powerful/trapped) — the unreachable authority the entire structure is organized around
 *   - - protocol_historians: Analytical observer (analytical/analytical) — sees the full structure; collects nothing, pays nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.62).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.66).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, extractiveness, 0.62).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, "Nakamoto's Departure and the Whitepaper's Interpretive Vacuum").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, "distributed_systems/monetary_theory/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, '17760e55-7d4a-49e6-8980-59614d3fe3ed').
narrative_ontology:cs_kernel_codification('17760e55-7d4a-49e6-8980-59614d3fe3ed', fixed_text).
narrative_ontology:cs_authority_grounding('17760e55-7d4a-49e6-8980-59614d3fe3ed', distributed).
narrative_ontology:cs_reading_relation('17760e55-7d4a-49e6-8980-59614d3fe3ed', bitcoin_whitepaper_purpose__store_of_value_reading, influences).
narrative_ontology:cs_reading_relation('17760e55-7d4a-49e6-8980-59614d3fe3ed', bitcoin_whitepaper_purpose__electronic_cash_reading, influences).
narrative_ontology:cs_axiom('17760e55-7d4a-49e6-8980-59614d3fe3ed', foundational, founder_authority_irrecoverable).
narrative_ontology:cs_axiom_status(founder_authority_irrecoverable, holdable).
narrative_ontology:cs_axiom_grounding('17760e55-7d4a-49e6-8980-59614d3fe3ed', founder_authority_irrecoverable, empirically_contingent).
narrative_ontology:cs_axiom('17760e55-7d4a-49e6-8980-59614d3fe3ed', foundational, textual_fidelity_unadjudicable).
narrative_ontology:cs_axiom_status(textual_fidelity_unadjudicable, holdable).
narrative_ontology:cs_axiom_grounding('17760e55-7d4a-49e6-8980-59614d3fe3ed', textual_fidelity_unadjudicable, conventional).
narrative_ontology:cs_reference_frame('17760e55-7d4a-49e6-8980-59614d3fe3ed', founder_adjudicated_kernel).
narrative_ontology:cs_drift_state('17760e55-7d4a-49e6-8980-59614d3fe3ed', post_2011_departure_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('17760e55-7d4a-49e6-8980-59614d3fe3ed', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, core_maintainers).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, hashpower_operators).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, full_node_operators).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, fork_claimant_factions).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, founder_identity_claimants).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, chain_split_holders).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, on_chain_transaction_users).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, interpretive_decentralization_doctrine).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, trustless_verification_epistemics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the reference implementation and the BIP process through which protocol changes are proposed and adopted. In the absence of a founder-oracle, their process role became the de facto interpretive authority: what they merge tends to become what Bitcoin does. They hold agenda-setting power no one elected them to, and they bear the corresponding attacks — every faction accuses them of hijacking or betraying the founder's intent, and no founder can be appealed to in their defense. Exit would mean abandoning stewardship of the project their professional identity is built around, with no legitimate successor mechanism to hand it to.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, core_maintainers, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, core_maintainers, beneficiary).

% Convert electricity into block validation and, in fork contests, decide which interpretation of the rules gets hashpower. The vacuum gives them veto power over any proposed interpretation — no change survives without their adoption — and arbitrage opportunities during splits, mining both sides and selling the losing chain's coins. Their hardware is chain-specific, so leaving the Bitcoin family means stranding capital; within the family they can move between the main chain and fork chains.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, hashpower_operators, beneficiary,
    organized, immediate, constrained, global).

% Run validating nodes that enforce the rules they verify for themselves. The vacuum means no authority can tell them what the protocol means; each operator's own verification is final, and this interpretive sovereignty is the direct benefit they collect. Leaving is constrained: the network they validate is the point of running the node, and switching to a fork chain means accepting a smaller, less secure network.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, full_node_operators, beneficiary,
    organized, biographical, constrained, global).

% Factions — large-block proponents, BCH and BSV projects — whose legitimacy claims rest on whitepaper fidelity that no authority can adjudicate. The vacuum protects their core asset: an unfalsifiable claim to be the true continuation of the founder's design. They can fork and take their chain with them; the exit is the fork, and the fork is also their strategy.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, fork_claimant_factions, beneficiary,
    organized, biographical, arbitrage, global).

% Persons who have claimed, or might claim, to be Satoshi Nakamoto or his heirs. The vacuum is their opportunity: no authority could definitively refute an identity claim, and the founder's roughly one million dormant coins make the claim potentially worth billions. They collect attention, funding, and legal standing from the claim — until enforcement arrives. Leaving is trapped: having staked identity and resources on the claim, recanting destroys everything the claim was built to gain. Since the 2024 COPA judgment, courts have begun foreclosing the claims.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, founder_identity_claimants, beneficiary,
    organized, biographical, trapped, global).

% Holders who bore the direct costs of fork contests: the 2017 split produced replay attacks, exchange confusion, and forced chain-selection decisions; the 2018 hash war destroyed value on both sides. They have no formal voice in protocol governance, selling during a split means realizing losses at the worst moment, and holding through means bearing the risk. Many are also ideologically committed to Bitcoin specifically, which binds them further.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, chain_split_holders, payer,
    moderate, biographical, constrained, global).

% People who used the chain for payments and bore the cost of interpretive stalemate: while the block-size contest froze capacity policy, fees spiked (December 2017 medians above thirty dollars; 2023 inscription congestion) and settlement became slow and unpredictable. Their exit is the most mobile in the story — other chains and fiat rails are available — which is exactly why the stalemate's costs reached them as attrition rather than as bargaining power.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, on_chain_transaction_users, payer,
    powerless, immediate, mobile, global).

% The protocol's author, silent since 2011, holding an estimated one million coins that have never moved. He is the one voice that could adjudicate every contested question — which reading is faithful, whether the block-size war departed from the design — and his unreachability is the condition the entire governance structure is organized around. Whether absent by death, choice, or key loss is unknown; the absence has so far been total and permanent. No seat in the structure can be filled by him, and every attempt to speak for him has been rejected or litigated.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, satoshi_nakamoto_absent, excluded,
    powerful, civilizational, trapped, global).

% Researchers, archivists, and journalists who document the founder's writings, the fork wars, and the identity claims. They see the full structure: the vacuum's anti-capture function, its costs, and the unfalsifiability of every fidelity claim. They collect nothing and pay nothing; their seat is the analytical record.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, protocol_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, core_maintainers).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of interpretive single-point-of-failure: it prevents any party — founder, foundation, or corporation — from holding decree power over the protocol's meaning. Legitimacy must be argued publicly and enforced by node operators verifying the rules themselves; the boundary of 'which chain is Bitcoin' is maintained through open contest rather than decree.
% TRANSFER_FUNCTION: Moves interpretive authority and agenda-setting power from a would-be founder-oracle to whoever accumulates de facto process control over the reference implementation — and, during fork contests, moves value from holders (split losses, replay-attack exposure) and payment users (fee spikes) to fork arbitrageurs and chain claimants.
% ABSENT_VOICES: The absent founder himself: the one voice that could settle the contest is structurally unreachable, which is the constraint. Ordinary holders had no formal seat during the fork wars — no on-chain governance vote, no deliberative channel; merchants and payment processors registered preferences only indirectly through exchange listing decisions. Their objections are preserved in forum archives and litigation records rather than in any governance mechanism.
% DISAPPEARANCE_RATIONALE: If the vacuum vanished overnight — an authoritative oracle suddenly able to speak — the governance structure would reorganize immediately: every fidelity claim would become adjudicable, fork proliferation would lose its premise, the maintainer seat's de facto agenda power would be subordinated to the oracle's rulings, and both sibling readings would have to submit to or secede from the founder's clarification. The entire arrangement is load-bearing on the absence.
% FOUNDING_PROBLEM: Nakamoto designed the protocol for his own absence: a founder-held interpretive oracle would be a single point of failure — coercible, capturable, and a trust dependency incompatible with a currency meant to need no trusted authority. The founding problem was how to remove founder-dependency from the protocol's authority structure; the vacuum is that removal, executed by departure.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the COPA v Wright judgment (2024) — a UK court with no stake in the protocol — found the founder-identity question live enough to litigate for years and the claim false, attesting both that the oracle seat is vacant and that capture attempts against it recur. Independent technology journalists' contemporaneous coverage (the 2014 misidentification episode and its rapid debunking by researchers outside any faction) attests the same. No beneficiary party's attestation is required for either fact.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62: the vacuum's standing costs are substantial but episodic — fork contests and stalemate spikes (2017 peak 0.72) alternate with process-legitimacy recovery (2021 trough 0.50), and the standing arrangement at interval end carries unresolved fidelity claims, periodic re-contestation (2023 inscriptions), and unaccountable agenda power at the maintainer seat. Suppression 0.66 with an unusual target: the vacuum's enforcement machinery does not suppress participation or exit — forks are open to anyone and payment users exit freely — it suppresses interpretive consolidation. Every attempt to fill the oracle seat has met escalating enforcement: the 2014 misidentification episode was publicly rejected within days; the 2015 XT fork met node-level counter-mobilization; identity claims met ostracism, exchange delistings, and finally litigation (COPA v Wright, 2024), the most coercive enforcement of the no-oracle rule in the record, driving the 2024–2026 suppression plateau. Theater 0.42: 'what would Satoshi do' invocation, genesis-block commemoration, and selective whitepaper quotation are real performative maintenance — each faction arms itself with the founder's ghost — but the vacuum is mostly maintained architecturally (nodes verify chains, not texts) and judicially, not theatrically. Accessibility collapse 0.30: understanding the vacuum collapses no alternatives — it proliferates them; anyone may fork, run any node, or hold any chain, the opposite of the natural-law profile. Resistance 0.55: the block-size war was substantially a revolt against interpretive stalemate itself — factions demanding a decision is resistance against a vacuum's consequences. All three tracked series share one eight-point grid. The extractiveness oscillation is one full fork-contest cycle (contest, spike, split or truce, recovery, re-contestation), and the oscillation is partly the extraction mechanism itself: each re-contestation re-prices protocol uncertainty and collects fees, attention, and forced chain-selection costs from the payer seats — an intermittent-reinforcement structure, not noise.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and the divergence is the point. From the core maintainer seat the vacuum is hard-won process legitimacy: they provide a real coordination service (review, testing, deliberation) and their agenda power accumulated through years of unglamorous work — the seat should compute rope-flavored. From the chain-split-holder seat the same arrangement is unresolvable imposition: no voice in protocol governance, forced chain selection, replay-attack exposure, and no authority to appeal to — snare-flavored. From the fork-faction seat the vacuum is exit freedom: the fork right is the discipline that keeps every interpreter honest, and the unfalsifiable fidelity claim is an asset. From the full-node-operator seat it is sovereignty: their own verification is final. The engine computes per-seat classifications from the structural data (power, exit, directionality); the authored tangled_rope claim does not adjudicate which seat's experience is 'the' constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: core_maintainers (agenda power accrues to them), hashpower_operators (veto plus split arbitrage), full_node_operators (interpretive sovereignty), fork_claimant_factions (unfalsifiable claims), founder_identity_claimants (credibility rents until enforcement arrives). Victims: chain_split_holders (split losses, replay attacks) and on_chain_transaction_users (fee volatility, attrition). Two overrides. Institutional → 0.25 for the core maintainer seat: the beneficiary declaration alone would derive near-full-subsidy directionality, but the seat also bears material counterflows — factional attack from every side, legal exposure, the burden of enforcing the no-oracle norm, and no founder to appeal to in its own defense; 0.25 encodes net beneficiary with real costs. Powerful → 0.50 for the absent founder: the constraint is constituted by his absence; he neither collects nor pays, and the derivation has no beneficiary or victim declaration to read for him, so symmetric encodes the structural fact. The payment-user seat is the most mobile in the story, which is why the stalemate's costs reached it as attrition rather than bargaining power; holders are more trapped (sunk financial and ideological commitment), so the same contest collects more from them per event.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — removing founder-dependency from the protocol's authority structure — was solved by the vacuum itself: the arrangement is the solution's standing form, not a mandate that outlived its function, so founding_problem_status=live with disappearance_verdict=world_rearranges is internally consistent and no zombie flag should fire. The tangled_rope classification blocks two mislabels. Calling the vacuum a rope would hide the fork-war costs that fell on holders and payment users while agenda power concentrated at the maintainer seat. Calling it a snare would erase the genuine anti-capture coordination — the vacuum leaves no founder to coerce, subpoena, or capture, which is a real and load-bearing service the costs ride on. The piton profile does not fit: the theater present (0.42) is ritual invocation around a live function, not performative maintenance of an atrophied one, and the enforcement machinery (delistings, litigation) is real coercion, not ritual. The demonstrated victims' coalition — the 2017 user-activated soft fork, in which node operators and holders forced a resolution the corporate roundtable had stalled — is the one convergence mechanism short of an oracle the record contains; it settled a practical question without settling any fidelity claim, which is why it is routed to the convergence_mechanism_status omega rather than treated as a refutation of the reading inside this story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_structural_delta,
    'This story is the nakamoto_oracle_opacity reading of kernel bitcoin_whitepaper_purpose; what structural content would the sibling readings (store_of_value_reading, electronic_cash_reading) carry instead, and where exactly does the disagreement sit?',
    'Author each sibling as its own constraint story and compare victim sets, epsilon over each reading''s own referent, and seat divergence. The disagreement is located at whether the whitepaper''s telos binds capacity policy — a question this reading holds unadjudicable.',
    'Under store_of_value_reading the binding constraint is decentralization primacy with capacity-demanding users as the pressured seat; under electronic_cash_reading the binding constraint is transactional usability with node operators as the pressured seat. This reading''s victims (fork-contest holders, stalemate users) are invariant across the siblings'' telos dispute, which is why the vacuum reading sits upstream of both.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Committer structure: one of three readings of the whitepaper-purpose kernel; siblings are separate constraints, not alternatives folded into this one.').

omega_variable(
    oracle_return_counterfactual,
    'If a cryptographically authenticated statement issued from the genesis keys, would the governance structure reorganize around the oracle, or would the community reject the oracle and defend the vacuum as a norm?',
    'The counterfactual is testable only by the event itself; proxies are community responses to partial authentication claims (the 2014 misidentification episode, the Wright litigation) and the pre-committed positions of major factions on founder authority.',
    'If an authenticated oracle would be accepted, the vacuum is a fact of absence and the constraint dissolves on the founder''s return; if it would be rejected, the vacuum is an enforced norm — suppression and requires_active_enforcement become load-bearing and the tangled_rope structure hardens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(oracle_return_counterfactual, empirical, 'Whether the no-oracle condition is mere absence or an actively maintained norm.').

omega_variable(
    vacuum_norm_vs_fact,
    'Is the interpretive vacuum maintained because no oracle exists (fact) or because the community enforces the no-oracle rule against claimants (norm)?',
    'Compare enforcement intensity against claimants of varying evidentiary strength: unauthenticated claims met ostracism; a sustained institutional claim met multi-year litigation (COPA v Wright, 2024). If enforcement scales with claim credibility, the norm account is supported.',
    'If normative, the suppression series measures real enforcement machinery and the vacuum is a maintained constraint; if mere fact, the enforcement measurements overstate and the arrangement drifts toward an unmanaged absence with rope-like structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vacuum_norm_vs_fact, empirical, 'Fact-of-absence versus enforced-norm accounts of the vacuum''s persistence.').

omega_variable(
    core_authority_extent,
    'How much of the de facto interpretive authority attributed to the core maintainer seat is genuine agenda power, and how much is coordination service that any competent maintainer group would provide?',
    'Compare BIP acceptance against hashpower and user-adoption divergences: the 2017 user-activated soft fork showed node-operator activation could override both miner and corporate interpretive preferences; measure how often maintainer preferences prevailed against organized opposition versus converged with it.',
    'If authority is mostly service provision, the core maintainer seat''s directionality drops toward pure coordination and gain attribution shifts toward fork factions and identity claimants; if agenda control is real, the capture-analog reading strengthens and the receipt surface hardens at the maintainer seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(core_authority_extent, empirical, 'Service-provision versus agenda-capture decomposition of the de facto interpreter''s power.').

omega_variable(
    convergence_mechanism_status,
    'Does the process route (BIP deliberation plus node-operator activation, as in the 2017 user-activated soft fork and the 2021 Taproot activation) constitute convergence on the kernel''s meaning, or only practical truce that leaves fidelity claims unsettled?',
    'Test whether post-consensus factions relinquish fidelity claims (BCH continued claiming whitepaper fidelity after losing hashpower and market share) and whether new contests reopen settled questions (the 2023 inscription controversy reopened block-space policy).',
    'If process convergence counts, the reading''s no-convergence premise weakens and the vacuum drifts toward rope; if only authoritative settlement counts, the vacuum''s tangled_rope structure holds and fork proliferation remains its standing output.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(convergence_mechanism_status, conceptual, 'Whether practical consensus without an oracle resolves or merely suspends the interpretive contest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 2011, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t2011, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2011, 0.12).
narrative_ontology:measurement(bitc_tr_t2014, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2014, 0.2).
narrative_ontology:measurement(bitc_tr_t2015, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2015, 0.3).
narrative_ontology:measurement(bitc_tr_t2017, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2017, 0.45).
narrative_ontology:measurement(bitc_tr_t2019, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2019, 0.52).
narrative_ontology:measurement(bitc_tr_t2021, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2021, 0.36).
narrative_ontology:measurement(bitc_tr_t2023, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2023, 0.4).
narrative_ontology:measurement(bitc_tr_t2026, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(bitc_be_t2011, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2011, 0.28).
narrative_ontology:measurement(bitc_be_t2014, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2014, 0.36).
narrative_ontology:measurement(bitc_be_t2015, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2015, 0.44).
narrative_ontology:measurement(bitc_be_t2017, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2017, 0.72).
narrative_ontology:measurement(bitc_be_t2019, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2019, 0.64).
narrative_ontology:measurement(bitc_be_t2021, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2021, 0.5).
narrative_ontology:measurement(bitc_be_t2023, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2023, 0.6).
narrative_ontology:measurement(bitc_be_t2026, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2026, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t2011, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2011, 0.2).
narrative_ontology:measurement(bitc_su_t2014, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2014, 0.28).
narrative_ontology:measurement(bitc_su_t2015, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2015, 0.38).
narrative_ontology:measurement(bitc_su_t2017, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2017, 0.58).
narrative_ontology:measurement(bitc_su_t2019, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2019, 0.64).
narrative_ontology:measurement(bitc_su_t2021, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2021, 0.56).
narrative_ontology:measurement(bitc_su_t2023, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2023, 0.6).
narrative_ontology:measurement(bitc_su_t2026, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2026, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, identity_coordination).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_whitepaper_purpose__store_of_value_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_whitepaper_purpose__electronic_cash_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the whitepaper's purpose' decomposes per the epsilon-invariance principle into three stories. This file (nakamoto_oracle_opacity) authors epsilon over the interpretive-vacuum governance condition itself; bitcoin_whitepaper_purpose__store_of_value_reading authors epsilon over the decentralization-primacy arrangement; bitcoin_whitepaper_purpose__electronic_cash_reading authors epsilon over the cash-telos arrangement. The upstream story (this one) influences both siblings because the vacuum is the condition under which their fidelity claims remain simultaneously sustainable and unadjudicable; each sibling file links back via its own network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, institutional, 0.25).
constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, powerful, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
