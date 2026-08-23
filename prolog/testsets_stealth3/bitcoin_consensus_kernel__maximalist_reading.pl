% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: Founding-Covenant Monetary Immutability (Maximalist Reading)
 *   domain: cryptoeconomics/monetary_systems/distributed_consensus
 *
 * SUMMARY:
 *   A decentralized monetary network froze its issuance schedule and supply
 *   cap at founding, and a governing coalition of holders, node operators,
 *   and reference-implementation maintainers defends that freeze as a
 *   covenant: any change to the founding monetary parameters is treated as a
 *   breach of faith with every participant. The arrangement solves a real
 *   credible-commitment problem — no committee can inflate — while
 *   simultaneously converting foreclosed protocol optionality and frozen
 *   block space into scarcity assurance that capitalizes into holder
 *   balances. This file instantiates ONE reading of the contested
 *   bitcoin_consensus_kernel: the maximalist reading, under which the
 *   whitepaper establishes immutable monetary policy and any change violates
 *   the founding covenant. The sibling readings (utility_reading,
 *   pragmatic_synthesis) are separate constraints in separate files with
 *   their own epsilon, victim sets, and classifications; they are not
 *   averaged into this one. EPSILON REFERENT: the standing arrangement under
 *   contest — the live governance regime in which monetary parameters are
 *   defended as untouchable — assessed by this reading's own lights. This
 *   reading endorses the standing arrangement, so the rule that the endorsed
 *   alternative is never the referent binds trivially; the referent is the
 *   covenant-enforced freeze itself, and the reading-indexed value credits
 *   its genuine coordination yield while recording the asymmetric transfer
 *   the structural record shows. KEY AGENTS (by structural relationship): -
 *   long_term_holders: primary beneficiary (organized/mobile) — scarcity
 *   assurance capitalizes into their balances; they defend the freeze -
 *   early_adopters: concentrated beneficiary (powerful/arbitrage) — captured
 *   the historical windfall; fund covenant advocacy - mining_pool_operators:
 *   enforcement-side collector (powerful/constrained) — collect scheduled
 *   issuance; specialized hardware locked to this chain -
 *   bitcoin_core_maintainers: agenda setter (institutional/identity_locked) —
 *   gatekeep the reference implementation; close parameter-adjacent proposals
 *   - full_node_operators: enforcement surface (organized/constrained) —
 *   collectively ratify or refuse any rule change - layer_two_builders:
 *   primary target (moderate/constrained) — build around limits they cannot
 *   move - protocol_change_proposers: target (moderate/constrained) —
 *   proposals adjacent to monetary parameters die in review -
 *   transaction_fee_payers: diffuse target (powerless/mobile) — bid for
 *   frozen block space - fork_community_members: excluded voice
 *   (organized/trapped) — already cast out for advocating rule changes -
 *   monetary_economists: analytical observer (moderate/analytical) — dispute
 *   whether absolute fixity is optimal
 *
 * KEY AGENTS:
 *   - long_term_holders: primary beneficiary (organized/mobile) — savings thesis depends on the schedule never changing; defend fixity through nodes, advocacy, and grant funding
 *   - early_adopters: concentrated beneficiary (powerful/arbitrage) — accumulated before the schedule's credibility was priced; deep liquidity lets them diversify while advocating
 *   - mining_pool_operators: enforcement-side collector (powerful/constrained) — collect block subsidies and fees under the fixed schedule; hardware has no use off this chain
 *   - bitcoin_core_maintainers: agenda setter (institutional/identity_locked) — review and close proposals; standing rests on stewardship of the original design
 *   - full_node_operators: enforcement surface (organized/constrained) — run validating software; a change becomes real only when they adopt it; most also hold balances tied to the rules
 *   - layer_two_builders: primary target (moderate/constrained) — anchor businesses to this chain's liquidity while requesting covenant-compatible features that wait years
 *   - protocol_change_proposers: target (moderate/constrained) — spend years on proposals that never merge; reputation rides on acceptance they cannot compel
 *   - transaction_fee_payers: diffuse target (powerless/mobile) — bid for block space fixed before they arrived; main lever is taking activity elsewhere
 *   - fork_community_members: excluded voice (organized/trapped) — split off in past conflicts; would re-enter the conversation if their rule sets were acknowledged
 *   - monetary_economists: analytical observer (moderate/analytical) — publish for and against absolute fixity; no enforcement role
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__maximalist_reading, 0.7).
domain_priors:suppression_score(bitcoin_consensus_kernel__maximalist_reading, 0.72).
domain_priors:theater_ratio(bitcoin_consensus_kernel__maximalist_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__maximalist_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_consensus_kernel__maximalist_reading, "Founding-Covenant Monetary Immutability (Maximalist Reading)").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__maximalist_reading, "cryptoeconomics/monetary_systems/distributed_consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__maximalist_reading, '5fb8a402-af74-460b-9c6e-fee441fbb872').
narrative_ontology:cs_kernel_codification('5fb8a402-af74-460b-9c6e-fee441fbb872', fixed_text).
narrative_ontology:cs_authority_grounding('5fb8a402-af74-460b-9c6e-fee441fbb872', lineage).
narrative_ontology:cs_interpretation_layer_present('5fb8a402-af74-460b-9c6e-fee441fbb872').
narrative_ontology:cs_reading_relation('5fb8a402-af74-460b-9c6e-fee441fbb872', bitcoin_consensus_kernel__utility_reading, forecloses).
narrative_ontology:cs_reading_relation('5fb8a402-af74-460b-9c6e-fee441fbb872', bitcoin_consensus_kernel__pragmatic_synthesis, forecloses).
narrative_ontology:cs_axiom('5fb8a402-af74-460b-9c6e-fee441fbb872', foundational, whitepaper_parameters_are_binding_covenant).
narrative_ontology:cs_axiom_status(whitepaper_parameters_are_binding_covenant, holdable).
narrative_ontology:cs_axiom_grounding('5fb8a402-af74-460b-9c6e-fee441fbb872', whitepaper_parameters_are_binding_covenant, deontological).
narrative_ontology:cs_axiom('5fb8a402-af74-460b-9c6e-fee441fbb872', foundational, credible_scarcity_requires_no_revision_path).
narrative_ontology:cs_axiom_status(credible_scarcity_requires_no_revision_path, holdable).
narrative_ontology:cs_axiom_grounding('5fb8a402-af74-460b-9c6e-fee441fbb872', credible_scarcity_requires_no_revision_path, instrumental).
narrative_ontology:cs_reference_frame('5fb8a402-af74-460b-9c6e-fee441fbb872', genesis_whitepaper_monetary_covenant).
narrative_ontology:cs_drift_state('5fb8a402-af74-460b-9c6e-fee441fbb872', contemporary_post_scaling_wars, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5fb8a402-af74-460b-9c6e-fee441fbb872', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, long_term_holders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, early_adopters).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, mining_pool_operators).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, layer_two_builders).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, protocol_change_proposers).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, transaction_fee_payers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, full_node_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold bitcoin as long-term savings denominated in a supply schedule they expect never to change. Their savings thesis depends on the emission schedule and cap staying fixed; they defend that fixity through node operation, social advocacy, and funding development grants aligned with it. Selling is technically easy, but most treat exit as betrayal of their own thesis and hold through controversies.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, long_term_holders, beneficiary,
    organized, generational, mobile, global).

% Accumulated large positions when the currency was illiquid and the fixed schedule's credibility was untested. Their wealth concentrates the appreciation that followed mass adoption of the schedule. Many are public voices whose standing rests on having been right early; they fund media, conferences, and lobbying that defend the founding parameters. Deep liquidity lets them diversify quietly, decoupling personal exposure from the outcome they advocate.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, early_adopters, beneficiary,
    powerful, generational, arbitrage, global).

% Aggregate hashpower and collect block subsidies and transaction fees under the fixed emission schedule. They extend whichever chain follows the rules they compiled, and can in principle redirect hashpower, but their specialized hardware has no use outside this chain, and the 2017 episode showed that deviating from the socially expected rules costs them revenue. They take the schedule as given and price hardware against it.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, mining_pool_operators, beneficiary,
    powerful, immediate, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__maximalist_reading, mining_pool_operators, agenda_setter).

% Maintain the reference implementation and review proposed changes. Proposals touching the emission schedule, the supply cap, or other founding parameters are closed without merge, in line with stated conservatism about consensus changes. Their reputations are built on stewardship of the original design; advocating revision would cost them their standing in the community they serve, so the conservative posture is also career-preserving.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, bitcoin_core_maintainers, agenda_setter,
    institutional, biographical, identity_locked, global).

% Run validating software that rejects blocks violating the rules they compiled in. Collectively they are the surface on which any rule change becomes real: a change happens only when they adopt it. Most also hold balances whose value depends on the rules staying as they are, and running a node is a standing commitment of time and attention that ties them to the chain they validated into.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, full_node_operators, agenda_setter,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__maximalist_reading, full_node_operators, beneficiary).

% Build payment channels, sidechains, and bridges that must route around base-layer limits that cannot be moved. Requests for covenant-compatible opcodes have waited years through repeated review cycles. Their businesses are anchored to this chain's liquidity and brand; rebuilding on a rival chain means surrendering the network effects they exist to serve.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, layer_two_builders, payer,
    moderate, biographical, constrained, global).

% Propose soft forks and research improvements. Anything adjacent to the monetary parameters meets reflexive refusal framed as fidelity to the founding design; several proposers have spent years on work that never reached merge. Their professional standing inside the ecosystem depends on acceptance they cannot compel, and moving to friendlier ecosystems means abandoning the largest deployed base.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, protocol_change_proposers, payer,
    moderate, biographical, constrained, global).

% Send transactions and bid for limited block space whose quantity was fixed by decisions made before they arrived. During congestion they pay fees that scale with demand against frozen supply, or wait, or move activity to other rails. Individually they have no channel into governance; their main lever is taking their business elsewhere, which many do.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, transaction_fee_payers, payer,
    powerless, immediate, mobile, global).

% Backed rule changes in past conflicts, split off with their holdings when the changes were refused, and now maintain smaller economies on the forked chains. They argue the founding parameters were always meant to evolve and would re-enter the conversation if invited; their absence is maintained by the main chain's refusal to acknowledge their rule sets as legitimate continuations.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, fork_community_members, excluded,
    organized, biographical, trapped, global).

% Study whether a permanently fixed supply schedule is optimal, comparing outcomes against elastic-supply monetary regimes and modeling the welfare effects of frozen block space. They publish analyses both supporting and contesting the fixity; they hold no enforcement role, and their recommendations reach governance only through persuasion.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, monetary_economists, observer,
    moderate, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_consensus_kernel__maximalist_reading, long_term_holders).
narrative_ontology:fixing_cost_class(bitcoin_consensus_kernel__maximalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates monetary expectations among millions of participants who need not trust any issuer: a fixed emission schedule and capped supply give everyone the same answer to 'how much money will exist', solving the credible-commitment problem that historically required trusting central banks or private issuers.
% TRANSFER_FUNCTION: Moves protocol optionality and congestion costs from builders and transactors to holders: every foreclosed change converts would-be flexibility into scarcity assurance that capitalizes into holder balances, and frozen block space converts user demand into fees bid against fixed supply.
% ABSENT_VOICES: Fork-community members who already argued for evolvable rules and were cast out; prospective users priced off the chain by frozen block space who never became participants and so never gained a voice; and future generations who will inherit parameters chosen without them. Legitimacy is adjudicated by current holders and node operators, so those who would bear a changed rule set but do not yet hold coins have no seat.
% DISAPPEARANCE_RATIONALE: If the covenant enforcement vanished overnight — if consensus formed that monetary parameters were revisable — the scarcity premium would unwind as holders repriced revision risk, competing rule-set proposals would fragment the chain, and the monetary credibility that substitutes for an issuer would need rebuilding from scratch. Portfolios, mining investment, and the layered economy all assume the schedule as given.
% FOUNDING_PROBLEM: Digital cash previously required a trusted issuer to prevent double-spending and to manage supply; the founding design aimed to remove that trusted party with a fixed, publicly verifiable issuance schedule that no committee could inflate.
% FOUNDING_PROBLEM_CORROBORATION: The double-spending problem and the failure mode of trusted issuers are attested by the academic cryptography literature (Byzantine-agreement and e-cash research predating the whitepaper) and by monetary historians documenting issuer debasement episodes — sources outside the holding community. Corroboration for the covenant as the necessary FORM of the solution is weaker: monetary economists actively dispute whether absolute fixity is optimal, and that dispute is carried in peer-reviewed literature rather than settled by the beneficiary coalition's assertions.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__maximalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__maximalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__maximalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_consensus_kernel__maximalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_consensus_kernel__maximalist_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is high (0.70 at interval end) but damped below what a hostile reading would author: the maximalist seat credits the covenant with real coordination yield (credible scarcity without an issuer), so the reading-indexed epsilon records substantial-but-not-total transfer — foreclosed optionality and frozen block space capitalized into holder balances. Suppression (0.72) is a raw structural property, unscaled by power or scope: enforcement runs through review gatekeeping in the reference implementation, the demonstrated economic strangulation of divergent chains, and hardware specificity that locks the enforcement-side collectors to this chain. Theater ratio (0.30) reflects a growing ritual layer — anniversary commemorations, proof-of-keys events, maximalist media cycles — atop a functional validation core; the dip after 2017 marks the period when enforcement was most functional (the user-activated soft-fork standoff) and ritual least needed. Accessibility collapse (0.55): within the system, alternatives are fully collapsed — no legitimate channel touches monetary parameters — but ecosystem-level alternatives (rival chains, layered rails) persist at network-effect cost, so collapse is partial overall. Resistance (0.60) is real and documented: the block-size wars, the 2017 chain split, and continuing opcode campaigns. The measurement series run on ONE shared time grid (2009, 2012, 2015, 2017, 2019, 2021, 2023, 2025) so every tracked metric is authored at every examined point; trajectories show an enforcement ratchet (suppression_requirement climbing steeply 2015–2019 as the social enforcement machinery matured, then plateauing) alongside extraction accumulation as stake concentration grew. Coordination type is resource_allocation: the covenant's dominant function is allocating credible scarcity and rationing fixed block space across global participants; the type-default floor applies, no override.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the holder and maintainer seats, the arrangement is experienced as covenant-keeping — the freeze is the product, fidelity is the ethic, and the costs borne elsewhere are the price of admission to sound money. From the builder and proposer seats, the same structure operates as foreclosure: years of work die in review, and the largest deployed base is held hostage to parameters chosen before their use cases existed. From the fee-payer seat it is a queue with a price that scales against supply fixed before they arrived. The engine computes these per-seat classifications from the structural data (power, exit, role); the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d: long_term_holders sit near the beneficiary end (mobile exit — selling is easy — damps d slightly, but their advocacy and node-running tie them in); early_adopters sit nearest the beneficiary end (arbitrage-grade exit plus concentrated gains). Mining_pool_operators derive mid-low d: they collect scheduled issuance but their constrained exit (chain-specific hardware) and enforcement role pull toward symmetry. Full_node_operators mix enforcement and holding — mid d. Bitcoin_core_maintainers are agenda setters whose careers and standing ride on non-revision; identity_locked exit places their effective relationship nearer the invested-than-neutral range despite administering rather than collecting. Victims drive high d: layer_two_builders and protocol_change_proposers are constrained (businesses and reputations anchored to this chain), placing them near the full-target end; transaction_fee_payers are powerless with immediate horizons but mobile exit (other rails exist), which damps their d somewhat without removing them from the target set. Fork_community_members are trapped and excluded — high d, no seat. No directionality overrides were needed: the derivation from declared roles, power, and exit produces the right shape.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — digital cash without a trusted issuer, with a schedule no committee could inflate — remains live, so this is not a resolved-mandatrophy case and mandatrophy_resolved is deliberately not declared. The watch-item is scope creep of the defensive function: the covenant was founded to prevent monetary debasement, but its contemporary operation blocks ALL change, including changes that would relieve congestion or enable covenant-compatible features. If base-layer problems accumulate while the covenant blocks every response, the gap between mandate (protect the schedule) and function (block everything) widens toward inertial persistence — the R5 mismatch consumer sees founding_problem_status=live paired with disappearance_verdict=world_rearranges, so no zombie flag fires today, but the T17-style accumulation signal in the extractiveness series is the early indicator to watch. The classification prevents mislabeling in both directions: reading the arrangement as pure coordination ignores the documented foreclosure of builders and the cohort-concentrated gains; reading it as pure extraction ignores the genuine credible-commitment function that millions voluntarily buy into. The hybrid is the honest center.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This story instantiates the maximalist reading of the bitcoin_consensus_kernel (''any change violates the founding covenant''). Would the utility_reading or the pragmatic_synthesis instantiate a structurally different constraint over the same protocol history?',
    'Observe which reading governs actual proposal adjudication in the next governance crisis: if upper-layer innovation is routinely ratified as covenant-consistent, the synthesis reading governs practice; if any proposal adjacent to the founding parameters dies in review regardless of layer, the maximalist reading governs.',
    'Sibling readings produce different victim sets (the utility_reading counts users of frozen block space as primary targets; the pragmatic_synthesis narrows targets to blocked base-layer changes) and author different epsilon over the identical referent. Classification of this file is valid only for the maximalist seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: which reading of the founding text governs the kernel.').

omega_variable(
    natural_invariant_vs_interested_construction,
    'Is the capped supply schedule a discovered Schelling invariant that would re-emerge under any founding, or a constructed rule sustained because identifiable cohorts profit from its fixity?',
    'Counterfactual design analysis plus the 2017 near-change episode: the parameters survived a coordinated attempt to alter adjacent limits only through explicit social mobilization and economic pressure on divergent chains, which indicates construction; a schedule that re-converged after every perturbation without mobilization would indicate invariant.',
    'If constructed-with-beneficiaries, mountain-flavored framings of the cap (''digital gold physics'') are false summits and the arrangement sits with the extractive hybrids; if invariant, the fixity carries negligible extraction and the coordination reading dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_invariant_vs_interested_construction, conceptual, 'Whether the frozen schedule is a natural law or an enforced construction with beneficiaries.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the enforcement that keeps monetary parameters frozen primarily structural (review gatekeeping, economic strangulation of divergent chains, application-specific hardware lock-in) or internalized (participant identity fused with the untouchable founding design)?',
    'Post-exit trajectory: track proposers and users who left for other ecosystems; if their revision advocacy persists undamped outside the enforcement environment, the internalized share is small; if they abandon change-advocacy after exiting, identity carried the suppression with them.',
    'Internalized suppression raises effective enforcement above the structural measure and makes the arrangement robust to gatekeeper turnover; purely structural suppression would decay if review gatekeeping ever liberalized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized share of the enforcement keeping parameters frozen.').

omega_variable(
    cohort_benefit_concentration,
    'Do late and small holders net-benefit from the frozen schedule, or do congestion costs and foreclosed scaling exceed their scarcity gains, concentrating net benefit in early cohorts?',
    'Cohort-level accounting: compare per-cohort appreciation attributable to scarcity assurance against per-cohort fee burdens and displacement to other rails.',
    'If net benefit concentrates in early cohorts, the beneficiary declaration narrows sharply and the extraction asymmetry sharpens; if broadly shared across cohorts, the coordination reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cohort_benefit_concentration, empirical, 'Distribution of net benefit across holder cohorts.').

omega_variable(
    l2_absorption_of_frozen_blockspace,
    'Does the second-layer economy absorb the costs of frozen base-layer throughput, converting would-be targets into accommodated users, or do the costs persist at the base layer?',
    'Fee-market and settlement-share data: if on-chain fee pressure falls as layered share grows without any base-layer change, absorption is real; recurring base-layer congestion spikes indicate unresolved cost-bearing.',
    'Full absorption would shrink the victim set and soften measured extraction; persistent base-layer cost-bearing keeps scalability participants firmly in the victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(l2_absorption_of_frozen_blockspace, empirical, 'Whether layered rails absorb the costs the frozen schedule creates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__maximalist_reading, 2009, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bck_max_read_tr_t2009, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 2009, 0.05).
narrative_ontology:measurement(bck_max_read_tr_t2012, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 2012, 0.1).
narrative_ontology:measurement(bck_max_read_tr_t2015, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 2015, 0.18).
narrative_ontology:measurement(bck_max_read_tr_t2017, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 2017, 0.28).
narrative_ontology:measurement(bck_max_read_tr_t2019, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 2019, 0.26).
narrative_ontology:measurement(bck_max_read_tr_t2021, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 2021, 0.27).
narrative_ontology:measurement(bck_max_read_tr_t2023, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 2023, 0.29).
narrative_ontology:measurement(bck_max_read_tr_t2025, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(bck_max_read_be_t2009, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 2009, 0.25).
narrative_ontology:measurement(bck_max_read_be_t2012, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 2012, 0.35).
narrative_ontology:measurement(bck_max_read_be_t2015, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 2015, 0.5).
narrative_ontology:measurement(bck_max_read_be_t2017, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 2017, 0.62).
narrative_ontology:measurement(bck_max_read_be_t2019, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 2019, 0.66).
narrative_ontology:measurement(bck_max_read_be_t2021, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 2021, 0.68).
narrative_ontology:measurement(bck_max_read_be_t2023, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 2023, 0.69).
narrative_ontology:measurement(bck_max_read_be_t2025, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 2025, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(bck_max_read_su_t2009, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 2009, 0.2).
narrative_ontology:measurement(bck_max_read_su_t2012, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 2012, 0.3).
narrative_ontology:measurement(bck_max_read_su_t2015, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 2015, 0.45).
narrative_ontology:measurement(bck_max_read_su_t2017, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 2017, 0.62).
narrative_ontology:measurement(bck_max_read_su_t2019, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 2019, 0.68).
narrative_ontology:measurement(bck_max_read_su_t2021, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 2021, 0.7).
narrative_ontology:measurement(bck_max_read_su_t2023, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 2023, 0.71).
narrative_ontology:measurement(bck_max_read_su_t2025, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__maximalist_reading, resource_allocation).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__utility_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__pragmatic_synthesis).

% DUAL FORMULATION NOTE:
% The colloquial label 'Bitcoin's monetary policy' decomposes into three structurally distinct constraints — one per reading of the bitcoin_consensus_kernel — per the epsilon-invariance principle. This file is the maximalist reading: any change violates the founding covenant; high epsilon against protocol evolution; victims in the scalability/innovation layers. The utility_reading authors the same protocol history as a minimum-viable-mechanism constraint with different victims (users of frozen block space) and higher epsilon; the pragmatic_synthesis splits base from layers and authors intermediate epsilon. Each file links the others via network.affects_constraints. Upstream/downstream texture: the maximalist reading's rhetorical dominance raises the legitimacy bar both siblings must clear in public dispute, so this reading shapes the operating environment of the others even where it does not displace their holders.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
