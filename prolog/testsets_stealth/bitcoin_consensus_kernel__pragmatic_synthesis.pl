% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__pragmatic_synthesis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_consensus_kernel__pragmatic_synthesis, []).

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
 *   constraint_id: bitcoin_consensus_kernel__pragmatic_synthesis
 *   human_readable: Bitcoin Consensus Kernel — Pragmatic Synthesis (Immutable Base, Open Layers)
 *   domain: cryptoeconomics/monetary_systems/distributed_consensus
 *
 * SUMMARY:
 *   Between 2015 and 2017 the Bitcoin community fought an acute governance
 *   war over the block size limit that exposed the unresolved question at the
 *   kernel's heart: does the whitepaper establish an immutable monetary
 *   covenant (the maximalist reading) or a minimum viable mechanism open to
 *   iteration (the utility reading)? The arrangement this story models — the
 *   pragmatic synthesis — is the settlement that ended the crisis: base-layer
 *   monetary rules are held immutable, while upper layers (Lightning
 *   channels, sidechains, federated chains) constitute the legitimate
 *   innovation space, partitioning the change surface so the highest-stakes
 *   parameters are frozen and everything else is either high-friction at base
 *   or permissionless above. The epsilon referent is this standing layered
 *   arrangement, assessed by the reading's own lights — which endorse it — so
 *   base extractiveness is low; the residual costs it acknowledges are
 *   congestion pricing of base access, foreclosed base-layer research
 *   programs, and the ideological cost both camps pay, the settlement's
 *   principal casualty being the coherence of either pure program. The
 *   arrangement is claimed as a scaffold: a transitional settlement carrying
 *   the community from founding crisis to mature layered system, with a
 *   conditional rather than dated sunset. Per Rule 1 this file authors only
 *   the pragmatic reading as one clean constraint; the sibling readings are
 *   separate stories linked via network.affects_constraints. The claim and
 *   the metrics are independent authored facts: claimed_type is scaffold; the
 *   metrics describe low-extraction, decaying-enforcement operation with slow
 *   ritual accumulation, and the engine computes per-seat classifications
 *   from the structural data.
 *
 * KEY AGENTS:
 *   - core_protocol_maintainers: agenda-setter seat (administer the BIP change process; monetary parameters excluded) [organized/identity_locked]
 *   - full_node_operators: enforcement backstop and governance beneficiary (activation requires them; their veto position is preserved by the settlement) [organized/constrained]
 *   - long_horizon_holders: primary beneficiary seat (fixed-supply premise of the store-of-value thesis) [moderate/identity_locked]
 *   - layer_two_developers: sanctioned innovators (the settlement designates layers as the legitimate innovation domain) [moderate/mobile]
 *   - small_value_onchain_users: payer seat (congestion pricing pushes them toward weaker-trust layer custody) [powerless/constrained]
 *   - onchain_scaling_developers: payer seat (base-layer research program foreclosed; many exited post-2017) [moderate/mobile]
 *   - maximalist_covenant_purists: partial beneficiary and residual objector (received base immutability; contest layer legitimacy) [organized/identity_locked]
 *   - mining_pool_operators: dual-positioned (fee scarcity sustains revenue; foreclosed from throughput expansion; capitulating counterparty in 2017) [powerful/mobile]
 *   - protocol_governance_researchers: analytical observer (study the crisis and settlement from outside all camps) [analytical/analytical]
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__pragmatic_synthesis, 0.16).
domain_priors:suppression_score(bitcoin_consensus_kernel__pragmatic_synthesis, 0.22).
domain_priors:theater_ratio(bitcoin_consensus_kernel__pragmatic_synthesis, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, extractiveness, 0.16).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__pragmatic_synthesis, scaffold).
narrative_ontology:human_readable(bitcoin_consensus_kernel__pragmatic_synthesis, "Bitcoin Consensus Kernel — Pragmatic Synthesis (Immutable Base, Open Layers)").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__pragmatic_synthesis, "cryptoeconomics/monetary_systems/distributed_consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__pragmatic_synthesis).
narrative_ontology:has_sunset_clause(bitcoin_consensus_kernel__pragmatic_synthesis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__pragmatic_synthesis, 'f3564858-3af5-4929-91ba-d8cba9b7c488').
narrative_ontology:cs_kernel_codification('f3564858-3af5-4929-91ba-d8cba9b7c488', fixed_text).
narrative_ontology:cs_authority_grounding('f3564858-3af5-4929-91ba-d8cba9b7c488', practice).
narrative_ontology:cs_interpretation_layer_present('f3564858-3af5-4929-91ba-d8cba9b7c488').
narrative_ontology:cs_reading_relation('f3564858-3af5-4929-91ba-d8cba9b7c488', bitcoin_consensus_kernel__maximalist_reading, influences).
narrative_ontology:cs_reading_relation('f3564858-3af5-4929-91ba-d8cba9b7c488', bitcoin_consensus_kernel__utility_reading, influences).
narrative_ontology:cs_axiom('f3564858-3af5-4929-91ba-d8cba9b7c488', foundational, base_monetary_immutability_is_instrumental).
narrative_ontology:cs_axiom_status(base_monetary_immutability_is_instrumental, holdable).
narrative_ontology:cs_axiom_grounding('f3564858-3af5-4929-91ba-d8cba9b7c488', base_monetary_immutability_is_instrumental, instrumental).
narrative_ontology:cs_axiom('f3564858-3af5-4929-91ba-d8cba9b7c488', foundational, layer_innovation_cannot_violate_kernel).
narrative_ontology:cs_axiom_status(layer_innovation_cannot_violate_kernel, holdable).
narrative_ontology:cs_axiom_grounding('f3564858-3af5-4929-91ba-d8cba9b7c488', layer_innovation_cannot_violate_kernel, conventional).
narrative_ontology:cs_reference_frame('f3564858-3af5-4929-91ba-d8cba9b7c488', immutable_base_open_layers_partition).
narrative_ontology:cs_drift_state('f3564858-3af5-4929-91ba-d8cba9b7c488', contemporary_post_settlement, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('f3564858-3af5-4929-91ba-d8cba9b7c488', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__pragmatic_synthesis, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__pragmatic_synthesis, long_horizon_holders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__pragmatic_synthesis, layer_two_developers).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__pragmatic_synthesis, full_node_operators).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__pragmatic_synthesis, core_protocol_maintainers).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__pragmatic_synthesis, maximalist_covenant_purists).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__pragmatic_synthesis, mining_pool_operators).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__pragmatic_synthesis, small_value_onchain_users).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__pragmatic_synthesis, onchain_scaling_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__pragmatic_synthesis, mining_pool_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Review and merge proposed changes to the base protocol through the BIP process and decide which proposals reach the activation stage. Since the 2017 settlement they administer a deliberately narrowed agenda: monetary parameters are off the table, and non-monetary upgrades proceed only with overwhelming activation support. Their stewardship identity is fused with the rule that the monetary rules never change; leaving would mean abandoning the project they curate. They collect no fees and absorb the political blame for every congestion episode.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, core_protocol_maintainers, agenda_setter,
    organized, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__pragmatic_synthesis, core_protocol_maintainers, beneficiary).

% Run the consensus rules on their own hardware and refuse blocks that break them; nothing activates on the network without a substantial share of them upgrading. The settlement preserves their position as the network's backstop: monetary rules cannot change over their refusal. They pay for that position with hardware, bandwidth, and permanent vigilance rather than receiving any fee, and exit means either trusting someone else's validation or leaving the asset entirely.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, full_node_operators, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__pragmatic_synthesis, full_node_operators, agenda_setter).

% Hold the asset across years or decades as a fixed-supply store of value; the settlement's frozen monetary policy is the premise of their thesis. Their identity and portfolio are fused with the twenty-one million cap, and selling into another monetary system would repudiate it. They pay nothing directly and bear no dilution, but their position depends on the settlement holding against both rival camps.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, long_horizon_holders, beneficiary,
    moderate, generational, identity_locked, global).

% Build payment channels, sidechains, and other second-layer systems on top of the unchanging base. The settlement designates their domain as the legitimate innovation space, concentrating talent, capital, and attention on their work. They could move to other chains and some do, but the base layer's security and liquidity are what their systems monetize.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, layer_two_developers, beneficiary,
    moderate, biographical, mobile, global).

% Transact in amounts too small to justify high base-layer fees. Deliberate blockspace scarcity prices them toward second-layer systems whose trust models — custodial channels, watchtowers, federated operators — are weaker than base-layer self-custody. They hold no seat in governance and no organized voice, and each alternative (staying on base and paying, accepting custodial layers, or leaving for other chains) carries real costs.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, small_value_onchain_users, payer,
    powerless, biographical, constrained, global).

% Research and advocate base-layer throughput improvements: larger blocks, new signature processing, base-level scaling. The settlement forecloses their program at the base, where the consensus environment is calibrated to refuse throughput and monetary-adjacent changes. Many exercised the available exit after 2017, building on the forked chain or elsewhere; those who remain work against the settlement's grain.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, onchain_scaling_developers, payer,
    moderate, biographical, mobile, global).

% Hold that the whitepaper's monetary policy is a binding founding covenant. The settlement honors half their program — the base is protected — while the layer economy's growth, speculative assets, and non-monetary uses of blockspace dilute the covenant's spirit in their view. They received their core demand while their broader program, a purist monetary network, receded; their identity is fused with the covenant reading and they cannot exit without repudiating it.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, maximalist_covenant_purists, beneficiary,
    organized, generational, identity_locked, global).

% Aggregate hash power and collect block subsidies plus transaction fees. Deliberate blockspace scarcity sustains the fee market that must replace declining subsidies as issuance halves, and these operators were the counterparty that capitulated in the 2017 activation standoff. Larger blocks would raise short-term throughput revenue but erode the scarcity their fee income depends on. Hash power is mobile across SHA-256 chains, though specialized hardware anchors them to the ecosystem.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, mining_pool_operators, beneficiary,
    powerful, immediate, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__pragmatic_synthesis, mining_pool_operators, payer).

% Study the 2015–2017 crisis and the settlement that ended it from outside every camp, publishing analyses of activation mechanics, censorship episodes, and governance capacity. They hold no coins at stake in the settlement's terms and no vote in its operation.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, protocol_governance_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_consensus_kernel__pragmatic_synthesis, mining_pool_operators).
narrative_ontology:fixing_cost_class(bitcoin_consensus_kernel__pragmatic_synthesis, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The settlement partitions the protocol's change surface to end a governance war that a leaderless network had no court to resolve: monetary parameters are frozen, removing the highest-stakes disputes from the agenda entirely; non-monetary base upgrades proceed only through near-unanimous activation; and all other innovation is channeled to layers that require no network-wide consensus. Each camp receives a domain it can accept, and the network receives a stable foundation under a growing application stack.
% TRANSFER_FUNCTION: Moves the right to change the system from the whole contested community to a partitioned structure: monetary authority to nobody (frozen), base-layer development authority to the maintainer-and-node-consensus process (non-monetary matters only), innovation authority to layer builders (permissionless). Materially, it moves transaction flow and fee revenue gradually from base to layers, and it moves ideological cost onto both camps — each surrenders its maximal program.
% ABSENT_VOICES: Small-value users priced toward custodial layers have no seat in governance and no organized voice; the arrangement's residual costs fall on them without representation. Future users, bound by a monetary policy adopted before they arrived, are absent by definition. Base-layer scaling researchers were pushed out of the agenda after 2017 and now speak from outside it.
% DISAPPEARANCE_RATIONALE: If the settlement dissolved overnight — if either the base freeze or the layer permission lapsed — the camps would resume fighting over the base layer's change surface: monetary-policy proposals would return, activation battles would recur, and the network would face the split risk that produced the 2017 fork. The layered ecosystem would lose the stable foundation its trust assumptions price in, and institutional holders would reprice policy risk.
% FOUNDING_PROBLEM: The 2015–2017 blocksize crisis: a leaderless protocol with no central authority faced an existential dispute over whether to raise the block limit — and beneath it the unresolved question of what the whitepaper mandates, immutable covenant or minimum viable mechanism — threatening a network split that would destroy both camps' asset.
% FOUNDING_PROBLEM_CORROBORATION: The kernel contest is not closed: the sibling readings remain live positions, and every proposed base-layer change reactivates the underlying question. Corroboration from outside the benefiting parties: the academic governance literature on blockchain decision-making, contemporaneous and retrospective accounts of the blocksize war written by researchers with no stake in either camp's victory, and the observable historical record that both camps' own literature attests the crisis and the settlement's terms — while each continues to dispute whether the founding problem is solved.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__pragmatic_synthesis, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__pragmatic_synthesis, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__pragmatic_synthesis, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_consensus_kernel__pragmatic_synthesis, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_consensus_kernel__pragmatic_synthesis, 0.16, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_consensus_kernel__pragmatic_synthesis_tests).
:- end_tests(bitcoin_consensus_kernel__pragmatic_synthesis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.16 at interval end) because the settlement channels rather than takes: its residual costs are congestion pricing that pushes small-value users to layers, foreclosed base-layer research programs, and the ideological cost both camps pay — no seat captures a rent stream beyond the fee flow examined under the security-budget omega. Suppression (0.22) is the normalized remnant of the 2017 enforcement arc: forum censorship, denial-of-service attacks against competing implementations, and the user-activated soft fork's credible chain-split threat built the settlement; the node-consensus veto remains the backstop, but active coercion decayed as the norm internalized — the suppression_requirement series traces that build-up-and-decay. Suppression is authored as a raw structural property, unscaled; only extractiveness is scaled by directionality and scope in the engine's computation. Theater (0.18) is low but rising slowly: the settlement works, and a growing share of immutability discourse is celebratory liturgy — genesis-day rituals, covenant invocations — rather than operative defense. Accessibility collapse is low (0.35): the settlement's design principle is that alternatives persist — layers, sidechains, and forks remain open, and the 2017 fork demonstrated exit is real — so understanding the arrangement does not close the option space. Resistance (0.25) is residual camp grumbling: each camp received a partial win, damping resistance below crisis levels without eliminating it. The three measurement series share one grid (2015/2017/2019/2021/2023/2025) with every metric authored at every point.
 *
 * PERSPECTIVAL GAP:
 *   Four seats experience the same rules differently. From the maintainer seat the settlement is a hard-won bounded agenda: monetary disputes are off the table and the remaining work is engineering. From the maximalist seat the same settlement is covenant dilution — the base was saved but the network's soul migrated to speculative layers. From the utility seat it is premature calcification — the base should iterate, and the freeze is self-inflicted. From the small-value user seat, immutable base simply means expensive base, experienced as pricing rather than principle. The payer seats (users, scaling researchers) and the beneficiary seats (holders, layer developers, node operators) should compute different classifications from identical structural facts; the engine derives that divergence from the declared positions, and this story's claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for long_horizon_holders, layer_two_developers, full_node_operators, core_protocol_maintainers, and maximalist_covenant_purists (partial — their core demand was met); mining_pool_operators are net beneficiaries through fee scarcity despite foreclosed throughput economics and their 2017 capitulation. Victim declarations drive high directionality for small_value_onchain_users (constrained exit — every alternative carries real cost) and onchain_scaling_developers (mobile exit — many exercised it after 2017, which moderates their trapped-ness relative to the users). Identity lock amplifies the beneficiaries who cannot leave (holders, maintainers, purists): the settlement subsidizes precisely the agents fused with it. Gain receipt is authored separately from benefit: the fee stream sustained by deliberate scarcity lands on mining_pool_operators, though this reading classifies that flow as the security budget rather than rent — the receipt surface records where the money lands, not whether the landing is justified.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a leaderless network's governance war over base-layer change — is live, not dead: the kernel contest is suspended by the settlement, not resolved by it, and every proposed base-layer change reactivates the underlying question. Live status paired with a world_rearranges disappearance verdict is the consistent configuration; no zombie flag. The mandatrophy risk runs the other direction: if the layer ecosystem matures and the contest goes dormant, the settlement's crisis-era coordination machinery (soft-fork mobilization, forum-level enforcement) atrophies while immutability discourse continues as ritual — the theater_ratio series (0.08 to 0.18 over the interval) tracks exactly that slow ceremonial accumulation. The scaffold claim keeps this visible: a transitional arrangement whose sunset is conditional must either retire when its work is done or admit it has become a standing constitution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates only the pragmatic_synthesis reading of the bitcoin_consensus_kernel; how would the classification change under the sibling readings of the same base-layer arrangement?',
    'Read the sibling stories (bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__utility_reading): each authors its own epsilon, beneficiary/victim structure, and type over the same arrangement.',
    'Under the maximalist reading the same arrangement is covenant dilution with different victims (the covenant''s universality) and different enforcement demands; under the utility reading the base freeze itself is the extraction and epsilon rises. The low-extraction profile authored here is reading-indexed, not topic-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling readings instantiate different constraints over the same arrangement.').

omega_variable(
    sunset_clause_realism,
    'Is the settlement''s transitional character real — does it carry a genuine sunset — or is the base freeze permanent in practice, converting the arrangement from transitional settlement into a standing constitution?',
    'Observe whether base-layer monetary change proposals resume when conditions shift (fee-market stress, state pressure, layer failures), and whether the community treats the freeze as revisable-by-consensus or as inviolable.',
    'If the sunset is real, the scaffold classification holds and the arrangement should retire as the layer ecosystem matures; if not, the arrangement drifts toward a permanent settlement or a ritually maintained truce, and the has_sunset_clause declaration overstates its transitional character.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_clause_realism, empirical, 'Whether the pragmatic settlement''s transitional justification is operative or aspirational.').

omega_variable(
    fee_security_budget_vs_congestion_rent,
    'Is the fee stream sustained by deliberate blockspace scarcity the designed security budget replacing declining subsidies, or congestion rent taken from users who cannot use layers?',
    'Economic analysis comparing fee levels to the marginal cost of base-layer settlement and to counterfactual fee levels under relaxed blockspace, segmented by user type (layer-capable versus base-bound).',
    'If a substantial share is congestion rent, the settlement''s effective extractiveness is higher than authored and the miners'' receipt seat is a capture seat; if it is security budget, the low epsilon stands and the receipt is functional payment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fee_security_budget_vs_congestion_rent, empirical, 'Whether scarcity-sustained fees are coordination cost or rent.').

omega_variable(
    layer_trust_migration_export,
    'Does the settlement eliminate the trust costs of base-layer change or export them to layers — do custodial channel setups, watchtowers, and federated sidechains reintroduce at layer level the counterparty risk the immutable base was supposed to make unnecessary?',
    'Measure custody concentration and trust assumptions across the layer ecosystem; compare realized layer-level loss events against base-layer self-custody baselines.',
    'If trust costs are exported rather than eliminated, the settlement''s low extractiveness is understated — it relocates costs to the seats least able to measure them rather than removing them; if layers genuinely reduce net trust costs, the settlement''s profile stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(layer_trust_migration_export, empirical, 'Whether the layered architecture relocates rather than removes trust costs.').

omega_variable(
    ideological_coherence_victim_status,
    'The settlement''s principal casualty is the coherence of either pure ideology — maximalist covenant or utility iteration — which is not an actor; is there any real actor group bearing material cost sufficient to ground the victim declarations, or are the declared victims'' costs incidental?',
    'Trace named actor welfare under the settlement versus the counterfactual victories of either camp: base-bound users'' transaction costs, scaling researchers'' program foreclosure, miners'' foreclosed throughput economics.',
    'If no actor bears material cost, the arrangement approaches pure coordination and the victim declarations overstate asymmetry; if actor costs are material, the payer seats carry more classification weight than the low headline metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ideological_coherence_victim_status, conceptual, 'Whether the victim of the settlement is an abstraction (ideological coherence) or grounded in actor costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__pragmatic_synthesis, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t2015, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 2015, 0.08).
narrative_ontology:measurement(bitc_tr_t2017, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 2017, 0.12).
narrative_ontology:measurement(bitc_tr_t2019, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 2019, 0.14).
narrative_ontology:measurement(bitc_tr_t2021, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 2021, 0.15).
narrative_ontology:measurement(bitc_tr_t2023, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 2023, 0.17).
narrative_ontology:measurement(bitc_tr_t2025, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 2025, 0.18).

% Extraction over time
narrative_ontology:measurement(bitc_be_t2015, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 2015, 0.12).
narrative_ontology:measurement(bitc_be_t2017, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 2017, 0.32).
narrative_ontology:measurement(bitc_be_t2019, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 2019, 0.2).
narrative_ontology:measurement(bitc_be_t2021, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 2021, 0.17).
narrative_ontology:measurement(bitc_be_t2023, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 2023, 0.15).
narrative_ontology:measurement(bitc_be_t2025, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 2025, 0.16).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t2015, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 2015, 0.35).
narrative_ontology:measurement(bitc_su_t2017, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 2017, 0.6).
narrative_ontology:measurement(bitc_su_t2019, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 2019, 0.4).
narrative_ontology:measurement(bitc_su_t2021, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 2021, 0.3).
narrative_ontology:measurement(bitc_su_t2023, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 2023, 0.25).
narrative_ontology:measurement(bitc_su_t2025, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 2025, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__pragmatic_synthesis, enforcement_mechanism).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__pragmatic_synthesis, bitcoin_consensus_kernel__maximalist_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__pragmatic_synthesis, bitcoin_consensus_kernel__utility_reading).

% DUAL FORMULATION NOTE:
% The kernel 'bitcoin consensus' decomposes into three readings with distinct epsilon values and beneficiary structures: the maximalist reading (covenant — immutability as end in itself, high suppression of any protocol change), the utility reading (minimum viable mechanism — low immutability, iterative base improvement), and this pragmatic synthesis (immutable base as the settlement precondition for layered innovation). This story is the settlement reading only; its low epsilon is assessed by its own lights over the standing layered arrangement. The settlement is downstream of the contest between the siblings — it exists because they conflict — and feeds back on both: it delivers the maximalist content while demoting the covenant framing from governing default to dissident position, and it blocks the utility program at base while legitimizing it at layers. Sibling files document their own epsilon and victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
