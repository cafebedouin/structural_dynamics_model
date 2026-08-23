% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__pragmatic_synthesis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: bitcoin_consensus_kernel__pragmatic_synthesis
 *   human_readable: Bitcoin Layered Settlement: Frozen Base, Open Layers (Pragmatic Synthesis Reading)
 *   domain: cryptoeconomics/monetary systems/distributed consensus
 *
 * SUMMARY:
 *   The bitcoin_consensus_kernel — the community's founding commitment that
 *   the whitepaper's monetary covenant is authoritative — is read three ways.
 *   This story instantiates the pragmatic_synthesis reading: the covenant
 *   binds only the base-layer monetary rules (the 21 million supply cap, the
 *   issuance schedule, the consensus validation rules), while upper layers —
 *   payment channels, sidechains, anchored chains — may innovate freely
 *   without kernel violation. The operative arrangement this reading produced
 *   is the post-2017 layered settlement: a base layer frozen against change
 *   and an innovation surface relocated above it. It was forged in the block
 *   size wars (2015-2017), consolidated after the 2017 chain split, and now
 *   functions as the ecosystem's operating constitution. Per the
 *   epsilon-invariance rule this is ONE constraint with ONE stable epsilon:
 *   the sibling readings (maximalist_reading, utility_reading) are separate
 *   stories in the same family, linked through network.affects_constraints,
 *   not alternatives folded into this one. Claim/metric independence: the
 *   claim (scaffold) states what this reading's arrangement is structurally —
 *   a transitional bridge held in place while the kernel contest resolves;
 *   the metrics state how it actually operates (low extraction, narrow
 *   suppression, low theater) — neither is tuned to the other or to a
 *   predicted engine output.
 *
 * KEY AGENTS:
 *   - - core_protocol_maintainers: Agenda-setting seat (institutional/mobile) — gatekeeps base-layer change through reference-implementation review; the immutability boundary is administered here
 *   - - full_node_operators: Enforcement seat (organized/mobile) — the distributed veto that makes base-layer monetary change fail; beneficiaries of unchanging rules
 *   - - layer_two_builders: Beneficiary seat (organized/mobile) — builds on a base that does not move under them
 *   - - long_term_holders: Beneficiary seat (organized/mobile) — the constituency for whom issuance immutability is the entire value proposition
 *   - - mining_operators: Beneficiary seat (powerful/constrained) — collects subsidy and fees under a fixed schedule; capital locked to the rule set
 *   - - protocol_maximalists: Payer seat (moderate/identity_locked) — materially won the base freeze, ideologically lost the whole-stack claim; the settlement's coherence cost lands here
 *   - - base_layer_iteration_advocates: Payer seat (powerless/constrained) — agenda foreclosed at the base since 2017
 *   - - base_layer_users: Payer/beneficiary seat (moderate/mobile) — pay the fee market immutability necessitates; receive finality and issuer-free monetary policy
 *   - - altcoin_protocols: Excluded seat (organized/arbitrage) — harvest the design space the settlement forecloses
 *   - - protocol_governance_scholars: Analytical observer (analytical/analytical) — studies the adjudication without holding a stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__pragmatic_synthesis, 0.22).
domain_priors:suppression_score(bitcoin_consensus_kernel__pragmatic_synthesis, 0.3).
domain_priors:theater_ratio(bitcoin_consensus_kernel__pragmatic_synthesis, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, extractiveness, 0.22).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__pragmatic_synthesis, scaffold).
narrative_ontology:human_readable(bitcoin_consensus_kernel__pragmatic_synthesis, "Bitcoin Layered Settlement: Frozen Base, Open Layers (Pragmatic Synthesis Reading)").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__pragmatic_synthesis, "cryptoeconomics/monetary systems/distributed consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__pragmatic_synthesis).
narrative_ontology:has_sunset_clause(bitcoin_consensus_kernel__pragmatic_synthesis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__pragmatic_synthesis, 'a1fbd58b-9553-4c0d-a541-4c515bfa8202').
narrative_ontology:cs_kernel_codification('a1fbd58b-9553-4c0d-a541-4c515bfa8202', fixed_text).
narrative_ontology:cs_authority_grounding('a1fbd58b-9553-4c0d-a541-4c515bfa8202', practice).
narrative_ontology:cs_interpretation_layer_present('a1fbd58b-9553-4c0d-a541-4c515bfa8202').
narrative_ontology:cs_reading_relation('a1fbd58b-9553-4c0d-a541-4c515bfa8202', bitcoin_consensus_kernel__maximalist_reading, forecloses).
narrative_ontology:cs_reading_relation('a1fbd58b-9553-4c0d-a541-4c515bfa8202', bitcoin_consensus_kernel__utility_reading, forecloses).
narrative_ontology:cs_axiom('a1fbd58b-9553-4c0d-a541-4c515bfa8202', foundational, base_layer_immutability_scope).
narrative_ontology:cs_axiom_status(base_layer_immutability_scope, holdable).
narrative_ontology:cs_axiom_grounding('a1fbd58b-9553-4c0d-a541-4c515bfa8202', base_layer_immutability_scope, conventional).
narrative_ontology:cs_axiom('a1fbd58b-9553-4c0d-a541-4c515bfa8202', foundational, layered_segregation_satisfies_both_camps).
narrative_ontology:cs_axiom_status(layered_segregation_satisfies_both_camps, holdable).
narrative_ontology:cs_axiom_grounding('a1fbd58b-9553-4c0d-a541-4c515bfa8202', layered_segregation_satisfies_both_camps, instrumental).
narrative_ontology:cs_axiom('a1fbd58b-9553-4c0d-a541-4c515bfa8202', secondary, soft_forks_are_not_kernel_violations).
narrative_ontology:cs_axiom_status(soft_forks_are_not_kernel_violations, holdable).
narrative_ontology:cs_axiom_grounding('a1fbd58b-9553-4c0d-a541-4c515bfa8202', soft_forks_are_not_kernel_violations, conventional).
narrative_ontology:cs_reference_frame('a1fbd58b-9553-4c0d-a541-4c515bfa8202', frozen_base_open_layers).
narrative_ontology:cs_drift_state('a1fbd58b-9553-4c0d-a541-4c515bfa8202', post_2017_settlement_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('a1fbd58b-9553-4c0d-a541-4c515bfa8202', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__pragmatic_synthesis, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__pragmatic_synthesis, full_node_operators).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__pragmatic_synthesis, layer_two_builders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__pragmatic_synthesis, long_term_holders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__pragmatic_synthesis, mining_operators).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__pragmatic_synthesis, altcoin_protocols).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__pragmatic_synthesis, protocol_maximalists).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__pragmatic_synthesis, base_layer_iteration_advocates).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__pragmatic_synthesis, base_layer_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__pragmatic_synthesis, base_layer_users).
narrative_ontology:constraint_vindicates(bitcoin_consensus_kernel__pragmatic_synthesis, layered_scaling_thesis).
narrative_ontology:constraint_vindicates(bitcoin_consensus_kernel__pragmatic_synthesis, sovereignless_consensus_enforcement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the reference implementation and review every proposed change to the consensus rules. Their review process is the de facto gate through which base-layer changes pass or die; they treat monetary parameters as untouchable and everything else as negotiable under extreme caution. They can leave the project — several have founded or joined other protocols — but their standing comes from this one.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, core_protocol_maintainers, agenda_setter,
    institutional, generational, mobile, global).

% Run software that independently validates every block against the consensus rules; collectively they are the veto that makes base-layer monetary change fail. They receive a monetary policy they can verify personally and never have to renegotiate. They can switch implementations or chains at will, and periodically do.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, full_node_operators, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__pragmatic_synthesis, full_node_operators, beneficiary).

% Build payment channels, sidechains, and applications on top of the frozen base. The base they build on does not change under them, which is the property their engineering depends on; above it they may deploy anything. If the base's guarantees eroded, their work would need to be rebuilt against a moving target.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, layer_two_builders, beneficiary,
    organized, biographical, mobile, global).

% Hold the asset across years on the strength of a supply schedule no coalition has been able to alter. The immutability of issuance is the core of their position; they can sell or migrate to other assets at any time, and their holding is a continuous bet that the schedule stays.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, long_term_holders, beneficiary,
    organized, generational, mobile, global).

% Convert capital into hash power and collect the block subsidy and transaction fees under a fixed issuance schedule. Their hardware is specialized to this chain's algorithm, so their capital is locked to the rule set they operate under; rule stability is what makes the capital deployment rational.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, mining_operators, beneficiary,
    powerful, biographical, constrained, global).

% Hold that the founding document governs the entire protocol and that the community's legitimacy flows from keeping it whole. The operative arrangement grants them the immutable base they demanded while legitimizing the layer activity they read as dilution. Their identity is fused with Bitcoin-as-a-whole-thing; leaving for another chain would dissolve the self-conception, so they remain inside the community they define themselves by, arguing against developments they cannot stop and would not leave over.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, protocol_maximalists, payer,
    moderate, generational, identity_locked, global).

% Argue that the base protocol itself should evolve — larger blocks, new script capabilities, revised parameters — and have lost every attempt to move it since 2017. Their preferred exit, a competing chain running their preferred rules, exists and was tried; staying means accepting that their agenda advances only if they persuade an economic majority that has repeatedly declined.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, base_layer_iteration_advocates, payer,
    powerless, biographical, constrained, global).

% Transact on the base for settlement and savings, paying fees that rise as the fixed block space fills. They receive finality and a monetary policy they need not trust any issuer to maintain; they can move to other chains, custodians, or higher layers whenever the fee or the wait exceeds their tolerance.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, base_layer_users, payer,
    moderate, immediate, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__pragmatic_synthesis, base_layer_users, beneficiary).

% Run competing chains that adopt exactly the design choices the settlement declines at the base — larger blocks, on-chain programmability, faster parameter change. They have no seat in the kernel conversation and benefit from each option the settlement forecloses.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, altcoin_protocols, excluded,
    organized, biographical, arbitrage, global).

% Study how a community with an authoritative but ambiguous founding text adjudicates change without a sovereign. They publish on the block size wars, soft-fork activation politics, and layer governance; they hold no stake in which reading wins.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, protocol_governance_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_consensus_kernel__pragmatic_synthesis, diffuse).
narrative_ontology:fixing_cost_class(bitcoin_consensus_kernel__pragmatic_synthesis, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single monetary consensus while permitting technical evolution: by freezing the kernel (supply cap, issuance schedule, consensus validation rules) and relocating experimentation to layers, the community avoids re-litigating foundational rules with every proposal and avoids schism over each change; every participant verifies the same unchanging monetary policy while innovation proceeds above it.
% TRANSFER_FUNCTION: Moves adjudication of monetary-rule changes out of any ordinary proposal process — base-layer changes require overwhelming economic consensus, which functions as a standing veto for the status quo — and channels innovation effort upward into layers; moves the ideological cost of compromise onto both purist camps' coherence; and, as the block subsidy declines, moves transaction fees from base-layer users to miners and routing and custody fees from layer users to layer operators.
% ABSENT_VOICES: Altcoin protocols and non-Bitcoin chain communities are structurally outside the conversation — the kernel is Bitcoin's whitepaper and they hold no seat; they would argue the settlement entrenches one chain's network effect and forecloses base-layer design exploration for everyone (their stakeholder entry records the arbitrage they harvest instead). Financial regulators are likewise absent: they hold monetary-sovereignty, AML, and consumer-protection stakes but no standing in consensus governance, and enforcement by distributed consensus leaves them no counterpart to regulate.
% DISAPPEARANCE_RATIONALE: If the settlement vanished overnight — if base rules became contestable and layer legitimacy withdrew — the ecosystem would immediately re-enter schism: competing base-layer forks would fragment the monetary base, every layer-two system built on frozen guarantees would need to re-anchor against a moving target, and the supply cap's status as enforceable fact would collapse into contest. Holders' monetary certainty, builders' engineering assumptions, and miners' capital deployment are all organized around the freeze; the world rearranges.
% FOUNDING_PROBLEM: The block size wars (2015-2017): an unresolved conflict over the whitepaper's meaning — whether the founding text mandates minimal blocks and whole-stack purity or permits evolution — through which every scaling proposal re-litigated the kernel and carried chain-split risk.
% FOUNDING_PROBLEM_CORROBORATION: The maximalist camp — a payer seat outside the beneficiary set — attests the founding conflict was real; their war narrative is the settlement's own recorded history. Contemporaneous developer mailing-list archives, the UASF episode, and the BCH chain split physically corroborate that the underlying conflict was live enough to split the chain. Academic governance literature on Bitcoin's consensus politics corroborates the adjudication problem's persistence from outside all camps. No party denies the war occurred; the parties dispute only whether the question it fought over is settled.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__pragmatic_synthesis, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__pragmatic_synthesis, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__pragmatic_synthesis, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_consensus_kernel__pragmatic_synthesis, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_consensus_kernel__pragmatic_synthesis, 0.22, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is low (0.22) because the settlement's design deliberately minimizes it: each camp receives its core demand (maximalists the frozen base, utility advocates the free layer surface) and the residual cost — ideological coherence borne by both purist camps, fee-market costs borne by users, foreclosed base-layer options — is real but small and uncaptured. Suppression (0.30) is narrow and structural: the consensus veto makes base-layer monetary change fail without forbidding exit (forks are possible, BCH exists, altchains abound); nothing is suppressed except change at the base itself. Theater (0.20) reflects the gap between immutability rhetoric and soft-fork practice — the rules have changed at the margins (SegWit, Taproot) while the rhetoric of immutability is maintained — but the core monetary policy has genuinely never moved, so the theater is marginal, not structural. Accessibility collapse (0.45): alternatives do not fully collapse — competing chains and rival readings remain live — but within the ecosystem, base-layer-change alternatives are effectively foreclosed once the settlement is understood. Resistance (0.45): the settlement was born from open war (the block size wars peaked with the 2017 UASF and the BCH split) and still meets periodic resistance — maximalist purity campaigns (the ordinals controversy), iteration-advocate proposals (covenants, drivechains) — but the post-2017 record is consolidation, not war. The measurement arc is rise-peak-decay, not monotonic: enforcement machinery was built under wartime pressure (suppression_requirement peaking at 0.45 in 2017), then relaxed as the settlement succeeded and compliance internalized; extractiveness peaked with the war's coherence costs and declined as the compromise began delivering for both camps. All three series share one time grid (2009-2026, eight points) per the alignment rule.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by design of the arrangement itself. From the beneficiary seats (holders, builders, miners, node operators) the settlement is close to pure coordination: the base does not move, innovation is free above it, and the fee market is the price of unconfiscatable monetary policy. From the maximalist seat the same settlement is a standing partial violation: it grants the base they demanded while legitimizing the layer economy their reading forbids — they hold both a vindicated material position and a defeated ideological one, which is why their exit is identity-locked (their identity is fused with Bitcoin-as-whole; leaving would dissolve the self-conception, so they stay and dissent; if that identity frame broke, their directionality would collapse toward exit and their extraction would convert to fork migration). From the iteration-advocate seat the settlement is foreclosure: their agenda has lost every attempt since 2017, their preferred exit was tried (the BCH fork) and demonstrated its cost, and remaining inside means accepting a base they cannot move. From the maintainer seat the settlement is role-constitutive — the immutability boundary is what their review authority administers. On same-level dynamics: maintainers (institutional), miners (powerful), holders and node operators (organized) hold comparable ecosystem standing but different exits — ASIC capital locks miners in, network-effect value locks holders in softly, maintainers' reputation is portable, and node software is trivially switchable — so identical global standing produces different constraint experiences. The powerless iteration advocates attempted coalition power in 2015-2017 (the big-block business coalition) and lost it; that record is itself the answer to the coalition question. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: node operators, layer-two builders, long-term holders, and miners (all inside the settlement's subsidy and stability structure), and — structurally — altcoin_protocols, which harvest exactly the design space the settlement forecloses at the base and are therefore beneficiaries from outside the conversation. Victim declarations: protocol_maximalists (ideological coherence), base_layer_iteration_advocates (foreclosed agenda), base_layer_users (fee-market costs). Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and spatial scope. One directionality override is authored: the moderate-power seats (protocol_maximalists, base_layer_users) would derive near-full-target directionality from their victim declarations alone — maximalists compounded by identity-locked exit, users by rising fees — but both hold substantial offsetting beneficiary positions the raw derivation cannot see: the frozen base IS the maximalists' core material demand (they won the parameter fight they lost the interpretive war over), and users receive finality and issuer-free policy that offsets their fee costs. The override sets both moderate seats slightly target-side of symmetric (d=0.55), reflecting net structural positions rather than the raw victim listing.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification is what prevents mislabeling in both directions. Reading the settlement as pure rope would miss that it is a peace treaty, not a constitution: its justification is the transition (hold the base still while the layer experiment resolves the contest), not a steady-state claim, and it carries a conditional sunset — the arrangement's own success condition terminates it, either by hardening into the settled architecture (if layers mature) or dissolving back into contest (if they fail). Reading it as snare would miss that it coerces no exit, extracts little, and leaves both rival readings live and holdable. The founding problem (how a community with an authoritative-but-ambiguous founding text adjudicates change without schism) is live, not dead — every base-layer proposal (covenants, drivechains, opcode changes) re-runs the adjudication — so no zombie-mandate flag is warranted; the arrangement's function is exercised continuously. The omega conditional_sunset_status tracks the real mandatrophy risk: that the settlement quietly stops being transitional (its bridging rhetoric decaying as the architecture becomes permanent), at which point the honest classification shifts from scaffold toward a settled rope and the sunset clause becomes theater. The measurement series is the drift record for that determination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_structure,
    'This story instantiates the pragmatic_synthesis reading of kernel bitcoin_consensus_kernel: the founding covenant''s immutability requirement binds only the base-layer monetary rules, and upper layers may innovate without kernel violation. What would the sibling readings change structurally — and where exactly is the disagreement located?',
    'The readings coexist as live community positions; the contest resolves only by evidence or attrition — layer maturity with preserved self-custody favoring the synthesis, a purity schism favoring the maximalist_reading, cumulative base-layer stagnation costs favoring the utility_reading.',
    'Under the maximalist_reading the entire layer economy becomes covenant violation: extractiveness rises sharply and the victim set inverts (protocol_maximalists become the vindicated seat). Under the utility_reading the freeze dissolves, this scaffold claim lapses, and extraction relocates to whatever coalition governs each base revision.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_structure, conceptual, 'Committer structure: one reading of the bitcoin consensus kernel; sibling readings would restructure beneficiaries, victims, and type.').

omega_variable(
    conditional_sunset_status,
    'Is the settlement''s sunset clause operative — does the arrangement terminate when the kernel contest resolves — or has the base-frozen/layers-free architecture already become a permanent steady state that merely describes itself as transitional?',
    'Track the bridging rhetoric against layer maturity: if the community stops describing the segregation as a compromise and treats it as simply the architecture, the sunset has lapsed and the honest classification shifts from scaffold toward a settled rope; if purity or iterability campaigns re-open the contest, the sunset remains live.',
    'A lapsed sunset makes the scaffold claim stale and the transition framing partially theatrical; a live sunset predicts dissolution or hardening on layer-maturity evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditional_sunset_status, empirical, 'Whether the scaffold''s conditional sunset is operative or the settlement has quietly become permanent.').

omega_variable(
    kernel_boundary_soft_fork_ambiguity,
    'Where is the actual immutability boundary? The reading declares base-layer monetary rules immutable, yet soft forks (SegWit, Taproot) have changed base-layer consensus rules while the immutability claim is maintained — is the boundary monetary policy only (supply, issuance) or all consensus rules?',
    'Adjudicate against revealed practice: which proposed changes draw absolute veto treatment (supply changes) versus ordinary negotiated evolution (opcode additions, activation politics).',
    'If the boundary is monetary-policy-only, the immutability claim is narrower than its rhetoric and the theater measure is overstated; if all consensus rules, then absorbed soft-fork changes were violations and the immutability claim is substantially theatrical, raising both theater and effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_boundary_soft_fork_ambiguity, conceptual, 'The immutability boundary is ambiguous between monetary policy and the full consensus rule set.').

omega_variable(
    ideological_extraction_materiality,
    'The settlement''s largest extraction is ideological coherence borne by both purist camps — a non-material cost. Should it weight comparably to material extraction in epsilon, or is it absorbed without behavioral consequence?',
    'Observe revealed cost: measurable exit or noncompliance by maximalists (purity-only node software campaigns, transaction-filtering movements) or by iteration advocates (fork migration) would show the coherence cost is behaviorally binding; continued participation without exit suggests absorption.',
    'If behaviorally binding, epsilon is understated and the settlement is more extractive than its material footprint; if absorbed, the victim declarations are partially nominal and the authored epsilon of 0.22 is approximately correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ideological_extraction_materiality, empirical, 'Whether ideological-coherence extraction is materially binding or behaviorally absorbed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__pragmatic_synthesis, 0, 17).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 0, 0.05).
narrative_ontology:measurement(bitc_tr_t2, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 2, 0.08).
narrative_ontology:measurement(bitc_tr_t4, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 4, 0.12).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 6, 0.22).
narrative_ontology:measurement(bitc_tr_t8, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 8, 0.3).
narrative_ontology:measurement(bitc_tr_t11, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 11, 0.24).
narrative_ontology:measurement(bitc_tr_t14, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 14, 0.21).
narrative_ontology:measurement(bitc_tr_t17, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 17, 0.2).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(bitc_be_t2, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 2, 0.12).
narrative_ontology:measurement(bitc_be_t4, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 4, 0.18).
narrative_ontology:measurement(bitc_be_t6, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 6, 0.3).
narrative_ontology:measurement(bitc_be_t8, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(bitc_be_t11, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 11, 0.28).
narrative_ontology:measurement(bitc_be_t14, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 14, 0.24).
narrative_ontology:measurement(bitc_be_t17, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 17, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(bitc_su_t2, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 2, 0.08).
narrative_ontology:measurement(bitc_su_t4, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 4, 0.15).
narrative_ontology:measurement(bitc_su_t6, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 6, 0.35).
narrative_ontology:measurement(bitc_su_t8, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(bitc_su_t11, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 11, 0.38).
narrative_ontology:measurement(bitc_su_t14, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 14, 0.33).
narrative_ontology:measurement(bitc_su_t17, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 17, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__pragmatic_synthesis, identity_coordination).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__pragmatic_synthesis, bitcoin_consensus_kernel__maximalist_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__pragmatic_synthesis, bitcoin_consensus_kernel__utility_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Bitcoin's founding covenant' covers three structurally distinct constraints instantiated by three readings of one kernel text: the maximalist_reading (whole-stack purity; high epsilon — every layer innovation is a violation surface), this pragmatic_synthesis (segregation; low-epsilon scaffold whose victim is ideological coherence), and the utility_reading (base iterability; extraction relocates to whatever coalition governs each revision). This story links both siblings per the epsilon-invariance decomposition rule; each sibling story must link back. The upstream/downstream structure runs both ways: the maximalist reading supplies the immutability premise the synthesis adopts at the base, the utility reading supplies the innovation premise the synthesis adopts at the layers, and the synthesis's settlement in turn defines both siblings' operating environments (the maximalist violation surface and the blocked base-iteration agenda are artifacts of this arrangement's existence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_consensus_kernel__pragmatic_synthesis, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
