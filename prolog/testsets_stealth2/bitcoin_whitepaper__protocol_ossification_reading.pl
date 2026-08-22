% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__protocol_ossification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: bitcoin_whitepaper__protocol_ossification_reading
 *   human_readable: Protocol Ossification Norm: Near-Universal Consensus Requirement on Bitcoin Base-Layer Change
 *   domain: economic/technological/governance
 *
 * SUMMARY:
 *   Since the 2015-2017 block-size conflict, the Bitcoin ecosystem has
 *   operated under a governance norm: no change to the base protocol is
 *   legitimate unless it approaches universal consensus, and stability of the
 *   rules outranks growth of capability. The norm operates through the
 *   improvement-proposal process, maintainer gatekeeping, and the standing
 *   veto of node operators who simply decline to upgrade. Its defenders
 *   present it as the discovered physics of leaderless money; its critics —
 *   privacy researchers, throughput advocates, stalled proposal authors —
 *   experience it as a permanently closed door with no formal rejection to
 *   appeal. Innovation has migrated to overlay layers, which is precisely the
 *   outcome this reading celebrates and the cash-oriented sibling reading
 *   mourns. Claim and metrics are authored independently: the claimed type is
 *   tangled_rope because the norm genuinely solves a commitment problem no
 *   other mechanism solves, while the metrics describe a moderately and
 *   increasingly costly arrangement whose burden falls on identifiable
 *   classes. Per the epsilon-invariance principle, the whitepaper label
 *   decomposes into three structurally distinct constraints (this reading
 *   plus the two siblings linked in network.affects_constraints); this file
 *   authors only the ossification reading.
 *
 * KEY AGENTS:
 *   - - long_term_holders: primary beneficiary (powerful/constrained) — the certainty premium capitalizes into their positions; exit means abandoning the thesis the norm protects
 *   - - core_maintainer_gatekeepers: agenda setter and bound party (institutional/identity_locked) — administers the consensus threshold it cannot cross alone; dual-positioned
 *   - - full_node_operators: collective veto holder (organized/mobile) — enforcement backstop whose leverage exists only while they keep validating
 *   - - incumbent_miners: beneficiary with stranded-capital exposure (institutional/constrained) — ASIC fleets are worthless under any change to proof-of-work or issuance
 *   - - l1_privacy_use_cases, fee_burdened_transactors, bip_authors_protocol_researchers: bearing classes (constrained exit) — pay in permanently exposed transaction graphs, priced-out payments, and stalled life's work
 *   - - layer2_protocol_builders: secondary beneficiaries (moderate/mobile) — inherit the innovation mandate the base layer refuses
 *   - - academic_governance_observers: analytical seat — documents the governance experiment without collecting or paying
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__protocol_ossification_reading, 0.58).
domain_priors:suppression_score(bitcoin_whitepaper__protocol_ossification_reading, 0.6).
domain_priors:theater_ratio(bitcoin_whitepaper__protocol_ossification_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__protocol_ossification_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__protocol_ossification_reading, "Protocol Ossification Norm: Near-Universal Consensus Requirement on Bitcoin Base-Layer Change").
narrative_ontology:topic_domain(bitcoin_whitepaper__protocol_ossification_reading, "economic/technological/governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__protocol_ossification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__protocol_ossification_reading, 'e44b1a1b-2b62-452a-89b1-844a584a849f').
narrative_ontology:cs_kernel_codification('e44b1a1b-2b62-452a-89b1-844a584a849f', fixed_text).
narrative_ontology:cs_authority_grounding('e44b1a1b-2b62-452a-89b1-844a584a849f', lineage).
narrative_ontology:cs_interpretation_layer_present('e44b1a1b-2b62-452a-89b1-844a584a849f').
narrative_ontology:cs_reading_relation('e44b1a1b-2b62-452a-89b1-844a584a849f', bitcoin_whitepaper__p2p_cash_reading, influences).
narrative_ontology:cs_reading_relation('e44b1a1b-2b62-452a-89b1-844a584a849f', bitcoin_whitepaper__digital_gold_reading, coexists_with).
narrative_ontology:cs_axiom('e44b1a1b-2b62-452a-89b1-844a584a849f', foundational, near_universal_consensus_confers_legitimacy).
narrative_ontology:cs_axiom_status(near_universal_consensus_confers_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('e44b1a1b-2b62-452a-89b1-844a584a849f', near_universal_consensus_confers_legitimacy, conventional).
narrative_ontology:cs_axiom('e44b1a1b-2b62-452a-89b1-844a584a849f', foundational, monetary_stability_outranks_capability_growth).
narrative_ontology:cs_axiom_status(monetary_stability_outranks_capability_growth, holdable).
narrative_ontology:cs_axiom_grounding('e44b1a1b-2b62-452a-89b1-844a584a849f', monetary_stability_outranks_capability_growth, instrumental).
narrative_ontology:cs_reference_frame('e44b1a1b-2b62-452a-89b1-844a584a849f', whitepaper_fidelity_stability_primacy).
narrative_ontology:cs_drift_state('e44b1a1b-2b62-452a-89b1-844a584a849f', contemporary_post_taproot_activation, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('e44b1a1b-2b62-452a-89b1-844a584a849f', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, long_term_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, incumbent_miners).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, full_node_operators).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, layer2_protocol_builders).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, l1_privacy_use_cases).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, fee_burdened_transactors).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, bip_authors_protocol_researchers).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, core_maintainer_gatekeepers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the asset as long-duration savings and treat unchanged rules as the core of their thesis. The certainty that today's issuance schedule and script semantics will govern tomorrow is capitalized into the price they paid. Within the system they hold no formal vote beyond what their holdings signal; their only exit is selling out entirely, which abandons the very position the norm protects.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, long_term_holders, beneficiary,
    powerful, generational, constrained, global).

% Operate specialized hashing hardware whose resale value depends entirely on the current proof-of-work and emission schedule; any change touching either strands the fleet. They earn from fee markets shaped by scarce blockspace and invest against the predictability the norm guarantees. Exit means liquidating hardware at distressed prices.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, incumbent_miners, beneficiary,
    institutional, biographical, constrained, global).

% Run independent validating software and can refuse to adopt any change they dislike, giving the class an absolute veto exercised through simple non-upgrade. The norm preserves that veto as the system's final backstop. Stopping a node is cheap, but their leverage exists only while they keep running one.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, full_node_operators, beneficiary,
    organized, biographical, mobile, global).

% Build payment channels, sidechains, and overlay systems on top of the rarely-changing base. The frozen base layer is their stable substrate: they innovate without waiting for base-layer politics, and the norm channels talent and capital toward their layer rather than the one beneath it.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, layer2_protocol_builders, beneficiary,
    moderate, biographical, mobile, global).

% Review, merge, or reject proposed protocol changes and administer the improvement-proposal process. They enforce the consensus threshold and are bound by it: their own proposals advance only when essentially the whole community signs on, so their best work routinely dies in review without formal rejection. Their standing, livelihoods, and sense of stewardship are fused with the role; stepping away means leaving the institution they embody.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, core_maintainer_gatekeepers, agenda_setter,
    institutional, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__protocol_ossification_reading, core_maintainer_gatekeepers, payer).

% Need confidentiality at the base layer — ordinary users, merchants, and activists whose transaction graphs are permanently public. Improvements that would help them stall indefinitely. Migrating to privacy-focused alternative chains means surrendering liquidity, tooling, and the security budget of the largest network.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, l1_privacy_use_cases, payer,
    moderate, immediate, constrained, global).

% Small-value senders, remittance corridors, and would-be everyday purchasers face fees and confirmation times shaped by deliberately scarce blockspace. Direct peer-to-peer purchase has migrated to custodial services that reintroduce intermediaries. Leaving for cheaper chains forfeits network effects and, for those holding the asset, the asset itself.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, fee_burdened_transactors, payer,
    powerless, immediate, constrained, global).

% Propose new opcodes, covenant constructions, and scaling designs. Under the norm their work waits years for a consensus that never forms, with no formal rejection to free them to move on. Some continue unpaid; others take their research to ecosystems that accept change. Their reputations anchor to a protocol that cannot absorb their output.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, bip_authors_protocol_researchers, payer,
    moderate, biographical, constrained, global).

% Study the governance experiment from outside: comparing its amendment threshold against other decentralized systems, documenting the fork war, and publishing analyses of how leaderless systems bind themselves. They neither receive nor bear the arrangement's flows.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, academic_governance_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper__protocol_ossification_reading, long_term_holders).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper__protocol_ossification_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the commitment problem of a leaderless monetary system: no faction — miners, developers, exchanges, or states — can unilaterally rewrite monetary rules, because any change must persuade essentially every constituency, including node operators who can simply refuse to validate. This makes credible the promise that today's rules will still govern tomorrow's holdings.
% TRANSFER_FUNCTION: Moves decision rights over base-protocol evolution away from would-be changers (feature-seeking developers, users needing capability) and toward the status-quo coalition of holders, miners, and validating nodes; concomitantly moves the option value of future improvement from all users into the certainty premium embedded in the asset.
% ABSENT_VOICES: Users who need base-layer capabilities — on-chain privacy, higher throughput, new scripting — have no formal seat: they object in forums and conferences but hold no veto and no agenda-setting power. Future participants not yet in the system are unrepresented entirely, though they inherit the frozen rules. The bearing classes are diffuse and unorganized; unlike the node operators who forced the UASF episode, they have shown no coalition vector capable of compelling a change.
% DISAPPEARANCE_RATIONALE: If the norm vanished overnight, competing factions would immediately press incompatible changes — larger blocks, covenant opcodes, altered issuance — each backed by some combination of miners, exchanges, and developers. The chain would face serial contentious forks, the certainty premium would evaporate until a new equilibrium consolidated, and development energy would scatter across competing versions. Arrangements throughout the ecosystem — custody, lending, corporate treasuries, overlay routing — depend on the rules not moving.
% FOUNDING_PROBLEM: After the 2015-2017 block-size conflict nearly split the chain, the surviving community generalized the lesson: a leaderless system cannot survive repeated contested changes, so legitimacy for any protocol change must approach unanimity, and stability of the rules outranks growth of capability.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties, the historical record of the block-size wars — documented by journalists, academic studies of Bitcoin governance, and testimony from the losing fork's own participants — attests that the founding problem (contentious-fork fragility and capture risk) was real. Whether it remains live is disputed: privacy researchers and base-layer advocates attest the problem is now invoked to freeze improvements the original design never contemplated, while holders and maintainers attest that capture risk persists with every activation debate. External corroboration exists for the problem's reality; its continuing salience is contested along exactly the beneficiary/bearing line.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__protocol_ossification_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__protocol_ossification_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__protocol_ossification_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
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
 *   Extractiveness 0.58: the norm delivers a real commitment device, but its costs compound — constrained blockspace keeps fees high, base-layer privacy never arrives, and proposal authors burn years against a threshold that never forms. Suppression 0.60: enforcement is social and professional (gatekeeping, ostracism of dissenters, fork-war mobilization) rather than legal, and forks remain lawful — BCH happened — so suppression is substantial but not total. Theater ratio 0.26: the consensus process genuinely vets changes, though 'decentralization' discourse grows more ritualized as actual change capacity atrophies. Accessibility collapse 0.45: alternatives (forks, alternative chains, overlays) persist but trade at a heavy network-effect and Lindy discount. Resistance 0.55: sustained and real — the fork war itself, recurring covenant and drivechain fights, ongoing privacy advocacy. The temporal series run on one shared grid (points 0,2,4,6,8). The trajectories tell a coherent entrenchment story: suppression_requirement FALLS (0.70 to 0.60) as war-mode enforcement relaxes into a self-enforcing norm, while base_extractiveness RISES (0.44 to 0.58) as the opportunity costs of the frozen base layer compound year over year, and theater creeps upward as the community's self-description drifts from operating principle to identity badge. Falling enforcement with rising burden is the signature of a norm that no longer needs to be defended loudly because its losers have stopped expecting the door to open.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently and the engine computes that divergence from the structural data. From the long-term holder's seat the norm is a guardian arrangement — the thing that makes savings survivable — and computes near-coordination-pure. From the bearing seats (privacy users, fee-burdened senders, stalled researchers) the same structure is a closed door maintained by other people's veto power, computing toward the enforced-extraction end. The maintainer seat is genuinely split: it wields the gate and is impaled on it, administering a threshold its own best work cannot pass. Same nominal ecosystem, radically different experienced arrangements — differentiated by directionality and exit, not by power alone.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real flows: holders collect the certainty premium, miners protect stranded capital and fee markets, node operators preserve their veto sovereignty, overlay builders receive the innovation mandate. Victim declarations map to real foregone capability: on-chain privacy never ships, small-value payments price out, proposals stall without closure, and maintainers' own agendas die in review. Exit asymmetry drives the spread: node operators can quit cheaply (mobile, pulling them toward the beneficiary end despite their veto), miners and holders are sunk-cost-constrained, and the bearing classes face network-effect walls that make their exit formally available and practically punishing. No directionality overrides were needed: the beneficiary/victim declarations plus exit atoms produce the right shape, including the maintainer dual position captured by listing that group under both agenda-setting and bearing roles.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a leaderless system cannot survive repeated contested changes — remains live, so this is not a resolved mandate and the arrangement is not drifting toward inertial maintenance. The classification guards against two mislabels. Calling the norm pure extraction ignores the coordination function that genuinely holds: without a near-unanimity requirement, any well-funded faction could rewrite monetary rules, and the certainty premium everything else rests on evaporates. Calling it pure coordination ignores the compounding asymmetric burden the same structure imposes on classes with no agenda-setting power. The live risk is forward-looking: if overlay layers mature to the point where frozen-base capability costs approach zero, the norm's burden collapses and it decays toward theatrical maintenance of a settled question — the theater_ratio series is the early-warning instrument for that transition, and its slow climb from 0.16 to 0.26 is worth watching without yet being diagnostic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the bitcoin_whitepaper kernel; what would the sibling readings (p2p_cash_reading, digital_gold_reading) change structurally?',
    'Comparative classification across the three reading stories: locate the disagreement in whether the whitepaper obligates capability growth (p2p_cash_reading), mandates only scarcity persistence (digital_gold_reading), or prescribes procedure only (this reading).',
    'Under the p2p_cash_reading the same frozen protocol computes far more burdensome (blocked scaling defeats the text''s stated purpose); under the digital_gold_reading it computes near-benign (stability is the point). Values are reading-indexed; the referent — the standing consensus-threshold arrangement — is shared across all three.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one of three readings of the whitepaper kernel; disagreement located in what the founding text obligates.').

omega_variable(
    technical_necessity_vs_incumbent_choice,
    'Is the near-universal threshold a discovered requirement of leaderless Nakamoto consensus, or a constructed governance choice that happens to favor incumbent holders, miners, and validating nodes?',
    'Cross-chain comparison of amendment regimes (on-chain governance chains, lower-threshold forks) measuring capture frequency, contentious-fork rate, and monetary-credibility outcomes against this chain''s record.',
    'If constructed, the norm carries false-summit pressure — presented as technical necessity while identifiable parties collect from its operation; if necessary, part of the measured burden is the irreducible price of the commitment device itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_necessity_vs_incumbent_choice, conceptual, 'Whether the consensus threshold is natural law of decentralized systems or incumbent-favoring construction.').

omega_variable(
    fork_exit_effectiveness,
    'Does the demonstrated ability to fork constitute meaningful exit from the consensus norm, or do network effects and the Lindy premium render exit nominal?',
    'Measure realized migration: market share, developer counts, and liquidity of major forks relative to the parent chain across the interval.',
    'If exit is nominal, effective pressure on the bearing classes exceeds the authored scalar and those seats sit nearer full-target than the structural data suggests; if exit is real, the norm''s coercive overhead is genuinely bounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fork_exit_effectiveness, empirical, 'Whether forking is a real exit option or a discounted one.').

omega_variable(
    l2_substitution_lossiness,
    'Can higher layers fully substitute the base-layer capabilities the norm blocks (confidential transactions, throughput, new scripting), or is substitution lossy in ways that concentrate uncompensated costs on specific user classes?',
    'Compare security, privacy, and custody guarantees of overlay implementations against the counterfactual base-layer designs researchers have proposed and the norm has stalled.',
    'If substitution is lossy, the bearing classes carry net uncompensated costs and the burden is real rather than deferred; if overlays genuinely deliver equivalent function, much of the measured cost is transition friction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(l2_substitution_lossiness, empirical, 'Whether layer-2 innovation compensates for frozen base-layer capability.').

omega_variable(
    threshold_strictness_ambiguity,
    'What actually counts as ''approaching universal'' consensus — unanimous economic-node adoption, a supermajority of hashpower, or something between — and does the operative threshold match the professed one?',
    'Audit activation histories (Taproot''s miner-signaling-plus-height activation, earlier soft forks) against the rhetoric of unanimity; survey maintainers and node operators for their revealed standards.',
    'A looser operative threshold lowers the effective barrier to change and shrinks the bearing set; a stricter one entrenches it further. The ambiguity lets the norm flex toward whichever reading best preserves itself in a given dispute.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threshold_strictness_ambiguity, conceptual, 'Gap between the professed unanimity standard and the threshold actually applied.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__protocol_ossification_reading, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement(bitc_tr_t2, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 2, 0.19).
narrative_ontology:measurement(bitc_tr_t4, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 6, 0.24).
narrative_ontology:measurement(bitc_tr_t8, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 8, 0.26).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(bitc_be_t2, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 2, 0.49).
narrative_ontology:measurement(bitc_be_t4, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 4, 0.53).
narrative_ontology:measurement(bitc_be_t6, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 6, 0.56).
narrative_ontology:measurement(bitc_be_t8, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 8, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(bitc_su_t2, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 2, 0.66).
narrative_ontology:measurement(bitc_su_t4, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 4, 0.63).
narrative_ontology:measurement(bitc_su_t6, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 6, 0.61).
narrative_ontology:measurement(bitc_su_t8, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 8, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__protocol_ossification_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_whitepaper__digital_gold_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_whitepaper__p2p_cash_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Bitcoin' (and the whitepaper behind it) covers three structurally distinct claims that must not share one epsilon. This story authors the protocol_ossification_reading — the governance-norm constraint on base-layer change. The digital_gold_reading authors the scarcity/store-of-value arrangement; the p2p_cash_reading authors the censorship-resistant-payments arrangement, for which the same frozen protocol computes substantially more costly because blocked scaling defeats the text's stated purpose. Upstream/downstream: this reading structurally influences the p2p_cash_reading (by blocking base-layer scaling it forces cash functionality into custodial overlays, changing that reading's operating environment without foreclosing it) and coexists with the digital_gold_reading (both are commonly held by the same coalitions; neither logically eliminates the other). Each family member links the others via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
