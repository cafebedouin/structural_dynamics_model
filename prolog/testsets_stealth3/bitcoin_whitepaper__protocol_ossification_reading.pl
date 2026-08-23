% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__protocol_ossification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
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
 *   human_readable: Near-Universal Consensus Gate on Bitcoin Base-Layer Change
 *   domain: cryptocurrency_economics/monetary_systems/technology_governance
 *
 * SUMMARY:
 *   This story instantiates the protocol_ossification_reading of the
 *   bitcoin_whitepaper kernel: the governance norm under which changes to
 *   Bitcoin's base consensus rules are illegitimate unless they approach
 *   universal acceptance, with stability held as the system's primary virtue.
 *   The norm has a genuine coordination core, since any change adopted over
 *   substantial dissent risks partitioning the ledger into two incompatible
 *   currencies, and a real extraction asymmetry, since the effective veto
 *   belongs to the most change-resistant adopters while every constituency
 *   whose use case requires base-layer modification (on-chain privacy,
 *   base-layer scaling, covenant and signature-hash features) pays for the
 *   incumbents' certainty. Per the epsilon-invariance principle this is one
 *   of three linked family stories; the sibling readings (p2p cash, digital
 *   gold) are separate files with their own epsilon, beneficiaries, and
 *   victims. Epsilon's referent here is the standing consensus-gate
 *   arrangement itself, assessed as it operates, never the arrangement any
 *   reading would prefer. The reading's own adherents experience low
 *   effective extraction through their beneficiary directionality, while
 *   blocked constituencies experience high effective extraction; the engine
 *   computes that per-seat divergence from the structural data. Assumptions:
 *   interval 0-10 maps to 2015-2025, the norm's operative life from the
 *   block-size war's onset to the present; metric values are
 *   endpoint-assessed at 2025.
 *
 * KEY AGENTS:
 *   - long_term_hodlers: primary beneficiary (powerful/identity_locked) — collects the stability premium; their thesis fuses with the immutability guarantee
 *   - economic_full_node_operators: enforcement core and secondary beneficiary (organized/constrained) — each operator's upgrade refusal is an effective veto; collectively they are what activation consists of
 *   - bitcoin_core_maintainers: agenda setter (institutional/identity_locked) — control the review gate every proposal passes; cannot activate alone but can starve any proposal
 *   - incumbent_miner_operators: secondary beneficiary (powerful/constrained) — single-purpose capital protected by rule stability
 *   - custodial_exchanges_and_funds: institutional beneficiary (institutional/arbitrage) — profit from predictability but can follow any winning chain
 *   - base_layer_feature_developers: primary target (moderate/constrained) — proposals die in review; careers and reputation are ecosystem-specific
 *   - privacy_dependent_users: target (powerless/trapped) — need base-layer privacy changes the gate blocks; every exit is worse than the position
 *   - high_fee_small_transactors: target (powerless/constrained) — priced out at saturation; relief routes run through custodians
 *   - altchain_innovators: excluded (moderate/mobile) — run the foreclosed experiments elsewhere; no seat in the conversation
 *   - protocol_governance_analysts: analytical observer (analytical/analytical) — comparative record, no stake in activation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__protocol_ossification_reading, 0.6).
domain_priors:suppression_score(bitcoin_whitepaper__protocol_ossification_reading, 0.66).
domain_priors:theater_ratio(bitcoin_whitepaper__protocol_ossification_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__protocol_ossification_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__protocol_ossification_reading, "Near-Universal Consensus Gate on Bitcoin Base-Layer Change").
narrative_ontology:topic_domain(bitcoin_whitepaper__protocol_ossification_reading, "cryptocurrency_economics/monetary_systems/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__protocol_ossification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__protocol_ossification_reading, 'b3613042-fdb7-4ac9-adee-11d91b5b1e47').
narrative_ontology:cs_kernel_codification('b3613042-fdb7-4ac9-adee-11d91b5b1e47', fixed_text).
narrative_ontology:cs_authority_grounding('b3613042-fdb7-4ac9-adee-11d91b5b1e47', lineage).
narrative_ontology:cs_interpretation_layer_present('b3613042-fdb7-4ac9-adee-11d91b5b1e47').
narrative_ontology:cs_reading_relation('b3613042-fdb7-4ac9-adee-11d91b5b1e47', bitcoin_whitepaper__p2p_cash_reading, influences).
narrative_ontology:cs_reading_relation('b3613042-fdb7-4ac9-adee-11d91b5b1e47', bitcoin_whitepaper__digital_gold_reading, influences).
narrative_ontology:cs_axiom('b3613042-fdb7-4ac9-adee-11d91b5b1e47', foundational, near_universal_consensus_gates_protocol_change).
narrative_ontology:cs_axiom_status(near_universal_consensus_gates_protocol_change, holdable).
narrative_ontology:cs_axiom_grounding('b3613042-fdb7-4ac9-adee-11d91b5b1e47', near_universal_consensus_gates_protocol_change, conventional).
narrative_ontology:cs_axiom('b3613042-fdb7-4ac9-adee-11d91b5b1e47', foundational, monetary_stability_is_primary_virtue).
narrative_ontology:cs_axiom_status(monetary_stability_is_primary_virtue, holdable).
narrative_ontology:cs_axiom_grounding('b3613042-fdb7-4ac9-adee-11d91b5b1e47', monetary_stability_is_primary_virtue, instrumental).
narrative_ontology:cs_reference_frame('b3613042-fdb7-4ac9-adee-11d91b5b1e47', satoshi_design_immutability).
narrative_ontology:cs_drift_state('b3613042-fdb7-4ac9-adee-11d91b5b1e47', contemporary_post_taproot_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('b3613042-fdb7-4ac9-adee-11d91b5b1e47', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, long_term_hodlers).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, incumbent_miner_operators).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, custodial_exchanges_and_funds).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, base_layer_feature_developers).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, privacy_dependent_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, high_fee_small_transactors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_core_maintainers).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, economic_full_node_operators).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__protocol_ossification_reading, credible_scarcity_doctrine).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__protocol_ossification_reading, social_consensus_layer_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold merge responsibility for the reference client most nodes run. They decide which protocol proposals receive sustained review and which stall; they cannot activate a change alone, but their review queue is the narrow passage every proposal must survive. Their standing in the community rests on stewardship of the existing rules, and their careers and social worlds are inside the project, so proposing or merging a divisive change would cost them that standing.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_core_maintainers, agenda_setter,
    institutional, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_core_maintainers, beneficiary).

% Run fully-validating nodes and choose which software version to accept. Each operator's refusal to adopt a new rule set is an effective veto, and their aggregate adoption decisions are what activation consists of. They gain predictable validation rules and freedom from forced upgrades. Running a node is a hobby-or-business commitment relatively few users undertake, so this seat is small relative to the holder population.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, economic_full_node_operators, agenda_setter,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__protocol_ossification_reading, economic_full_node_operators, beneficiary).

% Hold the asset across years or decades as a savings vehicle, with portfolios and public identities organized around the promise that issuance and the supply cap will never change. Every protocol revision is a tail risk to that promise. Selling is the only exit, and selling contradicts the thesis they hold; many are also the loudest voices policing deviation from the stability line.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, long_term_hodlers, beneficiary,
    powerful, generational, identity_locked, global).

% Operate warehouses of single-purpose hashing hardware whose resale value outside this network is near zero. Changes to proof-of-work, difficulty adjustment, or block subsidy would strand that capital; stability protects the amortization schedules. They periodically attempt to push favored changes, as in the 2017 agreement to back larger blocks, but their structural interest lies with the rules their machines already implement.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, incumbent_miner_operators, beneficiary,
    powerful, biographical, constrained, global).

% Custody the majority of coins and provide the on-ramps most newcomers use. Predictable rules minimize their operational and compliance overhead. Unlike committed holders, they can follow whichever chain markets reward and list competing assets freely, so their loyalty runs to liquidity rather than to any particular rule set.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, custodial_exchanges_and_funds, beneficiary,
    institutional, biographical, arbitrage, global).

% Design and champion protocol improvements: covenant opcodes, signature-hash modes that would enable more robust second-layer designs, cross-input signature aggregation, stronger privacy tooling. Their proposals pass through years of review, mailing-list debate, and public criticism, and most die without activation. Their expertise and reputations are ecosystem-specific; pivoting to other networks carries a community cost, and building on higher layers instead means abandoning the problems they wanted to solve at the base.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, base_layer_feature_developers, payer,
    moderate, biographical, constrained, global).

% Need transactions that surveillant employers, creditors, or governments cannot easily trace: people remitting wages, escaping confiscation, or refusing commercial surveillance. Meaningful on-chain privacy improvements require the kind of protocol change the consensus gate blocks, so they rely on workarounds with weaker guarantees. Moving to privacy-focused alternative networks exposes them to delisting and legal attention, so every exit is worse than the position.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, privacy_dependent_users, payer,
    powerless, biographical, trapped, global).

% Send small or urgent payments and face fees that spike whenever block space saturates. Scaling the base layer would relieve them, but capacity changes are exactly what the consensus gate resists; the available relief routes run through custodial services that reintroduce the intermediaries the system was built to remove. Alternative networks exist but lack the merchant acceptance and liquidity they depend on.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, high_fee_small_transactors, payer,
    powerless, immediate, constrained, global).

% Experiment with the features the gate blocks, such as scheduled hard-fork cadences, rich scripting, and privacy defaults, on separate networks. They are outside the conversation that decides Bitcoin's rules; their exclusion is maintained by the norm itself, since importing their innovations would require the very changes the gate vetoes. They would open the gate to feature competition if they had a seat.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, altchain_innovators, excluded,
    moderate, biographical, mobile, global).

% Study decentralized governance comparatively: activation histories, veto patterns, and fork outcomes across networks. They take no position in activation fights and bear none of the costs; their contribution is the comparative record the other seats argue from.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, protocol_governance_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper__protocol_ossification_reading, long_term_hodlers).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper__protocol_ossification_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents network partition: any base-protocol change adopted over substantial dissent risks splitting the ledger into two incompatible currencies, destroying the fungibility and security budgets of both sides. The near-universal threshold ensures only changes acceptable to essentially all validators, miners, merchants, and holders activate, and it keeps the monetary schedule constant and independently auditable.
% TRANSFER_FUNCTION: Moves protocol-amendment rights from change proponents (feature developers, unserved user classes) to the most conservative adopters: any sufficiently large minority of node operators can veto a change indefinitely, so effective control concentrates in the least change-inclined coalition. It also moves optionality from future users, whose use cases are foreclosed, to present holders, who receive certainty.
% ABSENT_VOICES: Future users and not-yet-existing use cases have no seat. Developers of blocked proposals sit inside the conversation but without merge-path leverage. High-fee-region users participate mostly as exchange customers rather than node operators. Altchain builders running the foreclosed experiments are outside the conversation entirely and would object that the veto is exercised over experiments they were never permitted to run.
% DISAPPEARANCE_RATIONALE: If the near-universal threshold vanished overnight and plurality acceptance sufficed, competing client teams would ship conflicting changes on a regular cadence; the first contested activation would split the ledger, exchanges would suspend withdrawals amid the ambiguity, and the store-of-value premium built on the promise that the rules never change would reprice sharply. The ecosystem would reorganize around whatever governance equilibrium emerged from the first split.
% FOUNDING_PROBLEM: A sequence of near-miss and actual partitions, including the March 2013 accidental chain split, the 2015 mid-year fork scare, and the 2017 block-size war ending in the Bitcoin Cash split, showed that protocol changes pushed without overwhelming support can permanently divide the network and destroy value on both sides.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: independent engineering post-mortems of the 2013 levelDB split, exchange incident-response documentation for chain-split handling, academic distributed-systems literature on consensus thresholds, and notably the blocked developers themselves, who attest the founding problem was real while disputing that it justifies the current veto breadth.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__protocol_ossification_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__protocol_ossification_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__protocol_ossification_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_whitepaper__protocol_ossification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__protocol_ossification_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.60: the coordination core is real and load-bearing — split prevention is why even blocked developers accept slow governance — but the veto asymmetry taxes every change-proposing constituency, and a decade of foreclosed use cases has accumulated as dead-weight loss borne by identifiable seats. Suppression 0.66 is a raw structural property, unscaled; only extractiveness is scaled by directionality and scope in the engine's computation. Enforcement is social-technical: review gatekeeping, node-refusal mechanics, exchange listing discipline, and ostracism of proposal authors framed as attacks. Alternatives exist (forks, altchains, layers) but exiting forfeits network effects and liquidity, so suppression is substantial without being total. Theater_ratio 0.32: security review and testing are genuine work, but a growing share of stability discourse is purity performance — rhetoric declaring the rules never change that overstates a record which includes SegWit and Taproot. Accessibility_collapse 0.55: alternatives partially collapse once the norm is understood, since fork exits have historically failed and retained little value, yet layers and rival networks keep substitute paths visible. Resistance 0.55: a continuous proposal stream (covenants, new sighash modes, drivechain debates) and the memory of the block-size war keep contestation alive; the victim classes are fragmented across developers, users, and outsiders with no coalition mechanism, which caps realized resistance. The three measurement series share one grid (t=0,2,4,6,8,10) so every metric is authored at every examined point. Suppression_requirement is authored because enforcement machinery visibly hardened over the interval — purity tests, proposal-abandonment patterns, and the maturation of social sanction — not merely because extraction shifted.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda seats should compute different types from identical structure. From the maintainer and hodler seats the norm is the constitution that makes the asset what it is: change is risk, and the gate is prudence. From the blocked-developer and unserved-user seats the same gate is a veto machine that converts their roadmaps into petitions before a hostile jury. Two institutional actors illustrate same-power divergence: custodial exchanges (institutional, arbitrage-grade exit) experience the norm as cheap predictability they can abandon at any reorganization, while maintainers (institutional, identity-locked stewardship) experience it as an identity they cannot walk away from. Identity-lock mechanisms differ by seat: hodlers fuse ideologically, where exit equals thesis betrayal; maintainers fuse professionally and communally, where standing dissolves on departure; developers are locked by career path dependence, since their expertise is ecosystem-specific. If the hodler identity frame broke — for instance, if a credible competing scarce asset absorbed the store-of-value premium — the beneficiary coalition's willingness to enforce would drop sharply.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive hodlers, miners, and custodians toward the beneficiary end of directionality; identity-lock pushes hodlers furthest, since their only exit contradicts their thesis, while custodians' arbitrage-grade exit places them nearest the beneficiary pole despite institutional power, because they can follow any chain. Victim declarations drive developers (constrained: reputation and career lock), privacy users (trapped: every exit is worse than the position), and fee-burdened transactors (constrained) toward the target end, with trapped seats nearest full-target. Maintainers and node operators are agenda-setter seats with partial benefit; the derivation places them mid-range, which matches their mixed position — they administer the norm, absorb its pressures, and collect standing from it. No directionality_overrides are authored: the derivation handles every seat, and the available override granularity (one d per power atom) would misprice this story's divergent institutional seats — custodians and maintainers share the institutional atom but sit at opposite ends, so any single override damages one of them.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification guards against both mislabels. Calling this a snare ignores that split-prevention is a genuine coordination function the victims themselves depend on — even blocked developers do not want casually contested hard forks — and that enforcement is consent-adjacent, since operators choose their own software. Calling it a rope ignores the veto asymmetry: the threshold grants the most conservative coalition a permanent tax on every change-proposing constituency, and the beneficiary set is identifiable and concentrated enough to collect. Mandatrophy status: the founding problem (partition catastrophe) is still live, so the mandate has not outlived its function and no resolved-mandatrophy flag is declared. The founding_problem_severity_decay omega tracks whether replay-protection maturity and institutional split-handling eventually decay the mandate while the norm persists at full strength; the drift path from tangled_rope toward inertial maintenance would run through that omega resolving affirmatively.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates the protocol_ossification_reading of the bitcoin_whitepaper kernel; would instantiating the p2p_cash_reading or digital_gold_reading instead yield a different constraint with a different victim set and epsilon?',
    'Author and compile the sibling stories and compare victim sets, directionality distributions, and computed types across the family.',
    'Under the p2p_cash reading the same governance norm presents a larger victim set (all cash-utility use cases) and higher epsilon; under the digital_gold reading the victim set shrinks and the norm computes closer to pure coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Kernel-contest routing: sibling readings instantiate different constraints from the same text.').

omega_variable(
    consensus_threshold_naturalness,
    'Is the near-universal activation threshold a discovered requirement of secure decentralized consensus, or a socially constructed norm that happens to serve incumbent holders?',
    'Comparative governance study across proof-of-work networks with different change cadences: do lower thresholds systematically produce instability, or do mature replay-protection and exchange practices contain split risk at lower thresholds?',
    'If constructed, the norm loses its naturality defense and reclassifies toward heavier extraction; if discovered, the coordination component strengthens and the rope share rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_threshold_naturalness, empirical, 'Naturality of the supermajority threshold.').

omega_variable(
    veto_diffusion_vs_concentration,
    'Does the veto embedded in the consensus requirement rest diffusely across thousands of node operators, or effectively in the small set of maintainers and influential voices who control which proposals ever reach an adoption decision?',
    'Trace the activation pipeline historically: catalog proposals that died at review or discussion stage versus those that reached signaling, and identify where each death occurred.',
    'Effective concentration converts the norm from coordination with a conservative tilt into enforced extraction with a coordination cover story; confirmed diffusion supports the tangled-rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_diffusion_vs_concentration, empirical, 'Where the veto actually sits.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the enforcement that holds the stability norm in place structural (activation mechanics, review gatekeeping, exchange listing behavior) or internalized (ideological fusion in which protocol change has become unthinkable for community members)?',
    'Post-exit trajectory analysis of splinter communities: if members who leave for other networks continue enforcing purity norms, the suppression is partly internalized; if enforcement dissolves on exit, it is structural.',
    'Internalized suppression raises the effective suppression above the structural measure and predicts persistence of the norm even if activation mechanics change; purely structural suppression would relax quickly if the gatekeeping seats changed hands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism of the norm''s enforcement.').

omega_variable(
    founding_problem_severity_decay,
    'Has the severity of the founding problem (chain-split catastrophe) decayed as replay protection, exchange procedures, and institutional split-handling matured, while the norm persists at full strength?',
    'Incident-rate and cost analysis of splits and near-splits after 2017 versus before; assess whether a modern contentious split would still be catastrophic.',
    'Confirmed decay shifts the story toward mandate-outlived-function territory and flags inertial drift; persistent severity confirms the mandate remains live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_severity_decay, empirical, 'Whether the founding problem still justifies the veto breadth.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__protocol_ossification_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(bitc_tr_t0, observed).
narrative_ontology:measurement(bitc_tr_t2, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 2, 0.22).
narrative_ontology:measurement_basis(bitc_tr_t2, observed).
narrative_ontology:measurement(bitc_tr_t4, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement_basis(bitc_tr_t4, observed).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 6, 0.27).
narrative_ontology:measurement_basis(bitc_tr_t6, observed).
narrative_ontology:measurement(bitc_tr_t8, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement_basis(bitc_tr_t8, observed).
narrative_ontology:measurement(bitc_tr_t10, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(bitc_tr_t10, observed).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(bitc_be_t0, observed).
narrative_ontology:measurement(bitc_be_t2, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 2, 0.48).
narrative_ontology:measurement_basis(bitc_be_t2, observed).
narrative_ontology:measurement(bitc_be_t4, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement_basis(bitc_be_t4, observed).
narrative_ontology:measurement(bitc_be_t6, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 6, 0.55).
narrative_ontology:measurement_basis(bitc_be_t6, observed).
narrative_ontology:measurement(bitc_be_t8, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement_basis(bitc_be_t8, observed).
narrative_ontology:measurement(bitc_be_t10, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement_basis(bitc_be_t10, observed).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(bitc_su_t0, observed).
narrative_ontology:measurement(bitc_su_t2, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 2, 0.54).
narrative_ontology:measurement_basis(bitc_su_t2, observed).
narrative_ontology:measurement(bitc_su_t4, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 4, 0.58).
narrative_ontology:measurement_basis(bitc_su_t4, observed).
narrative_ontology:measurement(bitc_su_t6, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 6, 0.61).
narrative_ontology:measurement_basis(bitc_su_t6, observed).
narrative_ontology:measurement(bitc_su_t8, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 8, 0.64).
narrative_ontology:measurement_basis(bitc_su_t8, observed).
narrative_ontology:measurement(bitc_su_t10, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(bitc_su_t10, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__protocol_ossification_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_whitepaper__p2p_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_whitepaper__digital_gold_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the bitcoin_whitepaper kernel per the epsilon-invariance principle: the colloquial label 'Bitcoin' conflates a payments specification (p2p_cash_reading), a scarcity charter (digital_gold_reading), and a governance metarule (this story). Each sibling is a separate file with its own epsilon, beneficiaries, and victims; this story links both because the governance norm is cited as evidence within the gold reading and operates as a binding condition on the cash reading. Upstream/downstream: the gold reading lends this norm its beneficiary structure; this norm degrades the cash reading's operating environment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
