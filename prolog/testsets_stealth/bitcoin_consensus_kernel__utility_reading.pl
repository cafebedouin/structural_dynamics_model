% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__utility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_consensus_kernel__utility_reading, []).

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
 *   constraint_id: bitcoin_consensus_kernel__utility_reading
 *   human_readable: Bitcoin Consensus Baseline as Minimum Viable Specification (Utility Reading)
 *   domain: cryptoeconomics/monetary systems/distributed consensus
 *
 * SUMMARY:
 *   The Bitcoin consensus mechanism — proof-of-work chain selection, the
 *   issuance schedule, and the rule set every full node enforces — is read
 *   here through the utility_reading of the bitcoin_consensus_kernel: the
 *   whitepaper established a minimum viable consensus baseline whose
 *   legitimacy tracks its usefulness, and which soft forks and upper-layer
 *   protocols may legitimately improve. The standing arrangement under
 *   contest, and the referent of every metric in this story, is that baseline
 *   as it actually operates: a globally enforced rule set that coordinates
 *   mutually distrusting parties, collects a security budget through issuance
 *   and fees, and re-opens its own rules on each improvement cycle. Under
 *   this reading the arrangement coordinates genuinely — adopters and
 *   builders capture an improvement dividend — while extracting moderately
 *   and asymmetrically: long-horizon holders bear rule-change risk against an
 *   immutability guarantee the operating frame does not actually fix in
 *   place, node operators bear a recurring upgrade-and-verify burden, and
 *   miners collect rents where revenue decouples from marginal cost. This
 *   story authors ONE reading as a clean, epsilon-invariant constraint; the
 *   maximalist and pragmatic-synthesis readings are separate constraints with
 *   their own epsilon values, linked through the network and reading-relation
 *   surfaces. The claimed type and the metrics are independent authored
 *   facts.
 *
 * KEY AGENTS:
 *   - protocol_developers: agenda-setting co-author (organized/mobile) — draft improvement proposals, maintain reference implementations, shepherd activation; collect no direct revenue from the rules
 *   - mining_operators: enforcement arm and primary monetary receipt (organized/constrained) — point hash power at a rule set; receive issuance and fees; carry capital-stranding risk
 *   - full_node_operators: enforcement backbone and recurring payer (moderate/constrained) — verify every block, absorb the upgrade-and-verify burden of each improvement cycle
 *   - layer2_builders: beneficiary-builders (moderate/mobile) — need guarantees strong enough to build on and flexibility enough to iterate
 *   - protocol_adopters: adopter-beneficiaries paying the fee and upgrade-risk bill (moderate/constrained)
 *   - immutability_reliant_holders: primary target seat (moderate/identity_locked) — hold on a fixed-rules premise the revisable baseline does not guarantee
 *   - maximalist_faction: excluded objectors (moderate/identity_locked) — no formal seat in upgrade governance; objection registers only as post-proposal resistance
 *   - financial_regulators: analytical observer with edge leverage (institutional/analytical) — shape the usability of improvements through exchange and custody rulings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__utility_reading, 0.5).
domain_priors:suppression_score(bitcoin_consensus_kernel__utility_reading, 0.4).
domain_priors:theater_ratio(bitcoin_consensus_kernel__utility_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__utility_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_consensus_kernel__utility_reading, "Bitcoin Consensus Baseline as Minimum Viable Specification (Utility Reading)").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__utility_reading, "cryptoeconomics/monetary systems/distributed consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__utility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__utility_reading, '84746523-fbee-4a88-bfa3-7b5abadefca6').
narrative_ontology:cs_kernel_codification('84746523-fbee-4a88-bfa3-7b5abadefca6', fixed_text).
narrative_ontology:cs_authority_grounding('84746523-fbee-4a88-bfa3-7b5abadefca6', expertise).
narrative_ontology:cs_interpretation_layer_present('84746523-fbee-4a88-bfa3-7b5abadefca6').
narrative_ontology:cs_reading_relation('84746523-fbee-4a88-bfa3-7b5abadefca6', bitcoin_consensus_kernel__maximalist_reading, forecloses).
narrative_ontology:cs_reading_relation('84746523-fbee-4a88-bfa3-7b5abadefca6', bitcoin_consensus_kernel__pragmatic_synthesis, influences).
narrative_ontology:cs_axiom('84746523-fbee-4a88-bfa3-7b5abadefca6', foundational, whitepaper_establishes_minimum_viable_baseline).
narrative_ontology:cs_axiom_status(whitepaper_establishes_minimum_viable_baseline, holdable).
narrative_ontology:cs_axiom_grounding('84746523-fbee-4a88-bfa3-7b5abadefca6', whitepaper_establishes_minimum_viable_baseline, empirically_contingent).
narrative_ontology:cs_axiom('84746523-fbee-4a88-bfa3-7b5abadefca6', foundational, iterative_improvement_is_kernel_faithful).
narrative_ontology:cs_axiom_status(iterative_improvement_is_kernel_faithful, holdable).
narrative_ontology:cs_axiom_grounding('84746523-fbee-4a88-bfa3-7b5abadefca6', iterative_improvement_is_kernel_faithful, instrumental).
narrative_ontology:cs_reference_frame('84746523-fbee-4a88-bfa3-7b5abadefca6', minimum_viable_baseline_specification).
narrative_ontology:cs_drift_state('84746523-fbee-4a88-bfa3-7b5abadefca6', contemporary_layer2_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('84746523-fbee-4a88-bfa3-7b5abadefca6', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, protocol_adopters).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, layer2_builders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, mining_operators).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, immutability_reliant_holders).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, full_node_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, protocol_adopters).
narrative_ontology:constraint_vindicates(bitcoin_consensus_kernel__utility_reading, nakamoto_consensus_sufficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Contribute to reference implementations and draft improvement proposals, shepherding them through review and activation. They collect no direct revenue from the rules they propose; their returns are professional standing, sponsorship income, and the value of the ecosystem their software runs. Exit is real: the same skills are in demand across many ledgers, though reputation accumulated here does not transfer at par.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, protocol_developers, agenda_setter,
    organized, biographical, mobile, global).

% Run the hardware that extends the chain and decide which rule set gets built upon by pointing hash power at it. They receive newly issued coins and transaction fees — the system's direct monetary flow — while carrying capital-intensive, energy-exposed operations and the risk that a rule change or fee-market shift strands equipment or compresses revenue. Redirecting to another ledger means writing off specialized hardware.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, mining_operators, agenda_setter,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__utility_reading, mining_operators, beneficiary).

% Independently verify every block and refuse invalid ones, which is what keeps rule enforcement distributed rather than delegated. Each accepted upgrade asks them to update and re-verify software, and staying current costs ongoing attention; falling behind means silently dropping off the network everyone else transacts on. In exchange they keep the ability to verify their own holdings without trusting anyone.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, full_node_operators, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__utility_reading, full_node_operators, agenda_setter).

% Build payment channels and other upper-layer protocols on top of the base ledger. The base rules' stability plus their openness to change is their business premise: guarantees strong enough to build on, flexibility enough to iterate. Their capital and customers are portable to other platforms, at the cost of rebuilding network presence and liquidity from zero.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, layer2_builders, beneficiary,
    moderate, biographical, mobile, global).

% Hold balances, run businesses, and move value across the ledger. They get a settlement layer no single party controls, and they pay fees that rise with congestion while bearing the disruption risk of each upgrade. Moving to a different ledger means rebuilding custody arrangements, counterparty relationships, and liquidity.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, protocol_adopters, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__utility_reading, protocol_adopters, payer).

% Hold for long horizons on the premise that the monetary rules — the fixed supply cap and issuance schedule — will not change. Under the revisable-baseline frame their guarantee is a policy choice rather than a law of nature: every improvement cycle re-opens the rules they depend on. Their identity as savers in a hard-capped asset is fused with the immutability story, which makes selling psychologically costly even when they object to a proposed change.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, immutability_reliant_holders, payer,
    moderate, generational, identity_locked, global).

% Read the founding document as a covenant fixing monetary policy permanently and oppose every proposed change on principle. They hold no formal seat in upgrade governance: proposals proceed when miners and the economic majority activate them, and the maximalist objection registers only as public argument and refusal to upgrade. Their holdings and public identity are bound up in the no-change reading.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, maximalist_faction, excluded,
    moderate, generational, identity_locked, global).

% Supervise the exchanges, custodians, and on-ramps through which the ledger touches the formal financial system. They do not participate in consensus, but their rulings on what may be traded or custodied shape which improvements are usable in practice. Their seat is observational, with enforcement leverage at the edges of the network rather than in it.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, financial_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_consensus_kernel__utility_reading, mining_operators).
narrative_ontology:fixing_cost_class(bitcoin_consensus_kernel__utility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves Byzantine agreement among mutually distrusting parties over a single transaction history without a trusted third party: double-spending prevention, transaction ordering, and rule enforcement are solved once at the protocol layer, as a deliberately minimal baseline that soft forks and upper layers can extend.
% TRANSFER_FUNCTION: Moves newly issued coins and transaction fees from the issuance schedule and users to mining operators as the security budget; moves rule-change option value from long-horizon holders who prefer fixed rules to builders and adopters who capture the gains from improvements.
% ABSENT_VOICES: Maximalist holders would object to every change but hold no seat in upgrade governance — activation proceeds on miner and economic-majority signal, so their objection arrives only as post-proposal resistance. Future users not yet in the network bear the consequences of baseline choices made before they arrive. Communities absorbing mining's energy and siting load are represented only indirectly, through regulators.
% DISAPPEARANCE_RATIONALE: The mining industry, exchange and custody businesses, upper-layer protocols, and the entire store-of-value position built on the ledger presuppose the consensus baseline. If it vanished overnight, every balance becomes unverifiable, every channel and custody arrangement dissolves, and the ecosystem reorganizes around whatever successor mechanism captures the displaced demand.
% FOUNDING_PROBLEM: Peer-to-peer electronic cash needed a way for strangers to agree on a single transaction history without a trusted intermediary — a minimum viable mechanism that solves double-spending once, at the protocol layer, so everything else can be built on top.
% FOUNDING_PROBLEM_CORROBORATION: The founding document attests it from outside the current benefiting parties: its title and abstract frame the mechanism as a solution to peer-to-peer electronic cash, and the author's early communications treat the design as a starting point with versioning and extensibility built in. Continued payment usage and upper-layer deployment corroborate that the problem remains live. The maximalist faction attests a different founding problem — immutable scarcity — and that disagreement is the live contest this reading sits inside; no beneficiary seat is the source of the attestation offered here.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__utility_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__utility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__utility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_consensus_kernel__utility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_consensus_kernel__utility_reading, 0.5, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_consensus_kernel__utility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_consensus_kernel__utility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_consensus_kernel__utility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.5 (moderate): the security budget is largely payment for a real service, but miner revenue decouples from marginal cost at scale, node operators absorb an unreimbursed upgrade burden, and long-horizon holders bear rule-change risk against a guarantee the operating frame does not hold fixed. Suppression is authored at 0.4 as a raw structural property — it is NOT scaled by power or scope; only extractiveness is scaled in the engine's arithmetic. The baseline is enforced by economic-majority partition: dissenters are not coerced into the upgraded network, they are left behind on a branch nobody accepts, and exit to other ledgers exists but is discounted by network effects. Theater is low (0.15): the mechanism does what it claims; the performative share is mostly decentralization rhetoric and node-count display, spiking during the 2017 block-size war when positioning substituted for engineering. Accessibility collapse is low (0.3): rival ledgers and fork variants persist as live alternatives, discounted rather than eliminated. Resistance is moderate (0.4): the block-size war, maximalist objection, and upgrade hesitancy are real, recurring friction the activation process must overcome. The suppression_requirement series is authored because this story specifically tracks enforcement-capacity change: enforcement machinery built toward the 2017 activation crisis, then partially relaxed as soft-fork activation norms matured. All three series share one time grid — every tracked metric is authored at every examined point — and the end-state values match the base_properties scalars. Claim and metrics are independent: the tangled_rope claim is what the utility reading's seat holds structurally true; the metrics describe the arrangement as this reading assesses it.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the developer and builder seats the baseline is enabling: a floor strong enough to build on and flexible enough to iterate — a coordination structure they would describe as mostly benign. From the immutability-reliant holder's seat the same structure is a standing rule-change risk attached to their savings; from the node operator's seat it is a recurring assessment of attention and verification labor. Miners experience the arrangement as payment for service plus competitive rent; regulators experience only its edges through exchange and custody supervision. The engine computes these per-seat divergences from the structural data; this story's claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (protocol_adopters, layer2_builders, mining_operators) derive low directionality; declared victims (immutability_reliant_holders, full_node_operators) derive high directionality, with the holders' identity-locked exit seating them near the full-target end because identity fusion blocks the repricing exit that would damp their effective extraction. One override is authored at the organized-power atom (d=0.25): a pure-beneficiary derivation would seat both organized agents near 0.1, but both mix beneficiary position with enforcement duty and cost-bearing that the derivation misses — developers carry unreimbursed coordination labor and legal exposure, and miners' revenue is the extraction vector itself, so neither sits at the beneficiary end. Node operators keep their derived high d despite their verification benefit because their payer role is recurring and structural rather than incidental.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — trustless agreement on a single transaction history — is live under this reading, so no mandatrophy is declared: the arrangement has not outlived its function. The classification work this reading performs is boundary-keeping. It refuses the maximalist move of reading every upgrader as a covenant-violator, which would misread a live coordination dividend as pure extraction; it equally refuses the pure-rope move of erasing the holder's rule-change risk and the node operator's upgrade burden, which would misread asymmetric extraction as pure coordination. The omega on the improvement mandate tracks the reading's own degradation path: if base-layer improvement becomes practically unreachable and all evolution migrates to upper layers, the utility frame collapses in practice into the pragmatic synthesis and this story's coordination claim narrows to the baseline as it stands.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the bitcoin_consensus_kernel — the utility_reading, which takes the whitepaper as a minimum viable baseline enabling iterative improvement. How would the sibling readings restructure the same standing arrangement?',
    'Author and compile the sibling stories — maximalist_reading and pragmatic_synthesis — then compare beneficiary/victim sets and epsilon across the family. Under the maximalist reading the sets invert: immutability-reliant holders become beneficiaries and upgrade advocates become targets, with measured extraction rising sharply. Under the pragmatic synthesis the arrangement splits into two constraints (immutable base layer, free upper layers) with separate epsilon values.',
    'Classification is reading-relative: this story''s tangled_rope verdict does not transfer across the kernel. The kernel-level dispute is located in what the whitepaper text establishes about base-layer revisability, and only cross-reading comparison takes that measurement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story is the utility_reading of a contested kernel; sibling readings restructure beneficiary/victim sets and split or raise epsilon.').

omega_variable(
    ossification_victim_status,
    'Do immutability_reliant_holders genuinely bear asymmetric costs under this reading, or is rule-change risk a disclosed, priced-in property of a voluntarily held asset?',
    'Examine adoption-era disclosures and holder onboarding: if the asset was marketed on absolute immutability (the maximalist frame) while the operating frame is revisable, the gap between promise and delivery is the extraction; if revisability was disclosed at adoption and holders could reprice, the payer seat weakens and the arrangement trends toward pure coordination.',
    'Determines whether the victim declaration is honest or over-claimed. A priced-in-risk resolution would lower effective extraction toward the beneficiary seats and pull the computed type toward rope; a promise-gap resolution confirms the tangled_rope asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ossification_victim_status, conceptual, 'Whether the holder victim seat reflects real extraction or voluntary exposure to a disclosed property of the asset.').

omega_variable(
    improvement_mandate_reality,
    'Is iterative improvement structurally enabled by the baseline as this reading claims, or is the improvement mandate aspirational while base-layer practice ossifies?',
    'Track soft-fork cadence, activation thresholds, and time-to-activation across the interval. If base-layer change becomes practically unreachable and all improvement migrates to upper layers, this reading collapses in practice into the pragmatic synthesis.',
    'If improvement is no longer structurally enabled, the coordination claim narrows to the baseline as-is, the reading''s distinguishing axiom loses operative content, and the story''s classification should be re-derived under the synthesis framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(improvement_mandate_reality, empirical, 'Whether the utility reading''s improvement mandate matches observable upgrade practice or is drifting into aspiration.').

omega_variable(
    holder_exit_suppression_mechanism,
    'Is the immutability_reliant_holders'' inability to exit structural (capital lock-in, custody arrangements, tax exposure) or internalized (identity fusion with the hard-money narrative making sale psychologically unthinkable)?',
    'Post-exit trajectory of holders who do sell: if they re-fuse with a successor hard-capped asset and reproduce the same holding behavior, the suppression is substantially internalized; if they reallocate freely, it was structural.',
    'If internalized, the payer seat carries the suppression with them after exit and the measured suppression overstates structural coercion; the split between structural and internalized shares feeds the suppression-mechanism ambiguity for this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(holder_exit_suppression_mechanism, empirical, 'Structural versus internalized suppression mechanism for the identity-locked holder seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__utility_reading, 2009, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t2009, bitcoin_consensus_kernel__utility_reading, theater_ratio, 2009, 0.04).
narrative_ontology:measurement(bitc_tr_t2012, bitcoin_consensus_kernel__utility_reading, theater_ratio, 2012, 0.07).
narrative_ontology:measurement(bitc_tr_t2015, bitcoin_consensus_kernel__utility_reading, theater_ratio, 2015, 0.11).
narrative_ontology:measurement(bitc_tr_t2017, bitcoin_consensus_kernel__utility_reading, theater_ratio, 2017, 0.22).
narrative_ontology:measurement(bitc_tr_t2019, bitcoin_consensus_kernel__utility_reading, theater_ratio, 2019, 0.18).
narrative_ontology:measurement(bitc_tr_t2021, bitcoin_consensus_kernel__utility_reading, theater_ratio, 2021, 0.14).
narrative_ontology:measurement(bitc_tr_t2023, bitcoin_consensus_kernel__utility_reading, theater_ratio, 2023, 0.15).
narrative_ontology:measurement(bitc_tr_t2026, bitcoin_consensus_kernel__utility_reading, theater_ratio, 2026, 0.15).

% Extraction over time
narrative_ontology:measurement(bitc_be_t2009, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 2009, 0.12).
narrative_ontology:measurement(bitc_be_t2012, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 2012, 0.22).
narrative_ontology:measurement(bitc_be_t2015, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 2015, 0.33).
narrative_ontology:measurement(bitc_be_t2017, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 2017, 0.48).
narrative_ontology:measurement(bitc_be_t2019, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 2019, 0.45).
narrative_ontology:measurement(bitc_be_t2021, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 2021, 0.43).
narrative_ontology:measurement(bitc_be_t2023, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 2023, 0.47).
narrative_ontology:measurement(bitc_be_t2026, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 2026, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t2009, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 2009, 0.05).
narrative_ontology:measurement(bitc_su_t2012, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 2012, 0.1).
narrative_ontology:measurement(bitc_su_t2015, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 2015, 0.28).
narrative_ontology:measurement(bitc_su_t2017, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 2017, 0.55).
narrative_ontology:measurement(bitc_su_t2019, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 2019, 0.46).
narrative_ontology:measurement(bitc_su_t2021, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 2021, 0.4).
narrative_ontology:measurement(bitc_su_t2023, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 2023, 0.41).
narrative_ontology:measurement(bitc_su_t2026, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 2026, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__utility_reading, resource_allocation).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel__maximalist_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel__pragmatic_synthesis).

% DUAL FORMULATION NOTE:
% The colloquial label 'Bitcoin's consensus rules' covers three structurally distinct constraints corresponding to three readings of one kernel (bitcoin_consensus_kernel). The maximalist reading instantiates an immutability covenant (high suppression, holders as beneficiaries, upgraders as violators); the pragmatic synthesis instantiates a two-layer arrangement (immutable base, free upper layers); this story instantiates the utility reading (revisable minimum viable baseline). Per the epsilon-invariance principle, each reading is a separate file with its own epsilon, beneficiary/victim structure, and claimed type; this story's epsilon is assessed on the standing arrangement — the consensus baseline as it operates — by the utility reading's own lights. The upstream/downstream pressure runs from this reading's activated soft forks toward the synthesis's kernel-boundary drawings, and the maximalist reading exerts repudiation pressure on both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_consensus_kernel__utility_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
