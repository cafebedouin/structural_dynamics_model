% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__utility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: bitcoin_consensus_kernel__utility_reading
 *   human_readable: Bitcoin Consensus Mechanism as Minimum Viable Improvable Baseline (Utility Reading)
 *   domain: cryptoeconomics/monetary systems/distributed consensus
 *
 * SUMMARY:
 *   The standing arrangement under contest is Bitcoin's consensus mechanism
 *   together with the regime through which its rules change: proof-of-work
 *   chain selection, independent full validation, and a proposal process that
 *   activates rule changes only when node operators, miners, and economic
 *   actors accept them. This file instantiates the utility_reading of the
 *   bitcoin_consensus_kernel, which reads the whitepaper as establishing a
 *   deliberately minimum viable mechanism whose design invites iterative
 *   improvement through backward-compatible rule changes and layered
 *   construction. Per the epsilon-invariance principle, the colloquial label
 *   'Bitcoin's consensus rules' decomposes into three structurally distinct
 *   constraints — this reading's evolving-mechanism arrangement, the
 *   maximalist_reading's immutability covenant, and the pragmatic_synthesis's
 *   two-tier split — linked as a constraint family through
 *   network.affects_constraints. Epsilon is authored for the standing
 *   arrangement as this reading assesses it: moderate, reflecting genuine
 *   coordination whose operation nonetheless concentrates protocol-authority
 *   standing in the contributor community and imposes real adaptation costs
 *   on node operators, miners, and fixity-premised holders. The claimed type
 *   and the metrics are independent authored facts.
 *
 * KEY AGENTS:
 *   - bitcoin_core_contributors: agenda setter (organized/identity_locked) — stewards the reference implementation and proposal process; protocol-authority standing accrues here
 *   - protocol_developers: beneficiary (moderate/mobile) — builds tooling on an improving substrate
 *   - layer2_operators: beneficiary (organized/constrained) — operates channels and bridges; working capital locked inside the arrangement
 *   - application_builders: beneficiary (moderate/mobile) — runs custody and payments on settlement assurances
 *   - new_adopters: beneficiary (powerless/mobile) — receives whatever rules the consensus serves, with no governance seat
 *   - ossification_dependent_holders: primary target (moderate/identity_locked) — bears erosion of the fixity assurance their valuation thesis prices
 *   - full_node_operators: target (moderate/constrained) — bears the upgrade-or-diverge burden of every activation
 *   - miners: dual enforcer and target (organized/constrained) — vetoes activations through hash signaling while bearing hardware and fee-market costs
 *   - exchange_and_custody_operators: institutional beneficiary and payer (institutional/mobile) — grows with the ecosystem, reworks integration at every fork
 *   - maximalist_constituency: excluded voice (organized/identity_locked) — premise dismissed rather than engaged under this reading's terms
 *   - financial_regulators: analytical observer (institutional/analytical) — constrains fiat interfaces, does not set consensus rules
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__utility_reading, 0.48).
domain_priors:suppression_score(bitcoin_consensus_kernel__utility_reading, 0.28).
domain_priors:theater_ratio(bitcoin_consensus_kernel__utility_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__utility_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_consensus_kernel__utility_reading, "Bitcoin Consensus Mechanism as Minimum Viable Improvable Baseline (Utility Reading)").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__utility_reading, "cryptoeconomics/monetary systems/distributed consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__utility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__utility_reading, 'c5f87a33-c56d-4328-97ce-6a6119c4d249').
narrative_ontology:cs_kernel_codification('c5f87a33-c56d-4328-97ce-6a6119c4d249', fixed_text).
narrative_ontology:cs_authority_grounding('c5f87a33-c56d-4328-97ce-6a6119c4d249', expertise).
narrative_ontology:cs_interpretation_layer_present('c5f87a33-c56d-4328-97ce-6a6119c4d249').
narrative_ontology:cs_reading_relation('c5f87a33-c56d-4328-97ce-6a6119c4d249', bitcoin_consensus_kernel__maximalist_reading, forecloses).
narrative_ontology:cs_reading_relation('c5f87a33-c56d-4328-97ce-6a6119c4d249', bitcoin_consensus_kernel__pragmatic_synthesis, influences).
narrative_ontology:cs_axiom('c5f87a33-c56d-4328-97ce-6a6119c4d249', foundational, iterative_improvement_is_design_intent).
narrative_ontology:cs_axiom_status(iterative_improvement_is_design_intent, holdable).
narrative_ontology:cs_axiom_grounding('c5f87a33-c56d-4328-97ce-6a6119c4d249', iterative_improvement_is_design_intent, empirically_contingent).
narrative_ontology:cs_axiom('c5f87a33-c56d-4328-97ce-6a6119c4d249', foundational, soft_fork_channel_preserves_core_guarantees).
narrative_ontology:cs_axiom_status(soft_fork_channel_preserves_core_guarantees, holdable).
narrative_ontology:cs_axiom_grounding('c5f87a33-c56d-4328-97ce-6a6119c4d249', soft_fork_channel_preserves_core_guarantees, instrumental).
narrative_ontology:cs_reference_frame('c5f87a33-c56d-4328-97ce-6a6119c4d249', minimum_viable_improvable_mechanism).
narrative_ontology:cs_drift_state('c5f87a33-c56d-4328-97ce-6a6119c4d249', post_taproot_institutional_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('c5f87a33-c56d-4328-97ce-6a6119c4d249', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, protocol_developers).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, layer2_operators).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, application_builders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, new_adopters).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, ossification_dependent_holders).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, full_node_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, exchange_and_custody_operators).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, miners).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, exchange_and_custody_operators).
narrative_ontology:constraint_vindicates(bitcoin_consensus_kernel__utility_reading, nakamoto_consensus_sufficiency_thesis).
narrative_ontology:constraint_vindicates(bitcoin_consensus_kernel__utility_reading, soft_fork_extensibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the reference implementation, author and shepherd improvement proposals, and convene the review process through which rule changes are activated. Standing, speaking invitations, grant funding, and hiring desirability accrue to recognized contributors. Leaving the project forfeits reputation accumulated around this specific codebase; several former members departed and lost influence overnight.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, bitcoin_core_contributors, agenda_setter,
    organized, generational, identity_locked, global).

% Build wallets, libraries, indexing services, and tooling on top of the protocol. Each accepted improvement expands what they can offer customers; each rejected one redirects their roadmaps. Their skills port to competing chains, so their attachment is professional rather than captive.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, protocol_developers, beneficiary,
    moderate, biographical, mobile, global).

% Operate payment-channel routing, sidechain bridges, and hosted services that settle ultimately to the base layer. Their working capital sits in channels and bridge collateral that retain value only while the base layer's rules hold. They campaign for improvements that increase layered throughput and absorb liquidity-management costs when activity migrates between layers.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, layer2_operators, beneficiary,
    organized, biographical, constrained, global).

% Run custody, payments, and treasury products that depend on the base layer's settlement assurances. Rule activations force integration rework and legal re-review; improvements expand their addressable market. Most operate across multiple assets and can shift emphasis if this network's trajectory disappoints.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, application_builders, beneficiary,
    moderate, biographical, mobile, global).

% Individuals and firms entering the network for savings or payment use. They receive whatever rules the current consensus serves them and have no seat in proposal debates; their practical exit is selling holdings and leaving, which costs little beyond market exposure.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, new_adopters, beneficiary,
    powerless, biographical, mobile, global).

% Hold large positions acquired on the premise that the monetary rules can never change; the durability of that premise is what their valuation thesis prices. Every accepted rule change erodes the assurance they paid for. Selling realizes the thesis loss and abandons a community identity built around fixity, so they stay and fund advocacy for frozen rules instead.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, ossification_dependent_holders, payer,
    moderate, generational, identity_locked, global).

% Run independently validating nodes out of principle, commerce, or hobby. Each activated rule change obliges a software upgrade; falling behind means silently diverging from consensus and rejecting blocks the rest of the network accepts. Individually they have no vote; collectively their acceptance is what makes a change real.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, full_node_operators, payer,
    moderate, biographical, constrained, global).

% Convert electricity into block-proposal rights under the current rules, which gives them a veto over proposed activations through hash-rate signaling. Every change alters their hardware's useful life and reshapes the fee market they depend on. Specialized rigs have no resale use outside this network, so exit means writing off capital.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, miners, payer,
    organized, immediate, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__utility_reading, miners, agenda_setter).

% Operate the fiat on-ramps, off-ramps, and custody that most newcomers touch. Ecosystem growth raises their volumes; each rule activation imposes integration, accounting, and compliance rework across jurisdictions. They are multi-asset businesses and can reallocate attention if this network's trajectory disappoints.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, exchange_and_custody_operators, beneficiary,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__utility_reading, exchange_and_custody_operators, payer).

% Advocates, commentators, and developers who read the founding document as an unbreakable monetary covenant. Under this reading's terms their premise is categorized as illegitimate rather than argued with, so they speak loudly in public channels while holding no standing in the proposal process. Many are also long-term holders whose wealth rides on the same fixity premise.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, maximalist_constituency, excluded,
    organized, generational, identity_locked, global).

% National and supranational authorities drafting custody, anti-money-laundering, and market-integrity rules around the asset. They take testimony, commission analyses, and can constrain the fiat interfaces, but they do not participate in setting the network's rules and watch the governance contest from outside.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, financial_regulators, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_consensus_kernel__utility_reading, bitcoin_core_contributors).
narrative_ontology:fixing_cost_class(bitcoin_consensus_kernel__utility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Orders mutually distrusting parties' transactions into a single append-only ledger without a central issuer: proof-of-work selects the canonical chain, every participant validates independently, and the difficulty adjustment keeps block production steady. The deliberately spare initial rule set kept the mechanism auditable by individuals, which is what allowed strangers to adopt it at all.
% TRANSFER_FUNCTION: Moves settlement assurance and monetary credibility to every participant symmetrically; moves protocol-authority standing — proposal shepherding, reference-implementation stewardship, grant flows — toward the core contributor community; moves adaptation costs (upgrade labor, hardware obsolescence, thesis erosion for fixity-premised holders) onto node operators, miners, and long-term holders whenever the rules change.
% ABSENT_VOICES: The maximalist constituency speaks publicly but holds no standing in the proposal process under this reading's terms — its founding-covenant premise is dismissed rather than engaged. Future holders not yet born have no seat though changes bind them longest. Users in low-connectivity regions who cannot run validating nodes depend on intermediaries and are absent from governance debates entirely.
% DISAPPEARANCE_RATIONALE: If the consensus mechanism vanished overnight, the settlement substrate under the network's stored value disappears: exchanges halt withdrawals, payment channels become unenforceable promises, the mining industry's specialized capital evaporates, and custody products lose the property they sell. Every arrangement built on the ledger rearranges around whatever replacement emerges, at enormous cost.
% FOUNDING_PROBLEM: Trustless peer-to-peer electronic cash: letting parties transact without a trusted intermediary, after earlier centralized digicash efforts collapsed with their issuers and in the aftermath of the 2008 banking failures.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: the cryptographic literature on Byzantine fault tolerance and hashcash that the whitepaper itself cites, over fifteen years of independent security analysis, state regulators' treatment of the network as a persistent settlement system, and the network's survival of documented attack attempts. No external party attests the narrower reading-specific claim — that the text intended a minimum viable baseline inviting iteration — which remains contested between this reading and its siblings; the builder community inside the benefiting set is its primary attestor.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__utility_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__utility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__utility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_consensus_kernel__utility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_consensus_kernel__utility_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored moderate (0.48): the mechanism delivers real trustless settlement to every participant, but its operation concentrates decision standing in the contributor community and levies recurring adaptation costs on validators, miners, and fixity-premised holders. Suppression is moderate-low (0.28): enforcement is social-plus-technical consensus rather than coercive apparatus, and alternatives exist — competing chains, exit to other assets — but the block-size wars showed dissenting implementations facing ostracism and service denial, and fork-off exits retained little value, so exit is realer in theory than in realized practice. Theater ratio 0.22: anniversary liturgy, whitepaper quotations deployed rhetorically, and maximalist-versus-utility combat overlay a mechanism whose core function runs continuously and is verifiable by anyone. Accessibility collapse 0.50: understanding the governance reality collapses the headless-decentralist picture many adopters hold, while multi-chain alternatives remain workable for those who accept smaller network effects. Resistance 0.58: the 2015-2017 block-size conflict, the 2017 chain split, and continuing pushback against each activation show the arrangement meets and absorbs real resistance. Coordination type is declared global_infrastructure: planetary-scale value-transfer coordination carrying the highest inherent complexity cost of the type set. The three temporal series share one eight-point grid spanning 2009-2025 (interval units are years since genesis); the suppression series is authored because enforcement capacity is a traced dynamic here — it built through the wars, peaked at the 2017 settlement, and decayed to a plateau afterward. Claimed type tangled_rope is asserted from structure — genuine coordination plus asymmetric costs plus active enforcement — independently of these metric values.
 *
 * PERSPECTIVAL GAP:
 *   From the contributor and builder seats the arrangement computes as earned coordination: a mechanism they maintain, extended through processes they staff. From the validator, miner, and fixity-holder seats the same arrangement computes as enforced adaptation: rules change over their objection, exit carries heavy losses, and the standing that decides changes accrues elsewhere. Miners straddle — they hold veto power over activations yet bear the costs of the changes they fail to veto. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (protocol developers, layer-2 operators, application builders, new adopters) derive low directionality — the arrangement subsidizes them, and their mobile exits push them further toward the beneficiary end. Declared victims (fixity-premised holders, node operators) derive high directionality; identity lock on the holders and constrained exit on the operators place them near the full-target end. Contributors, as agenda setters collecting standing and grant flow, sit near the beneficiary end. Miners are the deliberate asymmetry: declared payers whose secondary agenda-setting role and veto capacity pull them toward symmetry — the derivation reads both declarations rather than the victim label alone. No explicit directionality overrides are authored: the beneficiary/victim declarations plus exit options produce the right relationships, and the dual-positioned agents carry their second positions through secondary_role rather than through override entries keyed to power atoms, which would sweep unintended seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — trustless settlement without an intermediary — remains live and is solved continuously, so the founding_problem_status x disappearance_verdict pair (live x world_rearranges) raises no zombie flag. The classification discipline matters in both directions here: reading the arrangement as pure rope would erase the real costs borne by validators, miners, and fixity holders and the standing rents accruing to the contributor core; reading it as a snare would erase the genuine coordination delivered to every seat and the absence of a coercive capturer — gains concentrate only in contributor standing, which is why gain_flow names that seat while the broader benefit remains diffuse across the builder economy. Fixing is prohibitive: no seat can freeze or radically open the rules without overcoming hash-power distribution, validator acceptance, and accumulated network effects whose cost dwarfs any single seat's benefit. A piton reading is unavailable: the function is not atrophied and its maintenance is not primarily theatrical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection,
    'Which reading of the bitcoin_consensus_kernel correctly characterizes the whitepaper''s normative content — minimum viable baseline inviting iteration (this file), immutable monetary covenant (maximalist_reading), or frozen-base-plus-innovative-layers (pragmatic_synthesis)?',
    'Textual-historical analysis of the whitepaper and early developer correspondence combined with revealed preference: which changes the maintainer community actually shepherded to activation, and which framings the surviving institutions reward.',
    'Selecting the maximalist reading would reclassify the same standing arrangement as a fixity constraint with holders as its beneficiaries and changers as its violators; selecting the synthesis would split this story into two constraints with different epsilon values. This file''s classification is valid only conditional on this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Kernel-level indexicality: the classified constraint exists only under this reading of the shared text.').

omega_variable(
    stewardship_rent_vs_coordination_cost,
    'Is the standing and resource flow that accrues to recognized core contributors a positional rent extracted through gatekeeping of the proposal process, or a meritocratic coordination cost of maintaining a security-critical codebase?',
    'Compare acceptance rates and review latency for proposals originating inside versus outside the recognized contributor set; trace grant and employment flows around the reference implementation.',
    'Concentrated gatekeeping rents would raise effective extraction on the builder class and push the arrangement toward its snare flank; dispersed meritocratic processing supports the coordination-dominant reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stewardship_rent_vs_coordination_cost, empirical, 'Whether protocol-authority concentration constitutes extraction or necessary maintenance cost.').

omega_variable(
    ossification_premium_materiality,
    'Do long-term holders actually price rule-fixity assurance such that accepted changes impose real losses on them, or is holder value driven by adoption dynamics that make the eroded guarantee financially nominal?',
    'Event studies around past rule-change activations measuring holder-class selling behavior, valuation-premium shifts, and survey evidence on stated holding motives.',
    'If the fixity premium is nominal, the victim declaration weakens and the arrangement drifts toward pure coordination; if material, the asymmetric-cost leg of the classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ossification_premium_materiality, empirical, 'Materiality of the fixity-assurance loss borne by long-term holders.').

omega_variable(
    dissenter_exit_realism,
    'Can dissenting factions realistically exit with their value by forking off, or does network-effect gravity make exit illusory enough that staying and complying is effectively coerced?',
    'Post-split value retention: what fraction of the original asset''s value and economic activity the largest exit fork retained over subsequent years.',
    'Illusory exit means effective suppression exceeds the structural measure and persistence depends more heavily on enforcement than the scalar suggests; realistic exit would lower measured suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dissenter_exit_realism, empirical, 'Whether fork-off exit is a real alternative or network gravity forecloses it.').

omega_variable(
    cs_authority_framing_ambiguity,
    'Is the authority that adjudicates legitimate protocol change grounded in demonstrated technical competence (expertise framing, adopted here) or in the practicing community''s own activity constituting the standard (practice framing)?',
    'Examine whether proposal legitimacy tracks demonstrated competence credentials (reviewed security analysis, formal verification) or participation standing in practitioner channels; both framings fit the observable process.',
    'Under the practice framing the interpretive layer is the community''s doing rather than a credentialed reviewer body, which changes drift attribution and could alter commitment-system classification signals without changing the beneficiary/victim structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_authority_framing_ambiguity, conceptual, 'Two coherent framings of the same adjudicating authority with divergent classification signals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__utility_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bck_utility_reading_tr_t0, bitcoin_consensus_kernel__utility_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(bck_utility_reading_tr_t0, observed).
narrative_ontology:measurement(bck_utility_reading_tr_t3, bitcoin_consensus_kernel__utility_reading, theater_ratio, 3, 0.08).
narrative_ontology:measurement_basis(bck_utility_reading_tr_t3, observed).
narrative_ontology:measurement(bck_utility_reading_tr_t6, bitcoin_consensus_kernel__utility_reading, theater_ratio, 6, 0.12).
narrative_ontology:measurement_basis(bck_utility_reading_tr_t6, observed).
narrative_ontology:measurement(bck_utility_reading_tr_t8, bitcoin_consensus_kernel__utility_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement_basis(bck_utility_reading_tr_t8, observed).
narrative_ontology:measurement(bck_utility_reading_tr_t10, bitcoin_consensus_kernel__utility_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement_basis(bck_utility_reading_tr_t10, observed).
narrative_ontology:measurement(bck_utility_reading_tr_t12, bitcoin_consensus_kernel__utility_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement_basis(bck_utility_reading_tr_t12, observed).
narrative_ontology:measurement(bck_utility_reading_tr_t14, bitcoin_consensus_kernel__utility_reading, theater_ratio, 14, 0.22).
narrative_ontology:measurement_basis(bck_utility_reading_tr_t14, observed).
narrative_ontology:measurement(bck_utility_reading_tr_t16, bitcoin_consensus_kernel__utility_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement_basis(bck_utility_reading_tr_t16, observed).

% Extraction over time
narrative_ontology:measurement(bck_utility_reading_be_t0, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(bck_utility_reading_be_t0, observed).
narrative_ontology:measurement(bck_utility_reading_be_t3, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 3, 0.32).
narrative_ontology:measurement_basis(bck_utility_reading_be_t3, observed).
narrative_ontology:measurement(bck_utility_reading_be_t6, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 6, 0.45).
narrative_ontology:measurement_basis(bck_utility_reading_be_t6, observed).
narrative_ontology:measurement(bck_utility_reading_be_t8, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement_basis(bck_utility_reading_be_t8, observed).
narrative_ontology:measurement(bck_utility_reading_be_t10, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(bck_utility_reading_be_t10, observed).
narrative_ontology:measurement(bck_utility_reading_be_t12, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement_basis(bck_utility_reading_be_t12, observed).
narrative_ontology:measurement(bck_utility_reading_be_t14, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 14, 0.49).
narrative_ontology:measurement_basis(bck_utility_reading_be_t14, observed).
narrative_ontology:measurement(bck_utility_reading_be_t16, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 16, 0.48).
narrative_ontology:measurement_basis(bck_utility_reading_be_t16, observed).

% Suppression requirement over time
narrative_ontology:measurement(bck_utility_reading_su_t0, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(bck_utility_reading_su_t0, observed).
narrative_ontology:measurement(bck_utility_reading_su_t3, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 3, 0.12).
narrative_ontology:measurement_basis(bck_utility_reading_su_t3, observed).
narrative_ontology:measurement(bck_utility_reading_su_t6, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 6, 0.3).
narrative_ontology:measurement_basis(bck_utility_reading_su_t6, observed).
narrative_ontology:measurement(bck_utility_reading_su_t8, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement_basis(bck_utility_reading_su_t8, observed).
narrative_ontology:measurement(bck_utility_reading_su_t10, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 10, 0.34).
narrative_ontology:measurement_basis(bck_utility_reading_su_t10, observed).
narrative_ontology:measurement(bck_utility_reading_su_t12, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 12, 0.31).
narrative_ontology:measurement_basis(bck_utility_reading_su_t12, observed).
narrative_ontology:measurement(bck_utility_reading_su_t14, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 14, 0.29).
narrative_ontology:measurement_basis(bck_utility_reading_su_t14, observed).
narrative_ontology:measurement(bck_utility_reading_su_t16, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 16, 0.28).
narrative_ontology:measurement_basis(bck_utility_reading_su_t16, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__utility_reading, global_infrastructure).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel__maximalist_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel__pragmatic_synthesis).

% DUAL FORMULATION NOTE:
% Constraint family: bitcoin_consensus_kernel decomposes by reading into three epsilon-distinct stories. This file (utility_reading) classifies the evolving-mechanism arrangement at moderate epsilon; bitcoin_consensus_kernel__maximalist_reading classifies the immutability covenant (near-zero extraction from the fixity seat, false-summit candidate via holder beneficiaries); bitcoin_consensus_kernel__pragmatic_synthesis splits base-freeze from layer-innovation into two linked constraints. The whitepaper text is the shared kernel; each reading instantiates different beneficiary/victim sets over the same standing arrangement, so the stories are linked, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
