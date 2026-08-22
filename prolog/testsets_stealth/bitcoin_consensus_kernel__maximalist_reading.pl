% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Whitepaper-as-Immutable-Covenant Constraint (Maximalist Reading)
 *   domain: cryptoeconomics/monetary systems/distributed consensus
 *
 * SUMMARY:
 *   This story instantiates the maximalist reading of the bitcoin consensus
 *   kernel: the whitepaper and its embedded issuance schedule constitute a
 *   binding founding covenant, and any protocol change that alters monetary
 *   policy violates it. Operatively, the constraint is the social and
 *   technical enforcement arrangement that makes the covenant stick —
 *   economic-majority veto over forks, full-node validation refusal, hashrate
 *   signaling, and a narrative apparatus that frames change advocacy as
 *   infidelity. It solves a real coordination problem (expectation
 *   convergence on fixed monetary rules without a governance committee) while
 *   asymmetrically extracting from those who would change or build upon the
 *   base layer: scalability developers, would-be innovators, and high-volume
 *   transactors bear opportunity costs and fee-market pressure, while the
 *   resulting scarcity premium concentrates in holders. Per the
 *   epsilon-referent rule, epsilon is authored for the standing arrangement
 *   under contest — the immutability-enforcement regime itself — not for any
 *   alternative arrangement the reading might endorse. The sibling readings
 *   (utility_reading, pragmatic_synthesis) are separate constraints in
 *   separate files; this file does not average over them.
 *
 * KEY AGENTS:
 *   - long_term_holders: Primary beneficiary (organized/identity_locked) — collect the scarcity premium the covenant guarantees; exit means abandoning the thesis and realizing taxable gains
 *   - early_adopter_accumulators: Concentrated beneficiary (powerful/arbitrage) — largest historical gains; can diversify out at will, which decouples their advocacy from their exposure
 *   - institutional_asset_managers: Secondary beneficiary (institutional/arbitrage) — market immutability as a product feature ('digital gold') post-ETF era
 *   - mining_pool_operators: Agenda-setter with secondary beneficiary position (institutional/constrained) — enforce via hashrate signaling, earn block rewards and fees under deliberately constrained block space, but bear stranded-capacity risk
 *   - full_node_operators: Agenda-setter (organized/identity_locked) — the validation veto; running a node is frequently an ideological practice fused with the covenant
 *   - core_protocol_developers: Primary payer (moderate/identity_locked) — propose and maintain consensus code; change advocates meet social enforcement, funding denial, and reputational cost
 *   - layer2_scalability_builders: Payer (moderate/constrained) — must route around base-layer rigidity and contend with delegitimization of even second-layer innovation
 *   - on_chain_transactors: Payer (powerless/constrained) — bear fee-market pressure from artificially scarce block space; individually without leverage
 *   - altcoin_ecosystem_participants: Excluded (moderate/mobile) — locked out of the legitimacy conversation by construction, pre-labeled as unfaithful
 *   - cryptoeconomic_analysts: Analytical observer (analytical/analytical) — see the full structure from outside the covenant frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__maximalist_reading, 0.72).
domain_priors:suppression_score(bitcoin_consensus_kernel__maximalist_reading, 0.56).
domain_priors:theater_ratio(bitcoin_consensus_kernel__maximalist_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0.56).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__maximalist_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_consensus_kernel__maximalist_reading, "Whitepaper-as-Immutable-Covenant Constraint (Maximalist Reading)").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__maximalist_reading, "cryptoeconomics/monetary systems/distributed consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__maximalist_reading, '956fc4d6-f325-41aa-bdea-dca9f92501cc').
narrative_ontology:cs_kernel_codification('956fc4d6-f325-41aa-bdea-dca9f92501cc', fixed_text).
narrative_ontology:cs_authority_grounding('956fc4d6-f325-41aa-bdea-dca9f92501cc', lineage).
narrative_ontology:cs_interpretation_layer_present('956fc4d6-f325-41aa-bdea-dca9f92501cc').
narrative_ontology:cs_reading_relation('956fc4d6-f325-41aa-bdea-dca9f92501cc', bitcoin_consensus_kernel__utility_reading, forecloses).
narrative_ontology:cs_reading_relation('956fc4d6-f325-41aa-bdea-dca9f92501cc', bitcoin_consensus_kernel__pragmatic_synthesis, coexists_with).
narrative_ontology:cs_axiom('956fc4d6-f325-41aa-bdea-dca9f92501cc', foundational, monetary_policy_immutability_supreme).
narrative_ontology:cs_axiom_status(monetary_policy_immutability_supreme, holdable).
narrative_ontology:cs_axiom_grounding('956fc4d6-f325-41aa-bdea-dca9f92501cc', monetary_policy_immutability_supreme, deontological).
narrative_ontology:cs_axiom('956fc4d6-f325-41aa-bdea-dca9f92501cc', foundational, whitepaper_constitutive_fidelity).
narrative_ontology:cs_axiom_status(whitepaper_constitutive_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('956fc4d6-f325-41aa-bdea-dca9f92501cc', whitepaper_constitutive_fidelity, conventional).
narrative_ontology:cs_reference_frame('956fc4d6-f325-41aa-bdea-dca9f92501cc', whitepaper_immutable_monetary_covenant).
narrative_ontology:cs_drift_state('956fc4d6-f325-41aa-bdea-dca9f92501cc', contemporary_etf_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('956fc4d6-f325-41aa-bdea-dca9f92501cc', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, long_term_holders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, early_adopter_accumulators).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, institutional_asset_managers).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, mining_pool_operators).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, core_protocol_developers).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, layer2_scalability_builders).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, on_chain_transactors).
narrative_ontology:constraint_vindicates(bitcoin_consensus_kernel__maximalist_reading, nakamoto_conservatism).
narrative_ontology:constraint_vindicates(bitcoin_consensus_kernel__maximalist_reading, digital_scarcity_premium_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the asset across cycles as a savings technology. The covenant guarantees the supply schedule their purchasing-power thesis depends on; every blocked change that preserves scarcity flows to them as premium. Selling means abandoning the thesis, realizing taxes, and exiting a community whose identity they share — exit is nominally open and practically fused.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, long_term_holders, beneficiary,
    organized, generational, identity_locked, global).

% Accumulated at negligible cost in 2010-2013 and hold concentrated stakes. They captured the largest historical gains from enforced scarcity and retain outsized influence over the economic majority that vetoes forks. Unlike later holders they can diversify out at will, so their advocacy for immutability is decoupled from their personal exposure.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, early_adopter_accumulators, beneficiary,
    powerful, generational, arbitrage, global).

% Issue spot vehicles and treasury products whose pitch rests on the fixed-supply guarantee. They market immutability as a feature, amplify the covenant narrative to clients, and can redeploy capital to other assets without cost to themselves if the frame breaks.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, institutional_asset_managers, beneficiary,
    institutional, biographical, arbitrage, global).

% Operate hashrate that signals for or against rule changes and earn block rewards plus fees under deliberately constrained block space. ASIC capital is sunk into this specific rule set, so they enforce the covenant that protects their equipment's usefulness, while carrying the risk that capped blocks strand capacity they built for growth.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, mining_pool_operators, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__maximalist_reading, mining_pool_operators, beneficiary).

% Run validating software that refuses any chain violating the rules they compiled. Individually cost-bearing and collectively decisive: no rule change survives their refusal to follow it. For many, node operation is an ideological practice — verification as membership — making their enforcement posture constitutive rather than chosen per-issue.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, full_node_operators, agenda_setter,
    organized, generational, identity_locked, global).

% Maintain consensus code and propose improvements. Proposals touching monetary parameters die socially before they die technically: review culture, funding access, and forum standing all route through covenant fidelity. Their reputations and career histories are fused with the project, so advocating change risks the professional identity the constraint surrounds; some have exited to rival chains and been written out of the lineage.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, core_protocol_developers, payer,
    moderate, biographical, identity_locked, global).

% Build routing networks, sidechains, and settlement layers on top of a base layer they cannot modify. Every design must fit through the narrow aperture the covenant leaves; even second-layer innovations (drivechains, new covenant opcodes) meet delegitimization campaigns. Their capital and user bases are invested in this ecosystem, so exit means writing off franchise value.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, layer2_scalability_builders, payer,
    moderate, biographical, constrained, global).

% Need block space for payments, settlements, and data. Deliberately scarce capacity means auction-priced fees that spike under demand; they have no vote, no fork leverage, and no organizational voice. Alternatives exist on other chains but carry liquidity and counterparty costs that bind smaller users hardest.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, on_chain_transactors, payer,
    powerless, immediate, constrained, global).

% Build and use rival monetary experiments, including forks of this very system. The covenant frame defines them as unfaithful by construction, so they are outside the legitimacy conversation regardless of technical merit; their exclusion is what the boundary-maintenance machinery produces and defends.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, altcoin_ecosystem_participants, excluded,
    moderate, biographical, mobile, global).

% Study incentive structures, security budgets, and governance dynamics from outside the covenant frame. They can name the asymmetry between coordination function and holder-capture without bearing either side's exposure, and their analyses feed regulatory and academic audiences rather than the enforcement process itself.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, cryptoeconomic_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_consensus_kernel__maximalist_reading, long_term_holders).
narrative_ontology:fixing_cost_class(bitcoin_consensus_kernel__maximalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converges the expectations of millions of anonymous participants on a single fixed monetary rule set: no committee decides issuance, no proposal can quietly alter the supply schedule, and every validator independently enforces the same frozen rules. Secondarily it maintains the boundary of 'what counts as Bitcoin' against proposed modifications.
% TRANSFER_FUNCTION: Moves scarcity premium and preserved optionality to holders of the existing coin, funded by opportunity costs imposed on would-be changers, scalability builders, and high-volume transactors; under capped block space it additionally routes fee revenue to miners at clearing prices set by enforced scarcity.
% ABSENT_VOICES: Users priced out by fee spikes, builders whose designs require base-layer flexibility, and low-margin remittance corridors are present only as rhetorical figures ('banking the unbanked') inside maximalist discourse — invoked to justify the system, never seated to contest it. Their objections would target the equation of monetary purity with network health.
% DISAPPEARANCE_RATIONALE: If the covenant and its enforcement vanished overnight, monetary expectations would lose their anchor: holders would reprice governance risk, competing rule-change proposals would flood forward simultaneously, and the network would likely fragment through repeated contentious forks until a new Schelling-point rule set emerged — or failed to. Custody products, miner revenue models, and developer norms are all arranged around the freeze and would reorganize.
% FOUNDING_PROBLEM: Nakamoto's problem: enabling peer-to-peer electronic cash without a trusted third party, solving double-spending with proof-of-work and removing discretionary monetary management via a fixed issuance schedule. The maximalist reading elevates the second element — the fixed schedule — into the supreme, inviolable commitment.
% FOUNDING_PROBLEM_CORROBORATION: The underlying trustless-consensus problem and its proof-of-work solution are corroborated outside the beneficiary set by the distributed-systems literature (Byzantine agreement, Sybil resistance) and by the cypherpunk archival record of pre-Bitcoin digital-cash attempts. However, no party outside the holder community corroborates the stronger maximalist gloss — that immutability of monetary policy is the founding commitment's supreme term — and the whitepaper's own title and body foreground electronic cash over monetary finality; that elevation is attested almost exclusively from within the benefiting seats, which is itself signal.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__maximalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__maximalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__maximalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.72) because the covenant's benefits (scarcity premium, monetary credibility) accrue pro-rata to holders while its costs (foregone throughput, blocked innovation, fee pressure) fall on non-holding participants and builders — a structurally asymmetric ledger that widened as institutional adoption converted immutability into a marketed product feature. Suppression (0.56, down from its 2017 peak of 0.66) reflects the enforcement machinery: the 2015-2017 block-size war featured forum moderation, funding denial, and DDoS campaigns against change advocates; after opponents forked away, overt enforcement relaxed but persists at elevated levels (inscription-filtering disputes, drivechain rejection). Suppression is authored as a raw structural property — the engine scales only extractiveness by directionality and scope. Theater ratio (0.34) captures the growing share of activity that is ritual boundary maintenance (purity tests, anniversary liturgy, 'not your keys' recitation) relative to functional consensus work; it rose steadily as the community institutionalized. Accessibility collapse is moderate (0.45): forks and rival chains exist and are usable, but within the covenant frame alternatives are narratively collapsed into 'scams,' so perceived option space shrinks faster than actual option space. Resistance is substantial (0.6): the constraint has repeatedly been actively contested (the 2017 fork war, ordinals controversy, recurring covenant/drivechain proposals) and has survived each contest, which is itself evidence of enforcement dependence. All three tracked metric series run on one shared time grid (2009-2026, eight points) so no metric row borrows another's end-state values.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute divergently by construction. From the agenda-setter seats (node operators, miners), the arrangement is the successful defense of a sacred commitment — the thing that makes the asset what it is; their identity fusion makes the covenant constitutive rather than constraining. From the beneficiary seats, it is a guarantee they paid for by holding through volatility. From the payer seats, the same structure operates as enforced stagnation: developers watch proposals die socially rather than technically, builders absorb the cost of routing around rigidity, and transactors pay clearing prices set by deliberate scarcity. The excluded seat never enters the computation at all — its exclusion is the enforcement object. The engine derives these divergent classifications from the structural data; the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place holders near the subsidized end: long-term holders (identity-locked, generational horizon) sit nearest d=0 since the covenant is their balance sheet; early adopters are pulled slightly toward symmetric by arbitrage-grade exit — they capture gains but can leave, so their effective exposure to the constraint's continuation is partial. Institutional managers similarly benefit while remaining mobile. Mining pool operators are dual-positioned: agenda-setter enforcement plus beneficiary revenue, offset by stranded-capacity costs — a mid-range d the derivation approximates from their secondary beneficiary role. Victims carry high d: developers and builders are identity-locked or constrained (trapped by reputation and sunk ecosystem investment), amplifying their effective extraction; transactors are powerless and constrained. Scope is global throughout, which modestly amplifies effective extraction for target seats since verification of enforcement uniformity is hard at planetary scale.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim guards against two opposite mislabels. Reading the arrangement as pure snare would erase its genuine coordination function: a fixed issuance schedule really does solve the expectation-convergence problem for millions of strangers running identical validation software with no committee, and the 2017 episode demonstrated the machinery resolves chain-identity crises (however brutally). Reading it as pure rope would erase the asymmetry: the coordination story is also the cover under which holder interests veto ecosystem evolution, and the enforcement apparatus exists because voluntary consent alone would not hold the freeze. Mandatrophy is not yet resolved — the founding problem (trustless consensus without discretionary monetary management) remains live, so this is not a piton candidate; the mismatch consumer should find status=live paired with verdict=world_rearranges, no zombie flag. The forward risk runs the other direction: if the security-budget omega resolves unsustainably, the covenant acquires a forced-revision trigger and the arrangement begins transitioning toward scaffold-like renegotiation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the bitcoin_consensus_kernel; how would classification shift under the sibling readings (utility_reading, pragmatic_synthesis)?',
    'Generate the sibling stories and compare computed types: utility_reading treats the whitepaper as a minimum viable mechanism enabling iteration (likely dissolving the victim classes into participants in legitimate improvement); pragmatic_synthesis confines immutability to the base layer and relocates extraction to whatever blocks upper-layer innovation.',
    'Under utility_reading the same arrangement likely computes as rope or scaffold (iteration is the designed mode, so ''blocked change'' is not extraction); under pragmatic_synthesis it stays tangled_rope but with a narrower victim set. The high-extraction profile is specific to the maximalist instantiation, not the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Indexical dependence of classification on kernel reading selection.').

omega_variable(
    covenant_status_of_whitepaper,
    'Is the whitepaper a binding founding covenant whose violation is illegitimate, or an engineering memorandum whose specifics were always provisional?',
    'None fully available: the text is ambiguous between constitutional and design-document registers, and the dispute is constitutive of the community rather than resolvable by closer reading. Observe whether an externally imposed shock (regulatory mandate, security-budget crisis) ever forces an explicit interpretive ruling.',
    'If memorandum, the constraint loses its legitimacy ground and its persistence becomes pure interest-group enforcement — the classification drifts from tangled_rope toward snare. If covenant, part of the measured extraction is the price of the credible commitment itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covenant_status_of_whitepaper, conceptual, 'Textual-status ambiguity of the founding document: covenant versus design memo.').

omega_variable(
    soft_fork_tolerance_boundary,
    'Does the covenant bind all consensus rules equally, or only the monetary policy (supply cap, issuance schedule)? Taproot activated in 2021 while drivechains and covenants were rejected.',
    'Observe the outcome distribution of future soft-fork proposals: classify each as monetary-touching or not, and track activation success against maximalist endorsement.',
    'If only monetary policy is sacrosanct, the constraint''s extraction targets innovation broadly and the victim set widens; if all rules are frozen in principle with pragmatic exceptions, the enforcement is selective and the theater component is larger than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(soft_fork_tolerance_boundary, empirical, 'Boundary of the immutability commitment: monetary policy only versus all consensus rules.').

omega_variable(
    internalized_vs_enforced_conformity,
    'Is conformist pressure within the maximalist community structural (economic-majority veto, forum moderation, funding denial) or internalized (identity fusion of holders and developers with the covenant)?',
    'Post-exit trajectory study of departed developers and builders: those who left for other ecosystems (e.g., after the 2017 split) and reported persistent self-censorship habits or continued deference to the covenant frame carry internalized suppression; those who immediately advocated change elsewhere carried only structural suppression.',
    'If substantially internalized, effective suppression exceeds the structural measure — targets carry the constraint with them after exit, and the identity_locked exit coding of the developer seat is confirmed rather than merely declared.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_enforced_conformity, empirical, 'Structural versus internalized mechanism of maximalist conformity.').

omega_variable(
    security_budget_longevity,
    'Can the fee market under a permanently capped block space sustain miner security budgets as block subsidies decline, or will the covenant eventually face a forced reinterpretation?',
    'Track fee-to-subsidy ratio trajectories and hashprice economics across successive halvings; model equilibrium fee levels required at subsidy exhaustion.',
    'If budgets are unsustainable, the covenant contains a deferred self-revision trigger and the constraint carries a hidden sunset dynamic; if sustainable, the immutability commitment is durable and the extraction profile is stable rather than transitional.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(security_budget_longevity, empirical, 'Whether the covenant is economically durable or contains a forced-revision trigger.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__maximalist_reading, 2009, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bck_maximalist_tr_t2009, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 2009, 0.06).
narrative_ontology:measurement(bck_maximalist_tr_t2011, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 2011, 0.09).
narrative_ontology:measurement(bck_maximalist_tr_t2013, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 2013, 0.13).
narrative_ontology:measurement(bck_maximalist_tr_t2015, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 2015, 0.19).
narrative_ontology:measurement(bck_maximalist_tr_t2017, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 2017, 0.27).
narrative_ontology:measurement(bck_maximalist_tr_t2020, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 2020, 0.3).
narrative_ontology:measurement(bck_maximalist_tr_t2023, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 2023, 0.33).
narrative_ontology:measurement(bck_maximalist_tr_t2026, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 2026, 0.34).

% Extraction over time
narrative_ontology:measurement(bck_maximalist_be_t2009, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 2009, 0.22).
narrative_ontology:measurement(bck_maximalist_be_t2011, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 2011, 0.3).
narrative_ontology:measurement(bck_maximalist_be_t2013, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 2013, 0.38).
narrative_ontology:measurement(bck_maximalist_be_t2015, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 2015, 0.52).
narrative_ontology:measurement(bck_maximalist_be_t2017, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 2017, 0.63).
narrative_ontology:measurement(bck_maximalist_be_t2020, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 2020, 0.67).
narrative_ontology:measurement(bck_maximalist_be_t2023, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 2023, 0.7).
narrative_ontology:measurement(bck_maximalist_be_t2026, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 2026, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(bck_maximalist_su_t2009, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 2009, 0.12).
narrative_ontology:measurement(bck_maximalist_su_t2011, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 2011, 0.18).
narrative_ontology:measurement(bck_maximalist_su_t2013, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 2013, 0.26).
narrative_ontology:measurement(bck_maximalist_su_t2015, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 2015, 0.5).
narrative_ontology:measurement(bck_maximalist_su_t2017, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 2017, 0.66).
narrative_ontology:measurement(bck_maximalist_su_t2020, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 2020, 0.62).
narrative_ontology:measurement(bck_maximalist_su_t2023, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 2023, 0.59).
narrative_ontology:measurement(bck_maximalist_su_t2026, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 2026, 0.56).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__maximalist_reading, identity_coordination).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, utility_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, pragmatic_synthesis).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Bitcoin's monetary policy' decomposes into three kernel readings with distinct epsilon values and victim sets. This member (maximalist_reading) carries the highest extraction profile because it freezes the entire base layer and casts change itself as violation. utility_reading sits upstream in legitimacy terms (its framing cites the whitepaper's own 'Peer-to-Peer Electronic Cash System' title and iterative design register), and its existence pressures this reading by supplying covenant-faithful counter-interpretation; pragmatic_synthesis mediates, accepting base immutability while draining the victim class. All three stories link one another via affects_constraints; classification comparisons are only valid across files, never averaged within one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
