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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Bitcoin Protocol Ossification Norm
 *   domain: cryptocurrency_economics/monetary_systems/technology_governance
 *
 * SUMMARY:
 *   This constraint story models the protocol_ossification_reading of the
 *   bitcoin_whitepaper kernel: the governance norm that Bitcoin base-layer
 *   protocol changes are illegitimate unless they achieve near-universal
 *   consensus, and that stability is the system's primary virtue. This
 *   reading structurally blocks base-layer innovation, routes development to
 *   higher layers, and creates a victim set of use-cases requiring base
 *   protocol changes. It is authored as a tangled_rope because the norm
 *   carries a genuine coordination function (preventing capture and
 *   preserving monetary predictability) while simultaneously extracting from
 *   innovators and users who need features the base layer refuses to adopt.
 *
 * KEY AGENTS:
 *   - node_operators: Primary agenda_setter (organized/identity_locked) â enforce ossification by refusing to run upgraded software
 *   - bitcoin_core_maintainers: Secondary agenda_setter (organized/mobile) â gate-keep the reference client under the ossification norm
 *   - layer_two_operators: Primary beneficiary (moderate/mobile) â capture innovation routed away from base layer
 *   - institutional_holders: Secondary beneficiary (powerful/mobile) â benefit from reduced tail risk and predictability
 *   - base_layer_innovators: Primary payer (moderate/constrained) â bear the cost of blocked upgrades
 *   - users_needing_base_features: Secondary payer (powerless/constrained) â excluded from features available on other chains
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__protocol_ossification_reading, 0.65).
domain_priors:suppression_score(bitcoin_whitepaper__protocol_ossification_reading, 0.78).
domain_priors:theater_ratio(bitcoin_whitepaper__protocol_ossification_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__protocol_ossification_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__protocol_ossification_reading, "Bitcoin Protocol Ossification Norm").
narrative_ontology:topic_domain(bitcoin_whitepaper__protocol_ossification_reading, "cryptocurrency_economics/monetary_systems/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__protocol_ossification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__protocol_ossification_reading, 'ddd94bb1-b6d2-4ffa-897b-7490432b9312').
narrative_ontology:cs_kernel_codification('ddd94bb1-b6d2-4ffa-897b-7490432b9312', fixed_text).
narrative_ontology:cs_authority_grounding('ddd94bb1-b6d2-4ffa-897b-7490432b9312', lineage).
narrative_ontology:cs_interpretation_layer_present('ddd94bb1-b6d2-4ffa-897b-7490432b9312').
narrative_ontology:cs_reading_relation('ddd94bb1-b6d2-4ffa-897b-7490432b9312', bitcoin_whitepaper__p2p_cash_reading, influences).
narrative_ontology:cs_reading_relation('ddd94bb1-b6d2-4ffa-897b-7490432b9312', bitcoin_whitepaper__digital_gold_reading, coexists_with).
narrative_ontology:cs_axiom('ddd94bb1-b6d2-4ffa-897b-7490432b9312', foundational, protocol_stability_as_primary_virtue).
narrative_ontology:cs_axiom_status(protocol_stability_as_primary_virtue, holdable).
narrative_ontology:cs_axiom_grounding('ddd94bb1-b6d2-4ffa-897b-7490432b9312', protocol_stability_as_primary_virtue, conventional).
narrative_ontology:cs_axiom('ddd94bb1-b6d2-4ffa-897b-7490432b9312', foundational, universal_consensus_requirement).
narrative_ontology:cs_axiom_status(universal_consensus_requirement, holdable).
narrative_ontology:cs_axiom_grounding('ddd94bb1-b6d2-4ffa-897b-7490432b9312', universal_consensus_requirement, conventional).
narrative_ontology:cs_reference_frame('ddd94bb1-b6d2-4ffa-897b-7490432b9312', ossified_base_layer_protocol).
narrative_ontology:cs_drift_state('ddd94bb1-b6d2-4ffa-897b-7490432b9312', contemporary_scaling_debates, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('ddd94bb1-b6d2-4ffa-897b-7490432b9312', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, layer_two_operators).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, institutional_holders).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, base_layer_innovators).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, users_needing_base_features).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run full-node software validating the entire Bitcoin chain; enforce ossification by refusing to adopt protocol upgrades that lack near-universal signaling consent. Their operational choice directly determines which consensus rules are live, and their identity is fused with the 'don't trust, verify' ethos that treats any base-layer change as suspect.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, node_operators, agenda_setter,
    organized, generational, identity_locked, global).

% Maintain the reference client implementation that most nodes run. Under the ossification norm they act as procedural gatekeepers: merging only non-controversial, backward-compatible changes and deferring to node-operator signaling. Their authority is reputational and technical rather than formal; they can exit to other projects but are identity-linked to Bitcoin's engineering culture.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_core_maintainers, agenda_setter,
    organized, generational, mobile, global).

% Build payment channels, sidechains, and rollup-like protocols on top of Bitcoin. Benefit from base-layer ossification because it prevents base-protocol competition, preserves the block-space scarcity that creates fee revenue, and drives user demand to L2 solutions for features the base layer will not implement.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, layer_two_operators, beneficiary,
    moderate, biographical, mobile, global).

% Hold Bitcoin as a treasury or inflation-hedge asset. Value protocol stability because it reduces tail risk from contentious hard forks, preserves the fixed monetary policy narrative, and supports the 'digital gold' framing that attracts institutional capital. Their holdings are large enough to influence discourse but do not directly set consensus rules.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, institutional_holders, beneficiary,
    powerful, generational, mobile, global).

% Propose and develop protocol upgrades such as new opcodes, drivechains, or block-size adjustments. Their proposals are systematically stalled by the near-universal consensus requirement and are delegitimized as 'contentious' regardless of technical merit. Exit is constrained because leaving for another chain sacrifices Bitcoin's liquidity and network effects.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, base_layer_innovators, payer,
    moderate, biographical, constrained, global).

% Require advanced scripting, privacy enhancements, or low-fee on-chain throughput that only base-layer changes can provide. Forced to accept higher-cost workarounds, custodial L2 solutions, or migrate to alternative chains with weaker security guarantees. They lack organizational voice in the BIP process and node-signaling mechanisms.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, users_needing_base_features, payer,
    powerless, immediate, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper__protocol_ossification_reading, diffuse).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper__protocol_ossification_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents special-interest capture of Bitcoin's monetary rule by requiring near-universal consent for any protocol change, thereby preserving predictability of the 21-million supply cap and settlement assurances against politically motivated alterations.
% TRANSFER_FUNCTION: Moves innovation potential, transaction throughput, and developer attention from the base protocol to higher-layer constructions; moves legitimacy from dissenting technical proposals to the existing frozen protocol state.
% ABSENT_VOICES: Developers advocating base-layer scaling or advanced scripting; users in regions where high on-chain fees exclude them from self-custodial participation; alternative client teams whose implementations are treated as adversarial rather than experimental.
% DISAPPEARANCE_RATIONALE: If the ossification norm vanished, contentious base-layer upgrades would be attempted immediately, the threat of chain splits would rise, capital allocated to layer-two protocols would shift back toward base-layer capabilities, and the institutional-holder coalition would face the tail risks they structured around.
% FOUNDING_PROBLEM: Bitcoin lacks a formal governance mechanism; without a high bar for changes, well-funded special interests or miner coalitions could push through protocol alterations that serve their narrow interests at the expense of monetary predictability.
% FOUNDING_PROBLEM_CORROBORATION: Node operators and conservative developers attest the problem remains live. Base-layer innovators and external cryptocurrency governance researchers attest the acute capture risk was resolved by the 2017 UASF and that the current ossification norm exceeds the founding problem's scope, operating instead as status-quo bias.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__protocol_ossification_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__protocol_ossification_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__protocol_ossification_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_whitepaper__protocol_ossification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__protocol_ossification_reading, 0.65, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.65) is substantial because the consensus threshold blocks beneficial upgrades, forcing costs onto users and innovators. Suppression (0.78) is high because dissenting technical proposals are delegitimized as 'contentious' and excluded from the reference client. Theater_ratio (0.47) is moderate-to-high: the 'stability' virtue is partly genuine monetary-policy protection and partly performative conservatism that signals legitimacy to institutional capital. Accessibility_collapse (0.72) is high because, within the Bitcoin frame, forks attempting base changes are treated as altcoins rather than legitimate continuations. Resistance (0.60) is moderate because the blocksize war and subsequent debates show persistent, organized opposition. The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The node operator and institutional holder seats experience this constraint as protective rope preserving sound money. The base-layer innovator and excluded-user seats experience it as extractive obstruction blocking necessary evolution. The engine computes this divergence from the same structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Node operators and Core maintainers sit near the symmetric-to-beneficiary end (d ~ 0.2â0.35): they enforce the constraint and derive identity and stability from it, but do not personally capture financial rents. Layer-two operators and institutional holders are clear beneficiaries (d ~ 0.1â0.2). Base-layer innovators and users needing features are targets (d ~ 0.8â0.9): they bear the costs of blocked innovation and higher-layer fees. No override is needed because the structural derivation matches the domain picture.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â preventing capture of decentralized governance â was arguably live during the blocksize war but is contested today. The norm has outlived its acute threat and now persists as institutionalized bias. The disappearance_verdict is world_rearranges because removing the ossification norm would re-open base-layer governance, shift L2 investment, and alter the risk profile for institutional holders.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ossification_defense_vs_capture,
    'Is the universal-consensus requirement a genuine defense against special-interest capture, or has it become a tool for incumbent entrenchment?',
    'Historical analysis of which proposed changes were blocked and whether blocking served identifiable incumbent interests (L2 operators, large holders) versus protecting the monetary rule.',
    'If the latter, reclassification toward snare is warranted because the coordination function has become a cover story for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ossification_defense_vs_capture, empirical, 'Whether ossification protects against capture or entrenches incumbents').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (cost of running nodes, technical barriers to fork creation) or internalized (the norm that base-layer change is inherently dangerous)?',
    'Post-proposal trajectory analysis â do rejected proposals persist as minority forks or migrate to other chains, or do they disappear because developers internalize the illegitimacy frame?',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests â the target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism in protocol governance').

omega_variable(
    innovation_routing_or_destruction,
    'Does ossification genuinely route innovation to higher layers, or does it destroy innovation value that would have existed on the base layer?',
    'Comparative functional analysis of Bitcoin L2 capabilities versus base-layer-smart-contract platforms; measure whether use-cases are preserved with equivalent security or abandoned.',
    'If value is destroyed rather than routed, extraction is higher than measured because the constraint eliminates rather than redirects surplus.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(innovation_routing_or_destruction, conceptual, 'Whether blocked innovation is routed to L2 or destroyed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__protocol_ossification_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bitc_tr_t4, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement(bitc_tr_t8, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement(bitc_tr_t16, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 16, 0.45).
narrative_ontology:measurement(bitc_tr_t20, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 20, 0.47).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(bitc_be_t4, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(bitc_be_t8, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(bitc_be_t12, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(bitc_be_t16, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 16, 0.62).
narrative_ontology:measurement(bitc_be_t20, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(bitc_su_t4, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 4, 0.58).
narrative_ontology:measurement(bitc_su_t8, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(bitc_su_t12, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 12, 0.75).
narrative_ontology:measurement(bitc_su_t16, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 16, 0.78).
narrative_ontology:measurement(bitc_su_t20, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 20, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__protocol_ossification_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
