% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Bitcoin Whitepaper Immutability Covenant (Maximalist Reading)
 *   domain: cryptoeconomics/monetary systems/distributed consensus
 *
 * SUMMARY:
 *   The Bitcoin whitepaper is treated by the maximalist reading as a binding
 *   covenant: the 21-million supply cap, the emission schedule, and the
 *   foundational consensus rules are immutable doctrines that any change
 *   violates. This reading creates an extractive constraint that locks
 *   protocol governance into immutability, preventing changes that would
 *   benefit scalability, privacy, or long-term resilience. The maximalist
 *   reading holds that deviation from the whitepaper is heresy; the pragmatic
 *   and utility readings hold that the whitepaper is a technical starting
 *   point, not an eternal law. This story instantiates the maximalist
 *   reading's constraint: high extractiveness against protocol changes,
 *   beneficiaries are holders and supply-enforcement coalitions, victims are
 *   innovation and scalability layers.
 *
 * KEY AGENTS:
 *   - hodlers_early_adopters: Primary beneficiary (d ≈ 0.1–0.2, near beneficiary end). Benefit from scarcity guarantees; coordinate to enforce immutability through nodes and consensus signaling.
 *   - supply_cap_enforcers: Agenda setter (d ≈ 0.3–0.4, near beneficiary/setter blend). Core developers and mining pools control soft-fork activation; maintain the doctrine that whitepaper is covenant.
 *   - scalability_innovation_layers: Primary victim (d ≈ 0.8–0.9, near target end). Constrained by immutable base layer; cannot propose changes they believe necessary. Identity-locked advocates bear active suppression.
 *   - protocol_change_advocates: Secondary victim (d ≈ 0.75–0.85, near target end). Identity-locked by past commitment to Bitcoin; resistance to their proposals is organized and doctrine-enforced.
 *   - rival_protocol_communities: Excluded (not directly targeted, but negatively defined by the immutability stance). Their existence is the primary pressure on the constraint.
 *   - regulatory_authorities: Observer (analytical seat, d ≈ 0.5, symmetric). Monitoring whether immutability is tenable as global monetary infrastructure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__maximalist_reading, 0.72).
domain_priors:suppression_score(bitcoin_consensus_kernel__maximalist_reading, 0.68).
domain_priors:theater_ratio(bitcoin_consensus_kernel__maximalist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__maximalist_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_consensus_kernel__maximalist_reading, "Bitcoin Whitepaper Immutability Covenant (Maximalist Reading)").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__maximalist_reading, "cryptoeconomics/monetary systems/distributed consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__maximalist_reading, '11ad4d64-2bd9-4231-9cec-be7e2749024e').
narrative_ontology:cs_kernel_codification('11ad4d64-2bd9-4231-9cec-be7e2749024e', fixed_text).
narrative_ontology:cs_authority_grounding('11ad4d64-2bd9-4231-9cec-be7e2749024e', extraction).
narrative_ontology:cs_interpretation_layer_present('11ad4d64-2bd9-4231-9cec-be7e2749024e').
narrative_ontology:cs_reading_relation('11ad4d64-2bd9-4231-9cec-be7e2749024e', bitcoin_consensus_kernel__utility_reading, forecloses).
narrative_ontology:cs_reading_relation('11ad4d64-2bd9-4231-9cec-be7e2749024e', bitcoin_consensus_kernel__pragmatic_synthesis, coexists_with).
narrative_ontology:cs_axiom('11ad4d64-2bd9-4231-9cec-be7e2749024e', foundational, whitepaper_constitutes_binding_covenant).
narrative_ontology:cs_axiom_status(whitepaper_constitutes_binding_covenant, holdable).
narrative_ontology:cs_axiom_grounding('11ad4d64-2bd9-4231-9cec-be7e2749024e', whitepaper_constitutes_binding_covenant, conventional).
narrative_ontology:cs_axiom('11ad4d64-2bd9-4231-9cec-be7e2749024e', foundational, protocol_change_violates_founding_commitment).
narrative_ontology:cs_axiom_status(protocol_change_violates_founding_commitment, holdable).
narrative_ontology:cs_axiom_grounding('11ad4d64-2bd9-4231-9cec-be7e2749024e', protocol_change_violates_founding_commitment, deontological).
narrative_ontology:cs_reference_frame('11ad4d64-2bd9-4231-9cec-be7e2749024e', whitepaper_immutable_covenant).
narrative_ontology:cs_drift_state('11ad4d64-2bd9-4231-9cec-be7e2749024e', contemporary_scaling_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('11ad4d64-2bd9-4231-9cec-be7e2749024e', '2026-06-19T14:32:15Z').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, hodlers_early_adopters).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, supply_cap_enforcers).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, scalability_innovation_layers).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, protocol_change_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, global_node_operators).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, global_node_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold Bitcoin with expectation that the 21-million supply cap is cryptographically enforced forever, as promised by the whitepaper. Benefit from scarcity guarantees that price the asset; fear any protocol change that could alter monetary policy. Coordinate through nodes, mining pools, and consensus enforcement to reject changes. Their purchasing power and narrative dominance depend on the reading holding.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, hodlers_early_adopters, beneficiary,
    organized, generational, arbitrage, global).

% Core protocol developers, full-node operators, and mining cartels that enforce immutability through rejection of protocol upgrades that would alter the supply schedule or consensus rules. Set the agenda by controlling which code is deployed and which soft-fork signals pass the activation threshold. Maintain the interpretation that the whitepaper is a binding covenant, not a technical specification amenable to revision.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, supply_cap_enforcers, agenda_setter,
    institutional, civilizational, analytical, global).

% Layer-2 solutions (Lightning, Rollups), alternative consensus research, and protocol enhancement proposals that would benefit from base-layer changes (larger blocks, different UTXO semantics, script enhancements). Pay the cost of immutability freeze through constrained design space: they must build on a protocol they cannot change, accepting Byzantine broadcast limits and settlement lag. Their innovations are gated by the maximalist reading's enforced stasis.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, scalability_innovation_layers, payer,
    moderate, biographical, constrained, global).

% Technical researchers and developers who believe protocol evolution is necessary for Bitcoin's long-term viability—addressing privacy, throughput, or addressing unforeseen consensus failures. Locked by professional identity and past commitment to Bitcoin's success: leaving the protocol community to work on rival chains signals betrayal of their decades of contribution. Resistance to their proposals is active and organized; soft-fork rejection signals make the cost of persistence high.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, protocol_change_advocates, payer,
    moderate, biographical, identity_locked, global).

% Run full nodes that validate and relay transactions. Benefit from the immutability constraint because it simplifies their operating assumption—they do not have to track governance disputes or prepare for rule changes. Also bear a cost: larger blocks or script changes that might improve efficiency are blocked. Their power is latent—they can collectively reject a soft-fork by non-adoption, but coordination is expensive and the incentive to defect (running a lighter client) is always present.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, global_node_operators, beneficiary,
    powerful, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__maximalist_reading, global_node_operators, payer).

% Ethereum, Monero, and other consensus networks that permit protocol governance and iterative change. Structurally excluded from Bitcoin's covenant enforcement because accepting their legitimacy would require admitting that immutability is a design choice, not a natural law. Their existence is the primary pressure on the constraint; each successful Ethereum fork or Monero privacy upgrade makes the maximalist reading's immutability doctrine harder to defend.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, rival_protocol_communities, excluded,
    powerful, generational, arbitrage, global).

% The whitepaper as a canonical text and Satoshi's stated intent (to the extent it can be reconstructed from forum posts and emails). Functions as an authority reference, not an agent. The maximalist reading treats the legacy as doctrinal; the utility reading treats it as technical specification. Disputes over what Satoshi 'really meant' drive the constraint's persistence.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, satoshi_nakamoto_legacy, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(bitcoin_consensus_kernel__maximalist_reading, satoshi_nakamoto_legacy).

% Nation-state governments and central banks observing whether Bitcoin's immutability makes it a viable alternative monetary network or a deflationary dead-weight. Regulatory pressure is diffuse—some jurisdictions actively hostile, others neutral. The immutability covenant affects their calculus of whether to recognize, suppress, or co-opt Bitcoin infrastructure.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, regulatory_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_consensus_kernel__maximalist_reading, hodlers_early_adopters).
narrative_ontology:fixing_cost_class(bitcoin_consensus_kernel__maximalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, verifiable monetary policy immune to political interference or governance capture: a fixed supply cap, transparent issuance schedule, and cryptographic enforcement that no single actor—miner, developer, or nation-state—can unilaterally alter. Solves the coordination problem of committing to scarcity without a trusted third party.
% TRANSFER_FUNCTION: Transfers purchasing power and narrative authority from potential protocol reformers (scalability researchers, privacy advocates) to holders of the existing supply (early adopters, hodlers, large mining cartels). Immutability locks in the current allocation; changes that would enhance efficiency or reach are rejected as covenant violations, preserving the scarcity premium for existing holders.
% ABSENT_VOICES: Potential future users who would benefit from protocol evolution (privacy, throughput, censorship resistance against quantum threats) cannot advocate effectively because the maximalist reading forecloses their proposals before they enter the consensus-formation process. Governance disputes are framed as heresy, not legitimate technical disagreement. The constraint silences the question 'is immutability optimal, or is it a tradeoff?' by treating immutability as doctrine.
% DISAPPEARANCE_RATIONALE: If the maximalist reading vanished—if the Bitcoin community acknowledged that the whitepaper is a technical specification, not a binding covenant—protocol governance would become explicit and contentious. Hard forks would become possible. The supply cap, transaction throughput, and script capabilities would all come under active discussion. Rival networks that permit evolution would no longer be existential threats; they would be competing design choices. The price of Bitcoin might shift as the scarcity narrative weakened, or it might stabilize as the network's actual utility became decoupled from founding mythology.
% FOUNDING_PROBLEM: Central banks and governments debase currency through inflation and capture monetary policy for political ends. Satoshi Nakamoto proposed a peer-to-peer electronic cash system with a fixed, algorithmically enforced supply cap as a proof-of-concept that trustless, apolitical money was possible. The whitepaper was the founding covenant: 21 million coins, emission schedule locked in, no authority to alter it.
% FOUNDING_PROBLEM_CORROBORATION: Hodlers and maximalist developers attest the problem is live and immutability is the solution. Pragmatic researchers and scalability advocates counter that the founding problem (central bank debasement) is partially solved but the cure (absolute immutability) causes new problems (inability to patch bugs, address new threats, scale beyond boutique currency use). Regulatory authorities treat the immutability claim as a design assertion, not a solved problem—they remain agnostic on whether apolitical money is possible or desirable. No corroboration from outside the hodler/maximalist coalition exists; the utility advocates constitute the primary internal dissent.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__maximalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__maximalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__maximalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bitcoin_consensus_kernel__maximalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_consensus_kernel__maximalist_reading, 0.72, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness at 0.72 reflects the constraint's high cost to innovation advocates: they pay through constrained design space and governance exclusion, while the constraint persists because immutability benefits a concentrated coalition (hodlers, large miners). The measurement series traces extractiveness rising from 0.58 at the constraint's informal adoption (early 2010s, when immutability doctrine was first articulated) to 0.72 as it hardened into consensus law (2023–2025, as regulatory pressure and scalability failures made the tradeoff more visible). Theater_ratio rises from 0.28 to 0.41, indicating that enforcement activity increasingly consists of rhetoric defending immutability as doctrine rather than practical security work. Suppression rises from 0.54 to 0.68: soft-fork signal rejection and social enforcement of immutability doctrine have become more explicit as upgrade proposals have accumulated. The temporal drift models the constraint's maturation from informal expectation (t0) to formalized covenant doctrine (t16).
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (supply_cap_enforcers) experiences the constraint as a genuine coordination mechanism: a way to commit credibly to scarcity, solve the problem of currency debasement, and prevent governance capture. The victim seat (protocol_change_advocates) experiences the same structure as enforced stasis: a doctrine that was once reasonable but is now a veto on necessary evolution. The hodler seat (early_adopters) experiences it as a scarcity guarantee—their asset's value depends on immutability being credible. The engine computes these divergent classifications from the structural data (power atoms, exit options, beneficiary/victim declarations): the same constraint produces rope-like properties for beneficiaries (coordination benefit with low suppression needed because they consent) and snare-like properties for victims (extraction via governance exclusion, active suppression required to hold the stasis). Claimed type is tangled_rope because the structure requires BOTH genuine coordination (scarcity commitment) AND asymmetric extraction (innovation victims subsidizing hodler security).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation: hodlers are beneficiaries with institutional/organized power and arbitrage exit (they can sell, move to rival chains, or buy more)—their d ≈ 0.15–0.25 (beneficiary-leaning). Supply_cap_enforcers are beneficiaries with institutional power and analytical exit (they set the rules so exit is formal/defined)—their d ≈ 0.25–0.35 (setter-leaning). Scalability layers are victims with moderate power and constrained exit (cannot change base layer, can only build atop it)—their d ≈ 0.8–0.9 (target-leaning). Protocol advocates are victims with moderate power and identity_locked exit (professional reputation and past commitment lock them in)—their d ≈ 0.75–0.85 (target-leaning, amplified by identity lock). Global node operators have mobile exit (can switch chains, run lighter clients) and benefit from simplicity, so d ≈ 0.35–0.45 (moderate). No directionality overrides are needed: the structural data produces internally coherent d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (central bank debasement) is NOT dead, but its relationship to the constraint is contested. Early Bitcoin (2009–2015) genuinely solved a coordination problem: committing to scarcity without authority. The immutability covenant was the mechanism. But by 2020–2025, the problem landscape shifted: Bitcoin's primary use became store-of-value (not medium-of-exchange), regulatory frameworks stabilized (making central-bank opposition less existential), and new consensus mechanisms emerged that permit governance without sacrificing scarcity (Ethereum 2.0, Cardano). The maximalist reading responds by re-framing the founding problem as eternal ('any inflation is always a threat') and immutability as eternal law. This is the mandatrophy signature: the constraint persists not because the founding problem lives, but because the beneficiary coalition (hodlers, supply enforcers) has committed itself to immutability as doctrine. Fixing would require re-opening the question 'is absolute immutability necessary?' which the maximalist reading forecloses. The constraint does not meet piton criteria (performance without function) because immutability does deliver scarcity assurance; but it approaches piton as the scarcity assurance decouples from use-case necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    immutability_doctrine_vs_technical_specification,
    'Is the whitepaper a binding doctrinal covenant, or a technical specification amenable to improvement?',
    'Observe whether the community admits hard-fork proposals for debate (specification view) or reflexively rejects them as covenant violation (doctrine view). Examine discourse: are change proposals debated on technical merits, or dismissed as heresy?',
    'If resolved as doctrine, the constraint persists as immutable law and victim seats remain locked. If resolved as specification, governance becomes open and the constraint reclassifies to rope or scaffold (depending on whether change is permitted or deferred).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(immutability_doctrine_vs_technical_specification, conceptual, 'The core framing ambiguity: whitepaper as eternal law vs. technical starting point.').

omega_variable(
    beneficiary_coalition_stability,
    'Will the hodler/mining-cartel coalition remain cohesive as scalability failures accumulate and rival networks demonstrate competitive viability?',
    'Track voting behavior in soft-fork activation (do nodes continue to accept supply-cap enforcement soft-forks?), measure wealth concentration (are early hodlers'' proportional ownership increasing or decreasing?), and monitor exit rates to rival chains (do dissatisfied advocates and developers leave, or stay and organize dissent?).',
    'If coalition fractures, suppression required to hold immutability rises sharply and the constraint reclassifies upward toward snare. If coalition holds, the constraint remains tangled_rope (coordination + extraction in blend).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_coalition_stability, empirical, 'Whether the beneficiary coalition can sustain immutability doctrine against cumulating costs.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression primarily structural (external rejection of soft-forks, mining-pool signaling) or internalized (developers accept immutability as doctrine and self-censor proposals)?',
    'Post-exit observation: if a developer leaves Bitcoin for Ethereum and immediately publishes the scalability proposals they could not surface in Bitcoin, suppression was internalized. If suppression persists (other chains also reject the proposals), suppression was structural.',
    'If primarily internalized, the constraint''s effective suppression is higher than measured (victims carry the constraint with them). If primarily structural, the constraint''s hold weakens if the external enforcement (mining pools, core devs) fails.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is external enforcement or internalized doctrinal commitment.').

omega_variable(
    quantum_threat_soft_fork_exception,
    'Would the maximalist coalition permit a soft-fork to address quantum-computer threats to ECDSA, or would quantum-resistant script changes be treated as covenant violation?',
    'Pose the scenario explicitly in Bitcoin governance forums and community discourse. Observe whether the response treats quantum mitigation as an exception to immutability or as a constraint violation that must be accepted.',
    'If an exception emerges, immutability is not absolute—the covenant has conditions. This would establish a precedent for other ''existential'' changes (e.g., scaling, privacy). If no exception, the constraint demonstrates that immutability can override survival, reducing its viability as long-term monetary infrastructure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(quantum_threat_soft_fork_exception, preference, 'Whether immutability permits exceptions for existential threats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__maximalist_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(bitc_tr_t0, observed).
narrative_ontology:measurement(bitc_tr_t2, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 2, 0.31).
narrative_ontology:measurement_basis(bitc_tr_t2, observed).
narrative_ontology:measurement(bitc_tr_t4, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 4, 0.34).
narrative_ontology:measurement_basis(bitc_tr_t4, observed).
narrative_ontology:measurement(bitc_tr_t8, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement_basis(bitc_tr_t8, observed).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement_basis(bitc_tr_t12, observed).
narrative_ontology:measurement(bitc_tr_t16, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 16, 0.41).
narrative_ontology:measurement_basis(bitc_tr_t16, observed).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(bitc_be_t0, observed).
narrative_ontology:measurement(bitc_be_t2, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 2, 0.62).
narrative_ontology:measurement_basis(bitc_be_t2, observed).
narrative_ontology:measurement(bitc_be_t4, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 4, 0.65).
narrative_ontology:measurement_basis(bitc_be_t4, observed).
narrative_ontology:measurement(bitc_be_t8, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 8, 0.69).
narrative_ontology:measurement_basis(bitc_be_t8, observed).
narrative_ontology:measurement(bitc_be_t12, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 12, 0.71).
narrative_ontology:measurement_basis(bitc_be_t12, observed).
narrative_ontology:measurement(bitc_be_t16, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 16, 0.72).
narrative_ontology:measurement_basis(bitc_be_t16, observed).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(bitc_su_t0, observed).
narrative_ontology:measurement(bitc_su_t2, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 2, 0.58).
narrative_ontology:measurement_basis(bitc_su_t2, observed).
narrative_ontology:measurement(bitc_su_t4, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 4, 0.61).
narrative_ontology:measurement_basis(bitc_su_t4, observed).
narrative_ontology:measurement(bitc_su_t8, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 8, 0.65).
narrative_ontology:measurement_basis(bitc_su_t8, observed).
narrative_ontology:measurement(bitc_su_t12, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 12, 0.67).
narrative_ontology:measurement_basis(bitc_su_t12, observed).
narrative_ontology:measurement(bitc_su_t16, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement_basis(bitc_su_t16, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__maximalist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(bitcoin_consensus_kernel__maximalist_reading, 0.12).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__utility_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__pragmatic_synthesis).

% DUAL FORMULATION NOTE:
% The bitcoin_consensus_kernel decomposes into three structurally distinct constraint readings. This story (maximalist_reading) treats the whitepaper as binding doctrine, producing high extractiveness against protocol change. The utility_reading treats the whitepaper as technical specification, producing lower extractiveness and permitting innovation. The pragmatic_synthesis permits base-layer immutability with upper-layer evolution, trading off between them. Each reading has different ε, different victim/beneficiary structures, and different classifications. They are linked via network.affects_constraints because changes in one reading's credibility directly affect the others' operating environment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_consensus_kernel__maximalist_reading, organized, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
