% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__utility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: Bitcoin Whitepaper Utility Reading: Minimum Viable Consensus Enabling Iterative Improvement
 *   domain: cryptoeconomics/monetary_systems/distributed_consensus
 *
 * SUMMARY:
 *   The Bitcoin whitepaper functions as a contested kernel within
 *   cryptoeconomics. This constraint story instantiates the utility reading:
 *   the whitepaper established a minimum viable consensus mechanism (Nakamoto
 *   proof-of-work) whose purpose was to bootstrap a distributed system open
 *   to iterative improvement via soft forks and layered extension. The
 *   reading treats absolute monetary ossification not as a guaranteed
 *   covenant but as a contingent, emergent outcome. Beneficiaries are
 *   builders and adopters who gain functional evolution; victims are agents
 *   who purchased bitcoin under an expectation of immutable base-layer rules
 *   and now face erosion of that certainty through protocol upgrades. The
 *   claim/metric independence is maintained: the constraint is claimed as
 *   tangled_rope because it combines genuine distributed coordination with
 *   asymmetric extraction of policy certainty, while the metrics are authored
 *   descriptively to that structure.
 *
 * KEY AGENTS:
 *   - Protocol builders (agenda_setter/beneficiary): moderate power, mobile exit â drive iterative improvement and capture ecosystem value.
 *   - Utility adopters (beneficiary): powerless, constrained exit â benefit from functional upgrades but depend on decentralized consensus.
 *   - Monetary ossification reliants (payer): moderate power, identity_locked exit â bear the cost of eroded immutability guarantees.
 *   - Consensus enforcers (agenda_setter): organized, mobile exit â miners and nodes whose enforcement makes upgrades binding.
 *   - External monetary theorists (observer): analytical seat â evaluate monetary integrity from outside the beneficiary coalition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__utility_reading, 0.5).
domain_priors:suppression_score(bitcoin_consensus_kernel__utility_reading, 0.55).
domain_priors:theater_ratio(bitcoin_consensus_kernel__utility_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__utility_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_consensus_kernel__utility_reading, "Bitcoin Whitepaper Utility Reading: Minimum Viable Consensus Enabling Iterative Improvement").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__utility_reading, "cryptoeconomics/monetary_systems/distributed_consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__utility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__utility_reading, 'dc8c6474-60ff-4166-a19c-0697b1d4051b').
narrative_ontology:cs_kernel_codification('dc8c6474-60ff-4166-a19c-0697b1d4051b', fixed_text).
narrative_ontology:cs_authority_grounding('dc8c6474-60ff-4166-a19c-0697b1d4051b', distributed).
narrative_ontology:cs_reading_relation('dc8c6474-60ff-4166-a19c-0697b1d4051b', bitcoin_consensus_kernel__maximalist_reading, forecloses).
narrative_ontology:cs_reading_relation('dc8c6474-60ff-4166-a19c-0697b1d4051b', bitcoin_consensus_kernel__pragmatic_synthesis, influences).
narrative_ontology:cs_axiom('dc8c6474-60ff-4166-a19c-0697b1d4051b', foundational, base_layer_iterative_legitimacy).
narrative_ontology:cs_axiom_status(base_layer_iterative_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('dc8c6474-60ff-4166-a19c-0697b1d4051b', base_layer_iterative_legitimacy, instrumental).
narrative_ontology:cs_axiom('dc8c6474-60ff-4166-a19c-0697b1d4051b', foundational, ossification_not_guaranteed).
narrative_ontology:cs_axiom_status(ossification_not_guaranteed, holdable).
narrative_ontology:cs_axiom_grounding('dc8c6474-60ff-4166-a19c-0697b1d4051b', ossification_not_guaranteed, instrumental).
narrative_ontology:cs_reference_frame('dc8c6474-60ff-4166-a19c-0697b1d4051b', min_viable_consensus_state).
narrative_ontology:cs_drift_state('dc8c6474-60ff-4166-a19c-0697b1d4051b', contemporary_evolution_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('dc8c6474-60ff-4166-a19c-0697b1d4051b', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, protocol_builders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, utility_adopters).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, monetary_ossification_reliants).
narrative_ontology:constraint_vindicates(bitcoin_consensus_kernel__utility_reading, nakamoto_consensus_viability).
narrative_ontology:constraint_vindicates(bitcoin_consensus_kernel__utility_reading, permissionless_innovation_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and propose backward-compatible upgrades, layer-2 protocols, and iterative improvements to the base consensus. They rely on the whitepaper framing the consensus as minimum viable and open to evolution, capturing value from an expanding functional ecosystem.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, protocol_builders, agenda_setter,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__utility_reading, protocol_builders, beneficiary).

% Use Bitcoin and its layered extensions for payments, savings, or programmable contracts. They benefit from protocol upgrades that increase throughput, privacy, or programmability, but depend on the base layer remaining decentralized and secure.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, utility_adopters, beneficiary,
    powerless, biographical, constrained, global).

% Hold bitcoin expecting an immutable, ossified monetary policy and settlement base. They treat any base-layer protocol change as an existential risk to the guarantee they purchased. Soft forks and iterative improvements erode the certainty they rely on, imposing cognitive and financial costs as the rule set shifts beneath their position.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, monetary_ossification_reliants, payer,
    moderate, generational, identity_locked, global).

% Miners and full-node operators who enforce the current consensus rules through proof-of-work and block validation. They signal acceptance of upgrades via hash power and version bits; their distributed enforcement is what makes any iterative improvement binding on the network.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, consensus_enforcers, agenda_setter,
    organized, biographical, mobile, global).

% Analyze whether iterative base-layer evolution preserves the monetary properties of fixed supply and censorship resistance, or whether utility-driven upgrades undermine the store-of-value function from outside the benefiting coalition.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, external_monetary_theorists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables distributed agents to agree on a single transaction history and global state without a trusted intermediary, using proof-of-work and longest-chain selection, while preserving a social-technical path for backward-compatible protocol upgrades.
% TRANSFER_FUNCTION: Moves block subsidy and transaction fees to miners as the cost of enforcing consensus; moves policy certainty away from monetary ossification reliants toward builders and adopters who capture value from functional evolution.
% ABSENT_VOICES: Central bank digital currency architects and sovereign monetary issuers are structurally excluded from the permissionless governance conversation; they would argue for discretionary monetary policy or controlled upgrade paths but hold no formal seat in the protocol.
% DISAPPEARANCE_RATIONALE: If the consensus kernel vanished, the distributed ledger would fragment into incompatible histories, layer-2 settlements would lose their anchoring finality, and the permissionless innovation ecosystem would collapse into recentralization or chaotic reorganization.
% FOUNDING_PROBLEM: The double-spending problem in digital cash: achieving consensus on transaction ordering and validity in an open, adversarial network without relying on a central authority or trusted third party.
% FOUNDING_PROBLEM_CORROBORATION: Distributed systems researchers and cryptographers outside the immediate Bitcoin beneficiary set attest that Byzantine fault-tolerant consensus in open networks remains an active research and engineering problem; the Nakamoto mechanism is one proposed solution among several, corroborated by independent security analysis and peer-reviewed critique.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__utility_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__utility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__utility_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_consensus_kernel__utility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_consensus_kernel__utility_reading, 0.5, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is moderate (0.50) because the iterative improvement path imposes real costs on immutability-dependent holders while delivering coordination benefits to builders and adopters. Suppression (0.55) reflects the structural suppression of the no-change alternative once a soft fork achieves economic majority and orphaned-chain risk forces compliance. Theater ratio is low (0.25) because the proof-of-work coordination is functionally real, not performative. Accessibility collapse (0.60) captures that once an upgrade is activated, exit to the pre-upgrade rule set becomes practically unavailable. Resistance (0.45) reflects sustained opposition from ossification-reliant stakeholders during activation debates (e.g., block-size wars, soft-fork signaling contests).
 *
 * PERSPECTIVAL GAP:
 *   Builders and utility adopters experience the constraint as coordination-with-evolution: a genuine solution to distributed agreement that improves over time. Monetary ossification reliants experience the same structure as extraction: a founding guarantee they paid for (via volatility absorption and early adoption risk) is being unilaterally revised by a coalition of innovators. The engine computes this divergence from the structural data â the victim declaration, identity_locked exit, and beneficiary roles â rather than from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Protocol builders and utility adopters are declared beneficiaries with relatively mobile or constrained exit, placing their directionality near the beneficiary end (low d, damped effective extraction). Monetary ossification reliants are declared victims with identity_locked exit, placing their directionality near the full-target end (high d, amplified effective extraction). Consensus enforcers are agenda_setters with organized power and mobile exit, implying low d despite not being declared beneficiaries, because they administer the constraint rather than bearing its extractive force. The asymmetry is between those who gain utility from change and those who lose certainty from it.
 *
 * MANDATROPHY ANALYSIS:
 *   The utility reading prevents mandatrophy mislabeling by acknowledging the genuine coordination function (solving distributed consensus without trusted third parties) while simultaneously naming the asymmetric extraction (the loss of ossification guarantees for a specific stakeholder class). If the victim group were omitted, the constraint would present as pure coordination and risk misclassification as rope; if the coordination function were denied, it would present as pure extraction and risk misclassification as snare. The tangled_rope classification captures the hybrid reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    utility_reading_kernel_legitimacy,
    'Does the utility reading (''minimum viable consensus enabling iterative improvement'') represent a stable, textually grounded reading of the whitepaper kernel, or is it a post-hoc rationalization that extracts legitimacy from the founding text to justify governance evolution by current stakeholders?',
    'Historical-textual analysis of the whitepaper and Satoshi-era communications against the claim of ''minimum viable'' intent; sociological study of whether the reading emerged organically from the founding period or was retrofitted after the fact to justify specific upgrade trajectories.',
    'If the reading is retrofitted, the constraint shifts toward snare (coordination story as cover for extraction by current builders); if genuinely embedded in the kernel, it remains tangled_rope or rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(utility_reading_kernel_legitimacy, conceptual, 'Whether the utility reading is authentic to the kernel or post-hoc.').

omega_variable(
    ossification_reliant_victim_status,
    'Do monetary ossification reliants bear a genuine structural cost from iterative base-layer improvement, or do they hold a contestable preference that the utility reading simply does not serve?',
    'Empirical analysis of portfolio rebalancing and holder behavior during soft-fork activation events; measurement of chain-split risk, replay protection costs, and volatility shocks borne by passive long-term holders.',
    'If structural costs are verified, the victim declaration is valid and the asymmetric extraction gate holds; if purely preference-based, the constraint may be better classified as rope with heterogeneous user types rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ossification_reliant_victim_status, empirical, 'Whether ossification-dependent holders are genuine victims of extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__utility_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(btc_util_tr_t0, bitcoin_consensus_kernel__utility_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(btc_util_tr_t3, bitcoin_consensus_kernel__utility_reading, theater_ratio, 3, 0.12).
narrative_ontology:measurement(btc_util_tr_t6, bitcoin_consensus_kernel__utility_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(btc_util_tr_t9, bitcoin_consensus_kernel__utility_reading, theater_ratio, 9, 0.18).
narrative_ontology:measurement(btc_util_tr_t12, bitcoin_consensus_kernel__utility_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(btc_util_tr_t15, bitcoin_consensus_kernel__utility_reading, theater_ratio, 15, 0.25).

% Extraction over time
narrative_ontology:measurement(btc_util_be_t0, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(btc_util_be_t3, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 3, 0.3).
narrative_ontology:measurement(btc_util_be_t6, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 6, 0.35).
narrative_ontology:measurement(btc_util_be_t9, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 9, 0.4).
narrative_ontology:measurement(btc_util_be_t12, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 12, 0.45).
narrative_ontology:measurement(btc_util_be_t15, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 15, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(btc_util_su_t0, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(btc_util_su_t3, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 3, 0.4).
narrative_ontology:measurement(btc_util_su_t6, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 6, 0.45).
narrative_ontology:measurement(btc_util_su_t9, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 9, 0.5).
narrative_ontology:measurement(btc_util_su_t12, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 12, 0.53).
narrative_ontology:measurement(btc_util_su_t15, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 15, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__utility_reading, global_infrastructure).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel__maximalist_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel__pragmatic_synthesis).

% DUAL FORMULATION NOTE:
% The natural-language concept 'Bitcoin consensus' decomposes into three structurally distinct readings of the whitepaper kernel: maximalist (immutable covenant), pragmatic synthesis (immutable base, innovative layers), and utility (minimum viable, iteratively improvable). Each reading instantiates a different constraint with distinct beneficiary/victim structures and epsilon values. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
