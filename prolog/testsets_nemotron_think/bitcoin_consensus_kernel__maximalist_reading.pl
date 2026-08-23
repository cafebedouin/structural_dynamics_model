% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   human_readable: Bitcoin Maximalist Immutable Monetary Policy
 *   domain: cryptoeconomic/monetary/consensus
 *
 * SUMMARY:
 *   The maximalist reading of the Bitcoin consensus kernel treats the 21M
 *   supply cap and immutable emission schedule as a founding covenant that
 *   cannot be altered without violating the system's constitutional
 *   legitimacy. This reading emerged from the whitepaper's presentation of
 *   Bitcoin as 'a purely peer-to-peer version of electronic cash' with a
 *   predetermined monetary policy. Over 2009-2024, the constraint's
 *   extractiveness has risen as the scarcity premium became the primary value
 *   proposition, while suppression of protocol changes (block size increases,
 *   tail emission proposals, drivechain sidechains) has intensified through
 *   social consensus enforcement by node operators and mining pools. The
 *   claimed mountain type reflects the maximalist self-presentation as
 *   natural law; the metrics capture the active enforcement and rising
 *   extraction from innovation layers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__maximalist_reading, 0.78).
domain_priors:suppression_score(bitcoin_consensus_kernel__maximalist_reading, 0.85).
domain_priors:theater_ratio(bitcoin_consensus_kernel__maximalist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__maximalist_reading, mountain).
narrative_ontology:human_readable(bitcoin_consensus_kernel__maximalist_reading, "Bitcoin Maximalist Immutable Monetary Policy").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__maximalist_reading, "cryptoeconomic/monetary/consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__maximalist_reading).
domain_priors:emerges_naturally(bitcoin_consensus_kernel__maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__maximalist_reading, '9d97c1c7-80d2-4d7e-9fdb-54f762628a36').
narrative_ontology:cs_kernel_codification('9d97c1c7-80d2-4d7e-9fdb-54f762628a36', fixed_text).
narrative_ontology:cs_authority_grounding('9d97c1c7-80d2-4d7e-9fdb-54f762628a36', lineage).
narrative_ontology:cs_interpretation_layer_present('9d97c1c7-80d2-4d7e-9fdb-54f762628a36').
narrative_ontology:cs_reading_relation('9d97c1c7-80d2-4d7e-9fdb-54f762628a36', bitcoin_consensus_kernel__pragmatic_synthesis, forecloses).
narrative_ontology:cs_reading_relation('9d97c1c7-80d2-4d7e-9fdb-54f762628a36', bitcoin_consensus_kernel__utility_reading, influences).
narrative_ontology:cs_axiom('9d97c1c7-80d2-4d7e-9fdb-54f762628a36', foundational, monetary_policy_immutability_absolute).
narrative_ontology:cs_axiom_status(monetary_policy_immutability_absolute, holdable).
narrative_ontology:cs_axiom_grounding('9d97c1c7-80d2-4d7e-9fdb-54f762628a36', monetary_policy_immutability_absolute, deontological).
narrative_ontology:cs_axiom('9d97c1c7-80d2-4d7e-9fdb-54f762628a36', foundational, whitepaper_as_constitutional_text).
narrative_ontology:cs_axiom_status(whitepaper_as_constitutional_text, holdable).
narrative_ontology:cs_axiom_grounding('9d97c1c7-80d2-4d7e-9fdb-54f762628a36', whitepaper_as_constitutional_text, conventional).
narrative_ontology:cs_axiom('9d97c1c7-80d2-4d7e-9fdb-54f762628a36', secondary, parameter_change_equals_covenant_violation).
narrative_ontology:cs_axiom_status(parameter_change_equals_covenant_violation, holdable).
narrative_ontology:cs_axiom_grounding('9d97c1c7-80d2-4d7e-9fdb-54f762628a36', parameter_change_equals_covenant_violation, deontological).
narrative_ontology:cs_reference_frame('9d97c1c7-80d2-4d7e-9fdb-54f762628a36', satoshi_whitepaper_covenant).
narrative_ontology:cs_drift_state('9d97c1c7-80d2-4d7e-9fdb-54f762628a36', post_blocksize_war_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9d97c1c7-80d2-4d7e-9fdb-54f762628a36', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, bitcoin_holders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, early_adopters).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, mining_pools).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, layer2_builders).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, scalability_researchers).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, protocol_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, institutional_investors).
narrative_ontology:constraint_vindicates(bitcoin_consensus_kernel__maximalist_reading, sound_money_doctrine).
narrative_ontology:constraint_vindicates(bitcoin_consensus_kernel__maximalist_reading, fixed_supply_superiority).
narrative_ontology:constraint_vindicates(bitcoin_consensus_kernel__maximalist_reading, consensus_immutability_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold BTC as store of value; benefit directly from the immutable cap through scarcity premium appreciation. Can exit to other assets instantly (arbitrage-grade), but their wealth is denominated in the constraint's credibility. They advocate for the maximalist rule because it protects their nominal holdings' purchasing power.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, bitcoin_holders, beneficiary,
    organized, biographical, arbitrage, global).

% Accumulated large positions at low cost; the immutable cap locks in their outsized returns. They fund maximalist narrative infrastructure (media, conferences, developer grants) and exercise disproportionate influence over social consensus. Exit is trivial but unnecessary — they are the constraint's primary political constituency.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, early_adopters, beneficiary,
    powerful, biographical, arbitrage, global).

% Enforce consensus rules by choosing which blocks to build on; collect block subsidies and transaction fees. The immutable cap guarantees their revenue schedule is predictable and non-dilutable. They cannot easily exit mining without sunk-cost loss, but they can signal support for or against protocol changes. Their coordination with maximalist holders sustains the enforcement equilibrium.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, mining_pools, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__maximalist_reading, mining_pools, beneficiary).

% Run validating nodes that reject blocks violating the 21M cap. They are the direct enforcement mechanism — a rule change requires them to upgrade software. They have arbitrage-grade exit (can run any client), but the maximalist social consensus makes running non-maximalist software a self-exclusion from 'Bitcoin'. Their incentive is network legitimacy, not direct extraction.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, node_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Build payment channels, rollups, and statechains on Bitcoin's base layer. The immutable cap and fixed block space constrain their design space: they must work within throughput limits that the maximalist rule forbids expanding. They bear opportunity cost of foregone innovations (e.g., covenants, drivechains) and pay higher fees during congestion. Exit means abandoning Bitcoin's liquidity and security — costly but possible.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, layer2_builders, payer,
    moderate, biographical, constrained, global).

% Research protocol-level scaling (block size, sharding, tail emission). Their work is structurally excluded by the maximalist consensus — proposals are rejected before technical evaluation. They bear career cost of working on 'non-Bitcoin' systems or leaving the domain. Exit is mobile (skills transfer to other chains) but identity-locked for those committed to Bitcoin's mission.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, scalability_researchers, payer,
    moderate, biographical, mobile, global).

% Maintain Bitcoin Core reference client. They implement the maximalist rule because the social consensus demands it; proposing cap changes ends their maintainer status. They bear the cost of maintaining software that cannot evolve its monetary policy, while competing chains iterate. Exit means leaving the project — high identity cost for long-term contributors.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, protocol_developers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__maximalist_reading, protocol_developers, excluded).

% Allocate capital to BTC as digital gold. The immutable cap is their primary investment thesis — it provides the regulatory and narrative clarity that enables institutional adoption. They do not run nodes or write code but their capital weight reinforces the maximalist equilibrium. Exit is instant but would signal loss of confidence in the thesis.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, institutional_investors, beneficiary,
    institutional, generational, arbitrage, global).

% Analyze Bitcoin's monetary policy as a case study in credible commitment and decentralized governance. They observe the constraint's operation without bearing its costs or collecting its rents. Their analysis informs but does not determine the social consensus.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, academic_cryptoeconomists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a credible, unalterable monetary policy that enables Bitcoin to function as a trust-minimized store of value without a central issuer — solving the time-inconsistency problem of discretionary monetary policy.
% TRANSFER_FUNCTION: Transfers option value of protocol flexibility from builders/researchers (who lose design space for scaling, privacy, and feature innovation) to holders (who gain certainty of fixed supply and predictable inflation schedule).
% ABSENT_VOICES: Future users who would benefit from a more scalable or feature-rich Bitcoin; unborn generations who inherit the monetary policy choice; developers who left the ecosystem because their proposals were foreclosed. They are absent because the constraint's enforcement mechanism (social consensus) treats their hypothetical preferences as irrelevant to the founding covenant.
% DISAPPEARANCE_RATIONALE: If the immutable cap constraint vanished overnight, Bitcoin would likely hard fork into multiple chains with different monetary policies (tail emission, dynamic supply, etc.), the 'digital gold' narrative would fracture, institutional capital would reallocate, and the maximalist coalition would lose its coordinating Schelling point. The world of Bitcoin-as-sound-money would rearrange fundamentally.
% FOUNDING_PROBLEM: Create a digital currency with a credible, non-discretionary monetary policy that cannot be debased by any central authority or majority coalition — solving the trust problem that caused all prior private currencies to fail.
% FOUNDING_PROBLEM_CORROBORATION: Maximalists (beneficiaries) attest the problem is live and the 21M cap is the only solution. Pragmatic synthesis proponents (outside beneficiaries) attest the founding problem (credible commitment) is solved by the base layer's difficulty adjustment and social consensus, not the specific 21M parameter — citing academic work on credible commitment mechanisms (Kroll et al. 2013, Chiu & Koeppl 2019). Utility reading proponents attest the problem was 'minimum viable consensus' and the cap was a parameter choice, not the solution.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__maximalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__maximalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__maximalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_consensus_kernel__maximalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_consensus_kernel__maximalist_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_consensus_kernel__maximalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_consensus_kernel__maximalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, ExtMetricName, E),
    domain_priors:suppression_score(bitcoin_consensus_kernel__maximalist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(bitcoin_consensus_kernel__maximalist_reading),
    narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(bitcoin_consensus_kernel__maximalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the immutable cap creates a structural transfer from would-be protocol innovators (who bear opportunity cost of constrained design space) to holders (who capture scarcity rent). Suppression (0.85) is very high because protocol changes violating the cap are actively rejected by the node network — not merely discouraged but made practically impossible without a contentious hard fork that maximalists frame as an attack. Theater ratio (0.25) is low-moderate: the coordination function (credible monetary policy) is real, but a growing share of enforcement energy defends the specific 21M number rather than the coordination function itself. Accessibility collapse (0.88) is near-maximal because alternatives (altcoins, layer-2 designs, sidechains) are treated as categorically different systems, not variations within Bitcoin. Resistance (0.15) is low because the constraint's beneficiaries control the enforcement machinery (nodes, hashpower, narrative).
 *
 * PERSPECTIVAL GAP:
 *   From the holder/maximalist seat, the constraint is a Mountain — an immutable law that creates trust. From the builder/developer seat, it is a Snare — an actively enforced extraction that forecloses design space and captures innovation value. The engine computes this divergence from the structural data: same constraint, opposite lived types.
 *
 * DIRECTIONALITY LOGIC:
 *   Bitcoin holders and early adopters are structural beneficiaries (d near 0.0): they collect the full scarcity premium without bearing protocol governance costs. Mining pools sit near symmetric (d ~0.4): they enforce the rule and collect fees, but their revenue depends on the cap's credibility. Layer-2 builders, scalability researchers, and protocol developers are structural targets (d near 1.0): their design space is constrained by the immutable base layer, they bear the cost of working around fixed throughput, and they have no voice in the consensus rule. Node operators are agenda_setters with institutional power and arbitrage-grade exit (they can run any software), but they coordinate on the maximalist rule because it legitimizes their role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (credible digital scarcity without trusted issuer) remains live — but the maximalist reading treats the specific 21M parameter as the solution rather than the adjustable mechanism. Mandatrophy risk is high: the constraint's coordination function (credible commitment) may be served by a broader class of rules, but the maximalist reading forecloses that inquiry by equating any parameter change with covenant violation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_covenant,
    'Is the 21M cap a discovered natural law of decentralized consensus (like the speed of light) or a constructed social covenant that benefits identifiable agents?',
    'Counterfactual history: if Satoshi had chosen 2.1M or 210M with identical emission curve, would the system''s coordination function differ? If not, the specific number is constructed, not natural.',
    'If constructed, the False Summit Mountain signature triggers — the constraint is a Tangled Rope (coordination + extraction) masquerading as Mountain. If natural, the Mountain claim holds and extraction metrics reflect coordination cost, not rent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_covenant, conceptual, 'Whether the constraint''s mountain claim survives parameter counterfactuals.').

omega_variable(
    extraction_incidence_on_innovation_layers,
    'Does the immutable cap extract from layer-2 builders and protocol researchers by foreclosing design space, or does it create the credible foundation that makes their work valuable?',
    'Measure layer-2 deployment velocity and capital efficiency on Bitcoin vs. chains with flexible base layers, controlling for network effects. If Bitcoin L2s systematically underperform despite larger capital base, extraction is real.',
    'If extraction is confirmed, the constraint is Tangled Rope from builder seats (coordination for holders, extraction from builders). If foundation value dominates, the constraint is Rope from all seats — the cap enables the coordination that builders leverage.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_incidence_on_innovation_layers, empirical, 'Direction of value flow between base-layer immutability and upper-layer innovation.').

omega_variable(
    enforcement_mechanism_capture,
    'Is the node-operator enforcement of the 21M cap a genuine decentralized consensus, or has it been captured by the holder-miner coalition that benefits from the cap?',
    'Analyze node operator demographics, signaling behavior during contentious debates (SegWit2x, Taproot), and correlation between node operation and BTC holdings. If node operators are predominantly large holders, enforcement is captured.',
    'If captured, suppression metrics reflect coalition enforcement, not decentralized consensus — the constraint is Snare from non-holder seats. If genuine, suppression is the cost of credible commitment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_capture, empirical, 'Whether the enforcement machinery is independent of the beneficiary coalition.').

omega_variable(
    kernel_reading_foreclosure_structure,
    'Does the maximalist reading''s core premise (any parameter change violates the covenant) logically foreclose the pragmatic synthesis reading (base immutable, upper layers free), or do they coexist as different parties'' commitments?',
    'Formalize both readings in a commitment logic framework. If pragmatic synthesis can be expressed without contradicting maximalist axioms, they coexist. If pragmatic synthesis requires denying a maximalist foundational axiom, foreclosure holds.',
    'Foreclosure means the kernel has a structural fault line — maximalist and pragmatic readings cannot occupy the same framework. Coexistence means the kernel supports multiple legitimate instantiations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_structure, conceptual, 'Logical relationship between maximalist and pragmatic synthesis readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__maximalist_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(btc_max_tr_t0, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(btc_max_tr_t3, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 3, 0.12).
narrative_ontology:measurement(btc_max_tr_t6, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(btc_max_tr_t9, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 9, 0.18).
narrative_ontology:measurement(btc_max_tr_t12, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(btc_max_tr_t15, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 15, 0.25).

% Extraction over time
narrative_ontology:measurement(btc_max_be_t0, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(btc_max_be_t3, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 3, 0.55).
narrative_ontology:measurement(btc_max_be_t6, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 6, 0.65).
narrative_ontology:measurement(btc_max_be_t9, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 9, 0.72).
narrative_ontology:measurement(btc_max_be_t12, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 12, 0.76).
narrative_ontology:measurement(btc_max_be_t15, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 15, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(btc_max_su_t0, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(btc_max_su_t3, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 3, 0.68).
narrative_ontology:measurement(btc_max_su_t6, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 6, 0.75).
narrative_ontology:measurement(btc_max_su_t9, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 9, 0.8).
narrative_ontology:measurement(btc_max_su_t12, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 12, 0.83).
narrative_ontology:measurement(btc_max_su_t15, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 15, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__maximalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(bitcoin_consensus_kernel__maximalist_reading, 0.08).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__pragmatic_synthesis).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__utility_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, lightning_network_consensus).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_script_evolution).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the bitcoin_consensus_kernel family. The maximalist reading claims the entire kernel (whitepaper + consensus rules) as immutable covenant. The pragmatic synthesis reading decomposes the kernel into base-layer monetary rules (immutable) and upper-layer protocols (innovation-permitting). The utility reading treats the kernel as a minimum viable coordination mechanism. The three readings share the whitepaper as kernel text but instantiate different constraints with different ε, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_consensus_kernel__maximalist_reading, institutional, 0.15).
constraint_indexing:directionality_override(bitcoin_consensus_kernel__maximalist_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
