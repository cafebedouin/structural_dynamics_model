% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__utility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bitcoin_consensus_kernel__utility_reading
 *   human_readable: Bitcoin Consensus as Iterative Improvement Mechanism (Utility Reading)
 *   domain: cryptoeconomics/monetary_systems
 *
 * SUMMARY:
 *   The Bitcoin Whitepaper establishes a consensus protocol with immutable
 *   monetary rules (fixed supply, defined emission schedule) but also
 *   describes a decentralized mechanism for iterative improvement. This
 *   reading instantiates the UTILITY interpretation: the Whitepaper's core
 *   claim is not that the specific initial design is unchangeable, but that a
 *   distributed set of actors CAN coordinate improvements without centralized
 *   authority or destructive forks. The constraint is the social agreement to
 *   permit iterative enhancement within base-layer immutability (soft forks,
 *   backward-compatible changes) and upper-layer experimentation (layer-two
 *   protocols, sidechains). This reading coexists with the MAXIMALIST reading
 *   (which holds the specific monetary rules inviolable) and the PRAGMATIC
 *   SYNTHESIS reading (which distinguishes immutable base-layer rules from
 *   innovative upper layers more explicitly). This story models the utility
 *   reading's structural position: moderate extraction because core
 *   developers exercise real gatekeeping authority; soft forks require
 *   consensus coordination, which distributes power more than centralized
 *   amendment would; but the coordination carries a narrative cost for those
 *   committed to absolute immutability.
 *
 * KEY AGENTS:
 *   - Protocol adopters: benefit from iterative improvement, have exit options to alternative chains
 *   - Application builders: benefit from upper-layer experimentation capability, are partially constrained by base-layer design
 *   - Core developers: set the consensus agenda, propose soft forks, constrained by peer review and activation requirements
 *   - Ossification guarantee advocates: bear identity-locked cost of narrative dissonance as protocol evolves
 *   - Maximalist doctrine holders: organized, identity-locked, experience soft forks as covenant violation
 *   - Network security apparatus: maintains incentive structure, analyzes whether changes preserve security
 *   - Alternative protocol designers: excluded from Bitcoin's activation machinery, cannot effectively propose radically different consensuses
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__utility_reading, 0.42).
domain_priors:suppression_score(bitcoin_consensus_kernel__utility_reading, 0.28).
domain_priors:theater_ratio(bitcoin_consensus_kernel__utility_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__utility_reading, rope).
narrative_ontology:human_readable(bitcoin_consensus_kernel__utility_reading, "Bitcoin Consensus as Iterative Improvement Mechanism (Utility Reading)").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__utility_reading, "cryptoeconomics/monetary_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__utility_reading, '7cafbabf-d97b-48e5-af08-1d348eb80e94').
narrative_ontology:cs_kernel_codification('7cafbabf-d97b-48e5-af08-1d348eb80e94', fixed_text).
narrative_ontology:cs_authority_grounding('7cafbabf-d97b-48e5-af08-1d348eb80e94', lineage).
narrative_ontology:cs_interpretation_layer_present('7cafbabf-d97b-48e5-af08-1d348eb80e94').
narrative_ontology:cs_reading_relation('7cafbabf-d97b-48e5-af08-1d348eb80e94', bitcoin_consensus_kernel__maximalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('7cafbabf-d97b-48e5-af08-1d348eb80e94', bitcoin_consensus_kernel__pragmatic_synthesis, coexists_with).
narrative_ontology:cs_axiom('7cafbabf-d97b-48e5-af08-1d348eb80e94', foundational, consensus_mechanism_iteratively_improvable).
narrative_ontology:cs_axiom_status(consensus_mechanism_iteratively_improvable, holdable).
narrative_ontology:cs_axiom_grounding('7cafbabf-d97b-48e5-af08-1d348eb80e94', consensus_mechanism_iteratively_improvable, instrumental).
narrative_ontology:cs_axiom('7cafbabf-d97b-48e5-af08-1d348eb80e94', foundational, soft_fork_backward_compatible_evolution_legitimate).
narrative_ontology:cs_axiom_status(soft_fork_backward_compatible_evolution_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('7cafbabf-d97b-48e5-af08-1d348eb80e94', soft_fork_backward_compatible_evolution_legitimate, conventional).
narrative_ontology:cs_reference_frame('7cafbabf-d97b-48e5-af08-1d348eb80e94', decentralized_consensus_substrate_framework).
narrative_ontology:cs_drift_state('7cafbabf-d97b-48e5-af08-1d348eb80e94', contemporary_scaling_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7cafbabf-d97b-48e5-af08-1d348eb80e94', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, protocol_adopters).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, application_builders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, layer_two_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, ossification_guarantees_holders).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, maximalist_doctrine_advocates).
narrative_ontology:constraint_vindicates(bitcoin_consensus_kernel__utility_reading, decentralized_consensus_is_experimentally_viable).
narrative_ontology:constraint_vindicates(bitcoin_consensus_kernel__utility_reading, base_layer_immutability_compatible_with_upper_layer_innovation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain access to a self-correcting consensus mechanism that improves over time without requiring centralized authority. Can exit by adopting alternative protocols, but benefit from network effects and established security properties of iterative refinement. Use the protocol's capability to evolve in response to real operational experience.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, protocol_adopters, beneficiary,
    organized, generational, mobile, global).

% Build on and within the protocol knowing the base layer will improve iteratively rather than ossify. Benefit from protocol enhancements (transaction throughput, script capability, cryptographic flexibility) without requiring hard forks that could create chain splits. Can build layer-two systems and experimental protocols atop the base layer.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, application_builders, beneficiary,
    moderate, generational, mobile, global).

% Propose, debate, and implement consensus changes through established review processes (BIPs, consensus rules). Wield institutional authority to decide which soft forks activate and which proposals are rejected. Constrained by peer review and the requirement that changes maintain backward compatibility or establish new coordination points.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, core_developers, agenda_setter,
    institutional, generational, constrained, global).

% Hold an ideological commitment that the Whitepaper's monetary policy is immutable and inviolable — that any deviation from fixed supply and defined emission schedule violates the founding covenant. Experience each iterative change as an erosion of their core value claim. Cannot credibly exit without abandoning their foundational commitment to Bitcoin as 'digital gold.'
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, ossification_guarantees_holders, payer,
    powerful, civilizational, identity_locked, global).

% Advocate that Bitcoin's legitimacy rests on immutable monetary rules and permissionless base-layer operation. View soft forks and layer-two systems as dilution of the purity claim. Bear the cost of narrative dissonance as the protocol iteratively improves in ways their doctrine declares impermissible.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, maximalist_doctrine_advocates, payer,
    organized, civilizational, identity_locked, global).

% Maintains the incentive structure (proof-of-work, miner selection, reward schedule) that underpins the consensus mechanism. Participates in fork activation as a mining/validation constituency. Analyzes whether iterative changes preserve or degrade security properties.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, network_security_apparatus, observer,
    institutional, generational, analytical, global).

% Would propose alternative consensus mechanisms or different evolutionary trajectories but lack the coordination power to activate them in Bitcoin's network. Could propose hard forks or new chains but face network-effect lock-in. Their voice is absent from the consensus-activation machinery.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, alternative_protocol_designers, excluded,
    moderate, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_consensus_kernel__utility_reading, core_developers).
narrative_ontology:fixing_cost_class(bitcoin_consensus_kernel__utility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a decentralized mechanism by which a distributed set of independent actors can propose, discuss, and collectively activate improvements to a consensus protocol without requiring centralized authority or split risk, enabling the network to adapt to operational experience, cryptographic advances, and scaling challenges while preserving core security properties.
% TRANSFER_FUNCTION: Moves authority to set consensus rules from centralized entities to a distributed body of developers, miners, and nodes; in exchange, ossification guarantees (the claim that the protocol is immutable) are reframed as narrative preferences rather than structural properties. The protocol itself transfers legitimacy from 'money with fixed rules' to 'experimentally viable consensus substrate.'
% ABSENT_VOICES: Alternative protocol designers and those committed to absolute immutability as a core covenant cannot effectively influence consensus-activation decisions; they would argue for either hard-fork restrictions or mandatory coin splits as required by protocol violation. Their exclusion is structural: activation requires coordination on the Bitcoin chain itself, which they reject.
% DISAPPEARANCE_RATIONALE: If iterative improvement ceased and the protocol ossified absolutely, the Bitcoin network would face obsolescence as security assumptions fail and scaling constraints bind. Builders would migrate to more adaptive protocols; adopters would seek alternatives as functionality ceiling hit hard limits. The distributed consensus mechanism would lose its core value proposition.
% FOUNDING_PROBLEM: How to coordinate decentralized improvements to a consensus protocol without creating a central authority or split-inducing fork risk — how to build a system that can adapt to real operational experience while preserving the claim of decentralization.
% FOUNDING_PROBLEM_CORROBORATION: Independent protocol researchers (Buterin on consensus upgrading, Zooko on fork mechanics) attest the problem remains active; Bitcoin core developers confirm iterative soft-fork activation is their operational model; network operators document real constraints (transaction throughput, script expressiveness) that required iterative solutions. No attesting authority claims the problem is solved or the original design was sufficient.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__utility_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__utility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__utility_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(bitcoin_consensus_kernel__utility_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_consensus_kernel__utility_reading_tests).
:- end_tests(bitcoin_consensus_kernel__utility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) because core developers do exercise real gatekeeping power — they propose and debate soft forks, and their social authority shapes which changes activate. However, the constraint distributes this power more widely than centralized authority would; activation requires coordination among miners, node operators, and users, which diffuses gatekeeping. Extraction grows from 0.30 to 0.42 over the interval as the protocol matures and the importance of core-developer consensus becomes empirically clear — early, near-theoretical discussions underestimated the coordination cost; later, operational experience shows consensus-achieving requires substantial negotiation. Suppression is low (0.28) because the constraint does not rely on coercion: disagreeing actors can fork the chain or build alternative protocols, and exit is structurally available (though path-dependent). Theater ratio is low (0.22) because the iteration machinery is functionally real — soft forks do change the protocol, layer-two protocols do enable new applications, and the peer-review process does surface genuine technical and policy tradeoffs. Theater increases modestly as the process becomes more formalized and visible (more documentation, more governance discussion), but functional improvement remains primary. The measurement series uses one shared time grid so every metric is authored at every point.
 *
 * PERSPECTIVAL GAP:
 *   The core developers and protocol adopters should compute as experiencing coordination and benefit; the ossification advocates should compute as experiencing identity-locked extraction despite participating in the adoption. The agenda setter (core developers) experiences the constraint as enabling distributed coordination and iterative refinement; the identity-locked payers experience the same constraint as a gradual betrayal of covenant. The engine computes this divergence from the structural data (power, exit options, beneficiary/victim status). The authored claim (rope: genuine coordination with minor extraction cost) reflects the utility reading's own evaluation; the authored metrics (moderate extractiveness, rising theater, modest suppression) model operational reality that the constraint's existence creates a gatekeeping structure even if the gate is distributed.
 *
 * DIRECTIONALITY LOGIC:
 *   Core developers occupy the agenda-setter seat (d~0.70): they propose changes, their technical judgment shapes which proposals are serious candidates, and they have constrained exit (leaving Bitcoin development constrains their influence elsewhere). Protocol adopters and application builders occupy beneficiary seats (d~0.20-0.30): they benefit from protocol improvements without running the consensus machinery, and they have mobile exit (adopt alternative chains if improvements don't materialize). Ossification guarantee advocates occupy the payer seat (d~0.85): they bear a narrative cost as soft forks activate — their foundational claim that Bitcoin's immutability is absolute is structurally contradicted by observed protocol evolution. Their exit is identity-locked because leaving Bitcoin abandons the asset they committed to as a representation of perfect monetary policy. The network security apparatus occupies an observer seat (analytical): it maintains technical properties but does not initiate consensus changes. Alternative protocol designers occupy an excluded seat: they are trapped because Bitcoin's network effects lock in coordination, and they cannot propose radically different consensuses within Bitcoin's activation mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading explicitly rejects mandatrophy: the founding problem (decentralized consensus improvement) is live and the constraint (iterative soft-fork mechanism) addresses it functionally. The maximalist reading, by contrast, is vulnerable to mandatrophy (the founding problem of monetary immutability is defined as solved by fixed rules, but the constraint's actual operation involves changing rules). The pragmatic synthesis reading avoids mandatrophy by declaring different layers have different inviolability. Under the utility reading, there is no mandated immutability to violate — the mandate is iterative adaptation. The constraint persists because network participants genuinely benefit from iterative improvement and because the coordination mechanism distributes power enough that no single actor can unilaterally change rules. This reading's vulnerability is not mandatrophy but identity-lock: those who stake their value claim on absolute monetary immutability experience the constraint as systematically undermining their core narrative, even though they benefit from the network's security properties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_committer_context,
    'Is the Whitepaper''s core claim the specific immutable monetary rules, or the mechanism for distributed consensus improvement, or both equally?',
    'Textual analysis of the Whitepaper''s framing; empirical study of which claims Bitcoin adopters cite when justifying commitment; examination of Satoshi''s stated intent (where recoverable) and community schism patterns when iterations have occurred.',
    'The utility reading rests on reading the mechanism as primary; the maximalist reading rests on reading the monetary specifics as primary. If textual and historical evidence favors one reading, that reading''s ε and type are confirmed and the sibling reading''s framing is recast as interpretive overlay. If evidence is ambiguous, both readings remain live, coexisting with different parties holding each.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_committer_context, conceptual, 'Interpretive contest within the Bitcoin kernel: mechanism vs. monetary covenant as the founding claim.').

omega_variable(
    soft_fork_as_circumvention,
    'When a soft fork changes consensus rules (e.g., witness data format changes, script capability additions), does it constitute protocol evolution or rule violation?',
    'Technical analysis of backward compatibility: if old nodes continue to validate the chain without deviation, it is a soft fork; if they diverge, it is a hard fork. Normative analysis: does backward compatibility suffice to preserve ''the original protocol,'' or does any rule change constitute violation?',
    'The utility reading treats soft forks as legitimate evolutionary mechanisms; the maximalist reading treats them as technical loopholes that preserve appearance of immutability while violating substance. If backward compatibility is sufficient, soft forks are evolution (utility reading confirmed); if substance-of-rules matters more than backward compatibility, soft forks are circumvention (maximalist reading vindicated).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(soft_fork_as_circumvention, conceptual, 'Whether soft forks constitute protocol evolution or rule violation under different normative frameworks.').

omega_variable(
    layer_two_protocol_binding,
    'Do layer-two protocols (Lightning Network, rollups) that operate atop Bitcoin count as Bitcoin consensus changes, or are they separate protocols layered on immutable base layer?',
    'Specification analysis: if layer-two rules require base-layer consensus changes to implement, they are bound; if they operate on unmodified base-layer rules, they are separate. Empirical analysis: do layer-two innovations'' success depend on base-layer soft forks, or can they succeed on fixed rules?',
    'The pragmatic synthesis reading treats layer-two as preserving base-layer immutability while enabling upper-layer innovation; the maximalist reading treats layer-two as inferior workarounds to ''true'' Bitcoin if they require any base-layer change. The utility reading treats layer-two as exemplifying the iterative-improvement mechanism: new protocols tested and adopted without requiring consensus of the entire base layer.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(layer_two_protocol_binding, empirical, 'Whether layer-two protocols preserve or require modification of base-layer immutability.').

omega_variable(
    core_developer_gatekeeping_concentration,
    'As the protocol matures, does the concentration of core-developer authority over consensus changes increase or decrease, and what threshold of concentration would convert the constraint from rope to tangled-rope or snare?',
    'Measurement of decision-making power distribution: track soft-fork proposal counts, activation rates by proposer, and whether minority developers can effectively veto changes. Historical analysis: has developer concentration increased relative to early years?',
    'High concentration would support reclassification to tangled_rope (coordination function plus asymmetric extraction). Low concentration would confirm rope. The utility reading assumes a rope classification; if concentration is rising and the beneficiary group (protocol adopters) cannot effectively exit, the constraint''s type shifts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(core_developer_gatekeeping_concentration, empirical, 'Whether core-developer gatekeeping power is distributed or concentrated, and whether it is rising over time.').

omega_variable(
    identity_lock_mechanism_internalization,
    'Is the suppression experienced by ossification advocates structural (they have few technical exit options) or internalized (they have exits but suppress their own departure to preserve narrative purity)?',
    'Post-fork behavior analysis: when soft forks activate despite advocate opposition, do advocates fork the chain, migrate to alternative protocols, or remain on Bitcoin while narrating the change as non-violation? If they remain and reframe, suppression is partly internalized.',
    'If internalized, the constraint''s effective suppression is higher than the authored 0.28 suggests, and the identity-lock is the vehicle. If structural, the low suppression score is accurate. The distinction affects whether the constraint should be re-read as identity_coordination type (which carries higher Boltzmann floor).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_internalization, empirical, 'Whether suppression of ossification advocates is structural constraint or internalized identity-lock.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__utility_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_consensus_kernel__utility_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(bitc_tr_t0, observed).
narrative_ontology:measurement(bitc_tr_t4, bitcoin_consensus_kernel__utility_reading, theater_ratio, 4, 0.12).
narrative_ontology:measurement_basis(bitc_tr_t4, observed).
narrative_ontology:measurement(bitc_tr_t8, bitcoin_consensus_kernel__utility_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement_basis(bitc_tr_t8, observed).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_consensus_kernel__utility_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement_basis(bitc_tr_t12, observed).
narrative_ontology:measurement(bitc_tr_t16, bitcoin_consensus_kernel__utility_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement_basis(bitc_tr_t16, observed).
narrative_ontology:measurement(bitc_tr_t20, bitcoin_consensus_kernel__utility_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(bitc_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(bitc_be_t0, observed).
narrative_ontology:measurement(bitc_be_t4, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 4, 0.36).
narrative_ontology:measurement_basis(bitc_be_t4, observed).
narrative_ontology:measurement(bitc_be_t8, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 8, 0.39).
narrative_ontology:measurement_basis(bitc_be_t8, observed).
narrative_ontology:measurement(bitc_be_t12, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 12, 0.41).
narrative_ontology:measurement_basis(bitc_be_t12, observed).
narrative_ontology:measurement(bitc_be_t16, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement_basis(bitc_be_t16, observed).
narrative_ontology:measurement(bitc_be_t20, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement_basis(bitc_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(bitc_su_t0, observed).
narrative_ontology:measurement(bitc_su_t4, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 4, 0.19).
narrative_ontology:measurement_basis(bitc_su_t4, observed).
narrative_ontology:measurement(bitc_su_t8, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 8, 0.22).
narrative_ontology:measurement_basis(bitc_su_t8, observed).
narrative_ontology:measurement(bitc_su_t12, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 12, 0.26).
narrative_ontology:measurement_basis(bitc_su_t12, observed).
narrative_ontology:measurement(bitc_su_t16, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 16, 0.27).
narrative_ontology:measurement_basis(bitc_su_t16, observed).
narrative_ontology:measurement(bitc_su_t20, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement_basis(bitc_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__utility_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(bitcoin_consensus_kernel__utility_reading, 0.12).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel__maximalist_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel__pragmatic_synthesis).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, layer_two_protocol_family).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, soft_fork_activation_machinery).

% DUAL FORMULATION NOTE:
% This constraint (utility reading) interprets the Bitcoin Whitepaper as establishing a decentralized mechanism for iterative consensus improvement. It coexists with maximalist_reading (which interprets the Whitepaper as establishing immutable monetary rules that cannot change) and pragmatic_synthesis (which separates base-layer immutability from upper-layer innovation). The three readings share the same kernel text but instantiate different ε values, beneficiary structures, and type classifications. All three belong to the bitcoin_consensus_kernel family; all three must be linked via network.affects_constraints. The utility reading is structurally dependent on the pragmatic synthesis reading (PS provides the 'layer distinction' that UR operationalizes as 'soft forks are upper-layer coordination'); UR influences maximalist reading by providing a live alternative interpretation that undermines the maximalist claim of inevitable covenant violation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_consensus_kernel__utility_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
