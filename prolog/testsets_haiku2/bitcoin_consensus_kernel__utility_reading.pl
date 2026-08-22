% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__utility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Bitcoin Consensus as Evolving Minimum Viable Mechanism (Utility Reading)
 *   domain: cryptoeconomics/monetary_systems
 *
 * SUMMARY:
 *   The Bitcoin consensus mechanism—described in the 2008 whitepaper as a
 *   'minimum viable' system enabling decentralized agreement—sits at the
 *   center of a reading contest among the cryptocurrency community. The
 *   utility reading frames the consensus mechanism as designed for iterative
 *   improvement: soft forks permit backward-compatible upgrades (e.g.,
 *   Taproot, witness data improvements); hard forks are exceptional
 *   democratic decisions (e.g., block size); layer-2 protocols (Lightning,
 *   Liquid) extend Bitcoin's capability without modifying base consensus.
 *   Under this reading, Bitcoin is a living system whose rules can change
 *   through transparent, signaled governance. The maximalist reading frames
 *   the whitepaper as an immutable covenant: any rule change—even soft
 *   forks—betrays the founding monetary guarantee. The pragmatic synthesis
 *   treats base-layer monetary rules (21M supply cap, PoW) as immutable while
 *   permitting upper-layer evolution. This constraint story instantiates ONLY
 *   the utility reading: Bitcoin's consensus is minimum viable, improvements
 *   are legitimate, beneficiaries are adopters/builders who gain from
 *   evolution, victims are those who hold absolute ossification guarantees.
 *
 * KEY AGENTS:
 *   - Bitcoin adopters: users and holders who benefit from network effects and utility improvements
 *   - Layer-2 developers: architects of Lightning, sidechains, rollups who depend on consensus permission
 *   - Node operators: validators who run full nodes and signal soft-fork acceptance
 *   - Protocol researchers: cryptographers and core developers proposing consensus improvements
 *   - Consensus maintainers: informal governance structure (Core devs, major miners, institutional custodians)
 *   - Monetary maximalists: excluded advocates of absolute rule immutability
 *   - Ossification purists: identity-locked believers in rule stasis
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__utility_reading, 0.48).
domain_priors:suppression_score(bitcoin_consensus_kernel__utility_reading, 0.32).
domain_priors:theater_ratio(bitcoin_consensus_kernel__utility_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__utility_reading, rope).
narrative_ontology:human_readable(bitcoin_consensus_kernel__utility_reading, "Bitcoin Consensus as Evolving Minimum Viable Mechanism (Utility Reading)").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__utility_reading, "cryptoeconomics/monetary_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__utility_reading, '9142f074-d342-4c60-bc4d-c249f0e8b574').
narrative_ontology:cs_kernel_codification('9142f074-d342-4c60-bc4d-c249f0e8b574', fixed_text).
narrative_ontology:cs_authority_grounding('9142f074-d342-4c60-bc4d-c249f0e8b574', practice).
narrative_ontology:cs_interpretation_layer_present('9142f074-d342-4c60-bc4d-c249f0e8b574').
narrative_ontology:cs_reading_relation('9142f074-d342-4c60-bc4d-c249f0e8b574', bitcoin_consensus_kernel__maximalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('9142f074-d342-4c60-bc4d-c249f0e8b574', bitcoin_consensus_kernel__pragmatic_synthesis, influences).
narrative_ontology:cs_axiom('9142f074-d342-4c60-bc4d-c249f0e8b574', foundational, whitepaper_minimum_viable_specification).
narrative_ontology:cs_axiom_status(whitepaper_minimum_viable_specification, holdable).
narrative_ontology:cs_axiom_grounding('9142f074-d342-4c60-bc4d-c249f0e8b574', whitepaper_minimum_viable_specification, conventional).
narrative_ontology:cs_axiom('9142f074-d342-4c60-bc4d-c249f0e8b574', foundational, consensus_change_via_signaled_adoption).
narrative_ontology:cs_axiom_status(consensus_change_via_signaled_adoption, holdable).
narrative_ontology:cs_axiom_grounding('9142f074-d342-4c60-bc4d-c249f0e8b574', consensus_change_via_signaled_adoption, conventional).
narrative_ontology:cs_reference_frame('9142f074-d342-4c60-bc4d-c249f0e8b574', consensus_as_living_specification).
narrative_ontology:cs_drift_state('9142f074-d342-4c60-bc4d-c249f0e8b574', institutional_adoption_phase, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9142f074-d342-4c60-bc4d-c249f0e8b574', '2026-06-15T14:32:00Z').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, bitcoin_adopters).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, layer_two_developers).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, node_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, proof_of_work_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Users and holders who benefit from a functioning, evolving consensus mechanism. They gain from network effects, market liquidity, and technological improvements that increase Bitcoin's utility as money and settlement layer. They can exit by divesting and moving to alternative chains or payment systems, but chose Bitcoin because its consensus model permits orderly improvement without requiring unanimous agreement on immutable rules.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, bitcoin_adopters, beneficiary,
    organized, generational, mobile, global).

% Developers building on layer-2 systems (Lightning Network, Liquid, sidechains, rollups) who depend on the base-layer consensus permitting their existence and evolution. They benefit because soft forks and consensus-layer flexibility allow them to extend Bitcoin's capability without forking away. They have exit options (build on Ethereum, other L1s) but chose Bitcoin because its governance framework treats improvements as legitimate.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, layer_two_developers, beneficiary,
    moderate, biographical, mobile, global).

% Individuals and institutions running full nodes and validating consensus. They participate in signaling acceptance of soft forks and in validating hard-fork readiness. They benefit from a mechanism that permits gradual, backward-compatible upgrades (soft forks) without forcing every node to upgrade immediately or fracturing the network. They are somewhat constrained by Bitcoin's base layer if they want to participate; exit means ceasing to run a node.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, node_operators, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__utility_reading, node_operators, agenda_setter).

% Academics, cryptographers, and core developers who research and propose improvements to the consensus mechanism. Under this reading, they are treated as legitimate architects whose proposals (BIPs, research papers) advance Bitcoin's function. They have high exit options (other projects, academia, finance) and choose to engage because Bitcoin's consensus model treats iterative improvement as valid.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, protocol_researchers, agenda_setter,
    moderate, biographical, mobile, global).

% Holders and advocates committed to the reading that Bitcoin's founding rules are immutable covenants. They would argue that any change to consensus rules—even soft forks, even layer-2 protocols—violates the founding commitment and represents a theft of the original monetary guarantee. They are excluded from the primary decision-making if their frame is that consensus rules cannot be touched; their objections are heard but treated as objections to evolution, not vetos.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, monetary_maximalists, excluded,
    powerful, civilizational, identity_locked, global).

% The informal governance structure (core developers, major miners, institutional custodians, researchers) that stewards consensus changes. Under the utility reading, they are legitimated to propose, debate, and implement soft forks and protocol upgrades on the grounds that the whitepaper describes a minimum viable consensus mechanism, not a finished product. They cannot unilaterally fork; changes require signaling and broad adoption.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_maintainers, agenda_setter,
    powerful, generational, analytical, global).

% Individuals and groups who believe Bitcoin's monetary properties derive from rule immutability and who interpret any consensus change—however marginal—as a betrayal of the founding commitment. They view soft forks as creeping normalization of rule mutation. Under this reading, their objection is valid but does not block evolution; they can exit to a fork (Bitcoin Cash, etc.) or to another consensus model.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, ossification_purists, excluded,
    moderate, civilizational, identity_locked, global).

% Miners and the ecosystem of hardware/energy suppliers whose incentives are aligned with Bitcoin's continued operation. They benefit from a consensus mechanism that permits efficiency improvements (e.g., Taproot, witness data optimization) because these lower validation costs without breaking their hash-commitment. They are somewhat constrained by Bitcoin's PoW commitment; moving to other chains means new hardware amortization.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, proof_of_work_community, beneficiary,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a minimum viable consensus mechanism that permits iterative cryptographic, efficiency, and feature improvements without requiring unanimous agreement on immutable rules. The mechanism coordinates disagreement: soft forks (backward-compatible upgrades) and layer-2 protocols allow network evolution while nodes retain the power to reject changes by refusing to upgrade. Hard forks are exceptional and can splinter the network, which creates strong incentives for consensus but permits recourse when irreconcilable views arise.
% TRANSFER_FUNCTION: Moves legitimacy to define 'valid consensus' from a fixed, immutable ruleset to a living, signaled process. Adopters and builders transfer power to propose changes to protocol researchers and consensus maintainers; in return, they retain the power to reject changes by refusing to upgrade. Ossification guarantees (the belief that rules must never change) are transferred away: the utility reading does not treat immutability as a foundational property.
% ABSENT_VOICES: Monetary maximalists and ossification purists would argue that consensus should be totally immutable and that treating the whitepaper as a minimum viable system—rather than a finished monetary constitution—is a betrayal of the founding covenant. They are structurally excluded by the utility reading's framing, though they can voice objections and can fork the network if consensus evolves in ways they reject.
% DISAPPEARANCE_RATIONALE: If the utility reading's legitimacy vanished—if the consensus culture reverted to strict immutabilism—Bitcoin's capacity to adapt to changing security landscapes, efficiency opportunities, and technological improvements would collapse. Layer-2 protocols would lose their evolutionary support; protocol researchers would redirect to other chains; node operators would face pressure to accept ossified rules or fork. The network would either splinter into maximalist-strict and pragmatic forks, or stagnate into a ceremonial network with declining utility.
% FOUNDING_PROBLEM: The Bitcoin whitepaper describes a minimum viable consensus mechanism (PoW, decentralized signaling, 51% attack resistance). It does not describe a finished monetary system with immutable rules. The founding problem is: how can a distributed network agree on changes to its rules without sacrificing decentralization or security? Answer: through soft forks (backward-compatible upgrades), hard forks (network-consensus decisions with exit by non-adopters), and layer-2 protocols (evolution outside the base consensus).
% FOUNDING_PROBLEM_CORROBORATION: The Bitcoin Core developer community, the Lightning Network research and implementation teams, and academic researchers in distributed consensus (outside the maximalist camp) attest that the founding problem persists: Bitcoin must adapt to quantum threats, scaling pressures, and efficiency improvements. Mining pools and institutional custodians attest that soft-fork governance has proven viable over 15+ years of practice. Monetary maximalists attest the founding problem is to preserve immutability, not to improve functionality—but this corroboration comes from inside the maximalist reading, not independent. External corroboration comes from computer science literature on consensus evolution and from the empirical fact that every successful decentralized network (Ethereum, etc.) has implemented governance mechanisms for change.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__utility_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__utility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__utility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bitcoin_consensus_kernel__utility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_consensus_kernel__utility_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.48 at interval end) because under the utility reading, protocol changes are legitimated through consensus signaling, but the process concentrates power in researchers and maintainers who propose changes. Adopters retain exit via hard-fork rejection, but switching costs are substantial. Theater is low (0.18) because the governance process is transparent—BIPs are public, signaling is visible, implementation is code-reviewed—and the soft-fork mechanism is technically sound. Suppression is moderate (0.32) because ossification advocates and maximalists are structurally excluded from the primary decision frame; their objections are heard but treated as objections to evolution, not vetos. The measurement trajectory shows slow, modest growth in extractiveness and suppression over the interval (0 to 20), reflecting increasing institutional adoption and rising pressure to formalize governance as the network scales. Growth flattens by t=20 as the soft-fork pattern becomes entrenched and node-operator signaling stabilizes.
 *
 * PERSPECTIVAL GAP:
 *   The utility reading constructs a large perspectival divergence between agenda-setting seats (protocol researchers, consensus maintainers) and payer seats (ossification maximalists). From the agenda-setter seat, treating consensus as evolving is legitimate and necessary for Bitcoin to survive quantum threats and scaling challenges. From the maximalist seat, any change is theft of a monetary guarantee. The excluded position (maximalists) experiences high effective extraction because the utility frame treats their objections as opinions, not constraints. Node operators and adopters occupy a middle ground: they benefit from evolution but retain veto power via refusal to upgrade, making their directionality closer to d=0.5 than pure beneficiary or target.
 *
 * DIRECTIONALITY LOGIC:
 *   Adopters (d near 0.2, beneficiaries): gain utility improvements and network growth without forced upgrade; mobile exit options to other chains reduce capture. Layer-2 developers (d near 0.15, beneficiaries): depend on consensus permission but retain mobile exit to other L1s; soft-fork mechanism legitimates their work. Node operators (d near 0.35, beneficiary+agenda-setter dual): benefit from evolution, signal acceptance of soft forks, but somewhat constrained to Bitcoin if they want base-layer participation. Protocol researchers (d near 0.25, agenda-setter): high power to propose changes but face community objection if changes are perceived as rent-seeking; mobile exit to other projects. Monetary maximalists (d near 0.78, excluded): experience the utility frame as extraction of their ossification guarantee; identity-locked exit because their belief system ties their identity to absolute immutability. Consensus maintainers (d near 0.55, agenda-setter): moderate extraction risk because they control proposal authority but lack veto power; community can fork away if they overreach.
 *
 * MANDATROPHY ANALYSIS:
 *   The utility reading avoids mandatrophy by treating consensus evolution as a live, contentious problem with no settled answer. The founding problem (how to permit rule changes in a decentralized system) remains open; the utility reading proposes an answer (soft forks + hard-fork democracy + layer-2 freedom) but does not claim to have solved it permanently. The reading's strength is that it describes an actual governance mechanism (soft-fork signaling) with 15+ years of empirical track record. Mandatrophy pressure comes from the maximalist reading's claim that the whitepaper established immutable rules: if that reading were correct and Bitcoin acted on it, the consensus mechanism would become ceremonial (rules unchanged while technology evolved around it). The utility reading resists mandatrophy by treating the whitepaper as an open specification, not a closed constitution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_immutability_vs_evolution,
    'Does the 2008 whitepaper describe an immutable monetary covenant or a minimum viable consensus mechanism designed for iterative improvement?',
    'Textual analysis of the whitepaper''s language (e.g., ''initial'' block reward, ''network adjusts'', ''may follow other paths'') and comparison with Satoshi Nakamoto''s contemporaneous writings. Historical analysis of the original codebase (hardcoded constants, signal-readiness mechanisms) to determine whether the code was written for immutability or flexibility.',
    'If the text is ambiguous but the code contains soft-fork readiness (version signaling, witness separation logic), the utility reading wins structurally. If the text is unambiguously ''this shall never change,'' the maximalist reading wins. The pragmatic reading splits the difference (rules immutable, mechanisms flexible).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_immutability_vs_evolution, conceptual, 'Whether the whitepaper''s foundational intent was monetary immutability or evolutionary capability.').

omega_variable(
    soft_fork_legitimacy_under_consensus,
    'Is a soft fork (backward-compatible change that doesn''t split the network) a ''true'' consensus change or does treating it as consensual constitute rule violation when some nodes disagree?',
    'Examine whether dissenting nodes (those that reject the soft fork) remain in consensus with non-dissenting nodes on the canonical ledger. If yes, the soft fork preserved consensus at the cost of some nodes'' rules being stricter; if no, the network split, and the ''consensus'' claim is false.',
    'If soft forks preserve consensus, they are a legitimate expression of consensus-by-signaling. If they create invisible forks (dissenting nodes out of sync without realizing it), the utility reading''s evolutionary mechanism breaks down and the reading becomes a snare (consensus maintainers enforce upgrades by silent forking).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(soft_fork_legitimacy_under_consensus, empirical, 'Whether soft forks constitute true consensus or coercive rule change masked as compatibility.').

omega_variable(
    ossification_guarantee_as_victim,
    'Is the ''immutable monetary policy'' that maximalists believe they hold a genuine property of the Bitcoin system or a belief about how the system should operate?',
    'Historical analysis: when consensus rules changed in Bitcoin''s past (block subsidy schedule, transaction format, script capabilities), did the maximalists'' belief system predict no change would occur? If the belief has repeatedly failed reality-testing, it is ideology, not property. If the belief has held in core monetary rules (21M cap, PoW) while failing on non-monetary features (script expansion, witness format), the boundary between ''true immutability'' and ''feature flexibility'' is real.',
    'If the ossification guarantee is a belief, not a property, then maximalists are not ''victims'' of consensus evolution—they are adherents of an ideology that the network rejected. If the guarantee is partially real (core money rules are stable, features change), then the utility reading extracts from maximalists by reframing what ''immutable'' means (operational stability vs. code immutability).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ossification_guarantee_as_victim, empirical, 'Whether immutability is a technical property or an ideological reading of Bitcoin''s history.').

omega_variable(
    excluded_voice_representation,
    'Are monetary maximalists genuinely excluded from consensus decision-making, or do they have structural veto power (via refusing to upgrade, forcing a hard fork that splits the network)?',
    'Examine past hard-fork events (Bitcoin Cash, etc.) to determine whether minority dissent forced a network split. If yes, dissent has veto power even if it is excluded from the primary decision frame. If no, dissent was overridden by the majority, confirming exclusion with power asymmetry.',
    'If veto power exists, maximalists are constrained, not excluded—they have substantial leverage over consensus changes via the threat of forking. If no veto power exists, the utility reading''s exclusion of their voice is genuine extraction: they lose the ability to enforce their reading of immutability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(excluded_voice_representation, empirical, 'Whether network-splitting hard forks represent veto power for dissenting minorities.').

omega_variable(
    layer_two_protocol_as_consensus_change,
    'Do layer-2 protocols (Lightning, sidechains) represent consensus-permitted evolution, or do they constitute de facto rule changes by routing around consensus constraints?',
    'Analyze whether layer-2 protocols require base-layer consensus approval to operate (e.g., do they require new opcodes, witness data formats, or soft forks?). If they require approval, they are consensus evolution. If they operate without approval (purely on top of existing rules), they are end-run evolution that the utility reading mistakes for legitimate innovation.',
    'If layer-2 requires soft forks, the utility reading''s claim that layer-2 enables ''evolution outside consensus'' is false—layer-2 is just another vector for consensus change. If layer-2 operates on top of unchanged rules, then the utility reading correctly identifies a mechanism for innovation that does not change core consensus.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(layer_two_protocol_as_consensus_change, empirical, 'Whether layer-2 protocols depend on base-layer consensus changes or can operate independently.').


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
narrative_ontology:measurement(bitc_tr_t2, bitcoin_consensus_kernel__utility_reading, theater_ratio, 2, 0.11).
narrative_ontology:measurement_basis(bitc_tr_t2, observed).
narrative_ontology:measurement(bitc_tr_t5, bitcoin_consensus_kernel__utility_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement_basis(bitc_tr_t5, observed).
narrative_ontology:measurement(bitc_tr_t10, bitcoin_consensus_kernel__utility_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement_basis(bitc_tr_t10, observed).
narrative_ontology:measurement(bitc_tr_t15, bitcoin_consensus_kernel__utility_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement_basis(bitc_tr_t15, projected).
narrative_ontology:measurement(bitc_tr_t20, bitcoin_consensus_kernel__utility_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(bitc_tr_t20, projected).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(bitc_be_t0, observed).
narrative_ontology:measurement(bitc_be_t2, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 2, 0.36).
narrative_ontology:measurement_basis(bitc_be_t2, observed).
narrative_ontology:measurement(bitc_be_t5, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 5, 0.41).
narrative_ontology:measurement_basis(bitc_be_t5, observed).
narrative_ontology:measurement(bitc_be_t10, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement_basis(bitc_be_t10, observed).
narrative_ontology:measurement(bitc_be_t15, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 15, 0.47).
narrative_ontology:measurement_basis(bitc_be_t15, projected).
narrative_ontology:measurement(bitc_be_t20, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement_basis(bitc_be_t20, projected).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(bitc_su_t0, observed).
narrative_ontology:measurement(bitc_su_t2, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 2, 0.22).
narrative_ontology:measurement_basis(bitc_su_t2, observed).
narrative_ontology:measurement(bitc_su_t5, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 5, 0.26).
narrative_ontology:measurement_basis(bitc_su_t5, observed).
narrative_ontology:measurement(bitc_su_t10, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 10, 0.31).
narrative_ontology:measurement_basis(bitc_su_t10, observed).
narrative_ontology:measurement(bitc_su_t15, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 15, 0.32).
narrative_ontology:measurement_basis(bitc_su_t15, projected).
narrative_ontology:measurement(bitc_su_t20, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 20, 0.32).
narrative_ontology:measurement_basis(bitc_su_t20, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__utility_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(bitcoin_consensus_kernel__utility_reading, 0.22).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel__maximalist_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel__pragmatic_synthesis).

% DUAL FORMULATION NOTE:
% The bitcoin_consensus_kernel contains three structurally distinct constraints corresponding to three readings: utility (this file), maximalist, and pragmatic. Each reading has a different ε, different beneficiary/victim structure, and different governance implications. The utility reading (this file) frames consensus as evolving and treats improvements as legitimate. The maximalist reading frames consensus as immutable and treats changes as covenant violation. The pragmatic reading splits the difference: immutable base layer, evolving upper layer. All three readings operate on the same technical system but instantiate different extractiveness profiles and governance legitimacies. Network edges link the three files bidirectionally; empirical and conceptual omegas in each file address sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_consensus_kernel__utility_reading, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
