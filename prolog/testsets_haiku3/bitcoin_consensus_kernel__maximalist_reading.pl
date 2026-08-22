% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Bitcoin Maximalist Covenant: Immutable Whitepaper Monetary Policy
 *   domain: cryptoeconomics/consensus/monetary_systems
 *
 * SUMMARY:
 *   The Bitcoin Whitepaper establishes a protocol with a fixed 21-million
 *   coin cap, a halving schedule, and Proof-of-Work consensus. The maximalist
 *   reading treats this specification as an inviolable monetary covenant: any
 *   change to consensus rules that alters these parameters violates the
 *   founding commitment. This reading emerged from cypherpunk ideology and
 *   has become institutionalized through node operator consensus, mining
 *   cartel alignment, and hodl-culture narrative control. The constraint
 *   operates as tangled rope: it provides genuine coordination (decentralized
 *   consensus on scarcity) while extracting from those who would innovate on
 *   the base layer or scale the protocol. The measurement series tracks how
 *   extractiveness and theater ratio have risen as the constraint hardened:
 *   early Bitcoin (interval start) treated the whitepaper as a minimum viable
 *   specification; contemporary Bitcoin (interval end) treats it as
 *   constitutional immutability.
 *
 * KEY AGENTS:
 *   - early_adopters: beneficiaries of scarcity guarantees; d≈0.1 (full beneficiary)
 *   - monetary_maximalists: agenda-setters defending immutability; d≈0.15 (secondary beneficiary, gatekeeper)
 *   - protocol_innovators: constrained by immutability veto; d≈0.85 (target)
 *   - layer2_developers: forced to off-chain architecture; d≈0.75 (target)
 *   - whitepaper_authority: the kernel (non-agent, immutable object); defines the constraint's referent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__maximalist_reading, 0.81).
domain_priors:suppression_score(bitcoin_consensus_kernel__maximalist_reading, 0.67).
domain_priors:theater_ratio(bitcoin_consensus_kernel__maximalist_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0.67).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__maximalist_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_consensus_kernel__maximalist_reading, "Bitcoin Maximalist Covenant: Immutable Whitepaper Monetary Policy").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__maximalist_reading, "cryptoeconomics/consensus/monetary_systems").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__maximalist_reading, '90827b46-084d-405a-bc83-13b81bf964d7').
narrative_ontology:cs_kernel_codification('90827b46-084d-405a-bc83-13b81bf964d7', fixed_text).
narrative_ontology:cs_authority_grounding('90827b46-084d-405a-bc83-13b81bf964d7', lineage).
narrative_ontology:cs_interpretation_layer_present('90827b46-084d-405a-bc83-13b81bf964d7').
narrative_ontology:cs_reading_relation('90827b46-084d-405a-bc83-13b81bf964d7', bitcoin_consensus_kernel__pragmatic_synthesis, influences).
narrative_ontology:cs_reading_relation('90827b46-084d-405a-bc83-13b81bf964d7', bitcoin_consensus_kernel__utility_reading, forecloses).
narrative_ontology:cs_axiom('90827b46-084d-405a-bc83-13b81bf964d7', foundational, monetary_covenant_immutability).
narrative_ontology:cs_axiom_status(monetary_covenant_immutability, holdable).
narrative_ontology:cs_axiom_grounding('90827b46-084d-405a-bc83-13b81bf964d7', monetary_covenant_immutability, deontological).
narrative_ontology:cs_axiom('90827b46-084d-405a-bc83-13b81bf964d7', secondary, early_adopter_scarcity_guarantee).
narrative_ontology:cs_axiom_status(early_adopter_scarcity_guarantee, holdable).
narrative_ontology:cs_axiom_grounding('90827b46-084d-405a-bc83-13b81bf964d7', early_adopter_scarcity_guarantee, instrumental).
narrative_ontology:cs_reference_frame('90827b46-084d-405a-bc83-13b81bf964d7', satoshi_covenant_framework).
narrative_ontology:cs_drift_state('90827b46-084d-405a-bc83-13b81bf964d7', contemporary_institutional_adoption, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('90827b46-084d-405a-bc83-13b81bf964d7', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, early_adopters).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, monetary_maximalists).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, hodl_incentive_structure).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, layer2_scalability_developers).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, protocol_innovators).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, governance_flexibility_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Acquired substantial Bitcoin holdings before protocol maturation. The immutability covenant protects their holdings against dilution through new supply or fundamental protocol redesign that would alter scarcity guarantees. They benefit from the constraint's enforcement because it prevents any monetary expansion or consensus change that would devalue early-acquired coins relative to the 21-million hard cap.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, early_adopters, beneficiary,
    organized, generational, arbitrage, global).

% Operate nodes, mining pools, and governance infrastructure. They actively defend the immutability reading through community consensus mechanisms, code review gatekeeping, and ideological messaging. They benefit by maintaining Bitcoin's positioning as 'digital gold' with unchangeable monetary properties—a narrative that justifies their authority as custodians of the original vision.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, monetary_maximalists, beneficiary,
    institutional, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__maximalist_reading, monetary_maximalists, agenda_setter).

% Propose technical improvements, optimizations, or feature additions that require changes to consensus rules. The immutability covenant constrains their design space: any innovation requiring base-layer modification must overcome the maximalist veto, even if technically sound and backward-compatible. They bear the cost of slower innovation cycles and must route proposals through hostile governance processes.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, protocol_innovators, payer,
    moderate, biographical, constrained, global).

% Build Lightning Network, sidechains, rollups, and other scaling solutions. The immutability covenant forces scalability to live outside the base layer—a structural constraint that limits their design options and increases user complexity. Any proposal for base-layer scaling changes (block size, UTXO model, signature schemes) meets maximalist resistance grounded in the covenant, even when such changes could improve efficiency.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, layer2_scalability_developers, payer,
    moderate, biographical, mobile, global).

% Include developers, institutional adopters, and cryptocurrency platforms who want Bitcoin governance to permit protocol adaptation, parameter tuning, or community override mechanisms for edge cases. The covenant forecloses their agenda: the immutability reading treats any such flexibility as a violation of founding principle, not as legitimate governance evolution. They are structurally excluded from meaningful influence on base-layer rules.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, governance_flexibility_advocates, payer,
    powerful, biographical, mobile, global).

% The Bitcoin Whitepaper (Satoshi Nakamoto, 2008) is the kernel being read. It specifies a 21-million coin hard cap, a block subsidy halving schedule, and a Proof-of-Work consensus mechanism. The document itself is immutable; what varies is how the community interprets its normative force—whether it establishes a binding monetary covenant or a minimum viable specification.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, whitepaper_authority, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(bitcoin_consensus_kernel__maximalist_reading, whitepaper_authority).

% Ethereum, Solana, Polkadot, and other layer-1 blockchains offer governance models that permit protocol changes through community voting or formal upgrade processes. The maximalist covenant's enforcement prevents Bitcoin from adopting similar flexibility, which keeps Bitcoin differentiated (and locked into immutability) but excludes these alternative governance approaches from Bitcoin's consensus layer.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, competing_layer1_systems, excluded,
    institutional, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_consensus_kernel__maximalist_reading, early_adopters).
narrative_ontology:fixing_cost_class(bitcoin_consensus_kernel__maximalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared, verifiable, and trustless record of monetary supply: the 21-million hard cap and halving schedule are embedded in consensus code that all participants verify, eliminating the need for a central authority to enforce monetary policy. Solves the coordination problem of achieving consensus on scarcity without institutional intermediation.
% TRANSFER_FUNCTION: Transfers opportunity cost from protocol innovators and scalability developers to early adopters and maximalist governance gatekeepers: monetary policy immutability forecloses design options and innovation pathways that could improve protocol efficiency or functionality, ensuring Bitcoin's monetary properties remain unchanged and early holders' scarcity guarantees never diminish.
% ABSENT_VOICES: Miners and node operators facing obsolescence from ASIC centralization would argue for retargeting difficulty or changing proof-of-work; users frustrated by base-layer transaction limits would push for block size increases; developers pursuing smart-contract functionality or privacy upgrades face gatekeeping from the immutability reading. These voices exist but are structurally excluded from base-layer governance because the covenant pre-adjudicates their proposals as violations.
% DISAPPEARANCE_RATIONALE: If the immutability covenant vanished and Bitcoin became a normal protocol subject to community governance votes, the entire incentive structure would shift: developers could propose and implement optimizations, scalability improvements could move to the base layer where architectural constraints allow them, mining incentives could be rebalanced, and the narrative positioning Bitcoin as 'digital gold' with unchangeable monetary properties would dissolve. The protocol would reorganize around governance flexibility rather than immutability doctrine.
% FOUNDING_PROBLEM: Early digital currencies required either a trusted central issuer to prevent double-spending and enforce monetary policy, or a mechanism allowing any participant to verify scarcity and supply without institutional mediation. The Whitepaper solves this by embedding the monetary schedule in the consensus protocol itself, making supply enforcement mathematically verifiable.
% FOUNDING_PROBLEM_CORROBORATION: Cypherpunk historians and protocol engineers outside the maximalist faction attest the founding problem was the *mechanism* for decentralized consensus, not the *immutability* of parameters—the Whitepaper specifies a working system, not a constitutional contract. Early Bitcoin discussions (Satoshi's forum posts, the Cypherpunk mailing list) show openness to parameter changes if consensus supported them; immutability emerged as a community doctrine later, not as Satoshi's explicit founding mandate. The maximalist reading is a later interpretive superposition, not a transparent reading of textual intent.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__maximalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__maximalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__maximalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bitcoin_consensus_kernel__maximalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_consensus_kernel__maximalist_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.81) and rising because the constraint's persistence depends on preventing base-layer changes that could improve efficiency, scalability, or functionality—innovation cost is borne by those who cannot route around it. Suppression is moderate-to-high (0.67) because enforcing immutability requires active gatekeeping: core developers must reject proposals, mining pools must signal resistance, node operators must coordinate on refusing blocks that violate the covenant. Theater is moderate (0.52) and rising: the foundational coordination function (decentralized scarcity verification) is real, but an increasing share of enforcement energy goes to theatrical defense of the covenant against proposals that would not harm that core function. The three measurement series run on one shared time grid (interval 0–16, representing roughly 2013–2021 historical trajectory, 2021–2030 projected). The rising theater ratio tracks the emergence of new scaling solutions (Lightning, sidechains) that prove base-layer immutability is not necessary for the coordination function, yet maximalist gatekeeping persists—theater increases as real coordination function decouples from the constraint's enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the maximalist agenda-setter seat (institutional power, arbitrage exit), the constraint is necessary coordination: Bitcoin's value and adoption story depend on the 21-million cap being truly immutable, and any governance flexibility would enable regulatory capture or dilution. From the protocol-innovator seat (moderate power, constrained exit), the same constraint is extractive gatekeeping: the 21-million cap is already enforced by hashing power and game theory; immutability doctrine adds no coordination value but blocks improvements that could benefit users and the broader ecosystem. The engine computes these divergent seat types from the structural data: early_adopters and monetary_maximalists derive d toward beneficiary; protocol_innovators and scalability developers derive d toward target. The authored claim (tangled_rope) reflects the real coordination function; the metrics reflect substantial extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (early_adopters, monetary_maximalists) benefit from the constraint's enforcement because it prevents dilution and ensures their narrative control over Bitcoin's identity is not challenged by governance votes. Victims (layer2_developers, protocol_innovators) pay through constrained design space, slower innovation, and repeated defeat in governance disputes where their proposals are pre-adjudicated as covenant violations. Early_adopters have arbitrage exit (they can move to alternative cryptocurrencies or off-chain assets) so d≈0.1; monetary_maximalists have mobile exit (they can fork or exit if the covenant is violated) so d≈0.15. Layer2_developers and protocol_innovators have constrained exit: they cannot leave Bitcoin entirely without abandoning accumulated protocol knowledge and ecosystem relationships, so d≈0.75–0.85.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits a living mandatrophy: the founding problem (create decentralized consensus on monetary supply) was solved by the protocol specification itself, not by the immutability doctrine. The whitepaper specifies the mechanism; enforcement of the 21-million cap comes from hashing power and game theory, not from an immutability covenant. Yet the constraint persists and strengthens because maximalist ideology—and early-adopter financial interest—have fused with the memory of the founding problem. The measured theater_ratio rise (0.28→0.52) indicates the mandatrophy: as technical competence grew and alternative scaling solutions proved viable, the need for base-layer immutability should have diminished, but enforcement effort instead intensified. The constraint survives not because it solves the original problem better, but because institutional actors benefit from preventing protocol evolution. Tangled Rope classification holds: real coordination exists, but extraction has accumulated on top of it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    covenant_vs_specification_ambiguity,
    'Does the Whitepaper establish a binding constitutional covenant, or a working specification that the community can modify if consensus changes?',
    'Textual analysis of Satoshi''s contemporaneous writings and forum discussions; comparison with how the cypherpunk tradition treated other protocols; historical reconstruction of whether early Bitcoin participants treated the 21-million cap as inviolable or as a default parameter subject to change.',
    'If the whitepaper is a covenant, the maximalist reading is justified and any base-layer change is a fundamental betrayal. If it is a specification, the constraint is a doctrine imposed by later readers, making it extractive rather than coordinative. This is the fundamental boundary between the maximalist, pragmatic, and utility readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(covenant_vs_specification_ambiguity, conceptual, 'Whether the Whitepaper is a binding constitutional covenant or a minimum viable specification.').

omega_variable(
    necessity_of_immutability_for_coordination,
    'Is base-layer monetary immutability necessary for decentralized consensus, or can layer-2 solutions and game-theoretic incentives provide sufficient commitment without a constitutional covenant?',
    'Empirical observation of Lightning Network and sidechain maturation: if these scaling solutions succeed without base-layer changes, immutability is not necessary for the coordination function it claims to serve.',
    'If immutability is not necessary, the constraint becomes pure extraction masked by coordination rhetoric. If it is necessary, the constraint''s enforced gatekeeping serves genuine coordination needs. Current evidence (circa 2026) suggests immutability is not necessary; this omega tracks whether maximalist refusal of base-layer changes remains justified by coordination logic or has become theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_of_immutability_for_coordination, empirical, 'Whether base-layer immutability is necessary for the coordination function or whether layer-2 solutions provide sufficient commitment.').

omega_variable(
    institutional_capture_of_maximalism,
    'Is the maximalist reading maintained because it genuinely solves a coordination problem, or because early adopters and mining pools benefit from preventing governance flexibility that would dilute their leverage and holdings?',
    'Analysis of decision-making incentives: where maximalist gatekeeping protects the coordination function (e.g., preventing 51% dilution of the hard cap), versus where it protects financial interests (e.g., blocking efficiency improvements that would reduce early-adopter relative advantage). Cross-referenced with governance voting patterns and fork behavior.',
    'If institutional capture is the primary driver, the constraint should be reclassified as snare (pure extraction with coordination as cover story). If genuine coordination needs are primary, tangled rope classification holds. This omega addresses the slot-machine problem: maximalists claim immutability is necessary, but the real enforcement targets any base-layer change, even those that would preserve monetary policy while improving efficiency.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_capture_of_maximalism, empirical, 'Whether the maximalist constraint is maintained by genuine coordination necessity or by institutional capture.').

omega_variable(
    suppression_structurality_internalization,
    'Is the measured suppression (0.67) structural—external barriers like code review gatekeeping and mining pool coordination—or internalized—protocol developers have absorbed the ideology that base-layer changes are illegitimate?',
    'Post-exit analysis: if developers who leave Bitcoin for other layer-1 systems continue to oppose base-layer modifications (suggesting internalized covenant belief), suppression is partially internalized. If they freely design governance-flexible protocols (suggesting the suppression was structural), then removing the gatekeeping reveals the suppression was external.',
    'If suppression is internalized, the constraint persists even if institutional gatekeeping is removed (developers have fused their identity with the covenant). If structural, removing gatekeeping would liberate blocked innovation. The distinction informs remedies and predicts post-governance-change behavior.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structurality_internalization, empirical, 'Whether suppression of base-layer innovation is structural (external gatekeeping) or internalized (developers believe covenant is legitimate).').

omega_variable(
    kernel_reading_divergence_scope,
    'Do the maximalist, pragmatic, and utility readings represent genuinely distinct logical positions, or are they stages of the same dispute with the maximalist reading foreclosing the others within a single coherent framework?',
    'Formal analysis of axiom compatibility: do the three readings'' foundational claims contradict each other (suggesting foreclosure), or do they coexist as live options held by different parties (suggesting coexistence)?',
    'If maximalism forecloses the pragmatic and utility readings, this constraint''s persistence depends on preventing alternatives from gaining institutional legitimacy; if they coexist, the constraint''s strength comes from majority coalition control, not logical necessity. Coexistence suggests the constraint could be challenged by coalition realignment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_divergence_scope, conceptual, 'Whether the maximalist reading logically forecloses other kernel readings or coexists with them as live options.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__maximalist_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(bitc_tr_t2, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 2, 0.35).
narrative_ontology:measurement(bitc_tr_t4, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 4, 0.42).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 6, 0.46).
narrative_ontology:measurement(bitc_tr_t8, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 8, 0.49).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 12, 0.51).
narrative_ontology:measurement(bitc_tr_t16, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 16, 0.52).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(bitc_be_t2, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 2, 0.54).
narrative_ontology:measurement(bitc_be_t4, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 4, 0.61).
narrative_ontology:measurement(bitc_be_t6, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(bitc_be_t8, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 8, 0.74).
narrative_ontology:measurement(bitc_be_t12, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 12, 0.78).
narrative_ontology:measurement(bitc_be_t16, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 16, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(bitc_su_t2, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 2, 0.52).
narrative_ontology:measurement(bitc_su_t4, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 4, 0.58).
narrative_ontology:measurement(bitc_su_t6, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 6, 0.61).
narrative_ontology:measurement(bitc_su_t8, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 8, 0.64).
narrative_ontology:measurement(bitc_su_t12, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 12, 0.66).
narrative_ontology:measurement(bitc_su_t16, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 16, 0.67).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__maximalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(bitcoin_consensus_kernel__maximalist_reading, 0.18).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__pragmatic_synthesis).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__utility_reading).

% DUAL FORMULATION NOTE:
% The bitcoin_consensus_kernel family decomposes into three constraint stories, each instantiating a different reading of the Whitepaper's normative force. The maximalist reading (this story) treats the specification as inviolable covenant; the pragmatic synthesis splits coordination layers to accommodate both immutability and innovation; the utility reading treats the specification as a minimum viable mechanism. Each reading has distinct ε, beneficiary/victim structure, and classification. The maximalist and utility readings likely foreclose each other's core premises (covenant vs. specification); the pragmatic synthesis attempts to coexist with both. The three stories are linked via network.affects_constraints to model their structural interdependence: maximalist gatekeeping directly constrains the design space that pragmatic and utility readings operate in.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_consensus_kernel__maximalist_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
