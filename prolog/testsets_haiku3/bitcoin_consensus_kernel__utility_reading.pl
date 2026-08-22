% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__utility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Bitcoin Whitepaper as Iterative Consensus Kernel (Utility Reading)
 *   domain: cryptoeconomics/monetary_systems
 *
 * SUMMARY:
 *   Bitcoin's whitepaper is a contested kernel. The utility reading
 *   interprets it as establishing a minimum viable consensus mechanism that
 *   enables and legitimizes iterative protocol improvement. Under this
 *   reading, the whitepaper solved the founding problem of decentralized
 *   consensus; the reading permits soft forks, layer-two protocols, and
 *   technical evolution as long as the core guarantees (decentralization,
 *   absence of central authority, hardness of the monetary policy floor) are
 *   preserved. This reading is distinguished from the maximalist reading
 *   (which treats the whitepaper as immutable monetary law) and the pragmatic
 *   synthesis (which permits iteration only at upper layers while treating
 *   the base layer as fixed). The utility reading reflects the actual
 *   dominant interpretation among protocol developers, layer-two builders,
 *   and much of the adopter ecosystem. Moderate extractiveness (0.48)
 *   reflects the cost borne by monetarist holders who lose the guarantee of
 *   immutability.
 *
 * KEY AGENTS:
 *   - Protocol developers: organize consensus on what counts as legitimate iteration within the whitepaper frame; propose and implement soft forks
 *   - Layer-two builders: depend on base-layer evolution to implement efficient layer-two protocols; benefit from the iteration-permitting reading
 *   - Adopter ecosystem: benefit from scaling and privacy improvements enabled by protocol evolution; their network-effects power grows with iteration
 *   - Monetary maximalist holders: bear the cost of losing the immutability guarantee; constrained exit to forks (Bitcoin Cash) that they view as the 'true' Bitcoin
 *   - Mining ecosystem: economically agnostic but structurally powerful; observe the constraint rather than drive it
 *   - Maximalist reading community: excluded from agenda-setting but maintain a competing interpretation; identity-locked to the immutability frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__utility_reading, 0.48).
domain_priors:suppression_score(bitcoin_consensus_kernel__utility_reading, 0.31).
domain_priors:theater_ratio(bitcoin_consensus_kernel__utility_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__utility_reading, rope).
narrative_ontology:human_readable(bitcoin_consensus_kernel__utility_reading, "Bitcoin Whitepaper as Iterative Consensus Kernel (Utility Reading)").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__utility_reading, "cryptoeconomics/monetary_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__utility_reading, 'be369fab-3a3a-41f2-aca4-d0fd6d2ba0ed').
narrative_ontology:cs_kernel_codification('be369fab-3a3a-41f2-aca4-d0fd6d2ba0ed', fixed_text).
narrative_ontology:cs_authority_grounding('be369fab-3a3a-41f2-aca4-d0fd6d2ba0ed', distributed).
narrative_ontology:cs_reading_relation('be369fab-3a3a-41f2-aca4-d0fd6d2ba0ed', bitcoin_consensus_kernel__maximalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('be369fab-3a3a-41f2-aca4-d0fd6d2ba0ed', bitcoin_consensus_kernel__pragmatic_synthesis, influences).
narrative_ontology:cs_axiom('be369fab-3a3a-41f2-aca4-d0fd6d2ba0ed', foundational, consensus_mechanism_enables_evolution).
narrative_ontology:cs_axiom_status(consensus_mechanism_enables_evolution, holdable).
narrative_ontology:cs_axiom_grounding('be369fab-3a3a-41f2-aca4-d0fd6d2ba0ed', consensus_mechanism_enables_evolution, instrumental).
narrative_ontology:cs_axiom('be369fab-3a3a-41f2-aca4-d0fd6d2ba0ed', foundational, monetary_policy_floor_immutable).
narrative_ontology:cs_axiom_status(monetary_policy_floor_immutable, holdable).
narrative_ontology:cs_axiom_grounding('be369fab-3a3a-41f2-aca4-d0fd6d2ba0ed', monetary_policy_floor_immutable, deontological).
narrative_ontology:cs_axiom('be369fab-3a3a-41f2-aca4-d0fd6d2ba0ed', secondary, soft_forks_preserve_covenant).
narrative_ontology:cs_axiom_status(soft_forks_preserve_covenant, holdable).
narrative_ontology:cs_axiom_grounding('be369fab-3a3a-41f2-aca4-d0fd6d2ba0ed', soft_forks_preserve_covenant, empirically_contingent).
narrative_ontology:cs_reference_frame('be369fab-3a3a-41f2-aca4-d0fd6d2ba0ed', whitepaper_as_minimum_viable_specification).
narrative_ontology:cs_drift_state('be369fab-3a3a-41f2-aca4-d0fd6d2ba0ed', post_taproot_adoption, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('be369fab-3a3a-41f2-aca4-d0fd6d2ba0ed', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, protocol_developers).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, adopter_ecosystem).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, layer_two_builders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, monetary_maximalist_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Propose and implement consensus-layer changes through soft forks, taproot upgrades, segwit adoption, and other backward-compatible protocol improvements. They interpret the whitepaper as establishing a minimum viable consensus mechanism that must evolve to remain competitive and secure. Their legitimacy rests on demonstrating network consensus through miner/node adoption.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, protocol_developers, agenda_setter,
    organized, generational, mobile, global).

% Users, merchants, exchanges, and institutions adopting Bitcoin. They benefit from protocol evolution that improves scalability, privacy, and security without requiring full replacement of the consensus base. Their choice set includes which version of the chain to follow; their power grows with network effects and liquidity.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, adopter_ecosystem, beneficiary,
    organized, biographical, mobile, global).

% Developers of the Lightning Network, Stacks, and other layer-two protocols that extend Bitcoin's capability without modifying the base consensus. They benefit from soft-fork improvements to the base layer (e.g., taproot enabling schnorr signatures for more efficient channels) and from the whitepaper reading that permits iteration. They operate with substantial exit optionality: layer-two protocols can fork, migrate to alternative chains, or establish independent value.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, layer_two_builders, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__utility_reading, layer_two_builders, agenda_setter).

% Holders who view Bitcoin's value as grounded in immutability of the monetary policy and perceive protocol evolution as dilution of that guarantee. They bear the cost of accepting that the whitepaper is a starting specification, not a final treaty. Their exit is to sell or migrate to a fork (e.g., Bitcoin Cash, which they view as adhering to the 'true' whitepaper), but switching entails leaving the largest network and liquidity. Their identity as 'monetary maximalists' is constitutively bound to the immutability claim.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, monetary_maximalist_holders, payer,
    moderate, biographical, identity_locked, global).

% Large mining pools and specialized mining operations. They are economically agnostic about protocol direction — they follow hashpower incentives and network value. Under this reading, they implement what the consensus decides, but their enormous capital investments give them effective veto over contentious changes. They observe the constraint rather than drive it.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, mining_ecosystem, observer,
    institutional, biographical, arbitrage, global).

% Adherents of the maximalist reading (the sibling constraint) who believe the whitepaper establishes immutable monetary policy and view soft forks as violations of the founding covenant. They are excluded from agenda-setting in this reading's framework, though they argue vociferously against it. Their identity as 'true Bitcoin believers' is constitutively bound to the immutability claim; exit would require abandoning that identity frame.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, maximalist_reading_community, excluded,
    moderate, generational, identity_locked, global).

% Adherents of the pragmatic-synthesis reading who accept that the base layer is immutable but the upper layers (layer-two protocols, applications, economics) are the true innovation frontier. Under this reading, they operate in a middle position: they permit base-layer iteration where they see technical necessity, but frame it as 'legitimate bug-fixing' not 'protocol redesign' to preserve the immutability guarantee as a limit-condition. They influence but do not fully guide this reading's framing.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, pragmatic_synthesis_community, observer,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_consensus_kernel__utility_reading, protocol_developers).
narrative_ontology:fixing_cost_class(bitcoin_consensus_kernel__utility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The whitepaper establishes a minimum viable proof-of-work consensus mechanism enabling a decentralized ledger. The utility reading frames the whitepaper itself as a founding specification that must evolve to solve emerging coordination problems: scaling, privacy preservation, and resistance to future cryptographic threats. Protocol developers coordinate on what 'evolution within the whitepaper' means — they solve the collective-action problem of upgrading the system without fragmenting the network.
% TRANSFER_FUNCTION: Moves legitimacy and governance authority from the static text of the whitepaper to the community consensus process that interprets it. Stakeholders who view the whitepaper as a final monetary constitution bear the cost of accepting that their reading no longer governs unilaterally; stakeholders who benefit from iterative protocol improvement gain the ability to propose and implement changes. Layer-two builders receive improved base-layer capabilities.
% ABSENT_VOICES: Closed-loop maximalist reading holders and those bound by the immutability frame are excluded from shaping this reading's interpretation. Holders of alternative cryptocurrencies (which implement different consensus models) are structurally absent from Bitcoin's consensus process. Future stakeholders whose interests depend on protocol stability cannot directly voice concerns about the pace or direction of iteration.
% DISAPPEARANCE_RATIONALE: If this reading of the whitepaper disappeared and the maximalist reading became dominant, Bitcoin's protocol would be constrained to the original technical specification with no soft forks, taproot, or segwit adoption. Layer-two protocols would lose efficiency gains from base-layer improvements. If instead the pragmatic synthesis became the single dominant reading, the boundary between 'legitimate base-layer bug-fixes' and 'true protocol changes' would sharpen, constraining the scope of consensus-layer iteration. The reading structures what kinds of changes the community can collectively permit.
% FOUNDING_PROBLEM: Bitcoin's whitepaper describes a proof-of-work consensus mechanism for a decentralized ledger with a fixed monetary policy and immutable rules. The founding problem was establishing a system that cannot be surreptitiously changed by any single authority — a trust-minimized ledger. The utility reading interprets this as: the whitepaper solved the founding problem of establishing consensus, but the consensus mechanism itself must be capable of evolving to address emergent scaling, privacy, and security problems that the original design did not anticipate.
% FOUNDING_PROBLEM_CORROBORATION: Satoshi Nakamoto's whitepaper and early writings on Bitcoin discuss the design as a proof-of-concept and acknowledge limitations (e.g., 'Simplified Payment Verification' for future scalability). Protocol developers who implemented taproot, segwit, and other soft forks attest that these changes preserve the founding guarantee (decentralization, immutability of the monetary policy, trustlessness) while solving emergent coordination problems. Independent analyses of layer-two protocols (Lightning Network research, academic cryptography papers) corroborate that iterative protocol improvement enables scaling without sacrificing the core innovation. Maximalist reading holders dispute this, arguing that any change violates the covenant; their corroboration of the 'immutability is the founding problem' reading comes from within the maximalist community, not independent observation.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__utility_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__utility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__utility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.48) and rising modestly over the interval (0.35 → 0.48) because the utility reading extracts a cost from those who view the whitepaper as a final monetary covenant: they lose the guarantee of immutability. The rise reflects the cumulative effect of protocol changes (segwit, taproot, potential future changes like UTXO set improvements) that narrow the maximalist reading's empirical footprint. Theater is low (0.22) because the actual function — enabling iterative consensus-based protocol improvement — is the real stated purpose; enforcement is minimal because consensus is earned through adoption and hashpower, not coercion. Suppression is also low (0.31) because the reading itself explicitly permits dissent: those who reject it can fork (Bitcoin Cash, Bitcoin SV) or stick with older protocol versions. The reading's legitimacy does not rest on suppressing the maximalist argument; rather, the utility reading's dominance reflects network-effects and adoption decisions by the majority coalition. Accessibility collapse is moderate (0.62) because once you understand that the whitepaper is interpretable as a living specification (not a final constitution), the perceived alternatives shift: you can adopt the maximalist reading by joining Bitcoin Cash, or adopt the pragmatic synthesis by acknowledging base-layer bounds. The collapse is not complete because the maximalist reading remains available and identity-coherent for its adherents.
 *
 * PERSPECTIVAL GAP:
 *   Protocol developers and layer-two builders experience this reading as pure coordination — a solution to the problem of how to upgrade the system without fragmenting it. Monetary maximalist holders experience it as extraction — the loss of the immutability guarantee they paid (in early adoption risk) to secure. The engine should compute this divergence from the structural data: maximalist holders are identity-locked to the immutability frame (exit_options = identity_locked), which amplifies directionality toward the payer end; protocol developers operate with mobile exit (they can propose forks or join alternative projects), which dampens directionality toward the beneficiary end. The same reading produces different effective extraction for different seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Protocol developers (power=organized, exit=mobile) are beneficiaries at the low-d end: they can propose changes, adopt the reading if it serves their interests, or migrate to other projects. Adopter ecosystem (power=organized, exit=mobile) benefits from iteration and has arbitrage-grade exit: they can switch to alternative L1 platforms (Ethereum, Solana) if Bitcoin's direction diverges from their preferences. Layer-two builders (power=powerful, exit=mobile) explicitly gain efficiency from base-layer soft forks and operate with full arbitrage exit. Monetary maximalist holders (power=moderate, exit=constrained → identity_locked) bear the cost of accepting a reading they did not choose: their exit requires either selling (liquidity-destructive) or migrating to a fork they view as less-valuable (network-effects-costly). The identity_locked exit is the key: they cannot exit without dissolving their identity as 'Bitcoin maximalists' — they are trapped in a cognitive frame the reading undermines.
 *
 * MANDATROPHY ANALYSIS:
 *   Under the utility reading, the mandate is explicit: enable iterative consensus-based protocol improvement to solve emerging coordination problems (scaling, privacy, security). This reading does NOT suffer from mandatrophy because the coordination function (enabling upgrades without fragmenting the network) is alive and actively served by the mechanism (soft forks, consensus signaling, adoption-based legitimacy). The maximalist reading, by contrast, would suffer mandatrophy if it became dominant and tried to enforce immutability: the founding problem (establishing trustless consensus) would be solved, but the enforcement mechanism (rejecting all protocol changes) would persist, producing a zombie constraint. The utility reading is not zombie; it is actively contested and functions as stated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    immutability_vs_evolution_boundary,
    'Where is the boundary between ''legitimate iterative improvement within the whitepaper'' and ''violation of the founding monetary covenant''? What counts as preserving the ''core guarantee'' vs. changing it?',
    'Empirical: if a proposed hard fork (e.g., increasing the supply cap or changing the difficulty algorithm) gains consensus adoption, the community''s behavior reveals where the boundary actually lies. Conceptual: formal analysis of which protocol properties are semantically essential to ''being Bitcoin'' vs. implementational details.',
    'If the boundary is empirically determined by adoption consensus, the utility reading is correct: evolution is legitimate as long as the community agrees. If the boundary is conceptually fixed (certain properties are intrinsically immutable), the maximalist reading gains force, and the utility reading overreaches.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immutability_vs_evolution_boundary, conceptual, 'The contested semantic boundary between protocol evolution and covenant violation.').

omega_variable(
    network_effects_vs_reading_truth,
    'Does the dominance of the utility reading reflect that it is the correct interpretation of the whitepaper, or does it reflect network-effects power accumulating behind the largest protocol-development coalition?',
    'Methodological: compare the dominance of the reading to the quality of the corroborating evidence from outside the dominant coalition (independent economists, cryptographers, academic analyses). Historical: trace the contingency points where the reading could have lost (the 2015 block-size debate, the 2017 scaling wars) and ask whether the outcome was overdetermined by the technical argument or by political organizing.',
    'If the reading''s dominance is overdetermined by the technical argument, the utility reading is robust. If dominance reflects political organizing and path-dependence, the reading is contingent and vulnerable to regime change (e.g., mining-coalition shift, regulatory capture, technical catastrophe that revives immutability demands).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(network_effects_vs_reading_truth, preference, 'Whether reading dominance reflects epistemic truth or institutional power.').

omega_variable(
    maximalist_reading_as_legitimacy_check,
    'Is the continued vigorous assertion of the maximalist reading (Bitcoin Cash, Bitcoin SV, hard-money advocates) a sign that the utility reading is suppressing a real alternative, or a sign that the utility reading is genuinely open and the maximalist reading simply has fewer adherents?',
    'Structural: measure suppression mechanisms preventing maximalist reading adoption (e.g., do mining pools actively refuse to work on Bitcoin Cash? Do exchanges de-list forks? Do major developers actively discourage the reading?). Compare to metrics of suppression in snare constraints. If suppression is genuine, the constraint is a snare of coordination disguised; if suppression is absent, the reading''s dominance reflects preference, not coercion.',
    'If suppression is genuine, the reading is a snare: the appearance of ''consensus-based legitimacy'' masks the fact that the maximalist reading is being forcibly excluded. If suppression is absent, the reading is a genuine rope: alternatives exist and can compete.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(maximalist_reading_as_legitimacy_check, empirical, 'Whether the constraint suppresses the maximalist reading or permits genuine alternatives.').

omega_variable(
    sibling_reading_epistemic_independence,
    'Do the three readings (utility, maximalist, pragmatic) represent genuinely independent epistemic frameworks for interpreting the whitepaper, or do they form a nested hierarchy where one reading logically implies bounds on the others?',
    'Logical analysis: map the axioms of each reading and check for entailment relationships. If axiom_A → bounds on axiom_B, the readings are not independent. Historical analysis: did the readings emerge simultaneously or sequentially? If pragmatic synthesis is a recent synthesis between maximalist and utility readings, it may be derivative rather than foundational.',
    'If the readings are independent, the kernel genuinely admits three coexisting interpretations. If one reading is logically entailed by another, the set should be reduced and the constraint family reorganized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_epistemic_independence, conceptual, 'The logical independence structure of the three kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__utility_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_consensus_kernel__utility_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(bitc_tr_t0, observed).
narrative_ontology:measurement(bitc_tr_t2, bitcoin_consensus_kernel__utility_reading, theater_ratio, 2, 0.11).
narrative_ontology:measurement_basis(bitc_tr_t2, observed).
narrative_ontology:measurement(bitc_tr_t5, bitcoin_consensus_kernel__utility_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement_basis(bitc_tr_t5, observed).
narrative_ontology:measurement(bitc_tr_t8, bitcoin_consensus_kernel__utility_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement_basis(bitc_tr_t8, observed).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_consensus_kernel__utility_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement_basis(bitc_tr_t12, observed).
narrative_ontology:measurement(bitc_tr_t15, bitcoin_consensus_kernel__utility_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement_basis(bitc_tr_t15, observed).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(bitc_be_t0, observed).
narrative_ontology:measurement(bitc_be_t2, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 2, 0.39).
narrative_ontology:measurement_basis(bitc_be_t2, observed).
narrative_ontology:measurement(bitc_be_t5, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 5, 0.43).
narrative_ontology:measurement_basis(bitc_be_t5, observed).
narrative_ontology:measurement(bitc_be_t8, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement_basis(bitc_be_t8, observed).
narrative_ontology:measurement(bitc_be_t12, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 12, 0.47).
narrative_ontology:measurement_basis(bitc_be_t12, observed).
narrative_ontology:measurement(bitc_be_t15, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement_basis(bitc_be_t15, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(bitcoin_consensus_kernel__utility_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__utility_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(bitcoin_consensus_kernel__utility_reading, 0.18).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel__maximalist_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel__pragmatic_synthesis).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, lightning_network_efficiency).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, cryptocurrency_regulatory_capture).

% DUAL FORMULATION NOTE:
% The bitcoin_consensus_kernel decomposes into three constraint stories per OQ-26 ε-invariance principle: utility_reading (this file, moderate extractiveness, permits iteration), maximalist_reading (high extractiveness, immutability as founding law), and pragmatic_synthesis (lower extractiveness, iteration at upper layers only). Each reading instantiates a different ε because the beneficiary/victim structure and the interpretation of 'legitimate change' differ structurally. They are not the same constraint viewed from different angles; they are genuinely different constraints grounded in the same text. All three are linked via network.affects_constraints to enable constraint-family analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_consensus_kernel__utility_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
