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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: bitcoin_consensus_kernel__maximalist_reading
 *   human_readable: Bitcoin Maximalist Covenant: Immutable Monetary Policy Enforcement
 *   domain: cryptoeconomics/monetary_systems/distributed_consensus
 *
 * SUMMARY:
 *   This constraint story instantiates the maximalist reading of the
 *   bitcoin_consensus_kernel: the whitepaper is treated as an immutable
 *   monetary covenant, and any base-layer protocol change violating this
 *   covenant is socially and technically rejected as illegitimate. This
 *   reading extracts from scalability and innovation layers by permanently
 *   enshrining early design choices (block size, issuance schedule, script
 *   capabilities) while concentrating purchasing-power protection in the
 *   holder class. It is one of three readings of a contested kernel; the
 *   utility_reading treats the whitepaper as a minimum viable consensus
 *   mechanism enabling iterative improvement, and the pragmatic_synthesis
 *   separates immutable base-layer money from mutable upper-layer innovation.
 *   The claim/metric independence principle is observed: the claimed type is
 *   tangled_rope (genuine coordination in sound money, asymmetric extraction
 *   from blocked innovation) and the metrics describe high suppression,
 *   substantial extraction, and rising theater over the interval.
 *
 * KEY AGENTS:
 *   - long_term_holders: Primary beneficiary (powerful/mobile) â purchasing power protected by enforced scarcity and immutability.
 *   - early_adopters: Primary beneficiary (powerful/mobile) â low cost basis and narrative authority.
 *   - base_layer_innovators: Primary target (moderate/constrained) â blocked from protocol upgrades by covenant enforcement.
 *   - scalability_researchers: Primary target (moderate/constrained) â research agenda suppressed by immutability norm.
 *   - transaction_dependent_users: Secondary target (powerless/constrained) â bear congestion costs from unchangeable block space limits.
 *   - distributed_consensus_enforcers: Agenda setter (organized/identity_locked) â node operators and influential developers who enforce the covenant.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__maximalist_reading, 0.72).
domain_priors:suppression_score(bitcoin_consensus_kernel__maximalist_reading, 0.8).
domain_priors:theater_ratio(bitcoin_consensus_kernel__maximalist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__maximalist_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_consensus_kernel__maximalist_reading, "Bitcoin Maximalist Covenant: Immutable Monetary Policy Enforcement").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__maximalist_reading, "cryptoeconomics/monetary_systems/distributed_consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__maximalist_reading, 'f792bb54-4785-4a0d-8f6e-e2e7d48c5cb3').
narrative_ontology:cs_kernel_codification('f792bb54-4785-4a0d-8f6e-e2e7d48c5cb3', fixed_text).
narrative_ontology:cs_authority_grounding('f792bb54-4785-4a0d-8f6e-e2e7d48c5cb3', distributed).
narrative_ontology:cs_reading_relation('f792bb54-4785-4a0d-8f6e-e2e7d48c5cb3', bitcoin_consensus_kernel__utility_reading, forecloses).
narrative_ontology:cs_reading_relation('f792bb54-4785-4a0d-8f6e-e2e7d48c5cb3', bitcoin_consensus_kernel__pragmatic_synthesis, influences).
narrative_ontology:cs_axiom('f792bb54-4785-4a0d-8f6e-e2e7d48c5cb3', foundational, monetary_policy_immutability_categorical).
narrative_ontology:cs_axiom_status(monetary_policy_immutability_categorical, holdable).
narrative_ontology:cs_axiom_grounding('f792bb54-4785-4a0d-8f6e-e2e7d48c5cb3', monetary_policy_immutability_categorical, conventional).
narrative_ontology:cs_axiom('f792bb54-4785-4a0d-8f6e-e2e7d48c5cb3', secondary, covenant_violation_by_hard_fork).
narrative_ontology:cs_axiom_status(covenant_violation_by_hard_fork, holdable).
narrative_ontology:cs_axiom_grounding('f792bb54-4785-4a0d-8f6e-e2e7d48c5cb3', covenant_violation_by_hard_fork, conventional).
narrative_ontology:cs_reference_frame('f792bb54-4785-4a0d-8f6e-e2e7d48c5cb3', whitepaper_covenant_immutability).
narrative_ontology:cs_drift_state('f792bb54-4785-4a0d-8f6e-e2e7d48c5cb3', contemporary_utility_challenges, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('f792bb54-4785-4a0d-8f6e-e2e7d48c5cb3', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, long_term_holders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, early_adopters).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, base_layer_innovators).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, scalability_researchers).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, transaction_dependent_users).
narrative_ontology:constraint_vindicates(bitcoin_consensus_kernel__maximalist_reading, sound_money_doctrine).
narrative_ontology:constraint_vindicates(bitcoin_consensus_kernel__maximalist_reading, trust_minimization_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold bitcoin as a long-term store of value; benefit from enforced scarcity and the social prohibition on monetary policy changes that would dilute their holdings; exit by selling into fiat or alternative assets.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, long_term_holders, beneficiary,
    powerful, generational, mobile, global).

% Accumulated bitcoin at low cost basis and possess narrative authority within the community; benefit from the institutionalization of the maximalist story that resists changes to the early distribution or rules; exit is liquid but identity and social status are tied to the covenant.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, early_adopters, beneficiary,
    powerful, generational, mobile, global).

% Develop technical improvements to Bitcoin's scripting, privacy, or throughput; their proposals for hard or soft forks are systematically opposed by the maximalist consensus enforcement; must either abandon the base layer or route innovation to separate chains or layers.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, base_layer_innovators, payer,
    moderate, biographical, constrained, global).

% Research and advocate for base-layer capacity increases or efficiency improvements; their work is treated as an attack on the covenant; conferences and funding channels are captured by the immutability narrative, constraining their ability to affect the protocol.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, scalability_researchers, payer,
    moderate, biographical, constrained, global).

% Use bitcoin for remittances, savings, or commerce and bear high fees and congestion due to fixed block space; base-layer improvements that could lower costs are blocked by the maximalist reading; exit to altcoins is possible but network effects and legitimacy norms constrain it.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, transaction_dependent_users, payer,
    powerless, immediate, constrained, global).

% Run nodes, maintain software, and participate in social coordination to reject protocol changes that alter the monetary policy or whitepaper covenant; their operational and social identity is fused with the immutability narrative; exit means abandoning the community's core tenet.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, distributed_consensus_enforcers, agenda_setter,
    organized, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a credibly fixed monetary supply and rule-set, removing the need for trusted third parties in monetary policy and preventing arbitrary debasement or political manipulation of the money stock.
% TRANSFER_FUNCTION: Moves potential purchasing power, transaction affordability, and protocol design authority from would-be innovators and active transactors to existing holders, by permanently constraining supply and blocking base-layer changes that might alter the early social contract.
% ABSENT_VOICES: Scalability researchers, developing-nation users who need low-fee base-layer transactions, and protocol engineers advocating for iterative hard-fork upgrades are structurally excluded; their proposals are treated as attacks on the covenant rather than legitimate technical contributions.
% DISAPPEARANCE_RATIONALE: If the maximalist covenant vanished overnight, the consensus kernel would fragment into competing interpretations; hard forks altering monetary policy or block parameters would proliferate; the concentrated store-of-value property would destabilize as the scarcity promise became negotiable; and the holder class would lose the coordination advantage of a credibly fixed rule-set.
% FOUNDING_PROBLEM: Creation of a peer-to-peer electronic cash system without a trusted third party, with a predictable, censorship-resistant monetary policy immune to sovereign manipulation.
% FOUNDING_PROBLEM_CORROBORATION: The original whitepaper and early cypherpunk literature attest the founding problem from outside the current concentrated holder class. However, many early contributors who emphasized the electronic cash use case have been socially expelled or distanced from the maximalist consensus, leaving contemporary corroboration from non-beneficiaries sparse; independent monetary historians and some original developers corroborate the electronic cash framing, while the holder class emphasizes the store-of-value reframing.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__maximalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__maximalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__maximalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_consensus_kernel__maximalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_consensus_kernel__maximalist_reading, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.72) is high because the constraint permanently blocks base-layer innovation that could reduce fees or expand functionality, forcing value into existing holder positions and expensive workarounds. Suppression (0.80) is high because the persistence of the covenant depends on active social enforcement: dissenting hard forks are delegitimized, 'shitcoin' rhetoric excludes alternatives, and developers proposing changes are ostracized. Theater_ratio (0.45) reflects the performative dimension of maximalist identityâritualized whitepaper citation, purity tests, and narrative maintenance that supplements the actual protocol enforcement. Accessibility_collapse (0.75) is high because, within the maximalist framework, any alternative to the covenant is framed as a scam or attack, collapsing the perceived legitimacy of exits. Resistance (0.70) is high due to the ongoing blocksize-war legacy, continuous proposals for protocol change, and the persistence of altcoin ecosystems as exit options. Temporal measurements show extraction and suppression rising as the network's market capitalization grew and the opportunity cost of immutability increased.
 *
 * PERSPECTIVAL GAP:
 *   From the holder seat, the constraint is a protective rope: it solves the coordination problem of trustworthy scarcity without a central bank. From the innovator seat, the same structure is a snare: it captures the protocol in amber to protect early wealth positions, actively suppressing improvements that would benefit users but dilute the narrative. The engine computes this divergence from the same structural data; the authored claim does not adjudicate between the seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Long-term holders and early adopters are structural beneficiaries: the covenant subsidizes their purchasing power and cost-basis advantage by preventing dilution or competition from upgraded chains. Their directionality sits near the beneficiary pole. Base-layer innovators, scalability researchers, and transaction-dependent users are structural targets: the constraint extracts from them by permanently raising the cost of improving the system they depend on. Distributed consensus enforcers occupy an ambiguous agenda-setting positionâtheir identity is fused with the covenant, but their power is organizational rather than extractive in the personal sense; they enforce the directionality distribution rather than occupying a clear target or beneficiary pole.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the constraint as pure coordination (rope) or pure extraction (snare). A rope reading would ignore the identifiable victimsâblocked innovators and high-fee usersâwho bear costs from the same structure that coordinates holders. A snare reading would ignore the genuine coordination function: the immutability does solve the hard problem of credible monetary policy, and holders are net beneficiaries of that stability, not merely rentiers extracting from a fake coordination story. The tangled_rope classification captures both the sound-money coordination and the asymmetric extraction against change.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_covenant_vs_living_document,
    'Does the Bitcoin whitepaper function as an immutable founding covenant or as a minimum viable specification subject to iterative refinement?',
    'Historical analysis of Satoshi''s post-release communications and the original codebase''s mutability; sociological mapping of when the ''covenant'' framing crystallized versus the ''utility'' framing.',
    'If the whitepaper was intended as a living specification, the maximalist reading is a retroactive extraction layer; if it was intended as a covenant, the utility reading is the extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_covenant_vs_living_document, conceptual, 'Whether the whitepaper is a fixed covenant or a mutable specification.').

omega_variable(
    immutability_extraction_or_protection,
    'Does base-layer immutability protect monetary stability for all participants, or does it extract surplus from subsequent innovation layers to benefit early holders?',
    'Comparative analysis of holder concentration versus innovation suppression; measurement of developer attrition and fork legitimacy premium.',
    'If extraction exceeds protection, the constraint computes as tangled rope or snare rather than rope; if protection dominates, it shifts toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immutability_extraction_or_protection, empirical, 'Whether immutability extracts from innovation or protects coordination.').

omega_variable(
    social_enforcement_internalization,
    'Is the suppression of protocol-change proposals achieved through structural protocol rules alone, or through internalized identity fusion with the whitepaper covenant?',
    'Post-exit behavior of former Bitcoin Core developers who proposed changes: do they continue to resist protocol evolution after leaving the project?',
    'If internalized, effective suppression is higher than structural measures suggest and the constraint exhibits identity-coordination extraction dynamics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(social_enforcement_internalization, empirical, 'Structural versus internalized suppression in the maximalist community.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__maximalist_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(btc_max_tr_t0, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(btc_max_tr_t3, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 3, 0.2).
narrative_ontology:measurement(btc_max_tr_t6, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement(btc_max_tr_t9, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 9, 0.38).
narrative_ontology:measurement(btc_max_tr_t12, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 12, 0.42).
narrative_ontology:measurement(btc_max_tr_t15, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 15, 0.45).

% Extraction over time
narrative_ontology:measurement(btc_max_be_t0, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(btc_max_be_t3, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 3, 0.25).
narrative_ontology:measurement(btc_max_be_t6, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 6, 0.4).
narrative_ontology:measurement(btc_max_be_t9, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 9, 0.55).
narrative_ontology:measurement(btc_max_be_t12, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 12, 0.65).
narrative_ontology:measurement(btc_max_be_t15, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 15, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(btc_max_su_t0, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(btc_max_su_t3, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 3, 0.35).
narrative_ontology:measurement(btc_max_su_t6, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(btc_max_su_t9, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 9, 0.75).
narrative_ontology:measurement(btc_max_su_t12, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 12, 0.78).
narrative_ontology:measurement(btc_max_su_t15, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 15, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__utility_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__pragmatic_synthesis).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the bitcoin_consensus_kernel. The maximalist reading claims high extractiveness against protocol changes; the utility reading claims low extractiveness with iterative improvement; the pragmatic_synthesis splits the difference. They share the same underlying protocol history but instantiate different structural claims about the legitimacy of change.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
