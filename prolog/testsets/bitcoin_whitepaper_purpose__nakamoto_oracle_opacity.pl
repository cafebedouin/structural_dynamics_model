% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper_purpose__nakamoto_oracle_opacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bitcoin_whitepaper_purpose__nakamoto_oracle_opacity
 *   human_readable: Bitcoin Whitepaper Purpose Under Nakamoto Oracle Opacity
 *   domain: technology_governance/distributed_systems/monetary_theory
 *
 * SUMMARY:
 *   Satoshi Nakamoto published the Bitcoin whitepaper in 2008 with an
 *   explicit title—'A Peer-to-Peer Electronic Cash System'—that frames the
 *   problem and stated purpose. He then disappeared in 2011, leaving no
 *   recorded succession plan or explicit guidance on the system's design
 *   tradeoffs (especially on-chain throughput vs. full-node verifiability).
 *   This absence created an interpretive vacuum: multiple readings of the
 *   whitepaper are structurally defensible. The protocol coalition
 *   controlling the mainline implementation claims fidelity to a
 *   store-of-value design (capped throughput, high security, settlement
 *   layer); cash-reading advocates claim fidelity to the stated
 *   electronic-cash purpose (low fees, transactional throughput). Both cite
 *   the same whitepaper text; neither can appeal to authorial clarification.
 *   The institutional power of the mainline coalition and exchange platforms
 *   creates asymmetric extraction: they benefit from the interpretive vacuum
 *   (claiming whitepaper authority without challenge), while users seeking
 *   cash functionality and competing implementations bear the cost of
 *   fragmentation and marginalization. This reading of the constraint focuses
 *   on the oracle-opacity itself—the absence of an authoritative
 *   interpreter—as the mechanism that sustains the extraction.
 *
 * KEY AGENTS:
 *   - Satoshi Nakamoto (absent oracle): authoritative interpreter eliminated by departure; leaves kernel text as sole legitimacy reference, ambiguous on key design tradeoffs
 *   - Protocol coalition (agenda_setter, beneficiary): core developers and major node operators; de facto interpretive authority in Satoshi's absence; benefits from ability to reframe whitepaper toward store-of-value design without challenge
 *   - Users seeking electronic-cash fidelity (payer): bear cost of divergence between stated purpose and implemented design; constrained exit (forking is costly)
 *   - Competing implementations (payer): claim whitepaper fidelity; bear network-effect fragmentation cost; cannot resolve interpretive contest without founder arbitration
 *   - Exchange service providers (beneficiary): profit from store-of-value narrative and price appreciation; indirectly benefit from protocol coalition's interpretive authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.62).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.71).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, extractiveness, 0.62).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, "Bitcoin Whitepaper Purpose Under Nakamoto Oracle Opacity").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, "technology_governance/distributed_systems/monetary_theory").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, '079cc11f-5a91-4348-9d6b-7ae99b830d28').
narrative_ontology:cs_kernel_codification('079cc11f-5a91-4348-9d6b-7ae99b830d28', fixed_text).
narrative_ontology:cs_authority_grounding('079cc11f-5a91-4348-9d6b-7ae99b830d28', extraction).
narrative_ontology:cs_reading_relation('079cc11f-5a91-4348-9d6b-7ae99b830d28', bitcoin_whitepaper_purpose__bitcoin_electronic_cash_reading, coexists_with).
narrative_ontology:cs_reading_relation('079cc11f-5a91-4348-9d6b-7ae99b830d28', bitcoin_whitepaper_purpose__bitcoin_store_of_value_reading, coexists_with).
narrative_ontology:cs_axiom('079cc11f-5a91-4348-9d6b-7ae99b830d28', foundational, oracle_absence_is_structural).
narrative_ontology:cs_axiom_status(oracle_absence_is_structural, holdable).
narrative_ontology:cs_axiom_grounding('079cc11f-5a91-4348-9d6b-7ae99b830d28', oracle_absence_is_structural, empirically_contingent).
narrative_ontology:cs_axiom('079cc11f-5a91-4348-9d6b-7ae99b830d28', foundational, institutional_power_fills_interpretive_vacuum).
narrative_ontology:cs_axiom_status(institutional_power_fills_interpretive_vacuum, holdable).
narrative_ontology:cs_axiom_grounding('079cc11f-5a91-4348-9d6b-7ae99b830d28', institutional_power_fills_interpretive_vacuum, empirically_contingent).
narrative_ontology:cs_reference_frame('079cc11f-5a91-4348-9d6b-7ae99b830d28', satoshi_sole_oracle_authority).
narrative_ontology:cs_drift_state('079cc11f-5a91-4348-9d6b-7ae99b830d28', contemporary_post_disappearance_institutional_coalescence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('079cc11f-5a91-4348-9d6b-7ae99b830d28', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, protocol_coalition_controlling_narrative).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, exchange_service_providers).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, users_seeking_whitepaper_fidelity).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, competing_implementations).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 to 0.62 over the interval as the protocol coalition's interpretive authority calcifies (early uncertainty about Bitcoin's purpose gives way to institutional consensus around store-of-value design). Theater ratio (0.28→0.58) captures increasing performative justification: protocol decisions are wrapped in whitepaper-fidelity rhetoric while substantively pursuing a design the whitepaper title does not support. Suppression requirement (0.48→0.71) reflects the growing cost of dissent: early forking (Bitcoin Cash at ~2017) was technically feasible but narratively illegitimate ('not the real Bitcoin'); by the interval's end, the mainline's authority is so established that fidelity challenges are dismissed as 'alt coins' without substantive engagement. Accessibility collapse is lower (0.48) because the whitepaper text is publicly available and the interpretive stakes are transparent—users can recognize the contest if they read carefully. Resistance is high (0.72) because multiple well-funded factions (competing implementations, original-purpose advocates, academic critics) actively dispute the mainline coalition's framing. The constraint persists because institutional power and network effects (exchange listings, mining concentration, developer talent) reinforce the coalition's authority despite ongoing resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the coalition's seat: 'We stewarded a sound, secure, decentralized system that achieves Bitcoin's stated goal: eliminating trusted intermediaries in value transfer. We chose settlement and security over transaction throughput because on-chain scale is not how p2p cash works.' From the cash advocate's seat: 'The whitepaper title is explicit—electronic cash—and they have systematically degraded on-chain throughput to keep their control over protocol evolution. They use 'settlement' and 'security' as code for 'limiting our power to force change.' From the researcher's seat: 'Satoshi was silent on the throughput-vs.-verifiability tradeoff, and both readings are textually defensible. The contest is genuinely unresolved, and institutional power is the only decider.' The constraint persists because the coalition's power and network effects make their reading the default, and most users never notice the contest.
 *
 * DIRECTIONALITY LOGIC:
 *   The protocol coalition sits near the beneficiary end (d~0.25): they control the narrative, extract interpretive authority, and benefit from the ability to redirect protocol development without justification. Exchange providers are also near-beneficiary (d~0.30): they profit from the store-of-value framing and have mobile exit (they could support competing implementations if the mainline faltered, but they do not because the mainline's dominance serves them). Users seeking electronic-cash function sit near-target (d~0.75): they pay the cost of interpretive marginalization (their reading is treated as technically inferior or nostalgic), have constrained exit (forking is costly), and collect no compensation. Competing implementations sit at mid-to-high target (d~0.65): they bear fragmentation costs and face constant narrative delegitimization, though they retain the arbitrage option of improving their implementations or capturing new use cases. Satoshi Nakamoto, as the absent oracle, occupies the analytical end (d=0.5 by convention for observers) but structurally should be d=0.0 (full beneficiary of the interpretive void—their absence enables the contest that keeps the system in design limbo) or d=1.0 (full target—they are the absent party everyone claims to represent). Neither mapping is clean; the analytical default captures the irrelevance of their current position, which is itself the point.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—eliminating trusted intermediaries in payment—is CONTESTED in status because the mainline implementation has become increasingly settlement-dependent on centralized exchanges and custodians (which reintroduces trusted intermediaries for actual commerce), while claiming to have solved the original problem by enabling peer-to-peer value transfer in principle. The protocol coalition would say the founding problem is LIVE: Bitcoin achieves trustless settlement, which is sufficient and more important than on-chain transactional throughput. Cash advocates would say the founding problem is DEAD as originally stated: if on-chain transactions are too expensive for everyday use, the 'peer-to-peer electronic cash' goal has been abandoned, and the system now solves a different problem (censorship-resistant settlement, store-of-value). This mandatrophy—the founding purpose and the implemented direction have diverged—is precisely what the oracle-opacity constraint enables. No one can appeal to Satoshi to say which reading is correct. The constraint sustains this ambiguity by keeping both readings simultaneously alive in the institutional and community conversation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    satoshi_authorial_intent_recovery,
    'Could Satoshi Nakamoto''s design intent be recovered from the whitepaper text, early forum posts, and code comments, independent of his explicit clarification?',
    'Hermeneutic analysis of the full corpus of Satoshi''s writings and the earliest codebase (pre-2011); consultation with cryptographers and computer scientists who worked with him or studied his writings; linguistic and historical analysis of the whitepaper''s context and contemporaneous discussions.',
    'If a coherent intent emerges clearly favoring one reading, the oracle-opacity constraint is weakened—users could appeal to recovered intent to challenge institutional reinterpretation. If the corpus reveals genuine ambiguity or even tension between the stated title and the technical design, the constraint strengthens—no recovery resolves the ambiguity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(satoshi_authorial_intent_recovery, empirical, 'Whether Satoshi''s design intent can be recovered without direct clarification from the absent author.').

omega_variable(
    oracle_replacement_impossibility,
    'Could a decentralized, non-hierarchical governance mechanism adjudicate whitepaper interpretation without recreating the oracle-dependency?',
    'Study of decentralized governance attempts in crypto projects (Dash masternodes, Ethereum governance, Cardano Voltaire era); analysis of whether consensus-based interpretation produces a different form of hierarchy or merely defers the problem.',
    'If a decentralized oracle-replacement exists, the constraint could be restructured without requiring Satoshi''s return. If not, the oracle-opacity is structurally irreducible, and the constraint is a permanent feature of systems with absent or departed founders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oracle_replacement_impossibility, conceptual, 'Whether oracle-opacity is an artifact of Bitcoin''s specific governance or an irreducible feature of decentralized systems with founding texts.').

omega_variable(
    institutional_power_vs_interpretive_legitimacy,
    'Does the protocol coalition''s interpretive authority derive from its institutional power (control of node software, exchange relationships, mining concentration) or from genuine technical superiority of the store-of-value reading?',
    'Counterfactual analysis: if a cash-reading implementation gained the same network effects and institutional support, would the store-of-value reading be dismissed as obsolete? Historical evidence from actual forks (Bitcoin Cash, Bitcoin SV) shows that even technically credible alternatives fail to dislodge the mainline despite fidelity arguments, suggesting power is the primary factor. Direct evidence would require access to internal discussions at major exchanges and development teams.',
    'If power is primary, the extraction persists because institutional asymmetry is robust to technical argument. If legitimacy is primary, demonstrating the store-of-value reading''s weakness could shift the constraint toward pure coordination (the reading is genuinely better, even if contested). The measured high theater ratio (0.58) suggests power dominates legitimacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_power_vs_interpretive_legitimacy, empirical, 'Whether institutional power or technical merit drives the protocol coalition''s interpretive authority.').

omega_variable(
    whitepaper_text_ambiguity_depth,
    'Is the whitepaper ambiguous because Satoshi was genuinely uncertain about the throughput-vs.-verifiability tradeoff, or because the document was written as a proof-of-concept and key design decisions were deferred to implementation?',
    'Close reading of the whitepaper against Satoshi''s other writings (forum posts, code comments, emails to early collaborators); analysis of whether the ambiguity appears intentional (neutral language, multiple paths explored) or accidental (silence where specification would be expected).',
    'If ambiguity is intentional, Satoshi may have been agnostic on the tradeoff and the oracle-opacity is less a loss than a recognition of Satoshi''s own openness. If deferred-design, the oracle-opacity is a gap in specification that Satoshi intended to resolve later (via code and implementation feedback). The impact on the constraint: unresolved ambiguity strengthens the constraint; deferred design suggests the oracle-opacity is temporary and recovery is possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(whitepaper_text_ambiguity_depth, conceptual, 'Whether whitepaper ambiguity is intentional or accidental, and whether Satoshi intended to resolve it later.').

omega_variable(
    extraction_mechanism_dependence_on_absence,
    'Would the measured extraction (0.62) and theater ratio (0.58) persist if Satoshi were to reappear and issue a definitive statement, even if the statement favored the store-of-value reading?',
    'Thought experiment: if Satoshi returned and said ''settlement layer, not transaction scale,'' would the coalition accept this as legitimacy closure, or would it become irrelevant if the technical and institutional design were already locked in? Analysis of how past Oracle statements (rare) were treated in Bitcoin governance.',
    'If extraction depends ON THE ABSENCE rather than on the content of the oracle''s statement, the constraint is unusually resilient—the oracle''s return might not reduce extraction at all. If extraction depends on interpretive liberty, a clear oracle statement (regardless of content) might raise extraction briefly (loss of plausible deniability) before settling it. This tests whether the oracle-opacity is a mechanism or a symptom.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_mechanism_dependence_on_absence, preference, 'Whether the extraction mechanism depends on Satoshi''s absence per se or on interpretive liberty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 0, 0.28).
narrative_ontology:measurement(bitc_tr_t2, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2, 0.35).
narrative_ontology:measurement(bitc_tr_t5, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 5, 0.45).
narrative_ontology:measurement(bitc_tr_t8, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 8, 0.54).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 12, 0.57).
narrative_ontology:measurement(bitc_tr_t15, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 15, 0.58).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bitc_be_t2, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(bitc_be_t5, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(bitc_be_t8, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 8, 0.59).
narrative_ontology:measurement(bitc_be_t12, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 12, 0.61).
narrative_ontology:measurement(bitc_be_t15, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 15, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(bitc_su_t2, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2, 0.56).
narrative_ontology:measurement(bitc_su_t5, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(bitc_su_t8, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(bitc_su_t12, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(bitc_su_t15, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 15, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, identity_coordination).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.12).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_store_of_value_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_electronic_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, cryptocurrency_consensus_governance).

% DUAL FORMULATION NOTE:
% The bitcoin_whitepaper_purpose kernel decomposes into three constraint stories, each instantiating a different reading of the founding text. The nakamoto_oracle_opacity reading (this story) focuses on the structural absence of the authoritative interpreter and the interpretive vacuum it creates. The electronic_cash_reading and store_of_value_reading stories instantiate the competing content claims. All three are linked: the oracle-opacity constraint structurally enables the persistent contest between the two content readings; neither content reading can be adjudicated without oracle return; the oracle-opacity is upstream in causal order.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
