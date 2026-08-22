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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: bitcoin_whitepaper_purpose__nakamoto_oracle_opacity
 *   human_readable: Bitcoin Whitepaper Purpose Interpretation Under Oracle Opacity
 *   domain: technology/monetary theory/governance
 *
 * SUMMARY:
 *   Satoshi Nakamoto published the Bitcoin whitepaper in 2008 and remained
 *   actively involved in protocol development until 2010, then disappeared in
 *   2011. This disappearance eliminated the possibility of authoritative
 *   clarification or dispute resolution on the system's intended purpose. The
 *   whitepaper text, titled 'Bitcoin: A Peer-to-Peer Electronic Cash System,'
 *   is simultaneously readable as binding the system to transactional use
 *   (cash function), decentralized architecture (peer-to-peer structure), and
 *   institutional trust-elimination (cryptographic proof). Under resource
 *   constraints — block size, on-chain throughput, computational overhead of
 *   full-node verification — these objectives conflict. Without an oracle to
 *   clarify which constraint is binding, the protocol development community
 *   fractured into interpretive factions, each claiming fidelity to Satoshi's
 *   whitepaper while implementing technically incompatible changes. This
 *   constraint is NOT about whether Bitcoin achieves its purpose; it is about
 *   the structural effect of the oracle's absence: interpretive vacuum
 *   enables fork proliferation, and both readings claim whitepaper fidelity,
 *   but no mechanism for convergence exists without founder clarification.
 *
 * KEY AGENTS:
 *   - Satoshi Nakamoto (oracle role, absent since 2011) — eliminated authoritative interpretation
 *   - Bitcoin Core developers (de facto interpreters, holding the organizing text) — benefit from oracle opacity by maintaining implicit authority
 *   - Electronic cash reading community (e.g., Bitcoin Cash advocates) — claim transactional purpose, bear cost of defending against divergent readings
 *   - Store-of-value reading community (e.g., Bitcoin Core conservatives) — claim decentralization-first purpose, similarly constrained
 *   - Users requiring coherent purpose — face contradictory protocol signals and forking events
 *   - Academic consensus seekers — observe the opacity as a structural feature enabling empirical competition between readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.68).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.52).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, extractiveness, 0.68).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, "Bitcoin Whitepaper Purpose Interpretation Under Oracle Opacity").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, "technology/monetary theory/governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 'cc12adea-4ed1-4657-a706-8ee24cd2712d').
narrative_ontology:cs_kernel_codification('cc12adea-4ed1-4657-a706-8ee24cd2712d', fixed_text).
narrative_ontology:cs_authority_grounding('cc12adea-4ed1-4657-a706-8ee24cd2712d', extraction).
narrative_ontology:cs_reading_relation('cc12adea-4ed1-4657-a706-8ee24cd2712d', bitcoin_whitepaper_purpose__electronic_cash_reading, coexists_with).
narrative_ontology:cs_reading_relation('cc12adea-4ed1-4657-a706-8ee24cd2712d', bitcoin_whitepaper_purpose__store_of_value_reading, coexists_with).
narrative_ontology:cs_axiom('cc12adea-4ed1-4657-a706-8ee24cd2712d', foundational, oracle_absence_enables_interpretation_vacuum).
narrative_ontology:cs_axiom_status(oracle_absence_enables_interpretation_vacuum, holdable).
narrative_ontology:cs_axiom_grounding('cc12adea-4ed1-4657-a706-8ee24cd2712d', oracle_absence_enables_interpretation_vacuum, empirically_contingent).
narrative_ontology:cs_axiom('cc12adea-4ed1-4657-a706-8ee24cd2712d', foundational, whitepaper_text_insufficient_for_constraint_disambiguation).
narrative_ontology:cs_axiom_status(whitepaper_text_insufficient_for_constraint_disambiguation, holdable).
narrative_ontology:cs_axiom_grounding('cc12adea-4ed1-4657-a706-8ee24cd2712d', whitepaper_text_insufficient_for_constraint_disambiguation, deontological).
narrative_ontology:cs_reference_frame('cc12adea-4ed1-4657-a706-8ee24cd2712d', satoshi_authoritative_oracle).
narrative_ontology:cs_drift_state('cc12adea-4ed1-4657-a706-8ee24cd2712d', post_2011_disappearance, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('cc12adea-4ed1-4657-a706-8ee24cd2712d', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, chain_developers_and_miners).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, protocol_implementers).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, users_requiring_coherent_purpose).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, divergent_reading_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, electronic_cash_reading_community).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, store_of_value_reading_community).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, legacy_bitcoin_core_culture).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, chain_developers_and_miners).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, electronic_cash_reading_community).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, store_of_value_reading_community).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, decentralized_consensus_requires_text_binding).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, absence_of_authoritative_interpreter_enables_fork).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Author of the 2008 whitepaper ('Bitcoin: A Peer-to-Peer Electronic Cash System') who participated in Bitcoin's genesis and early development through 2010, then disappeared in 2011, taking the authoritative reading of the system's founding purpose with them. No mechanism for clarification, correction, or adjudication of contested interpretations remains available.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, satoshi_nakamoto_oracle_role, agenda_setter,
    institutional, civilizational, trapped, global).

% Benefit from the interpretive vacuum: each faction of the protocol development community can claim fidelity to 'Satoshi's vision' while implementing divergent changes. The oracle opacity enables them to pursue technical direction without needing to resolve the contested purpose. Pay indirectly by bearing the coordination cost of managing forks and maintaining legitimacy claims for their reading.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, chain_developers_and_miners, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, chain_developers_and_miners, payer).

% Holds that the whitepaper's title and abstract bind the system to everyday transactional use ('cash system' clearly indicates payment function). Carries the cost of defending this reading against divergent factions without access to the founder's authoritative clarification. The oracle opacity forces them to maintain legitimacy through argument rather than oracle appeal.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, electronic_cash_reading_community, payer,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, electronic_cash_reading_community, beneficiary).

% Holds that decentralization and full-node verifiability are the binding constraints; on-chain capacity is subordinated to these goals. Reads 'peer-to-peer' and 'without relying on trust' as core, treating transaction throughput as secondary. Carries the cost of defending this reading in the oracle opacity, unable to appeal to Satoshi for arbitration.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, store_of_value_reading_community, payer,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, store_of_value_reading_community, beneficiary).

% End users and merchants who need a stable, clearly-scoped system purpose to make decisions about adoption, feature expectations, and use cases. The oracle opacity leaves them unable to know which reading represents the 'true' Bitcoin purpose, forcing them to navigate contradictory protocol signals and forking events.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, users_requiring_coherent_purpose, payer,
    powerless, biographical, mobile, global).

% Factions within the protocol development community advocating fundamentally different technical directions (SegWit, block size increases, privacy enhancements, smart contract capability). The oracle opacity enables simultaneous claims of whitepaper fidelity, preventing convergence on a single system purpose. Forks are the only resolution mechanism available.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, divergent_reading_communities, payer,
    moderate, generational, constrained, global).

% The early developers and maintainers of Bitcoin Core who had direct contact with or memory of Satoshi's stated priorities, and who now function as the de facto authoritative interpreters despite no formal oracle role. Their power derives from network effects and installed-code dominance, not from Satoshi's explicit delegation. Benefit from the inability of other factions to definitively contradict them via oracle appeal.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, legacy_bitcoin_core_culture, agenda_setter,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, legacy_bitcoin_core_culture, beneficiary).

% Researchers analyzing Bitcoin's design space and monetary properties without institutional stake in any reading faction. Observe the oracle opacity as a structural feature enabling empirical competition between readings rather than oracle-resolved closure.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, academic_consensus_seekers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, legacy_bitcoin_core_culture).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The whitepaper text as a fixed reference enables the global protocol development community to coordinate disagreement: all factions can claim fidelity to Satoshi's intent and debate the whitepaper's binding constraints using the text as common ground. Without the text, the community would lack any shared reference frame.
% TRANSFER_FUNCTION: Interpretive authority transfers implicitly from the absent founder (Satoshi Nakamoto) to the dominant implementation's maintainers (Bitcoin Core developers), who gain de facto veto power over protocol changes via network effects and installed-code dominance. The transfer is sustained by the oracle opacity: alternative reading communities cannot appeal to Satoshi to overturn Bitcoin Core's decisions, so they must fork.
% ABSENT_VOICES: Satoshi Nakamoto is the primary absent voice: the system was designed with Satoshi as oracle, but the oracle disappeared. Other absent voices: the broader user community (merchants, retail adopters) whose interests are bifurcated by forks and network-effect losses; ideological ancestors of Bitcoin who articulated cryptographic trust (cypherpunk community, Nakamura, Szabo, others) but are not present to clarify what 'peer-to-peer' means under technical constraints; regulatory authorities who would have incentives to push toward a specific reading (cash vs. store-of-value) but are excluded from the development consensus.
% DISAPPEARANCE_RATIONALE: If Satoshi Nakamoto had remained available, the protocol development community would face continuous pressure to converge on a single reading or would fork against the explicit judgment of the founder (raising legitimacy costs for forks). The oracle opacity eliminates this pressure: forks can claim Satoshi fidelity without contradiction, and the dominant implementation can maintain de facto authority without formal legitimacy challenge. The entire fork ecology (Bitcoin Cash, Bitcoin Gold, Bitcoin Diamond, Litecoin, Dogecoin, etc.) is structured around the absence of oracle resolution.
% FOUNDING_PROBLEM: Bitcoin was designed to solve institutional trust as a constraint on value transfer: a system where participants need not trust any third party (bank, payment processor, government) to execute transfers or verify the ledger. The founding problem is simultaneously: (1) enabling digital cash transactions without intermediaries (transactional use case), (2) maintaining decentralized verification such that no participant relies on others' trust in a central node (architectural property), and (3) using cryptographic proof to eliminate epistemic trust (security property). Under the constraint that on-chain throughput and full-node verifiability trade off against each other, these objectives conflict — Satoshi's whitepaper asserts both but does not resolve the trade-off.
% FOUNDING_PROBLEM_CORROBORATION: The electronic cash reading cites Satoshi's whitepaper title ('peer-to-peer electronic cash system'), the abstract's framing, and the choice of 10-minute block time and 21-million supply cap (both aligned with payments use). The store-of-value reading cites Satoshi's emphasis on cryptographic proof (section 1 and throughout), the design choice of full-node verifiability, and the choice of 10-minute block time (aligned with decentralization, not payment speed). Neither reading is corroborated by Satoshi post-disappearance. Independent corroboration from outside the benefiting parties: academic analyses (Nakamoto 2008, follow-up research on consensus economics) support elements of both readings; merchant-adoption metrics and lightning-network development suggest transactional viability is possible but not achieved at scale on-chain; peer review and cryptographic community consensus generally uphold the security and decentralization properties. No independent source has resolved the trade-off — all corroboration remains at the level of technical feasibility, not founding problem resolution.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.42 (early period, when the community was smaller and more unified) to 0.68 (contemporary), plateauing around year 25. This trajectory reflects the accumulation of de facto interpretive authority in the dominant implementation (Bitcoin Core) without explicit mandate. The oracle opacity enables this authority to operate without formal legitimacy challenge — rival factions cannot appeal to Satoshi to overturn decisions. Theater ratio rises from 0.25 to 0.41 and plateaus: as forks proliferate (Bitcoin Cash, Bitcoin Gold, Bitcoin Diamond, etc.), each fork invokes the whitepaper and Satoshi's intent, but the invocations operate as performative claims rather than binding interpretation. Suppression rises to 0.52 and stabilizes: the mechanism that suppresses alternative readings is neither coercive law enforcement nor network-effect monopoly, but rather the path-dependency of mining incentives and exchange-rate primacy, which are maintained through active enforcement by the dominant implementation maintainers (requiring computational effort to sustain the network effect).
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter role (Bitcoin Core maintainers) experience the oracle opacity as enabling — they maintain the dominant implementation and can claim Satoshi fidelity for their technical direction. The payer roles (both reading communities and users) experience it as constraining — they must argue for their reading without oracle appeal, and if they fork, they immediately lose network effects to the dominant implementation. The beneficiary status of chain developers is ambiguous: they benefit from being able to operate without founder constraints, but they also bear the cost of managing forks and legitimacy claims.
 *
 * DIRECTIONALITY LOGIC:
 *   Bitcoin Core maintainers: d ≈ 0.15–0.25 (institutional power, arbitrage-grade exit via forking their own client, but implicit benefit from oracle opacity enabling de facto authority). Electronic cash and store-of-value reading communities: d ≈ 0.55–0.65 (powerful actors, constrained exit via network effects, symmetric balance of benefit from the coordination function and cost from the extraction of interpretive authority). Divergent reading communities: d ≈ 0.70–0.80 (moderate power, trapped by mining economics and exchange rates, victimized by the oracle opacity which forces costly fork-based coordination). Users: d ≈ 0.75–0.85 (powerless, mobile exit but high informational cost, bears the cost of the coordination failure without benefit).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (solving the need for institutional-trust-free value transfer) is contested: electronic cash reading says the problem requires transactional use at scale; store-of-value reading says the problem requires decentralized verification. The oracle opacity prevents the mandatrophy from being formally resolved — Satoshi cannot attest which constraint is binding. The tension between the two readings is structural, not temporary: transaction throughput and full-node verifiability are in genuine trade-off at the technical level. Without oracle clarification, the community resolves the tension through forking, which is not a solution but a cost-shifting mechanism (users and merchants bear the cost of navigating incompatible ledgers). The theater ratio reflects this: early governance invocations (citing Satoshi's emails and intent) slowly transition to network-effect arguments (Bitcoin Cash and other forks operate as alternatives but lose exchange-rate dominance) and technical justifications (SegWit, Lightning Network) that claim to serve the original purpose while de-coupling from the white paper's text.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    oracle_counterfactual_authority,
    'If Satoshi Nakamoto had remained available for interpretation, would the protocol development community have converged on a single reading of the whitepaper''s binding constraints, or would factions have forked anyway?',
    'Counterfactual analysis: examine the pattern of forks post-Satoshi against historical forks in systems with available oracles (e.g., Ethereum under Vitalik Buterin); study early Bitcoin development emails to infer what Satoshi might have said about the trade-offs. If forks still would have occurred despite Satoshi''s availability (due to technical impossibility, not interpretive ambiguity), the oracle opacity is not the root cause of fragmentation.',
    'If convergence would have been likely with oracle availability, the oracle opacity is structurally extractive (enables de facto authority without legitimacy challenge). If forking would have occurred regardless, the oracle opacity is incidental to the coordination problem.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(oracle_counterfactual_authority, conceptual, 'Whether oracle absence is the root cause or a symptom of deeper technical constraints.').

omega_variable(
    whitepaper_constraint_tradeoff_binding,
    'Is there a technical reading of the whitepaper that satisfies both the ''cash system'' (transactional throughput) and ''peer-to-peer'' (full-node verifiability) constraints simultaneously, or are they fundamentally incompatible at scale?',
    'Empirical: test whether layer-2 solutions (Lightning Network, rollups) or novel consensus designs (Proof of Stake variants, sharding) can satisfy both constraints. If a technical solution emerges that both reading communities accept, the tradeoff is contingent; if no such solution emerges despite decades of research, the tradeoff is binding.',
    'If the tradeoff is binding, the oracle opacity is a mechanism for avoiding a decision that must be made; if the tradeoff is contingent, the oracle opacity unnecessarily blocks consensus on a solvable problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(whitepaper_constraint_tradeoff_binding, empirical, 'Whether the foundational technical tensions admit a unified solution.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Does the dominant Bitcoin Core implementation maintain its position through structural barriers (mining economics, exchange-rate network effects) or internalized authority (the community believes Bitcoin Core is the legitimate heir to Satoshi''s project)?',
    'If a rival fork with identical technical merits but stronger transactional capacity emerges and the community does not migrate to it, suppression is internalized (legitimacy claim is binding). If the community migrates readily to superior alternatives despite Bitcoin Core claiming Satoshi fidelity, suppression is structural (path-dependency of network effects). Direct probe: measure the exchange-rate adoption trajectory of forks that explicitly reject Bitcoin Core''s reading.',
    'If internalized, the oracle opacity creates stronger lock-in (the community perpetuates it by choosing belief in Bitcoin Core''s legitimacy); if structural, the opacity is a symptom of underlying economic incentives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of divergent readings derives from economic structure or epistemic/institutional commitment.').

omega_variable(
    reading_foreclosure_via_network_effects,
    'Do the electronic cash reading and store-of-value reading truly coexist, or does one reading''s technical implementation logically foreclose the other in a unified ledger?',
    'Analyze the technical constraints: if increasing on-chain throughput (cash reading) necessarily decreases node verifiability and increases centralization pressure (violation of store-of-value reading), the readings are logically incompatible in a single system. If both can be achieved through layer-2 scaling or alternative consensus, they coexist. Empirical: observe whether Bitcoin Cash and Bitcoin Core ledgers have been able to maintain protocol updates that serve both purposes, or whether each fork is forced to sacrifice one constraint.',
    'If the readings are logically incompatible, one will eventually foreclose the other despite oracle opacity (network effects will select the stronger constraint). If they coexist, the oracle opacity is a genuine perpetual ambiguity that enables parallel evolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_via_network_effects, empirical, 'Whether the kernel''s sibling readings are mutually exclusive or genuinely coexistable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitcoin_oracle_opacity_tr_t0, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(bitcoin_oracle_opacity_tr_t0, observed).
narrative_ontology:measurement(bitcoin_oracle_opacity_tr_t5, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(bitcoin_oracle_opacity_tr_t5, observed).
narrative_ontology:measurement(bitcoin_oracle_opacity_tr_t10, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(bitcoin_oracle_opacity_tr_t10, observed).
narrative_ontology:measurement(bitcoin_oracle_opacity_tr_t15, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(bitcoin_oracle_opacity_tr_t15, observed).
narrative_ontology:measurement(bitcoin_oracle_opacity_tr_t20, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(bitcoin_oracle_opacity_tr_t20, observed).
narrative_ontology:measurement(bitcoin_oracle_opacity_tr_t25, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(bitcoin_oracle_opacity_tr_t25, observed).
narrative_ontology:measurement(bitcoin_oracle_opacity_tr_t30, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(bitcoin_oracle_opacity_tr_t30, observed).
narrative_ontology:measurement(bitcoin_oracle_opacity_tr_t35, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(bitcoin_oracle_opacity_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(bitcoin_oracle_opacity_be_t0, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(bitcoin_oracle_opacity_be_t0, observed).
narrative_ontology:measurement(bitcoin_oracle_opacity_be_t5, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(bitcoin_oracle_opacity_be_t5, observed).
narrative_ontology:measurement(bitcoin_oracle_opacity_be_t10, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(bitcoin_oracle_opacity_be_t10, observed).
narrative_ontology:measurement(bitcoin_oracle_opacity_be_t15, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 15, 0.61).
narrative_ontology:measurement_basis(bitcoin_oracle_opacity_be_t15, observed).
narrative_ontology:measurement(bitcoin_oracle_opacity_be_t20, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(bitcoin_oracle_opacity_be_t20, observed).
narrative_ontology:measurement(bitcoin_oracle_opacity_be_t25, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(bitcoin_oracle_opacity_be_t25, observed).
narrative_ontology:measurement(bitcoin_oracle_opacity_be_t30, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(bitcoin_oracle_opacity_be_t30, observed).
narrative_ontology:measurement(bitcoin_oracle_opacity_be_t35, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(bitcoin_oracle_opacity_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(bitcoin_oracle_opacity_su_t0, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(bitcoin_oracle_opacity_su_t0, observed).
narrative_ontology:measurement(bitcoin_oracle_opacity_su_t5, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 5, 0.4).
narrative_ontology:measurement_basis(bitcoin_oracle_opacity_su_t5, observed).
narrative_ontology:measurement(bitcoin_oracle_opacity_su_t10, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 10, 0.45).
narrative_ontology:measurement_basis(bitcoin_oracle_opacity_su_t10, observed).
narrative_ontology:measurement(bitcoin_oracle_opacity_su_t15, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 15, 0.49).
narrative_ontology:measurement_basis(bitcoin_oracle_opacity_su_t15, observed).
narrative_ontology:measurement(bitcoin_oracle_opacity_su_t20, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 20, 0.51).
narrative_ontology:measurement_basis(bitcoin_oracle_opacity_su_t20, observed).
narrative_ontology:measurement(bitcoin_oracle_opacity_su_t25, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 25, 0.52).
narrative_ontology:measurement_basis(bitcoin_oracle_opacity_su_t25, observed).
narrative_ontology:measurement(bitcoin_oracle_opacity_su_t30, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(bitcoin_oracle_opacity_su_t30, observed).
narrative_ontology:measurement(bitcoin_oracle_opacity_su_t35, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 35, 0.52).
narrative_ontology:measurement_basis(bitcoin_oracle_opacity_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, global_infrastructure).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.18).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_whitepaper_purpose__electronic_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_whitepaper_purpose__store_of_value_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'bitcoin_whitepaper_purpose'. The kernel comprises a fixed text (the 2008 whitepaper) and an authority structure (Satoshi as oracle) designed to resolve interpretive disputes. Satoshi's 2011 disappearance eliminated the authority structure, leaving the text as contested substrate. The three sibling readings (electronic_cash_reading, store_of_value_reading, and this constraint itself) instantiate different positions on what the whitepaper's binding constraints are. Each reading has its own constraint_id, its own epsilon value, its own stakeholder structure, and its own type classification. They are linked via this network.affects_constraints field to enable contention analysis: a change in one reading's perceived legitimacy (e.g., Bitcoin Cash gaining exchange-rate parity) creates structural pressure on the other readings. The delta between readings is NOT a measurement ambiguity (one constraint viewed from two angles) — it is a structural fact about the kernel: the text is interpreted differently by different communities, and no oracle is available to disambiguate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, institutional, 0.2).
constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, powerful, 0.62).
constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
