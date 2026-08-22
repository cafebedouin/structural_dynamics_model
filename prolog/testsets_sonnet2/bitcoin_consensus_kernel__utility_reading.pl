% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__utility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Bitcoin Consensus Kernel — Utility/Iterative-Improvement Reading
 *   domain: cryptoeconomics/monetary_systems/distributed_consensus
 *
 * SUMMARY:
 *   Bitcoin's consensus mechanism has evolved through numerous soft forks
 *   (P2SH, SegWit, Taproot) and enabled layer-2 protocols (Lightning Network,
 *   sidechains) built atop the base layer. The utility reading treats this
 *   evolution as the whitepaper's intended trajectory — a working v1 meant to
 *   be iteratively improved by a technical community, analogous to internet
 *   protocol evolution. Under this reading, developers and miners who drive
 *   change are legitimate stewards, and holders who object to any protocol
 *   modification are treated as misreading the document's actual claim. The
 *   claim (tangled_rope) and the metrics are authored independently: the
 *   coordination function (bootstrapping and scaling a distributed ledger) is
 *   genuine, but so is the asymmetric cost borne by those who staked their
 *   holding thesis, applications, or breakaway communities on the opposite
 *   reading being correct.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__utility_reading, 0.42).
domain_priors:suppression_score(bitcoin_consensus_kernel__utility_reading, 0.38).
domain_priors:theater_ratio(bitcoin_consensus_kernel__utility_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__utility_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_consensus_kernel__utility_reading, "Bitcoin Consensus Kernel — Utility/Iterative-Improvement Reading").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__utility_reading, "cryptoeconomics/monetary_systems/distributed_consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__utility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__utility_reading, 'd4bcc316-f995-405f-acb0-6f7cfabbb21a').
narrative_ontology:cs_kernel_codification('d4bcc316-f995-405f-acb0-6f7cfabbb21a', fixed_text).
narrative_ontology:cs_authority_grounding('d4bcc316-f995-405f-acb0-6f7cfabbb21a', practice).
narrative_ontology:cs_interpretation_layer_present('d4bcc316-f995-405f-acb0-6f7cfabbb21a').
narrative_ontology:cs_reading_relation('d4bcc316-f995-405f-acb0-6f7cfabbb21a', bitcoin_consensus_kernel__maximalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('d4bcc316-f995-405f-acb0-6f7cfabbb21a', bitcoin_consensus_kernel__pragmatic_synthesis, influences).
narrative_ontology:cs_axiom('d4bcc316-f995-405f-acb0-6f7cfabbb21a', foundational, whitepaper_specifies_provisional_mechanism).
narrative_ontology:cs_axiom_status(whitepaper_specifies_provisional_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('d4bcc316-f995-405f-acb0-6f7cfabbb21a', whitepaper_specifies_provisional_mechanism, conventional).
narrative_ontology:cs_axiom('d4bcc316-f995-405f-acb0-6f7cfabbb21a', foundational, developer_miner_consensus_constitutes_legitimate_amendment).
narrative_ontology:cs_axiom_status(developer_miner_consensus_constitutes_legitimate_amendment, holdable).
narrative_ontology:cs_axiom_grounding('d4bcc316-f995-405f-acb0-6f7cfabbb21a', developer_miner_consensus_constitutes_legitimate_amendment, instrumental).
narrative_ontology:cs_reference_frame('d4bcc316-f995-405f-acb0-6f7cfabbb21a', whitepaper_as_provisional_working_specification).
narrative_ontology:cs_drift_state('d4bcc316-f995-405f-acb0-6f7cfabbb21a', post_segwit_taproot_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d4bcc316-f995-405f-acb0-6f7cfabbb21a', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, protocol_developers).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, layer_two_builders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, new_adopters).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, mining_pool_operators).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, long_term_holders_expecting_fixed_rules).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, minority_fork_communities).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, users_relying_on_base_layer_finality_guarantees).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the reference client and propose soft forks and protocol upgrades (SegWit, Taproot, layer-2 anchoring). They read the whitepaper as describing a minimum viable mechanism meant to be improved, and their continued relevance and technical authority depend on the protocol remaining amenable to change. They can fork the codebase or migrate influence to competing implementations if their proposals are rejected.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, protocol_developers, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__utility_reading, protocol_developers, beneficiary).

% Build payment channels, sidechains, and settlement layers on top of base-layer consensus. Their business models depend on the base layer accepting new opcodes, soft-fork activations, and script upgrades. They benefit directly from the iterative-improvement reading and would lose their technical foundation under a frozen-rules interpretation.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, layer_two_builders, beneficiary,
    moderate, biographical, mobile, global).

% Enter the network expecting continued scalability and usability improvements — lower fees, faster settlement, better wallets. They have no stake in a frozen 2009-era protocol and directly benefit from ongoing development; their exit option is simply choosing a different chain if improvement stalls.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, new_adopters, beneficiary,
    powerless, immediate, mobile, global).

% Signal support for soft forks via hash power and can effectively veto or ratify protocol changes. They benefit from an evolvable protocol that can adapt to fee-market changes, ASIC developments, and new revenue mechanisms (ordinals, inscriptions) that increase transaction demand.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, mining_pool_operators, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__utility_reading, mining_pool_operators, agenda_setter).

% Acquired and held the asset specifically because they understood the whitepaper as establishing fixed, credibly-neutral monetary rules. Each successful soft fork or governance change under the utility reading erodes the specific promise they built their holding thesis on — their exit would mean abandoning years of accumulated position and the belief system justifying it.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, long_term_holders_expecting_fixed_rules, payer,
    powerless, generational, identity_locked, global).

% Split off (Bitcoin Cash, Bitcoin SV, etc.) when they rejected specific iterative changes, asserting the original whitepaper's rules were being violated. They bear the cost of network effect loss, reduced liquidity, and being labeled illegitimate forks by the dominant chain's community — the price of contesting the utility reading's legitimacy claim.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, minority_fork_communities, payer,
    moderate, biographical, constrained, global).

% Depend on settlement and script behavior remaining exactly as it was when they built applications, custody arrangements, or multisig setups atop the base layer. Soft forks that are technically backward-compatible can still silently change fee dynamics, script validity edge cases, or mempool behavior in ways that break their assumptions without their consent.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, users_relying_on_base_layer_finality_guarantees, payer,
    powerless, immediate, trapped, global).

% Argue that the whitepaper's true contribution was demonstrating that a fixed monetary policy could be enforced without central authority, and that ANY change to consensus rules — however well-intentioned — undermines the core value proposition. Their objection is structurally present in every governance debate but is treated as a minority position within the utility-reading community's own forums and conferences.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, monetary_ossification_advocates, excluded,
    moderate, civilizational, identity_locked, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_consensus_kernel__utility_reading, diffuse).
narrative_ontology:fixing_cost_class(bitcoin_consensus_kernel__utility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a distributed network of miners, node operators, and developers around a continuously improvable consensus mechanism, allowing the protocol to adapt to scaling pressure, new cryptographic techniques, and emergent use cases without requiring a full replacement of the network.
% TRANSFER_FUNCTION: Moves technical authority and de facto monetary-policy-adjacent decision power from the original, fixed whitepaper specification toward an ongoing developer/miner coalition; moves risk from that coalition onto holders and users who built expectations around rule permanence.
% ABSENT_VOICES: Monetary ossification advocates and minority fork communities would object that each 'improvement' is itself a rule change requiring the same consent as any other alteration to a monetary covenant; they are present in public forums but structurally outvoted by the miner-signaling and developer-consensus process this reading treats as legitimate.
% DISAPPEARANCE_RATIONALE: If the iterative-improvement mechanism (soft fork governance, BIP process, miner signaling) disappeared overnight, developers and layer-2 builders would lose their path to protocol evolution and adoption growth would likely stall — but holders who prize fixed rules would consider this a return to the original, more legitimate design, not a loss. Whether the world 'rearranges' depends entirely on which reading of the kernel is asked.
% FOUNDING_PROBLEM: The whitepaper needed to specify SOME concrete, working consensus mechanism to bootstrap a functioning decentralized ledger — Nakamoto consensus as originally specified was necessarily a first working version, not a final theoretical optimum, given the state of the art in 2008-2009.
% FOUNDING_PROBLEM_CORROBORATION: Protocol developers and layer-2 builders (the reading's own beneficiaries) attest the founding problem was explicitly framed as provisional — citing Satoshi's own forum posts discussing future block size and protocol adjustments. However, long-term holders and monetary ossification advocates, who are outside the beneficiary set, dispute this reading of the historical record and argue the 21 million cap and core validation rules were presented as permanent; no unaffiliated third-party historian corroborates either camp's reading of authorial intent, since the author is pseudonymous and no longer participates.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__utility_reading, contested).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__utility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__utility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bitcoin_consensus_kernel__utility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_consensus_kernel__utility_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is moderate (0.42) and rising over the measured interval — reflecting an accumulating pattern of protocol changes (SegWit activation controversy, Taproot, ongoing debates over covenants/OP_CTV) each of which redistributes some value or expectation from rule-permanence holders toward the developer/builder coalition. Suppression is moderate (0.38): dissenting minority forks are not banned, but network effects, exchange listings, and community narrative strongly disfavor them, and the 'legitimate chain' framing itself functions as social suppression of the ossification reading. Theater ratio (0.28) captures that some governance process (BIP review, mailing list debate) is substantive but an increasing share functions as legitimation ritual for decisions already made by concentrated developer/mining coalitions.
 *
 * PERSPECTIVAL GAP:
 *   From the protocol-developer and layer-2-builder seats, this is straightforwardly coordination — a genuine collective-action solution to scaling and feature demand, with the whitepaper's authority behind it. From the long-term-holder and minority-fork seats, the identical soft-fork process reads as slow-motion extraction: each change is technically 'backward compatible' but cumulatively redefines the asset's promised properties without their consent, using the same governance machinery the developers call legitimate improvement.
 *
 * DIRECTIONALITY LOGIC:
 *   Protocol developers, layer-2 builders, new adopters, and mining pool operators are declared beneficiaries — they either directly profit from an evolvable protocol or joined the network already expecting evolution, so their derived directionality sits toward the beneficiary end. Long-term holders expecting fixed rules, minority fork communities, and base-layer-dependent users are declared victims: they bear the redefinition costs, have constrained-to-trapped exit (identity fused with a specific reading of the asset, or infrastructure fused with specific script behavior), and their derived directionality sits toward the target end. No override was needed — the beneficiary/victim declarations map cleanly onto the structural asymmetry the utility reading itself acknowledges.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem interview captures the mandatrophy risk directly: protocol developers attest the founding problem (bootstrap a working decentralized ledger, later improve it) is still live and evolving, while ossification-oriented holders attest the founding problem (prove fixed monetary policy is enforceable without central authority) was already solved at genesis and the ongoing 'improvement' process is a different, unmandated function riding on the original's legitimacy. Classifying this as tangled_rope rather than snare prevents mislabeling the real coordination function (scaling, feature development serving actual users) as pure extraction; classifying it as tangled_rope rather than rope prevents ignoring the asymmetric, identity-locked cost imposed on holders who reasonably relied on a different reading of the same founding document.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Does the original whitepaper''s silence on a formal amendment process mean informal developer/miner consensus IS the intended amendment mechanism (utility reading), or does it mean no legitimate amendment mechanism exists at all (maximalist reading)?',
    'This is not resolvable by appeal to the text alone — Satoshi''s departure and pseudonymity mean authorial intent cannot be directly queried. Resolution mechanism would be a supermajority, cross-faction convergence on a single reading sustained over multiple contested forks, which has not occurred.',
    'If the maximalist reading were to become dominant, current soft-fork governance would be reclassified as illegitimate rule-breaking rather than protocol stewardship, converting much of what this reading treats as coordination into pure extraction from the ossification-expecting holder base.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Sibling-reading location: whether informal governance is the amendment mechanism or evidence no amendment mechanism was intended.').

omega_variable(
    soft_fork_consent_mechanism,
    'Does miner/node signaling for a soft fork constitute meaningful consent from holders and users, or does it substitute a narrow technical constituency''s preferences for the broader stakeholder set''s actual consent?',
    'Compare outcomes of contentious forks (SegWit2x failure vs. Taproot near-unanimous activation) to assess whether the signaling mechanism tracks genuine broad consent or reflects concentrated technical/mining power regardless of holder sentiment.',
    'If signaling tracks concentrated power rather than broad consent, the extraction component of this tangled_rope classification is understated; if it reliably tracks broad legitimacy, the coordination component is stronger than the current 0.42 extractiveness score suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(soft_fork_consent_mechanism, empirical, 'Whether the existing consent mechanism for protocol change is structurally adequate.').

omega_variable(
    beneficiary_victim_role_fluidity,
    'Are the declared beneficiary/victim groups stable over time, or do individual holders shift categories as their expectations update (e.g., a holder who initially expected fixed rules but later embraces the utility reading)?',
    'Longitudinal survey of holder sentiment across major fork events would show whether category membership is fixed by initial belief or fluid with narrative shifts.',
    'High fluidity would suggest the victim category is smaller and more self-selected than the static declaration implies, potentially reducing the effective extraction measured; low fluidity would confirm the current victim declaration as durable and structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_victim_role_fluidity, empirical, 'Whether victim/beneficiary status is a fixed structural position or a fluid belief that updates with community narrative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__utility_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_consensus_kernel__utility_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bitc_tr_t8, bitcoin_consensus_kernel__utility_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(bitc_tr_t16, bitcoin_consensus_kernel__utility_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(bitc_tr_t24, bitcoin_consensus_kernel__utility_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(bitc_tr_t32, bitcoin_consensus_kernel__utility_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(bitc_tr_t40, bitcoin_consensus_kernel__utility_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(bitc_be_t8, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 8, 0.24).
narrative_ontology:measurement(bitc_be_t16, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 16, 0.31).
narrative_ontology:measurement(bitc_be_t24, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 24, 0.36).
narrative_ontology:measurement(bitc_be_t32, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(bitc_be_t40, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(bitc_su_t8, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 8, 0.2).
narrative_ontology:measurement(bitc_su_t16, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 16, 0.27).
narrative_ontology:measurement(bitc_su_t24, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 24, 0.32).
narrative_ontology:measurement(bitc_su_t32, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 32, 0.36).
narrative_ontology:measurement(bitc_su_t40, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__utility_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(bitcoin_consensus_kernel__utility_reading, 0.12).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel__maximalist_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel__pragmatic_synthesis).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the bitcoin_consensus_kernel. maximalist_reading treats the whitepaper as immutable monetary covenant (near-zero extractiveness under its own lights, high accessibility_collapse for any deviation). pragmatic_synthesis treats base-layer rules as fixed while permitting upper-layer innovation (lower extractiveness than this reading, since it forecloses base-layer change as an extraction vector entirely). This utility_reading carries the highest authored extractiveness of the three because it treats even base-layer soft-fork evolution as within-scope, generating the widest beneficiary/victim asymmetry. Each story's ε is fixed to its own reading and is not averaged or reconciled across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
