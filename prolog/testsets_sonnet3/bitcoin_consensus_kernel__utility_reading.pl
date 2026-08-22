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
 *   This story instantiates the utility/iterative-improvement reading of the
 *   Bitcoin consensus kernel: the whitepaper is read as establishing a
 *   minimum viable consensus mechanism, not a frozen covenant, so soft forks
 *   and layer-2 protocols represent legitimate evolution rather than covenant
 *   violation. Under this reading, beneficiaries are the builders and
 *   adopters who need the protocol to keep adding capability (SegWit-style
 *   capacity increases, Taproot-style privacy/scripting gains, ongoing L2
 *   settlement innovation), while the costs land on parties who priced in an
 *   assumption of permanent fixity — long-term holders, hardware wallet
 *   maintainers who must track a moving target, and minority nodes whose
 *   formal veto is structurally weaker than it appears because soft forks are
 *   engineered to be backward-compatible. This is a Tangled Rope: it
 *   genuinely coordinates a large, permissionless network around a shared
 *   upgrade path (real coordination function) while extracting certainty and
 *   imposing maintenance costs on parties who did not consent to treating the
 *   rules as provisional (real extraction), and it requires active
 *   enforcement in the form of miner signaling thresholds and
 *   reference-implementation gatekeeping to hold.
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
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__utility_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_consensus_kernel__utility_reading, "Bitcoin Consensus Kernel — Utility/Iterative-Improvement Reading").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__utility_reading, "cryptoeconomics/monetary_systems/distributed_consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__utility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__utility_reading, '732f7796-a4ac-41cd-9704-57de2fb65cda').
narrative_ontology:cs_kernel_codification('732f7796-a4ac-41cd-9704-57de2fb65cda', fixed_text).
narrative_ontology:cs_authority_grounding('732f7796-a4ac-41cd-9704-57de2fb65cda', practice).
narrative_ontology:cs_interpretation_layer_present('732f7796-a4ac-41cd-9704-57de2fb65cda').
narrative_ontology:cs_reading_relation('732f7796-a4ac-41cd-9704-57de2fb65cda', bitcoin_consensus_kernel__maximalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('732f7796-a4ac-41cd-9704-57de2fb65cda', bitcoin_consensus_kernel__pragmatic_synthesis, influences).
narrative_ontology:cs_axiom('732f7796-a4ac-41cd-9704-57de2fb65cda', foundational, protocol_is_extensible_by_design).
narrative_ontology:cs_axiom_status(protocol_is_extensible_by_design, holdable).
narrative_ontology:cs_axiom_grounding('732f7796-a4ac-41cd-9704-57de2fb65cda', protocol_is_extensible_by_design, conventional).
narrative_ontology:cs_axiom('732f7796-a4ac-41cd-9704-57de2fb65cda', secondary, rough_consensus_activation_is_legitimate_governance).
narrative_ontology:cs_axiom_status(rough_consensus_activation_is_legitimate_governance, holdable).
narrative_ontology:cs_axiom_grounding('732f7796-a4ac-41cd-9704-57de2fb65cda', rough_consensus_activation_is_legitimate_governance, instrumental).
narrative_ontology:cs_reference_frame('732f7796-a4ac-41cd-9704-57de2fb65cda', whitepaper_as_extensible_starting_design).
narrative_ontology:cs_drift_state('732f7796-a4ac-41cd-9704-57de2fb65cda', post_taproot_layer2_expansion_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('732f7796-a4ac-41cd-9704-57de2fb65cda', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, layer2_protocol_builders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, application_developers).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, new_adopters).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, mining_pool_operators).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, long_term_holders_expecting_fixed_rules).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, hardware_wallet_maintainers).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, minority_nodes_rejecting_soft_forks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build payment channels, sidechains, and settlement layers on top of the base consensus rules, relying on the ability to propose and activate soft forks (e.g. SegWit, Taproot) that extend script capability without breaking full-node validation. Their business model depends on the base layer remaining amenable to incremental extension rather than freezing at the whitepaper's original opcode set.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, layer2_protocol_builders, beneficiary,
    organized, biographical, mobile, global).

% Build wallets, exchanges, and services that consume whatever consensus rules are live. They benefit from soft-fork upgrades that add functionality (covenants, better privacy, smart contract primitives) without requiring them to migrate users to a new chain.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, application_developers, beneficiary,
    moderate, biographical, mobile, global).

% Enter the network expecting a functioning, evolving payment and settlement system. They benefit from improvements in transaction throughput, privacy, and usability that iterative consensus changes deliver, without having agreed to any particular 'immutable' founding covenant themselves.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, new_adopters, beneficiary,
    powerless, biographical, mobile, global).

% Signal for and activate soft forks by running upgraded software and setting version bits; in this reading their role in advancing consensus rules is treated as legitimate stewardship of a mechanism intended to evolve, not usurpation of a fixed protocol. Their exit is constrained by sunk capital in mining hardware calibrated to specific rule sets.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, mining_pool_operators, agenda_setter,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__utility_reading, mining_pool_operators, beneficiary).

% Acquired the asset on the premise (as they understood it) that the twenty-one million cap and base monetary policy were permanently fixed by the whitepaper's design. Under this reading, the 'minimum viable mechanism' framing treats even monetary-policy-adjacent debates as open to iterative revision by rough consensus, which erodes the certainty they priced into their holding. They cannot exit the asset's rule-set without exiting the asset itself.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, long_term_holders_expecting_fixed_rules, payer,
    powerless, generational, trapped, global).

% Must continuously re-certify firmware and signing logic against a moving target of soft-fork activations. Each iterative improvement imposes a maintenance and audit cost that a genuinely frozen protocol would not require; they bear this cost to keep older devices compatible with the evolving live chain.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, hardware_wallet_maintainers, payer,
    moderate, biographical, constrained, global).

% Run full nodes that reject a given soft fork's new rules on principle, but soft forks are engineered to be backward-compatible so that non-upgraded nodes still accept the new blocks as valid — leaving objectors economically absorbed into the new rules rather than genuinely able to reject them. Their nominal veto is structurally weaker than their formal validation role suggests.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, minority_nodes_rejecting_soft_forks, payer,
    powerless, biographical, trapped, global).

% Write and review the reference implementation's proposed changes (BIPs), framing the whitepaper as establishing a floor — a minimum viable mechanism — rather than a ceiling. They mediate which proposals reach activation readiness, exercising substantial informal authority over what counts as 'iterative improvement' versus 'unacceptable change.'
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, core_protocol_developers, agenda_setter,
    institutional, generational, analytical, global).

% Argue that this reading's 'minimum viable, iteratively improvable' framing is itself a category error — that the whitepaper's monetary policy is a founding covenant, not a draft. They are present in public discourse but structurally outvoted in the rough-consensus process this reading treats as legitimate; their objection is heard but does not block activation.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, maximalist_holders, excluded,
    organized, civilizational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a large, permissionless set of nodes, miners, and users around a shared, upgradeable rule set — allowing the network to add capability (better scripting, privacy, scaling primitives) through backward-compatible soft forks without requiring universal simultaneous agreement or a disruptive hard fork/chain split.
% TRANSFER_FUNCTION: Moves protocol-design authority and de facto veto power from the diffuse, rules-are-fixed expectation of long-term holders toward the smaller set of core developers and signaling miners who determine which soft forks activate; moves ongoing maintenance and re-certification costs onto downstream implementers (wallet software, hardware signers) who must track a moving rule set.
% ABSENT_VOICES: Long-term holders who bought the asset on a strict fixed-supply, fixed-rules premise are not organized as a formal veto bloc and have no seat in the BIP process; maximalist advocates are vocal in public forums but are structurally unable to block soft-fork activation once miner/node rough consensus forms.
% DISAPPEARANCE_RATIONALE: If the 'minimum viable, iteratively improvable' reading were displaced by the maximalist reading tomorrow, the practical effect on already-activated soft forks (SegWit, Taproot) would be minimal — those changes are embedded in the live chain — but the legitimacy basis for FUTURE protocol changes would shift dramatically: any further evolution would need to justify itself as consistent with an immutable covenant rather than as ordinary iteration, likely freezing further base-layer development. Whether that constitutes 'world rearranges' or 'world unchanged' is exactly what the three readings dispute.
% FOUNDING_PROBLEM: The whitepaper needed to establish SOME workable, deployable consensus mechanism to solve double-spending without a trusted third party; it explicitly described itself as a starting design ('the system is secure as long as...') rather than a finished, permanently fixed specification, leaving room for later refinement as the network's needs and threat model became clearer.
% FOUNDING_PROBLEM_CORROBORATION: Core developers and layer-2 builders attest the founding problem was 'build a working, extensible system' and that iterative improvement is squarely within the whitepaper's own stated design philosophy. Independent computer-science historians and several early mailing-list participants (outside the current beneficiary set) corroborate that the original design was explicitly provisional in places (e.g., the author's own forum posts describing planned future changes to scripting and scaling); however, long-term holders and maximalist commentators dispute that this provisionality extended to monetary policy specifically, and no neutral third party has definitively settled which subset of rules the founding design treated as fixed versus open.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__utility_reading, contested).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__utility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__utility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extraction (0.42) is moderate, not extreme: soft forks are opt-in in the narrow technical sense (non-upgraded nodes still validate) but the practical effect is that objecting minorities are economically absorbed rather than genuinely exited. Suppression (0.38) reflects the informal-but-real gatekeeping exercised by core developers and mining pools over which proposals reach activation, without rising to the level of a snare's coercive apparatus. Theater ratio (0.28) captures that the 'rough consensus and running code' process has real technical substance but also carries a degree of legitimacy theater — describing outcomes as organic community consensus when a small set of maintainers and large pools exercise outsized influence over what gets proposed at all. Accessibility collapse is moderate (0.35): running an old client, refusing to upgrade, or forking away remain technically available, so alternatives have not collapsed as completely as under a genuine monopoly. Resistance (0.58) is substantial because the maximalist faction is loud, organized, and persistent even though structurally outvoted.
 *
 * DIRECTIONALITY LOGIC:
 *   Builders, application developers, and new adopters sit toward the beneficiary end: they gain capability and lower barriers to entry from each iterative improvement and bear little of the associated maintenance or certainty cost. Mining pool operators and core developers are agenda-setters who both administer the evolution process and benefit from the legitimacy and continued relevance it grants them. Long-term holders and minority nodes sit toward the target end: they bear a cost (eroded certainty, formally weaker veto than expected) that flows through the same mechanism the coordination story celebrates. Hardware wallet maintainers are a genuine payer class — a real, recurring, non-symbolic cost of the constraint's iterative nature.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem interview resolves as contested rather than dead or clearly live: the whitepaper's own text supports a provisional-design reading for scripting and scaling, but that provisionality is disputed as extending to monetary policy specifically. This prevents the story from either dismissing the utility reading as pure rent-seeking (mandatrophy would require pronouncing the founding problem dead while the mechanism persists — here the problem status itself remains open) or crediting it uncritically as pure coordination (the tangled_rope classification requires acknowledging the real cost imposed on long-term holders and minority nodes). The Q5 disappearance verdict is deliberately 'contested' because the three kernel readings disagree about what would even count as 'the world rearranging.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    monetary_policy_boundary_ambiguity,
    'Did the whitepaper''s own design philosophy treat monetary policy (the 21 million cap, issuance schedule) as within the same ''iteratively improvable'' category as scripting and scaling, or did it implicitly or explicitly treat monetary policy as a separate, fixed commitment?',
    'Close textual and historical analysis of the whitepaper plus contemporaneous author statements (mailing list, forum posts) specifically addressing whether the supply cap was ever presented as provisional versus foundational; corroboration from independent historians of the project who are not current stakeholders in either camp.',
    'If the founding design demonstrably treated monetary policy as equally open to iteration, this reading''s extraction score should fall (the cost to long-term holders reflects a mistaken expectation, not a genuine broken promise). If the founding design demonstrably ring-fenced monetary policy specifically, this reading understates extraction and the pragmatic_synthesis reading is the more accurate structural account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monetary_policy_boundary_ambiguity, empirical, 'Whether the whitepaper''s provisionality extended to monetary policy or only to non-monetary protocol features.').

omega_variable(
    soft_fork_consent_quality,
    'Does the backward-compatibility property of soft forks constitute genuine consent from non-upgrading nodes, or does it constitute engineered non-exit — a mechanism designed so that objection is technically registered but practically ineffective?',
    'Analysis of historical soft-fork activation episodes (e.g., SegWit''s UASF/BIP148 period) for evidence of genuine optionality: did meaningfully-sized minorities who rejected the change retain a functioning, valued chain, or were they economically absorbed?',
    'If soft forks reliably absorb objectors economically rather than allowing a viable minority chain to persist, the suppression score for this reading should be higher than authored, and the tangled_rope classification''s active-enforcement component is understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(soft_fork_consent_quality, empirical, 'Whether soft-fork backward compatibility represents real consent or engineered inescapability for dissenting nodes.').

omega_variable(
    kernel_reading_selection_stakes,
    'Is the choice among the three kernel readings (maximalist, pragmatic_synthesis, utility) itself a neutral interpretive question, or does it functionally determine who wins ongoing protocol-design disputes — making the reading choice itself a site of extraction?',
    'Track which reading is invoked by which faction in live protocol debates (e.g., block size wars, covenant opcode proposals) and whether invocation correlates with material stake in the outcome.',
    'If reading-choice correlates strongly with material interest, the three-reading decomposition itself should be understood as a live political contest rather than a settled interpretive typology, which would argue for even more caution in treating any single reading''s ε as descriptively final.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_stakes, conceptual, 'Whether the kernel-reading trichotomy is a neutral interpretive frame or itself a contested political tool.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__utility_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_consensus_kernel__utility_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bitc_tr_t3, bitcoin_consensus_kernel__utility_reading, theater_ratio, 3, 0.14).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_consensus_kernel__utility_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(bitc_tr_t9, bitcoin_consensus_kernel__utility_reading, theater_ratio, 9, 0.22).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_consensus_kernel__utility_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement(bitc_tr_t15, bitcoin_consensus_kernel__utility_reading, theater_ratio, 15, 0.28).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(bitc_be_t3, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 3, 0.28).
narrative_ontology:measurement(bitc_be_t6, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 6, 0.33).
narrative_ontology:measurement(bitc_be_t9, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 9, 0.37).
narrative_ontology:measurement(bitc_be_t12, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 12, 0.4).
narrative_ontology:measurement(bitc_be_t15, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 15, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(bitc_su_t3, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 3, 0.24).
narrative_ontology:measurement(bitc_su_t6, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 6, 0.28).
narrative_ontology:measurement(bitc_su_t9, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 9, 0.32).
narrative_ontology:measurement(bitc_su_t12, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 12, 0.35).
narrative_ontology:measurement(bitc_su_t15, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 15, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__utility_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(bitcoin_consensus_kernel__utility_reading, 0.12).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel__maximalist_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel__pragmatic_synthesis).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the bitcoin_consensus_kernel, decomposed per the epsilon-invariance principle: measuring 'the Bitcoin social contract' as a single constraint conflates three structurally distinct claims with different epsilon values. maximalist_reading treats the whitepaper as an immutable covenant (lowest extraction — any deviation is simply illegitimate and doesn't count as the constraint operating). pragmatic_synthesis splits the base layer (immutable) from upper layers (open) and sits at intermediate extraction. utility_reading (this story) treats the whole mechanism as iteratively improvable and authors the highest extraction because it is the only reading under which even monetary-policy-adjacent change is framed as legitimate iteration rather than covenant violation. All three share the same underlying kernel (the whitepaper's founding text and the network's consensus process) but diverge on what counts as fixed versus revisable within it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
