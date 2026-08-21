% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Bitcoin Maximalist Reading of Monetary Immutability
 *   domain: cryptoeconomics/monetary_systems/distributed_consensus
 *
 * SUMMARY:
 *   This constraint represents the 'maximalist reading' of Bitcoin's core
 *   monetary policy, asserting that the whitepaper establishes an immutable,
 *   unchangeable covenant for the system's operation. Any deviation from this
 *   original vision, particularly regarding monetary supply or base-layer
 *   protocol, is considered a violation. While claimed as a 'mountain' (a
 *   fundamental, unchangeable truth), the presence of identifiable
 *   beneficiaries and victims, coupled with high extractiveness and active
 *   enforcement, suggests a constructed constraint that benefits specific
 *   groups.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__maximalist_reading, 0.78).
domain_priors:suppression_score(bitcoin_consensus_kernel__maximalist_reading, 0.85).
domain_priors:theater_ratio(bitcoin_consensus_kernel__maximalist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__maximalist_reading, mountain).
narrative_ontology:human_readable(bitcoin_consensus_kernel__maximalist_reading, "Bitcoin Maximalist Reading of Monetary Immutability").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__maximalist_reading, "cryptoeconomics/monetary_systems/distributed_consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__maximalist_reading).
domain_priors:emerges_naturally(bitcoin_consensus_kernel__maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__maximalist_reading, '87df75b9-07e5-418f-b8bd-71765f050535').
narrative_ontology:cs_kernel_codification('87df75b9-07e5-418f-b8bd-71765f050535', fixed_text).
narrative_ontology:cs_authority_grounding('87df75b9-07e5-418f-b8bd-71765f050535', lineage).
narrative_ontology:cs_interpretation_layer_present('87df75b9-07e5-418f-b8bd-71765f050535').
narrative_ontology:cs_reading_relation('87df75b9-07e5-418f-b8bd-71765f050535', bitcoin_consensus_kernel__pragmatic_synthesis, coexists_with).
narrative_ontology:cs_reading_relation('87df75b9-07e5-418f-b8bd-71765f050535', bitcoin_consensus_kernel__utility_reading, forecloses).
narrative_ontology:cs_axiom('87df75b9-07e5-418f-b8bd-71765f050535', foundational, monetary_immutability_is_absolute).
narrative_ontology:cs_axiom_status(monetary_immutability_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('87df75b9-07e5-418f-b8bd-71765f050535', monetary_immutability_is_absolute, deontological).
narrative_ontology:cs_axiom('87df75b9-07e5-418f-b8bd-71765f050535', foundational, whitepaper_is_founding_covenant).
narrative_ontology:cs_axiom_status(whitepaper_is_founding_covenant, holdable).
narrative_ontology:cs_axiom_grounding('87df75b9-07e5-418f-b8bd-71765f050535', whitepaper_is_founding_covenant, conventional).
narrative_ontology:cs_reference_frame('87df75b9-07e5-418f-b8bd-71765f050535', satoshi_vision_unaltered).
narrative_ontology:cs_drift_state('87df75b9-07e5-418f-b8bd-71765f050535', contemporary_scaling_debates, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('87df75b9-07e5-418f-b8bd-71765f050535', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, early_bitcoin_holders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, maximalist_community).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, protocol_innovators).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, scalability_solution_developers).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, new_users_seeking_lower_fees).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the fixed monetary supply and resistance to protocol changes, which they believe preserves their wealth and the system's integrity. Their identity is often tied to Bitcoin's original vision.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, early_bitcoin_holders, beneficiary,
    powerful, generational, identity_locked, global).

% Actively defends the immutability of Bitcoin's core monetary policy as established in the whitepaper. They exert social and technical pressure against any proposed changes, viewing them as violations of a founding covenant. Their influence is primarily through social consensus and developer mindshare.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, maximalist_community, agenda_setter,
    organized, generational, identity_locked, global).

% Seek to improve Bitcoin's functionality, scalability, or privacy through protocol changes. They face significant resistance and often incur high costs (time, resources, social capital) in trying to gain consensus for their proposals, which are frequently framed as violating the 'founding covenant'.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, protocol_innovators, payer,
    moderate, biographical, constrained, global).

% Develop layer-2 solutions (e.g., Lightning Network) to address Bitcoin's transaction throughput limitations. While their work is often tolerated, they are constrained by the immutability of the base layer, which limits the scope and efficiency of their innovations.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, scalability_solution_developers, payer,
    moderate, biographical, constrained, global).

% Are attracted to Bitcoin's promise but find high transaction fees and slow confirmation times prohibitive for everyday use. They bear the costs of the base layer's limited scalability, with few options to influence policy.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, new_users_seeking_lower_fees, payer,
    powerless, immediate, constrained, global).

% Argue for a more flexible approach to Bitcoin's protocol, prioritizing utility, lower fees, and broader adoption over strict adherence to original design principles. They are often marginalized or dismissed within maximalist discourse, leading them to explore alternative cryptocurrencies.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, utility_advocates, excluded,
    organized, biographical, mobile, global).

% Seeks a middle ground, acknowledging the importance of base layer stability while advocating for innovation on upper layers. They observe the maximalist stance and its effects, often attempting to bridge the ideological divide.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, pragmatic_synthesis_community, observer,
    organized, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_consensus_kernel__maximalist_reading, early_bitcoin_holders).
narrative_ontology:fixing_cost_class(bitcoin_consensus_kernel__maximalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universally agreed-upon, fixed monetary supply and a censorship-resistant transaction ledger, ensuring predictable economic policy and trust in the system's scarcity.
% TRANSFER_FUNCTION: Transfers wealth preservation and monetary sovereignty to early adopters and long-term holders by resisting inflation and protocol changes, while transferring costs of limited scalability, high transaction fees, and restricted innovation to protocol developers and new users.
% ABSENT_VOICES: Advocates for protocol flexibility, lower transaction fees, or alternative scaling solutions that require base-layer changes are often dismissed or deplatformed within maximalist discourse, effectively excluding their perspectives from core protocol development.
% DISAPPEARANCE_RATIONALE: If the maximalist reading of immutable monetary policy vanished, the Bitcoin network would likely fragment into competing versions with different monetary policies, leading to a collapse of trust, network effect, and value for the original chain. The entire cryptoeconomic landscape would reorganize.
% FOUNDING_PROBLEM: To create a truly decentralized, censorship-resistant digital cash system with a fixed supply, free from central bank manipulation, inflationary pressures, and arbitrary rule changes.
% FOUNDING_PROBLEM_CORROBORATION: The maximalist community strongly attests to the problem's ongoing relevance, citing continued concerns about fiat currency inflation and centralized control. While critics (e.g., utility_advocates) might argue the problem has evolved or that the current solution is suboptimal, the core problem of centralized monetary control is widely acknowledged across the crypto space, even if solutions differ.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__maximalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__maximalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__maximalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(bitcoin_consensus_kernel__maximalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_consensus_kernel__maximalist_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.78) reflects the costs imposed on those seeking innovation or lower transaction fees due to the fixed base layer. Suppression (0.85) is high because the maximalist community actively resists and socially enforces adherence to the 'founding covenant,' often marginalizing dissenting voices. The theater ratio is low (0.10) because the belief in immutability is genuinely held and enforced, not merely performative. Accessibility collapse is high (0.88) as alternatives are framed as fundamentally flawed or illegitimate. Resistance is moderate (0.55) due to ongoing debates and the emergence of alternative crypto assets.
 *
 * PERSPECTIVAL GAP:
 *   From the maximalist perspective, this constraint is a natural law, a fundamental truth of sound money. From the perspective of protocol innovators or new users, it operates as a highly extractive and suppressive barrier to progress and utility. The engine's classification will highlight this divergence from the claimed 'mountain' type.
 *
 * DIRECTIONALITY LOGIC:
 *   Early Bitcoin holders and the maximalist community are clear beneficiaries, as the immutability preserves their wealth and ideological purity. Protocol innovators, scalability solution developers, and new users seeking lower fees are victims, bearing the costs of limited flexibility and high transaction costs. Utility advocates are excluded, as their proposals are often rejected outright.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate, from the maximalist view, is to preserve Bitcoin's original vision of decentralized, immutable digital scarcity. This mandate is considered 'live' by its proponents. However, the high extractiveness and suppression suggest that while the founding problem (centralized monetary control) remains relevant, the maximalist solution may have accumulated extractive layers, potentially operating as a 'false summit' or 'tangled rope' for those outside the beneficiary group.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_constraint,
    'Is Bitcoin''s monetary immutability a genuine natural law (a ''mountain'') or a constructed constraint that primarily benefits early adopters and the maximalist community (a ''false summit'' or ''tangled rope'')?',
    'Analysis of the social and technical enforcement mechanisms: if persistence relies heavily on active community suppression and ideological framing rather than inherent technical necessity, it leans towards a constructed constraint.',
    'If reclassified as a constructed constraint, it would highlight the extractive nature of the arrangement for non-beneficiaries, shifting the policy debate from ''violation of natural law'' to ''renegotiation of social contract''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_constraint, conceptual, 'Ambiguity between inherent truth and socially constructed benefit.').

omega_variable(
    technical_necessity_vs_ideological_choice,
    'To what extent is the immutability of Bitcoin''s base layer a technical necessity for its security and decentralization, versus an ideological choice enforced by the maximalist community?',
    'Comparative analysis with other decentralized systems that allow for more flexible protocol evolution while maintaining security, or formal proofs of the trade-offs involved.',
    'If largely an ideological choice, it strengthens the case for re-evaluating the constraint''s benefits and costs, potentially leading to a reclassification away from ''mountain''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_necessity_vs_ideological_choice, empirical, 'Distinguishing technical limits from community-enforced dogma.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (protocol rules, technical difficulty of forks) or internalized (ideological pressure, social ostracization within the community)?',
    'Post-exit suppression trajectory for developers who attempt alternative forks: if social pressure and ideological condemnation persist and hinder adoption even after technical hurdles are overcome, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them after exit attempts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the Bitcoin community.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__maximalist_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bitc_tr_t3, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 3, 0.1).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 6, 0.1).
narrative_ontology:measurement(bitc_tr_t9, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 9, 0.1).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(bitc_tr_t15, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 15, 0.1).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(bitc_be_t3, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 3, 0.65).
narrative_ontology:measurement(bitc_be_t6, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 6, 0.7).
narrative_ontology:measurement(bitc_be_t9, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 9, 0.74).
narrative_ontology:measurement(bitc_be_t12, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 12, 0.76).
narrative_ontology:measurement(bitc_be_t15, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 15, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(bitc_su_t3, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 3, 0.75).
narrative_ontology:measurement(bitc_su_t6, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 6, 0.8).
narrative_ontology:measurement(bitc_su_t9, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 9, 0.82).
narrative_ontology:measurement(bitc_su_t12, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 12, 0.84).
narrative_ontology:measurement(bitc_su_t15, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 15, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__maximalist_reading, identity_coordination).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__pragmatic_synthesis).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__utility_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'bitcoin_consensus_kernel'. Each reading instantiates a distinct constraint with its own ε and classification, linked here as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
