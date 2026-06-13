% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__utility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   human_readable: Bitcoin Consensus Kernel (Utility Reading)
 *   domain: cryptoeconomics/monetary_systems/distributed_consensus
 *
 * SUMMARY:
 *   This constraint represents the 'utility reading' of the Bitcoin consensus
 *   kernel, where the whitepaper is understood as establishing a minimum
 *   viable consensus mechanism designed for iterative improvement. This
 *   reading prioritizes the network's long-term utility, scalability, and
 *   adaptability through mechanisms like soft forks and layer-2 protocols,
 *   viewing them as legitimate evolution rather than violations of a fixed
 *   covenant. It contrasts with more rigid interpretations by allowing for a
 *   moderate degree of protocol change to enhance functionality and adoption.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__utility_reading, 0.45).
domain_priors:suppression_score(bitcoin_consensus_kernel__utility_reading, 0.3).
domain_priors:theater_ratio(bitcoin_consensus_kernel__utility_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__utility_reading, rope).
narrative_ontology:human_readable(bitcoin_consensus_kernel__utility_reading, "Bitcoin Consensus Kernel (Utility Reading)").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__utility_reading, "cryptoeconomics/monetary_systems/distributed_consensus").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__utility_reading, '8310cb74-4bcb-433e-a3f8-41be1149eaf7').
narrative_ontology:cs_kernel_codification('8310cb74-4bcb-433e-a3f8-41be1149eaf7', fixed_text).
narrative_ontology:cs_authority_grounding('8310cb74-4bcb-433e-a3f8-41be1149eaf7', practice).
narrative_ontology:cs_interpretation_layer_present('8310cb74-4bcb-433e-a3f8-41be1149eaf7').
narrative_ontology:cs_reading_relation('8310cb74-4bcb-433e-a3f8-41be1149eaf7', bitcoin_consensus_kernel__maximalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('8310cb74-4bcb-433e-a3f8-41be1149eaf7', bitcoin_consensus_kernel__pragmatic_synthesis, coexists_with).
narrative_ontology:cs_axiom('8310cb74-4bcb-433e-a3f8-41be1149eaf7', foundational, protocol_evolution_is_legitimate).
narrative_ontology:cs_axiom_status(protocol_evolution_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('8310cb74-4bcb-433e-a3f8-41be1149eaf7', protocol_evolution_is_legitimate, instrumental).
narrative_ontology:cs_axiom('8310cb74-4bcb-433e-a3f8-41be1149eaf7', secondary, utility_drives_adoption).
narrative_ontology:cs_axiom_status(utility_drives_adoption, holdable).
narrative_ontology:cs_axiom_grounding('8310cb74-4bcb-433e-a3f8-41be1149eaf7', utility_drives_adoption, empirically_contingent).
narrative_ontology:cs_reference_frame('8310cb74-4bcb-433e-a3f8-41be1149eaf7', minimum_viable_consensus_for_evolution).
narrative_ontology:cs_drift_state('8310cb74-4bcb-433e-a3f8-41be1149eaf7', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8310cb74-4bcb-433e-a3f8-41be1149eaf7', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, adopters).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, builders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, layer_2_developers).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, monetary_ossification_guarantees).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__utility_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness (0.45) is moderate, reflecting the 'cost' of maintaining a decentralized, evolving system (e.g., transaction fees, development overhead, the 'victimization' of absolute ossification guarantees). Suppression (0.30) is low, as this reading encourages participation in the evolution process rather than strictly enforcing a static state. Theater ratio (0.10) is low, indicating that most activity is genuinely functional, focused on development and improvement. The claimed type is 'rope' because it facilitates coordination among a broad set of beneficiaries (adopters, builders) for a common good (a useful, evolving monetary system) with relatively low coercive overhead, though it does 'extract' from the idea of absolute immutability.
 *
 * PERSPECTIVAL GAP:
 *   Adopters and builders experience this as a beneficial, adaptive framework, while maximalist ideologues (who are 'payers' in this context) perceive it as a deviation from core principles. Core developers, as agenda-setters, navigate between these perspectives, aiming for changes that enhance utility without compromising fundamental security. The engine's per-seat classification would reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Adopters, builders, and layer-2 developers are clear beneficiaries (low d) as the constraint enables their activities and provides a platform for growth. 'Monetary ossification guarantees' is an abstract victim (high d) because this reading directly challenges its absolute nature. Maximalist ideologues are also payers (high d) as their rigid worldview is 'extracted from' by the flexibility of this reading. Core developers, while agenda-setters, also bear the responsibility of maintaining the system, placing them closer to symmetric (moderate d) in their relationship to the constraint's evolution.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading inherently guards against mandatrophy by embracing iterative improvement. The 'founding problem' (decentralized digital cash) is considered 'live,' and the consensus mechanism is seen as a tool to continuously address it, preventing the constraint from becoming a 'piton' (atrophied function) or a 'snare' (pure extraction) by adapting to new challenges and opportunities. The moderate extractiveness is seen as a necessary cost of this ongoing adaptation, not a sign of decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    utility_vs_immutability_tension,
    'Is the pursuit of utility and iterative improvement fundamentally compatible with Bitcoin''s core value proposition of immutable monetary policy, or does it introduce an irreducible tension?',
    'Long-term observation of network security, decentralization metrics, and user adoption trends in response to protocol changes. If utility-driven changes consistently lead to centralization or security vulnerabilities, the tension is real.',
    'If incompatible, the ''utility reading'' might be reclassified as a ''tangled_rope'' or ''snare'' from the perspective of those prioritizing immutability, as it would be seen as extracting from the core promise. If compatible, it reinforces the ''rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(utility_vs_immutability_tension, conceptual, 'The inherent tension between evolving utility and absolute immutability in a decentralized monetary system.').

omega_variable(
    soft_fork_governance_legitimacy,
    'Does the current soft fork governance process genuinely represent broad consensus and decentralized decision-making, or is it susceptible to capture by a small group of core developers or powerful mining pools?',
    'Empirical analysis of soft fork activation patterns, developer influence, and miner signaling behavior. Examination of dissenting voices and their ability to block or propose alternatives.',
    'If captured, the ''rope'' classification would shift towards ''tangled_rope'' or ''snare'' for those excluded from decision-making, as the ''coordination'' would be revealed as asymmetric extraction of control. If genuinely decentralized, it strengthens the ''rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(soft_fork_governance_legitimacy, empirical, 'The true decentralization and legitimacy of the soft fork governance process.').

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a genuine ''utility reading'' of the Bitcoin consensus kernel, or is it a ''pragmatic synthesis'' reading that merely tolerates utility on upper layers while maintaining base layer immutability?',
    'Analysis of core developer statements and implemented soft forks: if base layer monetary rules are ever modified (beyond bug fixes) for utility, it''s a utility reading. If utility is strictly confined to layer-2, it''s pragmatic synthesis.',
    'If it''s a pragmatic synthesis, the extractiveness from ''monetary_ossification_guarantees'' would be lower, and the ''maximalist_ideologues'' would be less ''victimized'', potentially shifting the classification closer to a ''mountain'' for the base layer.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Distinguishing between a utility-first interpretation and a layered immutability approach.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__utility_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_consensus_kernel__utility_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(bitc_tr_t3, bitcoin_consensus_kernel__utility_reading, theater_ratio, 3, 0.09).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_consensus_kernel__utility_reading, theater_ratio, 6, 0.09).
narrative_ontology:measurement(bitc_tr_t9, bitcoin_consensus_kernel__utility_reading, theater_ratio, 9, 0.1).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_consensus_kernel__utility_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(bitc_tr_t15, bitcoin_consensus_kernel__utility_reading, theater_ratio, 15, 0.1).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bitc_be_t3, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(bitc_be_t6, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 6, 0.4).
narrative_ontology:measurement(bitc_be_t9, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 9, 0.42).
narrative_ontology:measurement(bitc_be_t12, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 12, 0.44).
narrative_ontology:measurement(bitc_be_t15, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 15, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(bitc_su_t3, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 3, 0.22).
narrative_ontology:measurement(bitc_su_t6, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 6, 0.25).
narrative_ontology:measurement(bitc_su_t9, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 9, 0.27).
narrative_ontology:measurement(bitc_su_t12, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 12, 0.29).
narrative_ontology:measurement(bitc_su_t15, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 15, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__utility_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(bitcoin_consensus_kernel__utility_reading, 0.15).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel__maximalist_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel__pragmatic_synthesis).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'bitcoin_consensus_kernel'. This 'utility_reading' emphasizes iterative improvement and layer-2 solutions, contrasting with the 'maximalist_reading' (immutable monetary policy) and the 'pragmatic_synthesis' (base layer immutable, upper layers flexible).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
