% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__pragmatic_synthesis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_consensus_kernel__pragmatic_synthesis, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: bitcoin_consensus_kernel__pragmatic_synthesis
 *   human_readable: Bitcoin Pragmatic Synthesis: Base Layer Immutability, Upper Layer Innovation
 *   domain: cryptoeconomics/monetary_systems/distributed_consensus
 *
 * SUMMARY:
 *   This constraint represents the 'pragmatic synthesis' reading of the
 *   Bitcoin consensus kernel, which posits that the base layer's monetary
 *   rules are immutable, while upper layers are free to innovate without
 *   violating the kernel. It's a compromise position aimed at resolving the
 *   tension between maximalist calls for absolute immutability and
 *   utility-driven demands for flexibility. The constraint functions as a
 *   scaffold, providing a framework for development that is intended to be
 *   transitional until a stable, widely accepted layered architecture is
 *   achieved, or the underlying ideological conflict is resolved.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__pragmatic_synthesis, 0.25).
domain_priors:suppression_score(bitcoin_consensus_kernel__pragmatic_synthesis, 0.35).
domain_priors:theater_ratio(bitcoin_consensus_kernel__pragmatic_synthesis, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, extractiveness, 0.25).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__pragmatic_synthesis, scaffold).
narrative_ontology:human_readable(bitcoin_consensus_kernel__pragmatic_synthesis, "Bitcoin Pragmatic Synthesis: Base Layer Immutability, Upper Layer Innovation").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__pragmatic_synthesis, "cryptoeconomics/monetary_systems/distributed_consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__pragmatic_synthesis).
narrative_ontology:has_sunset_clause(bitcoin_consensus_kernel__pragmatic_synthesis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__pragmatic_synthesis, 'e440b7b9-a4a4-4d66-82ee-6ca46214a3d2').
narrative_ontology:cs_kernel_codification('e440b7b9-a4a4-4d66-82ee-6ca46214a3d2', fixed_text).
narrative_ontology:cs_authority_grounding('e440b7b9-a4a4-4d66-82ee-6ca46214a3d2', practice).
narrative_ontology:cs_interpretation_layer_present('e440b7b9-a4a4-4d66-82ee-6ca46214a3d2').
narrative_ontology:cs_reading_relation('e440b7b9-a4a4-4d66-82ee-6ca46214a3d2', bitcoin_consensus_kernel__maximalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e440b7b9-a4a4-4d66-82ee-6ca46214a3d2', bitcoin_consensus_kernel__utility_reading, coexists_with).
narrative_ontology:cs_axiom('e440b7b9-a4a4-4d66-82ee-6ca46214a3d2', foundational, base_layer_immutability_is_sacrosanct).
narrative_ontology:cs_axiom_status(base_layer_immutability_is_sacrosanct, holdable).
narrative_ontology:cs_axiom_grounding('e440b7b9-a4a4-4d66-82ee-6ca46214a3d2', base_layer_immutability_is_sacrosanct, conventional).
narrative_ontology:cs_axiom('e440b7b9-a4a4-4d66-82ee-6ca46214a3d2', foundational, upper_layer_innovation_is_essential).
narrative_ontology:cs_axiom_status(upper_layer_innovation_is_essential, holdable).
narrative_ontology:cs_axiom_grounding('e440b7b9-a4a4-4d66-82ee-6ca46214a3d2', upper_layer_innovation_is_essential, instrumental).
narrative_ontology:cs_reference_frame('e440b7b9-a4a4-4d66-82ee-6ca46214a3d2', layered_architecture_stability).
narrative_ontology:cs_drift_state('e440b7b9-a4a4-4d66-82ee-6ca46214a3d2', contemporary_development_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e440b7b9-a4a4-4d66-82ee-6ca46214a3d2', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__pragmatic_synthesis, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__pragmatic_synthesis, layer_2_developers).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__pragmatic_synthesis, bitcoin_users).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__pragmatic_synthesis, utility_advocates).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__pragmatic_synthesis, maximalist_ideologues).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__pragmatic_synthesis, ideological_coherence).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__pragmatic_synthesis, bitcoin_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the Bitcoin base layer protocol, defining the rules for immutability and the interfaces for upper layers. They enforce the separation of concerns, resisting base-layer changes while supporting layer-2 innovation.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, bitcoin_core_developers, agenda_setter,
    institutional, generational, constrained, global).

% Build applications and protocols on top of Bitcoin's base layer, benefiting from its security and immutability while having the flexibility to innovate without altering the core. They are net beneficiaries of this synthesis.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, layer_2_developers, beneficiary,
    organized, biographical, mobile, global).

% Benefit from the stability and security of the base layer and the utility provided by upper-layer innovations. They indirectly pay for the complexity of a layered system and the ideological friction it entails.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, bitcoin_users, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__pragmatic_synthesis, bitcoin_users, payer).

% Adhere to a strict interpretation of Bitcoin's whitepaper, viewing any innovation or compromise as a violation of its founding principles. They bear the cost of ideological compromise and feel excluded from the direction of development.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, maximalist_ideologues, payer,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__pragmatic_synthesis, maximalist_ideologues, excluded).

% Seek to maximize Bitcoin's utility and scalability through iterative improvements and new features. They benefit from the pragmatic synthesis as it allows for innovation, even if it doesn't go as far as they might wish for base-layer flexibility.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, utility_advocates, beneficiary,
    organized, biographical, mobile, global).

% The abstract concept of a unified, internally consistent Bitcoin ideology. It 'pays' the cost of compromise, as the pragmatic synthesis inherently involves balancing conflicting ideals, leading to a less coherent, more fragmented ideological landscape.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, ideological_coherence, payer,
    powerless, generational, trapped, universal).
narrative_ontology:stakeholder_non_agent(bitcoin_consensus_kernel__pragmatic_synthesis, ideological_coherence).

% Study the evolution of Bitcoin's consensus mechanisms and social layers, analyzing the trade-offs and long-term implications of the pragmatic synthesis. They are outside the direct economic flows but provide critical commentary.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, analytical_observers, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate development efforts by clearly separating the immutable base layer from flexible upper layers, allowing innovation without compromising the core monetary properties of Bitcoin.
% TRANSFER_FUNCTION: Transfers the burden of feature development and scalability from the highly conservative base layer to more agile upper layers. It also transfers the cost of ideological compromise to those who prefer absolute purity or absolute flexibility.
% ABSENT_VOICES: Those who believe the base layer *must* evolve to remain relevant (e.g., for quantum resistance) are marginalized, as are those who believe *any* innovation beyond the base layer is a dangerous distraction from its core purpose.
% DISAPPEARANCE_RATIONALE: If the pragmatic synthesis vanished, the Bitcoin ecosystem would face immediate and severe ideological conflict. Either the base layer would become completely ossified, stifling all innovation, or it would become subject to uncontrolled changes, undermining its core value proposition. The layered architecture would collapse, and the entire development paradigm would need to be re-established.
% FOUNDING_PROBLEM: The fundamental tension between Bitcoin's design for immutable, scarce digital money and the practical need for scalability, new features, and broader utility in a rapidly evolving technological landscape.
% FOUNDING_PROBLEM_CORROBORATION: Academic cryptographers, blockchain architects, and independent economic analysts consistently highlight this ongoing tension. While the specific solutions are debated, the existence of the problem itself is widely acknowledged outside of purely maximalist or utility-focused camps.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__pragmatic_synthesis, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__pragmatic_synthesis, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__pragmatic_synthesis, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(bitcoin_consensus_kernel__pragmatic_synthesis, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_consensus_kernel__pragmatic_synthesis, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_consensus_kernel__pragmatic_synthesis_tests).
:- end_tests(bitcoin_consensus_kernel__pragmatic_synthesis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a scaffold due to its transitional nature in mediating ideological conflict and enabling a path forward for development. Extractiveness is low (0.25) because the primary goal is coordination and enabling innovation, not rent-seeking. Suppression (0.35) is moderate, as it actively enforces the boundary between layers and suppresses attempts to change the base layer, but it does not suppress innovation itself. Theater ratio is low (0.15) as it represents a genuine, functional compromise. Resistance is moderate (0.55) due to ongoing ideological pushback from both maximalist and more radical utility-focused camps. Accessibility collapse is moderate (0.45) because while the base layer is fixed, the upper layers offer significant freedom.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Layer 2 developers and utility advocates, this synthesis is a beneficial framework enabling progress. From the maximalist perspective, it is a deviation from core principles, a form of extraction of ideological purity. The engine's classification will highlight this divergence, showing a beneficial scaffold for some seats and a more extractive or constraining type for others.
 *
 * DIRECTIONALITY LOGIC:
 *   Layer 2 developers and general Bitcoin users are beneficiaries, gaining from the stability and innovation. Utility advocates also benefit from the flexibility this synthesis allows. Maximalist ideologues are payers, as they bear the cost of compromise and feel their ideal of absolute purity is violated. The abstract 'ideological coherence' is also a victim, as the synthesis inherently fragments the ideological landscape. Bitcoin Core developers act as agenda-setters, defining and enforcing the boundaries of this synthesis.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    immutability_social_construct_vs_technical_fact,
    'Is the ''immutability'' of Bitcoin''s base layer a purely technical property, or is it a social construct maintained by consensus and developer practice?',
    'Analysis of historical hard forks and contentious soft forks: if ''immutability'' has demonstrably shifted due to social consensus, it supports the social construct view.',
    'If primarily a social construct, the ''immutability'' claim has a higher theater ratio and is more susceptible to political pressure, potentially reclassifying the base layer itself as a Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immutability_social_construct_vs_technical_fact, conceptual, 'Ambiguity of base layer immutability''s grounding.').

omega_variable(
    sustainability_of_layer_separation,
    'Is the clear separation between the immutable base layer and flexible upper layers sustainable in the long term, or will pressure for base-layer changes eventually become irresistible?',
    'Longitudinal study of technological evolution and user demands: if critical functionalities consistently require base-layer changes, the separation is unsustainable.',
    'If unsustainable, the scaffold''s transitional nature becomes more pronounced, and its eventual failure could lead to a Snare (if base-layer changes are forced extractively) or a Piton (if the system ossifies).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sustainability_of_layer_separation, empirical, 'Long-term viability of the layered architecture.').

omega_variable(
    cost_of_ideological_compromise,
    'What is the true cost of the ideological compromise embodied by the pragmatic synthesis, particularly in terms of community fragmentation and decision-making friction?',
    'Sociological studies of the Bitcoin community, analysis of governance proposals, and tracking of developer exodus/entry patterns.',
    'A high cost would indicate that the ''scaffold'' is less efficient than claimed, potentially pushing its extractiveness upward and its coordination function downward, making it closer to a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_of_ideological_compromise, empirical, 'Quantifying the cost of ideological fragmentation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__pragmatic_synthesis, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bitc_tr_t10, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 10, 0.12).
narrative_ontology:measurement(bitc_tr_t20, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 20, 0.13).
narrative_ontology:measurement(bitc_tr_t30, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 30, 0.14).
narrative_ontology:measurement(bitc_tr_t40, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 40, 0.15).
narrative_ontology:measurement(bitc_tr_t50, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(bitc_be_t10, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(bitc_be_t20, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 20, 0.23).
narrative_ontology:measurement(bitc_be_t30, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 30, 0.24).
narrative_ontology:measurement(bitc_be_t40, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 40, 0.25).
narrative_ontology:measurement(bitc_be_t50, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 50, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(bitc_su_t10, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 10, 0.32).
narrative_ontology:measurement(bitc_su_t20, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 20, 0.33).
narrative_ontology:measurement(bitc_su_t30, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 30, 0.34).
narrative_ontology:measurement(bitc_su_t40, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 40, 0.35).
narrative_ontology:measurement(bitc_su_t50, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 50, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__pragmatic_synthesis, enforcement_mechanism).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__pragmatic_synthesis, bitcoin_consensus_kernel__maximalist_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__pragmatic_synthesis, bitcoin_consensus_kernel__utility_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'bitcoin_consensus_kernel', which also includes 'maximalist_reading' and 'utility_reading'. This 'pragmatic_synthesis' reading attempts to mediate between the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
