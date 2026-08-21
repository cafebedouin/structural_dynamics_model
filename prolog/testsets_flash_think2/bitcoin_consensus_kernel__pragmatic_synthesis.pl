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
 *   constraint_id: bitcoin_consensus_kernel__pragmatic_synthesis
 *   human_readable: Bitcoin Pragmatic Synthesis: Base Layer Immutability, Upper Layer Innovation
 *   domain: cryptoeconomics/monetary_systems/distributed_consensus
 *
 * SUMMARY:
 *   This constraint represents the 'pragmatic synthesis' reading of the
 *   Bitcoin consensus kernel, which posits that the base layer's monetary
 *   rules are immutable, while upper layers are permitted to innovate without
 *   violating the kernel. It aims to resolve the tension between maximalist
 *   calls for absolute immutability and utility-driven demands for
 *   flexibility. The prompt explicitly labels this a 'low extractiveness
 *   scaffold,' implying it's a transitional support structure, despite its
 *   intent to be a lasting solution. The 'victim' is ideological coherence,
 *   as the synthesis inherently compromises strict adherence to any single,
 *   pure ideology.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__pragmatic_synthesis, 0.15).
domain_priors:suppression_score(bitcoin_consensus_kernel__pragmatic_synthesis, 0.4).
domain_priors:theater_ratio(bitcoin_consensus_kernel__pragmatic_synthesis, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, extractiveness, 0.15).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__pragmatic_synthesis, scaffold).
narrative_ontology:human_readable(bitcoin_consensus_kernel__pragmatic_synthesis, "Bitcoin Pragmatic Synthesis: Base Layer Immutability, Upper Layer Innovation").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__pragmatic_synthesis, "cryptoeconomics/monetary_systems/distributed_consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__pragmatic_synthesis).
narrative_ontology:has_sunset_clause(bitcoin_consensus_kernel__pragmatic_synthesis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__pragmatic_synthesis, '9ac22603-9c06-44aa-a234-2ed53ec09479').
narrative_ontology:cs_kernel_codification('9ac22603-9c06-44aa-a234-2ed53ec09479', fixed_text).
narrative_ontology:cs_authority_grounding('9ac22603-9c06-44aa-a234-2ed53ec09479', practice).
narrative_ontology:cs_interpretation_layer_present('9ac22603-9c06-44aa-a234-2ed53ec09479').
narrative_ontology:cs_reading_relation('9ac22603-9c06-44aa-a234-2ed53ec09479', bitcoin_consensus_kernel__maximalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('9ac22603-9c06-44aa-a234-2ed53ec09479', bitcoin_consensus_kernel__utility_reading, coexists_with).
narrative_ontology:cs_axiom('9ac22603-9c06-44aa-a234-2ed53ec09479', foundational, base_layer_immutability_principle).
narrative_ontology:cs_axiom_status(base_layer_immutability_principle, holdable).
narrative_ontology:cs_axiom_grounding('9ac22603-9c06-44aa-a234-2ed53ec09479', base_layer_immutability_principle, deontological).
narrative_ontology:cs_axiom('9ac22603-9c06-44aa-a234-2ed53ec09479', foundational, layered_innovation_necessity).
narrative_ontology:cs_axiom_status(layered_innovation_necessity, holdable).
narrative_ontology:cs_axiom_grounding('9ac22603-9c06-44aa-a234-2ed53ec09479', layered_innovation_necessity, instrumental).
narrative_ontology:cs_reference_frame('9ac22603-9c06-44aa-a234-2ed53ec09479', layered_protocol_evolution).
narrative_ontology:cs_drift_state('9ac22603-9c06-44aa-a234-2ed53ec09479', contemporary_cryptoeconomic_landscape, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9ac22603-9c06-44aa-a234-2ed53ec09479', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__pragmatic_synthesis, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__pragmatic_synthesis, layer_2_developers).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__pragmatic_synthesis, bitcoin_users).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__pragmatic_synthesis, maximalist_ideologues).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__pragmatic_synthesis, ideological_coherence).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__pragmatic_synthesis, utility_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the base layer protocol, enforcing the rules that define its immutability and the boundaries for upper-layer innovation. They are constrained by the existing codebase and community consensus.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, core_developers, agenda_setter,
    institutional, biographical, constrained, global).

% Benefit from the stability of the base layer while having the flexibility to innovate and build new functionalities on upper layers without needing to alter the core protocol. They can choose to build on other chains if this synthesis becomes too restrictive.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, layer_2_developers, beneficiary,
    organized, biographical, mobile, global).

% Benefit from the perceived security and immutability of the base layer, combined with the utility and scalability offered by upper-layer innovations. Their exit options are constrained by network effects and liquidity.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, bitcoin_users, beneficiary,
    moderate, biographical, constrained, global).

% Bear the cost of ideological compromise, as this synthesis deviates from their strict interpretation of Bitcoin's immutable nature. They are often excluded from decision-making processes regarding upper-layer development and see any innovation as a potential violation of the founding covenant.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, maximalist_ideologues, payer,
    powerless, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__pragmatic_synthesis, maximalist_ideologues, excluded).

% Benefit from the pragmatic approach that allows for necessary innovation and scalability, aligning with their view that Bitcoin should evolve to meet user needs. They can also build on other platforms if this synthesis proves too slow or conservative.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, utility_advocates, beneficiary,
    organized, biographical, mobile, global).

% Observe the evolution of Bitcoin's architecture, assessing its implications for financial stability, consumer protection, and illicit finance. They can impose external constraints or recognize certain layers for specific regulatory purposes.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_consensus_kernel__pragmatic_synthesis, diffuse).
narrative_ontology:fixing_cost_class(bitcoin_consensus_kernel__pragmatic_synthesis, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the development and adoption of Bitcoin by providing a stable, immutable base layer for monetary policy while allowing for flexible innovation and scalability on upper layers, thereby bridging ideological divides.
% TRANSFER_FUNCTION: Transfers the burden of innovation and feature development from the highly conservative base layer to more agile upper layers, while transferring perceived monetary stability and security to the base layer.
% ABSENT_VOICES: Those who advocate for fundamental changes to the base layer (e.g., different monetary policy, new cryptographic primitives) are largely excluded from the 'pragmatic synthesis' discussion, as it prioritizes base layer immutability. Their arguments are often dismissed as violating core tenets.
% DISAPPEARANCE_RATIONALE: If this pragmatic synthesis vanished, the Bitcoin community would likely fracture into irreconcilable camps: maximalists rejecting all innovation, and utility advocates abandoning Bitcoin for more flexible chains. This would severely hinder Bitcoin's development, adoption, and long-term viability as a global monetary system, leading to a significant reorganization of the cryptoeconomic landscape.
% FOUNDING_PROBLEM: The inherent tension between Bitcoin's foundational principle of immutable, decentralized monetary policy and the practical necessity for scalability, privacy, and feature innovation to achieve widespread adoption.
% FOUNDING_PROBLEM_CORROBORATION: Independent cryptographers, economists, and blockchain researchers consistently highlight the ongoing challenge of balancing immutability with innovation in decentralized systems. Academic papers and industry reports from outside the immediate Bitcoin community corroborate this persistent tension.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__pragmatic_synthesis, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__pragmatic_synthesis, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__pragmatic_synthesis, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(bitcoin_consensus_kernel__pragmatic_synthesis, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_consensus_kernel__pragmatic_synthesis, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   The low extractiveness (0.15) reflects that this synthesis primarily serves a coordination function, enabling diverse development without direct rent-seeking from the core. Suppression (0.40) is moderate, as active enforcement is required to maintain the distinction between layers and prevent base layer changes. Theater ratio (0.10) is low, as the synthesis is a genuine attempt at a functional compromise, not mere performance. The 'scaffold' classification, as per the prompt, implies a sunset clause, which is interpreted as the point where the community either fully converges on a new, stable architecture or resolves the underlying tension in a more fundamental way.
 *
 * PERSPECTIVAL GAP:
 *   Maximalist ideologues perceive this synthesis as a betrayal of Bitcoin's core principles, experiencing it as an extractive compromise. Utility advocates and Layer 2 developers, however, see it as a necessary and beneficial coordination mechanism. The engine's classification will highlight this divergence, showing a low-extraction scaffold from the perspective of beneficiaries, but a more extractive or constraining force for those whose ideological positions are compromised.
 *
 * DIRECTIONALITY LOGIC:
 *   Layer 2 developers and Bitcoin users are beneficiaries, gaining from both stability and innovation. Maximalist ideologues are victims, as their strict interpretation is compromised, leading to a loss of ideological purity. Core developers act as agenda-setters, mediating this synthesis. The 'ideological coherence' victim is a conceptual cost borne by the community's internal consistency.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    synthesis_transitional_or_permanent,
    'Is this ''pragmatic synthesis'' truly a transitional scaffold, or has it become a permanent, self-justifying structure for managing Bitcoin''s evolution?',
    'Analysis of community discourse and development roadmaps for explicit sunset conditions or long-term architectural goals that supersede this synthesis. If no such conditions or goals emerge over a decade, reclassify as a more permanent type (e.g., Tangled Rope).',
    'If permanent, the ''scaffold'' classification is misleading, and the constraint''s true nature might be a Tangled Rope (if extraction persists) or a Rope (if it genuinely coordinates without significant extraction). The ''has_sunset_clause'' would become theatrical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(synthesis_transitional_or_permanent, conceptual, 'Whether the synthesis is a temporary bridge or a stable state.').

omega_variable(
    ideological_coherence_as_victim,
    'Is ''ideological coherence'' a genuine victim of this synthesis, or is its perceived loss merely a re-framing of the debate by those who resist compromise?',
    'Sociological and philosophical analysis of the community''s shared values and narratives. If a new, coherent, and widely accepted ideology emerges that incorporates the synthesis, then ''ideological coherence'' is no longer a victim. If fragmentation persists, it remains a victim.',
    'If not a genuine victim, the ''extraction'' from maximalist ideologues might be overstated, potentially lowering the overall extractiveness and shifting the classification towards a pure Rope. If it is a genuine victim, the cost of compromise is real.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ideological_coherence_as_victim, conceptual, 'The nature of ideological coherence as a victim.').

omega_variable(
    base_layer_immutability_degree,
    'To what extent is the Bitcoin base layer truly immutable, or does its ''immutability'' rely on social consensus that could shift?',
    'Empirical observation of successful or attempted base layer protocol changes over time, and analysis of the social and economic costs associated with such changes. A successful, contentious change would demonstrate a lower degree of true immutability.',
    'If the base layer is less immutable than claimed, the ''pragmatic synthesis'' might be built on a weaker foundation, increasing its fragility and potentially leading to higher future extractiveness if changes are forced through.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(base_layer_immutability_degree, empirical, 'The actual degree of base layer immutability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__pragmatic_synthesis, 2015, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t2015, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 2015, 0.08).
narrative_ontology:measurement(bitc_tr_t2020, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 2020, 0.09).
narrative_ontology:measurement(bitc_tr_t2025, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 2025, 0.1).
narrative_ontology:measurement(bitc_tr_t2030, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 2030, 0.11).
narrative_ontology:measurement(bitc_tr_t2035, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 2035, 0.12).

% Extraction over time
narrative_ontology:measurement(bitc_be_t2015, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 2015, 0.12).
narrative_ontology:measurement(bitc_be_t2020, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 2020, 0.14).
narrative_ontology:measurement(bitc_be_t2025, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 2025, 0.15).
narrative_ontology:measurement(bitc_be_t2030, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 2030, 0.16).
narrative_ontology:measurement(bitc_be_t2035, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 2035, 0.17).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t2015, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 2015, 0.35).
narrative_ontology:measurement(bitc_su_t2020, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 2020, 0.38).
narrative_ontology:measurement(bitc_su_t2025, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 2025, 0.4).
narrative_ontology:measurement(bitc_su_t2030, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 2030, 0.42).
narrative_ontology:measurement(bitc_su_t2035, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 2035, 0.43).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__pragmatic_synthesis, resource_allocation).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__pragmatic_synthesis, bitcoin_consensus_kernel__maximalist_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__pragmatic_synthesis, bitcoin_consensus_kernel__utility_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'bitcoin_consensus_kernel', representing a pragmatic synthesis between maximalist and utility-driven interpretations. It is linked to its sibling readings, 'maximalist_reading' and 'utility_reading', as they all address the same core tension from different perspectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
