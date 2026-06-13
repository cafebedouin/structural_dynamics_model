% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__hybrid_near_miss_learning
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_avoidance_retention__hybrid_near_miss_learning, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_avoidance_retention__hybrid_near_miss_learning
 *   human_readable: Hybrid Near-Miss Learning for Catastrophe Avoidance
 *   domain: safety_engineering/organizational_learning/high_reliability_systems
 *
 * SUMMARY:
 *   This constraint describes the necessity of a 'hybrid' approach to
 *   maintaining competence in high-reliability systems, combining distributed
 *   learning from near-misses and foreign incidents with high-realism drills.
 *   It posits that neither pure simulation nor waiting for actual
 *   catastrophes is sufficient. This reading emphasizes the role of
 *   incident-sharing networks and cross-organizational learning as critical
 *   for competence retention, contrasting with views that prioritize either
 *   simulation or catastrophe as the primary learning mechanism.
 *
 * KEY AGENTS:
 *   - high_reliability_organizations: Agenda-setter (institutional/constrained) — designs and implements learning systems
 *   - safety_regulators: Beneficiary (institutional/analytical) — benefits from reduced risk, mandates practices
 *   - public_at_large: Beneficiary (powerless/trapped) — ultimate recipient of safety benefits
 *   - incident_investigators: Agenda-setter (organized/mobile) — provides critical data for learning
 *   - organizational_culture_resistors: Payer (moderate/constrained) — bears the cost of adapting to a learning culture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.15).
domain_priors:suppression_score(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.2).
domain_priors:theater_ratio(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, extractiveness, 0.15).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__hybrid_near_miss_learning, rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__hybrid_near_miss_learning, "Hybrid Near-Miss Learning for Catastrophe Avoidance").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__hybrid_near_miss_learning, "safety_engineering/organizational_learning/high_reliability_systems").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__hybrid_near_miss_learning).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__hybrid_near_miss_learning, 'ad1f04c5-24e9-47b1-9cc4-2ca3e60b1e4b').
narrative_ontology:cs_kernel_codification('ad1f04c5-24e9-47b1-9cc4-2ca3e60b1e4b', implicit).
narrative_ontology:cs_authority_grounding('ad1f04c5-24e9-47b1-9cc4-2ca3e60b1e4b', expertise).
narrative_ontology:cs_interpretation_layer_present('ad1f04c5-24e9-47b1-9cc4-2ca3e60b1e4b').
narrative_ontology:cs_reading_relation('ad1f04c5-24e9-47b1-9cc4-2ca3e60b1e4b', catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, coexists_with).
narrative_ontology:cs_reading_relation('ad1f04c5-24e9-47b1-9cc4-2ca3e60b1e4b', catastrophe_avoidance_retention__catastrophe_as_necessary_selector, coexists_with).
narrative_ontology:cs_axiom('ad1f04c5-24e9-47b1-9cc4-2ca3e60b1e4b', foundational, distributed_learning_is_sufficient).
narrative_ontology:cs_axiom_status(distributed_learning_is_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('ad1f04c5-24e9-47b1-9cc4-2ca3e60b1e4b', distributed_learning_is_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('ad1f04c5-24e9-47b1-9cc4-2ca3e60b1e4b', foundational, catastrophe_is_avoidable_through_learning).
narrative_ontology:cs_axiom_status(catastrophe_is_avoidable_through_learning, holdable).
narrative_ontology:cs_axiom_grounding('ad1f04c5-24e9-47b1-9cc4-2ca3e60b1e4b', catastrophe_is_avoidable_through_learning, empirically_contingent).
narrative_ontology:cs_reference_frame('ad1f04c5-24e9-47b1-9cc4-2ca3e60b1e4b', continuous_adaptive_safety).
narrative_ontology:cs_drift_state('ad1f04c5-24e9-47b1-9cc4-2ca3e60b1e4b', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ad1f04c5-24e9-47b1-9cc4-2ca3e60b1e4b', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, high_reliability_organizations).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, safety_regulators).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, public_at_large).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, organizational_culture_resistors).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__hybrid_near_miss_learning, organizational_learning_theory).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__hybrid_near_miss_learning, resilience_engineering_principles).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These organizations (e.g., aviation, nuclear power) actively design and implement systems for learning from near-misses, foreign incidents, and high-realism drills. They invest heavily in incident reporting, analysis, and training to maintain competence and avoid catastrophic failures. Their reputation and continued operation depend on this learning.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, high_reliability_organizations, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the reduced risk of catastrophic events and the stability of the industries they oversee. They often mandate or encourage these learning practices, using incident data to inform policy and oversight. They rely on the organizations' commitment to this learning for effective regulation.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, safety_regulators, beneficiary,
    institutional, generational, analytical, national).

% Are the ultimate beneficiaries of effective catastrophe avoidance, as their safety and well-being are protected. They bear no direct cost for the learning system but would suffer the consequences of its failure. Their trust in these systems is crucial for their social license to operate.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, public_at_large, beneficiary,
    powerless, generational, trapped, global).

% Professionals who analyze near-misses and incidents to extract lessons. Their work directly feeds the distributed learning process, identifying systemic vulnerabilities and best practices. Their expertise is critical to the effectiveness of the constraint.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, incident_investigators, agenda_setter,
    organized, biographical, mobile, global).

% Individuals or sub-groups within organizations who resist transparent incident reporting, blame-free analysis, or participation in drills due to fear of reprisal, complacency, or perceived burden. They bear the 'cost' of adapting to a learning culture.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, organizational_culture_resistors, payer,
    moderate, immediate, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the distributed collection, analysis, and dissemination of safety-critical information across different organizations and timeframes, enabling collective learning from diverse sources (near-misses, foreign incidents, drills) to prevent catastrophic failures.
% TRANSFER_FUNCTION: Transfers knowledge, best practices, and risk awareness from specific incidents and simulations to the broader organizational and industry-wide competence base, from those who experience or simulate incidents to those who need to avoid them.
% ABSENT_VOICES: Organizations or industries that lack robust incident-sharing networks and a culture of distributed learning are effectively absent from this conversation. They would advocate for less investment in 'non-catastrophic' learning, but their silence is often a symptom of their vulnerability.
% DISAPPEARANCE_RATIONALE: If this distributed learning constraint vanished, high-reliability organizations would rapidly lose their adaptive capacity. Competence would degrade, leading to an increase in catastrophic failures as organizations would only learn from their own, often fatal, mistakes. Industries like aviation would become significantly more dangerous.
% FOUNDING_PROBLEM: The recognition that learning solely from actual catastrophes is too costly and slow, and that learning solely from isolated simulations is insufficient to capture real-world complexity and emergent risks.
% FOUNDING_PROBLEM_CORROBORATION: Safety experts, accident investigators, and academic researchers consistently corroborate the ongoing need for hybrid learning approaches, citing historical examples of industries that failed to learn effectively and the continuous evolution of complex system risks. This is attested by independent bodies like NTSB reports and academic safety journals.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__hybrid_near_miss_learning, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__hybrid_near_miss_learning, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__hybrid_near_miss_learning, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_avoidance_retention__hybrid_near_miss_learning, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__hybrid_near_miss_learning_tests).
:- end_tests(catastrophe_avoidance_retention__hybrid_near_miss_learning_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Rope because it genuinely solves a collective action problem (catastrophe avoidance) with net benefits for participants and society. Extractiveness is low (0.15) as the costs are primarily investments in safety, not rents. Suppression is low (0.2) because participation is largely driven by self-preservation and regulatory alignment, not coercion, though some cultural resistance exists. Theater ratio is low (0.1) as the learning activities are genuinely functional. Accessibility collapse is high (0.8) because once the necessity of this hybrid learning is understood, alternatives (pure simulation or pure catastrophe learning) are seen as inadequate. Resistance is low (0.1) due to the clear benefits of avoiding catastrophe.
 *
 * PERSPECTIVAL GAP:
 *   High-reliability organizations and safety regulators largely share a perspective on the necessity and benefits of this constraint, experiencing it as a coordination mechanism. Organizational culture resistors, however, may perceive it as an extractive burden due to the effort and cultural change required, even if the long-term benefits are clear. The public experiences it as a background condition of safety.
 *
 * DIRECTIONALITY LOGIC:
 *   High-reliability organizations and safety regulators are beneficiaries (d near 0.0) as they directly benefit from enhanced safety and stability. The public is also a beneficiary. Organizational culture resistors are payers (d near 1.0) as they bear the immediate costs of compliance and cultural change, even if the ultimate outcome is beneficial.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling genuine, ongoing coordination (hybrid learning) as a Snare or Piton. The founding problem (insufficient learning from single sources) is still live, and the constraint actively addresses it, indicating it has not atrophied. The benefits of catastrophe avoidance are continuously realized, justifying the ongoing investment and coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    learning_network_efficacy,
    'How effective are existing incident-sharing networks and cross-organizational learning mechanisms in different industries?',
    'Comparative studies of safety outcomes and learning rates across industries with varying levels of network integration (e.g., aviation vs. healthcare).',
    'If networks are found to be weak or ineffective in certain sectors, the constraint''s ''hybrid'' function is degraded, potentially shifting its classification towards a more extractive or theatrical type for those sectors, as the coordination benefits are not fully realized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(learning_network_efficacy, empirical, 'Empirical effectiveness of distributed learning networks.').

omega_variable(
    simulation_fidelity_threshold,
    'At what level of fidelity and realism do simulations become functionally equivalent to real catastrophic events for competence maintenance?',
    'Neurocognitive and behavioral studies comparing responses to high-fidelity simulations versus actual incidents, or longitudinal studies tracking competence decay in simulated vs. real-world contexts.',
    'If a high-fidelity threshold is empirically established, it would strengthen the ''simulation_as_proxy_catastrophe'' reading, potentially influencing resource allocation away from distributed incident learning. If no such threshold exists, it reinforces the ''hybrid'' reading''s claim that simulation alone is insufficient.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_fidelity_threshold, empirical, 'Functional equivalence of simulation to catastrophe.').

omega_variable(
    catastrophe_necessity_ambiguity,
    'Is there an irreducible component of ''selection pressure'' or ''organizational trauma'' from actual catastrophes that cannot be replicated by near-miss learning or simulation?',
    'Longitudinal studies of organizational resilience and adaptation following actual catastrophes versus prolonged periods of near-miss learning, focusing on deep cultural and structural changes.',
    'If such an irreducible component exists, it would lend credence to the ''catastrophe_as_necessary_selector'' reading, suggesting that some level of catastrophic failure is unavoidable for ultimate competence, challenging the ''hybrid'' reading''s premise of full avoidance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(catastrophe_necessity_ambiguity, conceptual, 'Irreducible necessity of actual catastrophes for competence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__hybrid_near_miss_learning, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t1970, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(cata_tr_t1980, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 1980, 0.08).
narrative_ontology:measurement(cata_tr_t1990, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(cata_tr_t2000, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(cata_tr_t2010, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(cata_tr_t2024, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(cata_be_t1970, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 1970, 0.1).
narrative_ontology:measurement(cata_be_t1980, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 1980, 0.12).
narrative_ontology:measurement(cata_be_t1990, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 1990, 0.13).
narrative_ontology:measurement(cata_be_t2000, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 2000, 0.14).
narrative_ontology:measurement(cata_be_t2010, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 2010, 0.15).
narrative_ontology:measurement(cata_be_t2024, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t1970, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 1970, 0.15).
narrative_ontology:measurement(cata_su_t1980, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 1980, 0.18).
narrative_ontology:measurement(cata_su_t1990, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 1990, 0.2).
narrative_ontology:measurement(cata_su_t2000, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 2000, 0.2).
narrative_ontology:measurement(cata_su_t2010, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 2010, 0.2).
narrative_ontology:measurement(cata_su_t2024, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__hybrid_near_miss_learning, information_standard).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_avoidance_retention__catastrophe_as_necessary_selector).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'catastrophe_avoidance_retention' kernel. This 'hybrid_near_miss_learning' reading emphasizes distributed learning from incidents and drills, contrasting with readings that prioritize pure simulation or actual catastrophes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
