% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__survival_competence_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_memory_preservation__survival_competence_reading
 *   human_readable: Catastrophe Memory Preservation (Survival Competence Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes a ritual practice, viewed through the lens of
 *   its function in preserving operational threat-recognition capacity across
 *   generations. It is a reading of the 'catastrophe_memory_preservation'
 *   kernel, focusing on the practical, survival-oriented aspects of the
 *   ritual. The ritual demands costly participation from the present
 *   generation (victims) to ensure the survival competence of future
 *   generations (beneficiaries). The constraint is classified as a Tangled
 *   Rope because it genuinely coordinates intergenerational knowledge
 *   transfer but does so through asymmetric extraction and active enforcement
 *   of adherence.
 *
 * KEY AGENTS:
 *   - present_generation_participants: Primary target (moderate/identity_locked) — bears extraction
 *   - future_generations: Primary beneficiary (powerless/analytical) — benefits from constraint
 *   - community_elders: Agenda setter (organized/constrained) — administers and enforces
 *   - external_observers: Analytical observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__survival_competence_reading, 0.78).
domain_priors:suppression_score(catastrophe_memory_preservation__survival_competence_reading, 0.65).
domain_priors:theater_ratio(catastrophe_memory_preservation__survival_competence_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__survival_competence_reading, "Catastrophe Memory Preservation (Survival Competence Reading)").
narrative_ontology:topic_domain(catastrophe_memory_preservation__survival_competence_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_preservation__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__survival_competence_reading, '139bd327-e606-435f-b287-bee22d8f108c').
narrative_ontology:cs_kernel_codification('139bd327-e606-435f-b287-bee22d8f108c', implicit).
narrative_ontology:cs_authority_grounding('139bd327-e606-435f-b287-bee22d8f108c', lineage).
narrative_ontology:cs_interpretation_layer_present('139bd327-e606-435f-b287-bee22d8f108c').
narrative_ontology:cs_reading_relation('139bd327-e606-435f-b287-bee22d8f108c', catastrophe_memory_preservation__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('139bd327-e606-435f-b287-bee22d8f108c', catastrophe_memory_preservation__hybrid_atrophy_reading, influences).
narrative_ontology:cs_axiom('139bd327-e606-435f-b287-bee22d8f108c', foundational, ritual_transfers_operational_knowledge).
narrative_ontology:cs_axiom_status(ritual_transfers_operational_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('139bd327-e606-435f-b287-bee22d8f108c', ritual_transfers_operational_knowledge, empirically_contingent).
narrative_ontology:cs_axiom('139bd327-e606-435f-b287-bee22d8f108c', foundational, collective_survival_demands_individual_sacrifice).
narrative_ontology:cs_axiom_status(collective_survival_demands_individual_sacrifice, holdable).
narrative_ontology:cs_axiom_grounding('139bd327-e606-435f-b287-bee22d8f108c', collective_survival_demands_individual_sacrifice, deontological).
narrative_ontology:cs_reference_frame('139bd327-e606-435f-b287-bee22d8f108c', pre_catastrophe_survival_protocols).
narrative_ontology:cs_drift_state('139bd327-e606-435f-b287-bee22d8f108c', contemporary_globalized_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('139bd327-e606-435f-b287-bee22d8f108c', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__survival_competence_reading, future_generations).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__survival_competence_reading, community_elders).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__survival_competence_reading, present_generation_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the direct costs of ritual participation: time, resources, emotional labor, and suppression of individual autonomy in favor of collective adherence. Their identity is often deeply intertwined with the community and its practices, making exit difficult despite the costs.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, present_generation_participants, payer,
    moderate, biographical, identity_locked, local).

% Are the intended beneficiaries of the ritual, receiving the transferred knowledge and operational competence for threat recognition and survival. They do not directly participate in the constraint's enforcement but benefit from its persistence.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, future_generations, beneficiary,
    powerless, generational, analytical, local).

% Administer and enforce the ritual, ensuring its fidelity and transmission. They benefit from the social cohesion and authority derived from their role as custodians of critical knowledge, but are also bound by the tradition's demands.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, community_elders, agenda_setter,
    organized, generational, constrained, local).

% Study the community's ritual practices, assessing their efficacy in transmitting knowledge and their social function. They are outside the direct influence of the constraint but can offer critical analysis.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, external_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the intergenerational transfer of critical survival knowledge and threat-recognition protocols, ensuring collective readiness for recurring catastrophes.
% TRANSFER_FUNCTION: Transfers operational knowledge, behavioral scripts, and emotional conditioning related to past catastrophes from older to younger generations, at the cost of present-generation autonomy and resources.
% ABSENT_VOICES: Individualists or rationalists who would question the efficacy or necessity of ritualized knowledge transfer, advocating for more 'efficient' or 'modern' educational methods. Their voices are often suppressed by the community's emphasis on tradition and collective survival.
% DISAPPEARANCE_RATIONALE: If the ritual vanished, the community would lose its primary mechanism for intergenerational knowledge transfer regarding catastrophe survival. Future generations would be less prepared, potentially leading to higher casualties or societal collapse during the next event, forcing a complete reorganization of survival strategies.
% FOUNDING_PROBLEM: A historical catastrophe (e.g., flood, famine, invasion) that devastated the community, leading to the establishment of rituals to prevent recurrence or mitigate impact.
% FOUNDING_PROBLEM_CORROBORATION: Community oral histories, archaeological evidence of past catastrophes, and ongoing environmental or geopolitical threats corroborate the founding problem's continued relevance. External anthropologists and historians also attest to the historical basis and the community's persistent vulnerability.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__survival_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__survival_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_preservation__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__survival_competence_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__survival_competence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_preservation__survival_competence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_preservation__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) due to the significant demands placed on present-generation participants, including time, resources, and the suppression of individual expression in favor of rigid adherence. Suppression (0.65) is moderate, maintained through social pressure, identity-locking mechanisms, and the perceived existential threat the ritual addresses. Theater ratio is low (0.20) because, from this reading's perspective, the ritual's core function of operational knowledge transfer is still highly active and effective, with minimal performative excess. The metrics reflect a system that is costly but functionally vital for survival.
 *
 * PERSPECTIVAL GAP:
 *   The present-generation participants experience this as a highly extractive and suppressive constraint, limiting their autonomy. In contrast, the community elders, while also bound by the tradition, perceive it as a necessary and beneficial coordination mechanism for collective survival. Future generations, as the ultimate beneficiaries, would likely view it as a foundational support, if they were able to articulate a perspective. The engine's per-seat classification will highlight these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Present-generation participants are the primary targets (high d) as they bear the direct costs and sacrifices. Future generations are the full beneficiaries (low d) as they receive the critical survival knowledge without direct cost. Community elders are beneficiaries of the constraint's persistence (social authority, continuity) but also bear administrative costs, placing them closer to symmetric (moderate d).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the constraint as pure extraction by emphasizing its genuine coordination function: the transfer of survival competence. It avoids the pitfall of assuming mandatrophy by asserting the founding problem is 'live' and the ritual's function is still operational, even if costly. The high extractiveness is seen as a necessary cost of this vital coordination, rather than a sign of atrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_efficacy_vs_symbolic_function,
    'Is the ritual primarily effective in transferring operational survival competence, or has its function largely shifted to symbolic mourning and identity formation?',
    'Empirical studies of post-catastrophe community responses, comparing outcomes in communities with and without such rituals, or assessing the actual application of ritual-transmitted knowledge in crisis simulations.',
    'If primarily symbolic, the extractiveness would be reclassified as pure extraction (Snare) with no genuine coordination function, aligning with the ''mourning_practice_reading''. If operational efficacy is confirmed, the Tangled Rope classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_efficacy_vs_symbolic_function, empirical, 'Distinguishes between the ritual''s claimed operational function and its potential symbolic-only function.').

omega_variable(
    identity_lock_vs_structural_suppression,
    'To what extent is the present generation''s ''identity_locked'' exit option a result of internalized identity fusion versus structural suppression by community norms and elder authority?',
    'Longitudinal studies of individuals who attempt to exit the community or modify ritual practices, observing the social and psychological costs incurred, and the mechanisms of enforcement applied by elders.',
    'If primarily internalized, the effective suppression is higher, as individuals carry the constraint with them. If primarily structural, interventions targeting elder authority or community norms would be more effective in reducing suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_suppression, empirical, 'Structural vs. internalized suppression mechanism for identity-locked participants.').

omega_variable(
    kernel_reading_divergence,
    'Given the ''catastrophe_memory_preservation'' kernel, how do the ''survival_competence_reading'', ''mourning_practice_reading'', and ''hybrid_atrophy_reading'' structurally differ in their assessment of the ritual''s primary function and extractiveness?',
    'Comparative analysis of each reading''s core axioms, stakeholder beneficiaries/victims, and claimed coordination functions, as instantiated in separate constraint stories.',
    'This omega documents the irreducible conceptual ambiguity at the kernel level. Resolution would clarify which reading best describes the constraint''s actual operation, potentially leading to reclassification of the ritual''s type depending on the dominant function (e.g., Snare if purely mourning, Piton if atrophied).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Documents the conceptual divergence between different readings of the catastrophe memory preservation kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__survival_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 20, 0.23).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 60, 0.21).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 80, 0.2).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 20, 0.72).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 40, 0.75).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 60, 0.77).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 80, 0.78).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 100, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 40, 0.63).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 60, 0.64).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 80, 0.65).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 100, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__survival_competence_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
