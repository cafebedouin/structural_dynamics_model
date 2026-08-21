% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__symbol_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__symbol_continuity_reading, []).

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
 *   constraint_id: catastrophe_memory_transmission__symbol_continuity_reading
 *   human_readable: Catastrophe Memory Transmission: Symbol Continuity Reading
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   This constraint describes a community's commitment to transmitting the
 *   memory of a past catastrophe through strict adherence to symbolic ritual
 *   forms. The 'symbol continuity' reading emphasizes the intrinsic value of
 *   preserving these forms for communal identity and mourning, even at the
 *   cost of adaptive capacity. It is one reading of the broader
 *   'catastrophe_memory_transmission' kernel. The constraint is claimed as a
 *   Tangled Rope because it genuinely coordinates identity and meaning
 *   (beneficiary: communal_identity_continuity) but does so through
 *   asymmetric extraction from adaptive capacity and individual autonomy
 *   (victims).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__symbol_continuity_reading, 0.65).
domain_priors:suppression_score(catastrophe_memory_transmission__symbol_continuity_reading, 0.7).
domain_priors:theater_ratio(catastrophe_memory_transmission__symbol_continuity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__symbol_continuity_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__symbol_continuity_reading, "Catastrophe Memory Transmission: Symbol Continuity Reading").
narrative_ontology:topic_domain(catastrophe_memory_transmission__symbol_continuity_reading, "religious_studies/collective_memory/ritual_studies").

domain_priors:requires_active_enforcement(catastrophe_memory_transmission__symbol_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__symbol_continuity_reading, 'd020c7ef-d4f9-4746-8155-b8d70ce499d0').
narrative_ontology:cs_kernel_codification('d020c7ef-d4f9-4746-8155-b8d70ce499d0', implicit).
narrative_ontology:cs_authority_grounding('d020c7ef-d4f9-4746-8155-b8d70ce499d0', practice).
narrative_ontology:cs_interpretation_layer_present('d020c7ef-d4f9-4746-8155-b8d70ce499d0').
narrative_ontology:cs_reading_relation('d020c7ef-d4f9-4746-8155-b8d70ce499d0', catastrophe_memory_transmission__operational_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('d020c7ef-d4f9-4746-8155-b8d70ce499d0', catastrophe_memory_transmission__hybrid_embedded_reading, coexists_with).
narrative_ontology:cs_axiom('d020c7ef-d4f9-4746-8155-b8d70ce499d0', foundational, symbolic_form_is_identity).
narrative_ontology:cs_axiom_status(symbolic_form_is_identity, holdable).
narrative_ontology:cs_axiom_grounding('d020c7ef-d4f9-4746-8155-b8d70ce499d0', symbolic_form_is_identity, deontological).
narrative_ontology:cs_axiom('d020c7ef-d4f9-4746-8155-b8d70ce499d0', secondary, fidelity_ensures_survival).
narrative_ontology:cs_axiom_status(fidelity_ensures_survival, holdable).
narrative_ontology:cs_axiom_grounding('d020c7ef-d4f9-4746-8155-b8d70ce499d0', fidelity_ensures_survival, theological).
narrative_ontology:cs_reference_frame('d020c7ef-d4f9-4746-8155-b8d70ce499d0', unbroken_symbolic_lineage).
narrative_ontology:cs_drift_state('d020c7ef-d4f9-4746-8155-b8d70ce499d0', contemporary_globalized_era, gap(repudiation_pressure, minor, false)).
narrative_ontology:cs_created_at('d020c7ef-d4f9-4746-8155-b8d70ce499d0', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, communal_identity_continuity).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, adaptive_capacity).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, individual_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The abstract entity representing the unbroken thread of communal identity and shared meaning, which is preserved and reinforced by the strict adherence to symbolic ritual forms. It 'benefits' by its continued existence and coherence.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, communal_identity_continuity, beneficiary,
    institutional, generational, identity_locked, local).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_transmission__symbol_continuity_reading, communal_identity_continuity).

% The members of the community who actively perform and transmit the rituals. They enforce fidelity to symbolic forms, believing this is essential for communal survival and identity. Their own identity is often deeply intertwined with their role in ritual transmission.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, ritual_practitioners, agenda_setter,
    organized, biographical, identity_locked, local).

% The abstract capacity of the community to innovate, change, and respond flexibly to new environmental or social challenges. It 'pays' by being constrained or sacrificed in favor of preserving fixed symbolic forms, even when those forms are no longer optimally suited for current conditions.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, adaptive_capacity, payer,
    powerless, generational, trapped, local).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_transmission__symbol_continuity_reading, adaptive_capacity).

% The capacity of individual members to question, reinterpret, or deviate from established ritual forms. It 'pays' by being suppressed in favor of collective symbolic coherence, leading to potential internal conflict or alienation for those who seek change.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, individual_autonomy, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_transmission__symbol_continuity_reading, individual_autonomy).

% Academics and researchers who study the community's ritual practices from an external, analytical perspective. They can identify the trade-offs between symbolic fidelity and adaptive capacity, but do not directly participate in or enforce the constraint.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, external_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective memory and identity through shared symbolic forms, ensuring a coherent narrative of past catastrophe and a unified communal response to grief and meaning-making.
% TRANSFER_FUNCTION: Transfers a sense of belonging, shared history, and existential meaning from the ritual to its participants, in exchange for strict adherence to prescribed symbolic actions and narratives.
% ABSENT_VOICES: Those who advocate for radical ritual innovation or adaptation to changing circumstances are often marginalized or excluded, as their proposals threaten the perceived integrity of the symbolic form. Their voices would argue for greater flexibility and responsiveness to the present.
% DISAPPEARANCE_RATIONALE: If the constraint of symbolic continuity vanished, the community's shared identity and memory of catastrophe would fragment, leading to a loss of cohesion and a re-evaluation of its core values and practices. The social fabric would need to re-form around new, potentially diverse, meaning-making systems.
% FOUNDING_PROBLEM: The community faced an existential threat or catastrophic loss, leading to a need for a mechanism to preserve collective memory, identity, and a shared framework for mourning and survival.
% FOUNDING_PROBLEM_CORROBORATION: Community elders and historians attest that the threat of identity dissolution and loss of meaning remains, making the ritual's function continuously vital. External anthropologists corroborate the historical role of such rituals in post-catastrophe communities, even if they question the current degree of fidelity.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__symbol_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__symbol_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__symbol_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_transmission__symbol_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__symbol_continuity_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__symbol_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_transmission__symbol_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_transmission__symbol_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because the cost of maintaining strict symbolic fidelity (in terms of lost adaptive capacity) is substantial. Suppression (0.7) is also high, as deviation from established forms is actively discouraged to preserve communal coherence. Theater ratio is low (0.2) because the ritual is genuinely functional for identity maintenance, not merely performative. The metrics show a slight increase in extractiveness and suppression over time as the community's commitment to symbolic fidelity hardens.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of ritual practitioners, the constraint is a necessary Rope, ensuring the survival of the community's soul. From the perspective of adaptive capacity or individual autonomy, it is a Snare, trapping the community in rigid forms that hinder its ability to respond to new challenges. The engine's computation will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Communal identity continuity is the primary beneficiary (d=0.0), as its existence is directly tied to the ritual's preservation. Ritual practitioners, whose identities are fused with this role, also benefit (d=0.1-0.2). Adaptive capacity and individual autonomy are the primary targets (d=0.8-0.9), as they are constrained or sacrificed for the sake of symbolic fidelity. External observers are analytical (d=0.5).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbolic_vs_operational_value,
    'To what extent is the symbolic fidelity of the ritual truly distinct from its operational competence-transmitting function?',
    'Comparative analysis with communities that have adapted rituals while maintaining identity, or longitudinal studies tracking the impact of ritual changes on both identity coherence and practical survival outcomes.',
    'If the functions are highly separable, the extraction from adaptive capacity is more clearly a cost of identity maintenance. If they are deeply intertwined (as in the ''hybrid_embedded_reading''), the extraction might be re-evaluated as a necessary cost of a more complex coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_vs_operational_value, conceptual, 'Distinguishing the intrinsic symbolic value from any embedded practical utility.').

omega_variable(
    identity_lock_strength,
    'How strong is the identity-lock on ritual practitioners and individual members? Is it primarily professional identity, relational identity, ideological identity, or institutional identity?',
    'Sociological studies, ethnographic observation, and interviews exploring the self-concept and social consequences of deviating from ritual norms.',
    'If the identity-lock is primarily ideological or relational, exit options are more constrained, increasing effective extraction. If it''s more institutional, changes in leadership or external pressure might more easily shift the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_strength, empirical, 'Understanding the specific mechanism binding agents to the ritual.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (e.g., social ostracism for deviation) or internalized (e.g., individuals self-censor due to deep belief in the ritual''s necessity)?',
    'Post-exit suppression trajectory: if suppression persists (e.g., guilt, self-doubt) after an individual leaves the community or deviates from the ritual, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the constraint more resilient to external challenge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in ritual adherence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__symbol_continuity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__symbol_continuity_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
