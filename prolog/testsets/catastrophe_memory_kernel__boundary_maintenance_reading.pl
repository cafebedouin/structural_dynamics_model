% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__boundary_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__boundary_maintenance_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__boundary_maintenance_reading
 *   human_readable: Catastrophe Memory Ritual as Boundary Maintenance
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes how ritualized mourning practices, particularly
 *   those commemorating a historical catastrophe, function to enforce group
 *   boundaries. By dictating who participates, how they participate, and what
 *   narratives are permissible, the ritual strengthens in-group cohesion and
 *   identity, often at the cost of individual autonomy and open relations
 *   with out-groups. It is a reading of the broader
 *   'catastrophe_memory_kernel' which can also be read as encoding survival
 *   competence, ensuring symbolic continuity, or encoding trauma.
 *
 * KEY AGENTS:
 *   - in_group_members: Primary beneficiary (institutional/constrained) — benefits from cohesion, pays in conformity
 *   - group_leadership: Agenda setter (institutional/arbitrage) — administers ritual, benefits from group solidarity
 *   - individual_autonomy: Primary victim (powerless/identity_locked) — bears conformity costs
 *   - out_group_relations: Victim (powerless/trapped) — bears exclusion costs
 *   - anthropologists: Analytical observer (analytical/analytical) — studies ritual function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__boundary_maintenance_reading, 0.6).
domain_priors:suppression_score(catastrophe_memory_kernel__boundary_maintenance_reading, 0.7).
domain_priors:theater_ratio(catastrophe_memory_kernel__boundary_maintenance_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__boundary_maintenance_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__boundary_maintenance_reading, "Catastrophe Memory Ritual as Boundary Maintenance").
narrative_ontology:topic_domain(catastrophe_memory_kernel__boundary_maintenance_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__boundary_maintenance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__boundary_maintenance_reading, '760d5734-1622-46c7-a203-1e893895079c').
narrative_ontology:cs_kernel_codification('760d5734-1622-46c7-a203-1e893895079c', implicit).
narrative_ontology:cs_authority_grounding('760d5734-1622-46c7-a203-1e893895079c', practice).
narrative_ontology:cs_interpretation_layer_present('760d5734-1622-46c7-a203-1e893895079c').
narrative_ontology:cs_reading_relation('760d5734-1622-46c7-a203-1e893895079c', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('760d5734-1622-46c7-a203-1e893895079c', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('760d5734-1622-46c7-a203-1e893895079c', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_axiom('760d5734-1622-46c7-a203-1e893895079c', foundational, group_identity_requires_distinct_memory).
narrative_ontology:cs_axiom_status(group_identity_requires_distinct_memory, holdable).
narrative_ontology:cs_axiom_grounding('760d5734-1622-46c7-a203-1e893895079c', group_identity_requires_distinct_memory, conventional).
narrative_ontology:cs_axiom('760d5734-1622-46c7-a203-1e893895079c', secondary, ritual_enforces_social_boundaries).
narrative_ontology:cs_axiom_status(ritual_enforces_social_boundaries, holdable).
narrative_ontology:cs_axiom_grounding('760d5734-1622-46c7-a203-1e893895079c', ritual_enforces_social_boundaries, empirically_contingent).
narrative_ontology:cs_reference_frame('760d5734-1622-46c7-a203-1e893895079c', cohesive_group_identity).
narrative_ontology:cs_drift_state('760d5734-1622-46c7-a203-1e893895079c', contemporary_globalized_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('760d5734-1622-46c7-a203-1e893895079c', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, in_group_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, group_leadership).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, individual_autonomy).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, out_group_relations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates shared identity and group cohesion by providing a common framework for remembering and responding to a historical catastrophe, reinforcing who 'we' are.
% TRANSFER_FUNCTION: Transfers social capital and belonging to conforming in-group members, while transferring conformity costs and exclusion to individuals seeking autonomy or out-groups.
% ABSENT_VOICES: Individuals who question the prescribed narrative or seek to forge connections outside the group are often marginalized or silenced; out-group members who wish to engage with the memory are excluded from participation, preventing alternative interpretations or reconciliation.
% DISAPPEARANCE_RATIONALE: If the ritual and its enforcement vanished, the group's boundaries would blur, internal cohesion would weaken, and individual members would have greater freedom to define their own identities and relationships, leading to a significant reorganization of social structures.
% FOUNDING_PROBLEM: The problem of maintaining a distinct group identity and cohesion, and preventing assimilation, in the aftermath of a shared historical catastrophe.
% FOUNDING_PROBLEM_CORROBORATION: Group leadership and many in-group members attest that the problem of maintaining identity and cohesion is still live, citing ongoing external pressures and internal generational shifts. Anthropologists and sociologists, from outside the benefiting parties, corroborate that the ritual actively serves this function, even if they dispute its necessity or ethical implications.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__boundary_maintenance_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__boundary_maintenance_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__boundary_maintenance_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_kernel__boundary_maintenance_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__boundary_maintenance_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__boundary_maintenance_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__boundary_maintenance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) reflects the cost of conformity and exclusion required to maintain group boundaries. Suppression (0.7) is high due to strong social pressure and potential ostracization for non-compliance. The theater ratio (0.2) is relatively low, as the ritual's boundary-maintenance function is genuinely active, though some elements might be performative. The claimed type is 'tangled_rope' because it provides the coordination function of group cohesion (beneficiary: in_group_members) but extracts from individual autonomy and out-group relations through active enforcement of ritual norms.
 *
 * PERSPECTIVAL GAP:
 *   In-group members and leadership perceive the ritual as a vital coordination mechanism for group survival and identity. Individual members seeking greater autonomy or out-group members attempting to bridge divides experience it as an extractive and suppressive force. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Group leadership and in-group members are beneficiaries, as the ritual reinforces their collective identity and power structure. Individual autonomy and out-group relations are victims, as they are constrained or excluded by the ritual's boundary-enforcing function. The active enforcement of ritual norms ensures these flows.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is to maintain group identity and cohesion in the face of historical catastrophe. While the catastrophe itself may be in the distant past, the 'problem' of maintaining group boundaries and identity is considered live by the leadership. The classification as a Tangled Rope prevents mislabeling it as a pure Rope (ignoring extraction) or a Snare (ignoring the genuine coordination of in-group cohesion).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint primarily about boundary maintenance, or is it a different reading of the catastrophe_memory_kernel?',
    'Empirical observation of ritual function: if the primary effect is exclusion and conformity, this reading is supported. If the primary effect is symbolic continuity, survival competence, or trauma encoding, a different reading is more appropriate.',
    'If reclassified to symbol_continuity_reading, extractiveness would likely be lower (more Rope-like); if to trauma_encoding_reading, suppression might be higher (internalized).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'This constraint is the boundary_maintenance_reading of the catastrophe_memory_kernel. Sibling readings include symbol_continuity_reading, survival_competence_reading, and trauma_encoding_reading. This reading emphasizes the social function of exclusion and conformity.').

omega_variable(
    internalized_vs_structural_suppression,
    'Is the suppression of individual autonomy primarily structural (external group pressure) or internalized (self-censorship due to identity fusion)?',
    'Post-exit trajectory: if conformity pressure persists after an individual leaves the group, it suggests internalized suppression. If it dissipates, it''s primarily structural.',
    'If internalized, the effective suppression is higher than the structural measure suggests, as the individual carries the constraint with them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Ambiguity in the mechanism of suppression for individual autonomy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__boundary_maintenance_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 20, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__boundary_maintenance_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__trauma_encoding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four distinct readings of the 'catastrophe_memory_kernel', each with different structural properties and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
