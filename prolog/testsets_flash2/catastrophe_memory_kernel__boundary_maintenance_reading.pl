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
 *   constraint_id: catastrophe_memory_kernel__boundary_maintenance_reading
 *   human_readable: Catastrophe Memory Kernel: Boundary Maintenance Reading
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes a reading of the 'catastrophe_memory_kernel'
 *   where ritualized mourning primarily functions to enforce group
 *   boundaries. The shared practice of remembering a past catastrophe is
 *   leveraged to define who belongs to the group and to reinforce collective
 *   identity, often at the expense of individual expression and relations
 *   with external groups. The constraint is claimed as a Tangled Rope because
 *   it offers genuine coordination (group cohesion) but with significant,
 *   asymmetric extraction (conformity, exclusion).
 *
 * KEY AGENTS:
 *   - in_group_members: Primary beneficiary (organized/identity_locked) — benefits from cohesion, pays with conformity
 *   - individual_autonomy: Primary victim (powerless/identity_locked) — bears conformity costs
 *   - out_group_relations: Secondary victim (powerless/trapped) — bears exclusion costs
 *   - ritual_leaders: Agenda setter (institutional/constrained) — enforces ritual, gains authority
 *   - historical_scholars: Analytical observer (analytical/analytical) — analyzes social function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__boundary_maintenance_reading, 0.65).
domain_priors:suppression_score(catastrophe_memory_kernel__boundary_maintenance_reading, 0.7).
domain_priors:theater_ratio(catastrophe_memory_kernel__boundary_maintenance_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__boundary_maintenance_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__boundary_maintenance_reading, "Catastrophe Memory Kernel: Boundary Maintenance Reading").
narrative_ontology:topic_domain(catastrophe_memory_kernel__boundary_maintenance_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__boundary_maintenance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__boundary_maintenance_reading, 'e84f4634-70fe-4bd6-9182-8d822e37820d').
narrative_ontology:cs_kernel_codification('e84f4634-70fe-4bd6-9182-8d822e37820d', implicit).
narrative_ontology:cs_authority_grounding('e84f4634-70fe-4bd6-9182-8d822e37820d', practice).
narrative_ontology:cs_interpretation_layer_present('e84f4634-70fe-4bd6-9182-8d822e37820d').
narrative_ontology:cs_reading_relation('e84f4634-70fe-4bd6-9182-8d822e37820d', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('e84f4634-70fe-4bd6-9182-8d822e37820d', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('e84f4634-70fe-4bd6-9182-8d822e37820d', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_axiom('e84f4634-70fe-4bd6-9182-8d822e37820d', foundational, group_identity_requires_clear_boundaries).
narrative_ontology:cs_axiom_status(group_identity_requires_clear_boundaries, holdable).
narrative_ontology:cs_axiom_grounding('e84f4634-70fe-4bd6-9182-8d822e37820d', group_identity_requires_clear_boundaries, conventional).
narrative_ontology:cs_axiom('e84f4634-70fe-4bd6-9182-8d822e37820d', foundational, shared_catastrophe_memory_defines_us).
narrative_ontology:cs_axiom_status(shared_catastrophe_memory_defines_us, holdable).
narrative_ontology:cs_axiom_grounding('e84f4634-70fe-4bd6-9182-8d822e37820d', shared_catastrophe_memory_defines_us, theological).
narrative_ontology:cs_reference_frame('e84f4634-70fe-4bd6-9182-8d822e37820d', cohesive_group_identity).
narrative_ontology:cs_drift_state('e84f4634-70fe-4bd6-9182-8d822e37820d', contemporary_pluralistic_society, gap(repudiation_pressure, minor, false)).
narrative_ontology:cs_created_at('e84f4634-70fe-4bd6-9182-8d822e37820d', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, in_group_members).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, individual_autonomy).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, out_group_relations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from strong group cohesion, shared identity, and a clear sense of belonging derived from participating in the mourning rituals. They experience social support and a reinforced sense of collective purpose, but pay with conformity pressure.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, in_group_members, beneficiary,
    organized, generational, identity_locked, local).

% Bears the cost of conformity pressure, suppression of individual dissent, and the emotional labor of performing prescribed grief. Individuals who question the ritual's form or meaning face social ostracization or internal conflict.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, individual_autonomy, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_kernel__boundary_maintenance_reading, individual_autonomy).

% Suffers from the exclusionary nature of the ritual, which reinforces 'us vs. them' narratives and can lead to strained or hostile relationships with external communities. The ritual actively defines who is 'in' and who is 'out'.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, out_group_relations, payer,
    powerless, generational, trapped, local).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_kernel__boundary_maintenance_reading, out_group_relations).

% Administer and enforce the correct performance of the mourning rituals, ensuring adherence to tradition and maintaining the group's boundaries. They derive authority and status from their role in preserving the collective memory and identity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, ritual_leaders, agenda_setter,
    institutional, generational, constrained, local).

% Analyze the historical development and social function of such rituals, often noting their role in identity formation and boundary maintenance, sometimes critically assessing their exclusionary aspects.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, historical_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective identity and social cohesion by providing a shared framework for remembering a catastrophe, ensuring that group boundaries are clearly defined and maintained through common practice.
% TRANSFER_FUNCTION: Transfers social capital, belonging, and reinforced identity to in-group members, while transferring conformity pressure and exclusion to individual autonomy and out-group relations.
% ABSENT_VOICES: Individuals seeking greater personal expression in mourning, or those advocating for more inclusive inter-group relations, are often marginalized or silenced by the strong emphasis on collective conformity and boundary maintenance within the ritual structure.
% DISAPPEARANCE_RATIONALE: If the ritual vanished, the group's cohesion would weaken, its boundaries would blur, and the collective memory of the catastrophe would fragment, leading to a significant reorganization of social structures and identity.
% FOUNDING_PROBLEM: The problem of maintaining group identity and cohesion in the face of external threats or internal fragmentation, particularly after a shared traumatic event.
% FOUNDING_PROBLEM_CORROBORATION: Ritual leaders and many in-group members attest that the problem of group cohesion and identity is still live, citing ongoing external pressures. Historical scholars corroborate that such rituals indeed serve this function, though they may question the necessity of their exclusionary aspects.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__boundary_maintenance_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__boundary_maintenance_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__boundary_maintenance_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_kernel__boundary_maintenance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__boundary_maintenance_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is moderate (0.65) because while the ritual provides a valuable coordination function (group cohesion), it also imposes significant costs in terms of individual freedom and external relations. Suppression is high (0.70) due to the strong social pressure to conform to ritual practices and the active exclusion of dissenting voices or alternative interpretations. Theater ratio is low (0.20) as the ritual's function is genuinely performed, not merely for show, though its stated purpose (pure remembrance) may mask its boundary-enforcing role. The time series shows a slight increase in extractiveness and suppression as the ritual becomes more entrenched over time.
 *
 * PERSPECTIVAL GAP:
 *   In-group members experience the ritual as a source of solidarity and belonging, while individuals within the group who value autonomy, and those outside the group, experience it as a mechanism of exclusion and conformity. The ritual leaders perceive it as essential for group survival and identity, justifying the costs.
 *
 * DIRECTIONALITY LOGIC:
 *   In-group members are beneficiaries (low d) due to the strong sense of belonging and social support. Individual autonomy and out-group relations are victims (high d) as they bear the costs of conformity and exclusion. Ritual leaders, as agenda setters, benefit from the authority and stability the ritual provides (low d).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as Tangled Rope prevents mislabeling this as a pure Rope (ignoring extraction) or a pure Snare (ignoring coordination). The ritual's mandate to maintain group identity is still live, but the means of doing so have become substantially extractive, leveraging shared memory for social control and boundary enforcement. The identity-lock on in-group members ensures persistence despite the costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ritual_function_ambiguity,
    'Is the primary function of the mourning ritual genuine remembrance and healing, or is it primarily boundary maintenance and social control?',
    'Comparative analysis of similar groups with different ritual structures: if groups with less rigid mourning rituals maintain cohesion, it suggests boundary maintenance is not the sole or primary function.',
    'If primarily remembrance, extractiveness would be lower, and the constraint might reclassify as a Rope. If primarily social control, the current Tangled Rope classification is reinforced, potentially shifting towards Snare if coordination is minimal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_function_ambiguity, conceptual, 'Ambiguity in the ritual''s core purpose.').

omega_variable(
    internalized_suppression_degree,
    'To what extent is the suppression of individual autonomy structural (social ostracization) versus internalized (self-censorship, guilt)?',
    'Post-exit suppression trajectory: if individuals who leave the group continue to experience self-censorship or guilt regarding the ritual, it indicates a high degree of internalized suppression.',
    'If internalized suppression is high, the effective suppression is greater than the structural measure suggests, making exit more difficult and the constraint more resilient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_degree, empirical, 'Structural vs. internalized suppression mechanism for individual autonomy.').

omega_variable(
    kernel_reading_distinction,
    'This constraint is one reading of the ''catastrophe_memory_kernel''. What specific structural elements would change if a sibling reading (e.g., ''survival_competence_reading'') were adopted as primary?',
    'Detailed comparative analysis of the ritual''s emphasis: if the ritual''s focus shifts from ''who belongs'' to ''how to survive,'' it indicates a change in the primary structural element.',
    'Adopting ''survival_competence_reading'' would likely reduce the emphasis on exclusion and conformity, potentially lowering extractiveness and suppression, and shifting the primary beneficiary from ''in_group_cohesion'' to ''adaptive_capacity''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Distinguishing this reading from sibling readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__boundary_maintenance_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 30, 0.19).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 40, 0.69).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__boundary_maintenance_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'catastrophe_memory_kernel', each representing a distinct structural claim about the function of shared mourning practice. Other readings include 'symbol_continuity_reading', 'survival_competence_reading', and 'trauma_encoding_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
