% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__trauma_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__trauma_encoding_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__trauma_encoding_reading
 *   human_readable: Ritual Encoding of Intergenerational Trauma as Warning System
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes how ritual practices within a community
 *   function to encode and transmit the psychological burden of
 *   intergenerational trauma, serving as a 'warning system' against future
 *   catastrophes. While intended to foster collective vigilance, this
 *   mechanism imposes significant psychological costs on descendants. This is
 *   one reading of the 'catastrophe_memory_kernel', focusing specifically on
 *   the trauma transmission aspect.
 *
 * KEY AGENTS:
 *   - descendants_bearing_trauma: Primary target (powerless/identity_locked) — bears psychological costs.
 *   - community_elders_and_ritual_leaders: Agenda setter (organized/constrained) — administers and transmits the ritual, deriving authority.
 *   - future_generations_collective_vigilance: Abstract beneficiary (analytical/analytical) — the systemic outcome of threat-detection capacity.
 *   - psychological_support_professionals: Observer (institutional/analytical) — analyzes impact, may advocate for change.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, 0.68).
domain_priors:suppression_score(catastrophe_memory_kernel__trauma_encoding_reading, 0.75).
domain_priors:theater_ratio(catastrophe_memory_kernel__trauma_encoding_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__trauma_encoding_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__trauma_encoding_reading, "Ritual Encoding of Intergenerational Trauma as Warning System").
narrative_ontology:topic_domain(catastrophe_memory_kernel__trauma_encoding_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__trauma_encoding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__trauma_encoding_reading, 'f690b047-67c3-4fea-a0c2-6c495bf7e0bc').
narrative_ontology:cs_kernel_codification('f690b047-67c3-4fea-a0c2-6c495bf7e0bc', formalized).
narrative_ontology:cs_authority_grounding('f690b047-67c3-4fea-a0c2-6c495bf7e0bc', lineage).
narrative_ontology:cs_interpretation_layer_present('f690b047-67c3-4fea-a0c2-6c495bf7e0bc').
narrative_ontology:cs_reading_relation('f690b047-67c3-4fea-a0c2-6c495bf7e0bc', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('f690b047-67c3-4fea-a0c2-6c495bf7e0bc', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('f690b047-67c3-4fea-a0c2-6c495bf7e0bc', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('f690b047-67c3-4fea-a0c2-6c495bf7e0bc', foundational, trauma_as_essential_warning).
narrative_ontology:cs_axiom_status(trauma_as_essential_warning, holdable).
narrative_ontology:cs_axiom_grounding('f690b047-67c3-4fea-a0c2-6c495bf7e0bc', trauma_as_essential_warning, empirically_contingent).
narrative_ontology:cs_axiom('f690b047-67c3-4fea-a0c2-6c495bf7e0bc', secondary, collective_memory_requires_affective_transmission).
narrative_ontology:cs_axiom_status(collective_memory_requires_affective_transmission, holdable).
narrative_ontology:cs_axiom_grounding('f690b047-67c3-4fea-a0c2-6c495bf7e0bc', collective_memory_requires_affective_transmission, conventional).
narrative_ontology:cs_reference_frame('f690b047-67c3-4fea-a0c2-6c495bf7e0bc', ancestral_catastrophe_response).
narrative_ontology:cs_drift_state('f690b047-67c3-4fea-a0c2-6c495bf7e0bc', contemporary_psychological_awareness, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f690b047-67c3-4fea-a0c2-6c495bf7e0bc', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__trauma_encoding_reading, future_generations_collective_vigilance).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__trauma_encoding_reading, descendants_bearing_trauma).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in rituals that re-enact or symbolize past catastrophes, internalizing the associated emotional and psychological burden. This burden is experienced as a cost, but is also fused with their collective identity, making exit from the ritual system difficult.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, descendants_bearing_trauma, payer,
    powerless, generational, identity_locked, local).

% Administer and transmit the rituals, believing they are preserving vital warnings for future generations. They enforce adherence to ritual forms and narratives, ensuring the 'warning system' remains intact. They bear the responsibility of transmission but also derive authority from it.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, community_elders_and_ritual_leaders, agenda_setter,
    organized, generational, constrained, local).

% The abstract capacity for collective threat-detection and preparedness that is maintained by the ritual's transmission of trauma. This 'beneficiary' is not an active agent but a systemic outcome, a form of collective early-warning capacity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, future_generations_collective_vigilance, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_kernel__trauma_encoding_reading, future_generations_collective_vigilance).

% Observe the psychological impact of these rituals on individuals and communities. They may advocate for therapeutic interventions or modifications to ritual practice to mitigate trauma, but do not directly participate in or enforce the constraint.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, psychological_support_professionals, observer,
    institutional, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective memory and vigilance against future threats by encoding past catastrophic experiences into ritual practices, ensuring that the 'lessons' of trauma are transmitted across generations.
% TRANSFER_FUNCTION: Transfers psychological burden and emotional resonance of past trauma from the originating generation to descendants, in exchange for a perceived increase in collective threat-vigilance and survival capacity.
% ABSENT_VOICES: Individual descendants who might wish to disengage from the traumatic memory or reinterpret it in a less burdensome way are often silenced by communal pressure and the perceived necessity of the warning system. Their voices are excluded by the collective imperative.
% DISAPPEARANCE_RATIONALE: If the ritual encoding of trauma vanished, the community would lose a significant part of its collective identity and its perceived early-warning system. While individuals might experience psychological relief, the collective would need to find new ways to transmit historical lessons and maintain vigilance, leading to a profound reorganization of social and psychological structures.
% FOUNDING_PROBLEM: The problem of ensuring the survival of a community after experiencing a catastrophic event, by preventing future generations from forgetting the lessons learned through immense suffering.
% FOUNDING_PROBLEM_CORROBORATION: Community elders and historians attest that the threat of similar catastrophes remains live, and that the rituals are essential for survival. External anthropologists and sociologists corroborate the historical trauma and the community's perception of ongoing vulnerability, even if they may question the efficacy or cost of the current ritual system.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__trauma_encoding_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__trauma_encoding_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__trauma_encoding_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_kernel__trauma_encoding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__trauma_encoding_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__trauma_encoding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__trauma_encoding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68) is high due to the psychological burden imposed on descendants, which is a direct cost of the 'warning system'. Suppression (0.75) is also high, as communal identity and social pressure make it difficult for individuals to opt out of the ritual system or challenge its traumatic content. The theater ratio (0.20) is relatively low, indicating that the rituals are genuinely believed to serve a functional purpose, even if that function comes at a high cost. The claimed type is 'tangled_rope' because it genuinely coordinates collective memory and vigilance (beneficiary: future_generations_collective_vigilance) but does so through an extractive mechanism (victim: descendants_bearing_trauma) that requires active enforcement (communal pressure, ritual adherence).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of community elders, the constraint is a necessary 'rope' for survival, ensuring the continuity of the group. From the perspective of descendants, it can feel like a 'snare' due to the involuntary psychological burden and the difficulty of exit. The engine's computation of per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Descendants bearing trauma are full targets (high d) due to the psychological costs and identity-locked exit. Community elders are agenda-setters, benefiting from the authority of transmission but also bearing the responsibility of maintaining the system (d closer to symmetric). The abstract 'collective vigilance' is a beneficiary (low d) as it is the intended positive outcome.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling it as a pure Snare, acknowledging the genuine coordination function of collective vigilance. However, it also highlights the asymmetric extraction and the need for active enforcement, preventing it from being mislabeled as a pure Rope. The 'live' status of the founding problem suggests that while the constraint is extractive, its mandate is not yet fully atrophied, though the 'contested' status of the founding problem corroboration indicates a growing challenge to its necessity in its current form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trauma_efficacy_vs_cost,
    'Does the psychological burden transmitted by the ritual genuinely enhance collective vigilance and survival, or is it a disproportionate cost for a diminishing return?',
    'Longitudinal studies comparing communities with similar historical traumas but different ritual practices, assessing actual threat-response efficacy against psychological well-being metrics.',
    'If the cost is disproportionate, the extractiveness of the constraint is higher than currently measured, and its coordination function is weaker, pushing it closer to a Snare. If efficacy is high, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trauma_efficacy_vs_cost, empirical, 'Assesses the functional utility of trauma transmission against its psychological cost.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (e.g., lack of alternative cultural frameworks) or internalized (e.g., identity fusion with the traumatic narrative)?',
    'Post-exit suppression trajectory: if individuals who leave the community or ritual system continue to experience the psychological burden and difficulty in forming new identities, it suggests a strong internalized component.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them after exit, making the constraint more resilient to external challenges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in ritual adherence.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint primarily a mechanism for encoding trauma as a warning system, or is that function secondary to other aspects of the catastrophe_memory_kernel?',
    'Analysis of community discourse and ritual exegesis: if the ''warning'' aspect is consistently foregrounded by participants and leaders, this reading is strongly supported. If other functions (e.g., identity, competence) are more prominent, this reading is less central.',
    'If this reading is less central, the overall extractiveness attributed to trauma transmission might be lower, and the constraint''s primary classification might shift towards a different reading of the kernel (e.g., symbol_continuity_reading).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms the centrality of the trauma-encoding function within the broader catastrophe_memory_kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__trauma_encoding_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 20, 0.23).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 60, 0.21).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 80, 0.2).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 60, 0.67).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 80, 0.68).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 40, 0.73).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 60, 0.74).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 80, 0.75).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 100, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__trauma_encoding_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four distinct readings of the 'catastrophe_memory_kernel', each with different ε values and structural properties. This reading focuses on the transmission of trauma for vigilance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
