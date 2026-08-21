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
 *   constraint_id: catastrophe_memory_preservation__survival_competence_reading
 *   human_readable: Ritual Preserves Operational Threat-Recognition Capacity (Survival Competence Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes a ritual practice, interpreted as actively
 *   preserving operational threat-recognition capacity across generations. It
 *   demands costly participation from the present generation, framed as
 *   essential for the future survival of the group. The constraint is claimed
 *   as a Tangled Rope because it genuinely coordinates intergenerational
 *   knowledge transfer (a benefit) but does so through significant, often
 *   coercive, extraction from the present generation's autonomy. The high
 *   extractiveness reflects the ongoing demands of the ritual, while moderate
 *   suppression reflects social pressure and identity-lock mechanisms. The
 *   low theater ratio reflects this reading's assertion of genuine,
 *   non-performative function.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__survival_competence_reading, 0.75).
domain_priors:suppression_score(catastrophe_memory_preservation__survival_competence_reading, 0.6).
domain_priors:theater_ratio(catastrophe_memory_preservation__survival_competence_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__survival_competence_reading, "Ritual Preserves Operational Threat-Recognition Capacity (Survival Competence Reading)").
narrative_ontology:topic_domain(catastrophe_memory_preservation__survival_competence_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_preservation__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__survival_competence_reading, 'f1cc0a70-27f0-435c-9a72-79f92fc92614').
narrative_ontology:cs_kernel_codification('f1cc0a70-27f0-435c-9a72-79f92fc92614', implicit).
narrative_ontology:cs_authority_grounding('f1cc0a70-27f0-435c-9a72-79f92fc92614', practice).
narrative_ontology:cs_interpretation_layer_present('f1cc0a70-27f0-435c-9a72-79f92fc92614').
narrative_ontology:cs_reading_relation('f1cc0a70-27f0-435c-9a72-79f92fc92614', catastrophe_memory_preservation__mourning_practice_reading, forecloses).
narrative_ontology:cs_reading_relation('f1cc0a70-27f0-435c-9a72-79f92fc92614', catastrophe_memory_preservation__hybrid_atrophy_reading, forecloses).
narrative_ontology:cs_axiom('f1cc0a70-27f0-435c-9a72-79f92fc92614', foundational, ritual_transmits_operational_knowledge).
narrative_ontology:cs_axiom_status(ritual_transmits_operational_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('f1cc0a70-27f0-435c-9a72-79f92fc92614', ritual_transmits_operational_knowledge, empirically_contingent).
narrative_ontology:cs_axiom('f1cc0a70-27f0-435c-9a72-79f92fc92614', secondary, present_sacrifice_ensures_future_survival).
narrative_ontology:cs_axiom_status(present_sacrifice_ensures_future_survival, holdable).
narrative_ontology:cs_axiom_grounding('f1cc0a70-27f0-435c-9a72-79f92fc92614', present_sacrifice_ensures_future_survival, instrumental).
narrative_ontology:cs_reference_frame('f1cc0a70-27f0-435c-9a72-79f92fc92614', intergenerational_competence_transmission).
narrative_ontology:cs_drift_state('f1cc0a70-27f0-435c-9a72-79f92fc92614', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f1cc0a70-27f0-435c-9a72-79f92fc92614', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__survival_competence_reading, future_generations_survival).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__survival_competence_reading, present_generation_autonomy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__survival_competence_reading, present_generation_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The custodians and enforcers of the ritual, they interpret its meaning, ensure its correct performance, and transmit its demands to the present generation. Their authority is grounded in lineage and the perceived efficacy of the ritual for group survival.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, ritual_elders, agenda_setter,
    institutional, generational, constrained, regional).

% Bear the direct costs of the ritual: time, resources, emotional labor, and suppression of individual desires for the collective good. Their participation is often deeply tied to their identity within the community, making exit difficult despite the burdens.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, present_generation_participants, payer,
    moderate, biographical, identity_locked, local).

% The ultimate beneficiaries of the ritual, receiving the preserved knowledge and behavioral patterns necessary for their survival in the face of recurring catastrophic threats. They have no agency in the present, their 'benefit' is a projected outcome.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, future_generations_survival, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_preservation__survival_competence_reading, future_generations_survival).

% Academics or external analysts who study the ritual from a distance, evaluating its claims of efficacy against empirical evidence or alternative explanations. They do not participate in or directly benefit from the ritual.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, secular_observers, observer,
    analytical, biographical, analytical, global).

% Scholars who argue that such rituals, while perhaps once functional, have largely atrophied into symbolic practices, losing their operational content. Their perspective is excluded from the internal logic and justification of the ritual itself.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, atrophy_theorists, excluded,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective memory, emotional responses, and specific behavioral protocols across generations to ensure the group's survival in the face of recurring catastrophic threats.
% TRANSFER_FUNCTION: Transfers the burden of costly ritual participation and adherence to specific behavioral norms from the present generation, with the aim of transferring operational survival competence to future generations.
% ABSENT_VOICES: Atrophy theorists and secular observers would question the direct operational efficacy of the ritual, arguing that its demands are disproportionate to its actual functional output, or that modern solutions are more effective. They are excluded by the ritual's internal epistemic framework.
% DISAPPEARANCE_RATIONALE: If the ritual and its associated practices vanished, this reading posits that future generations would lack the critical, operationally-relevant knowledge and behavioral patterns to recognize and respond to catastrophic threats, leading to a higher likelihood of group extinction or severe disruption.
% FOUNDING_PROBLEM: A historical catastrophic event (e.g., famine, plague, invasion, environmental collapse) that severely threatened the group's existence and required specific, coordinated responses for survival.
% FOUNDING_PROBLEM_CORROBORATION: The community's oral histories, ancestral narratives, and the continued existence of the group despite recurring challenges are cited as corroboration. External historical records may confirm past catastrophes, but the direct link between ritual and operational survival is primarily attested by the ritual elders and community members, with skepticism from external analysts.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__survival_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__survival_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(catastrophe_memory_preservation__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__survival_competence_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.75) stems from the continuous, often arduous, demands placed on the present generation, which are seen as a necessary 'cost' for future survival. Suppression (0.60) is maintained through strong social norms, the authority of elders, and the deep integration of ritual participation into individual and collective identity. The low theater ratio (0.20) is consistent with this reading's core premise that the ritual's function is real and effective, not merely symbolic or inertial. Accessibility collapse and resistance are moderate, as alternatives (e.g., modern scientific approaches to disaster preparedness) are not fully collapsed but are often viewed as insufficient or culturally inappropriate by the community.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the ritual elders and participants (within this reading), the constraint is a vital, functional mechanism for collective survival, justifying its costs. From the perspective of external observers or those who question its operational efficacy, the same demands might appear as pure extraction or an atrophied practice. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'future_generations_survival' are the primary beneficiaries, as the ritual is explicitly designed to ensure their well-being and continuity. The 'present_generation_participants' are the primary payers, bearing the direct costs and sacrifices. The 'ritual_elders' act as agenda-setters, enforcing the constraint and benefiting from the authority and stability it provides. Secular observers and atrophy theorists are external to the ritual's internal logic, providing analytical perspectives.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_efficacy_ambiguity,
    'Does the ritual actually transmit operational threat-recognition capacity and survival competence, or is its function primarily symbolic and identity-forming?',
    'Longitudinal ethnographic studies comparing survival outcomes of groups maintaining such rituals versus those that have abandoned them, or empirical testing of knowledge transfer mechanisms embedded in the ritual.',
    'If the operational efficacy is low, the constraint''s extractiveness would be reclassified as higher (less justified by coordination), and its claimed type might shift towards Snare or Piton. If high, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_efficacy_ambiguity, empirical, 'Ambiguity regarding the ritual''s actual functional output versus its symbolic role.').

omega_variable(
    threat_recurrence_ambiguity,
    'Is the catastrophic threat that founded the ritual still a live and recurring danger, or has its nature changed, making the ritual''s specific operational responses obsolete?',
    'Geological, ecological, or historical analysis to determine the actual recurrence rate and nature of the founding catastrophe, compared to the ritual''s prescribed responses.',
    'If the threat is no longer live or has changed significantly, the ''founding_problem_status'' would shift to ''dead'', potentially reclassifying the constraint towards Piton due to functional atrophy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_recurrence_ambiguity, empirical, 'Whether the founding problem remains relevant to the ritual''s operational claims.').

omega_variable(
    internalized_suppression_ambiguity,
    'To what extent is participation in the ritual driven by genuine belief in its operational efficacy versus internalized social pressure, fear of ostracization, or identity-lock mechanisms?',
    'Sociological studies examining individual motivations for participation, and the psychological impact of non-participation or exit from the community.',
    'If internalized suppression is a dominant factor, the effective suppression for ''present_generation_participants'' is higher than structural measures suggest, reinforcing the extractive nature of the Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for ritual participation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__survival_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 60, 0.2).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 80, 0.2).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 20, 0.72).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 40, 0.73).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 60, 0.74).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 80, 0.75).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 100, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 20, 0.57).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 60, 0.59).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 80, 0.6).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 100, 0.6).


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
