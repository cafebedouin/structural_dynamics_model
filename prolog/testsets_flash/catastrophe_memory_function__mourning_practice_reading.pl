% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__mourning_practice_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_memory_function__mourning_practice_reading
 *   human_readable: Catastrophe Memory Function: Mourning Practice Reading (Tisha B'Av)
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   This constraint describes the 'mourning practice' reading of the
 *   catastrophe memory function, exemplified by rituals like Tisha B'Av. It
 *   focuses on how ritual primarily preserves collective identity and
 *   boundary norms through memorial obligation, with minimal emphasis on
 *   transmitting adaptive survival competence. The ritual itself is the
 *   mechanism for maintaining group identity and processing collective grief,
 *   rather than a means to an external end of institutional transformation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__mourning_practice_reading, 0.25).
domain_priors:suppression_score(catastrophe_memory_function__mourning_practice_reading, 0.4).
domain_priors:theater_ratio(catastrophe_memory_function__mourning_practice_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__mourning_practice_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__mourning_practice_reading, "Catastrophe Memory Function: Mourning Practice Reading (Tisha B'Av)").
narrative_ontology:topic_domain(catastrophe_memory_function__mourning_practice_reading, "religious_studies/ritual_theory/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__mourning_practice_reading, 'b6dd810b-a5cb-4f3d-b788-2a51d1dc09af').
narrative_ontology:cs_kernel_codification('b6dd810b-a5cb-4f3d-b788-2a51d1dc09af', formalized).
narrative_ontology:cs_authority_grounding('b6dd810b-a5cb-4f3d-b788-2a51d1dc09af', lineage).
narrative_ontology:cs_interpretation_layer_present('b6dd810b-a5cb-4f3d-b788-2a51d1dc09af').
narrative_ontology:cs_reading_relation('b6dd810b-a5cb-4f3d-b788-2a51d1dc09af', catastrophe_memory_function__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('b6dd810b-a5cb-4f3d-b788-2a51d1dc09af', catastrophe_memory_function__hybrid_transformation_reading, coexists_with).
narrative_ontology:cs_axiom('b6dd810b-a5cb-4f3d-b788-2a51d1dc09af', foundational, memory_as_identity_preservation).
narrative_ontology:cs_axiom_status(memory_as_identity_preservation, holdable).
narrative_ontology:cs_axiom_grounding('b6dd810b-a5cb-4f3d-b788-2a51d1dc09af', memory_as_identity_preservation, deontological).
narrative_ontology:cs_axiom('b6dd810b-a5cb-4f3d-b788-2a51d1dc09af', foundational, ritual_as_boundary_maintenance).
narrative_ontology:cs_axiom_status(ritual_as_boundary_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('b6dd810b-a5cb-4f3d-b788-2a51d1dc09af', ritual_as_boundary_maintenance, conventional).
narrative_ontology:cs_reference_frame('b6dd810b-a5cb-4f3d-b788-2a51d1dc09af', traditional_mourning_framework).
narrative_ontology:cs_drift_state('b6dd810b-a5cb-4f3d-b788-2a51d1dc09af', contemporary_secularization_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('b6dd810b-a5cb-4f3d-b788-2a51d1dc09af', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, religious_community).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, community_leaders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_function__mourning_practice_reading, individual_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participates in the ritual, reinforcing collective identity and shared memory of catastrophe. The ritual provides a structured outlet for grief and a sense of belonging, but also imposes obligations and emotional costs.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, religious_community, beneficiary,
    organized, generational, identity_locked, global).

% Administer and interpret the ritual, ensuring its continuity and adherence to tradition. They benefit from the reinforced authority and cohesion within the community, but bear the responsibility of preserving the practice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, community_leaders, agenda_setter,
    institutional, generational, constrained, global).

% Bear the personal and emotional costs of observing the ritual, including fasting, abstaining from certain activities, and engaging in somber reflection. Their participation is driven by religious identity and social belonging.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, individual_adherents, payer,
    moderate, biographical, identity_locked, local).

% Study the ritual from an academic perspective, analyzing its social, psychological, and historical functions without direct participation or adherence to its norms.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, secular_observers, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective mourning and remembrance of historical catastrophes, ensuring the transmission of shared identity and boundary norms across generations within the religious community.
% TRANSFER_FUNCTION: Transfers emotional labor, time, and adherence to specific behavioral norms from individual adherents to the collective memory and identity of the religious community.
% ABSENT_VOICES: Those who have left the religious community or who advocate for a purely secular or individualistic approach to memory would object to the prescriptive nature of the ritual and its emphasis on collective obligation.
% DISAPPEARANCE_RATIONALE: If the ritual disappeared, the religious community's collective identity and memory of catastrophe would fragment, leading to a loss of shared mourning practices and a weakening of internal boundary norms. The social fabric would need to reorganize around new forms of remembrance or risk dissolution.
% FOUNDING_PROBLEM: The problem of preserving the memory of catastrophic historical losses and maintaining group cohesion and identity in their aftermath, preventing assimilation or despair.
% FOUNDING_PROBLEM_CORROBORATION: Historians and sociologists of religion, from outside the immediate religious community, corroborate the ongoing challenge of collective memory transmission and identity maintenance in the face of historical trauma, affirming the problem's continued relevance.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__mourning_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__mourning_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__mourning_practice_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_function__mourning_practice_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__mourning_practice_reading_tests).
:- end_tests(catastrophe_memory_function__mourning_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) as the primary function is coordination of collective memory and identity, with costs mainly being emotional and temporal obligations rather than material extraction. Suppression is moderate (0.4) due to social pressure and identity-lock mechanisms that discourage non-participation, but direct coercion is absent. Theater ratio is low (0.1) as the ritual's performance is largely congruent with its stated purpose of remembrance and identity maintenance. Accessibility collapse is high (0.7) because for adherents, the ritual is a fundamental part of their identity, making alternatives to collective mourning difficult to conceive or adopt. Resistance is low (0.15) as the community largely accepts the necessity and value of the practice.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the religious community, the ritual is a vital Rope, essential for cultural and religious continuity. From an individual adherent's perspective, it might feel more like a Tangled Rope due to the emotional and social obligations, but the net benefit of belonging and shared meaning generally outweighs the costs. The engine's classification will reflect this balance.
 *
 * DIRECTIONALITY LOGIC:
 *   The religious community and its leaders are beneficiaries, gaining cohesion and authority. Individual adherents are payers, bearing the emotional and temporal costs. All are identity-locked to varying degrees, as participation is tied to group membership and self-concept. Secular observers are analytical, outside the constraint's direct influence.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mourning_vs_competence_ambiguity,
    'Is the ritual purely about mourning and identity, or does it implicitly transmit survival competence for institutional transformation?',
    'Longitudinal ethnographic studies tracking community adaptive responses to new crises, correlating with ritual participation patterns. If communities with strong mourning rituals also show enhanced adaptive capacity, the ''survival_competence_reading'' gains empirical support.',
    'If survival competence is also transmitted, the constraint''s coordination function is broader, potentially shifting its classification towards a ''hybrid_transformation_reading'' or even ''survival_competence_reading'' if that function becomes dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mourning_vs_competence_ambiguity, empirical, 'Distinguishing the primary function of catastrophe memory rituals.').

omega_variable(
    identity_lock_strength,
    'To what extent is participation in the ritual genuinely identity-locked versus socially coerced?',
    'Surveys and qualitative interviews with former adherents who have exited the community, exploring the perceived costs and benefits of non-participation and the mechanisms of social pressure versus internal identity fusion.',
    'If social coercion is a stronger factor than identity fusion, the ''suppression'' metric might be understated, and the ''exit_options'' for individual adherents might be closer to ''constrained'' than ''identity_locked'', potentially increasing effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_strength, empirical, 'Assessing the nature of identity-lock in ritual participation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__mourning_practice_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 25, 0.1).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(cata_tr_t75, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 75, 0.1).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 25, 0.22).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 50, 0.23).
narrative_ontology:measurement(cata_be_t75, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 75, 0.24).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 100, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 25, 0.37).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 50, 0.38).
narrative_ontology:measurement(cata_su_t75, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 75, 0.39).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 100, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__mourning_practice_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'catastrophe_memory_function' kernel, focusing on mourning practice and boundary norms. It is distinct from the 'survival_competence_reading' and 'hybrid_transformation_reading' which emphasize adaptive mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
