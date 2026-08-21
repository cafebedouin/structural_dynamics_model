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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: catastrophe_memory_function__mourning_practice_reading
 *   human_readable: Commemorative Ritual for Group Identity and Mourning
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   This constraint describes a ritual practice (e.g., Tisha B'Av) whose
 *   primary function is to preserve the memory of a catastrophic loss and
 *   maintain the collective identity and boundary norms of a group. It is a
 *   'mourning_practice_reading' of the broader 'catastrophe_memory_function'
 *   kernel, emphasizing the ritual's role in D1 (collective mourning) and D4
 *   (identity coordination) without explicitly incorporating D5 (survival
 *   competence transmission). The ritual is understood as a core mechanism
 *   for group cohesion and historical continuity.
 *
 * KEY AGENTS:
 *   - group_members: Beneficiary (moderate/constrained) — participate, gain identity, bear costs
 *   - religious_leaders: Agenda_setter (institutional/constrained) — administer, interpret, enforce norms
 *   - future_generations: Beneficiary (powerless/trapped) — inherit identity and memory
 *   - secular_observers: Observer (analytical/analytical) — study the ritual
 *   - assimilated_members: Excluded (moderate/mobile) — question relevance, but outside conversation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__mourning_practice_reading, 0.25).
domain_priors:suppression_score(catastrophe_memory_function__mourning_practice_reading, 0.4).
domain_priors:theater_ratio(catastrophe_memory_function__mourning_practice_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__mourning_practice_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__mourning_practice_reading, "Commemorative Ritual for Group Identity and Mourning").
narrative_ontology:topic_domain(catastrophe_memory_function__mourning_practice_reading, "religious_studies/ritual_theory/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_function__mourning_practice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__mourning_practice_reading, '17b11d4d-ba3c-468c-a31c-cbeab5a19a08').
narrative_ontology:cs_kernel_codification('17b11d4d-ba3c-468c-a31c-cbeab5a19a08', implicit).
narrative_ontology:cs_authority_grounding('17b11d4d-ba3c-468c-a31c-cbeab5a19a08', practice).
narrative_ontology:cs_interpretation_layer_present('17b11d4d-ba3c-468c-a31c-cbeab5a19a08').
narrative_ontology:cs_reading_relation('17b11d4d-ba3c-468c-a31c-cbeab5a19a08', catastrophe_memory_function__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('17b11d4d-ba3c-468c-a31c-cbeab5a19a08', catastrophe_memory_function__hybrid_transformation_reading, coexists_with).
narrative_ontology:cs_axiom('17b11d4d-ba3c-468c-a31c-cbeab5a19a08', foundational, collective_mourning_is_identity_constitutive).
narrative_ontology:cs_axiom_status(collective_mourning_is_identity_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('17b11d4d-ba3c-468c-a31c-cbeab5a19a08', collective_mourning_is_identity_constitutive, deontological).
narrative_ontology:cs_axiom('17b11d4d-ba3c-468c-a31c-cbeab5a19a08', foundational, memory_of_catastrophe_must_be_preserved_through_ritual).
narrative_ontology:cs_axiom_status(memory_of_catastrophe_must_be_preserved_through_ritual, holdable).
narrative_ontology:cs_axiom_grounding('17b11d4d-ba3c-468c-a31c-cbeab5a19a08', memory_of_catastrophe_must_be_preserved_through_ritual, conventional).
narrative_ontology:cs_reference_frame('17b11d4d-ba3c-468c-a31c-cbeab5a19a08', unbroken_commemorative_lineage).
narrative_ontology:cs_drift_state('17b11d4d-ba3c-468c-a31c-cbeab5a19a08', contemporary_secularization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('17b11d4d-ba3c-468c-a31c-cbeab5a19a08', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, group_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, religious_leaders).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, future_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in the ritual, gaining a reinforced sense of collective identity, shared memory, and emotional solidarity. They bear the costs of time, emotional labor, and adherence to ritual norms, but are net beneficiaries of the group cohesion.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, group_members, beneficiary,
    moderate, biographical, constrained, global).

% Administer, interpret, and enforce the norms of the ritual. They benefit from the maintenance of group identity and their role in its preservation, but are constrained by the tradition itself and the expectations of the community.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, religious_leaders, agenda_setter,
    institutional, generational, constrained, global).

% Inherit the collective memory, identity, and cultural norms preserved by the ritual. They are beneficiaries of this transmission, but have no agency in shaping the ritual's form or existence.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Study the ritual's function in collective memory and identity formation from an academic perspective. They are outside the direct operation of the constraint, but their analysis can influence external perceptions.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, secular_observers, observer,
    analytical, biographical, analytical, global).

% Individuals who have distanced themselves from the group's core identity and ritual practices. They might question the relevance or burden of the ritual but are no longer part of the internal conversation about its maintenance.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, assimilated_members, excluded,
    moderate, biographical, mobile, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_function__mourning_practice_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_function__mourning_practice_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective mourning, transmits shared memory of a catastrophic loss, and reinforces group identity and boundary norms across generations, preventing cultural fragmentation.
% TRANSFER_FUNCTION: Transfers shared memory, emotional solidarity, and cultural norms across generations; demands time, emotional labor, and adherence to specific practices from participants.
% ABSENT_VOICES: Assimilated or secularized former members who might question the ongoing relevance or burden of the ritual, or those who advocate for alternative forms of memory transmission that do not involve ritual obligation.
% DISAPPEARANCE_RATIONALE: If the ritual and its associated obligations vanished overnight, the group's collective identity and memory of the catastrophe would fragment, leading to cultural dissolution or significant redefinition within a few generations. The social fabric and shared purpose would erode.
% FOUNDING_PROBLEM: The need to preserve the memory of a catastrophic loss and maintain group cohesion and identity in its aftermath, preventing the community from dissolving or losing its distinctiveness.
% FOUNDING_PROBLEM_CORROBORATION: Historians and sociologists of religion, as well as the lived experience of the community and its ongoing challenges, corroborate the persistent need for collective memory and identity maintenance in the face of external pressures and internal change. Scholarly works on collective trauma and ritual theory also support this reading.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__mourning_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__mourning_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__mourning_practice_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(catastrophe_memory_function__mourning_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__mourning_practice_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is low (0.25) because the ritual's primary function is coordination and identity maintenance, with costs (time, emotional labor) generally accepted as necessary for group belonging. Suppression is moderate (0.4) reflecting social pressure and communal expectations to participate, rather than coercive enforcement. Theater ratio is low (0.2) as the ritual's performative elements are genuinely functional in reinforcing memory and identity. Accessibility collapse is high (0.7) because for deeply embedded groups, there are few effective alternatives to ritual for maintaining this specific form of collective memory and identity. Resistance is low (0.1) from within the actively participating group, as the ritual is seen as foundational.
 *
 * PERSPECTIVAL GAP:
 *   Insiders (group_members, religious_leaders) perceive the ritual as a vital, almost natural, component of their identity and continuity, with its costs being inherent to belonging. Outsiders (secular_observers, assimilated_members) might view it as an archaic burden or a mechanism of social control, questioning its necessity or efficiency.
 *
 * DIRECTIONALITY LOGIC:
 *   Group members and future generations are clear beneficiaries, gaining identity and memory. Religious leaders are agenda-setters who also benefit from the stability and continuity of the group they lead. There are no explicit 'victims' as the costs are diffuse and generally accepted as part of participation. Assimilated members are 'excluded' as they are no longer subject to the constraint's direct influence.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preserving memory and identity) is still live, preventing mislabeling as a piton. Its coordination function is genuine, preventing mislabeling as a snare. The low extractiveness and accepted costs distinguish it from a tangled rope, despite active social enforcement. The 'live' status of the founding problem (per the six questions) directly supports this assessment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mourning_vs_survival_competence_ambiguity,
    'Is this ritual purely a mourning practice and identity-maintenance mechanism (D1/D4), or does it also implicitly transmit adaptive survival competence (D5)?',
    'Longitudinal ethnographic study of the community''s response to subsequent crises: if the ritual''s practices directly inform adaptive strategies, then D5 is also present.',
    'If D5 is also present, the constraint''s coordination function is broader, potentially shifting its Boltzmann classification or influencing its network effects on other adaptive constraints. If not, its function remains focused on D1/D4.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mourning_vs_survival_competence_ambiguity, empirical, 'Distinguishing pure mourning/identity from adaptive competence transmission.').

omega_variable(
    social_vs_internalized_enforcement,
    'To what extent is the persistence of this ritual driven by active social enforcement (communal pressure) versus internalized identity and belief?',
    'Sociological surveys measuring individual adherence in the absence of direct communal oversight, or studies of generational transmission patterns in diaspora communities.',
    'If primarily internalized, the effective suppression is higher and more resilient to external pressures, as individuals carry the constraint within their self-concept. If primarily social, the constraint is more vulnerable to shifts in communal structure or external pressures that weaken social ties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_vs_internalized_enforcement, empirical, 'Structural vs. internalized suppression mechanism for ritual adherence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__mourning_practice_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 10, 0.23).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 20, 0.24).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 30, 0.25).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 40, 0.25).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 50, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 10, 0.39).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 50, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__mourning_practice_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function__hybrid_transformation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'catastrophe_memory_function' kernel, focusing on mourning practice and identity. It coexists with and influences other readings that emphasize survival competence or a hybrid of functions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
