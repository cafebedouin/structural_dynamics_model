% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__competence_reading, []).

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
 *   constraint_id: preparedness_commitment__competence_reading
 *   human_readable: Preparedness as Live Exercised Knowledge (Competence Reading)
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes preparedness as a system of live, exercised
 *   knowledge, where routines are actively maintained and adapted to ensure
 *   operational capacity across generational shifts. It emphasizes continuous
 *   learning, realistic drills, and effective knowledge transfer to prevent
 *   the degradation of institutional memory and adaptive capabilities. This
 *   is one reading of the broader 'preparedness_commitment' kernel, focusing
 *   on genuine competence.
 *
 * KEY AGENTS:
 *   - frontline_responders: Primary beneficiary (organized/constrained) — directly uses and maintains live knowledge.
 *   - affected_communities: Ultimate beneficiary (powerless/trapped) — relies on effective preparedness for safety.
 *   - institutional_leadership: Agenda setter (institutional/constrained) — responsible for policy and resource allocation.
 *   - training_and_exercise_staff: Agenda setter (moderate/mobile) — designs and implements competence-building programs.
 *   - new_recruits: Beneficiary (powerless/constrained) — receives transferred knowledge.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__competence_reading, 0.15).
domain_priors:suppression_score(preparedness_commitment__competence_reading, 0.2).
domain_priors:theater_ratio(preparedness_commitment__competence_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__competence_reading, rope).
narrative_ontology:human_readable(preparedness_commitment__competence_reading, "Preparedness as Live Exercised Knowledge (Competence Reading)").
narrative_ontology:topic_domain(preparedness_commitment__competence_reading, "disaster_preparedness/institutional_memory/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__competence_reading, '59de6dc5-b89b-44d7-818d-f918e7d209bd').
narrative_ontology:cs_kernel_codification('59de6dc5-b89b-44d7-818d-f918e7d209bd', formalized).
narrative_ontology:cs_authority_grounding('59de6dc5-b89b-44d7-818d-f918e7d209bd', expertise).
narrative_ontology:cs_interpretation_layer_present('59de6dc5-b89b-44d7-818d-f918e7d209bd').
narrative_ontology:cs_reading_relation('59de6dc5-b89b-44d7-818d-f918e7d209bd', preparedness_commitment__husk_reading, forecloses).
narrative_ontology:cs_reading_relation('59de6dc5-b89b-44d7-818d-f918e7d209bd', preparedness_commitment__hybrid_reading, influences).
narrative_ontology:cs_axiom('59de6dc5-b89b-44d7-818d-f918e7d209bd', foundational, operational_competence_is_paramount).
narrative_ontology:cs_axiom_status(operational_competence_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('59de6dc5-b89b-44d7-818d-f918e7d209bd', operational_competence_is_paramount, empirically_contingent).
narrative_ontology:cs_axiom('59de6dc5-b89b-44d7-818d-f918e7d209bd', foundational, generational_knowledge_transfer_is_essential).
narrative_ontology:cs_axiom_status(generational_knowledge_transfer_is_essential, holdable).
narrative_ontology:cs_axiom_grounding('59de6dc5-b89b-44d7-818d-f918e7d209bd', generational_knowledge_transfer_is_essential, empirically_contingent).
narrative_ontology:cs_reference_frame('59de6dc5-b89b-44d7-818d-f918e7d209bd', adaptive_operational_excellence).
narrative_ontology:cs_drift_state('59de6dc5-b89b-44d7-818d-f918e7d209bd', contemporary_practice, gap(stable, minor, true)).
narrative_ontology:cs_created_at('59de6dc5-b89b-44d7-818d-f918e7d209bd', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__competence_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, frontline_responders).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, affected_communities).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, institutional_leadership).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, new_recruits).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directly benefit from effective training and drills, which enhance their operational capacity and safety during actual emergencies. They are the primary carriers of live exercised knowledge.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, frontline_responders, beneficiary,
    organized, biographical, constrained, local).

% Are the ultimate beneficiaries of a competent and adaptive preparedness system, as it directly impacts their safety, resilience, and recovery from disasters. Their well-being depends on the system's effectiveness.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, affected_communities, beneficiary,
    powerless, generational, trapped, local).

% Sets policy, allocates resources for training and drills, and is responsible for ensuring the long-term operational capacity of preparedness systems. They benefit from the legitimacy and trust generated by effective preparedness.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, institutional_leadership, agenda_setter,
    institutional, generational, constrained, national).

% Designs, implements, and evaluates drills and training programs. Their role is critical in translating policy into live exercised knowledge and adapting routines to evolving threats and personnel.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, training_and_exercise_staff, agenda_setter,
    moderate, biographical, mobile, regional).

% Benefit from robust training and mentorship that integrates them into existing routines and transfers critical operational knowledge, preventing generational knowledge loss.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, new_recruits, beneficiary,
    powerless, immediate, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that diverse actors (responders, leadership, support staff) can effectively coordinate and execute complex emergency responses, maintaining operational capacity across personnel changes and evolving threats.
% TRANSFER_FUNCTION: Transfers operational knowledge, adaptive capacity, and institutional memory across generations of personnel, from experienced responders to new recruits, ensuring continuous competence.
% ABSENT_VOICES: Future generations of affected communities, who would demand robust, live-exercised preparedness, are structurally absent from current decision-making but are the ultimate stakeholders in the system's long-term effectiveness.
% DISAPPEARANCE_RATIONALE: If the commitment to preparedness as live exercised knowledge vanished, operational capacity would rapidly degrade, institutional memory would be lost with generational turnover, and communities would face catastrophic consequences during the next major disaster. The entire disaster response ecosystem would collapse.
% FOUNDING_PROBLEM: The recurring failure to effectively respond to predictable disasters due to lack of coordinated action, outdated knowledge, and insufficient training, leading to preventable loss of life and property.
% FOUNDING_PROBLEM_CORROBORATION: Independent disaster commissions, academic studies of emergency management, and historical analyses of disaster responses consistently corroborate the ongoing need for robust, live-exercised preparedness to mitigate the impact of natural and man-made catastrophes.
narrative_ontology:disappearance_verdict(preparedness_commitment__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(preparedness_commitment__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__competence_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__competence_reading_tests).
:- end_tests(preparedness_commitment__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15, trending down) because the system primarily generates collective benefits (safety, resilience) with minimal overhead beyond necessary coordination costs. Suppression is also low (0.2, trending down) as participation is largely driven by shared goals and professional commitment, rather than coercion. Theater ratio is very low (0.05, trending down) because drills are designed for genuine learning and testing, not mere performance. Accessibility collapse is moderate (0.7) as alternatives to coordinated preparedness are severely limited in a complex disaster scenario, but not entirely absent (e.g., individual self-reliance). Resistance is low (0.1) due to the clear benefits and shared understanding of the necessity of effective preparedness.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of frontline responders and affected communities, this constraint is a pure Rope, delivering essential coordination and safety. Institutional leadership also views it as a Rope, providing legitimacy and effective governance. There is minimal perspectival gap in this reading because all parties are aligned on the value of genuine competence.
 *
 * DIRECTIONALITY LOGIC:
 *   Frontline responders and affected communities are clear beneficiaries, receiving direct safety and resilience benefits. Institutional leadership and training staff are agenda-setters and beneficiaries, as they enable and benefit from the system's effectiveness. New recruits are beneficiaries of knowledge transfer. There are no identifiable victims in this reading, as the system is designed for collective good.
 *
 * MANDATROPHY ANALYSIS:
 *   This competence reading actively resists mandatrophy by prioritizing live, exercised knowledge over mere procedural compliance. The low theater ratio and continuous adaptation prevent the mandate from atrophying into a performative shell. The classification as a Rope reflects its ongoing, genuine coordination function, preventing mislabeling as a Piton or Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_vs_performance_drift,
    'To what extent do actual drills and training exercises test genuine decision-making and adaptive capacity, versus merely rehearsing pre-scripted outcomes for compliance?',
    'Independent, unannounced audits of exercise design and execution, coupled with post-exercise analysis of emergent problems and adaptive solutions, rather than just pass/fail metrics.',
    'If drills are primarily performative, the system''s true competence is lower than assessed, and the constraint drifts towards a ''husk_reading'' (Piton or Snare), indicating a higher theater ratio and potential for catastrophic failure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competence_vs_performance_drift, empirical, 'Distinguishing genuine competence-building from performative compliance in preparedness exercises.').

omega_variable(
    generational_knowledge_transfer_efficacy,
    'How effectively is critical operational knowledge and adaptive capacity transferred to new generations of personnel, preventing a ''D5 break'' (discontinuity of competence)?',
    'Longitudinal studies tracking the performance of new recruits in unscripted scenarios, and qualitative analysis of mentorship programs and institutional learning mechanisms.',
    'If knowledge transfer is ineffective, the system''s long-term resilience is compromised, increasing its vulnerability to mandatrophy and potentially reclassifying it towards a ''husk_reading'' as competence erodes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_knowledge_transfer_efficacy, empirical, 'Assessing the success of intergenerational knowledge transfer in maintaining operational capacity.').

omega_variable(
    reading_framing_choice,
    'Is this constraint best framed as a ''competence_reading'' (live exercised knowledge), or does it contain significant elements of ''husk_reading'' (memorial performance) or ''hybrid_reading'' (layered system)?',
    'A comprehensive, multi-stakeholder assessment of the system''s actual operational outcomes, resource allocation priorities, and the stated vs. enacted justifications for its routines.',
    'If significant ''husk'' elements are found, the constraint''s extractiveness and theater ratio would be higher, and its classification would shift towards a Piton or Snare. If ''hybrid'' elements are dominant, the complexity of the system''s internal dynamics would increase, requiring a different analytical approach.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_choice, conceptual, 'Ambiguity in framing preparedness as pure competence versus containing performative or hybrid elements.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__competence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__competence_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(prep_tr_t10, preparedness_commitment__competence_reading, theater_ratio, 10, 0.04).
narrative_ontology:measurement(prep_tr_t20, preparedness_commitment__competence_reading, theater_ratio, 20, 0.03).
narrative_ontology:measurement(prep_tr_t30, preparedness_commitment__competence_reading, theater_ratio, 30, 0.02).
narrative_ontology:measurement(prep_tr_t40, preparedness_commitment__competence_reading, theater_ratio, 40, 0.01).
narrative_ontology:measurement(prep_tr_t50, preparedness_commitment__competence_reading, theater_ratio, 50, 0.01).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__competence_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(prep_be_t10, preparedness_commitment__competence_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement(prep_be_t20, preparedness_commitment__competence_reading, base_extractiveness, 20, 0.13).
narrative_ontology:measurement(prep_be_t30, preparedness_commitment__competence_reading, base_extractiveness, 30, 0.12).
narrative_ontology:measurement(prep_be_t40, preparedness_commitment__competence_reading, base_extractiveness, 40, 0.11).
narrative_ontology:measurement(prep_be_t50, preparedness_commitment__competence_reading, base_extractiveness, 50, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_commitment__competence_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(prep_su_t10, preparedness_commitment__competence_reading, suppression_requirement, 10, 0.18).
narrative_ontology:measurement(prep_su_t20, preparedness_commitment__competence_reading, suppression_requirement, 20, 0.16).
narrative_ontology:measurement(prep_su_t30, preparedness_commitment__competence_reading, suppression_requirement, 30, 0.14).
narrative_ontology:measurement(prep_su_t40, preparedness_commitment__competence_reading, suppression_requirement, 40, 0.12).
narrative_ontology:measurement(prep_su_t50, preparedness_commitment__competence_reading, suppression_requirement, 50, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__competence_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
