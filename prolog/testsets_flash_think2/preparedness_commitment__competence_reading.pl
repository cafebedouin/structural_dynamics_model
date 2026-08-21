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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: preparedness_commitment__competence_reading
 *   human_readable: Preparedness as Live Exercised Knowledge (Competence Reading)
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes preparedness as a dynamic, actively maintained
 *   system of knowledge and routines that ensures genuine operational
 *   capacity across generations of personnel. It emphasizes continuous
 *   learning, realistic drills, and effective knowledge transfer to prevent
 *   the degradation of institutional memory. This is the 'competence reading'
 *   of the broader 'preparedness_commitment' kernel, focusing on the
 *   functional efficacy of the system.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__competence_reading, 0.18).
domain_priors:suppression_score(preparedness_commitment__competence_reading, 0.15).
domain_priors:theater_ratio(preparedness_commitment__competence_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__competence_reading, rope).
narrative_ontology:human_readable(preparedness_commitment__competence_reading, "Preparedness as Live Exercised Knowledge (Competence Reading)").
narrative_ontology:topic_domain(preparedness_commitment__competence_reading, "disaster_preparedness/institutional_memory/commitment_systems").

domain_priors:requires_active_enforcement(preparedness_commitment__competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__competence_reading, '9439568b-e677-482f-94b1-2bd69b9ce4dd').
narrative_ontology:cs_kernel_codification('9439568b-e677-482f-94b1-2bd69b9ce4dd', formalized).
narrative_ontology:cs_authority_grounding('9439568b-e677-482f-94b1-2bd69b9ce4dd', expertise).
narrative_ontology:cs_interpretation_layer_present('9439568b-e677-482f-94b1-2bd69b9ce4dd').
narrative_ontology:cs_reading_relation('9439568b-e677-482f-94b1-2bd69b9ce4dd', preparedness_commitment__husk_reading, forecloses).
narrative_ontology:cs_reading_relation('9439568b-e677-482f-94b1-2bd69b9ce4dd', preparedness_commitment__hybrid_reading, influences).
narrative_ontology:cs_axiom('9439568b-e677-482f-94b1-2bd69b9ce4dd', foundational, operational_fidelity_is_paramount).
narrative_ontology:cs_axiom_status(operational_fidelity_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('9439568b-e677-482f-94b1-2bd69b9ce4dd', operational_fidelity_is_paramount, empirically_contingent).
narrative_ontology:cs_axiom('9439568b-e677-482f-94b1-2bd69b9ce4dd', foundational, knowledge_decays_without_exercise).
narrative_ontology:cs_axiom_status(knowledge_decays_without_exercise, holdable).
narrative_ontology:cs_axiom_grounding('9439568b-e677-482f-94b1-2bd69b9ce4dd', knowledge_decays_without_exercise, empirically_contingent).
narrative_ontology:cs_reference_frame('9439568b-e677-482f-94b1-2bd69b9ce4dd', adaptive_competence_paradigm).
narrative_ontology:cs_drift_state('9439568b-e677-482f-94b1-2bd69b9ce4dd', contemporary_budget_cuts_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9439568b-e677-482f-94b1-2bd69b9ce4dd', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(preparedness_commitment__competence_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, preparedness_agencies).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, first_responders).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, general_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_commitment__competence_reading, first_responders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for designing, implementing, and overseeing preparedness routines and training. They benefit from a functional system that validates their mandate and protects the public. Their role is to ensure the knowledge remains live and exercised.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, preparedness_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Participate in drills and training, internalizing the exercised knowledge. They are direct beneficiaries of effective coordination in emergencies, which enhances their safety and efficacy. They bear the costs of training time and effort.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, first_responders, beneficiary,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__competence_reading, first_responders, payer).

% The ultimate beneficiaries of effective disaster preparedness, experiencing reduced harm and faster recovery. They bear diffuse costs through taxes but have no direct control over the routines.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, general_public, beneficiary,
    powerless, immediate, constrained, local).

% Allocate resources and set legislative mandates for preparedness. They observe the efficacy of the routines and are influenced by public perception and expert advice regarding operational capacity.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, policy_makers, observer,
    institutional, generational, analytical, national).

% Develop and deliver the training programs that ensure knowledge transfer and exercise. They are critical to maintaining the 'live' aspect of the knowledge and benefit from the professional recognition of a highly competent system.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, training_personnel, agenda_setter,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_commitment__competence_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_commitment__competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures a shared, up-to-date, and practiced operational capacity across diverse agencies and personnel generations, enabling effective, coordinated response to inevitable disasters.
% TRANSFER_FUNCTION: Transfers critical operational knowledge, skills, and adaptive decision-making capabilities from experienced personnel and past events to new recruits and evolving threats, maintaining institutional readiness.
% ABSENT_VOICES: Those who dismiss preparedness as an unnecessary cost or 'cry wolf' scenarios, often driven by short-term fiscal priorities. They are typically excluded from the operational planning and training processes, as their input would undermine the commitment to readiness.
% DISAPPEARANCE_RATIONALE: If preparedness as live exercised knowledge vanished, institutional memory would rapidly degrade into inert protocols. Subsequent disasters would be met with chaotic, uncoordinated, and ineffective responses, leading to catastrophic loss of life, property, and societal trust, fundamentally reorganizing the social contract around safety.
% FOUNDING_PROBLEM: The historical experience of catastrophic, uncoordinated disaster responses, where lack of practiced knowledge and inter-agency coordination led to preventable loss and prolonged suffering.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of past disasters (e.g., Hurricane Katrina, 9/11), scientific projections of future risks (e.g., climate change impacts, seismic activity), and independent analyses from disaster relief organizations (e.g., FEMA, Red Cross) and academic researchers consistently corroborate the ongoing need for robust preparedness.
narrative_ontology:disappearance_verdict(preparedness_commitment__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(preparedness_commitment__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__competence_reading, 0.18, 'gemini-2.5-flash', 'none', direct).

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
 *   The metrics reflect a highly functional coordination mechanism. Extractiveness is low (0.18) as costs are primarily for genuine training and maintenance, not rent-seeking. Suppression is low (0.15) because participation is largely driven by the perceived value and necessity of effective response, rather than coercion. Theater ratio is low (0.10) because drills are designed for realistic testing and learning, not merely for show. The system is actively enforced to maintain standards, but this enforcement is seen as necessary for collective benefit. The temporal measurements show relative stability, indicating a well-maintained system over time.
 *
 * PERSPECTIVAL GAP:
 *   This 'competence reading' would compute as a Rope for all participants, reflecting genuine coordination and shared benefit. Sibling readings, such as the 'husk_reading' (memorial performance without competence), would compute very differently, likely as a Piton or Snare, due to higher theater and potential extraction from performative compliance.
 *
 * DIRECTIONALITY LOGIC:
 *   Preparedness agencies and training personnel are agenda-setters and beneficiaries, as they manage and benefit from a functional system. First responders are beneficiaries (safer, more effective) but also bear costs (training time). The general public is the ultimate beneficiary of safety. There are no identifiable 'victims' in this reading, as costs are diffuse and proportionate to the collective benefit of disaster resilience.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_fidelity_measurement,
    'How accurately do current drills and exercises reflect real-world disaster scenarios and decision-making pressures?',
    'Independent, third-party evaluation of drill realism, post-incident analysis comparing drill performance to actual response, and expert assessment of training methodologies.',
    'If fidelity is low, the ''live exercised knowledge'' claim is weakened, pushing the constraint towards a ''husk_reading'' (higher theater, lower genuine coordination), potentially reclassifying it as a Piton or Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(operational_fidelity_measurement, empirical, 'Assessing the realism and effectiveness of preparedness exercises.').

omega_variable(
    generational_knowledge_transfer_efficacy,
    'Is the system effectively transferring critical operational knowledge and adaptive capacity across generational turnover in personnel?',
    'Longitudinal studies tracking knowledge retention and performance of new recruits, exit interviews with retiring personnel, and analysis of ''near-miss'' incidents for evidence of knowledge gaps.',
    'Failure to transfer knowledge effectively would indicate a drift towards a ''husk_reading'' or a ''piton'' where the system performs rituals without retaining core function, increasing the risk of D5 breaks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_knowledge_transfer_efficacy, empirical, 'Evaluating the success of inter-generational knowledge transfer in preparedness.').

omega_variable(
    competence_vs_husk_framing_ambiguity,
    'Is this constraint a genuine ''competence_reading'' or is it, in practice, closer to a ''husk_reading'' where routines are performed without true operational capacity?',
    'Empirical resolution of ''operational_fidelity_measurement'' and ''generational_knowledge_transfer_efficacy'' omegas. If both resolve negatively, the ''husk_reading'' is more accurate.',
    'If resolved as a ''husk_reading'', the constraint would reclassify from Rope to Piton (due to high theater and atrophied function) or even Snare (if resources are extracted for performative maintenance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_vs_husk_framing_ambiguity, conceptual, 'Distinguishing genuine competence from performative compliance in preparedness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__competence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(prep_tr_t10, preparedness_commitment__competence_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(prep_tr_t20, preparedness_commitment__competence_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(prep_tr_t30, preparedness_commitment__competence_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement(prep_tr_t40, preparedness_commitment__competence_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(prep_tr_t50, preparedness_commitment__competence_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__competence_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(prep_be_t10, preparedness_commitment__competence_reading, base_extractiveness, 10, 0.19).
narrative_ontology:measurement(prep_be_t20, preparedness_commitment__competence_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement(prep_be_t30, preparedness_commitment__competence_reading, base_extractiveness, 30, 0.19).
narrative_ontology:measurement(prep_be_t40, preparedness_commitment__competence_reading, base_extractiveness, 40, 0.18).
narrative_ontology:measurement(prep_be_t50, preparedness_commitment__competence_reading, base_extractiveness, 50, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_commitment__competence_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(prep_su_t10, preparedness_commitment__competence_reading, suppression_requirement, 10, 0.16).
narrative_ontology:measurement(prep_su_t20, preparedness_commitment__competence_reading, suppression_requirement, 20, 0.15).
narrative_ontology:measurement(prep_su_t30, preparedness_commitment__competence_reading, suppression_requirement, 30, 0.16).
narrative_ontology:measurement(prep_su_t40, preparedness_commitment__competence_reading, suppression_requirement, 40, 0.15).
narrative_ontology:measurement(prep_su_t50, preparedness_commitment__competence_reading, suppression_requirement, 50, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__competence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, disaster_response_protocols).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, public_safety_funding).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, preparedness_commitment__husk_reading).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, preparedness_commitment__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'competence_reading' of the 'preparedness_commitment' kernel. It is structurally distinct from the 'husk_reading' (memorial performance) and 'hybrid_reading' (layered system) due to differing ε values and functional claims, but all are part of the same constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
