% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__hybrid_reading, []).

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
 *   constraint_id: preparedness_commitment__hybrid_reading
 *   human_readable: Preparedness as Layered Commitment-Competence System
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes preparedness as a layered system, where
 *   memorial elements (e.g., drills, commemorative events) stabilize
 *   long-term societal commitment to readiness, while competence elements
 *   (e.g., training, equipment maintenance) ensure functional operational
 *   capacity. The 'hybrid_reading' acknowledges the necessity and tension
 *   between these two layers. The constraint is claimed as a Tangled Rope
 *   because it genuinely coordinates long-term commitment but also involves
 *   asymmetric extraction due to the costs of maintaining both layers and the
 *   potential for memorial elements to overshadow competence, leading to
 *   hidden costs borne by frontline responders and taxpayers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__hybrid_reading, 0.45).
domain_priors:suppression_score(preparedness_commitment__hybrid_reading, 0.3).
domain_priors:theater_ratio(preparedness_commitment__hybrid_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_commitment__hybrid_reading, "Preparedness as Layered Commitment-Competence System").
narrative_ontology:topic_domain(preparedness_commitment__hybrid_reading, "disaster_preparedness/institutional_memory/commitment_systems").

domain_priors:requires_active_enforcement(preparedness_commitment__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__hybrid_reading, 'dee62fa3-9112-4bd0-aff6-3820feaa7e68').
narrative_ontology:cs_kernel_codification('dee62fa3-9112-4bd0-aff6-3820feaa7e68', formalized).
narrative_ontology:cs_authority_grounding('dee62fa3-9112-4bd0-aff6-3820feaa7e68', lineage).
narrative_ontology:cs_interpretation_layer_present('dee62fa3-9112-4bd0-aff6-3820feaa7e68').
narrative_ontology:cs_reading_relation('dee62fa3-9112-4bd0-aff6-3820feaa7e68', preparedness_commitment__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('dee62fa3-9112-4bd0-aff6-3820feaa7e68', preparedness_commitment__competence_reading, coexists_with).
narrative_ontology:cs_axiom('dee62fa3-9112-4bd0-aff6-3820feaa7e68', foundational, commitment_requires_memorialization).
narrative_ontology:cs_axiom_status(commitment_requires_memorialization, holdable).
narrative_ontology:cs_axiom_grounding('dee62fa3-9112-4bd0-aff6-3820feaa7e68', commitment_requires_memorialization, conventional).
narrative_ontology:cs_axiom('dee62fa3-9112-4bd0-aff6-3820feaa7e68', foundational, competence_requires_active_maintenance).
narrative_ontology:cs_axiom_status(competence_requires_active_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('dee62fa3-9112-4bd0-aff6-3820feaa7e68', competence_requires_active_maintenance, empirically_contingent).
narrative_ontology:cs_reference_frame('dee62fa3-9112-4bd0-aff6-3820feaa7e68', balanced_layered_preparedness).
narrative_ontology:cs_drift_state('dee62fa3-9112-4bd0-aff6-3820feaa7e68', contemporary_budget_cycles, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dee62fa3-9112-4bd0-aff6-3820feaa7e68', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__hybrid_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, public_safety_agencies).
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, political_leaders).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, taxpayers).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, frontline_responders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for maintaining preparedness, they balance memorial elements (drills, ceremonies) with competence elements (training, equipment). They benefit from the stability of commitment but bear the cost of maintaining both layers, often under budget pressure.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, public_safety_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the public perception of preparedness, especially the memorial elements that signal commitment. They often prioritize visible, symbolic acts over deep, costly competence building, shifting the burden of actual function to agencies and responders.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, political_leaders, beneficiary,
    powerful, immediate, mobile, national).

% Bear the financial cost of maintaining both layers of preparedness. They are often unaware of the tension between memorial and competence elements, assuming all spending contributes to actual readiness.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, taxpayers, payer,
    organized, biographical, constrained, national).

% Experience the direct consequences of any imbalance, often having to compensate for competence gaps with personal sacrifice. Their professional identity binds them to the system, making exit difficult despite the costs.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, frontline_responders, payer,
    moderate, biographical, identity_locked, local).

% Are the ultimate recipients of preparedness efforts, but their voices are often absent from the design and funding decisions. They bear the highest costs when the system fails due to competence atrophy.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, disaster_victims, excluded,
    powerless, immediate, trapped, local).

% Analyze the dynamics between memorial and competence elements in preparedness systems, identifying points of failure and resilience. They provide external critique and propose structural improvements.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, institutional_memory_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates long-term societal commitment to disaster readiness (memorial layer) with the practical, operational capacity to respond effectively (competence layer), ensuring a baseline of safety and resilience across generations.
% TRANSFER_FUNCTION: Transfers resources (funding, personnel, attention) from taxpayers to public safety agencies, with a portion of these resources being diverted to symbolic memorial activities that stabilize political commitment, rather than direct competence building. The cost of competence gaps is transferred to frontline responders and disaster victims.
% ABSENT_VOICES: Disaster victims and future generations, who bear the ultimate costs of preparedness failures, are largely absent from the decision-making processes that shape the balance between memorial and competence elements. Their interests are often represented by proxies or not at all.
% DISAPPEARANCE_RATIONALE: If this layered system of preparedness vanished, the long-term commitment to readiness would erode, and operational competence would rapidly decay without the memorial layer to stabilize it. Society would become significantly more vulnerable to disasters, leading to catastrophic human and economic costs.
% FOUNDING_PROBLEM: Societies face recurrent threats from natural and man-made disasters, requiring sustained, intergenerational commitment and adaptive operational capacity to mitigate harm and ensure recovery.
% FOUNDING_PROBLEM_CORROBORATION: Public safety agencies and institutional memory scholars corroborate that the problem of sustained, adaptive preparedness remains live, citing ongoing disaster events and the challenges of maintaining readiness over time. Political leaders also attest to the problem, often emphasizing the need for visible commitment.
narrative_ontology:disappearance_verdict(preparedness_commitment__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(preparedness_commitment__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__hybrid_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__hybrid_reading_tests).
:- end_tests(preparedness_commitment__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate because while the system provides genuine coordination, the dual-layer maintenance creates overhead, and the memorial layer can sometimes divert resources or attention from pure competence building, leading to a form of 'extraction' where resources are used for symbolic rather than functional ends. Suppression (0.30) is low because resistance to preparedness is generally low, but there's a subtle suppression of critical voices that might challenge the balance between memorial and competence elements. Theater ratio (0.20) is present but not dominant, reflecting that while memorial elements have a performative aspect, they also serve a genuine commitment-stabilizing function. The slight increase in extractiveness over time reflects the tendency for the system to accumulate overhead and for the tension between layers to create inefficiencies.
 *
 * PERSPECTIVAL GAP:
 *   Public safety agencies and political leaders might perceive this as a necessary and efficient system for long-term preparedness, emphasizing the commitment-stabilizing role of memorial elements. Taxpayers and frontline responders, however, might experience the inefficiencies and competence gaps as a form of extraction, where their resources or efforts are used to maintain a system that is not optimally functional. The engine's per-seat classification would reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Public safety agencies are agenda-setters, balancing the layers. Political leaders are beneficiaries, gaining from visible commitment. Taxpayers and frontline responders are payers, bearing the costs of maintenance and competence gaps. Disaster victims are excluded, bearing the ultimate cost of failure. This hybrid reading acknowledges that all parties benefit from the overall coordination, but the specific layering creates differential costs and benefits, leading to a Tangled Rope classification.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    memorial_competence_balance,
    'What is the optimal balance between memorial and competence elements for effective, sustainable preparedness, and how does the current system deviate?',
    'Longitudinal studies comparing preparedness outcomes in systems with different memorial-to-competence ratios, combined with expert elicitation and cost-benefit analysis.',
    'Resolving this would clarify whether the current ''hybrid'' system is efficient or if the memorial layer is disproportionately extractive, leading to reclassification towards a Snare if the balance is severely off.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(memorial_competence_balance, empirical, 'Determining the functional efficiency of the memorial vs. competence balance.').

omega_variable(
    resource_diversion_measurement,
    'To what extent are resources allocated to preparedness diverted from competence-building to purely symbolic memorial activities?',
    'Detailed financial audits and program evaluations that disaggregate spending on memorial events, drills, and symbolic infrastructure from direct training, equipment, and operational readiness.',
    'Quantifying resource diversion would refine the extractiveness metric. High diversion would push the classification closer to a Snare, indicating that the coordination story is largely cover for resource misallocation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_diversion_measurement, empirical, 'Measuring the actual resource allocation between memorial and competence functions.').

omega_variable(
    hybrid_vs_husk_distinction,
    'Is this ''hybrid_reading'' genuinely distinct from the ''husk_reading'', or does the memorial layer so dominate that competence is effectively atrophied?',
    'Empirical assessment of operational readiness during actual disaster events: if the system consistently fails to perform despite memorial activities, it leans towards the ''husk_reading''.',
    'If the ''husk_reading'' is found to be more accurate, the constraint would reclassify to a Piton or Snare, with significantly higher theater_ratio and extractiveness, as the coordination function would be largely performative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hybrid_vs_husk_distinction, conceptual, 'Distinguishing genuine hybrid functionality from performative husk.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__hybrid_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__hybrid_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(prep_tr_t10, preparedness_commitment__hybrid_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(prep_tr_t20, preparedness_commitment__hybrid_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(prep_tr_t30, preparedness_commitment__hybrid_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(prep_tr_t40, preparedness_commitment__hybrid_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(prep_tr_t50, preparedness_commitment__hybrid_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__hybrid_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(prep_be_t10, preparedness_commitment__hybrid_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(prep_be_t20, preparedness_commitment__hybrid_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(prep_be_t30, preparedness_commitment__hybrid_reading, base_extractiveness, 30, 0.43).
narrative_ontology:measurement(prep_be_t40, preparedness_commitment__hybrid_reading, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(prep_be_t50, preparedness_commitment__hybrid_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_commitment__hybrid_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(prep_su_t10, preparedness_commitment__hybrid_reading, suppression_requirement, 10, 0.27).
narrative_ontology:measurement(prep_su_t20, preparedness_commitment__hybrid_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement(prep_su_t30, preparedness_commitment__hybrid_reading, suppression_requirement, 30, 0.29).
narrative_ontology:measurement(prep_su_t40, preparedness_commitment__hybrid_reading, suppression_requirement, 40, 0.3).
narrative_ontology:measurement(prep_su_t50, preparedness_commitment__hybrid_reading, suppression_requirement, 50, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, preparedness_commitment__husk_reading).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, preparedness_commitment__competence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'preparedness_commitment' kernel. The 'hybrid_reading' acknowledges the tension and interaction between memorial and competence elements, influencing and coexisting with the 'husk_reading' and 'competence_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
