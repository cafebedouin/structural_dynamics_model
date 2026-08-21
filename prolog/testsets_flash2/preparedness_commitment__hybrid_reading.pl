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
 *   human_readable: Preparedness as Layered Commitment System (Hybrid Reading)
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   This constraint models preparedness as a layered commitment system,
 *   combining memorial elements (symbolic actions, rituals, historical
 *   narratives) that stabilize long-term societal commitment with competence
 *   elements (training, equipment, operational routines) that maintain
 *   functional capacity. This 'hybrid reading' acknowledges the necessity of
 *   both, but also the inherent tension and maintenance cost of balancing
 *   them. The claimed type is 'tangled_rope' because it genuinely coordinates
 *   (long-term readiness) but also involves asymmetric extraction (taxpayers
 *   and frontline responders bear costs, while political leaders benefit from
 *   symbolic gestures).
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
narrative_ontology:human_readable(preparedness_commitment__hybrid_reading, "Preparedness as Layered Commitment System (Hybrid Reading)").
narrative_ontology:topic_domain(preparedness_commitment__hybrid_reading, "disaster_preparedness/institutional_memory/commitment_systems").

domain_priors:requires_active_enforcement(preparedness_commitment__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__hybrid_reading, 'e62eced0-9a4e-4da1-b394-4956b6803d32').
narrative_ontology:cs_kernel_codification('e62eced0-9a4e-4da1-b394-4956b6803d32', formalized).
narrative_ontology:cs_authority_grounding('e62eced0-9a4e-4da1-b394-4956b6803d32', lineage).
narrative_ontology:cs_interpretation_layer_present('e62eced0-9a4e-4da1-b394-4956b6803d32').
narrative_ontology:cs_reading_relation('e62eced0-9a4e-4da1-b394-4956b6803d32', preparedness_commitment__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('e62eced0-9a4e-4da1-b394-4956b6803d32', preparedness_commitment__competence_reading, coexists_with).
narrative_ontology:cs_axiom('e62eced0-9a4e-4da1-b394-4956b6803d32', foundational, preparedness_requires_dual_maintenance).
narrative_ontology:cs_axiom_status(preparedness_requires_dual_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('e62eced0-9a4e-4da1-b394-4956b6803d32', preparedness_requires_dual_maintenance, empirically_contingent).
narrative_ontology:cs_axiom('e62eced0-9a4e-4da1-b394-4956b6803d32', secondary, tension_between_memorial_and_competence_is_inherent).
narrative_ontology:cs_axiom_status(tension_between_memorial_and_competence_is_inherent, holdable).
narrative_ontology:cs_axiom_grounding('e62eced0-9a4e-4da1-b394-4956b6803d32', tension_between_memorial_and_competence_is_inherent, empirically_contingent).
narrative_ontology:cs_reference_frame('e62eced0-9a4e-4da1-b394-4956b6803d32', post_cold_war_institutionalization).
narrative_ontology:cs_drift_state('e62eced0-9a4e-4da1-b394-4956b6803d32', contemporary_climate_crisis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e62eced0-9a4e-4da1-b394-4956b6803d32', '').
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

% Benefit from the public perception of preparedness, especially the memorial elements that signal commitment. They often prioritize visible, symbolic actions over deeper, more costly competence-building, shifting costs to agencies and taxpayers.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, political_leaders, beneficiary,
    powerful, immediate, mobile, national).

% Bear the financial cost of maintaining both layers of preparedness through taxes. They are often unaware of the distinction between memorial and competence elements, making it difficult to demand accountability for effective preparedness.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, taxpayers, payer,
    organized, biographical, constrained, national).

% Are directly impacted by the quality of preparedness. They experience the tension between memorial drills and actual operational competence. They are identity-locked by their commitment to public service but bear the direct costs of inadequate competence during crises.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, frontline_responders, payer,
    moderate, biographical, identity_locked, local).

% Are the ultimate targets of preparedness efforts but have no direct voice in its design or maintenance. They bear the catastrophic costs of preparedness failures, often after the fact.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, disaster_victims, excluded,
    powerless, immediate, trapped, local).

% Analyze the long-term evolution of preparedness systems, identifying patterns of memorialization, competence decay, and the cyclical nature of commitment. They provide an external, long-term perspective on the constraint's effectiveness.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, institutional_historians, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates long-term societal commitment to disaster readiness (memorial layer) with the practical capacity to respond to and mitigate disasters (competence layer), ensuring a baseline of safety and response capability across generations.
% TRANSFER_FUNCTION: Transfers resources (funding, personnel, attention) from taxpayers to public safety agencies and political leaders, in exchange for the promise and occasional delivery of disaster readiness. It also transfers the burden of preparedness failures to frontline responders and disaster victims.
% ABSENT_VOICES: Future generations and potential disaster victims are largely absent from the decision-making process, bearing the consequences of current choices without direct input. Their interests are represented imperfectly by public safety agencies and political leaders.
% DISAPPEARANCE_RATIONALE: If this layered system of preparedness vanished, the societal commitment to disaster readiness would erode, leading to a rapid decay of competence. This would result in increased vulnerability to disasters, higher casualties, and a breakdown of social order during crises, forcing a complete reorganization of public safety and governance.
% FOUNDING_PROBLEM: Societies face recurring, unpredictable threats (natural disasters, pandemics, technological failures) that require sustained, intergenerational commitment and operational capacity to mitigate their impact.
% FOUNDING_PROBLEM_CORROBORATION: Public safety agencies and institutional historians corroborate that the founding problem of recurring, unpredictable threats remains live. Recent disaster events and ongoing risk assessments from independent scientific bodies attest to the continued necessity of preparedness, even as the effectiveness of current systems is debated.
narrative_ontology:disappearance_verdict(preparedness_commitment__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.45) is moderate because the system does deliver some genuine preparedness, but the tension between memorial and competence layers often leads to inefficiencies and misallocation of resources, creating a 'tax' on effective readiness. Suppression (0.30) is low because direct coercion is not the primary mechanism; rather, it's institutional inertia and the difficulty of challenging established practices. Theater ratio (0.20) reflects the presence of performative elements (drills for show, symbolic investments) that are not purely functional but serve to maintain public commitment. The cyclical nature of extractiveness and theater reflects the 'disaster cycle' where attention and resources surge after a crisis, then wane, leading to periods of competence decay and rising theatricality.
 *
 * PERSPECTIVAL GAP:
 *   Political leaders may perceive the system as a successful 'rope' due to the stability provided by memorial elements, while frontline responders, experiencing the gaps in competence, might see it as closer to a 'snare' during a crisis. The hybrid reading attempts to capture this tension, acknowledging both the coordination function and the extractive inefficiencies.
 *
 * DIRECTIONALITY LOGIC:
 *   Public safety agencies are agenda-setters, balancing the layers. Political leaders are beneficiaries, gaining from the symbolic value of preparedness. Taxpayers and frontline responders are payers, bearing the financial and operational costs, respectively. Disaster victims are excluded, experiencing the consequences without input. Institutional historians are observers, analyzing the system's long-term dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading prevents mislabeling by acknowledging that while the core mandate (preparedness) remains live, the *balance* between memorial and competence elements can drift, leading to mandatrophy in the competence layer even as the memorial layer persists. It's not a full piton because the competence function is still actively, if imperfectly, maintained, and the commitment layer is genuinely functional in preventing abandonment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balance_of_layers_optimal,
    'Is the current balance between memorial and competence elements optimal for effective preparedness, or does one layer dominate to the detriment of the other?',
    'Comparative analysis of preparedness systems across different jurisdictions and historical periods, correlating resource allocation to each layer with actual disaster outcomes and response effectiveness.',
    'If memorial elements are found to consistently dominate at the expense of competence, the extractiveness and theater_ratio would be re-evaluated upwards, pushing the classification closer to a ''snare'' or ''piton''. If competence elements are found to be robustly maintained, the classification would shift towards a ''rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balance_of_layers_optimal, empirical, 'Assesses the functional balance between symbolic commitment and operational capacity.').

omega_variable(
    cost_of_dual_maintenance,
    'What is the irreducible cost of maintaining both memorial and competence layers simultaneously, and how much of the measured extraction is attributable to this inherent complexity versus rent-seeking?',
    'Detailed cost-benefit analysis of integrated preparedness systems, distinguishing between costs necessary for dual function and those arising from inefficiency or misaligned incentives. This would require isolating the ''overhead'' of managing the tension between the two layers.',
    'If the irreducible cost of dual maintenance is high, a larger portion of the measured extractiveness would be reclassified as ''coordination cost'', potentially shifting the constraint towards a ''rope''. If the cost is low, the current extractiveness would be more clearly identified as rent-seeking.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cost_of_dual_maintenance, conceptual, 'Distinguishes inherent complexity costs from extractive overhead in a layered system.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (e.g., budget constraints, institutional inertia) or internalized (e.g., cultural norms against questioning preparedness rituals)?',
    'Post-reform trajectory: if suppression persists after structural barriers are removed (e.g., increased funding, clear mandates), reclassify as partially internalized. Qualitative studies of institutional culture and decision-making processes.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as agents carry the suppression with them. This would make it harder to reform the system even with external pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in institutional preparedness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__hybrid_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__hybrid_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(prep_tr_t10, preparedness_commitment__hybrid_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(prep_tr_t20, preparedness_commitment__hybrid_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(prep_tr_t30, preparedness_commitment__hybrid_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(prep_tr_t40, preparedness_commitment__hybrid_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(prep_tr_t50, preparedness_commitment__hybrid_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__hybrid_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(prep_be_t10, preparedness_commitment__hybrid_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(prep_be_t20, preparedness_commitment__hybrid_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(prep_be_t30, preparedness_commitment__hybrid_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(prep_be_t40, preparedness_commitment__hybrid_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(prep_be_t50, preparedness_commitment__hybrid_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_commitment__hybrid_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(prep_su_t10, preparedness_commitment__hybrid_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(prep_su_t20, preparedness_commitment__hybrid_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(prep_su_t30, preparedness_commitment__hybrid_reading, suppression_requirement, 30, 0.29).
narrative_ontology:measurement(prep_su_t40, preparedness_commitment__hybrid_reading, suppression_requirement, 40, 0.32).
narrative_ontology:measurement(prep_su_t50, preparedness_commitment__hybrid_reading, suppression_requirement, 50, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, preparedness_commitment__competence_reading).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, preparedness_commitment__husk_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'preparedness_commitment' kernel, which also includes 'competence_reading' and 'husk_reading'. This hybrid reading acknowledges the tension and interaction between the two simpler readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
