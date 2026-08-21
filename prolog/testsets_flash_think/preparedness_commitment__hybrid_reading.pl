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
 *   constraint_id: preparedness_commitment__hybrid_reading
 *   human_readable: Preparedness as Layered Commitment-Competence System
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes preparedness as a layered system, where
 *   memorial elements (e.g., historical sites, commemorative events,
 *   institutional rituals) function to stabilize long-term commitment, while
 *   competence elements (e.g., training, drills, resource allocation) aim to
 *   maintain functional operational capacity. The 'hybrid_reading'
 *   acknowledges the necessity of both layers but highlights the inherent
 *   tension and maintenance costs arising from their interaction. The claimed
 *   type is 'tangled_rope' because it genuinely coordinates long-term
 *   societal commitment and operational readiness, but also involves
 *   significant extraction due to the costs of maintaining both layers and
 *   the potential for resources to be misallocated or for memorial aspects to
 *   become performative.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__hybrid_reading, 0.65).
domain_priors:suppression_score(preparedness_commitment__hybrid_reading, 0.55).
domain_priors:theater_ratio(preparedness_commitment__hybrid_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_commitment__hybrid_reading, "Preparedness as Layered Commitment-Competence System").
narrative_ontology:topic_domain(preparedness_commitment__hybrid_reading, "disaster_preparedness/institutional_memory/commitment_systems").

domain_priors:requires_active_enforcement(preparedness_commitment__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__hybrid_reading, '7bcd297f-7218-4834-b102-cc5ab00031cb').
narrative_ontology:cs_kernel_codification('7bcd297f-7218-4834-b102-cc5ab00031cb', formalized).
narrative_ontology:cs_authority_grounding('7bcd297f-7218-4834-b102-cc5ab00031cb', practice).
narrative_ontology:cs_interpretation_layer_present('7bcd297f-7218-4834-b102-cc5ab00031cb').
narrative_ontology:cs_reading_relation('7bcd297f-7218-4834-b102-cc5ab00031cb', preparedness_commitment__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('7bcd297f-7218-4834-b102-cc5ab00031cb', preparedness_commitment__competence_reading, coexists_with).
narrative_ontology:cs_axiom('7bcd297f-7218-4834-b102-cc5ab00031cb', foundational, commitment_requires_memorialization).
narrative_ontology:cs_axiom_status(commitment_requires_memorialization, holdable).
narrative_ontology:cs_axiom_grounding('7bcd297f-7218-4834-b102-cc5ab00031cb', commitment_requires_memorialization, conventional).
narrative_ontology:cs_axiom('7bcd297f-7218-4834-b102-cc5ab00031cb', foundational, operational_capacity_is_essential).
narrative_ontology:cs_axiom_status(operational_capacity_is_essential, holdable).
narrative_ontology:cs_axiom_grounding('7bcd297f-7218-4834-b102-cc5ab00031cb', operational_capacity_is_essential, empirically_contingent).
narrative_ontology:cs_reference_frame('7bcd297f-7218-4834-b102-cc5ab00031cb', balanced_layered_resilience).
narrative_ontology:cs_drift_state('7bcd297f-7218-4834-b102-cc5ab00031cb', contemporary_resource_constraints, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7bcd297f-7218-4834-b102-cc5ab00031cb', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__hybrid_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, institutional_memory_keepers).
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, disaster_response_agencies).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, taxpayers).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, frontline_responders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for maintaining the memorial elements of preparedness, ensuring that past lessons are not forgotten. They benefit from the stability these elements provide but also bear the cost of their maintenance and the tension with competence demands.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, institutional_memory_keepers, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the foundational commitment and some level of competence provided by the system. They are tasked with operational readiness but often find resources diverted to memorial aspects or face challenges in translating memorial commitment into practical competence.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, disaster_response_agencies, beneficiary,
    institutional, biographical, constrained, national).

% Bear the financial costs of maintaining the entire layered preparedness system, including both memorial and competence elements. They often lack direct influence over the allocation of these resources.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, taxpayers, payer,
    powerless, immediate, constrained, national).

% Directly experience the consequences of the system's actual competence. While they benefit from functional training and resources, they also bear the costs of any gaps between memorial commitment and operational reality, including potential risks to their safety.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, frontline_responders, payer,
    moderate, immediate, constrained, local).

% Responsible for setting the overall policy and funding priorities for preparedness. They navigate the political demands for visible memorialization and the practical needs for operational competence, often balancing competing interests.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, policy_makers, agenda_setter,
    powerful, biographical, mobile, national).

% Academics, researchers, and independent analysts who study the effectiveness and evolution of preparedness systems. They provide critical assessments of the balance between memorial and competence elements but have no direct power to alter the system.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, analytical_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_commitment__hybrid_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_commitment__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes long-term societal commitment to disaster preparedness through memorial elements (e.g., commemorative events, historical archives) while simultaneously maintaining functional operational competence through active training, resource allocation, and drills.
% TRANSFER_FUNCTION: Transfers financial resources, personnel time, and political attention to sustain both the symbolic commitment to 'never again' and the practical capacity to respond. It also transfers the burden of potential disaster impacts from the general populace to the preparedness system itself.
% ABSENT_VOICES: Future generations, who will inherit the consequences of current preparedness choices, and communities disproportionately affected by disasters, who may advocate for different resource allocations or a stronger emphasis on specific competence areas, are often not directly represented in the system's design or ongoing adjustments.
% DISAPPEARANCE_RATIONALE: If this layered system vanished overnight, the long-term commitment to preparedness would erode, operational competence would atrophy due to lack of sustained investment and practice, and society would become catastrophically vulnerable to foreseeable and recurring disasters, leading to widespread loss of life and infrastructure.
% FOUNDING_PROBLEM: Preventing catastrophic loss of life and infrastructure from recurring disasters, ensuring long-term societal resilience, and learning from past failures to avoid their repetition.
% FOUNDING_PROBLEM_CORROBORATION: Disaster historians, climate scientists, public health experts, and independent risk assessment bodies consistently corroborate the ongoing need for robust preparedness, from outside the direct beneficiaries of the existing system. They highlight the persistent threat of disasters and the continuous challenge of maintaining resilience.
narrative_ontology:disappearance_verdict(preparedness_commitment__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(preparedness_commitment__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__hybrid_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_commitment__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_commitment__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.65) due to the substantial resources required to maintain both the symbolic and functional aspects of preparedness, and the inefficiencies that can arise from the tension between them. Suppression (0.55) is moderate, reflecting the active enforcement needed to ensure compliance with preparedness protocols and to manage the inherent conflicts between memorial and competence priorities. Theater ratio (0.45) is also moderate, as memorial elements, while crucial for commitment, can sometimes become performative without corresponding functional competence. The cyclical measurements reflect periods of heightened attention to competence (e.g., after a major disaster) followed by a drift back towards more costly or performative memorialization.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of institutional memory keepers, the memorial layer is essential for long-term commitment and identity. From the perspective of frontline responders, the competence layer is paramount for effective action and safety. The 'hybrid_reading' attempts to integrate these, but the inherent tension means that no single seat experiences the constraint as perfectly balanced or efficient. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional memory keepers and disaster response agencies are beneficiaries, as they are empowered and resourced by the system, though they also bear the costs of its internal tensions. Taxpayers and frontline responders are victims, bearing the financial costs and the direct risks of any system failures or inefficiencies. Policy makers act as agenda-setters, attempting to balance the competing demands of the two layers. Analytical observers provide external assessment.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint best understood as the ''hybrid_reading'' of the ''preparedness_commitment'' kernel, or does it lean more towards a ''husk_reading'' or ''competence_reading''?',
    'Empirical analysis of resource allocation, training outcomes, and post-disaster performance: if resources disproportionately flow to symbolic acts without functional improvement, it leans towards ''husk''; if memorial elements are neglected, it leans towards ''competence''.',
    'If reclassified as ''husk_reading'', extractiveness and theater_ratio would be higher, and the coordination function would be seen as cover. If reclassified as ''competence_reading'', extractiveness might be lower, and the focus would be on pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, empirical, 'Ambiguity in the dominant reading of preparedness commitment.').

omega_variable(
    balance_of_layers_optimality,
    'Is the current balance between memorial and competence elements optimal for achieving long-term resilience, or is one layer over-resourced/under-resourced relative to its function?',
    'Cost-benefit analysis comparing investment in memorial vs. competence elements against actual disaster outcomes and long-term commitment metrics.',
    'If imbalanced, the measured extractiveness may be higher than necessary, indicating inefficiency or misallocation of resources within the system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balance_of_layers_optimality, empirical, 'Optimality of resource allocation between memorial and competence layers.').

omega_variable(
    drift_to_husk_risk,
    'How easily does the hybrid system drift towards a ''husk_reading'' (memorial performance without competence) when under resource pressure or leadership changes?',
    'Longitudinal studies of preparedness systems under stress, observing changes in resource allocation, training frequency, and post-event review processes.',
    'A high risk of drift to ''husk'' implies a more fragile and potentially more extractive system, where the coordination function is easily undermined by performative elements.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(drift_to_husk_risk, empirical, 'Susceptibility of the hybrid system to degrade into purely performative preparedness.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative preparedness models structural (institutional inertia, funding mechanisms) or internalized (belief in the existing system''s efficacy)?',
    'Post-policy-change analysis: if alternative models emerge and thrive after structural barriers are removed, suppression was structural. If resistance to change persists, it''s partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as actors carry the suppression with them even if external barriers are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative preparedness models.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__hybrid_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__hybrid_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(prep_tr_t10, preparedness_commitment__hybrid_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement(prep_tr_t20, preparedness_commitment__hybrid_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement(prep_tr_t30, preparedness_commitment__hybrid_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(prep_tr_t40, preparedness_commitment__hybrid_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement(prep_tr_t50, preparedness_commitment__hybrid_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__hybrid_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(prep_be_t10, preparedness_commitment__hybrid_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(prep_be_t20, preparedness_commitment__hybrid_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(prep_be_t30, preparedness_commitment__hybrid_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(prep_be_t40, preparedness_commitment__hybrid_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(prep_be_t50, preparedness_commitment__hybrid_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_commitment__hybrid_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(prep_su_t10, preparedness_commitment__hybrid_reading, suppression_requirement, 10, 0.53).
narrative_ontology:measurement(prep_su_t20, preparedness_commitment__hybrid_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(prep_su_t30, preparedness_commitment__hybrid_reading, suppression_requirement, 30, 0.54).
narrative_ontology:measurement(prep_su_t40, preparedness_commitment__hybrid_reading, suppression_requirement, 40, 0.57).
narrative_ontology:measurement(prep_su_t50, preparedness_commitment__hybrid_reading, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
