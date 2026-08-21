% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence__husk_reading, []).

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
 *   constraint_id: preparedness_persistence__husk_reading
 *   human_readable: Disaster Preparedness as Memorial Performance (Husk Reading)
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes disaster preparedness as a 'husk' of its former
 *   self: the outward forms of drills and inspections persist, but the
 *   underlying operational competence has atrophied. This reading frames the
 *   constraint as a Piton, where the primary function has degraded, but the
 *   structure remains due to institutional inertia and the theatrical
 *   maintenance of legitimacy. The claimed type is Piton, reflecting this
 *   atrophy, while the metrics show low extractiveness (as no single party
 *   captures significant rents from the *atrophied* function) but high
 *   theater ratio.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__husk_reading, 0.15).
domain_priors:suppression_score(preparedness_persistence__husk_reading, 0.05).
domain_priors:theater_ratio(preparedness_persistence__husk_reading, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, theater_ratio, 0.85).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__husk_reading, piton).
narrative_ontology:human_readable(preparedness_persistence__husk_reading, "Disaster Preparedness as Memorial Performance (Husk Reading)").
narrative_ontology:topic_domain(preparedness_persistence__husk_reading, "disaster_preparedness/institutional_memory/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__husk_reading, '807328eb-37bd-43af-a4c3-50ad409d9302').
narrative_ontology:cs_kernel_codification('807328eb-37bd-43af-a4c3-50ad409d9302', formalized).
narrative_ontology:cs_authority_grounding('807328eb-37bd-43af-a4c3-50ad409d9302', lineage).
narrative_ontology:cs_interpretation_layer_present('807328eb-37bd-43af-a4c3-50ad409d9302').
narrative_ontology:cs_reading_relation('807328eb-37bd-43af-a4c3-50ad409d9302', preparedness_persistence__competence_reading, forecloses).
narrative_ontology:cs_reading_relation('807328eb-37bd-43af-a4c3-50ad409d9302', preparedness_persistence__hybrid_reading, influences).
narrative_ontology:cs_axiom('807328eb-37bd-43af-a4c3-50ad409d9302', foundational, form_supersedes_function).
narrative_ontology:cs_axiom_status(form_supersedes_function, holdable).
narrative_ontology:cs_axiom_grounding('807328eb-37bd-43af-a4c3-50ad409d9302', form_supersedes_function, conventional).
narrative_ontology:cs_axiom('807328eb-37bd-43af-a4c3-50ad409d9302', secondary, appearance_equals_reality).
narrative_ontology:cs_axiom_status(appearance_equals_reality, holdable).
narrative_ontology:cs_axiom_grounding('807328eb-37bd-43af-a4c3-50ad409d9302', appearance_equals_reality, conventional).
narrative_ontology:cs_reference_frame('807328eb-37bd-43af-a4c3-50ad409d9302', original_mandate_of_readiness).
narrative_ontology:cs_drift_state('807328eb-37bd-43af-a4c3-50ad409d9302', contemporary_era_of_audit_culture, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('807328eb-37bd-43af-a4c3-50ad409d9302', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__husk_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__husk_reading, institutional_legitimacy).
narrative_ontology:constraint_victim(preparedness_persistence__husk_reading, population_at_flood_risk).
narrative_ontology:constraint_victim(preparedness_persistence__husk_reading, first_responders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The abstract concept of institutional legitimacy benefits from the appearance of preparedness, even if actual competence is low. The public perceives the institution as 'doing its job' by conducting drills and inspections, which maintains public trust and funding.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, institutional_legitimacy, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_non_agent(preparedness_persistence__husk_reading, institutional_legitimacy).

% These agencies administer the drills and inspections. While they may genuinely desire competence, the institutional incentives often prioritize the completion of scheduled activities and documentation over genuine operational readiness, leading to a focus on form over substance. They are constrained by budget cycles and political mandates.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, disaster_preparedness_agencies, agenda_setter,
    institutional, biographical, constrained, national).

% This population bears the ultimate cost of atrophied preparedness. They are given a false sense of security by the drills and inspections, but face direct harm when actual disasters occur and the response is inadequate. Their exit options are limited by geography and economic constraints.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, population_at_flood_risk, payer,
    powerless, immediate, trapped, local).

% These individuals participate in the drills and are on the front lines during disasters. They experience the gap between theoretical preparedness and actual capacity directly. They bear the burden of inadequate training and resources, leading to increased risk and burnout. Their exit options are constrained by professional identity and commitment to public service.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, first_responders, payer,
    moderate, biographical, constrained, local).

% Sets the policy and funding priorities for disaster preparedness. Often prioritizes visible actions (drills, reports) that signal competence to the electorate, rather than deep, costly investments in actual capacity that may not be immediately apparent. Their time horizon is often tied to election cycles.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, political_leadership, agenda_setter,
    powerful, immediate, mobile, national).

% External bodies tasked with assessing the effectiveness of preparedness programs. They often identify gaps between reported compliance and actual readiness, but their recommendations may be ignored or diluted by political and institutional pressures. Their power is in reporting, not direct enforcement.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, independent_auditors, observer,
    organized, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint coordinates the appearance of readiness, ensuring that various agencies and personnel perform their assigned roles in drills and inspections, creating a public narrative of competence and safety.
% TRANSFER_FUNCTION: Transfers a sense of security and institutional legitimacy to the public and political leadership, in exchange for the actual operational capacity of preparedness agencies and the safety of at-risk populations.
% ABSENT_VOICES: The voices of those who have experienced actual disaster response failures, or those who would advocate for a radical overhaul of preparedness systems based on genuine operational metrics, are often marginalized or dismissed as 'alarmist' in favor of maintaining the existing performance-based system.
% DISAPPEARANCE_RATIONALE: If the performance of drills and inspections vanished overnight, the illusion of preparedness would collapse, leading to a crisis of institutional legitimacy. Public trust would erode, funding would be questioned, and there would be immense pressure to either rebuild genuine capacity or explicitly acknowledge the lack thereof. The political and social landscape around disaster response would be forced to rearrange.
% FOUNDING_PROBLEM: The original problem was to ensure public safety and effective response to natural and man-made disasters through coordinated planning, training, and resource allocation.
% FOUNDING_PROBLEM_CORROBORATION: Independent disaster analysts, academic researchers in public administration, and investigative journalists consistently report that while the 'form' of preparedness persists, the 'function' of genuine operational competence has atrophied in many areas. Post-disaster reviews frequently highlight systemic failures that indicate a disconnect between drills and reality. The agencies themselves, and political leadership, often maintain the problem is 'live' but under-resourced, rather than acknowledging the atrophy of competence.
narrative_ontology:disappearance_verdict(preparedness_persistence__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__husk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(preparedness_persistence__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__husk_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence__husk_reading_tests).
:- end_tests(preparedness_persistence__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.15) reflects that while the system is inefficient and ultimately harmful, no single party is actively extracting large rents from the *degraded* function; the 'benefit' is diffuse institutional legitimacy. Suppression is low (0.05) because the constraint persists more through inertia and lack of challenge than active coercion. The high theater ratio (0.85) is central to this reading: the vast majority of activity is performative, designed to create an impression of readiness rather than achieve actual readiness. Accessibility collapse is low (0.2) because the 'alternatives' (genuine competence) are conceptually clear but institutionally difficult to achieve. Resistance is low (0.1) because the diffuse nature of the harm and the institutional inertia make organized resistance difficult.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the disaster preparedness agencies and political leadership, the constraint might be seen as a necessary, if imperfect, Rope or Scaffold, providing some level of coordination and public reassurance. From the perspective of the population at risk and first responders, it is a Piton, a hollow shell that fails when truly needed. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional legitimacy (an abstract entity) is the primary beneficiary, as the performance maintains its standing. Disaster preparedness agencies and political leadership act as agenda-setters, perpetuating the system. The population at flood risk and first responders are the victims, bearing the costs of inadequate actual preparedness. Independent auditors are observers, capable of analytical exit but often constrained in their ability to force change.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is a clear case of mandatrophy: the original mandate (genuine preparedness) has atrophied, but the institutional structure persists. The classification as Piton directly addresses this by identifying the high theater ratio and low actual benefit. It prevents mislabeling as a Rope (which would imply genuine coordination) or a Snare (which would imply active, concentrated extraction). The persistence is due to inertia and the diffuse benefit of legitimacy, not active rent-seeking by a specific party from the atrophied function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_measurement_ambiguity,
    'How can ''operational competence'' be objectively measured and distinguished from ''compliance with drill protocols''?',
    'Development of independent, real-world simulation exercises with unannounced parameters, and post-disaster analysis that directly links preparedness activities to response outcomes, rather than relying on self-reported metrics.',
    'If competence can be robustly measured and shown to be low, it would strongly corroborate the ''husk_reading'' and solidify the Piton classification. If competence is higher than assumed, it would shift towards a ''hybrid_reading'' or even ''competence_reading''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competence_measurement_ambiguity, empirical, 'Distinguishing actual operational competence from performative compliance.').

omega_variable(
    institutional_incentive_structure,
    'To what extent do institutional incentives (funding, career progression, political optics) actively disincentivize genuine competence building in favor of performative compliance?',
    'Qualitative sociological studies of preparedness agencies, analysis of budget allocations vs. reported outcomes, and examination of career paths for individuals who prioritize ''real'' vs. ''performative'' readiness.',
    'Strong evidence of perverse incentives would reinforce the ''husk_reading'' by explaining the mechanism of atrophy. If incentives are found to align with competence, it would challenge this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_incentive_structure, empirical, 'Role of institutional incentives in driving performative vs. actual preparedness.').

omega_variable(
    reading_framing_choice,
    'Is this constraint best framed as a ''husk_reading'' (atrophied competence), a ''competence_reading'' (live knowledge), or a ''hybrid_reading'' (stratified competence)?',
    'Consensus among independent experts after reviewing empirical evidence from multiple disaster responses and preparedness audits, explicitly weighing the evidence for each reading''s core claims.',
    'The choice of framing fundamentally alters the classification. The ''husk_reading'' leads to Piton, ''competence_reading'' to Rope/Mountain, and ''hybrid_reading'' to a more complex, potentially Tangled Rope or Scaffold, depending on the balance of competence and ritual.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_choice, conceptual, 'Under-determination of the core framing for preparedness persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__husk_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_persistence__husk_reading, theater_ratio, 0, 0.6).
narrative_ontology:measurement(prep_tr_t5, preparedness_persistence__husk_reading, theater_ratio, 5, 0.7).
narrative_ontology:measurement(prep_tr_t10, preparedness_persistence__husk_reading, theater_ratio, 10, 0.78).
narrative_ontology:measurement(prep_tr_t15, preparedness_persistence__husk_reading, theater_ratio, 15, 0.82).
narrative_ontology:measurement(prep_tr_t20, preparedness_persistence__husk_reading, theater_ratio, 20, 0.85).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_persistence__husk_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(prep_be_t5, preparedness_persistence__husk_reading, base_extractiveness, 5, 0.08).
narrative_ontology:measurement(prep_be_t10, preparedness_persistence__husk_reading, base_extractiveness, 10, 0.12).
narrative_ontology:measurement(prep_be_t15, preparedness_persistence__husk_reading, base_extractiveness, 15, 0.14).
narrative_ontology:measurement(prep_be_t20, preparedness_persistence__husk_reading, base_extractiveness, 20, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_persistence__husk_reading, suppression_requirement, 0, 0.02).
narrative_ontology:measurement(prep_su_t5, preparedness_persistence__husk_reading, suppression_requirement, 5, 0.03).
narrative_ontology:measurement(prep_su_t10, preparedness_persistence__husk_reading, suppression_requirement, 10, 0.04).
narrative_ontology:measurement(prep_su_t15, preparedness_persistence__husk_reading, suppression_requirement, 15, 0.045).
narrative_ontology:measurement(prep_su_t20, preparedness_persistence__husk_reading, suppression_requirement, 20, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__husk_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_persistence__husk_reading, preparedness_persistence__competence_reading).
narrative_ontology:affects_constraint(preparedness_persistence__husk_reading, preparedness_persistence__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'husk_reading' of the 'preparedness_persistence' kernel, where drills and inspections are memorial performance. It is linked to the 'competence_reading' and 'hybrid_reading' as sibling interpretations of the same underlying kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
