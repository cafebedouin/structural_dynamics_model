% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__survival_competence_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: catastrophe_memory_kernel__survival_competence_reading
 *   human_readable: Ritual as Persecution-Survival Competence Transmission
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes a specific reading of a 'catastrophe memory
 *   kernel' — the idea that ritual practice encodes and transmits adaptive
 *   capacity for persecution-survival within a community. It focuses on the
 *   functional aspect of ritual as a form of 'survival training' that
 *   enhances community resilience under threat. The constraint is claimed as
 *   a Rope, reflecting its genuine coordination function for the community,
 *   but with moderate extractiveness due to the costs borne by individuals
 *   facing assimilation pressures. This is one reading of a contested kernel,
 *   where other readings emphasize symbolic continuity, trauma encoding, or
 *   boundary maintenance.
 *
 * KEY AGENTS:
 *   - persecuted_community: Primary beneficiary (organized/identity_locked) — gains adaptive capacity and resilience.
 *   - assimilating_individuals: Primary payer (powerless/constrained) — bears costs of resisting assimilation.
 *   - community_elders_and_leaders: Agenda setter (institutional/identity_locked) — maintains and transmits the ritual.
 *   - dominant_culture: Observer (institutional/analytical) — creates the external pressure context.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__survival_competence_reading, 0.45).
domain_priors:suppression_score(catastrophe_memory_kernel__survival_competence_reading, 0.6).
domain_priors:theater_ratio(catastrophe_memory_kernel__survival_competence_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__survival_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__survival_competence_reading, "Ritual as Persecution-Survival Competence Transmission").
narrative_ontology:topic_domain(catastrophe_memory_kernel__survival_competence_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__survival_competence_reading, '5bdc238e-6243-4783-b1f7-ab6cef9b602b').
narrative_ontology:cs_kernel_codification('5bdc238e-6243-4783-b1f7-ab6cef9b602b', implicit).
narrative_ontology:cs_authority_grounding('5bdc238e-6243-4783-b1f7-ab6cef9b602b', practice).
narrative_ontology:cs_interpretation_layer_present('5bdc238e-6243-4783-b1f7-ab6cef9b602b').
narrative_ontology:cs_reading_relation('5bdc238e-6243-4783-b1f7-ab6cef9b602b', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('5bdc238e-6243-4783-b1f7-ab6cef9b602b', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_reading_relation('5bdc238e-6243-4783-b1f7-ab6cef9b602b', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('5bdc238e-6243-4783-b1f7-ab6cef9b602b', foundational, ritual_transmits_adaptive_behavior).
narrative_ontology:cs_axiom_status(ritual_transmits_adaptive_behavior, holdable).
narrative_ontology:cs_axiom_grounding('5bdc238e-6243-4783-b1f7-ab6cef9b602b', ritual_transmits_adaptive_behavior, empirically_contingent).
narrative_ontology:cs_axiom('5bdc238e-6243-4783-b1f7-ab6cef9b602b', secondary, collective_memory_is_survival_resource).
narrative_ontology:cs_axiom_status(collective_memory_is_survival_resource, holdable).
narrative_ontology:cs_axiom_grounding('5bdc238e-6243-4783-b1f7-ab6cef9b602b', collective_memory_is_survival_resource, empirically_contingent).
narrative_ontology:cs_reference_frame('5bdc238e-6243-4783-b1f7-ab6cef9b602b', community_resilience_framework).
narrative_ontology:cs_drift_state('5bdc238e-6243-4783-b1f7-ab6cef9b602b', contemporary_globalized_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('5bdc238e-6243-4783-b1f7-ab6cef9b602b', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, persecuted_community).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__survival_competence_reading, assimilating_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The community benefits from the ritual by collectively rehearsing historical persecution events, thereby transmitting adaptive strategies and fostering resilience against future threats. The ritual reinforces a shared identity crucial for survival.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, persecuted_community, beneficiary,
    organized, generational, identity_locked, local).

% Individuals who seek to assimilate into the dominant culture find the ritual's demands (time, emotional labor, adherence to distinct practices) to be a cost. The ritual actively counteracts assimilation pressures, making it harder for these individuals to fully integrate into external society without perceived loss of identity or community standing.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, assimilating_individuals, payer,
    powerless, biographical, constrained, local).

% These individuals are responsible for preserving, teaching, and enforcing the ritual practices. They ensure the accurate transmission of the 'survival competence' narrative and its associated behaviors, acting as custodians of collective memory and adaptive capacity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, community_elders_and_leaders, agenda_setter,
    institutional, generational, identity_locked, local).

% The dominant culture often views the ritual as an anachronism or a barrier to integration, exerting subtle or overt pressure for the persecuted community to abandon such practices. It does not directly participate but its presence creates the context for the ritual's function.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, dominant_culture, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective memory and behavioral responses of a persecuted community, ensuring the transmission of adaptive strategies for survival across generations by rehearsing past catastrophes and their successful navigation.
% TRANSFER_FUNCTION: Transfers historical knowledge, emotional resilience, and practical survival strategies from past generations to current and future members of the community, at the cost of individual assimilation and emotional labor.
% ABSENT_VOICES: Individuals who have fully assimilated into the dominant culture are absent; they would argue the ritual is an unnecessary burden that prevents full integration and perpetuates historical grievances rather than fostering present-day adaptation.
% DISAPPEARANCE_RATIONALE: If the ritual vanished, the community would lose a core mechanism for transmitting adaptive capacity and collective identity. Over generations, this would lead to increased vulnerability to persecution, erosion of distinct cultural identity, and accelerated assimilation, fundamentally altering the community's structure and survival prospects.
% FOUNDING_PROBLEM: The historical and ongoing threat of persecution and existential threats to the community's survival and distinct identity.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies of persecuted communities, historical records of survival under duress, and sociological analyses of cultural resilience corroborate the ongoing relevance of the founding problem. Community members' testimonies also consistently affirm the ritual's role in maintaining their distinct identity and capacity to endure.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__survival_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__survival_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_kernel__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__survival_competence_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__survival_competence_reading_tests).
:- end_tests(catastrophe_memory_kernel__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate, reflecting the necessary 'cost' of maintaining a distinct identity and behavioral repertoire in the face of external pressures, primarily borne by individuals who might otherwise assimilate. Suppression (0.6) is present as the internal social pressure to conform to ritual practices, which are seen as vital for group cohesion and survival. Theater ratio is low (0.1) because the ritual is highly functional; its performative aspects directly serve the purpose of memory transmission and competence building. The claimed type is Rope because the primary function is genuine coordination for collective survival, with beneficiaries (the community) and payers (assimilating individuals) both contributing to and benefiting from the overall resilience, albeit with asymmetric costs.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the persecuted community and its leaders, the ritual is a vital, life-sustaining coordination mechanism. From the perspective of assimilating individuals, it can feel like a burden or a barrier to personal freedom and integration. The engine's classification will reflect this divergence, with the community seat likely computing as a Rope/Scaffold and the individual payer seat computing as a Tangled Rope or Snare due to the costs of identity-lock and constrained exit.
 *
 * DIRECTIONALITY LOGIC:
 *   The persecuted_community is a clear beneficiary (d near 0.0) as the ritual directly contributes to its survival and resilience. Assimilating_individuals are targets (d near 1.0) because the ritual imposes costs on their desire for external integration. Community_elders_and_leaders are also beneficiaries (d near 0.0) as they are the custodians of this adaptive capacity and their authority is reinforced by its maintenance. The dominant_culture is an observer, its directionality is analytical.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (persecution-survival) is still live, preventing mislabeling as a Piton. The moderate extractiveness and suppression are directly tied to the ongoing function of transmitting adaptive capacity in a hostile environment, distinguishing it from pure extraction. The classification as Rope acknowledges its genuine coordination function, while the extractiveness metrics capture the real costs of maintaining this function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    survival_competence_empirical_validation,
    'To what extent can the ''adaptive capacity for persecution-survival'' transmitted by ritual be empirically validated as effective?',
    'Longitudinal ethnographic studies comparing communities with and without such rituals under similar persecution pressures, or historical analysis of survival rates correlated with ritual adherence.',
    'If empirically validated, the functional claim of this reading is strengthened, reinforcing its Rope classification. If not, the ritual''s function might be reclassified as more symbolic or identity-based, potentially shifting the constraint towards a more extractive type if costs remain high without clear benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survival_competence_empirical_validation, empirical, 'Empirical evidence for the effectiveness of ritual in transmitting survival competence.').

omega_variable(
    kernel_reading_focus_shift,
    'Is this constraint primarily about transmitting survival competence, or is it more fundamentally about symbolic continuity, trauma encoding, or boundary maintenance?',
    'Analysis of community discourse, ritual content, and historical context to determine the dominant explicit and implicit functions. This would involve comparing the ''survival competence'' narrative with those of sibling readings.',
    'If the primary function is found to be symbolic continuity (symbol_continuity_reading), the extractiveness might be lower (more Rope-like). If it''s trauma encoding (trauma_encoding_reading), the suppression might be higher. If it''s boundary maintenance (boundary_maintenance_reading), the extractiveness on assimilating individuals would be more central. This would lead to reclassification under a different kernel reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_focus_shift, conceptual, 'Ambiguity in the primary function of the catastrophe memory ritual across different readings of the kernel.').

omega_variable(
    identity_lock_vs_choice,
    'To what extent is the ''identity_locked'' exit option for the persecuted community a genuine structural constraint versus a chosen commitment?',
    'Sociological studies on the perceived costs and benefits of assimilation, and the psychological impacts of leaving the community''s identity frame. Analysis of historical periods where assimilation was more or less feasible.',
    'If identity-lock is primarily a chosen commitment, the effective suppression and extractiveness for individuals might be lower, as the ''cost'' is self-imposed. If it''s a structural constraint (e.g., due to external discrimination), the effective extractiveness is higher, as exit is genuinely foreclosed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_choice, empirical, 'Distinguishing between chosen identity commitment and structurally imposed identity-lock.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__survival_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 25, 0.1).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(cata_tr_t75, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 75, 0.1).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 25, 0.42).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 50, 0.45).
narrative_ontology:measurement(cata_be_t75, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 75, 0.43).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 50, 0.6).
narrative_ontology:measurement(cata_su_t75, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 75, 0.59).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 100, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__survival_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_kernel__survival_competence_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__trauma_encoding_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'catastrophe_memory_kernel'. Its sibling readings (symbol_continuity_reading, trauma_encoding_reading, boundary_maintenance_reading) offer alternative interpretations of the same ritual practices, each with distinct beneficiaries, victims, and classifications. All are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
