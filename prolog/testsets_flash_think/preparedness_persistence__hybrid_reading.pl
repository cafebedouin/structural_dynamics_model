% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence__hybrid_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: preparedness_persistence__hybrid_reading
 *   human_readable: Stratified Disaster Preparedness (Hybrid Reading)
 *   domain: disaster_preparedness/institutional_memory
 *
 * SUMMARY:
 *   This constraint describes disaster preparedness as a stratified system,
 *   where some components (e.g., engineering inspections) maintain genuine
 *   competence, while others (e.g., evacuation drills) have largely atrophied
 *   into ritualized performance. This is the 'hybrid_reading' of the
 *   'preparedness_persistence' kernel, acknowledging both functional and
 *   performative aspects within the same overall system. The overall
 *   classification as a Piton reflects that the system's persistence is
 *   increasingly driven by inertia and theatrical maintenance, even as
 *   pockets of genuine competence remain.
 *
 * KEY AGENTS:
 *   - emergency_management_agencies: Agenda setter (institutional/constrained) — administers and benefits from the system's persistence.
 *   - engineering_inspection_teams: Beneficiary (organized/mobile) — performs functional work within the system.
 *   - public_citizens: Payer/Excluded (powerless/trapped) — bears risk, pays taxes, lacks voice.
 *   - frontline_responders: Payer (organized/constrained) — faces operational gaps from ritualization.
 *   - drill_contractors: Beneficiary (powerful/arbitrage) — profits from ritualized activities.
 *   - critical_evaluators: Observer/Excluded (analytical/analytical) — assesses efficacy, often ignored.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__hybrid_reading, 0.45).
domain_priors:suppression_score(preparedness_persistence__hybrid_reading, 0.55).
domain_priors:theater_ratio(preparedness_persistence__hybrid_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__hybrid_reading, piton).
narrative_ontology:human_readable(preparedness_persistence__hybrid_reading, "Stratified Disaster Preparedness (Hybrid Reading)").
narrative_ontology:topic_domain(preparedness_persistence__hybrid_reading, "disaster_preparedness/institutional_memory").

domain_priors:requires_active_enforcement(preparedness_persistence__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__hybrid_reading, '61cfc0d9-ce4b-433e-8e39-a3169d1e4146').
narrative_ontology:cs_kernel_codification('61cfc0d9-ce4b-433e-8e39-a3169d1e4146', formalized).
narrative_ontology:cs_authority_grounding('61cfc0d9-ce4b-433e-8e39-a3169d1e4146', lineage).
narrative_ontology:cs_interpretation_layer_present('61cfc0d9-ce4b-433e-8e39-a3169d1e4146').
narrative_ontology:cs_reading_relation('61cfc0d9-ce4b-433e-8e39-a3169d1e4146', preparedness_persistence__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('61cfc0d9-ce4b-433e-8e39-a3169d1e4146', preparedness_persistence__competence_reading, coexists_with).
narrative_ontology:cs_axiom('61cfc0d9-ce4b-433e-8e39-a3169d1e4146', foundational, preparedness_is_stratified_competence_and_ritual).
narrative_ontology:cs_axiom_status(preparedness_is_stratified_competence_and_ritual, holdable).
narrative_ontology:cs_axiom_grounding('61cfc0d9-ce4b-433e-8e39-a3169d1e4146', preparedness_is_stratified_competence_and_ritual, empirically_contingent).
narrative_ontology:cs_axiom('61cfc0d9-ce4b-433e-8e39-a3169d1e4146', secondary, ritual_maintains_institutional_form).
narrative_ontology:cs_axiom_status(ritual_maintains_institutional_form, holdable).
narrative_ontology:cs_axiom_grounding('61cfc0d9-ce4b-433e-8e39-a3169d1e4146', ritual_maintains_institutional_form, empirically_contingent).
narrative_ontology:cs_reference_frame('61cfc0d9-ce4b-433e-8e39-a3169d1e4146', functional_stratification_model).
narrative_ontology:cs_drift_state('61cfc0d9-ce4b-433e-8e39-a3169d1e4146', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('61cfc0d9-ce4b-433e-8e39-a3169d1e4146', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(preparedness_persistence__hybrid_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, emergency_management_agencies).
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, drill_contractors).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, public_citizens).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, frontline_responders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, engineering_inspection_teams).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers preparedness mandates, allocates budgets for drills and inspections. Benefits from maintaining the status quo, even if some components are ritualized, as it preserves institutional relevance and funding streams.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, emergency_management_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Performs genuinely competent technical assessments of infrastructure. Their work is functional and critical, representing the 'mountain' aspect of preparedness. They benefit from the system's overall funding, even if other parts are less effective.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, engineering_inspection_teams, beneficiary,
    organized, biographical, mobile, regional).

% Pays for preparedness through taxes but bears the primary risk of disaster due to gaps in actual readiness. Often excluded from meaningful input on preparedness efficacy, relying on official assurances.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, public_citizens, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__hybrid_reading, public_citizens, excluded).

% Operates at the interface of actual disaster and preparedness plans. Experiences the direct consequences of ritualized drills and competent inspections, often facing gaps between training and reality. Bears the cost of system inefficiencies and unpreparedness.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, frontline_responders, payer,
    organized, biographical, constrained, local).

% Profits from designing and executing evacuation drills and other preparedness exercises, regardless of their actual efficacy. Their business model incentivizes the continuation of these rituals, even if they are largely performative.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, drill_contractors, beneficiary,
    powerful, biographical, arbitrage, national).

% Academics, journalists, and independent analysts who assess the true state of preparedness. Their findings often highlight the disparity between claimed competence and ritualized performance, but their voices may be suppressed or ignored by official channels.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, critical_evaluators, observer,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__hybrid_reading, critical_evaluators, excluded).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate various agencies and public responses to potential disasters, ensuring a baseline level of readiness and a framework for emergency action.
% TRANSFER_FUNCTION: Transfers public funds (via taxes) to emergency management agencies and contractors for preparedness activities. It also transfers the burden of actual risk and operational gaps onto frontline responders and the public.
% ABSENT_VOICES: The public, particularly those in high-risk areas, and independent critical evaluators who would demand greater transparency and accountability for the efficacy of preparedness measures, rather than their mere performance.
% DISAPPEARANCE_RATIONALE: If the entire preparedness framework vanished, even its ritualized components, the immediate response to any disaster would be chaotic, leading to significantly higher casualties and economic damage. The competent parts (e.g., infrastructure inspection) would cease, and the performative parts (e.g., drills) would no longer provide even a theatrical sense of security, forcing a complete re-evaluation and reconstruction of disaster response mechanisms.
% FOUNDING_PROBLEM: To mitigate the catastrophic impact of natural and man-made disasters on populations and infrastructure, ensuring public safety and rapid recovery.
% FOUNDING_PROBLEM_CORROBORATION: Emergency management agencies claim the problem is live and complex, requiring continuous funding. Independent audits, post-disaster reports, and critical evaluators (outside the benefiting parties) argue that while the problem is real, significant portions of the current system have atrophied into ritual, leaving the core problem inadequately addressed.
narrative_ontology:disappearance_verdict(preparedness_persistence__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(preparedness_persistence__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__hybrid_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence__hybrid_reading_tests).
:- end_tests(preparedness_persistence__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `claimed_type` is Piton because the overall system's persistence is increasingly due to institutional inertia and theatrical maintenance, as evidenced by the high `theater_ratio` (0.70). While genuine competence exists in subsystems (e.g., engineering inspections), the 'ritualized' components consume resources without proportional benefit, contributing to a moderate `extractiveness` (0.45). `Suppression` (0.55) is necessary to maintain the illusion of full competence and to deflect criticism of the ritualized aspects. The `accessibility_collapse` is moderate (0.40) because while alternatives to ritualized drills exist, implementing them faces significant institutional resistance. The temporal measurements show a clear trend of rising `theater_ratio` and `base_extractiveness` over time, indicating a drift towards more performative and less functional operation, consistent with a Piton lifecycle.
 *
 * PERSPECTIVAL GAP:
 *   Emergency management agencies and drill contractors perceive the system as functional and necessary, justifying their roles and revenues. In contrast, frontline responders and critical evaluators experience the system's stratified nature directly, recognizing the gaps created by ritualization and the risks borne by the public. The public, largely excluded, experiences a general sense of security that may not align with actual readiness.
 *
 * DIRECTIONALITY LOGIC:
 *   Emergency management agencies and drill contractors are beneficiaries, as they derive institutional power, funding, or profit from the system's operation, including its ritualized parts. Public citizens and frontline responders are payers/victims, bearing the costs of inefficiency, risk, and operational gaps. Engineering inspection teams are beneficiaries of the overall funding, even if their specific work is functional. Critical evaluators are observers, structurally positioned to analyze but often excluded from influencing the system's core operations.
 *
 * MANDATROPHY ANALYSIS:
 *   This hybrid reading directly addresses mandatrophy. The 'ritualized' components of preparedness represent functions whose original mandate (e.g., effective training for real-world scenarios) has atrophied, but the activities persist due to institutional inertia and the benefits they provide to certain actors (e.g., contractors, agencies maintaining budgets). The high `theater_ratio` is a direct indicator of this mandatrophy, where performance has replaced genuine function. The system avoids being a Snare because the extraction is diffuse and the primary function hasn't entirely vanished, but it's not a Rope because the benefits are no longer symmetric and significant resources are consumed by performative elements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ritual_efficacy_ambiguity,
    'To what extent do the ''ritualized'' components (e.g., evacuation drills) still contribute to actual preparedness or public psychological resilience, even if their direct operational efficacy is low?',
    'Empirical studies on the long-term behavioral impacts of drills, public perception surveys, and comparative analysis with regions that have different preparedness approaches.',
    'If ritualized components are found to have significant indirect benefits (e.g., maintaining public awareness, fostering a sense of community resilience), the `theater_ratio` might be slightly lower, and the `extractiveness` could be re-evaluated as a necessary, albeit indirect, coordination cost. If not, the Piton classification is strongly reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_efficacy_ambiguity, empirical, 'Assessing the latent functions of ritualized preparedness activities.').

omega_variable(
    competence_erosion_threshold,
    'At what point does the erosion of competence in some subsystems begin to critically undermine the efficacy of even the genuinely competent subsystems?',
    'System-level modeling of interdependencies between preparedness components, and post-disaster forensic analysis of cascading failures.',
    'If a critical threshold is identified, the overall `accessibility_collapse` and `resistance` metrics for the entire system would need to be re-evaluated, potentially shifting the constraint towards a more severe classification (e.g., Snare) as the entire system becomes a liability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competence_erosion_threshold, empirical, 'Identifying the tipping point where ritualization compromises overall system integrity.').

omega_variable(
    kernel_reading_distinction,
    'Is this ''hybrid_reading'' a distinct structural constraint, or merely an intermediate state between the ''competence_reading'' and ''husk_reading''?',
    'Analysis of the persistence mechanisms: if the hybrid state is actively maintained by distinct institutional dynamics (e.g., specific beneficiaries of ritualization), it is a distinct constraint. If it''s a passive decay, it''s a transitional state.',
    'If it''s a distinct constraint, the Piton classification holds. If it''s a transitional state, the classification might shift towards the ''husk_reading'' (a more pronounced Piton or Snare) as decay progresses, or towards the ''competence_reading'' (Rope/Mountain) if active reforms reverse the trend.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Clarifying the structural distinctiveness of the hybrid preparedness state.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__hybrid_reading, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t1990, preparedness_persistence__hybrid_reading, theater_ratio, 1990, 0.4).
narrative_ontology:measurement(prep_tr_t1995, preparedness_persistence__hybrid_reading, theater_ratio, 1995, 0.5).
narrative_ontology:measurement(prep_tr_t2000, preparedness_persistence__hybrid_reading, theater_ratio, 2000, 0.6).
narrative_ontology:measurement(prep_tr_t2005, preparedness_persistence__hybrid_reading, theater_ratio, 2005, 0.65).
narrative_ontology:measurement(prep_tr_t2010, preparedness_persistence__hybrid_reading, theater_ratio, 2010, 0.68).
narrative_ontology:measurement(prep_tr_t2015, preparedness_persistence__hybrid_reading, theater_ratio, 2015, 0.69).
narrative_ontology:measurement(prep_tr_t2020, preparedness_persistence__hybrid_reading, theater_ratio, 2020, 0.7).
narrative_ontology:measurement(prep_tr_t2025, preparedness_persistence__hybrid_reading, theater_ratio, 2025, 0.7).

% Extraction over time
narrative_ontology:measurement(prep_be_t1990, preparedness_persistence__hybrid_reading, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(prep_be_t1995, preparedness_persistence__hybrid_reading, base_extractiveness, 1995, 0.35).
narrative_ontology:measurement(prep_be_t2000, preparedness_persistence__hybrid_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(prep_be_t2005, preparedness_persistence__hybrid_reading, base_extractiveness, 2005, 0.42).
narrative_ontology:measurement(prep_be_t2010, preparedness_persistence__hybrid_reading, base_extractiveness, 2010, 0.43).
narrative_ontology:measurement(prep_be_t2015, preparedness_persistence__hybrid_reading, base_extractiveness, 2015, 0.44).
narrative_ontology:measurement(prep_be_t2020, preparedness_persistence__hybrid_reading, base_extractiveness, 2020, 0.45).
narrative_ontology:measurement(prep_be_t2025, preparedness_persistence__hybrid_reading, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t1990, preparedness_persistence__hybrid_reading, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(prep_su_t1995, preparedness_persistence__hybrid_reading, suppression_requirement, 1995, 0.5).
narrative_ontology:measurement(prep_su_t2000, preparedness_persistence__hybrid_reading, suppression_requirement, 2000, 0.52).
narrative_ontology:measurement(prep_su_t2005, preparedness_persistence__hybrid_reading, suppression_requirement, 2005, 0.53).
narrative_ontology:measurement(prep_su_t2010, preparedness_persistence__hybrid_reading, suppression_requirement, 2010, 0.54).
narrative_ontology:measurement(prep_su_t2015, preparedness_persistence__hybrid_reading, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement(prep_su_t2020, preparedness_persistence__hybrid_reading, suppression_requirement, 2020, 0.55).
narrative_ontology:measurement(prep_su_t2025, preparedness_persistence__hybrid_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__hybrid_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
