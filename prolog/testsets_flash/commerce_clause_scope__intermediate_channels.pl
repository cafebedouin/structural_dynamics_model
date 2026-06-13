% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__intermediate_channels
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__intermediate_channels, []).

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
 *   constraint_id: commerce_clause_scope__intermediate_channels
 *   human_readable: Commerce Clause Scope: Intermediate Channels Reading
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   This constraint represents the 'intermediate channels' reading of the
 *   Commerce Clause, which defines the scope of federal power to regulate
 *   interstate commerce. It acknowledges federal authority over channels,
 *   instrumentalities, and activities substantially affecting commerce, but
 *   introduces limiting principles to protect state sovereignty and
 *   non-economic local activity. This reading attempts to balance federal and
 *   state power, but its limiting principles (e.g., the economic/non-economic
 *   distinction) are often subject to judicial interpretation and can be seen
 *   as manipulable, leading to a contested and somewhat extractive outcome
 *   for conceptual coherence.
 *
 * KEY AGENTS:
 *   - federal_government: Agenda setter (institutional/generational) — benefits from ability to regulate national economic issues.
 *   - states_on_local_matters: Beneficiary (institutional/generational) — benefits from retained authority over non-economic local matters.
 *   - conceptual_coherence_of_commerce_power: Victim (analytical/civilizational) — suffers from the instability and manipulability of the limiting principles.
 *   - regulated_non_economic_actors: Payer (moderate/biographical) — bears the cost of federal regulation when their activities are deemed to affect interstate commerce, even if non-economic.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__intermediate_channels, 0.45).
domain_priors:suppression_score(commerce_clause_scope__intermediate_channels, 0.6).
domain_priors:theater_ratio(commerce_clause_scope__intermediate_channels, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, extractiveness, 0.45).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__intermediate_channels, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__intermediate_channels, "Commerce Clause Scope: Intermediate Channels Reading").
narrative_ontology:topic_domain(commerce_clause_scope__intermediate_channels, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_scope__intermediate_channels).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__intermediate_channels, '21152894-2a87-4e79-a921-90e346ed6498').
narrative_ontology:cs_kernel_codification('21152894-2a87-4e79-a921-90e346ed6498', fixed_text).
narrative_ontology:cs_authority_grounding('21152894-2a87-4e79-a921-90e346ed6498', lineage).
narrative_ontology:cs_interpretation_layer_present('21152894-2a87-4e79-a921-90e346ed6498').
narrative_ontology:cs_reading_relation('21152894-2a87-4e79-a921-90e346ed6498', commerce_clause_scope__narrow_originalist, coexists_with).
narrative_ontology:cs_reading_relation('21152894-2a87-4e79-a921-90e346ed6498', commerce_clause_scope__broad_effects_test, coexists_with).
narrative_ontology:cs_axiom('21152894-2a87-4e79-a921-90e346ed6498', foundational, federal_power_limited_by_enumerated_powers).
narrative_ontology:cs_axiom_status(federal_power_limited_by_enumerated_powers, holdable).
narrative_ontology:cs_axiom_grounding('21152894-2a87-4e79-a921-90e346ed6498', federal_power_limited_by_enumerated_powers, deontological).
narrative_ontology:cs_axiom('21152894-2a87-4e79-a921-90e346ed6498', foundational, states_retain_police_power_over_local_non_economic_activity).
narrative_ontology:cs_axiom_status(states_retain_police_power_over_local_non_economic_activity, holdable).
narrative_ontology:cs_axiom_grounding('21152894-2a87-4e79-a921-90e346ed6498', states_retain_police_power_over_local_non_economic_activity, deontological).
narrative_ontology:cs_reference_frame('21152894-2a87-4e79-a921-90e346ed6498', post_new_deal_federalism).
narrative_ontology:cs_drift_state('21152894-2a87-4e79-a921-90e346ed6498', contemporary_judicial_review, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('21152894-2a87-4e79-a921-90e346ed6498', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__intermediate_channels, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, federal_government).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, states_on_local_matters).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, conceptual_coherence_of_commerce_power).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, regulated_non_economic_actors).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__intermediate_channels, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(commerce_clause_scope__intermediate_channels, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__intermediate_channels_tests).
:- end_tests(commerce_clause_scope__intermediate_channels_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because while federal power is extensive, the limiting principles do impose some checks. Suppression (0.6) is present as federal authority is actively enforced, and states' ability to resist is constrained by judicial review. The theater ratio (0.2) is relatively low, as the principles are genuinely applied, but the conceptual instability of the 'economic/non-economic' distinction introduces some performative aspects in judicial reasoning. Accessibility collapse (0.4) is moderate; while federal power is broad, states retain some areas of exclusive authority. Resistance (0.3) is moderate, as states and regulated entities frequently challenge the application of federal power.
 *
 * PERSPECTIVAL GAP:
 *   The federal government perceives this as a necessary and legitimate exercise of power to address national issues, while states and regulated non-economic actors may view it as an overreach that erodes local control. The 'conceptual coherence' victim experiences the constraint as a constant struggle for clear boundaries, which the federal government may dismiss as mere legal complexity.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government is a primary beneficiary (d=0.0-0.1) as it gains broad regulatory authority. States, particularly regarding local non-economic matters, are also beneficiaries (d=0.1-0.2) as the limiting principles protect some of their autonomy. Conceptual coherence is a victim (d=0.9-1.0) due to the inherent tension and manipulability of the limiting principles. Regulated non-economic actors are payers (d=0.7-0.8) as they bear the direct costs of federal regulation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Tangled Rope because it genuinely coordinates federal and state powers to address national economic issues while simultaneously extracting from state autonomy and conceptual clarity through its ambiguous limiting principles. The active enforcement of federal regulations, coupled with the ongoing judicial interpretation, prevents it from becoming a Piton. Its coordination function (addressing national economic problems) is still live, but the extraction (from states and conceptual coherence) is a persistent feature, not a temporary support, ruling out Scaffold. It's not a Snare because there is a genuine, albeit imperfect, coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine intermediate reading of the Commerce Clause, or does it functionally collapse into either a broad effects test or a narrow originalist view?',
    'Analysis of future Supreme Court decisions: if the limiting principles are consistently applied to invalidate federal economic regulation, it leans narrow; if they are consistently circumvented, it leans broad. If the distinctions hold, the intermediate reading is stable.',
    'If it collapses to broad_effects_test, federal power is more extensive, and state autonomy is reduced. If it collapses to narrow_originalist, federal power is severely curtailed, and states gain more autonomy. If stable, the current balance is maintained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, empirical, 'This constraint is the ''intermediate_channels'' reading of the ''commerce_clause_scope'' kernel. Sibling readings are ''narrow_originalist'' and ''broad_effects_test''.').

omega_variable(
    economic_non_economic_distinction_stability,
    'Is the distinction between ''economic'' and ''non-economic'' activity a stable and predictable limiting principle, or is it inherently manipulable?',
    'Empirical study of lower court application and Supreme Court clarification over time: consistent application suggests stability; inconsistent or outcome-driven application suggests manipulability.',
    'If unstable, the limiting principles offer less protection to state autonomy and non-economic local activity, increasing federal power and extractiveness. If stable, the constraint functions as intended, balancing federal and state power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_non_economic_distinction_stability, conceptual, 'The stability of the ''economic'' vs ''non-economic'' distinction as a limiting principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__intermediate_channels, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, commerce_clause_scope__intermediate_channels, theater_ratio, 0, 0.25).
narrative_ontology:measurement(comm_tr_t10, commerce_clause_scope__intermediate_channels, theater_ratio, 10, 0.22).
narrative_ontology:measurement(comm_tr_t20, commerce_clause_scope__intermediate_channels, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, commerce_clause_scope__intermediate_channels, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(comm_be_t10, commerce_clause_scope__intermediate_channels, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(comm_be_t20, commerce_clause_scope__intermediate_channels, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, commerce_clause_scope__intermediate_channels, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(comm_su_t10, commerce_clause_scope__intermediate_channels, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(comm_su_t20, commerce_clause_scope__intermediate_channels, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__intermediate_channels, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, commerce_clause_scope__narrow_originalist).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, commerce_clause_scope__broad_effects_test).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Commerce Clause scope kernel. Its structural properties and classification are distinct from the 'narrow_originalist' and 'broad_effects_test' readings, which are modeled as separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
