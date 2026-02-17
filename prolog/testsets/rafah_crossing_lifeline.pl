% ============================================================================
% CONSTRAINT STORY: rafah_crossing_lifeline
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_rafah_crossing_lifeline, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: rafah_crossing_lifeline
 * human_readable: The Rafah Medical Bottleneck
 * domain: political/geopolitical
 * * SUMMARY:
 * The Rafah crossing serves as a "literal lifeline" for Palestinians needing 
 * medical treatment. Under a hypothetical 2026 peace plan, it has reopened as a 
 * highly restricted, multi-layered security bottleneck with a strict daily
 * throughput limit. It is a temporary measure tied to demilitarization milestones.
 * * KEY AGENTS:
 * - The Patient: Subject (Powerless) - Individuals like Mohammed Mahdi seeking 
 * urgent medical exit from a pool of thousands.
 * - The Oversight Committee: Beneficiary (Institutional) - A multi-entity security 
 * body (e.g., Israel, Egypt, EU, PA) managing the crossing.
 * - The UN Auditor: Auditor (Analytical) - Tracking the 18,500 people needing 
 * medical evacuation versus the 50-person daily cap.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is low (not a rent-seeking toll), but Suppression is extreme due to
% the severe throughput limit and security checks.
domain_priors:base_extractiveness(rafah_crossing_lifeline, 0.15).
domain_priors:suppression_score(rafah_crossing_lifeline, 0.95).
domain_priors:theater_ratio(rafah_crossing_lifeline, 0.40).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(rafah_crossing_lifeline, extractiveness, 0.15).
narrative_ontology:constraint_metric(rafah_crossing_lifeline, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(rafah_crossing_lifeline, theater_ratio, 0.40).

% Constraint self-claim: It is presented as a necessary coordination mechanism.
narrative_ontology:constraint_claim(rafah_crossing_lifeline, rope).
narrative_ontology:human_readable(rafah_crossing_lifeline, "The Rafah Medical Bottleneck").

% Binary flags
domain_priors:requires_active_enforcement(rafah_crossing_lifeline).
narrative_ontology:has_sunset_clause(rafah_crossing_lifeline). % Tied to peace plan milestones

% Structural property derivation hooks:
% has_coordination_function/1 is derived from constraint_beneficiary/2
narrative_ontology:constraint_beneficiary(rafah_crossing_lifeline, successful_medical_evacuees).
narrative_ontology:constraint_beneficiary(rafah_crossing_lifeline, regional_security_actors).
narrative_ontology:constraint_victim(rafah_crossing_lifeline, waitlisted_patients).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE PATIENT (MOUNTAIN)
% With extreme suppression (S=0.95) and low extraction, the bottleneck is
% experienced as an immovable, natural-law-like barrier.
constraint_indexing:constraint_classification(rafah_crossing_lifeline, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE OVERSIGHT COMMITTEE (SCAFFOLD)
% Viewed by authorities as a temporary, high-security bridge to demilitarization,
% a necessary scaffold to support a fragile peace process.
constraint_indexing:constraint_classification(rafah_crossing_lifeline, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (ROPE)
% From a global, analytical view, the low extraction (χ = 0.15 * 1.15 * 1.2 = 0.207)
% highlights the coordination function (enabling medical evacuation, however limited)
% over the intense, localized suppression.
constraint_indexing:constraint_classification(rafah_crossing_lifeline, rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rafah_crossing_lifeline_tests).

test(perspectival_gap) :-
    % Verify the gap between the patient (Mountain) and the committee (Scaffold).
    constraint_indexing:constraint_classification(rafah_crossing_lifeline, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rafah_crossing_lifeline, scaffold, context(agent_power(institutional), _, _, _)).

test(scaffold_validation) :-
    % Verify it is classified as a Scaffold and has the required sunset clause.
    constraint_indexing:constraint_classification(rafah_crossing_lifeline, scaffold, _),
    narrative_ontology:has_sunset_clause(rafah_crossing_lifeline).

test(suppression_intensity) :-
    domain_priors:suppression_score(rafah_crossing_lifeline, S),
    S > 0.90. % Reflects the multi-layered security bottleneck.

:- end_tests(rafah_crossing_lifeline_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Perspectival Gap is extreme, driven by the disparity between the constraint's
 * function and its throughput. To a patient with a life-threatening condition,
 * the 50-person daily cap makes the crossing an effective Mountain; it is an
 * immovable object. To the peace plan architects, it is a Scaffold—a temporary,
 * high-friction necessity to enable a political transition. The analytical
 * observer sees a low-extraction Rope, focusing on the coordination function
 * itself, abstracted from the human cost of its inefficiency.
 *
 * * MANDATROPHY ANALYSIS:
 * This is not a Tangled Rope, so Mandatrophy is not the primary risk. However,
 * the Scaffold classification correctly prevents mislabeling. It acknowledges
 * the coordination function (unlike a Snare) but also its temporary, constructed
 * nature (unlike a Mountain), contingent on the sunset clause being honored.
 * If the sunset clause is ignored, it risks degrading into a Piton or Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_rafah_demilitarization,
    'Will the political actors meet the demilitarization milestones required to trigger the sunset clause?',
    'Direct verification by the designated European border-monitoring team.',
    'If False: The Scaffold collapses, likely reverting to a closed border (Mountain/Snare). If True: The Scaffold is dismantled as planned.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rafah_crossing_lifeline, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio: Rises as political posturing and bureaucratic friction
% begin to overshadow the humanitarian function.
narrative_ontology:measurement(rafah_tr_t0, rafah_crossing_lifeline, theater_ratio, 0, 0.10).
narrative_ontology:measurement(rafah_tr_t5, rafah_crossing_lifeline, theater_ratio, 5, 0.25).
narrative_ontology:measurement(rafah_tr_t10, rafah_crossing_lifeline, theater_ratio, 10, 0.40).

% Extraction: Remains low and stable, as the primary goal is security and
% control, not rent-seeking.
narrative_ontology:measurement(rafah_ex_t0, rafah_crossing_lifeline, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(rafah_ex_t5, rafah_crossing_lifeline, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(rafah_ex_t10, rafah_crossing_lifeline, base_extractiveness, 10, 0.15).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type: The crossing allocates a scarce resource (exit permits).
narrative_ontology:coordination_type(rafah_crossing_lifeline, resource_allocation).

% Network relationships: The crossing's existence is contingent on a broader
% political framework. Its failure would impact the framework's viability.
narrative_ontology:affects_constraint(rafah_crossing_lifeline, gaza_peace_plan_2026).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */