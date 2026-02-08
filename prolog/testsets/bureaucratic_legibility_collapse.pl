% ============================================================================
% CONSTRAINT STORY: bureaucratic_legibility_collapse
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-16
% ============================================================================

:- module(constraint_bureaucratic_legibility_collapse, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: bureaucratic_legibility_collapse
 * human_readable: The Administrative Whiteout
 * domain: political/organizational/informational
 * * SUMMARY:
 * A scenario where the metrics used by an institution to "see" and manage its
 * domain become so decoupled from reality that the institution's actions
 * produce the opposite of their intended effects. This "Rope" for
 * large-scale governance becomes a "Snare" as the subject's reality is
 * erased by the system's map, liquidating their ability to provide
 * meaningful feedback or exit the failing administrative logic.
 * * KEY AGENTS:
 * - Field Practitioner: Subject (Powerless)
 * - Central Planning Bureau: Beneficiary (Institutional)
 * - Information Systems Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.86) as the collapse of legibility siphons the subject's
% real-world effort into the maintenance of non-functional reporting
% structures that no longer track the actual territory.
domain_priors:base_extractiveness(bureaucratic_legibility_collapse, 0.86).
domain_priors:suppression_score(bureaucratic_legibility_collapse, 0.74).
domain_priors:theater_ratio(bureaucratic_legibility_collapse, 0.92). % Extreme theater: meticulous metrics masking zero situational awareness.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(bureaucratic_legibility_collapse, extractiveness, 0.86).
narrative_ontology:constraint_metric(bureaucratic_legibility_collapse, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(bureaucratic_legibility_collapse, theater_ratio, 0.92).

% Constraint self-claim: The institution claims this is a necessary coordination tool.
narrative_ontology:constraint_claim(bureaucratic_legibility_collapse, tangled_rope).

% Binary flags and structural properties for Tangled Rope classification.
domain_priors:requires_active_enforcement(bureaucratic_legibility_collapse).
narrative_ontology:constraint_beneficiary(bureaucratic_legibility_collapse, central_planning_bureau).
narrative_ontology:constraint_victim(bureaucratic_legibility_collapse, field_practitioners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The subject is trapped: their actual problems are illegible to the
% system, and they are punished for deviations from the "official" (false) data.
constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views the metrics as a Rope—the only way to coordinate
% behavior across a vast and diverse population from a centralized point.
constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the combination of a coordination function (beneficiary exists),
% asymmetric extraction (victim exists), and active enforcement. The high
% extraction and suppression confirm the Tangled Rope classification.
constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bureaucratic_legibility_collapse_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional planner.
    constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties) :-
    % Verify that all three required properties for Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(bureaucratic_legibility_collapse, _), % derives has_coordination_function
    narrative_ontology:constraint_victim(bureaucratic_legibility_collapse, _),     % derives has_asymmetric_extraction
    domain_priors:requires_active_enforcement(bureaucratic_legibility_collapse).

test(extraction_mandatrophy) :-
    % Ensure extraction (0.86) triggers mandatory v3.4 resolution logic.
    domain_priors:base_extractiveness(bureaucratic_legibility_collapse, E),
    E > 0.70.

:- end_tests(bureaucratic_legibility_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint models a system suffering from severe Goodhart Drift. The
 * extraction score (0.86) is high because the effort required to maintain the
 * false reality of the metrics is immense, directly siphoning resources and
 * agency from those on the ground. The theater ratio (0.92) is also extremely
 * high, indicating that the system's activities are almost entirely performative.
 *
 * While the high theater ratio might suggest a Piton, the extremely high
 * base extractiveness (0.86) makes that classification invalid (Piton requires ε <= 0.10).
 * Instead, the system is best classified as a Tangled Rope from an analytical
 * perspective. It retains the structure of a coordination mechanism (it has
 * beneficiaries and a coordination claim) but has become pathologically
 * extractive and coercive. The high theater ratio is a symptom of its dysfunction,
 * not a reclassification.
 *
 * * PERSPECTIVAL GAP:
 * The Field Practitioner experiences a Snare because they must lie on reports
 * to make their work "count," and are punished for non-compliance. The Central
 * Bureau sees a Rope because the aggregated (false) metrics are the only thing
 * that allows for the coordination of large-scale budgets and personnel.
 *
 * * [RESOLVED MANDATROPHY]:
 * The Mandatrophy is resolved by the Tangled Rope classification. This prevents
 * the system from being misclassified as a pure Snare (which would ignore its
 * coordination origins and structure) or a Piton (which would ignore the massive,
 * ongoing extraction). It correctly identifies the hybrid nature of a failed
 * coordination system that has become parasitic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_reality_reconnection,
    'Can decentralized feedback loops restore legibility, or is hierarchy an irreducible barrier (Snare vs Mountain)?',
    'Tracking the delta between local practitioner reports and central bureau metrics after a policy shock.',
    'If delta shrinks: Tangled Rope of current design. If delta grows: Mountain of Informational Entropy.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(bureaucratic_legibility_collapse, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint models a system that degraded over time, starting as a
% functional coordination mechanism and collapsing into a performative,
% extractive state.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(blc_tr_t0, bureaucratic_legibility_collapse, theater_ratio, 0, 0.15).
narrative_ontology:measurement(blc_tr_t5, bureaucratic_legibility_collapse, theater_ratio, 5, 0.55).
narrative_ontology:measurement(blc_tr_t10, bureaucratic_legibility_collapse, theater_ratio, 10, 0.92).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(blc_ex_t0, bureaucratic_legibility_collapse, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(blc_ex_t5, bureaucratic_legibility_collapse, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(blc_ex_t10, bureaucratic_legibility_collapse, base_extractiveness, 10, 0.86).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type: The metrics are used to allocate budgets and personnel.
narrative_ontology:coordination_type(bureaucratic_legibility_collapse, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */