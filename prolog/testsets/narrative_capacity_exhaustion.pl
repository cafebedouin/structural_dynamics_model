% ============================================================================
% CONSTRAINT STORY: narrative_capacity_exhaustion
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_narrative_capacity_exhaustion, []).

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
 * * constraint_id: narrative_capacity_exhaustion
 * human_readable: The Storytelling Burnout
 * domain: social/technological
 * * SUMMARY:
 * A scenario where the "Rope" of digital content creation and storytelling
 * becomes a competitive treadmill that requires more narrative output than
 * the subject has lived experience to support. This coordination tool for
 * social presence becomes a "Snare" as the subject's authentic agency is
 * liquidated to satisfy the constant demand for "new content," trapping
 * the user in a territory of performative exhaustion where "meaning" is
 * sacrificed for "frequency."
 * * KEY AGENTS:
 * - Content Creator: Subject (Powerless)
 * - Engagement Algorithm: Beneficiary (Institutional)
 * - Cultural Saturation Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.88) reflects the total siphoning of the subject's
% creative surplus to maintain the institutional "engagement" metric.
domain_priors:base_extractiveness(narrative_capacity_exhaustion, 0.88).
domain_priors:suppression_score(narrative_capacity_exhaustion, 0.77).   % Offline or "slow" creativity is suppressed by algorithmically-induced invisibility.
domain_priors:theater_ratio(narrative_capacity_exhaustion, 0.91).       % Extreme theater: "Creator Wellness" grants masking the structural extraction of burnout.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(narrative_capacity_exhaustion, extractiveness, 0.88).
narrative_ontology:constraint_metric(narrative_capacity_exhaustion, suppression_requirement, 0.77).
narrative_ontology:constraint_metric(narrative_capacity_exhaustion, theater_ratio, 0.91).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(narrative_capacity_exhaustion, tangled_rope).

% Binary flags and structural properties for Tangled Rope
domain_priors:requires_active_enforcement(narrative_capacity_exhaustion). % Algorithmic downranking is active enforcement.
narrative_ontology:constraint_beneficiary(narrative_capacity_exhaustion, platform_algorithms).
narrative_ontology:constraint_victim(narrative_capacity_exhaustion, content_creators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The creator is trapped: they must keep telling stories to remain relevant,
% but the pace liquidates their capacity for actual thought.
constraint_indexing:constraint_classification(narrative_capacity_exhaustion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The platform views the exhaustion as a Rope—the essential coordination
% substrate for maintaining a 24/7 global stream of "fresh" data.
constraint_indexing:constraint_classification(narrative_capacity_exhaustion, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the combination of a genuine coordination function (for the platform)
% with severe asymmetric extraction (from the creator), requiring active enforcement.
constraint_indexing:constraint_classification(narrative_capacity_exhaustion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.91) > 0.70 triggers Piton: the "Authenticity Guide"
% is an inertial spike; it signals creative freedom while 0.88 extraction occurs.
constraint_indexing:constraint_classification(narrative_capacity_exhaustion, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(narrative_exhaustion_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless creator vs Rope for the institutional platform.
    constraint_indexing:constraint_classification(narrative_capacity_exhaustion, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(narrative_capacity_exhaustion, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(narrative_capacity_exhaustion, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(piton_trigger) :-
    % Ensure extreme theater (0.91) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(narrative_capacity_exhaustion, piton,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties) :-
    % Verify all three required properties for Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(narrative_capacity_exhaustion, _),
    narrative_ontology:constraint_victim(narrative_capacity_exhaustion, _),
    domain_priors:requires_active_enforcement(narrative_capacity_exhaustion).

:- end_tests(narrative_exhaustion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.88) reflects a state where the "coordination"
 * benefit of a global creator economy is achieved by liquidating the subject's
 * primary creative and psychological agency. The suppression score (0.77)
 * represents the algorithmic penalty for failing to meet the platform's
 * cadence, effectively making alternative, slower forms of creativity invisible.
 * The extreme theater ratio (0.91) is for corporate "creator wellness"
 * initiatives that perform concern while the underlying extractive model intensifies.
 *
 * * PERSPECTIVAL GAP:
 * The Content Creator feels a Snare because they have become a "content machine"
 * with no lived reality to report. The Algorithm-as-Institution sees a Rope
 * because the exhaustion coordinates a perfectly steady and predictable data feed.
 * The Analytical Observer sees a Tangled Rope, recognizing both the coordination
 * function and the severe, enforced extraction.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * Mandatrophy is the risk of misclassifying a system with a coordination function
 * as a pure Snare, thereby missing the pathology. This is resolved here by the
 * Tangled Rope classification. It correctly identifies that the system isn't
 * just pure extraction; it has a genuine (if parasitic) coordination function
 * for the beneficiary. This nuance is critical for understanding why such systems
 * persist: they are not merely oppressive but also functional for a powerful index.
 * The Piton classification further resolves this by showing how parts of the
 * system have decayed into pure theater, masking the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_synthetic_narrative_ceiling,
    'Is the demand for human-generated narrative a fundamental limit (Mountain), or can AI-generated stories fully substitute it, revealing the current system as a remediable Snare?',
    'Tracking the engagement half-life of 100% synthetic vs 100% human-original content threads over a civilizational time horizon.',
    'If engagement holds: Snare of current technique. If it drops: Mountain of Human Meaning.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(narrative_capacity_exhaustion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint evolved from a genuine coordination tool into an extractive
% treadmill. The measurements model this degradation over the interval.
%
% Theater ratio over time (metric_substitution drift):
narrative_ontology:measurement(nce_tr_t0, narrative_capacity_exhaustion, theater_ratio, 0, 0.20).
narrative_ontology:measurement(nce_tr_t5, narrative_capacity_exhaustion, theater_ratio, 5, 0.60).
narrative_ontology:measurement(nce_tr_t10, narrative_capacity_exhaustion, theater_ratio, 10, 0.91).

% Extraction over time (extraction_accumulation drift):
narrative_ontology:measurement(nce_ex_t0, narrative_capacity_exhaustion, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(nce_ex_t5, narrative_capacity_exhaustion, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(nce_ex_t10, narrative_capacity_exhaustion, base_extractiveness, 10, 0.88).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The constraint functions as a form of global infrastructure for attention allocation.
narrative_ontology:coordination_type(narrative_capacity_exhaustion, global_infrastructure).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */