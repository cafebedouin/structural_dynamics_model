% ============================================================================
% CONSTRAINT STORY: epistemic_overload_collapse
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-20
% ============================================================================

:- module(constraint_epistemic_overload_collapse, []).

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
 * * constraint_id: epistemic_overload_collapse
 * human_readable: The Signal-Drowning Vortex
 * domain: cognitive/informational/technological
 * * SUMMARY:
 * A scenario where the volume, velocity, and contradictory nature of
 * available information (claimed as a Rope) exceed the biological and cognitive limits
 * of the human subject to process it. This coordination tool for "total
 * information awareness" becomes a "Snare" as the subject's primary
 * truth-seeking agency is liquidated, trapping them in a terminal state
 * of apathy, paralysis, or reliance on low-fidelity heuristics, effectively
 * collapsing their ability to participate in meaningful collective action.
 *
 * * KEY AGENTS:
 * - Information Citizen: Subject (Powerless)
 * - Attention/Data Aggregator: Beneficiary (Institutional)
 * - Cognitive Integrity Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.89) reflects the liquidation of the subject's
% primary cognitive autonomy to feed the institutional need for constant data consumption.
domain_priors:base_extractiveness(epistemic_overload_collapse, 0.89).
domain_priors:suppression_score(epistemic_overload_collapse, 0.81). % Silence or "low-data" environments are suppressed by social and economic mandates for connectivity.
domain_priors:theater_ratio(epistemic_overload_collapse, 0.92).    % Extreme theater: "Fact-Checkers" and "Context Notes" that add more data-load while signaling truth.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(epistemic_overload_collapse, extractiveness, 0.89).
narrative_ontology:constraint_metric(epistemic_overload_collapse, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(epistemic_overload_collapse, theater_ratio, 0.92).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(epistemic_overload_collapse, tangled_rope).
narrative_ontology:human_readable(epistemic_overload_collapse, "The Signal-Drowning Vortex").

% Binary flags
domain_priors:requires_active_enforcement(epistemic_overload_collapse). % Required for Tangled Rope

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(epistemic_overload_collapse, attention_data_aggregator).
narrative_ontology:constraint_victim(epistemic_overload_collapse, information_citizen).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The citizen is trapped: they must use the information flood to remain
% socially and economically legible, but doing so liquidates their capacity for discernment.
constraint_indexing:constraint_classification(epistemic_overload_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The aggregator views the flood as a Rope—the essential coordination
% substrate for achieving a "Fully Transparent" global society.
constraint_indexing:constraint_classification(epistemic_overload_collapse, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.89) and active enforcement masking as essential coordination.
constraint_indexing:constraint_classification(epistemic_overload_collapse, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.92) > 0.70 triggers Piton: the "Information Literacy"
% seminar is an inertial spike; it performatively addresses the overload while 0.89 extraction continues.
constraint_indexing:constraint_classification(epistemic_overload_collapse, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(epistemic_overload_collapse, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_overload_collapse_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless citizen vs Rope for the institutional aggregator.
    constraint_indexing:constraint_classification(epistemic_overload_collapse, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_overload_collapse, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_overload_collapse, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(piton_trigger) :-
    % Ensure extreme theater (0.92) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(epistemic_overload_collapse, piton,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties) :-
    % Verify that all three required properties for a Tangled Rope are declared.
    narrative_ontology:constraint_beneficiary(epistemic_overload_collapse, _),
    narrative_ontology:constraint_victim(epistemic_overload_collapse, _),
    domain_priors:requires_active_enforcement(epistemic_overload_collapse).

:- end_tests(epistemic_overload_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.89) reflects a "Mandatrophy" state where the
 * "coordination" benefit of total information access is achieved by
 * liquidating the subject's primary capacity for epistemic agency. The high
 * theater ratio (0.92) indicates that performative "solutions" like fact-checkers
 * and media literacy programs now consume more resources than they contribute
 * to solving the underlying problem, instead becoming part of the noise.
 *
 * * PERSPECTIVAL GAP:
 * The Information Citizen feels a Snare because their cognitive environment is
 * fundamentally hostile to truth-seeking. The Aggregator sees a Rope because
 * the total capture of data coordinates a perfectly legible and monetizeable
 * global attention market. The Analytical Observer sees a Tangled Rope,
 * recognizing both the coordination claim and the severe asymmetric extraction.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Tangled Rope classification. The system correctly identifies
 * that a genuine coordination function (information dissemination) exists but has
 * been coupled with a predatory extractive function (liquidation of cognitive
 * agency). The Piton classification further refines this by showing that attempts
 * to fix the problem have themselves become inert and theatrical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_cognitive_saturation,
    'Can AI "Information Filters" restore the Rope, or is overload an entropic "Mountain" (Snare vs Mountain)?',
    'Tracking the belief-delta between subjects with human-curated vs AI-curated news feeds over 24 months.',
    'If belief stabilizes: Snare of current technique. If it diverges: Mountain of Cognitive Limits.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(epistemic_overload_collapse, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The constraint began as a genuine coordination tool (low extraction, low theater)
% but degraded over time as the volume of information became a vector for
% extracting attention and agency.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(eoc_tr_t0, epistemic_overload_collapse, theater_ratio, 0, 0.25).
narrative_ontology:measurement(eoc_tr_t5, epistemic_overload_collapse, theater_ratio, 5, 0.65).
narrative_ontology:measurement(eoc_tr_t10, epistemic_overload_collapse, theater_ratio, 10, 0.92).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(eoc_ex_t0, epistemic_overload_collapse, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(eoc_ex_t5, epistemic_overload_collapse, base_extractiveness, 5, 0.77).
narrative_ontology:measurement(eoc_ex_t10, epistemic_overload_collapse, base_extractiveness, 10, 0.89).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The constraint operates as a foundational layer for information exchange.
narrative_ontology:coordination_type(epistemic_overload_collapse, global_infrastructure).

% Network relationships (structural influence edges)
% The collapse of epistemic agency directly degrades the integrity of civic discourse.
narrative_ontology:affects_constraint(epistemic_overload_collapse, civic_discourse_integrity).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */