% ============================================================================
% CONSTRAINT STORY: rotation_seven_black_soil
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_rotation_seven_black_soil, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
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
 * * constraint_id: rotation_seven_black_soil
 * human_readable: R7 Black Soil Toxicity
 * domain: biological/environmental
 * * SUMMARY:
 * The "black soil" in a specific sector of a space station contains a biological
 * toxin that causes irreversible kidney failure and death in humans exposed to it.
 * The constraint is the unchangeable biological effect of the toxin on the human
 * body, which dictates individual fates and institutional strategy.
 * * KEY AGENTS:
 * - Sector Seven Residents: Subjects (Powerless) who are exposed to the soil.
 * - Station Management: Beneficiary (Institutional) who manages the crisis via protocols.
 * - High Command: Auditor (Analytical) who views the toxicity as a fixed parameter for strategic planning.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(rotation_seven_black_soil, 0.95). % Mountain/Snare extraction >= 0.46. Extracts life itself.
domain_priors:suppression_score(rotation_seven_black_soil, 0.1).   % Structural property. The effects are physically evident, not suppressed.
domain_priors:theater_ratio(rotation_seven_black_soil, 0.1).       % Not performative; the effects are real.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(rotation_seven_black_soil, extractiveness, 0.95).
narrative_ontology:constraint_metric(rotation_seven_black_soil, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(rotation_seven_black_soil, theater_ratio, 0.1).

% Constraint self-claim (what does the constraint claim to be?)
% The institutional framing is that this is an unchangeable fact of nature.
narrative_ontology:constraint_claim(rotation_seven_black_soil, mountain).

% Structural property derivation hooks:
% The station's crisis management protocols create a coordination function.
% The victims are those exposed to the soil.
narrative_ontology:constraint_beneficiary(rotation_seven_black_soil, station_management).
narrative_ontology:constraint_victim(rotation_seven_black_soil, sector_seven_residents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For an exposed resident, the toxicity is a slow, inescapable trap that
% extracts their health and life.
constraint_indexing:constraint_classification(rotation_seven_black_soil, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For station management, the protocols for handling the crisis are a Rope.
% They coordinate the response, manage exposure, and maintain order, even if
% they cannot solve the underlying problem.
constraint_indexing:constraint_classification(rotation_seven_black_soil, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% From a strategic, long-term perspective, the toxin's biological effect is an
% immutable physical law—a Mountain that dictates policy like abandoning the sector.
constraint_indexing:constraint_classification(rotation_seven_black_soil, mountain,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rotation_seven_black_soil_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless, institutional, and analytical.
    constraint_indexing:constraint_classification(rotation_seven_black_soil, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rotation_seven_black_soil, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(rotation_seven_black_soil, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypeInstitutional \= TypeAnalytical,
    TypePowerless \= TypeAnalytical.

test(threshold_validation) :-
    % Verify the constraint meets the high-extraction threshold for a Snare.
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(rotation_seven_black_soil, ExtMetricName, E),
    E >= 0.46.

:- end_tests(rotation_seven_black_soil_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness is 0.95 because the constraint extracts life itself, the
 * ultimate resource. Suppression is low (0.1) as the effects (illness) are
 * physically obvious. Theater is also low (0.1) as the consequences are real, not performative.
 * The Perspectival Gap is stark:
 * - For the powerless individual, it's a Snare; their body is trapped by a fatal biological process.
 * - For the institutional manager, the *protocols* for dealing with the crisis are a Rope for coordinating a response.
 * - For the analytical strategist, the underlying biological fact is a Mountain—an unchangeable law of nature that forces large-scale strategic decisions.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * This is a classic Mandatrophic Mountain. The extreme extraction (0.95) is not the result
 * of a constructed rule but an immutable physical reality. The system resolves this by
 * correctly identifying the core constraint as a Mountain from the analytical view,
 * while acknowledging its manifestation as a Snare for those directly affected. The
 * institutional 'Rope' is a response to the Mountain, not the constraint itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_toxicity_reversibility,
    'Is the black soil toxicity truly an irreversible Mountain, or could a yet-undiscovered technology (e.g., advanced bioremediation, nanomedicine) reverse its effects?',
    'Long-term research into detoxification methods, medical breakthroughs in kidney regeneration, and geological analysis of soil composition.',
    'If irreversible, it remains a Mountain. If remediable, it transforms into a manageable problem, potentially a Tangled Rope (costly cure) or Rope (universal cure).',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(rotation_seven_black_soil, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for this high-extraction constraint.
% The perceived extractiveness increased as the long-term lethality became clear.
% Theater ratio remained low as the problem was always a physical reality.

% Theater ratio over time (metric_substitution detection):
narrative_ontology:measurement(rotation_seven_black_soil_tr_t0, rotation_seven_black_soil, theater_ratio, 0, 0.1).
narrative_ontology:measurement(rotation_seven_black_soil_tr_t5, rotation_seven_black_soil, theater_ratio, 5, 0.1).
narrative_ontology:measurement(rotation_seven_black_soil_tr_t10, rotation_seven_black_soil, theater_ratio, 10, 0.1).

% Extraction over time (extraction_accumulation detection):
narrative_ontology:measurement(rotation_seven_black_soil_ex_t0, rotation_seven_black_soil, base_extractiveness, 0, 0.80).
narrative_ontology:measurement(rotation_seven_black_soil_ex_t5, rotation_seven_black_soil, base_extractiveness, 5, 0.90).
narrative_ontology:measurement(rotation_seven_black_soil_ex_t10, rotation_seven_black_soil, base_extractiveness, 10, 0.95).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% This constraint is a biological fact, not a coordination mechanism, so
% Boltzmann data is not applicable. No network relationships are specified.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */