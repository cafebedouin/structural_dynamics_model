% ============================================================================
% CONSTRAINT STORY: strait_coercion_2025
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_strait_coercion_2025, []).

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
 * * constraint_id: strait_coercion_2025
 * human_readable: Normalized Taiwan Strait Military Coercion
 * domain: political/military
 * * SUMMARY:
 * By 2025, Chinese military activity around Taiwan has decoupled from external
 * political triggers (signaling) and shifted to internal readiness cycles
 * and training schedules (preparation). Presence became near-continuous,
 * with lulls driven by weather or domestic holidays rather than diplomacy.
 * This normalization imposes constant security and psychological costs on Taiwan.
 * * KEY AGENTS:
 * - Taiwanese Citizenry: Subject (Powerless)
 * - PLA High Command: Beneficiary (Institutional)
 * - Strategic Analysts: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is high (0.55) as the activity imposes constant security costs
% and psychological pressure on Taiwan without a coordination benefit for them.
domain_priors:base_extractiveness(strait_coercion_2025, 0.55). % Mountain <= 0.15, Rope <= 0.15, Snare >= 0.46
domain_priors:suppression_score(strait_coercion_2025, 0.90).   % Structural property (raw, unscaled). Vanishingly rare lulls.
domain_priors:theater_ratio(strait_coercion_2025, 0.35).       % Piton detection (>= 0.70). Low value reflects shift from 'theater' to functional prep.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(strait_coercion_2025, extractiveness, 0.55).
narrative_ontology:constraint_metric(strait_coercion_2025, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(strait_coercion_2025, theater_ratio, 0.35).

% Constraint self-claim (what does the constraint claim to be?)
% The activity is framed as a necessary enforcement of sovereignty claims.
narrative_ontology:constraint_claim(strait_coercion_2025, tangled_rope).
narrative_ontology:human_readable(strait_coercion_2025, "Normalized Taiwan Strait Military Coercion").

% Binary flags
domain_priors:requires_active_enforcement(strait_coercion_2025). % Required for Tangled Rope

% Structural property derivation hooks:
% These are required for Tangled Rope classification.
narrative_ontology:constraint_beneficiary(strait_coercion_2025, pla_high_command).
narrative_ontology:constraint_victim(strait_coercion_2025, taiwanese_citizenry).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The constant, high-suppression military presence is an inescapable trap,
% draining resources and creating psychological pressure.
constraint_indexing:constraint_classification(strait_coercion_2025, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Internal training schedules and readiness cycles act as the primary
% coordination mechanism for joint operations and force projection.
constraint_indexing:constraint_classification(strait_coercion_2025, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the asymmetric extraction of security resources from Taiwan while
% acknowledging the genuine internal coordination function for the PLA.
constraint_indexing:constraint_classification(strait_coercion_2025, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: DIPLOMATIC ACTORS (SNARE)
% Traditional signaling is retroactively framed (opportunistic justification)
% to trap external actors into a reactive loop, forcing responses.
constraint_indexing:constraint_classification(strait_coercion_2025, snare,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(strait_coercion_2025_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(strait_coercion_2025, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(strait_coercion_2025, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_is_tangled_rope) :-
    % The analytical view must resolve the conflict as a Tangled Rope.
    constraint_indexing:constraint_classification(strait_coercion_2025, tangled_rope, context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    domain_priors:base_extractiveness(strait_coercion_2025, E),
    E >= 0.46. % Triggers Snare/Tangled logic.

:- end_tests(strait_coercion_2025_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.55) reflects the constant drain on Taiwan's defensive
 * readiness. The perspectival gap is significant: the institution (PLA)
 * sees a 'Rope' for internal training efficiency, while the subject (Taiwan)
 * experiences a 'Snare'—an inescapable, coercive pressure. The original
 * classification of 'Piton' for the subject was incorrect, as the theater_ratio
 * (0.35) is well below the 0.70 threshold; the constraint is highly functional
 * in its coercive purpose, not inert.
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * Labeling this as a 'Tangled Rope' from an analytical view correctly
 * acknowledges the internal coordination benefits for the PLA (training/testing)
 * while preventing the system from ignoring the aggressive extraction practiced
 * against Taiwan. This avoids misclassifying it as a pure Snare or pure Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_strait_coercion_2025,
    'Is the signaling purely retroactive (opportunistic) or are some spikes still genuine tactical responses?',
    'Correlation analysis of internal PLA command directives vs external triggers over a 5-year period.',
    'Retroactive = Pure Readiness Constraint; Tactical = Hybrid Signaling Constraint.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing and lifecycle drift analysis
narrative_ontology:interval(strait_coercion_2025, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the constraint's evolution from a signaling tool
% (higher theater) to a normalized readiness cycle (lower theater, higher extraction).
% This is required as base_extractiveness > 0.46.

% Theater ratio over time (metric_substitution: signaling -> function):
narrative_ontology:measurement(sc25_tr_t0, strait_coercion_2025, theater_ratio, 0, 0.65).
narrative_ontology:measurement(sc25_tr_t5, strait_coercion_2025, theater_ratio, 5, 0.50).
narrative_ontology:measurement(sc25_tr_t10, strait_coercion_2025, theater_ratio, 10, 0.35).

% Extraction over time (extraction_accumulation as presence intensifies):
narrative_ontology:measurement(sc25_ex_t0, strait_coercion_2025, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(sc25_ex_t5, strait_coercion_2025, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(sc25_ex_t10, strait_coercion_2025, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The internal PLA coordination function is a form of enforcement mechanism
% for readiness and sovereignty claims.
narrative_ontology:coordination_type(strait_coercion_2025, enforcement_mechanism).

% This military constraint has direct structural influence on economic and
% technological constraints, such as the global semiconductor supply chain.
narrative_ontology:affects_constraint(strait_coercion_2025, global_semiconductor_supply).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */