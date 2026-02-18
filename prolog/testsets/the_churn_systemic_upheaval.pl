% ============================================================================
% CONSTRAINT STORY: the_churn_systemic_upheaval
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_the_churn_systemic_upheaval, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: the_churn_systemic_upheaval
 * human_readable: The Churn (Systemic Collapse and Rebirth)
 * domain: political/social/economic
 * * SUMMARY:
 * "The Churn" represents a period of systemic instability where established social and political rules dissolve and reconfigure. It is characterized by high mortality for those caught within it, rendering individual effort or identity "meaningless" in the face of macro-scale structural shifts. It is described as an inevitable, recurring process of systemic self-destruction and renewal.
 * * KEY AGENTS:
 * - The Veteran Survivor (Amos): Subject who accepts the churn as a natural, inevitable law.
 * - The Disposable Subject (Spy): A "loose end" liquidated by the shifting rules.
 * - The Systemic Architect (Baltimore Boss): Beneficiary who defines the "game" and leverages the transition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(the_churn_systemic_upheaval, 0.9). % Rationale: Maximally extractive; it consumes the lives of those "caught up" in it.
domain_priors:suppression_score(the_churn_systemic_upheaval, 0.8).   % Rationale: The previous "rules of the game" are torn down, suppressing any alternative stable reality.
domain_priors:theater_ratio(the_churn_systemic_upheaval, 0.1).       % Rationale: The Churn is brutally functional, not performative.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(the_churn_systemic_upheaval, extractiveness, 0.9).
narrative_ontology:constraint_metric(the_churn_systemic_upheaval, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(the_churn_systemic_upheaval, theater_ratio, 0.1).

% Constraint self-claim (what does the constraint claim to be?)
% Rationale: It is presented as an inevitable force of nature ("Like water's wet. Sky's up").
narrative_ontology:constraint_claim(the_churn_systemic_upheaval, snare).
narrative_ontology:human_readable(the_churn_systemic_upheaval, "The Churn (Systemic Collapse and Rebirth)").
narrative_ontology:topic_domain(the_churn_systemic_upheaval, "political/social/economic").

% Structural property derivation hooks:
% The beneficiary is the new system that emerges, and the victims are those consumed by the process.
narrative_ontology:constraint_beneficiary(the_churn_systemic_upheaval, the_new_system).
narrative_ontology:constraint_victim(the_churn_systemic_upheaval, liquidated_individuals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE DISPOSABLE SUBJECT (SNARE)
% For the victim, the churn is a coercive trap where their death is predetermined.
constraint_indexing:constraint_classification(the_churn_systemic_upheaval, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE SYSTEMIC ARCHITECT (ROPE)
% For the institutional player, the churn is a functional, albeit violent,
% mechanism for clearing out "loose ends" and building "somethin' new".
constraint_indexing:constraint_classification(the_churn_systemic_upheaval, rope,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE VETERAN SURVIVOR (MOUNTAIN)
% For the survivor, the churn is an unchangeable natural law comparable to physics.
constraint_indexing:constraint_classification(the_churn_systemic_upheaval, mountain,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (SNARE)
% The metrics confirm a high-extraction, high-suppression system.
% χ = 0.9 * 1.15 * 1.2 = 1.242. This is a clear Snare.
constraint_indexing:constraint_classification(the_churn_systemic_upheaval, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(the_churn_systemic_upheaval_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless, institutional, and moderate.
    constraint_indexing:constraint_classification(the_churn_systemic_upheaval, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(the_churn_systemic_upheaval, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(the_churn_systemic_upheaval, mountain, context(agent_power(moderate), _, _, _)).

test(extraction_threshold) :-
    % Extraction is high (0.9), confirming the "liquidation" aspect and Snare classification.
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(the_churn_systemic_upheaval, ExtMetricName, E),
    E > 0.46.

:- end_tests(the_churn_systemic_upheaval_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Churn is a classic example of a constraint whose classification depends entirely on one's
 * position relative to it. For those being eliminated (powerless), it is a lethal Snare. For those
 * orchestrating or benefiting from the new order (institutional), it is a Rope for social reorganization.
 * For those who survive it through luck and skill but without power (moderate), it appears as an
 * impersonal, unchangeable Mountain. The base extractiveness is set to 0.9 to reflect the literal
 * extraction of life from its victims.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The system avoids misclassifying this as a pure Snare by acknowledging the perspectival shifts.
 * While analytically a Snare due to its extreme extraction, the institutional 'Rope' view and the
 * survivor's 'Mountain' view are critical to understanding its stability and function as a
 * mechanism of systemic change, preventing a simplistic "pure evil" classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_the_churn_intent,
    "Is the liquidation of 'loose ends' a byproduct of structural collapse (Mountain) or an intentional predatory strategy by architects (Snare)?",
    "Audit of architect resource preservation vs. 'loose end' survival rates during historical churn events.",
    "If intent exists, it confirms the Snare classification as primary. If it's a byproduct, the Mountain classification gains weight.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(the_churn_systemic_upheaval, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The Churn is modeled as a rapid, intense event. Extraction begins high and intensifies
% to its peak, while theater remains minimal throughout its functional, brutal lifecycle.
% This data is required because base_extractiveness (0.9) > 0.46.

% Theater ratio over time (consistently low):
narrative_ontology:measurement(the_churn_tr_t0, the_churn_systemic_upheaval, theater_ratio, 0, 0.1).
narrative_ontology:measurement(the_churn_tr_t5, the_churn_systemic_upheaval, theater_ratio, 5, 0.1).
narrative_ontology:measurement(the_churn_tr_t10, the_churn_systemic_upheaval, theater_ratio, 10, 0.1).

% Extraction over time (starts high and intensifies):
narrative_ontology:measurement(the_churn_ex_t0, the_churn_systemic_upheaval, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(the_churn_ex_t5, the_churn_systemic_upheaval, base_extractiveness, 5, 0.85).
narrative_ontology:measurement(the_churn_ex_t10, the_churn_systemic_upheaval, base_extractiveness, 10, 0.9).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The Churn is a destructive/reconstructive force, not a stable coordination mechanism.
% Therefore, coordination_type is not applicable. No network relationships are defined in the source material.
% narrative_ontology:coordination_type(the_churn_systemic_upheaval, enforcement_mechanism).
% narrative_ontology:affects_constraint(the_churn_systemic_upheaval, other_constraint_id).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */