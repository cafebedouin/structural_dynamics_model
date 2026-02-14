% ============================================================================
% CONSTRAINT STORY: utopia_apocalypse_fragility
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_utopia_apocalypse_fragility, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: utopia_apocalypse_fragility
 * human_readable: The Utopia-Apocalypse Cliff-Edge
 * domain: social/political/philosophical
 * * SUMMARY:
 * This constraint describes the inherent fragility of social order (Utopia), which rests on
 * a "cliff-edge" and can transition into Apocalypse through slight shifts in structure,
 * personnel, or culture. It posits that life is a stochastic migration
 * from safety to catastrophe. The maintenance of "Utopia" requires active enforcement of
 * norms and structures to prevent this slide.
 * * KEY AGENTS:
 * - The Utopian Strategist: Institutional rule-shaper using coordination and science to manage the cliff.
 * - The Moderate Citizen: A citizen who benefits from Utopia but is vulnerable to the cliff's extraction.
 * - The Victim of Calamity: The individual pushed from safety into horror by systemic shifts.
 * - The Existential Realist: The observer who recognizes the secondary nature of truth to stories.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(utopia_apocalypse_fragility, 0.7). % Snare extraction >= 0.46
domain_priors:suppression_score(utopia_apocalypse_fragility, 0.6).
domain_priors:theater_ratio(utopia_apocalypse_fragility, 0.1).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(utopia_apocalypse_fragility, extractiveness, 0.7).
narrative_ontology:constraint_metric(utopia_apocalypse_fragility, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(utopia_apocalypse_fragility, theater_ratio, 0.1).

% Constraint self-claim (what does the constraint claim to be?)
% Analytically, the stochastic nature of existence is claimed as an unchangeable law.
narrative_ontology:constraint_claim(utopia_apocalypse_fragility, tangled_rope).

% Binary flags
% The maintenance of "Utopia" requires constant, active effort to prevent the slide.
domain_priors:requires_active_enforcement(utopia_apocalypse_fragility).

% Structural property derivation hooks:
% Systemic transformation benefits from the destruction of the old state.
narrative_ontology:constraint_beneficiary(utopia_apocalypse_fragility, systemic_transformation).
% Individual human agency is systematically victimized by catastrophic shifts.
narrative_ontology:constraint_victim(utopia_apocalypse_fragility, individual_human_agency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE VICTIM OF CALAMITY (SNARE)
% The shift to horror is felt as a totalizing, predatory trap.
constraint_indexing:constraint_classification(utopia_apocalypse_fragility, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE UTOPIAN STRATEGIST (ROPE)
% Views the system as a necessary coordination mechanism to manage fragility.
constraint_indexing:constraint_classification(utopia_apocalypse_fragility, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE MODERATE CITIZEN (TANGLED ROPE)
% Experiences the system as both beneficial coordination and a source of existential risk.
constraint_indexing:constraint_classification(utopia_apocalypse_fragility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE EXISTENTIAL REALIST (MOUNTAIN)
% Views the underlying fragility as an unchangeable, natural law of complex systems.
constraint_indexing:constraint_classification(utopia_apocalypse_fragility, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(utopia_apocalypse_fragility_tests).

test(perspectival_gap) :-
    % Verify the gap between the victim (Snare) and the strategist (Rope).
    constraint_indexing:constraint_classification(utopia_apocalypse_fragility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(utopia_apocalypse_fragility, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    assertion(TypePowerless == snare),
    assertion(TypeInstitutional == rope),
    TypePowerless \= TypeInstitutional.

test(tangled_rope_emergence) :-
    % Verify that a moderate agent perceives the mixed nature of the constraint.
    constraint_indexing:constraint_classification(utopia_apocalypse_fragility, tangled_rope, context(agent_power(moderate), _, _, _)).

test(threshold_validation) :-
    % High extraction is required for Snare/Tangled Rope classifications.
    narrative_ontology:constraint_metric(utopia_apocalypse_fragility, extractiveness, E),
    assertion(E >= 0.46).

:- end_tests(utopia_apocalypse_fragility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE: [RESOLVED MANDATROPHY]
 * The high extraction score (0.7) reflects the total loss of safety and agency for a victim
 * of systemic collapse. This triggers the Mandatrophy gate. The resolution is perspectival:
 * what is a catastrophic Snare for the victim is a necessary Rope of coordination for the
 * institutional strategist managing the system's fragility. The Tangled Rope classification
 * for the moderate citizen captures this duality, acknowledging both the coordination benefit
 * and the asymmetric extraction risk. The addition of `requires_active_enforcement` is critical,
 * as it grounds the Tangled Rope classification in the reality that "Utopia" is not a passive
 * state but one that requires continuous effort to maintain against stochastic decay.
 * The temporal data models this decay, showing extraction (fragility) increasing over time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    utopia_apocalypse_fragility_omega1,
    'Is the shift from safety to calamity a functional necessity of stochastic systems or a predatory result of human failure?',
    'Audit of cultural shifts preceding the "fell wind" arrival',
    'If necessity: Mountain. If predatory choice: Snare.',
    confidence_without_resolution(medium)
).

omega_variable(
    utopia_apocalypse_fragility_omega2,
    'Can the strength of individual belief influence the underlying stochastic structure of the cliff-edge?',
    'Long-term tracking of social outcomes based on belief-strength metrics in fragile systems',
    'If potent: The cliff-edge is a manipulable Rope. If impotent: It is a fixed Mountain.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(utopia_apocalypse_fragility, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint's fragility (extraction) is modeled as increasing over time,
% representing systemic decay or entropy. This data is required because base_extractiveness > 0.46.

% Theater ratio over time (slight increase in performative management):
narrative_ontology:measurement(uaf_tr_t0, utopia_apocalypse_fragility, theater_ratio, 0, 0.0).
narrative_ontology:measurement(uaf_tr_t5, utopia_apocalypse_fragility, theater_ratio, 5, 0.05).
narrative_ontology:measurement(uaf_tr_t10, utopia_apocalypse_fragility, theater_ratio, 10, 0.1).

% Extraction over time (increasing fragility/risk):
narrative_ontology:measurement(uaf_ex_t0, utopia_apocalypse_fragility, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(uaf_ex_t5, utopia_apocalypse_fragility, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(uaf_ex_t10, utopia_apocalypse_fragility, base_extractiveness, 10, 0.7).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The management of the "cliff-edge" is a form of societal rule-setting.
narrative_ontology:coordination_type(utopia_apocalypse_fragility, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */