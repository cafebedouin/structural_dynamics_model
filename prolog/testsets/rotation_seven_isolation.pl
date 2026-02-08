% ============================================================================
% CONSTRAINT STORY: rotation_seven_isolation
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_rotation_seven_isolation, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: rotation_seven_isolation
 * human_readable: R7 Medical Isolation Protocol
 * domain: political/technological/social
 * * SUMMARY:
 * The R7 Protocol enforces the immediate removal and isolation of "flagged"
 * individuals under the guise of an unchangeable safety necessity. In reality,
 * it is a highly enforced, extractive mechanism that removes social "friction"
 * by erasing the sick from the station's population, suppressing all
 * alternative care models.
 * * KEY AGENTS:
 * - Anna: The subject being "extracted" and isolated (Powerless).
 * - Nurse Chen: The enforcer maintaining the protocol for station safety (Institutional).
 * - Rina: The observer who identifies the underlying extractive nature (Analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(rotation_seven_isolation, 0.85). % Snare extraction >= 0.46
domain_priors:suppression_score(rotation_seven_isolation, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(rotation_seven_isolation, 0.10).       % Low theater; this is a brutally functional system.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(rotation_seven_isolation, extractiveness, 0.85).
narrative_ontology:constraint_metric(rotation_seven_isolation, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(rotation_seven_isolation, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be a necessary coordination mechanism for public health.
narrative_ontology:constraint_claim(rotation_seven_isolation, snare).

% Binary flags
domain_priors:requires_active_enforcement(rotation_seven_isolation). % Requires monitoring, locked wards, etc.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(rotation_seven_isolation, station_population_health).
narrative_ontology:constraint_victim(rotation_seven_isolation, isolated_individuals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% Anna perceives an unyielding, inescapable trap. The protocol's effect is
% total extraction of her social existence.
% χ = 0.85 * π(powerless:1.5) * σ(local:0.8) = 1.02
constraint_indexing:constraint_classification(rotation_seven_isolation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Nurse Chen sees the protocol as essential coordination for public health.
% From her institutional view, the extraction is a net negative value,
% meaning the system provides more benefit than it costs her role.
% χ = 0.85 * π(institutional:-0.2) * σ(regional:0.9) = -0.153
constraint_indexing:constraint_classification(rotation_seven_isolation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% The default analytical context reveals a coercive system. The high base
% extraction and suppression, amplified by global scope, classify it as a Snare.
% χ = 0.85 * π(analytical:1.15) * σ(global:1.2) = 1.173
constraint_indexing:constraint_classification(rotation_seven_isolation, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rotation_seven_isolation_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(rotation_seven_isolation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rotation_seven_isolation, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(snare_threshold_validation) :-
    % Verify the base metrics meet the Snare criteria.
    domain_priors:base_extractiveness(rotation_seven_isolation, E),
    domain_priors:suppression_score(rotation_seven_isolation, S),
    E >= 0.46,
    S >= 0.60.

:- end_tests(rotation_seven_isolation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect a system of extreme, functional coercion. The base
 * extractiveness (0.85) represents the total removal of an individual from
 * society. The suppression score (0.70) reflects the active denial of
 * alternative care models. The perspectival gap is severe: the enforcer
 * (Nurse Chen) experiences it as a beneficial coordination Rope, while the
 * victim (Anna) and the analytical observer see it as a predatory Snare.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The high extraction (0.85) triggers mandatrophy analysis. The resolution
 * lies in the perspectival classification. The system is not a malfunctioning
 * Rope; it is a highly efficient Snare that functions as a Rope for the
 * institutional agent. Recognizing this duality—that a constraint can be
 * simultaneously Rope and Snare depending on the index—is the core insight
 * that resolves the mandatrophy. It correctly identifies the harm without
 * mislabeling the function perceived by the beneficiary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_r7_isolation,
    "Is the suppression of alternative care a result of genuine resource impossibility (a Mountain aspect) or a constructed policy choice to maintain efficiency (a Snare aspect)?",
    "A full, independent audit of station resources and medical technology capabilities compared to peer systems.",
    "If a Mountain, the protocol is a tragic necessity. If a Snare, it is a deliberate act of institutional violence.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(rotation_seven_isolation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for high-extraction constraint (E > 0.46).
% Models a system that was always severe but intensified slightly over time.
%
% Theater ratio over time (remains low, indicating no drift to performance):
narrative_ontology:measurement(r7_iso_tr_t0, rotation_seven_isolation, theater_ratio, 0, 0.10).
narrative_ontology:measurement(r7_iso_tr_t5, rotation_seven_isolation, theater_ratio, 5, 0.10).
narrative_ontology:measurement(r7_iso_tr_t10, rotation_seven_isolation, theater_ratio, 10, 0.10).

% Extraction over time (shows slight intensification of the protocol):
narrative_ontology:measurement(r7_iso_ex_t0, rotation_seven_isolation, base_extractiveness, 0, 0.80).
narrative_ontology:measurement(r7_iso_ex_t5, rotation_seven_isolation, base_extractiveness, 5, 0.82).
narrative_ontology:measurement(r7_iso_ex_t10, rotation_seven_isolation, base_extractiveness, 10, 0.85).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The protocol is a coordination mechanism, enabling Boltzmann analysis.
narrative_ontology:coordination_type(rotation_seven_isolation, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */