% ============================================================================
% CONSTRAINT STORY: collective_action_deadlock
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_collective_action_deadlock, []).

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
 * * constraint_id: collective_action_deadlock
 * human_readable: The Infinite Deliberation Loop
 * domain: political/social
 * * SUMMARY:
 * This constraint represents a state where a group is unable to coordinate
 * a response to a critical threat because the internal rules for decision-making
 * favor a veto by any single stakeholder. This stalemate functions as a 'Snare'
 * for participants trapped by the process, while appearing as a 'Rope' to the
 * beneficiaries who wield the veto power.
 * * KEY AGENTS:
 * - Non-Veto Stakeholders: Subject (Powerless)
 * - Veto Players: Beneficiary (Institutional)
 * - Game Theorist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.75) as the deadlock consumes time/resources while
% the underlying crisis worsens, effectively extracting future optionality.
domain_priors:base_extractiveness(collective_action_deadlock, 0.75).
domain_priors:suppression_score(collective_action_deadlock, 0.65).
domain_priors:theater_ratio(collective_action_deadlock, 0.72). % High theater: constant meetings with no output.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(collective_action_deadlock, extractiveness, 0.75).
narrative_ontology:constraint_metric(collective_action_deadlock, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(collective_action_deadlock, theater_ratio, 0.72).

% Constraint self-claim: The system claims to be a fair coordination mechanism.
narrative_ontology:constraint_claim(collective_action_deadlock, tangled_rope).
narrative_ontology:human_readable(collective_action_deadlock, "The Infinite Deliberation Loop").
narrative_ontology:topic_domain(collective_action_deadlock, "political/social").

% Binary flags and structural properties for Tangled Rope classification.
domain_priors:requires_active_enforcement(collective_action_deadlock). % Enforced by procedural rules.
narrative_ontology:constraint_beneficiary(collective_action_deadlock, veto_players).
narrative_ontology:constraint_victim(collective_action_deadlock, non_veto_stakeholders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For a stakeholder without veto power, the process is a trap that consumes
% their agency and resources with no possibility of success. The high base
% extraction is amplified by their powerlessness.
constraint_indexing:constraint_classification(collective_action_deadlock, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Veto players view the deadlock as a 'Rope'—a necessary protection of their
% institutional interests and a coordination mechanism for stability.
constraint_indexing:constraint_classification(collective_action_deadlock, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analytical view sees a hybrid: a system with a coordination function
% (beneficiary exists) but also severe asymmetric extraction (victim exists)
% that requires active enforcement. This is a canonical Tangled Rope.
constraint_indexing:constraint_classification(collective_action_deadlock, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(collective_action_deadlock_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless and Rope for the institutional beneficiary.
    constraint_indexing:constraint_classification(collective_action_deadlock, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(collective_action_deadlock, rope,
        context(agent_power(institutional), _, _, _)),
    % Ensure the analytical view is different from both.
    constraint_indexing:constraint_classification(collective_action_deadlock, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties) :-
    % Verify that all conditions for a Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(collective_action_deadlock, _),
    narrative_ontology:constraint_victim(collective_action_deadlock, _),
    domain_priors:requires_active_enforcement(collective_action_deadlock).

:- end_tests(collective_action_deadlock_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.75) is high, representing the 'Mandatrophy' where
 * the opportunity cost of inaction and erosion of future options becomes severe.
 * The suppression score (0.65) reflects the procedural rigidity that prevents
 * alternative decision-making paths. The high theater ratio (0.72) indicates
 * that the process is more performative than functional.
 *
 * PERSPECTIVAL GAP:
 * The Non-Veto Stakeholder feels a Snare (their agency is consumed by a process
 * they cannot influence). The Veto Player feels a Rope (the rules coordinate
 * action in a way that protects their specific interests).
 *
 * MANDATROPHY ANALYSIS:
 * * [RESOLVED MANDATROPHY]: The mandatrophy is resolved by the Tangled Rope
 * classification. This acknowledges both the coordination function prized by
 * veto players (preventing a misclassification as a pure Snare) and the severe
 * asymmetric extraction imposed on other stakeholders (preventing a
 * misclassification as a pure Rope). The high theater ratio (0.72) further
 * indicates the system's decay into performative, rather than functional,
 * coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_veto_logic,
    'Is the deadlock a result of genuine value conflict (Mountain) or strategic rent-seeking (Snare)?',
    'Shadow negotiation audit to identify undisclosed payoffs for veto relief.',
    'If payoffs found: Snare. If value conflict persists: Mountain of pluralism.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(collective_action_deadlock, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint started as a reasonable consensus rule but decayed over time,
% becoming more extractive and performative as the stakes increased and
% positions hardened.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(cad_tr_t0, collective_action_deadlock, theater_ratio, 0, 0.30).
narrative_ontology:measurement(cad_tr_t5, collective_action_deadlock, theater_ratio, 5, 0.55).
narrative_ontology:measurement(cad_tr_t10, collective_action_deadlock, theater_ratio, 10, 0.72).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(cad_ex_t0, collective_action_deadlock, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(cad_ex_t5, collective_action_deadlock, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(cad_ex_t10, collective_action_deadlock, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The consensus rule acts as an enforcement mechanism for the status quo.
narrative_ontology:coordination_type(collective_action_deadlock, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */