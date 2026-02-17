% ============================================================================
% CONSTRAINT STORY: happiness_of_others
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_happiness_of_others, []).

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
 * * constraint_id: happiness_of_others
 * human_readable: The Social Responsibility for the Happiness of Others
 * domain: social/psychological
 * * SUMMARY:
 * This constraint models the social norm that one is responsible for, and can directly cause, the happiness of another person.
 * The narrative argues this is a fallacy; one can only create the *conditions* for happiness. The attempt to directly engineer another's emotional state is an extractive trap, draining the "helper" and disempowering the "helped."
 * * KEY AGENTS:
 * - The Crucified: An individual in a state of extreme suffering, for whom happiness is an impossible choice. (Subject)
 * - The Obsessive Caretaker: An individual who attempts to force happiness on another, becoming a victim of the constraint.
 * - The Therapy Practice: An institution that provides structured support for well-being, benefiting from a clear boundary of responsibility. (Beneficiary)
 * - The Systems Analyst: An observer analyzing the dynamics of codependency and agency. (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(happiness_of_others, 0.75). % Attempting to "make" others happy extracts the total energy of the helper.
domain_priors:suppression_score(happiness_of_others, 0.50).   % Suppresses the alternative view of individual emotional sovereignty.
domain_priors:theater_ratio(happiness_of_others, 0.15).       % The actions are genuine, not performative, but misguided.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(happiness_of_others, extractiveness, 0.75).
narrative_ontology:constraint_metric(happiness_of_others, suppression_requirement, 0.50).
narrative_ontology:constraint_metric(happiness_of_others, theater_ratio, 0.15).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be a form of social coordination (mutual care).
narrative_ontology:constraint_claim(happiness_of_others, tangled_rope).
narrative_ontology:human_readable(happiness_of_others, "The Social Responsibility for the Happiness of Others").

% Binary flags
% The social norm requires active enforcement through praise, guilt, and expectation.
domain_priors:requires_active_enforcement(happiness_of_others).

% Structural property derivation hooks:
% Beneficiaries (therapists, self-sufficient individuals) use the boundary for coordination.
narrative_ontology:constraint_beneficiary(happiness_of_others, therapy_practices).
narrative_ontology:constraint_beneficiary(happiness_of_others, individuals_with_strong_boundaries).
% Victims are those trapped in codependent relationships.
narrative_ontology:constraint_victim(happiness_of_others, obsessive_caretakers).
narrative_ontology:constraint_victim(happiness_of_others, enmeshed_family_members).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT ("THE CRUCIFIED") - MOUNTAIN
% For someone in extreme suffering, the unavailability of happiness is an immutable law.
% χ = 0.75 * π(powerless:1.5) * σ(local:0.8) = 0.90
constraint_indexing:constraint_classification(happiness_of_others, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (THERAPY PRACTICE) - ROPE
% For a therapist, the boundary of responsibility is a pure coordination tool.
% χ = 0.75 * π(institutional:-0.2) * σ(regional:0.9) = -0.135 (Effective extraction is negative/beneficial)
constraint_indexing:constraint_classification(happiness_of_others, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER - TANGLED ROPE
% The analyst sees both the coordination function (maintaining boundaries) and the
% asymmetric extraction from those who don't respect them.
% χ = 0.75 * π(analytical:1.15) * σ(global:1.2) = 1.035
constraint_indexing:constraint_classification(happiness_of_others, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(happiness_of_others_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(happiness_of_others, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(happiness_of_others, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == mountain,
    TypeInstitutional == rope.

test(analytical_classification_is_tangled_rope) :-
    % The analytical view must resolve the conflict into a Tangled Rope.
    constraint_indexing:constraint_classification(happiness_of_others, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Verify this is a high-extraction constraint.
    narrative_ontology:constraint_metric(happiness_of_others, extractiveness, E),
    E >= 0.46.

:- end_tests(happiness_of_others_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness (0.75) is high because the attempt to control another's happiness is a totalizing, energy-draining endeavor for the caretaker.
 * The suppression score (0.5) reflects the societal pressure to engage in this behavior, suppressing the alternative of radical personal autonomy.
 *
 * The Perspectival Gap is stark:
 * - For the 'Crucified' (powerless, trapped), the absence of happiness is a physical law, an unchangeable Mountain.
 * - For a Therapy Practice (institutional, arbitrage), the principle "you can't make others happy" is a foundational Rope. It coordinates the therapeutic relationship, defines professional boundaries, and prevents burnout.
 * - The Analytical observer sees the whole system: a well-meaning social norm that functions as a Rope for the psychologically sophisticated but becomes a brutal Snare for codependents. This dual nature makes it a canonical Tangled Rope.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The Tangled Rope classification is critical. A simpler analysis might label the constraint a Snare (for the caretaker) or a Rope (for the therapist). By classifying it as a Tangled Rope, the system acknowledges that the *same principle* has both a valid coordination function (boundary setting) and a severe, asymmetric extraction mechanism (codependency). This prevents mislabeling the entire structure as purely malicious or purely beneficial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_happiness_of_others,
    "Is the inability to 'make' others happy a hard biological limit (Mountain) or an emergent social boundary (Rope) evolved to preserve individual sovereignty and prevent codependency?",
    "Comparative psychological studies of autonomy vs. happiness in dyads, combined with neurological studies on empathy vs. emotional contagion.",
    "If it's a hard limit, the constraint is a Mountain of physics. If it's an evolved boundary, it's a Rope of social structure, and attempts to violate it create a Snare.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(happiness_of_others, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This social norm is modeled as starting with high extraction (codependent assumptions)
% and slightly intensifying before the "boundary" insight becomes more widespread.
% Theater is low because the effort is genuine, not performative.

% Theater ratio over time:
narrative_ontology:measurement(happiness_of_others_tr_t0, happiness_of_others, theater_ratio, 0, 0.10).
narrative_ontology:measurement(happiness_of_others_tr_t5, happiness_of_others, theater_ratio, 5, 0.12).
narrative_ontology:measurement(happiness_of_others_tr_t10, happiness_of_others, theater_ratio, 10, 0.15).

% Extraction over time:
narrative_ontology:measurement(happiness_of_others_ex_t0, happiness_of_others, base_extractiveness, 0, 0.70).
narrative_ontology:measurement(happiness_of_others_ex_t5, happiness_of_others, base_extractiveness, 5, 0.72).
narrative_ontology:measurement(happiness_of_others_ex_t10, happiness_of_others, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The coordination type is an enforcement mechanism for social boundaries.
narrative_ontology:coordination_type(happiness_of_others, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */