% ============================================================================
% CONSTRAINT STORY: recipe_scaling_ai
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_recipe_scaling_ai, []).

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
 * * constraint_id: recipe_scaling_ai
 * human_readable: The NYT Cooking Generative Scaling Constraint
 * domain: technological
 * * SUMMARY:
 * A generative AI tool that automates recipe doubling/halving. While it
 * removes the 'mental math' barrier for users, it introduces 'untested' risks
 * regarding thermodynamics and physical vessel limits, effectively offloading
 * the cognitive burden of validation and risk assessment onto the cook.
 * * KEY AGENTS:
 * - Novice Home Cooks: Subject (Powerless against 'orange text' instructions).
 * - NYT Cooking Platform: Beneficiary (Institutional coordination provider).
 * - Professional Recipe Editors: Auditor (Analytical guardians of culinary 'common sense').
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is 0.49 because the system shifts the risk of 'untested' failure
% and the cognitive load of validation onto the user's 'instincts'.
domain_priors:base_extractiveness(recipe_scaling_ai, 0.49).
domain_priors:suppression_score(recipe_scaling_ai, 0.25).
domain_priors:theater_ratio(recipe_scaling_ai, 0.10).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(recipe_scaling_ai, extractiveness, 0.49).
narrative_ontology:constraint_metric(recipe_scaling_ai, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(recipe_scaling_ai, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% It presents itself as a helpful tool, a pure coordination mechanism.
narrative_ontology:constraint_claim(recipe_scaling_ai, tangled_rope).
narrative_ontology:human_readable(recipe_scaling_ai, "The NYT Cooking Generative Scaling Constraint").
narrative_ontology:topic_domain(recipe_scaling_ai, "technological").

% Binary flags
narrative_ontology:has_sunset_clause(recipe_scaling_ai).      % Implied by 'beta' status and 'constantly improving' language.
domain_priors:requires_active_enforcement(recipe_scaling_ai). % User must 'taste and adjust', enforcing the final quality.

% Structural property derivation hooks:
% These are required for Tangled Rope and Scaffold classifications.
narrative_ontology:constraint_beneficiary(recipe_scaling_ai, nyt_cooking_platform).
narrative_ontology:constraint_victim(recipe_scaling_ai, novice_home_cooks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE NOVICE COOK (SNARE)
% Without 'decades of experience,' the scaled-but-untested recipe is a trap.
% The user feels high extraction as they bear the risk of failure.
% χ = 0.49 * 1.5 (powerless) * 0.8 (local) = 0.588
constraint_indexing:constraint_classification(recipe_scaling_ai, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PLATFORM (ROPE)
% Viewed as essential infrastructure that solves a user coordination problem.
% The platform feels minimal extraction.
% χ = 0.49 * -0.2 (institutional) * 1.0 (national) = -0.098
constraint_indexing:constraint_classification(recipe_scaling_ai, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE CULINARY ANALYST (TANGLED ROPE)
% Recognizes the coordination benefit but also the asymmetric extraction of risk.
% χ = 0.49 * 1.15 (analytical) * 1.2 (global) = 0.676
constraint_indexing:constraint_classification(recipe_scaling_ai, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE BETA FEATURE (SCAFFOLD)
% The feature is 'constantly working to improve,' implying a temporary state.
constraint_indexing:constraint_classification(recipe_scaling_ai, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))) :-
    narrative_ontology:has_sunset_clause(recipe_scaling_ai).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(recipe_scaling_ai_tests).

test(perspectival_gap) :-
    % Verify the constraint is a Snare for the powerless but a Rope for the institution.
    constraint_indexing:constraint_classification(recipe_scaling_ai, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(recipe_scaling_ai, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(recipe_scaling_ai, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    domain_priors:base_extractiveness(recipe_scaling_ai, E),
    E >= 0.46, % Triggers high-extraction logic requirements.
    % Verify tangled rope requirements are met
    domain_priors:requires_active_enforcement(recipe_scaling_ai),
    narrative_ontology:constraint_beneficiary(recipe_scaling_ai, _),
    narrative_ontology:constraint_victim(recipe_scaling_ai, _).

:- end_tests(recipe_scaling_ai_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.49) reflects the 'untested' nature of the output.
 * While the AI provides the 'math,' it extracts the user's cognitive labor
 * to monitor 'doneness cues' and 'vessel capacity', effectively offloading
 * the risk of failure from the platform to the individual cook. This creates a
 * significant perspectival gap: the platform sees a pure coordination tool (Rope),
 * while the novice cook experiences a high-risk trap (Snare). The analytical
 * view, which accounts for both the coordination function and the asymmetric
 * risk, correctly identifies it as a Tangled Rope.
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * This is a 'Tangled Rope' because the coordination benefit (scaling 25,000+ recipes)
 * is real, but it is coupled with a significant, asymmetric extraction of cognitive
 * load and risk onto the user. Classifying it as a pure Snare would ignore the
 * genuine utility, while classifying it as a Rope would ignore the hidden costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_ai_gastronomy,
    'Can generative AI reliably compute thermal mass and evaporation rates?',
    'A/B testing of AI-scaled recipes against human-tested control groups.',
    'True = Scaling becomes a Mountain (Law of Physics); False = Remains a permanent Snare/Tangled Rope.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(recipe_scaling_ai, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint has high extraction (0.49 > 0.46), requiring temporal data.
% The model shows extraction increasing as the feature moved from a simple
% calculator (low extraction) to a deeply integrated system that users rely on,
% shifting more cognitive risk over time. Theater remains low as it is functional.

% Theater ratio over time (metric_substitution detection):
narrative_ontology:measurement(recipe_scaling_ai_tr_t0, recipe_scaling_ai, theater_ratio, 0, 0.05).
narrative_ontology:measurement(recipe_scaling_ai_tr_t5, recipe_scaling_ai, theater_ratio, 5, 0.08).
narrative_ontology:measurement(recipe_scaling_ai_tr_t10, recipe_scaling_ai, theater_ratio, 10, 0.10).

% Extraction over time (extraction_accumulation detection):
narrative_ontology:measurement(recipe_scaling_ai_ex_t0, recipe_scaling_ai, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(recipe_scaling_ai_ex_t5, recipe_scaling_ai, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(recipe_scaling_ai_ex_t10, recipe_scaling_ai, base_extractiveness, 10, 0.49).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The tool provides a standardized method for transforming recipe information.
narrative_ontology:coordination_type(recipe_scaling_ai, information_standard).

% Boltzmann floor override (only if domain knowledge justifies)
% narrative_ontology:boltzmann_floor_override(recipe_scaling_ai, 0.1).

% Network relationships (structural influence edges)
% narrative_ontology:affects_constraint(recipe_scaling_ai, other_constraint_id).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */