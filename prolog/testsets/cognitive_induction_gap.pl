% ============================================================================
% CONSTRAINT STORY: cognitive_induction_gap
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_cognitive_induction_gap, []).

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
 * * constraint_id: cognitive_induction_gap
 * human_readable: The Induction Gap (Cognitive Compromise)
 * domain: psychological/social
 * * SUMMARY:
 * This constraint models the "problem of induction" as a social and psychological vulnerability. While humans use intuitive leaps to bridge the gap between experience and novel situations, factors like high stress, social pressure, and emotional investment can compromise this ability. This leads to a state where an individual's judgment is extracted in favor of group cohesion or flawed heuristics.
 * * KEY AGENTS:
 * - The Stressed Decision-Maker: Subject (Powerless), whose cognitive capacity is overwhelmed.
 * - The Crisis Management Team: Beneficiary (Institutional), which relies on group cohesion for rapid action.
 * - The Epistemologist: Auditor (Analytical), who observes the structural trade-offs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(cognitive_induction_gap, 0.70). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(cognitive_induction_gap, 0.60).   % Structural property (raw, unscaled). Social pressure and information overload suppress dissent.
domain_priors:theater_ratio(cognitive_induction_gap, 0.10).       % Piton detection (>= 0.70). The effect is functional, not theatrical.

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(cognitive_induction_gap, extractiveness, 0.70).
narrative_ontology:constraint_metric(cognitive_induction_gap, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(cognitive_induction_gap, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(cognitive_induction_gap, tangled_rope).
narrative_ontology:human_readable(cognitive_induction_gap, "The Induction Gap (Cognitive Compromise)").
narrative_ontology:topic_domain(cognitive_induction_gap, "psychological/social").

% Binary flags
% narrative_ontology:has_sunset_clause(cognitive_induction_gap).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(cognitive_induction_gap). % Required for Tangled Rope. Social suasion is a form of active enforcement.

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(cognitive_induction_gap, group_cohesion).
narrative_ontology:constraint_victim(cognitive_induction_gap, individual_judgment).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE STRESSED DECISION-MAKER (SNARE)
% For the individual under high stress and information overload, the gap is a snare.
% Their cognitive capacity is extracted by pressure, strangling their ability to make
% effective intuitive leaps and leading to poor, coerced decisions.
constraint_indexing:constraint_classification(cognitive_induction_gap, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE CRISIS MANAGEMENT TEAM (ROPE)
% For an institutional body that needs rapid, unified action, leveraging group
% dynamics to bridge the induction gap is a coordination tool (Rope). It ensures
% cohesion and decisiveness, viewing the suppression of individual doubt as a
% necessary feature for operational effectiveness.
constraint_indexing:constraint_classification(cognitive_induction_gap, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the coordination function (group cohesion) and the
% asymmetric extraction (loss of individual judgment). The need for active social
% enforcement to maintain this balance makes it a canonical Tangled Rope.
constraint_indexing:constraint_classification(cognitive_induction_gap, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE PRACTICAL INTUITIVE (ROPE)
% For an agent who consciously uses feedback to improve their intuitive leaps over
% time, the gap is a manageable coordination problem. They are not trapped by it
% but use it as a tool, making it a Rope.
constraint_indexing:constraint_classification(cognitive_induction_gap, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cognitive_induction_gap_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(cognitive_induction_gap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cognitive_induction_gap, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_classification_is_tangled_rope) :-
    % The analytical observer should classify this as Tangled Rope.
    constraint_indexing:constraint_classification(cognitive_induction_gap, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Verify that the base extractiveness is high, justifying Snare/Tangled Rope classifications.
    domain_priors:base_extractiveness(cognitive_induction_gap, E),
    E >= 0.46.

:- end_tests(cognitive_induction_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness (0.70) is high because the constraint fundamentally hijacks an individual's capacity for judgment. The suppression score (0.60) reflects the power of social suasion and information overload to prevent individuals from realizing their judgment is compromised.
 *
 * The Perspectival Gap is stark:
 * - The 'powerless' individual experiences this as a Snare, a cognitive trap where their autonomy is extracted by overwhelming circumstances.
 * - The 'institutional' beneficiary sees it as a Rope, a necessary tool for forging group cohesion and enabling rapid, unified action, even at the cost of individual nuance.
 * - The 'analytical' observer, seeing both the coordination benefit and the extractive cost, correctly identifies it as a Tangled Rope. This classification is critical because it acknowledges the dual nature of the constraint, preventing a misclassification as either pure coordination (Rope) or pure predation (Snare).
 *
 * [RESOLVED MANDATROPHY]
 * The Tangled Rope classification resolves mandatrophy by acknowledging that the mechanism (social cohesion) has a genuine coordination function for the group, while simultaneously imposing a severe extractive cost on the individual. It is not simply a Snare, because the group *does* achieve a coordinated outcome. It is not a Rope, because this outcome is achieved via asymmetric extraction from its members.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_cognitive_induction_gap,
    'Is the cognitive gap a fundamental limit of logic (Mountain) or a biological/social vulnerability (Tangled Rope)?',
    'Development of a formal system that solves the problem of induction, or neuroscientific evidence showing the mechanism is a bypassable heuristic.',
    'If Mountain, the constraint is immutable. If Tangled Rope, it is a structural flaw in human cognition that can be mitigated with better tools or social structures.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(cognitive_induction_gap, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. This models a scenario where the social
% mechanisms for enforcing cohesion become more refined and effective over time,
% increasing the extraction of individual judgment.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time (remains low as the effect is functional):
narrative_ontology:measurement(cig_tr_t0, cognitive_induction_gap, theater_ratio, 0, 0.05).
narrative_ontology:measurement(cig_tr_t5, cognitive_induction_gap, theater_ratio, 5, 0.08).
narrative_ontology:measurement(cig_tr_t10, cognitive_induction_gap, theater_ratio, 10, 0.10).

% Extraction over time (the cost to individual judgment increases as groupthink solidifies):
narrative_ontology:measurement(cig_ex_t0, cognitive_induction_gap, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(cig_ex_t5, cognitive_induction_gap, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(cig_ex_t10, cognitive_induction_gap, base_extractiveness, 10, 0.70).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Social suasion acts as an informal enforcement mechanism for group norms.
narrative_ontology:coordination_type(cognitive_induction_gap, enforcement_mechanism).

% Boltzmann floor override (only if domain knowledge justifies)
% narrative_ontology:boltzmann_floor_override(cognitive_induction_gap, 0.1).

% Network relationships (structural influence edges)
% narrative_ontology:affects_constraint(cognitive_induction_gap, another_constraint).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */