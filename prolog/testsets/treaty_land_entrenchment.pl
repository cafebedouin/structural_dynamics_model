% ============================================================================
% CONSTRAINT STORY: treaty_land_entrenchment
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-15
% ============================================================================

:- module(constraint_treaty_land_entrenchment, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

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
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: treaty_land_entrenchment
 *   human_readable: Treaty Sovereignty (The Crown-Indigenous Mountain)
 *   domain: legal/political
 *
 * SUMMARY:
 *   Alberta exists on Treaty 6, 7, and 8 lands. These treaties are agreements
 *   with the Crown, not the province. Any sovereigntist move to separate from
 *   Canada creates an irreducible collision with the "Mountain" of Treaty
 *   obligations, which cannot be unilaterally transferred to a new state
 *   without Indigenous consent. This constraint is the existence of that
 *   foundational legal fact.
 *
 * KEY AGENTS (by structural relationship):
 *   - separatist_state_architects: Primary target (organized/constrained) — their goal of unilateral secession is blocked by the constraint.
 *   - first_nations_holders: Primary beneficiary (organized/trapped) — their sovereignty and title are entrenched by the treaties.
 *   - federal_crown: Institutional actor (institutional/constrained) — the legal counter-party to the treaties, bound by them.
 *   - provincial_citizen: Powerless actor (powerless/trapped) - experiences the legal structure as an unchangeable fact.
 *   - constitutional_lawyers: Analytical observer — sees the full constitutional structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_land_entrenchment, 0.08).
domain_priors:suppression_score(treaty_land_entrenchment, 0.02).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(treaty_land_entrenchment, 0.05).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_land_entrenchment, extractiveness, 0.08).
narrative_ontology:constraint_metric(treaty_land_entrenchment, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(treaty_land_entrenchment, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl.
narrative_ontology:constraint_metric(treaty_land_entrenchment, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(treaty_land_entrenchment, resistance, 0.01).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(treaty_land_entrenchment, mountain).
narrative_ontology:human_readable(treaty_land_entrenchment, "Treaty Sovereignty (The Crown-Indigenous Mountain)").
narrative_ontology:topic_domain(treaty_land_entrenchment, "legal/political").

% --- Emergence flag (required for mountain constraints) ---
% The treaties emerge from historical statecraft and are now entrenched as a
% constitutional reality, functioning as a natural law of the political system.
domain_priors:emerges_naturally(treaty_land_entrenchment).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Although this is a mountain, these declarations clarify the structural roles.
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(treaty_land_entrenchment, first_nations_holders).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(treaty_land_entrenchment, separatist_state_architects).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE TREATY HOLDER (MOUNTAIN)
% Treaties are seen as eternal, sacred covenants that pre-date and
% outlast provincial political structures. An unchangeable foundation.
constraint_indexing:constraint_classification(treaty_land_entrenchment, mountain,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE SECESSIONIST (MOUNTAIN)
% From a "Prosperity Project" view, Treaty obligations are an immovable
% constitutional obstacle blocking the path to unilateral sovereignty.
% With ε=0.08, it is not a Snare, but a Mountain.
constraint_indexing:constraint_classification(treaty_land_entrenchment, mountain,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% Recognizes a constitutional Mountain that renders unilateral
% secession legally impossible without total system collapse.
constraint_indexing:constraint_classification(treaty_land_entrenchment, mountain,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(continental))).

% PERSPECTIVE 4: THE FEDERAL CROWN (INSTITUTIONAL)
% As the legal counter-party to the treaties, the Crown views them as a
% foundational, constitutionally entrenched, and unchangeable reality.
constraint_indexing:constraint_classification(treaty_land_entrenchment, mountain,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(constrained), % Cannot unilaterally abrogate treaties
            spatial_scope(national))).

% PERSPECTIVE 5: THE PROVINCIAL CITIZEN (POWERLESS)
% A non-Indigenous citizen experiences the treaty structure as a fixed,
% background legal fact they have no power to alter.
constraint_indexing:constraint_classification(treaty_land_entrenchment, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treaty_land_entrenchment_tests).

test(uniform_mountain_classification) :-
    % Verify this is a uniform-type constraint: all perspectives classify it as a mountain.
    forall(
        constraint_indexing:constraint_classification(treaty_land_entrenchment, Type, _),
        Type == mountain
    ).

test(threshold_validation) :-
    % Verify the metrics are consistent with a Mountain classification.
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(suppression_metric_name, SuppMetricName),
    narrative_ontology:constraint_metric(treaty_land_entrenchment, ExtMetricName, E),
    narrative_ontology:constraint_metric(treaty_land_entrenchment, SuppMetricName, S),
    E =< 0.25,
    S =< 0.05.

:- end_tests(treaty_land_entrenchment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Classified as a Mountain because Treaty rights are constitutionalized and
 *   derive from a source of law (The Crown) that is independent of provincial
 *   sovereignty. A province cannot "secede" from a Treaty agreement to which it is not
 *   the primary party. The base extractiveness (ε=0.08) and suppression (0.02)
 *   are extremely low, consistent with a foundational law of a political system.
 *
 * PERSPECTIVAL GAP:
 *   There is no gap in classification type; all actors perceive an unchangeable
 *   Mountain. The gap is in valence and interpretation. For First Nations and the
 *   Crown, it is a protective, foundational Mountain that guarantees order. For
 *   secessionists, it is an obstructive Mountain that blocks their political ambitions.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the First Nations whose title and rights are entrenched by
 *   the treaties. The victim is any political movement attempting a "clean break"
 *   from Canada without addressing the foundational law of the land, as their
 *   primary goal is structurally impossible.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents the narrative of provincial sovereignty from being
 *   viewed as a simple exit or arbitrage opportunity. It forces the recognition
 *   of a pre-existing, immovable coordination structure that cannot be bypassed,
 *   correctly identifying it as a structural limit rather than a policy choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_treaty_land_entrenchment,
    'Can a new Alberta state legally inherit Crown Treaty obligations?',
    'Constitutional analysis of successor-state law and the requirement for Indigenous consent.',
    'If no, secession creates a legal vacuum on 100% of the territory, rendering the new state''s claim to the land baseless. If yes, the "sovereignty" is immediately constrained by the same obligations.',
    confidence_without_resolution(low)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_treaty_land_entrenchment, conceptual, 'Legal succession of Treaty obligations under international and constitutional law').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(treaty_land_entrenchment, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required for low-extraction constraints (ε < 0.46).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The treaties function as a foundational enforcement mechanism for sovereignty claims.
narrative_ontology:coordination_type(treaty_land_entrenchment, enforcement_mechanism).

% Network relationships (structural influence edges)
% This Mountain is the ultimate structural limit on any provincial attempt
% to frame sovereignty as a simple arbitrage play.
narrative_ontology:affects_constraint(treaty_land_entrenchment, sovereignty_as_arbitrage).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations correctly models the relationships.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */