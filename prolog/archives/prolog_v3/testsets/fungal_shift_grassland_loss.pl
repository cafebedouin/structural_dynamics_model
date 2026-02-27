% ============================================================================
% CONSTRAINT STORY: fungal_shift_grassland_loss
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_fungal_shift_grassland_loss, []).

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
    domain_priors:emerges_naturally/1.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fungal_shift_grassland_loss
 *   human_readable: Ecological Transition from Grassland to Scrub via Fungal Network Disruption
 *   domain: ecological
 *
 * SUMMARY:
 *   Warming winters reduce stable snowpack in mountain ecosystems, altering
 *   the soil environment. This disrupts the complex, symbiotic arbuscular
 *   mycorrhizal fungi (AMF) networks that co-evolved with native grasses.
 *   The resulting shift to a less diverse, "fast-colonizing" fungal community
 *   favors opportunistic shrubs, which outcompete and replace the grasslands,
 *   leading to a permanent shift in the ecosystem's state.
 *
 * KEY AGENTS (by structural relationship):
 *   - Native Mountain Grasslands: Primary target (powerless/trapped) — lose their ecological niche.
 *   - Opportunistic Shrubs: Primary beneficiary (organized/mobile) — expand their range into former grasslands.
 *   - AM Fungal Communities: The medium through which the constraint operates; their composition *is* the constraint.
 *   - Ecologists: Analytical observer — study the mechanism and consequences.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fungal_shift_grassland_loss, 0.08).
domain_priors:suppression_score(fungal_shift_grassland_loss, 0.02).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(fungal_shift_grassland_loss, 0.0).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fungal_shift_grassland_loss, extractiveness, 0.08).
narrative_ontology:constraint_metric(fungal_shift_grassland_loss, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(fungal_shift_grassland_loss, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain.
narrative_ontology:constraint_metric(fungal_shift_grassland_loss, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(fungal_shift_grassland_loss, resistance, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(fungal_shift_grassland_loss, mountain).

% --- Binary flags ---
% This is a natural process, not a designed mechanism. No enforcement or sunset.

% --- Emergence flag (required for mountain constraints) ---
% This constraint is a direct consequence of biophysical laws under new
% climate conditions. It is not designed or enforced by any agent.
domain_priors:emerges_naturally(fungal_shift_grassland_loss).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Although this is a Mountain, we declare beneficiary/victim for narrative
% clarity and to show the directionality of the ecological pressure. The low ε
% ensures the classification remains Mountain regardless of perspective.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(fungal_shift_grassland_loss, opportunistic_shrubs).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(fungal_shift_grassland_loss, native_mountain_grasslands).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function.
   This is a uniform-type constraint: due to extremely low base extractiveness (ε)
   and suppression, it classifies as Mountain from all perspectives. The
   perspectival scaling of χ has a negligible effect.
   ========================================================================== */

% PERSPECTIVE 1: THE TARGET (NATIVE GRASSLANDS)
% The grasslands are trapped by the new ecological reality. Their co-evolved
% fungal partners are gone, and they cannot adapt or move. From their
% perspective, this is an unchangeable, absolute limit on their survival.
constraint_indexing:constraint_classification(fungal_shift_grassland_loss, mountain,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE BENEFICIARY (OPPORTUNISTIC SHRUBS)
% The shrubs benefit from the altered soil conditions. For them, this is a
% natural law of the new environment that enables their expansion. They
% experience it as a fixed, advantageous reality, not a system of coordination.
constraint_indexing:constraint_classification(fungal_shift_grassland_loss, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (ECOLOGISTS)
% Scientists observing the system identify it as a state shift governed by
% biophysical laws. They see a causal chain rooted in physics (climate change)
% leading to a stable but different ecosystem state. It is a natural law of
% the new climate regime.
constraint_indexing:constraint_classification(fungal_shift_grassland_loss, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fungal_shift_grassland_loss_tests).

test(classification_invariance, [forall(member(P, [powerless, organized, analytical]))]) :-
    % Verify that this is a uniform-type constraint, classifying as Mountain
    % from all key perspectives. This is the hallmark of a natural law.
    constraint_indexing:constraint_classification(fungal_shift_grassland_loss, mountain, context(agent_power(P), _, _, _)).

test(mountain_thresholds_met) :-
    % Verify the constraint meets the strict numerical criteria for a Mountain.
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(suppression_metric_name, SupMetricName),
    narrative_ontology:constraint_metric(fungal_shift_grassland_loss, ExtMetricName, E),
    narrative_ontology:constraint_metric(fungal_shift_grassland_loss, SupMetricName, S),
    E =< 0.25,
    S =< 0.05.

test(natural_law_profile_complete) :-
    % Verify that the full natural law profile is declared, enabling
    % certification by the structural_signatures module.
    domain_priors:emerges_naturally(fungal_shift_grassland_loss),
    narrative_ontology:constraint_metric(fungal_shift_grassland_loss, accessibility_collapse, AC), AC >= 0.85,
    narrative_ontology:constraint_metric(fungal_shift_grassland_loss, resistance, R), R =< 0.15.

:- end_tests(fungal_shift_grassland_loss_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This constraint is classified as a Mountain because it represents a
 *   non-negotiable shift in biophysical reality for the agents involved. The
 *   base extractiveness (ε=0.08) is low, reflecting that this is a highly
 *   efficient, near-lossless transformation of the ecosystem's state, not a
 *   coercive extraction by one agent from another in the human sense. The
 *   suppression score (0.02) is also very low, as the lack of alternatives for
 *   the grasses is a consequence of physical reality (their symbiotic fungi
 *   are gone), not active enforcement. The constraint is certified as a
 *   natural law via the `emerges_naturally` flag and the `accessibility_collapse`
 *   (0.95) and `resistance` (0.05) metrics.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap; this is a uniform-type constraint. All agents,
 *   whether they benefit (shrubs) or suffer (grasses), experience the constraint
 *   as an immutable fact of their environment. The extremely low ε value ensures
 *   that the effective extraction χ remains near zero across all perspectives,
 *   leading to a consistent 'Mountain' classification. This invariance is a
 *   key signature of a true natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Despite being a Mountain, the process has clear directionality. The
 *   `opportunistic_shrubs` are declared beneficiaries, and the `native_mountain_grasslands`
 *   are victims. This reflects the ecological reality of the shift. The Deferential
 *   Realism engine uses this to calculate directionality `d`, but because `d`
 *   scales a very small ε, the final classification is unaffected. This correctly
 *   models a natural process that has winners and losers without being a
 *   system of social coordination or extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The Mountain classification, backed by the Natural Law Profile, prevents
 *   misinterpreting this ecological shift as a Snare or Tangled Rope. One might
 *   colloquially say the shrubs are "exploiting" the grasses, but the framework
 *   correctly identifies that this is not an actively enforced, high-extraction
 *   system. It is a change in the fundamental rules of the environment. The
 *   system's stability comes from physics, not from coercion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_fungal_shift_grassland_loss,
    'Is the grassland-to-scrub transition a permanent, irreversible tipping point, or could the original fungal network be restored if warming trends were reversed?',
    'Long-term ecological monitoring and controlled experiments reintroducing stable snowpack conditions over decades.',
    'If reversible, the constraint is a function of a dynamic variable (climate). If irreversible, it is a permanent state change, solidifying its Mountain classification.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(fungal_shift_grassland_loss, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data is required for high-extraction constraints (ε > 0.46) to
% detect drift. As this is a low-extraction Mountain constraint (ε = 0.08),
% temporal lifecycle measurements are not applicable. The constraint's core
% properties are stable and intrinsic to the new ecological state.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% This is a natural process, not a human-designed coordination mechanism.
% No coordination_type or Boltzmann floor is applicable.

% Network relationships (structural influence edges)
% This local ecological constraint is a direct downstream consequence of a
% global-scale anthropogenic constraint.
narrative_ontology:affects_constraint(global_carbon_cycle_disruption, fungal_shift_grassland_loss).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary. The structural declarations of beneficiary and
% victim accurately capture the ecological pressures, and the resulting
% classification is uniform across all perspectives due to the low ε.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */