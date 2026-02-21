% ============================================================================
% CONSTRAINT STORY: gravitational_lensing_cosmic_dawn
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-20
% ============================================================================

:- module(constraint_gravitational_lensing_cosmic_dawn, []).

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
 *   constraint_id: gravitational_lensing_cosmic_dawn
 *   human_readable: Gravitational Lensing as a Cosmic Telescope
 *   domain: physical / scientific
 *
 * SUMMARY:
 *   The theory of General Relativity predicts that massive objects, such as
 *   galaxies and galaxy clusters, warp spacetime, causing light from more
 *   distant objects to bend around them. This effect, known as gravitational
 *   lensing, acts as a natural telescope, magnifying extremely faint and distant
 *   objects that would otherwise be undetectable. This constraint story models
 *   the physical law itself, as demonstrated by the recent observation of QZ1,
 *   a "monster star" from the cosmic dawn magnified over 4000 times by the
 *   galaxy cluster Abell 2744.
 *
 * KEY AGENTS (by structural relationship):
 *   - Cosmologists/Astrophysicists: Analytical observers (analytical/arbitrage) — they leverage the physical law to make discoveries.
 *   - Early Universe Theorists: Analytical observers (analytical/analytical) — their models are confirmed or falsified by the data this constraint reveals.
 *   - Instrument Engineers (e.g., JWST team): Organized actors (organized/mobile) — they build technology specifically designed to exploit this physical effect.
 *   - All Agents: Universal targets (powerless/trapped) — All physical agents are subject to the laws of physics without recourse.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gravitational_lensing_cosmic_dawn, 0.02).
domain_priors:suppression_score(gravitational_lensing_cosmic_dawn, 0.01).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(gravitational_lensing_cosmic_dawn, 0.0).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gravitational_lensing_cosmic_dawn, extractiveness, 0.02).
narrative_ontology:constraint_metric(gravitational_lensing_cosmic_dawn, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(gravitational_lensing_cosmic_dawn, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. These values certify the constraint as a natural law.
narrative_ontology:constraint_metric(gravitational_lensing_cosmic_dawn, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(gravitational_lensing_cosmic_dawn, resistance, 0.01).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(gravitational_lensing_cosmic_dawn, mountain).

% --- Binary flags ---
% N/A for this constraint.

% --- Emergence flag (required for mountain constraints) ---
% This constraint is a fundamental aspect of spacetime geometry.
domain_priors:emerges_naturally(gravitational_lensing_cosmic_dawn).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Omitted. As a natural law (Mountain), this constraint lacks the social
% structure of beneficiaries and victims. All agents are subject to it equally.
% The engine will use canonical d values, which, combined with near-zero ε,
% will correctly classify it as Mountain from all perspectives.

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

% UNIFORM-TYPE CONSTRAINT (MOUNTAIN-ONLY)
% A natural law classifies identically from all perspectives. The following
% classifications demonstrate this invariance. The extremely low base
% extractiveness (ε=0.02) ensures that effective extraction (χ) is negligible
% for all indices, leading to a consistent Mountain classification.

% PERSPECTIVE 1: THE ANALYTICAL OBSERVER (COSMOLOGIST)
constraint_indexing:constraint_classification(gravitational_lensing_cosmic_dawn, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE ORGANIZED EXPLOITER (INSTRUMENT ENGINEER)
constraint_indexing:constraint_classification(gravitational_lensing_cosmic_dawn, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE UNIVERSAL SUBJECT (ANY PHYSICAL AGENT)
constraint_indexing:constraint_classification(gravitational_lensing_cosmic_dawn, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gravitational_lensing_cosmic_dawn_tests).

test(invariance, [nondet]) :-
    % For a Mountain constraint, there should be no perspectival gap.
    % All perspectives should classify it as a Mountain.
    constraint_indexing:constraint_classification(gravitational_lensing_cosmic_dawn, Type1, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(gravitational_lensing_cosmic_dawn, Type2, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gravitational_lensing_cosmic_dawn, Type3, context(agent_power(organized), _, _, _)),
    Type1 == mountain,
    Type2 == mountain,
    Type3 == mountain.

test(natural_law_profile_adherence) :-
    narrative_ontology:constraint_metric(gravitational_lensing_cosmic_dawn, extractiveness, E), E =< 0.25,
    narrative_ontology:constraint_metric(gravitational_lensing_cosmic_dawn, suppression_requirement, S), S =< 0.05,
    narrative_ontology:constraint_metric(gravitational_lensing_cosmic_dawn, accessibility_collapse, AC), AC >= 0.85,
    narrative_ontology:constraint_metric(gravitational_lensing_cosmic_dawn, resistance, R), R =< 0.15,
    domain_priors:emerges_naturally(gravitational_lensing_cosmic_dawn).

:- end_tests(gravitational_lensing_cosmic_dawn_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This constraint models a fundamental law of physics as described by
 *   General Relativity. As such, it is a canonical Mountain.
 *   - Base Extractiveness (ε=0.02): The law itself is non-extractive. The minuscule
 *     value represents the abstract "cost" of information being distorted, though
 *     this distortion is precisely what enables observation.
 *   - Suppression (S=0.01): The constraint suppresses nothing; on the contrary,
 *     it enables observation of otherwise inaccessible phenomena.
 *   - Natural Law Profile: The scores for accessibility_collapse (0.98) and
 *     resistance (0.01) reflect its status as a non-negotiable, universally
 *     accepted physical law. There is no viable alternative for this type of
 *     observation, and no meaningful resistance to the theory. The
 *     `emerges_naturally` flag is critical for the Mountain classification gate.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. This is a defining feature of a Mountain
 *   constraint. The laws of gravity are invariant for cosmologists, engineers,
 *   and laypersons. Its classification as a Mountain is stable across all
 *   possible indices of Power, Time, Exit, and Scope.
 *
 * DIRECTIONALITY LOGIC:
 *   `constraint_beneficiary` and `constraint_victim` declarations are omitted
 *   because they are inapplicable to a natural law. While astronomers *use* the
 *   law to their professional benefit, this is not a structural benefit derived
 *   from extraction. The system's directionality engine will fall back to
 *   canonical `d` values for each power atom. Because ε is extremely low,
 *   the resulting effective extraction χ = ε × f(d) × σ(S) will remain
 *   negligible for all agents, correctly locking in the Mountain classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies a physical law as an unchangeable
 *   Mountain. It avoids mischaracterizing it as a human-designed system. For
 *   example, it is not a Rope, because it isn't a convention for coordination.
 *   It is not a Snare, because it extracts nothing. This distinguishes the
 *   physical law (the Mountain) from the human institutions built to leverage it
 *   (e.g., funding for JWST, which would be a separate Rope or Tangled Rope).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_gravitational_lensing_cosmic_dawn,
    'Is General Relativity a complete description of gravity, or do quantum gravity or dark energy effects introduce subtle deviations in lensing at extreme distances?',
    'Precision measurements of multiple lensed systems at various redshifts, looking for systematic deviations from GR predictions.',
    'If GR is incomplete, the constraint is still a Mountain, but its precise mathematical form would change. If GR holds, our understanding of this cosmic-scale Mountain is confirmed.',
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(gravitational_lensing_cosmic_dawn, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% As a physical law, this constraint does not drift over human or civilizational
% timescales. The measurements are constant, reflecting its stability. This
% data is included to demonstrate its unchanging nature.
narrative_ontology:measurement(glcd_tr_t0, gravitational_lensing_cosmic_dawn, theater_ratio, 0, 0.0).
narrative_ontology:measurement(glcd_tr_t5, gravitational_lensing_cosmic_dawn, theater_ratio, 5, 0.0).
narrative_ontology:measurement(glcd_tr_t10, gravitational_lensing_cosmic_dawn, theater_ratio, 10, 0.0).

narrative_ontology:measurement(glcd_ex_t0, gravitational_lensing_cosmic_dawn, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(glcd_ex_t5, gravitational_lensing_cosmic_dawn, base_extractiveness, 5, 0.02).
narrative_ontology:measurement(glcd_ex_t10, gravitational_lensing_cosmic_dawn, base_extractiveness, 10, 0.02).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Network relationships (structural influence edges)
% Gravitational lensing is a fundamental tool that affects our understanding of
% other major cosmological constraints.
narrative_ontology:affects_constraint(gravitational_lensing_cosmic_dawn, dark_matter_distribution).
narrative_ontology:affects_constraint(gravitational_lensing_cosmic_dawn, hubble_constant_tension).
narrative_ontology:affects_constraint(gravitational_lensing_cosmic_dawn, early_universe_star_formation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary. The structural data (or lack thereof, for a
% Mountain) and the extremely low base extractiveness are sufficient for the
% engine to derive the correct, invariant classification.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */