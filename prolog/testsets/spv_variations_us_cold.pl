% ============================================================================
% CONSTRAINT STORY: spv_variations_us_cold
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-08-01
% ============================================================================

:- module(constraint_spv_variations_us_cold, []).

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
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: spv_variations_us_cold
 *   human_readable: Stratospheric Polar Vortex Variations (US Cold Outbreaks)
 *   domain: atmospheric_science
 *
 * SUMMARY:
 *   This constraint describes the atmospheric dynamics of the Stratospheric
 *   Polar Vortex (SPV) that lead to extreme cold-air outbreaks (CAOs) in the
 *   continental US. The story captures a critical duality: the underlying
 *   physical phenomenon is an unchangeable natural law (Mountain), while the
 *   human-developed classification system (e.g., P-clusters) used to predict
 *   its behavior functions as a pure coordination mechanism (Rope) for meteorologists.
 *
 * KEY AGENTS (by structural relationship):
 *   - US Residents in CAO paths: Primary target (powerless/trapped) — experiences the physical phenomenon as an unchangeable fact.
 *   - Meteorological Forecasters: Primary beneficiary (institutional/arbitrage) — uses the predictive model to coordinate responses.
 *   - Atmospheric Scientists: Analytical observer — studies both the physical law and the predictive models.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Extraction is negligible (0.02) as this is a natural law.
% Suppression is set to the Mountain ceiling (0.05) as there are no alternatives to the laws of physics.
domain_priors:base_extractiveness(spv_variations_us_cold, 0.02).
domain_priors:suppression_score(spv_variations_us_cold, 0.05).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(spv_variations_us_cold, 0.05).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(spv_variations_us_cold, extractiveness, 0.02).
narrative_ontology:constraint_metric(spv_variations_us_cold, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(spv_variations_us_cold, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain.
% Accessibility collapse is 1.0 as physical laws offer no alternatives.
% Resistance is 0.0 as one cannot meaningfully resist atmospheric physics.
narrative_ontology:constraint_metric(spv_variations_us_cold, accessibility_collapse, 1.0).
narrative_ontology:constraint_metric(spv_variations_us_cold, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
% The claim is 'rope', reflecting that the primary human-interaction layer is the
% coordination model built on top of the physical law.
narrative_ontology:constraint_claim(spv_variations_us_cold, rope).

% --- Emergence flag (required for mountain constraints) ---
% The stratospheric polar vortex is a natural atmospheric phenomenon
% governed by physical laws — it emerges naturally without human design.
domain_priors:emerges_naturally(spv_variations_us_cold).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing? (i.e., the predictive model)
narrative_ontology:constraint_beneficiary(spv_variations_us_cold, meteorological_forecasters).
%
% Who bears disproportionate cost? (i.e., from the physical phenomenon)
narrative_ontology:constraint_victim(spv_variations_us_cold, us_residents_in_cao_paths).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function.
   The engine derives d from beneficiary/victim membership + exit_options.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE RESIDENT IN A COLD OUTBREAK (MOUNTAIN)
% For a person experiencing extreme cold, the SPV's behavior is an unchangeable
% physical limit of their environment. The low extraction and suppression scores
% confirm this Mountain classification.
constraint_indexing:constraint_classification(spv_variations_us_cold, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE METEOROLOGICAL FORECASTER (ROPE)
% For forecasters, the P-cluster classification system is a pure coordination
% tool (Rope). It allows them to standardize predictions and coordinate warnings
% and resource allocation.
constraint_indexing:constraint_classification(spv_variations_us_cold, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (ROPE)
% The analytical perspective aligns with the constraint_claim, focusing on the
% human-interactive layer (the model) which is a Rope. It acknowledges the
% underlying Mountain but classifies the system by its function.
constraint_indexing:constraint_classification(spv_variations_us_cold, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(spv_variations_us_cold_tests).

test(perspectival_gap) :-
    % Verify the gap: Mountain for the target, Rope for the beneficiary.
    constraint_indexing:constraint_classification(spv_variations_us_cold, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(spv_variations_us_cold, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary,
    TypeTarget == mountain,
    TypeBeneficiary == rope.

test(threshold_validation) :-
    % Verify metrics are within the valid Mountain range.
    domain_priors:base_extractiveness(spv_variations_us_cold, E),
    domain_priors:suppression_score(spv_variations_us_cold, S),
    E =< 0.25,
    S =< 0.05.

:- end_tests(spv_variations_us_cold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This story models a physical phenomenon (SPV) and the human coordination
 *   system built to predict it. The base metrics (ε=0.02, suppression=0.05) are
 *   set to reflect the underlying natural law, placing them firmly in the
 *   Mountain category. This is crucial for the powerless perspective. The
 *   `constraint_claim` is 'rope' because the primary object of analysis from a
 *   systems perspective is the human-made predictive model, which is a pure
 *   coordination tool.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark and illustrative. For a resident trapped in a cold-air
 *   outbreak, the event is an unchangeable fact of nature (Mountain). For a
 *   forecaster, the system for predicting that event is a coordination tool
 *   (Rope) that enables planning and mitigation. The system correctly captures
 *   that the same underlying phenomenon generates different constraint types
 *   depending on the agent's structural relationship to it.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `meteorological_forecasters` benefit from the predictive
 *     model, which functions as an information standard. This justifies the
 *     `rope` classification from their perspective.
 *   - Victim: `us_residents_in_cao_paths` bear the costs of the physical
 *     phenomenon itself. Their relationship is with the natural law, not the
 *     model, justifying the `mountain` classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This story resolves a potential misclassification flagged by the linter as
 *   'SCAFFOLD_DANGER_ZONE'. A low-extraction, non-enforced coordination
 *   mechanism can be mistaken for a Scaffold by the engine. By correctly
 *   classifying the powerless perspective as a Mountain (which is narratively
 *   justified), the file signals the presence of a natural law component,
 *   allowing the linter and engine to distinguish this stable Rope from a
 *   temporary Scaffold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_spv_enso_link,
    'Does La Niña directly drive P2 variability or is P2 a response to complex tropospheric ENSO influences?',
    'Longer-term reanalysis (beyond 41 years) and high-resolution climate modeling.',
    'Direct Link = Higher predictability of NWUS cold; Complex Response = Lower confidence in ratio shifts.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
% The reporting engine reads this fact. This is an empirical question resolvable by more data.
narrative_ontology:omega_variable(omega_spv_enso_link, empirical, 'Uncertainty about the direct causal link between La Niña and P2 variability.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(spv_variations_us_cold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required for low-extraction constraints (base_extractiveness <= 0.46).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The P-cluster system is an information standard for interpreting atmospheric data.
narrative_ontology:coordination_type(spv_variations_us_cold, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed. The structural derivation from beneficiary/victim
% declarations correctly models the relationships for this constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */