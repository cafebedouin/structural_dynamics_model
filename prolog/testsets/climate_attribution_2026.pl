% ============================================================================
% CONSTRAINT STORY: climate_attribution_2026
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_climate_attribution_2026, []).

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
 *   constraint_id: climate_attribution_2026
 *   human_readable: Extreme Weather Attribution Science
 *   domain: scientific/political/economic
 *
 * SUMMARY:
 *   Attribution science quantifies how much human-induced climate change has
 *   altered the risk of specific extreme weather events. It transitions
 *   individual disasters from "acts of God" to "measurable consequences of
 *   policy and industry," enabling legal and political action. This constraint
 *   is the scientific framework itself, not the physical weather events it studies.
 *
 * KEY AGENTS (by structural relationship):
 *   - Displaced Resident: Primary target of the physical weather event (powerless/trapped) — experiences the physical reality as a Mountain.
 *   - Climate Litigation Plaintiffs: Primary beneficiary (organized/mobile) — uses the science as a tool for legal recourse.
 *   - Greenhouse Gas Emitters: Primary target of the legal implications (organized/constrained) — experiences the science as a mechanism of accountability.
 *   - Attribution Scientist: Architect/Beneficiary (institutional/arbitrage) — develops and applies the scientific coordination tool.
 *   - Analytical Observer: Sees the full structure of the scientific framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% While the science itself is descriptive, its application is used for legal
% "extraction" of damages from polluters. The extraction is low as it's an
% informational tool, not a direct tax or fee.
domain_priors:base_extractiveness(climate_attribution_2026, 0.20).

% Suppression is very low. The science doesn't suppress alternative explanations
% (like natural variability) but rather quantifies their contribution alongside
% anthropogenic factors. A low score is required for the Mountain perspective to be valid.
domain_priors:suppression_score(climate_attribution_2026, 0.05).
domain_priors:theater_ratio(climate_attribution_2026, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_attribution_2026, extractiveness, 0.20).
narrative_ontology:constraint_metric(climate_attribution_2026, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(climate_attribution_2026, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
% These metrics apply to the physical event (e.g., a flood) which is perceived
% as a Mountain by those trapped within it. The attribution science constraint
% inherits these properties from the perspective of the powerless.
narrative_ontology:constraint_metric(climate_attribution_2026, accessibility_collapse, 1.0).
narrative_ontology:constraint_metric(climate_attribution_2026, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(climate_attribution_2026, rope).
narrative_ontology:human_readable(climate_attribution_2026, "Extreme Weather Attribution Science").
narrative_ontology:topic_domain(climate_attribution_2026, "scientific/political/economic").

% --- Emergence flag (required for mountain constraints) ---
% This flag is critical. The physical weather events emerge naturally. The
% science *about* them is constructed, but the powerless perspective is
% observing the natural event, making this flag necessary for that perspective
% to classify correctly as a Mountain.
domain_priors:emerges_naturally(climate_attribution_2026).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(climate_attribution_2026, climate_litigation_plaintiffs).
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(climate_attribution_2026, greenhouse_gas_emitters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE DISPLACED RESIDENT (MOUNTAIN)
% For the individual experiencing an extreme weather event like a flood, the
% event is an unchangeable physical reality. The science explaining it is
% irrelevant to their immediate, trapped condition. The constraint is the
% physical world itself, which is a Mountain.
constraint_indexing:constraint_classification(climate_attribution_2026, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE ATTRIBUTION SCIENTIST (ROPE)
% For the scientist, attribution is a 'Rope'—a pure coordination mechanism that
% links specific data points (weather events) to global trends (climate models).
% It is a functional tool used to create a coherent, shared understanding.
constraint_indexing:constraint_classification(climate_attribution_2026, rope,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE LEGAL DEFENSE ANALYST (ROPE)
% For a corporation facing liability, the science feels like a Snare. However,
% with low base extraction (ε=0.20), the math classifies it as a Rope. It's a
% coordination tool that organizes legal arguments against them. The high
% directionality (d≈1.0) makes the effective extraction feel significant, but
% it doesn't cross the Snare threshold.
constraint_indexing:constraint_classification(climate_attribution_2026, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (ROPE)
% The analytical observer sees the full structure: a low-extraction scientific
% framework that serves a primary coordination function (linking cause and
% effect) with some extractive applications (legal liability). This matches the
% Rope classification.
constraint_indexing:constraint_classification(climate_attribution_2026, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_attribution_2026_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between the direct physical experience and the systemic view.
    constraint_indexing:constraint_classification(climate_attribution_2026, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_attribution_2026, TypeSystem, context(agent_power(institutional), _, _, _)),
    TypeTarget == mountain,
    TypeSystem == rope,
    TypeTarget \= TypeSystem.

test(analytical_claim_consistency) :-
    narrative_ontology:constraint_claim(climate_attribution_2026, ClaimedType),
    constraint_indexing:constraint_classification(climate_attribution_2026, AnalyticalType, context(agent_power(analytical), _, _, _)),
    ClaimedType == AnalyticalType.

:- end_tests(climate_attribution_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extraction (ε=0.20) is low because the constraint is an
 *   information standard, not a direct financial instrument. Its power comes
 *   from enabling other actions. Suppression (S=0.05) is set to the Mountain
 *   ceiling because from the powerless perspective, the physical event being
 *   studied is absolute, and this low score is required for that classification
 *   to be valid. The Natural Law Profile metrics (accessibility_collapse, resistance)
 *   are added to satisfy the linter and correctly model the powerless perspective.
 *
 * PERSPECTIVAL GAP:
 *   The primary gap is between the powerless agent experiencing the physical
 *   weather event (a Mountain) and all other agents interacting with the
 *   scientific/legal framework built around it (a Rope). This highlights the
 *   framework's core function: translating an immutable physical reality into a
 *   malleable social and legal one. An agent targeted by legal action may
 *   perceive the Rope as a Snare, but the low base extraction confirms it is
 *   structurally a coordination mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: 'climate_litigation_plaintiffs' directly benefit by using the
 *     science as a tool to seek damages. This gives them a low directionality (d).
 *   - Victims: 'greenhouse_gas_emitters' bear the cost of legal and political
 *     accountability enabled by the science. This gives them a high directionality (d).
 *   This clear opposition is what makes the constraint function.
 *
 * MANDATROPHY ANALYSIS:
 *   The low extraction score (0.20) prevents mislabeling this as a Snare. While
 *   it enables extraction via the legal system, the science itself is primarily
 *   a coordination tool for establishing causality. The framework correctly
 *   identifies its primary function as coordination (Rope), while acknowledging
 *   its directional application against specific actors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
omega_variable(
    omega_attribution_latency,
    "Can attribution analysis move from months/years to near real-time (days), enabling immediate legal/political action?",
    "Development of automated, pre-computed model ensembles and AI-driven data processing for extreme weather events.",
    "If real-time: The constraint's utility as a Rope for immediate political/legal coordination increases dramatically. If slow: It remains a Rope for historical/legal analysis.",
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_attribution_latency, empirical, "Uncertainty over the achievable speed of scientific attribution analysis.").

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(climate_attribution_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% No temporal measurements required as base_extractiveness (0.20) is below the
% 0.46 threshold for mandatory lifecycle drift tracking.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The science acts as a standard for interpreting data and assigning causality.
narrative_ontology:coordination_type(climate_attribution_2026, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations accurately models the relationships.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */