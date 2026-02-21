% ============================================================================
% CONSTRAINT STORY: geocentric_cosmology
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_geocentric_cosmology, []).

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
 *   constraint_id: geocentric_cosmology
 *   human_readable: The Geocentric Cosmological Model (as embodied by the Antikythera Mechanism)
 *   domain: technological
 *
 * SUMMARY:
 *   The Antikythera Mechanism is a physical instantiation of the geocentric
 *   cosmological model prevalent in Hellenistic Greece. This model served as a
 *   powerful coordination device for astronomical prediction, calendar-making,
 *   and religious festivals. However, its fundamental inaccuracy and the
 *   institutional weight behind it suppressed alternative (heliocentric)
 *   models for over 1,500 years, representing a significant cognitive and
 *   scientific extraction from future generations.
 *
 * KEY AGENTS (by structural relationship):
 *   - Future Scholars & Alternative Cosmologists: Primary target (powerless/trapped) — bears the cognitive extraction of a flawed paradigm.
 *   - Hellenistic Astronomical Elite: Primary beneficiary (institutional/arbitrage) — benefits from the model's predictive power and social status.
 *   - Modern Historian of Science: Analytical observer — sees the full dual function of coordination and suppression.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geocentric_cosmology, 0.55). % Cognitive opportunity cost of a flawed but dominant paradigm.
domain_priors:suppression_score(geocentric_cosmology, 0.75).   % Structural property (raw, unscaled). Very high suppression of alternatives.
domain_priors:theater_ratio(geocentric_cosmology, 0.10).       % Piton detection (>= 0.70). Initially very low; not primarily performative.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geocentric_cosmology, extractiveness, 0.55).
narrative_ontology:constraint_metric(geocentric_cosmology, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(geocentric_cosmology, theater_ratio, 0.10).

% --- NL Profile Metrics (required for mountain constraints) ---
% This is a human-constructed model, not a natural law. These are not applicable.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(geocentric_cosmology, tangled_rope).

% --- Binary flags ---
% No sunset clause.
domain_priors:requires_active_enforcement(geocentric_cosmology). % Required for Tangled Rope. The paradigm required active intellectual and institutional defense.

% --- Emergence flag (required for mountain constraints) ---
% Does not emerge naturally; it is a human-constructed model.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(geocentric_cosmology, hellenistic_astronomical_elite).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(geocentric_cosmology, future_scholars_and_alternative_cosmologists).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three) -> MET
%   Scaffold:     beneficiary + (has_sunset_clause OR no enforcement) -> NOT MET
%   Snare:        victim required; beneficiary optional -> MET

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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Future scholars trapped within the paradigm, unable to pursue alternatives.
% The cost is the 1500-year delay in the scientific revolution.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ.
constraint_indexing:constraint_classification(geocentric_cosmology, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The Hellenistic elite who designed and used tools like the Antikythera Mechanism.
% For them, it's a powerful coordination device for calendars and predictions.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ.
constraint_indexing:constraint_classification(geocentric_cosmology, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% A modern historian of science sees both functions: the genuine coordination
% and the severe, long-term extraction. The high ε and suppression, combined
% with both beneficiary and victim groups, points to a Tangled Rope.
% This matches the constraint_claim.
constraint_indexing:constraint_classification(geocentric_cosmology, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geocentric_cosmology_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target (Snare) and beneficiary (Rope).
    constraint_indexing:constraint_classification(geocentric_cosmology, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(geocentric_cosmology, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    format('Perspectival gap validated: Target sees Snare, Beneficiary sees Rope.~n').

test(tangled_rope_conditions_met) :-
    % A Tangled Rope requires a beneficiary, victim, and active enforcement.
    narrative_ontology:constraint_beneficiary(geocentric_cosmology, _),
    narrative_ontology:constraint_victim(geocentric_cosmology, _),
    domain_priors:requires_active_enforcement(geocentric_cosmology),
    format('Tangled Rope structural conditions are met.~n').

test(analytical_view_matches_claim) :-
    narrative_ontology:constraint_claim(geocentric_cosmology, ClaimType),
    constraint_indexing:constraint_classification(geocentric_cosmology, ClaimType, context(agent_power(analytical), _, _, _)),
    format('Analytical view classification matches the declared constraint claim.~n').

:- end_tests(geocentric_cosmology_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): This is high because the constraint isn't
 *     extracting wealth, but something more fundamental: cognitive opportunity.
 *     The cost of enforcing a flawed paradigm for 1.5 millennia is immense,
 *     measured in delayed scientific progress.
 *   - Suppression (0.75): The model achieved near-total dominance, first
 *     through the authority of classical philosophers (Aristotle, Ptolemy) and
 *     later through its integration into Church doctrine, making alternatives
 *     not just unpopular but heretical.
 *   - Type: The constraint is a quintessential Tangled Rope because it possessed
 *     a genuine, powerful coordination function (the 'Rope') for its time,
 *     while simultaneously imposing a severe, asymmetric cost (the 'Snare') on
 *     scientific inquiry.
 *
 * PERSPECTIVAL GAP:
 *   - The Hellenistic elite (Beneficiary) experience the constraint as a pure
 *     Rope. It solves their immediate coordination problems (calendars, navigation,
 *     rituals) with immense effectiveness and predictive power relative to
 *     alternatives available at the time. The long-term extractive cost is
 *     invisible to them.
 *   - Future scholars and proponents of alternative models (Victims) experience
 *     it as a Snare. They are trapped by dogma and institutional inertia,
 *     unable to pursue more accurate models without immense personal and
 *     professional risk. For them, the coordination function is irrelevant;
 *     the suppression is everything.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `hellenistic_astronomical_elite`. They benefit from the
 *     predictive power, the intellectual framework, and the social status
 *     conferred by mastering this complex knowledge.
 *   - Victim: `future_scholars_and_alternative_cosmologists`. They bear the
 *     cost of a scientific dead-end, their intellectual freedom constrained by
 *     the weight of established authority. The derivation of high directionality
 *     (d ≈ 0.95) for this group reflects their trapped status.
 *
 * MANDATROPHY ANALYSIS:
 *   This case powerfully demonstrates the need for the Tangled Rope category.
 *   A simplistic analysis might label the geocentric model a pure Snare ("it
 *   was wrong and held back science") or a pure Rope ("it was a brilliant
*    coordination tool for its era"). Both are incomplete. The Tangled Rope
 *   classification correctly identifies that it was *both simultaneously*,
 *   preventing the mischaracterization of a functional system as pure extraction
 *   or an extractive system as pure coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_geocentric_cosmology,
    'To what extent was the geocentric model''s longevity a result of its functional coordination vs. active institutional suppression of alternatives?',
    'Detailed historiographical analysis comparing the model''s predictive failures against the timeline of institutional enforcement actions (e.g., the trial of Galileo).',
    'If due to function, it''s a slowly degrading Rope. If due to suppression, its Snare-like properties intensified much earlier.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(geocentric_cosmology, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint (ε=0.55 > 0.46), so temporal
% measurements are required to track its lifecycle drift. The model became
% more extractive over time as contradicting evidence mounted, making adherence
% to it a progressively worse "deal".

% Theater ratio over time (remains low as the model was always functional, not performative):
narrative_ontology:measurement(gcosmo_tr_t0, geocentric_cosmology, theater_ratio, 0, 0.05).
narrative_ontology:measurement(gcosmo_tr_t5, geocentric_cosmology, theater_ratio, 5, 0.08).
narrative_ontology:measurement(gcosmo_tr_t10, geocentric_cosmology, theater_ratio, 10, 0.10).

% Extraction over time (extraction accumulates as the model's flaws become more apparent):
narrative_ontology:measurement(gcosmo_ex_t0, geocentric_cosmology, base_extractiveness, 0, 0.25). % Initially, a very good approximation.
narrative_ontology:measurement(gcosmo_ex_t5, geocentric_cosmology, base_extractiveness, 5, 0.45). % Mid-period, epicycles are needed to patch accumulating errors.
narrative_ontology:measurement(gcosmo_ex_t10, geocentric_cosmology, base_extractiveness, 10, 0.55).% By the end, the model is a complex, costly fix for a bad premise.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The model provided a shared standard for astronomical data and prediction.
narrative_ontology:coordination_type(geocentric_cosmology, information_standard).

% Network relationships
% The geocentric model was foundational to many later theological and
% philosophical constraints.
narrative_ontology:affects_constraint(geocentric_cosmology, scholastic_theology).
narrative_ontology:affects_constraint(geocentric_cosmology, doctrine_of_divine_providence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% The standard derivation from beneficiary/victim groups and exit options
% accurately models the dynamics of this constraint. No overrides are necessary.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */