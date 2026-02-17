% ============================================================================
% CONSTRAINT STORY: lindy_effect
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_lindy_effect, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: lindy_effect
 *   human_readable: The Lindy Effect
 *   domain: social/intellectual
 *
 * SUMMARY:
 *   The Lindy Effect is a theorized phenomenon where the future life expectancy of
 *   non-perishable things (ideas, books, technologies) is proportional to their
 *   current age. It functions as a powerful heuristic for filtering information,
 *   but also creates a structural bias against novelty. This dual nature—a
 *   genuine coordination function (filtering) combined with asymmetric suppression
 *   of new entrants—makes it a canonical Tangled Rope.
 *
 * KEY AGENTS (by structural relationship):
 *   - Disruptive Innovators: Primary target (powerless/constrained) — bears the cost of being new.
 *   - Established Institutions: Primary beneficiary (institutional/arbitrage) — leverages its age as a signal of quality.
 *   - Pragmatic Investors: Secondary beneficiary (moderate/mobile) — uses the effect as a coordination heuristic.
 *   - Analytical Observer: Sees the full structure as a Tangled Rope.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: The effect doesn't actively steal value, but it does "extract"
% attention and opportunity from the new and gift it to the old based on age.
domain_priors:base_extractiveness(lindy_effect, 0.20).

% Rationale: The "Time-Tested" narrative actively suppresses new ideas by
% labeling them "fragile" or "unproven", creating a significant barrier to entry.
% This high suppression score is why it cannot be a Mountain.
domain_priors:suppression_score(lindy_effect, 0.50).

% Rationale: The effect is a substantive social dynamic, not a performative one.
domain_priors:theater_ratio(lindy_effect, 0.11).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lindy_effect, extractiveness, 0.20).
narrative_ontology:constraint_metric(lindy_effect, suppression_requirement, 0.50).
narrative_ontology:constraint_metric(lindy_effect, theater_ratio, 0.11).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(lindy_effect, tangled_rope).
narrative_ontology:human_readable(lindy_effect, "The Lindy Effect").

% --- Binary flags ---
% The effect is self-reinforcing through cumulative advantage and cognitive bias.
domain_priors:requires_active_enforcement(lindy_effect).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(lindy_effect, established_institutions).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(lindy_effect, disruptive_innovators).

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

% PERSPECTIVE 1: THE DISRUPTIVE STARTUP (SNARE)
% For a new innovator, the Lindy Effect is a Snare. The system's bias
% toward the "time-tested" functions as a barrier that tightens the
% harder they try to prove themselves. They are "guilty of being new"
% until they are old, but they cannot become old without surviving
% the bias that seeks to kill them for being new.
constraint_indexing:constraint_classification(lindy_effect, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: THE ESTABLISHED INSTITUTION (ROPE)
% For an established institution, the Lindy Effect is a Rope. It is a
% powerful mechanism for maintaining authority and relevance. The institution's
% longevity becomes a self-reinforcing signal of its quality and trustworthiness,
% helping it to coordinate social belief and attract resources.
constraint_indexing:constraint_classification(lindy_effect, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% For the statistician, the Lindy Effect is a Tangled Rope. It has a genuine
% coordination function (filtering by time), but this function is inseparable
% from the asymmetric suppression of new entrants (a Snare-like quality).
% The high suppression score (0.5) makes a Mountain classification impossible;
% it is a social, not physical, law.
constraint_indexing:constraint_classification(lindy_effect, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE PRAGMATIC INVESTOR (ROPE)
% For the investor, the Lindy Effect is a Rope. It is a coordination
% mechanism that allows them to distinguish "signal" from "noise." By
% prioritizing things that have already survived for 50 years, they use
% the constraint as a tether to reality and enduring value.
constraint_indexing:constraint_classification(lindy_effect, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lindy_effect_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target, beneficiary, and analyst.
    constraint_indexing:constraint_classification(lindy_effect, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lindy_effect, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(lindy_effect, TypeAnalyst, context(agent_power(analytical), _, _, _)),
    TypeTarget = snare,
    TypeBeneficiary = rope,
    TypeAnalyst = tangled_rope,
    TypeTarget \= TypeBeneficiary,
    TypeBeneficiary \= TypeAnalyst.

test(tangled_rope_structural_properties) :-
    % Verify that the structural conditions for a Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(lindy_effect, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(lindy_effect, _),     % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(lindy_effect).

:- end_tests(lindy_effect_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics were chosen to capture the dual nature of the Lindy Effect.
 *   Base extractiveness (ε=0.20) is low because the effect doesn't directly
 *   transfer material resources, but rather opportunity and attention. The key
 *   metric is suppression (0.50), which reflects the significant structural
 *   barrier faced by new ideas. This high suppression score is what makes it a
 *   Tangled Rope from an analytical view, not a Mountain. A true Mountain
 *   (like a law of physics) has near-zero suppression because alternatives are
 *   incoherent, not merely disadvantaged.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For established institutions (beneficiaries), the effect is
 *   a pure Rope, a simple tool for coordinating belief around their enduring
 *   value. For disruptive innovators (victims), it's a Snare, a self-tightening
 *   trap where the only way to gain legitimacy (age) is to survive a system
 *   biased against them for their lack of it. The analytical observer sees both
 *   functions simultaneously, classifying it as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `established_institutions` (universities, religions, classic texts)
 *     directly benefit as their age is converted into authority and trust.
 *   - Victim: `disruptive_innovators` (startups, new art movements, novel theories)
 *     bear the cost, as they must expend enormous energy to overcome the default
 *     skepticism reserved for the unproven.
 *   The engine derives a low `d` for institutions (negative χ) and a high `d` for
 *   innovators (high χ), correctly modeling the asymmetric dynamic.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two errors. First, it avoids calling the
 *   effect a pure Rope, which would ignore the severe disadvantage it imposes on
 *   newcomers. Second, it avoids calling it a pure Snare, which would ignore its
 *   genuine, non-trivial function as a heuristic for filtering noise in a complex
 *   world. The Tangled Rope classification acknowledges both the coordination
 *   and the extraction/suppression inherent in the structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_lindy_effect,
    'Does the speed of digital iteration (e.g., AI) break the Lindy Effect or simply create a "Compressed Lindy" cycle?',
    'Longitudinal study of software/meme survival vs. philosophical text survival in the 21st century.',
    'If it breaks, the effect degrades from a Tangled Rope to a Piton (a historical heuristic). If it compresses, the effect remains a powerful Tangled Rope, just on faster timescales.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_lindy_effect, empirical, 'Whether the Lindy Effect holds for rapidly iterating digital phenomena.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(lindy_effect, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extractiveness is low (< 0.46), so temporal measurements are not required
% by the linter for this constraint. The effect is considered structurally stable
% over its interval.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% The Lindy Effect acts as a filter on information, making it a standard.
narrative_ontology:coordination_type(lindy_effect, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed. The structural derivation from beneficiary/victim
% declarations and exit options accurately models the dynamics of this constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */