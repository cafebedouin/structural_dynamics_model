% ============================================================================
% CONSTRAINT STORY: empty_tomb_transformation
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_empty_tomb_transformation, []).

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
    constraint_indexing:directionality_override/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: empty_tomb_transformation
 *   human_readable: The Resurrection Cycle (Empty Tombs)
 *   domain: religious/social/psychological
 *
 * SUMMARY:
 *   Life is framed as a constant series of dying to old selves and being reborn
 *   into new lives. Each life stage or "chapter" concludes with a "tomb of
 *   transition" that must be exited by "rolling back the stone" of the past
 *   to emerge transformed. The constraint is the requirement for this "death"
 *   to precede "new life".
 *
 * KEY AGENTS (by structural relationship):
 *   - The Transitioning Subject: Primary target (powerless/trapped) — bears the extraction of their old identity.
 *   - The Resurrected Self: Primary beneficiary (moderate/mobile) — benefits from the transformation into a new life.
 *   - The Liturgical Institution: Secondary beneficiary (institutional/arbitrage) — benefits by providing the coordinating narrative.
 *   - The Pastor/Witness: Analytical observer — sees the full structure as a repeating cycle.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: Moderate (0.4). The process extracts the "old life" and involves "facing our own suffering".
% While generative of "new life," the transition phase is a temporary liquidation of the self.
domain_priors:base_extractiveness(empty_tomb_transformation, 0.4).
% Rationale: Moderate (0.4). The "tomb" phase suppresses the visibility of the new life.
% The institutional framing can also suppress alternative, secular models of personal growth.
domain_priors:suppression_score(empty_tomb_transformation, 0.4).
% Rationale: Low theater. The constraint describes a substantive psychological process,
% not a performative one.
domain_priors:theater_ratio(empty_tomb_transformation, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(empty_tomb_transformation, extractiveness, 0.4).
narrative_ontology:constraint_metric(empty_tomb_transformation, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(empty_tomb_transformation, theater_ratio, 0.08).

% --- Constraint claim (must match analytical perspective type) ---
% Rationale: The constraint has a genuine coordination function (helping people navigate
% life transitions) but also involves asymmetric extraction (the death of the old self).
% It requires active framing/enforcement by an institution to maintain its meaning.
% This combination of coordination, extraction, and enforcement is characteristic of a Tangled Rope.
narrative_ontology:constraint_claim(empty_tomb_transformation, tangled_rope).

% --- Binary flags ---
% Rationale: The institutional framing of the cycle as a core religious doctrine
% constitutes a form of active enforcement of this psychological model.
domain_priors:requires_active_enforcement(empty_tomb_transformation).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(empty_tomb_transformation, the_resurrected_self).
narrative_ontology:constraint_beneficiary(empty_tomb_transformation, liturgical_institutions).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(empty_tomb_transformation, the_static_ego).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE STUCK INDIVIDUAL (SNARE)
% For the individual unable to emerge, the cycle is a Snare. The "tomb of
% transition" becomes a static trap where life has ended but rebirth has not
% begun, extracting the agent's hope and agency.
constraint_indexing:constraint_classification(empty_tomb_transformation, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE REBORN AGENT (ROPE)
% For the agent who successfully transitions, the cycle is a Rope. It is a
% functional coordination mechanism for growth, allowing them to use past
% "deaths" as a record of transformation that informs their new life.
constraint_indexing:constraint_classification(empty_tomb_transformation, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: THE LITURGICAL INSTITUTION (ROPE)
% For the institution, the story is a Rope—a tool used to coordinate the
% community's response to suffering and change, transforming a "fairy tale"
% into a functional map for resilience.
constraint_indexing:constraint_classification(empty_tomb_transformation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analytical observer sees the full structure: a coordination mechanism
% (Rope for the reborn) that relies on asymmetric extraction (Snare for the stuck)
% and requires active institutional framing. This hybrid nature is a Tangled Rope.
% The "Mountain" view of the pastor is a valid perspective, but not the complete
% analytical picture which must account for the extraction.
constraint_indexing:constraint_classification(empty_tomb_transformation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(empty_tomb_transformation_tests).

test(perspectival_gap) :-
    % Verify that the stuck individual (powerless) and the institution see different realities.
    constraint_indexing:constraint_classification(empty_tomb_transformation, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(empty_tomb_transformation, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(empty_tomb_transformation, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    TypeTarget == snare,
    TypeBeneficiary == rope,
    TypeAnalytical == tangled_rope.

test(threshold_validation_for_tangled_rope) :-
    % Verify metrics are consistent with a Tangled Rope classification.
    narrative_ontology:constraint_metric(empty_tomb_transformation, extractiveness, E),
    narrative_ontology:constraint_metric(empty_tomb_transformation, suppression_requirement, S),
    E >= 0.30,
    S >= 0.40.

:- end_tests(empty_tomb_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base scores (ε=0.4, S=0.4) reflect the significant psychological cost
 *   ("death" of the old self) and the suppression of alternative models of
 *   growth required by this framing. The analytical classification is
 *   Tangled Rope because the constraint possesses both a genuine coordination
 *   function (a map for resilience) and a clear mechanism of asymmetric
 *   extraction (the suffering of the transition phase).
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the powerless individual stuck in transition, it's
 *   a Snare of despair. For the institution providing the narrative, it's a
 *   coordinating Rope. For the analytical observer, it's a Tangled Rope that
 *   contains both truths. The pastor's view of it as a "Mountain" (an
 *   unchangeable law) is a valid perspective on the inevitability of change,
 *   but this view elides the extractive and coercive elements seen from other
 *   indices.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: 'the_resurrected_self' benefits by achieving a new state of
 *     being. 'liturgical_institutions' benefit by having their central narrative
 *     validated and used as a social coordination tool.
 *   - Victim: 'the_static_ego' or the old self is the entity that must be
 *     "liquidated" or "left behind," bearing the full cost of the transformation.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope prevents two errors. It avoids mislabeling
 *   it as a pure Mountain, which would ignore the real suffering (extraction)
 *   of those who get stuck. It also avoids mislabeling it as a pure Snare,
 *   which would ignore the genuine coordination benefit it provides to those
 *   who successfully navigate the cycle using the provided narrative map.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resurrection_agency,
    "Is the ability to 'roll back the stone' an internal choice (Rope) or a stochastic event that requires external grace (Mountain)?",
    "Longitudinal tracking of individuals in 'tombs of transition' with vs. without internal 'hope' metrics",
    "If choice: Rebirth is a Rope. If stochastic: It is a Mountain/Snare.",
    confidence_without_resolution(medium)
).

omega_variable(
    tomb_duration_determinism,
    "Is the length of the 'tomb of transition' fixed by the nature of the chapter (Mountain) or variable based on individual effort (Rope)?",
    "Comparison of transition times across diverse psychological 'deaths'",
    "If fixed: The tomb is a Mountain. If variable: It is a Rope.",
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(empty_tomb_transformation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extractiveness is not > 0.46, so temporal measurements are not required.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% This constraint functions as a standard for interpreting psychological change.
narrative_ontology:coordination_type(empty_tomb_transformation, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed; the structural derivation from beneficiary/victim
% declarations is sufficient to model the dynamics.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */