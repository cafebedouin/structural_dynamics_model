% ============================================================================
% CONSTRAINT STORY: sorites_paradox
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_sorites_paradox, []).

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
 *   constraint_id: sorites_paradox
 *   human_readable: The Sorites Paradox (Application of Legal Cutoffs)
 *   domain: legal/social
 *
 * SUMMARY:
 *   This constraint models the *application* of arbitrary sharp boundaries
 *   (legal cutoffs) to solve the problem of vague predicates (the Sorites
 *   Paradox). The paradox itself (the logical problem of vagueness) could be
 *   modeled as a separate Mountain constraint. This story focuses on the
 *   social tool created to manage it: a legal or bureaucratic rule that
 *   defines a hard line where reality is continuous (e.g., age of majority,
 *   income tax brackets, blood alcohol limits). This tool serves a coordination
 *   function but creates severe, asymmetric extraction for those near the line.
 *
 * KEY AGENTS (by structural relationship):
 *   - Individuals at cutoff margins: Primary target (powerless/trapped) — bears the full cost of the arbitrary line, where a marginal difference results in a total change of legal status (e.g., eligible vs. ineligible, legal vs. criminal).
 *   - Bureaucratic/Legal Systems: Primary beneficiary (institutional/arbitrage) — benefits from the certainty and administrative efficiency of a clear, enforceable rule, which simplifies decision-making at scale.
 *   - Analytical Observer: Sees the full structure as a Tangled Rope — a necessary coordination tool that is inherently extractive and coercive.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: Moderate (0.40). The system of cutoffs extracts fairness and
% opportunity from those at the margins. It forces a cost on society to
% maintain arbitrary lines, which disproportionately harms those who fall
% just on the wrong side of a threshold.
domain_priors:base_extractiveness(sorites_paradox, 0.40).

% Rationale: High (0.70). The constraint actively suppresses "continuous truth"
% or fuzzy judgment. Legal systems must ignore the reality of gradual change
% to maintain the binary logic required for scalable law and software.
% Alternatives (like degree-based justice) are actively suppressed due to
% complexity.
domain_priors:suppression_score(sorites_paradox, 0.70).
domain_priors:theater_ratio(sorites_paradox, 0.10).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sorites_paradox, extractiveness, 0.40).
narrative_ontology:constraint_metric(sorites_paradox, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(sorites_paradox, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(sorites_paradox, tangled_rope).

% --- Binary flags ---
% Required for Tangled Rope. The legal cutoffs (e.g., age 18, $40k income)
% do not emerge naturally; they require constant definition, legislation,
% and judicial enforcement.
domain_priors:requires_active_enforcement(sorites_paradox).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(sorites_paradox, legal_bureaucracies).
narrative_ontology:constraint_beneficiary(sorites_paradox, software_engineers_implementing_rules).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(sorites_paradox, individuals_at_cutoff_margins).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE INDIVIDUAL AT THE MARGIN (SNARE)
% Agent who bears the most extraction. For the person who misses a cutoff
% by a single day or dollar, the arbitrary line is a Snare. It extracts their
% future, mobility, or quality of life based on a distinction that has no
% material reality.
constraint_indexing:constraint_classification(sorites_paradox, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BUREAUCRATIC SYSTEM (ROPE)
% Agent who benefits most. For the institution, the arbitrary line is a Rope.
% It creates a clear coordination mechanism that pulls the system toward
% certainty and administrative scale, enabling efficient governance by ignoring
% the "heap" of continuous reality.
constraint_indexing:constraint_classification(sorites_paradox, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the genuine coordination function (Rope for the
% institution) and the severe asymmetric extraction (Snare for the victim).
% The combination of these, requiring active enforcement, is a Tangled Rope.
constraint_indexing:constraint_classification(sorites_paradox, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sorites_paradox_tests).

test(perspectival_gap) :-
    % Verify the core perspectival gap: Snare for powerless, Rope for institutional.
    constraint_indexing:constraint_classification(sorites_paradox, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sorites_paradox, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(sorites_paradox, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements) :-
    % Verify that all structural requirements for a Tangled Rope are met.
    domain_priors:requires_active_enforcement(sorites_paradox),
    narrative_ontology:constraint_beneficiary(sorites_paradox, _),
    narrative_ontology:constraint_victim(sorites_paradox, _).

test(tangled_rope_metric_thresholds) :-
    % Verify metrics are within the canonical range for a Tangled Rope.
    narrative_ontology:constraint_metric(sorites_paradox, extractiveness, E),
    narrative_ontology:constraint_metric(sorites_paradox, suppression_requirement, S),
    E >= 0.30,
    S >= 0.40.

:- end_tests(sorites_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The original file misidentified this constraint as a Mountain, leading to
 *   linter errors because the metrics (ε=0.4, S=0.7) described a high-extraction,
 *   high-suppression system. This regeneration corrects the error by reframing
 *   the story to be about the *application* of legal cutoffs, which is a
 *   Tangled Rope, not the logical paradox itself. The metrics are appropriate
 *   for a system that provides real coordination but at a high coercive cost.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the legal system (beneficiary), the cutoff is a
 *   pure coordination tool (Rope) that enables governance at scale. For the
 *   individual at the margin (victim), that same line is a coercive trap
 *   (Snare) that invalidates their real-world status based on an infinitesimal
 *   difference. The analytical view, seeing both functions, correctly
 *   identifies it as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `legal_bureaucracies` and `software_engineers_implementing_rules`
 *     gain certainty, efficiency, and reduced cognitive load. The constraint
 *     subsidizes their operations.
 *   - Victims: `individuals_at_cutoff_margins` bear the direct cost. Their
 *     reality is continuous, but the system's binary logic extracts opportunity,
 *     rights, or status from them. This clear victim/beneficiary structure
 *     drives the perspectival gap via the directionality function f(d).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies that the system of legal cutoffs
 *   is not pure extraction (a Snare) because it has a genuine, indispensable
 *   coordination function for large-scale societies. It is also not pure
 *   coordination (a Rope) because the costs are borne asymmetrically and
 *   coercively. The Tangled Rope classification captures this necessary but
 *   dangerous duality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_sorites_paradox,
    "Can 'Fuzzy Logic' or probabilistic systems replace binary cutoffs at scale, or is binary decision-making a fundamental requirement (a Mountain) of social efficiency?",
    "Monitor the adoption of non-binary decision algorithms in high-stakes legal or insurance adjudication over a 20-year period.",
    "If fuzzy logic is viable, this constraint is a remediable Tangled Rope. If not, it is a permanent feature of governance, bordering on a Mountain of social physics.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(sorites_paradox, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extractiveness is 0.40, which is below the 0.46 threshold for
% mandatory temporal data. No measurement/5 facts are required.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The use of legal cutoffs is a mechanism for enforcing rules and allocating
% resources (like benefits or penalties).
narrative_ontology:coordination_type(sorites_paradox, enforcement_mechanism).

% --- Network Decomposition (Constraint Families) ---
%
% DUAL FORMULATION NOTE:
% This constraint story models the *application* of legal cutoffs (ε=0.40, Tangled Rope).
% It is part of a constraint family decomposed from the general concept of "The Sorites Paradox".
% A separate, related story could model the underlying logical paradox.
% Related stories:
%   - sorites_paradox_logical (ε≈0.05, Mountain): The unchangeable feature of
%     language that vague predicates lack sharp boundaries.
%
% narrative_ontology:affects_constraint(sorites_paradox_logical, sorites_paradox).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed. The standard derivation from beneficiary/victim
% declarations and exit options accurately models the directionality for
% the agents in this scenario.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */