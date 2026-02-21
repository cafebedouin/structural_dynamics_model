% ============================================================================
% CONSTRAINT STORY: german_board_gender_quota
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_german_board_gender_quota, []).

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
 *   constraint_id: german_board_gender_quota
 *   human_readable: "German Gender Quota for Corporate Boards (FüPoG II)"
 *   domain: economic
 *
 * SUMMARY:
 *   This constraint models the German "Führungspositionen-Gesetz II" (FüPoG II),
 *   a law effective August 2021. It mandates that for large, publicly listed
 *   companies with co-determination, if the board has more than three members,
 *   at least one must be a woman upon new appointment. Non-compliance results
 *   in the appointment being legally void (the "empty chair" principle). The
 *   law is a direct intervention in corporate governance to increase gender
 *   diversity at the highest levels.
 *
 * KEY AGENTS (by structural relationship):
 *   - qualified_female_executives: Primary beneficiary (organized/mobile) — gains access to previously inaccessible positions.
 *   - passed_over_male_candidates: Primary target (powerless/trapped) — loses opportunities in a restricted selection pool.
 *   - affected_corporate_boards: Institutional target (institutional/constrained) — bears cost of reduced hiring autonomy.
 *   - allbright_foundation_analyst: Analytical observer — sees the full structure of coercion and coordination.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(german_board_gender_quota, 0.40). % Extracts autonomy from boards and opportunity from some candidates.
domain_priors:suppression_score(german_board_gender_quota, 0.85).   % Structural property (raw, unscaled). The "empty chair" rule makes alternatives legally impossible.
domain_priors:theater_ratio(german_board_gender_quota, 0.15).       % Piton detection (>= 0.70). The law is highly effective, not performative.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(german_board_gender_quota, extractiveness, 0.40).
narrative_ontology:constraint_metric(german_board_gender_quota, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(german_board_gender_quota, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
% Not a mountain constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(german_board_gender_quota, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(german_board_gender_quota). % Required for Tangled Rope. The "empty chair" principle is the enforcement.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
narrative_ontology:constraint_beneficiary(german_board_gender_quota, qualified_female_executives).
narrative_ontology:constraint_victim(german_board_gender_quota, affected_corporate_boards).
narrative_ontology:constraint_victim(german_board_gender_quota, passed_over_male_candidates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (A PASSED-OVER MALE CANDIDATE)
% This individual experiences the law as a direct barrier to an opportunity.
% From this perspective, the extraction of opportunity is salient.
% Engine derives victim + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42.
% χ = 0.40 * 1.42 * 1.0 (national) = 0.568. This is within the Tangled Rope range [0.40, 0.90].
% It's not a Snare because the extraction serves an explicit, broad coordination goal.
constraint_indexing:constraint_classification(german_board_gender_quota, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (QUALIFIED FEMALE EXECUTIVES)
% This group sees the law as a mechanism to overcome a coordination failure
% (homosocial reproduction on boards). For them, it is a pure Rope.
% Engine derives beneficiary + mobile exit → d ≈ 0.15 → f(d) ≈ -0.01.
% χ = 0.40 * -0.01 * 1.0 (national) = -0.004. This is a clear Rope.
constraint_indexing:constraint_classification(german_board_gender_quota, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (ALLBRIGHT FOUNDATION ANALYST)
% The analyst sees both the coordination function and the coercive, extractive
% nature of the enforcement mechanism, classifying it as a Tangled Rope.
% Engine derives analytical → d ≈ 0.72 → f(d) ≈ 1.15.
% χ = 0.40 * 1.15 * 1.2 (global) = 0.552. This is a clear Tangled Rope.
constraint_indexing:constraint_classification(german_board_gender_quota, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE INSTITUTIONAL TARGET (AFFECTED CORPORATE BOARD)
% The board is a powerful institution but is constrained by the law. It feels
% the extraction of its autonomy in hiring.
% Engine derives victim + constrained exit → d ≈ 0.8 -> f(d) ≈ 1.28
% χ = 0.40 * 1.28 * 1.0 (national) = 0.512. This is also a Tangled Rope.
constraint_indexing:constraint_classification(german_board_gender_quota, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(german_board_gender_quota_tests).

test(perspectival_gap_beneficiary_vs_target) :-
    constraint_indexing:constraint_classification(german_board_gender_quota, rope,
        context(agent_power(organized), _, _, _)),
    constraint_indexing:constraint_classification(german_board_gender_quota, tangled_rope,
        context(agent_power(powerless), _, _, _)),
    true.

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(german_board_gender_quota, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_gates_pass) :-
    domain_priors:base_extractiveness(german_board_gender_quota, E), E >= 0.30,
    domain_priors:suppression_score(german_board_gender_quota, S), S >= 0.40,
    narrative_ontology:constraint_beneficiary(german_board_gender_quota, _),
    narrative_ontology:constraint_victim(german_board_gender_quota, _),
    domain_priors:requires_active_enforcement(german_board_gender_quota).

:- end_tests(german_board_gender_quota_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.40): The law doesn't extract money, but it extracts
 *     a significant degree of autonomy from company boards in their core function
 *     of appointing leadership. It also extracts opportunity from a subset of
 *     potential candidates. The value is moderate but undeniable.
 *   - Suppression (S=0.85): The "empty chair" enforcement is absolute. There is
 *     no legal alternative for an affected company but to comply. This high
 *     suppression score reflects the coercive power of the law.
 *   - Classification: The constraint is a quintessential Tangled Rope. It has a
 *     clear, demonstrable coordination function (breaking up self-perpetuating
 *     networks to improve diversity) benefiting one group, and an equally clear,
 *     coercive, extractive function imposed on another group.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the beneficiaries (`qualified_female_executives`),
 *   the law is a Rope that solves a coordination problem, creating a fairer market.
 *   For those who bear the costs (`affected_corporate_boards`, `passed_over_male_candidates`),
 *   the coercive, extractive nature is far more salient, leading to a Tangled Rope
 *   classification. They experience the constraint's teeth directly. The analytical
 *   observer synthesizes both aspects and confirms the Tangled Rope classification.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `qualified_female_executives`. The law is explicitly designed
 *     to create opportunities for this group. Their directionality `d` is low.
 *   - Victims: `affected_corporate_boards` (lose autonomy) and `passed_over_male_candidates`
 *     (lose specific opportunities). Their directionality `d` is high, as the
 *     constraint extracts from them to achieve its goal. This mapping drives the
 *     perspectival gap in effective extraction (χ) and thus classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two potential errors.
 *   1. It is not a pure Rope: Acknowledging the high suppression and base
 *      extraction prevents mislabeling a coercive state mandate as a purely
 *      voluntary coordination mechanism.
 *   2. It is not a pure Snare: Even from the victim's perspective, the χ value
 *      does not reach the Snare threshold (≥0.66). This is crucial. The system
 *      correctly identifies that the extraction is instrumental to a stated,
 *      broad public policy goal (coordination), not an end in itself for the
 *      benefit of a narrow elite. This distinguishes a regulatory Tangled Rope
 *      from a predatory Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_german_board_gender_quota,
    'Does the quota lead to genuine integration and cultural change, or does it result in tokenism where the new appointees are structurally sidelined?',
    'Longitudinal studies tracking the committee assignments, speaking time, and policy influence of board members appointed under the quota, compared to those not.',
    'If genuine change -> Confirms strong coordination function, solidifies Tangled Rope. If tokenism -> Theater ratio increases over time, may degrade towards a Piton as firms learn to perform compliance without substance.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Interval represents 2021 (T=0) to 2031 (T=10).
narrative_ontology:interval(german_board_gender_quota, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extractiveness is > 0.30, and this is a Tangled Rope, a type prone to drift.
% We model the law as structurally stable in its first decade.
% T=0 is 2021, T=5 is 2026, T=10 is 2031.
% The final values at T=10 match the base properties.

% Theater ratio over time: stable and low, as the law is effective.
narrative_ontology:measurement(gbgq_tr_t0, german_board_gender_quota, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gbgq_tr_t5, german_board_gender_quota, theater_ratio, 5, 0.15).
narrative_ontology:measurement(gbgq_tr_t10, german_board_gender_quota, theater_ratio, 10, 0.15).

% Extraction over time: stable, as the law's text and enforcement have not changed.
narrative_ontology:measurement(gbgq_ex_t0, german_board_gender_quota, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(gbgq_ex_t5, german_board_gender_quota, base_extractiveness, 5, 0.40).
narrative_ontology:measurement(gbgq_ex_t10, german_board_gender_quota, base_extractiveness, 10, 0.40).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The law re-allocates the resource of board seats.
narrative_ontology:coordination_type(german_board_gender_quota, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations combined with exit options correctly models the dynamics of
% this constraint for all key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */