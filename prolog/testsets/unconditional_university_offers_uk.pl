% ============================================================================
% CONSTRAINT STORY: unconditional_university_offers_uk
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_unconditional_university_offers_uk, []).

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
 *   constraint_id: unconditional_university_offers_uk
 *   human_readable: Use of Unconditional Offers in UK University Admissions
 *   domain: economic/social
 *
 * SUMMARY:
 *   Following the removal of student number caps in 2015, UK universities
 *   increasingly used "unconditional offers" to secure student admissions in a
 *   more competitive market. This practice, particularly "conditional
 *   unconditional" offers (which become unconditional only if the student makes
 *   that university their firm choice), creates a coercive dynamic. While framed
 *   as reducing student stress, data from the regulator (Office for Students) shows
 *   it leads to students being more likely to miss their predicted A-level grades,
 *   extracting academic potential for the university's enrollment security.
 *
 * KEY AGENTS (by structural relationship):
 *   - Prospective Students: Primary target (powerless/trapped) — they accept an offer that reduces their final academic attainment in exchange for certainty.
 *   - UK Universities (esp. mid-tariff): Primary beneficiary (institutional/arbitrage) — they use the mechanism to secure enrollment and reduce uncertainty, gaining a competitive edge.
 *   - Office for Students (OfS) / Regulator: Secondary actor (institutional/constrained) — tasked with upholding standards, they are a victim of the system-gaming and must expend resources to combat it.
 *   - Analytical Observer: Sees the full structure, recognizing both the coordination function and the asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_university_offers_uk, 0.55).
domain_priors:suppression_score(unconditional_university_offers_uk, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(unconditional_university_offers_uk, 0.40).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_university_offers_uk, extractiveness, 0.55).
narrative_ontology:constraint_metric(unconditional_university_offers_uk, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(unconditional_university_offers_uk, theater_ratio, 0.40).

% --- NL Profile Metrics ---
% Not a mountain constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(unconditional_university_offers_uk, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(unconditional_university_offers_uk). % Required for Tangled Rope. The UCAS system and offer conditions enforce it.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(unconditional_university_offers_uk, uk_universities).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(unconditional_university_offers_uk, prospective_students).
narrative_ontology:constraint_victim(unconditional_university_offers_uk, education_regulators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function.
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifier σ(national) = 1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE PROSPECTIVE STUDENT (PRIMARY TARGET)
% A student who accepts the offer is trapped; their incentive to perform is
% reduced, leading to lower attainment. Victim + Trapped -> d ≈ 0.95 -> f(d) ≈ 1.42.
% χ = 0.55 * 1.42 * 1.0 = 0.781. With χ > 0.66 and suppression > 0.60, this is a Snare.
constraint_indexing:constraint_classification(unconditional_university_offers_uk, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE UNIVERSITY (PRIMARY BENEFICIARY)
% The university uses this as a tool to gain an advantage in the market.
% Beneficiary + Arbitrage -> d ≈ 0.05 -> f(d) ≈ -0.12.
% χ = 0.55 * -0.12 * 1.0 = -0.066. Negative χ classifies as a Rope.
constraint_indexing:constraint_classification(unconditional_university_offers_uk, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The observer sees both the coordination function for universities and the
% extractive harm to students.
% Analytical power -> d ≈ 0.72 -> f(d) ≈ 1.15. Scope σ(global) = 1.2.
% χ = 0.55 * 1.15 * 1.2 = 0.759.
% χ (0.759) > 0.40, Suppression (0.70) > 0.40, and the constraint has both
% coordination (beneficiary) and extraction (victim) functions. This is a Tangled Rope.
constraint_indexing:constraint_classification(unconditional_university_offers_uk, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---
% The Office for Students (OfS) is an institutional actor, but it's constrained
% by its mandate and limited powers, and is a victim of the system-gaming.
% Victim + Constrained -> d is high, but lower than powerless/trapped. Est d ≈ 0.7.
% The regulator sees the system as a harmful hybrid.
constraint_indexing:constraint_classification(unconditional_university_offers_uk, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_university_offers_uk_tests).

test(perspectival_gap_student_vs_university) :-
    constraint_indexing:constraint_classification(unconditional_university_offers_uk, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(unconditional_university_offers_uk, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(unconditional_university_offers_uk, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_gates_pass) :-
    % A constraint must have all three to be a canonical Tangled Rope.
    narrative_ontology:constraint_beneficiary(unconditional_university_offers_uk, _),
    narrative_ontology:constraint_victim(unconditional_university_offers_uk, _),
    domain_priors:requires_active_enforcement(unconditional_university_offers_uk).

:- end_tests(unconditional_university_offers_uk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): Set high to reflect the statistically significant, negative impact on student A-level attainment, a direct extraction of human capital.
 *   - Suppression Score (0.70): "Conditional unconditional" offers are highly coercive. They suppress a student's ability to choose other universities based on their final grades by locking them into a "firm choice" early, thus removing alternatives.
 *   - Theater Ratio (0.40): The policy is framed as a student-friendly move to "reduce stress," but its primary function is market capture. This gap between stated purpose and actual function justifies a moderate theater score.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For a university (beneficiary with arbitrage), this is a Rope—a pure coordination tool to manage enrollment risk and compete effectively. For a student (powerless and trapped), it is a Snare—a tempting offer that leads to a measurably worse personal outcome. This difference is driven entirely by their structural positions, which the directionality 'd' parameter captures.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `uk_universities`. They directly benefit by securing students, which translates to ~£9,250 per year in tuition fees per student.
 *   - Victim: `prospective_students`. They bear the cost through lower grades, which impacts their future academic and career options.
 *   - Victim: `education_regulators`. The OfS is also a victim, forced to expend political and regulatory capital to police a market distortion created by the universities.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The constraint creates a conflict between two institutional actors: the universities and the regulator (OfS). While both are 'institutional', their experiences diverge. The universities have `exit_options(arbitrage)`—they can choose to use this tool or not to gain advantage. The regulator has `exit_options(constrained)`—it cannot ignore the problem and must act, but its tools (fines, deregistration) are blunt and politically costly. This difference in exit options, combined with their opposing victim/beneficiary roles, correctly classifies the constraint as a Rope for one and a Tangled Rope for the other.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a classic case of Mandatrophy. A mechanism with a plausible coordination function (matching students to university places) is used for asymmetric extraction. A simple analysis might call it "competition" (a Rope) or a "predatory practice" (a Snare). The Tangled Rope classification from the analytical perspective correctly identifies its hybrid nature: it *does* coordinate, but it does so by inflicting harm. This prevents mislabeling it as either purely beneficial or purely malicious, capturing the true, messy reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_unconditional_offers,
    'Is the observed drop in student attainment a permanent structural harm, or a temporary behavioral artifact that students and schools would eventually adapt to?',
    'Longitudinal studies tracking the career outcomes and further education success of student cohorts who accepted unconditional offers vs. those who did not.',
    'If harm is permanent, the ε is correctly or even under-estimated. If it is temporary, the constraint is less a Snare and more a flawed Scaffold that requires adjustment.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(unconditional_university_offers_uk, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The practice escalated dramatically after the 2015 removal of student number caps.
% The timeline represents 2013-2023. T=0 is pre-escalation, T=10 is the post-peak state.
% Final values must match the base properties in Section 2.

% Theater ratio over time:
narrative_ontology:measurement(unconditional_offers_tr_t0, unconditional_university_offers_uk, theater_ratio, 0, 0.10).
narrative_ontology:measurement(unconditional_offers_tr_t5, unconditional_university_offers_uk, theater_ratio, 5, 0.35).
narrative_ontology:measurement(unconditional_offers_tr_t10, unconditional_university_offers_uk, theater_ratio, 10, 0.40).

% Extraction over time:
narrative_ontology:measurement(unconditional_offers_ex_t0, unconditional_university_offers_uk, base_extractiveness, 0, 0.10).
narrative_ontology:measurement(unconditional_offers_ex_t5, unconditional_university_offers_uk, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(unconditional_offers_ex_t10, unconditional_university_offers_uk, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This constraint is a mechanism for allocating a resource (university places).
narrative_ontology:coordination_type(unconditional_university_offers_uk, resource_allocation).

% Network relationships (structural influence edges)
% This policy directly impacts the perceived value and standards of A-levels.
narrative_ontology:affects_constraint(unconditional_university_offers_uk, a_level_examination_standards_uk).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this story. The structural declarations
% (beneficiary/victim) combined with the agents' exit options allow the
% engine to derive accurate directionality values for all key perspectives.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */