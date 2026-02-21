% ============================================================================
% CONSTRAINT STORY: uk_student_visa_dependents
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-24
% ============================================================================

:- module(constraint_uk_student_visa_dependents, []).

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
 *   constraint_id: uk_student_visa_dependents
 *   human_readable: UK policy restricting dependents of international students
 *   domain: political/economic
 *
 * SUMMARY:
 *   In an effort to reduce net migration figures, the UK government implemented
 *   a policy preventing international students on taught postgraduate courses
 *   (like one-year master's degrees) from bringing family members (dependents)
 *   with them. This rule carves out an exception for students on research-based
 *   postgraduate programs. The policy creates a significant cost for affected
 *   students and financial pressure on UK universities, which rely on international
 *   student fees.
 *
 * KEY AGENTS (by structural relationship):
 *   - International Masters Students: Primary target (powerless/constrained) — bear the cost of family separation or forgoing UK education.
 *   - UK Government (Home Office): Primary beneficiary (institutional/arbitrage) — benefits from lower headline migration numbers, a key political goal.
 *   - UK Universities: Secondary target (institutional/constrained) — face reduced international student applications and revenue loss.
 *   - Analytical Observer: Analytical observer — sees the full structure of coordination (border control) and asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_student_visa_dependents, 0.55).
domain_priors:suppression_score(uk_student_visa_dependents, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(uk_student_visa_dependents, 0.40).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_student_visa_dependents, extractiveness, 0.55).
narrative_ontology:constraint_metric(uk_student_visa_dependents, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(uk_student_visa_dependents, theater_ratio, 0.40).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(uk_student_visa_dependents, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(uk_student_visa_dependents). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(uk_student_visa_dependents, uk_government_migration_hawks).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(uk_student_visa_dependents, international_masters_students).
narrative_ontology:constraint_victim(uk_student_visa_dependents, uk_universities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (INTERNATIONAL STUDENT)
% An international student on a taught master's course experiences this policy
% as a pure coercive barrier. The high suppression and extraction make it a Snare,
% forcing a painful choice between education and family life.
constraint_indexing:constraint_classification(uk_student_visa_dependents, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (UK GOVERNMENT)
% The government faction focused on migration numbers sees this as a legitimate
% and effective tool for policy coordination (managing borders). The costs are
% externalized. From their view, it's a Rope.
% Engine derives d from: beneficiary + arbitrage exit → d≈0.05 → f(d)≈-0.12 → negative χ.
constraint_indexing:constraint_classification(uk_student_visa_dependents, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The analyst sees both the valid coordination function (a state's right to
% control its borders) and the significant, asymmetrically imposed costs.
% This dual nature is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(uk_student_visa_dependents, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---

% PERSPECTIVE 4: UK UNIVERSITIES
% Universities are institutional actors but are victims of this policy. They
% cannot exit the UK regulatory system, making their exit 'constrained'.
% They perceive the high extraction (loss of revenue, talent) and coercion,
% while understanding the government's stated rationale. This classifies as a
% Tangled Rope, but with a higher effective extraction than the government sees.
constraint_indexing:constraint_classification(uk_student_visa_dependents, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_student_visa_dependents_tests).

test(perspectival_gap_student_vs_gov) :-
    % Verify perspectival gap between target (student) and beneficiary (government).
    constraint_indexing:constraint_classification(uk_student_visa_dependents, TypeStudent, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_student_visa_dependents, TypeGov, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    assertion(TypeStudent == snare),
    assertion(TypeGov == rope),
    TypeStudent \= TypeGov.

test(perspectival_gap_inter_institutional) :-
    % Verify perspectival gap between the two institutional actors.
    constraint_indexing:constraint_classification(uk_student_visa_dependents, TypeGov, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(uk_student_visa_dependents, TypeUni, context(agent_power(institutional), _, exit_options(constrained), _)),
    assertion(TypeGov == rope),
    assertion(TypeUni == tangled_rope),
    TypeGov \= TypeUni.

test(tangled_rope_gate_compliance) :-
    % Verify that all three conditions for a Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(uk_student_visa_dependents, _),
    narrative_ontology:constraint_victim(uk_student_visa_dependents, _),
    domain_priors:requires_active_enforcement(uk_student_visa_dependents).

:- end_tests(uk_student_visa_dependents_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): High. This represents the severe cost imposed on students (family separation, career disruption) and the financial loss to universities, relative to the political gain for the government.
 *   - Suppression Score (0.70): High. The policy directly and effectively eliminates the option for a specific class of students to bring their families, forcing them to seek alternatives outside the UK system or come alone.
 *   - Theater Ratio (0.40): Moderate. While the policy has a real effect on migration numbers, a significant part of its function is performative—demonstrating "toughness" on immigration for a political audience, even at a potential economic cost.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For a student, the policy is a pure barrier (Snare) with no discernible benefit. For the government, it's a simple tool of control (Rope). For universities, it's a damaging regulation they are forced to navigate (Tangled Rope). The analytical view synthesizes these, recognizing both the coordination function and the asymmetric extraction, classifying it as a classic Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `uk_government_migration_hawks`. The policy directly serves their political objective of reducing headline net migration figures.
 *   - Victims: `international_masters_students` and `uk_universities`. Students bear the direct personal cost. Universities bear the secondary financial and reputational costs. The engine derives a high directionality `d` for these groups, leading to high effective extraction (χ).
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This story highlights a key inter-institutional conflict. Both the government and universities are `institutional` actors, but their relationship to the constraint is opposite. The government has `arbitrage` exit (it can change the policy at will), while universities have `constrained` exit (they are bound by it). This difference in exit options, combined with their beneficiary/victim status, is what allows the engine to derive different `d` values and thus different classifications (Rope vs. Tangled Rope), capturing the structural reality of the conflict.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the hybrid nature of the policy. A simplistic analysis might label it purely as "immigration control" (Rope) or purely as "predatory policy" (Snare). The Tangled Rope classification acknowledges the legitimate state function of border control while simultaneously quantifying the high, targeted extraction required to achieve it, preventing the coordination narrative from obscuring the coercive reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_uk_student_visa_dependents,
    'Does the long-term economic damage from lost talent and university funding outweigh the short-term political benefit of reduced net migration figures?',
    'Longitudinal studies tracking GDP contribution from student cohorts, university financial health reports, and analysis of skill shortages in key sectors over a 5-10 year period.',
    'If True, the policy is a net-negative Tangled Rope causing long-term institutional damage. If False, it is a politically effective, albeit extractive, Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(uk_student_visa_dependents, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extractiveness is > 0.46, so temporal data is required.
% This models the policy intensifying over time as political pressure
% to reduce migration numbers grew.
%
% Theater ratio over time (metric substitution):
narrative_ontology:measurement(uk_student_visa_dependents_tr_t0, uk_student_visa_dependents, theater_ratio, 0, 0.20).
narrative_ontology:measurement(uk_student_visa_dependents_tr_t5, uk_student_visa_dependents, theater_ratio, 5, 0.35).
narrative_ontology:measurement(uk_student_visa_dependents_tr_t10, uk_student_visa_dependents, theater_ratio, 10, 0.40).

% Extraction over time (extraction accumulation):
narrative_ontology:measurement(uk_student_visa_dependents_ex_t0, uk_student_visa_dependents, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(uk_student_visa_dependents_ex_t5, uk_student_visa_dependents, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(uk_student_visa_dependents_ex_t10, uk_student_visa_dependents, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This policy is a form of state enforcement.
narrative_ontology:coordination_type(uk_student_visa_dependents, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The automatic derivation
% from beneficiary/victim declarations and exit options (arbitrage vs.
% constrained) accurately captures the structural dynamics between the
% government, universities, and students.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */