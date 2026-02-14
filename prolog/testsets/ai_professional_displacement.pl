% ============================================================================
% CONSTRAINT STORY: ai_professional_displacement
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_ai_professional_displacement, []).

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
 *   constraint_id: ai_professional_displacement
 *   human_readable: AI-Driven Displacement of Entry-Level Professional Pathways
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The automation of entry-level "grunt work" in professional services (law, finance, consulting)
 *   by AI systems is creating a structural barrier to career entry for new graduates. As bots
 *   perform the work previously done by junior employees, hiring rates decline, eroding the
 *   traditional pathways to financial stability and skill acquisition in these fields.
 *
 * KEY AGENTS (by structural relationship):
 *   - Entry-Level Professionals: Primary target (powerless/trapped) — face a collapsing job market with few alternatives.
 *   - Corporate Employers/Investors: Primary beneficiary (institutional/arbitrage) — deploy AI to cut labor costs and increase efficiency.
 *   - Mid-Level Managers: Secondary actor (moderate/constrained) — coordinate AI tools but oversee the decay of human mentorship pipelines.
 *   - Analytical Observer: Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_professional_displacement, 0.71).
domain_priors:suppression_score(ai_professional_displacement, 0.80).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(ai_professional_displacement, 0.55).       % Reflects "AI up-skilling" rhetoric vs. actual headcount reduction.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_professional_displacement, extractiveness, 0.71).
narrative_ontology:constraint_metric(ai_professional_displacement, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(ai_professional_displacement, theater_ratio, 0.55).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(ai_professional_displacement, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(ai_professional_displacement). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(ai_professional_displacement, corporate_employers_and_investors).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(ai_professional_displacement, entry_level_professionals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Recent graduates face a hiring downturn with zero bargaining power. Career
% entry points are removed without immediate, viable alternatives.
constraint_indexing:constraint_classification(ai_professional_displacement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% CEOs and investors view AI as a pure coordination tool to improve
% competitiveness and efficiency, with labor cost reduction as a direct benefit.
constraint_indexing:constraint_classification(ai_professional_displacement, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% An analyst sees the dual nature: a genuine coordination/efficiency gain for
% firms, coupled with severe, asymmetric extraction from the junior labor pool.
constraint_indexing:constraint_classification(ai_professional_displacement, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: MID-LEVEL MANAGER (TANGLED ROPE)
% Managers use AI to coordinate departmental output (Rope function) but also
% manage the extractive consequences: loss of mentorship pipelines and human
% capital development (Snare function).
constraint_indexing:constraint_classification(ai_professional_displacement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_professional_displacement_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(ai_professional_displacement, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_professional_displacement, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary,
    TypeTarget == snare,
    TypeBeneficiary == rope.

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(ai_professional_displacement, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation_for_tangled_rope) :-
    domain_priors:base_extractiveness(ai_professional_displacement, E),
    domain_priors:suppression_score(ai_professional_displacement, S),
    E >= 0.30,
    S >= 0.40.

:- end_tests(ai_professional_displacement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.71) and suppression (0.80) are high, reflecting the
 *   severe, non-negotiable impact on the junior labor market. There are few-to-no
 *   alternative pathways for this cohort to enter these professions, hence the high
 *   suppression score. The constraint is actively enforced by market pressures and
 *   corporate hiring policies.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For institutional beneficiaries, this is a Rope—a tool for
 *   coordinating resources more efficiently. For powerless graduates, it's a Snare
 *   that removes the first rung of the career ladder. The system correctly captures
 *   that the same technological shift can be purely beneficial for one group and
 *   purely extractive for another.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: 'corporate_employers_and_investors' directly profit from reduced
 *     labor costs and increased productivity. Their arbitrage exit option reflects their
 *     ability to deploy capital and technology globally.
 *   - Victims: 'entry_level_professionals' bear the full cost through lost opportunities,
 *     wage suppression, and career path destruction. Their trapped status reflects the
 *     lack of alternative entry points into high-skill professions.
 *
 * MANDATROPHY ANALYSIS:
 *   The analytical classification as Tangled Rope is crucial. It prevents mislabeling
 *   this complex dynamic as either a pure Snare (ignoring the genuine efficiency gains)
 *   or a pure Mountain (falsely naturalizing a contingent economic arrangement). The
 *   high extraction and clear beneficiary/victim structure resolve the Mandatrophy
 *   ambiguity; this is not a natural law but a system of asymmetric extraction built
 *   atop a coordination technology.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_ai_displacement_1,
    'Is graduate hiring reduction a functional necessity of the technology or a predatory choice for short-term profit?',
    'Longitudinal audit of firm reinvestment into human capital vs. shareholder returns.',
    'If necessity: closer to Mountain. If predatory choice: confirms Snare/Tangled Rope.',
    confidence_without_resolution(medium)
).

omega_variable(
    omega_ai_displacement_2,
    'Will AI productivity gains eventually create new, different entry-level roles, or is the ladder broken permanently?',
    'Monitor real wage growth and creation of novel job titles vs. productivity indices through 2030.',
    'If new roles emerge: Scaffold. If not: the Snare tightens.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(ai_professional_displacement, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio: Rising from functional tool integration (0.20) to performative
% "Corporate Social Responsibility/Upskilling" theater (0.55) as hiring freezes continue.
narrative_ontology:measurement(ai_disp_tr_t0, ai_professional_displacement, theater_ratio, 0, 0.20).
narrative_ontology:measurement(ai_disp_tr_t5, ai_professional_displacement, theater_ratio, 5, 0.42).
narrative_ontology:measurement(ai_disp_tr_t10, ai_professional_displacement, theater_ratio, 10, 0.55).

% Extraction: Progressive accumulation of labor efficiency gains at the expense
% of human capital development (junior career pathways).
narrative_ontology:measurement(ai_disp_ex_t0, ai_professional_displacement, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(ai_disp_ex_t5, ai_professional_displacement, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(ai_disp_ex_t10, ai_professional_displacement, base_extractiveness, 10, 0.71).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(ai_professional_displacement, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations and exit options accurately models the directionality.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */