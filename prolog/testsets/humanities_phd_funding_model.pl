% ============================================================================
% CONSTRAINT STORY: humanities_phd_funding_model
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-22
% ============================================================================

:- module(constraint_humanities_phd_funding_model, []).

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
 *   constraint_id: humanities_phd_funding_model
 *   human_readable: The "Fully Funded" Humanities PhD Model in the US
 *   domain: economic/social
 *
 * SUMMARY:
 *   This constraint describes the financial and labor model of US-based
 *   humanities Ph.D. programs. These programs are often advertised as "fully
 *   funded" via tuition waivers and small stipends. However, the stipends
 *   are frequently below living wages for the university's location,
 *   forcing students to take on significant debt to survive. In exchange,
 *   students provide years of low-cost teaching and research labor that
 *   subsidizes the undergraduate education system.
 *
 * KEY AGENTS (by structural relationship):
 *   - Humanities Graduate Students: Primary target (powerless/trapped) — provide
 *     underpaid labor, accumulate debt, and face a poor academic job market.
 *   - University Administrations: Primary beneficiary (institutional/arbitrage) —
 *     receive a steady supply of cheap, highly-skilled labor to teach
 *     undergraduates, enhancing institutional prestige and revenue.
 *   - Analytical Observer: External analyst (analytical/analytical) — sees
 *     the full structure of coordination mixed with asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humanities_phd_funding_model, 0.55).
domain_priors:suppression_score(humanities_phd_funding_model, 0.85).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(humanities_phd_funding_model, 0.40).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humanities_phd_funding_model, extractiveness, 0.55).
narrative_ontology:constraint_metric(humanities_phd_funding_model, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(humanities_phd_funding_model, theater_ratio, 0.40).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(humanities_phd_funding_model, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(humanities_phd_funding_model). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(humanities_phd_funding_model, university_administrations).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(humanities_phd_funding_model, humanities_graduate_students).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (GRADUATE STUDENT)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% This high effective extraction (χ > 0.66) and high suppression classify
% the system as a Snare from their perspective.
constraint_indexing:constraint_classification(humanities_phd_funding_model, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (UNIVERSITY ADMINISTRATION)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
% From this view, the constraint is highly beneficial. However, the system's
% high base extractiveness (ε=0.55 > 0.45) prevents a Rope classification.
% It is a Tangled Rope, reflecting the mix of coordination and extraction they manage.
constraint_indexing:constraint_classification(humanities_phd_funding_model, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context. Recognizes both the coordination function
% (educating scholars, organizing teaching) and the severe asymmetric
% extraction. The combination of beneficiary, victim, and active enforcement,
% alongside the metric values, results in a Tangled Rope classification.
constraint_indexing:constraint_classification(humanities_phd_funding_model, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humanities_phd_funding_model_tests).

test(perspectival_gap_is_snare_vs_tangled_rope) :-
    constraint_indexing:constraint_classification(humanities_phd_funding_model, snare,
        context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(humanities_phd_funding_model, tangled_rope,
        context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(humanities_phd_funding_model, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_gates_pass) :-
    narrative_ontology:constraint_beneficiary(humanities_phd_funding_model, _),
    narrative_ontology:constraint_victim(humanities_phd_funding_model, _),
    domain_priors:requires_active_enforcement(humanities_phd_funding_model).

:- end_tests(humanities_phd_funding_model_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): Set high to reflect the net value transfer
 *     from graduate students to universities. This value is derived from
 *     the market value of their teaching labor minus their below-market
 *     stipends, compounded by the debt they incur for living expenses.
 *   - Suppression (S=0.85): Extremely high. The Ph.D. is a credentialing
 *     monopoly; there is no alternative path to an academic career in the
 *     humanities. This suppresses both alternative career routes and
 *     collective bargaining power among students.
 *   - Theater (T=0.40): The term "fully funded" is a key piece of theater
 *     that obscures the reality of debt-fueled subsistence. The prestige
 *     and "life of the mind" framing also serve this theatrical function.
 *
 * PERSPECTIVAL GAP:
 *   The gap is severe. For the graduate student (powerless, trapped), the
 *   system is a pure Snare. They are induced to enter with promises of
 *   funding, only to find themselves trapped by sunk costs (time and money)
 *   in a position of high labor extraction and debt accumulation. For the
 *   university administration (institutional, arbitrage), it's a highly
 *   effective Tangled Rope. They experience the benefits of coordination
 *   (a structured supply of teaching labor) while externalizing the costs
 *   onto the students. The high base extraction prevents this from being a
 *   benign Rope, even from the beneficiary's perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `university_administrations`. They get cheap labor, which
 *     allows them to offer more undergraduate courses and/or lower instructional
 *     costs, boosting revenue and rankings. Their position is institutional
 *     with arbitrage exit (they can always recruit a new cohort of students).
 *   - Victim: `humanities_graduate_students`. They provide the underpaid labor
 *     and bear the financial risk and debt. Their position is powerless and
 *     trapped due to the credentialing monopoly and sunk costs.
 *   This structural relationship directly drives the derivation of `d` and the resulting perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two potential errors. First, it does
 *   not mislabel the system as a pure Rope, because the analytical perspective
 *   and high base extraction (ε=0.55) correctly identify the severe extractive
 *   component. Second, it does not mislabel it as a pure Snare from a system
 *   level, because it acknowledges the genuine (if overpriced) coordination
 *   function of organizing education and credentialing. It is the combination—the
 *   Tangled Rope—that captures the full structure, while correctly classifying
 *   the lived experience of the target as a Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_humanities_phd_funding_model,
    'Is the primary value of the humanities Ph.D. its educational content or its theatrical function as a gatekeeping credential?',
    'Analysis of long-term career outcomes (both academic and non-academic) for graduates vs. non-completers, controlling for entrance qualifications.',
    'If primarily educational, the base extractiveness (ε) might be slightly lower. If primarily theatrical, the theater ratio (T) would be higher, pushing it closer to a Piton for non-completers.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(humanities_phd_funding_model, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data shows the degradation of this system over recent decades.
% As the academic job market worsened and university costs rose, the model
% became progressively more extractive. Required because ε > 0.46.

% Theater ratio over time: The "fully funded" narrative became more prominent
% as the financial reality for students worsened.
narrative_ontology:measurement(hphd_tr_t0, humanities_phd_funding_model, theater_ratio, 0, 0.20).
narrative_ontology:measurement(hphd_tr_t5, humanities_phd_funding_model, theater_ratio, 5, 0.30).
narrative_ontology:measurement(hphd_tr_t10, humanities_phd_funding_model, theater_ratio, 10, 0.40).

% Extraction over time: Stipends stagnated relative to cost of living while
% teaching loads and reliance on grad labor increased.
narrative_ontology:measurement(hphd_ex_t0, humanities_phd_funding_model, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(hphd_ex_t5, humanities_phd_funding_model, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(hphd_ex_t10, humanities_phd_funding_model, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The system coordinates the allocation of teaching resources, faculty time,
% and educational credentialing.
narrative_ontology:coordination_type(humanities_phd_funding_model, resource_allocation).

% Network relationships (structural influence edges)
% The cheap labor from this model structurally affects the cost and
% availability of undergraduate education.
narrative_ontology:affects_constraint(humanities_phd_funding_model, undergraduate_tuition_costs).
narrative_ontology:affects_constraint(humanities_phd_funding_model, academic_publishing_model).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The standard derivation
% chain (beneficiary/victim + exit options -> d) accurately captures the
% structural relationships between graduate students and universities.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */