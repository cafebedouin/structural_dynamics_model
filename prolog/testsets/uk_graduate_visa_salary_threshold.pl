% ============================================================================
% CONSTRAINT STORY: uk_graduate_visa_salary_threshold
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-24
% ============================================================================

:- module(constraint_uk_graduate_visa_salary_threshold, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: uk_graduate_visa_salary_threshold
 *   human_readable: UK Graduate Visa Minimum Salary Threshold
 *   domain: economic/political
 *
 * SUMMARY:
 *   The UK government has tightened the rules for the Graduate Visa route, a
 *   two-year post-study work visa for international students. Graduates must
 *   now secure a "skilled job" with a minimum salary (e.g., £30,000+) or
 *   switch to another visa to remain in the UK. This policy is framed as
 *   preventing abuse and selecting for high-skilled migrants, but critics
 *   argue it creates a significant barrier for graduates, harms university
 *   revenues, and restricts the talent pool for UK businesses, particularly
 *   outside of high-paying sectors like finance in London.
 *
 * KEY AGENTS (by structural relationship):
 *   - International Graduates: Primary target (powerless/trapped) — bear the financial and career costs.
 *   - UK Government/Home Office: Primary beneficiary (institutional/arbitrage) — achieves political goal of reducing net migration.
 *   - UK Universities & Businesses: Secondary victims (institutional/constrained) — lose revenue and talent.
 *   - Analytical Observer: Sees the full structure, including the gap between policy rhetoric and on-the-ground impact.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_graduate_visa_salary_threshold, 0.75).
domain_priors:suppression_score(uk_graduate_visa_salary_threshold, 0.80).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(uk_graduate_visa_salary_threshold, 0.40).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_graduate_visa_salary_threshold, extractiveness, 0.75).
narrative_ontology:constraint_metric(uk_graduate_visa_salary_threshold, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(uk_graduate_visa_salary_threshold, theater_ratio, 0.40).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(uk_graduate_visa_salary_threshold, snare).
narrative_ontology:human_readable(uk_graduate_visa_salary_threshold, "UK Graduate Visa Minimum Salary Threshold").

% --- Binary flags ---
domain_priors:requires_active_enforcement(uk_graduate_visa_salary_threshold). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(uk_graduate_visa_salary_threshold, uk_government_home_office).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(uk_graduate_visa_salary_threshold, international_graduates).
narrative_ontology:constraint_victim(uk_graduate_visa_salary_threshold, uk_universities_and_businesses).

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

% PERSPECTIVE 1: THE PRIMARY TARGET (INTERNATIONAL GRADUATE)
% As a victim with trapped exit options (leave the country or fail to qualify),
% the engine derives a high d (≈0.95), leading to a high effective
% extraction (χ), classifying the constraint as a Snare.
constraint_indexing:constraint_classification(uk_graduate_visa_salary_threshold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (UK GOVERNMENT)
% As a beneficiary with arbitrage exit options (they can change the policy at
% will), the engine derives a very low d (≈0.05), leading to a negative
% effective extraction (χ). From this viewpoint, it's a pure coordination
% mechanism (Rope) to manage migration flows.
constraint_indexing:constraint_classification(uk_graduate_visa_salary_threshold, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The analytical observer sees the high base extraction and suppression. Even
% with a moderate directionality (d≈0.72), the scaled extraction (χ) is
% extremely high, and the structural presence of a victim group pushes the
% classification to Snare, as the "coordination" function is overwhelmed by
% the coercive extraction.
constraint_indexing:constraint_classification(uk_graduate_visa_salary_threshold, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---

% PERSPECTIVE 4: AFFECTED INSTITUTIONS (UK UNIVERSITIES)
% As institutional victims, universities have constrained exit options (they can't
% easily leave the UK market). This results in a high derived d value, leading to
% a high χ. For them, a system meant to attract talent has become a Snare
% that damages their financial model and global competitiveness.
constraint_indexing:constraint_classification(uk_graduate_visa_salary_threshold, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_graduate_visa_salary_threshold_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(uk_graduate_visa_salary_threshold, snare,
        context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(uk_graduate_visa_salary_threshold, rope,
        context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(perspectival_gap_inter_institutional) :-
    % Verify perspectival gap between two institutional actors.
    constraint_indexing:constraint_classification(uk_graduate_visa_salary_threshold, rope,
        context(agent_power(institutional), _, exit_options(arbitrage), _)), % Government
    constraint_indexing:constraint_classification(uk_graduate_visa_salary_threshold, snare,
        context(agent_power(institutional), _, exit_options(constrained), _)), % University
    !. % Ensure at least one solution exists

test(analytical_claim_matches) :-
    narrative_ontology:constraint_claim(uk_graduate_visa_salary_threshold, Claim),
    constraint_indexing:constraint_classification(uk_graduate_visa_salary_threshold, Claim,
        context(agent_power(analytical), _, _, _)).

:- end_tests(uk_graduate_visa_salary_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.75): High. This reflects not just visa fees but
 *     the significant capture of sunk costs (tuition fees) and the foreclosure
 *     of future opportunities if the graduate cannot meet the narrow criteria.
 *   - Suppression (S=0.80): High. The rule severely limits the graduate's
 *     options post-study, suppressing entrepreneurial paths, work in lower-paying
 *     but high-social-value sectors, or gradual career progression.
 *   - Theater Ratio (T=0.40): Moderate. There is a significant performative
 *     aspect to the policy, aimed at demonstrating a "tough on immigration"
 *     stance for a domestic political audience, which may not align with the
 *     optimal economic strategy for talent retention.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. The UK Government, as the beneficiary with arbitrage power,
 *   views this as a benign Rope for sorting migrants. For the international graduate,
 *   who is trapped by their prior investment and limited options, it's a Snare that
 *   can abruptly end their career plans and force them out of the country.
 *   This is a classic case where the architect of a system experiences it as
 *   coordination, while the target experiences it as coercion.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `uk_government_home_office`. The policy directly serves its
 *     stated political goal of reducing net migration numbers.
 *   - Victims: `international_graduates` and `uk_universities_and_businesses`.
 *     Graduates bear the direct cost and risk. Universities and businesses suffer
 *     the secondary effects of reduced international enrollment and a smaller
 *     talent pool. This clear, asymmetric distribution of costs and benefits is
 *     what drives the large perspectival gap.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The model captures the conflict between two institutional actors. The Home
 *   Office (institutional, arbitrage) implements the policy and sees a Rope.
 *   The university sector (institutional, constrained) is a victim of the policy
 *   and sees a Snare. Their `constrained` exit status reflects their inability
 *   to simply opt-out of the UK's regulatory environment, making them vulnerable
 *   to the policy's extractive effects in a way the government is not.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY]
 *   This classification correctly identifies the structure as a Snare from the
 *   target's view, preventing the government's "coordination" narrative (Rope)
 *   from being accepted as the objective reality. The framework shows that while
 *   a coordination function (sorting workers) is claimed, the mechanism is so
 *   coercive (high suppression) and its costs so asymmetrically distributed
 *   (high extraction) that it functions as a Snare for those subject to it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_uk_grad_visa_1,
    'Is the policy''s primary function to genuinely select for "high-skilled" talent (a very coercive Tangled Rope), or to performatively reduce net migration figures regardless of economic harm (a Snare)?',
    'Long-term analysis of the net economic impact, comparing the tax revenue from retained graduates against the lost revenue from deterred students and unfilled vacancies.',
    'If primarily for talent selection, the high extraction is a design flaw. If for migration numbers, the extraction is the intended mechanism.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_graduate_visa_salary_threshold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This policy represents an intensification over time. The measurements model
% the shift from a more liberal post-study work regime to the current,
% highly extractive one. This demonstrates extraction_accumulation.

% Theater ratio over time:
narrative_ontology:measurement(uk_grad_visa_tr_t0, uk_graduate_visa_salary_threshold, theater_ratio, 0, 0.10).
narrative_ontology:measurement(uk_grad_visa_tr_t5, uk_graduate_visa_salary_threshold, theater_ratio, 5, 0.25).
narrative_ontology:measurement(uk_grad_visa_tr_t10, uk_graduate_visa_salary_threshold, theater_ratio, 10, 0.40).

% Extraction over time:
narrative_ontology:measurement(uk_grad_visa_ex_t0, uk_graduate_visa_salary_threshold, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(uk_grad_visa_ex_t5, uk_graduate_visa_salary_threshold, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(uk_grad_visa_ex_t10, uk_graduate_visa_salary_threshold, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% This visa rule is a mechanism for allocating the resource of "right to work".
narrative_ontology:coordination_type(uk_graduate_visa_salary_threshold, resource_allocation).

% This policy directly influences the funding models of UK universities and
% the labor supply for key sectors.
narrative_ontology:affects_constraint(uk_graduate_visa_salary_threshold, uk_higher_education_funding).
narrative_ontology:affects_constraint(uk_graduate_visa_salary_threshold, uk_tech_sector_labor_supply).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation
% from beneficiary/victim declarations and exit options accurately models the
% power dynamics and perspectival gaps between the government, graduates,
% and universities.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */