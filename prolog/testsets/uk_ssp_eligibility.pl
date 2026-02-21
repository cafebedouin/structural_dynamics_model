% ============================================================================
% CONSTRAINT STORY: uk_ssp_eligibility
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-23
% ============================================================================

:- module(constraint_uk_ssp_eligibility, []).

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
 *   constraint_id: uk_ssp_eligibility
 *   human_readable: UK Statutory Sick Pay (SSP) Eligibility and Rate
 *   domain: economic/political
 *
 * SUMMARY:
 *   The UK's Statutory Sick Pay (SSP) system has eligibility criteria,
 *   primarily a Lower Earnings Limit (LEL), that excludes a significant
 *   portion of the low-wage, part-time, and gig economy workforce. For those
 *   who do qualify, the payment rate is too low to live on, forcing workers
 *   to choose between their health and their income. This constraint shifts
 *   the financial burden of sickness from employers onto the most vulnerable
 *   workers and creates negative public health externalities.
 *
 * KEY AGENTS (by structural relationship):
 *   - Low-income and gig economy workers: Primary target (powerless/trapped) — bears extraction by being denied sick pay.
 *   - Employers of low-wage labor: Primary beneficiary (institutional/arbitrage) — benefits from reduced labor costs and externalizing risk.
 *   - The UK Government: Secondary actor (institutional/constrained) — experiences a mix of costs (public health) and benefits (lower direct payouts).
 *   - Public health analysts: Analytical observer — sees the full structure of cost-shifting and systemic risk.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_ssp_eligibility, 0.62).
domain_priors:suppression_score(uk_ssp_eligibility, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(uk_ssp_eligibility, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_ssp_eligibility, extractiveness, 0.62).
narrative_ontology:constraint_metric(uk_ssp_eligibility, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(uk_ssp_eligibility, theater_ratio, 0.20).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(uk_ssp_eligibility, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(uk_ssp_eligibility). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(uk_ssp_eligibility, employers_of_low_wage_labor).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(uk_ssp_eligibility, low_income_and_gig_economy_workers).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three) -> MET
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
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
%   χ = 0.62 * 1.42 * 1.0 (national scope) = 0.88
constraint_indexing:constraint_classification(uk_ssp_eligibility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
%   χ = 0.62 * -0.12 * 1.0 (national scope) = -0.07
constraint_indexing:constraint_classification(uk_ssp_eligibility, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context. Sees both coordination and extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
%   χ = 0.62 * 1.15 * 1.2 (global scope) = 0.85
constraint_indexing:constraint_classification(uk_ssp_eligibility, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: INTER-INSTITUTIONAL - THE STATE (TANGLED ROPE)
% The government is an institutional actor but is constrained by the system's
% negative externalities (e.g., public health costs). It is both a beneficiary
% (low direct payout) and a victim (systemic risk).
% Its constrained exit options and dual role result in a higher d than the
% employer beneficiary, leading to a Tangled Rope classification.
constraint_indexing:constraint_classification(uk_ssp_eligibility, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_ssp_eligibility_tests).

test(perspectival_gap_target_beneficiary) :-
    constraint_indexing:constraint_classification(uk_ssp_eligibility, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(uk_ssp_eligibility, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    format('~NTarget sees Snare, Beneficiary sees Rope. Gap confirmed.~n').

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(uk_ssp_eligibility, tangled_rope, context(agent_power(analytical), _, _, _)),
    format('~nAnalytical view correctly identifies Tangled Rope structure.~n').

test(tangled_rope_structural_gates_pass) :-
    narrative_ontology:constraint_beneficiary(uk_ssp_eligibility, _),
    narrative_ontology:constraint_victim(uk_ssp_eligibility, _),
    domain_priors:requires_active_enforcement(uk_ssp_eligibility),
    format('~nAll three structural requirements for Tangled Rope are present.~n').

:- end_tests(uk_ssp_eligibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.62): High. The system extracts value by compelling
 *     the unwell to work or forcing them to absorb the full financial shock of
 *     illness. This value is captured by employers as a direct reduction in
 *     labor costs.
 *   - Suppression (0.75): High. For a low-wage worker, the alternatives to
 *     complying (working while sick) are unemployment or destitution.
 *     There is little individual bargaining power and weak collective action.
 *   - The analytical classification is Tangled Rope because the system possesses
 *     both a genuine (though skewed) coordination function (standardizing sick
 *     pay rules for employers) and a massive asymmetric extraction function.
 *     This dual nature is the hallmark of a Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   - For a low-income worker (powerless, trapped), the system is a pure Snare.
 *     It offers no meaningful support and forces a choice between health and
 *     income. The calculated effective extraction (χ) is ~0.88, well into Snare territory.
 *   - For an employer of low-wage labor (institutional, arbitrage), the system
 *     is a Rope. It coordinates labor standards in a way that minimizes their costs
 *     and liabilities, effectively acting as a subsidy. The calculated χ is negative.
 *   - This gap is the core of the political conflict: one group's safety net is
 *     another group's cost-saving mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `employers_of_low_wage_labor`. They directly benefit from the LEL
 *     and low rate, which lowers their operational costs. This declaration drives
 *     the directionality `d` towards 0 for the institutional perspective.
 *   - Victim: `low_income_and_gig_economy_workers`. They are the direct targets of
 *     the extraction, bearing the costs of a system that fails to protect them.
 *     This declaration drives `d` towards 1.0 for the powerless/trapped perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two common errors. First, it does not
 *   mislabel the system as a pure Snare from an analytical perspective, because
 *   that would ignore its (beneficial to some) coordination function. Second, it
 *   avoids labeling it a pure (if flawed) Rope, because that would ignore the
 *   massive, asymmetric extraction. The Tangled Rope classification captures this
 *   essential duality: a system that provides coordination for one group by
 *   extracting heavily from another. The potential for coalition formation among
 *   the 'powerless' (e.g., via unions) could shift their power atom to 'organized',
 *   reducing their effective extraction and potentially changing the classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_uk_ssp_eligibility,
    'Is the SSP system''s failure to cover precarious workers a deliberate policy feature to maintain labor market flexibility, or a bug of legislative inertia?',
    'Review of historical policy documents and minutes from lobbying meetings between business groups and government officials during periods of labor market reform.',
    'If deliberate, it confirms the deeply embedded Tangled Rope structure. If inertia, it suggests the system has degraded from a former Rope into a Tangled Rope due to neglect, which might imply an easier path to reform.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(uk_ssp_eligibility, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint, so temporal data is required.
% The model shows extraction accumulating over the last ~20 years as the
% nature of the UK labor market shifted towards more precarious work, while
% the SSP system failed to adapt.

% Theater ratio over time:
narrative_ontology:measurement(uk_ssp_tr_t0, uk_ssp_eligibility, theater_ratio, 0, 0.15).
narrative_ontology:measurement(uk_ssp_tr_t5, uk_ssp_eligibility, theater_ratio, 5, 0.18).
narrative_ontology:measurement(uk_ssp_tr_t10, uk_ssp_eligibility, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(uk_ssp_ex_t0, uk_ssp_eligibility, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(uk_ssp_ex_t5, uk_ssp_eligibility, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(uk_ssp_ex_t10, uk_ssp_eligibility, base_extractiveness, 10, 0.62).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The SSP system allocates risk and financial support (or lack thereof).
narrative_ontology:coordination_type(uk_ssp_eligibility, resource_allocation).

% Network relationships (structural influence edges)
% The SSP system is structurally coupled with the legal framework governing
% the gig economy, as one enables the extractive potential of the other.
narrative_ontology:affects_constraint(uk_ssp_eligibility, gig_economy_labor_law).
narrative_ontology:affects_constraint(uk_minimum_wage_policy, uk_ssp_eligibility).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation from
% beneficiary/victim declarations and exit options accurately models the
% directionality for all key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */