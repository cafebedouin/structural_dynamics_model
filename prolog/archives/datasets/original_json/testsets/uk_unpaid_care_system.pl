% ============================================================================
% CONSTRAINT STORY: uk_unpaid_care_system
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-24
% ============================================================================

:- module(constraint_uk_unpaid_care_system, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: uk_unpaid_care_system
 *   human_readable: The UK's reliance on unpaid carers for social and healthcare
 *   domain: economic/social
 *
 * SUMMARY:
 *   The UK's social care system relies heavily on a large population of unpaid
 *   carers, typically family members, who provide essential support to ill,
 *   elderly, or disabled relatives. This system extracts immense economic value
 *   (estimated at £162bn annually) from these individuals, who receive minimal
 *   state support (e.g., Carer's Allowance of £81.90/week) and face significant
 *   financial, professional, and personal hardship.
 *
 * KEY AGENTS (by structural relationship):
 *   - Unpaid Carers: Primary target (powerless/trapped) — bears extreme extraction of time, finances, and well-being.
 *   - UK Government & Taxpayers: Primary beneficiary (institutional/arbitrage) — benefits from a massive subsidy to the health and social care systems.
 *   - Care Advocacy Groups (e.g., Carers UK): Secondary actor (organized/constrained) - attempt to reform the system.
 *   - Analytical Observer: Policy analyst or sociologist (analytical/analytical) — sees the full structure of value transfer.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% £162bn/year is extracted. This is a massive, unidirectional value transfer.
domain_priors:base_extractiveness(uk_unpaid_care_system, 0.85).
% Alternatives (private full-time care) are prohibitively expensive or unavailable,
% creating a high degree of coercion.
domain_priors:suppression_score(uk_unpaid_care_system, 0.90).
% Carer's Allowance exists but is tokenistic relative to the value extracted,
% so theater is present but low.
domain_priors:theater_ratio(uk_unpaid_care_system, 0.20).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_unpaid_care_system, extractiveness, 0.85).
narrative_ontology:constraint_metric(uk_unpaid_care_system, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(uk_unpaid_care_system, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(uk_unpaid_care_system, snare).
narrative_ontology:human_readable(uk_unpaid_care_system, "The UK's reliance on unpaid carers for social and healthcare").
narrative_ontology:topic_domain(uk_unpaid_care_system, "economic/social").

% --- Binary flags ---
% The rules governing Carer's Allowance and eligibility are actively enforced.
domain_priors:requires_active_enforcement(uk_unpaid_care_system).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
%
% Who benefits from this constraint existing?
% The state avoids funding a formal social care system, saving billions.
narrative_ontology:constraint_beneficiary(uk_unpaid_care_system, uk_government_and_taxpayers).
%
% Who bears disproportionate cost?
% Carers sacrifice careers, savings, and health.
narrative_ontology:constraint_victim(uk_unpaid_care_system, unpaid_carers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function.
   The engine derives d from beneficiary/victim membership + exit_options.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE UNPAID CARER (PRIMARY TARGET)
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42.
% χ = 0.85 (ε) * 1.42 (f(d)) * 1.0 (σ(national)) ≈ 1.21.
% This high effective extraction (χ > 0.66) results in a clear Snare classification.
constraint_indexing:constraint_classification(uk_unpaid_care_system, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE UK GOVERNMENT (PRIMARY BENEFICIARY)
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12.
% χ = 0.85 (ε) * -0.12 (f(d)) * 1.0 (σ(national)) ≈ -0.10.
% From the state's perspective, this is a highly efficient coordination mechanism
% that provides immense value (negative extraction), hence a Rope.
constraint_indexing:constraint_classification(uk_unpaid_care_system, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15. Global scope amplifies χ.
% χ = 0.85 (ε) * 1.15 (f(d)) * 1.2 (σ(global)) ≈ 1.17.
% The analytical view, accounting for the full system structure and comparing
% globally, also identifies a Snare, aligning with the target's experience.
constraint_indexing:constraint_classification(uk_unpaid_care_system, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ADVOCACY GROUP
% An organized group has more power than an individual, but is still constrained.
% Engine derives d for 'organized' victim with 'constrained' exit -> d ≈ 0.65 -> f(d) ≈ 1.0
% χ = 0.85 * 1.0 * 1.0 ≈ 0.85. Still a Snare, but the perceived extraction is
% lower than for a trapped individual, reflecting their ability to effect marginal change.
constraint_indexing:constraint_classification(uk_unpaid_care_system, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_unpaid_care_system_tests).

test(perspectival_gap_is_snare_vs_rope) :-
    constraint_indexing:constraint_classification(uk_unpaid_care_system, snare,
        context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(uk_unpaid_care_system, rope,
        context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_snare) :-
    constraint_indexing:constraint_classification(uk_unpaid_care_system, snare,
        context(agent_power(analytical), _, _, _)).

test(high_extraction_and_suppression_scores) :-
    domain_priors:base_extractiveness(uk_unpaid_care_system, E), E >= 0.46,
    domain_priors:suppression_score(uk_unpaid_care_system, S), S >= 0.60.

:- end_tests(uk_unpaid_care_system_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.85): This score reflects the immense, one-way
 *     transfer of value (£162bn per year) from unpaid carers to the state.
 *   - Suppression (0.90): The lack of affordable, accessible professional care
 *     options, combined with social and familial pressures, leaves carers with
 *     virtually no alternative. This is a highly coercive system.
 *   - The combination of extremely high extraction and high suppression makes
 *     this a canonical Snare.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the carer (powerless/trapped), the system is a
 *   Snare that consumes their life, finances, and well-being for minimal
 *   compensation. For the state (institutional/arbitrage), it is a Rope—a
 *   massively beneficial coordination solution that outsources a huge social
 *   cost for free. The state can tell a story of "supporting families" while
 *   benefiting from the extractive reality.
 *
 * DIRECTIONALITY LOGIC:
 *   The direction of value flow is unambiguous. Labor, opportunity cost, and
 *   financial resources flow from the `unpaid_carers` (victim group) to the
 *   `uk_government_and_taxpayers` (beneficiary group). The beneficiary/victim
 *   declarations directly model this structural reality, allowing the engine
 *   to derive the correct directionality (d) for each agent and reveal the
 *   Rope/Snare gap.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 *   This case is a powerful guard against mandatrophy. A naive analysis might
 *   frame this as a "social contract" or a necessary coordination mechanism.
 *   The Deferential Realism framework, by indexing to the powerless agent and
 *   quantifying the effective extraction (χ), correctly identifies the system's
 *   predatory nature from the perspective of those trapped within it. It
 *   prevents the beneficiary's "Rope" narrative from obscuring the victim's
 *   "Snare" reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_uk_unpaid_care_system,
    'Is the system a deliberately designed Snare to minimize state costs, or a path-dependent outcome of policy decay and demographic shifts (a degraded system)?',
    'A historical analysis of UK social care policy decisions since the 1970s, comparing stated intentions with funding allocations and outcomes.',
    'If deliberate, it is a pure Snare. If decayed, it might have elements of a Piton, where institutional inertia prevents reform of a broken system.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_unpaid_care_system, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Required as base_extractiveness (0.85) > 0.46.
% Modeling a decade where austerity, an aging population, and NHS pressures
% have increased the burden on unpaid carers.

% Theater ratio: Token support payments may have increased slightly, but not
% in line with the rising costs, making them feel more performative over time.
narrative_ontology:measurement(uk_care_tr_t0, uk_unpaid_care_system, theater_ratio, 0, 0.15).
narrative_ontology:measurement(uk_care_tr_t5, uk_unpaid_care_system, theater_ratio, 5, 0.18).
narrative_ontology:measurement(uk_care_tr_t10, uk_unpaid_care_system, theater_ratio, 10, 0.20).

% Extraction: The economic value extracted has grown as the population ages
% and formal services are cut, increasing the hours carers must provide.
narrative_ontology:measurement(uk_care_ex_t0, uk_unpaid_care_system, base_extractiveness, 0, 0.80).
narrative_ontology:measurement(uk_care_ex_t5, uk_unpaid_care_system, base_extractiveness, 5, 0.82).
narrative_ontology:measurement(uk_care_ex_t10, uk_unpaid_care_system, base_extractiveness, 10, 0.85).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% This system functions as a mechanism for allocating the resource of care labor.
narrative_ontology:coordination_type(uk_unpaid_care_system, resource_allocation).

% This Snare is structurally linked to failures in other parts of the UK state.
% Its existence allows underfunding in formal social care and eases pressure on the NHS.
narrative_ontology:affects_constraint(uk_unpaid_care_system, uk_social_care_funding_crisis).
narrative_ontology:affects_constraint(uk_unpaid_care_system, uk_nhs_capacity_limits).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The standard derivation chain
% (beneficiary/victim declarations + exit options) accurately models the
% structural relationships and produces the correct perspectival gap.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */