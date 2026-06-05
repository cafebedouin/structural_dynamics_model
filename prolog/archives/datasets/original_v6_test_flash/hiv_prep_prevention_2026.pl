% ============================================================================
% CONSTRAINT STORY: hiv_prep_prevention_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hiv_prep_prevention_2026, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hiv_prep_prevention_2026
 *   human_readable: PrEP-Mediated HIV Prevention
 *   domain: technological/social
 *
 * SUMMARY:
 *   Pre-exposure prophylaxis (PrEP) is a daily antiviral medication that
 *   reduces the risk of HIV transmission. While highly effective when used
 *   correctly, access barriers, potential side effects, and behavioral
 *   changes create a complex interplay of benefits and drawbacks. The
 *   constraint's extractiveness (0.45) is considered moderate since its
 *   success comes at a price point and requires consistent effort. The
 *   suppresion (0.35) is driven by limited access in some locations and
 *   discomfort with side effects in others.
 *
 * KEY AGENTS:
 *   - Pharmaceutical Companies: Primary beneficiary (institutional/arbitrage) through increased sales.
 *   - Individuals with Limited Access: Primary victim (powerless/trapped) due to disparities.
 *   - Individuals Experiencing Side Effects: Secondary victim (moderate/constrained) balancing benefits and risks.
 *   - Public Health Organizations: (Organized/constrained) Balancing benefits of prevention vs resources and behavioral changes.
 *   - Analytical Observer: (analytical/analytical) Balancing the benefits of prevention with the complications of side effects and behavioral modification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hiv_prep_prevention_2026, 0.45).
domain_priors:suppression_score(hiv_prep_prevention_2026, 0.35).
domain_priors:theater_ratio(hiv_prep_prevention_2026, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hiv_prep_prevention_2026, extractiveness, 0.45).
narrative_ontology:constraint_metric(hiv_prep_prevention_2026, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(hiv_prep_prevention_2026, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hiv_prep_prevention_2026, tangled_rope).
narrative_ontology:human_readable(hiv_prep_prevention_2026, "PrEP-Mediated HIV Prevention").
narrative_ontology:topic_domain(hiv_prep_prevention_2026, "technological/social").

domain_priors:requires_active_enforcement(hiv_prep_prevention_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hiv_prep_prevention_2026, pharmaceutical_companies).
narrative_ontology:constraint_beneficiary(hiv_prep_prevention_2026, public_health_organizations).
narrative_ontology:constraint_victim(hiv_prep_prevention_2026, individuals_with_limited_access).
narrative_ontology:constraint_victim(hiv_prep_prevention_2026, individuals_experiencing_side_effects).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Individuals with limited access to healthcare and resources may face significant barriers to accessing PrEP, effectively trapping them in a high-risk situation. This perspective represents a snare.
constraint_indexing:constraint_classification(hiv_prep_prevention_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Perspective 2: Pharmaceutical companies benefit from the widespread adoption of PrEP through increased sales and revenue, experiencing the constraint as a rope, a coordinated system that brings positive results.
constraint_indexing:constraint_classification(hiv_prep_prevention_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 3: Individuals who experience side effects from PrEP are in a constrained position. They benefit from HIV prevention but suffer potential harm. It's a tangled rope.
constraint_indexing:constraint_classification(hiv_prep_prevention_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Perspective 4: Public health organizations benefit through the prevention of new HIV infections and decreasing cost of HIV treatment over time. But their resources are limited, making it a tangled rope.
constraint_indexing:constraint_classification(hiv_prep_prevention_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 5: From an analytical standpoint, PrEP represents a complex interplay of benefits and drawbacks. It has successfully prevented HIV transmission, but it has also generated concerns around equity, side effects, and potential risk compensation.
constraint_indexing:constraint_classification(hiv_prep_prevention_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hiv_prep_prevention_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hiv_prep_prevention_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hiv_prep_prevention_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(hiv_prep_prevention_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   PreP involves coordination via drug manufacturers. Active enforcement is required due to patient compliance requirements. Access constraints and the possibility of adverse side effects exist. Increased risky behaviors could result in lower effectiveness of PrEP; reduces coordination.
 *
 * PERSPECTIVAL GAP:
 *   The pharmaceutical companies profit substantially, while individuals with limited access remain at high risk. Those with side effects must bear the burden of potential ill effects. The benefits and drawbacks are not evenly distributed. An organized effort, if pursued, might be able to resolve these disparities.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality depends on the interplay between exit options, power atoms, and structural beneficiary/victim relationships. The pharmaceutical firm experiences the relationship as coordination, public health faces resource constraints, and certain communities are trapped. The agent's power and potential action directly impact the extraction calculation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not easily mislabeled. The presence of both coordination and extraction aspects suggests that it is a tangled rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    access_equity,
    'How can equitable access to PrEP be ensured across different socioeconomic and demographic groups?',
    'Implementation of targeted outreach programs, subsidies, and policy changes to address disparities.',
    'Improved access reduces the snare for marginalized groups; resolves some extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(access_equity, empirical, 'Addresses concerns of equitable PrEP access.').

omega_variable(
    risk_compensation,
    'Does PrEP use lead to risk compensation, diminishing its overall effectiveness in HIV prevention?',
    'Monitoring sexual behavior patterns, HIV incidence rates, and STD prevalence among PrEP users.',
    'Increased risky behavior undermines the effectiveness of PrEP; reduces coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(risk_compensation, empirical, 'Identifies potential risk compensation behaviors').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hiv_prep_prevention_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hiv__tr_t0, hiv_prep_prevention_2026, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hiv__tr_t5, hiv_prep_prevention_2026, theater_ratio, 5, 0.15).
narrative_ontology:measurement(hiv__tr_t10, hiv_prep_prevention_2026, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(hiv__be_t0, hiv_prep_prevention_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hiv__be_t5, hiv_prep_prevention_2026, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(hiv__be_t10, hiv_prep_prevention_2026, base_extractiveness, 10, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hiv_prep_prevention_2026, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
