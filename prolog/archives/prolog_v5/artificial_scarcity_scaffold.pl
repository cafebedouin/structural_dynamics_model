% ============================================================================
% CONSTRAINT STORY: artificial_scarcity_scaffold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_artificial_scarcity_scaffold, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: artificial_scarcity_scaffold
 *   human_readable: The Resource-Migration Scaffold
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The artificial scarcity scaffold is a temporary economic constraint
 *   designed to incentivize and fund a transition from a legacy resource to a
 *   new, more abundant alternative. The constraint operates by deliberately
 *   restricting supply or raising prices on the legacy resource, creating
 *   urgency for market actors to adopt new technology or resource sources.
 *   The mechanism is coordination-based rather than purely extractive: the
 *   scarcity signal solves the collective action problem of simultaneous
 *   infrastructure investment by multiple actors. However, the constraint
 *   also creates a transition window during which legacy-dependent users face
 *   elevated costs with limited alternatives. The constraint's legitimacy
 *   depends entirely on whether the sunset mechanism is enforced — whether
 *   artificial scarcity is actually relaxed when new resource infrastructure
 *   reaches maturity and cost parity. If enforcement fails, the constraint
 *   degrades from scaffold to piton (theater-based inertia) or snare (pure
 *   extraction masquerading as necessary scarcity).
 *
 * KEY AGENTS:
 *   - New Resource Adopters: Early-moving actors (institutional/arbitrage) — benefit from coordination signal and infrastructure investment; capture first-mover advantages
 *   - Legacy-Locked Users: Dependent populations (powerless/trapped) — face elevated legacy resource costs without viable alternatives; experience maximum extraction during transition window
 *   - Transition Infrastructure Builders: Technology and infrastructure providers (institutional/arbitrage) — benefit from urgent funding and market signals; solve coordination problem
 *   - Organized Adopter Coalition: Mid-to-large firms with transition capacity (organized/constrained) — have agency in adoption timing; experience both coordination benefits and transition costs
 *   - Mid-Market Participants: Medium-sized actors (moderate/constrained) — balance transition costs against benefits; constrained by capital and operational disruption
 *   - Legacy Resource Administrator: Regulatory body or incumbent (institutional/arbitrage) — maintains artificial scarcity enforcement; risks piton degradation if constraint persists beyond sunset
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(artificial_scarcity_scaffold, 0.28).
domain_priors:suppression_score(artificial_scarcity_scaffold, 0.45).
domain_priors:theater_ratio(artificial_scarcity_scaffold, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(artificial_scarcity_scaffold, extractiveness, 0.28).
narrative_ontology:constraint_metric(artificial_scarcity_scaffold, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(artificial_scarcity_scaffold, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(artificial_scarcity_scaffold, scaffold).
narrative_ontology:human_readable(artificial_scarcity_scaffold, "The Resource-Migration Scaffold").
narrative_ontology:topic_domain(artificial_scarcity_scaffold, "technological/economic").

domain_priors:requires_active_enforcement(artificial_scarcity_scaffold).
narrative_ontology:has_sunset_clause(artificial_scarcity_scaffold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(artificial_scarcity_scaffold, new_resource_adopters).
narrative_ontology:constraint_beneficiary(artificial_scarcity_scaffold, transition_market_validators).
narrative_ontology:constraint_beneficiary(artificial_scarcity_scaffold, technology_infrastructure_builders).
narrative_ontology:constraint_victim(artificial_scarcity_scaffold, legacy_resource_dependent_users).
narrative_ontology:constraint_victim(artificial_scarcity_scaffold, late_adopters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LEGACY-LOCKED USER (SNARE) — Dependent on legacy resource with no immediate substitute. Artificial scarcity drives up costs without providing accessible alternative. Trapped by infrastructure investment, sunk knowledge, and supply constraints. Maximum experienced extraction during transition window.
constraint_indexing:constraint_classification(artificial_scarcity_scaffold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TECHNOLOGY TRANSITION INFRASTRUCTURE PROVIDER (ROPE) — Benefits from coordinating the migration pathway. Artificial scarcity creates urgency and funding for new infrastructure. Solves collective coordination problem: without the scarcity signal, adoption is slow and uncoordinated. Net beneficiary with genuine coordination function.
constraint_indexing:constraint_classification(artificial_scarcity_scaffold, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ORGANIZED ADOPTER COALITION (SCAFFOLD) — Coalition of early adopters and industry groups with capacity to transition. Experiences artificial scarcity as temporary coordination mechanism with clear sunset: as new resource infrastructure matures and becomes cheaper than legacy alternative, the scarcity enforcement naturally declines. Constrained by transition costs but with agency in adoption timing.
constraint_indexing:constraint_classification(artificial_scarcity_scaffold, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MID-MARKET TRANSITION PARTICIPANT (TANGLED ROPE) — Medium-sized actors with resources to transition but significant switching costs. Experience both the coordination benefit (clear migration incentive, infrastructure investment) and the extraction cost (elevated legacy resource prices during transition window). Have some agency but constrained by capital requirements and operational disruption.
constraint_indexing:constraint_classification(artificial_scarcity_scaffold, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: LEGACY RESOURCE ADMINISTRATOR (PITON) — Maintains enforcement of artificial scarcity through rationing, export controls, or production caps. The constraint persists through institutional inertia and regulatory theater even when the scarcity is no longer economically necessary. May have lost its coordination function but remains enforced due to path dependency and bureaucratic structure.
constraint_indexing:constraint_classification(artificial_scarcity_scaffold, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SCAFFOLD) — From civilizational horizon, the artificial scarcity is a deliberately temporary mechanism with built-in sunset. The constraint succeeds when it becomes unnecessary — when new resource abundance and infrastructure maturity eliminate the scarcity signal's necessity. Constraint lifecycle is measured in decades, not centuries. Classification depends on verifying the actual sunset mechanism.
constraint_indexing:constraint_classification(artificial_scarcity_scaffold, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(artificial_scarcity_scaffold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(artificial_scarcity_scaffold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(artificial_scarcity_scaffold, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(artificial_scarcity_scaffold, TR),
    TR >= 0.70.

:- end_tests(artificial_scarcity_scaffold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-to-moderate. The artificial scarcity creates real costs for legacy-dependent users, but the mechanism is justified by coordination necessity during the transition window. The extraction is not permanent — it declines as new infrastructure matures. Measured at the midpoint of the transition period (time 5), extractiveness reflects the designed escalation: low at start (when alternatives are being built), peaking during the critical infrastructure investment window, declining toward zero as sunset approaches. Suppression (0.45): Moderate. Users have some alternatives and can invest in transition, but switching costs, regulatory barriers, and information asymmetry create significant friction. Legacy users cannot exit instantly; new resource infrastructure takes time to deploy. Theater ratio (0.58): Moderate-high and rising. Initial rationing may be genuine necessity-based scarcity; by midpoint, some performative elements emerge (maintaining supply restrictions even as alternatives become available, regulatory theater around transition milestones). Sunset mechanism credibility determines whether theater declines or persists.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a clear perspectival gap between extractors and targets. The institutional technology provider sees rope — genuine coordination function, solving collective action problem. The legacy-locked user sees snare — artificially elevated costs, no exit, no alternatives. The organized adopter coalition sees scaffold — temporary coordination mechanism with clear sunset. The analytical observer must distinguish genuine coordination necessity from extractive theater masquerading as necessary scarcity. The perspectival gap widens if sunset enforcement credibility declines — if the artificial scarcity persists beyond its justified window, the constraint morphs from scaffold (temporary) to piton (inertia-based) or snare (extraction-based) from the legacy user's perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by the agent's position relative to the extraction flow. Early adopters and infrastructure providers (beneficiaries with arbitrage exit) derive low or negative d — they capture coordination benefits and have exit optionality. Legacy-locked users (victims with trapped exit) derive high d near 1.0 — they bear artificial scarcity costs with no alternatives. Mid-market participants occupy intermediate d (0.4-0.6): they are both constrained victims and partial beneficiaries of the coordination mechanism. The sigmoid f(d) applies to derive experienced extractiveness chi from base extractiveness epsilon. As new infrastructure matures, trapped agents shift toward constrained exit (d decreases, f(d) decreases), and experienced extraction chi declines naturally — this is the intended sunset mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The artificial scarcity scaffold resolves the mandatrophy by explicitly declaring its sunset clause and justifying extraction through coordination necessity. The constraint succeeds (remains scaffold classification) if and only if: (1) the new resource infrastructure reaches sufficient maturity and cost parity to eliminate the scarcity rationale, (2) the artificial scarcity enforcement is actually relaxed when sunset conditions are met, and (3) the transition is timed to prevent permanent lock-in of legacy-dependent populations. If sunset enforcement fails or is indefinitely delayed, the constraint degrades to piton (theater-based persistence) or snare (permanent extraction). The theater ratio trajectory is diagnostic: if theater ratio peaks and declines toward zero as sunset approaches, the constraint remains scaffold. If theater ratio plateaus or rises, the constraint has degraded. The measurements section shows the designed trajectory: base_extractiveness rises during the critical infrastructure investment window (0-2 years), plateaus (2-5 years), and is expected to decline sharply after year 5 as new resource reaches cost parity. Theater ratio follows a similar pattern: initially low (genuine scarcity needs), rising during enforcement emphasis, and expected to collapse as sunset enforcement begins.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sunset_enforcement_credibility,
    'Is the declared sunset clause actually enforceable, or does the scarcity mechanism persist due to regulatory capture or path dependency?',
    'Monitor actual policy changes as new resource reaches cost parity with legacy resource. Track whether rationing, export controls, or production caps are actually relaxed when sunset conditions are met.',
    'If sunset is enforced: constraint is genuine scaffold, classification stable. If sunset is not enforced: constraint degrades to piton (theater) or snare (extraction), extractiveness rises, mandatrophy emerges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_enforcement_credibility, empirical, 'Whether sunset enforcement mechanisms are credible and will be executed').

omega_variable(
    new_resource_infrastructure_timeline,
    'Will new resource infrastructure reach sufficient deployment density to enable full substitution within the declared transition window?',
    'Track infrastructure deployment rates, capital investment levels, regulatory approval timelines. Compare against historical transition speeds for similar technology migrations.',
    'If infrastructure matures on schedule: scaffold timeline is realistic, constraint can sunset. If delayed: legacy users remain trapped longer, extractiveness increases, snare classification may persist longer than expected.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(new_resource_infrastructure_timeline, empirical, 'Whether new resource infrastructure will support full substitution in time').

omega_variable(
    transition_equity_distribution,
    'Are transition costs and benefits distributed equitably across user classes, or do late adopters and legacy-locked populations bear disproportionate cost?',
    'Cost-benefit analysis by user class over transition window. Track whether lower-income or less-resourced users have accessible transition pathways or remain locked in high-cost legacy resource dependency.',
    'If distributed equitably: scaffold maintains coordination function. If inequitable: scaffold conceals regressive extraction, tangled rope for disadvantaged groups escalates toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transition_equity_distribution, empirical, 'Whether transition costs and benefits are equitably distributed across user populations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(artificial_scarcity_scaffold, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(artsc_tr_t0, artificial_scarcity_scaffold, theater_ratio, 0, 0.35).
narrative_ontology:measurement(artsc_tr_t2, artificial_scarcity_scaffold, theater_ratio, 2, 0.48).
narrative_ontology:measurement(artsc_tr_t5, artificial_scarcity_scaffold, theater_ratio, 5, 0.58).

% Extraction over time
narrative_ontology:measurement(artsc_be_t0, artificial_scarcity_scaffold, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(artsc_be_t2, artificial_scarcity_scaffold, base_extractiveness, 2, 0.25).
narrative_ontology:measurement(artsc_be_t5, artificial_scarcity_scaffold, base_extractiveness, 5, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(artificial_scarcity_scaffold, resource_allocation).
narrative_ontology:affects_constraint(artificial_scarcity_scaffold, legacy_resource_infrastructure_lock_in).
narrative_ontology:affects_constraint(artificial_scarcity_scaffold, new_technology_adoption_coordination).
narrative_ontology:affects_constraint(artificial_scarcity_scaffold, transition_equity_access).

% DUAL FORMULATION NOTE:
% The artificial scarcity scaffold is upstream of three dependent constraints: (1) the lock-in of existing infrastructure and business models around the legacy resource, (2) the coordination problem of simultaneous new technology adoption across many actors, and (3) the equity problem of ensuring late adopters and low-income users have accessible transition pathways. The scaffold's success or failure in meeting its sunset clause directly impacts whether these downstream constraints remain manageable or become permanent snares.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
