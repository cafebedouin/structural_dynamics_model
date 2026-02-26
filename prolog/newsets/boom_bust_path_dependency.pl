% ============================================================================
% CONSTRAINT STORY: boom_bust_path_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_boom_bust_path_dependency, []).

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
 *   constraint_id: boom_bust_path_dependency
 *   human_readable: The Heritage Fund Piton (Fiscal Volatility Path)
 *   domain: economic/policy
 *
 * SUMMARY:
 *   This constraint models the path dependency of Alberta's fiscal policy,
 *   rooted in the management of its Heritage Fund. Established to save
 *   resource wealth and stabilize revenues, the fund's purpose was undermined
 *   by policy choices starting in the 1980s to halt contributions and
 *   prioritize low royalties. This created a boom-bust fiscal structure where
 *   resource revenue is used for current spending and tax suppression in good
 *   times, leading to severe public service cuts in bad times. The Heritage
 *   Fund now functions as a 'Piton': its original purpose has atrophied, but
 *   it persists as a powerful political symbol of fiscal prudence, masking
 *   the underlying volatility.
 *
 * KEY AGENTS:
 *   - Public Service Recipients: Primary victims (powerless/trapped) who bear the cost of budget cuts.
 *   - Future Generations: Abstract victims (powerless/trapped) who inherit a depleted fund and volatile fiscal structure.
 *   - Oil and Gas Industry: Primary beneficiary (institutional/arbitrage) who profits from low royalty rates.
 *   - Current Taxpayers (Boom Times): Secondary beneficiaries (organized/mobile) who receive tax cuts funded by non-renewable revenues.
 *   - Provincial Government: Institutional actor (institutional/constrained) that maintains the inertial system for political expediency.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(boom_bust_path_dependency, 0.55).
domain_priors:suppression_score(boom_bust_path_dependency, 0.75).
domain_priors:theater_ratio(boom_bust_path_dependency, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(boom_bust_path_dependency, extractiveness, 0.55).
narrative_ontology:constraint_metric(boom_bust_path_dependency, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(boom_bust_path_dependency, theater_ratio, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(boom_bust_path_dependency, tangled_rope).
narrative_ontology:human_readable(boom_bust_path_dependency, "The Heritage Fund Piton (Fiscal Volatility Path)").
narrative_ontology:topic_domain(boom_bust_path_dependency, "economic/policy").

domain_priors:requires_active_enforcement(boom_bust_path_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(boom_bust_path_dependency, oil_and_gas_industry).
narrative_ontology:constraint_beneficiary(boom_bust_path_dependency, current_taxpayers_boom_times).
narrative_ontology:constraint_beneficiary(boom_bust_path_dependency, political_incumbents).
narrative_ontology:constraint_victim(boom_bust_path_dependency, future_generations_of_albertans).
narrative_ontology:constraint_victim(boom_bust_path_dependency, public_service_recipients).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PUBLIC SERVICE RECIPIENT (SNARE) — Experiences budget cuts to essential services (health, education) during bust cycles. They are trapped within the provincial system and bear the direct costs of fiscal instability. d≈0.95, f(d)≈1.42, σ=0.9 → χ≈0.70. This high effective extraction meets the Snare threshold.
constraint_indexing:constraint_classification(boom_bust_path_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: OIL AND GAS INDUSTRY (ROPE) — Benefits from a low and stable royalty regime, viewing it as a pure coordination mechanism to attract capital investment. Can exit via capital flight if the regime changes. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.09. The negative extraction indicates a net subsidy.
constraint_indexing:constraint_classification(boom_bust_path_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: PROVINCIAL GOVERNMENT (PITON) — Manages the Heritage Fund, which has become largely performative. Its original function as a powerful fiscal stabilizer has atrophied, but it persists as a political symbol of prudence. The high theater_ratio (0.80) triggers the Piton classification. The government is constrained by political path dependency.
constraint_indexing:constraint_classification(boom_bust_path_dependency, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FISCAL CONSERVATIVE TAXPAYER (ROPE) — As an organized voting bloc, they benefit from and support the low-tax environment funded by resource revenues instead of broad-based taxes. They see the system as a successful coordination mechanism for their primary interest. d≈0.25, f(d)≈0.12, σ=1.0 → χ≈0.07.
constraint_indexing:constraint_classification(boom_bust_path_dependency, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees the full structure: a system that coordinates benefits for some (industry, current taxpayers) while extracting heavily from others (future generations, public services). The high suppression of alternatives and active political enforcement confirm the Tangled Rope classification. d≈0.73, f(d)≈1.15, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(boom_bust_path_dependency, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(boom_bust_path_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(boom_bust_path_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(boom_bust_path_dependency, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(boom_bust_path_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(boom_bust_path_dependency, TR),
    TR >= 0.70.

:- end_tests(boom_bust_path_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) represents the significant opportunity cost borne by the public and future generations—wealth that was not saved or invested in durable public goods. Suppression (0.75) is high due to a deeply entrenched political ideology that actively resists alternatives like higher royalties, a sales tax, or Norway-style savings discipline. Theater Ratio (0.80) is very high, as the Heritage Fund is politically celebrated as a major achievement, while its actual function as a fiscal stabilizer has been negligible for decades compared to its potential.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For industry and low-tax advocates, the system is a Rope—a successful coordination of interests. For the government managing it, it's a Piton—an inertial, performative tool. For those dependent on public services, it is a Snare—a trap of fiscal instability they cannot escape. The analytical observer sees the whole picture as a Tangled Rope, where the 'coordination' for some is enabled by active, coercive extraction from others.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries like the oil industry have arbitrage exit options, driving their directionality (d) near zero and making effective extraction (χ) negative (a subsidy). Victims like public service recipients are trapped, driving their 'd' near one and χ to its maximum, resulting in a Snare. The government is constrained by political ideology, and its perspective is dominated by the high theater ratio, yielding a Piton. The analytical view, balancing these factors, classifies the overall structure as a Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This case resolves a potential mandatrophy by demonstrating how a single policy structure can be correctly classified as multiple constraint types simultaneously. It avoids mislabeling the system as 'just' a coordination failure (Rope) or 'just' a degraded institution (Piton) by acknowledging the severe, coercive extraction experienced by its victims (Snare). The framework correctly identifies that the Piton (the fund itself) is a key component of the broader Tangled Rope (the fiscal policy regime).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_curse_contingency,
    'Is Alberta''s fiscal volatility an inevitable outcome of a ''resource curse'' (Mountain-like), or is it the result of contingent policy choices that could be reversed (Tangled Rope)?',
    'Comparative analysis with other resource-rich jurisdictions (e.g., Norway, Alaska) that have implemented different fiscal models.',
    'If deemed inevitable, the constraint is closer to a Mountain, and policy interventions are futile. If contingent, it confirms the Tangled Rope/Snare classifications and implies that alternative policies are possible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resource_curse_contingency, conceptual, 'Distinguishing between inevitable resource curse and contingent policy choices.').

omega_variable(
    political_will_threshold,
    'What level of sustained fiscal crisis or public pressure would be required to overcome the political path dependency and reform the royalty and savings structure?',
    'Historical case studies of major policy shifts in other jurisdictions; polling data on public appetite for higher taxes or royalty reviews during economic downturns.',
    'Defines the conditions under which the constraint could be dismantled or transition to a Scaffold. Without reaching this threshold, the Piton/Snare structure remains stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_will_threshold, empirical, 'The threshold of crisis needed to break political path dependency.').

omega_variable(
    fair_royalty_rate,
    'What constitutes a ''fair share'' of resource revenue for the public versus a ''competitive'' rate for industry?',
    'This is not empirically resolvable, as it depends on competing economic models and normative values regarding public ownership of resources.',
    'The perceived fairness of the royalty rate determines whether the system is viewed as a legitimate Rope (coordination) or an extractive Tangled Rope/Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fair_royalty_rate, preference, 'The normative and unresolvable question of a ''fair'' royalty rate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(boom_bust_path_dependency, 1987, 2027).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(boom_tr_t1987, boom_bust_path_dependency, theater_ratio, 1987, 0.25).
narrative_ontology:measurement(boom_tr_t2007, boom_bust_path_dependency, theater_ratio, 2007, 0.6).
narrative_ontology:measurement(boom_tr_t2027, boom_bust_path_dependency, theater_ratio, 2027, 0.8).

% Extraction over time
narrative_ontology:measurement(boom_be_t1987, boom_bust_path_dependency, base_extractiveness, 1987, 0.2).
narrative_ontology:measurement(boom_be_t2007, boom_bust_path_dependency, base_extractiveness, 2007, 0.45).
narrative_ontology:measurement(boom_be_t2027, boom_bust_path_dependency, base_extractiveness, 2027, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(boom_bust_path_dependency, resource_allocation).
narrative_ontology:affects_constraint(boom_bust_path_dependency, provincial_service_level_instability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
