% ============================================================================
% CONSTRAINT STORY: cascading_constraint_failure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cascading_constraint_failure, []).

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
 *   constraint_id: cascading_constraint_failure
 *   human_readable: The Dominos of Systemic Collapse
 *   domain: technological/infrastructural/economic
 *
 * SUMMARY:
 *   This constraint describes the failure mode of modern, tightly-coupled
 *   technological, infrastructural, and economic systems. In pursuit of
 *   efficiency, these systems (e.g., just-in-time supply chains, centralized
 *   power grids, high-frequency trading) eliminate buffers and redundancies.
 *   This tight coupling is a Rope in normal operation, enabling high
 *   performance. However, it also creates fragility, where a single failure
 *   can propagate uncontrollably, leading to a systemic collapse. The
 *   constraint is not a single rule but the emergent property of the system's
 *   architecture.
 *
 * KEY AGENTS:
 *   - End Users / General Public: Primary victim (powerless/trapped) — Depend on the system and bear the full cost of its failure.
 *   - System Architects / Efficiency Investors: Primary beneficiary (institutional/arbitrage) — Profit from the system's hyper-efficiency and can externalize the tail risk.
 *   - System Operators: Secondary victim (moderate/constrained) — Manage the system's day-to-day function and its inherent fragility.
 *   - Regulatory Bodies: Institutional actor (institutional/constrained) — Tasked with preventing collapse, but their tools are often performative and inadequate (Piton).
 *   - Resilience Advocates: Organized agents (organized/mobile) — Develop alternative, decentralized systems, viewing the mainstream system as a temporary scaffold.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cascading_constraint_failure, 0.55).
domain_priors:suppression_score(cascading_constraint_failure, 0.75).
domain_priors:theater_ratio(cascading_constraint_failure, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cascading_constraint_failure, extractiveness, 0.55).
narrative_ontology:constraint_metric(cascading_constraint_failure, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(cascading_constraint_failure, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cascading_constraint_failure, tangled_rope).
narrative_ontology:human_readable(cascading_constraint_failure, "The Dominos of Systemic Collapse").
narrative_ontology:topic_domain(cascading_constraint_failure, "technological/infrastructural/economic").

domain_priors:requires_active_enforcement(cascading_constraint_failure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cascading_constraint_failure, system_designers).
narrative_ontology:constraint_beneficiary(cascading_constraint_failure, efficiency_investors).
narrative_ontology:constraint_victim(cascading_constraint_failure, end_users).
narrative_ontology:constraint_victim(cascading_constraint_failure, general_public).
narrative_ontology:constraint_victim(cascading_constraint_failure, downstream_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END USER (SNARE) — Trapped within critical infrastructure (power, finance, supply chains) with no viable alternatives. They bear the full, catastrophic cost of a systemic failure they cannot prevent or exit. The latent risk itself is the extraction. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.78.
constraint_indexing:constraint_classification(cascading_constraint_failure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SYSTEM ARCHITECT (ROPE) — Experiences the system as a pure coordination mechanism that creates immense efficiency. Benefits from the tight coupling and can hedge or exit before a collapse. The risk is externalized. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.08. Negative effective extraction indicates a net subsidy.
constraint_indexing:constraint_classification(cascading_constraint_failure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: SYSTEM OPERATOR (TANGLED ROPE) — Experiences both the coordination benefits (the system functions) and the extractive costs (managing extreme fragility, high stress, blame for failures). They are constrained within the system's design. d≈0.75, f(d)≈1.10, σ=0.9 → χ≈0.54.
constraint_indexing:constraint_classification(cascading_constraint_failure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: REGULATORY BODY (PITON) — The regulatory framework designed to prevent such collapses is largely performative. Stress tests and compliance checks are theatrical rituals that cannot model or prevent true systemic cascades. The institution persists through inertia despite its functional degradation. theater_ratio=0.75 satisfies the piton gate (≥0.70).
constraint_indexing:constraint_classification(cascading_constraint_failure, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: RESILIENCE ADVOCATE (SCAFFOLD) — Views the tightly-coupled centralized system as a temporary, fragile scaffold. They are actively building alternative, decentralized, and resilient systems (e.g., microgrids, local supply chains, crypto-finance) with the expectation that the old system will eventually fail and be replaced. The sunset clause is the inevitable collapse itself.
constraint_indexing:constraint_classification(cascading_constraint_failure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a sufficiently abstract perspective, the tendency of optimized, complex adaptive systems to become 'robust yet fragile' and exist at a critical state is an emergent, law-like property. This view naturalizes the risk of collapse as an unavoidable feature of complexity itself. The engine will flag this as a false summit, as the high ε and suppression values are products of design choices, not natural law.
constraint_indexing:constraint_classification(cascading_constraint_failure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cascading_constraint_failure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cascading_constraint_failure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cascading_constraint_failure, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cascading_constraint_failure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cascading_constraint_failure, TR),
    TR >= 0.70.

:- end_tests(cascading_constraint_failure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.55): Represents the latent risk of catastrophic failure that is transferred from system designers to end users. It's not a continuous drain but a potential, massive loss imposed on the powerless. Suppression (0.75): High. For critical infrastructure like the power grid or financial system, there are no meaningful alternatives for the average person. Opting out is not possible. Theater Ratio (0.75): High. Regulatory oversight, such as bank stress tests or infrastructure audits, often becomes a performative ritual that creates a false sense of security while failing to address the underlying structural fragility.
 *
 * PERSPECTIVAL GAP:
 *   This is a diagnostic exemplar for indexical classification. The same system is a high-efficiency Rope to its architect, a latent Snare to its user, a degraded Piton to its regulator, a temporary Scaffold to its replacement's designer, a tragic Tangled Rope to its operator, and a law-like Mountain to the abstract theorist. The 'truth' of the constraint is the full presheaf of these perspectives; no single classification is complete.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (System Designers) have arbitrage exit and benefit from the efficiency, leading to a low/negative 'd' value and a Rope classification. Victims (End Users) are trapped and bear all risk, leading to a high 'd' value and a Snare classification. Other agents fall in between based on their mix of costs, benefits, and exit options, producing the full range of types from the same base metrics.
 *
 * MANDATROPHY ANALYSIS:
 *   This story resolves the mandatrophy by demonstrating that a single structural reality can, and should, generate multiple, seemingly contradictory classifications. The error is to seek a single 'correct' type. The system's function is to show how the beneficiary's Rope is simultaneously the victim's Snare. The high base extractiveness and suppression correctly identify the potential for severe harm, while the perspectival analysis maps out how that harm is distributed and perceived across the network of agents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficiency_fragility_tradeoff,
    'Is the trade-off between system efficiency and fragility a fundamental, unavoidable law (Mountain) or a contingent policy choice that prioritizes short-term gains (Tangled Rope)?',
    'Comparative analysis of systems with different design philosophies (e.g., centralized vs. decentralized power grids) and their performance under stress.',
    'If fundamental, the risk is irreducible. If a choice, alternative designs could mitigate it, making the current system a Snare by suppressing safer alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(efficiency_fragility_tradeoff, conceptual, 'Whether the efficiency-fragility trade-off is a natural law or a policy choice.').

omega_variable(
    black_swan_predictability,
    'Are systemic collapses truly unpredictable ''black swan'' events, or are they predictable ''gray rhino'' failures of a known fragile design?',
    'Forensic analysis of past collapses (e.g., 2008 financial crisis, Texas power grid failure) to identify pre-existing warnings and ignored risk assessments.',
    'If predictable, the failure to act constitutes a higher degree of extraction (Snare). If truly unpredictable, the system is closer to a tragic Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_swan_predictability, empirical, 'Distinguishing unpredictable ''black swans'' from predictable failures.').

omega_variable(
    decoupling_cost,
    'What is the true economic and social cost of building more resilient, less-coupled systems, and who would bear that cost?',
    'Techno-economic modeling of decentralized vs. centralized infrastructure, including transition costs and changes in efficiency.',
    'If costs are prohibitively high, the current system may be a tragic Tangled Rope. If costs are manageable but opposed by incumbents, it is a Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decoupling_cost, empirical, 'The cost-benefit analysis of building resilient, decoupled alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cascading_constraint_failure, 1980, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(casc_tr_t1980, cascading_constraint_failure, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(casc_tr_t2005, cascading_constraint_failure, theater_ratio, 2005, 0.55).
narrative_ontology:measurement(casc_tr_t2030, cascading_constraint_failure, theater_ratio, 2030, 0.75).

% Extraction over time
narrative_ontology:measurement(casc_be_t1980, cascading_constraint_failure, base_extractiveness, 1980, 0.2).
narrative_ontology:measurement(casc_be_t2005, cascading_constraint_failure, base_extractiveness, 2005, 0.4).
narrative_ontology:measurement(casc_be_t2030, cascading_constraint_failure, base_extractiveness, 2030, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cascading_constraint_failure, resource_allocation).
narrative_ontology:affects_constraint(cascading_constraint_failure, global_financial_system).
narrative_ontology:affects_constraint(cascading_constraint_failure, national_power_grid).
narrative_ontology:affects_constraint(cascading_constraint_failure, just_in_time_supply_chains).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
