% ============================================================================
% CONSTRAINT STORY: circadian_decoupling_arbitrage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_circadian_decoupling_arbitrage, []).

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
 *   constraint_id: circadian_decoupling_arbitrage
 *   human_readable: The Circadian Decoupling Arbitrage
 *   domain: bio_industrial
 *
 * SUMMARY:
 *   This constraint describes the institutionalized decoupling of human
 *   activity from the natural 24-hour solar cycle to enable continuous
 *   industrial and commercial operations. By enforcing work schedules that
 *   defy innate biological rhythms, industrial systems arbitrage the high
 *   capital efficiency of 24/7 operation against the externalized health and
 *   social costs borne by a class of 'shift workers'. This creates a
 *   fundamental conflict between biological reality and economic
 *   optimization.
 *
 * KEY AGENTS:
 *   - Shift Workers: Primary victims (powerless/trapped) — bear the biological costs of circadian disruption.
 *   - Industrial Operators: Primary beneficiaries (institutional/arbitrage) — capture the economic gains from continuous capital utilization.
 *   - Global Consumers: Secondary beneficiaries (moderate/mobile) — benefit from 24/7 services and supply chains.
 *   - Labor Unions: Organized representatives (organized/constrained) — negotiate the terms of the extraction.
 *   - Automation Advocates: Powerful agents (powerful/arbitrage) — view the system as a temporary scaffold to be replaced by technology.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(circadian_decoupling_arbitrage, 0.65).
domain_priors:suppression_score(circadian_decoupling_arbitrage, 0.75).
domain_priors:theater_ratio(circadian_decoupling_arbitrage, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(circadian_decoupling_arbitrage, extractiveness, 0.65).
narrative_ontology:constraint_metric(circadian_decoupling_arbitrage, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(circadian_decoupling_arbitrage, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(circadian_decoupling_arbitrage, tangled_rope).
narrative_ontology:human_readable(circadian_decoupling_arbitrage, "The Circadian Decoupling Arbitrage").
narrative_ontology:topic_domain(circadian_decoupling_arbitrage, "bio_industrial").

domain_priors:requires_active_enforcement(circadian_decoupling_arbitrage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(circadian_decoupling_arbitrage, industrial_operators).
narrative_ontology:constraint_beneficiary(circadian_decoupling_arbitrage, global_consumers).
narrative_ontology:constraint_victim(circadian_decoupling_arbitrage, shift_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SHIFT WORKER (SNARE) — Trapped by economic necessity and limited local alternatives. Bears the full biological and social cost of circadian disruption (metabolic, carcinogenic, psychiatric). The system extracts health and time for wages that don't cover the externality. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.74.
constraint_indexing:constraint_classification(circadian_decoupling_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: INDUSTRIAL OPERATOR (ROPE) — Experiences the constraint as a pure coordination problem: maximizing capital efficiency of 24/7 infrastructure. The biological cost to labor is an externality. From this perspective, shift work is the rope that pulls production and capital together continuously. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.09.
constraint_indexing:constraint_classification(circadian_decoupling_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes both the genuine coordination function (enabling a 24/7 global economy) and the severe, asymmetric extraction of health and well-being from a specific labor class. The claimed type. d≈0.73, f(d)≈1.15, σ=1.2 → χ≈0.90.
constraint_indexing:constraint_classification(circadian_decoupling_arbitrage, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: LABOR UNION (TANGLED ROPE) — Engages with the constraint by negotiating its terms (shift differentials, work hour limits) rather than eliminating it. Acknowledges the coordination need for the industry to exist, but fights to mitigate the extraction. Constrained because they cannot abolish shift work without eliminating the jobs. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.49.
constraint_indexing:constraint_classification(circadian_decoupling_arbitrage, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: AUTOMATION ADVOCATE (SCAFFOLD) — Views human shift work as a temporary, inefficient bridge to a future of full automation. The constraint is a scaffold holding up 24/7 industry until cost-effective robotics can replace human labor, representing a clear sunset clause for the biological extraction.
constraint_indexing:constraint_classification(circadian_decoupling_arbitrage, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: 'PRICE OF PROGRESS' ADVOCATE (MOUNTAIN - FALSE SUMMIT) — This perspective naturalizes the constraint, framing the costs as an unavoidable, immutable law of technological society. The engine will reject this classification as a false summit, as the base properties (ε=0.65, suppression=0.75, requires_active_enforcement=true) are inconsistent with a Mountain.
constraint_indexing:constraint_classification(circadian_decoupling_arbitrage, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(circadian_decoupling_arbitrage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(circadian_decoupling_arbitrage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(circadian_decoupling_arbitrage, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(circadian_decoupling_arbitrage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(circadian_decoupling_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.65) is high, reflecting the severe, well-documented negative health impacts (metabolic syndrome, cancer, mental illness) and social dislocation imposed on workers. Suppression (0.75) is high because for many workers in specific regions or skill brackets, shift work is the only viable employment, making exit options extremely limited. Theater Ratio (0.40) reflects the rise of corporate wellness programs that perform concern for employee well-being (e.g., mindfulness apps) without addressing the root cause of harm—the schedule itself.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the industrial operator, it's a Rope—a simple, elegant solution to a coordination problem. For the worker, it's a Snare—a trap that trades their long-term health for short-term wages. The analytical observer sees the truth in both: a system with a genuine coordination function that is leveraged for severe, asymmetric extraction, which is the definition of a Tangled Rope. Other perspectives, like the union (mitigating the Tangled Rope) or the automation advocate (seeing a Scaffold), highlight different strategies for engaging with the core conflict.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is driven by the clear division of costs and benefits. Industrial Operators are beneficiaries with arbitrage exit, yielding a low 'd' value and negative effective extraction (χ < 0), hence they perceive a Rope. Shift Workers are victims with trapped exit, yielding a high 'd' value and high positive effective extraction (χ > 0.66), hence they perceive a Snare. The system's structure creates these opposing realities.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a powerful resolution to mandatrophy. A naive analysis might label 24/7 industry as pure extraction (Snare) or pure coordination (Rope). The DR framework avoids this by demonstrating they are both valid, indexed perspectives. The analytical classification of Tangled Rope correctly identifies the dual nature of the constraint: it simultaneously solves a real coordination problem (how to run a hospital overnight) while imposing an extractive cost. It prevents the mislabeling of necessary coordination as pure evil, and the mislabeling of severe biological harm as a mere cost of doing business.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causality_of_health_detriments,
    'Are the negative health outcomes in shift workers caused directly by circadian disruption, or are they confounded by the socioeconomic factors (diet, stress, access to care) of the demographic that typically performs shift work?',
    'Longitudinal studies with control groups matched for socioeconomic status but differing in work schedules.',
    'If directly causal, the extraction (ε) is high. If heavily confounded, the extraction is lower and attributable to other social constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(causality_of_health_detriments, empirical, 'Distinguishing direct biological harm from socioeconomic confounders').

omega_variable(
    automation_viability_horizon,
    'What is the realistic time horizon for automation to replace the majority of human shift work in key industries (logistics, manufacturing, healthcare)?',
    'Techno-economic analysis of robotics, AI, and capital investment cycles.',
    'A short horizon (<15 years) validates the ''Scaffold'' perspective. A long horizon (>50 years) renders the sunset clause theoretical, strengthening the ''Snare'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(automation_viability_horizon, empirical, 'Timeline for automation replacing human shift labor').

omega_variable(
    valuation_of_biological_cost,
    'How should society value a year of healthy life-span lost to occupational hazards versus the economic output gained from 24/7 operations?',
    'Ethical and economic framework development (e.g., Quality-Adjusted Life Year valuation) and public policy debate.',
    'This is a fundamental value judgment that determines whether the extraction is considered acceptable. It shifts the boundary between Rope (acceptable coordination cost) and Snare (unacceptable extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(valuation_of_biological_cost, preference, 'Valuation of human health vs. economic output').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(circadian_decoupling_arbitrage, 1870, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(circ_tr_t0, circadian_decoupling_arbitrage, theater_ratio, 0, 0.0).
narrative_ontology:measurement(circ_tr_t75, circadian_decoupling_arbitrage, theater_ratio, 75, 0.15).
narrative_ontology:measurement(circ_tr_t150, circadian_decoupling_arbitrage, theater_ratio, 150, 0.4).

% Extraction over time
narrative_ontology:measurement(circ_be_t0, circadian_decoupling_arbitrage, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(circ_be_t75, circadian_decoupling_arbitrage, base_extractiveness, 75, 0.55).
narrative_ontology:measurement(circ_be_t150, circadian_decoupling_arbitrage, base_extractiveness, 150, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(circadian_decoupling_arbitrage, global_infrastructure).
narrative_ontology:affects_constraint(circadian_decoupling_arbitrage, global_supply_chain_fragility).
narrative_ontology:affects_constraint(circadian_decoupling_arbitrage, public_health_outcomes_inequality).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
