% ============================================================================
% CONSTRAINT STORY: civilizational_maintenance_debt
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_civilizational_maintenance_debt, []).

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
 *   constraint_id: civilizational_maintenance_debt
 *   human_readable: The Crumbling Foundation: Civilizational Maintenance Debt
 *   domain: technological/economic
 *
 * SUMMARY:
 *   This constraint models the systemic deferral of maintenance on essential
 *   public infrastructure (transport, water, energy grids). While this
 *   infrastructure provides a crucial coordination function for society,
 *   political and economic incentives favor funding new, visible projects
 *   over the less glamorous work of upkeep. This creates a growing
 *   'maintenance debt'—a form of extraction where value is pulled from the
 *   future to subsidize the present. The system becomes a hybrid: it still
 *   coordinates, but it does so by imposing a massive, un-consented cost on
 *   future generations.
 *
 * KEY AGENTS:
 *   - Future Generations: Primary victim (powerless/trapped) — Inherit the full, compounded cost of decay without having any say in the decisions.
 *   - Short-Term Political Actors: Primary beneficiary (institutional/arbitrage) — Gain political capital by avoiding unpopular taxes and funding popular projects, deferring costs beyond their term in office.
 *   - Current Taxpayers: Secondary beneficiary (moderate/mobile) — Benefit from lower taxes or other public services in the present, at the cost of future liability.
 *   - Users of Failing Infrastructure: Secondary victim (moderate/constrained) — Bear the immediate, localized costs of system failures, such as water main breaks or power outages.
 *   - Maintenance Engineers: Analytical agent (organized/constrained) — Understand the scale of the problem but are constrained by political budget allocation, which they see as a performative, degraded process.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(civilizational_maintenance_debt, 0.58).
domain_priors:suppression_score(civilizational_maintenance_debt, 0.7).
domain_priors:theater_ratio(civilizational_maintenance_debt, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(civilizational_maintenance_debt, extractiveness, 0.58).
narrative_ontology:constraint_metric(civilizational_maintenance_debt, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(civilizational_maintenance_debt, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(civilizational_maintenance_debt, tangled_rope).
narrative_ontology:human_readable(civilizational_maintenance_debt, "The Crumbling Foundation: Civilizational Maintenance Debt").
narrative_ontology:topic_domain(civilizational_maintenance_debt, "technological/economic").

domain_priors:requires_active_enforcement(civilizational_maintenance_debt).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(civilizational_maintenance_debt, short_term_political_actors).
narrative_ontology:constraint_beneficiary(civilizational_maintenance_debt, current_taxpayers).
narrative_ontology:constraint_victim(civilizational_maintenance_debt, future_generations).
narrative_ontology:constraint_victim(civilizational_maintenance_debt, users_of_failing_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE GENERATIONS (SNARE) — Trapped in time, they inherit a degraded system and bear the full, compounded cost of deferred maintenance. They had no voice in the decisions but face the consequences. For them, it is pure, inescapable extraction. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.82.
constraint_indexing:constraint_classification(civilizational_maintenance_debt, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SHORT-TERM POLITICAL ACTORS (ROPE) — They benefit by diverting funds from low-visibility maintenance to popular new projects or tax cuts, arbitraging short-term political gain against long-term decay they won't be in office to manage. They see infrastructure as a coordination tool to be managed for immediate public approval. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.07.
constraint_indexing:constraint_classification(civilizational_maintenance_debt, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes the dual nature: the infrastructure provides a genuine coordination function, but the funding model creates an asymmetric extraction from the future to benefit the present. The high suppression reflects the difficulty of creating alternative infrastructure. This is the canonical classification. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.80.
constraint_indexing:constraint_classification(civilizational_maintenance_debt, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE MAINTENANCE ENGINEER (PITON) — This agent sees the maintenance allocation process itself as a degraded institution. The engineering rationale for preventative maintenance is ignored in favor of political theater (ribbon cuttings, emergency patches). The function of rational asset management has atrophied, replaced by performative gestures. The high theater_ratio (0.75) confirms this view.
constraint_indexing:constraint_classification(civilizational_maintenance_debt, piton,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: USER OF FAILING INFRASTRUCTURE (TANGLED ROPE) — Experiences both the benefits of the system (when it works) and the direct costs of its failure (boil water advisories, bridge closures). They are constrained, unable to easily switch to an alternative, and bear localized extraction. d≈0.85, f(d)≈1.15, σ=0.8 → χ≈0.53.
constraint_indexing:constraint_classification(civilizational_maintenance_debt, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(civilizational_maintenance_debt_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(civilizational_maintenance_debt, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(civilizational_maintenance_debt, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(civilizational_maintenance_debt, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(civilizational_maintenance_debt, TR),
    TR >= 0.70.

:- end_tests(civilizational_maintenance_debt_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.58): High. Represents the significant value of the deferred maintenance costs, which function as a loan taken from the future. Suppression (0.70): High. Individuals and communities have virtually no ability to create alternatives to large-scale public infrastructure, trapping them within the decaying system. Theater Ratio (0.75): High. Political discourse is dominated by performative announcements of new projects ('Infrastructure Week') while the systemic issue of maintenance is ignored. This high ratio allows for the Piton classification from the engineering perspective, as the process of allocating funds has become decoupled from the functional need.
 *
 * PERSPECTIVAL GAP:
 *   The core gap is between the present and the future. Political actors in the present experience the system as a Rope they are skillfully managing for public benefit. Future generations experience it as an inescapable Snare, a debt trap laid by their ancestors. The analytical observer sees the reality: a Tangled Rope, where a vital coordination function has been co-opted for temporal extraction. Meanwhile, the engineer on the ground sees a Piton: a system of rational asset management that has ceased to function, persisting only as a set of rituals.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (politicians, current taxpayers) have immediate time horizons and arbitrage/mobile exit options, leading to a low 'd' value and a perception of coordination (Rope). Victims (future generations) are trapped with a generational time horizon, leading to a high 'd' value and the perception of pure extraction (Snare). Agents in the middle, like current users or engineers, are constrained and see the mixed nature of the system (Tangled Rope or Piton).
 *
 * MANDATROPHY ANALYSIS:
 *   This case avoids mandatrophy by correctly identifying the constraint as a hybrid. A naive analysis might label it a Snare (focusing only on the victims) or a Rope (focusing only on the beneficiaries and the system's function). The Tangled Rope classification, from the analytical perspective, correctly captures the essential conflict: a system that is simultaneously providing a coordination good and executing a massive, asymmetric extraction. The multiple perspectives show that Snare and Rope are also valid, but incomplete, structural readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decay_rate_linearity,
    'Is the rate of infrastructure decay and cost accumulation linear, or does it accelerate non-linearly as systems pass critical failure thresholds?',
    'Longitudinal engineering analysis of asset lifecycle data across multiple infrastructure classes (e.g., water, transport, energy).',
    'If linear, the problem is a large but manageable debt. If non-linear and accelerating, it points towards a potential cascade failure, making the Snare classification more accurate from more perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decay_rate_linearity, empirical, 'Whether infrastructure decay and cost accumulation is linear or non-linear.').

omega_variable(
    political_will_vs_structural_inability,
    'Is maintenance deferral a correctable failure of political will, or a structural inability of short-term electoral cycles to manage long-term, low-visibility liabilities?',
    'Comparative political science analysis of governance systems (e.g., comparing electoral democracies with states having long-term central planning) and their success in managing infrastructure maintenance.',
    'If a failure of will, the constraint is a contingent Tangled Rope. If a structural inability, it approaches a Mountain-like feature of certain governance models, suggesting the need for institutional redesign rather than just different political choices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_will_vs_structural_inability, conceptual, 'Distinguishing between a failure of political will and a structural governance flaw.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(civilizational_maintenance_debt, 1980, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(civi_tr_t1980, civilizational_maintenance_debt, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(civi_tr_t2005, civilizational_maintenance_debt, theater_ratio, 2005, 0.6).
narrative_ontology:measurement(civi_tr_t2030, civilizational_maintenance_debt, theater_ratio, 2030, 0.75).

% Extraction over time
narrative_ontology:measurement(civi_be_t1980, civilizational_maintenance_debt, base_extractiveness, 1980, 0.2).
narrative_ontology:measurement(civi_be_t2005, civilizational_maintenance_debt, base_extractiveness, 2005, 0.45).
narrative_ontology:measurement(civi_be_t2030, civilizational_maintenance_debt, base_extractiveness, 2030, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(civilizational_maintenance_debt, global_infrastructure).
narrative_ontology:affects_constraint(civilizational_maintenance_debt, economic_productivity).
narrative_ontology:affects_constraint(civilizational_maintenance_debt, public_health_outcomes).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
