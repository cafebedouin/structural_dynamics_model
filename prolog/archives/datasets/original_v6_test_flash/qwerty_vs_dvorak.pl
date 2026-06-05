% ============================================================================
% CONSTRAINT STORY: qwerty_vs_dvorak
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_vs_dvorak, []).

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
 *   constraint_id: qwerty_vs_dvorak
 *   human_readable: QWERTY vs. Dvorak Keyboard Layout: Technological Lock-in
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The QWERTY keyboard layout, despite potential ergonomic and efficiency
 *   drawbacks compared to alternatives like Dvorak, has become entrenched due
 *   to network effects and switching costs. This creates a technological
 *   lock-in where the inferior standard persists. The constraint story
 *   analyses the dynamics from different perspectives.
 *
 * KEY AGENTS:
 *   - Potential Dvorak Adopters: Primary target (powerless/trapped) - bear the cost of lock-in
 *   - Dvorak Keyboard Users: Moderate actors (moderate/constrained) - experience mixed benefits and costs
 *   - QWERTY Keyboard Manufacturers: Primary beneficiaries (institutional/arbitrage) - gain from the standard's dominance
 *   - Typing Tutors (QWERTY): Institutional actors (institutional/constrained) - benefit from training demands.
 *   - Ergonomic Health of the Typing Population: Victim of QWERTY's inefficiency.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_vs_dvorak, 0.6).
domain_priors:suppression_score(qwerty_vs_dvorak, 0.7).
domain_priors:theater_ratio(qwerty_vs_dvorak, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_vs_dvorak, extractiveness, 0.6).
narrative_ontology:constraint_metric(qwerty_vs_dvorak, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(qwerty_vs_dvorak, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_vs_dvorak, tangled_rope).
narrative_ontology:human_readable(qwerty_vs_dvorak, "QWERTY vs. Dvorak Keyboard Layout: Technological Lock-in").
narrative_ontology:topic_domain(qwerty_vs_dvorak, "technological/economic").

domain_priors:requires_active_enforcement(qwerty_vs_dvorak).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_vs_dvorak, qwerty_keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_vs_dvorak, typing_tutors_qwerty).
narrative_ontology:constraint_victim(qwerty_vs_dvorak, dvorak_keyboard_users).
narrative_ontology:constraint_victim(qwerty_vs_dvorak, potential_dvorak_adopters).
narrative_ontology:constraint_victim(qwerty_vs_dvorak, ergonomic_health_typing_population).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Potential Dvorak Adopters (Snare) - Face high switching costs (retraining, new keyboards), network effects (QWERTY familiarity), and limited support. They are essentially trapped in the QWERTY ecosystem. Maximum perceived extraction.
constraint_indexing:constraint_classification(qwerty_vs_dvorak, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Perspective 2: Dvorak Keyboard Users (Tangled Rope) - Benefit from potentially faster typing speeds and ergonomic advantages but are constrained by QWERTY's dominance. They experience both coordination (small community) and extraction (compatibility issues).
constraint_indexing:constraint_classification(qwerty_vs_dvorak, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 3: QWERTY Keyboard Manufacturers (Rope) - Benefit from the established standard, reducing manufacturing and marketing costs. They experience coordination, as their products are widely compatible and accepted. Arbitrage through continued sales
constraint_indexing:constraint_classification(qwerty_vs_dvorak, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 4: Typing Tutors (QWERTY) - Benefit from the prevalence of QWERTY keyboards. Retraining is costly, but the status quo supports their business model. Constrained by the market, but benefiting from it too.
constraint_indexing:constraint_classification(qwerty_vs_dvorak, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 5: Analytical Observer (Tangled Rope) - Observes the lock-in effect and the inefficiencies it creates. Views the situation as a mixed coordination and extraction problem due to network effects and switching costs.
constraint_indexing:constraint_classification(qwerty_vs_dvorak, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_vs_dvorak_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(qwerty_vs_dvorak, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(qwerty_vs_dvorak, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_vs_dvorak, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_vs_dvorak_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): High. Potential Dvorak adopters face high switching costs, reinforcing QWERTY dominance. Suppression (0.70): High. Network effects and lack of widespread Dvorak support strongly suppress Dvorak adoption. Theater ratio (0.30): Low. Relatively little performative activity; the constraint's effects are largely structural.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from differing structural positions. Potential Dvorak adopters are trapped in QWERTY's dominance (Snare). QWERTY keyboard manufacturers and tutors benefit from the coordination it provides (Rope), while Dvorak users experience a mix of extraction (compatibility) and coordination (potential efficiency gains) (Tangled Rope).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is driven by the structural position. Potential adopters are targets and bear the cost, manufacturers are beneficiaries. Dvorak users have both costs and benefits, leading to a moderate directionality. The analytical observer sees the full picture.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ergonomic_benefit_magnitude,
    'What is the true magnitude of ergonomic benefits conferred by Dvorak vs QWERTY, accounting for individual variation?',
    'Large-scale studies controlling for typing style, posture, and pre-existing conditions.',
    'If Dvorak''s benefits are substantial, stronger rationale for intervention. If marginal, the lock-in is less problematic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ergonomic_benefit_magnitude, empirical, 'Magnitude of ergonomic benefits of Dvorak').

omega_variable(
    retraining_cost_barrier,
    'How significant is the retraining cost for QWERTY users to switch to Dvorak?',
    'Studies on learning curves, measuring time and effort required for proficient Dvorak typing.',
    'High retraining cost reinforces the snare. Low retraining cost weakens the snare perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(retraining_cost_barrier, empirical, 'Significance of retraining cost').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_vs_dvorak, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_vs_dvorak, theater_ratio, 0, 0.1).
narrative_ontology:measurement(qwer_tr_t50, qwerty_vs_dvorak, theater_ratio, 50, 0.2).
narrative_ontology:measurement(qwer_tr_t100, qwerty_vs_dvorak, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_vs_dvorak, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(qwer_be_t50, qwerty_vs_dvorak, base_extractiveness, 50, 0.55).
narrative_ontology:measurement(qwer_be_t100, qwerty_vs_dvorak, base_extractiveness, 100, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_vs_dvorak, information_standard).
narrative_ontology:affects_constraint(qwerty_vs_dvorak, betamax_vs_vhs).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
