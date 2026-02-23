% ============================================================================
% CONSTRAINT STORY: antifragility_u2_exp_r1
% ============================================================================
% Version: 7.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_antifragility_u2_exp_r1, []).

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
 *   constraint_id: antifragility_u2_exp_r1
 *   human_readable: Antifragility (Gaining from Disorder)
 *   domain: technological/economic/biological
 *
 * SUMMARY:
 *   Antifragility describes systems that increase in capability, resilience,
 *   or robustness as a result of stressors, shocks, volatility, and
 *   randomness. While this appears to be a desirable property, its
 *   implementation often involves a transfer of fragility. Some parts of the
 *   system gain from disorder by making other parts more vulnerable to it.
 *   This creates a significant perspectival gap between those who benefit and
 *   those who pay the price.
 *
 * KEY AGENTS:
 *   - Optimized Serfs (Victim): Powerless agents trapped in fragile positions, bearing the downside of volatility.
 *   - Antifragile Practitioner (Beneficiary): Moderate-power agents with arbitrage options who can structure their exposure to harvest upside from volatility.
 *   - Fragilista Bureaucrat (Enforcer/Victim): Institutional agents who attempt to suppress all volatility, inadvertently creating systemic fragility and concentrating risk.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(antifragility_u2_exp_r1, 0.75).
domain_priors:suppression_score(antifragility_u2_exp_r1, 0.65).
domain_priors:theater_ratio(antifragility_u2_exp_r1, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(antifragility_u2_exp_r1, extractiveness, 0.75).
narrative_ontology:constraint_metric(antifragility_u2_exp_r1, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(antifragility_u2_exp_r1, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(antifragility_u2_exp_r1, tangled_rope).
narrative_ontology:human_readable(antifragility_u2_exp_r1, "Antifragility (Gaining from Disorder)").
narrative_ontology:topic_domain(antifragility_u2_exp_r1, "technological/economic/biological").

domain_priors:requires_active_enforcement(antifragility_u2_exp_r1).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(antifragility_u2_exp_r1, antifragile_practitioner).
narrative_ontology:constraint_victim(antifragility_u2_exp_r1, fragile_institutions).
narrative_ontology:constraint_victim(antifragility_u2_exp_r1, optimized_serfs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For the agent whose stability is sacrificed for system volatility, the constraint is a pure Snare. They bear the costs of disorder without reaping the benefits.
constraint_indexing:constraint_classification(antifragility_u2_exp_r1, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For the agent who can structure their affairs to gain from volatility (the 'barbell strategy'), the constraint is a pure coordination mechanism (Rope) for harvesting upside.
constraint_indexing:constraint_classification(antifragility_u2_exp_r1, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% For the institution attempting to manage the system, it is a Tangled Rope: a necessary coordination function (maintaining order) that is inextricably linked with asymmetric extraction (creating fragility).
constraint_indexing:constraint_classification(antifragility_u2_exp_r1, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% From a civilizational or biological perspective, the principle that systems gain from disorder is an unchangeable feature of reality, a Mountain.
constraint_indexing:constraint_classification(antifragility_u2_exp_r1, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(antifragility_u2_exp_r1_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(antifragility_u2_exp_r1, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(antifragility_u2_exp_r1, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(antifragility_u2_exp_r1, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(antifragility_u2_exp_r1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.75) represents the 'convexity' of the payoff structure: the antifragile agent captures unbounded upside from positive 'black swan' events, while their downside is capped. This capped downside is externalized and becomes the unbounded risk borne by the fragile victims. Suppression (0.65) is high because the system actively eliminates safe, low-volatility options, forcing agents into fragile dependencies.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. The practitioner with arbitrage exit sees a Rope for building wealth. The trapped serf sees a Snare that drains their resources and stability. The institutional actor sees a Tangled Rope, wrestling with the dual need for stability and adaptation. The long-term analytical observer sees a Mountain, an inescapable evolutionary dynamic. The system's true nature is this superposition of all four perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by exposure to volatility and the ability to exit. Beneficiaries ('antifragile_practitioner') are defined by their ability to choose their exposure and exit losing positions (arbitrage), giving them a low 'd' value. Victims ('optimized_serfs', 'fragile_institutions') are defined by their inability to avoid exposure and their trapped status, giving them a high 'd' value. The constraint extracts vitality and resources from the latter to subsidize the former.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by classifying the constraint as a Tangled Rope from the institutional perspective. This avoids mislabeling it as a pure Rope (ignoring the victims) or a pure Snare (ignoring the genuine adaptive function it serves at a system level). The Tangled Rope classification correctly identifies that the coordination function (system adaptation) is achieved via an inseparable mechanism of asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    antifragility_extraction_intent,
    'Is the high extraction (0.75) a functional necessity for evolutionary adaptation, or a predatory feature of a socially constructed system?',
    'A cross-system audit of 'Skin in the Game' metrics to determine if downside risk is systematically transferred to specific groups or is a shared property of the system.',
    'If it is a functional necessity, the analytical classification trends towards Mountain. If it is predatory and constructed, it trends towards Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(antifragility_extraction_intent, empirical, 'Whether the high extraction is a functional necessity or a predatory construct.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(antifragility_u2_exp_r1, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anti_tr_t0, antifragility_u2_exp_r1, theater_ratio, 0, 0.15).
narrative_ontology:measurement(anti_tr_t5, antifragility_u2_exp_r1, theater_ratio, 5, 0.35).
narrative_ontology:measurement(anti_tr_t10, antifragility_u2_exp_r1, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(anti_be_t0, antifragility_u2_exp_r1, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(anti_be_t5, antifragility_u2_exp_r1, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(anti_be_t10, antifragility_u2_exp_r1, base_extractiveness, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(antifragility_u2_exp_r1, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
