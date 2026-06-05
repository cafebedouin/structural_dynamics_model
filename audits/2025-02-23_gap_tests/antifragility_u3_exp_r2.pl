% ============================================================================
% CONSTRAINT STORY: antifragility_u3_exp_r2
% ============================================================================
% Version: 4.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_antifragility_u3_exp_r2, []).

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
 *   constraint_id: antifragility_u3_exp_r2
 *   human_readable: Antifragility (Gaining from Disorder)
 *   domain: technological/economic/biological
 *
 * SUMMARY:
 *   Antifragility describes systems that gain from disorder, volatility, and
 *   stressors. This property is fundamental to evolution and complex systems.
 *   However, when applied within human economic and social structures, it can
 *   become a mechanism for transferring risk, where a prepared minority
 *   benefits from the fragility of an optimized majority. The constraint's
 *   classification thus depends entirely on the observer's position relative
 *   to this flow of risk and reward.
 *
 * KEY AGENTS:
 *   - Optimized Serfs (Victims): Individuals or firms in hyper-efficient, brittle roles (powerless/trapped).
 *   - Convexity Practitioners (Beneficiaries): Agents who structure their affairs to have asymmetric upside from volatility (moderate/arbitrage).
 *   - Central Planners (Enforcers/Victims): Institutions that try to suppress volatility, inadvertently creating larger systemic risks (institutional/constrained).
 *   - Evolutionary Processes (Beneficiary): The abstract, system-level process of selection that benefits from the failure of fragile components.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(antifragility_u3_exp_r2, 0.75).
domain_priors:suppression_score(antifragility_u3_exp_r2, 0.65).
domain_priors:theater_ratio(antifragility_u3_exp_r2, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(antifragility_u3_exp_r2, extractiveness, 0.75).
narrative_ontology:constraint_metric(antifragility_u3_exp_r2, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(antifragility_u3_exp_r2, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(antifragility_u3_exp_r2, tangled_rope).
narrative_ontology:human_readable(antifragility_u3_exp_r2, "Antifragility (Gaining from Disorder)").
narrative_ontology:topic_domain(antifragility_u3_exp_r2, "technological/economic/biological").

domain_priors:requires_active_enforcement(antifragility_u3_exp_r2).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(antifragility_u3_exp_r2, convexity_practitioners).
narrative_ontology:constraint_beneficiary(antifragility_u3_exp_r2, system_level_evolutionary_processes).
narrative_ontology:constraint_victim(antifragility_u3_exp_r2, fragile_institutions).
narrative_ontology:constraint_victim(antifragility_u3_exp_r2, optimized_serfs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For the individual whose livelihood depends on a fragile, optimized system, volatility is catastrophic. They bear the costs of others' gains from disorder.
constraint_indexing:constraint_classification(antifragility_u3_exp_r2, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For the agent who understands and can structure their affairs to benefit from volatility (e.g., via a barbell investment strategy), antifragility is a tool for coordination with reality, yielding significant upside.
constraint_indexing:constraint_classification(antifragility_u3_exp_r2, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% Institutions tasked with maintaining stability (e.g., central banks) see antifragility as a dual-edged sword. They must suppress volatility (a coordination function) but in doing so, they build up systemic fragility, which leads to extractive blowups.
constraint_indexing:constraint_classification(antifragility_u3_exp_r2, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% From a sufficiently long and broad perspective, antifragility is a fundamental, unchangeable property of all complex adaptive systems. It is the mechanism of evolution.
constraint_indexing:constraint_classification(antifragility_u3_exp_r2, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(antifragility_u3_exp_r2_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(antifragility_u3_exp_r2, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(antifragility_u3_exp_r2, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(antifragility_u3_exp_r2, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(antifragility_u3_exp_r2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness of 0.75 reflects the 'convexity bias' inherent in the system: the antifragile agent harvests unbounded upside from volatility, while the downside is externalized and borne by the fragile agent. Suppression (0.65) is high because modern economic incentives strongly favor optimization and apparent stability, creating widespread fragility and suppressing the development of robust alternatives.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the practitioner with arbitrage, it's a Rope for navigating reality. For the serf trapped in a fragile job, it's a Snare that destroys them during crises. For the institution, it's a Tangled Rope of managing short-term stability versus long-term systemic risk. For the analyst viewing evolution, it's an unchangeable Mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are agents and systems with 'positive convexity'—the ability to gain from disorder. This includes investors with barbell strategies and evolution itself. Victims are agents with 'negative convexity' or 'fragility'—those optimized for a narrow range of conditions who are destroyed by unexpected events. The system actively transfers value from the latter to the former during shocks.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by the Tangled Rope classification, which captures the dual function seen by institutional actors. A simpler classification would fail: calling it a pure Snare ignores the genuine (if misguided) coordination function of seeking stability, while calling it a Rope ignores the massive, asymmetric extraction that occurs when that stability inevitably breaks.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    antifragility_extraction_intent,
    'Is the high extraction observed in human systems a functional necessity for evolutionary selection, or a predatory feature of a deliberately constructed system?',
    'Comparative analysis of systems with and without 'skin in the game' for the primary beneficiaries. If beneficiaries are systematically shielded from downside risk, the extraction is predatory, not functional.',
    'If functional necessity: re-classify analytical perspective as Mountain. If predatory: re-classify analytical perspective as Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(antifragility_extraction_intent, empirical, 'Distinguishing between necessary evolutionary culling and predatory risk transfer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(antifragility_u3_exp_r2, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anti_tr_t0, antifragility_u3_exp_r2, theater_ratio, 0, 0.1).
narrative_ontology:measurement(anti_tr_t5, antifragility_u3_exp_r2, theater_ratio, 5, 0.35).
narrative_ontology:measurement(anti_tr_t10, antifragility_u3_exp_r2, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(anti_be_t0, antifragility_u3_exp_r2, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(anti_be_t5, antifragility_u3_exp_r2, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(anti_be_t10, antifragility_u3_exp_r2, base_extractiveness, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(antifragility_u3_exp_r2, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
