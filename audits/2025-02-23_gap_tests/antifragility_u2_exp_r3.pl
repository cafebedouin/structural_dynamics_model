% ============================================================================
% CONSTRAINT STORY: antifragility_u2_exp_r3
% ============================================================================
% Version: 7.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_antifragility_u2_exp_r3, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: antifragility_u2_exp_r3
 *   human_readable: Antifragility (Gaining from Disorder)
 *   domain: technological/economic/biological
 *
 * SUMMARY:
 *   Antifragility describes systems that gain from disorder, stressors, and
 *   volatility. This principle manifests as a constraint with vastly
 *   different implications depending on an agent's position. It enables
 *   adaptive evolution for the whole system over long time horizons, but
 *   often at the direct and severe expense of its most optimized and fragile
 *   components in the short term.
 *
 * KEY AGENTS:
 *   - Optimized Serfs (Victim): Individuals or entities optimized for stability, who bear the costs of volatility (powerless/trapped).
 *   - Antifragile Practitioner (Beneficiary): Agents who use a 'barbell' strategy to harvest upside from volatility while capping downside (moderate/arbitrage).
 *   - Fragilista Bureaucrats (Enforcer): Institutional actors who enforce policies aimed at eliminating volatility, thereby creating hidden fragilities that are later exploited (institutional/constrained).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(antifragility_u2_exp_r3, 0.75).
domain_priors:suppression_score(antifragility_u2_exp_r3, 0.65).
domain_priors:theater_ratio(antifragility_u2_exp_r3, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(antifragility_u2_exp_r3, extractiveness, 0.75).
narrative_ontology:constraint_metric(antifragility_u2_exp_r3, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(antifragility_u2_exp_r3, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(antifragility_u2_exp_r3, tangled_rope).
narrative_ontology:human_readable(antifragility_u2_exp_r3, "Antifragility (Gaining from Disorder)").
narrative_ontology:topic_domain(antifragility_u2_exp_r3, "technological/economic/biological").

domain_priors:requires_active_enforcement(antifragility_u2_exp_r3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(antifragility_u2_exp_r3, antifragile_practitioner).
narrative_ontology:constraint_victim(antifragility_u2_exp_r3, fragile_institutions).
narrative_ontology:constraint_victim(antifragility_u2_exp_r3, optimized_serfs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Experiences the system as pure, inescapable extraction where their stability is sacrificed for others' gains.
constraint_indexing:constraint_classification(antifragility_u2_exp_r3, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Views the principle as a pure coordination tool to navigate and profit from volatility, with minimal perceived extraction.
constraint_indexing:constraint_classification(antifragility_u2_exp_r3, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% Enforces stability-seeking policies that inadvertently create fragility, experiencing the system as a necessary but costly coordination mechanism with unintended consequences.
constraint_indexing:constraint_classification(antifragility_u2_exp_r3, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% Recognizes the dual nature: a genuine adaptive mechanism (coordination) that relies on severe, asymmetric extraction from fragile components.
constraint_indexing:constraint_classification(antifragility_u2_exp_r3, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(antifragility_u2_exp_r3_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(antifragility_u2_exp_r3, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(antifragility_u2_exp_r3, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(antifragility_u2_exp_r3, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(antifragility_u2_exp_r3_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high base extractiveness (ε=0.75) represents the core mechanism of 'convexity harvesting,' where beneficiaries capture unbounded gains from positive shocks while their losses are capped, often by transferring the negative consequences ('concavity') to the victims. Suppression (0.65) is high because the dominant socio-economic paradigm promotes optimization and efficiency, making it difficult to adopt strategies that appear less efficient but are more robust.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. The practitioner with arbitrage exit options sees a Rope—a tool for personal gain. The trapped 'serf' sees a Snare—a system that extracts their resources and resilience without consent or compensation. The institutional actor sees a Tangled Rope—a system they try to manage for stability (coordination) but which generates crises (extraction). The analytical observer, bound by the high ε, also classifies it as a Tangled Rope, recognizing both functions are present.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural positions. The 'antifragile_practitioner' is a beneficiary with arbitrage exit, leading to a low or negative effective extraction (χ), hence seeing a Rope. The 'optimized_serfs' are victims with trapped exit, leading to a very high χ, hence seeing a Snare. The institutional enforcer is also a victim of the system's blowups but benefits from its periods of stability, resulting in an intermediate χ and a Tangled Rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification is crucial for resolving mandatrophy. It prevents mischaracterizing the system as purely predatory (a Snare) or purely beneficial/adaptive (a Rope or Mountain). It correctly identifies that a genuine, system-level coordination function (adaptation to volatility) is structurally coupled with a severe, asymmetric extraction mechanism. This dual nature is the defining feature of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_necessity,
    'Is the high extraction from fragile components a necessary feature for system adaptation, or a predatory artifact of its implementation in human systems?',
    'Comparative analysis of convexity payoffs in natural ecosystems versus human financial markets. Quantify the degree to which downside is socialized.',
    'If a necessary feature of all complex systems, it points towards a Mountain (requiring decomposition into a separate story with a lower epsilon). If a predatory artifact, it confirms the Snare/Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_necessity, conceptual, 'Whether the extraction is a necessary evolutionary feature or a predatory economic one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(antifragility_u2_exp_r3, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anti_tr_t0, antifragility_u2_exp_r3, theater_ratio, 0, 0.1).
narrative_ontology:measurement(anti_tr_t5, antifragility_u2_exp_r3, theater_ratio, 5, 0.35).
narrative_ontology:measurement(anti_tr_t10, antifragility_u2_exp_r3, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(anti_be_t0, antifragility_u2_exp_r3, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(anti_be_t5, antifragility_u2_exp_r3, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(anti_be_t10, antifragility_u2_exp_r3, base_extractiveness, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
