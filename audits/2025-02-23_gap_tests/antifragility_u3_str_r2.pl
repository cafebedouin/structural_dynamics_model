% ============================================================================
% CONSTRAINT STORY: antifragility_u3_str_r2
% ============================================================================
% Version: 3.5 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_antifragility_u3_str_r2, []).

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
 *   constraint_id: antifragility_u3_str_r2
 *   human_readable: Antifragility (Gaining from Disorder)
 *   domain: technological/economic/biological
 *
 * SUMMARY:
 *   Antifragility describes systems that increase in capability, resilience,
 *   or robustness as a result of stressors, shocks, volatility, and
 *   randomness. While this may be a natural property of complex adaptive
 *   systems (a Mountain), its application as a strategy in human social and
 *   economic systems creates a stark perspectival gap. Practitioners can
 *   structure their affairs to harvest upside from volatility while
 *   externalizing the downside to others, who become 'optimized serfs' in
 *   fragile systems.
 *
 * KEY AGENTS:
 *   - The Optimized Serf: Primary target (powerless/trapped) - bears the costs of volatility.
 *   - The Barbell Practitioner: Primary beneficiary (moderate/arbitrage) - harvests the gains from volatility.
 *   - The Fragilista/Bureaucrat: Institutional enforcer (institutional/constrained) - creates and maintains the fragile systems that enable extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(antifragility_u3_str_r2, 0.75).
domain_priors:suppression_score(antifragility_u3_str_r2, 0.65).
domain_priors:theater_ratio(antifragility_u3_str_r2, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(antifragility_u3_str_r2, extractiveness, 0.75).
narrative_ontology:constraint_metric(antifragility_u3_str_r2, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(antifragility_u3_str_r2, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(antifragility_u3_str_r2, tangled_rope).
narrative_ontology:human_readable(antifragility_u3_str_r2, "Antifragility (Gaining from Disorder)").
narrative_ontology:topic_domain(antifragility_u3_str_r2, "technological/economic/biological").

domain_priors:requires_active_enforcement(antifragility_u3_str_r2).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(antifragility_u3_str_r2, antifragile_practitioner).
narrative_ontology:constraint_victim(antifragility_u3_str_r2, fragile_institutions).
narrative_ontology:constraint_victim(antifragility_u3_str_r2, optimized_serfs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: The subject of a fragile system who bears the downside of volatility without consent or compensation.
constraint_indexing:constraint_classification(antifragility_u3_str_r2, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: The agent who understands the principle and uses it as a coordination tool to harvest upside from volatility.
constraint_indexing:constraint_classification(antifragility_u3_str_r2, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: The institutional actor who enforces short-term stability, inadvertently creating long-term fragility and enabling extraction.
constraint_indexing:constraint_classification(antifragility_u3_str_r2, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: The analytical observer who sees both the valid evolutionary function and the severe asymmetric extraction in its human application.
constraint_indexing:constraint_classification(antifragility_u3_str_r2, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(antifragility_u3_str_r2_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(antifragility_u3_str_r2, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(antifragility_u3_str_r2, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(antifragility_u3_str_r2, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(antifragility_u3_str_r2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.75) is high, representing the 'convexity bias' where gains from positive black swans are privatized by the practitioner, while losses from negative black swans are socialized or borne by the fragile. Suppression (0.65) reflects the lack of alternatives for those trapped in fragile systems (e.g., jobs with non-portable benefits, economies dependent on a single industry). The theater ratio (0.55) reflects the growth of corporate 'resilience' initiatives that are performative rather than structurally robust.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. The practitioner experiences a Rope, a powerful tool for navigating reality. The serf experiences a Snare, a trap where their stability is sacrificed for someone else's gain. The institutional actor sees a Tangled Rope, acknowledging the need for stability (coordination) but being complicit in a system of extraction. The analyst also sees a Tangled Rope, recognizing the valid underlying principle but classifying its implementation as a hybrid of coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by exposure to convexity. The beneficiary ('antifragile_practitioner') has positive convexity (unlimited upside, limited downside) and arbitrage exit options, making them a clear beneficiary (low d). The victim ('optimized_serfs') has negative convexity (limited upside, catastrophic downside) and is trapped, making them the clear target (high d).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by classifying the constraint as a Tangled Rope from the analytical perspective. A naive analysis might misclassify it as a Mountain ('it's just evolution') or a pure Snare ('it's just exploitation'). The Tangled Rope classification correctly identifies that a valid, natural principle (the coordination function of evolutionary selection) has been weaponized into a mechanism for asymmetric extraction, requiring active enforcement by institutional 'fragilistas'.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    antifragility_extraction_intent,
    'Is the high extraction (convexity bias) a necessary feature for systemic evolution, or is it a predatory mechanism that benefits a few at the expense of the many?',
    'Longitudinal study comparing outcomes in systems with and without explicit 'skin-in-the-game' rules for those who design the systems and benefit from volatility.',
    'If a necessary feature, the civilizational perspective trends towards Mountain. If predatory, the institutional perspective trends towards Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(antifragility_extraction_intent, conceptual, 'Whether the high extraction is a functional necessity for evolution or a predatory feature of its implementation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(antifragility_u3_str_r2, 2007, 2027).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anti_tr_t2007, antifragility_u3_str_r2, theater_ratio, 2007, 0.1).
narrative_ontology:measurement(anti_tr_t2017, antifragility_u3_str_r2, theater_ratio, 2017, 0.35).
narrative_ontology:measurement(anti_tr_t2027, antifragility_u3_str_r2, theater_ratio, 2027, 0.55).

% Extraction over time
narrative_ontology:measurement(anti_be_t2007, antifragility_u3_str_r2, base_extractiveness, 2007, 0.25).
narrative_ontology:measurement(anti_be_t2017, antifragility_u3_str_r2, base_extractiveness, 2017, 0.55).
narrative_ontology:measurement(anti_be_t2027, antifragility_u3_str_r2, base_extractiveness, 2027, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(antifragility_u3_str_r2, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
