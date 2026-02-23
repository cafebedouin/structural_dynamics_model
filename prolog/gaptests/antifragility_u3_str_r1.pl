% ============================================================================
% CONSTRAINT STORY: antifragility_u3_str_r1
% ============================================================================
% Version: 4.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-21
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_antifragility_u3_str_r1, []).

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
 *   constraint_id: antifragility_u3_str_r1
 *   human_readable: Antifragility (Gaining from Disorder)
 *   domain: technological/economic/biological
 *
 * SUMMARY:
 *   Antifragility describes the property of systems that increase in
 *   capability, resilience, or robustness as a result of stressors, shocks,
 *   volatility, and randomness. This dynamic creates a stark perspectival
 *   gap. For a biological system over evolutionary time, it is an
 *   unchangeable law (Mountain). For an informed individual practitioner who
 *   can structure their affairs to have 'skin in the game' and benefit from
 *   upside, it is a powerful coordination tool (Rope). For the subject of a
 *   system designed by others to be 'efficient' (fragile), whose potential
 *   gains are capped while their losses are socialized or borne by them, it
 *   is a predatory trap (Snare).
 *
 * KEY AGENTS:
 *   - Optimized Serfs (victims): Employees, debtors, or citizens in roles optimized for stability, who are harmed by systemic shocks.
 *   - Antifragile Practitioner (beneficiary): Individuals or firms using strategies (e.g., barbell) to harvest upside from volatility, often while externalizing downside risk.
 *   - Fragile Institutions (enforcers/victims): Bureaucracies and corporations that create and enforce fragile systems in the name of efficiency and risk-management, simultaneously benefiting from the stability and being the primary victims of its eventual failure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(antifragility_u3_str_r1, 0.75).
domain_priors:suppression_score(antifragility_u3_str_r1, 0.65).
domain_priors:theater_ratio(antifragility_u3_str_r1, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(antifragility_u3_str_r1, extractiveness, 0.75).
narrative_ontology:constraint_metric(antifragility_u3_str_r1, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(antifragility_u3_str_r1, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(antifragility_u3_str_r1, tangled_rope).
narrative_ontology:human_readable(antifragility_u3_str_r1, "Antifragility (Gaining from Disorder)").
narrative_ontology:topic_domain(antifragility_u3_str_r1, "technological/economic/biological").

domain_priors:requires_active_enforcement(antifragility_u3_str_r1).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(antifragility_u3_str_r1, antifragile_practitioner).
narrative_ontology:constraint_victim(antifragility_u3_str_r1, fragile_institutions).
narrative_ontology:constraint_victim(antifragility_u3_str_r1, optimized_serfs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: The subject whose livelihood is optimized for efficiency, making them fragile to shocks. The system extracts their upside while they bear the downside.
constraint_indexing:constraint_classification(antifragility_u3_str_r1, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: The agent who understands the principle and uses it as a coordination strategy (e.g., barbell investing) to benefit from volatility. For them, it is a pure tool.
constraint_indexing:constraint_classification(antifragility_u3_str_r1, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: The institutional actor who enforces the fragile system. They see the coordination benefits (stability, predictability) but are also entangled in the extraction that makes the system vulnerable.
constraint_indexing:constraint_classification(antifragility_u3_str_r1, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: From a long-term, evolutionary perspective, the principle that stressors select for resilience is an unchangeable feature of complex systems.
constraint_indexing:constraint_classification(antifragility_u3_str_r1, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(antifragility_u3_str_r1_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(antifragility_u3_str_r1, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(antifragility_u3_str_r1, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(antifragility_u3_str_r1, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(antifragility_u3_str_r1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.75) is high because the core mechanism involves a transfer of optionality. The antifragile agent gains from volatility because the fragile agent pays the price. This 'convexity bias' is a direct extraction of value. Suppression (0.65) is also high because the systems that create fragility (e.g., employment contracts, debt obligations, centralized regulation) make it extremely difficult for the 'optimized serf' to opt out or adopt an antifragile posture themselves.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. The beneficiary sees a rational strategy for navigating a complex world (Rope). The victim experiences a system that privatizes gains and socializes losses, trapping them (Snare). The institutional actor sees a necessary, if flawed, tool for managing society (Tangled Rope). The analytical observer, taking a civilizational view, sees a fundamental law of nature (Mountain). The conflict arises from applying a natural law as a justification for a social or economic arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is clear: the constraint extracts from those with negative convexity (the fragile, who are harmed more by losses than they benefit from gains) and subsidizes those with positive convexity (the antifragile, who have limited downside and unlimited upside). Beneficiaries are those with the capital, knowledge, and freedom to adopt 'barbell' strategies. Victims are those whose roles are defined by others to maximize predictable output, thereby making them fragile.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by classifying the human-scale system as a Tangled Rope. A simplistic analysis might call it a pure Snare (focusing only on the victim) or a pure Rope (focusing only on the practitioner). The Tangled Rope classification correctly identifies that a genuine coordination principle (how to survive uncertainty) has been co-opted into a mechanism for asymmetric extraction, enforced by institutions that benefit from the illusion of stability it provides.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    antifragility_extraction_intent,
    'Is the high extraction (convexity harvesting) a necessary feature of evolutionary selection, or is it a predatory feature of an engineered social/economic system?',
    'Comparative analysis of systems with and without 'skin in the game' for the beneficiaries. If downside is socialized by design, it's predatory.',
    'If a necessary feature of all complex adaptive systems, it trends toward Mountain. If an engineered and avoidable feature, it is a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(antifragility_extraction_intent, empirical, 'Whether the high extraction is a functional necessity of evolution or a predatory social construct.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(antifragility_u3_str_r1, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anti_tr_t0, antifragility_u3_str_r1, theater_ratio, 0, 0.15).
narrative_ontology:measurement(anti_tr_t5, antifragility_u3_str_r1, theater_ratio, 5, 0.35).
narrative_ontology:measurement(anti_tr_t10, antifragility_u3_str_r1, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(anti_be_t0, antifragility_u3_str_r1, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(anti_be_t5, antifragility_u3_str_r1, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(anti_be_t10, antifragility_u3_str_r1, base_extractiveness, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(antifragility_u3_str_r1, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
