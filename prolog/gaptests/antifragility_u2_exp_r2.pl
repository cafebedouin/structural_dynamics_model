% ============================================================================
% CONSTRAINT STORY: antifragility_u2_exp_r2
% ============================================================================
% Version: 7.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_antifragility_u2_exp_r2, []).

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
 *   constraint_id: antifragility_u2_exp_r2
 *   human_readable: Antifragility (Gaining from Disorder)
 *   domain: technological/economic/biological
 *
 * SUMMARY:
 *   Antifragility describes systems that increase in capability or resilience
 *   in response to stressors and volatility. While this may be a fundamental
 *   property of evolutionary systems (a Mountain), its application in human
 *   social and economic systems creates a dynamic where specific agents
 *   ('practitioners') benefit from volatility, while costs are externalized
 *   onto others (the 'fragile' or 'optimized serfs').
 *
 * KEY AGENTS:
 *   - Optimized Serfs: Primary targets (powerless/trapped) - bear the costs of systemic shocks.
 *   - Antifragile Practitioners: Primary beneficiaries (moderate/arbitrage) - harvest upside from volatility.
 *   - Fragilista / Bureaucrats: Institutional enforcers (institutional/constrained) - manage the system but are themselves vulnerable to its fragility.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(antifragility_u2_exp_r2, 0.75).
domain_priors:suppression_score(antifragility_u2_exp_r2, 0.65).
domain_priors:theater_ratio(antifragility_u2_exp_r2, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(antifragility_u2_exp_r2, extractiveness, 0.75).
narrative_ontology:constraint_metric(antifragility_u2_exp_r2, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(antifragility_u2_exp_r2, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(antifragility_u2_exp_r2, tangled_rope).
narrative_ontology:human_readable(antifragility_u2_exp_r2, "Antifragility (Gaining from Disorder)").
narrative_ontology:topic_domain(antifragility_u2_exp_r2, "technological/economic/biological").

domain_priors:requires_active_enforcement(antifragility_u2_exp_r2).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(antifragility_u2_exp_r2, antifragile_practitioner).
narrative_ontology:constraint_beneficiary(antifragility_u2_exp_r2, the_system_as_a_whole).
narrative_ontology:constraint_victim(antifragility_u2_exp_r2, fragile_institutions).
narrative_ontology:constraint_victim(antifragility_u2_exp_r2, optimized_serfs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: The victim who bears the cost of systemic shocks and volatility.
constraint_indexing:constraint_classification(antifragility_u2_exp_r2, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: The beneficiary who structures their affairs to gain from disorder.
constraint_indexing:constraint_classification(antifragility_u2_exp_r2, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: The institutional actor who enforces the system but is also vulnerable to it.
constraint_indexing:constraint_classification(antifragility_u2_exp_r2, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: The primary analytical view, seeing the implemented human system as a hybrid of coordination and extraction.
constraint_indexing:constraint_classification(antifragility_u2_exp_r2, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: The deep analytical view, seeing the underlying evolutionary mechanism as a natural law.
constraint_indexing:constraint_classification(antifragility_u2_exp_r2, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(antifragility_u2_exp_r2_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(antifragility_u2_exp_r2, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(antifragility_u2_exp_r2, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(antifragility_u2_exp_r2, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(antifragility_u2_exp_r2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.75) represents the transfer of downside risk from victims to beneficiaries. The system 'learns' by sacrificing its most fragile components. Suppression (0.65) is high because mechanisms that would protect the fragile (e.g., bailouts, robust social safety nets) are actively dismantled to ensure 'skin in the game' and allow for creative destruction.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. For the beneficiary with arbitrage, it's a Rope for generating wealth. For the trapped victim, it's a Snare that extracts their livelihood and security. For the long-term evolutionary observer, it's a Mountain, a law of nature. The system analyst sees the Tangled Rope, recognizing both the coordination function (systemic adaptation) and the brutal, asymmetric extraction required to achieve it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are practitioners who have structured their affairs (e.g., via a 'barbell' strategy) to have convex responses to volatility. Victims are those in linear, fragile roles who are optimized for stability and are destroyed by shocks. The system extracts resilience from the victims and transfers it to the beneficiaries and the system as a whole.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by classifying the implemented system as a Tangled Rope, not a pure Mountain or Snare. This avoids two errors: 1) Mistaking a predatory social arrangement for a 'law of nature' (false Mountain). 2) Ignoring the genuine adaptive/coordination benefits that the system provides at a macro level (false Snare). The Tangled Rope classification correctly identifies the presence of both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    antifragility_extraction_intent,
    'Is the high extraction a functional necessity for systemic evolution or a predatory mechanism benefiting a select few?',
    'Longitudinal study comparing outcomes in systems with and without 'skin in the game' enforcement for beneficiaries.',
    'If necessity: Mountain. If predatory: Snare. The current classification of Tangled Rope reflects this ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(antifragility_extraction_intent, empirical, 'Whether the high extraction is a functional necessity or predatory.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(antifragility_u2_exp_r2, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anti_tr_t0, antifragility_u2_exp_r2, theater_ratio, 0, 0.1).
narrative_ontology:measurement(anti_tr_t5, antifragility_u2_exp_r2, theater_ratio, 5, 0.35).
narrative_ontology:measurement(anti_tr_t10, antifragility_u2_exp_r2, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(anti_be_t0, antifragility_u2_exp_r2, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(anti_be_t5, antifragility_u2_exp_r2, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(anti_be_t10, antifragility_u2_exp_r2, base_extractiveness, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(antifragility_u2_exp_r2, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
