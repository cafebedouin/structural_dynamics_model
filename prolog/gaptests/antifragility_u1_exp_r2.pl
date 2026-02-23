% ============================================================================
% CONSTRAINT STORY: antifragility_u1_exp_r2
% ============================================================================
% Version: 3.5 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-16
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_antifragility_u1_exp_r2, []).

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
 *   constraint_id: antifragility_u1_exp_r2
 *   human_readable: Antifragility (Gaining from Disorder)
 *   domain: technological/economic/biological
 *
 * SUMMARY:
 *   Antifragility describes systems that increase in capability, resilience,
 *   or robustness as a result of stressors, shocks, volatility, noise,
 *   mistakes, faults, attacks, or failures. While it can be seen as a
 *   fundamental property of all complex adaptive systems (a Mountain), its
 *   application in social and economic domains creates a stark perspectival
 *   gap. Those who can selectively engage with volatility benefit (a Rope),
 *   while those optimized for stability by fragile institutions are harmed by
 *   the externalized consequences (a Snare).
 *
 * KEY AGENTS:
 *   - Optimized Serfs (e.g., salaried employees in fragile industries): Primary victims (powerless/trapped)
 *   - Antifragile Practitioners (e.g., traders, entrepreneurs with barbell strategies): Primary beneficiaries (moderate/arbitrage)
 *   - Fragile Institutions (e.g., centralized bureaucracies, over-leveraged banks): Enforcers and eventual victims (institutional/constrained)
 *   - Analytical Observers (e.g., systems theorists): View the full structure (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(antifragility_u1_exp_r2, 0.75).
domain_priors:suppression_score(antifragility_u1_exp_r2, 0.65).
domain_priors:theater_ratio(antifragility_u1_exp_r2, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(antifragility_u1_exp_r2, extractiveness, 0.75).
narrative_ontology:constraint_metric(antifragility_u1_exp_r2, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(antifragility_u1_exp_r2, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(antifragility_u1_exp_r2, tangled_rope).
narrative_ontology:human_readable(antifragility_u1_exp_r2, "Antifragility (Gaining from Disorder)").
narrative_ontology:topic_domain(antifragility_u1_exp_r2, "technological/economic/biological").

domain_priors:requires_active_enforcement(antifragility_u1_exp_r2).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(antifragility_u1_exp_r2, antifragile_practitioner).
narrative_ontology:constraint_victim(antifragility_u1_exp_r2, fragile_institutions).
narrative_ontology:constraint_victim(antifragility_u1_exp_r2, optimized_serfs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of an individual whose livelihood is optimized for stability, volatility introduced by others is a purely extractive force from which they cannot escape.
constraint_indexing:constraint_classification(antifragility_u1_exp_r2, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For an agent with the resources and knowledge to selectively engage with volatility (the 'barbell strategy'), antifragility is a coordination tool for generating upside.
constraint_indexing:constraint_classification(antifragility_u1_exp_r2, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% An institution attempting to manage a complex system sees both the coordination function (maintaining stability) and the extractive consequences when suppressed volatility inevitably erupts.
constraint_indexing:constraint_classification(antifragility_u1_exp_r2, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% From a sufficiently long and detached perspective, antifragility is an unchangeable law of complex adaptive systems; a feature of reality itself.
constraint_indexing:constraint_classification(antifragility_u1_exp_r2, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(antifragility_u1_exp_r2_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(antifragility_u1_exp_r2, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(antifragility_u1_exp_r2, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(antifragility_u1_exp_r2, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(antifragility_u1_exp_r2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high base extractiveness (ε=0.75) represents the externalization of downside risk. The antifragile agent captures the upside from volatility, while the downside is absorbed by the fragile parts of the system. Suppression (0.65) is high because fragile systems actively eliminate optionality and alternatives in the name of optimization and efficiency, trapping participants.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. For the beneficiary with arbitrage, it's a tool for personal gain (Rope). For the victim trapped in a fragile system, it's a mechanism of ruin (Snare). For the long-term observer, it's an unchangeable law of nature (Mountain). The analytical classification of Tangled Rope captures the fact that it is a real phenomenon with a coordination aspect (adaptation) that has been weaponized to create asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are defined by their ability to harvest upside from volatility due to having 'skin in the game' and options (arbitrage exit). Victims are defined by their lack of options and exposure to negative convexity; they are forced to bear the costs of volatility that benefits others (trapped exit). This structural difference in exit options is the primary driver of directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by recognizing that antifragility is not a pure Snare (it has a real adaptive function) nor a pure Mountain (its application is a choice with victims). The Tangled Rope classification correctly identifies the dual nature: a genuine coordination mechanism (system-level adaptation) co-opted for asymmetric extraction (practitioners benefiting from the ruin of others). This prevents mislabeling a natural law as pure predation, or vice-versa.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    antifragility_extraction_intent,
    'Is the high extraction (externalized risk) a necessary feature of evolutionary selection, or a predatory feature of the system's implementation by beneficiaries?',
    'Comparative analysis of systems where beneficiaries have 'skin in the game' versus those where they do not, to determine if the resilience function can be decoupled from the extractive component.',
    'If the extraction is a necessary component, the constraint is fundamentally a Mountain. If it is a predatory implementation, the constraint is fundamentally a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(antifragility_extraction_intent, empirical, 'Whether the high extraction is a functional necessity for evolution or a predatory choice by practitioners.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(antifragility_u1_exp_r2, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anti_tr_t0, antifragility_u1_exp_r2, theater_ratio, 0, 0.15).
narrative_ontology:measurement(anti_tr_t5, antifragility_u1_exp_r2, theater_ratio, 5, 0.35).
narrative_ontology:measurement(anti_tr_t10, antifragility_u1_exp_r2, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(anti_be_t0, antifragility_u1_exp_r2, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(anti_be_t5, antifragility_u1_exp_r2, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(anti_be_t10, antifragility_u1_exp_r2, base_extractiveness, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(antifragility_u1_exp_r2, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
