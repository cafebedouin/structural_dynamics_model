% ============================================================================
% CONSTRAINT STORY: attritional_warfare_doctrine_ru_ua_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_attritional_warfare_doctrine_ru_ua_2026, []).

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
 *   constraint_id: attritional_warfare_doctrine_ru_ua_2026
 *   human_readable: Russian Attritional Warfare Doctrine in Ukraine (2026)
 *   domain: political/military
 *
 * SUMMARY:
 *   This constraint models the Russian military's doctrine of attritional
 *   warfare as practiced in Ukraine, particularly through the use of
 *   'Storm-Z' units composed of convicts. These units are used in human-wave
 *   style assaults, often with little equipment or support, to probe
 *   Ukrainian defenses, force the expenditure of ammunition, and achieve
 *   incremental territorial gains. The doctrine's logic is a grim calculus:
 *   trading the lives of a disposable, coerced population for strategic
 *   advantage against a more limited, professional military force. The system
 *   relies on extreme coercion, including the use of barrier troops to
 *   prevent retreat.
 *
 * KEY AGENTS:
 *   - Storm-Z Convict Soldiers: Primary victims (powerless/trapped) — their lives are the primary resource being extracted.
 *   - Russian High Command: Primary beneficiaries (institutional/arbitrage) — they design and implement the doctrine to achieve strategic goals.
 *   - Ukrainian Military Personnel: Secondary victims (organized/constrained) — forced to expend resources and bear casualties to counter the assaults.
 *   - Russian State Leadership: Primary beneficiaries (institutional/arbitrage) — the ultimate architects and beneficiaries of the war strategy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(attritional_warfare_doctrine_ru_ua_2026, 0.85).
domain_priors:suppression_score(attritional_warfare_doctrine_ru_ua_2026, 0.9).
domain_priors:theater_ratio(attritional_warfare_doctrine_ru_ua_2026, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(attritional_warfare_doctrine_ru_ua_2026, extractiveness, 0.85).
narrative_ontology:constraint_metric(attritional_warfare_doctrine_ru_ua_2026, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(attritional_warfare_doctrine_ru_ua_2026, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(attritional_warfare_doctrine_ru_ua_2026, snare).
narrative_ontology:human_readable(attritional_warfare_doctrine_ru_ua_2026, "Russian Attritional Warfare Doctrine in Ukraine (2026)").
narrative_ontology:topic_domain(attritional_warfare_doctrine_ru_ua_2026, "political/military").

domain_priors:requires_active_enforcement(attritional_warfare_doctrine_ru_ua_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(attritional_warfare_doctrine_ru_ua_2026, russian_high_command).
narrative_ontology:constraint_beneficiary(attritional_warfare_doctrine_ru_ua_2026, russian_state_leadership).
narrative_ontology:constraint_victim(attritional_warfare_doctrine_ru_ua_2026, storm_z_convict_soldiers).
narrative_ontology:constraint_victim(attritional_warfare_doctrine_ru_ua_2026, ukrainian_military_personnel).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE STORM-Z SOLDIER (SNARE) — Coerced into service with no escape, their life is the resource being extracted. Retreat or refusal is met with execution. From this view, the doctrine is a pure death trap. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.97.
constraint_indexing:constraint_classification(attritional_warfare_doctrine_ru_ua_2026, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE RUSSIAN HIGH COMMAND (ROPE) — From the perspective of strategic planners, the doctrine is a coordination mechanism for achieving military objectives. It solves the problem of generating offensive momentum by treating manpower as an expendable resource. d≈0.05, f(d)≈-0.12, σ=0.9 → χ≈-0.09. Negative effective extraction signifies a net beneficiary.
constraint_indexing:constraint_classification(attritional_warfare_doctrine_ru_ua_2026, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE UKRAINIAN FIELD COMMANDER (TANGLED ROPE) — Experiences the doctrine as both a coherent enemy strategy (coordination) and a brutal, extractive assault that forces the expenditure of ammunition and personnel (extraction). They are constrained to respond. d≈0.55, f(d)≈0.75, σ=0.9 → χ≈0.57.
constraint_indexing:constraint_classification(attritional_warfare_doctrine_ru_ua_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (SNARE) — The analyst sees the full structure: a state-level strategy whose functional core is the pure, coercive extraction of life from a trapped population. The coordination function is entirely parasitic on this extraction. The high base extraction and suppression, amplified by global scope, classify it as a Snare. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈1.17. This exceeds the Snare threshold.
constraint_indexing:constraint_classification(attritional_warfare_doctrine_ru_ua_2026, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: THE STATE PROPAGANDIST (SCAFFOLD) — This perspective frames the brutal tactics as a temporary, heroic, and necessary sacrifice to achieve a permanent national victory. The 'sunset clause' is the end of the war, making the extreme measures a scaffold for a future peace. This is a narrative framing, not a structural reality.
constraint_indexing:constraint_classification(attritional_warfare_doctrine_ru_ua_2026, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(attritional_warfare_doctrine_ru_ua_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(attritional_warfare_doctrine_ru_ua_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(attritional_warfare_doctrine_ru_ua_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(attritional_warfare_doctrine_ru_ua_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(attritional_warfare_doctrine_ru_ua_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.85) is extremely high, as the doctrine directly converts human lives into tactical progress. Suppression (0.90) is also extremely high, reflecting the use of convicts who have no alternative and the presence of barrier troops to enforce compliance with lethal force. The Theater Ratio (0.15) is low because the doctrine is brutally functional and largely devoid of performative elements; it is a straightforward, grim application of force.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the convict soldier, it is an inescapable death trap (Snare). For the Russian High Command, it is a rational, if brutal, tool for winning a war (Rope). For the opposing Ukrainian commander, it is a complex threat that is both strategic and extractive (Tangled Rope). The analytical observer, weighing the extreme coercion and extraction against the claimed coordination function, concludes that the system is fundamentally a Snare, where the coordination is entirely parasitic on the extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is driven by the clear division of beneficiaries and victims. The Storm-Z soldiers are victims with trapped exit options, leading to a maximal directionality value (d≈0.95) and a classification as a Snare. The Russian High Command are beneficiaries with arbitrage exit options (they could choose other strategies), leading to a negative directionality value (d≈0.05) and a Rope classification. The Ukrainian forces are victims but are organized and constrained, placing them in the middle, where they perceive the mixed nature of the constraint as a Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates how the DR framework avoids the mandatrophy of mislabeling state violence. A conventional analysis might simply call this 'strategy' (a Rope) or 'war' (a Mountain). By indexing to the powerless, trapped agent (the convict soldier), the framework correctly identifies the core of the doctrine as a high-coercion, high-extraction Snare. It reveals that the 'coordination' function experienced by the beneficiaries is built upon a foundation of pure extraction from the victims, preventing the sanitization of the doctrine's true nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrine_strategic_efficacy,
    'Is the attritional doctrine achieving its strategic goals (e.g., depleting Ukrainian forces faster than Russian forces, gaining key territory) at a sustainable cost?',
    'Post-conflict analysis of casualty ratios, ammunition expenditure rates, and territorial control changes, compared against stated Russian strategic objectives.',
    'If found to be ineffective and unsustainable, the doctrine is not just a Snare but a failed one, bordering on a Piton of state-level strategic incompetence. If effective, it is a grimly functional Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_strategic_efficacy, empirical, 'Whether the doctrine is achieving its strategic goals effectively.').

omega_variable(
    convict_soldier_agency,
    'To what degree do convict soldiers act purely from coercion versus internalizing state narratives of patriotism or redemption?',
    'Systematic interviews with captured or surviving Storm-Z soldiers; analysis of internal communications and battlefield conduct.',
    'If action is almost entirely coerced, the Snare classification is absolute. If a significant degree of ideological buy-in exists, the system has elements of a Tangled Rope even for its victims, as they perceive a (coercively offered) coordination benefit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(convict_soldier_agency, empirical, 'The degree of agency versus pure coercion for convict soldiers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(attritional_warfare_doctrine_ru_ua_2026, 2022, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(attr_tr_t0, attritional_warfare_doctrine_ru_ua_2026, theater_ratio, 0, 0.4).
narrative_ontology:measurement(attr_tr_t2, attritional_warfare_doctrine_ru_ua_2026, theater_ratio, 2, 0.25).
narrative_ontology:measurement(attr_tr_t4, attritional_warfare_doctrine_ru_ua_2026, theater_ratio, 4, 0.15).

% Extraction over time
narrative_ontology:measurement(attr_be_t0, attritional_warfare_doctrine_ru_ua_2026, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(attr_be_t2, attritional_warfare_doctrine_ru_ua_2026, base_extractiveness, 2, 0.78).
narrative_ontology:measurement(attr_be_t4, attritional_warfare_doctrine_ru_ua_2026, base_extractiveness, 4, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(attritional_warfare_doctrine_ru_ua_2026, resource_allocation).
narrative_ontology:affects_constraint(attritional_warfare_doctrine_ru_ua_2026, russian_demographic_stability).
narrative_ontology:affects_constraint(attritional_warfare_doctrine_ru_ua_2026, global_ammunition_supply_chains).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
