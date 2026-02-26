% ============================================================================
% CONSTRAINT STORY: authoritarian_power_paradox
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_authoritarian_power_paradox, []).

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
 *   constraint_id: authoritarian_power_paradox
 *   human_readable: The Paradox of Authoritarian Centralization
 *   domain: political/social/technological
 *
 * SUMMARY:
 *   The paradox of authoritarian centralization describes how a regime's
 *   attempt to secure its power by centralizing control and suppressing
 *   dissent paradoxically undermines its stability. By eliminating negative
 *   feedback loops and independent 'diagnostic sensors' (free press,
 *   opposition parties, academic freedom), the leadership becomes
 *   increasingly isolated from reality, unable to accurately perceive and
 *   respond to threats. This creates a brittle system that appears strong but
 *   is vulnerable to sudden shocks, as seen in historical and contemporary
 *   authoritarian states.
 *
 * KEY AGENTS:
 *   - Central Ruling Elite: Primary beneficiary (institutional/arbitrage) — Perceives centralization as a necessary tool for stability and control (Rope).
 *   - General Populace / Dissidents: Primary victim (powerless/trapped) — Experiences the full coercive and extractive nature of the state (Snare).
 *   - Regional Administrators: Secondary victim/beneficiary (powerful/constrained) — Must navigate the demands of the central power while dealing with local realities they cannot report upwards (Tangled Rope).
 *   - Rival Geopolitical Powers: External actors (organized/mobile) — View the regime's internal fragility as a temporary state to be exploited (Scaffold).
 *   - Late-Stage Sycophants: Internal actors (institutional/constrained) — Engage in the performative rituals of a system whose functional purpose has atrophied (Piton).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(authoritarian_power_paradox, 0.68).
domain_priors:suppression_score(authoritarian_power_paradox, 0.85).
domain_priors:theater_ratio(authoritarian_power_paradox, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(authoritarian_power_paradox, extractiveness, 0.68).
narrative_ontology:constraint_metric(authoritarian_power_paradox, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(authoritarian_power_paradox, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(authoritarian_power_paradox, tangled_rope).
narrative_ontology:human_readable(authoritarian_power_paradox, "The Paradox of Authoritarian Centralization").
narrative_ontology:topic_domain(authoritarian_power_paradox, "political/social/technological").

domain_priors:requires_active_enforcement(authoritarian_power_paradox).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(authoritarian_power_paradox, central_ruling_elite).
narrative_ontology:constraint_victim(authoritarian_power_paradox, general_populace).
narrative_ontology:constraint_victim(authoritarian_power_paradox, suppressed_opposition).
narrative_ontology:constraint_victim(authoritarian_power_paradox, regional_administrators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE DISSIDENT (SNARE) — Experiences the full coercive force of the state. Information control, surveillance, and suppression of dissent create a high-extraction, high-suppression environment with no exit. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.97.
constraint_indexing:constraint_classification(authoritarian_power_paradox, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE LEADER (ROPE) — Experiences the system as a pure coordination mechanism for ensuring stability and executing a national vision. The costs (suppressed feedback, popular discontent) are externalized and invisible. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.08. Negative effective extraction signifies a net subsidy.
constraint_indexing:constraint_classification(authoritarian_power_paradox, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE REGIONAL ADMINISTRATOR (TANGLED ROPE) — Experiences both the coordination benefits (implementing central directives) and the extractive costs (inability to report bad news, risk of purges, policy misaligned with local reality). d≈0.75, f(d)≈1.10, σ=1.0 → χ≈0.75.
constraint_indexing:constraint_classification(authoritarian_power_paradox, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE SYCOPHANT (PITON) — In a mature, isolated regime, the primary function of governance is replaced by performative loyalty. The system persists through institutional inertia and ritual, not effective administration. The base theater_ratio of 0.75 meets the Piton gate (≥0.70).
constraint_indexing:constraint_classification(authoritarian_power_paradox, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE RIVAL POWER (SCAFFOLD) — Views the authoritarian state's self-inflicted blindness as a temporary, unstable structure. The paradox creates a strategic opportunity that will resolve upon the regime's eventual collapse. The 'sunset clause' is the predicted failure of the brittle state.
constraint_indexing:constraint_classification(authoritarian_power_paradox, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: THE IDEOLOGUE (MOUNTAIN) — Believes that centralization and elite rule are an immutable, natural law of human social organization ('The Iron Law of Oligarchy'). This perspective naturalizes the constraint. The engine will detect this as a false summit, as the base properties (ε=0.68, suppression=0.85) violate the Mountain classification criteria.
constraint_indexing:constraint_classification(authoritarian_power_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: THE ANALYTICAL OBSERVER (TANGLED ROPE) — The default analytical view, which recognizes the system's dual function: it performs genuine coordination (stability, infrastructure) while simultaneously relying on severe extraction (suppression of feedback and dissent) to maintain itself. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.94.
constraint_indexing:constraint_classification(authoritarian_power_paradox, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(authoritarian_power_paradox_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(authoritarian_power_paradox, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(authoritarian_power_paradox, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(authoritarian_power_paradox, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(authoritarian_power_paradox, TR),
    TR >= 0.70.

:- end_tests(authoritarian_power_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high, representing the extraction of resources, loyalty, and, crucially, the right to provide negative feedback. Suppression (0.85) is very high, as the system's existence depends on actively crushing alternative power centers and information sources. Theater Ratio (0.75) is high, reflecting the tendency of such regimes to devolve into performative loyalty tests and propaganda as their connection to reality weakens. The temporal measurements show both extractiveness and theater increasing as the regime matures and becomes more isolated.
 *
 * PERSPECTIVAL GAP:
 *   This constraint is a diagnostic exemplar, producing six different classifications from a single set of base properties. The Leader sees a Rope, a tool for national coordination. The Dissident sees a Snare, a trap of pure coercion. The Bureaucrat sees a Tangled Rope, a mix of function and extraction. The Sycophant sees a Piton, a hollowed-out ritual. The Rival Power sees a Scaffold, a temporary structure doomed to fail. The Ideologue sees a Mountain, a natural law of power. The gap is not a disagreement about facts, but a direct consequence of each agent's structural position relative to the flow of power and information.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality `d` is derived from structural relationships. The Leader (beneficiary, arbitrage exit) has a very low `d`, resulting in negative effective extraction (Rope). The Dissident (victim, trapped exit) has a very high `d`, resulting in maximum effective extraction (Snare). The Regional Administrator (victim, constrained exit) has a high but not maximal `d`, leading to the Tangled Rope classification. Other perspectives are derived from their respective power and exit options, demonstrating how the χ formula maps structural position to classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This case resolves the mandatrophy by demonstrating that 'Rope', 'Snare', and 'Tangled Rope' are not mutually exclusive labels for a system, but perspectival facets of a single, complex structure. The error is to ask 'Which type is it?' The correct question is 'From which structural position are you observing?' The system *is* the presheaf of all its perspectival classifications. The analytical observer's classification of Tangled Rope is the most complete, but the Leader's experience of it as a Rope and the Dissident's experience of it as a Snare are equally valid structural realities.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    feedback_suppression_intent,
    'Is the suppression of diagnostic feedback an unintended bug of centralization, or an intended feature to maintain ideological purity and control, even at the cost of long-term stability?',
    'Analysis of internal party documents, speeches, and decision-making patterns regarding dissent and negative reporting.',
    'If a bug, the system is a tragic Tangled Rope. If a feature, it is a pure Snare where stability is secondary to ideological control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feedback_suppression_intent, conceptual, 'Whether suppression of feedback is a bug or feature of the system.').

omega_variable(
    digital_technology_accelerant,
    'Does digital surveillance and censorship technology fundamentally alter the paradox by making total control possible, or does it accelerate the paradox by making the regime more brittle and blind to non-digital threats?',
    'Comparative analysis of collapse scenarios in pre-digital vs. digital authoritarian states; measuring the speed of information shocks leading to instability.',
    'If it alters the paradox, digital authoritarianism may be a stable new form of governance. If it accelerates it, such regimes are more fragile than they appear.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(digital_technology_accelerant, empirical, 'Impact of digital technology on the stability of centralized regimes.').

omega_variable(
    collapse_threshold,
    'What is the critical threshold of information suppression and leadership isolation beyond which regime collapse becomes statistically inevitable?',
    'Historical data modeling of failed states, correlating metrics like media freedom, elite purges, and economic misallocation with subsequent collapse.',
    'Defines the point at which the system transitions from a stable Tangled Rope to a terminal Piton or a collapsing Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collapse_threshold, empirical, 'The information suppression threshold for inevitable regime collapse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(authoritarian_power_paradox, 1980, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(auth_tr_t1980, authoritarian_power_paradox, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(auth_tr_t2005, authoritarian_power_paradox, theater_ratio, 2005, 0.6).
narrative_ontology:measurement(auth_tr_t2030, authoritarian_power_paradox, theater_ratio, 2030, 0.75).

% Extraction over time
narrative_ontology:measurement(auth_be_t1980, authoritarian_power_paradox, base_extractiveness, 1980, 0.4).
narrative_ontology:measurement(auth_be_t2005, authoritarian_power_paradox, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(auth_be_t2030, authoritarian_power_paradox, base_extractiveness, 2030, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(authoritarian_power_paradox, enforcement_mechanism).
narrative_ontology:affects_constraint(authoritarian_power_paradox, economic_stagnation).
narrative_ontology:affects_constraint(authoritarian_power_paradox, technological_adoption_lag).
narrative_ontology:affects_constraint(authoritarian_power_paradox, information_censorship_models).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
