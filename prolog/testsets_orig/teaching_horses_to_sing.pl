% ============================================================================
% CONSTRAINT STORY: teaching_horses_to_sing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_teaching_horses_to_sing, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: teaching_horses_to_sing
 *   human_readable: The Sing-or-Die Gambit
 *   domain: political/power_dynamics
 *
 * SUMMARY:
 *   The Sing-or-Die Gambit is a classical wisdom tale in which a condemned
 *   prisoner escapes execution by offering to teach the monarch's horse to
 *   sing within one year. The constraint embeds a structural trap: both the
 *   prisoner and monarch are bound by an impossible bargain that appears to
 *   offer escape but systematically extracts from both parties. The prisoner
 *   bears the cost of the impossible labor and loss of freedom during the
 *   reprieve period. The monarch bears the cost of maintaining public
 *   commitment to an absurd task, which erodes institutional legitimacy and
 *   decision-making autonomy. The constraint's power derives from its
 *   theatrical necessity — both parties must perform belief in the
 *   possibility to maintain the bargain's social function (mercy
 *   demonstration, wit celebration, wisdom narrative). The theater ratio
 *   (0.81) reflects that the entire constraint operates on performative
 *   commitment rather than genuine feasibility. The extractiveness (0.62)
 *   reflects high suppression of alternatives and significant asymmetry, but
 *   lower than a pure execution (which would be 0.95) because the reprieve
 *   itself has psychological value. The constraint resolves through time
 *   running out, escape, or the monarch's eventual mercy — but the underlying
 *   structure persists: an impossible task as the mechanism for distributing
 *   agency and legitimacy.
 *
 * KEY AGENTS:
 *   - Condemned Prisoner: Primary victim (powerless/trapped) — faces execution with zero alternatives; the gambit extracts bodily autonomy and labor in exchange for temporary reprieve with no guaranteed outcome
 *   - Monarch: Secondary victim (powerful/arbitrage) — appears powerful but is structurally trapped by the bargain's social logic; cannot easily back out without losing face; extraction flows from legitimacy loss and constrained agency
 *   - Court/Kingdom Collective: Organized observer (organized/mobile) — benefits from coordination narrative (mercy, wonder, wisdom) but bears cost of theater maintenance and institutional strain
 *   - Historical Tradition: Institutional actor (institutional/constrained) — preserves constraint through narrative performance; theater has outlived functional extraction mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(teaching_horses_to_sing, 0.62).
domain_priors:suppression_score(teaching_horses_to_sing, 0.68).
domain_priors:theater_ratio(teaching_horses_to_sing, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(teaching_horses_to_sing, extractiveness, 0.62).
narrative_ontology:constraint_metric(teaching_horses_to_sing, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(teaching_horses_to_sing, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(teaching_horses_to_sing, snare).
narrative_ontology:human_readable(teaching_horses_to_sing, "The Sing-or-Die Gambit").
narrative_ontology:topic_domain(teaching_horses_to_sing, "political/power_dynamics").

domain_priors:requires_active_enforcement(teaching_horses_to_sing).
% --- Structural relationships ---
narrative_ontology:constraint_victim(teaching_horses_to_sing, condemned_prisoner).
narrative_ontology:constraint_victim(teaching_horses_to_sing, monarch_agency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONDEMNED PRISONER (SNARE) — Faces execution with zero legitimate alternatives. The promise is purely theatrical: the prisoner knows teaching a horse to sing is impossible, but must perform the bargain to delay death. Trapped with suppressed alternatives (cannot refuse, cannot appeal, cannot escape). Maximum extraction: trades bodily autonomy and labor for temporary reprieve that provides no real exit.
constraint_indexing:constraint_classification(teaching_horses_to_sing, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MONARCH (SNARE) — Appears to hold power but is structurally trapped by the gambit's logic. The constraint extracts from the monarch's legitimacy and decision-making autonomy. Once committed to the bargain, the monarch cannot easily rescind it without appearing foolish or weak. The 'impossible task' becomes an extractive mechanism that forces the monarch to either: (a) maintain an absurd public commitment, (b) admit the deception and lose face, or (c) carry out the execution anyway, confirming the monarch's cruelty. All paths are extractive. The monarch's arbitrage (theoretically could execute at will) is suppressed by the constraint's social logic: backing out reveals the trap.
constraint_indexing:constraint_classification(teaching_horses_to_sing, snare,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: COURT/KINGDOM COLLECTIVE (TANGLED ROPE) — The kingdom's institutions have a mixed relationship to the constraint. The bargain performs coordination: it demonstrates mercy, generosity, and wisdom (the monarch appears willing to grant reprieve for promise of wonder). It also solves the execution logistics problem (creates narrative excuse for delay). But it extracts from institutional legitimacy — the court must maintain the fiction that the horse-singing is a genuine possibility, which requires sustained performative belief. The kingdom's collective eye both benefits (coordination narrative) and bears costs (theater maintenance).
constraint_indexing:constraint_classification(teaching_horses_to_sing, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (ROPE) — From an analytical distance, the gambit is a pure coordination mechanism that solves mutual problems elegantly: the prisoner gains reprieve, the monarch gains a narrative path to mercy, the kingdom gains a story of wisdom and wonder. The impossibility of the task is not experienced as extraction from this view — it is the structural mechanism that makes all the coordination possible. Low extractiveness from analytical frame because the 'extraction' (the impossible labor) never had to succeed; it succeeds by being impossible, which both parties understand.
constraint_indexing:constraint_classification(teaching_horses_to_sing, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: HISTORICAL INSTITUTION (PITON) — The constraint persists across time through narrative institutionalization. The story is told as a wisdom tale (the prisoner escapes through wit, the monarch learns generosity, the kingdom celebrates cleverness). But the original extractive mechanism has atrophied — modern auditors no longer believe teaching horses to sing is genuinely attempted. The theater ratio remains high (0.81) because the story continues to be performed as legend despite the underlying structure no longer functioning as originally intended. The constraint maintains itself through institutional inertia and narrative momentum, not through active enforcement.
constraint_indexing:constraint_classification(teaching_horses_to_sing, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(teaching_horses_to_sing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(teaching_horses_to_sing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(teaching_horses_to_sing, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(teaching_horses_to_sing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(teaching_horses_to_sing, TR),
    TR >= 0.70.

:- end_tests(teaching_horses_to_sing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderately high. The prisoner faces an impossible task with no exit, extracting labor and autonomy. The monarch faces erosion of legitimacy and decision-making autonomy through commitment to an absurd public pledge. The value is not maximum (0.95) because both parties arguably consent and the reprieve has real value (delays execution, creates narrative possibility). Suppression (0.68): High. The prisoner has suppressed alternatives (cannot refuse without execution, cannot escape easily, cannot negotiate terms). The monarch's alternatives are suppressed by social logic (cannot back out without appearing weak, cannot admit the trick without confirming cruelty). Theater ratio (0.81): Very high. The entire constraint operates through performative commitment. Both parties must act as though horse-singing is genuinely possible, even if they privately understand the mechanism's actual function. The theater increases over the interval (0.72 → 0.81) as the year progresses and the impossibility becomes more apparent — maintaining belief requires escalating performative investment. Claimed type (Snare): The constraint extracts from both the prisoner and monarch through suppressed alternatives and impossible commitments. The high theater ratio indicates a Piton perspective is also valid (narrative institution), but the primary structure is extractive rather than merely degraded.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prisoner_escape_mechanism,
    'Does the prisoner intend to actually teach the horse to sing, escape during the year, or rely on the monarch''s eventual mercy?',
    'Textual analysis of prisoner''s internal logic; historical versions of the tale reveal different prisoner strategies (flight, actual training attempts, reliance on time running out)',
    'If escape: constraint is Snare (extraction + suppressed alternative). If training attempt: constraint is Rope (coordination toward impossible goal). If mercy-reliance: constraint is Scaffold (temporary reprieve with soft sunset).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prisoner_escape_mechanism, conceptual, 'What is the prisoner''s actual strategy within the bargain').

omega_variable(
    monarch_genuine_belief,
    'Does the monarch genuinely believe horse-singing is possible, or is the monarch complicit in the bargain''s implicit escape logic?',
    'Character analysis from narrative tradition; comparison across versions (some portray naive monarch, others portray wise monarch who secretly enables escape)',
    'If genuine belief: constraint is Snare (extraction through deception). If complicit: constraint is Rope (coordinated escape mechanism with theatrical cover). Shifts directionality and suppression characterization entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(monarch_genuine_belief, conceptual, 'Whether the monarch genuinely believes the task is possible').

omega_variable(
    social_legitimacy_threshold,
    'What is the threshold at which sustained public commitment to an impossible task becomes institutionally self-refuting for the monarch?',
    'Analysis of court dynamics; observation of similar historical commitments and their breaking points; measurement of public confidence in monarch during delay period',
    'If threshold < 1 year: monarch cannot credibly maintain constraint, making it Rope (coordination collapse forces resolution). If threshold > 3 years: monarch''s institutional grip tightens (Snare deepens as suppression increases).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_legitimacy_threshold, empirical, 'Timeline before monarch''s credibility breaks from sustaining impossible task').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(teaching_horses_to_sing, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(thsing_tr_t0, teaching_horses_to_sing, theater_ratio, 0, 0.72).
narrative_ontology:measurement(thsing_tr_t6, teaching_horses_to_sing, theater_ratio, 6, 0.81).
narrative_ontology:measurement(thsing_tr_t12, teaching_horses_to_sing, theater_ratio, 12, 0.81).

% Extraction over time
narrative_ontology:measurement(thsing_be_t0, teaching_horses_to_sing, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(thsing_be_t6, teaching_horses_to_sing, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(thsing_be_t12, teaching_horses_to_sing, base_extractiveness, 12, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(teaching_horses_to_sing, enforcement_mechanism).
narrative_ontology:affects_constraint(teaching_horses_to_sing, monarch_institutional_legitimacy).
narrative_ontology:affects_constraint(teaching_horses_to_sing, reprieve_as_temporary_extraction).

% DUAL FORMULATION NOTE:
% The Sing-or-Die Gambit can be decomposed into two related constraints: (1) the immediate prisoner-monarch exchange (extractiveness 0.62, Snare), and (2) the institutional narrative persistence of the tale as a wisdom story (extractiveness lower, Piton). The network link indicates that the tale's legendary preservation depends on the underlying snare structure being maintained — if the monarch's legitimacy collapse becomes historical fact rather than absorbed legend, the narrative institution degrades.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(teaching_horses_to_sing, powerful, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
