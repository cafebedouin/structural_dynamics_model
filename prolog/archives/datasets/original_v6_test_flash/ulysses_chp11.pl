% ============================================================================
% CONSTRAINT STORY: ulysses_chp11
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_chp11, []).

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
 *   constraint_id: ulysses_chp11
 *   human_readable: The Auditory Lure (Ormond Hotel)
 *   domain: social/artistic/biological
 *
 * SUMMARY:
 *   Chapter 11 of Ulysses, often referred to as the 'Sirens' chapter,
 *   portrays the Ormond Hotel bar as a complex auditory environment where
 *   sounds and words create a mesmerizing, yet potentially manipulative,
 *   experience. The music, conversations, and performative storytelling in
 *   the bar form an 'auditory lure' that captivates patrons, influencing
 *   their thoughts and actions. This constraint focuses on the tension
 *   between the artistic coordination of sounds and stories and the potential
 *   for extraction from listeners. Skilled narrators and charismatic
 *   performers benefit from the captivated audience, while passive listeners
 *   may be subject to manipulation or distorted communication.
 *
 * KEY AGENTS:
 *   - Skilled Narrators: Primary beneficiaries (institutional/arbitrage) – capitalize on the captivated audience.
 *   - Charismatic Performers: Secondary beneficiaries (powerful/mobile) – enhance their reputation and influence.
 *   - Passive Listeners: Primary victims (powerless/trapped) – subject to manipulation.
 *   - Truthful Communication: Secondary victim (moderate/constrained) – distortion and embellishment may hinder accurate exchange.
 *   - Casual Bar Patron: Moderate with mobility, both drawn in and potentially manipulated.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp11, 0.6).
domain_priors:suppression_score(ulysses_chp11, 0.4).
domain_priors:theater_ratio(ulysses_chp11, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp11, extractiveness, 0.6).
narrative_ontology:constraint_metric(ulysses_chp11, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(ulysses_chp11, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp11, tangled_rope).
narrative_ontology:human_readable(ulysses_chp11, "The Auditory Lure (Ormond Hotel)").
narrative_ontology:topic_domain(ulysses_chp11, "social/artistic/biological").

domain_priors:requires_active_enforcement(ulysses_chp11).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp11, skilled_narrators).
narrative_ontology:constraint_beneficiary(ulysses_chp11, charismatic_performers).
narrative_ontology:constraint_victim(ulysses_chp11, passive_listeners).
narrative_ontology:constraint_victim(ulysses_chp11, truthful_communication).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of a listener completely captivated by the performance, unable to critically assess or disengage.
constraint_indexing:constraint_classification(ulysses_chp11, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% Perspective of a skilled narrator who benefits from the captivating atmosphere of the bar to enhance their performance and influence.
constraint_indexing:constraint_classification(ulysses_chp11, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(local))).

% Analytical perspective recognizing both the coordination and extraction aspects of the auditory lure in the Ormond Hotel.
constraint_indexing:constraint_classification(ulysses_chp11, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% Perspective of a casual bar patron who is both drawn in by the performance and slightly manipulated by it.
constraint_indexing:constraint_classification(ulysses_chp11, tangled_rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp11_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp11, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp11, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ulysses_chp11, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ulysses_chp11_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): High. The auditory lure extracts attention, critical thinking, and potentially emotional investment from listeners. Suppression (0.4): Moderate. Listeners are subtly encouraged to suspend disbelief and embrace the performance, limiting critical assessment but not completely eliminating free thought. The elevated extraction arises from the performer's skill at creating an immersive environment and from the inherent desire of listeners to be entertained. Theater ratio (0.3): Low. The theatrical aspect is present but not overwhelming. Performers prioritize creating an engaging atmosphere, and listeners are generally willing participants in the exchange.
 *
 * PERSPECTIVAL GAP:
 *   The skilled narrator and charismatic performer both experience the situation as a coordination opportunity, where their skills are amplified by the setting and receptive audience. Casual patrons experience a mixed situation, where they gain entertainment value but also surrender some measure of their critical faculties and objectivity. However, the truly passive listener, lacking critical faculties or social agency, find themselves fully entrapped by the allure of the situation. The analytical observer recognizes all perspectives as a tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are determined by the power and exit options of agents within the auditory environment. Narrators and performers, with institutional power and the capacity to manipulate the setting, see the situation as positive coordination. At the other extreme, individuals with no agency or options for escape find themselves entrapped. The casual patron, with mobility but limited power, exists in a mixed state.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    narrative_authenticity,
    'To what extent does the auditory lure distort or obscure truthful communication?',
    'Analysis of narrative content and speaker motivations in the Ormond Hotel setting.',
    'Higher distortion leads to a stronger snare classification, while more authentic expression shifts it towards a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrative_authenticity, empirical, 'The level of distortion or obfuscation within the auditory lure.').

omega_variable(
    listener_agency,
    'How much agency do listeners retain to critically evaluate and resist the auditory lure?',
    'Observation of listener behavior and responses in the bar setting.',
    'Greater listener agency weakens the snare, potentially shifting the overall classification towards a tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(listener_agency, empirical, 'The degree to which listeners can resist the auditory lure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp11, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ulys_tr_t0, ulysses_chp11, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ulys_tr_t5, ulysses_chp11, theater_ratio, 5, 0.3).
narrative_ontology:measurement(ulys_tr_t10, ulysses_chp11, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(ulys_be_t0, ulysses_chp11, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(ulys_be_t5, ulysses_chp11, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(ulys_be_t10, ulysses_chp11, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp11, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
