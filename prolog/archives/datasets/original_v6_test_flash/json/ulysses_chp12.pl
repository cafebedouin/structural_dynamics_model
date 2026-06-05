% ============================================================================
% CONSTRAINT STORY: ulysses_chp12
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [Draft]
% ============================================================================

:- module(constraint_ulysses_chp12, []).

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
 *   constraint_id: ulysses_chp12
 *   human_readable: The Cyclopean Snare (Barney Kiernan's Pub)
 *   domain: social/political/nationalist
 *
 * SUMMARY:
 *   Leopold Bloom's entry into Barney Kiernan's pub reveals the social
 *   dynamics of Irish nationalism in 1904 Dublin. While the pub serves as a
 *   hub for nationalist coordination, Bloom, as an outsider, experiences its
 *   darker side, facing xenophobia and ultimately violence. This environment
 *   showcases how a seemingly positive force like nationalism can become a
 *   source of extraction and suppression.
 *
 * KEY AGENTS:
 *   - Nationalist Regulars: Primary beneficiaries (powerful/arbitrage) - Coordinate, reinforce shared beliefs.
 *   - Outsiders/Nonconformists: Primary victims (powerless/trapped) - Face xenophobia and potential violence.
 *   - The Citizen: Enforcer of nationalist norms
 *   - Leopold Bloom: Target of nationalist hostility
 *   - Narrator: Observer of the pub's dynamics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp12, 0.65).
domain_priors:suppression_score(ulysses_chp12, 0.7).
domain_priors:theater_ratio(ulysses_chp12, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp12, extractiveness, 0.65).
narrative_ontology:constraint_metric(ulysses_chp12, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ulysses_chp12, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp12, tangled_rope).
narrative_ontology:human_readable(ulysses_chp12, "The Cyclopean Snare (Barney Kiernan's Pub)").
narrative_ontology:topic_domain(ulysses_chp12, "social/political/nationalist").

domain_priors:requires_active_enforcement(ulysses_chp12).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp12, nationalist_regulars).
narrative_ontology:constraint_victim(ulysses_chp12, outsiders_nonconformists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For an outsider like Bloom, the pub becomes a Snare. Trapped in a hostile social environment, he faces constant verbal attacks and the threat of physical violence for his perceived lack of patriotism.
constraint_indexing:constraint_classification(ulysses_chp12, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% For the nationalist regulars, the pub is a Rope. It provides a space for coordinating and reinforcing shared beliefs, creating a sense of belonging and collective identity.
constraint_indexing:constraint_classification(ulysses_chp12, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% From an analytical perspective, the pub embodies a Tangled Rope. It facilitates coordination among nationalists while simultaneously extracting from and suppressing dissenting voices. The coordination and asymmetric extraction is clear.
constraint_indexing:constraint_classification(ulysses_chp12, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp12_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp12, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp12, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ulysses_chp12, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ulysses_chp12_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. The pub's environment extracts from outsiders by subjecting them to social pressure, verbal abuse, and the threat of violence if they do not conform to nationalist ideals. Suppression (0.70): High. Dissenting opinions are actively suppressed through intimidation and social exclusion. Theater ratio (0.40): Moderate. While there is a performative aspect to the nationalism on display, it also serves a genuine function of coordinating and reinforcing shared beliefs.
 *
 * PERSPECTIVAL GAP:
 *   The differing perspectives highlight the dual nature of nationalism. For the nationalists, the pub is a space for positive coordination and belonging. For outsiders, it is a site of exclusion and potential harm. The analytical perspective acknowledges both aspects, recognizing the Tangled Rope dynamic.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (nationalist_regulars) experience the pub as a coordination mechanism, while victims (outsiders_nonconformists) experience it as a source of extraction and suppression. The directionality is determined by their relative power and exit options. The nationalists have power within the pub and the ability to coordinate, while outsiders are trapped and lack the power to resist the social pressure.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intensity_nationalism,
    'How intense does nationalism need to become before its coercive aspects outweigh its coordinating benefits?',
    'Historical analysis of nationalist movements and their consequences.',
    'Determines whether a particular instance of nationalism should be viewed as a positive force (Rope) or a negative one (Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intensity_nationalism, conceptual, 'Threshold for nationalism''s coercive vs coordinating effects').

omega_variable(
    social_enforcement_threshold,
    'At what point does social pressure become undue coercion?',
    'Ethical and legal analysis of freedom of speech and association.',
    'Determines whether the actions of the nationalists constitute legitimate social enforcement or illegitimate suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_enforcement_threshold, preference, 'Point at which social pressure becomes coercion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp12, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ulys_tr_t0, ulysses_chp12, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ulys_tr_t1, ulysses_chp12, theater_ratio, 1, 0.3).
narrative_ontology:measurement(ulys_tr_t2, ulysses_chp12, theater_ratio, 2, 0.4).

% Extraction over time
narrative_ontology:measurement(ulys_be_t0, ulysses_chp12, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(ulys_be_t1, ulysses_chp12, base_extractiveness, 1, 0.6).
narrative_ontology:measurement(ulys_be_t2, ulysses_chp12, base_extractiveness, 2, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
