% ============================================================================
% CONSTRAINT STORY: magna_carta_liberties
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_liberties, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: magna_carta_liberties
 *   human_readable: The Great Charter of Liberties
 *   domain: political/legal
 *
 * SUMMARY:
 *   The Magna Carta, signed in 1215, was a peace treaty between King John and
 *   rebellious barons, aiming to limit royal power and protect certain rights
 *   and privileges. It represents a significant moment in the development of
 *   constitutional law and the struggle for individual liberties.
 *
 * KEY AGENTS:
 *   - King John: Primary target (powerless/trapped) - bore the cost of limited royal authority.
 *   - Rebellious Barons: Primary beneficiary (powerful/constrained) - gained rights and protections but were still constrained by the agreement.
 *   - English Nobility: Secondary beneficiary (moderate/constrained) - the nobility benefited from the overall limitation of the King's power.
 *   - Freemen: Secondary beneficiary (powerless/constrained) - gained some protections from arbitrary royal actions.
 *   - Royal Authority: Victim (institutional/constrained) - The power of the royal was targeted for limited authority.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_liberties, 0.55).
domain_priors:suppression_score(magna_carta_liberties, 0.4).
domain_priors:theater_ratio(magna_carta_liberties, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_liberties, extractiveness, 0.55).
narrative_ontology:constraint_metric(magna_carta_liberties, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(magna_carta_liberties, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_liberties, tangled_rope).
narrative_ontology:human_readable(magna_carta_liberties, "The Great Charter of Liberties").
narrative_ontology:topic_domain(magna_carta_liberties, "political/legal").

domain_priors:requires_active_enforcement(magna_carta_liberties).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_liberties, rebellious_barons).
narrative_ontology:constraint_beneficiary(magna_carta_liberties, english_nobility).
narrative_ontology:constraint_beneficiary(magna_carta_liberties, freemen).
narrative_ontology:constraint_victim(magna_carta_liberties, king_john).
narrative_ontology:constraint_victim(magna_carta_liberties, royal_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% King John, forced to sign the Magna Carta, experienced it as a significant constraint on his power and authority. He had little to no exit options and was essentially trapped by the circumstances.
constraint_indexing:constraint_classification(magna_carta_liberties, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% The rebellious barons, while gaining rights and protections, were still constrained by the terms of the Magna Carta and the need to maintain a balance of power. They benefited from the limitations placed on the king, but also bore the cost of potential royal reprisal. They had moderate power and were constrained rather than fully free to act.
constraint_indexing:constraint_classification(magna_carta_liberties, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Over time, the Magna Carta became a cornerstone of the English legal system, providing a basis for common law and the protection of individual rights. The legal system benefits from the clarity and structure provided by the charter, enabling coordination across legal actors. However it also is constrained in its interpretation by historical context and application of modern legal principles.
constraint_indexing:constraint_classification(magna_carta_liberties, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% From a modern perspective, many clauses of the Magna Carta are obsolete or have been superseded by later legislation. Its symbolic importance as a foundation of liberty and the rule of law overshadows its practical relevance in contemporary legal systems. Thus the scholars study this charter as an artifact. It once served as a strong constraint, now it serves as theater (the past affecting the present), and not directly. Many specific conditions of the Great Charter of Liberties is no longer relevant.
constraint_indexing:constraint_classification(magna_carta_liberties, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_liberties_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(magna_carta_liberties, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(magna_carta_liberties, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_liberties, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(magna_carta_liberties, TR),
    TR >= 0.70.

:- end_tests(magna_carta_liberties_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is relatively high because King John was forced to concede power, limiting his ability to extract resources and exert authority. The suppression is moderate, as the Magna Carta aimed to suppress arbitrary royal actions and protect certain rights, but it did not eliminate royal power altogether. The theater ratio is relatively low, as the Magna Carta had real legal and political consequences, although its symbolic importance has grown over time.
 *
 * PERSPECTIVAL GAP:
 *   King John saw it as a snare, trapping him and limiting his power. The Barons viewed it as a Tangled Rope - an agreement that did them good but was still restricting. Over time, The English Legal system and English society benefited from the constraints over the King in this document.
 *
 * DIRECTIONALITY LOGIC:
 *   King John was the victim and had limited escape - while the Barons had the benefit to gain rights and were a moderately powerful group.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    royal_prerogative_vs_rule_of_law,
    'To what extent does the Magna Carta genuinely limit royal prerogative versus simply codifying existing customs and privileges?',
    'Historical analysis of legal and political practice before and after 1215, focusing on instances where royal actions were challenged or constrained by the charter.',
    'If it significantly limits royal prerogative, then it represents a true shift towards the rule of law. If it largely codifies existing customs, its impact is more symbolic than substantive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(royal_prerogative_vs_rule_of_law, empirical, 'Extent of limitation on royal power').

omega_variable(
    enforcement_mechanism_effectiveness,
    'How effectively were the clauses of the Magna Carta enforced in practice, particularly those protecting the rights of freemen?',
    'Examination of court records, administrative documents, and other primary sources to assess the actual implementation of the charter''s provisions.',
    'If enforcement was weak or inconsistent, the Magna Carta may have been more of a rhetorical tool than a practical safeguard. If enforcement was strong, it represents a genuine protection of individual liberties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_effectiveness, empirical, 'Effectiveness of enforcement of clauses').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_liberties, 0, 800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_liberties, theater_ratio, 0, 0.1).
narrative_ontology:measurement(magn_tr_t100, magna_carta_liberties, theater_ratio, 100, 0.4).
narrative_ontology:measurement(magn_tr_t800, magna_carta_liberties, theater_ratio, 800, 0.75).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_liberties, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(magn_be_t100, magna_carta_liberties, base_extractiveness, 100, 0.57).
narrative_ontology:measurement(magn_be_t800, magna_carta_liberties, base_extractiveness, 800, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(magna_carta_liberties, english_bill_of_rights).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
