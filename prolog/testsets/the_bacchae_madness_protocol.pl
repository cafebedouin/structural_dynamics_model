% ============================================================================
% CONSTRAINT STORY: the_bacchae_madness_protocol
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_the_bacchae_madness_protocol, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: the_bacchae_madness_protocol
 * human_readable: The Dionysian Mandate of Ecstasy
 * domain: religious/political/social
 * * SUMMARY:
 * This constraint models the collision between the secular order of King Pentheus
 * and the divine, chaotic mandate of the god Dionysus. Pentheus attempts to
 * suppress the cult of Dionysus using state power, viewing it as a social ill.
 * However, the Dionysian mandate operates as an irresistible force of nature
 * that extracts the sanity, authority, and ultimately the life of the king when
 * he refuses to acknowledge and integrate this new reality.
 * * KEY AGENTS:
 * - The Maenads (Agave): Subjects (Powerless), caught in a divine frenzy they cannot control.
 * - The Dionysian Cult: Beneficiary (Institutional), the nascent religious order whose power is cemented by the events.
 * - Cadmus & Tiresias: Observers (Analytical), elders who understand the futility of resisting a god.
 * - Pentheus: Victim (Institutional Power made Powerless), the king whose rigid order is destroyed.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Rationale: 0.95. The mandate extracts everything from its primary victim (Pentheus):
% his authority, dignity, sanity, and finally his life. This is a terminal extraction event.
domain_priors:base_extractiveness(the_bacchae_madness_protocol, 0.95).
% Rationale: 0.85. The mandate actively suppresses secular law, rational thought,
% and individual will in both the followers and the resistors.
domain_priors:suppression_score(the_bacchae_madness_protocol, 0.85).
% Rationale: 0.10. The constraint is direct and brutally functional. There is
% very little performative action; the violence and madness are real.
domain_priors:theater_ratio(the_bacchae_madness_protocol, 0.10).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(the_bacchae_madness_protocol, extractiveness, 0.95).
narrative_ontology:constraint_metric(the_bacchae_madness_protocol, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(the_bacchae_madness_protocol, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% Dionysus presents his power not as a choice but as a fundamental, divine law of nature.
narrative_ontology:constraint_claim(the_bacchae_madness_protocol, tangled_rope).
narrative_ontology:human_readable(the_bacchae_madness_protocol, "The Dionysian Mandate of Ecstasy").

% Binary flags
% The mandate requires active enforcement by Dionysus (divine influence) and the Maenads (physical violence).
domain_priors:requires_active_enforcement(the_bacchae_madness_protocol).

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(the_bacchae_madness_protocol, dionysian_cult).
narrative_ontology:constraint_victim(the_bacchae_madness_protocol, pentheus).
narrative_ontology:constraint_victim(the_bacchae_madness_protocol, theban_secular_order).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (THE MAENADS)
% For the women of Thebes, the divine madness is a Snare. It extracts their
% prior social identity, memory, and maternal instinct, trapping them in a
% state of violent ecstasy they did not choose and cannot escape.
constraint_indexing:constraint_classification(the_bacchae_madness_protocol, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (THE DIONYSIAN CULT)
% From the perspective of the nascent religious institution, the mandate is a
% Rope. It coordinates followers, demonstrates the god's power, and violently
% establishes the cult's legitimacy, binding the community into a new order.
constraint_indexing:constraint_classification(the_bacchae_madness_protocol, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (CADMUS & TIRESIAS)
% The elders, Cadmus and Tiresias, recognize Dionysus not as a political actor
% but as a fundamental force. To them, the mandate is a Mountain—an unchangeable
% law of the cosmos. One does not fight it; one coordinates with it to avoid
% being crushed. This is the system's final classification.
constraint_indexing:constraint_classification(the_bacchae_madness_protocol, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(the_bacchae_madness_protocol_tests).

test(perspectival_gap_divinity) :-
    % Verify the gap between the powerless (Snare) and the institutional (Rope).
    constraint_indexing:constraint_classification(the_bacchae_madness_protocol, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(the_bacchae_madness_protocol, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless == snare,
    TypeInstitutional == rope,
    TypePowerless \= TypeInstitutional.

test(analytical_resolution_is_mountain) :-
    % Verify the analytical observer correctly identifies the constraint as a force of nature.
    constraint_indexing:constraint_classification(the_bacchae_madness_protocol, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    TypeAnalytical == mountain.

test(terminal_extraction_threshold) :-
    % The base extraction must be extremely high to reflect the narrative's outcome.
    domain_priors:base_extractiveness(the_bacchae_madness_protocol, E),
    E >= 0.95.

:- end_tests(the_bacchae_madness_protocol_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The core of this story is the classification of a divine mandate. While it
 * functions as a Snare for its victims (Pentheus, Agave) and a Rope for its
 * beneficiaries (the cult), the analytical perspective correctly identifies it
 * as a Mountain. It is a force beyond human negotiation, whose "rules" are
 * as immutable as gravity. Resistance is not a political act; it is a category
 * error that results in annihilation. The extreme extraction score (0.95) is
 * justified by this framing: the mandate is not seeking to tax or control, but
 * to achieve total recognition, and the price for denial is everything.
 *
 * [RESOLVED MANDATROPHY]
 * The Mandatrophy here belongs to Pentheus. His "Mandate of Secular Order"
 * has atrophied his society's capacity to integrate the irrational, the divine,
 * and the chaotic. By creating a rigid system with no margin for ecstasy, he
 * ensures that when that force arrives, it does so explosively rather than
 * metabolically. The high extraction is not a feature of a flawed human system
 * but the logical outcome of a brittle system encountering an irresistible force.
 * The system correctly identifies this as a Mountain from the analytical view,
 * resolving the ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_the_bacchae_madness_protocol,
    'Is the Dionysian mandate an external, objective divine force (Mountain) or a projection of repressed human psychology (a Snare constructed by the psyche)?',
    'Neuro-theological analysis of ecstatic states vs. historical evidence of the Dionysian cults actual political power.',
    'If objective force, it is a true Mountain. If psychological projection, it is a Snare that society builds for itself out of repression.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(the_bacchae_madness_protocol, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The constraint intensifies rapidly as Dionysus's influence grows and Pentheus's resistance hardens.
% Theater ratio remains low throughout; the actions are direct.
narrative_ontology:measurement(tbmp_tr_t0, the_bacchae_madness_protocol, theater_ratio, 0, 0.05).
narrative_ontology:measurement(tbmp_tr_t5, the_bacchae_madness_protocol, theater_ratio, 5, 0.05).
narrative_ontology:measurement(tbmp_tr_t10, the_bacchae_madness_protocol, theater_ratio, 10, 0.10).

% Extraction begins as social disruption and escalates to total destruction.
narrative_ontology:measurement(tbmp_ex_t0, the_bacchae_madness_protocol, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(tbmp_ex_t5, the_bacchae_madness_protocol, base_extractiveness, 5, 0.70).
narrative_ontology:measurement(tbmp_ex_t10, the_bacchae_madness_protocol, base_extractiveness, 10, 0.95).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The mandate functions as an enforcement mechanism for a new divine law.
narrative_ontology:coordination_type(the_bacchae_madness_protocol, enforcement_mechanism).

% The arrival of the Dionysian mandate directly impacts and ultimately shatters the existing civic order.
narrative_ontology:affects_constraint(the_bacchae_madness_protocol, theban_civic_order).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */