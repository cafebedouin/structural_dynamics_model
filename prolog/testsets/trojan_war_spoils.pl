% ============================================================================
% CONSTRAINT STORY: trojan_war_spoils
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_trojan_war_spoils, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: trojan_war_spoils
 * human_readable: The Allocation of Captives after the Fall of Troy
 * domain: military/social
 * * SUMMARY:
 * After the total defeat of Troy, its surviving women and children are
 * transformed into "spoils of war." The constraint is the Greek system of
 * allocation (a lottery) and the subsequent master-slave relationship that
 * dictates the survivors' destination, labor, and lifespan. It represents a
 * total conversion of human agency into property.
 * * KEY AGENTS:
 * - Hecuba & the Trojan Women: Subjects (Powerless), former royalty now slaves.
 * - The Greek High Command (Agamemnon, Odysseus): Beneficiaries (Institutional), who view the allocation as a logistical necessity.
 * - Talthybius: The Greek Herald, an agent of the institution who carries out its orders.
 * - Astyanax: Child-victim whose existence is suppressed by the "Greeks' law."
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(trojan_war_spoils, 1.0). % Total extraction: life, liberty, and future potential are seized.
domain_priors:suppression_score(trojan_war_spoils, 1.0).   % Total suppression: alternatives (escape, ransom, survival of the royal line) are actively and violently eliminated.
domain_priors:theater_ratio(trojan_war_spoils, 0.0).       % No theatricality; the function is brutally direct.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(trojan_war_spoils, extractiveness, 1.0).
narrative_ontology:constraint_metric(trojan_war_spoils, suppression_requirement, 1.0).
narrative_ontology:constraint_metric(trojan_war_spoils, theater_ratio, 0.0).

% Constraint self-claim: The Greeks claim this is merely the enforcement of military custom.
narrative_ontology:constraint_claim(trojan_war_spoils, snare).
narrative_ontology:human_readable(trojan_war_spoils, "The Allocation of Captives after the Fall of Troy").
narrative_ontology:topic_domain(trojan_war_spoils, "military/social").

% Binary flags
domain_priors:requires_active_enforcement(trojan_war_spoils). % The Greek army is present to enforce the allocation.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(trojan_war_spoils, greek_army).
narrative_ontology:constraint_victim(trojan_war_spoils, trojan_survivors).
narrative_ontology:constraint_victim(trojan_war_spoils, hecuba).
narrative_ontology:constraint_victim(trojan_war_spoils, astyanax).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (HECUBA & THE WOMEN) - MOUNTAIN
% For the women, their enslavement is not a system to be analyzed but an
% immutable, crushing reality akin to a natural disaster. The suppression is
% so total (S=1.0) that it feels like a law of physics.
constraint_indexing:constraint_classification(trojan_war_spoils, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE BENEFICIARY (GREEK HIGH COMMAND) - ROPE
% For the Greek leadership, the spoils system is a pure Rope: a functional
% coordination mechanism to divide loot fairly among the victorious kings,
% preventing internal conflict within their army.
constraint_indexing:constraint_classification(trojan_war_spoils, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER - SNARE
% Analytically, the system is a Snare. It has a coordination function for one
% group (the Greeks) but achieves it through the total, coercive extraction
% of value and life from another group (the Trojans). The high extraction (1.0)
% and suppression (1.0) are definitive.
constraint_indexing:constraint_classification(trojan_war_spoils, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trojan_war_spoils_tests).

test(perspectival_gap_subject_beneficiary) :-
    % Verify the classic gap: the powerless see a Mountain (or Snare), the institutional see a Rope.
    constraint_indexing:constraint_classification(trojan_war_spoils, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trojan_war_spoils, rope, context(agent_power(institutional), _, _, _)).

test(analytical_observer_sees_snare) :-
    % Verify the analytical observer correctly identifies the high-extraction, high-suppression system as a Snare.
    constraint_indexing:constraint_classification(trojan_war_spoils, snare, context(agent_power(analytical), _, _, _)).

test(total_extraction_and_suppression) :-
    % Verify the base metrics reflect the narrative of total subjugation.
    narrative_ontology:constraint_metric(trojan_war_spoils, extractiveness, 1.0),
    narrative_ontology:constraint_metric(trojan_war_spoils, suppression_requirement, 1.0).

:- end_tests(trojan_war_spoils_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores of 1.0 for both extractiveness and suppression reflect the
 * absolute nature of the constraint: slavery and the execution of the heir
 * (Astyanax) represent the complete removal of an agent's present and future
 * value for the benefit of the institution.
 *
 * The key perspectival gap is between the Greek command (Rope) and the Trojan
 * women (Mountain). The Greeks see a logistical tool for maintaining army cohesion.
 * The women experience an inescapable fate. This is a canonical example of how a
 * single system can be both a coordination mechanism and a tool of total
 * extraction, depending on the index. The analytical view resolves this duality
 * by classifying it as a Snare, acknowledging both functions.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * This constraint represents a structural mandate of total war in the Homeric Age.
 * The system does not misclassify this as pure extraction because the 'Rope'
 * perspective from the Greek side is captured, acknowledging its coordination
 * function (distributing spoils). However, the analytical conclusion is 'Snare'
 * because the alternative—diplomatic surrender or ransom—was actively suppressed.
 * Odysseus's argument for killing Astyanax to prevent a future Trojan restoration
 * is explicit evidence of suppressing alternatives, confirming the system's
 * primary function is extractive control, not just coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_trojan_war_spoils,
    "Is the Trojan defeat a literal Mountain of divine will (the will of Poseidon/Athena) or a geopolitical Snare constructed entirely by Greek strategy?",
    "Textual ambiguity in the play's prologue, which frames the events as both a result of divine anger and human action. Resolving this would require external confirmation of the gods' intent.",
    "If Divine Will -> Mountain: The play is a tragedy about humanity's helplessness against fate. If Human Strategy -> Snare: The play is a political critique of the ethics of power and the brutality of victors.",
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(trojan_war_spoils, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint is modeled as being absolute from its inception (the fall of
% the city). There is no gradual accumulation of extraction; it begins at the
% maximum. This is reflected in the flat trajectory of the metrics.
%
% Theater ratio over time:
narrative_ontology:measurement(tws_tr_t0, trojan_war_spoils, theater_ratio, 0, 0.0).
narrative_ontology:measurement(tws_tr_t5, trojan_war_spoils, theater_ratio, 5, 0.0).
narrative_ontology:measurement(tws_tr_t10, trojan_war_spoils, theater_ratio, 10, 0.0).

% Extraction over time:
narrative_ontology:measurement(tws_ex_t0, trojan_war_spoils, base_extractiveness, 0, 1.0).
narrative_ontology:measurement(tws_ex_t5, trojan_war_spoils, base_extractiveness, 5, 1.0).
narrative_ontology:measurement(tws_ex_t10, trojan_war_spoils, base_extractiveness, 10, 1.0).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% From the perspective of the beneficiaries (the Greeks), the constraint's
% coordination function is to allocate resources (the spoils of war).
narrative_ontology:coordination_type(trojan_war_spoils, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */