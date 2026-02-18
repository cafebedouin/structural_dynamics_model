% ============================================================================
% CONSTRAINT STORY: bip_narrative_illusion
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-16
% ============================================================================

:- module(constraint_bip_narrative_illusion, []).

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
 * * constraint_id: bip_narrative_illusion
 * human_readable: The Black Iron Prison (BIP) and Sensory Optimization
 * domain: philosophical/social/technological
 * * SUMMARY:
 * This constraint defines the modern world as a "Black Iron Prison" (BIP)—a system created by corporate and institutional power that keeps humans subordinate to "Empire". It operates through a "bombardment of sensory stimulus" that forces individuals to cultivate an "analytical understanding" of the world, optimizing behaviors for productivity ("doing and thinking") while suppressing alternative modes of being ("the good heart").
 * * KEY AGENTS:
 * - The Worldly Person: Subject (Powerless), viewed as a "slave to Empire," trapped in sense experience and judgment.
 * - The Corporate Elite/Shareholder: Beneficiary (Institutional), the "covert coven" whose interests are served by the "college of corporations".
 * - The Systems Analyst: Auditor (Analytical), observing the structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(bip_narrative_illusion, 0.85). % Rationale: High. The system extracts the "Being" and "Heart" of the individual to serve the "interests of business leaders and shareholders".
domain_priors:suppression_score(bip_narrative_illusion, 0.75).   % Rationale: High. The system suppresses "warmth" (heart) in favor of "competence" (mind) and "analytical understanding".
domain_priors:theater_ratio(bip_narrative_illusion, 0.10).       % Rationale: Low. The system is brutally functional and does not rely on theatricality.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(bip_narrative_illusion, extractiveness, 0.85).
narrative_ontology:constraint_metric(bip_narrative_illusion, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(bip_narrative_illusion, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be a necessary structure for getting things done.
narrative_ontology:constraint_claim(bip_narrative_illusion, tangled_rope).
narrative_ontology:human_readable(bip_narrative_illusion, "The Black Iron Prison (BIP) and Sensory Optimization").
narrative_ontology:topic_domain(bip_narrative_illusion, "philosophical/social/technological").

% Binary flags
domain_priors:requires_active_enforcement(bip_narrative_illusion). % Required for Tangled Rope. The "college of corporations" actively maintains the system.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(bip_narrative_illusion, corporate_and_shareholder_elites).
narrative_ontology:constraint_victim(bip_narrative_illusion, general_populace).
narrative_ontology:constraint_victim(bip_narrative_illusion, alternative_modes_of_being).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the worldly subject, the BIP is a Snare. The standard of "doing and thinking" tightens around their
% identity, extracting their "Being" and leaving them "chained to lesser versions" of themselves.
constraint_indexing:constraint_classification(bip_narrative_illusion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For the institutional player, the "analytical understanding" of the world is a Rope—a functional
% coordination mechanism used to "get things done" and serve shareholder interests.
constraint_indexing:constraint_classification(bip_narrative_illusion, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees a system with a genuine coordination function for its beneficiaries (Rope-like)
% but which relies on high, asymmetric extraction from its victims and active enforcement (Snare-like).
% This hybrid nature is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(bip_narrative_illusion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bip_narrative_illusion_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(bip_narrative_illusion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bip_narrative_illusion, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_view_is_tangled_rope) :-
    % The analytical view should resolve the gap into a Tangled Rope.
    constraint_indexing:constraint_classification(bip_narrative_illusion, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation_high_extraction) :-
    % Verify the constraint is correctly identified as high-extraction.
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(bip_narrative_illusion, ExtMetricName, E),
    E >= 0.46.

:- end_tests(bip_narrative_illusion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint models a pervasive socio-economic structure described in philosophical terms. The base
 * extractiveness (0.85) and suppression (0.75) are high, reflecting the narrative of a "prison" that extracts
 * human potential ("Being", "Heart") to fuel a corporate system.
 *
 * The Perspectival Gap is stark: for the institutional beneficiary, the system is a 'Rope' for efficient
 * coordination and value creation. For the powerless subject, it is a 'Snare' that traps them in a limited
 * mode of existence.
 *
 * The analytical perspective resolves this conflict by classifying the system as a 'Tangled Rope'. This is
 * the correct classification because the constraint possesses all three required properties: a genuine
 * coordination function (beneficiary exists), asymmetric extraction (victim exists), and requires active
 * enforcement to maintain the asymmetry. The original classification of 'Mountain' for the analytical view
 * was a category error; philosophical reframing of a social construct as "immutable" does not make it a
 * natural law in this classification system.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The Tangled Rope classification correctly prevents Mandatrophy by acknowledging the system's coordination
 * function for elites, rather than mislabeling the entire structure as a pure Snare. It captures the dual-use
 * nature of the constraint, which is key to its stability and persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_bip_intent,
    'Is the sensory bombardment a byproduct of technological growth (emergent) or an intentional control strategy by the elite (constructed)?',
    'Audit of corporate/state resource allocation toward behavior optimization vs. public well-being infrastructure.',
    'If emergent, it is a Tangled Rope leaning towards Mountain properties. If constructed, it is a classic Tangled Rope with high coercive intent.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(bip_narrative_illusion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the intensification of the BIP over the last century (interval 0-10).
% Extraction has steadily increased as corporate power consolidated.
% Theater ratio remains low, as the system's function is overt.

% Theater ratio over time:
narrative_ontology:measurement(bip_tr_t0, bip_narrative_illusion, theater_ratio, 0, 0.10).
narrative_ontology:measurement(bip_tr_t5, bip_narrative_illusion, theater_ratio, 5, 0.10).
narrative_ontology:measurement(bip_tr_t10, bip_narrative_illusion, theater_ratio, 10, 0.10).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(bip_ex_t0, bip_narrative_illusion, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(bip_ex_t5, bip_narrative_illusion, base_extractiveness, 5, 0.70).
narrative_ontology:measurement(bip_ex_t10, bip_narrative_illusion, base_extractiveness, 10, 0.85).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The "college of corporations" acts as a global enforcement mechanism for a specific
% mode of economic and social behavior.
narrative_ontology:coordination_type(bip_narrative_illusion, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */