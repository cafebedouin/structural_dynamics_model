% ============================================================================
% CONSTRAINT STORY: the_calm_protocol_suppression
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_the_calm_protocol_suppression, []).

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
 * * constraint_id: the_calm_protocol_suppression
 * human_readable: The "Calm" of Antarctic Protocol
 * domain: social/psychological
 * * SUMMARY:
 * This constraint models the psychological and institutional "weight" described
 * in the narrative of Soh and Mbatha. "The Calm" is a state where the unyielding
 * physical constraints of the Antarctic are met with a rigid adherence to
 * "Protocol". However, beneath this coordination lies a "weight" of suppressed
 * trauma—specifically an incident involving a bird, an axe, and a choice—that
 * functions as a Snare, extracting the mental health of those who carry the story.
 * * KEY AGENTS:
 * - Soh & Mbatha: The individuals carrying the psychological burden (Powerless)
 * - Expedition Leadership/Protocol: The institutional structure prioritizing mission success (Institutional)
 * - External Auditor: Analytical observer assessing the system's health (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(the_calm_protocol_suppression, 0.75). % High extraction; the "weight" extracts peace and sanity.
domain_priors:suppression_score(the_calm_protocol_suppression, 0.80).   % High suppression; "Nothing happened" is the enforced narrative.
domain_priors:theater_ratio(the_calm_protocol_suppression, 0.10).       % Low theatricality; the suppression is brutally functional.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(the_calm_protocol_suppression, extractiveness, 0.75).
narrative_ontology:constraint_metric(the_calm_protocol_suppression, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(the_calm_protocol_suppression, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(the_calm_protocol_suppression, tangled_rope).
narrative_ontology:human_readable(the_calm_protocol_suppression, "The \"Calm\" of Antarctic Protocol").

% Binary flags
domain_priors:requires_active_enforcement(the_calm_protocol_suppression). % The protocol and unspoken rules require active maintenance.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(the_calm_protocol_suppression, expedition_mission_success).
narrative_ontology:constraint_victim(the_calm_protocol_suppression, soh).
narrative_ontology:constraint_victim(the_calm_protocol_suppression, mbatha).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For Soh and Mbatha, the protocol is a Snare. It extracts their mental
% well-being under the guise of maintaining order.
constraint_indexing:constraint_classification(the_calm_protocol_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The expedition leadership views the protocol as a pure Rope, a necessary
% coordination tool for survival and mission success, blind to the extraction.
constraint_indexing:constraint_classification(the_calm_protocol_suppression, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% An analyst sees both the genuine coordination function (protocol for survival)
% and the asymmetric extraction (psychological weight). It requires active
% enforcement and has clear victims and beneficiaries. This is a classic Tangled Rope.
constraint_indexing:constraint_classification(the_calm_protocol_suppression, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(the_calm_protocol_suppression_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(the_calm_protocol_suppression, TypePowerless,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(the_calm_protocol_suppression, TypeInstitutional,
        context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_classification_is_tangled_rope) :-
    % The analytical view must resolve the conflict as a Tangled Rope.
    constraint_indexing:constraint_classification(the_calm_protocol_suppression, TypeAnalytical,
        context(agent_power(analytical), _, _, _)),
    TypeAnalytical == tangled_rope.

test(high_extraction_and_suppression) :-
    domain_priors:base_extractiveness(the_calm_protocol_suppression, E),
    domain_priors:suppression_score(the_calm_protocol_suppression, S),
    E >= 0.7,
    S >= 0.8.

:- end_tests(the_calm_protocol_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect a system where a necessary coordination protocol has been
 * co-opted to enforce silence and suppress trauma for the sake of mission
 * objectives. The base extraction (0.75) represents the severe psychological
 * cost to individuals, while the high suppression (0.80) reflects the active
 * effort to maintain the "Calm" by forbidding discussion of the underlying trauma.
 *
 * PERSPECTIVAL GAP:
 * The gap is stark. For expedition leadership (Institutional), the protocol is a
 * life-saving Rope. For the individuals carrying the trauma (Powerless), it is a
 * Snare that isolates them and perpetuates their suffering.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The Mandate is "Expedition Progress/Success". The Margin extracted is the
 * "Psychological Safety/Honesty" of the crew. By prioritizing the mission's
 * "Nothing happened" status, the system extracts the engineers' sanity. The
 * Tangled Rope classification correctly identifies this dual nature, preventing
 * the system from misclassifying it as either pure coordination (Rope) or pure
 * malice (Snare). It is a functional system with a parasitic, extractive component.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_the_calm_protocol_suppression,
    "Are the skuas a physical manifestation of environmental pressure (Mountain) or a shared psychological index of the 'Weight' (part of the Snare)?",
    "Verification of field journal photographs and sensor logs by an external biologist (Analytical Observer).",
    "If physical: The environment is an independent Mountain. If shared index: The Snare's power to alter perception is higher than measured.",
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(the_calm_protocol_suppression, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for this high-extraction constraint. The model assumes the
% protocol began as a pure coordination tool and the extractive element
% was introduced and intensified after the traumatic incident.
%
% Theater ratio over time (remains low and functional):
narrative_ontology:measurement(tcp_tr_t0, the_calm_protocol_suppression, theater_ratio, 0, 0.05).
narrative_ontology:measurement(tcp_tr_t5, the_calm_protocol_suppression, theater_ratio, 5, 0.08).
narrative_ontology:measurement(tcp_tr_t10, the_calm_protocol_suppression, theater_ratio, 10, 0.10).

% Extraction over time (grows as the unspoken rule solidifies):
narrative_ontology:measurement(tcp_ex_t0, the_calm_protocol_suppression, base_extractiveness, 0, 0.10).
narrative_ontology:measurement(tcp_ex_t5, the_calm_protocol_suppression, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(tcp_ex_t10, the_calm_protocol_suppression, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The protocol is a set of rules for survival and mission conduct.
narrative_ontology:coordination_type(the_calm_protocol_suppression, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */