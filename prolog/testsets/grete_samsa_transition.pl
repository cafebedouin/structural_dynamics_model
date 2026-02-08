% ============================================================================
% CONSTRAINT STORY: grete_samsa_transition
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_grete_samsa_transition, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: grete_samsa_transition
 * human_readable: Grete's Burden and Ascendance in The Metamorphosis
 * domain: social/economic
 * * SUMMARY:
 * This constraint models the evolving role of Grete Samsa following her brother
 * Gregor's transformation. Initially a compassionate act of caregiving, her
 * responsibility becomes an extractive burden that she ultimately resolves by
 * seizing authority within the family and demanding Gregor's removal. The
 * constraint is the set of social and economic obligations placed upon her,
 * which she transforms from a burden into a source of power.
 * * KEY AGENTS:
 * - Early Grete: The initial, compassionate caregiver (Powerless).
 * - Final Grete: The family's ultimate decision-maker (Institutional).
 * - Gregor Samsa: The subject of the constraint (Victim).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Base extractiveness is very high (0.75). Grete's final decision extracts
% Gregor's life to secure her own future and the family's social/economic survival.
domain_priors:base_extractiveness(grete_samsa_transition, 0.75).
% Suppression is moderate (0.5). Grete actively suppresses her mother's
% involvement and later suppresses Gregor's status as a family member,
% referring to him as "it".
domain_priors:suppression_score(grete_samsa_transition, 0.5).
% Theater ratio is very low (0.1). Grete's actions are brutally functional
% and pragmatic, not performative or for show.
domain_priors:theater_ratio(grete_samsa_transition, 0.1).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(grete_samsa_transition, extractiveness, 0.75).
narrative_ontology:constraint_metric(grete_samsa_transition, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(grete_samsa_transition, theater_ratio, 0.1).

% The constraint is presented as a necessary, constructed solution to a problem.
% However, the analytical view reveals it is a Tangled Rope.
narrative_ontology:constraint_claim(grete_samsa_transition, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(grete_samsa_transition). % Required for Tangled Rope

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(grete_samsa_transition, grete_samsa). % Gains status and power.
narrative_ontology:constraint_victim(grete_samsa_transition, gregor_samsa). % Is dehumanized and disposed of.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY GRETE (THE SUBJECT)
% As a powerless caregiver, her new role is a form of coordination that gives
% her purpose and a unique status in the family. It is a Rope.
constraint_indexing:constraint_classification(grete_samsa_transition, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: FINAL GRETE (THE BENEFICIARY)
% Having seized authority, Grete's perspective is institutional within the family
% unit. Her decision to remove Gregor is framed as a necessary coordination act
% (a Rope) to ensure the family's survival and future prosperity.
constraint_indexing:constraint_classification(grete_samsa_transition, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: GREGOR SAMSA (THE VICTIM'S IMPLICIT VIEW)
% From Gregor's perspective (or a powerless observer sympathetic to him),
% Grete's actions are a fatal Snare. What began as care becomes a trap from
% which there is no escape, culminating in his abandonment and death.
% We use a powerless agent with a biographical time horizon to model this.
constraint_indexing:constraint_classification(grete_samsa_transition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER
% The analytical view sees both the coordination function (Grete organizing
% care, then organizing the family's future) and the brutal, asymmetric
% extraction (Gregor's life). This duality, requiring active enforcement,
% defines it as a Tangled Rope.
constraint_indexing:constraint_classification(grete_samsa_transition, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(grete_samsa_transition_tests).

test(perspectival_gap_beneficiary_vs_victim) :-
    % Verify the gap between the beneficiary (Institutional Grete) and the victim's view (Powerless Gregor proxy).
    constraint_indexing:constraint_classification(grete_samsa_transition, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(grete_samsa_transition, TypeVictim, context(agent_power(powerless), time_horizon(biographical), exit_options(trapped), _)),
    TypeBeneficiary == rope,
    TypeVictim == snare,
    TypeBeneficiary \= TypeVictim.

test(analytical_observer_sees_tangled_rope) :-
    constraint_indexing:constraint_classification(grete_samsa_transition, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation_high_extraction) :-
    narrative_ontology:constraint_metric(grete_samsa_transition, extractiveness, E),
    E >= 0.46.

:- end_tests(grete_samsa_transition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness is set to a high 0.75 to reflect the ultimate cost
 * to Gregor: his life. The suppression score of 0.5 reflects Grete's active
 * gatekeeping of Gregor's care and narrative.
 *
 * The key perspectival gap is not just between early and late Grete, but between
 * Grete as the beneficiary and Gregor as the victim. For "Final Grete", who has
 * achieved institutional power within the family, the act of removing Gregor is
 * a 'Rope'—a difficult but necessary act of coordination to save the family unit.
 * For Gregor (represented by a powerless, trapped agent), this same act is a
 * fatal 'Snare'. This stark divergence is characteristic of a Tangled Rope.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The high extraction (0.75) risks a Mandatrophy error, where a system might
 * misclassify this complex social dynamic as pure, villainous extraction (a Snare).
 * The Tangled Rope classification prevents this by acknowledging the genuine (if
 * ruthless) coordination function Grete performs for her family's benefit, while
 * simultaneously accounting for the devastating extraction imposed on Gregor. It
 * correctly identifies that a single set of actions can be both coordination and
 * extraction, depending on the index.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_grete_samsa_transition,
    "Did Grete truly believe Gregor could no longer understand them, or was it a strategic lie to ease the family's conscience and justify his removal?",
    "Requires access to Grete's internal monologue, which Kafka denies the reader.",
    "If Lie: Grete is a ruthless agent cutting a Snare. If Belief: She is a tragic figure trapped by a Rope that became too heavy.",
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(grete_samsa_transition, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint intensifies dramatically over the story's interval.
% Extraction starts low (the cost of food) and ends at an existential level.
% Theater ratio remains low throughout; her actions are pragmatic, not for show.

% Theater ratio over time:
narrative_ontology:measurement(grete_tr_t0, grete_samsa_transition, theater_ratio, 0, 0.05).
narrative_ontology:measurement(grete_tr_t5, grete_samsa_transition, theater_ratio, 5, 0.1).
narrative_ontology:measurement(grete_tr_t10, grete_samsa_transition, theater_ratio, 10, 0.1).

% Extraction over time:
narrative_ontology:measurement(grete_ex_t0, grete_samsa_transition, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(grete_ex_t5, grete_samsa_transition, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(grete_ex_t10, grete_samsa_transition, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The constraint evolves from resource allocation (food, space) into an
% enforcement mechanism (isolating Gregor, then demanding his removal).
narrative_ontology:coordination_type(grete_samsa_transition, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */