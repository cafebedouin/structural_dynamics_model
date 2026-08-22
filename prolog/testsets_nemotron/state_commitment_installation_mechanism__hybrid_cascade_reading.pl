% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__hybrid_cascade_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__hybrid_cascade_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: state_commitment_installation_mechanism__hybrid_cascade_reading
 *   human_readable: Hybrid Cascade State Commitment Installation
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint models the hybrid cascade reading of the
 *   state_commitment_installation_mechanism kernel. The reading holds that
 *   state commitments are initiated at the apex (by emperors, reforming
 *   bureaucracies, revolutionary vanguards) but require validation from
 *   fringe actors (local notables, religious authorities, guild masters,
 *   regional power-brokers) to stabilize. The two phases — installation and
 *   validation — are distinct: the first is coercive and extractive; the
 *   second is adaptive and legitimation-producing. The constraint is claimed
 *   as a scaffold because it carries a sunset logic: once the commitment is
 *   stabilized (internalized, routinized, or abandoned), the active cascade
 *   machinery is no longer needed. The same kernel admits two sibling
 *   readings: endogenous_climb_reading (legitimacy only climbs from below)
 *   and exogenous_imposition_reading (legitimacy only flows from above). This
 *   reading occupies the structural middle: apex initiates, fringe validates,
 *   mid-level bears costs, local populations pay.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.35).
domain_priors:suppression_score(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.25).
domain_priors:theater_ratio(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__hybrid_cascade_reading, scaffold).
narrative_ontology:human_readable(state_commitment_installation_mechanism__hybrid_cascade_reading, "Hybrid Cascade State Commitment Installation").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__hybrid_cascade_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__hybrid_cascade_reading).
narrative_ontology:has_sunset_clause(state_commitment_installation_mechanism__hybrid_cascade_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__hybrid_cascade_reading, '15708c71-f9d0-434d-bd64-0a2700633b49').
narrative_ontology:cs_kernel_codification('15708c71-f9d0-434d-bd64-0a2700633b49', distributed).
narrative_ontology:cs_authority_grounding('15708c71-f9d0-434d-bd64-0a2700633b49', practice).
narrative_ontology:cs_interpretation_layer_present('15708c71-f9d0-434d-bd64-0a2700633b49').
narrative_ontology:cs_reading_relation('15708c71-f9d0-434d-bd64-0a2700633b49', state_commitment_installation_mechanism__endogenous_climb_reading, influences).
narrative_ontology:cs_reading_relation('15708c71-f9d0-434d-bd64-0a2700633b49', state_commitment_installation_mechanism__exogenous_imposition_reading, influences).
narrative_ontology:cs_axiom('15708c71-f9d0-434d-bd64-0a2700633b49', foundational, two_phase_legitimacy_necessity).
narrative_ontology:cs_axiom_status(two_phase_legitimacy_necessity, holdable).
narrative_ontology:cs_axiom_grounding('15708c71-f9d0-434d-bd64-0a2700633b49', two_phase_legitimacy_necessity, empirically_contingent).
narrative_ontology:cs_axiom('15708c71-f9d0-434d-bd64-0a2700633b49', foundational, fringe_interpretive_agency).
narrative_ontology:cs_axiom_status(fringe_interpretive_agency, holdable).
narrative_ontology:cs_axiom_grounding('15708c71-f9d0-434d-bd64-0a2700633b49', fringe_interpretive_agency, empirically_contingent).
narrative_ontology:cs_reference_frame('15708c71-f9d0-434d-bd64-0a2700633b49', pre_bureaucratic_legitimacy_formation).
narrative_ontology:cs_drift_state('15708c71-f9d0-434d-bd64-0a2700633b49', early_modern_state_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('15708c71-f9d0-434d-bd64-0a2700633b49', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__hybrid_cascade_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__hybrid_cascade_reading, apex_elites).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__hybrid_cascade_reading, fringe_validators).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__hybrid_cascade_reading, mid_level_administrators).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__hybrid_cascade_reading, local_populations).
narrative_ontology:constraint_vindicates(state_commitment_installation_mechanism__hybrid_cascade_reading, hybrid_legitimacy_formation).
narrative_ontology:constraint_vindicates(state_commitment_installation_mechanism__hybrid_cascade_reading, two_phase_state_formation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Installs new commitments at the apex of the state apparatus through decree, law, or institutional reform. They possess the authority to initiate the cascade but depend on downstream adoption for the commitment to become stable. Their extraction comes from the concentrated power of initiation and the rents captured during the installation phase.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, apex_elites, agenda_setter,
    institutional, generational, arbitrage, national).

% Local elites, religious authorities, guild leaders, or cultural intermediaries at the institutional fringe who validate, interpret, and adapt the apex commitment. They gain status, resources, and autonomy by performing this validation function. Their exit is constrained because their authority derives from the very system they validate, but they can withhold validation to extract concessions.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, fringe_validators, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__hybrid_cascade_reading, fringe_validators, agenda_setter).

% Provincial governors, bureaucrats, military officers, and judicial officials who must implement the cascade downward. They bear the cost of enforcement, face resistance from below, and risk punishment from above for failure. Their exit is constrained by career dependence on the state apparatus, but they can drag feet, reinterpret, or quietly resist.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, mid_level_administrators, payer,
    organized, biographical, constrained, national).

% Peasants, urban commoners, and subject communities who experience the commitment as new taxes, labor obligations, cultural impositions, or legal restrictions. They have no voice in the installation or validation phases and bear the material costs. Their exit is trapped — migration is costly, dangerous, and often legally restricted. They are excluded from the legitimacy conversation despite being the ultimate bearers of the constraint.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, local_populations, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__hybrid_cascade_reading, local_populations, excluded).

% Analysts who study the two-phase pattern across cases. They see the full structural arc — apex initiation, mid-level friction, fringe validation, and stabilization or collapse. They collect no rents and pay no costs from the constraint itself.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of how a new state commitment (religion, law, administrative reform, ideology) achieves society-wide legitimacy without either pure imposition (which triggers revolt) or pure organic emergence (which is too slow for state-building timelines). The two-phase cascade coordinates apex authority with fringe adaptation.
% TRANSFER_FUNCTION: Moves legitimacy and compliance from fringe validators (who supply adaptive interpretation and local enforcement) to apex elites (who supply the initiating mandate and central resources), while extracting labor, resources, and cultural conformity from mid-level administrators and local populations.
% ABSENT_VOICES: Local populations are structurally excluded from the legitimacy negotiation — they experience the commitment as fait accompli. Would-be alternative validators (rival religious orders, competing legal traditions, autonomous communal structures) are either co-opted or suppressed during the validation phase. Their absence is what makes the cascade 'hybrid' rather than 'negotiated'.
% DISAPPEARANCE_RATIONALE: If the hybrid cascade mechanism vanished, new state commitments would either fail to stabilize (apex imposes without fringe buy-in → revolt or passive resistance) or take generations to legitimize (pure endogenous climb). The state's capacity to install transformative commitments at historical speed would collapse. The mechanism is not a natural law — it is a constructed coordination device that state-builders rediscover and deploy.
% FOUNDING_PROBLEM: Early state formation and imperial consolidation faced a legitimacy-speed tradeoff: top-down imposition was fast but brittle; bottom-up emergence was durable but too slow for competitive interstate environments. The hybrid cascade was the institutional innovation that resolved this tradeoff.
% FOUNDING_PROBLEM_CORROBORATION: State-centered historians (Tilly, Mann) attest the speed imperative is live — states still need rapid legitimacy formation. Anthropologists of the state (Scott, Clastres) and subaltern historians attest the 'founding problem' is a retrospective rationalization of extraction — local populations never consented to the tradeoff, and the speed imperative serves elite interests. No neutral arbiter exists; the corroboration split mirrors the beneficiary/victim structure.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__hybrid_cascade_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__hybrid_cascade_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__hybrid_cascade_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(state_commitment_installation_mechanism__hybrid_cascade_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__hybrid_cascade_reading_tests).
:- end_tests(state_commitment_installation_mechanism__hybrid_cascade_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35) is moderate: the constraint extracts compliance and resources during the cascade but declines as stabilization proceeds (measurement series shows 0.45→0.35). Suppression (0.25) is present but not overwhelming — the mechanism relies more on fringe co-optation than brute force. Theater ratio (0.15) is low because the validation phase performs genuine adaptive work, not mere performance. Accessibility collapse (0.4) is moderate: alternatives (pure imposition, pure emergence) exist but are structurally disadvantaged for state-building timelines. Resistance (0.3) is real but channeled — mid-level administrators resist implementation details; local populations resist materially but lack coordinated voice.
 *
 * PERSPECTIVAL GAP:
 *   The apex seat experiences this as a coordination scaffold it built and controls. The fringe validators experience it as a negotiated bargain they can leverage. The mid-level administrators experience it as an enforced extraction they must implement. The local populations experience it as an imposed burden they cannot escape. The engine will compute four different effective classifications from the same structural data — this divergence is the measurement, not an error.
 *
 * DIRECTIONALITY LOGIC:
 *   Apex elites are structural beneficiaries (d ≈ 0.15): they initiate and capture rents during installation. Fringe validators are dual-positioned (d ≈ 0.35): they benefit from validation rents but must perform adaptive labor. Mid-level administrators are payers (d ≈ 0.7): they enforce downward, absorb resistance, face upward accountability. Local populations are full targets (d ≈ 0.95): they pay material and cultural costs with no exit and no voice. The engine computes these from the declared roles and exit options; the measurement series captures the temporal shift from installation (higher extraction) to stabilization (lower extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold's founding problem (legitimacy-speed tradeoff) is contested: state-builders still claim it; subaltern voices say the tradeoff never served them. The sunset clause is structural — the cascade machinery is meant to become obsolete once the commitment stabilizes. But in practice, states often re-trigger cascades for new commitments, making the scaffold recurring rather than one-shot. The mandatrophy question: does the mechanism persist because the founding problem is live, or because the cascade machinery itself generates rents for apex and fringe actors? The measurement series shows extractiveness plateauing at 0.35 rather than declining to near-zero, suggesting residual extraction persists after stabilization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    apex_initiation_necessity,
    'Is apex initiation structurally necessary for the commitments this reading covers, or could they have emerged endogenously given more time?',
    'Counterfactual comparison across cases: identify commitments of the same type (religious, administrative, ideological) that emerged without apex initiation in comparable societies. Measure time-to-stabilization and durability.',
    'If apex initiation is not necessary, the hybrid cascade is a contingent accelerant, not a structural requirement — the scaffold''s justification weakens. If necessary, the scaffold is a genuine solution to a binding constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(apex_initiation_necessity, empirical, 'Whether the apex phase is a structural necessity or a contingent accelerant.').

omega_variable(
    fringe_validation_autonomy,
    'Do fringe validators have genuine interpretive autonomy, or is their ''validation'' a coerced performance under apex threat?',
    'Analyze cases where fringe actors modified, delayed, or refused validation. Did the apex accommodate, coerce, or bypass them? Measure the distribution of outcomes.',
    'If validation is coerced performance, the hybrid cascade collapses into exogenous imposition with a theatrical validation layer (piton risk). If genuine autonomy exists, the coordination function is real and the extraction is the price of negotiation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fringe_validation_autonomy, conceptual, 'Whether the validation phase is genuine coordination or coerced theater.').

omega_variable(
    kernel_reading_relations,
    'What is the structural relationship between this hybrid_cascade_reading and its sibling readings (endogenous_climb_reading, exogenous_imposition_reading)?',
    'Assess whether a single framework could hold this reading and a sibling simultaneously, or whether this reading''s core premise (two-phase apex-to-fringe) logically forecloses the sibling''s core premise (pure endogenous or pure exogenous).',
    'If forecloses: the readings are mutually exclusive within any single analytical framework. If coexists_with: different parties can hold different readings simultaneously. If influences: this reading creates structural pressure on sibling readings without foreclosing them.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_relations, conceptual, 'Structural relationship to sibling readings of the same kernel.').

omega_variable(
    stabilization_metric_ambiguity,
    'What counts as ''stabilization'' triggering the scaffold''s sunset — internalization, routinization, or mere absence of overt resistance?',
    'Define operational criteria for stabilization across case studies. Track whether extractiveness and suppression actually decline to near-zero post-stabilization, or whether residual extraction persists.',
    'If stabilization is declared prematurely (mere absence of overt resistance), the scaffold''s sunset is fictive and the constraint becomes a piton. If genuine internalization occurs, extractiveness should approach zero.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stabilization_metric_ambiguity, empirical, 'Operational definition of the scaffold''s sunset condition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__hybrid_cascade_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(stat_tr_t10, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(stat_tr_t20, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(stat_tr_t30, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(stat_tr_t40, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(stat_tr_t50, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(stat_be_t10, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(stat_be_t20, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(stat_be_t30, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 30, 0.32).
narrative_ontology:measurement(stat_be_t40, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 40, 0.35).
narrative_ontology:measurement(stat_be_t50, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 50, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(stat_su_t10, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(stat_su_t20, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(stat_su_t30, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 30, 0.25).
narrative_ontology:measurement(stat_su_t40, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 40, 0.25).
narrative_ontology:measurement(stat_su_t50, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 50, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__hybrid_cascade_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.1).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__hybrid_cascade_reading, state_commitment_installation_mechanism__endogenous_climb_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__hybrid_cascade_reading, state_commitment_installation_mechanism__exogenous_imposition_reading).

% DUAL FORMULATION NOTE:
% This constraint and its two siblings form the state_commitment_installation_mechanism kernel family. The hybrid_cascade_reading asserts a two-phase structure (apex installation → fringe validation) that structurally mediates between the pure endogenous and pure exogenous readings. The endogenous reading forecloses the apex initiation phase; the exogenous reading forecloses the fringe validation phase. This reading claims both phases are necessary and neither alone suffices.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_commitment_installation_mechanism__hybrid_cascade_reading, organized, 0.35).
constraint_indexing:directionality_override(state_commitment_installation_mechanism__hybrid_cascade_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
