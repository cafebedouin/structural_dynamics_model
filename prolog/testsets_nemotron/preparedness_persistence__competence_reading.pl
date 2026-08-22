% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence__competence_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: preparedness_persistence__competence_reading
 *   human_readable: Operational Readiness Through Live-Exercised Practice
 *   domain: institutional/operational/commitment_system
 *
 * SUMMARY:
 *   This constraint instantiates the competence_reading of the
 *   preparedness_persistence kernel. It holds that drills, exercises, and
 *   inspections are not symbolic performances but live-exercised knowledge —
 *   the physical and cognitive infrastructure of disaster response is
 *   maintained only through realistic practice that exercises full response
 *   chains under unscripted conditions. The constraint is Mountain (the
 *   physical reality: without exercise, knowledge and coordination decay as a
 *   matter of physics and biology) plus Rope (the coordination function:
 *   multi-agency response requires practiced interoperability that cannot be
 *   improvised). No extraction structure exists — no party profits from the
 *   drills themselves, no victim class bears their cost asymmetrically; the
 *   cost is the price of the coordination function. The husk_reading claims
 *   this is memorial theater; the hybrid_reading claims it is stratified.
 *   This reading asserts uniform competence is achievable and empirically
 *   observable in institutions that maintain drill fidelity.
 *
 * KEY AGENTS:
 *   - operational_institutions: Primary agenda_setter (institutional/generational/arbitrage/global) — designs and executes drill regimes; bears cost of realistic exercise; benefits from maintained readiness
 *   - frontline_responders: Primary beneficiary (organized/biographical/mobile/continental) — gains muscle memory, cross-agency fluency, and decision speed from realistic drills; exit is mobile (can transfer skills)
 *   - incident_victims: Secondary beneficiary (powerless/immediate/trapped/local) — bears consequences when readiness fails; not a direct participant but the ultimate stakeholder of the coordination function
 *   - audit_authorities: Observer (institutional/generational/analytical/national) — verifies drill fidelity against standards; does not run drills or bear their cost
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__competence_reading, 0.03).
domain_priors:suppression_score(preparedness_persistence__competence_reading, 0.05).
domain_priors:theater_ratio(preparedness_persistence__competence_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, extractiveness, 0.03).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__competence_reading, mountain).
narrative_ontology:human_readable(preparedness_persistence__competence_reading, "Operational Readiness Through Live-Exercised Practice").
narrative_ontology:topic_domain(preparedness_persistence__competence_reading, "institutional/operational/commitment_system").

domain_priors:emerges_naturally(preparedness_persistence__competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__competence_reading, 'e98683a2-0a75-4fad-b8f0-59c6c307c936').
narrative_ontology:cs_kernel_codification('e98683a2-0a75-4fad-b8f0-59c6c307c936', distributed).
narrative_ontology:cs_authority_grounding('e98683a2-0a75-4fad-b8f0-59c6c307c936', practice).
narrative_ontology:cs_interpretation_layer_present('e98683a2-0a75-4fad-b8f0-59c6c307c936').
narrative_ontology:cs_reading_relation('e98683a2-0a75-4fad-b8f0-59c6c307c936', preparedness_persistence__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('e98683a2-0a75-4fad-b8f0-59c6c307c936', preparedness_persistence__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('e98683a2-0a75-4fad-b8f0-59c6c307c936', foundational, exercise_maintains_competence).
narrative_ontology:cs_axiom_status(exercise_maintains_competence, holdable).
narrative_ontology:cs_axiom_grounding('e98683a2-0a75-4fad-b8f0-59c6c307c936', exercise_maintains_competence, empirically_contingent).
narrative_ontology:cs_axiom('e98683a2-0a75-4fad-b8f0-59c6c307c936', foundational, drill_fidelity_threshold_is_achievable).
narrative_ontology:cs_axiom_status(drill_fidelity_threshold_is_achievable, holdable).
narrative_ontology:cs_axiom_grounding('e98683a2-0a75-4fad-b8f0-59c6c307c936', drill_fidelity_threshold_is_achievable, empirically_contingent).
narrative_ontology:cs_reference_frame('e98683a2-0a75-4fad-b8f0-59c6c307c936', institutional_practice_standard).
narrative_ontology:cs_drift_state('e98683a2-0a75-4fad-b8f0-59c6c307c936', post_covid_after_action_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('e98683a2-0a75-4fad-b8f0-59c6c307c936', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__competence_reading, preparedness_persistence).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, operational_institutions).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, frontline_responders).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, incident_victims).
narrative_ontology:constraint_vindicates(preparedness_persistence__competence_reading, practice_maintains_competence).
narrative_ontology:constraint_vindicates(preparedness_persistence__competence_reading, operational_readiness_is_physical_not_symbolic).
narrative_ontology:constraint_vindicates(preparedness_persistence__competence_reading, drill_realism_preserves_knowledge).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, funds, and executes drill regimes across hazard scenarios. Bears the full cost of realistic exercise (personnel time, equipment wear, scenario development, cross-agency coordination overhead). Receives the direct benefit of maintained operational readiness — the institution's core function depends on it. Can shift drill investment across hazards and jurisdictions; exit means accepting degradation of response capacity.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, operational_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__competence_reading, operational_institutions, beneficiary).

% Participates in drills as primary trainees. Gains muscle memory, decision heuristics under stress, cross-agency communication fluency, and equipment familiarity that cannot be acquired in classroom or tabletop settings. The skills are portable across agencies and jurisdictions — exit is mobile. Bears personal cost (time, physical risk in realistic exercises) but receives disproportionate professional benefit.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, frontline_responders, beneficiary,
    organized, biographical, mobile, continental).

% The population exposed to hazard when readiness fails. Does not participate in drills and cannot exit the hazard zone. Bears catastrophic cost (life, property, displacement) if the coordination function collapses. Is the ultimate stakeholder of the constraint's success but has no voice in its design or execution. This reading treats their interest as the constraint's vindicated proposition (readiness saves lives), not as a victim of the constraint itself.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, incident_victims, beneficiary,
    powerless, immediate, trapped, local).

% Verifies drill fidelity against regulatory and professional standards. Reviews after-action reports, observes exercises, certifies compliance. Does not design or fund drills, does not bear their operational cost, does not receive readiness benefit directly. Their analytical seat sees the full structure — they are the empirical check on whether this reading or the husk_reading describes reality.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, audit_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Multi-agency disaster response requires practiced interoperability that cannot be improvised during an incident. Live exercises are the only mechanism that maintains the physical-cognitive infrastructure of command, control, communication, and coordination across organizational boundaries under stress.
% TRANSFER_FUNCTION: Moves institutional resources (personnel time, equipment, scenario development budget) into maintained operational readiness — the capacity to execute coordinated response under real incident conditions. The transfer is cost-for-function, not rent extraction; no seat captures the transfer as gain.
% ABSENT_VOICES: The husk_reading and hybrid_reading would argue that many institutions' drills ARE ritualized — that the excluded voice is the frontline responder who knows the drills are theater but cannot say so. In this reading, that voice is not excluded; the drill regime itself (when run at fidelity) surfaces that feedback through after-action reviews.
% DISAPPEARANCE_RATIONALE: If live-exercised drills and inspections disappeared overnight, operational knowledge would decay on a timescale of months to years (empirically observed in post-Cold-War civil defense, post-9/11 hospital surge capacity, etc.). Incident outcomes would deteriorate measurably within the first major event. The physical infrastructure of readiness (knowledge, coordination, muscle memory) cannot be maintained without the exercise that creates it.
% FOUNDING_PROBLEM: Operational knowledge and multi-agency coordination decay irreversibly without realistic practice. Tabletop exercises, paper plans, and classroom training do not maintain the physical-cognitive infrastructure required for incident response. The founding problem is the entropy of unexercised competence.
% FOUNDING_PROBLEM_CORROBORATION: Every major incident after-action report (9/11 Commission, Katrina, Fukushima, COVID-19, Maui wildfires) identifies unexercised coordination failures as primary contributors to poor outcomes. Independent after-action reviews from outside the benefiting institutions (GAO, NAS, international peer reviews) consistently corroborate that drill fidelity predicts incident performance. No credible source contends the problem is solved or gone.
narrative_ontology:disappearance_verdict(preparedness_persistence__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(preparedness_persistence__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__competence_reading, 0.03, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence__competence_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, ExtMetricName, E),
    domain_priors:suppression_score(preparedness_persistence__competence_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(preparedness_persistence__competence_reading),
    narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(preparedness_persistence__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.03): the drills extract no rent; their cost is the coordination function's necessary overhead. Suppression is negligible (0.05): alternatives (no drills, paper exercises) are not suppressed — they are known to fail. Theater ratio is low (0.08): the vast majority of drill activity is functional; any performative element is noise, not structure. Accessibility collapse is high (0.92): once you accept that operational knowledge decays without exercise, there is no alternative to live practice — the physical world enforces it. Resistance is near-zero (0.04): no constituency opposes realistic drills; opposition only arises when drills are theatrical (which this reading denies is the norm).
 *
 * PERSPECTIVAL GAP:
 *   The husk_reading would compute this as Piton (high theater, low function); the hybrid_reading would compute it as Tangled Rope in some sectors (coordination + extraction). This reading's structural data produces Mountain-Rope uniformly because it asserts the drill fidelity threshold is met. The engine will compute Mountain-Rope from these metrics; the divergence from sibling readings is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   All named seats are beneficiaries of the coordination function. Operational institutions pay the cost of drills but receive the readiness benefit — symmetric (d ~ 0.5). Frontline responders receive disproportionate benefit relative to their cost share — d < 0.5. Incident victims bear catastrophic cost if readiness fails but have no exit — their d is high for the *absence* of the constraint, but the constraint itself does not extract from them. No victim class exists in this reading; the husk_reading would create one (taxpayers/public paying for theater).
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy risk: the founding problem (operational knowledge decays without exercise) remains live and is corroborated by every major incident after-action report. The arrangement persists because the problem persists; no extraction structure has layered onto it. If drill fidelity decayed into theater, mandatrophy would activate — but this reading asserts that has not occurred.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disambiguation,
    'Is preparedness a genuine Mountain-Rope hybrid (this reading), a ritualized husk (husk_reading), or a stratified mix (hybrid_reading)?',
    'Empirical audit of drill fidelity vs. incident outcomes across comparable institutions; cross-institutional comparison of inspection regimes that do/don''t exercise full response chains.',
    'If husk_reading is validated, extractiveness rises sharply (theatrical maintenance of form without function); if hybrid_reading holds, this reading''s claim of uniform competence is overbroad and sector-specific. Both would invalidate the pure Mountain-Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'Which reading of the preparedness kernel is structurally accurate.').

omega_variable(
    practice_vs_performance_boundary,
    'Where does live exercise shade into ritual — what fidelity threshold separates Mountain from Piton?',
    'Longitudinal tracking of drill realism metrics (unscripted variables, cross-agency integration, consequence severity) correlated with actual incident performance; institutional ethnography of drill culture.',
    'If the boundary is lower than assumed, many ''competent'' institutions are actually Piton-class — theater_ratio understated, classification shifts. This reading asserts a high threshold (realistic exercise = Mountain); the husk_reading asserts the threshold is routinely breached.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(practice_vs_performance_boundary, empirical, 'The fidelity threshold that distinguishes live exercise from memorial performance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__competence_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_persistence__competence_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(prep_tr_t5, preparedness_persistence__competence_reading, theater_ratio, 5, 0.06).
narrative_ontology:measurement(prep_tr_t10, preparedness_persistence__competence_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement(prep_tr_t15, preparedness_persistence__competence_reading, theater_ratio, 15, 0.07).
narrative_ontology:measurement(prep_tr_t20, preparedness_persistence__competence_reading, theater_ratio, 20, 0.08).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_persistence__competence_reading, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(prep_be_t5, preparedness_persistence__competence_reading, base_extractiveness, 5, 0.02).
narrative_ontology:measurement(prep_be_t10, preparedness_persistence__competence_reading, base_extractiveness, 10, 0.03).
narrative_ontology:measurement(prep_be_t15, preparedness_persistence__competence_reading, base_extractiveness, 15, 0.03).
narrative_ontology:measurement(prep_be_t20, preparedness_persistence__competence_reading, base_extractiveness, 20, 0.03).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_persistence__competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__competence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_persistence__competence_reading, 0.08).
narrative_ontology:affects_constraint(preparedness_persistence__competence_reading, preparedness_persistence__husk_reading).
narrative_ontology:affects_constraint(preparedness_persistence__competence_reading, preparedness_persistence__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the preparedness_persistence kernel. The husk_reading and hybrid_reading are sibling constraints with different ε values and stakeholder structures. This reading claims uniform Mountain-Rope classification; the husk_reading claims Piton/Snare; the hybrid_reading claims a constraint family with mixed types. All three share the same kernel_id but instantiate different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
