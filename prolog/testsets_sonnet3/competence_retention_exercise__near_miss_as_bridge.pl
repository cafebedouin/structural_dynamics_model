% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__near_miss_as_bridge
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_retention_exercise__near_miss_as_bridge, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: competence_retention_exercise__near_miss_as_bridge
 *   human_readable: Near-Miss Investigation as Bridge Between Simulation and Catastrophe for Competence Retention
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   In high-reliability domains (aviation, nuclear power, chemical process
 *   safety), the question of what validates ongoing competence training is
 *   contested. This constraint instantiates the 'near-miss as bridge'
 *   reading: near-miss incidents and minor failures, actively investigated
 *   and fed back into simulator curricula, provide sufficient real-world
 *   grounding to keep training valid without requiring catastrophic events.
 *   This is distinct from a pure simulation-sufficiency reading (which claims
 *   simulation alone is structurally equivalent to reality) and from a
 *   catastrophe-necessity reading (which claims only actual disaster produces
 *   genuine learning). The near-miss bridge reading treats catastrophe as
 *   neither necessary nor sufficient — the real epistemic work is done by
 *   disciplined investigation and integration of sub-catastrophic events.
 *
 * KEY AGENTS:
 *   - operating_crews: beneficiaries of validated training, also the source of near-miss data
 *   - safety_investigation_units: agenda-setters administering the reporting-to-curriculum pipeline
 *   - regulatory_oversight_bodies: beneficiaries who certify competence without demanding catastrophic proof
 *   - simulator_curriculum_designers: beneficiaries whose scenario libraries depend on real incident input
 *   - frontline_reporting_staff: payers bearing personal exposure risk from self-reporting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__near_miss_as_bridge, 0.28).
domain_priors:suppression_score(competence_retention_exercise__near_miss_as_bridge, 0.22).
domain_priors:theater_ratio(competence_retention_exercise__near_miss_as_bridge, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, extractiveness, 0.28).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__near_miss_as_bridge, rope).
narrative_ontology:human_readable(competence_retention_exercise__near_miss_as_bridge, "Near-Miss Investigation as Bridge Between Simulation and Catastrophe for Competence Retention").
narrative_ontology:topic_domain(competence_retention_exercise__near_miss_as_bridge, "safety_engineering/organizational_learning").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__near_miss_as_bridge, '3ce00764-22a2-4926-97f7-85b8709e8778').
narrative_ontology:cs_kernel_codification('3ce00764-22a2-4926-97f7-85b8709e8778', distributed).
narrative_ontology:cs_authority_grounding('3ce00764-22a2-4926-97f7-85b8709e8778', practice).
narrative_ontology:cs_interpretation_layer_present('3ce00764-22a2-4926-97f7-85b8709e8778').
narrative_ontology:cs_reading_relation('3ce00764-22a2-4926-97f7-85b8709e8778', competence_retention_exercise__simulation_as_sufficient, coexists_with).
narrative_ontology:cs_reading_relation('3ce00764-22a2-4926-97f7-85b8709e8778', competence_retention_exercise__catastrophe_as_necessary, coexists_with).
narrative_ontology:cs_axiom('3ce00764-22a2-4926-97f7-85b8709e8778', foundational, sub_catastrophic_feedback_is_epistemically_adequate).
narrative_ontology:cs_axiom_status(sub_catastrophic_feedback_is_epistemically_adequate, holdable).
narrative_ontology:cs_axiom_grounding('3ce00764-22a2-4926-97f7-85b8709e8778', sub_catastrophic_feedback_is_epistemically_adequate, empirically_contingent).
narrative_ontology:cs_axiom('3ce00764-22a2-4926-97f7-85b8709e8778', foundational, catastrophe_exposure_is_neither_necessary_nor_sufficient_for_competence).
narrative_ontology:cs_axiom_status(catastrophe_exposure_is_neither_necessary_nor_sufficient_for_competence, holdable).
narrative_ontology:cs_axiom_grounding('3ce00764-22a2-4926-97f7-85b8709e8778', catastrophe_exposure_is_neither_necessary_nor_sufficient_for_competence, empirically_contingent).
narrative_ontology:cs_reference_frame('3ce00764-22a2-4926-97f7-85b8709e8778', post_disaster_reactive_training_model).
narrative_ontology:cs_drift_state('3ce00764-22a2-4926-97f7-85b8709e8778', contemporary_high_reliability_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3ce00764-22a2-4926-97f7-85b8709e8778', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__near_miss_as_bridge, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, operating_crews).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, safety_investigation_units).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, regulatory_oversight_bodies).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, simulator_curriculum_designers).
narrative_ontology:constraint_victim(competence_retention_exercise__near_miss_as_bridge, frontline_reporting_staff).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rely on simulator training that stays current because near-miss reports feed new failure modes back into scenario design. Their competence is tested against situations that actually occurred, not just designer imagination. They cannot personally choose whether a catastrophe happens to validate their training, but they can and do generate the near-miss reports that keep the loop live.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, operating_crews, beneficiary,
    moderate, biographical, constrained, national).

% Run the near-miss reporting and investigation apparatus: collect incident reports, extract causal chains, and translate findings into simulator curriculum updates. They administer the bridge between real-world minor failure and rehearsed competence, and could in principle let the near-miss channel atrophy in favor of pure simulation or wait-for-disaster postures.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, safety_investigation_units, agenda_setter,
    institutional, generational, analytical, national).

% Certify that training programs remain valid without demanding proof of catastrophic exposure. The near-miss bridge gives them an evidentiary basis for competence claims that does not require waiting for or causing disasters, which is what makes ongoing certification politically and ethically viable.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, regulatory_oversight_bodies, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__near_miss_as_bridge, regulatory_oversight_bodies, observer).

% Depend on a steady supply of real incident data to keep scenarios from calcifying into stale, gameable set-pieces. Without near-miss inputs their simulators drift toward theater; with them, the training stays anchored to actual system behavior. Their professional value rests on the bridge holding.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, simulator_curriculum_designers, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__near_miss_as_bridge, simulator_curriculum_designers, agenda_setter).

% Bear the exposure of self-reporting near-misses that could be read as personal error, in blame-retaining cultures, even when the formal system claims non-punitive reporting. They do the labor that makes the whole bridge function, and carry disproportionate career risk if the promised protections are not honored in practice.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, frontline_reporting_staff, payer,
    powerless, immediate, constrained, local).

% Hold that only actual catastrophic failure produces the organizational learning and visceral stakes competence retention requires; they are not administering this bridge system and their view that near-miss data is a pale substitute for real disaster does not enter the operational decision loop, though it surfaces in post-incident debate.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, catastrophe_as_necessary_advocates, excluded,
    moderate, generational, analytical, national).

% Hold that high-fidelity simulation alone is structurally equivalent to real events and that near-miss integration is unnecessary overhead; they compete for training budget and design authority against the hybrid bridge model this constraint describes.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, simulation_as_sufficient_advocates, excluded,
    organized, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_retention_exercise__near_miss_as_bridge, diffuse).
narrative_ontology:fixing_cost_class(competence_retention_exercise__near_miss_as_bridge, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the maintenance of operational competence across a workforce that cannot ethically or practically be exposed to real catastrophes for training purposes, by channeling minor real-world failures into simulator curriculum revision — closing the gap between rehearsal and reality without requiring disaster.
% TRANSFER_FUNCTION: Moves informational value from frontline incident-reporting labor (who generate the raw near-miss data, often at personal exposure) to curriculum designers, investigation units, and ultimately the wider operating workforce and regulators who benefit from validated, current training.
% ABSENT_VOICES: Advocates of the catastrophe-as-necessary view argue the bridge is a comfortable fiction that lets institutions claim competence without paying its real cost; advocates of simulation-as-sufficient argue the near-miss channel is unnecessary friction. Neither faction administers the reporting-to-curriculum pipeline and both are structurally outside the operational decision loop this constraint describes.
% DISAPPEARANCE_RATIONALE: If near-miss investigation and integration vanished, simulator curricula would drift toward designer assumption and stale scenario libraries, competence certification would either revert to catastrophe-dependent validation or become unmoored from real failure modes, and frontline reporting incentives would collapse since there would be no downstream use for the reports.
% FOUNDING_PROBLEM: Organizations needed a way to validate and continuously update rehearsal-based training without waiting for, or causing, an actual catastrophic failure — the founding problem was closing the credibility gap between simulated competence and demonstrated real-world competence.
% FOUNDING_PROBLEM_CORROBORATION: Independent aviation and nuclear-industry safety researchers outside the certifying bodies and outside the curriculum-design firms attest that near-miss-informed training updates measurably track real system failure modes better than simulation alone; this corroboration comes from academic human-factors literature and cross-industry incident databases, not solely from the investigation units or regulators who benefit from the arrangement.
narrative_ontology:disappearance_verdict(competence_retention_exercise__near_miss_as_bridge, world_rearranges).
narrative_ontology:founding_problem_status(competence_retention_exercise__near_miss_as_bridge, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__near_miss_as_bridge, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_retention_exercise__near_miss_as_bridge, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__near_miss_as_bridge, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__near_miss_as_bridge_tests).
:- end_tests(competence_retention_exercise__near_miss_as_bridge_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low-moderate (0.28) and rises only slightly over the interval: the coordination function (keeping training current) is real and dominant, but a genuine cost is borne by frontline reporters whose incident disclosures can be used against them despite formal non-punitive-reporting policies — this is the source of the small but real extraction and victim declaration. Suppression is low (0.22) because reporting is generally voluntary and alternatives (withholding reports, informal workarounds) are not systematically blocked, though career risk creates some chilling effect. Theater ratio is low and rises slowly (0.10 to 0.18) reflecting a mild drift toward compliance-oriented reporting rituals as programs mature, but the function remains substantially real. Accessibility collapse is moderate (0.35): once understood, the reporting-to-curriculum pipeline is hard for an individual reporter to route around, but organizational alternatives (whistleblower channels, union grievance processes) persist. Resistance is moderate (0.4), consistent with genuine but contained friction from reporting staff and from rival curriculum-design factions.
 *
 * PERSPECTIVAL GAP:
 *   From the investigation unit and regulator seats, this is straightforwardly rope: a low-coercion coordination mechanism that lets competence be validated ethically. From the frontline reporting seat, the same mechanism can register as quietly extractive — labor and exposure without matching protection — even though the aggregate extraction score stays low. The engine should show this seat divergence: institutional seats near full beneficiary, frontline seat pulled toward the target end despite the constraint's generally benign profile.
 *
 * DIRECTIONALITY LOGIC:
 *   Operating crews, investigation units, regulators, and curriculum designers all sit toward the beneficiary end: the arrangement subsidizes their competence claims, certification legitimacy, and professional relevance respectively. Frontline reporting staff sit closer to the target end: they generate the raw material the whole bridge depends on, at personal risk, with constrained exit (their livelihood depends on continuing to work in the reporting system) — this asymmetry is what grounds the victim declaration despite the constraint's overall low extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (closing the credibility gap between rehearsal and reality without catastrophe) remains live per corroborating academic and cross-industry evidence, which is why this is not treated as mandatrophy — the near-miss bridge is still doing real work, not merely performing it. Should independent corroboration later show curriculum updates no longer track real incident patterns (rising theater_ratio, stagnant reporting quality), that would be the signal the bridge has degraded toward pure ritual.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    near_miss_sufficiency_ambiguity,
    'Is near-miss data actually sufficient to capture the full space of catastrophic failure modes, or does it systematically under-sample low-frequency, high-severity tail events that only manifest in actual disasters?',
    'Comparative analysis of post-catastrophe root-cause findings against pre-catastrophe near-miss investigation records in the same domain: if the catastrophe''s causal chain was already visible in near-miss data, the bridge reading is supported; if the causal chain was novel and absent from near-miss records, the catastrophe-necessity reading gains ground.',
    'If near-miss data systematically misses tail-risk causal chains, this constraint''s claimed sufficiency is overstated and the classification should shift toward acknowledging a residual dependency on catastrophe exposure that the near-miss bridge cannot substitute for.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(near_miss_sufficiency_ambiguity, empirical, 'Whether near-miss data captures the full distribution of failure modes or only the more frequent, lower-severity tail.').

omega_variable(
    reporting_culture_authenticity,
    'Is the frontline non-punitive reporting protection genuinely honored in practice across the organizations that rely on this bridge, or is it a formal policy that is quietly violated when politically convenient?',
    'Track disciplinary and career outcomes for reporting staff following near-miss disclosures, compared against the formal non-punitive-reporting policy commitments, across multiple organizations and over time.',
    'If protection is frequently violated, the frontline_reporting_staff victim declaration is understated and effective extraction (and suppression) on that seat is higher than the low aggregate suppression score suggests — the constraint would drift toward tangled_rope at that seat even while remaining rope in aggregate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reporting_culture_authenticity, empirical, 'Whether non-punitive reporting protections for frontline staff are actually honored or only nominally declared.').

omega_variable(
    committer_reading_selection_ambiguity,
    'Is the choice of ''near_miss_as_bridge'' over its sibling readings (''simulation_as_sufficient'' and ''catastrophe_as_necessary'') itself contestable within a single organization''s actual practice, or do organizations cleanly self-sort into one reading?',
    'Survey of actual training-validation criteria used across high-reliability organizations: do they explicitly rely on near-miss integration as load-bearing, treat simulation alone as sufficient, or implicitly wait for disaster to trigger major curriculum overhaul?',
    'If most organizations in practice operate a hybrid that leans more heavily on one sibling reading than declared policy admits, the near_miss_as_bridge reading may be aspirational rather than descriptive for a subset of the domain, which would require decomposing this story further by sub-domain rather than treating it as a single clean reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_reading_selection_ambiguity, conceptual, 'Whether the near-miss-as-bridge reading is a clean structural description or an aspirational policy claim that masks reliance on sibling readings in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__near_miss_as_bridge, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 0, 0.1).
narrative_ontology:measurement(comp_tr_t4, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 4, 0.12).
narrative_ontology:measurement(comp_tr_t8, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 8, 0.13).
narrative_ontology:measurement(comp_tr_t12, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 12, 0.15).
narrative_ontology:measurement(comp_tr_t16, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 16, 0.16).
narrative_ontology:measurement(comp_tr_t20, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 20, 0.17).
narrative_ontology:measurement(comp_tr_t24, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 24, 0.18).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(comp_be_t4, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 4, 0.24).
narrative_ontology:measurement(comp_be_t8, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 8, 0.25).
narrative_ontology:measurement(comp_be_t12, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 12, 0.26).
narrative_ontology:measurement(comp_be_t16, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 16, 0.27).
narrative_ontology:measurement(comp_be_t20, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(comp_be_t24, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 24, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(competence_retention_exercise__near_miss_as_bridge, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__near_miss_as_bridge, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_retention_exercise__near_miss_as_bridge, 0.1).
narrative_ontology:affects_constraint(competence_retention_exercise__near_miss_as_bridge, competence_retention_exercise__simulation_as_sufficient).
narrative_ontology:affects_constraint(competence_retention_exercise__near_miss_as_bridge, competence_retention_exercise__catastrophe_as_necessary).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the competence_retention_exercise kernel, decomposed per the epsilon-invariance principle because the three readings assign structurally different sufficiency conditions (and different implied ε) to the same underlying question of what validates competence retention. The near_miss_as_bridge reading claims a hybrid mechanism with low-moderate extraction and a narrow but real victim class (frontline reporters); the simulation_as_sufficient reading would claim negligible extraction (pure rope, no catastrophe or near-miss dependency); the catastrophe_as_necessary reading would claim high extraction (dependency on disaster exposure imposes costs on those exposed to or caught in catastrophic events). Each is authored as its own file with its own stakeholders and ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
