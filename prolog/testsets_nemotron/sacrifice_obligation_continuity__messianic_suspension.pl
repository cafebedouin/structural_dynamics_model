% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__messianic_suspension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__messianic_suspension, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: sacrifice_obligation_continuity__messianic_suspension
 *   human_readable: Sacrifice Obligation Continuity (Messianic Suspension Reading)
 *   domain: religious/ritual/textual
 *
 * SUMMARY:
 *   The messianic_suspension reading holds that the biblical obligation to
 *   offer sacrifices was not abrogated by the Temple's destruction but
 *   suspended by divine will pending messianic restoration. Study of
 *   sacrificial law (especially Kodashim) functions as a
 *   readiness-maintenance protocol — keeping the system 'warm' for
 *   reactivation. This reading instantiates a scaffold: it has a declared
 *   sunset (messianic restoration), a coordination function (preserving
 *   system coherence), and moderate extractiveness (the readiness burden)
 *   without active victims because the obligation is explicitly suspended,
 *   not enforced.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__messianic_suspension, 0.38).
domain_priors:suppression_score(sacrifice_obligation_continuity__messianic_suspension, 0.15).
domain_priors:theater_ratio(sacrifice_obligation_continuity__messianic_suspension, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, extractiveness, 0.38).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__messianic_suspension, scaffold).
narrative_ontology:human_readable(sacrifice_obligation_continuity__messianic_suspension, "Sacrifice Obligation Continuity (Messianic Suspension Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__messianic_suspension, "religious/ritual/textual").

narrative_ontology:has_sunset_clause(sacrifice_obligation_continuity__messianic_suspension).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__messianic_suspension, '3d0a1dc9-00bf-4aa3-b4c2-534efdc5fa5d').
narrative_ontology:cs_kernel_codification('3d0a1dc9-00bf-4aa3-b4c2-534efdc5fa5d', fixed_text).
narrative_ontology:cs_authority_grounding('3d0a1dc9-00bf-4aa3-b4c2-534efdc5fa5d', lineage).
narrative_ontology:cs_interpretation_layer_present('3d0a1dc9-00bf-4aa3-b4c2-534efdc5fa5d').
narrative_ontology:cs_reading_relation('3d0a1dc9-00bf-4aa3-b4c2-534efdc5fa5d', sacrifice_obligation_continuity__archival_preservation, forecloses).
narrative_ontology:cs_reading_relation('3d0a1dc9-00bf-4aa3-b4c2-534efdc5fa5d', sacrifice_obligation_continuity__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('3d0a1dc9-00bf-4aa3-b4c2-534efdc5fa5d', sacrifice_obligation_continuity__study_as_performance, influences).
narrative_ontology:cs_axiom('3d0a1dc9-00bf-4aa3-b4c2-534efdc5fa5d', foundational, commandment_suspended_not_abrogated).
narrative_ontology:cs_axiom_status(commandment_suspended_not_abrogated, holdable).
narrative_ontology:cs_axiom_grounding('3d0a1dc9-00bf-4aa3-b4c2-534efdc5fa5d', commandment_suspended_not_abrogated, deontological).
narrative_ontology:cs_axiom('3d0a1dc9-00bf-4aa3-b4c2-534efdc5fa5d', foundational, messianic_restoration_as_sunset).
narrative_ontology:cs_axiom_status(messianic_restoration_as_sunset, holdable).
narrative_ontology:cs_axiom_grounding('3d0a1dc9-00bf-4aa3-b4c2-534efdc5fa5d', messianic_restoration_as_sunset, theological).
narrative_ontology:cs_axiom('3d0a1dc9-00bf-4aa3-b4c2-534efdc5fa5d', secondary, study_maintains_readiness_not_fulfillment).
narrative_ontology:cs_axiom_status(study_maintains_readiness_not_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('3d0a1dc9-00bf-4aa3-b4c2-534efdc5fa5d', study_maintains_readiness_not_fulfillment, conventional).
narrative_ontology:cs_reference_frame('3d0a1dc9-00bf-4aa3-b4c2-534efdc5fa5d', sinai_temple_service_continuity).
narrative_ontology:cs_drift_state('3d0a1dc9-00bf-4aa3-b4c2-534efdc5fa5d', contemporary_post_temple_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3d0a1dc9-00bf-4aa3-b4c2-534efdc5fa5d', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, rabbinic_authority_tradition).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, study_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, messianic_restoration_awaiters).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__messianic_suspension, study_institutions).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__messianic_suspension, observant_laity).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__messianic_suspension, messianic_restoration_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__messianic_suspension, commandment_suspension_not_violation_principle).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__messianic_suspension, study_as_readiness_maintenance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the interpretive framework that suspends rather than abrogates the sacrifice obligation. Preserves institutional continuity across the Temple's absence while avoiding the guilt-structure of active violation. Collects legitimacy from being the designated custodians of the suspended system.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, rabbinic_authority_tradition, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__messianic_suspension, rabbinic_authority_tradition, beneficiary).

% Yeshivas and kollels that organize curricula around sacrificial law (Kodashim tractates, Temple service codes). They benefit from the sustained relevance of their core curriculum and the institutional funding that follows. They pay in the form of scholarly labor directed toward a system that cannot currently be practiced, diverting energy from other possible foci.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, study_institutions, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__messianic_suspension, study_institutions, payer).

% Bear the readiness burden: they structure their religious identity around a system they cannot enact, internalize the loss as exile rather than failure, and fund the study institutions that maintain the suspension framework. No active extraction occurs because the obligation is suspended, not demanded.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, observant_laity, payer,
    moderate, biographical, constrained, global).

% Their entire religious self-concept is fused with the future restoration. The suspension framework gives their waiting a structured grammar — they are not 'doing nothing,' they are 'maintaining readiness.' Exit would require dismantling the identity that makes the wait meaningful.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, messianic_restoration_awaiters, beneficiary,
    powerless, civilizational, identity_locked, universal).

% Historians and scholars of religion who read the suspension as a post-hoc rationalization for institutional survival after 70 CE. They would argue the obligation was not suspended but transformed, and that the readiness narrative obscures the creative adaptation. Not seated in the tradition's internal conversation.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, critical_scholars_outside_tradition, excluded,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(sacrifice_obligation_continuity__messianic_suspension, critical_scholars_outside_tradition).

% Explicitly rejected the sacrifice obligation and its restoration as part of their theological program. They are not 'waiting' — they have exited the framework entirely. Their absence from the conversation is structural, not accidental.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, reform_liberal_judaism, excluded,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a coherent religious-legal system across a 2000-year gap in physical practice by framing the suspension as a divinely ordained pause rather than a human abrogation. This preserves the tradition's internal consistency and the authority structure that depends on it.
% TRANSFER_FUNCTION: Moves scholarly labor, institutional resources, and identity-investment from the observant laity and study institutions toward the maintenance of a suspended system. The transfer is voluntary and framed as sacred duty, not coerced extraction.
% ABSENT_VOICES: Critical historians who would frame the suspension as a survival strategy rather than a divine pause; Reform/Liberal movements that have theologically exited the system; Palestinian/Second Temple groups whose alternative halakhic trajectories were suppressed. The first are excluded by epistemic boundary; the second by theological exit; the third by historical contingency.
% DISAPPEARANCE_RATIONALE: If the suspension framework vanished, the tradition would face a trilemma: declare the commandments abrogated (breaking continuity), declare current non-performance sinful (creating mass guilt), or invent a new fulfillment mode. The study institutions would lose their curricular anchor; the laity's identity grammar would collapse; the messianic awaiters' structured waiting would dissolve.
% FOUNDING_PROBLEM: After the Temple's destruction in 70 CE, the sacrificial system — the central pillar of biblical religion — became physically impossible. The tradition needed to preserve the commandments' authority without demanding the impossible, and without admitting the system had ended.
% FOUNDING_PROBLEM_CORROBORATION: The rabbinic tradition itself attests the problem is live (messianic restoration remains the telos). Critical scholars outside the tradition (e.g., Neusner, Boyarin, Schwartz) attest the problem was solved by creative transformation, not suspension — the 'founding problem' is a retrospective construction. No neutral arbiter exists; the contest is structural.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__messianic_suspension, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__messianic_suspension, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__messianic_suspension, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(sacrifice_obligation_continuity__messianic_suspension, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__messianic_suspension, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__messianic_suspension_tests).
:- end_tests(sacrifice_obligation_continuity__messianic_suspension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) reflects the real but non-coercive burden of maintaining readiness across millennia — scholarly labor, identity investment, institutional resources directed toward a system that cannot be practiced. Suppression (0.15) is low because the constraint operates through identity and tradition, not force; alternatives (Reform exit, secular exit) exist and are taken. Theater ratio (0.22) captures the growing performative element: as restoration recedes temporally, the study increasingly resembles ritualized preservation rather than genuine preparation. Accessibility collapse (0.45) is moderate — the suspension framework makes alternatives (abrogation, transformation) thinkable but structurally costly within the tradition. Resistance (0.28) reflects the historical contest from Karaites, Reform, and critical scholarship.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat, this is a divinely ordained scaffold preserving eternal law. From the payer seats (laity, study institutions), it is a genuine coordination structure that carries real but voluntary costs. From the excluded critical seat, it is a post-hoc institutional survival strategy. The engine will compute these divergences from the structural data; the claimed_type (scaffold) reflects the authoring seat's structural judgment.
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinic authority tradition sits near the beneficiary end (d ≈ 0.15) — it collects institutional legitimacy and interpretive monopoly from the suspension framework. Study institutions are near-symmetric (d ≈ 0.5) — they benefit from curricular centrality but pay in diverted scholarly labor. Observant laity are moderate targets (d ≈ 0.6) — they bear the readiness burden and fund the institutions but gain identity coherence. Messianic awaiters are identity-locked beneficiaries (d ≈ 0.2) — the framework structures their waiting as sacred rather than empty. The excluded seats (critical scholars, Reform) would sit at d ≈ 0.8 if seated — they bear the cost of exclusion from the tradition's epistemic authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Temple destruction making sacrifice impossible) was real. The suspension framework solved it elegantly. Whether the problem is 'dead' (restoration impossible) or 'live' (restoration awaited) is the central theological contest. The scaffold classification captures this: the sunset clause (messianic restoration) is the declared endpoint. If restoration never comes, the scaffold becomes a piton — maintained by inertia and identity-lock. The moderate extractiveness without active victims distinguishes it from snare; the sunset clause distinguishes it from rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_vs_transformation_ambiguity,
    'Is the suspension framework a genuine theological claim about divine will, or a post-hoc institutional survival strategy that retroactively reinterprets the Temple''s destruction?',
    'Comparative analysis of early rabbinic sources (Yavneh era, 70-135 CE) to determine whether ''suspension pending restoration'' language appears immediately or emerges generations later as the restoration recedes.',
    'If post-hoc, the constraint''s claimed_type shifts from scaffold (genuine transitional coordination) to piton (inertial maintenance of a transformed system). The beneficiaries (rabbinic authority) would be revealed as constructors rather than custodians.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suspension_vs_transformation_ambiguity, conceptual, 'Whether the suspension narrative is originative or retrospective.').

omega_variable(
    readiness_burden_as_extraction,
    'Does the ''readiness burden'' on observant laity constitute genuine extraction, or is it a voluntary identity-investment that the agents would not describe as costly?',
    'Ethnographic study of contemporary observant communities: measure resource allocation (time, money, educational focus) toward sacrificial law vs. other mitzvot; assess whether agents experience the allocation as burden or privilege.',
    'If experienced as burden, extractiveness is underestimated and the constraint trends toward tangled_rope. If experienced as privilege, the low suppression and moderate extractiveness are accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(readiness_burden_as_extraction, empirical, 'Phenomenology of the readiness burden from the payer seat.').

omega_variable(
    messianic_restoration_as_sunset_credibility,
    'Does the messianic restoration sunset clause retain genuine credibility as a structural endpoint, or has it become a perpetual horizon that can never arrive?',
    'Track theological discourse across the interval: does the restoration timeline contract, expand, or dissolve into metaphor? Correlate with theater_ratio trajectory.',
    'If the sunset has dissolved into metaphor, has_sunset_clause becomes functionally false and the scaffold classification collapses to piton. The 1948/1967 inflection points in measurements test this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_restoration_as_sunset_credibility, conceptual, 'Whether the scaffold''s sunset remains structurally operative.').

omega_variable(
    committer_frame_disagreement_location,
    'Where exactly do the four kernel readings disagree structurally — on the obligation''s current status, the nature of study, the restoration''s necessity, or the Tradition''s authority to adjudicate?',
    'Map each reading''s axiom set (see cs_structure.axioms) and identify the minimal set of propositions whose truth-values differ across readings. The disagreement locus is the set of propositions that are foundational for one reading and denied by another.',
    'If the disagreement is on the obligation''s current status (suspended vs. ended vs. physically required), the kernel is about ontological status of commandments. If on study''s nature (preparation vs. fulfillment vs. preservation), the kernel is about epistemic efficacy of textual engagement. Different loci produce different constraint families.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_disagreement_location, conceptual, 'Structural locus of disagreement among the four kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__messianic_suspension, 70, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t70, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 70, 0.08).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 500, 0.12).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1000, 0.16).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1500, 0.19).
narrative_ontology:measurement(sacr_tr_t1800, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1800, 0.2).
narrative_ontology:measurement(sacr_tr_t1948, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1948, 0.21).
narrative_ontology:measurement(sacr_tr_t1967, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1967, 0.21).
narrative_ontology:measurement(sacr_tr_t2024, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(sacr_be_t70, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 70, 0.15).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 500, 0.22).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1000, 0.28).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1500, 0.32).
narrative_ontology:measurement(sacr_be_t1800, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1800, 0.35).
narrative_ontology:measurement(sacr_be_t1948, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1948, 0.36).
narrative_ontology:measurement(sacr_be_t1967, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1967, 0.37).
narrative_ontology:measurement(sacr_be_t2024, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 2024, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t70, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 70, 0.05).
narrative_ontology:measurement(sacr_su_t500, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 500, 0.08).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 1000, 0.1).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 1500, 0.12).
narrative_ontology:measurement(sacr_su_t1800, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 1800, 0.13).
narrative_ontology:measurement(sacr_su_t1948, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 1948, 0.14).
narrative_ontology:measurement(sacr_su_t1967, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 1967, 0.14).
narrative_ontology:measurement(sacr_su_t2024, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__messianic_suspension, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_continuity__messianic_suspension, 0.1).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity__archival_preservation).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity__performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity__study_as_performance).

% DUAL FORMULATION NOTE:
% This constraint and its three siblings form the sacrifice_obligation_continuity kernel family. Each reading instantiates a different constraint from the same kernel: messianic_suspension (scaffold, sunset at restoration), archival_preservation (mountain/rope, no normative force), performance_only (tangled_rope, physical performance required), study_as_performance (rope, study = fulfillment). The epsilon values differ because each reading assesses the standing arrangement (the suspended obligation system) by its own lights: messianic_suspension sees moderate readiness burden; archival_preservation sees negligible extraction; performance_only sees high extraction (demand for impossible performance); study_as_performance sees low extraction (voluntary engagement).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_obligation_continuity__messianic_suspension, powerless, 0.2).
constraint_indexing:directionality_override(sacrifice_obligation_continuity__messianic_suspension, analytical, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
