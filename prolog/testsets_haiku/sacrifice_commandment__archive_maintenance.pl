% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__archive_maintenance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__archive_maintenance, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: sacrifice_commandment__archive_maintenance
 *   human_readable: Sacrifice Commandment Study as Archive Maintenance for Future Restoration
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   After the Temple's destruction in 70 CE, Jewish communities faced a
 *   halakhic crisis: the commandment to perform sacrifices became physically
 *   impossible. The archive-maintenance reading resolves this by reframing
 *   study of sacrificial law as fulfilling the commandment through knowledge
 *   preservation. Rather than physical ritual (forbidden), the obligation
 *   becomes textual and intellectual—scholars preserve the technical details
 *   (which animals, which procedures, which prayers, which timings) so that
 *   if and when the Temple is rebuilt in a messianic age, the knowledge
 *   exists ready to be implemented. This reading frames present study as
 *   labor performed now for a benefit deferred to an uncertain future; the
 *   present generation bears the cost (study time), the future generation
 *   receives the benefit (ready knowledge). The constraint is CLAIMED as
 *   scaffold (temporary, with a sunset at messianic restoration) but the
 *   authoring reflects moderate extraction because the benefit is contingent,
 *   the future is uncertain, and the present cost is real and ongoing.
 *
 * KEY AGENTS:
 *   - Talmudic scholars: agenda-setters who determine curriculum and enforce study requirements.
 *   - Present observant communities: identity-locked payers bearing the study obligation despite no present material benefit.
 *   - Future messianic generation: theoretical beneficiaries with no voice in present decisions.
 *   - Rabbinical authority bodies: observers who adjudicate the reading's doctrinal status.
 *   - Non-observant communities: excluded from the framework, contesting the obligation's legitimacy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__archive_maintenance, 0.45).
domain_priors:suppression_score(sacrifice_commandment__archive_maintenance, 0.28).
domain_priors:theater_ratio(sacrifice_commandment__archive_maintenance, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, extractiveness, 0.45).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__archive_maintenance, scaffold).
narrative_ontology:human_readable(sacrifice_commandment__archive_maintenance, "Sacrifice Commandment Study as Archive Maintenance for Future Restoration").
narrative_ontology:topic_domain(sacrifice_commandment__archive_maintenance, "religious/halakhic").

domain_priors:requires_active_enforcement(sacrifice_commandment__archive_maintenance).
narrative_ontology:has_sunset_clause(sacrifice_commandment__archive_maintenance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__archive_maintenance, '67a3e342-0861-47e5-ba3b-8c5ccaa5958c').
narrative_ontology:cs_kernel_codification('67a3e342-0861-47e5-ba3b-8c5ccaa5958c', fixed_text).
narrative_ontology:cs_authority_grounding('67a3e342-0861-47e5-ba3b-8c5ccaa5958c', lineage).
narrative_ontology:cs_interpretation_layer_present('67a3e342-0861-47e5-ba3b-8c5ccaa5958c').
narrative_ontology:cs_reading_relation('67a3e342-0861-47e5-ba3b-8c5ccaa5958c', sacrifice_commandment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('67a3e342-0861-47e5-ba3b-8c5ccaa5958c', sacrifice_commandment__study_as_performance, coexists_with).
narrative_ontology:cs_axiom('67a3e342-0861-47e5-ba3b-8c5ccaa5958c', foundational, knowledge_preservation_fulfills_commandment).
narrative_ontology:cs_axiom_status(knowledge_preservation_fulfills_commandment, holdable).
narrative_ontology:cs_axiom_grounding('67a3e342-0861-47e5-ba3b-8c5ccaa5958c', knowledge_preservation_fulfills_commandment, deontological).
narrative_ontology:cs_axiom('67a3e342-0861-47e5-ba3b-8c5ccaa5958c', secondary, messianic_restoration_is_expected_event).
narrative_ontology:cs_axiom_status(messianic_restoration_is_expected_event, holdable).
narrative_ontology:cs_axiom_grounding('67a3e342-0861-47e5-ba3b-8c5ccaa5958c', messianic_restoration_is_expected_event, theological).
narrative_ontology:cs_reference_frame('67a3e342-0861-47e5-ba3b-8c5ccaa5958c', temple_destroyed_commandment_suspended_by_law).
narrative_ontology:cs_drift_state('67a3e342-0861-47e5-ba3b-8c5ccaa5958c', contemporary_post_2000_years_dispersion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('67a3e342-0861-47e5-ba3b-8c5ccaa5958c', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__archive_maintenance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, future_messianic_generation).
narrative_ontology:constraint_victim(sacrifice_commandment__archive_maintenance, present_observant_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and transmit detailed technical knowledge of Temple sacrifice procedures through continuous study, commentary, and textual elaboration. They justify this as preserving the knowledge required for future restoration when the Temple is rebuilt; they set the curriculum and enforce the study requirement on their students and communities.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, talmudic_scholars, agenda_setter,
    organized, generational, identity_locked, global).

% Obligated to devote study time and intellectual resources to sacrificial law despite the commandment being suspended (no Temple exists to perform sacrifices). They cannot perform the physical commandment itself, only study it. The cost is real—time spent studying law about sacrifices cannot be spent on other observances or personal pursuits—but the benefit is deferred to an uncertain messianic future.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, present_observant_communities, payer,
    moderate, biographical, identity_locked, regional).

% Theoretical beneficiary of preserved technical knowledge when/if Temple reconstruction occurs. They inherit a fully elaborated legal and practical apparatus for reinstiating sacrifice. They bear no present cost (they do not exist yet) and receive no present benefit. Their interests are represented argumentatively, not actively.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, future_messianic_generation, beneficiary,
    powerless, civilizational, analytical, universal).

% Do not participate in the study regimen and do not recognize the obligation. They are excluded from the decision-making apparatus that defines the constraint's scope and enforcement; they are outside the religious framework that grounds the commandment.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, non_observant_jewish_communities, excluded,
    moderate, biographical, mobile, regional).

% Adjudicate interpretive disputes about how much detail is necessary to preserve, whether the commandment requires continuous study or occasional review, and whether the archive-maintenance reading is doctrinally sound. They do not themselves benefit or pay; they determine the constraint's boundaries.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, rabbinical_authority_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_commandment__archive_maintenance, diffuse).
narrative_ontology:fixing_cost_class(sacrifice_commandment__archive_maintenance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves knowledge necessary for reinstituting the sacrificial system in a messianic future when the Temple is rebuilt. Coordinates scholarly effort across generations to maintain technical precision and legal elaboration so that detailed understanding does not degrade or fragment.
% TRANSFER_FUNCTION: Moves study time and intellectual labor from present observant communities (who perform the study) to a hypothetical future generation (who would inherit ready-to-implement knowledge). The present generation bears the cost of maintenance; the future generation receives the benefit. In the absence of Temple, the sacrifice commandment itself is transferred from physical performance (impossible) to textual preservation (required).
% ABSENT_VOICES: Descendants of the future messianic era would benefit from preserved knowledge but have no voice in the present decision to impose study obligations. Non-observant Jewish communities are excluded from the framework and would likely contest the use of present labor to preserve knowledge for a religiously contingent future scenario.
% DISAPPEARANCE_RATIONALE: If the study obligation disappeared, the technical knowledge of sacrifice law would degrade over generations; if messianic restoration occurs, the apparatus would need to be reconstructed from fragmentary sources. However, the necessity of this preservation is itself contested—sibling readings deny that knowledge preservation is the proper fulfillment of the commandment (one argues the commandment is suspended; another argues study itself IS the fulfillment, not merely archive maintenance).
% FOUNDING_PROBLEM: After the Temple's destruction (70 CE), the physical performance of sacrifices became impossible. The commandment to maintain the sacrificial system was suspended in practice. The founding problem was: how does a community preserve the commandment's force and knowledge when material performance is forbidden (Jewish law prohibits sacrifices outside the Temple)? The archive-maintenance reading answers: through continuous scholarly study that preserves technical readiness for an eventual restoration.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinical authorities across centuries attest that the study obligation is derived from commandments to 'remember' and 'study' the law. Commentaries in the Talmud, Maimonides, and modern halakhic works affirm the preservation function as a rationale. However, the messianic precondition is contested—some authorities dispute whether the future scenario justifies present labor, and others argue the commandment itself is satisfied through study without reference to future restoration. No external, non-benefiting party can corroborate the messianic premise.
narrative_ontology:disappearance_verdict(sacrifice_commandment__archive_maintenance, contested).
narrative_ontology:founding_problem_status(sacrifice_commandment__archive_maintenance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__archive_maintenance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_commandment__archive_maintenance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__archive_maintenance, 0.45, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__archive_maintenance_tests).
:- end_tests(sacrifice_commandment__archive_maintenance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45 at interval end) because the constraint transfers present labor (study time) to a future benefit (knowledge preservation) whose realization is contingent on messianic restoration—a low-probability, indefinitely-deferred benefit from the present generation's perspective. Suppression is low (0.28) because the constraint is maintained by identity-fusion (being Jewish, being observant means accepting study as mandatory) rather than by coercive enforcement or barrier-creation; participants do not resist because refusing the study obligation would require rejecting religious identity. Theater ratio rises from 0.45 to 0.62 over the interval, indicating that the functional (preservation of technical knowledge) and performative (enacting the commandment symbolically, ritually affirming messianic hope) components drift—study increasingly becomes a rite of faith in restoration rather than technical preparation. The rise in theater_ratio suggests the constraint's primary function shifts from archive maintenance toward maintaining community identity and future hope.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (scholars) and the payers (observant communities) experience this constraint differently. Scholars author the curriculum and see themselves as fulfilling an intellectual duty to preserve knowledge; they have professional prestige and institutional authority (agenda-setting power). Observant communities experience it as a non-negotiable obligation; they cannot exit because exiting the study requirement means breaking religious identity. From the scholars' seat, the constraint is coordination (we preserve together). From the community's seat, the constraint is extraction (we labor now for a benefit we may never see, decided by others). The engine computes these per-seat differences from the structural data: scholars have higher power (organized, institutional), lower exit cost (can reframe their work); communities have moderate power (organized religiously but not in broader society), identity-locked exit (cannot abandon the commandment without apostasy). The messianic beneficiary is powerless and absent.
 *
 * DIRECTIONALITY LOGIC:
 *   The agenda-setter seat (scholars) sits near the beneficiary end: they author curriculum, they gain institutional standing, they frame the interpretation. The present observant community sits near the target end: identity-locked, they cannot exit, they bear the study cost, and they receive no concrete benefit (knowledge is preserved but not used in their lifetime). The theoretical future beneficiary is structured as powerless, analytical—their interests exist in principle but have no voice. The scholars' directionality is low (~0.2–0.3, beneficiary-proximate); the community's directionality is high (~0.65–0.75, target-proximate); this divergence should produce different types when computed per-seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: the need to preserve knowledge in the absence of material performance remains unresolved. However, the founding_problem_status × disappearance_verdict mismatch is the diagnostic: the founding problem is LIVE (knowledge-preservation is still necessary), but if the archive-maintenance reading disappeared (communities ceased studying sacrificial law), the question is whether the world would rearrange (messianic restoration would be impossible, alternative approaches would be needed) or remain unchanged (most of the world does not expect or prepare for messianic restoration anyway). The constraint serves a doctrine-bound community, not universal function. This is not classical mandatrophy (a function that has atrophied), but rather a prophetic commitment whose justification remains perpetually contingent. The theater_ratio rise indicates a drift from instrumental (archive) to symbolic (hope-maintenance) function—a precursor to mandatrophy if belief in restoration weakens further.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    messianic_contingency,
    'How does the probability and timeline of messianic restoration affect the rationality of the present study burden? If restoration is indefinitely deferred or probabilistically near-zero, is the present cost justified by the deferred benefit?',
    'This is fundamentally a question of faith and eschatology, not empirical fact. Resolution would require the observant community to explicitly assess the expected value of the knowledge (restoration probability × knowledge value) and declare whether present study remains justified under their own probability estimates.',
    'If communities assessed the expected value as negative or near-zero, the archive-maintenance reading would lose doctrinal justification and communities might shift to the study_as_performance reading (study justified by present fulfillment, not future benefit). This would reclassify the constraint from Scaffold to Rope (coordination without deferred contingency).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(messianic_contingency, preference, 'Whether the deferred messianic benefit justifies present study cost.').

omega_variable(
    knowledge_preservation_alternative_frame,
    'Is the preservation of technical knowledge a distinct obligation (requiring systematic study and elaboration) or merely a side effect of studying law for present use (where preservation happens incidentally, not as an end)?',
    'Compare the scholarly output (elaboration, commentary, detail) in periods where preservation was an explicit goal versus periods where it was not. If the detail level is similar regardless, preservation is incidental; if explicit-goal periods show significantly greater elaboration, preservation is a distinct driver.',
    'If preservation is incidental (not a distinct obligation), then the archive_maintenance reading is post-hoc justification for study that would occur anyway. This would support the study_as_performance reading (study fulfills the commandment directly, not through future-oriented preservation). The constraint would be reclassified to Rope, not Scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_preservation_alternative_frame, empirical, 'Whether knowledge preservation is an explicit separate obligation or incidental to other study goals.').

omega_variable(
    identity_locked_exit_costs,
    'To what degree does the identity-locked exit option reflect genuine religious identity (fundamental self-concept) versus socialization and institutional enforcement that could change if permitted?',
    'Natural experiment: survey communities that have relaxed the study obligation (e.g., communities that no longer require daily study of sacrifice law) and measure whether religious identity persists among those who reduced study. If identity remains robust despite reduced study, the exit was genuinely constrained by institutional requirement, not core identity.',
    'If identity-locking is more institutional than essential, the suppression metric understates the actual enforcement overhead. Actual suppression would be higher; the constraint would shift toward Snare. Conversely, if identity truly fuses with the obligation, suppression is correctly low because participants maintain it willingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_exit_costs, empirical, 'Degree of identity fusion versus institutional constraint in maintaining the study obligation.').

omega_variable(
    theater_ratio_causation,
    'Does the rise in theater_ratio from 0.45 to 0.62 reflect a genuine drift in function (from instrumental archive-maintenance to performative hope-affirmation) or merely the increasingly ritualized form of unchanged study practice?',
    'Analyze commentary and exegetical output: does later scholarship show more attention to the psychological/communal benefits of study versus the technical knowledge preserved? Do sources increasingly invoke messianic hope and faith in restoration rather than architectural or procedural accuracy?',
    'If the drift is genuine (function shifting to performance), the constraint is approaching Piton (maintained more for what it expresses than what it produces). If the rise is formal (study increasingly ceremonial but not functionally changed), the constraint remains Scaffold, temporarily justified by deferred restoration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_causation, empirical, 'Whether rising theater ratio reflects functional drift or formal elaboration.').

omega_variable(
    reading_coexistence_mechanism,
    'How do the three sibling readings coexist within contemporary rabbinical discourse? Do different schools hold them as non-negotiable doctrinal positions (full coexistence), or does each school consider the others heretical/invalid (attempts at foreclosure)?',
    'Survey contemporary halakhic literature: do authoritative sources acknowledge multiple valid readings of the sacrifice commandment, or does each source claim exclusive correctness?',
    'If readings genuinely coexist (different schools hold different readings without condemning others), the structure confirms coexists_with relations. If schools attempt mutual exclusion, the reading_relations should be revised to include forecloses edges where logical contradiction is claimed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_coexistence_mechanism, empirical, 'Whether the three sibling readings coexist peacefully or attempt mutual foreclosure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__archive_maintenance, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__archive_maintenance, theater_ratio, 0, 0.45).
narrative_ontology:measurement(sacr_tr_t250, sacrifice_commandment__archive_maintenance, theater_ratio, 250, 0.5).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_commandment__archive_maintenance, theater_ratio, 500, 0.55).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_commandment__archive_maintenance, theater_ratio, 1000, 0.6).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_commandment__archive_maintenance, theater_ratio, 1500, 0.61).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_commandment__archive_maintenance, theater_ratio, 2000, 0.62).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__archive_maintenance, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sacr_be_t250, sacrifice_commandment__archive_maintenance, base_extractiveness, 250, 0.4).
narrative_ontology:measurement(sacr_be_t500, sacrifice_commandment__archive_maintenance, base_extractiveness, 500, 0.42).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_commandment__archive_maintenance, base_extractiveness, 1000, 0.44).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_commandment__archive_maintenance, base_extractiveness, 1500, 0.45).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_commandment__archive_maintenance, base_extractiveness, 2000, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_commandment__archive_maintenance, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(sacr_su_t250, sacrifice_commandment__archive_maintenance, suppression_requirement, 250, 0.26).
narrative_ontology:measurement(sacr_su_t500, sacrifice_commandment__archive_maintenance, suppression_requirement, 500, 0.27).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_commandment__archive_maintenance, suppression_requirement, 1000, 0.28).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_commandment__archive_maintenance, suppression_requirement, 1500, 0.28).
narrative_ontology:measurement(sacr_su_t2000, sacrifice_commandment__archive_maintenance, suppression_requirement, 2000, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__archive_maintenance, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_commandment__archive_maintenance, 0.12).
narrative_ontology:affects_constraint(sacrifice_commandment__archive_maintenance, sacrifice_commandment__performance_only).
narrative_ontology:affects_constraint(sacrifice_commandment__archive_maintenance, sacrifice_commandment__study_as_performance).

% DUAL FORMULATION NOTE:
% The sacrifice_commandment kernel generates three distinct constraints, each a reading of how the commandment persists after Temple destruction. Archive_maintenance (this story) treats study as knowledge preservation for future restoration. Performance_only treats the commandment as suspended, not fulfilled. Study_as_performance treats study itself as the fulfillment. Each has different beneficiaries, different extraction profiles, and different temporal horizons. The three are linked as siblings competing within the same interpretive tradition. Archive_maintenance influences both others: it assumes knowledge can be preserved (relevant to performance_only, which must explain why restoration is possible), and it competes with study_as_performance for the legitimacy of ongoing study (if study fulfills the commandment in itself, preservation is unnecessary).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_commandment__archive_maintenance, organized, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
