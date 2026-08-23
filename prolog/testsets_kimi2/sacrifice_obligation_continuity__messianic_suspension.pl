% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__messianic_suspension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: sacrifice_obligation_continuity__messianic_suspension
 *   human_readable: Messianic Suspension of Sacrificial Obligation with Study as Readiness Maintenance
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   Following the destruction of the Second Temple in 70 CE, the rabbinic
 *   tradition developed a framework in which the biblical sacrificial
 *   commandments remain binding in principle but are suspended in practice
 *   pending messianic restoration. This reading rejects both archival
 *   preservation (which treats the law as defunct cultural memory) and
 *   study-as-performance (which treats textual engagement as fulfillment of
 *   the commandment). Instead, it institutes a maintenance protocol:
 *   intensive study of sacrificial tractates, preservation of priestly
 *   genealogies, and performance of related rituals (such as the Passover
 *   sacrifice in absence) as readiness exercises rather than fulfillment. The
 *   constraint extracts moderate communal resources to maintain an expertise
 *   infrastructure that has no current application, but imposes this burden
 *   without guiltâthose who do not study sacrifices are not sinners, and
 *   the community is not condemned for the Temple's absence. The
 *   classification as scaffold captures this transitional logic: the
 *   arrangement exists to carry the tradition across a gap, justified by the
 *   future transition rather than by the steady state of study.
 *
 * KEY AGENTS:
 *   - Rabbinic judiciary (institutional/agenda_setter): Sets halakhic parameters for what maintains readiness; benefits from institutional continuity.
 *   - Torah scholars (moderate/beneficiary): Bear the identity-locked burden of non-productive expertise; their lives are organized around the readiness protocol.
 *   - Kohanic lineage (moderate/beneficiary): Maintain hereditary priestly identity without current function; generational time horizon, identity-locked exit.
 *   - Observant communities (organized/beneficiary): Fund and host the infrastructure; bear diffuse costs of maintenance.
 *   - Secular biblical scholars (organized/excluded): Study the same texts without normative framework; excluded from the readiness discourse.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__messianic_suspension, 0.45).
domain_priors:suppression_score(sacrifice_obligation_continuity__messianic_suspension, 0.25).
domain_priors:theater_ratio(sacrifice_obligation_continuity__messianic_suspension, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, extractiveness, 0.45).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__messianic_suspension, scaffold).
narrative_ontology:human_readable(sacrifice_obligation_continuity__messianic_suspension, "Messianic Suspension of Sacrificial Obligation with Study as Readiness Maintenance").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__messianic_suspension, "religious_law/ritual_studies/textual_tradition").

narrative_ontology:has_sunset_clause(sacrifice_obligation_continuity__messianic_suspension).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__messianic_suspension, 'dfa2c570-f3f5-4c89-8871-489b966266de').
narrative_ontology:cs_kernel_codification('dfa2c570-f3f5-4c89-8871-489b966266de', fixed_text).
narrative_ontology:cs_authority_grounding('dfa2c570-f3f5-4c89-8871-489b966266de', lineage).
narrative_ontology:cs_interpretation_layer_present('dfa2c570-f3f5-4c89-8871-489b966266de').
narrative_ontology:cs_reading_relation('dfa2c570-f3f5-4c89-8871-489b966266de', sacrifice_obligation_continuity__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('dfa2c570-f3f5-4c89-8871-489b966266de', sacrifice_obligation_continuity__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('dfa2c570-f3f5-4c89-8871-489b966266de', sacrifice_obligation_continuity__archival_preservation, influences).
narrative_ontology:cs_axiom('dfa2c570-f3f5-4c89-8871-489b966266de', foundational, sacrificial_obligation_suspended_not_abrogated).
narrative_ontology:cs_axiom_status(sacrificial_obligation_suspended_not_abrogated, holdable).
narrative_ontology:cs_axiom_grounding('dfa2c570-f3f5-4c89-8871-489b966266de', sacrificial_obligation_suspended_not_abrogated, deontological).
narrative_ontology:cs_axiom('dfa2c570-f3f5-4c89-8871-489b966266de', foundational, study_of_sacrifice_is_readiness_not_fulfillment).
narrative_ontology:cs_axiom_status(study_of_sacrifice_is_readiness_not_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('dfa2c570-f3f5-4c89-8871-489b966266de', study_of_sacrifice_is_readiness_not_fulfillment, conventional).
narrative_ontology:cs_reference_frame('dfa2c570-f3f5-4c89-8871-489b966266de', second_temple_cult_operational).
narrative_ontology:cs_drift_state('dfa2c570-f3f5-4c89-8871-489b966266de', post_second_temple_destruction, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('dfa2c570-f3f5-4c89-8871-489b966266de', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, rabbinic_judiciary).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, torah_scholars).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, kohanic_lineage).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, observant_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the halakhic parameters for what counts as maintaining readiness for sacrificial restoration; adjudicates questions of priestly genealogy, purity law, and temple architecture through interpretive tradition. Derives institutional authority from continuity with the chain of transmission.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, rabbinic_judiciary, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__messianic_suspension, rabbinic_judiciary, beneficiary).

% Devote their lives to studying tractates of sacrificial law that have no current application; supported by communal funding and institutional stipends. Their personal and professional identity is fused with the maintenance of this non-productive expertise as a sacred duty.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, torah_scholars, beneficiary,
    moderate, biographical, identity_locked, national).

% Maintain genealogical records and hereditary identity as priests in waiting; preserve purity restrictions and tribal affiliation without current cultic function. The identity is constitutive of family status and marriageability regardless of individual choice.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, kohanic_lineage, beneficiary,
    moderate, generational, identity_locked, national).

% Fund academies and scholars through communal taxation and charitable obligations; participate in ritualized study cycles and liturgical reminders of sacrifices. Bear the diffuse economic and temporal cost of maintaining a readiness infrastructure that may never be activated in their lifetimes.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, observant_communities, beneficiary,
    organized, biographical, constrained, global).

% Study sacrificial texts as historical, literary, and archaeological artifacts. Their methodological naturalism and lack of normative commitment to rabbinic authority exclude them from the internal discourse about readiness and restoration.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, secular_biblical_scholars, excluded,
    organized, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_continuity__messianic_suspension, diffuse).
narrative_ontology:fixing_cost_class(sacrifice_obligation_continuity__messianic_suspension, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a specialized body of sacrificial law expertise, priestly genealogical identity, and ritual technical capacity across a period when the Temple cult is inoperative, so that restoration of the sacrificial site could proceed without generational knowledge loss.
% TRANSFER_FUNCTION: Moves communal economic resources and human lifetimes into the maintenance of non-productive expertise and identity markers; transfers normative authority to rabbinic interpreters who adjudicate what counts as adequate readiness.
% ABSENT_VOICES: Secular biblical scholars who read the texts as cultural memory rather than normative law; activist messianic movements who would advocate immediate physical restoration regardless of current halakhic consensus; voices arguing for permanent abrogation of the obligation.
% DISAPPEARANCE_RATIONALE: If the framework of messianic suspension vanished, the communal rationale for maintaining non-productive sacrificial expertise would collapse. The community would be forced into a choice between archival preservation, study-as-fulfillment, or activist violation of the status quo; the specialized knowledge would dissipate within a generation.
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE and the consequent cessation of sacrificial practice, which created the crisis of how to relate to a large corpus of divine commandments that suddenly became impossible to perform.
% FOUNDING_PROBLEM_CORROBORATION: The destruction of the Temple is corroborated by Roman historiography and archaeology independent of rabbinic claims. However, the specific legal framing of 'suspension awaiting restoration' as opposed to 'abrogation' or 'archival preservation' is attested primarily within the rabbinic tradition itself; no external corroboration selects suspension over the other readings.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__messianic_suspension, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__messianic_suspension, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__messianic_suspension, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__messianic_suspension, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__messianic_suspension, 0.45, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is moderate (0.45) because substantial communal resources are diverted to maintaining expertise with no current return. Suppression is low (0.25) because the obligation is explicitly suspendedâthere is no guilt for non-performance, only a diffuse social expectation that the community maintain capacity. Theater_ratio is moderate (0.32) and rising: as temporal distance from actual practice grows, the readiness exercises become increasingly performative (e.g., reading sacrificial passages as liturgical substitute rather than technical rehearsal). Accessibility_collapse is moderately high (0.65) because the rabbinic interpretive tradition has made alternatives such as permanent abrogation or study-as-fulfillment normatively unavailable within traditional communities. Resistance is low (0.15) because the suspension reading is broadly accepted as the pragmatic solution to the post-Temple crisis.
 *
 * PERSPECTIVAL GAP:
 *   The rabbinic judiciary and scholars experience the constraint as genuine continuity and sacred duty; the secular excluded observer sees it as resource allocation to a non-functional institution. The engine computes this divergence from the structural data: agenda-setters and beneficiaries with constrained or identity-locked exit face low effective extraction, while an analytical observer with arbitrage-grade exit would face near-zero extraction. The absence of a distinct victim set prevents the computed type from drifting toward snare or tangled_rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The four named beneficiary agents all sit near the symmetric-to-beneficiary end of directionality: the rabbinic judiciary and scholars benefit from institutional continuity and social role; the kohanim benefit from maintained hereditary status; the observant communities benefit from collective hope and identity. The readiness burden they bear is real but is treated by the engine as coordination overhead rather than asymmetric extraction because the structural declarations show no victim set. The excluded secular scholars are not targets of extractionâthey simply stand outside the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as scaffold prevents the mandatrophy error of reading a transitional maintenance protocol as either pure coordination (rope) or pure extraction (snare). The founding problemâthe Temple's destructionâis still live, so the scaffold has not yet become a piton, though the rising theater_ratio signals drift in that direction. If the founding problem were declared dead while the arrangement persisted, the mismatch flag would fire.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_vs_abrogation_status,
    'Is the sacrificial obligation genuinely suspended awaiting restoration, or permanently abrogated by historical circumstance?',
    'A universally recognized Sanhedrin or messianic restoration that reactivates practice would support suspension; definitive historical demonstration that the rabbinic category of suspension was a post-hoc reconstruction would support abrogation.',
    'Would convert the scaffold to a rope if restoration arrives, or to a piton or archival constraint if the wait proves permanent and the readiness function atrophies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suspension_vs_abrogation_status, conceptual, 'Whether the obligation is suspended or abrogated').

omega_variable(
    readiness_expertise_practicality,
    'Does the maintained expertise constitute reactivatable technical knowledge, or has it become a scholastic exercise irrelevant to actual sacrificial practice?',
    'Empirical assessment of whether scholars trained only in textual study could actually perform sacrifices without the lost oral and practical tradition.',
    'High theater ratio combined with low practical utility would shift classification toward piton; demonstrated utility would confirm the scaffold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(readiness_expertise_practicality, empirical, 'Whether readiness expertise is practically reactivatable').

omega_variable(
    committer_reading_boundary,
    'How would classification change if the study_as_performance reading were adopted as the operative framework?',
    'Comparative analysis of the sibling constraint story for study_as_performance.',
    'Would likely shift from scaffold (transitional maintenance) to tangled_rope or rope (active fulfillment), potentially creating a victim set if study becomes compulsive performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_reading_boundary, conceptual, 'Structural delta between messianic_suspension and study_as_performance').

omega_variable(
    suppression_internalization_ambiguity,
    'Is the readiness burden borne through free commitment or internalized obligation?',
    'Post-exit observation of individuals who leave observant communities: if the burden persists as guilt or identity loss, suppression is partially internalized.',
    'If internalized, effective extraction exceeds the structural suppression measure because the target carries the constraint after formal exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_ambiguity, empirical, 'Structural vs internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__messianic_suspension, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 500, 0.18).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1000, 0.24).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1500, 0.28).
narrative_ontology:measurement(sacr_tr_t1800, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1800, 0.3).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 2000, 0.32).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 500, 0.32).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1000, 0.38).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1500, 0.42).
narrative_ontology:measurement(sacr_be_t1800, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1800, 0.44).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 2000, 0.45).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_continuity__messianic_suspension, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__messianic_suspension, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the sacrifice_obligation_continuity kernel. It shares the same biblical kernel with study_as_performance, performance_only, and archival_preservation, but decomposes into a distinct constraint because its epsilon, beneficiary structure, and type differ structurally from its siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
