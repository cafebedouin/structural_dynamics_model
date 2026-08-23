% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__study_as_performance, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: kodashim_commandment_status__study_as_performance
 *   human_readable: Study of Sacrifice Laws as Commandment Fulfillment (Torah Study Reading)
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   This constraint story instantiates the study_as_performance reading of
 *   the contested kernel kodashim_commandment_status. In this reading, the
 *   study of Talmudic tractates concerning sacrificial law (Kodashim) is not
 *   a temporary placeholder or a deferred hope but a fully valid fulfillment
 *   of the commandments themselves. The kernel is occupied through
 *   intellectual engagement, and no performance gap exists. The reading
 *   competes with performance_only (commandment suspended without altar) and
 *   messianic_deferral (commandment latent until restoration).
 *
 * KEY AGENTS:
 *   - halakhic_community (beneficiary/organized): Maintains obligations via study without Temple infrastructure.
 *   - rabbinic_interpreters (agenda_setter/institutional): Transmit and certify the study framework.
 *   - temple_advocates (excluded/moderate): Hold the performance-only view and are outside the consensus.
 *   - religious_studies_scholars (observer/analytical): Track the mechanism as a coordination solution.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__study_as_performance, 0.02).
domain_priors:suppression_score(kodashim_commandment_status__study_as_performance, 0.05).
domain_priors:theater_ratio(kodashim_commandment_status__study_as_performance, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, extractiveness, 0.02).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__study_as_performance, rope).
narrative_ontology:human_readable(kodashim_commandment_status__study_as_performance, "Study of Sacrifice Laws as Commandment Fulfillment (Torah Study Reading)").
narrative_ontology:topic_domain(kodashim_commandment_status__study_as_performance, "religious/halakhic").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__study_as_performance, '24847287-868e-463f-b1f9-81176ebbf546').
narrative_ontology:cs_kernel_codification('24847287-868e-463f-b1f9-81176ebbf546', fixed_text).
narrative_ontology:cs_authority_grounding('24847287-868e-463f-b1f9-81176ebbf546', lineage).
narrative_ontology:cs_interpretation_layer_present('24847287-868e-463f-b1f9-81176ebbf546').
narrative_ontology:cs_reading_relation('24847287-868e-463f-b1f9-81176ebbf546', kodashim_commandment_status__performance_only, forecloses).
narrative_ontology:cs_reading_relation('24847287-868e-463f-b1f9-81176ebbf546', kodashim_commandment_status__messianic_deferral, influences).
narrative_ontology:cs_axiom('24847287-868e-463f-b1f9-81176ebbf546', foundational, study_fulfillment_equivalence).
narrative_ontology:cs_axiom_status(study_fulfillment_equivalence, holdable).
narrative_ontology:cs_axiom_grounding('24847287-868e-463f-b1f9-81176ebbf546', study_fulfillment_equivalence, deontological).
narrative_ontology:cs_axiom('24847287-868e-463f-b1f9-81176ebbf546', foundational, commandment_perpetual_incumbency).
narrative_ontology:cs_axiom_status(commandment_perpetual_incumbency, holdable).
narrative_ontology:cs_axiom_grounding('24847287-868e-463f-b1f9-81176ebbf546', commandment_perpetual_incumbency, deontological).
narrative_ontology:cs_reference_frame('24847287-868e-463f-b1f9-81176ebbf546', covenantal_study_continuity).
narrative_ontology:cs_drift_state('24847287-868e-463f-b1f9-81176ebbf546', post_temple_diaspora, gap(stable, minor, true)).
narrative_ontology:cs_created_at('24847287-868e-463f-b1f9-81176ebbf546', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__study_as_performance, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, halakhic_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains covenantal continuity by fulfilling Temple-era commandments through Torah study in the absence of the sacrificial altar. Benefits from a framework that preserves ritual obligation without requiring institutional conditions that do not currently exist.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, halakhic_community, beneficiary,
    organized, generational, constrained, global).

% Transmits and adjudicates the interpretive tradition that equates advanced study of Kodashim with sacrificial performance. Sets curriculum and certifies expertise in the relevant tractates. Does not extract material benefit from the arrangement; maintains authority through textual fidelity and lineage.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, rabbinic_interpreters, agenda_setter,
    institutional, generational, constrained, global).

% Maintain that sacrifice laws are inoperable without a physical altar and view the study-as-performance reading as displacement rather than true fulfillment. Their voice appears in theological debate but is structurally excluded from normative halakhic consensus in communities where the study reading dominates.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, temple_advocates, excluded,
    moderate, generational, constrained, global).

% Analyze the halakhic mechanism by which textual study substitutes for ritual performance across religions. Neither benefit from nor bear costs of the constraint; track its stability as a coordination solution to cultic discontinuity.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, religious_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains covenantal continuity and collective religious obligation in the absence of the Jerusalem Temple by providing a decentralized, intellectually-based mechanism for fulfilling sacrificial commandments without centralized priestly infrastructure.
% TRANSFER_FUNCTION: Moves religious obligation from the altar-performance register to the intellectual-study register, distributing the commandment's weight across the learned community rather than concentrating it in a priestly caste or suspending it entirely.
% ABSENT_VOICES: Temple-centric and priestly-lineage groups who hold that sacrifice requires physical altar and who are structurally absent from the rabbinic normative framework that validates study-as-fulfillment.
% DISAPPEARANCE_RATIONALE: If study no longer counted as fulfillment, the observant community would face a massive unfulfilled commandment burden or would need to adopt alternative frameworks such as messianic deferral or performance suspension, significantly altering rabbinic curriculum and daily religious life.
% FOUNDING_PROBLEM: The destruction of the Second Temple eliminated the physical and institutional infrastructure for biblical sacrifice, threatening the continuity of a major class of commandments and the covenantal framework that depended on them.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by historians of religion and Jewish history outside the rabbinic beneficiary set; the destruction of 70 CE is a documented historical fact, and the problem of cultic continuity is widely recognized in academic religious studies.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__study_as_performance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_commandment_status__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__study_as_performance, 0.02, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__study_as_performance_tests).
:- end_tests(kodashim_commandment_status__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.02) because the arrangement moves obligation to study without extracting surplus from any party; suppression is minimal (0.05) because the constraint operates through normative continuity rather than coercion; theater_ratio is low (0.08) because study is functional fulfillment, not performative maintenance of a dead form. Accessibility_collapse is moderate-low (0.25): within this reading's framework, alternatives such as performance-only become inaccessible, but the broader field of Jewish law retains multiple live positions. Resistance is negligible (0.05) because the reading commands broad consensus where it dominates.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (halakhic community) and the agenda_setter seat (rabbinic interpreters) converge in type because there is no asymmetric extraction to differentiate them; both experience the constraint as coordination. The excluded seat (temple_advocates) experiences the same structure as inadequate or false, but because they are not structurally targeted for extraction, their opposition registers as dissent rather than victimization. The engine will compute a narrow gap between seated types due to the absence of victims and the uniformity of low directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   The halakhic community is declared beneficiary, yielding a low directionality value and negligible effective extraction. No victim group is declared because the structural delta specifies zero extraction and no harmed parties. The rabbinic interpreters sit near the agenda_setter position with no extraction to capture, so their derived directionality also sits near the beneficiary end. Without a payer population, the engine cannot generate a high-chi seat; the computed classification should remain rope from all occupied seats.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids piton classification because its coordination function is live rather than atrophied: the study of Kodashim remains a central curriculum element, and the commandment is treated as currently operative. It avoids snare classification because there is no identifiable victim set and no suppression of alternatives for extractive ends. It avoids mountain classification because the constraint is a constructed rabbinic interpretation, not a natural law. The metrics and structural data are authored independently of the claimed rope type; if the engine computed a different classification, that divergence would signal either hidden extraction or overestimation of coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_performance_ontology,
    'Does intellectual engagement with sacrifice laws achieve the same covenantal effect as blood-offering, or is it a compensatory mechanism that leaves the original form unfulfilled?',
    'Comparative halakhic analysis of responsa and meta-halakhic theory examining whether study is ontologically equivalent or rabbinically authorized substitution.',
    'If compensatory rather than equivalent, base_extractiveness rises because the constraint leaves a performative gap that study does not fully close.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_performance_ontology, conceptual, 'Ontological status of study relative to sacrificial performance').

omega_variable(
    communal_enforcement_nature,
    'Is adherence to this reading enforced through communal normative pressure or purely through individual theological conviction?',
    'Ethnographic study of halakhic communities measuring social sanctions versus voluntary adherence.',
    'If communal pressure is significant, suppression is higher than the authored scalar suggests; if purely individual, the constraint operates closer to pure information-standard coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(communal_enforcement_nature, empirical, 'Structural versus internalized enforcement mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__study_as_performance, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__study_as_performance, theater_ratio, 0, 0.05).
narrative_ontology:measurement(koda_tr_t500, kodashim_commandment_status__study_as_performance, theater_ratio, 500, 0.06).
narrative_ontology:measurement(koda_tr_t1000, kodashim_commandment_status__study_as_performance, theater_ratio, 1000, 0.07).
narrative_ontology:measurement(koda_tr_t1500, kodashim_commandment_status__study_as_performance, theater_ratio, 1500, 0.08).
narrative_ontology:measurement(koda_tr_t2000, kodashim_commandment_status__study_as_performance, theater_ratio, 2000, 0.08).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__study_as_performance, base_extractiveness, 0, 0.01).
narrative_ontology:measurement(koda_be_t500, kodashim_commandment_status__study_as_performance, base_extractiveness, 500, 0.02).
narrative_ontology:measurement(koda_be_t1000, kodashim_commandment_status__study_as_performance, base_extractiveness, 1000, 0.02).
narrative_ontology:measurement(koda_be_t1500, kodashim_commandment_status__study_as_performance, base_extractiveness, 1500, 0.02).
narrative_ontology:measurement(koda_be_t2000, kodashim_commandment_status__study_as_performance, base_extractiveness, 2000, 0.02).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_commandment_status__study_as_performance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, kodashim_commandment_status__performance_only).
narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, kodashim_commandment_status__messianic_deferral).

% DUAL FORMULATION NOTE:
% The kernel kodashim_commandment_status decomposes into three structurally distinct constraints (study_as_performance, performance_only, messianic_deferral) because each reading assigns a different epsilon, a different beneficiary/victim structure, and a different persistence condition to the same biblical commandments. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
