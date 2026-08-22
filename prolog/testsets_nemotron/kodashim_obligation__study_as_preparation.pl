% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_preparation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_preparation, []).

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
 *   constraint_id: kodashim_obligation__study_as_preparation
 *   human_readable: Kodashim Study as Preparation for Messianic Restoration
 *   domain: religious_studies/jewish_law/textual_preservation
 *
 * SUMMARY:
 *   Following the destruction of the Second Temple (70 CE), the sacrificial
 *   system of kodashim became technically unperformable. This reading holds
 *   that the law remains binding de jure but its performance is suspended de
 *   facto until messianic restoration. Study of the sacrificial codes
 *   (Mishnah/Tosefta Kodashim, relevant Talmudic tractates, Maimonides'
 *   Hilkhot Avodah) functions as instrumental preparation — preserving the
 *   technical knowledge (measurements, sequences, disqualifications, priestly
 *   genealogies) required to resume performance when the Temple is rebuilt.
 *   The current generation bears the cost of maintaining this knowledge
 *   infrastructure without receiving its cosmic benefit (deferred tikkun);
 *   the beneficiary is the messianic-era generation that will actually
 *   perform the restored rites. Temple restoration is structurally required
 *   as the sunset condition — without it, the preparation has no object.
 *
 * KEY AGENTS:
 *   - current_study_generation: Primary payer (moderate/constrained) — bears cognitive and opportunity costs of preserving technical knowledge for a restoration they will not witness
 *   - messianic_future_generation: Primary beneficiary (analytical/civilizational) — receives intact technical framework enabling immediate performance at restoration
 *   - rabbinic_authorities: Agenda setter (institutional/generational) — curate, transmit, and adjudicate the technical corpus; determine what counts as adequate preparation
 *   - temple_mount_activists: Secondary payer (organized/identity_locked) — invest political/social capital in restoration advocacy; exit blocked by identity fusion with the cause
 *   - secular_scholars: Observer (analytical/universal) — study the corpus as historical/philological data without accepting its binding status
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_preparation, 0.18).
domain_priors:suppression_score(kodashim_obligation__study_as_preparation, 0.25).
domain_priors:theater_ratio(kodashim_obligation__study_as_preparation, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, extractiveness, 0.18).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_preparation, scaffold).
narrative_ontology:human_readable(kodashim_obligation__study_as_preparation, "Kodashim Study as Preparation for Messianic Restoration").
narrative_ontology:topic_domain(kodashim_obligation__study_as_preparation, "religious_studies/jewish_law/textual_preservation").

narrative_ontology:has_sunset_clause(kodashim_obligation__study_as_preparation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_preparation, 'b9fb2ed5-c936-41c5-b67e-779db3610056').
narrative_ontology:cs_kernel_codification('b9fb2ed5-c936-41c5-b67e-779db3610056', fixed_text).
narrative_ontology:cs_authority_grounding('b9fb2ed5-c936-41c5-b67e-779db3610056', lineage).
narrative_ontology:cs_interpretation_layer_present('b9fb2ed5-c936-41c5-b67e-779db3610056').
narrative_ontology:cs_reading_relation('b9fb2ed5-c936-41c5-b67e-779db3610056', kodashim_obligation__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('b9fb2ed5-c936-41c5-b67e-779db3610056', kodashim_obligation__study_as_archive, coexists_with).
narrative_ontology:cs_axiom('b9fb2ed5-c936-41c5-b67e-779db3610056', foundational, sacrificial_law_binding_but_suspended).
narrative_ontology:cs_axiom_status(sacrificial_law_binding_but_suspended, holdable).
narrative_ontology:cs_axiom_grounding('b9fb2ed5-c936-41c5-b67e-779db3610056', sacrificial_law_binding_but_suspended, deontological).
narrative_ontology:cs_axiom('b9fb2ed5-c936-41c5-b67e-779db3610056', foundational, study_as_instrumental_preparation).
narrative_ontology:cs_axiom_status(study_as_instrumental_preparation, holdable).
narrative_ontology:cs_axiom_grounding('b9fb2ed5-c936-41c5-b67e-779db3610056', study_as_instrumental_preparation, conventional).
narrative_ontology:cs_axiom('b9fb2ed5-c936-41c5-b67e-779db3610056', foundational, temple_restoration_required_sunset).
narrative_ontology:cs_axiom_status(temple_restoration_required_sunset, holdable).
narrative_ontology:cs_axiom_grounding('b9fb2ed5-c936-41c5-b67e-779db3610056', temple_restoration_required_sunset, theological).
narrative_ontology:cs_reference_frame('b9fb2ed5-c936-41c5-b67e-779db3610056', post_churban_rabbinic_formation).
narrative_ontology:cs_drift_state('b9fb2ed5-c936-41c5-b67e-779db3610056', contemporary_long_exile, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b9fb2ed5-c936-41c5-b67e-779db3610056', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_preparation, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, messianic_future_generation).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_preparation, current_study_generation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, temple_mount_activists).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_preparation, temple_mount_activists).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_preparation, temple_restoration_necessity).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_preparation, binding_continuity_of_sacrificial_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Observant Jews who dedicate significant study time to kodashim tractates (Mishnah Kodashim, Zevachim, Menachot, Hullin, Bekhorot, Arakhin, Temurah, Keritot, Meilah, Tamid, Middot, Kinnim) without expectation of performing the rites in their lifetime. They bear the cognitive load of mastering complex technical details (measurements, sequences, disqualification chains) and the opportunity cost of study time directed toward a deferred cosmic function. Exit from the observant framework is possible but socially and identity-costly.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, current_study_generation, payer,
    moderate, biographical, constrained, global).

% The generation alive at messianic restoration who will inherit an intact technical corpus enabling immediate resumption of sacrificial performance. They receive the benefit of preserved knowledge without bearing the maintenance cost across the exile. Their situation is counterfactual from the current perspective — they are the teleological terminus of the preparation.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, messianic_future_generation, beneficiary,
    analytical, civilizational, analytical, universal).

% The rabbinic leadership (poskim, roshei yeshiva, kollel administrators) who curate the kodashim curriculum, determine which technical details are prioritized, adjudicate disputes about restoration requirements, and police the boundary between faithful preparation and unauthorized innovation (e.g., whether to prepare for a Third Temple with modified rites). They administer the constraint's transmission without directly bearing its study costs or receiving its messianic benefits.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, rabbinic_authorities, agenda_setter,
    institutional, generational, mobile, global).

% Activist groups (e.g., Temple Institute, Temple Mount Faithful) who invest political capital, fundraising, and personal risk in advocacy for Temple restoration and practical preparation (vessel manufacture, priestly training, red heifer research). Their self-concept is fused with the restoration narrative — exit would constitute identity dissolution. They bear disproportionate costs (social ostracism, legal jeopardy, physical danger) while also experiencing anticipatory benefit from 'advancing' the restoration.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, temple_mount_activists, payer,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(kodashim_obligation__study_as_preparation, temple_mount_activists, beneficiary).

% Academic scholars of rabbinic literature, Second Temple Judaism, and Jewish law who study kodashim texts as historical, philological, and legal data. They do not accept the binding status of the law or the messianic framework. Their exit is costless — they can engage or disengage from the corpus without identity or community penalty. They provide external corroboration for the founding problem narrative (R5).
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, secular_scholars, observer,
    analytical, civilizational, arbitrage, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_obligation__study_as_preparation, diffuse).
narrative_ontology:fixing_cost_class(kodashim_obligation__study_as_preparation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the technical knowledge (measurements, sequences, disqualifications, priestly requirements) necessary to resume sacrificial performance when the Temple is rebuilt, preventing knowledge loss across an indefinite exile.
% TRANSFER_FUNCTION: Moves study time, cognitive effort, and communal resources from the current generation (who maintain the corpus) to the messianic future generation (who will use it for actual performance). No material surplus is extracted — the transfer is intergenerational preparation cost.
% ABSENT_VOICES: Jews who reject the binding status of sacrificial law (Reform, secular, some Conservative positions) — they would argue the preparation is unnecessary because the law is either obsolete or metaphorically fulfilled. They are largely excluded from the observant frameworks where this constraint operates, though some remain within the community as dissenting voices.
% DISAPPEARANCE_RATIONALE: If the obligation to study kodashim as preparation vanished overnight, observant communities would reallocate massive study resources (thousands of kollel hours annually, curriculum space in yeshivas, publication effort) to other areas of Torah study. The technical corpus would degrade within a generation — priestly genealogies, altar measurements, disqualification chains would be lost. At restoration, the community would face a knowledge vacuum requiring revelation or reconstruction.
% FOUNDING_PROBLEM: After the Temple's destruction (70 CE), the sacrificial system became unperformable but the Torah's commandments regarding sacrifices remained textually binding. The founding problem: how to maintain the technical knowledge required for sacrificial performance across an indefinite exile until messianic restoration makes performance possible again.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the rabbinic tradition itself (Mishnah/Talmud formation as explicit response to Temple loss), by Maimonides' codification of sacrificial law in Mishneh Torah (Hilkhot Avodah) as binding halakha, and by secular scholars (e.g., Jacob Neusner, Isaiah Gafni) who document the rabbinic project of preserving sacrificial law post-70 as a historical fact — corroboration from outside the benefiting messianic future.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_preparation, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_preparation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_preparation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(kodashim_obligation__study_as_preparation, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_preparation, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_preparation_tests).
:- end_tests(kodashim_obligation__study_as_preparation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because study is framed as instrumental preparation, not extraction — the cost is real but bounded (dedicated study time, cognitive load) and the arrangement claims no surplus from the current generation. Suppression is low-moderate (0.25) — alternatives (abandoning kodashim study, reinterpreting it as non-binding) exist but are socially discouraged within observant communities; enforcement is communal norm pressure, not state coercion. Theater ratio is elevated (0.42) because the gap between preparation and performance grows with time — much study preserves details (e.g., precise altar measurements, specific disqualification chains) whose operational relevance is uncertain even at restoration. Accessibility collapse is moderate (0.35) — one can exit observant frameworks, but identity-locked agents experience high internal barriers. Resistance is low (0.15) — the constraint is internally maintained by the community that values it. The claimed type is scaffold: temporary support with a structural sunset (Temple restoration) that justifies the arrangement.
 *
 * PERSPECTIVAL GAP:
 *   From the current study generation's seat: real cognitive costs for a benefit they cannot receive — experienced as obligation with deferred payoff. From the messianic future's seat: pure benefit (intact technical corpus enabling immediate function). From the rabbinic authorities' seat: coordination maintenance — they administer the corpus, determine curricular priorities, and police the boundary between preparation and innovation. The engine computes these as different effective extractions from the same base ε via directionality derivation.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary: messianic_future_generation — receives the preserved technical framework at zero cost to themselves. Victim: current_study_generation — bears the maintenance cost (study time, cognitive load, opportunity cost) without receiving the cosmic repair the system is designed to produce. The rabbinic authorities are agenda_setters (institutional power, generational horizon) who curate what counts as adequate preparation. Temple Mount activists are identity_locked — their self-concept is fused with the restoration narrative, making exit psychologically prohibitive. Secular scholars are analytical observers with arbitrage-grade exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserving sacrificial technical knowledge across the exile until restoration) remains live per this reading — the Temple has not been rebuilt, so the preparation is still necessary. The mandate has not atrophied because the sunset condition is structural and unmet. However, the rising theater ratio over two millennia signals drift: preparation increasingly preserves details whose operational necessity is uncertain, and the coordination function (maintaining a community of practitioners ready to perform) weakens as the exile extends. This is not yet mandatrophy — the structural justification (future restoration) remains intact — but it is a scaffold approaching its structural limits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of a contested kernel (kodashim_obligation) rather than an independent constraint?',
    'Comparative structural analysis of the three declared readings: study_as_preparation, study_as_performance, study_as_archive. If they share a referent (the post-70 CE status of kodashim) but instantiate different beneficiary/victim structures and ε values, they are kernel readings.',
    'If confirmed as kernel readings, each must be authored as a separate constraint story with its own ε, stakeholders, and classification, linked via network.affects_constraints. The ε-invariance principle requires decomposition — a single story cannot capture observable-dependent classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three study orientations are structurally distinct constraints sharing a kernel').

omega_variable(
    restoration_probability,
    'What is the structural status of the sunset condition (Temple restoration) — is it a genuine future event with non-zero probability, or a theological postulate that functions as an unfalsifiable deferral?',
    'Assess whether the reading''s internal logic treats restoration as a contingent future event that could fail to occur (making the preparation potentially wasted) or as a guaranteed eschatological certainty (making the preparation''s instrumentality ceremonial).',
    'If restoration is treated as guaranteed, the scaffold''s sunset clause becomes performative — the arrangement persists without genuine transitional justification, drifting toward piton. If contingent, the preparation bears real option value and the scaffold classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_probability, conceptual, 'Whether the messianic restoration sunset is a genuine contingent trigger or an unfalsifiable deferral').

omega_variable(
    technical_knowledge_decay,
    'Does the technical knowledge being preserved (measurements, sequences, disqualifications) have determinate content that can be verified at restoration, or does it require living practice to maintain operational meaning?',
    'Compare the preserved textual corpus against the requirements of actual performance: are all variables specified (e.g., exact altar dimensions, valid priestly lineages, disqualification boundaries), or are there gaps that only living practice could fill?',
    'If the corpus has critical gaps requiring living practice, the preparation function is partially fictive — the scaffold preserves a form without its operational substance, increasing theater_ratio. If complete, the preparation is genuinely instrumental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_knowledge_decay, empirical, 'Whether the preserved technical corpus is operationally complete for restoration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_preparation, 70, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t70, kodashim_obligation__study_as_preparation, theater_ratio, 70, 0.15).
narrative_ontology:measurement(koda_tr_t500, kodashim_obligation__study_as_preparation, theater_ratio, 500, 0.25).
narrative_ontology:measurement(koda_tr_t1000, kodashim_obligation__study_as_preparation, theater_ratio, 1000, 0.35).
narrative_ontology:measurement(koda_tr_t1500, kodashim_obligation__study_as_preparation, theater_ratio, 1500, 0.4).
narrative_ontology:measurement(koda_tr_t1800, kodashim_obligation__study_as_preparation, theater_ratio, 1800, 0.42).
narrative_ontology:measurement(koda_tr_t2024, kodashim_obligation__study_as_preparation, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(koda_be_t70, kodashim_obligation__study_as_preparation, base_extractiveness, 70, 0.35).
narrative_ontology:measurement(koda_be_t500, kodashim_obligation__study_as_preparation, base_extractiveness, 500, 0.28).
narrative_ontology:measurement(koda_be_t1000, kodashim_obligation__study_as_preparation, base_extractiveness, 1000, 0.22).
narrative_ontology:measurement(koda_be_t1500, kodashim_obligation__study_as_preparation, base_extractiveness, 1500, 0.19).
narrative_ontology:measurement(koda_be_t1800, kodashim_obligation__study_as_preparation, base_extractiveness, 1800, 0.18).
narrative_ontology:measurement(koda_be_t2024, kodashim_obligation__study_as_preparation, base_extractiveness, 2024, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t70, kodashim_obligation__study_as_preparation, suppression_requirement, 70, 0.45).
narrative_ontology:measurement(koda_su_t500, kodashim_obligation__study_as_preparation, suppression_requirement, 500, 0.35).
narrative_ontology:measurement(koda_su_t1000, kodashim_obligation__study_as_preparation, suppression_requirement, 1000, 0.3).
narrative_ontology:measurement(koda_su_t1500, kodashim_obligation__study_as_preparation, suppression_requirement, 1500, 0.25).
narrative_ontology:measurement(koda_su_t1800, kodashim_obligation__study_as_preparation, suppression_requirement, 1800, 0.25).
narrative_ontology:measurement(koda_su_t2024, kodashim_obligation__study_as_preparation, suppression_requirement, 2024, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_preparation, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_obligation__study_as_preparation, 0.08).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_preparation, kodashim_obligation__study_as_performance).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_preparation, kodashim_obligation__study_as_archive).

% DUAL FORMULATION NOTE:
% This is the study_as_preparation reading of the kodashim_obligation kernel. The three readings decompose the single colloquial label 'kodashim study' into structurally distinct constraints: preparation (this reading, ε=0.18, scaffold), performance (ε higher, tangled_rope — study enacts cosmic function now), archive (ε≈0, mountain — historical preservation without binding force). They share the referent (post-70 kodashim status) but differ in beneficiary structure, victim set, and sunset logic.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
