% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__performance_only, []).

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
 *   constraint_id: sacrifice_commandment__performance_only
 *   human_readable: Sacrifice Commandment â Performance-Only Reading
 *   domain: religious_studies/halakhic_theory
 *
 * SUMMARY:
 *   This constraint instantiates the performance_only reading of the biblical
 *   sacrifice commandment kernel: physical execution in the Temple is
 *   strictly required, and without the Temple the commandment is suspended
 *   and unfulfilled. Structurally, this reading has generated 1,900 years of
 *   intensive rabbinic study of sacrificial law despite its own logic that
 *   such study does not discharge the divine obligation. The scholarly labor
 *   directed at this unperformable corpus diverts cognitive and institutional
 *   resources from applied ('living') halakha, constituting an extractive
 *   loop in which the rabbinic establishment maintains authority through
 *   mastery of a textually dense but practically null domain. The authored
 *   claim is snare; the metrics are authored independently to describe this
 *   extraction.
 *
 * KEY AGENTS:
 *   - rabbinic_establishment (institutional/arbitrage): agenda-setter and beneficiaryâcontrols curriculum and captures prestige
 *   - talmudic_students (moderate/identity_locked): primary targetâscholarly labor diverted to unperformable acts
 *   - contemporary_halakha_seekers (powerless/constrained): secondary targetâapplied law neglected
 *   - temple_restoration_advocates (organized/constrained): excluded voiceâwould end the suspension
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__performance_only, 0.88).
domain_priors:suppression_score(sacrifice_commandment__performance_only, 0.72).
domain_priors:theater_ratio(sacrifice_commandment__performance_only, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, extractiveness, 0.88).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, theater_ratio, 0.8).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__performance_only, snare).
narrative_ontology:human_readable(sacrifice_commandment__performance_only, "Sacrifice Commandment â Performance-Only Reading").
narrative_ontology:topic_domain(sacrifice_commandment__performance_only, "religious_studies/halakhic_theory").

domain_priors:requires_active_enforcement(sacrifice_commandment__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__performance_only, '6fba9f79-dd59-44c4-95c9-05c708b6ed32').
narrative_ontology:cs_kernel_codification('6fba9f79-dd59-44c4-95c9-05c708b6ed32', fixed_text).
narrative_ontology:cs_authority_grounding('6fba9f79-dd59-44c4-95c9-05c708b6ed32', lineage).
narrative_ontology:cs_interpretation_layer_present('6fba9f79-dd59-44c4-95c9-05c708b6ed32').
narrative_ontology:cs_reading_relation('6fba9f79-dd59-44c4-95c9-05c708b6ed32', sacrifice_commandment__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('6fba9f79-dd59-44c4-95c9-05c708b6ed32', sacrifice_commandment__archive_maintenance, coexists_with).
narrative_ontology:cs_axiom('6fba9f79-dd59-44c4-95c9-05c708b6ed32', foundational, physical_execution_strictly_required).
narrative_ontology:cs_axiom_status(physical_execution_strictly_required, holdable).
narrative_ontology:cs_axiom_grounding('6fba9f79-dd59-44c4-95c9-05c708b6ed32', physical_execution_strictly_required, deontological).
narrative_ontology:cs_axiom('6fba9f79-dd59-44c4-95c9-05c708b6ed32', foundational, study_does_not_fulfill_sacrificial_commandment).
narrative_ontology:cs_axiom_status(study_does_not_fulfill_sacrificial_commandment, holdable).
narrative_ontology:cs_axiom_grounding('6fba9f79-dd59-44c4-95c9-05c708b6ed32', study_does_not_fulfill_sacrificial_commandment, deontological).
narrative_ontology:cs_reference_frame('6fba9f79-dd59-44c4-95c9-05c708b6ed32', temple_centric_halakhic_order).
narrative_ontology:cs_drift_state('6fba9f79-dd59-44c4-95c9-05c708b6ed32', contemporary_diaspora_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('6fba9f79-dd59-44c4-95c9-05c708b6ed32', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__performance_only, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, rabbinic_establishment).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, talmudic_students).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, contemporary_halakha_seekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls halakhic curriculum, ordination, and institutional prestige. Determines that sacrificial law remains a central object of intensive study despite the Temple's absence. Derives authority and institutional continuity from mastery of this esoteric, textually dense corpus.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, rabbinic_establishment, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_commandment__performance_only, rabbinic_establishment, beneficiary).

% Direct scholarly labor toward sacrificial tractates and temple architecture under the performance_only reading that defines these acts as unperformable and non-fulfilling. Their identity and social standing are fused with mastery of this tradition; exit means abandoning the prestige track of rabbinic study.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, talmudic_students, payer,
    moderate, biographical, identity_locked, global).

% Require responsive halakhic guidance on family law, economics, and technology, but find the scholarly class's attention and prestige diverted toward temple-sacrifice theory. Their practical concerns are institutionally deprioritized relative to the esoteric corpus.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, contemporary_halakha_seekers, payer,
    powerless, immediate, constrained, global).

% Advocate for steps toward rebuilding the Temple, which would resolve the suspension and end the extractive study loop. Their political and theological position is marginalized by the scholarly consensus that treats sacrificial law as a purely textual object.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, temple_restoration_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_commandment__performance_only, rabbinic_establishment).
narrative_ontology:fixing_cost_class(sacrifice_commandment__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under the performance_only reading, the sacrifice commandment itself is suspended and has no active coordination function. The surrounding institutional arrangement coordinates communal identity and rabbinic textual continuity, but does so around a corpus defined as unperformable.
% TRANSFER_FUNCTION: Moves scholarly labor, curricular time, and institutional prestige from applied contemporary halakha to the study of unperformable sacrificial rituals; transfers authority and status to the rabbinic class that commands this esoteric knowledge.
% ABSENT_VOICES: Contemporary halakha seekers, scholars advocating applied ethical Torah, and Temple restoration movements are structurally excluded from curriculum-setting and halakhic agenda-setting.
% DISAPPEARANCE_RATIONALE: If the performance_only reading disappearedâwhether through Temple restoration or rabbinic abandonment of the readingâscholarly labor would reallocate to living law, collapsing the authority economy built on esoteric mastery of suspended practice and fundamentally reorganizing rabbinic institutions.
% FOUNDING_PROBLEM: The destruction of the Second Temple created a crisis of biblical commandment observance, requiring a halakhic framework for handling the inability to perform sacrifice.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians of religion and critical scholars attest the Temple has been absent for 1,900 years; modern Orthodox and feminist halakhic voices attest the suspension has ossified into permanent institutional extraction. The rabbinic establishment formally maintains the problem is live, but no external corroboration supports the claim that this 1,900-year suspension should continue to dominate curricular allocation.
narrative_ontology:disappearance_verdict(sacrifice_commandment__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__performance_only, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__performance_only, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_commandment__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__performance_only, 0.88, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_commandment__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_commandment__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.88) reflects 1,900 years of obligatory study labor directed at a domain the reading itself defines as unperformable. Suppression (0.72) is curricular and cognitive: yeshiva gatekeeping and identity-lock prevent redirection of attention. Theater ratio (0.80) is high because the study functions as a virtuoso performance of piety that substitutes for the commandment's actual performance. Accessibility_collapse (0.65) is moderate because applied halakha exists as an alternative but is institutionally devalued. Resistance (0.35) reflects modernist, feminist, and applied-halakha movements that contest the allocation. The measurement series share a single time grid to prevent misaligned substitution.
 *
 * PERSPECTIVAL GAP:
 *   The rabbinic establishment seat experiences the arrangement as sacred continuity and textual fidelity; the talmudic student and lay seeker seats experience it as a diversion of intellectual resources from pressing contemporary problems. The engine computes this divergence from structural dataâthe authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinic_establishment is a declared beneficiary with arbitrage-grade exit and institutional power, placing d near the beneficiary end. Talmudic_students are declared victims with identity_locked exit and moderate power, placing d near the target end. Contemporary_halakha_seekers are victims with constrained exit and low power, also near the target end. The effective extraction is amplified for the target seats and damped for the beneficiary seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâTemple destruction and the suspension of sacrificeâis dead by any empirical measure after 1,900 years. The performance_only reading prevents mandatrophy resolution by defining the suspended corpus as eternally central to rabbinic study. Were the reading abandoned, the constraint would either resolve into archive_maintenance (if study is preparation) or dissolve entirely (if the commandment is simply suspended), reallocating scholarly labor to living law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the curricular enforcement structural (yeshiva gatekeeping, economic incentives) or internalized (students believe sacrificial study is the highest form of Torah)?',
    'Survey of student motivations and post-exit trajectories; if students leaving yeshiva stop valuing sacrificial study, suppression was internalized.',
    'If internalized, effective extraction exceeds structural measures; the constraint travels with the agent after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression in Torah study allocation').

omega_variable(
    study_labor_coordination_or_extraction,
    'Does the study of sacrificial law under the performance_only reading serve a genuine coordination function (communal identity continuity), or is it pure extraction of cognitive labor?',
    'Comparative analysis of communities that de-emphasize sacrificial study versus those that maintain it; do identity markers persist or degrade?',
    'If identity persists without intensive sacrificial study, the coordination function was cover and classification shifts toward snare; if identity fragments, the study was genuinely coordinating.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(study_labor_coordination_or_extraction, conceptual, 'Whether sacrificial study under performance-only coordinates identity or extracts labor').

omega_variable(
    temple_destruction_as_natural_or_constructed_boundary,
    'Is the Temple absence a brute historical fact that naturally suspends the commandment (mountain-like), or a constructed boundary that the scholarly class uses to maintain the study-extraction system?',
    'Counterfactual analysis of scholarly attention allocation if Temple restoration were politically achievable; does the establishment resist or welcome such a shift?',
    'If the establishment resists restoration, the suspension is functionally a constructed snare; if it welcomes it, the constraint is closer to a scaffold awaiting conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temple_destruction_as_natural_or_constructed_boundary, empirical, 'Whether the Temple absence functions as natural limit or constructed extraction mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__performance_only, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__performance_only, theater_ratio, 0, 0.05).
narrative_ontology:measurement(sacr_tr_t300, sacrifice_commandment__performance_only, theater_ratio, 300, 0.15).
narrative_ontology:measurement(sacr_tr_t600, sacrifice_commandment__performance_only, theater_ratio, 600, 0.3).
narrative_ontology:measurement(sacr_tr_t900, sacrifice_commandment__performance_only, theater_ratio, 900, 0.45).
narrative_ontology:measurement(sacr_tr_t1200, sacrifice_commandment__performance_only, theater_ratio, 1200, 0.58).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_commandment__performance_only, theater_ratio, 1500, 0.7).
narrative_ontology:measurement(sacr_tr_t1800, sacrifice_commandment__performance_only, theater_ratio, 1800, 0.76).
narrative_ontology:measurement(sacr_tr_t1950, sacrifice_commandment__performance_only, theater_ratio, 1950, 0.8).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__performance_only, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(sacr_be_t300, sacrifice_commandment__performance_only, base_extractiveness, 300, 0.22).
narrative_ontology:measurement(sacr_be_t600, sacrifice_commandment__performance_only, base_extractiveness, 600, 0.38).
narrative_ontology:measurement(sacr_be_t900, sacrifice_commandment__performance_only, base_extractiveness, 900, 0.52).
narrative_ontology:measurement(sacr_be_t1200, sacrifice_commandment__performance_only, base_extractiveness, 1200, 0.65).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_commandment__performance_only, base_extractiveness, 1500, 0.75).
narrative_ontology:measurement(sacr_be_t1800, sacrifice_commandment__performance_only, base_extractiveness, 1800, 0.84).
narrative_ontology:measurement(sacr_be_t1950, sacrifice_commandment__performance_only, base_extractiveness, 1950, 0.88).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_commandment__performance_only, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__performance_only, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, sacrifice_commandment__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, sacrifice_commandment__archive_maintenance).

% DUAL FORMULATION NOTE:
% The sacrifice_commandment kernel decomposes into three structurally distinct constraints. The performance_only reading generates high extraction because it channels labor toward unperformable acts without claiming study as fulfillment. The study_as_performance reading would lower extraction by making study itself the commanded act. The archive_maintenance reading would reframe study as coordination for future restoration. Each reading warrants its own epsilon and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
