% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_preparation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   domain: religious/jewish_law/textual_preservation
 *
 * SUMMARY:
 *   This constraint instantiates the study_as_preparation reading of the
 *   kodashim_obligation kernel: the claim that biblical sacrificial law
 *   remains binding despite the Temple's destruction, and that intensive
 *   study of these laws functions as instrumental preservation of technical
 *   knowledge for messianic restoration. The reading stands in contest with
 *   study_as_archive (historical preservation only) and study_as_performance
 *   (study enacts cosmic function directly). The arrangement extracts modest
 *   but real cognitive resources from the current generationâwho study
 *   procedures they cannot performâand transfers the preserved capacity to
 *   a non-present messianic beneficiary. The low extractiveness reflects the
 *   absence of a present rent-collector; the scaffolding logic reflects the
 *   arrangement's explicit teleology of transition back to performance.
 *
 * KEY AGENTS:
 *   - current_generation_jews: Primary payer â bears the study obligation and deferred cosmic repair under conditions of ritual impossibility.
 *   - rabbinic_transmitters: Agenda setter â maintains curriculum and textual continuity without direct material capture.
 *   - messianic_future_community: Intended beneficiary â non-present recipient of preserved technical knowledge upon restoration.
 *   - secular_historians: Excluded voice â reads the texts as archival, not binding preparation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_preparation, 0.25).
domain_priors:suppression_score(kodashim_obligation__study_as_preparation, 0.35).
domain_priors:theater_ratio(kodashim_obligation__study_as_preparation, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, extractiveness, 0.25).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_preparation, scaffold).
narrative_ontology:human_readable(kodashim_obligation__study_as_preparation, "Kodashim Study as Preparation for Messianic Restoration").
narrative_ontology:topic_domain(kodashim_obligation__study_as_preparation, "religious/jewish_law/textual_preservation").

domain_priors:requires_active_enforcement(kodashim_obligation__study_as_preparation).
narrative_ontology:has_sunset_clause(kodashim_obligation__study_as_preparation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_preparation, '4040c4f2-165b-454c-ba2c-7a2a09e21f43').
narrative_ontology:cs_kernel_codification('4040c4f2-165b-454c-ba2c-7a2a09e21f43', fixed_text).
narrative_ontology:cs_authority_grounding('4040c4f2-165b-454c-ba2c-7a2a09e21f43', lineage).
narrative_ontology:cs_interpretation_layer_present('4040c4f2-165b-454c-ba2c-7a2a09e21f43').
narrative_ontology:cs_reading_relation('4040c4f2-165b-454c-ba2c-7a2a09e21f43', kodashim_obligation__study_as_archive, forecloses).
narrative_ontology:cs_reading_relation('4040c4f2-165b-454c-ba2c-7a2a09e21f43', kodashim_obligation__study_as_performance, coexists_with).
narrative_ontology:cs_axiom('4040c4f2-165b-454c-ba2c-7a2a09e21f43', foundational, torah_binding_across_temple_absence).
narrative_ontology:cs_axiom_status(torah_binding_across_temple_absence, holdable).
narrative_ontology:cs_axiom_grounding('4040c4f2-165b-454c-ba2c-7a2a09e21f43', torah_binding_across_temple_absence, deontological).
narrative_ontology:cs_axiom('4040c4f2-165b-454c-ba2c-7a2a09e21f43', foundational, restoration_requires_technical_preservation).
narrative_ontology:cs_axiom_status(restoration_requires_technical_preservation, holdable).
narrative_ontology:cs_axiom_grounding('4040c4f2-165b-454c-ba2c-7a2a09e21f43', restoration_requires_technical_preservation, instrumental).
narrative_ontology:cs_reference_frame('4040c4f2-165b-454c-ba2c-7a2a09e21f43', restored_temple_torah_obligation).
narrative_ontology:cs_drift_state('4040c4f2-165b-454c-ba2c-7a2a09e21f43', post_second_temple_diaspora, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('4040c4f2-165b-454c-ba2c-7a2a09e21f43', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_preparation, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, messianic_future_community).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_preparation, current_generation_jews).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bound by halakhic obligation to study the laws of Kodashim despite the Temple's destruction; bears the cognitive and temporal cost of mastering complex sacrificial procedures that cannot be performed; exit is blocked by religious identity fusion and the communal expectation that Torah study encompasses even presently inapplicable domains.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, current_generation_jews, payer,
    organized, generational, identity_locked, global).

% Determines the curriculum and interprets the scope of Torah study obligations; maintains textual continuity of sacrificial law across the Temple-less period; authority depends on preserving this knowledge but no direct material extraction occurs from the arrangement itself.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, rabbinic_transmitters, agenda_setter,
    institutional, civilizational, constrained, national).

% The future Jewish community in the messianic era; receives the preserved technical knowledge necessary to restore legitimate sacrificial worship; not a present actor and cannot influence current arrangements or decline the inherited preparation.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, messianic_future_community, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(kodashim_obligation__study_as_preparation, messianic_future_community).

% Would argue that Kodashim study is historical preservation of a defunct cultic system rather than binding preparation for restoration; excluded from halakhic discourse that authorizes and enforces the obligation.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, secular_historians, excluded,
    analytical, generational, analytical, global).

% Observes the intergenerational knowledge transfer and curriculum enforcement from outside the theological framework; documents how the preparation reading maintains textual continuity under conditions of ritual impossibility.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, academic_religious_studies, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_obligation__study_as_preparation, diffuse).
narrative_ontology:fixing_cost_class(kodashim_obligation__study_as_preparation, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves technical sacrificial knowledge across the intergenerational gap created by Temple destruction, ensuring that the capacity for legitimate restoration exists when historical conditions permit performance to resume.
% TRANSFER_FUNCTION: Moves cognitive labor, memorization, and study time from the current generation into preserved textual expertise and trained rabbinic memory, held in trust for a future messianic community.
% ABSENT_VOICES: Secular historians and critical biblical scholars who would argue the texts are archaic remnants rather than binding law; Reform Jewish voices that prioritize ethical monotheism over ritual preservation; they are structurally excluded from the halakhic discourse that frames the obligation.
% DISAPPEARANCE_RATIONALE: If the preparation framing vanished, the extensive study of sacrificial law by communities with no Temple would lose its primary justification; yeshivot would redirect curricular resources to applied halakha, and the specialized technical knowledge would atrophy within one or two generations.
% FOUNDING_PROBLEM: The destruction of the Second Temple created a crisis of continuity: biblical sacrificial law could not be performed, risking total loss of the technical knowledge required for legitimate worship upon restoration.
% FOUNDING_PROBLEM_CORROBORATION: Attested by Talmudic tractates Avodah Zarah and Mishnah Eduyot, which document the urgency of preserving Temple procedures; corroborated by medieval commentators such as Maimonides in Mishneh Torah, Hilchot Beit HaBechirah, who systematized the laws explicitly for future implementation; modern academic historians of religion attest to the historical rupture from outside the theological benefiting framework.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_preparation, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_preparation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_preparation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_obligation__study_as_preparation, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_preparation, 0.25, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is low (0.25) because the constraint extracts cognitive labor rather than material rents, and no present agent concentrates the proceeds. Suppression is moderate-low (0.35): enforcement operates through institutional curriculum and communal religious identity rather than physical coercion. Theater ratio is low (0.12) because the study is genuinely functional as technical preservation, though a small performative element grows over time as the knowledge becomes more abstract. Accessibility collapse is moderate (0.58): within the halakhic framework, letting the knowledge lapse is unthinkable, but the framework itself is contestable. Resistance is low (0.15) because objections to this specific obligation are usually subsumed into broader resistance to rabbinic authority rather than targeting Kodashim study independently.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (current generation) experiences the constraint as a burden of deferred repairâstudying laws for a restoration they may never see. The agenda-setter seat (rabbinic transmitters) experiences it as a sacred duty of continuity. The intended beneficiary seat (messianic future) is not present to register any experience. The engine computes this divergence from the structural asymmetry: present payers, absent beneficiaries, and institutional administrators caught between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Current_generation_jews is the structural target: they bear the study costs, their exit is identity-locked, and they receive no performative payoff. Rabbinic_transmitters sits near symmetric: they administer the constraint and derive authority from it, but do not materially capture the extraction. Messianic_future_community is the structural beneficiary but is a non-agent, so it does not feed directionality computation as an active rent-collector. The secular historians are excluded entirely. The net effective extraction is damped by the absence of a present beneficiary capturing rents.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâthe destruction of the Temple and risk of knowledge lossâis still live, which prevents misclassification as piton. The preparation framing gives the constraint a clear transition logic rather than an atrophied steady state. Because no present party concentrates the gains, it avoids snare classification despite having identifiable payers. The low theater ratio confirms that the function has not been hollowed out into performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is this constraint a reading of a contested kernel where study genuinely functions as preparation, or is the preparation framing a post-hoc justification for maintaining textual preservation and rabbinic authority?',
    'Comparative historical analysis across the three kernel readings to trace whether the preparation logic predates or postdates the institutional stabilization of Kodashim curriculum.',
    'If the preparation framing is post-hoc, the constraint''s low extractiveness weakens and it may reclassify toward identity_coordination or tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the preparation reading is structurally primary or retroactive justification.').

omega_variable(
    obligation_naturalness,
    'Is the obligation to study Kodashim a revealed divine constraint or a rabbinically constructed mechanism to maintain institutional continuity across Temple destruction?',
    'Historical source criticism of the textual layers commanding Kodashim study; sociological analysis of rabbinic authority formation in Yavneh and beyond.',
    'If constructed rather than revealed, the constraint''s emergence is not natural and its classification shifts from scaffold toward tangled_rope or snare depending on present beneficiary structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(obligation_naturalness, empirical, 'Divine revelation versus institutional construction of the study obligation.').

omega_variable(
    future_beneficiary_agency,
    'Can a non-present messianic community function as a structural beneficiary, or does the benefit accrue to present rabbinic institutions that control the textual tradition?',
    'Trace present resource flowsâstatus, funding, and institutional powerâgenerated by the Kodashim curriculum in contemporary yeshivot and rabbinic courts.',
    'If present institutions capture the benefit, directionality shifts toward the rabbinic class and effective extractiveness rises substantially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_beneficiary_agency, empirical, 'Whether present or future agents are the true beneficiaries of preserved knowledge.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_preparation, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kodashim_prep_tr_t0, kodashim_obligation__study_as_preparation, theater_ratio, 0, 0.05).
narrative_ontology:measurement(kodashim_prep_tr_t400, kodashim_obligation__study_as_preparation, theater_ratio, 400, 0.06).
narrative_ontology:measurement(kodashim_prep_tr_t800, kodashim_obligation__study_as_preparation, theater_ratio, 800, 0.08).
narrative_ontology:measurement(kodashim_prep_tr_t1200, kodashim_obligation__study_as_preparation, theater_ratio, 1200, 0.1).
narrative_ontology:measurement(kodashim_prep_tr_t1600, kodashim_obligation__study_as_preparation, theater_ratio, 1600, 0.11).
narrative_ontology:measurement(kodashim_prep_tr_t2000, kodashim_obligation__study_as_preparation, theater_ratio, 2000, 0.12).

% Extraction over time
narrative_ontology:measurement(kodashim_prep_be_t0, kodashim_obligation__study_as_preparation, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(kodashim_prep_be_t400, kodashim_obligation__study_as_preparation, base_extractiveness, 400, 0.14).
narrative_ontology:measurement(kodashim_prep_be_t800, kodashim_obligation__study_as_preparation, base_extractiveness, 800, 0.18).
narrative_ontology:measurement(kodashim_prep_be_t1200, kodashim_obligation__study_as_preparation, base_extractiveness, 1200, 0.21).
narrative_ontology:measurement(kodashim_prep_be_t1600, kodashim_obligation__study_as_preparation, base_extractiveness, 1600, 0.23).
narrative_ontology:measurement(kodashim_prep_be_t2000, kodashim_obligation__study_as_preparation, base_extractiveness, 2000, 0.25).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_obligation__study_as_preparation, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_preparation, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
