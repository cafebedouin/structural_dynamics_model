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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: kodashim_obligation__study_as_preparation
 *   human_readable: Kodashim Study as Messianic Preparation
 *   domain: religious/jewish_law
 *
 * SUMMARY:
 *   This constraint instantiates the 'study_as_preparation' reading of the
 *   contested kodashim_obligation kernel. Under this reading, the sacrificial
 *   laws of the Torah remain fully binding despite the destruction of the
 *   Temple, and their study constitutes instrumental preparation for
 *   messianic restoration rather than performance substitution or mere
 *   historical archiving. The current generation bears the labor of technical
 *   preservation; the messianic future is the nominal beneficiary. The
 *   structural question is whether this is intertemporal coordination or
 *   institutional extraction dressed in redemptive deferral.
 *
 * KEY AGENTS:
 *   - rabbinic_academy: agenda_setter (institutional/identity_locked) â administers curriculum and enforces the obligation across diaspora
 *   - current_generation_community: primary payer (moderate/identity_locked) â bears study labor for deferred performance
 *   - messianic_future_community: nominal beneficiary (moderate/constrained) â future recipient of preserved technical knowledge
 *   - secular_historians: excluded (organized/mobile) â would read the system as defunct but are outside halakhic deliberation
 *   - critical_anthropologist: analytical observer (moderate/analytical) â tracks deferred performance as cultural mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_preparation, 0.3).
domain_priors:suppression_score(kodashim_obligation__study_as_preparation, 0.45).
domain_priors:theater_ratio(kodashim_obligation__study_as_preparation, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, extractiveness, 0.3).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_preparation, tangled_rope).
narrative_ontology:human_readable(kodashim_obligation__study_as_preparation, "Kodashim Study as Messianic Preparation").
narrative_ontology:topic_domain(kodashim_obligation__study_as_preparation, "religious/jewish_law").

domain_priors:requires_active_enforcement(kodashim_obligation__study_as_preparation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_preparation, '43bdd790-b195-4f45-8c14-81c59eebec99').
narrative_ontology:cs_kernel_codification('43bdd790-b195-4f45-8c14-81c59eebec99', fixed_text).
narrative_ontology:cs_authority_grounding('43bdd790-b195-4f45-8c14-81c59eebec99', lineage).
narrative_ontology:cs_interpretation_layer_present('43bdd790-b195-4f45-8c14-81c59eebec99').
narrative_ontology:cs_reading_relation('43bdd790-b195-4f45-8c14-81c59eebec99', kodashim_obligation__study_as_archive, forecloses).
narrative_ontology:cs_reading_relation('43bdd790-b195-4f45-8c14-81c59eebec99', kodashim_obligation__study_as_performance, coexists_with).
narrative_ontology:cs_axiom('43bdd790-b195-4f45-8c14-81c59eebec99', foundational, sacrificial_law_binding_despite_nonperformance).
narrative_ontology:cs_axiom_status(sacrificial_law_binding_despite_nonperformance, holdable).
narrative_ontology:cs_axiom_grounding('43bdd790-b195-4f45-8c14-81c59eebec99', sacrificial_law_binding_despite_nonperformance, deontological).
narrative_ontology:cs_axiom('43bdd790-b195-4f45-8c14-81c59eebec99', foundational, temple_restoration_structurally_required).
narrative_ontology:cs_axiom_status(temple_restoration_structurally_required, holdable).
narrative_ontology:cs_axiom_grounding('43bdd790-b195-4f45-8c14-81c59eebec99', temple_restoration_structurally_required, empirically_contingent).
narrative_ontology:cs_reference_frame('43bdd790-b195-4f45-8c14-81c59eebec99', temple_era_performative_norm).
narrative_ontology:cs_drift_state('43bdd790-b195-4f45-8c14-81c59eebec99', post_second_temple_destruction, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('43bdd790-b195-4f45-8c14-81c59eebec99', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_preparation, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, messianic_future_community).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_preparation, current_generation_community).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_preparation, oral_law_authority).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_preparation, bindingness_across_dispossession).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the curriculum of Talmudic study including Kodashim tractates, sets standards for yeshiva education, and interprets the scope of the obligation. Their authority derives from an unbroken chain of transmission claiming to originate at Sinai. Exit would mean abandoning the foundational premise of their institutional existence.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, rabbinic_academy, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Obligated to devote study time to sacrificial laws that cannot be enacted. They experience this as a genuine religious duty with merit, but structurally bear the labor of maintaining a technical corpus whose performance they will likely never witness. Leaving the obligation means leaving the community.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, current_generation_community, payer,
    moderate, biographical, identity_locked, global).

% The anticipated community of the messianic era which will inherit the preserved technical knowledge and restored conditions for Temple worship. They do not currently exist as an organized seat, but are the nominal recipients of the intertemporal transfer.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, messianic_future_community, beneficiary,
    moderate, civilizational, constrained, global).

% Read Kodashim as an extinct Iron Age priestly system. They would argue that treating these texts as binding obligations rather than historical sources is a category error, but their framework is excluded from halakhic deliberation.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, secular_historians, excluded,
    organized, biographical, mobile, global).

% Observes the constraint as a mechanism of cultural preservation under conditions of structural dispossession. They neither pay nor benefit from the halakhic system, but track how deferred performance functions to maintain group cohesion across diaspora.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, critical_anthropologist, observer,
    moderate, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserving technical sacrificial law across a temporal gap during which the physical Temple and priesthood are absent, so that a future community can resume performance immediately upon restoration of the requisite conditions.
% TRANSFER_FUNCTION: Moves sustained cognitive labor, curricular attention, and mnemonic discipline from the present generation to the maintenance of a detailed textual-technical corpus; the benefit is deposited with a future messianic community that will possess the restored Temple.
% ABSENT_VOICES: Secular historians who classify Kodashim as a defunct Iron Age cultic system; Karaite critics who reject the rabbinic oral-law framework that sustains the obligation; Christian supersessionist voices who read the system as permanently abrogated; and internally, mystical currents that claim study already substitutes for sacrifice.
% DISAPPEARANCE_RATIONALE: From within the covenantal framework, disappearance would mean irretrievable loss of technical knowledge and a rupture in the chain of tradition; the world would rearrange around that loss. From an external frame, the texts persist as documents regardless of whether anyone studies them as law, so the world would remain materially unchanged.
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE eliminated the physical and institutional conditions for the performance of biblical sacrificial commandments, creating a crisis of continuity: how to relate to a large corpus of divine law that had become impossible to perform.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested extensively within rabbinic literature (Talmud, Mishnah) and by Maimonides. No external, non-beneficiary party attests this as a live crisis; secular historians record the destruction as a historical event but do not treat the resulting halakhic impasse as a problem requiring a continued legal solution.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_preparation, contested).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_preparation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_preparation, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_obligation__study_as_preparation, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_preparation, 0.3, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is low (0.30) because the coordination function is genuine and the transfer is deferred to a non-present beneficiary, leaving limited present-day capture. Suppression is moderate (0.45) because the constraint depends on rabbinic curriculum enforcement and social norm maintenance to sustain study of otherwise irrelevant texts. Theater is low (0.25) because the study does produce real technical competence and mnemonic fidelity. Accessibility collapse is high (0.75) because once the rabbinic framework is accepted, alternatives (archive, abrogation) collapse entirely. Resistance is low (0.15) because the committed community internalizes the obligation and external resistance does not engage the framework.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (rabbinic academy) experiences the constraint as a sacred duty of custodianship; the payer seat (current generation) experiences it as a binding obligation with deferred gratification. The engine will compute divergent per-seat classifications because the rabbinic seat is the administrator with low directionality, while the current generation is the target of the obligation with high directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   The messianic_future_community is declared beneficiary and receives low directionality (the constraint subsidizes their future position by depositing knowledge). The current_generation_community is declared victim/payer and receives high directionality (the constraint extracts their present cognitive labor). The rabbinic_academy is agenda_setter, not beneficiary; their directionality is derived as intermediate because they both administer and are bound by the same obligation, though their identity-lock pulls them toward the beneficiary end. No override is needed because this structural derivation matches the actual asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying as tangled_rope prevents mislabeling this as pure rope (which would ignore the deferred extraction from the current generation) or pure snare (which would ignore the genuine coordination problem of preserving technical knowledge across a two-millennium gap). It captures the hybridity: a real preservation function plus an asymmetric temporal transfer from present to future. If the Temple were restored tomorrow, the constraint would dissolve into direct performance, confirming its scaffold-like nature; but absent that resolution, it operates as a tangled rope sustained by active rabbinic enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Is this constraint a genuine intertemporal coordination mechanism, or does the study obligation serve present-day institutional maintenance under the cover of messianic deferral?',
    'Comparative analysis of resource allocation: if rabbinic institutions extract substantial present-day status or material support from the study obligation that exceeds the coordination cost of preservation, the preparation reading functions as extraction cover.',
    'Would reclassify from tangled_rope toward snare if present-day capture is concentrated; would support rope classification if extraction is genuinely negligible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Whether the preparation frame masks present-day extraction.').

omega_variable(
    obligation_naturalness,
    'Does the bindingness of unperformable law emerge naturally from the textual kernel, or is it a constructed rabbinic innovation to manage post-destruction trauma?',
    'Historical-critical analysis of Second Temple and rabbinic literature to determine whether the ''binding but suspended'' framework predates the destruction or was developed in Yavneh.',
    'If constructed, the constraint''s legitimacy rests on rabbinic authority rather than textual natural law, raising extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(obligation_naturalness, empirical, 'Historical origin of the binding-yet-suspended framework.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the enforcement of Kodashim study structural (rabbinic curriculum requirements) or internalized (believers'' self-concept fused with textual fidelity)?',
    'Measure study rates and curriculum emphasis in communities with and without centralized rabbinic authority; if study persists absent external enforcement, suppression is largely internalized.',
    'Internalized suppression raises effective extraction because the target carries the constraint beyond external enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_preparation, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_obligation__study_as_preparation, theater_ratio, 0, 0.12).
narrative_ontology:measurement(koda_tr_t400, kodashim_obligation__study_as_preparation, theater_ratio, 400, 0.18).
narrative_ontology:measurement(koda_tr_t800, kodashim_obligation__study_as_preparation, theater_ratio, 800, 0.22).
narrative_ontology:measurement(koda_tr_t1200, kodashim_obligation__study_as_preparation, theater_ratio, 1200, 0.24).
narrative_ontology:measurement(koda_tr_t1600, kodashim_obligation__study_as_preparation, theater_ratio, 1600, 0.26).
narrative_ontology:measurement(koda_tr_t2000, kodashim_obligation__study_as_preparation, theater_ratio, 2000, 0.28).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_obligation__study_as_preparation, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(koda_be_t400, kodashim_obligation__study_as_preparation, base_extractiveness, 400, 0.28).
narrative_ontology:measurement(koda_be_t800, kodashim_obligation__study_as_preparation, base_extractiveness, 800, 0.32).
narrative_ontology:measurement(koda_be_t1200, kodashim_obligation__study_as_preparation, base_extractiveness, 1200, 0.3).
narrative_ontology:measurement(koda_be_t1600, kodashim_obligation__study_as_preparation, base_extractiveness, 1600, 0.27).
narrative_ontology:measurement(koda_be_t2000, kodashim_obligation__study_as_preparation, base_extractiveness, 2000, 0.3).

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
