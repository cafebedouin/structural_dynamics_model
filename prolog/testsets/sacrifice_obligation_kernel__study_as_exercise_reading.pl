% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__study_as_exercise_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__study_as_exercise_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sacrifice_obligation_kernel__study_as_exercise_reading
 *   human_readable: Sacrifice Obligation Fulfilled Through Intellectual Study (Rabbinic Reading)
 *   domain: religious/halakhic/commitment_system
 *
 * SUMMARY:
 *   Study of the sacrificial laws in Talmudic and rabbinic tradition is
 *   understood as genuine fulfillment of the obligation to offer sacrifice
 *   (korban), particularly after the destruction of the Second Temple in 70
 *   CE made physical performance impossible. Under this reading, intellectual
 *   engagement with the texts and concepts of sacrifice law constitutes the
 *   legitimate exercise of the mitzvah in the current era. The rabbinic
 *   interpretive community maintains the authority to adjudicate what counts
 *   as proper study and adequate understanding, but the reading itself
 *   describes a coordination problem solved through reinterpretation rather
 *   than an extractive mechanism. No victim set exists because the
 *   obligation's transformation is authorized and widely accepted within the
 *   tradition. The beneficiary is rabbinic interpretive authority, which
 *   holds monopoly on what counts as legitimate fulfillment, but this appears
 *   to be a coordination function (maintaining coherence across diaspora
 *   communities, across time, and across changing material conditions) rather
 *   than extraction.
 *
 * KEY AGENTS:
 *   - Rabbinic interpretive authority (institutional): Sets canon of study, adjudicates disputes about adequacy, maintains tradition; held analytical/power-setting role over the reading's legitimacy
 *   - Committed practitioners (moderate power): Can fulfill obligation through study, accessible without Temple; beneficiary of the reading's expansion of accessibility
 *   - Jewish diaspora communities (organized): Benefit from making the obligation non-vacuous in diaspora; study is available everywhere Temple is not
 *   - Alternative-reading advocates (moderate power, excluded): Hold performance-only, messianic suspension, or symbolic-archive readings; structurally marginalized by dominance of study-as-exercise reading
 *   - Halakhic observer (analytical seat): Examines how interpretive authority sustains and defends the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__study_as_exercise_reading, 0.0).
domain_priors:suppression_score(sacrifice_obligation_kernel__study_as_exercise_reading, 0.0).
domain_priors:theater_ratio(sacrifice_obligation_kernel__study_as_exercise_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__study_as_exercise_reading, rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel__study_as_exercise_reading, "Sacrifice Obligation Fulfilled Through Intellectual Study (Rabbinic Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__study_as_exercise_reading, "religious/halakhic/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__study_as_exercise_reading, 'a837bb35-6121-417b-b76a-bd9d62a1ae5f').
narrative_ontology:cs_kernel_codification('a837bb35-6121-417b-b76a-bd9d62a1ae5f', fixed_text).
narrative_ontology:cs_authority_grounding('a837bb35-6121-417b-b76a-bd9d62a1ae5f', lineage).
narrative_ontology:cs_interpretation_layer_present('a837bb35-6121-417b-b76a-bd9d62a1ae5f').
narrative_ontology:cs_reading_relation('a837bb35-6121-417b-b76a-bd9d62a1ae5f', sacrifice_obligation_kernel__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('a837bb35-6121-417b-b76a-bd9d62a1ae5f', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('a837bb35-6121-417b-b76a-bd9d62a1ae5f', sacrifice_obligation_kernel__symbolic_archive_reading, coexists_with).
narrative_ontology:cs_axiom('a837bb35-6121-417b-b76a-bd9d62a1ae5f', foundational, intellectual_engagement_constitutes_mitzvah_performance).
narrative_ontology:cs_axiom_status(intellectual_engagement_constitutes_mitzvah_performance, holdable).
narrative_ontology:cs_axiom_grounding('a837bb35-6121-417b-b76a-bd9d62a1ae5f', intellectual_engagement_constitutes_mitzvah_performance, deontological).
narrative_ontology:cs_axiom('a837bb35-6121-417b-b76a-bd9d62a1ae5f', foundational, mitzvah_obligation_remains_binding_post_temple).
narrative_ontology:cs_axiom_status(mitzvah_obligation_remains_binding_post_temple, holdable).
narrative_ontology:cs_axiom_grounding('a837bb35-6121-417b-b76a-bd9d62a1ae5f', mitzvah_obligation_remains_binding_post_temple, deontological).
narrative_ontology:cs_reference_frame('a837bb35-6121-417b-b76a-bd9d62a1ae5f', sacrificial_obligation_binding_status).
narrative_ontology:cs_drift_state('a837bb35-6121-417b-b76a-bd9d62a1ae5f', contemporary_diaspora_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a837bb35-6121-417b-b76a-bd9d62a1ae5f', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_interpretive_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, committed_practitioners).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, jewish_diaspora_communities).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__study_as_exercise_reading, talmudic_substitution_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__study_as_exercise_reading, intellect_as_sacred_action).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__study_as_exercise_reading, study_as_mitzvah_performance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The interpretive community (yeshiva scholars, halakhic decisors, Talmudic authorities) maintains and transmits the doctrine that study of sacrificial law fulfills the mitzvah obligation. They set the canon of what counts as proper study, adjudicate disputes about the adequacy of engagement, and hold interpretive monopoly over the reading of textual sources. This authority is a benefit insofar as control over legitimate fulfillment allows them to sustain institutional authority in the absence of Temple practice.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_interpretive_authority, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_interpretive_authority, beneficiary).

% Individuals who accept the study-as-exercise reading can fulfill a binding obligation through study and contemplation, which is accessible without Temple, priesthood, or animal sacrifice. This removes a structural impossibility (Temple reconstruction) from the mitzvah's demand set. Study is available to women, the diaspora-based, and those without priestly lineage — a genuine expansion of accessible fulfillment relative to the performance-only reading. Their commitment to the reading may be identity-locked to Jewish religious identity.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, committed_practitioners, beneficiary,
    moderate, biographical, identity_locked, global).

% Without Temple and priesthood, the study-as-exercise reading makes the sacrifice obligation non-vacuous for communities without geographic access to Jerusalem, priestly lineage, or the political conditions for ritual performance. The constraint translates an impossible obligation into a sustainable one: all Jews can study; the mitzvah becomes operative everywhere.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, jewish_diaspora_communities, beneficiary,
    organized, generational, constrained, global).

% Parties who hold the performance-only reading (sacrifice requires literal physical performance), the messianic-suspension reading (the obligation is divinely held in suspension), or the symbolic-archive reading (study preserves continuity but makes no halakhic claim) are structurally excluded from the authoritative conversation about what constitutes fulfillment under this reading. Their arguments are heard within the halakhic tradition but the study-as-exercise reading's institutional dominance means their objections do not reshape the ruling framework.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, alternative_readings_advocates, excluded,
    moderate, generational, constrained, global).

% Groups advocating for literal Temple reconstruction and performance-based sacrifice are, under this reading, attempting to activate an obligation the rabbinic consensus has already transformed. They are excluded from the institutional legitimacy loop: their position is heard as a minority practice, not as a competing authoritative reading. They remain trapped within the Jewish legal system but cannot revise its dominant interpretation.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, temple_reconstruction_movements, excluded,
    moderate, generational, trapped, regional).

% An analyst external to the tradition can examine how this reading instantiates halakhic authority: how textual interpretation becomes binding obligation, how the constraint on legitimate fulfillment is maintained, and how alternative readings are managed within the tradition.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, halakhic_scholar_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the structural problem of a mitzvah (commandment) that became literally impossible to perform after Temple destruction: transforms an obligation demanding physical sacrifice into one fulfillable through intellectual engagement with sacred texts. This enables the entire Jewish religious system to persist as binding even in the diaspora without Temple or priesthood.
% TRANSFER_FUNCTION: Transfers the power to adjudicate legitimate fulfillment from performance capacity (who has access to Temple, priesthood, animals) to interpretive authority (who controls the reading of textual sources and the standards of adequate study). Under this reading, the mitzvah obligation is 'moved' from the physical/ritual domain to the intellectual/hermeneutic domain, and control over what constitutes proper engagement shifts to the rabbinic interpretive community.
% ABSENT_VOICES: Practitioners who believe in literal performance-only fulfillment are excluded from the authoritative conversation — their objections exist in the tradition but do not reshape the institutional ruling. Temple reconstruction advocates who claim the obligation should be reactivated in its original form are structurally outside the dominant reading's legitimacy framework. Early Christian interpreters of Jewish law (who may have argued the mitzvah obligation is superseded entirely) are historically excluded from the rabbinic conversation itself.
% DISAPPEARANCE_RATIONALE: If this reading vanished and the performance-only reading prevailed, the entire edifice of post-Temple Jewish religious life would face a crisis: the mitzvah would become an impossible, unfulfilable obligation (no Temple, no priesthood, no legitimate performance possible). Judaism would either have to rebuild the Temple to restore fulfillment, declare the mitzvah suspended indefinitely (the messianic reading), or abandon the claim that it remains binding. The study-as-exercise reading is architecturally central to how Jewish practice remains coherent after Temple destruction.
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE made direct animal sacrifice impossible for the Jewish people. Yet the obligation to offer sacrifice (korban) remained textually binding in Torah and rabbinic literature. The reading solves this by reinterpreting what 'fulfilling the mitzvah' means: intellectual study and contemplation of the sacrificial laws constitute the legitimate form of the obligation under conditions where physical performance is structurally impossible.
% FOUNDING_PROBLEM_CORROBORATION: The problem is confirmed by external historical record: the Temple was destroyed, sacrifice ended, yet Jewish communities continued to claim the mitzvah was binding. Talmudic sources themselves (external to any individual reading, part of the shared textual corpus) document this tension: how can an obligation to sacrifice remain binding when sacrifice is impossible? The study-as-exercise reading is attested in Talmudic passages (e.g., discussions in Menachot and Zevachim tractates) and appears in medieval and modern halakhic decisors (Maimonides, R. Isaac Alfasi). The problem is also confirmed by comparative religious history: other traditions (Islam after the Prophet's era, Christianity post-Temple Judaism) faced parallel problems of rituals becoming impossible and solved them similarly through reinterpretation.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__study_as_exercise_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__study_as_exercise_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__study_as_exercise_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__study_as_exercise_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__study_as_exercise_reading_tests).
:- end_tests(sacrifice_obligation_kernel__study_as_exercise_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored zero because the reading solves a genuine structural problem (Temple destruction made sacrifice impossible) through a coherent reinterpretation that is institutionally accepted and widely internalized. There is no victim set — no one is forced into a role they did not consent to. Accessibility actually expands under this reading: all Jews can study, whereas only the priesthood could perform sacrifice. The suppression is zero because the reading is not maintained against resistance through coercion; it is the dominant halakhic consensus. Theater_ratio is zero because the study is not a facade for another function; it is the actual function the reading claims. The constraint's persistence is not theatrical maintenance but institutional transmission and genuine practice. Measurements are flat across 2000 years of the reading's history because the reading's core structure has been stable: study fulfills the mitzvah, rabbinic authority adjudicates adequacy, the obligation remains binding. The interval spans from earliest post-Temple rabbinic attestation (~100 CE) through contemporary era (~2100 CE), modeling the reading's historical span.
 *
 * PERSPECTIVAL GAP:
 *   Rabbinic interpretive authority and committed practitioners should compute similarly under this reading: both benefit from the reinterpretation and neither is extracted from. However, alternative-reading advocates (excluded) compute very differently: they believe the reading forecloses their own position unjustly or that the study-as-exercise claim is unauthorized, and they experience the institutional dominance as suppression of their voice. The engine should compute this perspective divergence from the structural data: agenda-setter (rabbinic authority) holds power and sets interpretive canon, excluded advocates hold moderate power but are structurally outside the authoritative framework. No directionality override is needed because the automatic derivation from beneficiary + power + exit should produce the right d values: beneficiaries and agenda-setter sit at beneficiary end; excluded advocates sit at a mixed position (moderate power, constrained exit, but no explicit victim role because they are not harmed by the reading's operation — they are excluded from authority, not extracted from).
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic interpretive authority benefits from control over legitimate fulfillment but this appears to be a coordination function, not extraction. The benefit is the power to adjudicate disputes and maintain institutional coherence in diaspora contexts. Committed practitioners and diaspora communities benefit from the reading making the obligation accessible. No stakeholder bears a cost or is victimized; the reading expands accessibility and solves a structural impossibility. Alternative-reading advocates are excluded from the authoritative conversation, but exclusion from authority is not the same as extraction or victimization. Their directionality should sit near symmetric or slightly toward the target end (excluded from authority, constrained exit, but no victim role). The reading itself describes a genuine coordination function (solving Temple destruction impossibility) coupled with institutional authority (rabbinic monopoly on legitimacy), but without extractive asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Temple destruction made sacrifice impossible) is demonstrably live in the historical record: the problem is attested by external sources (Roman historical record confirms Temple destruction), and the reading's solution persists across 2000 years of Jewish practice. There is no mandatrophy here — the obligation remains meaningful and the reading remains the authoritative halakhic consensus. The reading's mandate has not outlived its function: facilitating fulfillment of the sacrifice obligation in diaspora contexts where Temple is absent is still the central function. If the Temple were restored and physical sacrifice became possible again, the reading would face challenge from the performance-only reading, but that is not mandatrophy; that is the reading's response to a changed material condition. Mandatrophy would be if the reading persisted theatrically while everyone tacitly accepted the mitzvah was no longer binding. That is not the case.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_status_live_or_settled,
    'Is the study-as-exercise reading the settled consensus of rabbinic authority, or one live position among genuinely competing readings?',
    'Examination of contemporary halakhic responsa, rulings from major yeshivas and batei din, and whether alternative readings (performance-only, messianic suspension, symbolic archive) are treated as binding minority opinions or as superseded positions. Survey of actual practice: what do contemporary Jews do when fulfilling the sacrifice obligation?',
    'If this reading is the consensus: it should classify as rope (genuine coordination without significant contestation). If it remains live among competitors: it may be tangled_rope (coordination coupled with interpretive asymmetry, where rabbinic authority maintains monopoly on what counts as legitimate fulfillment). If it is theatrically invoked but not truly practiced: piton (inertial institutional performance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_status_live_or_settled, empirical, 'Whether the study-as-exercise reading is the authoritative consensus or one of multiple live positions.').

omega_variable(
    rabbinic_benefit_extraction_or_coordination_cost,
    'Does the interpretive monopoly held by rabbinic authority represent a genuine coordination cost (adjudicating disputes, maintaining textual authority), or does it constitute extractive benefit (monopoly on legitimacy rent)?',
    'Historical analysis of how the authority is used: are the interpretive rules applied consistently across different social positions? Do wealthy, powerful, or influential community members get different interpretations than others? Is there evidence of the authority resisting alternative readings because they would dilute institutional power?',
    'If coordination cost: the beneficiary designation is accurate and extractiveness remains near zero. If extractive: the constraint may be tangled_rope (coordination function + asymmetric interpretive control) or snare (the coordination story is cover for monopoly on legitimacy). This would shift the directionality of rabbinic authority toward higher d values and the effective extraction upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rabbinic_benefit_extraction_or_coordination_cost, empirical, 'Whether rabbinic interpretive authority represents legitimate coordination function or monopolistic extraction.').

omega_variable(
    alternative_readings_genuinely_impossible_or_suppressed,
    'Are the alternative readings (performance-only, messianic suspension, symbolic archive) logically foreclosed by the core premises of the study-as-exercise reading, or are they suppressed/marginalized by institutional power?',
    'Logical analysis of the axioms: does accepting ''study as legitimate exercise of mitzvah'' necessarily entail rejecting ''physical performance is the sole legitimate form''? (No — one could hold that both are acceptable forms, or that study is preparatory but not fulfilling.) If the alternatives are logically possible but institutionally marginalized, that indicates coexistence rather than foreclosure, and suggests the relation should be ''coexists_with'' rather than ''forecloses''.',
    'If alternatives are genuinely foreclosed by the core axiom: the reading_relations entry for each sibling should declare ''forecloses''. If alternatives are logically possible but institutionally suppressed: the reading_relations should declare ''coexists_with'' and this suggests higher extraction (monopoly on legitimacy rather than logical necessity).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_readings_genuinely_impossible_or_suppressed, conceptual, 'Whether alternative readings are logically ruled out or institutionally suppressed.').

omega_variable(
    identity_locked_vs_mobile_commitment,
    'For practitioners who accept this reading, is the commitment to study-as-exercise rooted in mobile, revisable intellectual conviction, or is it identity-locked (fused with Jewish identity, irrevisable without existential cost)?',
    'Ethnographic observation: what happens when a practitioner encounters evidence that study may not fulfill the obligation (e.g., scholarly argument for performance-only reading)? Can they revise their commitment without experiencing loss of identity? Or is the study-as-exercise reading so embedded in how they understand ''being Jewish'' that rejecting it feels like apostasy?',
    'If mobile: practitioners exit and revise commitment at lower cost; the constraint''s hold is less identity-locked. If identity-locked: practitioners stay within the reading even if intellectually unconvinced; exit is identity-destructive. This affects effective suppression (measured zero here, but identity-lock may be an internalized suppression mechanism) and affects directionality for practitioners (may sit closer to target than the mobile/beneficiary framing suggests).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_vs_mobile_commitment, empirical, 'Whether practitioner commitment to the study-as-exercise reading is mobile or identity-locked.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__study_as_exercise_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement_basis(sacr_tr_t0, observed).
narrative_ontology:measurement(sacr_tr_t250, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 250, 0.0).
narrative_ontology:measurement_basis(sacr_tr_t250, observed).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 500, 0.0).
narrative_ontology:measurement_basis(sacr_tr_t500, observed).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 1000, 0.0).
narrative_ontology:measurement_basis(sacr_tr_t1000, observed).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 1500, 0.0).
narrative_ontology:measurement_basis(sacr_tr_t1500, observed).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 2000, 0.0).
narrative_ontology:measurement_basis(sacr_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 0, 0.0).
narrative_ontology:measurement_basis(sacr_be_t0, observed).
narrative_ontology:measurement(sacr_be_t250, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 250, 0.0).
narrative_ontology:measurement_basis(sacr_be_t250, observed).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 500, 0.0).
narrative_ontology:measurement_basis(sacr_be_t500, observed).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 1000, 0.0).
narrative_ontology:measurement_basis(sacr_be_t1000, observed).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 1500, 0.0).
narrative_ontology:measurement_basis(sacr_be_t1500, observed).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 2000, 0.0).
narrative_ontology:measurement_basis(sacr_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 0, 0.0).
narrative_ontology:measurement_basis(sacr_su_t0, observed).
narrative_ontology:measurement(sacr_su_t250, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 250, 0.0).
narrative_ontology:measurement_basis(sacr_su_t250, observed).
narrative_ontology:measurement(sacr_su_t500, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 500, 0.0).
narrative_ontology:measurement_basis(sacr_su_t500, observed).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 1000, 0.0).
narrative_ontology:measurement_basis(sacr_su_t1000, observed).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 1500, 0.0).
narrative_ontology:measurement_basis(sacr_su_t1500, observed).
narrative_ontology:measurement(sacr_su_t2000, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 2000, 0.0).
narrative_ontology:measurement_basis(sacr_su_t2000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__study_as_exercise_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_kernel__study_as_exercise_reading, 0.0).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% The sacrifice_obligation_kernel decomposes into four constraint stories, one per reading. Each reading instantiates a different claim about what fulfillment means and carries different structural implications (extraction, victim sets, beneficiaries). The study_as_exercise_reading (this story) describes zero extractiveness and genuine coordination; the performance_only_reading describes structural impossibility (no Temple = obligation cannot be fulfilled); the messianic_suspension_reading describes authorized transformation (suspension is divinely declared, not extracted); the symbolic_archive_reading describes a purely cultural-historical function without halakhic claim. All four are linked via affects_constraints because they contest the meaning of the same kernel and would produce different practical outcomes if adopted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
