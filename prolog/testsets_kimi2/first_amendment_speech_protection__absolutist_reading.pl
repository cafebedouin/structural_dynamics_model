% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_speech_protection__absolutist_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: first_amendment_speech_protection__absolutist_reading
 *   human_readable: First Amendment Absolutist Reading â Categorical Speech Protection
 *   domain: constitutional law / political philosophy / speech regulation
 *
 * SUMMARY:
 *   This constraint instantiates the absolutist reading of the
 *   first_amendment_speech_protection kernel, which holds that the First
 *   Amendment text 'Congress shall make no law...abridging the freedom of
 *   speech' creates a categorical prohibition on content-based speech
 *   restrictions, subject only to narrow historical exclusions such as
 *   incitement and obscenity. Sibling readings include the
 *   harm_limited_reading (protection yields to demonstrated unconsented-to
 *   harm) and the categorical_balancing_reading (protected/unprotected
 *   categories determined by case-by-case balancing), both of which this
 *   reading forecloses within a single interpretive framework. The structural
 *   delta for this reading is that the protected speech set is maximized, the
 *   cost of unregulated speech is externalized to targeted minorities, and
 *   the primary beneficiary is the speaker/majority while the victim is
 *   targeted minorities bearing systemic oppression costs.
 *
 * KEY AGENTS:
 *   - majority_speakers: Primary beneficiary (powerful/mobile) â receives categorical protection from speech regulation and externalizes harm costs.
 *   - targeted_minorities: Primary target (powerless/identity_locked) â bears systemic costs of protected hate speech and harassment without legislative remedy.
 *   - federal_judiciary: Agenda setter (institutional/constrained) â administers and enforces the absolutist reading through judicial review.
 *   - critical_legal_scholars: Analytical observer (analytical/analytical) â documents distributional asymmetry and systemic externalization.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__absolutist_reading, 0.72).
domain_priors:suppression_score(first_amendment_speech_protection__absolutist_reading, 0.75).
domain_priors:theater_ratio(first_amendment_speech_protection__absolutist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__absolutist_reading, "First Amendment Absolutist Reading â Categorical Speech Protection").
narrative_ontology:topic_domain(first_amendment_speech_protection__absolutist_reading, "constitutional law / political philosophy / speech regulation").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__absolutist_reading, '87b75288-5941-452e-8419-9983cb014840').
narrative_ontology:cs_kernel_codification('87b75288-5941-452e-8419-9983cb014840', fixed_text).
narrative_ontology:cs_authority_grounding('87b75288-5941-452e-8419-9983cb014840', lineage).
narrative_ontology:cs_interpretation_layer_present('87b75288-5941-452e-8419-9983cb014840').
narrative_ontology:cs_reading_relation('87b75288-5941-452e-8419-9983cb014840', first_amendment_speech_protection__harm_limited_reading, forecloses).
narrative_ontology:cs_reading_relation('87b75288-5941-452e-8419-9983cb014840', first_amendment_speech_protection__categorical_balancing_reading, forecloses).
narrative_ontology:cs_axiom('87b75288-5941-452e-8419-9983cb014840', foundational, no_law_textual_absolutism).
narrative_ontology:cs_axiom_status(no_law_textual_absolutism, holdable).
narrative_ontology:cs_axiom_grounding('87b75288-5941-452e-8419-9983cb014840', no_law_textual_absolutism, conventional).
narrative_ontology:cs_axiom('87b75288-5941-452e-8419-9983cb014840', secondary, narrow_historical_exclusion_doctrine).
narrative_ontology:cs_axiom_status(narrow_historical_exclusion_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('87b75288-5941-452e-8419-9983cb014840', narrow_historical_exclusion_doctrine, conventional).
narrative_ontology:cs_reference_frame('87b75288-5941-452e-8419-9983cb014840', categorical_no_law_framework).
narrative_ontology:cs_drift_state('87b75288-5941-452e-8419-9983cb014840', digital_speech_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('87b75288-5941-452e-8419-9983cb014840', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, majority_speakers).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, targeted_minorities).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__absolutist_reading, marketplace_of_ideas_doctrine).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__absolutist_reading, content_neutrality_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Express controversial and offensive views with near-categorical constitutional protection from government restriction. Bear no legal cost for speech that imposes social and psychological harms on marginalized groups. The constraint subsidizes their expressive freedom by externalizing compliance costs onto minorities.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, majority_speakers, beneficiary,
    powerful, generational, mobile, national).

% Bear concentrated systemic costs from hate speech, harassment, and intimidation that the absolutist reading protects from legislative remedy. Cannot exit minority identity; legal and political avenues to regulate harmful speech are structurally foreclosed by judicial invalidation of content-based restrictions.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, targeted_minorities, payer,
    powerless, biographical, identity_locked, national).

% Interprets and enforces the First Amendment to actively invalidate federal and state speech regulations. Bound by textualist commitments, precedent, and originalist methodology that treat 'no law' as categorical. Retains theoretical capacity to revise the reading but faces significant doctrinal and professional constraints against doing so.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Document and analyze the distributional asymmetry of the absolutist framework, arguing that categorical speech protection operates as a structural subsidy to majority power while insulating systemic oppression from democratic correction.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, critical_legal_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(first_amendment_speech_protection__absolutist_reading, majority_speakers).
narrative_ontology:fixing_cost_class(first_amendment_speech_protection__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents government censorship of political dissent and maintains an open public discourse by categorically prohibiting content-based speech restrictions, solving the coordination problem of trusting the state to regulate speech fairly.
% TRANSFER_FUNCTION: Moves the social and psychological costs of unregulated speechâharassment, hate speech, and intimidationâfrom majority speakers and the political majority to targeted minorities, while moving constitutional protection from government overreach to all speakers.
% ABSENT_VOICES: Targeted minorities seeking hate-speech or harassment protections, and state legislatures attempting to regulate speech for equity or safety, are structurally excluded from effective constitutional discourse; their legislative outputs are invalidated before they can operate.
% DISAPPEARANCE_RATIONALE: If the absolutist reading vanished overnight, legislatures would enact content-based speech regulationsâincluding hate-speech codes and harassment protectionsâthe US speech landscape would shift toward European or Canadian models, and the distribution of expressive costs would reorganize toward minority-protective regimes.
% FOUNDING_PROBLEM: Government suppression of political dissent, opposition press, and sedition under British colonial rule and the early Republic (Alien and Sedition Acts).
% FOUNDING_PROBLEM_CORROBORATION: Historians and originalist scholars attest to the founding-era suppression problem. Critical legal scholars, critical race theorists, and international human rights bodies attest that the current absolutist reading now generates distinct systemic harms, corroborating the contested status from outside the benefiting parties.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__absolutist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(first_amendment_speech_protection__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__absolutist_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(first_amendment_speech_protection__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the absolutist reading prevents democratic majorities from mitigating speech-based harms to minorities, effectively extracting protective capacity from the least powerful. Suppression is high (0.75) because the constraint's persistence depends on active judicial invalidation of minority-protective legislation, not on natural consensus. Theater is moderate-low (0.25): judicial enforcement is largely sincere textualism, though some originalist performance accompanies doctrinal maintenance. Accessibility collapse is high (0.80) because once the categorical framework is established, alternative regulatory models (European hate-speech regimes) become jurisprudentially inaccessible. Resistance is moderate (0.60) because critical legal movements and minority communities actively contest the doctrine. The measurement series share a single time grid to prevent misaligned drift dating.
 *
 * PERSPECTIVAL GAP:
 *   The majority speaker seat and the targeted minority seat should compute to radically different classifications: from the majority position the arrangement is legitimate coordination protecting liberty from government overreach; from the minority position the identical structure operates as active extraction that prevents democratic protection from harassment and hate. The engine computes this divergence from the structural data rather than the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Majority speakers are structural beneficiaries: they collect expressive subsidy and have mobile exit (can speak freely across jurisdictions). Their directionality sits near the beneficiary end. Targeted minorities are structural targets: they pay through systemic oppression, cannot exit minority status (identity_locked), and face trapped political options. Their directionality sits near the full-target end. The federal judiciary is the agenda setter, not a direct beneficiary of extraction, but structurally committed to the constraint's maintenance through professional and methodological lock-in.
 *
 * MANDATROPHY ANALYSIS:
 *   The absolutist reading prevents mislabeling by preserving a genuine coordination function: it actually does prevent government censorship and protect dissent. Without the victim/beneficiary asymmetry and the active judicial enforcement against minority-protective laws, the constraint might appear as a rope or mountain. The presence of identifiable, concentrated costs on powerless, identity-locked agents transforms it into tangled_rope rather than rope, and the absence of a concentrated beneficiary capturing monetary rent distinguishes it from snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    absolutist_kernel_location,
    'Does the absolutist reading represent a discovered constraint of constitutional text, or a constructed constraint that channels expressive power toward majority speakers?',
    'Historical and linguistic analysis of founding-era understanding of ''no law''; empirical analysis of which groups benefit from the doctrine''s persistence over time.',
    'If constructed rather than discovered, the reading is a false-summit candidate and reclassification toward snare or tangled_rope is reinforced; if genuinely textually compelled, the high extraction metrics indicate an irreconcilable tension in the constitutional order itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolutist_kernel_location, conceptual, 'Whether the absolutist reading is a natural feature of constitutional meaning or a constructed power channel').

omega_variable(
    minority_harm_externality,
    'Is the systemic harm to targeted minorities an unintended externality of speech coordination, or an inherent structural feature of the absolutist reading?',
    'Comparative constitutional analysis: do non-absolutist speech regimes produce measurably less minority systemic oppression, and does the harm derive specifically from the categorical prohibition on content regulation?',
    'If inherent, extractiveness is structurally embedded in the doctrine and cannot be separated from its coordination function; if external, the coordination function is potentially separable from the harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_harm_externality, conceptual, 'Whether minority harm is inseparable from absolutist speech protection').

omega_variable(
    judicial_suppression_source,
    'Is the suppression of minority-protective speech laws driven by active judicial enforcement, or by minorities'' internalized acceptance of the doctrine''s legitimacy?',
    'Post-decision political behavior analysis: do minorities and their legislative allies continue to pursue speech regulation after judicial invalidation (structural suppression), or does mobilization collapse and acceptance follow (internalized suppression)?',
    'If internalized, effective suppression exceeds the structural measure and the constraint''s persistence is partly carried by the targets themselves; if structural, extraction is maintained entirely by active institutional enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_suppression_source, empirical, 'Structural versus internalized suppression mechanism for minority-protective legislation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__absolutist_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t0, first_amendment_speech_protection__absolutist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(firs_tr_t14, first_amendment_speech_protection__absolutist_reading, theater_ratio, 14, 0.18).
narrative_ontology:measurement(firs_tr_t28, first_amendment_speech_protection__absolutist_reading, theater_ratio, 28, 0.22).
narrative_ontology:measurement(firs_tr_t42, first_amendment_speech_protection__absolutist_reading, theater_ratio, 42, 0.25).
narrative_ontology:measurement(firs_tr_t56, first_amendment_speech_protection__absolutist_reading, theater_ratio, 56, 0.26).
narrative_ontology:measurement(firs_tr_t70, first_amendment_speech_protection__absolutist_reading, theater_ratio, 70, 0.25).

% Extraction over time
narrative_ontology:measurement(firs_be_t0, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(firs_be_t14, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 14, 0.5).
narrative_ontology:measurement(firs_be_t28, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 28, 0.6).
narrative_ontology:measurement(firs_be_t42, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 42, 0.68).
narrative_ontology:measurement(firs_be_t56, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 56, 0.7).
narrative_ontology:measurement(firs_be_t70, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 70, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t0, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(firs_su_t14, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 14, 0.5).
narrative_ontology:measurement(firs_su_t28, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 28, 0.65).
narrative_ontology:measurement(firs_su_t42, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 42, 0.75).
narrative_ontology:measurement(firs_su_t56, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 56, 0.77).
narrative_ontology:measurement(firs_su_t70, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 70, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection__harm_limited_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection__categorical_balancing_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the first_amendment_speech_protection kernel. The absolutist reading, harm-limited reading, and categorical-balancing reading are structurally distinct constraints with different epsilon values, beneficiary/victim structures, and failure modes. They form a constraint family linked by shared constitutional text but decomposed per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
