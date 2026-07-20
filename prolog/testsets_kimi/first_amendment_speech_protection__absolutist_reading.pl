% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: first_amendment_speech_protection__absolutist_reading
 *   human_readable: First Amendment Absolutist Reading â Categorical Speech Protection
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The absolutist reading of the First Amendment holds that the text
 *   'Congress shall make no law... abridging the freedom of speech'
 *   establishes categorical protection: government may not regulate speech
 *   except within narrow historical categories such as incitement, obscenity,
 *   and fighting words. This reading coordinates broad speech protection but
 *   asymmetrically extracts from targeted minorities, who bear systemic costs
 *   of unregulated hate speech and harassment that democratic majorities are
 *   prevented from restricting. The constraint is a commitment-system reading
 *   of a fixed constitutional text, enforced by judicial review. This story
 *   instantiates the absolutist reading as one structurally distinct
 *   constraint within the First Amendment kernel family; the harm-limited and
 *   categorical-balancing readings are separate constraints.
 *
 * KEY AGENTS:
 *   - judiciary (institutional/analytical): agenda-setter enforcing the absolutist reading through judicial review
 *   - majority_speakers (powerful/mobile): beneficiaries of maximal speech protection shielded from regulation
 *   - targeted_minorities (powerless/identity_locked): payers bearing systemic costs of unregulated targeted speech
 *   - regulation_advocates (organized/constrained): excluded from judicial success by the categorical rule
 *   - constitutional_scholars (analytical/analytical): observers analyzing doctrinal coherence and distributional effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__absolutist_reading, 0.72).
domain_priors:suppression_score(first_amendment_speech_protection__absolutist_reading, 0.78).
domain_priors:theater_ratio(first_amendment_speech_protection__absolutist_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__absolutist_reading, "First Amendment Absolutist Reading â Categorical Speech Protection").
narrative_ontology:topic_domain(first_amendment_speech_protection__absolutist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__absolutist_reading, '5eeeaeb5-18dc-47f7-a110-8b7095d807f6').
narrative_ontology:cs_kernel_codification('5eeeaeb5-18dc-47f7-a110-8b7095d807f6', fixed_text).
narrative_ontology:cs_authority_grounding('5eeeaeb5-18dc-47f7-a110-8b7095d807f6', lineage).
narrative_ontology:cs_interpretation_layer_present('5eeeaeb5-18dc-47f7-a110-8b7095d807f6').
narrative_ontology:cs_reading_relation('5eeeaeb5-18dc-47f7-a110-8b7095d807f6', first_amendment_speech_protection__harm_limited_reading, forecloses).
narrative_ontology:cs_reading_relation('5eeeaeb5-18dc-47f7-a110-8b7095d807f6', first_amendment_speech_protection__categorical_balancing_reading, forecloses).
narrative_ontology:cs_axiom('5eeeaeb5-18dc-47f7-a110-8b7095d807f6', foundational, no_abridgment_absolute).
narrative_ontology:cs_axiom_status(no_abridgment_absolute, holdable).
narrative_ontology:cs_axiom_grounding('5eeeaeb5-18dc-47f7-a110-8b7095d807f6', no_abridgment_absolute, conventional).
narrative_ontology:cs_reference_frame('5eeeaeb5-18dc-47f7-a110-8b7095d807f6', constitutional_textualism).
narrative_ontology:cs_drift_state('5eeeaeb5-18dc-47f7-a110-8b7095d807f6', contemporary_speech_regulation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5eeeaeb5-18dc-47f7-a110-8b7095d807f6', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, majority_speakers).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, targeted_minorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the absolutist reading through judicial review, striking down speech regulations that exceed narrow historical exclusions. Controls the doctrinal boundary of protected speech and maintains the categorical framework through precedent and constitutional interpretation.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Benefit from maximal constitutional protection of expressive activity, including speech that may harass or intimidate minorities. Their expressive costs are subsidized by the constraint's prohibition on harm-based regulation, and they face no regulatory exit penalty.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, majority_speakers, beneficiary,
    powerful, biographical, mobile, national).

% Bear the systemic costs of unregulated hate speech, targeted harassment, and intimidation that the absolutist reading prevents legislatures from restricting. Their minority identity is the target of the unregulated speech, and they cannot exit the identity or easily exit the jurisdiction.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, targeted_minorities, payer,
    powerless, biographical, identity_locked, national).

% Civil rights and anti-discrimination organizations that advocate for hate speech laws and targeted harassment regulation are structurally excluded from judicial success. Their preferred regulatory frameworks are foreclosed by the categorical rule before reaching any balancing analysis.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, regulation_advocates, excluded,
    organized, generational, constrained, national).

% Analyze the historical pedigree, textual coherence, and distributional consequences of the absolutist reading without bearing its costs or receiving its benefits. They document the gap between the categorical doctrine and comparative constitutional practice.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents government censorship by categorically prohibiting laws that abridge freedom of speech, establishing a broad protected sphere for public expression with only narrow historical exceptions.
% TRANSFER_FUNCTION: Moves the cost of unregulated speech from government and majority speakers to targeted minorities, who bear systemic harassment and intimidation that democratic majorities are prevented from restricting.
% ABSENT_VOICES: Legislators and advocacy groups seeking harm-based speech regulation are structurally excluded; their arguments for hate speech codes and targeted harassment laws are foreclosed by the categorical rule before reaching judicial balancing.
% DISAPPEARANCE_RATIONALE: If the absolutist reading vanished overnight, legislatures would enact hate speech regulations, campus speech codes, and targeted harassment laws currently foreclosed; judicial doctrine would reorganize around balancing tests, and the distribution of expressive risk would shift from minorities to regulatory institutions.
% FOUNDING_PROBLEM: Colonial and early American government suppression of political dissent, religious speech, and seditious libel created a demand for categorical constitutional limits on speech regulation.
% FOUNDING_PROBLEM_CORROBORATION: Historical records from the ratification era corroborate anti-censorship concerns. However, contemporary civil rights organizations outside the absolutist tradition attest that government censorship of political dissent is no longer the primary threat, and the absolutist reading now externalizes costs onto minorities that the founding generation did not face.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__absolutist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.72) is high because the absolutist reading prevents legislatures from addressing speech-based harms to minorities, externalizing those costs as the nominal price of liberty. Suppression (0.78) is higher because the constraint's persistence depends on courts actively striking down democratically enacted speech regulations that fall outside narrow historical exclusions. Theater ratio (0.35) reflects that judicial opinions perform textual fidelity and historical analysis while systematically producing distributional consequences favoring majority speakers. Accessibility collapse (0.82) is high because once the absolutist frame is accepted, harm-based regulatory alternatives are constitutionally foreclosed. Resistance (0.55) reflects persistent scholarly critique and minority advocacy that has not dislodged the doctrine. Measurements share a single time grid.
 *
 * PERSPECTIVAL GAP:
 *   The judiciary and majority speakers experience the constraint as protecting fundamental liberty and preventing government overreach. Targeted minorities experience the same constraint as disabling the democratic mechanisms that could protect them from systemic oppression. The engine computes this divergence from structural data: the beneficiary seat (majority_speakers) has mobile exit and powerful status, while the payer seat (targeted_minorities) is identity-locked and powerless.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary (agenda_setter, institutional, analytical exit) derives low directionality as administrator, though it does not personally collect extraction. Majority speakers (beneficiary, powerful, mobile) derive near-beneficiary directionality because the constraint subsidizes their expressive activity by shielding it from regulation. Targeted minorities (payer, powerless, identity_locked) derive near-target directionality because the constraint specifically prevents regulation of speech that targets their identity. Regulation advocates (excluded, organized, constrained) sit near target but are outside the direct extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The absolutist reading prevents mandatrophy mislabeling by acknowledging the genuine coordination functionâpreventing government censorshipâwhile documenting the asymmetric extraction that falls on targeted minorities. Without the coordination function, this would be a snare; without the victim group, it would be mislabeled as a rope. The Tangled Rope classification captures both the real liberty interest and the real distributional cost, preventing either pure-coordination or pure-extraction misclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    absolutism_grounding_ambiguity,
    'Is the absolutist reading grounded solely in the constitutional text as enacted positive law, or does it rely on an extratextual deontological commitment to speech as an absolute right?',
    'Historical analysis of ratification debates and early judicial practice; if the text alone supports the reading without natural-rights supplementation, the grounding is conventional.',
    'A conventional grounding makes the reading vulnerable to historical evidence that the original understanding was not absolutist; a deontological grounding insulates it from historical challenge but changes the nature of the commitment system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolutism_grounding_ambiguity, conceptual, 'Whether the absolutist reading rests on textual positivism or natural rights').

omega_variable(
    harm_externalization_vs_necessary_cost,
    'Is the systemic harm borne by targeted minorities an unavoidable cost of genuine liberty, or an asymmetric extraction preventable without collapsing speech protection?',
    'Comparative constitutional analysis of jurisdictions with hate speech laws that maintain robust political speech protection (e.g., Canada, Germany); functional comparison of democratic discourse quality and minority safety metrics.',
    'If alternative frameworks protect speech while reducing minority harm, the constraint''s extraction exceeds its coordination value, pushing classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_externalization_vs_necessary_cost, empirical, 'Whether minority harm is a necessary cost of speech protection or extractive surplus').

omega_variable(
    narrow_exclusions_expansion,
    'Will the narrow historical exclusions framework expand to accommodate new harms, or will the absolutist reading foreclose all new categories of speech regulation?',
    'Track Supreme Court decisions over the next decade for recognition of new categorical exclusions or explicit rejections of harm-based categories.',
    'Refusal to recognize new exclusions raises suppression and extractiveness; expansion drifts the reading toward categorical balancing or harm-limitation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(narrow_exclusions_expansion, empirical, 'Whether the absolutist reading will remain categorical or drift toward balancing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__absolutist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t0, first_amendment_speech_protection__absolutist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(firs_tr_t10, first_amendment_speech_protection__absolutist_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(firs_tr_t20, first_amendment_speech_protection__absolutist_reading, theater_ratio, 20, 0.23).
narrative_ontology:measurement(firs_tr_t30, first_amendment_speech_protection__absolutist_reading, theater_ratio, 30, 0.27).
narrative_ontology:measurement(firs_tr_t40, first_amendment_speech_protection__absolutist_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(firs_tr_t50, first_amendment_speech_protection__absolutist_reading, theater_ratio, 50, 0.33).
narrative_ontology:measurement(firs_tr_t60, first_amendment_speech_protection__absolutist_reading, theater_ratio, 60, 0.35).

% Extraction over time
narrative_ontology:measurement(firs_be_t0, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(firs_be_t10, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(firs_be_t20, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(firs_be_t30, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(firs_be_t40, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(firs_be_t50, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 50, 0.7).
narrative_ontology:measurement(firs_be_t60, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 60, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t0, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(firs_su_t10, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(firs_su_t20, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(firs_su_t30, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(firs_su_t40, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(firs_su_t50, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 50, 0.76).
narrative_ontology:measurement(firs_su_t60, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 60, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, harm_limited_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, categorical_balancing_reading).

% DUAL FORMULATION NOTE:
% The first_amendment_speech_protection kernel decomposes into three structurally distinct constraints per the epsilon-invariance principle. The absolutist reading (categorical protection, high extraction via minority harm externalization), harm_limited_reading (protection yields to demonstrated harm), and categorical_balancing_reading (case-by-case value balancing) share the same constitutional text but instantiate different constraints with different epsilon values, beneficiary structures, and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
