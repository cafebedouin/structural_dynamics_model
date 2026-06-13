% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__absolutist_reading, []).

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
 *   constraint_id: speech_protection_kernel__absolutist_reading
 *   human_readable: Absolutist Speech Protection (Kernel Reading)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the absolutist reading of the
 *   speech-protection kernel: the First Amendment protects speech
 *   near-categorically, and listener harm—even severe, group-targeted harm—is
 *   not a valid ground for legal restriction. Speech can only be restricted
 *   if it meets narrow categorical exclusions (incitement to imminent lawless
 *   action, true threats, defamation, etc.). The reading emerged from the
 *   Enlightenment principle of speaker autonomy and was crystallized in
 *   20th-century American constitutional doctrine (Brandenburg v. Ohio, New
 *   York Times v. Sullivan). Under this reading, speaker autonomy is
 *   maximized; victim harm claims about hate speech, harassment, and group
 *   subordination are rejected as sufficient grounds for restriction. The
 *   constraint is CLAIMED as tangled_rope (it coordinates a rule-set while
 *   extracting from victims) and the authored metrics support that:
 *   extractiveness rises modestly over the interval as digital platforms
 *   amplify hate speech reach; suppression is high because the boundary's
 *   persistence requires actively resisting pressure from harm-threshold and
 *   dignity readings; theater is moderate-low because the doctrine maintains
 *   real coordination function (preventing majoritarian censorship) alongside
 *   the extraction.
 *
 * KEY AGENTS:
 *   - absolutist_doctrine_adherents (institutional, agenda-setter): maintain the reading through judicial interpretation
 *   - speakers_without_demographic_power (powerless, beneficiary): dissidents and marginalized voices shielded from majoritarian silencing
 *   - institutional_speech_producers (institutional, beneficiary): media, platforms, publishers benefiting from expansive immunity
 *   - targets_of_hate_speech (powerless, payer, identity-locked): members of stigmatized groups bearing psychic and social costs
 *   - harassment_and_threat_victims (moderate, payer, constrained): individuals in sustained abuse campaigns
 *   - subordinated_groups (organized, payer, constrained): women, racial minorities, LGBTQ+ people arguing for equality-based restrictions
 *   - first_amendment_judiciary (institutional, agenda-setter): courts authoritatively interpreting and applying the doctrine
 *   - harm_threshold_advocates (organized, excluded, trapped): legal scholars and advocates arguing for wider harm-based grounds, excluded from doctrinal authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__absolutist_reading, 0.62).
domain_priors:suppression_score(speech_protection_kernel__absolutist_reading, 0.71).
domain_priors:theater_ratio(speech_protection_kernel__absolutist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__absolutist_reading, "Absolutist Speech Protection (Kernel Reading)").
narrative_ontology:topic_domain(speech_protection_kernel__absolutist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_kernel__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__absolutist_reading, '418cea3e-ed53-4cbe-bc6d-a16377e76fc9').
narrative_ontology:cs_kernel_codification('418cea3e-ed53-4cbe-bc6d-a16377e76fc9', fixed_text).
narrative_ontology:cs_authority_grounding('418cea3e-ed53-4cbe-bc6d-a16377e76fc9', lineage).
narrative_ontology:cs_interpretation_layer_present('418cea3e-ed53-4cbe-bc6d-a16377e76fc9').
narrative_ontology:cs_reading_relation('418cea3e-ed53-4cbe-bc6d-a16377e76fc9', speech_protection_kernel__harm_threshold_reading, influences).
narrative_ontology:cs_reading_relation('418cea3e-ed53-4cbe-bc6d-a16377e76fc9', speech_protection_kernel__dignity_reading, influences).
narrative_ontology:cs_reading_relation('418cea3e-ed53-4cbe-bc6d-a16377e76fc9', speech_protection_kernel__democratic_participation_reading, influences).
narrative_ontology:cs_reading_relation('418cea3e-ed53-4cbe-bc6d-a16377e76fc9', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_axiom('418cea3e-ed53-4cbe-bc6d-a16377e76fc9', foundational, speaker_autonomy_trumps_listener_harm).
narrative_ontology:cs_axiom_status(speaker_autonomy_trumps_listener_harm, holdable).
narrative_ontology:cs_axiom_grounding('418cea3e-ed53-4cbe-bc6d-a16377e76fc9', speaker_autonomy_trumps_listener_harm, deontological).
narrative_ontology:cs_axiom('418cea3e-ed53-4cbe-bc6d-a16377e76fc9', foundational, categorical_exclusions_exhaustive).
narrative_ontology:cs_axiom_status(categorical_exclusions_exhaustive, holdable).
narrative_ontology:cs_axiom_grounding('418cea3e-ed53-4cbe-bc6d-a16377e76fc9', categorical_exclusions_exhaustive, deontological).
narrative_ontology:cs_reference_frame('418cea3e-ed53-4cbe-bc6d-a16377e76fc9', speaker_autonomy_constitution).
narrative_ontology:cs_drift_state('418cea3e-ed53-4cbe-bc6d-a16377e76fc9', contemporary_digital_harm_accumulation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('418cea3e-ed53-4cbe-bc6d-a16377e76fc9', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__absolutist_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, speakers_without_demographic_power).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, dissidents_and_marginalized_voices).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, institutional_speech_producers).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, targets_of_hate_speech).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, harassment_victims).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, subordinated_groups).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__absolutist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(speech_protection_kernel__absolutist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_kernel__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.62 at interval end) because the constraint imposes asymmetric costs: targets of hate speech, harassment victims, and subordinated groups bear measurable harm (psychological distress, employment discrimination, epistemic subordination, threat of violence) without legal recourse, while institutional speakers collect immunity. The asymmetry is structural: being a member of a stigmatized group is not a choice (identity-locked), so targets cannot exit. Suppression is high (0.71) because the absolutist reading is judicially entrenched and actively defended against pressure from alternative readings; courts must continuously rule against harm-based restrictions and resist statutory narrowing. The measurement series tracks the constraint's tightening: extractiveness rises as digital platforms amplify hate speech reach and as harm-accumulation accelerates; suppression rises as pressure from harm-threshold and dignity readings intensifies (legislative campaigns, amicus briefs, academic critique) and doctrine must work harder to hold the boundary. Theater is moderate-low (0.28) because the coordination function is real: the reading does prevent majoritarian censorship and protects dissident voices, but an increasing share of the effort goes to defending the boundary against alternative readings rather than to the original coordination problem.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (absolutist scholars, First Amendment judiciary) compute the constraint as a rope or mountain—a necessary coordination against majoritarian suppression. The payer seats (hate speech targets, harassment victims, subordinated groups) compute it as a snare—extraction disguised as freedom principle. The beneficiary seats split: speakers-without-demographic-power benefit genuinely from the protection; institutional speech-producers benefit from immunity but also from the doctrine's prestige and from being positioned as defenders of freedom. The engine should compute a divergence between the agenda-setter reading (higher beneficiary weight, lower victim weight from their structural position) and the victim-seat reading (high extraction, high identity-lock, high subordination cost). This divergence is the measurement the constraint story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality: speakers-without-power (d=0.1-0.25, near beneficiary end) are structurally protected by the boundary; institutional producers (d=0.15-0.35) benefit from liability immunity and from doctrine prestige. Victim directionality: targets of hate speech (d=0.85-1.0, near full-target end) are identity-locked, cannot exit, bear accumulating harm with no legal recourse; harassment victims (d=0.75-0.90) are constrained, cannot avoid platforms where abuse occurs; subordinated groups (d=0.70-0.85) are organized (slightly lower d than atomized targets) but structurally disempowered in the doctrine's authority structure. Agenda-setters (d=0.5, symmetric) maintain the doctrine and experience costs (pressure, critique) and benefits (prestige, theoretical coherence) roughly equally. No directionality overrides needed: the structural derivation from beneficiary/victim + exit options produces the right d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The absolutist reading's founding problem was real: majoritarian governments and powerful groups used censorship laws to suppress dissent. That problem was historically true and explains the doctrine's emergence. The problem's contemporary status is contested: absolutist adherents argue it remains live (governments still attempt censorship, dissidents still face suppression risk). Critics argue the founding problem has been substantially solved—modern democracies have stable speech protections—but the doctrine persists as rent extraction for institutional speakers and structural subordination of targeted groups. The constraint does NOT exhibit classic mandatrophy (where the function has atrophied entirely and only performance remains). Instead, it exhibits FUNCTION DRIFT: the original coordination problem (preventing majoritarian suppression) is still real and important, but the boundary has become decoupled from that problem's solution. The boundary protects speakers-without-power (which it should), but it also protects hate speech, harassment, and group subordination (which the original problem did not address). This is tangled_rope territory, not piton: real coordination function + asymmetric extraction + active enforcement = coordination that has been contaminated by extraction, not coordination replaced by performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    speaker_autonomy_vs_victim_equality,
    'Are speaker autonomy and victim equality from speech-based harm fundamentally irreconcilable, or can a narrower speech-protection boundary serve both?',
    'Comparative constitutional analysis: jurisdictions with narrower hate-speech and harassment restrictions (Canada, Germany, UK) show whether speech protection and victim protection can coexist. Structural analysis of how the absolute boundary affects speakers-without-power: do marginalized speakers benefit from absolute protection, or do they benefit more from a boundary that excludes hate speech and subordination?',
    'If fundamentally irreconcilable, the constraint is a genuine structural choice (tragedy: you cannot have both maximum speaker autonomy AND victim protection from group harm). If separable, the absolute boundary is a false necessity—the reading has chosen speaker autonomy over victim equality, making it a values choice, not a logical imperative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speaker_autonomy_vs_victim_equality, conceptual, 'Whether the absolutist boundary''s beneficiaries (speaker autonomy) and its harms (victim vulnerability) are structurally inseparable or a contingent policy choice.').

omega_variable(
    hate_speech_amplification_feedback,
    'Does digital platform amplification of hate speech change the structural relationship between the absolutist reading and its victims, such that the same boundary produces different harms in a networked-speech environment than in a pre-digital context?',
    'Temporal analysis: measure hate-speech targeting, doxing, and harassment-campaign severity before and after digital-platform scaling. Compare jurisdictions with different speech restrictions and identical platform saturation to isolate the reading''s effect. Structural analysis: does the networked-feedback loop (slur → algorithmic amplification → coordinated pile-on → harassment campaign) constitute a category difference from individual hate speech in the pre-digital era?',
    'If yes: the absolutist reading''s extraction has increased as a side effect of technological change—the boundary is the same but the harms accumulate faster. The reading may be sustainable in low-amplification contexts but extractive in high-amplification contexts (suggests conditional validity). If no: the boundary produces harm independently of amplification mechanism—the reading''s extraction is structural to the doctrine itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hate_speech_amplification_feedback, empirical, 'Whether digital amplification changes the harm-accumulation properties of the absolutist boundary.').

omega_variable(
    identity_locked_exit_ambiguity,
    'How much of the suppression experienced by hate-speech targets (identity-locked exit_options) is due to the absolutist boundary itself, and how much is due to the underlying group subordination that the boundary merely fails to address?',
    'Counterfactual analysis: in jurisdictions with harm-threshold or dignity-based restrictions on hate speech, do targets of hate speech exit the public sphere less, or does exit remain high because the underlying group subordination is unchanged? Structural analysis: is the identity-lock itself (being visibly a member of a stigmatized group) the primary suppression mechanism, with the speech protection boundary as a secondary factor?',
    'If the boundary is the primary suppression mechanism: restricting hate speech would materially increase targets'' public participation (the constraint is actively extractive). If the underlying subordination is primary: targets would remain suppressed even with speech restrictions because their group subordination persists through non-speech channels (the constraint is a symptom, not the cause, of their subordination, and extraction attribution is partly misplaced).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_exit_ambiguity, empirical, 'Attribution ambiguity: how much of targets'' suppression and exit is caused by the speech-protection reading versus the structural subordination it exists alongside.').

omega_variable(
    categorical_exclusion_drift,
    'Over time, have the judicial interpretations of the narrow categorical exclusions (incitement, true threats, defamation) been narrowed or maintained, and what role does the absolutist reading play in that drift?',
    'Doctrinal history: analyze landmark cases (Brandenburg, New York Times v. Sullivan, Virginia v. Black) and their application trajectory. Measure: (1) how courts have applied the incitement standard (raising the bar for what counts as incitement); (2) how defamation doctrine evolved (New York Times standard raising bar for liability); (3) how threat doctrine changed (requiring specificity and imminence). Determine whether the absolutist reading drives the narrowing or merely rationalizes it.',
    'If the absolutist reading actively drives narrowing: the doctrine continuously tightens the exclusions, making the protection increasingly absolute (extraction increases over time, theater increases as enforcement effort rises). If the reading is neutral to doctrinal drift: the narrowing is driven by courts'' deference to speech and institutional speakers, and the reading is a post-hoc justification. In either case, understanding the drift direction helps predict the constraint''s trajectory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(categorical_exclusion_drift, empirical, 'Whether the absolutist reading drives or rationalizes the historical narrowing of categorical exclusions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__absolutist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__absolutist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(spee_tr_t5, speech_protection_kernel__absolutist_reading, theater_ratio, 5, 0.21).
narrative_ontology:measurement(spee_tr_t10, speech_protection_kernel__absolutist_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(spee_tr_t15, speech_protection_kernel__absolutist_reading, theater_ratio, 15, 0.26).
narrative_ontology:measurement(spee_tr_t22, speech_protection_kernel__absolutist_reading, theater_ratio, 22, 0.27).
narrative_ontology:measurement(spee_tr_t30, speech_protection_kernel__absolutist_reading, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__absolutist_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(spee_be_t5, speech_protection_kernel__absolutist_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(spee_be_t10, speech_protection_kernel__absolutist_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement(spee_be_t15, speech_protection_kernel__absolutist_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(spee_be_t22, speech_protection_kernel__absolutist_reading, base_extractiveness, 22, 0.62).
narrative_ontology:measurement(spee_be_t30, speech_protection_kernel__absolutist_reading, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__absolutist_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(spee_su_t5, speech_protection_kernel__absolutist_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(spee_su_t10, speech_protection_kernel__absolutist_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(spee_su_t15, speech_protection_kernel__absolutist_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(spee_su_t22, speech_protection_kernel__absolutist_reading, suppression_requirement, 22, 0.71).
narrative_ontology:measurement(spee_su_t30, speech_protection_kernel__absolutist_reading, suppression_requirement, 30, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__absolutist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_kernel__absolutist_reading, 0.12).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__democratic_participation_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__marketplace_reading).

% DUAL FORMULATION NOTE:
% The speech-protection kernel comprises five structurally distinct constraint stories, each instantiating a different reading of the same contested kernel. The absolutist reading defines the widest protection boundary (speech near-categorical, listener harm not grounds for restriction). This reading INFLUENCES the harm-threshold, dignity, democratic-participation, and marketplace readings by establishing the maximal protection position that other readings react against and narrow. All five stories are linked via network.affects_constraints; each should include a parallel dual_formulation_note naming its siblings. The ε values differ substantially across readings: absolutist and marketplace readings have moderate-to-high extraction (speaker/institutional benefits, victim costs); harm-threshold, dignity, and democratic-participation readings have lower extraction (narrower protection, victim protection incorporated). These are NOT readings of the same constraint with different metrics; they are different constraints with different ε values rooted in different boundary definitions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_kernel__absolutist_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
