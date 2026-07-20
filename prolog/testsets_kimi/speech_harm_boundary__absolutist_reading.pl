% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__absolutist_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: speech_harm_boundary__absolutist_reading
 *   human_readable: Absolutist Speech Protection Doctrine (High Harm Threshold)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the absolutist reading of the
 *   speech_harm_boundary kernel in constitutional law. It protects speech
 *   near-absolutely, permitting government regulation only within a narrow,
 *   historically fixed set of unprotected categoriesâprincipally
 *   incitement, true threats, defamation, and obscenity. The harm override
 *   threshold is set extremely high, meaning that most offensive, hateful,
 *   harassing, or dignitary-injurious speech remains constitutionally
 *   protected. This reading is contested by two sibling readings: a
 *   harm-balancing reading that permits proportionate regulation to prevent
 *   demonstrated harm, and a dignity reading that categorically excludes
 *   personhood-denying speech from protection.
 *
 * KEY AGENTS:
 *   - Judiciary: agenda-setter (institutional/analytical) â enforces the absolutist doctrine through constitutional review.
 *   - Speakers and media organizations: beneficiaries (organized/constrained) â operate with broad immunity from speech regulation.
 *   - Targets of harmful speech: payers/victims (powerless/trapped) â bear unrecouped costs of protected expression.
 *   - Legislative and executive branches: payers (institutional/constrained) â denied regulatory tools to protect targets.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__absolutist_reading, 0.68).
domain_priors:suppression_score(speech_harm_boundary__absolutist_reading, 0.62).
domain_priors:theater_ratio(speech_harm_boundary__absolutist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__absolutist_reading, "Absolutist Speech Protection Doctrine (High Harm Threshold)").
narrative_ontology:topic_domain(speech_harm_boundary__absolutist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_harm_boundary__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__absolutist_reading, '84470a74-0816-4816-ad94-2d564a572517').
narrative_ontology:cs_kernel_codification('84470a74-0816-4816-ad94-2d564a572517', fixed_text).
narrative_ontology:cs_authority_grounding('84470a74-0816-4816-ad94-2d564a572517', lineage).
narrative_ontology:cs_interpretation_layer_present('84470a74-0816-4816-ad94-2d564a572517').
narrative_ontology:cs_reading_relation('84470a74-0816-4816-ad94-2d564a572517', speech_harm_boundary__harm_balancing_reading, coexists_with).
narrative_ontology:cs_reading_relation('84470a74-0816-4816-ad94-2d564a572517', speech_harm_boundary__dignity_reading, coexists_with).
narrative_ontology:cs_axiom('84470a74-0816-4816-ad94-2d564a572517', foundational, no_abridgment_absolute).
narrative_ontology:cs_axiom_status(no_abridgment_absolute, holdable).
narrative_ontology:cs_axiom_grounding('84470a74-0816-4816-ad94-2d564a572517', no_abridgment_absolute, conventional).
narrative_ontology:cs_axiom('84470a74-0816-4816-ad94-2d564a572517', foundational, speaker_autonomy_trumps_harm).
narrative_ontology:cs_axiom_status(speaker_autonomy_trumps_harm, holdable).
narrative_ontology:cs_axiom_grounding('84470a74-0816-4816-ad94-2d564a572517', speaker_autonomy_trumps_harm, deontological).
narrative_ontology:cs_reference_frame('84470a74-0816-4816-ad94-2d564a572517', constitutional_textual_absolutism).
narrative_ontology:cs_drift_state('84470a74-0816-4816-ad94-2d564a572517', digital_speech_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('84470a74-0816-4816-ad94-2d564a572517', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__absolutist_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, speakers_and_media_organizations).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, targets_of_harmful_speech).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, legislative_and_executive_branches).
narrative_ontology:constraint_vindicates(speech_harm_boundary__absolutist_reading, viewpoint_neutrality_principle).
narrative_ontology:constraint_vindicates(speech_harm_boundary__absolutist_reading, marketplace_of_ideas_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the constitutional speech doctrine, striking down legislation that abridges protected speech and delineating the narrow unprotected categories. Sets the operational boundary between protected expression and regulable harm. Cannot be overridden except by constitutional amendment or subsequent doctrinal reversal.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Publish, broadcast, and disseminate speech with near-immunity from government regulation. Benefit from judicial injunctions against speech-restrictive laws and from the absence of liability for most offensive or injurious expression. Operate within the constitutional framework and cannot exit it, but enjoy broad protection for content that would be regulable under competing readings.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, speakers_and_media_organizations, beneficiary,
    organized, biographical, constrained, national).

% Bear the costs of harmful but constitutionally protected speechâdignitary injury, psychological harm, incitement to private discrimination, targeted harassmentâwithout government recourse because the harm override threshold is set extremely high. Cannot practically exit the public discourse environment where the harm occurs, and cannot obtain legal remedy for most injuries under this reading.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, targets_of_harmful_speech, payer,
    powerless, immediate, trapped, national).

% Enact and enforce laws that touch on speech. Under this reading, most proposed regulationsâsuch as hate speech laws, campaign speech limits, or content-moderation mandatesâare preemptively constrained by judicial review. Must draft narrowly around a historically fixed, small set of unprotected categories and face routine invalidation of novel protective legislation.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, legislative_and_executive_branches, payer,
    institutional, generational, constrained, national).

narrative_ontology:fixing_cost_class(speech_harm_boundary__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents government overreach into public discourse by establishing a bright-line, content-neutral prohibition on most speech regulation, thereby enabling broad democratic participation without fear of viewpoint-based censorship.
% TRANSFER_FUNCTION: Moves the costs of unregulated speechâdignitary harms, psychological injuries, and discriminatory incitementâfrom the state and speakers to the targets of that speech, by foreclosing government remedies that would otherwise shift or internalize those costs.
% ABSENT_VOICES: Targets of sustained but protected hate speech and dehumanizing propaganda who lack recourse outside the narrow unprotected categories; legislators and regulatory bodies whose speech-limiting protective statutes are struck down; comparative human rights frameworks that treat dignity as a counterweight to expression.
% DISAPPEARANCE_RATIONALE: If the absolutist constraint disappeared overnight, legislatures would promptly enact broader speech regulations, media liability regimes would expand, courts would pivot to proportionality balancing, and the public discourse environment would reorganize around a substantially wider set of unprotected harms.
% FOUNDING_PROBLEM: Government suppression of dissenting political speech, viewpoint-based censorship, and prior restraints on publication.
% FOUNDING_PROBLEM_CORROBORATION: Historians and civil liberties organizations attest the founding problem of government censorship was real and persists. Comparative constitutional scholars and international human rights bodies attest that the current absolutist reading exceeds what is necessary to prevent that problem, and that the doctrine now shields speakers at the expense of targets.
narrative_ontology:disappearance_verdict(speech_harm_boundary__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__absolutist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_harm_boundary__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__absolutist_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_harm_boundary__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_harm_boundary__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.68) because the constraint systematically shifts the costs of unregulated speechâpsychological, dignitary, and discriminatory harmsâto targets who lack legal recourse. Suppression is moderate-high (0.62) because the constraint's persistence depends on active judicial nullification of democratically enacted protective legislation, not merely on non-use. Theater ratio is low (0.25) because judicial enforcement is substantive and doctrinally grounded, not performative. Accessibility collapse is moderate (0.60): alternative regulatory frameworks common in comparable democracies are effectively foreclosed within this jurisdiction. Resistance is moderate (0.55) due to persistent scholarly, legislative, and social-movement challenge. The temporal series show rising extraction and enforcement intensity as the absolutist reading solidified and extended to new communicative contexts.
 *
 * PERSPECTIVAL GAP:
 *   The speaker seat experiences the constraint as protective coordination against government censorship, while the target seat experiences it as structural extraction that denies remedy for genuine harm. The judiciary experiences it as faithful enforcement of constitutional text. The legislative seat experiences it as a disabling constraint on democratic policymaking. The engine should compute divergent per-seat classifications from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Speakers and media organizations are declared beneficiaries with constrained exit, placing their directionality near the beneficiary pole. Targets of harmful speech are declared victims with trapped exit, placing their directionality near the full-target pole. The judiciary, as agenda-setter, occupies an administrative position with analytical exit. Legislative branches are payers with constrained institutional exit, yielding a moderate-to-high directionality reflecting their inability to regulate.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling the constraint as a pure snare by capturing its genuine coordination function: it solves a real collective-action problem of preventing government viewpoint discrimination and censorship. It prevents mislabeling as a pure rope by acknowledging the asymmetric extraction: the same structure that coordinates speakers against government overreach imposes unrecouped harm costs on targets. The mandatrophy flag is not triggered because the founding problemâgovernment censorshipâremains contestedly live, even as the current absolutist form may exceed its original justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_imputation_ambiguity,
    'Does the harm borne by targets stem from the legal doctrine itself by denying remedy, or from the underlying speech act independent of any legal framing?',
    'Comparative cross-jurisdictional analysis: if similar speech acts produce similar harm rates in jurisdictions with broader remedies, the doctrine is not the primary causal source of extraction.',
    'If the doctrine causes the harm, extractiveness is intrinsic to the constraint. If the speech act causes it independently, the constraint''s extraction is lower and its classification shifts toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_imputation_ambiguity, empirical, 'Whether target harm is caused by the doctrine or by the speech act itself').

omega_variable(
    absolutist_textual_mandate,
    'Does the constitutional kernel textually mandate near-absolute protection, or does it merely permit that reading among other structurally viable interpretations?',
    'Historical-linguistic analysis of the kernel text and founding-era legal usage; assessment of whether the text underdetermines the reading.',
    'If the text mandates absolutism, the coordination function is tightly coupled to the kernel. If the text underdetermines the reading, the constraint is more constructed and its extraction is harder to distinguish from interpretive choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolutist_textual_mandate, conceptual, 'Whether the kernel text compels the absolutist reading').

omega_variable(
    regulatory_suppression_source,
    'Is the suppression of alternative regulatory frameworks driven primarily by judicial enforcement, or by internalized political culture treating speech regulation as illegitimate regardless of judicial doctrine?',
    'Observe legislative behavior in jurisdictions sharing the political culture but with differing judicial doctrines; if regulation fails even without judicial review, suppression is partially internalized.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure. If purely judicial, suppression tracks enforcement capacity and is more mutable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_suppression_source, empirical, 'Structural vs internalized suppression of speech regulation alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__absolutist_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(speech_abs_tr_t0, speech_harm_boundary__absolutist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(speech_abs_tr_t14, speech_harm_boundary__absolutist_reading, theater_ratio, 14, 0.15).
narrative_ontology:measurement(speech_abs_tr_t28, speech_harm_boundary__absolutist_reading, theater_ratio, 28, 0.18).
narrative_ontology:measurement(speech_abs_tr_t42, speech_harm_boundary__absolutist_reading, theater_ratio, 42, 0.2).
narrative_ontology:measurement(speech_abs_tr_t56, speech_harm_boundary__absolutist_reading, theater_ratio, 56, 0.23).
narrative_ontology:measurement(speech_abs_tr_t70, speech_harm_boundary__absolutist_reading, theater_ratio, 70, 0.25).

% Extraction over time
narrative_ontology:measurement(speech_abs_be_t0, speech_harm_boundary__absolutist_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(speech_abs_be_t14, speech_harm_boundary__absolutist_reading, base_extractiveness, 14, 0.54).
narrative_ontology:measurement(speech_abs_be_t28, speech_harm_boundary__absolutist_reading, base_extractiveness, 28, 0.6).
narrative_ontology:measurement(speech_abs_be_t42, speech_harm_boundary__absolutist_reading, base_extractiveness, 42, 0.64).
narrative_ontology:measurement(speech_abs_be_t56, speech_harm_boundary__absolutist_reading, base_extractiveness, 56, 0.68).
narrative_ontology:measurement(speech_abs_be_t70, speech_harm_boundary__absolutist_reading, base_extractiveness, 70, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(speech_abs_su_t0, speech_harm_boundary__absolutist_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(speech_abs_su_t14, speech_harm_boundary__absolutist_reading, suppression_requirement, 14, 0.52).
narrative_ontology:measurement(speech_abs_su_t28, speech_harm_boundary__absolutist_reading, suppression_requirement, 28, 0.58).
narrative_ontology:measurement(speech_abs_su_t42, speech_harm_boundary__absolutist_reading, suppression_requirement, 42, 0.64).
narrative_ontology:measurement(speech_abs_su_t56, speech_harm_boundary__absolutist_reading, suppression_requirement, 56, 0.68).
narrative_ontology:measurement(speech_abs_su_t70, speech_harm_boundary__absolutist_reading, suppression_requirement, 70, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, speech_harm_boundary__harm_balancing_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, speech_harm_boundary__dignity_reading).

% DUAL FORMULATION NOTE:
% The speech_harm_boundary kernel decomposes into at least three structurally distinct constraints: the absolutist reading (near-absolute protection, high target cost), the harm-balancing reading (presumptive protection yielding to demonstrated harm), and the dignity reading (personhood override). Each reading carries a different epsilon, different beneficiary/victim structures, and different classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
