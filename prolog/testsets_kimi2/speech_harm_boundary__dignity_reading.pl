% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__dignity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__dignity_reading, []).

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
 *   constraint_id: speech_harm_boundary__dignity_reading
 *   human_readable: Dignity-Supremacy Speech Exclusion (Dignity Reading)
 *   domain: constitutional/law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the dignity_reading of the
 *   speech_harm_boundary kernel: a constitutional arrangement in which human
 *   dignity is hierarchically superior to freedom of expression, rendering
 *   personhood-denying speech (Holocaust denial, hate speech, group
 *   defamation) categorically unprotected. Rooted in post-war constitutional
 *   settlements (notably the German Basic Law), the constraint extracts
 *   heavily from speakers of excluded expression while coordinating the
 *   protection of targeted identity groups. It is claimed as a tangled rope
 *   â genuine coordination against dehumanization coupled with asymmetric
 *   extraction from disfavored speakers â and the metrics are authored
 *   independently to reflect that structural diagnosis.
 *
 * KEY AGENTS:
 *   - Constitutional authority: agenda-setter (institutional/constrained) â interprets and enforces the dignity-speech hierarchy
 *   - Targeted identity groups: beneficiary (organized/identity_locked) â receive state-backed shielding from personhood-denying expression
 *   - Speakers of excluded speech: primary payer (moderate/constrained) â bear categorical criminal and civil penalties
 *   - Chilled researchers: secondary payer (moderate/constrained) â face self-censorship and material seizure risks
 *   - Free speech advocates: observer (organized/analytical) â challenge the structural asymmetry from outside the beneficiary set
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__dignity_reading, 0.78).
domain_priors:suppression_score(speech_harm_boundary__dignity_reading, 0.72).
domain_priors:theater_ratio(speech_harm_boundary__dignity_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__dignity_reading, "Dignity-Supremacy Speech Exclusion (Dignity Reading)").
narrative_ontology:topic_domain(speech_harm_boundary__dignity_reading, "constitutional/law/political_philosophy").

domain_priors:requires_active_enforcement(speech_harm_boundary__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__dignity_reading, 'd66c1eae-b3ab-41b8-9284-1e83ee8fde4b').
narrative_ontology:cs_kernel_codification('d66c1eae-b3ab-41b8-9284-1e83ee8fde4b', fixed_text).
narrative_ontology:cs_authority_grounding('d66c1eae-b3ab-41b8-9284-1e83ee8fde4b', lineage).
narrative_ontology:cs_interpretation_layer_present('d66c1eae-b3ab-41b8-9284-1e83ee8fde4b').
narrative_ontology:cs_reading_relation('d66c1eae-b3ab-41b8-9284-1e83ee8fde4b', speech_harm_boundary__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('d66c1eae-b3ab-41b8-9284-1e83ee8fde4b', speech_harm_boundary__harm_balancing_reading, coexists_with).
narrative_ontology:cs_axiom('d66c1eae-b3ab-41b8-9284-1e83ee8fde4b', foundational, human_dignity_inviolable_absolute).
narrative_ontology:cs_axiom_status(human_dignity_inviolable_absolute, holdable).
narrative_ontology:cs_axiom_grounding('d66c1eae-b3ab-41b8-9284-1e83ee8fde4b', human_dignity_inviolable_absolute, deontological).
narrative_ontology:cs_axiom('d66c1eae-b3ab-41b8-9284-1e83ee8fde4b', foundational, personhood_denial_forfeits_speech_protection).
narrative_ontology:cs_axiom_status(personhood_denial_forfeits_speech_protection, holdable).
narrative_ontology:cs_axiom_grounding('d66c1eae-b3ab-41b8-9284-1e83ee8fde4b', personhood_denial_forfeits_speech_protection, deontological).
narrative_ontology:cs_reference_frame('d66c1eae-b3ab-41b8-9284-1e83ee8fde4b', dignity_supremacy_framework).
narrative_ontology:cs_drift_state('d66c1eae-b3ab-41b8-9284-1e83ee8fde4b', digital_globalization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d66c1eae-b3ab-41b8-9284-1e83ee8fde4b', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__dignity_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, targeted_identity_groups).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, speakers_of_excluded_speech).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, chilled_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the constitutional hierarchy that subordinates freedom of expression to human dignity, validating categorical exclusions for personhood-denying speech. Bound by constitutional text and precedent, but possesses the final say over the speech-dignity boundary through judicial review and statutory oversight.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, constitutional_authority, agenda_setter,
    institutional, generational, constrained, national).

% Rely on state enforcement to shield them from public expression that denies their personhood or group dignity. Their equal civic status is asserted to depend on the absence of such speech. They cannot exit the identity that makes them targets, so their benefit is structurally locked to the constraint's operation.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, targeted_identity_groups, beneficiary,
    organized, biographical, identity_locked, national).

% Face criminal penalties, censorship, and civil liability for expressing categories of speech judged to deny personhood to protected groups. Remaining in the jurisdiction requires silence on these topics; leaving entails full cultural and civic displacement.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, speakers_of_excluded_speech, payer,
    moderate, immediate, constrained, national).

% Engage in scholarly or journalistic inquiry that risks crossing into prohibited expression categories; face seized materials, prosecution threats, and self-censorship despite absence of intent to dehumanize.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, chilled_researchers, payer,
    moderate, biographical, constrained, national).

% Monitor and legally challenge dignity-based speech restrictions, arguing that categorical exclusions empower state overreach and chill legitimate discourse. They neither bear the direct cost of exclusion nor receive dignity protection but analyze the structural asymmetry.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, free_speech_advocates, observer,
    organized, generational, analytical, continental).

narrative_ontology:fixing_cost_class(speech_harm_boundary__dignity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protects the dignity and equal civic standing of targeted identity groups by removing personhood-denying expression from public discourse, preventing the erosion of social standing through verbal degradation and symbolically reaffirming constitutional equality.
% TRANSFER_FUNCTION: Moves the ability to speak certain categories of expression from individual speakers to state disciplinary control, substituting dignity protection for speech freedom in the constitutional hierarchy.
% ABSENT_VOICES: Absolutist free speech advocates and scholars in jurisdictions without dignity override are formally present in legal proceedings but structurally lose; dissident speakers in the jurisdiction itself are procedurally heard but substantively excluded by the categorical rule.
% DISAPPEARANCE_RATIONALE: If the dignity-subordination rule vanished overnight, previously censored speech would enter public discourse, targeted groups would lose state-backed shielding from personhood-denying attacks, and constitutional jurisprudence would reorganize around either absolutist or balancing frameworks.
% FOUNDING_PROBLEM: Post-totalitarian and post-genocide societies faced the question of how to prevent the recurrence of ideological dehumanization that had enabled state violence, seeking to dignify historically persecuted groups through constitutional law.
% FOUNDING_PROBLEM_CORROBORATION: Targeted identity groups and human rights institutions attest the problem remains live. Free speech advocates and comparative constitutional scholars from outside the dignity-jurisdiction framework attest the founding evil is addressed through other means and the arrangement persists as asymmetric restriction; historical-institute reports from non-beneficiary jurisdictions corroborate that dehumanization recurs through structural factors beyond speech.
narrative_ontology:disappearance_verdict(speech_harm_boundary__dignity_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__dignity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__dignity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_harm_boundary__dignity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__dignity_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__dignity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_harm_boundary__dignity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_harm_boundary__dignity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because categorical exclusion removes entire categories of expression from constitutional protection, transferring speech capacity to state disciplinary control. Suppression is high (0.72) because persistence requires active judicial, administrative, and platform-level enforcement to remove prohibited expression and deter speakers. Theater ratio is moderate (0.32) because the dignity-protection function is largely genuine, but enforcement against global digital speech generates performative over-removal and symbolic prosecutions that exceed functional necessity. Accessibility collapse (0.65) is substantial within dignity-supremacy jurisdictions, where the absolutist alternative becomes constitutionally unthinkable, though cross-jurisdictional visibility prevents total collapse. Resistance (0.55) reflects sustained legal and political pushback from free speech advocates, dissident scholars, and transnational platform interests.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (speakers of excluded speech, chilled researchers) experience the constraint as pure extraction: their expression is criminalized regardless of intent or proportionality. The beneficiary seat (targeted identity groups) experiences it as protective coordination that secures equal civic standing. The agenda-setter seat (constitutional authority) experiences it as the enforcement of a foundational constitutional value. The engine will compute high directionality (near 1.0) for the payer seats and low directionality for the beneficiary seat, producing divergent per-seat classifications despite the single constraint structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Targeted identity groups are declared beneficiaries and carry identity_locked exit, situating them at the beneficiary end of the directionality spectrum. Speakers of excluded speech and chilled researchers are declared victims (payers) with constrained exit, situating them near full target. The constitutional authority is not declared as beneficiary or victim; its directionality reverts to the institutional power atom fallback, but structurally it administers extraction rather than receiving it.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy mislabeling by requiring both a genuine coordination function (dignity protection) and asymmetric extraction (categorical speech suppression) for the tangled rope classification. If the dignity-protection story were cover for state censorship without protected-group benefit, it would classify as snare; if speech were restricted only to the extent empirically necessary to prevent harm (proportionality), it would approach scaffold or rope. The categorical nature of the exclusion, coupled with the existence of identifiable payer seats, keeps the classification in tangled rope rather than collapsing to either coordination or extraction pole.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dignity_ban_efficacy,
    'Do categorical exclusions for dignity-violating speech reduce social dehumanization and protect targeted groups, or do they displace expression and generate backlash?',
    'Cross-jurisdictional longitudinal studies comparing dignity-based regimes with balancing regimes and absolutist regimes on metrics of hate crime, group subjective security, and extremist mobilization.',
    'If bans are ineffective or counterproductive, the coordination story weakens and the constraint shifts toward snare-like pure extraction. If effective, the tangled rope classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dignity_ban_efficacy, empirical, 'Empirical efficacy of dignity-based speech bans').

omega_variable(
    chilling_effect_scope,
    'Does the categorical exclusion of personhood-denying speech chill legitimate historical research, satire, and artistic expression beyond the intended target?',
    'Content analysis of censored materials and interviews with historians, artists, and journalists in dignity-supremacy jurisdictions.',
    'A broad chilling effect would increase the victim set and push epsilon higher; a tightly contained effect supports the current metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chilling_effect_scope, empirical, 'Scope of chilling effect beyond targeted speech').

omega_variable(
    cs_framing_alternative,
    'Is this constraint better framed as an enforcement mechanism governing speech or as an identity-coordination mechanism policing group membership boundaries?',
    'Comparative analysis of whether the constraint''s primary failure mode is enforcement collapse or boundary dissolution of protected group status.',
    'If identity_coordination is the dominant frame, Boltzmann coupling thresholds differ and the floor adjusts; this changes excess-extraction calculations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_alternative, conceptual, 'Alternative CS framing as identity coordination vs enforcement').

omega_variable(
    kernel_reading_sibling_delta,
    'How would the structural classification change if the absolutist or harm-balancing reading of the same kernel were adopted instead of the dignity reading?',
    'Generate sibling constraint stories and compare epsilon, beneficiary/victim sets, and computed per-seat types across the kernel family.',
    'The dignity reading produces a high-extraction, high-suppression constraint with speakers as payers; sibling readings would redistribute directionalities and reduce extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_sibling_delta, conceptual, 'Sibling reading structural delta for speech harm boundary kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__dignity_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_harm_boundary__dignity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(spee_tr_t14, speech_harm_boundary__dignity_reading, theater_ratio, 14, 0.2).
narrative_ontology:measurement(spee_tr_t28, speech_harm_boundary__dignity_reading, theater_ratio, 28, 0.25).
narrative_ontology:measurement(spee_tr_t42, speech_harm_boundary__dignity_reading, theater_ratio, 42, 0.3).
narrative_ontology:measurement(spee_tr_t56, speech_harm_boundary__dignity_reading, theater_ratio, 56, 0.32).
narrative_ontology:measurement(spee_tr_t70, speech_harm_boundary__dignity_reading, theater_ratio, 70, 0.32).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_harm_boundary__dignity_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(spee_be_t14, speech_harm_boundary__dignity_reading, base_extractiveness, 14, 0.6).
narrative_ontology:measurement(spee_be_t28, speech_harm_boundary__dignity_reading, base_extractiveness, 28, 0.68).
narrative_ontology:measurement(spee_be_t42, speech_harm_boundary__dignity_reading, base_extractiveness, 42, 0.72).
narrative_ontology:measurement(spee_be_t56, speech_harm_boundary__dignity_reading, base_extractiveness, 56, 0.76).
narrative_ontology:measurement(spee_be_t70, speech_harm_boundary__dignity_reading, base_extractiveness, 70, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_harm_boundary__dignity_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(spee_su_t14, speech_harm_boundary__dignity_reading, suppression_requirement, 14, 0.58).
narrative_ontology:measurement(spee_su_t28, speech_harm_boundary__dignity_reading, suppression_requirement, 28, 0.65).
narrative_ontology:measurement(spee_su_t42, speech_harm_boundary__dignity_reading, suppression_requirement, 42, 0.7).
narrative_ontology:measurement(spee_su_t56, speech_harm_boundary__dignity_reading, suppression_requirement, 56, 0.73).
narrative_ontology:measurement(spee_su_t70, speech_harm_boundary__dignity_reading, suppression_requirement, 70, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__dignity_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is the dignity_reading of the speech_harm_boundary kernel, decomposed from the absolutist and harm_balancing readings due to structurally distinct epsilon values, beneficiary/victim distributions, and axiomatic foundations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
