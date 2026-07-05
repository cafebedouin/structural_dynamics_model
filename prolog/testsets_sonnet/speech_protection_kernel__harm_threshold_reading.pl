% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__harm_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__harm_threshold_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: speech_protection_kernel__harm_threshold_reading
 *   human_readable: Harm-Threshold Reading of Speech Protection (Victim Harm Overrides Speaker Autonomy)
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   This story instantiates the harm_threshold_reading of the
 *   speech_protection_kernel: speech protection is not categorical but
 *   conditional on the absence of demonstrable harm to identifiable victims.
 *   Where a harm showing clears an evidentiary threshold, the doctrine treats
 *   the victim's injury claim as overriding the speaker's expressive
 *   interest, restricting the protection boundary relative to absolutist or
 *   marketplace readings and expanding the category of unprotected speech.
 *   This is one reading among several live in constitutional discourse; the
 *   sibling readings (absolutist, marketplace, dignity,
 *   democratic-participation) are separate constraints with their own ε
 *   values, not alternative measurements of this one. The genuine
 *   coordination function (giving injured parties recourse) and the
 *   asymmetric extraction (discretionary threshold application falling
 *   hardest on powerless dissident speakers) coexist in the same doctrinal
 *   machinery, which is why this reading is authored as tangled_rope rather
 *   than a pure rope or pure snare.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__harm_threshold_reading, 0.42).
domain_priors:suppression_score(speech_protection_kernel__harm_threshold_reading, 0.51).
domain_priors:theater_ratio(speech_protection_kernel__harm_threshold_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, suppression_requirement, 0.51).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__harm_threshold_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__harm_threshold_reading, "Harm-Threshold Reading of Speech Protection (Victim Harm Overrides Speaker Autonomy)").
narrative_ontology:topic_domain(speech_protection_kernel__harm_threshold_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__harm_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__harm_threshold_reading, '69c0e88c-8591-4621-9f8f-8a945eedfbe8').
narrative_ontology:cs_kernel_codification('69c0e88c-8591-4621-9f8f-8a945eedfbe8', distributed).
narrative_ontology:cs_authority_grounding('69c0e88c-8591-4621-9f8f-8a945eedfbe8', distributed).
narrative_ontology:cs_reading_relation('69c0e88c-8591-4621-9f8f-8a945eedfbe8', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('69c0e88c-8591-4621-9f8f-8a945eedfbe8', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('69c0e88c-8591-4621-9f8f-8a945eedfbe8', speech_protection_kernel__dignity_reading, influences).
narrative_ontology:cs_reading_relation('69c0e88c-8591-4621-9f8f-8a945eedfbe8', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('69c0e88c-8591-4621-9f8f-8a945eedfbe8', foundational, demonstrable_victim_harm_defeats_speaker_autonomy).
narrative_ontology:cs_axiom_status(demonstrable_victim_harm_defeats_speaker_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('69c0e88c-8591-4621-9f8f-8a945eedfbe8', demonstrable_victim_harm_defeats_speaker_autonomy, empirically_contingent).
narrative_ontology:cs_axiom('69c0e88c-8591-4621-9f8f-8a945eedfbe8', secondary, harm_is_individually_adjudicable_via_evidentiary_threshold).
narrative_ontology:cs_axiom_status(harm_is_individually_adjudicable_via_evidentiary_threshold, holdable).
narrative_ontology:cs_axiom_grounding('69c0e88c-8591-4621-9f8f-8a945eedfbe8', harm_is_individually_adjudicable_via_evidentiary_threshold, instrumental).
narrative_ontology:cs_reference_frame('69c0e88c-8591-4621-9f8f-8a945eedfbe8', categorical_speech_liberty_baseline).
narrative_ontology:cs_drift_state('69c0e88c-8591-4621-9f8f-8a945eedfbe8', contemporary_networked_harassment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('69c0e88c-8591-4621-9f8f-8a945eedfbe8', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, documented_harm_claimants).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, targeted_minority_communities).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, harm_adjudication_bodies).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, controversial_speakers).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, dissident_political_commentators).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, satirists_and_provocateurs).
narrative_ontology:constraint_vindicates(speech_protection_kernel__harm_threshold_reading, speech_can_constitute_material_injury).
narrative_ontology:constraint_vindicates(speech_protection_kernel__harm_threshold_reading, unregulated_speech_can_produce_asymmetric_social_costs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals or groups who can present evidence of concrete injury (reputational, psychological, economic, or physical) traceable to specific speech acts. They petition courts, tribunals, or platform bodies to have the speech restricted or the speaker sanctioned, using the harm threshold as their evidentiary lever.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, documented_harm_claimants, beneficiary,
    moderate, biographical, constrained, national).

% Groups historically subject to speech that facilitates discrimination or violence against them. The harm-threshold framework gives them a doctrinal path to restriction that does not depend on proving the speech constitutes structural subordination (the dignity reading's higher bar), only demonstrable harm in a given instance.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, targeted_minority_communities, beneficiary,
    organized, generational, constrained, national).

% Courts, regulatory commissions, and platform trust-and-safety tribunals that administer the harm threshold test: they define what counts as 'demonstrable' harm, weigh evidence, and issue rulings that either uphold or lift speech restrictions. They set and revise the evidentiary bar itself, which is the constraint's actual point of leverage.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, harm_adjudication_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Speakers whose statements are alleged to cause harm and who face injunction, liability, deplatforming, or criminal sanction once a harm claim clears the threshold. They must either self-censor pre-emptively, litigate the harm determination (costly and uncertain), or accept restriction; there is no exit from the jurisdiction's speech regime short of relocating entirely.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, controversial_speakers, payer,
    moderate, biographical, constrained, national).

% Speakers criticizing powerful institutions or unpopular truths whose speech is recharacterized as harmful by those institutions weaponizing the harm-threshold doctrine defensively. They lack the resources to litigate a harm determination and are disproportionately exposed to the threshold's discretionary application.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, dissident_political_commentators, payer,
    powerless, biographical, trapped, national).

% Speakers using exaggeration, mockery, or provocation whose expressive form is easily mischaracterized as literal harm-causing assertion under a harm-threshold test that does not naturally distinguish rhetorical mode from literal claim. They bear the chilling effect of uncertainty about where the threshold will be drawn in their specific case.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, satirists_and_provocateurs, payer,
    powerless, biographical, constrained, national).

% Appellate and supreme courts that review harm-threshold determinations for consistency with broader constitutional speech guarantees, comparing this reading against sibling doctrines (absolutist, marketplace, dignity, democratic-participation) when resolving conflicts between them.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, constitutional_courts, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a doctrinal mechanism for weighing genuine, demonstrable injury from speech against the speaker's expressive interest, preventing a purely absolutist regime from leaving injured parties with no recourse.
% TRANSFER_FUNCTION: Moves the practical entitlement to speak freely from the speaker to the claimed victim once a harm showing clears the evidentiary threshold — restricting, sanctioning, or removing the speech and shifting reputational and legal risk onto the speaker.
% ABSENT_VOICES: Speakers whose expression is chilled pre-emptively (before any claim is filed) are never heard from at all — the doctrine only sees the cases that reach adjudication, not the self-censorship it induces upstream. Marginalized dissidents without resources to contest a harm finding are functionally unheard even when a claim is filed against them.
% DISAPPEARANCE_RATIONALE: If the harm threshold vanished overnight, harm claimants would lose their primary doctrinal lever against injurious speech and would have to fall back on narrower categorical exceptions (incitement, defamation with independent proof of falsity, true threats); courts would need an entirely different doctrinal architecture to handle harm claims, and speakers currently self-censoring under threshold uncertainty would face a materially different risk calculus.
% FOUNDING_PROBLEM: Legal systems needed a way to protect victims of speech-caused injury (defamation, harassment, incitement-adjacent speech) without abandoning speech protection altogether, in contexts where purely categorical rules (only literal incitement, only defamation) proved too narrow to capture new harms (algorithmically amplified harassment, coordinated targeting campaigns).
% FOUNDING_PROBLEM_CORROBORATION: Harm claimants and adjudication bodies attest the founding problem remains live, citing ongoing documented injuries from online harassment and targeted defamation campaigns. Civil liberties organizations and dissident-speech advocates, external to the beneficiary set, attest the doctrine has drifted from its founding injury-remediation function toward a general-purpose suppression tool used disproportionately against politically weak speakers, and that the 'demonstrable harm' bar has been progressively lowered in practice — a claim partially corroborated by comparative caselaw analysis showing threshold inconsistency across jurisdictions applying the same nominal test.
narrative_ontology:disappearance_verdict(speech_protection_kernel__harm_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__harm_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__harm_threshold_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_kernel__harm_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__harm_threshold_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__harm_threshold_reading_tests).
:- end_tests(speech_protection_kernel__harm_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects that a meaningful share of restriction under this doctrine falls on speakers whose 'harm' is contested, rhetorical, or politically inconvenient to powerful institutions, not solely on genuinely injurious speech. Suppression (0.51) is moderate-high because the doctrine requires active adjudicatory enforcement (courts, tribunals, platform bodies) to draw and redraw the threshold line, and that enforcement machinery has hardened over the measured interval as harm claims have proliferated (accessibility_collapse 0.4, reflecting that categorical alternatives — pure incitement or defamation standards — remain doctrinally available even as the harm-threshold approach dominates practice). Resistance (0.62) is substantial because civil liberties advocates and affected speakers actively contest threshold determinations in nearly every high-profile application. Theater ratio (0.28, rising modestly) reflects a growing gap between the doctrine's stated injury-remediation function and its increasing use as a general suppression tool for reputationally costly but not clearly injurious speech.
 *
 * DIRECTIONALITY LOGIC:
 *   Harm claimants and targeted communities sit near the beneficiary end: the doctrine exists to give them a remedy where none existed under narrower categorical rules. Harm adjudication bodies are agenda-setters with institutional power and analytical exit — they administer the threshold and are largely insulated from its consequences. Controversial speakers and dissident commentators sit near the target end: they bear restriction, liability, or sanction once a harm claim clears the bar, and their exit options are constrained or trapped because self-censorship or relocation are the only alternatives to litigation they often cannot afford. Satirists face a distinct vulnerability: their speech mode (exaggeration, mockery) is structurally mismatched to a literal-harm evidentiary test, producing chilling effects even absent any actual claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — victims of genuinely injurious speech having no doctrinal recourse under purely categorical free-speech rules — remains partly live (harassment and targeted defamation persist and evolve with new communication technology), which prevents a clean mandatrophy verdict. But the founding_problem_status is authored as contested precisely because external corroboration (civil liberties monitoring, comparative caselaw) documents the threshold's use well beyond its original injury-remediation scope, particularly against politically weak speakers. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (real victims do get real recourse) while flagging the asymmetric extraction (discretionary application disadvantaging powerless dissidents) that a pure-rope or pure-mountain reading would erase.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_threshold_calibration_ambiguity,
    'Where exactly does ''demonstrable harm'' sit on the spectrum from concrete physical/economic injury to diffuse reputational or emotional distress, and who controls that calibration over time?',
    'Longitudinal comparative caselaw analysis tracking whether the evidentiary bar for ''demonstrable'' harm has been lowered, raised, or held stable across jurisdictions applying nominally the same standard, cross-checked against who initiates successful claims (concentrated institutional actors vs. genuinely injured individuals).',
    'If the threshold has been progressively lowered to capture reputational or purely offense-based claims, the doctrine has drifted from coordination (remedying real injury) toward extraction (suppressing inconvenient speech); if the bar has remained high and consistently applied, the tangled_rope classification should weight more heavily toward the coordination pole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_threshold_calibration_ambiguity, empirical, 'Whether the harm threshold''s evidentiary bar has drifted lower over time, expanding the doctrine''s reach beyond its founding injury-remediation function.').

omega_variable(
    sibling_reading_selection_ambiguity,
    'Given that the speech_protection_kernel supports at least five structurally distinct readings (absolutist, marketplace, dignity, harm_threshold, democratic_participation), what determines which reading a given jurisdiction or adjudicatory body actually applies in a specific case, and is that selection itself contestable or principled?',
    'Doctrinal survey of case outcomes where multiple readings could plausibly apply, checking whether courts articulate a principled basis for selecting harm_threshold over, e.g., dignity_reading or marketplace_reading, or whether selection tracks the political valence of the speaker/victim pair.',
    'If reading-selection tracks outcome-desired results rather than principled doctrinal criteria, the harm_threshold_reading''s apparent coherence as a standalone constraint is partly illusory — it may function as a discretionary escape hatch selected precisely when it produces restriction that other readings would not authorize. This would raise the effective extractiveness beyond what this story''s ε alone captures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_selection_ambiguity, conceptual, 'Whether courts'' choice among the kernel''s sibling readings is principled or outcome-driven, bearing on whether harm_threshold_reading has independent doctrinal integrity.').

omega_variable(
    chilling_effect_measurement_gap,
    'How much self-censorship does the harm-threshold doctrine induce among speakers who never become the subject of a formal claim, and is that upstream chilling effect part of this constraint''s true extraction or a separate phenomenon?',
    'Survey-based or natural-experiment research measuring speech behavior change among at-risk speaker populations (satirists, dissidents) following high-profile harm-threshold rulings, compared to jurisdictions without such rulings.',
    'If chilling effects are large and concentrated among powerless speakers, the effective extractiveness experienced by that population is substantially higher than the formal-claims-only measure this story''s extractiveness score reflects, since most of the doctrine''s coercive effect never surfaces in adjudicated cases.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(chilling_effect_measurement_gap, empirical, 'Whether pre-emptive self-censorship induced by threshold uncertainty constitutes unmeasured extraction beyond what formal harm-claim outcomes capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__harm_threshold_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__harm_threshold_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(spee_tr_t8, speech_protection_kernel__harm_threshold_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(spee_tr_t16, speech_protection_kernel__harm_threshold_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(spee_tr_t24, speech_protection_kernel__harm_threshold_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(spee_tr_t32, speech_protection_kernel__harm_threshold_reading, theater_ratio, 32, 0.25).
narrative_ontology:measurement(spee_tr_t40, speech_protection_kernel__harm_threshold_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(spee_be_t8, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(spee_be_t16, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 16, 0.33).
narrative_ontology:measurement(spee_be_t24, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 24, 0.37).
narrative_ontology:measurement(spee_be_t32, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(spee_be_t40, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(spee_su_t8, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(spee_su_t16, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 16, 0.44).
narrative_ontology:measurement(spee_su_t24, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 24, 0.47).
narrative_ontology:measurement(spee_su_t32, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 32, 0.49).
narrative_ontology:measurement(spee_su_t40, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 40, 0.51).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__harm_threshold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling constraints decomposing the colloquial concept 'speech protection kernel' per the ε-invariance principle. Each sibling reading (absolutist, marketplace, dignity, harm_threshold, democratic_participation) has its own ε, its own beneficiary/victim structure, and its own classification, because measuring 'speech protection' through the harm-avoidance observable yields a structurally different constraint than measuring it through the truth-discovery, dignity-preservation, or self-governance observables. This file (harm_threshold_reading) is authored as tangled_rope; the sibling files should be authored independently rather than as alternate measurements of this file's ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
