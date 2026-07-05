% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__harm_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__harm_limited_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: speech_protection_boundary__harm_limited_reading
 *   human_readable: Harm-Limited Reading of Speech Protection (Dignity/Equality/Anti-Harassment Boundary)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint models the harm-limited reading of the speech protection
 *   kernel: a jurisdiction (or line of doctrine) that conditions speech
 *   protection on the absence of significant harm to dignity, equality, and
 *   freedom from harassment. Unlike the absolutist reading (protection
 *   near-total, harm exception limited to imminent lawless action) or the
 *   balancing reading (case-by-case weighing without a categorical harm
 *   floor), this reading builds a categorical exclusion: speech causing
 *   significant dignitary/equality/harassment harm falls outside protection
 *   by definition, not by ad hoc weighing. This narrows the protected set and
 *   installs courts/tribunals as gatekeepers empowered to classify speech
 *   into the excluded category, with attendant risk of viewpoint-selective or
 *   inconsistent application. This is a single, ε-stable reading — the other
 *   two readings are separate constraints, not alternate measurements of this
 *   one.
 *
 * KEY AGENTS:
 *   - historically_marginalized_groups: primary beneficiary (organized/constrained) — gains protection against dignitary harm
 *   - harassment_targets: primary beneficiary (moderate/constrained) — gains standing against harassing speech
 *   - state_speech_regulators: agenda_setter (institutional/analytical) — administers the harm threshold
 *   - controversial_speakers: primary target (moderate/constrained) — bears sanction risk
 *   - dissenting_minority_viewpoints: secondary target (powerless/trapped) — swept into unprotected category without power to contest
 *   - constitutional_scholars: analytical observer — tracks doctrinal drift across the kernel's three readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__harm_limited_reading, 0.52).
domain_priors:suppression_score(speech_protection_boundary__harm_limited_reading, 0.61).
domain_priors:theater_ratio(speech_protection_boundary__harm_limited_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, accessibility_collapse, 0.47).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__harm_limited_reading, "Harm-Limited Reading of Speech Protection (Dignity/Equality/Anti-Harassment Boundary)").
narrative_ontology:topic_domain(speech_protection_boundary__harm_limited_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_boundary__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__harm_limited_reading, '4581c4e2-b120-412e-8ce8-b408e747fec7').
narrative_ontology:cs_kernel_codification('4581c4e2-b120-412e-8ce8-b408e747fec7', distributed).
narrative_ontology:cs_authority_grounding('4581c4e2-b120-412e-8ce8-b408e747fec7', distributed).
narrative_ontology:cs_reading_relation('4581c4e2-b120-412e-8ce8-b408e747fec7', speech_protection_boundary__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('4581c4e2-b120-412e-8ce8-b408e747fec7', speech_protection_boundary__balancing_reading, influences).
narrative_ontology:cs_axiom('4581c4e2-b120-412e-8ce8-b408e747fec7', foundational, dignitarian_harm_floor_overrides_content_neutrality).
narrative_ontology:cs_axiom_status(dignitarian_harm_floor_overrides_content_neutrality, holdable).
narrative_ontology:cs_axiom_grounding('4581c4e2-b120-412e-8ce8-b408e747fec7', dignitarian_harm_floor_overrides_content_neutrality, deontological).
narrative_ontology:cs_axiom('4581c4e2-b120-412e-8ce8-b408e747fec7', foundational, substantive_equality_is_a_coequal_constitutional_value_to_expression).
narrative_ontology:cs_axiom_status(substantive_equality_is_a_coequal_constitutional_value_to_expression, holdable).
narrative_ontology:cs_axiom_grounding('4581c4e2-b120-412e-8ce8-b408e747fec7', substantive_equality_is_a_coequal_constitutional_value_to_expression, conventional).
narrative_ontology:cs_reference_frame('4581c4e2-b120-412e-8ce8-b408e747fec7', post_civil_rights_dignitarian_consensus).
narrative_ontology:cs_drift_state('4581c4e2-b120-412e-8ce8-b408e747fec7', contemporary_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4581c4e2-b120-412e-8ce8-b408e747fec7', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__harm_limited_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, historically_marginalized_groups).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, harassment_targets).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, equality_rights_advocates).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, state_speech_regulators).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, controversial_speakers).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, dissenting_minority_viewpoints).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, satirists_and_provocateurs).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, coded_speech_targets_of_overbroad_enforcement).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, harassment_targets).
narrative_ontology:constraint_vindicates(speech_protection_boundary__harm_limited_reading, dignitarian_theory_of_free_expression).
narrative_ontology:constraint_vindicates(speech_protection_boundary__harm_limited_reading, substantive_equality_as_constitutional_value).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups historically subject to dehumanizing rhetoric gain a legal backstop against speech that degrades dignity or entrenches subordination. They cannot exit the public sphere where such speech circulates, so the reading's protection operates as their primary recourse short of social ostracism of speakers.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, historically_marginalized_groups, beneficiary,
    organized, generational, constrained, national).

% Individuals subject to sustained harassment campaigns gain standing to seek suppression or sanction of the harassing speech. They also bear cost when the boundary is applied inconsistently or slowly, leaving them exposed during adjudication delays.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, harassment_targets, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__harm_limited_reading, harassment_targets, payer).

% Advocacy organizations and sympathetic jurists press courts and legislatures to define and expand the harm categories (dignity, equality, freedom from harassment) that narrow protected speech. They shape doctrine, litigate test cases, and benefit professionally and institutionally from the boundary's continued salience.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, equality_rights_advocates, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__harm_limited_reading, equality_rights_advocates, beneficiary).

% Courts, human rights tribunals, and administrative bodies apply the harm standard case-by-case, deciding what counts as significant harm to dignity or equality. This grants them substantial discretion over which speech is unprotected, with attendant risk of inconsistent or politically inflected application.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, state_speech_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Speakers advancing unpopular, offensive, or provocative views face potential sanction, deplatforming, or liability if their speech is judged to inflict significant dignitary or equality harm. Their exit options are limited to self-censorship or relocation to less-regulated jurisdictions or platforms.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, controversial_speakers, payer,
    moderate, biographical, constrained, national).

% Minority political or religious viewpoints that clash with dominant equality norms (e.g., traditionalist positions on gender or sexuality) risk being swept into the unprotected category despite lacking the institutional power to contest the classification. They have little capacity to relitigate boundary lines set by better-resourced actors.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, dissenting_minority_viewpoints, payer,
    powerless, biographical, trapped, national).

% Satire, dark comedy, and provocative art that trades in taboo or offensive material face chilling effects because harm-based tests are difficult to apply predictably to ironic or exaggerated speech. They must anticipate ex post harm findings with no clear ex ante line.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, satirists_and_provocateurs, payer,
    moderate, biographical, constrained, national).

% Speakers using ambiguous, coded, or context-dependent language (dog whistles, cultural references) can be caught by overbroad application of the harm standard even absent clear intent to harass, because the standard's discretion cuts both ways — it can under-catch sophisticated harassment and over-catch ambiguous speech from less legally sophisticated speakers.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, coded_speech_targets_of_overbroad_enforcement, payer,
    powerless, immediate, trapped, national).

% Free-speech-maximalist organizations argue the harm-limited reading imports subjective, politically contestable harm categories into speech law, inviting viewpoint discrimination. Their objections are heard in dissenting opinions and public commentary but do not control the boundary's operation once codified into governing doctrine.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, civil_liberties_organizations, excluded,
    organized, generational, mobile, national).

% Study how the harm-limited standard is applied across cases, tracking drift, inconsistency, and comparative outcomes against the absolutist and balancing readings. Their analysis can inform doctrinal reform but does not itself adjudicate cases.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_boundary__harm_limited_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_protection_boundary__harm_limited_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for weighing the social costs of degrading, exclusionary, or harassing speech against expressive liberty, allowing marginalized groups and harassment targets to seek legal recourse rather than relying solely on counter-speech or private remedies.
% TRANSFER_FUNCTION: Moves protection away from speech deemed to inflict significant dignitary, equality, or harassment harm and toward the targets of that speech; simultaneously transfers discretionary power over the protected/unprotected boundary to courts and regulatory bodies, and transfers social and legal risk onto speakers whose expression falls into contested or ambiguous harm categories.
% ABSENT_VOICES: Civil liberties absolutists and defenders of maximalist protection are present in dissent and commentary but structurally lose once a jurisdiction adopts the harm-limited reading as governing doctrine; individuals whose coded or context-dependent speech is swept up by overbroad application rarely have the resources to challenge misclassification and are functionally unheard at the moment of enforcement.
% DISAPPEARANCE_RATIONALE: If the harm-limited reading disappeared overnight and jurisdictions reverted to an absolutist or narrow balancing standard, previously sanctionable hate speech, harassment campaigns, and dignitary attacks would regain protection; marginalized groups would lose a primary legal recourse against degrading rhetoric, while previously chilled speakers would regain expressive latitude. Litigation dockets, tribunal caseloads, and platform moderation policies keyed to the standard would all require reconstruction.
% FOUNDING_PROBLEM: Concern that formally neutral, near-absolute speech protection allowed dehumanizing, exclusionary, and harassing speech to entrench social subordination of already-marginalized groups, undermining their substantive equality and dignity even where no imminent violence was threatened.
% FOUNDING_PROBLEM_CORROBORATION: Equality rights advocates and courts applying the standard attest the founding problem remains live, citing ongoing harassment and hate speech harms. Civil liberties organizations and free-speech scholars, external to the reading's beneficiary coalition, attest that the standard has drifted from remedying demonstrated severe harm toward broad discretionary suppression of disfavored viewpoints, and that comparative jurisdictions with narrower harm exceptions show lower incidence of documented dignitary harm without correspondingly higher hate speech rates — a contested empirical claim not independently resolved.
narrative_ontology:disappearance_verdict(speech_protection_boundary__harm_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__harm_limited_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__harm_limited_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_boundary__harm_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__harm_limited_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__harm_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__harm_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_boundary__harm_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52 at interval end) and rising: the boundary began as a targeted remedy for severe dignitary/harassment harm and has, per corroborating outside sources, drifted toward broader discretionary suppression — a classic tangled-rope trajectory where a real coordination function (protecting people from dehumanizing speech) accrues extractive overhead (viewpoint-selective gatekeeping) over time. Suppression is higher than extractiveness (0.61) because the mechanism depends on active enforcement — courts and tribunals must affirmatively classify speech as unprotected, and that classification power is coercive regardless of how sympathetically it is exercised. Theater ratio is comparatively low (0.28): the harm-remedying function remains substantially real, not merely performative, though it is increasing as the standard is invoked in cases further from its founding paradigm (severe, targeted harassment) toward more marginal or politically contested speech. Accessibility collapse is moderate (0.47): affected speakers still have some avenues (private platforms, alternative jurisdictions, appeal), but formal legal protection genuinely narrows once a court adopts this reading. Resistance is high (0.71), reflecting sustained, well-resourced opposition from civil liberties organizations and free-speech scholars.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically marginalized groups and harassment targets are structural beneficiaries: the categorical harm exclusion exists to protect them, and their directionality sits near the beneficiary end. Equality rights advocates and state regulators are agenda-setters who administer and expand the boundary; their directionality reflects institutional benefit from the standard's continued salience and discretion. Controversial speakers, dissenting minority viewpoints, satirists, and coded-speech targets are structural targets: the same mechanism that protects the beneficiary groups extracts expressive latitude from them, with powerless/trapped agents (dissenting minorities, coded-speech targets) sitting nearest the full-target end because they lack resources to contest misclassification, while moderate-power targets (controversial speakers, satirists) have some capacity to litigate or relocate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — formally neutral absolutism permitting entrenchment of subordination through dehumanizing speech — remains genuinely contested as live: marginalized groups and equality advocates attest it persists, while civil liberties scholars attest the remedy has outrun the original harm paradigm and now functions partly as viewpoint-selective suppression untethered from demonstrated severe harm. This is not resolved in this story; the founding_problem_status is 'contested' precisely because corroboration from outside the beneficiary coalition disputes the doctrine's current scope even while conceding its original justification. Classifying this as tangled_rope (rather than snare) preserves the genuine coordination function — protection of severe harassment/dignitary harm targets — from being collapsed into pure extraction, while the beneficiary/victim/enforcement structure prevents the reverse error of treating the boundary as costless coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_threshold_objectivity,
    'Can ''significant harm to dignity, equality, and freedom from harassment'' be defined with enough determinacy to prevent the classification from collapsing into viewpoint discrimination, or is the threshold irreducibly political?',
    'Comparative doctrinal analysis across jurisdictions applying similar harm standards, tracking inter-adjudicator consistency and correlation between speaker viewpoint and harm findings.',
    'If the threshold proves irreducibly indeterminate and correlates with speaker viewpoint, the reading functions closer to a snare wearing a coordination justification; if it proves administrable with consistent, viewpoint-neutral application, the tangled_rope classification with a genuine coordination core is well supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_threshold_objectivity, empirical, 'Whether the dignitary/equality/harassment harm threshold is administrable or irreducibly political.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Given that the underlying kernel (where speech protection ends) admits at least three coherent readings — absolutist, balancing, and harm-limited — what determines which reading a given jurisdiction or court adopts, and is that selection itself principled or contingent on political composition of the adjudicating body?',
    'Track doctrinal shifts across changes in court composition; if the reading in force correlates strongly with the political valence of sitting judges rather than with stable doctrinal reasoning, the kernel''s readings function more as contested political commitments than as competing legal principles.',
    'If reading selection tracks judicial composition rather than principled reasoning, all three readings (including this one) carry elevated theater risk — the doctrinal justification may be substantially post hoc relative to an outcome-driven selection.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether selection among the kernel''s sibling readings is principled or politically contingent.').

omega_variable(
    overbreadth_vs_underreach_asymmetry,
    'Does the harm-limited standard, in practice, more often fail by under-protecting genuine harassment/dignitary-harm victims (underreach) or by over-suppressing ambiguous, coded, or minority speech (overbreadth)?',
    'Empirical audit of enforcement outcomes: proportion of cases where clearly severe harassment went unremedied versus proportion where ambiguous or minority-viewpoint speech was sanctioned.',
    'If overbreadth dominates, the victim set (dissenting minorities, coded-speech targets) is larger than the beneficiary set in practice, pushing the constraint toward snare; if underreach dominates, the coordination function is under-realized and the constraint may be better characterized as a weak rope with insufficient enforcement rather than a tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(overbreadth_vs_underreach_asymmetry, empirical, 'Whether real-world application over-suppresses ambiguous speech or under-protects genuine harassment victims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__harm_limited_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_boundary__harm_limited_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(spee_tr_t8, speech_protection_boundary__harm_limited_reading, theater_ratio, 8, 0.17).
narrative_ontology:measurement(spee_tr_t16, speech_protection_boundary__harm_limited_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(spee_tr_t24, speech_protection_boundary__harm_limited_reading, theater_ratio, 24, 0.23).
narrative_ontology:measurement(spee_tr_t32, speech_protection_boundary__harm_limited_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(spee_tr_t40, speech_protection_boundary__harm_limited_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_boundary__harm_limited_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(spee_be_t8, speech_protection_boundary__harm_limited_reading, base_extractiveness, 8, 0.39).
narrative_ontology:measurement(spee_be_t16, speech_protection_boundary__harm_limited_reading, base_extractiveness, 16, 0.44).
narrative_ontology:measurement(spee_be_t24, speech_protection_boundary__harm_limited_reading, base_extractiveness, 24, 0.47).
narrative_ontology:measurement(spee_be_t32, speech_protection_boundary__harm_limited_reading, base_extractiveness, 32, 0.5).
narrative_ontology:measurement(spee_be_t40, speech_protection_boundary__harm_limited_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_boundary__harm_limited_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(spee_su_t8, speech_protection_boundary__harm_limited_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(spee_su_t16, speech_protection_boundary__harm_limited_reading, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(spee_su_t24, speech_protection_boundary__harm_limited_reading, suppression_requirement, 24, 0.57).
narrative_ontology:measurement(spee_su_t32, speech_protection_boundary__harm_limited_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(spee_su_t40, speech_protection_boundary__harm_limited_reading, suppression_requirement, 40, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__harm_limited_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, speech_protection_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, speech_protection_boundary__balancing_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the speech_protection_boundary kernel, decomposed per the ε-invariance principle: absolutist_reading (near-total protection, narrow imminent-lawless-action exception), balancing_reading (no categorical harm floor, case-by-case weighing), and harm_limited_reading (this file — categorical harm exclusion for dignity/equality/harassment harm). Each reading has a distinct ε, distinct beneficiary/victim structure, and distinct classification; they are linked here rather than merged because measuring 'speech protection' under each reading yields structurally different constraints, not different measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
