% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Near-Absolute Speech Protection with Narrow Harm-Override Category
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the absolutist reading of the
 *   speech_harm_boundary kernel: a doctrinal architecture that treats speech
 *   protection as operating near-absolutely, with an unprotected category set
 *   held deliberately narrow (incitement to imminent lawless action, true
 *   threats, defamation, obscenity) and an extremely high threshold for any
 *   harm-based override. The doctrine emerged principally to prevent
 *   majoritarian and governmental suppression of dissenting political speech,
 *   and it genuinely succeeds at that coordination function — political
 *   dissidents and unpopular speakers retain a structural shield against
 *   censorship that is difficult for any single administration or majority to
 *   erode. But the same narrow category set that protects the powerless
 *   dissident also protects powerful speakers, publishers, and platforms
 *   whose speech vilifies, dehumanizes, or harasses identifiable targets,
 *   with those targets bearing the accumulated harm and having essentially no
 *   legal recourse. This is not the harm_balancing_reading (which subjects
 *   protection to proportionality balancing against demonstrated harm) or the
 *   dignity_reading (which makes personhood-denying speech categorically
 *   unprotected regardless of political value) — this reading holds the
 *   override threshold at its highest setting and keeps the unprotected
 *   category set narrowest. The three readings are separate constraints in
 *   this framework, linked via network.affects_constraints; each carries its
 *   own epsilon.
 *
 * KEY AGENTS:
 *   - controversial_speakers: beneficiary (moderate/mobile) — speaks with near-total legal confidence
 *   - media_publishers: beneficiary (organized/mobile) — editorial speech shielded from all but narrowest liability
 *   - political_dissidents: beneficiary (powerless/constrained) — the doctrine's strongest coordination justification
 *   - provocateur_speech_platforms: beneficiary/agenda_setter (institutional/arbitrage) — litigates and administers to keep the threshold narrow
 *   - targets_of_hate_speech: payer (powerless/trapped) — bears dignitary harm with no recourse
 *   - harassment_targets: payer (powerless/trapped) — cumulative harm rarely meets any single protected-speech exception
 *   - minority_communities_subject_to_vilification: payer (organized/constrained) — diffuse class-level cost
 *   - legislators_and_courts: agenda_setter (institutional/analytical) — sets and defends the boundary
 *   - constitutional_scholars: observer (analytical/analytical) — documents tradeoffs and comparative frameworks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__absolutist_reading, 0.58).
domain_priors:suppression_score(speech_harm_boundary__absolutist_reading, 0.42).
domain_priors:theater_ratio(speech_harm_boundary__absolutist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__absolutist_reading, "Near-Absolute Speech Protection with Narrow Harm-Override Category").
narrative_ontology:topic_domain(speech_harm_boundary__absolutist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_harm_boundary__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__absolutist_reading, '3c3b61d4-c93a-4651-87a2-e8378210e7a0').
narrative_ontology:cs_kernel_codification('3c3b61d4-c93a-4651-87a2-e8378210e7a0', fixed_text).
narrative_ontology:cs_authority_grounding('3c3b61d4-c93a-4651-87a2-e8378210e7a0', lineage).
narrative_ontology:cs_interpretation_layer_present('3c3b61d4-c93a-4651-87a2-e8378210e7a0').
narrative_ontology:cs_reading_relation('3c3b61d4-c93a-4651-87a2-e8378210e7a0', speech_harm_boundary__dignity_reading, forecloses).
narrative_ontology:cs_reading_relation('3c3b61d4-c93a-4651-87a2-e8378210e7a0', speech_harm_boundary__harm_balancing_reading, coexists_with).
narrative_ontology:cs_axiom('3c3b61d4-c93a-4651-87a2-e8378210e7a0', foundational, speaker_autonomy_categorically_prior_to_dignitary_harm).
narrative_ontology:cs_axiom_status(speaker_autonomy_categorically_prior_to_dignitary_harm, holdable).
narrative_ontology:cs_axiom_grounding('3c3b61d4-c93a-4651-87a2-e8378210e7a0', speaker_autonomy_categorically_prior_to_dignitary_harm, deontological).
narrative_ontology:cs_axiom('3c3b61d4-c93a-4651-87a2-e8378210e7a0', foundational, censorship_risk_from_power_outweighs_uncompensated_speech_harm).
narrative_ontology:cs_axiom_status(censorship_risk_from_power_outweighs_uncompensated_speech_harm, holdable).
narrative_ontology:cs_axiom_grounding('3c3b61d4-c93a-4651-87a2-e8378210e7a0', censorship_risk_from_power_outweighs_uncompensated_speech_harm, instrumental).
narrative_ontology:cs_reference_frame('3c3b61d4-c93a-4651-87a2-e8378210e7a0', categorical_speaker_autonomy_primacy).
narrative_ontology:cs_drift_state('3c3b61d4-c93a-4651-87a2-e8378210e7a0', digital_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3c3b61d4-c93a-4651-87a2-e8378210e7a0', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__absolutist_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, controversial_speakers).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, media_publishers).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, political_dissidents).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, provocateur_speech_platforms).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, targets_of_hate_speech).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, harassment_targets).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, minority_communities_subject_to_vilification).
narrative_ontology:constraint_vindicates(speech_harm_boundary__absolutist_reading, marketplace_of_ideas_doctrine).
narrative_ontology:constraint_vindicates(speech_harm_boundary__absolutist_reading, counter_speech_remedy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Can publish inflammatory, offensive, or provocative political and social speech with near-total confidence that courts will not impose liability absent incitement to imminent lawless action, a true threat, defamation of a private fact, or obscenity. Faces almost no legal exposure for speech that causes reputational, psychological, or dignitary harm short of those narrow categories.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, controversial_speakers, beneficiary,
    moderate, biographical, mobile, national).

% Operates under a doctrine that shields editorial decisions, including publication of content that damages individual reputations or dignity, from all but the narrowest liability categories. Benefits directly from the high override threshold in defamation and privacy litigation.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, media_publishers, beneficiary,
    organized, generational, mobile, national).

% Relies on the near-absolute protection to voice unpopular or minority political positions without fear that a government or majority could suppress the speech merely by asserting it causes harm. This is the doctrine's strongest coordination justification: protecting the powerless critic of power.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, political_dissidents, beneficiary,
    powerless, biographical, constrained, national).

% Platforms and legal advocacy organizations litigate to keep the harm-override threshold narrow, framing any expansion as a slippery slope toward censorship. They administer content policy and litigation strategy that entrenches the doctrine and directly profit from permissive content moderation regimes modeled on it.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, provocateur_speech_platforms, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__absolutist_reading, provocateur_speech_platforms, agenda_setter).

% Bears the accumulated dignitary, psychological, and social cost of speech that vilifies, dehumanizes, or demeans on the basis of group identity, with almost no legal recourse because the harm does not rise to incitement, true threat, defamation, or obscenity. Has no meaningful exit — cannot opt out of a shared public and digital sphere where such speech circulates.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, targets_of_hate_speech, payer,
    powerless, biographical, trapped, national).

% Experiences sustained, targeted, repetitive speech-based harassment that individually may fall under the protected-speech umbrella even as its cumulative effect is severe. Legal remedies require proving categories (true threat, stalking-adjacent conduct) that are drawn narrowly and are hard to satisfy on a speech-by-speech basis.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, harassment_targets, payer,
    powerless, immediate, trapped, local).

% As a class, bears the diffuse social cost of normalized group-vilifying speech that the doctrine treats as protected political or social commentary. Can organize counter-speech and advocacy but cannot obtain injunctive or damages relief through the legal system for the underlying speech itself.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, minority_communities_subject_to_vilification, payer,
    organized, generational, constrained, national).

% Sets and enforces the boundary between protected and unprotected speech through case law and statute, maintaining the narrow unprotected-category set (incitement, true threats, defamation, obscenity) against pressure to expand it. Determines where the override threshold sits and how high it must be cleared.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, legislators_and_courts, agenda_setter,
    institutional, generational, analytical, national).

% Analyzes the doctrine's tradeoffs, tracks comparative approaches (e.g., dignity-based European frameworks), and produces the scholarship that documents both the coordination value of the near-absolute rule and its distributive costs to targets of harmful speech.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_harm_boundary__absolutist_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_harm_boundary__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents government or majoritarian actors from suppressing dissenting, unpopular, or offensive political and social speech by requiring an extremely high showing of concrete, imminent, or narrowly-categorized harm before speech can be restricted — solving the genuine collective-action problem of censorship creep, where any harm-based override standard is vulnerable to expansion by whoever holds power to define 'harm.'
% TRANSFER_FUNCTION: Moves the cost of unrestrained speech from speakers and platforms (who bear no liability short of narrow categories) onto identifiable targets of vilifying, dehumanizing, or harassing speech, who absorb dignitary and psychological harm with minimal legal recourse.
% ABSENT_VOICES: Targets of persistent group-based vilification and individualized harassment campaigns are structurally underrepresented in the doctrine's formation — courts and legislatures hear from speakers, publishers, and civil liberties advocates far more often than from injured parties, whose remedies require clearing categories drawn specifically to exclude their harms.
% DISAPPEARANCE_RATIONALE: If the near-absolute protection and its narrow override categories disappeared overnight, courts would need an entirely different balancing framework; speakers and publishers would face new liability exposure for offensive or dignitary-harming speech, political dissidents would lose a structural shield against majoritarian suppression, and the entire architecture of American free-expression litigation, content moderation policy, and journalism practice would reorganize around a different standard.
% FOUNDING_PROBLEM: Historical experience with sedition laws, licensing regimes, and majoritarian suppression of unpopular political speech (abolitionist, labor, anti-war, civil rights speech) demonstrated that any harm-based override standard could be weaponized by those in power to silence dissent — the doctrine was built to make censorship structurally difficult regardless of who holds power.
% FOUNDING_PROBLEM_CORROBORATION: Free speech advocacy organizations and constitutional scholars attest the anti-censorship problem remains live, citing ongoing government efforts to restrict protest and dissenting speech globally. Civil rights organizations, dignity-based legal scholars, and comparative law analysts (citing European and Canadian frameworks that manage both concerns) attest that the founding problem has been substantially addressed by narrower, better-calibrated doctrines elsewhere, and that the absolutist reading's persistence in its current form increasingly serves speaker and platform interests rather than the original anti-censorship purpose — this corroboration comes from outside the beneficiary set (dignity-harm litigants, minority advocacy groups, and comparative legal scholarship), not from speakers or platforms themselves.
narrative_ontology:disappearance_verdict(speech_harm_boundary__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__absolutist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_harm_boundary__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__absolutist_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is set at 0.58 (moderate-high, rising over the interval) because the doctrine's narrow unprotected-category set channels increasing real-world harm (digital-era vilification, coordinated harassment campaigns) onto powerless targets while speakers and platforms bear essentially no liability — the effective transfer has intensified as speech volume and reach have scaled through digital platforms, even though the doctrinal standard itself is largely static. Suppression is set lower (0.42) because the doctrine does not coercively suppress alternative speech or exit for the SPEAKER side (that is the point of the reading) — the suppression that exists is directed at targets' capacity to obtain legal remedy, which is a structural foreclosure rather than active coercion. Theater ratio is low-moderate (0.22) reflecting that most enforcement activity (case litigation, doctrinal maintenance) is genuinely functional to the coordination purpose, though a growing share defends platform-scale content policies modeled on the doctrine rather than the core anti-censorship purpose. Accessibility collapse is moderate (0.35): alternative doctrinal frameworks (harm balancing, dignity-based) are visibly practiced in other jurisdictions, so alternatives have not collapsed globally, only domestically. Resistance is high (0.68): civil rights organizations, dignity scholars, and harassment-target advocacy groups actively contest the narrow threshold in courts, legislatures, and public discourse.
 *
 * PERSPECTIVAL GAP:
 *   From the speaker/publisher/platform seats, this reading looks like a Rope or even a Mountain — a near-inviolable structural protection against censorship that everyone benefits from equally. From the target seats (hate-speech targets, harassment targets, vilified minority communities), the same doctrine looks like a Snare — extraction of dignitary and psychological cost with no functioning remedy, dressed in the language of universal coordination benefit. The tangled_rope classification holds both readings simultaneously: the coordination function (anti-censorship shield for dissidents) is real AND coexists with asymmetric extraction (harm absorbed disproportionately by identifiable, less powerful targets) through the same narrow-category structure, requiring active judicial and legislative enforcement to hold the line against both expansion pressure and erosion pressure.
 *
 * DIRECTIONALITY LOGIC:
 *   Political dissidents and controversial speakers derive low d (beneficiary end) because the doctrine subsidizes their speech activity directly — near-zero liability risk regardless of speech content, provided it stays outside the narrow categories. Media publishers and platforms derive similarly low d, amplified by mobile/arbitrage exit options (they can forum-shop jurisdictions and platforms can operate across borders). Targets of hate speech and harassment derive high d (target end) because they are trapped — they cannot opt out of the shared public sphere where the speech circulates, and the doctrine's narrow category set specifically excludes the harms they experience from any override pathway. Minority communities as an organized class sit closer to the target end despite some organizational power, because their exit options remain constrained: advocacy and counter-speech are available, but the legal system itself offers no direct remedy.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling this doctrine as pure extraction (which would erase the genuine, historically-grounded anti-censorship coordination function that protects politically powerless dissidents) or as pure coordination (which would erase the real, differentially-borne cost to identifiable target groups). The founding problem — protecting dissent from majoritarian suppression — remains partially live (contested status), which is precisely why this is not a pure Snare or a Piton: there is a real ongoing coordination function, but it now runs through the same channel as a real ongoing extraction, and both facts must be held together.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    absolutist_vs_dignity_foreclosure,
    'Does committing to the absolutist reading''s narrow-category, high-threshold structure logically foreclose the dignity_reading''s core premise (that personhood-denying speech is categorically unprotected), or can a single legal tradition hold both in tension across different doctrinal areas?',
    'Examine whether jurisdictions that adopt the absolutist reading as constitutional baseline (e.g., U.S. First Amendment doctrine) can simultaneously carve out dignity-based exceptions without abandoning the absolutist framework''s core commitment — historically, U.S. doctrine has resisted such carve-outs (R.A.V. v. St. Paul), suggesting genuine foreclosure at the constitutional level, though statutory or common-law dignity protections sometimes coexist alongside it.',
    'If the absolutist reading forecloses dignity protections in a single framework, then jurisdictions choosing this reading structurally cannot later adopt dignity-based hate speech restrictions without a doctrinal rupture — this affects how comparative constitutional scholars model doctrinal convergence or divergence across legal systems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolutist_vs_dignity_foreclosure, conceptual, 'Whether the absolutist and dignity readings are logically incompatible within one constitutional framework.').

omega_variable(
    counter_speech_remedy_efficacy,
    'Does the doctrine''s vindicated proposition — that counter-speech is an adequate remedy for harmful speech — actually function as claimed, or is it a legitimating fiction that obscures the real asymmetry in speech reach and resources between speakers and targets?',
    'Empirical study of whether counter-speech measurably mitigates the psychological, reputational, and social harms experienced by targets of vilifying or harassing speech, controlling for platform amplification asymmetries and resource disparities between speakers and targets.',
    'If counter-speech is empirically inadequate as a remedy, the coordination story (marketplace of ideas self-corrects) is substantially weaker than claimed, and the tangled_rope''s extraction component is larger relative to its coordination component than currently authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_speech_remedy_efficacy, empirical, 'Whether counter-speech functions as a real remedy or as cover for uncompensated harm.').

omega_variable(
    narrow_category_stability_under_digital_scale,
    'Was the narrow unprotected-category set (incitement, true threats, defamation, obscenity) calibrated for a pre-digital speech environment, and does it remain fit for purpose when speech can be coordinated, amplified, and targeted at unprecedented scale?',
    'Compare doctrinal harm assumptions embedded in foundational case law (largely mid-20th century) against contemporary evidence on coordinated harassment campaigns, algorithmic amplification, and cumulative dignitary harm from high-volume targeted speech.',
    'If the category set is stale relative to digital-era harm patterns, the rising extractiveness trend in this story''s measurements reflects a genuine functional mismatch, not merely increased speech volume — this would support the T17-style hypothesis that the constraint''s founding calibration has not kept pace with its operating environment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrow_category_stability_under_digital_scale, empirical, 'Whether the doctrine''s category boundaries remain calibrated to contemporary speech-harm dynamics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__absolutist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_harm_boundary__absolutist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(spee_tr_t8, speech_harm_boundary__absolutist_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(spee_tr_t16, speech_harm_boundary__absolutist_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement(spee_tr_t24, speech_harm_boundary__absolutist_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement(spee_tr_t32, speech_harm_boundary__absolutist_reading, theater_ratio, 32, 0.2).
narrative_ontology:measurement(spee_tr_t40, speech_harm_boundary__absolutist_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_harm_boundary__absolutist_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(spee_be_t8, speech_harm_boundary__absolutist_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(spee_be_t16, speech_harm_boundary__absolutist_reading, base_extractiveness, 16, 0.47).
narrative_ontology:measurement(spee_be_t24, speech_harm_boundary__absolutist_reading, base_extractiveness, 24, 0.51).
narrative_ontology:measurement(spee_be_t32, speech_harm_boundary__absolutist_reading, base_extractiveness, 32, 0.55).
narrative_ontology:measurement(spee_be_t40, speech_harm_boundary__absolutist_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_harm_boundary__absolutist_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(spee_su_t8, speech_harm_boundary__absolutist_reading, suppression_requirement, 8, 0.32).
narrative_ontology:measurement(spee_su_t16, speech_harm_boundary__absolutist_reading, suppression_requirement, 16, 0.35).
narrative_ontology:measurement(spee_su_t24, speech_harm_boundary__absolutist_reading, suppression_requirement, 24, 0.37).
narrative_ontology:measurement(spee_su_t32, speech_harm_boundary__absolutist_reading, suppression_requirement, 32, 0.4).
narrative_ontology:measurement(spee_su_t40, speech_harm_boundary__absolutist_reading, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__absolutist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_harm_boundary__absolutist_reading, 0.12).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, harm_balancing_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, dignity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the speech_harm_boundary kernel. absolutist_reading (this file) sets the highest override threshold and narrowest unprotected-category set; harm_balancing_reading sets a lower, more permeable threshold subject to proportionality analysis; dignity_reading replaces the harm-threshold logic entirely with a categorical personhood-based exclusion. All three are linked bidirectionally in network.affects_constraints in each file. The epsilon values differ substantially across the three: this reading is authored at 0.58 (tangled_rope, moderate-high extraction concentrated on trapped targets); the harm_balancing_reading is expected to show lower extraction with higher administrative/theater cost from case-by-case balancing; the dignity_reading is expected to show lower extraction on vilification-target victims but potential new extraction on speakers whose political or artistic speech is swept into the categorical exclusion.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
