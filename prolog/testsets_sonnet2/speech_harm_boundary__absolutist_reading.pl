% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Speech Harm Boundary — Absolutist Reading (Near-Absolute Protection, Narrow Unprotected Categories)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the absolutist reading of the
 *   speech-harm-boundary kernel: speech protection operates near-absolutely,
 *   with an unprotected-category set narrowed to incitement of imminent
 *   lawless action, true threats, defamation under actual malice, and
 *   obscenity, and an override threshold set so high that almost no showing
 *   of harm short of those categories can restrict speech. The reading
 *   genuinely coordinates against state and majoritarian censorship of
 *   dissent — a real function documented in its founding cases — but the same
 *   high threshold structurally transfers the cost of unaddressed harm onto
 *   individuals and groups who are targeted by speech that falls short of the
 *   carve-outs. This is a tangled rope: courts and civil-liberties
 *   institutions coordinate a genuine anti-censorship function while targets
 *   of hate speech, harassment, and reputational harm pay through the same
 *   doctrinal structure, and the arrangement requires active judicial
 *   enforcement (case-by-case doctrine, standing, procedural bars) to hold at
 *   its current threshold. The dignity_reading and harm_balancing_reading are
 *   separate constraints — not measured here — that would locate ε
 *   differently because they draw the override line elsewhere and shift the
 *   victim set accordingly.
 *
 * KEY AGENTS:
 *   - courts_and_appellate_judiciary: sets and enforces the narrow unprotected-category doctrine (institutional/analytical)
 *   - high_reach_speakers and controversial_publishers: primary beneficiaries of the near-zero liability zone (powerful-organized/arbitrage-mobile)
 *   - targets_of_hate_speech, harassment_targets_online, minority_group_members, defamed_private_individuals_below_threshold: bear harm without remedy (powerless-moderate/trapped-constrained)
 *   - civil_liberties_advocates: institutional defenders of the doctrine's coordination function (organized/analytical)
 *   - legislatures_proposing_harm_carveouts: excluded attempts at a lower threshold (institutional/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__absolutist_reading, 0.58).
domain_priors:suppression_score(speech_harm_boundary__absolutist_reading, 0.32).
domain_priors:theater_ratio(speech_harm_boundary__absolutist_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__absolutist_reading, "Speech Harm Boundary — Absolutist Reading (Near-Absolute Protection, Narrow Unprotected Categories)").
narrative_ontology:topic_domain(speech_harm_boundary__absolutist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_harm_boundary__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__absolutist_reading, 'e631c158-c58d-44e9-998b-2a1bbfb526d0').
narrative_ontology:cs_kernel_codification('e631c158-c58d-44e9-998b-2a1bbfb526d0', fixed_text).
narrative_ontology:cs_authority_grounding('e631c158-c58d-44e9-998b-2a1bbfb526d0', lineage).
narrative_ontology:cs_interpretation_layer_present('e631c158-c58d-44e9-998b-2a1bbfb526d0').
narrative_ontology:cs_reading_relation('e631c158-c58d-44e9-998b-2a1bbfb526d0', speech_harm_boundary__harm_balancing_reading, coexists_with).
narrative_ontology:cs_reading_relation('e631c158-c58d-44e9-998b-2a1bbfb526d0', speech_harm_boundary__dignity_reading, forecloses).
narrative_ontology:cs_axiom('e631c158-c58d-44e9-998b-2a1bbfb526d0', foundational, autonomy_of_speaker_over_downstream_harm).
narrative_ontology:cs_axiom_status(autonomy_of_speaker_over_downstream_harm, holdable).
narrative_ontology:cs_axiom_grounding('e631c158-c58d-44e9-998b-2a1bbfb526d0', autonomy_of_speaker_over_downstream_harm, deontological).
narrative_ontology:cs_axiom('e631c158-c58d-44e9-998b-2a1bbfb526d0', foundational, narrow_categorical_carveouts_only).
narrative_ontology:cs_axiom_status(narrow_categorical_carveouts_only, holdable).
narrative_ontology:cs_axiom_grounding('e631c158-c58d-44e9-998b-2a1bbfb526d0', narrow_categorical_carveouts_only, conventional).
narrative_ontology:cs_axiom('e631c158-c58d-44e9-998b-2a1bbfb526d0', secondary, government_definition_of_harm_is_inherently_suspect).
narrative_ontology:cs_axiom_status(government_definition_of_harm_is_inherently_suspect, holdable).
narrative_ontology:cs_axiom_grounding('e631c158-c58d-44e9-998b-2a1bbfb526d0', government_definition_of_harm_is_inherently_suspect, instrumental).
narrative_ontology:cs_reference_frame('e631c158-c58d-44e9-998b-2a1bbfb526d0', founding_era_anti_sedition_protection).
narrative_ontology:cs_drift_state('e631c158-c58d-44e9-998b-2a1bbfb526d0', contemporary_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e631c158-c58d-44e9-998b-2a1bbfb526d0', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__absolutist_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, high_reach_speakers).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, controversial_publishers).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, political_provocateurs).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, media_institutions).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, targets_of_hate_speech).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, harassment_targets_online).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, minority_group_members).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, defamed_private_individuals_below_threshold).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, civil_liberties_advocates).
narrative_ontology:constraint_vindicates(speech_harm_boundary__absolutist_reading, marketplace_of_ideas_doctrine).
narrative_ontology:constraint_vindicates(speech_harm_boundary__absolutist_reading, content_neutrality_principle).
narrative_ontology:constraint_vindicates(speech_harm_boundary__absolutist_reading, chilling_effect_prevention_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draw and re-draw the narrow unprotected categories (incitement to imminent lawless action, true threats, defamation with actual malice, obscenity) case by case, setting the override threshold so high that almost all speech clears it. Justify the line as necessary to prevent government or majority suppression of disfavored viewpoints. Their doctrine is what makes the near-absolute reading operative law rather than mere norm.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, courts_and_appellate_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Public figures, commentators, and organized advocacy voices with large platforms operate with almost no legal exposure for speech that causes serious reputational, psychological, or social harm to targets, so long as it stops short of incitement or actionable defamation. They can shift venues or platforms freely if one forum restricts them, so the doctrine's protection is a genuine asset to their reach.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, high_reach_speakers, beneficiary,
    powerful, biographical, arbitrage, national).

% Outlets and platforms that monetize provocative or harm-adjacent content rely on the near-absolute standard to avoid liability for content that predictably causes downstream harm short of the narrow carve-outs. They can relocate hosting or incorporate in favorable jurisdictions if regulation tightens.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, controversial_publishers, beneficiary,
    organized, biographical, mobile, national).

% Individuals subjected to sustained public vilification, slurs, or dehumanizing rhetoric that falls short of incitement or a true threat have no legal remedy under this reading; their only recourse is counter-speech or platform-level moderation, both of which they do not control. They cannot exit the public sphere without abandoning employment, community, or voice.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, targets_of_hate_speech, payer,
    powerless, biographical, trapped, local).

% Face coordinated harassment campaigns that individually stay under the true-threat threshold while cumulatively producing severe harm; the doctrine evaluates each utterance in isolation and does not aggregate a pattern into an override. Their exit is deactivation or silence, which itself concedes the contested space.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, harassment_targets_online, payer,
    powerless, immediate, trapped, global).

% Bear the diffuse, dignitary and social costs of a legal environment that tolerates group-disparaging speech as a structural feature rather than an aberration; the harm compounds across incidents without any single incident crossing the override line. They can seek legislative change but cannot exit the jurisdiction's speech regime without emigrating.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, minority_group_members, payer,
    powerless, generational, constrained, national).

% Suffer reputational harm from false statements that do not meet the actual-malice or falsity-plus-damages bar required for defamation liability, especially where they are treated as public figures. Litigation is available in principle but the evidentiary and procedural bar is set deliberately high, so in practice few succeed.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, defamed_private_individuals_below_threshold, payer,
    moderate, biographical, constrained, national).

% Defend the near-absolute standard as the structural safeguard against government censorship of dissent, arguing that any lower threshold would be captured by whoever holds power to define 'harm.' They benefit from the doctrine's persistence as the basis of their institutional mission and litigation practice.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, civil_liberties_advocates, beneficiary,
    organized, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__absolutist_reading, civil_liberties_advocates, observer).

% Periodically attempt to legislate narrower speech protections tied to demonstrated harm (hate speech statutes, harassment statutes) and are repeatedly struck down or narrowed by the judiciary applying the absolutist doctrine. Their preferred balancing framework is not permitted to operate as binding law under this reading.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, legislatures_proposing_harm_carveouts, excluded,
    institutional, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_harm_boundary__absolutist_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_harm_boundary__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, predictable, content-neutral rule that prevents any government or transient majority from redefining 'harmful speech' to suppress dissent, minority viewpoints, or unpopular political positions — solving the coordination problem of who gets to police speech by refusing almost anyone that power.
% TRANSFER_FUNCTION: Moves the cost of resolving speech-related harm from the state and from would-be regulators onto the individuals who are its targets: platforms, courts, and speakers bear near-zero liability for harm short of the narrow categories, while targeted individuals absorb the reputational, psychological, and dignitary costs directly and without institutional remedy.
% ABSENT_VOICES: Targets of persistent sub-threshold harassment and hate speech, and the legislatures attempting to codify narrower harm-based carve-outs on their behalf, are structurally excluded from setting the override threshold — the doctrine is set by courts interpreting precedent, not by those who bear its costs.
% DISAPPEARANCE_RATIONALE: If the near-absolute standard were replaced overnight by a lower harm-override threshold, a substantial volume of currently protected speech would become actionable or restrictable; publishers, platforms, and high-reach speakers would face new liability exposure and likely curtail some content, while previously unprotected targets would gain new civil and possibly criminal remedies. Legislatures currently blocked from enacting harm-based speech statutes would see those statutes survive review.
% FOUNDING_PROBLEM: Historical experience of state and majoritarian suppression of political dissent, minority religious expression, and unpopular ideas — the doctrine was built to prevent government officials or transient majorities from using 'harm' as a pretext to silence disfavored speakers.
% FOUNDING_PROBLEM_CORROBORATION: Civil liberties institutions and constitutional historians attest the suppression risk remains live, citing ongoing government attempts to regulate speech under harm-adjacent pretexts. Independent researchers studying online harassment and hate-speech harms, and comparative scholars of jurisdictions with harm-balancing regimes that have not produced the predicted censorship collapse, attest that the founding problem's continued severity at the current threshold level is contested rather than settled — the corroboration comes from outside both the doctrine's institutional defenders and its critics' advocacy organizations.
narrative_ontology:disappearance_verdict(speech_harm_boundary__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__absolutist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.58 at interval end) is substantial but not extreme: the doctrine's genuine anti-censorship function keeps it well below a pure-snare reading, but the sustained, uncompensated harm absorbed by targets who have no remedy for sub-threshold speech justifies a meaningfully positive score. Suppression is comparatively low (0.32) because the mechanism is not primarily coercive against speakers — it is the ABSENCE of a remedy for targets, not active coercion of them, that constitutes the cost; the coercive element that exists is the doctrine's active blocking of legislative harm-carveouts. Accessibility collapse is moderate (0.4): targets do have some recourse (counter-speech, platform moderation, narrow defamation suits) even if it is structurally inadequate. Resistance is high (0.7): harassment-target advocacy, minority-rights litigation, and periodic legislative attempts to narrow protection are all active, ongoing contestation of this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   High-reach speakers, controversial publishers, and civil liberties advocates sit near the beneficiary end: the doctrine subsidizes their expressive reach and, for advocates, their institutional mission, and their exit options (arbitrage, mobile, analytical) reflect genuine ability to route around any tightening. Targets of hate speech, harassment, and sub-threshold defamation sit near the full-target end: they are trapped or constrained, bear the harm directly, and have no structural exit from the public sphere without personal cost. Courts sit at the agenda-setting seat with analytical exit — they administer the line but do not personally bear its costs or collect its benefits, which is why their seat classification will diverge sharply from both the beneficiary and payer seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — state and majoritarian suppression of dissent — remains partially live (governments continue to attempt speech regulation under harm pretexts), which prevents a clean mandatrophy verdict; this is authored as tangled_rope rather than snare precisely because the coordination function has not fully atrophied. However, the accumulating extraction shown in the measurement series (0.42 → 0.58) documents a doctrine whose harm-absorption cost has grown even as the suppression threat it guards against has evolved in form (from state censorship toward diffuse private harm), which is the divergence this story is measuring rather than resolving.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    censorship_risk_vs_current_harm_severity,
    'Is the state/majoritarian censorship risk that justifies the near-absolute threshold still commensurate with the harm currently being absorbed by targets, or has the balance of risks shifted since the doctrine''s founding cases?',
    'Comparative empirical study of jurisdictions that have adopted narrower harm-balancing standards, tracking whether censorship of dissent/minority viewpoints increased relative to baseline, alongside longitudinal measurement of harm borne by targets under each regime.',
    'If comparative evidence shows harm-balancing regimes do not produce the predicted censorship expansion, the absolutist reading''s coordination justification weakens relative to its extraction cost, supporting reclassification toward snare; if censorship risk is shown to remain acute, the tangled_rope classification''s coordination component is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(censorship_risk_vs_current_harm_severity, empirical, 'Whether the doctrine''s founding risk calculus still matches current conditions.').

omega_variable(
    kernel_reading_selection_stakes,
    'This story is one reading (absolutist) of a three-way contested kernel (speech_harm_boundary). Which reading a legal system adopts is not adjudicated by this story — but the choice determines whose harm counts and whose autonomy is protected. What structural signal would indicate a jurisdiction is drifting from absolutist toward harm_balancing or dignity readings without a formal doctrinal change?',
    'Track lower-court and administrative-body treatment of harm-adjacent speech claims for de facto threshold lowering (e.g., expanding true-threat doctrine, broadening actual-malice findings) that occurs without Supreme Court or equivalent apex-court reversal of the absolutist framework.',
    'Detecting de facto drift toward a sibling reading would mean this story''s ε and victim set are becoming stale even while the formal doctrine (and hence this story''s claimed_type) remains nominally unchanged — a case where the codified kernel and lived practice diverge.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_stakes, conceptual, 'How reading-drift beneath a stable formal doctrine would be detected.').

omega_variable(
    aggregation_of_subthreshold_harm,
    'Should the override threshold evaluate individual utterances in isolation, or aggregate patterns of coordinated/repeated speech (as in sustained harassment campaigns) when determining whether the harm threshold is crossed?',
    'Doctrinal analysis of whether any jurisdiction applying an absolutist-style standard has developed an aggregation doctrine for harassment, and empirical comparison of harm outcomes for targets under isolated-utterance versus aggregated-pattern review.',
    'If aggregation is conceptually incompatible with the absolutist reading''s core commitment to per-utterance content neutrality, harassment_targets_online has no structural path to relief within this reading regardless of cumulative severity — raising the effective extractiveness beyond what per-incident analysis alone suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregation_of_subthreshold_harm, conceptual, 'Whether isolated-utterance review structurally forecloses relief for cumulative harm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__absolutist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_harm_boundary__absolutist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(spee_tr_t8, speech_harm_boundary__absolutist_reading, theater_ratio, 8, 0.09).
narrative_ontology:measurement(spee_tr_t16, speech_harm_boundary__absolutist_reading, theater_ratio, 16, 0.1).
narrative_ontology:measurement(spee_tr_t24, speech_harm_boundary__absolutist_reading, theater_ratio, 24, 0.1).
narrative_ontology:measurement(spee_tr_t32, speech_harm_boundary__absolutist_reading, theater_ratio, 32, 0.11).
narrative_ontology:measurement(spee_tr_t40, speech_harm_boundary__absolutist_reading, theater_ratio, 40, 0.12).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_harm_boundary__absolutist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(spee_be_t8, speech_harm_boundary__absolutist_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(spee_be_t16, speech_harm_boundary__absolutist_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(spee_be_t24, speech_harm_boundary__absolutist_reading, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(spee_be_t32, speech_harm_boundary__absolutist_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(spee_be_t40, speech_harm_boundary__absolutist_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_harm_boundary__absolutist_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(spee_su_t8, speech_harm_boundary__absolutist_reading, suppression_requirement, 8, 0.24).
narrative_ontology:measurement(spee_su_t16, speech_harm_boundary__absolutist_reading, suppression_requirement, 16, 0.27).
narrative_ontology:measurement(spee_su_t24, speech_harm_boundary__absolutist_reading, suppression_requirement, 24, 0.29).
narrative_ontology:measurement(spee_su_t32, speech_harm_boundary__absolutist_reading, suppression_requirement, 32, 0.31).
narrative_ontology:measurement(spee_su_t40, speech_harm_boundary__absolutist_reading, suppression_requirement, 40, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, speech_harm_boundary__harm_balancing_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, speech_harm_boundary__dignity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the speech_harm_boundary kernel. absolutist_reading (this story) authors a narrow unprotected-category set and high ε for speaker/publisher autonomy with targets bearing harm costs. harm_balancing_reading authors a lower override threshold with proportionality balancing, shifting ε and the victim/beneficiary split. dignity_reading authors dignity as a categorical trump over protection, producing a different unprotected-category set (personhood-denying speech) and a correspondingly different ε and victim set. Each story's ε is stable and reading-specific; they are not to be averaged or reconciled into a single 'speech harm boundary' constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
