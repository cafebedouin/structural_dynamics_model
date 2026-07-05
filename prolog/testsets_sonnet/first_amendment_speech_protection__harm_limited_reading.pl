% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__harm_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_speech_protection__harm_limited_reading, []).

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
 *   constraint_id: first_amendment_speech_protection__harm_limited_reading
 *   human_readable: First Amendment Protection — Harm-Limited Reading
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the harm-limited reading of the First Amendment
 *   speech-protection kernel: the view that protection is not categorical but
 *   yields wherever speech is shown to cause demonstrable, unconsented-to
 *   harm to identifiable people. Under this reading the protected-speech set
 *   contracts around a harm boundary drawn and redrawn by courts. The
 *   beneficiaries are those who can show injury from speech — harassment
 *   victims and targeted minority groups; the victims are speakers,
 *   especially dissidents and provocateurs, whose expression becomes newly
 *   contestable once a harm-showing can defeat protection that would
 *   otherwise be categorical. This is a genuinely distinct constraint from
 *   its siblings: the absolutist reading (categorical protection, narrow
 *   historical exclusions only) and the categorical_balancing reading
 *   (protected/unprotected categories fixed by case-type, not by a
 *   case-by-case harm showing) are separate constraints, generated
 *   separately, each with their own epsilon.
 *
 * KEY AGENTS:
 *   - targeted_minority_groups: Primary beneficiary (powerless/trapped) — gains a doctrinal hook against harmful speech
 *   - controversial_speakers: Primary target (moderate/constrained) — loses categorical protection once harm is shown
 *   - dissident_political_actors: Secondary target (powerless/trapped) — vulnerable to harm standard being weaponized by power
 *   - courts_and_legislatures: Agenda-setter (institutional/analytical) — defines and administers the harm boundary
 *   - civil_liberties_organizations: Analytical observer/excluded — objects to the standard's manipulability but does not control its application
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__harm_limited_reading, 0.42).
domain_priors:suppression_score(first_amendment_speech_protection__harm_limited_reading, 0.55).
domain_priors:theater_ratio(first_amendment_speech_protection__harm_limited_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__harm_limited_reading, "First Amendment Protection — Harm-Limited Reading").
narrative_ontology:topic_domain(first_amendment_speech_protection__harm_limited_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__harm_limited_reading, '86b82286-d0eb-4c69-8969-43c79a75151e').
narrative_ontology:cs_kernel_codification('86b82286-d0eb-4c69-8969-43c79a75151e', fixed_text).
narrative_ontology:cs_authority_grounding('86b82286-d0eb-4c69-8969-43c79a75151e', lineage).
narrative_ontology:cs_interpretation_layer_present('86b82286-d0eb-4c69-8969-43c79a75151e').
narrative_ontology:cs_reading_relation('86b82286-d0eb-4c69-8969-43c79a75151e', first_amendment_speech_protection__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('86b82286-d0eb-4c69-8969-43c79a75151e', first_amendment_speech_protection__categorical_balancing_reading, coexists_with).
narrative_ontology:cs_axiom('86b82286-d0eb-4c69-8969-43c79a75151e', foundational, harm_defeats_categorical_protection).
narrative_ontology:cs_axiom_status(harm_defeats_categorical_protection, holdable).
narrative_ontology:cs_axiom_grounding('86b82286-d0eb-4c69-8969-43c79a75151e', harm_defeats_categorical_protection, deontological).
narrative_ontology:cs_axiom('86b82286-d0eb-4c69-8969-43c79a75151e', secondary, consent_marks_the_speech_harm_boundary).
narrative_ontology:cs_axiom_status(consent_marks_the_speech_harm_boundary, holdable).
narrative_ontology:cs_axiom_grounding('86b82286-d0eb-4c69-8969-43c79a75151e', consent_marks_the_speech_harm_boundary, conventional).
narrative_ontology:cs_reference_frame('86b82286-d0eb-4c69-8969-43c79a75151e', text_as_categorical_shield).
narrative_ontology:cs_drift_state('86b82286-d0eb-4c69-8969-43c79a75151e', contemporary_harassment_jurisprudence_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('86b82286-d0eb-4c69-8969-43c79a75151e', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, targeted_minority_groups).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, harassment_victims).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, vulnerable_speech_targets).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, controversial_speakers).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, dissident_political_actors).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, provocative_artists_and_satirists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, platform_intermediaries).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, platform_intermediaries).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__harm_limited_reading, harm_principle_as_speech_limit).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__harm_limited_reading, consent_based_speech_boundary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the brunt of targeted hate speech, harassment campaigns, and dehumanizing rhetoric. Under this reading, courts and legislatures may restrict speech shown to cause them demonstrable harm — reputational, psychological, or physical. They cannot exit the polity or the discourse; their protection depends entirely on harm being provable and courts being willing to find it.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, targeted_minority_groups, beneficiary,
    powerless, generational, trapped, national).

% Individuals subjected to targeted online or in-person harassment campaigns that are structured as 'speech.' The harm-limited reading gives them a doctrinal hook to seek injunctions or damages once harm is demonstrated, where the absolutist reading would offer none.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, harassment_victims, beneficiary,
    powerless, biographical, trapped, local).

% Speakers whose provocative, offensive, or politically charged expression is now subject to after-the-fact harm review. They face the chilling effect of not knowing in advance whether a court will find their speech harmful enough to lose protection; their only exit is self-censorship or relocation to jurisdictions with stronger absolutist protections.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, controversial_speakers, payer,
    moderate, biographical, constrained, national).

% Political dissidents whose sharp rhetoric against powerful institutions or majorities can be recast as 'harmful' by those institutions, using the harm standard as a tool to suppress the speech most necessary to check power. They have the least resources to litigate a harm-boundary defense.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, dissident_political_actors, payer,
    powerless, biographical, trapped, national).

% Satirists, provocateurs, and transgressive artists whose work depends on causing discomfort or offense. Under the harm-limited reading, the value of their expression is weighed against demonstrated harm, creating unpredictable liability for work that is not obviously political but is culturally disruptive.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, provocative_artists_and_satirists, payer,
    moderate, biographical, constrained, national).

% Administer the harm-limited standard: define what counts as demonstrable harm, what counts as consent, and where the boundary sits. They hold the discretion that makes this reading operate; their evolving harm doctrine determines whose speech survives and whose does not.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, courts_and_legislatures, agenda_setter,
    institutional, generational, analytical, national).

% Free-speech advocacy groups argue the harm standard is inherently manipulable and will be used asymmetrically — powerful actors will claim harm from criticism while genuinely harmed minorities may still fail to meet evidentiary thresholds. Their objections are heard in litigation and commentary but do not control how courts apply the standard.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, civil_liberties_organizations, excluded,
    organized, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(first_amendment_speech_protection__harm_limited_reading, civil_liberties_organizations, observer).

% Social media and publishing platforms must decide what to host under a shifting harm standard. They benefit from a workable legal theory to remove harassment content, but bear liability exposure and compliance costs, and can relocate or restructure services across jurisdictions with different harm thresholds.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, platform_intermediaries, payer,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(first_amendment_speech_protection__harm_limited_reading, platform_intermediaries, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a doctrinal mechanism for weighing the collective interest in open discourse against the concrete, provable harm speech can inflict on specific individuals or groups, allowing the legal system to intervene where speech functions as a vector for injury rather than as contribution to public discourse.
% TRANSFER_FUNCTION: Moves protection away from speakers whose expression is shown to cause demonstrable, unconsented-to harm, and toward the targets of that speech — shifting the burden of the discourse's costs from the harmed party onto the speaker, enforced through injunctions, damages, or content removal.
% ABSENT_VOICES: Speakers whose expression is ambiguous or borderline are not represented in the doctrine's formation — the harm standard is set by courts interpreting past cases, and future speakers bear the uncertainty without a seat in defining where the boundary falls. Marginalized speakers with unpopular views but no direct 'harm' claim may find themselves without either absolutist or harm-based protection.
% DISAPPEARANCE_RATIONALE: If courts abandoned the harm-limited reading overnight in favor of pure absolutism, harassment victims and targeted minority groups would lose their primary legal recourse against speech-based injury, and platforms would face pressure to revert to minimal content moderation; conversely if this reading hardened into the dominant approach, controversial and dissident speech would face substantially higher legal risk and self-censorship would increase measurably.
% FOUNDING_PROBLEM: Pure textual absolutism ('no law' means no law) leaves no doctrinal room to address speech that inflicts concrete, provable injury on identifiable people — harassment, targeted defamation, incitement with clear causal harm — treating all such injury as an acceptable cost of open discourse.
% FOUNDING_PROBLEM_CORROBORATION: Tort law scholars and harassment-victim advocacy organizations outside the direct beneficiary groups attest that demonstrable, unconsented harm from speech is a persistent and measurable phenomenon (documented in civil harassment litigation and defamation case law); however, civil liberties organizations dispute that the harm-limited framework is the correct or safest institutional response, arguing the problem is real but this solution invites abuse by the powerful.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__harm_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__harm_limited_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__harm_limited_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(first_amendment_speech_protection__harm_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__harm_limited_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__harm_limited_reading_tests).
:- end_tests(first_amendment_speech_protection__harm_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) because the harm-limited reading transfers real value — legal recourse and reduced injury — to harmed parties, but it is not a pure coordination gain: dissidents and provocateurs bear a genuine cost in reduced protection and increased legal uncertainty. Suppression (0.55) reflects that the standard requires active judicial and legislative enforcement to hold the boundary and is not self-executing; it has risen over the interval as harm doctrine has hardened into case law and injunctive practice. Theater ratio is comparatively low (0.28) because the harm-finding function is largely substantive rather than performative, though some rise reflects formulaic harm findings in later cases that function more as doctrinal cover than genuine adjudication.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of targeted minority groups and harassment victims, this reading looks like coordination — a needed correction to an absolutist rule that ignored real injury. From the seat of controversial speakers and dissidents, the same doctrine looks like an extraction mechanism: a harm-finding apparatus that can be turned against exactly the speech that most needs protection, since 'harm' is elastic and institutional actors are well-positioned to claim it. The engine's per-seat computation should reflect this divergence rather than resolve it in either direction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary groups (targeted minorities, harassment victims) are powerless but structurally advantaged by this specific doctrinal reading — it exists to protect them, so their directionality sits toward the beneficiary end despite their low general power. Controversial speakers, dissidents, and satirists are declared victims: the reading's core function is to strip their categorical protection once harm is shown, placing them structurally toward the target end regardless of their moderate general power. Courts are the agenda-setting administrator with analytical exit — they do not bear the doctrine's costs or benefits directly but control its content.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (absolutism's blindness to demonstrable harm) remains live — harassment and targeted injury from speech are not settled or historical problems. This blocks a mandatrophy verdict: the doctrine cannot be dismissed as an atrophied vestige defending a solved problem. However, the doctrine's administrators (courts) could, in principle, over-extend the harm standard to cover merely offensive or politically inconvenient speech, which is precisely the risk civil liberties organizations flag — this is the boundary-drift concern captured in the omega variables below, not a mandatrophy question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_standard_manipulability,
    'Does the harm-limited reading''s harm-and-consent test get applied symmetrically, or does it systematically favor powerful claimants able to marshal evidence of harm over less-resourced speakers and less legible harms?',
    'Empirical audit of harm-finding case outcomes by claimant power/resources; track whether harm findings correlate with claimant institutional capacity rather than injury severity.',
    'If systematically asymmetric, the reading functions less as protection for the vulnerable and more as a tool available disproportionately to well-resourced actors, which would shift the classification toward greater extraction from powerless dissidents specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_standard_manipulability, empirical, 'Whether the harm standard is applied symmetrically across claimant power levels.').

omega_variable(
    kernel_reading_selection,
    'Is the harm-limited reading, the absolutist reading, or the categorical-balancing reading the historically and doctrinally dominant interpretation of the First Amendment kernel, and does that dominance shift over time?',
    'Doctrinal history review of Supreme Court speech jurisprudence across eras; track which reading''s logic actually decided landmark cases versus which was invoked rhetorically.',
    'Determines whether this constraint represents the operative legal reality, an emerging minority position, or a contested framework competing for dominance — affects how much weight courts_and_legislatures'' administration should be read as settled versus contested authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Which kernel reading is doctrinally dominant, and whether dominance is stable or shifting.').

omega_variable(
    consent_definition_ambiguity,
    'What counts as ''unconsented-to'' harm from speech — is exposure to public discourse itself a form of implicit consent, or does the harm-limited reading treat all speech recipients as non-consenting by default?',
    'Analysis of how courts applying harm-limited reasoning define consent boundaries — e.g., opt-out mechanisms, public figure doctrine, captive audience doctrine.',
    'A narrow consent definition (most exposure counts as consented) would sharply limit this reading''s practical scope; a broad definition (most exposure is unconsented) would expand it dramatically, changing the effective extractiveness measured here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_definition_ambiguity, conceptual, 'How the consent boundary is drawn determines the practical scope of the harm-limited reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__harm_limited_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t0, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(firs_tr_t10, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(firs_tr_t20, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(firs_tr_t30, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(firs_tr_t40, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(firs_tr_t50, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 50, 0.27).
narrative_ontology:measurement(firs_tr_t60, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(firs_be_t0, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(firs_be_t10, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 10, 0.27).
narrative_ontology:measurement(firs_be_t20, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 20, 0.31).
narrative_ontology:measurement(firs_be_t30, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(firs_be_t40, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement(firs_be_t50, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 50, 0.4).
narrative_ontology:measurement(firs_be_t60, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 60, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t0, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(firs_su_t10, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(firs_su_t20, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(firs_su_t30, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 30, 0.47).
narrative_ontology:measurement(firs_su_t40, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(firs_su_t50, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 50, 0.53).
narrative_ontology:measurement(firs_su_t60, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 60, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__harm_limited_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection__absolutist_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection__categorical_balancing_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposed from the natural-language label 'First Amendment speech protection,' per the ε-invariance principle. absolutist_reading treats protection as categorical with only narrow historical exclusions (near-mountain, minimal extraction). categorical_balancing_reading fixes protected/unprotected status by pre-defined category (obscenity, true threats) rather than case-specific harm showing. harm_limited_reading (this story) makes protection turn on a case-specific harm-and-consent showing, producing meaningfully higher extraction from speakers and a distinct beneficiary/victim structure than either sibling. All three are linked via affects_constraints because a court's adoption of one reading structurally narrows the doctrinal space available to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
