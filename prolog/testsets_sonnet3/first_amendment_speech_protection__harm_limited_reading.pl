% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__harm_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: first_amendment_speech_protection__harm_limited_reading
 *   human_readable: Harm-Limited Reading of First Amendment Speech Protection
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates ONE of three contested readings of the First
 *   Amendment speech-protection kernel: the harm-limited reading, under which
 *   constitutional protection yields whenever speech causes demonstrable,
 *   unconsented-to harm. Under this reading, courts and legislatures build a
 *   doctrinal mechanism that contracts the protected-speech set around a harm
 *   boundary — protection is conditional, not categorical, and conditional on
 *   proof of injury rather than on membership in a small set of historically
 *   unprotected categories. This is NOT the absolutist reading (categorical
 *   protection except narrow historical exclusions) and NOT the
 *   categorical-balancing reading (ad hoc weighing of speech value against
 *   harm without a harm-threshold trigger); those are separate constraints
 *   with their own ε and stakeholder structures, linked here via
 *   network.affects_constraints. The harm-limited reading genuinely
 *   coordinates a real problem — remediable injury from speech that the
 *   absolutist baseline leaves unaddressed — while also creating a mechanism
 *   that, once harm is legally cognizable and contestable, can be invoked
 *   asymmetrically against disfavored or marginal speakers whose expression
 *   is provocative rather than genuinely injurious.
 *
 * KEY AGENTS:
 *   - targeted_minority_groups: primary beneficiary (powerless/trapped) — gains a legal remedy for injuries the absolutist reading would leave unaddressed
 *   - controversial_speakers: primary target (moderate/constrained) — bears liability exposure and chilling effects when harm is claimed against their expression
 *   - courts_and_legislatures: agenda_setter (institutional/analytical) — administers the harm threshold and controls its expansion or contraction
 *   - free_speech_absolutists: excluded voice (organized/constrained) — objects that the harm standard is manipulable but is sidelined once this reading becomes controlling doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__harm_limited_reading, 0.42).
domain_priors:suppression_score(first_amendment_speech_protection__harm_limited_reading, 0.55).
domain_priors:theater_ratio(first_amendment_speech_protection__harm_limited_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__harm_limited_reading, "Harm-Limited Reading of First Amendment Speech Protection").
narrative_ontology:topic_domain(first_amendment_speech_protection__harm_limited_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__harm_limited_reading, '63783896-0419-4735-bffe-3b25cdaae4b9').
narrative_ontology:cs_kernel_codification('63783896-0419-4735-bffe-3b25cdaae4b9', fixed_text).
narrative_ontology:cs_authority_grounding('63783896-0419-4735-bffe-3b25cdaae4b9', lineage).
narrative_ontology:cs_interpretation_layer_present('63783896-0419-4735-bffe-3b25cdaae4b9').
narrative_ontology:cs_reading_relation('63783896-0419-4735-bffe-3b25cdaae4b9', first_amendment_speech_protection__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('63783896-0419-4735-bffe-3b25cdaae4b9', first_amendment_speech_protection__categorical_balancing_reading, coexists_with).
narrative_ontology:cs_axiom('63783896-0419-4735-bffe-3b25cdaae4b9', foundational, demonstrated_harm_defeats_speech_protection).
narrative_ontology:cs_axiom_status(demonstrated_harm_defeats_speech_protection, holdable).
narrative_ontology:cs_axiom_grounding('63783896-0419-4735-bffe-3b25cdaae4b9', demonstrated_harm_defeats_speech_protection, empirically_contingent).
narrative_ontology:cs_axiom('63783896-0419-4735-bffe-3b25cdaae4b9', secondary, unconsented_injury_is_the_operative_trigger_not_offense).
narrative_ontology:cs_axiom_status(unconsented_injury_is_the_operative_trigger_not_offense, holdable).
narrative_ontology:cs_axiom_grounding('63783896-0419-4735-bffe-3b25cdaae4b9', unconsented_injury_is_the_operative_trigger_not_offense, deontological).
narrative_ontology:cs_reference_frame('63783896-0419-4735-bffe-3b25cdaae4b9', harm_principle_constitutional_baseline).
narrative_ontology:cs_drift_state('63783896-0419-4735-bffe-3b25cdaae4b9', contemporary_digital_harassment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('63783896-0419-4735-bffe-3b25cdaae4b9', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, targeted_minority_groups).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, harassment_victims).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, defamation_plaintiffs).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, controversial_speakers).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, advocacy_organizations_at_margin).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, provocative_journalists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, defamation_plaintiffs).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__harm_limited_reading, harm_principle_as_speech_limit).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__harm_limited_reading, unconsented_dignitary_injury_is_cognizable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the brunt of hate speech, harassment campaigns, and targeted defamation. Under this reading, courts and legislatures can recognize demonstrated psychological, reputational, or physical harm as sufficient to withdraw protection from the speech that caused it. They cannot exit the harm by leaving the jurisdiction of public discourse; the reading gives them a legal handle they otherwise lack.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, targeted_minority_groups, beneficiary,
    powerless, biographical, trapped, national).

% Individuals subjected to coordinated online or offline harassment campaigns framed by harassers as protected expression. This reading lets them seek injunctions or damages once harm is demonstrated, rather than needing to fit their claim into narrow historical unprotected categories (true threats, fighting words).
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, harassment_victims, beneficiary,
    powerless, immediate, trapped, local).

% Private and public figures harmed by false statements. They benefit from a harm-triggered contraction of protection, but bear litigation costs and time when courts still require them to establish falsity, fault, and injury under this reading's evidentiary demands.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, defamation_plaintiffs, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(first_amendment_speech_protection__harm_limited_reading, defamation_plaintiffs, payer).

% Political commentators, satirists, and dissidents whose speech provokes strong reactions. Under this reading their protection is contingent on downstream harm not materializing or not being provable — a standard they cannot fully control in advance, since audience reaction is partly outside their power. Self-censorship is a rational response to uncertain liability.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, controversial_speakers, payer,
    moderate, biographical, constrained, national).

% Groups engaged in confrontational protest speech, boycotts, or public naming-and-shaming campaigns against powerful actors. They face the risk that their targets will characterize reputational or economic harm as sufficient to trigger regulation, chilling tactics that have historically been core to their advocacy.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, advocacy_organizations_at_margin, payer,
    organized, generational, constrained, national).

% Reporters and commentators whose investigative or opinion work exposes uncomfortable truths about identifiable individuals or institutions. The harm-limited reading exposes them to liability whenever a subject can demonstrate reputational or emotional injury, regardless of the speech's public value, unless they can affirmatively establish a countervailing public-interest defense.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, provocative_journalists, payer,
    moderate, biographical, constrained, national).

% Adjudicate what counts as demonstrable, unconsented-to harm sufficient to withdraw protection. They administer the line-drawing, decide evidentiary standards for 'demonstrable harm,' and can expand or contract the doctrine case by case, which is the actual site of enforcement.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, courts_and_legislatures, agenda_setter,
    institutional, generational, analytical, national).

% Civil libertarians who hold that any harm-triggered contraction is a slippery slope that swallows the categorical protection entirely. Their objection is that 'harm' is infinitely expandable and that this reading effectively hands censorship power to whoever can plausibly claim injury; they are structurally sidelined once a court adopts the harm-limited framework as controlling doctrine.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, free_speech_absolutists, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(first_amendment_speech_protection__harm_limited_reading, diffuse).
narrative_ontology:fixing_cost_class(first_amendment_speech_protection__harm_limited_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal mechanism to recognize and remedy real, demonstrated injuries caused by speech — harassment, targeted defamation, incitement with concrete fallout — that the categorical and absolutist readings would leave entirely unaddressed or address only through narrow historical exceptions.
% TRANSFER_FUNCTION: Moves legal standing and remedy availability from speakers whose expression is later found harmful to the people who can demonstrate they were harmed; shifts the evidentiary and reputational risk of speech onto the speaker rather than leaving it entirely on the target.
% ABSENT_VOICES: Free speech absolutists and civil libertarian organizations who would argue the harm standard is manipulable and that its adoption erodes the categorical baseline; they participate in academic and advocacy discourse but are structurally outside the courts' adjudicative process once a jurisdiction adopts this reading as controlling doctrine.
% DISAPPEARANCE_RATIONALE: If the harm-limited reading disappeared and courts reverted strictly to the absolutist or narrow-categorical baseline, harassment and targeted-defamation victims would lose a significant portion of their current legal remedies, and speech currently chilled by liability exposure (aggressive commentary, provocative journalism) would expand; the compensation and injunction landscape for demonstrated speech-caused harm would visibly contract.
% FOUNDING_PROBLEM: Courts needed a way to address speech that produces concrete, identifiable injury (targeted harassment, defamation, incitement fallout) without either leaving victims remediless under an absolutist reading or resorting to unpredictable ad hoc balancing that offers no stable notice to speakers.
% FOUNDING_PROBLEM_CORROBORATION: Harm-limited proponents (civil rights litigators, tort scholars) attest the founding problem is live — harassment and targeted defamation remain widespread and undercompensated. Free speech absolutist scholars and journalism advocacy organizations, situated outside the beneficiary set, attest the doctrine has drifted from remedying demonstrated injury toward chilling disfavored viewpoints, citing expanding harm definitions in recent case law as evidence the original problem has become a vehicle for broader speech suppression.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__harm_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__harm_limited_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__harm_limited_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.42 at interval end) and rising: the doctrine begins as a genuine remedy mechanism for demonstrated injury and drifts, as harm definitions broaden in case law, toward capturing more speech under liability exposure than the founding problem strictly required. Suppression is meaningfully above baseline (0.55) because the chilling effect on controversial and provocative speech is a structural feature, not an incidental cost — speakers must self-censor against uncertain future harm findings. Theater ratio stays low throughout (0.1 to 0.2) because the enforcement mechanism (litigation, injunctions, statutory harm claims) is substantively functional, not performative — courts are actually adjudicating real harm claims, not merely ritualizing legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (harassment and defamation victims), the harm-limited reading looks like coordination — a genuine remedy for real injury that the categorical baseline denies them. From the payer seats (controversial speakers, advocacy organizations, journalists), the same doctrine looks like extraction: an open-ended liability standard that can be invoked whenever a target claims injury, regardless of the speech's underlying value. The engine computes these as structurally different experiences of the same arrangement rather than reconciling them to a single verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Targeted minority groups, harassment victims, and defamation plaintiffs sit near the beneficiary end of directionality: the doctrine subsidizes their capacity to seek remedy, and their exit from the underlying harm (being targeted) is otherwise trapped. Controversial speakers, advocacy organizations engaged in confrontational tactics, and provocative journalists sit near the target end: they bear the liability risk and chilling effect the doctrine imposes, with only constrained exit (self-censorship or costly litigation defense) available to them. Courts and legislatures are the agenda-setting institutional seat that administers where the harm line falls and can move it in either direction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (remediable injury from targeted speech going unaddressed) remains partially live — harassment and defamation persist — which argues against full mandatrophy. But the corroboration split matters: outside observers (free speech scholars, journalism advocates) attest the doctrine has expanded past its founding scope into a general-purpose speech-suppression tool, which is the classic tangled-rope signature — genuine coordination function coexisting with asymmetric extraction that requires active enforcement (ongoing litigation and injunctive machinery) to sustain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_threshold_expandability,
    'Is the ''demonstrable, unconsented-to harm'' threshold a stable, bounded category, or does it structurally tend to expand as courts and legislatures apply it to new fact patterns?',
    'Longitudinal case-law analysis tracking whether the category of legally cognizable ''harm'' from speech has widened over successive rulings, and whether the expansion rate correlates with plaintiff power or speaker disfavor.',
    'If the threshold is stable, this reading functions closer to a genuine tangled rope with a bounded extraction component; if it structurally expands, the reading trends toward snare as more speech becomes liability-exposed over time regardless of the founding problem''s actual scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_threshold_expandability, empirical, 'Whether the harm threshold is a stable boundary or a structurally expanding one.').

omega_variable(
    reading_selection_as_political_outcome,
    'Is the adoption of the harm-limited reading over the absolutist or categorical-balancing readings a matter of neutral constitutional interpretation, or does the selection itself track which political and social coalitions currently hold institutional power to litigate harm claims?',
    'Comparative analysis of which reading dominant courts adopt in different eras and jurisdictions, cross-referenced with which coalitions were positioned to benefit from harm-based liability at that time.',
    'If reading-selection tracks political power rather than interpretive principle, the entire kernel contest is itself an arena of extraction rather than genuine doctrinal disagreement, which would recontextualize all three sibling constraints as instruments in a broader struggle rather than independent good-faith readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_as_political_outcome, conceptual, 'Whether kernel-reading selection reflects neutral interpretation or coalition power.').

omega_variable(
    consent_as_the_operative_boundary,
    'Does ''unconsented-to'' do genuine independent work in this reading (distinguishing it from a pure harm-balancing test), or does it collapse into harm once courts operationalize the standard?',
    'Doctrinal analysis of whether courts applying this reading treat consent as a distinct, dispositive element (e.g., assumption of risk in public discourse) or merely as a rhetorical gloss on the harm finding.',
    'If consent does independent work, this reading is more genuinely distinct from categorical_balancing_reading than it might appear; if it collapses into harm, the two readings may converge in practice despite differing in their stated frameworks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_as_the_operative_boundary, conceptual, 'Whether the consent element is doctrinally load-bearing or merely rhetorical.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__harm_limited_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t0, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(firs_tr_t8, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(firs_tr_t16, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 16, 0.14).
narrative_ontology:measurement(firs_tr_t24, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 24, 0.16).
narrative_ontology:measurement(firs_tr_t32, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 32, 0.18).
narrative_ontology:measurement(firs_tr_t40, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(firs_be_t0, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(firs_be_t8, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(firs_be_t16, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 16, 0.33).
narrative_ontology:measurement(firs_be_t24, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 24, 0.37).
narrative_ontology:measurement(firs_be_t32, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(firs_be_t40, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t0, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(firs_su_t8, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 8, 0.41).
narrative_ontology:measurement(firs_su_t16, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 16, 0.46).
narrative_ontology:measurement(firs_su_t24, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 24, 0.5).
narrative_ontology:measurement(firs_su_t32, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 32, 0.53).
narrative_ontology:measurement(firs_su_t40, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__harm_limited_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(first_amendment_speech_protection__harm_limited_reading, 0.1).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection__absolutist_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection__categorical_balancing_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints decomposing the colloquial 'First Amendment speech protection' concept per the ε-invariance principle. absolutist_reading treats protection as categorical with only narrow historical carve-outs (lowest ε, minimal beneficiary/victim structure). categorical_balancing_reading treats the protected/unprotected line as a case-by-case value-versus-harm weighing without a harm-threshold trigger. harm_limited_reading (this story) treats protection as contracting specifically and only around demonstrated, unconsented-to harm — a narrower, more procedurally bounded mechanism than open-ended balancing but a broader contraction than absolutism permits. Each reading has its own ε, beneficiary/victim structure, and classification; they are not the same constraint viewed from different angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
