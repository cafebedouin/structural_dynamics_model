% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__harm_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: speech_protection_kernel__harm_threshold_reading
 *   human_readable: Harm-Threshold Speech Protection Doctrine
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   The harm-threshold reading of speech protection holds that speech loses
 *   its protected status when it causes demonstrable harm to identifiable
 *   victims. Originating in Mill's harm principle and institutionalized
 *   through doctrines like clear and present danger, imminent lawless action,
 *   true threats, and defamation, this reading has expanded over a century to
 *   cover harassment, hate speech (in some jurisdictions), non-consensual
 *   intimate imagery, and misinformation causing concrete harm. The
 *   constraint operates as a tangled rope: it genuinely coordinates victim
 *   protection against real harms (incitement to violence, targeted threats,
 *   defamatory falsehoods) while simultaneously extracting speech liberty
 *   from speakers — particularly marginalized speakers caught in overbroad
 *   applications. The extraction has accumulated as harm categories expanded
 *   from narrow imminent violence to include emotional distress, dignitary
 *   harm, and structural subordination claims. Theatricality has risen as
 *   platforms perform algorithmic content moderation that mimics judicial
 *   balancing but lacks due process. Suppression requirement has grown as
 *   enforcement shifted from post-hoc liability to pre-emptive filtering.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__harm_threshold_reading, 0.58).
domain_priors:suppression_score(speech_protection_kernel__harm_threshold_reading, 0.62).
domain_priors:theater_ratio(speech_protection_kernel__harm_threshold_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__harm_threshold_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__harm_threshold_reading, "Harm-Threshold Speech Protection Doctrine").
narrative_ontology:topic_domain(speech_protection_kernel__harm_threshold_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__harm_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__harm_threshold_reading, '7ff90987-0d58-45c1-8766-c01b14fc4ec6').
narrative_ontology:cs_kernel_codification('7ff90987-0d58-45c1-8766-c01b14fc4ec6', distributed).
narrative_ontology:cs_authority_grounding('7ff90987-0d58-45c1-8766-c01b14fc4ec6', practice).
narrative_ontology:cs_interpretation_layer_present('7ff90987-0d58-45c1-8766-c01b14fc4ec6').
narrative_ontology:cs_reading_relation('7ff90987-0d58-45c1-8766-c01b14fc4ec6', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('7ff90987-0d58-45c1-8766-c01b14fc4ec6', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('7ff90987-0d58-45c1-8766-c01b14fc4ec6', speech_protection_kernel__dignity_reading, influences).
narrative_ontology:cs_reading_relation('7ff90987-0d58-45c1-8766-c01b14fc4ec6', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('7ff90987-0d58-45c1-8766-c01b14fc4ec6', foundational, demonstrable_harm_overrides_speaker_autonomy).
narrative_ontology:cs_axiom_status(demonstrable_harm_overrides_speaker_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('7ff90987-0d58-45c1-8766-c01b14fc4ec6', demonstrable_harm_overrides_speaker_autonomy, empirically_contingent).
narrative_ontology:cs_axiom('7ff90987-0d58-45c1-8766-c01b14fc4ec6', secondary, harm_threshold_is_judicially_administrable).
narrative_ontology:cs_axiom_status(harm_threshold_is_judicially_administrable, holdable).
narrative_ontology:cs_axiom_grounding('7ff90987-0d58-45c1-8766-c01b14fc4ec6', harm_threshold_is_judicially_administrable, conventional).
narrative_ontology:cs_reference_frame('7ff90987-0d58-45c1-8766-c01b14fc4ec6', classical_liberal_harm_principle).
narrative_ontology:cs_drift_state('7ff90987-0d58-45c1-8766-c01b14fc4ec6', contemporary_digital_harm_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7ff90987-0d58-45c1-8766-c01b14fc4ec6', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, victims_of_harmful_speech).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, targeted_groups).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, speakers_restricted_by_harm_threshold).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, political_dissidents_caught_in_overbreadth).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, targeted_groups).
narrative_ontology:constraint_vindicates(speech_protection_kernel__harm_threshold_reading, harm_principle_as_speech_limit).
narrative_ontology:constraint_vindicates(speech_protection_kernel__harm_threshold_reading, state_interest_in_preventing_harm).
narrative_ontology:constraint_vindicates(speech_protection_kernel__harm_threshold_reading, victim_protection_justifies_speech_restriction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who suffer demonstrable harm from speech (defamation, harassment, incitement, true threats). They gain protection when courts/platforms recognize their harm claims and restrict the offending speech. Their exit from the constraint is limited — they cannot opt out of being potential victims, but they can seek legal remedies or platform enforcement.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, victims_of_harmful_speech, beneficiary,
    moderate, biographical, constrained, national).

% Groups historically targeted by hate speech, hate crimes, and structural vilification. They benefit from harm-threshold doctrines that recognize group-based harm. However, they also pay when overbroad harm definitions are used to restrict their own counter-speech or protest. Their identity-locked exit means they cannot leave the structural position that makes them both primary beneficiaries and collateral payers.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, targeted_groups, beneficiary,
    moderate, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__harm_threshold_reading, targeted_groups, payer).

% Speakers whose expression is restricted because it crosses the demonstrable harm threshold — including protesters, artists, journalists, and ordinary citizens. They bear the cost of self-censorship, legal liability, or platform removal. Their exit is constrained: they can modify speech to avoid the threshold, but cannot exit the jurisdiction of the doctrine without leaving the polity or platform.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, speakers_restricted_by_harm_threshold, payer,
    moderate, biographical, constrained, national).

% Marginalized speakers whose legitimate dissent, whistleblowing, or minority viewpoints are suppressed under harm-threshold doctrines that expand 'harm' to include offense to power, national security claims, or majority sensibilities. They have minimal exit options — they cannot access alternative forums effectively and face state retaliation for continued speech.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, political_dissidents_caught_in_overbreadth, payer,
    powerless, biographical, trapped, national).

% Judicial bodies (Supreme Court, lower courts), administrative agencies (FCC, FTC), and legislative bodies that define, calibrate, and enforce the harm threshold. They set the doctrinal tests (clear and present danger, imminent lawless action, true threats, harassment standards), adjudicate cases, and determine which harms are 'demonstrable.' They can shift the threshold through precedent and rulemaking.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, courts_and_regulators, agenda_setter,
    institutional, generational, arbitrage, national).

% Private platforms (social media, hosting providers) that implement harm-threshold policies at scale — hate speech, harassment, misinformation, violence incitement policies. They operationalize the doctrine through terms of service, algorithmic enforcement, and human review. They have mobile exit (can change policies, jurisdiction-shop) but face regulatory pressure to maintain harm thresholds.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, platform_content_moderators, agenda_setter,
    institutional, biographical, mobile, global).

% Civil liberties organizations, legal scholars, and activists who argue speech protection should be near-categorical and listener harm is not grounds for restriction. They are structurally excluded from the harm-threshold framework because their position rejects its foundational premise. They litigate, advocate, and publish but cannot participate in calibrating a threshold they deny should exist.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, absolutist_free_speech_advocates, excluded,
    organized, civilizational, analytical, national).

% Academics, comparativists, and international bodies (UN Special Rapporteurs, Venice Commission) who study harm-threshold doctrines across jurisdictions. They analyze doctrinal evolution, empirical effects, and normative coherence. They neither collect nor pay — they observe and critique from outside the constraint's operational structure.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, legal_scholars_and_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a structured mechanism for adjudicating when speech causes demonstrable harm to identifiable victims, enabling courts and platforms to restrict specific categories of harmful expression (incitement, true threats, defamation, targeted harassment) while preserving a residual sphere of protected speech.
% TRANSFER_FUNCTION: Transfers speech liberty from speakers to victim protection: when a speaker's expression meets the demonstrable harm threshold, the speaker loses the liberty to make that expression, and the victim gains protection from that harm. The transfer is mediated by institutional gatekeepers (courts, platforms) who define and apply the threshold.
% ABSENT_VOICES: Absolutist free speech advocates who reject any harm-based restriction; marginalized speakers whose counter-speech and protest are swept up by overbroad harm definitions; future generations who inherit a narrowed speech environment; speakers in jurisdictions without strong institutional capacity to adjudicate harm fairly.
% DISAPPEARANCE_RATIONALE: If the harm-threshold doctrine vanished overnight, speech protection would default toward either absolutist or marketplace models. Victims of incitement, true threats, defamation, and targeted harassment would lose doctrinally recognized protection. Courts and platforms would lose their primary framework for content-based restrictions. The entire architecture of 'unprotected categories' would collapse, requiring new justifications for any speech regulation.
% FOUNDING_PROBLEM: The classical liberal problem of reconciling free expression with the tangible harms speech can inflict on individuals — defamation destroying reputation, incitement provoking violence, threats coercing compliance, harassment denying equal participation. The harm threshold was built to draw a line: speech causing demonstrable, particularized harm to identifiable victims falls outside protection.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem remains live per: (1) Supreme Court majorities and dissents across eras (Schenck, Brandenburg, Virginia v. Black, Counterman) explicitly debating the harm threshold's scope; (2) International human rights bodies (UN HRC General Comment 34, ECtHR Handyside, Delfi) affirming harm-based restrictions as legitimate while contesting their boundaries; (3) Platform policy teams at Meta, Google, X/Twitter documenting daily adjudication of harm-threshold edge cases; (4) Empirical researchers documenting both chilling effects and measurable harm reduction — none of these sources are beneficiaries of the restriction regime.
narrative_ontology:disappearance_verdict(speech_protection_kernel__harm_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__harm_threshold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__harm_threshold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_kernel__harm_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__harm_threshold_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__harm_threshold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__harm_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_kernel__harm_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58) reflects the substantial speech liberty transferred to victim protection — the doctrine now covers categories unimaginable in 1919 (online harassment, revenge porn, algorithmic amplification of harmful content). Suppression (0.62) captures active enforcement: courts issue injunctions, platforms deploy automated takedowns, states criminalize hate speech. Theater ratio (0.38) reflects the gap between the doctrine's stated precision ('demonstrable harm') and its operational vagueness — balancing tests, contextual factors, and platform-scale moderation produce inconsistent outcomes that perform protection while extracting broadly. Accessibility collapse (0.45) is moderate: alternatives exist (absolutist, marketplace, dignity frameworks) but the harm threshold dominates institutional practice. Resistance (0.55) is significant: absolutist litigation, platform policy battles, academic critique, and international pushback all contest the threshold's expansion.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (courts), the constraint appears as genuine coordination — a principled balance between speech and harm, refined through precedent. From the payer seat (restricted speakers), it appears as extraction — a moving target that increasingly captures legitimate expression. From the excluded seat (absolutists), it appears as a snare — a cover story for state/platform control. The engine will compute these divergences from the structural data; this commentary explains why they exist structurally.
 *
 * DIRECTIONALITY LOGIC:
 *   Victims and targeted groups are structural beneficiaries (d near 0.0-0.2): the constraint subsidizes their protection, they collect safety without running the adjudication machinery. Speakers restricted by the threshold are targets (d near 0.7-0.9): they pay the liberty cost, face constrained exit. Political dissidents caught in overbreadth are fully trapped targets (d near 1.0): powerless, no exit, identity-locked in their vulnerability. Courts and platforms are agenda_setters with arbitrage-grade exit (d near 0.1-0.2): they administer the constraint, can shift its calibration, face minimal personal cost. Absolutist advocates are excluded (analytical exit): they observe but cannot participate in threshold-setting. The directionality derivation from beneficiary/victim declarations plus power/exit produces the expected seat divergence: beneficiary seats compute toward rope/mountain; payer seats compute toward snare/tangled_rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (balancing speech against demonstrable harm) remains live — new technologies create new harm vectors (deepfakes, doxxing, algorithmic amplification). However, the constraint exhibits mandatrophy signals: (1) the harm threshold has expanded far beyond its founding core (imminent violence → emotional distress → structural subordination); (2) the coordination function (protecting identifiable victims from particularized harm) has been stretched to cover diffuse, probabilistic, and collective harms; (3) enforcement has shifted from judicial post-hoc review to platform pre-emption, changing the constraint's character without formal revision. The mandate has not been resolved — it has drifted. The constraint persists because the founding problem is live, but its current form exceeds what the founding problem justifies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_threshold_demonstrability,
    'What counts as ''demonstrable'' harm — must it be empirically quantified, or do judicial notice, legislative findings, and platform policy judgments suffice?',
    'Comparative analysis of evidentiary standards across jurisdictions (US strict scrutiny vs. European margin of appreciation vs. platform internal standards) and empirical studies of enforcement outcomes.',
    'If ''demonstrable'' requires rigorous empirical proof, the constraint narrows toward rope; if legislative/platform judgment suffices, it widens toward snare. Determines whether extraction is bounded by evidence or discretionary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_threshold_demonstrability, conceptual, 'Epistemic standard for harm demonstration — the boundary between coordination and extraction.').

omega_variable(
    overbreadth_vs_underprotection_tradeoff,
    'Does the harm-threshold doctrine''s current calibration produce more overbreadth (suppressing protected speech) or underprotection (failing to restrict harmful speech)?',
    'Systematic coding of false positives (protected speech restricted) and false negatives (harmful speech permitted) across a representative case corpus, with inter-coder reliability.',
    'If overbreadth dominates, the constraint leans snare; if underprotection dominates, it leans rope; if both are substantial and asymmetric across speaker groups, it confirms tangled_rope with identity-locked payer seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(overbreadth_vs_underprotection_tradeoff, empirical, 'Calibration accuracy of the harm threshold — whether it solves the coordination problem or primarily extracts.').

omega_variable(
    platform_vs_judicial_harm_definition,
    'Are platform harm-threshold policies (hate speech, harassment, misinformation) convergent with or divergent from judicial doctrines, and does the divergence create a parallel extraction regime?',
    'Taxonomy mapping of platform policy categories to legal unprotected categories; analysis of cases where platforms restrict speech courts would protect and vice versa.',
    'If platforms enforce a broader, less accountable harm threshold, the constraint''s effective extraction is higher than judicial doctrine alone suggests — a dual-regime extraction. If convergent, the constraint is more coherent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_vs_judicial_harm_definition, empirical, 'Institutional fragmentation of the harm threshold — one constraint or two?').

omega_variable(
    kernel_reading_foreclosure_boundary,
    'Does the harm_threshold_reading''s core premise (demonstrable harm overrides speaker autonomy) logically foreclose the absolutist_reading within a single legal framework, or can a framework accommodate both as context-dependent exceptions?',
    'Doctrinal analysis of whether any jurisdiction simultaneously maintains a categorical speech protection principle and a harm-threshold exception without contradiction — examining constitutional text, precedent structure, and theoretical coherence.',
    'If forecloses, the kernel has a structural fault line — frameworks must choose. If coexists_with, the kernel tolerates persistent pluralism. Determines the reading_relation classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_boundary, conceptual, 'Logical compatibility of harm-threshold and absolutist readings within one framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__harm_threshold_reading, 0, 105).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sp_harm_threshold_tr_t0, speech_protection_kernel__harm_threshold_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sp_harm_threshold_tr_t20, speech_protection_kernel__harm_threshold_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(sp_harm_threshold_tr_t40, speech_protection_kernel__harm_threshold_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(sp_harm_threshold_tr_t60, speech_protection_kernel__harm_threshold_reading, theater_ratio, 60, 0.32).
narrative_ontology:measurement(sp_harm_threshold_tr_t80, speech_protection_kernel__harm_threshold_reading, theater_ratio, 80, 0.35).
narrative_ontology:measurement(sp_harm_threshold_tr_t105, speech_protection_kernel__harm_threshold_reading, theater_ratio, 105, 0.38).

% Extraction over time
narrative_ontology:measurement(sp_harm_threshold_be_t0, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sp_harm_threshold_be_t20, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(sp_harm_threshold_be_t40, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(sp_harm_threshold_be_t60, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement(sp_harm_threshold_be_t80, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 80, 0.55).
narrative_ontology:measurement(sp_harm_threshold_be_t105, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 105, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sp_harm_threshold_su_t0, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(sp_harm_threshold_su_t20, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(sp_harm_threshold_su_t40, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(sp_harm_threshold_su_t60, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 60, 0.58).
narrative_ontology:measurement(sp_harm_threshold_su_t80, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 80, 0.6).
narrative_ontology:measurement(sp_harm_threshold_su_t105, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 105, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__harm_threshold_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_kernel__harm_threshold_reading, 0.12).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__democratic_participation_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, platform_content_moderation_regime).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, defamation_law).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, incitement_law).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, true_threats_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one of five readings of the speech_protection_kernel. The harm_threshold_reading centers victim harm as the override trigger; the absolutist_reading rejects harm as a ground; the marketplace_reading substitutes counterspeech; the dignity_reading centers structural subordination; the democratic_participation_reading centers political necessity. All five share the kernel but instantiate different constraints with different ε, beneficiaries, and victims. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_kernel__harm_threshold_reading, institutional, 0.15).
constraint_indexing:directionality_override(speech_protection_kernel__harm_threshold_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
