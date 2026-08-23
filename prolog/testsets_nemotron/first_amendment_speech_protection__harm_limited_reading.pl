% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__harm_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-21
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
 *   constraint_id: first_amendment_speech_protection__harm_limited_reading
 *   human_readable: First Amendment Harm-Limited Reading: Protection Yields to Demonstrable Unconsented Harm
 *   domain: constitutional_law/political_philosophy/speech_regulation
 *
 * SUMMARY:
 *   This constraint story models the harm-limited reading of First Amendment
 *   speech protection: the principle that constitutional protection yields
 *   when speech causes demonstrable, unconsented-to harm. This is one of
 *   three live readings of the contested kernel
 *   'first_amendment_speech_protection.' The harm-limited reading contracts
 *   the protected speech set around a harm boundary, permitting regulation
 *   when harm is proven. Beneficiaries are vulnerable minorities and targeted
 *   communities who gain protection from hate speech, harassment, and
 *   incitement. Victims are speakers whose expression causes such harm (hate
 *   speakers, harassers) and controversial speakers caught by
 *   over-application. The constraint is a tangled rope: it coordinates
 *   protection against genuine harms (coordination function) but
 *   asymmetrically extracts from speakers through boundary uncertainty,
 *   chilling effects, and the state's power to define 'harm' (extraction
 *   function), requiring active judicial and legislative enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__harm_limited_reading, 0.48).
domain_priors:suppression_score(first_amendment_speech_protection__harm_limited_reading, 0.35).
domain_priors:theater_ratio(first_amendment_speech_protection__harm_limited_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__harm_limited_reading, "First Amendment Harm-Limited Reading: Protection Yields to Demonstrable Unconsented Harm").
narrative_ontology:topic_domain(first_amendment_speech_protection__harm_limited_reading, "constitutional_law/political_philosophy/speech_regulation").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__harm_limited_reading, '373a575c-e265-40a2-a1b8-6d713d707874').
narrative_ontology:cs_kernel_codification('373a575c-e265-40a2-a1b8-6d713d707874', formalized).
narrative_ontology:cs_authority_grounding('373a575c-e265-40a2-a1b8-6d713d707874', lineage).
narrative_ontology:cs_interpretation_layer_present('373a575c-e265-40a2-a1b8-6d713d707874').
narrative_ontology:cs_reading_relation('373a575c-e265-40a2-a1b8-6d713d707874', first_amendment_speech_protection__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('373a575c-e265-40a2-a1b8-6d713d707874', first_amendment_speech_protection__categorical_balancing_reading, influences).
narrative_ontology:cs_axiom('373a575c-e265-40a2-a1b8-6d713d707874', foundational, speech_protection_yields_to_demonstrable_harm).
narrative_ontology:cs_axiom_status(speech_protection_yields_to_demonstrable_harm, holdable).
narrative_ontology:cs_axiom_grounding('373a575c-e265-40a2-a1b8-6d713d707874', speech_protection_yields_to_demonstrable_harm, deontological).
narrative_ontology:cs_axiom('373a575c-e265-40a2-a1b8-6d713d707874', foundational, unconsented_harm_triggers_regulation).
narrative_ontology:cs_axiom_status(unconsented_harm_triggers_regulation, holdable).
narrative_ontology:cs_axiom_grounding('373a575c-e265-40a2-a1b8-6d713d707874', unconsented_harm_triggers_regulation, deontological).
narrative_ontology:cs_reference_frame('373a575c-e265-40a2-a1b8-6d713d707874', postwar_harm_principle_framework).
narrative_ontology:cs_drift_state('373a575c-e265-40a2-a1b8-6d713d707874', contemporary_digital_speech_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('373a575c-e265-40a2-a1b8-6d713d707874', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, vulnerable_minorities).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, targeted_communities).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, speakers_causing_harm).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, controversial_speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, legislatures).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, platforms_intermediaries).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, targeted_communities).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, platforms_intermediaries).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__harm_limited_reading, harm_principle_speech_regulation).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__harm_limited_reading, dignity_based_speech_limits).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the brunt of hate speech, targeted harassment, and speech that incites violence against their identity groups. Gain protection from demonstrable harms when the reading is applied. Cannot exit the vulnerability — identity is not a chosen affiliation. Their safety and dignity depend on the harm boundary being enforced.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, vulnerable_minorities, beneficiary,
    powerless, generational, trapped, national).

% Communities systematically targeted by harmful speech (religious minorities, LGBTQ+ communities, racial minorities). Benefit from harm-based limits but also face risk that the harm standard is weaponized against their own counter-speech or advocacy. Exit from targeting is structurally constrained.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, targeted_communities, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(first_amendment_speech_protection__harm_limited_reading, targeted_communities, payer).

% Speakers whose expression crosses the demonstrable harm threshold — hate speech purveyors, targeted harassers, those inciting violence. Bear regulatory costs (injunctions, damages, criminal penalties). Can modify speech to avoid harm threshold but face chilling effects and boundary uncertainty. Exit means self-censorship or platform migration.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, speakers_causing_harm, payer,
    moderate, biographical, constrained, national).

% Speakers engaging in legitimate but controversial discourse (political dissent, academic inquiry, artistic expression) who face over-application of the harm standard. Bear costs of legal defense, platform removal, reputational damage when harm is alleged but not clearly proven. Exit is constrained by professional and social necessity of public speech.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, controversial_speakers, payer,
    moderate, biographical, constrained, national).

% Adjudicate the harm boundary case by case. Define 'demonstrable' and 'unconsented-to' through precedent. Their institutional legitimacy depends on perceived neutrality in drawing the line. Do not bear extraction costs directly but shape the constraint's operation through doctrinal evolution.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, courts_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Enact statutes implementing harm-based speech regulations (hate speech laws, harassment statutes, anti-doxxing laws). Benefit politically from responding to constituent demands for protection. Can calibrate statutory harm thresholds. Exit via repeal or judicial invalidation.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, legislatures, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(first_amendment_speech_protection__harm_limited_reading, legislatures, beneficiary).

% Monitor and litigate the harm boundary from a speech-protective stance. Argue for narrow harm definitions, strict scrutiny, and procedural safeguards. Do not directly bear extraction nor collect rents; their institutional mission is constraining state power over speech.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, civil_liberties_organizations, observer,
    organized, generational, analytical, national).

% Private platforms (social media, hosting providers) that enforce harm-based speech policies under legal pressure or voluntarily. Bear compliance costs, content moderation infrastructure costs, and liability risk. Benefit from legal clarity and safe harbors. Can exit jurisdictions or restrict services but face market pressure to operate globally.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, platforms_intermediaries, payer,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(first_amendment_speech_protection__harm_limited_reading, platforms_intermediaries, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective action problem of protecting vulnerable groups from speech that inflicts demonstrable, unconsented harm — harassment, incitement, targeted hate — where individual resistance is impossible and platform self-regulation is inconsistent. Provides a legal framework for identifying and remedying specific harms rather than relying on categorical bans or absolute immunity.
% TRANSFER_FUNCTION: Moves regulatory burden and speech restriction from vulnerable minorities (who would bear the harm) to speakers whose expression crosses the demonstrable harm threshold. Transfers enforcement authority to courts and platforms. Transfers political capital to legislatures that enact harm-based statutes.
% ABSENT_VOICES: Future speakers whose expression would be chilled by an expansive harm standard but who are not yet identifiable. Marginalized speakers within vulnerable communities who use reclaimed or confrontational language that could be classified as harmful under a broad reading. Foreign speakers subject to extraterritorial application. These voices are structurally excluded from the adjudicative process.
% DISAPPEARANCE_RATIONALE: If the harm-limited reading vanished overnight, the absolutist reading would expand the protected speech set dramatically — hate speech, targeted harassment, and incitement would gain constitutional protection absent narrow historical exceptions. Vulnerable minorities would lose legal recourse against demonstrable harms. Platforms would face pressure to adopt absolutist moderation or face liability for over-removal. The speech regulation landscape would fundamentally reorganize around categorical protection.
% FOUNDING_PROBLEM: The post-WWII recognition that absolute speech protection enables the spread of hate propaganda that facilitates genocide and systemic oppression, combined with the civil rights era recognition that targeted speech inflicts dignitary and material harm on vulnerable groups that the marketplace of ideas cannot remedy.
% FOUNDING_PROBLEM_CORROBORATION: International human rights law (ICCPR Art. 20, ICERD Art. 4) and European constitutional courts corroborate that democracy requires harm-based speech limits. The absolutist reading's proponents (US-centric free speech absolutists) contest whether the founding problem was ever correctly diagnosed or whether the remedy creates greater harm through state censorship power.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__harm_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__harm_limited_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__harm_limited_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(first_amendment_speech_protection__harm_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__harm_limited_reading, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__harm_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__harm_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(first_amendment_speech_protection__harm_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) reflects the reading's regulatory bite: speakers whose expression crosses the harm threshold face real sanctions, and the boundary's indeterminacy creates chilling effects beyond the core. Suppression (0.35) is moderate: the constraint does not categorically ban speech categories but requires case-by-case harm demonstration, leaving alternatives open for speech that stays below the threshold. Theater ratio (0.18) is low: the harm adjudication function is genuine, not performative, though performative enforcement (symbolic prosecutions, platform over-removal) exists. Accessibility collapse (0.32) is modest: alternative regulatory frameworks (categorical balancing, absolutism) remain live and contested. Resistance (0.42) is significant: civil liberties organizations, absolutist jurists, and political movements actively contest the harm boundary's expansion.
 *
 * PERSPECTIVAL GAP:
 *   From the vulnerable minority seat, the constraint is a rope (genuine coordination against harm). From the speaker-causing-harm seat, it is a snare (extraction via vague harm standards). From the controversial speaker seat, it is a tangled rope (coordination function real but extraction via over-application). From the court seat, it is a scaffold (transitional doctrine managing competing commitments). The engine computes these per-seat types from the structural data; the authored claim (tangled_rope) reflects the reading's institutional self-presentation.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable minorities (powerless, trapped) are structural beneficiaries: they gain protection without administering the regime (d near 0.0). Targeted communities (moderate, constrained) are beneficiaries who also face risk of the standard being turned against their counter-speech (d ~0.3). Speakers causing harm (moderate, constrained) are primary targets: they bear sanctions and chilling effects (d ~0.8). Controversial speakers (moderate, constrained) are secondary targets: they bear over-application costs (d ~0.6). Courts (institutional, analytical) are agenda-setters with near-symmetric position (d ~0.5) — they administer but do not personally extract or pay. Legislatures (institutional, arbitrage) are agenda-setters who benefit politically (d ~0.2). Platforms (powerful, mobile) are payers who also benefit from legal clarity (d ~0.4). Civil liberties orgs (organized, analytical) are observers (d ~0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing hate propaganda enabling oppression) remains live but contested. The harm-limited reading has not resolved into pure coordination (rope) because the harm boundary's indeterminacy sustains extraction from speakers. It has not degraded into a snare because the coordination function (protecting vulnerable groups from demonstrable harm) is genuinely operative and vindicated by international law. It is not a piton because active doctrinal development continues — the constraint is not maintained theatrically. Mandatrophy is unresolved: the arrangement's original justification persists but its current operation extracts beyond that justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_demonstrability_standard,
    'What evidentiary standard governs ''demonstrable'' harm — must harm be proven beyond reasonable doubt, by preponderance, or by a lower administrative threshold?',
    'Supreme Court precedent on the mens rea and evidentiary requirements for speech restrictions (e.g., Brandenburg imminence, True Threats standard, counter-speech doctrine). Empirical study of how lower courts apply the standard.',
    'A high standard (beyond reasonable doubt, imminence required) reduces extraction and chilling, moving the reading toward rope. A low standard (preponderance, dignitary harm sufficient) increases extraction and over-application, moving toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_demonstrability_standard, conceptual, 'The evidentiary threshold for ''demonstrable'' harm determines the constraint''s effective extractiveness.').

omega_variable(
    consent_boundary_in_speech,
    'What constitutes ''unconsented-to'' in speech contexts — does presence in a public forum imply consent to offensive speech? Does membership in a targeted group imply non-consent to hate speech?',
    'Philosophical analysis of consent in public discourse; legal precedent on captive audience doctrine, public forum doctrine, and targeted harassment.',
    'A narrow consent concept (public presence = consent) expands protection, reducing extraction. A broad concept (targeted identity = automatic non-consent) contracts protection, increasing extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_boundary_in_speech, conceptual, 'The consent boundary determines the harm-limited reading''s scope and thus its effective extractiveness.').

omega_variable(
    weaponization_risk,
    'How frequently is the harm standard weaponized against marginalized speakers'' counter-speech, reclaimed language, or advocacy?',
    'Empirical study of hate speech law enforcement patterns in jurisdictions with harm-based regimes (Europe, Canada); case law analysis of counter-speech prosecutions.',
    'High weaponization frequency would reveal the constraint as a snare for vulnerable speakers despite its beneficiary framing. Low frequency would support the coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(weaponization_risk, empirical, 'Whether the harm standard is asymmetrically applied against the communities it purports to protect.').

omega_variable(
    kernel_reading_framing_underdetermination,
    'Does the harm-limited reading''s core premise (protection yields to demonstrable harm) logically foreclose the absolutist reading, or do they coexist as competing frameworks?',
    'Structural analysis of whether a single legal framework could simultaneously hold both premises, or whether they are held by different institutional coalitions without logical resolution.',
    'If forecloses, the kernel has a genuine logical fault line. If coexists_with, the contest is political/institutional, not logical — both readings remain live options for different actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing_underdetermination, conceptual, 'Framing under-determination in the kernel: whether sibling readings are logically incompatible or politically competing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__harm_limited_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t1945, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(firs_tr_t1965, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 1965, 0.08).
narrative_ontology:measurement(firs_tr_t1985, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 1985, 0.12).
narrative_ontology:measurement(firs_tr_t1995, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(firs_tr_t2005, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 2005, 0.16).
narrative_ontology:measurement(firs_tr_t2015, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 2015, 0.17).
narrative_ontology:measurement(firs_tr_t2025, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 2025, 0.18).

% Extraction over time
narrative_ontology:measurement(firs_be_t1945, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement(firs_be_t1965, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 1965, 0.22).
narrative_ontology:measurement(firs_be_t1985, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 1985, 0.31).
narrative_ontology:measurement(firs_be_t1995, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 1995, 0.38).
narrative_ontology:measurement(firs_be_t2005, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 2005, 0.41).
narrative_ontology:measurement(firs_be_t2015, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 2015, 0.45).
narrative_ontology:measurement(firs_be_t2025, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 2025, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t1945, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 1945, 0.1).
narrative_ontology:measurement(firs_su_t1965, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 1965, 0.18).
narrative_ontology:measurement(firs_su_t1985, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 1985, 0.25).
narrative_ontology:measurement(firs_su_t1995, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 1995, 0.28).
narrative_ontology:measurement(firs_su_t2005, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 2005, 0.3).
narrative_ontology:measurement(firs_su_t2015, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 2015, 0.33).
narrative_ontology:measurement(firs_su_t2025, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 2025, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__harm_limited_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection__absolutist_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection__categorical_balancing_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel 'first_amendment_speech_protection.' The absolutist_reading and categorical_balancing_reading are sibling constraints with different ε values, beneficiary/victim structures, and claimed types. The harm-limited reading's ε (0.48) is substantially higher than the absolutist_reading's (near 0) because it permits regulation. The categorical_balancing_reading's ε is intermediate. All three share the same referent (First Amendment speech protection doctrine) but instantiate different constraints per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(first_amendment_speech_protection__harm_limited_reading, institutional, 0.25).
constraint_indexing:directionality_override(first_amendment_speech_protection__harm_limited_reading, powerful, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
