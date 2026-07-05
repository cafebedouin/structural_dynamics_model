% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__absolutist_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: speech_protection_kernel__absolutist_reading
 *   human_readable: Absolutist Reading of Speech Protection (Categorical, Speaker-Autonomy-Maximizing)
 *   domain: Constitutional Law / Political Philosophy / Communication Rights
 *
 * SUMMARY:
 *   This story instantiates the absolutist reading of the speech-protection
 *   kernel: speech protection operates near-categorically, structured around
 *   a small set of fixed exclusions (incitement to imminent lawless action,
 *   true threats, fraud, obscenity as narrowly defined), and listener harm
 *   claims outside those categories are not cognizable grounds for
 *   restriction. This is one reading among five siblings sharing the same
 *   kernel text and doctrinal lineage; the harm_threshold, marketplace,
 *   dignity, and democratic_participation readings are separate constraint
 *   stories with their own ε and stakeholder structures, not alternative
 *   measurements of this one. Under the absolutist reading, extraction is
 *   comparatively low and suppression is comparatively low relative to its
 *   siblings — the coordination function (predictable protection against
 *   selective state suppression) is the dominant structural feature. Where
 *   extraction exists, it is diffuse and displaced onto targeted groups whose
 *   harm claims are foreclosed by the categorical boundary itself, not by any
 *   active enforcement action against them.
 *
 * KEY AGENTS:
 *   - unpopular_speakers: Primary beneficiary (moderate/mobile) — protected regardless of listener impact
 *   - dissident_political_movements: Primary beneficiary (organized/mobile) — widest room to organize and agitate
 *   - civil_liberties_litigators: Agenda-setter (institutional/arbitrage) — authors and defends the categorical boundary lines
 *   - targeted_minority_groups: Primary payer (powerless/trapped) — harm claims categorically non-cognizable
 *   - harassment_targets: Secondary payer (powerless/constrained) — real harm falls outside narrow exceptions
 *   - constitutional_courts: Analytical observer (institutional/analytical) — administers but does not collect
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__absolutist_reading, 0.28).
domain_priors:suppression_score(speech_protection_kernel__absolutist_reading, 0.15).
domain_priors:theater_ratio(speech_protection_kernel__absolutist_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__absolutist_reading, rope).
narrative_ontology:human_readable(speech_protection_kernel__absolutist_reading, "Absolutist Reading of Speech Protection (Categorical, Speaker-Autonomy-Maximizing)").
narrative_ontology:topic_domain(speech_protection_kernel__absolutist_reading, "Constitutional Law / Political Philosophy / Communication Rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__absolutist_reading, '8921bdfa-38da-49c3-bb0b-9f9ab61e2370').
narrative_ontology:cs_kernel_codification('8921bdfa-38da-49c3-bb0b-9f9ab61e2370', fixed_text).
narrative_ontology:cs_authority_grounding('8921bdfa-38da-49c3-bb0b-9f9ab61e2370', lineage).
narrative_ontology:cs_interpretation_layer_present('8921bdfa-38da-49c3-bb0b-9f9ab61e2370').
narrative_ontology:cs_reading_relation('8921bdfa-38da-49c3-bb0b-9f9ab61e2370', speech_protection_kernel__harm_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('8921bdfa-38da-49c3-bb0b-9f9ab61e2370', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('8921bdfa-38da-49c3-bb0b-9f9ab61e2370', speech_protection_kernel__dignity_reading, forecloses).
narrative_ontology:cs_reading_relation('8921bdfa-38da-49c3-bb0b-9f9ab61e2370', speech_protection_kernel__democratic_participation_reading, influences).
narrative_ontology:cs_axiom('8921bdfa-38da-49c3-bb0b-9f9ab61e2370', foundational, speaker_autonomy_categorically_prior_to_listener_harm).
narrative_ontology:cs_axiom_status(speaker_autonomy_categorically_prior_to_listener_harm, holdable).
narrative_ontology:cs_axiom_grounding('8921bdfa-38da-49c3-bb0b-9f9ab61e2370', speaker_autonomy_categorically_prior_to_listener_harm, deontological).
narrative_ontology:cs_axiom('8921bdfa-38da-49c3-bb0b-9f9ab61e2370', foundational, state_discretion_over_harm_is_the_greater_danger).
narrative_ontology:cs_axiom_status(state_discretion_over_harm_is_the_greater_danger, holdable).
narrative_ontology:cs_axiom_grounding('8921bdfa-38da-49c3-bb0b-9f9ab61e2370', state_discretion_over_harm_is_the_greater_danger, instrumental).
narrative_ontology:cs_reference_frame('8921bdfa-38da-49c3-bb0b-9f9ab61e2370', founding_era_anti_sedition_protection).
narrative_ontology:cs_drift_state('8921bdfa-38da-49c3-bb0b-9f9ab61e2370', contemporary_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8921bdfa-38da-49c3-bb0b-9f9ab61e2370', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__absolutist_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, unpopular_speakers).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, dissident_political_movements).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, media_organizations).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, civil_liberties_litigators).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, targeted_minority_groups).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, harassment_targets).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, defamed_private_individuals).
narrative_ontology:constraint_vindicates(speech_protection_kernel__absolutist_reading, speaker_autonomy_primacy_doctrine).
narrative_ontology:constraint_vindicates(speech_protection_kernel__absolutist_reading, content_neutrality_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals and groups holding minority, radical, or offensive views who rely on the categorical protection standard to speak without prior restraint or after-the-fact liability triggered by listener distress. The absolutist reading is the maximal shield for their expression; any move toward a harm threshold narrows their protection.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, unpopular_speakers, beneficiary,
    moderate, biographical, mobile, national).

% Organized political actors challenging incumbent power who depend on the widest possible protection boundary to organize, agitate, and criticize without officials characterizing their speech as harmful to public order or to targeted officials.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, dissident_political_movements, beneficiary,
    organized, generational, mobile, national).

% Publishers and broadcasters whose commercial and institutional interest is served by a bright-line rule that limits liability exposure to narrow categorical exceptions (incitement, true threats, fraud) rather than an open-ended harm inquiry that would require case-by-case litigation risk assessment.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, media_organizations, beneficiary,
    institutional, generational, arbitrage, national).

% Legal advocacy organizations and doctrinal scholars who author, litigate, and defend the categorical-exclusions framework in courts. They set the boundary lines (what counts as incitement, true threat, obscenity) and benefit professionally and institutionally from the doctrine's stability and prestige.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, civil_liberties_litigators, agenda_setter,
    institutional, generational, arbitrage, national).

% Groups subject to organized hate speech, slurs, and dehumanizing rhetoric that does not meet the narrow incitement or true-threats thresholds. Under the absolutist reading their claims that the speech functions as harm or subordination are categorically non-cognizable; they cannot exit the communicative environment they live and work within.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, targeted_minority_groups, payer,
    powerless, biographical, trapped, national).

% Individuals subjected to sustained, targeted speech campaigns (doxxing-adjacent rhetoric, coordinated public shaming, repeated public insult) that inflicts real psychological and reputational cost but falls outside the categorical exclusions. The doctrine offers them no recourse because the harm, however real, is listener harm rather than a recognized exception.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, harassment_targets, payer,
    powerless, immediate, constrained, local).

% Private individuals harmed by false statements who must clear demanding actual-malice or negligence thresholds calibrated to protect speaker autonomy; the categorical framework's presumption against liability means many genuinely harmful falsehoods go unremedied.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, defamed_private_individuals, payer,
    moderate, biographical, constrained, national).

% Adjudicate where the categorical lines fall, drawing on precedent and doctrine developed by advocacy groups and scholars. They administer the boundary but do not personally collect from its operation; their institutional legitimacy rests on maintaining a predictable, minimally content-based rule.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, constitutional_courts, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__absolutist_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_protection_kernel__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a predictable, content-neutral bright line that lets speakers, publishers, and platforms know in advance what speech is protected without litigating listener-impact case by case, and prevents the state from using vague harm standards to selectively suppress disfavored viewpoints.
% TRANSFER_FUNCTION: Moves the burden of unremedied harm from speakers (who retain protection regardless of listener impact) onto targets of that speech (who absorb reputational, psychological, and dignitary costs the doctrine treats as non-cognizable unless a narrow categorical exception applies).
% ABSENT_VOICES: Targeted minority groups and harassment targets rarely appear as parties in the foundational free-speech cases that established the categorical framework; the doctrine was substantially built through cases brought by or on behalf of speakers, publishers, and dissidents, not by those bearing the listener-harm costs the rule declares irrelevant.
% DISAPPEARANCE_RATIONALE: If the categorical, speaker-autonomy-maximizing standard were replaced overnight by a harm-threshold standard, dissident and minority political speech would face immediate new liability exposure, media organizations would need to reassess publication risk case-by-case, and a large volume of currently-protected offensive, provocative, or harmful expression would become contestable in court.
% FOUNDING_PROBLEM: Historical suppression of dissent, prior restraint of unpopular political and religious speech, and government officials using vague standards (breach of peace, seditious libel, obscenity) to selectively silence disfavored speakers.
% FOUNDING_PROBLEM_CORROBORATION: Civil liberties litigators and constitutional courts attest the founding problem (government suppression of dissent) remains live and that the categorical rule is the only reliable check against selective enforcement. Independent commentary from critical race and dignitary-harm scholarship — a body of work outside the beneficiary set — attests that the founding problem has been substantially solved for political dissent specifically, while the categorical rule now also shields a distinct and growing category of targeted harassment and group-defamation speech that the founding cases never contemplated.
narrative_ontology:disappearance_verdict(speech_protection_kernel__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__absolutist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_kernel__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__absolutist_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__absolutist_reading_tests).
:- end_tests(speech_protection_kernel__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.28 at 2024) because the primary function of the absolutist reading is genuine coordination — protecting dissidents and unpopular speakers against selective suppression — and only a secondary, diffuse cost falls on targeted groups whose harm claims the doctrine forecloses by category rather than by targeted enforcement against them. Suppression is authored low (0.15) because the doctrine does not actively coerce anyone to speak or refrain from speaking; its suppressive character, such as it is, operates by declaring certain harm claims non-cognizable rather than by punishing speech. Accessibility collapse is moderate (0.35): once a speaker or publisher understands the categorical framework, alternative frames (harm-based liability) are foreclosed as a matter of doctrine, but political and legislative alternatives remain live and contested — this is not a natural-law-grade collapse. Resistance is moderate-high (0.55) reflecting the genuinely contested, actively litigated status of the doctrine's boundary lines.
 *
 * PERSPECTIVAL GAP:
 *   From the civil liberties litigator and dissident-speaker seats, the absolutist reading looks like Rope: a minimal, well-calibrated coordination mechanism preventing government overreach. From the targeted minority group and harassment target seats, the identical doctrine can compute as Tangled Rope or worse: a structure that requires active judicial enforcement (upholding the categorical exclusions against harm claims) that coordinates protection for some while imposing uncompensated costs on others through the very same boundary-drawing mechanism. The engine's per-seat computation is expected to diverge here; that divergence is the intended data point about this reading, not an error.
 *
 * DIRECTIONALITY LOGIC:
 *   Unpopular speakers, dissident movements, and media organizations sit near the beneficiary end: the doctrine subsidizes their expression by removing listener-harm liability exposure. Civil liberties litigators are agenda-setters who administer and benefit professionally from the doctrine's stability. Targeted minority groups and harassment targets sit near the full-target end: trapped or constrained exit, no cognizable claim within the framework, and the cost is borne individually while the benefit (predictability against state suppression) is diffuse and society-wide. Defamed private individuals sit closer to symmetric — the doctrine still offers a remedy path, just a narrow and speaker-favoring one.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — government suppression of dissent via vague harm-adjacent standards (seditious libel, breach of peace) — remains partially live wherever political speech is targeted by state actors, which argues against mandatrophy for that core function. But the doctrine has also come to shield a category of speech (targeted harassment, group-based dehumanizing rhetoric) that the founding cases did not contemplate and that does not implicate government suppression of dissent at all. The founding_problem_status is authored 'contested' rather than 'dead' because the anti-suppression function remains genuinely operative even as the doctrine's boundary has drifted to also cover cases outside its original justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_exclusion_completeness,
    'Are the narrow categorical exclusions (incitement, true threats, fraud, narrowly-defined obscenity) a complete and principled enumeration of speech that causes cognizable harm, or an arbitrary historical accretion that happens to exclude harms disproportionately borne by less powerful groups?',
    'Comparative doctrinal history: trace how each exclusion was added, by whom, and whether harms to powerless groups were systematically excluded from consideration at the moments the categories were fixed.',
    'If the exclusions are a principled minimal set, the absolutist reading functions closer to genuine coordination with low residual extraction. If the exclusions are an accretion that happened to exclude harms to powerless groups, the low measured extractiveness understates the doctrine''s actual extractive function and the constraint sits closer to Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_exclusion_completeness, conceptual, 'Whether the categorical exclusion set is principled or a historically contingent accretion favoring powerful speakers.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the absolutist reading the historically dominant reading of the speech-protection kernel, or one contested reading among several live readings currently competing for doctrinal supremacy?',
    'Track citation frequency and controlling-precedent status of the absolutist framework versus harm_threshold, dignity, and democratic_participation framings across appellate courts over the interval; a reading that is losing ground in controlling precedent is a different structural fact than one that remains dominant.',
    'If the absolutist reading is dominant and stable, this story''s low extraction/suppression profile is the operative real-world constraint. If the reading is contested and losing ground to harm_threshold or dignity readings in controlling jurisprudence, the sibling constraint (harm_threshold_reading) may be the more accurate description of the operative legal environment for targeted groups, even though this story remains valid as the doctrine the absolutist reading itself claims to be.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, empirical, 'Whether the absolutist reading is the historically dominant or merely one contested reading of the kernel.').

omega_variable(
    listener_harm_cognizability_boundary,
    'Is the line between ''categorical exclusion'' (incitement, true threats) and ''ordinary listener harm'' (which the doctrine declares non-cognizable) itself a natural joint in the concept of harm, or a constructed boundary that could be drawn differently?',
    'Cross-jurisdictional comparison: jurisdictions applying dignity-based or harm-threshold readings to structurally similar speech acts and comparing outcomes and social effects.',
    'If the boundary is a natural joint, the absolutist reading''s exclusion of listener harm is principled rather than merely speaker-favoring. If constructed, the exclusion is a policy choice that systematically favors speaker autonomy over listener protection, which strengthens the case that this reading''s low extractiveness score understates real-world cost to the payer stakeholders.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(listener_harm_cognizability_boundary, conceptual, 'Whether the categorical/ordinary-harm boundary is a natural joint or a constructed, speaker-favoring policy line.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__absolutist_reading, 1919, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1919, speech_protection_kernel__absolutist_reading, theater_ratio, 1919, 0.08).
narrative_ontology:measurement_basis(spee_tr_t1919, observed).
narrative_ontology:measurement(spee_tr_t1960, speech_protection_kernel__absolutist_reading, theater_ratio, 1960, 0.09).
narrative_ontology:measurement_basis(spee_tr_t1960, observed).
narrative_ontology:measurement(spee_tr_t1980, speech_protection_kernel__absolutist_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement_basis(spee_tr_t1980, observed).
narrative_ontology:measurement(spee_tr_t2000, speech_protection_kernel__absolutist_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement_basis(spee_tr_t2000, observed).
narrative_ontology:measurement(spee_tr_t2012, speech_protection_kernel__absolutist_reading, theater_ratio, 2012, 0.11).
narrative_ontology:measurement_basis(spee_tr_t2012, observed).
narrative_ontology:measurement(spee_tr_t2024, speech_protection_kernel__absolutist_reading, theater_ratio, 2024, 0.12).
narrative_ontology:measurement_basis(spee_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(spee_be_t1919, speech_protection_kernel__absolutist_reading, base_extractiveness, 1919, 0.18).
narrative_ontology:measurement_basis(spee_be_t1919, observed).
narrative_ontology:measurement(spee_be_t1960, speech_protection_kernel__absolutist_reading, base_extractiveness, 1960, 0.2).
narrative_ontology:measurement_basis(spee_be_t1960, observed).
narrative_ontology:measurement(spee_be_t1980, speech_protection_kernel__absolutist_reading, base_extractiveness, 1980, 0.22).
narrative_ontology:measurement_basis(spee_be_t1980, observed).
narrative_ontology:measurement(spee_be_t2000, speech_protection_kernel__absolutist_reading, base_extractiveness, 2000, 0.25).
narrative_ontology:measurement_basis(spee_be_t2000, observed).
narrative_ontology:measurement(spee_be_t2012, speech_protection_kernel__absolutist_reading, base_extractiveness, 2012, 0.26).
narrative_ontology:measurement_basis(spee_be_t2012, observed).
narrative_ontology:measurement(spee_be_t2024, speech_protection_kernel__absolutist_reading, base_extractiveness, 2024, 0.28).
narrative_ontology:measurement_basis(spee_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1919, speech_protection_kernel__absolutist_reading, suppression_requirement, 1919, 0.1).
narrative_ontology:measurement_basis(spee_su_t1919, observed).
narrative_ontology:measurement(spee_su_t1960, speech_protection_kernel__absolutist_reading, suppression_requirement, 1960, 0.11).
narrative_ontology:measurement_basis(spee_su_t1960, observed).
narrative_ontology:measurement(spee_su_t1980, speech_protection_kernel__absolutist_reading, suppression_requirement, 1980, 0.13).
narrative_ontology:measurement_basis(spee_su_t1980, observed).
narrative_ontology:measurement(spee_su_t2000, speech_protection_kernel__absolutist_reading, suppression_requirement, 2000, 0.14).
narrative_ontology:measurement_basis(spee_su_t2000, observed).
narrative_ontology:measurement(spee_su_t2012, speech_protection_kernel__absolutist_reading, suppression_requirement, 2012, 0.14).
narrative_ontology:measurement_basis(spee_su_t2012, observed).
narrative_ontology:measurement(spee_su_t2024, speech_protection_kernel__absolutist_reading, suppression_requirement, 2024, 0.15).
narrative_ontology:measurement_basis(spee_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__absolutist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_kernel__absolutist_reading, 0.1).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, democratic_participation_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling constraint stories decomposing the natural-language 'speech protection kernel' per the ε-invariance principle. Each sibling reading (absolutist, harm_threshold, marketplace, dignity, democratic_participation) has its own ε, beneficiary/victim structure, and classification, because measuring 'speech protection' through each reading's operative test yields structurally different extraction profiles and different victim sets. This story (absolutist_reading) is authored with the widest protection boundary and lowest measured extraction among the five; harm_threshold_reading is expected to show materially higher measured extraction toward speakers/publishers and lower toward targeted groups, as the two stories model opposite allocations of the same underlying transfer. All five are linked bidirectionally via affects_constraints to preserve the constraint-family network.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
