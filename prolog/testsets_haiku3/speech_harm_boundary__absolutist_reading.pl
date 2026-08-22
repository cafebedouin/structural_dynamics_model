% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Absolutist Speech Protection Doctrine (Harm Override Threshold)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The absolutist reading of the speech-harm boundary holds that speech
 *   protection operates near-absolutely, with only narrow exceptions
 *   (incitement to imminent lawless action, true threats, defamation,
 *   obscenity). This reading prioritizes speaker autonomy and guards against
 *   government censorship by setting an extremely high threshold for
 *   harm-based speech restrictions. The doctrine operates as rope—genuine
 *   coordination around a stable legal boundary—while simultaneously
 *   extracting from targets of hate speech and denying speech who bear
 *   uncompensated harm. The claim/metric gap is intentional: the absolutist
 *   doctrine is CLAIMED as rope (speakers and press frame it as essential
 *   coordination against government overreach) while the authored metrics
 *   reflect rising extractiveness over time as awareness of asymmetric harm
 *   grows and alternative readings gain salience. The engine measures this
 *   divergence; the story does not reconcile them.
 *
 * KEY AGENTS:
 *   - speakers_heterodox_viewpoints: beneficiary (high d → low extraction burden)
 *   - institutional_press: beneficiary/agenda_setter (institutional power, arbitrage exit, governs doctrine interpretation)
 *   - targets_hate_speech: payer (powerless, identity-locked, subject to uncompensated harm)
 *   - marginalized_communities_denying_speech: payer (moderate power, constrained exit, systematic exclusion from remedy)
 *   - courts_applying_doctrine: agenda_setter (institutional power, interprets boundary narrowly)
 *   - legislators_reform_advocates: excluded (powerful but structurally barred from unilateral doctrine change)
 *   - comparative_legal_systems: observer (witness that alternative framings exist and function)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__absolutist_reading, 0.68).
domain_priors:suppression_score(speech_harm_boundary__absolutist_reading, 0.19).
domain_priors:theater_ratio(speech_harm_boundary__absolutist_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, suppression_requirement, 0.19).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__absolutist_reading, rope).
narrative_ontology:human_readable(speech_harm_boundary__absolutist_reading, "Absolutist Speech Protection Doctrine (Harm Override Threshold)").
narrative_ontology:topic_domain(speech_harm_boundary__absolutist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_harm_boundary__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__absolutist_reading, '9e6bc697-1a81-4021-a32f-de41a9f6214d').
narrative_ontology:cs_kernel_codification('9e6bc697-1a81-4021-a32f-de41a9f6214d', fixed_text).
narrative_ontology:cs_authority_grounding('9e6bc697-1a81-4021-a32f-de41a9f6214d', lineage).
narrative_ontology:cs_interpretation_layer_present('9e6bc697-1a81-4021-a32f-de41a9f6214d').
narrative_ontology:cs_reading_relation('9e6bc697-1a81-4021-a32f-de41a9f6214d', speech_harm_boundary__dignity_reading, coexists_with).
narrative_ontology:cs_reading_relation('9e6bc697-1a81-4021-a32f-de41a9f6214d', speech_harm_boundary__harm_balancing_reading, coexists_with).
narrative_ontology:cs_axiom('9e6bc697-1a81-4021-a32f-de41a9f6214d', foundational, speaker_autonomy_nearly_inviolable).
narrative_ontology:cs_axiom_status(speaker_autonomy_nearly_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('9e6bc697-1a81-4021-a32f-de41a9f6214d', speaker_autonomy_nearly_inviolable, deontological).
narrative_ontology:cs_axiom('9e6bc697-1a81-4021-a32f-de41a9f6214d', foundational, harm_insufficient_to_restrict_speech).
narrative_ontology:cs_axiom_status(harm_insufficient_to_restrict_speech, holdable).
narrative_ontology:cs_axiom_grounding('9e6bc697-1a81-4021-a32f-de41a9f6214d', harm_insufficient_to_restrict_speech, empirically_contingent).
narrative_ontology:cs_reference_frame('9e6bc697-1a81-4021-a32f-de41a9f6214d', speaker_autonomy_supremacy).
narrative_ontology:cs_drift_state('9e6bc697-1a81-4021-a32f-de41a9f6214d', contemporary_digital_hate_campaigns, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9e6bc697-1a81-4021-a32f-de41a9f6214d', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__absolutist_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, speakers_heterodox_viewpoints).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, institutional_press).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, targets_hate_speech).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, marginalized_communities_denying_speech).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain broad legal protection to articulate contested claims—political dissent, religious argument, social critique—without state suppression. The absolutist doctrine shields even offensive, inflammatory, or emotionally harmful speech from legal liability so long as it does not meet the narrow incitement-true-threat-defamation threshold. Beneficiaries include political minorities, religious minorities, controversial academics, and activist organizations whose speech would face legal jeopardy under narrower doctrines.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, speakers_heterodox_viewpoints, beneficiary,
    organized, generational, mobile, national).

% Operates under broad protection for investigative journalism, opinion editorial, and reporting on matters of public concern. The absolutist doctrine creates a high bar for plaintiffs suing for defamation or emotional harm from publication, enabling the press to cover controversial subjects, expose wrongdoing, and critique powerful actors without fear of crippling legal liability. Publishers can reach audiences globally; their exit from the doctrine would mean accepting liability they can distribute costs to avoid.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, institutional_press, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__absolutist_reading, institutional_press, agenda_setter).

% Endure legally unprotected speech targeting their identity groups—slurs, dehumanization, false assertions about group characteristics—without legal remedy unless the speech rises to incitement (direct call to imminent violence) or true threat (directed threat of violence to the individual). The harm is psychological, reputational, and social: exclusion from public discourse, internalized stigma, chilling effects on their own speech participation. Exit is identity-locked; they cannot cease being members of the target group.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, targets_hate_speech, payer,
    powerless, biographical, identity_locked, national).

% Operate in a legal environment where coordinated campaigns to deny their humanity, rationalize their exclusion, or portray them as threats to the majority remain protected speech. Systematic speech campaigns can erode social standing, suppress their participation in democratic processes, and create hostile environments in employment, education, and civic life. They have constrained exit: organizing counter-speech, seeking private-platform remedies, or leaving jurisdictions where such speech flourishes—but no legal cause of action against the denying speech itself unless it crosses into incitement.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, marginalized_communities_denying_speech, payer,
    moderate, generational, constrained, national).

% Interpret and apply the absolutist doctrine by narrowly construing the exceptions (incitement, true threat, defamation, obscenity) and rejecting harm-based or dignity-based restrictions. They decide cases, set precedent, and define the boundary of protected vs. unprotected speech. Their power lies in their authority to declare speech protected; their constraint is that departure from the established doctrine requires overruling precedent and faces political backlash from speakers and press who rely on the doctrine's stability.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, courts_applying_doctrine, agenda_setter,
    institutional, generational, analytical, national).

% Seek to narrow the absolutist doctrine via statute or constitutional amendment—adopting harm-balancing or dignity-based exceptions—but find their path blocked by court precedent, constitutional interpretation, and institutional resistance from speakers and press who have organized around the doctrine. They represent constituencies harmed by unprotected speech and argue for proportional restrictions; their exclusion is structural: courts are the gatekeepers of constitutional doctrine, and legislatures lack the unilateral power to override established precedent without constitutional amendment (a high barrier).
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, legislators_reform_advocates, excluded,
    powerful, generational, constrained, national).

% Other constitutional democracies (Canada, Germany, UK) apply harm-balancing or dignity-based frameworks that permit speech restrictions the absolutist doctrine forbids. They serve as comparative reference points: evidence that alternative doctrines are workable, that higher speech restrictions do not necessarily destroy press freedom or democratic contestation, and that the absolutist framing is one choice rather than a natural necessity.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, comparative_legal_systems, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_harm_boundary__absolutist_reading, institutional_press).
narrative_ontology:fixing_cost_class(speech_harm_boundary__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a predictable, stable legal boundary between protected and unprotected speech, enabling speakers to understand their rights with minimal chilling effect and reducing government discretion to suppress disfavored viewpoints. The narrow exceptions (incitement, true threat, defamation, obscenity) coordinate on a shared understanding that speech remains sovereign except in narrow cases of direct imminent harm or fraud.
% TRANSFER_FUNCTION: Shifts the cost of speech tolerance from speakers (who pay legal liability for harmful utterance) to targets (who bear psychological, reputational, and social harm without legal remedy). Speakers and institutional press receive protection; targets of hate speech and denying speech receive no legal claim for harm. The transfer is uncompensated and identity-locked for marginalized targets.
% ABSENT_VOICES: Marginalized communities, targets of hate speech, and those advocating harm-balancing exceptions are structurally excluded from the doctrine's framing. They would argue that dignity-denying speech causes measurable harm and that proportional restrictions would not destroy press freedom or dissent. They are excluded because courts, not affected communities, interpret the doctrine, and because the doctrine's own logic treats harm as insufficiently weighty to override speech protection.
% DISAPPEARANCE_RATIONALE: If the absolutist doctrine disappeared overnight and courts shifted to harm-balancing or dignity-based frameworks, the legal landscape would reorganize: plaintiffs would gain causes of action for dignity-denying speech; publishers would face heightened litigation risk; speech could be restricted on grounds currently foreclosed. The redistribution would be sharp: speaker costs would rise, target remedies would expand, and institutional press would need to recalibrate editorial risk assessments.
% FOUNDING_PROBLEM: Government had used obscenity and sedition laws to suppress political dissent, religious minority speech, and journalism. The absolutist doctrine was constructed to create a constitutional floor preventing government from weaponizing speech law against disfavored viewpoints.
% FOUNDING_PROBLEM_CORROBORATION: First Amendment scholars and libertarian legal theorists attest the problem remains live and justify the doctrine as essential protection against government overreach. Civil rights advocates and dignity-focused scholars attest the problem is substantially solved (modern governments are less predatory) and the doctrine now operates to protect hate speech and denying speech rather than to prevent government suppression—the founding problem's context has shifted. Comparative legal scholars note that other democracies solved the government-overreach problem without adopting absolutism, suggesting the problem is real but the absolutist solution is one choice rather than a necessity.
narrative_ontology:disappearance_verdict(speech_harm_boundary__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__absolutist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_harm_boundary__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__absolutist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.68 at interval end) because the doctrine systematically shifts harm costs to powerless, identity-locked targets without legal remedy. The measurement series shows rising extractiveness from 0.48 to 0.68 over the interval, reflecting historical accumulation: as hate speech campaigns grew more sophisticated and coordinated (Charlottesville, social media coordination), as digital networks amplified denying speech reach, as empirical research documented harm to targets, the extraction became more visible even as the doctrine remained stable. Theater is very low (0.12): the doctrine performs minimal theatrical maintenance—it is genuinely believed by its beneficiaries and skeptics alike. Suppression is low (0.19) because the doctrine operates through legal principle and court precedent rather than active coercion; targets' exit is blocked by identity rather than by enforcement machinery (a structural fact about the constraint, not suppression as coercive force). Accessibility collapse is very low (0.22): alternatives remain highly accessible (harm-balancing frameworks, dignity-based doctrines, comparative legal models), which is why the doctrine is contested rather than naturalised.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (speakers, institutional press) and the payer seats (targets, marginalized communities) should compute radically differently. From the beneficiary perspective, the doctrine is genuine coordination: a principled boundary that prevents government overreach and enables dissent. From the payer perspective, the doctrine is extraction disguised as principle: the harm they absorb is real and uncompensated, while beneficiaries reap protection. Courts apply the doctrine as coordinators (trustees of the principle); targets experience it as enforcement of an asymmetric rule. The engine computes seat-specific types; the absolutist reading instantiates a constraint whose structure generates opposed experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Speakers (particularly heterodox, minority, activist speakers) are beneficiaries: the doctrine shields them from liability and enables their participation. Institutional press is a beneficiary with institutional power and arbitrage exit—they can distribute globally and buffer risk. Targets of hate speech are victims: they bear psychological and social harm without legal remedy and cannot exit (identity-locked). Marginalized communities are victims: they face systematic denying speech that erodes their social standing and democratic participation, with no legal cause of action. Courts are agenda-setters: they interpret the doctrine narrowly and defend its boundaries. Legislators are excluded: they cannot unilaterally override the doctrine. The directionality derivation: beneficiaries get low d (protection reduces their cost); victims get high d (they bear uncompensated harm). Identity-locked targets have no exit alternative, which amplifies d toward 1.0.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy classification because its founding problem (government weaponization of speech law) remains live in comparative perspective—other democracies still struggle with the balance—even though within the U.S. context the problem has substantially shifted (modern government is less predatory, but hate speech campaigns are more coordinated and damaging). The founding problem status is 'contested,' not 'dead,' which preserves the coordination claim. However, the rising extractiveness and the growing salience of harm-based alternatives suggest the constraint is approaching a mandatrophy condition: the original coordination function (preventing government overreach) is secure, but the extraction (uncompensated harm to targets) has become visible as a primary function rather than an incidental cost. An omega tracks this tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_obsolescence,
    'Is the founding problem (government weaponization of speech law to suppress dissent and journalism) still live, or has institutional and constitutional development substantially solved it?',
    'Comparative analysis of speech prosecution rates and outcomes in the U.S. vs. other democracies over time; examination of whether government selectively prosecutes disfavored speech at scale.',
    'If the problem is substantially solved, the doctrine''s continued operation shifts from coordination (solving the founding problem) to extraction (protecting speakers while targets absorb harm). If the problem remains live, the doctrine retains its coordination function and mandatrophy does not apply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the historical context justifying absolutism still obtains or has shifted.').

omega_variable(
    harm_measurability_and_identity_lock,
    'Does uncompensated harm to targets of hate speech constitute a genuine injury that should weight in the speech balance, or is psychological and social harm insufficiently concrete to override speaker autonomy?',
    'Longitudinal studies of health outcomes, democratic participation, and speech engagement by targets of hate campaigns; comparison of jurisdictions adopting harm-balancing doctrines to assess whether documented harms decline without destroying speaker freedom.',
    'If harm is measurable and avoidable without destroying press freedom (comparative evidence), the extraction becomes unjustifiable and the constraint shifts toward snare classification. If harm is diffuse or unavoidable without collapse of speaker protection, the extraction remains an acceptable cost of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_measurability_and_identity_lock, empirical, 'Whether the harm to targets is a cost of coordination or an unjustified extraction.').

omega_variable(
    reading_foreclosure_test,
    'Does the absolutist reading''s core axiom (speaker autonomy is nearly inviolable) logically foreclose the dignity reading''s core axiom (personhood-denying speech is categorically unprotected)?',
    'Jurisprudential analysis: can a single constitutional framework coherently hold both ''speaker autonomy nearly inviolable'' and ''personhood-denying speech per se unprotected''? Or do these axioms contradict such that no unified framework can hold both?',
    'If axioms foreclose each other, the readings enter a forecloses relationship (rare, rare case). If they coexist (as currently held by different judicial and scholarly communities), they remain coexisting readings. This affects the network relationship declared in cs_structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_test, conceptual, 'Whether the absolutist and dignity readings are logically incompatible or can coexist in different institutional frameworks.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression of payer participation (targets withdrawing from public discourse) structural (enforced by legal inability to sue, by coordinated harassment campaigns, by algorithmic amplification of denying speech) or internalized (targets have internalized beliefs that they do not deserve to speak, fused their identity with vulnerability)?',
    'Longitudinal tracking of target participation rates before and after exposure to denying speech; comparison across jurisdictions with different legal doctrines to isolate structural vs. internalized suppression.',
    'If suppression is internalized, the constraint''s effective suppression is higher than the structural measure (0.19) suggests—targets carry the suppression with them after exposure. If structural, the suppression reflects the legal doctrine''s enforcement. The distinction affects whether the constraint is classification-stable or drifts under policy change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of target participation is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__absolutist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_harm_boundary__absolutist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(spee_tr_t0, observed).
narrative_ontology:measurement(spee_tr_t8, speech_harm_boundary__absolutist_reading, theater_ratio, 8, 0.09).
narrative_ontology:measurement_basis(spee_tr_t8, observed).
narrative_ontology:measurement(spee_tr_t16, speech_harm_boundary__absolutist_reading, theater_ratio, 16, 0.1).
narrative_ontology:measurement_basis(spee_tr_t16, observed).
narrative_ontology:measurement(spee_tr_t24, speech_harm_boundary__absolutist_reading, theater_ratio, 24, 0.11).
narrative_ontology:measurement_basis(spee_tr_t24, observed).
narrative_ontology:measurement(spee_tr_t32, speech_harm_boundary__absolutist_reading, theater_ratio, 32, 0.12).
narrative_ontology:measurement_basis(spee_tr_t32, observed).
narrative_ontology:measurement(spee_tr_t40, speech_harm_boundary__absolutist_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement_basis(spee_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_harm_boundary__absolutist_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(spee_be_t0, observed).
narrative_ontology:measurement(spee_be_t8, speech_harm_boundary__absolutist_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement_basis(spee_be_t8, observed).
narrative_ontology:measurement(spee_be_t16, speech_harm_boundary__absolutist_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement_basis(spee_be_t16, observed).
narrative_ontology:measurement(spee_be_t24, speech_harm_boundary__absolutist_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement_basis(spee_be_t24, observed).
narrative_ontology:measurement(spee_be_t32, speech_harm_boundary__absolutist_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement_basis(spee_be_t32, observed).
narrative_ontology:measurement(spee_be_t40, speech_harm_boundary__absolutist_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(spee_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_harm_boundary__absolutist_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(spee_su_t0, observed).
narrative_ontology:measurement(spee_su_t8, speech_harm_boundary__absolutist_reading, suppression_requirement, 8, 0.14).
narrative_ontology:measurement_basis(spee_su_t8, observed).
narrative_ontology:measurement(spee_su_t16, speech_harm_boundary__absolutist_reading, suppression_requirement, 16, 0.16).
narrative_ontology:measurement_basis(spee_su_t16, observed).
narrative_ontology:measurement(spee_su_t24, speech_harm_boundary__absolutist_reading, suppression_requirement, 24, 0.18).
narrative_ontology:measurement_basis(spee_su_t24, observed).
narrative_ontology:measurement(spee_su_t32, speech_harm_boundary__absolutist_reading, suppression_requirement, 32, 0.19).
narrative_ontology:measurement_basis(spee_su_t32, observed).
narrative_ontology:measurement(spee_su_t40, speech_harm_boundary__absolutist_reading, suppression_requirement, 40, 0.19).
narrative_ontology:measurement_basis(spee_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__absolutist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_harm_boundary__absolutist_reading, 0.12).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, speech_harm_boundary__dignity_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, speech_harm_boundary__harm_balancing_reading).

% DUAL FORMULATION NOTE:
% The speech-harm boundary is a contested kernel admitting three readings: absolutist (this story), dignity-based, and harm-balancing. Each reading instantiates a different constraint with its own ε, beneficiary/victim structure, and type. The three are linked via the network.affects_constraints array: absolutist influences (creates structural downstream pressure on) both dignity and harm-balancing readings by establishing a high precedential bar for alternative doctrines. This story models absolutism as rope; the dignity reading models categorical exclusion as a tangled_rope or snare (depending on enforcement); the harm-balancing reading models proportional balancing as a rope or tangled_rope. The three readings are coexisting, not foreclosed—different constitutional traditions and courts hold all three simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_harm_boundary__absolutist_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
