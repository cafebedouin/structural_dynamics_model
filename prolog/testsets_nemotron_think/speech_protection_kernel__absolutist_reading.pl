% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   human_readable: Near-Categorical Speech Protection (Absolutist Reading)
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   This constraint story instantiates the absolutist reading of the
 *   speech_protection_kernel: the First Amendment operates as a
 *   near-categorical bar on content-based speech restrictions, rejecting
 *   listener harm as a ground for regulation except within narrow,
 *   historically recognized categorical exclusions (incitement, true threats,
 *   obscenity, defamation, child pornography, fraud, speech integral to
 *   criminal conduct). The reading claims the status of a constitutional
 *   mountain — a fixed principle of constitutional law that admits no
 *   balancing. The metrics describe a constraint that began as genuine
 *   coordination against state censorship (low extraction, high suppression
 *   requirement against government) but has drifted toward extracting from
 *   listeners (rising extraction, falling suppression requirement) as the
 *   primary threat shifted from state suppression to private harm. The
 *   claim/metric divergence is deliberate: the engine measures whether a
 *   claimed mountain with beneficiaries and victims computes as a false
 *   summit.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__absolutist_reading, 0.35).
domain_priors:suppression_score(speech_protection_kernel__absolutist_reading, 0.15).
domain_priors:theater_ratio(speech_protection_kernel__absolutist_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__absolutist_reading, mountain).
narrative_ontology:human_readable(speech_protection_kernel__absolutist_reading, "Near-Categorical Speech Protection (Absolutist Reading)").
narrative_ontology:topic_domain(speech_protection_kernel__absolutist_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:emerges_naturally(speech_protection_kernel__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__absolutist_reading, '20b15c00-de85-4940-a1ee-242564edea39').
narrative_ontology:cs_kernel_codification('20b15c00-de85-4940-a1ee-242564edea39', formalized).
narrative_ontology:cs_authority_grounding('20b15c00-de85-4940-a1ee-242564edea39', lineage).
narrative_ontology:cs_interpretation_layer_present('20b15c00-de85-4940-a1ee-242564edea39').
narrative_ontology:cs_reading_relation('20b15c00-de85-4940-a1ee-242564edea39', speech_protection_kernel__harm_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('20b15c00-de85-4940-a1ee-242564edea39', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('20b15c00-de85-4940-a1ee-242564edea39', speech_protection_kernel__dignity_reading, forecloses).
narrative_ontology:cs_reading_relation('20b15c00-de85-4940-a1ee-242564edea39', speech_protection_kernel__democratic_participation_reading, influences).
narrative_ontology:cs_axiom('20b15c00-de85-4940-a1ee-242564edea39', foundational, listener_harm_never_grounds_restriction).
narrative_ontology:cs_axiom_status(listener_harm_never_grounds_restriction, holdable).
narrative_ontology:cs_axiom_grounding('20b15c00-de85-4940-a1ee-242564edea39', listener_harm_never_grounds_restriction, deontological).
narrative_ontology:cs_axiom('20b15c00-de85-4940-a1ee-242564edea39', foundational, categorical_exclusions_exhaustive_and_fixed).
narrative_ontology:cs_axiom_status(categorical_exclusions_exhaustive_and_fixed, holdable).
narrative_ontology:cs_axiom_grounding('20b15c00-de85-4940-a1ee-242564edea39', categorical_exclusions_exhaustive_and_fixed, conventional).
narrative_ontology:cs_axiom('20b15c00-de85-4940-a1ee-242564edea39', secondary, government_competence_presumption_against_balancing).
narrative_ontology:cs_axiom_status(government_competence_presumption_against_balancing, holdable).
narrative_ontology:cs_axiom_grounding('20b15c00-de85-4940-a1ee-242564edea39', government_competence_presumption_against_balancing, instrumental).
narrative_ontology:cs_reference_frame('20b15c00-de85-4940-a1ee-242564edea39', first_amendment_absolutism).
narrative_ontology:cs_drift_state('20b15c00-de85-4940-a1ee-242564edea39', contemporary_algorithmic_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('20b15c00-de85-4940-a1ee-242564edea39', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__absolutist_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, speakers).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, press_institutions).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, advocacy_organizations).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, targeted_listeners).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, marginalized_groups).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, harassment_victims).
narrative_ontology:constraint_vindicates(speech_protection_kernel__absolutist_reading, speaker_autonomy_primacy).
narrative_ontology:constraint_vindicates(speech_protection_kernel__absolutist_reading, viewpoint_neutrality_principle).
narrative_ontology:constraint_vindicates(speech_protection_kernel__absolutist_reading, government_competence_skepticism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enjoy near-absolute protection for expression regardless of listener impact. Can publish, speak, and disseminate without fear of content-based restriction unless speech falls into narrow categorical exclusions (incitement, true threats, obscenity, defamation). Exit is effectively unlimited — they can speak anywhere, anytime, on any platform.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, speakers, beneficiary,
    organized, biographical, arbitrage, national).

% Operate as organized speakers with institutional resources to test boundaries and litigate edge cases. Benefit from the doctrinal floor that prevents government from restricting publication based on anticipated harm. Shape the contour of categorical exclusions through strategic litigation.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, press_institutions, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__absolutist_reading, press_institutions, agenda_setter).

% Use absolutist protection to advance controversial causes without suppression. The constraint enables civil rights, anti-war, and other movements to speak when majorities would silence them. Their exit is constrained only by resource limits, not legal barriers.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, advocacy_organizations, beneficiary,
    organized, biographical, mobile, national).

% Bear the full brunt of harmful speech (hate speech, harassment, targeted abuse) with no legal remedy under this reading. Cannot exit the harm — speech reaches them in homes, workplaces, public spaces. The constraint structurally denies them standing to restrict the source of harm.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, targeted_listeners, payer,
    moderate, biographical, constrained, local).

% Disproportionately targeted by hate speech and structural vilification. The absolutist reading treats their subordination as the price of liberty. Identity-locked because group membership is immutable and the harm compounds across generations — exit from the constraint would require exit from the identity that makes them targets.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, marginalized_groups, payer,
    powerless, generational, identity_locked, national).

% Face sustained, personalized speech campaigns (stalking, doxxing, threats below 'true threat' threshold) that fall outside categorical exclusions. Trapped because the constraint denies injunctive relief and the speech follows them across platforms and physical spaces.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, harassment_victims, payer,
    powerless, immediate, trapped, local).

% Administer the constraint by policing the boundary of categorical exclusions. Their role is to say 'this is not protected' for a vanishingly small set of speech categories. They do not collect rents but hold the authoritative interpretive power that maintains the absolutist floor.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, courts_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Barred from enacting harm-based speech restrictions. Can only regulate within the narrow categorical exclusions the courts recognize. Their exclusion is structural — the constraint exists precisely to prevent legislative balancing of speaker and listener interests.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, legislature, excluded,
    institutional, biographical, constrained, national).

% Analyze the constraint's operation, track doctrinal drift, and debate whether the absolutist reading still serves its founding coordination function or has become a false summit protecting power rather than dissent.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of government censorship by establishing a bright-line rule: the state may not restrict speech based on its content or anticipated harm to listeners. This coordinates speaker behavior (speak freely) and government behavior (do not suppress) without case-by-case adjudication of harm.
% TRANSFER_FUNCTION: Transfers the cost of harmful speech from speakers (who would bear liability or restraint under a harm-based regime) to listeners (who absorb the harm without remedy). The transfer is diffuse — each harmful utterance imposes a small cost on many listeners — but structurally one-directional.
% ABSENT_VOICES: Targeted listeners, marginalized groups, and harassment victims are structurally excluded from the constitutional conversation. They would argue for harm-based exceptions, dignity-based limits, or democratic participation thresholds, but the constraint's architecture denies them standing. They are absent not by choice but by doctrinal design.
% DISAPPEARANCE_RATIONALE: If the absolutist constraint vanished overnight, legislatures would enact harm-based speech regulations within months. Hate speech laws, harassment injunctions, dignity protections, and platform liability regimes would proliferate. The speech environment would reorganize around listener protection rather than speaker immunity — a fundamental rearrangement of the communication order.
% FOUNDING_PROBLEM: The founding problem was government suppression of dissent: sedition acts, wartime censorship, loyalty oaths, and the use of 'harm to the state' or 'harm to public morals' as pretexts for silencing political opponents. The absolutist rule was built to make censorship structurally impossible by removing the balancing test altogether.
% FOUNDING_PROBLEM_CORROBORATION: Civil liberties organizations (ACLU, FIRE) and originalist scholars attest the founding problem remains live — government censorship pressures persist in new forms (platform pressure, national security letters, protest restrictions). Critical race theorists, feminist legal scholars, and dignity-rights advocates attest the founding problem is substantially solved for state suppression but the constraint now primarily protects private power (hate groups, harassers, disinformation operators) rather than dissenters. Legislative history from the 1960s civil rights era shows the absolutist reading was contested even at its doctrinal formation.
narrative_ontology:disappearance_verdict(speech_protection_kernel__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__absolutist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_kernel__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__absolutist_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__absolutist_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, ExtMetricName, E),
    domain_priors:suppression_score(speech_protection_kernel__absolutist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(speech_protection_kernel__absolutist_reading),
    narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(speech_protection_kernel__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.05 to 0.35 across the interval because the constraint's operation increasingly transfers harm-costs to listeners without remedy. Early era: the constraint suppressed government censorship (coordination function dominant). Late era: the constraint enables private actors to inflict harm with impunity (extraction function emergent). Suppression requirement falls because the constraint no longer needs active enforcement against the state — the state has internalized the rule; the suppression now operates against legislative attempts to protect listeners. Theater ratio remains low but rises slightly as 'absolutist' rhetoric masks the constraint's failure to address novel harm vectors (algorithmic amplification, targeted harassment campaigns, stochastic terrorism). Accessibility collapse is high (0.88) because harm-based alternatives are doctrinally foreclosed. Resistance is moderate (0.42) because the constraint faces sustained intellectual and political challenge from other readings.
 *
 * PERSPECTIVAL GAP:
 *   From the speaker/press seat, the constraint is a mountain — a natural law of free expression that enables all other liberties. From the marginalized_group seat, the constraint is a snare — a rule that licenses their subordination under the guise of liberty. From the court seat, the constraint is a rope — a coordination mechanism that prevents judicial balancing but requires constant boundary maintenance. The engine computes these per-seat types from the structural data; the authored claim (mountain) represents the absolutist reading's self-understanding, not the computed reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Speakers, press, and advocacy organizations are structural beneficiaries (d near 0.0) — the constraint subsidizes their speech by immunizing it from consequence. Targeted listeners, marginalized groups, and harassment victims are structural payers (d near 1.0) — they bear the full cost of harmful speech with no exit. Courts are agenda_setters (d ~ 0.5) — they administer the boundary but collect no rents. Legislature is excluded (d undefined) — barred from the coordination game entirely. The identity_locked exit for marginalized_groups and trapped exit for harassment_victims amplify their effective extraction toward the full-target end.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (government censorship of dissent) is contested — live for state suppression, substantially solved for the original threat model, but the constraint now primarily protects private power. The coordination function (preventing government censorship) persists but the extraction function (immunizing private harm) has grown. This is a classic mandatrophy pattern: the constraint's mandate has partially outlived its function, but the constraint persists because the beneficiary coalition (speakers, press, courts) has institutionalized the absolutist reading as constitutional identity. The mandatrophy is not resolved — the constraint remains in contested territory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_absolutist_reading,
    'How does the absolutist reading''s claim to the First Amendment text differ structurally from sibling readings'' claims to the same text?',
    'Map each reading''s structural relationship to the kernel: which categorical exclusions each reading recognizes, which harm thresholds each reading admits, which speaker/listener balance each reading encodes. The kernel is the shared text; the readings are distinct constraints with different ε, different beneficiary/victim structures, different types.',
    'If the readings are structurally distinct constraints (different ε, different victims), they must be modeled as separate stories linked by network.affects_constraints, not as one story with measurement parameters. This omega confirms the decomposition is valid.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_absolutist_reading, conceptual, 'Commitment kernel decomposition: absolutist reading as distinct constraint from harm_threshold, marketplace, dignity, democratic_participation readings.').

omega_variable(
    mountain_naturalness_vs_constructed_beneficiaries,
    'Is the absolutist rule a genuine constitutional mountain (emergent from the logic of limited government) or a constructed constraint that benefits identifiable speaker coalitions?',
    'Historical analysis of the absolutist reading''s emergence: was it recognized as categorical from the Founding, or constructed in the 20th century (Schenk → Brandenburg trajectory) by judicial actors aligned with speaker interests? Compare with parallel constitutional regimes that adopted harm-based frameworks.',
    'If constructed, the mountain claim is a false summit — the constraint would reclassify as tangled_rope (coordination + extraction) or snare (extraction with coordination cover). FSM signature would trigger on beneficiary declaration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_naturalness_vs_constructed_beneficiaries, empirical, 'False summit mountain detection: natural law claim vs. constructed beneficiary structure.').

omega_variable(
    categorical_exclusion_boundary_stability,
    'Are the categorical exclusions (incitement, true threats, obscenity, defamation) stable natural kinds or doctrinal artifacts that shrink/expand with judicial composition?',
    'Track the exclusion boundary over time: Chaplinsky (1942) fighting words, Brandenburg (1969) incitement, R.A.V. (1992) content discrimination within exclusions, Stevens (2010) animal crush videos, Alvarez (2012) false statements. Measure whether the ''narrow categories'' are narrowing or expanding.',
    'If exclusions are doctrinally unstable, the ''near-categorical'' claim is performative — the constraint''s actual operation is a moving boundary administered by courts, not a fixed mountain. Theater ratio would be understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_exclusion_boundary_stability, empirical, 'Stability of the categorical exclusion boundary as mountain certification evidence.').

omega_variable(
    algorithmic_amplification_as_new_harm_vector,
    'Does algorithmic amplification of harmful speech constitute a novel harm vector that the absolutist reading''s categorical exclusions cannot capture, creating a structural extraction gap?',
    'Empirical study of harm amplification: compare harm incidence and severity in pre-algorithmic vs. algorithmic speech environments. Test whether existing categorical exclusions (designed for human-scale speech) cover algorithmically amplified stochastic terrorism, radicalization pipelines, and coordinated harassment.',
    'If algorithmic amplification creates harms outside categorical exclusions, the constraint''s extraction rises without doctrinal adaptation — the mountain claim becomes increasingly detached from the constraint''s actual operation. This would show as rising extractiveness in future measurements.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_amplification_as_new_harm_vector, empirical, 'Novel harm vectors outside 20th-century categorical exclusions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__absolutist_reading, 1919, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spk_abs_tr_t1919, speech_protection_kernel__absolutist_reading, theater_ratio, 1919, 0.02).
narrative_ontology:measurement(spk_abs_tr_t1940, speech_protection_kernel__absolutist_reading, theater_ratio, 1940, 0.03).
narrative_ontology:measurement(spk_abs_tr_t1969, speech_protection_kernel__absolutist_reading, theater_ratio, 1969, 0.05).
narrative_ontology:measurement(spk_abs_tr_t1992, speech_protection_kernel__absolutist_reading, theater_ratio, 1992, 0.08).
narrative_ontology:measurement(spk_abs_tr_t2010, speech_protection_kernel__absolutist_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(spk_abs_tr_t2024, speech_protection_kernel__absolutist_reading, theater_ratio, 2024, 0.12).

% Extraction over time
narrative_ontology:measurement(spk_abs_be_t1919, speech_protection_kernel__absolutist_reading, base_extractiveness, 1919, 0.05).
narrative_ontology:measurement(spk_abs_be_t1940, speech_protection_kernel__absolutist_reading, base_extractiveness, 1940, 0.08).
narrative_ontology:measurement(spk_abs_be_t1969, speech_protection_kernel__absolutist_reading, base_extractiveness, 1969, 0.15).
narrative_ontology:measurement(spk_abs_be_t1992, speech_protection_kernel__absolutist_reading, base_extractiveness, 1992, 0.25).
narrative_ontology:measurement(spk_abs_be_t2010, speech_protection_kernel__absolutist_reading, base_extractiveness, 2010, 0.32).
narrative_ontology:measurement(spk_abs_be_t2024, speech_protection_kernel__absolutist_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(spk_abs_su_t1919, speech_protection_kernel__absolutist_reading, suppression_requirement, 1919, 0.85).
narrative_ontology:measurement(spk_abs_su_t1940, speech_protection_kernel__absolutist_reading, suppression_requirement, 1940, 0.65).
narrative_ontology:measurement(spk_abs_su_t1969, speech_protection_kernel__absolutist_reading, suppression_requirement, 1969, 0.35).
narrative_ontology:measurement(spk_abs_su_t1992, speech_protection_kernel__absolutist_reading, suppression_requirement, 1992, 0.2).
narrative_ontology:measurement(spk_abs_su_t2010, speech_protection_kernel__absolutist_reading, suppression_requirement, 2010, 0.15).
narrative_ontology:measurement(spk_abs_su_t2024, speech_protection_kernel__absolutist_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__absolutist_reading, information_standard).
narrative_ontology:boltzmann_floor_override(speech_protection_kernel__absolutist_reading, 0.02).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the speech_protection_kernel. All five readings share the First Amendment text as kernel but instantiate distinct constraints with different ε, different beneficiary/victim structures, and different classifications. The absolutist reading claims mountain status; the harm_threshold and dignity readings likely compute as tangled_rope or snare; the marketplace and democratic_participation readings likely compute as rope or scaffold. The network edges represent the structural influence of the absolutist reading's doctrinal dominance on sibling readings — the absolutist floor sets the baseline from which other readings must argue for exceptions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_kernel__absolutist_reading, institutional, 0.15).
constraint_indexing:directionality_override(speech_protection_kernel__absolutist_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
