% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__absolutist_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: speech_protection_boundary__absolutist_reading
 *   human_readable: Absolutist Reading of Speech Protection (Brandenburg Standard)
 *   domain: constitutional_law/political_philosophy/speech_regulation
 *
 * SUMMARY:
 *   This constraint represents the 'absolutist' reading of First Amendment
 *   speech protection, primarily defined by the Brandenburg v. Ohio standard
 *   (1969), which limits unprotected speech to direct incitement to imminent
 *   lawless action. This reading prioritizes maximal protection for speech,
 *   even offensive or hateful, over concerns about its aggregate harm. It is
 *   a reading of the broader 'speech_protection_boundary' kernel, which is
 *   contested by 'balancing' and 'harm-limited' readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__absolutist_reading, 0.65).
domain_priors:suppression_score(speech_protection_boundary__absolutist_reading, 0.75).
domain_priors:theater_ratio(speech_protection_boundary__absolutist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__absolutist_reading, "Absolutist Reading of Speech Protection (Brandenburg Standard)").
narrative_ontology:topic_domain(speech_protection_boundary__absolutist_reading, "constitutional_law/political_philosophy/speech_regulation").

domain_priors:requires_active_enforcement(speech_protection_boundary__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__absolutist_reading, '5c8a7490-33c5-4c4c-9590-437e68905809').
narrative_ontology:cs_kernel_codification('5c8a7490-33c5-4c4c-9590-437e68905809', fixed_text).
narrative_ontology:cs_authority_grounding('5c8a7490-33c5-4c4c-9590-437e68905809', lineage).
narrative_ontology:cs_interpretation_layer_present('5c8a7490-33c5-4c4c-9590-437e68905809').
narrative_ontology:cs_reading_relation('5c8a7490-33c5-4c4c-9590-437e68905809', speech_protection_boundary__harm_limited_reading, forecloses).
narrative_ontology:cs_reading_relation('5c8a7490-33c5-4c4c-9590-437e68905809', speech_protection_boundary__balancing_reading, coexists_with).
narrative_ontology:cs_axiom('5c8a7490-33c5-4c4c-9590-437e68905809', foundational, speech_is_presumptively_free).
narrative_ontology:cs_axiom_status(speech_is_presumptively_free, holdable).
narrative_ontology:cs_axiom_grounding('5c8a7490-33c5-4c4c-9590-437e68905809', speech_is_presumptively_free, deontological).
narrative_ontology:cs_axiom('5c8a7490-33c5-4c4c-9590-437e68905809', foundational, harm_is_only_imminent_lawless_action).
narrative_ontology:cs_axiom_status(harm_is_only_imminent_lawless_action, holdable).
narrative_ontology:cs_axiom_grounding('5c8a7490-33c5-4c4c-9590-437e68905809', harm_is_only_imminent_lawless_action, conventional).
narrative_ontology:cs_reference_frame('5c8a7490-33c5-4c4c-9590-437e68905809', post_brandenburg_jurisprudence).
narrative_ontology:cs_drift_state('5c8a7490-33c5-4c4c-9590-437e68905809', contemporary_social_media_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5c8a7490-33c5-4c4c-9590-437e68905809', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__absolutist_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, speakers_of_controversial_speech).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, civil_liberties_advocates).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, minoritized_communities).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, targets_of_hate_speech).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from broad protection for their expression, even if it is offensive or hateful, as long as it does not directly incite imminent violence. They can express views without fear of legal reprisal.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, speakers_of_controversial_speech, beneficiary,
    moderate, immediate, mobile, national).

% Champion this reading as essential for a robust marketplace of ideas and a bulwark against government censorship. Their institutional mission is to defend maximal speech rights.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, civil_liberties_advocates, beneficiary,
    organized, generational, analytical, national).

% Bear the aggregate harm of hate speech, harassment, and incitement that falls short of the Brandenburg standard. They experience a chilling effect on their own speech and participation in public life, and face increased vulnerability to violence.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, minoritized_communities, payer,
    powerless, generational, trapped, local).

% Directly experience the psychological and social costs of speech that dehumanizes, threatens, or marginalizes them. Their identity is often the target, making exit from the 'victim' position impossible without abandoning their identity.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, targets_of_hate_speech, payer,
    powerless, immediate, identity_locked, local).

% Interprets and enforces the Brandenburg standard, often requiring a high bar for speech restrictions. It is tasked with balancing First Amendment rights against public order, but this reading prioritizes speech over other concerns.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, judicial_system, agenda_setter,
    institutional, civilizational, constrained, national).

% Are constrained in their ability to pass laws regulating speech that causes harm but does not meet the Brandenburg standard, due to judicial review. They would seek to protect vulnerable groups from speech-related harms.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, legislative_bodies, excluded,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, high bar for speech regulation, providing predictability for speakers and limiting government discretion, thereby coordinating the exercise of free expression.
% TRANSFER_FUNCTION: Transfers the burden of speech-related harms from speakers and the state (which is limited in its ability to regulate) to minoritized communities and targets of hate speech, who bear the social and psychological costs.
% ABSENT_VOICES: Minoritized communities and targets of hate speech are often marginalized in the legal and political discourse that shapes speech doctrine, despite bearing the primary costs. Their experiences of harm are often discounted or reframed as necessary externalities of free expression.
% DISAPPEARANCE_RATIONALE: If the Brandenburg standard vanished, the legal landscape for speech would immediately become highly contested and uncertain. Legislative bodies would likely move to regulate speech causing harm, and the balance of power between speakers and those harmed by speech would fundamentally shift.
% FOUNDING_PROBLEM: The problem of government overreach and censorship, particularly during times of political dissent or social upheaval, where speech was suppressed based on its content or potential for abstract harm.
% FOUNDING_PROBLEM_CORROBORATION: Civil liberties advocates and historical records corroborate the ongoing threat of government censorship. However, minoritized communities and legal scholars argue that while censorship is a live problem, the absolutist reading has created a new problem of unaddressed speech-related harms, shifting the burden rather than solving the underlying issue of power imbalance. The problem is live, but its scope and the appropriate solution are contested.
narrative_ontology:disappearance_verdict(speech_protection_boundary__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__absolutist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__absolutist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(speech_protection_boundary__absolutist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_boundary__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it provides a genuine coordination function (clear rules for free speech) but also involves asymmetric extraction. Speakers of controversial speech and civil liberties advocates benefit from broad protection, while minoritized communities and targets of hate speech bear significant costs in the form of unaddressed harms. Extraction (0.65) is substantial due to the externalized costs, and suppression (0.75) is high because the legal framework actively suppresses attempts to regulate harmful speech that doesn't meet the Brandenburg bar. Theater ratio is low (0.1) as the judicial system genuinely enforces this standard, rather than merely performing it.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of speakers and civil liberties advocates, this is a robust Rope, ensuring freedom. From the perspective of minoritized communities, it operates as a Snare, extracting their safety and dignity for the benefit of others' expression. The judicial system, as agenda-setter, experiences it as a complex, high-stakes balancing act, but its structural commitment to this reading means it enforces the extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Speakers and civil liberties advocates are beneficiaries (d near 0.0) as the constraint subsidizes their ability to speak freely. Minoritized communities and targets of hate speech are victims (d near 1.0) as they bear the unmitigated harms. The judicial system, while nominally neutral, acts as an agenda-setter that enforces the constraint, thus facilitating the extraction from victims to beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing government censorship) is still live, but its application in the absolutist reading has led to a situation where the 'solution' to one problem (censorship) has created or exacerbated another (unaddressed speech-related harms). The classification as Tangled Rope prevents mislabeling it as a pure Rope (ignoring the victims) or a pure Snare (ignoring the coordination function of clear speech rules). The persistence of the founding problem (censorship) is used as cover for the accumulating extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aggregate_harm_quantification,
    'How can the aggregate, systemic harm to minoritized communities from speech that falls short of Brandenburg be reliably quantified and weighed against the benefits of maximal speech protection?',
    'Longitudinal sociological studies, public health data on stress and discrimination, and legal scholarship on the cumulative impact of hate speech.',
    'If quantifiable and severe, it would strengthen arguments for re-evaluating the absolutist reading, potentially shifting the constraint towards a ''harm-limited'' or ''balancing'' classification by demonstrating the true costs borne by victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregate_harm_quantification, empirical, 'Quantification of diffuse, cumulative harms from protected speech.').

omega_variable(
    absolutist_vs_balancing_framing,
    'Is the absolutist reading of speech protection a necessary consequence of First Amendment text and history, or a policy choice that could be reinterpreted to allow for greater harm mitigation?',
    'Deep historical and textual analysis of First Amendment jurisprudence, combined with comparative constitutional law studies on speech regimes in other democracies.',
    'If it''s primarily a policy choice, it opens the door for legislative or judicial re-evaluation without violating foundational principles, potentially shifting the constraint towards a ''balancing'' or ''harm-limited'' reading. If it''s a necessary textual consequence, then any change would require constitutional amendment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(absolutist_vs_balancing_framing, conceptual, 'Whether the absolutist reading is textually compelled or a policy choice.').

omega_variable(
    reading_impact_on_minoritized_speech,
    'Does the broad protection for offensive speech, under the absolutist reading, paradoxically suppress the speech of minoritized communities by creating a hostile environment?',
    'Empirical studies on self-censorship and public participation rates among minoritized groups in environments with high levels of protected hate speech.',
    'If confirmed, it would reveal a deeper, counter-intuitive form of suppression within the constraint, where the protection of some speech leads to the suppression of other speech, further strengthening the ''snare'' aspect for victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_impact_on_minoritized_speech, empirical, 'Impact of absolutist speech protection on minoritized communities'' own speech.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__absolutist_reading, 1969, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1969, speech_protection_boundary__absolutist_reading, theater_ratio, 1969, 0.05).
narrative_ontology:measurement(spee_tr_t1980, speech_protection_boundary__absolutist_reading, theater_ratio, 1980, 0.07).
narrative_ontology:measurement(spee_tr_t1990, speech_protection_boundary__absolutist_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(spee_tr_t2000, speech_protection_boundary__absolutist_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(spee_tr_t2010, speech_protection_boundary__absolutist_reading, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(spee_tr_t2024, speech_protection_boundary__absolutist_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(spee_be_t1969, speech_protection_boundary__absolutist_reading, base_extractiveness, 1969, 0.5).
narrative_ontology:measurement(spee_be_t1980, speech_protection_boundary__absolutist_reading, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(spee_be_t1990, speech_protection_boundary__absolutist_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(spee_be_t2000, speech_protection_boundary__absolutist_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(spee_be_t2010, speech_protection_boundary__absolutist_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(spee_be_t2024, speech_protection_boundary__absolutist_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1969, speech_protection_boundary__absolutist_reading, suppression_requirement, 1969, 0.6).
narrative_ontology:measurement(spee_su_t1980, speech_protection_boundary__absolutist_reading, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(spee_su_t1990, speech_protection_boundary__absolutist_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(spee_su_t2000, speech_protection_boundary__absolutist_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(spee_su_t2010, speech_protection_boundary__absolutist_reading, suppression_requirement, 2010, 0.73).
narrative_ontology:measurement(spee_su_t2024, speech_protection_boundary__absolutist_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, hate_speech_regulation_boundary).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, online_content_moderation_standards).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'speech_protection_boundary' kernel. The other readings are 'harm_limited_reading' and 'balancing_reading', each with different structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
