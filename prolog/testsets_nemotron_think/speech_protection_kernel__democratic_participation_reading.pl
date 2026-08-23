% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__democratic_participation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__democratic_participation_reading, []).

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
 *   constraint_id: speech_protection_kernel__democratic_participation_reading
 *   human_readable: Democratic Participation Reading of Speech Protection
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   The democratic participation reading of the First Amendment holds that
 *   speech protection is strongest for political expression necessary for
 *   self-governance. This reading, originating in Meiklejohn and
 *   institutionalized through tiered scrutiny doctrine (strict scrutiny for
 *   political speech, intermediate for commercial, rational basis for
 *   low-value categories), creates an internal hierarchy within protected
 *   speech. The constraint is the doctrinal structure that allocates
 *   constitutional protection differentially based on a speech's classified
 *   relationship to democratic deliberation. The reading presents this
 *   hierarchy as a constitutional mountain — a structural necessity of
 *   democratic theory. But the hierarchy has identifiable beneficiaries
 *   (political speakers) and victims (commercial, artistic, and marginalized
 *   speakers), and the category boundary 'political' is actively contested
 *   and litigated. This is a false summit candidate: a constraint that claims
 *   natural constitutional status while distributing protection
 *   asymmetrically.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__democratic_participation_reading, 0.35).
domain_priors:suppression_score(speech_protection_kernel__democratic_participation_reading, 0.45).
domain_priors:theater_ratio(speech_protection_kernel__democratic_participation_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__democratic_participation_reading, mountain).
narrative_ontology:human_readable(speech_protection_kernel__democratic_participation_reading, "Democratic Participation Reading of Speech Protection").
narrative_ontology:topic_domain(speech_protection_kernel__democratic_participation_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__democratic_participation_reading).
domain_priors:emerges_naturally(speech_protection_kernel__democratic_participation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__democratic_participation_reading, '771f7806-642a-4095-8573-caa5d1e42af1').
narrative_ontology:cs_kernel_codification('771f7806-642a-4095-8573-caa5d1e42af1', fixed_text).
narrative_ontology:cs_authority_grounding('771f7806-642a-4095-8573-caa5d1e42af1', lineage).
narrative_ontology:cs_interpretation_layer_present('771f7806-642a-4095-8573-caa5d1e42af1').
narrative_ontology:cs_reading_relation('771f7806-642a-4095-8573-caa5d1e42af1', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('771f7806-642a-4095-8573-caa5d1e42af1', speech_protection_kernel__harm_threshold_reading, influences).
narrative_ontology:cs_reading_relation('771f7806-642a-4095-8573-caa5d1e42af1', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('771f7806-642a-4095-8573-caa5d1e42af1', speech_protection_kernel__dignity_reading, influences).
narrative_ontology:cs_axiom('771f7806-642a-4095-8573-caa5d1e42af1', foundational, political_speech_necessary_for_self_governance).
narrative_ontology:cs_axiom_status(political_speech_necessary_for_self_governance, holdable).
narrative_ontology:cs_axiom_grounding('771f7806-642a-4095-8573-caa5d1e42af1', political_speech_necessary_for_self_governance, deontological).
narrative_ontology:cs_axiom('771f7806-642a-4095-8573-caa5d1e42af1', secondary, hierarchical_protection_justified_by_democratic_function).
narrative_ontology:cs_axiom_status(hierarchical_protection_justified_by_democratic_function, holdable).
narrative_ontology:cs_axiom_grounding('771f7806-642a-4095-8573-caa5d1e42af1', hierarchical_protection_justified_by_democratic_function, instrumental).
narrative_ontology:cs_reference_frame('771f7806-642a-4095-8573-caa5d1e42af1', classical_first_amendment_doctrine).
narrative_ontology:cs_drift_state('771f7806-642a-4095-8573-caa5d1e42af1', contemporary_doctrinal_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('771f7806-642a-4095-8573-caa5d1e42af1', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, political_speakers).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, electoral_candidates).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, political_parties).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, political_press).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, commercial_speakers).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, artistic_speakers).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, nonpolitical_protesters).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, marginalized_community_speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, electoral_candidates).
narrative_ontology:constraint_vindicates(speech_protection_kernel__democratic_participation_reading, democratic_self_governance_requires_uninhibited_political_discourse).
narrative_ontology:constraint_vindicates(speech_protection_kernel__democratic_participation_reading, first_amendment_hierarchy_serves_constitutional_structure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates the boundaries of protected speech categories through tiered scrutiny doctrine. Defines what counts as 'political speech' versus commercial, obscene, or low-value speech. Its categorical decisions determine which speakers receive strict scrutiny protection and which receive lesser review.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, supreme_court, agenda_setter,
    institutional, generational, analytical, national).

% Candidates, officeholders, political parties, and political press organizations. Receive the highest level of constitutional protection for their speech. Can access courts to challenge restrictions with strong presumption of invalidity. Their speech is treated as the core of First Amendment concern.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, political_speakers, beneficiary,
    powerful, biographical, mobile, national).

% Benefit from robust protection for campaign speech and political advocacy. But also bear costs of complex regulatory compliance (campaign finance disclosure, coordination rules) that the democratic participation framework generates. Their speech is protected but heavily structured.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, electoral_candidates, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__democratic_participation_reading, electoral_candidates, payer).

% Corporations, advertisers, professionals engaged in commercial speech. Receive intermediate scrutiny (Central Hudson) rather than strict scrutiny. Their speech can be restricted more easily if government asserts substantial interest. The democratic participation reading treats commercial speech as derivative, not core.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, commercial_speakers, payer,
    moderate, biographical, constrained, national).

% Artists, writers, musicians, performers whose work is not overtly political. Protection depends on whether courts categorize their expression as 'political' or 'artistic.' Non-political artistic speech receives less rigorous protection and can be restricted under broader government interests.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, artistic_speakers, payer,
    moderate, biographical, constrained, national).

% Protesters whose speech addresses issues courts classify as non-political (e.g., purely local grievances, lifestyle choices). Face greater restrictions on time, place, manner and face higher barriers to challenging suppression. Their exit from the constraint is nearly impossible.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, nonpolitical_protesters, payer,
    powerless, immediate, trapped, local).

% Communities whose speech traditions, cultural expression, or advocacy get categorized as non-political by dominant legal frameworks. Their speech is often the first restricted when 'political' is narrowly defined. They cannot exit the categorization system that renders their expression vulnerable.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, marginalized_community_speakers, excluded,
    powerless, generational, identity_locked, national).

% Listeners and participants in democratic discourse. Benefit from robust political debate but also bear costs when non-political speech they value (art, commercial information, cultural expression) is restricted. Can mobilize politically but have no direct doctrinal voice.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, general_public, observer,
    organized, biographical, mobile, national).

% Scholars who analyze, critique, and shape the categories. Produce the theoretical frameworks courts adopt. Some defend the hierarchy as necessary for democracy; others expose how 'political' becomes a tool of exclusion. They observe from outside the constraint's direct operation.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, legal_academy, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of ensuring democratic self-governance by identifying and prioritizing the speech essential to collective decision-making. The hierarchy coordinates legal protection around the constitutional structure's need for uninhibited political discourse.
% TRANSFER_FUNCTION: Moves robust constitutional protection (strict scrutiny, categorical presumption against restriction) from the full domain of speech to the sub-domain classified as 'political.' Political speakers gain near-absolute protection; non-political speakers lose it. The transfer is protection-from-restriction from non-political to political speakers.
% ABSENT_VOICES: Marginalized communities whose speech is structurally categorized as non-political (cultural expression, identity-based advocacy, survival speech). Future generations who inherit the category boundaries. Speakers in emerging domains (digital platforms, algorithmic expression) not yet mapped onto the political/non-political binary. They are excluded because the doctrine's categories were forged in 20th-century broadcast and print contexts.
% DISAPPEARANCE_RATIONALE: If the political/non-political hierarchy vanished overnight, all speech would receive equal strict scrutiny protection. Commercial regulation would face near-insurmountable barriers. Artistic expression would be constitutionally armored. Campaign finance regulation would collapse. The entire architecture of speech regulation would restructure around a flat protection baseline.
% FOUNDING_PROBLEM: How to protect the speech necessary for democratic self-governance from government suppression, while allowing regulation of speech that does not serve the democratic function (commercial fraud, obscenity, incitement). The founding problem is distinguishing the speech the Constitution exists to protect from speech it does not.
% FOUNDING_PROBLEM_CORROBORATION: Political theorists (Dahl, Habermas, Meiklejohn) attest the problem remains live: democracy requires privileged political discourse. Critical legal scholars (Matsuda, Delgado, Lawrence) and empirical political scientists (Gilens, Page) attest the 'political' category is manipulated to entrench power and exclude marginalized voices. The corroboration is split across the beneficiary/payer divide.
narrative_ontology:disappearance_verdict(speech_protection_kernel__democratic_participation_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__democratic_participation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__democratic_participation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_kernel__democratic_participation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__democratic_participation_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__democratic_participation_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, ExtMetricName, E),
    domain_priors:suppression_score(speech_protection_kernel__democratic_participation_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(speech_protection_kernel__democratic_participation_reading),
    narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(speech_protection_kernel__democratic_participation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35) reflects the differential allocation of protection: non-political speakers bear the cost of weaker scrutiny. The reading sees this as coordination, not extraction, so the metric is moderate. Suppression (0.45) is moderate because non-political speech faces real restrictions (commercial regulation, obscenity laws, time-place-manner on non-political protest) but political speech faces almost none. Theater ratio (0.15) is low: the doctrine is genuinely operationalized, not performative. Accessibility collapse (0.65) is moderately high: once a court classifies speech as non-political, alternatives for the speaker collapse significantly. Resistance (0.55) is moderate: constant litigation over category boundaries (Citizens United, Sorrell, Reed, Masterpiece Cakeshop) shows the hierarchy is contested.
 *
 * PERSPECTIVAL GAP:
 *   From the Supreme Court's seat, the hierarchy is a coordination mechanism solving the founding problem — it looks like a mountain. From commercial speakers' seat, it is an extraction mechanism denying them equal protection — it computes as tangled rope or snare. From marginalized communities' seat, it is a snare that categorizes their speech out of protection. The engine computes this per-seat divergence from the structural data; the authored claim (mountain) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court (agenda_setter) sits at d ≈ 0.1 — it administers the hierarchy and benefits from its institutional authority. Political speakers (beneficiaries) sit at d ≈ 0.15 — they receive the subsidy of strict scrutiny. Commercial and artistic speakers (payers) sit at d ≈ 0.7 — they bear the cost of intermediate/rational basis review. Marginalized community speakers (excluded) sit at d ≈ 0.9 — they are trapped in the non-political category by identity-locked exit. The general public and legal academy (observers) sit near d ≈ 0.5. The derivation follows from beneficiary/victim declarations plus exit options: political speakers have mobile exit (can shift forums, platforms); non-political speakers are constrained by regulatory reach; marginalized speakers are identity-locked into the categorization system.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting democratic discourse from suppression) remains contested. The democratic participation reading argues the problem is live and the hierarchy still solves it. Critics argue the hierarchy has outlived its function: the 'political' category now protects corporate political spending (Citizens United) while excluding the speech of the dispossessed. The mandatrophy question — whether the arrangement persists because it solves a live problem or because it benefits the political class — is exactly the contested status. The reading prevents mislabeling by insisting the hierarchy is functional (coordination for democracy); the metrics reveal the extraction pattern (asymmetric protection).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'How does the democratic participation reading''s structural relationship to the speech protection kernel differ from its sibling readings, and what classification consequences follow?',
    'Compare the ε values, beneficiary/victim structures, and computed per-seat classifications across all five readings. The reading with the strongest coordination function and weakest extraction should compute as mountain; readings with asymmetric extraction should compute as tangled rope or snare.',
    'If this reading computes as mountain from the Court''s seat but tangled rope from commercial speakers'' seat, the seat divergence is the measurement. If all readings compute similarly, the kernel may have a single dominant structure beneath the rhetorical differences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Committee-frame structural positioning of this reading within the kernel family.').

omega_variable(
    political_category_boundary,
    'Is the ''political speech'' category a stable, discoverable constitutional boundary, or a manipulable legal construct that expands to protect powerful speakers and contracts to exclude marginalized ones?',
    'Longitudinal doctrinal analysis: track the expansion/contraction of ''political speech'' category across Court eras and correlate with speaker identity and power. If the category tracks power rather than democratic function, it is a construct.',
    'If the category is a stable constitutional boundary, the hierarchy is a mountain (coordination). If it is a manipulable construct tracking power, the hierarchy is a snare or tangled rope (extraction via categorization).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_category_boundary, empirical, 'Whether the core category of the reading is a natural constitutional kind or a constructed boundary.').

omega_variable(
    coordination_extraction_ambiguity,
    'Does the differential protection hierarchy coordinate democratic discourse (solving a collective action problem for self-governance) or extract protection from non-political speakers for the benefit of political elites?',
    'Counterfactual: if the hierarchy were flattened to equal protection for all speech, would democratic deliberation degrade measurably? If yes, coordination. If democratic discourse is robust without it, extraction.',
    'Determines whether the claimed mountain is a genuine coordination structure or a false summit. FSM signature triggers on mountain + beneficiaries; this omega documents the ambiguity the FSM detects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_ambiguity, conceptual, 'Whether the hierarchy''s function is genuinely coordinative or extractive cover.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__democratic_participation_reading, 1942, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spdp_tr_t1942, speech_protection_kernel__democratic_participation_reading, theater_ratio, 1942, 0.05).
narrative_ontology:measurement(spdp_tr_t1964, speech_protection_kernel__democratic_participation_reading, theater_ratio, 1964, 0.08).
narrative_ontology:measurement(spdp_tr_t1976, speech_protection_kernel__democratic_participation_reading, theater_ratio, 1976, 0.1).
narrative_ontology:measurement(spdp_tr_t1992, speech_protection_kernel__democratic_participation_reading, theater_ratio, 1992, 0.12).
narrative_ontology:measurement(spdp_tr_t2010, speech_protection_kernel__democratic_participation_reading, theater_ratio, 2010, 0.14).
narrative_ontology:measurement(spdp_tr_t2024, speech_protection_kernel__democratic_participation_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(spdp_be_t1942, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 1942, 0.15).
narrative_ontology:measurement(spdp_be_t1964, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 1964, 0.22).
narrative_ontology:measurement(spdp_be_t1976, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 1976, 0.28).
narrative_ontology:measurement(spdp_be_t1992, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 1992, 0.31).
narrative_ontology:measurement(spdp_be_t2010, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 2010, 0.33).
narrative_ontology:measurement(spdp_be_t2024, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(spdp_su_t1942, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 1942, 0.3).
narrative_ontology:measurement(spdp_su_t1964, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 1964, 0.35).
narrative_ontology:measurement(spdp_su_t1976, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 1976, 0.4).
narrative_ontology:measurement(spdp_su_t1992, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 1992, 0.42).
narrative_ontology:measurement(spdp_su_t2010, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 2010, 0.44).
narrative_ontology:measurement(spdp_su_t2024, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__democratic_participation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(speech_protection_kernel__democratic_participation_reading, 0.08).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__dignity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the speech_protection_kernel. The absolutist_reading forecloses hierarchy (all speech equal). The harm_threshold_reading replaces category with harm analysis. The marketplace_reading replaces democratic function with truth-discovery. The dignity_reading replaces democratic function with anti-subordination. This reading's hierarchy structurally influences the harm_threshold and dignity readings by setting the baseline categories they must react to.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_kernel__democratic_participation_reading, institutional, 0.1).
constraint_indexing:directionality_override(speech_protection_kernel__democratic_participation_reading, powerful, 0.15).
constraint_indexing:directionality_override(speech_protection_kernel__democratic_participation_reading, moderate, 0.7).
constraint_indexing:directionality_override(speech_protection_kernel__democratic_participation_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
