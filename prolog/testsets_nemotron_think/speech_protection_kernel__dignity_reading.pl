% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__dignity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__dignity_reading, []).

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
 *   constraint_id: speech_protection_kernel__dignity_reading
 *   human_readable: Speech Protection Conditional on Not Functioning as Structural Subordination
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   This constraint story captures the dignity reading of the speech
 *   protection kernel — the constitutional position that speech protection is
 *   conditional on expression not functioning as structural subordination of
 *   target groups. Emerging from post-WWII constitutionalism (German Basic
 *   Law, later Canadian Charter, South African Constitution, ECHR
 *   jurisprudence), this reading recognizes group harm as distinct from
 *   individual harm, treats hate speech and group libel as categorically
 *   unprotected, and makes equal dignity a precondition for legitimate speech
 *   protection. The constraint operates through active enforcement: hate
 *   speech statutes, group libel laws, constitutional balancing tests that
 *   weigh dignity against expression. Beneficiaries are historically
 *   subordinated groups whose identity is fused with the framework
 *   (identity_locked exit). Victims include hate speech speakers, group libel
 *   publishers, and absolutist advocates whose professional identity is
 *   constituted through opposition (also identity_locked). The constraint
 *   shows moderate extraction (0.42) rising from post-war origins, moderate
 *   suppression (0.38) reflecting enforcement machinery, and low theater
 *   (0.22) indicating the coordination function remains substantial. This is
 *   a kernel reading — one of five contested readings of the
 *   speech_protection_kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__dignity_reading, 0.42).
domain_priors:suppression_score(speech_protection_kernel__dignity_reading, 0.38).
domain_priors:theater_ratio(speech_protection_kernel__dignity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__dignity_reading, "Speech Protection Conditional on Not Functioning as Structural Subordination").
narrative_ontology:topic_domain(speech_protection_kernel__dignity_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__dignity_reading, '770074f6-83f4-495e-a37a-2351d7acf533').
narrative_ontology:cs_kernel_codification('770074f6-83f4-495e-a37a-2351d7acf533', formalized).
narrative_ontology:cs_authority_grounding('770074f6-83f4-495e-a37a-2351d7acf533', lineage).
narrative_ontology:cs_interpretation_layer_present('770074f6-83f4-495e-a37a-2351d7acf533').
narrative_ontology:cs_reading_relation('770074f6-83f4-495e-a37a-2351d7acf533', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('770074f6-83f4-495e-a37a-2351d7acf533', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_reading_relation('770074f6-83f4-495e-a37a-2351d7acf533', speech_protection_kernel__harm_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('770074f6-83f4-495e-a37a-2351d7acf533', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_axiom('770074f6-83f4-495e-a37a-2351d7acf533', foundational, equal_dignity_precondition_for_legitimate_speech_protection).
narrative_ontology:cs_axiom_status(equal_dignity_precondition_for_legitimate_speech_protection, holdable).
narrative_ontology:cs_axiom_grounding('770074f6-83f4-495e-a37a-2351d7acf533', equal_dignity_precondition_for_legitimate_speech_protection, deontological).
narrative_ontology:cs_axiom('770074f6-83f4-495e-a37a-2351d7acf533', foundational, group_harm_structurally_distinct_from_individual_harm).
narrative_ontology:cs_axiom_status(group_harm_structurally_distinct_from_individual_harm, holdable).
narrative_ontology:cs_axiom_grounding('770074f6-83f4-495e-a37a-2351d7acf533', group_harm_structurally_distinct_from_individual_harm, deontological).
narrative_ontology:cs_axiom('770074f6-83f4-495e-a37a-2351d7acf533', secondary, hate_speech_as_structural_subordination_unprotected).
narrative_ontology:cs_axiom_status(hate_speech_as_structural_subordination_unprotected, holdable).
narrative_ontology:cs_axiom_grounding('770074f6-83f4-495e-a37a-2351d7acf533', hate_speech_as_structural_subordination_unprotected, deontological).
narrative_ontology:cs_reference_frame('770074f6-83f4-495e-a37a-2351d7acf533', post_war_constitutional_dignity_order).
narrative_ontology:cs_drift_state('770074f6-83f4-495e-a37a-2351d7acf533', contemporary_digital_speech_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('770074f6-83f4-495e-a37a-2351d7acf533', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__dignity_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, historically_subordinated_groups).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, dignity_interest_holders).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, equal_citizenship_advocates).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, hate_speech_speakers).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, group_libel_publishers).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, absolutist_speech_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, legislatures).
narrative_ontology:constraint_vindicates(speech_protection_kernel__dignity_reading, equal_dignity_precondition_for_speech).
narrative_ontology:constraint_vindicates(speech_protection_kernel__dignity_reading, group_harm_distinct_from_individual_harm).
narrative_ontology:constraint_vindicates(speech_protection_kernel__dignity_reading, structural_subordination_as_speech_harm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicate the boundary between protected speech and speech that functions as structural subordination. Define what counts as a target group, what constitutes group harm, and when dignity interests outweigh speech claims. Their rulings set the enforceable standard.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% Enact hate speech and group libel statutes that operationalize the dignity reading. Gain political credit from protected groups for recognition. Face pressure from speech absolutists and international bodies. Exit means repealing or amending their own laws.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, legislatures, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__dignity_reading, legislatures, beneficiary).

% Gain legal recognition that speech targeting their group identity can constitute structural subordination, not mere offense. Use hate speech laws to challenge dehumanizing rhetoric. Their identity is fused with the protection — exit from the framework means losing the legal recognition of group-based harm.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, historically_subordinated_groups, beneficiary,
    organized, generational, identity_locked, national).

% Individuals and organizations that advocate for equal dignity as a constitutional value. Benefit from the doctrinal framework that treats dignity as a precondition for legitimate speech protection. Their professional and advocacy capital is tied to this reading.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, dignity_interest_holders, beneficiary,
    moderate, biographical, constrained, national).

% Speakers whose expression is restricted because it targets protected groups in ways deemed structurally subordinating. Face criminal penalties, civil liability, or platform removal. Exit means self-censorship, migration to jurisdictions with absolutist regimes, or underground speech.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, hate_speech_speakers, payer,
    moderate, immediate, constrained, national).

% Media entities and platforms that publish content classified as group libel. Bear compliance costs, content moderation expenses, and liability risk. Can relocate operations or restructure content pipelines — exit is costly but structurally available.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, group_libel_publishers, payer,
    powerful, biographical, mobile, global).

% Civil liberties organizations and scholars who view any content-based restriction as illegitimate. Lose doctrinal ground when dignity reading prevails. Their professional identity is constituted through opposition to this framework — exit means abandoning their core mission.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, absolutist_speech_advocates, payer,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__dignity_reading, absolutist_speech_advocates, excluded).

% Groups not formally recognized as target groups under current hate speech doctrine (e.g., political dissidents, economic classes, unenumerated identities). Would claim protection if the framework expanded, but have no standing in the current adjudication. Their exclusion is structural — the framework's group definitions gatekeep access.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, unrecognized_marginalized_groups, excluded,
    powerless, generational, trapped, national).

% Monitor whether national dignity-based speech restrictions comply with Article 19 ICCPR and Article 20 hate speech obligations. Issue concluding observations that pressure legislatures and courts. Neither collect nor pay — they audit the constraint's alignment with transnational norms.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents speech from operating as a mechanism of structural subordination that denies equal dignity and citizenship to target groups. Solves the coordination problem of maintaining a public sphere where historically subordinated groups can participate as equals rather than as objects of dehumanization.
% TRANSFER_FUNCTION: Transfers speech liberty from speakers who would target protected groups with subordinating expression to the dignity interests of those groups. The constraint moves the legal permission to dehumanize from the speaker's column to the target group's protection column.
% ABSENT_VOICES: Unrecognized marginalized groups (political dissidents, economic classes, stateless persons) who would seek protection under a dignity framework but fall outside enumerated categories. Future generations who will inherit the boundary lines drawn today. Speakers in jurisdictions without hate speech laws who experience no such restriction — their absence shapes the comparative baseline.
% DISAPPEARANCE_RATIONALE: If the dignity reading vanished overnight, hate speech and group libel laws grounded in structural subordination would lose their doctrinal foundation. Legislatures would face pressure to repeal or narrow them. Courts would revert to harm-threshold or marketplace frameworks. The public sphere would reorganize around a different theory of speech harm — target groups would lose specific legal recognition of group-based subordination as a speech injury.
% FOUNDING_PROBLEM: Post-WWII constitutional orders confronted the failure of classical liberal speech theory to prevent the use of mass communication as a tool of group dehumanization and genocide preparation. The Weimar experience showed that formal speech neutrality could enable structural subordination. The dignity reading was built to ensure speech protection does not become a shield for the communicative infrastructure of hierarchy.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the drafting history of the German Basic Code (Article 1 dignity, Article 5 speech), the Canadian Charter's Section 15/27 interplay, and the International Convention on the Elimination of All Forms of Racial Discrimination (Article 4). Constitutional courts in Germany, Canada, South Africa, and the European Court of Human Rights have cited this genealogy. Absolutist and marketplace readings dispute whether the founding problem was correctly diagnosed or whether the remedy creates worse pathologies.
narrative_ontology:disappearance_verdict(speech_protection_kernel__dignity_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__dignity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__dignity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_kernel__dignity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__dignity_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__dignity_reading_tests).
:- end_tests(speech_protection_kernel__dignity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) reflects the genuine transfer of speech liberty from subordinating speakers to protected groups' dignity interests — not pure rent extraction but a structural reallocation. Suppression (0.38) is moderate because enforcement targets specific categories (hate speech, group libel) rather than speech broadly; alternatives (counterspeech, democratic deliberation) remain legally available for non-subordinating expression. Theater (0.22) is low because the dignity coordination function — preventing the public sphere from becoming a mechanism of hierarchy — is actively performed by courts and legislatures, not merely performed. Accessibility collapse (0.55) is mid-range: once the dignity framework is understood, alternatives (absolutist, marketplace) remain conceptually available but are legally foreclosed in adopting jurisdictions. Resistance (0.48) reflects ongoing contestation from absolutist and marketplace readings, and from jurisdictions that reject the dignity framework.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (courts/legislatures), the constraint appears as necessary coordination — preventing the speech right from becoming a tool of hierarchy. From the primary beneficiary seat (historically subordinated groups), it appears as existential protection — the legal recognition that their equal citizenship requires insulation from subordinating speech. From the payer seats (hate speakers, absolutist advocates), it appears as illegitimate suppression — a content-based restriction that privileges certain groups' dignity over universal speech liberty. The engine computes these divergences from the structural data; the authored claim (tangled_rope) reflects the genuine coordination-extraction hybrid.
 *
 * DIRECTIONALITY LOGIC:
 *   Constitutional courts and legislatures are agenda_setters with institutional power and analytical/constrained exit — they define and administer the boundary. Historically subordinated groups are primary beneficiaries with organized power but identity_locked exit — their citizenship standing is constituted through the framework. Dignity interest holders are secondary beneficiaries with moderate power and constrained exit. Hate speech speakers and group libel publishers are payers with constrained to mobile exit — they bear the restriction but can relocate or self-censor. Absolutist advocates are payers with identity_locked exit — their professional identity requires opposition. Unrecognized marginalized groups are excluded with trapped exit — they have no standing in the current framework. International bodies are observers with analytical exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing speech from enabling structural subordination) remains contested — not dead (subordination persists) but not universally accepted as live (absolutist and marketplace readings dispute the diagnosis). The dignity reading has not atrophied into piton: its enforcement machinery is active, its coordination function is invoked in new contexts (online hate speech, algorithmic amplification), and its beneficiaries remain organized. However, the rising extractiveness trajectory (0.15→0.42) and expanding category recognition (new protected groups, new speech forms) create mandatrophy risk: if the framework expands to cover speech that is not structurally subordinating but merely offensive, the coordination function degrades into pure extraction. The theater ratio's slow rise (0.08→0.22) signals some performative expansion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Does the dignity reading''s core premise (speech protection conditional on not functioning as structural subordination) logically foreclose the absolutist_reading''s core premise (near-categorical protection) within a single constitutional framework, or can a framework instantiate both through contextual modules?',
    'Comparative constitutional analysis: examine jurisdictions that claim to balance both (e.g., US strict scrutiny with narrow exceptions vs. German/European categorical exclusions). Determine whether the exceptions swallow the rule or the rule structures the exceptions.',
    'If forecloses: the kernel cannot stably hold both readings; constitutional orders must choose. If coexists_with: the kernel supports stable pluralism where different speech domains operate under different logics. Classification shifts from tangled_rope toward rope (if stable pluralism) or snare (if foreclosure creates suppression without coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether dignity and absolutist readings are structurally compatible within one framework.').

omega_variable(
    group_harm_operationalization,
    'Can ''structural subordination'' and ''group harm'' be operationalized with sufficient precision to prevent the dignity reading from expanding into a general offense-based speech restriction regime?',
    'Longitudinal study of hate speech jurisprudence: track whether protected categories and prohibited speech types expand beyond groups with historical subordination and speech that materially contributes to hierarchy. Measure correlation between category expansion and theater_ratio increase.',
    'If operationalization fails: the constraint drifts toward snare (extraction without coordination). If operationalization holds: tangled_rope classification stabilizes. Affects mandatrophy_resolved assessment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(group_harm_operationalization, empirical, 'Whether the dignity reading''s boundary criteria resist mission creep.').

omega_variable(
    identity_locked_exit_asymmetry,
    'Do historically subordinated groups and absolutist advocates both experience identity_locked exit, and if so, does this symmetry mean the constraint extracts equally from both sides (rope) or asymmetrically (tangled_rope/snare)?',
    'Exit cost comparison: measure material and psychological costs for a subordinated group member leaving the dignity framework''s protection vs. an absolutist advocate abandoning their oppositional identity. Assess whether the constraint''s persistence depends on one side''s lock-in more than the other''s.',
    'If asymmetry favors payer lock-in: constraint is snare-flavored. If symmetry: constraint is rope-flavored (mutual identity-locked coordination). If asymmetry favors beneficiary lock-in: constraint is scaffold-flavored (transitional protection).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_exit_asymmetry, empirical, 'Whether identity_locked exit operates symmetrically across beneficiary and payer seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__dignity_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spkd_tr_t1945, speech_protection_kernel__dignity_reading, theater_ratio, 1945, 0.08).
narrative_ontology:measurement(spkd_tr_t1960, speech_protection_kernel__dignity_reading, theater_ratio, 1960, 0.12).
narrative_ontology:measurement(spkd_tr_t1975, speech_protection_kernel__dignity_reading, theater_ratio, 1975, 0.16).
narrative_ontology:measurement(spkd_tr_t1990, speech_protection_kernel__dignity_reading, theater_ratio, 1990, 0.19).
narrative_ontology:measurement(spkd_tr_t2005, speech_protection_kernel__dignity_reading, theater_ratio, 2005, 0.21).
narrative_ontology:measurement(spkd_tr_t2025, speech_protection_kernel__dignity_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(spkd_be_t1945, speech_protection_kernel__dignity_reading, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement(spkd_be_t1960, speech_protection_kernel__dignity_reading, base_extractiveness, 1960, 0.22).
narrative_ontology:measurement(spkd_be_t1975, speech_protection_kernel__dignity_reading, base_extractiveness, 1975, 0.31).
narrative_ontology:measurement(spkd_be_t1990, speech_protection_kernel__dignity_reading, base_extractiveness, 1990, 0.38).
narrative_ontology:measurement(spkd_be_t2005, speech_protection_kernel__dignity_reading, base_extractiveness, 2005, 0.41).
narrative_ontology:measurement(spkd_be_t2025, speech_protection_kernel__dignity_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(spkd_su_t1945, speech_protection_kernel__dignity_reading, suppression_requirement, 1945, 0.25).
narrative_ontology:measurement(spkd_su_t1960, speech_protection_kernel__dignity_reading, suppression_requirement, 1960, 0.3).
narrative_ontology:measurement(spkd_su_t1975, speech_protection_kernel__dignity_reading, suppression_requirement, 1975, 0.34).
narrative_ontology:measurement(spkd_su_t1990, speech_protection_kernel__dignity_reading, suppression_requirement, 1990, 0.36).
narrative_ontology:measurement(spkd_su_t2005, speech_protection_kernel__dignity_reading, suppression_requirement, 2005, 0.37).
narrative_ontology:measurement(spkd_su_t2025, speech_protection_kernel__dignity_reading, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__dignity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(speech_protection_kernel__dignity_reading, 0.08).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__democratic_participation_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__marketplace_reading).

% DUAL FORMULATION NOTE:
% This constraint is the dignity_reading of the speech_protection_kernel. The kernel decomposes into five readings with distinct ε values and beneficiary/victim structures. The dignity reading (this story) has ε=0.42 and recognizes group harm. The absolutist_reading has ε≈0.05 (near-zero extraction) but recognizes no group harm. The democratic_participation_reading has ε≈0.15 (low extraction, political speech focus). The harm_threshold_reading has ε≈0.25 (moderate extraction, individual harm focus). The marketplace_reading has ε≈0.10 (low extraction, truth-discovery coordination). They are linked via affects_constraints. The dignity reading forecloses the absolutist_reading within a single framework but coexists_with the others across jurisdictions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_kernel__dignity_reading, organized, 0.15).
constraint_indexing:directionality_override(speech_protection_kernel__dignity_reading, moderate, 0.75).
constraint_indexing:directionality_override(speech_protection_kernel__dignity_reading, powerful, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
