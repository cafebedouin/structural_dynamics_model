% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__dignity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__dignity_reading, []).

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
 *   constraint_id: speech_harm_boundary__dignity_reading
 *   human_readable: Dignity-Based Categorical Speech Exclusion
 *   domain: constitutional_law/political_philosophy/communication_ethics
 *
 * SUMMARY:
 *   This constraint story instantiates the dignity_reading of the
 *   speech_harm_boundary kernel: speech protection is subordinate to human
 *   dignity, and personhood-denying speech (Holocaust denial, hate speech,
 *   group defamation) is categorically unprotected. The constraint operates
 *   through constitutional provisions, criminal codes, and human rights
 *   treaties that carve categorical exclusions from speech protection. It
 *   claims to solve a genuine coordination problem — protecting the equal
 *   standing without which free discourse is a weapon of the strong against
 *   the weak — but extracts heavily from speakers whose expression falls
 *   within the exclusions, requiring active enforcement (criminal
 *   prosecution, content removal, professional sanctions) that intensifies in
 *   the digital era. The claimed type is tangled_rope: genuine coordination
 *   function (dignity protection) fused with asymmetric extraction (speakers
 *   bear the cost). The engine will compute per-seat classifications from the
 *   structural data; the divergence between the agenda-setter/beneficiary
 *   seats (which experience coordination) and payer seats (which experience
 *   extraction) is the measurement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__dignity_reading, 0.78).
domain_priors:suppression_score(speech_harm_boundary__dignity_reading, 0.85).
domain_priors:theater_ratio(speech_harm_boundary__dignity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__dignity_reading, "Dignity-Based Categorical Speech Exclusion").
narrative_ontology:topic_domain(speech_harm_boundary__dignity_reading, "constitutional_law/political_philosophy/communication_ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__dignity_reading, '49dd65c0-e722-4dfc-8aae-23e0b8a64dde').
narrative_ontology:cs_kernel_codification('49dd65c0-e722-4dfc-8aae-23e0b8a64dde', formalized).
narrative_ontology:cs_authority_grounding('49dd65c0-e722-4dfc-8aae-23e0b8a64dde', lineage).
narrative_ontology:cs_interpretation_layer_present('49dd65c0-e722-4dfc-8aae-23e0b8a64dde').
narrative_ontology:cs_reading_relation('49dd65c0-e722-4dfc-8aae-23e0b8a64dde', speech_harm_boundary__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('49dd65c0-e722-4dfc-8aae-23e0b8a64dde', speech_harm_boundary__harm_balancing_reading, coexists_with).
narrative_ontology:cs_axiom('49dd65c0-e722-4dfc-8aae-23e0b8a64dde', foundational, human_dignity_trumps_expression).
narrative_ontology:cs_axiom_status(human_dignity_trumps_expression, holdable).
narrative_ontology:cs_axiom_grounding('49dd65c0-e722-4dfc-8aae-23e0b8a64dde', human_dignity_trumps_expression, deontological).
narrative_ontology:cs_axiom('49dd65c0-e722-4dfc-8aae-23e0b8a64dde', foundational, personhood_denial_categorically_excluded).
narrative_ontology:cs_axiom_status(personhood_denial_categorically_excluded, holdable).
narrative_ontology:cs_axiom_grounding('49dd65c0-e722-4dfc-8aae-23e0b8a64dde', personhood_denial_categorically_excluded, deontological).
narrative_ontology:cs_axiom('49dd65c0-e722-4dfc-8aae-23e0b8a64dde', secondary, equality_precondition_for_free_discourse).
narrative_ontology:cs_axiom_status(equality_precondition_for_free_discourse, holdable).
narrative_ontology:cs_axiom_grounding('49dd65c0-e722-4dfc-8aae-23e0b8a64dde', equality_precondition_for_free_discourse, deontological).
narrative_ontology:cs_reference_frame('49dd65c0-e722-4dfc-8aae-23e0b8a64dde', post_war_dignity_constitutionalism).
narrative_ontology:cs_drift_state('49dd65c0-e722-4dfc-8aae-23e0b8a64dde', contemporary_digital_hate_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('49dd65c0-e722-4dfc-8aae-23e0b8a64dde', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__dignity_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, targeted_identity_groups).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, historically_marginalized_communities).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, victims_of_identity_based_violence).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, speakers_of_identity_harm_speech).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, holocaust_deniers).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, organized_hate_speech_actors).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, group_defamation_publishers).
narrative_ontology:constraint_vindicates(speech_harm_boundary__dignity_reading, human_dignity_as_supreme_constitutional_value).
narrative_ontology:constraint_vindicates(speech_harm_boundary__dignity_reading, personhood_denial_excluded_from_speech_protection).
narrative_ontology:constraint_vindicates(speech_harm_boundary__dignity_reading, equality_as_precondition_for_free_discourse).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicate the boundary between protected speech and dignity-violating speech; authoritatively interpret constitutional dignity provisions; their rulings define the operational contour of the categorical exclusions. They bear institutional responsibility for maintaining the coherence of the dignity framework while facing legitimacy challenges from competing speech frameworks.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__dignity_reading, constitutional_courts, observer).

% Groups protected by categorical exclusions (racial, religious, ethnic, LGBTQ+, disability communities). The constraint secures their equal standing in public discourse by removing personhood-denying speech from protection. Their exit from the constraint's protection is identity-locked — they cannot opt out of being targeted by hate speech, and the constraint's protection is constitutive of their civic equality. They experience the constraint as enabling rather than restricting.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, targeted_identity_groups, beneficiary,
    organized, generational, identity_locked, national).

% Individuals who have suffered violence incited or normalized by dignity-denying speech. The constraint's categorical exclusion aims to break the speech-violence pathway. They are trapped in the harm the constraint addresses; the constraint's enforcement is a condition of their physical and psychic safety. They do not choose the constraint; they survive its absence.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, victims_of_identity_based_violence, beneficiary,
    powerless, biographical, trapped, local).

% Persons whose expression falls within the categorical exclusions (Holocaust denial, hate speech, group defamation). They bear the full restrictive weight of the constraint — criminal liability, civil sanctions, platform removal, professional consequences. Their exit options are constrained: they can cease the prohibited speech, migrate to jurisdictions with weaker exclusions, or speak underground. The constraint extracts their expressive liberty as the price of the dignity protection.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, speakers_of_identity_harm_speech, payer,
    moderate, biographical, constrained, national).

% A specific subclass of identity-harm speakers whose speech is categorically criminalized in multiple jurisdictions (Germany, Austria, France, etc.). They face the most severe enforcement: imprisonment, professional ruin, social ostracization. No legitimate exit exists within the constraint's jurisdiction — the speech itself is defined as an attack on the constitutional order. They are trapped by the constraint's categorical logic.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, holocaust_deniers, payer,
    powerless, immediate, trapped, national).

% Organized movements (neo-Nazi groups, white supremacist organizations, extremist political parties) that strategically deploy dignity-denying speech. They bear enforcement costs (bans, prosecutions, asset seizures) but possess organizational mobility — cross-border operations, digital platform migration, coded language adaptation. They actively contest the constraint's legitimacy and seek to shift the boundary, functioning as both payers and counter-agenda-setters.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, organized_hate_speech_actors, payer,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__dignity_reading, organized_hate_speech_actors, agenda_setter).

% Media platforms, publishers, and digital intermediaries that host or amplify identity-harm speech. They bear compliance costs (content moderation, legal liability, regulatory fines) but possess arbitrage-grade exit: jurisdictional forum-shopping, platform architecture changes, encryption, decentralized hosting. The constraint's extraction from them is modulated by their capacity to route around enforcement.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, group_defamation_publishers, payer,
    powerful, biographical, arbitrage, global).

% Civil libertarians, First Amendment absolutists (US-centered), and liberal theorists who hold that speech protection operates near-absolutely with extremely high harm thresholds. They are excluded from the dignity framework's internal conversation — their position is treated as external critique rather than internal dissent. They would object to categorical exclusions as viewpoint discrimination and slippery-slope precursors, but the constraint's legitimacy structure does not accommodate their objection as a participating voice.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, free_speech_absolutists, excluded,
    organized, civilizational, analytical, global).

% Jurists and scholars (e.g., Canadian, Israeli, South African constitutional courts; European Court of Human Rights marginal opinions) who advocate presumptive protection with proportionality balancing rather than categorical exclusion. They are partially excluded — their framework operates in adjacent jurisdictions and influences the dignity reading's evolution, but within a pure dignity-reading framework they occupy no authorized seat. They would argue categorical rules are over-inclusive and under-inclusive simultaneously.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, proportionality_balancing_advocates, excluded,
    institutional, generational, analytical, global).

% Dissenting voices inside protected groups (e.g., feminist critics of religious hate speech laws, LGBTQ+ critics of certain anti-hate frameworks) who argue the constraint suppresses intra-group contestation. They are identity-locked — they cannot exit the group identity that makes them subject to the constraint's protective logic, yet the constraint's categorical form may silence their internal critique. Their objection is structurally invisible to the constraint's operation.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, minority_viewpoints_within_protected_groups, excluded,
    powerless, biographical, identity_locked, local).

% Comparative constitutional scholars who track the global diffusion, mutation, and contestation of dignity-based speech exclusions. They neither collect nor pay; they map the constraint's structural variants across jurisdictions (German militant democracy, Canadian reasonable limits, US exceptionalism, South African transformative constitutionalism). Their analytical seat sees the full kernel — all three readings — which the constraint's internal participants cannot.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, legal_scholars_comparative_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of protecting human dignity and equal civic standing against speech that denies personhood to groups — a problem no individual can solve alone and that unregulated discourse systematically fails to address. The categorical exclusion creates a stable floor: certain attacks on dignity are removed from the speech market entirely, preventing the coordination failure where dominant groups use expressive liberty to entrench subordination.
% TRANSFER_FUNCTION: Moves expressive liberty from speakers of identity-harm speech (who lose the legal right to deny others' personhood) to targeted groups (who gain secured equal standing in public discourse). The transfer is not monetary but status-based: the constraint reallocates the civic capacity to speak without existential qualification. Speakers of dignity-denying speech pay with criminal liability and social exclusion; protected groups receive the precondition for equal participation.
% ABSENT_VOICES: Free speech absolutists (who reject any categorical exclusion) are structurally excluded — their framework is treated as external to the dignity constitution. Proportionality-balancing advocates (who reject categorical rules for case-by-case balancing) are partially excluded — they influence from adjacent jurisdictions but hold no authorized seat inside the categorical framework. Minority viewpoints within protected groups (who dissent from the majority's definition of harm) are identity-locked excluded — they cannot exit the group identity that subjects them to the constraint's protective logic, yet the categorical form may silence their internal critique.
% DISAPPEARANCE_RATIONALE: If categorical dignity exclusions vanished overnight, hate speech laws across Europe, Canada, South Africa, and international human rights frameworks would collapse. Targeted groups would lose their primary legal shield against personhood-denying speech. Organized hate actors would gain unrestricted access to mainstream platforms. The speech environment would reorganize around the absolutist or balancing reading — the dignity floor would disappear, and the coordination problem of equal standing would revert to an unsolved state.
% FOUNDING_PROBLEM: The post-WWII recognition that unregulated speech enabled the dehumanization propaganda that made genocide possible — that 'free speech' without a dignity floor functions as a weapon against the equality that makes discourse possible. The founding problem is speech that denies the personhood of its targets, thereby destroying the reciprocal recognition that free discourse presupposes.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the Nuremberg Trials record, the Universal Declaration of Human Rights (Art. 1, 7, 29), the German Basic Law (Art. 1), the International Covenant on Civil and Political Rights (Art. 20), the Canadian Charter (s. 1, 15), the South African Constitution (s. 16(2)), and the jurisprudence of the European Court of Human Rights (e.g., Garaudy v. France, Pastörs v. Germany). These sources — spanning victor justice, constitutive documents, and ongoing adjudication — corroborate that the problem of personhood-denying speech as a threat to democratic order remains live. No significant authority outside the beneficiary set disputes the founding problem's existence; the contest is over the remedy (categorical vs. balancing).
narrative_ontology:disappearance_verdict(speech_harm_boundary__dignity_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__dignity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__dignity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_harm_boundary__dignity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__dignity_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__dignity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_harm_boundary__dignity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_harm_boundary__dignity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint categorically removes entire categories of speech from protection, imposing criminal liability and severe sanctions on speakers — a substantial transfer of expressive liberty. Suppression is very high (0.85) because the constraint's persistence depends on active enforcement machinery (specialized hate speech units, platform regulation, transnational cooperation) that actively suppresses alternatives (the absolutist and balancing frameworks) and permits no opt-out for targeted speakers. Theater ratio is moderate (0.42) and rising: the dignity rationale is genuine but a growing share of enforcement activity targets marginal edge cases and digital amplification effects rather than core Holocaust denial/hate speech, suggesting performative expansion. Accessibility collapse (0.68) reflects that once the categorical logic is accepted, alternatives (balancing, absolutism) appear as betrayals of dignity rather than competing frameworks. Resistance (0.55) is moderate: organized hate actors resist strategically, absolutists resist intellectually, but the constraint's institutional embeddedness in post-war constitutionalism makes frontal repeal nearly impossible.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter/beneficiary seats experience the constraint as genuine coordination: a solved collective-action problem that secures the precondition of free discourse. The payer seats experience it as enforced extraction: a categorical ban that removes their speech from the protection the system offers everyone else, maintained by coercive machinery they cannot escape. The engine computes this divergence from the declared roles, power, and exit options. The authored claim (tangled_rope) asserts both experiences are structurally real simultaneously — the constraint IS coordination for some and extraction for others, not a confusion to be resolved.
 *
 * DIRECTIONALITY LOGIC:
 *   Constitutional courts (agenda_setter/observer) sit near the beneficiary end (d ~ 0.15): they administer the constraint and their institutional legitimacy derives from upholding the dignity framework. Targeted identity groups and victims of identity violence (beneficiaries) sit at the deep beneficiary end (d ~ 0.05–0.10): the constraint constitutes their civic equality; they are identity-locked or trapped relative to it. Speakers of identity-harm speech, Holocaust deniers (payers) sit at the deep target end (d ~ 0.90–0.95): they bear the full extractive weight with trapped or constrained exit. Organized hate actors (payer/agenda_setter) sit at high target but with mobility (d ~ 0.75): they pay enforcement costs but contest the agenda. Group defamation publishers (payers) sit at moderate target (d ~ 0.55): powerful with arbitrage-grade exit. Free speech absolutists and balancing advocates (excluded) sit outside the directionality derivation — their structural position is non-participation. Minority viewpoints within protected groups (excluded) are identity-locked non-participants. Legal scholars (observers) are analytical (d = 0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing dehumanization propaganda that enables genocide) remains live — identity-based violence persists, digital amplification creates new vectors, and the dignity floor is contested globally. The constraint has not atrophied; its enforcement has intensified. However, the categorical form creates a mandatrophy risk: as the exclusions expand (from Holocaust denial to hate speech to group defamation to 'harmful misinformation' adjacent to identity categories), the coordination function may become a cover for expanding speech control. The theater_ratio rise (0.25→0.42) signals this drift. The constraint is not yet a piton — the coordination function is real and the mandate is live — but the trajectory warrants monitoring.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'How does the dignity_reading''s categorical exclusion structurally differ from the absolutist_reading''s near-absolute protection and the harm_balancing_reading''s proportionality approach, such that they instantiate different constraints with different ε rather than one constraint measured differently?',
    'Compare the three readings'' beneficiary/victim structures, enforcement architectures, and founding problem status. If each reading names different beneficiaries, different victims, different enforcement machinery, and different founding problem verdicts, they are different constraints per ε-invariance.',
    'Confirms the kernel decomposition is analytically sound. If the readings share the same beneficiary/victim structure and differ only in rhetorical framing, the decomposition would be spurious and the kernel would be a single constraint with measurement variance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Validates the kernel/reading decomposition: three structurally distinct constraints, not one constraint with three measurements.').

omega_variable(
    dignity_naturalness_vs_construction,
    'Is the dignity floor a discovered natural law of constitutional order (a Mountain) or a constructed political choice that benefits identifiable actors (a False Summit Mountain or Tangled Rope)?',
    'Trace the genealogy: if the dignity categorical exclusion emerges from the logical structure of reciprocal recognition (Kantian/Habermasian necessity), it trends Mountain. If it emerges from post-WWII political settlement by specific victors and constitutional drafters, benefiting specific groups and institutions, it trends constructed. The FSM signature will test beneficiary presence on a claimed Mountain.',
    'If Mountain, the constraint''s categorical form is immutable and extraction metrics are misread. If constructed (Tangled Rope/Snare), the high extractiveness is the point — the constraint extracts from speakers to secure dignity for beneficiaries. This omega directly gates FSM evaluation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dignity_naturalness_vs_construction, conceptual, 'Whether the dignity floor is a natural law of discourse or a political construction with beneficiaries.').

omega_variable(
    suppression_mechanism_digital_era,
    'Is the rising suppression_requirement (0.72→0.85) structural (digital platforms enable hate speech at unprecedented scale, requiring new enforcement machinery) or internalized (targeted groups have absorbed the suppression, self-censoring beyond legal requirements)?',
    'Measure post-exit suppression trajectory: if speakers who leave jurisdictional reach (e.g., migrate to US platforms) still self-censor dignity-violating speech, internalization is present. If enforcement machinery growth correlates with platform affordances (algorithmic amplification, encryption, decentralization), structural pressure dominates.',
    'If internalized, effective suppression exceeds the structural measure — targets carry the constraint internally. This would increase χ for payer seats beyond the engine''s structural derivation, potentially shifting computed types toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_digital_era, empirical, 'Structural vs. internalized suppression in the digital enforcement escalation.').

omega_variable(
    categorical_vs_balancing_boundary,
    'Does the categorical exclusion''s rigidity (no proportionality defense for Holocaust denial) create a structural foreclosure of the harm_balancing_reading within any single constitutional framework, or do they coexist as live options across jurisdictions?',
    'Examine constitutional systems that have attempted hybrid approaches (e.g., Canadian Charter s.1 balancing applied to hate speech; South African limitation clause). If no stable hybrid exists — if adopting categorical exclusion for core cases logically commits a framework to rejecting balancing for those cases — then forecloses. If stable hybrids exist, coexists_with.',
    'Determines the reading_relation to harm_balancing_reading. The current authorship declares coexists_with; this omega tests whether that is structurally defensible or whether the categorical logic internally forecloses balancing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_vs_balancing_boundary, conceptual, 'Whether categorical dignity exclusions logically foreclose proportionality balancing within a single framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__dignity_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_harm_boundary__dignity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(spee_tr_t0, observed).
narrative_ontology:measurement(spee_tr_t16, speech_harm_boundary__dignity_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement_basis(spee_tr_t16, observed).
narrative_ontology:measurement(spee_tr_t32, speech_harm_boundary__dignity_reading, theater_ratio, 32, 0.33).
narrative_ontology:measurement_basis(spee_tr_t32, observed).
narrative_ontology:measurement(spee_tr_t48, speech_harm_boundary__dignity_reading, theater_ratio, 48, 0.38).
narrative_ontology:measurement_basis(spee_tr_t48, observed).
narrative_ontology:measurement(spee_tr_t64, speech_harm_boundary__dignity_reading, theater_ratio, 64, 0.4).
narrative_ontology:measurement_basis(spee_tr_t64, observed).
narrative_ontology:measurement(spee_tr_t80, speech_harm_boundary__dignity_reading, theater_ratio, 80, 0.42).
narrative_ontology:measurement_basis(spee_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_harm_boundary__dignity_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(spee_be_t0, observed).
narrative_ontology:measurement(spee_be_t16, speech_harm_boundary__dignity_reading, base_extractiveness, 16, 0.62).
narrative_ontology:measurement_basis(spee_be_t16, observed).
narrative_ontology:measurement(spee_be_t32, speech_harm_boundary__dignity_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement_basis(spee_be_t32, observed).
narrative_ontology:measurement(spee_be_t48, speech_harm_boundary__dignity_reading, base_extractiveness, 48, 0.72).
narrative_ontology:measurement_basis(spee_be_t48, observed).
narrative_ontology:measurement(spee_be_t64, speech_harm_boundary__dignity_reading, base_extractiveness, 64, 0.75).
narrative_ontology:measurement_basis(spee_be_t64, observed).
narrative_ontology:measurement(spee_be_t80, speech_harm_boundary__dignity_reading, base_extractiveness, 80, 0.78).
narrative_ontology:measurement_basis(spee_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_harm_boundary__dignity_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement_basis(spee_su_t0, observed).
narrative_ontology:measurement(spee_su_t16, speech_harm_boundary__dignity_reading, suppression_requirement, 16, 0.75).
narrative_ontology:measurement_basis(spee_su_t16, observed).
narrative_ontology:measurement(spee_su_t32, speech_harm_boundary__dignity_reading, suppression_requirement, 32, 0.78).
narrative_ontology:measurement_basis(spee_su_t32, observed).
narrative_ontology:measurement(spee_su_t48, speech_harm_boundary__dignity_reading, suppression_requirement, 48, 0.81).
narrative_ontology:measurement_basis(spee_su_t48, observed).
narrative_ontology:measurement(spee_su_t64, speech_harm_boundary__dignity_reading, suppression_requirement, 64, 0.83).
narrative_ontology:measurement_basis(spee_su_t64, observed).
narrative_ontology:measurement(spee_su_t80, speech_harm_boundary__dignity_reading, suppression_requirement, 80, 0.85).
narrative_ontology:measurement_basis(spee_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__dignity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(speech_harm_boundary__dignity_reading, 0.08).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, speech_harm_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, speech_harm_boundary__harm_balancing_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, platform_content_moderation_regime).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, international_hate_speech_treaty_obligations).

% DUAL FORMULATION NOTE:
% This constraint is one member of the speech_harm_boundary constraint family (kernel). The three readings — absolutist_reading, dignity_reading, harm_balancing_reading — are distinct constraints with different ε, different beneficiary/victim structures, and different enforcement architectures. They are linked by shared referent (the speech/harm boundary) but instantiate different structural arrangements. The dignity_reading's ε (0.78) reflects heavy restriction on dignity-violating speakers; the absolutist_reading would author ε ≈ 0.10; the harm_balancing_reading would author ε ≈ 0.45. The ε-invariance principle requires separate stories: changing the reading changes the constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_harm_boundary__dignity_reading, organized, 0.75).
constraint_indexing:directionality_override(speech_harm_boundary__dignity_reading, powerful, 0.55).
constraint_indexing:directionality_override(speech_harm_boundary__dignity_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
