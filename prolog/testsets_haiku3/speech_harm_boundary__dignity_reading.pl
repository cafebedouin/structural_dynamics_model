% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__dignity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: speech_harm_boundary__dignity_reading
 *   human_readable: Speech Subordinate to Human Dignity (Categorical Exclusion Reading)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint embodies the dignity-reading of the speech/harm boundary
 *   kernel: speech that denies the personhood or fundamental humanity of
 *   protected groups is categorically unprotected, not because of empirical
 *   harm-balancing but because dignity is a constitutional prior—a condition
 *   for rights-bearing itself. The reading instantiates a tangled rope:
 *   genuine coordination function (protecting the dignity floor that makes
 *   democratic participation possible for all) coupled with substantial
 *   extraction from speakers of dignity-violating speech. The constraint is
 *   heavily enforced through criminal law (hate speech statutes), civil
 *   remedies (group defamation), and institutional exclusion (platform
 *   removal, professional sanction). The measure of extractiveness (0.82 at
 *   interval end) reflects that speakers lose expressive freedom
 *   categorically, without case-by-case balancing; the suppression metric
 *   (0.78) reflects the active enforcement infrastructure required to
 *   maintain the boundary. Theater ratio is moderate (0.28) because the
 *   constraint carries genuine coordination content (dignity-as-prior) but
 *   enforcement increasingly focuses on boundary maintenance rather than the
 *   original coordination problem. The authored claim (tangled_rope) matches
 *   the measurement profile: the engine should compute this as tangled_rope
 *   from every seat except the speakers' seat, which may compute snare
 *   depending on exit options. The measurement series track enforcement
 *   intensification from t=0 to t≈20, then plateau as the boundary stabilizes
 *   and becomes institutionalized.
 *
 * KEY AGENTS:
 *   - Protected identity groups: beneficiaries; receive categorical dignity protection; bear diffuse costs of boundary maintenance
 *   - Speakers of dignity-violating speech: payers; face suppression and exclusion; exit constrained by identity fusion
 *   - Constitutional courts and regulators: agenda-setters; define and enforce the boundary; possess institutional power
 *   - Absolutist free-speech advocates: excluded/displaced; their reading is rejected; have institutional voice but institutional weight against them
 *   - Journalism and publishing: payers/excluded; constrained by boundary definitions; navigate litigation risk
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__dignity_reading, 0.82).
domain_priors:suppression_score(speech_harm_boundary__dignity_reading, 0.78).
domain_priors:theater_ratio(speech_harm_boundary__dignity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__dignity_reading, "Speech Subordinate to Human Dignity (Categorical Exclusion Reading)").
narrative_ontology:topic_domain(speech_harm_boundary__dignity_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_harm_boundary__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__dignity_reading, '872786e1-ee66-4ad9-a79c-bb354c7beecd').
narrative_ontology:cs_kernel_codification('872786e1-ee66-4ad9-a79c-bb354c7beecd', fixed_text).
narrative_ontology:cs_authority_grounding('872786e1-ee66-4ad9-a79c-bb354c7beecd', lineage).
narrative_ontology:cs_interpretation_layer_present('872786e1-ee66-4ad9-a79c-bb354c7beecd').
narrative_ontology:cs_reading_relation('872786e1-ee66-4ad9-a79c-bb354c7beecd', speech_harm_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('872786e1-ee66-4ad9-a79c-bb354c7beecd', speech_harm_boundary__harm_balancing_reading, influences).
narrative_ontology:cs_axiom('872786e1-ee66-4ad9-a79c-bb354c7beecd', foundational, human_dignity_constitutional_prior).
narrative_ontology:cs_axiom_status(human_dignity_constitutional_prior, holdable).
narrative_ontology:cs_axiom_grounding('872786e1-ee66-4ad9-a79c-bb354c7beecd', human_dignity_constitutional_prior, deontological).
narrative_ontology:cs_axiom('872786e1-ee66-4ad9-a79c-bb354c7beecd', foundational, personhood_inalienable_by_speech).
narrative_ontology:cs_axiom_status(personhood_inalienable_by_speech, holdable).
narrative_ontology:cs_axiom_grounding('872786e1-ee66-4ad9-a79c-bb354c7beecd', personhood_inalienable_by_speech, deontological).
narrative_ontology:cs_reference_frame('872786e1-ee66-4ad9-a79c-bb354c7beecd', dignity_as_non_negotiable_foundation).
narrative_ontology:cs_drift_state('872786e1-ee66-4ad9-a79c-bb354c7beecd', contemporary_expansion_phase, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('872786e1-ee66-4ad9-a79c-bb354c7beecd', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__dignity_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, protected_identity_groups).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, constitutional_dignity_framework).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, speakers_of_dignity_violating_speech).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, absolutist_free_speech_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, protected_identity_groups).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, journalism_and_publishing).
narrative_ontology:constraint_vindicates(speech_harm_boundary__dignity_reading, human_dignity_primacy).
narrative_ontology:constraint_vindicates(speech_harm_boundary__dignity_reading, personhood_inalienability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive categorical legal protection from speech that denies their personhood, fundamental humanity, or inalienable dignity. This includes Holocaust denial laws, hate speech statutes, and civil group-defamation remedies. They also incur diffuse costs: the maintenance of the boundary requires institutional resources, the definition of the boundary is contested and may expand in unwanted directions, and the constraint's operation can produce unwanted side effects (performative trials, scapegoating of prosecuted speakers, over-inclusion of borderline speech).
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, protected_identity_groups, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__dignity_reading, protected_identity_groups, payer).

% Face criminal prosecution, civil liability, employment sanction, and social exclusion for speech that the constraint classifies as dignity-violating (Holocaust denial, hate speech, group dehumanization, personhood-negation rhetoric). Exit is constrained: behavioral exit requires adopting different utterances (which many experience as identity-loss); geographic exit requires moving to jurisdictions without dignity protection (which may be unavailable). For many speakers, the suppression is internalized—the claim itself becomes identity-constituting such that silence or exit means self-erasure.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, speakers_of_dignity_violating_speech, payer,
    moderate, biographical, identity_locked, national).

% Interpret and enforce the dignity boundary: defining which speech is categorically unprotected, prosecuting violations, awarding remedies, and administering the enforcement infrastructure. They possess institutional power to shape how the boundary expands or contracts over time. They answer to constitutional review (constitutional courts interpret the written constitution) and sometimes to international review (human rights bodies).
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, constitutional_courts_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Constitutional scholars, legal organizations (ACLU in the US, IFEX internationally), and judicial coalitions who argue that speech should be protected unless it incites imminent violence or causes direct concrete harm. They are excluded from shaping the dignity-reading regime: their reading is rejected as subordinating a fundamental right to abstract dignity claims. They have institutional voice and mobility (can migrate to jurisdictions with absolutist frameworks, can publish in academia and law reviews), but face institutional pressure and normative displacement in dignity-prioritizing societies.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, absolutist_free_speech_advocates, excluded,
    powerful, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__dignity_reading, absolutist_free_speech_advocates, payer).

% Navigate the dignity boundary when covering sensitive topics: reporting on genocide denial, hate movements, controversial historical claims, or events affecting protected groups. They are excluded from the boundary-setting process but pay through litigation risk, editorial constraints, self-censorship, and institutional pressure. Their exit is partially mobile (can publish internationally) but constrained by readership and distribution in the home jurisdiction.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, journalism_and_publishing, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__dignity_reading, journalism_and_publishing, excluded).

% Observe and evaluate the dignity boundary's operation: the European Court of Human Rights, UN Human Rights Committee, Inter-American Commission, and other bodies that assess whether national hate-speech laws comply with international human rights standards. They observe from outside any single jurisdiction and compare across readings, making their role analytical rather than stakes-holding.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_harm_boundary__dignity_reading, constitutional_courts_regulators).
narrative_ontology:fixing_cost_class(speech_harm_boundary__dignity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes human dignity as a non-negotiable constitutional foundation: personhood cannot be collectively negated by speech; no group can be legally reduced to non-person status through utterance. This coordinates societies around the commitment that rights-bearing is inalienable and immune to rhetorical erasure.
% TRANSFER_FUNCTION: Transfers expressive freedom from speakers of dignity-violating speech to the state and protected groups: the state gains prosecutorial authority; protected groups gain legal standing and remedies; speakers lose utterance rights for a class of speech.
% ABSENT_VOICES: Absolutist free-speech advocates are normatively displaced but present in institutional discourse (law reviews, some court dissents, international bodies). Speakers of dignity-violating speech are silenced by the constraint itself, not merely disagreed with—their absence is structural, not deliberative. Comparative constitutional traditions in absolutist jurisdictions (US, some Commonwealth countries) are absent from the dignity-reading regime's shaping process.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, protected groups would lose legal dignity protection; personhood-denial would return to public discourse; the social meaning of constitutional rights would shift—dignity would become a contestable value rather than a constitutional prior. The institutional and discursive landscape would reorganize toward either absolutist or harm-balancing frameworks.
% FOUNDING_PROBLEM: Post-Holocaust recognition that speech dehumanizing entire populations functionally precedes atrocity: legal systems must protect dignity as a condition for democratic participation and rights-bearing itself, not as a competing value to be balanced against speech.
% FOUNDING_PROBLEM_CORROBORATION: Holocaust historians, genocide scholars (Ben Kiernan, Timothy Snyder), and empirical researchers on hate-speech causation outside the benefiting parties attest that dehumanizing speech preceded systematic atrocity in multiple 20th-century genocides. International legal instruments codify this connection. Absolutist free-speech scholars contest whether the causal pathway is as direct in contemporary contexts, but do not contest that the historical connection exists.
narrative_ontology:disappearance_verdict(speech_harm_boundary__dignity_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__dignity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__dignity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_harm_boundary__dignity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__dignity_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.82) because the constraint removes speaker choice entirely for a class of speech (categorical, not proportional). The speaker's position is that dignity protection requires no case-by-case showing of causation or empirical harm—the personhood-denial itself violates dignity and forfeits protection. Suppression is higher still (0.78) because enforcement is active and ongoing: states prosecute, civil courts award damages, platforms remove content, employers sanction speech. The constraint is not passive; it requires dedicated enforcement infrastructure. Theater is lower (0.28) because while boundary-maintenance has theatrical elements (performative denunciation of hate speech, symbolic court victories), the core coordination function (dignity as constitutional prior) remains substantive—the theater has not yet displaced the function. The measurement trajectory shows enforcement intensification from t=0 to t≈20, reflecting expansion of hate-speech law and social pressure against dignity-violating speech in European and some Commonwealth jurisdictions, then stabilization as the boundary becomes institutionalized. At t=35 the metrics plateau, suggesting the regime has reached equilibrium: enforcement is stable, boundary definitions are settled, resistance persists but does not grow. The temporal series are authored on one shared grid (every metric at every time point) so cross-metric comparison is valid.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (protected groups), the constraint is coordination—a necessary floor for equal citizenship. From the suppressed-speaker seat, it is pure extraction: expressive freedom lost without proportional return. From the courts' analytical seat, it is enforcement of a normative priority (dignity > speech in this reading). From the absolutist advocate seat, it appears as a false-coordination that trades away a fundamental right for what they read as identity-group privilege. The engine computes per-seat classification from power, exit, beneficiary/victim structure, and the shared metrics; the authored claim (tangled_rope) reflects the structure from the beneficiary and regulatory seats—genuine coordination + asymmetric extraction + active enforcement. The payer seats may compute snare depending on how identity_locked and constrained exit options modulate directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (protected_identity_groups): collects dignity protection without running the enforcement machinery; d is low (near beneficiary pole, ~0.2-0.3) because they receive net benefit and have mobile exit (can speak in other jurisdictions or forums). However, they also pay through diffuse maintenance costs, so their d is not fully beneficiary-end; secondary role of payer moderates. Payers (speakers_of_dignity_violating_speech): lose expressive freedom categorically; exit is identity_locked (for many, adopting non-dignity-violating speech is identity loss; for others, spatial exit to absolutist jurisdictions is available but carries social cost); d is high (near target pole, ~0.85-0.95). Excluded/displaced (absolutist_advocates, journalism): face institutional pressure and cannot reshape the boundary; secondary role or pure exclusion; d depends on power and exit (institutional actors with mobile exit sit middle; constrained publishers sit higher target). The directionality derivation chain produces these values from the authored structural data; overrides are not needed because the power atoms and exit options align the derivation to the real structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: speech preceding atrocity remains empirically documented and remains a concern in jurisdictions where genocide-denial or hate speech is prevalent. The disappearance verdict is world_rearranges: if the constraint vanished, protected groups would lose legal recourse and personhood-denial would re-enter public discourse. These are consistent: the constraint's founding mandate has not become obsolete. However, mandatrophy risk exists in three forms: (1) Boundary expansion: as courts extend the exclusion to increasingly broad categories of speech (criticism of state policies affecting groups, speech on sensitive historical claims), the constraint begins to suppress legitimate discourse and the coordination function (dignity protection) becomes contaminated by extraction (suppression of political dissent). (2) Theater accumulation: as courts treat boundary-maintenance as its own end (symbolic trials, prosecutions of elderly deniers with minimal real-world reach), the constraint's extractive surface grows relative to coordination. (3) Identity-lock deepening: as the constraint becomes institutionalized, speakers' identity-fusion with prohibited claims intensifies (the claim becomes identity-constituting), making exit impossible even in principle. The measurement of theater_ratio plateauing at 0.28 and extraction stabilizing at 0.82 suggests the regime is not yet entering mandatrophy—the boundary has not yet become purely performative—but the trajectory from t=0 to t=20 (theater increasing, extraction increasing) shows the risk vector. The omega variables address this fragility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dignity_boundary_expansion,
    'As the dignity-reading regime matures, does the boundary''s scope expand from atrocity-connected speech (Holocaust denial, explicit dehumanization) to broader categories (criticism of state policies toward groups, heterodox historical claims, philosophical skepticism about group identity)?',
    'Longitudinal analysis of case law and statute: tracking which speech categories are brought under prosecution or civil liability over decades, measuring the expansion rate.',
    'If boundary expands into legitimate discourse, the constraint transitions from tangled_rope (coordination + targeted extraction) to snare (extraction masked by coordination rhetoric). Mandatrophy risk rises sharply. If boundary remains tightly defined around personhood-denial, the coordination function is preserved.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dignity_boundary_expansion, empirical, 'Risk of boundary creep toward suppression of legitimate discourse.').

omega_variable(
    speaker_identity_lock_mechanism,
    'For speakers whose dignity-violating claims are identity-constituting (ideology, religious doctrine, group-membership claim), is the suppression structural (external barriers to utterance) or internalized (the speaker has fused identity with the claim such that exit means identity-loss)?',
    'Ethnographic and psychological studies of speakers in dignity-protecting regimes: do they maintain the claim in private while self-censoring publicly (structural suppression), or do they internalize the constraint and the claim becomes central to how they understand themselves?',
    'If suppression is structural only, exit is behaviorally available (change utterance, move jurisdiction). If internalized, the speaker carries the suppression after exit—the constraint''s effective suppression is higher than the structural measure suggests, and directionality for identity-locked speakers approaches full target (d~0.95). Classification and mandatrophy risk both shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speaker_identity_lock_mechanism, empirical, 'Whether speaker suppression is structural or identity-internalized; affects effective extraction and exit options.').

omega_variable(
    coordination_content_degradation,
    'Does the dignity-protection coordination function degrade over time as courts shift from protecting against personhood-denial toward maintaining symbolic enforcement and boundary policing for their own sake?',
    'Qualitative analysis of judicial reasoning in hate-speech cases over decades: tracking whether courts justify rulings by reference to dignity protection and the harms of group-dehumanization, or by reference to maintaining the boundary and upholding precedent.',
    'If degradation occurs (shift from coordination rationale to boundary-maintenance rationale), theater_ratio should rise above 0.5 and the constraint should compute as piton rather than tangled_rope. If coordination content remains primary, the theater ratio stays below 0.4 and the constraint holds tangled-rope class.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_content_degradation, empirical, 'Whether judicial commitment to dignity protection as the core function remains intact or theaters performance replaces it.').

omega_variable(
    reading_foreclosure_via_axiom_override,
    'Does the absolutist reading''s foundational axiom (speaker_liberty_near_absolute) become empirically overridden by evidence that dignity-violating speech precedes atrocity, such that the absolutist reading logically breaks even for its own advocates?',
    'New evidence or historiography establishing causal pathways from speech to atrocity with specificity that absolutist legal theory must accommodate—or reversion to pre-causal empirical theories. Whether absolutist scholars abandon or re-interpret their axiom.',
    'If overridden, the absolutist reading becomes untenable (axiom_overriding drift) and the kernel resolves toward either dignity or harm-balancing readings. If the axiom holds despite evidence, the kernel remains contested. This omega documents the commission-system refutation path for the absolutist reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_via_axiom_override, empirical, 'Whether new evidence on speech-atrocity causation forecloses the absolutist reading by contradicting its foundational empirical axiom.').

omega_variable(
    international_coordination_emergence,
    'Does the dignity reading spread internationally through treaty obligation (ICC, UN hate-speech conventions) and comparative law diffusion, creating a global regime that converges on the dignity-reading interpretation of the kernel?',
    'Tracking ratification of hate-speech treaties, adoption of hate-speech law in new jurisdictions, and citation of dignity-reading courts in countries without prior hate-speech tradition.',
    'If convergence occurs, the absolutist reading becomes a regional minority position rather than a co-equal constitutional option. The kernel''s contestation may asymptote toward resolution in favor of the dignity reading (though the absolutist reading would likely persist in some jurisdictions). International courts and human rights bodies would amplify the dignity reading''s authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_coordination_emergence, conceptual, 'Whether the dignity reading achieves hegemonic status internationally or remains one of multiple live readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__dignity_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(speech_dignity_tr_t0, speech_harm_boundary__dignity_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(speech_dignity_tr_t0, projected).
narrative_ontology:measurement(speech_dignity_tr_t5, speech_harm_boundary__dignity_reading, theater_ratio, 5, 0.21).
narrative_ontology:measurement_basis(speech_dignity_tr_t5, projected).
narrative_ontology:measurement(speech_dignity_tr_t10, speech_harm_boundary__dignity_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement_basis(speech_dignity_tr_t10, observed).
narrative_ontology:measurement(speech_dignity_tr_t15, speech_harm_boundary__dignity_reading, theater_ratio, 15, 0.26).
narrative_ontology:measurement_basis(speech_dignity_tr_t15, observed).
narrative_ontology:measurement(speech_dignity_tr_t20, speech_harm_boundary__dignity_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(speech_dignity_tr_t20, observed).
narrative_ontology:measurement(speech_dignity_tr_t25, speech_harm_boundary__dignity_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(speech_dignity_tr_t25, observed).
narrative_ontology:measurement(speech_dignity_tr_t30, speech_harm_boundary__dignity_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(speech_dignity_tr_t30, observed).
narrative_ontology:measurement(speech_dignity_tr_t35, speech_harm_boundary__dignity_reading, theater_ratio, 35, 0.28).
narrative_ontology:measurement_basis(speech_dignity_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(speech_dignity_be_t0, speech_harm_boundary__dignity_reading, base_extractiveness, 0, 0.64).
narrative_ontology:measurement_basis(speech_dignity_be_t0, projected).
narrative_ontology:measurement(speech_dignity_be_t5, speech_harm_boundary__dignity_reading, base_extractiveness, 5, 0.69).
narrative_ontology:measurement_basis(speech_dignity_be_t5, projected).
narrative_ontology:measurement(speech_dignity_be_t10, speech_harm_boundary__dignity_reading, base_extractiveness, 10, 0.74).
narrative_ontology:measurement_basis(speech_dignity_be_t10, observed).
narrative_ontology:measurement(speech_dignity_be_t15, speech_harm_boundary__dignity_reading, base_extractiveness, 15, 0.78).
narrative_ontology:measurement_basis(speech_dignity_be_t15, observed).
narrative_ontology:measurement(speech_dignity_be_t20, speech_harm_boundary__dignity_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement_basis(speech_dignity_be_t20, observed).
narrative_ontology:measurement(speech_dignity_be_t25, speech_harm_boundary__dignity_reading, base_extractiveness, 25, 0.81).
narrative_ontology:measurement_basis(speech_dignity_be_t25, observed).
narrative_ontology:measurement(speech_dignity_be_t30, speech_harm_boundary__dignity_reading, base_extractiveness, 30, 0.82).
narrative_ontology:measurement_basis(speech_dignity_be_t30, observed).
narrative_ontology:measurement(speech_dignity_be_t35, speech_harm_boundary__dignity_reading, base_extractiveness, 35, 0.82).
narrative_ontology:measurement_basis(speech_dignity_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(speech_dignity_su_t0, speech_harm_boundary__dignity_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement_basis(speech_dignity_su_t0, projected).
narrative_ontology:measurement(speech_dignity_su_t5, speech_harm_boundary__dignity_reading, suppression_requirement, 5, 0.7).
narrative_ontology:measurement_basis(speech_dignity_su_t5, projected).
narrative_ontology:measurement(speech_dignity_su_t10, speech_harm_boundary__dignity_reading, suppression_requirement, 10, 0.74).
narrative_ontology:measurement_basis(speech_dignity_su_t10, observed).
narrative_ontology:measurement(speech_dignity_su_t15, speech_harm_boundary__dignity_reading, suppression_requirement, 15, 0.76).
narrative_ontology:measurement_basis(speech_dignity_su_t15, observed).
narrative_ontology:measurement(speech_dignity_su_t20, speech_harm_boundary__dignity_reading, suppression_requirement, 20, 0.77).
narrative_ontology:measurement_basis(speech_dignity_su_t20, observed).
narrative_ontology:measurement(speech_dignity_su_t25, speech_harm_boundary__dignity_reading, suppression_requirement, 25, 0.78).
narrative_ontology:measurement_basis(speech_dignity_su_t25, observed).
narrative_ontology:measurement(speech_dignity_su_t30, speech_harm_boundary__dignity_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement_basis(speech_dignity_su_t30, observed).
narrative_ontology:measurement(speech_dignity_su_t35, speech_harm_boundary__dignity_reading, suppression_requirement, 35, 0.78).
narrative_ontology:measurement_basis(speech_dignity_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__dignity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_harm_boundary__dignity_reading, 0.12).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, speech_harm_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, speech_harm_boundary__harm_balancing_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, hate_speech_prosecution_infrastructure).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, group_defamation_law).

% DUAL FORMULATION NOTE:
% The speech_harm_boundary kernel admits three structurally distinct constraint readings: ABSOLUTIST_READING (speech protection near-absolute; harm threshold very high), DIGNITY_READING (this story: speech subordinate to dignity; categorical exclusions), HARM_BALANCING_READING (presumptive protection yielding to demonstrated harm; proportional balancing). Each reading has its own ε, its own beneficiary/victim structure, its own measured suppression and enforcement. The three readings are linked by network edges because each reading's viability affects the others: dignity courts create institutional pressure on absolutist jurisdictions; harm-balancing courts compete with both; outcomes in one jurisdiction reverberate through comparative law. The three stories are separate constraint_ids with separate JSON files; they share the kernel_id and declare their reading_relations and axioms in cs_structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_harm_boundary__dignity_reading, moderate, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
