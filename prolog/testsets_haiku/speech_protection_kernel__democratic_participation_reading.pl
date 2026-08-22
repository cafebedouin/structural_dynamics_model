% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__democratic_participation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: speech_protection_kernel__democratic_participation_reading
 *   human_readable: Political Speech Protection for Democratic Self-Governance
 *   domain: constitutional/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the democratic-participation reading of the
 *   speech protection kernel: a constitutional norm that provides the highest
 *   protection for political expression necessary for democratic
 *   self-governance, while permitting greater restriction of non-political
 *   speech (commercial, entertainment, private). The reading is one of five
 *   live interpretations of the same underlying commitment to free expression
 *   — it differs from the absolutist reading (which grants all speech equal
 *   protection), the dignity reading (which subordinates speech to
 *   non-subordination of target groups), the harm threshold reading (which
 *   makes all speech conditional on absence of demonstrable injury), and the
 *   marketplace reading (which focuses on truth-discovery through competitive
 *   speech). This story narrates the democratic-participation reading alone:
 *   its beneficiaries (political participants and historically marginalized
 *   groups using voice for political mobilization), its payers (commercial
 *   speakers and those expressing non-political content), its structural
 *   function (enabling democratic contestation), and its foundational tension
 *   (between protecting political participation and addressing dignitary harm
 *   to target groups of hate speech). The constraint is CLAIMED as rope (a
 *   genuine coordination mechanism protecting democratic participation) while
 *   the authored metrics show moderate extractiveness (the hierarchy
 *   privileges political speakers over others) and suppression (the
 *   constraint requires active enforcement to prevent courts and regulators
 *   from flattening the hierarchy or reversing it). The measurement series
 *   track the constraint's enforcement intensity over a 30-unit interval,
 *   showing steady-state operation with no dramatic drift — the reading has
 *   stabilized in constitutional doctrine.
 *
 * KEY AGENTS:
 *   - political_participants: Citizens and organized groups able to mobilize for elections, advocacy, and government critique. Primary beneficiaries of the highest protection tier.
 *   - vulnerable_populations_with_voice: Historically marginalized groups whose political expression is protected as political speech more strongly than equivalent speech by dominant groups would be.
 *   - commercial_and_private_speakers: Bear the cost of the hierarchy; their non-political speech is more readily restricted.
 *   - courts_and_adjudicators: Enforce the political/non-political distinction and assign protection levels accordingly. Agenda-setters of the constraint.
 *   - state_regulators: Constrained from restricting political speech but authorized to regulate non-political speech.
 *   - target_groups_of_hate_speech: Structurally excluded from the primary beneficiary frame despite being present in the discourse; their injury claims are heard but subordinated to political speaker liberty.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__democratic_participation_reading, 0.38).
domain_priors:suppression_score(speech_protection_kernel__democratic_participation_reading, 0.42).
domain_priors:theater_ratio(speech_protection_kernel__democratic_participation_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__democratic_participation_reading, rope).
narrative_ontology:human_readable(speech_protection_kernel__democratic_participation_reading, "Political Speech Protection for Democratic Self-Governance").
narrative_ontology:topic_domain(speech_protection_kernel__democratic_participation_reading, "constitutional/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_kernel__democratic_participation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__democratic_participation_reading, '20e06453-3713-4adf-bc65-296bd725c576').
narrative_ontology:cs_kernel_codification('20e06453-3713-4adf-bc65-296bd725c576', fixed_text).
narrative_ontology:cs_authority_grounding('20e06453-3713-4adf-bc65-296bd725c576', lineage).
narrative_ontology:cs_interpretation_layer_present('20e06453-3713-4adf-bc65-296bd725c576').
narrative_ontology:cs_reading_relation('20e06453-3713-4adf-bc65-296bd725c576', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('20e06453-3713-4adf-bc65-296bd725c576', speech_protection_kernel__harm_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('20e06453-3713-4adf-bc65-296bd725c576', speech_protection_kernel__marketplace_reading, influences).
narrative_ontology:cs_reading_relation('20e06453-3713-4adf-bc65-296bd725c576', speech_protection_kernel__dignity_reading, influences).
narrative_ontology:cs_axiom('20e06453-3713-4adf-bc65-296bd725c576', foundational, political_speech_foundational_to_democratic_legitimacy).
narrative_ontology:cs_axiom_status(political_speech_foundational_to_democratic_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('20e06453-3713-4adf-bc65-296bd725c576', political_speech_foundational_to_democratic_legitimacy, deontological).
narrative_ontology:cs_axiom('20e06453-3713-4adf-bc65-296bd725c576', foundational, protection_hierarchy_political_above_nonpolitical).
narrative_ontology:cs_axiom_status(protection_hierarchy_political_above_nonpolitical, holdable).
narrative_ontology:cs_axiom_grounding('20e06453-3713-4adf-bc65-296bd725c576', protection_hierarchy_political_above_nonpolitical, instrumental).
narrative_ontology:cs_reference_frame('20e06453-3713-4adf-bc65-296bd725c576', political_speech_priority_doctrine).
narrative_ontology:cs_drift_state('20e06453-3713-4adf-bc65-296bd725c576', contemporary_digital_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('20e06453-3713-4adf-bc65-296bd725c576', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, political_participants).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, vulnerable_populations_with_voice).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, commercial_and_private_speakers).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, state_regulators).
narrative_ontology:constraint_vindicates(speech_protection_kernel__democratic_participation_reading, democratic_participation_essential_to_legitimacy).
narrative_ontology:constraint_vindicates(speech_protection_kernel__democratic_participation_reading, political_discourse_foundational_to_self_governance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Citizens and organized groups able to participate in electoral politics, campaign financing, advocacy, and critical evaluation of government. The reading's protection is strongest for their speech — they can contest policy, criticize officials, and mobilize constituencies with constitutional backing. The constraint grants them standing and institutional pathways to challenge restrictions on their political expression.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, political_participants, beneficiary,
    organized, generational, mobile, national).

% Historically marginalized groups whose access to political voice depends on robust speech protection — minority communities, dissidents, social movements that lack conventional political power. The reading protects their speech AS political expression (protest, consciousness-raising, agenda-setting) more strongly than it would protect equivalent speech if reframed as personal or commercial.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, vulnerable_populations_with_voice, beneficiary,
    moderate, generational, constrained, national).

% Advertisers, corporations, entertainment platforms, and individual speakers expressing non-political content (commercial speech, entertainment, personal narrative without political valence) receive lower constitutional protection under this reading. Their speech can be restricted more readily if it causes dignitary or psychological harm, or if other non-political interests are served by restriction. They bear the burden of the hierarchy: political speech gets priority in conflicts.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, commercial_and_private_speakers, payer,
    powerful, biographical, constrained, national).

% Must classify speech along the political/non-political axis and assign protection levels accordingly. The reading creates a judicial function — determining what counts as political participation, what harm thresholds apply, whether a speaker's message is political in character or merely personal/commercial. Courts become the enforcers of the hierarchy.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, courts_and_adjudicators, agenda_setter,
    institutional, generational, analytical, national).

% Can regulate non-political speech more readily but face heightened restrictions on regulating political speech. The reading constrains their authority to restrict protest, campaign speech, or dissent even when they find it harmful, divisive, or offensive. Simultaneously, they are tasked with enforcing the distinction between protected and regulable speech.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, state_regulators, payer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__democratic_participation_reading, state_regulators, agenda_setter).

% Groups subjected to hate speech, slurs, and dehumanizing rhetoric lack a strong seat at the table defining 'political participation' — their claim that such speech subordinates them and silences them is heard but subordinated to the political speaker's right to express it. They are not absent from the discourse (they testify, they organize, they sue) but the reading systematically downgrades their damage claims relative to the political speaker's liberty claim.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, target_groups_of_hate_speech, excluded,
    moderate, biographical, constrained, national).

% Advocates for the absolutist reading (all speech equally protected), the dignity reading (speech conditional on not subordinating target groups), the harm reading (speech conditional on absence of demonstrable injury), and the marketplace reading (truth-discovery focus). These positions remain live in judicial and legislative debate; each offers a different classification of the same utterances.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, sibling_reading_advocates, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__democratic_participation_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_protection_kernel__democratic_participation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a constitutional priority: political expression necessary for democratic self-governance receives the highest protection against state suppression. Solves the coordination problem of how a democracy maintains the capacity for citizens to contest power, mobilize constituencies, and hold government accountable without government being able to preemptively silence dissent.
% TRANSFER_FUNCTION: Transfers protection from commercial and private speakers to political speakers. Non-political speech (advertising, personal narrative, entertainment, commercial activity) becomes more regulable; political speech becomes less regulable. The asymmetry is intentional: it values political participation above other speech interests.
% ABSENT_VOICES: Target groups of hate speech and slurs are not absent from the discourse (they testify about harms, they organize politically, they bring cases), but their claims about dignitary harm and subordination are systematically heard as lower-weight than the political speaker's claim to freedom. The reading treats the targets' injury as real but politically subordinate to democratic participation. Absolutist and dignity readings, if present at the table as primary interpreters, would reweight this heavily.
% DISAPPEARANCE_RATIONALE: If the reading disappeared and an alternative (absolutist or marketplace) took its place, the constitutional hierarchy would collapse: speech restrictions on non-political content would need to meet the same scrutiny as restrictions on political speech, or alternatively all speech would be equally subject to harm-based restrictions. The state's capacity to regulate commercial deception, harassment, defamation, and incitement would shift. The political speech position would lose its special-case protection.
% FOUNDING_PROBLEM: Democracies require that citizens retain the capacity to contest government power, form political movements, and access information and platforms for electoral participation — all without government being able to preemptively silence dissent by classifying it as harmful, obscene, or disruptive.
% FOUNDING_PROBLEM_CORROBORATION: Political scientists, constitutional historians, and democratic theorists outside the judiciary attest the founding problem remains live: authoritarian regimes demonstrate the consequence of giving government control over 'harmful speech' classifications. Judicial testimony, legislative history, and comparative constitutional scholarship document the persistence of the problem. Courts themselves cite democratic participation as the foundational rationale. Target-group advocates dispute that the reading adequately solves the problem (they argue it leaves them unprotected), but they do not dispute that the problem exists.
narrative_ontology:disappearance_verdict(speech_protection_kernel__democratic_participation_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__democratic_participation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__democratic_participation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_kernel__democratic_participation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__democratic_participation_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__democratic_participation_reading_tests).
:- end_tests(speech_protection_kernel__democratic_participation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end, rising from 0.28 at start) because the hierarchy privileges political speech over non-political speech, creating an asymmetry: some speakers' expression is more protected than others' equivalent expression. This is the reading's whole point — it is intentionally extractive in the sense that it transfers protection from commercial/private speakers to political speakers. However, this is extractiveness-from-hierarchy, not extractiveness-from-coercion: no speaker is expelled from the system, and the hierarchy tracks a genuine coordination function (protecting democratic participation). The measurement series show a rapid rise in the early interval (t0-t10), then stabilization, suggesting the reading settled into stable institutional form and then faced increasing pressure to enforce distinctions (theater ratio rises from 0.12 to 0.22, indicating courts and regulators invest more effort in classification and boundary-maintenance). Suppression requirement is moderate (0.42) because the constraint's persistence depends on active enforcement: judges must continuously classify speech as political or non-political, and state regulators must resist the temptation to restrict dissent by reframing it as non-political harm. Without this enforcement, regulators would flatten the hierarchy, and the reading would collapse into marketplace or absolutist form.
 *
 * PERSPECTIVAL GAP:
 *   The political participant seats and the beneficiary seats perceive the constraint as genuine rope: it protects their capacity to contest power and mobilize constituencies — a real coordination problem solved. The commercial/private speaker seats perceive the same structure as extractive hierarchy: their equivalent speech receives lower protection not because of functional necessity but because the hierarchy privileges a different class of speaker. State regulators and courts perceive enforcement burden: the constraint requires them to continuously make political/non-political distinctions and defend them against legal challenge. Target groups of hate speech perceive both beneficiary and payer roles: they are beneficiaries as political participants (their protest, consciousness-raising, and agenda-setting receive high protection) but payers as targets (they bear the dignitary cost of protected speech against them, and the reading subordinates their injury claims). The engine computes these divergences from the structural data: beneficiary role + organized power produces low directionality for political participants; payer role + commercial framing produces high directionality for non-political speakers; excluded role produces a muted voice for target groups in the primary frame.
 *
 * DIRECTIONALITY LOGIC:
 *   Political participants are beneficiaries with organized power and mobile exit (they can shift political activity across jurisdictions or platforms, though each choice carries cost). Their directionality is low — they are subsidized by the constraint's hierarchy. Commercial speakers are payers (they bear the cost of lower protection) with powerful institutional power but constrained exit (they cannot leave the jurisdiction without losing market access). Their directionality is moderate-high — they are targets of the hierarchy but not completely trapped. Vulnerable populations are dual-positioned: as political participants they are beneficiaries with constrained exit (they are locked into the political arena as a means of gaining voice) — directionality low-moderate; as targets of hate speech they are excluded from the primary beneficiary frame — directionality muted. Courts and regulators are agenda-setters with institutional power and analytical exit — directionality near symmetric, driven by their burden of classification work.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (democracies require protected capacity for citizens to contest power) remains live: authoritarian regimes and democratic backsliding worldwide attest the persistent threat. The constraint's function has not atrophied. However, the reading faces internal mandate drift: courts increasingly face demands to restrict speech on grounds of dignity harm and subordination (dignity reading), harm to vulnerable groups (harm threshold reading), or discovery-of-truth failure (marketplace reading). The theater ratio's rise (0.12 → 0.22) suggests courts are spending more effort defending the political-speech-priority classification against challenges and fewer hours on the substantive judgment itself — classification work becomes more theatrical as dissent grows. This is not mandatrophy (the function lives) but mandate contestation: the reading is under sustained pressure from sibling readings, and enforcement intensity is rising. The constraint does not risk Piton status; it risks being displaced by a sibling reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_speech_boundary_ambiguity,
    'What counts as political speech necessary for democratic self-governance? Where lies the boundary between core political expression and adjacent non-political speech that the reading would allow greater restriction of?',
    'Judicial caselaw and constitutional jurisprudence over time; comparative constitutional analysis across democracies with different boundary definitions; empirical study of which speech restrictions courts approve under the political-speech priority rule.',
    'A narrow boundary (only electoral, candidacy, government critique speech is core political) produces higher extractiveness from non-political speakers and clearer enforcement. A broad boundary (commercial speech, artistic expression, personal narrative can all be political if they inform democratic discourse) produces lower extractiveness and harder enforcement. The reading''s actual operation depends on where courts locate the boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_speech_boundary_ambiguity, conceptual, 'The operational definition of ''political speech necessary for democratic self-governance'' in legal practice.').

omega_variable(
    hierarchy_vs_absolute_reading_pressure,
    'Is the democratic-participation reading stable, or is it under structural pressure from sibling readings (absolutist, dignity, harm-threshold, marketplace) that would flatten or reorient the hierarchy?',
    'Tracking shifts in judicial doctrine, legislative movements, and constitutional amendment proposals over time; monitoring whether courts expand non-political-speech protection or subordinate political-speech protection to competing values.',
    'If pressure is high and uncontested, the reading risks displacement by a sibling reading, especially the dignity reading (which is gaining ground in international human rights law) or the marketplace reading (which is growing in digital contexts). The constraint would not vanish but would be reclassified. If pressure is moderate and contested, the reading remains live and stable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hierarchy_vs_absolute_reading_pressure, empirical, 'Doctrinal stability of the democratic-participation reading against sibling readings.').

omega_variable(
    target_group_voice_exclusion,
    'Are target groups of hate speech and slurs adequately heard in the framing that subordinates their dignitary injury to the speaker''s political liberty, or does the reading systematically exclude their claims from primary consideration?',
    'Empirical analysis of target-group participation in constitutional litigation and doctrine-setting; study of whether the reading provides procedural or substantive mechanisms for target groups to contest their exclusion; observation of whether doctrine migrates toward the dignity reading.',
    'If target groups are systematically unheard, the reading risks mandatrophy (it continues not because it solves a problem but because the institution maintaining it benefits from the status quo). If target groups have adequate voice and dissent, the constraint remains a live contestation. A shift toward the dignity reading would reclassify the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(target_group_voice_exclusion, empirical, 'Whether the reading''s exclusion of target-group voice is a feature or a defect.').

omega_variable(
    sibling_reading_coexistence_foreclosure,
    'Do the democratic-participation and absolutist readings genuinely coexist as live options (different parties hold each), or does one logically foreclose the other (no framework could hold both)?',
    'Examination of constitutional doctrine and practice: can a court apply democratic-participation reasoning in some cases and absolutist reasoning in others without contradiction? Or would adopting absolutism require explicitly overruling the political-speech-priority doctrine?',
    'If they coexist (different parties, different jurisdictions, different moments in time all adopt one or the other), the relation is coexists_with. If they foreclose each other (adopting absolutism logically entails abandoning the hierarchy), the relation is forecloses. This affects the terminal attractor: coexistence predicts long-term stability; foreclosure predicts a winner-take-all outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_coexistence_foreclosure, conceptual, 'Logical relationship between democratic-participation and absolutist readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__democratic_participation_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__democratic_participation_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(spee_tr_t0, observed).
narrative_ontology:measurement(spee_tr_t5, speech_protection_kernel__democratic_participation_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement_basis(spee_tr_t5, observed).
narrative_ontology:measurement(spee_tr_t10, speech_protection_kernel__democratic_participation_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement_basis(spee_tr_t10, observed).
narrative_ontology:measurement(spee_tr_t15, speech_protection_kernel__democratic_participation_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement_basis(spee_tr_t15, observed).
narrative_ontology:measurement(spee_tr_t20, speech_protection_kernel__democratic_participation_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(spee_tr_t20, observed).
narrative_ontology:measurement(spee_tr_t25, speech_protection_kernel__democratic_participation_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement_basis(spee_tr_t25, observed).
narrative_ontology:measurement(spee_tr_t30, speech_protection_kernel__democratic_participation_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(spee_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(spee_be_t0, observed).
narrative_ontology:measurement(spee_be_t5, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement_basis(spee_be_t5, observed).
narrative_ontology:measurement(spee_be_t10, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement_basis(spee_be_t10, observed).
narrative_ontology:measurement(spee_be_t15, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 15, 0.38).
narrative_ontology:measurement_basis(spee_be_t15, observed).
narrative_ontology:measurement(spee_be_t20, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 20, 0.39).
narrative_ontology:measurement_basis(spee_be_t20, observed).
narrative_ontology:measurement(spee_be_t25, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement_basis(spee_be_t25, observed).
narrative_ontology:measurement(spee_be_t30, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(spee_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(spee_su_t0, observed).
narrative_ontology:measurement(spee_su_t5, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement_basis(spee_su_t5, observed).
narrative_ontology:measurement(spee_su_t10, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement_basis(spee_su_t10, observed).
narrative_ontology:measurement(spee_su_t15, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 15, 0.42).
narrative_ontology:measurement_basis(spee_su_t15, observed).
narrative_ontology:measurement(spee_su_t20, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement_basis(spee_su_t20, observed).
narrative_ontology:measurement(spee_su_t25, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement_basis(spee_su_t25, observed).
narrative_ontology:measurement(spee_su_t30, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement_basis(spee_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__democratic_participation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_kernel__democratic_participation_reading, 0.12).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__marketplace_reading).

% DUAL FORMULATION NOTE:
% The speech_protection_kernel family comprises five constraint stories, each instantiating a different reading of the same constitutional commitment. They share the referent (the Free Speech Clause and its judicial interpretation) but differ in ε, beneficiary structure, and stakeholder positions because they define the boundary between protected and regulable speech differently. This story (democratic_participation_reading) instantiates the hierarchical reading: political speech gets highest protection; non-political speech is more readily restricted. The absolutist_reading, by contrast, grants all speech equal protection (lower overall ε and no extractiveness from non-political speakers). The dignity_reading elevates target groups and conditions speech protection on non-subordination (different beneficiary structure). The harm_threshold_reading makes all speech conditional on absence of demonstrable victim injury (different boundary logic). The marketplace_reading focuses on truth-discovery and accepts speech restrictions that impair that function (different coordination function). The five stories should be read as a family, with network edges linking them. Each affects the others by demonstrating alternative framings of the same commitment text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
