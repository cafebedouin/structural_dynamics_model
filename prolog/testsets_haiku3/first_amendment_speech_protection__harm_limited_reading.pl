% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__harm_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: first_amendment_speech_protection__harm_limited_reading
 *   human_readable: First Amendment Speech Protection (Harm-Limited Reading)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The harm-limited reading of First Amendment protection holds that free
 *   speech protection yields when speech causes demonstrable unconsented-to
 *   harm to others. This reading emerges from the conflict between absolutist
 *   protection ('no law' means no law) and cases where speech functions as
 *   subordination or injury toward vulnerable groups. The reading draws a
 *   boundary: harmless or consensual speech remains protected; speech causing
 *   measurable injury (harassment, hate campaigns, incitement-adjacent
 *   speech) may be regulated. The kernel is the First Amendment text itself
 *   (fixed); the contest is over what 'protection' permits when speech harms
 *   others. This story instantiates the harm-limited reading as a constraint
 *   whose operation benefits vulnerable minorities (by recognizing
 *   speech-as-injury) and imposes costs on speakers whose expression causes
 *   harm (by subjecting them to regulation). The claim/metric gap is
 *   intentional: the reading is CLAIMED as tangled_rope (coordination +
 *   extraction) while metrics show substantial suppression and theater
 *   growth, modeling regulatory mission-creep and the operational burden of
 *   harm-determination.
 *
 * KEY AGENTS:
 *   - vulnerable_minorities: powerless, trapped, benefit from harm recognition and legal intervention
 *   - unconsented_harm_victims: moderate power, constrained exit, benefit from regulation of speech injuring them
 *   - speakers_causing_demonstrable_harm: moderate power, constrained exit, bear costs of speech regulation
 *   - regulators_speech_adjudicators: institutional power, set harm boundary and enforce it
 *   - absolutist_speech_defenders: excluded, contest the harm boundary itself
 *   - judicial_interpretation_authority: institutional power, resolves the reading contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__harm_limited_reading, 0.68).
domain_priors:suppression_score(first_amendment_speech_protection__harm_limited_reading, 0.72).
domain_priors:theater_ratio(first_amendment_speech_protection__harm_limited_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__harm_limited_reading, "First Amendment Speech Protection (Harm-Limited Reading)").
narrative_ontology:topic_domain(first_amendment_speech_protection__harm_limited_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__harm_limited_reading, '2618a03c-d0e2-40e5-b0d8-f4ed521444d1').
narrative_ontology:cs_kernel_codification('2618a03c-d0e2-40e5-b0d8-f4ed521444d1', fixed_text).
narrative_ontology:cs_authority_grounding('2618a03c-d0e2-40e5-b0d8-f4ed521444d1', lineage).
narrative_ontology:cs_interpretation_layer_present('2618a03c-d0e2-40e5-b0d8-f4ed521444d1').
narrative_ontology:cs_reading_relation('2618a03c-d0e2-40e5-b0d8-f4ed521444d1', first_amendment_speech_protection__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('2618a03c-d0e2-40e5-b0d8-f4ed521444d1', first_amendment_speech_protection__categorical_balancing_reading, influences).
narrative_ontology:cs_axiom('2618a03c-d0e2-40e5-b0d8-f4ed521444d1', foundational, harm_boundary_as_limiting_principle).
narrative_ontology:cs_axiom_status(harm_boundary_as_limiting_principle, holdable).
narrative_ontology:cs_axiom_grounding('2618a03c-d0e2-40e5-b0d8-f4ed521444d1', harm_boundary_as_limiting_principle, deontological).
narrative_ontology:cs_axiom('2618a03c-d0e2-40e5-b0d8-f4ed521444d1', foundational, speech_can_constitute_injury).
narrative_ontology:cs_axiom_status(speech_can_constitute_injury, holdable).
narrative_ontology:cs_axiom_grounding('2618a03c-d0e2-40e5-b0d8-f4ed521444d1', speech_can_constitute_injury, empirically_contingent).
narrative_ontology:cs_reference_frame('2618a03c-d0e2-40e5-b0d8-f4ed521444d1', categorical_speech_protection_with_narrow_exceptions).
narrative_ontology:cs_drift_state('2618a03c-d0e2-40e5-b0d8-f4ed521444d1', contemporary_internet_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('2618a03c-d0e2-40e5-b0d8-f4ed521444d1', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, vulnerable_minorities).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, unconsented_harm_victims).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, speakers_causing_demonstrable_harm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups historically targeted by speech (racial minorities, religious groups, LGBTQ+ persons, immigrant communities) gain protection when their exposure to demonstrably harmful speech is deemed unconsented-to harm. They benefit from the constraint's recognition of speech-as-injury and its validation of their claims that hateful expression causes measurable damage. They cannot exit the speech environment or its harms by choice.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, vulnerable_minorities, beneficiary,
    powerless, generational, trapped, national).

% Individuals subjected to harassment, doxxing, targeted abuse, incitement-adjacent speech, or speech that creates hostile environments gain recognition of harm when the constraint applies. They benefit from legal intervention when harm is 'demonstrable.' Exit is constrained: they cannot opt out of public speech addressed at them; they can restrict their own exposure but at significant social cost (leaving online spaces, limiting professional participation).
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, unconsented_harm_victims, beneficiary,
    moderate, biographical, constrained, national).

% Speakers whose speech is regulated or restricted under this reading bear the cost: they lose the ability to express their views publicly without legal consequence, face injunction or removal from platforms, lose employment, or face civil liability. The constraint determines which speakers are subject to limitation based on demonstrated harm caused by their expression. They cannot fully exit the constraint — exit would require abandoning their message or relocating to jurisdictions with different speech norms.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, speakers_causing_demonstrable_harm, payer,
    moderate, biographical, constrained, national).

% Courts, administrative bodies, and social media platforms enforce the harm boundary: they must determine when speech crosses from protected to regulable, whether harm is 'demonstrable,' and what evidence suffices. They set the standard, adjudicate disputes, and administer remedies. They carry the institutional authority to define and detect 'harm' and to decide when First Amendment protection yields.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, regulators_speech_adjudicators, agenda_setter,
    institutional, generational, analytical, national).

% Civil liberties groups, libertarian legal advocates, and some academic free-speech theorists who hold that First Amendment protection is categorical would contest this reading. They argue that regulable harm must be limited to narrow historical categories (true threats, incitement, defamation) and that expanded harm definitions weaponize speech restriction. They are excluded from the constraint's beneficiary calculus but remain vocal in the legal and cultural contest over the kernel.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, absolutist_speech_defenders, excluded,
    powerful, generational, analytical, national).

% The Supreme Court and federal courts function as the canonical interpreters of First Amendment meaning. They are the final adjudicators of which reading prevails in constitutional doctrine. They observe and resolve the contest between readings through case law and constitutional amendment possibilities.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, judicial_interpretation_authority, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(first_amendment_speech_protection__harm_limited_reading, regulators_speech_adjudicators).
narrative_ontology:fixing_cost_class(first_amendment_speech_protection__harm_limited_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Operates a speech-regulation boundary: identifies when expression causes unconsented-to harm sufficient to justify legal intervention, creating a clear rule separating protected speech (harmless or consented-to) from regulable speech (demonstrably harmful). Solves the coordination problem of how to balance individual expression freedom against collective protection from injury, by anchoring regulation in harm evidence rather than speaker intent or listener offense.
% TRANSFER_FUNCTION: Moves regulatory authority from the speaker (who controls expression) to harm-victims and their institutional advocates (who can restrict harmful speech once harm is proven). Transfers the burden of harm-bearing from victims to speakers: speakers who cause harm now bear the cost of avoiding it or defending their expression. Moves institutional power to regulators and courts who determine harm boundaries.
% ABSENT_VOICES: Speakers engaging in speech that falls below the harm threshold but that some audiences find offensive, insulting, or morally objectionable — they lack standing to invoke harm under this reading. Absolutist speech defenders are structurally excluded from the beneficiary framing; they would argue that the harm boundary itself is the harmful restriction. Categories of speech not yet recognized as harmful (emerging harms, novel forms of injury) may be absent from regulatory consideration until harm is demonstrated and adjudicated.
% DISAPPEARANCE_RATIONALE: If this reading disappeared and were replaced by the absolutist reading, the protected speech set would expand: speakers currently restricted under harm doctrine would regain freedom; harm victims would lose legal recourse to regulate demonstrably harmful expression. If replaced by the categorical-balancing reading, regulable categories would shift in definition (balancing value against harm rather than restricting at harm boundary). The constraint's absence would reshape who bears speech-injury costs and whose expression is legally permitted.
% FOUNDING_PROBLEM: Early-stage First Amendment doctrine (1791) addressed government censorship of political criticism; it lacked tools for addressing non-state speech injuries (mob violence against minorities, targeted harassment, hate campaigns). The harm-limited reading emerged as a response to speech's role in catalyzing violence against protected groups and in creating hostile environments that restrict vulnerable minorities' own speech participation. The founding problem was: how to protect political expression while preventing speech from becoming a mechanism of minority subordination.
% FOUNDING_PROBLEM_CORROBORATION: Harm-harm victims (civil rights organizations, disability advocates, targeted communities) testify that the founding problem is live: hate speech campaigns and coordinated harassment constitute ongoing injury to protected groups. Absolutist defenders argue the founding problem is a framing error: the relevant harm is government censorship, not private speech injury. Academic literature on speech harm (Langton, Rae, scholars of subordination) from outside the benefiting parties provides independent corroboration that speech-as-injury is a structural phenomenon; however, legal doctrine remains contested on whether harm constitutes a First Amendment limit.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__harm_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__harm_limited_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__harm_limited_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(first_amendment_speech_protection__harm_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__harm_limited_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness starts low (0.42) because the reading's initial justification is genuine: protecting vulnerable groups from speech-injury is a coordination problem. It rises to 0.68 over 30 time units and plateaus, modeling expansion of harm categories and regulatory scope beyond the original vulnerable-minority focus. By endpoint, regulators are applying harm doctrine to a broader set of speakers, including those whose speech is offensive but not injury-causing. Theater_ratio rises from 0.18 to 0.41, indicating that enforcement increasingly maintains the regulatory machinery itself rather than addressing the founding harm problem. Suppression rises from 0.48 to 0.72 and plateaus, modeling the ongoing enforcement burden required to sustain the harm boundary against contestation from absolutist speakers. The plateau (no further rise after t=30) captures that enforcement stabilizes at a high level once the regulatory infrastructure is established. These metrics model the tangled-rope character: genuine coordination (protection of speech-injured groups) bundled with substantial extraction (suppression of speakers, regulatory mission-creep).
 *
 * PERSPECTIVAL GAP:
 *   From the vulnerable-minority and harm-victim seats, the constraint is protective — regulation enables their own speech participation in hostile environments. From the speaker seats (especially moderate-power speakers whose expression might be regulated under expanding harm criteria), the constraint is restrictive — it suppresses their expression in the name of protecting others from harm. From the regulator seat, the constraint is the authorization to enforce a boundary, which creates institutional interest in maintaining and expanding that boundary. The engine should compute substantially different types for each seat: beneficiary seats should compute lower extraction; payer seats should compute higher extraction. The gap models how the same structural arrangement (the harm boundary) protects some and restricts others.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable_minorities and unconsented_harm_victims are structural beneficiaries: they receive protection from legal intervention (low directionality, high benefit). Their exit_options are trapped/constrained because they cannot exit the speech environment or opt out of targeted harm. Speakers_causing_demonstrable_harm are structural targets (high directionality): they bear the cost of speech regulation, face removal or restriction, and cannot fully exit the constraint (exit would require abandoning their message). They have constrained exit_options. Regulators are the agenda_setter (institutional power) with analytical exit — they administer the constraint and can alter it. Absolutist defenders are excluded: they would argue for a different boundary entirely, but the constraint's operation does not include their position. Judicial authority is the observer: they can reshape the constraint through constitutional interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is CONTESTED: vulnerable groups attest harm is live and ongoing; absolutist defendants attest the founding problem is a misdirection (the real problem is government speech restriction, not private-speech injury). The constraint is claimed as tangled_rope (it genuinely coordinates minority protection while extracting from speakers), which avoids the false-choice between pure coordination and pure extraction. The theater_ratio rise (0.18 → 0.41) models regulatory mission-creep: as harm doctrine becomes established, enforcement activity increasingly defends the regulatory machinery itself rather than addressing the specific injury-caused harms that justified the reading. This mission-creep is the mandatrophy signal: the constraint persists partly because regulators have institutional interest in maintaining it, not solely because the founding harm problem remains live. The theater plateau (0.41 from t=30 onward) captures that the constraint does not become pure theater (it retains real harm-addressing function), but a substantial share of enforcement is now theatrical — regulatory performance rather than victim protection.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demonstrable_harm_definition_instability,
    'What constitutes ''demonstrable'' harm sufficient to limit First Amendment protection? How is harm measured and by whom?',
    'Legal doctrine clarification through Supreme Court decisions or Congressional legislation specifying harm categories (direct physical injury, psychological injury, economic injury, relational injury, subordination-as-harm). Empirical research on speech-caused injury (harassment, trauma, coordinated targeting). Comparative study of harm thresholds across jurisdictions.',
    'A narrow definition of demonstrable harm (only immediate threats of violence) keeps the constraint close to absolutism; a broad definition (including offensive speech, microaggressions, identity-based insult) expands regulatory scope substantially. The measured extractiveness (0.68) assumes a moderate-to-broad harm definition; narrowing the definition would lower extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demonstrable_harm_definition_instability, empirical, 'Whether ''demonstrable harm'' can be specified with enough precision to constrain regulatory scope.').

omega_variable(
    regulatory_scope_creep_mechanism,
    'Is the rising suppression and theater ratio over the interval a symptom of inevitable regulatory expansion, or a consequence of specific institutional incentives and procedural choices?',
    'Historical comparison of harm-limitation doctrines across jurisdictions and eras. Analysis of regulatory agency growth and enforcement patterns. Study of how harm definitions expand once regulatory authority is established (the Goodhart/specification-game problem).',
    'If inevitable (regulatory expansion is structural), the constraint will continue to extract from speakers as harm categories expand, suggesting conversion to snare. If contingent on institutional design (sunable through procedural constraint), the constraint could be reformed to stabilize at a protective harm-boundary without creep toward extractive regulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_scope_creep_mechanism, empirical, 'Whether regulatory scope expansion is intrinsic to harm-limitation doctrine or a contingent outcome of institutional design.').

omega_variable(
    absolutist_vs_harm_limited_logical_incompatibility,
    'Do the absolutist reading and the harm-limited reading foreclose one another (logically incompatible core premises), or do they coexist as alternative positions held by different parties?',
    'Careful analysis of the core premises: absolutism asserts that First Amendment protection is categorically broad; harm-limitation asserts that protection contracts at harm boundary. These premises directly contradict if both claim to state what the First Amendment text permits. Analysis of whether any single legal framework could coherently hold both (e.g., could one say ''protection is broad AND contracts at harm'' without logical contradiction, or does one premise logically foreclose the other?).',
    'If they foreclose (logically incompatible), the kernel contest has a necessary winner — one reading must prevail for constitutional doctrine to be coherent. If they coexist (held by different parties in genuine dispute), the kernel remains contested and both readings remain live. This affects how the constraint is classified (defeated reading vs. live alternative reading).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(absolutist_vs_harm_limited_logical_incompatibility, conceptual, 'Logical status of the relationship between absolutist and harm-limited readings.').

omega_variable(
    regulator_capture_harm_boundary,
    'To what extent is the harm boundary captured or controlled by the regulators who enforce it? Do regulators use harm doctrine to expand their institutional authority independent of actual harm to victims?',
    'Empirical study of regulatory agency behavior: do agencies expand harm categories to increase enforcement scope? Do they resist narrowing harm definitions even when evidence suggests categories are over-broad? Comparison of agency-driven vs. victim-driven harm claims. Analysis of how regulatory resources are allocated (proportion spent on core victim protection vs. general speech policing).',
    'High regulator capture would confirm the theater_ratio rise is regulatory mission-creep; the constraint would be partially reclassified toward snare (extraction for institutional maintenance rather than victim protection). Low capture would support the tangled_rope claim (coordination + some extraction, but not predatory).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulator_capture_harm_boundary, empirical, 'Whether the harm boundary is legitimately protecting victims or has been captured for regulatory expansion.').

omega_variable(
    kernel_reading_contest_status,
    'Which reading (absolutist, categorical-balancing, harm-limited) currently prevails in U.S. First Amendment doctrine, and is the contest genuinely unresolved or has one reading achieved dominance?',
    'Analysis of current Supreme Court doctrine (dominant test/standard used). Historical periodization of which reading has held sway (Warren Court era, Burger Court, Roberts Court). Identification of whether the contest is still active or has been settled by jurisprudence.',
    'If harm-limited is currently dominant, the constraint describes the operative constitutional reading and has achieved legitimacy. If absolutist is currently dominant, the harm-limited reading describes a subordinate or rejected interpretation. If categorical-balancing is dominant, harm-limited is one variant within a broader balancing framework. The reading''s operative status affects how its beneficiaries and victims are institutionally recognized.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_status, empirical, 'Whether the harm-limited reading prevails in current constitutional doctrine.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.72) structural (external barriers imposed by regulation, legal threat, platform removal) or internalized (speakers internalize harm-boundary norms and self-censor without external enforcement)?',
    'Empirical study of speech behavior: do speakers avoid expression only when external enforcement is active, or do they internalize the harm boundary and avoid harmful speech even when enforcement is absent? Comparison of speaker behavior across jurisdictions with different enforcement strength. Post-removal suppression trajectory: if suppression persists after external enforcement ends, it is partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — speakers carry the suppression with them even if enforcement weakens. If structural, removal of enforcement machinery would reduce suppression significantly. High internalization would support the theater_ratio interpretation (performance becomes real) and suggest the constraint has achieved cultural legitimacy even if extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is externally imposed or internalized by speakers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__harm_limited_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t0, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(firs_tr_t5, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 5, 0.23).
narrative_ontology:measurement(firs_tr_t10, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(firs_tr_t15, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 15, 0.33).
narrative_ontology:measurement(firs_tr_t20, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 20, 0.37).
narrative_ontology:measurement(firs_tr_t25, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 25, 0.39).
narrative_ontology:measurement(firs_tr_t30, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(firs_tr_t35, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement(firs_tr_t40, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(firs_be_t0, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(firs_be_t5, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(firs_be_t10, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(firs_be_t15, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(firs_be_t20, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(firs_be_t25, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(firs_be_t30, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(firs_be_t35, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(firs_be_t40, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t0, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(firs_su_t5, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement(firs_su_t10, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(firs_su_t15, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(firs_su_t20, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(firs_su_t25, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(firs_su_t30, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(firs_su_t35, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement(firs_su_t40, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__harm_limited_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(first_amendment_speech_protection__harm_limited_reading, 0.12).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection__absolutist_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection__categorical_balancing_reading).

% DUAL FORMULATION NOTE:
% The first_amendment_speech_protection kernel decomposes into three constraint stories, each instantiating a different reading of the constitutional text. The harm-limited reading (this story) asserts that protection yields at demonstrable harm boundaries. The absolutist reading asserts categorical protection. The categorical-balancing reading asserts ad-hoc balancing of speech value against competing interests. These are structurally distinct constraints with different ε values, beneficiary/victim structures, and compliance mechanisms. They are NOT the same constraint measured differently; they are three genuinely different constraint interpretations of the same kernel text. Each reading would produce a different set of speakers subject to regulation, different harm thresholds, different institutional authorities, and different outcomes when sibling readings gain jurisprudential ground. Constraint family links document the mutual influence: harm-limited influences the categorical-balancing reading (if harm-doctrine becomes established, balancing standards shift to incorporate harm categories); absolutist forecloses or is foreclosed by harm-limited (one reading's core premise may directly contradict the other's, or they may coexist as live positions in the legal system).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(first_amendment_speech_protection__harm_limited_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
