% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__proportionality_balancing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__proportionality_balancing, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: humane_treatment_standard__proportionality_balancing
 *   human_readable: Common Article 3 Proportionality Balancing Standard
 *   domain: international_humanitarian_law/human_rights/state_security
 *
 * SUMMARY:
 *   Common Article 3 of the Geneva Conventions prohibits torture and
 *   degrading treatment of detained persons. Three distinct readings exist:
 *   (1) absolute prohibition—no techniques are ever permissible; (2)
 *   contextual necessity—security imperatives override humanitarian
 *   protections in crisis; (3) proportionality balancing—neither absolute
 *   prohibition nor unlimited discretion, but case-by-case assessment of
 *   whether techniques are proportional to security needs. This constraint
 *   story instantiates reading (3). The proportionality standard positions
 *   courts as gatekeepers, interrogators as constrained (but not prohibited),
 *   and detainees as protected (but not absolutely). The framework presents
 *   itself as balanced but operates asymmetrically: interrogators assess
 *   proportionality in real time with operational discretion, courts review
 *   after the fact with deference to security judgments, and detainees cannot
 *   contest the assessment until interrogation concludes. The constraint
 *   simultaneously coordinates (provides a shared language for legitimacy)
 *   and extracts (permits interrogation that would be prohibited under
 *   absolute-prohibition reading). The kernel context: this is one reading of
 *   a textual kernel (Common Article 3) that three communities interpret
 *   differently. Absolute-prohibition advocates read the text as establishing
 *   non-derogable standards; proportionality advocates read the same text as
 *   permitting balancing; security-context advocates read it as
 *   context-dependent. The proportionality reading is neither the plainest
 *   reading nor the most protective—it is the middle path courts have
 *   institutionally evolved toward, which conveniently expands judicial
 *   authority.
 *
 * KEY AGENTS:
 *   - Interrogating state authorities: institutional agenda-setters controlling the initial proportionality assessment and interrogation technique selection
 *   - Detained persons: powerless payers, trapped without legal standing to contest proportionality in real time
 *   - Oversight courts: institutional beneficiaries whose authority expands when every case becomes a proportionality question; gatekeepers of the standard
 *   - Detainee protection advocates: organized payers and excluded voices, systematically outside the proportionality decision loop
 *   - International oversight bodies: analytical observers without enforcement power over national courts or interrogation authorities
 *   - Security apparatus: institutional beneficiaries operating under proportionality framing that permits escalation justified by threat assessment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__proportionality_balancing, 0.62).
domain_priors:suppression_score(humane_treatment_standard__proportionality_balancing, 0.58).
domain_priors:theater_ratio(humane_treatment_standard__proportionality_balancing, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, extractiveness, 0.62).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__proportionality_balancing, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__proportionality_balancing, "Common Article 3 Proportionality Balancing Standard").
narrative_ontology:topic_domain(humane_treatment_standard__proportionality_balancing, "international_humanitarian_law/human_rights/state_security").

domain_priors:requires_active_enforcement(humane_treatment_standard__proportionality_balancing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__proportionality_balancing, '5fb48b3d-0506-4993-8f22-aa5501912ad9').
narrative_ontology:cs_kernel_codification('5fb48b3d-0506-4993-8f22-aa5501912ad9', fixed_text).
narrative_ontology:cs_authority_grounding('5fb48b3d-0506-4993-8f22-aa5501912ad9', extraction).
narrative_ontology:cs_interpretation_layer_present('5fb48b3d-0506-4993-8f22-aa5501912ad9').
narrative_ontology:cs_reading_relation('5fb48b3d-0506-4993-8f22-aa5501912ad9', humane_treatment_standard__absolute_prohibition, forecloses).
narrative_ontology:cs_reading_relation('5fb48b3d-0506-4993-8f22-aa5501912ad9', humane_treatment_standard__contextual_necessity, influences).
narrative_ontology:cs_axiom('5fb48b3d-0506-4993-8f22-aa5501912ad9', foundational, proportionality_principle_governs_interrogation).
narrative_ontology:cs_axiom_status(proportionality_principle_governs_interrogation, holdable).
narrative_ontology:cs_axiom_grounding('5fb48b3d-0506-4993-8f22-aa5501912ad9', proportionality_principle_governs_interrogation, deontological).
narrative_ontology:cs_axiom('5fb48b3d-0506-4993-8f22-aa5501912ad9', foundational, detainee_dignity_constrained_not_absolute).
narrative_ontology:cs_axiom_status(detainee_dignity_constrained_not_absolute, overridden).
narrative_ontology:cs_axiom_grounding('5fb48b3d-0506-4993-8f22-aa5501912ad9', detainee_dignity_constrained_not_absolute, empirically_contingent).
narrative_ontology:cs_reference_frame('5fb48b3d-0506-4993-8f22-aa5501912ad9', humane_treatment_non_derogable).
narrative_ontology:cs_drift_state('5fb48b3d-0506-4993-8f22-aa5501912ad9', contemporary_security_crisis_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5fb48b3d-0506-4993-8f22-aa5501912ad9', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__proportionality_balancing, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, interrogating_state_authorities).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, security_apparatus).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, procedural_legitimacy_doctrine).
narrative_ontology:constraint_victim(humane_treatment_standard__proportionality_balancing, detained_persons).
narrative_ontology:constraint_victim(humane_treatment_standard__proportionality_balancing, detainee_protection_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, oversight_courts).
narrative_ontology:constraint_vindicates(humane_treatment_standard__proportionality_balancing, proportionality_principle_in_law).
narrative_ontology:constraint_vindicates(humane_treatment_standard__proportionality_balancing, state_security_imperative).
narrative_ontology:constraint_vindicates(humane_treatment_standard__proportionality_balancing, judicial_gatekeeping_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set interrogation protocol, determine what techniques are 'proportional' to security threats, establish baseline detention conditions. Justify techniques via case-specific security necessity. Operate within a framework claiming proportionality rather than absolute prohibition, which permits discretion on a per-detainee basis. Control the initial assessment of what techniques the threat level permits.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, interrogating_state_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Subject to interrogation techniques deemed 'proportional' to the interrogator's assessment of threat. Have no mechanism to contest the proportionality judgment in real time. Bear the physical and psychological cost of techniques permitted under the balancing standard. Exit is legally impossible; resistance through the constraint itself (refusal to speak) often triggers escalation of interrogation intensity justified as proportional to non-cooperation.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, detained_persons, payer,
    powerless, immediate, trapped, local).

% Positioned as gatekeepers and proportionality arbiters. Review interrogation cases ex post facto or through habeas petitions. Vindicate judicial authority by deciding individual cases rather than issuing categorical prohibitions. Benefit from the expanded role: proportionality framing makes every case a matter of judicial discretion, expanding the docket and the court's institutional significance. Simultaneously constrained by the framework: courts cannot declare entire classes of technique categorically impermissible without repudiating the proportionality standard itself.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, oversight_courts, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__proportionality_balancing, oversight_courts, agenda_setter).

% Argue the proportionality frame is a permission structure disguised as a limitation; they contend absolute prohibition is the only enforceable standard. Operate outside the decision loop on individual cases (excluded from proportionality assessments). Can petition courts but cannot set initial standards. Bear institutional costs when their advocacy frames are rejected by courts applying the proportionality test.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, detainee_protection_advocates, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__proportionality_balancing, detainee_protection_advocates, excluded).

% Monitor Common Article 3 compliance through treaty monitoring and fact-finding missions. Can issue reports and recommendations but have no enforcement power over national courts or interrogation authorities. Observe that proportionality assessments diverge sharply across jurisdictions and over time; report findings but cannot resolve the underlying contestation over the kernel reading.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, international_oversight_bodies, observer,
    institutional, generational, analytical, global).

% Operates under the proportionality standard as institutional policy. Has institutional interest in techniques that maximize extraction of intelligence while claiming proportional balance to potential detainees and courts. Proportionality language permits operational flexibility: specific threat assessments justify escalation. Institutional survival depends on demonstrating that interrogation yields actionable intelligence (which proportionality framing emphasizes over absolute prohibitions).
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, security_apparatus, beneficiary,
    institutional, generational, arbitrage, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(humane_treatment_standard__proportionality_balancing, interrogating_state_authorities).
narrative_ontology:fixing_cost_class(humane_treatment_standard__proportionality_balancing, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates how states may conduct interrogation while maintaining minimal humanitarian standards: identifies permissible methods via case-by-case proportionality assessment rather than categorical prohibition, enabling interrogation to proceed with claimed legitimacy while offering potential detainee protection via judicial review.
% TRANSFER_FUNCTION: Transfers authority to interrogate with techniques deemed 'proportional' from detainees (who lose autonomy and dignity) to state interrogators and courts (who gain institutional authority and discretionary decision-making power over permissible treatment levels). The transfer is framed as balanced but operates asymmetrically: authorities assess proportionality in real time with operational discretion; detainees cannot contest the assessment until after interrogation concludes.
% ABSENT_VOICES: Detainees themselves have no voice in the proportionality assessment that determines their treatment—they cannot speak until interrogation ends, and speaking during interrogation may be taken as non-cooperation justifying intensified techniques. Absolute-prohibition advocates are systematically excluded from the proportionality decision loop; their framing (no techniques are ever proportionate) is treated as non-negotiable rather than as a competing valid reading of the kernel.
% DISAPPEARANCE_RATIONALE: If the proportionality balancing standard disappeared, states would either adopt absolute prohibition (as advocates propose) or adopt unlimited discretion (as security-first authorities prefer). The proportionality framework itself—the middle path—would cease to mediate the clash. Interrogation practices would reorganize around whichever reading prevailed, courts would issue different rulings, and the institutional role of judicial gatekeeping would shrink or expand accordingly.
% FOUNDING_PROBLEM: States need to interrogate detainees in security crises and need legitimacy for detention practices; detainees need protection from torture and degrading treatment. Common Article 3 aims to solve both problems by requiring humane treatment that respects human dignity while permitting security-justified techniques.
% FOUNDING_PROBLEM_CORROBORATION: State security authorities attest the founding problem is live and that proportionality balancing solves it operationally. International humanitarian law scholars and human rights monitoring bodies attest the founding problem is real but dispute whether proportionality balancing solves it or creates a permission structure; they cite case evidence showing divergent proportionality assessments across jurisdictions and over time. Detainee testimony and medical/psychological assessment from outside the security apparatus establish that techniques classified as 'proportional' cause lasting harm, contradicting the claim that proportionality protects dignity.
narrative_ontology:disappearance_verdict(humane_treatment_standard__proportionality_balancing, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__proportionality_balancing, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__proportionality_balancing, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(humane_treatment_standard__proportionality_balancing, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__proportionality_balancing_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(humane_treatment_standard__proportionality_balancing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(humane_treatment_standard__proportionality_balancing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62) because the constraint permits techniques that absolute prohibition would forbid, shifting interrogation intensity upward along a spectrum the detainee cannot contest. Suppression is moderate (0.58) because detainees are trapped and powerless but have potential legal remedies through court review (which reduces suppression below the level of purely arbitrary authority). Theater ratio is moderate (0.41): the proportionality assessment is partly performative—it provides legitimacy narrative for interrogators—but courts do engage substantive review, making some proportion of the constraint's operation genuinely functional. Accessibility collapse is moderate-high (0.68): detainees have no practical alternative to detention and interrogation; once detained, they cannot exit the constraint. Resistance is high (0.72): advocates, international bodies, and some courts actively resist the proportionality reading, pushing toward absolute prohibition. The measurement trajectory shows extractiveness rising steeply in the first half of the interval (security crises drive technique intensification) then plateauing as court precedent stabilizes (t=10 to t=25), indicating institutional lock-in. Theater ratio rises alongside, suggesting courts increasingly play a confirmatory role rather than substantive gate. Suppression requirement rises slightly but remains stable, indicating the constraint requires consistent active enforcement but does not require escalating suppression intensity. All measurements share one time grid (the interval [0, 25]) so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The proportionality standard creates profound seat divergence. From the interrogating authority's seat, proportionality is a constraint: techniques must be justified by threat level, interrogation is regulated, and courts can reject disproportionate cases. From the detained person's seat, proportionality is a permission: whatever the interrogator assesses as proportional is permitted; the detainee has no way to contest the assessment in real time; courts review after harm is done. From the court's seat, proportionality is institutional authority: the court becomes the final arbiter, its docket expands, and its role shifts from enforcer of prohibition to case-by-case gatekeeper. From the advocate's seat, proportionality is a betrayal: the text is read to permit what it should prohibit, and the advocate cannot access the decision loop to contest it. The engine computes these divergences from the structural data: different power atoms, different exit options, different role relationships to the constraint. The divergence is the measurement the system is designed to detect.
 *
 * DIRECTIONALITY LOGIC:
 *   Interrogating state authorities have d near 0.0 (beneficiary): they gain discretion, operational flexibility, and institutional legitimacy from the proportionality standard. Courts have d near 0.15–0.25 (beneficiary with minor cost): they gain authority and jurisdiction but are constrained by the need to appear neutral and proportionate. Detained persons have d near 0.95 (target): they bear the cost of techniques permitted under proportionality, have no legal standing in real-time assessment, and cannot exit. Detainee protection advocates have d near 0.80 (target): they pay institutional cost when courts reject their framing and are excluded from the decision loop. Security apparatus has d near 0.0 (beneficiary): the constraint permits operational flexibility justified by threat assessment. International observers have d near 0.5 (symmetric): they observe and report but have no power to enforce or change the constraint. The beneficiary set (interrogators, courts, security apparatus) is concentrated and captures the constraint's gains; the victim set (detained persons, advocates) is diffuse and bears costs without control. This asymmetry is the hallmark of tangled_rope: genuine coordination (legitimate interrogation framework) plus asymmetric extraction (permits detainee harm that advocates cannot prevent).
 *
 * MANDATROPHY ANALYSIS:
 *   The proportionality standard addresses a real founding problem: states need to interrogate detainees and need legitimacy for detention practices; detainees need protection from torture. But the problem statement splits into three incompatible versions depending on which reading of Common Article 3 one adopts. The absolute-prohibition reading says: 'The founding problem is solved by categorical prohibition—no techniques ever.' The contextual-necessity reading says: 'The founding problem is solved by letting states override humanitarian protections when security demands it.' The proportionality reading (this one) says: 'The founding problem is solved by case-by-case balancing—neither absolute nor unlimited.' These are genuinely different solutions to genuinely different problem framings. The proportionality reading is not mandatrophied (the mandate has not outlived its function); it is actively contested. Courts have invested institutional authority in this reading; if they abandoned it for absolute prohibition, they would lose case-by-case authority and become administrators of a bright-line rule. Advocates argue the proportionality reading is a mandate that HAS outlived its function—that the 'balance' it claims to strike has repeatedly favored interrogators, not detainees, and that courts have become tools of the interrogation system rather than detainee protectors. The mismatch between founding problem (stated as coordination) and current operation (observed as extraction with legitimacy theater) is captured in the omegas.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_assessment_divergence,
    'Is the proportionality standard empirically determinate—do courts and interrogators converge on what techniques are proportional to what threat levels—or is ''proportional'' a semantic cover for divergent judgments?',
    'Comparative analysis of proportionality verdicts across jurisdictions and interrogators for identical fact patterns; if convergence is absent, the standard is indeterminate and functions as discretion.',
    'If indeterminate, the constraint is snare-like (discretion masked as standard); if determinate, it is genuinely tangled_rope (coordination plus extraction both real). The divergence determines whether the constraint stabilizes via judicial legitimacy or via institutional power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_assessment_divergence, empirical, 'Whether proportionality judgments converge or diverge across contexts and actors.').

omega_variable(
    reading_contestation_in_authority_structure,
    'Does the proportionality reading exist because courts have institutionally evolved to prefer case-by-case discretion (and thus prefer this reading over absolute prohibition), or does it reflect a genuine epistemic claim about Common Article 3''s correct interpretation?',
    'Historical investigation of court evolution and doctrine shift; comparison with non-judicial commentary on the same text; analysis of whether courts adopted proportionality before or after it became institutionally convenient.',
    'If institutional capture is the driver, the proportionality reading is extraction riding a coordination problem; if epistemic, it may be genuine. This determines whether the constraint''s persistence depends on institutional power or legitimate interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contestation_in_authority_structure, conceptual, 'Whether proportionality reading reflects judicial evolution or authority-structure preference.').

omega_variable(
    detainee_suppression_mechanism,
    'Is detainee suppression (powerlessness to resist or contest the proportionality assessment) structural (lack of legal standing, communication barriers, power asymmetry) or internalized (detainees believe they deserve interrogation and accept proportionality judgments as legitimate)?',
    'Post-release detainee testimony and psychological assessment; comparison of compliance with interrogation across regimes with explicit judicial proportionality oversight versus those with opaque authority assessments.',
    'If structural, the suppression is a measurable feature of the constraint; if internalized, the suppression persists after release and is higher than measured. This affects the classification''s stability: internalized suppression makes the constraint self-maintaining, reducing dependence on active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(detainee_suppression_mechanism, empirical, 'Whether detainee suppression is structural, internalized, or mixed.').

omega_variable(
    kernel_reading_as_false_legitimacy,
    'Is the proportionality reading a genuine alternative interpretation of Common Article 3''s text, or is it a false summit—a constraint that benefits state interrogators and courts but claims foundation in a neutral principle?',
    'Textual analysis of Common Article 3 by independent scholars with no stake in interrogation policy; empirical comparison of harm outcomes under proportionality versus absolute-prohibition regimes; assessment of whether proportionality doctrine has changed to accommodate practice or practice has changed to match doctrine.',
    'If false summit, the constraint should reclassify to snare at the victim seat; if genuine reading, it remains tangled_rope. The FSM (false-summit-mountain) signature does not apply here (claimed_type is tangled_rope, not mountain), but the conceptual dynamic is parallel: a reading that claims neutrality but systematically benefits interrogators.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_as_false_legitimacy, conceptual, 'Whether proportionality is a defensible reading or a false legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__proportionality_balancing, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, humane_treatment_standard__proportionality_balancing, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(huma_tr_t0, observed).
narrative_ontology:measurement(huma_tr_t5, humane_treatment_standard__proportionality_balancing, theater_ratio, 5, 0.33).
narrative_ontology:measurement_basis(huma_tr_t5, observed).
narrative_ontology:measurement(huma_tr_t10, humane_treatment_standard__proportionality_balancing, theater_ratio, 10, 0.37).
narrative_ontology:measurement_basis(huma_tr_t10, observed).
narrative_ontology:measurement(huma_tr_t15, humane_treatment_standard__proportionality_balancing, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(huma_tr_t15, observed).
narrative_ontology:measurement(huma_tr_t20, humane_treatment_standard__proportionality_balancing, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(huma_tr_t20, observed).
narrative_ontology:measurement(huma_tr_t25, humane_treatment_standard__proportionality_balancing, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(huma_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, humane_treatment_standard__proportionality_balancing, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(huma_be_t0, observed).
narrative_ontology:measurement(huma_be_t5, humane_treatment_standard__proportionality_balancing, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(huma_be_t5, observed).
narrative_ontology:measurement(huma_be_t10, humane_treatment_standard__proportionality_balancing, base_extractiveness, 10, 0.59).
narrative_ontology:measurement_basis(huma_be_t10, observed).
narrative_ontology:measurement(huma_be_t15, humane_treatment_standard__proportionality_balancing, base_extractiveness, 15, 0.61).
narrative_ontology:measurement_basis(huma_be_t15, observed).
narrative_ontology:measurement(huma_be_t20, humane_treatment_standard__proportionality_balancing, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(huma_be_t20, observed).
narrative_ontology:measurement(huma_be_t25, humane_treatment_standard__proportionality_balancing, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(huma_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, humane_treatment_standard__proportionality_balancing, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(huma_su_t0, observed).
narrative_ontology:measurement(huma_su_t5, humane_treatment_standard__proportionality_balancing, suppression_requirement, 5, 0.54).
narrative_ontology:measurement_basis(huma_su_t5, observed).
narrative_ontology:measurement(huma_su_t10, humane_treatment_standard__proportionality_balancing, suppression_requirement, 10, 0.56).
narrative_ontology:measurement_basis(huma_su_t10, observed).
narrative_ontology:measurement(huma_su_t15, humane_treatment_standard__proportionality_balancing, suppression_requirement, 15, 0.58).
narrative_ontology:measurement_basis(huma_su_t15, observed).
narrative_ontology:measurement(huma_su_t20, humane_treatment_standard__proportionality_balancing, suppression_requirement, 20, 0.58).
narrative_ontology:measurement_basis(huma_su_t20, observed).
narrative_ontology:measurement(huma_su_t25, humane_treatment_standard__proportionality_balancing, suppression_requirement, 25, 0.58).
narrative_ontology:measurement_basis(huma_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__proportionality_balancing, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(humane_treatment_standard__proportionality_balancing, 0.18).
narrative_ontology:affects_constraint(humane_treatment_standard__proportionality_balancing, humane_treatment_standard__absolute_prohibition).
narrative_ontology:affects_constraint(humane_treatment_standard__proportionality_balancing, humane_treatment_standard__contextual_necessity).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the humane_treatment_standard kernel (Common Article 3). The absolute_prohibition reading interprets the text as establishing non-derogable standards. The contextual_necessity reading permits security-driven exceptions. The proportionality_balancing reading (this one) establishes a middle path where courts decide individual cases. All three stories share a kernel but diverge in ε (proportion of interrogation that is extractive), beneficiary/victim structure (who benefits from that reading), and type classification. The proportionality reading permits more interrogation than absolute prohibition but less than contextual necessity, with extraction concentrated on powerless detainees and diffuse benefits to institutional authorities and courts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(humane_treatment_standard__proportionality_balancing, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
