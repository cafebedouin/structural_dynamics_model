% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__categorical_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_speech_protection__categorical_balancing_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: first_amendment_speech_protection__categorical_balancing_reading
 *   human_readable: First Amendment Categorical Balancing (Judicial Case-by-Case Reading)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the categorical balancing reading of
 *   the First Amendment kernel. The reading treats the First Amendment as
 *   creating a judicially-administered boundary between protected and
 *   unprotected speech categories, drawn via case-by-case balancing of speech
 *   value against demonstrable harm. The beneficiary of this reading is the
 *   institutional judiciary, which gains authority to continuously redefine
 *   the protected set through doctrine. The victims are: (1) speakers whose
 *   expression falls within unprotected categories, who receive no
 *   constitutional shield regardless of the actual harm caused by their
 *   specific utterances; (2) legal predictability itself, which is sacrificed
 *   to preserve judicial interpretive flexibility. The constraint is CLAIMED
 *   as tangled_rope (coordination via balancing function + asymmetric
 *   extraction via judicial authority maintenance) while the authored metrics
 *   describe moderate extractiveness and significant suppression—the engine
 *   computes per-seat divergence from this structural data.
 *
 * KEY AGENTS:
 *   - federal_judiciary: institutional agenda-setter maintaining authority over speech boundaries via continuous reinterpretation
 *   - speakers_in_protected_categories: organized beneficiaries receiving predictable (if negotiated) protection
 *   - categories_deemed_unprotected: powerless victims trapped in categorical exclusions
 *   - state_legislatures: excluded parties constrained to operate within judicially-set zones
 *   - minority_advocacy_groups: constrained payers occupying borderline categories with unpredictable protection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__categorical_balancing_reading, 0.68).
domain_priors:suppression_score(first_amendment_speech_protection__categorical_balancing_reading, 0.62).
domain_priors:theater_ratio(first_amendment_speech_protection__categorical_balancing_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__categorical_balancing_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__categorical_balancing_reading, "First Amendment Categorical Balancing (Judicial Case-by-Case Reading)").
narrative_ontology:topic_domain(first_amendment_speech_protection__categorical_balancing_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__categorical_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__categorical_balancing_reading, '9d3ce4b8-4fdd-48d7-a9a9-7155b7b32a21').
narrative_ontology:cs_kernel_codification('9d3ce4b8-4fdd-48d7-a9a9-7155b7b32a21', fixed_text).
narrative_ontology:cs_authority_grounding('9d3ce4b8-4fdd-48d7-a9a9-7155b7b32a21', extraction).
narrative_ontology:cs_interpretation_layer_present('9d3ce4b8-4fdd-48d7-a9a9-7155b7b32a21').
narrative_ontology:cs_reading_relation('9d3ce4b8-4fdd-48d7-a9a9-7155b7b32a21', first_amendment_speech_protection__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('9d3ce4b8-4fdd-48d7-a9a9-7155b7b32a21', first_amendment_speech_protection__harm_limited_reading, influences).
narrative_ontology:cs_axiom('9d3ce4b8-4fdd-48d7-a9a9-7155b7b32a21', foundational, judicial_categories_define_protection).
narrative_ontology:cs_axiom_status(judicial_categories_define_protection, holdable).
narrative_ontology:cs_axiom_grounding('9d3ce4b8-4fdd-48d7-a9a9-7155b7b32a21', judicial_categories_define_protection, deontological).
narrative_ontology:cs_axiom('9d3ce4b8-4fdd-48d7-a9a9-7155b7b32a21', foundational, speech_value_harm_balancing_legitimate).
narrative_ontology:cs_axiom_status(speech_value_harm_balancing_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('9d3ce4b8-4fdd-48d7-a9a9-7155b7b32a21', speech_value_harm_balancing_legitimate, conventional).
narrative_ontology:cs_reference_frame('9d3ce4b8-4fdd-48d7-a9a9-7155b7b32a21', judicial_superintendence_of_speech_boundaries).
narrative_ontology:cs_drift_state('9d3ce4b8-4fdd-48d7-a9a9-7155b7b32a21', contemporary_political_polarization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9d3ce4b8-4fdd-48d7-a9a9-7155b7b32a21', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, federal_judiciary).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, categories_deemed_unprotected).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, legal_predictability).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, speakers_in_protected_categories).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, minority_advocacy_groups).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__categorical_balancing_reading, judicial_superintendence_of_speech_boundaries).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__categorical_balancing_reading, balancing_test_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the First Amendment via case-by-case balancing, adjudicating which speech categories merit protection and which do not. Sets the boundaries of 'obscenity,' 'incitement,' 'true threats,' 'fighting words' through doctrine. Each ruling expands or contracts the protected set. Maintains authority over the meaning of the amendment through continuous reinterpretation.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Receive protection from state suppression provided they stay within judicially-recognized protected categories (political speech, artistic expression, commercial speech where permitted). Benefit from predictable, if negotiated, boundaries. Cost: must constantly monitor judicial reinterpretation; cannot speak in ways the courts have not yet certified as protected.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, speakers_in_protected_categories, beneficiary,
    organized, biographical, mobile, national).

% Speech within categories the judiciary has deemed unprotected (obscenity, incitement, true threats, fighting words) receives no First Amendment shield. Speakers in these categories bear the cost of the categorical regime: their speech can be criminalized or suppressed without constitutional constraint. The trapped position stems from the category definition, not individual choice—a speaker cannot 'exit' unprotected speech by personal effort if their expression falls within the category.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, categories_deemed_unprotected, payer,
    powerless, immediate, trapped, national).

% The balancing-test approach trades certainty for flexibility: each new case can redefine the boundaries of protected speech. This preserves judicial interpretive authority but imposes costs on parties trying to comply with speech law ex ante. Rules are not fixed; outcomes depend on judicial judgment about proportionality and harm.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, legal_predictability, payer,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(first_amendment_speech_protection__categorical_balancing_reading, legal_predictability).

% Cannot freely regulate speech; must operate within the judicially-set boundaries of protected and unprotected categories. Can legislate only in unprotected zones. Would prefer fixed, negotiated boundaries or the power to redefine them; excluded from the conversation about where the lines are drawn.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, state_legislatures, excluded,
    organized, generational, constrained, national).

% Often occupy borderline categories where balancing tests produce unpredictable protection. Speech that challenges dominant norms may be characterized as incitement, fighting words, or true threats by state actors, leaving the judiciary to certify protection after suppression has occurred. Their exit is constrained: they cannot know ex ante whether their speech will be deemed protected.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, minority_advocacy_groups, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(first_amendment_speech_protection__categorical_balancing_reading, minority_advocacy_groups, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(first_amendment_speech_protection__categorical_balancing_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(first_amendment_speech_protection__categorical_balancing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes stable doctrinal categories (obscenity, incitement, true threats) that partition speech into protected and unprotected zones, creating a known boundary that speakers and state actors can reference. Solves the coordination problem: without categorical law, every speech act would trigger individualized judicial review, paralyzing both speakers and regulators.
% TRANSFER_FUNCTION: Transfers authority to define the protected/unprotected boundary from legislatures and communities to the federal judiciary. Moves predictability (from fixed rules) into discretionary balancing (case-by-case adjudication). Speakers conform to judicially-set categories in exchange for constitutional protection; states conform to judicial doctrine in exchange for having some regulatory space (unprotected categories).
% ABSENT_VOICES: The speakers whose expression falls within unprotected categories have no seat at the table—their interests are adjudicated after suppression by parties (state actors, victims of speech-caused harm) who have incentive to keep categories narrow. Communities claiming injury from speech in 'protected' categories (e.g., hate speech communities) are structurally excluded from the balancing: they are not the recognized harm vector in First Amendment doctrine.
% DISAPPEARANCE_RATIONALE: If the categorical balancing regime vanished, speech regulation would either revert to legislative rule-making (states independently criminalize speech categories, with no uniform constitutional ceiling—a fragmented landscape) or shift to absolutist protection (no judicially-recognized unprotected categories—a different constraint entirely). The current equilibrium would dissolve.
% FOUNDING_PROBLEM: Determined that the First Amendment, though written absolutely ('no law'), must have boundaries: some speech (incitement to imminent lawless action, true threats of violence) cannot be protected without enabling direct harm. The balancing test emerged as a judicial solution to define where the boundaries are, moving from blanket categorical exclusions (sedition, libel) toward a more speech-protective stance grounded in harm assessment.
% FOUNDING_PROBLEM_CORROBORATION: The judiciary attests the problem is live: speech can incite violence, threaten individuals, incur measurable harm. Civil rights advocates and scholars outside the judiciary attest that the founding problem (protecting speakers from state censorship) persists and that the categorical balancing approach has been underprotective: marginalized groups experience the unprotected categories as cover for suppression. Absolutist scholars argue the problem is solved by the text ('no law') and balancing is mission creep.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__categorical_balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__categorical_balancing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__categorical_balancing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(first_amendment_speech_protection__categorical_balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__categorical_balancing_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__categorical_balancing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__categorical_balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(first_amendment_speech_protection__categorical_balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures 0.68 because the categorical balancing regime concentrates speech-boundary-setting authority in the judiciary, which uses this authority to narrow protected zones over time (a weak trend visible in measurements 0–60, then flattening). Suppression measures 0.62 because speakers in borderline categories face active suppression (state enforcement in unprotected zones) that can only be overturned if the judiciary later certifies protection—the burden of proof is on the speaker to convince judges their speech should move from unprotected to protected. Theater measures 0.41 (moderate) because the balancing test is partially performative: the rhetoric of 'balancing' speech value against harm suggests careful calibration, but the actual outcome is that the judiciary maintains interpretive authority regardless of how the balance is struck. The unprotected categories (obscenity, incitement, true threats) remain stable in name but drift in scope as doctrine evolves. Accessibility_collapse measures 0.58 because alternatives to the categorical balancing regime exist (absolutism, harm-limited approaches, legislative rule-making) but are institutionally suppressed by the judiciary's control of the authoritative reading. Resistance measures 0.72 because speakers, civil rights advocates, and absolutist scholars actively contest the balancing approach—but the judiciary retains power despite this resistance. The temporal measurements show extractiveness rising from t0 to t60 (judicial authority consolidating over decades) then flattening (the regime reaches an equilibrium), while theater rises slightly throughout (performative maintenance becoming more salient as the underlying coordination function stabilizes).
 *
 * PERSPECTIVAL GAP:
 *   The federal judiciary and speakers in protected categories should compute near the beneficiary end (d ≈ 0.1–0.3): the regime offers them stable, predictable boundaries and constitutional authority. Speakers in unprotected categories compute near the target end (d ≈ 0.8–1.0): they are trapped by the categorical regime and receive no protection regardless of the actual harm their speech causes. State legislatures compute near the middle-target end (d ≈ 0.6–0.7): they have some regulatory space in unprotected zones but are constrained by the judicially-set boundaries and cannot independently redefine them. This divergence reflects the structural asymmetry: the regime is built on an agreement between the judiciary and the protected-speech beneficiaries at the expense of unprotected-speech payers.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal judiciary is the primary beneficiary (maintains interpretive authority over speech boundaries, collects the 'rent' of public deference to judicial interpretations). Speakers in protected categories are secondary beneficiaries (receive predictable protection, though the protection depends on staying within judicially-recognized boundaries). Speakers in unprotected categories and legal predictability are the primary victims: speakers are trapped in categories the judiciary has defined, and legal predictability is sacrificed to preserve the judiciary's flexibility to rebalance. State legislatures and minority advocacy groups are excluded rather than directly victimized, but their exclusion serves the judicial beneficiary. Exit options differentiate the seats: the judiciary has analytical exit (can always reinterpret); speakers in protected categories have mobile exit (can shift to different kinds of speech if their current speech is threatened); speakers in unprotected categories have trapped exit (no individual choice can move them from an unprotected category); legal predictability has no exit (it is not an agent). This distribution of exit_options feeds the directionality computation: trapped victims anchor the target end, mobile beneficiaries sit at the beneficiary end.
 *
 * MANDATROPHY ANALYSIS:
 *   The categorical balancing regime solves a genuine coordination problem (defining speech boundaries ex ante so speakers and regulators can plan) but that solution has been partially captured by the judiciary as a mechanism to preserve its own authority. The mandatrophy question: has the founding problem (the need to protect speech from state censorship without allowing speech that directly causes harm) been solved, or has it been substituted with a different problem (the need to preserve judicial control over speech boundaries)? The measurement series suggests the regime has plateaued: extractiveness rises to t60, then flattens at t75, suggesting the judicial authority has consolidated and the regime has reached equilibrium. Theater rises throughout, indicating that over time more of the regime's activity is performative maintenance (rhetoric of balancing) rather than functional problem-solving (actual calibration of speech protection). The mandatrophy-resolved flag should be set to true if this reading acknowledges that the founding problem (state censorship of dissent) has been substantially solved and the regime now primarily extracts judicial authority rather than protects speech. However, this reading does NOT acknowledge that: from the categorical balancing perspective, the founding problem remains live (speech can still cause harm; unprotected categories must remain narrow but must exist). The engine may compute mandatrophy divergence, but this reading authors it as unresolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_boundary_drift,
    'Do the judicially-defined categories (obscenity, incitement, true threats) have stable meanings across time and contexts, or do they drift in scope as doctrine evolves?',
    'Longitudinal analysis of Supreme Court opinions defining each category across decades, coding for doctrinal expansion/contraction. Comparison of the scope of ''incitement'' in Brandenburg (1969) vs. contemporary sedition cases; same for ''obscenity'' under Miller (1973) and later applications.',
    'If boundaries drift significantly, the constraint operates less as stable categorical law and more as discretionary balancing disguised in categorical language (theater_ratio should rise and accessibility_collapse should fall). If boundaries are stable, the categorical framing captures the actual operation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(categorical_boundary_drift, empirical, 'Whether categorical boundaries are stable or drift over time').

omega_variable(
    trapped_exit_mechanism,
    'For speakers whose expression falls within an unprotected category, what is the actual mechanism by which they might exit the category (move their speech to protected territory)? Is exit genuinely trapped, or are there paths speakers can take to reframe their speech as protected?',
    'Case studies of speakers who attempted to reframe banned speech as protected (e.g., incitement vs. political advocacy; obscenity vs. artistic expression). Analysis of success rates and the costs speakers bear in reframing attempts.',
    'If speakers have workable reframing paths, exit_options should be ''constrained'' rather than ''trapped,'' d should shift downward for this seat, and effective extraction should be lower. If reframing is blocked by the categorical definition itself (a speaker cannot frame unprotected speech as protected without changing the substance of their message), then trapped is correct and extraction remains high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trapped_exit_mechanism, empirical, 'Whether trapped exit option is structurally accurate or overstates constraint').

omega_variable(
    judicial_authority_preservation_motive,
    'Does the judiciary maintain the categorical balancing regime primarily because it solves a genuine coordination problem (defining speech boundaries ex ante), or primarily because it preserves judicial authority to redefine boundaries over time?',
    'Analysis of judicial opinions: do they emphasize coordination/predictability benefits (forward-looking) or judicial flexibility/superintendence benefits (backward-looking)? Comparison of judicial rhetoric over time as the regime matures.',
    'If the primary motive is coordination, the regime is less extractive than authored (beneficiary=all speakers, extractiveness should drop). If the primary motive is authority preservation, the regime is as extractive as authored (beneficiary=judiciary, extractiveness stands). This is located in the axis between mandatrophy-resolved (problem solved) and mandatrophy-live (problem persists in new form).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_authority_preservation_motive, conceptual, 'Whether the regime is primarily coordination or primarily authority preservation').

omega_variable(
    reading_vs_absolutist_foreclosure_test,
    'Does the categorical balancing reading logically foreclose the absolutist reading within a single judicial framework, or can both readings coexist as live options?',
    'Theoretical analysis: can a court hold that (1) the First Amendment provides categorical protection except for judicially-recognized unprotected categories, AND (2) the First Amendment text (''no law'') means what it says absolutely? If the court must choose, they foreclose; if both can hold simultaneously, coexistence is correct.',
    'If foreclosure is correct, reading_relations.relation should be ''forecloses'' and the engine will compute type divergence between readings. If coexistence is correct, relation should be ''coexists_with'' and readings are fully live alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_absolutist_foreclosure_test, conceptual, 'Logical foreclosure vs. coexistence between categorical balancing and absolutist readings').

omega_variable(
    harm_limitation_vs_categorical_balancing_boundary,
    'What is the structural relationship between the categorical balancing reading and the harm_limited_reading? Does categorical balancing foreclose, coexist with, or influence harm-limitation?',
    'Theoretical analysis: categorical balancing defines unprotected categories WITHOUT requiring proof of actual harm in the individual case (the category itself justifies suppression). Harm-limitation requires proof of actual harm. These approaches produce different case outcomes when a speaker in an unprotected category causes no actual harm: categorical balancing would allow suppression; harm-limitation would not. Can both approaches hold in one framework? Probably not—they foreclose on actual case outcomes.',
    'If foreclosure is correct, reading_relations.relation to harm_limited_reading should be ''forecloses.'' If the readings merely influence each other (harm-limitation creates pressure to make unprotected categories narrower), relation should be ''influences.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_limitation_vs_categorical_balancing_boundary, conceptual, 'Structural relationship between categorical balancing and harm-limitation approaches').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__categorical_balancing_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t0, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(firs_tr_t0, observed).
narrative_ontology:measurement(firs_tr_t10, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement_basis(firs_tr_t10, observed).
narrative_ontology:measurement(firs_tr_t20, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement_basis(firs_tr_t20, observed).
narrative_ontology:measurement(firs_tr_t30, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 30, 0.36).
narrative_ontology:measurement_basis(firs_tr_t30, observed).
narrative_ontology:measurement(firs_tr_t45, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 45, 0.39).
narrative_ontology:measurement_basis(firs_tr_t45, observed).
narrative_ontology:measurement(firs_tr_t60, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement_basis(firs_tr_t60, observed).
narrative_ontology:measurement(firs_tr_t75, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 75, 0.41).
narrative_ontology:measurement_basis(firs_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(firs_be_t0, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 0, 0.41).
narrative_ontology:measurement_basis(firs_be_t0, observed).
narrative_ontology:measurement(firs_be_t10, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(firs_be_t10, observed).
narrative_ontology:measurement(firs_be_t20, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement_basis(firs_be_t20, observed).
narrative_ontology:measurement(firs_be_t30, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 30, 0.61).
narrative_ontology:measurement_basis(firs_be_t30, observed).
narrative_ontology:measurement(firs_be_t45, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 45, 0.66).
narrative_ontology:measurement_basis(firs_be_t45, observed).
narrative_ontology:measurement(firs_be_t60, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(firs_be_t60, observed).
narrative_ontology:measurement(firs_be_t75, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 75, 0.68).
narrative_ontology:measurement_basis(firs_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t0, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(firs_su_t0, observed).
narrative_ontology:measurement(firs_su_t10, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement_basis(firs_su_t10, observed).
narrative_ontology:measurement(firs_su_t20, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement_basis(firs_su_t20, observed).
narrative_ontology:measurement(firs_su_t30, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 30, 0.59).
narrative_ontology:measurement_basis(firs_su_t30, observed).
narrative_ontology:measurement(firs_su_t45, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 45, 0.61).
narrative_ontology:measurement_basis(firs_su_t45, observed).
narrative_ontology:measurement(firs_su_t60, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement_basis(firs_su_t60, observed).
narrative_ontology:measurement(firs_su_t75, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 75, 0.62).
narrative_ontology:measurement_basis(firs_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__categorical_balancing_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(first_amendment_speech_protection__categorical_balancing_reading, 0.12).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection__absolutist_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection__harm_limited_reading).

% DUAL FORMULATION NOTE:
% The first_amendment_speech_protection kernel decomposes into three structurally distinct constraint stories: (1) absolutist_reading: text-based protection, near-zero extraction, mountain candidate. (2) categorical_balancing_reading (this story): judicially-administered categories, moderate-high extraction, tangled_rope. (3) harm_limited_reading: causation-based protection, harm-focused extraction. The three readings are sibling interpretations of the same constitutional text; they are not different framings of one constraint but genuinely different constraints with different ε values, different beneficiaries, and different victim structures. Each reading grounds itself in a different interpretation of what the First Amendment IS—what harm it prevents, what authority enforces it, what the protected set includes. The readings coexist institutionally (different judges, different eras, different jurisdictions adopt different readings) and logically constrain each other (a court that adopts categorical balancing has forecloses absolutism within its own framework for that specific case).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
