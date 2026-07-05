% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__categorical_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: First Amendment as Judicially-Balanced Category System
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the categorical_balancing_reading of the First
 *   Amendment kernel: the doctrine that protected speech is defined not by
 *   the text's absolute language nor by a fixed harm threshold, but by a
 *   growing, judicially-maintained taxonomy of categories (obscenity,
 *   incitement, true threats, fighting words, defamation) each requiring its
 *   own multi-factor balancing test weighed case by case against asserted
 *   harms. This is a distinct constraint from the absolutist_reading (which
 *   treats 'no law' as categorical except for narrow historical carve-outs
 *   fixed at the founding) and the harm_limited_reading (which ties
 *   protection loss to demonstrable unconsented harm rather than
 *   judicially-defined category membership). The three readings have
 *   different beneficiary structures, different victim sets, and different
 *   epsilon values — they are not the same constraint viewed three ways; they
 *   are three constraints sharing a contested kernel (the First Amendment
 *   text and its interpretive tradition).
 *
 * KEY AGENTS:
 *   - federal_judiciary: institutional agenda-setter maintaining interpretive control over category boundaries
 *   - appellate_bar_specialists and constitutional_law_academy: organized beneficiaries of doctrinal complexity
 *   - speakers_in_disfavored_categories, low_resource_litigants, minority_political_movements: powerless payers bearing unpredictability and asymmetric application
 *   - state_and_local_prosecutors: institutional beneficiaries exploiting balancing discretion at the charging stage
 *   - civil_liberties_organizations: excluded organized voice arguing for the rival absolutist reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__categorical_balancing_reading, 0.58).
domain_priors:suppression_score(first_amendment_speech_protection__categorical_balancing_reading, 0.52).
domain_priors:theater_ratio(first_amendment_speech_protection__categorical_balancing_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__categorical_balancing_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__categorical_balancing_reading, "First Amendment as Judicially-Balanced Category System").
narrative_ontology:topic_domain(first_amendment_speech_protection__categorical_balancing_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__categorical_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__categorical_balancing_reading, '67ea7963-66bf-4082-84d2-3d8f9562c9d5').
narrative_ontology:cs_kernel_codification('67ea7963-66bf-4082-84d2-3d8f9562c9d5', fixed_text).
narrative_ontology:cs_authority_grounding('67ea7963-66bf-4082-84d2-3d8f9562c9d5', lineage).
narrative_ontology:cs_interpretation_layer_present('67ea7963-66bf-4082-84d2-3d8f9562c9d5').
narrative_ontology:cs_reading_relation('67ea7963-66bf-4082-84d2-3d8f9562c9d5', first_amendment_speech_protection__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('67ea7963-66bf-4082-84d2-3d8f9562c9d5', first_amendment_speech_protection__harm_limited_reading, coexists_with).
narrative_ontology:cs_axiom('67ea7963-66bf-4082-84d2-3d8f9562c9d5', foundational, judicial_category_taxonomy_is_legitimate_interpretive_method).
narrative_ontology:cs_axiom_status(judicial_category_taxonomy_is_legitimate_interpretive_method, holdable).
narrative_ontology:cs_axiom_grounding('67ea7963-66bf-4082-84d2-3d8f9562c9d5', judicial_category_taxonomy_is_legitimate_interpretive_method, conventional).
narrative_ontology:cs_axiom('67ea7963-66bf-4082-84d2-3d8f9562c9d5', foundational, speech_value_is_commensurable_with_harm_for_balancing_purposes).
narrative_ontology:cs_axiom_status(speech_value_is_commensurable_with_harm_for_balancing_purposes, holdable).
narrative_ontology:cs_axiom_grounding('67ea7963-66bf-4082-84d2-3d8f9562c9d5', speech_value_is_commensurable_with_harm_for_balancing_purposes, instrumental).
narrative_ontology:cs_reference_frame('67ea7963-66bf-4082-84d2-3d8f9562c9d5', case_by_case_categorical_balancing_doctrine).
narrative_ontology:cs_drift_state('67ea7963-66bf-4082-84d2-3d8f9562c9d5', post_brandenburg_post_miller_contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('67ea7963-66bf-4082-84d2-3d8f9562c9d5', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, appellate_bar_specialists).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, constitutional_law_academy).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, speakers_in_disfavored_categories).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, low_resource_litigants).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, minority_political_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, state_and_local_prosecutors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and redefines the boundaries of categories like obscenity, incitement, true threats, and fighting words through case-by-case balancing tests (Brandenburg imminence, Miller obscenity prongs, Virginia v. Black cross-burning intent). Each new case is an opportunity to expand or contract the protected set. The judiciary's interpretive discretion is the mechanism itself — no legislature or executive can finally settle what speech is protected without the courts weighing in category by category.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% Build careers litigating the boundary lines of the balancing tests — media law firms, First Amendment clinics, and specialized appellate counsel derive steady income and prestige from the doctrine's permanent unsettledness. A bright-line rule would eliminate much of this practice area's billable complexity.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, appellate_bar_specialists, beneficiary,
    organized, biographical, arbitrage, national).

% Generates scholarship, doctrine commentary, and casebook material from the perpetual contestability of category boundaries. Faculty reputations and tenure cases are built on proposing new balancing frameworks or critiquing existing ones. A settled categorical or harm-based rule would shrink this generative terrain.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, constitutional_law_academy, beneficiary,
    organized, generational, mobile, national).

% Individuals whose speech falls near a contested category boundary — protest organizers accused of incitement, artists accused of obscenity, activists accused of making threats — cannot know in advance whether their speech is protected until a court balances value against harm after the fact, often after arrest, prosecution, or civil liability has already attached. The chilling effect is borne immediately and individually.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, speakers_in_disfavored_categories, payer,
    powerless, immediate, trapped, local).

% Lack the resources to litigate a balancing-test case through multiple appellate levels to establish that their speech falls on the protected side of a line. Well-resourced speakers (major media companies, well-funded advocacy organizations) can afford the years-long process; individuals and small organizations often settle, plead, or self-censor rather than bear the cost of vindicating a contested classification.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, low_resource_litigants, payer,
    powerless, biographical, trapped, national).

% Historically, the balancing framework's incitement and 'fighting words' categories have been applied asymmetrically against speech from minority political and labor movements (civil rights protest speech, labor organizing rhetoric) more readily than against majoritarian speech carrying comparable rhetorical intensity, because the balancing test's harm assessment is itself value-laden and applied by judges drawn disproportionately from majority backgrounds.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, minority_political_movements, payer,
    powerless, generational, constrained, national).

% As a systemic good, predictability itself is degraded by the balancing approach: no citizen, publisher, or platform can determine ex ante with confidence whether contemplated speech falls inside or outside protection, because the test is inherently retrospective and fact-sensitive rather than rule-bound.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, legal_predictability, payer,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(first_amendment_speech_protection__categorical_balancing_reading, legal_predictability).

% Exercise discretion to charge speech-adjacent conduct (threats, incitement, obscenity) knowing the balancing framework gives them latitude to argue for classification as unprotected in borderline cases, extracting plea deals or settlements from defendants who cannot afford to litigate the classification question to a favorable appellate resolution.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, state_and_local_prosecutors, beneficiary,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(first_amendment_speech_protection__categorical_balancing_reading, state_and_local_prosecutors, agenda_setter).

% Argue in briefs and public advocacy for bright-line categorical protection (the absolutist reading) precisely because the balancing framework's unpredictability chills speech before any court ever reaches the merits; their institutional voice is heard in litigation but the balancing framework itself, once entrenched as doctrine, is not put to a vote — it persists through incremental case law that no single organized advocacy effort can overturn wholesale.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, civil_liberties_organizations, excluded,
    organized, civilizational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(first_amendment_speech_protection__categorical_balancing_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(first_amendment_speech_protection__categorical_balancing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for adjudicating genuinely hard cases where speech interacts with concrete harms (true threats, incitement to imminent violence, obscenity as legally defined) without a legislature having to enumerate every exception in advance — courts can calibrate protection to context in ways a rigid categorical text cannot.
% TRANSFER_FUNCTION: Moves interpretive authority over the boundary of protected speech from the legislature and the public text itself to the judiciary; moves the cost of establishing where a given act of speech falls from institutions with resources (media companies, established advocacy groups) who can litigate boundary cases, onto individuals and minority movements who cannot, and who bear chilling effects and adverse classifications in the interim.
% ABSENT_VOICES: Individual defendants prosecuted under obscenity, incitement, or threats statutes rarely have the resources to appeal a category-defining case to the Supreme Court; the doctrine is overwhelmingly shaped by cases that reach that level through well-funded institutional litigants (newspapers, universities, advocacy organizations with legal arms), meaning the balancing tests are calibrated against a narrow, resourced slice of speakers even though they bind everyone.
% DISAPPEARANCE_RATIONALE: If the case-by-case balancing framework disappeared and were replaced overnight with either a strict categorical/absolutist rule or a harm-limited rule, decades of doctrine (Brandenburg, Miller, Chaplinsky, Virginia v. Black) would need to be discarded or reinterpreted; entire practice areas built on litigating category boundaries would shrink; prosecutorial charging discretion over speech-adjacent conduct would narrow sharply; and predictability for ordinary speakers would increase substantially in either direction.
% FOUNDING_PROBLEM: The felt need, beginning most concretely in the early-to-mid 20th century (Schenck, Whitney, Chaplinsky), to reconcile an absolute constitutional text with the practical reality that some utterances (true threats, incitement to imminent lawless action, obscenity as then understood) seemed to warrant restriction without abandoning the broader free-speech guarantee.
% FOUNDING_PROBLEM_CORROBORATION: Sitting judges and mainstream constitutional scholars (largely drawn from or credentialed by the same academy that benefits from the doctrine's complexity) attest the founding problem remains live and requires ongoing judicial calibration. Outside corroboration is thinner: some legal historians and comparative constitutionalists (noting that other liberal democracies handle the same hard cases via legislated harm standards rather than open-ended judicial balancing) argue the felt necessity for judge-made balancing was itself a path-dependent choice rather than an inevitable response to the underlying problem, and that the problem could have been solved by narrower, legislatively-defined harm categories instead.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__categorical_balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__categorical_balancing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__categorical_balancing_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(first_amendment_speech_protection__categorical_balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__categorical_balancing_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.58) reflects that the balancing framework does perform a real coordination function (resolving genuinely hard cases the absolute text does not answer on its face) but layers onto that function a persistent transfer: interpretive authority and litigation advantage flow to institutional actors (judiciary, specialized bar, academy) while unpredictability costs are borne by individuals and under-resourced litigants who cannot afford to establish where the line falls for their own speech. Suppression (0.52) is moderate — the framework does not categorically ban speech, but the ex ante uncertainty about which side of a balancing test one's speech falls on produces real chilling effects functioning as soft suppression. Theater ratio (0.44) is elevated because a substantial share of appellate litigation activity in this area consists of relitigating boundary factors (imminence, obscenity's community-standards prong, threat context) that rarely change outcomes but sustain the doctrinal apparatus and its dependent practice/academic ecosystem. Accessibility collapse is moderate (0.4): alternative readings (absolutist, harm-limited) remain live in scholarship and dissent, so the categorical-balancing framework has not fully foreclosed its rivals, though it has captured the operative doctrine. Resistance (0.62) is substantial: civil liberties organizations, originalist judges, and harm-reduction advocates all actively contest this reading from different directions.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal judiciary sits at the low-d beneficiary end: it both administers the balancing test and is the entity whose discretion the test exists to preserve — no external body can finally fix category boundaries without judicial re-balancing. Appellate specialists and legal academics are secondary beneficiaries whose exit options are high (mobile/arbitrage) but whose professional position depends on the framework's persistence. Powerless individual speakers, especially those in politically disfavored categories, sit at the high-d target end: trapped exit options (an individual facing prosecution cannot simply route around the doctrine), immediate time horizon (the chilling effect operates now, on this speech act), and no meaningful capacity to litigate a category-defining appeal. Minority political movements are treated as a distinct payer group because the balancing test's harm assessment has a documented asymmetric-application history distinct from the general unpredictability cost borne by all disfavored-category speakers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling absolute text with cases that seemed to demand restriction) was real at inception but its status is now contested: for institutional legal actors it remains a live, ongoing problem requiring continuous judicial attention (which conveniently sustains their role); for critics it is a problem that could be, and in other jurisdictions has been, solved by legislatively fixed harm categories, making the perpetual balancing apparatus in the U.S. system a maintained-rather-than-necessary feature. The mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges is the diagnostic signal here: the doctrine clearly has real stakeholders and real consequences (nothing about it is inert), but whether its continuation reflects genuine ongoing necessity or captured institutional persistence is exactly the unresolved question this reading's omega variables are meant to hold open.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balancing_necessity_vs_institutional_capture,
    'Is the perpetual case-by-case balancing framework structurally necessary to handle genuinely hard speech cases, or is it a maintained institutional arrangement that a fixed legislative harm standard (as in comparable liberal democracies) could replace without loss of legitimate coordination function?',
    'Comparative constitutional analysis: examine outcomes in jurisdictions using codified harm-based speech statutes rather than open-ended judicial balancing, and assess whether predictability improved without corresponding loss of the ability to handle hard cases (true threats, incitement, obscenity).',
    'If comparable outcomes are achievable via fixed statutory harm standards, the balancing framework''s persistence reflects institutional interest (judicial and professional) rather than necessity, strengthening the tangled_rope classification toward a heavier extraction weighting. If hard cases are demonstrably better handled by ongoing judicial calibration, the coordination function is more clearly genuine and load-bearing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_necessity_vs_institutional_capture, empirical, 'Whether the balancing framework''s ongoing calibration function is structurally necessary or institutionally maintained.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Given that the First Amendment text itself (''Congress shall make no law...'') does not specify a categorical, balancing, or harm-limited interpretive method, on what basis does the categorical-balancing reading claim priority over its absolutist and harm-limited siblings within the same constitutional kernel?',
    'Doctrinal and historical analysis of how the balancing approach became dominant (tracing Holmes''s clear-and-present-danger dicta through Brandenburg to Miller and beyond) versus counterfactual analysis of what an entrenched absolutist or harm-limited reading would have produced at each major juncture.',
    'If the categorical-balancing reading''s dominance is better explained by path-dependent judicial choice than by textual or structural necessity, this reading''s status as ''the'' operative First Amendment doctrine (rather than one contested reading among three) is itself part of what the judiciary''s beneficiary position stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Committer-frame ambiguity: why this reading rather than a sibling reading governs current doctrine, and what that selection itself reveals about beneficiary structure.').

omega_variable(
    asymmetric_category_application_causal_mechanism,
    'Is the documented historical asymmetry in how incitement and fighting-words categories have been applied against minority political movements a contingent feature of specific judicial personnel and eras, or a structural feature of any balancing test administered by demographically non-representative courts?',
    'Longitudinal analysis of category-application outcomes across periods of greater and lesser judicial demographic diversity, controlling for case characteristics.',
    'If structural, the victim classification for minority_political_movements is a persistent feature of this reading regardless of which judges sit; if contingent, the harm is remediable through judicial composition change without altering the balancing framework itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(asymmetric_category_application_causal_mechanism, empirical, 'Whether asymmetric application to minority movements is inherent to balancing-by-judiciary or contingent on judicial composition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__categorical_balancing_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t0, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(firs_tr_t20, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(firs_tr_t40, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 40, 0.33).
narrative_ontology:measurement(firs_tr_t60, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 60, 0.37).
narrative_ontology:measurement(firs_tr_t80, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 80, 0.41).
narrative_ontology:measurement(firs_tr_t100, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 100, 0.44).

% Extraction over time
narrative_ontology:measurement(firs_be_t0, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(firs_be_t20, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(firs_be_t40, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 40, 0.47).
narrative_ontology:measurement(firs_be_t60, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement(firs_be_t80, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 80, 0.55).
narrative_ontology:measurement(firs_be_t100, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t0, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(firs_su_t20, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(firs_su_t40, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 40, 0.45).
narrative_ontology:measurement(firs_su_t60, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 60, 0.48).
narrative_ontology:measurement(firs_su_t80, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 80, 0.5).
narrative_ontology:measurement(firs_su_t100, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 100, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__categorical_balancing_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(first_amendment_speech_protection__categorical_balancing_reading, 0.12).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection__absolutist_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection__harm_limited_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint-family members sharing the first_amendment_speech_protection kernel. The absolutist_reading treats the founding text as categorically protective with only narrow historical carve-outs (beneficiary: speakers broadly; near-mountain low extraction). The harm_limited_reading ties protection loss to demonstrable unconsented harm rather than judicial category membership (beneficiary: harm-claimants; different victim set centered on speech causing diffuse rather than category-defined harm). This categorical_balancing_reading has the highest measured extractiveness of the three because its beneficiary structure (institutional judiciary plus dependent professional/academic ecosystem) is the most concentrated and the most clearly benefits from the doctrine's perpetual unsettledness. Each file must be evaluated independently; do not average epsilon across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
