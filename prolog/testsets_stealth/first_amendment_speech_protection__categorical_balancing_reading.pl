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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: First Amendment Protected/Unprotected Category System via Case-by-Case Judicial Balancing
 *   domain: constitutional law/political philosophy/speech regulation
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   first_amendment_speech_protection: the categorical balancing reading,
 *   under which the First Amendment's protected speech set is constituted by
 *   judicially drawn categories (obscenity, incitement, true threats,
 *   fighting words excluded) refined through case-by-case weighing of speech
 *   value against asserted harm. The sibling readings — absolutist_reading
 *   and harm_limited_reading — are separate constraints in separate files;
 *   per the epsilon-invariance principle this file contains one reading with
 *   one stable epsilon, and the contest among readings is routed to omega
 *   variables rather than averaged into the metrics. The epsilon referent is
 *   the standing categorical-balancing arrangement itself, assessed from the
 *   authoring seat: a regime that genuinely coordinates speech law while
 *   concentrating interpretive authority in the bench, imposing retrospective
 *   uncertainty on speakers, and sorting disproportionate shares of dissident
 *   and minority speech into the excluded categories. Claim and metrics are
 *   independent authored facts: the claimed type is what I believe
 *   structurally true; the metrics describe the regime's actual operation as
 *   I read the historical record.
 *
 * KEY AGENTS:
 *   - - us_supreme_court: Agenda-setter and primary beneficiary (institutional/identity_locked) — administers the category system, controls its rate of change, collects interpretive authority
 *   - - federal_appellate_judiciary: Secondary beneficiary (institutional/identity_locked) — applies the categories, gains doctrinal structure and legitimacy from the framework
 *   - - dissident_and_minority_speakers: Primary target (moderate/trapped) — bear retrospective sorting into excluded categories and the chilling cost of uncertainty
 *   - - mainstream_speakers_and_press: Incidental beneficiary with payer secondary role (powerful/mobile) — collect robust core protection at low cost, finance boundary litigation
 *   - - state_legislatures_and_regulators: Regulated payer (institutional/constrained) — regulatory programs subject to category-line vetoes drawn elsewhere
 *   - - legal_predictability: Non-agent bearer of cost (rule-of-law interest) — eroded by retrospective case-by-case valuation
 *   - - constitutional_law_academy: Secondary beneficiary (organized/identity_locked) — careers ride on expounding the categories
 *   - - civil_liberties_bar: Analytical observer (organized/analytical) — sees the full structure across the case stream, collects no rent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__categorical_balancing_reading, 0.58).
domain_priors:suppression_score(first_amendment_speech_protection__categorical_balancing_reading, 0.55).
domain_priors:theater_ratio(first_amendment_speech_protection__categorical_balancing_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, resistance, 0.32).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__categorical_balancing_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__categorical_balancing_reading, "First Amendment Protected/Unprotected Category System via Case-by-Case Judicial Balancing").
narrative_ontology:topic_domain(first_amendment_speech_protection__categorical_balancing_reading, "constitutional law/political philosophy/speech regulation").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__categorical_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__categorical_balancing_reading, 'fed36cdd-51ed-4733-bc05-14fb3cf6c6d5').
narrative_ontology:cs_kernel_codification('fed36cdd-51ed-4733-bc05-14fb3cf6c6d5', fixed_text).
narrative_ontology:cs_authority_grounding('fed36cdd-51ed-4733-bc05-14fb3cf6c6d5', extraction).
narrative_ontology:cs_interpretation_layer_present('fed36cdd-51ed-4733-bc05-14fb3cf6c6d5').
narrative_ontology:cs_reading_relation('fed36cdd-51ed-4733-bc05-14fb3cf6c6d5', first_amendment_speech_protection__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('fed36cdd-51ed-4733-bc05-14fb3cf6c6d5', first_amendment_speech_protection__harm_limited_reading, coexists_with).
narrative_ontology:cs_axiom('fed36cdd-51ed-4733-bc05-14fb3cf6c6d5', foundational, speech_protection_is_judicially_administered).
narrative_ontology:cs_axiom_status(speech_protection_is_judicially_administered, holdable).
narrative_ontology:cs_axiom_grounding('fed36cdd-51ed-4733-bc05-14fb3cf6c6d5', speech_protection_is_judicially_administered, conventional).
narrative_ontology:cs_axiom('fed36cdd-51ed-4733-bc05-14fb3cf6c6d5', foundational, category_boundaries_require_case_by_case_valuation).
narrative_ontology:cs_axiom_status(category_boundaries_require_case_by_case_valuation, holdable).
narrative_ontology:cs_axiom_grounding('fed36cdd-51ed-4733-bc05-14fb3cf6c6d5', category_boundaries_require_case_by_case_valuation, empirically_contingent).
narrative_ontology:cs_reference_frame('fed36cdd-51ed-4733-bc05-14fb3cf6c6d5', court_administered_category_system).
narrative_ontology:cs_drift_state('fed36cdd-51ed-4733-bc05-14fb3cf6c6d5', contemporary_originalist_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fed36cdd-51ed-4733-bc05-14fb3cf6c6d5', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, us_supreme_court).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, federal_appellate_judiciary).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, constitutional_law_academy).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, mainstream_speakers_and_press).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, dissident_and_minority_speakers).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, state_legislatures_and_regulators).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, legal_predictability).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, mainstream_speakers_and_press).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Nine justices decide which speech categories receive protection and which fall outside it, writing the multi-factor tests lower courts must apply. Each term the Court chooses which speech disputes to hear, controlling the speed and direction in which the category lines move. Its authority and institutional self-understanding are bound up with being the body that says what the Constitution's speech guarantee means; abandoning that role would mean redefining the institution itself.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, us_supreme_court, agenda_setter,
    institutional, generational, identity_locked, national).

% Hundreds of circuit and district judges apply the Supreme Court's categories to novel fact patterns. The doctrine supplies their opinions with structure, citable standards, and reversal-avoidance heuristics; it also exposes them to reversal when their weighing diverges from the current majority's instincts. Life tenure removes career pressure, but their professional identity forms around faithful administration of the framework.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, federal_appellate_judiciary, beneficiary,
    institutional, generational, identity_locked, national).

% Protesters, dissidents, sexual-minority publishers, radical organizers, and nonmainstream religious or political speakers learn only after the fact whether their speech landed in a protected or excluded category. Historically the excluded categories — incitement, obscenity, fighting words, threats — have been filled disproportionately with their speech. Speaking is their principal lever for changing their circumstances, so silence is not a usable exit; the realistic options are speak and risk prosecution or litigation, or self-censor.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, dissident_and_minority_speakers, payer,
    moderate, biographical, trapped, national).

% Large media organizations and established political speakers operate almost entirely inside the strongly protected core. They collect the regime's protections at low operating cost — their speech rarely approaches the excluded categories — though they finance litigation when boundaries are tested. Their resources and platforms give them exits unavailable to smaller speakers, including relocating operations or distribution channels.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, mainstream_speakers_and_press, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(first_amendment_speech_protection__categorical_balancing_reading, mainstream_speakers_and_press, payer).

% State governments repeatedly attempt to regulate speech adjacent to the excluded categories — obscenity, harassment, election misinformation, intimate-image sharing — and see their statutes struck down or narrowed when they cross a line drawn elsewhere. They can redraft around the lines at the cost of staff time and litigation risk, but they cannot opt out of the framework that reviews their work.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, state_legislatures_and_regulators, payer,
    institutional, biographical, constrained, regional).

% The rule-of-law interest in knowing in advance what speech is punishable. Case-by-case valuation determines the protected set retrospectively, one dispute at a time, so no speaker or regulator can consult a stable rule; this interest loses ground with each new multi-factor test. Recorded here as a non-agent bearer of cost for completeness.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, legal_predictability, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(first_amendment_speech_protection__categorical_balancing_reading, legal_predictability).

% Law professors, treatise writers, and casebook authors build careers explicating, defending, and critiquing the category system. The doctrine's complexity generates the scholarly output the field runs on; a radically simplified or dismantled framework would shrink the interpretive enterprise that employs them.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, constitutional_law_academy, beneficiary,
    organized, generational, identity_locked, national).

% Litigators at civil-liberties organizations see the whole structure across hundreds of cases: which clients win, which categories absorb which speech, where the tests diverge from their stated rationales. They take no rent from the arrangement and analyze it from outside the bench-legislature axis.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, civil_liberties_bar, observer,
    organized, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(first_amendment_speech_protection__categorical_balancing_reading, us_supreme_court).
narrative_ontology:fixing_cost_class(first_amendment_speech_protection__categorical_balancing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The category system solves a real problem: unconditional protection of all speech is untenable (threats, fraud, defamation require regulation), and standardless ad hoc judgment is worse. Judicially defined categories give lower courts, legislatures, and speakers shared reference points for what regulation is permissible, making speech law administrable across thousands of disputes and giving speakers rough ex ante signals about risk.
% TRANSFER_FUNCTION: Moves interpretive authority over the boundaries of permissible speech from speakers and legislatures to the federal judiciary — specifically the power to define, case by case, which speech counts as valuable enough to protect. It also moves litigation costs from the public purse to individual speakers, who must sue to discover whether their speech is protected.
% ABSENT_VOICES: The speakers chilled into silence never appear — the regime's heaviest costs fall on those who never file suit because they cannot afford it or cannot risk it. Also absent: the absolutist juristic tradition (the Black-Douglas line) whose reading lost and whose descendants stand outside the doctrinal conversation, and ordinary citizens whose speech lives are governed by tests they have never heard of.
% DISAPPEARANCE_RATIONALE: If the category-and-balancing regime vanished overnight, speech law would rearrange immediately: either protection would become near-absolute (content-based regulation collapsing wholesale) or legislative regulation would flood previously excluded zones. Thousands of precedents would lose their organizing structure, every pending speech prosecution and statute would become newly contestable, and the judiciary would lose the gatekeeping role that organizes its docket.
% FOUNDING_PROBLEM: Early twentieth-century courts faced an unworkable text: 'no law' could not literally mean no law, yet ad hoc punishment of dissent (Debs, Schenck-era prosecutions) threatened to reduce the guarantee to nothing. The founding problem was how to reconcile the absolute text with the undeniable need to regulate some speech, without handing legislators unlimited discretion — answered by inventing judicially administered categories and case-by-case valuation.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians outside the judiciary corroborate the genealogy: the balancing apparatus is documented as a pragmatic judicial invention evolving from Hand's Masses opinion through Holmes and Brandeis to Brandenburg, not a textual command. Justice Black's dissenting opinions explicitly attested, from inside the institution but outside the benefiting faction, that the category-and-balance approach had no basis in the amendment's text. Textualist and originalist scholarship continues to corroborate that the framework is constructed rather than discovered.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__categorical_balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__categorical_balancing_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__categorical_balancing_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(first_amendment_speech_protection__categorical_balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__categorical_balancing_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.58: the regime delivers real protection to most speech, but its costs are concentrated and structural — protection is retrospective, litigation-dependent, and category-mediated, and the excluded categories have historically absorbed dissident and minority speech at disproportionate rates. Suppression 0.55 is authored as a raw structural property (unscaled by the engine): the framework's persistence depends on active machinery — certiorari gatekeeping, stare decisis hardening, the judicial-supremacy norm — that forecloses rival interpretive frameworks inside adjudication. Theater ratio 0.50: the multi-factor tests perform rigorous neutral methodology over what is often substantive judicial preference; roughly half the doctrinal activity constrains outcomes, half rationalizes them. Accessibility collapse 0.40: alternatives persist outside the courtroom (speaking with risk, drafting around lines, advocating rival readings) but collapse inside it. Resistance 0.32: persistent scholarly and dissenting critique, periodic legislative pushback, none effectively organized against the framework itself. The temporal series share one grid and tell a U-shaped story: the early regime was highly extractive (sedition prosecutions routinely sustained, 1919), liberalized to peak protection around Brandenburg (1969), then re-hardened as doctrine ossified into preference-driven balancing and the enforcement machinery rebuilt to defend the framework against originalist assault. The suppression_requirement series is authored deliberately: this story traces enforcement-capacity change (intensive early deployment of judicial authority against dissent, relaxation during the broad-protection era, re-intensified gatekeeping late in the interval), which the scalar base_properties.suppression alone cannot carry.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different types from identical structure. From the agenda-setter seat (the Court), the arrangement is faithful stewardship of an unworkable text — the categories are discoveries, the balancing is necessity. From the trapped payer seats (dissident speakers), the same structure operates as a categorization lottery: protection arrives only after the fact, priced in prosecution risk, and the excluded categories function as containers their speech keeps falling into. From the legislature seat, it is an unaccountable veto player over democratically enacted programs. The engine derives this divergence from power, exit, and directional position; the divergence is the measurement, not noise to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   The Court and appellate judiciary sit at the beneficiary pole: they collect interpretive authority and doctrinal centrality, and their identity-lock (the Marbury function fused with institutional self-concept) places them deep at the subsidized end — effective extraction inverts toward subsidy for them. The academy adds a second low-directionality seat: complexity is its payroll. Mainstream press is a near-pure beneficiary with arbitrage-grade mobility, damping its already-low directionality further. Dissident and minority speakers sit at the target pole: victims of the sorting function, trapped because silence forfeits their only lever, so effective extraction amplifies toward the full-target end. State legislatures are targets with power but constrained exit — materially extracted-from, unable to leave the review framework. Legal predictability is authored as a non-agent victim: it feeds no directionality arithmetic (by design), but records the diffuse rule-of-law cost the delta identifies. Note the scaling asymmetry the engine applies: only extractiveness scales with directionality and scope; suppression enters raw.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification blocks two symmetrical mislabels. Reading the regime as pure coordination (rope) — the judiciary's own self-description — would erase the asymmetric extraction: discretion rents flowing to the bench, sorting costs borne by minorities, predictability losses borne by everyone. Reading it as pure extraction (snare) — the absolutist critique's framing — would erase the genuine coordination function: administrability across thousands of disputes, shared reference points, real protection for core speech. Mandatrophy status: the founding problem (reconciling an absolute text with regulable speech) is live, so the mandate has not outlived its function and no atrophy declaration is authored. However, the rising theater_ratio series tracks Goodhart drift within the live mandate: the tests increasingly perform justification rather than constrain outcomes, which is the leading indicator that would convert this tangled_rope toward piton if the coordination function continued atrophying while the apparatus persisted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_restructuring,
    'This constraint is one reading of the kernel first_amendment_speech_protection (reading: categorical_balancing_reading). What would adoption of a sibling reading — absolutist_reading or harm_limited_reading — change structurally?',
    'Appointment composition shifts or explicit overruling that installs a sibling reading as the operative doctrine; observe whether the beneficiary set (judicial interpretive control) and victim set (sorting of minority speech, predictability losses) dissolve or migrate.',
    'An absolutist sibling would eliminate the judicial-discretion beneficiary structure entirely and collapse the category apparatus, driving measured extraction toward zero; a harm-limited sibling would re-center victims as harm-claimants and move the extraction burden onto speakers facing harm allegations. The disagreement is located in WHO draws the protected/unprotected line: the text itself, the harm suffered, or the court.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_restructuring, conceptual, 'Committer structure: sibling readings of the same kernel would restructure this constraint''s beneficiary and victim sets.').

omega_variable(
    necessity_vs_institutional_interest,
    'Is judicial administration of speech categories a structural necessity of any workable free-speech constitution (an unavoidable coordination cost), or a constructed arrangement that serves the judiciary''s institutional interest in interpretive monopoly?',
    'Comparative constitutional analysis of jurisdictions that allocate speech-line-drawing differently (legislative definitions, proportionality review, citizen juries), plus counterfactual analysis of whether an administrable regime without judicial category stewardship is coherent.',
    'If necessity, part of the measured burden is the irreducible price of coordination and the classification sits closer to rope; if institutional interest, the discretion rents are extractive overhead and the classification sits closer to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_vs_institutional_interest, conceptual, 'Whether the category system is an unavoidable coordination cost or a self-serving judicial construction.').

omega_variable(
    invisible_chilling_population,
    'How large is the population of speakers who never reach a courtroom because the regime''s retrospective, litigation-dependent protection structure deters them from speaking at all?',
    'Survey and natural-experiment data on speech abstention correlated with doctrinal uncertainty (e.g., comparing speech activity before and after category-narrowing decisions).',
    'The regime''s largest cost is systematically invisible to the case record; a larger unseen population raises true extractiveness above the measured 0.58 and strengthens the target-side directionality of dissident speakers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(invisible_chilling_population, empirical, 'Size of the chilled-speaker population invisible to adjudicated cases.').

omega_variable(
    vestigial_category_theater,
    'Which declared categories (obscenity, fighting words, group libel remnants) are load-bearing regulators of real disputes versus theatrical remnants maintained in doctrine but producing almost no operative applications?',
    'Empirical census of applications per category across reported decisions over the interval; categories with near-zero successful applications while retaining doctrinal prominence are theatrical.',
    'A higher share of vestigial categories raises the functional theater ratio above the authored 0.50 and pushes the regime toward inertial maintenance; a lower share confirms the categories do real sorting work.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vestigial_category_theater, empirical, 'Load-bearing versus vestigial composition of the unprotected-category set.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__categorical_balancing_reading, 1919, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t1919, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1919, 0.22).
narrative_ontology:measurement(firs_tr_t1942, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1942, 0.28).
narrative_ontology:measurement(firs_tr_t1969, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1969, 0.36).
narrative_ontology:measurement(firs_tr_t1986, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1986, 0.43).
narrative_ontology:measurement(firs_tr_t2007, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 2007, 0.47).
narrative_ontology:measurement(firs_tr_t2026, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 2026, 0.5).

% Extraction over time
narrative_ontology:measurement(firs_be_t1919, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1919, 0.74).
narrative_ontology:measurement(firs_be_t1942, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1942, 0.66).
narrative_ontology:measurement(firs_be_t1969, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1969, 0.47).
narrative_ontology:measurement(firs_be_t1986, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1986, 0.51).
narrative_ontology:measurement(firs_be_t2007, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 2007, 0.55).
narrative_ontology:measurement(firs_be_t2026, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 2026, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t1919, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1919, 0.6).
narrative_ontology:measurement(firs_su_t1942, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1942, 0.54).
narrative_ontology:measurement(firs_su_t1969, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1969, 0.44).
narrative_ontology:measurement(firs_su_t1986, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1986, 0.47).
narrative_ontology:measurement(firs_su_t2007, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 2007, 0.51).
narrative_ontology:measurement(firs_su_t2026, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 2026, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__categorical_balancing_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection__absolutist_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection__harm_limited_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the kernel first_amendment_speech_protection. The colloquial label 'First Amendment speech protection' conflates three structurally distinct claims with distinct epsilon values: the absolutist reading (protection categorical except narrow historical exclusions; negligible discretionary extraction), this categorical balancing reading (judicially administered categories with case-by-case valuation; moderate extraction — discretion rents to the bench, sorting costs to minority speakers, predictability losses), and the harm-limited reading (protection yields to demonstrable unconsented harm; extraction shifts onto speakers facing harm allegations). The fixed constitutional text is upstream of all three; the siblings are parallel instantiations, not stages. Linked via affects_constraints for contamination propagation: degradation of this reading's legitimacy (originalist critique) pressures both siblings' environments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
