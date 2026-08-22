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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: First Amendment Categorical Balancing Framework
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The categorical-balancing reading of the First Amendment holds that
 *   speech protection emerges from case-by-case judicial weighing of speech
 *   value against harm rather than from a textual rule. Courts develop
 *   doctrine by examining whether speech (obscene, inciting imminent
 *   lawlessness, a true threat) falls into categories the Court determines
 *   warrant reduced protection. The beneficiary is the federal judiciary,
 *   which retains interpretive authority to redraw category boundaries. The
 *   victims are speakers whose novel or marginalized speech falls into
 *   unprotected categories by judicial determination, and legal
 *   predictability itself — advance notice of what is protected. This story
 *   instantiates one reading of the First Amendment kernel; sibling readings
 *   (absolutist, harm-limited) would author the same constitutional text as
 *   producing different constraints with different beneficiaries and victims.
 *   This constraint story describes the categorical-balancing reading's
 *   structural consequences, not the contested truth of whether the reading
 *   is correct.
 *
 * KEY AGENTS:
 *   - Federal judiciary: maintains interpretive control over category boundaries; shifts what is protected by redefining categories.
 *   - Marginalized speakers: bear litigation risk when novel or heterodox speech faces categorical classification; lack resources to litigate boundaries.
 *   - Legal predictability: the non-agent capacity of speakers to know ex ante what is protected; eroded by deferred boundary-setting.
 *   - Established political orthodoxy: benefits from balancing logic that tends to protect orthodox speech while suppressing heterodox alternatives.
 *   - Novel speech forms: internet expression, algorithmic speech, performance art lack established precedent; face high uncertainty in category assignment.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__categorical_balancing_reading, 0.68).
domain_priors:suppression_score(first_amendment_speech_protection__categorical_balancing_reading, 0.52).
domain_priors:theater_ratio(first_amendment_speech_protection__categorical_balancing_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__categorical_balancing_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__categorical_balancing_reading, "First Amendment Categorical Balancing Framework").
narrative_ontology:topic_domain(first_amendment_speech_protection__categorical_balancing_reading, "constitutional/political").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__categorical_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__categorical_balancing_reading, 'd716d733-35fd-4c91-ae6d-e1025f77bcbb').
narrative_ontology:cs_kernel_codification('d716d733-35fd-4c91-ae6d-e1025f77bcbb', fixed_text).
narrative_ontology:cs_authority_grounding('d716d733-35fd-4c91-ae6d-e1025f77bcbb', lineage).
narrative_ontology:cs_interpretation_layer_present('d716d733-35fd-4c91-ae6d-e1025f77bcbb').
narrative_ontology:cs_reading_relation('d716d733-35fd-4c91-ae6d-e1025f77bcbb', first_amendment_speech_protection__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('d716d733-35fd-4c91-ae6d-e1025f77bcbb', first_amendment_speech_protection__harm_limited_reading, coexists_with).
narrative_ontology:cs_axiom('d716d733-35fd-4c91-ae6d-e1025f77bcbb', foundational, balancing_enables_protection).
narrative_ontology:cs_axiom_status(balancing_enables_protection, holdable).
narrative_ontology:cs_axiom_grounding('d716d733-35fd-4c91-ae6d-e1025f77bcbb', balancing_enables_protection, empirically_contingent).
narrative_ontology:cs_axiom('d716d733-35fd-4c91-ae6d-e1025f77bcbb', foundational, judicial_flexibility_over_textual_rule).
narrative_ontology:cs_axiom_status(judicial_flexibility_over_textual_rule, holdable).
narrative_ontology:cs_axiom_grounding('d716d733-35fd-4c91-ae6d-e1025f77bcbb', judicial_flexibility_over_textual_rule, deontological).
narrative_ontology:cs_reference_frame('d716d733-35fd-4c91-ae6d-e1025f77bcbb', judicial_flexibility_framework).
narrative_ontology:cs_drift_state('d716d733-35fd-4c91-ae6d-e1025f77bcbb', contemporary_digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d716d733-35fd-4c91-ae6d-e1025f77bcbb', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, established_political_orthodoxy).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, marginalized_speakers).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, novel_speech_forms).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, legal_predictability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Supreme Court and lower federal courts retain final authority to define speech categories and determine protection levels. They administer the constraint by issuing precedent, reviewing lower-court decisions, and explicitly redrawing category boundaries in high-profile cases. They frame their role as protecting speech against censorship while balancing legitimate state interests. They also define what counts as balancing and what counts as abdication of judicial responsibility, giving them control over the constraint's own justification.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Speakers whose political, sexual, religious, or artistic expression falls into judicial unprotected categories (incitement, obscenity, true threats) face criminal prosecution, civil liability, or prior restraint. They cannot afford extended litigation; they lack platforms to reach audiences without facing category scrutiny; they cannot leave the jurisdiction. Their recourse is to self-censor, to litigate and appeal (high cost), or to accept liability. Marginalized speakers bear the asymmetric risk of miscategorization in trial courts, before appellate correction is available.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, marginalized_speakers, payer,
    powerless, biographical, trapped, national).

% The structural property that law provides advance notice to speakers of what conduct is protected or prohibited. Case-by-case balancing means speakers cannot know ex ante whether their proposed speech will be protected; they must either self-censor or litigate. This undermines the traditional rule-of-law principle that law guides conduct through advance notice.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, legal_predictability, payer,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(first_amendment_speech_protection__categorical_balancing_reading, legal_predictability).

% Political speech that aligns with court-assessed national consensus (national security, public order, moral majority positions) receives favorable framings in balancing tests. When courts weight speech value against harm, they tend to find lower harm in orthodoxy-aligned speech and higher speech value in orthodoxy-supporting expression. Established orthodoxy has exit options: it can fund litigation, lobby the legislature, or shift public opinion to influence how courts frame future balancing tests.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, established_political_orthodoxy, beneficiary,
    organized, generational, arbitrage, national).

% Internet expression, algorithmic content, memes, deepfakes, decentralized organizing platforms, and other speech technologies that postdate Supreme Court categorical doctrine lack established precedent. Courts must retrofit them into categories designed for analog speech; the fit is often poor. Innovators in speech form bear substantial litigation risk that their technology will be deemed unprotected. They have some exit options (moving to jurisdictions with clearer rules) but are substantially constrained by the First Amendment's national scope.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, novel_speech_forms, payer,
    moderate, biographical, constrained, national).

% Organizations (ACLU, First Amendment Coalition, partisan speech-protection groups) that advocate for broad speech protection would argue for fixed category boundaries and reduced judicial discretion. They participate in litigation but do not set the constraint; the Supreme Court retains authority to accept or reject their arguments. Their exclusion is structural: the balancing framework allows courts to frame their arguments as correct in principle but incorrect in application (case-by-case judgment supersedes categorical advocacy).
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, civil_liberties_advocates, excluded,
    organized, biographical, constrained, national).

% Congress can legislate speech restrictions (sedition laws, cybersecurity regulations, revenge-porn bans), but the Supreme Court's final interpretive power over the First Amendment means Congress operates under judicial veto. The categorical balancing framework allows courts to strike down laws that the Court deems insufficiently justified by competing interests, limiting legislative authority and keeping interpretive power in the judiciary.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, legislative_branch, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(first_amendment_speech_protection__categorical_balancing_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(first_amendment_speech_protection__categorical_balancing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Balances social coordination interests (preventing violence, protecting children, maintaining public order) against individual expression rights. The judicial category system coordinates between competing goods: it protects speech necessary for democratic function and individual autonomy while suppressing speech that poses immediate, demonstrable harms.
% TRANSFER_FUNCTION: Transfers definitional authority over speech protection from explicit legal text to judicial case-by-case assessment. Moves legal predictability (the property that speakers can know ex ante what is protected) from speakers to courts. Moves risk of miscategorization from state to marginalized speakers who may be prosecuted under uncertain doctrine and later vindicated on appeal.
% ABSENT_VOICES: Speakers harmed by miscategorization in lower courts speak only after suffering prosecution or injunction. Marginalized groups whose speech is systematically pushed into unprotected categories lack collective power to participate in boundary-setting. International human-rights bodies that argue First Amendment doctrine violates international norms are excluded from the U.S. constitutional conversation. Future speakers with technologies not yet invented cannot participate in shaping the categories that will govern them.
% DISAPPEARANCE_RATIONALE: If categorical balancing disappeared and speech received absolute protection (or if categories were fixed by legislative rule rather than judicial redefinition), the political equilibrium would shift substantially: marginalized speakers would gain predictable protection; organized majorities would lose the ability to use courts to suppress novel political tactics; litigants would face different baseline rules. The entire constitutional law of speech would reorganize around different institutional authority.
% FOUNDING_PROBLEM: Early First Amendment doctrine (1920s–1940s) relied on rigid categorical judgments (sedition, obscenity) that courts applied without examining whether speech actually caused the harms asserted. The Court's response was to develop case-by-case balancing: examine the actual speech, its context, the demonstrated harm, and weigh them before deciding protection.
% FOUNDING_PROBLEM_CORROBORATION: Judiciary attests the problem of rigid categories producing unjust suppression remains live. Free-speech advocates from outside the judiciary (civil-liberties organizations, legal scholars) attest the founding problem was solved in the 1960s-70s (Brandenburg era; Times v. Sullivan era) and the constraint now persists as institutional power preservation, not as a response to the founding harm. Comparative constitutional law scholarship from non-U.S. traditions documents that democracies protect speech without case-by-case balancing, suggesting the founding problem is not universal.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__categorical_balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__categorical_balancing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__categorical_balancing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.68) is moderately high because the constraint transfers definitional authority from the text to the judiciary, and the judiciary's power to redraw boundaries means speakers cannot be certain of their protection ex ante. Suppression (0.52) is moderate: the constraint does not impose outright prohibition, but the uncertainty and litigation risk function as a suppression mechanism for speakers with limited resources. Theater (0.41) is moderate-high: significant energy in lower courts goes to applying and litigating category boundaries, but the category definitions themselves are substantive — this is not pure performance. Resistance (0.71) is high because free-speech advocates, civil-society organizations, and speakers themselves mount substantial resistance to category narrowing and to the balancing framework generally. Accessibility collapse (0.58) is moderate: alternatives to court-defined categories exist (textual reading, legislative clarity) but are foreclosed by the Supreme Court's institutional dominance in interpreting the Constitution. The temporal series shows extraction gradually accumulating (0.42 → 0.68 over 75 years) as the balancing framework becomes more entrenched and courts apply it to an expanding range of speech forms (digital, algorithmic) that fit poorly into the original categories. Theater ratio rises with extraction, suggesting that maintaining boundary flexibility requires increasing performative effort as courts justify why novel cases fall into or outside established categories. The shared temporal grid ensures each metric is authored at every time point.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's seat, the balancing framework is a solution to an important problem: rigid categories produced unjust suppression in the mid-20th century, and case-by-case judgment allows courts to protect speech that rigid rules would have suppressed. From marginalized speakers' seats, the framework is a source of legal uncertainty that privileges the powerful: major speakers (corporations, established political organizations) can afford to litigate category boundaries repeatedly; marginal speakers face high litigation cost and must self-censor to avoid risk. From the civil-society observer seat, the framework concentrates interpretive authority in the judiciary while leaving speakers with no advance notice of protection. The engine should compute this divergence from the structural data: a powerful institutional actor (judiciary) with interpretive authority and low litigation cost versus powerless speakers with trapped exit should show different per-seat types.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal judiciary sits at the beneficiary end (d near 0.0) because it retains interpretive authority and controls the redefinition of categories — no other seat can unilaterally change the framework. Marginalized speakers sit at the target end (d near 1.0): they bear legal risk, face uncertain protection, and have no power to shift the framework. Established orthodoxy sits near the beneficiary end (d ~ 0.15–0.25) because the balancing logic tends to filter out heterodox speech, though it is not institutionally captured the way the judiciary is. Legal predictability is an abstract entity (agent: false) and does not have directionality in the traditional sense, but its collapse benefits the judiciary (they retain authority) and harms speakers (they cannot plan conduct). The legislative branch is an observer: it can pass speech laws, but the Supreme Court's final interpretive power means it operates under a de facto judicial veto, limiting its directional position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (rigid categories producing unjust suppression) was substantially solved by the Brandenburg framework and the Times v. Sullivan era (1960s–1970s). However, the categorical balancing constraint persists and has expanded. The theater ratio rising from 0.25 to 0.41 suggests increasing performative maintenance: courts spend more energy justifying why novel speech forms fit into categories designed for pre-digital expression (is algorithmic content 'speech'? is a deepfake 'obscene'?). The constraint displays the mandatrophy signature: it was built to solve a real problem, the problem is substantially solved, but the constraint persists (and expands) due to institutional inertia and the judiciary's interest in maintaining interpretive authority. A snare reading would argue the constraint is pure extraction dressed in coordination language; a rope reading would argue it continues to serve a real coordination function (preventing violence, protecting children). The categorization depends on whether the judiciary's power to define categories is proportionate to the coordination problem solved — this is the subject of the foundational axiom ambiguity (see omegas).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balancing_versus_rule_necessity,
    'Is case-by-case balancing structurally necessary to protect important speech while suppressing genuine harms, or does it primarily serve the judiciary''s interest in maintaining interpretive authority?',
    'Comparative analysis: other democracies (Canada, Australia, Israel) protect speech under fixed constitutional rules and balance competing interests at the legislative rather than judicial stage; if their speech protection is comparably robust, balancing is not necessary.',
    'If balancing is not necessary, the constraint reclassifies from tangled_rope (genuine coordination + asymmetric extraction) to snare (pure extraction with coordination cover). If necessary, it remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_versus_rule_necessity, empirical, 'Whether the balancing framework is required to serve speech protection or primarily serves institutional power.').

omega_variable(
    categorical_definition_versus_discretion,
    'Is the uncertainty in category assignment (which speech is protected) a feature of the balancing approach or a defect of judicial implementation?',
    'Textual analysis of Supreme Court opinions: do courts state that category boundaries are deliberately left fluid to allow case-by-case judgment, or do they express regret about uncertainty and attempt to provide clearer rules?',
    'If uncertainty is intentional, it suggests the judiciary values retention of interpretive authority over legal clarity. If unintentional, it suggests the balancing framework produces uncertainty as a side effect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(categorical_definition_versus_discretion, conceptual, 'Whether legal uncertainty is a structural feature or an implementation problem.').

omega_variable(
    suppression_mechanism_structural_versus_internalized,
    'Do marginalized speakers self-censor due to objective legal uncertainty and litigation risk (structural suppression), or due to believing that their speech is legitimately unprotected (internalized suppression)?',
    'Empirical studies of speaker behavior pre- and post-judicial category announcements; interviews with speakers who have litigated or considered litigation; analysis of speech activity in jurisdictions with fixed versus balancing frameworks.',
    'If primarily structural, removing uncertainty (fixed categories) would restore suppressed speech. If primarily internalized, speakers would need identity reframing or repeated exposure to speakers who exercise protected speech before recovering it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_versus_internalized, empirical, 'The mechanism of suppression in uncertain legal environments.').

omega_variable(
    axiom_hostility_to_originalism,
    'Does the categorical-balancing reading''s foundational claim (that balancing is required to protect speech) foreclose the absolutist reading''s foundational claim (that textual ''no law'' provides clearer protection)?',
    'Logical analysis: can a framework that values ''balance'' and ''flexibility'' coexist with a framework that values ''categorical rules'' in the same legal system? Or do they require mutually exclusive premises about what law should be?',
    'If foreclosed, the readings are incompatible siblings; both cannot be instantiated in the same constitutional order. If coexisting, they are live alternatives that different judges and scholars can advocate for.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(axiom_hostility_to_originalism, conceptual, 'The logical relationship between balancing and absolutist interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__categorical_balancing_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t0, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(firs_tr_t10, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 10, 0.29).
narrative_ontology:measurement(firs_tr_t25, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 25, 0.35).
narrative_ontology:measurement(firs_tr_t40, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 40, 0.39).
narrative_ontology:measurement(firs_tr_t60, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 60, 0.41).
narrative_ontology:measurement(firs_tr_t75, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 75, 0.41).

% Extraction over time
narrative_ontology:measurement(firs_be_t0, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(firs_be_t10, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(firs_be_t25, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(firs_be_t40, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(firs_be_t60, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 60, 0.67).
narrative_ontology:measurement(firs_be_t75, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 75, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t0, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(firs_su_t10, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(firs_su_t25, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 25, 0.48).
narrative_ontology:measurement(firs_su_t40, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 40, 0.51).
narrative_ontology:measurement(firs_su_t60, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 60, 0.52).
narrative_ontology:measurement(firs_su_t75, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 75, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__categorical_balancing_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(first_amendment_speech_protection__categorical_balancing_reading, 0.18).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection__absolutist_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection__harm_limited_reading).

% DUAL FORMULATION NOTE:
% The first_amendment_speech_protection kernel is instantiated by three separate constraints, one per reading. The categorical_balancing_reading (this story) describes a constraint in which courts define protected categories through case-by-case adjudication. The absolutist_reading describes a constraint in which protection is categorical except for narrow historical exceptions ('no law' means nearly-absolute protection). The harm_limited_reading describes a constraint in which protection yields only when speech causes demonstrable harm. Each reading instantiates a different constraint with different beneficiaries, victims, and protection patterns. The readings coexist as live positions in contemporary constitutional debate; they are not sequenced or nested. This story describes the balancing reading alone, without averaging or hedging across the sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(first_amendment_speech_protection__categorical_balancing_reading, organized, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
