% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_speech_protection__absolutist_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: first_amendment_speech_protection__absolutist_reading
 *   human_readable: First Amendment Absolutist Reading: Categorical Speech Protection
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The absolutist reading of the First Amendment treats the constitutional
 *   text 'no law... abridging the freedom of speech' as a categorical
 *   prohibition that admits only narrow, historically-grounded carve-outs
 *   (incitement to imminent lawless action, defamation, obscenity). Under
 *   this reading, speech protection is maximized; harm to minorities is
 *   externalized as a cost of categorical liberty rather than grounds for
 *   regulation. The reading benefits speakers and publishers (broad
 *   protection, predictable legal rule) while imposing costs on targeted
 *   minorities (hate speech, dehumanizing rhetoric, no legal recourse). This
 *   is ONE reading of a contested kernel — the First Amendment text is
 *   subject to multiple coherent interpretations, each with different
 *   beneficiary/victim structures and different constitutional implications.
 *   The claim (mountain) and metrics (high extractiveness, moderate
 *   suppression) diverge deliberately: this story treats the absolutist
 *   reading as a natural-law-style constitutional claim while describing its
 *   operational effects as substantially extractive for targeted minorities.
 *
 * KEY AGENTS:
 *   - Speakers and Publishers (beneficiary; organized power; arbitrage exit) — operate under maximal speech protection; need not justify before publishing
 *   - Majority Political Coalitions (beneficiary; institutional power; mobile exit) — benefit from symmetric protection that constrains state suppression of their own political speech
 *   - Targeted Minorities (victim/payer; powerless; identity-locked exit) — bear the cost of hate speech, slurs, dehumanizing rhetoric with no legal recourse
 *   - Constitutional Originalists (agenda-setter; institutional power; analytical exit) — interpret the text and defend the categorical reading in litigation and doctrine
 *   - Harm-Limited Reading Advocates (excluded; organized power; constrained exit) — argue speech causing demonstrable harm should receive reduced protection
 *   - Categorical Balancing Advocates (excluded; organized power; constrained exit) — advocate case-by-case balancing rather than categorical protection
 *   - State Actors (observer; institutional power; analytical exit) — bound by the constraint; cannot restrict speech without violating the categorical prohibition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__absolutist_reading, 0.68).
domain_priors:suppression_score(first_amendment_speech_protection__absolutist_reading, 0.45).
domain_priors:theater_ratio(first_amendment_speech_protection__absolutist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__absolutist_reading, mountain).
narrative_ontology:human_readable(first_amendment_speech_protection__absolutist_reading, "First Amendment Absolutist Reading: Categorical Speech Protection").
narrative_ontology:topic_domain(first_amendment_speech_protection__absolutist_reading, "constitutional/political").

domain_priors:emerges_naturally(first_amendment_speech_protection__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__absolutist_reading, 'ec01c62f-e503-4a6c-a02e-52efc6bdc858').
narrative_ontology:cs_kernel_codification('ec01c62f-e503-4a6c-a02e-52efc6bdc858', fixed_text).
narrative_ontology:cs_authority_grounding('ec01c62f-e503-4a6c-a02e-52efc6bdc858', lineage).
narrative_ontology:cs_interpretation_layer_present('ec01c62f-e503-4a6c-a02e-52efc6bdc858').
narrative_ontology:cs_reading_relation('ec01c62f-e503-4a6c-a02e-52efc6bdc858', first_amendment_speech_protection__categorical_balancing_reading, coexists_with).
narrative_ontology:cs_reading_relation('ec01c62f-e503-4a6c-a02e-52efc6bdc858', first_amendment_speech_protection__harm_limited_reading, coexists_with).
narrative_ontology:cs_axiom('ec01c62f-e503-4a6c-a02e-52efc6bdc858', foundational, categorical_text_fixes_meaning).
narrative_ontology:cs_axiom_status(categorical_text_fixes_meaning, holdable).
narrative_ontology:cs_axiom_grounding('ec01c62f-e503-4a6c-a02e-52efc6bdc858', categorical_text_fixes_meaning, conventional).
narrative_ontology:cs_axiom('ec01c62f-e503-4a6c-a02e-52efc6bdc858', foundational, speech_protection_maximized_by_design).
narrative_ontology:cs_axiom_status(speech_protection_maximized_by_design, holdable).
narrative_ontology:cs_axiom_grounding('ec01c62f-e503-4a6c-a02e-52efc6bdc858', speech_protection_maximized_by_design, deontological).
narrative_ontology:cs_reference_frame('ec01c62f-e503-4a6c-a02e-52efc6bdc858', categorical_prohibition_on_speech_regulation).
narrative_ontology:cs_drift_state('ec01c62f-e503-4a6c-a02e-52efc6bdc858', contemporary_social_media_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ec01c62f-e503-4a6c-a02e-52efc6bdc858', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, speakers_and_publishers).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, majority_political_coalitions).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, targeted_minorities).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, hate_speech_recipients).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate under a legal regime that treats speech as categorically protected unless it falls within narrow historical carve-outs (incitement, defamation, obscenity). They benefit from the maximal protected speech set and need not justify speech before publishing. Can litigate exclusions; can exit to other jurisdictions with similar protections. Collective understanding of the First Amendment as an absolute constraint on state power, not a balancing mechanism.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, speakers_and_publishers, beneficiary,
    organized, biographical, arbitrage, national).

% Operate in a constitutional order where their speech is presumptively protected and they need not fear the state weaponizing speech law against their political expression. The absolutist reading protects their speech from the same categorical framework that protects all speech, including speech they oppose. Benefit from the structural symmetry that makes suppression of their own political opponents difficult.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, majority_political_coalitions, beneficiary,
    institutional, generational, mobile, national).

% Experience hate speech, slurs, and dehumanizing rhetoric as categorically protected expression. Bear the psychological and social harm of speech targeting their race, religion, sexual orientation, or national origin without legal recourse to suppress or sanction the speaker. Cannot exit the identity that triggers the targeting; cannot rely on law to provide protection from this category of harm. Must absorb the speech as an externalized cost of a rights regime framed as categorical protection of speakers rather than equal dignitary standing of targets.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, targeted_minorities, payer,
    powerless, biographical, identity_locked, national).

% Interpret the First Amendment text 'Congress shall make no law... abridging the freedom of speech' as stating a categorical prohibition that admits only historical carve-outs (incitement, defamation, obscenity), not contemporary balancing. Set the constitutional agenda by litigating and defending this reading. Treat the text's categorical language as fixed and the scope of protection as presumptively maximal. Defend the reading as legally and textually correct, independent of downstream harms.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, constitutional_originalists, agenda_setter,
    institutional, generational, analytical, national).

% Argue that speech causing demonstrable unconsented-to harm (targeted harassment, threats, incitement to violence, systematic dehumanization) should receive reduced First Amendment protection or be subject to harm-balancing tests. Excluded from the absolutist framework because that framework treats harm as an insufficient reason to restrict speech. Would reshape the constraint if admitted but lack the institutional power to change constitutional doctrine; can organize politically but cannot alter the legal text the reading is grounded in.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, harm_limited_reading_advocates, excluded,
    organized, biographical, constrained, national).

% Advocate for case-by-case balancing of speech value against competing interests (equality, safety, dignity, privacy). Would construct protected/unprotected categories through judicial review rather than via text and history. Excluded from the absolutist reading because that reading treats categorical balancing as a betrayal of the text's categorical language. Operate in jurisdictions where this reading prevails; can litigate to change the standard but do not hold the interpretive authority.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, categorical_balancing_advocates, excluded,
    organized, biographical, constrained, national).

% Operate under constraints imposed by the absolutist reading: cannot restrict speech to protect minorities from hate speech, cannot weaponize speech law against political opponents, cannot balance speaker rights against other constitutional interests without running afoul of the categorical prohibition. Bound by the reading whether or not they endorse it. Analytical seat: they describe what constraints they operate under rather than defending or attacking the reading.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, state_actors, observer,
    institutional, generational, analytical, national).

% Recognize rights to free expression AND rights to freedom from discrimination, requiring speech regulation when speech targets protected categories. Excluded from the U.S. absolutist framework by the constitutional text and institutional commitment; their alternative reading (speech protection conditional on non-discrimination effects) does not apply in the U.S. jurisdictional context. Would reshape the constraint if their authority extended to U.S. constitutional law.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, international_human_rights_frameworks, excluded,
    organized, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(first_amendment_speech_protection__absolutist_reading, speakers_and_publishers).
narrative_ontology:fixing_cost_class(first_amendment_speech_protection__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, predictable legal rule: speech is categorically protected unless it falls within narrow, historically grounded carve-outs (incitement to imminent lawless action, defamation, obscenity). Speakers and publishers need not navigate case-by-case balancing tests; the constraint coordinates on a fixed legal rule rather than ad-hoc judicial judgment.
% TRANSFER_FUNCTION: Transfers the burden of tolerance from speakers to targets. Speakers are freed from legal liability for speech harm; targets must absorb harm without legal redress. The constraint moves the cost of speech-related suffering onto those targeted by hate speech and dehumanizing rhetoric, externalizing that cost as the price of a categorical rights regime.
% ABSENT_VOICES: Targeted minorities whose speech is protected by the same rule but whose dignity and safety are harmed by protected speech targeting them are present in the legal system but excluded from the interpretive consensus: they lack the institutional power to reshape the reading and operate under the constraint rather than inside the decision-making process. International human rights frameworks that balance speech protection against discrimination are structurally excluded by U.S. sovereignty and constitutional commitment.
% DISAPPEARANCE_RATIONALE: If the absolutist reading vanished and a different reading (harm-limited or categorical-balancing) took its place, the First Amendment's scope of protection would contract; some speech presently protected would become regulable; speech law would become more complex and contingent. Speakers would face greater legal risk; targeted minorities would gain some legal recourse. The political economy of speech regulation would reorganize around a different baseline.
% FOUNDING_PROBLEM: Prevent the state from using speech regulation as a tool to suppress political opposition and dissent. The Framers feared that governments would restrict speech to maintain power; a categorical prohibition on speech regulation was designed to make that suppression structurally impossible, protecting the conditions for political self-government.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional originalists and many First Amendment scholars attest the founding problem is still live, citing contemporary government attempts to regulate speech (content moderation requests, platform pressure, viewpoint-based harassment by state actors). Civil rights scholars and harm-limited reading advocates attest the founding problem has been substantially solved in modern democracies (state suppression of dissent is rare; hate speech and targeted harassment are the greater threats to equal political participation). The empirical fact of which risk is greater — state suppression of dissent or private/semi-private speech harm to minorities — is contested; outside corroboration comes from jurisdictional comparison (countries with hate speech restrictions do not systematically revert to authoritarian state suppression of dissent: Canada, UK, Germany, France, Australia all maintain robust protections for political dissent while restricting hate speech).
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__absolutist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(first_amendment_speech_protection__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__absolutist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, ExtMetricName, E),
    domain_priors:suppression_score(first_amendment_speech_protection__absolutist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(first_amendment_speech_protection__absolutist_reading),
    narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(first_amendment_speech_protection__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) is high because the absolutist reading externalizes substantial costs onto targeted minorities without legal recourse, and those costs are uncompensated. The reading is defended as a natural law (constitutional principle, text-based rule) but operates to extract tolerance from vulnerable groups who bear hate-speech harms. Suppression is moderate (0.45) because the constraint does not require active suppression of speech itself — it requires suppression of legal remedies available to targets. Theater is low-to-moderate (0.22) because the constraint is genuinely grounded in constitutional text and historical practice, not entirely performative, but the justifications drift: early articulations emphasized state suppression prevention; contemporary articulations emphasize maximal speaker freedom independent of the state-suppression risk. The measurement series tracks a slow rise in extractiveness from t0 to t30, plateauing thereafter — as social media amplified speech harm and organized targeting became easier, the extractive effect on minorities intensified, but the legal rule remained fixed. Theater ratio rises gradually as defensive-speech-protection rhetoric (silencing prevents dissent) becomes less credible relative to offensive-speech-amplification evidence (platforms amplify hate speech), though the constraint's categorical form never wavers. Suppression requirement is stable: preventing legal remedies for hate-speech targets requires consistent enforcement of the categorical rule, not ramping enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the speaker/publisher seat, the absolutist reading is a genuine protective constraint — it prevents the state from weaponizing speech law and provides predictable legal coverage for expression. From the targeted-minority seat, the same reading is structurally extractive — it denies them equal legal protection and externalizes harm as the cost of a rights regime framed for speakers rather than for equal dignitary standing. The originalist agenda-setter sees the text as settled and categorical; harm-limited advocates see the same text as requiring interpretation responsive to contemporary harms. The engine computes per-seat classifications from the structural data: beneficiary seats (speakers, majority coalitions) will compute toward lower extraction and higher coordination benefit; victim seats (targeted minorities) will compute toward higher extraction and lower coordination benefit. The claim (mountain/natural-law) is independent of these per-seat computations; the metrics describe what the constraint actually does regardless of the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Speakers and publishers benefit from the constraint without running it — they collect enhanced freedom of action. Directionality toward them is low (beneficiary end). Majority political coalitions benefit structurally (the rule that protects all speakers protects them symmetrically and constrains state suppression of their dissent); d is low for them as well. Targeted minorities are the targets — the constraint's categorical protection of hate speech operates directly against their interests; they have no legal recourse and cannot exit the identity that triggers targeting; d is high (target end). Constitutional originalists run the interpretive apparatus and defend the reading; they are agenda-setters but do not directly collect extraction (the benefit flows to speakers). Their power is institutional and their exit is analytical, which sets them apart from pure beneficiaries. Harm-limited and balancing advocates are excluded from the reading's framework — their alternative interpretations do not apply under the absolutist reading, and they lack institutional power to reshape it. State actors are bound observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The absolutist reading is claimed as mountain (natural law, constitutional principle) but the metrics reveal substantial extraction (0.68). This triggers mandatrophy questions: Is the founding problem (state suppression of dissent) still live, or has it been substantially solved such that the categorical rule now persists as protection for harmful speech without corresponding state-suppression risk? The six_questions.founding_problem_status answers 'contested': originalists say suppression risk is live; civil rights scholars say speech harm to minorities is the greater threat. The resolution is empirical: do democracies with hate-speech restrictions systematically revert to authoritarian state suppression of dissent? Cross-jurisdictional evidence (Canada, UK, Germany, etc.) suggests no — hate-speech restrictions coexist with robust political dissent protection. This suggests the founding problem may be dead or substantially solved, yet the constraint persists. If so, the reading is mandatrophe: the categorical rule was built to solve a state-suppression problem; that problem is contested or solved; the constraint now functions to protect speech that harms vulnerable groups without corresponding suppression-prevention value. The classification resists a clean Piton label (the constraint is not primarily performative; it is actively enforced through litigation and doctrine) but carries a mandatrophy signature: founding problem contested/dead, world would rearrange if the reading changed (targeted minorities would gain recourse), yet the reading persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_reading,
    'Is the absolutist reading a discovery of the constitutional text''s true meaning (natural law, fixed principle), or a constructed interpretation that benefits identifiable parties (speakers, publishers, majority coalitions)?',
    'Historico-textual analysis: does the text itself support only the absolutist reading, or can other readings claim equal textual warrant? Beneficiary analysis: do speakers and majority coalitions benefit from this reading in ways they would not under alternative readings? If both alternative readings have textual warrant and the absolutist reading benefits specific parties, the reading is partially constructed.',
    'If constructed, the reading loses the mountain/natural-law classification and becomes a contested constraint (rope or snare depending on the extraction level and whether it benefits the majority). If genuinely fixed by the text, it retains the mountain claim despite high extractiveness (a false summit candidate — see next omega).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_reading, conceptual, 'Whether the absolutist reading is a discovery of the text''s fixed meaning or a constructed interpretation grounded in beneficiary interests.').

omega_variable(
    founding_problem_obsolescence,
    'Is the founding problem (state suppression of dissent) still structurally live in contemporary democracies, or has the threat shifted such that the categorical rule now protects harmful speech without corresponding suppression-prevention value?',
    'Comparative constitutional analysis: do democracies with hate-speech restrictions (Canada, UK, Germany, France, Australia) systematically show greater state suppression of political dissent than the U.S.? Empirical trend analysis: has state suppression of speech-based dissent increased or decreased in recent decades in jurisdictions with categorical speech protection?',
    'If the founding problem is substantially solved (state suppression of dissent is rare in modern democracies, including those with hate-speech restrictions), the constraint is mandatrophe: it persists to protect speech without corresponding prevention-of-suppression value. The classification would shift toward snare or piton (extraction without coordination function). If the founding problem is live, the constraint retains its mountain/coordination grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the state-suppression risk the reading was built to prevent is still live or has been substantially solved.').

omega_variable(
    harm_to_minorities_as_cost,
    'Is the harm borne by targeted minorities (hate speech, dehumanizing rhetoric, absence of legal recourse) a necessary and unavoidable cost of the categorical rule, or is it contingent on how the rule is enforced and balanced against equal-protection principles?',
    'Test whether speech protection could coexist with civil-rights protections against targeted harassment and discrimination (as some harm-limited readings propose). If harm to minorities can be reduced without expanding state suppression of dissent, the harm is contingent, not necessary.',
    'If harm is necessary, the constraint cannot be reformed without abandoning the categorical reading. If harm is contingent, alternative readings (harm-limited, balancing) could achieve the same suppression-prevention goal with lower costs to minorities. The classification of the absolutist reading would not change, but alternative readings become more structurally justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_to_minorities_as_cost, conceptual, 'Whether hate-speech harms to minorities are inherent to categorical speech protection or contingent on how the rule is enforced.').

omega_variable(
    reading_contestation_and_authority,
    'Which reading''s authority grounding is strongest — originalist textualism, judicial precedent, contemporary harm evidence, or consensus among affected communities?',
    'Analyze the institutional power structures supporting each reading: originalist readings have recent Supreme Court support (2022-present); harm-limited readings have legislative support in other democracies and emerging international consensus; balancing readings have deep judicial precedent. The authority question is: which grounding carries the most legitimacy in the U.S. constitutional system?',
    'If originalist textualism is the binding authority, the absolutist reading is locked in place and alternative readings cannot displace it without constitutional amendment. If judicial precedent is the binding authority, subsequent courts can adopt alternative readings. If contemporary harm evidence is binding, the reading would shift toward harm-limited or balancing. The answer determines whether the constraint is stable (textualism) or contestable (precedent, evidence, consensus).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contestation_and_authority, conceptual, 'Which authority structure has the strongest claim to bind the reading in the U.S. constitutional system.').

omega_variable(
    identity_lock_mechanism_for_minorities,
    'Is the identity-locked exit assigned to targeted minorities a structural property of the constraint, or a property of identity itself that the constraint merely activates?',
    'Analyze whether targeted minorities could exit the identity (change race, religion, sexual orientation, national origin) to escape the constraint''s harms. If exit is genuinely impossible (identity is constitutive of personhood), identity-lock is structural. If exit is theoretically possible but practically unthinkable, the suppression is internalized. If exit is possible and some targets do exit (e.g., religious conversion, national relocation), identity-lock is overstated.',
    'If identity-lock is structural, suppression is absolute for targets and the constraint cannot be reformed by making exit easier. If identity-lock is internalized or overstated, the constraint''s suppression can be reduced by changing how identity-targeted harm is legally treated. The finding affects whether the constraint is truly immovable (mountain) for this seat or whether alternative readings could reduce identity-fusion effects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_for_minorities, empirical, 'Whether the identity-lock exit for targeted minorities is a structural feature or an effect of how the constraint is enforced.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__absolutist_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t0, first_amendment_speech_protection__absolutist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(firs_tr_t5, first_amendment_speech_protection__absolutist_reading, theater_ratio, 5, 0.11).
narrative_ontology:measurement(firs_tr_t10, first_amendment_speech_protection__absolutist_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(firs_tr_t15, first_amendment_speech_protection__absolutist_reading, theater_ratio, 15, 0.17).
narrative_ontology:measurement(firs_tr_t20, first_amendment_speech_protection__absolutist_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(firs_tr_t25, first_amendment_speech_protection__absolutist_reading, theater_ratio, 25, 0.21).
narrative_ontology:measurement(firs_tr_t30, first_amendment_speech_protection__absolutist_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(firs_tr_t35, first_amendment_speech_protection__absolutist_reading, theater_ratio, 35, 0.22).

% Extraction over time
narrative_ontology:measurement(firs_be_t0, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(firs_be_t5, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(firs_be_t10, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(firs_be_t15, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(firs_be_t20, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(firs_be_t25, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(firs_be_t30, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(firs_be_t35, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t0, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(firs_su_t5, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(firs_su_t10, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 10, 0.41).
narrative_ontology:measurement(firs_su_t15, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 15, 0.43).
narrative_ontology:measurement(firs_su_t20, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(firs_su_t25, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 25, 0.45).
narrative_ontology:measurement(firs_su_t30, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement(firs_su_t35, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 35, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__absolutist_reading, information_standard).
narrative_ontology:boltzmann_floor_override(first_amendment_speech_protection__absolutist_reading, 0.05).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection__categorical_balancing_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection__harm_limited_reading).

% DUAL FORMULATION NOTE:
% The First Amendment kernel 'no law... abridging the freedom of speech' admits three structurally distinct readings: absolutist (categorical protection with narrow historical carve-outs), categorical-balancing (speech categories via case-by-case judicial review), and harm-limited (speech protection yields when demonstrable unconsented-to harm occurs). Each reading has a different ε (extractiveness), different beneficiary/victim structure, and different claim-to-metrics relationship. This story instantiates the absolutist reading. The sibling stories instantiate the other readings. They are not measurements of the same constraint; they are different constraints grounded in the same text. Network links capture the fact that all three readings are in contest and adoption of one affects the viability of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
