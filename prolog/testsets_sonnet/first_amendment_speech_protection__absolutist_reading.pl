% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: first_amendment_speech_protection__absolutist_reading
 *   human_readable: First Amendment Absolutist Reading — Categorical Textual Protection
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the absolutist reading of the First Amendment
 *   speech-protection kernel: the text 'Congress shall make no law...
 *   abridging the freedom of speech' is read as categorical, admitting only a
 *   small, historically fixed set of exclusions (true threats, incitement to
 *   imminent lawless action, obscenity, fighting words, defamation) and
 *   rejecting any general judicial balancing of speech value against harm.
 *   Since Brandenburg v. Ohio (1969) narrowed the incitement exception to
 *   imminent lawless action, the categorical reading has become the operative
 *   doctrine in much of U.S. First Amendment law, particularly for
 *   content-based and viewpoint-based restrictions. This is a genuine
 *   coordination mechanism — it prevents official discretion to punish
 *   disfavored speech — but it also structurally externalizes the costs of
 *   tolerating harmful expression onto whichever groups become targets,
 *   without a doctrinal mechanism to weigh that harm against the speech's
 *   value. It is a hybrid: the coordination function (protecting dissent from
 *   official suppression) is real, but so is the asymmetric extraction
 *   (targeted minorities bear costs the doctrine has no mechanism to
 *   recognize), and the arrangement persists only because courts actively
 *   strike down content-based regulation whenever legislatures attempt it —
 *   hence tangled_rope rather than a clean rope or mountain.
 *
 * KEY AGENTS:
 *   - dominant_speakers: primary beneficiary (powerful/arbitrage) — protected regardless of harm caused
 *   - political_majorities: institutional beneficiary (institutional/arbitrage) — insulated from minority-protective legislation
 *   - targeted_racial_minorities: primary target (powerless/trapped) — bears organized hate speech as protected activity
 *   - vulnerable_speech_recipients: diffuse target (powerless/trapped) — absorbs background costs with no standing
 *   - supreme_court: agenda-setter (institutional/analytical) — enforces the categorical line against legislative deviation
 *   - civil_liberties_litigators: agenda-setter (organized/mobile) — professionally and institutionally invested in the doctrine's persistence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__absolutist_reading, 0.42).
domain_priors:suppression_score(first_amendment_speech_protection__absolutist_reading, 0.35).
domain_priors:theater_ratio(first_amendment_speech_protection__absolutist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__absolutist_reading, "First Amendment Absolutist Reading — Categorical Textual Protection").
narrative_ontology:topic_domain(first_amendment_speech_protection__absolutist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__absolutist_reading, '4c7872b1-aeff-4fb5-97b0-a282a977e3b3').
narrative_ontology:cs_kernel_codification('4c7872b1-aeff-4fb5-97b0-a282a977e3b3', fixed_text).
narrative_ontology:cs_authority_grounding('4c7872b1-aeff-4fb5-97b0-a282a977e3b3', lineage).
narrative_ontology:cs_interpretation_layer_present('4c7872b1-aeff-4fb5-97b0-a282a977e3b3').
narrative_ontology:cs_reading_relation('4c7872b1-aeff-4fb5-97b0-a282a977e3b3', first_amendment_speech_protection__harm_limited_reading, coexists_with).
narrative_ontology:cs_reading_relation('4c7872b1-aeff-4fb5-97b0-a282a977e3b3', first_amendment_speech_protection__categorical_balancing_reading, influences).
narrative_ontology:cs_axiom('4c7872b1-aeff-4fb5-97b0-a282a977e3b3', foundational, categorical_text_admits_no_general_balancing).
narrative_ontology:cs_axiom_status(categorical_text_admits_no_general_balancing, holdable).
narrative_ontology:cs_axiom_grounding('4c7872b1-aeff-4fb5-97b0-a282a977e3b3', categorical_text_admits_no_general_balancing, conventional).
narrative_ontology:cs_axiom('4c7872b1-aeff-4fb5-97b0-a282a977e3b3', secondary, externalized_minority_harm_is_liberty_cost_not_constitutional_injury).
narrative_ontology:cs_axiom_status(externalized_minority_harm_is_liberty_cost_not_constitutional_injury, holdable).
narrative_ontology:cs_axiom_grounding('4c7872b1-aeff-4fb5-97b0-a282a977e3b3', externalized_minority_harm_is_liberty_cost_not_constitutional_injury, instrumental).
narrative_ontology:cs_reference_frame('4c7872b1-aeff-4fb5-97b0-a282a977e3b3', brandenburg_categorical_settlement).
narrative_ontology:cs_drift_state('4c7872b1-aeff-4fb5-97b0-a282a977e3b3', contemporary_platform_speech_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('4c7872b1-aeff-4fb5-97b0-a282a977e3b3', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, dominant_speakers).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, political_majorities).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, media_and_publishing_institutions).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, civil_liberties_litigators).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, targeted_racial_minorities).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, targeted_religious_minorities).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, harassment_targets_in_public_forums).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, vulnerable_speech_recipients).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__absolutist_reading, textual_originalism).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__absolutist_reading, categorical_free_speech_doctrine).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__absolutist_reading, marketplace_of_ideas_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Occupy the loudest platforms and largest audiences; the categorical reading protects their speech from content-based restriction regardless of downstream harm to targeted groups. They can say what they wish with almost no risk of legal liability, and they invoke the absolutist text whenever regulation is proposed.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, dominant_speakers, beneficiary,
    powerful, civilizational, arbitrage, national).

% Control legislatures and courts across election cycles; the categorical rule keeps hate-speech, harassment, and disinformation regulation off the table as a matter of doctrine, insulating majoritarian rhetoric from minority-protective legislation that a balancing regime would permit.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, political_majorities, beneficiary,
    institutional, generational, arbitrage, national).

% Rely on maximal categorical protection to publish provocative, defamatory-adjacent, or harm-adjacent content with minimal editorial liability exposure, and lobby aggressively against any move toward the harm-limited or balancing readings.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, media_and_publishing_institutions, beneficiary,
    organized, generational, arbitrage, national).

% Bring and win the cases that entrench the categorical rule; their institutional identity and professional mission are built around defending speech absolutism, and they administer the doctrine's boundaries case by case even while claiming it is simply 'the text.'
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, civil_liberties_litigators, agenda_setter,
    organized, civilizational, mobile, national).

% Bear cross burnings, racist rallies, and organized hate speech as constitutionally protected activity under the categorical rule; have no legal recourse against the speech itself and must absorb the psychological, reputational, and sometimes physical safety costs as the price of the majority's liberty.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, targeted_racial_minorities, payer,
    powerless, biographical, trapped, local).

% Face protected picketing, defamatory religious caricature, and organized vilification campaigns; the categorical reading treats these as core protected expression, leaving communities to self-fund counter-speech or relocation as their only remedies.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, targeted_religious_minorities, payer,
    powerless, biographical, trapped, local).

% Endure sustained public harassment campaigns that fall short of narrow historical exclusions like true threats or incitement; the categorical line is drawn upstream of most of what they experience as harm, so the law offers no intervention until the conduct escalates far beyond speech alone.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, harassment_targets_in_public_forums, payer,
    powerless, immediate, constrained, local).

% Children, institutionalized persons, and captive audiences absorb the externalized costs of maximal protection in schools, workplaces, and public spaces; they have essentially no voice in doctrinal formation and are treated as bearing a background cost rather than as parties with standing to be weighed.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, vulnerable_speech_recipients, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(first_amendment_speech_protection__absolutist_reading, vulnerable_speech_recipients, excluded).

% The historically fixed carve-outs (true threats, incitement to imminent lawless action, obscenity, defamation, fighting words) that the absolutist reading treats as the ONLY legitimate limits; their narrowness is precisely what makes the categorical rule categorical rather than a case-by-case balance.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, narrow_exclusion_categories, observer,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(first_amendment_speech_protection__absolutist_reading, narrow_exclusion_categories).

% Adjudicates which historical exclusions count as sufficiently narrow and enforces the categorical rule against legislative attempts to regulate speech based on content or viewpoint; its rulings are what give the textual claim actual coercive force against contrary state and federal law.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(first_amendment_speech_protection__absolutist_reading, supreme_court, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(first_amendment_speech_protection__absolutist_reading, dominant_speakers).
narrative_ontology:fixing_cost_class(first_amendment_speech_protection__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bright-line, predictable rule that prevents government officials and shifting political majorities from suppressing disfavored viewpoints on a case-by-case basis, protecting dissenters, minorities, and unpopular speakers from majoritarian censorship in the paradigm case.
% TRANSFER_FUNCTION: Moves the cost of tolerating harmful, degrading, or threatening expression from the state (which cannot regulate it) onto the individuals and communities targeted by that expression, who must absorb reputational, psychological, and safety costs as the price of categorical protection for everyone else.
% ABSENT_VOICES: Targeted minorities, harassment victims, and vulnerable speech recipients rarely appear as parties in the foundational doctrinal cases that establish the categorical rule; the doctrine was substantially built through cases brought by and on behalf of powerful, well-resourced speakers (publishers, political dissidents with institutional backing, organized advocacy groups) rather than by those bearing its downstream costs.
% DISAPPEARANCE_RATIONALE: If the categorical reading disappeared and a balancing or harm-limited regime took over, legislatures would gain doctrinal room to regulate hate speech, harassment, and targeted vilification; media and advocacy organizations would face new liability exposure; and enforcement of speech-adjacent harms would shift from being categorically foreclosed to being litigated case by case — a substantial rearrangement of political and legal practice.
% FOUNDING_PROBLEM: Colonial and early American governments used seditious libel prosecutions, licensing requirements, and ad hoc suppression to silence political dissent and unpopular religious views; the categorical text was meant to remove discretion from officials who could otherwise punish speech they disliked under any pretext.
% FOUNDING_PROBLEM_CORROBORATION: Civil liberties organizations and originalist scholars attest the founding problem remains fully live — official discretion to punish disfavored speech has not disappeared and any softening invites its return. Civil rights scholars, critical race theorists, and comparative constitutional scholars examining peer democracies with narrower protections attest that the founding problem (arbitrary official suppression of dissent) has been substantially addressed by modern doctrine's other safeguards, and that the residual categorical absolutism now primarily serves to block minority-protective regulation rather than to prevent the original evil; this corroboration comes from outside the free-speech-litigation community that benefits from the current doctrine.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__absolutist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(first_amendment_speech_protection__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__absolutist_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__absolutist_reading_tests).
:- end_tests(first_amendment_speech_protection__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than extreme because the categorical rule genuinely protects dissenters and minorities in the paradigm case (the coordination function is real, not a fig leaf) — but it has risen over the historical interval (0.22 to 0.42) as the doctrine hardened from Schenck-era clear-and-present-danger balancing into the narrow, fixed Brandenburg exclusions, systematically closing off harm-based regulation that earlier, less absolutist doctrine would have permitted. Suppression is moderate and has fallen over time (0.50 to 0.35) as courts have grown more confident in the categorical rule and less reliant on ad hoc emergency justifications — the rule now enforces itself through settled precedent rather than active case-by-case suppression of alternatives. Theater ratio is low but rising slightly (0.10 to 0.22): most enforcement is functional (actual case law, actual injunctions against speech-restrictive statutes), but some invocation of the categorical text in contemporary disputes over platform speech and campus speech codes is performative signaling rather than doctrinally necessary.
 *
 * PERSPECTIVAL GAP:
 *   From the dominant speaker's seat, this is nearly indistinguishable from a pure Rope: a neutral rule that protects everyone equally and happens to protect them most because they speak most. From the targeted minority's seat, the same rule computes as extraction: a categorical shield that was built and is maintained by powerful speakers and litigators, which forecloses exactly the legislative remedies that would address the harms they experience. The engine's per-seat computation should diverge sharply between these two positions even though both parties are looking at the identical doctrinal text.
 *
 * DIRECTIONALITY LOGIC:
 *   Dominant speakers and political majorities are near the full-beneficiary end: they hold arbitrage-level exit (they can simply speak, litigate, or legislate around any attempted restriction) and the doctrine subsidizes their expression regardless of downstream cost. Targeted minorities and vulnerable recipients sit near the full-target end: trapped exit (they cannot leave the jurisdiction or the public forum easily), powerless structural position, and the doctrine offers no mechanism to recognize their harm as cognizable. Civil liberties litigators and the Supreme Court are agenda-setters whose institutional and professional identity is fused with the doctrine's persistence — this is not simple beneficiary capture but identity-lock: the litigators' career paths and worldview are constituted by absolutist advocacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (arbitrary official censorship of political and religious dissent) remains partially live — governments still attempt content-based suppression — which is why this is not simply a dead mandate maintained by inertia (that would be a piton). But the doctrine's categorical rigidity has outrun its founding justification in the specific domain of targeted harassment and hate speech, where the modern harm is qualitatively different from 18th- and 19th-century seditious libel prosecutions. Classifying this as tangled_rope rather than snare or mountain preserves the genuine coordination function (protection against official discretion) while still registering the asymmetric extraction (externalized minority harm) that a pure Rope or Mountain classification would erase.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_line_naturalness,
    'Is the boundary between ''core protected speech'' and ''narrow historical exclusion'' a discovered constitutional fact, or a constructed doctrinal choice that could have been drawn differently (as the sibling balancing and harm-limited readings demonstrate it was, at other points in the Court''s own history)?',
    'Comparative doctrinal history: trace how the boundary moved across Schenck (1919), Dennis (1951), Brandenburg (1969), and R.A.V. (1992) — if the line has moved substantially in response to political and social pressure rather than converging on a fixed textual meaning, that supports the constructed reading.',
    'If the categorical line is constructed rather than discovered, the absolutist reading''s claim to be simply ''following the text'' is itself a rhetorical move that obscures an ongoing extraction-favoring choice, strengthening the tangled_rope classification over any mountain-adjacent reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_line_naturalness, conceptual, 'Whether the categorical exclusion boundary is discovered or constructed.').

omega_variable(
    kernel_committer_disagreement_location,
    'Where exactly does the absolutist reading diverge from the harm_limited_reading and categorical_balancing_reading — is it a disagreement about constitutional text, about the empirical magnitude of speech-caused harm, or about which institution (courts vs. legislatures) should be trusted to weigh harm against liberty?',
    'Doctrinal genealogy comparing the reasoning in absolutist opinions (Brandenburg, Cohen v. California) against balancing-era opinions (Dennis, Chaplinsky) to isolate whether the disagreement is textual, empirical, or institutional-trust-based.',
    'If the disagreement is primarily institutional-trust-based (distrust of legislative and executive discretion) rather than textual, the absolutist reading''s textualist framing (''no law means no law'') is doing rhetorical work beyond what the underlying institutional-trust argument would justify on its own — this would strengthen the case that the reading functions partly as legitimating cover for a policy preference.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_committer_disagreement_location, conceptual, 'Locating where the three sibling readings actually diverge — text, empirics, or institutional trust.').

omega_variable(
    false_summit_naturalness_beneficiary_tension,
    'This reading is not authored as a mountain and does not claim emerges_naturally, but the underlying rhetorical move (''the text simply says no law'') often functions in public discourse as a mountain-claim — a natural, inevitable reading rather than a contested judicial choice among live alternatives. Is the doctrine''s own self-presentation understating its constructedness?',
    'Survey how the doctrine is taught and invoked in legal education and public commentary versus how professional constitutional scholars across the political spectrum characterize its contestedness.',
    'If the doctrine is publicly presented as more natural/inevitable than professional scholarship treats it, that gap is itself evidence of legitimation work being done beyond the doctrine''s actual epistemic status — relevant to how much weight the beneficiary-serving framing should carry in classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_summit_naturalness_beneficiary_tension, conceptual, 'Gap between public natural-law framing and professional recognition of doctrinal contestedness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__absolutist_reading, 1919, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t1919, first_amendment_speech_protection__absolutist_reading, theater_ratio, 1919, 0.1).
narrative_ontology:measurement(firs_tr_t1940, first_amendment_speech_protection__absolutist_reading, theater_ratio, 1940, 0.12).
narrative_ontology:measurement(firs_tr_t1969, first_amendment_speech_protection__absolutist_reading, theater_ratio, 1969, 0.15).
narrative_ontology:measurement(firs_tr_t1990, first_amendment_speech_protection__absolutist_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(firs_tr_t2010, first_amendment_speech_protection__absolutist_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(firs_tr_t2024, first_amendment_speech_protection__absolutist_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(firs_be_t1919, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 1919, 0.22).
narrative_ontology:measurement(firs_be_t1940, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 1940, 0.28).
narrative_ontology:measurement(firs_be_t1969, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 1969, 0.31).
narrative_ontology:measurement(firs_be_t1990, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 1990, 0.36).
narrative_ontology:measurement(firs_be_t2010, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement(firs_be_t2024, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t1919, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 1919, 0.5).
narrative_ontology:measurement(firs_su_t1940, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 1940, 0.45).
narrative_ontology:measurement(firs_su_t1969, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 1969, 0.38).
narrative_ontology:measurement(firs_su_t1990, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 1990, 0.36).
narrative_ontology:measurement(firs_su_t2010, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 2010, 0.35).
narrative_ontology:measurement(firs_su_t2024, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__absolutist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(first_amendment_speech_protection__absolutist_reading, 0.12).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, harm_limited_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, categorical_balancing_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the first_amendment_speech_protection kernel, decomposed per the ε-invariance principle: absolutist_reading (this story, ε=0.42, tangled_rope), harm_limited_reading (separate story, expected higher ε given broader harm-recognition but narrower protected set), and categorical_balancing_reading (separate story, case-by-case weighing). Each reading produces a structurally different beneficiary/victim allocation from the same constitutional text, which is why they are authored as three separate constraint stories rather than one story with a measurement parameter. All three must remain mutually linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
