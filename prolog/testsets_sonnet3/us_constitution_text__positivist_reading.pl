% ============================================================================
% CONSTRAINT STORY: us_constitution_text__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__positivist_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: us_constitution_text__positivist_reading
 *   human_readable: Constitutional Validity as Formal-Pedigree Rule (Positivist Reading)
 *   domain: constitutional_law/legal_philosophy
 *
 * SUMMARY:
 *   This story instantiates the positivist reading of the US Constitution
 *   kernel: validity is a function of correct formal enactment (bicameralism
 *   and presentment for statutes; Article V for amendments; valid delegation
 *   and hierarchy for regulations), never a function of moral content or
 *   historical intent. The reading's coordination value is real — it gives
 *   courts a determinate, non-arbitrary test for what counts as law. Its
 *   extraction runs through the same channel: claims that cannot be
 *   translated into a procedurally valid hook are simply not cognizable, no
 *   matter their moral force, and this systematically favors whoever
 *   historically controlled or currently controls the enactment machinery.
 *   The 1868 and 1965 dips in extractiveness track the Reconstruction and
 *   Civil Rights amendments successfully running substantive claims through
 *   the formal Article V/statutory channel, temporarily narrowing the gap
 *   between procedural validity and substantive justice; the modest post-2000
 *   rise tracks renewed gridlock in the amendment process, which widens that
 *   gap again as unenacted claims accumulate.
 *
 * KEY AGENTS:
 *   - judicial_institution_and_bar: agenda_setter/beneficiary (institutional/arbitrage) — administers and profits from the pedigree test's legitimacy
 *   - legislative_and_executive_incumbents: beneficiary (institutional/arbitrage) — entrenches outcomes via correct procedure
 *   - settled_property_and_contract_holders: beneficiary (organized/mobile) — gets predictability from source-fixed validity
 *   - unenacted_substantive_justice_claimants: payer (powerless/trapped) — bears the cost of claims with no procedural hook
 *   - structurally_excluded_minorities_at_founding: payer (powerless/trapped) — bears the ongoing cost of exclusionary-adjacent founding text treated as fully valid
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__positivist_reading, 0.42).
domain_priors:suppression_score(us_constitution_text__positivist_reading, 0.58).
domain_priors:theater_ratio(us_constitution_text__positivist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_text__positivist_reading, "Constitutional Validity as Formal-Pedigree Rule (Positivist Reading)").
narrative_ontology:topic_domain(us_constitution_text__positivist_reading, "constitutional_law/legal_philosophy").

domain_priors:requires_active_enforcement(us_constitution_text__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__positivist_reading, '723e0ed8-bdd4-4079-bacc-cf8ef979be08').
narrative_ontology:cs_kernel_codification('723e0ed8-bdd4-4079-bacc-cf8ef979be08', formalized).
narrative_ontology:cs_authority_grounding('723e0ed8-bdd4-4079-bacc-cf8ef979be08', practice).
narrative_ontology:cs_interpretation_layer_present('723e0ed8-bdd4-4079-bacc-cf8ef979be08').
narrative_ontology:cs_reading_relation('723e0ed8-bdd4-4079-bacc-cf8ef979be08', us_constitution_text__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('723e0ed8-bdd4-4079-bacc-cf8ef979be08', us_constitution_text__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_axiom('723e0ed8-bdd4-4079-bacc-cf8ef979be08', foundational, validity_is_source_not_content).
narrative_ontology:cs_axiom_status(validity_is_source_not_content, holdable).
narrative_ontology:cs_axiom_grounding('723e0ed8-bdd4-4079-bacc-cf8ef979be08', validity_is_source_not_content, conventional).
narrative_ontology:cs_axiom('723e0ed8-bdd4-4079-bacc-cf8ef979be08', secondary, moral_content_is_legally_inert_absent_enactment).
narrative_ontology:cs_axiom_status(moral_content_is_legally_inert_absent_enactment, holdable).
narrative_ontology:cs_axiom_grounding('723e0ed8-bdd4-4079-bacc-cf8ef979be08', moral_content_is_legally_inert_absent_enactment, conventional).
narrative_ontology:cs_reference_frame('723e0ed8-bdd4-4079-bacc-cf8ef979be08', procedural_pedigree_validity).
narrative_ontology:cs_drift_state('723e0ed8-bdd4-4079-bacc-cf8ef979be08', contemporary_gridlock_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('723e0ed8-bdd4-4079-bacc-cf8ef979be08', '').
narrative_ontology:cs_kernel_id(us_constitution_text__positivist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, judicial_institution_and_bar).
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, legislative_and_executive_incumbents).
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, settled_property_and_contract_holders).
narrative_ontology:constraint_victim(us_constitution_text__positivist_reading, unenacted_substantive_justice_claimants).
narrative_ontology:constraint_victim(us_constitution_text__positivist_reading, structurally_excluded_minorities_at_founding).
narrative_ontology:constraint_victim(us_constitution_text__positivist_reading, litigants_relying_on_moral_argument_alone).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Courts and the professional bar administer and police the pedigree test: a norm is law if it was enacted through the correct procedure (bicameralism, presentment, Article V amendment, valid delegation), regardless of its moral content. Judges enforce this by refusing to treat morally compelling but procedurally unenacted claims as constitutional law. The bar's professional authority and the judiciary's institutional legitimacy both rest on being able to say what counts as law independent of what anyone thinks law should say.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, judicial_institution_and_bar, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_text__positivist_reading, judicial_institution_and_bar, beneficiary).

% Officials who hold power through the formally validated process benefit from a rule that makes their occupancy of office and their enacted statutes unchallengeable on pure moral grounds. Because validity tracks pedigree, not content, incumbents who control the enactment machinery (majorities, veto points, appointment power) can entrench outcomes by running them through the correct procedure once, after which substantive objection alone cannot dislodge them.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, legislative_and_executive_incumbents, beneficiary,
    institutional, generational, arbitrage, national).

% Parties who have organized their affairs around existing enacted law get predictability: their title, contracts, and settled expectations cannot be undone by a court substituting its own moral judgment for the procedurally validated text. They can exit specific transactions or jurisdictions but not the background rule itself, which they generally prefer intact.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, settled_property_and_contract_holders, beneficiary,
    organized, generational, mobile, national).

% Litigants and movements whose claim is that a law or practice is unjust, but who cannot point to a formally enacted textual or amendment-based hook, are told the claim is not cognizable as constitutional law no matter how compelling its moral force. Their only lawful path is the amendment or legislative process, which requires supermajorities or incumbent cooperation they typically lack. They cannot exit the legal system and litigate the injustice on moral grounds alone.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, unenacted_substantive_justice_claimants, payer,
    powerless, biographical, trapped, national).

% Groups who were excluded from the enactment process that produced the founding and early amendment text (they could not vote, were counted as property, or were simply absent from the drafting rooms) bear the ongoing cost of a validity rule that treats that procedurally-clean but exclusionary text as fully authoritative regardless of who was absent from its making. Formal correction requires running the same exclusionary-adjacent machinery in reverse via Article V.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, structurally_excluded_minorities_at_founding, payer,
    powerless, civilizational, trapped, national).

% Advocates who bring natural-law or pure-justice arguments before courts operating under the pedigree rule find such arguments treated as legally irrelevant unless translated into a procedurally valid textual hook. They can reformulate as statutory or doctrinal argument (costly, uncertain) or lose; they cannot make the moral claim itself dispositive.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, litigants_relying_on_moral_argument_alone, payer,
    moderate, biographical, constrained, national).

% Jurisprudence scholars analyze whether source-based validity actually explains judicial behavior or is itself a contested normative choice among originalism and living constitutionalism. They do not adjudicate cases but shape how the pedigree rule is defended or challenged in legal education and commentary.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, legal_scholars_and_theorists, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_text__positivist_reading, judicial_institution_and_bar).
narrative_ontology:fixing_cost_class(us_constitution_text__positivist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate, content-independent test for what counts as valid constitutional law, so that officials, courts, and citizens can identify binding law without each dispute collapsing into first-order moral argument — this genuinely solves a coordination problem of legal certainty and non-arbitrary adjudication.
% TRANSFER_FUNCTION: Moves adjudicative authority away from claims of substantive justice and toward whoever controls or historically controlled the formal enactment machinery (legislatures, ratifying conventions, amendment supermajorities), and correspondingly moves the cost of injustice not captured by that machinery onto those who cannot mobilize it.
% ABSENT_VOICES: Groups excluded from the original enactment process (enslaved persons, women, non-property-holders) and contemporary claimants whose grievances have not been translated into enacted text are structurally absent from the pedigree calculus — the rule does not ask whether they were present when validity was fixed, only whether the correct procedure was followed.
% DISAPPEARANCE_RATIONALE: If courts abandoned source-validity and adjudicated purely on moral content, the settled-expectations function of constitutional law would collapse: contracts, property arrangements, and institutional roles secured by procedurally valid enactments would become contestable on substantive grounds at any time, and legislative incumbents would lose the entrenchment value of having run policy through Article I/Article V machinery. Litigation would shift dramatically toward first-order moral argument in every case.
% FOUNDING_PROBLEM: Legal positivism as applied to constitutional interpretation was built to solve the problem of adjudicative arbitrariness — preventing judges from smuggling their own moral or political preferences into constitutional decisions under cover of interpreting 'true' meaning, by anchoring validity to an observable, procedural fact (was this enacted correctly) rather than a contestable evaluative one (is this just).
% FOUNDING_PROBLEM_CORROBORATION: Positivist legal theorists (Hart, and constitutional scholars following him) attest the arbitrariness problem remains live and that pedigree-based validity is the only stable solution. Critical legal scholars and some civil rights historians, writing from outside the positivist tradition, attest that the 'determinacy' the rule delivers is itself a distributive choice — it determinately favors whoever already controlled the enactment process — and that this makes the founding problem statement incomplete rather than resolved. No neutral corroborating source exists outside legal theory itself; the dispute is intramural to jurisprudence.
narrative_ontology:disappearance_verdict(us_constitution_text__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__positivist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_text__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__positivist_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__positivist_reading_tests).
:- end_tests(us_constitution_text__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate rather than high because the positivist rule's coordination function — determinate, non-arbitrary adjudication — is genuine and substantial; it is not zero because the same determinacy mechanically forecloses substantive claims lacking pedigree, and that foreclosure falls asymmetrically on those historically or currently excluded from the enactment process. Suppression (0.58) is meaningfully above extractiveness because the rule's enforcement is categorical: it does not weigh how compelling an unenacted claim is, it simply excludes it from the space of cognizable constitutional argument — that is a structural closing-off of an entire mode of legal argument, not a graduated cost. Accessibility collapse (0.62) reflects that once a court adopts strict source-validity, moral argument alone stops being a viable litigation strategy in that court; resistance (0.55) reflects ongoing, serious jurisprudential contest (positivism vs. natural law, vs. living constitutionalism) rather than settled acceptance.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are those who either administer the pedigree test (judiciary/bar, whose institutional authority depends on law being identifiable independent of moral dispute) or who already hold validated positions (incumbent officials, settled-expectation holders) — the rule protects what they have secured through correct procedure. Victims are claimants whose substantive claim has no available procedural hook, and — with civilizational time horizon — groups excluded from the historical enactment process whose absence the rule does not correct, only perpetuates until run through Article V in reverse. The powerless/trapped exit options for both victim groups reflect that the only lawful remedy (constitutional amendment or new legislation) requires exactly the supermajority coordination they typically lack.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing judicial moral arbitrariness by anchoring validity to observable procedure — remains genuinely live: no jurisdiction has found a stable substitute for some form of pedigree test, and judges do smuggle preference into 'true meaning' claims absent one. This is not mandatrophy in the simple sense of a dead purpose kept alive by inertia. But the tangled_rope classification is warranted because the same procedural anchor that prevents arbitrary judicial moral override also mechanically entrenches whoever controlled the historical enactment process, with no built-in correction for participatory exclusion at that founding moment other than the same high-supermajority machinery. The coordination function and the extraction function are the same mechanism, not two separable parts — that is exactly the tangled_rope signature, not a snare (there is real coordination value, not merely extraction dressed as coordination) and not a mountain (the rule is a chosen jurisprudential commitment, contested by two live sibling readings, not a physical or logical necessity).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    positivist_reading_of_us_constitution_text,
    'This constraint instantiates the positivist reading of the us_constitution_text kernel: validity tracks formal enactment procedure only. The sibling readings (originalist_reading: validity tracks original public meaning at ratification; living_constitutionalist_reading: validity tracks evolving societal application of founding principles) are separate constraints, not alternative measurements of this one. Where is the disagreement located?',
    'The disagreement is located at the test for validity itself, not at any shared empirical fact: positivism asks ''was the correct procedure followed,'' originalism asks ''what did the enacted text mean to the ratifying public,'' and living constitutionalism asks ''what does the founding principle require given contemporary circumstances.'' No single empirical inquiry adjudicates between these — they are different theories of what makes constitutional law valid, held by different judges and schools simultaneously.',
    'A sibling reading would change the victim set: under originalism, victims are those whose claims depart from original public meaning regardless of enactment correctness (a broader payer class including claims that ARE procedurally valid but interpretively novel); under living constitutionalism, the victim set shifts toward those whose settled expectations are disrupted by evolving interpretation, and the beneficiary set shifts toward contemporary substantive-justice claimants. ε and the beneficiary/victim structure authored here are specific to the positivist reading and do not transfer.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(positivist_reading_of_us_constitution_text, conceptual, 'Committer-frame declaration: this story is the positivist reading of the us_constitution_text kernel; sibling readings are separate constraints with different victim structures.').

omega_variable(
    pedigree_test_determinacy_ambiguity,
    'Does the formal-pedigree test actually deliver the determinacy it claims, or does ''correct procedure'' itself require interpretive judgment (was delegation validly authorized, was presentment properly executed) that reintroduces the moral/political discretion positivism claims to exclude?',
    'Empirical survey of hard procedural-validity cases (e.g., disputed quorum calls, questions of valid delegation, contested ratification procedures) to assess whether courts resolve them via genuinely content-independent criteria or via disguised substantive reasoning.',
    'If procedural determinacy is largely illusory, the coordination benefit claimed for this reading is smaller than authored and the classification should move toward snare (coordination story as cover); if procedural determinacy holds even in hard cases, tangled_rope with genuine coordination value is the accurate reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedigree_test_determinacy_ambiguity, empirical, 'Whether positivist procedural determinacy is genuine or a disguised vehicle for substantive judgment.').

omega_variable(
    founding_exclusion_correction_burden,
    'Is the civilizational-timescale cost borne by groups excluded from the original enactment process adequately correctable through the ordinary Article V amendment channel, or does the same supermajority-coordination requirement that produced the original exclusion also block its correction?',
    'Historical analysis of amendment success/failure rates for provisions specifically remedying founding-era exclusion (13th/14th/15th/19th Amendments succeeded; ERA and various reparative proposals failed) against the general amendment success rate, to assess whether exclusion-correcting amendments face a structurally higher bar.',
    'If exclusion-correcting amendments systematically fail at higher rates than baseline, this strengthens the victim-side reading (the rule structurally entrenches founding-era exclusion); if they succeed at comparable rates, the tangled_rope''s extraction component is weaker than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_exclusion_correction_burden, empirical, 'Whether the formal amendment channel adequately corrects founding-era participatory exclusion or merely reproduces its coordination barrier.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__positivist_reading, 1787, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1787, us_constitution_text__positivist_reading, theater_ratio, 1787, 0.1).
narrative_ontology:measurement_basis(us_c_tr_t1787, observed).
narrative_ontology:measurement(us_c_tr_t1868, us_constitution_text__positivist_reading, theater_ratio, 1868, 0.13).
narrative_ontology:measurement_basis(us_c_tr_t1868, observed).
narrative_ontology:measurement(us_c_tr_t1937, us_constitution_text__positivist_reading, theater_ratio, 1937, 0.16).
narrative_ontology:measurement_basis(us_c_tr_t1937, observed).
narrative_ontology:measurement(us_c_tr_t1965, us_constitution_text__positivist_reading, theater_ratio, 1965, 0.18).
narrative_ontology:measurement_basis(us_c_tr_t1965, observed).
narrative_ontology:measurement(us_c_tr_t2000, us_constitution_text__positivist_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement_basis(us_c_tr_t2000, observed).
narrative_ontology:measurement(us_c_tr_t2024, us_constitution_text__positivist_reading, theater_ratio, 2024, 0.22).
narrative_ontology:measurement_basis(us_c_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1787, us_constitution_text__positivist_reading, base_extractiveness, 1787, 0.55).
narrative_ontology:measurement_basis(us_c_be_t1787, observed).
narrative_ontology:measurement(us_c_be_t1868, us_constitution_text__positivist_reading, base_extractiveness, 1868, 0.48).
narrative_ontology:measurement_basis(us_c_be_t1868, observed).
narrative_ontology:measurement(us_c_be_t1937, us_constitution_text__positivist_reading, base_extractiveness, 1937, 0.44).
narrative_ontology:measurement_basis(us_c_be_t1937, observed).
narrative_ontology:measurement(us_c_be_t1965, us_constitution_text__positivist_reading, base_extractiveness, 1965, 0.4).
narrative_ontology:measurement_basis(us_c_be_t1965, observed).
narrative_ontology:measurement(us_c_be_t2000, us_constitution_text__positivist_reading, base_extractiveness, 2000, 0.41).
narrative_ontology:measurement_basis(us_c_be_t2000, observed).
narrative_ontology:measurement(us_c_be_t2024, us_constitution_text__positivist_reading, base_extractiveness, 2024, 0.42).
narrative_ontology:measurement_basis(us_c_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1787, us_constitution_text__positivist_reading, suppression_requirement, 1787, 0.68).
narrative_ontology:measurement_basis(us_c_su_t1787, observed).
narrative_ontology:measurement(us_c_su_t1868, us_constitution_text__positivist_reading, suppression_requirement, 1868, 0.62).
narrative_ontology:measurement_basis(us_c_su_t1868, observed).
narrative_ontology:measurement(us_c_su_t1937, us_constitution_text__positivist_reading, suppression_requirement, 1937, 0.58).
narrative_ontology:measurement_basis(us_c_su_t1937, observed).
narrative_ontology:measurement(us_c_su_t1965, us_constitution_text__positivist_reading, suppression_requirement, 1965, 0.53).
narrative_ontology:measurement_basis(us_c_su_t1965, observed).
narrative_ontology:measurement(us_c_su_t2000, us_constitution_text__positivist_reading, suppression_requirement, 2000, 0.56).
narrative_ontology:measurement_basis(us_c_su_t2000, observed).
narrative_ontology:measurement(us_c_su_t2024, us_constitution_text__positivist_reading, suppression_requirement, 2024, 0.58).
narrative_ontology:measurement_basis(us_c_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__positivist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_text__positivist_reading, originalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__positivist_reading, living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% This constraint, originalist_reading, and living_constitutionalist_reading are three readings of the same kernel (us_constitution_text): a single persisting commitment (the constitutional text and its claim to supreme authority) that different interpretive communities read differently, each instantiating a structurally distinct constraint with its own epsilon, beneficiary/victim structure, and classification. This reading (positivist) locates validity in procedural pedigree; originalist_reading locates it in original public meaning; living_constitutionalist_reading locates it in evolving application of founding principle. None of the three ε values should be averaged or reconciled — each is the intrinsic property of a distinct reading-indexed constraint, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
