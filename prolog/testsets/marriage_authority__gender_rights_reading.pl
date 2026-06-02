% ============================================================================
% CONSTRAINT STORY: marriage_authority__gender_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__gender_rights_reading, []).

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
 *   constraint_id: marriage_authority__gender_rights_reading
 *   human_readable: Marriage Authority Under Gender Rights Constitutional Reading
 *   domain: legal_pluralism/constitutional_law/family_law
 *
 * SUMMARY:
 *   This constraint instantiates the GENDER RIGHTS READING of the contested
 *   marriage authority kernel. The reading claims that constitutional
 *   guarantees of equality (Articles 14-15 in the Indian context, analogous
 *   provisions in other pluralist democracies) mandate judicial reform of
 *   personal law provisions that embed patriarchal extraction — specifically
 *   unilateral talaq, unequal inheritance, contingent maintenance, and
 *   no-fault divorce asymmetries. The reading is not a claim that personal
 *   law should be abolished, but that constitutional equality requires
 *   reformation of the kernel (marriage authority) to include gender
 *   protections. This differs structurally from the secularist reading (which
 *   seeks to eliminate personal law entirely) and the communal autonomy
 *   reading (which treats gender hierarchy as intrinsic to cultural
 *   identity). The gender rights reading occupies the middle ground: it
 *   claims constitutional equality can be accommodated within personal law
 *   traditions through doctrinal reform. The constraint exhibits two opposing
 *   extraction dynamics: (1) women within patriarchal personal law bear
 *   extraction (unequal marital rights, identity-locked exit), and (2)
 *   communal authorities experience extraction from judicial authority
 *   expansion (loss of autonomous adjudicatory power). The reading
 *   simultaneously targets (extracts from) patriarchal personal law
 *   provisions while being constrained by (extracted from by) traditional
 *   communal authorities and secularist impatience. The theater ratio decline
 *   (0.62 → 0.55) reflects that as judicial doctrine matures, the
 *   performative aspects decrease — courts develop clearer tests, explicit
 *   standards for gender equality compliance, and measurable criteria rather
 *   than case-by-case discretion. The extractiveness rise (0.45 → 0.68)
 *   reflects increasing pressure on communal authorities to comply with
 *   judicial directives, and increasing cost to women who remain within
 *   unreformed personal law jurisdictions.
 *
 * KEY AGENTS:
 *   - Women Within Patriarchal Personal Law (powerless/identity_locked): Primary victims. Structurally mobile (legal protections available, geographic mobility possible) but identity-fused with marital and communal roles. Unilateral talaq, limited property rights, maintenance contingency, no-fault divorce unavailability constitute the extraction mechanism.
 *   - Women's Rights Advocates & Constitutional Equality Doctrine Supporters (institutional/arbitrage): Primary beneficiaries. Expand authority through judicial expansion; benefit from framing gender rights as constitutional mandate. Include progressive judges, civil rights lawyers, women's organizations aligned with secular constitutional order.
 *   - Personal Law Community Authorities (organized/constrained): Secondary victims of judicial authority extraction; beneficiaries from maintenance of patriarchal internal structure. Experience the gender rights reading as illegitimate intervention in communal self-governance.
 *   - Constitutional Court Authority (institutional/arbitrage): Meta-beneficiary. Expands institutional reach; claims authority from constitutional equality doctrine; experiences constraint as legitimate jurisdiction expansion.
 *   - Secularist Reform Coalition (organized/constrained): Constrained allies of the gender rights reading. See personal law itself as indefensible; forced to work within pluralist framework they reject.
 *   - Colonial-Era Legal Pluralism Framework (institutional/arbitrage): Institutional substrate. Persists through path dependency; gender rights reading accelerates its degradation by treating it as reformable rather than immutable.
 *   - Analytical Observer: Views the constraint from the position of inherent pluralism tensions (risks naturalizing patriarchal extraction as inevitable cost of respecting cultural diversity).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__gender_rights_reading, 0.68).
domain_priors:suppression_score(marriage_authority__gender_rights_reading, 0.75).
domain_priors:theater_ratio(marriage_authority__gender_rights_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__gender_rights_reading, snare).
narrative_ontology:human_readable(marriage_authority__gender_rights_reading, "Marriage Authority Under Gender Rights Constitutional Reading").
narrative_ontology:topic_domain(marriage_authority__gender_rights_reading, "legal_pluralism/constitutional_law/family_law").

domain_priors:requires_active_enforcement(marriage_authority__gender_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__gender_rights_reading, '0bd5fbda-29cf-40cf-ae2c-d7c565684fa9').
narrative_ontology:cs_kernel_codification('0bd5fbda-29cf-40cf-ae2c-d7c565684fa9', fixed_text).
narrative_ontology:cs_authority_grounding('0bd5fbda-29cf-40cf-ae2c-d7c565684fa9', extraction).
narrative_ontology:cs_interpretation_layer_present('0bd5fbda-29cf-40cf-ae2c-d7c565684fa9').
narrative_ontology:cs_reading_relation('0bd5fbda-29cf-40cf-ae2c-d7c565684fa9', marriage_authority__communal_autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('0bd5fbda-29cf-40cf-ae2c-d7c565684fa9', marriage_authority__secularist_reading, coexists_with).
narrative_ontology:cs_reading_relation('0bd5fbda-29cf-40cf-ae2c-d7c565684fa9', marriage_authority__federalist_millet_reading, influences).
narrative_ontology:cs_reading_relation('0bd5fbda-29cf-40cf-ae2c-d7c565684fa9', marriage_authority__judicial_harmonization_reading, coexists_with).
narrative_ontology:cs_axiom('0bd5fbda-29cf-40cf-ae2c-d7c565684fa9', foundational, constitutional_equality_constrains_all_law_domains).
narrative_ontology:cs_axiom_status(constitutional_equality_constrains_all_law_domains, holdable).
narrative_ontology:cs_axiom_grounding('0bd5fbda-29cf-40cf-ae2c-d7c565684fa9', constitutional_equality_constrains_all_law_domains, deontological).
narrative_ontology:cs_axiom('0bd5fbda-29cf-40cf-ae2c-d7c565684fa9', foundational, patriarchal_provisions_constitute_unjust_extraction).
narrative_ontology:cs_axiom_status(patriarchal_provisions_constitute_unjust_extraction, holdable).
narrative_ontology:cs_axiom_grounding('0bd5fbda-29cf-40cf-ae2c-d7c565684fa9', patriarchal_provisions_constitute_unjust_extraction, deontological).
narrative_ontology:cs_reference_frame('0bd5fbda-29cf-40cf-ae2c-d7c565684fa9', constitutional_equality_mandated_pluralism).
narrative_ontology:cs_drift_state('0bd5fbda-29cf-40cf-ae2c-d7c565684fa9', contemporary_post_feminist_jurisprudence_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0bd5fbda-29cf-40cf-ae2c-d7c565684fa9', '').
narrative_ontology:cs_kernel_id(marriage_authority__gender_rights_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, women_rights_advocates).
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, constitutional_equality_doctrine).
narrative_ontology:constraint_victim(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law).
narrative_ontology:constraint_victim(marriage_authority__gender_rights_reading, communal_autonomy_claims).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WOMAN SUBJECT TO PATRIARCHAL PERSONAL LAW (SNARE) — Identity fused with communal and marital role; exit from the marriage/personal law regime requires abandoning family identity, community membership, and religious identity simultaneously. Structurally mobile (could relocate, has legal protections in secular law) but identity-locked in the patriarchal framework. Experiences maximum extraction: unilateral talaq, limited inheritance, maintenance contingent on male discretion, no-fault divorce unavailable. No exit mechanism; suppression internalized through religious and cultural identity frame.
constraint_indexing:constraint_classification(marriage_authority__gender_rights_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: JUDICIAL EQUALITY DOCTRINE COALITION (ROPE) — Progressive judges, constitutional scholars, women's rights organizations. See constitutional equality guarantees (Articles 14-15) as a coordination mechanism that solves a real problem: how to protect individual rights while respecting pluralism. This reading treats gender rights within personal law as a coordination problem, not pure extraction. Benefits from the constraint insofar as judicial authority expands; constrained by need to maintain legitimacy with communal authorities and traditional institutions.
constraint_indexing:constraint_classification(marriage_authority__gender_rights_reading, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PERSONAL LAW COMMUNITY AUTHORITY (TANGLED ROPE) — Religious scholars, community councils (khap panchayats, halakha courts, etc.). Experience the gender rights reading as an external extraction: judicial overreach removes their authority to adjudicate family matters. But also coordinate internal legitimacy: must maintain some appearance of fairness within patriarchal framework to retain allegiance. Genuine coordination function exists (dispute resolution, community norm-setting) alongside asymmetric extraction (judges seizing authority, communal rules losing force).
constraint_indexing:constraint_classification(marriage_authority__gender_rights_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SECULARIST REFORM COALITION (SNARE) — Civil rights lawyers, state equality advocates. See personal law itself as the snare mechanism; gender rights reading is the only exit available. Constrained by need to maintain judicial coalition; cannot openly reject communal legitimacy without fracturing the equality doctrine. Experiences extraction in being forced to work within a pluralist framework that they see as indefensible.
constraint_indexing:constraint_classification(marriage_authority__gender_rights_reading, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: COLONIAL-ERA LEGAL PLURALISM FRAMEWORK (PITON) — The institutional structure (millet system, personal law recognition) that the gender rights reading seeks to reform is itself a degraded institution. It persists through path dependency and habit rather than functional need. Theater ratio reflects that the framework claims to 'respect pluralism' while actually preserving patriarchal extraction. The framework's authority has atrophied; the gender rights reading accelerates this decay by filling the legitimacy gap with constitutional doctrine.
constraint_indexing:constraint_classification(marriage_authority__gender_rights_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CONSTITUTIONAL COURT AUTHORITY (TANGLED ROPE) — Benefits from expanded authority over family law; sees this as legitimate extension of constitutional equality mandate. But must maintain coordination function: legitimacy depends on being perceived as neutral arbiter, not as agent of secularism. Genuine coordination (adjudicating conflicting rights claims) embedded in asymmetric extraction (claiming authority from personal law sphere).
constraint_indexing:constraint_classification(marriage_authority__gender_rights_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / LEGAL PLURALISM NATURAL LAW (MOUNTAIN) — From civilizational scope, some tension between individual rights and communal authority is inherent to pluralist systems. This perspective naturalizes the patriarchal-extraction feature as an immutable property of cultural pluralism: 'communities will always have internal gender hierarchies, and respecting pluralism means accepting this.' However, the structural data contradicts mountain classification — identifiable beneficiaries (traditional authorities) and clear extraction mechanisms show this is a false summit, not a law of nature.
constraint_indexing:constraint_classification(marriage_authority__gender_rights_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__gender_rights_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(marriage_authority__gender_rights_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(marriage_authority__gender_rights_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__gender_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(marriage_authority__gender_rights_reading, TR),
    TR >= 0.70.

:- end_tests(marriage_authority__gender_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68, high): The gender rights reading produces substantial effective extraction because it operates via judicial authority expansion in a domain traditionally governed by communities. Women within patriarchal personal law experience high extraction (unilateral talaq, limited inheritance, maintenance contingency, no-fault divorce unavailability). Communal authorities experience extraction from loss of autonomous adjudicatory power. The extractiveness is not as severe as pure snare (0.80+) because the reading's stated goal is reformation, not elimination, of personal law; and because women have some structural mobility (legal protections in secular law, exit available at cost). However, the reading's operative mechanism in practice is often expansion of judicial authority at the expense of communal authority, without simultaneous resolution of identity-lock dynamics for women. Suppression (0.75, high): Substantial suppressive force operates at three levels: (1) direct suppression of women via patriarchal personal law provisions, (2) suppression of alternative gender-reform framings (community-internal reform, accommodation approaches) by judicial dominance, (3) internalized suppression through identity-lock (women cannot perceive exit from within patriarchal identity frame). Suppression rises over time (0.68 → 0.75) as judicial doctrine hardens and communal authorities respond with hardening of traditional provisions to resist reform. Theater ratio (0.55, moderate, declining): The reading has relatively lower theater than other institutional constraints because judicial doctrine must produce concrete legal tests (e.g., for what constitutes 'cruelty' as grounds for divorce, what property shares are required). As the doctrine matures, the performative aspects decrease — judges issue explicit standards rather than case-by-case discretion. The decline reflects increasing specificity of equality doctrine, not elimination of theatrical elements (the theater persists in communal authorities' performative resistance and in the gap between judicial doctrine and actual practice in communities).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival gaps across institutional and individual power levels. The woman subject to patriarchal personal law (powerless/identity_locked) perceives the constraint as maximum extraction (snare), with no exit available that does not require abandoning her identity. The personal law community authority (organized/constrained) perceives the constraint as extraction of their adjudicatory power by the state (tangled_rope or snare from their position), while seeing internal patriarchal structure as legitimate coordination. The constitutional court (institutional/arbitrage) perceives the constraint as legitimate authority expansion and sees themselves as solving a coordination problem (tangled_rope). The gender rights advocate (institutional/arbitrage) perceives snare (patriarchal personal law) being targeted by their reading, but may not perceive the extraction they themselves inflict on communal authority. The analytical observer at the civilizational level risks seeing inherent pluralism tension (mountain) rather than contingent institutional arrangement. The secularist coalition (organized/constrained) perceives the constraint as inadequate — they see snare but want elimination, not reform, of personal law. These gaps reveal that no single indexical position captures the full structure; the presheaf of perspectives is required to see the multiple extraction mechanisms and coordination functions simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective is derived from the agent's structural relationship to the marriage authority kernel and to the gender rights reading's operative mechanism. Women within patriarchal personal law have d ≈ 0.88 (full target of extraction): identity-locked exit, victim group designation, no arbitrage options. They experience high f(d) ≈ 1.30. Personal law community authorities have d ≈ 0.72 (target of judicial extraction, but beneficiary of internal gender hierarchy): organized power, constrained exit, experience both extraction loss (authority) and benefits (maintenance of patriarchal structure). The constitutional court has d ≈ 0.15 (full beneficiary: expands authority, claims legitimacy): institutional power, arbitrage exit options, primary beneficiary of the reading. Women's rights advocates have d ≈ 0.20 (beneficiary: authority expansion, policy victory): institutional power, arbitrage exit. The secularist coalition has d ≈ 0.45 (mixed: policy alignment but constrained by pluralism): organized power, constrained exit (cannot openly reject pluralism without fracturing coalition), partial beneficiary. These directionality values drive differential f(d) mapping: beneficiaries experience negative or low χ (the constraint does not extract from them); victims experience high χ (extraction is severe). The presheaf over different d values captures why the constraint classifies differently across perspectives.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_authority_legitimacy,
    'On what grounds does the constitutional court claim authority over personal law domains traditionally governed by communal/religious authority?',
    'Textual analysis of constitutional equality provisions (Articles 14-15) versus explicit personal law exemptions; comparative jurisprudence across pluralist democracies (India, Israel, Canada, Malaysia); legitimacy surveys within affected communities',
    'If judicial reading of equality is overreach: gender rights reading is extractive reframing of what communities see as illegitimate state intrusion (strengthens snare classification). If personal law exemptions are judicially indefensible: gender rights reading is necessary correction (reframes as legitimate authority expansion). Either way, the legitimacy grounding is contested and cannot be settled by text alone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_authority_legitimacy, conceptual, 'Legitimacy grounds for judicial authority over personal law').

omega_variable(
    women_agency_within_patriarchal_frame,
    'Are women within patriarchal personal law systems systematically identity-locked (unable to perceive exit as thinkable from within their identity frame) or strategically constrained (understanding exit as possible but too costly)?',
    'Ethnographic research on lived experience of agency; studies of women''s perception of personal law provisions; analysis of women who exit patriarchal frameworks (what triggers identity frame shift); comparison with women in secular law jurisdictions',
    'If identity-locked predominates: the gender rights reading is addressing a binding mechanism that transcends material barriers — the constitutional equality doctrine may not reach the actual constraint. If constrained predominates: the gender rights reading is correctly targeting material barriers (legal authority to deny unilateral divorce, property rights, etc.), and constitutional reform can be effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(women_agency_within_patriarchal_frame, empirical, 'Whether women in patriarchal personal law are identity-locked or materially constrained').

omega_variable(
    communal_authority_accommodationist_capacity,
    'Can communal/religious authorities reform internal gender provisions to comply with constitutional equality while maintaining core communal identity and legitimacy?',
    'Historical analysis of communal legal evolution (Islamic law reforms in Turkey, Tunisia, Morocco; rabbinic law developments; Hindu law codification); qualitative interviews with religious scholars on whether gender equality is intrinsic to doctrine or historically contingent; assessment of whether gender equality accommodates or forecloses the communal reading of the kernel',
    'If accommodationist reform is possible: the gender rights reading creates pressure for endogenous reform rather than external extraction — classification shifts toward tangled_rope (genuine coordination function becomes visible). If accommodationist reform is impossible: the readings foreclose each other — the gender rights reading and communal autonomy reading cannot coexist in the same authority framework; snare classification is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(communal_authority_accommodationist_capacity, empirical, 'Whether communal authorities can accommodate gender equality without identity dissolution').

omega_variable(
    reading_kernel_coherence,
    'Is this reading''s invocation of ''constitutional equality'' addressing the same kernel (marriage authority) or is it attempting to replace the kernel with a different commitment (secular individual rights)?',
    'Textual close reading of constitutional court judgments; analysis of whether courts treat personal law as a legitimate kernel (to be reformed) or as illegitimate (to be progressively eliminated); examination of whether the reading claims marriage authority should remain anchored in communal law (reformed) or migrate entirely to state law (replaced)',
    'If the reading reforms the kernel: the constraint is tangled_rope — genuine coordination function (gender equality within communal framework) embedded in extraction (judicial authority expansion). If the reading replaces the kernel: the constraint is snare — the reading''s core move is to deny the legitimacy of communal authority entirely, treating patriarchal extraction as the only real function of personal law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_coherence, conceptual, 'Whether the reading reforms or replaces the marriage authority kernel').

omega_variable(
    foundational_axiom_empirical_status,
    'Is the axiom ''constitutional equality is fundamental'' grounded in discovered universal principle (deontological) or in historically contingent emergence of rights consciousness (instrumental/conventional)?',
    'History of philosophy survey on rights doctrine evolution; comparative jurisprudence on how different constitutional traditions justify equality; analysis of whether equality doctrine predates contemporary pluralism or emerges from conflict with traditional authorities',
    'If deontological grounding: the gender rights reading stands on principle that cannot be negotiated — forecloses communal autonomy reading. If instrumental/conventional: the reading is one defensible policy choice among others — coexists_with communal autonomy reading, both are legitimate frameworks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foundational_axiom_empirical_status, conceptual, 'Epistemic grounding of foundational equality axiom').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__gender_rights_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_gender_tr_t0, marriage_authority__gender_rights_reading, theater_ratio, 0, 0.62).
narrative_ontology:measurement(marr_gender_tr_t5, marriage_authority__gender_rights_reading, theater_ratio, 5, 0.58).
narrative_ontology:measurement(marr_gender_tr_t10, marriage_authority__gender_rights_reading, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(marr_gender_be_t0, marriage_authority__gender_rights_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(marr_gender_be_t5, marriage_authority__gender_rights_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(marr_gender_be_t10, marriage_authority__gender_rights_reading, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(marr_gender_su_t0, marriage_authority__gender_rights_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(marr_gender_su_t5, marriage_authority__gender_rights_reading, suppression_requirement, 5, 0.72).
narrative_ontology:measurement(marr_gender_su_t10, marriage_authority__gender_rights_reading, suppression_requirement, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__gender_rights_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__judicial_harmonization_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, patriarchal_talaq_mechanism).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, personal_law_inheritance_asymmetry).

% DUAL FORMULATION NOTE:
% The marriage authority kernel decomposes into multiple structurally distinct constraints corresponding to different readings. The gender rights reading is one such constraint; it differs from the communal autonomy reading by target (patriarchal provisions vs. communal authority) and from the secularist reading by goal (reform within pluralism vs. elimination of pluralism). Each reading has its own ε value reflecting the reading's operative extraction mechanism. The gender rights reading (ε=0.68, snare/tangled_rope) is more extractive than a successful judicial_harmonization reading would be (projected ε=0.35, tangled_rope) because it operates through authority expansion rather than negotiation. Network edges link the readings together so that the system can model how the gender rights reading's success exerts structural pressure on communal autonomy and secularist alternatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
