% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__traditionalist_taqlid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_hadith_substrate__traditionalist_taqlid, []).

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
 *   constraint_id: quran_hadith_substrate__traditionalist_taqlid
 *   human_readable: Classical Madhhab Authority via Taqlid (Traditionalist Reading)
 *   domain: religious/legal/institutional
 *
 * SUMMARY:
 *   This constraint embodies the traditionalist reading of the Quran-hadith
 *   substrate kernel: classical fiqh schools (madhhabs) represent
 *   authoritative consensus (ijma), and contemporary Muslims are obligated
 *   via taqlid to follow established rulings rather than engage in
 *   independent reasoning (ijtihad). This is ONE READING of a contested
 *   kernel; sibling readings (reformist ijtihad and state hybrid) instantiate
 *   different constraints from the same source commitment. The traditionalist
 *   reading suppresses alternative interpretations through institutional
 *   enforcement, maintains unequal legal status for women and religious
 *   minorities under classical frameworks, and concentrates interpretive
 *   authority in hierarchical institutions. The metrics reflect high
 *   extraction and suppression; the claimed type is tangled rope because the
 *   constraint solves a genuine coordination problem (settled law,
 *   predictable jurisprudence) while simultaneously extracting from those
 *   constrained by classical rulings they did not consent to and cannot
 *   challenge within the framework.
 *
 * KEY AGENTS:
 *   - Traditionalist ulama: institutional beneficiaries who control the interpretive apparatus and derive authority from madhhab lineage
 *   - Madhhab institutions: establishment structures (seminaries, fatwa councils) that concentrate and reproduce authority; institutionally trapped in identity with their school
 *   - Progressive Muslim scholars: intellectual targets excluded from authority; efforts to reinterpret are delegitimized
 *   - Women under classical family law: powerless payers bearing unequal inheritance, guardianship, and divorce restrictions
 *   - Religious minorities under dhimmi framework: trapped payers under subordinate legal status with no exit
 *   - Laity seeking Quranic understanding: suppressed agents told taqlid is binding rather than optional
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__traditionalist_taqlid, 0.68).
domain_priors:suppression_score(quran_hadith_substrate__traditionalist_taqlid, 0.79).
domain_priors:theater_ratio(quran_hadith_substrate__traditionalist_taqlid, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, extractiveness, 0.68).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__traditionalist_taqlid, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__traditionalist_taqlid, "Classical Madhhab Authority via Taqlid (Traditionalist Reading)").
narrative_ontology:topic_domain(quran_hadith_substrate__traditionalist_taqlid, "religious/legal/institutional").

domain_priors:requires_active_enforcement(quran_hadith_substrate__traditionalist_taqlid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__traditionalist_taqlid, '3ea0ae4d-893d-4c59-b28d-71311b5a36b9').
narrative_ontology:cs_kernel_codification('3ea0ae4d-893d-4c59-b28d-71311b5a36b9', fixed_text).
narrative_ontology:cs_authority_grounding('3ea0ae4d-893d-4c59-b28d-71311b5a36b9', lineage).
narrative_ontology:cs_interpretation_layer_present('3ea0ae4d-893d-4c59-b28d-71311b5a36b9').
narrative_ontology:cs_reading_relation('3ea0ae4d-893d-4c59-b28d-71311b5a36b9', quran_hadith_substrate__reformist_ijtihad, coexists_with).
narrative_ontology:cs_reading_relation('3ea0ae4d-893d-4c59-b28d-71311b5a36b9', quran_hadith_substrate__state_hybrid, coexists_with).
narrative_ontology:cs_axiom('3ea0ae4d-893d-4c59-b28d-71311b5a36b9', foundational, ijma_binds_contemporaries).
narrative_ontology:cs_axiom_status(ijma_binds_contemporaries, holdable).
narrative_ontology:cs_axiom_grounding('3ea0ae4d-893d-4c59-b28d-71311b5a36b9', ijma_binds_contemporaries, deontological).
narrative_ontology:cs_axiom('3ea0ae4d-893d-4c59-b28d-71311b5a36b9', foundational, taqlid_obligatory_not_optional).
narrative_ontology:cs_axiom_status(taqlid_obligatory_not_optional, holdable).
narrative_ontology:cs_axiom_grounding('3ea0ae4d-893d-4c59-b28d-71311b5a36b9', taqlid_obligatory_not_optional, conventional).
narrative_ontology:cs_reference_frame('3ea0ae4d-893d-4c59-b28d-71311b5a36b9', classical_madhhab_consensus_locked).
narrative_ontology:cs_drift_state('3ea0ae4d-893d-4c59-b28d-71311b5a36b9', contemporary_pluralist_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3ea0ae4d-893d-4c59-b28d-71311b5a36b9', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, traditionalist_ulama).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, conservative_mosque_hierarchies).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, progressive_muslim_scholars).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, women_seeking_equal_rights).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, religious_minorities_under_dhimmi_framework).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, laity_seeking_direct_quranic_interpretation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Senior scholars and jurisconsults within the four established madhhabs (Hanafi, Maliki, Shafi'i, Hanbali) who interpret, transmit, and enforce classical rulings. They maintain the interpretive apparatus, train students in madhhab methodology, issue fatwas, and adjudicate disputes within their framework. Their authority is grounded in the lineage of transmission from the founder-imam and the claim that ijma (consensus of classical scholars) has settled the law.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, traditionalist_ulama, agenda_setter,
    institutional, generational, identity_locked, global).

% Formal and informal institutional structures (seminaries, fatwa councils, Islamic university departments, mosque networks) that reproduce and enforce madhhab authority. They benefit from the taqlid obligation because it concentrates interpretive power within their hierarchy and channels resources (students, donations, political influence) to them. They carry centuries of institutional inertia and identity fusion with their madhhab.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions, beneficiary,
    institutional, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions, agenda_setter).

% Mosque leadership structures and community religious authorities in conservative-leaning communities who benefit from clear, binding rulings. They need not deliberate on contested issues; they enforce the established madhhab position and derive legitimacy from representing 'authentic Islam' as preserved in the schools.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, conservative_mosque_hierarchies, beneficiary,
    organized, generational, constrained, regional).

% Muslim intellectuals, theologians, and academics advocating contextual ijtihad, reinterpretation of classical rulings in light of contemporary ethics, and direct engagement with Quranic principles. They bear the cost of institutional marginalization: their fatwas are delegitimized as 'not madhhab-conform,' they face social and professional pressure, and their interpretations cannot gain traction in conservative communities or formal Islamic institutions. Their intellectual contributions are systematically suppressed by the taqlid framework.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, progressive_muslim_scholars, payer,
    moderate, biographical, constrained, global).

% Women navigating classical Islamic family law (inheritance shares, marriage guardianship, divorce procedures, testimony weight) as enforced by traditionalist institutions. They bear the extraction through unequal legal status, restricted agency in marriage and divorce, and diminished inheritance rights preserved in classical rulings. Taqlid suppresses alternative readings of Quranic verses that would grant equal status. Their exit options are trapped by religious identity, family structure, and community consequence.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, women_seeking_equal_rights, payer,
    powerless, biographical, identity_locked, global).

% Christians, Jews, Yazidis, and other non-Muslim minorities in contexts where classical madhhab rulings (the dhimmi system) remain institutionally enforced. They bear differentiated legal status, restricted property rights, and subordinate civic position as structured by classical jurisprudence. Taqlid forecloses reinterpretations that would grant equal citizenship or religious freedom; their exit is blocked by geography, family, and legal constraint.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, religious_minorities_under_dhimmi_framework, payer,
    powerless, biographical, trapped, regional).

% Ordinary Muslims who might wish to understand Quranic principles directly and reason about contemporary applications but are told by conservative religious authorities that taqlid (following a madhhab) is a binding obligation, not optional. They bear the cost of intellectual dependence: their questions are redirected to madhhab rulings, direct Quranic study is discouraged as leading to heresy, and personal reasoning (ijtihad) is framed as presumptuous. Their agency is suppressed.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, laity_seeking_direct_quranic_interpretation, payer,
    powerless, biographical, identity_locked, global).

% Institutional and intellectual structures (universities, reformist fatwa councils, progressive Islamic organizations) advocating contextual ijtihad and reinterpretation. They are structurally excluded from authority in traditionalist-dominated communities; their interpretations are delegitimized; and their capacity to shape Islamic law is systematically constrained by the institutional dominance of the madhhabs.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, reformist_ijtihad_movement, excluded,
    powerful, generational, constrained, global).

% Governments in Muslim-majority countries that selectively adopt classical rulings for family law and criminal codes while applying different legal frameworks in commercial and administrative law. They observe the traditionalist reading and must negotiate with it (often incorporating it into state law) while also pursuing modernization and sovereignty claims. Their stance on taqlid varies: some enforce it, others use it strategically, still others attempt hybrid compromises.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, state_authorities_claiming_islamic_legitimacy, observer,
    institutional, generational, analytical, national).

% Scholars outside the traditional authority structure (historians, sociologists, comparative religionists) who study the constraint's operation empirically and document how taqlid suppresses alternative readings, how institutional mechanisms enforce it, and what the distributional consequences are for different stakeholder groups.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, academic_islamic_studies_observers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions).
narrative_ontology:fixing_cost_class(quran_hadith_substrate__traditionalist_taqlid, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Islamic jurisprudence across diverse contexts by anchoring interpretation to four established schools and their classical rulings: eliminates the chaos of unlimited individual ijtihad; provides settled law for contract, family, and criminal matters; enables community cohesion through shared normative framework; transmits legal knowledge reliably across generations.
% TRANSFER_FUNCTION: Transfers interpretive authority from the laity and from contemporary scholars to the classical madhhab establishments and their living representatives. It moves deference, legitimacy, and legal power upward through institutional hierarchies; moves compliance, constraint, and suppression of alternative voices downward to those obligated to follow without questioning.
% ABSENT_VOICES: Progressive Muslim scholars advocating ijtihad, women's rights advocates seeking equal legal standing, religious minorities seeking full citizenship rather than dhimmi status, and Muslims seeking direct Quranic reasoning are structurally excluded from authority in the traditionalist reading. They would argue that ijma is not binding, that classical rulings have outdated ethical premises, and that taqlid suppresses human agency and justice. The traditionalist framework excludes them by fiat.
% DISAPPEARANCE_RATIONALE: If the taqlid obligation vanished, Islamic jurisprudence would fragment into competing methodologies (unconstrained ijtihad, reformed reinterpretations, contextual ethics); institutional power would disperse from the classical madhhab establishments to universities, think tanks, and civil-society scholars; women and minorities would gain interpretive space for equal-rights readings; traditional mosque hierarchies would lose their claims to settled, binding authority. The Islamic world would reorganize around competing legitimate interpretive methodologies rather than hierarchical submission to classical schools.
% FOUNDING_PROBLEM: Early Islamic jurisprudence faced fragmentation: scholars disagreed on methods, different regions developed divergent practices, and the lack of formal unity threatened the coherence of Islamic law. The madhhab system crystallized consensus around four major schools in the medieval period; taqlid was an institutional response to lock in a unified framework and prevent endless reinterpretation.
% FOUNDING_PROBLEM_CORROBORATION: Progressive Muslim scholars and academic historians document that modern communication, literacy, and institutional capacity make the unity-by-constraint argument obsolete: Islamic law can be coherent and diverse simultaneously; other religious traditions maintain normative authority without suppressing interpretation; and contemporary taqlid enforcement persists not because fragmentation threatens Islam but because institutional power benefits from closure. Traditionalist authorities claim the founding problem is perennially live (chaos lurks if taqlid weakens), but corroboration from outside the benefiting parties contradicts this.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__traditionalist_taqlid, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__traditionalist_taqlid, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__traditionalist_taqlid, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_hadith_substrate__traditionalist_taqlid, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__traditionalist_taqlid, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_hadith_substrate__traditionalist_taqlid_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_hadith_substrate__traditionalist_taqlid, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_hadith_substrate__traditionalist_taqlid_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because taqlid concentrates interpretive authority, prevents upward mobility of alternative scholarship, and enforces compliance with rulings that benefit institutional hierarchies. The measurements show gradual increase through the first 25 time points (institutional hardening in response to modernization pressures, more aggressive enforcement of taqlid), then plateau as digital media and global communication create countervailing pressure that cannot be fully suppressed but also cannot displace the institutional structures. Suppression is higher (0.79) than extractiveness because suppressing alternative readings is the active work the constraint must do: if ijtihad were permitted, the entire institutional hierarchy would lose legitimacy. Theater rises early (first 15 time points) as traditionalist authorities emphasize scholarly consensus and doctrinal legitimacy when challenged by reformist arguments, then stabilizes once the performance ceiling is reached. Accessibility collapse is moderately high (0.72) because educated Muslims can understand both the traditionalist and reformist readings, but institutional and social consequences suppress the ability to act on alternative understandings. Resistance is moderate (0.58) because reformist intellectual movements, women's rights advocates, and progressive institutions mount real counter-pressure, but the institutional entrenchment of the madhhabs and their control of mosque networks constrains how far that resistance can shift actual practice in traditionalist-majority communities.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute radically different constraint types. From the traditionalist-ulama seat, the constraint is rope (coordination solving the unity problem, consensual participation in a shared framework). From the women's seat, it is snare (coercive extraction of unequal legal status with suppressed exits and no exit alternatives). From the progressive scholar seat, it is tangled rope tilting toward snare (coordination element present for those who benefit, extraction element dominant for those excluded and suppressed). The engine should flag this divergence as the defining feature of the constraint's operation: it is experienced as legitimate authority by institutional beneficiaries and as coercive suppression by victims.
 *
 * DIRECTIONALITY LOGIC:
 *   The traditionalist ulama and madhhab institutions sit at the beneficiary end of directionality (d near 0.0): they collect authority, influence, prestige, and resources; their exits are abundant (they can reinterpret if they choose; they are not trapped, they are institutionally fused). Progressive scholars face high d (toward 1.0): they are targets of suppression, excluded from authority, face professional consequences for alternative readings. Women and minorities face the highest d (closest to 1.0): they are trapped (identity-locked to family, community, religion), constrained by legal disability, and powerless to change the rules affecting them. The laity face moderate-high d: they are told to be passive recipients of madhhab rulings, but they retain some agency to seek alternative sources or question privately. State authorities face complex d: they extract benefits (using madhhab authority to legitimize family law) but also constrain their own reforms (locked into classical frameworks when modernization pressure rises). The directionality override for traditionalist_ulama should be explicit (d ≈ 0.15) because they appear institutionally powerful but their identity fusion with the madhhab creates subtle trappedness; an override moves them toward the beneficiary end to reflect that their agency, while real, is wholly invested in defending the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing jurisprudential chaos through institutional unity) is categorized as DEAD by external corroboration: academic historians, reformist scholars outside the benefiting circle, and practical evidence show that Islamic jurisprudence can remain coherent while permitting interpretive diversity (as demonstrated by pluralist Islamic societies and communities where multiple madhabs coexist). Yet the taqlid obligation persists and is actively reinforced. This is a classic mandatrophy marker: the founding mandate has outlived its function; the constraint persists through institutional inertia and the concentration of benefits to the madhhab hierarchy, not through necessity. The theater_ratio rising early (0.25→0.42) reflects traditionalist authorities doubling down on rhetorical claims about ijma and doctrinal authenticity precisely when the empirical mandate dissolves—a signature piton-type behavior. However, the extraction metrics remain high (not dropping below 0.68), which keeps the classification as tangled rope rather than piton: unlike a pure piton, this constraint still actively extracts (unequal rights, suppressed agency) rather than merely performing; the theatrical component coexists with real enforced extraction, not as a replacement for it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ijma_vs_contemporary_consensus,
    'Can ijma (consensus of classical scholars) bind contemporary Muslims who did not participate in that consensus and who inhabit different ethical and technological contexts? Or is ijma a historical agreement that retains moral weight but does not bind those with new evidence or reasoning?',
    'Epistemological and jurisprudential analysis of consensus-binding across time; examination of whether ijma requires reaffirmation by each generation of scholars or binds perpetually; empirical study of how Muslim communities actually treat classical consensus (some accept it as binding, others treat it as authoritative but revisable).',
    'If ijma must be reaffirmed per generation, taqlid becomes optional (each generation can ijtihad), and the constraint transitions from tangled_rope to scaffold. If ijma binds perpetually, taqlid remains structurally binding, and institutional extraction persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ijma_vs_contemporary_consensus, conceptual, 'Whether consensus binds across generations or must be renewed.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the suppression of alternative readings maintained primarily through institutional barriers (fatwas delegitimizing reformist scholarship, institutions excluding reformists from authority, family consequences for questioning), or through internalized belief (Muslims believing taqlid is theologically mandatory, not just institutionally preferred)?',
    'Post-institutional analysis: when institutional enforcement is removed (e.g., internet access to reformist scholarship, diaspora contexts with weak institutional hierarchy, secular legal codes displacing madhhab authority), does suppression persist? If suppression erodes, it was structural; if it persists, it has internalized roots.',
    'If primarily structural, removing institutional enforcement could shift the constraint type toward rope or even mountain-natural-law status. If internalized, the constraint persists even without enforcement because targets have fused their understanding of Islam with the taqlid obligation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression is institutional or internalized.').

omega_variable(
    equal_rights_reading_foreclosure,
    'Is the exclusion of women''s-rights and minority-rights readings from authority within traditionalist contexts a logical consequence of the taqlid principle itself, or a contingent institutional choice that traditionalist scholars could overcome without abandoning taqlid?',
    'Jurisprudential analysis of whether taqlid-compliant reinterpretations of Quranic verses on women''s inheritance, marriage agency, and dhimmi status are logically possible. Historical example: some madhhab scholars offered minority opinions on these issues; could those be elevated without violating taqlid methodology?',
    'If equal-rights readings are logically possible within taqlid, the extraction directed at women and minorities is a contingent institutional choice, and mandatrophy could be resolved by internal reform. If they are foreclosed, the structural extraction is more deeply embedded.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(equal_rights_reading_foreclosure, conceptual, 'Whether women''s and minority rights can be secured within taqlid framework.').

omega_variable(
    kernel_contest_foreclosure_status,
    'Within a single Islamic community or polity, can traditionalist taqlid, reformist ijtihad, and state hybrid readings coexist as live interpretive options, or does adopting one foreclose the others? Are these genuinely different readings of one kernel, or do they represent competing kernels?',
    'Study of pluralist Islamic communities (diaspora communities, multicultural societies) where multiple readings are simultaneously practiced; analysis of whether theological coherence is preserved or whether communities fragment into separate traditions.',
    'If coexistence is empirically sustainable and philosophically coherent, the readings are sibling relations and the coexists_with classification holds. If coexistence leads to practical fragmentation or theological incoherence, the readings may constitute separate kernels, and the network topology should be restructured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_foreclosure_status, conceptual, 'Whether three readings coexist or separate into competing traditions.').

omega_variable(
    natural_law_claim_false_summit_candidate,
    'Traditionalist authorities present taqlid as grounded in natural jurisprudential law (ijma is binding by divine decree, madhab consensus reflects immutable Islamic principles). Is this a genuine mountain-type natural law, or a false summit where institutional beneficiaries claim naturalness to resist scrutiny?',
    'Comparative religious-law analysis: do similar authority-concentration mechanisms exist in other traditions (Catholicism, Rabbinical Judaism, Hindu jurisprudence)? Do they claim natural-law status? If taqlid is uniquely necessary for Islam while other traditions manage without it, the natural-law claim weakens. Empirical observation: when institutional enforcement is weakened, does the taqlid obligation persist as natural law, or does it erode?',
    'If genuine mountain, the constraint should be reclassified; if false summit, it remains tangled_rope and mandatrophy persists. High likelihood of false-summit status given the institutional beneficiary structure and the finding that mandatrophy is present (founding problem dead but constraint persists).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_claim_false_summit_candidate, empirical, 'Whether taqlid is natural law or institutional false summit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__traditionalist_taqlid, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 0, 0.25).
narrative_ontology:measurement(qura_tr_t5, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 5, 0.28).
narrative_ontology:measurement(qura_tr_t10, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 10, 0.32).
narrative_ontology:measurement(qura_tr_t15, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 15, 0.36).
narrative_ontology:measurement(qura_tr_t20, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 20, 0.4).
narrative_ontology:measurement(qura_tr_t25, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 25, 0.41).
narrative_ontology:measurement(qura_tr_t30, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 30, 0.42).
narrative_ontology:measurement(qura_tr_t40, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(qura_be_t5, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(qura_be_t10, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(qura_be_t15, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(qura_be_t20, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(qura_be_t25, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(qura_be_t30, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(qura_be_t40, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(qura_su_t5, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 5, 0.71).
narrative_ontology:measurement(qura_su_t10, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 10, 0.74).
narrative_ontology:measurement(qura_su_t15, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 15, 0.76).
narrative_ontology:measurement(qura_su_t20, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(qura_su_t25, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 25, 0.79).
narrative_ontology:measurement(qura_su_t30, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 30, 0.79).
narrative_ontology:measurement(qura_su_t40, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 40, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__traditionalist_taqlid, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(quran_hadith_substrate__traditionalist_taqlid, 0.12).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate__reformist_ijtihad).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate__state_hybrid).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the quran_hadith_substrate kernel. The reformist ijtihad reading (constraint_id: quran_hadith_substrate__reformist_ijtihad) presents competing epistemology and beneficiary structure; the state_hybrid reading (constraint_id: quran_hadith_substrate__state_hybrid) describes institutional compromise. All three share the kernel commitment to Quranic-hadith guidance but diverge on whether ijma binds, whether ijtihad is permitted, and whether state authority can override doctrinal fidelity. The three stories are NOT competing measurements of one constraint—they are different constraints arising from different readings of the same kernel. Each has its own ε, its own beneficiary/victim structure, its own type classification. Decomposition follows the ε-invariance principle: a traditionalist reading computes ε for the taqlid obligation as high (extraction for modernists, suppressed alternatives); a reformist reading would compute ε for that same obligation differently (it would score the traditionalist impediment to ijtihad as extraction). The readings have different referents and different observer positions. Link all three via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_hadith_substrate__traditionalist_taqlid, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
