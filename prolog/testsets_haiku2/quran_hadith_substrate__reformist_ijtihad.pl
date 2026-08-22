% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__reformist_ijtihad
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_hadith_substrate__reformist_ijtihad, []).

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
 *   constraint_id: quran_hadith_substrate__reformist_ijtihad
 *   human_readable: Reformist Ijtihad: Contextual Ethics Over Literalist Application
 *   domain: religious/legal/ethical
 *
 * SUMMARY:
 *   Reformist Islamic jurisprudence mandates ijtihad (contextual
 *   interpretation) when classical fiqh rulings conflict with contemporary
 *   ethics, human rights norms, or public interest (maslaha). This reading
 *   privileges the Quran's ethical trajectory over literalist hadith
 *   application. It is one reading of the contested Quran-hadith substrate
 *   kernel, contested alongside traditionalist taqlid (classical madhab
 *   authority) and state-hybrid approaches (selective adoption of classical
 *   and reformist law). The reformist reading benefits progressive Muslims,
 *   women, LGBTQ+ individuals, and religious minorities by providing Islamic
 *   jurisprudential grounds for rights-aligned rulings. It extracts from
 *   traditionalist authority structures whose legitimacy depends on
 *   interpretive gatekeeping. The constraint exhibits moderate extractiveness
 *   (0.42) and moderate suppression (0.48) — higher in contexts where
 *   traditionalist counter-mobilization is strong, lower where reformist
 *   institutions have secure backing. The claim (tangled_rope) and metrics
 *   align: genuine coordination function (solving the binding problem of
 *   Islamic authenticity + contemporary ethics) combined with asymmetric
 *   extraction (transferring interpretive authority from classical
 *   gatekeepers). Temporal measurements show rising suppression_requirement
 *   and theater_ratio from t0 to t30, then partial stabilization as
 *   traditionalist mobilization intensifies and pushes back; the constraint
 *   hardens its institutional boundaries but also becomes more performatively
 *   defended.
 *
 * KEY AGENTS:
 *   - Progressive Muslim scholars: institutional agenda-setters, beneficiaries from interpretive authority transfer
 *   - Traditionalist fiqh gatekeepers (Al-Azhar, madhab networks, conservative ulama): institutional payers, facing erosion of methodological monopoly
 *   - Women, LGBTQ+ individuals, religious minorities: organized and dispersed beneficiaries, identity-locked to Islam, dependent on reformist jurisprudence for rights-compatible belonging
 *   - Literalist-salafi movements: excluded, identity-locked payers, institutionally active counter-mobilizers
 *   - Reform-oriented states: institutional beneficiaries, arbitrage-positioned between Islamic legitimacy and rights alignment
 *   - Transnational human rights institutions: analytical-seat beneficiaries, leverage reformist scholarship as proof of Islam-rights compatibility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__reformist_ijtihad, 0.42).
domain_priors:suppression_score(quran_hadith_substrate__reformist_ijtihad, 0.48).
domain_priors:theater_ratio(quran_hadith_substrate__reformist_ijtihad, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, extractiveness, 0.42).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__reformist_ijtihad, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__reformist_ijtihad, "Reformist Ijtihad: Contextual Ethics Over Literalist Application").
narrative_ontology:topic_domain(quran_hadith_substrate__reformist_ijtihad, "religious/legal/ethical").

domain_priors:requires_active_enforcement(quran_hadith_substrate__reformist_ijtihad).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__reformist_ijtihad, '87ec671f-43a7-4b0f-9cde-37c58df061f6').
narrative_ontology:cs_kernel_codification('87ec671f-43a7-4b0f-9cde-37c58df061f6', fixed_text).
narrative_ontology:cs_authority_grounding('87ec671f-43a7-4b0f-9cde-37c58df061f6', lineage).
narrative_ontology:cs_interpretation_layer_present('87ec671f-43a7-4b0f-9cde-37c58df061f6').
narrative_ontology:cs_reading_relation('87ec671f-43a7-4b0f-9cde-37c58df061f6', quran_hadith_substrate__traditionalist_taqlid, coexists_with).
narrative_ontology:cs_reading_relation('87ec671f-43a7-4b0f-9cde-37c58df061f6', quran_hadith_substrate__state_hybrid, coexists_with).
narrative_ontology:cs_axiom('87ec671f-43a7-4b0f-9cde-37c58df061f6', foundational, quranic_ethics_supersedes_literalist_hadith).
narrative_ontology:cs_axiom_status(quranic_ethics_supersedes_literalist_hadith, holdable).
narrative_ontology:cs_axiom_grounding('87ec671f-43a7-4b0f-9cde-37c58df061f6', quranic_ethics_supersedes_literalist_hadith, deontological).
narrative_ontology:cs_axiom('87ec671f-43a7-4b0f-9cde-37c58df061f6', foundational, ijtihad_mandated_when_texts_conflict).
narrative_ontology:cs_axiom_status(ijtihad_mandated_when_texts_conflict, holdable).
narrative_ontology:cs_axiom_grounding('87ec671f-43a7-4b0f-9cde-37c58df061f6', ijtihad_mandated_when_texts_conflict, conventional).
narrative_ontology:cs_axiom('87ec671f-43a7-4b0f-9cde-37c58df061f6', secondary, maqasid_sharia_justifies_contextual_reasoning).
narrative_ontology:cs_axiom_status(maqasid_sharia_justifies_contextual_reasoning, holdable).
narrative_ontology:cs_axiom_grounding('87ec671f-43a7-4b0f-9cde-37c58df061f6', maqasid_sharia_justifies_contextual_reasoning, instrumental).
narrative_ontology:cs_reference_frame('87ec671f-43a7-4b0f-9cde-37c58df061f6', quranic_ethical_intentionality).
narrative_ontology:cs_drift_state('87ec671f-43a7-4b0f-9cde-37c58df061f6', contemporary_human_rights_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('87ec671f-43a7-4b0f-9cde-37c58df061f6', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, progressive_muslim_scholars).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, women_seeking_equality).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, lgbtq_muslims).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, religious_minorities).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, reform_oriented_states).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, traditionalist_authority_structures).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, classical_fiqh_gatekeepers).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, literalist_interpreters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, transnational_human_rights_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Articulate and defend reformist ijtihad as the authentic interpretive method grounded in Quranic ethics and maqasid al-sharia. They argue that contextual reasoning is mandated by the Quran itself and that literalist application of medieval rulings to contemporary questions violates the Quranic ethical trajectory. They set the interpretive agenda within reform-oriented institutions, academic centers, and state legal commissions. They benefit from expanded interpretive authority and from resolving contradictions between classical law and contemporary human rights norms.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, progressive_muslim_scholars, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__reformist_ijtihad, progressive_muslim_scholars, beneficiary).

% Classical fiqh schools and their institutional representatives (Al-Azhar, conservative madhabs, traditional ulama networks) face pressure to legitimize themselves against reformist claims that their methodological authority is historically contingent rather than divinely mandated. They bear the cost of defending taqlid against the charge that it freezes interpretive authority in medieval consensus. Their exit is constrained by dependence on institutional continuity and on the appearance of methodological orthodoxy.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, traditionalist_authority_structures, payer,
    institutional, generational, constrained, global).

% Gain from reformist ijtihad's prioritization of Quranic egalitarian ethics over medieval rulings on guardianship, divorce, inheritance, and testimony. They can point to Quranic verses affirming dignity and rights and demand contemporary jurisprudence ground itself there rather than in hadith literature coded for medieval social structures. Their ability to exercise this exit has expanded with global literacy and transnational feminist networks.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, women_seeking_equality, beneficiary,
    organized, biographical, mobile, global).

% Reformist ijtihad provides grounds to argue that literalist rulings on sexuality reflect historical social organization, not eternal Quranic principles, and that maqasid frameworks centering dignity and justice can reframe sexual ethics. Their identity is fused with Islam; exit from the tradition is identity dissolution, not relocation. They depend on reformist scholars to generate interpretive space for their existence as Muslims.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, lgbtq_muslims, beneficiary,
    moderate, biographical, identity_locked, global).

% Benefit when reformist ijtihad frames Quranic ethics of pluralism and religious freedom as binding jurisprudential principles, constraining traditionalist readings that treat non-Muslim status as permanently subordinate. Reformist scholars argue that quranic covenants with people of the book and protections for religious conscience override literalist minority-subjugation rulings. Their exit is geographically and institutionally constrained; they depend on reformist interpretive authority to secure basic rights.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, religious_minorities, beneficiary,
    moderate, biographical, constrained, regional).

% Traditional scholars and madrasa networks whose professional identity and institutional authority rest on mastery of classical fiqh. Reformist ijtihad treats their medieval corpus as historically contingent rather than timeless, undermining their claim to transmit divinely-grounded knowledge. They are trapped by career path dependence and by institutional structures that have no other currency. Their suppression is partly structural (access to reformist interpretive authority is contested) and partly internalized (professional identity fused with classical methodology).
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, classical_fiqh_gatekeepers, payer,
    powerful, generational, trapped, global).

% Practitioners and communities committed to literalist hadith application as the only authentic methodology (salafi movements, hadith-purist networks). Reformist ijtihad's claim that ethical context overrides literalist hadith directly contradicts their core epistemological premise. Their identity is fused with literalist methodology; exit would require abandoning their understanding of authentic Islam. They face suppression not only from external institutional pressure but from internal identity commitment that makes reformism unthinkable.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, literalist_interpreters, payer,
    organized, generational, identity_locked, global).

% States seeking to modernize family law, criminal codes, or civil rights frameworks find legitimacy in reformist ijtihad — they can claim to follow Islamic jurisprudence while aligning with international human rights norms and domestic constitutional commitments. Reformist scholars provide institutional cover for legal modernization. They benefit from the ability to maintain Islamic legitimacy while navigating geopolitical and rights-based pressures.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, reform_oriented_states, beneficiary,
    institutional, generational, arbitrage, national).

% International human rights bodies, NGO networks, and secular legal academics gain interpretive legitimacy by pointing to reformist ijtihad as proof that Islamic jurisprudence is compatible with rights frameworks. The existence of reformist Islamic scholarship provides them with a counternarrative to claims that rights norms are inherently non-Islamic. They do not enforce the constraint but benefit from its existence.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, transnational_human_rights_institutions, beneficiary,
    institutional, generational, analytical, global).

% Organized traditionalist movements (wahhabism, salafism, conservative madrasa networks) that actively resist reformist readings are structurally excluded from the reformist ijtihad constraint's decision-making. They would argue that contextual ethics opens the door to cultural relativism and abandonment of immutable divine law. Their exclusion is enforced by institutional gatekeeping and by control of prestigious Islamic institutions in some regions; in others, they mount active counter-mobilization.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, traditionalist_counter_mobilization, excluded,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_hadith_substrate__reformist_ijtihad, progressive_muslim_scholars).
narrative_ontology:fixing_cost_class(quran_hadith_substrate__reformist_ijtihad, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reformist ijtihad solves the binding problem of how contemporary Muslims can remain authentically Islamic while engaging with modern ethical frameworks, human rights norms, and scientific understanding. It coordinates Muslims across diverse contexts (women, LGBTQ+ individuals, minorities, modernizing states) around the principle that fidelity to Islam means fidelity to Quranic ethical intent, not to medieval literal applications. It enables religious belonging without requiring abandonment of contemporary ethical commitments.
% TRANSFER_FUNCTION: Moves interpretive authority from classical fiqh schools and their hereditary gatekeepers to progressive scholars, reformist institutions, and contextually-situated reasoning. Transfers cultural legitimacy from traditionalist gatekeeping to ijtihad-as-dynamic-engagement. In the context of legal modernization, transfers the capacity to align Islamic law with human rights and constitutional norms from secular legal systems alone to reformist Islamic jurisprudence. Transfers voice and authority to women, minorities, and LGBTQ+ Muslims who were silenced under literalist frameworks.
% ABSENT_VOICES: Traditionalist and literalist scholars who hold the core premise that classical rulings reflect eternal divine will are structurally excluded from setting the agenda within reformist institutions. Salafi movements and conservative madrasa networks are not in the conversation where reformist interpretive standards are determined. They would object that contextualizing ethics opens the door to relativism and that the Quran itself commands obedience to the Sunnah. Their absence reflects institutional gatekeeping by reformist scholars and progressive institutions.
% DISAPPEARANCE_RATIONALE: If reformist ijtihad as a mandated methodology disappeared and traditionalist taqlid reasserted full interpretive authority, Muslim-majority legal systems would face renewed pressure to criminalize same-sex relations, restrict women's legal equality, and treat non-Muslims as permanently subordinate. Women's movements, LGBTQ+ organizations, and minority rights advocates would lose institutional cover within Islamic jurisprudence and would be forced to frame rights claims as secular rather than authentically Islamic. States that have adopted reformist family law would face legitimacy challenges. The global Muslim intelligentsia would reorganize around whether Islamic identity is compatible with contemporary ethics — the constraint shapes that fundamental question.
% FOUNDING_PROBLEM: Early 20th-century Islamic modernist movements (Abduh, Rida, Iqbal) identified that literal application of medieval fiqh to contemporary contexts produced incoherent results: rules designed for 7th-century social structures manifestly harmed people in modern settings. They argued that the Quran itself models contextual reasoning (ijtihad) and that freezing interpretation in medieval consensus violated the Quranic ethical trajectory. The founding problem is the tension between Islamic authenticity and contemporary ethical knowledge.
% FOUNDING_PROBLEM_CORROBORATION: Progressive Muslim scholars and human rights organizations testify that the founding problem is live: millions of Muslims experience impossible choices between religious belonging and ethical commitments. Traditionalist scholars counter that the founding problem is a false dilemma — they argue that classical fiqh is not incoherent but contextually coherent for its time, and that Muslims must adapt their ethics, not their jurisprudence. Secular-left critics testify that the problem exists but argue the reformist solution does not go far enough (that full secularization is required). The reformist scholars' reading is corroborated by lived testimony of gender-justice advocates and LGBTQ+ Muslim organizing outside the institutional beneficiary class.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__reformist_ijtihad, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__reformist_ijtihad, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__reformist_ijtihad, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_hadith_substrate__reformist_ijtihad, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__reformist_ijtihad, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_hadith_substrate__reformist_ijtihad_tests).
:- end_tests(quran_hadith_substrate__reformist_ijtihad_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42, growing to peak at t30 then stabilizing) because the constraint both solves a real coordination problem (Muslim authenticity + contemporary ethics) and asymmetrically transfers interpretive authority away from classical gatekeepers. The rise from t0 (0.25) to t30 (0.43) reflects increasing institutional consolidation of reformist ijtihad and sharpening defensiveness from traditionalist structures — the more established the reform reading becomes, the more it must actively suppress traditionalist alternatives to maintain legitimacy. The plateau and slight decline from t30 to t50 reflects traditionalist counter-mobilization hardening but not reversing the reformist advance — the constraint reaches an equilibrium of mutual institutional entrenchment. Suppression is moderate-high (0.48) because the constraint's persistence depends on excluding traditionalist gatekeepers from reformist institutional spaces and on suppressing the counter-claim that literalism is authentic Islam. The suppression rises from t0 (0.31) through t30 (0.51) as the conflict intensifies, then stabilizes as both sides establish institutional strongholds. Theater_ratio is moderate (0.28) because reformist ijtihad is a genuine interpretive method (not pure performance) but faces pressure to perform methodological rigor when traditionalists question its coherence — scholars must increasingly defend the consistency of ethical contextualization to maintain credibility. Accessibility_collapse is moderate (0.61 mean): beneficiaries like women and LGBTQ+ Muslims see reformist ijtihad as the primary access point to Islamic belonging (high collapse of traditionalist alternatives), but because reformist institutions and arguments are still contested, the alternatives are not entirely foreclosed — traditionalist spaces remain accessible to those who choose them. Resistance is high (0.72) because traditionalist movements, literalist scholars, and conservative Muslim populations mount strong active resistance — the constraint meets organized counter-mobilization, not passive drift. The one-shared grid ensures every metric is authored at every time point; resistance and suppression are aligned upward because both the traditionalist counter-mobilization and reformist institutional defensiveness intensify together.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats perceive structurally different constraints. From the progressive scholar seat: the constraint is genuine coordination (solving the binding problem of Muslim identity + ethics) and the transfer of authority from medieval gatekeeping to contextual reasoning is natural refinement, not extraction. From the traditionalist seat: the constraint is methodological extraction — taking authority earned through centuries of textual mastery and reassigning it to lawyers, activists, and political actors with no traditional training. From the woman's seat: the constraint is enabling (opens space for rights-compatible belonging) AND extractive (reformist scholars and states gain authority over how women's rights are defined within Islam — women do not set the reformist agenda, they participate in it). From the literalist seat: the constraint is pure suppression — a fundamentalist claim that ethics trumps revealed law. The engine's per-seat classification will show these divergences: progressive scholars compute toward coordination/rope-beneficiary territory; traditionalist gatekeepers and literalists compute toward snare/piton territory (extraction riding institutional inertia); women and minorities compute toward tangled_rope-beneficiary (genuine coordination with subordinated participation).
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive scholars: d near 0.0 (full beneficiary) — they gain interpretive authority, institutional positioning, and legitimacy from the constraint. Their power is institutional (they control reformist centers, academic networks, state advisory positions), so directionality amplifies their subsidy. Women/LGBTQ+/minorities: d near 0.3-0.5 depending on exit options. Women in rights-aligned jurisdictions have higher exit (if women's rights law exists secularly, they could exit Islam, though identity-lock prevents it) — d shifts upward slightly. LGBTQ+ Muslims have identity_locked exit, so they cannot arbitrage to secular rights frameworks — d stays low despite moderate power. Traditional gatekeepers: d near 1.0 (full target) — they are the ones from whom authority is transferred, even though they retain institutional control in some regions. Their constrained exit (career path locked to classical fiqh) and institutional-but-declining power amplifies extraction. Literalist movements: d near 1.0 (full target) — they are the direct victims of the constraint's suppression of their methodological claims. Excluded from reformist decision-making, they mount costly counter-mobilization. States: d near 0.2 (strong beneficiary) — they gain legal legitimacy and rights alignment without secularizing, and they have arbitrage (they could adopt pure secular law or remain traditionalist — they choose reformist cover), so d stays low despite institutional power. The engine will compute per-seat types from this directionality: progressive scholars and states will appear as net beneficiaries of coordination; traditional gatekeepers and literalists as targets of asymmetric extraction; beneficiary groups (women, minorities) as partially captured but with some coordination surplus.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids the mandatrophy trap (claim that founding problem is dead, but constraint persists) by virtue of contested founding_problem_status. The founding problem (tension between Islamic authenticity and contemporary ethics) remains LIVE — millions of Muslims experience it daily, and the two sibling readings (traditionalist taqlid, state-hybrid) continue to offer competing solutions. The constraint persists because the problem persists, not because the problem died and institutional inertia froze the solution. However, there is a secondary mandatrophy risk: IF the founding problem were to resolve (e.g., if a global consensus emerged that either ethics or Islamic law must yield completely), the reformist ijtihad constraint might persist theatrically as an institutional identity marker rather than as a solution to a live binding problem. Theater_ratio rising from t20 to t30 (0.21 to 0.29) hints at this: as the constraint becomes more entrenched, a growing share of its institutional practice is performance (defending methodological consistency, maintaining boundaries with traditionalists) rather than solving the original problem. The constraint does not yet exhibit classic piton indicators (beneficiaries = zero, only inertia remains) — beneficiaries are real and concentrated — but continued theater_ratio rise could signal drift toward piton status in the longer term.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_mechanism_traditionalist,
    'Is traditionalist fiqh gatekeeping maintained primarily through structural institutional barriers (madrasa control, appointment authority, credentialing monopoly) or through internalized professional identity so fused with classical methodology that gatekeepers could not cognitively access alternatives?',
    'Post-institutional-pressure trajectories: if traditionalist scholars develop novel arguments after losing institutional authority, suppression was structural; if they remain committed to classical positions despite institutional loss, suppression was partly internalized.',
    'If primarily internalized, the constraint''s effective suppression on traditionalist seats is higher than the structural measure suggests — gatekeepers carry the constraint with them after exit. If primarily structural, focusing on institutional reform could partially resolve the asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_traditionalist, empirical, 'Structural vs. internalized suppression of traditionalist interpretive authority').

omega_variable(
    kernel_contest_methodological_legitimacy,
    'Is this constraint one reading of a contested kernel (the Quran-hadith substrate and how to adjudicate between them), or is it a claim about what the kernel''s correct reading is? Does the reformist ijtihad reading assert it is the ONLY legitimate reading, or does it acknowledge multiple readings coexist?',
    'Textual analysis of reformist scholars'' framing: do they claim ijtihad is mandatory when texts conflict, or do they claim it is permissible? Do they argue traditionalist taqlid is illegitimate, or historically superseded? Where do they locate the fault line between legitimate diversity and illegitimate heresy?',
    'If the reading claims to be the sole legitimate interpretation, it is not merely one reading but a totalizing claim — the engine would flag higher extraction at the structural level (methodological monopoly). If it claims ijtihad is mandatory in some contexts and taqlid permissible in others, it is a genuine coexisting reading with demarcation rules — lower extraction, higher legitimacy as an alternative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_methodological_legitimacy, conceptual, 'Whether reformist ijtihad claims exclusivity or coexistence with traditionalist approaches').

omega_variable(
    women_exit_constitution,
    'For women who benefit from reformist ijtihad, what portion of the measured accessibility_collapse reflects inability to exit the constraint (remaining Muslim while traditionalist rulings bind) versus unwillingness to exit (identity-locked to Islam)?',
    'Qualitative mapping of women''s reasoning about Muslim identity: how many frame reformist ijtihad as the only way to remain authentically Muslim? How many would remain Muslim if traditionalist rulings were somehow made avoidable? How many treat exit from Islam as identity dissolution?',
    'High identity-lock (unwillingness to exit) indicates that even if traditionalist rulings were technically avoidable, beneficiaries would choose the constraint over exit — they need reformist ijtihad to exist. Low identity-lock indicates the constraint''s persistence depends more on institutional barriers than on beneficiary commitment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(women_exit_constitution, empirical, 'Identity fusion vs. institutional trapping in women''s relationship to the constraint').

omega_variable(
    literalist_counter_mobilization_growth,
    'Over the interval (t0=0 to tn=50), is traditionalist-literalist counter-mobilization growing stronger (gaining institutional resources, expanding madrasa networks, recruiting scholars), staying stable, or declining?',
    'Institutional audit: track madrasa enrollment trends, scholarly output from salafi networks, funding sources for traditionalist institutions, demographic shifts in conservative Islamic education. Track the measurement inflection: resistance is high and stable throughout; if counter-mobilization is growing, is theater_ratio also rising (traditionalist gatekeepers becoming more performative)?',
    'If counter-mobilization is strengthening, the constraint is riding a wave and vulnerable to reversal — effective extraction measured at t50 may not persist beyond the interval. If stable or declining, the constraint may be settling into institutional equilibrium.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(literalist_counter_mobilization_growth, empirical, 'Trajectory of traditionalist institutional power relative to reformist institutional anchoring').

omega_variable(
    reformist_ijtihad_internal_coherence,
    'Do progressive scholars using reformist ijtihad converge on a stable, predictable methodology for balancing Quranic ethics against contemporary contexts, or do different reformist schools produce contradictory conclusions from the same ethical principles?',
    'Comparative jurisprudence: examine multiple reformist rulings on the same issue from different scholars/schools (e.g., family law, sexual ethics, religious freedom). Do they reach the same conclusion? If not, does the disagreement follow consistent methodological rules, or is the output indeterminate?',
    'High internal coherence suggests the constraint is sustainable as a genuine alternative jurisprudential method. Low coherence (outcomes are scholar-dependent or arbitrary) suggests reformist ijtihad may function more as rhetoric cover for individual judgment than as a transferable interpretive system — this would reduce its credibility and increase theater_ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformist_ijtihad_internal_coherence, conceptual, 'Methodological stability of reformist ijtihad across practitioners and contexts').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__reformist_ijtihad, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 0, 0.12).
narrative_ontology:measurement(qura_tr_t10, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 10, 0.16).
narrative_ontology:measurement(qura_tr_t20, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 20, 0.21).
narrative_ontology:measurement(qura_tr_t30, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 30, 0.29).
narrative_ontology:measurement(qura_tr_t40, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 40, 0.31).
narrative_ontology:measurement(qura_tr_t50, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(qura_be_t10, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(qura_be_t20, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(qura_be_t30, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 30, 0.43).
narrative_ontology:measurement(qura_be_t40, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 40, 0.41).
narrative_ontology:measurement(qura_be_t50, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 50, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 0, 0.31).
narrative_ontology:measurement(qura_su_t10, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(qura_su_t20, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(qura_su_t30, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 30, 0.51).
narrative_ontology:measurement(qura_su_t40, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 40, 0.49).
narrative_ontology:measurement(qura_su_t50, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 50, 0.48).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=50
narrative_ontology:measurement(qura_grid_01, quran_hadith_substrate__reformist_ijtihad, accessibility_collapse(class), 0, 0.62).
narrative_ontology:measurement(qura_grid_02, quran_hadith_substrate__reformist_ijtihad, accessibility_collapse(class), 50, 0.72).
narrative_ontology:measurement(qura_grid_03, quran_hadith_substrate__reformist_ijtihad, accessibility_collapse(individual), 0, 0.48).
narrative_ontology:measurement(qura_grid_04, quran_hadith_substrate__reformist_ijtihad, accessibility_collapse(individual), 50, 0.64).
narrative_ontology:measurement(qura_grid_05, quran_hadith_substrate__reformist_ijtihad, accessibility_collapse(organizational), 0, 0.58).
narrative_ontology:measurement(qura_grid_06, quran_hadith_substrate__reformist_ijtihad, accessibility_collapse(organizational), 50, 0.68).
narrative_ontology:measurement(qura_grid_07, quran_hadith_substrate__reformist_ijtihad, accessibility_collapse(structural), 0, 0.68).
narrative_ontology:measurement(qura_grid_08, quran_hadith_substrate__reformist_ijtihad, accessibility_collapse(structural), 50, 0.82).
narrative_ontology:measurement(qura_grid_09, quran_hadith_substrate__reformist_ijtihad, resistance(class), 0, 0.74).
narrative_ontology:measurement(qura_grid_10, quran_hadith_substrate__reformist_ijtihad, resistance(class), 50, 0.77).
narrative_ontology:measurement(qura_grid_11, quran_hadith_substrate__reformist_ijtihad, resistance(individual), 0, 0.68).
narrative_ontology:measurement(qura_grid_12, quran_hadith_substrate__reformist_ijtihad, resistance(individual), 50, 0.71).
narrative_ontology:measurement(qura_grid_13, quran_hadith_substrate__reformist_ijtihad, resistance(organizational), 0, 0.72).
narrative_ontology:measurement(qura_grid_14, quran_hadith_substrate__reformist_ijtihad, resistance(organizational), 50, 0.75).
narrative_ontology:measurement(qura_grid_15, quran_hadith_substrate__reformist_ijtihad, resistance(structural), 0, 0.68).
narrative_ontology:measurement(qura_grid_16, quran_hadith_substrate__reformist_ijtihad, resistance(structural), 50, 0.69).
narrative_ontology:measurement(qura_grid_17, quran_hadith_substrate__reformist_ijtihad, stakes_inflation(class), 0, 0.54).
narrative_ontology:measurement(qura_grid_18, quran_hadith_substrate__reformist_ijtihad, stakes_inflation(class), 50, 0.73).
narrative_ontology:measurement(qura_grid_19, quran_hadith_substrate__reformist_ijtihad, stakes_inflation(individual), 0, 0.42).
narrative_ontology:measurement(qura_grid_20, quran_hadith_substrate__reformist_ijtihad, stakes_inflation(individual), 50, 0.58).
narrative_ontology:measurement(qura_grid_21, quran_hadith_substrate__reformist_ijtihad, stakes_inflation(organizational), 0, 0.58).
narrative_ontology:measurement(qura_grid_22, quran_hadith_substrate__reformist_ijtihad, stakes_inflation(organizational), 50, 0.71).
narrative_ontology:measurement(qura_grid_23, quran_hadith_substrate__reformist_ijtihad, stakes_inflation(structural), 0, 0.68).
narrative_ontology:measurement(qura_grid_24, quran_hadith_substrate__reformist_ijtihad, stakes_inflation(structural), 50, 0.81).
narrative_ontology:measurement(qura_grid_25, quran_hadith_substrate__reformist_ijtihad, suppression(class), 0, 0.35).
narrative_ontology:measurement(qura_grid_26, quran_hadith_substrate__reformist_ijtihad, suppression(class), 50, 0.51).
narrative_ontology:measurement(qura_grid_27, quran_hadith_substrate__reformist_ijtihad, suppression(individual), 0, 0.28).
narrative_ontology:measurement(qura_grid_28, quran_hadith_substrate__reformist_ijtihad, suppression(individual), 50, 0.42).
narrative_ontology:measurement(qura_grid_29, quran_hadith_substrate__reformist_ijtihad, suppression(organizational), 0, 0.42).
narrative_ontology:measurement(qura_grid_30, quran_hadith_substrate__reformist_ijtihad, suppression(organizational), 50, 0.54).
narrative_ontology:measurement(qura_grid_31, quran_hadith_substrate__reformist_ijtihad, suppression(structural), 0, 0.51).
narrative_ontology:measurement(qura_grid_32, quran_hadith_substrate__reformist_ijtihad, suppression(structural), 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__reformist_ijtihad, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(quran_hadith_substrate__reformist_ijtihad, 0.18).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate__traditionalist_taqlid).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate__state_hybrid).

% DUAL FORMULATION NOTE:
% Reformist ijtihad is one of three coexisting readings of the Quran-hadith substrate kernel. It shares the referent (the standing tension between Quranic principles and hadith rulings) with traditionalist taqlid and state-hybrid, but authors a different ε (moderate extractiveness 0.42 for this reading's instantiation of the constraint) and different beneficiary/victim structure (benefits progressive Muslims and rights-aligned constituencies; extracts from traditionalist gatekeepers). The three constraint stories are linked by network.affects_constraints: each reading influences the others by competing for institutional authority and by shaping what counts as legitimate Islamic jurisprudence. Traditionalist taqlid would show high suppression and lower ε (enforcement of interpretive monopoly) but with different payers/beneficiaries (traditionalists as beneficiaries). State-hybrid would show political pragmatism replacing pure jurisprudential logic, different victims (constituencies unable to access whichever law-system serves them), and institutional power concentrated in states rather than scholars.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
