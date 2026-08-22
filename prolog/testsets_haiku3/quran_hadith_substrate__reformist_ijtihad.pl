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
 *   constraint_id: quran_hadith_substrate__reformist_ijtihad
 *   human_readable: Contextual Ijtihad Mandate vs. Classical Fiqh Authority
 *   domain: religious/legal/ethical
 *
 * SUMMARY:
 *   Islamic jurisprudence stands at a doctrinal crossroads. Classical fiqh
 *   schools developed sophisticated jurisprudence for medieval conditions;
 *   contemporary Muslims face challenges—gender equality, LGBTQ dignity,
 *   religious pluralism, human rights norms—that classical texts did not
 *   explicitly address. This constraint describes ONE READING of the
 *   contested kernel (quran_hadith_substrate): the reformist ijtihad reading,
 *   which mandates contextual reinterpretation when classical rulings
 *   conflict with Quranic ethical principles, contemporary human rights
 *   norms, or public interest (maslaha). This reading benefits progressive
 *   scholars, women, LGBTQ individuals, and religious minorities by creating
 *   doctrinal space for equity-oriented interpretations. It extracts from
 *   traditionalist authority structures—classical fiqh gatekeepers and
 *   conservative ulama networks—by undermining their claim to interpretive
 *   monopoly. The claim/metric gap is intentional: reformists CLAIM this
 *   reading as coordination (solving how Islam responds to modernity); the
 *   authored metrics describe moderately extractive, actively enforced
 *   operation (suppression of traditionalist alternatives). The engine
 *   computes this divergence per-seat; the story does not reconcile claim to
 *   metrics.
 *
 * KEY AGENTS:
 *   - Progressive Muslim scholars: agenda-setters and beneficiaries; drive reformist ijtihad through publications, universities, policy councils
 *   - Women, LGBTQ individuals, religious minorities: beneficiaries seeking equity-oriented reinterpretations of classical rulings
 *   - Traditionalist authority structures: institutional payers whose gatekeeping legitimacy depends on taqlid (following established precedent)
 *   - Classical fiqh scholars and conservative ulama networks: payer seats; lose interpretive monopoly as reformist readings gain institutional traction
 *   - State actors: excluded but materially influential; determine whether reformist or traditionalist readings gain curriculum, funding, judicial authority
 *   - International human rights bodies: excluded from doctrinal argument but set external standards that beneficiaries leverage
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__reformist_ijtihad, 0.42).
domain_priors:suppression_score(quran_hadith_substrate__reformist_ijtihad, 0.48).
domain_priors:theater_ratio(quran_hadith_substrate__reformist_ijtihad, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, extractiveness, 0.42).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__reformist_ijtihad, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__reformist_ijtihad, "Contextual Ijtihad Mandate vs. Classical Fiqh Authority").
narrative_ontology:topic_domain(quran_hadith_substrate__reformist_ijtihad, "religious/legal/ethical").

domain_priors:requires_active_enforcement(quran_hadith_substrate__reformist_ijtihad).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__reformist_ijtihad, '8eef16af-52d4-4158-8fe7-7245e46936da').
narrative_ontology:cs_kernel_codification('8eef16af-52d4-4158-8fe7-7245e46936da', formalized).
narrative_ontology:cs_authority_grounding('8eef16af-52d4-4158-8fe7-7245e46936da', lineage).
narrative_ontology:cs_interpretation_layer_present('8eef16af-52d4-4158-8fe7-7245e46936da').
narrative_ontology:cs_reading_relation('8eef16af-52d4-4158-8fe7-7245e46936da', quran_hadith_substrate__traditionalist_taqlid, coexists_with).
narrative_ontology:cs_reading_relation('8eef16af-52d4-4158-8fe7-7245e46936da', quran_hadith_substrate__state_hybrid, influences).
narrative_ontology:cs_axiom('8eef16af-52d4-4158-8fe7-7245e46936da', foundational, contextual_reinterpretation_is_mandatory).
narrative_ontology:cs_axiom_status(contextual_reinterpretation_is_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('8eef16af-52d4-4158-8fe7-7245e46936da', contextual_reinterpretation_is_mandatory, deontological).
narrative_ontology:cs_axiom('8eef16af-52d4-4158-8fe7-7245e46936da', foundational, quranic_ethical_trajectory_trumps_hadith_literalism).
narrative_ontology:cs_axiom_status(quranic_ethical_trajectory_trumps_hadith_literalism, holdable).
narrative_ontology:cs_axiom_grounding('8eef16af-52d4-4158-8fe7-7245e46936da', quranic_ethical_trajectory_trumps_hadith_literalism, deontological).
narrative_ontology:cs_axiom('8eef16af-52d4-4158-8fe7-7245e46936da', secondary, maslaha_obligates_contemporary_reinterpretation).
narrative_ontology:cs_axiom_status(maslaha_obligates_contemporary_reinterpretation, holdable).
narrative_ontology:cs_axiom_grounding('8eef16af-52d4-4158-8fe7-7245e46936da', maslaha_obligates_contemporary_reinterpretation, instrumental).
narrative_ontology:cs_reference_frame('8eef16af-52d4-4158-8fe7-7245e46936da', quran_hadith_classical_fiqh_synthesis).
narrative_ontology:cs_drift_state('8eef16af-52d4-4158-8fe7-7245e46936da', contemporary_human_rights_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('8eef16af-52d4-4158-8fe7-7245e46936da', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, progressive_muslim_scholars).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, women_seeking_equity_rulings).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, lgbtq_individuals).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, religious_minorities).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, human_rights_advocates_within_islam).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, traditionalist_authority_structures).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, classical_fiqh_gatekeepers).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, conservative_ulama_networks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, women_seeking_equity_rulings).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lead the reformist ijtihad movement through publications, academic platforms, policy advisory roles, and teaching. They argue contextual reinterpretation is mandatory when classical rulings conflict with Quranic ethics, contemporary human rights, or public interest. They benefit from legitimacy derived from Quranic hermeneutics and gain institutional voice in universities, Islamic councils, and policy discussions. They set the agenda in diaspora and progressive institutions but remain embattled in traditionalist-majority regions.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, progressive_muslim_scholars, agenda_setter,
    moderate, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__reformist_ijtihad, progressive_muslim_scholars, beneficiary).

% Seek rulings on marriage, inheritance, testimony, guardianship, and employment that reflect equal moral and legal status. Reformist ijtihad creates space for reinterpretations (e.g., re-contextualizing historical hadith on women's testimony as addressing 7th-century conditions rather than transcendent law). They benefit from increased dignity and legal autonomy within Islamic frameworks. They also pay the cost of contention with traditionalist communities and the vulnerability of rulings that lack institutional consensus.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, women_seeking_equity_rulings, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__reformist_ijtihad, women_seeking_equity_rulings, payer).

% Face explicit prohibition in classical fiqh (prohibition of same-sex acts, enforcement through classical hudud and ta'zir frameworks). Reformist ijtihad creates analytical space for reframing: drawing on maqasid al-shariah (objectives of the law), recontextualizing historical rulings as reflecting pre-modern social arrangements, and prioritizing Quranic themes of dignity and non-harm. The beneficiary position is precarious because reformist readings offer theological legitimacy but carry no enforcement power in predominantly traditionalist communities. Their identity-lock (Islamic identity fused with personal identity) makes exit into secular frameworks impossible for many.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, lgbtq_individuals, beneficiary,
    powerless, biographical, identity_locked, global).

% Non-Muslim minorities within Muslim-majority states face classical fiqh rulings on dhimmi status, religious freedom, and citizenship rights. Reformist ijtihad enables reinterpretations grounding minority protections in Quranic pluralism, historical coexistence, and maslaha arguments for inclusive citizenship. Trapped by geography and structural position; their protection depends entirely on reformist mobilization against traditionalist institutional dominance.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, religious_minorities, beneficiary,
    powerless, generational, trapped, global).

% Bridge Islamic scholarship and international human rights norms. They use reformist ijtihad as the primary lever to argue that Islamic jurisprudence can accommodate universal human rights without abandoning Islamic identity. They benefit from the constraint's existence (it provides legitimacy for their advocacy) but also bear the cost of contention: traditionalist communities reject their readings as Western imposition; Western frameworks sometimes view Islamic grounding as insufficient compromise.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, human_rights_advocates_within_islam, beneficiary,
    moderate, generational, mobile, global).

% Classical fiqh schools, traditionalist religious hierarchies, and institutional gatekeepers of Islamic jurisprudence. Reformist ijtihad directly undermines their claim to exclusive interpretive authority: if reinterpretation is mandated to respond to contemporary ethics, their role as transmitters of frozen precedent loses legitimacy. They bear the cost of losing interpretive monopoly, institutional relevance in progressive regions, and the ability to exclude lay reinterpretation. Constrained exit because their entire institutional investment depends on traditionalist legitimacy.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, traditionalist_authority_structures, payer,
    institutional, generational, constrained, regional).

% Scholars and institutions whose authority derives from mastery of medieval jurisprudential texts and their application without recontextualization. The reformist mandate directly opposes their framing: reformists argue taqlid is obsolete when rulings contradict Quranic ethics or harm public interest; gatekeepers argue taqlid is obligatory because classical schools reached consensus. They lose institutional status and publishing platforms as reformist readings gain traction in universities and policy councils.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, classical_fiqh_gatekeepers, payer,
    powerful, generational, constrained, regional).

% Networks of conservative scholars across Muslim-majority regions whose livelihood, reputation, and institutional position depend on defending classical rulings as binding. Reformist ijtihad threatens their authority within their own communities and creates internal fragmentation (younger scholars defect to reformist frameworks). Identity-locked because their entire professional identity is built on doctrinal continuity; they cannot exit without professional self-dissolution.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, conservative_ulama_networks, payer,
    organized, generational, identity_locked, regional).

% National governments that design family law, criminal codes, and religious curricula. Formally excluded from the reformist ijtihad doctrinal debate itself, but materially influential: states that mandate religious curricula, fund universities, appoint official muftis, and adjudicate family law determine whether reformist or traditionalist readings gain institutional resources and legal force. Their absence from direct argument preserves their ability to claim neutrality while materially favoring one side.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, state_actors_in_muslim_majority_regions, excluded,
    institutional, generational, mobile, national).

% UN bodies, NGOs, and Western governments that monitor Islamic jurisprudence through a human rights lens. Formally excluded from Islamic doctrinal argument but materially influential: their public criticism of classical rulings (on women's testimony, LGBTQ prohibition, criminal law) creates political pressure that beneficiaries of reformist ijtihad leverage. Their exclusion from theological argument creates asymmetry: they set an external standard without participating in internal reasoning.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, international_human_rights_bodies, excluded,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_hadith_substrate__reformist_ijtihad, progressive_muslim_scholars).
narrative_ontology:fixing_cost_class(quran_hadith_substrate__reformist_ijtihad, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework in which Islamic jurisprudence can integrate contemporary ethical norms, human rights principles, and evolved understandings of public interest without abandoning Islamic legitimacy. Solves the coordination problem: how can Muslims maintain doctrinal identity while responding to historical change, contextual variation, and new challenges (gender equality, LGBTQ dignity, religious pluralism) that classical texts did not explicitly address?
% TRANSFER_FUNCTION: Transfers interpretive authority from institutional gatekeepers (classical fiqh schools, traditionalist ulama networks) to contextually-informed scholars who foreground Quranic principles and maslaha. Also transfers legitimacy and institutional resources (university positions, policy influence, publishing platforms) from traditionalist frameworks to reformist ones, and gains for women, LGBTQ individuals, and religious minorities in legal recognition and dignity framing.
% ABSENT_VOICES: Traditionalist ulama and conservative religious authorities are structurally marginalized in spaces where reformist ijtihad dominates (diaspora universities, progressive Islamic councils, human rights advocacy). Conversely, in traditionalist-majority regions and institutions, reformist voices are excluded or suppressed. The broader Muslim-majority populations (non-scholars) whose daily life is governed by jurisprudence are not participants in the doctrinal argument; their interests are claimed by both sides but their voice is absent. State actors that design family law codes operate in the shadows of this debate, excluded from doctrinal argument but materially determining which readings gain institutional force.
% DISAPPEARANCE_RATIONALE: If the reformist ijtihad mandate vanished, classical fiqh gatekeeping would re-concentrate; women's inheritance and testimony would revert to literalist applications; LGBTQ individuals would lose the one Islamic-grounded argument for dignity; religious minorities would lose a source of pluralist reinterpretation; progressive scholars would lose institutional legitimacy and publishing platforms. The Muslim world would reorganize around traditionalist authority structures, and the doctrinal landscape would narrow. Conversely, traditionalist authority structures would recover interpretive monopoly, and their institutional power would reconcentrate.
% FOUNDING_PROBLEM: Classical fiqh schools developed sophisticated jurisprudence adapted to medieval conditions. As Muslims entered modernity—colonization, nation-states, industrial economies, democratic theory, human rights frameworks—classical rulings on women, minorities, and criminal law increasingly conflicted with lived ethics and contemporary public interest. The founding problem: Islamic jurisprudence was locked in a past-oriented framework (taqlid) that could not respond to new conditions without internal contradiction or irrelevance.
% FOUNDING_PROBLEM_CORROBORATION: Progressive scholars and rights advocates attest the founding problem is live: women and minorities still face jurisprudential barriers to equality that cannot be squared with contemporary understandings of human dignity. Traditionalist ulama attest the founding problem is a false framing: the problem is not classical jurisprudence but the corruption of Islamic society by Western values; the solution is deeper commitment to traditional rulings, not their abandonment. Independent sociologists and legal scholars (non-Muslim and Muslim) document that gender inequality, religious discrimination, and criminalization of LGBTQ identity in Islamic legal systems reflect historical rulings that conflict with internationally-recognized human rights standards. The conflict between these attestations is the kernel contest itself.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__reformist_ijtihad, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__reformist_ijtihad, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__reformist_ijtihad, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.42 at interval end, declining from 0.58 at start). This reflects the reading's contested status: in contexts where reformist scholars hold institutional power (diaspora universities, progressive Islamic councils), extraction is lower (~0.42) because beneficiaries have meaningful voice and traditionalist suppression is weaker. In traditionalist-majority regions, extraction would be higher because reformist readings are marginalized. The measurement series captures this institutional volatility: extractiveness declines from t0 to t15 (peak reformist institutional presence), then stabilizes or slightly rises (t25-40, reflecting conservative counter-mobilization and regional variation). Suppression follows a similar arc: it starts high (0.65, traditionalist gatekeeping dominates) and declines as reformist frameworks gain platforms, then stabilizes or rises slightly as traditionalist resistance hardens. Theater ratio is moderate (0.31 at end): reformist ijtihad includes genuine doctrinal work (Quranic hermeneutics, maqasid reasoning) alongside performative claims about 'Islamic authenticity' that mask power interests. Accessibility collapse is moderate (0.52): alternatives (traditionalist taqlid, secular nationalism) remain live for beneficiaries, though the reformist reading creates substantial pressure. Resistance is high (0.71) because traditionalist actors actively defend classical authority and contest reformist reinterpretations.
 *
 * PERSPECTIVAL GAP:
 *   Massive divergence expected across seats. From reformist scholars' position: the arrangement is genuine coordination solving how Islam integrates modernity without losing identity—low extractiveness, functional suppression of only illegitimate alternatives (literalism, interpretive monopoly). From traditionalist gatekeepers' position: the arrangement is enforced reinterpretation that strips their authority and imposes alien ethics—high extractiveness, unjust suppression of legitimate Islamic learning. From beneficiary seats (women, LGBTQ, minorities): extraction is low and suppression is directed at oppressive classical rulings, not at legitimate voices—they compute low extractiveness from the constraint on traditional authority. From excluded state actors: the arrangement is uncontrollable doctrinal fragmentation that prevents unified jurisprudence—moderate extractiveness, inadequate suppression to maintain coherent law. The engine computes these divergences from the authored structural data; no single seat's perspective is privileged.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality flows from beneficiary/victim declarations and exit options. Progressive scholars benefit from the mandate (they set the agenda, gain platforms, legitimize their interpretations) but are also payers (they invest effort, face traditionalist backlash, carry risk of doctrinal rejection if reformist readings fail to gain consensus). Their d is moderate-to-low (around 0.35-0.45): they are net beneficiaries but not pure targets; they have mobile exit options (can shift to secular advocacy, academic philosophy) and power within their institutional niches. Women, LGBTQ, minorities benefit substantially but are highly constrained (identity-locked in many cases, trapped by geography); their d is near the beneficiary end (0.15-0.25) despite constraints because the constraint's primary function is to benefit them. Traditionalist authority structures are pure targets (d near 1.0): they lose interpretive monopoly, institutional status, and have constrained exit (their identity and livelihood depend on defending classical rulings). Suppression is moderate (0.48) because reformist advocacy operates through debate, institutional competition, and policy influence, not through overt coercion—but traditionalist platforms are actively de-funded in progressive contexts, and reformist readings are banned or marginalized in conservative regions. The asymmetry: reformists suppress alternatives by shifting institutional resources and epistemic standards (what counts as legitimate scholarship); traditionalists suppress through community pressure and state backing (in regions where they retain power).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is NOT obsolete; rather, it is permanently contested. The founding problem—how Islam responds to modernity while preserving identity—is live and will remain live across the measurement interval (t0-40 spans ~25 years of projected change). This is not a solved problem that the constraint now merely performs around; it is an active dispute with real stakes. The mandatrophy risk is conditional: if traditionalist counter-mobilization succeeds in re-establishing doctrinal monopoly (a live possibility in conservative regions, especially with state backing), the reformist mandate becomes theatrical—it persists as a claim by diaspora and progressive minorities but lacks institutional force. Conversely, if reformist institutions fully displace traditionalist ones, the mandate dissolves because the coordination problem (how to integrate ethics without losing identity) is solved and no longer requires active enforcement. The current state (t15, ~2040) is equilibrium: both readings maintain institutional footholds; neither has won definitively. The constraint's persistence depends on ongoing contention, not on solution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_vs_reading_distinction,
    'Is this constraint better understood as ONE reading of a contested kernel (quran_hadith_substrate), or as an independent claim about how Islamic jurisprudence ought to operate?',
    'Examine whether rejecting the reformist reading requires rejecting the Quran/Hadith foundation itself, or whether it only requires accepting a different reading (traditionalist taqlid). If the latter, the kernel/reading distinction holds; if the former, this is a higher-level constraint.',
    'If a genuine kernel reading: the constraint''s classification reflects one equilibrium in an ongoing doctrinal contest with no final resolution. If an independent claim: the constraint would classify differently (its extraction would reflect institutional dominance rather than kernel-reading structure). Kernel reading status affects the interpretation of divergence across seats and the meaning of traditionalist suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_vs_reading_distinction, conceptual, 'Whether this constraint is one reading of a contested kernel or an independent claim about Islamic jurisprudence.').

omega_variable(
    extraction_measurement_ambiguity,
    'Is the measured extraction (0.42) the cost to traditionalist authority structures, or the cost to beneficiary seats, or a system-level measure?',
    'Disaggregate extraction per seat: measure the cost traditionalist gatekeepers experience (higher) vs. the benefit progressive scholars and marginalized beneficiaries experience (negative cost, i.e., subsidy). The system-level 0.42 is a weighted average; seat-level measures would show divergence.',
    'If traditionalist extraction is substantially higher (0.65-0.75), the constraint is better understood as high-extractive from the gatekeeper seat. If beneficiary seats measure negative extraction (genuine benefit), the constraint''s aggregate extraction masks asymmetric transfer. This affects whether reformist claims of ''coordination'' or traditionalist claims of ''injustice'' are validated by seat-level metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_measurement_ambiguity, empirical, 'Whether base_extractiveness 0.42 represents equal burden-sharing or masks asymmetric extraction across seats.').

omega_variable(
    institutional_backing_volatility,
    'How durable is the reformist ijtihad reading''s institutional backing? Can it sustain itself against traditionalist counter-mobilization and state-level defection?',
    'Monitor institutional outcomes over the interval: university curriculum adoption, publication platforms, judicial precedent, state funding allocation, and community-level acceptance. Track whether reformist gains in diaspora and progressive institutions are offset by traditionalist gains in conservative regions.',
    'If backing proves durable, extractiveness remains moderate (~0.42) and the constraint persists as active tangled rope. If backing erodes (state defection, community rejection), extractiveness would rise substantially as reformist readings lose institutional force and become marginalized advocacy. The measurement series would inflect upward after t25.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_backing_volatility, empirical, 'Whether reformist institutional presence is sustainable or vulnerable to traditionalist counter-mobilization.').

omega_variable(
    mandate_vs_doctrine_confusion,
    'Does the ''mandate'' for contextual ijtihad describe an actual institutional requirement (e.g., state law, official mufti declarations), or a normative claim by reformist scholars about what ought to be done?',
    'Survey which Muslim-majority states, official judicial bodies, and established institutions have FORMALLY adopted the reformist mandate as binding law vs. how many reformist scholars simply argue for it without institutional authority. Distinguish de jure mandate from de facto advocacy.',
    'If the mandate is largely de facto (normative advocacy), the constraint''s ''enforcement'' is institutional competition and epistemic pressure, not coercive suppression. Suppression would be lower than authored (maybe 0.35-0.40) because traditionalist alternatives are not formally banned. If de jure (state-backed), suppression would be higher (0.55-0.65) because traditionalist readings would face formal prohibition. The current 0.48 reflects a mixed picture: strong in some regions/institutions, weak in others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_vs_doctrine_confusion, empirical, 'Whether the reformist ijtihad mandate is formally institutionalized or primarily normative advocacy.').

omega_variable(
    reading_foreclosure_risk,
    'Could either the reformist reading or the traditionalist reading logically foreclose the other within the bounds of Islamic commitment, or will they coexist indefinitely?',
    'Examine whether accepting reformist ijtihad as mandatory requires rejecting classical fiqh schools as legitimate, or whether a reformist can hold both (reformed classical learning). Similarly, examine whether traditionalist taqlid requires rejecting contextual reinterpretation entirely. If both can coexist within a single framework, they are truly coexisting readings; if one requires denying the other, foreclosure is possible.',
    'If coexistence is logically stable: the constraint will remain contested indefinitely, and the measurement series should show oscillation rather than monotonic trend. If foreclosure is possible (one reading logically rules out the other): the constraint''s trajectory should show eventual dominance by one reading or the other, and extraction should rise sharply for the losing seat. Current data suggests coexistence; future acceleration toward dominance would signal foreclosure dynamics activating.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_risk, conceptual, 'Whether reformist and traditionalist readings can logically coexist or whether one must eventually foreclose the other.').

omega_variable(
    maslaha_operationalization_ambiguity,
    'What counts as ''maslaha'' (public interest)? Is it determined by scholarly consensus, popular preference, state decree, or universal human rights norms? Different operationalizations would produce different reinterpretations.',
    'Document how reformist scholars define and apply maslaha in specific rulings (e.g., women''s rights, LGBTQ dignity, minority protections). Compare those definitions with how traditionalists define maslaha. Survey whether maslaha judgments converge across reformist scholars or diverge based on geographic/cultural context.',
    'If maslaha is operationalized as universal human rights: reformist reinterpretations will converge globally and suppress traditionalist readings decisively. If operationalized as local consensus or state interest: reinterpretations will diverge by region, and traditionalist alternatives will survive in conservative contexts. This affects whether the constraint operates as global coordination (unified ethical standard) or as fragmented advocacy (competing regional ethics).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maslaha_operationalization_ambiguity, conceptual, 'Whether maslaha-based reinterpretation produces unified global ethical standards or divergent regional outcomes.').

omega_variable(
    committer_frame_kernel_reading_status,
    'Is the reformist ijtihad constraint better understood as one reading of a contested kernel, or as an advocacy position that cuts across kernel/non-kernel boundaries?',
    'Examine whether reformist scholars frame their position as ''reinterpreting the Quran/Hadith foundation'' (kernel reading) or as ''rejecting classical jurisprudence entirely in favor of modern ethics'' (non-kernel advocacy). Survey whether the reading''s legitimacy depends on Quranic grounding or on external human rights norms.',
    'If kernel reading: the constraint operates within Islamic doctrinal bounds and coexists indefinitely with traditionalist readings. If advocacy position: the constraint is higher-level (about whether Islamic jurisprudence should exist at all) and creates sharper bifurcation between Islamic and secular frameworks. This affects whether the measurement series should show equilibrium (kernel contest) or trend toward dominance (advocacy battle).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_kernel_reading_status, conceptual, 'Whether reformist ijtihad is a kernel reading or an advocacy position that transcends kernel boundaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__reformist_ijtihad, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 0, 0.22).
narrative_ontology:measurement(qura_tr_t5, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 5, 0.26).
narrative_ontology:measurement(qura_tr_t10, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 10, 0.28).
narrative_ontology:measurement(qura_tr_t15, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 15, 0.31).
narrative_ontology:measurement(qura_tr_t25, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 25, 0.32).
narrative_ontology:measurement(qura_tr_t35, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 35, 0.31).
narrative_ontology:measurement(qura_tr_t40, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 40, 0.31).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(qura_be_t5, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 5, 0.51).
narrative_ontology:measurement(qura_be_t10, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 10, 0.46).
narrative_ontology:measurement(qura_be_t15, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(qura_be_t25, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 25, 0.43).
narrative_ontology:measurement(qura_be_t35, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 35, 0.45).
narrative_ontology:measurement(qura_be_t40, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(qura_su_t5, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(qura_su_t10, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(qura_su_t15, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 15, 0.49).
narrative_ontology:measurement(qura_su_t25, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 25, 0.48).
narrative_ontology:measurement(qura_su_t35, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 35, 0.5).
narrative_ontology:measurement(qura_su_t40, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__reformist_ijtihad, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(quran_hadith_substrate__reformist_ijtihad, 0.18).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate__traditionalist_taqlid).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate__state_hybrid).

% DUAL FORMULATION NOTE:
% The quran_hadith_substrate kernel decomposes into three structurally distinct constraint stories: reformist_ijtihad (this story, epsilon ~0.42, emphasis on contextual reinterpretation), traditionalist_taqlid (epsilon ~0.65, emphasis on interpretive monopoly), and state_hybrid (epsilon ~0.50, emphasis on institutional sovereignty). All three share the same kernel (Quran/Hadith as foundational text) but instantiate radically different extraction structures and beneficiary/victim dyads. The network edges establish the constraint family: each reading influences the others through institutional competition and doctrinal pressure, but none forecloses the others within the bounds of Islamic commitment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_hadith_substrate__reformist_ijtihad, powerless, 0.18).
constraint_indexing:directionality_override(quran_hadith_substrate__reformist_ijtihad, organized, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
