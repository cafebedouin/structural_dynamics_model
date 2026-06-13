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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: quran_hadith_substrate__traditionalist_taqlid
 *   human_readable: Traditionalist Taqlid Authority: Classical Madhhab Rulings as Binding Consensus
 *   domain: religious/legal/institutional
 *
 * SUMMARY:
 *   The traditionalist taqlid reading instantiates a specific claim about
 *   Islamic jurisprudence: that the classical fiqh schools (Hanafi, Maliki,
 *   Shafi'i, Hanbali) represent authoritative consensus (ijma) binding on
 *   contemporary Muslims, and that individual believers are obligated to
 *   follow (taqlid) established madhhab rulings without questioning them.
 *   This reading is ONE instantiation of the contested kernel
 *   'quran_hadith_substrate' — a kernel that also contains reformist ijtihad
 *   (which reads the same Quranic and hadithic sources as mandating
 *   contextual reinterpretation) and state-hybrid (which selectively adopts
 *   classical rulings in state codes while applying different frameworks
 *   elsewhere). The traditionalist reading claims high beneficiary
 *   concentration (ulama establishment, madhhab institutions), identifiable
 *   victims (progressive Muslims, women, minorities), and substantial
 *   institutional enforcement. This story DOES NOT evaluate whether the
 *   traditionalist or reformist reading is textually correct — it models the
 *   traditionalist reading as a constraint with measurable structural
 *   properties: ε around 0.68 (high extraction), suppression around 0.79
 *   (active enforcement of orthodoxy), theater ratio rising over time
 *   (increasing performative maintenance of the authority framework against
 *   internal contestation).
 *
 * KEY AGENTS:
 *   - ulama_establishment: Institutional beneficiary setting and enforcing the taqlid obligation; operates with arbitrage exit (can reframe 'authentic tradition' without formal framework change)
 *   - madhhab_institutions: Institutional beneficiary (seminaries, jurisprudential schools, Islamic councils) that administers and transmits the constraint; constrained exit (institutional stability requires the framework's persistence)
 *   - mosque_hierarchies: Organized beneficiary enforcing the constraint at local level; distributed authority that amplifies institutional suppression
 *   - lay_believers_within_madhhab: Powerless, identity-locked payers; majority in traditionalist-dominant contexts; receive guidance stability but bear intellectual constraint
 *   - progressive_muslims: Moderate-power, constrained-exit payers; suppress their ijtihad-based reinterpretations; diaspora contexts offer looser enforcement
 *   - women_seeking_legal_parity: Powerless, identity-locked payers; face institutionalized male-preferential rulings (wilayah, divorce asymmetry, testimony weight); challenging the constraint risks community expulsion
 *   - religious_minorities: Powerless, trapped-exit payers; classical dhimmi rulings institutionalized by the constraint; geographic exit or religious conversion are primary options
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
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__traditionalist_taqlid, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__traditionalist_taqlid, "Traditionalist Taqlid Authority: Classical Madhhab Rulings as Binding Consensus").
narrative_ontology:topic_domain(quran_hadith_substrate__traditionalist_taqlid, "religious/legal/institutional").

domain_priors:requires_active_enforcement(quran_hadith_substrate__traditionalist_taqlid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__traditionalist_taqlid, '8190a35a-7469-48f5-b073-fd87f60a0883').
narrative_ontology:cs_kernel_codification('8190a35a-7469-48f5-b073-fd87f60a0883', fixed_text).
narrative_ontology:cs_authority_grounding('8190a35a-7469-48f5-b073-fd87f60a0883', lineage).
narrative_ontology:cs_interpretation_layer_present('8190a35a-7469-48f5-b073-fd87f60a0883').
narrative_ontology:cs_reading_relation('8190a35a-7469-48f5-b073-fd87f60a0883', quran_hadith_substrate__reformist_ijtihad, coexists_with).
narrative_ontology:cs_reading_relation('8190a35a-7469-48f5-b073-fd87f60a0883', quran_hadith_substrate__state_hybrid, influences).
narrative_ontology:cs_axiom('8190a35a-7469-48f5-b073-fd87f60a0883', foundational, ijma_establishes_binding_consensus).
narrative_ontology:cs_axiom_status(ijma_establishes_binding_consensus, holdable).
narrative_ontology:cs_axiom_grounding('8190a35a-7469-48f5-b073-fd87f60a0883', ijma_establishes_binding_consensus, conventional).
narrative_ontology:cs_axiom('8190a35a-7469-48f5-b073-fd87f60a0883', foundational, taqlid_obligation_on_laity).
narrative_ontology:cs_axiom_status(taqlid_obligation_on_laity, holdable).
narrative_ontology:cs_axiom_grounding('8190a35a-7469-48f5-b073-fd87f60a0883', taqlid_obligation_on_laity, deontological).
narrative_ontology:cs_axiom('8190a35a-7469-48f5-b073-fd87f60a0883', secondary, classical_schools_represent_exhaustive_interpretation).
narrative_ontology:cs_axiom_status(classical_schools_represent_exhaustive_interpretation, overridden).
narrative_ontology:cs_axiom_grounding('8190a35a-7469-48f5-b073-fd87f60a0883', classical_schools_represent_exhaustive_interpretation, empirically_contingent).
narrative_ontology:cs_reference_frame('8190a35a-7469-48f5-b073-fd87f60a0883', ijma_binds_classical_schools).
narrative_ontology:cs_drift_state('8190a35a-7469-48f5-b073-fd87f60a0883', contemporary_digital_pluralism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8190a35a-7469-48f5-b073-fd87f60a0883', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, ulama_establishment).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, mosque_hierarchies).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, progressive_muslims).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, women_seeking_legal_parity).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, religious_minorities).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, lay_believers_challenging_interpretations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, lay_believers_within_madhhab).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, lay_believers_within_madhhab).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__traditionalist_taqlid, ijma_as_binding_consensus).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__traditionalist_taqlid, madhhab_authority_doctrine).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__traditionalist_taqlid, taqlid_obligation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Custodians and interpreters of the classical fiqh schools. They claim interpretive authority grounded in chains of transmission (isnad) from classical scholars. They set and enforce what counts as authoritative taqlid, which madhhab rulings are binding, and what alternative readings constitute deviation. They benefit directly from the constraint's operation through institutional authority, endowment control, and social prestige. Their arbitrage option consists of selective reframing of what constitutes 'authentic tradition' without formally abandoning the taqlid framework.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, ulama_establishment, agenda_setter,
    institutional, generational, arbitrage, continental).

% The formal institutions (jurisprudential schools, seminary networks, Islamic councils) that claim to embody and transmit the classical rulings. Their legitimacy and resource allocation depend directly on the constraint's persistence. They administer educational curricula that reinforce taqlid obligation and control which interpretations are taught. They benefit from institutional stability and the flow of students, donations, and state recognition that follow from occupying the authorized interpretive seat.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions, beneficiary,
    institutional, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions, agenda_setter).

% Local and regional mosque leadership structures that administer the constraint at the lay believer level. They determine which madhhab rulings are preached, which alternative interpretations are suppressed, and who has standing to dispute authoritative guidance. They benefit from their gatekeeping position and the deference it confers. Their options for changing the framework are limited by dependence on the ulama establishment for legitimacy.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, mosque_hierarchies, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__traditionalist_taqlid, mosque_hierarchies, agenda_setter).

% The majority of Muslims in traditionalist-dominant contexts who follow taqlid because it is presented as obligation and as their inherited religious identity. They receive stable guidance and a clear path to righteousness; they also bear the cost of intellectual constraint and the prohibition on questioning established rulings. Exit is identity-fused: leaving taqlid means leaving the religious community, abandoning one's cultural inheritance, and risking family and social rupture. They experience the constraint as both protecting them (reliable guidance) and confining them (no voice in rule-making).
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, lay_believers_within_madhhab, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__traditionalist_taqlid, lay_believers_within_madhhab, beneficiary).

% Muslims seeking ijtihad-based reinterpretation of classical rulings in light of contemporary ethics, human rights norms, and changed social conditions. They argue for methodological pluralism and contextual jurisprudence. The constraint suppresses their interpretations through institutional gatekeeping, social pressure within Muslim communities, and in some jurisdictions, state enforcement against 'unorthodox' readings. Their exit options are limited: they can leave Islam (rare), compartmentalize their practice (bearing cognitive dissonance), attempt internal reform (facing organized resistance), or migrate to diaspora contexts with looser enforcement.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, progressive_muslims, payer,
    moderate, biographical, constrained, global).

% Women in traditionalist-dominant contexts who experience classical fiqh rulings on marriage, divorce, testimony, and inheritance as unjust. Classical madhhab rulings institutionalize male guardianship (wilayah), restrict women's right to initiate divorce unilaterally, and weight female testimony at half value in some domains. The constraint locks them into these frameworks by presenting them as immutable consensus; appeals for reinterpretation are suppressed by the institutional hierarchy. Exit is identity-locked: challenging taqlid authority in the name of gender equality risks exclusion from religious community, family breakdown, and economic vulnerability.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, women_seeking_legal_parity, payer,
    powerless, biographical, identity_locked, national).

% Non-Muslims (Christians, Jews, Yazidis, others) in jurisdictions where classical fiqh frameworks (particularly the dhimmi system) are authoritatively applied. The constraint institutionalizes their subordinate legal and social status through rules about testimony, property, taxation (jizya), and religious practice. They have no voice in the interpretation process and face active suppression of alternative frameworks. Exit is geographic (emigration) or involves abandoning their religious identity.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, religious_minorities, payer,
    powerless, biographical, trapped, national).

% Scholars and jurists who advocate ijtihad-based reform but are structurally excluded from the authoritative decision-making apparatus. They publish, teach in independent institutions, and build alternative communities, but their interpretations lack the institutional weight and enforcement machinery the ulama establishment commands. They are often marginalized or actively opposed by the constraint's enforcers.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, reformist_scholars, excluded,
    moderate, biographical, constrained, global).

% Government authorities that in some jurisdictions enforce classical fiqh rulings through criminal and family law codes, and in other jurisdictions resist or selectively adopt them. They observe and sometimes actively reinforce the constraint's enforcement architecture. In hybrid states, they enforce the constraint in domains where it aligns with state interest (social control) while overriding it in domains that conflict (commercial law, international trade).
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, state_actors, observer,
    institutional, generational, analytical, national).

% Islamic legal scholars, religious studies researchers, and comparative law analysts from both inside and outside Muslim communities who document the constraint's operation, history, and effects. They produce evidence of how taqlid suppresses reformist interpretation, how classical gender-differential rulings were applied and contested, and how the constraint's institutional support has evolved. Their analytical work sometimes influences diaspora Muslim communities and policy responses but does not directly participate in the binding interpretation process.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, academic_observers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_hadith_substrate__traditionalist_taqlid, ulama_establishment).
narrative_ontology:fixing_cost_class(quran_hadith_substrate__traditionalist_taqlid, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides unified, stable Islamic legal guidance across diverse contexts by anchoring contemporary Muslim practice to classically-derived consensus (ijma) and the four established schools. Solves the coordination problem of ensuring doctrinal consistency, preventing schism, and allowing Muslims to know and follow authoritative law. Centralizes jurisprudential authority to avoid the problem of unlimited interpretive pluralism.
% TRANSFER_FUNCTION: Moves interpretive authority from individual Muslims and democratized reading communities to the institutional ulama establishment. Transfers intellectual labor (questioning, contextual reasoning) from lay believers to credentialed scholars. Transfers decision-making power from women, minorities, and religious minorities to male ulama and those with institutional standing. Transfers legitimacy from methodologically open sources (Quran, hadith, reasoning) to the closed corpus of classical madhhab opinions.
% ABSENT_VOICES: Progressive Muslims advocating contextual ijtihad, women challenging gender-differential rulings, religious minorities seeking equal legal status under Islamic law, and scholars from outside the classical madhhab lineages. These groups would dispute that the classical schools represent genuine consensus (ijma) and would argue that the constraint suppresses rather than facilitates legitimate reasoning. They are not excluded from religious space entirely, but they are excluded from the formal apparatus that determines what counts as authoritative Islamic law.
% DISAPPEARANCE_RATIONALE: If the constraint vanished — if taqlid obligation were no longer enforced and Muslims were free to adopt ijtihad-based reinterpretation without institutional sanction — the institutional structure of Islamic jurisprudence would reorganize. The madhhab schools would lose their gatekeeping authority. Women and minorities would gain voice in interpretive processes. Legal rules on marriage, divorce, testimony, and inheritance would face pressure to align with contemporary ethics. The ulama establishment would either adapt by reframing their authority or face institutional decline. The constraint's disappearance would not eliminate Islam, but it would fundamentally alter the distribution of interpretive power.
% FOUNDING_PROBLEM: In early Islamic history (7th-10th centuries), the Quran and hadith corpus required authoritative interpretation. Multiple scholars reached different conclusions on the same questions. The constraint (ijma-based taqlid) was developed to establish stable consensus and prevent endless interpretive fragmentation that would prevent Muslims from living as a unified community under Islamic law.
% FOUNDING_PROBLEM_CORROBORATION: The ulama establishment attests the founding problem is live: contemporary Muslims still need stable guidance and interpretive consistency. Progressive scholars and human rights organizations attest the founding problem is substantially solved: modern communication, literacy, and education make pluralistic interpretation stable; the constraint now persists primarily to preserve institutional authority rather than prevent chaos. Islamic legal history scholarship (from scholars outside the benefiting parties) documents that the classical schools themselves were not monolithic, that ijtihad continued informally, and that the 'closing of the gate of ijtihad' was a post-hoc narrative rather than a historical event. This external corroboration supports the 'persistence after function' reading.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__traditionalist_taqlid, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__traditionalist_taqlid, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__traditionalist_taqlid, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(quran_hadith_substrate__traditionalist_taqlid, 'none', 1).

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
 *   The metrics track a constraint in institutional maturity but facing internal contestation. Base extractiveness starts at 0.58 (substantial but not maximal) and rises to 0.68 over the 40-unit interval. This rise reflects two dynamics: (1) the constraint's application has hardened in contexts of state enforcement (Saudi Arabia, Iran, Taliban-controlled Afghanistan) where classical fiqh rulings are law, not just guidance; (2) the cost of exit has risen as digital communication and global reformist networks make alternative readings more visible and tempting, requiring more aggressive suppression. Suppression is consistently high (0.72→0.79) because the constraint's persistence depends on actively blocking reformist interpretations, silencing women's critiques of gender-differential rulings, and excluding minority voices from interpretive authority. Theater ratio is moderate-low (0.28→0.42) because a real coordination function exists (guidance stability, doctrinal unity) but it is increasingly supplemented by performative authority maintenance: elaborate justifications for why classical rulings are 'scientifically' correct, why women actually benefit from male guardianship, why interpretive plurality would cause chaos. The rising theater ratio signals that functional justification is weakening relative to institutional inertia. Accessibility collapse (0.71) reflects the fact that exiting taqlid obligation is nearly impossible for those with identity fusion to Islam and community; alternatives collapse once the constraint is understood as 'what Islam requires' rather than 'one tradition's interpretation.' Resistance (0.58) is moderate: progressive Muslims mount scholarly counterarguments, women's movements challenge gender rulings, some Muslims practice reformist ijtihad quietly, but organized institutional resistance from within the ulama is limited because career advancement depends on taqlid orthodoxy.
 *
 * PERSPECTIVAL GAP:
 *   The ulama establishment and traditionalist believers perceive the constraint as protecting Islam's doctrinal integrity and preventing harmful fragmentation. Progressive Muslims and women perceive it as institutional suppression of legitimate reasoning and equality claims. Lay believers within the madhhab perceive both: genuine comfort in stable guidance, genuine constraint from lack of voice. The engine computes different per-seat types from this structural asymmetry: the agenda-setter (ulama) seat may compute as rope (they coordinated something stable and beneficial) while the payer seats (progressive Muslims, women) compute as snare or tangled_rope (they are extracted from and suppressed). This divergence is not an error — it is the measurement the system is designed to reveal. The claimed type (tangled_rope) reflects that BOTH functions are present: genuine coordination (stable jurisprudence) AND asymmetric extraction (authority monopoly, women's suppression, minority exclusion). The single ε bin does not capture per-seat experience; the per-seat computation does.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries (ulama, madhhab institutions, mosque hierarchies) have high directionality toward extraction because they control the rules, set the interpretations, and collect the social authority and institutional resources that flow from the constraint. Their d values cluster around 0.15–0.35 (moderate to powerful institutional actors benefiting). The victims (progressive Muslims, women, minorities) have high d values toward 0.75–0.95 (powerless to moderate actors bearing suppression and extraction). Lay believers within the madhhab are dual-positioned: they benefit from guidance stability (d ~0.4) but also bear intellectual constraint (d ~0.6), producing a near-symmetric seat. This directionality structure is derived from the beneficiary/victim declarations and the power/exit axes: beneficiaries with institutional power and arbitrage exit options have low χ (extraction is damped); victims with powerless/moderate power and identity-locked/trapped exit have high χ (extraction is amplified). The comment 'beneficiaries include traditional religious authority structures' is NOT overriding the derivation; it is feeding the derivation — the engine reads these declarations and computes d accordingly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (interpretive fragmentation in early Islam) is DEAD in the modern context: literacy is near-universal in Muslim-majority countries, communication is global, and Muslims manage religious pluralism in diaspora without institutional taqlid enforcement. Yet the constraint persists because institutional interests (madhhab seminary endowments, ulama prestige, state's use of classical law for social control) keep it alive. This is a classic mandatrophy signature: function → institution → inertia. The theater ratio rise (0.28→0.42) is the diagnostic marker: as functional justification weakens ('we need taqlid to prevent chaos' rings hollow when millions of Muslims peacefully practice reform-minded Islam), the constraint's defenders increasingly rely on theatrical moves: elaborate theological arguments for why reform is forbidden, performative celebrations of 'authentic tradition,' state-enforced suppression presented as 'protecting Islam.' The constraint is NOT a pure piton (performance-driven inertia); it retains genuine coordination function and the beneficiaries are concentrated enough to defend it. But mandatrophy is clearly present: the fit between founding problem and current operation has degraded. This is a tangled_rope where the rope function (coordination) persists but the extraction function (authority monopoly) now exceeds what coordination requires.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ijma_as_historical_fact_vs_normative_claim,
    'Is ijma (authoritative consensus) a genuine historical fact about what the classical scholars agreed on, or a retrospective normative claim that the later tradition projected back onto the classical period?',
    'Historical textual analysis of classical jurisprudential sources: do early fiqh texts show consensus, or do they show ongoing dispute that later tradition selectively narrated as agreement? Comparison of claims about ijma made by different classical scholars themselves.',
    'If ijma is a historical fact, the taqlid obligation rests on genuine consensus and represents authentic Islamic tradition. If ijma is a normative projection, taqlid is a post-hoc institutional claim to authority, and the foundation for suppressing ijtihad collapses. The structurally observed suppression (0.79) would then derive from institutional interest rather than doctrinal necessity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ijma_as_historical_fact_vs_normative_claim, empirical, 'Whether claimed consensus is a discovered fact or a constructed narrative').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.79) primarily structural (institutional gatekeeping, state enforcement, mosque hierarchies blocking alternative interpretations) or internalized (believers have internalized the taqlid obligation as part of their religious identity and self-suppress alternatives)?',
    'Post-exit trajectory analysis: when Muslims emigrate to diaspora contexts where institutional enforcement is loose, do they continue to experience suppression as internal (identity-fused obligation), or do they rapidly adopt reformist positions? Survey data on attitudes toward ijtihad across traditionalist-dominant and diaspora contexts.',
    'If suppression is primarily structural, weakening institutional enforcement (secularizing states, weakening madhhab institutions) would reduce the constraint''s operation. If it is primarily internalized, the constraint persists even after exit because believers carry the suppression with them as part of their identity. A mixed suppression mechanism (structural + internalized) would require both institutional reform AND identity work to resolve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of ijtihad is enforced by external institutional barriers or internalized through religious identity').

omega_variable(
    women_consent_to_gender_differential_rulings,
    'Do women in traditionalist contexts experience classical gender-differential rulings (male guardianship, unilateral talaq rights, testimony weight, inheritance shares) as part of a coordinated system they consent to, or as imposed extraction they tolerate due to exit traps?',
    'Qualitative research on women''s own accounts of their relationship to madhhab family law; analysis of women''s reform movements within Islamic contexts; comparison of attitudes where women have voice in interpretation (some diaspora, some reform-minded communities) versus where they don''t.',
    'If women consent to the framework, the constraint is a benign coordination mechanism. If women experience it as extraction they cannot exit, it is a core victim relationship, and the tangled_rope type reflects actual structural asymmetry rather than perceptual difference. The ulama''s framing of women''s consent (''Islam honors women''s roles'') versus women''s own accounts of constraint are in tension; external evidence resolves which better describes the structural reality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(women_consent_to_gender_differential_rulings, empirical, 'Whether women''s compliance with gender-differential rulings reflects genuine consent or identity-locked exit trap').

omega_variable(
    kernel_reading_foreclosure_test,
    'Do the traditionalist reading''s core claims logically foreclose the reformist reading''s core claims within a single coherent interpretive framework, or can both readings coexist as live positions held by different Muslim communities?',
    'Logical analysis of the core claims: traditionalist asserts ijma binds; reformist asserts ijtihad mandated when rulings conflict with ethics. Can a single Islamic framework logically hold both ''ijma is binding'' AND ''ijtihad is mandated when rulings conflict''? (Answer: yes, if ijma is bounded in scope or if ''ethics'' is read as part of the ijma itself.) If coexistence is logically possible, the readings coexist_with each other; if not, one forecloses the other.',
    'This determines the cs_structure.reading_relations value. If the readings genuinely foreclose each other, institutional competition is zero-sum; reform of one requires destruction of the other. If they coexist, peaceful pluralism is structurally possible, and the constraint''s suppression reflects institutional choice rather than logical necessity. Current reality (both readings live in different Muslim communities) suggests coexistence is achievable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_test, conceptual, 'Whether traditionalist taqlid logically forecloses reformist ijtihad or both can coexist as live readings').

omega_variable(
    committer_frame_sibling_reading_contestation,
    'Are the sibling readings (reformist_ijtihad, state_hybrid) each separately ε-invariant constraints, or are they variant measurements of a single constraint?',
    'Apply the ε-invariance test: if changing the observable used to evaluate the constraint changes ε, the observer is looking at different constraints. Reformist ijtihad measures whether Quranic ethics trajectory overrides literalist hadith (high ε when applied, suppressed by taqlid enforcement). State-hybrid measures whether state-selected classical rulings in family law coexist with reformist frameworks in commerce (moderate ε, bifurcated enforcement). Traditionalist taqlid measures whether binding consensus binds contemporary Muslims (high ε when enforced). Are these the same constraint measured differently, or three separate constraints with different ε values?',
    'If they are separate constraints, each story models a distinct structural reality with its own beneficiary/victim/enforcement profile. If they are one constraint with measurement variance, the three stories should merge into one. The prompt presents them as sibling readings of one kernel, suggesting one constraint-family with three decomposed instantiations. This omega documents that decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_sibling_reading_contestation, conceptual, 'Committer-frame confirmation: are the three readings separate ε-invariant constraints or measurements of one constraint?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__traditionalist_taqlid, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(qura_tr_t0, observed).
narrative_ontology:measurement(qura_tr_t8, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 8, 0.31).
narrative_ontology:measurement_basis(qura_tr_t8, observed).
narrative_ontology:measurement(qura_tr_t16, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 16, 0.35).
narrative_ontology:measurement_basis(qura_tr_t16, observed).
narrative_ontology:measurement(qura_tr_t24, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 24, 0.38).
narrative_ontology:measurement_basis(qura_tr_t24, observed).
narrative_ontology:measurement(qura_tr_t32, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 32, 0.4).
narrative_ontology:measurement_basis(qura_tr_t32, observed).
narrative_ontology:measurement(qura_tr_t40, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(qura_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(qura_be_t0, observed).
narrative_ontology:measurement(qura_be_t8, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 8, 0.61).
narrative_ontology:measurement_basis(qura_be_t8, observed).
narrative_ontology:measurement(qura_be_t16, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 16, 0.64).
narrative_ontology:measurement_basis(qura_be_t16, observed).
narrative_ontology:measurement(qura_be_t24, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 24, 0.66).
narrative_ontology:measurement_basis(qura_be_t24, observed).
narrative_ontology:measurement(qura_be_t32, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 32, 0.67).
narrative_ontology:measurement_basis(qura_be_t32, observed).
narrative_ontology:measurement(qura_be_t40, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(qura_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 0, 0.72).
narrative_ontology:measurement_basis(qura_su_t0, observed).
narrative_ontology:measurement(qura_su_t8, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 8, 0.74).
narrative_ontology:measurement_basis(qura_su_t8, observed).
narrative_ontology:measurement(qura_su_t16, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 16, 0.76).
narrative_ontology:measurement_basis(qura_su_t16, observed).
narrative_ontology:measurement(qura_su_t24, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 24, 0.77).
narrative_ontology:measurement_basis(qura_su_t24, observed).
narrative_ontology:measurement(qura_su_t32, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 32, 0.78).
narrative_ontology:measurement_basis(qura_su_t32, observed).
narrative_ontology:measurement(qura_su_t40, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 40, 0.79).
narrative_ontology:measurement_basis(qura_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__traditionalist_taqlid, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_hadith_substrate__traditionalist_taqlid, 0.12).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate__reformist_ijtihad).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate__state_hybrid).

% DUAL FORMULATION NOTE:
% The quran_hadith_substrate kernel is instantiated in three separate constraints, each representing a different reading of the same Quranic-hadithic sources. (1) traditionalist_taqlid: ijma binds; taqlid obligation; high extraction and suppression. (2) reformist_ijtihad: ijtihad mandated when classical rulings conflict with contemporary ethics; high extraction (from institutional resistance), moderate suppression (looser enforcement in diaspora). (3) state_hybrid: state selectively adopts classical rulings in family/criminal law while applying reformist/secular frameworks in commerce; moderate extraction (bifurcated enforcement). Each reading has a distinct ε value because the observable — what counts as legitimate reasoning about Islamic law — differs across readings. The traditionalist reading measures suppression of ijtihad as coercive control; the reformist reading measures institutional barriers to alternative interpretation; the state-hybrid reading measures bifurcated law application. All three affect one another through institutional competition, state enforcement choices, and individual Muslim choices about which reading to follow. The traditionalist reading currently exercises most enforcement machinery, particularly in state contexts, giving it structural dominance; the reformist reading exercises influence in diaspora and academic contexts; the state-hybrid reading exercises bifurcated enforcement in hybrid-regime states.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_hadith_substrate__traditionalist_taqlid, powerless, 0.88).
constraint_indexing:directionality_override(quran_hadith_substrate__traditionalist_taqlid, moderate, 0.72).
constraint_indexing:directionality_override(quran_hadith_substrate__traditionalist_taqlid, organized, 0.25).
constraint_indexing:directionality_override(quran_hadith_substrate__traditionalist_taqlid, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
