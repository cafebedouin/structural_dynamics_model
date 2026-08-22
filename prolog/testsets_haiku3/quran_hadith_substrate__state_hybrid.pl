% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__state_hybrid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_hadith_substrate__state_hybrid, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: quran_hadith_substrate__state_hybrid
 *   human_readable: State-Selective Sharia Adoption (Hybrid Legal Framework)
 *   domain: religious/political/legal
 *
 * SUMMARY:
 *   A state grounds its legitimacy in Islamic governance while preserving
 *   secular legal frameworks in domains where policy flexibility and
 *   international integration are economically critical. The state
 *   selectively codifies classical Islamic rulings in family law and criminal
 *   codes (high public religious salience, low policy flexibility cost) while
 *   applying reformist or secular frameworks in commercial and administrative
 *   law (high policy flexibility value, lower religious expectations). This
 *   constraint is the state_hybrid reading of the contested
 *   quran_hadith_substrate kernel — instantiating ONE coherent position on
 *   how Islamic jurisprudence should relate to state law-making. The sibling
 *   readings (traditionalist_taqlid: comprehensive classical application;
 *   reformist_ijtihad: contemporary ethical re-interpretation) represent
 *   competing framings of the same kernel, not alternatives examined within
 *   this story. This story describes the state's reading and its structural
 *   consequences; the other readings are separate constraints with different
 *   ε values, different beneficiary/victim structures, and different
 *   classifications.
 *
 * KEY AGENTS:
 *   - state_executive_elites: agenda-setter, controls legal framework selection (institutional power, trapped)
 *   - traditionalist_scholars: organized resistance, advocate comprehensive fiqh application (organized power, constrained exit)
 *   - reformist_critics: identity-locked payers, critical readings delegitimized (moderate power, identity lock)
 *   - secular_commercial_interests: structural beneficiaries, preserve economic predictability (powerful, mobile)
 *   - women_and_minorities: powerless payers, subject to classical family/criminal law (powerless, trapped)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__state_hybrid, 0.38).
domain_priors:suppression_score(quran_hadith_substrate__state_hybrid, 0.52).
domain_priors:theater_ratio(quran_hadith_substrate__state_hybrid, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, extractiveness, 0.38).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__state_hybrid, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__state_hybrid, "State-Selective Sharia Adoption (Hybrid Legal Framework)").
narrative_ontology:topic_domain(quran_hadith_substrate__state_hybrid, "religious/political/legal").

domain_priors:requires_active_enforcement(quran_hadith_substrate__state_hybrid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__state_hybrid, 'e661fdfa-7079-4141-9b23-c1824c881df3').
narrative_ontology:cs_kernel_codification('e661fdfa-7079-4141-9b23-c1824c881df3', fixed_text).
narrative_ontology:cs_authority_grounding('e661fdfa-7079-4141-9b23-c1824c881df3', extraction).
narrative_ontology:cs_interpretation_layer_present('e661fdfa-7079-4141-9b23-c1824c881df3').
narrative_ontology:cs_reading_relation('e661fdfa-7079-4141-9b23-c1824c881df3', quran_hadith_substrate__traditionalist_taqlid, coexists_with).
narrative_ontology:cs_reading_relation('e661fdfa-7079-4141-9b23-c1824c881df3', quran_hadith_substrate__reformist_ijtihad, influences).
narrative_ontology:cs_axiom('e661fdfa-7079-4141-9b23-c1824c881df3', foundational, state_political_sovereignty_precedence).
narrative_ontology:cs_axiom_status(state_political_sovereignty_precedence, holdable).
narrative_ontology:cs_axiom_grounding('e661fdfa-7079-4141-9b23-c1824c881df3', state_political_sovereignty_precedence, instrumental).
narrative_ontology:cs_axiom('e661fdfa-7079-4141-9b23-c1824c881df3', secondary, selective_application_legitimacy_based_on_domain_value).
narrative_ontology:cs_axiom_status(selective_application_legitimacy_based_on_domain_value, holdable).
narrative_ontology:cs_axiom_grounding('e661fdfa-7079-4141-9b23-c1824c881df3', selective_application_legitimacy_based_on_domain_value, instrumental).
narrative_ontology:cs_reference_frame('e661fdfa-7079-4141-9b23-c1824c881df3', state_sovereign_legal_selectivity).
narrative_ontology:cs_drift_state('e661fdfa-7079-4141-9b23-c1824c881df3', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e661fdfa-7079-4141-9b23-c1824c881df3', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__state_hybrid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, state_executive_elites).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, secular_commercial_interests).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, traditionalist_scholars).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, reformist_critics).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, women_and_minorities_in_family_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Selects which Islamic rulings to codify and which to suspend based on regime legitimacy requirements and economic policy needs. Uses family law and criminal rulings (high public salience) to signal Islamic governance while maintaining secular frameworks in commercial and administrative law (high policy flexibility). Enforces this selective application through courts, executive decree, and control of judicial interpretation. Collects political legitimacy from both traditionalist constituencies (who see Islamic law applied in some domains) and international economic partners (who need predictable commercial law).
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, state_executive_elites, agenda_setter,
    institutional, biographical, trapped, national).

% Advocate for comprehensive, consistent application of classical fiqh across all legal domains. Their vision of Islamic jurisprudence as an integrated system is truncated when the state applies some rulings selectively while ignoring others. They can teach in private settings and publish, but state law-making authority excludes their comprehensive framework; exit options are limited to jurisdictions with more comprehensive Islamic law application or retreat to non-state institutions (limited leverage).
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, traditionalist_scholars, payer,
    organized, generational, constrained, national).

% Argue that classical rulings should be re-examined against contemporary ethics, human rights standards, and public interest (maslaha); they advocate contextual ijtihad and evolutionary interpretation. When the state selectively applies classical rulings (e.g., harsh criminal penalties, restrictive family law codes) while claiming Islamic legitimacy, their critical readings are treated as threatening to state stability and religious authority. They are caught between insider status (as Muslims) and outsider exclusion (their readings delegitimize the state's selective framework). Their identity as Muslim reformers makes geographic exit costly.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, reformist_critics, payer,
    moderate, biographical, identity_locked, national).

% Benefit from predictable, secular commercial law (contracts, corporate governance, property, bankruptcy) that enables international trade and investment. The state's hybrid framework preserves their operating environment while signaling Islamic governance in other domains. They have exit options (relocating to other jurisdictions with stable commercial law) but the arrangement's flexibility keeps them invested.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, secular_commercial_interests, beneficiary,
    powerful, biographical, mobile, global).

% Subject to classical family law rulings (inheritance asymmetries, marriage/divorce imbalances, custody rules) when applied by state courts. They have no voice in whether the state adopts these rulings and often lack resources to exit the jurisdiction or challenge the rules through legal remedies. They pay through substantive legal disabilities.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, women_and_minorities_in_family_law, payer,
    powerless, biographical, trapped, national).

% Monitors the coherence and human-rights compliance of the state's hybrid framework. They document selective application, highlight contradictions between domains, and can pressure via sanctions or conditionality. They are not parties to the constraint but observe its operation and produce external commentary.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, international_legal_community, observer,
    institutional, generational, analytical, global).

% Religious endowments, schools, and seminaries that teach comprehensive Islamic jurisprudence. They are formally outside state law-making but their authority is diminished when the state selectively applies classical rulings in some domains while ignoring others, fragmenting the coherence of Islamic legal knowledge they transmit.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, traditionalist_institutions, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_hadith_substrate__state_hybrid, state_executive_elites).
narrative_ontology:fixing_cost_class(quran_hadith_substrate__state_hybrid, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legitimacy bridge: the state signals Islamic governance in domains of high public religious salience (family, criminal law) while maintaining secular-rationalist frameworks where state policy flexibility and international economic integration are paramount (commerce, administration). Coordinates disparate constituencies (traditionalist voters, international investors, state security apparatus) under a single institutional umbrella by offering each a different legal regime.
% TRANSFER_FUNCTION: Moves legitimacy authority from comprehensive Islamic jurisprudence (where traditionalists would control) to the state executive, which selects which rulings to apply and which to suspend. Transfers predictability and policy flexibility to secular commercial interests (benefiting international investors and state elites managing finance/trade). Transfers legal disabilities and constraint to traditionalists (whose comprehensive vision is rejected) and reformists (whose critical readings threaten regime stability). Transfers substantive rights losses to women and minorities when classical family and criminal rulings are applied.
% ABSENT_VOICES: Classical traditionalist jurisprudence as a comprehensive, unified system (excluded); reformist and human-rights oriented readings that would challenge the state's selective application (excluded and suppressed); and grassroots constituencies affected by criminal or family law rulings who have no formal voice in whether the state codifies those rulings.
% DISAPPEARANCE_RATIONALE: If the state's hybrid framework vanished, the political economy of legitimacy would reorganize. The state would face pressure to either adopt comprehensive classical Islamic law (satisfying traditionalists, likely alienating international investors and secular commercial interests) or fully secularize the legal code (satisfying reformists and international community, likely alienating traditionalist constituencies). The selective arrangement itself would collapse; the state would be forced to choose a coherent framework rather than maintaining the current selectivity.
% FOUNDING_PROBLEM: Post-colonial nation-states inherited a dual challenge: populations with strong Islamic religious identity and expectation of Islamic governance, AND the need for secular commercial frameworks to participate in international markets and manage modern economies. Classical fiqh was coherent within its own framework but not designed to govern commercial law, administrative procedure, or international contract enforcement. Reformism offered re-interpretation but lacked state authority and was seen as too flexible. The state's solution: adopt Islamic rulings where public religious legitimacy is highest-value (family, criminal law) while preserving secular law where efficiency and international integration matter most.
% FOUNDING_PROBLEM_CORROBORATION: The state elite and secular commercial elites attest this dual challenge persists and justify the hybrid framework as the necessary solution. International legal and human-rights organizations, independent scholars, and traditionalist institutions attest that the founding problem is substantially about regime legitimacy management rather than a genuine jurisprudential necessity — comparable jurisdictions achieve both Islamic governance and commercial coherence through other means. Reformist scholars testify that the selective application reveals political incentives, not doctrinal constraint.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__state_hybrid, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__state_hybrid, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__state_hybrid, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_hadith_substrate__state_hybrid, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__state_hybrid, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_hadith_substrate__state_hybrid_tests).
:- end_tests(quran_hadith_substrate__state_hybrid_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end) because the state collects legitimacy authority from classical jurisprudence but does not suppress all alternatives — traditionalists and reformists can teach, publish, and advocate outside state law-making; the constraint operates through selective law-making, not total prohibition. Suppression is moderate (0.52) because traditionalist comprehensive vision and reformist critical readings are excluded from state law-making and reformist positions face active regime delegitimation, but neither is fully silenced outside formal law. Theater ratio is high (0.58) because the constraint's persistence depends partly on performing Islamic governance (adopting classical rulings in high-visibility domains) while conducting actual policy through secular frameworks (commercial law, administration). The measurement series shows rising extractiveness and theater from t0 to t15 (accumulating regime reliance on the hybrid frame, intensifying performance), then slight decay from t15-t25 (projected: rising reformist pressure and international scrutiny begin to erode the credibility of the separation). Rising suppression_requirement tracks this dynamic — the state must suppress reformist and traditionalist critique more actively as the contradiction becomes visible.
 *
 * PERSPECTIVAL GAP:
 *   From the state executive seat, the hybrid framework is genuine coordination solving a real dual challenge (religious legitimacy + economic integration). From the traditionalist seat, it is extraction and truncation of their comprehensive vision. From the reformist seat, it is instrumentalization of classical rulings for political gain while delegitimizing their critical readings. From the secular commercial seat, it is beneficial coordination — they get predictable law and regime stability. From the women's/minorities' seat, it is pure extraction — they bear the cost of classical family/criminal rulings with no voice in the decision. The engine computes these divergent per-seat classifications from the structural data; the authored metrics describe the constraint's operation from the analytical (external observer) position.
 *
 * DIRECTIONALITY LOGIC:
 *   State elites are the agenda-setter and beneficiary (d near 0.0): they control the selection mechanism and collect legitimacy without bearing the cost of comprehensive Islamic law application. Secular commercial interests are beneficiaries (d near 0.0-0.2): they benefit from predictable commercial law and regime stability. Traditionalists are targets (d near 0.8-0.9): they bear the cost of their comprehensive vision being rejected and selectively applied; they have constrained exit and organized resistance but no agenda-setting power. Reformists are targets (d near 0.7-0.8): their critical readings are delegitimized and excluded from state law-making; identity lock (Islamic identity + scholar status) prevents full exit. Women and minorities in family law are full targets (d near 1.0): they bear the cost of classical rulings with no voice or exit options. The constraint's structural asymmetry (agenda-setter benefits, targets pay) is the signature of a Tangled Rope: genuine coordination function (the state solves a real dual challenge) AND asymmetric extraction (the solution privileges state and commercial interests while suppressing traditionalist and reformist readings and subordinating women/minorities).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids false classification as pure Rope (which would require symmetric benefit or low suppression) because: (1) the beneficiaries (state elites, commercial interests) collected the coordination function itself — they decided WHICH rulings to apply and which to suspend, not participants in a pre-existing coordination problem; (2) suppression is active and required to exclude traditionalist and reformist framings from state law-making; (3) theater ratio is high, indicating the constraint's legitimacy depends partly on performance (appearing to apply Islamic law comprehensively while actually preserving secular law in economically critical domains). It avoids false classification as pure Snare because the constraint does solve a genuine coordination problem — it does bridge religious legitimacy and economic integration in a way that allows the state to function with both constituencies. The Tangled Rope classification holds: the coordination function is real (both parts are necessary), the extraction is real (the state and commercial interests capture the decision-making, traditionalists and reformists are excluded, women/minorities bear substantive costs), and active enforcement is required (courts, executive decree, delegitimation of competing readings).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_sovereignty_vs_doctrinal_fidelity,
    'Is the state''s selective application of classical rulings a legitimate exercise of political sovereignty in adapting Islamic law to modern state governance, or is it a cover story for instrumentalizing religion to extract political legitimacy?',
    'Comparative institutional analysis: do states that adopt comprehensive Islamic law across all domains achieve equivalent economic integration and political stability? Do states that fully secularize achieve equivalent religious legitimacy? How do state actors themselves justify the selectivity when questioned?',
    'If selectivity is a legitimate adaptation, the constraint''s ε and classification may lower — it becomes genuine coordination with accepted asymmetry. If instrumentalization, ε stays high and the constraint remains Tangled Rope or becomes Snare. The boundary is contestable precisely because it depends on how one weighs political sovereignty against doctrinal consistency.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_sovereignty_vs_doctrinal_fidelity, conceptual, 'Sovereignty-based selectivity vs. instrumental exploitation of religious authority').

omega_variable(
    traditionalist_coherence_vs_state_pragmatism,
    'Does classical Islamic jurisprudence require comprehensive, consistent application across all legal domains (coherence test), or is selective application compatible with fiqh''s own internal logic?',
    'Jurisprudential analysis from traditionalist scholars: do the classical schools themselves provide precedent for differential application across domains? Can a state apply classical family law while secular commercial law and still be within fiqh''s coherence bounds?',
    'If coherence is required, traditionalists are victims of constraint truncation (current reading holds). If selective application is fiqh-compatible, traditionalists lose the claim that the state is violating their framework — the extraction becomes purely about state supremacy over interpretive authority, not about jurisprudential inconsistency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(traditionalist_coherence_vs_state_pragmatism, conceptual, 'Whether Islamic jurisprudence mandates comprehensive or permits selective application').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of reformist and traditionalist alternative readings structural (legal barriers, exclusion from state law-making, delegitimation via official channels) or partly internalized (reformists and traditionalists accept the state''s framing as legitimate)?',
    'Post-exit analysis: if a reformist or traditionalist scholar gains influence in a competing state institution or external platform, does suppression persist? Do reformist and traditionalist readings gain traction when the state''s hybrid framework faces credibility crises (e.g., international criticism, economic disruption)?',
    'If suppression is entirely structural, the constraint''s suppression metric is accurate as authored. If partly internalized, the effective suppression is higher — targets carry the internalized frame with them even when external barriers are lowered; reclassification toward higher suppression or piton-type theater dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural barriers vs. internalized acceptance of state legal selectivity').

omega_variable(
    kernel_reading_relationship,
    'Is the state_hybrid reading a coherent alternative interpretation of the Quran/Hadith substrate, or is it a meta-reading about HOW to implement Islamic law in modern states rather than a reading of the sources themselves?',
    'Jurisprudential analysis: do classical Islamic sources provide warrant for state-level selectivity based on political sovereignty and economic necessity? Or is the state_hybrid reading a pragmatic political position that brackets the sources altogether?',
    'If meta-reading (bracketing the sources), the constraint''s kernel_context shifts — it may be better understood as the state''s political position on how to relate to Islamic sources, rather than as a reading of the sources per se. This affects how the constraint relates to traditionalist and reformist readings — they would be framings of the sources; the state_hybrid would be a governance position about which readings to operationalize.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relationship, conceptual, 'Whether state_hybrid is a source-reading or a meta-political position on source implementation').

omega_variable(
    beneficiary_identity_elite_capture,
    'Are state executive elites and secular commercial interests the true beneficiaries, or are these groups using the state''s selective framework as cover for elite capture of Islamic legitimacy while undermining genuine Islamic governance?',
    'Structural analysis: if state elites were sincerely committed to Islamic law, would they not apply it comprehensively? Does the state''s selective application follow a pattern of preserving domains where elites extract economic or political rent (commercial law, administrative law) while applying Islamic law in domains where popular religious expectation is highest?',
    'If elite capture is the driver, the constraint becomes less Tangled Rope (hybrid coordination) and more Snare (pure extraction of legitimacy cover). ε may rise, theater_ratio interpretation shifts from performance-of-coordination to performance-of-religiosity, beneficiary list may clarify to ''state_executive_elites'' only (secular_commercial_interests become indirect beneficiaries via the state''s policy preservation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_identity_elite_capture, empirical, 'Whether selectivity serves genuine dual-coordination or elite-capture camouflage').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__state_hybrid, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_hadith_substrate__state_hybrid, theater_ratio, 0, 0.45).
narrative_ontology:measurement_basis(qura_tr_t0, observed).
narrative_ontology:measurement(qura_tr_t5, quran_hadith_substrate__state_hybrid, theater_ratio, 5, 0.51).
narrative_ontology:measurement_basis(qura_tr_t5, observed).
narrative_ontology:measurement(qura_tr_t10, quran_hadith_substrate__state_hybrid, theater_ratio, 10, 0.56).
narrative_ontology:measurement_basis(qura_tr_t10, observed).
narrative_ontology:measurement(qura_tr_t15, quran_hadith_substrate__state_hybrid, theater_ratio, 15, 0.61).
narrative_ontology:measurement_basis(qura_tr_t15, observed).
narrative_ontology:measurement(qura_tr_t20, quran_hadith_substrate__state_hybrid, theater_ratio, 20, 0.59).
narrative_ontology:measurement_basis(qura_tr_t20, projected).
narrative_ontology:measurement(qura_tr_t25, quran_hadith_substrate__state_hybrid, theater_ratio, 25, 0.58).
narrative_ontology:measurement_basis(qura_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_hadith_substrate__state_hybrid, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(qura_be_t0, observed).
narrative_ontology:measurement(qura_be_t5, quran_hadith_substrate__state_hybrid, base_extractiveness, 5, 0.32).
narrative_ontology:measurement_basis(qura_be_t5, observed).
narrative_ontology:measurement(qura_be_t10, quran_hadith_substrate__state_hybrid, base_extractiveness, 10, 0.38).
narrative_ontology:measurement_basis(qura_be_t10, observed).
narrative_ontology:measurement(qura_be_t15, quran_hadith_substrate__state_hybrid, base_extractiveness, 15, 0.41).
narrative_ontology:measurement_basis(qura_be_t15, observed).
narrative_ontology:measurement(qura_be_t20, quran_hadith_substrate__state_hybrid, base_extractiveness, 20, 0.39).
narrative_ontology:measurement_basis(qura_be_t20, projected).
narrative_ontology:measurement(qura_be_t25, quran_hadith_substrate__state_hybrid, base_extractiveness, 25, 0.38).
narrative_ontology:measurement_basis(qura_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_hadith_substrate__state_hybrid, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(qura_su_t0, observed).
narrative_ontology:measurement(qura_su_t5, quran_hadith_substrate__state_hybrid, suppression_requirement, 5, 0.46).
narrative_ontology:measurement_basis(qura_su_t5, observed).
narrative_ontology:measurement(qura_su_t10, quran_hadith_substrate__state_hybrid, suppression_requirement, 10, 0.51).
narrative_ontology:measurement_basis(qura_su_t10, observed).
narrative_ontology:measurement(qura_su_t15, quran_hadith_substrate__state_hybrid, suppression_requirement, 15, 0.55).
narrative_ontology:measurement_basis(qura_su_t15, observed).
narrative_ontology:measurement(qura_su_t20, quran_hadith_substrate__state_hybrid, suppression_requirement, 20, 0.53).
narrative_ontology:measurement_basis(qura_su_t20, projected).
narrative_ontology:measurement(qura_su_t25, quran_hadith_substrate__state_hybrid, suppression_requirement, 25, 0.52).
narrative_ontology:measurement_basis(qura_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__state_hybrid, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_hadith_substrate__state_hybrid, 0.12).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, traditionalist_taqlid).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, reformist_ijtihad).

% DUAL FORMULATION NOTE:
% The quran_hadith_substrate kernel decomposes into three constraint stories, each instantiating a different reading with distinct ε values and classifications. state_hybrid (this constraint) represents the state's position: political sovereignty takes precedence; selective application is legitimate. traditionalist_taqlid represents the classical jurisprudential position: comprehensive application and doctrinal coherence are mandated. reformist_ijtihad represents the critical-revisionist position: contemporary ethics and contextual reinterpretation should guide implementation. Each reading produces a different victim set, different suppression mechanisms, and different type classifications. The three are linked via network.affects_constraints to show how shifts in one reading's authority (e.g., rising international human-rights pressure on the state_hybrid reading) affect the others' institutional conditions. ε-invariance is maintained: each story's ε is the extraction level of THAT reading's referent (the standing arrangement THAT reading describes), not an average or weighted combination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_hadith_substrate__state_hybrid, organized, 0.75).
constraint_indexing:directionality_override(quran_hadith_substrate__state_hybrid, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
