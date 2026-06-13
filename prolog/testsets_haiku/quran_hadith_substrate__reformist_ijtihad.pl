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
 *   constraint_id: quran_hadith_substrate__reformist_ijtihad
 *   human_readable: Reformist Ijtihad: Contextual Re-Interpretation Against Classical Rulings
 *   domain: religious/legal/ethical
 *
 * SUMMARY:
 *   Reformist Islamic jurisprudence mandates contextual reinterpretation of
 *   classical rulings when they conflict with contemporary ethics, human
 *   rights norms, or maslaha (public interest). The framework prioritizes the
 *   Quran's ethical trajectory (movement toward justice, human dignity,
 *   compassion) over literalist application of hadith and classical fiqh.
 *   Beneficiaries include progressive scholars, women, LGBTQ+ Muslims, and
 *   religious minorities whose rights are constrained by classical rulings;
 *   victims include traditionalist authority structures whose legitimacy
 *   depends on interpretive monopoly. The constraint is CLAIMED as Tangled
 *   Rope because it solves a genuine coordination problem (how to remain
 *   Islamic while adapting to contemporary ethics) AND asymmetrically
 *   redistributes interpretive authority from classical gatekeepers to
 *   distributed networks of progressive scholars. The claim/metric
 *   independence principle is applied: extractiveness is moderate (0.42)
 *   because the reform movement is still gaining institutional backing and
 *   faces strong traditionalist backlash; suppression is moderate-low (0.38)
 *   because reformist readings increasingly operate in academic and diaspora
 *   spaces where enforcement against alternative readings is limited.
 *
 * KEY AGENTS:
 *   - Progressive Muslim scholars (organized, global, mobile exit) — set the reformist agenda, establish institutes, train new generation in contextual ijtihad
 *   - Women in Muslim communities (moderate power, biographical horizon, constrained exit) — benefit from reformist arguments for marriage autonomy, inheritance equality, witness testimony
 *   - LGBTQ+ Muslims (powerless, immediate horizon, trapped exit) — benefit from interpretive space that allows Islamic belonging despite classical condemnation
 *   - Religious minorities in Muslim-majority states (powerless, biographical horizon, constrained exit) — benefit from Quranic-ethics arguments for equal citizenship
 *   - Traditionalist fiqh authorities (organized, generational, identity-locked) — pay the cost of lost interpretive monopoly; identity fused with classical madhhab framework
 *   - Classical madhhab gatekeepers (powerful, generational, identity-locked) — institutional prestige depends on immutability of established jurisprudence; erosion is existential threat
 *   - State authorities (institutional, generational, analytical position) — can enforce or suppress reformist readings; some deploy selectively across domains
 *   - Literalist and fundamentalist movements (excluded from the reformist framework itself; their objection is treated as backward rather than engaged)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__reformist_ijtihad, 0.42).
domain_priors:suppression_score(quran_hadith_substrate__reformist_ijtihad, 0.38).
domain_priors:theater_ratio(quran_hadith_substrate__reformist_ijtihad, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, extractiveness, 0.42).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__reformist_ijtihad, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__reformist_ijtihad, "Reformist Ijtihad: Contextual Re-Interpretation Against Classical Rulings").
narrative_ontology:topic_domain(quran_hadith_substrate__reformist_ijtihad, "religious/legal/ethical").

domain_priors:requires_active_enforcement(quran_hadith_substrate__reformist_ijtihad).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__reformist_ijtihad, '260ec6ea-7703-4cd8-b5a3-e02d618819f2').
narrative_ontology:cs_kernel_codification('260ec6ea-7703-4cd8-b5a3-e02d618819f2', fixed_text).
narrative_ontology:cs_authority_grounding('260ec6ea-7703-4cd8-b5a3-e02d618819f2', lineage).
narrative_ontology:cs_interpretation_layer_present('260ec6ea-7703-4cd8-b5a3-e02d618819f2').
narrative_ontology:cs_reading_relation('260ec6ea-7703-4cd8-b5a3-e02d618819f2', quran_hadith_substrate__traditionalist_taqlid, coexists_with).
narrative_ontology:cs_reading_relation('260ec6ea-7703-4cd8-b5a3-e02d618819f2', quran_hadith_substrate__state_hybrid, influences).
narrative_ontology:cs_axiom('260ec6ea-7703-4cd8-b5a3-e02d618819f2', foundational, quranic_ethical_principles_override_hadith_literalism).
narrative_ontology:cs_axiom_status(quranic_ethical_principles_override_hadith_literalism, holdable).
narrative_ontology:cs_axiom_grounding('260ec6ea-7703-4cd8-b5a3-e02d618819f2', quranic_ethical_principles_override_hadith_literalism, deontological).
narrative_ontology:cs_axiom('260ec6ea-7703-4cd8-b5a3-e02d618819f2', foundational, contextual_necessity_justifies_reinterpretation).
narrative_ontology:cs_axiom_status(contextual_necessity_justifies_reinterpretation, holdable).
narrative_ontology:cs_axiom_grounding('260ec6ea-7703-4cd8-b5a3-e02d618819f2', contextual_necessity_justifies_reinterpretation, empirically_contingent).
narrative_ontology:cs_axiom('260ec6ea-7703-4cd8-b5a3-e02d618819f2', secondary, maslaha_permits_legal_evolution).
narrative_ontology:cs_axiom_status(maslaha_permits_legal_evolution, holdable).
narrative_ontology:cs_axiom_grounding('260ec6ea-7703-4cd8-b5a3-e02d618819f2', maslaha_permits_legal_evolution, instrumental).
narrative_ontology:cs_reference_frame('260ec6ea-7703-4cd8-b5a3-e02d618819f2', quranic_primacy_with_contextual_application).
narrative_ontology:cs_drift_state('260ec6ea-7703-4cd8-b5a3-e02d618819f2', contemporary_human_rights_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('260ec6ea-7703-4cd8-b5a3-e02d618819f2', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, progressive_muslim_scholars).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, women_in_muslim_communities).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, lgbtq_plus_muslims).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, religious_minorities_in_islamic_polities).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, traditionalist_fiqh_authorities).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, classical_madhhab_gatekeepers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, diaspora_muslim_communities).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__reformist_ijtihad, quranic_ethical_trajectory_supremacy).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__reformist_ijtihad, contextual_necessity_doctrine).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__reformist_ijtihad, maslaha_prioritization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Frame and promote contextual ijtihad as the legitimate method for Islamic jurisprudence in the contemporary era. They argue that the Quran's ethical trajectory (movement toward equality, human dignity, protection of the vulnerable) should override literalist hadith rulings when they conflict with contemporary ethics and human rights norms. They publish scholarly work in peer-reviewed journals, establish institutes dedicated to reformist Islamic jurisprudence, teach in secular universities' religious studies departments, and train a new generation of jurists in reformist methodology. Their authority derives from Islamic textual scholarship credentials and engagement with both traditional Islamic sources and contemporary ethical frameworks. They operate in universities, think tanks, diaspora community centers, and international forums. They benefit substantially from the normalization of reformist ijtihad because it expands their interpretive authority and creates institutional platforms.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, progressive_muslim_scholars, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__reformist_ijtihad, progressive_muslim_scholars, beneficiary).

% Benefit when reformist ijtihad undermines classical rulings on marriage contract autonomy, unilateral divorce (talaq), inheritance inequity, and restricted witnessing. Contextual ijtihad enables arguments for equal spousal choice, equitable property rights, and full legal capacity. Their ability to leverage these arguments depends critically on whether reformist scholars occupy institutional positions with authority-conferring power and whether their societies acknowledge reformist interpretations as legitimate Islam. In communities where reformist scholars lack authority, women remain constrained by classical rulings codified in state family law.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, women_in_muslim_communities, beneficiary,
    moderate, biographical, constrained, global).

% Benefit from reformist reframing when scholars argue that classical rulings on same-sex relations reflect pre-modern social context and that the Quran's core ethical commitments (mercy, justice for all humans, human dignity) take precedence over literalist hadith application. Reformist ijtihad creates a possibility space for belonging within Islam without requiring wholesale rejection of Islamic identity; without it, they face binary choice between literalist condemnation or exit from Islamic community. Their situation is highly time-pressured (immediate horizon) because identity belonging is urgent and their exit options are severely trapped by kinship, cultural belonging, and spiritual identity.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, lgbtq_plus_muslims, beneficiary,
    powerless, immediate, trapped, global).

% Benefit when reformist scholars argue that classical dhimmi rules (inferior legal status, special taxation, restricted public roles, limited testimony) conflict with Quranic principles of human equality and are context-bound to medieval political arrangements where Islamic states faced security threats. Reformist ijtihad opens arguments for equal citizenship, equal legal testimony, and protection of minority religious practice grounded in Islamic ethics rather than requiring appeal to secular law alone. Their exit options are constrained by geography and legal status; they cannot easily leave Muslim-majority states, and their rights remain vulnerable to shifting political winds.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, religious_minorities_in_islamic_polities, beneficiary,
    powerless, biographical, constrained, national).

% Bear the cost of reformist ijtihad because their institutional authority rests on the claim that classical madhhab rulings represent binding scholarly consensus (ijma) and are not revisable by individual contemporary scholars who lack the deep learning of medieval jurists. When reformists argue for contextual reinterpretation, they directly undermine the gatekeeping function that sustains traditionalist institutional power and prestige. These authorities occupy formal positions (grand muftis, heads of state sharia councils, deans of Islamic law universities, leaders of established Islamic organizations). Their identity is fused with the immutability of classical jurisprudence; leaving that position means ceasing to be a traditionalist authority. The generational time horizon reflects that their career trajectories and institutional legitimacy are built over decades within classical frameworks.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, traditionalist_fiqh_authorities, payer,
    organized, generational, identity_locked, global).

% Experience erosion of their interpretive monopoly when reformist scholars claim the right to reopen classical jurisprudential questions and reinterpret foundational rulings. Their institutional prestige, career advancement, and scholarly legitimacy derive from controlling what counts as authentic Islamic jurisprudence. Reformist ijtihad decentralizes that authority, enabling any competent scholar (and increasingly activists and community members) to claim the right to reinterpret Islamic law. They work through formal state-backed institutions, religious endowments (awqaf), international fatwa bodies, and traditional Islamic universities. Their identity is inseparable from the framework they defend; exit from the traditionalist position means abandoning their scholarly identity and institutional role.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, classical_madhhab_gatekeepers, payer,
    powerful, generational, identity_locked, global).

% Occupy a position of sovereign control over which legal readings are codified into state law. Some states deploy reformist ijtihad selectively (family law under Quranic principles of equality, commercial law under economic necessity doctrines) while excluding it from criminal law and maintaining classical punishments. Their role as observer reflects their capacity to enforce or suppress reformist authority without themselves being primarily beneficiary or victim of the constraint—though they may gain political legitimacy from seeming 'moderate Islam' while suppressing reformist voices in other domains.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, state_authorities_in_muslim_majority_countries, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__reformist_ijtihad, state_authorities_in_muslim_majority_countries, agenda_setter).

% Benefit from reformist ijtihad in contexts where they navigate minority status within pluralistic legal frameworks and diverse religious communities. They can articulate Islamic values (justice, human dignity, family stability) while claiming compatibility with secular law, human rights norms, and their states' constitutional orders. Reformist methodology enables them to practice Islam authentically without wholesale rejection of the social contracts their societies impose. Their mobility (relative to trapped populations in Muslim-majority states) comes from geographic dispersion across multiple legal jurisdictions and less institutional entanglement with state religious authority.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, diaspora_muslim_communities, beneficiary,
    moderate, biographical, mobile, global).

% Are structurally excluded from the reformist ijtihad framework—their position that contextual reinterpretation dissolves binding Islamic law into subjective preference is treated as backward, literalist, or incoherent rather than engaged as a coherent alternative reading. They would argue that classical rulings are immutable and that the Quran's apparent ethical trajectory reflects ancient Arab cultural context, not timeless principles. They appear in backlash movements, counter-scholarship in Salafi networks, and religious authority within conservative communities, but they lack institutional platforms in universities, international human rights forums, and reformist-dominated academic spaces where the constraint's authority is concentrated.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, literalist_and_fundamentalist_movements, excluded,
    organized, generational, trapped, global).

% Religious studies scholars, anthropologists, comparative law researchers, and historians of Islamic thought observe the constraint from outside the tradition's internal authority structures. They analyze how the reformist reading operates, its social consequences, institutional spread, and relationship to traditionalist and state-hybrid readings. They have no stake in the outcome and can document the mechanisms, effects, and competing truth claims without adjudicating them. Their analytical distance enables meta-level observation of the constraint's operation across contexts.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, analytical_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_hadith_substrate__reformist_ijtihad, progressive_muslim_scholars).
narrative_ontology:fixing_cost_class(quran_hadith_substrate__reformist_ijtihad, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a shared framework for Islamic jurisprudence that allows Muslims in diverse temporal and geographical contexts to claim continuity with the tradition while adapting its rules to contemporary ethical and legal challenges. It solves the coordination problem of how to remain authentically Islamic while honoring evolving understandings of justice, human rights, and public welfare.
% TRANSFER_FUNCTION: Transfers interpretive authority from classical gatekeepers (traditionalist scholars, state-backed mufti bodies) to a distributed network of progressive scholars who invoke Quranic principles and contextual analysis. The "payment" from traditionalist structures is a loss of monopoly control over what counts as legitimate Islamic jurisprudence; the gain to progressive and marginalized groups is expanded agency in religious self-interpretation.
% ABSENT_VOICES: Literalist and fundamentalist movements are structurally excluded from the reformist framework itself—their objection that contextual reinterpretation dissolves binding law is treated as backward or incoherent rather than engaged as a coherent alternative reading. Lower-income traditionalist scholars and rural Islamic teachers (who lack platforms in international academic discourse) are also absent, though they hold substantial authority in local communities.
% DISAPPEARANCE_RATIONALE: If reformist ijtihad as a legitimized methodology disappeared, women's legal status in Muslim-majority jurisdictions would revert to classical rules in domains where reformist arguments currently hold (marriage autonomy, inheritance); LGBTQ+ Muslims would lose the interpretive space that currently allows some to claim Islamic belonging; and religious minorities would lose arguments grounded in Islamic ethics for equal citizenship. The institutional landscape would shift back toward gatekeeping by traditionalist authorities and state-backed mufti systems.
% FOUNDING_PROBLEM: Classical Islamic jurisprudence, developed in the 8th–10th centuries, generated rules that contemporary Muslims experience as incompatible with modern understandings of human rights, gender equality, scientific knowledge, and democratic governance. The founding problem is the tension between textual fidelity and ethical coherence: how can Muslims honor the Islamic tradition while living in societies with different ethical assumptions and legal orders?
% FOUNDING_PROBLEM_CORROBORATION: Progressive Islamic scholars and human rights organizations attest that the founding problem persists: classical rules on family law, criminal punishment, and minority status conflict with international human rights conventions that most Muslim-majority states have ratified. Traditionalist scholars attest the problem differently: the problem is not classical rulings but Muslim societies' abandonment of them for secular frameworks. Religious minorities and women's rights advocates in Muslim-majority countries attest that living under classical rules as codified state law creates genuine harms (unequal inheritance, forced marriage, restricted testimony). Academic observers from religious studies and law document the persistence of this tension across Muslim-majority and diaspora communities.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__reformist_ijtihad, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__reformist_ijtihad, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__reformist_ijtihad, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(quran_hadith_substrate__reformist_ijtihad, 'none', 1).

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
 *   Extractiveness measures how much the constraint redistributes interpretive authority from classical gatekeepers to progressive scholars. At t=0 (founding of reformist movement, ~1980s-1990s), extractiveness was low (0.25) because traditionalist structures dominated institutional spaces and reformist readings had marginal platforms. Over the interval, extractiveness rises to 0.42 as reformist scholars gain positions in international universities, establish think tanks, and influence diaspora religious authority. The plateau at t=24-40 reflects traditionalist counter-mobilization: states in Muslim-majority countries increasingly codify classical rules in family law (state_hybrid reading), and Salafi movements mobilize against reformist hermeneutics. Suppression requirement falls from 0.55 to 0.38 because the reformist framework operates in spaces (academic discourse, diaspora communities, international human rights forums) where enforcement against alternative readings is weaker than in state-controlled religious institutions. Theater ratio rises slightly (0.18→0.28) because reformist scholars increasingly emphasize compatibility with human rights (narrative performance for legitimacy) while actual implementation varies by context. Accessibility of alternatives: the coercion grid shows declining collapse at class level (0.38→0.35) because women and marginalized groups increasingly access reformist arguments without gatekeeping, but organizational and structural alternatives remain partially collapsed (gatekeepers maintain control in formal state institutions). Resistance increases over the interval (0.45→0.58 structurally, 0.52→0.68 organizationally) reflecting traditionalist backlash and Salafi counter-movements.
 *
 * PERSPECTIVAL GAP:
 *   From the progressive scholar seat, reformist ijtihad is genuine coordination solving a real problem: how to keep Islam alive and meaningful for contemporary Muslims without abandoning textual fidelity. From the traditionalist gatekeeper seat, the same constraint operates as authority erosion and loss of institutional control—what progressives call 'contextual necessity' gatekeepers experience as 'textual dissolution.' The engine should compute these differently: progressive scholars (d near beneficiary end: they gain interpretive legitimacy, no substantial cost) should see a Rope-like structure (coordination with benefit); traditionalist gatekeepers (d near target end: they lose monopoly control, high institutional cost) should see a Snare-like structure (extraction of authority). The scholarly field itself (organized, powerful) sits symmetrically: they both gain (access to interpretation) and lose (authority fragmentation) simultaneously, putting them near d=0.5. This divergence is structural and should be captured in per-seat computation from the stakeholder situation data.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive Muslim scholars are the primary beneficiary (role: agenda_setter + beneficiary, d near 0.1-0.2): they expand interpretive authority and gain institutional platforms. Traditionalist authorities are the primary victim (role: payer, d near 0.8-0.9): they lose gatekeeping control and institutional prestige. Women, LGBTQ+ Muslims, and religious minorities benefit (d toward 0.2-0.3) but their exit options remain constrained by broader social structures, moderating their directionality. The constraint exhibits high asymmetry: progressives lose little by reformist success (they already lack institutional power), while traditionalists lose their foundational claim to authoritative interpretation. This asymmetry justifies the Tangled Rope classification (coordination function + extraction from one side). Directionality override consideration: traditionalist scholars are coded identity_locked, which should moderate their exit options further (they cannot simply leave scholarship and remain traditionalist). Progressive scholars are coded mobile because they can operate across institutions (universities, think tanks, diaspora communities) without losing scholarly authority—their flexibility is a structural advantage in the reformist frame.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (tension between classical rules and contemporary ethics) remains live and is widely attested outside beneficiary circles (religious minorities, women's rights organizations, international human rights bodies). The disappearance verdict is world_rearranges: if reformist ijtihad lost legitimacy, women in Muslim-majority states would revert to classical marriage law, LGBTQ+ Muslims would lose interpretive space, and minorities would lose Quranic-ethics arguments for citizenship. This prevents mandatrophy classification. However, the constraint should be monitored for theater-ratio drift: if reformist scholars increasingly invoke Quranic ethics as cover for outcomes determined by other values (secular human rights norms, Western advocacy), the coordination function (solving the Islamic-modernity tension) could atrophy while performative invocation of Quranic principles persists. At theater_ratio=0.28, the constraint is still substantively functional (pedagogy and argument structure remain intact), but rising theater over the interval (0.18→0.28) suggests mild erosion. A sharp rise to >0.5 would warrant Piton reclassification (authority maintained theatrically rather than through genuine coordination).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reformist_vs_traditionalist_kernel_closure,
    'Does the reformist reading logically foreclose the traditionalist reading, or do they coexist as competing lived commitments across different institutional and social contexts?',
    'Ethnographic and historical analysis of Muslim-majority societies and diaspora communities: do traditionalist and reformist readings operate simultaneously in the same jurisdictions (coexists_with), or does reformist dominance structurally eliminate traditionalist authority (forecloses)?',
    'If forecloses: the constraint is fundamentally about authority replacement and represents a major institutional shift. If coexists_with: the constraint operates as a contested plurality where both readings claim legitimacy in different venues and populations. The classification would remain Tangled Rope in either case, but the framing of what is extracted (monopoly vs. pluralism) would shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformist_vs_traditionalist_kernel_closure, empirical, 'Whether reformist and traditionalist readings are logically exclusive or institutionally coexistent.').

omega_variable(
    identity_lock_mechanism_traditionalist_scholars,
    'For traditionalist scholars coded as identity_locked, is the identity lock structural (career path, institutional roles, economic dependence on classical madhhab authority) or internalized (worldview that treats classical rulings as metaphysically binding)?',
    'Post-exit narrative analysis: when traditionalist scholars convert to reformist readings, what friction remains? If the friction is purely institutional (loss of position, social ostracism), the lock is structural. If personal conviction remains that classical rulings are binding even after exiting, the lock is internalized.',
    'If purely structural, the escape velocity is lower and exit options improve when institutions shift. If internalized, the constraint carries higher effective suppression because even institutional exit does not dissolve the cognitive bind.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_traditionalist_scholars, empirical, 'Identity-lock mechanism for traditionalist gatekeepers.').

omega_variable(
    quranic_trajectory_operationalization,
    'What counts as the Quran''s ''ethical trajectory''? Different scholars extract different trajectories (trajectory toward monotheism, toward mercy, toward justice, toward equality, toward social cohesion). Does reformist ijtihad anchor to one trajectory or remain open-ended?',
    'Comparative analysis of reformist scholarly outputs: do they converge on one ethical principle or diverge? Can the framework distinguish a principled application of Quranic ethics from ad hoc invocation of the Quran to justify preferred outcomes?',
    'If operationalization is unclear, reformist ijtihad risks becoming instrumentalized (using the framework to justify any outcome). If one trajectory dominates, the framework has structure but may exclude legitimate ethical readings. This affects whether reformist ijtihad counts as genuine coordination or as extractive cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quranic_trajectory_operationalization, conceptual, 'Definitional ambiguity in ''Quranic ethical trajectory'' that could enable instrumentalization or exclude legitimate readings.').

omega_variable(
    maslaha_scope_and_capture,
    'Who determines what counts as maslaha (public interest)? If progressive scholars claim the right to interpret maslaha, could traditionalist scholars make identical claims to justify their preferred rulings?',
    'Analysis of which rulings are justified via maslaha by reformist vs. traditionalist scholars. If the same mechanism (contextual interpretation of public interest) is applied asymmetrically to different domains, the framework itself may be extractive.',
    'If maslaha scope is symmetric between readings, the framework is genuinely open-ended and permissive of interpretive plurality. If reformists monopolize maslaha claims while dismissing traditionalist maslaha arguments, the constraint becomes a form of extractive framing (using the language of contextual necessity to legitimize predetermined conclusions).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(maslaha_scope_and_capture, empirical, 'Risk of asymmetric deployment of maslaha reasoning that would make reformist ijtihad a cover story for preferred outcomes.').

omega_variable(
    kernel_reading_sibling_ambiguity,
    'This constraint is one reading of the quran_hadith_substrate kernel. Are the sibling readings (traditionalist_taqlid, state_hybrid) genuinely logical alternatives to this reading, or are they the same underlying constraint viewed from different institutional positions?',
    'Structural decomposition: do the readings differ in their ε (extractiveness) when measured against the same verification standard, or do they differ only in whose authority is privileged? If ε diverges substantially, they are two constraints. If ε is similar and only authority assignment differs, they may be observational variants of one constraint.',
    'If genuinely separate constraints: this reading (reformist ijtihad) is one distinct institutional story. If variants: the real constraint is ''contested substrate for Islamic jurisprudence'' and this reading is one faction''s claim on that contested space. The latter framing would reclassify this constraint as Tangled Rope at the meta-level (factions contesting the kernel itself).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_sibling_ambiguity, conceptual, 'Whether this reading is structurally independent or a variant of a meta-constraint about kernel contestation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__reformist_ijtihad, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(qura_tr_t0, projected).
narrative_ontology:measurement(qura_tr_t8, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 8, 0.21).
narrative_ontology:measurement_basis(qura_tr_t8, observed).
narrative_ontology:measurement(qura_tr_t16, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 16, 0.25).
narrative_ontology:measurement_basis(qura_tr_t16, observed).
narrative_ontology:measurement(qura_tr_t24, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 24, 0.29).
narrative_ontology:measurement_basis(qura_tr_t24, observed).
narrative_ontology:measurement(qura_tr_t32, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 32, 0.27).
narrative_ontology:measurement_basis(qura_tr_t32, observed).
narrative_ontology:measurement(qura_tr_t40, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(qura_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(qura_be_t0, projected).
narrative_ontology:measurement(qura_be_t8, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 8, 0.32).
narrative_ontology:measurement_basis(qura_be_t8, observed).
narrative_ontology:measurement(qura_be_t16, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 16, 0.38).
narrative_ontology:measurement_basis(qura_be_t16, observed).
narrative_ontology:measurement(qura_be_t24, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 24, 0.42).
narrative_ontology:measurement_basis(qura_be_t24, observed).
narrative_ontology:measurement(qura_be_t32, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 32, 0.4).
narrative_ontology:measurement_basis(qura_be_t32, observed).
narrative_ontology:measurement(qura_be_t40, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 40, 0.42).
narrative_ontology:measurement_basis(qura_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(qura_su_t0, projected).
narrative_ontology:measurement(qura_su_t8, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 8, 0.52).
narrative_ontology:measurement_basis(qura_su_t8, observed).
narrative_ontology:measurement(qura_su_t16, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 16, 0.45).
narrative_ontology:measurement_basis(qura_su_t16, observed).
narrative_ontology:measurement(qura_su_t24, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 24, 0.4).
narrative_ontology:measurement_basis(qura_su_t24, observed).
narrative_ontology:measurement(qura_su_t32, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 32, 0.38).
narrative_ontology:measurement_basis(qura_su_t32, observed).
narrative_ontology:measurement(qura_su_t40, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 40, 0.38).
narrative_ontology:measurement_basis(qura_su_t40, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(qura_grid_01, quran_hadith_substrate__reformist_ijtihad, accessibility_collapse(class), 0, 0.38).
narrative_ontology:measurement(qura_grid_02, quran_hadith_substrate__reformist_ijtihad, accessibility_collapse(class), 40, 0.35).
narrative_ontology:measurement(qura_grid_03, quran_hadith_substrate__reformist_ijtihad, accessibility_collapse(individual), 0, 0.42).
narrative_ontology:measurement(qura_grid_04, quran_hadith_substrate__reformist_ijtihad, accessibility_collapse(individual), 40, 0.38).
narrative_ontology:measurement(qura_grid_05, quran_hadith_substrate__reformist_ijtihad, accessibility_collapse(organizational), 0, 0.62).
narrative_ontology:measurement(qura_grid_06, quran_hadith_substrate__reformist_ijtihad, accessibility_collapse(organizational), 40, 0.52).
narrative_ontology:measurement(qura_grid_07, quran_hadith_substrate__reformist_ijtihad, accessibility_collapse(structural), 0, 0.55).
narrative_ontology:measurement(qura_grid_08, quran_hadith_substrate__reformist_ijtihad, accessibility_collapse(structural), 40, 0.48).
narrative_ontology:measurement(qura_grid_09, quran_hadith_substrate__reformist_ijtihad, resistance(class), 0, 0.62).
narrative_ontology:measurement(qura_grid_10, quran_hadith_substrate__reformist_ijtihad, resistance(class), 40, 0.72).
narrative_ontology:measurement(qura_grid_11, quran_hadith_substrate__reformist_ijtihad, resistance(individual), 0, 0.58).
narrative_ontology:measurement(qura_grid_12, quran_hadith_substrate__reformist_ijtihad, resistance(individual), 40, 0.65).
narrative_ontology:measurement(qura_grid_13, quran_hadith_substrate__reformist_ijtihad, resistance(organizational), 0, 0.52).
narrative_ontology:measurement(qura_grid_14, quran_hadith_substrate__reformist_ijtihad, resistance(organizational), 40, 0.68).
narrative_ontology:measurement(qura_grid_15, quran_hadith_substrate__reformist_ijtihad, resistance(structural), 0, 0.45).
narrative_ontology:measurement(qura_grid_16, quran_hadith_substrate__reformist_ijtihad, resistance(structural), 40, 0.58).
narrative_ontology:measurement(qura_grid_17, quran_hadith_substrate__reformist_ijtihad, stakes_inflation(class), 0, 0.35).
narrative_ontology:measurement(qura_grid_18, quran_hadith_substrate__reformist_ijtihad, stakes_inflation(class), 40, 0.38).
narrative_ontology:measurement(qura_grid_19, quran_hadith_substrate__reformist_ijtihad, stakes_inflation(individual), 0, 0.42).
narrative_ontology:measurement(qura_grid_20, quran_hadith_substrate__reformist_ijtihad, stakes_inflation(individual), 40, 0.45).
narrative_ontology:measurement(qura_grid_21, quran_hadith_substrate__reformist_ijtihad, stakes_inflation(organizational), 0, 0.72).
narrative_ontology:measurement(qura_grid_22, quran_hadith_substrate__reformist_ijtihad, stakes_inflation(organizational), 40, 0.68).
narrative_ontology:measurement(qura_grid_23, quran_hadith_substrate__reformist_ijtihad, stakes_inflation(structural), 0, 0.68).
narrative_ontology:measurement(qura_grid_24, quran_hadith_substrate__reformist_ijtihad, stakes_inflation(structural), 40, 0.65).
narrative_ontology:measurement(qura_grid_25, quran_hadith_substrate__reformist_ijtihad, suppression(class), 0, 0.28).
narrative_ontology:measurement(qura_grid_26, quran_hadith_substrate__reformist_ijtihad, suppression(class), 40, 0.25).
narrative_ontology:measurement(qura_grid_27, quran_hadith_substrate__reformist_ijtihad, suppression(individual), 0, 0.32).
narrative_ontology:measurement(qura_grid_28, quran_hadith_substrate__reformist_ijtihad, suppression(individual), 40, 0.28).
narrative_ontology:measurement(qura_grid_29, quran_hadith_substrate__reformist_ijtihad, suppression(organizational), 0, 0.65).
narrative_ontology:measurement(qura_grid_30, quran_hadith_substrate__reformist_ijtihad, suppression(organizational), 40, 0.58).
narrative_ontology:measurement(qura_grid_31, quran_hadith_substrate__reformist_ijtihad, suppression(structural), 0, 0.58).
narrative_ontology:measurement(qura_grid_32, quran_hadith_substrate__reformist_ijtihad, suppression(structural), 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__reformist_ijtihad, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_hadith_substrate__reformist_ijtihad, 0.12).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate__traditionalist_taqlid).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate__state_hybrid).

% DUAL FORMULATION NOTE:
% The quran_hadith_substrate kernel decomposes into three constraint stories reflecting three competing institutional readings of how Islamic jurisprudence should prioritize Quranic text, hadith, classical schools, and contextual necessity. Each reading has distinct ε, stakeholders, and beneficiary/victim structures. REFORMIST_IJTIHAD (this story) prioritizes Quranic ethics and contemporary maslaha, benefiting progressive scholars and marginalized groups while extracting from traditionalist gatekeepers. TRADITIONALIST_TAQLID prioritizes classical consensus and immutable jurisprudence, benefiting established authorities while constraining reformist reinterpretation. STATE_HYBRID selectively applies classical and reformist frames by domain, benefiting state sovereignty while creating incoherence in pluralistic legal systems. The three readings are linked via network.affects_constraints: reformist arguments directly challenge traditionalist claims, and state hybrid readings both depend on and obscure the traditionalist/reformist contest by claiming political override. Each story must be generated independently with its own ε, beneficiaries, and metrics—the kernel decomposition prevents collapsing the three into one story with an 'observer parameter.'

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_hadith_substrate__reformist_ijtihad, powerful, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
