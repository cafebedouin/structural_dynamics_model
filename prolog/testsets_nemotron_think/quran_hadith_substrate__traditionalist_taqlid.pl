% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__traditionalist_taqlid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: quran_hadith_substrate__traditionalist_taqlid
 *   human_readable: Classical Madhhab Taqlid Obligation
 *   domain: religious/legal
 *
 * SUMMARY:
 *   This constraint story captures the traditionalist_taqlid reading of the
 *   quran_hadith_substrate kernel: the claim that classical fiqh schools
 *   (madhhabs) crystallized the authoritative consensus (ijma) of the early
 *   Muslim community, and that contemporary Muslims are religiously obligated
 *   to follow established madhhab rulings through taqlid (deferential
 *   adherence) rather than independent ijtihad. The reading presents this as
 *   a coordination solution — providing stable, authoritative legal guidance
 *   that prevents fragmentation — but operates with high suppression of
 *   alternative interpretive paths and asymmetric extraction benefiting
 *   traditional ulama institutions. The constraint is actively enforced
 *   through both state mechanisms (classical family law codification,
 *   apostasy/blasphemy laws in traditionalist-dominant jurisdictions) and
 *   social mechanisms (communal authority, mosque hierarchies, educational
 *   pipelines). Victims include progressive Muslims denied interpretive
 *   agency, women seeking equal legal status in personal law, and religious
 *   minorities subjected to classical dhimmi frameworks. The measurement
 *   series (0-100, representing roughly 1920-2020) shows rising extraction
 *   and theater as the coordination function atrophies relative to
 *   institutional maintenance, while suppression intensifies in response to
 *   reformist challenges.
 *
 * KEY AGENTS:
 *   - traditional_ulama: Primary beneficiary (institutional/arbitrage) — controls interpretive authority, collects social capital and material resources from taqlid system
 *   - madhhab_institutions: Primary beneficiary (institutional/arbitrage) — curricular control, certification monopolies, endowment revenue
 *   - mosque_hierarchies: Beneficiary (organized/constrained) — local enforcement nodes, communal authority derives from madhhab affiliation
 *   - progressive_muslims: Primary victim (moderate/trapped) — denied interpretive agency, subjected to rulings they contest
 *   - women_seeking_equal_status: Primary victim (moderate/trapped) — bear gendered extraction in personal law (marriage, divorce, inheritance, testimony)
 *   - religious_minorities_dhimmi: Victim (powerless/trapped) — subjected to classical protection/subjection framework without consent
 *   - state_authorities_traditionalist: Agenda setter/beneficiary (institutional/arbitrage) — codifies madhhab rulings as positive law, gains Islamic legitimacy
 *   - reformist_scholars: Excluded (moderate/constrained) — structurally barred from authoritative interpretation
 *   - academic_observers: Observer (analytical/analytical) — studies the system from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__traditionalist_taqlid, 0.72).
domain_priors:suppression_score(quran_hadith_substrate__traditionalist_taqlid, 0.78).
domain_priors:theater_ratio(quran_hadith_substrate__traditionalist_taqlid, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, extractiveness, 0.72).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__traditionalist_taqlid, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__traditionalist_taqlid, "Classical Madhhab Taqlid Obligation").
narrative_ontology:topic_domain(quran_hadith_substrate__traditionalist_taqlid, "religious/legal").

domain_priors:requires_active_enforcement(quran_hadith_substrate__traditionalist_taqlid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__traditionalist_taqlid, 'bacabeb0-a634-4c58-8801-d9ecce5f0c0b').
narrative_ontology:cs_kernel_codification('bacabeb0-a634-4c58-8801-d9ecce5f0c0b', formalized).
narrative_ontology:cs_authority_grounding('bacabeb0-a634-4c58-8801-d9ecce5f0c0b', lineage).
narrative_ontology:cs_interpretation_layer_present('bacabeb0-a634-4c58-8801-d9ecce5f0c0b').
narrative_ontology:cs_reading_relation('bacabeb0-a634-4c58-8801-d9ecce5f0c0b', quran_hadith_substrate__reformist_ijtihad, forecloses).
narrative_ontology:cs_reading_relation('bacabeb0-a634-4c58-8801-d9ecce5f0c0b', quran_hadith_substrate__state_hybrid, influences).
narrative_ontology:cs_axiom('bacabeb0-a634-4c58-8801-d9ecce5f0c0b', foundational, classical_ijma_binding_on_all_generations).
narrative_ontology:cs_axiom_status(classical_ijma_binding_on_all_generations, holdable).
narrative_ontology:cs_axiom_grounding('bacabeb0-a634-4c58-8801-d9ecce5f0c0b', classical_ijma_binding_on_all_generations, deontological).
narrative_ontology:cs_axiom('bacabeb0-a634-4c58-8801-d9ecce5f0c0b', foundational, madhhab_taqlid_obligatory_for_laypersons).
narrative_ontology:cs_axiom_status(madhhab_taqlid_obligatory_for_laypersons, holdable).
narrative_ontology:cs_axiom_grounding('bacabeb0-a634-4c58-8801-d9ecce5f0c0b', madhhab_taqlid_obligatory_for_laypersons, conventional).
narrative_ontology:cs_axiom('bacabeb0-a634-4c58-8801-d9ecce5f0c0b', secondary, gate_of_ijtihad_closed).
narrative_ontology:cs_axiom_status(gate_of_ijtihad_closed, holdable).
narrative_ontology:cs_axiom_grounding('bacabeb0-a634-4c58-8801-d9ecce5f0c0b', gate_of_ijtihad_closed, conventional).
narrative_ontology:cs_reference_frame('bacabeb0-a634-4c58-8801-d9ecce5f0c0b', classical_madhhab_authority).
narrative_ontology:cs_drift_state('bacabeb0-a634-4c58-8801-d9ecce5f0c0b', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bacabeb0-a634-4c58-8801-d9ecce5f0c0b', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, traditional_ulama).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, mosque_hierarchies).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, progressive_muslims).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, women_seeking_equal_status).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, religious_minorities_dhimmi).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, state_authorities_traditionalist).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__traditionalist_taqlid, classical_ijma_binding_authority).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__traditionalist_taqlid, taqlid_as_religious_obligation).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__traditionalist_taqlid, madhhab_infallibility_in_usul).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Senior scholars and institutional leaders of the madhhab system. They control the authoritative interpretation of fiqh, staff the sharia courts and fatwa bodies, direct the curricula of madrasas, and manage waqf endowments that fund the institutional infrastructure. Their authority derives from isnad (transmission chains) linking them to classical founders. They can move between institutional posts across countries, giving them arbitrage-grade exit, but their identity and capital are fused with the madhhab system. They justify taqlid as preservation of divine law; critics see institutional rent capture.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, traditional_ulama, agenda_setter,
    institutional, generational, arbitrage, global).

% The corporate entities of the four Sunni madhhabs (Hanafi, Maliki, Shafi'i, Hanbali) and their associated seminaries (Al-Azhar, Deoband, Qom-adjacent for Shi'i parallel), fatwa councils, and certification bodies. They control the pipeline from student to mufti, issue binding legal opinions, and collect fees for certification, arbitration, and education. Their endowments (waqf) and state subsidies provide material base. They coordinate globally through organizations like the International Islamic Fiqh Academy. Exit is arbitrage-grade for the institution (can relocate, rebrand), but the institution's existence depends on the taqlid system.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions, beneficiary,
    institutional, generational, arbitrage, global).

% Local and regional mosque networks, imams, and community leaders who implement madhhab rulings in daily practice — leading prayers, conducting marriages/funerals, advising congregants. Their authority derives from madhhab affiliation and institutional appointment. They benefit from communal trust, material support (donations, state salaries in some contexts), and social status. Exit is constrained: leaving the madhhab framework means losing communal recognition and livelihood, but some migrate to reformist or independent spaces.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, mosque_hierarchies, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__traditionalist_taqlid, mosque_hierarchies, agenda_setter).

% Muslims who seek to interpret the sources directly or through contemporary ethical frameworks (human rights, gender equality, pluralism). They are denied authoritative interpretive voice — the taqlid system defines ijtihad as closed to non-mujtahids, and the mujtahid qualification is controlled by the madhhab institutions. They bear the cost of rulings they contest (e.g., on apostasy, blasphemy, gender, sexuality) with no structural path to change them from within. Exit options: remain and dissent (risk sanction), leave the tradition (apostasy stigma, communal rupture), or migrate to minority reformist spaces (marginal, resource-poor). Identity-locked for many: Muslim identity is constituted through the tradition they contest.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, progressive_muslims, payer,
    moderate, biographical, trapped, global).

% Muslim women advocating for equal rights in marriage, divorce, child custody, inheritance, testimony, and religious leadership. Classical madhhab rulings systematically disadvantage women in these domains (e.g., unilateral male divorce, half inheritance shares, testimony weight, guardianship requirements). The taqlid obligation binds them to these rulings as religious duty. Reformist arguments (maslaha, maqasid, ethical trajectory) are dismissed as exceeding lay competence. Exit is trapped: compliance violates conscience; resistance risks communal exclusion and state sanction in traditionalist jurisdictions; reformist spaces exist but lack authoritative recognition. Identity-locked: gendered religious identity fuses submission to the system with piety.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, women_seeking_equal_status, payer,
    moderate, biographical, trapped, global).

% Non-Muslim communities (Christians, Jews, Zoroastrians, others) living under classical dhimmi frameworks codified in family law and personal status codes of traditionalist-dominant states. They are subject to discriminatory rules: poll tax (jizya) historical memory, restricted worship, building/repair limitations, testimony inequality, marriage restrictions (Muslim women cannot marry non-Muslim men), inheritance exclusion. The taqlid system treats these rulings as immutable ijma. They have no voice in the interpretive process — the ulama adjudicate their status without their participation. Exit is trapped: emigration is costly and not universally available; conversion is the only internal exit but coerced. Generational time horizon: the framework persists across generations regardless of individual consent.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, religious_minorities_dhimmi, payer,
    powerless, generational, trapped, national).

% Governments in traditionalist-dominant contexts (Saudi Arabia, Iran, Pakistan, Gulf states, parts of Southeast Asia) that codify madhhab rulings as positive law — family law, criminal law (hudud), blasphemy/apostasy statutes. They gain Islamic legitimacy by enforcing the traditionalist reading, control the ulama through appointment and funding, and use the system for social control. They benefit from the constraint's suppression function (dissent = religious deviance = political threat). Exit is arbitrage-grade: states can and do shift toward state_hybrid models (e.g., Morocco, Tunisia, Indonesia partial reforms), but legitimacy costs are high.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, state_authorities_traditionalist, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__traditionalist_taqlid, state_authorities_traditionalist, beneficiary).

% Scholars and intellectuals advocating reformist_ijtihad: contextual reinterpretation, ethical prioritization, maqasid-based reasoning. They are structurally excluded from authoritative bodies (fiqh academies, fatwa councils, judiciary) — the taqlid system defines their method as illegitimate. They operate in universities, NGOs, minority communities, digital spaces. They bear reputational costs (heresy accusations, fatwas against them) and are denied institutional resources. Exit is constrained: they cannot enter the mainstream authority structure without submitting to taqlid; they build parallel structures with limited recognition.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, reformist_scholars, excluded,
    moderate, biographical, constrained, global).

% Scholars of Islamic law, history, anthropology, sociology, political science studying the madhhab system from outside. They have no stake in the constraint's operation — they analyze its structure, history, and effects. Their exit is analytical: they can change frameworks, methods, or objects of study without personal cost. They provide the corroboration for founding_problem_status (contested) by documenting historical contingency of madhhab consolidation and the political negotiation of the taqlid/ijtihad boundary.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, academic_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_hadith_substrate__traditionalist_taqlid, traditional_ulama).
narrative_ontology:fixing_cost_class(quran_hadith_substrate__traditionalist_taqlid, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, authoritative legal framework for Muslim communities through recognized schools of law (madhhabs), preventing chaotic fragmentation of legal practice by channeling interpretation through authorized transmitters and established methodologies (usul al-fiqh). Solves the post-prophetic problem of how to derive binding law from fixed revelation across time and space.
% TRANSFER_FUNCTION: Moves interpretive authority and final legal determination from individual conscience and direct textual engagement to institutional madhhab authorities (ulama, muftis, councils). Moves social compliance costs — gendered legal disabilities, subjection of minorities, foreclosure of dissent — onto those subject to the rulings. Moves material resources (waqf revenue, state salaries, certification fees, communal donations) to the ulama/institutional beneficiaries.
% ABSENT_VOICES: Reformist scholars (advocating contextual ijtihad), progressive Muslims (seeking ethical reinterpretation), women's rights advocates (challenging gendered rulings), religious minorities (subject to dhimmi framework without consent), LGBTQ+ Muslims (criminalized under classical hudud), ex-Muslims (apostasy penalties). They are excluded from authoritative interpretation by the taqlid system's definition of who may speak for the tradition. Their absence is structural: the system's coherence depends on their exclusion.
% DISAPPEARANCE_RATIONALE: If the taqlid obligation and madhhab authority vanished overnight: (1) Muslims would need new mechanisms for legal guidance — individual ijtihad, new institutions, or state law would fill the vacuum; (2) madhhab institutions (Al-Azhar, Deoband, Fiqh Academies) would lose their legitimating function and primary revenue basis; (3) family law codes in traditionalist states (Saudi Arabia, Iran, Pakistan, etc.) would lose their doctrinal foundation, requiring legislative replacement; (4) the ulama class would lose its distinctive authority claim, reshaping religious-political relations; (5) reformist and state_hybrid readings would become the only live frameworks, restructuring the entire kernel's interpretive field.
% FOUNDING_PROBLEM: Post-prophetic need for authoritative legal derivation from fixed revelation (Quran/Hadith) to prevent chaotic fragmentation of legal practice across the expanding Muslim community. The early community faced competing interpretations, regional divergence, and political fissures; the madhhab system crystallized around recognized masters to provide stable, systematic, and widely accepted legal guidance.
% FOUNDING_PROBLEM_CORROBORATION: Traditionalist ulama attest the problem is live: fragmentation risk persists without taqlid (citing contemporary sectarianism and 'DIY fatwa' culture). Historians of Islamic law outside the beneficiary set (Wael Hallaq, George Makdisi, Christopher Melchert, Knut Vikør) document that madhhab consolidation was historically contingent — a 10th-12th century settlement, not a prophetic mandate — and that the 'closing of the gate of ijtihad' is a later doctrinal construction. Sociologists of religion (Talal Asad, Saba Mahmood) show the taqlid/ijtihad boundary as politically negotiated. No consensus exists; the founding problem's status is genuinely contested.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__traditionalist_taqlid, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__traditionalist_taqlid, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__traditionalist_taqlid, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_hadith_substrate__traditionalist_taqlid, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__traditionalist_taqlid, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   The claimed_type is tangled_rope because the constraint possesses BOTH a genuine coordination function (stable legal framework preventing chaotic fragmentation, solved through recognized schools) AND asymmetric extraction (ulama institutions capture interpretive rents, progressive Muslims/women/minorities bear costs). Requires active enforcement: suppression of alternative readings is not incidental but constitutive — the system's authority depends on foreclosing ijtihad by non-mujtahids. Metrics: extractiveness 0.72 (high — institutional rents from certification, education, adjudication, state salaries; compliance costs on subjects); suppression 0.78 (high — state law, social sanction, educational gatekeeping); theater_ratio 0.45 (moderate-rising — coordination function real but declining share of enforcement activity; growing performative defense of 'tradition' against reformist challenges); accessibility_collapse 0.82 (very high — alternatives structurally foreclosed by definition of taqlid); resistance 0.55 (moderate — reformist movements exist but face high suppression). The interval 0-100 tracks roughly 1920-2020: early 20th century saw madhhab system still functionally coordinating; mid-century state codification increased extraction; late-century reformist challenges triggered suppression intensification and theater growth.
 *
 * PERSPECTIVAL GAP:
 *   The ulama/madhhab seat experiences this as genuine coordination they maintain — a sacred trust preserving divine law. The progressive Muslim/woman/minority seats experience it as enforced extraction with no exit. The state authority seat in traditionalist contexts experiences it as legitimacy infrastructure they administer. The reformist scholar seat experiences it as structural exclusion. The engine computes per-seat classifications from these structural asymmetries: the same constraint computes as rope/scaffold from the agenda-setter seat, snare from the victim seats, piton from the observer seat seeing institutional inertia.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (traditional_ulama, madhhab_institutions, mosque_hierarchies, state_authorities_traditionalist) collect interpretive authority, material resources, social capital, and political legitimacy — directionality d near 0.0 (beneficiary end). Victims (progressive_muslims, women_seeking_equal_status, religious_minorities_dhimmi) bear compliance costs, denied agency, gendered legal disabilities, and subjection without consent — directionality d near 1.0 (target end). Reformist_scholars are excluded — not coordinated, not extracted from directly, but structurally prevented from competing — directionality ambiguous (engine derives from exit_options: constrained). Academic_observers are analytical — d = 0.5 by definition. The derivation chain: beneficiary/victim declarations + power atoms (institutional vs moderate/powerless) + exit_options (arbitrage vs trapped) → engine computes d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-prophetic need for authoritative legal derivation preventing fragmentation) was live in the formative period. The traditionalist reading claims it remains live — the same fragmentation risk exists today. Reformist readings argue the problem is dead/transformed: modern conditions (nation-states, human rights norms, mass literacy) require new coordination forms. Corroboration from outside beneficiaries: historians of Islamic law (Hallaq, Makdisi, Melchert) document madhhab consolidation as historically contingent, not inevitable; sociologists of religion document the taqlid/ijtihad boundary as politically negotiated. The mandatrophy tension: if founding problem is dead but arrangement persists with high extraction, the constraint drifts toward snare/piton. The traditionalist reading denies mandatrophy by declaring founding problem live; the engine detects the drift via theater_ratio rise and extraction accumulation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint is the traditionalist_taqlid reading of the quran_hadith_substrate kernel. What structural elements distinguish it from the reformist_ijtihad and state_hybrid readings?',
    'Comparative constraint analysis across the three readings: map beneficiary/victim sets, suppression mechanisms, and extraction profiles for each reading of the same kernel.',
    'If the three readings show divergent ε values and distinct victim structures, the kernel label ''quran_hadith_substrate'' is confirmed as a conflation of structurally distinct constraints — each reading instantiates a different constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Kernel reading decomposition: traditionalist_taqlid vs reformist_ijtihad vs state_hybrid').

omega_variable(
    coordination_extraction_boundary,
    'Is the madhhab system''s coordination function (legal certainty, community cohesion) structurally separable from its extraction function (ulama institutional rents, suppression of dissenting interpretation)?',
    'Counterfactual: if enforcement of taqlid were relaxed but madhhab texts remained available as non-binding guidance, would legal coordination collapse or persist through voluntary adherence?',
    'If separable, the high extraction is not the price of coordination but a separable layer of institutional rent — supporting tangled_rope over rope. If inseparable, part of measured ε is genuine coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether coordination and extraction are structurally separable in the madhhab system').

omega_variable(
    state_enforcement_dependency,
    'To what degree does the constraint''s suppression rely on state enforcement (family law codes, blasphemy/apostasy laws) versus social enforcement (communal shunning, mosque authority)?',
    'Comparative study of taqlid enforcement in states with classical family law codification vs. secular states with Muslim minorities — measure suppression differential.',
    'If state enforcement is the primary suppression engine, the constraint''s persistence is politically contingent; if social enforcement suffices, it is more structurally inertial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_enforcement_dependency, empirical, 'State vs. social enforcement as suppression mechanism').

omega_variable(
    dhimmi_framework_extraction,
    'Does the classical dhimmi framework''s application to religious minorities constitute a distinct extraction circuit from the taqlid obligation on Muslims, or is it the same constraint operating on a different population?',
    'Analyze whether religious minorities are subject to the same interpretive authority (ulama/madhhab) or a separate state-administered framework; trace benefit flows.',
    'If distinct circuit, religious minorities are victims of a coupled but separate constraint (link via network.affects_constraints). If same circuit, their victimization is direct evidence of the taqlid constraint''s asymmetric extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dhimmi_framework_extraction, conceptual, 'Whether dhimmi subjection is same or distinct constraint from Muslim taqlid').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__traditionalist_taqlid, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qhs_taqlid_tr_t0, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(qhs_taqlid_tr_t0, observed).
narrative_ontology:measurement(qhs_taqlid_tr_t20, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 20, 0.32).
narrative_ontology:measurement_basis(qhs_taqlid_tr_t20, observed).
narrative_ontology:measurement(qhs_taqlid_tr_t40, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 40, 0.38).
narrative_ontology:measurement_basis(qhs_taqlid_tr_t40, observed).
narrative_ontology:measurement(qhs_taqlid_tr_t60, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 60, 0.42).
narrative_ontology:measurement_basis(qhs_taqlid_tr_t60, observed).
narrative_ontology:measurement(qhs_taqlid_tr_t80, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 80, 0.44).
narrative_ontology:measurement_basis(qhs_taqlid_tr_t80, observed).
narrative_ontology:measurement(qhs_taqlid_tr_t100, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 100, 0.45).
narrative_ontology:measurement_basis(qhs_taqlid_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(qhs_taqlid_be_t0, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(qhs_taqlid_be_t0, observed).
narrative_ontology:measurement(qhs_taqlid_be_t20, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(qhs_taqlid_be_t20, observed).
narrative_ontology:measurement(qhs_taqlid_be_t40, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 40, 0.67).
narrative_ontology:measurement_basis(qhs_taqlid_be_t40, observed).
narrative_ontology:measurement(qhs_taqlid_be_t60, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 60, 0.7).
narrative_ontology:measurement_basis(qhs_taqlid_be_t60, observed).
narrative_ontology:measurement(qhs_taqlid_be_t80, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 80, 0.71).
narrative_ontology:measurement_basis(qhs_taqlid_be_t80, observed).
narrative_ontology:measurement(qhs_taqlid_be_t100, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 100, 0.72).
narrative_ontology:measurement_basis(qhs_taqlid_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(qhs_taqlid_su_t0, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 0, 0.65).
narrative_ontology:measurement_basis(qhs_taqlid_su_t0, observed).
narrative_ontology:measurement(qhs_taqlid_su_t20, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(qhs_taqlid_su_t20, observed).
narrative_ontology:measurement(qhs_taqlid_su_t40, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 40, 0.74).
narrative_ontology:measurement_basis(qhs_taqlid_su_t40, observed).
narrative_ontology:measurement(qhs_taqlid_su_t60, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 60, 0.76).
narrative_ontology:measurement_basis(qhs_taqlid_su_t60, observed).
narrative_ontology:measurement(qhs_taqlid_su_t80, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 80, 0.77).
narrative_ontology:measurement_basis(qhs_taqlid_su_t80, observed).
narrative_ontology:measurement(qhs_taqlid_su_t100, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 100, 0.78).
narrative_ontology:measurement_basis(qhs_taqlid_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__traditionalist_taqlid, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_hadith_substrate__traditionalist_taqlid, 0.08).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate__reformist_ijtihad).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate__state_hybrid).

% DUAL FORMULATION NOTE:
% This constraint (traditionalist_taqlid) and its siblings (reformist_ijtihad, state_hybrid) form a constraint family decomposing the quran_hadith_substrate kernel. The ε values differ substantially: traditionalist_taqlid ε ≈ 0.72 (high extraction, institutionalized enforcement); reformist_ijtihad ε ≈ 0.35 (lower extraction, coordination via ethical reinterpretation); state_hybrid ε ≈ 0.55 (moderate extraction, selective enforcement). They share the same textual substrate but instantiate different constraints with different beneficiary/victim structures and suppression mechanisms. Network edges reflect structural influence: traditionalist_taqlid forecloses reformist_ijtihad within any single framework and influences state_hybrid by setting legitimacy conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_hadith_substrate__traditionalist_taqlid, institutional, 0.15).
constraint_indexing:directionality_override(quran_hadith_substrate__traditionalist_taqlid, moderate, 0.85).
constraint_indexing:directionality_override(quran_hadith_substrate__traditionalist_taqlid, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
