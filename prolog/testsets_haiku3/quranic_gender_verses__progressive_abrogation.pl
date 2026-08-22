% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__progressive_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__progressive_abrogation, []).

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
 *   constraint_id: quranic_gender_verses__progressive_abrogation
 *   human_readable: Qur'anic Gender Verses: Progressive Abrogation Reading
 *   domain: religious/legal/gender
 *
 * SUMMARY:
 *   The progressive abrogation reading interprets Qur'anic verses on gender
 *   through the principle of naskh (abrogation), claiming that later
 *   egalitarian principles (e.g., 49:13 on universal human dignity) supersede
 *   earlier gender-specific rules (4:34 on guardianship, 2:282 on testimony
 *   weight, 4:11 on inheritance shares). This reading is ONE instantiation of
 *   a contested kernel—the Qur'anic gender verses themselves—and competes
 *   with literal-hierarchical and contextual-egalitarian interpretations.
 *   Under this reading, traditional authority structures that defend gender
 *   differentiation are delegitimized as anti-Qur'anic; women's full legal
 *   parity is the Qur'an's true endpoint. The constraint extracts heavily
 *   from traditionalist scholars and communities identity-locked to literal
 *   interpretations, while benefiting egalitarian reformers and women
 *   interpreters seeking doctrinal authority for gender equity. This story
 *   authors the progressive abrogation reading as a clean, ε-invariant
 *   constraint; it does NOT average across readings or hedge ε. Sibling
 *   readings (literal_hierarchical, contextual_egalitarian) are separate
 *   constraint stories, linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - egalitarian_reform_movements: organized beneficiaries advancing the reading in scholarship and policy
 *   - women_interpreters: moderate-power beneficiaries gaining doctrinal authority but facing institutional barriers
 *   - literal_traditionalist_scholars: institutional payers whose authority is delegitimized; identity_locked exit
 *   - communities_identity_bound_to_literal_reading: powerless payers facing epistemic violence and social pressure
 *   - contextual_egalitarian_scholars: beneficiary-payers whose hermeneutic is both supported and challenged
 *   - traditional_religious_authority_institutions: agenda_setters maintaining boundary enforcement
 *   - secular_governance_systems: excluded from authority but may benefit from the reading's outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__progressive_abrogation, 0.92).
domain_priors:suppression_score(quranic_gender_verses__progressive_abrogation, 0.78).
domain_priors:theater_ratio(quranic_gender_verses__progressive_abrogation, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, extractiveness, 0.92).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__progressive_abrogation, tangled_rope).
narrative_ontology:human_readable(quranic_gender_verses__progressive_abrogation, "Qur'anic Gender Verses: Progressive Abrogation Reading").
narrative_ontology:topic_domain(quranic_gender_verses__progressive_abrogation, "religious/legal/gender").

domain_priors:requires_active_enforcement(quranic_gender_verses__progressive_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__progressive_abrogation, '61df7782-e7e9-436a-ab5a-f73ddacd6885').
narrative_ontology:cs_kernel_codification('61df7782-e7e9-436a-ab5a-f73ddacd6885', fixed_text).
narrative_ontology:cs_authority_grounding('61df7782-e7e9-436a-ab5a-f73ddacd6885', extraction).
narrative_ontology:cs_interpretation_layer_present('61df7782-e7e9-436a-ab5a-f73ddacd6885').
narrative_ontology:cs_reading_relation('61df7782-e7e9-436a-ab5a-f73ddacd6885', quranic_gender_verses__literal_hierarchical, coexists_with).
narrative_ontology:cs_reading_relation('61df7782-e7e9-436a-ab5a-f73ddacd6885', quranic_gender_verses__contextual_egalitarian, influences).
narrative_ontology:cs_axiom('61df7782-e7e9-436a-ab5a-f73ddacd6885', foundational, quranic_trajectory_toward_universal_dignity).
narrative_ontology:cs_axiom_status(quranic_trajectory_toward_universal_dignity, holdable).
narrative_ontology:cs_axiom_grounding('61df7782-e7e9-436a-ab5a-f73ddacd6885', quranic_trajectory_toward_universal_dignity, empirically_contingent).
narrative_ontology:cs_axiom('61df7782-e7e9-436a-ab5a-f73ddacd6885', foundational, gender_specific_verses_contextual_not_eternal).
narrative_ontology:cs_axiom_status(gender_specific_verses_contextual_not_eternal, holdable).
narrative_ontology:cs_axiom_grounding('61df7782-e7e9-436a-ab5a-f73ddacd6885', gender_specific_verses_contextual_not_eternal, empirically_contingent).
narrative_ontology:cs_reference_frame('61df7782-e7e9-436a-ab5a-f73ddacd6885', quranic_moral_trajectory_toward_equity).
narrative_ontology:cs_drift_state('61df7782-e7e9-436a-ab5a-f73ddacd6885', contemporary_institutional_advancement, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('61df7782-e7e9-436a-ab5a-f73ddacd6885', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__progressive_abrogation, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, women_interpreters).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, egalitarian_reform_movements).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, literal_traditionalist_scholars).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, communities_identity_bound_to_literal_reading).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, contextual_egalitarian_scholars).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, contextual_egalitarian_scholars).
narrative_ontology:constraint_vindicates(quranic_gender_verses__progressive_abrogation, quranic_moral_trajectory_toward_equity).
narrative_ontology:constraint_vindicates(quranic_gender_verses__progressive_abrogation, naskh_as_hermeneutic_principle).
narrative_ontology:constraint_vindicates(quranic_gender_verses__progressive_abrogation, women_full_legal_parity_as_quranic_endpoint).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organized networks of Islamic scholars, advocates, and civil society organizations advancing gender-egalitarian interpretations of Islamic law. Under the progressive abrogation reading, they gain doctrinal authority to claim that the Qur'an's true trajectory leads to women's full legal parity. Institutionalize the reading through educational programs, publications, fatwas, and policy advocacy. Benefit from international recognition as legitimate Islamic thought and from normative shifts in family law, testimony rules, and inheritance in jurisdictions where the reading gains traction.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, egalitarian_reform_movements, beneficiary,
    organized, generational, mobile, global).

% Islamic scholars, jurists, and legal professionals (women and male allies) who adopt and advance the progressive abrogation reading. Gain authority to speak within Islamic jurisprudence as representatives of the tradition's true intent rather than as external voices importing secular feminism. Face institutional gatekeeping in seminaries, councils of senior scholars, and fatwa bodies that may reject or marginalize the reading. Exit costs are high: abandoning the reading may require accepting marginalization within Islamic institutions or leaving the field entirely.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, women_interpreters, beneficiary,
    moderate, biographical, constrained, global).

% Senior scholars, fatwa councils, and theological authorities who defend a literal-hierarchical reading of verses 4:34, 2:282, and 4:11 as direct, timeless divine ordinance establishing male guardianship and differentiated rights. The progressive abrogation reading directly invalidates their interpretive framework and delegitimizes their scholarly authority as misreading or medieval-era constraint on the Qur'an's actual trajectory. Exit from the literalist position requires abandoning centuries of transmitted jurisprudence (tafsir, hadith commentary, fiqh lineages) that constitute their professional and scholarly identity. They pay through loss of institutional authority, scholarly recognition, and influence over Islamic legal development.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, literal_traditionalist_scholars, payer,
    institutional, generational, identity_locked, global).

% Families and communities—primarily in traditionalist-majority regions—whose social structure, kinship norms, guardianship practices, and self-understanding are organized around literal interpretations of gender verses. Experience the progressive abrogation reading as epistemic violence: external reinterpretation that frames their lived practice, inherited jurisprudence, and relational arrangements as un-Islamic, developmentally primitive, or contrary to the Qur'an's true meaning. Face pressure through educational systems, media, policy changes, and social delegitimization to abandon kinship norms. Exit options are severely constrained: staying in the community means practicing what reformers call 'un-Islamic' gender roles; leaving means severing kinship, religious identity, and social belonging. The power asymmetry is extreme: their objections are framed as resistance to progress rather than legitimate interpretive positions.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, communities_identity_bound_to_literal_reading, payer,
    powerless, biographical, identity_locked, regional).

% Scholars and jurists who advocate for gender equity through reinterpretation of Qur'anic principles (maqasid al-sharia—overarching objectives like justice, dignity, freedom) without invoking the abrogation principle. They benefit from the progressive abrogation reading's support for their equity endpoint and from institutional validation that gender equality is genuinely Qur'anic. However, they pay through hermeneutic subordination: the naskh framework asserts that abrogation (not context-sensitive maqasid interpretation) is the mechanism explaining gender-specific verses. This threatens their methodological authority and the interpretive flexibility that allows them to support both equity and certain traditionalist practices. They face pressure to either abandon contextual interpretation or adopt the stronger abrogation claim.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, contextual_egalitarian_scholars, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__progressive_abrogation, contextual_egalitarian_scholars, payer).

% Islamic scholarly hierarchies, fatwa councils, theological seminaries, and interpretive authorities (Al-Azhar, international jurisprudential bodies, national Islamic councils) whose institutional legitimacy rests on their authority to adjudicate valid Islamic scholarship and law. Maintain boundary enforcement over which interpretations count as serious Islamic jurisprudence vs. exogenous impositions or doctrinal error. The progressive abrogation reading challenges their authority structure: if naskh validly overrides literalist readings, then the institutions' custodianship of transmitted jurisprudence (which included the literalist positions) is flawed. Enforce the reading's legitimacy (or literalism's, in resistor institutions) through fatwa, educational curricula, credentialing of scholars, and public religious leadership. Trapped between institutional inertia (their authority derives from safeguarding transmitted tradition) and the pressure to modernize jurisprudence toward contemporary gender norms.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, traditional_religious_authority_institutions, agenda_setter,
    institutional, civilizational, trapped, global).

% Legal systems outside Islamic jurisprudence—nation-state courts, human rights frameworks, international law bodies—that impose gender-egalitarian law on Muslim-majority countries or regulate Islamic law practices within secular jurisdictions. Would benefit from the progressive abrogation reading as Islamic doctrinal justification for those egalitarian legal outcomes, since it validates gender equity as intrinsically Islamic rather than imposed from outside. However, they are structurally excluded from adjudicating Islamic hermeneutics; their attempts to use the reading as a legitimation tool for state law are often resisted by both traditionalist and egalitarian Islamic scholars as illegitimate intrusion into religious authority. Their exclusion is maintained through the principle that Islamic jurisprudence is governed by Islamic authority structures, not secular law.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, secular_governance_systems, excluded,
    institutional, generational, arbitrage, global).

% The accumulated corpus of Qur'anic interpretation (tafsir) across Islamic history—the scholarly and spiritual readings developed over 1400 years. Neither collects from nor pays into the constraint; it is the body of knowledge within which all three readings (progressive abrogation, literal hierarchical, contextual egalitarian) claim legitimacy. Observes how different readings selectively invoke tafsir tradition to support their positions and how institutional gatekeeping determines which tafsir sources are recognized as authoritative.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, quranic_exegetical_tradition, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(quranic_gender_verses__progressive_abrogation, quranic_exegetical_tradition).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quranic_gender_verses__progressive_abrogation, egalitarian_reform_movements).
narrative_ontology:fixing_cost_class(quranic_gender_verses__progressive_abrogation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes scriptural interpretation around a hermeneutic principle (naskh) that enables doctrinal development: reading the Qur'an as a trajectory rather than a fixed set of timeless ordinances allows the Islamic tradition to claim evolving moral insight without abandoning textual authority.
% TRANSFER_FUNCTION: Moves authority from literal traditionalist scholarly lineages (who defend 4:34, 2:282, 4:11 as binding) to egalitarian reformers and women interpreters. Transfers legitimacy from established institutions of Qur'anic interpretation to new scholars claiming alignment with the text's 'true trajectory.' Transfers social costs from women subject to gender-specific rules onto traditionalist communities and literalist scholars, whose worldviews are reframed as un-Islamic or developmentally incomplete.
% ABSENT_VOICES: Literal traditionalist scholars are present as payers but marginalized; secular feminists outside Islam are excluded from authority to interpret the Qur'an; women subject to literal rules in practice but not engaged in scholarly interpretation remain absent from the authority conversation.
% DISAPPEARANCE_RATIONALE: If the progressive abrogation reading disappeared (reverted to dominant literalism), Islamic gender jurisprudence would reorganize entirely: legal parity framings would lose doctrinal grounding within Islamic scholarship, women reformers would lose the authority to claim Qur'anic sanction for equality, and traditional gender-specific rules would hold undisputed authority. Conversely, if literalism disappeared (abrogation reading achieved total hegemony), the traditionalist jurisprudence would be delegitimized and communities organized around it would face institutional pressure to abandon inherited practices.
% FOUNDING_PROBLEM: The Qur'an contains verses that modern egalitarians read as gender-differentiating (4:34, 2:282, 4:11) and later verses emphasizing universal human dignity (49:13, 30:21). The problem: how to maintain scriptural authority and doctrinal continuity while aligning Islamic jurisprudence with contemporary understandings of human rights and gender equality. The founding problem is the intra-Islamic interpretive crisis: literalism and egalitarianism appear incompatible.
% FOUNDING_PROBLEM_CORROBORATION: Egalitarian reformers and women scholars attest that the founding problem is live and urgent: they invoke the naskh principle as the solution. Traditionalist scholars attest the problem is a false crisis created by external pressure; they argue no incompatibility exists if gender-specific verses are understood as timeless divine ordinance. Secular human-rights advocates and feminist scholars (from outside Islam) attest the problem exists but argue the progressive abrogation reading is itself a compromise that fails to achieve full equality. No unified corroboration exists—the status remains contested across authority structures.
narrative_ontology:disappearance_verdict(quranic_gender_verses__progressive_abrogation, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__progressive_abrogation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__progressive_abrogation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quranic_gender_verses__progressive_abrogation, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__progressive_abrogation, 0.92, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__progressive_abrogation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quranic_gender_verses__progressive_abrogation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quranic_gender_verses__progressive_abrogation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is very high (0.92 at interval end) because the reading completely inverts the normative status of gender-specific verses—what was binding becomes abrogated, what traditionalists defend as divine ordinance becomes medieval misreading. This is not a marginal reinterpretation; it is a comprehensive doctrinal realignment that invalidates centuries of transmitted jurisprudence. Suppression is high (0.78) because this reading's institutional propagation depends on actively excluding literalist interpretations from recognition as legitimate Islamic scholarship—fatwa councils, seminaries, and scholarly hierarchies enforce which readings count. Theater ratio is moderate (0.41) because the reading includes genuine hermeneutic work (the naskh principle is a real Islamic tool), but a substantial share of institutional activity defends the reading's authority status rather than advancing interpretive rigor. The measurement series show both extractiveness and suppression rising over the interval (0–40), reflecting the reading's increasing institutional penetration and the corresponding escalation of boundary enforcement against literalist alternatives. Accessibility collapse is highest at the structural level (0.72 at t=40: the framework itself becomes harder to exit once the abrogation principle is institutionalized) and lower at the individual level (0.62: individuals retain some exit via private belief, though social costs are high). Resistance remains relatively stable (0.65–0.72 across levels), indicating persistent organized pushback from traditionalist scholars and communities.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (egalitarian reformers, women interpreters), the progressive abrogation reading is legitimate hermeneutics recovering the Qur'an's true trajectory. From the literalist seats (traditionalist scholars, identity-bound communities), the same reading is exogenous imposition that violates textual authority and inherited tradition. From the contextual-egalitarian seat, the reading is both ally (it supports women's full parity) and threat (it usurps the contextual-interpretation hermeneutic). The engine computes these divergent per-seat classifications from the structural data—who benefits, who bears costs, what directionality each faces. The authored claim (tangled_rope) reflects the reading's hybrid structure: genuine coordination function (organizing scriptural interpretation around a hermeneutic principle), AND substantial extraction (from literalists and identity-bound communities). The metrics should diverge by seat, not be tuned to the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Egalitarian reform movements are structural beneficiaries (d → 0.15): they advance the reading, gain institutional recognition, collect the normative shift toward equality. Women interpreters are beneficiaries facing constrained exit (d → 0.25–0.35): they gain doctrinal authority but are trapped within Islamic scholarly institutions that may penalize the reading; exit means leaving the tradition. Literalist scholars are payers at the institutional level (d → 0.85): their authority is directly invalidated; they are identity_locked (exit means abandoning centuries of transmitted tradition and their scholarly identity). Communities organized around literal reading are powerless payers (d → 0.92): they face the most severe directionality—trapped exit, diffuse social pressure, epistemic violence that frames their worldview as un-Islamic. Contextual egalitarians have complex positioning (d → 0.5–0.6): they benefit from the equity outcome but are threatened by the hermeneutic methodology that sidelines context-based interpretation. Traditional authority institutions are dual-positioned: as agenda_setters they maintain the reading's enforcement (moderate d), but they are trapped because their legitimacy depends on controlling interpretation boundaries—they cannot easily abandon the authority function without dissolution.
 *
 * MANDATROPHY ANALYSIS:
 *   The progressive abrogation reading exhibits mandatrophy risk: the founding problem ('how to reconcile scriptural authority with gender equality') was genuine in the late 20th century when the reading gained traction. But the naskh principle, once institutionalized, becomes a tool for delegitimizing any non-egalitarian reading rather than a hermeneutic principle applied to specific textual ambiguities. The extraction from literalists and identity-bound communities persists not because the founding problem is live but because the reading's authority requires silencing alternatives. The theater_ratio rise (0.28 → 0.41) reflects this drift: early institutional deployment emphasized genuine exegetical work; later deployment emphasizes boundary enforcement and delegitimization. The six_questions assessment is critical: founding_problem_status is contested because egalitarian reformers attest the problem is live (women's equality is not yet secured), while traditionalists attest the problem is false (no incompatibility exists if verses are read literally). This contestation is itself signal of mandatrophy pressure—the constraint persists because different authority structures disagree on whether it solves anything, not because the problem is universally recognized.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naskh_boundary_ambiguity,
    'Does the naskh principle apply consistently to verses 4:34, 2:282, and 4:11, or do these verses have different logical status in the Qur''anic architecture (some potentially eternal, others contextual)?',
    'Detailed textual analysis across the Qur''anic corpus examining each verse''s internal framing (e.g., ''for all time'' vs. ''in your matter'' vs. conditional phrasing). Cross-examination by non-partisan exegetes from different traditional schools.',
    'If the verses have inconsistent logical status, the progressive abrogation reading is overreaching—it flattens potentially distinct categories into a single ''abrogated'' class. The reading''s hermeneutic coherence and its claim to represent the Qur''an''s true trajectory would be weakened. If naskh applies uniformly, the reading''s structural legitimacy is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(naskh_boundary_ambiguity, conceptual, 'Whether the naskh principle applies consistently across the gender-specific verses or whether they have different textual status.').

omega_variable(
    identity_fusion_suppression_mechanism,
    'Is the measured suppression of literalist readings structurally enforced (institutional gates, fatwa councils actively excluding them), or is it internalized (scholars abandon the reading because they adopt the egalitarian framework''s legitimacy claims)?',
    'Post-institutional-pressure surveys of traditionalist scholars: do they privately retain literalist positions but publicly conform? Are there cases where scholars successfully defend literalist readings within institutional contexts? How do scholars describe the experience of exit pressure—external barriers or internalized conviction?',
    'If suppression is purely structural, the reading''s persistence depends on institutional maintenance and could reverse if authority structures shift. If suppression is substantially internalized (scholars genuinely accept egalitarianism as more Qur''anic), the reading''s dominance is more stable but the social costs to communities identity-locked to literalism are higher (internalized rather than external pressure). If internalized, the omegas on epistemic violence become more critical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_suppression_mechanism, empirical, 'Whether suppression of literalist readings is structural institutional enforcement or internalized acceptance of egalitarian legitimacy.').

omega_variable(
    epistemic_violence_feedback,
    'Does the institutional delegitimization of literalist readings as ''un-Islamic'' (reframing them as medieval misreading rather than legitimate jurisprudence) create a feedback loop where communities resist the progressive reading, hardening their identity-lock and resistance?',
    'Longitudinal study of communities exposed to the progressive abrogation reading: does institutional pressure correlate with increased traditionalist mobilization, community boundary-strengthening, or identity-fusion (the community becomes ''defenders of traditional Islam'' rather than just practitioners)? Do resistance movements emerge in response to epistemic delegitimization?',
    'If yes, the reading''s effective extractiveness may be understated: the measured 0.92 captures direct normative inversion, but if it triggers resistance cycles and identity-strengthening, the constraint''s actual social costs are higher. The reading would be self-reinforcing extraction (the more it suppresses, the more communities entrench, requiring more suppression). If no, the reading''s acceptance is slower but less socially destructive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_violence_feedback, empirical, 'Whether institutional delegitimization of literalist readings triggers feedback hardening of community resistance and identity-lock.').

omega_variable(
    kernel_reading_foreclosure_status,
    'Does the progressive abrogation reading logically foreclose the literal-hierarchical reading within a single Islamic framework, or do they remain coexistent as competing interpretations held by different parties?',
    'Formal logical analysis: if a scholar adopts the naskh principle (later verses abrogate earlier ones), can they simultaneously hold that gender-specific verses are eternally binding? Or does accepting naskh entail denying the eternal status? If the logical relationship is foreclosure, the readings are mutually exclusive in principle; if coexistence, they are live options held by different authority structures.',
    'If foreclosure, the engine will compute a structured mutual-negation relation: adoption of the progressive reading entails rejection of literalism within any coherent framework. If coexistence, the readings remain live competitors and the constraint is pure contention between authority structures (tangled_rope of hermeneutics). Classification differs accordingly: foreclosure suggests one reading will eventually dominate; coexistence suggests permanent institutional pluralism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_status, conceptual, 'Whether naskh-acceptance logically forecloses literalism within a single Islamic framework or whether readings coexist as competing interpretations.').

omega_variable(
    women_beneficiary_definition_ambiguity,
    'Who are ''women'' as beneficiaries in this reading? Does the benefit accrue to all women Muslim and non-Muslim, women who identify with Islamic jurisprudence, or only women scholars/reformers actively adopting the reading?',
    'Ethnographic research with women in communities where the reading is institutionalized vs. communities resistant to it. Do women subject to gender-specific rules in practice report feeling benefited by the reading''s doctrinal shift? Or is the benefit primarily to women scholars and advocates?',
    'If benefit is concentrated among women scholars/reformers (moderate and powerful seats), the constraint is more extractive from communities than the current classification suggests: it redistributes authority among elites while leaving women in traditionalist communities facing epistemic violence without tangible legal change. If women broadly report benefit (even in communities slow to adopt the reading), the extraction is real but less concentrated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(women_beneficiary_definition_ambiguity, empirical, 'Whether the reading''s benefit to women is concentrated among scholars/reformers or distributed across women in affected communities.').

omega_variable(
    alternative_hermeneutics_suppression,
    'Is the contextual-egalitarian reading (which also supports women''s equality but via maqasid/overarching principles rather than abrogation) actively suppressed by the progressive abrogation framework, or do the two readings coexist as parallel egalitarian approaches?',
    'Institutional analysis: do seminaries and fatwa councils that adopt the progressive reading also exclude contextual approaches? Do scholarly publications that advance abrogation actively delegitimize context-based exegesis? Or are both methods recognized as legitimate paths to gender equity?',
    'If contextual reading is suppressed, the progressive abrogation reading''s extractiveness includes not just invalidating literalism but also establishing a monopoly on acceptable egalitarian hermeneutics—higher extraction from scholars who prefer context-based methodology. If coexistence, the reading is less extractive (egalitarian scholars retain methodological pluralism). Matters for the reading_relations field: influences vs. forecloses on the contextual-egalitarian sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_hermeneutics_suppression, empirical, 'Whether the progressive abrogation reading suppresses the contextual-egalitarian reading or coexists with it as a parallel egalitarian approach.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__progressive_abrogation, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quranic_gender_verses__progressive_abrogation, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(qura_tr_t0, observed).
narrative_ontology:measurement(qura_tr_t5, quranic_gender_verses__progressive_abrogation, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(qura_tr_t5, observed).
narrative_ontology:measurement(qura_tr_t10, quranic_gender_verses__progressive_abrogation, theater_ratio, 10, 0.34).
narrative_ontology:measurement_basis(qura_tr_t10, observed).
narrative_ontology:measurement(qura_tr_t15, quranic_gender_verses__progressive_abrogation, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(qura_tr_t15, observed).
narrative_ontology:measurement(qura_tr_t20, quranic_gender_verses__progressive_abrogation, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(qura_tr_t20, observed).
narrative_ontology:measurement(qura_tr_t30, quranic_gender_verses__progressive_abrogation, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(qura_tr_t30, observed).
narrative_ontology:measurement(qura_tr_t40, quranic_gender_verses__progressive_abrogation, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(qura_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quranic_gender_verses__progressive_abrogation, base_extractiveness, 0, 0.71).
narrative_ontology:measurement_basis(qura_be_t0, observed).
narrative_ontology:measurement(qura_be_t5, quranic_gender_verses__progressive_abrogation, base_extractiveness, 5, 0.75).
narrative_ontology:measurement_basis(qura_be_t5, observed).
narrative_ontology:measurement(qura_be_t10, quranic_gender_verses__progressive_abrogation, base_extractiveness, 10, 0.79).
narrative_ontology:measurement_basis(qura_be_t10, observed).
narrative_ontology:measurement(qura_be_t15, quranic_gender_verses__progressive_abrogation, base_extractiveness, 15, 0.84).
narrative_ontology:measurement_basis(qura_be_t15, observed).
narrative_ontology:measurement(qura_be_t20, quranic_gender_verses__progressive_abrogation, base_extractiveness, 20, 0.88).
narrative_ontology:measurement_basis(qura_be_t20, observed).
narrative_ontology:measurement(qura_be_t30, quranic_gender_verses__progressive_abrogation, base_extractiveness, 30, 0.91).
narrative_ontology:measurement_basis(qura_be_t30, observed).
narrative_ontology:measurement(qura_be_t40, quranic_gender_verses__progressive_abrogation, base_extractiveness, 40, 0.92).
narrative_ontology:measurement_basis(qura_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quranic_gender_verses__progressive_abrogation, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(qura_su_t0, observed).
narrative_ontology:measurement(qura_su_t5, quranic_gender_verses__progressive_abrogation, suppression_requirement, 5, 0.66).
narrative_ontology:measurement_basis(qura_su_t5, observed).
narrative_ontology:measurement(qura_su_t10, quranic_gender_verses__progressive_abrogation, suppression_requirement, 10, 0.7).
narrative_ontology:measurement_basis(qura_su_t10, observed).
narrative_ontology:measurement(qura_su_t15, quranic_gender_verses__progressive_abrogation, suppression_requirement, 15, 0.73).
narrative_ontology:measurement_basis(qura_su_t15, observed).
narrative_ontology:measurement(qura_su_t20, quranic_gender_verses__progressive_abrogation, suppression_requirement, 20, 0.76).
narrative_ontology:measurement_basis(qura_su_t20, observed).
narrative_ontology:measurement(qura_su_t30, quranic_gender_verses__progressive_abrogation, suppression_requirement, 30, 0.77).
narrative_ontology:measurement_basis(qura_su_t30, observed).
narrative_ontology:measurement(qura_su_t40, quranic_gender_verses__progressive_abrogation, suppression_requirement, 40, 0.78).
narrative_ontology:measurement_basis(qura_su_t40, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(qura_grid_01, quranic_gender_verses__progressive_abrogation, accessibility_collapse(class), 0, 0.58).
narrative_ontology:measurement(qura_grid_02, quranic_gender_verses__progressive_abrogation, accessibility_collapse(class), 40, 0.71).
narrative_ontology:measurement(qura_grid_03, quranic_gender_verses__progressive_abrogation, accessibility_collapse(individual), 0, 0.51).
narrative_ontology:measurement(qura_grid_04, quranic_gender_verses__progressive_abrogation, accessibility_collapse(individual), 40, 0.62).
narrative_ontology:measurement(qura_grid_05, quranic_gender_verses__progressive_abrogation, accessibility_collapse(organizational), 0, 0.64).
narrative_ontology:measurement(qura_grid_06, quranic_gender_verses__progressive_abrogation, accessibility_collapse(organizational), 40, 0.78).
narrative_ontology:measurement(qura_grid_07, quranic_gender_verses__progressive_abrogation, accessibility_collapse(structural), 0, 0.55).
narrative_ontology:measurement(qura_grid_08, quranic_gender_verses__progressive_abrogation, accessibility_collapse(structural), 40, 0.72).
narrative_ontology:measurement(qura_grid_09, quranic_gender_verses__progressive_abrogation, resistance(class), 0, 0.68).
narrative_ontology:measurement(qura_grid_10, quranic_gender_verses__progressive_abrogation, resistance(class), 40, 0.71).
narrative_ontology:measurement(qura_grid_11, quranic_gender_verses__progressive_abrogation, resistance(individual), 0, 0.62).
narrative_ontology:measurement(qura_grid_12, quranic_gender_verses__progressive_abrogation, resistance(individual), 40, 0.65).
narrative_ontology:measurement(qura_grid_13, quranic_gender_verses__progressive_abrogation, resistance(organizational), 0, 0.72).
narrative_ontology:measurement(qura_grid_14, quranic_gender_verses__progressive_abrogation, resistance(organizational), 40, 0.73).
narrative_ontology:measurement(qura_grid_15, quranic_gender_verses__progressive_abrogation, resistance(structural), 0, 0.68).
narrative_ontology:measurement(qura_grid_16, quranic_gender_verses__progressive_abrogation, resistance(structural), 40, 0.72).
narrative_ontology:measurement(qura_grid_17, quranic_gender_verses__progressive_abrogation, stakes_inflation(class), 0, 0.62).
narrative_ontology:measurement(qura_grid_18, quranic_gender_verses__progressive_abrogation, stakes_inflation(class), 40, 0.81).
narrative_ontology:measurement(qura_grid_19, quranic_gender_verses__progressive_abrogation, stakes_inflation(individual), 0, 0.48).
narrative_ontology:measurement(qura_grid_20, quranic_gender_verses__progressive_abrogation, stakes_inflation(individual), 40, 0.68).
narrative_ontology:measurement(qura_grid_21, quranic_gender_verses__progressive_abrogation, stakes_inflation(organizational), 0, 0.75).
narrative_ontology:measurement(qura_grid_22, quranic_gender_verses__progressive_abrogation, stakes_inflation(organizational), 40, 0.89).
narrative_ontology:measurement(qura_grid_23, quranic_gender_verses__progressive_abrogation, stakes_inflation(structural), 0, 0.68).
narrative_ontology:measurement(qura_grid_24, quranic_gender_verses__progressive_abrogation, stakes_inflation(structural), 40, 0.86).
narrative_ontology:measurement(qura_grid_25, quranic_gender_verses__progressive_abrogation, suppression(class), 0, 0.58).
narrative_ontology:measurement(qura_grid_26, quranic_gender_verses__progressive_abrogation, suppression(class), 40, 0.76).
narrative_ontology:measurement(qura_grid_27, quranic_gender_verses__progressive_abrogation, suppression(individual), 0, 0.52).
narrative_ontology:measurement(qura_grid_28, quranic_gender_verses__progressive_abrogation, suppression(individual), 40, 0.71).
narrative_ontology:measurement(qura_grid_29, quranic_gender_verses__progressive_abrogation, suppression(organizational), 0, 0.68).
narrative_ontology:measurement(qura_grid_30, quranic_gender_verses__progressive_abrogation, suppression(organizational), 40, 0.81).
narrative_ontology:measurement(qura_grid_31, quranic_gender_verses__progressive_abrogation, suppression(structural), 0, 0.61).
narrative_ontology:measurement(qura_grid_32, quranic_gender_verses__progressive_abrogation, suppression(structural), 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__progressive_abrogation, identity_coordination).
narrative_ontology:boltzmann_floor_override(quranic_gender_verses__progressive_abrogation, 0.12).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, quranic_gender_verses__literal_hierarchical).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, quranic_gender_verses__contextual_egalitarian).

% DUAL FORMULATION NOTE:
% The constraint family 'quranic_gender_verses' decomposes into three structurally distinct stories, each representing a different reading of the same kernel (the Qur'anic gender verses). Each reading instantiates a different constraint with a different ε, beneficiary/victim structure, and institutional position. Progressive abrogation (this story, ε ≈ 0.92): complete normative inversion, high extraction from literalists; literal-hierarchical (ε ≈ 0.15): naturalized constraint, low extraction because it frames gender differentiation as timeless divine law; contextual-egalitarian (ε ≈ 0.68): moderate extraction, uses hermeneutic flexibility to support equity without invoking abrogation. The three readings coexist as live positions in Islamic scholarship; none is currently dominant globally. Links: progressive_abrogation influences both siblings (destabilizes their authority claims) and coexists_with them (they remain institutional live options). Each reading's ε is fixed to its own epistemic framework (what counts as extraction under that reading's lights), not averaged across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quranic_gender_verses__progressive_abrogation, powerful, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
