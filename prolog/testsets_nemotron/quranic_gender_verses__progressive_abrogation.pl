% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__progressive_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: quranic_gender_verses__progressive_abrogation
 *   human_readable: Progressive Abrogation Reading of Quranic Gender Verses
 *   domain: religious/hermeneutic/legal
 *
 * SUMMARY:
 *   This constraint story represents the progressive_abrogation reading of
 *   the contested kernel quranic_gender_verses. The reading holds that later
 *   Qur'anic verses articulating universal human dignity (notably 49:13)
 *   abrogate earlier gender-specific legal rules (4:11 inheritance, 2:282
 *   testimony, 4:34 marital authority) via the principle of naskh. This is a
 *   complete normative reversal: the constraint operates as a tangled_rope
 *   because it coordinates a genuine egalitarian hermeneutic (beneficiaries:
 *   women seeking legal parity, progressive scholars, reformist communities)
 *   while extracting from traditional authority structures (victims:
 *   literalist scholars, communities whose identity is bound to literal
 *   reading). The extraction is very high (0.85) because the reading
 *   delegitimizes the entire traditional fiqh edifice on gender; suppression
 *   is high (0.78) because maintaining this reading within traditional
 *   institutions requires active enforcement against expulsion, denunciation,
 *   and loss of scholarly authority. Theater ratio (0.42) reflects that part
 *   of the constraint's operation is performative — the abrogation argument
 *   is deployed in contexts where institutional power prevents its actual
 *   implementation.
 *
 * KEY AGENTS:
 *   - women_seeking_legal_parity: Primary beneficiary (moderate/identity_locked) — gains full legal standing but faces communal rupture
 *   - progressive_quranic_scholars: Agenda-setter/beneficiary (organized/identity_locked) — constructs and defends the reading; bears high exit costs within traditional institutions
 *   - traditional_ulama_adherents: Primary victim (institutional/identity_locked) — loses hermeneutic monopoly, scholarly authority, communal coherence
 *   - literalist_scholarly_networks: Victim (organized/identity_locked) — constraint delegitimizes their core interpretive framework
 *   - communities_bound_to_literal_identity: Victim (organized/trapped) — identity fused with literal reading; exit is existential
 *   - reformist_muslim_communities: Beneficiary (organized/constrained) — gains theological legitimacy for egalitarian practice
 *   - contextual_egalitarian_scholars: Observer/secondary_actor (analytical/mobile) — holds adjacent but distinct reading
 *   - state_legal_systems: Inter-institutional actor (institutional/arbitrage) — selectively adopts or suppresses readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__progressive_abrogation, 0.85).
domain_priors:suppression_score(quranic_gender_verses__progressive_abrogation, 0.78).
domain_priors:theater_ratio(quranic_gender_verses__progressive_abrogation, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, extractiveness, 0.85).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__progressive_abrogation, tangled_rope).
narrative_ontology:human_readable(quranic_gender_verses__progressive_abrogation, "Progressive Abrogation Reading of Quranic Gender Verses").
narrative_ontology:topic_domain(quranic_gender_verses__progressive_abrogation, "religious/hermeneutic/legal").

domain_priors:requires_active_enforcement(quranic_gender_verses__progressive_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__progressive_abrogation, '6be96e82-37ff-4701-8734-0009cc2a26bd').
narrative_ontology:cs_kernel_codification('6be96e82-37ff-4701-8734-0009cc2a26bd', fixed_text).
narrative_ontology:cs_authority_grounding('6be96e82-37ff-4701-8734-0009cc2a26bd', lineage).
narrative_ontology:cs_interpretation_layer_present('6be96e82-37ff-4701-8734-0009cc2a26bd').
narrative_ontology:cs_reading_relation('6be96e82-37ff-4701-8734-0009cc2a26bd', quranic_gender_verses__literal_hierarchical, forecloses).
narrative_ontology:cs_reading_relation('6be96e82-37ff-4701-8734-0009cc2a26bd', quranic_gender_verses__contextual_egalitarian, influences).
narrative_ontology:cs_axiom('6be96e82-37ff-4701-8734-0009cc2a26bd', foundational, later_egalitarian_verses_abrogate_earlier_gender_rules).
narrative_ontology:cs_axiom_status(later_egalitarian_verses_abrogate_earlier_gender_rules, holdable).
narrative_ontology:cs_axiom_grounding('6be96e82-37ff-4701-8734-0009cc2a26bd', later_egalitarian_verses_abrogate_earlier_gender_rules, deontological).
narrative_ontology:cs_axiom('6be96e82-37ff-4701-8734-0009cc2a26bd', foundational, naskh_operates_as_universal_hermeneutic_principle).
narrative_ontology:cs_axiom_status(naskh_operates_as_universal_hermeneutic_principle, holdable).
narrative_ontology:cs_axiom_grounding('6be96e82-37ff-4701-8734-0009cc2a26bd', naskh_operates_as_universal_hermeneutic_principle, conventional).
narrative_ontology:cs_reference_frame('6be96e82-37ff-4701-8734-0009cc2a26bd', classical_fiqh_gender_settled).
narrative_ontology:cs_drift_state('6be96e82-37ff-4701-8734-0009cc2a26bd', contemporary_egalitarian_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6be96e82-37ff-4701-8734-0009cc2a26bd', '2026-08-15T14:32:17Z').
narrative_ontology:cs_kernel_id(quranic_gender_verses__progressive_abrogation, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, women_seeking_legal_parity).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, progressive_quranic_scholars).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, reformist_muslim_communities).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, traditional_ulama_adherents).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, literalist_scholarly_networks).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, communities_bound_to_literal_identity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, state_legal_systems).
narrative_ontology:constraint_vindicates(quranic_gender_verses__progressive_abrogation, universal_human_dignity_quran_49_13).
narrative_ontology:constraint_vindicates(quranic_gender_verses__progressive_abrogation, naskh_as_native_quranic_principle).
narrative_ontology:constraint_vindicates(quranic_gender_verses__progressive_abrogation, maqasid_based_egalitarian_hermeneutic).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Women who seek full legal parity in inheritance, testimony, marriage, and divorce under Islamic law. They gain a hermeneutic that validates their equality as Qur'anic intent, but adopting it often means rupture with family, community, and religious identity structures that treat the literal reading as constitutive. Exit from the literal framework is identity_locked — leaving it dissolves the communal and familial bonds that constitute their social world.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, women_seeking_legal_parity, beneficiary,
    moderate, biographical, identity_locked, global).

% Scholars who develop, teach, and defend the progressive_abrogation reading in academic, seminary, and public spheres. They set the hermeneutic agenda for this reading but face severe institutional retaliation within traditional structures: denial of ijaza, exclusion from scholarly councils, fatwas of deviation, loss of mosque and university positions. Their scholarly identity is fused with this reading — exit means abandoning a life's intellectual project and the community that recognizes it.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, progressive_quranic_scholars, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__progressive_abrogation, progressive_quranic_scholars, beneficiary).

% Traditional scholars and institutions whose authority rests on the literal_hierarchical reading as settled fiqh. The progressive_abrogation reading extracts their hermeneutic monopoly, scholarly credibility, and institutional legitimacy. They cannot exit the constraint's scope because their entire epistemic and social world is organized around the literal reading's authority — identity_locked at the institutional level. Their resistance is structural: control of seminaries, fatwa bodies, judicial appointments, and communal trust.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, traditional_ulama_adherents, payer,
    institutional, generational, identity_locked, global).

% Transnational networks of scholars (e.g., Salafi, Deobandi, traditional madhhab institutions) that treat the gender verses as timeless divine law. The progressive_abrogation reading delegitimizes their core interpretive framework. They are organized and powerful but identity_locked — their institutional identity cannot survive the reading's acceptance. They enforce suppression through fatwa networks, educational curricula, and communal discipline.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, literalist_scholarly_networks, payer,
    organized, generational, identity_locked, global).

% Communities (villages, diaspora enclaves, congregations) where the literal reading is not just believed but constitutes collective identity — marriage practices, inheritance customs, gender roles are woven into communal coherence. The progressive_abrogation reading threatens existential rupture. They are trapped: no exit without community dissolution, no voice in scholarly discourse, and state law often enforces the literal reading they depend on.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, communities_bound_to_literal_identity, payer,
    organized, generational, trapped, local).

% Muslim communities (e.g., progressive mosques, feminist Muslim organizations, reformist movements) that adopt egalitarian practice but need theological legitimacy. This reading provides it. They are constrained — they can exit to secular or other religious frameworks, but at cost of Islamic authenticity. They benefit from the constraint's coordination function without bearing its full scholarly enforcement costs.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, reformist_muslim_communities, beneficiary,
    organized, biographical, constrained, global).

% Scholars who hold the contextual_egalitarian reading (verses as historical steps requiring maqasid reinterpretation). They see the progressive_abrogation reading as a sibling — structurally adjacent but distinct in mechanism (naskh vs. maqasid). They are analytical observers with mobile exit: they can engage or disengage from the abrogation debate without identity rupture. Their presence creates competitive pressure on the progressive_abrogation reading's claim to be the only egalitarian path.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, contextual_egalitarian_scholars, observer,
    analytical, generational, mobile, global).

% Nation-states with Muslim personal status laws (family law, inheritance) that selectively adopt or suppress readings to serve governance goals. They can arbitrage between readings: cite progressive_abrogation to justify reform, suppress it to maintain traditional order, or promote contextual_egalitarian as a compromise. They are not bound by any reading's internal logic — they extract legitimacy from whichever reading serves policy.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, state_legal_systems, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__progressive_abrogation, state_legal_systems, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quranic_gender_verses__progressive_abrogation, progressive_quranic_scholars).
narrative_ontology:fixing_cost_class(quranic_gender_verses__progressive_abrogation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a native Qur'anic hermeneutic (naskh) that reconciles revelation with universal human dignity, enabling egalitarian Islamic law without abandoning scriptural authority. Solves the coordination problem of how Muslim communities can affirm both Qur'anic authority and gender justice simultaneously.
% TRANSFER_FUNCTION: Moves hermeneutic authority, scholarly legitimacy, and legal validity from traditional ulama and literalist networks to progressive scholars and reformist communities. Women gain legal parity; traditional structures lose their monopoly on Qur'anic interpretation. The transfer is total: the abrogation logic leaves no room for the literal reading as a live option.
% ABSENT_VOICES: Women in literalist communities who cannot articulate dissent without communal exile; progressive scholars in traditional seminaries who cannot publish or teach this reading; minority Muslim communities in non-Muslim-majority states where state law imposes either secular family law or literalist personal status law, giving them no access to this hermeneutic.
% DISAPPEARANCE_RATIONALE: If the progressive_abrogation reading vanished overnight, the primary native-Qur'anic path to full gender parity would disappear. Reformist communities would lose theological legitimacy; progressive scholars would lose their core hermeneutic; women seeking legal parity within Islam would be forced toward secular frameworks or the contextual_egalitarian reading (which operates differently). The literal_hierarchical reading would regain unchallenged hermeneutic dominance in traditional spaces. The world of Islamic legal discourse would rearrange significantly.
% FOUNDING_PROBLEM: The Qur'an contains gender-differentiated rules (inheritance, testimony, marital authority) that contradict its own universal dignity principles (49:13). Classical fiqh treated this as settled; modernity made the contradiction a crisis of legitimacy for Muslim communities committed to both revelation and justice. The progressive_abrogation reading was built to resolve this by declaring the egalitarian verses abrogative.
% FOUNDING_PROBLEM_CORROBORATION: The crisis of legitimacy is attested by multiple independent sources: classical jurists who noted the tension (e.g., al-Shatibi on maqasid), colonial-era reformers (Muhammad Abduh, Qasim Amin) who identified the contradiction as a civilizational challenge, contemporary Muslim feminists (Amina Wadud, Asma Barlas) who document the lived contradiction, and international human rights bodies that cite gender-differentiated personal status law as a compliance gap. No single beneficiary group monopolizes this attestation.
narrative_ontology:disappearance_verdict(quranic_gender_verses__progressive_abrogation, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__progressive_abrogation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__progressive_abrogation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(quranic_gender_verses__progressive_abrogation, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__progressive_abrogation, 0.85, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   The claimed_type is tangled_rope because the reading simultaneously coordinates a genuine egalitarian hermeneutic (universal dignity as Qur'anic telos) and extracts asymmetrically from traditional authority (complete delegitimization of centuries of fiqh). The very high extractiveness (0.85) reflects the total normative reversal — women gain full legal parity, but traditional structures lose all hermeneutic ground. Suppression (0.78) is high because the reading's persistence in traditional spaces requires active enforcement: scholars adopting it face expulsion, fatwas of deviation, loss of institutional positions. Theater ratio (0.42) captures that much abrogation discourse is performative in contexts where state law or social power prevents actual legal change. Accessibility collapse (0.68) is moderate-high: once the abrogation logic is accepted, the literal reading becomes structurally inaccessible as a live option, but the contextual_egalitarian reading remains a viable alternative path. Resistance (0.75) is high from institutional traditionalists who control credentialing, mosques, and educational pipelines.
 *
 * PERSPECTIVAL GAP:
 *   From the progressive scholar's seat, the constraint is a rope with high coordination value — it solves the problem of reconciling revelation with justice. From the traditional ulama's seat, it is a snare — the abrogation argument is cover for dismantling authority. From the literalist community's seat, it is an existential threat — identity destruction via hermeneutic displacement. From the state's seat, it is a tool — selectively deployed to legitimize reform or suppressed to maintain social order. The engine computes these divergences from the structural data: beneficiaries get low d (subsidy), victims get high d (extraction), identity_locked exit amplifies extraction for victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (women_seeking_legal_parity, progressive_quranic_scholars, reformist_muslim_communities) receive the constraint's coordination subsidy: a hermeneutic that grants full legal parity. Their directionality d is low (~0.15-0.25) — they are net recipients. Victims (traditional_ulama_adherents, literalist_scholarly_networks, communities_bound_to_literal_identity) bear the extraction: loss of hermeneutic monopoly, scholarly authority, communal identity. Their d is high (~0.85-0.95) — identity_locked exit means they cannot leave the constraint's scope without existential loss. Progressive scholars are dual-positioned: they set the agenda (agenda_setter) but also bear high personal costs within traditional institutions (identity_locked exit, institutional retaliation). The contextual_egalitarian reading creates a pressure gradient: it offers a lower-extraction path to similar outcomes, pulling some would-be progressive_abrogation adopters toward coexistence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling revelation with justice for women) is live and contested. The progressive_abrogation reading resolves it via complete normative reversal, but this creates new extraction: traditional authority is comprehensively delegitimized. The mandatrophy risk is that the reading becomes a snare if the coordination function (egalitarian hermeneutic) is captured by actors who use it to destroy traditional communities rather than build just ones. The contextual_egalitarian reading is the lower-extraction alternative that keeps the founding problem live without the total reversal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the progressive_abrogation reading a legitimate instantiation of the quranic_gender_verses kernel, or does it constitute a distinct constraint that forecloses on the kernel''s other readings?',
    'Comparative structural analysis of the three declared readings (progressive_abrogation, literal_hierarchical, contextual_egalitarian) against the ε-invariance principle: if each reading produces a distinct ε with distinct beneficiary/victim structures, they are separate constraints linked by network.affects_constraints, not variants of one constraint.',
    'If progressive_abrogation is a separate constraint, its very high extractiveness (0.85) is an authored fact about this reading''s operation, not a claim about the kernel as a whole. The kernel itself has no ε; only readings do.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether the kernel-reading decomposition is structurally valid or whether the three readings collapse into fewer constraints.').

omega_variable(
    naskh_scope_ambiguity,
    'Does the principle of naskh (abrogation) operate as a universal hermeneutic rule that definitively supersedes earlier gender-specific verses, or is its scope contested even within the progressive_abrogation reading?',
    'Examination of classical and contemporary tafsir literature on whether 49:13 and related egalitarian verses are categorized as nasikh (abrogating) over 4:11, 2:282, 4:34, and whether any traditional scholar accepts this categorization.',
    'If naskh scope is contested within the reading, the constraint''s suppression (0.78) partly reflects intra-reading coercion, not only inter-reading conflict. This changes the directionality logic for progressive scholars.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naskh_scope_ambiguity, empirical, 'Whether the abrogation mechanism itself is settled within this reading.').

omega_variable(
    epistemic_violence_risk,
    'Does adopting this reading within traditional institutions inflict epistemic violence on communities whose identity is bound to the literal reading, and is that violence an inherent feature of the constraint or a contingent effect of power asymmetry?',
    'Longitudinal study of scholars and communities who transition from literal_hierarchical to progressive_abrogation frameworks: measure identity disruption, communal rupture, and whether alternative transitional pathways (e.g., contextual_egalitarian) mitigate the violence.',
    'If epistemic violence is inherent, the constraint''s extraction includes identity destruction as a structural cost borne by literalist communities — raising suppression and altering the victim structure. If contingent, the violence is a power artifact, not a constraint property.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_violence_risk, preference, 'Whether identity destruction is a structural feature of the normative reversal or a contingent power effect.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__progressive_abrogation, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_tr_t0, quranic_gender_verses__progressive_abrogation, theater_ratio, 0, 0.15).
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_tr_t50, quranic_gender_verses__progressive_abrogation, theater_ratio, 50, 0.22).
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_tr_t100, quranic_gender_verses__progressive_abrogation, theater_ratio, 100, 0.28).
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_tr_t150, quranic_gender_verses__progressive_abrogation, theater_ratio, 150, 0.35).
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_tr_t200, quranic_gender_verses__progressive_abrogation, theater_ratio, 200, 0.4).
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_tr_t250, quranic_gender_verses__progressive_abrogation, theater_ratio, 250, 0.42).
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_tr_t300, quranic_gender_verses__progressive_abrogation, theater_ratio, 300, 0.42).

% Extraction over time
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_be_t0, quranic_gender_verses__progressive_abrogation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_be_t50, quranic_gender_verses__progressive_abrogation, base_extractiveness, 50, 0.45).
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_be_t100, quranic_gender_verses__progressive_abrogation, base_extractiveness, 100, 0.55).
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_be_t150, quranic_gender_verses__progressive_abrogation, base_extractiveness, 150, 0.65).
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_be_t200, quranic_gender_verses__progressive_abrogation, base_extractiveness, 200, 0.75).
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_be_t250, quranic_gender_verses__progressive_abrogation, base_extractiveness, 250, 0.82).
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_be_t300, quranic_gender_verses__progressive_abrogation, base_extractiveness, 300, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_su_t0, quranic_gender_verses__progressive_abrogation, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_su_t50, quranic_gender_verses__progressive_abrogation, suppression_requirement, 50, 0.5).
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_su_t100, quranic_gender_verses__progressive_abrogation, suppression_requirement, 100, 0.6).
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_su_t150, quranic_gender_verses__progressive_abrogation, suppression_requirement, 150, 0.68).
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_su_t200, quranic_gender_verses__progressive_abrogation, suppression_requirement, 200, 0.73).
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_su_t250, quranic_gender_verses__progressive_abrogation, suppression_requirement, 250, 0.76).
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_su_t300, quranic_gender_verses__progressive_abrogation, suppression_requirement, 300, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__progressive_abrogation, identity_coordination).
narrative_ontology:boltzmann_floor_override(quranic_gender_verses__progressive_abrogation, 0.08).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, quranic_gender_verses__literal_hierarchical).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, quranic_gender_verses__contextual_egalitarian).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, state_personal_status_law_reform).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, muslim_family_law_codification).

% DUAL FORMULATION NOTE:
% The quranic_gender_verses kernel decomposes into three constraint stories with distinct ε values and beneficiary/victim structures. This reading (progressive_abrogation) has the highest extractiveness (0.85) because it forecloses the literal reading entirely via naskh. The literal_hierarchical reading has lower extractiveness for its adherents but high suppression of alternatives. The contextual_egalitarian reading has moderate extractiveness and functions as a transitional scaffold. All three are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quranic_gender_verses__progressive_abrogation, organized, 0.3).
constraint_indexing:directionality_override(quranic_gender_verses__progressive_abrogation, institutional, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
