% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__reformist_ijtihad
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Reformist Ijtihad: Contextual Reasoning Mandated Over Literalist Hadith Application
 *   domain: religious/legal/authority
 *
 * SUMMARY:
 *   The reformist ijtihad reading of the Quran-hadith substrate mandates
 *   contextual reasoning when classical rulings conflict with contemporary
 *   ethics, human rights norms, or maslaha (public interest). It prioritizes
 *   the Quran's ethical trajectory — the arc from the Meccan emphasis on
 *   human dignity, justice, and mercy through the Medinan legislative
 *   instantiations — over literalist application of hadith that reflect
 *   7th-century Arabian social norms. This reading operates as a Tangled
 *   Rope: it genuinely coordinates a solution for Muslims who refuse the
 *   false choice between tradition and conscience, while simultaneously
 *   extracting interpretive authority from traditional institutions whose
 *   legitimacy depends on monopoly control of 'orthodoxy.' The constraint
 *   requires active enforcement (institutional recognition, academic
 *   legitimacy, state tolerance) to persist against traditionalist
 *   counter-mobilization. In jurisdictions where reformist readings have
 *   state backing (e.g., Tunisia post-2011, Indonesia's moderate mainstream),
 *   ε is lower (~0.35) and the coordination function dominates. Where they
 *   face state hostility (e.g., Saudi Arabia, Iran, Egypt post-2013), ε rises
 *   toward 0.50 as the constraint becomes a site of contestation rather than
 *   settled practice.
 *
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
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__reformist_ijtihad, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__reformist_ijtihad, "Reformist Ijtihad: Contextual Reasoning Mandated Over Literalist Hadith Application").
narrative_ontology:topic_domain(quran_hadith_substrate__reformist_ijtihad, "religious/legal/authority").

domain_priors:requires_active_enforcement(quran_hadith_substrate__reformist_ijtihad).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__reformist_ijtihad, '7a3ee525-54e8-40dd-a2a6-a3923667e711').
narrative_ontology:cs_kernel_codification('7a3ee525-54e8-40dd-a2a6-a3923667e711', fixed_text).
narrative_ontology:cs_authority_grounding('7a3ee525-54e8-40dd-a2a6-a3923667e711', lineage).
narrative_ontology:cs_interpretation_layer_present('7a3ee525-54e8-40dd-a2a6-a3923667e711').
narrative_ontology:cs_reading_relation('7a3ee525-54e8-40dd-a2a6-a3923667e711', quran_hadith_substrate__traditionalist_taqlid, coexists_with).
narrative_ontology:cs_reading_relation('7a3ee525-54e8-40dd-a2a6-a3923667e711', quran_hadith_substrate__state_hybrid, influences).
narrative_ontology:cs_axiom('7a3ee525-54e8-40dd-a2a6-a3923667e711', foundational, quran_ethical_trajectory_primacy_over_hadith_literalism).
narrative_ontology:cs_axiom_status(quran_ethical_trajectory_primacy_over_hadith_literalism, holdable).
narrative_ontology:cs_axiom_grounding('7a3ee525-54e8-40dd-a2a6-a3923667e711', quran_ethical_trajectory_primacy_over_hadith_literalism, deontological).
narrative_ontology:cs_axiom('7a3ee525-54e8-40dd-a2a6-a3923667e711', foundational, contextual_ijtihad_obligatory_when_classical_fiqh_contradicts_haqq).
narrative_ontology:cs_axiom_status(contextual_ijtihad_obligatory_when_classical_fiqh_contradicts_haqq, holdable).
narrative_ontology:cs_axiom_grounding('7a3ee525-54e8-40dd-a2a6-a3923667e711', contextual_ijtihad_obligatory_when_classical_fiqh_contradicts_haqq, instrumental).
narrative_ontology:cs_axiom('7a3ee525-54e8-40dd-a2a6-a3923667e711', secondary, human_dignity_as_maqasid_central).
narrative_ontology:cs_axiom_status(human_dignity_as_maqasid_central, holdable).
narrative_ontology:cs_axiom_grounding('7a3ee525-54e8-40dd-a2a6-a3923667e711', human_dignity_as_maqasid_central, deontological).
narrative_ontology:cs_reference_frame('7a3ee525-54e8-40dd-a2a6-a3923667e711', classical_madhhab_consensus).
narrative_ontology:cs_drift_state('7a3ee525-54e8-40dd-a2a6-a3923667e711', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7a3ee525-54e8-40dd-a2a6-a3923667e711', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, progressive_muslims).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, women_in_muslim_contexts).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, lgbtq_muslims).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, religious_minorities_under_islamic_law).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, reformist_scholars).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, traditional_ulema_institutions).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, classical_madhhab_authorities).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, state_religious_establishments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce and defend the reformist interpretive framework through academic work, fatwas, and institutional positions. Their authority derives from methodological coherence and ethical resonance rather than institutional monopoly. Exit means returning to traditionalist frameworks or leaving religious discourse entirely.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, reformist_scholars, agenda_setter,
    organized, generational, constrained, global).

% Gain religious legitimacy for ethical commitments (gender equality, LGBTQ+ inclusion, interfaith pluralism) that would be excluded under traditionalist readings. Their constraint is that this legitimacy remains contested and institutionally fragile — they remain vulnerable to takfiri accusations and social ostracism.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, progressive_muslims, beneficiary,
    moderate, biographical, constrained, global).

% Directly affected by reformist readings on marriage, divorce, inheritance, testimony, and public participation. The reformist reading opens interpretive space that traditionalist readings foreclose. Exit from the constraint is identity-locked — their self-understanding as Muslim women is constituted through this relationship; leaving the tradition entirely is experienced as existential loss, not mere preference change.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, women_in_muslim_contexts, beneficiary,
    powerless, biographical, identity_locked, global).

% The reformist reading is the only interpretive framework within the tradition that affords them religious legitimacy without requiring self-negation. Traditionalist readings uniformly condemn; state_hybrid readings offer no stable protection. Exit is identity-locked — the constraint IS the possibility of remaining Muslim; leaving the tradition is the only alternative, experienced as spiritual amputation.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, lgbtq_muslims, beneficiary,
    powerless, biographical, identity_locked, global).

% Benefit from reformist readings that prioritize Quranic pluralism verses (e.g., 2:256, 5:48) over classical dhimmi frameworks. Their situation varies by jurisdiction — where reformist readings have state backing they gain legal protections; where traditionalist authority prevails they remain subordinate. Exit options are constrained by geography and citizenship.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, religious_minorities_under_islamic_law, beneficiary,
    powerless, biographical, constrained, regional).

% Lose interpretive monopoly and the material/symbolic capital it commands (endowments, state appointments, educational control, definitional authority over 'orthodoxy'). They experience the reformist reading as an existential threat to their institutional rationale. Their exit is constrained — they cannot abandon the tradition without dissolving their institutional identity, but they can and do mount counter-mobilization (fatwa councils, state lobbying, educational curricula control).
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, traditional_ulema_institutions, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__reformist_ijtihad, traditional_ulema_institutions, agenda_setter).

% The four Sunni madhhabs and Twelver Shia usuli schools lose authority when their specific rulings are subjected to contextual override. Their coherence depends on the claim that the madhhab system IS the reliable transmission of revelation. Reformist ijtihad treats madhhab rulings as historically contingent, not binding. Exit is constrained — the madhhab structure is their institutional form; abandoning it means ceasing to exist as that institution.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, classical_madhhab_authorities, payer,
    organized, generational, constrained, global).

% State-appointed muftis, councils, and ministries of religious affairs lose control over the official interpretive pipeline when reformist readings gain traction. They experience extraction of their gatekeeping function. However, their exit is mobile — states can and do pivot between traditionalist, reformist, and state_hybrid readings as political expedience demands. They are not identity-locked to any single reading.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, state_religious_establishments, payer,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__reformist_ijtihad, state_religious_establishments, agenda_setter).

% Monitor and engage with reformist readings as potential allies for human rights advocacy within Muslim-majority contexts. They do not bear the constraint's costs or collect its benefits directly, but their strategic calculations are shaped by whether the reformist reading is viable. Their seat is analytical — they assess the constraint's operational reality from outside the tradition.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, secular_human_rights_actors, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a methodological framework for Muslims to reconcile their religious commitment with contemporary ethical convictions (human rights, gender equality, pluralism) without abandoning the tradition — solving the coordination problem of 'how to remain authentically Muslim while affirming universal ethical norms.'
% TRANSFER_FUNCTION: Transfers interpretive authority from classical madhhab institutions and state religious establishments to reformist scholars and the communities they empower (women, LGBTQ+, minorities). The transfer moves definitional control over 'what Islam requires' from gatekeepers of classical fiqh to practitioners of contextual maqasid-based reasoning.
% ABSENT_VOICES: Ex-Muslims who left the tradition because reformist readings were unavailable or unconvincing; conservative lay Muslims who experience reformist readings as capitulation to Western values but lack institutional platforms; victims of reformist overreach where contextual reasoning is used to legitimize state authoritarianism (e.g., 'maslaha' invoked to suspend rights).
% DISAPPEARANCE_RATIONALE: If the reformist reading vanished overnight, millions of Muslims who currently find ethical coherence within the tradition would face a forced choice: adopt traditionalist readings that condemn their convictions, or leave Islam. The institutional ecology of Muslim-minority communities in the West, progressive Islamic organizations, and reformist seminaries would collapse or radically transform. The constraint's disappearance rearranges the lived religious landscape.
% FOUNDING_PROBLEM: Classical fiqh developed in historical contexts (7th-10th centuries) that institutionalized patriarchal, hierarchical, and exclusivist norms (gender inequality, slavery, dhimmi status, hudud punishments, apostasy laws). The founding problem: how can the tradition remain ethically viable when its classical layer contradicts the Quran's own ethical trajectory (justice, mercy, human dignity) and contemporary moral consciousness?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the Quranic text itself (e.g., progressive revelation on slavery, women's rights, religious pluralism) and by the historical fact that classical jurists ALREADY used contextual tools (istihsan, maslaha, sadd al-dhara'i) to soften harsh literalist outcomes — the reformist reading extends a pre-existing methodological impulse. Corroboration from outside beneficiaries: Western scholars of Islamic law (e.g., Wael Hallaq, Mohammad Fadel, Khaled Abou El Fadl) document the classical tradition's own internal flexibility; human rights organizations (e.g., Musawah, Karamah) ground advocacy in the same ethical trajectory.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__reformist_ijtihad, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__reformist_ijtihad, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__reformist_ijtihad, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(quran_hadith_substrate__reformist_ijtihad, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__reformist_ijtihad, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.42) reflects the moderate but real transfer of authority from traditional gatekeepers to reformist actors — the constraint is not pure coordination (beneficiaries gain without equivalent institutional cost) nor pure extraction (traditional institutions are not merely victimized; they retain massive structural power). Suppression (0.38) is significant but not total: traditionalist institutions actively suppress reformist voices (fatwas of deviation, state censorship, academic blacklisting), but alternatives persist and circulate. Theater ratio (0.28) captures the performative dimension: some reformist discourse is calibrated for Western funding/legitimacy rather than internal coherence, and some traditionalist 'defense of tradition' is political theater for state patronage. Accessibility collapse (0.35) is low — the reformist reading is one live option among several; the classical texts remain accessible and the traditionalist reading remains dominant in most institutional settings. Resistance (0.55) is high — the constraint faces organized, well-resourced opposition from traditionalist institutions and state religious establishments.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (traditional_ulema_institutions, classical_madhhab_authorities) experience the constraint as active extraction of their interpretive monopoly — a loss they fight through fatwa councils, state lobbying, and educational control. The beneficiary seats (women_in_muslim_contexts, lgbtq_muslims) experience it as the condition of their religious survival — not a 'benefit' in the instrumental sense but the only framework where their self-understanding is not heresy. The agenda_setter seats (reformist_scholars, state_religious_establishments) experience it as a methodological commitment or a political tool respectively. The engine computes this divergence from the structural data — the claimed_type 'tangled_rope' reflects the genuine coordination function for the identity-locked beneficiaries AND the asymmetric extraction from institutional authorities.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist scholars (agenda_setter, organized power, constrained exit) sit near the beneficiary end of directionality (d ~0.25) — they gain authority and platform from the constraint's operation, though their exit is constrained by professional identity. Progressive Muslims, women, LGBTQ+, and religious minorities (beneficiaries, powerless-to-moderate power, identity_locked-to-constrained exit) sit at varying points: identity_locked beneficiaries (women, LGBTQ+) experience the constraint as subsidy (d ~0.15) — it is the condition of their religious existence — but their powerlessness means they cannot defend it. Traditional ulema and madhhab authorities (victims, institutional/organized power, constrained exit) sit near the target end (d ~0.85) — they bear the authority transfer, but their institutional power lets them mount counter-extraction. State religious establishments (victim/agenda_setter, institutional power, mobile exit) are the most fluid — they can pivot readings as political winds shift, so their effective extraction varies by regime.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (classical fiqh's ethical obsolescence) remains LIVE — the gap between classical rulings and contemporary ethics has widened, not closed. The constraint is NOT a piton: its coordination function is actively needed by identity-locked beneficiaries, and its extraction from traditional institutions is contested, not atrophied. However, the rising theater_ratio (0.12→0.28) and the state_hybrid reading's opportunistic adoption of reformist language for authoritarian ends suggest a mandatrophy risk: the constraint could degrade into a scaffold (if state actors co-opt it for controlled reform) or a snare (if reformist rhetoric becomes cover for new forms of exclusion). The mandate has NOT outlived its function — the ethical trajectory it serves is still being contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reformist_capture_by_state,
    'To what extent has the reformist reading been captured by state actors who deploy ''contextual ijtihad'' and ''maslaha'' rhetoric to legitimize authoritarian policies (e.g., Egypt''s Al-Azhar under Sisi, UAE''s ''moderate Islam'' branding)?',
    'Track institutional funding, appointment patterns, and fatwa outputs of reformist scholars in state-backed institutions vs. independent reformist spaces; measure divergence in positions on political dissent vs. social reform.',
    'If capture is extensive, the constraint''s extraction profile shifts: the primary extractor becomes the state using reformist language, not traditional institutions losing authority. The coordination function for identity-locked beneficiaries becomes instrumentalized. Classification could shift from tangled_rope toward snare (state extraction) or scaffold (state-managed transition).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformist_capture_by_state, empirical, 'Whether state co-optation has inverted the constraint''s beneficiary structure').

omega_variable(
    identity_locked_suppression_mechanism,
    'For women_in_muslim_contexts and lgbtq_muslims, is the suppression they experience under traditionalist readings primarily structural (legal penalties, social enforcement) or internalized (theological self-condemnation, epistemic closure), and does the reformist reading dissolve both or only the structural layer?',
    'Longitudinal qualitative studies of individuals who transition from traditionalist to reformist frameworks: measure persistence of internalized suppression (shame, fear, self-surveillance) after structural constraints are removed.',
    'If suppression is substantially internalized and the reformist reading only addresses structural suppression, the constraint''s effective suppression for identity-locked beneficiaries is higher than the structural measure suggests — they carry the suppression with them. This would raise the ''true'' ε for these seats and could shift seat-level classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_suppression_mechanism, empirical, 'Structural vs. internalized suppression for identity-locked beneficiaries').

omega_variable(
    traditionalist_counter_mobilization_effectiveness,
    'How effectively do traditionalist institutions convert their structural power (state appointments, endowments, educational pipelines, transnational networks) into durable suppression of the reformist reading, vs. merely slowing its diffusion?',
    'Comparative analysis of jurisdictions where traditionalist institutions have state backing (Saudi Arabia, Iran, Pakistan) vs. where they operate without state monopoly (Western diaspora, Indonesia, Senegal) — measure reformist institutional survival rates, generational transmission, and scholarly output.',
    'If traditionalist counter-mobilization is highly effective, the reformist reading''s ε rises (more resources needed to maintain the coordination function) and its claimed_type may shift toward snare (if coordination becomes aspirational) or piton (if it persists only theatrically). If ineffective, ε falls and the reading consolidates as rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(traditionalist_counter_mobilization_effectiveness, empirical, 'Whether traditionalist structural power translates into durable suppression').

omega_variable(
    kernel_framing_underdetermination,
    'Does the quran_hadith_substrate kernel admit a SINGLE authoritative reading (as traditionalist_taqlid claims) or is it inherently polyvalent (as reformist_ijtihad and state_hybrid demonstrate)? The framing itself is contested — different readings disagree on what the kernel IS.',
    'This is a conceptual omega — resolution requires meta-theoretical adjudication about the nature of scriptural authority, not empirical data. The engine can track which framing each reading''s axioms commit to.',
    'If the kernel is inherently polyvalent, all three readings are legitimate coordinate positions (coexists_with relations hold). If the kernel admits one authoritative reading, two readings are structural errors — forecloses relations would obtain. This determines whether the constraint family is a genuine pluralistic field or a contested monopoly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the shared substrate kernel is mono-valent or polyvalent by nature').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__reformist_ijtihad, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qhr_rij_tr_t1970, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(qhr_rij_tr_t1985, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(qhr_rij_tr_t2000, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(qhr_rij_tr_t2010, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 2010, 0.24).
narrative_ontology:measurement(qhr_rij_tr_t2015, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 2015, 0.26).
narrative_ontology:measurement(qhr_rij_tr_t2020, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 2020, 0.27).
narrative_ontology:measurement(qhr_rij_tr_t2025, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(qhr_rij_be_t1970, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 1970, 0.18).
narrative_ontology:measurement(qhr_rij_be_t1985, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 1985, 0.25).
narrative_ontology:measurement(qhr_rij_be_t2000, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 2000, 0.33).
narrative_ontology:measurement(qhr_rij_be_t2010, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 2010, 0.38).
narrative_ontology:measurement(qhr_rij_be_t2015, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement(qhr_rij_be_t2020, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 2020, 0.41).
narrative_ontology:measurement(qhr_rij_be_t2025, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(qhr_rij_su_t1970, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 1970, 0.22).
narrative_ontology:measurement(qhr_rij_su_t1985, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 1985, 0.28).
narrative_ontology:measurement(qhr_rij_su_t2000, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 2000, 0.32).
narrative_ontology:measurement(qhr_rij_su_t2010, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 2010, 0.35).
narrative_ontology:measurement(qhr_rij_su_t2015, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 2015, 0.37).
narrative_ontology:measurement(qhr_rij_su_t2020, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 2020, 0.38).
narrative_ontology:measurement(qhr_rij_su_t2025, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__reformist_ijtihad, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_hadith_substrate__reformist_ijtihad, 0.08).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate__traditionalist_taqlid).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate__state_hybrid).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, muslim_personal_law_reform).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, islamic_feminist_hermeneutics).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, lgbtq_muslim_affirmation).

% DUAL FORMULATION NOTE:
% This constraint (reformist_ijtihad) is one of three declared readings of the quran_hadith_substrate kernel. The kernel decomposes into: (1) ehrenfest_barrier equivalent: the Quranic text's ethical trajectory as empirically traceable (mountain-like, low ε); (2) spectral_universality equivalent: the hadith corpus's historical contingency and internal contradictions (mountain-like, low ε); (3) eigenvector_thermalization equivalent: the AUTHORITY STRUCTURE that adjudicates between them — this is the contested, extractive layer where the three readings operate. This constraint maps to the eigenvector_thermalization layer for the reformist reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_hadith_substrate__reformist_ijtihad, institutional, 0.25).
constraint_indexing:directionality_override(quran_hadith_substrate__reformist_ijtihad, powerless, 0.15).
constraint_indexing:directionality_override(quran_hadith_substrate__reformist_ijtihad, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
