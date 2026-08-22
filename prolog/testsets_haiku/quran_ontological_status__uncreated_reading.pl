% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__uncreated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_ontological_status__uncreated_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: quran_ontological_status__uncreated_reading
 *   human_readable: Qur'an as Uncreated Eternal Divine Speech (kalām Allāh qadīm)
 *   domain: theological/political
 *
 * SUMMARY:
 *   This constraint instantiates the uncreated-Qur'an reading (kalām Allāh
 *   qadīm) of the contested kernel quran_ontological_status. The doctrine
 *   asserts that the Qur'an is uncreated, coeternal divine speech — a fact of
 *   divine nature rather than a temporal artifact. This reading privileges
 *   literalist hermeneutics, maximizes prophetic authority, and treats
 *   textual meaning as fixed divine fact. The constraint benefits
 *   traditionalist jurists and literalist theological schools while imposing
 *   costs on rational theologians, metaphorical interpreters, and reform
 *   movements seeking textual flexibility. The CLAIM (mountain — natural
 *   ontological fact) diverges from the METRICS (extractiveness and
 *   suppression both high): the constraint is authored as a claim to
 *   naturality while the measurements show rising suppression and extraction
 *   across its institutional history, particularly during periods of
 *   enforcement (Abbasid al-Mutawakkil era, post-Saladin Sunni-revival
 *   periods). This divergence is intentional: the engine measures whether the
 *   constraint's natural-law claim is sustained by its actual operation or
 *   whether the divergence signals false-summit structure.
 *
 * KEY AGENTS:
 *   - Traditionist jurists (institutional beneficiary) — maintain interpretive authority through the doctrine's fixing of textual meaning
 *   - Literalist theological schools (organized beneficiary) — depend on uncreated status for their theological framework
 *   - Rational theologians and Mu'tazilites (powerful but marginalized payer) — bear the cost of foreclosed philosophical reconciliation
 *   - Reform movements (identity-locked payer) — cannot reinterpret law for new circumstances without rupturing Islamic identity
 *   - Political authorities (institutional agenda-setter) — enforce the constraint through doctrinal enforcement and suppression of alternatives
 *   - Scholarly analytical observer (observer) — measures the constraint's operation across 1200-year interval
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__uncreated_reading, 0.68).
domain_priors:suppression_score(quran_ontological_status__uncreated_reading, 0.72).
domain_priors:theater_ratio(quran_ontological_status__uncreated_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, accessibility_collapse, 0.89).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__uncreated_reading, mountain).
narrative_ontology:human_readable(quran_ontological_status__uncreated_reading, "Qur'an as Uncreated Eternal Divine Speech (kalām Allāh qadīm)").
narrative_ontology:topic_domain(quran_ontological_status__uncreated_reading, "theological/political").

domain_priors:emerges_naturally(quran_ontological_status__uncreated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__uncreated_reading, 'a4a7a36e-c35d-4dce-b67c-d0ea671fb7f9').
narrative_ontology:cs_kernel_codification('a4a7a36e-c35d-4dce-b67c-d0ea671fb7f9', formalized).
narrative_ontology:cs_authority_grounding('a4a7a36e-c35d-4dce-b67c-d0ea671fb7f9', lineage).
narrative_ontology:cs_interpretation_layer_present('a4a7a36e-c35d-4dce-b67c-d0ea671fb7f9').
narrative_ontology:cs_reading_relation('a4a7a36e-c35d-4dce-b67c-d0ea671fb7f9', quran_ontological_status__created_reading, coexists_with).
narrative_ontology:cs_reading_relation('a4a7a36e-c35d-4dce-b67c-d0ea671fb7f9', quran_ontological_status__state_enforced_creation_reading, coexists_with).
narrative_ontology:cs_axiom('a4a7a36e-c35d-4dce-b67c-d0ea671fb7f9', foundational, revelation_coeternal_with_divine_essence).
narrative_ontology:cs_axiom_status(revelation_coeternal_with_divine_essence, holdable).
narrative_ontology:cs_axiom_grounding('a4a7a36e-c35d-4dce-b67c-d0ea671fb7f9', revelation_coeternal_with_divine_essence, deontological).
narrative_ontology:cs_axiom('a4a7a36e-c35d-4dce-b67c-d0ea671fb7f9', foundational, textual_meaning_fixed_divine_fact).
narrative_ontology:cs_axiom_status(textual_meaning_fixed_divine_fact, holdable).
narrative_ontology:cs_axiom_grounding('a4a7a36e-c35d-4dce-b67c-d0ea671fb7f9', textual_meaning_fixed_divine_fact, conventional).
narrative_ontology:cs_reference_frame('a4a7a36e-c35d-4dce-b67c-d0ea671fb7f9', divine_revelation_coeternity).
narrative_ontology:cs_drift_state('a4a7a36e-c35d-4dce-b67c-d0ea671fb7f9', contemporary_institutional_practice_1200_years_post_institutionalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a4a7a36e-c35d-4dce-b67c-d0ea671fb7f9', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__uncreated_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, traditionist_jurists).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, literalist_theological_schools).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, anti_rationalist_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, rational_theologians).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, metaphorical_interpreters).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, reform_movements_seeking_textual_flexibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the constraint's establishment of textual meaning as fixed divine fact rather than contingent artifact. This reading secures their interpretive authority as transmitters of unambiguous divine will. The uncreated status gives their jurisprudence the character of recovering pre-existing divine law rather than constructing new doctrine. Their exit option is toward rationalist reinterpretation, which would undermine their institutional standing.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, traditionist_jurists, beneficiary,
    institutional, generational, arbitrage, global).

% Their theological framework depends on the Qur'an's literal meaning being coeternal with God — a fixed reference point that cannot be dissolved into metaphor or historical contingency. The uncreated reading forecloses the rationalist move of interpreting anthropomorphic language (God's hand, face, throne) as metaphorical. Their exit toward created-speech readings would require wholesale philosophical reorientation.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, literalist_theological_schools, beneficiary,
    organized, generational, constrained, global).

% Bear the cost of the constraint's establishment: their philosophical project of reconciling revelation with rational inquiry is structurally constrained. The uncreated doctrine forecloses their favored interpretive moves (metaphorical reading, textual contingency, meaning-making as rational activity). Their exit is toward created-speech readings or toward secular philosophy, where the constraint does not apply. Historical pressure (Abbasid rationalism, modern Islamic modernism) shows they have attempted this exit repeatedly.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, rational_theologians, payer,
    powerful, biographical, mobile, global).

% Their hermeneutical practice is constrained by the fixed-meaning doctrine: passages that would benefit from metaphorical reading (anthropomorphisms, apparent contradictions, historically-contingent laws) must be reconciled to literal divine truth instead. The uncreated reading makes their interpretive flexibility structurally illegitimate. Their exit is toward minority theological schools or secular studies of religion, where they face institutional marginalization.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, metaphorical_interpreters, payer,
    moderate, biographical, constrained, global).

% Movements seeking to modernize Islamic law or ethics face the constraint's foreclosure of the textual flexibility they require. If the Qur'an is uncreated, eternally fixed in meaning, then reform movements cannot reinterpret it to address new circumstances or reject its apparent endorsement of slavery, patriarchal authority, or premodern governance. Their exit is into secular nationalism or theological heterodoxy, which involves rupture of identity as Muslims within the Islamic tradition.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, reform_movements_seeking_textual_flexibility, payer,
    moderate, biographical, identity_locked, global).

% Enforce the constraint by establishing literalist Qur'anic doctrine as the binding legal standard and suppressing rationalist reinterpretation. Historical example: the Abbasid caliphate initially promoted the created-speech reading to centralize hermeneutical authority under rational elites; later caliphates (post-Samanids) established the uncreated reading to secure legitimacy against reform movements. The constraint's enforcement machinery (inquisition, mihna; book burning; legal sanction against heterodox theology) is the source of the suppression metric.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, political_authorities_citing_the_constraint, agenda_setter,
    institutional, generational, arbitrage, national).

% Observes the constraint's operation across historical and contemporary Islamic contexts. Measures whether the doctrine functions as fixed natural law (mountain) or as politically enforced doctrine (snare/tangled rope). Notes the divergence between the reading's claim to naturality (God coeternal with uncreated speech) and its historical contingency (the doctrine emerged in 8th-century polemic and was institutionalized through suppression of alternatives).
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, scholarly_analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_ontological_status__uncreated_reading, traditionist_jurists).
narrative_ontology:fixing_cost_class(quran_ontological_status__uncreated_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The uncreated-Qur'an doctrine provides a stable reference point for Islamic jurisprudence: if all believers work from the same fixed-meaning text coeternal with God, their legal rulings derive from a common source rather than diverging into radically plural schools. This prevents the fragmentation that would occur if each rational theologian could reinterpret the text to suit their philosophical framework.
% TRANSFER_FUNCTION: Transfers interpretive authority from philosophers and rational theologians (who would claim the right to metaphorical or contextual reading) to traditionalist jurists and literalist scholars (who claim the right to transmit uncreated meaning). Transfer is of institutional gatekeeping power and interpretive legitimacy: those certified in orthodox jurisprudence can adjudicate disputes; rational reinterpretations are excluded as heretical or ignorant.
% ABSENT_VOICES: Rational philosophers and Mu'tazilite theologians are structurally excluded — after the reversal at t=150 (al-Mutawakkil), rationalist theology is marginalized from official discourse. Reform movements seeking to modernize Islamic ethics (abolition of slavery, women's rights, secular governance) are excluded because the fixed-meaning doctrine prevents reinterpretation of problematic passages. Secular Qur'anic scholars studying the text's historical composition are excluded because the doctrine forecloses historical-critical inquiry into the text's origins.
% DISAPPEARANCE_RATIONALE: Defender perspective: if the uncreated doctrine vanished, Islamic law would collapse into relativism and heresy — each school would claim equal authority and Islam would fragment. Reformist perspective: Islamic law has already substantially diversified despite the doctrine (Hanafi, Maliki, Shafi'i, Hanbali schools all coexist; contemporary global Islam has generated countless interpretations); the doctrine now functions to suppress adaptive interpretation rather than prevent fragmentation, so its disappearance would enable modernization without causing collapse. Analytical perspective: the doctrine's disappearance would enable reform movements and rationalist schools to flourish; the world would rearrange toward greater hermeneutical pluralism within Islam.
% FOUNDING_PROBLEM: In the 8th-9th centuries, Islamic scholars debated: how is God's eternal nature related to revelation? If revelation is created, it is contingent and temporal, potentially diminishing its absolute authority and God's transcendence. If revelation is uncreated and coeternal with God, it preserves God's perfection (nothing is created external to God except creation itself) and the Qur'an's unquestionable status.
% FOUNDING_PROBLEM_CORROBORATION: Rationalist and modernist Islamic scholars attest that the founding problem has shifted: contemporary Islamic thought largely accepts both that revelation can be textually studied and that jurisprudence must adapt to new circumstances, indicating the original problem (preserving transcendence vs. fixing authority) has been substantially resolved in practice. Traditionalist scholars attest the problem is live and ever-recurring: each generation of rationalists threatens to dissolve the fixed standard, making the doctrine's protection essential. Historical-critical Qur'anic scholars from outside the benefiting traditionalist parties (non-Muslims studying the text's composition; secular Muslims; comparative-religion scholars) attest that the problem emerged through institutional politics and suppression, not from any deep conceptual necessity — the created and uncreated readings coexist logically and historically, and the uncreated reading's dominance results from al-Mutawakkil's backing rather than from rational victory.
narrative_ontology:disappearance_verdict(quran_ontological_status__uncreated_reading, contested).
narrative_ontology:founding_problem_status(quran_ontological_status__uncreated_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__uncreated_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_ontological_status__uncreated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__uncreated_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__uncreated_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_ontological_status__uncreated_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, ExtMetricName, E),
    domain_priors:suppression_score(quran_ontological_status__uncreated_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(quran_ontological_status__uncreated_reading),
    narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(quran_ontological_status__uncreated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness series rises from 0.15 (early, pre-institutionalization) to 0.68 (stable at interval end), tracking the doctrine's institutionalization through suppression of alternatives. The suppression_requirement shows sharp rise at 150-year mark (al-Mutawakkil's reversal of the Mu'tazilite mihna, institutionalizing uncreated doctrine; suppression rises to 0.68 and stays high thereafter), indicating that the constraint's persistence depends increasingly on active enforcement rather than voluntary acceptance. Theater_ratio rises from 0.08 to 0.41, suggesting that an increasing share of enforcement activity defends the doctrine's authority rather than addressing genuine coordination problems — a signature of extraction riding on coordination. The plateau at 900-1200 years indicates institutional equilibrium: the doctrine is so thoroughly embedded in Islamic institutions that enforcement is routinized and theater ratio stabilizes. This is the inverse of decay: the constraint has become inertial, maintained by institutional theater and suppression rather than by the coordination problem it once solved. The measurement grid is aligned: all three metrics share the six time points (t=0, 150, 300, 600, 900, 1200).
 *
 * PERSPECTIVAL GAP:
 *   From the traditionalist-jurist seat, the constraint is a genuine mountain — divine speech is what it is, coeternal with God, and their duty is to transmit it faithfully. From the rational theologian's seat, the constraint is a snare — the doctrine forecloses their philosophical project through institutional suppression, not through logical necessity. From the reform-movement seat, the constraint is a tangled rope: the coordination it provides (fixing meaning) benefits them incidentally (their followers know the same text) while its extraction severely constrains them (cannot reinterpret for new circumstances). The engine should compute divergent types across these three seats from the structural data alone. The analytical observer seat (highest power, analytical time horizon) sees both the doctrine's coordination value (prevents total fragmentation of Islamic law) and its extraction value (enables institutional gatekeeping and prevents adaptive reinterpretation). This perspectival gap is exactly what per-seat classification should reveal.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (traditionist jurists, literalist schools) experience low d toward this constraint — it subsidizes their interpretive authority and institutional standing. Payers (rational theologians, reform movements, metaphorical interpreters) experience high d — the constraint extracts their interpretive freedom and institutional legitimacy. Political authorities implementing enforcement experience moderate d with arbitrage exit (they can shift to created-speech reading if political incentives change, as happened historically), placing them nearer the beneficiary end despite their enforcement role. The constraint's directionality is NOT symmetric: those who benefit from fixed textual meaning are institutional and have arbitrage exits; those who bear the cost are either powerful (rational theologians with exit into secular philosophy) or identity-locked (reform movements that cannot leave Islam without ceasing to be Islamic). This structural asymmetry drives the high d divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (9th-century debate: how to preserve God's transcendence while affirming revelation's authority) was live at t=0-300 and has become increasingly dead by t=1200. The constraint persists not because scholars are actively debating the founding problem (they largely accept it as settled) but because institutional gatekeeping and suppression of alternatives maintain it. This is the classic mandatrophy condition: mandate outdated, mechanism inertial. The theater_ratio rising from 0.08 to 0.41 while suppression stays high indicates that enforcement is increasingly performing the constraint's authority rather than solving any live coordination problem. The contemporary Islamic landscape shows that Islamic law has NOT fragmented into radical schools despite (or because of) reform movements and rationalist minority schools — the constraint's coordination value is real but modest, and its extraction value is substantial. The engine's classification at the analytical seat should show tangled-rope (residual coordination, heavy extraction) despite the doctrine's claim to mountain status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_political_enforcement,
    'Is the uncreated-Qur''an doctrine a genuine natural ontological fact (mountain), or is it a politically enforced doctrine whose claim to naturality is a cover story?',
    'Historical contingency test: if the doctrine emerged through political suppression of alternatives and would not persist without institutional enforcement, it is constructed (snare/tangled-rope) rather than natural. Ethnographic test: do contemporary Muslims who have never encountered the institutional enforcement still accept the doctrine as natural, or do they adopt it through socialization within orthodox institutions?',
    'If constructed, the constraint reclassifies from mountain to tangled-rope or snare at the analytical seat; the beneficiary identification becomes definitive (traditionalist-jurist institutional gatekeeping benefits from naturalization), and the mandate-obsolescence diagnosis strengthens (false summit candidate). If genuinely natural, the high suppression and extraction metrics indicate that the constraint''s operation is dramatically asymmetric — suppression protects a natural fact that needs no protection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_political_enforcement, conceptual, 'Whether the doctrine''s natural-law status is ontologically grounded or politically constructed.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.72) primarily structural (institutional penalties, exile, execution) or primarily internalized (scholars internalize the constraint''s authority and self-censor)?',
    'Post-exit trajectory: if suppressed rational theologians and reform-movement scholars continue to resist the doctrine after institutional exit (writing in secular contexts, leading diaspora communities), suppression is partly internalized; if they accept it after exit, suppression is primarily structural. Institutional-exit ethnography: do scholars who leave orthodox institutions maintain their rationalist commitments or do they drift toward orthodoxy?',
    'If primarily structural, the constraint''s effective suppression would drop significantly if institutional enforcement were removed, suggesting reclassification potential. If primarily internalized, the constraint persists even after institutional exit and represents deeper epistemic capture — effective suppression is higher than the structural measure alone suggests, and reform is harder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism of suppression: structural barriers vs. internalized acceptance.').

omega_variable(
    coordination_function_persistent,
    'Does the doctrine''s coordination function (preventing Islamic law from fragmenting into radically divergent schools) still operate at t=1200, or has it become vestigial theater while extraction persists?',
    'Comparative-jurisprudence test: measure the diversity of Islamic legal schools, rulings, and interpretations at different times. If diversity remains low despite the doctrine''s mounting extraction (theater_ratio rising), coordination function persists and extraction is layered on top. If diversity rises while suppression stays constant, the coordination function is degrading and suppression is pure gatekeeping without coordination work.',
    'If coordination persists, the constraint is tangled-rope (real coordination, real extraction). If coordination is vestigial, the constraint should reclassify toward piton (mostly performance and institutional inertia, not coordination problem-solving) or toward snare (pure extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_persistent, empirical, 'Whether the doctrine''s coordination function remains active or has atrophied.').

omega_variable(
    reform_movement_identity_lock_mechanism,
    'Why do reform movements seeking textual flexibility remain within Islamic identity frameworks rather than exit into secular modernism or heterodox religions? Is the identity lock genuine (the constraint is fused with Islamic identity and cannot be questioned without identity rupture) or strategic (exit is available but costly)?',
    'Comparative exit analysis: if reform movements that exit Islam into secular contexts or heterodox faiths adopt similar hermeneutical flexibility and flourish, the exit is real and the lock is strategic (the constraint imposes cost but not logical impossibility). If they maintain versions of the same constraint even outside Islam, the lock is partially internalized.',
    'If genuine identity lock, reform movements are trap-adjacent (not technically trapped but trapped-feeling due to identity fusion); their directionality should compute toward target (high d), amplifying the extraction metric. If strategic, they are constrained-exit (arbitrage possible at exit cost); their directionality is moderate, and extraction is real but not maximal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_movement_identity_lock_mechanism, empirical, 'Mechanism of identity-lock exit constraint on reform movements.').

omega_variable(
    sibling_reading_foreclosure_structure,
    'Does this reading (uncreated) logically foreclose the created reading in a single coherent framework, or do they coexist as different institutional positions held by different factions?',
    'Logical compatibility test: can a single theologian hold both that God is transcendent (essential to created-speech reading) and that revelation is coeternal with God (essential to uncreated reading) without internal contradiction? If yes, the readings coexist at the logical level and their institutional conflict is political, not conceptual. If no, they foreclose each other.',
    'If logically foreclosing, cs_structure.reading_relations should declare forecloses rather than coexists_with. If coexisting logically but conflicting institutionally, coexists_with is correct and the sibling-reading conflict is explained by institutional competition, not conceptual incompatibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_structure, conceptual, 'Whether this reading logically forecloses its siblings or coexists with them.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__uncreated_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_ontological_status__uncreated_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(qura_tr_t150, quran_ontological_status__uncreated_reading, theater_ratio, 150, 0.28).
narrative_ontology:measurement(qura_tr_t300, quran_ontological_status__uncreated_reading, theater_ratio, 300, 0.35).
narrative_ontology:measurement(qura_tr_t600, quran_ontological_status__uncreated_reading, theater_ratio, 600, 0.4).
narrative_ontology:measurement(qura_tr_t900, quran_ontological_status__uncreated_reading, theater_ratio, 900, 0.41).
narrative_ontology:measurement(qura_tr_t1200, quran_ontological_status__uncreated_reading, theater_ratio, 1200, 0.41).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_ontological_status__uncreated_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(qura_be_t150, quran_ontological_status__uncreated_reading, base_extractiveness, 150, 0.42).
narrative_ontology:measurement(qura_be_t300, quran_ontological_status__uncreated_reading, base_extractiveness, 300, 0.58).
narrative_ontology:measurement(qura_be_t600, quran_ontological_status__uncreated_reading, base_extractiveness, 600, 0.65).
narrative_ontology:measurement(qura_be_t900, quran_ontological_status__uncreated_reading, base_extractiveness, 900, 0.68).
narrative_ontology:measurement(qura_be_t1200, quran_ontological_status__uncreated_reading, base_extractiveness, 1200, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_ontological_status__uncreated_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(qura_su_t150, quran_ontological_status__uncreated_reading, suppression_requirement, 150, 0.68).
narrative_ontology:measurement(qura_su_t300, quran_ontological_status__uncreated_reading, suppression_requirement, 300, 0.75).
narrative_ontology:measurement(qura_su_t600, quran_ontological_status__uncreated_reading, suppression_requirement, 600, 0.72).
narrative_ontology:measurement(qura_su_t900, quran_ontological_status__uncreated_reading, suppression_requirement, 900, 0.72).
narrative_ontology:measurement(qura_su_t1200, quran_ontological_status__uncreated_reading, suppression_requirement, 1200, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__uncreated_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(quran_ontological_status__uncreated_reading, 0.14).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, quran_ontological_status__created_reading).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, quran_ontological_status__state_enforced_creation_reading).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, prophetic_authority_maximization).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, literalist_hermeneutics_privilege).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, rationalist_theological_marginalization).

% DUAL FORMULATION NOTE:
% The kernel quran_ontological_status decomposes into three structurally distinct constraints: the created_reading (ontologically distinct) claims the Qur'an is created speech, treating revelation as temporal artifact; the uncreated_reading (this constraint) claims the Qur'an is coeternal divine fact; the state_enforced_creation_reading (distinct institutional mechanism) layers Mu'tazilite political enforcement onto created-speech doctrine. All three share the same referent (the Qur'an's ontological status) but have different ε values: created_reading ε≈0.35 (modest coordination of rational theology, low extraction), uncreated_reading ε≈0.68 (this reading: residual coordination, substantial extraction via institutional gatekeeping), state_enforced_creation_reading ε≈0.82 (pure extraction through inquisition machinery). The ε divergence reflects different equilibria in the same theological space. This story instantiates only the uncreated_reading; the siblings are separate JSON files linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_ontological_status__uncreated_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
