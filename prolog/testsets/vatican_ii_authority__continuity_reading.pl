% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__continuity_reading, []).

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
 *   constraint_id: vatican_ii_authority__continuity_reading
 *   human_readable: Vatican II Continuity Reading: Organic Development Doctrine
 *   domain: theological/ecclesiastical
 *
 * SUMMARY:
 *   Vatican II (1962–1965) convened to aggiornamento—update the Church for
 *   the modern world. The Council issued 16 documents authorizing substantial
 *   changes: liturgical reform (Mass in vernacular, reformed ritual),
 *   ecumenical opening, democratic governance structures in dioceses, revised
 *   seminary training. The question that splits the Church: are these reforms
 *   continuous with prior Catholic doctrine (the 'continuity' reading) or do
 *   they represent substantive rupture? This constraint story instantiates
 *   the continuity reading: Vatican II was legitimate organic development of
 *   an unchanging deposit of faith; the 16 documents are valid; reforms are
 *   faithful when interpreted through hermeneutic of continuity. This is ONE
 *   reading of a contested kernel. The rival readings (rupture_reading,
 *   composite_overdetermination_reading) are separate constraints with their
 *   own ε values, stakeholder structures, and classifications.
 *
 * KEY AGENTS:
 *   - progressive_reformers: Theologians, bishops, pastoral leaders who read Vatican II as authorizing reform; they benefit from the continuity framing because it legitimizes their work
 *   - traditionalist_resistance: Bishops, priests, lay faithful attached to pre-conciliar practice; they pay the cost of accepting reforms by assenting to a reading they find unconvincing
 *   - vatican_institutional_authority: The papal hierarchy and curia; they benefit because the continuity reading preserves their authority and renders the Council authoritative
 *   - academic_theologians: Scholars split by the reading—progressives benefit, traditionalists must defend unpopular reforms as continuous
 *   - lay_faithful_ambiguous: Excluded from authorized interpretation; their lived experience of change is not part of the dispute's formal voices
 *   - hermeneutic_tradition_observers: Analytical seat; they measure whether the reading is textually sustainable or strategically necessary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__continuity_reading, 0.38).
domain_priors:suppression_score(vatican_ii_authority__continuity_reading, 0.52).
domain_priors:theater_ratio(vatican_ii_authority__continuity_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__continuity_reading, rope).
narrative_ontology:human_readable(vatican_ii_authority__continuity_reading, "Vatican II Continuity Reading: Organic Development Doctrine").
narrative_ontology:topic_domain(vatican_ii_authority__continuity_reading, "theological/ecclesiastical").

domain_priors:requires_active_enforcement(vatican_ii_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__continuity_reading, 'd12ba7c6-15e1-4ce1-86a0-8085a017095f').
narrative_ontology:cs_kernel_codification('d12ba7c6-15e1-4ce1-86a0-8085a017095f', fixed_text).
narrative_ontology:cs_authority_grounding('d12ba7c6-15e1-4ce1-86a0-8085a017095f', extraction).
narrative_ontology:cs_interpretation_layer_present('d12ba7c6-15e1-4ce1-86a0-8085a017095f').
narrative_ontology:cs_reading_relation('d12ba7c6-15e1-4ce1-86a0-8085a017095f', vatican_ii_authority__rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('d12ba7c6-15e1-4ce1-86a0-8085a017095f', vatican_ii_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('d12ba7c6-15e1-4ce1-86a0-8085a017095f', foundational, doctrine_develops_organically_under_guidance).
narrative_ontology:cs_axiom_status(doctrine_develops_organically_under_guidance, holdable).
narrative_ontology:cs_axiom_grounding('d12ba7c6-15e1-4ce1-86a0-8085a017095f', doctrine_develops_organically_under_guidance, theological).
narrative_ontology:cs_axiom('d12ba7c6-15e1-4ce1-86a0-8085a017095f', foundational, hermeneutic_of_continuity_resolves_apparent_contradictions).
narrative_ontology:cs_axiom_status(hermeneutic_of_continuity_resolves_apparent_contradictions, holdable).
narrative_ontology:cs_axiom_grounding('d12ba7c6-15e1-4ce1-86a0-8085a017095f', hermeneutic_of_continuity_resolves_apparent_contradictions, instrumental).
narrative_ontology:cs_reference_frame('d12ba7c6-15e1-4ce1-86a0-8085a017095f', organic_doctrinal_development_within_deposit_of_faith).
narrative_ontology:cs_drift_state('d12ba7c6-15e1-4ce1-86a0-8085a017095f', contemporary_textual_historical_scholarship, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d12ba7c6-15e1-4ce1-86a0-8085a017095f', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__continuity_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, progressive_reformers).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, conciliar_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, vatican_institutional_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, academic_theologians).
narrative_ontology:constraint_victim(vatican_ii_authority__continuity_reading, traditionalist_resistance).
narrative_ontology:constraint_victim(vatican_ii_authority__continuity_reading, academic_theologians).
narrative_ontology:constraint_vindicates(vatican_ii_authority__continuity_reading, organic_doctrinal_development_principle).
narrative_ontology:constraint_vindicates(vatican_ii_authority__continuity_reading, hermeneutic_of_continuity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Theologians, bishops, and clergy who read Vatican II as authorizing substantial pastoral and doctrinal reform while remaining within tradition. They benefit by framing reform as legitimate development rather than rupture; this framing legitimizes their work and protects it from charges of infidelity. Their exit is constrained because leaving the Church hierarchy means losing the platform and institutional authority needed to implement reforms.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, progressive_reformers, beneficiary,
    organized, generational, constrained, global).

% Bishops, priests, and lay faithful committed to pre-conciliar theology and practice. They bear the cost of the continuity reading by accepting reforms they experience as substantive ruptures, justified by a hermeneutic they find unconvincing. Their exit is identity-locked: leaving would mean abandoning their Catholic identity as they have constituted it, or forming schismatic communities. The continuity reading's success depends on their assent or at least their strategic silence.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, traditionalist_resistance, payer,
    moderate, generational, identity_locked, global).

% The papal hierarchy and curial apparatus that convened, implemented, and continue to interpret Vatican II. The continuity reading is the official doctrine they defend; it protects papal authority by rendering the Council harmonious with prior teaching and thus legitimate. Their exit is arbitrage-grade: they can revise the reading, call a new Council, or issue new interpretive guidance.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, vatican_institutional_authority, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Scholars in universities and seminaries tasked with explicating the Council's meaning. Progressive academics benefit by the continuity reading (it legitimizes their work, opens space for development); traditionalist academics pay by being required to defend unpopular reforms as continuous with prior doctrine. Their exit is mobile: they can publish, change institutions, or pursue secular academic posts.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, academic_theologians, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__continuity_reading, academic_theologians, payer).

% Catholic lay people who experience the reforms in practice but have no formal voice in interpretation. Many experience discontinuity yet are told it is continuity; their lived experience is not part of the dispute's authorized voices. They are trapped: exit means leaving the Church and severing family/community bonds.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, lay_faithful_ambiguous, excluded,
    powerless, biographical, trapped, global).

% Historians, philosophers of language, and theological methodologists who analyze how the Council's texts are read and interpreted. They observe whether the continuity reading is textually sustainable, strategically necessary, or empirically descriptive of actual change.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, hermeneutic_tradition_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_authority__continuity_reading, vatican_institutional_authority).
narrative_ontology:fixing_cost_class(vatican_ii_authority__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Renders the universal Church's teaching coherent across a major transition: the continuity reading allows progressive and traditional bishops, theologians, and lay faithful to accept the Council's 16 documents as legitimate expressions of one faith rather than fragments of a broken tradition. It solves the coordination problem of preserving papal authority and doctrinal legitimacy across substantive pastoral change.
% TRANSFER_FUNCTION: Transfers hermeneutic authority from strict textual originalism (pre-conciliar reading method) to 'living development' (post-conciliar reading method); this shifts who can legitimately interpret doctrine—those claiming organic continuity gain standing to expand prior meanings. Progressive reformers gain legitimacy; traditionalists must either accept the new reading or claim the Council was hijacked (a position that isolates them).
% ABSENT_VOICES: Lay faithful whose lived experience of liturgical and pastoral change differs radically from the 'development' narrative are not part of the authorized interpretive community. Historians who document textual rupture are sometimes dismissed as hostile or externalist. The Orthodox and Protestant communities who see the Council as a fundamental shift are excluded from the hermeneutic conversation.
% DISAPPEARANCE_RATIONALE: Progressive Catholics argue that without the continuity reading, Vatican II becomes a schism event and the post-conciliar Church loses its claimed continuity with prior tradition—forcing either a rupture theology or institutional collapse. Traditionalists and historians argue that the world has already rearranged: reforms have been implemented, practice has changed, and the continuity reading is theological theater covering real displacement. The verdict is contested because the world's rearrangement depends on whether one treats the reading as constitutive of authority (papal acceptance = validity) or as description (validity depends on actual textual coherence).
% FOUNDING_PROBLEM: Vatican I defined papal infallibility and centralized authority; this created institutional pressure to maintain consistency if the Church was going to adapt to modernity. Vatican II faced the problem of how to reform the Church's approach to ecumenism, liturgy, and the world without appearing to reverse prior doctrine or admit previous popes erred. The continuity reading solves this: it allows reform as development within the same deposit of faith, preserving papal authority and doctrinal legitimacy simultaneously.
% FOUNDING_PROBLEM_CORROBORATION: The Vatican's own documents (Dei Verbum, Unitatis Redintegratio) frame the Council as developing doctrine. Pope Benedict XVI explicitly adopted 'hermeneutic of continuity' as the official reading method. Historians (Massimo Faggioli, Christoph Theobald, others outside the benefiting parties) argue the founding problem was institutional self-preservation, not genuine doctrinal coherence—the Council needed a continuity narrative to maintain papal authority, independent of whether the texts genuinely support continuous reading. The corroboration splits: Vatican sources attests continuity is real; academic historians attest the narrative was strategically necessary and textually contested.
narrative_ontology:disappearance_verdict(vatican_ii_authority__continuity_reading, contested).
narrative_ontology:founding_problem_status(vatican_ii_authority__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__continuity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_authority__continuity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__continuity_reading_tests).
:- end_tests(vatican_ii_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 endpoint) because the continuity reading transfers hermeneutic authority from textual originalism to 'living development'—this benefits progressive interpreters but does not collect concentrated rents in the economic sense. Suppression is moderate-high (0.52) because the reading's persistence requires active doctrinal enforcement: traditionalist dissent must be managed (through teaching documents, disciplinary measures, institutional pressure), and rival readings (rupture, overdetermination) must be suppressed in the seminaries and hierarchy. Theater ratio is moderate (0.31) and slowly rising through the interval: initially the reading was 'real' doctrinal work (t=0–12); as historical and textual scholarship increasingly documented discontinuity, more effort went into performative maintenance of the continuity narrative (t=12–36). The slight decline (t=48–60) reflects stabilization: the reading has become institutional orthodoxy, so performative maintenance is less desperate. The measurement grid is shared across all three metrics; every metric is authored at every time point. The interval spans 1962 (Council opening, t=0) to 2022 (60 years post-Council); we measure the constraint's operation as a reading-enforcement system, not as a description of the Council itself.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (Vatican institutional authority) and the beneficiary seats (progressive reformers) should compute as rope or weak tangled_rope because they genuinely coordinate on the reading and both benefit from its coherence. The traditionalist resistance seat should compute as tangled_rope or snare because the reading is imposed on them despite their belief it is false; they bear the cost of assenting to or resisting a doctrine they find unconvincing. Academic theologians split: progressives compute as rope beneficiaries, traditionalists as trapped or snare-positioned payers. The engine computes this divergence from the per-seat power, exit, and beneficiary/victim structure. The constraint is CLAIMED as rope (coordination); the metrics and stakeholder structure support reading it as tangled_rope (coordination for some, extraction for others).
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive reformers (d ~0.2–0.3, low target end): they benefit from the reading's legitimacy, have organized power, and mobile exit (they can publish externally, teach in secular universities). Vatican institutional authority (d ~0.1–0.2, beneficiary end): it sets the reading, collects the benefit (preserved authority), has arbitrage exit. Traditionalists (d ~0.75–0.85, high target end): they are the targets of the reading's enforcement, bear the cost (must assent to or resist a reading they reject), have identity-locked exit (leaving the Church means severing their constitutive identity). Academic theologians split by position (d ranges 0.2–0.75 depending on progressive/traditionalist stance). Lay faithful (d ~0.8, high target end): they are excluded from interpretation, experience the reforms, have trapped exit.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading does NOT resolve via mandatrophy. The founding problem (maintaining papal authority and doctrinal legitimacy across reform) is structurally live: the Vatican must still manage the coherence of its teaching, traditionalists must still be kept in communion, progressives must still claim legitimacy. The continuity reading is not a degraded rope maintained by inertia—it is actively defended through episcopal teaching, seminary instruction, interpretive documents, and disciplinary action against rival readings. The theater ratio is moderate and stable (not rising into piton territory), suggesting the reading is more than pure performance, though growing textual scholarship increasingly reveals the performative component. The constraint persists because the Vatican has the authority to define legitimate reading and can suppress rival interpretations in the institutional hierarchy. It is not a piton because institutional authority actively maintains it, not from inertia but from ongoing coordinating need.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hermeneutic_textual_coherence,
    'Do Vatican II''s 16 documents genuinely express continuous development of prior Catholic doctrine, or do they contain substantive reversals that can only be reconciled through interpretive gymnastics?',
    'Systematic textual comparison by neutral (non-benefiting) historians and theologians: comparison of specific doctrinal statements (e.g., on religious freedom, secular authority, salvation outside the Church) before and after the Council, measuring linguistic coherence and propositional continuity. Cross-reference with Vatican documents'' own hermeneutic guidelines.',
    'If documents are genuinely coherent under traditional hermeneutics, the continuity reading is substantive and the constraint computes as rope. If coherence requires non-traditional interpretive methods, the reading is partly performative and the constraint should compute as tangled_rope. If documents contain genuine logical contradictions, the reading is theater and the constraint computes as snare or piton.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hermeneutic_textual_coherence, empirical, 'Textual coherence of Vatican II under continuity hermeneutics').

omega_variable(
    institutional_authority_vs_textual_fact,
    'Does the Vatican''s institutional authority to define legitimate reading settle whether the continuity reading is TRUE, or only whether it is ENFORCED?',
    'Philosophical analysis of authority and truth (how much does institutional power determine doctrine in a faith tradition?); comparative study of other contested kernels where institutional authority defined readings. The distinction hinges on whether authority *constitutes* truth in this domain or merely enforces a politically useful interpretation.',
    'If authority settles truth, the continuity reading is the real constraint (a rope-side coordination). If authority only enforces, the ''true'' constraint is the textual state (rupture or composite reading), and the continuity reading is a performative overlay. This determines whether the constraint''s function is genuinely coordinating or primarily extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_authority_vs_textual_fact, conceptual, 'Whether institutional authority constitutes doctrinal truth or merely enforces narrative').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the traditionalist resistance suppressed by external institutional pressure (canonical penalties, removal from teaching posts, hierarchy enforcement) or by internalized commitment to Church unity and papal obedience that makes exit unthinkable even without coercion?',
    'Post-exit or post-marginalization trajectory analysis: if traditionalist priests or bishops removed from institutional leverage maintain the same position, suppression is partly internalized. If they shift position upon exit, suppression was largely structural. Measure through interviews, publishing records, and movement toward schismatic communities.',
    'If suppression is structural, the traditionalist seat computes as snare-trapped. If suppression is internalized, the constraint''s effective suppression is higher than the raw metric (0.52) suggests—the target carries the suppression with them. This affects whether the reading persists through active enforcement or internalized identity-fusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Suppression mechanism: structural enforcement vs. internalized identity-commitment').

omega_variable(
    reading_kernel_foreclosure,
    'Does the continuity reading logically foreclose the rupture reading, or do they coexist as live positions held by different institutional factions?',
    'Logical analysis: if the continuity reading is true, is the rupture reading logically impossible (foreclosed), or merely false and contestable (coexists_with)? The distinction depends on whether ''development'' and ''rupture'' are genuinely opposed categories or just different interpretive frames for the same event.',
    'If the readings foreclose each other, only one can be institutional doctrine; the winning reading is definitional of authority. If they coexist, both remain live alternatives and the constraint is managing contested space rather than settling a dispute. This affects the reading_relations field in cs_structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_foreclosure, conceptual, 'Whether continuity and rupture readings logically foreclose each other or coexist as live alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__continuity_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_authority__continuity_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(vati_tr_t0, observed).
narrative_ontology:measurement(vati_tr_t6, vatican_ii_authority__continuity_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement_basis(vati_tr_t6, observed).
narrative_ontology:measurement(vati_tr_t12, vatican_ii_authority__continuity_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement_basis(vati_tr_t12, observed).
narrative_ontology:measurement(vati_tr_t24, vatican_ii_authority__continuity_reading, theater_ratio, 24, 0.29).
narrative_ontology:measurement_basis(vati_tr_t24, observed).
narrative_ontology:measurement(vati_tr_t36, vatican_ii_authority__continuity_reading, theater_ratio, 36, 0.32).
narrative_ontology:measurement_basis(vati_tr_t36, observed).
narrative_ontology:measurement(vati_tr_t48, vatican_ii_authority__continuity_reading, theater_ratio, 48, 0.31).
narrative_ontology:measurement_basis(vati_tr_t48, observed).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_authority__continuity_reading, theater_ratio, 60, 0.31).
narrative_ontology:measurement_basis(vati_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_authority__continuity_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(vati_be_t0, observed).
narrative_ontology:measurement(vati_be_t6, vatican_ii_authority__continuity_reading, base_extractiveness, 6, 0.32).
narrative_ontology:measurement_basis(vati_be_t6, observed).
narrative_ontology:measurement(vati_be_t12, vatican_ii_authority__continuity_reading, base_extractiveness, 12, 0.36).
narrative_ontology:measurement_basis(vati_be_t12, observed).
narrative_ontology:measurement(vati_be_t24, vatican_ii_authority__continuity_reading, base_extractiveness, 24, 0.38).
narrative_ontology:measurement_basis(vati_be_t24, observed).
narrative_ontology:measurement(vati_be_t36, vatican_ii_authority__continuity_reading, base_extractiveness, 36, 0.4).
narrative_ontology:measurement_basis(vati_be_t36, observed).
narrative_ontology:measurement(vati_be_t48, vatican_ii_authority__continuity_reading, base_extractiveness, 48, 0.39).
narrative_ontology:measurement_basis(vati_be_t48, observed).
narrative_ontology:measurement(vati_be_t60, vatican_ii_authority__continuity_reading, base_extractiveness, 60, 0.38).
narrative_ontology:measurement_basis(vati_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_authority__continuity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(vati_su_t0, observed).
narrative_ontology:measurement(vati_su_t6, vatican_ii_authority__continuity_reading, suppression_requirement, 6, 0.4).
narrative_ontology:measurement_basis(vati_su_t6, observed).
narrative_ontology:measurement(vati_su_t12, vatican_ii_authority__continuity_reading, suppression_requirement, 12, 0.45).
narrative_ontology:measurement_basis(vati_su_t12, observed).
narrative_ontology:measurement(vati_su_t24, vatican_ii_authority__continuity_reading, suppression_requirement, 24, 0.5).
narrative_ontology:measurement_basis(vati_su_t24, observed).
narrative_ontology:measurement(vati_su_t36, vatican_ii_authority__continuity_reading, suppression_requirement, 36, 0.52).
narrative_ontology:measurement_basis(vati_su_t36, observed).
narrative_ontology:measurement(vati_su_t48, vatican_ii_authority__continuity_reading, suppression_requirement, 48, 0.53).
narrative_ontology:measurement_basis(vati_su_t48, observed).
narrative_ontology:measurement(vati_su_t60, vatican_ii_authority__continuity_reading, suppression_requirement, 60, 0.52).
narrative_ontology:measurement_basis(vati_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__continuity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vatican_ii_authority__continuity_reading, 0.18).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, vatican_ii_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, vatican_ii_authority__composite_overdetermination_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, traditional_mass_prohibition__enforcement_constraint).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, papal_infallibility__scope_and_limits).

% DUAL FORMULATION NOTE:
% The vatican_ii_authority kernel decomposes into three structurally distinct constraints: continuity_reading (this story), rupture_reading, and composite_overdetermination_reading. The three readings have different ε values, different beneficiary/victim structures, and would compute to different types. The continuity reading is claimed as rope (genuine coordination with progressive and institutional alignment); the rupture reading claims the Council documents as snare (imposing rupture disguised as development); the composite reading claims overdetermination (the texts cannot be resolved into either reading, leaving structural ambiguity). Each reading is a separate constraint because each has a different ε—how one measures the Council's continuity or rupture with prior doctrine determines which constraint one is analyzing. These are linked via network.affects_constraints because the readings contend for the same institutional authority and teaching platform.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_authority__continuity_reading, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
