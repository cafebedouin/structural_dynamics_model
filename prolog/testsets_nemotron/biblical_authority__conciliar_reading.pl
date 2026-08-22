% ============================================================================
% CONSTRAINT STORY: biblical_authority__conciliar_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_authority__conciliar_reading, []).

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
 *   constraint_id: biblical_authority__conciliar_reading
 *   human_readable: Biblical Authority — Conciliar/Patristic Reading
 *   domain: theology/religious_studies/history_of_christianity
 *
 * SUMMARY:
 *   This constraint story models the conciliar/patristic reading of biblical
 *   authority — the hermeneutic dominant in Eastern Orthodoxy and parts of
 *   Oriental Orthodoxy, Anglicanism, and Lutheranism. Scripture is not
 *   self-interpreting (contra sola scriptura) nor does it require a living
 *   magisterium (contra Roman Catholic tradition-scripture reading). Instead,
 *   its authoritative interpretation is found in the reception of the seven
 *   ecumenical councils (325-787) and the patristic consensus they embody.
 *   Tradition is living continuity (paradosis) — the Holy Spirit guiding the
 *   church through history — not a static deposit guarded by a central
 *   office. The constraint coordinates doctrinal unity across autocephalous
 *   churches while extracting the capacity for rapid doctrinal adaptation.
 *   Episcopal collegiality and monastic witness are the primary
 *   beneficiaries; the pressure for timely response to modernity is the
 *   primary victim.
 *
 * KEY AGENTS:
 *   - episcopal_collegiality: Primary beneficiary/agenda_setter (institutional/identity_locked) — holds interpretive authority through conciliar continuity
 *   - autocephalous_churches: Primary beneficiaries (institutional/identity_locked) — maintain communion through shared hermeneutic without centralization
 *   - rapid_doctrinal_adaptation: Primary victim (powerless/trapped) — bears the cost of structural slowness
 *   - parish_laity: Secondary payer/beneficiary (moderate/constrained) — receives stability but bears doctrinal lag
 *   - monastic_elders: Agenda_setter/beneficiary (organized/identity_locked) — sets interpretive tone through ascetic witness
 *   - ecumenical_dialogue_partners: Observer (institutional/analytical) — maps the constraint from outside
 *   - secular_modernity: Excluded (powerful/arbitrage) — generates adaptation pressure but has no voice in the process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__conciliar_reading, 0.32).
domain_priors:suppression_score(biblical_authority__conciliar_reading, 0.41).
domain_priors:theater_ratio(biblical_authority__conciliar_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__conciliar_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__conciliar_reading, "Biblical Authority — Conciliar/Patristic Reading").
narrative_ontology:topic_domain(biblical_authority__conciliar_reading, "theology/religious_studies/history_of_christianity").

domain_priors:requires_active_enforcement(biblical_authority__conciliar_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__conciliar_reading, '6b9de584-213b-496d-9810-e0640fafc4e6').
narrative_ontology:cs_kernel_codification('6b9de584-213b-496d-9810-e0640fafc4e6', fixed_text).
narrative_ontology:cs_authority_grounding('6b9de584-213b-496d-9810-e0640fafc4e6', lineage).
narrative_ontology:cs_interpretation_layer_present('6b9de584-213b-496d-9810-e0640fafc4e6').
narrative_ontology:cs_reading_relation('6b9de584-213b-496d-9810-e0640fafc4e6', biblical_authority__sola_scriptura_reading, coexists_with).
narrative_ontology:cs_reading_relation('6b9de584-213b-496d-9810-e0640fafc4e6', biblical_authority__tradition_scripture_reading, coexists_with).
narrative_ontology:cs_axiom('6b9de584-213b-496d-9810-e0640fafc4e6', foundational, scripture_requires_conciliar_reception).
narrative_ontology:cs_axiom_status(scripture_requires_conciliar_reception, holdable).
narrative_ontology:cs_axiom_grounding('6b9de584-213b-496d-9810-e0640fafc4e6', scripture_requires_conciliar_reception, conventional).
narrative_ontology:cs_axiom('6b9de584-213b-496d-9810-e0640fafc4e6', foundational, tradition_as_living_continuity_not_static_deposit).
narrative_ontology:cs_axiom_status(tradition_as_living_continuity_not_static_deposit, holdable).
narrative_ontology:cs_axiom_grounding('6b9de584-213b-496d-9810-e0640fafc4e6', tradition_as_living_continuity_not_static_deposit, deontological).
narrative_ontology:cs_axiom('6b9de584-213b-496d-9810-e0640fafc4e6', secondary, no_single_see_has_universal_jurisdiction).
narrative_ontology:cs_axiom_status(no_single_see_has_universal_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('6b9de584-213b-496d-9810-e0640fafc4e6', no_single_see_has_universal_jurisdiction, conventional).
narrative_ontology:cs_reference_frame('6b9de584-213b-496d-9810-e0640fafc4e6', undivided_church_conciliar_hermeneutic).
narrative_ontology:cs_drift_state('6b9de584-213b-496d-9810-e0640fafc4e6', post_schism_modernity, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6b9de584-213b-496d-9810-e0640fafc4e6', '').
narrative_ontology:cs_kernel_id(biblical_authority__conciliar_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, episcopal_collegiality).
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, autocephalous_churches).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, rapid_doctrinal_adaptation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, parish_laity).
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, monastic_elders).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, parish_laity).
narrative_ontology:constraint_vindicates(biblical_authority__conciliar_reading, conciliar_authority).
narrative_ontology:constraint_vindicates(biblical_authority__conciliar_reading, patristic_consensus_as_hermeneutic).
narrative_ontology:constraint_vindicates(biblical_authority__conciliar_reading, tradition_as_living_continuity).
narrative_ontology:constraint_vindicates(biblical_authority__conciliar_reading, sacramental_mystery).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops in council collectively interpret Scripture through the lens of patristic consensus and previous ecumenical definitions. They benefit from the authority to define doctrinal boundaries and the structural position of being the church's teaching office. Their identity is fused with the conciliar-hermeneutic tradition — exit would mean abandoning their vocation's self-understanding as successors to the apostles in collegial continuity. They administer the constraint through synodical processes and canonical enforcement of doctrinal norms.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, episcopal_collegiality, beneficiary,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(biblical_authority__conciliar_reading, episcopal_collegiality, agenda_setter).

% Self-governing churches (Constantinople, Alexandria, Antioch, Jerusalem, Moscow, Serbia, Romania, Bulgaria, Georgia, Cyprus, Greece, Poland, Albania, Czech Lands and Slovakia, America) that recognize the conciliar reading as their defining hermeneutic. They benefit from the constraint's fragmentation-tolerant structure — no single magisterium can impose on them — while collectively maintaining the interpretive tradition. Their institutional identity is constituted by communion in this reading; exit from the conciliar framework would dissolve their ecclesial self-understanding.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, autocephalous_churches, beneficiary,
    institutional, civilizational, identity_locked, regional).

% The pressure for swift doctrinal response to new scientific, cultural, or existential challenges (bioethics, digital ontology, climate theology, gender anthropology). Under the conciliar reading, adaptation requires convocation, consensus-seeking across autocephalous bodies, and reception by the faithful — a process measured in decades or centuries. The constraint extracts the capacity for rapid doctrinal agility; the cost is paid by the church's ability to speak to the present moment with authority. There is no exit from this structural slowness without abandoning the reading itself.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, rapid_doctrinal_adaptation, payer,
    powerless, immediate, trapped, global).

% Lay faithful who receive doctrinal stability and sacramental continuity as coordination benefits, but bear the cost of doctrinal lag — teachings that may not address their lived realities (contraception, divorce/remarriage, same-sex relationships, end-of-life decisions). Their exit is constrained by sacramental identity, community embeddedness, and the conviction that the church is the body of Christ; leaving is experienced as spiritual rupture rather than consumer choice.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, parish_laity, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(biblical_authority__conciliar_reading, parish_laity, beneficiary).

% Monastics who preserve and transmit the patristic mind (phronema) through ascetic practice, liturgical continuity, and spiritual fatherhood/motherhood. They set the affective and interpretive tone for how the conciliar reading is lived — not through canonical legislation but through the authority of holiness. They benefit from the constraint's elevation of patristic consensus as the hermeneutic key, which makes their form of life the gold standard of theological reception.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, monastic_elders, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(biblical_authority__conciliar_reading, monastic_elders, beneficiary).

% Roman Catholic, Oriental Orthodox, Protestant, and Anglican interlocutors who engage the conciliar reading from outside. They observe its internal coherence, its resistance to centralized definition, and its claim to represent the undivided church's hermeneutic. Their analytical position allows them to map the constraint's structural dynamics without being bound by its identity logic.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, ecumenical_dialogue_partners, observer,
    institutional, generational, analytical, global).

% The broader cultural, scientific, and political order that generates the challenges to which rapid doctrinal adaptation would respond. It is structurally excluded from the conciliar process — no voice in councils, no standing in synods — yet its questions set the agenda for what adaptation would need to address. The constraint's slowness is most visible in the widening gap between the questions modernity asks and the answers the reading can presently give.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, secular_modernity, excluded,
    powerful, civilizational, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified hermeneutic for Scripture across diverse autocephalous churches without centralized magisterial authority — coordinating doctrinal continuity, sacramental recognition, and communion through the shared reference points of the seven ecumenical councils and the patristic consensus they codified.
% TRANSFER_FUNCTION: Moves interpretive authority from individual conscience or local innovation to the conciliar-patristic consensus; moves the cost of doctrinal slowness onto communities facing novel questions; moves the benefit of stability and identity onto episcopal and monastic structures that embody the tradition.
% ABSENT_VOICES: The victims of doctrinal lag — women discerning ordination, LGBTQ+ faithful seeking sacramental recognition, divorced-and-remarried laity excluded from communion, scientists and ethicists within the church needing authoritative guidance on novel problems — are present in the parishes but structurally excluded from the conciliar process that defines the reading's boundaries. Their voices reach the agenda-setters only through pastoral filtration.
% DISAPPEARANCE_RATIONALE: If the conciliar reading vanished overnight, the autocephalous churches would lose their shared hermeneutic bond — communion would fracture into either localized biblicism (drifting toward sola_scriptura dynamics) or ad hoc episcopal authoritarianism. The sacramental economy (mysteries as grace-bearing acts rather than symbolic memorials) would lose its theological ground. A new coordination mechanism would need to emerge, likely through either Roman Catholic-style centralization or Protestant-style fragmentation.
% FOUNDING_PROBLEM: The 4th-5th century church faced christological and trinitarian controversies that threatened to fracture the empire's unity. Arius, Nestorius, Eutyches, and their opponents each claimed Scripture for their position. The conciliar reading was built to solve: how can Scripture function as authoritative revelation when its interpretation is bitterly contested, without resorting to either imperial coercion or individualistic fragmentation?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the conciliar acts themselves (Nicaea I, Constantinople I, Ephesus, Chalcedon, Constantinople II, Constantinople III, Nicaea II) and by patristic historians outside the benefiting episcopal structure (e.g., R.P.C. Hanson, The Search for the Christian Doctrine of God; Khaled Anatolios, Retrieving Nicaea). Contemporary Orthodox theologians (Metropolitan John Zizioulas, Christos Yannaras) attest the problem remains live in a new key: the unity of the church in a fragmented world. Critics (Protestant historians like Jaroslav Pelikan, Catholic theologians like Yves Congar) argue the founding problem was substantially solved by the 8th century and the arrangement now persists as institutional self-preservation.
narrative_ontology:disappearance_verdict(biblical_authority__conciliar_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__conciliar_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__conciliar_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(biblical_authority__conciliar_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__conciliar_reading, 0.32, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__conciliar_reading_tests).
:- end_tests(biblical_authority__conciliar_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.32) because the constraint does not extract material resources or labor — it extracts interpretive agility, the capacity to reformulate doctrine quickly. Suppression (0.41) is structural: the conciliar process itself, with its requirement for cross-autocephalous consensus and reception by the faithful, suppresses rapid adaptation. The suppression is not coercive in the modern sense (no inquisitorial machinery) but is structural — the constraint's coordination function IS its suppression mechanism. Theater ratio (0.28) reflects genuine patristic engagement mixed with ritualized conciliar language that can obscure the absence of living reception. Accessibility collapse (0.52) is moderate: alternative hermeneutics (sola scriptura, magisterial) remain live options for those willing to exit the communion, but within the communion alternatives are largely collapsed. Resistance (0.58) is significant: the history of the reading includes schisms (Nestorian, Monophysite, Old Believer, True Orthodox) and ongoing internal contestation (ecumenical participation, calendar reform, diaspora governance).
 *
 * PERSPECTIVAL GAP:
 *   From the episcopal-collegial seat, the constraint is a rope: genuine coordination of Scripture interpretation across diversity without coercion. From the rapid-adaptation seat, it is a snare: the coordination story covers structural extraction of agility. From the parish-laity seat, it is a tangled rope: real sacramental coordination mixed with real doctrinal extraction. The engine computes this divergence from the declared structural data — the claimed_type (tangled_rope) reflects the authoring seat's assessment that the coordination function is genuine but asymmetrically extractive.
 *
 * DIRECTIONALITY LOGIC:
 *   Episcopal collegiality and autocephalous churches sit at the beneficiary end (d near 0.0): they hold the interpretive keys, their identity is constituted by the reading, and exit would dissolve their institutional self-understanding. Rapid doctrinal adaptation sits at the target end (d near 1.0): it is the pressure the constraint structurally resists and from which it extracts the cost of slowness. Parish laity sit near symmetric (d ~ 0.5): they receive genuine coordination benefits (sacramental continuity, doctrinal stability) and bear genuine costs (doctrinal lag on lived questions). Monastic elders are beneficiaries with agenda-setting influence (d low but with structural leverage). Ecumenical dialogue partners are analytical observers (d not applicable). Secular modernity is excluded but generates the extraction pressure — its questions are the raw material the constraint processes slowly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (christological/trinitarian unity amid interpretive contestation) was substantially resolved by 787. The mandate has partially outlived its function — the seven councils are settled, the patristic consensus is codified. Yet new problems (modernity's questions) have emerged that the conciliar machinery was not designed to address. The constraint persists not by inertia alone but because the autocephalous churches experience it as constitutive of their identity. Mandatrophy is resolved in the sense that the constraint's continuation is acknowledged as identity-maintenance rather than problem-solving — but unresolved in that the identity claim itself is contested by the victims of doctrinal lag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conciliar_reception_boundary,
    'Where does the conciliar reading''s authority end and local episcopal discretion begin? The seven councils are settled, but post-787 councils (e.g., 879-880, 1341-1351, 1672, 1819, 1872) have contested reception.',
    'Historical-theological analysis of reception patterns across autocephalous churches; synodical acts on disputed councils.',
    'If post-787 councils are authoritatively received, the constraint''s coordination scope expands and extractiveness may rise (more defined boundaries = less adaptation space). If they are locally optional, the constraint is more fragmented than modeled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conciliar_reception_boundary, conceptual, 'The scope of ''ecumenical councils'' in the conciliar reading — fixed at seven or extendable?').

omega_variable(
    patristic_consensus_identification,
    'How is ''patristic consensus'' identified when fathers disagree? The constraint treats consensus as a discoverable fact, but patristic exegesis contains significant diversity (Antiochene vs. Alexandrian, Maximus vs. later syntheses).',
    'Patristic scholarship mapping consensus zones vs. open questions; synodical definitions that cite specific fathers as authoritative.',
    'If consensus is narrower than claimed, the constraint''s coordination function weakens (more interpretive space = less extraction of agility). If consensus is enforced beyond historical warrant, extraction rises.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(patristic_consensus_identification, conceptual, 'The operational definition of patristic consensus as a hermeneutic constraint').

omega_variable(
    identity_locked_vs_structural_exit,
    'Is the identity_locked exit option for episcopal/monastic actors structural (canonical penalties, loss of orders) or internalized (vocational self-understanding)? The metric treats them identically but the mechanism matters for classification.',
    'Comparative study of bishops/monastics who left Orthodoxy for Catholicism/Protestantism — did they face structural barriers or vocational crisis?',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression after exit. If structural, the constraint''s suppression is more directly measurable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_vs_structural_exit, empirical, 'Mechanism of identity lock for clerical/monastic beneficiaries').

omega_variable(
    kernel_framing_underdetermination,
    'Does the biblical_authority kernel have one coherent framing (Scripture''s interpretive authority) or two (Scripture''s authority vs. Tradition''s authority)? The conciliar reading fuses them; the sibling readings separate them differently.',
    'Comparative analysis of how each sibling reading structures the kernel''s authority claims — do they disagree on the kernel''s boundaries or on the legitimate reading procedure?',
    'If the kernel is underdetermined (two framings), the sibling relations may shift: conciliar may foreclose sola_scriptura on one framing but coexist on another.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel itself admits a single structural framing or multiple').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__conciliar_reading, 325, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bacr_tr_t325, biblical_authority__conciliar_reading, theater_ratio, 325, 0.12).
narrative_ontology:measurement(bacr_tr_t451, biblical_authority__conciliar_reading, theater_ratio, 451, 0.15).
narrative_ontology:measurement(bacr_tr_t787, biblical_authority__conciliar_reading, theater_ratio, 787, 0.18).
narrative_ontology:measurement(bacr_tr_t1054, biblical_authority__conciliar_reading, theater_ratio, 1054, 0.22).
narrative_ontology:measurement(bacr_tr_t1453, biblical_authority__conciliar_reading, theater_ratio, 1453, 0.25).
narrative_ontology:measurement(bacr_tr_t1724, biblical_authority__conciliar_reading, theater_ratio, 1724, 0.27).
narrative_ontology:measurement(bacr_tr_t2025, biblical_authority__conciliar_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(bacr_be_t325, biblical_authority__conciliar_reading, base_extractiveness, 325, 0.18).
narrative_ontology:measurement(bacr_be_t451, biblical_authority__conciliar_reading, base_extractiveness, 451, 0.22).
narrative_ontology:measurement(bacr_be_t787, biblical_authority__conciliar_reading, base_extractiveness, 787, 0.25).
narrative_ontology:measurement(bacr_be_t1054, biblical_authority__conciliar_reading, base_extractiveness, 1054, 0.28).
narrative_ontology:measurement(bacr_be_t1453, biblical_authority__conciliar_reading, base_extractiveness, 1453, 0.3).
narrative_ontology:measurement(bacr_be_t1724, biblical_authority__conciliar_reading, base_extractiveness, 1724, 0.31).
narrative_ontology:measurement(bacr_be_t2025, biblical_authority__conciliar_reading, base_extractiveness, 2025, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(bacr_su_t325, biblical_authority__conciliar_reading, suppression_requirement, 325, 0.25).
narrative_ontology:measurement(bacr_su_t451, biblical_authority__conciliar_reading, suppression_requirement, 451, 0.3).
narrative_ontology:measurement(bacr_su_t787, biblical_authority__conciliar_reading, suppression_requirement, 787, 0.35).
narrative_ontology:measurement(bacr_su_t1054, biblical_authority__conciliar_reading, suppression_requirement, 1054, 0.38).
narrative_ontology:measurement(bacr_su_t1453, biblical_authority__conciliar_reading, suppression_requirement, 1453, 0.4).
narrative_ontology:measurement(bacr_su_t1724, biblical_authority__conciliar_reading, suppression_requirement, 1724, 0.41).
narrative_ontology:measurement(bacr_su_t2025, biblical_authority__conciliar_reading, suppression_requirement, 2025, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__conciliar_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_authority__conciliar_reading, 0.08).
narrative_ontology:affects_constraint(biblical_authority__conciliar_reading, biblical_authority__sola_scriptura_reading).
narrative_ontology:affects_constraint(biblical_authority__conciliar_reading, biblical_authority__tradition_scripture_reading).
narrative_ontology:affects_constraint(biblical_authority__conciliar_reading, orthodox_synodical_governance).
narrative_ontology:affects_constraint(biblical_authority__conciliar_reading, orthodox_sacramental_theology).
narrative_ontology:affects_constraint(biblical_authority__conciliar_reading, orthodox_ecumenical_relations).

% DUAL FORMULATION NOTE:
% Biblical authority kernel family: three readings (conciliar, sola_scriptura, tradition_scripture) with distinct ε values and beneficiary/victim structures. Conciliar reading: moderate ε (0.32), episcopal beneficiaries, adaptation victims. Sola scriptura reading: low ε (0.15), individual conscience beneficiaries, fragmentation victims. Tradition-scripture reading: higher ε (0.45), magisterial beneficiaries, local church victims. Linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_authority__conciliar_reading, institutional, 0.15).
constraint_indexing:directionality_override(biblical_authority__conciliar_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
