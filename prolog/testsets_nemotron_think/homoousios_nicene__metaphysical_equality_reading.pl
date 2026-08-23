% ============================================================================
% CONSTRAINT STORY: homoousios_nicene__metaphysical_equality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_nicene__metaphysical_equality_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: homoousios_nicene__metaphysical_equality_reading
 *   human_readable: Homoousios as Metaphysical Equality of Father and Son
 *   domain: historical_theology/ecclesiastical_history/philosophy_of_religion
 *
 * SUMMARY:
 *   The Council of Nicaea (325 CE) adopted homoousios ('consubstantial') to
 *   define the Son's relationship to the Father: same divine essence (ousia),
 *   co-eternal, no subordination in being. This reading presents the formula
 *   as a metaphysical truth about God's nature — a Mountain claim of
 *   ontological necessity. Structurally, however, it operates as a enforced
 *   boundary: the Nicene orthodox bishops and imperial authority benefit from
 *   the interpretive power and institutional unity it secures; Arian,
 *   subordinationist, and homoiousian parties are anathematized, exiled, and
 *   suppressed. The constraint requires active enforcement (imperial law,
 *   conciliar canons, episcopal discipline) and shows high suppression of
 *   alternatives. The claim/metric divergence is deliberate: the reading
 *   claims Mountain (metaphysical necessity) while the authored metrics
 *   describe a heavily enforced, extractive boundary — the engine measures
 *   this divergence; do not reconcile the claim to the metrics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_nicene__metaphysical_equality_reading, 0.75).
domain_priors:suppression_score(homoousios_nicene__metaphysical_equality_reading, 0.85).
domain_priors:theater_ratio(homoousios_nicene__metaphysical_equality_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_nicene__metaphysical_equality_reading, mountain).
narrative_ontology:human_readable(homoousios_nicene__metaphysical_equality_reading, "Homoousios as Metaphysical Equality of Father and Son").
narrative_ontology:topic_domain(homoousios_nicene__metaphysical_equality_reading, "historical_theology/ecclesiastical_history/philosophy_of_religion").

domain_priors:requires_active_enforcement(homoousios_nicene__metaphysical_equality_reading).
domain_priors:emerges_naturally(homoousios_nicene__metaphysical_equality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_nicene__metaphysical_equality_reading, '007f3bb6-02ac-442f-aab1-5a5a17bd5491').
narrative_ontology:cs_kernel_codification('007f3bb6-02ac-442f-aab1-5a5a17bd5491', formalized).
narrative_ontology:cs_authority_grounding('007f3bb6-02ac-442f-aab1-5a5a17bd5491', lineage).
narrative_ontology:cs_interpretation_layer_present('007f3bb6-02ac-442f-aab1-5a5a17bd5491').
narrative_ontology:cs_reading_relation('007f3bb6-02ac-442f-aab1-5a5a17bd5491', homoousios_nicene__subordinationist_reading, forecloses).
narrative_ontology:cs_reading_relation('007f3bb6-02ac-442f-aab1-5a5a17bd5491', homoousios_nicene__honorific_similarity_reading, forecloses).
narrative_ontology:cs_axiom('007f3bb6-02ac-442f-aab1-5a5a17bd5491', foundational, homoousios_entails_ontological_identity).
narrative_ontology:cs_axiom_status(homoousios_entails_ontological_identity, holdable).
narrative_ontology:cs_axiom_grounding('007f3bb6-02ac-442f-aab1-5a5a17bd5491', homoousios_entails_ontological_identity, deontological).
narrative_ontology:cs_axiom('007f3bb6-02ac-442f-aab1-5a5a17bd5491', secondary, conciliar_definition_binds_conscience).
narrative_ontology:cs_axiom_status(conciliar_definition_binds_conscience, holdable).
narrative_ontology:cs_axiom_grounding('007f3bb6-02ac-442f-aab1-5a5a17bd5491', conciliar_definition_binds_conscience, conventional).
narrative_ontology:cs_reference_frame('007f3bb6-02ac-442f-aab1-5a5a17bd5491', nicene_conciliar_settlement).
narrative_ontology:cs_drift_state('007f3bb6-02ac-442f-aab1-5a5a17bd5491', post_chalcedonian_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('007f3bb6-02ac-442f-aab1-5a5a17bd5491', '').
narrative_ontology:cs_kernel_id(homoousios_nicene__metaphysical_equality_reading, homoousios_nicene).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, nicene_orthodox_bishops).
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, imperial_authority).
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, episcopal_hierarchy).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, arian_bishops).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, subordinationist_theologians).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, homoiousian_party).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, dissenting_clergy).
narrative_ontology:constraint_vindicates(homoousios_nicene__metaphysical_equality_reading, trinitarian_orthodoxy).
narrative_ontology:constraint_vindicates(homoousios_nicene__metaphysical_equality_reading, christological_equality).
narrative_ontology:constraint_vindicates(homoousios_nicene__metaphysical_equality_reading, conciliar_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Champions of the homoousios formula at Nicaea and after; gain interpretive authority over christological orthodoxy, control of episcopal appointments, and imperial patronage. Their theological identity is fused with the formula — abandoning it would dissolve their authority. Exit means theological and institutional suicide.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, nicene_orthodox_bishops, beneficiary,
    institutional, generational, identity_locked, continental).

% Constantine and successors convene and enforce conciliar decisions; use church unity as political cement. The homoousios settlement gives the emperor a unified church backing imperial authority. Can shift support between theological factions (as Constantius II did) but needs some doctrinal settlement for stability.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, imperial_authority, agenda_setter,
    institutional, generational, arbitrage, continental).

% Conciliar authority distributes interpretive power to the collective episcopate; metropolitans and patriarchs gain jurisdiction defined by adherence to Nicene faith. Individual bishops can resist (as Athanasius did) but the hierarchy as a structure benefits from the formula's enforcement.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, episcopal_hierarchy, beneficiary,
    organized, generational, constrained, continental).

% Led by Arius, Eusebius of Nicomedia; teach Son is created, subordinate in being. Anathematized at Nicaea, exiled, churches suppressed. Some regain imperial favor under Constantius but the homoousios boundary structurally excludes them from legitimate episcopacy. Exit requires recantation — theological identity prevents it.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, arian_bishops, payer,
    organized, biographical, trapped, continental).

% Hold that Son derives being from Father, shares divinity but not equality (Origenist tradition, later 'Arian' spectrum). The metaphysical equality reading renders their position heretical by definition. Can modify language but cannot accept homoousios as ontological identity without abandoning their system.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, subordinationist_theologians, payer,
    moderate, biographical, constrained, continental).

% Semi-Arian position (Basil of Ancyra, etc.): Son is homoiousios (like substance) not homoousios. Attempt middle ground but the metaphysical equality reading forecloses this — 'like' is not 'same'. Eventually absorbed into Nicene orthodoxy or suppressed; their compromise position is structurally unstable.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, homoiousian_party, payer,
    organized, biographical, constrained, continental).

% Parish priests, monks, laity who resist the formula or cannot articulate it. Forced to conform by episcopal visitation, imperial law, social pressure. No theological exit — only conformity or persecution. Bear the enforcement cost without theological agency.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, dissenting_clergy, payer,
    powerless, immediate, trapped, local).

% Patristic scholars, historians of doctrine, systematic theologians analyzing the constraint from outside the enforcement structure. See the metaphysical claim, the political enforcement, and the victim structure simultaneously. No material stake in the outcome.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, theological_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unified christological confession enabling ecclesiastical communion across the Roman Empire; resolved the Arian controversy by providing a single formula that could be enforced as the boundary of orthodoxy.
% TRANSFER_FUNCTION: Moves interpretive authority and institutional legitimacy from dissenting christological positions to the Nicene orthodox episcopate; moves episcopal jurisdiction and imperial patronage to those who subscribe to homoousios; moves the cost of non-conformity (exile, deposition, anathema) onto Arian and subordinationist parties.
% ABSENT_VOICES: Gothic and Germanic Arian churches (Ulfilas' mission) operating outside imperial enforcement; Jewish and pagan critics who rejected christological formulation entirely; monastic communities in Egypt and Syria who resisted both Arian and Nicene imperial theology; later Miaphysite communities who saw homoousios as Nestorianizing.
% DISAPPEARANCE_RATIONALE: If the homoousios-as-metaphysical-equality constraint vanished overnight, the entire Nicene-Constantinopolitan Trinitarian framework would lose its ontological foundation; the Chalcedonian christological settlement (which depends on homoousios applied to Christ's natures) would collapse; the episcopal hierarchy's conciliar authority would lose its doctrinal warrant; Arian, subordinationist, or modalist christologies would restructure the church's confession and imperial relationship.
% FOUNDING_PROBLEM: The Arian controversy (c. 318-325) threatened to split the church and destabilize Constantine's newly unified empire: Arius taught the Son was created and subordinate, while Alexander of Alexandria insisted on the Son's eternal generation and full divinity. No existing formula could reconcile the parties — the empire needed a single confession to secure ecclesiastical and political unity.
% FOUNDING_PROBLEM_CORROBORATION: Nicene fathers (Athanasius, Hilary of Poitiers) attest the Arian threat was real and the formula necessary; Arian sources (via Athanasius' polemics) and modern historians (Hanson, Ayres, Lienhard) attest the formulation created new philosophical problems (how can two be one ousia?) and the enforcement created victims; the homoiousian compromise attempt shows the founding problem was not cleanly solved by Nicaea.
narrative_ontology:disappearance_verdict(homoousios_nicene__metaphysical_equality_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_nicene__metaphysical_equality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_nicene__metaphysical_equality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(homoousios_nicene__metaphysical_equality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_nicene__metaphysical_equality_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_nicene__metaphysical_equality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_nicene__metaphysical_equality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, ExtMetricName, E),
    domain_priors:suppression_score(homoousios_nicene__metaphysical_equality_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(homoousios_nicene__metaphysical_equality_reading),
    narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(homoousios_nicene__metaphysical_equality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75) because the formula transfers interpretive authority, episcopal jurisdiction, and imperial patronage to the Nicene party while imposing exile, deposition, and anathema on dissenters. Suppression is very high (0.85) because persistence depends on actively excluding alternative christologies through state power, not theological persuasion alone. Theater ratio is moderate (0.40) — the theological debates are genuine and philosophically sophisticated, but a substantial portion of conciliar and imperial activity serves to defend the boundary rather than explore the mystery. Accessibility collapse is high (0.82) — once the formula is accepted as metaphysical truth, alternatives appear not just wrong but impossible. Resistance remains significant (0.60) — the Arian controversy persisted for decades, homoiousian compromise attempted, and enforcement required repeated councils and imperial turnover.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (Nicene bishops, hierarchy) experience the constraint as genuine coordination — a theological truth that unifies the church. The payer seats (Arians, subordinationists, dissenting clergy) experience it as enforced extraction — a political boundary imposed by imperial power. The engine computes this divergence from the structural data: same constraint, opposite classifications from different seats. The authored claim (mountain) reflects the beneficiary seat's self-understanding; the metrics reflect the payer seat's reality.
 *
 * DIRECTIONALITY LOGIC:
 *   The Nicene orthodox bishops and episcopal hierarchy are structural beneficiaries (d near 0.0-0.2): they collect interpretive authority, jurisdictional privilege, and imperial favor. Imperial authority is agenda-setter with arbitrage exit (d ~0.15): it can shift between factions but needs a settlement. Arian bishops are trapped targets (d near 1.0): identity-locked, no exit without recantation. Subordinationist theologians and homoiousians are constrained targets (d ~0.7-0.85): they can modify language but the metaphysical equality reading structurally forecloses their core premise. Dissenting clergy are powerless trapped (d ~0.95): no theological agency, only conformity or persecution. Theological observers are analytical (d=0.5): see the full structure without material stake.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was founded to solve the Arian controversy (ecclesiastical unity, imperial stability). That problem is contested: Nicenes say Arianism persists as eternal heresy; Arians say the formula misdiagnosed the problem; historians say Nicaea created new problems (philosophical incoherence of 'one ousia in three hypostases', enforcement victims). The mandate has not atrophied — the formula remains the Trinitarian boundary — but its function shifted from solving a specific controversy to constituting the ontology of Christian orthodoxy. The extraction (episcopal authority, exclusion of alternatives) persists because the constraint became the identity of the institution, not because the founding problem remains live in its original form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_boundary,
    'Is homoousios as metaphysical equality a genuine ontological truth about God (Mountain) or a constructed doctrinal boundary that benefits the Nicene party (false summit Mountain / Tangled Rope)?',
    'Comparative analysis of pre-Nicene theology (Origen, Dionysius of Alexandria) showing whether ''homoousios'' was used as metaphysical identity or honorific similarity; historical analysis of Constantine''s and Ossius'' role in imposing the term; philosophical analysis of whether ''one ousia in three hypostases'' is coherent or a political compromise.',
    'If genuine metaphysical truth, the constraint is Mountain despite beneficiaries (FSM does not fire). If constructed boundary, it is Tangled Rope (coordination + extraction) or Snare (if coordination is pretext). The FSM signature evaluates any Mountain with declared beneficiaries — this omega documents the irreducible ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_boundary, conceptual, 'Whether the metaphysical equality reading describes reality or constructs a boundary').

omega_variable(
    philosophical_coherence_of_homoousios,
    'Is the claim ''Father and Son are one ousia yet two hypostases'' philosophically coherent, or does it require the enforcement structure to maintain plausibility?',
    'Systematic engagement with fourth-century philosophical theology (Neoplatonic ontology, Cappadocian distinction of ousia/hypostasis); modern analytic theology assessments of Trinitarian coherence; comparison with alternative formulations (Social Trinitarianism, Latin psychological analogy).',
    'If coherent, the coordination function is genuine and extraction may be the price of truth. If incoherent, the enforcement structure is doing the work the philosophy cannot — supporting Snare or high-extraction Tangled Rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(philosophical_coherence_of_homoousios, conceptual, 'Whether the metaphysical claim can bear its own weight without enforcement').

omega_variable(
    foreclosure_of_subordinationist_reading,
    'Does the metaphysical equality reading logically foreclose the subordinationist reading, or do they coexist as competing interpretations of the same kernel?',
    'Logical analysis of whether ''Son derives being from Father'' (subordinationist) is compatible with ''Father and Son are homoousios'' (Nicene). The Cappadocians claimed compatibility (eternal generation preserves equality); Arians claimed incompatibility (generation implies subordination). The engine''s forecloses relation requires logical contradiction in any single framework.',
    'If forecloses, the readings cannot coexist in one theological framework — structural displacement is real. If coexists_with, the kernel remains contested without logical resolution — both readings remain live positions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreclosure_of_subordinationist_reading, conceptual, 'Logical relationship between metaphysical equality and subordinationist readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_nicene__metaphysical_equality_reading, 0, 126).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homoousios_nicene__metaphysical_equality_reading_tr_t0, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(homoousios_nicene__metaphysical_equality_reading_tr_t15, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement(homoousios_nicene__metaphysical_equality_reading_tr_t30, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(homoousios_nicene__metaphysical_equality_reading_tr_t45, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 45, 0.42).
narrative_ontology:measurement(homoousios_nicene__metaphysical_equality_reading_tr_t60, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(homoousios_nicene__metaphysical_equality_reading_tr_t75, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 75, 0.38).
narrative_ontology:measurement(homoousios_nicene__metaphysical_equality_reading_tr_t90, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 90, 0.38).
narrative_ontology:measurement(homoousios_nicene__metaphysical_equality_reading_tr_t105, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 105, 0.39).
narrative_ontology:measurement(homoousios_nicene__metaphysical_equality_reading_tr_t126, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 126, 0.4).

% Extraction over time
narrative_ontology:measurement(homoousios_nicene__metaphysical_equality_reading_be_t0, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(homoousios_nicene__metaphysical_equality_reading_be_t15, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(homoousios_nicene__metaphysical_equality_reading_be_t30, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement(homoousios_nicene__metaphysical_equality_reading_be_t45, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 45, 0.75).
narrative_ontology:measurement(homoousios_nicene__metaphysical_equality_reading_be_t60, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 60, 0.76).
narrative_ontology:measurement(homoousios_nicene__metaphysical_equality_reading_be_t75, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 75, 0.75).
narrative_ontology:measurement(homoousios_nicene__metaphysical_equality_reading_be_t90, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 90, 0.74).
narrative_ontology:measurement(homoousios_nicene__metaphysical_equality_reading_be_t105, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 105, 0.74).
narrative_ontology:measurement(homoousios_nicene__metaphysical_equality_reading_be_t126, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 126, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(homoousios_nicene__metaphysical_equality_reading_su_t0, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(homoousios_nicene__metaphysical_equality_reading_su_t15, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 15, 0.8).
narrative_ontology:measurement(homoousios_nicene__metaphysical_equality_reading_su_t30, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 30, 0.85).
narrative_ontology:measurement(homoousios_nicene__metaphysical_equality_reading_su_t45, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 45, 0.88).
narrative_ontology:measurement(homoousios_nicene__metaphysical_equality_reading_su_t60, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 60, 0.85).
narrative_ontology:measurement(homoousios_nicene__metaphysical_equality_reading_su_t75, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 75, 0.83).
narrative_ontology:measurement(homoousios_nicene__metaphysical_equality_reading_su_t90, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 90, 0.82).
narrative_ontology:measurement(homoousios_nicene__metaphysical_equality_reading_su_t105, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 105, 0.83).
narrative_ontology:measurement(homoousios_nicene__metaphysical_equality_reading_su_t126, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 126, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_nicene__metaphysical_equality_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(homoousios_nicene__metaphysical_equality_reading, 0.08).
narrative_ontology:affects_constraint(homoousios_nicene__metaphysical_equality_reading, chalcedonian_dyophysitism).
narrative_ontology:affects_constraint(homoousios_nicene__metaphysical_equality_reading, constantinopolitan_trinitarianism).
narrative_ontology:affects_constraint(homoousios_nicene__metaphysical_equality_reading, cyrilline_miaphysitism).

% DUAL FORMULATION NOTE:
% This constraint (metaphysical_equality_reading) and its siblings (subordinationist_reading, honorific_similarity_reading) form the homoousios_nicene constraint family. The kernel is the Nicene term 'homoousios'; each reading instantiates a different constraint with different ε, beneficiaries, and victims. This reading has high ε (0.75) because it enforces strict identity; honorific_similarity_reading would have lower ε (coordination without strict boundary); subordinationist_reading would have different victim/beneficiary structure. Linked via network.affects_constraints to downstream christological constraints that depend on this reading's metaphysical framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(homoousios_nicene__metaphysical_equality_reading, institutional, 0.15).
constraint_indexing:directionality_override(homoousios_nicene__metaphysical_equality_reading, organized, 0.85).
constraint_indexing:directionality_override(homoousios_nicene__metaphysical_equality_reading, moderate, 0.8).
constraint_indexing:directionality_override(homoousios_nicene__metaphysical_equality_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
