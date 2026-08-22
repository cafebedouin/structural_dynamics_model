% ============================================================================
% CONSTRAINT STORY: biblical_authority__conciliar_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: biblical_authority__conciliar_reading
 *   human_readable: Conciliar Reading: Scripture Through the Seven Ecumenical Councils and Patristic Consensus
 *   domain: theology/history_of_christianity
 *
 * SUMMARY:
 *   This story authors the conciliar reading of the biblical-authority
 *   kernel: scripture is interpreted through the seven ecumenical councils
 *   (325–787 CE) and the ongoing witness of the Church Fathers, understood as
 *   living tradition rather than a formal magisterial teaching office. Unlike
 *   the sola scriptura reading (scripture alone, self-interpreting) and the
 *   tradition-scripture reading (a standing magisterium that actively guards
 *   and can further define the deposit of faith), this reading locates
 *   authority in a horizontal, collegial memory: no single interpreter can
 *   overrule conciliar consensus, and doctrinal development happens, if at
 *   all, through reception across the whole communion over long stretches of
 *   time rather than through a magisterial pronouncement. The extraction here
 *   is real but moderate and diffuse — episcopal collegiality and the
 *   patristic corpus function as gatekeepers of legitimate interpretation,
 *   benefiting the episcopal college and the autocephalous hierarchies that
 *   operate within its boundaries, while the primary cost falls on those
 *   needing doctrinal responsiveness to circumstances the ancient councils
 *   did not anticipate.
 *
 * KEY AGENTS:
 *   - episcopal_college: primary agenda-setter (institutional/arbitrage) — administers conciliar reception without a single juridical head
 *   - patristic_theological_tradition: primary beneficiary (institutional/analytical) — the normative filter invoked to authorize or reject doctrinal claims
 *   - autocephalous_church_hierarchies: secondary beneficiary/agenda-setter (institutional/constrained) — local autonomy protected by, and bounded by, the shared conciliar framework
 *   - laity_seeking_doctrinal_change, reform_minded_clergy, diaspora_communities_needing_new_pastoral_answers: bear the cost of interpretive lag and disciplinary risk
 *   - ecumenical_councils_themselves: non-agent kernel artifact — the fixed historical reference point the whole reading is built on
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__conciliar_reading, 0.44).
domain_priors:suppression_score(biblical_authority__conciliar_reading, 0.4).
domain_priors:theater_ratio(biblical_authority__conciliar_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__conciliar_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__conciliar_reading, "Conciliar Reading: Scripture Through the Seven Ecumenical Councils and Patristic Consensus").
narrative_ontology:topic_domain(biblical_authority__conciliar_reading, "theology/history_of_christianity").

domain_priors:requires_active_enforcement(biblical_authority__conciliar_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__conciliar_reading, '1639d058-dcf9-43af-b7d9-597c10db1150').
narrative_ontology:cs_kernel_codification('1639d058-dcf9-43af-b7d9-597c10db1150', distributed).
narrative_ontology:cs_authority_grounding('1639d058-dcf9-43af-b7d9-597c10db1150', practice).
narrative_ontology:cs_interpretation_layer_present('1639d058-dcf9-43af-b7d9-597c10db1150').
narrative_ontology:cs_reading_relation('1639d058-dcf9-43af-b7d9-597c10db1150', biblical_authority__sola_scriptura_reading, coexists_with).
narrative_ontology:cs_reading_relation('1639d058-dcf9-43af-b7d9-597c10db1150', biblical_authority__tradition_scripture_reading, influences).
narrative_ontology:cs_axiom('1639d058-dcf9-43af-b7d9-597c10db1150', foundational, conciliar_reception_not_magisterial_decree).
narrative_ontology:cs_axiom_status(conciliar_reception_not_magisterial_decree, holdable).
narrative_ontology:cs_axiom_grounding('1639d058-dcf9-43af-b7d9-597c10db1150', conciliar_reception_not_magisterial_decree, conventional).
narrative_ontology:cs_axiom('1639d058-dcf9-43af-b7d9-597c10db1150', foundational, no_single_bishop_can_overrule_ecumenical_consensus).
narrative_ontology:cs_axiom_status(no_single_bishop_can_overrule_ecumenical_consensus, holdable).
narrative_ontology:cs_axiom_grounding('1639d058-dcf9-43af-b7d9-597c10db1150', no_single_bishop_can_overrule_ecumenical_consensus, conventional).
narrative_ontology:cs_axiom('1639d058-dcf9-43af-b7d9-597c10db1150', secondary, doctrine_develops_by_generational_reception_not_formal_definition).
narrative_ontology:cs_axiom_status(doctrine_develops_by_generational_reception_not_formal_definition, holdable).
narrative_ontology:cs_axiom_grounding('1639d058-dcf9-43af-b7d9-597c10db1150', doctrine_develops_by_generational_reception_not_formal_definition, instrumental).
narrative_ontology:cs_reference_frame('1639d058-dcf9-43af-b7d9-597c10db1150', seven_ecumenical_councils_consensus_patrum).
narrative_ontology:cs_drift_state('1639d058-dcf9-43af-b7d9-597c10db1150', contemporary_global_orthodoxy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1639d058-dcf9-43af-b7d9-597c10db1150', '').
narrative_ontology:cs_kernel_id(biblical_authority__conciliar_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, episcopal_college).
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, patristic_theological_tradition).
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, autocephalous_church_hierarchies).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, laity_seeking_doctrinal_change).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, reform_minded_clergy).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, diaspora_communities_needing_new_pastoral_answers).
narrative_ontology:constraint_vindicates(biblical_authority__conciliar_reading, consensus_patrum_doctrine).
narrative_ontology:constraint_vindicates(biblical_authority__conciliar_reading, conciliar_infallibility_of_ecumenical_councils).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops in communion adjudicate what counts as authentic reception of the seven ecumenical councils and the patristic consensus (consensus patrum). They convene synods, discipline clergy who deviate, and hold that no single bishop (including any patriarch) can overrule conciliar consensus. Their authority is collective and horizontal rather than vertical, and they administer this without a single juridical head who could unilaterally change the arrangement.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, episcopal_college, agenda_setter,
    institutional, civilizational, arbitrage, continental).

% The corpus of Church Fathers (Athanasius, the Cappadocians, Maximus, et al.) functions as a normative filter: any new theological proposal is tested against whether the Fathers 'received' something like it. This tradition is not a governing body but is invoked constantly as the measure of orthodoxy, and its authority is vindicated every time a proposal is rejected for lacking patristic warrant.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, patristic_theological_tradition, beneficiary,
    institutional, civilizational, analytical, continental).

% National and regional churches (Greek, Russian, Antiochian, Serbian, etc.) govern themselves independently within shared conciliar boundaries. Each hierarchy benefits from local administrative autonomy but is bound not to unilaterally revise doctrine outside conciliar consensus without risking schism from the wider communion — a structural brake that also protects each hierarchy's own local authority from central override.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, autocephalous_church_hierarchies, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(biblical_authority__conciliar_reading, autocephalous_church_hierarchies, agenda_setter).

% Ordinary believers who want the Church to address contemporary questions (remarriage, contraception, women's roles, bioethics) directly encounter a system that treats absence of patristic precedent as evidence against change rather than as an open question. Their pastoral needs are addressed case-by-case through economia (pastoral flexibility) rather than through doctrinal revision, which resolves individual hardship without ever revisiting the underlying rule.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, laity_seeking_doctrinal_change, payer,
    powerless, biographical, constrained, local).

% Priests and theologians who argue a doctrine should develop beyond patristic-era formulations face the charge of ecclesiological innovation (an accusation with real disciplinary teeth — suspension, defrocking, or loss of standing). Exit means leaving Orthodoxy entirely, since the conciliar framework has no internal mechanism analogous to a magisterial doctrinal development process that could formally ratify their position from within.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, reform_minded_clergy, payer,
    moderate, biographical, trapped, national).

% Orthodox communities living in modern pluralist, diaspora contexts (intermarriage, secular legal regimes, bioethical dilemmas unknown to the Fathers) must apply patristic-era categories to situations the councils never anticipated. They bear the cost of interpretive lag: waiting years or generations for a synodal answer, or receiving inconsistent local answers from different autocephalous jurisdictions.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, diaspora_communities_needing_new_pastoral_answers, payer,
    powerless, biographical, constrained, regional).

% The seven ecumenical councils (325–787 CE) are the fixed historical reference points the whole system reads scripture through. They are not an actor but the kernel artifact itself — cited, never convened again in a form the whole communion universally recognizes as having equal authority.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, ecumenical_councils_themselves, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(biblical_authority__conciliar_reading, ecumenical_councils_themselves).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_authority__conciliar_reading, episcopal_college).
narrative_ontology:fixing_cost_class(biblical_authority__conciliar_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, non-papal mechanism for the whole Church to agree on what scripture means on contested questions (the nature of Christ, the Trinity, icon veneration) without requiring a single juridical authority — solving the genuine problem of doctrinal fragmentation across a communion with no central government.
% TRANSFER_FUNCTION: Moves interpretive authority away from individual bishops, theologians, or laity and concentrates it in the collegial memory of councils-plus-Fathers; moves the cost of doctrinal stasis onto those who need answers to questions the ancient councils did not address.
% ABSENT_VOICES: Reform-minded theologians proposing doctrinal development are heard but structurally disfavored by a system that treats novelty itself as suspect; diaspora laity with modern pastoral dilemmas have no formal channel to petition for a new ecumenical council, since none has been convened as universally binding in over a millennium.
% DISAPPEARANCE_RATIONALE: If the conciliar-patristic framework vanished, Orthodox doctrinal identity would fragment rapidly along national or personal lines; the autocephalous structure itself, which depends on shared conciliar boundaries to remain in communion rather than becoming separate churches, would lose its unifying reference and likely splinter further.
% FOUNDING_PROBLEM: The early Church faced genuine christological and trinitarian controversies (Arianism, Nestorianism, Monophysitism, iconoclasm) that threatened to fracture Christian communities; ecumenical councils were convened to produce binding, communion-wide resolutions without a single bishop claiming universal jurisdiction.
% FOUNDING_PROBLEM_CORROBORATION: Historians of late antiquity and Byzantine church historians outside the Orthodox communion (e.g. scholars working from Roman Catholic, Protestant, and secular academic seats) corroborate that the original councils solved acute, specific doctrinal crises; the same scholars note the founding problem (active christological schism) has been resolved for centuries, while reform-minded Orthodox theologians (e.g., figures associated with modern Orthodox theological renewal movements) argue from inside the tradition that the mechanism now functions primarily to block reconsideration of settled questions rather than to resolve live ones.
narrative_ontology:disappearance_verdict(biblical_authority__conciliar_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__conciliar_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__conciliar_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_authority__conciliar_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__conciliar_reading, 0.44, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.44) sits moderate — lower than a magisterial reading with a single teaching authority that can be captured, but non-trivial because the consensus patrum functions as a real gatekeeping mechanism with disciplinary consequences (suspension, loss of clerical standing) for those who press doctrinal development. Suppression (0.4) is likewise moderate: there is no single office to coerce compliance, but the informal charge of 'innovation' functions as social and institutional suppression across autocephalous churches. Theater ratio (0.3) reflects that most conciliar invocation is functionally real (genuine doctrinal boundary-maintenance) with a growing performative layer as councils are cited more as rhetorical trump cards than as live deliberative bodies (none has met with universally recognized ecumenical authority in over a millennium). Accessibility collapse (0.5) is moderate: alternative interpretive paths (private judgment, magisterial development) are foreclosed within Orthodoxy specifically, but exit to other Christian traditions remains fully available, unlike a true mountain. Resistance (0.45) reflects real friction — reform movements exist and are vocal — but rarely succeed in altering the framework itself.
 *
 * DIRECTIONALITY LOGIC:
 *   The episcopal college and patristic tradition sit near the beneficiary end: they administer and are vindicated by the framework without individually bearing its costs, and their institutional exit options (arbitrage, analytical distance from any single ruling) insulate them from the consequences of doctrinal stasis. Autocephalous hierarchies are a genuine dual case — they benefit from local autonomy the conciliar boundary protects, but are simultaneously constrained by that same boundary from unilateral action, hence the secondary agenda_setter role. Laity, reform clergy, and diaspora communities carry the transfer: they need answers the framework is structurally slow or unable to give, and their exit options range from constrained (accept economia-style pastoral workarounds) to trapped (reform clergy who would lose ecclesiastical standing entirely by leaving).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — active christological and trinitarian schism threatening to fracture the early Church — is genuinely dead as an acute crisis; no faction today is on the verge of splitting over the substance of Nicene trinitarianism. But the mechanism built to solve that dead crisis (binding conciliar consensus plus patristic reception) persists and is now applied to a different class of problem (modern pastoral and bioethical questions) it was never built to answer, which is where the tangled_rope reading differs from a pure mountain or pure rope: the coordination function (preventing schism) is still real and valuable, but it now also functions as an extraction mechanism against those needing doctrinal responsiveness the mechanism structurally resists providing. This is corroborated from outside the beneficiary set by academic historians of the councils themselves, which keeps the founding-problem narrative from being self-serving.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conciliar_reading_vs_sibling_readings_kernel_location,
    'Is the disagreement between the conciliar, sola scriptura, and tradition-scripture readings located in WHO holds interpretive authority (a person/office vs. a collegial body vs. the text itself), or in WHETHER doctrine can develop at all beyond apostolic-era deposit?',
    'Compare each reading''s own account of doctrinal development: the magisterial reading affirms formal development (e.g., Marian dogmas defined centuries after the councils); the conciliar reading denies formal development but permits generational reception; sola scriptura denies any authoritative extra-scriptural development. The disagreement is located primarily in the locus-of-authority axis, with the development question following from it.',
    'If the disagreement is fundamentally about locus of authority, the readings coexist as live ecclesiological options across different communions rather than one refuting the others empirically; if it were about a testable historical claim, one reading could in principle be shown false.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conciliar_reading_vs_sibling_readings_kernel_location, conceptual, 'Where within the biblical_authority kernel the conciliar/sola scriptura/tradition-scripture readings actually diverge.').

omega_variable(
    coordination_extraction_separability_conciliar,
    'Is the schism-prevention coordination function of ecumenical conciliarism separable from its gatekeeping effect on modern doctrinal development, or are they the same mechanism operating on different questions?',
    'Examine cases where autocephalous churches have addressed genuinely novel pastoral questions (e.g., bioethical rulings, calendar reform) without full ecumenical consensus: if communion held without splintering, the functions are separable in practice even if not in official theory.',
    'If separable, the extractive gatekeeping on new questions is not required to preserve the coordination function, and a lower-extraction version of conciliarism is structurally available; if inseparable, the extraction is close to the coordination cost itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability_conciliar, empirical, 'Whether the schism-preventing and development-blocking functions of conciliar authority can be structurally disentangled.').

omega_variable(
    fragmentation_is_feature_or_defect,
    'Is the moderate fragmentation across autocephalous churches (each interpreting conciliar consensus somewhat independently) a stabilizing feature of the conciliar reading, or an early sign of the same authority erosion that eventually produces schism?',
    'Track whether inter-autocephalous disagreements (e.g., over calendar, jurisdiction, or recent pastoral rulings) resolve through renewed pan-Orthodox consultation or harden into permanent, unrecognized breaks in communion over multi-decade horizons.',
    'If disagreements resolve, fragmentation is a feature consistent with living tradition; if they harden, the conciliar reading''s claimed unity is increasingly nominal and its extraction profile understates real fragmentation costs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fragmentation_is_feature_or_defect, empirical, 'Whether autocephalous fragmentation under conciliar reading is stable pluralism or incipient schism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__conciliar_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_authority__conciliar_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(bibl_tr_t0, observed).
narrative_ontology:measurement(bibl_tr_t20, biblical_authority__conciliar_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement_basis(bibl_tr_t20, observed).
narrative_ontology:measurement(bibl_tr_t40, biblical_authority__conciliar_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(bibl_tr_t40, observed).
narrative_ontology:measurement(bibl_tr_t60, biblical_authority__conciliar_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement_basis(bibl_tr_t60, observed).
narrative_ontology:measurement(bibl_tr_t80, biblical_authority__conciliar_reading, theater_ratio, 80, 0.26).
narrative_ontology:measurement_basis(bibl_tr_t80, observed).
narrative_ontology:measurement(bibl_tr_t100, biblical_authority__conciliar_reading, theater_ratio, 100, 0.3).
narrative_ontology:measurement_basis(bibl_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_authority__conciliar_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement_basis(bibl_be_t0, observed).
narrative_ontology:measurement(bibl_be_t20, biblical_authority__conciliar_reading, base_extractiveness, 20, 0.25).
narrative_ontology:measurement_basis(bibl_be_t20, observed).
narrative_ontology:measurement(bibl_be_t40, biblical_authority__conciliar_reading, base_extractiveness, 40, 0.32).
narrative_ontology:measurement_basis(bibl_be_t40, observed).
narrative_ontology:measurement(bibl_be_t60, biblical_authority__conciliar_reading, base_extractiveness, 60, 0.38).
narrative_ontology:measurement_basis(bibl_be_t60, observed).
narrative_ontology:measurement(bibl_be_t80, biblical_authority__conciliar_reading, base_extractiveness, 80, 0.41).
narrative_ontology:measurement_basis(bibl_be_t80, observed).
narrative_ontology:measurement(bibl_be_t100, biblical_authority__conciliar_reading, base_extractiveness, 100, 0.44).
narrative_ontology:measurement_basis(bibl_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_authority__conciliar_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(bibl_su_t0, observed).
narrative_ontology:measurement(bibl_su_t20, biblical_authority__conciliar_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement_basis(bibl_su_t20, observed).
narrative_ontology:measurement(bibl_su_t40, biblical_authority__conciliar_reading, suppression_requirement, 40, 0.3).
narrative_ontology:measurement_basis(bibl_su_t40, observed).
narrative_ontology:measurement(bibl_su_t60, biblical_authority__conciliar_reading, suppression_requirement, 60, 0.33).
narrative_ontology:measurement_basis(bibl_su_t60, observed).
narrative_ontology:measurement(bibl_su_t80, biblical_authority__conciliar_reading, suppression_requirement, 80, 0.37).
narrative_ontology:measurement_basis(bibl_su_t80, observed).
narrative_ontology:measurement(bibl_su_t100, biblical_authority__conciliar_reading, suppression_requirement, 100, 0.4).
narrative_ontology:measurement_basis(bibl_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__conciliar_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_authority__conciliar_reading, 0.1).
narrative_ontology:affects_constraint(biblical_authority__conciliar_reading, sola_scriptura_reading).
narrative_ontology:affects_constraint(biblical_authority__conciliar_reading, tradition_scripture_reading).

% DUAL FORMULATION NOTE:
% Part of the biblical_authority kernel family. sola_scriptura_reading denies any authoritative interpretive layer beyond the text itself (lowest claimed extraction, highest fragmentation risk from disagreement over private judgment). tradition_scripture_reading vests interpretive authority in a standing magisterium capable of further doctrinal definition (higher, more centralized clerical extraction, lower fragmentation, faster doctrinal responsiveness). conciliar_reading occupies a structural middle: episcopal rather than papal extraction, moderate fragmentation via autocephaly, and slow, reception-based rather than declarative doctrinal change. Each reading has its own ε, beneficiary/victim structure, and classification per DP-001; they are linked here, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
