% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__trinitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_divine_nature__trinitarian_reading, []).

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
 *   constraint_id: biblical_divine_nature__trinitarian_reading
 *   human_readable: Trinitarian Orthodoxy — Essence-Unity Enforcement
 *   domain: theological/religious_authority
 *
 * SUMMARY:
 *   The trinitarian reading of the biblical divine nature kernel
 *   (biblical_divine_nature__trinitarian_reading) instantiates the constraint
 *   that three hypostases share one ousia, enforced through the homoousios
 *   formula and anathema machinery. This is ONE reading of a contested
 *   kernel; sibling readings (modalist_reading, unitarian_reading)
 *   instantiate different constraints with different victim sets and
 *   enforcement structures. The trinitarian reading achieved imperial
 *   enforcement at Nicea (325) and Constantinople (381), becoming the sole
 *   legal christology in the Roman Empire. Its persistence across 1700 years
 *   — through schism, reformation, toleration, and secularization — is
 *   maintained by institutional inertia, ecumenical recognition regimes, and
 *   the structural exclusion of non-Trinitarian groups from the definition of
 *   'Christian.' The constraint extracts conformity (creedal assent,
 *   episcopal submission, liturgical compliance) from all who seek
 *   recognition within the catholic/orthodox/protestant mainstream. The
 *   victim set shifts across epochs but the extraction structure is stable:
 *   non-conformity to the essence-unity formula excludes you from the
 *   sacramental, social, and political goods the arrangement controls.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__trinitarian_reading, 0.82).
domain_priors:suppression_score(biblical_divine_nature__trinitarian_reading, 0.91).
domain_priors:theater_ratio(biblical_divine_nature__trinitarian_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__trinitarian_reading, snare).
narrative_ontology:human_readable(biblical_divine_nature__trinitarian_reading, "Trinitarian Orthodoxy — Essence-Unity Enforcement").
narrative_ontology:topic_domain(biblical_divine_nature__trinitarian_reading, "theological/religious_authority").

domain_priors:requires_active_enforcement(biblical_divine_nature__trinitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__trinitarian_reading, '92c2d285-f07e-44d3-b27a-a4b2778823d2').
narrative_ontology:cs_kernel_codification('92c2d285-f07e-44d3-b27a-a4b2778823d2', formalized).
narrative_ontology:cs_authority_grounding('92c2d285-f07e-44d3-b27a-a4b2778823d2', lineage).
narrative_ontology:cs_interpretation_layer_present('92c2d285-f07e-44d3-b27a-a4b2778823d2').
narrative_ontology:cs_reading_relation('92c2d285-f07e-44d3-b27a-a4b2778823d2', biblical_divine_nature__modalist_reading, forecloses).
narrative_ontology:cs_reading_relation('92c2d285-f07e-44d3-b27a-a4b2778823d2', biblical_divine_nature__unitarian_reading, coexists_with).
narrative_ontology:cs_axiom('92c2d285-f07e-44d3-b27a-a4b2778823d2', foundational, three_hypostases_one_ousia).
narrative_ontology:cs_axiom_status(three_hypostases_one_ousia, holdable).
narrative_ontology:cs_axiom_grounding('92c2d285-f07e-44d3-b27a-a4b2778823d2', three_hypostases_one_ousia, conventional).
narrative_ontology:cs_axiom('92c2d285-f07e-44d3-b27a-a4b2778823d2', foundational, homoousios_as_exclusionary_boundary).
narrative_ontology:cs_axiom_status(homoousios_as_exclusionary_boundary, holdable).
narrative_ontology:cs_axiom_grounding('92c2d285-f07e-44d3-b27a-a4b2778823d2', homoousios_as_exclusionary_boundary, conventional).
narrative_ontology:cs_reference_frame('92c2d285-f07e-44d3-b27a-a4b2778823d2', nicene_constantinopolitan_settlement).
narrative_ontology:cs_drift_state('92c2d285-f07e-44d3-b27a-a4b2778823d2', contemporary_pluralism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('92c2d285-f07e-44d3-b27a-a4b2778823d2', '').
narrative_ontology:cs_kernel_id(biblical_divine_nature__trinitarian_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__trinitarian_reading, nicene_institutional_hierarchy).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__trinitarian_reading, imperial_state_church).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, arian_christians).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, unitarian_christians).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, oneness_pentecostals).
narrative_ontology:constraint_vindicates(biblical_divine_nature__trinitarian_reading, homoousios_doctrine).
narrative_ontology:constraint_vindicates(biblical_divine_nature__trinitarian_reading, monotheism_through_essence_unity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines, adjudicates, and enforces the homoousios formula through conciliar decrees, creeds, and anathema. Controls episcopal appointments, theological education, and sacramental validity. Collects institutional legitimacy, property, and state patronage from orthodoxy enforcement. Can shift doctrinal emphasis but cannot abandon the trinitarian kernel without institutional suicide.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, nicene_institutional_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Receives political unification and social cohesion from a single imperially enforced creed. The trinitarian formula becomes the test of civic loyalty; dissent is sedition. Gains tax base, military loyalty, and bureaucratic coherence. Cannot easily exit the arrangement without losing the theological glue holding the polity together.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, imperial_state_church, beneficiary,
    institutional, generational, constrained, continental).

% Hold that the Son is begotten, not co-eternal — a distinct divine person subordinate to the Father. Face exile, property confiscation, prohibition from public office, and anathema. Their churches are seized, clergy deposed, texts burned. Exit means recanting their core christology or fleeing beyond imperial reach (Gothic kingdoms, Persia).
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, arian_christians, payer,
    organized, generational, trapped, continental).

% Confess numerical singularity of God — Father alone is God; Son and Spirit are created or functional. In medieval Christendom: executed as heretics (Servetus burned at Geneva 1553). In early modern: imprisoned, exiled, books censored. In modernity: marginalized in mainline denominations, denied ordination, congregations dissolved. No territorial refuge until toleration acts (late 17th c. onward).
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, unitarian_christians, payer,
    powerless, biographical, trapped, regional).

% Affirm modalist reading: Father/Son/Spirit are manifestations of one person (Jesus Name theology). Denied fellowship by trinitarian Pentecostals (Assemblies of God 1916 split). Labeled 'heretics' by mainline and evangelical bodies. Excluded from ecumenical bodies (WCC, NAE). Build parallel institutions — exit is possible but costs their entire ecclesiastical network and family ties.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, oneness_pentecostals, payer,
    moderate, biographical, constrained, global).

% Hold the sequential-modes reading (Sabellius, early Roman modalists, modern Oneness). Would argue the trinitarian formula introduces tritheism and violates biblical monotheism. Never granted conciliar voice after Nicea; their texts survive only in opponent quotations. Structural exclusion is the condition of trinitarian coherence.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, modalist_reading_adherents, excluded,
    organized, generational, constrained, global).

% Hold the numerical-singularity reading (Socinians, early Unitarians, modern Biblical Unitarians). Would argue trinitarianism is philosophical Hellenization, not biblical revelation. Excluded from catholic/orthodox/protestant orthodoxy by shared creedal commitment. Survive in tolerated margins (Transylvania, Poland-Lithuania, England post-1689, America post-1815).
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, unitarian_reading_adherents, excluded,
    moderate, generational, constrained, global).

% Traces the contingency of the homoousios settlement: a Greek philosophical term (ousia/hypostasis) imposed on biblical data (monogenes, proskuneo, shema) under imperial pressure. Notes the victim sets shift across epochs — Arians (4th c.), anti-Nicenes (5th-7th c.), medieval unitarians (12th-16th c.), Oneness Pentecostals (20th c.) — while the enforcement structure persists.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, historical_theologian_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, imperially enforceable formula for divine identity that resolves the christological chaos of the 3rd-4th centuries — one creed, one loyalty test, one ecclesiastical hierarchy. Solves the coordination problem of 'which Jesus is worshipped?' across a fracturing Mediterranean world.
% TRANSFER_FUNCTION: Moves theological authority, ecclesiastical office, imperial patronage, and civic belonging from non-conforming groups to the Nicene hierarchy. Extracts conformity through anathema: the price of inclusion is confessing the homoousios; the cost of refusal is exclusion from the sacramental, social, and political order.
% ABSENT_VOICES: The pre-Nicene diversity (subordinationist, modalist, adoptionist, binitarian) that the settlement erased — their texts survive only in fragments quoted by opponents. The Gothic Arian churches (Ulfilas' translation, Visigothic Spain, Vandal Africa) destroyed by Nicene reconquest. The Radical Reformation unitarians (Servetus, Socinus, Polish Brethren) executed or exiled by both Catholic and Protestant establishments. Modern Oneness Pentecostals dismissed as 'cultic' without engagement.
% DISAPPEARANCE_RATIONALE: If the trinitarian enforcement constraint vanished overnight: the Nicene hierarchy would lose its defining boundary and its claim to exclusive sacramental validity; imperial/state churches would lose their theological cohesion; Arian, Unitarian, and Oneness communities would emerge from structural suppression into open contestation; the creedal consensus that underwrites ecumenical recognition would dissolve; Christianity would fracture into christological pluralism resembling the pre-Nicene landscape.
% FOUNDING_PROBLEM: The 3rd-century crisis: multiple incompatible christologies (adoptionist, modalist, subordinationist, logos-theologian) coexisted without a mechanism for resolution. Imperial unity required a single catholic confession; the church required a single rule of faith to adjudicate baptism, eucharist, and episcopal communion. The homoousios formula at Nicea (325) and Constantinople (381) provided that mechanism.
% FOUNDING_PROBLEM_CORROBORATION: The Nicene hierarchy attests the problem is live: christological confusion persists (Mormonism, Jehovah's Witnesses, liberal protestantism denying uniqueness of Christ). Non-Trinitarian historians (Williams 'Arius', Tuggy 'Trinitarianism') attest the founding problem was substantially solved by the 5th century — the creed settled the imperial church's cohesion — and the arrangement's persistence now serves institutional self-preservation. Patristic scholars (Ayres 'Nicaea and its Legacy', Khaled Anatolios) document the formula's contingency: homoousios was a novel, non-biblical term chosen for its exclusionary precision, not its explanatory necessity.
narrative_ontology:disappearance_verdict(biblical_divine_nature__trinitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_divine_nature__trinitarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__trinitarian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(biblical_divine_nature__trinitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_divine_nature__trinitarian_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_divine_nature__trinitarian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_divine_nature__trinitarian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_divine_nature__trinitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the constraint's operation moves immense resources — institutional authority, state patronage, sacramental validity, communal belonging, property, epistemic credibility — from non-Trinitarians to the Nicene hierarchy and its successor bodies. The commission is not marginal cost of coordination; it is the price of admission to the Christian social order. Suppression (0.91) is very high because the constraint's persistence has always required active exclusion: anathema, exile, execution, censorship, ecclesiastical discipline, denial of toleration, ecumenical gatekeeping. The enforcement machinery (conciliar canons, imperial laws, inquisitorial courts, confessional states, denominational ordination standards) is the constraint's active component. Theater ratio (0.18) is low because the theological labor (patristic synthesis, conciliar definitions, systematic theology) is genuine intellectual work, not mere performance — but the growing gap between the formula's philosophical apparatus and its biblical warrant, and the routine recitation of creeds by populations that do not understand them, introduces a performative layer.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (Nicene hierarchy), the constraint appears as genuine coordination: it solved the 4th-century christological chaos, preserved monotheism against tritheism and unitarianism, and provided the theological grammar for Christian civilization. From the payer seats (Arians, Unitarians, Oneness), the same structure appears as violent extraction: a Greek philosophical formula imposed on biblical data by imperial power, maintained by killing or excluding those who read the texts differently. From the excluded seats, it appears as epistemic injustice: their readings were never granted a conciliar hearing; the settlement was ratified by the winners. The engine computes this divergence from the declared power/exit/role structure — the claimed type (snare) reflects the payer-seat reality; the institutional self-understanding would claim rope or mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   The Nicene hierarchy (agenda_setter) sits at the beneficiary pole: it defines the constraint, administers its enforcement, and collects the rents of orthodoxy (legitimacy, property, state alliance). The imperial state church (beneficiary) receives political cohesion and civic religion. The victim groups (Arians, Unitarians, Oneness) are structural payers: they bear the full cost of the constraint's enforcement (exile, death, marginalization, institutional denial) while receiving none of its benefits. Their exit options range from trapped (Arians under empire, Unitarians under confessionally uniform states) to constrained (Oneness Pentecostals can build parallel denominations but lose ecumenical recognition and family ties). The excluded seats (modalist and unitarian reading adherents) are not coordinated by the constraint — their exclusion IS the constraint's coordination mechanism. The observer seat sees the full structure: a contingent philosophical settlement hardened into a boundary that extracts conformity from all who would be recognized as Christian.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (3rd-century christological chaos requiring imperial resolution) is historically dead — the Roman Empire is gone, the christological consensus it enforced is fractured, and the coordination function (single creed for single polity) no longer applies. Yet the constraint persists with escalating extractiveness because the Nicene hierarchy and its successor institutions (Catholic, Orthodox, Magisterial Protestant) have fused their identity with the homoousios formula. Abandoning it would dissolve their claim to catholicity, apostolic succession, and ecumenical recognition. The mandate has atrophied into identity-maintenance; the constraint is now a piton drifting toward snare (theater rising, extraction accumulating). The mandatrophy is unresolved: the arrangement's beneficiaries cannot declare it resolved without institutional suicide; its victims cannot force resolution without the power the constraint denies them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the trinitarian reading a genuine discovery of the kernel''s structure, or a constructed constraint that benefits the Nicene hierarchy?',
    'Historical analysis of the Nicea-Constantinople settlement: was homoousios forced by biblical data or chosen for its exclusionary precision? Patristic scholarship (Ayres, Anatolios, Williams) documents the term''s novelty and the imperial pressure behind its adoption.',
    'If constructed, the constraint is a false summit (mountain claim masking snare operation) — the FSM signature would trigger. If genuine discovery, the high extractiveness is the price of preserving revealed truth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the trinitarian formula is a natural-law constraint of revelation or a constructed boundary with identifiable beneficiaries.').

omega_variable(
    suppression_mechanism_shift,
    'How much of the measured suppression (0.91) is structural (state power, ecclesiastical courts) versus internalized (theological formation that makes non-Trinitarian reading unthinkable)?',
    'Compare suppression trajectories in contexts with vs. without state enforcement: early centuries (imperial), Reformation era (confessional states), modern pluralism (voluntary associations). If suppression persists at high levels without structural enforcement, internalization is significant.',
    'If substantially internalized, the constraint''s effective suppression exceeds its structural measure — the target carries the suppression after exit (identity-locked exit dynamics). This would increase effective extraction for identity-locked payers beyond the engine''s structural derivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_shift, empirical, 'Structural vs. internalized suppression in the trinitarian enforcement regime across epochs.').

omega_variable(
    coordination_extraction_separability,
    'Is the trinitarian formula''s coordination function (common creed, common baptism, common episcopacy) separable from its extraction function (anathema, exclusion, resource capture)?',
    'Counterfactual: could a christian communion maintain shared sacraments and mutual recognition WITHOUT the homoousios boundary? The ecumenical movement''s convergence texts (Lima Baptism Eucharist Ministry, Porvoo Agreement) test this — they achieve coordination across trinitarian differences but stall at non-Trinitarian inclusion.',
    'If inseparable, the measured extraction includes the price of coordination itself (the constraint is tangled_rope, not pure snare). If separable, the exclusionary boundary is pure extraction riding on a minimal coordination core.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the coordination and extraction components of trinitarian orthodoxy are structurally separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__trinitarian_reading, 325, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t325, biblical_divine_nature__trinitarian_reading, theater_ratio, 325, 0.05).
narrative_ontology:measurement(bibl_tr_t381, biblical_divine_nature__trinitarian_reading, theater_ratio, 381, 0.08).
narrative_ontology:measurement(bibl_tr_t451, biblical_divine_nature__trinitarian_reading, theater_ratio, 451, 0.1).
narrative_ontology:measurement(bibl_tr_t1054, biblical_divine_nature__trinitarian_reading, theater_ratio, 1054, 0.12).
narrative_ontology:measurement(bibl_tr_t1517, biblical_divine_nature__trinitarian_reading, theater_ratio, 1517, 0.14).
narrative_ontology:measurement(bibl_tr_t1553, biblical_divine_nature__trinitarian_reading, theater_ratio, 1553, 0.15).
narrative_ontology:measurement(bibl_tr_t1689, biblical_divine_nature__trinitarian_reading, theater_ratio, 1689, 0.15).
narrative_ontology:measurement(bibl_tr_t1916, biblical_divine_nature__trinitarian_reading, theater_ratio, 1916, 0.17).
narrative_ontology:measurement(bibl_tr_t2025, biblical_divine_nature__trinitarian_reading, theater_ratio, 2025, 0.18).

% Extraction over time
narrative_ontology:measurement(bibl_be_t325, biblical_divine_nature__trinitarian_reading, base_extractiveness, 325, 0.45).
narrative_ontology:measurement(bibl_be_t381, biblical_divine_nature__trinitarian_reading, base_extractiveness, 381, 0.55).
narrative_ontology:measurement(bibl_be_t451, biblical_divine_nature__trinitarian_reading, base_extractiveness, 451, 0.62).
narrative_ontology:measurement(bibl_be_t1054, biblical_divine_nature__trinitarian_reading, base_extractiveness, 1054, 0.7).
narrative_ontology:measurement(bibl_be_t1517, biblical_divine_nature__trinitarian_reading, base_extractiveness, 1517, 0.75).
narrative_ontology:measurement(bibl_be_t1553, biblical_divine_nature__trinitarian_reading, base_extractiveness, 1553, 0.78).
narrative_ontology:measurement(bibl_be_t1689, biblical_divine_nature__trinitarian_reading, base_extractiveness, 1689, 0.76).
narrative_ontology:measurement(bibl_be_t1916, biblical_divine_nature__trinitarian_reading, base_extractiveness, 1916, 0.8).
narrative_ontology:measurement(bibl_be_t2025, biblical_divine_nature__trinitarian_reading, base_extractiveness, 2025, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t325, biblical_divine_nature__trinitarian_reading, suppression_requirement, 325, 0.7).
narrative_ontology:measurement(bibl_su_t381, biblical_divine_nature__trinitarian_reading, suppression_requirement, 381, 0.8).
narrative_ontology:measurement(bibl_su_t451, biblical_divine_nature__trinitarian_reading, suppression_requirement, 451, 0.85).
narrative_ontology:measurement(bibl_su_t1054, biblical_divine_nature__trinitarian_reading, suppression_requirement, 1054, 0.88).
narrative_ontology:measurement(bibl_su_t1517, biblical_divine_nature__trinitarian_reading, suppression_requirement, 1517, 0.9).
narrative_ontology:measurement(bibl_su_t1553, biblical_divine_nature__trinitarian_reading, suppression_requirement, 1553, 0.92).
narrative_ontology:measurement(bibl_su_t1689, biblical_divine_nature__trinitarian_reading, suppression_requirement, 1689, 0.89).
narrative_ontology:measurement(bibl_su_t1916, biblical_divine_nature__trinitarian_reading, suppression_requirement, 1916, 0.9).
narrative_ontology:measurement(bibl_su_t2025, biblical_divine_nature__trinitarian_reading, suppression_requirement, 2025, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__trinitarian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_divine_nature__trinitarian_reading, 0.1).
narrative_ontology:affects_constraint(biblical_divine_nature__trinitarian_reading, biblical_divine_nature__modalist_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__trinitarian_reading, biblical_divine_nature__unitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__trinitarian_reading, christological_settlement__chalcedonian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__trinitarian_reading, ecumenical_recognition_regime__wcc_membership).
narrative_ontology:affects_constraint(biblical_divine_nature__trinitarian_reading, ministerial_ordination_standards__trinitarian_confession).

% DUAL FORMULATION NOTE:
% This constraint is one member of the biblical_divine_nature kernel family (trinitarian_reading, modalist_reading, unitarian_reading). The kernel's label 'the Trinity' conflates three structurally distinct constraints with different ε values, victim sets, and enforcement histories. The trinitarian reading has the highest ε (0.82) and the only imperial enforcement machinery; the modalist reading was foreclosed at Nicea but persists as Oneness Pentecostalism (ε ≈ 0.3, coordination without imperial extraction); the unitarian reading was suppressed but survived in tolerated margins (ε ≈ 0.15, rope-like coordination among dissenters). The ε-invariance principle requires separate stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_divine_nature__trinitarian_reading, institutional, 0.1).
constraint_indexing:directionality_override(biblical_divine_nature__trinitarian_reading, organized, 0.85).
constraint_indexing:directionality_override(biblical_divine_nature__trinitarian_reading, powerless, 0.95).
constraint_indexing:directionality_override(biblical_divine_nature__trinitarian_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
